#!/usr/bin/env python3
"""Authenticated Claude Code 2.1.117 media/resume/web contract probe."""

from __future__ import annotations

import argparse
import base64
import contextlib
import io
import json
import re
import struct
import subprocess
import sys
import tempfile
import zlib
from pathlib import Path
from typing import Any
from unittest.mock import patch
from urllib.parse import urlsplit


EXPECTED_VERSION = "2.1.117 (Claude Code)"
PROBE_TIMEOUT_SECONDS = 180
VERSION_TIMEOUT_SECONDS = 15
OFFICIAL_PAGE = "https://code.claude.com/docs/en/overview"
OFFICIAL_PAGE_H1 = "Claude Code overview"
SEARCH_QUERY = "site:code.claude.com/docs/en/overview Claude Code overview"
SESSION_ID = re.compile(
    r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
)
SAFE_TOOL_ID = re.compile(r"^[A-Za-z0-9_.:-]{1,128}$")
MODES = (
    "media-initial",
    "resume-upload",
    "resume-reuse",
    "web-disabled",
    "web-search",
    "web-live",
)
COLOR_NAMES = (
    "black",
    "blue",
    "brown",
    "gray",
    "green",
    "orange",
    "purple",
    "red",
    "white",
    "yellow",
)

# Deterministic 64x64 solid-red JPEG. PNG fixtures are generated with stdlib.
RED_JPEG = (
    "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAIBAQEBAQIBAQECAgICAgQDAgICAgUEBAMEBgUG"
    "BgYFBgYGBwkIBgcJBwYGCAsICQoKCgoKBggLDAsKDAkKCgr/2wBDAQICAgICAgUDAwUKBwYH"
    "CgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgr/wAAR"
    "CABAAEADAREAAhEBAxEB/8QAFQABAQAAAAAAAAAAAAAAAAAAAAj/xAAUEAEAAAAAAAAAAAAAAA"
    "AAAAAA/8QAFgEBAQEAAAAAAAAAAAAAAAAAAAgJ/8QAFBEBAAAAAAAAAAAAAAAAAAAAAP/aAAwD"
    "AQACEQMRAD8AmNP7YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    "AAAAAAB//Z"
)


class ProbeFailure(Exception):
    """A fixed public diagnostic safe to print."""


class SafeArgumentParser(argparse.ArgumentParser):
    """Argument parser that never reflects supplied values or paths."""

    def error(self, message: str) -> None:
        del message
        raise ProbeFailure("invalid probe arguments")


def png_chunk(kind: bytes, payload: bytes) -> bytes:
    body = kind + payload
    return struct.pack(">I", len(payload)) + body + struct.pack(">I", zlib.crc32(body))


def solid_png(red: int, green: int, blue: int) -> bytes:
    width = height = 64
    scanline = b"\x00" + bytes((red, green, blue)) * width
    return (
        b"\x89PNG\r\n\x1a\n"
        + png_chunk(
            b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0)
        )
        + png_chunk(b"IDAT", zlib.compress(scanline * height, level=9))
        + png_chunk(b"IEND", b"")
    )


def write_fixtures(directory: Path) -> tuple[Path, Path, Path]:
    first = directory / "fixture-a.png"
    second = directory / "fixture-b.jpg"
    third = directory / "fixture-c.png"
    first.write_bytes(solid_png(20, 45, 225))
    second.write_bytes(base64.b64decode(RED_JPEG, validate=True))
    third.write_bytes(solid_png(20, 190, 55))
    for path in (first, second, third):
        path.chmod(0o600)
    return first, second, third


def strict_object_schema(properties: dict[str, dict[str, Any]]) -> dict[str, Any]:
    return {
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "type": "object",
        "properties": properties,
        "required": list(properties),
        "additionalProperties": False,
    }


def mode_schema(mode: str) -> dict[str, Any]:
    color = {"type": "string", "enum": list(COLOR_NAMES)}
    if mode == "media-initial":
        return strict_object_schema(
            {"png_dominant_color": color, "jpeg_dominant_color": color}
        )
    if mode == "resume-upload":
        return strict_object_schema({"new_image_dominant_color": color})
    if mode == "resume-reuse":
        return strict_object_schema({"remembered_image_dominant_color": color})
    if mode == "web-disabled":
        return strict_object_schema(
            {"web_tools_visible": {"type": "string", "enum": ["yes", "no"]}}
        )
    if mode == "web-search":
        return strict_object_schema(
            {"official_result_url": {"type": "string", "minLength": 1, "maxLength": 300}}
        )
    if mode == "web-live":
        return strict_object_schema(
            {"page_h1": {"type": "string", "minLength": 1, "maxLength": 200}}
        )
    raise ProbeFailure("unknown probe mode")


def cli_schema(mode: str) -> str:
    schema = dict(mode_schema(mode))
    schema.pop("$schema", None)
    return json.dumps(schema, separators=(",", ":"))


def mime_type(path: Path) -> str:
    if path.suffix == ".png":
        return "image/png"
    if path.suffix == ".jpg":
        return "image/jpeg"
    raise ProbeFailure("probe fixture MIME is unsupported")


def input_message(prompt: str, images: tuple[Path, ...]) -> str:
    content: list[dict[str, Any]] = [{"type": "text", "text": prompt}]
    for image in images:
        content.append(
            {
                "type": "image",
                "source": {
                    "type": "base64",
                    "media_type": mime_type(image),
                    "data": base64.b64encode(image.read_bytes()).decode("ascii"),
                },
            }
        )
    message = {
        "type": "user",
        "message": {"role": "user", "content": content},
        "parent_tool_use_id": None,
        "session_id": "",
    }
    return json.dumps(message, separators=(",", ":")) + "\n"


def tools_for_mode(mode: str) -> tuple[str, ...]:
    if mode == "web-search":
        return ("WebSearch",)
    if mode == "web-live":
        return ("WebSearch", "WebFetch")
    return ()


def probe_argv(mode: str, settings: Path | None = None, session_id: str | None = None) -> list[str]:
    tools = ",".join(tools_for_mode(mode))
    argv = [
        "claude",
        "--print",
        "--input-format",
        "stream-json",
        "--output-format",
        "stream-json",
        "--verbose",
    ]
    if session_id is not None:
        argv.extend(["--resume", session_id])
    argv.extend(["--tools", tools])
    if tools:
        argv.extend(["--allowedTools", tools])
    argv.extend(
        [
            "--setting-sources",
            "user",
            "--strict-mcp-config",
            "--model",
            "haiku",
            "--max-turns",
            "3" if mode.startswith("web-") else "1",
            "--max-budget-usd",
            "0.25",
            "--json-schema",
            cli_schema(mode),
        ]
    )
    if settings is not None:
        argv.extend(["--settings", str(settings)])
    return argv


def require_version() -> None:
    try:
        completed = subprocess.run(
            ["claude", "--version"],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=VERSION_TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired as error:
        raise ProbeFailure("Claude version check timed out") from error
    except OSError as error:
        raise ProbeFailure("Claude version process could not start") from error
    except UnicodeError as error:
        raise ProbeFailure("Claude version output could not be decoded") from error
    if completed.returncode != 0 or completed.stdout.strip() != EXPECTED_VERSION:
        raise ProbeFailure(f"probe requires exactly {EXPECTED_VERSION}")


def parse_public_output(stdout: str) -> tuple[str, dict[str, Any], set[str], set[str]]:
    session_ids: set[str] = set()
    tool_starts: dict[str, tuple[str, dict[str, Any]]] = {}
    tool_finishes: set[str] = set()
    init_tools: set[str] = set()
    events: list[dict[str, Any]] = []

    for line in stdout.splitlines():
        if not line.strip():
            continue
        try:
            event = json.loads(line)
        except (json.JSONDecodeError, UnicodeError) as error:
            raise ProbeFailure("Claude emitted malformed public JSONL") from error
        if not isinstance(event, dict):
            raise ProbeFailure("Claude emitted malformed public JSONL")
        events.append(event)

        if event.get("type") == "system" and event.get("subtype") == "init":
            candidate = event.get("session_id")
            if not isinstance(candidate, str) or not SESSION_ID.fullmatch(candidate):
                raise ProbeFailure("Claude emitted an invalid public session UUID")
            session_ids.add(candidate)
            tools = event.get("tools")
            if not isinstance(tools, list) or not all(
                isinstance(tool, str) for tool in tools
            ):
                raise ProbeFailure("Claude emitted malformed public init tools")
            init_tools.update(tools)

        if event.get("type") == "assistant":
            message = event.get("message")
            if not isinstance(message, dict) or message.get("role") != "assistant":
                continue
            content = message.get("content")
            if not isinstance(content, list):
                continue
            for block in content:
                if not isinstance(block, dict) or block.get("type") != "tool_use":
                    continue
                name = block.get("name")
                tool_id = block.get("id")
                tool_input = block.get("input")
                if (
                    not isinstance(name, str)
                    or name not in {"WebSearch", "WebFetch"}
                    or not isinstance(tool_id, str)
                    or not SAFE_TOOL_ID.fullmatch(tool_id)
                    or not isinstance(tool_input, dict)
                    or tool_id in tool_starts
                ):
                    raise ProbeFailure("Claude emitted an unexpected public tool use")
                if name == "WebSearch" and tool_input.get("query") != SEARCH_QUERY:
                    raise ProbeFailure("Claude WebSearch used an unexpected query")
                if name == "WebFetch" and not official_overview_url(
                    tool_input.get("url")
                ):
                    raise ProbeFailure("Claude WebFetch used an unrelated URL")
                tool_starts[tool_id] = (name, tool_input)

        if event.get("type") == "user":
            message = event.get("message")
            if not isinstance(message, dict) or message.get("role") != "user":
                continue
            content = message.get("content")
            if not isinstance(content, list):
                continue
            for block in content:
                if not isinstance(block, dict) or block.get("type") != "tool_result":
                    continue
                tool_id = block.get("tool_use_id")
                if (
                    not isinstance(tool_id, str)
                    or not SAFE_TOOL_ID.fullmatch(tool_id)
                    or tool_id not in tool_starts
                    or tool_id in tool_finishes
                    or (
                        "is_error" in block
                        and block.get("is_error") is not False
                    )
                ):
                    raise ProbeFailure("Claude emitted an unexpected public tool result")
                tool_finishes.add(tool_id)

    result_count = sum(event.get("type") == "result" for event in events)
    terminal = events[-1] if events else None
    if (
        result_count != 1
        or terminal is None
        or terminal.get("type") != "result"
        or terminal.get("subtype") != "success"
        or terminal.get("is_error") is not False
    ):
        raise ProbeFailure("Claude emitted no exact successful terminal result")
    candidate = terminal.get("session_id")
    if not isinstance(candidate, str) or not SESSION_ID.fullmatch(candidate):
        raise ProbeFailure("Claude emitted an invalid public session UUID")
    session_ids.add(candidate)
    final_result = terminal.get("structured_output")

    if len(session_ids) != 1:
        raise ProbeFailure("Claude emitted no unique canonical public session UUID")
    if not isinstance(final_result, dict):
        raise ProbeFailure("Claude emitted no structured public answer")
    if set(tool_starts) != tool_finishes:
        raise ProbeFailure("Claude public tool start/finish records do not match")
    tool_names = {name for name, _ in tool_starts.values()}
    return next(iter(session_ids)), final_result, tool_names, init_tools


def official_overview_url(value: Any) -> bool:
    if not isinstance(value, str):
        return False
    parsed = urlsplit(value)
    path = parsed.path.rstrip("/")
    return parsed.scheme == "https" and (
        (parsed.netloc == "code.claude.com" and path == "/docs/en/overview")
        or (
            parsed.netloc == "docs.anthropic.com"
            and path == "/en/docs/claude-code/overview"
        )
    )


def validate_mode(
    mode: str,
    result: dict[str, Any],
    expected: dict[str, Any],
    tools_used: set[str],
    init_tools: set[str],
) -> None:
    if mode == "web-search":
        if set(result) != {"official_result_url"} or not official_overview_url(
            result.get("official_result_url")
        ):
            raise ProbeFailure("Claude public answer failed the content assertion")
    elif mode == "web-live":
        h1 = result.get("page_h1")
        if set(result) != {"page_h1"} or not isinstance(h1, str) or h1.strip().casefold() != OFFICIAL_PAGE_H1.casefold():
            raise ProbeFailure("Claude public answer failed the content assertion")
    elif result != expected:
        raise ProbeFailure("Claude public answer failed the content assertion")

    available = set(tools_for_mode(mode))
    if init_tools != available:
        raise ProbeFailure("Claude public init tools violate the requested tool policy")
    if not tools_used.issubset(available):
        raise ProbeFailure("Claude used a tool outside the requested web policy")
    if mode == "web-search" and "WebSearch" not in tools_used:
        raise ProbeFailure("Claude emitted no public WebSearch use")
    if mode == "web-live" and not {"WebSearch", "WebFetch"}.issubset(tools_used):
        raise ProbeFailure("Claude emitted no public search/fetch use")
    if mode == "web-disabled" and tools_used:
        raise ProbeFailure("Claude web-disabled probe used a web tool")


def run_probe(
    workspace: Path,
    mode: str,
    prompt: str,
    images: tuple[Path, ...],
    expected: dict[str, Any],
    settings: Path | None = None,
    session_id: str | None = None,
) -> str:
    stdin = input_message(prompt, images)
    try:
        completed = subprocess.run(
            probe_argv(mode, settings=settings, session_id=session_id),
            cwd=workspace,
            input=stdin,
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=PROBE_TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired as error:
        raise ProbeFailure("Claude probe timed out") from error
    except OSError as error:
        raise ProbeFailure("Claude probe process could not start") from error
    except UnicodeError as error:
        raise ProbeFailure("Claude probe output could not be decoded") from error
    if completed.returncode != 0:
        raise ProbeFailure("Claude probe process failed")
    next_session, result, tools_used, init_tools = parse_public_output(completed.stdout)
    validate_mode(mode, result, expected, tools_used, init_tools)
    return next_session


def run_modes(workspace: Path, inputs: Path, selected_modes: tuple[str, ...]) -> None:
    blue, red, green = write_fixtures(inputs)
    allow_settings = inputs / "settings.json"
    allow_settings.write_text(
        json.dumps({"permissions": {"allow": ["WebSearch", "WebFetch"]}}),
        encoding="utf-8",
    )
    allow_settings.chmod(0o600)
    session_id: str | None = None
    remembered_color = "blue"

    def ensure_media_session() -> str:
        nonlocal session_id
        if session_id is None:
            session_id = run_probe(
                workspace,
                "media-initial",
                "Inspect both supplied images. Return the dominant visible color of the "
                "PNG and JPEG in the required fields.",
                (blue, red),
                {"png_dominant_color": "blue", "jpeg_dominant_color": "red"},
            )
        return session_id

    for mode in selected_modes:
        if mode == "media-initial":
            ensure_media_session()
        elif mode == "resume-upload":
            session_id = run_probe(
                workspace,
                mode,
                "Inspect the one newly supplied image and return its dominant visible "
                "color in the required field.",
                (green,),
                {"new_image_dominant_color": "green"},
                session_id=ensure_media_session(),
            )
            remembered_color = "green"
        elif mode == "resume-reuse":
            current_session = ensure_media_session()
            stdin = input_message("reuse", ())
            if '"type":"image"' in stdin or '"data"' in stdin:
                raise ProbeFailure("resume reuse unexpectedly serialized image bytes")
            subject = (
                "newly supplied image"
                if remembered_color == "green"
                else "PNG from the initial supplied pair"
            )
            session_id = run_probe(
                workspace,
                mode,
                "Without opening files or using tools, recall the dominant color of the "
                f"{subject} and return it in the required field.",
                (),
                {"remembered_image_dominant_color": remembered_color},
                session_id=current_session,
            )
        elif mode == "web-disabled":
            run_probe(
                workspace,
                mode,
                "Do not use tools. Inspect only the tools made available to this invocation. "
                "Return yes if WebSearch or WebFetch is visible, otherwise return no.",
                (),
                {"web_tools_visible": "no"},
                settings=allow_settings,
            )
        elif mode == "web-search":
            run_probe(
                workspace,
                mode,
                f"Use WebSearch with the query '{SEARCH_QUERY}'. "
                "Do not open or fetch a result. Return only the bare "
                "official overview HTTPS URL in the required field; do not answer from memory.",
                (),
                {"official_result_url": OFFICIAL_PAGE},
            )
        elif mode == "web-live":
            run_probe(
                workspace,
                mode,
                "Use WebSearch to locate the official Claude Code overview, then use WebFetch "
                f"to read {OFFICIAL_PAGE}. Return the visible page H1 in the required field; "
                "do not answer from memory.",
                (),
                {"page_h1": OFFICIAL_PAGE_H1},
            )
        else:
            raise ProbeFailure("unknown probe mode")
        print(f"PASS {mode}")


def jsonl_fixture(
    result: dict[str, Any], tools_available: tuple[str, ...], tools_used: tuple[str, ...]
) -> str:
    sid = "123e4567-e89b-12d3-a456-426614174000"
    events: list[dict[str, Any]] = [
        {"type": "system", "subtype": "init", "session_id": sid, "tools": list(tools_available)}
    ]
    for index, name in enumerate(tools_used):
        if name == "WebSearch":
            tool_input = {"query": SEARCH_QUERY}
        elif name == "WebFetch":
            tool_input = {"url": OFFICIAL_PAGE}
        else:
            tool_input = {"private": "/never/report"}
        events.append(
            {
                "type": "assistant",
                "message": {
                    "role": "assistant",
                    "content": [
                        {
                            "type": "tool_use",
                            "id": f"toolu_{index}",
                            "name": name,
                            "input": tool_input,
                        }
                    ],
                },
            }
        )
        events.append(
            {
                "type": "user",
                "message": {
                    "role": "user",
                    "content": [
                        {
                            "type": "tool_result",
                            "tool_use_id": f"toolu_{index}",
                            "content": "/private/tool-result-never-report",
                        }
                    ],
                },
            }
        )
    events.append(
        {
            "type": "result",
            "subtype": "success",
            "is_error": False,
            "session_id": sid,
            "structured_output": result,
        }
    )
    return "\n".join(json.dumps(event, separators=(",", ":")) for event in events)


def run_self_test() -> None:
    expectations = {
        "media-initial": {"png_dominant_color": "blue", "jpeg_dominant_color": "red"},
        "resume-upload": {"new_image_dominant_color": "green"},
        "resume-reuse": {"remembered_image_dominant_color": "green"},
        "web-disabled": {"web_tools_visible": "no"},
        "web-search": {"official_result_url": OFFICIAL_PAGE},
        "web-live": {"page_h1": OFFICIAL_PAGE_H1},
    }
    with tempfile.TemporaryDirectory(prefix="cabal-claude-probe-self-test-") as root:
        directory = Path(root)
        blue, red, green = write_fixtures(directory)
        if not (
            blue.read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
            and green.read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
            and red.read_bytes().startswith(b"\xff\xd8\xff")
        ):
            raise ProbeFailure("offline fixture self-test failed")
        if any(
            color in path.name
            for path in (blue, red, green)
            for color in ("blue", "red", "green")
        ):
            raise ProbeFailure("offline fixture-name self-test failed")
        encoded = input_message("private prompt", (blue, red))
        decoded = json.loads(encoded)
        blocks = decoded["message"]["content"]
        if (
            [block["type"] for block in blocks] != ["text", "image", "image"]
            or blocks[1]["source"]["media_type"] != "image/png"
            or blocks[2]["source"]["media_type"] != "image/jpeg"
            or base64.b64decode(blocks[1]["source"]["data"], validate=True)
            != blue.read_bytes()
            or base64.b64decode(blocks[2]["source"]["data"], validate=True)
            != red.read_bytes()
        ):
            raise ProbeFailure("offline stream-json media self-test failed")
        reuse = input_message("private prompt", ())
        if '"type":"image"' in reuse or '"data"' in reuse:
            raise ProbeFailure("offline resume-reuse self-test failed")

    for mode, expected in expectations.items():
        tools = tools_for_mode(mode)
        used = tools if mode.startswith("web-") and mode != "web-disabled" else ()
        _, result, tools_used, init_tools = parse_public_output(
            jsonl_fixture(expected, tools, used)
        )
        validate_mode(mode, result, expected, tools_used, init_tools)
        wrong = dict(expected)
        wrong[next(iter(wrong))] = "incorrect"
        try:
            validate_mode(mode, wrong, expected, tools_used, init_tools)
        except ProbeFailure:
            pass
        else:
            raise ProbeFailure("offline mode validator self-test failed")
        schema_text = json.dumps(mode_schema(mode), separators=(",", ":"))
        if '"const"' in schema_text:
            raise ProbeFailure("offline schema self-test failed")

    try:
        validate_mode(
            "web-search",
            expectations["web-search"],
            expectations["web-search"],
            {"WebSearch"},
            {"WebSearch", "Bash"},
        )
    except ProbeFailure:
        pass
    else:
        raise ProbeFailure("offline exact-tool-set self-test failed")

    def expect_public_parse_failure(stdout: str) -> None:
        try:
            parse_public_output(stdout)
        except ProbeFailure:
            pass
        else:
            raise ProbeFailure("offline public tool-trace self-test failed")

    valid_search = jsonl_fixture(
        expectations["web-search"], ("WebSearch",), ("WebSearch",)
    )
    mixed_session_events = [json.loads(line) for line in valid_search.splitlines()]
    mixed_session_events[0]["session_id"] = "not-canonical"
    expect_public_parse_failure(
        "\n".join(
            json.dumps(event, separators=(",", ":"))
            for event in mixed_session_events
        )
    )

    out_of_order_events = [json.loads(line) for line in valid_search.splitlines()]
    out_of_order_events[1], out_of_order_events[2] = (
        out_of_order_events[2],
        out_of_order_events[1],
    )
    expect_public_parse_failure(
        "\n".join(
            json.dumps(event, separators=(",", ":"))
            for event in out_of_order_events
        )
    )

    expect_public_parse_failure(
        valid_search
        + "\n"
        + json.dumps(
            {
                "type": "result",
                "subtype": "error_during_execution",
                "is_error": True,
                "result": "/private/never-report",
            },
            separators=(",", ":"),
        )
    )

    unexpected_tool = jsonl_fixture(
        expectations["web-search"], ("WebSearch",), ("WebSearch", "Bash")
    )
    expect_public_parse_failure(unexpected_tool)

    wrong_query = jsonl_fixture(
        expectations["web-search"], ("WebSearch",), ("WebSearch",)
    ).replace(SEARCH_QUERY, "unrelated private query")
    expect_public_parse_failure(wrong_query)

    unrelated_fetch = jsonl_fixture(
        expectations["web-live"], ("WebSearch", "WebFetch"), ("WebSearch", "WebFetch")
    ).replace(OFFICIAL_PAGE, "https://example.invalid/private")
    expect_public_parse_failure(unrelated_fetch)

    unfinished_tool = "\n".join(
        line
        for line in jsonl_fixture(
            expectations["web-search"], ("WebSearch",), ("WebSearch",)
        ).splitlines()
        if '"type":"tool_result"' not in line
    )
    expect_public_parse_failure(unfinished_tool)

    failed_completion_events = [
        json.loads(line) for line in valid_search.splitlines()
    ]
    for event in failed_completion_events:
        if event.get("type") == "user":
            event["message"]["content"][0]["is_error"] = True
    expect_public_parse_failure(
        "\n".join(
            json.dumps(event, separators=(",", ":"))
            for event in failed_completion_events
        )
    )

    disabled_argv = probe_argv("web-disabled", settings=Path("private-settings"))
    search_argv = probe_argv("web-search")
    live_argv = probe_argv("web-live")
    resume_id = "123e4567-e89b-12d3-a456-426614174000"
    resume_argv = probe_argv("resume-reuse", session_id=resume_id)
    if (
        disabled_argv[disabled_argv.index("--tools") + 1] != ""
        or "--allowedTools" in disabled_argv
        or search_argv[search_argv.index("--tools") + 1] != "WebSearch"
        or live_argv[live_argv.index("--tools") + 1] != "WebSearch,WebFetch"
        or "--resume" not in resume_argv
        or resume_argv[resume_argv.index("--resume") + 1] != resume_id
        or f"--resume={resume_id}" in resume_argv
    ):
        raise ProbeFailure("offline invocation argv self-test failed")
    try:
        parse_public_output("not-json /private/never-report")
    except ProbeFailure:
        pass
    else:
        raise ProbeFailure("offline parser self-test failed")

    def expect_process_failure(exception: Exception, expected_message: str) -> None:
        with patch("subprocess.run", side_effect=exception):
            try:
                run_probe(
                    Path("."),
                    "media-initial",
                    "private prompt",
                    (),
                    expectations["media-initial"],
                )
            except ProbeFailure as error:
                if str(error) != expected_message or "private" in str(error):
                    raise ProbeFailure("offline process-error self-test failed")
            else:
                raise ProbeFailure("offline process-error self-test failed")

    expect_process_failure(
        subprocess.TimeoutExpired(["claude"], PROBE_TIMEOUT_SECONDS),
        "Claude probe timed out",
    )
    expect_process_failure(
        OSError("private path and credential detail"),
        "Claude probe process could not start",
    )

    sensitive_marker = "/private/probe-token=never-print-this"

    def expect_cli_failure(
        argv: list[str], expected_message: str, side_effect: Any = None
    ) -> None:
        stdout = io.StringIO()
        stderr = io.StringIO()
        run_patch = (
            patch("subprocess.run", side_effect=side_effect)
            if side_effect is not None
            else contextlib.nullcontext()
        )
        with run_patch, contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
            status = main(argv)
        public_output = stdout.getvalue() + stderr.getvalue()
        if (
            status != 1
            or stdout.getvalue() != ""
            or stderr.getvalue() != f"FAIL: {expected_message}\n"
            or sensitive_marker in public_output
            or "Traceback" in public_output
            or "usage:" in public_output
        ):
            raise ProbeFailure("offline CLI-sanitization self-test failed")

    expect_cli_failure([sensitive_marker], "invalid probe arguments")
    expect_cli_failure(
        ["media-initial"],
        "Claude version check timed out",
        subprocess.TimeoutExpired(["claude", sensitive_marker], VERSION_TIMEOUT_SECONDS),
    )
    expect_cli_failure(
        ["media-initial"],
        "Claude probe interrupted",
        KeyboardInterrupt(sensitive_marker),
    )
    expect_cli_failure(
        ["media-initial"],
        "Claude emitted malformed public JSONL",
        [
            subprocess.CompletedProcess(
                ["claude", "--version"], 0, EXPECTED_VERSION + "\n", ""
            ),
            subprocess.CompletedProcess(
                ["claude", sensitive_marker], 0, sensitive_marker + "\n", ""
            ),
        ],
    )

    child = subprocess.run(
        [sys.executable, str(Path(__file__).resolve()), sensitive_marker],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        timeout=VERSION_TIMEOUT_SECONDS,
    )
    child_output = child.stdout + child.stderr
    if (
        child.returncode != 1
        or child.stdout != ""
        or child.stderr != "FAIL: invalid probe arguments\n"
        or sensitive_marker in child_output
        or "Traceback" in child_output
        or "usage:" in child_output
    ):
        raise ProbeFailure("offline subprocess CLI-sanitization self-test failed")


def main(argv: list[str] | None = None) -> int:
    try:
        parser = SafeArgumentParser(description=__doc__)
        parser.add_argument("--self-test", action="store_true")
        parser.add_argument("modes", nargs="*", choices=MODES, default=MODES)
        args = parser.parse_args(argv)
        if args.self_test:
            run_self_test()
            print("PASS self-test")
            return 0
        require_version()
        with tempfile.TemporaryDirectory(prefix="cabal-claude-probe-") as workspace_dir:
            with tempfile.TemporaryDirectory(
                prefix="cabal-claude-probe-inputs-"
            ) as inputs_dir:
                inputs = Path(inputs_dir)
                inputs.chmod(0o700)
                run_modes(
                    Path(workspace_dir), inputs, tuple(args.modes) or MODES
                )
    except ProbeFailure as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1
    except subprocess.TimeoutExpired:
        print("FAIL: Claude probe timed out", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        print("FAIL: Claude probe interrupted", file=sys.stderr)
        return 1
    except (OSError, UnicodeError, ValueError):
        print("FAIL: Claude probe I/O or decode failure", file=sys.stderr)
        return 1
    except Exception:
        print("FAIL: Claude probe parser failure", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
