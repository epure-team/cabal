#!/usr/bin/env python3
"""Authenticated OpenCode 1.14.20 content-dependent media/web probe."""

from __future__ import annotations

import argparse
import base64
import contextlib
import io
import json
import os
import re
import struct
import subprocess
import sys
import tempfile
import threading
import zlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Any
from unittest.mock import patch
from urllib.parse import urlsplit


EXPECTED_VERSION = "1.14.20"
DEFAULT_MODEL = "openai/gpt-5.4-mini"
PROBE_TIMEOUT_SECONDS = 300
VERSION_TIMEOUT_SECONDS = 15
OFFICIAL_PAGE = "https://opencode.ai/docs/cli/"
OFFICIAL_PAGE_H1 = "CLI"
PROBE_AGENT = "build"
SESSION_ID = re.compile(r"^ses_[0-9a-f]{12}[A-Za-z0-9]{14}$")
SAFE_IDENTIFIER = re.compile(r"^[A-Za-z0-9_.:-]{1,128}$")
SAFE_MODEL = re.compile(r"^[A-Za-z0-9_.:/+-]{1,200}$")
MODES = (
    "structured-output",
    "media-initial",
    "resume-upload",
    "resume-reuse",
    "schema-retry-media",
    "web-disabled",
    "web-search",
    "web-search-fetch",
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

# Deterministic 64x64 solid-red JPEG. PNG fixtures use only the standard
# library so the probe has no image-package dependency.
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
    """Argument parser whose errors never reproduce supplied values or paths."""

    def error(self, message: str) -> None:
        del message
        raise ProbeFailure("invalid probe arguments")


class PublicOutput:
    def __init__(
        self,
        *,
        session_id: str,
        text: str,
        tools: tuple[tuple[str, str], ...],
        usage_seen: bool,
    ) -> None:
        self.session_id = session_id
        self.text = text
        self.tools = tools
        self.usage_seen = usage_seen


def png_chunk(kind: bytes, payload: bytes) -> bytes:
    body = kind + payload
    return struct.pack(">I", len(payload)) + body + struct.pack(">I", zlib.crc32(body))


def solid_png(red: int, green: int, blue: int) -> bytes:
    width = height = 64
    scanline = b"\x00" + bytes((red, green, blue)) * width
    pixels = scanline * height
    return (
        b"\x89PNG\r\n\x1a\n"
        + png_chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
        + png_chunk(b"IDAT", zlib.compress(pixels, level=9))
        + png_chunk(b"IEND", b"")
    )


def write_fixtures(directory: Path) -> tuple[Path, Path, Path]:
    blue = directory / "attachment one.png"
    red = directory / "attachment two.jpg"
    green = directory / "attachment three.png"
    blue.write_bytes(solid_png(20, 45, 225))
    red.write_bytes(base64.b64decode(RED_JPEG, validate=True))
    green.write_bytes(solid_png(20, 190, 55))
    for path in (blue, red, green):
        path.chmod(0o600)
    return blue, red, green


def policy_rules(search: str, fetch: str) -> dict[str, str]:
    if search not in {"allow", "deny"} or fetch not in {"allow", "deny"}:
        raise ProbeFailure("invalid fixed web policy")
    return {"websearch": search, "webfetch": fetch}


def fixed_config(search: str, fetch: str) -> str:
    rules = policy_rules(search, fetch)
    return json.dumps(
        {
            "share": "disabled",
            "permission": rules,
            "agent": {
                PROBE_AGENT: {
                    "permission": rules,
                }
            },
        },
        separators=(",", ":"),
    )


def fixed_env(search: str, fetch: str) -> dict[str, str]:
    rules = policy_rules(search, fetch)
    env = os.environ.copy()
    env["OPENCODE_PERMISSION"] = json.dumps(rules, separators=(",", ":"))
    env["OPENCODE_CONFIG_CONTENT"] = fixed_config(search, fetch)
    env["OPENCODE_ENABLE_EXA"] = "1" if search == "allow" else "0"
    env["OPENCODE_AUTO_SHARE"] = "0"
    env["OPENCODE_DISABLE_AUTOUPDATE"] = "1"
    env["OPENCODE_DISABLE_LSP_DOWNLOAD"] = "1"
    return env


def require_version() -> None:
    try:
        completed = subprocess.run(
            ["opencode", "--version"],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=VERSION_TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired as error:
        raise ProbeFailure("OpenCode version check timed out") from error
    except OSError as error:
        raise ProbeFailure("OpenCode version process could not start") from error
    except UnicodeError as error:
        raise ProbeFailure("OpenCode version output could not be decoded") from error
    if completed.returncode != 0 or completed.stdout.strip() != EXPECTED_VERSION:
        raise ProbeFailure(f"probe requires exactly OpenCode {EXPECTED_VERSION}")


def require_help_contract() -> None:
    try:
        completed = subprocess.run(
            ["opencode", "run", "--help"],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=VERSION_TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired as error:
        raise ProbeFailure("OpenCode help check timed out") from error
    except OSError as error:
        raise ProbeFailure("OpenCode help process could not start") from error
    except UnicodeError as error:
        raise ProbeFailure("OpenCode help output could not be decoded") from error
    help_text = completed.stdout + completed.stderr
    required = ("--format", "--file", "--session", "--model", "--agent")
    if completed.returncode != 0 or not all(flag in help_text for flag in required):
        raise ProbeFailure("OpenCode run help lacks the required baseline surface")


def command_argv(
    model: str,
    images: tuple[Path, ...] = (),
    session_id: str | None = None,
) -> list[str]:
    if not SAFE_MODEL.fullmatch(model):
        raise ProbeFailure("invalid probe model")
    if session_id is not None and not SESSION_ID.fullmatch(session_id):
        raise ProbeFailure("invalid OpenCode session identifier")
    argv = [
        "opencode",
        "run",
        "--pure",
        "--format",
        "json",
        "--agent",
        PROBE_AGENT,
        "--model",
        model,
    ]
    if session_id is not None:
        argv.extend(("--session", session_id))
    for image in images:
        argv.extend(("--file", str(image.resolve())))
    argv.append("-")
    return argv


def nonnegative_int(value: Any) -> int | None:
    if isinstance(value, bool) or not isinstance(value, int) or value < 0:
        return None
    return value


def parse_public_output(stdout: str) -> PublicOutput:
    session_id: str | None = None
    text: list[str] = []
    tools: list[tuple[str, str]] = []
    usage_seen = False

    for line in stdout.splitlines():
        if not line.strip():
            continue
        try:
            event = json.loads(line)
        except (json.JSONDecodeError, UnicodeError) as error:
            raise ProbeFailure("OpenCode emitted malformed public JSONL") from error
        if not isinstance(event, dict):
            raise ProbeFailure("OpenCode emitted malformed public JSONL")

        candidate = event.get("sessionID")
        if isinstance(candidate, str) and SESSION_ID.fullmatch(candidate):
            if session_id is not None and candidate != session_id:
                raise ProbeFailure("OpenCode public JSONL changed session identifier")
            session_id = candidate

        event_type = event.get("type")
        part = event.get("part")
        if not isinstance(part, dict):
            continue

        if event_type == "text":
            timing = part.get("time")
            if (
                part.get("type") == "text"
                and isinstance(part.get("text"), str)
                and isinstance(timing, dict)
                and isinstance(timing.get("end"), (int, float))
                and not isinstance(timing.get("end"), bool)
            ):
                text.append(part["text"])
            continue

        if event_type == "tool_use" and part.get("type") == "tool":
            name = part.get("tool")
            state = part.get("state")
            if (
                isinstance(name, str)
                and SAFE_IDENTIFIER.fullmatch(name)
                and isinstance(state, dict)
                and state.get("status") in {"completed", "error"}
            ):
                tools.append((name, state["status"]))
            continue

        if event_type == "step_finish" and part.get("type") == "step-finish":
            tokens = part.get("tokens")
            if not isinstance(tokens, dict):
                continue
            cache = tokens.get("cache")
            values = [
                nonnegative_int(tokens.get("input")),
                nonnegative_int(tokens.get("output")),
            ]
            if isinstance(cache, dict):
                values.extend(
                    (
                        nonnegative_int(cache.get("read")),
                        nonnegative_int(cache.get("write")),
                    )
                )
            usage_seen = usage_seen or any(value is not None for value in values)

    if session_id is None:
        raise ProbeFailure("OpenCode emitted no canonical public session identifier")
    return PublicOutput(
        session_id=session_id,
        text="".join(text),
        tools=tuple(tools),
        usage_seen=usage_seen,
    )


def parse_object(output: PublicOutput) -> dict[str, Any]:
    try:
        value = json.loads(output.text)
    except (json.JSONDecodeError, UnicodeError) as error:
        raise ProbeFailure("OpenCode emitted no structured public answer") from error
    if not isinstance(value, dict):
        raise ProbeFailure("OpenCode emitted no structured public answer")
    return value


def validate_exact(result: dict[str, Any], expected: dict[str, Any]) -> None:
    if result != expected:
        raise ProbeFailure("OpenCode public answer failed the content assertion")


def validate_color_object(result: dict[str, Any], fields: tuple[str, ...]) -> None:
    if set(result) != set(fields):
        raise ProbeFailure("OpenCode structured answer has the wrong shape")
    if any(result.get(field) not in COLOR_NAMES for field in fields):
        raise ProbeFailure("OpenCode structured answer has an invalid color")


def run_probe(
    workspace: Path,
    argv: list[str],
    prompt: str,
    *,
    search: str = "deny",
    fetch: str = "deny",
) -> PublicOutput:
    try:
        completed = subprocess.run(
            argv,
            cwd=workspace,
            input=prompt,
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=PROBE_TIMEOUT_SECONDS,
            env=fixed_env(search, fetch),
        )
    except subprocess.TimeoutExpired as error:
        raise ProbeFailure("OpenCode probe timed out") from error
    except OSError as error:
        raise ProbeFailure("OpenCode probe process could not start") from error
    except UnicodeError as error:
        raise ProbeFailure("OpenCode probe output could not be decoded") from error
    if completed.returncode != 0:
        raise ProbeFailure("OpenCode probe process failed")
    return parse_public_output(completed.stdout)


def structured_probe(workspace: Path, model: str) -> None:
    output = run_probe(
        workspace,
        command_argv(model),
        'Return only compact JSON with exactly {"probe_status":"ready"}.',
    )
    validate_exact(parse_object(output), {"probe_status": "ready"})
    if not output.usage_seen:
        raise ProbeFailure("OpenCode emitted no public token usage")


def initial_media_probe(
    workspace: Path,
    model: str,
    blue: Path,
    red: Path,
) -> str:
    output = run_probe(
        workspace,
        command_argv(model, (blue, red)),
        "Inspect both attached images. Return only compact JSON with exactly "
        "png_dominant_color and jpeg_dominant_color, each one lowercase basic "
        "color word.",
    )
    result = parse_object(output)
    validate_color_object(result, ("png_dominant_color", "jpeg_dominant_color"))
    validate_exact(
        result,
        {"png_dominant_color": "blue", "jpeg_dominant_color": "red"},
    )
    return output.session_id


def resume_upload_probe(
    workspace: Path,
    model: str,
    session_id: str,
    green: Path,
) -> str:
    output = run_probe(
        workspace,
        command_argv(model, (green,), session_id),
        "Inspect the newly attached image. Return only compact JSON with exactly "
        "new_image_dominant_color as one lowercase basic color word.",
    )
    validate_exact(parse_object(output), {"new_image_dominant_color": "green"})
    if output.session_id != session_id:
        raise ProbeFailure("OpenCode resume changed the public session identifier")
    return output.session_id


def resume_reuse_probe(workspace: Path, model: str, session_id: str) -> None:
    argv = command_argv(model, session_id=session_id)
    if "--file" in argv or "-f" in argv:
        raise ProbeFailure("OpenCode resume reuse unexpectedly uploads an image")
    output = run_probe(
        workspace,
        argv,
        "Without opening files or using tools, recall the dominant color of the "
        "most recently attached image in this session. Return only compact JSON "
        "with exactly remembered_image_dominant_color.",
    )
    validate_exact(
        parse_object(output),
        {"remembered_image_dominant_color": "green"},
    )
    if output.session_id != session_id:
        raise ProbeFailure("OpenCode resume changed the public session identifier")


def schema_retry_media_probe(
    workspace: Path,
    model: str,
    blue: Path,
    red: Path,
) -> None:
    images = (blue, red)
    original_prompt = (
        "Inspect both attached images. Answer in one natural-language sentence, "
        "not JSON, naming the dominant PNG and JPEG colors."
    )
    first = run_probe(workspace, command_argv(model, images), original_prompt)
    try:
        first_result = json.loads(first.text)
    except (json.JSONDecodeError, UnicodeError):
        first_result = None
    expected = {"png_dominant_color": "blue", "jpeg_dominant_color": "red"}
    if first_result == expected:
        raise ProbeFailure("OpenCode schema retry probe received no invalid first answer")

    schema = {
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "type": "object",
        "properties": {
            "png_dominant_color": {"type": "string", "enum": ["blue"]},
            "jpeg_dominant_color": {"type": "string", "enum": ["red"]},
        },
        "required": ["png_dominant_color", "jpeg_dominant_color"],
        "additionalProperties": False,
    }
    retry_prompt = (
        original_prompt
        + "\n\n## Required output schema\n"
        + json.dumps(schema, separators=(",", ":"))
        + "\n\nYour previous response was invalid JSON. Return only output compliant "
        "with the schema."
    )
    second_argv = command_argv(model, images)
    if second_argv.count("--file") != 2:
        raise ProbeFailure("OpenCode fresh schema retry omitted image delivery")
    second = run_probe(workspace, second_argv, retry_prompt)
    validate_exact(parse_object(second), expected)
    if first.session_id == second.session_id:
        raise ProbeFailure("OpenCode schema retry did not use a fresh session")


def write_hostile_web_config(workspace: Path) -> None:
    rules = {"websearch": "allow", "webfetch": "allow"}
    content = {
        "permission": rules,
        "agent": {
            PROBE_AGENT: {
                "permission": rules,
            }
        },
    }
    (workspace / "opencode.json").write_text(
        json.dumps(content, separators=(",", ":")),
        encoding="utf-8",
    )


def web_tool_completed(output: PublicOutput, name: str) -> bool:
    return (name, "completed") in output.tools


class MarkerServer:
    def __init__(self) -> None:
        self.marker = "CBL07C_LOCAL_MARKER_7F3A9D"
        self.requests = 0
        owner = self

        class Handler(BaseHTTPRequestHandler):
            def do_GET(self) -> None:  # noqa: N802 - stdlib callback name
                owner.requests += 1
                body = owner.marker.encode("ascii")
                self.send_response(200)
                self.send_header("Content-Type", "text/plain")
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)

            def log_message(self, format: str, *args: object) -> None:
                del format, args

        self.server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)

    @property
    def url(self) -> str:
        port = self.server.server_address[1]
        return f"http://127.0.0.1:{port}/probe"

    def __enter__(self) -> MarkerServer:
        self.thread.start()
        return self

    def __exit__(self, *exc: object) -> None:
        del exc
        self.server.shutdown()
        self.server.server_close()
        self.thread.join(timeout=5)


def web_disabled_probe(workspace: Path, model: str) -> None:
    write_hostile_web_config(workspace)
    search = run_probe(
        workspace,
        command_argv(model),
        "Attempt to call websearch for 'site:opencode.ai/docs/cli OpenCode CLI'. "
        "Do not use any other tool. Return only compact JSON with exactly "
        "websearch_completed as a boolean.",
    )
    if search.tools:
        raise ProbeFailure("OpenCode disabled policy allowed a web tool")

    with MarkerServer() as marker:
        fetched = run_probe(
            workspace,
            command_argv(model),
            f"Attempt to call webfetch on {marker.url}. Do not use any other tool. "
            "Return only compact JSON with exactly fetched_marker as a string.",
        )
        if (
            marker.requests != 0
            or marker.marker in fetched.text
            or fetched.tools
        ):
            raise ProbeFailure("OpenCode disabled policy allowed web access")


def official_result_url(value: Any) -> bool:
    if not isinstance(value, str):
        return False
    parsed = urlsplit(value)
    return (
        parsed.scheme == "https"
        and parsed.netloc == "opencode.ai"
        and parsed.path.rstrip("/") == "/docs/cli"
    )


def web_search_probe(workspace: Path, model: str) -> None:
    output = run_probe(
        workspace,
        command_argv(model),
        "You MUST call websearch for 'site:opencode.ai/docs/cli OpenCode CLI'. "
        "Do not call webfetch or open any page. Return only compact JSON with "
        "exactly official_result_url set to the bare official CLI documentation "
        "URL from the search result.",
        search="allow",
        fetch="deny",
    )
    result = parse_object(output)
    if set(result) != {"official_result_url"} or not official_result_url(
        result.get("official_result_url")
    ):
        raise ProbeFailure("OpenCode search answer failed the content assertion")
    if not web_tool_completed(output, "websearch"):
        raise ProbeFailure("OpenCode emitted no completed public websearch lifecycle")
    if web_tool_completed(output, "webfetch"):
        raise ProbeFailure("OpenCode search-only policy allowed webfetch")


def web_search_fetch_probe(workspace: Path, model: str) -> None:
    output = run_probe(
        workspace,
        command_argv(model),
        "You MUST call websearch to locate the official OpenCode CLI "
        f"documentation, then MUST call webfetch on {OFFICIAL_PAGE}. Read the "
        "fetched page, not memory. Return only compact JSON with exactly page_h1 "
        "set to its visible primary heading.",
        search="allow",
        fetch="allow",
    )
    validate_exact(parse_object(output), {"page_h1": OFFICIAL_PAGE_H1})
    if not web_tool_completed(output, "websearch") or not web_tool_completed(
        output, "webfetch"
    ):
        raise ProbeFailure("OpenCode emitted no complete public search/fetch lifecycle")


def run_modes(
    workspace: Path,
    sealed_inputs: Path,
    selected_modes: tuple[str, ...],
    model: str,
) -> None:
    blue, red, green = write_fixtures(sealed_inputs)
    session_id: str | None = None
    uploaded_green = False

    def ensure_session() -> str:
        nonlocal session_id
        if session_id is None:
            session_id = initial_media_probe(workspace, model, blue, red)
        return session_id

    for mode in selected_modes:
        if mode == "structured-output":
            structured_probe(workspace, model)
        elif mode == "media-initial":
            ensure_session()
        elif mode == "resume-upload":
            session_id = resume_upload_probe(
                workspace,
                model,
                ensure_session(),
                green,
            )
            uploaded_green = True
        elif mode == "resume-reuse":
            current_session = ensure_session()
            if not uploaded_green:
                session_id = resume_upload_probe(
                    workspace,
                    model,
                    current_session,
                    green,
                )
                uploaded_green = True
            resume_reuse_probe(workspace, model, session_id or current_session)
        elif mode == "schema-retry-media":
            schema_retry_media_probe(workspace, model, blue, red)
        elif mode == "web-disabled":
            web_disabled_probe(workspace, model)
        elif mode == "web-search":
            web_search_probe(workspace, model)
        elif mode == "web-search-fetch":
            web_search_fetch_probe(workspace, model)
        else:
            raise ProbeFailure("unknown probe mode")
        print(f"PASS {mode}")


def public_jsonl_fixture() -> str:
    session = "ses_123456789abcABCDEFGHIJKLMN"
    events = [
        {
            "type": "step_start",
            "sessionID": session,
            "part": {"type": "step-start"},
        },
        {
            "type": "reasoning",
            "sessionID": session,
            "part": {
                "type": "reasoning",
                "text": "private reasoning",
                "time": {"end": 2},
            },
        },
        {
            "type": "text",
            "sessionID": session,
            "part": {"type": "text", "text": "private user text"},
        },
        {
            "type": "tool_use",
            "sessionID": session,
            "part": {
                "type": "tool",
                "tool": "webfetch",
                "callID": "call-private",
                "state": {
                    "status": "completed",
                    "input": {"url": "https://private.invalid/"},
                    "output": "private tool result",
                },
            },
        },
        {
            "type": "error",
            "sessionID": session,
            "error": {"message": "private error"},
        },
        {
            "type": "text",
            "sessionID": session,
            "part": {
                "type": "text",
                "text": '{"probe_status":"ready"}',
                "time": {"end": 3},
            },
        },
        {
            "type": "step_finish",
            "sessionID": session,
            "part": {
                "type": "step-finish",
                "cost": 0.01,
                "tokens": {
                    "input": 12,
                    "output": 3,
                    "reasoning": 99,
                    "cache": {"read": 4, "write": 2},
                },
            },
        },
    ]
    return "\n".join(json.dumps(event, separators=(",", ":")) for event in events)


def run_self_test() -> None:
    with tempfile.TemporaryDirectory(prefix="cabal-opencode-probe-self-test-") as root:
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
        argv = command_argv(DEFAULT_MODEL, (blue, red))
        expected_tail = ["--file", str(blue.resolve()), "--file", str(red.resolve()), "-"]
        if argv[-5:] != expected_tail:
            raise ProbeFailure("offline repeated-file argv self-test failed")
        resumed = command_argv(
            DEFAULT_MODEL,
            session_id="ses_123456789abcABCDEFGHIJKLMN",
        )
        if "--session" not in resumed or "--file" in resumed or "-f" in resumed:
            raise ProbeFailure("offline resume argv self-test failed")

    parsed = parse_public_output(public_jsonl_fixture())
    if (
        parsed.session_id != "ses_123456789abcABCDEFGHIJKLMN"
        or parsed.text != '{"probe_status":"ready"}'
        or parsed.tools != (("webfetch", "completed"),)
        or not parsed.usage_seen
    ):
        raise ProbeFailure("offline public parser self-test failed")
    private_markers = (
        "private reasoning",
        "private user text",
        "private.invalid",
        "private tool result",
        "private error",
    )
    public_projection = parsed.text + repr(parsed.tools)
    if any(marker in public_projection for marker in private_markers):
        raise ProbeFailure("offline parser privacy self-test failed")

    validate_color_object(
        {"png_dominant_color": "blue", "jpeg_dominant_color": "red"},
        ("png_dominant_color", "jpeg_dominant_color"),
    )
    for wrong in (
        {"png_dominant_color": "blue"},
        {"png_dominant_color": "cyan", "jpeg_dominant_color": "red"},
    ):
        try:
            validate_color_object(
                wrong,
                ("png_dominant_color", "jpeg_dominant_color"),
            )
        except ProbeFailure:
            pass
        else:
            raise ProbeFailure("offline content validator self-test failed")

    try:
        parse_public_output("not-json")
    except ProbeFailure:
        pass
    else:
        raise ProbeFailure("offline malformed parser self-test failed")

    def expect_process_failure(exception: Exception, expected_message: str) -> None:
        with patch("subprocess.run", side_effect=exception):
            try:
                run_probe(Path("."), ["opencode"], "private prompt")
            except ProbeFailure as error:
                if str(error) != expected_message or "private" in str(error):
                    raise ProbeFailure("offline process-error self-test failed")
            else:
                raise ProbeFailure("offline process-error self-test failed")

    expect_process_failure(
        subprocess.TimeoutExpired(["opencode"], PROBE_TIMEOUT_SECONDS),
        "OpenCode probe timed out",
    )
    expect_process_failure(
        OSError("private path and credential detail"),
        "OpenCode probe process could not start",
    )

    sensitive_marker = "/private/probe-token=never-print-this"

    def expect_cli_failure(
        argv: list[str],
        expected_message: str,
        side_effect: Any = None,
    ) -> None:
        stdout = io.StringIO()
        stderr = io.StringIO()
        run_patch = (
            patch("subprocess.run", side_effect=side_effect)
            if side_effect is not None
            else contextlib.nullcontext()
        )
        with run_patch, contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(
            stderr
        ):
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
        "OpenCode version check timed out",
        subprocess.TimeoutExpired(
            ["opencode", sensitive_marker],
            VERSION_TIMEOUT_SECONDS,
        ),
    )
    expect_cli_failure(
        ["media-initial"],
        "OpenCode probe interrupted",
        KeyboardInterrupt(sensitive_marker),
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
        model = os.environ.get("CABAL_E2E_MODEL_OPENCODE", DEFAULT_MODEL)
        if not SAFE_MODEL.fullmatch(model):
            raise ProbeFailure("invalid probe model")
        require_version()
        require_help_contract()
        with tempfile.TemporaryDirectory(prefix="cabal-opencode-probe-") as temp_dir:
            with tempfile.TemporaryDirectory(
                prefix="cabal opencode sealed inputs "
            ) as inputs_dir:
                sealed_inputs = Path(inputs_dir)
                sealed_inputs.chmod(0o700)
                run_modes(
                    Path(temp_dir),
                    sealed_inputs,
                    tuple(args.modes) or MODES,
                    model,
                )
    except ProbeFailure as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1
    except subprocess.TimeoutExpired:
        print("FAIL: OpenCode probe timed out", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        print("FAIL: OpenCode probe interrupted", file=sys.stderr)
        return 1
    except (OSError, UnicodeError, ValueError):
        print("FAIL: OpenCode probe I/O or decode failure", file=sys.stderr)
        return 1
    except Exception:
        print("FAIL: OpenCode probe parser failure", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
