#!/usr/bin/env python3
"""Below-baseline OpenCode media/web observation probe (not capability evidence)."""

from __future__ import annotations

import argparse
import base64
import contextlib
import io
import json
import os
import re
import secrets
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

AUTH_TESTED_VERSION = "1.2.24"
DESCRIPTOR_BASELINE_VERSION = "1.14.20"
EXPECTED_VERSION = AUTH_TESTED_VERSION
DEFAULT_MODEL = "openai/gpt-5.4"
PROBE_TIMEOUT_SECONDS = 300
VERSION_TIMEOUT_SECONDS = 15
OFFICIAL_PAGE = "https://opencode.ai/docs/cli/"
OFFICIAL_PAGE_H1 = "CLI"
PROBE_AGENT = "cabal-probe-" + secrets.token_hex(8)
SESSION_ID = re.compile(r"^ses_[0-9a-f]{12}[A-Za-z0-9]{14}$")
MESSAGE_ID = re.compile(r"^msg_[0-9a-f]{12}[A-Za-z0-9]{14}$")
PART_ID = re.compile(r"^prt_[0-9a-f]{12}[A-Za-z0-9]{14}$")
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


def version_tuple(version: str) -> tuple[int, int, int]:
    if not re.fullmatch(r"[0-9]+\.[0-9]+\.[0-9]+", version):
        raise ProbeFailure("invalid fixed OpenCode version")
    major, minor, patch_version = version.split(".")
    return int(major), int(minor), int(patch_version)


def version_at_least(version: str, baseline: str) -> bool:
    return version_tuple(version) >= version_tuple(baseline)


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
    return {
        "websearch": search,
        "webfetch": fetch,
        "codesearch": "deny",
        "task": "deny",
    }


def fixed_config(search: str, fetch: str) -> str:
    rules = policy_rules(search, fetch)
    return json.dumps(
        {
            "share": "disabled",
            "permission": rules,
            "agent": {
                PROBE_AGENT: {
                    "mode": "primary",
                    "permission": rules,
                }
            },
        },
        separators=(",", ":"),
    )


def fixed_env(
    search: str,
    fetch: str,
    workspace: Path | None = None,
) -> dict[str, str]:
    rules = policy_rules(search, fetch)
    env = os.environ.copy()
    env.pop("OPENCODE_DB", None)
    env["OPENCODE_PERMISSION"] = json.dumps(rules, separators=(",", ":"))
    env["OPENCODE_CONFIG_CONTENT"] = fixed_config(search, fetch)
    env["OPENCODE_CONFIG_DIR"] = ""
    env["OPENCODE_DISABLE_PROJECT_CONFIG"] = "1"
    env["OPENCODE_EXPERIMENTAL"] = "0"
    env["OPENCODE_EXPERIMENTAL_EXA"] = "0"
    env["OPENCODE_ENABLE_EXA"] = "1" if search == "allow" else "0"
    env["OPENCODE_AUTO_SHARE"] = "0"
    env["OPENCODE_DISABLE_AUTOUPDATE"] = "1"
    env["OPENCODE_DISABLE_LSP_DOWNLOAD"] = "1"
    if workspace is not None:
        config_home = workspace / ".cabal-opencode-isolated-config"
        config_home.mkdir(mode=0o700, exist_ok=True)
        config_home.chmod(0o700)
        env["XDG_CONFIG_HOME"] = str(config_home)
        env["OPENCODE_TEST_HOME"] = str(config_home)
        env["OPENCODE_TEST_MANAGED_CONFIG_DIR"] = str(config_home / "managed")
        env["OPENCODE_CONFIG"] = str(workspace / "opencode.json")
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


def finite_nonnegative_number(value: Any) -> float | None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None
    number = float(value)
    if number < 0 or not (float("-inf") < number < float("inf")):
        return None
    return number


def valid_completed_timing(value: Any) -> bool:
    if not isinstance(value, dict):
        return False
    start = finite_nonnegative_number(value.get("start"))
    end = finite_nonnegative_number(value.get("end"))
    return start is not None and end is not None and end >= start


def public_envelope(event: dict[str, Any], part: Any) -> tuple[str, str] | None:
    if not isinstance(part, dict):
        return None
    session_id = event.get("sessionID")
    part_session_id = part.get("sessionID")
    message_id = part.get("messageID")
    if (
        finite_nonnegative_number(event.get("timestamp")) is None
        or not isinstance(session_id, str)
        or not SESSION_ID.fullmatch(session_id)
        or part_session_id != session_id
        or not isinstance(message_id, str)
        or not MESSAGE_ID.fullmatch(message_id)
        or not isinstance(part.get("id"), str)
        or not PART_ID.fullmatch(part["id"])
    ):
        return None
    return session_id, message_id


def parse_public_output(stdout: str) -> PublicOutput:
    session_id: str | None = None
    message_id: str | None = None
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

        event_type = event.get("type")
        if event_type == "error":
            raise ProbeFailure("OpenCode emitted an error event")
        if event_type not in {"step_start", "text", "tool_use", "step_finish"}:
            if isinstance(event_type, str):
                continue
            raise ProbeFailure("OpenCode emitted malformed public JSONL")
        part = event.get("part")
        envelope = public_envelope(event, part)
        if envelope is None:
            raise ProbeFailure("OpenCode emitted an invalid public JSONL envelope")
        candidate_session, candidate_message = envelope
        if session_id is not None and candidate_session != session_id:
            raise ProbeFailure("OpenCode public JSONL changed session identifier")
        session_id = candidate_session

        if event_type == "step_start":
            if part.get("type") != "step-start":
                raise ProbeFailure("OpenCode emitted an invalid public JSONL envelope")
            message_id = candidate_message
            continue

        if message_id is None or candidate_message != message_id:
            raise ProbeFailure("OpenCode emitted an invalid public JSONL envelope")

        if event_type == "text":
            if (
                part.get("type") != "text"
                or not isinstance(part.get("text"), str)
                or not valid_completed_timing(part.get("time"))
            ):
                raise ProbeFailure("OpenCode emitted an invalid public JSONL envelope")
            text.append(part["text"])
            continue

        if event_type == "tool_use" and part.get("type") == "tool":
            name = part.get("tool")
            state = part.get("state")
            if (
                isinstance(name, str)
                and SAFE_IDENTIFIER.fullmatch(name)
                and isinstance(part.get("callID"), str)
                and SAFE_IDENTIFIER.fullmatch(part["callID"])
                and isinstance(state, dict)
            ):
                if state.get("status") == "error":
                    raise ProbeFailure("OpenCode emitted a failed tool event")
                if (
                    state.get("status") == "completed"
                    and isinstance(state.get("input"), dict)
                    and isinstance(state.get("output"), str)
                    and isinstance(state.get("title"), str)
                    and isinstance(state.get("metadata"), dict)
                    and valid_completed_timing(state.get("time"))
                ):
                    tools.append((name, "completed"))
                    continue
            raise ProbeFailure("OpenCode emitted an invalid public JSONL envelope")

        if event_type == "step_finish" and part.get("type") == "step-finish":
            tokens = part.get("tokens")
            if (
                not isinstance(part.get("reason"), str)
                or finite_nonnegative_number(part.get("cost")) is None
                or not isinstance(tokens, dict)
            ):
                raise ProbeFailure("OpenCode emitted an invalid public JSONL envelope")
            cache = tokens.get("cache")
            values = [
                nonnegative_int(tokens.get("input")),
                nonnegative_int(tokens.get("output")),
                nonnegative_int(tokens.get("reasoning")),
            ]
            if not isinstance(cache, dict):
                raise ProbeFailure("OpenCode emitted an invalid public JSONL envelope")
            values.extend(
                (
                    nonnegative_int(cache.get("read")),
                    nonnegative_int(cache.get("write")),
                )
            )
            total = tokens.get("total")
            if any(value is None for value in values) or (
                total is not None and nonnegative_int(total) is None
            ):
                raise ProbeFailure("OpenCode emitted an invalid public JSONL envelope")
            usage_seen = True
            continue

        raise ProbeFailure("OpenCode emitted an invalid public JSONL envelope")

    if session_id is None:
        raise ProbeFailure("OpenCode emitted no canonical public session identifier")
    if not text:
        raise ProbeFailure("OpenCode emitted no completed public response text")
    if not usage_seen:
        raise ProbeFailure("OpenCode emitted no valid public completion usage")
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
            env=fixed_env(search, fetch, workspace),
        )
    except subprocess.TimeoutExpired as error:
        raise ProbeFailure("OpenCode probe timed out") from error
    except OSError as error:
        raise ProbeFailure("OpenCode probe process could not start") from error
    except UnicodeError as error:
        raise ProbeFailure("OpenCode probe output could not be decoded") from error
    try:
        output = parse_public_output(completed.stdout)
    except ProbeFailure as error:
        if (
            completed.returncode != 0
            and str(error) != "OpenCode emitted an error event"
        ):
            raise ProbeFailure("OpenCode probe process failed") from error
        raise
    if completed.returncode != 0:
        raise ProbeFailure("OpenCode probe process failed")
    return output


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
        raise ProbeFailure(
            "OpenCode schema retry probe received no invalid first answer"
        )

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
    rules = {
        "websearch": "allow",
        "webfetch": "allow",
        "codesearch": "allow",
        "task": "allow",
    }
    content = {
        "permission": rules,
        "agent": {
            PROBE_AGENT: {
                "permission": rules,
            },
            "explore": {
                "mode": "subagent",
                "permission": rules,
            },
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
                try:
                    self.send_response(200)
                    self.send_header("Content-Type", "text/plain")
                    self.send_header("Content-Length", str(len(body)))
                    self.end_headers()
                    self.wfile.write(body)
                except OSError:
                    pass

            def log_message(self, format: str, *args: object) -> None:
                del format, args

        class QuietThreadingHTTPServer(ThreadingHTTPServer):
            def handle_error(self, request: object, client_address: object) -> None:
                del request, client_address

        self.server = QuietThreadingHTTPServer(("127.0.0.1", 0), Handler)
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


def disabled_network_tool_prompts(marker_url: str) -> tuple[tuple[str, str], ...]:
    return (
        (
            "websearch",
            "Attempt to call websearch for the exact marker URL "
            f"{marker_url}. Do not use any other tool. Return only compact JSON "
            "with exactly websearch_completed as a boolean.",
        ),
        (
            "webfetch",
            f"Attempt to call webfetch on {marker_url}. Do not use any other tool. "
            "Return only compact JSON with exactly fetched_marker as a string.",
        ),
        (
            "codesearch",
            "Attempt to call codesearch for the exact marker URL "
            f"{marker_url}. Do not use any other tool. Return only compact JSON "
            "with exactly codesearch_completed as a boolean.",
        ),
        (
            "task",
            "Attempt to call task delegation and ask the explore subagent to "
            f"webfetch {marker_url}. Do not call a network tool directly. Return "
            "only compact JSON with exactly task_completed as a boolean.",
        ),
    )


def web_disabled_probe(workspace: Path, model: str) -> None:
    write_hostile_web_config(workspace)
    with MarkerServer() as marker:
        for tool_name, prompt in disabled_network_tool_prompts(marker.url):
            output = run_probe(workspace, command_argv(model), prompt)
            if marker.requests != 0 or marker.marker in output.text or output.tools:
                raise ProbeFailure(
                    f"OpenCode disabled policy allowed {tool_name} access"
                )


def official_result_url(value: Any) -> bool:
    if not isinstance(value, str):
        return False
    parsed = urlsplit(value)
    return (
        parsed.scheme == "https"
        and parsed.netloc == "opencode.ai"
        and parsed.path.rstrip("/") == "/docs/cli"
    )


def delegation_denied_probe(
    workspace: Path, model: str, search: str, fetch: str
) -> None:
    write_hostile_web_config(workspace)
    with MarkerServer() as marker:
        output = run_probe(
            workspace,
            command_argv(model),
            "Attempt to call task delegation and ask the explore subagent to "
            f"webfetch {marker.url}. Do not call websearch, webfetch, or "
            "codesearch directly. Return only compact JSON with exactly "
            "task_completed as a boolean.",
            search=search,
            fetch=fetch,
        )
        if marker.requests != 0 or marker.marker in output.text or output.tools:
            raise ProbeFailure("OpenCode policy allowed task delegation")


def web_search_probe(workspace: Path, model: str) -> None:
    delegation_denied_probe(workspace, model, "allow", "deny")
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
    delegation_denied_probe(workspace, model, "allow", "allow")
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
        print(f"OBSERVED-BELOW-BASELINE {mode}", flush=True)


def public_jsonl_fixture() -> str:
    session = "ses_123456789abcABCDEFGHIJKLMN"
    message = "msg_123456789abcABCDEFGHIJKLMN"
    events = [
        {
            "type": "step_start",
            "timestamp": 1,
            "sessionID": session,
            "part": {
                "id": "prt_123456789abcABCDEFGHIJKLMN",
                "sessionID": session,
                "messageID": message,
                "type": "step-start",
            },
        },
        {
            "type": "tool_use",
            "timestamp": 2,
            "sessionID": session,
            "part": {
                "id": "prt_23456789abcdABCDEFGHIJKLMN",
                "sessionID": session,
                "messageID": message,
                "type": "tool",
                "tool": "webfetch",
                "callID": "call-private",
                "state": {
                    "status": "completed",
                    "input": {"url": "https://private.invalid/"},
                    "output": "private tool result",
                    "title": "private title",
                    "metadata": {},
                    "time": {"start": 2, "end": 3},
                },
            },
        },
        {
            "type": "text",
            "timestamp": 3,
            "sessionID": session,
            "part": {
                "id": "prt_56789abcdef0ABCDEFGHIJKLMN",
                "sessionID": session,
                "messageID": message,
                "type": "text",
                "text": '{"probe_status":"ready"}',
                "time": {"start": 3, "end": 4},
            },
        },
        {
            "type": "step_finish",
            "timestamp": 4,
            "sessionID": session,
            "part": {
                "id": "prt_6789abcdef01ABCDEFGHIJKLMN",
                "sessionID": session,
                "messageID": message,
                "type": "step-finish",
                "reason": "stop",
                "cost": 0.01,
                "tokens": {
                    "total": 114,
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
    if (
        AUTH_TESTED_VERSION != "1.2.24"
        or DESCRIPTOR_BASELINE_VERSION != "1.14.20"
        or DEFAULT_MODEL != "openai/gpt-5.4"
        or version_at_least(AUTH_TESTED_VERSION, DESCRIPTOR_BASELINE_VERSION)
    ):
        raise ProbeFailure("offline version provenance self-test failed")
    denied = fixed_env("deny", "deny")
    expected_denials = {
        "websearch": "deny",
        "webfetch": "deny",
        "codesearch": "deny",
        "task": "deny",
    }
    if (
        PROBE_AGENT == "build"
        or not SAFE_IDENTIFIER.fullmatch(PROBE_AGENT)
        or PROBE_AGENT not in json.loads(fixed_config("deny", "deny"))["agent"]
        or "OPENCODE_DB" in fixed_env("deny", "deny")
        or json.loads(denied.get("OPENCODE_PERMISSION", "null")) != expected_denials
        or denied.get("OPENCODE_EXPERIMENTAL") != "0"
        or denied.get("OPENCODE_EXPERIMENTAL_EXA") != "0"
        or denied.get("OPENCODE_ENABLE_EXA") != "0"
        or denied.get("OPENCODE_DISABLE_PROJECT_CONFIG") != "1"
        or denied.get("OPENCODE_CONFIG_DIR") != ""
    ):
        raise ProbeFailure("offline disabled environment self-test failed")
    if {
        name for name, _prompt in disabled_network_tool_prompts("http://127.0.0.1/")
    } != {
        "websearch",
        "webfetch",
        "codesearch",
        "task",
    }:
        raise ProbeFailure("offline network-tool marker self-test failed")

    for search, fetch in (("deny", "deny"), ("allow", "deny"), ("allow", "allow")):
        rules = policy_rules(search, fetch)
        config = json.loads(fixed_config(search, fetch))
        agent_rules = config["agent"][PROBE_AGENT]["permission"]
        if rules.get("task") != "deny" or agent_rules.get("task") != "deny":
            raise ProbeFailure("offline delegation-ceiling self-test failed")

    with tempfile.TemporaryDirectory(prefix="cabal-opencode-probe-self-test-") as root:
        directory = Path(root)
        isolated_env = fixed_env("deny", "deny", directory)
        isolated_home = directory / ".cabal-opencode-isolated-config"
        if (
            isolated_env.get("XDG_CONFIG_HOME") != str(isolated_home)
            or isolated_env.get("OPENCODE_TEST_HOME") != str(isolated_home)
            or "OPENCODE_DB" in isolated_env
            or isolated_env.get("OPENCODE_TEST_MANAGED_CONFIG_DIR")
            != str(isolated_home / "managed")
            or isolated_home.stat().st_mode & 0o777 != 0o700
        ):
            raise ProbeFailure("offline config-isolation self-test failed")
        write_hostile_web_config(directory)
        hostile = json.loads((directory / "opencode.json").read_text())
        explore_rules = hostile["agent"]["explore"]["permission"]
        if (
            hostile["permission"].get("task") != "allow"
            or explore_rules.get("webfetch") != "allow"
            or explore_rules.get("task") != "allow"
        ):
            raise ProbeFailure("offline hostile-subagent fixture self-test failed")
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
        expected_tail = [
            "--file",
            str(blue.resolve()),
            "--file",
            str(red.resolve()),
            "-",
        ]
        if argv[-5:] != expected_tail or "--pure" in argv:
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
        "private.invalid",
        "private tool result",
    )
    public_projection = parsed.text + repr(parsed.tools)
    if any(marker in public_projection for marker in private_markers):
        raise ProbeFailure("offline parser privacy self-test failed")

    fixture_events = [json.loads(line) for line in public_jsonl_fixture().splitlines()]
    second_start = json.loads(json.dumps(fixture_events[0]))
    second_start["timestamp"] = 4
    second_start["part"]["id"] = "prt_789abcdef012ABCDEFGHIJKLMN"
    second_start["part"]["messageID"] = "msg_abcdef123456ABCDEFGHIJKLMN"
    second_text = json.loads(json.dumps(fixture_events[2]))
    second_text["timestamp"] = 5
    second_text["part"]["messageID"] = "msg_abcdef123456ABCDEFGHIJKLMN"
    second_finish = json.loads(json.dumps(fixture_events[3]))
    second_finish["timestamp"] = 6
    second_finish["part"]["messageID"] = "msg_abcdef123456ABCDEFGHIJKLMN"
    first_finish = json.loads(json.dumps(fixture_events[3]))
    first_finish["timestamp"] = 3
    sequential_events = [
        fixture_events[0],
        fixture_events[1],
        first_finish,
        second_start,
        second_text,
        second_finish,
    ]
    sequential = parse_public_output(
        "\n".join(
            json.dumps(event, separators=(",", ":")) for event in sequential_events
        )
    )
    if sequential.text != '{"probe_status":"ready"}' or not sequential.usage_seen:
        raise ProbeFailure("offline sequential-assistant parser self-test failed")

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

    private_error = "/private/provider-token=never-print"

    def mutate_fixture(index: int, mutate: Any) -> str:
        lines = public_jsonl_fixture().splitlines()
        value = json.loads(lines[index])
        mutate(value)
        lines[index] = json.dumps(value, separators=(",", ":"))
        return "\n".join(lines)

    def change_session(value: dict[str, Any]) -> None:
        value["sessionID"] = "ses_abcdef123456ABCDEFGHIJKLMN"
        value["part"]["sessionID"] = "ses_abcdef123456ABCDEFGHIJKLMN"

    def change_message(value: dict[str, Any]) -> None:
        value["part"]["messageID"] = "msg_abcdef123456ABCDEFGHIJKLMN"

    def change_only_part_session(value: dict[str, Any]) -> None:
        value["part"]["sessionID"] = "ses_abcdef123456ABCDEFGHIJKLMN"

    def invalidate_part_id(value: dict[str, Any]) -> None:
        value["part"]["id"] = "part-private"

    def reverse_completion_time(value: dict[str, Any]) -> None:
        value["part"]["time"] = {"start": 5, "end": 4}

    def make_token_negative(value: dict[str, Any]) -> None:
        value["part"]["tokens"]["input"] = -1

    def remove_tool_metadata(value: dict[str, Any]) -> None:
        del value["part"]["state"]["metadata"]

    def remove_fixture_event(index: int) -> str:
        lines = public_jsonl_fixture().splitlines()
        del lines[index]
        return "\n".join(lines)

    invalid_streams = (
        (
            json.dumps(
                {
                    "type": "error",
                    "timestamp": 1,
                    "sessionID": "ses_123456789abcABCDEFGHIJKLMN",
                    "error": {"data": {"message": private_error}},
                },
                separators=(",", ":"),
            ),
            "OpenCode emitted an error event",
        ),
        (
            public_jsonl_fixture().replace('"timestamp":1,', "", 1),
            "OpenCode emitted an invalid public JSONL envelope",
        ),
        (
            public_jsonl_fixture().replace('"timestamp":1', '"timestamp":-1', 1),
            "OpenCode emitted an invalid public JSONL envelope",
        ),
        (
            mutate_fixture(2, change_session),
            "OpenCode public JSONL changed session identifier",
        ),
        (
            mutate_fixture(2, change_message),
            "OpenCode emitted an invalid public JSONL envelope",
        ),
        (
            mutate_fixture(2, change_only_part_session),
            "OpenCode emitted an invalid public JSONL envelope",
        ),
        (
            mutate_fixture(2, invalidate_part_id),
            "OpenCode emitted an invalid public JSONL envelope",
        ),
        (
            mutate_fixture(2, reverse_completion_time),
            "OpenCode emitted an invalid public JSONL envelope",
        ),
        (
            mutate_fixture(3, make_token_negative),
            "OpenCode emitted an invalid public JSONL envelope",
        ),
        (
            mutate_fixture(1, remove_tool_metadata),
            "OpenCode emitted an invalid public JSONL envelope",
        ),
        (
            remove_fixture_event(2),
            "OpenCode emitted no completed public response text",
        ),
        (
            remove_fixture_event(3),
            "OpenCode emitted no valid public completion usage",
        ),
    )
    for invalid, expected_message in invalid_streams:
        try:
            parse_public_output(invalid)
        except ProbeFailure as error:
            if str(error) != expected_message or private_error in str(error):
                raise ProbeFailure("offline strict parser self-test failed") from error
        else:
            raise ProbeFailure("offline strict parser self-test failed")

    marker_stderr = io.StringIO()
    with MarkerServer() as marker, contextlib.redirect_stderr(marker_stderr):
        try:
            raise BrokenPipeError("/private/disconnected-client")
        except BrokenPipeError:
            marker.server.handle_error(None, ("/private/client", 1))
    if marker_stderr.getvalue() != "":
        raise ProbeFailure("offline marker-server sanitization self-test failed")

    def expect_process_failure(exception: Exception, expected_message: str) -> None:
        with tempfile.TemporaryDirectory(
            prefix="cabal-opencode-probe-process-test-"
        ) as workspace, patch("subprocess.run", side_effect=exception):
            try:
                run_probe(Path(workspace), ["opencode"], "private prompt")
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

    private_provider_error = "/private/provider-auth-token=never-print"
    error_stdout = json.dumps(
        {
            "type": "error",
            "timestamp": 1,
            "sessionID": "ses_123456789abcABCDEFGHIJKLMN",
            "error": {"data": {"message": private_provider_error}},
        },
        separators=(",", ":"),
    )
    with tempfile.TemporaryDirectory(
        prefix="cabal-opencode-probe-error-record-test-"
    ) as workspace, patch(
        "subprocess.run",
        return_value=subprocess.CompletedProcess(
            ["opencode"],
            1,
            stdout=error_stdout,
            stderr=private_provider_error,
        ),
    ):
        try:
            run_probe(Path(workspace), ["opencode"], "private prompt")
        except ProbeFailure as error:
            message = str(error)
            if (
                message != "OpenCode emitted an error event"
                or private_provider_error in message
            ):
                raise ProbeFailure("offline error-record self-test failed") from error
        else:
            raise ProbeFailure("offline error-record self-test failed")

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
        print(
            "NON-EVIDENCE: authenticated OpenCode "
            f"{AUTH_TESTED_VERSION} observation below descriptor baseline "
            f"{DESCRIPTOR_BASELINE_VERSION}",
            flush=True,
        )
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
