#!/usr/bin/env python3
"""Bounded Copilot CLI 1.0.54 media and web-control probe."""

from __future__ import annotations

import argparse
import base64
import contextlib
import io
import json
import math
import os
import re
import struct
import subprocess
import sys
import tempfile
import zlib
from dataclasses import dataclass
from pathlib import Path
from typing import Any
from unittest.mock import patch


EXPECTED_VERSION = "1.0.54"
PROBE_TIMEOUT_SECONDS = 180
VERSION_TIMEOUT_SECONDS = 15
OFFICIAL_PAGE = (
    "https://raw.githubusercontent.com/github/copilot-cli/"
    "v1.0.54/LICENSE.md"
)
OFFICIAL_PAGE_MARKER = "right-title-interest"
SESSION_ID = re.compile(
    r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
)
SAFE_TOOL_NAME = re.compile(r"^[A-Za-z0-9_.:-]{1,128}$")
MODES = (
    "protocol",
    "media-png",
    "media-jpeg",
    "media",
    "web-disabled",
    "web-exact-url",
)


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


@dataclass(frozen=True)
class ToolObservation:
    call_id: str
    name: str
    success: bool | None


@dataclass(frozen=True)
class PublicOutput:
    session_id: str
    text: str
    tools: tuple[ToolObservation, ...]
    output_tokens: int


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


def write_fixtures(directory: Path) -> tuple[Path, Path]:
    png = directory / "attachment one.png"
    jpeg = directory / "attachment two.jpg"
    png.write_bytes(solid_png(20, 45, 225))
    jpeg.write_bytes(base64.b64decode(RED_JPEG, validate=True))
    png.chmod(0o600)
    jpeg.chmod(0o600)
    return png, jpeg


def require_version() -> None:
    try:
        completed = subprocess.run(
            ["copilot", "--version"],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=VERSION_TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired as error:
        raise ProbeFailure("Copilot version check timed out") from error
    except OSError as error:
        raise ProbeFailure("Copilot version process could not start") from error
    except UnicodeError as error:
        raise ProbeFailure("Copilot version output could not be decoded") from error
    match = re.search(r"GitHub Copilot CLI ([0-9]+\.[0-9]+\.[0-9]+)\.", completed.stdout)
    if completed.returncode != 0 or match is None or match.group(1) != EXPECTED_VERSION:
        raise ProbeFailure(f"probe requires exactly Copilot CLI {EXPECTED_VERSION}")


def common_argv(*, web_tools: bool) -> list[str]:
    tools = ["view", "grep", "glob"]
    if web_tools:
        tools.append("web_fetch")
    argv = [
        "copilot",
        "--prefer-version",
        EXPECTED_VERSION,
        "--no-auto-update",
        "--no-remote",
        "--no-experimental",
        "--no-ask-user",
        "--disable-builtin-mcps",
        "--output-format",
        "json",
        "--stream",
        "off",
        "--available-tools=" + ",".join(tools),
        "--allow-all-tools",
        "--deny-tool=shell",
        "--deny-tool=write",
        "--deny-tool=memory",
        "--disallow-temp-dir",
    ]
    return argv


def invocation_argv(
    *,
    prompt: str,
    attachments: tuple[Path, ...] = (),
    allowed_url: str | None = None,
) -> list[str]:
    web_tools = allowed_url is not None
    argv = common_argv(web_tools=web_tools)
    if allowed_url is None:
        argv.append("--deny-tool=url")
    else:
        argv.append(f"--allow-url={allowed_url}")
    for directory in sorted({attachment.resolve().parent for attachment in attachments}):
        argv.extend(("--add-dir", str(directory)))
    argv.extend(("--model", "claude-haiku-4.5"))
    for attachment in attachments:
        argv.extend(("--attachment", str(attachment.resolve())))
    argv.extend(("-p", prompt))
    return argv


def nonnegative_int(value: Any) -> int | None:
    if isinstance(value, bool) or not isinstance(value, int) or value < 0:
        return None
    return value


def finite_nonnegative(value: Any) -> float | None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None
    number = float(value)
    if number < 0 or not math.isfinite(number):
        return None
    return number


def safe_id(value: Any) -> str | None:
    if isinstance(value, str) and SAFE_TOOL_NAME.fullmatch(value):
        return value
    return None


def require_object(value: Any) -> dict[str, Any]:
    if not isinstance(value, dict):
        raise ProbeFailure("Copilot emitted an invalid public JSONL record")
    return value


def require_string(value: Any) -> str:
    if not isinstance(value, str):
        raise ProbeFailure("Copilot emitted an invalid public JSONL record")
    return value


def require_safe_id(value: Any) -> str:
    identifier = safe_id(value)
    if identifier is None:
        raise ProbeFailure("Copilot emitted an invalid public JSONL record")
    return identifier


def validate_usage(value: Any) -> None:
    usage = require_object(value)
    if set(usage) != {
        "codeChanges",
        "premiumRequests",
        "sessionDurationMs",
        "totalApiDurationMs",
    }:
        raise ProbeFailure("Copilot emitted an invalid terminal result")
    code_changes = require_object(usage["codeChanges"])
    if set(code_changes) != {"filesModified", "linesAdded", "linesRemoved"}:
        raise ProbeFailure("Copilot emitted an invalid terminal result")
    files = code_changes["filesModified"]
    if not isinstance(files, list) or not all(isinstance(item, str) for item in files):
        raise ProbeFailure("Copilot emitted an invalid terminal result")
    if (
        nonnegative_int(code_changes["linesAdded"]) is None
        or nonnegative_int(code_changes["linesRemoved"]) is None
        or finite_nonnegative(usage["premiumRequests"]) is None
        or nonnegative_int(usage["sessionDurationMs"]) is None
        or nonnegative_int(usage["totalApiDurationMs"]) is None
    ):
        raise ProbeFailure("Copilot emitted an invalid terminal result")


def validate_ignored_record(record_type: str, data: dict[str, Any]) -> None:
    if record_type == "session.mcp_servers_loaded":
        servers = data.get("servers")
        if not isinstance(servers, list):
            raise ProbeFailure("Copilot emitted an invalid public JSONL record")
    elif record_type == "session.skills_loaded":
        skills = data.get("skills")
        if not isinstance(skills, list):
            raise ProbeFailure("Copilot emitted an invalid public JSONL record")
    elif record_type == "session.info":
        require_string(data.get("infoType"))
        require_string(data.get("message"))
    elif record_type == "session.tools_updated":
        require_string(data.get("model"))
    elif record_type == "assistant.reasoning":
        require_string(data.get("content"))
        require_string(data.get("reasoningId"))
    else:
        raise ProbeFailure("Copilot emitted an unsupported public JSONL record")


def protocol_summary(stdout: str) -> tuple[str, ...]:
    """Return only safe record-shape metadata for initial protocol inspection."""
    def shape(value: Any, key: str = "") -> str:
        if value is None:
            return "null"
        if isinstance(value, bool):
            return "bool"
        if isinstance(value, int):
            return "int"
        if isinstance(value, float):
            return "float"
        if isinstance(value, str):
            if key.lower().endswith("id"):
                if not value:
                    return "string[empty-id]"
                if SESSION_ID.fullmatch(value):
                    return "string[uuid]"
                if safe_id(value):
                    return "string[safe-id]"
                return f"string[opaque-id-length-{len(value)}]"
            return "string"
        if isinstance(value, list):
            members = sorted({shape(item, key) for item in value})
            return "list[" + "|".join(members) + "]"
        if isinstance(value, dict):
            fields = [
                f"{key}:{shape(item, key)}"
                for key, item in sorted(value.items())
                if safe_id(key)
            ]
            return "object{" + ",".join(fields) + "}"
        return "invalid"

    summary: list[str] = []
    for line in stdout.splitlines():
        if not line.strip():
            continue
        try:
            record = json.loads(line)
        except (json.JSONDecodeError, UnicodeError) as error:
            raise ProbeFailure("Copilot emitted malformed public JSONL") from error
        if not isinstance(record, dict):
            raise ProbeFailure("Copilot emitted malformed public JSONL")
        record_type = record.get("type")
        safe_type = safe_id(record_type) or "<invalid-type>"
        details = ""
        data = record.get("data")
        if safe_type == "tool.execution_start" and isinstance(data, dict):
            tool_name = safe_id(data.get("toolName"))
            if tool_name is not None:
                details = f";tool={tool_name}"
        elif safe_type == "tool.execution_complete" and isinstance(data, dict):
            success = data.get("success")
            if isinstance(success, bool):
                details = f";success={str(success).lower()}"
        summary.append(f"type={safe_type};shape={shape(record)}{details}")
    return tuple(summary)


def parse_public_output(stdout: str) -> PublicOutput:
    records: list[dict[str, Any]] = []
    for line in stdout.splitlines():
        if not line.strip():
            continue
        try:
            records.append(require_object(json.loads(line)))
        except (json.JSONDecodeError, UnicodeError) as error:
            raise ProbeFailure("Copilot emitted malformed public JSONL") from error
    if not records:
        raise ProbeFailure("Copilot emitted no public JSONL records")
    result_records = [record for record in records if record.get("type") == "result"]
    if len(result_records) != 1 or records[-1] is not result_records[0]:
        raise ProbeFailure("Copilot emitted no exact terminal success")

    seen_event_ids: set[str] = set()
    seen_user_message = False
    active_turn: str | None = None
    active_interaction: str | None = None
    turns_finished = 0
    final_text: str | None = None
    output_tokens = 0
    requested_tools: dict[str, str] = {}
    started_tools: dict[str, str] = {}
    finished_tools: set[str] = set()
    observations: list[ToolObservation] = []

    ignored = {
        "session.mcp_servers_loaded",
        "session.skills_loaded",
        "session.info",
        "session.tools_updated",
        "assistant.reasoning",
    }
    for index, record in enumerate(records):
        record_type = require_safe_id(record.get("type"))
        if record_type == "result":
            if index != len(records) - 1:
                raise ProbeFailure("Copilot emitted records after terminal success")
            if set(record) != {"type", "timestamp", "exitCode", "sessionId", "usage"}:
                raise ProbeFailure("Copilot emitted an invalid terminal result")
            if record.get("exitCode") != 0 or active_turn is not None or turns_finished == 0:
                raise ProbeFailure("Copilot emitted no exact terminal success")
            session_id = require_string(record.get("sessionId"))
            if not SESSION_ID.fullmatch(session_id):
                raise ProbeFailure("Copilot emitted an invalid terminal session")
            validate_usage(record.get("usage"))
            if final_text is None or not final_text.strip():
                raise ProbeFailure("Copilot emitted no final public assistant text")
            if set(requested_tools) != set(started_tools) or set(started_tools) != finished_tools:
                raise ProbeFailure("Copilot emitted an incomplete public tool lifecycle")
            return PublicOutput(
                session_id=session_id,
                text=final_text,
                tools=tuple(observations),
                output_tokens=output_tokens,
            )

        if "result" in record_type or "error" in record_type:
            raise ProbeFailure("Copilot emitted an error record")
        required_top = {"type", "id", "parentId", "timestamp", "data"}
        if not required_top.issubset(record):
            raise ProbeFailure("Copilot emitted an invalid public JSONL record")
        event_id = require_safe_id(record.get("id"))
        if event_id in seen_event_ids:
            raise ProbeFailure("Copilot emitted a duplicate public JSONL record")
        seen_event_ids.add(event_id)
        require_string(record.get("parentId"))
        require_string(record.get("timestamp"))
        data = require_object(record.get("data"))

        if record_type in ignored:
            if record_type != "assistant.reasoning" and seen_user_message:
                raise ProbeFailure("Copilot emitted an invalid public JSONL order")
            if record_type == "assistant.reasoning" and active_turn is None:
                raise ProbeFailure("Copilot emitted an invalid public JSONL order")
            validate_ignored_record(record_type, data)
            continue
        if record_type == "user.message":
            if seen_user_message or active_turn is not None or turns_finished != 0:
                raise ProbeFailure("Copilot emitted an invalid public JSONL order")
            attachments = data.get("attachments")
            if not isinstance(attachments, list):
                raise ProbeFailure("Copilot emitted an invalid public JSONL record")
            require_string(data.get("content"))
            require_safe_id(data.get("interactionId"))
            seen_user_message = True
            continue
        if record_type == "assistant.turn_start":
            if not seen_user_message or active_turn is not None:
                raise ProbeFailure("Copilot emitted an invalid public JSONL order")
            active_turn = require_safe_id(data.get("turnId"))
            active_interaction = require_safe_id(data.get("interactionId"))
            final_text = None
            continue
        if record_type == "assistant.message":
            if active_turn is None or data.get("turnId") != active_turn:
                raise ProbeFailure("Copilot emitted an invalid public JSONL order")
            if data.get("interactionId") != active_interaction:
                raise ProbeFailure("Copilot emitted an invalid public JSONL record")
            content = require_string(data.get("content"))
            require_safe_id(data.get("messageId"))
            tokens = nonnegative_int(data.get("outputTokens"))
            requests = data.get("toolRequests")
            if tokens is None or not isinstance(requests, list):
                raise ProbeFailure("Copilot emitted an invalid public JSONL record")
            output_tokens += tokens
            if requests:
                final_text = None
                for request_value in requests:
                    request = require_object(request_value)
                    call_id = require_safe_id(request.get("toolCallId"))
                    name = require_safe_id(request.get("name"))
                    if call_id in requested_tools:
                        raise ProbeFailure("Copilot emitted a duplicate public tool call")
                    requested_tools[call_id] = name
            elif content.strip():
                final_text = content
            else:
                final_text = None
            continue
        if record_type == "tool.execution_start":
            if active_turn is None or data.get("turnId") != active_turn:
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            call_id = require_safe_id(data.get("toolCallId"))
            name = require_safe_id(data.get("toolName"))
            if requested_tools.get(call_id) != name or call_id in started_tools:
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            started_tools[call_id] = name
            continue
        if record_type == "tool.execution_complete":
            if active_turn is None or data.get("turnId") != active_turn:
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            call_id = require_safe_id(data.get("toolCallId"))
            success = data.get("success")
            if call_id not in started_tools or call_id in finished_tools:
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            if success is not True:
                raise ProbeFailure("Copilot emitted a failed public tool result")
            finished_tools.add(call_id)
            observations.append(ToolObservation(call_id, started_tools[call_id], True))
            continue
        if record_type == "assistant.turn_end":
            if active_turn is None or data.get("turnId") != active_turn:
                raise ProbeFailure("Copilot emitted an invalid public JSONL order")
            active_turn = None
            active_interaction = None
            turns_finished += 1
            continue
        raise ProbeFailure("Copilot emitted an unsupported public JSONL record")
    raise ProbeFailure("Copilot emitted no exact terminal success")


def prompt_for_mode(mode: str) -> str:
    if mode == "protocol":
        return "Return only the word ready. Do not use tools."
    if mode in {"media", "media-swapped-control"}:
        return (
            "Inspect the two attached images in their displayed order. Return only a "
            "JSON object with exactly the string fields first_image_dominant_color and "
            "second_image_dominant_color. Use one lowercase English color word for each."
        )
    if mode == "media-omitted-control":
        return (
            "No attachments are intentionally supplied. Do not use tools. "
            "Return only the word no-attachments."
        )
    if mode in {"media-png", "media-jpeg"}:
        return (
            "Inspect the attached image. Return only a JSON object with exactly the "
            "string field dominant_color. Use one lowercase English color word."
        )
    if mode == "web-disabled":
        return (
            f"Use the web fetch tool to read {OFFICIAL_PAGE}. If the tool is unavailable "
            "or denied, return only the word blocked. Do not answer from memory."
        )
    if mode == "web-exact-url":
        return (
            f"Use web fetch to read {OFFICIAL_PAGE}. In section 4, identify the three "
            "comma-separated nouns describing what GitHub and its licensors retain. Return "
            "only a JSON object with exactly the string field reservation_triplet. Its value "
            "must be those lowercase nouns joined by hyphens in source order. Do not answer "
            "from memory."
        )
    raise ProbeFailure("unknown probe mode")


def parse_answer(text: str) -> dict[str, Any]:
    text = text.strip()
    if text.startswith("```json") and text.endswith("```"):
        text = text[7:-3].strip()
    elif text.startswith("```") and text.endswith("```"):
        text = text[3:-3].strip()
    try:
        answer = json.loads(text)
    except (json.JSONDecodeError, UnicodeError) as error:
        raise ProbeFailure(
            "Copilot emitted no exact structured public answer "
            f"(object-like={text.startswith('{') and text.endswith('}')}, "
            f"marker-seen={OFFICIAL_PAGE_MARKER in text.lower()}, length={len(text)})"
        ) from error
    if not isinstance(answer, dict):
        raise ProbeFailure("Copilot emitted no exact structured public answer")
    return answer


def validate_public_output(mode: str, output: PublicOutput) -> None:
    completed_tool_names = [tool.name for tool in output.tools if tool.success is True]
    if mode == "protocol":
        if output.text.strip().lower() != "ready" or completed_tool_names:
            raise ProbeFailure("Copilot protocol control failed")
        return
    if mode == "media-png":
        if parse_answer(output.text) != {"dominant_color": "blue"}:
            raise ProbeFailure("Copilot PNG answer failed the content assertion")
        if completed_tool_names != ["view"]:
            raise ProbeFailure("Copilot PNG attachment had no exact tool lifecycle")
        return
    if mode == "media-jpeg":
        if parse_answer(output.text) != {"dominant_color": "red"}:
            raise ProbeFailure("Copilot JPEG answer failed the content assertion")
        if completed_tool_names != ["view"]:
            raise ProbeFailure("Copilot JPEG attachment had no exact tool lifecycle")
        return
    if mode == "media":
        expected = {
            "first_image_dominant_color": "blue",
            "second_image_dominant_color": "red",
        }
        if parse_answer(output.text) != expected:
            raise ProbeFailure("Copilot media answer failed the content assertion")
        if completed_tool_names != ["view", "view"]:
            raise ProbeFailure("Copilot media attachments had no exact tool lifecycles")
        return
    if mode == "media-swapped-control":
        expected = {
            "first_image_dominant_color": "red",
            "second_image_dominant_color": "blue",
        }
        if parse_answer(output.text) != expected:
            raise ProbeFailure("Copilot swapped-media control failed")
        if completed_tool_names != ["view", "view"]:
            raise ProbeFailure("Copilot swapped-media control had no exact tool lifecycles")
        return
    if mode == "media-omitted-control":
        if output.text.strip().lower() != "no-attachments" or completed_tool_names:
            raise ProbeFailure("Copilot omitted-media control failed")
        return
    if mode == "web-disabled":
        if output.text.strip().lower() != "blocked" or completed_tool_names:
            raise ProbeFailure("Copilot Web_disabled control failed")
        return
    if mode == "web-exact-url":
        if OFFICIAL_PAGE_MARKER not in output.text.lower():
            raise ProbeFailure("Copilot exact-URL answer failed the content assertion")
        if completed_tool_names != ["web_fetch"]:
            raise ProbeFailure("Copilot exact-URL mode had no exact tool lifecycle")
        return
    raise ProbeFailure("unknown probe mode")


def redacted_diagnostic(stderr: str, workspace: Path, argv: list[str]) -> str:
    redacted = stderr
    for value in sorted(argv, key=len, reverse=True):
        if value.startswith("/") or value.startswith("http") or " " in value:
            redacted = redacted.replace(value, "<redacted>")
    redacted = redacted.replace(str(workspace.resolve()), "<workspace>")
    redacted = re.sub(r"(?<![A-Za-z0-9])/(?:[^\s:'\"]+/?)+", "<path>", redacted)
    redacted = re.sub(r"[A-Za-z0-9_-]{32,}", "<opaque>", redacted)
    return " ".join(redacted.split())[:300]


def run_invocation(workspace: Path, argv: list[str], debug_protocol: bool) -> str:
    env = os.environ.copy()
    env["NO_COLOR"] = "1"
    env["COPILOT_AUTO_UPDATE"] = "false"
    env["COPILOT_DISABLE_TERMINAL_TITLE"] = "1"
    for name in (
        "COPILOT_ALLOW_ALL",
        "COPILOT_ALLOW_ALL_PATHS",
        "COPILOT_ALLOW_ALL_URLS",
    ):
        env.pop(name, None)
    with tempfile.TemporaryDirectory(prefix="cabal-copilot-config-") as raw_config:
        config_home = Path(raw_config)
        config_home.chmod(0o700)
        env["COPILOT_HOME"] = str(config_home)
        try:
            completed = subprocess.run(
                argv,
                cwd=workspace,
                check=False,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                timeout=PROBE_TIMEOUT_SECONDS,
                env=env,
            )
        except subprocess.TimeoutExpired as error:
            raise ProbeFailure("Copilot probe timed out") from error
        except OSError as error:
            raise ProbeFailure("Copilot probe process could not start") from error
        except UnicodeError as error:
            raise ProbeFailure("Copilot probe output could not be decoded") from error
    if completed.returncode != 0:
        lowered = completed.stderr.lower()
        category = next(
            (
                label
                for needle, label in (
                    ("auth", "authentication"),
                    ("unsupported file type", "unsupported-file-type"),
                    ("unsupported mime", "unsupported-mime"),
                    ("not found", "not-found"),
                    ("does not exist", "not-found"),
                    ("failed to read", "read"),
                    ("failed to process", "processing"),
                    ("too large", "too-large"),
                    ("attachment", "attachment"),
                    ("image", "image"),
                    ("permission", "permission"),
                    ("unknown option", "option"),
                    ("invalid", "invalid-input"),
                    ("model", "model"),
                )
                if needle in lowered
            ),
            "unknown",
        )
        detail = redacted_diagnostic(completed.stderr, workspace, argv)
        suffix = f": {detail}" if debug_protocol and detail else ""
        raise ProbeFailure(
            f"Copilot probe exited unsuccessfully ({category}){suffix}"
        )
    return completed.stdout


def run_mode(mode: str, debug_protocol: bool) -> None:
    require_version()
    with tempfile.TemporaryDirectory(prefix="cabal-copilot-probe-") as raw_workspace:
        workspace = Path(raw_workspace)
        workspace.chmod(0o700)
        github_config = workspace / ".github"
        github_config.mkdir(mode=0o700)
        (github_config / "copilot-instructions.md").write_text(
            "# Managed probe instructions\n", encoding="utf-8"
        )
        (github_config / "copilot").mkdir(mode=0o700)
        (github_config / "copilot" / "settings.json").write_text(
            "{}\n", encoding="utf-8"
        )
        (github_config / "lsp.json").write_text(
            '{"lspServers":{}}\n', encoding="utf-8"
        )
        (github_config / "mcp.json").write_text(
            '{"mcpServers":{}}\n', encoding="utf-8"
        )
        attachments: tuple[Path, ...] = ()
        if mode in {"media", "media-png", "media-jpeg"}:
            png, jpeg = write_fixtures(workspace)
            attachments = {
                "media": (png, jpeg),
                "media-png": (png,),
                "media-jpeg": (jpeg,),
            }[mode]

        def run_case(case: str, case_attachments: tuple[Path, ...]) -> None:
            allowed_url = OFFICIAL_PAGE if case == "web-exact-url" else None
            argv = invocation_argv(
                prompt=prompt_for_mode(case),
                attachments=case_attachments,
                allowed_url=allowed_url,
            )
            stdout = run_invocation(workspace, argv, debug_protocol)
            if debug_protocol:
                for item in protocol_summary(stdout):
                    print(item)
            summary = protocol_summary(stdout)
            if not summary:
                raise ProbeFailure("Copilot emitted no public JSONL records")
            validate_public_output(case, parse_public_output(stdout))

        run_case(mode, attachments)
        if mode == "media":
            run_case("media-swapped-control", tuple(reversed(attachments)))
            for attachment in attachments:
                attachment.unlink()
            run_case("media-omitted-control", ())


def selftest() -> None:
    prompt = "opaque prompt"
    first = Path("/tmp/probe attachment one.png")
    second = Path("/tmp/probe-attachment-two.jpg")
    argv = invocation_argv(prompt=prompt, attachments=(first, second))
    assert argv.count("--attachment") == 2
    prompt_index = argv.index("-p")
    assert argv[prompt_index + 1] == prompt
    assert "--yolo" not in argv
    assert "--allow-all-paths" not in argv
    assert "--allow-all-urls" not in argv
    assert "--deny-tool=url" in argv
    assert protocol_summary('{"type":"result","data":{"content":"secret"}}\n') == (
        "type=result;shape=object{data:object{content:string},type:string}",
    )
    with contextlib.redirect_stderr(io.StringIO()) as stderr:
        with patch.object(subprocess, "run", side_effect=OSError("sensitive")):
            try:
                require_version()
            except ProbeFailure as error:
                print(error, file=sys.stderr)
    assert "sensitive" not in stderr.getvalue()
    print("PASS selftest")


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = SafeArgumentParser(add_help=True)
    parser.add_argument("mode", choices=("selftest",) + MODES)
    parser.add_argument("--debug-protocol", action="store_true")
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    try:
        args = parse_args(argv)
        if args.mode == "selftest":
            selftest()
        else:
            run_mode(args.mode, args.debug_protocol)
            print(f"PASS {args.mode}")
        return 0
    except ProbeFailure as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1
    except (AssertionError, OSError, ValueError, UnicodeError):
        print("FAIL: probe selftest failed", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
