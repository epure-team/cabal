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
    "https://raw.githubusercontent.com/github/copilot-cli/" "v1.0.54/LICENSE.md"
)
OFFICIAL_PAGE_MARKER = "right-title-interest"
SESSION_ID = re.compile(
    r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
)
SAFE_TOOL_NAME = re.compile(r"^[A-Za-z0-9_.:-]{1,128}$")
PUBLIC_TOOL_NAMES = frozenset(("view", "grep", "glob", "web_fetch"))
MODES = (
    "protocol",
    "media-png",
    "media-jpeg",
    "media",
    "web-disabled",
    "web-exact-url",
)
OCAML_MAX_INT = (1 << (struct.calcsize("P") * 8 - 2)) - 1


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
    match = re.search(
        r"GitHub Copilot CLI ([0-9]+\.[0-9]+\.[0-9]+)\.", completed.stdout
    )
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
    for directory in sorted(
        {attachment.resolve().parent for attachment in attachments}
    ):
        argv.extend(("--add-dir", str(directory)))
    argv.extend(("--model", "claude-haiku-4.5"))
    for attachment in attachments:
        argv.extend(("--attachment", str(attachment.resolve())))
    argv.extend(("-p", prompt))
    return argv


def nonnegative_int(value: Any) -> int | None:
    if (
        isinstance(value, bool)
        or not isinstance(value, int)
        or value < 0
        or value > OCAML_MAX_INT
    ):
        return None
    return value


def finite_nonnegative(value: Any) -> float | None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None
    if isinstance(value, int) and value > OCAML_MAX_INT:
        return None
    try:
        number = float(value)
    except OverflowError:
        return None
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


def reject_duplicate_object_fields(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    value: dict[str, Any] = {}
    for name, member in pairs:
        if name in value:
            raise ProbeFailure("Copilot emitted an invalid public JSONL record")
        value[name] = member
    return value


def load_public_json(line: str) -> Any:
    return json.loads(line, object_pairs_hook=reject_duplicate_object_fields)


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
    if files != []:
        raise ProbeFailure("Copilot emitted an invalid terminal result")
    if (
        nonnegative_int(code_changes["linesAdded"]) != 0
        or nonnegative_int(code_changes["linesRemoved"]) != 0
        or finite_nonnegative(usage["premiumRequests"]) is None
        or nonnegative_int(usage["sessionDurationMs"]) is None
        or nonnegative_int(usage["totalApiDurationMs"]) is None
    ):
        raise ProbeFailure("Copilot emitted an invalid terminal result")


def validate_ignored_record(record_type: str, data: dict[str, Any]) -> None:
    if record_type == "session.mcp_servers_loaded":
        if set(data) != {"servers"} or data["servers"] != []:
            raise ProbeFailure("Copilot emitted an invalid public JSONL record")
    elif record_type == "session.skills_loaded":
        if set(data) != {"skills"}:
            raise ProbeFailure("Copilot emitted an invalid public JSONL record")
        skills = data.get("skills")
        if not isinstance(skills, list):
            raise ProbeFailure("Copilot emitted an invalid public JSONL record")
    elif record_type == "session.info":
        if set(data) != {"infoType", "message"}:
            raise ProbeFailure("Copilot emitted an invalid public JSONL record")
        require_string(data.get("infoType"))
        require_string(data.get("message"))
    elif record_type == "session.tools_updated":
        if set(data) != {"model"}:
            raise ProbeFailure("Copilot emitted an invalid public JSONL record")
        require_string(data.get("model"))
    elif record_type == "assistant.reasoning":
        if set(data) != {"content", "reasoningId"}:
            raise ProbeFailure("Copilot emitted an invalid public JSONL record")
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
            record = load_public_json(line)
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
            records.append(require_object(load_public_json(line)))
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
    requested_tools: dict[str, tuple[str, str]] = {}
    started_tools: dict[str, tuple[str, str]] = {}
    finished_tools: set[tuple[str, str]] = set()
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
            if (
                nonnegative_int(record.get("exitCode")) != 0
                or active_turn is not None
                or turns_finished == 0
            ):
                raise ProbeFailure("Copilot emitted no exact terminal success")
            session_id = require_string(record.get("sessionId"))
            if not SESSION_ID.fullmatch(session_id):
                raise ProbeFailure("Copilot emitted an invalid terminal session")
            validate_usage(record.get("usage"))
            if final_text is None or not final_text.strip():
                raise ProbeFailure("Copilot emitted no final public assistant text")
            if (
                set(requested_tools) != set(started_tools)
                or {
                    (call_id, turn_id)
                    for call_id, (_name, turn_id) in started_tools.items()
                }
                != finished_tools
            ):
                raise ProbeFailure(
                    "Copilot emitted an incomplete public tool lifecycle"
                )
            return PublicOutput(
                session_id=session_id,
                text=final_text,
                tools=tuple(observations),
                output_tokens=output_tokens,
            )

        if "result" in record_type or "error" in record_type:
            raise ProbeFailure("Copilot emitted an error record")
        required_top = {"type", "id", "parentId", "timestamp", "data"}
        if set(record) != required_top:
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
            if set(data) != {"attachments", "content", "interactionId"}:
                raise ProbeFailure("Copilot emitted an invalid public JSONL record")
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
            if set(data) != {"interactionId", "turnId"}:
                raise ProbeFailure("Copilot emitted an invalid public JSONL record")
            active_turn = require_safe_id(data.get("turnId"))
            active_interaction = require_safe_id(data.get("interactionId"))
            final_text = None
            continue
        if record_type == "assistant.message":
            if active_turn is None or data.get("turnId") != active_turn:
                raise ProbeFailure("Copilot emitted an invalid public JSONL order")
            if set(data) != {
                "content",
                "interactionId",
                "messageId",
                "outputTokens",
                "toolRequests",
                "turnId",
            }:
                raise ProbeFailure("Copilot emitted an invalid public JSONL record")
            if data.get("interactionId") != active_interaction:
                raise ProbeFailure("Copilot emitted an invalid public JSONL record")
            content = require_string(data.get("content"))
            require_safe_id(data.get("messageId"))
            tokens = nonnegative_int(data.get("outputTokens"))
            requests = data.get("toolRequests")
            if tokens is None or not isinstance(requests, list):
                raise ProbeFailure("Copilot emitted an invalid public JSONL record")
            if output_tokens > OCAML_MAX_INT - tokens:
                raise ProbeFailure("Copilot emitted an invalid public JSONL record")
            output_tokens += tokens
            if requests:
                final_text = None
                for request_value in requests:
                    request = require_object(request_value)
                    if set(request) != {"toolCallId", "name", "arguments"}:
                        raise ProbeFailure(
                            "Copilot emitted an invalid public tool lifecycle"
                        )
                    call_id = require_safe_id(request.get("toolCallId"))
                    name = require_safe_id(request.get("name"))
                    if name not in PUBLIC_TOOL_NAMES:
                        raise ProbeFailure(
                            "Copilot emitted an invalid public tool lifecycle"
                        )
                    require_object(request.get("arguments"))
                    if call_id in requested_tools:
                        raise ProbeFailure(
                            "Copilot emitted a duplicate public tool call"
                        )
                    requested_tools[call_id] = (name, active_turn)
            elif content.strip():
                final_text = content
            else:
                final_text = None
            continue
        if record_type == "tool.execution_start":
            if active_turn is None or data.get("turnId") != active_turn:
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            if set(data) != {"turnId", "toolCallId", "toolName"}:
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            call_id = require_safe_id(data.get("toolCallId"))
            name = require_safe_id(data.get("toolName"))
            if name not in PUBLIC_TOOL_NAMES:
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            if (
                requested_tools.get(call_id) != (name, active_turn)
                or call_id in started_tools
            ):
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            started_tools[call_id] = (name, active_turn)
            continue
        if record_type == "tool.execution_complete":
            if active_turn is None or data.get("turnId") != active_turn:
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            if set(data) not in (
                {"turnId", "toolCallId", "success"},
                {"turnId", "toolCallId", "toolName", "success"},
            ):
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            call_id = require_safe_id(data.get("toolCallId"))
            success = data.get("success")
            started = started_tools.get(call_id)
            if started is None or (call_id, active_turn) in finished_tools:
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            name, started_turn = started
            if started_turn != active_turn or (
                "toolName" in data and data["toolName"] != name
            ):
                raise ProbeFailure("Copilot emitted an invalid public tool lifecycle")
            if success is not True:
                raise ProbeFailure("Copilot emitted a failed public tool result")
            finished_tools.add((call_id, active_turn))
            observations.append(ToolObservation(call_id, name, True))
            continue
        if record_type == "assistant.turn_end":
            if active_turn is None or data.get("turnId") != active_turn:
                raise ProbeFailure("Copilot emitted an invalid public JSONL order")
            if set(data) != {"turnId"}:
                raise ProbeFailure("Copilot emitted an invalid public JSONL record")
            if any(
                request_turn == active_turn
                and (call_id, active_turn) not in finished_tools
                for call_id, (_name, request_turn) in requested_tools.items()
            ):
                raise ProbeFailure(
                    "Copilot emitted an incomplete public tool lifecycle"
                )
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
            raise ProbeFailure(
                "Copilot swapped-media control had no exact tool lifecycles"
            )
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
        if parse_answer(output.text) != {"reservation_triplet": OFFICIAL_PAGE_MARKER}:
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
        raise ProbeFailure(f"Copilot probe exited unsuccessfully ({category}){suffix}")
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
        (github_config / "lsp.json").write_text('{"lspServers":{}}\n', encoding="utf-8")
        (github_config / "mcp.json").write_text('{"mcpServers":{}}\n', encoding="utf-8")
        (workspace / ".mcp.json").write_text('{"mcpServers":{}}\n', encoding="utf-8")
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

        if mode == "web-disabled":
            # A successful exact-URL fetch first proves that credentials,
            # network access, and the web tool work. The following separately
            # configured invocation must then demonstrate the active denial.
            run_case("web-exact-url", ())
            run_case("web-disabled", ())
        else:
            run_case(mode, attachments)
        if mode == "media":
            run_case("media-swapped-control", tuple(reversed(attachments)))
            for attachment in attachments:
                attachment.unlink()
            run_case("media-omitted-control", ())


def public_event(index: int, record_type: str, data: dict[str, Any]) -> dict[str, Any]:
    return {
        "type": record_type,
        "id": f"event-{index}",
        "parentId": "00000000-0000-0000-0000-000000000000",
        "timestamp": "2026-09-04T12:00:00.000Z",
        "data": data,
    }


def public_jsonl_fixture(
    text: str,
    tools: tuple[str, ...] = (),
    *,
    mcp_servers: list[Any] | None = None,
) -> str:
    interaction_id = "interaction-1"
    turn_id = "turn-1"
    records: list[dict[str, Any]] = [
        public_event(
            1,
            "session.mcp_servers_loaded",
            {"servers": [] if mcp_servers is None else mcp_servers},
        ),
        public_event(
            2,
            "user.message",
            {
                "attachments": [],
                "content": "private prompt",
                "interactionId": interaction_id,
            },
        ),
        public_event(
            3,
            "assistant.turn_start",
            {"interactionId": interaction_id, "turnId": turn_id},
        ),
    ]
    event_index = 4
    for tool_index, tool_name in enumerate(tools, start=1):
        call_id = f"call-{tool_index}"
        records.extend(
            (
                public_event(
                    event_index,
                    "assistant.message",
                    {
                        "content": "",
                        "interactionId": interaction_id,
                        "messageId": f"message-{event_index}",
                        "outputTokens": 1,
                        "toolRequests": [
                            {
                                "toolCallId": call_id,
                                "name": tool_name,
                                "arguments": {"path": "/private/never-print"},
                            }
                        ],
                        "turnId": turn_id,
                    },
                ),
                public_event(
                    event_index + 1,
                    "tool.execution_start",
                    {
                        "turnId": turn_id,
                        "toolCallId": call_id,
                        "toolName": tool_name,
                    },
                ),
                public_event(
                    event_index + 2,
                    "tool.execution_complete",
                    {"turnId": turn_id, "toolCallId": call_id, "success": True},
                ),
            )
        )
        event_index += 3
    records.extend(
        (
            public_event(
                event_index,
                "assistant.message",
                {
                    "content": text,
                    "interactionId": interaction_id,
                    "messageId": f"message-{event_index}",
                    "outputTokens": 7,
                    "toolRequests": [],
                    "turnId": turn_id,
                },
            ),
            public_event(event_index + 1, "assistant.turn_end", {"turnId": turn_id}),
            {
                "type": "result",
                "timestamp": "2026-09-04T12:00:01.000Z",
                "exitCode": 0,
                "sessionId": "123e4567-e89b-12d3-a456-426614174000",
                "usage": {
                    "codeChanges": {
                        "filesModified": [],
                        "linesAdded": 0,
                        "linesRemoved": 0,
                    },
                    "premiumRequests": 1.0,
                    "sessionDurationMs": 1000,
                    "totalApiDurationMs": 500,
                },
            },
        )
    )
    return "\n".join(json.dumps(record, separators=(",", ":")) for record in records)


def expect_probe_failure(action: Any, label: str) -> None:
    try:
        action()
    except ProbeFailure:
        return
    raise ProbeFailure(f"offline {label} self-test failed")


def mutate_public_fixture(fixture: str, mutation: Any) -> str:
    records = [load_public_json(line) for line in fixture.splitlines()]
    mutation(records)
    return "\n".join(json.dumps(record, separators=(",", ":")) for record in records)


def selftest() -> None:
    sensitive_marker = "/private/probe-token=never-print-this"
    with tempfile.TemporaryDirectory(prefix="cabal-copilot-probe-self-test-") as root:
        directory = Path(root)
        png, jpeg = write_fixtures(directory)
        if (
            not png.read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
            or not jpeg.read_bytes().startswith(b"\xff\xd8\xff")
            or any(
                color in path.name for path in (png, jpeg) for color in ("blue", "red")
            )
            or any((path.stat().st_mode & 0o777) != 0o600 for path in (png, jpeg))
        ):
            raise ProbeFailure("offline fixture self-test failed")

    prompt = "opaque prompt"
    first = Path("/tmp/first input/probe attachment one.png")
    second = Path("/tmp/second input/probe-attachment-two.jpg")
    argv = invocation_argv(prompt=prompt, attachments=(first, second))
    attachment_values = [
        argv[index + 1] for index, value in enumerate(argv) if value == "--attachment"
    ]
    if (
        attachment_values != [str(first.resolve()), str(second.resolve())]
        or argv[argv.index("-p") + 1] != prompt
        or "--deny-tool=url" not in argv
        or any(
            unsafe in argv
            for unsafe in (
                "--yolo",
                "--allow-all",
                "--allow-all-paths",
                "--allow-all-urls",
            )
        )
    ):
        raise ProbeFailure("offline argv self-test failed")
    web_argv = invocation_argv(prompt=prompt, allowed_url=OFFICIAL_PAGE)
    if (
        "--available-tools=view,grep,glob,web_fetch" not in web_argv
        or f"--allow-url={OFFICIAL_PAGE}" not in web_argv
        or "--deny-tool=url" in web_argv
    ):
        raise ProbeFailure("offline web argv self-test failed")

    summary = protocol_summary('{"type":"result","data":{"content":"private value"}}\n')
    if summary != (
        "type=result;shape=object{data:object{content:string},type:string}",
    ):
        raise ProbeFailure("offline protocol-summary self-test failed")

    mode_fixtures = {
        "protocol": ("ready", ()),
        "media-png": ('{"dominant_color":"blue"}', ("view",)),
        "media-jpeg": ('{"dominant_color":"red"}', ("view",)),
        "media": (
            '{"first_image_dominant_color":"blue","second_image_dominant_color":"red"}',
            ("view", "view"),
        ),
        "media-swapped-control": (
            '{"first_image_dominant_color":"red","second_image_dominant_color":"blue"}',
            ("view", "view"),
        ),
        "media-omitted-control": ("no-attachments", ()),
        "web-disabled": ("blocked", ()),
        "web-exact-url": (
            '{"reservation_triplet":"right-title-interest"}',
            ("web_fetch",),
        ),
    }
    for mode, (text, tools) in mode_fixtures.items():
        output = parse_public_output(public_jsonl_fixture(text, tools))
        validate_public_output(mode, output)
        wrong = PublicOutput(
            session_id=output.session_id,
            text="incorrect",
            tools=output.tools,
            output_tokens=output.output_tokens,
        )
        expect_probe_failure(
            lambda mode=mode, wrong=wrong: validate_public_output(mode, wrong),
            f"{mode} validator",
        )

    tool_fixture = public_jsonl_fixture("ready", ("view",))
    expect_probe_failure(
        lambda: parse_public_output("not-json"), "malformed JSONL parser"
    )
    expect_probe_failure(
        lambda: parse_public_output(
            public_jsonl_fixture("ready", (), mcp_servers=[{"name": "hostile"}])
        ),
        "nonempty MCP rejection",
    )
    expect_probe_failure(
        lambda: parse_public_output(public_jsonl_fixture("ready", ("shell",))),
        "forbidden tool rejection",
    )

    def fail_tool(records: list[dict[str, Any]]) -> None:
        next(
            record
            for record in records
            if record.get("type") == "tool.execution_complete"
        )["data"]["success"] = False

    def cross_turn(records: list[dict[str, Any]]) -> None:
        next(
            record for record in records if record.get("type") == "tool.execution_start"
        )["data"]["turnId"] = "turn-2"

    def outstanding_tool(records: list[dict[str, Any]]) -> None:
        records[:] = [
            record
            for record in records
            if record.get("type") != "tool.execution_complete"
        ]

    def workspace_change(records: list[dict[str, Any]]) -> None:
        result = next(record for record in records if record.get("type") == "result")
        result["usage"]["codeChanges"] = {
            "filesModified": [sensitive_marker],
            "linesAdded": 1,
            "linesRemoved": 0,
        }

    def extra_public_field(records: list[dict[str, Any]]) -> None:
        assistant = next(
            record for record in records if record.get("type") == "assistant.message"
        )
        assistant["data"]["privatePath"] = sensitive_marker

    def tool_request(records: list[dict[str, Any]]) -> dict[str, Any]:
        assistant = next(
            record
            for record in records
            if record.get("type") == "assistant.message"
            and record["data"]["toolRequests"]
        )
        return assistant["data"]["toolRequests"][0]

    def missing_arguments(records: list[dict[str, Any]]) -> None:
        del tool_request(records)["arguments"]

    def bad_arguments(records: list[dict[str, Any]]) -> None:
        tool_request(records)["arguments"] = sensitive_marker

    def result_status(records: list[dict[str, Any]]) -> None:
        result = next(record for record in records if record.get("type") == "result")
        result["status"] = "success"

    def result_error(records: list[dict[str, Any]]) -> None:
        result = next(record for record in records if record.get("type") == "result")
        result["error"] = sensitive_marker

    def floating_output_tokens(records: list[dict[str, Any]]) -> None:
        assistant = next(
            record for record in records if record.get("type") == "assistant.message"
        )
        assistant["data"]["outputTokens"] = 1.0

    def floating_session_duration(records: list[dict[str, Any]]) -> None:
        result = next(record for record in records if record.get("type") == "result")
        result["usage"]["sessionDurationMs"] = 1000.0

    def floating_exit_code(records: list[dict[str, Any]]) -> None:
        result = next(record for record in records if record.get("type") == "result")
        result["exitCode"] = 0.0

    def oversized_session_duration(records: list[dict[str, Any]]) -> None:
        result = next(record for record in records if record.get("type") == "result")
        result["usage"]["sessionDurationMs"] = 1 << 62

    def oversized_premium_requests(records: list[dict[str, Any]]) -> None:
        result = next(record for record in records if record.get("type") == "result")
        result["usage"]["premiumRequests"] = 1 << 62

    def cumulative_output_token_overflow(records: list[dict[str, Any]]) -> None:
        assistants = [
            record for record in records if record.get("type") == "assistant.message"
        ]
        assistants[0]["data"]["outputTokens"] = (1 << 62) - 1
        assistants[-1]["data"]["outputTokens"] = 1

    for label, mutation in (
        ("failed tool", fail_tool),
        ("cross-turn tool", cross_turn),
        ("outstanding tool", outstanding_tool),
        ("workspace change", workspace_change),
        ("extra public field", extra_public_field),
        ("missing tool arguments", missing_arguments),
        ("non-object tool arguments", bad_arguments),
        ("terminal status field", result_status),
        ("terminal error field", result_error),
        ("floating output tokens", floating_output_tokens),
        ("floating session duration", floating_session_duration),
        ("floating exit code", floating_exit_code),
        ("oversized session duration", oversized_session_duration),
        ("oversized premium requests", oversized_premium_requests),
        ("cumulative output token overflow", cumulative_output_token_overflow),
    ):
        expect_probe_failure(
            lambda mutation=mutation: parse_public_output(
                mutate_public_fixture(tool_fixture, mutation)
            ),
            label,
        )

    duplicate_field = tool_fixture.replace(
        '"type":"user.message"',
        '"type":"user.message","type":"user.message"',
        1,
    )
    expect_probe_failure(
        lambda: parse_public_output(duplicate_field), "duplicate JSON object field"
    )
    nested_duplicate_field = tool_fixture.replace(
        '"arguments":{"path":"/private/never-print"}',
        '"arguments":{"path":"/private/never-print",'
        '"path":"/private/also-never-print"}',
        1,
    )
    expect_probe_failure(
        lambda: parse_public_output(nested_duplicate_field),
        "nested duplicate JSON object field",
    )

    validate_ignored_record("session.skills_loaded", {"skills": []})
    validate_ignored_record("session.info", {"infoType": "notice", "message": "public"})
    validate_ignored_record("session.tools_updated", {"model": "public-model"})
    validate_ignored_record(
        "assistant.reasoning", {"content": "public", "reasoningId": "reason-1"}
    )
    expect_probe_failure(
        lambda: validate_ignored_record(
            "session.tools_updated",
            {"model": "public-model", "tools": ["shell"]},
        ),
        "effective tool update",
    )

    def expect_process_failure(action: Any, expected: str) -> None:
        try:
            action()
        except ProbeFailure as error:
            if str(error) != expected or sensitive_marker in str(error):
                raise ProbeFailure("offline process-error self-test failed") from error
        else:
            raise ProbeFailure("offline process-error self-test failed")

    with patch.object(
        subprocess,
        "run",
        side_effect=subprocess.TimeoutExpired(["copilot", sensitive_marker], 1),
    ):
        expect_process_failure(require_version, "Copilot version check timed out")
    with patch.object(subprocess, "run", side_effect=OSError(sensitive_marker)):
        expect_process_failure(
            require_version, "Copilot version process could not start"
        )
    with tempfile.TemporaryDirectory() as workspace:
        for exception, expected in (
            (
                subprocess.TimeoutExpired(["copilot", sensitive_marker], 1),
                "Copilot probe timed out",
            ),
            (OSError(sensitive_marker), "Copilot probe process could not start"),
            (
                UnicodeDecodeError("utf-8", b"x", 0, 1, sensitive_marker),
                "Copilot probe output could not be decoded",
            ),
        ):
            with patch.object(subprocess, "run", side_effect=exception):
                expect_process_failure(
                    lambda: run_invocation(Path(workspace), ["copilot"], False),
                    expected,
                )
        failed = subprocess.CompletedProcess(
            ["copilot", sensitive_marker], 1, "", sensitive_marker
        )
        with patch.object(subprocess, "run", return_value=failed):
            try:
                run_invocation(Path(workspace), ["copilot"], True)
            except ProbeFailure as error:
                if sensitive_marker in str(error):
                    raise ProbeFailure("offline diagnostic redaction self-test failed")
            else:
                raise ProbeFailure("offline diagnostic redaction self-test failed")

    def expect_cli_failure(
        cli_argv: list[str], expected: str, side_effect: Any = None
    ) -> None:
        stdout = io.StringIO()
        stderr = io.StringIO()
        process_patch = (
            patch.object(subprocess, "run", side_effect=side_effect)
            if side_effect is not None
            else contextlib.nullcontext()
        )
        with process_patch, contextlib.redirect_stdout(
            stdout
        ), contextlib.redirect_stderr(stderr):
            status = main(cli_argv)
        public = stdout.getvalue() + stderr.getvalue()
        if (
            status != 1
            or stdout.getvalue() != ""
            or stderr.getvalue() != f"FAIL: {expected}\n"
            or sensitive_marker in public
            or "Traceback" in public
            or "usage:" in public
        ):
            raise ProbeFailure("offline CLI-sanitization self-test failed")

    expect_cli_failure([sensitive_marker], "invalid probe arguments")
    expect_cli_failure(
        ["protocol"],
        "Copilot version check timed out",
        subprocess.TimeoutExpired(["copilot", sensitive_marker], 1),
    )
    expect_cli_failure(
        ["protocol"], "Copilot probe interrupted", KeyboardInterrupt(sensitive_marker)
    )
    expect_cli_failure(
        ["protocol"],
        "Copilot emitted malformed public JSONL",
        [
            subprocess.CompletedProcess(
                ["copilot", "--version"],
                0,
                f"GitHub Copilot CLI {EXPECTED_VERSION}.\n",
                "",
            ),
            subprocess.CompletedProcess(
                ["copilot", sensitive_marker], 0, sensitive_marker, ""
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
    if (
        child.returncode != 1
        or child.stdout != ""
        or child.stderr != "FAIL: invalid probe arguments\n"
        or sensitive_marker in child.stdout + child.stderr
        or "Traceback" in child.stdout + child.stderr
        or "usage:" in child.stdout + child.stderr
    ):
        raise ProbeFailure("offline subprocess CLI-sanitization self-test failed")


def parse_args(argv: list[str]) -> argparse.Namespace:
    if argv == ["selftest"]:
        argv = ["--self-test"]
    parser = SafeArgumentParser(add_help=True)
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("--debug-protocol", action="store_true")
    parser.add_argument("modes", nargs="*", choices=MODES, default=())
    args = parser.parse_args(argv)
    if args.self_test and args.modes:
        raise ProbeFailure("invalid probe arguments")
    return args


def main(argv: list[str]) -> int:
    try:
        args = parse_args(argv)
        if args.self_test:
            selftest()
            print("PASS self-test")
        else:
            for mode in tuple(args.modes) or MODES:
                run_mode(mode, args.debug_protocol)
                print(f"PASS {mode}")
        return 0
    except ProbeFailure as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        print("FAIL: Copilot probe interrupted", file=sys.stderr)
        return 1
    except (AssertionError, OSError, ValueError, UnicodeError):
        print("FAIL: probe self-test failed", file=sys.stderr)
        return 1
    except Exception:
        print("FAIL: probe parser failure", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
