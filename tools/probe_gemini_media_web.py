#!/usr/bin/env python3
"""Bounded Gemini CLI 0.38.2 media, resume, and web-policy probe."""

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
import zlib
from pathlib import Path
from typing import Any
from unittest.mock import patch
from urllib.parse import urlsplit

EXPECTED_VERSION = "0.38.2"
BASELINE_COMMAND = ("npx", "--yes", "@google/gemini-cli@0.38.2")
PROBE_TIMEOUT_SECONDS = 180
VERSION_TIMEOUT_SECONDS = 30
OFFICIAL_PAGE = "https://geminicli.com/docs/cli/headless/"
OFFICIAL_PAGE_H1 = "Headless mode reference"
SESSION_ID = re.compile(
    r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
)
MODES = (
    "media-initial",
    "resume-upload",
    "resume-reuse",
    "web-disabled",
    "web-search",
    "web-live",
)
INTERNAL_CONTROL_MODES = (
    "media-swapped-control",
    "media-omitted-control",
    "web-disabled-control",
)

# Deterministic 64x64 solid-red JPEG. PNG fixtures are generated below with
# the standard library, keeping the probe independent of image packages.
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
    """A fixed diagnostic that is safe to print."""


class SafeArgumentParser(argparse.ArgumentParser):
    """Argument parser that never reproduces an invalid supplied value."""

    def error(self, message: str) -> None:
        del message
        raise ProbeFailure("invalid probe arguments")


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
    blue = directory / "asset 01,$(noop).png"
    red = directory / "asset-02.jpg"
    green = directory / "asset-03.png"
    blue.write_bytes(solid_png(20, 45, 225))
    red.write_bytes(base64.b64decode(RED_JPEG, validate=True))
    green.write_bytes(solid_png(20, 190, 55))
    for path in (blue, red, green):
        path.chmod(0o600)
    return blue, red, green


def escape_at_path(path: Path) -> str:
    """Render one literal @ path using Gemini's baseline backslash grammar."""
    value = str(path.resolve())
    if any(character in value for character in ("\x00", "\n", "\r")):
        raise ProbeFailure("fixture path is not representable")
    rendered = ["@"]
    for character in value:
        if not (character.isalnum() or character in "/_-"):
            rendered.append("\\")
        rendered.append(character)
    return "".join(rendered)


def policy_text(mode: str) -> str:
    if mode in {
        "media-initial",
        "media-swapped-control",
        "media-omitted-control",
        "resume-upload",
        "resume-reuse",
        "web-disabled",
    }:
        return (
            '[[rule]]\ntoolName = ["google_web_search", "web_fetch"]\n'
            'decision = "deny"\npriority = 999\n'
        )
    if mode == "web-search":
        return (
            '[[rule]]\ntoolName = "google_web_search"\n'
            'decision = "allow"\npriority = 999\n\n'
            '[[rule]]\ntoolName = "web_fetch"\n'
            'decision = "deny"\npriority = 999\n'
        )
    if mode in {"web-live", "web-disabled-control"}:
        return (
            '[[rule]]\ntoolName = ["google_web_search", "web_fetch"]\n'
            'decision = "allow"\npriority = 999\n'
        )
    raise ProbeFailure("unknown probe mode")


def write_policy(directory: Path, mode: str) -> Path:
    path = directory / f"{mode}.policy.toml"
    path.write_text(policy_text(mode), encoding="utf-8")
    path.chmod(0o600)
    return path


def standard_admin_policy_conflict() -> bool:
    if os.name == "nt":
        directories = (Path(r"C:\ProgramData\gemini-cli\policies"),)
    else:
        directories = (
            Path("/etc/gemini-cli/policies"),
            Path("/Library/Application Support/GeminiCli/policies"),
        )
    for directory in directories:
        try:
            if any(path.name.endswith(".toml") for path in directory.iterdir()):
                return True
        except FileNotFoundError:
            continue
        except OSError:
            return True
    return False


def command_prefix(installed: bool) -> list[str]:
    return ["gemini"] if installed else list(BASELINE_COMMAND)


def require_version(command: list[str]) -> None:
    try:
        completed = subprocess.run(
            command + ["--version"],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=VERSION_TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired as error:
        raise ProbeFailure("Gemini version check timed out") from error
    except OSError as error:
        raise ProbeFailure("Gemini version process could not start") from error
    except UnicodeError as error:
        raise ProbeFailure("Gemini version output could not be decoded") from error
    if completed.returncode != 0 or completed.stdout.strip() != EXPECTED_VERSION:
        raise ProbeFailure(f"probe requires exactly Gemini CLI {EXPECTED_VERSION}")


def invocation_argv(
    command: list[str],
    policy: Path,
    include_directory: Path | None = None,
    session_id: str | None = None,
) -> list[str]:
    argv = command + [
        "--output-format",
        "stream-json",
        "-y",
        "--policy",
        str(policy.resolve()),
        "--admin-policy",
        str(policy.resolve()),
    ]
    if include_directory is not None:
        argv.extend(("--include-directories", str(include_directory.resolve())))
    if session_id is not None:
        if not SESSION_ID.fullmatch(session_id):
            raise ProbeFailure("probe session identity is invalid")
        argv.extend(("--resume", session_id))
    argv.extend(("-p", ""))
    return argv


def parse_answer(text: str) -> dict[str, Any]:
    stripped = text.strip()
    if stripped.startswith("```json") and stripped.endswith("```"):
        stripped = stripped[7:-3].strip()
    elif stripped.startswith("```") and stripped.endswith("```"):
        stripped = stripped[3:-3].strip()
    try:
        answer = json.loads(stripped)
    except (json.JSONDecodeError, UnicodeError) as error:
        raise ProbeFailure("Gemini emitted no structured public answer") from error
    if not isinstance(answer, dict):
        raise ProbeFailure("Gemini emitted no structured public answer")
    return answer


def parse_public_output(stdout: str) -> tuple[str, dict[str, Any], set[str], set[str]]:
    session_id: str | None = None
    assistant_chunks: list[str] = []
    started_tools: set[str] = set()
    finished_tools: set[str] = set()
    tools_by_id: dict[str, str] = {}
    successful_result = False
    for line in stdout.splitlines():
        if not line.strip():
            continue
        try:
            event = json.loads(line)
        except (json.JSONDecodeError, UnicodeError) as error:
            raise ProbeFailure("Gemini emitted malformed public stream JSON") from error
        if not isinstance(event, dict):
            raise ProbeFailure("Gemini emitted malformed public stream JSON")
        event_type = event.get("type")
        if event_type == "init":
            candidate = event.get("session_id")
            if isinstance(candidate, str) and SESSION_ID.fullmatch(candidate):
                session_id = candidate
        elif event_type == "message" and event.get("role") == "assistant":
            content = event.get("content")
            if isinstance(content, str):
                assistant_chunks.append(content)
        elif event_type == "tool_use":
            tool_name = event.get("tool_name")
            if isinstance(tool_name, str):
                started_tools.add(tool_name)
                tool_id = event.get("tool_id")
                if isinstance(tool_id, str):
                    tools_by_id[tool_id] = tool_name
        elif event_type == "tool_result":
            tool_name = event.get("tool_name")
            if not isinstance(tool_name, str):
                tool_id = event.get("tool_id")
                if isinstance(tool_id, str):
                    tool_name = tools_by_id.get(tool_id)
            if isinstance(tool_name, str):
                finished_tools.add(tool_name)
        elif event_type == "result" and event.get("status") == "success":
            successful_result = True
    if session_id is None:
        raise ProbeFailure("Gemini emitted no canonical public session UUID")
    if not successful_result:
        raise ProbeFailure("Gemini emitted no successful public result")
    answer = parse_answer("".join(assistant_chunks))
    return session_id, answer, started_tools, finished_tools


def official_result(value: Any) -> bool:
    if not isinstance(value, str):
        return False
    parsed = urlsplit(value)
    return (
        parsed.scheme == "https"
        and parsed.netloc == "geminicli.com"
        and parsed.path.rstrip("/") == "/docs/cli/headless"
    )


def prompt_without_answer_leak(text: str) -> str:
    lowered = text.lower()
    for value in ("blue", "red", "green"):
        if re.search(rf"\b{value}\b", lowered):
            raise ProbeFailure("probe prompt leaks expected media content")
    if OFFICIAL_PAGE_H1.lower() in lowered:
        raise ProbeFailure("probe prompt leaks expected web content")
    return text


def media_pair_prompt(paths: tuple[Path, ...]) -> str:
    references = "\n".join(escape_at_path(path) for path in paths)
    return prompt_without_answer_leak(
        f"{references}\n\nInspect the referenced images in their displayed order. Return "
        "only a JSON object with exactly the string fields "
        '"first_image_dominant_color" and "second_image_dominant_color". '
        "Use one lowercase English color word for each visible dominant color."
    )


def resume_upload_prompt(path: Path) -> str:
    return prompt_without_answer_leak(
        f"{escape_at_path(path)}\n\nInspect the newly referenced image. Return only a "
        'JSON object with exactly the string field "new_image_dominant_color", '
        "using one lowercase English color word for the visible dominant color."
    )


def resume_reuse_prompt() -> str:
    return prompt_without_answer_leak(
        "Without opening files or using tools, recall the dominant color of the most "
        "recently inspected image. Return only a JSON object with exactly the string "
        'field "remembered_image_dominant_color", using one lowercase English color '
        "word."
    )


def web_live_prompt() -> str:
    return prompt_without_answer_leak(
        "Use web search, then web fetch to read "
        f"{OFFICIAL_PAGE}. Return only a JSON object with exactly the string field "
        '"page_h1" containing the visible primary H1. Do not answer from memory.'
    )


def validate_result(
    mode: str,
    answer: dict[str, Any],
    started_tools: set[str],
    finished_tools: set[str],
) -> None:
    expected_answers: dict[str, dict[str, Any]] = {
        "media-initial": {
            "first_image_dominant_color": "blue",
            "second_image_dominant_color": "red",
        },
        "media-swapped-control": {
            "first_image_dominant_color": "red",
            "second_image_dominant_color": "blue",
        },
        "resume-upload": {"new_image_dominant_color": "green"},
        "resume-reuse": {"remembered_image_dominant_color": "green"},
        "web-disabled-control": {"page_h1": OFFICIAL_PAGE_H1},
        "web-live": {"page_h1": OFFICIAL_PAGE_H1},
    }
    web_tools = {"google_web_search", "web_fetch"}
    leaked_media_answer = expected_answers["media-initial"]
    if mode == "media-omitted-control":
        if answer == leaked_media_answer:
            raise ProbeFailure("Gemini reproduced the omitted-image answer")
        return
    if mode in expected_answers and answer != expected_answers[mode]:
        raise ProbeFailure("Gemini public answer failed the content assertion")
    if mode == "web-disabled" and (started_tools | finished_tools) & web_tools:
        raise ProbeFailure("Gemini Web_disabled policy permitted a web tool")
    if mode == "web-search":
        if set(answer) != {"official_result_url"} or not official_result(
            answer.get("official_result_url")
        ):
            raise ProbeFailure("Gemini public answer failed the content assertion")
        if (
            "google_web_search" not in started_tools
            or "google_web_search" not in finished_tools
            or "web_fetch" in started_tools | finished_tools
        ):
            raise ProbeFailure("Gemini emitted no search-only public lifecycle")
    if mode in {"web-live", "web-disabled-control"} and not web_tools.issubset(
        started_tools & finished_tools
    ):
        raise ProbeFailure("Gemini emitted no complete public search/fetch lifecycle")


def run_probe(
    workspace: Path,
    argv: list[str],
    prompt: str,
    mode: str,
    debug_public: bool,
) -> str:
    try:
        completed = subprocess.run(
            argv,
            cwd=workspace,
            input=prompt,
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=PROBE_TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired as error:
        raise ProbeFailure("Gemini probe timed out") from error
    except OSError as error:
        raise ProbeFailure("Gemini probe process could not start") from error
    except UnicodeError as error:
        raise ProbeFailure("Gemini probe output could not be decoded") from error
    if completed.returncode != 0:
        lowered = (completed.stdout + "\n" + completed.stderr).lower()
        if any(
            marker in lowered
            for marker in ("auth", "credential", "ineligibletier", "quota")
        ):
            raise ProbeFailure("Gemini authentication is unavailable")
        raise ProbeFailure("Gemini probe process failed")
    session_id, answer, started_tools, finished_tools = parse_public_output(
        completed.stdout
    )
    validate_result(mode, answer, started_tools, finished_tools)
    if debug_public:
        print(
            "DEBUG public-lifecycle search=%s fetch=%s"
            % (
                "yes" if "google_web_search" in started_tools else "no",
                "yes" if "web_fetch" in started_tools else "no",
            ),
            file=sys.stderr,
        )
    return session_id


def run_modes(
    command: list[str],
    workspace: Path,
    private_inputs: Path,
    selected_modes: tuple[str, ...],
    debug_public: bool,
) -> None:
    blue, red, green = write_fixtures(private_inputs)
    session_id: str | None = None
    green_uploaded = False
    media_controls_ran = False

    def ensure_session() -> str:
        nonlocal session_id, media_controls_ran
        if session_id is None:
            mode = "media-initial"
            policy = write_policy(private_inputs, mode)
            session_id = run_probe(
                workspace,
                invocation_argv(command, policy, private_inputs),
                media_pair_prompt((blue, red)),
                mode,
                debug_public,
            )
        if not media_controls_ran:
            swapped_mode = "media-swapped-control"
            swapped_policy = write_policy(private_inputs, swapped_mode)
            run_probe(
                workspace,
                invocation_argv(command, swapped_policy, private_inputs),
                media_pair_prompt((red, blue)),
                swapped_mode,
                debug_public,
            )
            omitted_mode = "media-omitted-control"
            omitted_policy = write_policy(private_inputs, omitted_mode)
            run_probe(
                workspace,
                invocation_argv(command, omitted_policy, private_inputs),
                media_pair_prompt((blue,)),
                omitted_mode,
                debug_public,
            )
            media_controls_ran = True
        return session_id

    def ensure_green_upload() -> str:
        nonlocal session_id, green_uploaded
        if not green_uploaded:
            current = ensure_session()
            mode = "resume-upload"
            policy = write_policy(private_inputs, mode)
            session_id = run_probe(
                workspace,
                invocation_argv(command, policy, private_inputs, current),
                resume_upload_prompt(green),
                mode,
                debug_public,
            )
            green_uploaded = True
        return session_id

    for mode in selected_modes:
        if mode == "media-initial":
            ensure_session()
        elif mode == "resume-upload":
            ensure_green_upload()
        elif mode == "resume-reuse":
            current = ensure_green_upload()
            policy = write_policy(private_inputs, mode)
            session_id = run_probe(
                workspace,
                invocation_argv(command, policy, session_id=current),
                resume_reuse_prompt(),
                mode,
                debug_public,
            )
        elif mode == "web-disabled":
            control_mode = "web-disabled-control"
            control_policy = write_policy(private_inputs, control_mode)
            run_probe(
                workspace,
                invocation_argv(command, control_policy),
                web_live_prompt(),
                control_mode,
                debug_public,
            )
            policy = write_policy(private_inputs, mode)
            run_probe(
                workspace,
                invocation_argv(command, policy),
                "Use web search and web fetch to read the official Gemini CLI headless "
                "documentation. If tools are unavailable, return only {}.",
                mode,
                debug_public,
            )
        elif mode == "web-search":
            policy = write_policy(private_inputs, mode)
            run_probe(
                workspace,
                invocation_argv(command, policy),
                "Use google_web_search only to locate the official Gemini CLI headless "
                "documentation. Do not open or fetch it. Return only JSON with the one "
                'field "official_result_url" containing its bare HTTPS URL.',
                mode,
                debug_public,
            )
        elif mode == "web-live":
            policy = write_policy(private_inputs, mode)
            run_probe(
                workspace,
                invocation_argv(command, policy),
                web_live_prompt(),
                mode,
                debug_public,
            )
        else:
            raise ProbeFailure("unknown probe mode")
        print(f"PASS {mode}")


def self_test() -> None:
    with tempfile.TemporaryDirectory(prefix="cabal-gemini-probe-self-test-") as raw:
        directory = Path(raw)
        blue, red, green = write_fixtures(directory)
        if any(
            color in path.name.lower()
            for path in (blue, red, green)
            for color in ("blue", "red", "green")
        ):
            raise ProbeFailure("self-test fixture name leaks expected content")
        if not blue.read_bytes().startswith(b"\x89PNG\r\n\x1a\n"):
            raise ProbeFailure("self-test PNG fixture failed")
        if not red.read_bytes().startswith(b"\xff\xd8"):
            raise ProbeFailure("self-test JPEG fixture failed")
        if not green.read_bytes().startswith(b"\x89PNG\r\n\x1a\n"):
            raise ProbeFailure("self-test PNG fixture failed")
        if any((path.stat().st_mode & 0o777) != 0o600 for path in (blue, red, green)):
            raise ProbeFailure("self-test fixture permissions failed")
        rendered = escape_at_path(blue)
        if (
            not rendered.startswith("@/")
            or "\\ " not in rendered
            or "\\$" not in rendered
        ):
            raise ProbeFailure("self-test path escaping failed")
        prompts = (
            media_pair_prompt((blue, red)),
            media_pair_prompt((red, blue)),
            media_pair_prompt((blue,)),
            resume_upload_prompt(green),
            resume_reuse_prompt(),
            web_live_prompt(),
        )
        if any(
            re.search(r"\b(?:blue|red|green)\b", prompt.lower())
            or OFFICIAL_PAGE_H1.lower() in prompt.lower()
            for prompt in prompts
        ):
            raise ProbeFailure("self-test prompt leaks expected answer")
        try:
            prompt_without_answer_leak("The expected answer is blue")
        except ProbeFailure:
            pass
        else:
            raise ProbeFailure("self-test answer-leak guard failed")
        for mode in MODES + INTERNAL_CONTROL_MODES:
            policy = write_policy(directory, mode)
            if (
                policy.stat().st_mode & 0o777
            ) != 0o600 or "priority = 999" not in policy.read_text():
                raise ProbeFailure("self-test policy failed")

    fixture = "\n".join(
        (
            '{"type":"init","session_id":"123e4567-e89b-12d3-a456-426614174000"}',
            '{"type":"message","role":"user","content":"private"}',
            '{"type":"tool_use","tool_name":"google_web_search","tool_id":"tool-1","parameters":{"query":"private"}}',
            '{"type":"tool_result","tool_name":"google_web_search","tool_id":"tool-1","status":"success","output":"private"}',
            '{"type":"message","role":"assistant","content":"{\\"official_result_url\\":\\"https://geminicli.com/docs/cli/headless/\\"}"}',
            '{"type":"result","status":"success","stats":{"input_tokens":1}}',
        )
    )
    session_id, answer, started, finished = parse_public_output(fixture)
    if not SESSION_ID.fullmatch(session_id) or started != finished:
        raise ProbeFailure("self-test public parser failed")
    validate_result("web-search", answer, started, finished)
    normal_media_answer = {
        "first_image_dominant_color": "blue",
        "second_image_dominant_color": "red",
    }
    try:
        validate_result(
            "media-initial",
            {"first_image_dominant_color": "wrong"},
            set(),
            set(),
        )
    except ProbeFailure:
        pass
    else:
        raise ProbeFailure("self-test content validator failed")
    try:
        validate_result("media-swapped-control", normal_media_answer, set(), set())
    except ProbeFailure:
        pass
    else:
        raise ProbeFailure("self-test swapped-image leakage was accepted")
    try:
        validate_result("media-omitted-control", normal_media_answer, set(), set())
    except ProbeFailure:
        pass
    else:
        raise ProbeFailure("self-test omitted-image leakage was accepted")
    validate_result(
        "media-omitted-control",
        {
            "first_image_dominant_color": "blue",
            "second_image_dominant_color": "unknown",
        },
        set(),
        set(),
    )
    try:
        validate_result(
            "web-disabled-control",
            {"page_h1": OFFICIAL_PAGE_H1},
            set(),
            set(),
        )
    except ProbeFailure:
        pass
    else:
        raise ProbeFailure("self-test web positive control accepted no tools")
    validate_result(
        "web-disabled-control",
        {"page_h1": OFFICIAL_PAGE_H1},
        {"google_web_search", "web_fetch"},
        {"google_web_search", "web_fetch"},
    )
    try:
        parse_public_output("not-json")
    except ProbeFailure:
        pass
    else:
        raise ProbeFailure("self-test malformed output validator failed")
    with patch("subprocess.run", side_effect=subprocess.TimeoutExpired("gemini", 1)):
        try:
            require_version(["gemini"])
        except ProbeFailure as error:
            if str(error) != "Gemini version check timed out":
                raise ProbeFailure("self-test timeout diagnostic failed") from error
        else:
            raise ProbeFailure("self-test timeout validator failed")
    try:
        invocation_argv(
            ["gemini"], Path("/tmp/policy"), session_id="private invalid session"
        )
    except ProbeFailure as error:
        if str(error) != "probe session identity is invalid":
            raise ProbeFailure("self-test session diagnostic failed") from error
    else:
        raise ProbeFailure("self-test session validator failed")

    secret = "PRIVATE_INVALID_ARGUMENT_VALUE"
    stderr = io.StringIO()
    with contextlib.redirect_stderr(stderr):
        try:
            parse_args(("--mode", secret))
        except ProbeFailure as error:
            if str(error) != "invalid probe arguments":
                raise ProbeFailure("self-test argument diagnostic failed") from error
        else:
            raise ProbeFailure("self-test argument validator failed")
    if secret in stderr.getvalue() or "usage:" in stderr.getvalue().lower():
        raise ProbeFailure("self-test argument privacy failed")


def parse_args(argv: tuple[str, ...] | None = None) -> argparse.Namespace:
    parser = SafeArgumentParser(
        description="Probe Gemini CLI baseline media/web behavior"
    )
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("--installed", action="store_true")
    parser.add_argument("--debug-public", action="store_true")
    parser.add_argument("--mode", action="append", choices=MODES)
    return parser.parse_args(argv)


def main(argv: tuple[str, ...] | None = None) -> int:
    try:
        args = parse_args(argv)
        if args.self_test:
            self_test()
            print("PASS self-test")
            return 0
        command = command_prefix(args.installed)
        require_version(command)
        if standard_admin_policy_conflict():
            raise ProbeFailure("standard Gemini administrator policy blocks the probe")
        selected_modes = tuple(args.mode or MODES)
        with tempfile.TemporaryDirectory(prefix="cabal-gemini-probe-") as raw:
            root = Path(raw)
            root.chmod(0o700)
            workspace = root / "workspace"
            private_inputs = root / "private-inputs"
            workspace.mkdir(mode=0o700)
            private_inputs.mkdir(mode=0o700)
            run_modes(
                command, workspace, private_inputs, selected_modes, args.debug_public
            )
        return 0
    except KeyboardInterrupt:
        print("ERROR: Gemini probe interrupted", file=sys.stderr)
        return 1
    except ProbeFailure as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1
    except Exception:
        print("ERROR: Gemini probe failed unexpectedly", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
