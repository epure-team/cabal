#!/usr/bin/env python3
"""Authenticated Codex 0.131.0 content-dependent media/resume/web probe."""

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


EXPECTED_VERSION = "codex-cli 0.131.0"
PROBE_TIMEOUT_SECONDS = 180
VERSION_TIMEOUT_SECONDS = 15
OFFICIAL_PAGE = "https://developers.openai.com/codex/cli/"
OFFICIAL_PAGE_H1 = "Inspect, edit, and run code from your terminal"
THREAD_ID = re.compile(
    r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
)
MODES = (
    "media-initial",
    "resume-upload",
    "resume-reuse",
    "web-cached",
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

# A deterministic 64x64 solid-red JPEG. The PNG fixtures are generated below
# with the Python standard library so the probe has no image-library dependency.
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
    """A public, fixed diagnostic safe to print."""


class SafeArgumentParser(argparse.ArgumentParser):
    """Argument parser whose errors never reproduce supplied values or paths."""

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
    if mode == "web-cached":
        return strict_object_schema(
            {"official_result_url": {"type": "string", "minLength": 1, "maxLength": 300}}
        )
    if mode == "web-live":
        return strict_object_schema(
            {"page_h1": {"type": "string", "minLength": 1, "maxLength": 200}}
        )
    raise ProbeFailure("unknown probe mode")


def write_schema(directory: Path, mode: str) -> Path:
    path = directory / f"{mode}.schema.json"
    path.write_text(
        json.dumps(mode_schema(mode), separators=(",", ":")), encoding="utf-8"
    )
    path.chmod(0o600)
    return path


def write_fixtures(directory: Path) -> tuple[Path, Path, Path]:
    blue = directory / "fixture-a.png"
    red = directory / "fixture-b.jpg"
    green = directory / "fixture-c.png"
    blue.write_bytes(solid_png(20, 45, 225))
    red.write_bytes(base64.b64decode(RED_JPEG, validate=True))
    green.write_bytes(solid_png(20, 190, 55))
    for path in (blue, red, green):
        path.chmod(0o600)
    return blue, red, green


def require_version() -> None:
    try:
        completed = subprocess.run(
            ["codex", "--version"],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=VERSION_TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired as error:
        raise ProbeFailure("Codex version check timed out") from error
    except OSError as error:
        raise ProbeFailure("Codex version process could not start") from error
    except UnicodeError as error:
        raise ProbeFailure("Codex version output could not be decoded") from error
    if completed.returncode != 0 or completed.stdout.strip() != EXPECTED_VERSION:
        raise ProbeFailure(f"probe requires exactly {EXPECTED_VERSION}")


def initial_argv(schema: Path, web_mode: str, images: tuple[Path, ...]) -> list[str]:
    argv = [
        "codex",
        "exec",
        "--json",
        "--skip-git-repo-check",
        "--ignore-user-config",
        "-c",
        f'web_search="{web_mode}"',
        "-s",
        "read-only",
        "--output-schema",
        str(schema),
    ]
    for image in images:
        argv.extend(("-i", str(image.resolve())))
    argv.append("-")
    return argv


def resume_argv(schema: Path, session_id: str, images: tuple[Path, ...]) -> list[str]:
    argv = [
        "codex",
        "exec",
        "--output-schema",
        str(schema),
        "-s",
        "read-only",
        "resume",
        session_id,
        "--json",
        "--skip-git-repo-check",
        "--ignore-user-config",
        "-c",
        'web_search="disabled"',
    ]
    for image in images:
        argv.extend(("-i", str(image.resolve())))
    argv.append("-")
    return argv


def web_action(item: dict[str, Any]) -> str | None:
    action = item.get("action")
    action_type: str | None = None
    if isinstance(action, dict) and isinstance(action.get("type"), str):
        action_type = action["type"].lower()
    elif isinstance(action, str):
        action_type = action.lower()
    if action_type in {"search", "web_search"}:
        return "search"
    if action_type in {"fetch", "open", "open_page", "view"}:
        return "fetch"
    if isinstance(item.get("url"), str) or (
        isinstance(action, dict) and isinstance(action.get("url"), str)
    ):
        return "fetch"
    if contains_official_page(item):
        return "fetch"
    if isinstance(item.get("query"), str):
        return "search"
    return None


def contains_official_page(value: Any) -> bool:
    if isinstance(value, str):
        return value.rstrip("/") == OFFICIAL_PAGE.rstrip("/")
    if isinstance(value, list):
        return any(contains_official_page(item) for item in value)
    if isinstance(value, dict):
        return any(contains_official_page(item) for item in value.values())
    return False


def is_official_codex_cli_result(value: Any) -> bool:
    if not isinstance(value, str):
        return False
    parsed = urlsplit(value)
    path = parsed.path.rstrip("/")
    return (
        parsed.scheme == "https"
        and parsed.netloc == "developers.openai.com"
        and (path == "/codex/cli" or path.startswith("/codex/cli/"))
    )


def parse_public_output(stdout: str) -> tuple[str, dict[str, Any], set[str], bool]:
    session_id: str | None = None
    final_message: str | None = None
    web_items: dict[str, dict[str, Any]] = {}
    official_page_seen = False

    for line in stdout.splitlines():
        if not line.strip():
            continue
        try:
            event = json.loads(line)
        except (json.JSONDecodeError, UnicodeError) as error:
            raise ProbeFailure("Codex emitted malformed public JSONL") from error
        if not isinstance(event, dict):
            raise ProbeFailure("Codex emitted malformed public JSONL")

        event_type = event.get("type")
        if event_type == "thread.started":
            candidate = event.get("thread_id")
            if isinstance(candidate, str) and THREAD_ID.fullmatch(candidate):
                session_id = candidate

        item = event.get("item")
        if not isinstance(item, dict):
            continue
        item_type = item.get("type")
        if event_type == "item.completed" and item_type == "agent_message":
            text = item.get("text")
            if isinstance(text, str):
                final_message = text
        if item_type != "web_search":
            continue

        item_id = item.get("id")
        if not isinstance(item_id, str) or not item_id:
            raise ProbeFailure("Codex emitted malformed public web lifecycle")
        state = web_items.setdefault(item_id, {"events": set(), "action": None})
        if event_type in {"item.started", "item.completed"}:
            state["events"].add(event_type)
        action = web_action(item)
        if action is not None:
            state["action"] = action
        official_page_seen = official_page_seen or contains_official_page(item)

    if session_id is None:
        raise ProbeFailure("Codex emitted no canonical public thread.started UUID")
    try:
        result = json.loads(final_message or "")
    except (json.JSONDecodeError, UnicodeError) as error:
        raise ProbeFailure("Codex emitted no structured public answer") from error
    if not isinstance(result, dict):
        raise ProbeFailure("Codex emitted no structured public answer")

    completed_actions = {
        state["action"]
        for state in web_items.values()
        if state["events"] == {"item.started", "item.completed"}
        and state["action"] is not None
    }
    return session_id, result, completed_actions, official_page_seen


def validate_mode(
    mode: str,
    result: dict[str, Any],
    expected: dict[str, Any],
    web_actions: set[str],
    official_page_seen: bool,
) -> None:
    cached_url = result.get("official_result_url")
    cached_url_matches = (
        mode == "web-cached"
        and set(result) == {"official_result_url"}
        and is_official_codex_cli_result(cached_url)
    )
    if not cached_url_matches and result != expected:
        raise ProbeFailure("Codex public answer failed the content assertion")
    if mode == "web-cached":
        if "search" not in web_actions:
            raise ProbeFailure("Codex emitted no complete public cached-search lifecycle")
        if "fetch" in web_actions:
            raise ProbeFailure("Codex cached-search probe emitted a forbidden fetch")
    if mode == "web-live":
        if not {"search", "fetch"}.issubset(web_actions) or not official_page_seen:
            raise ProbeFailure("Codex emitted no complete public search/fetch lifecycle")


def run_probe(
    workspace: Path,
    argv: list[str],
    prompt: str,
    mode: str,
    expected: dict[str, Any],
    debug_public: bool = False,
) -> str:
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
        )
    except subprocess.TimeoutExpired as error:
        raise ProbeFailure("Codex probe timed out") from error
    except OSError as error:
        raise ProbeFailure("Codex probe process could not start") from error
    except UnicodeError as error:
        raise ProbeFailure("Codex probe output could not be decoded") from error
    if completed.returncode != 0:
        raise ProbeFailure("Codex probe process failed")
    session_id, result, web_actions, official_page_seen = parse_public_output(
        completed.stdout
    )
    if debug_public and mode.startswith("web-"):
        print(
            "DEBUG public-web search=%s fetch=%s official-page=%s"
            % (
                "yes" if "search" in web_actions else "no",
                "yes" if "fetch" in web_actions else "no",
                "yes" if official_page_seen else "no",
            ),
            file=sys.stderr,
        )
    validate_mode(mode, result, expected, web_actions, official_page_seen)
    return session_id


def run_modes(
    workspace: Path,
    sealed_inputs: Path,
    selected_modes: tuple[str, ...],
    debug_public: bool = False,
) -> None:
    blue, red, green = write_fixtures(sealed_inputs)
    session_id: str | None = None
    remembered_color: str | None = None

    def ensure_session() -> str:
        nonlocal session_id, remembered_color
        if session_id is None:
            mode = "media-initial"
            session_id = run_probe(
                workspace,
                initial_argv(write_schema(sealed_inputs, mode), "disabled", (blue, red)),
                "Inspect both attached images. Report the dominant visible color of the "
                "PNG and of the JPEG using the required JSON fields.",
                mode,
                {"png_dominant_color": "blue", "jpeg_dominant_color": "red"},
                debug_public,
            )
            remembered_color = "blue"
        return session_id

    for mode in selected_modes:
        if mode == "media-initial":
            ensure_session()
        elif mode == "resume-upload":
            session_id = run_probe(
                workspace,
                resume_argv(
                    write_schema(sealed_inputs, mode), ensure_session(), (green,)
                ),
                "Inspect the one newly attached image and report its dominant visible "
                "color using the required JSON field.",
                mode,
                {"new_image_dominant_color": "green"},
                debug_public,
            )
            remembered_color = "green"
        elif mode == "resume-reuse":
            current_session = ensure_session()
            expected_color = remembered_color
            if expected_color is None:
                raise ProbeFailure("probe session state is unavailable")
            argv = resume_argv(write_schema(sealed_inputs, mode), current_session, ())
            if "-i" in argv:
                raise ProbeFailure("resume reuse unexpectedly uploads an image")
            session_id = run_probe(
                workspace,
                argv,
                "Without opening files or using tools, recall the dominant color of the "
                "most recently inspected attached image and return it using the required "
                "JSON field.",
                mode,
                {"remembered_image_dominant_color": expected_color},
                debug_public,
            )
        elif mode == "web-cached":
            run_probe(
                workspace,
                initial_argv(write_schema(sealed_inputs, mode), "cached", ()),
                "Use web search only with the query "
                "'site:developers.openai.com/codex/cli OpenAI Codex CLI' to locate "
                "an official Codex CLI documentation result. Do not open, fetch, "
                "click, or read any page. Set official_result_url to the bare absolute "
                "HTTPS URL from that search result, with no title or Markdown.",
                mode,
                {"official_result_url": OFFICIAL_PAGE},
                debug_public,
            )
        elif mode == "web-live":
            run_probe(
                workspace,
                initial_argv(write_schema(sealed_inputs, mode), "live", ()),
                "Use web search to locate the official OpenAI Codex CLI page, then open "
                f"and read {OFFICIAL_PAGE}. Return the page's visible primary H1 text "
                "using the required JSON field. Do not answer from memory.",
                mode,
                {"page_h1": OFFICIAL_PAGE_H1},
                debug_public,
            )
        else:
            raise ProbeFailure("unknown probe mode")
        print(f"PASS {mode}")


def jsonl_fixture(result: dict[str, Any], web_lifecycle: str) -> str:
    events: list[dict[str, Any]] = [
        {
            "type": "thread.started",
            "thread_id": "123e4567-e89b-12d3-a456-426614174000",
        }
    ]
    if web_lifecycle in {"search", "search-fetch"}:
        events.extend(
            [
                {
                    "type": "item.started",
                    "item": {
                        "id": "search-1",
                        "type": "web_search",
                        "action": {"type": "search", "query": "OpenAI Codex CLI"},
                    },
                },
                {
                    "type": "item.completed",
                    "item": {
                        "id": "search-1",
                        "type": "web_search",
                        "action": {"type": "search", "query": "OpenAI Codex CLI"},
                    },
                },
            ]
        )
    if web_lifecycle == "search-fetch":
        events.extend(
            [
                {
                    "type": "item.started",
                    "item": {
                        "id": "fetch-1",
                        "type": "web_search",
                        "action": {"type": "open_page", "url": OFFICIAL_PAGE},
                    },
                },
                {
                    "type": "item.completed",
                    "item": {
                        "id": "fetch-1",
                        "type": "web_search",
                        "action": {"type": "open_page", "url": OFFICIAL_PAGE},
                    },
                },
            ]
        )
    events.append(
        {
            "type": "item.completed",
            "item": {
                "id": "answer-1",
                "type": "agent_message",
                "text": json.dumps(result, separators=(",", ":")),
            },
        }
    )
    return "\n".join(json.dumps(event, separators=(",", ":")) for event in events)


def run_self_test() -> None:
    expectations = {
        "media-initial": {"png_dominant_color": "blue", "jpeg_dominant_color": "red"},
        "resume-upload": {"new_image_dominant_color": "green"},
        "resume-reuse": {"remembered_image_dominant_color": "green"},
        "web-cached": {"official_result_url": OFFICIAL_PAGE},
        "web-live": {"page_h1": OFFICIAL_PAGE_H1},
    }
    with tempfile.TemporaryDirectory(prefix="cabal-codex-probe-self-test-") as root:
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
        for mode in MODES:
            json.loads(write_schema(directory, mode).read_text(encoding="utf-8"))
    for mode, expected in expectations.items():
        schema = mode_schema(mode)
        if "const" in json.dumps(schema) or any(
            value in json.dumps(schema) for value in expected.values() if " " in value
        ):
            raise ProbeFailure("offline schema self-test failed")
        _, result, actions, official_page_seen = parse_public_output(
            jsonl_fixture(
                expected,
                "search-fetch"
                if mode == "web-live"
                else "search"
                if mode == "web-cached"
                else "none",
            )
        )
        validate_mode(mode, result, expected, actions, official_page_seen)
        if mode == "web-cached":
            validate_mode(
                mode,
                {"official_result_url": OFFICIAL_PAGE + "features"},
                expected,
                actions,
                official_page_seen,
            )
        wrong = dict(expected)
        first_key = next(iter(wrong))
        wrong[first_key] = "incorrect"
        try:
            validate_mode(mode, wrong, expected, actions, official_page_seen)
        except ProbeFailure:
            pass
        else:
            raise ProbeFailure("offline mode validator self-test failed")

    for mode, actions, official_page_seen in (
        ("web-cached", set(), False),
        ("web-cached", {"search", "fetch"}, True),
        ("web-live", {"search"}, True),
        ("web-live", {"search", "fetch"}, False),
    ):
        try:
            validate_mode(
                mode,
                expectations[mode],
                expectations[mode],
                actions,
                official_page_seen,
            )
        except ProbeFailure:
            pass
        else:
            raise ProbeFailure("offline web-lifecycle self-test failed")

    reuse = resume_argv(
        Path("schema.json"), "123e4567-e89b-12d3-a456-426614174000", ()
    )
    if "-i" in reuse:
        raise ProbeFailure("offline resume argv self-test failed")
    try:
        parse_public_output("not-json")
    except ProbeFailure:
        pass
    else:
        raise ProbeFailure("offline parser self-test failed")

    def expect_process_failure(exception: Exception, expected_message: str) -> None:
        with patch("subprocess.run", side_effect=exception):
            try:
                run_probe(
                    Path("."),
                    ["codex"],
                    "private prompt",
                    "media-initial",
                    expectations["media-initial"],
                )
            except ProbeFailure as error:
                if str(error) != expected_message or "private" in str(error):
                    raise ProbeFailure("offline process-error self-test failed")
            else:
                raise ProbeFailure("offline process-error self-test failed")

    expect_process_failure(
        subprocess.TimeoutExpired(["codex"], PROBE_TIMEOUT_SECONDS),
        "Codex probe timed out",
    )
    expect_process_failure(
        OSError("private path and credential detail"),
        "Codex probe process could not start",
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
        "Codex version check timed out",
        subprocess.TimeoutExpired(["codex", sensitive_marker], VERSION_TIMEOUT_SECONDS),
    )
    expect_cli_failure(
        ["media-initial"],
        "Codex probe interrupted",
        KeyboardInterrupt(sensitive_marker),
    )
    expect_cli_failure(
        ["media-initial"],
        "Codex emitted malformed public JSONL",
        [
            subprocess.CompletedProcess(
                ["codex", "--version"], 0, EXPECTED_VERSION + "\n", ""
            ),
            subprocess.CompletedProcess(
                ["codex", sensitive_marker], 0, sensitive_marker + "\n", ""
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
        parser.add_argument(
            "--debug-public",
            action="store_true",
            help="print only fixed public lifecycle booleans; never raw backend data",
        )
        parser.add_argument("modes", nargs="*", choices=MODES, default=MODES)
        args = parser.parse_args(argv)
        if args.self_test:
            run_self_test()
            print("PASS self-test")
            return 0
        require_version()
        with tempfile.TemporaryDirectory(prefix="cabal-codex-probe-") as temp_dir:
            with tempfile.TemporaryDirectory(
                prefix="cabal-codex-probe-inputs-"
            ) as inputs_dir:
                sealed_inputs = Path(inputs_dir)
                sealed_inputs.chmod(0o700)
                run_modes(
                    Path(temp_dir),
                    sealed_inputs,
                    tuple(args.modes) or MODES,
                    args.debug_public,
                )
    except ProbeFailure as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1
    except subprocess.TimeoutExpired:
        print("FAIL: Codex probe timed out", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        print("FAIL: Codex probe interrupted", file=sys.stderr)
        return 1
    except (OSError, UnicodeError, ValueError):
        print("FAIL: Codex probe I/O or decode failure", file=sys.stderr)
        return 1
    except Exception:
        print("FAIL: Codex probe parser failure", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
