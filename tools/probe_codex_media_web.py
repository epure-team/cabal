#!/usr/bin/env python3
"""Authenticated Codex 0.131.0 media/resume/web probe with public-only output."""

from __future__ import annotations

import argparse
import base64
import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path


EXPECTED_VERSION = "codex-cli 0.131.0"
THREAD_ID = re.compile(r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")
MODES = (
    "media-initial",
    "resume-upload",
    "resume-reuse",
    "web-cached",
    "web-live",
)

PNG_FIXTURE = (
    "iVBORw0KGgoAAAANSUhEUgAAAAIAAAACAQMAAABIeJ9nAAAAIGNIUk0AAHomAACAhAAA+gAA"
    "AIDoAAB1MAAA6mAAADqYAAAXcJy6UTwAAAAGUExURQAA/////3vcmSwAAAABYktHRAH/Ai3e"
    "AAAAB3RJTUUH6gkCCDAvppZirAAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyNi0wOS0wMlQwOD"
    "o0ODo0NyswMDowMJ9uVKMAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjYtMDktMDJUMDg6NDg6"
    "NDcrMDA6MDDuM+wfAAAAKHRFWHRkYXRlOnRpbWVzdGFtcAAyMDI2LTA5LTAyVDA4OjQ4Oj"
    "Q3KzAwOjAwuSbNwAAAAAxJREFUCNdjYGBgAAAABAABJzQnCgAAAABJRU5ErkJggg=="
)
JPEG_FIXTURE = (
    "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAMCAgICAgMCAgIDAwMDBAYEBAQEBAgGBgUGCQgK"
    "CgkICQkKDA8MCgsOCwkJDRENDg8QEBEQCgwSExIQEw8QEBD/2wBDAQMDAwQDBAgEBAgQCwkL"
    "EBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBD/wA"
    "ARCAACAAIDAREAAhEBAxEB/8QAFAABAAAAAAAAAAAAAAAAAAAACP/EABQQAQAAAAAAAAAAAAAA"
    "AAAAAAD/xAAVAQEBAAAAAAAAAAAAAAAAAAAHCf/EABQRAQAAAAAAAAAAAAAAAAAAAAD/2gAMAw"
    "EAAhEDEQA/ADoDFU3/2Q=="
)


class ProbeFailure(Exception):
    pass


def require_version() -> None:
    completed = subprocess.run(
        ["codex", "--version"],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    if completed.returncode != 0 or completed.stdout.strip() != EXPECTED_VERSION:
        raise ProbeFailure(f"probe requires exactly {EXPECTED_VERSION}")


def write_fixtures(workspace: Path) -> tuple[Path, Path, Path]:
    png = workspace / "fixture blue.png"
    jpeg = workspace / "fixture red.jpg"
    schema = workspace / "result.schema.json"
    png.write_bytes(base64.b64decode(PNG_FIXTURE))
    jpeg.write_bytes(base64.b64decode(JPEG_FIXTURE))
    schema.write_text(
        json.dumps(
            {
                "$schema": "https://json-schema.org/draft/2020-12/schema",
                "type": "object",
                "properties": {"ok": {"type": "boolean", "const": True}},
                "required": ["ok"],
                "additionalProperties": False,
            },
            separators=(",", ":"),
        ),
        encoding="utf-8",
    )
    for path in (png, jpeg, schema):
        path.chmod(0o600)
    return png, jpeg, schema


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


def resume_argv(
    schema: Path, session_id: str, images: tuple[Path, ...]
) -> list[str]:
    # output-schema and sandbox are exec-root options; resume JSON/config/image
    # options follow the subcommand and its canonical UUID argument.
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


def run_probe(
    workspace: Path, argv: list[str], prompt: str, require_web: bool = False
) -> str:
    completed = subprocess.run(
        argv,
        cwd=workspace,
        input=prompt,
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    if completed.returncode != 0:
        raise ProbeFailure("Codex probe process failed (raw output suppressed)")

    session_id: str | None = None
    final_message: str | None = None
    web_started = False
    web_completed = False
    for line in completed.stdout.splitlines():
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue
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
        if item_type == "web_search":
            web_started = web_started or event_type == "item.started"
            web_completed = web_completed or event_type == "item.completed"

    if session_id is None:
        raise ProbeFailure("Codex emitted no canonical public thread.started UUID")
    try:
        result = json.loads(final_message or "")
    except json.JSONDecodeError as error:
        raise ProbeFailure("Codex emitted no schema-conforming public answer") from error
    if result != {"ok": True}:
        raise ProbeFailure("Codex public answer did not satisfy the probe assertion")
    if require_web and not (web_started and web_completed):
        raise ProbeFailure("Codex emitted no complete public web_search lifecycle")
    return session_id


def run_modes(
    workspace: Path, sealed_inputs: Path, selected_modes: tuple[str, ...]
) -> None:
    png, jpeg, schema = write_fixtures(sealed_inputs)
    session_id: str | None = None

    def ensure_session() -> str:
        nonlocal session_id
        if session_id is None:
            session_id = run_probe(
                workspace,
                initial_argv(schema, "disabled", (png, jpeg)),
                'Inspect both attached fixtures, then return exactly {"ok":true}.',
            )
        return session_id

    for mode in selected_modes:
        if mode == "media-initial":
            ensure_session()
        elif mode == "resume-upload":
            session_id = run_probe(
                workspace,
                resume_argv(schema, ensure_session(), (png, jpeg)),
                'Inspect both newly attached fixtures, then return exactly {"ok":true}.',
            )
        elif mode == "resume-reuse":
            session_id = run_probe(
                workspace,
                resume_argv(schema, ensure_session(), ()),
                'Resume without uploading fixtures again and return exactly {"ok":true}.',
            )
        elif mode == "web-cached":
            run_probe(
                workspace,
                initial_argv(schema, "cached", ()),
                "Use web search to find official OpenAI Codex CLI documentation, then "
                'return exactly {"ok":true}.',
                require_web=True,
            )
        elif mode == "web-live":
            run_probe(
                workspace,
                initial_argv(schema, "live", ()),
                "Use live web search and open an official OpenAI Codex CLI documentation "
                'page, then return exactly {"ok":true}.',
                require_web=True,
            )
        else:
            raise ProbeFailure("unknown probe mode")
        print(f"PASS {mode}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("modes", nargs="*", choices=MODES, default=MODES)
    args = parser.parse_args()
    try:
        require_version()
        with tempfile.TemporaryDirectory(prefix="cabal-codex-probe-") as temp_dir:
            with tempfile.TemporaryDirectory(
                prefix="cabal-codex-probe-inputs-"
            ) as inputs_dir:
                sealed_inputs = Path(inputs_dir)
                sealed_inputs.chmod(0o700)
                run_modes(
                    Path(temp_dir), sealed_inputs, tuple(args.modes) or MODES
                )
    except (OSError, ProbeFailure) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
