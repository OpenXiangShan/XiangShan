#!/usr/bin/env python3
"""Validate and normalize the generated XiangShan RTL filelist for MemBlock."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from collections import defaultdict
from pathlib import Path
from typing import Any


MODULE_RE = re.compile(r"(?m)^\s*module\s+([A-Za-z_$][A-Za-z0-9_$]*)\b")


class FilelistError(RuntimeError):
    pass


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def sha256_files(paths: list[Path], base: Path) -> str:
    digest = hashlib.sha256()
    for path in paths:
        digest.update(str(path.resolve().relative_to(base.resolve())).encode("utf-8"))
        digest.update(b"\0")
        with path.open("rb") as stream:
            for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                digest.update(chunk)
        digest.update(b"\0")
    return digest.hexdigest()


def strip_comments(text: str) -> str:
    text = re.sub(r"/\*.*?\*/", "", text, flags=re.DOTALL)
    return re.sub(r"//[^\n]*", "", text)


def load_source_filelist(path: Path) -> list[Path]:
    base = path.resolve().parent
    files: list[Path] = []
    seen: set[Path] = set()
    for line_number, raw_line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        line = raw_line.split("#", 1)[0].strip()
        if not line:
            continue
        if line.startswith(("+", "-")):
            raise FilelistError(
                f"unsupported directive at {path}:{line_number}: {line!r}"
            )
        candidate = Path(line)
        resolved = (candidate if candidate.is_absolute() else base / candidate).resolve()
        if resolved.suffix not in (".sv", ".v"):
            raise FilelistError(f"unsupported RTL file at {path}:{line_number}: {line!r}")
        if not resolved.is_file():
            raise FilelistError(f"missing RTL file at {path}:{line_number}: {resolved}")
        if resolved in seen:
            raise FilelistError(f"duplicate RTL file at {path}:{line_number}: {resolved}")
        seen.add(resolved)
        files.append(resolved)
    if not files:
        raise FilelistError(f"filelist is empty: {path}")
    return files


def declared_modules(path: Path) -> list[str]:
    text = strip_comments(path.read_text(encoding="utf-8", errors="replace"))
    return MODULE_RE.findall(text)


def prepare(
    source_filelist: Path,
    bind_file: Path,
    config: dict[str, Any],
) -> tuple[list[Path], dict[str, Any]]:
    source_files = load_source_filelist(source_filelist)
    rtl_config = config.get("rtl_filelist", {})
    excluded_basenames = set(rtl_config.get("exclude_basenames", []))
    excluded = [path for path in source_files if path.name in excluded_basenames]
    selected = [path for path in source_files if path.name not in excluded_basenames]

    supplemental: list[Path] = []
    source_directory = source_filelist.resolve().parent
    selected_set = set(selected)
    for pattern in rtl_config.get("supplemental_globs", []):
        matches = sorted(
            (path.resolve() for path in source_directory.glob(pattern) if path.is_file()),
            key=str,
        )
        if not matches:
            raise FilelistError(
                f"supplemental RTL pattern matched no files in {source_directory}: {pattern}"
            )
        for path in matches:
            if path.suffix not in (".sv", ".v"):
                raise FilelistError(f"supplemental RTL is not Verilog: {path}")
            if path not in selected_set:
                selected_set.add(path)
                supplemental.append(path)
    selected.extend(supplemental)

    top_basename = rtl_config.get("top_file", "MemBlock.sv")
    top_matches = [path for path in selected if path.name == top_basename]
    if len(top_matches) != 1:
        raise FilelistError(
            f"expected exactly one selected {top_basename}, found {len(top_matches)}"
        )
    top_path = top_matches[0]
    module_to_files: dict[str, list[Path]] = defaultdict(list)
    for path in selected:
        for module in declared_modules(path):
            module_to_files[module].append(path)
    collisions = {
        module: paths for module, paths in module_to_files.items() if len(paths) > 1
    }
    if collisions:
        details = "; ".join(
            f"{module}: {', '.join(str(path) for path in paths)}"
            for module, paths in sorted(collisions.items())
        )
        raise FilelistError(f"duplicate module declarations: {details}")
    if config.get("module", "MemBlock") not in module_to_files:
        raise FilelistError("top module declaration is absent from the selected RTL")

    bind_path = bind_file.resolve()
    if not bind_path.is_file():
        raise FilelistError(f"assertion bind file is missing: {bind_path}")
    output_files = selected + [bind_path]
    metadata = {
        "schema_version": 1,
        "top_module": config.get("module", "MemBlock"),
        "top_file": str(top_path),
        "source_filelist": str(source_filelist.resolve()),
        "source_filelist_sha256": sha256_file(source_filelist),
        "source_file_count": len(source_files),
        "selected_file_count": len(selected),
        "declared_module_count": len(module_to_files),
        "excluded_files": [str(path) for path in excluded],
        "supplemental_files": [str(path) for path in supplemental],
        "bind_file": str(bind_path),
        "bind_file_sha256": sha256_file(bind_path),
        "complete_rtl_sha256": sha256_files(selected, source_directory),
    }
    return output_files, metadata


def render_filelist(files: list[Path]) -> str:
    return "".join(f"{path}\n" for path in files)


def render_metadata(metadata: dict[str, Any]) -> str:
    return json.dumps(metadata, indent=2, sort_keys=True) + "\n"


def write_if_changed(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if not path.exists() or path.read_text(encoding="utf-8") != content:
        path.write_text(content, encoding="utf-8")


def check_content(path: Path, expected: str) -> None:
    if not path.exists() or path.read_text(encoding="utf-8") != expected:
        raise FilelistError(f"generated file is stale or missing: {path}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("generate", "check"))
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--bind", type=Path, required=True)
    parser.add_argument("--config", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--metadata", type=Path, required=True)
    args = parser.parse_args()
    try:
        config = json.loads(args.config.read_text(encoding="utf-8"))
        files, metadata = prepare(args.source, args.bind, config)
        filelist_text = render_filelist(files)
        metadata_text = render_metadata(metadata)
        if args.action == "generate":
            write_if_changed(args.output, filelist_text)
            write_if_changed(args.metadata, metadata_text)
        else:
            check_content(args.output, filelist_text)
            check_content(args.metadata, metadata_text)
    except (OSError, json.JSONDecodeError, FilelistError) as error:
        print(f"prepare_rtl.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
