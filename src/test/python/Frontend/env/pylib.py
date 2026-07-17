from __future__ import annotations

import os
from pathlib import Path


def frontend_pylib_path() -> Path:
    override = os.getenv("TB_FRONTEND_PYLIB", "").strip()
    if override:
        return Path(override).expanduser()

    sim = os.getenv("TB_FRONTEND_SIM", "verilator").strip().lower()
    if sim not in {"verilator", "vcs"}:
        raise RuntimeError("TB_FRONTEND_SIM must be one of: verilator vcs")

    repo_root = Path(__file__).resolve().parents[5]
    return repo_root / "build-frontend" / f"pylib-{sim}"


def frontend_offset_path() -> Path:
    return frontend_pylib_path() / "Frontend" / "Frontend_offset.yaml"
