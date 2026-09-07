from __future__ import annotations

import os
from pathlib import Path


def frontend_build_root_path() -> Path:
    repo_root = Path(__file__).resolve().parents[6]
    return repo_root / "build-frontend"


def frontend_itlb_ptw_req_get_gpa_path() -> str:
    return "Frontend_top.Frontend._inner_itlb_io_ptw_req_0_bits_getGpa"


def frontend_pylib_path() -> Path:
    sim = os.getenv("TB_FRONTEND_SIM", "verilator").strip().lower()
    if sim not in {"verilator", "vcs"}:
        raise RuntimeError("TB_FRONTEND_SIM must be one of: verilator vcs")

    return frontend_build_root_path() / f"pylib-{sim}"


def frontend_offset_path() -> Path:
    return frontend_pylib_path() / "Frontend" / "Frontend_offset.yaml"
