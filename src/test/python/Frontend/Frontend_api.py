# coding=utf-8
"""
Frontend DUT creation and public root API exports.

This module mirrors the role of `MemBlock_api.py` at the Frontend package root:
  1. expose the DUT factory and pytest fixture used by tests
  2. expose the public `api_Frontend_*` helpers from the real env layer
  3. keep the root import contract stable while `env/` remains the implementation
"""

import os
import sys


_HERE = os.path.dirname(os.path.abspath(__file__))
_REPO_ROOT = os.path.abspath(os.path.join(_HERE, "..", "..", "..", ".."))
_PYLIB_PATH = os.path.join(_REPO_ROOT, "build-frontend", "pylib")

for _path in (_PYLIB_PATH, _HERE):
    if _path not in sys.path:
        sys.path.insert(0, _path)

from env.api import (
    api_Frontend_check_pc_sequence,
    api_Frontend_enable_fst_dump,
    api_Frontend_flush_fst_dump,
    api_Frontend_get_branch_stats,
    api_Frontend_inject_redirect,
    api_Frontend_load_golden_trace,
    api_Frontend_load_program,
    api_Frontend_load_program_file,
    api_Frontend_pause_fst_dump,
    api_Frontend_prepare_program_and_nemu_trace,
    api_Frontend_run_until_commit,
    api_Frontend_set_bp_ctrl_enable,
    api_Frontend_set_log_level,
)
from env.fixtures import create_dut, dut


__all__ = [
    "api_Frontend_check_pc_sequence",
    "api_Frontend_enable_fst_dump",
    "api_Frontend_flush_fst_dump",
    "api_Frontend_get_branch_stats",
    "api_Frontend_inject_redirect",
    "api_Frontend_load_golden_trace",
    "api_Frontend_load_program",
    "api_Frontend_load_program_file",
    "api_Frontend_pause_fst_dump",
    "api_Frontend_prepare_program_and_nemu_trace",
    "api_Frontend_run_until_commit",
    "api_Frontend_set_bp_ctrl_enable",
    "api_Frontend_set_log_level",
    "create_dut",
    "dut",
]
