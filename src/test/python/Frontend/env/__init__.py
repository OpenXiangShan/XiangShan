"""Stable package-root facade for the frontend memblock-style migration.

Keep the root exports intentionally conservative during the refactor: the
canonical implementations now live under ``agents/``, ``monitors/``,
``model/``, ``sequences/``, and ``request_apis.py``, while this module keeps
the historically stable imports available for tests and downstream users.
"""

from .api import (
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
    api_Frontend_run_until_golden_complete,
    api_Frontend_set_bp_ctrl_enable,
    api_Frontend_set_log_level,
)
from .agents import ICacheAgent, PTWAgent, UncacheAgent
from .core.backend_model import BackendModel
from .core.frontend_env import FrontendEnv
from .monitors import FrontendMonitor
from .support.logging_utils import configure_env_logging
from .model import BranchChecker, GoldenTrace, MemoryModel, PageTableModel, TraceEntry

__all__ = [
    "FrontendEnv",
    "ICacheAgent",
    "PTWAgent",
    "UncacheAgent",
    "BackendModel",
    "FrontendMonitor",
    "BranchChecker",
    "MemoryModel",
    "PageTableModel",
    "GoldenTrace",
    "TraceEntry",
    "configure_env_logging",
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
    "api_Frontend_run_until_golden_complete",
    "api_Frontend_set_bp_ctrl_enable",
    "api_Frontend_set_log_level",
]
