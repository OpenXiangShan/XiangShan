"""Compatibility shim for the legacy frontend monitor imports."""

from __future__ import annotations

from .model.branch_checker import BranchChecker
from .monitors.frontend_monitor import FrontendMonitor, Observation

__all__ = ["Observation", "FrontendMonitor", "BranchChecker"]
