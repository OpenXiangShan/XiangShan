# coding=utf-8
"""
Frontend verification environment pytest global config.

This file intentionally stays thin:
  1. add the compiled Frontend pylib to `sys.path`
  2. add the Frontend package root to `sys.path`
  3. ensure the build-frontend artifact directory exists
"""

import os
import sys
from datetime import datetime
from pathlib import Path

import pytest

_TEST_ROOT = os.path.dirname(os.path.abspath(__file__))

if _TEST_ROOT not in sys.path:
    sys.path.insert(0, _TEST_ROOT)

from env.runtime.pylib import (
    frontend_build_root_path,
    frontend_pylib_path,
)

_PYLIB_PATH = str(frontend_pylib_path())
_DATA_DIR = str(frontend_build_root_path() / "artifacts")

for _path in (_PYLIB_PATH,):
    if _path not in sys.path:
        sys.path.insert(0, _path)

os.makedirs(_DATA_DIR, exist_ok=True)


def pytest_configure(config):
    if not hasattr(config, "workerinput") and not os.getenv("TB_RUN_ID", "").strip():
        os.environ["TB_RUN_ID"] = (
            f"frontend_pytest_{datetime.now().strftime('%Y%m%d_%H%M%S_%f')}_{os.getpid()}"
        )
    config.addinivalue_line(
        "markers",
        "funcov_bins(*bin_ids): declare the functional coverage Bin_ID targets for this DUT test",
    )
    config.addinivalue_line(
        "markers",
        "funcov_tps(*tp_ids): declare the testpoint IDs targeted by this DUT test",
    )
    config.addinivalue_line(
        "markers",
        (
            "funcov_closure_pending: keep a currently unreachable coverage-closure "
            "scenario runnable without making it a release-gate failure"
        ),
    )


def pytest_testnodedown(node, error):
    del error
    report_path = node.workeroutput.get("toffee_report_path")
    if report_path:
        paths = getattr(node.config, "_frontend_toffee_worker_reports", None)
        if paths is None:
            paths = []
            node.config._frontend_toffee_worker_reports = paths
        paths.append(Path(report_path))


@pytest.hookimpl(trylast=True)
def pytest_sessionfinish(session, exitstatus):
    from env.runtime.fixtures import (
        finish_vcs_batch_dut,
        toffee_session_report_path,
        write_toffee_session_report,
    )

    if hasattr(session.config, "workerinput"):
        worker_id = str(session.config.workerinput.get("workerid", "worker"))
        report_path = write_toffee_session_report(
            session, toffee_session_report_path(worker_id)
        )
        if report_path is not None:
            session.config.workeroutput["toffee_report_path"] = str(report_path.resolve())
    else:
        worker_reports = list(getattr(session.config, "_frontend_toffee_worker_reports", []))
        if worker_reports:
            from env.funcov.toffee_bridge import merge_native_toffee_reports

            merge_native_toffee_reports(worker_reports, toffee_session_report_path())
        else:
            write_toffee_session_report(session)
    finish_vcs_batch_dut()


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_makereport(item, call):
    """Expose phase reports to the fixture that finalizes funcov artifacts."""
    outcome = yield
    report = outcome.get_result()
    setattr(item, f"rep_{report.when}", report)
