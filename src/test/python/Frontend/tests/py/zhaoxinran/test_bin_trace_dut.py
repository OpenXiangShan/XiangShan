import os

import pytest

from env.runtime.frontend_init import FrontendInitConfig
from tests.py.support.bin_trace import run_bin_trace


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_RUN_PIPELINE_TEST = os.getenv("TB_BIN_TRACE_PIPELINE") == "1"


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.skipif(not _RUN_PIPELINE_TEST, reason="pipeline-only test")
@pytest.mark.parametrize("frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True)
def test_bin_trace(env):
    run_bin_trace(env)
