import pytest

from tools.write_frontend_build_manifest import _is_dut_input_path


@pytest.mark.parametrize(
    "path",
    [
        "Makefile",
        "build.sc",
        "src/main/scala/xiangshan/frontend/Frontend.scala",
        "src/main/resources/frontend.conf",
        "rocket-chip/src/main/scala/foo.scala",
    ],
)
def test_design_inputs_require_dut_rebuild(path):
    assert _is_dut_input_path(path)


@pytest.mark.parametrize(
    "path",
    [
        "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv",
        "src/test/python/Frontend/env/funcov/py/ifu/cfvec_funcov.py",
        "src/test/python/Frontend/tests/py/jiabowen/test_functional_coverage_baremode.py",
        "docs/README.md",
    ],
)
def test_verification_inputs_do_not_require_dut_rebuild(path):
    assert not _is_dut_input_path(path)
