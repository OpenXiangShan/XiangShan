from __future__ import annotations

import os
import pytest

from env.runtime.frontend_init import FrontendInitConfig
from env.sequences import (
    TranslationScenario,
)
from tests.py.support.gstage_provenance import (
    scenario as _scenario,
    run_gstage_provenance,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


pytestmark = pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)


_PROVENANCE_CASES = (
    pytest.param(
        _scenario(
            "translation-gstage-fake-pte-gpf",
            s1_patch=(("s1_entry_v", 0), ("s1_pf", 0), ("s1_af", 0)),
            gpf=1,
            gaf=0,
        ),
        "instruction_guest_page_fault",
        True,
        id="fake-pte-gpf",
    ),
    pytest.param(
        _scenario(
            "translation-gstage-fake-pte-gaf",
            s1_patch=(("s1_entry_v", 0), ("s1_pf", 0), ("s1_af", 0)),
            gpf=0,
            gaf=1,
        ),
        "instruction_access_fault",
        False,
        id="fake-pte-gaf",
    ),
    pytest.param(
        _scenario(
            "translation-gstage-vs-nonleaf-gpf",
            s1_patch=(
                ("s1_entry_v", 1),
                ("s1_entry_perm_r", 0),
                ("s1_entry_perm_w", 0),
                ("s1_entry_perm_x", 0),
                ("s1_pf", 0),
                ("s1_af", 0),
            ),
            gpf=1,
            gaf=0,
        ),
        "instruction_guest_page_fault",
        True,
        id="vs-nonleaf-gpf",
    ),
    pytest.param(
        _scenario(
            "translation-gstage-vs-nonleaf-gaf",
            s1_patch=(
                ("s1_entry_v", 1),
                ("s1_entry_perm_r", 0),
                ("s1_entry_perm_w", 0),
                ("s1_entry_perm_x", 0),
                ("s1_pf", 0),
                ("s1_af", 0),
            ),
            gpf=0,
            gaf=1,
        ),
        "instruction_access_fault",
        False,
        id="vs-nonleaf-gaf",
    ),
)


@pytest.mark.parametrize("scenario,expected_fault,expect_gpaddr_write", _PROVENANCE_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_address_translation_gstage_provenance(
    env,
    scenario: TranslationScenario,
    expected_fault: str,
    expect_gpaddr_write: bool,
) -> None:
    run_gstage_provenance(env, scenario, expected_fault, expect_gpaddr_write)
