from __future__ import annotations

import os
import pytest

from env.runtime.frontend_init import FrontendInitConfig
from env.sequences import (
    TranslationScenario,
    TranslationPtwResponseOverride,
    TranslationPte,
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
        id="vs-nonleaf-gaf",
    ),
)


@pytest.mark.parametrize("scenario,expected_fault", _PROVENANCE_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_address_translation_gstage_provenance(
    env,
    scenario: TranslationScenario,
    expected_fault: str,
) -> None:
    run_gstage_provenance(env, scenario, expected_fault)


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nonleaf_pte_gpf_frontend_gpaddr(env) -> None:
    va = 0x8020_100C
    table_gpa = 0x8003_F000
    scenario = TranslationScenario(
        scenario_id="nonleaf-pte-gpf-gpaddr",
        va=va,
        gpa=0x8060_000C,
        pa=0x8040_000C,
        payload=b"\x13\x00\x00\x00" * 16,
        s2xlate=3,
        priv_virt=1,
        s1_pte=TranslationPte(asid=5, vmid=7),
        s2_pte=TranslationPte(vmid=7),
        vsatp_asid=5,
        hgatp_vmid=7,
        expected_path="fault",
        expected_result="guest_fault",
        ptw_response_overrides=(
            TranslationPtwResponseOverride(
                vpn=va >> 12,
                s2xlate=3,
                patch=(
                    ("s1_entry_level", 1),
                    ("s1_entry_ppn", (table_gpa >> 12) >> 3),
                    ("s1_ppn_low", tuple(
                        (table_gpa >> 12) & 7 if i == ((va >> 12) & 7) else 0
                        for i in range(8)
                    )),
                    ("s1_entry_v", 1),
                    ("s1_entry_perm_r", 0),
                    ("s1_entry_perm_w", 0),
                    ("s1_entry_perm_x", 0),
                    ("s1_pf", 0),
                    ("s1_af", 0),
                    ("s2_gpf", 1),
                ),
            ),
        ),
    )
    run_gstage_provenance(env, scenario, "instruction_guest_page_fault")
