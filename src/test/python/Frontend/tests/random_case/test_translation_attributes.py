from __future__ import annotations

from types import SimpleNamespace

import pytest

from .translation_attributes import (
    PBMT_IO,
    PBMT_NC,
    attribute_plan_from_dict,
    translation_attribute_plan,
)
from .translation_timing import TRANSLATION_SCHEMA_VERSION, build_translation_scenario


def test_attribute_profiles_are_deterministic_and_cover_required_axes() -> None:
    profiles = [translation_attribute_plan(index) for index in range(6)]

    assert [plan.profile for plan in profiles] == [
        "pma_cacheable",
        "pma_non_cacheable",
        "pbmt_nc_over_cacheable_pma",
        "pbmt_io_over_cacheable_pma",
        "pma_cacheable_to_non_cacheable",
        "pma_non_cacheable_to_cacheable",
    ]
    assert {plan.pbmt for plan in profiles} == {0, PBMT_NC, PBMT_IO}
    assert profiles[4].for_pages(4) == (True, False, True, False)
    assert profiles[5].for_pages(4) == (False, True, False, True)
    assert profiles[2].pbmte == profiles[3].pbmte == 1


def test_attribute_profile_round_trip_preserves_replay_contract() -> None:
    original = translation_attribute_plan(8)
    restored = attribute_plan_from_dict(original.to_dict())

    assert restored == original
    assert restored.for_pages(3) == (True, True, True)
    assert restored.pbmt_for_pages(3) == (PBMT_NC, PBMT_NC, PBMT_NC)


def test_per_page_pbmt_contract_requires_pbmte_and_preserves_legacy_shape() -> None:
    mixed = attribute_plan_from_dict(
        {
            "schema_version": 1,
            "profile": "mixed-page-pbmt",
            "pbmt": 0,
            "pbmte": 1,
            "pma_pattern": [1, 0, 1],
            "pbmt_pattern": [0, PBMT_NC, PBMT_NC],
        }
    )

    assert mixed.pbmt_for_pages(5) == (0, PBMT_NC, PBMT_NC, 0, PBMT_NC)
    assert mixed.to_dict()["pbmt_pattern"] == [0, PBMT_NC, PBMT_NC]

    with pytest.raises(ValueError, match="PBMTE"):
        attribute_plan_from_dict(
            {
                "schema_version": 1,
                "profile": "illegal-mixed-page-pbmt",
                "pbmt": 0,
                "pbmte": 0,
                "pma_pattern": [1],
                "pbmt_pattern": [PBMT_NC],
            }
        )


@pytest.mark.parametrize(
    ("ordinal", "expected_machine_pbmte"),
    (
        pytest.param(0, None, id="pma-cacheable-does-not-request-pbmte"),
        pytest.param(1, None, id="pma-non-cacheable-does-not-request-pbmte"),
        pytest.param(2, 1, id="pbmt-nc-requires-pbmte"),
        pytest.param(3, 1, id="pbmt-io-requires-pbmte"),
        pytest.param(4, None, id="cross-page-pma-does-not-request-pbmte"),
    ),
)
def test_translation_builder_requests_pbmte_only_for_non_pma_profiles(
    ordinal: int,
    expected_machine_pbmte: int | None,
) -> None:
    plan = translation_attribute_plan(ordinal)
    scenario_spec = SimpleNamespace(
        scenario_id=f"translation-attribute-{ordinal}",
        ordinal=ordinal,
        parameters={
            "translation": {
                "schema_version": TRANSLATION_SCHEMA_VERSION,
                "randomizer_seed": 0x5A390001,
                "ordinal": ordinal,
                "attribute_plan": plan.to_dict(),
            }
        },
    )

    scenario = build_translation_scenario(
        scenario_spec,
        b"\x13\x00\x00\x00" * 1024,
    )

    assert scenario.ptw_machine_pbmte == expected_machine_pbmte


def test_translation_builder_propagates_per_page_pbmt_without_changing_pma() -> None:
    plan = attribute_plan_from_dict(
        {
            "schema_version": 1,
            "profile": "page-pbmt-nc",
            "pbmt": 0,
            "pbmte": 1,
            "pma_pattern": [1, 0, 1],
            "pbmt_pattern": [0, PBMT_NC, PBMT_NC],
        }
    )
    scenario_spec = SimpleNamespace(
        scenario_id="translation-page-pbmt",
        ordinal=0,
        parameters={
            "translation": {
                "schema_version": TRANSLATION_SCHEMA_VERSION,
                "randomizer_seed": 0x5A390001,
                "ordinal": 0,
                "attribute_plan": plan.to_dict(),
            }
        },
    )

    scenario = build_translation_scenario(
        scenario_spec,
        b"\x13\x00\x00\x00" * 3072,
    )

    assert scenario.s1_page_pbmt == (0, PBMT_NC, PBMT_NC)
    assert scenario.s1_pte.pbmt == 0
    assert scenario.ptw_machine_pbmte == 1


@pytest.mark.parametrize(
    "payload",
    (
        {"schema_version": 0, "profile": "bad", "pbmt": 0, "pbmte": 0, "pma_pattern": [1]},
        {"schema_version": 1, "profile": "bad", "pbmt": 1, "pbmte": 0, "pma_pattern": [1]},
        {"schema_version": 1, "profile": "bad", "pbmt": 3, "pbmte": 1, "pma_pattern": [1]},
    ),
)
def test_attribute_profile_rejects_invalid_contract(payload: dict[str, object]) -> None:
    with pytest.raises(ValueError):
        attribute_plan_from_dict(payload)
