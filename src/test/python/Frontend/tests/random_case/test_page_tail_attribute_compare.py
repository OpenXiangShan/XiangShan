from .page_tail_attribute_compare import (
    _normalized_program_map,
    _normalized_realization,
    _normalized_scenario,
)


def test_attribute_compare_normalizers_reject_non_attribute_program_drift():
    control_map = {
        "binary_path": "/control/case.bin",
        "source_path": "/control/case.S",
        "instructions": [{"pc": 0x80000000, "size": 4}],
    }
    candidate_map = {
        **control_map,
        "binary_path": "/candidate/case.bin",
        "source_path": "/candidate/case.S",
        "instructions": [{"pc": 0x80000000, "size": 4}],
    }
    assert _normalized_program_map(control_map) == _normalized_program_map(
        candidate_map
    )
    candidate_map["instructions"][0]["size"] = 2
    assert _normalized_program_map(control_map) != _normalized_program_map(
        candidate_map
    )


def test_attribute_compare_normalizers_allow_only_reviewed_scenario_delta():
    control = {
        "parameters": {
            "pattern_reuse": {"pattern_id": "page_tail_pair_sparse", "attribute_mode": "alternating"},
            "translation": {
                "attribute_plan": {"profile": "alternating", "pma_pattern": [1, 0]},
                "physical_base": 0x80400000,
            },
        }
    }
    candidate = {
        "parameters": {
            "pattern_reuse": {"pattern_id": "page_tail_pair_sparse", "attribute_mode": "rvi_nc_pair"},
            "translation": {
                "attribute_plan": {"profile": "rvi_nc_pair", "pma_pattern": [1, 0, 1, 0, 0]},
                "physical_base": 0x80400000,
            },
        }
    }
    assert _normalized_scenario(control) == _normalized_scenario(candidate)
    candidate["parameters"]["translation"]["physical_base"] += 0x1000
    assert _normalized_scenario(control) != _normalized_scenario(candidate)

    assert _normalized_realization({"artifact_paths": {"trace": "a"}, "status": "REALIZED"}) == (
        _normalized_realization({"artifact_paths": {"trace": "b"}, "status": "REALIZED"})
    )
