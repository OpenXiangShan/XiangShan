from __future__ import annotations

import pytest

from . import campaign_case_matrix as campaign_runner


def test_family_count_overrides_make_the_generation_matrix_explicit() -> None:
    overrides = campaign_runner._parse_family_count_overrides(
        ["cfg_layout_mix=33", "translation_timing_cross=3"]
    )
    families, counts = campaign_runner._resolve_family_case_counts([], overrides, 34)

    assert families == ["cfg_layout_mix", "translation_timing_cross"]
    assert counts == {"cfg_layout_mix": 33, "translation_timing_cross": 3}

    with pytest.raises(ValueError, match="duplicate"):
        campaign_runner._parse_family_count_overrides(
            ["cfg_layout_mix=1", "cfg_layout_mix=2"]
        )
    with pytest.raises(ValueError, match="not selected"):
        campaign_runner._resolve_family_case_counts(
            ["cfg_layout_mix"],
            {"translation_timing_cross": 1},
            1,
        )
