from pathlib import Path

import pytest


_REPO_ROOT = Path(__file__).resolve().parents[7]
_COMMON_SIGNALS = {
    "itlb_ptw_request_get_gpa": "Frontend_top.Frontend.inner_itlb.io_ptw_req_0_bits_getGpa",
}

def _registered_names(offset: Path) -> set[str]:
    return {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }


@pytest.mark.parametrize("simulator", ["verilator", "vcs"])
def test_translation_permission_signal_contract_matches_generated_inventory(simulator: str) -> None:
    offset = _REPO_ROOT / "build-frontend" / f"pylib-{simulator}" / "Frontend" / "Frontend_offset.yaml"
    assert offset.is_file(), f"{simulator} DUT signal inventory is required"

    registered = _registered_names(offset)
    missing = {name: signal for name, signal in _COMMON_SIGNALS.items() if signal not in registered}

    assert not missing, {"simulator": simulator, "missing_translation_permission_signals": missing}
