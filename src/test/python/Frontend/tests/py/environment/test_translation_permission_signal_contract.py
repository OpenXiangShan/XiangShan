from pathlib import Path
from env.runtime.pylib import frontend_itlb_ptw_req_get_gpa_path, frontend_offset_path

def _registered_names(offset: Path) -> set[str]:
    return {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }


def test_translation_permission_signal_contract_matches_generated_inventory() -> None:
    offset = frontend_offset_path()
    assert offset.is_file(), f"selected DUT signal inventory is required: {offset}"

    registered = _registered_names(offset)
    signal = frontend_itlb_ptw_req_get_gpa_path()
    missing = {"itlb_ptw_request_get_gpa": signal} if signal not in registered else {}

    assert not missing, {"missing_translation_permission_signals": missing}
