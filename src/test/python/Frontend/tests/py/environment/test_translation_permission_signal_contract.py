from pathlib import Path

import pytest


_REPO_ROOT = Path(__file__).resolve().parents[7]
_COMMON_SIGNALS = {
    "mainpipe_request_va": "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_vAddr_0_addr",
    "mainpipe_pmp_execute": "Frontend_top.Frontend.inner_icache.mainPipe.io_pmp_resp_instr",
    "mainpipe_pmp_mmio": "Frontend_top.Frontend.inner_icache.mainPipe.io_pmp_resp_mmio",
}
_VCS_PMP_REQUEST_ADDR = (
    "Frontend_top.Frontend.inner_icache.mainPipe.io_pmp_req_bits_addr",
    "Frontend_top.Frontend.inner_icache.mainPipe.__Vtogcov__io_pmp_req_bits_addr",
)


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
    if simulator == "vcs" and not any(signal in registered for signal in _VCS_PMP_REQUEST_ADDR):
        missing["mainpipe_pmp_request_addr"] = list(_VCS_PMP_REQUEST_ADDR)

    assert not missing, {"simulator": simulator, "missing_translation_permission_signals": missing}
