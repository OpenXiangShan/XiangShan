from pathlib import Path

import pytest


_REPO_ROOT = Path(__file__).resolve().parents[7]
_COMMON_SIGNALS = {
    "mainpipe_s1_valid": "Frontend_top.Frontend.inner_icache.mainPipe.s1_valid",
    "mainpipe_start_vaddr_pruned": "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_vAddr_0_addr",
    "mainpipe_p_tag": "Frontend_top.Frontend.inner_icache.mainPipe.s1_wayLookupEntry_0_pTag",
    "mainpipe_pmp_execute": "Frontend_top.Frontend.inner_icache.mainPipe.io_pmp_resp_instr",
    "mainpipe_pmp_mmio": "Frontend_top.Frontend.inner_icache.mainPipe.io_pmp_resp_mmio",
    "prefetchpipe_s1_valid": "Frontend_top.Frontend.inner_icache.prefetcher.s1_valid",
    "prefetchpipe_start_vaddr": "Frontend_top.Frontend.inner_icache.prefetcher.s1_req_0_startVAddr_addr",
    "prefetchpipe_p_tag": "Frontend_top.Frontend.inner_icache.prefetcher.s1_pTag",
    "prefetchpipe_pmp_addr": "Frontend_top.Frontend.inner_icache.prefetcher.io_pmp_req_bits_addr",
    "prefetchpipe_pmp_execute": "Frontend_top.Frontend.inner_icache.prefetcher.io_pmp_resp_instr",
    "prefetchpipe_pmp_mmio": "Frontend_top.Frontend.inner_icache.prefetcher.io_pmp_resp_mmio",
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
