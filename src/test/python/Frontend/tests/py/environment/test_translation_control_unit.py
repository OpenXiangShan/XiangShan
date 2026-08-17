from __future__ import annotations

import pytest

from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend
from env.support import PmpPmaConfig
from env.support.pmp_pma import (
    PMA_ADDR_BASE,
    PMA_CFG_BASE,
    PMP_ADDR_BASE,
    PMP_CFG_BASE,
    csr_addresses_for_entry,
    encode_pmp_pma_addr,
    encode_pmp_pma_cfg,
)


def _env_with_events():
    events = []
    return FrontendEnv(FakeDUTFrontend(), register_callbacks=False, event_sink=events.append), events


def test_pmp_pma_cfg_and_address_encoding_matches_frontend_layout() -> None:
    cfg = PmpPmaConfig(match="napot", read=True, write=True, execute=True, locked=True, cacheable=True)

    assert encode_pmp_pma_cfg(cfg) == 0xDF
    assert encode_pmp_pma_addr(0x80001000, cfg, size=0x1000) == 0x200005FF
    assert csr_addresses_for_entry("pmp", 9) == (PMP_CFG_BASE + 2, PMP_ADDR_BASE + 9, 1)
    assert csr_addresses_for_entry("pma", 9) == (PMA_CFG_BASE + 2, PMA_ADDR_BASE + 9, 1)


@pytest.mark.parametrize(
    "config,addr,size",
    [
        (PmpPmaConfig(match="napot", write=True), 0x80000000, 0x1000),
        (PmpPmaConfig(match="napot"), 0x80000000, 0x1800),
        (PmpPmaConfig(match="napot"), 0x80000004, 0x1000),
        (PmpPmaConfig(match="napot"), 0x80000000, 0x800),
        (PmpPmaConfig(match="na4"), 0x80000000, None),
        (PmpPmaConfig(match="tor"), 0x80000000, 0x1000),
    ],
)
def test_pmp_pma_encoding_rejects_invalid_inputs(config, addr, size) -> None:
    if config.write and not config.read:
        with pytest.raises(ValueError, match="write"):
            encode_pmp_pma_cfg(config)
    else:
        with pytest.raises(ValueError):
            encode_pmp_pma_addr(addr, config, size=size)


def test_write_distributed_csr_completes_transaction_and_logs_event() -> None:
    env, events = _env_with_events()

    record = env.write_distributed_csr(0x3A0, 0x1F, settle_cycles=2)

    assert record == {"cycle": 0, "addr": 0x3A0, "data": 0x1F, "settle_cycles": 2}
    assert env.current_cycle == 3
    assert env.dut.io_csrCtrl_distribute_csr_w_valid.value == 0
    assert env.dut.io_csrCtrl_distribute_csr_w_bits_addr.value == 0x3A0
    assert env.dut.io_csrCtrl_distribute_csr_w_bits_data.value == 0x1F
    assert events[-1]["type"] == "control.distributed_csr_write"


def test_pmp_pma_entry_writes_preserve_other_cfg_bytes_in_mirror() -> None:
    env, _events = _env_with_events()

    first = env.write_pmp_entry(0, PmpPmaConfig(match="napot", read=True, execute=True), 0x80000000, size=0x1000)
    second = env.write_pmp_entry(1, PmpPmaConfig(match="napot", read=True, write=True, execute=True), 0x80002000, size=0x1000)

    assert first["cfg_csr"] == PMP_CFG_BASE
    assert first["addr_csr"] == PMP_ADDR_BASE
    assert second["cfg_csr"] == PMP_CFG_BASE
    assert second["addr_csr"] == PMP_ADDR_BASE + 1
    assert env.csr_write_log[-2]["data"] == 0x1D | (0x1F << 8)
    assert env._pmp_pma_cfg_words["pmp"][PMP_CFG_BASE] == 0x1D | (0x1F << 8)


def test_pulse_sfence_drives_all_fields_for_exactly_requested_cycles() -> None:
    env, events = _env_with_events()

    record = env.pulse_sfence(addr=0x80001234, rs1=1, rs2=1, ident=7, hv=1, hg=1, cycles=2)

    assert record == {"addr": 0x80001234, "rs1": 1, "rs2": 1, "id": 7, "hv": 1, "hg": 1, "cycles": 2, "cycle": 0}
    assert env.current_cycle == 2
    assert env.dut.io_sfence_valid.value == 0
    assert env.dut.io_sfence_bits_addr.value == 0x80001234
    assert env.dut.io_sfence_bits_id.value == 7
    assert events[-1]["type"] == "control.sfence"


def test_update_translation_context_updates_values_and_pulses_affected_groups() -> None:
    env, events = _env_with_events()

    record = env.update_translation_context(satp_mode=8, satp_asid=3, satp_ppn=0x81000, priv_imode=1, priv_virt=1, cycles=2)

    assert record == {"changed": {"satp": True, "vsatp": False, "hgatp": False, "priv_virt": True}, "cycles": 2}
    assert env.current_cycle == 2
    assert env.dut.io_tlbCsr_satp_mode.value == 8
    assert env.dut.io_tlbCsr_satp_asid.value == 3
    assert env.dut.io_tlbCsr_satp_ppn.value == 0x81000
    assert env.dut.io_tlbCsr_priv_imode.value == 1
    assert env.dut.io_tlbCsr_priv_virt.value == 1
    assert env.dut.io_tlbCsr_satp_changed.value == 0
    assert env.dut.io_tlbCsr_priv_virt_changed.value == 0
    assert events[-1]["type"] == "control.translation_context"
