from __future__ import annotations

import pytest

from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend
from env.support import fold_pc


def test_frontend_monitor_snapshot_is_shared_before_backend_drive(monkeypatch) -> None:
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False)
    pc = 0x8000_1000
    dut.io_backend_fromFtq_wen.value = 1
    dut.io_backend_fromFtq_ftqIdx.value = 3
    dut.io_backend_fromFtq_startPc_addr.value = pc >> 1
    dut.io_backend_cfVec_0_valid.value = 1
    dut.io_backend_cfVec_0_bits_foldpc.value = fold_pc(pc)
    dut.io_backend_cfVec_0_bits_instr.value = 0x13
    dut.io_backend_cfVec_0_bits_ftqPtr_value.value = 3
    dut.io_backend_cfVec_0_bits_ftqOffset.value = 1
    dut.io_backend_cfVec_0_bits_crossPageIPFFix.value = 1
    dut.io_backend_cfVec_0_bits_exceptionVec_1.value = 1
    dut.io_ptw_req_0_valid.value = 1
    dut.io_ptw_req_0_ready.value = 1
    dut.io_ptw_req_0_bits_vpn.value = 0x80001
    dut.io_ptw_resp_valid.value = 1
    dut.io_ptw_resp_ready.value = 1
    dut.io_ptw_resp_bits_s1_entry_ppn.value = 0x123
    env.ptw_agent.last_drive_expectation = {
        "vpn": 0x80001, "s2xlate": 0, "get_gpa": 0,
        "resp": {"s1_entry_ppn": 0x123},
    }

    snapshot = env.monitor.capture_cycle_observation(7)
    env._begin_backend_cycle(7)
    assert env.backend_model.current_frontend_observation() is snapshot.backend
    assert env.backend_model._cfvec_cycle_snapshot is snapshot.cfvec

    dut.io_backend_fromFtq_wen.value = 0
    dut.io_backend_cfVec_0_valid.value = 0
    dut.io_ptw_req_0_valid.value = 0
    dut.io_ptw_resp_valid.value = 0
    dut.io_ptw_resp_bits_s1_entry_ppn.value = 0

    env.ptw_resp_input_checker.on_clock_edge(7)
    assert env.ptw_resp_input_checker.get_stats()["checked_count"] == 1
    assert env.ptw_resp_input_checker.get_stats()["error_count"] == 0

    observed = []
    oracle = env.translation_oracle
    oracle.active = {"fetch_observation_ready": True}
    monkeypatch.setattr(oracle, "observe_ptw_request", lambda *_args, **kwargs: observed.append(("req", kwargs)))
    monkeypatch.setattr(oracle, "observe_ptw_response", lambda *_args, **kwargs: observed.append(("resp", kwargs)))
    monkeypatch.setattr(oracle, "observe_cfvec", lambda *_args, **kwargs: observed.append(("cfvec", kwargs)))
    oracle.on_clock_edge(7)
    assert observed[0] == ("req", {"vpn": 0x80001, "s2xlate": 0, "get_gpa": 0})
    assert observed[1][0] == "resp" and observed[1][1]["response"]["s1_entry_ppn"] == 0x123
    assert observed[2][0] == "cfvec"
    assert observed[2][1]["pc"] == pc
    assert observed[2][1]["cross_page"] is True
    assert observed[2][1]["exception_bits"][1] == 1

    env.flow_checker.on_clock_edge(7)
    assert env.monitor.observations[0].pc == pc
    assert env.monitor.exception_mark_count == 1

    next_snapshot = env.monitor.capture_cycle_observation(8)
    assert next_snapshot.backend.from_ftq_wen == 0
    assert not next_snapshot.cfvec.slots[0].valid
    assert next_snapshot.ptw["resp_valid"] == 0


def test_frontend_monitor_redirect_uses_captured_pre_backend_value() -> None:
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False)
    dut.io_backend_toFtq_redirect_valid.value = 1
    dut.io_backend_toFtq_redirect_bits_pc.value = 0x8000_1000
    dut.io_backend_toFtq_redirect_bits_target.value = 0x1000
    dut.io_backend_toFtq_redirect_bits_taken.value = 1

    env.monitor.capture_cycle_observation(10)
    assert env.flow_checker.last_dut_redirect is None
    assert env.monitor.observations == []
    env._begin_backend_cycle(10)
    dut.io_backend_toFtq_redirect_valid.value = 0
    dut.io_backend_toFtq_redirect_bits_target.value = 0x2000
    env.flow_checker.on_clock_edge(10)

    assert env.monitor.last_dut_redirect["target_pc"] == 0x1000
    assert env.monitor._recovery_target_pc == 0x1000
    assert env.monitor._skip_cfvec_until_cycle == 11


def test_icache_agent_uses_monitored_request_snapshot() -> None:
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False)
    pc = 0x8000_1000
    env.memory.load_bin(b"\x13\x00\x00\x00" * 16, pc)
    dut.auto_inner_icache_client_out_a_valid.value = 1
    dut.auto_inner_icache_client_out_a_bits_source.value = 2
    dut.auto_inner_icache_client_out_a_bits_address.value = pc

    request = env.monitor.capture_icache_request(7)
    dut.auto_inner_icache_client_out_a_valid.value = 0
    dut.auto_inner_icache_client_out_a_bits_address.value = pc + 64
    env.icache_agent.on_clock_edge(7, request)

    assert env.icache_agent.request_records[0]["address"] == pc
    assert env.icache_agent.request_records[0]["source"] == 2
    with pytest.raises(AssertionError, match="stale request snapshot"):
        env.icache_agent.on_clock_edge(8, request)
