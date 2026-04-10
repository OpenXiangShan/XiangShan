from __future__ import annotations

from types import SimpleNamespace

from env.monitor import Observation
from env.sequences import CheckPcSequence, InjectRedirectSequence, RunUntilGoldenTraceCompleteSequence
from env.transactions import PcSequenceExpectation, RedirectTxn


class _NoRawDutAccess:
    def __getattr__(self, name):
        raise AssertionError(f"raw DUT peek is not allowed: {name}")


class _CheckPcEnv:
    def __init__(self, pcs: list[int]) -> None:
        self._pcs = list(pcs)
        self.monitor = SimpleNamespace(observations=[])
        self.dut = _NoRawDutAccess()

    def step(self, cycles: int) -> int:
        assert int(cycles) == 1
        if self._pcs:
            pc = int(self._pcs.pop(0))
            self.monitor.observations.append(
                Observation(cycle=len(self.monitor.observations), slot=0, pc=pc, instr=0, is_rvc=False, pred_taken=False)
            )
        return 1


class _RedirectMonitor:
    def __init__(self) -> None:
        self._pcs: list[int] = []

    def recent_pcs(self, limit: int = 16) -> list[int]:
        return self._pcs[-int(limit) :]


class _InjectRedirectBackendModel:
    def __init__(self) -> None:
        self.target_pc: int | None = None
        self.delay_cycles = "unset"

    def inject_redirect(self, target_pc: int, reason: str, delay_cycles=None) -> None:
        self.target_pc = int(target_pc)
        self.delay_cycles = delay_cycles


class _InjectRedirectEnv:
    def __init__(self) -> None:
        self.monitor = _RedirectMonitor()
        self.backend_model = _InjectRedirectBackendModel()
        self.dut = _NoRawDutAccess()

    def step(self, cycles: int) -> int:
        assert int(cycles) == 1
        if self.backend_model.target_pc is not None:
            self.monitor._pcs.append(int(self.backend_model.target_pc))
        return 1


class _GoldenTraceEnv:
    def __init__(self) -> None:
        self.dut = _NoRawDutAccess()
        self.monitor = SimpleNamespace(get_errors=lambda: [])
        self.backend_model = SimpleNamespace(
            golden_trace=SimpleNamespace(entries=[SimpleNamespace(pc=0x80000000)], cursor=0),
            has_pending_work=lambda: False,
            current_golden_pc=lambda: 0x80000000,
        )

    def step(self, cycles: int) -> int:
        assert int(cycles) == 1
        self.backend_model.golden_trace.cursor = 1
        return 1


def test_check_pc_sequence_uses_monitor_observations_without_raw_dut_peeks():
    env = _CheckPcEnv([0x80000000, 0x80000004])

    matched = CheckPcSequence(
        expectation=PcSequenceExpectation(expected_pcs=(0x80000000, 0x80000004), max_cycles=4)
    ).run(env)

    assert matched is True


def test_inject_redirect_sequence_uses_monitor_recent_pcs_without_raw_dut_peeks():
    env = _InjectRedirectEnv()

    observed = InjectRedirectSequence(
        txn=RedirectTxn(target_pc=0x80000080, reason="unit-test", max_cycles=2),
    ).run(env)

    assert observed is True
    assert env.monitor.recent_pcs()[-1] == 0x80000080
    assert env.backend_model.delay_cycles is None


def test_run_until_golden_trace_complete_uses_monitor_and_backend_model_contracts_only():
    env = _GoldenTraceEnv()

    completed = RunUntilGoldenTraceCompleteSequence(max_cycles=2).run(env)

    assert completed is True
