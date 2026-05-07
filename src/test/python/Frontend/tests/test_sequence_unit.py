from __future__ import annotations

from types import SimpleNamespace

from env.monitor import Observation
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.sequences import CheckPcSequence, InjectRedirectSequence, LoadGoldenTraceSequence, RunUntilGoldenTraceCompleteSequence
from env.transactions import GoldenTraceSource, PcSequenceExpectation, RedirectTxn


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


class _LoadGoldenTraceEnv:
    def __init__(self) -> None:
        self.loaded = None

    def load_golden_trace_file(self, path: str, start_index: int = 0) -> int:
        self.loaded = (path, int(start_index))
        return 7


def test_golden_trace_reset_rejects_out_of_range_cursor() -> None:
    trace = GoldenTrace([TraceEntry(index=0, pc=0x80000000, instr=0, size=4)])

    trace.reset(1)
    assert trace.cursor == 1

    try:
        trace.reset(2)
        assert False, "expected ValueError"
    except ValueError:
        pass


class _StagnantGoldenTraceEnv:
    def __init__(self) -> None:
        self.dut = _NoRawDutAccess()
        self.monitor = SimpleNamespace(get_errors=lambda: [])
        self.backend_model = SimpleNamespace(
            golden_trace=SimpleNamespace(entries=[SimpleNamespace(pc=0x80000000)], cursor=0),
            has_pending_work=lambda: True,
            current_golden_pc=lambda: 0x80000000,
        )

    def step(self, cycles: int) -> int:
        assert int(cycles) == 1
        return 1


class _GoldenTraceWithPostTraceTailEnv:
    def __init__(self) -> None:
        self.dut = _NoRawDutAccess()
        self.monitor = SimpleNamespace(get_errors=lambda: [])
        self.backend_model = SimpleNamespace(
            golden_trace=SimpleNamespace(entries=[SimpleNamespace(pc=0x80000000)], cursor=0),
            has_pending_work=lambda: True,
            pending_work_count=lambda: 7,
            golden_completion_pending_work_count=lambda: 0,
            current_golden_pc=lambda: None,
        )

    def step(self, cycles: int) -> int:
        assert int(cycles) == 1
        self.backend_model.golden_trace.cursor = 1
        return 1


class _GoldenTraceTargetCursorEnv:
    def __init__(self) -> None:
        self.dut = _NoRawDutAccess()
        self.monitor = SimpleNamespace(get_errors=lambda: [])
        self.backend_model = SimpleNamespace(
            golden_trace=SimpleNamespace(entries=[SimpleNamespace(pc=0x80000000)] * 4, cursor=0),
            has_pending_work=lambda: False,
            current_golden_pc=lambda: 0x80000000,
        )
        self.steps = 0

    def step(self, cycles: int) -> int:
        assert int(cycles) == 1
        self.steps += 1
        if self.steps == 1:
            self.backend_model.golden_trace.cursor = 1
        elif self.steps == 2:
            self.backend_model.golden_trace.cursor = 2
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

    result = RunUntilGoldenTraceCompleteSequence(max_cycles=2).run(env)

    assert result.completed is True


def test_run_until_golden_trace_complete_stops_on_stagnant_cursor_limit():
    env = _StagnantGoldenTraceEnv()

    result = RunUntilGoldenTraceCompleteSequence(max_cycles=100, stagnant_cycles_limit=3).run(env)

    assert result.completed is False


def test_run_until_golden_trace_complete_ignores_post_trace_tail_work():
    env = _GoldenTraceWithPostTraceTailEnv()

    result = RunUntilGoldenTraceCompleteSequence(max_cycles=2).run(env)

    assert result.completed is True
    assert result.pending_work == 0


def test_run_until_golden_trace_complete_passes_at_target_cursor() -> None:
    env = _GoldenTraceTargetCursorEnv()

    result = RunUntilGoldenTraceCompleteSequence(max_cycles=10, target_cursor=2).run(env)

    assert result.completed is True
    assert result.status == "cursor_target"
    assert result.cursor >= 2


def test_load_golden_trace_sequence_passes_start_index_through() -> None:
    env = _LoadGoldenTraceEnv()

    count = LoadGoldenTraceSequence(
        source=GoldenTraceSource(path="/tmp/trace.jsonl", start_index=123),
    ).run(env)

    assert count == 7
    assert env.loaded == ("/tmp/trace.jsonl", 123)
