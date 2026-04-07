from dataclasses import dataclass

from env.transactions import CommitTarget, PcSequenceExpectation, ProgramImage, RedirectTxn
from env.sequences import (
    CheckPcSequence,
    InjectRedirectSequence,
    LoadProgramSequence,
    RunUntilCommitSequence,
)


class _FakeProgramEnv:
    def __init__(self):
        self.calls = []

    def load_program(self, payload, base_addr):
        self.calls.append((bytes(payload), int(base_addr)))


class _FakeBackendModel:
    def __init__(self, commit_result=0):
        self.commit_result = int(commit_result)
        self.commit_calls = []
        self.redirect_calls = []

    def wait_for_commits(self, target_count, max_cycles):
        self.commit_calls.append((int(target_count), int(max_cycles)))
        return self.commit_result

    def inject_redirect(self, target_pc, reason, delay_cycles=0):
        self.redirect_calls.append((int(target_pc), str(reason), int(delay_cycles)))


class _FakeRedirectMonitor:
    def __init__(self, recent_pc_batches):
        self.recent_pc_batches = [list(batch) for batch in recent_pc_batches]
        self.calls = 0

    def recent_pcs(self, limit=16):
        idx = min(self.calls, len(self.recent_pc_batches) - 1)
        self.calls += 1
        return list(self.recent_pc_batches[idx])


class _FakeRedirectEnv:
    def __init__(self, recent_pc_batches, commit_result=0):
        self.backend_model = _FakeBackendModel(commit_result=commit_result)
        self.monitor = _FakeRedirectMonitor(recent_pc_batches)
        self.steps = []

    def step(self, cycles=1):
        self.steps.append(int(cycles))


@dataclass
class _Observation:
    pc: int


class _FakeCheckPcEnv:
    def __init__(self, batches):
        self.monitor = type("Monitor", (), {"observations": []})()
        self._batches = [[_Observation(pc) for pc in batch] for batch in batches]
        self.steps = 0

    def step(self, cycles=1):
        self.steps += int(cycles)
        if self._batches:
            self.monitor.observations.extend(self._batches.pop(0))


def test_load_program_sequence_delegates_to_env_load_program():
    env = _FakeProgramEnv()
    sequence = LoadProgramSequence(ProgramImage(payload=b"\x13\x00\x00\x00", base_addr=0x80000000))

    written = sequence.run(env)

    assert written == 4
    assert env.calls == [(b"\x13\x00\x00\x00", 0x80000000)]


def test_run_until_commit_sequence_delegates_to_backend_model():
    env = _FakeRedirectEnv(recent_pc_batches=[[]], commit_result=3)
    sequence = RunUntilCommitSequence(CommitTarget(target_count=4, max_cycles=12))

    got = sequence.run(env)

    assert got == 3
    assert env.backend_model.commit_calls == [(4, 12)]


def test_inject_redirect_sequence_waits_until_monitor_reports_target_pc():
    env = _FakeRedirectEnv(recent_pc_batches=[[], [0x80000010], [0x80000020]])
    sequence = InjectRedirectSequence(
        RedirectTxn(target_pc=0x80000020, reason="unit-test", max_cycles=3),
        redirect_delay_cycles=7,
    )

    ok = sequence.run(env)

    assert ok is True
    assert env.backend_model.redirect_calls == [(0x80000020, "unit-test", 7)]
    assert env.steps == [1, 1, 1]
    assert env.monitor.calls == 3


def test_inject_redirect_sequence_times_out_when_target_pc_never_appears():
    env = _FakeRedirectEnv(recent_pc_batches=[[], [], []])
    sequence = InjectRedirectSequence(
        RedirectTxn(target_pc=0x80000040, reason="timeout", max_cycles=3),
        redirect_delay_cycles=5,
    )

    ok = sequence.run(env)

    assert ok is False
    assert env.backend_model.redirect_calls == [(0x80000040, "timeout", 5)]
    assert env.steps == [1, 1, 1]


def test_check_pc_sequence_matches_expected_order_from_monitor_observations():
    env = _FakeCheckPcEnv(
        batches=[
            [0x80000000],
            [0x80000004, 0x80000008],
        ]
    )
    sequence = CheckPcSequence(PcSequenceExpectation(expected_pcs=(0x80000000, 0x80000008), max_cycles=4))

    ok = sequence.run(env)

    assert ok is True
    assert env.steps == 2
