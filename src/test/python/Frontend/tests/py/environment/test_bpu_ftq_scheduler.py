from types import SimpleNamespace

import pytest

from env.core.backend_model import BackendModel
from env.model.backend_state import QueueInstr, PATH_STATE_WRONG
from env.support.bpu_ftq_scheduler import BpuFtqScheduler


def backend():
    model = BackendModel()
    model._cfvec_queue.append(QueueInstr(cycle=10, slot=0, pc=0x80000004, instr=0x0040006F,
        is_rvc=False, pred_taken=False, ftq_flag=1, ftq_value=3, ftq_offset=3, is_last_in_entry=False))
    model._ftq_start_pc_cache[67] = 0x80000000
    model.current_cycle = 12
    return model


def test_live_identity_uses_flag_and_authoritative_start_pc():
    model = backend()
    identity = model.live_ftq_identities()[0]
    assert identity["start_pc"] == 0x80000000
    resolve = model.queue_directed_resolve(identity, target=0x80000008)
    assert (resolve.ftq_flag, resolve.ftq_value, resolve.pc, resolve.inst_pc) == (1, 3, 0x80000000, 0x80000004)
    assert resolve.ready_cycle == 12
    model._ftq_start_pc_cache.clear()
    assert model.live_ftq_identities() == ()


def test_stale_wrong_path_and_disabled_resolves_are_rejected():
    model = backend()
    identity = model.live_ftq_identities()[0]
    model._cfvec_queue[0].path_state = PATH_STATE_WRONG
    with pytest.raises(AssertionError, match="stale"):
        model.queue_directed_resolve(identity, target=0x80000008)
    model.set_explicit_injection_enabled(False)
    with pytest.raises(AssertionError, match="disabled"):
        model.queue_directed_resolve(identity, target=0x80000008)
    assert not model._pending_resolves


def test_scheduler_waits_for_valid_flush_and_exact_relation():
    samples = iter([{"bpu_valid": 0, "match": True}, {"bpu_valid": 1, "match": False},
                    {"bpu_valid": 1, "match": True}])
    steps = []
    env = SimpleNamespace(step=lambda n: steps.append(n))
    scheduler = BpuFtqScheduler(env)
    assert scheduler.wait_for_bpu_flush(lambda _: next(samples), lambda s: s["match"], max_cycles=2)["match"]
    assert steps == [1, 1]


def test_training_captures_identity_at_the_window_not_before():
    model = backend()
    env = SimpleNamespace(backend_model=model, current_cycle=0)
    def step(_):
        env.current_cycle += 1
        model._cfvec_queue[0].cycle += 1
    env.step = step
    scheduler = BpuFtqScheduler(env)
    resolve = scheduler.train_when(lambda e: e.current_cycle == 2, target=0x80000008, max_cycles=2)
    assert resolve.ftq_value == 3
    assert env.current_cycle == 2
