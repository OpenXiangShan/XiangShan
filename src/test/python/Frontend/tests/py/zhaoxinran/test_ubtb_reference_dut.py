"""Prove the shared uBTB checker compares a real Frontend DUT prediction."""

import os

import pytest


_BASE = 0x8000_0000


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires compiled DUT")
def test_ubtb_predicts_trained_self_jump(env):
    # JAL x0, 0 repeatedly fetches one PC and trains a taken uBTB entry.
    env.load_program((0x0000006F).to_bytes(4, "little") +
                     (0x00000013).to_bytes(4, "little") * 1023, _BASE)
    assert env.ubtb_checker is not None
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    for _ in range(2000):
        env.step(1)
        if env.ubtb_checker.checked_count:
            break
    assert env.ubtb_checker.checked_count > 0, "shared uBTB checker made no comparison"
    assert not env.get_errors()
