"""Instruction placement for cacheable exception-path tests."""

import random
from typing import Literal

from env.support import PcBoundary, RandomPc, random_pc


def exception_entry(rng: random.Random, kind: Literal["cross64", "illegal_rvc", "trigger"]) -> RandomPc:
    offset = 0 if kind == "trigger" else 0x3E
    return random_pc(
        rng,
        va_range=(0x80070000, 0x800A0000),
        pa_range=(0x80070000, 0x800A0000),
        offsets=(offset,),
        boundaries=(PcBoundary(64, width=4, relation="cross"),) if kind == "cross64" else (),
        same_address=True,
        instruction_bytes=2 if kind == "illegal_rvc" else 4,
    )
