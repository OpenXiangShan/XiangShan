"""Program placement for directed branch tests."""

import random

from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from env.support import PcBoundary, PcMapping, RandomPc, random_pc, scenario_rng
from tests.py.support.instructions import encode_jal_x0


def initialize_branch_entry(env, scenario_key: str) -> int:
    region = (0x8000_0000, 0x8010_0000)
    entry = random_pc(
        scenario_rng(f"{scenario_key}/pc")[2],
        va_range=region, pa_range=region, offsets=(0,), same_address=True,
    )
    initialize_frontend(env, FrontendInitConfig(reset_vector=entry.pc))
    return entry.pc


def jal_forward_program(rng: random.Random) -> tuple[RandomPc, bytes, int]:
    page_size = 0x1000
    image_size = 64 * 4
    region_size = 32 * page_size
    entries = tuple(
        offset for offset in range(0, region_size, 64)
        if offset % page_size + image_size <= page_size
    )
    placement = random_pc(
        rng,
        mappings=(PcMapping(0x80000000, 0x80000000, region_size),),
        entry_offsets=entries,
        boundaries=(PcBoundary(64, relation="start"),),
        same_address=True,
        instruction_bytes=4,
    )
    target_pc = placement.pc + 4 * rng.randint(2, 16)
    program = [0x00000013] * 64
    program[0] = encode_jal_x0(target_pc - placement.pc)
    payload = b"".join(instr.to_bytes(4, "little") for instr in program)
    return placement, payload, target_pc
