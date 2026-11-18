"""Bare-mode program placement for directed VType tests."""

from env.support import random_pc, record_scenario, scenario_rng


def vtype_entry(env, scenario_id: str, *, ordinal: int = 0) -> int:
    scenario_key = f"zhaoxinran/vtype/entry/{scenario_id}"
    base_seed, seed, rng = scenario_rng(scenario_key, ordinal=ordinal)
    placement = random_pc(
        rng,
        va_range=(0x80000000, 0x80020000),
        pa_range=(0x80000000, 0x80020000),
        offsets=(0,),
        same_address=True,
        instruction_bytes=4,
    )
    record_scenario(
        env, scenario_key, base_seed=base_seed, seed=seed, ordinal=ordinal,
        parameters={"pc": placement.pc, "pa": placement.pa},
    )
    return placement.pc
