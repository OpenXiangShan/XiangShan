"""Cacheable startup, MainPipe observations and dual-fetch transaction evidence."""

from __future__ import annotations

from collections.abc import Callable, Sequence

from env.core.transactions import CommitTarget
from env.runtime.frontend_init import (
    LoadProgram,
    ProgramImage,
    initialize_frontend,
    run_initialization_steps,
)
from env.sequences import RunUntilCommitSequence

from .init_plans import make_cacheable_init_config

from .programs import (
    BPU_TRAINING_BASE,
    CACHEABLE_BASE as CACHEABLE_BASE,
    DUAL_FETCH_BASE as DUAL_FETCH_BASE,
    build_dual_fetch_program,
    build_nop_program,
)


def _aliases(name: str) -> tuple[str, str]:
    return (name, f"TOP.{name}")


_SIGNALS = {
    "last_fire": _aliases("Frontend_top.Frontend.inner_icache.missUnit.lastFire"),
    "last_fire_next": _aliases(
        "Frontend_top.Frontend.inner_icache.missUnit.lastFireNext"
    ),
    "miss_resp_valid": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.io_missResp_valid"
    ),
    "miss_resp_corrupt": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.__Vtogcov__io_missResp_bits_corrupt"
    ),
    "miss_resp_denied": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.__Vtogcov__io_missResp_bits_denied"
    ),
    "s1_valid": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_valid"),
    "s1_req1_valid": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_valid"
    ),
    "s1_req0_addr": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_vAddr_0_addr"
    ),
    "s1_req1_addr": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_vAddr_0_addr"
    ),
    "s1_ftq0_flag": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_ftqIdx_flag"
    ),
    "s1_ftq0_value": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_ftqIdx_value"
    ),
    "s1_ftq1_flag": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_ftqIdx_flag"
    ),
    "s1_ftq1_value": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_ftqIdx_value"
    ),
    "s1_exception": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_exception_value"
    ),
    "s1_mmio": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_isMmio"),
    "backend_redirect": _aliases(
        "Frontend_top.Frontend.inner_ftq.backendRedirect_valid"
    ),
    "s1_fire": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_fire"),
    "s1_flush": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_flush"),
    "to_ifu_valid": (
        "Frontend_top.Frontend._inner_icache_io_toIfu_req_valid",
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_valid",
        "TOP.Frontend_top.Frontend._inner_icache_io_toIfu_req_valid",
    ),
    "to_ifu_exception": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_exceptionOut_value"
    ),
}


for _index in range(4):
    _SIGNALS[f"should_fetch_{_index}"] = _aliases(
        f"Frontend_top.Frontend.inner_icache.mainPipe.s1_shouldFetch_{_index}"
    )
    _SIGNALS[f"mshr_valid_{_index}"] = _aliases(
        f"Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValid_{_index // 2}_{_index % 2}"
    )
    _SIGNALS[f"mshr_valid_reg_{_index}"] = _aliases(
        f"Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValidReg_{_index // 2}_{_index % 2}"
    )


def _try_read(env, names: Sequence[str]) -> int | None:
    for name in names:
        try:
            signal = getattr(env.dut, str(name), None)
            if signal is None:
                getter = getattr(env.dut, "GetInternalSignal", None)
                signal = getter(str(name)) if callable(getter) else None
            value = None if signal is None else getattr(signal, "value", None)
            if value is not None:
                return int(value)
        except Exception:
            continue
    return None


def _require_read(env, names: Sequence[str]) -> int:
    value = _try_read(env, names)
    assert value is not None, {"missing_internal_signals": list(names)}
    return int(value)


def _sample_mainpipe(env, cycle: int) -> dict:
    return {
        "cycle": int(cycle),
        **{name: _require_read(env, aliases) for name, aliases in _SIGNALS.items()},
    }


def register_mainpipe_observer(env) -> list[dict]:
    samples: list[dict] = []

    def observe(cycle, active_env) -> None:
        samples.append(_sample_mainpipe(active_env, cycle))

    env.register_cycle_observer(observe)
    return samples


def _dual_tag(sample: dict) -> tuple[tuple[int, int], tuple[int, int]]:
    return (
        (int(sample["s1_ftq0_flag"]), int(sample["s1_ftq0_value"])),
        (int(sample["s1_ftq1_flag"]), int(sample["s1_ftq1_value"])),
    )


def completed_dual_transactions(samples: Sequence[dict]) -> list[dict]:
    completed: list[dict] = []
    active: dict | None = None
    last_started_tag = None
    for sample in samples:
        if (
            sample["s1_valid"] == 1
            and sample["s1_req1_valid"] == 1
            and (sample["s1_exception"] == 0)
            and (sample["s1_mmio"] == 0)
            and (sample["s1_flush"] == 0)
        ):
            tag = _dual_tag(sample)
            if tag != last_started_tag:
                misses = [bool(sample[f"should_fetch_{index}"]) for index in range(4)]
                req0_miss = bool(misses[0] or misses[1])
                req1_miss = bool(misses[2] or misses[3])
                active = {
                    "tag": tag,
                    "start_cycle": int(sample["cycle"]),
                    "req0_addr": int(sample["s1_req0_addr"]) << 1,
                    "req1_addr": int(sample["s1_req1_addr"]) << 1,
                    "pattern": {
                        (False, False): "hit_hit",
                        (False, True): "hit_miss",
                        (True, False): "miss_hit",
                        (True, True): "miss_miss",
                    }[req0_miss, req1_miss],
                    "required": misses,
                    "raw_cycles": [None] * 4,
                    "registered_cycles": [None] * 4,
                }
                last_started_tag = tag
        if active is None:
            continue
        for index, required in enumerate(active["required"]):
            if not required:
                continue
            if (
                sample[f"mshr_valid_{index}"] == 1
                and active["raw_cycles"][index] is None
            ):
                active["raw_cycles"][index] = int(sample["cycle"])
            if (
                sample[f"mshr_valid_reg_{index}"] == 1
                and active["registered_cycles"][index] is None
            ):
                active["registered_cycles"][index] = int(sample["cycle"])
        if (
            sample["s1_valid"] == 1
            and sample["s1_req1_valid"] == 1
            and (_dual_tag(sample) == active["tag"])
            and (sample["s1_fire"] == 1)
        ):
            active["fire_cycle"] = int(sample["cycle"])
            active["registered_exact"] = all(
                (
                    not required
                    or (
                        active["raw_cycles"][index] is not None
                        and active["registered_cycles"][index]
                        == active["raw_cycles"][index] + 1
                    )
                    for index, required in enumerate(active["required"])
                )
            )
            completed.append(active)
            active = None
    return completed


def run_until(env, predicate: Callable[[], bool], *, max_cycles: int) -> bool:
    for _ in range(int(max_cycles)):
        if predicate():
            return True
        env.step(1)
    return bool(predicate())


def load_nops(env, base: int, *, words: int = 256) -> None:
    run_initialization_steps(
        env, (LoadProgram(ProgramImage(build_nop_program(words), int(base))),)
    )


def load_nop_program(env, words: int = 256, base_addr: int = BPU_TRAINING_BASE) -> None:
    # This loader historically advances one cycle; cacheable startup does not.
    run_initialization_steps(
        env,
        (
            LoadProgram(
                ProgramImage(build_nop_program(words), base_addr), step_cycles=1
            ),
        ),
    )


def load_two_fetch_loop(env, base: int) -> None:
    run_initialization_steps(
        env, (LoadProgram(ProgramImage(build_dual_fetch_program(), int(base))),)
    )


def warmup_commits(env, target_count: int = 4, max_cycles: int = 3000) -> int:
    return RunUntilCommitSequence(
        target=CommitTarget(target_count=int(target_count), max_cycles=int(max_cycles)),
    ).run(env)


def initialize_cacheable_stream(
    env, base: int, *, latency: int, samples: list[dict] | None = None
) -> list[dict]:
    """Compatibility entry; keep observer registration explicit in new tests."""
    observed = samples if samples is not None else register_mainpipe_observer(env)
    initialize_frontend(
        env, make_cacheable_init_config(env.config, base, latency=latency)
    )
    return observed
