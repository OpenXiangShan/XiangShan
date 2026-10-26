"""Shared MainPipe observations and cacheable stream setup."""

from __future__ import annotations

from .dut_support import read_dut_signal


_TO_IFU_VALID = (
    "Frontend_top.Frontend._inner_icache_io_toIfu_req_valid",
    "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_valid",
    "TOP.Frontend_top.Frontend._inner_icache_io_toIfu_req_valid",
)
_TO_IFU_EXCEPTION = (
    "Frontend_top.Frontend._inner_icache_io_toIfu_req_bits_info_0_icacheMeta_exception_value",
    "TOP.Frontend_top.Frontend._inner_icache_io_toIfu_req_bits_info_0_icacheMeta_exception_value",
)


def register_mainpipe_observer(env) -> list[dict]:
    samples: list[dict] = []

    def observe(cycle, active_env) -> None:
        samples.append(
            {
                "cycle": int(cycle),
                "to_ifu_valid": read_dut_signal(active_env, _TO_IFU_VALID, required=True),
                "to_ifu_exception": read_dut_signal(
                    active_env, _TO_IFU_EXCEPTION, required=True
                ),
            }
        )

    env.register_cycle_observer(observe)
    return samples


def initialize_cacheable_stream(
    env,
    base: int,
    *,
    latency: int,
    samples: list[dict] | None = None,
) -> list[dict]:
    env.load_program((0x0000_0013).to_bytes(4, "little") * 256, int(base))
    env.icache_agent.configure(
        hit_latency=int(latency),
        miss_latency=int(latency),
        miss_rate=1.0,
        seed=0x6219,
    )
    observed = samples if samples is not None else register_mainpipe_observer(env)
    env.initialize(reset_vector=int(base), bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(int(base))
    return observed
