"""Main-fetch observations shared by redirect recovery scenarios."""

from tests.py.support.uncache_scenarios import _require_first_dut_signal


_MAIN_FETCH_VALID_SIGNALS = (
    "Frontend_top.Frontend.inner_icache.mainPipe.io_fromFtq_valid",
    "Frontend_top.Frontend._inner_ftq_io_toICache_toMainPipe_valid",
    "Frontend_top.Frontend.inner_ftq.io_toICache_toMainPipe_valid",
)
_MAIN_FETCH_READY_SIGNALS = (
    "Frontend_top.Frontend.inner_icache.mainPipe.io_fromFtq_ready",
    "Frontend_top.Frontend._inner_icache_io_fromFtq_toMainPipe_ready",
    "Frontend_top.Frontend.inner_icache.io_fromFtq_toMainPipe_ready",
)


def register_main_fetch_observer(env) -> list[dict[str, int]]:
    samples: list[dict[str, int]] = []

    def capture(cycle, current_env) -> None:
        samples.append(
            {
                "cycle": int(cycle),
                "redirect": int(current_env.backend_to_ftq_if.redirect_valid.value),
                "valid": _require_first_dut_signal(current_env, _MAIN_FETCH_VALID_SIGNALS),
                "ready": _require_first_dut_signal(current_env, _MAIN_FETCH_READY_SIGNALS),
            }
        )

    env.register_cycle_observer(capture)
    return samples


def wait_for_three_main_fetches_after_redirect(
    env, samples: list[dict[str, int]], *, max_cycles: int = 1024
) -> None:
    redirect_cycles = [sample["cycle"] for sample in samples if sample["redirect"] == 1]
    assert redirect_cycles, {"reason": "backend fault redirect was not observed"}
    redirect_cycle = max(redirect_cycles)
    for _ in range(int(max_cycles)):
        fires = [
            sample
            for sample in samples
            if sample["cycle"] > redirect_cycle
            and sample["valid"] == 1
            and sample["ready"] == 1
        ]
        if len(fires) >= 3:
            env.step(2)
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": "fewer than three main-fetch handshakes followed backend fault redirect",
            "redirect_cycle": redirect_cycle,
            "samples_tail": samples[-64:],
        }
    )
