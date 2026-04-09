from __future__ import annotations

try:
    import toffee.funcov as fc
except Exception:  # pragma: no cover
    fc = None


def _read_signal_value(dut, name: str, default: int = 0) -> int:
    signal = getattr(dut, str(name), None)
    if signal is None:
        return int(default)
    value = getattr(signal, "value", None)
    if value is None:
        return int(default)
    return int(value)


def _safe_add_watch_point(group, dut, bins, name):
    try:
        group.add_watch_point(dut, bins, name=name)
    except Exception:
        pass


def get_coverage_groups(dut):
    if fc is None or dut is None:
        return []

    fg_fetch = fc.CovGroup("FG-FETCH")
    fg_branch = fc.CovGroup("FG-BRANCH")
    fg_redirect = fc.CovGroup("FG-REDIRECT")
    fg_exception = fc.CovGroup("FG-EXCEPTION")
    fg_perf = fc.CovGroup("FG-PERFORMANCE")

    _safe_add_watch_point(
        fg_fetch,
        dut,
        {
            "CK-WIDTH-1P": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_{i}_valid").value) for i in range(8)
            ) >= 1,
            "CK-WIDTH-4P": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_{i}_valid").value) for i in range(8)
            ) >= 4,
            "CK-RVC": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_{i}_bits_isRvc").value)
                for i in range(8)
                if int(getattr(x, f"io_backend_cfVec_{i}_valid").value) == 1
            ) > 0,
        },
        name="FC-FETCH",
    )

    _safe_add_watch_point(
        fg_branch,
        dut,
        {
            "CK-PRED-TAKEN": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_{i}_bits_predTaken").value)
                for i in range(8)
                if int(getattr(x, f"io_backend_cfVec_{i}_valid").value) == 1
            ) > 0,
            "CK-FIXED-TAKEN": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_{i}_bits_fixedTaken").value)
                for i in range(8)
                if int(getattr(x, f"io_backend_cfVec_{i}_valid").value) == 1
            ) > 0,
        },
        name="FC-BRANCH",
    )

    _safe_add_watch_point(
        fg_redirect,
        dut,
        {
            "CK-REDIRECT": lambda x: int(x.io_backend_toFtq_redirect_valid.value) == 1,
        },
        name="FC-REDIRECT",
    )

    _safe_add_watch_point(
        fg_exception,
        dut,
        {
            "CK-EXC": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_0_bits_exceptionVec_{k}").value)
                for k in (1, 2, 12, 19, 20)
            ) > 0,
        },
        name="FC-EXCEPTION",
    )

    _safe_add_watch_point(
        fg_perf,
        dut,
        {
            "CK-ICACHE-FIRE": lambda x: _read_signal_value(x, "auto_inner_icache_client_out_a_valid") == 1
            and _read_signal_value(x, "auto_inner_icache_client_out_a_ready") == 1,
            "CK-IBUF-FULL": lambda x: _read_signal_value(x, "io_frontendInfo_ibufFull") == 1,
        },
        name="FC-PERFORMANCE",
    )

    return [fg_fetch, fg_branch, fg_redirect, fg_exception, fg_perf]
