from env.pylib import frontend_offset_path


_IFU_PREV_HALF_RVI_SIGNALS = (
    (
        "Frontend_top.Frontend.inner_ifu.s0_prevEndIsHalfRvi",
        "TOP.Frontend_top.Frontend.inner_ifu.s0_prevEndIsHalfRvi",
    ),
    (
        "Frontend_top.Frontend.inner_ifu.s1_prevEndIsHalfRvi",
        "TOP.Frontend_top.Frontend.inner_ifu.s1_prevEndIsHalfRvi",
    ),
    (
        "Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviData",
        "TOP.Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviData",
    ),
    (
        "Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviPc_addr",
        "TOP.Frontend_top.Frontend.inner_ifu.s1_prevEndHalfRviPc_addr",
    ),
    (
        "Frontend_top.Frontend.inner_ifu.s2_prevEndIsHalfRvi",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_prevEndIsHalfRvi",
    ),
    (
        "Frontend_top.Frontend.inner_ifu.s2_valid_valid",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_valid_valid",
    ),
    (
        "Frontend_top.Frontend.inner_ifu.s2_prevEndHalfRviData",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_prevEndHalfRviData",
    ),
    (
        "Frontend_top.Frontend.inner_ifu.s2_prevEndHalfPc_addr",
        "TOP.Frontend_top.Frontend.inner_ifu.s2_prevEndHalfPc_addr",
    ),
)
_IFU_BACKEND_REDIRECT_SIGNALS = (
    "Frontend_top.Frontend.inner_ftq.backendRedirect_valid",
    "TOP.Frontend_top.Frontend.inner_ftq.backendRedirect_valid",
    "Frontend_top.io_backend_toFtq_redirect_valid",
    "io_backend_toFtq_redirect_valid",
)
_IFU_UNCACHE_NEED_RESEND_SIGNALS = (
    "Frontend_top.Frontend.inner_ifu.uncacheNeedResend",
    "TOP.Frontend_top.Frontend.inner_ifu.uncacheNeedResend",
)


def test_uncache_prev_half_signal_contract_matches_dut_inventory() -> None:
    """The cross-page observer must fail closed when any retimed state is absent."""
    offset = frontend_offset_path()
    assert offset.exists(), "DUT signal inventory is required before signal-contract tests"
    registered = {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }
    required = [
        *_IFU_PREV_HALF_RVI_SIGNALS,
        _IFU_BACKEND_REDIRECT_SIGNALS,
        _IFU_UNCACHE_NEED_RESEND_SIGNALS,
    ]
    missing = [list(names) for names in required if not any(name in registered for name in names)]
    assert not missing, {"missing_internal_signals": missing}
