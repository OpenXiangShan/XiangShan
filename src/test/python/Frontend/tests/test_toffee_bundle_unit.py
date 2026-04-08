from env.bundles import (
    BackendFromFtqBundle,
    BackendCtrlBundle,
    BackendObserveBundle,
    CSRControlBundle,
    ClockResetBundle,
    FrontendInfoBundle,
    ICacheBundle,
    PTWBundle,
    UncacheBundle,
)


def test_bundle_modules_export_phase1_bundle_types():
    assert ClockResetBundle.__name__ == "ClockResetBundle"
    assert ICacheBundle.__name__ == "ICacheBundle"
    assert UncacheBundle.__name__ == "UncacheBundle"
    assert PTWBundle.__name__ == "PTWBundle"
    assert BackendCtrlBundle.__name__ == "BackendCtrlBundle"
    assert BackendObserveBundle.__name__ == "BackendObserveBundle"
    assert BackendFromFtqBundle.__name__ == "BackendFromFtqBundle"
    assert FrontendInfoBundle.__name__ == "FrontendInfoBundle"
    assert CSRControlBundle.__name__ == "CSRControlBundle"


def test_backend_observe_bundle_covers_current_monitor_signals():
    required = {
        "cfvec_valid",
        "cfvec_pc",
        "cfvec_instr",
        "cfvec_is_rvc",
        "cfvec_pred_taken",
        "cfvec_exception_vec",
        "redirect_valid",
        "redirect_bits_pc",
        "redirect_bits_target",
        "redirect_bits_taken",
    }
    assert required.issubset(set(dir(BackendObserveBundle())))


def test_backend_from_ftq_and_frontend_info_bundles_cover_backend_model_reads():
    assert hasattr(BackendFromFtqBundle(), "io_backend_fromFtq_wen")
    assert hasattr(BackendFromFtqBundle(), "io_backend_fromFtq_ftqIdx")
    assert hasattr(BackendFromFtqBundle(), "io_backend_fromFtq_startPc_addr")
    assert hasattr(FrontendInfoBundle(), "io_frontendInfo_ibufFull")
