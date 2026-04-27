from __future__ import annotations

from toffee import Bundle, Signal, SignalList


class BackendObserveBundle(Bundle):
    SIGNAL_BINDINGS = {
        "redirect_valid": "io_backend_toFtq_redirect_valid",
        "redirect_bits_pc": "io_backend_toFtq_redirect_bits_pc",
        "redirect_bits_target": "io_backend_toFtq_redirect_bits_target",
        "redirect_bits_taken": "io_backend_toFtq_redirect_bits_taken",
    }

    cfvec_valid = SignalList("io_backend_cfVec_#_valid", 8)
    cfvec_pc = SignalList("io_backend_cfVec_#_bits_pc", 8)
    cfvec_instr = SignalList("io_backend_cfVec_#_bits_instr", 8)
    cfvec_is_rvc = SignalList("io_backend_cfVec_#_bits_isRvc", 8)
    cfvec_pred_taken = SignalList("io_backend_cfVec_#_bits_predTaken", 8)
    cfvec_fixed_taken = SignalList("io_backend_cfVec_#_bits_fixedTaken", 8)
    cfvec_ftq_ptr_flag = SignalList("io_backend_cfVec_#_bits_ftqPtr_flag", 8)
    cfvec_ftq_ptr_value = SignalList("io_backend_cfVec_#_bits_ftqPtr_value", 8)
    cfvec_ftq_offset = SignalList("io_backend_cfVec_#_bits_ftqOffset", 8)
    cfvec_is_last_in_ftq_entry = SignalList("io_backend_cfVec_#_bits_isLastInFtqEntry", 8)
    cfvec_exception_vec = [SignalList(f"io_backend_cfVec_{slot}_bits_exceptionVec_#", 24) for slot in range(8)]
    redirect_valid = Signal()
    redirect_bits_pc = Signal()
    redirect_bits_target = Signal()
    redirect_bits_taken = Signal()
