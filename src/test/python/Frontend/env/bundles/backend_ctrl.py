from __future__ import annotations

from toffee import Bundle, Signal, SignalList


class BackendCtrlBundle(Bundle):
    SIGNAL_BINDINGS = {
        "can_accept": "io_backend_canAccept",
        "commit_valid": "io_backend_toFtq_commit_valid",
        "commit_bits_flag": "io_backend_toFtq_commit_bits_flag",
        "commit_bits_value": "io_backend_toFtq_commit_bits_value",
        "redirect_valid": "io_backend_toFtq_redirect_valid",
        "redirect_bits_pc": "io_backend_toFtq_redirect_bits_pc",
        "redirect_bits_target": "io_backend_toFtq_redirect_bits_target",
        "redirect_bits_taken": "io_backend_toFtq_redirect_bits_taken",
        "redirect_bits_ftq_idx_flag": "io_backend_toFtq_redirect_bits_ftqIdx_flag",
        "redirect_bits_ftq_idx_value": "io_backend_toFtq_redirect_bits_ftqIdx_value",
        "redirect_bits_ftq_offset": "io_backend_toFtq_redirect_bits_ftqOffset",
        "redirect_bits_is_rvc": "io_backend_toFtq_redirect_bits_isRVC",
        "redirect_bits_attribute_branch_type": "io_backend_toFtq_redirect_bits_attribute_branchType",
        "redirect_bits_attribute_ras_action": "io_backend_toFtq_redirect_bits_attribute_rasAction",
        "redirect_bits_level": "io_backend_toFtq_redirect_bits_level",
        "redirect_bits_backend_igpf": "io_backend_toFtq_redirect_bits_backendIGPF",
        "redirect_bits_backend_ipf": "io_backend_toFtq_redirect_bits_backendIPF",
        "redirect_bits_backend_iaf": "io_backend_toFtq_redirect_bits_backendIAF",
        "redirect_bits_debug_is_ctrl": "io_backend_toFtq_redirect_bits_debugIsCtrl",
        "redirect_bits_debug_is_mem_vio": "io_backend_toFtq_redirect_bits_debugIsMemVio",
        "wfi_req": "io_backend_wfi_wfiReq",
        "ftq_idx_ahead_0_valid": "io_backend_toFtq_ftqIdxAhead_0_valid",
    }

    can_accept = Signal()
    commit_valid = Signal()
    commit_bits_flag = Signal()
    commit_bits_value = Signal()
    redirect_valid = Signal()
    redirect_bits_pc = Signal()
    redirect_bits_target = Signal()
    redirect_bits_taken = Signal()
    redirect_bits_ftq_idx_flag = Signal()
    redirect_bits_ftq_idx_value = Signal()
    redirect_bits_ftq_offset = Signal()
    redirect_bits_is_rvc = Signal()
    redirect_bits_attribute_branch_type = Signal()
    redirect_bits_attribute_ras_action = Signal()
    redirect_bits_level = Signal()
    redirect_bits_backend_igpf = Signal()
    redirect_bits_backend_ipf = Signal()
    redirect_bits_backend_iaf = Signal()
    redirect_bits_debug_is_ctrl = Signal()
    redirect_bits_debug_is_mem_vio = Signal()
    resolve_valid = SignalList("io_backend_toFtq_resolve_#_valid", 3)
    resolve_bits_ftq_idx_flag = SignalList("io_backend_toFtq_resolve_#_bits_ftqIdx_flag", 3)
    resolve_bits_ftq_idx_value = SignalList("io_backend_toFtq_resolve_#_bits_ftqIdx_value", 3)
    resolve_bits_ftq_offset = SignalList("io_backend_toFtq_resolve_#_bits_ftqOffset", 3)
    resolve_bits_pc_addr = SignalList("io_backend_toFtq_resolve_#_bits_pc_addr", 3)
    resolve_bits_target_addr = SignalList("io_backend_toFtq_resolve_#_bits_target_addr", 3)
    resolve_bits_taken = SignalList("io_backend_toFtq_resolve_#_bits_taken", 3)
    resolve_bits_mispredict = SignalList("io_backend_toFtq_resolve_#_bits_mispredict", 3)
    resolve_bits_attribute_branch_type = SignalList("io_backend_toFtq_resolve_#_bits_attribute_branchType", 3)
    resolve_bits_attribute_ras_action = SignalList("io_backend_toFtq_resolve_#_bits_attribute_rasAction", 3)
    call_ret_commit_valid = SignalList("io_backend_toFtq_callRetCommit_#_valid", 8)
    call_ret_commit_bits_ras_action = SignalList("io_backend_toFtq_callRetCommit_#_bits_rasAction", 8)
    call_ret_commit_bits_ftq_ptr_value = SignalList("io_backend_toFtq_callRetCommit_#_bits_ftqPtr_value", 8)
    wfi_req = Signal()
    ftq_idx_ahead_0_valid = Signal()

    def drive_idle(self) -> None:
        self.can_accept.value = 1
        self.commit_valid.value = 0
        self.redirect_valid.value = 0
        self.redirect_bits_pc.value = 0
        self.redirect_bits_target.value = 0
        self.redirect_bits_taken.value = 0
        self.redirect_bits_ftq_idx_flag.value = 0
        self.redirect_bits_ftq_idx_value.value = 0
        self.redirect_bits_ftq_offset.value = 0
        self.redirect_bits_is_rvc.value = 0
        self.redirect_bits_attribute_branch_type.value = 0
        self.redirect_bits_attribute_ras_action.value = 0
        self.redirect_bits_level.value = 0
        self.redirect_bits_backend_igpf.value = 0
        self.redirect_bits_backend_ipf.value = 0
        self.redirect_bits_backend_iaf.value = 0
        self.redirect_bits_debug_is_ctrl.value = 0
        self.redirect_bits_debug_is_mem_vio.value = 0
        for signal in self.resolve_valid:
            signal.value = 0
        for signal in self.call_ret_commit_valid:
            signal.value = 0
        for signal in self.call_ret_commit_bits_ras_action:
            signal.value = 0
        for signal in self.call_ret_commit_bits_ftq_ptr_value:
            signal.value = 0
        self.wfi_req.value = 0
        self.ftq_idx_ahead_0_valid.value = 0
