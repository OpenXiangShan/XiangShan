from __future__ import annotations

from toffee import Bundle, Signal, SignalList


class BackendCfVecBundle(Bundle):
    OPTIONAL_ATTRS = ("cfvec_is_last_in_ftq_entry",)

    cfvec_valid = SignalList("io_backend_cfVec_#_valid", 8)
    cfvec_instr = SignalList("io_backend_cfVec_#_bits_instr", 8)
    cfvec_foldpc = SignalList("io_backend_cfVec_#_bits_foldpc", 8)
    cfvec_exception_vec_20 = SignalList("io_backend_cfVec_#_bits_exceptionVec_20", 8)
    cfvec_exception_vec_19 = SignalList("io_backend_cfVec_#_bits_exceptionVec_19", 8)
    cfvec_exception_vec_12 = SignalList("io_backend_cfVec_#_bits_exceptionVec_12", 8)
    cfvec_exception_vec_2 = SignalList("io_backend_cfVec_#_bits_exceptionVec_2", 8)
    cfvec_exception_vec_1 = SignalList("io_backend_cfVec_#_bits_exceptionVec_1", 8)
    cfvec_satp_flush_first_fetch_fault = SignalList(
        "io_backend_cfVec_#_bits_satpFlushFirstFetchFault", 8
    )
    cfvec_backend_exception = SignalList("io_backend_cfVec_#_bits_backendException", 8)
    cfvec_trigger = SignalList("io_backend_cfVec_#_bits_trigger", 8)
    cfvec_is_rvc = SignalList("io_backend_cfVec_#_bits_isRvc", 8)
    cfvec_pred_taken = SignalList("io_backend_cfVec_#_bits_predTaken", 8)
    cfvec_cross_page_ipf_fix = SignalList("io_backend_cfVec_#_bits_crossPageIPFFix", 8)
    cfvec_ftq_ptr_flag = SignalList("io_backend_cfVec_#_bits_ftqPtr_flag", 8)
    cfvec_ftq_ptr_value = SignalList("io_backend_cfVec_#_bits_ftqPtr_value", 8)
    cfvec_ftq_offset = SignalList("io_backend_cfVec_#_bits_ftqOffset", 8)
    cfvec_vtype_illegal = SignalList("io_backend_cfVec_#_bits_vtype_illegal", 8)
    cfvec_vtype_vma = SignalList("io_backend_cfVec_#_bits_vtype_vma", 8)
    cfvec_vtype_vta = SignalList("io_backend_cfVec_#_bits_vtype_vta", 8)
    cfvec_vtype_vsew = SignalList("io_backend_cfVec_#_bits_vtype_vsew", 8)
    cfvec_vtype_vlmul = SignalList("io_backend_cfVec_#_bits_vtype_vlmul", 8)
    cfvec_specvtype_illegal = SignalList("io_backend_cfVec_#_bits_specvtype_illegal", 8)
    cfvec_specvtype_vma = SignalList("io_backend_cfVec_#_bits_specvtype_vma", 8)
    cfvec_specvtype_vta = SignalList("io_backend_cfVec_#_bits_specvtype_vta", 8)
    cfvec_specvtype_vsew = SignalList("io_backend_cfVec_#_bits_specvtype_vsew", 8)
    cfvec_specvtype_vlmul = SignalList("io_backend_cfVec_#_bits_specvtype_vlmul", 8)


class BackendStatusBundle(Bundle):
    SIGNAL_BINDINGS = {
        "backend_empty": "io_backend_backendEmpty",
    }

    backend_empty = Signal()
    stall_reason = SignalList("io_backend_stallReason_reason_#", 8)

    def drive_idle(self) -> None:
        self.backend_empty.value = 1


class BackendFromFtqBundle(Bundle):
    io_backend_fromFtq_wen = Signal()
    io_backend_fromFtq_ftqIdx = Signal()
    io_backend_fromFtq_startPc_addr = Signal()


class BackendFromIfuBundle(Bundle):
    SIGNAL_BINDINGS = {
        "gpaddr_mem_wen": "io_backend_fromIfu_gpAddrMem_wen",
        "gpaddr_mem_waddr": "io_backend_fromIfu_gpAddrMem_waddr",
        "gpaddr_mem_gpaddr": "io_backend_fromIfu_gpAddrMem_wdata_gpaddr",
        "gpaddr_mem_is_for_vs_nonleaf_pte": (
            "io_backend_fromIfu_gpAddrMem_wdata_isForVSnonLeafPTE"
        ),
    }

    gpaddr_mem_wen = Signal()
    gpaddr_mem_waddr = Signal()
    gpaddr_mem_gpaddr = Signal()
    gpaddr_mem_is_for_vs_nonleaf_pte = Signal()


class BackendToFtqBundle(Bundle):
    SIGNAL_BINDINGS = {
        "redirect_valid": "io_backend_toFtq_redirect_valid",
        "redirect_bits_ftq_idx_flag": "io_backend_toFtq_redirect_bits_ftqIdx_flag",
        "redirect_bits_ftq_idx_value": "io_backend_toFtq_redirect_bits_ftqIdx_value",
        "redirect_bits_pc": "io_backend_toFtq_redirect_bits_pc",
        "redirect_bits_taken": "io_backend_toFtq_redirect_bits_taken",
        "redirect_bits_ftq_offset": "io_backend_toFtq_redirect_bits_ftqOffset",
        "redirect_bits_is_rvc": "io_backend_toFtq_redirect_bits_isRVC",
        "redirect_bits_attribute_branch_type": "io_backend_toFtq_redirect_bits_attribute_branchType",
        "redirect_bits_attribute_ras_action": "io_backend_toFtq_redirect_bits_attribute_rasAction",
        "redirect_bits_target": "io_backend_toFtq_redirect_bits_target",
        "redirect_bits_level": "io_backend_toFtq_redirect_bits_level",
        "redirect_bits_backend_igpf": "io_backend_toFtq_redirect_bits_backendIGPF",
        "redirect_bits_backend_ipf": "io_backend_toFtq_redirect_bits_backendIPF",
        "redirect_bits_backend_iaf": "io_backend_toFtq_redirect_bits_backendIAF",
        "redirect_bits_satp_flush": "io_backend_toFtq_redirect_bits_satpFlush",
        "redirect_bits_debug_is_ctrl": "io_backend_toFtq_redirect_bits_debugIsCtrl",
        "redirect_bits_debug_is_mem_vio": "io_backend_toFtq_redirect_bits_debugIsMemVio",
        "ftq_idx_ahead_valid": "io_backend_toFtq_ftqIdxAhead_valid",
        "ftq_idx_ahead_flag": "io_backend_toFtq_ftqIdxAhead_bits_flag",
        "ftq_idx_ahead_value": "io_backend_toFtq_ftqIdxAhead_bits_value",
        "commit_valid": "io_backend_toFtq_commit_valid",
        "commit_bits_flag": "io_backend_toFtq_commit_bits_flag",
        "commit_bits_value": "io_backend_toFtq_commit_bits_value",
        "resolve_valid": "io_backend_toFtq_resolve_#_valid",
        "resolve_bits_ftq_idx_flag": "io_backend_toFtq_resolve_#_bits_ftqIdx_flag",
        "resolve_bits_ftq_idx_value": "io_backend_toFtq_resolve_#_bits_ftqIdx_value",
        "resolve_bits_ftq_offset": "io_backend_toFtq_resolve_#_bits_ftqOffset",
        "resolve_bits_pc_addr": "io_backend_toFtq_resolve_#_bits_pc_addr",
        "resolve_bits_target_addr": "io_backend_toFtq_resolve_#_bits_target_addr",
        "resolve_bits_taken": "io_backend_toFtq_resolve_#_bits_taken",
        "resolve_bits_mispredict": "io_backend_toFtq_resolve_#_bits_mispredict",
        "resolve_bits_attribute_branch_type": "io_backend_toFtq_resolve_#_bits_attribute_branchType",
        "resolve_bits_attribute_ras_action": "io_backend_toFtq_resolve_#_bits_attribute_rasAction",
        "call_ret_commit_valid": "io_backend_toFtq_callRetCommit_#_valid",
        "call_ret_commit_bits_ras_action": "io_backend_toFtq_callRetCommit_#_bits_rasAction",
        "call_ret_commit_bits_ftq_ptr_value": "io_backend_toFtq_callRetCommit_#_bits_ftqPtr_value",
    }

    redirect_valid = Signal()
    redirect_bits_ftq_idx_flag = Signal()
    redirect_bits_ftq_idx_value = Signal()
    redirect_bits_pc = Signal()
    redirect_bits_taken = Signal()
    redirect_bits_ftq_offset = Signal()
    redirect_bits_is_rvc = Signal()
    redirect_bits_attribute_branch_type = Signal()
    redirect_bits_attribute_ras_action = Signal()
    redirect_bits_target = Signal()
    redirect_bits_level = Signal()
    redirect_bits_backend_igpf = Signal()
    redirect_bits_backend_ipf = Signal()
    redirect_bits_backend_iaf = Signal()
    redirect_bits_satp_flush = Signal()
    redirect_bits_debug_is_ctrl = Signal()
    redirect_bits_debug_is_mem_vio = Signal()
    ftq_idx_ahead_valid = Signal()
    ftq_idx_ahead_flag = Signal()
    ftq_idx_ahead_value = Signal()
    commit_valid = Signal()
    commit_bits_flag = Signal()
    commit_bits_value = Signal()
    resolve_valid = SignalList("io_backend_toFtq_resolve_#_valid", 3)
    resolve_bits_ftq_idx_flag = SignalList("io_backend_toFtq_resolve_#_bits_ftqIdx_flag", 3)
    resolve_bits_ftq_idx_value = SignalList("io_backend_toFtq_resolve_#_bits_ftqIdx_value", 3)
    resolve_bits_ftq_offset = SignalList("io_backend_toFtq_resolve_#_bits_ftqOffset", 3)
    resolve_bits_pc_addr = SignalList("io_backend_toFtq_resolve_#_bits_pc_addr", 3)
    resolve_bits_target_addr = SignalList("io_backend_toFtq_resolve_#_bits_target_addr", 3)
    resolve_bits_taken = SignalList("io_backend_toFtq_resolve_#_bits_taken", 3)
    resolve_bits_mispredict = SignalList("io_backend_toFtq_resolve_#_bits_mispredict", 3)
    resolve_bits_attribute_branch_type = SignalList(
        "io_backend_toFtq_resolve_#_bits_attribute_branchType", 3
    )
    resolve_bits_attribute_ras_action = SignalList(
        "io_backend_toFtq_resolve_#_bits_attribute_rasAction", 3
    )
    call_ret_commit_valid = SignalList("io_backend_toFtq_callRetCommit_#_valid", 8)
    call_ret_commit_bits_ras_action = SignalList(
        "io_backend_toFtq_callRetCommit_#_bits_rasAction", 8
    )
    call_ret_commit_bits_ftq_ptr_value = SignalList(
        "io_backend_toFtq_callRetCommit_#_bits_ftqPtr_value", 8
    )

    def drive_idle(self) -> None:
        for signal in (
            self.redirect_valid,
            self.redirect_bits_ftq_idx_flag,
            self.redirect_bits_ftq_idx_value,
            self.redirect_bits_pc,
            self.redirect_bits_taken,
            self.redirect_bits_ftq_offset,
            self.redirect_bits_is_rvc,
            self.redirect_bits_attribute_branch_type,
            self.redirect_bits_attribute_ras_action,
            self.redirect_bits_target,
            self.redirect_bits_level,
            self.redirect_bits_backend_igpf,
            self.redirect_bits_backend_ipf,
            self.redirect_bits_backend_iaf,
            self.redirect_bits_satp_flush,
            self.redirect_bits_debug_is_ctrl,
            self.redirect_bits_debug_is_mem_vio,
            self.ftq_idx_ahead_valid,
            self.ftq_idx_ahead_flag,
            self.ftq_idx_ahead_value,
            self.commit_valid,
            self.commit_bits_flag,
            self.commit_bits_value,
        ):
            signal.value = 0
        for signals in (
            self.resolve_valid,
            self.resolve_bits_ftq_idx_flag,
            self.resolve_bits_ftq_idx_value,
            self.resolve_bits_ftq_offset,
            self.resolve_bits_pc_addr,
            self.resolve_bits_target_addr,
            self.resolve_bits_taken,
            self.resolve_bits_mispredict,
            self.resolve_bits_attribute_branch_type,
            self.resolve_bits_attribute_ras_action,
            self.call_ret_commit_valid,
            self.call_ret_commit_bits_ras_action,
            self.call_ret_commit_bits_ftq_ptr_value,
        ):
            for signal in signals:
                signal.value = 0


class BackendToIBufBundle(Bundle):
    SIGNAL_BINDINGS = {
        "decode_can_accept": "io_backend_toIBuf_decodeCanAccept",
        "resuming_vtype": "io_backend_toIBuf_resumingVType",
        "walk_to_arch_vtype": "io_backend_toIBuf_walkToArchVType",
        "walk_vtype_valid": "io_backend_toIBuf_walkVType_valid",
        "walk_vtype_bits_illegal": "io_backend_toIBuf_walkVType_bits_illegal",
        "walk_vtype_bits_vma": "io_backend_toIBuf_walkVType_bits_vma",
        "walk_vtype_bits_vta": "io_backend_toIBuf_walkVType_bits_vta",
        "walk_vtype_bits_vsew": "io_backend_toIBuf_walkVType_bits_vsew",
        "walk_vtype_bits_vlmul": "io_backend_toIBuf_walkVType_bits_vlmul",
        "vsetvl_vtype_illegal": "io_backend_toIBuf_vsetvlVType_illegal",
        "vsetvl_vtype_vma": "io_backend_toIBuf_vsetvlVType_vma",
        "vsetvl_vtype_vta": "io_backend_toIBuf_vsetvlVType_vta",
        "vsetvl_vtype_vsew": "io_backend_toIBuf_vsetvlVType_vsew",
        "vsetvl_vtype_vlmul": "io_backend_toIBuf_vsetvlVType_vlmul",
        "commit_vtype_valid": "io_backend_toIBuf_commitVType_vtype_valid",
        "commit_vtype_bits_illegal": "io_backend_toIBuf_commitVType_vtype_bits_illegal",
        "commit_vtype_bits_vma": "io_backend_toIBuf_commitVType_vtype_bits_vma",
        "commit_vtype_bits_vta": "io_backend_toIBuf_commitVType_vtype_bits_vta",
        "commit_vtype_bits_vsew": "io_backend_toIBuf_commitVType_vtype_bits_vsew",
        "commit_vtype_bits_vlmul": "io_backend_toIBuf_commitVType_vtype_bits_vlmul",
        "commit_vtype_has_vsetvl": "io_backend_toIBuf_commitVType_hasVsetvl",
    }

    decode_can_accept = Signal()
    resuming_vtype = Signal()
    walk_to_arch_vtype = Signal()
    walk_vtype_valid = Signal()
    walk_vtype_bits_illegal = Signal()
    walk_vtype_bits_vma = Signal()
    walk_vtype_bits_vta = Signal()
    walk_vtype_bits_vsew = Signal()
    walk_vtype_bits_vlmul = Signal()
    vsetvl_vtype_illegal = Signal()
    vsetvl_vtype_vma = Signal()
    vsetvl_vtype_vta = Signal()
    vsetvl_vtype_vsew = Signal()
    vsetvl_vtype_vlmul = Signal()
    commit_vtype_valid = Signal()
    commit_vtype_bits_illegal = Signal()
    commit_vtype_bits_vma = Signal()
    commit_vtype_bits_vta = Signal()
    commit_vtype_bits_vsew = Signal()
    commit_vtype_bits_vlmul = Signal()
    commit_vtype_has_vsetvl = Signal()

    def drive_idle(self) -> None:
        for attr_name in self.SIGNAL_BINDINGS:
            getattr(self, attr_name).value = 0
        self.decode_can_accept.value = 1


class BackendWfiBundle(Bundle):
    SIGNAL_BINDINGS = {
        "wfi_req": "io_backend_wfi_wfiReq",
        "wfi_safe": "io_backend_wfi_wfiSafe",
    }

    wfi_req = Signal()
    wfi_safe = Signal()

    def drive_idle(self) -> None:
        self.wfi_req.value = 0
