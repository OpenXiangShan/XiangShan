from __future__ import annotations

from toffee import Bundle, Signal


class TlbCsrBundle(Bundle):
    io_tlbCsr_satp_mode = Signal()
    io_tlbCsr_satp_asid = Signal()
    io_tlbCsr_satp_ppn = Signal()
    io_tlbCsr_satp_changed = Signal()
    io_tlbCsr_vsatp_mode = Signal()
    io_tlbCsr_vsatp_asid = Signal()
    io_tlbCsr_vsatp_ppn = Signal()
    io_tlbCsr_vsatp_changed = Signal()
    io_tlbCsr_hgatp_mode = Signal()
    io_tlbCsr_hgatp_vmid = Signal()
    io_tlbCsr_hgatp_ppn = Signal()
    io_tlbCsr_hgatp_changed = Signal()
    io_tlbCsr_mbmc_BME = Signal()
    io_tlbCsr_mbmc_CMODE = Signal()
    io_tlbCsr_mbmc_BCLEAR = Signal()
    io_tlbCsr_mbmc_BMA = Signal()
    io_tlbCsr_priv_mxr = Signal()
    io_tlbCsr_priv_sum = Signal()
    io_tlbCsr_priv_vmxr = Signal()
    io_tlbCsr_priv_vsum = Signal()
    io_tlbCsr_priv_virt = Signal()
    io_tlbCsr_priv_virt_changed = Signal()
    io_tlbCsr_priv_spvp = Signal()
    io_tlbCsr_priv_imode = Signal()
    io_tlbCsr_priv_dmode = Signal()
    io_tlbCsr_priv_debug = Signal()
    io_tlbCsr_mPBMTE = Signal()
    io_tlbCsr_hPBMTE = Signal()
    io_tlbCsr_pmm_mseccfg = Signal()
    io_tlbCsr_pmm_menvcfg = Signal()
    io_tlbCsr_pmm_henvcfg = Signal()
    io_tlbCsr_pmm_hstatus = Signal()
    io_tlbCsr_pmm_senvcfg = Signal()

    def drive_idle(self) -> None:
        for attr_name in (
            "io_tlbCsr_satp_mode", "io_tlbCsr_satp_asid", "io_tlbCsr_satp_ppn",
            "io_tlbCsr_satp_changed", "io_tlbCsr_vsatp_mode", "io_tlbCsr_vsatp_asid",
            "io_tlbCsr_vsatp_ppn", "io_tlbCsr_vsatp_changed", "io_tlbCsr_hgatp_mode",
            "io_tlbCsr_hgatp_vmid", "io_tlbCsr_hgatp_ppn", "io_tlbCsr_hgatp_changed",
            "io_tlbCsr_mbmc_BME", "io_tlbCsr_mbmc_CMODE", "io_tlbCsr_mbmc_BCLEAR",
            "io_tlbCsr_mbmc_BMA", "io_tlbCsr_priv_mxr", "io_tlbCsr_priv_sum",
            "io_tlbCsr_priv_vmxr", "io_tlbCsr_priv_vsum", "io_tlbCsr_priv_virt",
            "io_tlbCsr_priv_virt_changed", "io_tlbCsr_priv_spvp", "io_tlbCsr_priv_imode",
            "io_tlbCsr_priv_dmode", "io_tlbCsr_priv_debug", "io_tlbCsr_mPBMTE",
            "io_tlbCsr_hPBMTE", "io_tlbCsr_pmm_mseccfg", "io_tlbCsr_pmm_menvcfg",
            "io_tlbCsr_pmm_henvcfg", "io_tlbCsr_pmm_hstatus", "io_tlbCsr_pmm_senvcfg",
        ):
            getattr(self, attr_name).value = 0
        self.io_tlbCsr_priv_imode.value = 3


class CSRControlBundle(Bundle):
    io_csrCtrl_pf_ctrl_l1I_pf_enable = Signal()
    io_csrCtrl_bp_ctrl_ubtbEnable = Signal()
    io_csrCtrl_bp_ctrl_abtbEnable = Signal()
    io_csrCtrl_bp_ctrl_mbtbEnable = Signal()
    io_csrCtrl_bp_ctrl_tageEnable = Signal()
    io_csrCtrl_bp_ctrl_scEnable = Signal()
    io_csrCtrl_bp_ctrl_ittageEnable = Signal()
    io_csrCtrl_bp_ctrl_rasEnable = Signal()
    io_csrCtrl_sbuffer_timeout = Signal()
    io_csrCtrl_ldld_vio_check_enable = Signal()
    io_csrCtrl_cache_error_enable = Signal()
    io_csrCtrl_distribute_csr_w_valid = Signal()
    io_csrCtrl_distribute_csr_w_bits_addr = Signal()
    io_csrCtrl_distribute_csr_w_bits_data = Signal()
    io_csrCtrl_frontend_trigger_tUpdate_valid = Signal()
    io_csrCtrl_frontend_trigger_tUpdate_bits_addr = Signal()
    io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType = Signal()
    io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select = Signal()
    io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action = Signal()
    io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain = Signal()
    io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2 = Signal()
    io_csrCtrl_frontend_trigger_tEnableVec_0 = Signal()
    io_csrCtrl_frontend_trigger_tEnableVec_1 = Signal()
    io_csrCtrl_frontend_trigger_tEnableVec_2 = Signal()
    io_csrCtrl_frontend_trigger_tEnableVec_3 = Signal()
    io_csrCtrl_frontend_trigger_debugMode = Signal()
    io_csrCtrl_frontend_trigger_triggerCanRaiseBpExp = Signal()
    io_csrCtrl_mem_trigger_tUpdate_valid = Signal()
    io_csrCtrl_mem_trigger_tUpdate_bits_addr = Signal()
    io_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType = Signal()
    io_csrCtrl_mem_trigger_tUpdate_bits_tdata_select = Signal()
    io_csrCtrl_mem_trigger_tUpdate_bits_tdata_action = Signal()
    io_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain = Signal()
    io_csrCtrl_mem_trigger_tUpdate_bits_tdata_store = Signal()
    io_csrCtrl_mem_trigger_tUpdate_bits_tdata_load = Signal()
    io_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2 = Signal()
    io_csrCtrl_mem_trigger_tEnableVec_0 = Signal()
    io_csrCtrl_mem_trigger_tEnableVec_1 = Signal()
    io_csrCtrl_mem_trigger_tEnableVec_2 = Signal()
    io_csrCtrl_mem_trigger_tEnableVec_3 = Signal()
    io_csrCtrl_mem_trigger_debugMode = Signal()
    io_csrCtrl_mem_trigger_triggerCanRaiseBpExp = Signal()
    io_csrCtrl_fsIsOff = Signal()

    def drive_idle(
        self,
        ubtb_enable: int = 1,
        abtb_enable: int = 1,
        mbtb_enable: int = 1,
        tage_enable: int = 1,
        sc_enable: int = 1,
        ittage_enable: int = 1,
    ) -> None:
        for attr_name in (
            "io_csrCtrl_pf_ctrl_l1I_pf_enable", "io_csrCtrl_bp_ctrl_ubtbEnable",
            "io_csrCtrl_bp_ctrl_abtbEnable", "io_csrCtrl_bp_ctrl_mbtbEnable",
            "io_csrCtrl_bp_ctrl_tageEnable", "io_csrCtrl_bp_ctrl_scEnable",
            "io_csrCtrl_bp_ctrl_ittageEnable", "io_csrCtrl_bp_ctrl_rasEnable",
            "io_csrCtrl_sbuffer_timeout", "io_csrCtrl_ldld_vio_check_enable",
            "io_csrCtrl_cache_error_enable", "io_csrCtrl_distribute_csr_w_valid",
            "io_csrCtrl_distribute_csr_w_bits_addr", "io_csrCtrl_distribute_csr_w_bits_data",
            "io_csrCtrl_frontend_trigger_tUpdate_valid", "io_csrCtrl_frontend_trigger_tUpdate_bits_addr",
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_matchType",
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_select",
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_action",
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_chain",
            "io_csrCtrl_frontend_trigger_tUpdate_bits_tdata_tdata2",
            "io_csrCtrl_frontend_trigger_tEnableVec_0", "io_csrCtrl_frontend_trigger_tEnableVec_1",
            "io_csrCtrl_frontend_trigger_tEnableVec_2", "io_csrCtrl_frontend_trigger_tEnableVec_3",
            "io_csrCtrl_frontend_trigger_debugMode", "io_csrCtrl_frontend_trigger_triggerCanRaiseBpExp",
            "io_csrCtrl_mem_trigger_tUpdate_valid", "io_csrCtrl_mem_trigger_tUpdate_bits_addr",
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType",
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_select",
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_action",
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain",
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_store",
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_load",
            "io_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2",
            "io_csrCtrl_mem_trigger_tEnableVec_0", "io_csrCtrl_mem_trigger_tEnableVec_1",
            "io_csrCtrl_mem_trigger_tEnableVec_2", "io_csrCtrl_mem_trigger_tEnableVec_3",
            "io_csrCtrl_mem_trigger_debugMode", "io_csrCtrl_mem_trigger_triggerCanRaiseBpExp",
            "io_csrCtrl_fsIsOff",
        ):
            getattr(self, attr_name).value = 0
        self.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
        self.io_csrCtrl_bp_ctrl_ubtbEnable.value = int(ubtb_enable)
        self.io_csrCtrl_bp_ctrl_abtbEnable.value = int(abtb_enable)
        self.io_csrCtrl_bp_ctrl_mbtbEnable.value = int(mbtb_enable)
        self.io_csrCtrl_bp_ctrl_tageEnable.value = int(tage_enable)
        self.io_csrCtrl_bp_ctrl_scEnable.value = int(sc_enable)
        self.io_csrCtrl_bp_ctrl_ittageEnable.value = int(ittage_enable)
