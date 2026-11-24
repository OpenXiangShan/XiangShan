from __future__ import annotations

from toffee import Bundle, Signal


class TlbCsrBundle(Bundle):
    OPTIONAL_SIGNAL_GROUPS = (("io_tlbCsr_priv_fenceVirt",),)
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
    io_tlbCsr_priv_virtChanged = Signal()
    io_tlbCsr_priv_fenceVirt = Signal()
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
            "io_tlbCsr_priv_virtChanged", "io_tlbCsr_priv_fenceVirt",
            "io_tlbCsr_priv_spvp", "io_tlbCsr_priv_imode",
            "io_tlbCsr_priv_dmode", "io_tlbCsr_priv_debug", "io_tlbCsr_mPBMTE",
            "io_tlbCsr_hPBMTE", "io_tlbCsr_pmm_mseccfg", "io_tlbCsr_pmm_menvcfg",
            "io_tlbCsr_pmm_henvcfg", "io_tlbCsr_pmm_hstatus", "io_tlbCsr_pmm_senvcfg",
        ):
            getattr(self, attr_name).value = 0
        self.io_tlbCsr_priv_imode.value = 3


class CSRControlBundle(Bundle):
    io_csrCtrl_pfCtrl_l1I_pf_enable = Signal()
    io_csrCtrl_bpCtrl_ubtbEnable = Signal()
    io_csrCtrl_bpCtrl_abtbEnable = Signal()
    io_csrCtrl_bpCtrl_mbtbEnable = Signal()
    io_csrCtrl_bpCtrl_tageEnable = Signal()
    io_csrCtrl_bpCtrl_scEnable = Signal()
    io_csrCtrl_bpCtrl_ittageEnable = Signal()
    io_csrCtrl_bpCtrl_rasEnable = Signal()
    io_csrCtrl_sbufferTimeout = Signal()
    io_csrCtrl_ldldVioCheckEnable = Signal()
    io_csrCtrl_cacheErrorEnable = Signal()
    io_csrCtrl_distributeCsr_w_valid = Signal()
    io_csrCtrl_distributeCsr_w_bits_addr = Signal()
    io_csrCtrl_distributeCsr_w_bits_data = Signal()
    io_csrCtrl_frontendTrigger_tUpdate_valid = Signal()
    io_csrCtrl_frontendTrigger_tUpdate_bits_addr = Signal()
    io_csrCtrl_frontendTrigger_tUpdate_bits_tdata_matchType = Signal()
    io_csrCtrl_frontendTrigger_tUpdate_bits_tdata_select = Signal()
    io_csrCtrl_frontendTrigger_tUpdate_bits_tdata_action = Signal()
    io_csrCtrl_frontendTrigger_tUpdate_bits_tdata_chain = Signal()
    io_csrCtrl_frontendTrigger_tUpdate_bits_tdata_tdata2 = Signal()
    io_csrCtrl_frontendTrigger_tEnableVec_0 = Signal()
    io_csrCtrl_frontendTrigger_tEnableVec_1 = Signal()
    io_csrCtrl_frontendTrigger_tEnableVec_2 = Signal()
    io_csrCtrl_frontendTrigger_tEnableVec_3 = Signal()
    io_csrCtrl_frontendTrigger_debugMode = Signal()
    io_csrCtrl_frontendTrigger_triggerCanRaiseBpExp = Signal()
    io_csrCtrl_memTrigger_tUpdate_valid = Signal()
    io_csrCtrl_memTrigger_tUpdate_bits_addr = Signal()
    io_csrCtrl_memTrigger_tUpdate_bits_tdata_matchType = Signal()
    io_csrCtrl_memTrigger_tUpdate_bits_tdata_select = Signal()
    io_csrCtrl_memTrigger_tUpdate_bits_tdata_action = Signal()
    io_csrCtrl_memTrigger_tUpdate_bits_tdata_chain = Signal()
    io_csrCtrl_memTrigger_tUpdate_bits_tdata_store = Signal()
    io_csrCtrl_memTrigger_tUpdate_bits_tdata_load = Signal()
    io_csrCtrl_memTrigger_tUpdate_bits_tdata_tdata2 = Signal()
    io_csrCtrl_memTrigger_tEnableVec_0 = Signal()
    io_csrCtrl_memTrigger_tEnableVec_1 = Signal()
    io_csrCtrl_memTrigger_tEnableVec_2 = Signal()
    io_csrCtrl_memTrigger_tEnableVec_3 = Signal()
    io_csrCtrl_memTrigger_debugMode = Signal()
    io_csrCtrl_memTrigger_triggerCanRaiseBpExp = Signal()
    io_csrCtrl_fsIsOff = Signal()

    def drive_idle(
        self,
        ubtb_enable: int = 1,
        abtb_enable: int = 1,
        mbtb_enable: int = 1,
        tage_enable: int = 1,
        sc_enable: int = 1,
        ittage_enable: int = 1,
        ras_enable: int = 1,
    ) -> None:
        for attr_name in (
            "io_csrCtrl_pfCtrl_l1I_pf_enable", "io_csrCtrl_bpCtrl_ubtbEnable",
            "io_csrCtrl_bpCtrl_abtbEnable", "io_csrCtrl_bpCtrl_mbtbEnable",
            "io_csrCtrl_bpCtrl_tageEnable", "io_csrCtrl_bpCtrl_scEnable",
            "io_csrCtrl_bpCtrl_ittageEnable", "io_csrCtrl_bpCtrl_rasEnable",
            "io_csrCtrl_sbufferTimeout", "io_csrCtrl_ldldVioCheckEnable",
            "io_csrCtrl_cacheErrorEnable", "io_csrCtrl_distributeCsr_w_valid",
            "io_csrCtrl_distributeCsr_w_bits_addr", "io_csrCtrl_distributeCsr_w_bits_data",
            "io_csrCtrl_frontendTrigger_tUpdate_valid", "io_csrCtrl_frontendTrigger_tUpdate_bits_addr",
            "io_csrCtrl_frontendTrigger_tUpdate_bits_tdata_matchType",
            "io_csrCtrl_frontendTrigger_tUpdate_bits_tdata_select",
            "io_csrCtrl_frontendTrigger_tUpdate_bits_tdata_action",
            "io_csrCtrl_frontendTrigger_tUpdate_bits_tdata_chain",
            "io_csrCtrl_frontendTrigger_tUpdate_bits_tdata_tdata2",
            "io_csrCtrl_frontendTrigger_tEnableVec_0", "io_csrCtrl_frontendTrigger_tEnableVec_1",
            "io_csrCtrl_frontendTrigger_tEnableVec_2", "io_csrCtrl_frontendTrigger_tEnableVec_3",
            "io_csrCtrl_frontendTrigger_debugMode", "io_csrCtrl_frontendTrigger_triggerCanRaiseBpExp",
            "io_csrCtrl_memTrigger_tUpdate_valid", "io_csrCtrl_memTrigger_tUpdate_bits_addr",
            "io_csrCtrl_memTrigger_tUpdate_bits_tdata_matchType",
            "io_csrCtrl_memTrigger_tUpdate_bits_tdata_select",
            "io_csrCtrl_memTrigger_tUpdate_bits_tdata_action",
            "io_csrCtrl_memTrigger_tUpdate_bits_tdata_chain",
            "io_csrCtrl_memTrigger_tUpdate_bits_tdata_store",
            "io_csrCtrl_memTrigger_tUpdate_bits_tdata_load",
            "io_csrCtrl_memTrigger_tUpdate_bits_tdata_tdata2",
            "io_csrCtrl_memTrigger_tEnableVec_0", "io_csrCtrl_memTrigger_tEnableVec_1",
            "io_csrCtrl_memTrigger_tEnableVec_2", "io_csrCtrl_memTrigger_tEnableVec_3",
            "io_csrCtrl_memTrigger_debugMode", "io_csrCtrl_memTrigger_triggerCanRaiseBpExp",
            "io_csrCtrl_fsIsOff",
        ):
            getattr(self, attr_name).value = 0
        self.io_csrCtrl_pfCtrl_l1I_pf_enable.value = 1
        self.io_csrCtrl_bpCtrl_ubtbEnable.value = int(ubtb_enable)
        self.io_csrCtrl_bpCtrl_abtbEnable.value = int(abtb_enable)
        self.io_csrCtrl_bpCtrl_mbtbEnable.value = int(mbtb_enable)
        self.io_csrCtrl_bpCtrl_tageEnable.value = int(tage_enable)
        self.io_csrCtrl_bpCtrl_scEnable.value = int(sc_enable)
        self.io_csrCtrl_bpCtrl_ittageEnable.value = int(ittage_enable)
        self.io_csrCtrl_bpCtrl_rasEnable.value = int(ras_enable)
