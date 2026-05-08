from __future__ import annotations

from toffee import Bundle, Signal


class CSRControlBundle(Bundle):
    # Newer Frontend DUT exports only mode/asid/vmid for these CSR groups.
    # Keep the legacy ppn fields optional so the Python env can still bind.
    OPTIONAL_ATTRS = {
        "io_tlbCsr_satp_ppn",
        "io_tlbCsr_vsatp_ppn",
        "io_tlbCsr_hgatp_ppn",
        "io_tlbCsr_priv_virt_changed",
        "io_tlbCsr_satp_changed",
        "io_tlbCsr_vsatp_changed",
        "io_tlbCsr_hgatp_changed",
    }

    io_tlbCsr_priv_imode = Signal()
    io_tlbCsr_priv_virt = Signal()
    io_tlbCsr_priv_virt_changed = Signal()
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
    io_csrCtrl_bp_ctrl_ubtbEnable = Signal()
    io_csrCtrl_bp_ctrl_abtbEnable = Signal()
    io_csrCtrl_bp_ctrl_mbtbEnable = Signal()
    io_csrCtrl_bp_ctrl_tageEnable = Signal()
    io_csrCtrl_bp_ctrl_scEnable = Signal()
    io_csrCtrl_bp_ctrl_ittageEnable = Signal()
    io_csrCtrl_pf_ctrl_l1I_pf_enable = Signal()
    io_csrCtrl_fsIsOff = Signal()
    io_csrCtrl_distribute_csr_w_valid = Signal()

    def drive_idle(
        self,
        ubtb_enable: int = 1,
        abtb_enable: int = 1,
        mbtb_enable: int = 1,
        tage_enable: int = 1,
        sc_enable: int = 1,
        ittage_enable: int = 1,
    ) -> None:
        self.io_tlbCsr_priv_imode.value = 3
        self.io_tlbCsr_priv_virt.value = 0
        self.io_tlbCsr_priv_virt_changed.value = 0
        self.io_tlbCsr_satp_mode.value = 0
        self.io_tlbCsr_satp_asid.value = 0
        self.io_tlbCsr_satp_ppn.value = 0
        self.io_tlbCsr_satp_changed.value = 0
        self.io_tlbCsr_vsatp_mode.value = 0
        self.io_tlbCsr_vsatp_asid.value = 0
        self.io_tlbCsr_vsatp_ppn.value = 0
        self.io_tlbCsr_vsatp_changed.value = 0
        self.io_tlbCsr_hgatp_mode.value = 0
        self.io_tlbCsr_hgatp_vmid.value = 0
        self.io_tlbCsr_hgatp_ppn.value = 0
        self.io_tlbCsr_hgatp_changed.value = 0
        self.io_csrCtrl_bp_ctrl_ubtbEnable.value = int(ubtb_enable)
        self.io_csrCtrl_bp_ctrl_abtbEnable.value = int(abtb_enable)
        self.io_csrCtrl_bp_ctrl_mbtbEnable.value = int(mbtb_enable)
        self.io_csrCtrl_bp_ctrl_tageEnable.value = int(tage_enable)
        self.io_csrCtrl_bp_ctrl_scEnable.value = int(sc_enable)
        self.io_csrCtrl_bp_ctrl_ittageEnable.value = int(ittage_enable)
        self.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
        self.io_csrCtrl_fsIsOff.value = 0
        self.io_csrCtrl_distribute_csr_w_valid.value = 0
