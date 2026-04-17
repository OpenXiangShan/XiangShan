from __future__ import annotations

from toffee import Bundle, Signal, SignalList


class PTWBundle(Bundle):
    SIGNAL_BINDINGS = {
        "req_0_ready": "io_ptw_req_0_ready",
        "req_0_valid": "io_ptw_req_0_valid",
        "req_0_bits_vpn": "io_ptw_req_0_bits_vpn",
        "req_0_bits_s2xlate": "io_ptw_req_0_bits_s2xlate",
        "resp_ready": "io_ptw_resp_ready",
        "resp_valid": "io_ptw_resp_valid",
        "resp_bits_s2xlate": "io_ptw_resp_bits_s2xlate",
        "resp_bits_s1_entry_tag": "io_ptw_resp_bits_s1_entry_tag",
        "resp_bits_s1_entry_asid": "io_ptw_resp_bits_s1_entry_asid",
        "resp_bits_s1_entry_vmid": "io_ptw_resp_bits_s1_entry_vmid",
        "resp_bits_s1_entry_n": "io_ptw_resp_bits_s1_entry_n",
        "resp_bits_s1_entry_pbmt": "io_ptw_resp_bits_s1_entry_pbmt",
        "resp_bits_s1_entry_perm_a": "io_ptw_resp_bits_s1_entry_perm_a",
        "resp_bits_s1_entry_perm_g": "io_ptw_resp_bits_s1_entry_perm_g",
        "resp_bits_s1_entry_perm_u": "io_ptw_resp_bits_s1_entry_perm_u",
        "resp_bits_s1_entry_perm_x": "io_ptw_resp_bits_s1_entry_perm_x",
        "resp_bits_s1_entry_perm_w": "io_ptw_resp_bits_s1_entry_perm_w",
        "resp_bits_s1_entry_perm_r": "io_ptw_resp_bits_s1_entry_perm_r",
        "resp_bits_s1_entry_level": "io_ptw_resp_bits_s1_entry_level",
        "resp_bits_s1_entry_v": "io_ptw_resp_bits_s1_entry_v",
        "resp_bits_s1_entry_ppn": "io_ptw_resp_bits_s1_entry_ppn",
        "resp_bits_s1_addr_low": "io_ptw_resp_bits_s1_addr_low",
        "resp_bits_s1_pf": "io_ptw_resp_bits_s1_pf",
        "resp_bits_s1_af": "io_ptw_resp_bits_s1_af",
        "sfence_valid": "io_sfence_valid",
        "sfence_bits_rs1": "io_sfence_bits_rs1",
        "sfence_bits_rs2": "io_sfence_bits_rs2",
        "sfence_bits_addr": "io_sfence_bits_addr",
        "sfence_bits_id": "io_sfence_bits_id",
        "sfence_bits_hv": "io_sfence_bits_hv",
        "sfence_bits_hg": "io_sfence_bits_hg",
    }

    req_0_ready = Signal()
    req_0_valid = Signal()
    req_0_bits_vpn = Signal()
    req_0_bits_s2xlate = Signal()
    resp_ready = Signal()
    resp_valid = Signal()
    resp_bits_s2xlate = Signal()
    resp_bits_s1_entry_tag = Signal()
    resp_bits_s1_entry_asid = Signal()
    resp_bits_s1_entry_vmid = Signal()
    resp_bits_s1_entry_n = Signal()
    resp_bits_s1_entry_pbmt = Signal()
    resp_bits_s1_entry_perm_a = Signal()
    resp_bits_s1_entry_perm_g = Signal()
    resp_bits_s1_entry_perm_u = Signal()
    resp_bits_s1_entry_perm_x = Signal()
    resp_bits_s1_entry_perm_w = Signal()
    resp_bits_s1_entry_perm_r = Signal()
    resp_bits_s1_entry_level = Signal()
    resp_bits_s1_entry_v = Signal()
    resp_bits_s1_entry_ppn = Signal()
    resp_bits_s1_addr_low = Signal()
    resp_bits_s1_ppn_low = SignalList("io_ptw_resp_bits_s1_ppn_low_#", 8)
    resp_bits_s1_valididx = SignalList("io_ptw_resp_bits_s1_valididx_#", 8)
    resp_bits_s1_pteidx = SignalList("io_ptw_resp_bits_s1_pteidx_#", 8)
    resp_bits_s1_pf = Signal()
    resp_bits_s1_af = Signal()
    sfence_valid = Signal()
    sfence_bits_rs1 = Signal()
    sfence_bits_rs2 = Signal()
    sfence_bits_addr = Signal()
    sfence_bits_id = Signal()
    sfence_bits_hv = Signal()
    sfence_bits_hg = Signal()

    def drive_idle(self) -> None:
        self.req_0_ready.value = 0
        self.resp_valid.value = 0
        self.resp_bits_s2xlate.value = 0
        self.resp_bits_s1_entry_tag.value = 0
        self.resp_bits_s1_entry_asid.value = 0
        self.resp_bits_s1_entry_vmid.value = 0
        self.resp_bits_s1_entry_n.value = 0
        self.resp_bits_s1_entry_pbmt.value = 0
        self.resp_bits_s1_entry_perm_a.value = 0
        self.resp_bits_s1_entry_perm_g.value = 0
        self.resp_bits_s1_entry_perm_u.value = 0
        self.resp_bits_s1_entry_perm_x.value = 0
        self.resp_bits_s1_entry_perm_w.value = 0
        self.resp_bits_s1_entry_perm_r.value = 0
        self.resp_bits_s1_entry_level.value = 0
        self.resp_bits_s1_entry_v.value = 0
        self.resp_bits_s1_entry_ppn.value = 0
        self.resp_bits_s1_addr_low.value = 0
        for signal in self.resp_bits_s1_ppn_low:
            signal.value = 0
        for signal in self.resp_bits_s1_valididx:
            signal.value = 0
        for signal in self.resp_bits_s1_pteidx:
            signal.value = 0
        self.resp_bits_s1_pf.value = 0
        self.resp_bits_s1_af.value = 0
        self.sfence_valid.value = 0
        self.sfence_bits_rs1.value = 0
        self.sfence_bits_rs2.value = 0
        self.sfence_bits_addr.value = 0
        self.sfence_bits_id.value = 0
        self.sfence_bits_hv.value = 0
        self.sfence_bits_hg.value = 0
