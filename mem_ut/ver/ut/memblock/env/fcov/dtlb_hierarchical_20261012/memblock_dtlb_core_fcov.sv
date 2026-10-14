//=========================================================
// memblock_dtlb_core_fcov：DTLB V2 只读功能覆盖率。
// 普通信号端口、时钟事件采样、逐特性直接建仓；没有结构体库、function 或采样分发器。
// 时延与长度采用范围仓；物理 way/slot/子页采用逐项仓。
//=========================================================
`ifndef MEMBLOCK_DTLB_CORE_FCOV__SV
`define MEMBLOCK_DTLB_CORE_FCOV__SV

`ifdef MEMBLOCK_UT_FCOV
module memblock_dtlb_core_fcov #(
    parameter integer GROUP_ID   = 0,
    parameter integer PORT_INDEX = 0,
    parameter bit HAS_FULLVA     = 1'b1,
    parameter bit HAS_GPA        = 1'b1,
    parameter bit HAS_PADDR1     = 1'b1,
    parameter bit HAS_LATE_KILL  = 1'b1,
    parameter bit HAS_HLVX       = 1'b1
) (
    input wire clock,
    input wire reset,
    input wire reset_backend_done,
    input wire req_valid,
    input wire req_kill,
    input wire req_late_kill,
    input wire [49:0] req_vaddr,
    input wire [63:0] req_fullva,
    input wire req_checkfullva,
    input wire [2:0] req_cmd,
    input wire req_hyperinst,
    input wire req_hlvx,
    input wire req_prefetch,
    input wire req_no_translate,
    input wire [47:0] req_pmpaddr,
    input wire req_mabuf,
    input wire [8:0] req_rob,
    input wire [63:0] req_effective_va,
    input wire req_pre_s1,
    input wire req_pre_s2,
    input wire [1:0] req_stage,
    input wire [1:0] req_pmm,
    input wire req_prepf,
    input wire req_pregpf,
    input wire req_preaf,
    input wire req_virt,
    input wire req_lookup,
    input wire resp_valid,
    input wire resp_miss,
    input wire [47:0] resp_paddr_0,
    input wire [47:0] resp_paddr_1,
    input wire [63:0] resp_gpaddr,
    input wire [63:0] resp_fullva,
    input wire [1:0] resp_pbmt,
    input wire [5:0] resp_excp,
    input wire resp_ext,
    input wire resp_hyper,
    input wire resp_nonleaf,
    input wire [49:0] req_out_vaddr,
    input wire [2:0] req_out_cmd,
    input wire [1:0] req_out_s2xlate,
    input wire [1:0] effective_mode,
    input wire resp_virt,
    input wire translate_enable,
    input wire storage_hit,
    input wire ptw_bypass_hit,
    input wire ptw_bypass_hit_fast,
    input wire resp_miss_read,
    input wire resp_need_replay,
    input wire resp_can_gpa,
    input wire resp_has_gpf,
    input wire resp_last_red,
    input wire resp_qvalid,
    input wire [37:0] resp_qvpn,
    input wire [1:0] resp_qstage,
    input wire resp_qgpa,
    input wire resp_replay,
    input wire resp_pmpvalid,
    input wire [47:0] resp_pmpaddr,
    input wire [2:0] resp_pmpcmd,
    input wire [1:0] resp_pmpmode,
    input wire [35:0] resp_ppn,
    input wire [1:0] resp_s1pbmt,
    input wire [1:0] resp_s2pbmt,
    input wire resp_s1_pf,
    input wire resp_s1_af,
    input wire resp_s1_v,
    input wire resp_s1_d,
    input wire resp_s1_a,
    input wire resp_s1_u,
    input wire resp_s1_x,
    input wire resp_s1_w,
    input wire resp_s1_r,
    input wire resp_s2_pf,
    input wire resp_s2_af,
    input wire resp_s2_d,
    input wire resp_s2_a,
    input wire resp_s2_x,
    input wire resp_s2_w,
    input wire resp_s2_r,
    input wire [3:0] csr_satp_mode,
    input wire [15:0] csr_asid,
    input wire csr_satp_changed,
    input wire [3:0] csr_vsatp_mode,
    input wire [15:0] csr_vasid,
    input wire csr_vsatp_changed,
    input wire [3:0] csr_hgatp_mode,
    input wire [15:0] csr_vmid,
    input wire csr_hgatp_changed,
    input wire csr_mxr,
    input wire csr_sum,
    input wire csr_vmxr,
    input wire csr_vsum,
    input wire csr_virt,
    input wire csr_virt_changed,
    input wire csr_spvp,
    input wire [1:0] csr_imode,
    input wire [1:0] csr_dmode,
    input wire csr_bme,
    input wire csr_cmode,
    input wire [1:0] csr_mseccfg,
    input wire [1:0] csr_menvcfg,
    input wire [1:0] csr_henvcfg,
    input wire [1:0] csr_hstatus,
    input wire [1:0] csr_senvcfg,
    input wire ptw_valid,
    input wire [1:0] ptw_stage,
    input wire ptw_getgpa,
    input wire [34:0] ptw_s1tag,
    input wire [15:0] ptw_s1asid,
    input wire [13:0] ptw_s1vmid,
    input wire [1:0] ptw_s1level,
    input wire ptw_s1n,
    input wire ptw_s1_g,
    input wire [2:0] ptw_s1low,
    input wire [7:0] ptw_valididx,
    input wire [37:0] ptw_s2tag,
    input wire [13:0] ptw_s2vmid,
    input wire [1:0] ptw_s2level,
    input wire ptw_s2n,
    input wire ptw_s2_g,
    input wire ptw_s2_u,
    input wire gpa_write,
    input wire gpa_ng,
    input wire gpa_ng_wire,
    input wire [8:0] gpa_count,
    input wire gpa_clear,
    input wire gpa_owner_valid,
    input wire [8:0] gpa_owner,
    input wire [37:0] gpa_vpn,
    input wire [1:0] gpa_stage,
    input wire gpa_response_ready,
    input wire [1:0] gpa_s1level,
    input wire gpa_s1fake,
    input wire redirect_valid,
    input wire redirect_level,
    input wire [8:0] redirect_rob,
    input wire [8:0] rob_head,
    input wire [47:0] storage_hit_vector,
    input wire [47:0][1:0] storage_levels,
    input wire [47:0] storage_napot
);

    reg req_valid_d1;
    reg req_kill_d1;
    reg req_late_kill_d1;
    reg [49:0] req_vaddr_d1;
    reg [63:0] req_fullva_d1;
    reg req_checkfullva_d1;
    reg [2:0] req_cmd_d1;
    reg req_hyperinst_d1;
    reg req_hlvx_d1;
    reg req_prefetch_d1;
    reg req_no_translate_d1;
    reg [47:0] req_pmpaddr_d1;
    reg req_mabuf_d1;
    reg [8:0] req_rob_d1;
    reg [63:0] req_effective_va_d1;
    reg req_pre_s1_d1;
    reg req_pre_s2_d1;
    reg [1:0] req_stage_d1;
    reg [1:0] req_pmm_d1;
    reg req_prepf_d1;
    reg req_pregpf_d1;
    reg req_preaf_d1;
    reg req_virt_d1;
    reg req_lookup_d1;
    reg [3:0] csr_satp_mode_d1;
    reg [15:0] csr_asid_d1;
    reg csr_satp_changed_d1;
    reg [3:0] csr_vsatp_mode_d1;
    reg [15:0] csr_vasid_d1;
    reg csr_vsatp_changed_d1;
    reg [3:0] csr_hgatp_mode_d1;
    reg [15:0] csr_vmid_d1;
    reg csr_hgatp_changed_d1;
    reg csr_mxr_d1;
    reg csr_sum_d1;
    reg csr_vmxr_d1;
    reg csr_vsum_d1;
    reg csr_virt_d1;
    reg csr_virt_changed_d1;
    reg csr_spvp_d1;
    reg [1:0] csr_imode_d1;
    reg [1:0] csr_dmode_d1;
    reg csr_bme_d1;
    reg csr_cmode_d1;
    reg [1:0] csr_mseccfg_d1;
    reg [1:0] csr_menvcfg_d1;
    reg [1:0] csr_henvcfg_d1;
    reg [1:0] csr_hstatus_d1;
    reg [1:0] csr_senvcfg_d1;
    reg req_valid_d2;
    reg [31:0] accepted_run_length;
    reg [31:0] accepted_run_length_d1;
    reg gpa_ng_d1;
    reg gpa_ready_d1;
    reg [8:0] gpa_count_d1;
    reg [37:0] gpa_vpn_d1;
    reg [1:0] gpa_stage_d1;
    reg ptw_valid_d1;
    reg [1:0] ptw_stage_d1;
    reg [1:0] ptw_s1level_d1;
    reg [1:0] ptw_s2level_d1;
    reg ptw_s1n_d1;
    reg ptw_s2n_d1;
    reg [34:0] ptw_s1tag_d1;
    reg [37:0] ptw_s2tag_d1;
    reg [2:0] ptw_s1low_d1;
    reg [7:0] ptw_valididx_d1;
    reg [31:0] miss_age;
    reg miss_pending;
    reg [49:0] miss_vaddr;
    reg [8:0] miss_rob;
    reg [31:0] retry_count;
    reg return_seen;
    reg history_valid;
    reg [49:0] req_vaddr_d2;
    reg previous_return_matches;
    reg [49:0] last_vaddr;
    reg [63:0] last_fullva;
    reg [47:0] last_paddr;
    reg [8:0] last_rob;
    reg [2:0] last_fault;
    reg [1:0] last_pbmt;
    reg last_response_valid;
    reg last_bypass;
    reg [3:0] permission_controls_d1;
    reg [47:0] context_ids_d1;
    reg [1:0] s2_ug_d1;
    reg return_older;
    reg bypass_fast_d1;
    reg ptw_getgpa_d1;
    reg owner_redirected_d1;
    reg redirect_valid_d1;
    reg fast_gpa_start_d1;
    reg gpa_start_d1;
    reg ptw_getgpa_event_d1;
    reg flushed_epoch;
    reg refill_seen;
    reg [1:0] selected_level;
    reg selected_napot;
    wire sample_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);
    wire request_due = req_valid_d1 && !req_kill_d1;
    wire response_sample = request_due && resp_valid;
    wire response_hit = storage_hit || ptw_bypass_hit;
    wire pre_fault = req_checkfullva_d1 && (req_prepf_d1 || req_pregpf_d1 || req_preaf_d1);
    wire read_command = req_out_cmd == 3'd0 || req_out_cmd == 3'd4;
    wire write_command = req_out_cmd == 3'd1 || req_out_cmd == 3'd5;
    wire s1_enabled = translate_enable && req_out_s2xlate != 2'd2;
    wire s2_enabled = translate_enable && req_out_s2xlate >= 2'd2;
    wire access_fault = (s1_enabled && resp_s1_af) || (s2_enabled && resp_s2_af);
    wire fake_pte = !resp_s1_v && !resp_s1_pf && !resp_s1_af && req_out_s2xlate != 2'd2;
    wire nonleaf_pte = resp_s1_v && !resp_s1_r && !resp_s1_w && !resp_s1_x && !resp_s1_pf && !resp_s1_af;
    wire effective_sum = (resp_virt || req_hyperinst_d1) ? csr_vsum : csr_sum;
    wire effective_mxr = csr_mxr || ((resp_virt || req_hyperinst_d1) && csr_vmxr);
    wire mode_permission = !(effective_mode == 2'd0 && !resp_s1_u) && !(effective_mode == 2'd1 && resp_s1_u &&
        !effective_sum);
    wire s1_read_permission = req_hlvx_d1 ? resp_s1_x : (resp_s1_r || (effective_mxr && resp_s1_x));
    wire s2_read_permission = req_hlvx_d1 ? resp_s2_x : (resp_s2_r || (csr_mxr && resp_s2_x));
    wire s1_page_fault = s1_enabled && !fake_pte && !nonleaf_pte &&
        ((read_command && (!mode_permission || !s1_read_permission || !resp_s1_a || resp_s1_pf)) ||
         (write_command && (!mode_permission || !resp_s1_w || !resp_s1_a || !resp_s1_d || resp_s1_pf)));
    wire s2_page_fault = s2_enabled &&
        ((read_command && (!s2_read_permission || !resp_s2_a || resp_s2_pf)) ||
         (write_command && (!resp_s2_w || !resp_s2_a || !resp_s2_d || resp_s2_pf)));
    wire [2:0] expected_fault = pre_fault ? {req_preaf_d1, req_pregpf_d1, req_prepf_d1} :
        !response_hit ? 3'b000 : {access_fault, s2_page_fault && !access_fault && !s1_page_fault,
        s1_page_fault && !access_fault};
    wire [2:0] actual_fault = {(|resp_excp[5:4]), (|resp_excp[3:2]), (|resp_excp[1:0])};
    wire fault_matches = actual_fault == expected_fault &&
        (read_command ? (resp_excp & 6'b101010) == 0 : write_command ? (resp_excp & 6'b010101) == 0 : 1'b1);
    wire [1:0] expected_pbmt = !translate_enable ? 2'd0 : req_out_s2xlate == 2'd2 ? resp_s2pbmt :
        req_out_s2xlate == 2'd3 && resp_s2pbmt != 0 ? resp_s2pbmt : resp_s1pbmt;
    wire [1:0] hit_source = ptw_bypass_hit ? 2'd1 : storage_hit ? 2'd0 : 2'd2;
    wire gpa_matches = gpa_vpn == req_out_vaddr[49:12] && gpa_stage == req_out_s2xlate;
    wire rob_younger = (req_rob_d1[8] ^ redirect_rob[8]) ^ (req_rob_d1[7:0] > redirect_rob[7:0]);
    wire current_redirect = redirect_valid && (rob_younger || (redirect_level && req_rob_d1 == redirect_rob));
    wire gpa_clear_event = gpa_clear || gpa_count == 9'd511;
    wire gpa_start = response_sample && resp_has_gpf && !ptw_bypass_hit && !gpa_ng && req_out_s2xlate != 2'd2 &&
        !(gpa_response_ready && gpa_matches) && !req_late_kill && !req_prefetch_d1 && !current_redirect &&
            !resp_last_red;
    wire blocked_by_gpa = gpa_ng && !gpa_response_ready && !gpa_matches;
    wire [4:0] miss_cause = {req_late_kill, blocked_by_gpa, resp_need_replay, ptw_valid_d1, ptw_valid};
    wire [1:0] raw_pmm = (req_hyperinst_d1 ? {1'b0, csr_spvp_d1} : csr_dmode_d1) == 2'd3 ? csr_mseccfg_d1 :
        (req_hyperinst_d1 ? {1'b0, csr_spvp_d1} : csr_dmode_d1) == 2'd1 ?
        ((req_hyperinst_d1 || csr_virt_d1) ? csr_henvcfg_d1 : csr_menvcfg_d1) :
        req_hyperinst_d1 ? (csr_imode_d1 == 0 ? csr_hstatus_d1 : csr_senvcfg_d1) :
        csr_virt_d1 ? csr_henvcfg_d1 : csr_senvcfg_d1;
    wire pmm_disabled = req_hlvx_d1 ||
        ((req_hyperinst_d1 ? {1'b0, csr_spvp_d1} : csr_dmode_d1) != 2'd3 &&
         (csr_mxr_d1 || ((csr_virt_d1 || req_hyperinst_d1) && csr_vmxr_d1)));
    wire [63:0] expected_effective_va = pmm_disabled ? req_fullva_d1 : raw_pmm == 2'd2 ?
        (req_pre_s1_d1 ? {{7{req_fullva_d1[56]}}, req_fullva_d1[56:0]} : {7'd0, req_fullva_d1[56:0]}) :
        raw_pmm == 2'd3 ? (req_pre_s1_d1 ? {{16{req_fullva_d1[47]}}, req_fullva_d1[47:0]} :
        {16'd0, req_fullva_d1[47:0]}) : req_fullva_d1;
    wire [15:0] masked_address_bits = req_pmm_d1 == 2'd2 ? {9'd0, req_fullva_d1[63:57]} : req_fullva_d1[63:48];
    wire [1:0] pmm_address_class = masked_address_bits == 0 ? 2'd0 :
        ((req_pmm_d1 == 2'd2 && masked_address_bits == 127) || masked_address_bits == 65535) ? 2'd1 : 2'd2;
    wire [3:0] active_va_mode = req_stage_d1 == 0 ? csr_satp_mode_d1 :
        req_stage_d1 == 2 ? csr_hgatp_mode_d1 : csr_vsatp_mode_d1;
    wire high_address_bad = (!req_pre_s1_d1 && !req_pre_s2_d1) ? req_effective_va_d1[63:48] != 0 :
        req_stage_d1 == 2 ? (active_va_mode == 8 ? req_effective_va_d1[63:41] != 0 : req_effective_va_d1[63:50] != 0) :
        active_va_mode == 8 ? req_effective_va_d1 != {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]} :
        req_effective_va_d1 != {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]};
    wire [47:0] expected_paddr = !translate_enable ? (req_no_translate_d1 ? req_pmpaddr : req_vaddr_d1[47:0]) :
        {resp_ppn, req_vaddr_d1[11:0]};
    wire address_matches = resp_paddr_0 == expected_paddr;
    wire [2:0] va_boundary_39 = req_effective_va_d1 == 0 ? 3'd0 : req_effective_va_d1 == 64'h0000003fffffffff ? 3'd1
        : req_effective_va_d1 == 64'hffffffc000000000 ? 3'd2 : req_effective_va_d1 == 64'h0000004000000000
 || req_effective_va_d1 == 64'h0000003ffffffffe ? 3'd3 : 3'd4;
    wire [2:0] va_boundary_48 = req_effective_va_d1 == 0 ? 3'd0 : req_effective_va_d1 == 64'h00007fffffffffff ? 3'd1
        : req_effective_va_d1 == 64'hffff800000000000 ? 3'd2 : req_effective_va_d1 == 64'h0000800000000000
 || req_effective_va_d1 == 64'h00007ffffffffffe ? 3'd3 : 3'd4;
    wire [2:0] va_boundary_41_unsigned = req_effective_va_d1 == 0 ? 3'd0 : req_effective_va_d1 ==
        64'h000001ffffffffff ? 3'd1 : req_effective_va_d1 == 64'h0000020000000000
 || req_effective_va_d1 == 64'h000001fffffffffe ? 3'd3 : 3'd4;
    wire [2:0] va_boundary_50_unsigned = req_effective_va_d1 == 0 ? 3'd0 : req_effective_va_d1 ==
        64'h0003ffffffffffff ? 3'd1 : req_effective_va_d1 == 64'h0004000000000000
 || req_effective_va_d1 == 64'h0003fffffffffffe ? 3'd3 : 3'd4;
    wire [2:0] va_boundary_48_unsigned = req_effective_va_d1 == 0 ? 3'd0 : req_effective_va_d1 ==
        64'h0000ffffffffffff ? 3'd1 : req_effective_va_d1 == 64'h0001000000000000
 || req_effective_va_d1 == 64'h0000fffffffffffe ? 3'd3 : 3'd4;

    wire [37:0] return_tag = ptw_stage == 2'd2 ? ptw_s2tag : {ptw_s1tag, ptw_s1low};
    wire [1:0] return_level = ptw_stage == 2'd2 ? ptw_s2level : ptw_stage != 2'd3 ? ptw_s1level :
        ptw_s1level < ptw_s2level ? ptw_s1level : ptw_s2level;
    wire return_napot = ptw_stage == 2'd2 ? ptw_s2n : ptw_stage != 2'd3 ? ptw_s1n :
        (ptw_s1n && ptw_s2level != 0) || (ptw_s2n && ptw_s1level != 0) || (ptw_s1n && ptw_s2n);
    wire return_context_matches = ptw_stage == req_out_s2xlate &&
        (req_out_s2xlate == 2 || ptw_s1_g || ptw_s1asid == (req_out_s2xlate == 0 ? csr_asid : csr_vasid)) &&
        (req_out_s2xlate == 0 || {2'd0, (req_out_s2xlate == 2 ? ptw_s2vmid : ptw_s1vmid)} == csr_vmid);
    wire return_address_matches = req_out_vaddr[49:39] == return_tag[37:27] &&
        (return_level == 3 || req_out_vaddr[38:30] == return_tag[26:18]) &&
        (return_level >= 2 || req_out_vaddr[29:21] == return_tag[17:9]) &&
        (return_level != 0 || (return_napot ? req_out_vaddr[20:16] == return_tag[8:4] :
        req_out_vaddr[20:15] == return_tag[8:3])) && (return_level != 0 || return_napot ||
        (req_out_s2xlate < 2 ? ptw_valididx[req_out_vaddr[14:12]] : return_tag[2:0] == req_out_vaddr[14:12]));
    wire current_return_matches = ptw_valid && return_context_matches && return_address_matches;
    wire [1:0] response_end_reason = resp_miss ? 2'd2 : actual_fault != 0 ? 2'd1 : 2'd0;
    wire [1:0] actual_miss_destination = {resp_qvalid, resp_replay};
    wire [1:0] response_knownness = $isunknown({resp_valid, resp_miss, resp_excp, resp_paddr_0, resp_pbmt}) ? 2'd1
        : 2'd0;
    wire owner_redirected = redirect_valid && gpa_owner_valid &&
        ((gpa_owner[8] ^ redirect_rob[8] ^ (gpa_owner[7:0] > redirect_rob[7:0])) ||
        (redirect_level && gpa_owner == redirect_rob));
    wire [1:0] redirect_relation = req_rob_d1 == redirect_rob ? 2'd0 : rob_younger ? 2'd2 : 2'd1;
    wire [11:0] expected_gpa_offset = req_fullva_d1[12] == req_out_vaddr[12] ? req_fullva_d1[11:0] :
        req_out_vaddr[11:0];
    wire [2:0] kill_hit_class = (req_late_kill && !req_kill_d1 ? 3'd3 : 3'd0) +
        (ptw_bypass_hit ? 3'd1 : storage_hit ? 3'd0 : 3'd2);
    // 仅保留时序点所需的固定容量历史；不扫描 testcase 历史，也不驱动 DUT。
    always @(posedge clock) begin

        if (reset) begin
            req_valid_d1 <= '0;
            req_kill_d1 <= '0;
            req_late_kill_d1 <= '0;
            req_vaddr_d1 <= '0;
            req_fullva_d1 <= '0;
            req_checkfullva_d1 <= '0;
            req_cmd_d1 <= '0;
            req_hyperinst_d1 <= '0;
            req_hlvx_d1 <= '0;
            req_prefetch_d1 <= '0;
            req_no_translate_d1 <= '0;
            req_pmpaddr_d1 <= '0;
            req_mabuf_d1 <= '0;
            req_rob_d1 <= '0;
            req_effective_va_d1 <= '0;
            req_pre_s1_d1 <= '0;
            req_pre_s2_d1 <= '0;
            req_stage_d1 <= '0;
            req_pmm_d1 <= '0;
            req_prepf_d1 <= '0;
            req_pregpf_d1 <= '0;
            req_preaf_d1 <= '0;
            req_virt_d1 <= '0;
            req_lookup_d1 <= '0;
            csr_satp_mode_d1 <= '0;
            csr_asid_d1 <= '0;
            csr_satp_changed_d1 <= '0;
            csr_vsatp_mode_d1 <= '0;
            csr_vasid_d1 <= '0;
            csr_vsatp_changed_d1 <= '0;
            csr_hgatp_mode_d1 <= '0;
            csr_vmid_d1 <= '0;
            csr_hgatp_changed_d1 <= '0;
            csr_mxr_d1 <= '0;
            csr_sum_d1 <= '0;
            csr_vmxr_d1 <= '0;
            csr_vsum_d1 <= '0;
            csr_virt_d1 <= '0;
            csr_virt_changed_d1 <= '0;
            csr_spvp_d1 <= '0;
            csr_imode_d1 <= '0;
            csr_dmode_d1 <= '0;
            csr_bme_d1 <= '0;
            csr_cmode_d1 <= '0;
            csr_mseccfg_d1 <= '0;
            csr_menvcfg_d1 <= '0;
            csr_henvcfg_d1 <= '0;
            csr_hstatus_d1 <= '0;
            csr_senvcfg_d1 <= '0;
            history_valid <= '0;
            req_valid_d2 <= '0;
            req_vaddr_d2 <= '0;
            accepted_run_length <= '0;
            accepted_run_length_d1 <= '0;
            gpa_ng_d1 <= '0;
            gpa_ready_d1 <= '0;
            gpa_count_d1 <= '0;
            gpa_vpn_d1 <= '0;
            gpa_stage_d1 <= '0;
            ptw_valid_d1 <= '0;
            ptw_stage_d1 <= '0;
            ptw_s1level_d1 <= '0;
            ptw_s2level_d1 <= '0;
            ptw_s1n_d1 <= '0;
            ptw_s2n_d1 <= '0;
            ptw_s1tag_d1 <= '0;
            ptw_s2tag_d1 <= '0;
            ptw_s1low_d1 <= '0;
            ptw_valididx_d1 <= '0;
            miss_age <= '0;
            miss_pending <= '0;
            miss_vaddr <= '0;
            miss_rob <= '0;
            retry_count <= '0;
            return_seen <= '0;
            last_vaddr <= '0;
            last_fullva <= '0;
            last_paddr <= '0;
            last_rob <= '0;
            last_fault <= '0;
            last_pbmt <= '0;
            last_response_valid <= '0;
            last_bypass <= '0;
            permission_controls_d1 <= '0;
            context_ids_d1 <= '0;
            s2_ug_d1 <= '0;
            return_older <= '0;
            bypass_fast_d1 <= '0;
            ptw_getgpa_d1 <= '0;
            owner_redirected_d1 <= '0;
            redirect_valid_d1 <= '0;
            fast_gpa_start_d1 <= '0;
            gpa_start_d1 <= '0;
            ptw_getgpa_event_d1 <= '0;
            flushed_epoch <= '0;
            refill_seen <= '0;
        end else begin
            req_valid_d1 <= req_valid;
            req_kill_d1 <= req_kill;
            req_late_kill_d1 <= req_late_kill;
            req_vaddr_d1 <= req_vaddr;
            req_fullva_d1 <= req_fullva;
            req_checkfullva_d1 <= req_checkfullva;
            req_cmd_d1 <= req_cmd;
            req_hyperinst_d1 <= req_hyperinst;
            req_hlvx_d1 <= req_hlvx;
            req_prefetch_d1 <= req_prefetch;
            req_no_translate_d1 <= req_no_translate;
            req_pmpaddr_d1 <= req_pmpaddr;
            req_mabuf_d1 <= req_mabuf;
            req_rob_d1 <= req_rob;
            req_effective_va_d1 <= req_effective_va;
            req_pre_s1_d1 <= req_pre_s1;
            req_pre_s2_d1 <= req_pre_s2;
            req_stage_d1 <= req_stage;
            req_pmm_d1 <= req_pmm;
            req_prepf_d1 <= req_prepf;
            req_pregpf_d1 <= req_pregpf;
            req_preaf_d1 <= req_preaf;
            req_virt_d1 <= req_virt;
            req_lookup_d1 <= req_lookup;
            csr_satp_mode_d1 <= csr_satp_mode;
            csr_asid_d1 <= csr_asid;
            csr_satp_changed_d1 <= csr_satp_changed;
            csr_vsatp_mode_d1 <= csr_vsatp_mode;
            csr_vasid_d1 <= csr_vasid;
            csr_vsatp_changed_d1 <= csr_vsatp_changed;
            csr_hgatp_mode_d1 <= csr_hgatp_mode;
            csr_vmid_d1 <= csr_vmid;
            csr_hgatp_changed_d1 <= csr_hgatp_changed;
            csr_mxr_d1 <= csr_mxr;
            csr_sum_d1 <= csr_sum;
            csr_vmxr_d1 <= csr_vmxr;
            csr_vsum_d1 <= csr_vsum;
            csr_virt_d1 <= csr_virt;
            csr_virt_changed_d1 <= csr_virt_changed;
            csr_spvp_d1 <= csr_spvp;
            csr_imode_d1 <= csr_imode;
            csr_dmode_d1 <= csr_dmode;
            csr_bme_d1 <= csr_bme;
            csr_cmode_d1 <= csr_cmode;
            csr_mseccfg_d1 <= csr_mseccfg;
            csr_menvcfg_d1 <= csr_menvcfg;
            csr_henvcfg_d1 <= csr_henvcfg;
            csr_hstatus_d1 <= csr_hstatus;
            csr_senvcfg_d1 <= csr_senvcfg;
            history_valid <= 1'b1;
            req_valid_d2 <= req_valid_d1;
            req_vaddr_d2 <= req_vaddr_d1;
            accepted_run_length_d1 <= accepted_run_length;
            if (req_valid && !req_kill)
                accepted_run_length <= accepted_run_length == 32'hffffffff ? accepted_run_length :
                    accepted_run_length + 1;
            else
                accepted_run_length <= 0;
            gpa_ng_d1 <= gpa_ng;
            gpa_ready_d1 <= gpa_response_ready;
            gpa_count_d1 <= gpa_count;
            gpa_vpn_d1 <= gpa_vpn;
            gpa_stage_d1 <= gpa_stage;
            ptw_valid_d1 <= ptw_valid;
            ptw_stage_d1 <= ptw_stage;
            ptw_s1level_d1 <= ptw_s1level;
            ptw_s2level_d1 <= ptw_s2level;
            ptw_s1n_d1 <= ptw_s1n;
            ptw_s2n_d1 <= ptw_s2n;
            ptw_s1tag_d1 <= ptw_s1tag;
            ptw_s2tag_d1 <= ptw_s2tag;
            ptw_s1low_d1 <= ptw_s1low;
            ptw_valididx_d1 <= ptw_valididx;
            previous_return_matches <= current_return_matches;
            return_older <= previous_return_matches;
            bypass_fast_d1 <= ptw_bypass_hit_fast;
            ptw_getgpa_d1 <= ptw_getgpa;
            owner_redirected_d1 <= owner_redirected;
            redirect_valid_d1 <= redirect_valid;
            fast_gpa_start_d1 <= gpa_start && resp_can_gpa && ptw_bypass_hit_fast;
            gpa_start_d1 <= gpa_start;
            ptw_getgpa_event_d1 <= ptw_valid && ptw_getgpa;
            permission_controls_d1 <= {csr_sum, csr_mxr, csr_vsum, csr_vmxr};
            context_ids_d1 <= {csr_asid, csr_vasid, csr_vmid};
            s2_ug_d1 <= {ptw_s2_u, ptw_s2_g};
            if (csr_satp_changed || csr_vsatp_changed || csr_hgatp_changed || csr_virt_changed)
                flushed_epoch <= 1'b1;
            if (gpa_write)
                refill_seen <= 1'b1;
            if (response_sample) begin
                last_vaddr <= req_vaddr_d1;
                last_fullva <= req_fullva_d1;
                last_paddr <= resp_paddr_0;
                last_rob <= req_rob_d1;
                last_fault <= actual_fault;
                last_pbmt <= resp_pbmt;
                last_bypass <= ptw_bypass_hit;
                last_response_valid <= 1'b1;
                if (resp_miss) begin
                    miss_pending <= 1'b1;
                    miss_vaddr <= req_vaddr_d1;
                    miss_rob <= req_rob_d1;
                    retry_count <= miss_pending ? retry_count + 1 : 1;
                end else if (miss_pending && miss_vaddr == req_vaddr_d1 && miss_rob == req_rob_d1) begin
                    miss_pending <= 1'b0;
                end
            end
            if (miss_pending) begin
                if (miss_age < 32'd256)
                    miss_age <= miss_age + 1;
                else
                    miss_pending <= 1'b0;
            end else begin
                miss_age <= 0;
            end
            if (current_return_matches)
                return_seen <= 1'b1;
        end
    end
    wire [11:0] page_offset_0 = req_vaddr_d1[11:0];
    wire [20:0] page_offset_1 = req_vaddr_d1[20:0];
    wire [29:0] page_offset_2 = req_vaddr_d1[29:0];
    wire [38:0] page_offset_3 = req_vaddr_d1[38:0];
    always @* begin
        selected_level = ptw_bypass_hit ? ptw_s1level_d1 : 2'd0;
        selected_napot = ptw_bypass_hit ? ptw_s1n_d1 : 1'b0;
        if (!ptw_bypass_hit) begin
            for (integer way = 0; way < 48; way = way + 1) begin
                if (storage_hit_vector[way]) begin
                    selected_level = selected_level | storage_levels[way];
                    selected_napot = selected_napot | storage_napot[way];
                end
            end
        end
    end
    // 诊断独立统计，不计入功能分母，不调用UVM错误报告。
    covergroup cg_interface_diagnostics @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_request_response : coverpoint ($isunknown(resp_valid && req_out_cmd == req_cmd_d1 &&
            req_out_vaddr == req_vaddr_d1) ? 2'd2 : (resp_valid && req_out_cmd == req_cmd_d1 &&
            req_out_vaddr == req_vaddr_d1) ? 2'd0 : 2'd1)
            iff (sample_enable && (request_due)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_translation_fault : coverpoint ($isunknown(fault_matches) ? 2'd2 : (fault_matches) ? 2'd0 : 2'd1)
            iff (sample_enable && (response_sample)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_address_mapping : coverpoint ($isunknown(address_matches) ? 2'd2 : (address_matches) ? 2'd0 : 2'd1)
            iff (sample_enable && (response_sample && !resp_miss && actual_fault == 0)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
        cp_pbmt_selection : coverpoint ($isunknown(resp_pbmt == expected_pbmt) ? 2'd2 : (resp_pbmt == expected_pbmt)
            ? 2'd0 : 2'd1)
            iff (sample_enable && (response_sample && response_hit)) {
            bins b_mismatch = {2'd1};
            bins b_unknown = {2'd2};
        }
    endgroup
    cg_interface_diagnostics interface_diagnostics;
    initial begin
        interface_diagnostics = new();
    end

    // 01.01 来源选择与共享端口
    covergroup cg_dtlb_01_01 @(posedge clock);
        option.per_instance = 1;
        // 01.01.001 REQ_命令类别_cmd0
        cp_01_01_001 : coverpoint req_cmd_d1
            iff (sample_enable && ((request_due && req_cmd_d1 == 0)) && (((resp_valid && req_out_cmd ==
                req_cmd_d1)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_command = {3'd0};
        }
        // 01.01.002 REQ_命令类别_cmd1
        cp_01_01_002 : coverpoint req_cmd_d1
            iff (sample_enable && ((request_due && req_cmd_d1 == 1)) && (((resp_valid && req_out_cmd ==
                req_cmd_d1)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_command = {3'd1};
        }
        // 01.01.003 REQ_命令类别_cmd4
        cp_01_01_003 : coverpoint req_cmd_d1
            iff (sample_enable && ((request_due && req_cmd_d1 == 4)) && (((resp_valid && req_out_cmd ==
                req_cmd_d1)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_command = {3'd4};
        }
        // 01.01.004 REQ_命令类别_cmd5
        cp_01_01_004 : coverpoint req_cmd_d1
            iff (sample_enable && ((request_due && req_cmd_d1 == 5)) && (((resp_valid && req_out_cmd ==
                req_cmd_d1)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_command = {3'd5};
        }
    endgroup
    cg_dtlb_01_01 cg_dtlb_01_01_inst;

    // 01.02 请求接受与流水节拍
    covergroup cg_dtlb_01_02 @(posedge clock);
        option.per_instance = 1;
        // 01.02.002 REQ_单拍请求
        cp_01_02_002 : coverpoint (resp_valid && req_out_vaddr == req_vaddr_d1 && req_out_cmd == req_cmd_d1)
            iff (sample_enable && ((request_due && !req_valid_d2))) {
            bins b_expected_result = {1'b1};
        }
        // 01.02.004 REQ_气泡传播
        cp_01_02_004 : coverpoint (!resp_valid)
            iff (sample_enable && ((req_valid_d2 && !req_valid_d1 && req_valid))) {
            bins b_expected_result = {1'b1};
        }
        // 01.02.006 REQ_无效拍payload变化
        cp_01_02_006 : coverpoint (!resp_valid)
            iff (sample_enable && ((history_valid && !req_valid_d1 && req_vaddr_d1 != req_vaddr_d2))) {
            bins b_expected_result = {1'b1};
        }
        // 01.02.001 REQ_复位空闲
        cp_01_02_001 : coverpoint resp_valid
            iff (sample_enable && (history_valid && !req_valid && !resp_valid) && ((!resp_valid) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 01.02.003 REQ_连续吞吐
        cp_01_02_003 : coverpoint accepted_run_length_d1
            iff (sample_enable && (accepted_run_length_d1 >= 2 && !req_valid) && ((!request_due ||
                resp_valid) === 1'b1)) {
            bins b_two = {2};
            bins b_three = {3};
            bins b_four_to_seven = {[4:7]};
            bins b_eight_or_more = {[8:$]};
        }
    endgroup
    cg_dtlb_01_02 cg_dtlb_01_02_inst;

    // 01.03 取消请求与后拍抑制
    covergroup cg_dtlb_01_03 @(posedge clock);
        option.per_instance = 1;
        // 01.03.001 REQ_请求拍kill
        cp_01_03_001 : coverpoint (!resp_valid)
            iff (sample_enable && ((req_valid_d1 && req_kill_d1))) {
            bins b_expected_result = {1'b1};
        }
        // 01.03.002 REQ_后拍req_kill
        cp_01_03_002 : coverpoint (!resp_qvalid && resp_replay)
            iff (sample_enable && ((request_due && req_late_kill))) {
            bins b_expected_result = {1'b1};
        }
        // 01.03.003 REQ_kill与hit_miss交叉
        cp_01_03_003 : coverpoint kill_hit_class
            iff (sample_enable && (req_valid_d1 && (req_kill_d1 || req_late_kill)) &&
                ((req_kill_d1 ? !resp_valid : !resp_qvalid) === 1'b1)) {
            bins b_kill_storage = {0};
            bins b_kill_bypass = {1};
            bins b_kill_miss = {2};
            bins b_late_storage = {3};
            bins b_late_bypass = {4};
            bins b_late_miss = {5};
        }
        // 01.03.004 REQ_kill仍可触发storage读
        cp_01_03_004 : coverpoint resp_valid
            iff (sample_enable && (req_valid_d1 && req_kill_d1 && storage_hit) && ((!resp_valid) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
    endgroup
    cg_dtlb_01_03 cg_dtlb_01_03_inst;

    // 01.04 响应关联与副本
    covergroup cg_dtlb_01_04 @(posedge clock);
        option.per_instance = 1;
        // 01.04.001 REQ_响应副本
        cp_01_04_001 : coverpoint (resp_paddr_0 == resp_paddr_1)
            iff (sample_enable && ((response_sample && !resp_miss && GROUP_ID == 0 && PORT_INDEX<3))) {
            bins b_expected_result = {1'b1};
        }
        // 01.04.002 FCOV_请求响应关联
        cp_01_04_002 : coverpoint (resp_valid && req_out_vaddr == req_vaddr_d1 && req_out_cmd == req_cmd_d1)
            iff (sample_enable && (request_due)) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_01_04 cg_dtlb_01_04_inst;

    // 01.05 请求控制与ROB关系
    covergroup cg_dtlb_01_05 @(posedge clock);
        option.per_instance = 1;
        // 01.05.002 ROB_redirect_valid0
        cp_01_05_002 : coverpoint (response_sample && !redirect_valid)
            iff (sample_enable && (response_sample && !redirect_valid)) {
            bins b_observed = {1'b1};
        }
        // 01.05.003 ROB_redirect_valid1
        cp_01_05_003 : coverpoint (response_sample && redirect_valid)
            iff (sample_enable && (response_sample && redirect_valid)) {
            bins b_observed = {1'b1};
        }
        // 01.05.004 ROB_redirect_equal_level0
        cp_01_05_004 : coverpoint (response_sample && redirect_valid && redirect_relation == 0 && redirect_level == 0)
            iff (sample_enable && (response_sample && redirect_valid && redirect_relation == 0 &&
                redirect_level == 0)) {
            bins b_observed = {1'b1};
        }
        // 01.05.005 ROB_redirect_equal_level1
        cp_01_05_005 : coverpoint (response_sample && redirect_valid && redirect_relation == 0 && redirect_level == 1)
            iff (sample_enable && (response_sample && redirect_valid && redirect_relation == 0 &&
                redirect_level == 1)) {
            bins b_observed = {1'b1};
        }
        // 01.05.006 ROB_redirect_older_level0
        cp_01_05_006 : coverpoint (response_sample && redirect_valid && redirect_relation == 1 && redirect_level == 0)
            iff (sample_enable && (response_sample && redirect_valid && redirect_relation == 1 &&
                redirect_level == 0)) {
            bins b_observed = {1'b1};
        }
        // 01.05.007 ROB_redirect_older_level1
        cp_01_05_007 : coverpoint (response_sample && redirect_valid && redirect_relation == 1 && redirect_level == 1)
            iff (sample_enable && (response_sample && redirect_valid && redirect_relation == 1 &&
                redirect_level == 1)) {
            bins b_observed = {1'b1};
        }
        // 01.05.008 ROB_redirect_younger_level0
        cp_01_05_008 : coverpoint (response_sample && redirect_valid && redirect_relation == 2 && redirect_level == 0)
            iff (sample_enable && (response_sample && redirect_valid && redirect_relation == 2 &&
                redirect_level == 0)) {
            bins b_observed = {1'b1};
        }
        // 01.05.009 ROB_redirect_younger_level1
        cp_01_05_009 : coverpoint (response_sample && redirect_valid && redirect_relation == 2 && redirect_level == 1)
            iff (sample_enable && (response_sample && redirect_valid && redirect_relation == 2 &&
                redirect_level == 1)) {
            bins b_observed = {1'b1};
        }
    endgroup
    cg_dtlb_01_05 cg_dtlb_01_05_inst;

    // 01.05 请求控制与ROB关系
    covergroup cg_dtlb_diag_01_05 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 01.05.001 请求命令_保留或非数据命令：
        // 测试点表明确要求“不生成运行期 coverpoint”，这里只保留编号说明。
    endgroup
    cg_dtlb_diag_01_05 cg_dtlb_diag_01_05_inst;

    // 02.01 Host翻译与直通
    covergroup cg_dtlb_02_01 @(posedge clock);
        option.per_instance = 1;
        // 02.01.001 MODE_host_bare
        cp_02_01_001 : coverpoint (!translate_enable)
            iff (sample_enable && ((response_sample && !resp_virt && !req_hyperinst_d1 && csr_satp_mode == 0 &&
                !req_no_translate_d1))) {
            bins b_expected_result = {1'b1};
        }
        // 02.01.002 MODE_host_sv39
        cp_02_01_002 : coverpoint csr_satp_mode
            iff (sample_enable &&
                ((response_sample && !resp_virt && !req_hyperinst_d1 && csr_satp_mode == 8 && effective_mode != 3
                    && !req_no_translate_d1)) &&
                (((translate_enable && req_out_s2xlate == 0)) === 1'b1)) {
            bins b_sv39 = {4'd8};
        }
        // 02.01.003 MODE_host_sv48
        cp_02_01_003 : coverpoint csr_satp_mode
            iff (sample_enable &&
                ((response_sample && !resp_virt && !req_hyperinst_d1 && csr_satp_mode == 9 && effective_mode != 3
                    && !req_no_translate_d1)) &&
                (((translate_enable && req_out_s2xlate == 0)) === 1'b1)) {
            bins b_sv48 = {4'd9};
        }
        // 02.01.004 MODE_M模式旁路
        cp_02_01_004 : coverpoint (!translate_enable)
            iff (sample_enable && ((response_sample && !resp_virt && !req_hyperinst_d1 && effective_mode == 3 &&
                    csr_satp_mode inside {8, 9}))) {
            bins b_expected_result = {1'b1};
        }
        // 02.01.005 MODE_no_translate
        cp_02_01_005 : coverpoint (!translate_enable && resp_paddr_0 == req_pmpaddr && resp_pbmt == 0)
            iff (sample_enable && ((response_sample && req_no_translate_d1 && (req_pre_s1_d1 || req_pre_s2_d1)))) {
            bins b_expected_result = {1'b1};
        }
        // 02.01.006 MODE_直通地址截位
        cp_02_01_006 : coverpoint (resp_paddr_0 == req_vaddr_d1[47:0])
            iff (sample_enable && ((response_sample && !translate_enable && !pre_fault && !req_no_translate_d1))) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_02_01 cg_dtlb_02_01_inst;

    // 02.02 Guest与Hyper翻译选择
    covergroup cg_dtlb_02_02 @(posedge clock);
        option.per_instance = 1;
        // 02.02.001 MODE_onlyS1
        cp_02_02_001 : coverpoint req_out_s2xlate
            iff (sample_enable &&
                ((response_sample && (resp_virt || req_hyperinst_d1) && csr_vsatp_mode inside {8, 9} &&
                    csr_hgatp_mode == 0 && effective_mode != 3 && !req_no_translate_d1))
 && (((req_out_s2xlate == 1 && translate_enable)) === 1'b1)) {
            bins b_translation_stage = {2'd1};
        }
        // 02.02.002 MODE_onlyS2
        cp_02_02_002 : coverpoint req_out_s2xlate
            iff (sample_enable &&
                ((response_sample && (resp_virt || req_hyperinst_d1) && csr_vsatp_mode == 0 &&
                    csr_hgatp_mode inside {8, 9} && effective_mode != 3 && !req_no_translate_d1))
 && (((req_out_s2xlate == 2 && translate_enable)) === 1'b1)) {
            bins b_translation_stage = {2'd2};
        }
        // 02.02.003 MODE_allStage
        cp_02_02_003 : coverpoint req_out_s2xlate
            iff (sample_enable &&
                ((response_sample && (resp_virt || req_hyperinst_d1) && csr_vsatp_mode inside {8, 9} &&
                    csr_hgatp_mode inside {8, 9} && effective_mode != 3 && !req_no_translate_d1))
 && (((req_out_s2xlate == 3 && translate_enable)) === 1'b1)) {
            bins b_translation_stage = {2'd3};
        }
        // 02.02.004 MODE_guest双Bare
        cp_02_02_004 : coverpoint req_out_s2xlate
            iff (sample_enable &&
                ((response_sample && (resp_virt || req_hyperinst_d1) && csr_vsatp_mode == 0 && csr_hgatp_mode == 0)) &&
                (((req_out_s2xlate == 2 && !translate_enable)) === 1'b1)) {
            bins b_translation_stage = {2'd2};
        }
        // 02.02.005 MODE_hyperinst有效模式
        cp_02_02_005 : coverpoint ((effective_mode == {1'b0, csr_spvp} && (GROUP_ID == 2 || PORT_INDEX == 3 ||
            resp_hyper)))
            iff (sample_enable && ((response_sample && req_hyperinst_d1))) {
            bins b_expected_result = {1'b1};
        }
        // 02.02.006 MODE_virt锁存
        cp_02_02_006 : coverpoint (resp_virt == req_virt_d1)
            iff (sample_enable && ((request_due && req_virt_d1 != req_virt))) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_02_02 cg_dtlb_02_02_inst;

    // 02.03 地址合法性与预异常
    covergroup cg_dtlb_02_03 @(posedge clock);
        option.per_instance = 1;
        // 02.03.001 VA_Sv39_合法边界_stage0_cmd0
        cp_02_03_001 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 0 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_satp_mode_d1 == 8 && (req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.002 VA_Sv39_合法边界_stage0_cmd1
        cp_02_03_002 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 0 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_satp_mode_d1 == 8 && (req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.003 VA_Sv39_合法边界_stage1_cmd0
        cp_02_03_003 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 1 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 8 && (req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.004 VA_Sv39_合法边界_stage1_cmd1
        cp_02_03_004 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 1 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 8 && (req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.005 VA_Sv39_合法边界_stage3_cmd0
        cp_02_03_005 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 3 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 8 && (req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.006 VA_Sv39_合法边界_stage3_cmd1
        cp_02_03_006 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 3 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 8 && (req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.007 VA_Sv39_高位非法_stage0_cmd0
        cp_02_03_007 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 0 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_satp_mode_d1 == 8 && !(req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.008 VA_Sv39_高位非法_stage0_cmd1
        cp_02_03_008 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 0 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_satp_mode_d1 == 8 && !(req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.009 VA_Sv39_高位非法_stage1_cmd0
        cp_02_03_009 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 1 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 8 && !(req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.010 VA_Sv39_高位非法_stage1_cmd1
        cp_02_03_010 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 1 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 8 && !(req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.011 VA_Sv39_高位非法_stage3_cmd0
        cp_02_03_011 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 3 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 8 && !(req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.012 VA_Sv39_高位非法_stage3_cmd1
        cp_02_03_012 : coverpoint va_boundary_39
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 3 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 8 && !(req_effective_va_d1 ==
                    {{25{req_effective_va_d1[38]}}, req_effective_va_d1[38:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.013 VA_Sv48_合法边界_stage0_cmd0
        cp_02_03_013 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 0 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_satp_mode_d1 == 9 && (req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.014 VA_Sv48_合法边界_stage0_cmd1
        cp_02_03_014 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 0 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_satp_mode_d1 == 9 && (req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.015 VA_Sv48_合法边界_stage1_cmd0
        cp_02_03_015 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 1 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 9 && (req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.016 VA_Sv48_合法边界_stage1_cmd1
        cp_02_03_016 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 1 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 9 && (req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.017 VA_Sv48_合法边界_stage3_cmd0
        cp_02_03_017 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 3 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 9 && (req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.018 VA_Sv48_合法边界_stage3_cmd1
        cp_02_03_018 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 3 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 9 && (req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_negative_limit = {2};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.019 VA_Sv48_高位非法_stage0_cmd0
        cp_02_03_019 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 0 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_satp_mode_d1 == 9 && !(req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.020 VA_Sv48_高位非法_stage0_cmd1
        cp_02_03_020 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 0 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_satp_mode_d1 == 9 && !(req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.021 VA_Sv48_高位非法_stage1_cmd0
        cp_02_03_021 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 1 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 9 && !(req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.022 VA_Sv48_高位非法_stage1_cmd1
        cp_02_03_022 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 1 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 9 && !(req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.023 VA_Sv48_高位非法_stage3_cmd0
        cp_02_03_023 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 3 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 9 && !(req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.024 VA_Sv48_高位非法_stage3_cmd1
        cp_02_03_024 : coverpoint va_boundary_48
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 3 && (req_pre_s1_d1 ||
                    req_pre_s2_d1) && csr_vsatp_mode_d1 == 9 && !(req_effective_va_d1 ==
                    {{16{req_effective_va_d1[47]}}, req_effective_va_d1[47:0]})))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.025 VA_Sv39x4_合法边界_stage2_cmd0
        cp_02_03_025 : coverpoint va_boundary_41_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 2 && req_pre_s2_d1 &&
                    csr_hgatp_mode_d1 == 8 && (req_effective_va_d1[63:41] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.026 VA_Sv39x4_合法边界_stage2_cmd1
        cp_02_03_026 : coverpoint va_boundary_41_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 2 && req_pre_s2_d1 &&
                    csr_hgatp_mode_d1 == 8 && (req_effective_va_d1[63:41] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.027 VA_Sv39x4_高位非法_stage2_cmd0
        cp_02_03_027 : coverpoint va_boundary_41_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 2 && req_pre_s2_d1 &&
                    csr_hgatp_mode_d1 == 8 && !(req_effective_va_d1[63:41] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.028 VA_Sv39x4_高位非法_stage2_cmd1
        cp_02_03_028 : coverpoint va_boundary_41_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 2 && req_pre_s2_d1 &&
                    csr_hgatp_mode_d1 == 8 && !(req_effective_va_d1[63:41] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.029 VA_Sv48x4_合法边界_stage2_cmd0
        cp_02_03_029 : coverpoint va_boundary_50_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 2 && req_pre_s2_d1 &&
                    csr_hgatp_mode_d1 == 9 && (req_effective_va_d1[63:50] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.030 VA_Sv48x4_合法边界_stage2_cmd1
        cp_02_03_030 : coverpoint va_boundary_50_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 2 && req_pre_s2_d1 &&
                    csr_hgatp_mode_d1 == 9 && (req_effective_va_d1[63:50] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.031 VA_Sv48x4_高位非法_stage2_cmd0
        cp_02_03_031 : coverpoint va_boundary_50_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 2 && req_pre_s2_d1 &&
                    csr_hgatp_mode_d1 == 9 && !(req_effective_va_d1[63:50] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.032 VA_Sv48x4_高位非法_stage2_cmd1
        cp_02_03_032 : coverpoint va_boundary_50_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 2 && req_pre_s2_d1 &&
                    csr_hgatp_mode_d1 == 9 && !(req_effective_va_d1[63:50] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.033 VA_BarePA_合法边界_stage0_cmd0
        cp_02_03_033 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 0 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && (req_effective_va_d1[63:48] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.034 VA_BarePA_合法边界_stage0_cmd1
        cp_02_03_034 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 0 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && (req_effective_va_d1[63:48] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.035 VA_BarePA_合法边界_stage1_cmd0
        cp_02_03_035 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 1 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && (req_effective_va_d1[63:48] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.036 VA_BarePA_合法边界_stage1_cmd1
        cp_02_03_036 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 1 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && (req_effective_va_d1[63:48] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.037 VA_BarePA_合法边界_stage2_cmd0
        cp_02_03_037 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 2 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && (req_effective_va_d1[63:48] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.038 VA_BarePA_合法边界_stage2_cmd1
        cp_02_03_038 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 2 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && (req_effective_va_d1[63:48] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.039 VA_BarePA_合法边界_stage3_cmd0
        cp_02_03_039 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 3 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && (req_effective_va_d1[63:48] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.040 VA_BarePA_合法边界_stage3_cmd1
        cp_02_03_040 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 3 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && (req_effective_va_d1[63:48] == 0)))
 && (((resp_valid && fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_zero = {0};
            bins b_positive_limit = {1};
            bins b_adjacent = {3};
            bins b_other_legal = {4};
        }
        // 02.03.041 VA_BarePA_高位非法_stage0_cmd0
        cp_02_03_041 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 0 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && !(req_effective_va_d1[63:48] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.042 VA_BarePA_高位非法_stage0_cmd1
        cp_02_03_042 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 0 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && !(req_effective_va_d1[63:48] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.043 VA_BarePA_高位非法_stage1_cmd0
        cp_02_03_043 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 1 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && !(req_effective_va_d1[63:48] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.044 VA_BarePA_高位非法_stage1_cmd1
        cp_02_03_044 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 1 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && !(req_effective_va_d1[63:48] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.045 VA_BarePA_高位非法_stage2_cmd0
        cp_02_03_045 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 2 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && !(req_effective_va_d1[63:48] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.046 VA_BarePA_高位非法_stage2_cmd1
        cp_02_03_046 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 2 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && !(req_effective_va_d1[63:48] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.047 VA_BarePA_高位非法_stage3_cmd0
        cp_02_03_047 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 0 && req_stage_d1 == 3 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && !(req_effective_va_d1[63:48] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.048 VA_BarePA_高位非法_stage3_cmd1
        cp_02_03_048 : coverpoint va_boundary_48_unsigned
            iff (sample_enable &&
                ((request_due && req_checkfullva_d1 && req_cmd_d1 == 1 && req_stage_d1 == 3 && !req_pre_s1_d1 &&
                    !req_pre_s2_d1 && !(req_effective_va_d1[63:48] == 0)))
 &&
                (((resp_valid && fault_matches && !resp_miss && (!HAS_GPA || (resp_gpaddr == req_effective_va_d1 &&
                    !resp_ext)))) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_adjacent_invalid = {3};
            bins b_other_invalid = {4};
        }
        // 02.03.049 VA_关闭检查
        cp_02_03_049 : coverpoint (!pre_fault)
            iff (sample_enable && ((request_due && HAS_FULLVA && !req_checkfullva_d1 && high_address_bad))) {
            bins b_expected_result = {1'b1};
        }
        // 02.03.050 VA_prefault优先级
        cp_02_03_050 : coverpoint (resp_valid && fault_matches && !resp_miss)
            iff (sample_enable && ((request_due && pre_fault))) {
            bins b_expected_result = {1'b1};
        }
        // 02.03.051 VA_prefault与PTW请求
        cp_02_03_051 : coverpoint (!resp_miss)
            iff (sample_enable && ((response_sample && pre_fault && resp_miss_read))) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_02_03 cg_dtlb_02_03_inst;

    // 02.04 PMM来源与屏蔽长度
    covergroup cg_dtlb_02_04 @(posedge clock);
        option.per_instance = 1;
        // 02.04.001 PMM_HS_PMLEN0
        cp_02_04_001 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 1 && csr_menvcfg_d1 == 0 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.002 PMM_HS_PMLEN7
        cp_02_04_002 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 1 && csr_menvcfg_d1 == 2 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.003 PMM_HS_PMLEN16
        cp_02_04_003 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 1 && csr_menvcfg_d1 == 3 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.004 PMM_HS_HYP_S_PMLEN0
        cp_02_04_004 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 1 && csr_henvcfg_d1 == 0 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.005 PMM_HS_HYP_S_PMLEN7
        cp_02_04_005 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 1 && csr_henvcfg_d1 == 2 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.006 PMM_HS_HYP_S_PMLEN16
        cp_02_04_006 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 1 && csr_henvcfg_d1 == 3 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.007 PMM_HS_HYP_U_PMLEN0
        cp_02_04_007 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 0 && csr_senvcfg_d1 == 0 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.008 PMM_HS_HYP_U_PMLEN7
        cp_02_04_008 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 0 && csr_senvcfg_d1 == 2 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.009 PMM_HS_HYP_U_PMLEN16
        cp_02_04_009 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 0 && csr_senvcfg_d1 == 3 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.010 PMM_HU_HYP_S_PMLEN0
        cp_02_04_010 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 1 && csr_henvcfg_d1 == 0 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.011 PMM_HU_HYP_S_PMLEN7
        cp_02_04_011 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 1 && csr_henvcfg_d1 == 2 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.012 PMM_HU_HYP_S_PMLEN16
        cp_02_04_012 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 1 && csr_henvcfg_d1 == 3 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.013 PMM_HU_HYP_U_PMLEN0
        cp_02_04_013 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 0 && csr_hstatus_d1 == 0 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.014 PMM_HU_HYP_U_PMLEN7
        cp_02_04_014 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 0 && csr_hstatus_d1 == 2 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.015 PMM_HU_HYP_U_PMLEN16
        cp_02_04_015 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 0 && csr_hstatus_d1 == 3 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.016 PMM_M_PMLEN0
        cp_02_04_016 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 3 && csr_mseccfg_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.017 PMM_M_PMLEN7
        cp_02_04_017 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 3 && csr_mseccfg_d1 == 2 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.018 PMM_M_PMLEN16
        cp_02_04_018 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 3 && csr_mseccfg_d1 == 3 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.019 PMM_M_HYP_S_PMLEN0
        cp_02_04_019 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 1 && csr_henvcfg_d1 == 0 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.020 PMM_M_HYP_S_PMLEN7
        cp_02_04_020 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 1 && csr_henvcfg_d1 == 2 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.021 PMM_M_HYP_S_PMLEN16
        cp_02_04_021 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 1 && csr_henvcfg_d1 == 3 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.022 PMM_M_HYP_U_PMLEN0
        cp_02_04_022 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 0 && csr_senvcfg_d1 == 0 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.023 PMM_M_HYP_U_PMLEN7
        cp_02_04_023 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 0 && csr_senvcfg_d1 == 2 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.024 PMM_M_HYP_U_PMLEN16
        cp_02_04_024 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 1 && csr_spvp_d1 == 0 && csr_senvcfg_d1 == 3 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.025 PMM_M_MPRV_S_PMLEN0
        cp_02_04_025 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 1 && csr_menvcfg_d1 == 0 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.026 PMM_M_MPRV_S_PMLEN7
        cp_02_04_026 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 1 && csr_menvcfg_d1 == 2 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.027 PMM_M_MPRV_S_PMLEN16
        cp_02_04_027 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 1 && csr_menvcfg_d1 == 3 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.028 PMM_M_MPRV_U_PMLEN0
        cp_02_04_028 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 0 && csr_senvcfg_d1 == 0 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.029 PMM_M_MPRV_U_PMLEN7
        cp_02_04_029 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 0 && csr_senvcfg_d1 == 2 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.030 PMM_M_MPRV_U_PMLEN16
        cp_02_04_030 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 3 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 0 && csr_senvcfg_d1 == 3 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.031 PMM_U_PMLEN0
        cp_02_04_031 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 0 && csr_senvcfg_d1 == 0 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.032 PMM_U_PMLEN7
        cp_02_04_032 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 0 && csr_senvcfg_d1 == 2 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.033 PMM_U_PMLEN16
        cp_02_04_033 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 0 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 0 && csr_senvcfg_d1 == 3 && csr_mxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.034 PMM_VS_PMLEN0
        cp_02_04_034 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 1 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 1 && csr_henvcfg_d1 == 0 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.035 PMM_VS_PMLEN7
        cp_02_04_035 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 1 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 1 && csr_henvcfg_d1 == 2 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.036 PMM_VS_PMLEN16
        cp_02_04_036 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 1 && csr_virt_d1 == 1 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 1 && csr_henvcfg_d1 == 3 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.037 PMM_VU_PMLEN0
        cp_02_04_037 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 1 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 0 && csr_senvcfg_d1 == 0 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.038 PMM_VU_PMLEN7
        cp_02_04_038 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 1 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 0 && csr_senvcfg_d1 == 2 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
        // 02.04.039 PMM_VU_PMLEN16
        cp_02_04_039 : coverpoint pmm_address_class
            iff (sample_enable && ((request_due && req_hlvx_d1 == 0 && csr_imode_d1 == 0 && csr_virt_d1 == 1 &&
                req_hyperinst_d1 == 0 && csr_dmode_d1 == 0 && csr_senvcfg_d1 == 3 && csr_mxr_d1 == 0 &&
                csr_vmxr_d1 == 0 && HAS_FULLVA)) &&
                (((resp_valid && req_effective_va_d1 == expected_effective_va && (!HAS_FULLVA || resp_fullva ==
                    req_effective_va_d1))) === 1'b1)) {
            bins b_masked_zero = {0};
            bins b_masked_ones = {1};
            bins b_masked_mixed = {2};
        }
    endgroup
    cg_dtlb_02_04 cg_dtlb_02_04_inst;

    // 02.05 PMM优先级与地址协同
    covergroup cg_dtlb_02_05 @(posedge clock);
        option.per_instance = 1;
        // 02.05.001 PMM_HLVX关闭屏蔽
        cp_02_05_001 : coverpoint (req_effective_va_d1 == req_fullva_d1)
            iff (sample_enable && ((request_due && req_hlvx_d1 && raw_pmm inside {2, 3}))) {
            bins b_expected_result = {1'b1};
        }
        // 02.05.002 PMM_MXR关闭屏蔽
        cp_02_05_002 : coverpoint (req_effective_va_d1 == req_fullva_d1)
            iff (sample_enable && ((request_due && csr_dmode_d1 != 3 &&
                (csr_mxr_d1 || ((csr_virt_d1 || req_hyperinst_d1) &&
                    csr_vmxr_d1)) && raw_pmm inside {2, 3}))) {
            bins b_expected_result = {1'b1};
        }
        // 02.05.003 PMM_M优先于MXR
        cp_02_05_003 : coverpoint ((!HAS_FULLVA || resp_fullva == req_effective_va_d1))
            iff (sample_enable && ((request_due && !req_hlvx_d1 && csr_dmode_d1 == 3 && csr_mxr_d1 &&
                csr_mseccfg_d1 inside {2, 3}))) {
            bins b_expected_result = {1'b1};
        }
        // 02.05.005 PMM_vaddr一致性
        cp_02_05_005 : coverpoint ((req_out_vaddr == req_vaddr_d1 && (!HAS_FULLVA || resp_fullva ==
            req_effective_va_d1)))
            iff (sample_enable && ((request_due && req_effective_va_d1 != req_fullva_d1))) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_02_05 cg_dtlb_02_05_inst;

    // 02.05 PMM优先级与地址协同
    covergroup cg_dtlb_diag_02_05 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 02.05.004：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_02_05_004 : coverpoint ($isunknown((req_effective_va_d1 == req_fullva_d1)) ? 2'd2 : ((req_effective_va_d1
            == req_fullva_d1)) ? 2'd0 : 2'd1)
            iff (sample_enable && ((request_due && req_pmm_d1 == 1))) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 02.05.004 PMM_reserved01
        cp_02_05_004 : coverpoint (req_effective_va_d1 == req_fullva_d1)
            iff (sample_enable && ((request_due && req_pmm_d1 == 1))) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_diag_02_05 cg_dtlb_diag_02_05_inst;

    // 03.01 有效项与Tag命中
    covergroup cg_dtlb_03_01 @(posedge clock);
        option.per_instance = 1;
        // 03.01.005 LOOKUP_VA别名同PA
        cp_03_01_005 : coverpoint address_matches
            iff (sample_enable && (response_sample && last_response_valid && last_vaddr[49:12] != req_vaddr_d1[49:12]
 && last_paddr[47:12] == resp_paddr_0[47:12] && !resp_miss)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_03_01 cg_dtlb_03_01_inst;

    // 03.03 基础页与超级页地址拼接
    covergroup cg_dtlb_03_03 @(posedge clock);
        option.per_instance = 1;
        // 03.03.001 PAGE_4KB
        cp_03_03_001 : coverpoint page_offset_0
            iff (sample_enable && (response_sample && response_hit && translate_enable && !resp_miss &&
                selected_level == 0
 && !selected_napot) && ((address_matches) === 1'b1)) {
            bins b_head = {0};
            bins b_tail = {12'hfff};
            bins b_middle = {[12'd1:12'hffe]};
        }
        // 03.03.002 PAGE_2MB
        cp_03_03_002 : coverpoint page_offset_1
            iff (sample_enable && (response_sample && response_hit && translate_enable && !resp_miss &&
                selected_level == 1
 && !selected_napot) && ((address_matches) === 1'b1)) {
            bins b_head = {0};
            bins b_tail = {21'h1fffff};
            bins b_middle = {[21'd1:21'h1ffffe]};
        }
        // 03.03.003 PAGE_1GB
        cp_03_03_003 : coverpoint page_offset_2
            iff (sample_enable && (response_sample && response_hit && translate_enable && !resp_miss &&
                selected_level == 2
 && !selected_napot) && ((address_matches) === 1'b1)) {
            bins b_head = {0};
            bins b_tail = {30'h3fffffff};
            bins b_middle = {[30'd1:30'h3ffffffe]};
        }
        // 03.03.004 PAGE_512GB
        cp_03_03_004 : coverpoint page_offset_3
            iff (sample_enable && (response_sample && response_hit && translate_enable && !resp_miss &&
                selected_level == 3
 && !selected_napot) && ((address_matches) === 1'b1)) {
            bins b_head = {0};
            bins b_tail = {39'h7fffffffff};
            bins b_middle = {[39'd1:39'h7ffffffffe]};
        }
    endgroup
    cg_dtlb_03_03 cg_dtlb_03_03_inst;

    // 03.07 跨页访问与结果分类
    covergroup cg_dtlb_03_07 @(posedge clock);
        option.per_instance = 1;
        // 03.07.001 CROSS_相邻页权限不同
        cp_03_07_001 : coverpoint fault_matches
            iff (sample_enable && (response_sample && last_response_valid && last_rob == req_rob_d1 &&
                last_fullva == req_fullva_d1 && last_vaddr[49:12] + 1 == req_vaddr_d1[49:12])) {
            bins b_expected_set = {1'b1};
        }
        // 03.07.002 FCOV_地址映射结果
        cp_03_07_002 : coverpoint address_matches
            iff (sample_enable && (response_sample && !resp_miss && actual_fault == 0)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_03_07 cg_dtlb_03_07_inst;

    // 04.01 读写权限与A/D
    covergroup cg_dtlb_04_01 @(posedge clock);
        option.per_instance = 1;
        // 04.01.001 S1_read_allow_cmd0
        cp_04_01_001 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 0 && !req_hlvx_d1 &&
                resp_s1_r && resp_s1_a && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.002 S1_read_allow_cmd4
        cp_04_01_002 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 4 && !req_hlvx_d1 &&
                resp_s1_r && resp_s1_a && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.003 S1_read_deny_cmd0
        cp_04_01_003 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 0 && !req_hlvx_d1 &&
                !resp_s1_r && resp_s1_x && resp_s1_a && !effective_mxr && !resp_s1_pf)) &&
                    (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.004 S1_read_deny_cmd4
        cp_04_01_004 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 4 && !req_hlvx_d1 &&
                !resp_s1_r && resp_s1_x && resp_s1_a && !effective_mxr && !resp_s1_pf)) &&
                    (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.005 S1_write_allow_cmd1
        cp_04_01_005 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 1 && !req_hlvx_d1 &&
                resp_s1_w && resp_s1_a && resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.006 S1_write_allow_cmd5
        cp_04_01_006 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 5 && !req_hlvx_d1 &&
                resp_s1_w && resp_s1_a && resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.007 S1_write_deny_cmd1
        cp_04_01_007 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 1 && !req_hlvx_d1 &&
                !resp_s1_w && resp_s1_a && resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.008 S1_write_deny_cmd5
        cp_04_01_008 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 5 && !req_hlvx_d1 &&
                !resp_s1_w && resp_s1_a && resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.009 S1_A0_read_cmd0
        cp_04_01_009 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 0 && !req_hlvx_d1 &&
                resp_s1_r && !resp_s1_a && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.010 S1_A0_read_cmd4
        cp_04_01_010 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 4 && !req_hlvx_d1 &&
                resp_s1_r && !resp_s1_a && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.011 S1_A0_write_cmd1
        cp_04_01_011 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 1 && !req_hlvx_d1 &&
                resp_s1_w && !resp_s1_a && resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.012 S1_A0_write_cmd5
        cp_04_01_012 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 5 && !req_hlvx_d1 &&
                resp_s1_w && !resp_s1_a && resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.013 S1_D0_write_cmd1
        cp_04_01_013 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 1 && !req_hlvx_d1 &&
                resp_s1_w && resp_s1_a && !resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.014 S1_D0_write_cmd5
        cp_04_01_014 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 5 && !req_hlvx_d1 &&
                resp_s1_w && resp_s1_a && !resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.015 S1_A0D0_write_cmd1
        cp_04_01_015 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 1 && !req_hlvx_d1 &&
                resp_s1_w && !resp_s1_a && !resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.016 S1_A0D0_write_cmd5
        cp_04_01_016 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 5 && !req_hlvx_d1 &&
                resp_s1_w && !resp_s1_a && !resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.017 S1_D0_read无影响_cmd0
        cp_04_01_017 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 0 && !req_hlvx_d1 &&
                resp_s1_r && resp_s1_a && !resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.018 S1_D0_read无影响_cmd4
        cp_04_01_018 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 4 && !req_hlvx_d1 &&
                resp_s1_r && resp_s1_a && !resp_s1_d && !resp_s1_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.019 S1_上游fault标记_cmd0
        cp_04_01_019 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 0 && !req_hlvx_d1 &&
                    resp_s1_pf))
 && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.020 S1_上游fault标记_cmd1
        cp_04_01_020 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 1 && !req_hlvx_d1 &&
                    resp_s1_pf))
 && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.021 S1_上游fault标记_cmd4
        cp_04_01_021 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 4 && !req_hlvx_d1 &&
                    resp_s1_pf))
 && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.022 S1_上游fault标记_cmd5
        cp_04_01_022 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s1_enabled && !fake_pte && !nonleaf_pte && mode_permission && req_out_cmd == 5 && !req_hlvx_d1 &&
                    resp_s1_pf))
 && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.023 S2_read_allow_cmd0
        cp_04_01_023 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 0 && !req_hlvx_d1 && resp_s2_r && resp_s2_a &&
                    !resp_s2_pf))
 && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.024 S2_read_allow_cmd4
        cp_04_01_024 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 4 && !req_hlvx_d1 && resp_s2_r && resp_s2_a &&
                    !resp_s2_pf))
 && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.025 S2_read_deny_cmd0
        cp_04_01_025 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 0 && !req_hlvx_d1 &&
                !resp_s2_r && resp_s2_x && resp_s2_a && !csr_mxr && !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.026 S2_read_deny_cmd4
        cp_04_01_026 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 4 && !req_hlvx_d1 &&
                !resp_s2_r && resp_s2_x && resp_s2_a && !csr_mxr && !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.027 S2_write_allow_cmd1
        cp_04_01_027 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 1 && !req_hlvx_d1 && resp_s2_w && resp_s2_a &&
                    resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.028 S2_write_allow_cmd5
        cp_04_01_028 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 5 && !req_hlvx_d1 && resp_s2_w && resp_s2_a &&
                    resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.029 S2_write_deny_cmd1
        cp_04_01_029 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 1 && !req_hlvx_d1 && !resp_s2_w && resp_s2_a &&
                    resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.030 S2_write_deny_cmd5
        cp_04_01_030 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 5 && !req_hlvx_d1 && !resp_s2_w && resp_s2_a &&
                    resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.031 S2_A0_read_cmd0
        cp_04_01_031 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 0 && !req_hlvx_d1 && resp_s2_r && !resp_s2_a &&
                    !resp_s2_pf))
 && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.032 S2_A0_read_cmd4
        cp_04_01_032 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 4 && !req_hlvx_d1 && resp_s2_r && !resp_s2_a &&
                    !resp_s2_pf))
 && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.033 S2_A0_write_cmd1
        cp_04_01_033 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 1 && !req_hlvx_d1 && resp_s2_w && !resp_s2_a &&
                    resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.034 S2_A0_write_cmd5
        cp_04_01_034 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 5 && !req_hlvx_d1 && resp_s2_w && !resp_s2_a &&
                    resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.035 S2_D0_write_cmd1
        cp_04_01_035 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 1 && !req_hlvx_d1 && resp_s2_w && resp_s2_a &&
                    !resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.036 S2_D0_write_cmd5
        cp_04_01_036 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 5 && !req_hlvx_d1 && resp_s2_w && resp_s2_a &&
                    !resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.037 S2_A0D0_write_cmd1
        cp_04_01_037 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 1 && !req_hlvx_d1 && resp_s2_w && !resp_s2_a &&
                    !resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.038 S2_A0D0_write_cmd5
        cp_04_01_038 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 5 && !req_hlvx_d1 && resp_s2_w && !resp_s2_a &&
                    !resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.039 S2_D0_read无影响_cmd0
        cp_04_01_039 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 0 && !req_hlvx_d1 && resp_s2_r && resp_s2_a &&
                    !resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.040 S2_D0_read无影响_cmd4
        cp_04_01_040 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 4 && !req_hlvx_d1 && resp_s2_r && resp_s2_a &&
                    !resp_s2_d &&
                !resp_s2_pf)) && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.041 S2_上游fault标记_cmd0
        cp_04_01_041 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 0 && !req_hlvx_d1 && resp_s2_pf)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.042 S2_上游fault标记_cmd1
        cp_04_01_042 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 1 && !req_hlvx_d1 && resp_s2_pf)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.043 S2_上游fault标记_cmd4
        cp_04_01_043 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 4 && !req_hlvx_d1 && resp_s2_pf)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.01.044 S2_上游fault标记_cmd5
        cp_04_01_044 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && !access_fault &&
                s2_enabled && !s1_page_fault && req_out_cmd == 5 && !req_hlvx_d1 && resp_s2_pf)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 0 && PORT_INDEX == 0) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
    endgroup
    cg_dtlb_04_01 cg_dtlb_04_01_inst;

    // 04.02 特权与动态权限控制
    covergroup cg_dtlb_04_02 @(posedge clock);
        option.per_instance = 1;
        // 04.02.002 S1_PRIV_U访U_cmd0
        cp_04_02_002 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && s1_enabled && !access_fault && !fake_pte &&
                    !nonleaf_pte && effective_mode == 0 && resp_s1_u
 && req_out_cmd == 0 && resp_s1_a && !resp_s1_pf && resp_s1_r && !req_hlvx_d1)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.003 S1_PRIV_U访U_cmd1
        cp_04_02_003 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && s1_enabled && !access_fault && !fake_pte &&
                    !nonleaf_pte && effective_mode == 0 && resp_s1_u
 && req_out_cmd == 1 && resp_s1_a && !resp_s1_pf && resp_s1_w && resp_s1_d)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.004 S1_PRIV_U访S_cmd0
        cp_04_02_004 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && s1_enabled && !access_fault && !fake_pte &&
                    !nonleaf_pte && effective_mode == 0 && !resp_s1_u
 && req_out_cmd == 0 && resp_s1_a && !resp_s1_pf && resp_s1_r && !req_hlvx_d1)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.005 S1_PRIV_U访S_cmd1
        cp_04_02_005 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && s1_enabled && !access_fault && !fake_pte &&
                    !nonleaf_pte && effective_mode == 0 && !resp_s1_u
 && req_out_cmd == 1 && resp_s1_a && !resp_s1_pf && resp_s1_w && resp_s1_d)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.006 S1_PRIV_S访S_cmd0
        cp_04_02_006 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && s1_enabled && !access_fault && !fake_pte &&
                    !nonleaf_pte && effective_mode == 1 && !resp_s1_u
 && req_out_cmd == 0 && resp_s1_a && !resp_s1_pf && resp_s1_r && !req_hlvx_d1)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.007 S1_PRIV_S访S_cmd1
        cp_04_02_007 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && s1_enabled && !access_fault && !fake_pte &&
                    !nonleaf_pte && effective_mode == 1 && !resp_s1_u
 && req_out_cmd == 1 && resp_s1_a && !resp_s1_pf && resp_s1_w && resp_s1_d)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.008 S1_PRIV_S访U_SUM0_cmd0
        cp_04_02_008 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && s1_enabled && !access_fault && !fake_pte &&
                    !nonleaf_pte && effective_mode == 1 && resp_s1_u && !effective_sum
 && req_out_cmd == 0 && resp_s1_a && !resp_s1_pf && resp_s1_r && !req_hlvx_d1)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.009 S1_PRIV_S访U_SUM0_cmd1
        cp_04_02_009 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && s1_enabled && !access_fault && !fake_pte &&
                    !nonleaf_pte && effective_mode == 1 && resp_s1_u && !effective_sum
 && req_out_cmd == 1 && resp_s1_a && !resp_s1_pf && resp_s1_w && resp_s1_d)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.010 S1_PRIV_S访U_SUM1_cmd0
        cp_04_02_010 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && s1_enabled && !access_fault && !fake_pte &&
                    !nonleaf_pte && effective_mode == 1 && resp_s1_u && effective_sum
 && req_out_cmd == 0 && resp_s1_a && !resp_s1_pf && resp_s1_r && !req_hlvx_d1)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.011 S1_PRIV_S访U_SUM1_cmd1
        cp_04_02_011 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && s1_enabled && !access_fault && !fake_pte &&
                    !nonleaf_pte && effective_mode == 1 && resp_s1_u && effective_sum
 && req_out_cmd == 1 && resp_s1_a && !resp_s1_pf && resp_s1_w && resp_s1_d)) &&
                (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.012 MXR_guest_00
        cp_04_02_012 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && (resp_virt || req_hyperinst_d1) && read_command
                    && !req_hlvx_d1 && translate_enable && mode_permission && !access_fault && !fake_pte &&
                    !nonleaf_pte && csr_mxr == 0 && csr_vmxr == 0 && !resp_s1_r && resp_s1_x && resp_s1_a &&
                    !resp_s1_pf && !resp_s2_r && resp_s2_x && resp_s2_a && !resp_s2_pf))
 && (((fault_matches)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.013 MXR_guest_01
        cp_04_02_013 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && (resp_virt || req_hyperinst_d1) && read_command
                    && !req_hlvx_d1 && translate_enable && mode_permission && !access_fault && !fake_pte &&
                    !nonleaf_pte && csr_mxr == 0 && csr_vmxr == 1 && !resp_s1_r && resp_s1_x && resp_s1_a &&
                    !resp_s1_pf && !resp_s2_r && resp_s2_x && resp_s2_a && !resp_s2_pf))
 && (((fault_matches)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.014 MXR_guest_10
        cp_04_02_014 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && (resp_virt || req_hyperinst_d1) && read_command
                    && !req_hlvx_d1 && translate_enable && mode_permission && !access_fault && !fake_pte &&
                    !nonleaf_pte && csr_mxr == 1 && csr_vmxr == 0 && !resp_s1_r && resp_s1_x && resp_s1_a &&
                    !resp_s1_pf && !resp_s2_r && resp_s2_x && resp_s2_a && !resp_s2_pf))
 && (((fault_matches)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.015 MXR_guest_11
        cp_04_02_015 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && (resp_virt || req_hyperinst_d1) && read_command
                    && !req_hlvx_d1 && translate_enable && mode_permission && !access_fault && !fake_pte &&
                    !nonleaf_pte && csr_mxr == 1 && csr_vmxr == 1 && !resp_s1_r && resp_s1_x && resp_s1_a &&
                    !resp_s1_pf && !resp_s2_r && resp_s2_x && resp_s2_a && !resp_s2_pf))
 && (((fault_matches)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.016 MXR_host
        cp_04_02_016 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && req_out_s2xlate == 0 && s1_enabled &&
                    read_command && !req_hlvx_d1 && mode_permission && !resp_s1_r && resp_s1_x && resp_s1_a &&
                    !resp_s1_pf && !access_fault && csr_vmxr))
 && (((fault_matches)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.017 HLVX_执行权限读取
        cp_04_02_017 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && req_hyperinst_d1 && req_hlvx_d1 && read_command
                    && translate_enable)) &&
                (((fault_matches)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.018 HLVX_MXR不可替代X
        cp_04_02_018 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && req_hlvx_d1 && read_command && translate_enable
                    && resp_s1_r && !resp_s1_x && effective_mxr && mode_permission && resp_s1_a && !access_fault))
 && (((fault_matches)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.02.001 MODE_CSR权限实时更新
        cp_04_02_001 : coverpoint fault_matches
            iff (sample_enable && (response_sample && last_response_valid && last_vaddr == req_vaddr_d1 &&
                {csr_sum, csr_mxr, csr_vsum, csr_vmxr} != permission_controls_d1)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_04_02 cg_dtlb_04_02_inst;

    // 04.03 阶段使能与无效字段隔离
    covergroup cg_dtlb_04_03 @(posedge clock);
        option.per_instance = 1;
        // 04.03.005 权限阶段使能_stage0_TE0
        cp_04_03_005 : coverpoint (fault_matches)
            iff (sample_enable && ((response_sample && req_out_s2xlate == 0 && translate_enable == 0))) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.006 权限阶段使能_stage0_TE1
        cp_04_03_006 : coverpoint (fault_matches)
            iff (sample_enable && ((response_sample && req_out_s2xlate == 0 && translate_enable == 1))) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.007 权限阶段使能_stage1_TE0
        cp_04_03_007 : coverpoint (fault_matches)
            iff (sample_enable && ((response_sample && req_out_s2xlate == 1 && translate_enable == 0))) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.008 权限阶段使能_stage1_TE1
        cp_04_03_008 : coverpoint (fault_matches)
            iff (sample_enable && ((response_sample && req_out_s2xlate == 1 && translate_enable == 1))) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.009 权限阶段使能_stage2_TE0
        cp_04_03_009 : coverpoint (fault_matches)
            iff (sample_enable && ((response_sample && req_out_s2xlate == 2 && translate_enable == 0))) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.010 权限阶段使能_stage2_TE1
        cp_04_03_010 : coverpoint (fault_matches)
            iff (sample_enable && ((response_sample && req_out_s2xlate == 2 && translate_enable == 1))) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.011 权限阶段使能_stage3_TE0
        cp_04_03_011 : coverpoint (fault_matches)
            iff (sample_enable && ((response_sample && req_out_s2xlate == 3 && translate_enable == 0))) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.012 权限阶段使能_stage3_TE1
        cp_04_03_012 : coverpoint (fault_matches)
            iff (sample_enable && ((response_sample && req_out_s2xlate == 3 && translate_enable == 1))) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.004 PERM_翻译关闭
        cp_04_03_004 : coverpoint (actual_fault == 0)
            iff (sample_enable && ((response_sample && !pre_fault && !translate_enable))) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.001 S2_U_G不参与本地权限
        cp_04_03_001 : coverpoint (actual_fault == last_fault)
            iff (sample_enable && (response_sample && last_response_valid && req_out_s2xlate >= 2 && ptw_valid &&
                {ptw_s2_u, ptw_s2_g} != s2_ug_d1 && req_out_vaddr == last_vaddr)) {
            bins b_expected_result = {1'b1};
        }
        // 04.03.002 PERM_S1无效stage不影响
        cp_04_03_002 : coverpoint fault_matches
            iff (sample_enable && (response_sample && translate_enable && req_out_s2xlate == 2)) {
            bins b_expected_set = {1'b1};
        }
        // 04.03.003 PERM_S2无效stage不影响
        cp_04_03_003 : coverpoint fault_matches
            iff (sample_enable && (response_sample && translate_enable && req_out_s2xlate < 2)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_04_03 cg_dtlb_04_03_inst;

    // 04.04 异常优先级与PTE类型
    covergroup cg_dtlb_04_04 @(posedge clock);
        option.per_instance = 1;
        // 04.04.013 FAULT_V0并非本地invalidPTE判定
        cp_04_04_013 : coverpoint (fault_matches)
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && s1_enabled && !resp_s1_v &&
                !resp_s1_af))) {
            bins b_expected_result = {1'b1};
        }
        // 04.04.014 FCOV_异常输出组合
        cp_04_04_014 : coverpoint (fault_matches)
            iff (sample_enable && ((response_sample && (response_hit || pre_fault || !translate_enable)))) {
            bins b_expected_result = {1'b1};
        }
        // 04.04.001 FAULT_S1_AF_cmd0
        cp_04_04_001 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && req_out_cmd == 0 && s1_enabled
                && resp_s1_af))
 && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.002 FAULT_S1_AF_cmd1
        cp_04_04_002 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && req_out_cmd == 1 && s1_enabled
                && resp_s1_af))
 && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.003 FAULT_S2_AF_cmd0
        cp_04_04_003 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && req_out_cmd == 0 && s2_enabled
                && resp_s2_af))
 && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.004 FAULT_S2_AF_cmd1
        cp_04_04_004 : coverpoint ptw_bypass_hit
            iff (sample_enable && ((response_sample && response_hit && !pre_fault && req_out_cmd == 1 && s2_enabled
                && resp_s2_af))
 && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.005 FAULT_PF优先GPF_cmd0
        cp_04_04_005 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && req_out_cmd == 0 && req_out_s2xlate == 3 &&
                    translate_enable && !access_fault && s1_page_fault && s2_page_fault))
 && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.006 FAULT_PF优先GPF_cmd1
        cp_04_04_006 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && req_out_cmd == 1 && req_out_s2xlate == 3 &&
                    translate_enable && !access_fault && s1_page_fault && s2_page_fault))
 && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.007 FAULT_AF优先PF_GPF_cmd0
        cp_04_04_007 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && req_out_cmd == 0 && req_out_s2xlate == 3 &&
                    translate_enable && access_fault && s1_page_fault && s2_page_fault))
 && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.008 FAULT_AF优先PF_GPF_cmd1
        cp_04_04_008 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && req_out_cmd == 1 && req_out_s2xlate == 3 &&
                    translate_enable && access_fault && s1_page_fault && s2_page_fault))
 && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.009 FAULT_fakePTE_cmd0
        cp_04_04_009 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && req_out_cmd == 0 && req_out_s2xlate == 3 &&
                    translate_enable && fake_pte && s2_page_fault))
 && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.010 FAULT_fakePTE_cmd1
        cp_04_04_010 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && req_out_cmd == 1 && req_out_s2xlate == 3 &&
                    translate_enable && fake_pte && s2_page_fault))
 && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.011 FAULT_nonLeaf_cmd0
        cp_04_04_011 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && req_out_cmd == 0 && req_out_s2xlate == 3 &&
                    translate_enable && nonleaf_pte && s2_page_fault))
 && (((fault_matches)) === 1'b1)) {
            option.weight = (GROUP_ID == 1 ? 0 : 1);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.04.012 FAULT_nonLeaf_cmd1
        cp_04_04_012 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && !pre_fault && req_out_cmd == 1 && req_out_s2xlate == 3 &&
                    translate_enable && nonleaf_pte && s2_page_fault))
 && (((fault_matches)) === 1'b1)) {
            option.weight = ((GROUP_ID == 1 || (GROUP_ID == 0 && PORT_INDEX == 0) || GROUP_ID == 2) ? 1 : 0);
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
    endgroup
    cg_dtlb_04_04 cg_dtlb_04_04_inst;

    // 04.05 异常分类与Consumer行为
    covergroup cg_dtlb_04_05 @(posedge clock);
        option.per_instance = 1;
        // 04.05.001 FAULT_SC_AMO分类
        cp_04_05_001 : coverpoint (fault_matches)
            iff (sample_enable && ((response_sample && req_out_cmd inside {4, 5} && (response_hit || pre_fault)))) {
            bins b_expected_result = {1'b1};
        }
        // 04.05.002 FAULT_正常vaNeedExt
        cp_04_05_002 : coverpoint (resp_ext)
            iff (sample_enable && ((response_sample && !pre_fault && GROUP_ID<2 && !(GROUP_ID == 0 && PORT_INDEX ==
                3)))) {
            bins b_expected_result = {1'b1};
        }
        // 04.05.003 FAULT_缓存与旁路等价
        cp_04_05_003 : coverpoint (actual_fault == last_fault && resp_pbmt == last_pbmt)
            iff (sample_enable && (response_sample && last_response_valid && last_bypass && storage_hit &&
                !ptw_bypass_hit &&
                last_vaddr == req_vaddr_d1)) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_04_05 cg_dtlb_04_05_inst;

    // 04.06 PBMT选取与覆盖
    covergroup cg_dtlb_04_06 @(posedge clock);
        option.per_instance = 1;
        // 04.06.001 PBMT_s0_00
        cp_04_06_001 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 0 && resp_s1pbmt == 0
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.002 PBMT_s0_01
        cp_04_06_002 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 0 && resp_s1pbmt == 0
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.003 PBMT_s0_02
        cp_04_06_003 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 0 && resp_s1pbmt == 0
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.004 PBMT_s0_10
        cp_04_06_004 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 0 && resp_s1pbmt == 1
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.005 PBMT_s0_11
        cp_04_06_005 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 0 && resp_s1pbmt == 1
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.006 PBMT_s0_12
        cp_04_06_006 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 0 && resp_s1pbmt == 1
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.007 PBMT_s0_20
        cp_04_06_007 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 0 && resp_s1pbmt == 2
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.008 PBMT_s0_21
        cp_04_06_008 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 0 && resp_s1pbmt == 2
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.009 PBMT_s0_22
        cp_04_06_009 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 0 && resp_s1pbmt == 2
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.010 PBMT_s1_00
        cp_04_06_010 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 1 && resp_s1pbmt == 0
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.011 PBMT_s1_01
        cp_04_06_011 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 1 && resp_s1pbmt == 0
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.012 PBMT_s1_02
        cp_04_06_012 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 1 && resp_s1pbmt == 0
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.013 PBMT_s1_10
        cp_04_06_013 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 1 && resp_s1pbmt == 1
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.014 PBMT_s1_11
        cp_04_06_014 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 1 && resp_s1pbmt == 1
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.015 PBMT_s1_12
        cp_04_06_015 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 1 && resp_s1pbmt == 1
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.016 PBMT_s1_20
        cp_04_06_016 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 1 && resp_s1pbmt == 2
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.017 PBMT_s1_21
        cp_04_06_017 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 1 && resp_s1pbmt == 2
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.018 PBMT_s1_22
        cp_04_06_018 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 1 && resp_s1pbmt == 2
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.019 PBMT_s2_00
        cp_04_06_019 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 2 && resp_s1pbmt == 0
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.020 PBMT_s2_01
        cp_04_06_020 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 2 && resp_s1pbmt == 0
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.021 PBMT_s2_02
        cp_04_06_021 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 2 && resp_s1pbmt == 0
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.022 PBMT_s2_10
        cp_04_06_022 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 2 && resp_s1pbmt == 1
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.023 PBMT_s2_11
        cp_04_06_023 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 2 && resp_s1pbmt == 1
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.024 PBMT_s2_12
        cp_04_06_024 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 2 && resp_s1pbmt == 1
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.025 PBMT_s2_20
        cp_04_06_025 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 2 && resp_s1pbmt == 2
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.026 PBMT_s2_21
        cp_04_06_026 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 2 && resp_s1pbmt == 2
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.027 PBMT_s2_22
        cp_04_06_027 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 2 && resp_s1pbmt == 2
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.028 PBMT_s3_00
        cp_04_06_028 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 3 && resp_s1pbmt == 0
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.029 PBMT_s3_01
        cp_04_06_029 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 3 && resp_s1pbmt == 0
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.030 PBMT_s3_02
        cp_04_06_030 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 3 && resp_s1pbmt == 0
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.031 PBMT_s3_10
        cp_04_06_031 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 3 && resp_s1pbmt == 1
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.032 PBMT_s3_11
        cp_04_06_032 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 3 && resp_s1pbmt == 1
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.033 PBMT_s3_12
        cp_04_06_033 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 3 && resp_s1pbmt == 1
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.034 PBMT_s3_20
        cp_04_06_034 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 3 && resp_s1pbmt == 2
                    && resp_s2pbmt == 0)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.035 PBMT_s3_21
        cp_04_06_035 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 3 && resp_s1pbmt == 2
                    && resp_s2pbmt == 1)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.036 PBMT_s3_22
        cp_04_06_036 : coverpoint ptw_bypass_hit
            iff (sample_enable &&
                ((response_sample && response_hit && translate_enable && req_out_s2xlate == 3 && resp_s1pbmt == 2
                    && resp_s2pbmt == 2)) &&
                (((resp_pbmt == expected_pbmt)) === 1'b1)) {
            bins b_storage = {1'b0};
            bins b_bypass = {1'b1};
        }
        // 04.06.037 PBMT_直通强制PMA
        cp_04_06_037 : coverpoint (resp_pbmt == 0)
            iff (sample_enable && ((response_sample && !translate_enable && (resp_s1pbmt != 0 || resp_s2pbmt != 0)))) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_04_06 cg_dtlb_04_06_inst;

    // 04.06 PBMT选取与覆盖
    covergroup cg_dtlb_diag_04_06 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 04.06.038：结果偏离或未知单列，正常命中不进入诊断仓。
        rs_04_06_038 : coverpoint ($isunknown((resp_pbmt == 3)) ? 2'd2 : ((resp_pbmt == 3)) ? 2'd0 : 2'd1)
            iff (sample_enable && ((response_sample && response_hit && translate_enable && expected_pbmt == 3))) {
            bins b_mismatch = {2'd1};
            bins unknown  = {2'd2};
        }
        // 04.06.038 PBMT_reserved
        cp_04_06_038 : coverpoint (resp_pbmt == 3)
            iff (sample_enable && ((response_sample && response_hit && translate_enable && expected_pbmt == 3))) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_diag_04_06 cg_dtlb_diag_04_06_inst;

    // 04.07 PMP交接与内存属性消费
    covergroup cg_dtlb_04_07 @(posedge clock);
        option.per_instance = 1;
        // 04.07.001 MODE_prefetch_PMP特权
        cp_04_07_001 : coverpoint (resp_pmpmode == csr_dmode)
            iff (sample_enable && ((response_sample && req_prefetch_d1 && req_hyperinst_d1 && csr_dmode != {1'b0,
                csr_spvp}))) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.002 PMP_翻译地址
        cp_04_07_002 : coverpoint (resp_pmpaddr == resp_paddr_0 && resp_pmpcmd == (req_hlvx_d1?7:req_out_cmd))
            iff (sample_enable && ((response_sample && translate_enable && !resp_miss && actual_fault == 0))) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.003 PMP_直通地址
        cp_04_07_003 : coverpoint (resp_pmpaddr == (req_no_translate_d1?req_pmpaddr:req_vaddr_d1[47:0]))
            iff (sample_enable && ((response_sample && !translate_enable))) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.004 PMP_noTranslate无resp
        cp_04_07_004 : coverpoint (resp_pmpvalid)
            iff (sample_enable && ((req_no_translate_d1 && !resp_valid && GROUP_ID != 1))) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.005 PMP_HLVX命令
        cp_04_07_005 : coverpoint (resp_pmpcmd == 7)
            iff (sample_enable && ((response_sample && req_hlvx_d1))) {
            bins b_expected_result = {1'b1};
        }
        // 04.07.006 PMP_模式分派
        cp_04_07_006 : coverpoint (resp_pmpmode == (req_prefetch_d1?csr_dmode:effective_mode))
            iff (sample_enable && ((response_sample))) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_04_07 cg_dtlb_04_07_inst;

    // 05.01 Miss产生与本地处理去向
    covergroup cg_dtlb_05_01 @(posedge clock);
        option.per_instance = 1;
        // 05.01.001 MISS_普通发PTW
        cp_05_01_001 : coverpoint (resp_qvalid && resp_qvpn == req_out_vaddr[49:12] && resp_qstage == req_out_s2xlate)
            iff (sample_enable && ((response_sample && resp_miss_read && !current_return_matches &&
                !previous_return_matches &&
                    !resp_need_replay && !blocked_by_gpa && !req_late_kill))) {
            bins b_expected_result = {1'b1};
        }
        // 05.01.002 MISS_多抑制同时发生
        cp_05_01_002 : coverpoint (!resp_qvalid && (GROUP_ID != 0 || PORT_INDEX == 3 || resp_replay))
            iff (sample_enable && ((response_sample && resp_miss_read && $countones(miss_cause) >= 2))) {
            bins b_expected_result = {1'b1};
        }
        // 05.01.003 MISS_最终miss与fastMiss差异
        cp_05_01_003 : coverpoint (resp_miss_read && resp_miss)
            iff (sample_enable && ((response_sample && storage_hit && resp_has_gpf && !ptw_bypass_hit && !gpa_matches &&
                    !req_prefetch_d1 && req_out_s2xlate != 2 && !resp_last_red))) {
            bins b_expected_result = {1'b1};
        }
        // 05.01.005 MISS_命中不发请求
        cp_05_01_005 : coverpoint (!resp_qvalid)
            iff (sample_enable && ((response_sample && response_hit && !resp_miss_read && !pre_fault &&
                !req_late_kill))) {
            bins b_expected_result = {1'b1};
        }
        // 05.01.006 FCOV_Miss处理去向
        cp_05_01_006 : coverpoint (response_sample && resp_miss_read)
            iff (sample_enable && ((response_sample && resp_miss_read))) {
            bins b_observed = {1'b1};
        }
        // 05.01.004 MISS_重试生命周期
        cp_05_01_004 : coverpoint fault_matches
            iff (sample_enable && (response_sample && miss_pending && req_vaddr_d1 == miss_vaddr &&
                req_rob_d1 == miss_rob && return_seen && !resp_miss)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_05_01 cg_dtlb_05_01_inst;

    // 05.02 返回窗口与重复请求过滤
    covergroup cg_dtlb_05_02 @(posedge clock);
        option.per_instance = 1;
        // 05.02.001 MISS_当拍返回重试
        cp_05_02_001 : coverpoint (!resp_qvalid && (GROUP_ID != 0 || PORT_INDEX == 3 || resp_replay))
            iff (sample_enable && ((response_sample && resp_miss_read && current_return_matches && !req_late_kill))) {
            bins b_expected_result = {1'b1};
        }
        // 05.02.002 MISS_前拍返回重试
        cp_05_02_002 : coverpoint (!resp_qvalid && (GROUP_ID != 0 || PORT_INDEX == 3 || resp_replay))
            iff (sample_enable && ((response_sample && resp_miss_read && !current_return_matches &&
                previous_return_matches &&
                    !req_late_kill))) {
            bins b_expected_result = {1'b1};
        }
        // 05.02.003 MISS_返回两拍后
        cp_05_02_003 : coverpoint (resp_qvalid || resp_need_replay || blocked_by_gpa || req_late_kill)
            iff (sample_enable && (response_sample && resp_miss_read && return_older && !current_return_matches &&
                !previous_return_matches)) {
            bins b_expected_result = {1'b1};
        }
        // 05.02.004 MISS_返回上下文不匹配
        cp_05_02_004 : coverpoint (resp_qvalid || resp_need_replay || blocked_by_gpa || req_late_kill)
            iff (sample_enable && (response_sample && resp_miss_read && (ptw_valid || ptw_valid_d1) &&
                !current_return_matches && !previous_return_matches)) {
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_05_02 cg_dtlb_05_02_inst;

    // 06.03 PTW旁路与读写冲突
    covergroup cg_dtlb_06_03 @(posedge clock);
        option.per_instance = 1;
        // 06.03.002 BYPASS_覆盖旧storage
        cp_06_03_002 : coverpoint address_matches
            iff (sample_enable && (response_sample && storage_hit && ptw_bypass_hit)) {
            bins b_expected_set = {1'b1};
        }
        // 06.03.003 BYPASS_不匹配返回
        cp_06_03_003 : coverpoint ptw_bypass_hit
            iff (sample_enable && (request_due && ptw_valid_d1 && !bypass_fast_d1) && ((!ptw_bypass_hit) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
        // 06.03.004 BYPASS_refill受抑仍旁路
        cp_06_03_004 : coverpoint ptw_bypass_hit
            iff (sample_enable && (request_due && bypass_fast_d1 && (ptw_getgpa_d1 || gpa_ng_d1))) {
            bins b_expected_set = {1'b1};
        }
        // 06.03.008 PTW回复无效_不形成下一拍旁路
        cp_06_03_008 : coverpoint ptw_bypass_hit
            iff (sample_enable && (request_due && !ptw_valid_d1) && ((!ptw_bypass_hit) === 1'b1)) {
            bins b_expected_clear = {1'b0};
        }
    endgroup
    cg_dtlb_06_03 cg_dtlb_06_03_inst;

    // 06.05 旁路PPN与GVPN拼接分支
    covergroup cg_dtlb_06_05 @(posedge clock);
        option.per_instance = 1;
        // 06.05.001 PTW旁路PPN_stage0_level0_n0
        cp_06_05_001 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 0 && ptw_stage_d1 == 0 &&
                ptw_s1level_d1 == 0 && ptw_s1n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.002 PTW旁路PPN_stage0_level0_n1
        cp_06_05_002 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 0 && ptw_stage_d1 == 0 &&
                ptw_s1level_d1 == 0 && ptw_s1n_d1 == 1)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.003 PTW旁路PPN_stage0_level1_n0
        cp_06_05_003 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 0 && ptw_stage_d1 == 0 &&
                ptw_s1level_d1 == 1 && ptw_s1n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.004 PTW旁路PPN_stage0_level2_n0
        cp_06_05_004 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 0 && ptw_stage_d1 == 0 &&
                ptw_s1level_d1 == 2 && ptw_s1n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.005 PTW旁路PPN_stage0_level3_n0
        cp_06_05_005 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 0 && ptw_stage_d1 == 0 &&
                ptw_s1level_d1 == 3 && ptw_s1n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.006 PTW旁路PPN_stage1_level0_n0
        cp_06_05_006 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 1 && ptw_stage_d1 == 1 &&
                ptw_s1level_d1 == 0 && ptw_s1n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.007 PTW旁路PPN_stage1_level0_n1
        cp_06_05_007 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 1 && ptw_stage_d1 == 1 &&
                ptw_s1level_d1 == 0 && ptw_s1n_d1 == 1)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.008 PTW旁路PPN_stage1_level1_n0
        cp_06_05_008 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 1 && ptw_stage_d1 == 1 &&
                ptw_s1level_d1 == 1 && ptw_s1n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.009 PTW旁路PPN_stage1_level2_n0
        cp_06_05_009 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 1 && ptw_stage_d1 == 1 &&
                ptw_s1level_d1 == 2 && ptw_s1n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.010 PTW旁路PPN_stage1_level3_n0
        cp_06_05_010 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 1 && ptw_stage_d1 == 1 &&
                ptw_s1level_d1 == 3 && ptw_s1n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.011 PTW旁路PPN_stage2_level0_n0
        cp_06_05_011 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 2 && ptw_stage_d1 == 2 &&
                ptw_s2level_d1 == 0 && ptw_s2n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.012 PTW旁路PPN_stage2_level0_n1
        cp_06_05_012 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 2 && ptw_stage_d1 == 2 &&
                ptw_s2level_d1 == 0 && ptw_s2n_d1 == 1)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.013 PTW旁路PPN_stage2_level1_n0
        cp_06_05_013 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 2 && ptw_stage_d1 == 2 &&
                ptw_s2level_d1 == 1 && ptw_s2n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.014 PTW旁路PPN_stage2_level2_n0
        cp_06_05_014 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 2 && ptw_stage_d1 == 2 &&
                ptw_s2level_d1 == 2 && ptw_s2n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.015 PTW旁路PPN_stage2_level3_n0
        cp_06_05_015 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 2 && ptw_stage_d1 == 2 &&
                ptw_s2level_d1 == 3 && ptw_s2n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.016 PTW旁路PPN_stage3_level0_n0
        cp_06_05_016 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 3 && ptw_stage_d1 == 3 &&
                ptw_s2level_d1 == 0 && ptw_s2n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.017 PTW旁路PPN_stage3_level0_n1
        cp_06_05_017 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 3 && ptw_stage_d1 == 3 &&
                ptw_s2level_d1 == 0 && ptw_s2n_d1 == 1)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.018 PTW旁路PPN_stage3_level1_n0
        cp_06_05_018 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 3 && ptw_stage_d1 == 3 &&
                ptw_s2level_d1 == 1 && ptw_s2n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.019 PTW旁路PPN_stage3_level2_n0
        cp_06_05_019 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 3 && ptw_stage_d1 == 3 &&
                ptw_s2level_d1 == 2 && ptw_s2n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
        // 06.05.020 PTW旁路PPN_stage3_level3_n0
        cp_06_05_020 : coverpoint address_matches
            iff (sample_enable && (response_sample && ptw_bypass_hit && req_stage_d1 == 3 && ptw_stage_d1 == 3 &&
                ptw_s2level_d1 == 3 && ptw_s2n_d1 == 0)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_06_05 cg_dtlb_06_05_inst;

    // 07.01 GPA启动资格与启动抑制
    covergroup cg_dtlb_07_01 @(posedge clock);
        option.per_instance = 1;
        // 07.01.003 GPA_非ROB头重放
        cp_07_01_003 : coverpoint ((resp_need_replay && !resp_qvalid && (GROUP_ID != 0 || PORT_INDEX == 3 ||
            resp_replay)))
            iff (sample_enable && ((response_sample && gpa_start && !resp_can_gpa && !req_mabuf_d1 &&
                !gpa_clear_event))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.005 GPA_不新建_prefetch
        cp_07_01_005 : coverpoint (!gpa_ng_wire)
            iff (sample_enable && ((response_sample && response_hit && resp_has_gpf && req_prefetch_d1 && !gpa_ng))) {
            bins b_expected_result = {1'b1};
        }
        // 07.01.006 GPA_不新建_bypass
        cp_07_01_006 : coverpoint (!gpa_ng_wire)
            iff (sample_enable && ((response_sample && resp_has_gpf && ptw_bypass_hit && !gpa_ng))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.007 GPA_不新建_已有匹配GPA
        cp_07_01_007 : coverpoint (!gpa_ng_wire)
            iff (sample_enable && ((response_sample && resp_has_gpf && gpa_response_ready && gpa_matches &&
                !gpa_ng))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.008 GPA_不新建_当前redirect
        cp_07_01_008 : coverpoint (!gpa_ng_wire)
            iff (sample_enable && ((response_sample && resp_has_gpf && current_redirect && !gpa_ng))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.009 GPA_不新建_前拍redirect
        cp_07_01_009 : coverpoint (!gpa_ng_wire)
            iff (sample_enable && ((response_sample && resp_has_gpf && resp_last_red && !gpa_ng))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.010 GPA_不新建_req_kill
        cp_07_01_010 : coverpoint (!gpa_ng_wire && !resp_qvalid)
            iff (sample_enable && ((response_sample && resp_has_gpf && req_late_kill && !gpa_ng))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.011 GPA_正常miss非getGpa
        cp_07_01_011 : coverpoint (!resp_qgpa)
            iff (sample_enable && ((response_sample && !response_hit && translate_enable && !pre_fault))) {
            bins b_expected_result = {1'b1};
        }
        // 07.01.001 GPA_进入ROB头
        cp_07_01_001 : coverpoint (gpa_ng_wire && resp_qgpa)
            iff (sample_enable && (gpa_start && resp_can_gpa && req_rob_d1 == rob_head)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.01.002 GPA_进入MABUF
        cp_07_01_002 : coverpoint (gpa_ng_wire && resp_qgpa)
            iff (sample_enable && (gpa_start && resp_can_gpa && req_rob_d1 != rob_head && req_mabuf_d1)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_07_01 cg_dtlb_07_01_inst;

    // 07.02 等待请求与返回匹配
    covergroup cg_dtlb_07_02 @(posedge clock);
        option.per_instance = 1;
        // 07.02.001 GPA_等待期间同页
        cp_07_02_001 : coverpoint ((resp_qvalid || current_return_matches || previous_return_matches ||
            resp_need_replay || req_late_kill))
            iff (sample_enable && ((response_sample && resp_miss_read && gpa_ng && !gpa_response_ready &&
                gpa_matches))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.002 GPA_等待期间异页
        cp_07_02_002 : coverpoint (!resp_qvalid && (GROUP_ID != 0 || PORT_INDEX == 3 || resp_replay))
            iff (sample_enable && ((response_sample && resp_miss_read && gpa_ng && !gpa_response_ready &&
                !gpa_matches))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.003 GPA_等待期间普通hit
        cp_07_02_003 : coverpoint (!resp_miss && !resp_qvalid)
            iff (sample_enable && ((response_sample && gpa_ng && storage_hit && !resp_has_gpf && !pre_fault))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.007 GPA_已取回时异页请求
        cp_07_02_007 : coverpoint (resp_qvalid)
            iff (sample_enable && ((response_sample && resp_miss_read && gpa_ng && gpa_response_ready && !gpa_matches &&
                    !current_return_matches && !previous_return_matches && !resp_need_replay))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.008 GPA_重取后同VPN换stage
        cp_07_02_008 : coverpoint (!gpa_matches)
            iff (sample_enable && ((response_sample && gpa_response_ready && gpa_vpn == req_out_vaddr[49:12] &&
                gpa_stage !=
                    req_out_s2xlate))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.004 GPA_匹配返回
        cp_07_02_004 : coverpoint gpa_response_ready
            iff (sample_enable && (ptw_valid_d1 && gpa_ng_d1 && ptw_stage_d1 == gpa_stage_d1 && !gpa_clear_event)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_set = {1'b1};
        }
        // 07.02.005 GPA_匹配返回getGpa位非门控
        cp_07_02_005 : coverpoint ptw_getgpa_d1
            iff (sample_enable && (ptw_valid_d1 && gpa_ng_d1 && ptw_stage_d1 == gpa_stage_d1 && !gpa_clear_event)
 && ((gpa_response_ready) === 1'b1)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_ordinary_return = {1'b0};
            bins b_gpa_return = {1'b1};
        }
        // 07.02.006 GPA_不匹配返回
        cp_07_02_006 : coverpoint (gpa_response_ready == gpa_ready_d1)
            iff (sample_enable && (ptw_valid_d1 && gpa_ng_d1 && ptw_stage_d1 != gpa_stage_d1 && !gpa_clear_event)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.02.009 GPA_getVpn范围匹配
        cp_07_02_009 : coverpoint gpa_response_ready
            iff (sample_enable && (ptw_valid_d1 && gpa_ng_d1 && ptw_stage_d1 == gpa_stage_d1 &&
                gpa_vpn_d1 != {ptw_s1tag_d1, ptw_s1low_d1})) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_07_02 cg_dtlb_07_02_inst;

    // 07.03 GPA消费、重定向与退出计数
    covergroup cg_dtlb_07_03 @(posedge clock);
        option.per_instance = 1;
        // 07.03.008 GPA_owner标识复用
        cp_07_03_008 : coverpoint (req_rob_d1 != gpa_owner)
            iff (sample_enable && ((response_sample && gpa_owner_valid && req_rob_d1[7:0] == gpa_owner[7:0] &&
                req_rob_d1[8] !=
                    gpa_owner[8]))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.03.001 GPA_消费并退出
        cp_07_03_001 : coverpoint (!resp_miss && fault_matches)
            iff (sample_enable && (response_sample && resp_has_gpf && gpa_ng && gpa_response_ready && gpa_matches)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.03.002 GPA_启动同拍fast返回
        cp_07_03_002 : coverpoint gpa_clear
            iff (sample_enable && (fast_gpa_start_d1)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_set = {1'b1};
        }
        // 07.03.003 GPA_redirect退出
        cp_07_03_003 : coverpoint (!gpa_ng && !gpa_response_ready && !gpa_owner_valid)
            iff (sample_enable && (owner_redirected_d1)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.03.004 GPA_redirect不影响older
        cp_07_03_004 : coverpoint gpa_ng
            iff (sample_enable && (gpa_ng_d1 && redirect_valid_d1 && !owner_redirected_d1 && !gpa_clear_event)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_set = {1'b1};
        }
        // 07.03.005 GPA_计数边界
        cp_07_03_005 : coverpoint gpa_count
            iff (sample_enable && (gpa_ng && gpa_count inside {9'd0, 9'd1, 9'd510, 9'd511})) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_zero = {0};
            bins b_one = {1};
            bins b_near_full = {510};
            bins b_full = {511};
        }
        // 07.03.006 GPA_计数保持
        cp_07_03_006 : coverpoint (gpa_count == gpa_count_d1)
            iff (sample_enable && (gpa_ng_d1 && !ptw_getgpa_event_d1 && !gpa_start_d1 && !owner_redirected_d1 &&
                gpa_count_d1 != 511)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.03.007 GPA_计数清零优先
        cp_07_03_007 : coverpoint (gpa_count == 0)
            iff (sample_enable && (ptw_getgpa_event_d1 && (gpa_start_d1 || owner_redirected_d1))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_07_03 cg_dtlb_07_03_inst;

    // 07.04 GPA地址与跨页故障定位
    covergroup cg_dtlb_07_04 @(posedge clock);
        option.per_instance = 1;
        // 07.04.001 CROSS_跨页首半GPF
        cp_07_04_001 : coverpoint (resp_gpaddr[11:0] == req_fullva_d1[11:0])
            iff (sample_enable && ((response_sample && resp_has_gpf && !resp_nonleaf &&
                req_fullva_d1[12] == req_out_vaddr[12] &&
                    req_fullva_d1[11:0] != req_out_vaddr[11:0] && GROUP_ID<2))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.002 CROSS_跨页第二半GPF
        cp_07_04_002 : coverpoint (resp_gpaddr[11:0] == req_out_vaddr[11:0])
            iff (sample_enable && ((response_sample && resp_has_gpf && !resp_nonleaf &&
                req_fullva_d1[12] != req_out_vaddr[12] &&
                    GROUP_ID<2))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.003 CROSS_onlyS2地址
        cp_07_04_003 : coverpoint ((resp_gpaddr == (req_fullva_d1[12] ==
            req_out_vaddr[12]?req_effective_va_d1:{14'd0, req_out_vaddr})))
            iff (sample_enable && ((response_sample && resp_has_gpf && req_out_s2xlate == 2 && GROUP_ID<2))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.009 GPA_leaf_offset
        cp_07_04_009 : coverpoint (resp_gpaddr[11:0] == req_fullva_d1[11:0])
            iff (sample_enable && ((response_sample && resp_has_gpf && !resp_nonleaf && !pre_fault && req_fullva_d1[12] ==
                    req_out_vaddr[12] && req_fullva_d1[11:0] == req_out_vaddr[11:0] && GROUP_ID<2))) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.004 GPA_nonLeaf_level1
        cp_07_04_004 : coverpoint (resp_gpaddr[11:0] == {req_vaddr_d1[20:12], 3'd0})
            iff (sample_enable && (response_sample && resp_has_gpf && resp_nonleaf && gpa_s1level == 1 && HAS_GPA)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.005 GPA_nonLeaf_level2
        cp_07_04_005 : coverpoint (resp_gpaddr[11:0] == {req_vaddr_d1[29:21], 3'd0})
            iff (sample_enable && (response_sample && resp_has_gpf && resp_nonleaf && gpa_s1level == 2 && HAS_GPA)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.006 GPA_nonLeaf_level3
        cp_07_04_006 : coverpoint (resp_gpaddr[11:0] == {req_vaddr_d1[38:30], 3'd0})
            iff (sample_enable && (response_sample && resp_has_gpf && resp_nonleaf && gpa_s1level == 3 && HAS_GPA)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.007 GPA_fakePTE_mode8
        cp_07_04_007 : coverpoint (resp_gpaddr[11:0] == {req_vaddr_d1[38:30], 3'd0})
            iff (sample_enable && (response_sample && resp_has_gpf && gpa_s1fake && csr_vsatp_mode == 8 && HAS_GPA)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
        // 07.04.008 GPA_fakePTE_mode9
        cp_07_04_008 : coverpoint (resp_gpaddr[11:0] == {req_vaddr_d1[47:39], 3'd0})
            iff (sample_enable && (response_sample && resp_has_gpf && gpa_s1fake && csr_vsatp_mode == 9 && HAS_GPA)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_07_04 cg_dtlb_07_04_inst;

    // 07.05 共享GPA状态竞争与恢复
    covergroup cg_dtlb_07_05 @(posedge clock);
        option.per_instance = 1;
        // 07.05.004 CONCURRENT_GPA_替换_预取
        cp_07_05_004 : coverpoint (!resp_miss || resp_qvalid || resp_replay)
            iff (sample_enable && (response_sample && gpa_ng && !resp_has_gpf)) {
            option.weight = (GROUP_ID == 2 ? 0 : 1);
            bins b_expected_result = {1'b1};
        }
    endgroup
    cg_dtlb_07_05 cg_dtlb_07_05_inst;

    // 08.05 在途刷新与新旧上下文恢复
    covergroup cg_dtlb_08_05 @(posedge clock);
        option.per_instance = 1;
        // 08.05.005 FENCE_新epoch重新填入
        cp_08_05_005 : coverpoint (address_matches && fault_matches)
            iff (sample_enable && (response_sample && flushed_epoch && refill_seen && storage_hit && !resp_miss)) {
            bins b_expected_result = {1'b1};
        }
        // 08.05.007 CONCURRENT_ASID_VMID快速切换
        cp_08_05_007 : coverpoint fault_matches
            iff (sample_enable && (response_sample && {csr_asid, csr_vasid, csr_vmid} != context_ids_d1)) {
            bins b_expected_set = {1'b1};
        }
        // 08.05.008 E2E_映射变更
        cp_08_05_008 : coverpoint address_matches
            iff (sample_enable && (response_sample && last_response_valid && flushed_epoch &&
                last_vaddr == req_vaddr_d1 && last_paddr != resp_paddr_0 && !resp_miss)) {
            bins b_expected_set = {1'b1};
        }
        // 08.05.009 E2E_故障修复
        cp_08_05_009 : coverpoint address_matches
            iff (sample_enable && (response_sample && last_response_valid && flushed_epoch &&
                last_vaddr == req_vaddr_d1 && last_fault != 0 && actual_fault == 0 && !resp_miss)) {
            bins b_expected_set = {1'b1};
        }
    endgroup
    cg_dtlb_08_05 cg_dtlb_08_05_inst;

    // 09.03 结果已知性与结束原因
    covergroup cg_dtlb_09_03 @(posedge clock);
        option.per_instance = 1;
        // 09.03.001 FCOV_输出已知性
        cp_09_03_001 : coverpoint response_knownness
            iff (sample_enable && (response_sample) &&
                ((!$isunknown({resp_valid, resp_miss, resp_excp, resp_paddr_0, resp_pbmt})) === 1'b1)) {
            bins b_all_known = {0};
        }
        // 09.03.002 FCOV_完成延迟与窗口结束
        cp_09_03_002 : coverpoint response_end_reason
            iff (sample_enable && (request_due) && ((resp_valid) === 1'b1)) {
            bins b_success = {0};
            bins b_fault = {1};
            bins b_replay = {2};
        }
    endgroup
    cg_dtlb_09_03 cg_dtlb_09_03_inst;

    initial begin
        cg_dtlb_01_01_inst = new();
        cg_dtlb_01_02_inst = new();
        cg_dtlb_01_03_inst = new();
        cg_dtlb_01_04_inst = new();
        cg_dtlb_01_05_inst = new();
        cg_dtlb_diag_01_05_inst = new();
        cg_dtlb_02_01_inst = new();
        cg_dtlb_02_02_inst = new();
        cg_dtlb_02_03_inst = new();
        cg_dtlb_02_04_inst = new();
        cg_dtlb_02_05_inst = new();
        cg_dtlb_diag_02_05_inst = new();
        cg_dtlb_03_01_inst = new();
        cg_dtlb_03_03_inst = new();
        cg_dtlb_03_07_inst = new();
        cg_dtlb_04_01_inst = new();
        cg_dtlb_04_02_inst = new();
        cg_dtlb_04_03_inst = new();
        cg_dtlb_04_04_inst = new();
        cg_dtlb_04_05_inst = new();
        cg_dtlb_04_06_inst = new();
        cg_dtlb_diag_04_06_inst = new();
        cg_dtlb_04_07_inst = new();
        cg_dtlb_05_01_inst = new();
        cg_dtlb_05_02_inst = new();
        cg_dtlb_06_03_inst = new();
        cg_dtlb_06_05_inst = new();
        cg_dtlb_07_01_inst = new();
        cg_dtlb_07_02_inst = new();
        cg_dtlb_07_03_inst = new();
        cg_dtlb_07_04_inst = new();
        cg_dtlb_07_05_inst = new();
        cg_dtlb_08_05_inst = new();
        cg_dtlb_09_03_inst = new();
    end

endmodule: memblock_dtlb_core_fcov
`endif

`endif
