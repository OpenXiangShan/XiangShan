module frontend_atp_funcov (
  input logic          clock,
  input logic          reset,
  input logic          io_tlbCsr_priv_virt,
  input logic [1:0]    io_tlbCsr_priv_imode,
  input logic [3:0]    io_tlbCsr_satp_mode,
  input logic [3:0]    io_tlbCsr_vsatp_mode,
  input logic [3:0]    io_tlbCsr_hgatp_mode,
  input logic [15:0]   io_tlbCsr_satp_asid,
  input logic [15:0]   io_tlbCsr_vsatp_asid,
  input logic [15:0]   io_tlbCsr_hgatp_vmid,
  input logic          io_tlbCsr_satp_changed,
  input logic          io_tlbCsr_vsatp_changed,
  input logic          io_tlbCsr_hgatp_changed,
  input logic          io_tlbCsr_priv_virt_changed,
  input logic          itlb_req_valid,
  input logic [49:0]   itlb_req_vaddr,
  input logic          itlb_resp_miss,
  input logic [1:0]    itlb_resp_pbmt,
  input logic          itlb_resp_pf_instr,
  input logic          itlb_resp_af_instr,
  input logic          itlb_resp_gpf_instr,
  input logic          ptw_req_valid,
  input logic          ptw_req_ready,
  input logic [37:0]   ptw_req_vpn,
  input logic [1:0]    ptw_req_s2xlate,
  input logic          ptw_resp_valid,
  input logic [1:0]    ptw_resp_s2xlate,
  input logic [1:0]    ptw_resp_s1_pbmt,
  input logic [1:0]    ptw_resp_s2_pbmt,
  input logic          ptw_resp_s1_v,
  input logic          ptw_resp_s1_perm_a,
  input logic          ptw_resp_s1_perm_u,
  input logic          ptw_resp_s1_perm_x,
  input logic          ptw_resp_s1_perm_w,
  input logic          ptw_resp_s1_perm_r,
  input logic          ptw_resp_s2_perm_a,
  input logic          ptw_resp_s2_perm_x,
  input logic          ptw_resp_s1_pf,
  input logic          ptw_resp_s1_af,
  input logic          ptw_resp_s2_gpf,
  input logic          ptw_resp_s2_gaf,
  input logic [1:0]    ptw_resp_s1_entry_level,
  input logic          ptw_resp_s1_entry_n,
  input logic [7:0]    ptw_resp_s1_valididx,
  input logic [7:0]    ptw_resp_s1_pteidx,
  input logic          flush_pipe,
  input logic          sfence_valid,
  input logic          sfence_rs1,
  input logic          sfence_rs2,
  input logic [49:0]   sfence_addr,
  input logic [15:0]   sfence_id,
  input logic          sfence_hv,
  input logic          sfence_hg,
  input logic          redirect_valid,
  input logic          backend_can_accept,
  input logic [47:0]   pmp_addr_0,
  input logic [47:0]   pmp_addr_1,
  input logic [31:0]   pmp_cfg_l,
  input logic [63:0]   pmp_cfg_a,
  input logic [31:0]   pmp_cfg_x,
  input logic [1471:0] pmp_addr,
  input logic [1535:0] pmp_mask,
  input logic [31:0]   pma_cfg_c,
  input logic [63:0]   pma_cfg_a,
  input logic [31:0]   pma_cfg_x,
  input logic [1471:0] pma_addr,
  input logic [1535:0] pma_mask,
  input logic          pmp_checker_0_x,
  input logic          pmp_checker_1_x
);

  localparam logic [1:0] NO_STAGE    = 2'b00;
  localparam logic [1:0] ONLY_STAGE1 = 2'b01;
  localparam logic [1:0] ONLY_STAGE2 = 2'b10;
  localparam logic [1:0] ALL_STAGE   = 2'b11;
  localparam logic [1:0] PBMT_PMA    = 2'b00;
  localparam logic [1:0] PBMT_NC     = 2'b01;
  localparam logic [1:0] PBMT_IO     = 2'b10;
  localparam logic [1:0] PBMT_RSVD   = 2'b11;
  localparam logic [1:0] ATTR_CACHE  = 2'b00;
  localparam logic [1:0] ATTR_NC     = 2'b01;
  localparam logic [1:0] ATTR_MMIO   = 2'b10;
  localparam logic [1:0] ATTR_BAD    = 2'b11;


  logic        atp_timing_ptw_inflight;
  logic        atp_timing_seen_csr_changed;
  logic        atp_timing_seen_sfence;
  logic        atp_timing_seen_flush_pipe;
  logic [1:0]  atp_timing_inflight_s2xlate;
  logic [37:0] atp_timing_last_ptw_req_vpn;

  logic        atp_timing_refill_seen;
  logic [1:0]  atp_timing_refill_s2xlate;
  logic [37:0] atp_timing_refill_vpn;
  logic        atp_timing_superpage_refill_seen;
  logic [7:0]  atp_timing_superpage_lane_seen;
  logic [2:0]  atp_timing_miss_kind;
  logic [2:0]  atp_timing_refill_hit_kind;
  logic [2:0]  atp_timing_sector_lane_kind;
  logic [2:0]  atp_timing_sfence_scope;
  logic [2:0]  atp_timing_sfence_stage;

  wire atp_timing_csr_changed =
    io_tlbCsr_satp_changed ||
    io_tlbCsr_vsatp_changed ||
    io_tlbCsr_hgatp_changed ||
    io_tlbCsr_priv_virt_changed;

  wire atp_timing_ptw_req_fire = ptw_req_valid && ptw_req_ready;
  wire [37:0] atp_timing_itlb_req_vpn = itlb_req_vaddr[49:12];
  wire [2:0] atp_timing_ptw_resp_lane = atp_timing_last_ptw_req_vpn[2:0];
  wire [7:0] atp_timing_ptw_resp_lane_oh = 8'b1 << atp_timing_ptw_resp_lane;
  wire [7:0] atp_timing_itlb_req_lane_oh = 8'b1 << atp_timing_itlb_req_vpn[2:0];

  wire atp_timing_itlb_miss_to_ptw =
    itlb_req_valid &&
    itlb_resp_miss &&
    atp_timing_ptw_req_fire;

  wire atp_timing_refill_then_hit =
    atp_timing_refill_seen &&
    itlb_req_valid &&
    !itlb_resp_miss &&
    (atp_timing_itlb_req_vpn == atp_timing_refill_vpn);

  wire atp_timing_s1_4k_sector_resp =
    ptw_resp_valid &&
    (ptw_resp_s2xlate == NO_STAGE) &&
    ptw_resp_s1_v &&
    (ptw_resp_s1_entry_level == 2'b00);

  wire atp_timing_vs_4k_sector_resp =
    ptw_resp_valid &&
    (ptw_resp_s2xlate == ALL_STAGE) &&
    io_tlbCsr_priv_virt &&
    ptw_resp_s1_v &&
    (ptw_resp_s1_entry_level == 2'b00) &&
    !ptw_resp_s2_gpf &&
    !ptw_resp_s2_gaf;

  wire atp_timing_resp_lane_valid = ptw_resp_s1_valididx[atp_timing_ptw_resp_lane];
  wire atp_timing_resp_lane_pteidx = ptw_resp_s1_pteidx[atp_timing_ptw_resp_lane];
  wire atp_timing_s1_superpage_resp =
    ptw_resp_valid &&
    ptw_resp_s1_v &&
    (ptw_resp_s1_entry_level != 2'b00) &&
    !ptw_resp_s1_entry_n;
  wire atp_timing_superpage_new_lane_hit =
    atp_timing_superpage_refill_seen &&
    itlb_req_valid &&
    !itlb_resp_miss &&
    ((atp_timing_superpage_lane_seen & ~atp_timing_itlb_req_lane_oh) != 8'b0);

  always_comb begin
    atp_timing_miss_kind = 3'd0;
    if (atp_timing_itlb_miss_to_ptw && !io_tlbCsr_priv_virt &&
        ((io_tlbCsr_satp_mode == 4'h8) || (io_tlbCsr_satp_mode == 4'h9)) &&
        ptw_req_s2xlate == NO_STAGE)
      atp_timing_miss_kind = 3'd1;
    else if (atp_timing_itlb_miss_to_ptw && io_tlbCsr_priv_virt &&
             ((io_tlbCsr_vsatp_mode == 4'h8) || (io_tlbCsr_vsatp_mode == 4'h9)) &&
             io_tlbCsr_hgatp_mode == 4'h0 && ptw_req_s2xlate == ONLY_STAGE1)
      atp_timing_miss_kind = 3'd2;
    else if (atp_timing_itlb_miss_to_ptw && io_tlbCsr_priv_virt && io_tlbCsr_vsatp_mode == 4'h0 &&
             ((io_tlbCsr_hgatp_mode == 4'h8) || (io_tlbCsr_hgatp_mode == 4'h9)) &&
             ptw_req_s2xlate == ONLY_STAGE2)
      atp_timing_miss_kind = 3'd3;
    else if (atp_timing_itlb_miss_to_ptw && io_tlbCsr_priv_virt &&
             ((io_tlbCsr_vsatp_mode == 4'h8) || (io_tlbCsr_vsatp_mode == 4'h9)) &&
             ((io_tlbCsr_hgatp_mode == 4'h8) || (io_tlbCsr_hgatp_mode == 4'h9)) &&
             ptw_req_s2xlate == ALL_STAGE)
      atp_timing_miss_kind = 3'd4;

    atp_timing_refill_hit_kind = 3'd0;
    if (atp_timing_refill_then_hit) begin
      case (atp_timing_refill_s2xlate)
        NO_STAGE: atp_timing_refill_hit_kind = 3'd1;
        ONLY_STAGE1: atp_timing_refill_hit_kind = 3'd2;
        ONLY_STAGE2: atp_timing_refill_hit_kind = 3'd3;
        ALL_STAGE: atp_timing_refill_hit_kind = 3'd4;
        default: ;
      endcase
    end

    atp_timing_sector_lane_kind = 3'd0;
    if (atp_timing_s1_4k_sector_resp && atp_timing_resp_lane_valid) atp_timing_sector_lane_kind = 3'd1;
    else if (atp_timing_s1_4k_sector_resp && !atp_timing_resp_lane_valid) atp_timing_sector_lane_kind = 3'd2;
    else if (atp_timing_vs_4k_sector_resp && atp_timing_resp_lane_pteidx && atp_timing_resp_lane_valid)
      atp_timing_sector_lane_kind = 3'd3;
    else if (atp_timing_vs_4k_sector_resp && atp_timing_resp_lane_pteidx && !atp_timing_resp_lane_valid)
      atp_timing_sector_lane_kind = 3'd4;

    atp_timing_sfence_scope = 3'd0;
    if (atp_timing_refill_seen && sfence_valid) begin
      if (sfence_rs1 && sfence_rs2) atp_timing_sfence_scope = 3'd1;
      else if (!sfence_rs1 && sfence_rs2) atp_timing_sfence_scope = 3'd2;
      else if (sfence_rs1 && !sfence_rs2) atp_timing_sfence_scope = 3'd3;
      else atp_timing_sfence_scope = 3'd4;
    end

    atp_timing_sfence_stage = 3'd0;
    if (atp_timing_refill_seen && sfence_valid && !sfence_hv && !sfence_hg &&
        !io_tlbCsr_priv_virt)
      atp_timing_sfence_stage = 3'd1;
    else if (atp_timing_refill_seen && sfence_valid && sfence_hv && !sfence_hg &&
             io_tlbCsr_priv_virt && atp_timing_refill_s2xlate == ONLY_STAGE1)
      atp_timing_sfence_stage = 3'd2;
    else if (atp_timing_refill_seen && sfence_valid && !sfence_hv && sfence_hg &&
             io_tlbCsr_priv_virt && atp_timing_refill_s2xlate == ONLY_STAGE2)
      atp_timing_sfence_stage = 3'd3;
    else if (atp_timing_refill_seen && sfence_valid && sfence_hv && !sfence_hg &&
             io_tlbCsr_priv_virt && atp_timing_refill_s2xlate == ALL_STAGE)
      atp_timing_sfence_stage = 3'd4;
    else if (atp_timing_refill_seen && sfence_valid && !sfence_hv && sfence_hg &&
             io_tlbCsr_priv_virt && atp_timing_refill_s2xlate == ALL_STAGE)
      atp_timing_sfence_stage = 3'd5;
  end

  always_ff @(posedge clock) begin
    if (reset) begin
      atp_timing_ptw_inflight <= 1'b0;
      atp_timing_seen_csr_changed <= 1'b0;
      atp_timing_seen_sfence <= 1'b0;
      atp_timing_seen_flush_pipe <= 1'b0;
      atp_timing_inflight_s2xlate <= NO_STAGE;
      atp_timing_last_ptw_req_vpn <= '0;
      atp_timing_refill_seen <= 1'b0;
      atp_timing_refill_s2xlate <= NO_STAGE;
      atp_timing_refill_vpn <= '0;
      atp_timing_superpage_refill_seen <= 1'b0;
      atp_timing_superpage_lane_seen <= '0;
    end else begin
      if (atp_timing_ptw_req_fire) begin
        atp_timing_ptw_inflight <= 1'b1;
        atp_timing_seen_csr_changed <= 1'b0;
        atp_timing_seen_sfence <= 1'b0;
        atp_timing_seen_flush_pipe <= 1'b0;
        atp_timing_inflight_s2xlate <= ptw_req_s2xlate;
        atp_timing_last_ptw_req_vpn <= ptw_req_vpn;
      end

      if (atp_timing_ptw_inflight && atp_timing_csr_changed) begin
        atp_timing_seen_csr_changed <= 1'b1;
      end
      if (atp_timing_ptw_inflight && sfence_valid) begin
        atp_timing_seen_sfence <= 1'b1;
      end
      if (atp_timing_ptw_inflight && flush_pipe) begin
        atp_timing_seen_flush_pipe <= 1'b1;
      end

      if (ptw_resp_valid) begin
        atp_timing_ptw_inflight <= 1'b0;
        atp_timing_refill_seen <= 1'b1;
        atp_timing_refill_s2xlate <= ptw_resp_s2xlate;
        atp_timing_refill_vpn <= atp_timing_last_ptw_req_vpn;
      end

      if (atp_timing_s1_superpage_resp) begin
        atp_timing_superpage_refill_seen <= 1'b1;
        atp_timing_superpage_lane_seen <= atp_timing_ptw_resp_lane_oh;
      end else if (atp_timing_superpage_refill_seen && itlb_req_valid && !itlb_resp_miss) begin
        atp_timing_superpage_lane_seen <= atp_timing_superpage_lane_seen | atp_timing_itlb_req_lane_oh;
      end

      if (atp_timing_csr_changed || sfence_valid || flush_pipe) begin
        atp_timing_refill_seen <= 1'b0;
        atp_timing_superpage_refill_seen <= 1'b0;
        atp_timing_superpage_lane_seen <= '0;
      end
    end
  end


  logic [31:0] pmp_match_0;
  logic [31:0] pmp_match_1;
  logic [31:0] pma_match_0;
  logic [31:0] pma_match_1;

  genvar entry;
  generate
    for (entry = 0; entry < 32; entry = entry + 1) begin : gen_entry_match
      wire [1:0] pmp_entry_a = pmp_cfg_a[entry * 2 +: 2];
      wire [47:0] pmp_entry_addr = {pmp_addr[entry * 46 + 10 +: 36], 12'h0};
      wire [47:0] pmp_prev_addr;
      wire [47:0] pmp_entry_mask = pmp_mask[entry * 48 +: 48];
      wire [1:0] pma_entry_a = pma_cfg_a[entry * 2 +: 2];
      wire [47:0] pma_entry_addr = {pma_addr[entry * 46 + 10 +: 36], 12'h0};
      wire [47:0] pma_prev_addr;
      wire [47:0] pma_entry_mask = pma_mask[entry * 48 +: 48];

      if (entry == 0) begin : first_entry
        assign pmp_prev_addr = 48'h0;
        assign pma_prev_addr = 48'h0;
      end else begin : later_entry
        assign pmp_prev_addr = {pmp_addr[(entry - 1) * 46 + 10 +: 36], 12'h0};
        assign pma_prev_addr = {pma_addr[(entry - 1) * 46 + 10 +: 36], 12'h0};
      end

      assign pmp_match_0[entry] = pmp_entry_a[1]
        ? ((pmp_addr_0 & ~pmp_entry_mask) == (pmp_entry_addr & ~pmp_entry_mask))
        : ((pmp_entry_a == 2'b01) &&
           (pmp_addr_0 >= pmp_prev_addr) && (pmp_addr_0 < pmp_entry_addr));
      assign pmp_match_1[entry] = pmp_entry_a[1]
        ? ((pmp_addr_1 & ~pmp_entry_mask) == (pmp_entry_addr & ~pmp_entry_mask))
        : ((pmp_entry_a == 2'b01) &&
           (pmp_addr_1 >= pmp_prev_addr) && (pmp_addr_1 < pmp_entry_addr));
      assign pma_match_0[entry] = pma_entry_a[1]
        ? ((pmp_addr_0 & ~pma_entry_mask) == (pma_entry_addr & ~pma_entry_mask))
        : ((pma_entry_a == 2'b01) &&
           (pmp_addr_0 >= pma_prev_addr) && (pmp_addr_0 < pma_entry_addr));
      assign pma_match_1[entry] = pma_entry_a[1]
        ? ((pmp_addr_1 & ~pma_entry_mask) == (pma_entry_addr & ~pma_entry_mask))
        : ((pma_entry_a == 2'b01) &&
           (pmp_addr_1 >= pma_prev_addr) && (pmp_addr_1 < pma_entry_addr));
    end
  endgenerate

  logic pmp_sel_0_valid;
  logic pmp_sel_1_valid;
  logic pma_sel_0_valid;
  logic pma_sel_1_valid;
  logic [4:0] pmp_sel_0_idx;
  logic [4:0] pma_sel_0_idx;
  logic [4:0] pma_sel_1_idx;
  logic pmp_sel_0_l;
  logic pmp_sel_0_x;
  logic [1:0] pmp_sel_0_a;
  logic [47:0] pmp_sel_0_lower;
  logic [47:0] pmp_sel_0_upper;
  logic [47:0] pmp_sel_0_mask;
  logic pmp_sel_1_x;
  logic pma_sel_0_c;
  logic pma_sel_0_x;
  logic [47:0] pma_sel_0_lower;
  logic [47:0] pma_sel_0_upper;
  logic pma_sel_1_c;
  logic pma_sel_1_x;
  integer pmp_match_count_0;
  integer idx;

  always_comb begin
    pmp_sel_0_valid = 1'b0;
    pmp_sel_1_valid = 1'b0;
    pma_sel_0_valid = 1'b0;
    pma_sel_1_valid = 1'b0;
    pmp_sel_0_idx = '0;
    pma_sel_0_idx = '0;
    pma_sel_1_idx = '0;
    pmp_sel_0_l = 1'b0;
    pmp_sel_0_x = 1'b0;
    pmp_sel_0_a = 2'b00;
    pmp_sel_0_lower = '0;
    pmp_sel_0_upper = '0;
    pmp_sel_0_mask = '0;
    pmp_sel_1_x = 1'b0;
    pma_sel_0_c = 1'b0;
    pma_sel_0_x = 1'b0;
    pma_sel_0_lower = '0;
    pma_sel_0_upper = '0;
    pma_sel_1_c = 1'b0;
    pma_sel_1_x = 1'b0;
    pmp_match_count_0 = 0;

    for (idx = 0; idx < 32; idx = idx + 1) begin
      if (pmp_match_0[idx]) begin
        pmp_match_count_0 = pmp_match_count_0 + 1;
      end
      if (!pmp_sel_0_valid && pmp_match_0[idx]) begin
        pmp_sel_0_valid = 1'b1;
        pmp_sel_0_idx = idx[4:0];
        pmp_sel_0_l = pmp_cfg_l[idx];
        pmp_sel_0_x = pmp_cfg_x[idx];
        pmp_sel_0_a = pmp_cfg_a[idx * 2 +: 2];
        pmp_sel_0_mask = pmp_mask[idx * 48 +: 48];
        pmp_sel_0_upper = pmp_cfg_a[idx * 2 +: 2] == 2'b01
          ? {pmp_addr[idx * 46 + 10 +: 36], 12'h0}
          : ({pmp_addr[idx * 46 + 10 +: 36], 12'h0} |
             pmp_mask[idx * 48 +: 48]);
        pmp_sel_0_lower = pmp_cfg_a[idx * 2 +: 2] == 2'b01
          ? (idx == 0 ? 48'h0 : {pmp_addr[(idx - 1) * 46 + 10 +: 36], 12'h0})
          : ({pmp_addr[idx * 46 + 10 +: 36], 12'h0} &
             ~pmp_mask[idx * 48 +: 48]);
      end
      if (!pmp_sel_1_valid && pmp_match_1[idx]) begin
        pmp_sel_1_valid = 1'b1;
        pmp_sel_1_x = pmp_cfg_x[idx];
      end
      if (!pma_sel_0_valid && pma_match_0[idx]) begin
        pma_sel_0_valid = 1'b1;
        pma_sel_0_idx = idx[4:0];
        pma_sel_0_c = pma_cfg_c[idx];
        pma_sel_0_x = pma_cfg_x[idx];
        pma_sel_0_upper = pma_cfg_a[idx * 2 +: 2] == 2'b01
          ? {pma_addr[idx * 46 + 10 +: 36], 12'h0}
          : ({pma_addr[idx * 46 + 10 +: 36], 12'h0} |
             pma_mask[idx * 48 +: 48]);
        pma_sel_0_lower = pma_cfg_a[idx * 2 +: 2] == 2'b01
          ? (idx == 0 ? 48'h0 : {pma_addr[(idx - 1) * 46 + 10 +: 36], 12'h0})
          : ({pma_addr[idx * 46 + 10 +: 36], 12'h0} &
             ~pma_mask[idx * 48 +: 48]);
      end
      if (!pma_sel_1_valid && pma_match_1[idx]) begin
        pma_sel_1_valid = 1'b1;
        pma_sel_1_idx = idx[4:0];
        pma_sel_1_c = pma_cfg_c[idx];
        pma_sel_1_x = pma_cfg_x[idx];
      end
    end
  end

  wire ptw_req_fire = ptw_req_valid && ptw_req_ready;
  wire fetch_sample = itlb_req_valid && !itlb_resp_miss;
  wire atp_no_translation_fault = !(itlb_resp_pf_instr ||
    itlb_resp_af_instr || itlb_resp_gpf_instr);
  wire [37:0] itlb_req_vpn = itlb_req_vaddr[49:12];
  wire pmp_pma_allow_0 = pmp_sel_0_valid && pma_sel_0_valid &&
    pmp_sel_0_x && pma_sel_0_x;
  wire pmp_pma_allow_both = pmp_pma_allow_0 && pmp_sel_1_valid &&
    pma_sel_1_valid && pmp_sel_1_x && pma_sel_1_x;
  wire [1:0] current_attr = itlb_resp_pbmt == PBMT_NC ? ATTR_NC :
    itlb_resp_pbmt == PBMT_IO ? ATTR_MMIO :
    itlb_resp_pbmt == PBMT_PMA ? (pma_sel_0_c ? ATTR_CACHE : ATTR_MMIO) :
    ATTR_BAD;
  wire current_attr_valid = fetch_sample && pmp_pma_allow_0 &&
    (current_attr != ATTR_BAD);
  wire [9:0] ptw_translation_signature = {
    ptw_resp_s1_v,
    ptw_resp_s1_pbmt,
    ptw_resp_s2_pbmt,
    ptw_resp_s1_perm_x,
    ptw_resp_s1_perm_a,
    ptw_resp_s1_perm_u,
    ptw_resp_s2_perm_x,
    ptw_resp_s2_perm_a
  };
  wire atp_tlb_csr_changed = io_tlbCsr_satp_changed ||
    io_tlbCsr_vsatp_changed || io_tlbCsr_hgatp_changed ||
    io_tlbCsr_priv_virt_changed;

  logic atp_ptw_inflight;
  logic [37:0] atp_inflight_vpn;
  logic [1:0] atp_inflight_s2xlate;
  logic [15:0] atp_inflight_satp_asid;
  logic [15:0] atp_inflight_vsatp_asid;
  logic [15:0] atp_inflight_hgatp_vmid;
  logic atp_refill_seen;
  logic [37:0] atp_refill_vpn;
  logic [1:0] atp_refill_s2xlate;
  logic [1:0] atp_refill_s1_pbmt;
  logic [1:0] atp_refill_s2_pbmt;
  logic [15:0] atp_refill_satp_asid;
  logic [15:0] atp_refill_vsatp_asid;
  logic [15:0] atp_refill_hgatp_vmid;
  logic atp_refill_priv_virt;
  logic atp_last_attr_valid;
  logic [37:0] atp_last_attr_vpn;
  logic [1:0] atp_last_attr;
  logic atp_last_translation_valid;
  logic [37:0] atp_last_translation_vpn;
  logic [9:0] atp_last_translation_signature;
  logic atp_sfence_attr_pending;
  logic [37:0] atp_sfence_attr_vpn;
  logic [1:0] atp_sfence_old_attr;
  logic atp_sfence_translation_pending;
  logic [37:0] atp_sfence_translation_vpn;
  logic [9:0] atp_sfence_old_translation_signature;
  logic atp_csr_translation_pending;
  logic [37:0] atp_csr_translation_vpn;
  logic [9:0] atp_csr_old_translation_signature;
  logic atp_redirect_pending;
  logic [1:0] atp_redirect_old_attr;
  logic [2:0] atp_sfence_attr_transition;
  logic [2:0] atp_pmp_lock_mode;
  logic [1:0] atp_tor_boundary;
  logic [1:0] atp_napot_boundary;
  logic atp_cross_pma_attribute;
  logic [2:0] atp_redirect_attr_transition;
  logic [3:0] atp_translation_mode;

  always_ff @(posedge clock) begin
    if (reset) begin
      atp_ptw_inflight <= 1'b0;
      atp_inflight_vpn <= '0;
      atp_inflight_s2xlate <= NO_STAGE;
      atp_inflight_satp_asid <= '0;
      atp_inflight_vsatp_asid <= '0;
      atp_inflight_hgatp_vmid <= '0;
      atp_refill_seen <= 1'b0;
      atp_refill_vpn <= '0;
      atp_refill_s2xlate <= NO_STAGE;
      atp_refill_s1_pbmt <= PBMT_PMA;
      atp_refill_s2_pbmt <= PBMT_PMA;
      atp_refill_satp_asid <= '0;
      atp_refill_vsatp_asid <= '0;
      atp_refill_hgatp_vmid <= '0;
      atp_refill_priv_virt <= 1'b0;
      atp_last_attr_valid <= 1'b0;
      atp_last_attr_vpn <= '0;
      atp_last_attr <= ATTR_BAD;
      atp_last_translation_valid <= 1'b0;
      atp_last_translation_vpn <= '0;
      atp_last_translation_signature <= '0;
      atp_sfence_attr_pending <= 1'b0;
      atp_sfence_attr_vpn <= '0;
      atp_sfence_old_attr <= ATTR_BAD;
      atp_sfence_translation_pending <= 1'b0;
      atp_sfence_translation_vpn <= '0;
      atp_sfence_old_translation_signature <= '0;
      atp_csr_translation_pending <= 1'b0;
      atp_csr_translation_vpn <= '0;
      atp_csr_old_translation_signature <= '0;
      atp_redirect_pending <= 1'b0;
      atp_redirect_old_attr <= ATTR_BAD;
    end else begin
      if (ptw_req_fire) begin
        atp_ptw_inflight <= 1'b1;
        atp_inflight_vpn <= ptw_req_vpn;
        atp_inflight_s2xlate <= ptw_req_s2xlate;
        atp_inflight_satp_asid <= io_tlbCsr_satp_asid;
        atp_inflight_vsatp_asid <= io_tlbCsr_vsatp_asid;
        atp_inflight_hgatp_vmid <= io_tlbCsr_hgatp_vmid;
      end
      if (ptw_resp_valid) begin
        atp_ptw_inflight <= 1'b0;
        atp_refill_seen <= 1'b1;
        atp_refill_vpn <= atp_inflight_vpn;
        atp_refill_s2xlate <= ptw_resp_s2xlate;
        atp_refill_s1_pbmt <= ptw_resp_s1_pbmt;
        atp_refill_s2_pbmt <= ptw_resp_s2_pbmt;
        atp_refill_satp_asid <= io_tlbCsr_satp_asid;
        atp_refill_vsatp_asid <= io_tlbCsr_vsatp_asid;
        atp_refill_hgatp_vmid <= io_tlbCsr_hgatp_vmid;
        atp_refill_priv_virt <= io_tlbCsr_priv_virt;
        atp_last_translation_valid <= 1'b1;
        atp_last_translation_vpn <= atp_inflight_vpn;
        atp_last_translation_signature <= ptw_translation_signature;
        if (atp_sfence_translation_pending &&
            atp_inflight_vpn == atp_sfence_translation_vpn)
          atp_sfence_translation_pending <= 1'b0;
        if (atp_csr_translation_pending &&
            atp_inflight_vpn == atp_csr_translation_vpn)
          atp_csr_translation_pending <= 1'b0;
      end
      if (current_attr_valid) begin
        atp_last_attr_valid <= 1'b1;
        atp_last_attr_vpn <= itlb_req_vpn;
        atp_last_attr <= current_attr;
        atp_redirect_pending <= 1'b0;
        if (atp_sfence_attr_pending && itlb_req_vpn == atp_sfence_attr_vpn) begin
          atp_sfence_attr_pending <= 1'b0;
        end
      end
      if (sfence_valid && atp_last_attr_valid &&
          (sfence_rs1 || sfence_addr[49:12] == atp_last_attr_vpn)) begin
        atp_sfence_attr_pending <= 1'b1;
        atp_sfence_attr_vpn <= atp_last_attr_vpn;
        atp_sfence_old_attr <= atp_last_attr;
      end
      if (sfence_valid && atp_last_translation_valid &&
          (sfence_rs1 || sfence_addr[49:12] == atp_last_translation_vpn)) begin
        atp_sfence_translation_pending <= 1'b1;
        atp_sfence_translation_vpn <= atp_last_translation_vpn;
        atp_sfence_old_translation_signature <= atp_last_translation_signature;
      end
      if (atp_tlb_csr_changed && atp_last_translation_valid) begin
        atp_csr_translation_pending <= 1'b1;
        atp_csr_translation_vpn <= atp_last_translation_vpn;
        atp_csr_old_translation_signature <= atp_last_translation_signature;
      end
      if (redirect_valid && atp_last_attr_valid) begin
        atp_redirect_pending <= 1'b1;
        atp_redirect_old_attr <= atp_last_attr;
      end
    end
  end

  wire atp_satp_asid_changed_after_refill = atp_refill_seen &&
    !atp_refill_priv_virt && io_tlbCsr_satp_changed &&
    (io_tlbCsr_satp_asid != atp_refill_satp_asid);
  wire atp_vsatp_asid_changed_after_refill = atp_refill_seen &&
    atp_refill_priv_virt && io_tlbCsr_vsatp_changed &&
    (io_tlbCsr_vsatp_asid != atp_refill_vsatp_asid);
  wire atp_hgatp_vmid_changed_after_refill = atp_refill_seen &&
    atp_refill_priv_virt && io_tlbCsr_hgatp_changed &&
    (io_tlbCsr_hgatp_vmid != atp_refill_hgatp_vmid);
  wire atp_priv_virt_changed_after_refill = atp_refill_seen &&
    io_tlbCsr_priv_virt_changed &&
    (io_tlbCsr_priv_virt != atp_refill_priv_virt);
  wire atp_sfence_addr_mismatch = !sfence_rs1 &&
    (sfence_addr[49:12] != atp_inflight_vpn);
  wire atp_sfence_id_mismatch = !sfence_rs2 &&
    ((sfence_hg && sfence_id != atp_inflight_hgatp_vmid) ||
     (!sfence_hg && sfence_id !=
       (atp_inflight_s2xlate == NO_STAGE
         ? atp_inflight_satp_asid : atp_inflight_vsatp_asid)));
  wire atp_sfence_domain_mismatch =
    (sfence_hg && atp_inflight_s2xlate != ONLY_STAGE2 &&
     atp_inflight_s2xlate != ALL_STAGE) ||
    (sfence_hv && atp_inflight_s2xlate != ONLY_STAGE1 &&
     atp_inflight_s2xlate != ALL_STAGE);
  wire atp_unmatched_sfence_during_ptw_wait = atp_ptw_inflight &&
    sfence_valid && (atp_sfence_addr_mismatch || atp_sfence_id_mismatch ||
                     atp_sfence_domain_mismatch);

  wire atp_sfence_attr_cacheable_to_mmio = current_attr_valid &&
    atp_sfence_attr_pending && itlb_req_vpn == atp_sfence_attr_vpn &&
    atp_sfence_old_attr == ATTR_CACHE && current_attr == ATTR_MMIO;
  wire atp_sfence_attr_cacheable_to_nc = current_attr_valid &&
    atp_sfence_attr_pending && itlb_req_vpn == atp_sfence_attr_vpn &&
    atp_sfence_old_attr == ATTR_CACHE && current_attr == ATTR_NC;
  wire atp_sfence_attr_mmio_to_cacheable = current_attr_valid &&
    atp_sfence_attr_pending && itlb_req_vpn == atp_sfence_attr_vpn &&
    atp_sfence_old_attr == ATTR_MMIO && current_attr == ATTR_CACHE;
  wire atp_sfence_attr_mmio_to_nc = current_attr_valid &&
    atp_sfence_attr_pending && itlb_req_vpn == atp_sfence_attr_vpn &&
    atp_sfence_old_attr == ATTR_MMIO && current_attr == ATTR_NC;
  wire atp_sfence_attr_nc_to_cacheable = current_attr_valid &&
    atp_sfence_attr_pending && itlb_req_vpn == atp_sfence_attr_vpn &&
    atp_sfence_old_attr == ATTR_NC && current_attr == ATTR_CACHE;
  wire atp_sfence_attr_nc_to_mmio = current_attr_valid &&
    atp_sfence_attr_pending && itlb_req_vpn == atp_sfence_attr_vpn &&
    atp_sfence_old_attr == ATTR_NC && current_attr == ATTR_MMIO;
  wire atp_sfence_translation_changed = ptw_resp_valid &&
    atp_sfence_translation_pending &&
    atp_inflight_vpn == atp_sfence_translation_vpn &&
    ptw_translation_signature != atp_sfence_old_translation_signature;
  wire atp_tlb_csr_translation_changed = ptw_resp_valid &&
    atp_csr_translation_pending &&
    atp_inflight_vpn == atp_csr_translation_vpn &&
    ptw_translation_signature != atp_csr_old_translation_signature;

  wire atp_pmp_pma_execute_allow = fetch_sample && pmp_sel_0_valid &&
    pma_sel_0_valid && pmp_sel_0_x && pma_sel_0_x;
  wire atp_pmp_or_pma_execute_deny = fetch_sample && pmp_sel_0_valid &&
    pma_sel_0_valid && (!pmp_sel_0_x || !pma_sel_0_x);
  wire atp_locked_m_mode_allow = fetch_sample && io_tlbCsr_priv_imode[1] &&
    pmp_sel_0_valid && pmp_sel_0_l && pmp_sel_0_x && pma_sel_0_x;
  wire atp_locked_m_mode_deny = fetch_sample && io_tlbCsr_priv_imode[1] &&
    pmp_sel_0_valid && pmp_sel_0_l && !pmp_sel_0_x && pma_sel_0_x;
  wire atp_locked_su_mode_allow = fetch_sample && !io_tlbCsr_priv_imode[1] &&
    pmp_sel_0_valid && pmp_sel_0_l && pmp_sel_0_x && pma_sel_0_x;
  wire atp_locked_su_mode_deny = fetch_sample && !io_tlbCsr_priv_imode[1] &&
    pmp_sel_0_valid && pmp_sel_0_l && !pmp_sel_0_x && pma_sel_0_x;
  wire atp_unlocked_m_mode_bypass = fetch_sample && io_tlbCsr_priv_imode[1] &&
    pmp_sel_0_valid && !pmp_sel_0_l && !pmp_sel_0_x && pma_sel_0_x;
  wire atp_tor_lower_boundary = fetch_sample && pmp_sel_0_a == 2'b01 &&
    pmp_sel_0_x && pmp_addr_0 == pmp_sel_0_lower;
  wire atp_tor_upper_boundary = fetch_sample && pmp_sel_0_idx != 0 &&
    pmp_cfg_a[(pmp_sel_0_idx - 1) * 2 +: 2] == 2'b01 &&
    !pmp_cfg_x[pmp_sel_0_idx - 1] &&
    pmp_addr_0 == {pmp_addr[(pmp_sel_0_idx - 1) * 46 + 10 +: 36], 12'h0};
  wire atp_tor_inside_range = fetch_sample && pmp_sel_0_a == 2'b01 &&
    pmp_sel_0_x && pmp_addr_0 > pmp_sel_0_lower &&
    pmp_addr_0 < pmp_sel_0_upper;
  wire atp_napot_lower_boundary = fetch_sample && pmp_sel_0_a == 2'b11 &&
    pmp_sel_0_x && pmp_addr_0 == pmp_sel_0_lower;
  wire atp_napot_upper_boundary = fetch_sample && pmp_sel_0_a == 2'b11 &&
    pmp_sel_0_x && pmp_addr_0 == pmp_sel_0_upper;
  wire atp_napot_inside_range = fetch_sample && pmp_sel_0_a == 2'b11 &&
    pmp_sel_0_x && pmp_addr_0 > pmp_sel_0_lower &&
    pmp_addr_0 < pmp_sel_0_upper;
  wire atp_pmp_overlap_low_index = fetch_sample && pmp_match_count_0 > 1;

  // Permission-source observations are sampled from the request-side
  // PMP/PMA decisions only.  They intentionally do not depend on the
  // eventual frontend exception/cfVec result.
  wire atp_pmp0_allow = pmp_checker_0_x;
  wire atp_pmp1_allow = pmp_checker_1_x;
  wire atp_pma0_allow = pma_sel_0_valid && pma_sel_0_x;
  wire atp_pma1_allow = pma_sel_1_valid && pma_sel_1_x;
  wire atp_cacheable_request = fetch_sample && itlb_resp_pbmt == PBMT_PMA &&
    pma_sel_0_valid && pma_sel_0_c;
  wire atp_uncache_request = fetch_sample &&
    (itlb_resp_pbmt == PBMT_NC || itlb_resp_pbmt == PBMT_IO);
  wire atp_cacheable_pmp_only_deny = atp_cacheable_request && atp_no_translation_fault &&
    ((!atp_pmp0_allow && atp_pma0_allow) ||
     (!atp_pmp1_allow && atp_pma1_allow));
  wire atp_cacheable_pma_only_deny = atp_cacheable_request && atp_no_translation_fault &&
    ((atp_pmp0_allow && !atp_pma0_allow) ||
     (atp_pmp1_allow && !atp_pma1_allow));
  wire atp_cacheable_both_deny = atp_cacheable_request && atp_no_translation_fault &&
    ((!atp_pmp0_allow && !atp_pma0_allow) ||
     (!atp_pmp1_allow && !atp_pma1_allow));
  wire atp_uncache_pmp_only_deny = atp_uncache_request && atp_no_translation_fault &&
    ((!atp_pmp0_allow && atp_pma0_allow) ||
     (!atp_pmp1_allow && atp_pma1_allow));
  wire atp_uncache_pma_only_deny = atp_uncache_request && atp_no_translation_fault &&
    ((atp_pmp0_allow && !atp_pma0_allow) ||
     (atp_pmp1_allow && !atp_pma1_allow));
  wire atp_uncache_both_deny = atp_uncache_request && atp_no_translation_fault &&
    ((!atp_pmp0_allow && !atp_pma0_allow) ||
     (!atp_pmp1_allow && !atp_pma1_allow));

  wire atp_cross_pma_same_attr = fetch_sample && pmp_addr_0 != pmp_addr_1 &&
    pma_sel_0_valid && pma_sel_1_valid && pma_sel_0_idx != pma_sel_1_idx &&
    pma_sel_0_c == pma_sel_1_c && pmp_pma_allow_both;
  wire atp_cross_pma_diff_attr = fetch_sample && pmp_addr_0 != pmp_addr_1 &&
    pma_sel_0_valid && pma_sel_1_valid && pma_sel_0_idx != pma_sel_1_idx &&
    pma_sel_0_c != pma_sel_1_c && pmp_pma_allow_both;
  wire atp_nonvirtual_bare = !io_tlbCsr_priv_virt &&
    io_tlbCsr_satp_mode == 4'h0 && io_tlbCsr_vsatp_mode == 4'h0 &&
    io_tlbCsr_hgatp_mode == 4'h0;
  wire atp_refill_fetch = current_attr_valid && atp_refill_seen &&
    itlb_req_vpn == atp_refill_vpn;
  wire atp_nonvirtual_bare_pma_cacheable = atp_nonvirtual_bare &&
    current_attr_valid && itlb_resp_pbmt == PBMT_PMA && pma_sel_0_c;
  wire atp_nonvirtual_ptw_pma_cacheable = !io_tlbCsr_priv_virt &&
    atp_refill_fetch && atp_refill_s1_pbmt == PBMT_PMA && pma_sel_0_c;
  wire atp_nonvirtual_bare_pma_mmio = atp_nonvirtual_bare &&
    current_attr_valid && itlb_resp_pbmt == PBMT_PMA && !pma_sel_0_c;
  wire atp_nonvirtual_ptw_pma_mmio = !io_tlbCsr_priv_virt &&
    atp_refill_fetch && atp_refill_s1_pbmt == PBMT_PMA && !pma_sel_0_c;
  wire atp_nonvirtual_pbmt_nc_or_io = !io_tlbCsr_priv_virt &&
    atp_refill_fetch && (atp_refill_s1_pbmt == PBMT_NC ||
                         atp_refill_s1_pbmt == PBMT_IO);
  wire atp_virtual_all_pbmt_pma = atp_refill_s2xlate == ONLY_STAGE1
    ? atp_refill_s1_pbmt == PBMT_PMA
    : atp_refill_s2xlate == ONLY_STAGE2
      ? atp_refill_s2_pbmt == PBMT_PMA
      : atp_refill_s2xlate == ALL_STAGE &&
        atp_refill_s1_pbmt == PBMT_PMA && atp_refill_s2_pbmt == PBMT_PMA;
  wire atp_virtual_all_pbmt_pma_cacheable = io_tlbCsr_priv_virt &&
    atp_refill_fetch && atp_virtual_all_pbmt_pma && pma_sel_0_c;
  wire atp_virtual_all_pbmt_pma_mmio = io_tlbCsr_priv_virt &&
    atp_refill_fetch && atp_virtual_all_pbmt_pma && !pma_sel_0_c;
  wire atp_virtual_single_stage_pbmt_nc_or_io = io_tlbCsr_priv_virt &&
    atp_refill_fetch &&
    (((atp_refill_s2xlate == ONLY_STAGE1) &&
      (atp_refill_s1_pbmt == PBMT_NC || atp_refill_s1_pbmt == PBMT_IO)) ||
     ((atp_refill_s2xlate == ONLY_STAGE2) &&
      (atp_refill_s2_pbmt == PBMT_NC || atp_refill_s2_pbmt == PBMT_IO)));
  wire atp_all_stage_g_pbmt_nc_or_io = io_tlbCsr_priv_virt &&
    atp_refill_fetch && atp_refill_s2xlate == ALL_STAGE &&
    atp_refill_s1_pbmt == PBMT_PMA &&
    (atp_refill_s2_pbmt == PBMT_NC || atp_refill_s2_pbmt == PBMT_IO);
  wire atp_all_stage_vs_pbmt_nc_or_io = io_tlbCsr_priv_virt &&
    atp_refill_fetch && atp_refill_s2xlate == ALL_STAGE &&
    (atp_refill_s1_pbmt == PBMT_NC || atp_refill_s1_pbmt == PBMT_IO);

  wire atp_redirect_cacheable_to_mmio = current_attr_valid &&
    atp_redirect_pending && atp_redirect_old_attr == ATTR_CACHE &&
    current_attr == ATTR_MMIO;
  wire atp_redirect_cacheable_to_nc = current_attr_valid &&
    atp_redirect_pending && atp_redirect_old_attr == ATTR_CACHE &&
    current_attr == ATTR_NC && backend_can_accept;
  wire atp_redirect_mmio_to_cacheable = current_attr_valid &&
    atp_redirect_pending && atp_redirect_old_attr == ATTR_MMIO &&
    current_attr == ATTR_CACHE;
  wire atp_mmio_to_nc = current_attr_valid && atp_last_attr_valid &&
    atp_last_attr == ATTR_MMIO && current_attr == ATTR_NC && backend_can_accept;
  wire atp_redirect_nc_to_cacheable = current_attr_valid &&
    atp_redirect_pending && atp_redirect_old_attr == ATTR_NC &&
    current_attr == ATTR_CACHE && backend_can_accept;
  wire atp_nc_to_mmio = current_attr_valid && atp_last_attr_valid &&
    atp_last_attr == ATTR_NC && current_attr == ATTR_MMIO && backend_can_accept;
  wire atp_pbmt_nc_at_pma_edge = current_attr_valid &&
    itlb_resp_pbmt == PBMT_NC && pma_sel_0_valid &&
    pmp_addr_0 >= pma_sel_0_upper - 48'd4 && pmp_addr_0 < pma_sel_0_upper;
  wire atp_pbmt_nc_after_pma_boundary = current_attr_valid &&
    itlb_resp_pbmt == PBMT_NC && pma_sel_0_valid && pma_sel_0_idx != 0 &&
    pmp_addr_0 == pma_sel_0_lower &&
    pma_cfg_c[pma_sel_0_idx] != pma_cfg_c[pma_sel_0_idx - 1];
  wire atp_pbmt_nc_cross_pma_regions = current_attr_valid &&
    itlb_resp_pbmt == PBMT_NC && pma_sel_0_valid && pma_sel_1_valid &&
    pma_sel_0_idx != pma_sel_1_idx && pma_sel_0_c != pma_sel_1_c;
  wire atp_reserved_pbmt_encoding = ptw_resp_valid && backend_can_accept &&
    pma_sel_0_valid && pma_sel_0_x && ptw_resp_s1_v &&
    ptw_resp_s1_perm_a && ptw_resp_s1_perm_x && !ptw_resp_s1_pf &&
    !ptw_resp_s1_af && !ptw_resp_s2_gpf && !ptw_resp_s2_gaf &&
    (ptw_resp_s1_pbmt == PBMT_RSVD || ptw_resp_s2_pbmt == PBMT_RSVD);
  wire atp_translation_observed = itlb_req_valid &&
    (!itlb_resp_miss || ptw_req_fire);
  wire atp_s1_leaf_executable = ptw_resp_valid && ptw_resp_s1_v &&
    ptw_resp_s1_perm_x && (!ptw_resp_s1_perm_w || ptw_resp_s1_perm_r);
  wire atp_s1_leaf_not_executable = ptw_resp_valid && ptw_resp_s1_v &&
    !ptw_resp_s1_perm_x;
  wire atp_s1_leaf_accessed_clear = ptw_resp_valid && ptw_resp_s1_v &&
    !ptw_resp_s1_perm_a;
  wire atp_s1_privilege_denied = ptw_resp_valid && ptw_resp_s1_v &&
    ((io_tlbCsr_priv_imode == 2'b00 && !ptw_resp_s1_perm_u) ||
     (io_tlbCsr_priv_imode == 2'b01 && ptw_resp_s1_perm_u));
  wire atp_s1_write_without_read = ptw_resp_valid && ptw_resp_s1_v &&
    ptw_resp_s1_perm_w && !ptw_resp_s1_perm_r;
  wire atp_s2_leaf_executable = ptw_resp_valid &&
    ptw_resp_s2xlate != NO_STAGE && ptw_resp_s2_perm_x &&
    ptw_resp_s2_perm_a && !ptw_resp_s2_gpf && !ptw_resp_s2_gaf;
  wire atp_s2_leaf_not_executable = ptw_resp_valid &&
    ptw_resp_s2xlate != NO_STAGE && !ptw_resp_s2_perm_x &&
    !ptw_resp_s2_gpf && !ptw_resp_s2_gaf;
  wire atp_s2_leaf_accessed_clear = ptw_resp_valid &&
    ptw_resp_s2xlate != NO_STAGE && ptw_resp_s2_perm_x &&
    !ptw_resp_s2_perm_a && !ptw_resp_s2_gpf && !ptw_resp_s2_gaf;
  wire atp_only_stage2_gpf = ptw_resp_valid &&
    ptw_resp_s2xlate == ONLY_STAGE2 && ptw_resp_s2_gpf && !ptw_resp_s2_gaf;
  wire atp_only_stage2_gaf = ptw_resp_valid &&
    ptw_resp_s2xlate == ONLY_STAGE2 && ptw_resp_s2_gaf && !ptw_resp_s2_gpf;
  wire atp_all_stage_fault_context = ptw_resp_valid &&
    ptw_resp_s2xlate == ALL_STAGE;
  wire atp_all_stage_vs_leaf_gpf = atp_all_stage_fault_context &&
    ptw_resp_s1_v && ptw_resp_s1_perm_x &&
    (!ptw_resp_s1_perm_w || ptw_resp_s1_perm_r) && !ptw_resp_s1_pf &&
    !ptw_resp_s1_af && ptw_resp_s2_gpf && !ptw_resp_s2_gaf;
  wire atp_all_stage_vs_leaf_gaf = atp_all_stage_fault_context &&
    ptw_resp_s1_v && ptw_resp_s1_perm_x &&
    (!ptw_resp_s1_perm_w || ptw_resp_s1_perm_r) && !ptw_resp_s1_pf &&
    !ptw_resp_s1_af && ptw_resp_s2_gaf && !ptw_resp_s2_gpf;
  wire atp_all_stage_vs_fake_gpf = atp_all_stage_fault_context &&
    !ptw_resp_s1_v && !ptw_resp_s1_pf && !ptw_resp_s1_af &&
    ptw_resp_s2_gpf && !ptw_resp_s2_gaf;
  wire atp_all_stage_vs_fake_gaf = atp_all_stage_fault_context &&
    !ptw_resp_s1_v && !ptw_resp_s1_pf && !ptw_resp_s1_af &&
    ptw_resp_s2_gaf && !ptw_resp_s2_gpf;
  wire atp_all_stage_vs_nonleaf_gpf = atp_all_stage_fault_context &&
    ptw_resp_s1_v && !ptw_resp_s1_perm_r && !ptw_resp_s1_perm_w &&
    !ptw_resp_s1_perm_x && !ptw_resp_s1_pf && !ptw_resp_s1_af &&
    ptw_resp_s2_gpf && !ptw_resp_s2_gaf;
  wire atp_all_stage_vs_nonleaf_gaf = atp_all_stage_fault_context &&
    ptw_resp_s1_v && !ptw_resp_s1_perm_r && !ptw_resp_s1_perm_w &&
    !ptw_resp_s1_perm_x && !ptw_resp_s1_pf && !ptw_resp_s1_af &&
    ptw_resp_s2_gaf && !ptw_resp_s2_gpf;
  wire atp_all_stage_s1_pf_s2_leaf = atp_all_stage_fault_context &&
    ptw_resp_s1_pf && !ptw_resp_s1_af && !ptw_resp_s2_gpf &&
    !ptw_resp_s2_gaf && ptw_resp_s2_perm_x && ptw_resp_s2_perm_a;
  wire atp_all_stage_s1_af_s2_leaf = atp_all_stage_fault_context &&
    ptw_resp_s1_af && !ptw_resp_s1_pf && !ptw_resp_s2_gpf &&
    !ptw_resp_s2_gaf && ptw_resp_s2_perm_x && ptw_resp_s2_perm_a;
  wire atp_all_stage_s1_pf_s2_gpf = atp_all_stage_fault_context &&
    ptw_resp_s1_pf && !ptw_resp_s1_af && ptw_resp_s2_gpf && !ptw_resp_s2_gaf;
  wire atp_all_stage_s1_pf_s2_gaf = atp_all_stage_fault_context &&
    ptw_resp_s1_pf && !ptw_resp_s1_af && ptw_resp_s2_gaf && !ptw_resp_s2_gpf;
  wire atp_all_stage_s1_af_s2_gpf = atp_all_stage_fault_context &&
    ptw_resp_s1_af && !ptw_resp_s1_pf && ptw_resp_s2_gpf && !ptw_resp_s2_gaf;
  wire atp_all_stage_s1_af_s2_gaf = atp_all_stage_fault_context &&
    ptw_resp_s1_af && !ptw_resp_s1_pf && ptw_resp_s2_gaf && !ptw_resp_s2_gpf;

  always_comb begin
    atp_translation_mode = 4'd0;
    if (atp_translation_observed) begin
      if (!io_tlbCsr_priv_virt) begin
        if (io_tlbCsr_satp_mode == 4'h0) atp_translation_mode = 4'd1;
        else if (io_tlbCsr_satp_mode == 4'h8) atp_translation_mode = 4'd2;
        else if (io_tlbCsr_satp_mode == 4'h9) atp_translation_mode = 4'd3;
      end else if (io_tlbCsr_vsatp_mode == 4'h0 && io_tlbCsr_hgatp_mode == 4'h0) begin
        atp_translation_mode = 4'd4;
      end else if (io_tlbCsr_vsatp_mode == 4'h8 && io_tlbCsr_hgatp_mode == 4'h0) begin
        atp_translation_mode = 4'd5;
      end else if (io_tlbCsr_vsatp_mode == 4'h9 && io_tlbCsr_hgatp_mode == 4'h0) begin
        atp_translation_mode = 4'd6;
      end else if (io_tlbCsr_vsatp_mode == 4'h0 && io_tlbCsr_hgatp_mode == 4'h8) begin
        atp_translation_mode = 4'd7;
      end else if (io_tlbCsr_vsatp_mode == 4'h0 && io_tlbCsr_hgatp_mode == 4'h9) begin
        atp_translation_mode = 4'd8;
      end else if (io_tlbCsr_vsatp_mode == 4'h8 && io_tlbCsr_hgatp_mode == 4'h8) begin
        atp_translation_mode = 4'd9;
      end else if (io_tlbCsr_vsatp_mode == 4'h9 && io_tlbCsr_hgatp_mode == 4'h8) begin
        atp_translation_mode = 4'd10;
      end else if (io_tlbCsr_vsatp_mode == 4'h8 && io_tlbCsr_hgatp_mode == 4'h9) begin
        atp_translation_mode = 4'd11;
      end else if (io_tlbCsr_vsatp_mode == 4'h9 && io_tlbCsr_hgatp_mode == 4'h9) begin
        atp_translation_mode = 4'd12;
      end
    end
    atp_sfence_attr_transition = atp_sfence_attr_cacheable_to_mmio ? 3'd1 :
      atp_sfence_attr_cacheable_to_nc ? 3'd2 : atp_sfence_attr_mmio_to_cacheable ? 3'd3 :
      atp_sfence_attr_mmio_to_nc ? 3'd4 : atp_sfence_attr_nc_to_cacheable ? 3'd5 :
      atp_sfence_attr_nc_to_mmio ? 3'd6 : 3'd0;
    atp_pmp_lock_mode = atp_locked_m_mode_allow ? 3'd1 :
      atp_locked_m_mode_deny ? 3'd2 : atp_locked_su_mode_allow ? 3'd3 :
      atp_locked_su_mode_deny ? 3'd4 : atp_unlocked_m_mode_bypass ? 3'd5 : 3'd0;
    atp_tor_boundary = atp_tor_lower_boundary ? 2'd1 : atp_tor_upper_boundary ? 2'd2 :
      atp_tor_inside_range ? 2'd3 : 2'd0;
    atp_napot_boundary = atp_napot_lower_boundary ? 2'd1 : atp_napot_upper_boundary ? 2'd2 :
      atp_napot_inside_range ? 2'd3 : 2'd0;
    atp_cross_pma_attribute = atp_cross_pma_diff_attr;
    atp_redirect_attr_transition = atp_redirect_cacheable_to_mmio ? 3'd1 :
      atp_redirect_cacheable_to_nc ? 3'd2 : atp_redirect_mmio_to_cacheable ? 3'd3 :
      atp_mmio_to_nc ? 3'd4 : atp_redirect_nc_to_cacheable ? 3'd5 :
      atp_nc_to_mmio ? 3'd6 : 3'd0;
  end

  covergroup frontend_atp_funcov_cg @(posedge clock);
    option.per_instance = 1;

    ATP_csr_changed_before_ptw_resp_cp: coverpoint (atp_timing_seen_csr_changed && ptw_resp_valid) iff (!reset) {
      bins csr_changed_before_ptw_resp = {1'b1};
    }
    ATP_translation_miss_cp: coverpoint atp_timing_miss_kind iff (!reset) {
      bins nonvirtual_s_stage = {3'd1};
      bins only_stage1 = {3'd2};
      bins only_stage2 = {3'd3};
      bins all_stage = {3'd4};
    }
    ATP_refill_then_hit_cp: coverpoint atp_timing_refill_hit_kind iff (!reset) {
      bins nonvirtual_s_stage = {3'd1};
      bins only_stage1 = {3'd2};
      bins only_stage2 = {3'd3};
      bins all_stage = {3'd4};
    }
    ATP_sector_lane_cp: coverpoint atp_timing_sector_lane_kind iff (!reset) {
      bins s1_valid = {3'd1};
      bins s1_invalid = {3'd2};
      bins vs_valid = {3'd3};
      bins vs_invalid = {3'd4};
    }
    ATP_superpage_multi_lane_hit_cp: coverpoint (atp_timing_superpage_new_lane_hit) iff (!reset) {
      bins multi_lane_hit = {1'b1};
    }
    ATP_flushpipe_with_ptw_req_cp: coverpoint (flush_pipe && atp_timing_ptw_req_fire) iff (!reset) {
      bins concurrent = {1'b1};
    }
    ATP_flushpipe_with_ptw_resp_cp: coverpoint (flush_pipe && ptw_resp_valid) iff (!reset) {
      bins concurrent = {1'b1};
    }
    ATP_flushpipe_during_ptw_wait_cp: coverpoint (atp_timing_seen_flush_pipe && ptw_resp_valid) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_sfence_with_ptw_req_cp: coverpoint (sfence_valid && atp_timing_ptw_req_fire) iff (!reset) {
      bins concurrent = {1'b1};
    }
    ATP_sfence_with_ptw_resp_cp: coverpoint (sfence_valid && ptw_resp_valid) iff (!reset) {
      bins concurrent = {1'b1};
    }
    ATP_sfence_during_ptw_wait_cp: coverpoint (atp_timing_seen_sfence && ptw_resp_valid) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_sfence_scope_cp: coverpoint atp_timing_sfence_scope iff (!reset) {
      bins all_addr_all_id = {3'd1};
      bins single_addr_all_id = {3'd2};
      bins all_addr_single_id = {3'd3};
      bins single_addr_single_id = {3'd4};
    }
    ATP_sfence_stage_cp: coverpoint atp_timing_sfence_stage iff (!reset) {
      bins nonvirtual_s_stage = {3'd1};
      bins only_stage1 = {3'd2};
      bins only_stage2 = {3'd3};
      bins all_stage_vs_side = {3'd4};
      bins all_stage_g_side = {3'd5};
    }

    ATP_translation_mode_cp: coverpoint atp_translation_mode iff (!reset) {
      bins nonvirtual_bare = {4'd1};
      bins nonvirtual_sv39 = {4'd2};
      bins nonvirtual_sv48 = {4'd3};
      bins virtual_bare = {4'd4};
      bins only_stage1_sv39 = {4'd5};
      bins only_stage1_sv48 = {4'd6};
      bins only_stage2_sv39x4 = {4'd7};
      bins only_stage2_sv48x4 = {4'd8};
      bins all_stage_sv39_sv39x4 = {4'd9};
      bins all_stage_sv48_sv39x4 = {4'd10};
      bins all_stage_sv39_sv48x4 = {4'd11};
      bins all_stage_sv48_sv48x4 = {4'd12};
    }
    ATP_s1_leaf_executable_cp: coverpoint atp_s1_leaf_executable iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_s1_leaf_not_executable_cp: coverpoint atp_s1_leaf_not_executable iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_s1_leaf_accessed_clear_cp: coverpoint atp_s1_leaf_accessed_clear iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_s1_privilege_denied_cp: coverpoint atp_s1_privilege_denied iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_s1_write_without_read_cp: coverpoint atp_s1_write_without_read iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_s2_leaf_executable_cp: coverpoint atp_s2_leaf_executable iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_s2_leaf_not_executable_cp: coverpoint atp_s2_leaf_not_executable iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_s2_leaf_accessed_clear_cp: coverpoint atp_s2_leaf_accessed_clear iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_only_stage2_gpf_cp: coverpoint atp_only_stage2_gpf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_only_stage2_gaf_cp: coverpoint atp_only_stage2_gaf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_fault_context_cp: coverpoint atp_all_stage_fault_context iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_vs_leaf_gpf_cp: coverpoint atp_all_stage_vs_leaf_gpf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_vs_leaf_gaf_cp: coverpoint atp_all_stage_vs_leaf_gaf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_vs_fake_gpf_cp: coverpoint atp_all_stage_vs_fake_gpf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_vs_fake_gaf_cp: coverpoint atp_all_stage_vs_fake_gaf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_vs_nonleaf_gpf_cp: coverpoint atp_all_stage_vs_nonleaf_gpf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_vs_nonleaf_gaf_cp: coverpoint atp_all_stage_vs_nonleaf_gaf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_s1_pf_s2_leaf_cp: coverpoint atp_all_stage_s1_pf_s2_leaf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_s1_af_s2_leaf_cp: coverpoint atp_all_stage_s1_af_s2_leaf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_s1_pf_s2_gpf_cp: coverpoint atp_all_stage_s1_pf_s2_gpf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_s1_pf_s2_gaf_cp: coverpoint atp_all_stage_s1_pf_s2_gaf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_s1_af_s2_gpf_cp: coverpoint atp_all_stage_s1_af_s2_gpf iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_s1_af_s2_gaf_cp: coverpoint atp_all_stage_s1_af_s2_gaf iff (!reset) {
      bins observed = {1'b1};
    }

    ATP_satp_asid_changed_cp: coverpoint (atp_satp_asid_changed_after_refill) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_vsatp_asid_changed_cp: coverpoint (atp_vsatp_asid_changed_after_refill) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_hgatp_vmid_changed_cp: coverpoint (atp_hgatp_vmid_changed_after_refill) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_priv_virt_changed_cp: coverpoint (atp_priv_virt_changed_after_refill) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_unmatched_sfence_wait_cp: coverpoint (atp_unmatched_sfence_during_ptw_wait) iff (!reset) {
      bins unmatched = {1'b1};
    }
    ATP_sfence_attribute_transition_cp: coverpoint atp_sfence_attr_transition iff (!reset) {
      bins cache_to_mmio = {3'd1};
      bins cache_to_nc = {3'd2};
      bins mmio_to_cache = {3'd3};
      bins mmio_to_nc = {3'd4};
      bins nc_to_cache = {3'd5};
      bins nc_to_mmio = {3'd6};
    }
    ATP_sfence_translation_changed_cp:
      coverpoint atp_sfence_translation_changed iff (!reset) {
        bins observed = {1'b1};
      }
    ATP_tlb_csr_translation_changed_cp:
      coverpoint atp_tlb_csr_translation_changed iff (!reset) {
        bins observed = {1'b1};
      }
    ATP_execute_permission_cp:
      coverpoint {
        atp_pmp_pma_execute_allow || atp_pmp_or_pma_execute_deny,
        atp_pmp_pma_execute_allow
      } iff (!reset) {
        bins allow = {2'b11};
        bins deny = {2'b10};
    }
    ATP_cacheable_pmp_only_deny_cp: coverpoint atp_cacheable_pmp_only_deny iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_cacheable_pma_only_deny_cp: coverpoint atp_cacheable_pma_only_deny iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_cacheable_pmp_pma_both_deny_cp: coverpoint atp_cacheable_both_deny iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_uncache_pmp_only_deny_cp: coverpoint atp_uncache_pmp_only_deny iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_uncache_pma_only_deny_cp: coverpoint atp_uncache_pma_only_deny iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_uncache_pmp_pma_both_deny_cp: coverpoint atp_uncache_both_deny iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_pmp_lock_mode_cp: coverpoint atp_pmp_lock_mode iff (!reset) {
      bins locked_m_allow = {3'd1};
      bins locked_m_deny = {3'd2};
      bins locked_su_allow = {3'd3};
      bins locked_su_deny = {3'd4};
      bins unlocked_m_bypass = {3'd5};
    }
    ATP_tor_boundary_cp: coverpoint atp_tor_boundary iff (!reset) {
      bins lower = {2'd1};
      bins upper = {2'd2};
      bins in_range = {2'd3};
    }
    ATP_napot_boundary_cp: coverpoint atp_napot_boundary iff (!reset) {
      bins lower = {2'd1};
      bins upper = {2'd2};
      bins in_range = {2'd3};
    }
    ATP_pmp_overlap_low_index_cp: coverpoint (atp_pmp_overlap_low_index) iff (!reset) {
      bins low_index_priority = {1'b1};
    }
    ATP_cross_pma_attribute_cp:
      coverpoint {
        atp_cross_pma_same_attr || atp_cross_pma_diff_attr,
        atp_cross_pma_attribute
      } iff (!reset) {
        bins same = {2'b10};
        bins different = {2'b11};
    }
    ATP_nonvirtual_bare_cache_cp: coverpoint (atp_nonvirtual_bare_pma_cacheable) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_nonvirtual_ptw_cache_cp: coverpoint (atp_nonvirtual_ptw_pma_cacheable) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_nonvirtual_bare_mmio_cp: coverpoint (atp_nonvirtual_bare_pma_mmio) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_nonvirtual_ptw_mmio_cp: coverpoint (atp_nonvirtual_ptw_pma_mmio) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_nonvirtual_nc_or_io_cp: coverpoint (atp_nonvirtual_pbmt_nc_or_io) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_virtual_cache_cp: coverpoint (atp_virtual_all_pbmt_pma_cacheable) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_virtual_mmio_cp: coverpoint (atp_virtual_all_pbmt_pma_mmio) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_virtual_single_stage_nc_or_io_cp: coverpoint (atp_virtual_single_stage_pbmt_nc_or_io) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_g_nc_or_io_cp: coverpoint (atp_all_stage_g_pbmt_nc_or_io) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_all_stage_vs_nc_or_io_cp: coverpoint (atp_all_stage_vs_pbmt_nc_or_io) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_redirect_attribute_transition_cp: coverpoint atp_redirect_attr_transition iff (!reset) {
      bins cache_to_mmio = {3'd1};
      bins cache_to_nc = {3'd2};
      bins mmio_to_cache = {3'd3};
      bins mmio_to_nc = {3'd4};
      bins nc_to_cache = {3'd5};
      bins nc_to_mmio = {3'd6};
    }
    ATP_pbmt_nc_at_pma_edge_cp: coverpoint (atp_pbmt_nc_at_pma_edge) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_pbmt_nc_after_pma_boundary_cp: coverpoint (atp_pbmt_nc_after_pma_boundary) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_pbmt_nc_cross_pma_regions_cp: coverpoint (atp_pbmt_nc_cross_pma_regions) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_reserved_pbmt_encoding_cp: coverpoint (atp_reserved_pbmt_encoding) iff (!reset) {
      bins reserved_encoding = {1'b1};
    }
  endgroup

  frontend_atp_funcov_cg atp_cov = new();
endmodule
