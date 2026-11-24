module frontend_atp_funcov #(
  parameter int BOUNDARY_PC_BITS = 51,
  parameter int BOUNDARY_PADDR_BITS = 48,
  parameter int BOUNDARY_FTQ_BITS = 6
) (
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
  input logic          pmp_checker_1_x,
  input logic boundary_flush,
  input logic boundary_context_changed,
  input logic boundary_request_fire,
  input logic [63:0] boundary_request_fullva,
  input logic [BOUNDARY_FTQ_BITS:0] boundary_request_ftq,
  input logic [2:0] boundary_request_backend_exception,
  input logic boundary_virt,
  input logic [1:0] boundary_imode,
  input logic [3:0] boundary_satp_mode,
  input logic [3:0] boundary_vsatp_mode,
  input logic [3:0] boundary_hgatp_mode,
  input logic boundary_translation_valid,
  input logic [BOUNDARY_PC_BITS-1:0] boundary_translation_pc,
  input logic [BOUNDARY_FTQ_BITS:0] boundary_translation_ftq,
  input logic boundary_s1_execute_allowed,
  input logic boundary_s2_execute_allowed,
  input logic boundary_pmp_execute_allowed,
  input logic boundary_ifu_s1_fire,
  input logic boundary_ifu_s1_flush,
  input logic [1:0] boundary_ifu_block_valid,
  input logic [1:0][BOUNDARY_PC_BITS-1:0] boundary_ifu_block_pc,
  input logic [1:0][BOUNDARY_FTQ_BITS:0] boundary_ifu_block_ftq,
  input logic [1:0][5:0] boundary_ifu_block_size,
  input logic [1:0][511:0] boundary_ifu_block_data,
  input logic boundary_half_valid,
  input logic [BOUNDARY_PC_BITS-1:0] boundary_half_pc,
  input logic [15:0] boundary_half_data,
  input logic boundary_previous_half_valid,
  input logic [BOUNDARY_PC_BITS-1:0] boundary_previous_half_pc,
  input logic [15:0] boundary_previous_half_data,
  input logic boundary_to_ibuffer_valid,
  input logic boundary_to_ibuffer_ready,
  input logic boundary_ifu_s2_flush,
  input logic [BOUNDARY_PC_BITS-1:0] boundary_ifu_s2_pc,
  input logic [BOUNDARY_FTQ_BITS:0] boundary_ifu_s2_ftq
);

  logic atp_nonvirtual_sv39_legal_boundary;
  logic atp_nonvirtual_sv48_legal_boundary;
  logic atp_virtual_sv39_legal_boundary;
  logic atp_virtual_sv48_legal_boundary;
  logic atp_nonvirtual_sv39_noncanonical;
  logic atp_nonvirtual_sv48_noncanonical;
  logic atp_virtual_sv39_noncanonical;
  logic atp_virtual_sv48_noncanonical;
  logic atp_sv39x4_legal_high_gpa;
  logic atp_sv48x4_legal_high_gpa;
  logic atp_sv39x4_overflow_gpa;
  logic atp_sv48x4_overflow_gpa;
  logic atp_bare_legal_boundary;
  logic atp_bare_address_overflow;
  logic atp_high_gpa_backpressure_recovery;
  logic atp_sv39_cross_boundary_rvi;
  logic atp_sv48_cross_boundary_rvi;
  logic atp_sv39x4_cross_boundary_rvi;
  logic atp_sv48x4_cross_boundary_rvi;

  // Request-owned Condition sampling for canonical/GPA boundaries. Checkpoint
  // correctness stays in checkers; response fault and GPA values do not gate hits.
  generate
    begin : address_boundary
      localparam logic [63:0] SV39_EDGE = 64'h0000_0040_0000_0000;
      localparam logic [63:0] SV48_EDGE = 64'h0000_8000_0000_0000;
      localparam logic [63:0] GPA39_EDGE = 64'h0000_0200_0000_0000;
      localparam logic [63:0] GPA48_EDGE = 64'h0004_0000_0000_0000;
      localparam logic [63:0] PA_EDGE = 64'b1 << BOUNDARY_PADDR_BITS;
      localparam int TAGS = 1 << (BOUNDARY_FTQ_BITS + 1);

      typedef enum logic [2:0] {
        NONE, NV39, NV48, VS39, VS48, G39, G48, BARE
      } address_kind;
      typedef struct packed {
        logic valid;
        logic [63:0] fullva;
        address_kind kind;
        logic stage1;
        logic stage2;
        logic [14:0] context_key;
        logic permissions;
      } request_record;
      request_record requests [TAGS];

      wire supervisor_or_user = boundary_imode == 2'd0 || boundary_imode == 2'd1;
      wire guest_mode_valid = boundary_hgatp_mode == 4'd0 || boundary_hgatp_mode == 4'd8 || boundary_hgatp_mode == 4'd9;
      // fullva is sign extended from GuardedPc even for unsigned G-stage addresses.
      wire [63:0] request_pc = {{(64-BOUNDARY_PC_BITS){1'b0}}, boundary_request_fullva[BOUNDARY_PC_BITS-1:0]};
      wire observe_request = boundary_request_fire && boundary_request_backend_exception == 3'd0;
      wire nonvirtual_sv39 = !boundary_virt && supervisor_or_user && boundary_satp_mode == 4'd8;
      wire nonvirtual_sv48 = !boundary_virt && supervisor_or_user && boundary_satp_mode == 4'd9;
      wire virtual_sv39 = boundary_virt && supervisor_or_user && boundary_vsatp_mode == 4'd8 && guest_mode_valid;
      wire virtual_sv48 = boundary_virt && supervisor_or_user && boundary_vsatp_mode == 4'd9 && guest_mode_valid;
      wire only_stage2 = boundary_virt && supervisor_or_user && boundary_vsatp_mode == 4'd0;
      wire bare = !boundary_virt && boundary_satp_mode == 4'd0;
      wire legal39_boundary = boundary_request_fullva == SV39_EDGE - 2 || boundary_request_fullva == -SV39_EDGE;
      wire legal48_boundary = boundary_request_fullva == SV48_EDGE - 2 || boundary_request_fullva == -SV48_EDGE;
      wire illegal39_boundary = boundary_request_fullva == SV39_EDGE || boundary_request_fullva == -SV39_EDGE - 2;
      wire illegal48_boundary = boundary_request_fullva == SV48_EDGE || boundary_request_fullva == -SV48_EDGE - 2;
      wire [14:0] context_key = {boundary_virt, boundary_imode, boundary_satp_mode, boundary_vsatp_mode, boundary_hgatp_mode};
      address_kind request_kind;

      always_comb begin
        request_kind = NONE;
        if (nonvirtual_sv39 && legal39_boundary) request_kind = NV39;
        if (nonvirtual_sv48 && legal48_boundary) request_kind = NV48;
        if (virtual_sv39 && legal39_boundary) request_kind = VS39;
        if (virtual_sv48 && legal48_boundary) request_kind = VS48;
        if (only_stage2 && boundary_hgatp_mode == 4'd8 && request_pc >= GPA39_EDGE / 2 &&
            request_pc < GPA39_EDGE) request_kind = G39;
        if (only_stage2 && boundary_hgatp_mode == 4'd9 && request_pc >= GPA48_EDGE / 2 &&
            request_pc < GPA48_EDGE) request_kind = G48;
        if (bare && request_pc == PA_EDGE - 2) request_kind = BARE;
      end

      function automatic logic same_request(input request_record entry,
          input logic [BOUNDARY_PC_BITS-1:0] pc);
        return entry.valid && entry.fullva[BOUNDARY_PC_BITS-1:0] == pc;
      endfunction

      function automatic logic [63:0] rvi_limit(input address_kind kind);
        case (kind)
          NV39: return SV39_EDGE;
          NV48: return SV48_EDGE;
          G39: return GPA39_EDGE;
          G48: return GPA48_EDGE;
          default: return 0;
        endcase
      endfunction

      function automatic logic range_inside(input request_record entry,
          input logic [5:0] size, input logic [511:0] data);
        logic [63:0] start_pc, last_pc, limit;
        logic [15:0] first_halfword;
        start_pc = entry.fullva;
        if (entry.kind inside {G39, G48, BARE})
          start_pc = {{(64-BOUNDARY_PC_BITS){1'b0}}, entry.fullva[BOUNDARY_PC_BITS-1:0]};
        last_pc = start_pc + (64'(size) << 1) - 1;
        first_halfword = data[16*int'(start_pc[5:1]) +: 16];
        case (entry.kind)
          NV39, VS39: limit = SV39_EDGE;
          NV48, VS48: limit = SV48_EDGE;
          G39: limit = GPA39_EDGE;
          G48: limit = GPA48_EDGE;
          BARE: limit = PA_EDGE;
          default: return 1'b0;
        endcase
        if (size == 0 || size > 32) return 1'b0;
        // At the last halfword, C.NOP is an independently known legal RVC witness.
        if (start_pc == limit - 2) return size == 1 && first_halfword == 16'h0001;
        if (entry.kind inside {NV39, VS39})
          return last_pc[63:39] == {25{last_pc[38]}};
        if (entry.kind inside {NV48, VS48})
          return last_pc[63:48] == {16{last_pc[47]}};
        return last_pc < limit;
      endfunction

      logic saved_half_valid;
      address_kind saved_half_kind;
      logic [BOUNDARY_PC_BITS-1:0] saved_half_pc;
      logic [15:0] saved_half_data;
      logic [14:0] saved_half_context;
      logic [63:0] saved_half_next_pc;
      logic next_saved_half_valid;
      address_kind next_saved_half_kind;
      logic [63:0] next_saved_half_next_pc;
      logic [14:0] next_saved_half_context;

      typedef enum logic [1:0] {IDLE, HOLDING, WAIT_FLUSH, WAIT_RECOVERY} hold_state;
      hold_state state;
      logic [1:0] stall_cycles;
      logic [BOUNDARY_FTQ_BITS:0] held_ftq;
      logic [BOUNDARY_PC_BITS-1:0] held_pc;
      wire held_transaction = boundary_to_ibuffer_valid && boundary_ifu_s2_ftq == held_ftq && boundary_ifu_s2_pc == held_pc;
      wire high_gpa_s2 = boundary_to_ibuffer_valid &&
        same_request(requests[boundary_ifu_s2_ftq], boundary_ifu_s2_pc) &&
        requests[boundary_ifu_s2_ftq].stage2 && !requests[boundary_ifu_s2_ftq].stage1 &&
        requests[boundary_ifu_s2_ftq].context_key[13:12] inside {2'd0, 2'd1} &&
        requests[boundary_ifu_s2_ftq].context_key[3:0] == 4'd9 &&
        requests[boundary_ifu_s2_ftq].fullva[BOUNDARY_PC_BITS-1:0] == BOUNDARY_PC_BITS'(GPA48_EDGE + 64'h20);

      always_comb begin
        atp_nonvirtual_sv39_legal_boundary = 1'b0;
        atp_nonvirtual_sv48_legal_boundary = 1'b0;
        atp_virtual_sv39_legal_boundary = 1'b0;
        atp_virtual_sv48_legal_boundary = 1'b0;
        atp_nonvirtual_sv39_noncanonical = 1'b0;
        atp_nonvirtual_sv48_noncanonical = 1'b0;
        atp_virtual_sv39_noncanonical = 1'b0;
        atp_virtual_sv48_noncanonical = 1'b0;
        atp_sv39x4_legal_high_gpa = 1'b0;
        atp_sv48x4_legal_high_gpa = 1'b0;
        atp_sv39x4_overflow_gpa = 1'b0;
        atp_sv48x4_overflow_gpa = 1'b0;
        atp_bare_legal_boundary = 1'b0;
        atp_bare_address_overflow = 1'b0;
        atp_high_gpa_backpressure_recovery = 1'b0;
        atp_sv39_cross_boundary_rvi = 1'b0;
        atp_sv48_cross_boundary_rvi = 1'b0;
        atp_sv39x4_cross_boundary_rvi = 1'b0;
        atp_sv48x4_cross_boundary_rvi = 1'b0;
        next_saved_half_valid = 1'b0;
        next_saved_half_kind = NONE;
        next_saved_half_next_pc = '0;
        next_saved_half_context = '0;
        if (!reset && !boundary_flush && !boundary_context_changed) begin
          if (observe_request) begin
            atp_nonvirtual_sv39_noncanonical = nonvirtual_sv39 && illegal39_boundary;
            atp_nonvirtual_sv48_noncanonical = nonvirtual_sv48 && illegal48_boundary;
            atp_virtual_sv39_noncanonical = virtual_sv39 && illegal39_boundary;
            atp_virtual_sv48_noncanonical = virtual_sv48 && illegal48_boundary;
            atp_sv39x4_overflow_gpa = only_stage2 && boundary_hgatp_mode == 4'd8 &&
              (request_pc == GPA39_EDGE || request_pc == GPA39_EDGE + 64'h20);
            atp_sv48x4_overflow_gpa = only_stage2 && boundary_hgatp_mode == 4'd9 &&
              (request_pc == GPA48_EDGE || request_pc == GPA48_EDGE + 64'h20);
            atp_bare_address_overflow = bare && BOUNDARY_PADDR_BITS < BOUNDARY_PC_BITS &&
              (request_pc == PA_EDGE || request_pc == PA_EDGE + 64'h20);
            atp_high_gpa_backpressure_recovery = state == WAIT_RECOVERY && only_stage2 && boundary_hgatp_mode == 4'd9 &&
              request_pc < GPA48_EDGE && request_pc[11:0] != 12'h020 &&
              request_pc[BOUNDARY_PC_BITS-1:12] != held_pc[BOUNDARY_PC_BITS-1:12];
          end
          if (boundary_ifu_s1_fire && !boundary_ifu_s1_flush) begin
            for (int b = 0; b < 2; b++) begin
              if (boundary_ifu_block_valid[b] &&
                  same_request(requests[boundary_ifu_block_ftq[b]], boundary_ifu_block_pc[b])) begin
                if (requests[boundary_ifu_block_ftq[b]].permissions &&
                    range_inside(requests[boundary_ifu_block_ftq[b]], boundary_ifu_block_size[b], boundary_ifu_block_data[b])) begin
                  case (requests[boundary_ifu_block_ftq[b]].kind)
                    NV39: atp_nonvirtual_sv39_legal_boundary = 1'b1;
                    NV48: atp_nonvirtual_sv48_legal_boundary = 1'b1;
                    VS39: atp_virtual_sv39_legal_boundary = 1'b1;
                    VS48: atp_virtual_sv48_legal_boundary = 1'b1;
                    G39: atp_sv39x4_legal_high_gpa = 1'b1;
                    G48: atp_sv48x4_legal_high_gpa = 1'b1;
                    BARE: atp_bare_legal_boundary = 1'b1;
                    default: ;
                  endcase
                end
                if (saved_half_valid && boundary_previous_half_valid && b == 0 &&
                    boundary_previous_half_pc == saved_half_pc && boundary_previous_half_data == saved_half_data &&
                    requests[boundary_ifu_block_ftq[b]].context_key == saved_half_context &&
                    requests[boundary_ifu_block_ftq[b]].fullva[BOUNDARY_PC_BITS-1:0] == BOUNDARY_PC_BITS'(saved_half_next_pc)) begin
                  case (saved_half_kind)
                    NV39: atp_sv39_cross_boundary_rvi = 1'b1;
                    NV48: atp_sv48_cross_boundary_rvi = 1'b1;
                    G39: atp_sv39x4_cross_boundary_rvi = 1'b1;
                    G48: atp_sv48x4_cross_boundary_rvi = 1'b1;
                    default: ;
                  endcase
                end
                if (boundary_half_valid && requests[boundary_ifu_block_ftq[b]].permissions &&
                    boundary_half_data[1:0] == 2'b11 && boundary_half_data[4:2] != 3'b111) begin
                  if (rvi_limit(requests[boundary_ifu_block_ftq[b]].kind) != 0 &&
                      boundary_half_pc == BOUNDARY_PC_BITS'(rvi_limit(requests[boundary_ifu_block_ftq[b]].kind) - 2) &&
                      boundary_ifu_block_pc[b] == boundary_half_pc) begin
                    next_saved_half_valid = 1'b1;
                    next_saved_half_kind = requests[boundary_ifu_block_ftq[b]].kind;
                    next_saved_half_next_pc = rvi_limit(requests[boundary_ifu_block_ftq[b]].kind);
                    next_saved_half_context = requests[boundary_ifu_block_ftq[b]].context_key;
                  end
                end
              end
            end
          end
        end
      end

      always_ff @(posedge clock) begin
        if (reset || boundary_flush || boundary_context_changed) begin
          for (int i = 0; i < TAGS; i++) requests[i] <= '0;
          saved_half_valid <= 1'b0;
        end else begin
          if (boundary_translation_valid && same_request(requests[boundary_translation_ftq], boundary_translation_pc))
            requests[boundary_translation_ftq].permissions <= boundary_pmp_execute_allowed &&
              (!requests[boundary_translation_ftq].stage1 || boundary_s1_execute_allowed) &&
              (!requests[boundary_translation_ftq].stage2 || boundary_s2_execute_allowed);
          if (boundary_request_fire) begin
            requests[boundary_request_ftq].valid <= boundary_request_backend_exception == 0;
            requests[boundary_request_ftq].fullva <= boundary_request_fullva;
            requests[boundary_request_ftq].kind <= request_kind;
            requests[boundary_request_ftq].stage1 <= boundary_virt ? boundary_vsatp_mode != 0 : boundary_satp_mode != 0;
            requests[boundary_request_ftq].stage2 <= boundary_virt && boundary_hgatp_mode != 0;
            requests[boundary_request_ftq].context_key <= context_key;
            requests[boundary_request_ftq].permissions <= 1'b0;
          end
          if (boundary_ifu_s1_flush) saved_half_valid <= 1'b0;
          else if (boundary_ifu_s1_fire) begin
            saved_half_valid <= next_saved_half_valid;
            saved_half_kind <= next_saved_half_kind;
            saved_half_pc <= boundary_half_pc;
            saved_half_data <= boundary_half_data;
            saved_half_context <= next_saved_half_context;
            saved_half_next_pc <= next_saved_half_next_pc;
          end
        end

        if (reset || boundary_context_changed) begin
          state <= IDLE;
          stall_cycles <= 0;
          held_ftq <= '0;
          held_pc <= '0;
        end else if (boundary_flush) begin
          state <= state == WAIT_FLUSH ? WAIT_RECOVERY : IDLE;
          stall_cycles <= 0;
        end else if (boundary_ifu_s2_flush) begin
          state <= IDLE;
          stall_cycles <= 0;
        end else begin
          case (state)
            IDLE: if (high_gpa_s2 && !boundary_to_ibuffer_ready) begin
              state <= HOLDING;
              held_ftq <= boundary_ifu_s2_ftq;
              held_pc <= boundary_ifu_s2_pc;
              stall_cycles <= 1;
            end
            HOLDING: begin
              if (!held_transaction) begin
                state <= IDLE;
                stall_cycles <= 0;
              end else if (!boundary_to_ibuffer_ready) begin
                if (stall_cycles < 2) stall_cycles <= stall_cycles + 1'b1;
              end else begin
                state <= stall_cycles == 2 ? WAIT_FLUSH : IDLE;
              end
            end
            WAIT_RECOVERY: if (atp_high_gpa_backpressure_recovery) state <= IDLE;
            default: ;
          endcase
        end
      end
    end
  endgenerate

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
    ATP_nonvirtual_sv39_legal_boundary_cp: coverpoint (atp_nonvirtual_sv39_legal_boundary) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_nonvirtual_sv48_legal_boundary_cp: coverpoint (atp_nonvirtual_sv48_legal_boundary) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_virtual_sv39_legal_boundary_cp: coverpoint (atp_virtual_sv39_legal_boundary) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_virtual_sv48_legal_boundary_cp: coverpoint (atp_virtual_sv48_legal_boundary) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_nonvirtual_sv39_noncanonical_cp: coverpoint (atp_nonvirtual_sv39_noncanonical) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_nonvirtual_sv48_noncanonical_cp: coverpoint (atp_nonvirtual_sv48_noncanonical) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_virtual_sv39_noncanonical_cp: coverpoint (atp_virtual_sv39_noncanonical) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_virtual_sv48_noncanonical_cp: coverpoint (atp_virtual_sv48_noncanonical) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_sv39x4_legal_high_gpa_cp: coverpoint (atp_sv39x4_legal_high_gpa) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_sv48x4_legal_high_gpa_cp: coverpoint (atp_sv48x4_legal_high_gpa) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_sv39x4_overflow_gpa_cp: coverpoint (atp_sv39x4_overflow_gpa) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_sv48x4_overflow_gpa_cp: coverpoint (atp_sv48x4_overflow_gpa) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_bare_legal_boundary_cp: coverpoint (atp_bare_legal_boundary) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_bare_address_overflow_cp: coverpoint (atp_bare_address_overflow) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_high_gpa_backpressure_recovery_cp: coverpoint (atp_high_gpa_backpressure_recovery) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_sv39_cross_boundary_rvi_cp: coverpoint (atp_sv39_cross_boundary_rvi) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_sv48_cross_boundary_rvi_cp: coverpoint (atp_sv48_cross_boundary_rvi) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_sv39x4_cross_boundary_rvi_cp: coverpoint (atp_sv39x4_cross_boundary_rvi) iff (!reset) {
      bins observed = {1'b1};
    }
    ATP_sv48x4_cross_boundary_rvi_cp: coverpoint (atp_sv48x4_cross_boundary_rvi) iff (!reset) {
      bins observed = {1'b1};
    }
  endgroup

  frontend_atp_funcov_cg atp_cov = new();
endmodule
