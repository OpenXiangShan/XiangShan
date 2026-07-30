module frontend_mmio_fetch_funcov (
  input logic        clock,
  input logic        reset,
  input logic        s2_valid,
  input logic        s2_req_is_uncache,
  input logic        s2_use_uncache,
  input logic        s2_double_line,
  input logic        s2_pmp_mmio_0,
  input logic [1:0]  s2_pbmt_0,
  input logic [2:0]  s2_exception_0,
  input logic        is_first_instr,
  input logic        uncache_input_valid,
  input logic        uncache_input_ready,
  input logic [1:0]  uncache_state,
  input logic        uncache_busy,
  input logic        ifu_stall,
  input logic        empty_after,
  input logic        backend_commit,
  input logic        backend_empty,
  input logic        ibuffer_empty,
  input logic        backend_can_accept,
  input logic        tl_a_valid,
  input logic        tl_a_ready,
  input logic [47:0] tl_a_addr,
  input logic        tl_d_valid,
  input logic [255:0] tl_d_data,
  input logic        tl_d_corrupt,
  input logic        tl_d_denied,
  input logic [1:0]  entry_state,
  input logic        entry_resending,
  input logic [46:0] entry_req_addr,
  input logic        uncache_resp_valid,
  input logic [31:0] uncache_resp_data,
  input logic [2:0]  uncache_resp_exception,
  input logic        uncache_resp_need_resend,
  input logic        to_ibuffer_valid,
  input logic        to_ibuffer_ready,
  input logic [35:0] to_ibuffer_enq,
  input logic [35:0] to_ibuffer_is_rvc,
  input logic [48:0] to_ibuffer_pc_0,
  input logic [2:0]  to_ibuffer_exception,
  input logic        to_ibuffer_exception_cross_page,
  input logic        backend_redirect,
  input logic        ifu_flush,
  input logic        uncache_redirect,
  input logic        wb_redirect,
  input logic        wb_path_valid,
  input logic [1:0]  uncache_branch_type,
  input logic        prev_end_half_rvi,
  input logic [15:0] prev_half_data,
  input logic [48:0] prev_half_pc,
  input logic [48:0] uncache_pc,
  input logic        cfvec_valid,
  input logic        wfi_safe
);

  localparam logic [1:0] IDLE             = 2'h0;
  localparam logic [1:0] WAIT_LAST_COMMIT = 2'h1;
  localparam logic [1:0] SEND_REQ         = 2'h2;
  localparam logic [1:0] WAIT_RESP        = 2'h3;
  localparam logic [1:0] PBMT_NC          = 2'h2;

  logic        last_mmio_delivery_seen;
  logic        last_mmio_is_rvc;
  logic [48:0] last_mmio_pc;
  logic        prev_backend_can_accept;
  logic [1:0]  prev_uncache_state;
  logic        stalled_a_seen;
  logic [47:0] stalled_a_addr;
  logic        cross_page_half_pending;
  logic        first_page_exception_seen;
  logic        cross_8b_resend_seen;
  logic        prev_tl_a_fire;
  logic        wait_a_flush_seen;
  logic        resend_flush_seen;
  logic        half_flush_seen;

  wire mmio_candidate = s2_valid && s2_pmp_mmio_0;
  wire nc_candidate = s2_valid && !s2_pmp_mmio_0 && s2_pbmt_0 == PBMT_NC;
  wire mmio_delivery = to_ibuffer_valid && to_ibuffer_ready &&
    s2_req_is_uncache && s2_pmp_mmio_0;
  wire single_delivery = $onehot(to_ibuffer_enq);
  wire delivered_is_rvc = |(to_ibuffer_enq & to_ibuffer_is_rvc);
  wire tl_a_fire = tl_a_valid && tl_a_ready;
  wire entry_wait_resp = entry_state == WAIT_RESP;
  wire page_tail_request = &entry_req_addr[11:1];
  wire beat_tail_request = &entry_req_addr[2:1];
  wire [15:0] tl_d_first_half = tl_d_data[entry_req_addr[1:0] * 16 +: 16];
  wire response_is_rvc = tl_d_first_half[1:0] != 2'b11;
  wire response_is_rvi = tl_d_first_half[1:0] == 2'b11;
  wire mmio_pending = mmio_candidate || uncache_busy || uncache_state != IDLE;
  wire [2:0] d_response_kind = (!entry_wait_resp || !tl_d_valid) ? 3'h0 :
    tl_d_denied ? 3'h3 : tl_d_corrupt ? 3'h2 :
    3'h1;
  wire [2:0] cfi_delivery_kind = mmio_delivery ?
    {1'b1, uncache_branch_type} : 3'h0;
  wire [1:0] tl_error_kind = !mmio_delivery ? 2'h0 :
    uncache_resp_exception == 3'h3 ? 2'h1 :
    uncache_resp_exception == 3'h5 ? 2'h2 : 2'h0;

  always_ff @(posedge clock) begin
    if (reset) begin
      last_mmio_delivery_seen <= 1'b0;
      last_mmio_is_rvc <= 1'b0;
      last_mmio_pc <= '0;
      prev_backend_can_accept <= 1'b0;
      prev_uncache_state <= IDLE;
      stalled_a_seen <= 1'b0;
      stalled_a_addr <= '0;
      cross_page_half_pending <= 1'b0;
      first_page_exception_seen <= 1'b0;
      cross_8b_resend_seen <= 1'b0;
      prev_tl_a_fire <= 1'b0;
      wait_a_flush_seen <= 1'b0;
      resend_flush_seen <= 1'b0;
      half_flush_seen <= 1'b0;
    end else begin
      if (ifu_flush || backend_redirect) begin
        last_mmio_delivery_seen <= 1'b0;
      end else if (mmio_delivery) begin
        last_mmio_delivery_seen <= 1'b1;
        last_mmio_is_rvc <= delivered_is_rvc;
        last_mmio_pc <= to_ibuffer_pc_0;
      end
      prev_backend_can_accept <= backend_can_accept;
      prev_uncache_state <= uncache_state;
      prev_tl_a_fire <= tl_a_fire;
      wait_a_flush_seen <= entry_state == SEND_REQ && tl_a_valid && !tl_a_ready && ifu_flush;
      resend_flush_seen <= entry_resending && ifu_flush;
      half_flush_seen <= prev_end_half_rvi && ifu_flush;

      if (tl_a_valid && !tl_a_ready) begin
        stalled_a_seen <= 1'b1;
        stalled_a_addr <= tl_a_addr;
      end else if (tl_a_fire || ifu_flush) begin
        stalled_a_seen <= 1'b0;
      end

      if (page_tail_request && uncache_resp_valid && uncache_resp_need_resend) begin
        cross_page_half_pending <= 1'b1;
      end else if ((cross_page_half_pending && mmio_delivery) || ifu_flush) begin
        cross_page_half_pending <= 1'b0;
      end

      if (page_tail_request && tl_d_valid && (tl_d_corrupt || tl_d_denied))
        first_page_exception_seen <= 1'b1;
      else if ((cross_page_half_pending && to_ibuffer_valid) || ifu_flush)
        first_page_exception_seen <= 1'b0;

      if (beat_tail_request && entry_wait_resp && !entry_resending && tl_d_valid &&
          response_is_rvi && !tl_d_corrupt && !tl_d_denied)
        cross_8b_resend_seen <= 1'b1;
      else if ((cross_8b_resend_seen && uncache_resp_valid) || ifu_flush)
        cross_8b_resend_seen <= 1'b0;
    end
  end

  covergroup frontend_mmio_fetch_cg @(posedge clock);
    option.per_instance = 1;

    MMIO_first_fetch_cp:
      coverpoint (mmio_candidate && is_first_instr && backend_can_accept) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_nonfirst_without_commit_cp:
      coverpoint (mmio_candidate && !is_first_instr && !backend_commit) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_nonfirst_with_commit_cp:
      coverpoint (mmio_candidate && !is_first_instr && backend_commit) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_backend_empty_ibuffer_nonempty_wait_cp:
      coverpoint (uncache_state == WAIT_LAST_COMMIT && backend_empty && !ibuffer_empty) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_ibuffer_empty_backend_nonempty_wait_cp:
      coverpoint (uncache_state == WAIT_LAST_COMMIT && !backend_empty && ibuffer_empty) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_backend_ibuffer_empty_release_cp:
      coverpoint (uncache_state == WAIT_LAST_COMMIT && backend_empty && ibuffer_empty) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_empty_release_with_ibuffer_stall_cp:
      coverpoint (empty_after && ifu_stall && uncache_state == SEND_REQ) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_consecutive_rvc_cp:
      coverpoint (last_mmio_delivery_seen && last_mmio_is_rvc && mmio_delivery && delivered_is_rvc) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_consecutive_rvi_cp:
      coverpoint (last_mmio_delivery_seen && !last_mmio_is_rvc && mmio_delivery && !delivered_is_rvc) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvc_then_rvi_cp:
      coverpoint (last_mmio_delivery_seen && last_mmio_is_rvc && mmio_delivery && !delivered_is_rvc) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvi_then_rvc_cp:
      coverpoint (last_mmio_delivery_seen && !last_mmio_is_rvc && mmio_delivery && delivered_is_rvc) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvc_pc_2b_progress_cp:
      coverpoint (last_mmio_delivery_seen && last_mmio_is_rvc && mmio_delivery &&
                  to_ibuffer_pc_0 == last_mmio_pc + 49'd2) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvi_pc_4b_progress_cp:
      coverpoint (last_mmio_delivery_seen && !last_mmio_is_rvc && mmio_delivery &&
                  to_ibuffer_pc_0 == last_mmio_pc + 49'd4) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvc_single_pc_2b_progress_cp:
      coverpoint (last_mmio_delivery_seen && last_mmio_is_rvc && mmio_delivery &&
                  delivered_is_rvc && single_delivery &&
                  to_ibuffer_pc_0 == last_mmio_pc + 49'd2) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvi_single_pc_4b_progress_cp:
      coverpoint (last_mmio_delivery_seen && !last_mmio_is_rvc && mmio_delivery &&
                  !delivered_is_rvc && single_delivery &&
                  to_ibuffer_pc_0 == last_mmio_pc + 49'd4) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_branch_delivery_cp:
      coverpoint (mmio_delivery && is_first_instr && backend_can_accept &&
                  uncache_branch_type == 2'h1) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_jump_delivery_cp:
      coverpoint (mmio_delivery && is_first_instr && backend_can_accept &&
                  uncache_branch_type inside {2'h2, 2'h3}) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_can_accept_stays_low_cp:
      coverpoint (mmio_pending && !prev_backend_can_accept && !backend_can_accept && !cfvec_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_can_accept_rises_cp:
      coverpoint (mmio_pending && !prev_backend_can_accept && backend_can_accept && cfvec_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_can_accept_stays_high_single_delivery_cp:
      coverpoint (mmio_delivery && prev_backend_can_accept && backend_can_accept && single_delivery) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_can_accept_falls_cp:
      coverpoint (mmio_pending && prev_backend_can_accept && !backend_can_accept && !cfvec_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_tl_a_stall_cp:
      coverpoint (tl_a_valid && !tl_a_ready) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_redirect_while_waiting_d_cp:
      coverpoint (uncache_state == WAIT_RESP && backend_redirect) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_redirect_in_wait_last_commit_cp:
      coverpoint (uncache_state == WAIT_LAST_COMMIT && backend_redirect) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_redirect_in_send_req_cp:
      coverpoint (uncache_state == SEND_REQ && tl_a_valid && !tl_a_ready && backend_redirect) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvi_cross_8b_cp:
      coverpoint (beat_tail_request && entry_wait_resp && !entry_resending &&
                  tl_d_valid && response_is_rvi &&
                  !tl_d_corrupt && !tl_d_denied) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvc_at_8b_tail_cp:
      coverpoint (beat_tail_request && entry_wait_resp && !entry_resending &&
                  tl_d_valid && response_is_rvc &&
                  !tl_d_corrupt && !tl_d_denied) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvi_not_cross_8b_cp:
      coverpoint entry_req_addr[2:1] iff (!reset && entry_wait_resp && !entry_resending &&
                  tl_d_valid && response_is_rvi && !tl_d_corrupt && !tl_d_denied) {
        bins offset_0 = {2'b00};
        bins offset_2 = {2'b01};
        bins offset_4 = {2'b10};
      }
    MMIO_cross_8b_first_d_error_cp:
      coverpoint {tl_d_corrupt, tl_d_denied} iff (!reset && beat_tail_request &&
                  entry_wait_resp && !entry_resending && tl_d_valid) {
        bins corrupt = {2'b10};
        bins corrupt_and_denied = {2'b11};
      }
    MMIO_rvi_cross_page_cp:
      coverpoint (page_tail_request && uncache_resp_valid &&
                  uncache_resp_need_resend) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvc_at_page_tail_cp:
      coverpoint (page_tail_request && uncache_resp_valid &&
                  uncache_resp_data[1:0] != 2'b11 && !uncache_resp_need_resend) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_s2_accept_without_flush_cp:
      coverpoint (mmio_candidate && !ifu_flush && !backend_redirect) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_pmp_mmio_non_nc_cp:
      coverpoint (s2_valid && s2_pmp_mmio_0 && s2_pbmt_0 != PBMT_NC) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_pmp_mmio_with_nc_cp:
      coverpoint (s2_valid && s2_pmp_mmio_0 && s2_pbmt_0 == PBMT_NC) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_nc_without_pmp_mmio_cp:
      coverpoint nc_candidate iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_attribute_with_fetch_exception_cp:
      coverpoint (mmio_candidate && s2_exception_0 != 3'h0) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_first_page_pmp_exception_cp:
      coverpoint (s2_valid && s2_req_is_uncache && s2_pmp_mmio_0 &&
                  s2_exception_0 == 3'h3 && !s2_use_uncache) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_excludes_multi_instruction_cache_delivery_cp:
      coverpoint (mmio_delivery && single_delivery) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_uncache_response_single_instruction_cp:
      coverpoint (uncache_resp_valid && mmio_delivery && single_delivery) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_and_nc_share_uncache_with_different_gate_cp:
      coverpoint {mmio_candidate && s2_use_uncache && uncache_input_valid,
                  nc_candidate && s2_use_uncache && uncache_input_valid} iff (!reset) {
        bins mmio = {2'b10};
        bins nc = {2'b01};
      }
    MMIO_uncache_idle_cp:
      coverpoint (uncache_state == IDLE && !uncache_input_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_idle_accept_to_wait_last_commit_cp:
      coverpoint (prev_uncache_state == IDLE && uncache_state == WAIT_LAST_COMMIT &&
                  s2_pmp_mmio_0) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_idle_accept_nc_without_wait_cp:
      coverpoint (prev_uncache_state == IDLE && uncache_state == SEND_REQ &&
                  s2_pbmt_0 == PBMT_NC && !s2_pmp_mmio_0) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_first_instruction_direct_send_cp:
      coverpoint (uncache_input_valid && is_first_instr && uncache_state == SEND_REQ) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_nonfirst_waits_for_both_empty_cp:
      coverpoint (uncache_state == WAIT_LAST_COMMIT && !is_first_instr && !empty_after) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_wait_last_commit_no_tl_request_cp:
      coverpoint (uncache_state == WAIT_LAST_COMMIT && !tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_wait_last_commit_flush_cp:
      coverpoint (uncache_state == WAIT_LAST_COMMIT && ifu_flush) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_wait_a_flush_clears_request_cp:
      coverpoint (wait_a_flush_seen && entry_state == IDLE && !tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_resend_flush_clears_state_cp:
      coverpoint (resend_flush_seen && !entry_resending && !tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_half_flush_clears_state_cp:
      coverpoint (half_flush_seen && !prev_end_half_rvi && !entry_resending) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_send_req_with_ibuffer_ready_cp:
      coverpoint (uncache_state == SEND_REQ && !ifu_stall && tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_send_req_with_ibuffer_stall_cp:
      coverpoint (uncache_state == SEND_REQ && ifu_stall && !tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_tl_a_stall_context_cp:
      coverpoint (stalled_a_seen && tl_a_valid && !tl_a_ready &&
                  tl_a_addr == stalled_a_addr) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_tl_a_fire_to_wait_resp_cp:
      coverpoint (prev_tl_a_fire && entry_wait_resp && !tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_wait_resp_without_d_cp:
      coverpoint (entry_wait_resp && !tl_d_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_d_response_sample_cp:
      coverpoint d_response_kind iff (!reset) {
        bins clean = {3'h1};
        bins corrupt = {3'h2};
        bins denied = {3'h3};
      }
    MMIO_d_response_with_flush_cp:
      coverpoint (entry_wait_resp && tl_d_valid && ifu_flush) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_d_response_need_resend_cp:
      coverpoint (uncache_resp_valid && uncache_resp_need_resend) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvi_cross_8b_not_page_cp:
      coverpoint (beat_tail_request && !page_tail_request && entry_wait_resp &&
                  !entry_resending && tl_d_valid && response_is_rvi &&
                  !tl_d_corrupt && !tl_d_denied) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_page_tail_need_resend_redirect_cp:
      coverpoint (page_tail_request && uncache_resp_valid &&
                  uncache_resp_need_resend && uncache_redirect) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_uncache_redirect_half_pc_cp:
      coverpoint (uncache_redirect && uncache_resp_need_resend &&
                  uncache_pc == prev_half_pc) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_uncache_redirect_half_data_cp:
      coverpoint (uncache_redirect && uncache_resp_need_resend &&
                  prev_half_data == uncache_resp_data[15:0]) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_half_refetch_without_commit_wait_cp:
      coverpoint (prev_end_half_rvi && uncache_input_valid &&
                  uncache_state != WAIT_LAST_COMMIT) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_half_refetch_complete_rvi_cp:
      coverpoint (cross_page_half_pending && mmio_delivery &&
                  !delivered_is_rvc && single_delivery) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_page_tail_rvc_no_resend_cp:
      coverpoint (page_tail_request && uncache_resp_valid &&
                  uncache_resp_data[1:0] != 2'b11 && !uncache_resp_need_resend) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_page_tail_rvc_no_half_wait_cp:
      coverpoint (page_tail_request && uncache_resp_valid &&
                  uncache_resp_data[1:0] != 2'b11 && !prev_end_half_rvi) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_page_tail_rvc_2b_progress_cp:
      coverpoint (mmio_delivery && delivered_is_rvc && wb_redirect && !wb_path_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_first_page_half_exception_cp:
      coverpoint (page_tail_request && tl_d_valid && (tl_d_corrupt || tl_d_denied)) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_first_page_half_error_no_resend_cp:
      coverpoint (page_tail_request && tl_d_valid && (tl_d_corrupt || tl_d_denied) &&
                  !uncache_resp_need_resend) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_uncache_redirect_half_state_cp:
      coverpoint (uncache_redirect && uncache_resp_need_resend &&
                  uncache_pc == prev_half_pc && prev_half_data == uncache_resp_data[15:0]) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_second_page_half_exception_cp:
      coverpoint (cross_page_half_pending && to_ibuffer_valid &&
                  to_ibuffer_exception != 3'h0) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_cross_page_second_exception_cp:
      coverpoint ((cross_page_half_pending && s2_valid && s2_req_is_uncache &&
                   prev_end_half_rvi && !s2_use_uncache &&
                   to_ibuffer_exception_cross_page) ? s2_exception_0 : 3'h0) iff (!reset) {
        bins pf  = {3'h1};
        bins gpf = {3'h2};
        bins af  = {3'h3};
      }
    MMIO_both_pages_exception_cp:
      coverpoint (first_page_exception_seen && to_ibuffer_valid &&
                  to_ibuffer_exception != 3'h0) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_first_page_iaf_not_illegal_cp:
      coverpoint (first_page_exception_seen && to_ibuffer_valid &&
                  to_ibuffer_exception == 3'h3) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvc_single_delivery_cp:
      coverpoint (mmio_delivery && delivered_is_rvc && single_delivery) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_rvi_single_delivery_cp:
      coverpoint (mmio_delivery && !delivered_is_rvc && single_delivery) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_cross_8b_rvi_delivered_cp:
      coverpoint (cross_8b_resend_seen && uncache_resp_valid &&
                  !uncache_resp_need_resend && mmio_delivery && !delivered_is_rvc) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_cross_8b_second_a_cp:
      coverpoint (cross_8b_resend_seen && entry_resending && tl_a_valid &&
                  tl_a_addr == {1'b0, entry_req_addr[46:3] + 44'd1, 3'b000}) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_cross_8b_second_d_cp:
      coverpoint {tl_d_corrupt, tl_d_denied} iff (!reset && cross_8b_resend_seen &&
                  entry_resending && entry_wait_resp && tl_d_valid) {
        bins clean = {2'b00};
        bins corrupt = {2'b10};
        bins corrupt_and_denied = {2'b11};
      }
    MMIO_cfi_uses_common_predecode_cp:
      coverpoint cfi_delivery_kind iff (!reset) {
        bins branch = {3'b101};
        bins jal_call = {3'b110};
        bins jalr_ret = {3'b111};
      }
    MMIO_checker_redirect_flushes_old_path_cp:
      coverpoint (wb_path_valid && wb_redirect && s2_req_is_uncache) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_result_held_by_ibuffer_backpressure_cp:
      coverpoint (uncache_resp_valid && to_ibuffer_valid && !to_ibuffer_ready) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_completion_uncache_flush_wb_cp:
      coverpoint (mmio_delivery && wb_redirect && !wb_path_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_backend_redirect_over_writeback_cp:
      coverpoint (backend_redirect && uncache_resp_valid && !uncache_redirect) iff (!reset) {
        bins observed = {1'b1};
      }
    MMIO_tl_error_to_instruction_slot_cp:
      coverpoint tl_error_kind iff (!reset) {
        bins denied = {2'h1};
        bins corrupt = {2'h2};
      }
    MMIO_pending_blocks_wfi_safe_cp:
      coverpoint (mmio_pending && !wfi_safe) iff (!reset) {
        bins observed = {1'b1};
      }
  endgroup

  frontend_mmio_fetch_cg mmio_fetch_cg = new();

endmodule
