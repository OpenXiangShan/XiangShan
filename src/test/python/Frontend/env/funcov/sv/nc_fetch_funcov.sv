module frontend_nc_fetch_funcov (
  input logic         clock,
  input logic         reset,
  input logic         s2_valid,
  input logic         s2_req_is_uncache,
  input logic         s2_use_uncache,
  input logic         s2_pmp_mmio_0,
  input logic [1:0]   s2_pbmt_0,
  input logic [2:0]   s2_exception_0,
  input logic         uncache_input_valid,
  input logic         uncache_input_ready,
  input logic [1:0]   uncache_state,
  input logic         uncache_busy,
  input logic         ifu_stall,
  input logic         backend_empty,
  input logic         ibuffer_empty,
  input logic         backend_can_accept,
  input logic         tl_a_valid,
  input logic         tl_a_ready,
  input logic [47:0]  tl_a_addr,
  input logic         tl_d_valid,
  input logic [255:0] tl_d_data,
  input logic         tl_d_corrupt,
  input logic         tl_d_denied,
  input logic [1:0]   entry_state,
  input logic         entry_resending,
  input logic [46:0]  entry_req_addr,
  input logic         uncache_resp_valid,
  input logic [31:0]  uncache_resp_data,
  input logic [2:0]   uncache_resp_exception,
  input logic         uncache_resp_need_resend,
  input logic         to_ibuffer_valid,
  input logic         to_ibuffer_ready,
  input logic [35:0]  to_ibuffer_enq,
  input logic [35:0]  to_ibuffer_is_rvc,
  input logic [48:0]  to_ibuffer_pc_0,
  input logic [2:0]   to_ibuffer_exception,
  input logic         to_ibuffer_exception_cross_page,
  input logic         backend_redirect,
  input logic         ifu_flush,
  input logic         uncache_redirect,
  input logic         wb_redirect,
  input logic         wb_path_valid,
  input logic [1:0]   uncache_branch_type,
  input logic         prev_end_half_rvi,
  input logic [15:0]  prev_half_data,
  input logic [48:0]  prev_half_pc,
  input logic [48:0]  uncache_pc,
  input logic         wfi_safe,
  input logic         icache_waylookup_valid,
  input logic [3:0]   icache_waymask_0,
  input logic [3:0]   icache_waymask_1
);

  localparam logic [1:0] IDLE             = 2'h0;
  localparam logic [1:0] WAIT_LAST_COMMIT = 2'h1;
  localparam logic [1:0] SEND_REQ         = 2'h2;
  localparam logic [1:0] WAIT_RESP        = 2'h3;
  localparam logic [1:0] PBMT_NC          = 2'h1;
  localparam logic [2:0] PATH_CACHEABLE   = 3'h1;
  localparam logic [2:0] PATH_NC          = 3'h2;
  localparam logic [2:0] PATH_MMIO        = 3'h3;
  logic        nc_active;
  logic        last_nc_delivery_seen;
  logic        last_nc_is_rvc;
  logic        last_nc_clean;
  logic [48:0] last_nc_pc;
  logic        prev_backend_can_accept;
  logic [1:0]  prev_uncache_state;
  logic        prev_tl_a_fire;
  logic        stalled_a_seen;
  logic [47:0] stalled_a_addr;
  logic        cross_8b_pending;
  logic        cross_page_pending;
  logic        nc_redirect_pending;
  logic        nc_to_mmio_pending;
  logic        previous_path_valid;
  logic [2:0]  previous_path;

  wire nc_candidate = s2_valid && s2_req_is_uncache &&
    !s2_pmp_mmio_0 && s2_pbmt_0 == PBMT_NC;
  wire mmio_candidate = s2_valid && s2_req_is_uncache &&
    (s2_pmp_mmio_0 || s2_pbmt_0 != PBMT_NC);
  wire uncache_input_fire = uncache_input_valid && uncache_input_ready;
  wire nc_accept = nc_candidate && s2_use_uncache && uncache_input_fire;
  wire tl_a_fire = tl_a_valid && tl_a_ready;
  wire entry_wait_resp = entry_state == WAIT_RESP;
  wire page_tail_request = &entry_req_addr[11:1];
  wire beat_tail_request = &entry_req_addr[2:1];
  wire [15:0] tl_d_first_half = tl_d_data[entry_req_addr[1:0] * 16 +: 16];
  wire response_is_rvc = tl_d_first_half[1:0] != 2'b11;
  wire response_is_rvi = tl_d_first_half[1:0] == 2'b11;
  wire single_delivery = $onehot(to_ibuffer_enq);
  wire delivered_is_rvc = |(to_ibuffer_enq & to_ibuffer_is_rvc);
  wire nc_delivery = nc_active && to_ibuffer_valid && to_ibuffer_ready;
  wire nc_clean_delivery = nc_delivery && uncache_resp_exception == 3'h0;
  wire nc_pending = nc_candidate || nc_active ||
    (uncache_busy && uncache_state != IDLE);
  wire [3:0] nc_path_selection =
    (s2_valid && s2_req_is_uncache) ? {1'b1, s2_pmp_mmio_0, s2_pbmt_0} : 4'h0;
  wire nc_waymask_hit = icache_waylookup_valid &&
    (icache_waymask_0 != 4'h0 || icache_waymask_1 != 4'h0);
  wire [1:0] nc_waymask_kind =
    (nc_accept && icache_waylookup_valid) ?
      (nc_waymask_hit ? 2'h2 : 2'h1) : 2'h0;
  wire [2:0] nc_d_response_kind =
    (nc_active && entry_wait_resp && tl_d_valid) ?
      {1'b1, tl_d_corrupt, tl_d_denied} : 3'h0;
  wire [2:0] nc_delivery_type =
    (nc_clean_delivery && single_delivery) ?
      {1'b1, delivered_is_rvc, 1'b0} : 3'h0;
  wire [2:0] nc_consecutive_type =
    (last_nc_delivery_seen && last_nc_clean && nc_clean_delivery) ?
      {1'b1, last_nc_is_rvc, delivered_is_rvc} : 3'h0;
  wire [2:0] nc_cfi_kind = !nc_delivery ? 3'h0 :
    uncache_branch_type == 2'h0 ? 3'h1 :
    uncache_branch_type == 2'h1 ? 3'h2 : 3'h3;
  wire mmio_accept = mmio_candidate && uncache_input_fire;
  wire cacheable_delivery = !nc_active && s2_valid && !s2_req_is_uncache &&
    to_ibuffer_valid && to_ibuffer_ready;
  wire path_event = nc_accept || mmio_accept || cacheable_delivery;
  wire [2:0] current_path = nc_accept ? PATH_NC :
    mmio_accept ? PATH_MMIO : PATH_CACHEABLE;
  wire [6:0] path_transition =
    (previous_path_valid && path_event) ?
      {1'b1, previous_path, current_path} : 7'h0;
  wire nc_to_mmio_event = previous_path_valid &&
    previous_path == PATH_NC && mmio_accept;

  always_ff @(posedge clock) begin
    if (reset) begin
      nc_active <= 1'b0;
      last_nc_delivery_seen <= 1'b0;
      last_nc_is_rvc <= 1'b0;
      last_nc_clean <= 1'b0;
      last_nc_pc <= '0;
      prev_backend_can_accept <= 1'b0;
      prev_uncache_state <= IDLE;
      prev_tl_a_fire <= 1'b0;
      stalled_a_seen <= 1'b0;
      stalled_a_addr <= '0;
      cross_8b_pending <= 1'b0;
      cross_page_pending <= 1'b0;
      nc_redirect_pending <= 1'b0;
      nc_to_mmio_pending <= 1'b0;
      previous_path_valid <= 1'b0;
      previous_path <= '0;
    end else begin
      if (ifu_flush) begin
        nc_active <= 1'b0;
      end else if (nc_accept) begin
        nc_active <= 1'b1;
      end else if (uncache_resp_valid) begin
        nc_active <= 1'b0;
      end

      if (ifu_flush || backend_redirect) begin
        last_nc_delivery_seen <= 1'b0;
      end else if (nc_delivery) begin
        last_nc_delivery_seen <= 1'b1;
        last_nc_is_rvc <= delivered_is_rvc;
        last_nc_clean <= uncache_resp_exception == 3'h0;
        last_nc_pc <= uncache_pc;
      end

      prev_backend_can_accept <= backend_can_accept;
      prev_uncache_state <= uncache_state;
      prev_tl_a_fire <= tl_a_fire;

      if (nc_active && tl_a_valid && !tl_a_ready) begin
        stalled_a_seen <= 1'b1;
        stalled_a_addr <= tl_a_addr;
      end else if (tl_a_fire || ifu_flush) begin
        stalled_a_seen <= 1'b0;
      end

      if (nc_active && beat_tail_request && entry_wait_resp &&
          !entry_resending && tl_d_valid && response_is_rvi &&
          !tl_d_corrupt && !tl_d_denied) begin
        cross_8b_pending <= 1'b1;
      end else if ((cross_8b_pending && uncache_resp_valid) || ifu_flush) begin
        cross_8b_pending <= 1'b0;
      end

      if (nc_active && page_tail_request && uncache_resp_valid &&
          uncache_resp_need_resend) begin
        cross_page_pending <= 1'b1;
      end else if ((cross_page_pending && nc_delivery) || ifu_flush) begin
        cross_page_pending <= 1'b0;
      end

      if (nc_pending && backend_redirect) begin
        nc_redirect_pending <= 1'b1;
      end else if ((nc_redirect_pending && path_event) || ifu_flush) begin
        nc_redirect_pending <= 1'b0;
      end

      if (nc_to_mmio_event) begin
        nc_to_mmio_pending <= 1'b1;
      end else if ((nc_to_mmio_pending && uncache_state == WAIT_LAST_COMMIT) ||
                   ifu_flush) begin
        nc_to_mmio_pending <= 1'b0;
      end

      if (path_event) begin
        previous_path_valid <= 1'b1;
        previous_path <= current_path;
      end

    end
  end

  covergroup frontend_nc_fetch_cg @(posedge clock);
    option.per_instance = 1;

    NC_path_selection_cp:
      coverpoint nc_path_selection iff (!reset) {
        bins pbmt_nc = {4'b1001};
        bins pmp_mmio = {4'b1100, 4'b1101, 4'b1110, 4'b1111};
      }
    NC_path_with_fetch_exception_cp:
      coverpoint (s2_valid && s2_req_is_uncache &&
                  s2_pbmt_0 == PBMT_NC && s2_exception_0 != 3'h0) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_non_nc_fetch_exception_cp:
      coverpoint (s2_valid && s2_pbmt_0 != PBMT_NC &&
                  !s2_pmp_mmio_0 && s2_exception_0 != 3'h0) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_pbmt_nc_with_pmp_mmio_cp:
      coverpoint (s2_valid && s2_req_is_uncache && s2_pmp_mmio_0 &&
                  s2_pbmt_0 == PBMT_NC) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_uncache_path_accept_cp:
      coverpoint nc_accept iff (!reset) {
        bins observed = {1'b1};
      }
    NC_waymask_state_cp:
      coverpoint nc_waymask_kind iff (!reset) {
        bins miss = {2'h1};
        bins hit = {2'h2};
      }
    NC_idle_to_send_req_cp:
      coverpoint (nc_active && prev_uncache_state == IDLE &&
                  uncache_state == SEND_REQ) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_send_without_empty_gate_cp:
      coverpoint (nc_active && uncache_state == SEND_REQ &&
                  (!backend_empty || !ibuffer_empty)) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_single_outstanding_backpressure_cp:
      coverpoint (nc_active && uncache_state != IDLE &&
                  uncache_input_valid && !uncache_input_ready) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_send_req_ready_cp:
      coverpoint (nc_active && uncache_state == SEND_REQ &&
                  !ifu_stall && tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_send_req_ibuffer_stall_cp:
      coverpoint (nc_active && uncache_state == SEND_REQ &&
                  ifu_stall && !tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_tl_a_stall_cp:
      coverpoint (nc_active && tl_a_valid && !tl_a_ready) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_tl_a_stable_cp:
      coverpoint (stalled_a_seen && tl_a_valid && !tl_a_ready &&
                  tl_a_addr == stalled_a_addr) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_tl_a_stall_release_cp:
      coverpoint (stalled_a_seen && tl_a_fire &&
                  tl_a_addr == stalled_a_addr) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_tl_a_fire_to_wait_resp_cp:
      coverpoint (nc_active && prev_tl_a_fire &&
                  uncache_state == WAIT_RESP) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_wait_resp_rejects_new_req_cp:
      coverpoint (nc_active && uncache_state == WAIT_RESP &&
                  !uncache_input_ready) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_tl_d_response_cp:
      coverpoint nc_d_response_kind iff (!reset) {
        bins clean = {3'b100};
        bins corrupt = {3'b110};
        bins denied = {3'b101, 3'b111};
      }
    NC_response_to_idle_cp:
      coverpoint (nc_active && uncache_resp_valid &&
                  uncache_state == IDLE) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_backend_redirect_pending_cp:
      coverpoint (nc_pending && backend_redirect) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_backend_redirect_wait_resp_cp:
      coverpoint (nc_active && uncache_state == WAIT_RESP &&
                  backend_redirect) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_checker_redirect_pending_cp:
      coverpoint (nc_pending && wb_path_valid && wb_redirect) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_flush_with_response_cp:
      coverpoint (nc_active && ifu_flush && uncache_resp_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_pending_blocks_wfi_safe_cp:
      coverpoint (nc_pending && !wfi_safe) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_backend_can_accept_low_cp:
      coverpoint (nc_pending && !backend_can_accept) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_backend_can_accept_rise_cp:
      coverpoint (nc_pending && !prev_backend_can_accept &&
                  backend_can_accept) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_backend_can_accept_high_cp:
      coverpoint (nc_pending && prev_backend_can_accept &&
                  backend_can_accept) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_backend_can_accept_fall_cp:
      coverpoint (nc_pending && prev_backend_can_accept &&
                  !backend_can_accept) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_result_held_by_ibuffer_cp:
      coverpoint (nc_active && to_ibuffer_valid && !to_ibuffer_ready) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_single_delivery_type_cp:
      coverpoint nc_delivery_type iff (!reset) {
        bins rvi = {3'b100};
        bins rvc = {3'b110};
      }
    NC_consecutive_type_cp:
      coverpoint nc_consecutive_type iff (!reset) {
        bins rvi_rvi = {3'b100};
        bins rvi_rvc = {3'b101};
        bins rvc_rvi = {3'b110};
        bins rvc_rvc = {3'b111};
      }
    NC_rvc_pc_progress_cp:
      coverpoint (last_nc_delivery_seen && last_nc_is_rvc && nc_delivery &&
                  uncache_pc == last_nc_pc + 49'd1) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_rvi_pc_progress_cp:
      coverpoint (last_nc_delivery_seen && !last_nc_is_rvc && nc_delivery &&
                  uncache_pc == last_nc_pc + 49'd2) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_cfi_delivery_cp:
      coverpoint nc_cfi_kind iff (!reset) {
        bins non_cfi = {3'h1};
        bins branch = {3'h2};
        bins jump = {3'h3};
      }
    NC_cross_8b_rvi_cp:
      coverpoint (nc_active && beat_tail_request && entry_wait_resp &&
                  !entry_resending && tl_d_valid && response_is_rvi &&
                  !tl_d_corrupt && !tl_d_denied) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_8b_tail_rvc_no_resend_cp:
      coverpoint (nc_active && beat_tail_request && entry_wait_resp &&
                  !entry_resending && tl_d_valid && response_is_rvc &&
                  !tl_d_corrupt && !tl_d_denied) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_cross_8b_second_request_cp:
      coverpoint (cross_8b_pending && entry_resending && tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_cross_8b_complete_delivery_cp:
      coverpoint (cross_8b_pending && uncache_resp_valid &&
                  !uncache_resp_need_resend && nc_delivery &&
                  !delivered_is_rvc && single_delivery) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_page_tail_rvi_resend_cp:
      coverpoint (nc_active && page_tail_request && uncache_resp_valid &&
                  uncache_resp_need_resend) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_page_tail_half_redirect_cp:
      coverpoint (nc_active && page_tail_request && uncache_resp_valid &&
                  uncache_resp_need_resend && uncache_redirect) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_page_tail_half_state_cp:
      coverpoint (uncache_redirect && uncache_resp_need_resend &&
                  uncache_pc == prev_half_pc &&
                  prev_half_data == uncache_resp_data[15:0]) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_page_tail_rvc_no_resend_cp:
      coverpoint (nc_active && page_tail_request && uncache_resp_valid &&
                  uncache_resp_data[1:0] != 2'b11 &&
                  !uncache_resp_need_resend) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_cross_page_complete_delivery_cp:
      coverpoint (cross_page_pending && nc_delivery &&
                  !delivered_is_rvc && single_delivery) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_cross_page_exception_cp:
      coverpoint (cross_page_pending && to_ibuffer_valid &&
                  to_ibuffer_exception_cross_page ?
                  to_ibuffer_exception : 3'h0) iff (!reset) {
        bins pf = {3'h1};
        bins gpf = {3'h2};
        bins af = {3'h3};
      }
    NC_first_page_iaf_not_illegal_cp:
      coverpoint (nc_delivery && page_tail_request &&
                  to_ibuffer_exception == 3'h3) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_first_page_exception_cp:
      coverpoint (nc_active && page_tail_request &&
                  s2_exception_0 != 3'h0) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_path_transition_cp:
      coverpoint path_transition iff (!reset) {
        bins cacheable_to_nc = {7'b1001010};
        bins nc_to_cacheable = {7'b1010001};
        bins nc_to_mmio = {7'b1010011};
      }
    NC_attribute_boundary_cp:
      coverpoint path_transition iff (!reset) {
        bins nc_to_cacheable = {7'b1010001};
        bins nc_to_mmio = {7'b1010011};
      }
    NC_to_mmio_wait_commit_cp:
      coverpoint (nc_to_mmio_pending &&
                  uncache_state == WAIT_LAST_COMMIT) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_after_backend_redirect_cp:
      coverpoint (nc_redirect_pending && path_event) iff (!reset) {
        bins observed = {1'b1};
      }
    NC_cross_page_prev_half_resume_cp:
      coverpoint (cross_page_pending && prev_end_half_rvi && nc_delivery &&
                  !delivered_is_rvc && single_delivery) iff (!reset) {
        bins observed = {1'b1};
      }
  endgroup

  frontend_nc_fetch_cg nc_fetch_cg = new();

endmodule
