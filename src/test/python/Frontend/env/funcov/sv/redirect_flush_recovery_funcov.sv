module frontend_redirect_flush_recovery_funcov (
  input logic                 clock,
  input logic                 reset,
  input logic                 backend_redirect_valid,
  input logic [49:0]          backend_redirect_target,
  input logic                 backend_redirect_level,
  input logic                 backend_redirect_is_rvc,
  input logic                 backend_redirect_iaf,
  input logic                 backend_redirect_ipf,
  input logic                 backend_redirect_igpf,
  input logic                 backend_redirect_is_ctrl,
  input logic                 backend_redirect_is_mem_vio,
  input logic                 ftq_idx_ahead_valid,
  input logic                 ftq_idx_ahead_flag,
  input logic [5:0]           ftq_idx_ahead_value,
  input logic                 backend_redirect_ftq_idx_flag,
  input logic [5:0]           backend_redirect_ftq_idx_value,
  input logic                 to_bpu_redirect_valid,
  input logic [48:0]          to_bpu_redirect_target,
  input logic                 icache_redirect_flush,
  input logic                 bpu_flush_valid,
  input logic                 ifu_wb_redirect_valid,
  input logic [49:0]          ifu_wb_redirect_target,
  input logic                 ifu_wb_redirect_taken,
  input logic                 ifu_wb_redirect_is_rvc,
  input logic                 ibuffer_empty,
  input logic                 icache_response_valid,
  input logic                 uncache_response_valid,
  input logic                 uncache_response_need_resend,
  input logic                 ifu_to_ibuffer_valid,
  input logic                 icache_to_ifu_valid,
  input logic                 itlb_flush_pipe,
  input logic                 fencei,
  input logic                 sfence_valid,
  input logic                 satp_changed,
  input logic                 vsatp_changed,
  input logic                 hgatp_changed,
  input logic                 priv_virt_changed,
  input logic [7:0]           cfvec_valid,
  input logic [7:0][49:0]     cfvec_pc,
  input logic [7:0]           cfvec_iaf,
  input logic [7:0]           cfvec_ipf,
  input logic [7:0]           cfvec_igpf,
  input logic [7:0]           cfvec_backend_exception
);

  logic        recovery_pending;
  logic [3:0]  recovery_age;
  logic [49:0] recovery_target;
  logic        recovery_iaf;
  logic        recovery_ipf;
  logic        recovery_igpf;
  logic        recovery_unclassified;
  logic        recovery_was_delayed;
  logic        cfvec_target_seen;
  logic        cfvec_iaf_seen;
  logic        cfvec_ipf_seen;
  logic        cfvec_igpf_seen;
  logic        cfvec_backend_exception_seen;

  wire ahead_idx_match = ftq_idx_ahead_valid &&
    {ftq_idx_ahead_flag, ftq_idx_ahead_value} ==
      {backend_redirect_ftq_idx_flag, backend_redirect_ftq_idx_value};
  wire redirect_other = backend_redirect_valid &&
    !backend_redirect_is_ctrl && !backend_redirect_is_mem_vio &&
    !backend_redirect_iaf && !backend_redirect_ipf && !backend_redirect_igpf;

  always_comb begin
    cfvec_target_seen = 1'b0;
    cfvec_iaf_seen = 1'b0;
    cfvec_ipf_seen = 1'b0;
    cfvec_igpf_seen = 1'b0;
    cfvec_backend_exception_seen = 1'b0;
    for (int slot = 0; slot < 8; slot++) begin
      cfvec_target_seen |= cfvec_valid[slot] && cfvec_pc[slot] == recovery_target;
      cfvec_iaf_seen |= cfvec_valid[slot] && cfvec_iaf[slot];
      cfvec_ipf_seen |= cfvec_valid[slot] && cfvec_ipf[slot];
      cfvec_igpf_seen |= cfvec_valid[slot] && cfvec_igpf[slot];
      cfvec_backend_exception_seen |= cfvec_valid[slot] && cfvec_backend_exception[slot];
    end

  end

  always_ff @(posedge clock) begin
    if (reset) begin
      recovery_pending <= 1'b0;
      recovery_age <= '0;
      recovery_target <= '0;
      recovery_iaf <= 1'b0;
      recovery_ipf <= 1'b0;
      recovery_igpf <= 1'b0;
      recovery_unclassified <= 1'b0;
      recovery_was_delayed <= 1'b0;
    end else begin
      if (backend_redirect_valid) begin
        recovery_pending <= 1'b1;
        recovery_age <= '0;
        recovery_target <= backend_redirect_target;
        recovery_iaf <= backend_redirect_iaf;
        recovery_ipf <= backend_redirect_ipf;
        recovery_igpf <= backend_redirect_igpf;
        recovery_unclassified <= redirect_other;
        recovery_was_delayed <= !ahead_idx_match;
      end else if (recovery_pending) begin
        recovery_age <= recovery_age + 1'b1;
        if (&recovery_age) recovery_pending <= 1'b0;
      end
    end
  end

  covergroup frontend_redirect_flush_recovery_cg @(posedge clock);
    option.per_instance = 1;

    RFR_control_flow_redirect_cp: coverpoint (backend_redirect_valid && backend_redirect_is_ctrl) iff (!reset) {
      bins observed = {1'b1};
    }
    RFR_memory_violation_redirect_cp: coverpoint (backend_redirect_valid && backend_redirect_is_mem_vio) iff (!reset) {
      bins observed = {1'b1};
    }
    RFR_unclassified_redirect_cp: coverpoint (redirect_other) iff (!reset) {
      bins observed = {1'b1};
    }
    RFR_unclassified_redirect_recovery_cp: coverpoint (recovery_pending && recovery_unclassified &&
          cfvec_backend_exception_seen) iff (!reset) {
      bins backend_exception_cfvec = {1'b1};
    }
    RFR_iaf_delivery_cp: coverpoint (recovery_pending && recovery_iaf && cfvec_iaf_seen) iff (!reset) { bins observed = {1'b1}; }
    RFR_ipf_delivery_cp: coverpoint (recovery_pending && recovery_ipf && cfvec_ipf_seen) iff (!reset) { bins observed = {1'b1}; }
    RFR_igpf_delivery_cp: coverpoint (recovery_pending && recovery_igpf && cfvec_igpf_seen) iff (!reset) { bins observed = {1'b1}; }

    RFR_redirect_range_cp:
      coverpoint {backend_redirect_valid, backend_redirect_level, backend_redirect_is_rvc}
        iff (!reset) {
        bins flush_after_rvi = {3'b100};
        bins flush_after_rvc = {3'b101};
        bins flush_itself_rvi = {3'b110};
        bins flush_itself_rvc = {3'b111};
    }

    RFR_ftq_idx_ahead_match_cp: coverpoint (backend_redirect_valid && ahead_idx_match && to_bpu_redirect_valid) iff (!reset) {
      bins observed = {1'b1};
    }
    RFR_delayed_redirect_cp: coverpoint (recovery_pending && recovery_was_delayed && to_bpu_redirect_valid) iff (!reset) {
      bins observed = {1'b1};
    }

    RFR_backend_redirect_to_bpu_cp: coverpoint (recovery_pending && to_bpu_redirect_valid &&
      to_bpu_redirect_target == recovery_target[48:0] && !ifu_wb_redirect_valid) iff (!reset) {
      bins target_matched = {1'b1};
    }
    RFR_ibuffer_delayed_empty_cp: coverpoint (recovery_pending && recovery_age == 4'd1 && ibuffer_empty) iff (!reset) {
      bins empty_at_t1 = {1'b1};
    }
    RFR_icache_redirect_flush_cp: coverpoint (recovery_pending && icache_redirect_flush) iff (!reset) {
      bins flush_observed = {1'b1};
    }
    RFR_ifu_pipeline_flush_cp: coverpoint (backend_redirect_valid && !ifu_to_ibuffer_valid) iff (!reset) {
      bins no_ibuffer_write = {1'b1};
    }
    RFR_late_icache_response_recovery_cp: coverpoint (recovery_pending && recovery_age >= 4'd1 &&
      icache_response_valid && cfvec_target_seen) iff (!reset) {
      bins target_recovered = {1'b1};
    }
    RFR_late_uncache_response_recovery_cp: coverpoint (recovery_pending && recovery_age >= 4'd1 &&
      uncache_response_valid && cfvec_target_seen) iff (!reset) {
      bins target_recovered = {1'b1};
    }
    RFR_prefetch_recovery_cp: coverpoint (recovery_pending && icache_redirect_flush && itlb_flush_pipe) iff (!reset) {
      bins flushpipe_observed = {1'b1};
    }
    RFR_main_fetch_recovery_cp: coverpoint (recovery_pending && recovery_age >= 4'd1 && icache_to_ifu_valid) iff (!reset) {
      bins recovered_request = {1'b1};
    }
    RFR_cfvec_target_recovery_cp: coverpoint (recovery_pending && recovery_age >= 4'd2 && cfvec_target_seen) iff (!reset) {
      bins target_recovered = {1'b1};
    }

    RFR_uncache_cross_page_redirect_cp: coverpoint (ifu_wb_redirect_valid && uncache_response_valid &&
      uncache_response_need_resend && !backend_redirect_valid) iff (!reset) { bins observed = {1'b1}; }
    RFR_uncache_sequential_redirect_cp: coverpoint (ifu_wb_redirect_valid && uncache_response_valid &&
      !uncache_response_need_resend && !backend_redirect_valid) iff (!reset) { bins observed = {1'b1}; }
    RFR_ifu_taken_target_redirect_cp: coverpoint (ifu_wb_redirect_valid && ifu_wb_redirect_taken &&
      ifu_wb_redirect_target != '0 && !backend_redirect_valid) iff (!reset) { bins observed = {1'b1}; }
    RFR_ifu_not_cfi_redirect_cp: coverpoint (ifu_wb_redirect_valid && !ifu_wb_redirect_taken &&
      !ifu_wb_redirect_is_rvc && !backend_redirect_valid) iff (!reset) { bins observed = {1'b1}; }
    RFR_ifu_invalid_taken_rvc_redirect_cp: coverpoint (ifu_wb_redirect_valid && !ifu_wb_redirect_taken &&
      ifu_wb_redirect_is_rvc && !backend_redirect_valid) iff (!reset) { bins observed = {1'b1}; }

    RFR_prefetch_flushpipe_cp: coverpoint (itlb_flush_pipe && (icache_redirect_flush || bpu_flush_valid)) iff (!reset) {
      bins flushpipe_observed = {1'b1};
    }
    RFR_bpu_s3_override_cp: coverpoint (bpu_flush_valid && !backend_redirect_valid) iff (!reset) {
      bins flush_observed = {1'b1};
    }
    RFR_fencei_cp: coverpoint (fencei) iff (!reset) {
      bins fencei_observed = {1'b1};
    }
    RFR_sfence_cp: coverpoint (sfence_valid) iff (!reset) {
      bins sfence_observed = {1'b1};
    }
    RFR_tlb_csr_changed_cp: coverpoint ((satp_changed || vsatp_changed || hgatp_changed || priv_virt_changed)) iff (!reset) {
      bins change_observed = {1'b1};
    }
  endgroup

  frontend_redirect_flush_recovery_cg cg = new();
endmodule
