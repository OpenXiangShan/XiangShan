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

    RFR_control_flow_redirect_cp: coverpoint 1'b1 iff (!reset &&
      backend_redirect_valid && backend_redirect_is_ctrl) {
      bins observed = {1'b1};
    }
    RFR_memory_violation_redirect_cp: coverpoint 1'b1 iff (!reset &&
      backend_redirect_valid && backend_redirect_is_mem_vio) {
      bins observed = {1'b1};
    }
    RFR_unclassified_redirect_cp: coverpoint 1'b1 iff (!reset && redirect_other) {
      bins observed = {1'b1};
    }
    RFR_unclassified_redirect_recovery_cp: coverpoint 1'b1
      iff (!reset && recovery_pending && recovery_unclassified &&
          cfvec_backend_exception_seen) {
      bins backend_exception_cfvec = {1'b1};
    }
    RFR_iaf_delivery_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && recovery_iaf && cfvec_iaf_seen) { bins observed = {1'b1}; }
    RFR_ipf_delivery_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && recovery_ipf && cfvec_ipf_seen) { bins observed = {1'b1}; }
    RFR_igpf_delivery_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && recovery_igpf && cfvec_igpf_seen) { bins observed = {1'b1}; }

    RFR_redirect_range_cp: coverpoint {backend_redirect_level, backend_redirect_is_rvc}
      iff (!reset && backend_redirect_valid) {
      bins flush_after_rvi = {2'b00};
      bins flush_after_rvc = {2'b01};
      bins flush_itself_rvi = {2'b10};
      bins flush_itself_rvc = {2'b11};
    }

    RFR_ftq_idx_ahead_match_cp: coverpoint 1'b1 iff (!reset &&
      backend_redirect_valid && ahead_idx_match && to_bpu_redirect_valid) {
      bins observed = {1'b1};
    }
    RFR_delayed_redirect_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && recovery_was_delayed && to_bpu_redirect_valid) {
      bins observed = {1'b1};
    }

    RFR_backend_redirect_to_bpu_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && to_bpu_redirect_valid &&
      to_bpu_redirect_target == recovery_target[48:0] && !ifu_wb_redirect_valid) {
      bins target_matched = {1'b1};
    }
    RFR_ibuffer_delayed_empty_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && recovery_age == 4'd1 && ibuffer_empty) {
      bins empty_at_t1 = {1'b1};
    }
    RFR_icache_redirect_flush_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && icache_redirect_flush) {
      bins flush_observed = {1'b1};
    }
    RFR_ifu_pipeline_flush_cp: coverpoint 1'b1 iff (!reset &&
      backend_redirect_valid && !ifu_to_ibuffer_valid) {
      bins no_ibuffer_write = {1'b1};
    }
    RFR_late_icache_response_recovery_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && recovery_age >= 4'd1 &&
      icache_response_valid && cfvec_target_seen) {
      bins target_recovered = {1'b1};
    }
    RFR_late_uncache_response_recovery_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && recovery_age >= 4'd1 &&
      uncache_response_valid && cfvec_target_seen) {
      bins target_recovered = {1'b1};
    }
    RFR_prefetch_recovery_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && icache_redirect_flush && itlb_flush_pipe) {
      bins flushpipe_observed = {1'b1};
    }
    RFR_main_fetch_recovery_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && recovery_age >= 4'd1 && icache_to_ifu_valid) {
      bins recovered_request = {1'b1};
    }
    RFR_cfvec_target_recovery_cp: coverpoint 1'b1 iff (!reset &&
      recovery_pending && recovery_age >= 4'd2 && cfvec_target_seen) {
      bins target_recovered = {1'b1};
    }

    RFR_uncache_cross_page_redirect_cp: coverpoint 1'b1 iff (!reset &&
      ifu_wb_redirect_valid && uncache_response_valid &&
      uncache_response_need_resend && !backend_redirect_valid) { bins observed = {1'b1}; }
    RFR_uncache_sequential_redirect_cp: coverpoint 1'b1 iff (!reset &&
      ifu_wb_redirect_valid && uncache_response_valid &&
      !uncache_response_need_resend && !backend_redirect_valid) { bins observed = {1'b1}; }
    RFR_ifu_taken_target_redirect_cp: coverpoint 1'b1 iff (!reset &&
      ifu_wb_redirect_valid && ifu_wb_redirect_taken &&
      ifu_wb_redirect_target != '0 && !backend_redirect_valid) { bins observed = {1'b1}; }
    RFR_ifu_not_cfi_redirect_cp: coverpoint 1'b1 iff (!reset &&
      ifu_wb_redirect_valid && !ifu_wb_redirect_taken &&
      !ifu_wb_redirect_is_rvc && !backend_redirect_valid) { bins observed = {1'b1}; }
    RFR_ifu_invalid_taken_rvc_redirect_cp: coverpoint 1'b1 iff (!reset &&
      ifu_wb_redirect_valid && !ifu_wb_redirect_taken &&
      ifu_wb_redirect_is_rvc && !backend_redirect_valid) { bins observed = {1'b1}; }

    RFR_prefetch_flushpipe_cp: coverpoint 1'b1 iff (!reset &&
      itlb_flush_pipe && (icache_redirect_flush || bpu_flush_valid)) {
      bins flushpipe_observed = {1'b1};
    }
    RFR_bpu_s3_override_cp: coverpoint 1'b1 iff (!reset &&
      bpu_flush_valid && !backend_redirect_valid) {
      bins flush_observed = {1'b1};
    }
    RFR_fencei_cp: coverpoint 1'b1 iff (!reset && fencei) {
      bins fencei_observed = {1'b1};
    }
    RFR_sfence_cp: coverpoint 1'b1 iff (!reset && sfence_valid) {
      bins sfence_observed = {1'b1};
    }
    RFR_tlb_csr_changed_cp: coverpoint 1'b1 iff (!reset &&
      (satp_changed || vsatp_changed || hgatp_changed || priv_virt_changed)) {
      bins change_observed = {1'b1};
    }
  endgroup

  frontend_redirect_flush_recovery_cg cg = new();
endmodule

bind Frontend frontend_redirect_flush_recovery_funcov u_frontend_redirect_flush_recovery_funcov (
  .clock(clock),
  .reset(reset),
  .backend_redirect_valid(io_backend_toFtq_redirect_valid),
  .backend_redirect_target(io_backend_toFtq_redirect_bits_target),
  .backend_redirect_level(io_backend_toFtq_redirect_bits_level),
  .backend_redirect_is_rvc(io_backend_toFtq_redirect_bits_isRVC),
  .backend_redirect_iaf(io_backend_toFtq_redirect_bits_backendIAF),
  .backend_redirect_ipf(io_backend_toFtq_redirect_bits_backendIPF),
  .backend_redirect_igpf(io_backend_toFtq_redirect_bits_backendIGPF),
  .backend_redirect_is_ctrl(io_backend_toFtq_redirect_bits_debugIsCtrl),
  .backend_redirect_is_mem_vio(io_backend_toFtq_redirect_bits_debugIsMemVio),
  .ftq_idx_ahead_valid(io_backend_toFtq_ftqIdxAhead_valid),
  .ftq_idx_ahead_flag(io_backend_toFtq_ftqIdxAhead_bits_flag),
  .ftq_idx_ahead_value(io_backend_toFtq_ftqIdxAhead_bits_value),
  .backend_redirect_ftq_idx_flag(io_backend_toFtq_redirect_bits_ftqIdx_flag),
  .backend_redirect_ftq_idx_value(io_backend_toFtq_redirect_bits_ftqIdx_value),
  .to_bpu_redirect_valid(_inner_ftq_io_toBpu_redirect_valid),
  .to_bpu_redirect_target(_inner_ftq_io_toBpu_redirect_bits_target_addr),
  .icache_redirect_flush(_inner_ftq_io_toICache_redirectFlush),
  .bpu_flush_valid(_inner_ftq_io_toICache_flushFromBpu_s3_valid),
  .ifu_wb_redirect_valid(_inner_ifu_io_toFtq_wbRedirect_valid),
  .ifu_wb_redirect_target(_inner_ifu_io_toFtq_wbRedirect_bits_target),
  .ifu_wb_redirect_taken(_inner_ifu_io_toFtq_wbRedirect_bits_taken),
  .ifu_wb_redirect_is_rvc(_inner_ifu_io_toFtq_wbRedirect_bits_isRVC),
  .ibuffer_empty(_inner_ibuffer_io_empty),
  .icache_response_valid(auto_inner_icache_client_out_d_valid),
  .uncache_response_valid(auto_inner_instrUncache_client_out_d_valid),
  .uncache_response_need_resend(_inner_instrUncache_io_toIfu_resp_bits_needResend),
  .ifu_to_ibuffer_valid(_inner_ifu_io_toIBuffer_valid),
  .icache_to_ifu_valid(_inner_icache_io_toIfu_req_valid),
  .itlb_flush_pipe(_inner_icache_io_itlbFlushPipe),
  .fencei(io_fencei),
  .sfence_valid(io_sfence_valid),
  .satp_changed(io_tlbCsr_satp_changed),
  .vsatp_changed(io_tlbCsr_vsatp_changed),
  .hgatp_changed(io_tlbCsr_hgatp_changed),
  .priv_virt_changed(io_tlbCsr_priv_virt_changed),
  .cfvec_valid({io_backend_cfVec_7_valid, io_backend_cfVec_6_valid,
                io_backend_cfVec_5_valid, io_backend_cfVec_4_valid,
                io_backend_cfVec_3_valid, io_backend_cfVec_2_valid,
                io_backend_cfVec_1_valid, io_backend_cfVec_0_valid}),
  .cfvec_pc({io_backend_cfVec_7_bits_pc, io_backend_cfVec_6_bits_pc,
             io_backend_cfVec_5_bits_pc, io_backend_cfVec_4_bits_pc,
             io_backend_cfVec_3_bits_pc, io_backend_cfVec_2_bits_pc,
             io_backend_cfVec_1_bits_pc, io_backend_cfVec_0_bits_pc}),
  .cfvec_iaf({io_backend_cfVec_7_bits_exceptionVec_1, io_backend_cfVec_6_bits_exceptionVec_1,
              io_backend_cfVec_5_bits_exceptionVec_1, io_backend_cfVec_4_bits_exceptionVec_1,
              io_backend_cfVec_3_bits_exceptionVec_1, io_backend_cfVec_2_bits_exceptionVec_1,
              io_backend_cfVec_1_bits_exceptionVec_1, io_backend_cfVec_0_bits_exceptionVec_1}),
  .cfvec_ipf({io_backend_cfVec_7_bits_exceptionVec_12, io_backend_cfVec_6_bits_exceptionVec_12,
              io_backend_cfVec_5_bits_exceptionVec_12, io_backend_cfVec_4_bits_exceptionVec_12,
              io_backend_cfVec_3_bits_exceptionVec_12, io_backend_cfVec_2_bits_exceptionVec_12,
              io_backend_cfVec_1_bits_exceptionVec_12, io_backend_cfVec_0_bits_exceptionVec_12}),
  .cfvec_igpf({io_backend_cfVec_7_bits_exceptionVec_20, io_backend_cfVec_6_bits_exceptionVec_20,
               io_backend_cfVec_5_bits_exceptionVec_20, io_backend_cfVec_4_bits_exceptionVec_20,
               io_backend_cfVec_3_bits_exceptionVec_20, io_backend_cfVec_2_bits_exceptionVec_20,
               io_backend_cfVec_1_bits_exceptionVec_20, io_backend_cfVec_0_bits_exceptionVec_20}),
  .cfvec_backend_exception({io_backend_cfVec_7_bits_backendException, io_backend_cfVec_6_bits_backendException,
                            io_backend_cfVec_5_bits_backendException, io_backend_cfVec_4_bits_backendException,
                            io_backend_cfVec_3_bits_backendException, io_backend_cfVec_2_bits_backendException,
                            io_backend_cfVec_1_bits_backendException, io_backend_cfVec_0_bits_backendException})
);
