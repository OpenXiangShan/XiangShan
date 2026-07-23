module frontend_ifu_exception_delivery_funcov (
  input logic             clock,
  input logic             reset,
  input logic             ptw_resp_valid,
  input logic [1:0]       ptw_resp_s2xlate,
  input logic             ptw_resp_s1_pf,
  input logic             ptw_resp_s1_af,
  input logic             ptw_resp_s2_gpf,
  input logic             ptw_resp_s2_gaf,
  input logic             ifu_to_ibuffer_valid,
  input logic             ifu_to_ibuffer_ready,
  input logic [2:0]       ifu_exception_type,
  input logic             ifu_exception_cross_page,
  input logic             uncache_response_valid,
  input logic [7:0]       cfvec_valid,
  input logic [7:0]       cfvec_is_rvc,
  input logic [7:0]       cfvec_illegal_instruction,
  input logic [7:0][3:0]  cfvec_trigger
);

  localparam logic [1:0] ONLY_STAGE1 = 2'b01;
  localparam logic [1:0] ONLY_STAGE2 = 2'b10;
  localparam logic [1:0] ALL_STAGE   = 2'b11;

  logic cfvec_triggered;
  logic illegal_rvc_seen;

  wire ifu_to_ibuffer_fire = ifu_to_ibuffer_valid && ifu_to_ibuffer_ready;
  wire cross_page_exception = ifu_to_ibuffer_fire && ifu_exception_cross_page &&
    ifu_exception_type != 3'd0;
  wire illegal_rvc_cfvec = |(cfvec_valid & cfvec_is_rvc & cfvec_illegal_instruction);
  wire any_cfvec_exception = |(cfvec_valid & cfvec_illegal_instruction);

  always_comb begin
    cfvec_triggered = 1'b0;
    for (int slot = 0; slot < 8; slot++) begin
      cfvec_triggered |= cfvec_valid[slot] && cfvec_trigger[slot] != 4'd0;
    end
  end

  always_ff @(posedge clock) begin
    if (reset) begin
      illegal_rvc_seen <= 1'b0;
    end else begin
      illegal_rvc_seen <= illegal_rvc_cfvec;
    end
  end

  covergroup frontend_ifu_exception_delivery_cg @(posedge clock);
    option.per_instance = 1;

    IFED_ptw_only_stage1_page_fault_cp: coverpoint (ptw_resp_valid && ptw_resp_s2xlate == ONLY_STAGE1 && ptw_resp_s1_pf) iff (!reset) {
      bins observed = {1'b1};
    }
    IFED_ptw_only_stage1_access_fault_cp: coverpoint (ptw_resp_valid && ptw_resp_s2xlate == ONLY_STAGE1 && ptw_resp_s1_af) iff (!reset) {
      bins observed = {1'b1};
    }
    IFED_ptw_only_stage2_guest_page_fault_cp: coverpoint (ptw_resp_valid && ptw_resp_s2xlate == ONLY_STAGE2 && ptw_resp_s2_gpf) iff (!reset) {
      bins observed = {1'b1};
    }
    IFED_ptw_only_stage2_guest_access_fault_cp: coverpoint (ptw_resp_valid && ptw_resp_s2xlate == ONLY_STAGE2 && ptw_resp_s2_gaf) iff (!reset) {
      bins observed = {1'b1};
    }
    IFED_ptw_all_stage_page_fault_cp: coverpoint (ptw_resp_valid && ptw_resp_s2xlate == ALL_STAGE && ptw_resp_s1_pf) iff (!reset) {
      bins observed = {1'b1};
    }
    IFED_ptw_all_stage_access_fault_cp: coverpoint (ptw_resp_valid && ptw_resp_s2xlate == ALL_STAGE && ptw_resp_s1_af) iff (!reset) {
      bins observed = {1'b1};
    }
    IFED_ptw_all_stage_guest_page_fault_cp: coverpoint (ptw_resp_valid && ptw_resp_s2xlate == ALL_STAGE && ptw_resp_s2_gpf) iff (!reset) {
      bins observed = {1'b1};
    }
    IFED_ptw_all_stage_guest_access_fault_cp: coverpoint (ptw_resp_valid && ptw_resp_s2xlate == ALL_STAGE && ptw_resp_s2_gaf) iff (!reset) {
      bins observed = {1'b1};
    }

    IFED_cross_page_exception_delivery_cp:
      coverpoint {cross_page_exception, ifu_exception_type} iff (!reset) {
        bins instruction_page_fault = {4'b1001};
        bins instruction_guest_page_fault = {4'b1010};
        bins instruction_access_fault = {4'b1011};
    }
    IFED_trigger_delivery_cp: coverpoint (cfvec_triggered) iff (!reset) {
      bins trigger_marked = {1'b1};
    }
    IFED_illegal_rvc_cp: coverpoint (illegal_rvc_cfvec) iff (!reset) {
      bins observed = {1'b1};
    }
    IFED_legal_after_illegal_rvc_cp: coverpoint (illegal_rvc_seen && cfvec_valid != '0 && !any_cfvec_exception) iff (!reset) {
      bins observed = {1'b1};
    }
    IFED_uncache_cross_page_exception_cp: coverpoint (cross_page_exception && uncache_response_valid) iff (!reset) {
      bins observed = {1'b1};
    }
  endgroup

  frontend_ifu_exception_delivery_cg cg = new();
endmodule
