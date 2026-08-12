module frontend_wfi_funcov (
  input logic        clock,
  input logic        reset,
  input logic        backend_wfi_req,
  input logic        delayed_wfi_req,
  input logic        backend_wfi_safe,
  input logic        icache_wfi_safe,
  input logic        instr_uncache_wfi_safe,
  input logic        icache_tl_a_valid,
  input logic        icache_tl_a_ready,
  input logic        instr_uncache_tl_a_valid,
  input logic        instr_uncache_tl_a_ready,
  input logic [13:0] icache_mshr_valid,
  input logic [13:0] icache_mshr_issue,
  input logic [1:0]  instr_uncache_entry_state,
  input logic        frontend_flush,
  input logic        backend_redirect
);

  localparam logic [1:0] REFILL_REQ  = 2'h1;
  localparam logic [1:0] REFILL_RESP = 2'h2;

  logic delayed_wfi_req_last_cycle;
  logic icache_issued_last_cycle;

  wire icache_a_fire = icache_tl_a_valid && icache_tl_a_ready;
  wire instr_uncache_a_fire =
    instr_uncache_tl_a_valid && instr_uncache_tl_a_ready;
  wire icache_mshr_unissued = |(icache_mshr_valid & ~icache_mshr_issue);
  wire icache_mshr_issued = |(icache_mshr_valid & icache_mshr_issue);
  wire icache_multiple_mshr_issued =
    $countones(icache_mshr_valid & icache_mshr_issue) >= 2;
  wire icache_prefetch_unissued =
    |(icache_mshr_valid[13:4] & ~icache_mshr_issue[13:4]);
  wire instr_uncache_refill_req = instr_uncache_entry_state == REFILL_REQ;
  wire instr_uncache_refill_resp = instr_uncache_entry_state == REFILL_RESP;

  always_ff @(posedge clock) begin
    if (reset) begin
      delayed_wfi_req_last_cycle <= 1'b0;
      icache_issued_last_cycle <= 1'b0;
    end else begin
      delayed_wfi_req_last_cycle <= delayed_wfi_req;
      icache_issued_last_cycle <= icache_mshr_issued;
    end
  end

  covergroup frontend_wfi_funcov_cg @(posedge clock);
    WFI_req_propagation_delay_cp:
      coverpoint (backend_wfi_req && delayed_wfi_req &&
                  !delayed_wfi_req_last_cycle) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_idle_safe_return_cp:
      coverpoint (delayed_wfi_req && !icache_mshr_valid &&
                  instr_uncache_entry_state == 2'h0 && icache_wfi_safe &&
                  instr_uncache_wfi_safe && backend_wfi_safe) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_safe_gated_by_req_cp:
      coverpoint (!backend_wfi_req && icache_wfi_safe &&
                  instr_uncache_wfi_safe && !backend_wfi_safe) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_safe_and_cp:
      coverpoint (delayed_wfi_req &&
                  (icache_wfi_safe ^ instr_uncache_wfi_safe) &&
                  !backend_wfi_safe) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_blocks_external_a_cp:
      coverpoint (delayed_wfi_req &&
                  (icache_mshr_unissued || instr_uncache_refill_req) &&
                  !icache_tl_a_valid && !instr_uncache_tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_release_reissues_blocked_request_cp:
      coverpoint (delayed_wfi_req_last_cycle && !delayed_wfi_req &&
                  (icache_mshr_unissued || instr_uncache_refill_req) &&
                  (icache_a_fire || instr_uncache_a_fire)) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_flush_or_redirect_cp:
      coverpoint (delayed_wfi_req && (frontend_flush || backend_redirect) &&
                  (icache_mshr_unissued || instr_uncache_refill_req) &&
                  !icache_tl_a_valid && !instr_uncache_tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_reset_request_cp:
      coverpoint (reset && backend_wfi_req) {
        bins observed = {1'b1};
      }
    WFI_icache_unissued_mshr_gated_cp:
      coverpoint (delayed_wfi_req && icache_mshr_unissued &&
                  !icache_tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_icache_issued_mshr_pending_cp:
      coverpoint (delayed_wfi_req && icache_mshr_issued &&
                  !icache_wfi_safe && !backend_wfi_safe) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_icache_pending_completion_cp:
      coverpoint (icache_issued_last_cycle && delayed_wfi_req &&
                  !icache_mshr_issued && icache_wfi_safe) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_icache_multiple_mshr_pending_cp:
      coverpoint (delayed_wfi_req && icache_multiple_mshr_issued &&
                  !icache_wfi_safe) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_prefetch_unissued_gated_cp:
      coverpoint (delayed_wfi_req && icache_prefetch_unissued &&
                  !icache_tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_instr_uncache_refill_req_gated_cp:
      coverpoint (delayed_wfi_req && instr_uncache_refill_req &&
                  !instr_uncache_tl_a_valid) iff (!reset) {
        bins observed = {1'b1};
      }
    WFI_instr_uncache_refill_resp_pending_cp:
      coverpoint (delayed_wfi_req && instr_uncache_refill_resp &&
                  !instr_uncache_wfi_safe && !backend_wfi_safe) iff (!reset) {
        bins observed = {1'b1};
      }
  endgroup

  frontend_wfi_funcov_cg wfi_funcov_cg = new();

endmodule
