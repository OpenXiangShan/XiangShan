module frontend_ifu_exception_delivery_funcov (
  input logic             clock,
  input logic             reset,
  input logic             ptw_resp_valid,
  input logic [1:0]       ptw_resp_s2xlate,
  input logic             ptw_resp_s1_pf,
  input logic             ptw_resp_s1_af,
  input logic             ptw_resp_s2_gpf,
  input logic             ptw_resp_s2_gaf,
  input logic             ptw_req_valid,
  input logic             ptw_req_ready,
  input logic [37:0]      ptw_req_vpn,
  input logic [1:0]       ptw_req_s2xlate,
  input logic             icache_d_valid,
  input logic             icache_d_denied,
  input logic             icache_d_corrupt,
  input logic             icache_cross64_first_denied,
  input logic             icache_cross64_first_corrupt,
  input logic             icache_cross64_second_denied,
  input logic             icache_cross64_second_corrupt,
  input logic             icache_cross64_rvi,
  input logic             icache_ifu_req_valid,
  input logic [48:0]      icache_ifu_start_vaddr,
  input logic [31:0]      icache_ifu_maybe_rvc_map,
  input logic [1:0]       icache_ifu_pbmt,
  input logic             icache_ifu_pmp_mmio,
  input logic [2:0]       icache_ifu_exception,
  input logic             icache_meta_ecc_error,
  input logic             icache_data_ecc_error,
  input logic             backend_can_accept,
  input logic             uncache_d_valid,
  input logic             uncache_d_denied,
  input logic             uncache_d_corrupt,
  input logic             uncache_req_valid,
  input logic             uncache_req_ready,
  input logic [2:0]       uncache_req_addr_low,
  input logic             uncache_ifu_resp_valid,
  input logic             uncache_ifu_resp_denied,
  input logic             uncache_ifu_resp_corrupt,
  input logic             uncache_ifu_resp_need_resend,
  input logic [46:0]      uncache_entry_req_addr,
  input logic             ifu_to_ibuffer_valid,
  input logic             ifu_to_ibuffer_ready,
  input logic             ifu_to_ibuffer_ftq_flag,
  input logic [5:0]       ifu_to_ibuffer_ftq_value,
  input logic [2:0]       ifu_exception_type,
  input logic             ifu_exception_cross_page,
  input logic             ifu_s2_req_is_uncache,
  input logic             ifu_s2_prev_end_half_rvi,
  input logic             ifu_s2_ftq_flag,
  input logic [5:0]       ifu_s2_ftq_value,
  input logic [2:0]       ifu_s2_icache_exception,
  input logic [2:0]       ifu_uncache_exception,
  input logic             trigger_pc_match,
  input logic             ifu_s2_cacheable,
  input logic             uncache_response_valid,
  input logic [7:0]       cfvec_valid,
  input logic [7:0]       cfvec_is_rvc,
  input logic [7:0]       cfvec_illegal_instruction,
  input logic [7:0][3:0]  cfvec_trigger
);

  localparam logic [1:0] NO_STAGE    = 2'b00;
  localparam logic [1:0] ONLY_STAGE1 = 2'b01;
  localparam logic [1:0] ONLY_STAGE2 = 2'b10;
  localparam logic [1:0] ALL_STAGE   = 2'b11;

  logic cfvec_triggered;
  logic illegal_rvc_seen;
  logic uncache_cross8_pending;
  logic uncache_cross8_wait_second;
  logic [2:0] uncache_cross8_first_error_sample;
  logic [2:0] uncache_cross8_second_error_sample;
  logic       uncache_cross_page_pending;
  logic       uncache_cross_page_wait_resp;
  logic [37:0] uncache_cross_page_expected_vpn;
  logic [1:0]  uncache_cross_page_s2xlate;
  logic       uncache_cross_page_fault_pending;
  logic [3:0] uncache_cross_page_fault_kind;
  logic       uncache_cross_page_delivery_pending;
  logic       uncache_cross_page_ftq_flag;
  logic [5:0] uncache_cross_page_ftq_value;
  logic       cacheable_cross_page_pending;
  logic       cacheable_cross_page_wait_resp;
  logic [37:0] cacheable_cross_page_expected_vpn;
  logic [1:0]  cacheable_cross_page_s2xlate;
  logic       cacheable_cross_page_fault_pending;
  logic [3:0] cacheable_cross_page_fault_kind;

  wire ifu_to_ibuffer_fire = ifu_to_ibuffer_valid && ifu_to_ibuffer_ready;
  wire uncache_req_fire = uncache_req_valid && uncache_req_ready;
  wire uncache_cross8_req_fire = uncache_req_fire && uncache_req_addr_low[2:1] == 2'b11;
  wire uncache_cross8_first_resp =
    uncache_cross8_pending && !uncache_cross8_wait_second && uncache_ifu_resp_valid;
  wire uncache_cross8_second_resp =
    uncache_cross8_pending && uncache_cross8_wait_second && uncache_ifu_resp_valid;
  wire uncache_cross_page_first_half =
    (uncache_entry_req_addr[11:1] == 11'h7ff) &&
    uncache_ifu_resp_valid && uncache_ifu_resp_need_resend;
  wire ptw_req_fire = ptw_req_valid && ptw_req_ready;
  wire uncache_cross_page_ptw_req = uncache_cross_page_pending &&
    ptw_req_fire && ptw_req_vpn == uncache_cross_page_expected_vpn;
  wire uncache_cross_page_ptw_resp = uncache_cross_page_wait_resp &&
    ptw_resp_valid && ptw_resp_s2xlate == uncache_cross_page_s2xlate;
  wire uncache_cross_page_ptw_fault =
    uncache_cross_page_ptw_resp &&
    ((ptw_resp_s2xlate == ONLY_STAGE1 && (ptw_resp_s1_pf || ptw_resp_s1_af)) ||
     (ptw_resp_s2xlate == ONLY_STAGE2 && (ptw_resp_s2_gpf || ptw_resp_s2_gaf)) ||
     (ptw_resp_s2xlate == ALL_STAGE &&
      (ptw_resp_s1_pf || ptw_resp_s1_af || ptw_resp_s2_gpf || ptw_resp_s2_gaf)));
  wire [1:0] uncache_cross_page_no_stage_fault =
    uncache_cross_page_ptw_resp && ptw_resp_s2xlate == NO_STAGE
      ? {ptw_resp_s1_pf, ptw_resp_s1_af} : 2'b00;
  wire cacheable_cross_page_first_half = icache_ifu_req_valid &&
    icache_ifu_start_vaddr[11:0] == 12'hfe0 &&
    !icache_ifu_maybe_rvc_map[5'd15] && icache_ifu_pbmt == 2'b00 &&
    !icache_ifu_pmp_mmio && icache_ifu_exception == 3'd0;
  wire cacheable_cross_page_ptw_req = cacheable_cross_page_pending &&
    ptw_req_fire && ptw_req_vpn == cacheable_cross_page_expected_vpn;
  wire cacheable_cross_page_ptw_resp = cacheable_cross_page_wait_resp &&
    ptw_resp_valid && ptw_resp_s2xlate == cacheable_cross_page_s2xlate;
  wire cacheable_cross_page_ptw_fault = cacheable_cross_page_ptw_resp &&
    ((ptw_resp_s2xlate == NO_STAGE && (ptw_resp_s1_pf || ptw_resp_s1_af)) ||
     (ptw_resp_s2xlate == ONLY_STAGE1 && (ptw_resp_s1_pf || ptw_resp_s1_af)) ||
     (ptw_resp_s2xlate == ONLY_STAGE2 && (ptw_resp_s2_gpf || ptw_resp_s2_gaf)) ||
     (ptw_resp_s2xlate == ALL_STAGE &&
      (ptw_resp_s1_pf || ptw_resp_s1_af || ptw_resp_s2_gpf || ptw_resp_s2_gaf)));
  wire cross_page_exception = ifu_to_ibuffer_fire && ifu_exception_cross_page &&
    ifu_exception_type != 3'd0;
  wire uncache_cross_page_single_exception_source =
    (ifu_s2_icache_exception != 3'd0) ^ (ifu_uncache_exception != 3'd0);
  wire uncache_cross_page_exception_condition = cross_page_exception &&
    uncache_cross_page_delivery_pending &&
    {ifu_to_ibuffer_ftq_flag, ifu_to_ibuffer_ftq_value} ==
      {uncache_cross_page_ftq_flag, uncache_cross_page_ftq_value} &&
    uncache_cross_page_single_exception_source;
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
      uncache_cross8_pending <= 1'b0;
      uncache_cross8_wait_second <= 1'b0;
      uncache_cross8_first_error_sample <= 3'b000;
      uncache_cross8_second_error_sample <= 3'b000;
      uncache_cross_page_pending <= 1'b0;
      uncache_cross_page_wait_resp <= 1'b0;
      uncache_cross_page_expected_vpn <= '0;
      uncache_cross_page_s2xlate <= NO_STAGE;
      uncache_cross_page_fault_pending <= 1'b0;
      uncache_cross_page_fault_kind <= 4'd0;
      uncache_cross_page_delivery_pending <= 1'b0;
      uncache_cross_page_ftq_flag <= 1'b0;
      uncache_cross_page_ftq_value <= '0;
      cacheable_cross_page_pending <= 1'b0;
      cacheable_cross_page_wait_resp <= 1'b0;
      cacheable_cross_page_expected_vpn <= '0;
      cacheable_cross_page_s2xlate <= NO_STAGE;
      cacheable_cross_page_fault_pending <= 1'b0;
      cacheable_cross_page_fault_kind <= 4'd0;
    end else begin
      uncache_cross_page_fault_pending <= 1'b0;
      cacheable_cross_page_fault_pending <= 1'b0;
      illegal_rvc_seen <= illegal_rvc_cfvec;
      uncache_cross8_first_error_sample <= {
        uncache_cross8_first_resp,
        uncache_ifu_resp_corrupt,
        uncache_ifu_resp_denied
      };
      uncache_cross8_second_error_sample <= {
        uncache_cross8_second_resp,
        uncache_ifu_resp_corrupt,
        uncache_ifu_resp_denied
      };

      if (uncache_cross_page_first_half) begin
        uncache_cross_page_pending <= 1'b1;
        uncache_cross_page_wait_resp <= 1'b0;
        uncache_cross_page_expected_vpn <=
          {{2{uncache_entry_req_addr[46]}}, uncache_entry_req_addr[46:11]} + 1'b1;
        uncache_cross_page_fault_pending <= 1'b0;
        uncache_cross_page_fault_kind <= 4'd0;
        uncache_cross_page_delivery_pending <= ifu_s2_req_is_uncache &&
          ifu_s2_prev_end_half_rvi;
        uncache_cross_page_ftq_flag <= ifu_s2_ftq_flag;
        uncache_cross_page_ftq_value <= ifu_s2_ftq_value;
      end
      if (uncache_cross_page_exception_condition)
        uncache_cross_page_delivery_pending <= 1'b0;
      if (uncache_cross_page_ptw_req) begin
        uncache_cross_page_pending <= 1'b0;
        uncache_cross_page_wait_resp <= 1'b1;
        uncache_cross_page_s2xlate <= ptw_req_s2xlate;
      end
      if (uncache_cross_page_ptw_resp) begin
        uncache_cross_page_wait_resp <= 1'b0;
      end
      if (uncache_cross_page_ptw_fault) begin
        uncache_cross_page_fault_pending <= 1'b1;
        if (ptw_resp_s2xlate == ONLY_STAGE1)
          uncache_cross_page_fault_kind <= ptw_resp_s1_pf ? 4'd1 : 4'd2;
        else if (ptw_resp_s2xlate == ONLY_STAGE2)
          uncache_cross_page_fault_kind <= ptw_resp_s2_gpf ? 4'd3 : 4'd4;
        else if (ptw_resp_s1_pf)
          uncache_cross_page_fault_kind <= 4'd5;
        else if (ptw_resp_s1_af)
          uncache_cross_page_fault_kind <= 4'd6;
        else if (ptw_resp_s2_gpf)
          uncache_cross_page_fault_kind <= 4'd7;
        else
          uncache_cross_page_fault_kind <= 4'd8;
      end

      if (cacheable_cross_page_first_half) begin
        cacheable_cross_page_pending <= 1'b1;
        cacheable_cross_page_wait_resp <= 1'b0;
        cacheable_cross_page_expected_vpn <=
          {{1{icache_ifu_start_vaddr[48]}}, icache_ifu_start_vaddr[48:12]} + 1'b1;
        cacheable_cross_page_fault_kind <= 4'd0;
      end
      if (cacheable_cross_page_ptw_req) begin
        cacheable_cross_page_pending <= 1'b0;
        cacheable_cross_page_wait_resp <= 1'b1;
        cacheable_cross_page_s2xlate <= ptw_req_s2xlate;
      end
      if (cacheable_cross_page_ptw_resp) begin
        cacheable_cross_page_wait_resp <= 1'b0;
      end
      if (cacheable_cross_page_ptw_fault) begin
        cacheable_cross_page_fault_pending <= 1'b1;
        if (ptw_resp_s2xlate == NO_STAGE)
          cacheable_cross_page_fault_kind <= ptw_resp_s1_pf ? 4'd1 : 4'd2;
        else if (ptw_resp_s2xlate == ONLY_STAGE1)
          cacheable_cross_page_fault_kind <= ptw_resp_s1_pf ? 4'd3 : 4'd4;
        else if (ptw_resp_s2xlate == ONLY_STAGE2)
          cacheable_cross_page_fault_kind <= ptw_resp_s2_gpf ? 4'd5 : 4'd6;
        else if (ptw_resp_s1_pf)
          cacheable_cross_page_fault_kind <= 4'd7;
        else if (ptw_resp_s1_af)
          cacheable_cross_page_fault_kind <= 4'd8;
        else if (ptw_resp_s2_gpf)
          cacheable_cross_page_fault_kind <= 4'd9;
        else
          cacheable_cross_page_fault_kind <= 4'd10;
      end

      if (uncache_cross8_req_fire) begin
        uncache_cross8_pending <= 1'b1;
        uncache_cross8_wait_second <= 1'b0;
      end else if (uncache_cross8_first_resp) begin
        if (uncache_ifu_resp_need_resend && !uncache_ifu_resp_corrupt &&
            !uncache_ifu_resp_denied) begin
          uncache_cross8_wait_second <= 1'b1;
        end else begin
          uncache_cross8_pending <= 1'b0;
          uncache_cross8_wait_second <= 1'b0;
        end
      end else if (uncache_cross8_second_resp) begin
        uncache_cross8_pending <= 1'b0;
        uncache_cross8_wait_second <= 1'b0;
      end
    end
  end

  covergroup frontend_ifu_exception_delivery_cg @(posedge clock);
    option.per_instance = 1;

    IFED_ptw_no_stage_page_fault_cp: coverpoint (ptw_resp_valid && ptw_resp_s2xlate == NO_STAGE && ptw_resp_s1_pf) iff (!reset) {
      bins observed = {1'b1};
    }
    IFED_ptw_no_stage_access_fault_cp: coverpoint (ptw_resp_valid && ptw_resp_s2xlate == NO_STAGE && ptw_resp_s1_af) iff (!reset) {
      bins observed = {1'b1};
    }
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

    IFED_icache_d_channel_error_cp:
      coverpoint {
        icache_d_valid,
        icache_d_corrupt,
        icache_d_denied
      } iff (!reset) {
        bins corrupt = {3'b110};
        bins denied = {3'b101};
        bins corrupt_and_denied = {3'b111};
    }
    IFED_icache_cross64_first_error_cp:
      coverpoint {
        icache_cross64_rvi,
        icache_cross64_first_corrupt,
        icache_cross64_first_denied
      } iff (!reset) {
        bins corrupt = {3'b110};
        bins denied = {3'b101};
        bins corrupt_and_denied = {3'b111};
      }
    IFED_icache_cross64_second_error_cp:
      coverpoint {
        icache_cross64_rvi,
        icache_cross64_first_corrupt,
        icache_cross64_first_denied,
        icache_cross64_second_corrupt,
        icache_cross64_second_denied
      } iff (!reset) {
        bins corrupt = {5'b10010};
        bins denied = {5'b10001};
        bins corrupt_and_denied = {5'b10011};
      }
    IFED_icache_meta_ecc_error_cp:
      coverpoint (icache_meta_ecc_error && backend_can_accept) iff (!reset) {
        bins observed = {1'b1};
    }
    IFED_icache_data_ecc_error_cp:
      coverpoint (icache_data_ecc_error && backend_can_accept) iff (!reset) {
        bins observed = {1'b1};
    }
    IFED_uncache_d_channel_error_cp:
      coverpoint {
        uncache_d_valid,
        uncache_d_corrupt,
        uncache_d_denied
      } iff (!reset) {
        bins corrupt = {3'b110};
        bins denied = {3'b101};
        bins corrupt_and_denied = {3'b111};
    }
    IFED_uncache_cross8_first_error_cp:
      coverpoint uncache_cross8_first_error_sample iff (!reset) {
        bins corrupt = {3'b110};
        bins denied = {3'b101};
        bins corrupt_and_denied = {3'b111};
    }
    IFED_uncache_cross8_second_error_cp:
      coverpoint uncache_cross8_second_error_sample iff (!reset) {
        bins corrupt = {3'b110};
        bins denied = {3'b101};
        bins corrupt_and_denied = {3'b111};
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
    IFED_cacheable_trigger_condition_cp:
      coverpoint (trigger_pc_match && ifu_s2_cacheable) iff (!reset) {
        bins observed = {1'b1};
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
    IFED_uncache_cross_page_condition_cp:
      coverpoint uncache_cross_page_exception_condition iff (!reset) {
        bins observed = {1'b1};
      }
    IFED_uncache_cross_page_ptw_fault_cp: coverpoint uncache_cross_page_fault_kind iff (!reset && uncache_cross_page_fault_pending) {
      bins only_stage1_page_fault = {4'd1};
      bins only_stage1_access_fault = {4'd2};
      bins only_stage2_guest_page_fault = {4'd3};
      bins only_stage2_guest_access_fault = {4'd4};
      bins all_stage_vs_page_fault = {4'd5};
      bins all_stage_vs_access_fault = {4'd6};
      bins all_stage_g_page_fault = {4'd7};
      bins all_stage_g_access_fault = {4'd8};
    }
    IFED_uncache_cross_page_no_stage_ptw_fault_cp:
      coverpoint uncache_cross_page_no_stage_fault iff (!reset) {
        bins page_fault = {2'b10};
        bins access_fault = {2'b01};
      }
    IFED_cacheable_cross_page_ptw_fault_cp:
      coverpoint cacheable_cross_page_fault_kind iff (!reset && cacheable_cross_page_fault_pending) {
        bins no_stage_page_fault = {4'd1};
        bins no_stage_access_fault = {4'd2};
        bins only_stage1_page_fault = {4'd3};
        bins only_stage1_access_fault = {4'd4};
        bins only_stage2_guest_page_fault = {4'd5};
        bins only_stage2_guest_access_fault = {4'd6};
        bins all_stage_vs_page_fault = {4'd7};
        bins all_stage_vs_access_fault = {4'd8};
        bins all_stage_g_page_fault = {4'd9};
        bins all_stage_g_access_fault = {4'd10};
      }
  endgroup

  frontend_ifu_exception_delivery_cg cg = new();
endmodule
