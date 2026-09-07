module frontend_ibuffer_funcov (
  input logic             clock,
  input logic             reset,
  input logic             in_ready,
  input logic             in_valid,
  input logic [35:0]      in_valid_lanes,
  input logic [35:0]      in_enq_enable,
  input logic [35:0]      in_is_rvc,
  input logic [2:0]       in_exception_type,
  input logic             in_backend_exception,
  input logic             in_exception_cross_page,
  input logic             ibuffer_empty,
  input logic             flush,
  input logic             backend_can_accept,
  input logic [7:0]       cfvec_valid,
  input logic [7:0][9:0]  cfvec_foldpc,
  input logic [7:0][5:0]  cfvec_ftq_value,
  input logic [7:0][4:0]  cfvec_ftq_offset,
  input logic [7:0]       cfvec_is_rvc,
  input logic             from_ftq_wen,
  input logic [5:0]       from_ftq_idx,
  input logic [49:0]      from_ftq_start_pc_addr
);

  logic        empty_last;
  logic        in_fire_last;
  logic [2:0]  backpressure_cycles;
  logic [6:0]  enqueued_since_flush;
  logic [49:0] ftq_start_pc [0:63];
  logic [63:0] ftq_start_pc_valid;

  function automatic logic [9:0] fold_pc(input logic [49:0] pc);
    logic [48:0] half_pc;
    begin
      half_pc = pc[49:1];
      fold_pc = half_pc[9:0] ^ half_pc[19:10] ^ half_pc[29:20] ^
        half_pc[39:30] ^ {1'b0, half_pc[48:40]};
    end
  endfunction

  wire in_fire = in_valid && in_ready;
  wire [35:0] active_input_lanes = in_valid_lanes & in_enq_enable;
  wire [5:0] input_count = $countones(active_input_lanes);
  wire [35:0] active_rvc = in_is_rvc & active_input_lanes;
  wire [3:0] tail_delivery_count = backend_can_accept && cfvec_valid != '0 ?
    $countones(cfvec_valid) : '0;
  wire [49:0] cfvec_0_pc = ftq_start_pc[cfvec_ftq_value[0]] +
    {44'd0, cfvec_ftq_offset[0], 1'b0} - (cfvec_is_rvc[0] ? 50'd0 : 50'd2);
  wire cfvec_0_pc_valid = ftq_start_pc_valid[cfvec_ftq_value[0]] &&
    fold_pc(cfvec_0_pc) == cfvec_foldpc[0];
  wire [2:0] queued_input_pc_offset = {
    !empty_last && backend_can_accept && cfvec_valid[0] && cfvec_0_pc_valid,
    cfvec_0_pc[2:1]
  };
  wire [2:0] bypass_pc_offset = {
    empty_last && in_fire && backend_can_accept && cfvec_valid[0] && cfvec_0_pc_valid,
    cfvec_0_pc[2:1]
  };

  always_ff @(posedge clock) begin
    if (reset) begin
      empty_last <= 1'b1;
      in_fire_last <= 1'b0;
      backpressure_cycles <= '0;
      enqueued_since_flush <= '0;
      ftq_start_pc_valid <= '0;
    end else begin
      empty_last <= ibuffer_empty;
      in_fire_last <= in_fire;
      if (from_ftq_wen) begin
        ftq_start_pc[from_ftq_idx] <= {from_ftq_start_pc_addr[48:0], 1'b0};
        ftq_start_pc_valid[from_ftq_idx] <= 1'b1;
      end

      if (backend_can_accept) begin
        backpressure_cycles <= '0;
      end else if (backpressure_cycles != 3'd7) begin
        backpressure_cycles <= backpressure_cycles + 1'b1;
      end

      if (flush) begin
        enqueued_since_flush <= '0;
      end else if (in_fire) begin
        if (enqueued_since_flush + input_count >= 7'd48)
          enqueued_since_flush <= 7'd48;
        else
          enqueued_since_flush <= enqueued_since_flush + input_count;
      end
    end
  end

  covergroup frontend_ibuffer_funcov_cg @(posedge clock);
    option.per_instance = 1;

    IBUF_rvi_16_delivery_cp: coverpoint (in_fire &&
      backend_can_accept && input_count == 6'd16 && active_rvc == '0) iff (!reset) {
      bins input_16_rvi = {1'b1};
    }
    IBUF_rvc_32_delivery_cp: coverpoint (in_fire &&
      backend_can_accept && input_count == 6'd32 && active_rvc == active_input_lanes) iff (!reset) {
      bins input_32_rvc = {1'b1};
    }
    IBUF_mixed_rvi_rvc_delivery_cp: coverpoint (in_fire &&
      (active_rvc != '0) && (active_rvc != active_input_lanes)) iff (!reset) {
      bins mixed_input = {1'b1};
    }
    IBUF_tail_delivery_cp: coverpoint tail_delivery_count iff (!reset) {
      bins one_to_seven = {[1:7]};
    }
    IBUF_adjacent_input_blocks_cp: coverpoint (in_fire_last && in_fire) iff (!reset) {
      bins consecutive_blocks = {1'b1};
    }

    IBUF_residual_then_enqueue_cp: coverpoint (!ibuffer_empty && in_fire) iff (!reset) {
      bins enqueue_with_residual = {1'b1};
    }
    IBUF_enqueue_dequeue_same_cycle_cp: coverpoint (!ibuffer_empty && backend_can_accept && in_fire) iff (!reset) {
      bins concurrent = {1'b1};
    }
    IBUF_pointer_wrap_traffic_cp: coverpoint (enqueued_since_flush == 7'd48) iff (!reset) {
      bins at_least_one_capacity = {1'b1};
    }
    IBUF_queued_input_pc_offset_cp: coverpoint queued_input_pc_offset iff (!reset) {
      bins offset_0 = {3'b100};
      bins offset_1 = {3'b101};
      bins offset_2 = {3'b110};
      bins offset_3 = {3'b111};
    }

    IBUF_bypass_up_to_8_cp: coverpoint (empty_last &&
      in_fire && backend_can_accept && input_count inside {[1:8]}) iff (!reset) {
      bins up_to_8 = {1'b1};
    }
    IBUF_bypass_over_8_cp: coverpoint (empty_last &&
      in_fire && backend_can_accept && input_count > 6'd8) iff (!reset) {
      bins over_8 = {1'b1};
    }
    IBUF_bypass_pc_offset_cp: coverpoint bypass_pc_offset iff (!reset) {
      bins offset_0 = {3'b100};
      bins offset_1 = {3'b101};
      bins offset_2 = {3'b110};
      bins offset_3 = {3'b111};
    }

    IBUF_single_cycle_backpressure_cp: coverpoint (backend_can_accept && backpressure_cycles == 3'd1) iff (!reset) {
      bins recovered = {1'b1};
    }
    IBUF_multi_cycle_backpressure_cp: coverpoint (backend_can_accept && backpressure_cycles >= 3'd2) iff (!reset) {
      bins recovered = {1'b1};
    }
    IBUF_backpressure_recovery_multi_delivery_cp: coverpoint (backend_can_accept &&
      backpressure_cycles >= 3'd1 && tail_delivery_count >= 4'd2) iff (!reset) {
      bins observed = {1'b1};
    }
    IBUF_backpressure_with_cfvec_cp: coverpoint (!backend_can_accept && cfvec_valid != '0) iff (!reset) {
      bins cfvec_present = {1'b1};
    }
    IBUF_upstream_backpressure_cp: coverpoint (!backend_can_accept && !in_ready) iff (!reset) {
      bins input_ready_low = {1'b1};
    }

    IBUF_flush_nonempty_cp: coverpoint (flush &&
      !ibuffer_empty) iff (!reset) {
      bins buffered_path = {1'b1};
    }
    IBUF_flush_with_ifu_input_cp: coverpoint (flush &&
      in_valid) iff (!reset) {
      bins concurrent = {1'b1};
    }

    IBUF_instruction_page_fault_cp: coverpoint (in_fire &&
      in_exception_type == 3'd1) iff (!reset) {
      bins observed = {1'b1};
    }
    IBUF_instruction_guest_page_fault_cp: coverpoint (in_fire &&
      in_exception_type == 3'd2) iff (!reset) {
      bins observed = {1'b1};
    }
    IBUF_instruction_access_fault_cp: coverpoint (in_fire &&
      in_exception_type == 3'd3) iff (!reset) {
      bins observed = {1'b1};
    }
    IBUF_illegal_instruction_cp: coverpoint (in_fire &&
      in_exception_type == 3'd4) iff (!reset) {
      bins observed = {1'b1};
    }
    IBUF_hardware_error_cp: coverpoint (in_fire &&
      in_exception_type == 3'd5) iff (!reset) {
      bins observed = {1'b1};
    }
    IBUF_backend_exception_cp: coverpoint (in_fire &&
      in_exception_type != 3'd0 && in_backend_exception) iff (!reset) {
      bins observed = {1'b1};
    }
    IBUF_cross_page_exception_cp: coverpoint (in_fire &&
      in_exception_type != 3'd0 && in_exception_cross_page) iff (!reset) {
      bins observed = {1'b1};
    }
  endgroup

  frontend_ibuffer_funcov_cg cg = new();
endmodule
