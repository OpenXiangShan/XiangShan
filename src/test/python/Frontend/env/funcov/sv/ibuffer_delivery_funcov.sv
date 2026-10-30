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

// VCS-side coverage for the 33 VType leaves in Frontend_testpoint_v1.0.csv.
// Checkpoints are verified separately; these points observe scenario conditions.
module frontend_vtype_funcov (
  input logic clock, reset,
  input logic decode_can_accept, redirect, resuming_vtype,
  input logic walk_to_arch, walk_valid,
  input logic [8:0] walk_type,
  input logic commit_valid, commit_vsetvl,
  input logic [8:0] commit_type, vsetvl_type,
  input logic [5:0] queued,
  input logic [3:0] dequeued,
  input logic enq_fire, enq_has_vset, enq_has_instruction,
  input logic [7:0] cf_valid,
  input logic [7:0][31:0] cf_instr,
  input logic [7:0][2:0] cf_fetch_fault
);
  function automatic logic is_vli(input logic [31:0] x);
    return x[31] == 0 && x[14:12] == 3'b111 && x[6:0] == 7'h57;
  endfunction
  function automatic logic is_ivli(input logic [31:0] x);
    return x[31:30] == 2'b11 && x[14:12] == 3'b111 && x[6:0] == 7'h57;
  endfunction
  function automatic logic is_vl(input logic [31:0] x);
    return x[31:25] == 7'b1000000 && x[14:12] == 3'b111 && x[6:0] == 7'h57;
  endfunction
  function automatic logic is_imm(input logic [31:0] x);
    return is_vli(x) || is_ivli(x);
  endfunction
  function automatic logic is_plain(input logic [31:0] x);
    return !is_imm(x) && !is_vl(x);
  endfunction
  function automatic logic reserved(input logic [31:0] x);
    return is_vli(x) ? |x[30:28] : is_ivli(x) && |x[29:28];
  endfunction
  function automatic int vlmax(input logic [5:0] sew_lmul);
    int sew;
    int max_sew;
    begin
      sew = 8 << sew_lmul[5:3];
      if (sew_lmul[5:3] > 3 || sew_lmul[2:0] == 4)
        return 0;
      case (sew_lmul[2:0])
        5: max_sew = 8;
        6: max_sew = 16;
        7: max_sew = 32;
        default: max_sew = 64;
      endcase
      if (sew > max_sew) return 0;
      if (sew_lmul[2:0] <= 3)
        return (128 / sew) << sew_lmul[2:0];
      return 128 / (sew << (8 - sew_lmul[2:0]));
    end
  endfunction
  function automatic logic legal(input logic [31:0] x);
    return is_imm(x) && !reserved(x) && vlmax(x[25:20]) != 0;
  endfunction
  function automatic logic keep_vl(input logic [31:0] x);
    return is_vli(x) && x[11:7] == 0 && x[19:15] == 0;
  endfunction
  function automatic logic ordinary_vset(input logic [31:0] x);
    return legal(x) && !keep_vl(x);
  endfunction

  localparam int F_REDIRECT_WALK_COMMIT = 0;
  localparam int F_REDIRECT_ARCH_WALK_COMMIT = 1;
  localparam int F_COMMIT_WITH_NEW_VSET = 2;
  localparam int F_COMMIT_WALK_EQUAL = 3;
  localparam int F_COMMIT_WALK_DIFFERENT = 4;
  localparam int F_PAYLOAD_WITHOUT_COMMIT = 5;
  localparam int F_VSETVL_COMMIT_EQUAL_RESUMING = 6;
  localparam int F_VSETVL_COMMIT_DIFFERENT_RESUMING = 7;
  localparam int F_VSETVL_COMMIT_DIFFERENT_RUNNING = 8;
  localparam int F_VSETVL_COMMIT_WALK_DIFFERENT = 9;
  localparam int F_VSETVL_COMMIT_WALK_EQUAL = 10;

  logic [7:0] cf_active, cf_vli, cf_ivli, cf_vl, cf_plain;
  logic [7:0] cf_legal, cf_reserved, cf_unsupported, cf_keep_vl;
  logic [7:0] cf_ordinary, cf_fault;
  logic [7:0] before_a, before_plain, before_middle_vset, before_middle_vl;
  logic [7:0] before_imm_vl, before_vl_imm, before_keep_base;
  logic [7:0] before_reserved_vli, before_reserved_ivli;
  logic [7:0] before_unsupported, before_legal_after_illegal, before_bp_vset;
  logic [7:0] before_multiple_vsets, cfg_diff_prev;
  logic [7:0] vlmax_changed, vlmax_nonzero;
  logic [7:0] vlmax_matches_prev, vlmax_matches_first;
  logic quiet;
  logic seen_a, prior_plain, middle_vset, middle_vl;
  logic imm_before_vl, vl_before_imm;
  logic illegal_before, keep_base, vl_seen;
  logic [5:0] first_cfg, prev_cfg;
  logic have_a, have_prior_plain, have_vset_middle, have_vl_middle;
  logic have_imm_before_vl, have_vl_before_imm;
  logic have_illegal, have_keep_base, have_vl;
  logic [5:0] cfg_a, cfg_prev;
  logic [8:0] arch_type;
  logic [8:0] last_output_type;
  logic [8:0] last_output_type_next;
  logic [1:0] vset_count;
  logic [1:0] vset_count_next;
  logic [1:0] reserved_followup, reserved_followup_next;
  logic unsupported_followup, unsupported_followup_next;
  logic legal_after_illegal, legal_after_illegal_next;
  logic resume_prev;
  logic [1:0] quiet_resume_count;
  logic resume_rise_with_queue;
  logic backpressure_prev;
  logic [1:0] backpressure_count;
  logic backpressure_vset;
  logic backpressure_released, backpressure_output_vset;
  logic redirect_walk_pending, redirect_arch_walk_pending;
  logic redirect_seen, walk_after_arch;
  logic ordinary_queued_after_vl, newpath_vset_enqueued;
  logic walk_recovery_pending, arch_recovery_pending;
  logic [8:0] recovery_type;
  logic [8:0] recovery_old_type;
  logic [10:0] followup_pending;
  logic [8:0] followup_type [0:10];
  logic [8:0] followup_other [0:10];
  logic [1:0] last_vset_kind;
  logic [1:0] last_vset_kind_next;
  logic [1:0] pair_pending_kind;
  logic [2:0] pair_sample;
  logic [8:0] prior_vsetvl_type;
  logic prior_vsetvl_type_valid;

  assign quiet = !redirect && !resuming_vtype && !walk_valid &&
    !walk_to_arch && !commit_valid && !commit_vsetvl;

  function automatic logic output_vset_except(
    input logic [8:0] first_type, second_type,
    input logic check_second
  );
    output_vset_except = 1'b0;
    for (int i = 0; i < 8; i++)
      output_vset_except |= cf_active[i] && cf_ordinary[i] &&
        {1'b0, cf_instr[i][27:20]} != first_type &&
        (!check_second || {1'b0, cf_instr[i][27:20]} != second_type);
  endfunction

  function automatic logic followup_completed();
    followup_completed = 1'b0;
    for (int i = 0; i < 11; i++)
      followup_completed |= followup_pending[i] &&
        output_vset_except(followup_type[i], followup_other[i],
          i == F_REDIRECT_WALK_COMMIT || i == F_REDIRECT_ARCH_WALK_COMMIT ||
          i == F_COMMIT_WITH_NEW_VSET || i == F_PAYLOAD_WITHOUT_COMMIT);
  endfunction

  always_comb begin : scan_cfvec
    logic [31:0] x;
    logic bp_vset;
    seen_a = have_a;
    prior_plain = have_prior_plain;
    middle_vset = have_vset_middle;
    middle_vl = have_vl_middle;
    imm_before_vl = have_imm_before_vl;
    vl_before_imm = have_vl_before_imm;
    illegal_before = have_illegal;
    keep_base = have_keep_base;
    vl_seen = have_vl;
    first_cfg = cfg_a;
    prev_cfg = cfg_prev;
    vset_count_next = vset_count;
    reserved_followup_next = reserved_followup;
    unsupported_followup_next = unsupported_followup;
    legal_after_illegal_next = legal_after_illegal;
    last_output_type_next = last_output_type;
    last_vset_kind_next = last_vset_kind;
    bp_vset = backpressure_output_vset;

    for (int i = 0; i < 8; i++) begin
      x = cf_instr[i];
      cf_active[i] = quiet && decode_can_accept && cf_valid[i];
      cf_vli[i] = is_vli(x);
      cf_ivli[i] = is_ivli(x);
      cf_vl[i] = is_vl(x);
      cf_plain[i] = is_plain(x);
      cf_legal[i] = legal(x);
      cf_reserved[i] = reserved(x);
      cf_unsupported[i] = is_imm(x) && !reserved(x) && vlmax(x[25:20]) == 0;
      cf_keep_vl[i] = keep_vl(x);
      cf_ordinary[i] = ordinary_vset(x);
      cf_fault[i] = |cf_fetch_fault[i];
      before_a[i] = seen_a;
      before_plain[i] = prior_plain;
      before_middle_vset[i] = middle_vset;
      before_middle_vl[i] = middle_vl;
      before_imm_vl[i] = imm_before_vl;
      before_vl_imm[i] = vl_before_imm;
      before_keep_base[i] = keep_base;
      before_reserved_vli[i] = reserved_followup_next[0];
      before_reserved_ivli[i] = reserved_followup_next[1];
      before_unsupported[i] = unsupported_followup_next;
      before_legal_after_illegal[i] = legal_after_illegal_next;
      before_bp_vset[i] = bp_vset;
      before_multiple_vsets[i] = vset_count_next >= 1;
      cfg_diff_prev[i] = x[25:20] != prev_cfg;
      vlmax_changed[i] = vlmax(prev_cfg) != vlmax(first_cfg);
      vlmax_nonzero[i] = vlmax(x[25:20]) != 0;
      vlmax_matches_prev[i] = vlmax(x[25:20]) == vlmax(prev_cfg);
      vlmax_matches_first[i] = vlmax(x[25:20]) == vlmax(first_cfg);

      if (cf_active[i]) begin
        if (cf_plain[i]) begin
          reserved_followup_next = 0;
          unsupported_followup_next = 0;
          legal_after_illegal_next = 0;
          prior_plain = seen_a;
        end
        if (is_imm(x)) begin
          if (cf_vli[i] && cf_reserved[i] && seen_a)
            reserved_followup_next[0] = 1;
          if (cf_ivli[i] && cf_reserved[i] && seen_a)
            reserved_followup_next[1] = 1;
          if (cf_unsupported[i] && seen_a)
            unsupported_followup_next = 1;
          if (cf_legal[i] && illegal_before)
            legal_after_illegal_next = 1;
          if (cf_ordinary[i]) begin
            if (backpressure_released) bp_vset = 1;
            if (vset_count_next != 3) vset_count_next = vset_count_next + 1'b1;
            if (!seen_a) begin
              first_cfg = x[25:20];
              seen_a = 1;
            end else if (cfg_diff_prev[i]) begin
              middle_vset = prior_plain;
              imm_before_vl = 1;
              keep_base = 1;
            end
            prev_cfg = x[25:20];
            last_output_type_next = {1'b0, x[27:20]};
            last_vset_kind_next = cf_vli[i] ? 2'd1 : 2'd2;
            vl_before_imm = 0;
            illegal_before = 0;
          end else if (cf_reserved[i])
            illegal_before = 1;
        end
        if (cf_vl[i] && seen_a) begin
          vl_before_imm = 1;
          middle_vl = prior_plain;
          vl_seen = 1;
        end
      end
    end
  end

  always_comb begin
    pair_sample = 0;
    if (quiet && followup_pending[F_COMMIT_WITH_NEW_VSET] &&
        output_vset_except(followup_type[F_COMMIT_WITH_NEW_VSET], followup_other[F_COMMIT_WITH_NEW_VSET], 1'b1))
      pair_sample = {1'b0, pair_pending_kind} + 3'd1;
  end

  always_ff @(posedge clock) begin
    if (reset) begin
      have_a <= 0; have_prior_plain <= 0; have_vset_middle <= 0;
      have_vl_middle <= 0;
      have_imm_before_vl <= 0; have_vl_before_imm <= 0;
      have_illegal <= 0; have_keep_base <= 0; have_vl <= 0;
      cfg_a <= 0; cfg_prev <= 0;
      arch_type <= 9'b100000000;
      last_output_type <= 9'b100000000;
      last_vset_kind <= 0; pair_pending_kind <= 0;
      vset_count <= 0; reserved_followup <= 0;
      unsupported_followup <= 0; legal_after_illegal <= 0;
      resume_prev <= 0;
      quiet_resume_count <= 0; resume_rise_with_queue <= 0;
      backpressure_prev <= 0; backpressure_count <= 0;
      backpressure_vset <= 0; backpressure_released <= 0;
      backpressure_output_vset <= 0;
      redirect_walk_pending <= 0;
      redirect_arch_walk_pending <= 0; redirect_seen <= 0;
      walk_after_arch <= 0;
      ordinary_queued_after_vl <= 0; newpath_vset_enqueued <= 0;
      walk_recovery_pending <= 0; arch_recovery_pending <= 0;
      recovery_type <= 0; recovery_old_type <= 0;
      followup_pending <= 0;
      prior_vsetvl_type <= 0; prior_vsetvl_type_valid <= 0;
    end else begin
      resume_prev <= resuming_vtype;
      if (!resuming_vtype || redirect || walk_valid || walk_to_arch ||
          commit_valid || commit_vsetvl) quiet_resume_count <= 0;
      else if (quiet_resume_count != 3)
        quiet_resume_count <= quiet_resume_count + 1'b1;
      if (!resume_prev && resuming_vtype)
        resume_rise_with_queue <= queued != 0 && decode_can_accept &&
          !redirect && !walk_valid && !walk_to_arch && !commit_valid && !commit_vsetvl;
      if (!resuming_vtype || redirect || walk_valid || walk_to_arch ||
          commit_valid || commit_vsetvl) resume_rise_with_queue <= 0;
      backpressure_prev <= !decode_can_accept;
      if (quiet && backpressure_vset && backpressure_count >= 2 &&
          backpressure_prev && decode_can_accept)
        backpressure_released <= 1;
      if (backpressure_released && quiet && decode_can_accept) begin
        for (int i = 0; i < 8; i++)
          if (cf_valid[i] && ordinary_vset(cf_instr[i]))
            backpressure_output_vset <= 1;
      end
      if (|(cf_active & cf_plain & before_bp_vset) || !quiet) begin
        backpressure_released <= 0;
        backpressure_output_vset <= 0;
      end
      if (quiet && !decode_can_accept && cf_valid == 8'hff) begin
        if (backpressure_count != 3) backpressure_count <= backpressure_count + 1'b1;
        if (enq_fire && enq_has_vset) backpressure_vset <= 1;
      end else if (decode_can_accept || !quiet) begin
        backpressure_count <= 0;
        backpressure_vset <= 0;
      end
      if (commit_vsetvl) begin
        arch_type <= vsetvl_type;
        prior_vsetvl_type_valid <= 0;
      end else if (commit_valid) arch_type <= commit_type;
      if (have_vl && !commit_vsetvl && quiet) begin
        if (prior_vsetvl_type_valid && vsetvl_type != prior_vsetvl_type &&
            prior_vsetvl_type == last_output_type) begin
          followup_pending[F_PAYLOAD_WITHOUT_COMMIT] <= 1;
          followup_type[F_PAYLOAD_WITHOUT_COMMIT] <= vsetvl_type;
          followup_other[F_PAYLOAD_WITHOUT_COMMIT] <= prior_vsetvl_type;
        end
        prior_vsetvl_type <= vsetvl_type;
        prior_vsetvl_type_valid <= 1;
      end

      if (have_vl && enq_fire && enq_has_instruction && !enq_has_vset)
        ordinary_queued_after_vl <= 1;
      if (redirect_seen && resuming_vtype && enq_fire && enq_has_vset)
        newpath_vset_enqueued <= 1;
      if (walk_valid && resuming_vtype && !walk_to_arch && !commit_valid &&
          !commit_vsetvl && have_a && walk_type != last_output_type) begin
        walk_recovery_pending <= 1;
        arch_recovery_pending <= 0;
        recovery_type <= walk_type;
      end
      if (walk_to_arch && resuming_vtype && !walk_valid && !commit_valid &&
          !commit_vsetvl && have_a && arch_type != last_output_type) begin
        arch_recovery_pending <= 1;
        walk_recovery_pending <= 0;
        recovery_type <= arch_type;
        recovery_old_type <= last_output_type;
      end
      if (commit_valid && walk_valid && resuming_vtype && !redirect && !commit_vsetvl &&
          !walk_to_arch && have_a && last_output_type != commit_type &&
          last_output_type != walk_type) begin
        if (commit_type == walk_type) begin
          followup_pending[F_COMMIT_WALK_EQUAL] <= 1;
          followup_type[F_COMMIT_WALK_EQUAL] <= walk_type;
        end else begin
          followup_pending[F_COMMIT_WALK_DIFFERENT] <= 1;
          followup_type[F_COMMIT_WALK_DIFFERENT] <= walk_type;
        end
      end
      if (commit_valid && !commit_vsetvl && !redirect && !resuming_vtype &&
          !walk_valid && !walk_to_arch && last_vset_kind != 0) begin
        for (int i = 0; i < 8; i++) begin
          if (cf_valid[i] && decode_can_accept && ordinary_vset(cf_instr[i]) &&
              commit_type == last_output_type &&
              {1'b0, cf_instr[i][27:20]} != commit_type) begin
            followup_pending[F_COMMIT_WITH_NEW_VSET] <= 1;
            followup_type[F_COMMIT_WITH_NEW_VSET] <= commit_type;
            followup_other[F_COMMIT_WITH_NEW_VSET] <= {1'b0, cf_instr[i][27:20]};
            pair_pending_kind <= {last_vset_kind == 2, is_ivli(cf_instr[i])};
          end
        end
      end
      if (have_vl && commit_vsetvl && commit_valid && !walk_to_arch &&
          commit_type == last_output_type) begin
        if (redirect_walk_pending && newpath_vset_enqueued && !walk_valid &&
            resuming_vtype && commit_type == last_output_type &&
            vsetvl_type != commit_type) begin
          followup_pending[F_REDIRECT_WALK_COMMIT] <= 1;
          followup_type[F_REDIRECT_WALK_COMMIT] <= vsetvl_type;
          followup_other[F_REDIRECT_WALK_COMMIT] <= commit_type;
        end
        if (redirect_arch_walk_pending && newpath_vset_enqueued &&
            !walk_valid && resuming_vtype && commit_type == last_output_type &&
            vsetvl_type != commit_type) begin
          followup_pending[F_REDIRECT_ARCH_WALK_COMMIT] <= 1;
          followup_type[F_REDIRECT_ARCH_WALK_COMMIT] <= vsetvl_type;
          followup_other[F_REDIRECT_ARCH_WALK_COMMIT] <= commit_type;
        end
        if (!walk_valid) begin
          if (resuming_vtype && vsetvl_type == commit_type) begin
            followup_pending[F_VSETVL_COMMIT_EQUAL_RESUMING] <= 1; followup_type[F_VSETVL_COMMIT_EQUAL_RESUMING] <= vsetvl_type;
          end
          if (resuming_vtype && vsetvl_type != commit_type) begin
            followup_pending[F_VSETVL_COMMIT_DIFFERENT_RESUMING] <= 1; followup_type[F_VSETVL_COMMIT_DIFFERENT_RESUMING] <= vsetvl_type;
          end
          if (!resuming_vtype && !redirect && vsetvl_type != commit_type) begin
            followup_pending[F_VSETVL_COMMIT_DIFFERENT_RUNNING] <= 1; followup_type[F_VSETVL_COMMIT_DIFFERENT_RUNNING] <= vsetvl_type;
          end
        end else if (resuming_vtype) begin
          if (vsetvl_type != walk_type && vsetvl_type != commit_type &&
              walk_type != commit_type) begin
            followup_pending[F_VSETVL_COMMIT_WALK_DIFFERENT] <= 1; followup_type[F_VSETVL_COMMIT_WALK_DIFFERENT] <= vsetvl_type;
          end
          if (vsetvl_type == walk_type && vsetvl_type != commit_type) begin
            followup_pending[F_VSETVL_COMMIT_WALK_EQUAL] <= 1; followup_type[F_VSETVL_COMMIT_WALK_EQUAL] <= vsetvl_type;
          end
        end
      end

      if (redirect) begin
        redirect_seen <= 1;
        newpath_vset_enqueued <= 0;
        followup_pending <= 0;
        redirect_walk_pending <= have_vl && walk_valid && !walk_to_arch &&
          !walk_after_arch && walk_type == last_output_type;
        redirect_arch_walk_pending <= have_vl && walk_after_arch && walk_valid &&
          walk_type == last_output_type;
        ordinary_queued_after_vl <= 0;
        prior_vsetvl_type_valid <= 0;
        walk_recovery_pending <= walk_valid && resuming_vtype &&
          !walk_to_arch && !commit_valid && !commit_vsetvl && have_a &&
          walk_type != last_output_type;
        arch_recovery_pending <= walk_to_arch && !walk_valid && resuming_vtype &&
          !commit_valid && !commit_vsetvl && have_a &&
          arch_type != last_output_type;
      end
      if (resuming_vtype && walk_to_arch && !walk_valid) walk_after_arch <= 1;
      if (!resuming_vtype) walk_after_arch <= 0;
      if (commit_vsetvl || (!resuming_vtype && redirect_seen)) begin
        redirect_seen <= 0;
        newpath_vset_enqueued <= 0;
        walk_after_arch <= 0;
        redirect_walk_pending <= 0;
        redirect_arch_walk_pending <= 0;
      end
      if (quiet && decode_can_accept && |cf_valid) begin
        have_a <= seen_a;
        have_prior_plain <= prior_plain;
        have_vset_middle <= middle_vset;
        have_vl_middle <= middle_vl;
        have_imm_before_vl <= imm_before_vl;
        have_vl_before_imm <= vl_before_imm;
        have_illegal <= illegal_before;
        have_keep_base <= keep_base;
        have_vl <= vl_seen;
        cfg_a <= first_cfg;
        cfg_prev <= prev_cfg;
        last_output_type <= last_output_type_next;
        last_vset_kind <= last_vset_kind_next;
        vset_count <= vset_count_next;
        reserved_followup <= reserved_followup_next;
        unsupported_followup <= unsupported_followup_next;
        legal_after_illegal <= legal_after_illegal_next;
        if ((walk_recovery_pending &&
             output_vset_except(recovery_type, '0, 1'b0)) ||
            (arch_recovery_pending &&
             output_vset_except(recovery_type, recovery_old_type, 1'b1)) ||
            followup_completed()) begin
          walk_recovery_pending <= 0;
          arch_recovery_pending <= 0;
          followup_pending <= 0;
        end
      end
      if (redirect || commit_vsetvl || walk_valid || walk_to_arch) begin
        have_a <= 0; have_prior_plain <= 0; have_vset_middle <= 0;
        have_vl_middle <= 0;
        have_imm_before_vl <= 0; have_vl_before_imm <= 0;
        have_illegal <= 0; have_keep_base <= 0;
        vset_count <= 0; reserved_followup <= 0;
        unsupported_followup <= 0; legal_after_illegal <= 0;
        if (commit_vsetvl) have_vl <= 0;
      end
    end
  end

  covergroup frontend_vtype_cg @(posedge clock);
    option.per_instance = 1;
    VTYPE_legal_vsetvli_cp:
      coverpoint (|(cf_active & cf_vli & cf_legal & ~cf_keep_vl & before_a))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_legal_vsetivli_cp:
      coverpoint (|(cf_active & cf_ivli & cf_legal & before_a))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvl_before_commit_cp:
      coverpoint (|(cf_active & cf_vl & before_a))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_plain_imm_plain_cp:
      coverpoint (|(cf_active & cf_plain & before_plain & before_middle_vset))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_plain_vsetvl_plain_cp:
      coverpoint (|(cf_active & cf_plain & before_plain & before_middle_vl))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_multiple_imm_vsets_cp:
      coverpoint (|(cf_active & cf_ordinary & before_multiple_vsets & cfg_diff_prev))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_imm_then_vsetvl_cp:
      coverpoint (|(cf_active & cf_vl & before_a & before_imm_vl))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvl_then_imm_cp:
      coverpoint (|(cf_active & cf_ordinary & before_vl_imm))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_full_backpressure_vset_cp:
      coverpoint (|(cf_active & cf_plain & before_bp_vset))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvli_reserved_cp:
      coverpoint (|(cf_active & cf_plain & before_reserved_vli))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetivli_reserved_cp:
      coverpoint (|(cf_active & cf_plain & before_reserved_ivli))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_unsupported_sew_lmul_cp:
      coverpoint (|(cf_active & cf_plain & before_unsupported))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_keep_vl_same_vlmax_cp:
      coverpoint (|(cf_active & cf_keep_vl & cf_legal & before_keep_base & vlmax_changed &
        vlmax_nonzero &
        vlmax_matches_prev))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_keep_vl_changed_vlmax_cp:
      coverpoint (|(cf_active & cf_keep_vl & cf_legal & before_keep_base & vlmax_changed &
        vlmax_nonzero &
        vlmax_matches_first))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_legal_after_illegal_cp:
      coverpoint (|(cf_active & cf_plain & before_legal_after_illegal))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_fetch_fault_vset_cp:
      coverpoint (|(cf_active & cf_legal & cf_fault & before_a))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_resuming_assert_cp:
      coverpoint (!redirect && !walk_valid && !walk_to_arch &&
        !commit_valid && !commit_vsetvl && resume_prev && resuming_vtype &&
        resume_rise_with_queue && queued != 0 && dequeued == 0)
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_resuming_release_cp:
      coverpoint (!redirect && !walk_valid && !walk_to_arch &&
        !commit_valid && !commit_vsetvl && resume_prev && !resuming_vtype &&
        quiet_resume_count >= 2 && queued != 0 && decode_can_accept)
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_walk_restore_cp:
      coverpoint (walk_recovery_pending &&
        output_vset_except(recovery_type, '0, 1'b0))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_walk_to_arch_restore_cp:
      coverpoint (arch_recovery_pending &&
        output_vset_except(recovery_type, recovery_old_type, 1'b1))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_commit_with_walk_to_arch_cp:
      coverpoint (walk_to_arch && resuming_vtype && !walk_valid && commit_valid &&
        !commit_vsetvl && arch_type != commit_type &&
        last_output_type != arch_type && last_output_type != commit_type)
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvl_redirect_without_feedback_cp:
      coverpoint (redirect && vl_seen && !resuming_vtype && !walk_valid &&
        !walk_to_arch && !commit_valid && !commit_vsetvl && queued != 0 &&
        ordinary_queued_after_vl)
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvl_redirect_walk_commit_cp:
      coverpoint (followup_pending[F_REDIRECT_WALK_COMMIT] &&
        output_vset_except(followup_type[F_REDIRECT_WALK_COMMIT], followup_other[F_REDIRECT_WALK_COMMIT], 1'b1))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvl_redirect_arch_walk_commit_cp:
      coverpoint (followup_pending[F_REDIRECT_ARCH_WALK_COMMIT] &&
        output_vset_except(followup_type[F_REDIRECT_ARCH_WALK_COMMIT], followup_other[F_REDIRECT_ARCH_WALK_COMMIT], 1'b1))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_commit_with_new_vset_cp:
      coverpoint pair_sample iff (!reset && followup_pending[F_COMMIT_WITH_NEW_VSET] &&
        output_vset_except(followup_type[F_COMMIT_WITH_NEW_VSET], followup_other[F_COMMIT_WITH_NEW_VSET], 1'b1)) {
        bins vli_commit_vli_output = {1};
        bins vli_commit_ivli_output = {2};
        bins ivli_commit_vli_output = {3};
        bins ivli_commit_ivli_output = {4};
      }
    VTYPE_commit_walk_equal_cp:
      coverpoint (followup_pending[F_COMMIT_WALK_EQUAL] &&
        output_vset_except(followup_type[F_COMMIT_WALK_EQUAL], '0, 1'b0))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_commit_walk_different_cp:
      coverpoint (followup_pending[F_COMMIT_WALK_DIFFERENT] &&
        output_vset_except(followup_type[F_COMMIT_WALK_DIFFERENT], '0, 1'b0))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvl_payload_without_commit_cp:
      coverpoint (followup_pending[F_PAYLOAD_WITHOUT_COMMIT] &&
        output_vset_except(followup_type[F_PAYLOAD_WITHOUT_COMMIT], followup_other[F_PAYLOAD_WITHOUT_COMMIT], 1'b1))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvl_commit_equal_resuming_cp:
      coverpoint (followup_pending[F_VSETVL_COMMIT_EQUAL_RESUMING] &&
        output_vset_except(followup_type[F_VSETVL_COMMIT_EQUAL_RESUMING], '0, 1'b0))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvl_commit_different_resuming_cp:
      coverpoint (followup_pending[F_VSETVL_COMMIT_DIFFERENT_RESUMING] &&
        output_vset_except(followup_type[F_VSETVL_COMMIT_DIFFERENT_RESUMING], '0, 1'b0))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvl_commit_different_running_cp:
      coverpoint (followup_pending[F_VSETVL_COMMIT_DIFFERENT_RUNNING] &&
        output_vset_except(followup_type[F_VSETVL_COMMIT_DIFFERENT_RUNNING], '0, 1'b0))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvl_commit_walk_different_cp:
      coverpoint (followup_pending[F_VSETVL_COMMIT_WALK_DIFFERENT] &&
        output_vset_except(followup_type[F_VSETVL_COMMIT_WALK_DIFFERENT], '0, 1'b0))
        iff (!reset) { bins observed = {1'b1}; }
    VTYPE_vsetvl_commit_walk_equal_cp:
      coverpoint (followup_pending[F_VSETVL_COMMIT_WALK_EQUAL] &&
        output_vset_except(followup_type[F_VSETVL_COMMIT_WALK_EQUAL], '0, 1'b0))
        iff (!reset) { bins observed = {1'b1}; }
  endgroup
  frontend_vtype_cg cg = new();
endmodule
