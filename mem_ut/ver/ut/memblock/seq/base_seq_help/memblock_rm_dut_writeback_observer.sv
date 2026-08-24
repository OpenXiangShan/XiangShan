//=========================================================
//File name    : memblock_rm_dut_writeback_observer.sv
//Module name  : memblock_rm_dut_writeback_observer
//Discribution : RM-only passive DUT integer writeback history
//Date         : 2026-08-20
//=========================================================
`ifndef MEMBLOCK_RM_DUT_WRITEBACK_OBSERVER__SV
`define MEMBLOCK_RM_DUT_WRITEBACK_OBSERVER__SV

// 这是 DUT writeback 的值型副本，不复用 raw event，也不写入现有 monitor/status 链。
// 所有 split integer-WB lane 共用一条历史队列；source_kind/lane 保留原始来源。
typedef struct packed {
    bit                  valid;
    bit [1:0]            source_kind;
    bit [2:0]            lane;
    bit                  rob_valid;
    bit                  rob_flag_valid;
    logic                rob_flag;
    bit                  rob_value_only_without_flag;
    logic [MEMBLOCK_ROB_VALUE_W-1:0] rob_value;
    bit                  data_valid;
    logic [63:0]         data;
    bit                  exception_valid;
    logic [23:0]         exception_vec;
    bit                  trigger_valid;
    logic [3:0]          trigger;
    bit                  flush_pipe_valid;
    logic                flush_pipe;
    bit                  replay_inst_valid;
    logic                replay_inst;
    bit                  pdest_valid;
    logic [7:0]          pdest;
    bit                  rf_wen_valid;
    logic                rf_wen;
    bit                  fp_wen_valid;
    logic                fp_wen;
    bit                  is_from_load_unit_valid;
    logic                is_from_load_unit;
    bit                  debug_valid;
    logic                debug_is_mmio;
    logic                debug_is_ncio;
    logic                debug_is_perf_cnt;
    int unsigned         sample_flush_epoch;
    longint unsigned     sample_cycle;
} memblock_rm_dut_writeback_record_t;

class memblock_rm_dut_writeback_observer extends uvm_object;
    localparam bit [1:0] RM_WB_SOURCE_LDA = 2'd1;
    localparam bit [1:0] RM_WB_SOURCE_STA = 2'd2;
    localparam bit [1:0] RM_WB_SOURCE_STD = 2'd3;

    typedef memblock_rm_dut_writeback_record_t writeback_record_t;
    typedef writeback_record_t writeback_record_q_t[$];

    virtual io_mem_to_ooo_int_wb_agent_agent_interface.mon_mp vif;
    bit vif_bound;
    bit running;
    bit previous_reset_backend_done;
    writeback_record_q_t writeback_history_q;
    longint unsigned writeback_count;
    longint unsigned unknown_valid_count;

    `uvm_object_utils(memblock_rm_dut_writeback_observer)

    function new(string name = "memblock_rm_dut_writeback_observer");
        super.new(name);
        vif_bound = 1'b0;
        running = 1'b0;
        previous_reset_backend_done = 1'b0;
        clear_history();
    endfunction:new

    function void bind_vif(
        virtual io_mem_to_ooo_int_wb_agent_agent_interface.mon_mp vif_i
    );
        if (vif_bound) begin
            return;
        end
        vif = vif_i;
        vif_bound = vif_i != null;
    endfunction:bind_vif

    function bit is_bound();
        return vif_bound && vif != null;
    endfunction:is_bound

    function void clear_history();
        writeback_history_q.delete();
        writeback_count = 0;
        unknown_valid_count = 0;
    endfunction:clear_history

    // Only a valid=1 lane is copied.  replay/flush records are intentionally
    // retained; the RM filters them when selecting a comparable Load record.
    function bit sample_lane(input bit [1:0] source_kind,
                             input int unsigned lane,
                             output writeback_record_t record);
        logic sampled_valid;

        record = '{default:'0};
        sampled_valid = 1'b0;
        case (source_kind)
            RM_WB_SOURCE_LDA: begin
                case (lane)
                    0: sampled_valid = vif.mon_cb.io_mem_to_ooo_writebackLda_0_valid;
                    1: sampled_valid = vif.mon_cb.io_mem_to_ooo_writebackLda_1_valid;
                    2: sampled_valid = vif.mon_cb.io_mem_to_ooo_writebackLda_2_valid;
                    default: return 1'b0;
                endcase
            end
            RM_WB_SOURCE_STA: begin
                case (lane)
                    0: sampled_valid = vif.mon_cb.io_mem_to_ooo_writebackSta_0_valid;
                    1: sampled_valid = vif.mon_cb.io_mem_to_ooo_writebackSta_1_valid;
                    default: return 1'b0;
                endcase
            end
            RM_WB_SOURCE_STD: begin
                case (lane)
                    0: sampled_valid = vif.mon_cb.io_mem_to_ooo_writebackStd_0_valid;
                    1: sampled_valid = vif.mon_cb.io_mem_to_ooo_writebackStd_1_valid;
                    default: return 1'b0;
                endcase
            end
            default: return 1'b0;
        endcase
        if (sampled_valid === 1'b0) begin
            return 1'b0;
        end
        if (sampled_valid !== 1'b1) begin
            unknown_valid_count++;
            return 1'b0;
        end

        record.valid = 1'b1;
        record.source_kind = source_kind;
        record.lane = lane[2:0];
        record.sample_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
        record.sample_cycle = $time;
        case (source_kind)
            RM_WB_SOURCE_LDA: begin
                record.rob_valid = 1'b1;
                record.rob_flag_valid = 1'b1;
                record.data_valid = 1'b1;
                record.exception_valid = 1'b1;
                record.trigger_valid = 1'b1;
                record.flush_pipe_valid = 1'b1;
                record.replay_inst_valid = 1'b1;
                record.pdest_valid = 1'b1;
                record.rf_wen_valid = 1'b1;
                record.fp_wen_valid = 1'b1;
                record.debug_valid = 1'b1;
                case (lane)
                    0: begin
                        record.rob_flag = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_flag;
                        record.rob_value = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value;
                        record.data = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_data;
                        record.trigger = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_trigger;
                        record.flush_pipe = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe;
                        record.replay_inst = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_replayInst;
                        record.pdest = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_pdest;
                        record.rf_wen = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_rfWen;
                        record.fp_wen = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_fpWen;
                        record.is_from_load_unit_valid = 1'b1;
                        record.is_from_load_unit = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_isFromLoadUnit;
                        record.debug_is_mmio = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO;
                        record.debug_is_ncio = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO;
                        record.debug_is_perf_cnt = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt;
                        record.exception_vec[3] = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3;
                        record.exception_vec[4] = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_4;
                        record.exception_vec[5] = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_5;
                        record.exception_vec[6] = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_6;
                        record.exception_vec[7] = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_7;
                        record.exception_vec[13] = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13;
                        record.exception_vec[15] = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_15;
                        record.exception_vec[19] = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_19;
                        record.exception_vec[21] = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_21;
                        record.exception_vec[23] = vif.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_23;
                    end
                    1: begin
                        record.rob_flag = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_flag;
                        record.rob_value = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_value;
                        record.data = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_data;
                        record.trigger = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_trigger;
                        record.flush_pipe = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe;
                        record.replay_inst = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_replayInst;
                        record.pdest = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_pdest;
                        record.rf_wen = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_rfWen;
                        record.fp_wen = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_fpWen;
                        record.debug_is_mmio = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO;
                        record.debug_is_ncio = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO;
                        record.debug_is_perf_cnt = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt;
                        record.exception_vec[3] = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3;
                        record.exception_vec[4] = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_4;
                        record.exception_vec[5] = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_5;
                        record.exception_vec[13] = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_13;
                        record.exception_vec[19] = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_19;
                        record.exception_vec[21] = vif.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_21;
                    end
                    2: begin
                        record.rob_flag = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_flag;
                        record.rob_value = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_value;
                        record.data = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_data;
                        record.trigger = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_trigger;
                        record.flush_pipe = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe;
                        record.replay_inst = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_replayInst;
                        record.pdest = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_pdest;
                        record.rf_wen = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_rfWen;
                        record.fp_wen = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_fpWen;
                        record.debug_is_mmio = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO;
                        record.debug_is_ncio = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO;
                        record.debug_is_perf_cnt = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt;
                        record.exception_vec[3] = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3;
                        record.exception_vec[4] = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_4;
                        record.exception_vec[5] = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_5;
                        record.exception_vec[13] = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_13;
                        record.exception_vec[19] = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_19;
                        record.exception_vec[21] = vif.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_21;
                    end
                endcase
            end
            RM_WB_SOURCE_STA: begin
                record.rob_valid = 1'b1;
                record.rob_flag_valid = 1'b1;
                record.exception_valid = 1'b1;
                record.trigger_valid = 1'b1;
                record.debug_valid = 1'b1;
                case (lane)
                    0: begin
                        record.rob_flag = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag;
                        record.rob_value = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value;
                        record.trigger = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_trigger;
                        record.flush_pipe_valid = 1'b1;
                        record.flush_pipe = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe;
                        record.debug_is_mmio = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO;
                        record.debug_is_ncio = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO;
                        record.exception_vec[0] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0;
                        record.exception_vec[1] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1;
                        record.exception_vec[2] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2;
                        record.exception_vec[3] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3;
                        record.exception_vec[4] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4;
                        record.exception_vec[5] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5;
                        record.exception_vec[6] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6;
                        record.exception_vec[7] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7;
                        record.exception_vec[8] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8;
                        record.exception_vec[9] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9;
                        record.exception_vec[10] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10;
                        record.exception_vec[11] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11;
                        record.exception_vec[12] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12;
                        record.exception_vec[13] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13;
                        record.exception_vec[14] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14;
                        record.exception_vec[15] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15;
                        record.exception_vec[16] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16;
                        record.exception_vec[17] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17;
                        record.exception_vec[18] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18;
                        record.exception_vec[19] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19;
                        record.exception_vec[20] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20;
                        record.exception_vec[21] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21;
                        record.exception_vec[22] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22;
                        record.exception_vec[23] = vif.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23;
                    end
                    1: begin
                        record.rob_flag = vif.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag;
                        record.rob_value = vif.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value;
                        record.trigger = vif.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_trigger;
                        record.debug_is_mmio = vif.mon_cb.io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO;
                        record.debug_is_ncio = vif.mon_cb.io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO;
                        record.exception_vec[3] = vif.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3;
                        record.exception_vec[6] = vif.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_6;
                        record.exception_vec[7] = vif.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_7;
                        record.exception_vec[15] = vif.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_15;
                        record.exception_vec[19] = vif.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_19;
                        record.exception_vec[23] = vif.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_23;
                    end
                endcase
            end
            RM_WB_SOURCE_STD: begin
                record.rob_valid = 1'b1;
                record.rob_value_only_without_flag = 1'b1;
                if (lane == 0) begin
                    record.rob_value = vif.mon_cb.io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value;
                end else begin
                    record.rob_value = vif.mon_cb.io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value;
                end
            end
        endcase
        return 1'b1;
    endfunction:sample_lane

    function void sample_and_enqueue(input bit [1:0] source_kind,
                                     input int unsigned lane);
        writeback_record_t record;
        if (sample_lane(source_kind, lane, record)) begin
            writeback_history_q.push_back(record);
            writeback_count++;
        end
    endfunction:sample_and_enqueue

    task run();
        if (!is_bound()) begin
            `uvm_error("RM_DUT_WB_PROBE", "integer-WB monitor VIF is not bound")
            return;
        end
        if (running) begin
            return;
        end
        running = 1'b1;
        forever begin
            @(vif.mon_cb);
            if (!memblock_sync_pkg::reset_backend_done) begin
                if (previous_reset_backend_done) begin
                    clear_history();
                end
                previous_reset_backend_done = 1'b0;
                continue;
            end
            previous_reset_backend_done = 1'b1;
            sample_and_enqueue(RM_WB_SOURCE_LDA, 0);
            sample_and_enqueue(RM_WB_SOURCE_LDA, 1);
            sample_and_enqueue(RM_WB_SOURCE_LDA, 2);
            sample_and_enqueue(RM_WB_SOURCE_STA, 0);
            sample_and_enqueue(RM_WB_SOURCE_STA, 1);
            sample_and_enqueue(RM_WB_SOURCE_STD, 0);
            sample_and_enqueue(RM_WB_SOURCE_STD, 1);
        end
    endtask:run

    // RM comparison performs the only selection: latest non-replay/non-flush
    // LDA record with the requested complete ROB key.
    function bit read_latest_load_by_rob(
        input memblock_rob_key_t rob_key,
        output writeback_record_t record
    );
        int index;
        record = '{default:'0};
        for (index = int'(writeback_history_q.size()) - 1; index >= 0; index--) begin
            if (writeback_history_q[index].valid &&
                writeback_history_q[index].source_kind == RM_WB_SOURCE_LDA &&
                writeback_history_q[index].rob_valid &&
                writeback_history_q[index].rob_flag_valid &&
                writeback_history_q[index].rob_flag == rob_key.flag &&
                writeback_history_q[index].rob_value == rob_key.value &&
                !$isunknown({writeback_history_q[index].rob_flag,
                             writeback_history_q[index].rob_value,
                             writeback_history_q[index].data,
                             writeback_history_q[index].exception_vec}) &&
                writeback_history_q[index].replay_inst_valid &&
                writeback_history_q[index].replay_inst === 1'b0 &&
                writeback_history_q[index].flush_pipe_valid &&
                writeback_history_q[index].flush_pipe === 1'b0) begin
                record = writeback_history_q[index];
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:read_latest_load_by_rob

endclass:memblock_rm_dut_writeback_observer

`endif // MEMBLOCK_RM_DUT_WRITEBACK_OBSERVER__SV
