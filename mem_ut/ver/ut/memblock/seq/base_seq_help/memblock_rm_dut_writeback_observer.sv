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
    bit                  owner_valid;
    memblock_uid_t       owner_uid;
    int unsigned         owner_dynamic_epoch;
    int unsigned         owner_issue_epoch;
    int unsigned         sample_flush_epoch;
    longint unsigned     sample_seq;
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
    // STA0按ROB key索引，CMO查询不逐次扫描所有Load/Store写回历史。
    writeback_record_q_t load_history_by_rob[bit [MEMBLOCK_ROB_VALUE_W:0]];
    writeback_record_q_t sta0_history_by_rob[bit [MEMBLOCK_ROB_VALUE_W:0]];
    // 已被RM唯一消费的CMO终态摘要，仅按物理ROB key索引；复用时由live owner优先替换。
    typedef struct packed {
        bit valid;
        writeback_record_t completion;
        memblock_sq_key_t sq;
        int unsigned sta_issue_epoch;
        int unsigned replay_seq;
        int unsigned instance_flush_epoch;
        bit [8:0] fu_op_type;
        bit late_normal_seen;
    } cmo_terminal_audit_t;
    cmo_terminal_audit_t cmo_terminal_by_rob[bit [MEMBLOCK_ROB_VALUE_W:0]];
    int unsigned cmo_late_error_count;
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
        sta0_history_by_rob.delete();
        load_history_by_rob.delete();
        cmo_terminal_by_rob.delete(); cmo_late_error_count=0;
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
            // 中文注释：X/Z valid 仍按既有语义不构造 RM 写回记录；仅在公共硬
            // 诊断开启时上报，避免默认配置把未知值升级为测试终止条件。
            if (memblock_sync_pkg::is_hard_xz_check_en()) begin
                memblock_sync_pkg::report_hard_xz_error(
                    get_type_name(),
                    $sformatf("RM writeback valid is X/Z: source_kind=%0d lane=%0d",
                              source_kind, lane));
            end
            return 1'b0;
        end

        record.valid = 1'b1;
        record.source_kind = source_kind;
        record.lane = lane[2:0];
        record.sample_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
        record.sample_seq = memblock_sync_pkg::peek_current_dut_global_sample();
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

    virtual function void report_cmo_audit_error(input string detail);
        cmo_late_error_count++;
        `uvm_error("RM_CMO_LATE_COMPLETION",detail)
    endfunction

    // 抽象职责：仅RM成功消费后冻结终态身份；后续写回增量检查不再依赖程序cursor重访该UID。
    function bit mark_cmo_completion_consumed(input writeback_record_t completion);
        common_data_transaction data;
        main_control_transaction main_tr;
        status_transaction status_v;
        cmo_terminal_audit_t terminal_v;
        bit [MEMBLOCK_ROB_VALUE_W:0] key;
        int unsigned instance_flush;
        data=common_data_transaction::m_inst;
        if(!completion.valid || !completion.owner_valid || completion.source_kind!=RM_WB_SOURCE_STA ||
           completion.lane!=0 || !completion.exception_valid || !completion.rob_flag_valid || data==null ||
           !data.is_valid_uid(completion.owner_uid)) return 0;
        status_v=data.get_status(completion.owner_uid); main_tr=data.get_main_transaction(completion.owner_uid);
        if(status_v==null || main_tr==null || main_tr.op_class!=MEMBLOCK_OP_CLASS_CBO ||
           status_v.dynamic_epoch!=completion.owner_dynamic_epoch ||
           status_v.get_rob_key()!=memblock_rob_key_t'{flag:completion.rob_flag,value:completion.rob_value}) return 0;
        if(!status_v.get_target_instance_flush_epoch(MEMBLOCK_ISSUE_TARGET_STA,instance_flush)) begin
            if(!status_v.active_instance_flush_epoch_valid) return 0;
            instance_flush=status_v.active_instance_flush_epoch;
        end
        key={completion.rob_flag,completion.rob_value};
        if(cmo_terminal_by_rob.exists(key) &&
           cmo_terminal_by_rob[key].completion.owner_uid==completion.owner_uid &&
           cmo_terminal_by_rob[key].completion.owner_dynamic_epoch==completion.owner_dynamic_epoch) return 0;
        terminal_v='0; terminal_v.valid=1; terminal_v.completion=completion;
        terminal_v.sq='{flag:status_v.sqIdx_flag,value:status_v.sqIdx_value}; terminal_v.sta_issue_epoch=status_v.sta_issue_epoch;
        terminal_v.replay_seq=status_v.replay_seq; terminal_v.instance_flush_epoch=instance_flush;
        terminal_v.fu_op_type=main_tr.fuOpType;
        cmo_terminal_by_rob[key]=terminal_v;
        // 注册只发生一次：补查该ROB在RM排到此UID之前已采到的尾拍，避免程序cursor较慢漏审。
        if(sta0_history_by_rob.exists(key)) begin
            foreach(sta0_history_by_rob[key][i])
                if(sta0_history_by_rob[key][i].sample_seq>completion.sample_seq)
                    audit_cmo_sta0(sta0_history_by_rob[key][i]);
        end
        return 1;
    endfunction

    // 抽象职责：对已消费CMO后的真实STA0逐事件检查。fault后的唯一normal尾拍不算第二次架构完成，
    // 但必须重用框架已有的完整终态provenance，第二fault/第二normal均报错。
    function void audit_cmo_sta0(input writeback_record_t record);
        common_data_transaction data;
        cmo_terminal_audit_t terminal_v;
        status_transaction status_v;
        main_control_transaction main_tr;
        memblock_uid_t live_uid;
        memblock_rob_key_t rob_key;
        bit [MEMBLOCK_ROB_VALUE_W:0] key;
        int unsigned instance_flush;
        if(!record.valid || record.source_kind!=RM_WB_SOURCE_STA || record.lane!=0 ||
           !record.rob_flag_valid) return;
        key={record.rob_flag,record.rob_value};
        if(!cmo_terminal_by_rob.exists(key)) return;
        terminal_v=cmo_terminal_by_rob[key];
        if(record.sample_seq<=terminal_v.completion.sample_seq) return;
        data=common_data_transaction::m_inst;
        rob_key='{flag:record.rob_flag,value:record.rob_value};
        if(record.owner_valid && (record.owner_uid!=terminal_v.completion.owner_uid ||
           record.owner_dynamic_epoch!=terminal_v.completion.owner_dynamic_epoch)) begin
            cmo_terminal_by_rob.delete(key); return;
        end
        if(data!=null && data.lookup_active_uid_by_rob(rob_key,live_uid)) begin
            status_v=data.get_status(live_uid);
            if(live_uid!=terminal_v.completion.owner_uid || status_v==null ||
               status_v.dynamic_epoch!=terminal_v.completion.owner_dynamic_epoch) begin
                // 真实ROB复用已明确新owner；新动态实例优先，不误判旧终态重复。
                cmo_terminal_by_rob.delete(key); return;
            end
        end
        if(terminal_v.completion.exception_vec!=0 && record.exception_valid &&
           record.exception_vec===24'b0 && !terminal_v.late_normal_seen && data!=null &&
           !data.lookup_active_uid_by_rob(rob_key,live_uid) &&
           data.is_valid_uid(terminal_v.completion.owner_uid)) begin
            status_v=data.get_status(terminal_v.completion.owner_uid);
            main_tr=data.get_main_transaction(terminal_v.completion.owner_uid);
            if(status_v!=null && main_tr!=null && !status_v.active && status_v.terminal_done &&
               status_v.fault && status_v.sta_fault && status_v.sta_writeback &&
               status_v.dynamic_epoch==terminal_v.completion.owner_dynamic_epoch &&
               status_v.get_rob_key()==rob_key && status_v.sqIdx_flag==terminal_v.sq.flag &&
               status_v.sqIdx_value==terminal_v.sq.value &&
               status_v.sta_issue_epoch==terminal_v.sta_issue_epoch && status_v.replay_seq==terminal_v.replay_seq &&
               status_v.get_target_instance_flush_epoch(MEMBLOCK_ISSUE_TARGET_STA,instance_flush) &&
               instance_flush==terminal_v.instance_flush_epoch && main_tr.op_class==MEMBLOCK_OP_CLASS_CBO &&
               main_tr.fuOpType==terminal_v.fu_op_type && status_v.exception_vec!=0 &&
               record.sample_flush_epoch>=terminal_v.completion.sample_flush_epoch &&
               record.sample_flush_epoch==memblock_sync_pkg::dispatch_flush_epoch) begin
                terminal_v.late_normal_seen=1; cmo_terminal_by_rob[key]=terminal_v; return;
            end
        end
        report_cmo_audit_error($sformatf(
            "已消费CMO收到非法重复STA0 uid=%0d epoch=%0d rob=%0d/%0d terminal=%0d duplicate=%0d exception=0x%0h",
            terminal_v.completion.owner_uid,terminal_v.completion.owner_dynamic_epoch,
            record.rob_flag,record.rob_value,terminal_v.completion.sample_seq,record.sample_seq,record.exception_vec));
    endfunction

    function void sample_and_enqueue(input bit [1:0] source_kind,
                                     input int unsigned lane);
        writeback_record_t record;
        common_data_transaction owner_data;
        memblock_rob_map_key_t owner_key;
        memblock_uid_t owner_uid;
        if (sample_lane(source_kind, lane, record)) begin
            // 采样当拍冻结动态owner，ROB环绕或同flush epoch复用不能冒充旧STA0。
            owner_data=common_data_transaction::m_inst;
            owner_key={record.rob_flag,record.rob_value};
            if(record.rob_valid && record.rob_flag_valid && owner_data!=null &&
               owner_data.uid_by_active_rob.exists(owner_key)) begin
                owner_uid=owner_data.uid_by_active_rob[owner_key];
                if(owner_data.is_valid_uid(owner_uid) && owner_data.status_by_uid[owner_uid]!=null) begin
                    record.owner_valid=1; record.owner_uid=owner_uid;
                    record.owner_dynamic_epoch=owner_data.status_by_uid[owner_uid].dynamic_epoch;
                    if (source_kind == RM_WB_SOURCE_LDA)
                        record.owner_issue_epoch=owner_data.status_by_uid[owner_uid].load_issue_epoch;
                end
            end
            audit_cmo_sta0(record);
            writeback_history_q.push_back(record);
            if (record.source_kind == RM_WB_SOURCE_LDA && record.rob_valid && record.rob_flag_valid)
                load_history_by_rob[{record.rob_flag,record.rob_value}].push_back(record);
            if (record.source_kind == RM_WB_SOURCE_STA && record.lane == 0 &&
                record.rob_valid && record.rob_flag_valid)
                sta0_history_by_rob[{record.rob_flag, record.rob_value}].push_back(record);
            writeback_count++;
        end
    endfunction:sample_and_enqueue

    task run();
        longint unsigned edge_sample;
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
            if (!memblock_sync_pkg::reset_backend_done ||
                memblock_sync_pkg::l2tlb_reset_active()) begin
                if (previous_reset_backend_done) begin
                    clear_history();
                end
                previous_reset_backend_done = 1'b0;
                continue;
            end
            memblock_sync_pkg::wait_for_l2tlb_sample_anchor($time,edge_sample);
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
    // LDA record with the requested ROB key. Payload X values are retained so
    // the RM reports a value mismatch rather than a missing writeback.
    // 抽象职责：返回同ROB及动态flush代际唯一的最终STA0；保留正常nonzero的flushPipe=1。
    function bit read_cmo_sta0_by_rob(input memblock_rob_key_t rob_key,
                                    input int unsigned flush_epoch,
                                    input memblock_uid_t uid,
                                    input int unsigned dynamic_epoch,
                                    output writeback_record_t record,
                                    output bit duplicate);
        bit [MEMBLOCK_ROB_VALUE_W:0] key;
        int unsigned match_count;
        record='0; duplicate=0; match_count=0; key={rob_key.flag,rob_key.value};
        if(!sta0_history_by_rob.exists(key)) return 0;
        foreach(sta0_history_by_rob[key][i]) begin
            if(sta0_history_by_rob[key][i].owner_valid &&
               sta0_history_by_rob[key][i].owner_uid==uid &&
               sta0_history_by_rob[key][i].owner_dynamic_epoch==dynamic_epoch &&
               sta0_history_by_rob[key][i].sample_flush_epoch==flush_epoch) begin
                record=sta0_history_by_rob[key][i]; match_count++;
            end
        end
        duplicate=match_count>1;
        return match_count==1;
    endfunction

    // 抽象职责：按ROB局部历史返回当前执行实例的最终Load写回，隔离旧代和重发。
    function bit read_load_instance(input memblock_uid_t uid, input memblock_rob_key_t rob_key,
        input int unsigned dynamic_epoch, input int unsigned issue_epoch,
        input bit flush_epoch_valid, input int unsigned flush_epoch,
        input longint unsigned minimum_sample, output writeback_record_t record);
        bit [MEMBLOCK_ROB_VALUE_W:0] key;
        record = '0;
        key = {rob_key.flag,rob_key.value};
        if (!load_history_by_rob.exists(key)) return 0;
        for (int index=int'(load_history_by_rob[key].size())-1;index>=0;index--) begin
            writeback_record_t candidate;
            candidate=load_history_by_rob[key][index];
            if (candidate.owner_valid && candidate.owner_uid==uid &&
                candidate.owner_dynamic_epoch==dynamic_epoch && candidate.owner_issue_epoch==issue_epoch &&
                (!flush_epoch_valid || candidate.sample_flush_epoch>=flush_epoch) &&
                candidate.sample_seq>=minimum_sample && candidate.replay_inst_valid &&
                candidate.replay_inst===1'b0 && candidate.flush_pipe_valid && candidate.flush_pipe===1'b0) begin
                record=candidate;
                return 1;
            end
        end
        return 0;
    endfunction:read_load_instance

    // 抽象职责：保留非标量旧consumer的查询合同，扫描仅限同ROB局部历史。
    function bit read_latest_load_by_rob(input memblock_rob_key_t rob_key,
        output writeback_record_t record);
        bit [MEMBLOCK_ROB_VALUE_W:0] key;
        record='0;key={rob_key.flag,rob_key.value};
        if (!load_history_by_rob.exists(key)) return 0;
        for (int index=int'(load_history_by_rob[key].size())-1;index>=0;index--) begin
            if (load_history_by_rob[key][index].replay_inst_valid &&
                load_history_by_rob[key][index].replay_inst===1'b0 &&
                load_history_by_rob[key][index].flush_pipe_valid &&
                load_history_by_rob[key][index].flush_pipe===1'b0) begin
                record=load_history_by_rob[key][index]; return 1;
            end
        end
        return 0;
    endfunction:read_latest_load_by_rob

    function bit read_atomic_loads_by_rob(
        input memblock_rob_key_t rob_key,
        input longint unsigned minimum_sample_seq,
        input int unsigned minimum_flush_epoch,
        output writeback_record_q_t records
    );
        writeback_record_t swap_record;
        records.delete();
        foreach (writeback_history_q[index]) begin
            if (writeback_history_q[index].valid &&
                writeback_history_q[index].source_kind == RM_WB_SOURCE_LDA &&
                writeback_history_q[index].lane == 0 &&
                writeback_history_q[index].rob_valid &&
                writeback_history_q[index].rob_flag_valid &&
                writeback_history_q[index].rob_flag === rob_key.flag &&
                writeback_history_q[index].rob_value === rob_key.value &&
                writeback_history_q[index].sample_seq >= minimum_sample_seq &&
                writeback_history_q[index].sample_flush_epoch == minimum_flush_epoch &&
                writeback_history_q[index].replay_inst_valid &&
                writeback_history_q[index].replay_inst === 1'b0 &&
                writeback_history_q[index].flush_pipe_valid &&
                writeback_history_q[index].flush_pipe === 1'b0) begin
                records.push_back(writeback_history_q[index]);
            end
        end
        for (int unsigned left = 0; left < records.size(); left++) begin
            for (int unsigned right = left + 1; right < records.size(); right++) begin
                if (records[right].sample_seq < records[left].sample_seq) begin
                    swap_record = records[left];
                    records[left] = records[right];
                    records[right] = swap_record;
                end
            end
        end
        return records.size() != 0;
    endfunction:read_atomic_loads_by_rob

endclass:memblock_rm_dut_writeback_observer

`endif // MEMBLOCK_RM_DUT_WRITEBACK_OBSERVER__SV
