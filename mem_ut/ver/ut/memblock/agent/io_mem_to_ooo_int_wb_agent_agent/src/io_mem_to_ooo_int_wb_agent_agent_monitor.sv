//=========================================================
//File name    : io_mem_to_ooo_int_wb_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_int_wb_agent_agent_monitor
//Discribution : io_mem_to_ooo_int_wb_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================

`ifndef IO_MEM_TO_OOO_INT_WB_AGENT_AGENT_MONITOR__SV
`define IO_MEM_TO_OOO_INT_WB_AGENT_AGENT_MONITOR__SV

class io_mem_to_ooo_int_wb_agent_agent_monitor  extends tcnt_monitor_base#(virtual io_mem_to_ooo_int_wb_agent_agent_interface,io_mem_to_ooo_int_wb_agent_agent_cfg,io_mem_to_ooo_int_wb_agent_agent_xaction);

    `uvm_component_utils(io_mem_to_ooo_int_wb_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern function memblock_sync_pkg::dispatch_raw_int_wb_t build_raw_int_wb_from_v2_port(
        input memblock_sync_pkg::memblock_int_wb_source_kind_e source_kind,
        input int unsigned port_id
    );
    extern task mon_data();
endclass:io_mem_to_ooo_int_wb_agent_agent_monitor

function io_mem_to_ooo_int_wb_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void io_mem_to_ooo_int_wb_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task io_mem_to_ooo_int_wb_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

// 中文注释：本 helper 只按 V2 split 顶层端口构造单个 raw event，不写 raw queue。
// source_kind 决定 payload 能力，port_id 只表示 kind 内 lane；不存在的 key/metadata 保持 empty raw 中性值。
function memblock_sync_pkg::dispatch_raw_int_wb_t
io_mem_to_ooo_int_wb_agent_agent_monitor::build_raw_int_wb_from_v2_port(
    input memblock_sync_pkg::memblock_int_wb_source_kind_e source_kind,
    input int unsigned port_id
);
    memblock_sync_pkg::dispatch_raw_int_wb_t raw;
    logic sampled_valid;

    raw = memblock_sync_pkg::make_empty_raw_int_wb();
    sampled_valid = 1'b0;
    case (source_kind)
        memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA: begin
            case (port_id)
                0: sampled_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_valid;
                1: sampled_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_valid;
                2: sampled_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_valid;
                default: `uvm_fatal("INT_WB_MON", $sformatf("invalid SCALAR_LDA port_id=%0d", port_id))
            endcase
        end
        memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA: begin
            case (port_id)
                0: sampled_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_valid;
                1: sampled_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_valid;
                default: `uvm_fatal("INT_WB_MON", $sformatf("invalid STA port_id=%0d", port_id))
            endcase
        end
        memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD: begin
            case (port_id)
                0: sampled_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackStd_0_valid;
                1: sampled_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackStd_1_valid;
                default: `uvm_fatal("INT_WB_MON", $sformatf("invalid STD port_id=%0d", port_id))
            endcase
        end
        default: `uvm_fatal("INT_WB_MON", $sformatf("invalid source_kind=%0d", source_kind))
    endcase

    if (sampled_valid === 1'b0) begin
        return raw;
    end
    if (sampled_valid !== 1'b1) begin
        `uvm_fatal("INT_WB_MON",
                   $sformatf("writeback valid is X/Z: source_kind=%0d port_id=%0d", source_kind, port_id))
    end

    raw.valid = 1'b1;
    raw.source_kind = source_kind;
    raw.port_id = port_id;
    raw.sample_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    raw.key_needs_state_lookup = 1'b1;
    raw.cycle = $time;

    case (source_kind)
        memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA: begin
            raw.rob_valid = 1'b1;
            raw.replay_inst_valid = 1'b1;
            raw.flush_pipe_valid = 1'b1;
            raw.trigger_valid = 1'b1;
            case (port_id)
                0: begin
                    raw.rob_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_flag;
                    raw.rob_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value;
                    raw.replay_inst = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_replayInst;
                    raw.flush_pipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe;
                    raw.trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_trigger;
                    raw.debug_is_mmio = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO;
                    raw.debug_is_ncio = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO;
                    raw.exception_vec[3] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3;
                    raw.exception_vec[4] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_4;
                    raw.exception_vec[5] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_5;
                    raw.exception_vec[6] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_6;
                    raw.exception_vec[7] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_7;
                    raw.exception_vec[13] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13;
                    raw.exception_vec[15] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_15;
                    raw.exception_vec[19] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_19;
                    raw.exception_vec[21] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_21;
                    raw.exception_vec[23] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_23;
                end
                1: begin
                    raw.rob_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_flag;
                    raw.rob_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_value;
                    raw.replay_inst = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_replayInst;
                    raw.flush_pipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe;
                    raw.trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_trigger;
                    raw.debug_is_mmio = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO;
                    raw.debug_is_ncio = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO;
                    raw.exception_vec[3] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3;
                    raw.exception_vec[4] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_4;
                    raw.exception_vec[5] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_5;
                    raw.exception_vec[13] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_13;
                    raw.exception_vec[19] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_19;
                    raw.exception_vec[21] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_21;
                end
                2: begin
                    raw.rob_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_flag;
                    raw.rob_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_value;
                    raw.replay_inst = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_replayInst;
                    raw.flush_pipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe;
                    raw.trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_trigger;
                    raw.debug_is_mmio = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO;
                    raw.debug_is_ncio = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO;
                    raw.exception_vec[3] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3;
                    raw.exception_vec[4] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_4;
                    raw.exception_vec[5] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_5;
                    raw.exception_vec[13] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_13;
                    raw.exception_vec[19] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_19;
                    raw.exception_vec[21] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_21;
                end
            endcase
        end
        memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA: begin
            raw.rob_valid = 1'b1;
            raw.trigger_valid = 1'b1;
            raw.flush_pipe_valid = port_id == 0;
            case (port_id)
                0: begin
                    raw.rob_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag;
                    raw.rob_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value;
                    raw.flush_pipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe;
                    raw.trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_trigger;
                    raw.debug_is_mmio = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO;
                    raw.debug_is_ncio = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO;
                    raw.exception_vec[0] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0;
                    raw.exception_vec[1] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1;
                    raw.exception_vec[2] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2;
                    raw.exception_vec[3] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3;
                    raw.exception_vec[4] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4;
                    raw.exception_vec[5] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5;
                    raw.exception_vec[6] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6;
                    raw.exception_vec[7] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7;
                    raw.exception_vec[8] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8;
                    raw.exception_vec[9] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9;
                    raw.exception_vec[10] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10;
                    raw.exception_vec[11] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11;
                    raw.exception_vec[12] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12;
                    raw.exception_vec[13] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13;
                    raw.exception_vec[14] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14;
                    raw.exception_vec[15] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15;
                    raw.exception_vec[16] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16;
                    raw.exception_vec[17] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17;
                    raw.exception_vec[18] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18;
                    raw.exception_vec[19] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19;
                    raw.exception_vec[20] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20;
                    raw.exception_vec[21] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21;
                    raw.exception_vec[22] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22;
                    raw.exception_vec[23] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23;
                end
                1: begin
                    raw.rob_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag;
                    raw.rob_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value;
                    raw.trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_trigger;
                    raw.debug_is_mmio = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO;
                    raw.debug_is_ncio = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO;
                    raw.exception_vec[3] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3;
                    raw.exception_vec[6] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_6;
                    raw.exception_vec[7] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_7;
                    raw.exception_vec[15] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_15;
                    raw.exception_vec[19] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_19;
                    raw.exception_vec[23] = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_23;
                end
            endcase
        end
        memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD: begin
            raw.rob_value_only_without_flag = 1'b1;
            case (port_id)
                0: raw.rob_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value;
                1: raw.rob_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value;
            endcase
        end
    endcase
    return raw;
endfunction:build_raw_int_wb_from_v2_port

task io_mem_to_ooo_int_wb_agent_agent_monitor::mon_data();

    logic io_mem_to_ooo_writebackLda_0_valid;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_4;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_5;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_6;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_7;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_15;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_19;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_21;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_23;
    logic [3:0] io_mem_to_ooo_writebackLda_0_bits_uop_trigger;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_rfWen;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_fpWen;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe;
    logic [7:0] io_mem_to_ooo_writebackLda_0_bits_uop_pdest;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_replayInst;
    logic [63:0] io_mem_to_ooo_writebackLda_0_bits_data;
    logic io_mem_to_ooo_writebackLda_0_bits_isFromLoadUnit;
    logic io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO;
    logic io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt;
    logic io_mem_to_ooo_writebackLda_1_valid;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_4;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_5;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_13;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_19;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_21;
    logic [3:0] io_mem_to_ooo_writebackLda_1_bits_uop_trigger;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_rfWen;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_fpWen;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe;
    logic [7:0] io_mem_to_ooo_writebackLda_1_bits_uop_pdest;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_value;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_replayInst;
    logic [63:0] io_mem_to_ooo_writebackLda_1_bits_data;
    logic io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO;
    logic io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt;
    logic io_mem_to_ooo_writebackLda_2_valid;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_4;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_5;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_13;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_19;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_21;
    logic [3:0] io_mem_to_ooo_writebackLda_2_bits_uop_trigger;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_rfWen;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_fpWen;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe;
    logic [7:0] io_mem_to_ooo_writebackLda_2_bits_uop_pdest;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_value;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_replayInst;
    logic [63:0] io_mem_to_ooo_writebackLda_2_bits_data;
    logic io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO;
    logic io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt;
    logic io_mem_to_ooo_writebackSta_0_valid;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23;
    logic [3:0] io_mem_to_ooo_writebackSta_0_bits_uop_trigger;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value;
    logic io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO;
    logic io_mem_to_ooo_writebackSta_1_valid;
    logic io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3;
    logic io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_6;
    logic io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_7;
    logic io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_15;
    logic io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_19;
    logic io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_23;
    logic [3:0] io_mem_to_ooo_writebackSta_1_bits_uop_trigger;
    logic io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value;
    logic io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO;
    logic io_mem_to_ooo_writebackStd_0_valid;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value;
    logic io_mem_to_ooo_writebackStd_1_valid;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value;
    memblock_sync_pkg::dispatch_raw_int_wb_t raw_int_wb;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_mem_to_ooo_writebackLda_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_valid;
        io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3;
        io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_4 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_4;
        io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_5 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_5;
        io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_6 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_6;
        io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_7 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_7;
        io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13;
        io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_15 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_15;
        io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_19 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_19;
        io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_21 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_21;
        io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_23 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_23;
        io_mem_to_ooo_writebackLda_0_bits_uop_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_trigger;
        io_mem_to_ooo_writebackLda_0_bits_uop_rfWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_rfWen;
        io_mem_to_ooo_writebackLda_0_bits_uop_fpWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_fpWen;
        io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe;
        io_mem_to_ooo_writebackLda_0_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_pdest;
        io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_flag;
        io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value;
        io_mem_to_ooo_writebackLda_0_bits_uop_replayInst = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_replayInst;
        io_mem_to_ooo_writebackLda_0_bits_data = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_data;
        io_mem_to_ooo_writebackLda_0_bits_isFromLoadUnit = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_isFromLoadUnit;
        io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO;
        io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO;
        io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt;
        io_mem_to_ooo_writebackLda_1_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_valid;
        io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3;
        io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_4 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_4;
        io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_5 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_5;
        io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_13 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_13;
        io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_19 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_19;
        io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_21 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_21;
        io_mem_to_ooo_writebackLda_1_bits_uop_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_trigger;
        io_mem_to_ooo_writebackLda_1_bits_uop_rfWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_rfWen;
        io_mem_to_ooo_writebackLda_1_bits_uop_fpWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_fpWen;
        io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe;
        io_mem_to_ooo_writebackLda_1_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_pdest;
        io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_flag;
        io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_value;
        io_mem_to_ooo_writebackLda_1_bits_uop_replayInst = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_replayInst;
        io_mem_to_ooo_writebackLda_1_bits_data = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_data;
        io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO;
        io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO;
        io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt;
        io_mem_to_ooo_writebackLda_2_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_valid;
        io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3;
        io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_4 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_4;
        io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_5 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_5;
        io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_13 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_13;
        io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_19 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_19;
        io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_21 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_21;
        io_mem_to_ooo_writebackLda_2_bits_uop_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_trigger;
        io_mem_to_ooo_writebackLda_2_bits_uop_rfWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_rfWen;
        io_mem_to_ooo_writebackLda_2_bits_uop_fpWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_fpWen;
        io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe;
        io_mem_to_ooo_writebackLda_2_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_pdest;
        io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_flag;
        io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_value;
        io_mem_to_ooo_writebackLda_2_bits_uop_replayInst = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_replayInst;
        io_mem_to_ooo_writebackLda_2_bits_data = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_data;
        io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO;
        io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO;
        io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt;
        io_mem_to_ooo_writebackSta_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_valid;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23;
        io_mem_to_ooo_writebackSta_0_bits_uop_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_trigger;
        io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe;
        io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag;
        io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value;
        io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO;
        io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO;
        io_mem_to_ooo_writebackSta_1_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_valid;
        io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3;
        io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_6 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_6;
        io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_7 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_7;
        io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_15 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_15;
        io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_19 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_19;
        io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_23 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_23;
        io_mem_to_ooo_writebackSta_1_bits_uop_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_trigger;
        io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag;
        io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value;
        io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO;
        io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO;
        io_mem_to_ooo_writebackStd_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackStd_0_valid;
        io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value;
        io_mem_to_ooo_writebackStd_1_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackStd_1_valid;
        io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_valid,io_mem_to_ooo_writebackLda_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3,io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_3,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_4,io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_4,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_5,io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_5,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_6,io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_6,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_7,io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_7,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13,io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_13,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_15,io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_15,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_19,io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_19,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_21,io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_21,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_23,io_mem_to_ooo_writebackLda_0_bits_uop_exceptionVec_23,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_trigger,io_mem_to_ooo_writebackLda_0_bits_uop_trigger,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_rfWen,io_mem_to_ooo_writebackLda_0_bits_uop_rfWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_fpWen,io_mem_to_ooo_writebackLda_0_bits_uop_fpWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe,io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_pdest,io_mem_to_ooo_writebackLda_0_bits_uop_pdest,8);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_flag,io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value,io_mem_to_ooo_writebackLda_0_bits_uop_robIdx_value,`MEMBLOCK_DUT_ROB_VALUE_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_uop_replayInst,io_mem_to_ooo_writebackLda_0_bits_uop_replayInst,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_data,io_mem_to_ooo_writebackLda_0_bits_data,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_isFromLoadUnit,io_mem_to_ooo_writebackLda_0_bits_isFromLoadUnit,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO,io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO,io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt,io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_valid,io_mem_to_ooo_writebackLda_1_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3,io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_3,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_4,io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_4,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_5,io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_5,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_13,io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_13,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_19,io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_19,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_21,io_mem_to_ooo_writebackLda_1_bits_uop_exceptionVec_21,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_trigger,io_mem_to_ooo_writebackLda_1_bits_uop_trigger,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_rfWen,io_mem_to_ooo_writebackLda_1_bits_uop_rfWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_fpWen,io_mem_to_ooo_writebackLda_1_bits_uop_fpWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe,io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_pdest,io_mem_to_ooo_writebackLda_1_bits_uop_pdest,8);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_value,io_mem_to_ooo_writebackLda_1_bits_uop_robIdx_value,`MEMBLOCK_DUT_ROB_VALUE_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_uop_replayInst,io_mem_to_ooo_writebackLda_1_bits_uop_replayInst,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_data,io_mem_to_ooo_writebackLda_1_bits_data,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO,io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO,io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt,io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_valid,io_mem_to_ooo_writebackLda_2_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3,io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_3,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_4,io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_4,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_5,io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_5,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_13,io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_13,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_19,io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_19,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_21,io_mem_to_ooo_writebackLda_2_bits_uop_exceptionVec_21,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_trigger,io_mem_to_ooo_writebackLda_2_bits_uop_trigger,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_rfWen,io_mem_to_ooo_writebackLda_2_bits_uop_rfWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_fpWen,io_mem_to_ooo_writebackLda_2_bits_uop_fpWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe,io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_pdest,io_mem_to_ooo_writebackLda_2_bits_uop_pdest,8);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_flag,io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_value,io_mem_to_ooo_writebackLda_2_bits_uop_robIdx_value,`MEMBLOCK_DUT_ROB_VALUE_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_uop_replayInst,io_mem_to_ooo_writebackLda_2_bits_uop_replayInst,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_data,io_mem_to_ooo_writebackLda_2_bits_data,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO,io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO,io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt,io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_valid,io_mem_to_ooo_writebackSta_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23,io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_trigger,io_mem_to_ooo_writebackSta_0_bits_uop_trigger,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe,io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value,io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value,`MEMBLOCK_DUT_ROB_VALUE_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO,io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO,io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_valid,io_mem_to_ooo_writebackSta_1_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3,io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_3,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_6,io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_6,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_7,io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_7,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_15,io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_15,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_19,io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_19,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_23,io_mem_to_ooo_writebackSta_1_bits_uop_exceptionVec_23,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_bits_uop_trigger,io_mem_to_ooo_writebackSta_1_bits_uop_trigger,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value,`MEMBLOCK_DUT_ROB_VALUE_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO,io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO,io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackStd_0_valid,io_mem_to_ooo_writebackStd_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value,io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value,`MEMBLOCK_DUT_ROB_VALUE_W);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackStd_1_valid,io_mem_to_ooo_writebackStd_1_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value,io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value,`MEMBLOCK_DUT_ROB_VALUE_W);
        end
        if(this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            // mon_data 是唯一 raw queue push owner，顺序固定为 LDA0/1/2、STA0/1、STD0/1。
            case (io_mem_to_ooo_writebackLda_0_valid)
                1'b0: ;
                1'b1: begin
                    raw_int_wb = build_raw_int_wb_from_v2_port(memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA, 0);
                    if (!raw_int_wb.valid) `uvm_fatal("INT_WB_MON", "builder dropped valid LDA0 event")
                    memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
                end
                default: `uvm_fatal("INT_WB_MON", "LDA0 valid is X/Z")
            endcase
            case (io_mem_to_ooo_writebackLda_1_valid)
                1'b0: ;
                1'b1: begin
                    raw_int_wb = build_raw_int_wb_from_v2_port(memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA, 1);
                    if (!raw_int_wb.valid) `uvm_fatal("INT_WB_MON", "builder dropped valid LDA1 event")
                    memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
                end
                default: `uvm_fatal("INT_WB_MON", "LDA1 valid is X/Z")
            endcase
            case (io_mem_to_ooo_writebackLda_2_valid)
                1'b0: ;
                1'b1: begin
                    raw_int_wb = build_raw_int_wb_from_v2_port(memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA, 2);
                    if (!raw_int_wb.valid) `uvm_fatal("INT_WB_MON", "builder dropped valid LDA2 event")
                    memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
                end
                default: `uvm_fatal("INT_WB_MON", "LDA2 valid is X/Z")
            endcase
            case (io_mem_to_ooo_writebackSta_0_valid)
                1'b0: ;
                1'b1: begin
                    raw_int_wb = build_raw_int_wb_from_v2_port(memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA, 0);
                    if (!raw_int_wb.valid) `uvm_fatal("INT_WB_MON", "builder dropped valid STA0 event")
                    memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
                end
                default: `uvm_fatal("INT_WB_MON", "STA0 valid is X/Z")
            endcase
            case (io_mem_to_ooo_writebackSta_1_valid)
                1'b0: ;
                1'b1: begin
                    raw_int_wb = build_raw_int_wb_from_v2_port(memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA, 1);
                    if (!raw_int_wb.valid) `uvm_fatal("INT_WB_MON", "builder dropped valid STA1 event")
                    memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
                end
                default: `uvm_fatal("INT_WB_MON", "STA1 valid is X/Z")
            endcase
            case (io_mem_to_ooo_writebackStd_0_valid)
                1'b0: ;
                1'b1: begin
                    raw_int_wb = build_raw_int_wb_from_v2_port(memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD, 0);
                    if (!raw_int_wb.valid) `uvm_fatal("INT_WB_MON", "builder dropped valid STD0 event")
                    memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
                end
                default: `uvm_fatal("INT_WB_MON", "STD0 valid is X/Z")
            endcase
            case (io_mem_to_ooo_writebackStd_1_valid)
                1'b0: ;
                1'b1: begin
                    raw_int_wb = build_raw_int_wb_from_v2_port(memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD, 1);
                    if (!raw_int_wb.valid) `uvm_fatal("INT_WB_MON", "builder dropped valid STD1 event")
                    memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
                end
                default: `uvm_fatal("INT_WB_MON", "STD1 valid is X/Z")
            endcase
        end
    end
endtask:mon_data

`endif
