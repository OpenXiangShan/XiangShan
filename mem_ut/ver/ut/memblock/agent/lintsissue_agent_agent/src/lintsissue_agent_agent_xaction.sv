//=========================================================
//File name    : lintsissue_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : lintsissue_agent_agent_xaction
//Discribution : lintsissue_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef LINTSISSUE_AGENT_AGENT_XACTION__SV
`define LINTSISSUE_AGENT_AGENT_XACTION__SV

class lintsissue_agent_agent_xaction  extends tcnt_data_base;
    // issueLda/issueSta/issueStd carry the scalar LSU entry traffic. Base constraints here only
    // capture width/capacity and broad opcode legality; per-port semantics are
    // expected to be tightened by scenario sequences.
    bit memblock_dispatch_wait_ready;
    // 中文注释：本次 issue xaction 是否使用非阻塞 ready 采样。
    // 置位：memblock_issue_dispatch_base_sequence 从 seq_csr_common 读取开关后写入。
    // 作用：为 1 时 driver 只采样一次 valid&&ready，未 fire port 不设置 fired_mask，sequence 后续不出队这些 item。
    bit memblock_dispatch_nonblocking_issue;
    int unsigned memblock_dispatch_ready_timeout;
    bit memblock_dispatch_aborted_by_redirect;
    int unsigned memblock_dispatch_flush_epoch;
    bit [(`MEMBLOCK_DUT_LOAD_PORT_BASE + `MEMBLOCK_DUT_LOAD_PIPE_NUM +
          `MEMBLOCK_DUT_STA_PIPE_NUM + `MEMBLOCK_DUT_STD_PIPE_NUM)-1:0]
        memblock_dispatch_fired_mask;

    rand bit [63:0] io_ooo_to_mem_issueLda_0_bits_src_0;
    rand bit io_ooo_to_mem_issueLda_0_bits_uop_fpWen;
    rand bit [`MEMBLOCK_DUT_FTQ_OFFSET_W-1:0] io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset;
    rand bit io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag;
    rand bit [`MEMBLOCK_DUT_FTQ_PTR_VALUE_W-1:0] io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value;
    rand bit [8:0] io_ooo_to_mem_issueLda_0_bits_uop_fuOpType;
    rand bit [31:0] io_ooo_to_mem_issueLda_0_bits_uop_imm;
    rand bit io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit;
    rand bit io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict;
    rand bit io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag;
    rand bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value;
    rand bit [49:0] io_ooo_to_mem_issueLda_0_bits_uop_pc;
    rand bit [7:0] io_ooo_to_mem_issueLda_0_bits_uop_pdest;
    rand bit io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC;
    rand bit io_ooo_to_mem_issueLda_0_bits_uop_rfWen;
    rand bit io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value;
    rand bit io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value;
    rand bit io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit;
    rand bit io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value;
    rand bit io_ooo_to_mem_issueLda_0_ready;
    rand bit io_ooo_to_mem_issueLda_0_valid;
    rand bit [63:0] io_ooo_to_mem_issueLda_1_bits_src_0;
    rand bit io_ooo_to_mem_issueLda_1_bits_uop_fpWen;
    rand bit [`MEMBLOCK_DUT_FTQ_OFFSET_W-1:0] io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset;
    rand bit io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag;
    rand bit [`MEMBLOCK_DUT_FTQ_PTR_VALUE_W-1:0] io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value;
    rand bit [8:0] io_ooo_to_mem_issueLda_1_bits_uop_fuOpType;
    rand bit [31:0] io_ooo_to_mem_issueLda_1_bits_uop_imm;
    rand bit io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit;
    rand bit io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict;
    rand bit io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag;
    rand bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value;
    rand bit [49:0] io_ooo_to_mem_issueLda_1_bits_uop_pc;
    rand bit [7:0] io_ooo_to_mem_issueLda_1_bits_uop_pdest;
    rand bit io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC;
    rand bit io_ooo_to_mem_issueLda_1_bits_uop_rfWen;
    rand bit io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value;
    rand bit io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value;
    rand bit io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit;
    rand bit io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value;
    rand bit io_ooo_to_mem_issueLda_1_ready;
    rand bit io_ooo_to_mem_issueLda_1_valid;
    rand bit [63:0] io_ooo_to_mem_issueLda_2_bits_src_0;
    rand bit io_ooo_to_mem_issueLda_2_bits_uop_fpWen;
    rand bit [`MEMBLOCK_DUT_FTQ_OFFSET_W-1:0] io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset;
    rand bit io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag;
    rand bit [`MEMBLOCK_DUT_FTQ_PTR_VALUE_W-1:0] io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value;
    rand bit [8:0] io_ooo_to_mem_issueLda_2_bits_uop_fuOpType;
    rand bit [31:0] io_ooo_to_mem_issueLda_2_bits_uop_imm;
    rand bit io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit;
    rand bit io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict;
    rand bit io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag;
    rand bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value;
    rand bit [49:0] io_ooo_to_mem_issueLda_2_bits_uop_pc;
    rand bit [7:0] io_ooo_to_mem_issueLda_2_bits_uop_pdest;
    rand bit io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC;
    rand bit io_ooo_to_mem_issueLda_2_bits_uop_rfWen;
    rand bit io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value;
    rand bit io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value;
    rand bit io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit;
    rand bit io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value;
    rand bit io_ooo_to_mem_issueLda_2_ready;
    rand bit io_ooo_to_mem_issueLda_2_valid;
    rand bit [63:0] io_ooo_to_mem_issueSta_0_bits_src_0;
    rand bit [8:0] io_ooo_to_mem_issueSta_0_bits_uop_fuOpType;
    rand bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_issueSta_0_bits_uop_fuType;
    rand bit [31:0] io_ooo_to_mem_issueSta_0_bits_uop_imm;
    rand bit [7:0] io_ooo_to_mem_issueSta_0_bits_uop_pdest;
    rand bit io_ooo_to_mem_issueSta_0_bits_uop_rfWen;
    rand bit io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value;
    rand bit io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value;
    rand bit io_ooo_to_mem_issueSta_0_ready;
    rand bit io_ooo_to_mem_issueSta_0_valid;
    rand bit [63:0] io_ooo_to_mem_issueSta_1_bits_src_0;
    rand bit [8:0] io_ooo_to_mem_issueSta_1_bits_uop_fuOpType;
    rand bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_issueSta_1_bits_uop_fuType;
    rand bit [31:0] io_ooo_to_mem_issueSta_1_bits_uop_imm;
    rand bit [7:0] io_ooo_to_mem_issueSta_1_bits_uop_pdest;
    rand bit io_ooo_to_mem_issueSta_1_bits_uop_rfWen;
    rand bit io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value;
    rand bit io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value;
    rand bit io_ooo_to_mem_issueSta_1_ready;
    rand bit io_ooo_to_mem_issueSta_1_valid;
    rand bit [63:0] io_ooo_to_mem_issueStd_0_bits_src_0;
    rand bit [8:0] io_ooo_to_mem_issueStd_0_bits_uop_fuOpType;
    rand bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_issueStd_0_bits_uop_fuType;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value;
    rand bit io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value;
    rand bit io_ooo_to_mem_issueStd_0_ready;
    rand bit io_ooo_to_mem_issueStd_0_valid;
    rand bit [63:0] io_ooo_to_mem_issueStd_1_bits_src_0;
    rand bit [8:0] io_ooo_to_mem_issueStd_1_bits_uop_fuOpType;
    rand bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_issueStd_1_bits_uop_fuType;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value;
    rand bit io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value;
    rand bit io_ooo_to_mem_issueStd_1_ready;
    rand bit io_ooo_to_mem_issueStd_1_valid;

    extern function new(string name="lintsissue_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(lintsissue_agent_agent_xaction)
        `uvm_field_int(memblock_dispatch_wait_ready, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_nonblocking_issue, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_ready_timeout, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_aborted_by_redirect, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_flush_epoch, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_fired_mask, UVM_ALL_ON);

        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_fpWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_imm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_pc, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_rfWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_fpWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_imm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_pc, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_rfWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_1_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_fpWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_imm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_pc, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_rfWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueLda_2_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_bits_uop_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_bits_uop_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_bits_uop_imm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_bits_uop_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_bits_uop_rfWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_bits_uop_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_bits_uop_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_bits_uop_imm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_bits_uop_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_bits_uop_rfWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueSta_1_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_0_bits_uop_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_0_bits_uop_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_1_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_1_bits_uop_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_1_bits_uop_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_1_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueStd_1_valid, UVM_ALL_ON);
    `uvm_object_utils_end

endclass:lintsissue_agent_agent_xaction































































































































function lintsissue_agent_agent_xaction::new(string name = "lintsissue_agent_agent_xaction");
    super.new();
    memblock_dispatch_wait_ready = 1'b0;
    memblock_dispatch_nonblocking_issue = 1'b0;
    memblock_dispatch_ready_timeout = 1000;
    memblock_dispatch_aborted_by_redirect = 1'b0;
    memblock_dispatch_flush_epoch = 0;
    memblock_dispatch_fired_mask = '0;
endfunction:new

function void lintsissue_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void lintsissue_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void lintsissue_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void lintsissue_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string lintsissue_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%smemblock_dispatch_wait_ready = 0x%0h ",pkt_str,this.memblock_dispatch_wait_ready);
    pkt_str = $sformatf("%smemblock_dispatch_nonblocking_issue = 0x%0h ",pkt_str,this.memblock_dispatch_nonblocking_issue);
    pkt_str = $sformatf("%smemblock_dispatch_ready_timeout = %0d ",pkt_str,this.memblock_dispatch_ready_timeout);

    return pkt_str;
endfunction:psdisplay

function bit lintsissue_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    lintsissue_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a lintsissue_agent_agent_xaction or its extend"))
    end
    super_result = super.compare(rhs_,comparer);
    if(super_result==0) begin
        super_result = 1;
        //foreach(this.pload_q[i]) begin
        //    if(this.pload_q[i]!=rhs_.pload_q[i]) begin
        //        super_result = 0;
        //        `uvm_info(get_type_name(),$sformatf("compare fail for this.pload[%0d]=0x%2h while the rhs_.pload[%0d]=0x%2h",i,this.pload_q[i],i,rhs_.pload_q[i]),UVM_NONE)
        //    end
        //end































































































































    end
    return super_result;
endfunction:compare

`endif
