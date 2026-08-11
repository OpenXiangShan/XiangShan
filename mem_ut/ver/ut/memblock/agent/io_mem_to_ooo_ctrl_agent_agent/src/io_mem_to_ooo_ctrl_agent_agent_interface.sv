//=========================================================
//File name    : io_mem_to_ooo_ctrl_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_ctrl_agent_agent_interface
//Discribution : io_mem_to_ooo_ctrl_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_CTRL_AGENT_AGENT_INTERFACE__SV
`define IO_MEM_TO_OOO_CTRL_AGENT_AGENT_INTERFACE__SV

`include "memblock_compile_params.svh"

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface io_mem_to_ooo_ctrl_agent_agent_interface  (input bit clk,input bit rst_n);

    // V2 把三个 load MMIO output 展平成独立端口。profile 数量不匹配时在
    // elaboration 初始阶段失败，避免 monitor 对 packed mirror 做错误解释。
    initial begin : check_v2_ctrl_profile
        if (`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM != 3) begin
            $fatal(1,
                   "V2 ctrl interface requires MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM=3, got %0d",
                   `MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM);
        end
        if (`MEMBLOCK_DUT_HAS_SQ_DEQ_PTR != 0) begin
            $fatal(1,
                   "V2 ctrl interface has no SQ deq pointer, MEMBLOCK_DUT_HAS_SQ_DEQ_PTR must be 0");
        end
    end

    logic [5:0] io_mem_to_ooo_topToBackendBypass_hartId;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
    logic io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
    logic [11:0] io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
    logic io_mem_to_ooo_topToBackendBypass_clintTime_valid;
    logic [63:0] io_mem_to_ooo_topToBackendBypass_clintTime_bits;
    logic io_mem_to_ooo_topToBackendBypass_l2FlushDone;
    logic [`MEMBLOCK_LQ_CANCEL_COUNT_W-1:0] io_mem_to_ooo_lqCancelCnt;
    logic [`MEMBLOCK_SQ_CANCEL_COUNT_W-1:0] io_mem_to_ooo_sqCancelCnt;
    logic [`MEMBLOCK_SQ_DEQ_COUNT_W-1:0] io_mem_to_ooo_sqDeq;
    logic [3:0] io_mem_to_ooo_lqDeq    ;
    logic io_mem_to_ooo_lqDeqPtr_flag  ;
    logic [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_mem_to_ooo_lqDeqPtr_value;
    logic io_mem_to_ooo_memoryViolation_valid;
    logic io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
    logic [`MEMBLOCK_DUT_FTQ_PTR_VALUE_W-1:0] io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
    logic [`MEMBLOCK_DUT_FTQ_OFFSET_W-1:0] io_mem_to_ooo_memoryViolation_bits_ftqOffset;
    logic io_mem_to_ooo_memoryViolation_bits_isRVC;
    logic io_mem_to_ooo_memoryViolation_bits_level;
    logic io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_memoryViolation_bits_robIdx_value;
    logic io_mem_to_ooo_sbIsEmpty      ;
    logic [63:0] io_mem_to_ooo_lsqio_vaddr;
    logic [63:0] io_mem_to_ooo_lsqio_gpaddr;
    logic io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
    logic io_mem_to_ooo_ldCancel_0_ld2Cancel;
    logic io_mem_to_ooo_ldCancel_1_ld2Cancel;
    logic io_mem_to_ooo_ldCancel_2_ld2Cancel;

    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value;
    logic io_mem_to_ooo_lsqio_loadMmio_0;
    logic io_mem_to_ooo_lsqio_loadMmio_1;
    logic io_mem_to_ooo_lsqio_loadMmio_2;
    logic io_mem_to_ooo_lsqio_storeMmio;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_mem_to_ooo_topToBackendBypass_hartId;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
        input  io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
        input  io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
        input  io_mem_to_ooo_topToBackendBypass_clintTime_valid;
        input  io_mem_to_ooo_topToBackendBypass_clintTime_bits;
        input  io_mem_to_ooo_topToBackendBypass_l2FlushDone;
        input  io_mem_to_ooo_lqCancelCnt;
        input  io_mem_to_ooo_sqCancelCnt;
        input  io_mem_to_ooo_sqDeq;
        input  io_mem_to_ooo_lqDeq;
        input  io_mem_to_ooo_lqDeqPtr_flag;
        input  io_mem_to_ooo_lqDeqPtr_value;
        input  io_mem_to_ooo_memoryViolation_valid;
        input  io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
        input  io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
        input  io_mem_to_ooo_memoryViolation_bits_ftqOffset;
        input  io_mem_to_ooo_memoryViolation_bits_isRVC;
        input  io_mem_to_ooo_memoryViolation_bits_level;
        input  io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
        input  io_mem_to_ooo_memoryViolation_bits_robIdx_value;
        input  io_mem_to_ooo_sbIsEmpty;
        input  io_mem_to_ooo_lsqio_vaddr;
        input  io_mem_to_ooo_lsqio_gpaddr;
        input  io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
        input  io_mem_to_ooo_ldCancel_0_ld2Cancel;
        input  io_mem_to_ooo_ldCancel_1_ld2Cancel;
        input  io_mem_to_ooo_ldCancel_2_ld2Cancel;

        input  io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value;
        input  io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value;
        input  io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value;
        input  io_mem_to_ooo_lsqio_loadMmio_0;
        input  io_mem_to_ooo_lsqio_loadMmio_1;
        input  io_mem_to_ooo_lsqio_loadMmio_2;
        input  io_mem_to_ooo_lsqio_storeMmio;
        input  io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value;
    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_mem_to_ooo_topToBackendBypass_hartId;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
        input  io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
        input  io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
        input  io_mem_to_ooo_topToBackendBypass_clintTime_valid;
        input  io_mem_to_ooo_topToBackendBypass_clintTime_bits;
        input  io_mem_to_ooo_topToBackendBypass_l2FlushDone;
        input  io_mem_to_ooo_lqCancelCnt;
        input  io_mem_to_ooo_sqCancelCnt;
        input  io_mem_to_ooo_sqDeq;
        input  io_mem_to_ooo_lqDeq;
        input  io_mem_to_ooo_lqDeqPtr_flag;
        input  io_mem_to_ooo_lqDeqPtr_value;
        input  io_mem_to_ooo_memoryViolation_valid;
        input  io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
        input  io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
        input  io_mem_to_ooo_memoryViolation_bits_ftqOffset;
        input  io_mem_to_ooo_memoryViolation_bits_isRVC;
        input  io_mem_to_ooo_memoryViolation_bits_level;
        input  io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
        input  io_mem_to_ooo_memoryViolation_bits_robIdx_value;
        input  io_mem_to_ooo_sbIsEmpty;
        input  io_mem_to_ooo_lsqio_vaddr;
        input  io_mem_to_ooo_lsqio_gpaddr;
        input  io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
        input  io_mem_to_ooo_ldCancel_0_ld2Cancel;
        input  io_mem_to_ooo_ldCancel_1_ld2Cancel;
        input  io_mem_to_ooo_ldCancel_2_ld2Cancel;

        input  io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value;
        input  io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value;
        input  io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value;
        input  io_mem_to_ooo_lsqio_loadMmio_0;
        input  io_mem_to_ooo_lsqio_loadMmio_1;
        input  io_mem_to_ooo_lsqio_loadMmio_2;
        input  io_mem_to_ooo_lsqio_storeMmio;
        input  io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value;
    endclocking:mon_cb

    // 中文注释：只把当前 V2 扁平 MMIO output 映射成参数化 packed mirror。
    // 本 accessor 不检查 X/Z、不解析 UID，也不写 raw queue 或运行期状态。
    function automatic void sample_mmio_outputs(
        output logic [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0] load_valid,
        output logic [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0][`MEMBLOCK_DUT_ROB_VALUE_W-1:0]
                     load_rob_value,
        output logic store_valid,
        output logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] store_rob_value
    );
        load_valid = '0;
        load_rob_value = '0;
        store_valid = mon_cb.io_mem_to_ooo_lsqio_storeMmio;
        store_rob_value = mon_cb.io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value;
        load_valid[0] = mon_cb.io_mem_to_ooo_lsqio_loadMmio_0;
        load_valid[1] = mon_cb.io_mem_to_ooo_lsqio_loadMmio_1;
        load_valid[2] = mon_cb.io_mem_to_ooo_lsqio_loadMmio_2;
        load_rob_value[0] = mon_cb.io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value;
        load_rob_value[1] = mon_cb.io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value;
        load_rob_value[2] = mon_cb.io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value;
    endfunction:sample_mmio_outputs

    // 中文注释：V2 sqDeq 只有 count，没有 pointer。所有输出先清零并保持
    // valid=0；函数体不得引用当前 profile 不存在的 interface 成员。
    function automatic void sample_sq_deq_ptr(
        input logic [`MEMBLOCK_SQ_DEQ_COUNT_W-1:0] sampled_sq_deq,
        output logic ptr_valid,
        output logic ptr_flag,
        output logic [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] ptr_value
    );
        ptr_valid = 1'b0;
        ptr_flag = 1'b0;
        ptr_value = '0;
    endfunction:sample_sq_deq_ptr

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:io_mem_to_ooo_ctrl_agent_agent_interface

`endif
