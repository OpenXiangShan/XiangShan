//=========================================================
//File name    : io_mem_to_ooo_ctrl_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_ctrl_agent_agent_monitor
//Discribution : io_mem_to_ooo_ctrl_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_CTRL_AGENT_AGENT_MONITOR__SV
`define IO_MEM_TO_OOO_CTRL_AGENT_AGENT_MONITOR__SV

class io_mem_to_ooo_ctrl_agent_agent_monitor  extends tcnt_monitor_base#(virtual io_mem_to_ooo_ctrl_agent_agent_interface,io_mem_to_ooo_ctrl_agent_agent_cfg,io_mem_to_ooo_ctrl_agent_agent_xaction);

    `uvm_component_utils(io_mem_to_ooo_ctrl_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:io_mem_to_ooo_ctrl_agent_agent_monitor

function io_mem_to_ooo_ctrl_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void io_mem_to_ooo_ctrl_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task io_mem_to_ooo_ctrl_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task io_mem_to_ooo_ctrl_agent_agent_monitor::mon_data();

    logic [5:0] io_mem_to_ooo_topToBackendBypass_hartId;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
    logic io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
    logic [12:0] io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
    logic io_mem_to_ooo_topToBackendBypass_clintTime_valid;
    logic [63:0] io_mem_to_ooo_topToBackendBypass_clintTime_bits;
    logic io_mem_to_ooo_topToBackendBypass_l2FlushDone;
    logic [6:0] io_mem_to_ooo_lqCancelCnt;
    logic [5:0] io_mem_to_ooo_sqCancelCnt;
    logic [1:0] io_mem_to_ooo_sqDeq    ;
    logic [3:0] io_mem_to_ooo_lqDeq    ;
    logic io_mem_to_ooo_lqDeqPtr_flag  ;
    logic [6:0] io_mem_to_ooo_lqDeqPtr_value;
    logic io_mem_to_ooo_memoryViolation_valid;
    logic io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
    logic [5:0] io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
    logic [4:0] io_mem_to_ooo_memoryViolation_bits_ftqOffset;
    logic io_mem_to_ooo_memoryViolation_bits_isRVC;
    logic io_mem_to_ooo_memoryViolation_bits_level;
    logic io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_memoryViolation_bits_robIdx_value;
    logic io_mem_to_ooo_sbIsEmpty      ;
    logic [63:0] io_mem_to_ooo_lsqio_vaddr;
    logic [63:0] io_mem_to_ooo_lsqio_gpaddr;
    logic io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
    logic io_mem_to_ooo_ldCancel_0_ld2Cancel;
    logic io_mem_to_ooo_ldCancel_1_ld2Cancel;
    logic io_mem_to_ooo_ldCancel_2_ld2Cancel;

    logic [7:0] io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value;
    logic [7:0] io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value;
    logic [7:0] io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value;
    logic io_mem_to_ooo_lsqio_loadMmio_0;
    logic io_mem_to_ooo_lsqio_loadMmio_1;
    logic io_mem_to_ooo_lsqio_loadMmio_2;
    logic io_mem_to_ooo_lsqio_storeMmio;
    logic [7:0] io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value;
    io_mem_to_ooo_ctrl_agent_agent_xaction  mon_tr;
    memblock_sync_pkg::dispatch_raw_ctrl_t raw_ctrl;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_mem_to_ooo_topToBackendBypass_hartId = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_hartId;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
        io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
        io_mem_to_ooo_topToBackendBypass_msiInfo_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
        io_mem_to_ooo_topToBackendBypass_msiInfo_bits = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
        io_mem_to_ooo_topToBackendBypass_clintTime_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_clintTime_valid;
        io_mem_to_ooo_topToBackendBypass_clintTime_bits = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_clintTime_bits;
        io_mem_to_ooo_topToBackendBypass_l2FlushDone = this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_l2FlushDone;
        io_mem_to_ooo_lqCancelCnt = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lqCancelCnt;
        io_mem_to_ooo_sqCancelCnt = this.vif.mon_mp.mon_cb.io_mem_to_ooo_sqCancelCnt;
        io_mem_to_ooo_sqDeq = this.vif.mon_mp.mon_cb.io_mem_to_ooo_sqDeq;
        io_mem_to_ooo_lqDeq = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lqDeq;
        io_mem_to_ooo_lqDeqPtr_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lqDeqPtr_flag;
        io_mem_to_ooo_lqDeqPtr_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lqDeqPtr_value;
        io_mem_to_ooo_memoryViolation_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_valid;
        io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
        io_mem_to_ooo_memoryViolation_bits_ftqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
        io_mem_to_ooo_memoryViolation_bits_ftqOffset = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_ftqOffset;
        io_mem_to_ooo_memoryViolation_bits_isRVC = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_isRVC;
        io_mem_to_ooo_memoryViolation_bits_level = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_level;
        io_mem_to_ooo_memoryViolation_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
        io_mem_to_ooo_memoryViolation_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_memoryViolation_bits_robIdx_value;
        io_mem_to_ooo_sbIsEmpty = this.vif.mon_mp.mon_cb.io_mem_to_ooo_sbIsEmpty;
        io_mem_to_ooo_lsqio_vaddr = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_vaddr;
        io_mem_to_ooo_lsqio_gpaddr = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_gpaddr;
        io_mem_to_ooo_lsqio_isForVSnonLeafPTE = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
        io_mem_to_ooo_ldCancel_0_ld2Cancel = this.vif.mon_mp.mon_cb.io_mem_to_ooo_ldCancel_0_ld2Cancel;
        io_mem_to_ooo_ldCancel_1_ld2Cancel = this.vif.mon_mp.mon_cb.io_mem_to_ooo_ldCancel_1_ld2Cancel;
        io_mem_to_ooo_ldCancel_2_ld2Cancel = this.vif.mon_mp.mon_cb.io_mem_to_ooo_ldCancel_2_ld2Cancel;

        io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value;
        io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value;
        io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value;
        io_mem_to_ooo_lsqio_loadMmio_0 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_loadMmio_0;
        io_mem_to_ooo_lsqio_loadMmio_1 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_loadMmio_1;
        io_mem_to_ooo_lsqio_loadMmio_2 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_loadMmio_2;
        io_mem_to_ooo_lsqio_storeMmio = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_storeMmio;
        io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_hartId,io_mem_to_ooo_topToBackendBypass_hartId,6);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip,io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip,io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip,io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip,io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31,io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43,io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_msiInfo_valid,io_mem_to_ooo_topToBackendBypass_msiInfo_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_msiInfo_bits,io_mem_to_ooo_topToBackendBypass_msiInfo_bits,13);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_clintTime_valid,io_mem_to_ooo_topToBackendBypass_clintTime_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_clintTime_bits,io_mem_to_ooo_topToBackendBypass_clintTime_bits,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_l2FlushDone,io_mem_to_ooo_topToBackendBypass_l2FlushDone,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lqCancelCnt,io_mem_to_ooo_lqCancelCnt,7);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_sqCancelCnt,io_mem_to_ooo_sqCancelCnt,6);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_sqDeq,io_mem_to_ooo_sqDeq,2);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lqDeq,io_mem_to_ooo_lqDeq,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lqDeqPtr_flag,io_mem_to_ooo_lqDeqPtr_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lqDeqPtr_value,io_mem_to_ooo_lqDeqPtr_value,7);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_valid,io_mem_to_ooo_memoryViolation_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag,io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_ftqIdx_value,io_mem_to_ooo_memoryViolation_bits_ftqIdx_value,6);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_ftqOffset,io_mem_to_ooo_memoryViolation_bits_ftqOffset,5);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_isRVC,io_mem_to_ooo_memoryViolation_bits_isRVC,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_level,io_mem_to_ooo_memoryViolation_bits_level,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_robIdx_flag,io_mem_to_ooo_memoryViolation_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_memoryViolation_bits_robIdx_value,io_mem_to_ooo_memoryViolation_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_sbIsEmpty,io_mem_to_ooo_sbIsEmpty,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lsqio_vaddr,io_mem_to_ooo_lsqio_vaddr,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lsqio_gpaddr,io_mem_to_ooo_lsqio_gpaddr,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_lsqio_isForVSnonLeafPTE,io_mem_to_ooo_lsqio_isForVSnonLeafPTE,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_ldCancel_0_ld2Cancel,io_mem_to_ooo_ldCancel_0_ld2Cancel,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_ldCancel_1_ld2Cancel,io_mem_to_ooo_ldCancel_1_ld2Cancel,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_ldCancel_2_ld2Cancel,io_mem_to_ooo_ldCancel_2_ld2Cancel,1);

        end
        if(this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            if (io_mem_to_ooo_lqDeq != '0 ||
                io_mem_to_ooo_sqDeq != '0 ||
                io_mem_to_ooo_memoryViolation_valid ||
                memblock_sync_pkg::dispatch_flushsb_waiting_empty) begin
                raw_ctrl = memblock_sync_pkg::make_empty_raw_ctrl();
                raw_ctrl.valid = 1'b1;
                raw_ctrl.lq_deq = io_mem_to_ooo_lqDeq;
                raw_ctrl.sq_deq = io_mem_to_ooo_sqDeq;
                raw_ctrl.lq_deq_ptr_flag = io_mem_to_ooo_lqDeqPtr_flag;
                raw_ctrl.lq_deq_ptr_value = io_mem_to_ooo_lqDeqPtr_value;
                raw_ctrl.memory_violation_valid = io_mem_to_ooo_memoryViolation_valid;
                raw_ctrl.memory_violation_rob_valid = io_mem_to_ooo_memoryViolation_valid;
                raw_ctrl.memory_violation_rob_flag = io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
                raw_ctrl.memory_violation_rob_value = io_mem_to_ooo_memoryViolation_bits_robIdx_value;
                raw_ctrl.memory_violation_level = io_mem_to_ooo_memoryViolation_bits_level;
                raw_ctrl.sb_is_empty = io_mem_to_ooo_sbIsEmpty;
                raw_ctrl.cycle = $time;
                memblock_sync_pkg::push_raw_ctrl(raw_ctrl);
            end
        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = io_mem_to_ooo_ctrl_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_hartId = io_mem_to_ooo_topToBackendBypass_hartId;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip = io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip = io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip = io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip = io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31 = io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43 = io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_msiInfo_valid = io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_msiInfo_bits = io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_clintTime_valid = io_mem_to_ooo_topToBackendBypass_clintTime_valid;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_clintTime_bits = io_mem_to_ooo_topToBackendBypass_clintTime_bits;
        //    mon_tr.io_mem_to_ooo_topToBackendBypass_l2FlushDone = io_mem_to_ooo_topToBackendBypass_l2FlushDone;
        //    mon_tr.io_mem_to_ooo_lqCancelCnt = io_mem_to_ooo_lqCancelCnt;
        //    mon_tr.io_mem_to_ooo_sqCancelCnt = io_mem_to_ooo_sqCancelCnt;
        //    mon_tr.io_mem_to_ooo_sqDeq = io_mem_to_ooo_sqDeq;
        //    mon_tr.io_mem_to_ooo_lqDeq = io_mem_to_ooo_lqDeq;
        //    mon_tr.io_mem_to_ooo_lqDeqPtr_flag = io_mem_to_ooo_lqDeqPtr_flag;
        //    mon_tr.io_mem_to_ooo_lqDeqPtr_value = io_mem_to_ooo_lqDeqPtr_value;
        //    mon_tr.io_mem_to_ooo_memoryViolation_valid = io_mem_to_ooo_memoryViolation_valid;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag = io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value = io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_ftqOffset = io_mem_to_ooo_memoryViolation_bits_ftqOffset;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_isRVC = io_mem_to_ooo_memoryViolation_bits_isRVC;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_level = io_mem_to_ooo_memoryViolation_bits_level;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_robIdx_flag = io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_memoryViolation_bits_robIdx_value = io_mem_to_ooo_memoryViolation_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_sbIsEmpty = io_mem_to_ooo_sbIsEmpty;
        //    mon_tr.io_mem_to_ooo_lsqio_vaddr = io_mem_to_ooo_lsqio_vaddr;
        //    mon_tr.io_mem_to_ooo_lsqio_gpaddr = io_mem_to_ooo_lsqio_gpaddr;
        //    mon_tr.io_mem_to_ooo_lsqio_isForVSnonLeafPTE = io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
        //    mon_tr.io_mem_to_ooo_ldCancel_0_ld2Cancel = io_mem_to_ooo_ldCancel_0_ld2Cancel;
        //    mon_tr.io_mem_to_ooo_ldCancel_1_ld2Cancel = io_mem_to_ooo_ldCancel_1_ld2Cancel;
        //    mon_tr.io_mem_to_ooo_ldCancel_2_ld2Cancel = io_mem_to_ooo_ldCancel_2_ld2Cancel;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
