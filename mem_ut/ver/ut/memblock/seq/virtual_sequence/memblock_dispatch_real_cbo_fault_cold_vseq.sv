`ifndef MEMBLOCK_DISPATCH_REAL_CBO_FAULT_COLD_VSEQ_SV
`define MEMBLOCK_DISPATCH_REAL_CBO_FAULT_COLD_VSEQ_SV
// 中文注释：唯一场景变化是建表和收尾覆盖检查；所有真实worker由父类持有。
class memblock_dispatch_real_cbo_fault_cold_vseq extends memblock_dispatch_real_smoke_vseq;
    `uvm_object_utils(memblock_dispatch_real_cbo_fault_cold_vseq)
    function new(string name="memblock_dispatch_real_cbo_fault_cold_vseq"); super.new(name); endfunction
    virtual task start_main_dispatch_sequence();
        memblock_main_dispatch_cbo_fault_cold_sequence main_seq;
        `uvm_do_on(main_seq,p_sequencer)
    endtask
    // 抽象职责：场景真实完成后独立断言目标行首访为CBOAck且实际DUT进入所配置fault，防空覆盖通过。
    virtual task body();
        common_data_transaction data;
        status_transaction status_v;
        mem_access_base_sequence::l2_d_error_line_record_t choice;
        mem_access_base_sequence::l2_d_error_state_e expected_choice;
        bit [23:0] expected_exception;
        int denied_wt,corrupt_wt;
        bit covered;
        seq_csr_common::init();
        denied_wt=seq_csr_common::get_l2_cbo_ack_denied_wt();
        corrupt_wt=seq_csr_common::get_l2_cbo_ack_corrupt_wt();
        if(denied_wt==100 && corrupt_wt==0) begin
            expected_choice=mem_access_base_sequence::L2_D_ERROR_DENIED; expected_exception=24'h80;
        end else if(denied_wt==0 && corrupt_wt==100) begin
            expected_choice=mem_access_base_sequence::L2_D_ERROR_CORRUPT; expected_exception=24'h080000;
        end else begin
            `uvm_fatal(get_type_name(),"cold CMO fault coverage requires exactly denied100/corrupt0 or denied0/corrupt100")
        end
        super.body();
        data=common_data_transaction::get();
        status_v=data.get_status(0); covered=1;
        if(status_v==null || !status_v.sta_fault || !status_v.terminal_done ||
           status_v.exception_vec!==expected_exception) begin
            covered=0;
            `uvm_error("CMO_COLD_FAULT_COVERAGE",$sformatf(
                "cold target未命中实际终态fault expected=0x%0h",expected_exception))
        end
        if(!mem_access_base_sequence::query_l2_line_history_at_sample(48'h8000_2000,
            memblock_sync_pkg::peek_current_dut_global_sample(),choice) || !choice.valid ||
            choice.first_kind!=3 || choice.state!=expected_choice) begin
            covered=0;
            `uvm_error("CMO_COLD_FAULT_COVERAGE",$sformatf(
                "cold target首访不是实际CBOAck或未命中配置error first_kind=%0d state=%0d expected=%0d",
                choice.first_kind,choice.state,expected_choice))
        end
        if(covered)
            `uvm_info("CMO_COLD_FAULT_COVERAGE",$sformatf(
                "实际CBOAck首访注错并完成DUT fault state=%0d expected_exception=0x%0h",choice.state,expected_exception),UVM_LOW)
    endtask
endclass

// 中文注释：独立CMO故障验收只替换建表；复用父body的真实fault和首次CBOAck注错覆盖断言。
class memblock_dispatch_real_cbo_fault_cold_isolated_vseq extends memblock_dispatch_real_cbo_fault_cold_vseq;
    `uvm_object_utils(memblock_dispatch_real_cbo_fault_cold_isolated_vseq)
    function new(string name="memblock_dispatch_real_cbo_fault_cold_isolated_vseq"); super.new(name); endfunction
    // 抽象职责：在父VSEQ现有worker生命周期内启动隔离建表，不改变错误响应或恢复检查。
    virtual task start_main_dispatch_sequence();
        memblock_main_dispatch_cbo_fault_cold_isolated_sequence main_seq;
        `uvm_do_on(main_seq,p_sequencer)
    endtask
endclass
`endif
