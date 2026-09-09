//=========================================================
// Dedicated CSR initial/dynamic configuration scenario
//=========================================================
`ifndef MEMBLOCK_CSR_RANDOM_CONFIG_VSEQ__SV
`define MEMBLOCK_CSR_RANDOM_CONFIG_VSEQ__SV

class memblock_csr_random_config_vseq extends memblock_dispatch_real_smoke_vseq;
    `uvm_object_utils(memblock_csr_random_config_vseq)

    function new(string name = "memblock_csr_random_config_vseq");
        super.new(name);
    endfunction:new

    virtual task body();
        seq_csr_common::init();
        if (memblock_sync_pkg::get_control_worker_topology_mode() !=
            memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_AUTO_MAIN_TABLE)
            `uvm_fatal(get_type_name(), "CSR random config VSEQ requires AUTO control topology")
        if (!seq_csr_common::get_main_mem_ranges_en() ||
            !seq_csr_common::get_pma_pmp_model_en())
            `uvm_fatal(get_type_name(),
                       "CSR random config VSEQ requires main memory ranges and the PMA/PMP model")
        memblock_sync_pkg::csr_special_sequence_active = 1'b1;
        super.body();
        memblock_sync_pkg::csr_special_sequence_active = 1'b0;
    endtask:body

    task wait_for_initial_config(input common_data_transaction data);
        int unsigned wait_count;
        wait_count = 0;
        while (!data.csr_initial_config_done && !data.is_global_stop_requested()) begin
            #1;
            wait_count++;
            if (wait_count > seq_csr_common::get_active_seq_no_progress_warn_cycles())
                `uvm_fatal(get_type_name(), "timed out waiting for initial CSR configuration")
        end
        if (!data.csr_initial_config_done)
            `uvm_fatal(get_type_name(), "CSR scenario stopped before initial configuration")
    endtask:wait_for_initial_config

    virtual task start_core_dispatch_flow();
        common_data_transaction data;
        memblock_lsqenq_dispatch_base_sequence lsqenq_seq;
        memblock_issue_dispatch_base_sequence issue_seq;
        memblock_lsqcommit_dispatch_base_sequence lsqcommit_seq;
        memblock_l2tlb_base_sequence l2tlb_seq;
        memblock_main_dispatch_auto_build_main_table_base_sequence main_seq;
        memblock_csr_initial_config_sequence initial_csr_seq;
        memblock_csr_control_base_sequence csr_control_seq;
        memblock_sfence_control_base_sequence sfence_control_seq;

        data = common_data_transaction::get();
        // 中文注释：main sequence 先建立主表并持续提供 monitor/control service；
        // initial CSR 独占 csr_ctrl_sqr 完成配置后，动态 worker 与全部流量 producer
        // 才能启动，因此初始配置与运行期 CSR 不存在双 producer 窗口。
        fork
            begin
                `uvm_do_on(main_seq, p_sequencer)
            end
            begin
                `uvm_do_on(initial_csr_seq, p_sequencer.csr_ctrl_sqr)
                `uvm_do_on(csr_control_seq, p_sequencer.csr_ctrl_sqr)
            end
            begin
                wait_for_initial_config(data);
                `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
            end
            begin
                wait_for_initial_config(data);
                `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
            end
            begin
                wait_for_initial_config(data);
                `uvm_do_on(lsqcommit_seq, p_sequencer.lsqcommit_sqr)
            end
            begin
                wait_for_initial_config(data);
                `uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr)
            end
            begin
                wait_for_initial_config(data);
                `uvm_do_on(sfence_control_seq, p_sequencer.fence_sqr)
            end
        join
    endtask:start_core_dispatch_flow
endclass:memblock_csr_random_config_vseq

`endif
