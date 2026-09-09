//=========================================================
// Startup-only complete CSR configuration sequence
//=========================================================
`ifndef MEMBLOCK_CSR_INITIAL_CONFIG_SEQUENCE__SV
`define MEMBLOCK_CSR_INITIAL_CONFIG_SEQUENCE__SV

class memblock_csr_initial_config_sequence extends uvm_sequence #(csr_ctrl_agent_agent_xaction);
    `uvm_object_utils(memblock_csr_initial_config_sequence)

    function new(string name = "memblock_csr_initial_config_sequence");
        super.new(name);
    endfunction:new

    task wait_for_full_snapshot(input csr_ctrl_agent_agent_xaction expected,
                                input longint unsigned after_sample);
        memblock_sync_pkg::memblock_csr_full_snapshot_t observed;
        bit [memblock_sync_pkg::MEMBLOCK_CSR_FULL_PAYLOAD_BITS-1:0] payload;
        int unsigned wait_count;
        payload = memblock_csr_config_state::pack_full_payload(expected);
        wait_count = 0;
        forever begin
            if (memblock_sync_pkg::get_latest_csr_full_snapshot(observed) &&
                observed.sample_seq > after_sample && observed.payload == payload) return;
            #1;
            wait_count++;
            if (wait_count > seq_csr_common::get_active_seq_no_progress_warn_cycles())
                `uvm_fatal(get_type_name(), "timed out waiting for complete initial CSR observation")
        end
    endtask:wait_for_full_snapshot

    task drive_write_plan(input common_data_transaction data,
                          input csr_ctrl_agent_agent_xaction level,
                          input memblock_csr_pmp_pma_profile_t profile);
        memblock_csr_pmp_pma_write_plan plan;
        plan = memblock_csr_pmp_pma_write_plan::type_id::create("initial_pmp_pma_plan");
        plan.build(profile, data.pma_pmp_model);
        foreach (plan.beats[i]) begin
            csr_ctrl_agent_agent_xaction write_tr;
            write_tr = csr_ctrl_agent_agent_xaction::type_id::create($sformatf("initial_csr_write_%0d", i));
            write_tr.copy(level);
            memblock_csr_config_state::clear_protocol_metadata(write_tr);
            write_tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b1;
            write_tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = plan.beats[i].addr;
            write_tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = plan.beats[i].data;
            write_tr.post_pkt_gap = 1;
            start_item(write_tr);
            finish_item(write_tr);
        end
        for (int unsigned wait_count = 0;
             !plan.model_matches(data.pma_pmp_model, profile); wait_count++) begin
            data.service_csr_pmp_pma_write_observations();
            #1;
            if (wait_count > seq_csr_common::get_active_seq_no_progress_warn_cycles())
                `uvm_fatal(get_type_name(), "PMP/PMA model did not observe the initial CSR write plan")
        end
    endtask:drive_write_plan

    virtual task body();
        common_data_transaction data;
        memblock_csr_sequence_cfg_t cfg;
        memblock_csr_randomizer randomizer;
        memblock_csr_pmp_pma_profile_t empty_region, profile;
        csr_ctrl_agent_agent_xaction tr;
        longint unsigned baseline_sample;
        int unsigned wait_count;

        seq_csr_common::init();
        data = common_data_transaction::get();
        if (data == null || !memblock_sync_pkg::csr_special_sequence_active)
            `uvm_fatal(get_type_name(), "initial CSR sequence started outside the dedicated CSR scenario")
        wait_count = 0;
        while (!data.main_table_ready ||
               !memblock_sync_pkg::control_runtime_ready_for_current_epoch()) begin
            #1;
            wait_count++;
            if (wait_count > seq_csr_common::get_active_seq_no_progress_warn_cycles())
                `uvm_fatal(get_type_name(), "timed out waiting for CSR startup barriers")
        end

        cfg = seq_csr_common::get_csr_sequence_cfg();
        tr = csr_ctrl_agent_agent_xaction::type_id::create("csr_initial_level");
        memblock_csr_config_state::configure_static_defaults(tr);
        randomizer = memblock_csr_randomizer::type_id::create("csr_initial_randomizer");
        empty_region = '{default:'0};
        randomizer.configure(cfg, 1'b0, null, empty_region);
        if (!randomizer.randomize())
            `uvm_fatal(get_type_name(), "initial CSR configuration has no legal weighted solution")
        randomizer.apply_to_transaction(tr);
        memblock_csr_config_state::clear_protocol_metadata(tr);
        profile = randomizer.make_region_profile(seq_csr_common::get_paddr_base(),
                                                 seq_csr_common::get_paddr_range());
        randomizer.log_selected_profile("initial", "startup_owner", tr, profile, 1'b1);

        baseline_sample = memblock_sync_pkg::peek_current_dut_global_sample();
        start_item(tr);
        finish_item(tr);
        wait_for_full_snapshot(tr, baseline_sample);
        drive_write_plan(data, tr, profile);
        data.publish_csr_committed_state(tr);
        data.publish_csr_pmp_pma_profile(profile);
        data.mark_csr_initial_config_done();
        `uvm_info(get_type_name(), "initial complete CSR configuration committed", UVM_LOW)
    endtask:body
endclass:memblock_csr_initial_config_sequence

`endif
