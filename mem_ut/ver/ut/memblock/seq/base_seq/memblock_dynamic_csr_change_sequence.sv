//=========================================================
// Event-triggered dynamic CSR child sequence
//=========================================================
`ifndef MEMBLOCK_DYNAMIC_CSR_CHANGE_SEQUENCE__SV
`define MEMBLOCK_DYNAMIC_CSR_CHANGE_SEQUENCE__SV

class memblock_dynamic_csr_change_sequence extends uvm_sequence #(csr_ctrl_agent_agent_xaction);
    memblock_csr_control_action_t action;
    bit action_valid;
    `uvm_object_utils(memblock_dynamic_csr_change_sequence)

    function new(string name = "memblock_dynamic_csr_change_sequence");
        super.new(name);
        action = '{default:'0};
        action_valid = 1'b0;
    endfunction:new

    function void set_action(input memblock_csr_control_action_t action_i);
        action = action_i;
        action_valid = 1'b1;
    endfunction:set_action

    task drive_write_plan(input common_data_transaction data,
                          input csr_ctrl_agent_agent_xaction level,
                          input memblock_csr_pmp_pma_profile_t profile);
        memblock_csr_pmp_pma_write_plan plan;
        plan = memblock_csr_pmp_pma_write_plan::type_id::create("dynamic_pmp_pma_plan");
        plan.build(profile, data.pma_pmp_model);
        foreach (plan.beats[i]) begin
            csr_ctrl_agent_agent_xaction write_tr;
            write_tr = csr_ctrl_agent_agent_xaction::type_id::create($sformatf("dynamic_csr_write_%0d", i));
            write_tr.copy(level);
            memblock_csr_config_state::clear_protocol_metadata(write_tr);
            write_tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b1;
            write_tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = plan.beats[i].addr;
            write_tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = plan.beats[i].data;
            write_tr.post_pkt_gap = 1;
            start_item(write_tr);
            finish_item(write_tr);
        end
    endtask:drive_write_plan

    task run_legacy(input common_data_transaction data);
        csr_ctrl_agent_agent_xaction tr;
        memblock_sync_pkg::dispatch_raw_csr_t ignored;
        if (!action.csr_baseline_valid)
            `uvm_fatal(get_type_name(), "legacy dynamic action has no CSR baseline")
        tr = csr_ctrl_agent_agent_xaction::type_id::create("legacy_dynamic_csr");
        // 兼容路径必须保留旧 transaction::new() 的零值基线；新的专项静态默认值
        // 不能反向改变非专项场景的 CSR 行为。
        memblock_csr_config_state::clear_protocol_metadata(tr);
        tr.io_ooo_to_mem_tlbCsr_satp_mode = action.csr_baseline.satp_mode;
        tr.io_ooo_to_mem_tlbCsr_satp_asid = action.csr_baseline.satp_asid + 16'h1;
        tr.io_ooo_to_mem_tlbCsr_satp_ppn = action.csr_baseline.satp_ppn;
        tr.io_ooo_to_mem_tlbCsr_vsatp_mode = action.csr_baseline.vsatp_mode;
        tr.io_ooo_to_mem_tlbCsr_vsatp_asid = action.csr_baseline.vsatp_asid;
        tr.io_ooo_to_mem_tlbCsr_vsatp_ppn = action.csr_baseline.vsatp_ppn;
        tr.io_ooo_to_mem_tlbCsr_hgatp_mode = action.csr_baseline.hgatp_mode;
        tr.io_ooo_to_mem_tlbCsr_hgatp_vmid = action.csr_baseline.hgatp_vmid;
        tr.io_ooo_to_mem_tlbCsr_hgatp_ppn = action.csr_baseline.hgatp_ppn;
        tr.io_ooo_to_mem_tlbCsr_priv_mxr = action.csr_baseline.priv_mxr;
        tr.io_ooo_to_mem_tlbCsr_priv_sum = action.csr_baseline.priv_sum;
        tr.io_ooo_to_mem_tlbCsr_priv_vmxr = action.csr_baseline.priv_vmxr;
        tr.io_ooo_to_mem_tlbCsr_priv_vsum = action.csr_baseline.priv_vsum;
        tr.io_ooo_to_mem_tlbCsr_priv_virt = action.csr_baseline.priv_virt;
        tr.io_ooo_to_mem_tlbCsr_priv_imode = action.csr_baseline.priv_imode;
        tr.io_ooo_to_mem_tlbCsr_priv_dmode = action.csr_baseline.priv_dmode;
        tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable = action.csr_baseline.hd_misalign_ld_enable;
        tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable = action.csr_baseline.hd_misalign_st_enable;
        // Keep the legacy action protocol aligned with the dedicated CSR path:
        // every dynamic level update carries the four-field one-cycle pulse.
        tr.io_ooo_to_mem_tlbCsr_satp_changed = 1'b1;
        tr.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b1;
        tr.io_ooo_to_mem_tlbCsr_hgatp_changed = 1'b1;
        tr.io_ooo_to_mem_tlbCsr_priv_virt_changed = 1'b1;
        action.expected_runtime_csr = memblock_csr_config_state::make_runtime_payload(tr);
        action.expected_runtime_csr_valid = 1'b1;
        if (!memblock_sync_pkg::get_latest_runtime_csr_snapshot(ignored,
              action.runtime_snapshot_seq_before_drive))
            `uvm_fatal(get_type_name(), "legacy dynamic action lost runtime baseline")
        start_item(tr);
        finish_item(tr);
        data.mark_csr_control_sendover(action);
    endtask:run_legacy

    virtual task body();
        common_data_transaction data;
        csr_ctrl_agent_agent_xaction committed, target, clean;
        memblock_csr_pmp_pma_profile_t current_region, target_region;
        memblock_csr_sequence_cfg_t cfg;
        memblock_csr_randomizer randomizer;
        memblock_sync_pkg::dispatch_raw_csr_t ignored;
        longint unsigned baseline_sample;

        if (!action_valid || !action.owner.valid ||
            action.completion_profile != MEMBLOCK_CONTROL_COMPLETION_RUNTIME_CSR_SNAPSHOT)
            `uvm_fatal(get_type_name(), "dynamic CSR child started without a valid action")
        seq_csr_common::init();
        data = common_data_transaction::get();
        if (!memblock_sync_pkg::csr_special_sequence_active) begin
            run_legacy(data);
            return;
        end
        if (!data.get_csr_committed_state(committed) ||
            !data.get_csr_pmp_pma_profile(current_region))
            `uvm_fatal(get_type_name(), "dynamic CSR action has no committed startup state")

        target = csr_ctrl_agent_agent_xaction::type_id::create("dynamic_csr_target");
        target.copy(committed);
        cfg = seq_csr_common::get_csr_sequence_cfg();
        randomizer = memblock_csr_randomizer::type_id::create("dynamic_csr_randomizer");
        randomizer.configure(cfg, 1'b1, committed, current_region);
        if (!randomizer.randomize())
            `uvm_fatal(get_type_name(), "dynamic CSR configuration has no non-current legal solution")
        randomizer.apply_to_transaction(target);
        memblock_csr_config_state::clear_protocol_metadata(target);
        // Dynamic CSR level items use one uniform update pulse: all ATP changed
        // fields and the CSR-facing virtualization changed field are asserted
        // together. The driver sanitizes all four fields on the next clock.
        target.io_ooo_to_mem_tlbCsr_satp_changed = 1'b1;
        target.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b1;
        target.io_ooo_to_mem_tlbCsr_hgatp_changed = 1'b1;
        target.io_ooo_to_mem_tlbCsr_priv_virt_changed = 1'b1;
        target_region = randomizer.make_region_profile(seq_csr_common::get_paddr_base(),
                                                       seq_csr_common::get_paddr_range());
        randomizer.log_selected_profile(
            "dynamic",
            $sformatf("uid_%0d_generation_%0d", action.owner.uid,
                      action.owner.action_generation),
            target, target_region, 1'b0);
        if (!memblock_sync_pkg::get_latest_runtime_csr_snapshot(ignored,
              action.runtime_snapshot_seq_before_drive))
            `uvm_fatal(get_type_name(), "dynamic CSR action lost runtime baseline")
        baseline_sample = memblock_sync_pkg::peek_current_dut_global_sample();
        start_item(target);
        finish_item(target);
        if (cfg.change_pmp_pma_enable)
            drive_write_plan(data, target, target_region);
        clean = csr_ctrl_agent_agent_xaction::type_id::create("dynamic_csr_committed");
        clean.copy(target);
        memblock_csr_config_state::clear_protocol_metadata(clean);
        // 中文注释：既有 runtime snapshot 把 changed 当作上升沿事件，下降沿不会
        // 递增 snapshot 序号，因此旧 completion gate 对比 3'b111 的动态 item；
        // candidate 提交仍由 service 另行对比 driver 保持的完整 clean(3'b000) level。
        action.expected_runtime_csr = memblock_csr_config_state::make_runtime_payload(target);
        action.expected_runtime_csr_valid = 1'b1;
        data.stage_csr_dynamic_candidate(action.owner, clean, target_region,
                                         baseline_sample, cfg.change_pmp_pma_enable);
        data.mark_csr_control_sendover(action);
        `uvm_info(get_type_name(),
                  $sformatf("dynamic CSR candidate staged and sent uid=%0d generation=%0d",
                            action.owner.uid, action.owner.action_generation), UVM_LOW)
    endtask:body
endclass:memblock_dynamic_csr_change_sequence

`endif
