//=========================================================
// Event-triggered dynamic CSR child sequence
//=========================================================
`ifndef MEMBLOCK_DYNAMIC_CSR_CHANGE_SEQUENCE__SV
`define MEMBLOCK_DYNAMIC_CSR_CHANGE_SEQUENCE__SV

// 中文注释：仅运输原CSR owner预约的真实HS fence，不另建控制标记或完成真源。
class memblock_csr_mapping_fence_sequence extends uvm_sequence #(fence_agent_agent_xaction);
    fence_agent_agent_xaction fence_item;
    function new(string name = "memblock_csr_mapping_fence_sequence");
        super.new(name);
    endfunction:new
    virtual task body();
        if (fence_item == null) begin
            `uvm_fatal("CSR_MAPPING_FENCE", "mapping fence transport requires an item")
            return;
        end
        start_item(fence_item);
        finish_item(fence_item);
    endtask:body
endclass:memblock_csr_mapping_fence_sequence

// 中文注释：B1限定路径的CSR运输；只负责正常UVM握手，监督与失败出口属于原dynamic child。
class memblock_csr_mapping_write_sequence extends uvm_sequence #(csr_ctrl_agent_agent_xaction);
    csr_ctrl_agent_agent_xaction write_item;
    function new(string name = "memblock_csr_mapping_write_sequence");
        super.new(name);
    endfunction:new
    virtual task body();
        start_item(write_item);
        finish_item(write_item);
    endtask:body
endclass:memblock_csr_mapping_write_sequence

class memblock_dynamic_csr_change_sequence extends uvm_sequence #(csr_ctrl_agent_agent_xaction);
    memblock_csr_control_action_t action;
    bit action_valid;
    // 中文注释：VSEQ经CSR worker显式注入，只有映射改变才使用；不查找隐藏层次。
    fence_agent_agent_sequencer mapping_fence_sqr;
    // 中文注释：最多六步的局部维护列表；WRITE_TARGET唯一，fence沿用原owner顺序完成。
    typedef enum int {WRITE_TARGET, FENCE_HS, FENCE_VS, FENCE_G} maintenance_step_e;
    maintenance_step_e maintenance_steps[$];
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

    static function bit hs_mapping_fence_required(
        input csr_ctrl_agent_agent_xaction old_level,
        input csr_ctrl_agent_agent_xaction target);
        if (old_level == null || target == null) return 1'b0;
        return (old_level.io_ooo_to_mem_tlbCsr_satp_mode != target.io_ooo_to_mem_tlbCsr_satp_mode ||
                old_level.io_ooo_to_mem_tlbCsr_satp_ppn != target.io_ooo_to_mem_tlbCsr_satp_ppn) &&
               (old_level.io_ooo_to_mem_tlbCsr_satp_mode != 0 ||
                target.io_ooo_to_mem_tlbCsr_satp_mode != 0);
    endfunction:hs_mapping_fence_required

    virtual function bit use_manual_target();
        return 1'b0;
    endfunction:use_manual_target

    // 中文注释：默认保留候选；手工PMM派生类只修改target并使用同一生效流程。
    virtual function void customize_target(input csr_ctrl_agent_agent_xaction committed,
                                           input csr_ctrl_agent_agent_xaction target);
    endfunction:customize_target

    // 抽象职责：按旧/新完整配置决定有界维护顺序，不修改实际CSR或软件映射。
    function void build_mapping_maintenance_steps(
        input csr_ctrl_agent_agent_xaction old_level,
        input csr_ctrl_agent_agent_xaction target);
        bit guest_changed, guest_present, pmm_width;
        guest_present = old_level.io_ooo_to_mem_tlbCsr_vsatp_mode != 0 ||
            target.io_ooo_to_mem_tlbCsr_vsatp_mode != 0 ||
            old_level.io_ooo_to_mem_tlbCsr_hgatp_mode != 0 ||
            target.io_ooo_to_mem_tlbCsr_hgatp_mode != 0;
        guest_changed = guest_present && (
            old_level.io_ooo_to_mem_tlbCsr_vsatp_mode != target.io_ooo_to_mem_tlbCsr_vsatp_mode ||
            old_level.io_ooo_to_mem_tlbCsr_vsatp_ppn != target.io_ooo_to_mem_tlbCsr_vsatp_ppn ||
            old_level.io_ooo_to_mem_tlbCsr_vsatp_asid != target.io_ooo_to_mem_tlbCsr_vsatp_asid ||
            old_level.io_ooo_to_mem_tlbCsr_hgatp_mode != target.io_ooo_to_mem_tlbCsr_hgatp_mode ||
            old_level.io_ooo_to_mem_tlbCsr_hgatp_ppn != target.io_ooo_to_mem_tlbCsr_hgatp_ppn ||
            old_level.io_ooo_to_mem_tlbCsr_hgatp_vmid != target.io_ooo_to_mem_tlbCsr_hgatp_vmid ||
            old_level.io_ooo_to_mem_tlbCsr_priv_virt != target.io_ooo_to_mem_tlbCsr_priv_virt);
        pmm_width = (old_level.io_ooo_to_mem_tlbCsr_pmm_henvcfg == 2'b11) !=
                    (target.io_ooo_to_mem_tlbCsr_pmm_henvcfg == 2'b11);
        pmm_width = pmm_width && (old_level.io_ooo_to_mem_tlbCsr_hgatp_mode == 4'd9 ||
                     target.io_ooo_to_mem_tlbCsr_hgatp_mode == 4'd9);
        maintenance_steps.delete();
        if (guest_changed) maintenance_steps.push_back(FENCE_VS);
        maintenance_steps.push_back(WRITE_TARGET);
        // 中文注释：SFENCE在dvirt=1下解释为VS维护；HS不可达期间不发送伪HS事件。
        // 每次Guest→Host均补全范围HS维护，覆盖Guest期间被修改的inactive SATP。
        if (!target.io_ooo_to_mem_tlbCsr_priv_virt &&
            (hs_mapping_fence_required(old_level,target) ||
             (old_level.io_ooo_to_mem_tlbCsr_priv_virt &&
              (old_level.io_ooo_to_mem_tlbCsr_satp_mode != 0 ||
               target.io_ooo_to_mem_tlbCsr_satp_mode != 0))))
            maintenance_steps.push_back(FENCE_HS);
        if (guest_changed) maintenance_steps.push_back(FENCE_VS);
        if (guest_changed || pmm_width) maintenance_steps.push_back(FENCE_G);
    endfunction:build_mapping_maintenance_steps

    function memblock_sfence_payload_t make_mapping_fence_payload(input maintenance_step_e step);
        memblock_sfence_payload_t payload;
        payload = '{default:'0};
        payload.valid = 1'b1;
        payload.ignore_addr = 1'b1;
        payload.ignore_id = 1'b1;
        payload.flushPipe = 1'b1;
        case (step)
            FENCE_HS: payload.target_stage = MEMBLOCK_SFENCE_TARGET_HS_S1;
            FENCE_VS: begin
                payload.target_stage = MEMBLOCK_SFENCE_TARGET_VS_S1;
                payload.hv = 1'b1;
            end
            FENCE_G: begin
                payload.target_stage = MEMBLOCK_SFENCE_TARGET_G_S2;
                payload.hg = 1'b1;
            end
            default: `uvm_fatal("CSR_MAPPING_FENCE", "write step has no fence payload")
        endcase
        return payload;
    endfunction:make_mapping_fence_payload

    // 中文注释：只读检查原action归属，不把reset后的当前epoch重新绑定给旧action。
    function bit mapping_action_current(input common_data_transaction data,
                                        input longint unsigned action_l2_reset_epoch);
        status_transaction status;
        if (data == null || !action.owner.valid || !data.is_valid_uid(action.owner.uid) ||
            !data.active_control_barrier_valid || data.active_control_barrier_uid != action.owner.uid ||
            !memblock_sync_pkg::control_runtime_ready_for_current_epoch() ||
            action.control_reset_epoch == 0 ||
            action.control_reset_epoch != memblock_sync_pkg::get_control_runtime_reset_epoch() ||
            memblock_sync_pkg::l2tlb_reset_active() ||
            action_l2_reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch()) return 1'b0;
        status = data.get_status(action.owner.uid);
        return memblock_control_owner_equal(status.control_owner, action.owner) &&
            status.control_kind == MEMBLOCK_CONTROL_KIND_CSR &&
            status.control_state == MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING &&
            status.control_action_enqueued && status.control_reset_epoch == action.control_reset_epoch;
    endfunction:mapping_action_current

    // 中文注释：只为需要mapping fence的special路径监督target/PMP握手。
    // kill只停止后续sequence动作，不能撤销driver已接收的写；失败后本action不再提交。
    task drive_mapping_csr_item(input common_data_transaction data,
                               input csr_ctrl_agent_agent_xaction tr,
                               input longint unsigned action_l2_reset_epoch,
                               output bit completed_ok);
        memblock_csr_mapping_write_sequence write_seq;
        bit transport_done;
        bit transport_failed;
        int unsigned waited;
        completed_ok = 1'b0;
        if (!mapping_action_current(data, action_l2_reset_epoch)) begin
            `uvm_fatal("CSR_MAPPING_FENCE", "stale owner/reset before mapping CSR write")
            return;
        end
        write_seq = new("csr_mapping_write_transport");
        write_seq.write_item = tr;
        transport_done = 1'b0;
        transport_failed = 1'b0;
        waited = 0;
        fork : mapping_write_transport_guard
            begin
                write_seq.start(m_sequencer, this);
                transport_done = 1'b1;
            end
            begin
                while (!transport_done) begin
                    #1ns;
                    if (transport_done) break;
                    if (!mapping_action_current(data, action_l2_reset_epoch) ||
                        waited++ >= seq_csr_common::get_active_seq_no_progress_warn_cycles()) begin
                        transport_failed = 1'b1;
                        `uvm_fatal("CSR_MAPPING_FENCE", "mapping CSR write owner/reset/watchdog failure")
                        break;
                    end
                end
            end
        join_any
        disable mapping_write_transport_guard;
        if (transport_failed || !transport_done) begin
            write_seq.kill();
            return;
        end
        completed_ok = mapping_action_current(data, action_l2_reset_epoch);
        if (!completed_ok)
            `uvm_fatal("CSR_MAPPING_FENCE", "owner/reset changed at mapping CSR write completion")
    endtask:drive_mapping_csr_item

    // 抽象职责：只消费monitor实际样本，分别确认changed事件和连续四拍clean传播。
    task wait_target_propagation(input common_data_transaction data,
        input csr_ctrl_agent_agent_xaction target, input csr_ctrl_agent_agent_xaction clean,
        input longint unsigned baseline_sample, input longint unsigned reset_epoch,
        output bit completed_ok);
        memblock_sync_pkg::memblock_csr_full_snapshot_t observed;
        memblock_sync_pkg::dispatch_raw_csr_t runtime_level, expected;
        longint unsigned last_sample, begin_sample;
        int unsigned stable_count, runtime_seq;
        bit changed_observed;
        completed_ok = 0;
        last_sample = 0;
        stable_count = 0;
        changed_observed = 0;
        begin_sample = memblock_sync_pkg::peek_current_dut_global_sample();
        expected = memblock_csr_config_state::make_runtime_payload(target);
        forever begin
            if (!mapping_action_current(data,reset_epoch)) begin
                `uvm_fatal("CSR_MAPPING_FENCE", "owner/reset invalid during CSR propagation")
                return;
            end
            if (memblock_sync_pkg::get_latest_runtime_csr_snapshot(runtime_level,runtime_seq) &&
                runtime_seq > action.runtime_snapshot_seq_before_drive &&
                runtime_level.satp_changed && runtime_level.vsatp_changed &&
                runtime_level.hgatp_changed && runtime_level.priv_virt_changed &&
                !memblock_sync_pkg::raw_csr_payload_changed(expected,runtime_level))
                changed_observed = 1;
            if (memblock_sync_pkg::get_latest_csr_full_snapshot(observed) &&
                observed.sample_seq > baseline_sample && observed.sample_seq != last_sample) begin
                if (observed.payload == memblock_csr_config_state::pack_full_payload(clean))
                    stable_count = observed.sample_seq == last_sample + 1 ? stable_count + 1 : 1;
                else stable_count = 0;
                last_sample = observed.sample_seq;
            end
            if (changed_observed && stable_count >= 4) begin
                completed_ok = 1;
                return;
            end
            if (memblock_sync_pkg::peek_current_dut_global_sample() - begin_sample >
                    seq_csr_common::get_active_seq_no_progress_warn_cycles()) begin
                `uvm_fatal("CSR_MAPPING_FENCE", "changed event or four clean samples missing")
                return;
            end
            #1ns;
        end
    endtask:wait_target_propagation

    // 抽象职责：在同一CSR owner下运输一次确定的全范围维护，并等待同event的C0/C4。
    task complete_mapping_fence(input common_data_transaction data,
        input memblock_sfence_payload_t payload,
        input longint unsigned action_l2_reset_epoch, output bit completed_ok);
        memblock_control_sfence_observation_t c0, effective;
        fence_agent_agent_xaction fence_tr;
        memblock_csr_mapping_fence_sequence fence_seq;
        longint unsigned reset_epoch;
        int unsigned waited;
        bit transport_done, transport_failed;
        completed_ok = 1'b0;
        reset_epoch = action_l2_reset_epoch;
        if (mapping_fence_sqr == null || !mapping_action_current(data,reset_epoch)) begin
            `uvm_fatal("CSR_MAPPING_FENCE", "mapping fence requires current owner and explicit sequencer")
            return;
        end
        if (!data.arm_csr_mapping_fence(action.owner, payload)) return;
        fence_tr = fence_agent_agent_xaction::type_id::create("csr_hs_mapping_fence");
        fence_tr.io_ooo_to_mem_sfence_valid = 1'b1;
        fence_tr.io_ooo_to_mem_sfence_bits_rs1 = 1'b1;
        fence_tr.io_ooo_to_mem_sfence_bits_rs2 = 1'b1;
        fence_tr.io_ooo_to_mem_sfence_bits_addr = '0;
        fence_tr.io_ooo_to_mem_sfence_bits_id = '0;
        fence_tr.io_ooo_to_mem_sfence_bits_hv = payload.hv;
        fence_tr.io_ooo_to_mem_sfence_bits_hg = payload.hg;
        fence_tr.io_ooo_to_mem_sfence_bits_flushPipe = 1'b1;
        fence_tr.pre_pkt_gap = 0;
        fence_tr.post_pkt_gap = 0;
        fence_seq = new("csr_hs_mapping_fence_transport");
        fence_seq.fence_item = fence_tr;
        transport_done = 1'b0;
        transport_failed = 1'b0;
        waited = 0;
        fork : mapping_fence_transport_guard
            begin
                fence_seq.start(mapping_fence_sqr, this);
                transport_done = 1'b1;
            end
            begin
                while (!transport_done) begin
                    #1ns;
                    if (transport_done) break;
                    if (!mapping_action_current(data, reset_epoch) ||
                        waited++ >= seq_csr_common::get_active_seq_no_progress_warn_cycles()) begin
                        transport_failed = 1'b1;
                        `uvm_fatal("CSR_MAPPING_FENCE", "mapping fence transport owner/reset/watchdog failure")
                        break;
                    end
                end
            end
        join_any
        disable mapping_fence_transport_guard;
        if (transport_failed || !transport_done) begin
            fence_seq.kill();
            return;
        end
        waited = 0;
        forever begin
            if (!mapping_action_current(data, reset_epoch)) begin
                `uvm_fatal("CSR_MAPPING_FENCE", "owner/reset changed while waiting for fence C4")
                return;
            end
            if (data.get_control_sfence_c0_observation(action.owner, c0) &&
                data.finish_csr_mapping_fence(action.owner, c0.lifecycle_event_seq,
                                             reset_epoch, effective)) break;
            if (waited++ >= seq_csr_common::get_active_seq_no_progress_warn_cycles()) begin
                `uvm_fatal("CSR_MAPPING_FENCE", "mapping fence C0/C4 watchdog expired")
                return;
            end
            #1ns;
        end
        `uvm_info("CSR_MAPPING_FENCE",
            $sformatf("uid=%0d C4 complete event=%0d reset=%0d anchor=%0d due=%0d before CSR sendover",
                action.owner.uid, effective.lifecycle_event_seq, effective.reset_epoch,
                effective.anchor_sample_seq, effective.due_sample_seq), UVM_LOW)
        completed_ok = 1'b1;
    endtask:complete_mapping_fence

    task drive_write_plan(input common_data_transaction data,
                          input csr_ctrl_agent_agent_xaction level,
                          input memblock_csr_pmp_pma_profile_t profile,
                          output longint unsigned expected_count,
                          input bit mapping_guard,
                          input longint unsigned action_l2_reset_epoch,
                          output bit completed_ok);
        memblock_csr_pmp_pma_write_plan plan;
        longint unsigned beat_count64;
        completed_ok = 1'b0;
        plan = memblock_csr_pmp_pma_write_plan::type_id::create("dynamic_pmp_pma_plan");
        plan.build(profile, data.pma_pmp_model);
        beat_count64 = $unsigned(plan.beats.size());
        if (!memblock_sync_pkg::pma_pmp_csr_epoch_is_current(
                data.pma_pmp_csr_replay_reset_epoch) ||
            !memblock_sync_pkg::checked_pma_pmp_csr_write_count_add(
                memblock_sync_pkg::raw_pma_pmp_csr_write_produced_count,
                beat_count64, expected_count)) begin
            `uvm_fatal(get_type_name(), "invalid dynamic PMA/PMP epoch or expected-count overflow")
        end
        foreach (plan.beats[i]) begin
            csr_ctrl_agent_agent_xaction write_tr;
            bit write_completed;
            write_tr = csr_ctrl_agent_agent_xaction::type_id::create($sformatf("dynamic_csr_write_%0d", i));
            write_tr.copy(level);
            memblock_csr_config_state::clear_protocol_metadata(write_tr);
            write_tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b1;
            write_tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = plan.beats[i].addr;
            write_tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = plan.beats[i].data;
            write_tr.post_pkt_gap = 1;
            if (mapping_guard) begin
                drive_mapping_csr_item(data, write_tr, action_l2_reset_epoch, write_completed);
                if (!write_completed) return;
            end else begin
                start_item(write_tr);
                finish_item(write_tr);
            end
        end
        completed_ok = 1'b1;
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
        tr.io_ooo_to_mem_tlbCsr_priv_spvp = action.csr_baseline.priv_spvp;
        tr.io_ooo_to_mem_tlbCsr_pmm_mseccfg = action.csr_baseline.pmm_mseccfg;
        tr.io_ooo_to_mem_tlbCsr_pmm_menvcfg = action.csr_baseline.pmm_menvcfg;
        tr.io_ooo_to_mem_tlbCsr_pmm_henvcfg = action.csr_baseline.pmm_henvcfg;
        tr.io_ooo_to_mem_tlbCsr_pmm_hstatus = action.csr_baseline.pmm_hstatus;
        tr.io_ooo_to_mem_tlbCsr_pmm_senvcfg = action.csr_baseline.pmm_senvcfg;
        tr.io_ooo_to_mem_tlbCsr_priv_virt = action.csr_baseline.priv_virt;
        tr.io_ooo_to_mem_tlbCsr_priv_imode = action.csr_baseline.priv_imode;
        tr.io_ooo_to_mem_tlbCsr_priv_dmode = action.csr_baseline.priv_dmode;
        tr.io_ooo_to_mem_tlbCsr_mPBMTE = action.csr_baseline.m_pbmt_en;
        tr.io_ooo_to_mem_tlbCsr_hPBMTE = action.csr_baseline.h_pbmt_en;
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
        begin
            csr_ctrl_agent_agent_xaction clean;
            longint unsigned sample_before;
            longint unsigned reset_epoch;
            bit propagated;
            reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
            if (!mapping_action_current(data,reset_epoch))
                `uvm_fatal(get_type_name(), "legacy CSR owner/reset is stale")
            sample_before = memblock_sync_pkg::peek_current_dut_global_sample();
            start_item(tr);
            finish_item(tr);
            clean = csr_ctrl_agent_agent_xaction::type_id::create("legacy_csr_clean");
            clean.copy(tr);
            memblock_csr_config_state::clear_protocol_metadata(clean);
            wait_target_propagation(data,tr,clean,sample_before,reset_epoch,propagated);
            if (!propagated) return;
        end
        data.mark_csr_control_sendover(action);
    endtask:run_legacy

    virtual task body();
        common_data_transaction data;
        csr_ctrl_agent_agent_xaction committed, target, clean;
        memblock_csr_pmp_pma_profile_t current_region, target_region;
        memblock_csr_sequence_cfg_t cfg;
        memblock_csr_randomizer randomizer;
        memblock_sync_pkg::dispatch_raw_csr_t ignored;
        longint unsigned baseline_sample, expected_write_count, action_l2_reset_epoch;
        longint unsigned wait_start_sample;
        bit step_ok, pmp_required;

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
        target_region = current_region;
        cfg = seq_csr_common::get_csr_sequence_cfg();
        randomizer = memblock_csr_randomizer::type_id::create("dynamic_csr_randomizer");
        if (!use_manual_target()) begin
            randomizer.configure(cfg,1'b1,committed,current_region);
            if (!randomizer.randomize())
                `uvm_fatal(get_type_name(), "dynamic CSR configuration has no non-current legal solution")
            randomizer.apply_to_transaction(target);
            if (cfg.change_pmp_pma_enable)
                target_region = randomizer.make_region_profile(seq_csr_common::get_paddr_base(),
                                                               seq_csr_common::get_paddr_range());
        end
        customize_target(committed,target);
        memblock_csr_config_state::check_target(target);
        memblock_csr_config_state::clear_protocol_metadata(target);
        pmp_required = target_region != current_region;
        if (!memblock_csr_config_state::differs_from(committed,target) && !pmp_required)
            `uvm_fatal(get_type_name(), "dynamic CSR target has no level or region change")
        randomizer.log_selected_profile("dynamic", $sformatf("uid%0d",action.owner.uid),
                                        target,target_region,1'b1);
        build_mapping_maintenance_steps(committed,target);
        if (maintenance_steps.size() > 1 && mapping_fence_sqr == null)
            `uvm_fatal("CSR_MAPPING_FENCE", "mapping maintenance requires explicit fence sequencer")
        action_l2_reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        wait_start_sample = memblock_sync_pkg::peek_current_dut_global_sample();
        // 中文注释：旧访问/地址记录排空前不产生任何CSR或fence副作用；公共服务并行推进。
        while (!data.csr_old_context_drained(action.owner)) begin
            if (!mapping_action_current(data,action_l2_reset_epoch))
                `uvm_fatal("CSR_MAPPING_FENCE", "owner/reset invalid while draining old context")
            if (memblock_sync_pkg::peek_current_dut_global_sample() - wait_start_sample >
                    seq_csr_common::get_active_seq_no_progress_warn_cycles())
                `uvm_fatal("CSR_MAPPING_FENCE", "old context drain watchdog expired")
            #1ns;
        end
        clean = csr_ctrl_agent_agent_xaction::type_id::create("dynamic_csr_committed");
        clean.copy(target);
        target.io_ooo_to_mem_tlbCsr_satp_changed = 1'b1;
        target.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b1;
        target.io_ooo_to_mem_tlbCsr_hgatp_changed = 1'b1;
        target.io_ooo_to_mem_tlbCsr_priv_virt_changed = 1'b1;
        expected_write_count = memblock_sync_pkg::raw_pma_pmp_csr_write_produced_count;
        baseline_sample = 0;
        foreach (maintenance_steps[i]) begin
            if (!mapping_action_current(data,action_l2_reset_epoch))
                `uvm_fatal("CSR_MAPPING_FENCE", "owner/reset invalid before maintenance step")
            if (maintenance_steps[i] == WRITE_TARGET) begin
                if (!memblock_sync_pkg::get_latest_runtime_csr_snapshot(ignored,
                        action.runtime_snapshot_seq_before_drive))
                    `uvm_fatal(get_type_name(), "dynamic CSR action lost runtime baseline")
                baseline_sample = memblock_sync_pkg::peek_current_dut_global_sample();
                // 中文注释：无论PMP是否有beat，始终先运输完整目标CSR level和changed。
                drive_mapping_csr_item(data,target,action_l2_reset_epoch,step_ok);
                if (!step_ok) return;
                if (pmp_required) begin
                    drive_write_plan(data,target,target_region,expected_write_count,1'b1,
                                     action_l2_reset_epoch,step_ok);
                    if (!step_ok) return;
                    begin
                        memblock_csr_pmp_pma_write_plan observed_plan;
                        observed_plan = memblock_csr_pmp_pma_write_plan::type_id::create("dynamic_observed_plan");
                        wait_start_sample = memblock_sync_pkg::peek_current_dut_global_sample();
                        forever begin
                            if (!mapping_action_current(data,action_l2_reset_epoch) ||
                                !data.service_csr_pmp_pma_write_observations()) return;
                            if (data.csr_pmp_pma_writes_observed(expected_write_count) &&
                                observed_plan.model_matches(data.pma_pmp_model,target_region)) break;
                            if (memblock_sync_pkg::peek_current_dut_global_sample() - wait_start_sample >
                                    seq_csr_common::get_active_seq_no_progress_warn_cycles())
                                `uvm_fatal("CSR_MAPPING_FENCE", "PMP/PMA target observation watchdog expired")
                            #1ns;
                        end
                    end
                end
                wait_target_propagation(data,target,clean,baseline_sample,
                                        action_l2_reset_epoch,step_ok);
            end else begin
                complete_mapping_fence(data,make_mapping_fence_payload(maintenance_steps[i]),
                                       action_l2_reset_epoch,step_ok);
            end
            if (!step_ok) return;
        end
        action.expected_runtime_csr = memblock_csr_config_state::make_runtime_payload(target);
        action.expected_runtime_csr_valid = 1'b1;
        data.stage_csr_dynamic_candidate(action.owner,clean,target_region,baseline_sample,
                                         pmp_required,expected_write_count);
        data.mark_csr_control_sendover(action);
        `uvm_info(get_type_name(),$sformatf("dynamic CSR candidate staged uid=%0d generation=%0d steps=%0d",
            action.owner.uid,action.owner.action_generation,maintenance_steps.size()),UVM_LOW)
    endtask:body
endclass:memblock_dynamic_csr_change_sequence

`endif
