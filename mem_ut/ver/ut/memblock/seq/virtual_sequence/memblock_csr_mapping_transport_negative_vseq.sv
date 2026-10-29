`ifndef MEMBLOCK_CSR_MAPPING_TRANSPORT_NEGATIVE_VSEQ__SV
`define MEMBLOCK_CSR_MAPPING_TRANSPORT_NEGATIVE_VSEQ__SV

// 中文注释：仅由隔离负例在build期factory替换真实fence driver；reset沿用安全idle。
// main只消费真实sequencer，不驱动VIF，分别制造无C0和无item_done；不用于功能回归。
class memblock_csr_mapping_negative_fence_driver extends fence_agent_agent_driver;
    `uvm_component_utils(memblock_csr_mapping_negative_fence_driver)
    static bit hold_item_done;
    static bit abandon_held_item;
    static bit holding_item;
    static bit quiesced;
    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction:new
    virtual task main_phase(uvm_phase phase);
        `uvm_info("CSR_MAPPING_NEGATIVE_DRIVER", get_type_name(), UVM_LOW)
        forever begin
            seq_item_port.get_next_item(req);
            if (hold_item_done) begin
                holding_item = 1'b1;
                wait(abandon_held_item);
                req = null;
                holding_item = 1'b0;
                quiesced = 1'b1;
                wait(1'b0);
            end
            seq_item_port.item_done();
            #1ns;
        end
    endtask:main_phase
endclass:memblock_csr_mapping_negative_fence_driver

// 中文注释：隔离CSR运输负例的driver；正常startup保留真实驱动/reset ACK，末例故意不item_done。
class memblock_csr_mapping_negative_csr_driver extends csr_ctrl_agent_agent_driver;
    `uvm_component_utils(memblock_csr_mapping_negative_csr_driver)
    static bit hold_item_done;
    static bit abandon_held_item;
    static bit holding_item;
    static bit quiesced;
    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction:new
    virtual task main_phase(uvm_phase phase);
        `uvm_info("CSR_MAPPING_NEGATIVE_DRIVER", get_type_name(), UVM_LOW)
        forever begin
            if (service_control_runtime_reset_request()) begin
                @this.vif.drv_mp.drv_cb;
                drive_idle(this.cfg.drv_mode);
                continue;
            end
            seq_item_port.try_next_item(req);
            if (req == null) begin
                @this.vif.drv_mp.drv_cb;
                drive_idle(this.cfg.drv_mode);
                continue;
            end
            if (hold_item_done) begin
                holding_item = 1'b1;
                wait(abandon_held_item);
                req = null;
                holding_item = 1'b0;
                quiesced = 1'b1;
                wait(1'b0);
            end
            @this.vif.drv_mp.drv_cb;
            send_pkt(req);
            seq_item_port.item_done();
        end
    endtask:main_phase
endclass:memblock_csr_mapping_negative_csr_driver

// 中文注释：独立lock owner不是被测child祖先，确保真实wait_for_grant被阻塞。
class memblock_csr_mapping_blocker_sequence extends uvm_sequence #(fence_agent_agent_xaction);
    bit locked;
    bit release_lock;
    function new(string name = "memblock_csr_mapping_blocker_sequence");
        super.new(name);
    endfunction:new
    virtual task body();
        lock();
        locked = 1'b1;
        wait(release_lock);
        unlock();
        locked = 1'b0;
    endtask:body
endclass:memblock_csr_mapping_blocker_sequence

class memblock_csr_mapping_write_blocker_sequence extends uvm_sequence #(csr_ctrl_agent_agent_xaction);
    bit locked;
    bit release_lock;
    function new(string name = "memblock_csr_mapping_write_blocker_sequence");
        super.new(name);
    endfunction:new
    virtual task body();
        lock();
        locked = 1'b1;
        wait(release_lock);
        unlock();
        locked = 1'b0;
    endtask:body
endclass:memblock_csr_mapping_write_blocker_sequence

// 中文注释：factory替换原CSR VSEQ，沿用basicTest已冻结的active topology。
// 不启动真实traffic；真实monitor提供稳定CSR及reset ACK，禁止临时改topology或伪造ACK。
class memblock_csr_mapping_transport_negative_vseq extends memblock_csr_random_config_vseq;
    `uvm_object_utils(memblock_csr_mapping_transport_negative_vseq)
    function new(string name = "memblock_csr_mapping_transport_negative_vseq");
        super.new(name);
    endfunction:new

    task require_true(input bit condition, input string message);
        if (!condition) `uvm_fatal("CSR_MAPPING_NEGATIVE_TEST", message)
    endtask:require_true

    task wait_current_control_ready();
        memblock_sync_pkg::memblock_control_csr_runtime_baseline_t baseline;
        int waited;
        waited = 0;
        while (!memblock_sync_pkg::get_control_csr_runtime_baseline(baseline)) begin
            require_true(waited++ < 200, "real control reset ACK/baseline watchdog");
            #1ns;
        end
    endtask:wait_current_control_ready

    // 中文注释：control reset会清driver私有hold，每次恢复均重发clean并等待新的真实full92。
    task drive_clean_target(input csr_ctrl_agent_agent_xaction clean,
                            output longint unsigned baseline_sample);
        memblock_csr_mapping_write_sequence startup;
        memblock_sync_pkg::memblock_csr_full_snapshot_t observed;
        int waited;
        baseline_sample = memblock_sync_pkg::peek_current_dut_global_sample();
        startup = new("negative_real_csr_startup");
        startup.write_item = clean;
        startup.start(p_sequencer.csr_ctrl_sqr);
        waited = 0;
        while (!(memblock_sync_pkg::get_latest_csr_full_snapshot(observed) &&
                 observed.sample_seq > baseline_sample &&
                 observed.payload == memblock_csr_config_state::pack_full_payload(clean))) begin
            require_true(waited++ < 200, "negative startup real monitor did not observe clean CSR");
            #1ns;
        end
    endtask:drive_clean_target

    virtual task body();
        common_data_transaction local_data;
        status_transaction status;
        memblock_dynamic_csr_change_sequence child;
        memblock_csr_mapping_blocker_sequence blocker;
        memblock_csr_mapping_write_blocker_sequence write_blocker;
        memblock_csr_control_action_t action;
        memblock_uncache_owner_expected_fatal_catcher catcher;
        csr_ctrl_agent_agent_xaction clean;
        csr_ctrl_agent_agent_xaction expected;
        memblock_sfence_payload_t payload;
        memblock_control_sfence_observation_t c0;
        longint unsigned reset_epoch;
        longint unsigned baseline_sample;
        bit completed_ok;
        bit task_returned;
        int waited;

        require_real_smoke_sqr();
        seq_csr_common::init();
        require_true(memblock_sync_pkg::uses_control_barrier_topology(), "negative test requires active topology");
        memblock_sync_pkg::csr_special_sequence_active = 1'b1;
        memblock_sync_pkg::request_control_runtime_reset("CSR mapping isolated negative bootstrap");
        wait_current_control_ready();
        clean = csr_ctrl_agent_agent_xaction::type_id::create("negative_clean_target");
        memblock_csr_config_state::configure_static_defaults(clean);
        memblock_csr_config_state::clear_protocol_metadata(clean);
        clean.io_ooo_to_mem_tlbCsr_priv_virt = 1'b0;
        drive_clean_target(clean, baseline_sample);

        // 0..5为稳定/缺C0C4/身份；6..9为fence等待身份；10..12为CSR写等待；13/14为两类noack。
        for (int scenario = 0; scenario < 15; scenario++) begin
            bit write_case;
            bit fence_lock_case;
            bit write_lock_case;
            write_case = scenario inside {10,11,12,14};
            fence_lock_case = scenario inside {1,6,7};
            write_lock_case = scenario inside {10,11,12};
            local_data = new($sformatf("negative_local_%0d", scenario));
            local_data.main_trans_num = 2;
            local_data.main_table_by_uid = new[2];
            local_data.status_by_uid = new[2];
            status = local_data.init_status_for_uid(0);
            status.control_kind = MEMBLOCK_CONTROL_KIND_CSR;
            status.control_state = MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING;
            status.control_action_enqueued = 1'b1;
            status.control_reset_epoch = memblock_sync_pkg::get_control_runtime_reset_epoch();
            status.control_owner = '{valid:1'b1, uid:0, dynamic_epoch:0,
                action_generation:1, kind:MEMBLOCK_CONTROL_KIND_CSR};
            local_data.active_control_barrier_valid = 1'b1;
            local_data.active_control_barrier_uid = 0;
            action = '{default:'0};
            action.owner = status.control_owner;
            action.control_reset_epoch = status.control_reset_epoch;
            action.completion_profile = MEMBLOCK_CONTROL_COMPLETION_RUNTIME_CSR_SNAPSHOT;
            child = memblock_dynamic_csr_change_sequence::type_id::create($sformatf("negative_child_%0d", scenario));
            child.set_action(action);
            child.set_sequencer(p_sequencer.csr_ctrl_sqr);
            child.mapping_fence_sqr = p_sequencer.fence_sqr;
            reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
            require_true(child.mapping_action_current(local_data, reset_epoch), "valid original action rejected");
            status.control_reset_epoch++;
            require_true(!child.mapping_action_current(local_data, reset_epoch), "stale status control epoch accepted");
            status.control_reset_epoch--;
            child.action.control_reset_epoch++;
            require_true(!child.mapping_action_current(local_data, reset_epoch), "old action rebound to current reset");
            child.action.control_reset_epoch--;
            require_true(!child.mapping_action_current(local_data, reset_epoch+1), "wrong frozen L2 reset accepted");
            expected = csr_ctrl_agent_agent_xaction::type_id::create("negative_expected");
            expected.copy(clean);
            if (scenario == 0 || scenario == 4 || scenario == 5)
                expected.io_ooo_to_mem_tlbCsr_priv_mxr = !clean.io_ooo_to_mem_tlbCsr_priv_mxr;
            if (fence_lock_case) begin
                blocker = new("negative_independent_lock");
                fork
                    blocker.start(p_sequencer.fence_sqr);
                join_none
                waited = 0;
                while (!blocker.locked) begin
                    require_true(waited++ < 200, "negative blocker lock watchdog");
                    #1ns;
                end
            end
            if (write_lock_case) begin
                write_blocker = new("negative_independent_write_lock");
                fork
                    write_blocker.start(p_sequencer.csr_ctrl_sqr);
                join_none
                waited = 0;
                while (!write_blocker.locked) begin
                    require_true(waited++ < 200, "negative CSR blocker lock watchdog");
                    #1ns;
                end
            end
            memblock_csr_mapping_negative_fence_driver::hold_item_done = scenario == 13;
            memblock_csr_mapping_negative_csr_driver::hold_item_done = scenario == 14;
            catcher = new($sformatf("negative_catcher_%0d", scenario),
                scenario == 0 ? "stable target observation watchdog expired" :
                scenario == 4 || scenario == 5 ? "owner/reset changed while waiting for stable target" :
                write_case ? "mapping CSR write owner/reset/watchdog failure" :
                scenario inside {1,6,7,13} ? "mapping fence transport owner/reset/watchdog failure" :
                scenario inside {8,9} ? "owner/reset changed while waiting for fence C4" :
                "mapping fence C0/C4 watchdog expired");
            uvm_report_cb::add(null, catcher);
            task_returned = 1'b0;
            completed_ok = 1'b1;
            fork : negative_case
                begin
                    if (write_case)
                        child.drive_mapping_csr_item(local_data, clean, reset_epoch, completed_ok);
                    else
                        child.complete_hs_mapping_fence(local_data, expected, baseline_sample,
                                                        reset_epoch, completed_ok);
                    task_returned = 1'b1;
                end
                begin
                    if (scenario == 3) begin
                        wait(status.control_sfence_c0_armed);
                        payload = status.control_expected_sfence;
                        payload.sample_seq = memblock_sync_pkg::peek_current_dut_global_sample();
                        payload.reset_epoch = reset_epoch;
                        memblock_sync_pkg::last_allocated_l2tlb_event_seq++;
                        payload.lifecycle_event_seq = memblock_sync_pkg::last_allocated_l2tlb_event_seq;
                        require_true(local_data.schedule_sfence_invalidate(payload, payload.sample_seq,
                            reset_epoch, payload.lifecycle_event_seq), "negative C0 schedule failed");
                    end else if (scenario inside {4,6,8,11}) begin
                        #5ns;
                        status.control_owner.action_generation++;
                    end else if (scenario inside {5,7,9,12}) begin
                        #5ns;
                        memblock_sync_pkg::request_control_runtime_reset("negative old action reset rejection");
                    end
                    wait(task_returned);
                end
                begin
                    #500ns;
                    `uvm_fatal("CSR_MAPPING_NEGATIVE_TEST", "negative task failed to terminate within outer bound")
                end
            join_any
            disable negative_case;
            uvm_report_cb::delete(null, catcher);
            require_true(task_returned && !completed_ok && catcher.caught_count == 1,
                         $sformatf("scenario %0d missed unique expected failure", scenario));
            require_true(status.control_state == MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING &&
                         status.control_action_enqueued && !local_data.csr_dynamic_candidate_valid,
                         "failed mapping task staged candidate or sent CSR over");
            if (scenario == 3)
                require_true(local_data.get_control_sfence_c0_observation(action.owner, c0) &&
                             !local_data.control_sfence_effective_observation.valid,
                             "missing-C4 negative did not retain real software C0");
            if (fence_lock_case) begin
                blocker.release_lock = 1'b1;
                waited = 0;
                while (blocker.locked || blocker.get_sequence_state() != UVM_FINISHED) begin
                    require_true(waited++ < 200, "negative fence blocker cleanup watchdog");
                    #1ns;
                end
            end
            if (write_lock_case) begin
                write_blocker.release_lock = 1'b1;
                waited = 0;
                while (write_blocker.locked || write_blocker.get_sequence_state() != UVM_FINISHED) begin
                    require_true(waited++ < 200, "negative CSR blocker cleanup watchdog");
                    #1ns;
                end
            end
            if (scenario == 13) begin
                require_true(memblock_csr_mapping_negative_fence_driver::holding_item,
                             "no-item_done negative did not reach original finish_item wait");
                memblock_csr_mapping_negative_fence_driver::abandon_held_item = 1'b1;
                waited = 0;
                while (!memblock_csr_mapping_negative_fence_driver::quiesced) begin
                    require_true(waited++ < 200, "negative fence driver cleanup watchdog");
                    #1ns;
                end
                p_sequencer.fence_sqr.stop_sequences();
            end
            if (scenario == 14) begin
                require_true(memblock_csr_mapping_negative_csr_driver::holding_item,
                             "CSR no-item_done negative missed original finish_item wait");
                memblock_csr_mapping_negative_csr_driver::abandon_held_item = 1'b1;
                waited = 0;
                while (!memblock_csr_mapping_negative_csr_driver::quiesced) begin
                    require_true(waited++ < 200, "negative CSR driver cleanup watchdog");
                    #1ns;
                end
                p_sequencer.csr_ctrl_sqr.stop_sequences();
            end
            if (scenario inside {5,7,9,12}) begin
                wait_current_control_ready();
                drive_clean_target(clean, baseline_sample);
            end
            `uvm_info("CSR_MAPPING_TRANSPORT_NEGATIVE", $sformatf("scenario=%0d PASS", scenario), UVM_LOW)
        end
        require_true(memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 0 &&
                     memblock_sync_pkg::raw_monitor_queue_size() == 0,
                     "isolated negative left unexpected monitor/PMP raw items");
        memblock_sync_pkg::csr_special_sequence_active = 1'b0;
    endtask:body
endclass:memblock_csr_mapping_transport_negative_vseq
`endif
