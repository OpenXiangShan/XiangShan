`ifndef SOFT_TEST_MEMBLOCK_BACKEND_CYCLE_SEQUENCE__SV
`define SOFT_TEST_MEMBLOCK_BACKEND_CYCLE_SEQUENCE__SV
// 抽象职责：通过生产建帧、最终资格和精确样本消费入口，检查后端发布单元的
// rollback范围、幂等记账、fault身份及地址生命周期；不替代真实DUT时序专项。
class soft_test_memblock_backend_cycle_sequence extends soft_test_memblock_dispatch_smoke_sequence;
    `uvm_object_utils(soft_test_memblock_backend_cycle_sequence)
    function new(string name="soft_test_memblock_backend_cycle_sequence");
        super.new(name);
    endfunction
    virtual task body();
        memblock_backend_cycle_scheduler scheduler;
        memblock_backend_cycle_frame_t normal_frame, rollback_frame, fault_frame;
        memblock_issue_q_item_t fired_items[$];
        memblock_sync_pkg::backend_fault_address_raw_t raw;
        status_transaction status;
        int unsigned saved_replay;
        longint unsigned sample_seq;
        ensure_dispatch_runtime_helpers();
        scheduler = memblock_backend_cycle_scheduler::get();
        commit_handler = lsq_commit_handler::get();
        commit_handler.bind_lsq_ctrl(lsq_ctrl);
        commit_handler.reset_lsqcommit_runtime_state();
        lsq_ctrl.reset();
        build_directed_main_table();
        admit_lsq_and_route_issue();
        fire_all_issue_items(fired_items);
        // synthetic setup: STA IQ-hit precedes the real-WB helper, matching its public contract.
        data.get_status(1).sta_issue_feedback_success = 1;
        inject_writeback_events(fired_items);
        normal_frame = scheduler.build_cycle_frame();
        if (normal_frame == null || normal_frame.commit_uids.size() != 2)
            `uvm_fatal("BACKEND_SELFTEST", "normal frozen batch must contain Load and Store")
        data.get_status(0).replay_seq++;
        if (commit_handler.check_backend_cycle_commit_eligibility(normal_frame))
            `uvm_fatal("BACKEND_SELFTEST", "stale normal replay identity was accepted")
        data.get_status(0).replay_seq--;
        scheduler.reserved_older_decision = normal_frame;
        data.backend_redirect_reserved = 1;
        data.backend_reserved_redirect = '{default:'0};
        data.backend_reserved_redirect.uid = 1;
        data.backend_reserved_redirect.source = MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION;
        data.backend_reserved_redirect.redirect.valid = 1;
        data.backend_reserved_redirect.redirect.rob_key = data.get_status(1).get_rob_key();
        data.backend_reserved_redirect.redirect.level = 1;
        data.backend_reserved_redirect.redirect.flush_itself = 1;
        rollback_frame = scheduler.build_cycle_frame();
        if (rollback_frame == null || !rollback_frame.rollback || rollback_frame.has_fault ||
            !rollback_frame.transport.has_tail || rollback_frame.commit_uids.size() != 1 ||
            rollback_frame.commit_uids[0] != 0 || rollback_frame.transport.head.scommit != 0 ||
            rollback_frame.transport.tail.pending_st || rollback_frame.transport.tail.pending_mmio ||
            rollback_frame.transport.tail.scommit != 0 || rollback_frame.transport.tail.redirect_valid)
            `uvm_fatal("BACKEND_SELFTEST", "rollback must preserve only older prefix and preload blocking tail")
        if (!commit_handler.check_backend_cycle_commit_eligibility(rollback_frame))
            `uvm_fatal("BACKEND_SELFTEST", "legal older prefix failed final preflight")
        rollback_frame.approved = 1;
        commit_handler.apply_sampled_backend_cycle(rollback_frame);
        commit_handler.apply_sampled_backend_cycle(rollback_frame);
        if (!data.get_status(0).rob_commit || data.get_status(1).rob_commit ||
            commit_handler.commit_cursor_uid != 1 || commit_handler.fault_head_waiting)
            `uvm_fatal("BACKEND_SELFTEST", "rollback application must commit older UID once without fault token")
        `uvm_info("BACKEND_SELFTEST", "rollback older prefix / tail / idempotent apply PASS", UVM_LOW)

        // 重建单独fault实例，使用生产最终资格函数拒绝同dynamic内已变化的replay。
        data.backend_redirect_reserved = 0;
        commit_handler.reset_lsqcommit_runtime_state();
        lsq_ctrl.reset();
        build_directed_main_table();
        admit_lsq_and_route_issue();
        fire_all_issue_items(fired_items);
        // synthetic setup: STA IQ-hit precedes the real-WB helper, matching its public contract.
        data.get_status(1).sta_issue_feedback_success = 1;
        inject_writeback_events(fired_items);
        status = data.get_status(0);
        status.fault = 1; status.exception_pending = 1;
        status.load_fault = 1; status.load_pass = 0; status.pass = 0;
        status.exception_vec = 24'h2000;
        fault_frame = new();
        fault_frame.has_fault = 1; fault_frame.uid = 0;
        fault_frame.rob_key = status.get_rob_key();
        fault_frame.dynamic_epoch = status.dynamic_epoch;
        fault_frame.issue_epoch = status.load_issue_epoch;
        fault_frame.replay_seq = status.replay_seq;
        fault_frame.exception_vec = status.exception_vec;
        fault_frame.transport.reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        if (!commit_handler.check_backend_cycle_commit_eligibility(fault_frame))
            `uvm_fatal("BACKEND_SELFTEST", "current fault instance failed preflight")
        saved_replay = status.replay_seq;
        status.replay_seq++;
        if (commit_handler.check_backend_cycle_commit_eligibility(fault_frame))
            `uvm_fatal("BACKEND_SELFTEST", "stale fault replay identity was accepted")
        status.replay_seq = saved_replay;
        status.load_issue_epoch++;
        if (commit_handler.check_backend_cycle_commit_eligibility(fault_frame))
            `uvm_fatal("BACKEND_SELFTEST", "stale fault issue identity was accepted")
        status.load_issue_epoch--;
        `uvm_info("BACKEND_SELFTEST", "fault issue/replay final identity PASS", UVM_LOW)

        // 在不等待真实时钟的同一次调用内构造邻拍毒值；生产消费者只能取target。
        sample_seq = memblock_sync_pkg::peek_current_dut_global_sample()+10;
        fault_frame.input_sample = sample_seq;
        fault_frame.target_sample = sample_seq+2;
        fault_frame.address_required = 1;
        fault_frame.address_source = memblock_backend_cycle_frame_t::ADDRESS_LSQ;
        fault_frame.address_status = memblock_backend_cycle_frame_t::WAITING;
        data.backend_fault_address_by_uid[0] = fault_frame;
        data.backend_address_wait_q.push_back(0);
        raw = '{default:'0};
        raw.reset_epoch = fault_frame.transport.reset_epoch;
        raw.sample_seq = sample_seq+1;
        raw.vaddr = 64'hbad; raw.gpaddr = 64'hbad;
        memblock_sync_pkg::publish_backend_fault_address(raw);
        data.service_backend_fault_address_samples();
        if (fault_frame.address_captured)
            `uvm_fatal("BACKEND_SELFTEST", "fault address consumed before exact target sample")
        raw.sample_seq = sample_seq+2;
        raw.vaddr = 0; raw.gpaddr = 64'h81234000; raw.is_for_vs_non_leaf = 1;
        memblock_sync_pkg::publish_backend_fault_address(raw);
        raw.sample_seq = sample_seq+3; raw.vaddr = 64'hbad; raw.gpaddr = 64'hbad;
        memblock_sync_pkg::publish_backend_fault_address(raw);
        data.service_backend_fault_address_samples();
        if (!fault_frame.address_captured || fault_frame.address.vaddr !== 64'd0 ||
            fault_frame.address.gpaddr !== 64'h81234000 || !fault_frame.address.is_for_vs_non_leaf ||
            data.backend_address_wait_q.size() != 0)
            `uvm_fatal("BACKEND_SELFTEST", "exact S+2 address association failed")
        `uvm_info("BACKEND_SELFTEST", "exact S+2 / zero VA / non-leaf attribution PASS", UVM_LOW)
        fault_frame.address_captured = 0;
        fault_frame.address_status = memblock_backend_cycle_frame_t::WAITING;
        data.backend_address_wait_q.push_back(0);
        scheduler.reset_runtime();
        if (fault_frame.address_status != memblock_backend_cycle_frame_t::CANCELLED_RESET ||
            data.backend_address_wait_q.size() != 0 || data.backend_frame != null ||
            memblock_sync_pkg::backend_release_by_id.num() != 0)
            `uvm_fatal("BACKEND_SELFTEST", "reset did not cancel old address/transport lifecycle")
        data.reset_all_tables(1);
        memblock_sync_pkg::dispatch_monitor_capture_en = 0;
        `uvm_info("BACKEND_SELFTEST", "backend cycle production helper selftest PASS", UVM_LOW)
    endtask
endclass
`endif
