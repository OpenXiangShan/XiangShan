// 中文注释：仅验证真实双driver及输入monitor的运输合同，不构造LSQ入队或访存违例。
`ifndef MEMBLOCK_BACKEND_TRANSPORT_SELFTEST_VSEQ__SV
`define MEMBLOCK_BACKEND_TRANSPORT_SELFTEST_VSEQ__SV

class memblock_backend_transport_lsq_sequence extends uvm_sequence #(lsqcommit_agent_agent_xaction);
    memblock_sync_pkg::backend_cycle_transport_t transport;
    `uvm_object_utils(memblock_backend_transport_lsq_sequence)
    function new(string name="memblock_backend_transport_lsq_sequence"); super.new(name); endfunction
    virtual task body();
        lsqcommit_agent_agent_xaction tr;
        tr = lsqcommit_agent_agent_xaction::type_id::create("lsq_transport_item");
        tr.backend_sync = 1'b1;
        tr.backend_cycle = transport;
        tr.pre_pkt_gap = 0;
        tr.post_pkt_gap = 0;
        start_item(tr);
        finish_item(tr);
    endtask
endclass

class memblock_backend_transport_redirect_sequence extends uvm_sequence #(redirect_agent_agent_xaction);
    memblock_sync_pkg::backend_cycle_transport_t transport;
    `uvm_object_utils(memblock_backend_transport_redirect_sequence)
    function new(string name="memblock_backend_transport_redirect_sequence"); super.new(name); endfunction
    virtual task body();
        redirect_agent_agent_xaction tr;
        tr = redirect_agent_agent_xaction::type_id::create("redirect_transport_item");
        tr.backend_sync = 1'b1;
        tr.backend_cycle = transport;
        tr.pre_pkt_gap = 0;
        tr.post_pkt_gap = 0;
        start_item(tr);
        finish_item(tr);
    endtask
endclass

class memblock_backend_transport_selftest_vseq extends virtual_base_sequence;
    // 中文注释：本场景自己的运输编号/复位身份；不启动业务scheduler，结束时清除运输槽。
    longint unsigned next_frame_id;
    longint unsigned reset_epoch;
    int unsigned checks;
    `uvm_object_utils(memblock_backend_transport_selftest_vseq)

    function new(string name="memblock_backend_transport_selftest_vseq");
        super.new(name);
        next_frame_id = 1;
        reset_epoch = 0;
        checks = 0;
    endfunction

    function void check(input bit condition, input string description);
        if (!condition) `uvm_fatal("BACKEND_TRANSPORT_SELFTEST", description)
        checks++;
    endfunction

    // 抽象职责：等待两个真实输入producer都完成指定sample；只读历史，不伪造采样。
    task wait_sample(input longint unsigned sample_seq);
        int unsigned waited;
        waited = 0;
        while (!memblock_sync_pkg::backend_lsq_input_by_sample.exists(sample_seq) ||
               !memblock_sync_pkg::backend_redirect_input_by_sample.exists(sample_seq)) begin
            #1ns;
            if (memblock_sync_pkg::l2tlb_reset_active() ||
                memblock_sync_pkg::get_l2tlb_current_reset_epoch() != reset_epoch)
                `uvm_fatal("BACKEND_TRANSPORT_SELFTEST", "reset changed during transport check")
            if (waited++ >= seq_csr_common::get_active_seq_no_progress_warn_cycles())
                `uvm_fatal("BACKEND_TRANSPORT_SELFTEST", "input monitor sample watchdog expired")
        end
    endtask

    // 抽象职责：合并同sample的LSQ和redirect实际输入，逐位确认冻结投影及来源身份。
    task check_sample(input longint unsigned sample_seq,
                      input memblock_sync_pkg::backend_cycle_projection_t expected,
                      input string label_name);
        memblock_sync_pkg::backend_cycle_input_t lsq_raw, redirect_raw;
        memblock_sync_pkg::backend_cycle_projection_t actual;
        wait_sample(sample_seq);
        lsq_raw = memblock_sync_pkg::backend_lsq_input_by_sample[sample_seq];
        redirect_raw = memblock_sync_pkg::backend_redirect_input_by_sample[sample_seq];
        check(lsq_raw.sample_seq == sample_seq && redirect_raw.sample_seq == sample_seq &&
              lsq_raw.reset_epoch == reset_epoch && redirect_raw.reset_epoch == reset_epoch,
              $sformatf("%s sample/reset provenance mismatch", label_name));
        actual = lsq_raw.value;
        actual.redirect_valid = redirect_raw.value.redirect_valid;
        actual.redirect_level = redirect_raw.value.redirect_level;
        actual.redirect_vls = redirect_raw.value.redirect_vls;
        actual.redirect_flag = redirect_raw.value.redirect_flag;
        actual.redirect_value = redirect_raw.value.redirect_value;
        check(actual === expected,
              $sformatf("%s sample=%0d expected=%h actual=%h", label_name, sample_seq, expected, actual));
    endtask

    // 抽象职责：在真实sequencer投递同一主/尾单元，先检查prepared无副作用，再共同
    // 确认未来E；通过mon_cb历史验证S=E+1及紧邻尾拍。取消分支不发布任何payload。
    task run_transport(input memblock_sync_pkg::backend_cycle_projection_t head,
                       input memblock_sync_pkg::backend_cycle_projection_t tail,
                       input bit has_tail,
                       input bit cancel_prepared,
                       input memblock_sync_pkg::backend_cycle_projection_t previous,
                       output longint unsigned sampled_head);
        memblock_sync_pkg::backend_cycle_transport_t transport;
        memblock_sync_pkg::backend_cycle_release_t release_state;
        memblock_backend_transport_lsq_sequence lsq_seq;
        memblock_backend_transport_redirect_sequence redirect_seq;
        longint unsigned ready_sample, edge_sample;
        bit lsq_done, redirect_done;
        int unsigned waited;

        check(head.scommit == 0 && tail.scommit == 0 &&
              !head.pending_st && !head.pending_mmio && !tail.pending_st && !tail.pending_mmio,
              "transport-only test must not commit or mark a nonexistent LSQ instruction pending");
        transport = '{default:'0};
        transport.frame_id = next_frame_id++;
        transport.tail_id = has_tail ? next_frame_id++ : 0;
        transport.reset_epoch = reset_epoch;
        transport.has_tail = has_tail;
        transport.head = head;
        transport.tail = tail;
        memblock_sync_pkg::prepare_backend_transport(transport.frame_id);
        lsq_done = 0;
        redirect_done = 0;
        fork : transport_children
            begin
                // 中文注释：先用标准宏在真实sqr创建，再填不可随机运输payload；
                // uvm_send保留该对象，避免uvm_do_on重新创建后丢失准备身份。
                `uvm_create_on(lsq_seq, p_sequencer.lsqcommit_sqr)
                lsq_seq.transport = transport;
                `uvm_send(lsq_seq)
                lsq_done = 1;
            end
            begin
                `uvm_create_on(redirect_seq, p_sequencer.redirect_sqr)
                redirect_seq.transport = transport;
                `uvm_send(redirect_seq)
                redirect_done = 1;
            end
            begin
                waited = 0;
                release_state = memblock_sync_pkg::get_backend_release(transport.frame_id);
                while (release_state.ready != 2'b11) begin
                    #1ns;
                    release_state = memblock_sync_pkg::get_backend_release(transport.frame_id);
                    if (waited++ >= seq_csr_common::get_active_seq_no_progress_warn_cycles())
                        `uvm_fatal("BACKEND_TRANSPORT_SELFTEST", "drivers did not both acknowledge prepared item")
                end
                check(!release_state.committed && release_state.driven == 0,
                      "two-ready must precede committed drive");
                ready_sample = memblock_sync_pkg::peek_current_dut_global_sample();
                for (int unsigned offset = 1; offset <= 3; offset++)
                    check_sample(ready_sample + offset, previous, "prepared_keeps_previous_inputs");
                if (cancel_prepared) begin
                    memblock_sync_pkg::backend_release_by_id[transport.frame_id].canceled = 1;
                    sampled_head = 0;
                end else begin
                    edge_sample = memblock_sync_pkg::peek_current_dut_global_sample() + 2;
                    memblock_sync_pkg::backend_release_by_id[transport.frame_id].edge_sample = edge_sample;
                    memblock_sync_pkg::backend_release_by_id[transport.frame_id].committed = 1;
                    sampled_head = edge_sample + 1;
                    check_sample(edge_sample, previous, "committed_before_drive_visible");
                    check_sample(sampled_head, head, "head_at_E_plus_1");
                    if (has_tail) check_sample(sampled_head + 1, tail, "tail_at_next_sample");
                end
                waited = 0;
                while (!lsq_done || !redirect_done) begin
                    #1ns;
                    if (waited++ >= seq_csr_common::get_active_seq_no_progress_warn_cycles())
                        `uvm_fatal("BACKEND_TRANSPORT_SELFTEST", "driver item did not finish naturally")
                end
                release_state = memblock_sync_pkg::get_backend_release(transport.frame_id);
                check(release_state.finished == 2'b11, "both drivers must finish the same transport");
                if (cancel_prepared) begin
                    check(release_state.driven == 0 && release_state.tail_driven == 0,
                          "canceled prepared item produced a drive acknowledgement");
                    check_sample(memblock_sync_pkg::peek_current_dut_global_sample() + 1,
                                 previous, "cancel_keeps_previous_inputs");
                end else begin
                    check(release_state.driven == 2'b11 &&
                          (!has_tail || release_state.tail_driven == 2'b11),
                          "head/tail acknowledgements were not complete");
                end
            end
        join
        memblock_sync_pkg::backend_release_by_id.delete(transport.frame_id);
    endtask

    virtual task body();
        memblock_sync_pkg::backend_cycle_projection_t idle, head, tail, same_value, canceled;
        longint unsigned sample_seq, first_same_sample, second_same_sample;
        int unsigned waited;

        require_virtual_sqr();
        require_agent_sqr("lsqcommit", p_sequencer.lsqcommit_sqr);
        require_agent_sqr("redirect", p_sequencer.redirect_sqr);
        seq_csr_common::init();
        check(!memblock_sync_pkg::backend_cycle_mode,
              "transport test requires exclusive ownership of LSQ/redirect producers");
        waited = 0;
        while (memblock_sync_pkg::l2tlb_reset_active() ||
               memblock_sync_pkg::get_l2tlb_current_reset_epoch() == 0 ||
               memblock_sync_pkg::peek_current_dut_global_sample() == 0) begin
            #1ns;
            if (waited++ >= seq_csr_common::get_active_seq_no_progress_warn_cycles())
                `uvm_fatal("BACKEND_TRANSPORT_SELFTEST", "CSR sample/reset baseline did not become ready")
        end
        reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        memblock_sync_pkg::reset_backend_cycle_transport();
        memblock_sync_pkg::backend_cycle_mode = 1;
        idle = '0;
        sample_seq = memblock_sync_pkg::peek_current_dut_global_sample() + 2;
        check_sample(sample_seq, idle, "initial_idle");

        head = idle;
        head.pending_value = 17;
        head.store_exception = 1;
        head.redirect_valid = 1;
        head.redirect_value = 17;
        tail = idle;
        tail.pending_value = 17;
        run_transport(head, tail, 1, 0, idle, sample_seq);

        // 中文注释：两个独立frame携带相同值；必须仍分别确认各自实际sample，
        // 不能用payload变化充当valid，也不能把上一帧的历史样本当作新完成。
        same_value = tail;
        same_value.pending_value = 23;
        run_transport(same_value, idle, 0, 0, tail, first_same_sample);
        run_transport(same_value, idle, 0, 0, same_value, second_same_sample);
        check(second_same_sample > first_same_sample,
              "identical payload frames must have different consumption samples");
        check_sample(first_same_sample, same_value, "first_identical_frame_retained");
        check_sample(second_same_sample, same_value, "second_identical_frame_retained");

        canceled = idle;
        canceled.pending_value = 31;
        canceled.store_exception = 1;
        canceled.redirect_valid = 1;
        canceled.redirect_value = 31;
        run_transport(canceled, idle, 1, 1, same_value, sample_seq);
        run_transport(idle, idle, 0, 0, same_value, sample_seq);
        check_sample(sample_seq + 2, idle, "terminal_idle");
        check(memblock_sync_pkg::backend_release_by_id.num() == 0,
              "transport test left a prepared release slot");
        memblock_sync_pkg::reset_backend_cycle_transport();
        `uvm_info("BACKEND_TRANSPORT_SELFTEST",
                  $sformatf("PASS checks=%0d real_driver_monitor=1 two_ready_future_E=1 head_S_E_plus_1=1 consecutive_tail=1 cancel_prepared=1 identical_frames=1 no_LSQ_commit=1", checks), UVM_LOW)
    endtask
endclass
`endif
