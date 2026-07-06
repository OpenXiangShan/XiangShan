//=========================================================
//File name    : memblock_main_dispatch_auto_build_main_table_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_main_dispatch_auto_build_main_table_base_sequence
//Discribution : real DUT dispatch end-to-end smoke orchestration
//Date         : 2026-05-19
//=========================================================
`ifndef MEMBLOCK_MAIN_DISPATCH_AUTO_BUILD_MAIN_TABLE_BASE_SEQUENCE__SV
`define MEMBLOCK_MAIN_DISPATCH_AUTO_BUILD_MAIN_TABLE_BASE_SEQUENCE__SV

class memblock_main_dispatch_auto_build_main_table_base_sequence extends memblock_dispatch_base_sequence;

    virtual lintsissue_agent_agent_interface service_vif;

    `uvm_object_utils(memblock_main_dispatch_auto_build_main_table_base_sequence)

    extern function new(string name = "memblock_main_dispatch_auto_build_main_table_base_sequence");
    extern virtual task body();
    extern virtual function void ensure_service_vif();
    extern virtual task service_real_dispatch_flow();
    extern virtual task service_monitor_once();
    extern virtual function bit all_transactions_terminal_done();
    extern virtual function void report_unfinished_status();
    extern virtual function void report_main_transaction(input memblock_uid_t uid);
    extern virtual function void report_hdl_bit(input string path);
    extern virtual function void report_hdl_value(input string path);

endclass:memblock_main_dispatch_auto_build_main_table_base_sequence

function memblock_main_dispatch_auto_build_main_table_base_sequence::new(string name = "memblock_main_dispatch_auto_build_main_table_base_sequence");
    super.new(name);
    service_vif = null;
endfunction:new

task memblock_main_dispatch_auto_build_main_table_base_sequence::body();
    build_main_table();
    `uvm_info(get_type_name(),
              $sformatf("real dispatch smoke main table ready: main_trans_num=%0d",
                        data.main_trans_num),
              UVM_LOW)
    service_real_dispatch_flow();
    data.end_test_check();
    `uvm_info(get_type_name(), "real dispatch smoke sequence completed", UVM_LOW)
endtask:body

function void memblock_main_dispatch_auto_build_main_table_base_sequence::ensure_service_vif();
    if (service_vif != null) begin
        return;
    end
    if (!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null, get_full_name(), "vif", service_vif) &&
        !uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null, "uvm_test_top.env.u_lintsissue_agent_agent*", "vif", service_vif)) begin
        `uvm_fatal(get_type_name(), "failed to get lintsissue service clock vif")
    end
endfunction:ensure_service_vif

task memblock_main_dispatch_auto_build_main_table_base_sequence::service_real_dispatch_flow();
    memblock_flushsb_base_sequence flushsb_seq;

    ensure_service_vif();
    flushsb_seq = memblock_flushsb_base_sequence::type_id::create("flushsb_seq");
    fork
        flushsb_seq.start(null);
    join_none
    forever begin
        @(negedge service_vif.clk);
        if (service_vif.rst_n !== 1'b1 ||
            memblock_sync_pkg::reset_backend_done !== 1'b1) begin
            continue;
        end
        service_monitor_once();
        if (!data.is_global_stop_requested()) begin
            route_all_issue_queues();
        end
        void'(all_transactions_terminal_done());
        if (data.is_global_stop_requested() &&
            !data.flushsb_request_pending()) begin
            break;
        end
    end
endtask:service_real_dispatch_flow

task memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once();
    memblock_sync_pkg::tick_dispatch_service_cycle();
    // 中文注释：本轮 raw int writeback、IQ feedback 和 memoryViolation 先收集成同一个 batch。
    // batch handler 先做 normalize 和 redirect-first 仲裁，只有未被 redirect 覆盖的 event 才能落状态。
    collect_runtime_context_events();
    collect_monitor_event_batch();
    exception_redirect_replay_task();
endtask:service_monitor_once

function bit memblock_main_dispatch_auto_build_main_table_base_sequence::all_transactions_terminal_done();
    if (data == null || data.main_trans_num == 0) begin
        return 1'b0;
    end
    // 10万笔场景下不能每拍全表扫描；terminal_done前缀跨过所有已终态uid，
    // 因此由顶层单点检查terminal_done前缀并请求global stop，子sequence只读stop标志退出。
    data.request_global_stop_if_done();
    return data.is_global_stop_requested();
endfunction:all_transactions_terminal_done

function void memblock_main_dispatch_auto_build_main_table_base_sequence::report_unfinished_status();
    if (data == null || data.main_trans_num == 0) begin
        `uvm_info(get_type_name(), "real dispatch smoke timeout before main table was built", UVM_LOW)
        return;
    end
    for (int unsigned uid = 0; uid < data.main_trans_num; uid++) begin
        status_transaction status;

        status = data.get_status(uid);
        if (!status.terminal_done ||
            status.active ||
            status.exception_pending || status.replay_pending ||
            status.redirect_pending || status.flushed ||
            !status.enq || !status.issue_ready ||
            !status.writeback || !status.rob_commit || !status.lsq_deq) begin
            `uvm_info(get_type_name(),
                      $sformatf("unfinished uid=%0d terminal_done=%0d active=%0d enq=%0d issue_ready=%0d tlb=%0d queued_l/s/a=%0d/%0d/%0d disp_l/s/a=%0d/%0d/%0d wb=%0d pass=%0d fault=%0d exception_vec=0x%0h rob_commit=%0d lsq_deq=%0d success=%0d pending exc/replay/redirect=%0d/%0d/%0d flushed=%0d",
                                uid,
                                status.terminal_done,
                                status.active,
                                status.enq,
                                status.issue_ready,
                                status.tlb_mapped,
                                status.queued_load,
                                status.queued_sta,
                                status.queued_std,
                                status.load_dispatched,
                                status.sta_dispatched,
                                status.std_dispatched,
                                status.writeback,
                                status.pass,
                                status.fault,
                                status.exception_vec,
                                status.rob_commit,
                                status.lsq_deq,
                                status.success,
                                status.exception_pending,
                                status.replay_pending,
                                status.redirect_pending,
                                status.flushed),
                      UVM_LOW)
            report_main_transaction(uid);
        end
    end
    `uvm_info(get_type_name(),
              $sformatf("queue/map summary load_q=%0d sta_q=%0d std_q=%0d active_rob=%0d lq=%0d sq=%0d flush=%0d redirect=%0d freeze_ack=%0d",
                        data.load_issue_q.size(),
                        data.sta_issue_q.size(),
                        data.std_issue_q.size(),
                        data.uid_by_active_rob.num(),
                        data.uid_by_lq.num(),
                        data.uid_by_sq.num(),
                        data.flush_in_progress,
                        data.active_redirect.valid,
                        data.issue_freeze_ack),
              UVM_LOW)
    report_hdl_bit("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_valid");
    report_hdl_bit("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_ready");
    report_hdl_value("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_bits_fuOpType");
    report_hdl_value("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_bits_src_0");
    report_hdl_value("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_bits_imm");
    report_hdl_value("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value");
    report_hdl_value("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value");
    report_hdl_value("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value");
    report_hdl_bit("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_1_0_ready");
    report_hdl_bit("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_2_0_ready");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_LoadUnit_0_io_tlb_req_valid");
    report_hdl_value("top_tb.U_MEMBLOCK._inner_LoadUnit_0_io_tlb_req_bits_vaddr");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_LoadUnit_0_io_dcache_req_valid");
    report_hdl_value("top_tb.U_MEMBLOCK._inner_LoadUnit_0_io_dcache_req_bits_vaddr");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_LoadUnit_0_io_lqWrite_valid");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_LoadUnit_0_io_ldout_toRob_valid");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_LoadUnit_0_io_ldout_toIntRf_valid");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_dcache_io_lsu_load_0_req_ready");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_dcache_io_lsu_load_1_req_ready");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_dcache_io_lsu_load_2_req_ready");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_dtlb_ld_tlb_ld_io_ptw_req_0_valid");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_dtlbRepeater_io_ptw_req_0_valid");
    report_hdl_bit("top_tb.U_MEMBLOCK.auto_inner_dcache_client_out_a_ready");
    report_hdl_bit("top_tb.U_MEMBLOCK.auto_inner_dcache_client_out_a_valid");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_ptw_io_tlb_1_req_0_ready");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_ptw_io_tlb_1_resp_valid");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_dtlb_ld_tlb_ld_io_requestor_0_resp_valid");
    report_hdl_bit("top_tb.U_MEMBLOCK._inner_dtlb_ld_tlb_ld_io_requestor_0_resp_bits_miss");
    report_hdl_bit("top_tb.U_MEMBLOCK.inner_vSegmentFlag");
endfunction:report_unfinished_status

function void memblock_main_dispatch_auto_build_main_table_base_sequence::report_main_transaction(input memblock_uid_t uid);
    main_control_transaction tr;

    if (data == null || !data.is_valid_uid(uid) || data.main_table_by_uid[uid] == null) begin
        `uvm_info(get_type_name(),
                  $sformatf("main transaction uid=%0d unavailable", uid),
                  UVM_LOW)
        return;
    end

    tr = data.main_table_by_uid[uid];
    `uvm_info(get_type_name(),
              $sformatf("main uid=%0d op_class=%0d fuType=0x%0h fuOpType=0x%0h src_0=0x%0h imm=0x%0h vaddr=0x%0h rob=%0d:%0d lq=%0d:%0d sq=%0d:%0d send_pri=%0d",
                        uid,
                        tr.op_class,
                        tr.fuType,
                        tr.fuOpType,
                        tr.src_0,
                        tr.imm,
                        tr.vaddr,
                        tr.robIdx_flag,
                        tr.robIdx_value,
                        tr.lqIdx_flag,
                        tr.lqIdx_value,
                        tr.sqIdx_flag,
                        tr.sqIdx_value,
                        tr.send_pri),
              UVM_LOW)
endfunction:report_main_transaction

function void memblock_main_dispatch_auto_build_main_table_base_sequence::report_hdl_bit(input string path);
    uvm_hdl_data_t value;

    if (uvm_hdl_read(path, value)) begin
        `uvm_info(get_type_name(),
                  $sformatf("hdl %s=%0b", path, value[0]),
                  UVM_LOW)
    end else begin
        `uvm_info(get_type_name(),
                  $sformatf("hdl %s unreadable", path),
                  UVM_LOW)
    end
endfunction:report_hdl_bit

function void memblock_main_dispatch_auto_build_main_table_base_sequence::report_hdl_value(input string path);
    uvm_hdl_data_t value;

    if (uvm_hdl_read(path, value)) begin
        `uvm_info(get_type_name(),
                  $sformatf("hdl %s=0x%0h", path, value),
                  UVM_LOW)
    end else begin
        `uvm_info(get_type_name(),
                  $sformatf("hdl %s unreadable", path),
                  UVM_LOW)
    end
endfunction:report_hdl_value

`endif
