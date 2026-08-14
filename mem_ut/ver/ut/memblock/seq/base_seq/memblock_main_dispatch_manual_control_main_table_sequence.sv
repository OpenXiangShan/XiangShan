//=========================================================
//File name    : memblock_main_dispatch_manual_control_main_table_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_main_dispatch_manual_control_main_table_sequence
//Discribution : real DUT dispatch directed manual control-table orchestration
//Date         : 2026-08-14
//=========================================================
`ifndef MEMBLOCK_MAIN_DISPATCH_MANUAL_CONTROL_MAIN_TABLE_SEQUENCE__SV
`define MEMBLOCK_MAIN_DISPATCH_MANUAL_CONTROL_MAIN_TABLE_SEQUENCE__SV

class memblock_main_dispatch_manual_control_main_table_sequence extends memblock_main_dispatch_manual_main_table_sequence;

    `uvm_object_utils(memblock_main_dispatch_manual_control_main_table_sequence)

    extern function new(string name = "memblock_main_dispatch_manual_control_main_table_sequence");
    extern virtual task body();
    extern virtual task build_directed_control_main_table();

endclass:memblock_main_dispatch_manual_control_main_table_sequence

function memblock_main_dispatch_manual_control_main_table_sequence::new(
    string name = "memblock_main_dispatch_manual_control_main_table_sequence"
);
    super.new(name);
endfunction:new

// 抽象职责：为 MANUAL_CONTROL_MAIN_TABLE 建立一个显式控制表，并在表完成后启动
// control bootstrap。它不读取 MEMBLOCK_USE_MANUAL_MAIN_TABLE，也不自动预约 CSR/SFence。
task memblock_main_dispatch_manual_control_main_table_sequence::body();
    if (memblock_sync_pkg::get_control_worker_topology_mode() !=
        memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_MANUAL_CONTROL_TABLE) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("manual control main sequence requires MANUAL_CONTROL_MAIN_TABLE mode, got %0d",
                             memblock_sync_pkg::get_control_worker_topology_mode()))
    end

    // uvm_do_on() child startup does not guarantee pre_body(); use the common
    // idempotent setup before the direct builder touches shared runtime state.
    ensure_dispatch_runtime_helpers();
    build_directed_control_main_table();
    check_main_table_control_policy("memblock_main_dispatch_manual_control_main_table_sequence");
    if (control_barrier_service == null) begin
        `uvm_fatal(get_type_name(), "manual control main table has no control barrier service")
    end
    control_barrier_service.initialize_control_runtime_bootstrap();

    `uvm_info(get_type_name(),
              $sformatf("manual control dispatch main table ready: main_trans_num=%0d",
                        data.main_trans_num),
              UVM_LOW)
    service_real_dispatch_flow();
    data.end_test_check();
    `uvm_info(get_type_name(), "manual control dispatch sequence completed", UVM_LOW)
endtask:body

// 抽象职责：构造当前专项的最小显式手工控制表。UID/ROB 由 import 路径统一编号；
// 这里仅定义排序键和控制分类，后续专项可覆写此 task 提供不同的手工表。
task memblock_main_dispatch_manual_control_main_table_sequence::build_directed_control_main_table();
    memblock_rob_key_t rob_key;

    clear_manual_main_table();
    set_manual_main_transaction(0, make_directed_transaction("manual_control_load",
                                                              MEMBLOCK_OP_CLASS_INT_LOAD,
                                                              0,
                                                              64'h0000_0000_8000_1000));

    rob_key.flag = 1'b0;
    rob_key.value = fit_directed_rob_value_or_fatal(
        1, "memblock_main_dispatch_manual_control_main_table_sequence CSR ROB");
    set_manual_main_transaction(1, make_control_main_transaction("manual_control_csr",
                                                                  0,
                                                                  MEMBLOCK_OP_CLASS_CSR_CONTROL,
                                                                  rob_key));

    rob_key.value = fit_directed_rob_value_or_fatal(
        2, "memblock_main_dispatch_manual_control_main_table_sequence SFence ROB");
    set_manual_main_transaction(2, make_control_main_transaction("manual_control_sfence",
                                                                  0,
                                                                  MEMBLOCK_OP_CLASS_SFENCE_CONTROL,
                                                                  rob_key));

    rob_key.value = fit_directed_rob_value_or_fatal(
        3, "memblock_main_dispatch_manual_control_main_table_sequence check_store ROB");
    set_manual_main_transaction(3, make_control_main_transaction("manual_control_check_store",
                                                                  0,
                                                                  MEMBLOCK_OP_CLASS_CHECK_STORE,
                                                                  rob_key));
    import_manual_main_table();
endtask:build_directed_control_main_table

`endif
