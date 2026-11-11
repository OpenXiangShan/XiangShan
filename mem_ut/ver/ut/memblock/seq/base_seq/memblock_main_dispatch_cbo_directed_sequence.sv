//=========================================================
//File name    : memblock_main_dispatch_cbo_directed_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_main_dispatch_cbo_directed_sequence
//Discribution : directed real DUT CBO main-table sequence
//Date         : 2026-11-09
//=========================================================
`ifndef MEMBLOCK_MAIN_DISPATCH_CBO_DIRECTED_SEQUENCE__SV
`define MEMBLOCK_MAIN_DISPATCH_CBO_DIRECTED_SEQUENCE__SV

// 中文注释：该 sequence 只替换静态主表构造。body、真实 monitor service、
// redirect/commit/deq 收敛和 terminal audit 全部继承 auto-main 父类。
class memblock_main_dispatch_cbo_directed_sequence extends
    memblock_main_dispatch_auto_build_main_table_base_sequence;

    `uvm_object_utils(memblock_main_dispatch_cbo_directed_sequence)

    extern function new(string name = "memblock_main_dispatch_cbo_directed_sequence");
    extern virtual task build_main_table();
    extern function main_control_transaction make_cbo_directed_transaction(
        input string tr_name,
        input memblock_op_class_e op_class,
        input bit [8:0] fuOpType,
        input int unsigned rob_value,
        input bit [63:0] base_addr
    );

endclass:memblock_main_dispatch_cbo_directed_sequence

function memblock_main_dispatch_cbo_directed_sequence::new(
    string name = "memblock_main_dispatch_cbo_directed_sequence"
);
    super.new(name);
endfunction:new

// 抽象职责：构造 older store、目标 CBO 和同 line/不同 line 年轻访存，
// 让 non-zero/fault redirect 后始终存在从 CBO UID+1 重建的静态工作。
task memblock_main_dispatch_cbo_directed_sequence::build_main_table();
    bit [8:0] cbo_fuOpType;
    memblock_sync_pkg::memblock_control_worker_topology_mode_e topology_mode;

    ensure_dispatch_runtime_helpers();
    topology_mode = memblock_sync_pkg::get_control_worker_topology_mode();
    if (topology_mode != memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_DISABLED &&
        topology_mode != memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_AUTO_MAIN_TABLE) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("directed CBO main table requires DISABLED or AUTO topology, got %0d",
                             memblock_sync_pkg::get_control_worker_topology_mode()))
    end

    cbo_fuOpType = pick_fuop_by_weight(MEMBLOCK_OP_CLASS_CBO,
                                       "directed CBO target");
    clear_manual_main_table();
    set_manual_main_transaction(
        0,
        make_cbo_directed_transaction("cbo_older_store",
                                      MEMBLOCK_OP_CLASS_STORE,
                                      MEMBLOCK_LSUOP_SD,
                                      0,
                                      64'h0000_0000_8000_1000));
    set_manual_main_transaction(
        1,
        make_cbo_directed_transaction("cbo_target",
                                      MEMBLOCK_OP_CLASS_CBO,
                                      cbo_fuOpType,
                                      1,
                                      64'h0000_0000_8000_2000));
    set_manual_main_transaction(
        2,
        make_cbo_directed_transaction("cbo_younger_same_line_load",
                                      MEMBLOCK_OP_CLASS_INT_LOAD,
                                      MEMBLOCK_LSUOP_LD,
                                      2,
                                      64'h0000_0000_8000_2008));
    set_manual_main_transaction(
        3,
        make_cbo_directed_transaction("cbo_younger_same_line_store",
                                      MEMBLOCK_OP_CLASS_STORE,
                                      MEMBLOCK_LSUOP_SD,
                                      3,
                                      64'h0000_0000_8000_2010));
    set_manual_main_transaction(
        4,
        make_cbo_directed_transaction("cbo_younger_other_line_load",
                                      MEMBLOCK_OP_CLASS_INT_LOAD,
                                      MEMBLOCK_LSUOP_LD,
                                      4,
                                      64'h0000_0000_8000_3000));
    set_manual_main_transaction(
        5,
        make_cbo_directed_transaction("cbo_younger_other_line_store",
                                      MEMBLOCK_OP_CLASS_STORE,
                                      MEMBLOCK_LSUOP_SD,
                                      5,
                                      64'h0000_0000_8000_4000));
    // AUTO复用已有控制worker完成flushSB/L2 flush和最终Store-table比较；
    // DISABLED仍保留静态Sv39 CSR，供PBMT/前置异常专项用例使用。
    if (topology_mode == memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_AUTO_MAIN_TABLE) begin
        set_manual_main_transaction(
            6,
            make_control_main_transaction("cbo_final_check_store", 6,
                                           MEMBLOCK_OP_CLASS_CHECK_STORE,
                                           '{flag:1'b0,value:MEMBLOCK_ROB_VALUE_W'(6)}));
    end
    import_manual_main_table();
    check_main_table_control_policy("memblock_main_dispatch_cbo_directed_sequence");
    // 中文注释：此定向builder覆盖了父类build_main_table，必须在整表导入后复用同一bootstrap。
    // AUTO的CSR/SFence worker由父VSEQ唯一启动；这里只建立控制运行期握手，不重建表或放宽检查。
    if (memblock_sync_pkg::uses_control_barrier_topology()) begin
        control_barrier_service.initialize_control_runtime_bootstrap();
    end
endtask:build_main_table

function main_control_transaction
memblock_main_dispatch_cbo_directed_sequence::make_cbo_directed_transaction(
    input string tr_name,
    input memblock_op_class_e op_class,
    input bit [8:0] fuOpType,
    input int unsigned rob_value,
    input bit [63:0] base_addr
);
    main_control_transaction tr;
    bit [MEMBLOCK_ROB_VALUE_W-1:0] fitted_rob_value;

    fitted_rob_value = fit_directed_rob_value_or_fatal(
        rob_value,
        $sformatf("%s::make_cbo_directed_transaction(%s)",
                  get_type_name(), tr_name));
    tr = main_control_transaction::type_id::create(tr_name);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), $sformatf("failed to create %s", tr_name))
    end

    tr.op_class = op_class;
    tr.robIdx_flag = 1'b0;
    tr.robIdx_value = fitted_rob_value;
    tr.lqIdx_flag = 1'b0;
    tr.lqIdx_value = '0;
    tr.sqIdx_flag = 1'b0;
    tr.sqIdx_value = '0;
    tr.src_0 = base_addr;
    tr.imm = 64'h0;
    tr.tlbAF = 1'b0;
    tr.tlbPF = 1'b0;
    tr.tlbGPF = 1'b0;
    tr.PBMT = '0;
    tr.pmaAF = 1'b0;
    tr.corrupt = 1'b0;
    tr.denied = 1'b0;
    tr.delay = 0;
    tr.send_pri = 0;
    tr.send_pri_std = 0;
    apply_op_class_template(tr, fuOpType);
    tr.update_vaddr();
    return tr;
endfunction:make_cbo_directed_transaction

`endif
