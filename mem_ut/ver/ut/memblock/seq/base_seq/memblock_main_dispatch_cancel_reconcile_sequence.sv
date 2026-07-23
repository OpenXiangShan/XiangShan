//=========================================================
//File name    : memblock_main_dispatch_cancel_reconcile_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_main_dispatch_cancel_reconcile_sequence
//Discribution : deterministic real DUT redirect/cancel main-table sequence
//Date         : 2026-07-22
//=========================================================
`ifndef MEMBLOCK_MAIN_DISPATCH_CANCEL_RECONCILE_SEQUENCE__SV
`define MEMBLOCK_MAIN_DISPATCH_CANCEL_RECONCILE_SEQUENCE__SV

// 中文注释：该 sequence 只替换 manual main table 的内容，复用父类的真实
// enqueue/issue/writeback/commit/deq service flow 和 end_test_check。uid0 是
// redirect anchor，uid1/uid2 是等待真实 LSQ sample 后由 directed vseq flush 的
// younger load/store victim；不在这里直接修改运行期 status 或 redirect 状态。
class memblock_main_dispatch_cancel_reconcile_sequence extends memblock_main_dispatch_manual_main_table_sequence;

    `uvm_object_utils(memblock_main_dispatch_cancel_reconcile_sequence)

    extern function new(string name = "memblock_main_dispatch_cancel_reconcile_sequence");
    extern virtual task build_directed_mixed_main_table();

endclass:memblock_main_dispatch_cancel_reconcile_sequence

function memblock_main_dispatch_cancel_reconcile_sequence::new(string name = "memblock_main_dispatch_cancel_reconcile_sequence");
    super.new(name);
endfunction:new

task memblock_main_dispatch_cancel_reconcile_sequence::build_directed_mixed_main_table();
    main_control_transaction anchor_load;
    main_control_transaction victim_load;
    main_control_transaction victim_store;
    main_control_transaction cbo_probe;
    memblock_op_behavior_t   load_behavior;
    memblock_op_behavior_t   store_behavior;
    memblock_op_behavior_t   cbo_behavior;

    clear_manual_main_table();

    anchor_load = make_directed_transaction("cancel_anchor_load",
                                           MEMBLOCK_OP_CLASS_INT_LOAD,
                                           0,
                                           64'h0000_0000_8000_1000);
    anchor_load.delay = 0;
    set_manual_main_transaction(0, anchor_load);

    victim_load = make_directed_transaction("cancel_victim_load",
                                           MEMBLOCK_OP_CLASS_INT_LOAD,
                                           1,
                                           64'h0000_0000_8000_2000);
    // 中文注释：较大的 ready_cycle 只让 victim 在 redirect 注入前留在 issue queue，
    // 不改变 DUT 的全局 flush 或 LSQ admission 逻辑；reissue 后仍由既有 scheduler 发射。
    victim_load.delay = 32;
    set_manual_main_transaction(1, victim_load);

    victim_store = make_directed_transaction("cancel_victim_store",
                                            MEMBLOCK_OP_CLASS_STORE,
                                            2,
                                            64'h0000_0000_8000_3000);
    victim_store.delay = 32;
    set_manual_main_transaction(2, victim_store);

    // 中文注释：只验证 V2 ROB sideband 分类，不把 CBO probe 放入主表，也不
    // 宣称覆盖 CBO issue/writeback/deq。CBO 是非 vector STU，和普通 scalar
    // store 一样属于 CommitType.STORE，因此必须计入 pendingst/scommit 分类。
    cbo_probe = make_directed_transaction("cancel_cbo_classification_probe",
                                          MEMBLOCK_OP_CLASS_STORE,
                                          3,
                                          64'h0000_0000_8000_4000);
    cbo_probe.op_class  = MEMBLOCK_OP_CLASS_CBO;
    cbo_probe.lsq_flow = MEMBLOCK_LSQ_FLOW_CBO;
    cbo_probe.fuOpType = MEMBLOCK_LSUOP_CBO_CLEAN;
    load_behavior = memblock_op_behavior_util::derive_op_behavior(anchor_load);
    store_behavior = memblock_op_behavior_util::derive_op_behavior(victim_store);
    cbo_behavior = memblock_op_behavior_util::derive_op_behavior(cbo_probe);
    if (memblock_op_behavior_util::is_scalar_rob_store_commit(load_behavior) ||
        !memblock_op_behavior_util::is_scalar_rob_store_commit(store_behavior) ||
        !memblock_op_behavior_util::is_scalar_rob_store_commit(cbo_behavior)) begin
        `uvm_fatal("ROB_STORE_CLASS",
                   $sformatf("V2 scalar ROB store classification mismatch load/store/cbo=%0d/%0d/%0d",
                             memblock_op_behavior_util::is_scalar_rob_store_commit(load_behavior),
                             memblock_op_behavior_util::is_scalar_rob_store_commit(store_behavior),
                             memblock_op_behavior_util::is_scalar_rob_store_commit(cbo_behavior)))
    end
    `uvm_info("ROB_STORE_CLASS",
              "V2 scalar ROB store classification accepts STORE/CBO and rejects LOAD",
              UVM_LOW)

    // import_manual_main_table() 按 ROB key 排序并建立真实 UID/status；本 sequence
    // 不直接调用 activate、reservation、cancel 或 terminal helper。
    import_manual_main_table();
endtask:build_directed_mixed_main_table

`endif
