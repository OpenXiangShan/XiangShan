`ifndef MEMBLOCK_MAIN_DISPATCH_CBO_FAULT_COLD_SEQUENCE_SV
`define MEMBLOCK_MAIN_DISPATCH_CBO_FAULT_COLD_SEQUENCE_SV
// 中文注释：仅故障验收使用cold CMO行；年轻访问均不同line，首次L2访问只能来自真实CBO A。
class memblock_main_dispatch_cbo_fault_cold_sequence extends memblock_main_dispatch_cbo_directed_sequence;
    `uvm_object_utils(memblock_main_dispatch_cbo_fault_cold_sequence)
    function new(string name="memblock_main_dispatch_cbo_fault_cold_sequence"); super.new(name); endfunction
    // 抽象职责：复用现有真实dispatch/runtime，建一条cold nonzero CMO和不同line年轻访问及尾CHECK_STORE。
    virtual task build_main_table();
        bit [8:0] op;
        ensure_dispatch_runtime_helpers();
        if(memblock_sync_pkg::get_control_worker_topology_mode()!=
           memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_AUTO_MAIN_TABLE)
            `uvm_fatal(get_type_name(),"cold CMO fault scenario requires existing AUTO topology")
        op=pick_fuop_by_weight(MEMBLOCK_OP_CLASS_CBO,"cold CMO fault target");
        if(!(op inside {MEMBLOCK_LSUOP_CBO_CLEAN,MEMBLOCK_LSUOP_CBO_FLUSH,MEMBLOCK_LSUOP_CBO_INVAL}))
            `uvm_fatal(get_type_name(),"cold CMO fault target must be final clean/flush/inval")
        clear_manual_main_table();
        set_manual_main_transaction(0,make_cbo_directed_transaction("cold_cmo_target",
            MEMBLOCK_OP_CLASS_CBO,op,0,64'h8000_2000));
        set_manual_main_transaction(1,make_cbo_directed_transaction("cold_cmo_younger_other_load",
            MEMBLOCK_OP_CLASS_INT_LOAD,MEMBLOCK_LSUOP_LD,1,64'h8000_3000));
        set_manual_main_transaction(2,make_cbo_directed_transaction("cold_cmo_younger_other_store",
            MEMBLOCK_OP_CLASS_STORE,MEMBLOCK_LSUOP_SD,2,64'h8000_4000));
        set_manual_main_transaction(3,make_control_main_transaction("cold_cmo_final_check_store",3,
            MEMBLOCK_OP_CLASS_CHECK_STORE,'{flag:1'b0,value:MEMBLOCK_ROB_VALUE_W'(3)}));
        import_manual_main_table();
        check_main_table_control_policy("cold CMO fault target");
        // 覆盖父类建表后必须交付同一个既有AUTO运行期bootstrap，worker仍由父VSEQ唯一启动。
        control_barrier_service.initialize_control_runtime_bootstrap();
    endtask
endclass

// 中文注释：独立验证CMO响应错误，年轻恢复流量仅保留Load；原含Store的cold回归继续保留。
class memblock_main_dispatch_cbo_fault_cold_isolated_sequence extends memblock_main_dispatch_cbo_fault_cold_sequence;
    `uvm_object_utils(memblock_main_dispatch_cbo_fault_cold_isolated_sequence)
    function new(string name="memblock_main_dispatch_cbo_fault_cold_isolated_sequence"); super.new(name); endfunction
    // 抽象职责：真实cold CMO后仅安排两个不同line的年轻Load，验收指令故障及Load恢复，不检查Store/flush排空。
    virtual task build_main_table();
        bit [8:0] op;
        ensure_dispatch_runtime_helpers();
        if(memblock_sync_pkg::get_control_worker_topology_mode()!=
           memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_DISABLED)
            `uvm_fatal(get_type_name(),"isolated cold CMO fault scenario requires existing DISABLED topology")
        op=pick_fuop_by_weight(MEMBLOCK_OP_CLASS_CBO,"isolated cold CMO fault target");
        if(!(op inside {MEMBLOCK_LSUOP_CBO_CLEAN,MEMBLOCK_LSUOP_CBO_FLUSH,MEMBLOCK_LSUOP_CBO_INVAL}))
            `uvm_fatal(get_type_name(),"isolated cold CMO target must be final clean/flush/inval")
        clear_manual_main_table();
        set_manual_main_transaction(0,make_cbo_directed_transaction("isolated_cold_cmo_target",
            MEMBLOCK_OP_CLASS_CBO,op,0,64'h8000_2000));
        set_manual_main_transaction(1,make_cbo_directed_transaction("isolated_cold_cmo_younger_load1",
            MEMBLOCK_OP_CLASS_INT_LOAD,MEMBLOCK_LSUOP_LD,1,64'h8000_3000));
        set_manual_main_transaction(2,make_cbo_directed_transaction("isolated_cold_cmo_younger_load2",
            MEMBLOCK_OP_CLASS_INT_LOAD,MEMBLOCK_LSUOP_LD,2,64'h8000_4000));
        // 手工表仅3项；既有import按实际表项数建立主表，没有writer也不添加CHECK_STORE。
        import_manual_main_table();
        check_main_table_control_policy("isolated cold CMO fault target");
        // DISABLED允许零control项；父real-smoke唯一启动既有静态Sv39 CSR，无AUTO worker/bootstrap。
    endtask
endclass
`endif
