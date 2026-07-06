//=========================================================
//File name    : memblock_main_dispatch_manual_main_table_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_main_dispatch_manual_main_table_sequence
//Discribution : real DUT dispatch deterministic load/store smoke orchestration
//Date         : 2026-05-19
//=========================================================
`ifndef MEMBLOCK_MAIN_DISPATCH_MANUAL_MAIN_TABLE_SEQUENCE__SV
`define MEMBLOCK_MAIN_DISPATCH_MANUAL_MAIN_TABLE_SEQUENCE__SV

class memblock_main_dispatch_manual_main_table_sequence extends memblock_main_dispatch_auto_build_main_table_base_sequence;

    `uvm_object_utils(memblock_main_dispatch_manual_main_table_sequence)

    extern function new(string name = "memblock_main_dispatch_manual_main_table_sequence");
    extern virtual task body();
    extern virtual task build_directed_mixed_main_table();
    extern virtual function main_control_transaction make_directed_transaction(input string tr_name,
                                                                              input memblock_op_class_e op_class,
                                                                              input int unsigned rob_value,
                                                                              input bit [63:0] base_addr);

endclass:memblock_main_dispatch_manual_main_table_sequence

function memblock_main_dispatch_manual_main_table_sequence::new(string name = "memblock_main_dispatch_manual_main_table_sequence");
    super.new(name);
endfunction:new

task memblock_main_dispatch_manual_main_table_sequence::body();
    build_directed_mixed_main_table();
    `uvm_info(get_type_name(),
              $sformatf("real mixed dispatch smoke main table ready: main_trans_num=%0d",
                        data.main_trans_num),
              UVM_LOW)
    service_real_dispatch_flow();
    data.end_test_check();
    `uvm_info(get_type_name(), "real mixed dispatch smoke sequence completed", UVM_LOW)
endtask:body

task memblock_main_dispatch_manual_main_table_sequence::build_directed_mixed_main_table();
    clear_manual_main_table();
    set_manual_main_transaction(0, make_directed_transaction("real_mixed_load",
                                                             MEMBLOCK_OP_CLASS_INT_LOAD,
                                                             0,
                                                             64'h0000_0000_8000_1000));
    set_manual_main_transaction(1, make_directed_transaction("real_mixed_store",
                                                             MEMBLOCK_OP_CLASS_STORE,
                                                             1,
                                                             64'h0000_0000_8000_2000));
    import_manual_main_table();
endtask:build_directed_mixed_main_table

function main_control_transaction memblock_main_dispatch_manual_main_table_sequence::make_directed_transaction(input string tr_name,
                                                                                                         input memblock_op_class_e op_class,
                                                                                                         input int unsigned rob_value,
                                                                                                         input bit [63:0] base_addr);
    main_control_transaction tr;

    tr = main_control_transaction::type_id::create(tr_name);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), $sformatf("failed to create %s", tr_name))
    end

    tr.op_class     = op_class;
    tr.robIdx_flag  = 1'b0;
    tr.robIdx_value = rob_value[8:0];
    tr.lqIdx_flag   = 1'b0;
    tr.lqIdx_value  = '0;
    tr.sqIdx_flag   = 1'b0;
    tr.sqIdx_value  = '0;
    tr.src_0        = base_addr;
    tr.imm          = 64'h0;
    tr.tlbAF        = 1'b0;
    tr.tlbPF        = 1'b0;
    tr.tlbGPF       = 1'b0;
    tr.PBMT         = '0;
    tr.pmaAF        = 1'b0;
    tr.corrupt      = 1'b0;
    tr.denied       = 1'b0;
    tr.delay        = 0;
    tr.send_pri     = 0;
    tr.send_pri_std = 0;

    case (op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD: begin
            tr.fuType    = MEMBLOCK_FUTYPE_LDU;
            tr.lsq_flow  = MEMBLOCK_LSQ_FLOW_LOAD;
            tr.fuOpType  = MEMBLOCK_LSUOP_LD;
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            tr.fuType    = MEMBLOCK_FUTYPE_STU;
            tr.lsq_flow  = MEMBLOCK_LSQ_FLOW_STORE;
            tr.fuOpType  = MEMBLOCK_LSUOP_SD;
            tr.numLsElem = 5'd1;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unsupported real mixed op_class=%0d", op_class))
        end
    endcase
    tr.update_vaddr();
    return tr;
endfunction:make_directed_transaction

`endif
