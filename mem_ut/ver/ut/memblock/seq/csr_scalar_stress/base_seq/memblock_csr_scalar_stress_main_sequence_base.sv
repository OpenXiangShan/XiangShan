//=========================================================
// CSR scalar 10K stress main-table base sequence
//=========================================================
`ifndef MEMBLOCK_CSR_SCALAR_STRESS_MAIN_SEQUENCE_BASE__SV
`define MEMBLOCK_CSR_SCALAR_STRESS_MAIN_SEQUENCE_BASE__SV

class memblock_csr_scalar_stress_main_sequence_base extends
    memblock_main_dispatch_auto_build_main_table_base_sequence;

    localparam int unsigned CSR_SCALAR_STRESS_BUSINESS_TRANS_NUM = 10000;

    `uvm_object_utils(memblock_csr_scalar_stress_main_sequence_base)

    extern function new(string name = "memblock_csr_scalar_stress_main_sequence_base");
    extern virtual task body();
    extern virtual task build_main_table();
    extern virtual function int unsigned expected_normal_slot_count();
    extern virtual function int unsigned expected_csr_marker_count();
    extern virtual function bit business_op_class_allowed(input memblock_op_class_e op_class);
    extern virtual function string stress_profile_name();
    extern virtual function void audit_main_table(input bit require_terminal);

endclass:memblock_csr_scalar_stress_main_sequence_base

function memblock_csr_scalar_stress_main_sequence_base::new(
    string name = "memblock_csr_scalar_stress_main_sequence_base"
);
    super.new(name);
endfunction:new

task memblock_csr_scalar_stress_main_sequence_base::body();
    super.body();
    audit_main_table(1'b1);
endtask:body

task memblock_csr_scalar_stress_main_sequence_base::build_main_table();
    super.build_main_table();
    audit_main_table(1'b0);
endtask:build_main_table

function int unsigned
memblock_csr_scalar_stress_main_sequence_base::expected_normal_slot_count();
    return 0;
endfunction:expected_normal_slot_count

function int unsigned
memblock_csr_scalar_stress_main_sequence_base::expected_csr_marker_count();
    return 0;
endfunction:expected_csr_marker_count

function bit memblock_csr_scalar_stress_main_sequence_base::business_op_class_allowed(
    input memblock_op_class_e op_class
);
    return 1'b0;
endfunction:business_op_class_allowed

function string memblock_csr_scalar_stress_main_sequence_base::stress_profile_name();
    return "UNCONFIGURED";
endfunction:stress_profile_name

function void memblock_csr_scalar_stress_main_sequence_base::audit_main_table(
    input bit require_terminal
);
    int unsigned business_count;
    int unsigned csr_marker_count;
    int unsigned check_store_count;
    int unsigned nonterminal_count;
    int unsigned expected_total_count;

    if (data == null || !data.main_table_ready) begin
        `uvm_fatal("MEMBLOCK_CSR_SCALAR_STRESS_AUDIT",
                   "stress audit requires a completed main table")
    end

    business_count = 0;
    csr_marker_count = 0;
    check_store_count = 0;
    nonterminal_count = 0;
    expected_total_count = expected_normal_slot_count() + 1;

    for (int unsigned uid = 0; uid < data.main_trans_num; uid++) begin
        main_control_transaction tr;

        tr = data.get_main_transaction(uid);
        case (tr.op_class)
            MEMBLOCK_OP_CLASS_CSR_CONTROL: begin
                csr_marker_count++;
            end
            MEMBLOCK_OP_CLASS_CHECK_STORE: begin
                check_store_count++;
                if (uid != data.main_trans_num - 1) begin
                    `uvm_fatal("MEMBLOCK_CSR_SCALAR_STRESS_AUDIT",
                               $sformatf("profile=%0s check_store uid=%0d is not final uid=%0d",
                                         stress_profile_name(), uid,
                                         data.main_trans_num - 1))
                end
            end
            default: begin
                if (!business_op_class_allowed(tr.op_class)) begin
                    `uvm_fatal("MEMBLOCK_CSR_SCALAR_STRESS_AUDIT",
                               $sformatf("profile=%0s uid=%0d has forbidden op_class=%0d",
                                         stress_profile_name(), uid, tr.op_class))
                end
                business_count++;
            end
        endcase
        if (require_terminal && !data.get_status(uid).terminal_done) begin
            nonterminal_count++;
        end
    end

    if (data.main_trans_num != expected_total_count) begin
        `uvm_fatal("MEMBLOCK_CSR_SCALAR_STRESS_AUDIT",
                   $sformatf("profile=%0s table_count=%0d expected=%0d",
                             stress_profile_name(), data.main_trans_num,
                             expected_total_count))
    end
    if (business_count != CSR_SCALAR_STRESS_BUSINESS_TRANS_NUM) begin
        `uvm_fatal("MEMBLOCK_CSR_SCALAR_STRESS_AUDIT",
                   $sformatf("profile=%0s business_count=%0d expected=%0d",
                             stress_profile_name(), business_count,
                             CSR_SCALAR_STRESS_BUSINESS_TRANS_NUM))
    end
    if (csr_marker_count != expected_csr_marker_count()) begin
        `uvm_fatal("MEMBLOCK_CSR_SCALAR_STRESS_AUDIT",
                   $sformatf("profile=%0s csr_marker_count=%0d expected=%0d",
                             stress_profile_name(), csr_marker_count,
                             expected_csr_marker_count()))
    end
    if (check_store_count != 1) begin
        `uvm_fatal("MEMBLOCK_CSR_SCALAR_STRESS_AUDIT",
                   $sformatf("profile=%0s check_store_count=%0d expected=1",
                             stress_profile_name(), check_store_count))
    end
    if (require_terminal && nonterminal_count != 0) begin
        `uvm_fatal("MEMBLOCK_CSR_SCALAR_STRESS_AUDIT",
                   $sformatf("profile=%0s nonterminal_count=%0d expected=0",
                             stress_profile_name(), nonterminal_count))
    end

    `uvm_info("MEMBLOCK_CSR_SCALAR_STRESS_AUDIT",
              $sformatf("phase=%0s profile=%0s business_count=%0d csr_marker_count=%0d check_store_count=%0d table_count=%0d nonterminal_count=%0d",
                        require_terminal ? "final" : "build",
                        stress_profile_name(), business_count,
                        csr_marker_count, check_store_count,
                        data.main_trans_num, nonterminal_count),
              UVM_LOW)
endfunction:audit_main_table

`endif
