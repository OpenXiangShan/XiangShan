//=========================================================
//File name    : soft_test_tc_boundary_addr_reuse_gate.sv
//Module name  : soft_test_tc_boundary_addr_reuse_gate
//Discribution : software-only boundary address-reuse gate test
//=========================================================
`ifndef SOFT_TEST_TC_BOUNDARY_ADDR_REUSE_GATE__SV
`define SOFT_TEST_TC_BOUNDARY_ADDR_REUSE_GATE__SV

class soft_test_tc_boundary_addr_reuse_gate extends soft_test_tc_dispatch_smoke;

    `uvm_component_utils(soft_test_tc_boundary_addr_reuse_gate)

    function new(string name = "soft_test_tc_boundary_addr_reuse_gate",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual task run_dispatch_smoke_sequence();
        soft_test_boundary_addr_reuse_gate_sequence gate_seq;

        gate_seq = soft_test_boundary_addr_reuse_gate_sequence::type_id::create(
            "boundary_addr_reuse_gate_seq");
        if (gate_seq == null) begin
            `uvm_fatal(get_type_name(),
                       "failed to create soft_test_boundary_addr_reuse_gate_sequence")
        end
        gate_seq.start(null);
    endtask:run_dispatch_smoke_sequence

endclass:soft_test_tc_boundary_addr_reuse_gate

class tc_boundary_addr_reuse_gate extends soft_test_tc_boundary_addr_reuse_gate;

    `uvm_component_utils(tc_boundary_addr_reuse_gate)

    function new(string name = "tc_boundary_addr_reuse_gate",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

endclass:tc_boundary_addr_reuse_gate

`endif
