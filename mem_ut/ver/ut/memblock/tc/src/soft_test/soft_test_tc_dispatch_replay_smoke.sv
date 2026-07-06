//=========================================================
//File name    : soft_test_tc_dispatch_replay_smoke.sv
//Author       : OpenAI_Codex
//Module name  : soft_test_tc_dispatch_replay_smoke
//Discribution : software-only dispatch replay closure smoke test
//Date         : 2026-05-19
//=========================================================
`ifndef SOFT_TEST_TC_DISPATCH_REPLAY_SMOKE__SV
`define SOFT_TEST_TC_DISPATCH_REPLAY_SMOKE__SV

class soft_test_tc_dispatch_replay_smoke extends soft_test_tc_dispatch_smoke;

    `uvm_component_utils(soft_test_tc_dispatch_replay_smoke)

    function new(string name = "soft_test_tc_dispatch_replay_smoke", uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual task run_dispatch_smoke_sequence();
        soft_test_memblock_dispatch_replay_smoke_sequence replay_smoke_seq;

        `uvm_info(get_type_name(), "Starting software dispatch replay smoke", UVM_MEDIUM)
        replay_smoke_seq = soft_test_memblock_dispatch_replay_smoke_sequence::type_id::create("replay_smoke_seq");
        if (replay_smoke_seq == null) begin
            `uvm_fatal(get_type_name(), "failed to create soft_test_memblock_dispatch_replay_smoke_sequence")
        end
        replay_smoke_seq.start(null);
        `uvm_info(get_type_name(), "soft_test_tc_dispatch_replay_smoke completed", UVM_MEDIUM)
    endtask:run_dispatch_smoke_sequence

endclass:soft_test_tc_dispatch_replay_smoke

class tc_dispatch_replay_smoke extends soft_test_tc_dispatch_replay_smoke;

    `uvm_component_utils(tc_dispatch_replay_smoke)

    function new(string name = "tc_dispatch_replay_smoke", uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

endclass:tc_dispatch_replay_smoke

`endif
