//=========================================================
//File name    : soft_test_memblock_uncache_retry_boost_sequence.sv
//Author       : OpenAI_Codex
//Module name  : soft_test_memblock_uncache_retry_boost_sequence
//Discribution : focused uncache retry boost directed checks
//Date         : 2026-10-15
//=========================================================
`ifndef SOFT_TEST_MEMBLOCK_UNCACHE_RETRY_BOOST_SEQUENCE__SV
`define SOFT_TEST_MEMBLOCK_UNCACHE_RETRY_BOOST_SEQUENCE__SV

class soft_test_memblock_uncache_retry_boost_sequence extends
    soft_test_memblock_pending_mmio_directed_sequence;

    `uvm_object_utils(soft_test_memblock_uncache_retry_boost_sequence)

    extern function new(
        string name = "soft_test_memblock_uncache_retry_boost_sequence"
    );
    extern virtual task body();

endclass:soft_test_memblock_uncache_retry_boost_sequence

function soft_test_memblock_uncache_retry_boost_sequence::new(
    string name = "soft_test_memblock_uncache_retry_boost_sequence"
);
    super.new(name);
endfunction:new

task soft_test_memblock_uncache_retry_boost_sequence::body();
    ensure_directed_helpers();
    run_uncache_retry_boost_scenario();
    `uvm_info(get_type_name(),
              "uncache retry boost focused directed checks completed",
              UVM_LOW)
endtask:body

`endif
