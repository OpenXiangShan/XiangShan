// 中文注释：纯框架 L2 行历史定向测试入口，使用 basicTest 的统一虚拟 sequence 调度。
`ifndef MEMBLOCK_L2_LINE_HISTORY_DIRECTED_VSEQ__SV
`define MEMBLOCK_L2_LINE_HISTORY_DIRECTED_VSEQ__SV
class memblock_l2_line_history_directed_vseq extends virtual_base_sequence;
    `uvm_object_utils(memblock_l2_line_history_directed_vseq)
    function new(string name = "memblock_l2_line_history_directed_vseq");
        super.new(name);
    endfunction
    virtual task body();
        soft_test_l2_line_history_sequence sequence_under_test;
        sequence_under_test = soft_test_l2_line_history_sequence::type_id::create("sequence_under_test");
        sequence_under_test.start(null);
    endtask
endclass
`endif
