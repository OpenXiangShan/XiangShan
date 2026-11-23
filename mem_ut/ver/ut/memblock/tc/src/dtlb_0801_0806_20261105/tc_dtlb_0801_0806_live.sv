`ifndef TC_DTLB_0801_0806_LIVE__SV
`define TC_DTLB_0801_0806_LIVE__SV
// 抽象职责：仅为本任务显式唯一L2TLB owner声明拓扑，不修改公共basicTest。
class tc_dtlb_0801_0806_live extends basicTest;
    `uvm_component_utils(tc_dtlb_0801_0806_live)
    function new(string name="tc_dtlb_0801_0806_live",uvm_component parent=null);super.new(name,parent);endfunction
    virtual function void end_of_elaboration_phase(uvm_phase phase);
        if(!(main_vseq_name inside {"memblock_dtlb_0804_live_vseq"}) || !seq_csr_common::get_l2tlb_seq_en())
            `uvm_fatal("DTLB0806_TOPOLOGY","dedicated VSEQ and responder enable required")
        memblock_sync_pkg::initialize_l2tlb_testcase_lifecycle(
            memblock_sync_pkg::MEMBLOCK_L2TLB_RESPONDER_ENABLED,
            memblock_sync_pkg::MEMBLOCK_L2TLB_TOPOLOGY_DISPATCH_ACTIVE,
            memblock_sync_pkg::MEMBLOCK_L2TLB_START_EXPLICIT,
            1,memblock_sync_pkg::l2tlb_responder_active,get_type_name());
        uvm_top.print_topology();
    endfunction
endclass
`endif
