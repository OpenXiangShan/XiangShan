`ifndef TC_DTLB_0501_0504_LIVE__SV
`define TC_DTLB_0501_0504_LIVE__SV
// 抽象职责：为本任务显式启动唯一正式L2TLB owner声明测试拓扑；不改basicTest白名单。
class tc_dtlb_0501_0504_live extends basicTest;
    `uvm_component_utils(tc_dtlb_0501_0504_live)
    function new(string name="tc_dtlb_0501_0504_live",uvm_component parent=null);super.new(name,parent);endfunction
    virtual function void end_of_elaboration_phase(uvm_phase phase);
        // basicTest此hook仅做拓扑冻结/打印；本例用同一公共API声明本例的显式owner。
        if(!(main_vseq_name inside {"memblock_dtlb_05_allocation_vseq","memblock_dtlb_05_merge_vseq","memblock_dtlb_05_integration_vseq"}) || !seq_csr_common::get_l2tlb_seq_en())
            `uvm_fatal("DTLB0504_TOPOLOGY","isolated live testcase requires its dedicated VSEQ and responder enable")
        memblock_sync_pkg::initialize_l2tlb_testcase_lifecycle(
            memblock_sync_pkg::MEMBLOCK_L2TLB_RESPONDER_ENABLED,
            memblock_sync_pkg::MEMBLOCK_L2TLB_TOPOLOGY_DISPATCH_ACTIVE,
            memblock_sync_pkg::MEMBLOCK_L2TLB_START_EXPLICIT,
            1,memblock_sync_pkg::l2tlb_responder_active,get_type_name());
        uvm_top.print_topology();
    endfunction
endclass
`endif
