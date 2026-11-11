`ifndef SOFT_TEST_TC_RM_CMO_CORE_SV
`define SOFT_TEST_TC_RM_CMO_CORE_SV
// 纯RM功能测试不创建主动DUT环境，直接调用真实core与被动来源索引实现。
class soft_test_tc_rm_cmo_core extends uvm_test;
    `uvm_component_utils(soft_test_tc_rm_cmo_core)
    function new(string name="soft_test_tc_rm_cmo_core",uvm_component parent=null);
        super.new(name,parent);
    endfunction
    virtual task run_phase(uvm_phase phase);
        rm_cmo_core_soft_test_t checks;
        phase.raise_objection(this);
        checks=new(); checks.run();
        `uvm_info("RM_CMO_CORE_SOFT", "TEST CASE PASSED", UVM_LOW)
        phase.drop_objection(this);
    endtask
endclass
class soft_test_tc_rm_cmo_request extends uvm_test;
    `uvm_component_utils(soft_test_tc_rm_cmo_request)
    function new(string name="soft_test_tc_rm_cmo_request",uvm_component parent=null);
        super.new(name,parent);
    endfunction
    virtual task run_phase(uvm_phase phase);
        rm_cmo_request_soft_test_t checks;
        phase.raise_objection(this);
        checks=new(); checks.run();
        `uvm_info("RM_CMO_REQUEST_SOFT", "TEST CASE PASSED", UVM_LOW)
        phase.drop_objection(this);
    endtask
endclass
class soft_test_tc_rm_uncache_error extends uvm_test;
    `uvm_component_utils(soft_test_tc_rm_uncache_error)
    function new(string name="soft_test_tc_rm_uncache_error",uvm_component parent=null);
        super.new(name,parent);
    endfunction
    virtual task run_phase(uvm_phase phase);
        rm_uncache_error_soft_test_t checks;
        phase.raise_objection(this);
        checks=new(); checks.run();
        `uvm_info("RM_UNCACHE_SOFT", "TEST CASE PASSED", UVM_LOW)
        phase.drop_objection(this);
    endtask
endclass
class soft_test_tc_rm_dcache_error extends uvm_test;
    `uvm_component_utils(soft_test_tc_rm_dcache_error)
    function new(string name="soft_test_tc_rm_dcache_error",uvm_component parent=null);
        super.new(name,parent);
    endfunction
    virtual task run_phase(uvm_phase phase);
        rm_dcache_error_observer_soft_test checks;
        string detail;
        phase.raise_objection(this);
        checks=new();
        if(!checks.run(detail)) `uvm_fatal("RM_D_ERROR_SOFT",detail)
        `uvm_info("RM_D_ERROR_SOFT", {"TEST CASE PASSED: ",detail}, UVM_LOW)
        phase.drop_objection(this);
    endtask
endclass
`endif
