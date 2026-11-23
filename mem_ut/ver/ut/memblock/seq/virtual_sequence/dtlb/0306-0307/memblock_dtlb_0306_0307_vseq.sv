`ifndef MEMBLOCK_DTLB_0306_0307_VSEQ__SV
`define MEMBLOCK_DTLB_0306_0307_VSEQ__SV
// 抽象职责：真实请求/匹配返回建立NAPOT或双页映射，独立检查PA/权限/配对。
// 仅继承HDL/时钟/请求-token/真实hit读取等helper，不运行父body；不改RTL输出或模型。
class memblock_dtlb_0306_0307_vseq extends memblock_dtlb_0301_0305_vseq;
    `uvm_object_utils(memblock_dtlb_0306_0307_vseq)
    // 本任务精确检查数；返回仍使用父级唯一VPN/stage token，不创建第二个全局owner。
    int unsigned target_checks,napot_windows,cross_pairs;
    function new(string name="memblock_dtlb_0306_0307_vseq");
        super.new(name);target_checks=0;napot_windows=0;cross_pairs=0;
    endfunction
    virtual function string scenario();return "napot";endfunction
    // 抽象职责：把当前真实PTW凭据变为一次匹配、合法权限和N位编码的返回。
    // deny_permission只拒绝本口命令；S1的R=0时W也为0，保持合法execute-only PTE。
    task write_return(input int g,input bit [49:0] va,input bit [47:0] pa,
                      input bit napot=0,input bit deny_permission=0,input bit [1:0] pbmt=0);
        string u;int stage;bit [47:0] s1_pa;bit [40:0] s1_ppn;bit [37:0] s2_ppn;
        bit [7:0] validmask;
        u=unit_name(g);stage=active_stage[g];
        if(!token_valid||token_group!=g||token_vpn!==va[49:12]||token_stage!=stage)
            `uvm_fatal("DTLB030607_TOKEN","response has no matching actual VPN/stage request")
        // 两阶段场景S1使用VA->恒等GPA，S2再映射到pa；单S1直接映射pa。
        s1_pa=stage==3?va[47:0]:pa;
        s1_ppn={8'b0,s1_pa[47:15]};s2_ppn={2'b0,pa[47:12]};
        if(napot)begin
            s1_ppn[0]=1; // PTE完整PPN低4位为1000；S1字段不含低三个ppn_low位。
            s2_ppn[3:0]=4'b1000;
        end
        validmask=8'b1<<va[14:12];
        drive({u,".io_ptw_resp_bits_s2xlate"},stage);
        drive({u,".io_ptw_resp_bits_s1_entry_tag"},va[49:15]);
        drive({u,".io_ptw_resp_bits_s1_addr_low"},va[14:12]);
        drive({u,".io_ptw_resp_bits_s1_entry_ppn"},s1_ppn);
        drive({u,".io_ptw_resp_bits_s1_entry_level"},0);
        drive({u,".io_ptw_resp_bits_s1_entry_n"},napot&&stage!=2);
        drive({u,".io_ptw_resp_bits_s1_entry_asid"},active_asid[g]);
        drive({u,".io_ptw_resp_bits_s1_entry_vmid"},active_vmid[g]);
        drive({u,".io_ptw_resp_bits_s1_entry_pbmt"},pbmt);
        drive({u,".io_ptw_resp_bits_s1_entry_v"},1);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_g"},0);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_d"},1);drive({u,".io_ptw_resp_bits_s1_entry_perm_a"},1);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_u"},1);drive({u,".io_ptw_resp_bits_s1_entry_perm_x"},1);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_w"},!deny_permission);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_r"},g==1||!deny_permission);
        drive({u,".io_ptw_resp_bits_s1_pf"},0);drive({u,".io_ptw_resp_bits_s1_af"},0);
        drive({u,".io_ptw_resp_bits_s2_entry_tag"},va[49:12]);
        drive({u,".io_ptw_resp_bits_s2_entry_ppn"},s2_ppn);
        drive({u,".io_ptw_resp_bits_s2_entry_level"},0);
        drive({u,".io_ptw_resp_bits_s2_entry_n"},napot&&stage>=2);
        drive({u,".io_ptw_resp_bits_s2_entry_vmid"},active_vmid[g]);
        drive({u,".io_ptw_resp_bits_s2_entry_pbmt"},pbmt);
        drive({u,".io_ptw_resp_bits_s2_entry_perm_g"},0);
        drive({u,".io_ptw_resp_bits_s2_entry_perm_d"},1);drive({u,".io_ptw_resp_bits_s2_entry_perm_a"},1);
        drive({u,".io_ptw_resp_bits_s2_entry_perm_u"},1);drive({u,".io_ptw_resp_bits_s2_entry_perm_x"},1);
        drive({u,".io_ptw_resp_bits_s2_entry_perm_w"},1);drive({u,".io_ptw_resp_bits_s2_entry_perm_r"},1);
        drive({u,".io_ptw_resp_bits_s2_gpf"},0);drive({u,".io_ptw_resp_bits_s2_gaf"},0);
        for(int sub=0;sub<8;sub++)begin
            drive($sformatf("%s.io_ptw_resp_bits_s1_valididx_%0d",u,sub),validmask[sub]);
            drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",u,sub),validmask[sub]);
            drive($sformatf("%s.io_ptw_resp_bits_s1_ppn_low_%0d",u,sub),napot?0:sub);
        end
        drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);
        @(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);
        token_valid=0;refills++;cycles(8);
    endtask
    task insert_entry(input int g,input bit [49:0] va,input bit [47:0] pa,
                      input bit napot=0,input bit deny_permission=0,input bit [1:0] pbmt=0);
        prepare_miss(g,va);write_return(g,va,pa,napot,deny_permission,pbmt);
    endtask
    task napot_matrix();
        bit [49:0] base,va;bit [47:0] pa;
        base=50'h80000000;pa=48'h40000000;
        for(int g=0;g<3;g++)begin
            for(int stage=0;stage<4;stage++)begin
                configure_context(g,stage);insert_entry(g,base,pa,1);
                for(int sub=0;sub<16;sub++)begin
                    // 子页0/15同时检查范围首尾，其余子页采用非零页内偏移。
                    va=base+50'(sub)*4096+(sub==0?0:sub==15?4095:291);
                    for(int p=0;p<ports(g);p++)begin
                        probe("napot64_subpage",g,p,va,pa+48'(va-base));target_checks++;
                        `uvm_info("DTLB030607_SAMPLE",$sformatf("NAPOT g=%0d stage=%0d port=%0d sub=%0d va=%0h pa=%0h",g,stage,p,sub,va,pa+48'(va-base)),UVM_LOW)
                    end
                end
                probe("napot64_outside",g,0,base+50'h10000,0,0,1);target_checks++;
                `uvm_info("DTLB030607_SAMPLE",$sformatf("NAPOT_OUTSIDE g=%0d stage=%0d",g,stage),UVM_LOW)
                napot_windows++;matrices++;
            end
        end
    endtask
    // 抽象职责：检查相邻页的一部分，保留同一原fullva和完整ROB，独立比较实际映射/异常/PBMT。
    task cross_probe(input int g,input int p,input bit [49:0] va,input bit [63:0] original_va,
                     input bit [47:0] expected_pa,input bit deny_permission,input bit [1:0] pbmt);
        string u,q,rsp;logic [5:0] exception,expected_exception;logic [47:0] hit;
        u=unit_name(g);q=$sformatf("%s.io_requestor_%0d_req",u,p);rsp=$sformatf("%s.io_requestor_%0d_resp",u,p);
        drive({q,"_bits_vaddr"},va);drive({q,"_bits_cmd"},g==1?1:0);
        if(!(g==2&&p==1))begin
            drive({q,"_bits_fullva"},original_va);drive({q,"_bits_checkfullva"},1);
            drive({q,"_bits_hyperinst"},0);drive({q,"_bits_debug_robIdx_flag"},0);
            drive({q,"_bits_debug_robIdx_value"},8'h40);
        end
        drive({q,"_valid"},1);@(posedge clock_vif.clk);
        @(negedge clock_vif.clk);drive({q,"_valid"},0);@(posedge clock_vif.clk);
        hit=hit_vector(g,p);expected_exception=deny_permission?(g==1?6'h02:6'h01):0;exception=actual_exception(rsp);
        if($isunknown(hit)||$countones(hit)!=1||read_hdl({rsp,"_valid"})!==uvm_hdl_data_t'(1)||
           read_hdl({rsp,"_bits_miss"})!==uvm_hdl_data_t'(0)||
           read_hdl({indexed(u,"req_out",p),"_vaddr"},50)!==uvm_hdl_data_t'(va)||
           read_hdl({indexed(u,"req_out",p),"_cmd"},3)!==uvm_hdl_data_t'(g==1?1:0))
            `uvm_error("DTLB030607_RESPONSE",$sformatf("g=%0d p=%0d va=%0h hit=%0h",g,p,va,hit))
        if(exception!==expected_exception||read_hdl({rsp,"_bits_pbmt_0"},2)!==uvm_hdl_data_t'(pbmt))
            `uvm_error("DTLB030607_PERMISSION",$sformatf("g=%0d p=%0d expected_fault=%0h actual_fault=%0h expected_pbmt=%0h actual_pbmt=%0h",g,p,expected_exception,exception,pbmt,read_hdl({rsp,"_bits_pbmt_0"},2)))
        if(read_hdl({rsp,"_bits_paddr_0"},48)!==uvm_hdl_data_t'(expected_pa))
            `uvm_error("DTLB030607_PA",$sformatf("g=%0d p=%0d va=%0h expected=%0h actual=%0h",g,p,va,expected_pa,read_hdl({rsp,"_bits_paddr_0"},48)))
        if(fullva_output(g,p)&&read_hdl({rsp,"_bits_fullva"},64)!==uvm_hdl_data_t'(original_va))
            `uvm_error("DTLB030607_FULLVA","cross-page original fullva changed")
        if(!(g==2&&p==1)&&
           (read_hdl({indexed(u,"req_out",p),"_debug_robIdx_flag"})!==uvm_hdl_data_t'(0)||
            read_hdl({indexed(u,"req_out",p),"_debug_robIdx_value"},8)!==uvm_hdl_data_t'(8'h40)))
            `uvm_error("DTLB030607_ROB","cross-page full ROB association changed")
        probes++;target_checks++;
        `uvm_info("DTLB030607_SAMPLE",$sformatf("CROSS g=%0d p=%0d va=%0h original=%0h pa=%0h excp=%0h pbmt=%0d",g,p,va,original_va,expected_pa,exception,pbmt),UVM_LOW)
        @(negedge clock_vif.clk);cycles(2);
    endtask
    task cross_matrix();
        bit [49:0] base;bit [63:0] original;
        base=50'h90000000;original=64'h90000ffc;
        for(int g=0;g<3;g++)begin
            for(int p=0;p<ports(g);p++)begin
                for(int kind=0;kind<3;kind++)begin
                    configure_context(g,0);
                    insert_entry(g,base,48'h50000000,0,kind==2,0);
                    insert_entry(g,base+4096,48'h60001000,0,kind==1,kind==0?1:2);
                    cross_probe(g,p,base+4092,original,48'h50000ffc,kind==2,0);
                    cross_probe(g,p,base+4096,original,48'h60001000,kind==1,kind==0?1:2);
                    cross_pairs++;matrices++;
                end
            end
        end
    endtask
    virtual task body();
        int expected_probes,expected_refills,expected_matrices;
        require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))
            `uvm_fatal("DTLB030607_CLOCK","missing real clock")
        if(seq_csr_common::get_l2tlb_seq_en()||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)
            `uvm_fatal("DTLB030607_OWNER","local fixture cannot share active L2TLB responder")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        `include "dtlb_0306_0307_input_defaults.svh"
        for(int p=0;p<8;p++)drive($sformatf("%s.inner_dtlbRepeater.io_tlb_req_%0d_valid",DUT,p),0);
        initialize_lookup_ports();
        case(scenario())
            "napot":begin napot_matrix();expected_probes=524;expected_refills=12;expected_matrices=12;end
            "cross":begin cross_matrix();expected_probes=48;expected_refills=48;expected_matrices=24;end
            default:`uvm_fatal("DTLB030607_SCENARIO",scenario())
        endcase
        if(token_valid||target_checks!=expected_probes||probes!=expected_probes||
           refills!=expected_refills||matrices!=expected_matrices||initialization_checks!=8)
            `uvm_error("DTLB030607_INCOMPLETE","request/return/matrix/init completion mismatch")
        cleanup();
        `uvm_info("DTLB030607_COMPLETE",$sformatf("scenario=%s requests=%0d returns=%0d matrices=%0d initialization=%0d napot_windows=%0d cross_pairs=%0d",scenario(),target_checks,refills,matrices,initialization_checks,napot_windows,cross_pairs),UVM_LOW)
    endtask
endclass
`define DTLB030607_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_dtlb_0306_0307_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`");super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB030607_CLASS(memblock_dtlb_03_napot_vseq,"napot")
`DTLB030607_CLASS(memblock_dtlb_03_cross_vseq,"cross")
`undef DTLB030607_CLASS
`endif
