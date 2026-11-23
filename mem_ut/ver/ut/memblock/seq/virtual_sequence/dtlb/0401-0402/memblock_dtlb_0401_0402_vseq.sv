`ifndef MEMBLOCK_DTLB_0401_0402_VSEQ__SV
`define MEMBLOCK_DTLB_0401_0402_VSEQ__SV
// 抽象职责：用真实请求/匹配PTE返回分别验证权限旁路与storage结果，不修改模型。
// 复用既有声明输入/时钟/token helper，父body不运行；公共L2TLB responder不启动。
class memblock_dtlb_0401_0402_vseq extends memblock_dtlb_0301_0305_vseq;
    `uvm_object_utils(memblock_dtlb_0401_0402_vseq)
    // 精确记录目标响应和场景窗口；与case合同核对，不替代逐例VDB计数。
    int unsigned target_checks,permission_windows,live_windows;
    function new(string name="memblock_dtlb_0401_0402_vseq");
        super.new(name);target_checks=0;permission_windows=0;live_windows=0;
    endfunction
    virtual function string scenario();return "s1";endfunction
    task target_request(input int g,input int p,input bit [49:0] va,input int cmd,
                        input bit hyper,input bit hlvx);
        string q;q=$sformatf("%s.io_requestor_%0d_req",unit_name(g),p);
        drive({q,"_bits_vaddr"},va);drive({q,"_bits_cmd"},cmd);
        if(!(g==2&&p==1))begin
            drive({q,"_bits_fullva"},{{14{va[49]}},va});drive({q,"_bits_checkfullva"},0);
            drive({q,"_bits_hyperinst"},hyper);
        end else if(hyper) `uvm_fatal("DTLB0402_CAP","PF1 has no hyperinst")
        if(g!=1&&!(g==2&&p==1))drive({q,"_bits_hlvx"},hlvx);
        else if(hlvx) `uvm_fatal("DTLB0402_CAP","port has no HLVX")
        drive({q,"_valid"},1);
    endtask
    // 抽象职责：按真实token设置合法叶PTE字段，先不发valid以便与目标请求同拍形成旁路。
    // s1={U,D,A,X,W,R}，s2={D,A,X,W,R}；PF是独立上游标记，不伪造异常输出。
    task return_fields(input int g,input bit [49:0] va,input bit [47:0] pa,
                       input bit [5:0] s1,input bit pf1,input bit [4:0] s2,input bit pf2);
        string u;bit [47:0] s1pa;
        u=unit_name(g);s1pa=active_stage[g]==3?va[47:0]:pa;
        if(!token_valid||token_group!=g||token_vpn!==va[49:12]||token_stage!=active_stage[g])
            `uvm_fatal("DTLB0402_TOKEN","PTE return without matching actual VPN/stage request")
        drive({u,".io_ptw_resp_bits_s2xlate"},active_stage[g]);
        drive({u,".io_ptw_resp_bits_s1_entry_tag"},va[49:15]);
        drive({u,".io_ptw_resp_bits_s1_addr_low"},va[14:12]);
        drive({u,".io_ptw_resp_bits_s1_entry_asid"},active_asid[g]);
        drive({u,".io_ptw_resp_bits_s1_entry_vmid"},active_vmid[g]);
        drive({u,".io_ptw_resp_bits_s1_entry_level"},0);drive({u,".io_ptw_resp_bits_s1_entry_n"},0);
        drive({u,".io_ptw_resp_bits_s1_entry_v"},1);drive({u,".io_ptw_resp_bits_s1_entry_ppn"},s1pa[47:15]);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_u"},s1[5]);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_d"},s1[4]);drive({u,".io_ptw_resp_bits_s1_entry_perm_a"},s1[3]);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_x"},s1[2]);drive({u,".io_ptw_resp_bits_s1_entry_perm_w"},s1[1]);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_r"},s1[0]);drive({u,".io_ptw_resp_bits_s1_pf"},pf1);
        drive({u,".io_ptw_resp_bits_s1_af"},0);drive({u,".io_ptw_resp_bits_s1_entry_perm_g"},0);
        drive({u,".io_ptw_resp_bits_s1_entry_pbmt"},0);
        drive({u,".io_ptw_resp_bits_s2_entry_tag"},va[49:12]);
        drive({u,".io_ptw_resp_bits_s2_entry_vmid"},active_vmid[g]);
        drive({u,".io_ptw_resp_bits_s2_entry_level"},0);drive({u,".io_ptw_resp_bits_s2_entry_n"},0);
        drive({u,".io_ptw_resp_bits_s2_entry_ppn"},pa[47:12]);
        drive({u,".io_ptw_resp_bits_s2_entry_perm_u"},1);drive({u,".io_ptw_resp_bits_s2_entry_perm_g"},0);
        drive({u,".io_ptw_resp_bits_s2_entry_perm_d"},s2[4]);drive({u,".io_ptw_resp_bits_s2_entry_perm_a"},s2[3]);
        drive({u,".io_ptw_resp_bits_s2_entry_perm_x"},s2[2]);drive({u,".io_ptw_resp_bits_s2_entry_perm_w"},s2[1]);
        drive({u,".io_ptw_resp_bits_s2_entry_perm_r"},s2[0]);drive({u,".io_ptw_resp_bits_s2_gpf"},pf2);
        drive({u,".io_ptw_resp_bits_s2_gaf"},0);drive({u,".io_ptw_resp_bits_s2_entry_pbmt"},0);
        for(int i=0;i<8;i++)begin
            drive($sformatf("%s.io_ptw_resp_bits_s1_valididx_%0d",u,i),1);
            drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",u,i),1);
            drive($sformatf("%s.io_ptw_resp_bits_s1_ppn_low_%0d",u,i),i);
        end
    endtask
    // 抽象职责：独立比较已知期望异常方向、PA、命令与来源；不读取覆盖模型的expected信号。
    task check_response_permission(input string ident,input int g,input int p,input bit [49:0] va,
                                  input bit [47:0] pa,input int cmd,input int fault,input bit bypass);
        string u,rsp,hit_signal;logic [5:0] actual,expected;uvm_hdl_data_t got_bypass,hit,miss;
        bit expected_miss;
        u=unit_name(g);rsp=$sformatf("%s.io_requestor_%0d_resp",u,p);
        hit_signal=$sformatf("%s._entries_io_r_resp_%0d_bits_hit",u,p);
        got_bypass=read_hdl({u,".readResult_p_hit_last_REG",p==0?"":$sformatf("_%0d",p)});hit=read_hdl(hit_signal);
        actual=actual_exception(rsp);expected=fault==0?0:fault==1?(cmd==1||cmd==5?6'h02:6'h01):(cmd==1||cmd==5?6'h08:6'h04);
        // 双阶段storage GPF会触发真实GPA续查，miss允许且必须准确检查；窗口reset取消后续工作。
        expected_miss=!bypass&&active_stage[g]==3&&fault==2;miss=read_hdl({rsp,"_bits_miss"});
        if(read_hdl({rsp,"_valid"})!==uvm_hdl_data_t'(1)||
           read_hdl({indexed(u,"req_out",p),"_cmd"},3)!==uvm_hdl_data_t'(cmd)||
           read_hdl({indexed(u,"req_out",p),"_vaddr"},50)!==uvm_hdl_data_t'(va)||
           miss!==uvm_hdl_data_t'(expected_miss))
            `uvm_error("DTLB0402_RESPONSE",$sformatf("%s g=%0d p=%0d cmd=%0d expected_miss=%0d got=%0h",ident,g,p,cmd,expected_miss,miss))
        if(got_bypass!==uvm_hdl_data_t'(bypass)||(!bypass&&hit!==uvm_hdl_data_t'(1)))
            `uvm_error("DTLB0402_SOURCE",$sformatf("%s bypass=%0h hit=%0h",ident,got_bypass,hit))
        if(actual!==expected)`uvm_error("DTLB0402_FAULT",$sformatf("%s cmd=%0d expected=%0h actual=%0h",ident,cmd,expected,actual))
        if(read_hdl({rsp,"_bits_paddr_0"},48)!==uvm_hdl_data_t'(pa))
            `uvm_error("DTLB0402_PA",$sformatf("%s expected=%0h actual=%0h",ident,pa,read_hdl({rsp,"_bits_paddr_0"},48)))
        target_checks++;
        `uvm_info("DTLB0402_SAMPLE",$sformatf("%s g=%0d p=%0d cmd=%0d source=%s fault=%0h miss=%0h pa=%0h check=%0d",ident,g,p,cmd,bypass?"bypass":"storage",actual,miss,pa,target_checks),UVM_LOW)
    endtask
    // 抽象职责：同一PTE分别在真实PTW旁路和写入后storage检查，完整隔离本窗口。
    task permission_case(input string ident,input int g,input int p,input int cmd,input int stage,
                         input int dmode,input bit hyper,input bit hlvx,input bit mxr,input bit vmxr,
                         input bit sum,input bit vsum,input bit [5:0] s1,input bit pf1,
                         input bit [4:0] s2,input bit pf2,input int fault);
        string u;bit [49:0] va;bit [47:0] pa;
        u=unit_name(g);va=50'h80000000;pa=48'h40000000;
        configure_context(g,stage);
        drive({u,".io_csr_priv_dmode"},dmode);drive({u,".io_csr_priv_spvp"},dmode==1);
        drive({u,".io_csr_priv_mxr"},mxr);drive({u,".io_csr_priv_vmxr"},vmxr);
        drive({u,".io_csr_priv_sum"},sum);drive({u,".io_csr_priv_vsum"},vsum);cycles(6);
        prepare_miss(g,va);return_fields(g,va,pa,s1,pf1,s2,pf2);
        target_request(g,p,va,cmd,hyper,hlvx);drive({u,".io_ptw_resp_valid"},1);
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);
        drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);drive({u,".io_ptw_resp_valid"},0);
        token_valid=0;refills++;@(posedge clock_vif.clk);
        check_response_permission(ident,g,p,va,pa,cmd,fault,1);
        @(negedge clock_vif.clk);cycles(8);
        target_request(g,p,va,cmd,hyper,hlvx);@(posedge clock_vif.clk);
        @(negedge clock_vif.clk);drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
        @(posedge clock_vif.clk);check_response_permission(ident,g,p,va,pa,cmd,fault,0);
        @(negedge clock_vif.clk);drive({u,".reset"},1);cycles(2);permission_windows++;
    endtask
    // 抽象职责：在同一缓存映射上连续请求，切换真实SUM并按响应拍CSR检查即时权限结果。
    task live_permission_matrix();
        string u;bit [49:0] va;bit [47:0] pa;bit sum_now;
        va=50'h80000000;pa=48'h40000000;
        for(int g=0;g<3;g++)begin
            for(int p=0;p<ports(g);p++)begin
                configure_context(g,0);insert_mapping(g,va,pa);
                target_request(g,p,va,g==1?1:0,0,0);@(posedge clock_vif.clk);@(negedge clock_vif.clk);
                for(int i=0;i<12;i++)begin
                    if(i==3)drive({unit_name(g),".io_csr_priv_sum"},0);
                    if(i==7)drive({unit_name(g),".io_csr_priv_sum"},1);
                    @(posedge clock_vif.clk);
                    sum_now=1'(read_hdl({unit_name(g),"._csr_delay_io_out_priv_sum"}));
                    check_response_permission("04.02.001_live_sum",g,p,va,pa,g==1?1:0,sum_now?0:1,0);
                    @(negedge clock_vif.clk);
                end
                drive($sformatf("%s.io_requestor_%0d_req_valid",unit_name(g),p),0);
                drive({unit_name(g),".reset"},1);cycles(2);live_windows++;
            end
        end
    endtask
    task run_permission_matrix();
        `include "dtlb_0401_0402_scenarios.svh"
    endtask
    virtual task body();
        int expected_checks,expected_returns,expected_windows;
        require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))
            `uvm_fatal("DTLB0402_CLOCK","missing real clock")
        if(seq_csr_common::get_l2tlb_seq_en()||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)
            `uvm_fatal("DTLB0402_OWNER","local fixture cannot share active L2TLB owner")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        `include "dtlb_0401_0402_input_defaults.svh"
        for(int p=0;p<8;p++)drive($sformatf("%s.inner_dtlbRepeater.io_tlb_req_%0d_valid",DUT,p),0);
        initialize_lookup_ports();
        if(scenario()=="live")live_permission_matrix();else run_permission_matrix();
        case(scenario())
            "s1":begin expected_checks=122;expected_returns=61;expected_windows=61;end
            "s2":begin expected_checks=122;expected_returns=61;expected_windows=61;end
            "priv":begin expected_checks=102;expected_returns=51;expected_windows=51;end
            "mxr":begin expected_checks=100;expected_returns=50;expected_windows=50;end
            "live":begin expected_checks=96;expected_returns=8;expected_windows=8;end
            default:begin expected_checks=0;expected_returns=0;expected_windows=0;end
        endcase
        if(token_valid||target_checks!=expected_checks||refills!=expected_returns||
           permission_windows+live_windows!=expected_windows||initialization_checks!=8)
            `uvm_error("DTLB0402_INCOMPLETE","request/return/window/init counts incomplete")
        cleanup();
        `uvm_info("DTLB0402_COMPLETE",$sformatf("scenario=%s requests=%0d returns=%0d windows=%0d initialization=%0d",scenario(),target_checks,refills,permission_windows+live_windows,initialization_checks),UVM_LOW)
    endtask
endclass
`define DTLB0402_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_dtlb_0401_0402_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`");super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB0402_CLASS(memblock_dtlb_04_s1_vseq,"s1")
`DTLB0402_CLASS(memblock_dtlb_04_s2_vseq,"s2")
`DTLB0402_CLASS(memblock_dtlb_04_priv_vseq,"priv")
`DTLB0402_CLASS(memblock_dtlb_04_mxr_vseq,"mxr")
`DTLB0402_CLASS(memblock_dtlb_04_live_vseq,"live")
`undef DTLB0402_CLASS
`endif
