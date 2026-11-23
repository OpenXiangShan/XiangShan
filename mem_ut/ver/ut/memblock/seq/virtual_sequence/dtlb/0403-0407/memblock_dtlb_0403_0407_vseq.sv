`ifndef MEMBLOCK_DTLB_0403_0407_VSEQ__SV
`define MEMBLOCK_DTLB_0403_0407_VSEQ__SV
// 抽象职责：通过真实请求及匹配返回验证阶段、异常优先级、PBMT和PMP交接。
// 只驱动声明input，consumer单列LoadUnit模块边界；不运行父body，不修改模型或输出。
class memblock_dtlb_0403_0407_vseq extends memblock_dtlb_0401_0402_vseq;
    `uvm_object_utils(memblock_dtlb_0403_0407_vseq)
    int unsigned checks_new,windows_new,extra_tokens,consumer_checks;
    function new(string name="memblock_dtlb_0403_0407_vseq");
        super.new(name);checks_new=0;windows_new=0;extra_tokens=0;consumer_checks=0;
    endfunction
    virtual function string scenario();return "fault";endfunction
    // 抽象职责：按输入合同比较真实异常、来源、PA、PBMT；AF/PF/GPF的正确输出属于PASS。
    task compare_result(input string ident,input int g,input int p,input bit [49:0] va,
                        input int cmd,input int fault,input int pbmt,input int bypass,
                        input int translated=1,input bit [47:0] pa=48'h40000000);
        string u,rsp;logic [5:0] actual,expected;uvm_hdl_data_t got_bypass,hit,miss;
        bit gpa_miss;
        u=unit_name(g);rsp=$sformatf("%s.io_requestor_%0d_resp",u,p);
        expected=fault==0?0:fault==4?(cmd==1||cmd==5?6'h20:6'h10):fault==1?(cmd==1||cmd==5?6'h02:6'h01):(cmd==1||cmd==5?6'h08:6'h04);
        actual=actual_exception(rsp);got_bypass=read_hdl({u,".readResult_p_hit_last_REG",p==0?"":$sformatf("_%0d",p)});
        hit=read_hdl($sformatf("%s._entries_io_r_resp_%0d_bits_hit",u,p));
        gpa_miss=translated&&active_stage[g]==3&&fault==2&&bypass==0;
        if(read_hdl({rsp,"_valid"})!==uvm_hdl_data_t'(1)||
           read_hdl({indexed(u,"req_out",p),"_vaddr"},50)!==uvm_hdl_data_t'(va)||
           read_hdl({indexed(u,"req_out",p),"_cmd"},3)!==uvm_hdl_data_t'(cmd))`uvm_error("DTLB0407_ASSOC",ident)
        if(actual!==expected)`uvm_error("DTLB0407_FAULT",$sformatf("%s exp=%0h got=%0h",ident,expected,actual))
        miss=read_hdl({rsp,"_bits_miss"});
        if(miss!==uvm_hdl_data_t'(gpa_miss))`uvm_error("DTLB0407_MISS",$sformatf("%s exp=%0d got=%0h",ident,gpa_miss,miss))
        if(bypass>=0&&(got_bypass!==uvm_hdl_data_t'(bypass)||(!bypass&&hit!==uvm_hdl_data_t'(1))))
            `uvm_error("DTLB0407_SOURCE",$sformatf("%s bypass=%0h hit=%0h",ident,got_bypass,hit))
        if(read_hdl({rsp,"_bits_pbmt_0"},2)!==uvm_hdl_data_t'(pbmt))
            `uvm_error("DTLB0407_PBMT",$sformatf("%s expected=%0d actual=%0h",ident,pbmt,read_hdl({rsp,"_bits_pbmt_0"},2)))
        if(fault==0&&read_hdl({rsp,"_bits_paddr_0"},48)!==uvm_hdl_data_t'(pa))
            `uvm_error("DTLB0407_PA",$sformatf("%s expected=%0h actual=%0h",ident,pa,read_hdl({rsp,"_bits_paddr_0"},48)))
        checks_new++;
        `uvm_info("DTLB0407_SAMPLE",$sformatf("%s g=%0d p=%0d cmd=%0d source=%0d fault=%0h miss=%0h pbmt=%0h pa=%0h n=%0d",ident,g,p,cmd,bypass,actual,miss,read_hdl({rsp,"_bits_pbmt_0"},2),read_hdl({rsp,"_bits_paddr_0"},48),checks_new),UVM_LOW)
    endtask
    // 抽象职责：用真实凭据构造故障/PBMT返回，分别比对旁路和storage；相异PBMT优先级独立case。
    task mapping_case(input string ident,input int g,input int p,input int cmd,input int stage,
                      input int fault,input bit [5:0] s1,input bit [4:0] s2,input bit pf1,input bit pf2,
                      input bit af1,input bit af2,input bit v1,input int pb1,input int pb2);
        string u;bit [49:0] va;bit [47:0] pa;int pb;
        u=unit_name(g);va=50'h80000000;pa=48'h40000000;
        configure_context(g,stage);prepare_miss(g,va);return_fields(g,va,pa,s1,pf1,s2,pf2);
        drive({u,".io_ptw_resp_bits_s1_entry_v"},v1);drive({u,".io_ptw_resp_bits_s1_af"},af1);
        drive({u,".io_ptw_resp_bits_s2_gaf"},af2);drive({u,".io_ptw_resp_bits_s1_entry_pbmt"},pb1);
        drive({u,".io_ptw_resp_bits_s2_entry_pbmt"},pb2);
        pb=stage==2?pb2:stage==3&&pb2!=0?pb2:pb1;
        target_request(g,p,va,cmd,0,0);drive({u,".io_ptw_resp_valid"},1);
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);
        drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);drive({u,".io_ptw_resp_valid"},0);token_valid=0;refills++;
        @(posedge clock_vif.clk);compare_result(ident,g,p,va,cmd,fault,pb,1);
        @(negedge clock_vif.clk);cycles(8);
        target_request(g,p,va,cmd,0,0);@(posedge clock_vif.clk);@(negedge clock_vif.clk);
        drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);@(posedge clock_vif.clk);
        compare_result(ident,g,p,va,cmd,fault,pb,0);@(negedge clock_vif.clk);
        drive({u,".reset"},1);cycles(2);windows_new++;
    endtask
    task mapping_matrix();
        `include "dtlb_0403_0407_scenarios.svh"
    endtask
    // 抽象职责：重复真实miss各产生一个返回凭据，第二个返回改变U/G，观察同VA权限不变。
    task stage_matrix();
        string u;bit [49:0] va;bit [47:0] pa;int cmd;
        va=50'h80000000;pa=48'h40000000;
        for(int g=0;g<3;g++)begin
            u=unit_name(g);cmd=g==1?1:0;configure_context(g,2);
            prepare_miss(g,va);token_valid=0;extra_tokens++;
            prepare_miss(g,va);return_fields(g,va,pa,6'h3f,0,5'h1f,0);
            drive({u,".io_ptw_resp_bits_s2_entry_perm_u"},0);drive({u,".io_ptw_resp_bits_s2_entry_perm_g"},0);
            for(int p=0;p<ports(g);p++)target_request(g,p,va,cmd,0,0);
            drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);@(negedge clock_vif.clk);
            drive({u,".io_ptw_resp_valid"},0);token_valid=0;refills++;
            for(int p=0;p<ports(g);p++)drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
            @(posedge clock_vif.clk);for(int p=0;p<ports(g);p++)compare_result("04.03.001_UG_first",g,p,va,cmd,0,0,1);
            @(negedge clock_vif.clk);cycles(8);
            for(int p=0;p<ports(g);p++)target_request(g,p,va,cmd,0,0);
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);
            for(int p=0;p<ports(g);p++)drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
            if(extra_tokens!=1)`uvm_fatal("DTLB0407_TOKEN","second real request token missing")
            drive({u,".io_ptw_resp_bits_s2_entry_perm_u"},1);drive({u,".io_ptw_resp_bits_s2_entry_perm_g"},1);
            drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);
            for(int p=0;p<ports(g);p++)compare_result("04.03.001_UG_changed",g,p,va,cmd,0,0,0);
            @(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);extra_tokens--;refills++;cycles(3);windows_new++;
        end
        for(int g=0;g<3;g++)begin
            if(g==1)continue;
            for(int stage=1;stage<=3;stage+=2)begin
                configure_context(g,stage);u=unit_name(g);
                for(int p=0;p<ports(g);p++)begin
                    target_request(g,p,va,0,0,0);drive($sformatf("%s.io_requestor_%0d_req_bits_no_translate",u,p),1);
                    if(!(g==2&&p==1))drive($sformatf("%s.io_requestor_%0d_req_bits_pmp_addr",u,p),pa);
                    @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
                    @(posedge clock_vif.clk);compare_result("04.03_stage_TE0",g,p,va,0,0,0,-1,0,(g==2&&p==1)?0:pa);
                    @(negedge clock_vif.clk);drive($sformatf("%s.io_requestor_%0d_req_bits_no_translate",u,p),0);cycles(2);
                end
                windows_new++;
            end
        end
        for(int g=0;g<3;g++)begin
            configure_context(g);u=unit_name(g);insert_mapping(g,va,pa);
            drive({u,".io_ptw_resp_bits_s1_entry_pbmt"},2); // 不无请求返回；为下面真实新地址请求构造新条目。
            configure_context(g);prepare_miss(g,va);return_fields(g,va,pa,6'h3f,0,5'h1f,0);
            drive({u,".io_ptw_resp_bits_s1_entry_pbmt"},2);drive({u,".io_ptw_resp_bits_s2_entry_pbmt"},1);
            drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);@(negedge clock_vif.clk);
            drive({u,".io_ptw_resp_valid"},0);token_valid=0;refills++;cycles(8);
            drive({u,".io_csr_priv_dmode"},3);cycles(6);
            for(int p=0;p<ports(g);p++)begin
                target_request(g,p,va,g==1?1:0,0,0);@(posedge clock_vif.clk);@(negedge clock_vif.clk);
                drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);@(posedge clock_vif.clk);
                compare_result("04.06.037_Bare_PMA",g,p,va,g==1?1:0,0,0,-1,0,va[47:0]);
                @(negedge clock_vif.clk);cycles(2);
            end
            windows_new++;
        end
    endtask
    task pmp_matrix();
        string u,q;bit [49:0] va;bit [47:0] pa;
        va=50'h80000000;pa=48'h40000000;
        for(int g=0;g<3;g++)begin
            for(int p=0;p<ports(g);p++)begin
                u=unit_name(g);q=$sformatf("%s.io_requestor_%0d_req",u,p);
                if(g!=1&&!(g==2&&p==1))begin
                    configure_context(g,1);insert_mapping(g,va,pa);
                    drive({u,".io_csr_priv_dmode"},3);drive({u,".io_csr_priv_spvp"},0);cycles(6);
                    target_request(g,p,va,0,1,1);drive({q,"_bits_isPrefetch"},1);
                    @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({q,"_valid"},0);
                    @(posedge clock_vif.clk);compare_result("04.07_prefetch_Hyper_HLVX",g,p,va,0,0,0,0);
                    if(read_hdl($sformatf("%s.io_pmpMode_%0d",u,p),2)!==uvm_hdl_data_t'(3)||
                       read_hdl($sformatf("%s.io_pmp_%0d_bits_cmd",u,p),3)!==uvm_hdl_data_t'(7)||
                       read_hdl($sformatf("%s.io_pmp_%0d_bits_addr",u,p),48)!==uvm_hdl_data_t'(pa))
                        `uvm_error("DTLB0407_PMP","prefetch Hyper/HLVX handoff differs")
                    @(negedge clock_vif.clk);drive({q,"_bits_isPrefetch"},0);windows_new++;
                end
                configure_context(g);drive({u,".io_csr_satp_mode"},0);cycles(6);
                target_request(g,p,va,g==1?1:0,0,0);@(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({q,"_valid"},0);
                @(posedge clock_vif.clk);compare_result("04.07_Bare_PMP",g,p,va,g==1?1:0,0,0,-1,0,va[47:0]);
                if(read_hdl($sformatf("%s.io_pmp_%0d_bits_addr",u,p),48)!==uvm_hdl_data_t'(va[47:0]))`uvm_error("DTLB0407_PMP","Bare PMP address mismatch")
                @(negedge clock_vif.clk);
                if(g!=1)begin
                    drive({q,"_bits_no_translate"},1);cycles(2);@(posedge clock_vif.clk);
                    if(read_hdl($sformatf("%s.io_requestor_%0d_resp_valid",u,p))!==uvm_hdl_data_t'(0)||
                       read_hdl($sformatf("%s.io_pmp_%0d_valid",u,p))!==uvm_hdl_data_t'(1))`uvm_error("DTLB0407_PMP","noTranslate no-response PMP valid missing")
                    checks_new++;`uvm_info("DTLB0407_SAMPLE",$sformatf("04.07_noresp g=%0d p=%0d",g,p),UVM_LOW)
                    @(negedge clock_vif.clk);drive({q,"_bits_no_translate"},0);
                end
                drive({u,".reset"},1);cycles(2);windows_new++;
            end
        end
    endtask
    // 抽象职责：所有consumer以真实输入事务更新mask历史；完整向量的knownness门控不能借force内部mask满足。
    task initialize_consumer_masks();
        string u;bit found;
        for(int index=0;index<5;index++)begin
            u=index<3?$sformatf("%s.inner_LoadUnit_%0d",DUT,index):$sformatf("%s.inner_StoreUnit_%0d",DUT,index-3);
            if(index<3)begin
                `include "dtlb_0403_0407_consumer_defaults.svh"
                drive({u,".io_ldout_ready"},1);drive({u,".io_dcache_req_ready"},1);
            end else begin
                `include "dtlb_0403_0407_store_defaults.svh"
            end
            drive({u,".reset"},1);cycles(8);drive({u,".reset"},0);cycles(4);
            drive({u,".io_tlb_resp_valid"},1);drive({u,".io_tlb_resp_bits_paddr_0"},48'h40000000);
            if(index<3)begin
                drive({u,".io_tlb_resp_bits_paddr_1"},48'h40000000);
                drive({u,".io_vecldin_bits_vaddr"},64'h80000000);drive({u,".io_vecldin_bits_basevaddr"},50'h80000000);
                drive({u,".io_vecldin_bits_mask"},16'hffff);drive({u,".io_vecldin_bits_vecActive"},1);
                drive({u,".io_vecldin_bits_alignedType"},4);drive({u,".io_vecldin_bits_uop_fuOpType"},9'h80);
                drive({u,".io_vecldin_valid"},1);
            end else begin
                drive({u,".io_stin_bits_uop_fuOpType"},MEMBLOCK_LSUOP_SD);
                drive({u,".io_stin_bits_src_0"},64'h80000000);drive({u,".io_stin_valid"},1);
            end
            found=0;
            for(int wait_cycles=0;wait_cycles<32;wait_cycles++)begin
                @(posedge clock_vif.clk);
                if(read_hdl({u,index<3?".io_vecldin_ready":".io_stin_ready"})===uvm_hdl_data_t'(1))begin found=1;break;end
            end
            if(!found)`uvm_fatal("DTLB0407_CONSUMER_INIT","real consumer initialization request not accepted")
            @(negedge clock_vif.clk);drive({u,index<3?".io_vecldin_valid":".io_stin_valid"},0);found=0;
            for(int wait_cycles=0;wait_cycles<32;wait_cycles++)begin
                @(posedge clock_vif.clk);
                if(read_hdl({u,".s2_valid"})===uvm_hdl_data_t'(1))begin found=1;break;end
            end
            if(!found)`uvm_fatal("DTLB0407_CONSUMER_INIT","consumer initialization never reached s2")
            if(read_hdl({u,index<3?".s2_in_r_mask":".s2_in_mask"},16)!==uvm_hdl_data_t'(index<3?16'hffff:16'h00ff))
                `uvm_error("DTLB0407_CONSUMER_INIT","real consumer mask initialization differs")
            @(negedge clock_vif.clk);drive({u,".reset"},1);cycles(4);
        end
    endtask

    // 抽象职责：真实LoadUnit向量输入产生16字节consumer事务，TLB/PMP用模块输入夹具提供响应。
    task consumer_matrix();
        string u;bit found;
        initialize_consumer_masks();
        u={DUT,".inner_LoadUnit_0"};
        `include "dtlb_0403_0407_consumer_defaults.svh"
        for(int mode=0;mode<2;mode++)begin
            drive({u,".reset"},1);cycles(8);drive({u,".reset"},0);cycles(4);
            drive({u,".io_dcache_req_ready"},1);drive({u,".io_tlb_resp_valid"},1);
            drive({u,".io_tlb_resp_bits_paddr_0"},48'h40000000);drive({u,".io_tlb_resp_bits_paddr_1"},48'h40000000);
            drive({u,".io_tlb_resp_bits_excp_0_pf_ld"},mode);drive({u,".io_pmp_ld"},mode);
            drive({u,".io_vecldin_bits_vaddr"},64'h80000000);drive({u,".io_vecldin_bits_basevaddr"},50'h80000000);
            drive({u,".io_vecldin_bits_mask"},16'hffff);drive({u,".io_vecldin_bits_vecActive"},1);
            drive({u,".io_vecldin_bits_alignedType"},4);drive({u,".io_vecldin_bits_uop_fuOpType"},9'h80);
            drive({u,".io_vecldin_valid"},1);found=0;
            for(int wait_cycles=0;wait_cycles<32;wait_cycles++)begin
                @(posedge clock_vif.clk);
                if(read_hdl({u,".io_vecldin_ready"})===uvm_hdl_data_t'(1))begin found=1;break;end
            end
            if(!found)`uvm_fatal("DTLB0407_CONSUMER","vector input never accepted")
            @(negedge clock_vif.clk);drive({u,".io_vecldin_valid"},0);found=0;
            for(int wait_cycles=0;wait_cycles<32;wait_cycles++)begin
                @(posedge clock_vif.clk);
                if(read_hdl({u,".s2_valid"})===uvm_hdl_data_t'(1))begin found=1;break;end
            end
            if(!found)`uvm_fatal("DTLB0407_CONSUMER","no actual consumer s2 transaction")
            if(read_hdl({u,".s2_in_r_mask"},16)!==uvm_hdl_data_t'(16'hffff)||
               read_hdl({u,".s2_in_r_paddr"},48)!==uvm_hdl_data_t'(48'h40000000)||
               read_hdl({u,".s2_vecActive"})!==uvm_hdl_data_t'(1)||
               read_hdl({u,".s2_in_r_uop_exceptionVec_13"})!==uvm_hdl_data_t'(mode)||
               read_hdl({u,".s2_exception_vec_5"})!==uvm_hdl_data_t'(mode))
                `uvm_error("DTLB0407_CONSUMER","consumer mask/PA/PF/PMP AF differs")
            checks_new++;consumer_checks++;windows_new++;
            `uvm_info("DTLB0407_SAMPLE",$sformatf("04.07_CONSUMER mode=%0d span=16 PF=%0h AF=%0h",mode,read_hdl({u,".s2_in_r_uop_exceptionVec_13"}),read_hdl({u,".s2_exception_vec_5"})),UVM_LOW)
            @(negedge clock_vif.clk);drive({u,".reset"},1);cycles(4);
        end
    endtask
    virtual task body();
        int expected_checks;
        require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))`uvm_fatal("DTLB0407_CLOCK","missing real clock")
        if(seq_csr_common::get_l2tlb_seq_en()||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)`uvm_fatal("DTLB0407_OWNER","local fixture and global responder conflict")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        `include "dtlb_0403_0407_input_defaults.svh"
        for(int p=0;p<8;p++)drive($sformatf("%s.inner_dtlbRepeater.io_tlb_req_%0d_valid",DUT,p),0);
        initialize_lookup_ports();
        case(scenario())
            "stage":stage_matrix();"pmp":pmp_matrix();"consumer":consumer_matrix();
            default:mapping_matrix();
        endcase
        case(scenario())
            "fault":expected_checks=108;
            "special":expected_checks=48;
            "pbmt":expected_checks=544;
            "pbmt_priority":expected_checks=32;
            "stage":expected_checks=36;
            "pmp":expected_checks=19;
            "consumer":expected_checks=2;
            default:expected_checks=0;
        endcase
        if(token_valid||extra_tokens||checks_new!=expected_checks||initialization_checks!=8)`uvm_error("DTLB0407_INCOMPLETE","target/init/token counts incomplete")
        cleanup();
        `uvm_info("DTLB0407_COMPLETE",$sformatf("scenario=%s requests=%0d returns=%0d windows=%0d initialization=%0d consumer=%0d",scenario(),checks_new,refills,windows_new,initialization_checks,consumer_checks),UVM_LOW)
    endtask
endclass
`define DTLB0407_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_dtlb_0403_0407_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`");super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB0407_CLASS(memblock_dtlb_04_fault_vseq,"fault")
`DTLB0407_CLASS(memblock_dtlb_04_special_vseq,"special")
`DTLB0407_CLASS(memblock_dtlb_04_pbmt_vseq,"pbmt")
`DTLB0407_CLASS(memblock_dtlb_04_pbmt_priority_vseq,"pbmt_priority")
`DTLB0407_CLASS(memblock_dtlb_04_stage_vseq,"stage")
`DTLB0407_CLASS(memblock_dtlb_04_pmp_vseq,"pmp")
`DTLB0407_CLASS(memblock_dtlb_04_consumer_vseq,"consumer")
`undef DTLB0407_CLASS
`endif
