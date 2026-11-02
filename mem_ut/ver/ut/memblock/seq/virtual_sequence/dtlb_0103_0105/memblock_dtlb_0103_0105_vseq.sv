`ifndef MEMBLOCK_DTLB_0103_0105_VSEQ__SV
`define MEMBLOCK_DTLB_0103_0105_VSEQ__SV

// DTLB声明输入边界的有限定向测试；只继承既有HDL/时钟/reset helper。
// 不调用父场景body，不启动公共dispatch/L2TLB responder，不驱动DTLB输出或覆盖模型。
class memblock_dtlb_0103_0105_vseq extends memblock_dtlb_0101_source_vseq;
    `uvm_object_utils(memblock_dtlb_0103_0105_vseq)
    // 每次只保留一个已观测DTLB miss token；prepare_return消费其VPN/stage快照。
    bit pending_return;
    bit [37:0] pending_vpn;
    bit [1:0] pending_stage;
    string pending_unit;
    // 有限窗口的真实检查计数，body结束前核对非零；不作为覆盖命中的替代。
    int unsigned checks, returns_checked, cases_checked;

    function new(string name="memblock_dtlb_0103_0105_vseq");
        super.new(name);pending_return=0;checks=0;returns_checked=0;cases_checked=0;
    endfunction
    virtual function string scenario();return "redirect";endfunction
    function string unit_name(input int group);
        case(group)
            0:return {DUT,".inner_dtlb_ld_tlb_ld"};
            1:return {DUT,".inner_dtlb_st_tlb_st"};
            2:return {DUT,".inner_dtlb_prefetch_tlb_prefetch"};
            default:begin `uvm_fatal("DTLB0305_GROUP","unknown group") return "";end
        endcase
    endfunction
    function int port_count(input int group);return group==0?4:2;endfunction

    // 初始化实际存在的输入字段；生成列表来自当前V2 RTL头部，不会触及output。
    task clear_inputs();
        `include "dtlb_0103_0105_input_defaults.svh"
        // 本例以DTLB为独立DUT边界：下游PTWNewFilter的8个声明输入置idle。
        // 不改变DTLB的PTW输出，request_case仍在真实PTW输出采集唯一token。
        // 防止局部miss误进入没有启动全局responder的MemBlock通路；不关闭监视器。
        for(int port_id=0;port_id<8;port_id++)
            drive($sformatf("%s.inner_dtlbRepeater.io_tlb_req_%0d_valid",DUT,port_id),0);
    endtask

    task configure_context(input bit translated);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_satp_mode",translated?8:0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_satp_asid",0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_vsatp_mode",0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_hgatp_mode",0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_priv_virt",0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_priv_dmode",translated?0:3);
        cycles(12);
        for(int group=0;group<3;group++) begin
            if(read_hdl({unit_name(group),".io_csr_satp_mode"},4)!==uvm_hdl_data_t'(translated?8:0))
                `uvm_fatal("DTLB0305_CSR","context did not reach DUT")
        end
    endtask

    // 仅以已观测的PTW请求构造4KB identity映射；返回在模块的输入边界完成。
    // 原始MemBlock全局filter的在途状态由结尾整体reset取消，不冒充全局L2TLB完成。
    task prepare_return(input string unit_path,input bit [49:0] address);
        if(!pending_return || pending_unit!=unit_path || pending_vpn!==address[49:12] || pending_stage!=0)
            `uvm_fatal("DTLB0305_TOKEN","PTW return has no matching observed miss")
        drive({unit_path,".io_ptw_resp_bits_s2xlate"},pending_stage);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_tag"},address[49:15]);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_ppn"},address[47:15]);
        drive({unit_path,".io_ptw_resp_bits_s1_addr_low"},address[14:12]);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_asid"},0);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_v"},1);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_d"},1);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_a"},1);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_g"},0);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_u"},1);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_x"},1);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_w"},1);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_r"},1);
        for(int sub=0;sub<8;sub++) begin
            drive($sformatf("%s.io_ptw_resp_bits_s1_ppn_low_%0d",unit_path,sub),sub);
            drive($sformatf("%s.io_ptw_resp_bits_s1_valididx_%0d",unit_path,sub),1);
            drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",unit_path,sub),1);
        end
        drive({unit_path,".io_ptw_resp_valid"},1);
    endtask

    // 抽象职责：一拍请求/下一拍响应配对；可合法叠加request kill、late kill或redirect。
    // kind:-1不检查TLB hit来源，0=storage，1=旁路，2=miss；不读覆盖模型状态。
    task request_case(input int group,input int port_id,input bit [49:0] address,
                      input bit req_kill=0,input bit late_kill=0,input int kind=-1,
                      input bit with_return=0,input bit capture_miss=0,
                      input bit red_valid=0,input bit [8:0] req_rob=0,
                      input bit [8:0] red_rob=0,input bit red_level=0);
        string unit_path,prefix,hit_signal,bypass_signal;
        bit [2:0] cmd;
        uvm_hdl_data_t hit,bypass,miss,rv,actual_vpn,stage;
        unit_path=unit_name(group);prefix=$sformatf("%s.io_requestor_%0d_req",unit_path,port_id);cmd=group==1?1:0;
        if(group==1 && (req_kill||late_kill)) `uvm_fatal("DTLB0305_CAP","Store has no kill input")
        if((group!=0||port_id==3)&&late_kill) `uvm_fatal("DTLB0305_CAP","selected port has no late kill")
        drive({prefix,"_bits_vaddr"},address);drive({prefix,"_bits_cmd"},cmd);
        // PF1的输入没有debug ROB字段；其覆盖绑定固定为0，不能伪造。
        if(!(group==2&&port_id==1)) begin
            drive({prefix,"_bits_debug_robIdx_flag"},req_rob[8]);
            drive({prefix,"_bits_debug_robIdx_value"},req_rob[7:0]);
        end else if(req_rob!=0) `uvm_fatal("DTLB0305_CAP","PF1 ROB must remain zero")
        if(group!=1) drive({prefix,"_bits_kill"},req_kill);
        if(with_return) prepare_return(unit_path,address);
        drive({prefix,"_valid"},1);
        @(posedge clock_vif.clk);
        if(read_hdl({prefix,"_valid"})!==uvm_hdl_data_t'(1)||read_hdl({prefix,"_bits_vaddr"},50)!==uvm_hdl_data_t'(address))
            `uvm_error("DTLB0305_REQ","request input not observed at sample")
        @(negedge clock_vif.clk);
        drive({prefix,"_valid"},0);
        if(with_return) begin
            drive({unit_path,".io_ptw_resp_valid"},0);pending_return=0;returns_checked++;
        end
        if(group==0&&port_id<3) drive($sformatf("%s.io_requestor_%0d_req_kill",unit_path,port_id),late_kill);
        drive({unit_path,".io_redirect_bits_robIdx_flag"},red_rob[8]);
        drive({unit_path,".io_redirect_bits_robIdx_value"},red_rob[7:0]);
        drive({unit_path,".io_redirect_bits_level"},red_level);
        drive({unit_path,".io_redirect_valid"},red_valid);
        @(posedge clock_vif.clk);
        rv=read_hdl($sformatf("%s.io_requestor_%0d_resp_valid",unit_path,port_id));
        if(rv!==uvm_hdl_data_t'(!req_kill))
            `uvm_error("DTLB0305_VALID",$sformatf("g=%0d p=%0d kill=%0d resp=%0h",group,port_id,req_kill,rv))
        if(!req_kill) begin
            if(read_hdl($sformatf("%s.req_out_%0d_vaddr",unit_path,port_id),50)!==uvm_hdl_data_t'(address)||
               read_hdl($sformatf("%s.req_out_%0d_cmd",unit_path,port_id),3)!==uvm_hdl_data_t'(cmd))
                `uvm_error("DTLB0305_PAYLOAD","response VA/cmd mismatch")
            if(!(group==2&&port_id==1) &&
                (read_hdl($sformatf("%s.req_out_%0d_debug_robIdx_flag",unit_path,port_id))!==uvm_hdl_data_t'(req_rob[8]) ||
                 read_hdl($sformatf("%s.req_out_%0d_debug_robIdx_value",unit_path,port_id),8)!==uvm_hdl_data_t'(req_rob[7:0])))
                `uvm_error("DTLB0305_ROB","full ROB pointer did not match previous request")
            if((scenario()=="copies" || scenario()=="redirect" || kind==0 || kind==1) &&
                (read_hdl($sformatf("%s.io_requestor_%0d_resp_bits_miss",unit_path,port_id))!==uvm_hdl_data_t'(0) ||
                 read_hdl($sformatf("%s.io_requestor_%0d_resp_bits_paddr_0",unit_path,port_id),48)!==uvm_hdl_data_t'(address[47:0])))
                `uvm_error("DTLB0305_ADDRESS","expected non-miss identity-mapped physical address")
            if(group==0&&port_id<3&&scenario()=="copies") begin
                if(read_hdl($sformatf("%s.io_requestor_%0d_resp_bits_paddr_0",unit_path,port_id),48)!==read_hdl($sformatf("%s.io_requestor_%0d_resp_bits_paddr_1",unit_path,port_id),48))
                    `uvm_error("DTLB0305_COPY","actual paddr copies differ")
            end
        end
        hit_signal=$sformatf("%s._entries_io_r_resp_%0d_bits_hit",unit_path,port_id);
        bypass_signal={unit_path,".readResult_p_hit_last_REG",port_id==0?"":$sformatf("_%0d",port_id)};
        hit=read_hdl(hit_signal);bypass=read_hdl(bypass_signal);
        miss=read_hdl($sformatf("%s.io_requestor_%0d_resp_bits_miss",unit_path,port_id));
        if(kind==0 && (hit!==uvm_hdl_data_t'(1)||bypass!==uvm_hdl_data_t'(0)))
            `uvm_error("DTLB0305_STORAGE",$sformatf("g=%0d p=%0d hit=%0h bypass=%0h",group,port_id,hit,bypass))
        if(kind==1 && bypass!==uvm_hdl_data_t'(1))
            `uvm_error("DTLB0305_BYPASS",$sformatf("g=%0d p=%0d bypass=%0h",group,port_id,bypass))
        if(kind==2 && (hit!==uvm_hdl_data_t'(0)||bypass!==uvm_hdl_data_t'(0)))
            `uvm_error("DTLB0305_MISS","expected neither storage nor bypass hit")
        if(req_kill && read_hdl($sformatf("%s.io_ptw_req_%0d_valid",unit_path,port_id))!==uvm_hdl_data_t'(0))
            `uvm_error("DTLB0305_KILL","killed request issued an unexpected PTW query")
        if(late_kill) begin
            if(read_hdl($sformatf("%s.io_ptw_req_%0d_valid",unit_path,port_id))!==uvm_hdl_data_t'(0)||read_hdl($sformatf("%s.io_tlbreplay_%0d",unit_path,port_id))!==uvm_hdl_data_t'(1))
                `uvm_error("DTLB0305_LATE","late kill did not suppress PTW/replay")
        end
        if(red_valid) begin
            bit [8:0] sampled_rob;
            sampled_rob=(group==2&&port_id==1)?0:
                {1'(read_hdl({prefix,"_bits_debug_robIdx_flag"})),8'(read_hdl({prefix,"_bits_debug_robIdx_value"},8))};
            if(sampled_rob!==req_rob ||
               read_hdl({unit_path,".io_redirect_valid"})!==uvm_hdl_data_t'(1) ||
               read_hdl({unit_path,".io_redirect_bits_robIdx_flag"})!==uvm_hdl_data_t'(red_rob[8]) ||
               read_hdl({unit_path,".io_redirect_bits_robIdx_value"},8)!==uvm_hdl_data_t'(red_rob[7:0]) ||
               read_hdl({unit_path,".io_redirect_bits_level"})!==uvm_hdl_data_t'(red_level))
                `uvm_error("DTLB0305_REDIRECT","redirect/ROB sampled input differs from scheduled matrix")
        end
        if(scenario()=="copies" && miss!==uvm_hdl_data_t'(0))
            `uvm_error("DTLB0305_COPY","copy/association check requires a non-miss Bare response")
        if(capture_miss) begin
            if(pending_return || read_hdl($sformatf("%s.io_ptw_req_%0d_valid",unit_path,port_id))!==uvm_hdl_data_t'(1))
                `uvm_fatal("DTLB0305_TOKEN","real miss request not visible")
            actual_vpn=read_hdl($sformatf("%s.io_ptw_req_%0d_bits_vpn",unit_path,port_id),38);
            stage=read_hdl($sformatf("%s.io_ptw_req_%0d_bits_s2xlate",unit_path,port_id),2);
            if(actual_vpn!==uvm_hdl_data_t'(address[49:12])||stage!==uvm_hdl_data_t'(0))
                `uvm_fatal("DTLB0305_TOKEN","miss VPN/stage does not match captured request")
            pending_return=1;pending_vpn=38'(actual_vpn);pending_stage=2'(stage);pending_unit=unit_path;
        end
        checks++;
        `uvm_info("DTLB0305_SAMPLE",$sformatf("scenario=%s group=%0d port=%0d va=%0h kill/late=%0d/%0d hit/bypass=%0h/%0h redirect=%0d reqrob=%0h redrob=%0h level=%0d",scenario(),group,port_id,address,req_kill,late_kill,hit,bypass,red_valid,req_rob,red_rob,red_level),UVM_LOW)
        @(negedge clock_vif.clk);
        drive({unit_path,".io_redirect_valid"},0);
        if(group==0&&port_id<3) drive($sformatf("%s.io_requestor_%0d_req_kill",unit_path,port_id),0);
        if(group!=1) drive({prefix,"_bits_kill"},0);
        cycles(3);
    endtask

    task cancellation_matrix(input bit query_only);
        bit [49:0] address,second;
        int group,start_port,end_port;
        group=query_only?2:0;start_port=query_only?1:0;end_port=query_only?2:3;
        for(int port_id=start_port;port_id<end_port;port_id++) begin
            address=50'h80010000+50'(port_id)*50'h20000;
            // 不先回填时的kill+miss，不生成待返回token。
            request_case(group,port_id,address,1,0,2);
            request_case(group,port_id,address,0,0,2,0,1);
            request_case(group,port_id,address,1,0,1,1);
            cycles(8);
            request_case(group,port_id,address,1,0,0);
            request_case(group,port_id,address,0,0,0);
            if(!query_only) begin
                second=address+50'h10000;
                request_case(group,port_id,second,0,0,2,0,1);
                request_case(group,port_id,second,0,1,1,1);
                cycles(8);
                request_case(group,port_id,second,0,1,0);
            end
            cases_checked++;
        end
    endtask

    task redirect_matrix();
        bit [8:0] reqrob,redrob;
        for(int group=0;group<3;group++) begin
            for(int port_id=0;port_id<port_count(group);port_id++) begin
                reqrob=(group==2||(group==0&&port_id==3))?9'd0:9'h40;
                for(int relation=0;relation<3;relation++) begin
                    redrob=relation==0?reqrob:relation==1?reqrob+9'd1:reqrob==0?9'h1ff:reqrob-9'd1;
                    for(int level=0;level<2;level++) begin
                        request_case(group,port_id,50'h80080000+50'(cases_checked)*64,0,0,-1,0,0,1,reqrob,redrob,1'(level));
                        cases_checked++;
                    end
                end
                if(reqrob!=0) begin
                    request_case(group,port_id,50'h800f0000,0,0,-1,0,0,1,9'h102,9'h0fa,0);
                    request_case(group,port_id,50'h800f1000,0,0,-1,0,0,1,9'h0fa,9'h102,1);
                    cases_checked+=2;
                end
            end
        end
    endtask

    task copy_matrix();
        for(int group=0;group<3;group++) begin
            for(int port_id=0;port_id<port_count(group);port_id++) begin
                request_case(group,port_id,50'h80090000+50'(cases_checked)*64);
                cases_checked++;
            end
        end
    endtask

    virtual task body();
        require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))
            `uvm_fatal("DTLB0305_CLOCK","missing real issue clock")
        if(seq_csr_common::get_l2tlb_seq_en()||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)
            `uvm_fatal("DTLB0305_OWNER","local DTLB input test cannot mix with active L2TLB owner")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        clear_inputs();configure_context(scenario()=="kill_ld"||scenario()=="kill_query");
        case(scenario())
            "kill_ld":cancellation_matrix(0);
            "kill_query":cancellation_matrix(1);
            "redirect":redirect_matrix();
            "copies":copy_matrix();
            default:`uvm_fatal("DTLB0305_SCENARIO",scenario())
        endcase
        if(checks!=(scenario()=="kill_ld"?24:scenario()=="kill_query"?5:scenario()=="copies"?8:58) ||
           returns_checked!=(scenario()=="kill_ld"?6:scenario()=="kill_query"?1:0))
            `uvm_error("DTLB0305_INCOMPLETE","scenario did not execute all required request/return checks")
        if(pending_return) `uvm_error("DTLB0305_TOKEN","unconsumed local PTW return token")
        cleanup();
        `uvm_info("DTLB0305_COMPLETE",$sformatf("scenario=%s requests=%0d cases=%0d returned=%0d",scenario(),checks,cases_checked,returns_checked),UVM_LOW)
    endtask
endclass

`define DTLB0305_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_dtlb_0103_0105_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`");super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB0305_CLASS(memblock_dtlb_0103_kill_ld_vseq,"kill_ld")
`DTLB0305_CLASS(memblock_dtlb_0103_kill_query_vseq,"kill_query")
`DTLB0305_CLASS(memblock_dtlb_0104_copies_vseq,"copies")
`DTLB0305_CLASS(memblock_dtlb_0105_redirect_vseq,"redirect")
`undef DTLB0305_CLASS
`endif
