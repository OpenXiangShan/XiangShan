`ifndef MEMBLOCK_DTLB_0201_0204_VSEQ__SV
`define MEMBLOCK_DTLB_0201_0204_VSEQ__SV

// 抽象职责：在真实DTLB声明输入边界检验翻译选择、地址预检查及指针屏蔽。
// 仅复用既有HDL/真实时钟/整体reset释放helper，不运行父类场景。
class memblock_dtlb_0201_0204_vseq extends memblock_dtlb_0101_source_vseq;
    `uvm_object_utils(memblock_dtlb_0201_0204_vseq)
    // 本次完成的实际请求检查数；与场景静态计划数量在body结尾比较。
    int unsigned checks, ptw_observed, canceled_windows, returned_tokens;
    function new(string name="memblock_dtlb_0201_0204_vseq");
        super.new(name);checks=0;ptw_observed=0;canceled_windows=0;returned_tokens=0;
    endfunction
    virtual function string scenario();return "host";endfunction
    function string unit_name(input int group);
        case(group)
            0:return {DUT,".inner_dtlb_ld_tlb_ld"};
            1:return {DUT,".inner_dtlb_st_tlb_st"};
            2:return {DUT,".inner_dtlb_prefetch_tlb_prefetch"};
            default:begin `uvm_fatal("DTLB0204_GROUP","invalid group") return "";end
        endcase
    endfunction
    function bit fullva_output(input int group,input int port_id);
        return group==1 || (group==0 && port_id<3);
    endfunction
    function string indexed(input string unit_path,input string field,input int port_id);
        return $sformatf("%s.%s_%0d",unit_path,field,port_id);
    endfunction
    // 实际存在的异常输出全部读取；不存在的方向不能虚构为可驱动端口。
    function logic [5:0] actual_exception(input string response);
        string names[6];logic [5:0] result;
        names='{"pf_ld","pf_st","gpf_ld","gpf_st","af_ld","af_st"};result=0;
        for(int i=0;i<6;i++) begin
            if(uvm_hdl_check_path({response,"_bits_excp_0_",names[i]}))
                result[i]=1'(read_hdl({response,"_bits_excp_0_",names[i]}));
        end
        return result;
    endfunction

    // 抽象职责：先发真实预异常请求取得VPN/stage凭据，再在声明PTW输入返回匹配4KB映射。
    // 使用正常load/store命令，fullva仅翻转bit63形成预PF/GPF；低50位和待测VPN保持一致。
    // PF1没有fullva/check字段时由同模块PF0的真实请求准备共享表项。
    task warm_translation(input string unit_path,input string req,input int port_id,
                          input bit [49:0] address,input bit [1:0] stage,
                          input bit [63:0] fullva,input bit [2:0] command);
        uvm_hdl_data_t vpn_seen,stage_seen;
        string warm_req;int warm_port;
        warm_req=req;warm_port=port_id;
        if(!uvm_hdl_check_path({req,"_bits_checkfullva"})) begin
            warm_port=0;warm_req={unit_path,".io_requestor_0_req"};
            drive({warm_req,"_bits_vaddr"},address);drive({warm_req,"_bits_hyperinst"},0);
        end
        drive({warm_req,"_bits_cmd"},command);
        drive({warm_req,"_bits_fullva"},fullva^64'h8000000000000000);
        drive({warm_req,"_bits_checkfullva"},1);
        drive({warm_req,"_valid"},1);@(posedge clock_vif.clk);
        @(negedge clock_vif.clk);drive({warm_req,"_valid"},0);@(posedge clock_vif.clk);
        vpn_seen=read_hdl($sformatf("%s.io_ptw_req_%0d_bits_vpn",unit_path,warm_port),38);
        stage_seen=read_hdl($sformatf("%s.io_ptw_req_%0d_bits_s2xlate",unit_path,warm_port),2);
        if(read_hdl($sformatf("%s.io_ptw_req_%0d_valid",unit_path,warm_port))!==uvm_hdl_data_t'(1) ||
           vpn_seen!==uvm_hdl_data_t'(address[49:12]) || stage_seen!==uvm_hdl_data_t'(stage))
            `uvm_fatal("DTLB0204_TOKEN","local return requires a matching actual DTLB PTW request")
        @(negedge clock_vif.clk);
        drive({unit_path,".io_ptw_resp_bits_s2xlate"},stage);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_tag"},address[49:15]);
        drive({unit_path,".io_ptw_resp_bits_s1_addr_low"},address[14:12]);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_ppn"},address[47:15]);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_v"},1);
        drive({unit_path,".io_ptw_resp_bits_s2_entry_tag"},address[49:12]);
        drive({unit_path,".io_ptw_resp_bits_s2_entry_ppn"},address[47:12]);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_d"},1);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_a"},1);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_u"},1);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_x"},1);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_w"},1);
        drive({unit_path,".io_ptw_resp_bits_s1_entry_perm_r"},1);
        drive({unit_path,".io_ptw_resp_bits_s2_entry_perm_d"},1);
        drive({unit_path,".io_ptw_resp_bits_s2_entry_perm_a"},1);
        drive({unit_path,".io_ptw_resp_bits_s2_entry_perm_u"},1);
        drive({unit_path,".io_ptw_resp_bits_s2_entry_perm_x"},1);
        drive({unit_path,".io_ptw_resp_bits_s2_entry_perm_w"},1);
        drive({unit_path,".io_ptw_resp_bits_s2_entry_perm_r"},1);
        for(int sub=0;sub<8;sub++) begin
            drive($sformatf("%s.io_ptw_resp_bits_s1_ppn_low_%0d",unit_path,sub),sub);
            drive($sformatf("%s.io_ptw_resp_bits_s1_valididx_%0d",unit_path,sub),1);
            drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",unit_path,sub),1);
        end
        drive({unit_path,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);
        @(negedge clock_vif.clk);drive({unit_path,".io_ptw_resp_valid"},0);cycles(8);
        returned_tokens++;
    endtask

    // 抽象职责：建立单个隔离请求窗口，并将所有输出与独立计算的预期进行比较。
    // 合法翻译路径先以真实请求凭据返回；窗口结束reset清理，不冒充全局页表遍历完成。
    task execute_case(input string ident,input int group,input int port_id,input bit [2:0] cmd,
                      input bit [63:0] fullva,input int satp,input int vsatp,input int hgatp,
                      input bit virt,input int imode,input int dmode,input bit hyper,input bit spvp,
                      input int pmm,input bit checkfull,input bit no_translate,input bit virt_flip);
        string unit_path,req,rsp;
        bit guest,pre_s1,pre_guest,translated,sign_ext,bad;
        bit [1:0] stage,mode,actual_mode;
        bit [63:0] effective;
        bit [49:0] vaddr;
        bit [47:0] pmpaddr;
        bit [2:0] fault;
        logic [5:0] expected_excp,actual_excp;
        uvm_hdl_data_t got,miss;
        unit_path=unit_name(group);req=$sformatf("%s.io_requestor_%0d_req",unit_path,port_id);
        rsp=$sformatf("%s.io_requestor_%0d_resp",unit_path,port_id);
        // 窗口间reset清理阵列/GPA/未回填miss及模型历史；覆盖计数自身不复位。
        drive({unit_path,".reset"},1);
        drive({unit_path,".io_csr_satp_mode"},satp);
        drive({unit_path,".io_csr_vsatp_mode"},vsatp);
        drive({unit_path,".io_csr_hgatp_mode"},hgatp);
        drive({unit_path,".io_csr_priv_virt"},virt);
        drive({unit_path,".io_csr_priv_virt_changed"},0);
        drive({unit_path,".io_csr_priv_imode"},imode);
        drive({unit_path,".io_csr_priv_dmode"},dmode);
        drive({unit_path,".io_csr_priv_spvp"},spvp);
        drive({unit_path,".io_csr_priv_sum"},1);
        drive({unit_path,".io_csr_priv_vsum"},1);
        // 本矩阵检查各上下文的EVA变换；五个PMM CSR相等，不宣称完成不同CSR值的选择器互斥验证。
        drive({unit_path,".io_csr_pmm_mseccfg"},pmm);
        drive({unit_path,".io_csr_pmm_menvcfg"},pmm);
        drive({unit_path,".io_csr_pmm_henvcfg"},pmm);
        drive({unit_path,".io_csr_pmm_hstatus"},pmm);
        drive({unit_path,".io_csr_pmm_senvcfg"},pmm);
        cycles(6);drive({unit_path,".reset"},0);cycles(4);canceled_windows++;
        mode=hyper?{1'b0,spvp}:2'(dmode);guest=virt||hyper;
        stage=!guest?0:vsatp==0?2:hgatp==0?1:3;
        pre_s1=!guest && satp inside {8,9} && mode!=3;
        pre_guest=guest && (vsatp inside {8,9} || hgatp inside {8,9}) && mode!=3;
        sign_ext=pre_s1 || (pre_guest&&vsatp!=0);
        effective=fullva;
        if(pmm==2) effective=sign_ext?{{7{fullva[56]}},fullva[56:0]}:{7'b0,fullva[56:0]};
        if(pmm==3) effective=sign_ext?{{16{fullva[47]}},fullva[47:0]}:{16'b0,fullva[47:0]};
        vaddr=effective[49:0];pmpaddr=48'h12345000+48'(port_id)*64;
        translated=(pre_s1||pre_guest)&&!no_translate;
        fault=0;
        if(checkfull) begin
            if(!(pre_s1||pre_guest)) begin
                if(effective[63:48]!=0) fault=3'b100;
            end else if(stage==2) begin
                if((hgatp==8&&effective[63:41]!=0)||(hgatp==9&&effective[63:50]!=0))fault=3'b010;
            end else begin
                if(((stage==0?satp:vsatp)==8 && effective!={{25{effective[38]}},effective[38:0]}) ||
                   ((stage==0?satp:vsatp)==9 && effective!={{16{effective[47]}},effective[47:0]}))fault=3'b001;
            end
        end
        expected_excp=cmd==1?{fault[2],1'b0,fault[1],1'b0,fault[0],1'b0}:
                                          {1'b0,fault[2],1'b0,fault[1],1'b0,fault[0]};
        drive({req,"_bits_vaddr"},vaddr);drive({req,"_bits_cmd"},cmd);
        if(!(group==2&&port_id==1)) begin
            drive({req,"_bits_fullva"},fullva);drive({req,"_bits_checkfullva"},checkfull);
            drive({req,"_bits_hyperinst"},hyper);
        end
        if(group!=1) begin
            drive({req,"_bits_no_translate"},no_translate);
            if(!(group==2&&port_id==1)) drive({req,"_bits_pmp_addr"},pmpaddr);
        end
        if(translated && fault==0) begin
            warm_translation(unit_path,req,port_id,vaddr,stage,fullva,group==2?3'd0:cmd);
            if(!(group==2&&port_id==1))drive({req,"_bits_fullva"},fullva);
            drive({req,"_bits_cmd"},cmd);
            if(uvm_hdl_check_path({req,"_bits_checkfullva"}))drive({req,"_bits_checkfullva"},checkfull);
        end
        drive({req,"_valid"},1);
        @(posedge clock_vif.clk);
        if(read_hdl({req,"_valid"})!==uvm_hdl_data_t'(1)) `uvm_error("DTLB0204_REQ","request not sampled")
        @(negedge clock_vif.clk);drive({req,"_valid"},0);
        if(virt_flip) begin
            // 只改变模块声明raw输入；有效请求之后不再发请求，紧接reset清理。
            drive({unit_path,".io_csr_priv_virt"},!virt);drive({unit_path,".io_csr_priv_virt_changed"},1);
        end
        @(posedge clock_vif.clk);
        if(read_hdl({rsp,"_valid"})!==uvm_hdl_data_t'(1) ||
           read_hdl({indexed(unit_path,"req_out",port_id),"_vaddr"},50)!==uvm_hdl_data_t'(vaddr) ||
           read_hdl({indexed(unit_path,"req_out",port_id),"_cmd"},3)!==uvm_hdl_data_t'(cmd))
            `uvm_error("DTLB0204_ASSOC",ident)
        if(read_hdl(indexed(unit_path,"req_out_s2xlate",port_id),2)!==uvm_hdl_data_t'(stage) ||
           read_hdl(indexed(unit_path,"virt_out",port_id))!==uvm_hdl_data_t'(virt))
            `uvm_error("DTLB0204_STAGE",ident)
        got=group==1?(read_hdl({unit_path,".s1_valid"})|read_hdl({unit_path,".s2_valid"})):
                      read_hdl(indexed(unit_path,"portTranslateEnable",port_id));
        if(got!==uvm_hdl_data_t'(translated)) `uvm_error("DTLB0204_ENABLE",ident)
        actual_mode=(group==2&&port_id==1)?2'(read_hdl({unit_path,"._csr_delay_io_out_priv_dmode"},2)):
            2'(read_hdl(indexed(unit_path,"mode",port_id),2));
        if(actual_mode!==mode) `uvm_error("DTLB0204_MODE",ident)
        actual_excp=actual_exception(rsp);
        if(actual_excp!==expected_excp)
            `uvm_error("DTLB0204_EXCEPTION",$sformatf("%s expected=%0h actual=%0h",ident,expected_excp,actual_excp))
        miss=read_hdl({rsp,"_bits_miss"});
        if(miss!==uvm_hdl_data_t'(0))
            `uvm_error("DTLB0204_MISS",$sformatf("%s expected=%0d actual=%0h",ident,0,miss))
        if(translated&&fault==0 && read_hdl({rsp,"_bits_paddr_0"},48)!==uvm_hdl_data_t'(vaddr[47:0]))
            `uvm_error("DTLB0204_TRANSLATED_PA",ident)
        if(!translated&&fault==0 && (read_hdl({rsp,"_bits_paddr_0"},48)!==uvm_hdl_data_t'(no_translate?pmpaddr:vaddr[47:0]) ||
                                   read_hdl({rsp,"_bits_pbmt_0"},2)!==uvm_hdl_data_t'(0)))
            `uvm_error("DTLB0204_PA",ident)
        if(!(group==2&&port_id==1) && read_hdl(indexed(unit_path,"EffectiveVa",port_id),64)!==uvm_hdl_data_t'(effective))
            `uvm_error("DTLB0204_EVA",ident)
        if(fullva_output(group,port_id) && read_hdl({rsp,"_bits_fullva"},64)!==uvm_hdl_data_t'(effective))
            `uvm_error("DTLB0204_FULLVA",ident)
        if(fullva_output(group,port_id) && fault!=0 && read_hdl({rsp,"_bits_gpaddr_0"},64)!==uvm_hdl_data_t'(effective))
            `uvm_error("DTLB0204_GPA",ident)
        if(read_hdl($sformatf("%s.io_ptw_req_%0d_valid",unit_path,port_id))===uvm_hdl_data_t'(1))ptw_observed++;
        checks++;
        `uvm_info("DTLB0204_SAMPLE",$sformatf("%s n=%0d stage=%0d mode=%0d translate=%0d eva=%0h fault=%0h miss=%0h",ident,checks,stage,mode,translated,effective,actual_excp,miss),UVM_LOW)
        @(negedge clock_vif.clk);drive({unit_path,".reset"},1);cycles(2);
    endtask

    task run_matrix();
        `include "dtlb_0201_0204_scenarios.svh"
    endtask
    virtual task body();
        int expected_count;
        require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))
            `uvm_fatal("DTLB0204_CLOCK","missing real issue clock")
        if(seq_csr_common::get_l2tlb_seq_en() || memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)
            `uvm_fatal("DTLB0204_OWNER","local module fixture cannot share active L2TLB owner")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        `include "dtlb_0201_0204_input_defaults.svh"
        for(int p=0;p<8;p++)drive($sformatf("%s.inner_dtlbRepeater.io_tlb_req_%0d_valid",DUT,p),0);
        run_matrix();
        case(scenario())
            "host":expected_count=45;
            "guest":expected_count=62;
            "address_ld":expected_count=455;
            "address_st":expected_count=138;
            "pmm":expected_count=585;
            default:expected_count=0;
        endcase
        if(checks!=expected_count || checks==0 || canceled_windows!=checks)
            `uvm_error("DTLB0204_INCOMPLETE","scenario matrix did not complete exact request count")
        cleanup();
        `uvm_info("DTLB0204_COMPLETE",$sformatf("scenario=%s requests=%0d windows=%0d observed_ptw=%0d returned_tokens=%0d",scenario(),checks,canceled_windows,ptw_observed,returned_tokens),UVM_LOW)
    endtask
endclass
`define DTLB0204_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_dtlb_0201_0204_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`");super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB0204_CLASS(memblock_dtlb_02_host_vseq,"host")
`DTLB0204_CLASS(memblock_dtlb_02_guest_vseq,"guest")
`DTLB0204_CLASS(memblock_dtlb_02_address_ld_vseq,"address_ld")
`DTLB0204_CLASS(memblock_dtlb_02_address_st_vseq,"address_st")
`DTLB0204_CLASS(memblock_dtlb_02_pmm_vseq,"pmm")
`undef DTLB0204_CLASS
`endif
