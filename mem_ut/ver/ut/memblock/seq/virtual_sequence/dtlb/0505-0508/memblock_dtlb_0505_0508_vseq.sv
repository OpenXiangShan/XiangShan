`ifndef MEMBLOCK_DTLB_0505_0508_VSEQ__SV
`define MEMBLOCK_DTLB_0505_0508_VSEQ__SV
// 抽象职责：提供FilterEntry合法请求，独立检查发射、范围返回和三组路由。
// ready/response/token始终由唯一正式owner维护；本类不修改RTL或覆盖模型。
class memblock_dtlb_0505_0508_vseq extends memblock_dtlb_0501_0504_vseq;
    `uvm_object_utils(memblock_dtlb_0505_0508_vseq)
    // observer_stop仅由body收尾置位；observer_done由只读观察线程退出时置位。
    bit observer_stop,observer_done;
    // 下列计数仅表示本任务实际检查结果；每个仿真构造时清零，不作为DUT或owner状态。
    int unsigned windows_done,physical_samples,route_seen[8],mixed_gpa_events;
    int unsigned multi_seen[3],mixed_sent_seen[3],overlap_seen[3],new_return_seen[3];
    int unsigned stall_seen[3][4],stall_misaligned[3],ooo_seen,ooo_latency_seen[5];
    function new(string name="memblock_dtlb_0505_0508_vseq");
        super.new(name);observer_stop=0;observer_done=0;windows_done=0;physical_samples=0;
        mixed_gpa_events=0;ooo_seen=0;
        for(int i=0;i<8;i++)route_seen[i]=0;
        for(int i=0;i<5;i++)ooo_latency_seen[i]=0;
        for(int g=0;g<3;g++)begin
            multi_seen[g]=0;mixed_sent_seen[g]=0;overlap_seen[g]=0;new_return_seen[g]=0;stall_misaligned[g]=0;
            for(int b=0;b<4;b++)stall_seen[g][b]=0;
        end
    endfunction
    virtual function string scenario();return "route";endfunction
    function string repeater();return {DUT,".inner_dtlbRepeater"};endfunction
    function int route_bit(input int g);return g==0?0:g==1?4:6;endfunction
    function int length_class(input int n);return n==1?0:n<=4?1:n<=16?2:n<=64?3:4;endfunction
    // 抽象职责：保留真实位宽内四态值，以定宽位掩码清除DPI扩展位，避免每次读遍历1024bit。
    function uvm_hdl_data_t read_local(input string path,input int width=1);
        uvm_hdl_data_t value,mask;
        if(!uvm_hdl_read(path,value))`uvm_fatal("DTLB0508_HDL",path)
        mask=(uvm_hdl_data_t'(1)<<width)-1;
        return value&mask;
    endfunction
    function logic [15:0] physical_valid(input int g);
        logic [15:0] value;value=0;
        for(int i=0;i<slots(g);i++)value[i]=1'(read_local($sformatf("%s.v_%0d",filter_path(g),i)));
        return value;
    endfunction
    // 抽象职责：从合法S1返回payload独立计算VPN匹配，不消费覆盖模型的rmatch。
    // 本专项仅使用稳定ASID0、stage0、无NAPOT的4KiB/2MiB映射。
    function bit response_matches(input string u,input bit [37:0] vpn,input int stage);
        bit [34:0] tag;int level;bit context_ok,address_ok;
        tag=35'(read_local({u,".io_ptw_resp_bits_s1_entry_tag"},35));
        level=int'(read_local({u,".io_ptw_resp_bits_s1_entry_level"},2));
        context_ok=(stage==0 && read_local({u,".io_ptw_resp_bits_s2xlate"},2)===uvm_hdl_data_t'(0)) &&
            (read_local({u,".io_ptw_resp_bits_s1_entry_asid"},16)===read_local({u,".io_csr_satp_asid"},16) ||
             read_local({u,".io_ptw_resp_bits_s1_entry_perm_g"})===uvm_hdl_data_t'(1));
        if(level==0)address_ok=(vpn[37:3]==tag) && read_local($sformatf("%s.io_ptw_resp_bits_s1_valididx_%0d",u,vpn[2:0]))===uvm_hdl_data_t'(1);
        else if(level==1)address_ok=(vpn[37:9]==tag[34:6]);
        else begin address_ok=0;`uvm_fatal("DTLB0508_SCOPE","unexpected page level in dedicated test") end
        return context_ok && address_ok;
    endfunction
    // 抽象职责：每拍核对32个物理slot及返回路由；64个只读握手记录仅作独立检查。
    // 所有循环均为物理上界，不扫描主表；不驱动ready/response或覆盖模型。
    task observe_physical();
        bit previous_valid;logic [15:0] expect_valid[3],expect_sent[3];
        logic [2:0] expected_route,expected_gpa;
        int stalls[3];bit obs_valid[64];bit [37:0] obs_vpn[64];int obs_cycle[64];int sample_no;
        previous_valid=0;expected_route=0;expected_gpa=0;sample_no=0;
        for(int g=0;g<3;g++)stalls[g]=0;
        for(int i=0;i<64;i++)obs_valid[i]=0;
        while(!observer_stop)begin
            logic [2:0] actual_route,actual_gpa,next_route,next_gpa;bit any_flush;
            @(posedge clock_vif.clk);sample_no++;actual_route=0;actual_gpa=0;next_route=0;next_gpa=0;any_flush=0;
            for(int g=0;g<3;g++)begin
                string u;logic [15:0] v,sent,rmatch,enqmask,gpa_bits;int highest,highest_match,issueidx;
                bit issue,ready,resp,flush;u=filter_path(g);v=physical_valid(g);sent=0;rmatch=0;enqmask=0;gpa_bits=0;highest=-1;highest_match=-1;
                for(int i=0;i<slots(g);i++)begin
                    sent[i]=1'(read_local($sformatf("%s.sent_%0d",u,i)));
                    gpa_bits[i]=1'(read_local($sformatf("%s.getGpa_%0d",u,i)));
                    if(v[i]&&!sent[i])highest=i;
                    if(v[i] && response_matches(u,38'(read_local($sformatf("%s.vpn_%0d",u,i),38)),int'(read_local($sformatf("%s.s2xlate_%0d",u,i),2))))begin rmatch[i]=1;highest_match=i;end
                end
                issue=1'(read_local({u,".io_ptw_req_0_valid"}));ready=1'(read_local({u,".io_ptw_req_0_ready"}));resp=1'(read_local({u,".io_ptw_resp_valid"}));flush=1'(read_local({u,".io_flush"}));any_flush|=flush;
                issueidx=int'(read_local({u,".issueindex"},g==0?4:3));
                if(previous_valid && (v!==expect_valid[g] || sent!==expect_sent[g]))
                    `uvm_error("DTLB0508_STATE",$sformatf("g=%0d v=%h expected=%h sent=%h expected_sent=%h",g,v,expect_valid[g],sent,expect_sent[g]))
                if(issue && (highest<0 || issueidx!=highest || !v[issueidx] || sent[issueidx]))
                    `uvm_error("DTLB0508_ISSUE","issue must select highest valid unsent physical entry")
                if(resp)begin
                    if(read_local({u,".io_refill"})!==uvm_hdl_data_t'(|rmatch))`uvm_error("DTLB0508_MATCH","refill differs from independent VPN/stage/context match")
                    if(highest_match>=0 && read_local({u,".io_getGpa"})!==uvm_hdl_data_t'(gpa_bits[highest_match]))`uvm_error("DTLB0508_GPA","getGpa differs from highest matching physical slot")
                    if($countones(rmatch)>1)begin
                        multi_seen[g]++;
                        if(|(rmatch&sent) && |(rmatch&~sent))mixed_sent_seen[g]++;
                        `uvm_info("DTLB0508_MULTI",$sformatf("g=%0d match_count=%h sent=%h highest=%0d selected_gpa=%0b occupancy=%0d owner_pending=%0d",g,rmatch,sent,highest_match,gpa_bits[highest_match],$countones(v),live_owner.outstanding_count()),UVM_LOW)
                    end
                    if(issue&&ready&&rmatch[issueidx])begin overlap_seen[g]++;`uvm_info("DTLB0508_OVERLAP",$sformatf("g=%0d slot=%0d",g,issueidx),UVM_LOW) end
                end
                for(int p=0;p<filter_ports(g);p++)begin
                    bit req,enq;int idx;req=1'(read_local($sformatf("%s.io_tlb_req_%0d_valid",u,p)));enq=1'(read_local($sformatf("%s.enqvalid_%0d",u,p)));idx=int'(read_local($sformatf("%s.enqidx_%0d",u,p),g==0?4:3));
                    if(enq)enqmask[idx]=1;
                    if(resp&&req&&response_matches(u,38'(read_local($sformatf("%s.io_tlb_req_%0d_bits_vpn",u,p),38)),int'(read_local($sformatf("%s.io_tlb_req_%0d_bits_s2xlate",u,p),2))))begin
                        if(enq)`uvm_error("DTLB0508_NEW_RETURN","matching current response must suppress allocation")
                        if(rmatch==0)begin new_return_seen[g]++;`uvm_info("DTLB0508_NEW_RETURN",$sformatf("g=%0d p=%0d enq=0 refill=0",g,p),UVM_LOW) end
                    end
                end
                expect_valid[g]=flush?0:(v|enqmask)&~(resp?rmatch:16'b0);
                expect_sent[g]=(sent&~enqmask)|(issue&&ready?16'(1<<issueidx):16'b0);
                if(issue&&!ready)stalls[g]++;
                else begin
                    if(issue&&ready&&stalls[g]>0)begin
                        int b;uvm_hdl_data_t recorded;b=stalls[g]==1?0:stalls[g]<=4?1:stalls[g]<=16?2:3;stall_seen[g][b]++;
                        recorded=read_local($sformatf("%s.u_dtlb_%s_filter_fcov.completed_stall_length",repeater(),g==0?"ld":g==1?"st":"pf"),32);
                        if(recorded!=uvm_hdl_data_t'(stalls[g]))stall_misaligned[g]++;
                        `uvm_info("DTLB0508_STALL",$sformatf("g=%0d real_length=%0d model_sample=%0d issue_slot=%0d",g,stalls[g],recorded,issueidx),UVM_LOW)
                    end
                    stalls[g]=0;
                end
                next_route[g]=resp&&|rmatch;next_gpa[g]=next_route[g]&&gpa_bits[highest_match];
                actual_route[g]=1'(read_local({unit_name(g),".io_ptw_resp_valid"}));
                actual_gpa[g]=1'(read_local({unit_name(g),".io_ptw_resp_bits_getGpa"}));
                if(read_local($sformatf("%s.io_tlb_resp_bits_vector_%0d",repeater(),route_bit(g)))!==uvm_hdl_data_t'(next_route[g]))`uvm_error("DTLB0508_ROUTE","repeater route bit differs from actual matched group")
            end
            if(previous_valid && (actual_route!==expected_route || (actual_gpa&expected_route)!==(expected_gpa&expected_route)))
                `uvm_error("DTLB0508_DELAY",$sformatf("route=%b expected=%b gpa=%b expected_gpa=%b",actual_route,expected_route,actual_gpa,expected_gpa))
            if(|next_route)begin
                route_seen[next_route]++;
                if($countones(next_route)>=2 && |(next_gpa&next_route) && (next_gpa&next_route)!=next_route)mixed_gpa_events++;
                `uvm_info("DTLB0508_ROUTE",$sformatf("mask=%b gpa=%b next_dtlb_check=1",next_route,next_gpa),UVM_LOW)
            end
            expected_route=next_route;expected_gpa=next_gpa;
            // 模块外返回仅观察，不创建可驱动的第二owner；不同sector窗口可唯一关联。
            if(any_flush)begin for(int i=0;i<64;i++)obs_valid[i]=0;end
            else begin
                if(read_local({repeater(),".io_ptw_resp_valid"})===uvm_hdl_data_t'(1))begin
                    bit [34:0] tag;int selected,match_count;bit younger;tag=35'(read_local({repeater(),".io_ptw_resp_bits_s1_entry_tag"},35));selected=-1;match_count=0;younger=0;
                    for(int i=0;i<64;i++)if(obs_valid[i]&&obs_vpn[i][37:3]==tag)begin match_count++;if(selected<0||obs_cycle[i]<obs_cycle[selected])selected=i;end
                    if(selected>=0)begin
                        int latency;latency=sample_no-obs_cycle[selected];
                        for(int i=0;i<64;i++)if(obs_valid[i]&&obs_cycle[i]<obs_cycle[selected])younger=1;
                        if(younger&&match_count==1)begin ooo_seen++;ooo_latency_seen[length_class(latency)]++;`uvm_info("DTLB0508_OOO",$sformatf("vpn=%h latency=%0d older_pending=1",obs_vpn[selected],latency),UVM_LOW) end
                        obs_valid[selected]=0;
                    end
                end
                if(read_local({repeater(),".io_ptw_req_0_valid"})===uvm_hdl_data_t'(1)&&read_local({repeater(),".io_ptw_req_0_ready"})===uvm_hdl_data_t'(1))begin
                    int free_idx;free_idx=-1;for(int i=0;i<64;i++)if(!obs_valid[i]&&free_idx<0)free_idx=i;
                    if(free_idx<0)`uvm_fatal("DTLB0508_OBS_FULL","bounded read-only observation window full")
                    obs_valid[free_idx]=1;obs_vpn[free_idx]=38'(read_local({repeater(),".io_ptw_req_0_bits_vpn"},38));obs_cycle[free_idx]=sample_no;
                end
            end
            previous_valid=1;physical_samples++;
        end
        observer_done=1;
    endtask
    // 抽象职责：等待真实链路排空，诊断上界只用于报错。
    task drain_window();
        bit empty;empty=0;
        for(int t=0;t<2400;t++)begin
            @(negedge clock_vif.clk);#1ps;
            if(live_owner.outstanding_count()==0 && physical_valid(0)==0 && physical_valid(1)==0 && physical_valid(2)==0)begin empty=1;break;end
        end
        if(!empty)`uvm_fatal("DTLB0508_DRAIN","actual owner/filter did not drain")
        cycles(6);
    endtask
    task pulse_group(input int g,input int p,input bit [37:0] vpn,input bit gpa=0);
        set_filter_request(g,p,vpn,0,gpa);@(posedge clock_vif.clk);@(negedge clock_vif.clk);clear_filter_requests(g);
    endtask
    task wait_accept(input longint unsigned target);
        for(int t=0;t<100 && live_owner.accepted_count<target;t++)begin @(negedge clock_vif.clk);#1ps;end
        if(live_owner.accepted_count<target)`uvm_fatal("DTLB0508_ACCEPT","real owner acceptance not observed")
    endtask
    // 抽象职责：覆盖全部路由mask及组独立getGpa，随后在空组验证当前返回抑制新请求。
    task route_matrix();
        for(int mask=1;mask<8;mask++)begin
            bit [37:0] vpn;longint unsigned a,c;vpn=38'h20000+38'(mask)*512;filter_reset();a=live_owner.accepted_count;c=live_owner.completed_count;
            for(int g=0;g<3;g++)if(mask&(1<<g))set_filter_request(g,0,vpn,0,g==2);
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);for(int g=0;g<3;g++)clear_filter_requests(g);
            wait_accept(a+$countones(3'(mask)));drain_window();
            if(live_owner.completed_count-c!=$countones(3'(mask)) || route_seen[mask]==0)`uvm_error("DTLB0508_ROUTE_COUNT","each accepted cross-group token must complete independently")
            windows_done++;
        end
        for(int g=0;g<3;g++)begin
            bit found;bit [37:0] vpn;longint unsigned a;int source;source=(g+1)%3;vpn=38'h30000+38'(g)*512;filter_reset();a=live_owner.accepted_count;pulse_group(source,0,vpn);wait_accept(a+1);found=0;
            for(int t=0;t<1200;t++)begin
                #1ps;if(read_local({filter_path(g),".io_ptw_resp_valid"})===uvm_hdl_data_t'(1))begin found=1;break;end
                @(negedge clock_vif.clk);
            end
            if(!found)`uvm_fatal("DTLB0508_NEW_WINDOW","no legitimate accepted response window")
            pulse_group(g,0,vpn);drain_window();if(new_return_seen[g]==0)`uvm_error("DTLB0508_NEW_WINDOW","empty-group current-return request window missing")
            windows_done++;
        end
        if(mixed_gpa_events<3)`uvm_error("DTLB0508_GPA_COUNT","missing cross-group mixed getGpa windows")
    endtask
    // 抽象职责：合法2MiB返回覆盖同组不同VPN，验证sent/unsent共同释放与最高匹配项。
    task multi_matrix();
        for(int g=0;g<3;g++)begin
            bit [37:0] vpn;longint unsigned a;vpn=38'h40000+38'(g)*1024;filter_reset();a=live_owner.accepted_count;
            pulse_group(g,0,vpn,0);wait_accept(a+1);pulse_group(g,1,vpn+8,1);drain_window();
            if(multi_seen[g]==0 || mixed_sent_seen[g]==0)`uvm_error("DTLB0508_MULTI_COUNT","real response must release sent and unsent slots together")
            windows_done++;
        end
    endtask
    // 抽象职责：以正式owner容量形成真实背压，逐拍检查长度；不操纵ready或模型历史。
    task stall_matrix();
        for(int g=0;g<3;g++)for(int variant=0;variant<8;variant++)begin
            int gap;bit [37:0] vpn;longint unsigned a;
            case(variant)0:gap=0;1:gap=40;2:gap=48;3:gap=56;4:gap=58;5:gap=60;6:gap=61;default:gap=62;endcase
            vpn=38'h50000+38'(g)*4096+38'(variant)*32;filter_reset();a=live_owner.accepted_count;
            pulse_group(g,0,vpn);wait_accept(a+1);if(gap>0)cycles(gap);pulse_group(g,1,vpn+8);drain_window();windows_done++;
        end
        for(int g=0;g<3;g++)if(stall_seen[g][3]==0)`uvm_error("DTLB0508_STALL_COUNT","long real backpressure missing")
    endtask
    // 抽象职责：不同sector并发请求配合既有加权延迟，检查真实返回乱序和延迟范围。
    task reorder_matrix();
        for(int window=0;window<12;window++)begin
            longint unsigned a,c;filter_reset();a=live_owner.accepted_count;c=live_owner.completed_count;
            for(int n=0;n<4;n++)begin
                for(int g=0;g<3;g++)for(int p=0;p<filter_ports(g);p++)set_filter_request(g,p,38'h80000+38'(window)*4096+38'(g)*512+38'(p)*64+38'(n)*8);
                @(posedge clock_vif.clk);@(negedge clock_vif.clk);for(int g=0;g<3;g++)clear_filter_requests(g);cycles(1);
            end
            drain_window();
            if(live_owner.accepted_count-a!=32 || live_owner.completed_count-c!=32)`uvm_error("DTLB0508_REORDER_COUNT","all 32 distinct requests must fire and complete")
            windows_done++;
        end
        if(ooo_seen==0)`uvm_error("DTLB0508_REORDER_COUNT","no independently observed out-of-order response")
    endtask
    virtual task body();
        int expected;require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))`uvm_fatal("DTLB0508_CLOCK","missing clock")
        if(!seq_csr_common::get_l2tlb_seq_en()||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)`uvm_fatal("DTLB0508_OWNER","explicit unique responder topology required")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        `include "dtlb_0501_0504_live_defaults.svh"
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_satp_mode",9);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_vsatp_mode",9);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_hgatp_mode",9);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_satp_asid",0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_vsatp_asid",0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_hgatp_vmid",0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_priv_dmode",1);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_priv_imode",1);cycles(12);initialize_lookup_ports();
        live_owner=memblock_l2tlb_base_sequence::type_id::create("dtlb0508_formal_owner");
        live_adapter=dispatch_monitor_event_adapter::type_id::create("dtlb0508_fence_adapter");live_owner_done=0;adapter_stop=0;
        fork
            begin live_owner.start(p_sequencer.L2tlb_sqr);live_owner_done=1;end
            begin while(!adapter_stop)begin @(negedge clock_vif.clk);live_adapter.service_l2tlb_sfence_events();void'(memblock_sync_pkg::grant_l2tlb_final_release());end end
        join_none
        cycles(12);
        for(int p=0;p<8;p++)drive($sformatf("%s.io_tlb_req_%0d_valid",repeater(),p),0);
        for(int g=0;g<3;g++)begin string u;u=filter_path(g);if(g==0)begin `include "dtlb_0501_0504_load_live.svh" end else begin `include "dtlb_0501_0504_small_live.svh" end end
        filter_reset();fork observe_physical();join_none
        case(scenario())
            "route":begin route_matrix();expected=10;end
            "multi":begin multi_matrix();expected=3;end
            "stall":begin stall_matrix();expected=24;end
            "reorder":begin reorder_matrix();expected=12;end
            default:`uvm_fatal("DTLB0508_CASE",scenario())
        endcase
        drain_window();filter_reset();
        begin common_data_transaction data;data=common_data_transaction::get();data.request_global_stop_if_done();end
        for(int n=0;n<100&&!live_owner_done;n++)@(negedge clock_vif.clk);
        if(!live_owner_done||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)`uvm_fatal("DTLB0508_OWNER","formal owner did not naturally release")
        if(live_owner.accepted_count!=live_owner.completed_count+live_owner.flush_canceled_count+live_owner.reset_canceled_count)`uvm_error("DTLB0508_ACCOUNT","accepted/completed/canceled accounting mismatch")
        observer_stop=1;wait(observer_done);adapter_stop=1;wait fork;
        for(int g=0;g<3;g++)`uvm_info("DTLB0508_GROUP",$sformatf("g=%0d multi=%0d mixed_sent=%0d overlap=%0d new_return=%0d stalls=%0d/%0d/%0d/%0d stale_stall=%0d",g,multi_seen[g],mixed_sent_seen[g],overlap_seen[g],new_return_seen[g],stall_seen[g][0],stall_seen[g][1],stall_seen[g][2],stall_seen[g][3],stall_misaligned[g]),UVM_LOW)
        `uvm_info("DTLB0508_OWNER_FINAL",$sformatf("accepted=%0d completed=%0d flush_canceled=%0d reset_canceled=%0d",live_owner.accepted_count,live_owner.completed_count,live_owner.flush_canceled_count,live_owner.reset_canceled_count),UVM_LOW)
        if(windows_done!=expected||initialization_checks!=8)`uvm_error("DTLB0508_INCOMPLETE","window/init count mismatch")
        cleanup();
        `uvm_info("DTLB0508_COMPLETE",$sformatf("scenario=%s windows=%0d initialization=%0d physical_samples=%0d mixed_gpa=%0d ooo=%0d latency_bins=%0d/%0d/%0d/%0d/%0d",scenario(),windows_done,initialization_checks,physical_samples,mixed_gpa_events,ooo_seen,ooo_latency_seen[0],ooo_latency_seen[1],ooo_latency_seen[2],ooo_latency_seen[3],ooo_latency_seen[4]),UVM_LOW)
    endtask
endclass
`define DTLB0508_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_dtlb_0505_0508_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`");super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB0508_CLASS(memblock_dtlb_0508_route_vseq,"route")
`DTLB0508_CLASS(memblock_dtlb_0506_multi_vseq,"multi")
`DTLB0508_CLASS(memblock_dtlb_0505_stall_vseq,"stall")
`DTLB0508_CLASS(memblock_dtlb_0508_reorder_vseq,"reorder")
`undef DTLB0508_CLASS
`endif
