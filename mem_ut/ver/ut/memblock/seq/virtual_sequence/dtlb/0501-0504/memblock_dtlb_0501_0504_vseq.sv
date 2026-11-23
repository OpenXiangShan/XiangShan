`ifndef MEMBLOCK_DTLB_0501_0504_VSEQ__SV
`define MEMBLOCK_DTLB_0501_0504_VSEQ__SV
// 抽象职责：FilterEntry声明接口的真实分配/合并/发送返回，以及DTLB本地Miss窗口。
// FilterEntry专项复用正式唯一L2TLB owner；本地窗口单独保持filter reset，检查器不屏蔽。
class memblock_dtlb_0501_0504_vseq extends memblock_dtlb_0401_0402_vseq;
    `uvm_object_utils(memblock_dtlb_0501_0504_vseq)
    // 当前场景检查数；FilterEntry真实token由正式owner持有，局部DTLB返回另独立记账。
    int unsigned samples_new,group_windows,filter_returns_observed;
    memblock_l2tlb_base_sequence live_owner;
    dispatch_monitor_event_adapter live_adapter;
    bit live_owner_done,adapter_stop;
    // 第二个真实本地Miss凭据：仅在连续两拍返回专项持有，先消费A再转入B。
    bit extra_token_valid;bit [37:0] extra_token_vpn;int extra_token_group;
    bit [15:0] allocated_seen[3];
    bit [1:0] old_sent_seen[3];
    function new(string name="memblock_dtlb_0501_0504_vseq");
        super.new(name);samples_new=0;group_windows=0;filter_returns_observed=0;
        for(int g=0;g<3;g++)begin allocated_seen[g]=0;old_sent_seen[g]=0;end
    endfunction
    virtual function string scenario();return "allocation";endfunction
    function string filter_path(input int g);
        case(g)
            0:return {DUT,".inner_dtlbRepeater.load_filter_load_entry"};
            1:return {DUT,".inner_dtlbRepeater.store_filter_store_entry"};
            2:return {DUT,".inner_dtlbRepeater.prefetch_filter_prefetch_entry"};
        endcase
        return "";
    endfunction
    function int filter_ports(input int g);return g==0?4:2;endfunction
    function int slots(input int g);return g==0?16:8;endfunction
    function logic [15:0] valid_slots(input int g);
        logic [15:0] value;value=0;
        for(int i=0;i<slots(g);i++)value[i]=1'(read_hdl($sformatf("%s.v_%0d",filter_path(g),i)));
        return value;
    endfunction
    task clear_filter_requests(input int g);
        for(int p=0;p<filter_ports(g);p++)drive($sformatf("%s.io_tlb_req_%0d_valid",filter_path(g),p),0);
    endtask
    // 抽象职责：从真实顶层fence接口发一拍SFENCE；正式owner与adapter各按C4合同处理。
    // 每个窗口仅一条fence，adapter每采样处理有界FIFO，不扫描主表。
    task filter_reset();
        string q;q="top_tb.u_memblock__fence_agent_if.io_ooo_to_mem_sfence";
        for(int g=0;g<3;g++)clear_filter_requests(g);
        drive({q,"_bits_rs1"},1);drive({q,"_bits_rs2"},1);drive({q,"_bits_addr"},0);
        drive({q,"_bits_id"},0);drive({q,"_bits_hv"},0);drive({q,"_bits_hg"},0);drive({q,"_bits_flushPipe"},0);
        drive({q,"_valid"},1);@(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({q,"_valid"},0);cycles(12);
        for(int g=0;g<3;g++)if(valid_slots(g)!=0)`uvm_fatal("DTLB0504_FENCE","real SFENCE did not flush all filters")
    endtask
    task set_filter_request(input int g,input int p,input bit [37:0] vpn,input int stage=0,input bit gpa=0);
        string q;q=$sformatf("%s.io_tlb_req_%0d",filter_path(g),p);
        drive({q,"_bits_vpn"},vpn);drive({q,"_bits_s2xlate"},stage);drive({q,"_bits_getGpa"},gpa);drive({q,"_valid"},1);
    endtask
    // 抽象职责：采样当前真实enq和index，下一拍核对slot实际有效与内容；不读取模型聚合期望。
    task filter_tick(input string ident,input int g,input bit [3:0] expected_enq,input bit same_idx=0);
        string u;logic [3:0] actual;int indices[4];bit [37:0] vpns[4];bit [1:0] stages[4];
        u=filter_path(g);actual=0;
        // 输入改变后让组合逻辑稳定；在下一个真实采样边沿前读取。
        #1ps;
        for(int p=0;p<filter_ports(g);p++)begin
            actual[p]=1'(read_hdl($sformatf("%s.enqvalid_%0d",u,p)));
            indices[p]=int'(read_hdl($sformatf("%s.enqidx_%0d",u,p),g==0?4:3));
            vpns[p]=38'(read_hdl($sformatf("%s.io_tlb_req_%0d_bits_vpn",u,p),38));
            stages[p]=2'(read_hdl($sformatf("%s.io_tlb_req_%0d_bits_s2xlate",u,p),2));
            if(actual[p])allocated_seen[g][indices[p]]=1;
            // 独立从真实物理表项查旧match，不使用bind倒序位图作为期望。
            if(read_hdl($sformatf("%s.io_tlb_req_%0d_valid",u,p))===uvm_hdl_data_t'(1))begin
                int physical_idx,model_idx;uvm_hdl_data_t raw_match,sent;
                physical_idx=-1;model_idx=-1;
                for(int i=0;i<slots(g);i++)if(read_hdl($sformatf("%s.v_%0d",u,i))===uvm_hdl_data_t'(1) && read_hdl($sformatf("%s.vpn_%0d",u,i),38)===uvm_hdl_data_t'(vpns[p]) && read_hdl($sformatf("%s.s2xlate_%0d",u,i),2)===uvm_hdl_data_t'(stages[p]))physical_idx=i;
                if(physical_idx>=0)begin
                    raw_match=read_hdl($sformatf("%s._entryIsMatchVec_%0d_T",u,p),slots(g));sent=read_hdl($sformatf("%s.sent_%0d",u,physical_idx));
                    for(int i=0;i<slots(g);i++)if(raw_match[i])model_idx=i;
                    if(actual[p] || indices[p]!=physical_idx)`uvm_error("DTLB0504_OLDIDX","real old entry must be reused without allocation")
                    old_sent_seen[g][int'(sent)]=1;
                    `uvm_info("DTLB0504_OLDMAP",$sformatf("%s g=%0d p=%0d actual_idx=%0d physical_idx=%0d sent=%0h raw_mask=%0h model_decoded_idx=%0d",ident,g,p,indices[p],physical_idx,sent,raw_match,model_idx),UVM_LOW)
                end
            end
        end
        if(actual!==expected_enq)`uvm_error("DTLB0504_ENQ",$sformatf("%s g=%0d expected=%0h actual=%0h",ident,g,expected_enq,actual))
        if(same_idx && indices[0]!=indices[1])`uvm_error("DTLB0504_MERGE","same-key ports did not reuse index")
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);
        for(int p=0;p<filter_ports(g);p++)if(actual[p])begin
            if(read_hdl($sformatf("%s.v_%0d",u,indices[p]))!==uvm_hdl_data_t'(1)||
               read_hdl($sformatf("%s.vpn_%0d",u,indices[p]),38)!==uvm_hdl_data_t'(vpns[p])||
               read_hdl($sformatf("%s.s2xlate_%0d",u,indices[p]),2)!==uvm_hdl_data_t'(stages[p]))
                `uvm_error("DTLB0504_SLOT","actual allocated slot did not retain VPN/stage")
        end
        clear_filter_requests(g);samples_new++;
        `uvm_info("DTLB0504_SAMPLE",$sformatf("%s g=%0d enq=%0h idx0=%0d idx1=%0d occupancy=%0d n=%0d",ident,g,actual,indices[0],indices[1],$countones(valid_slots(g)),samples_new),UVM_LOW)
        cycles(1);
    endtask
    // 抽象职责：等待正式owner产生真实返回，按返回tag定位当前有效slot；不制造返回或ready。
    task await_filter_return(input int g,output int slot);
        string u;bit found;bit [37:0] returned_vpn;int st;
        u=filter_path(g);found=0;slot=-1;
        for(int t=0;t<1200;t++)begin
            if(read_hdl({u,".io_ptw_resp_valid"})===uvm_hdl_data_t'(1))begin
                returned_vpn={35'(read_hdl({u,".io_ptw_resp_bits_s1_entry_tag"},35)),3'(read_hdl({u,".io_ptw_resp_bits_s1_addr_low"},3))};
                st=int'(read_hdl({u,".io_ptw_resp_bits_s2xlate"},2));
                for(int i=0;i<slots(g);i++)if((read_hdl($sformatf("%s.v_%0d",u,i))===uvm_hdl_data_t'(1)) && read_hdl($sformatf("%s.vpn_%0d",u,i),38)===uvm_hdl_data_t'(returned_vpn) && read_hdl($sformatf("%s.s2xlate_%0d",u,i),2)===uvm_hdl_data_t'(st))begin slot=i;found=1;end
                if(found)break;
            end
            @(negedge clock_vif.clk);
        end
        if(!found)`uvm_fatal("DTLB0504_RETURN","no real owner return matched a valid slot within diagnostic bound")
        filter_returns_observed++;
        `uvm_info("DTLB0504_REAL_RETURN",$sformatf("g=%0d slot=%0d vpn=%0h owner_accepted=%0d owner_completed=%0d",g,slot,returned_vpn,live_owner.accepted_count,live_owner.completed_count),UVM_LOW)
    endtask
    task allocation_matrix();
        string u;int part,idx;bit [15:0] before_valid;
        for(int g=0;g<3;g++)begin
            u=filter_path(g);filter_reset();
            set_filter_request(g,0,38'h10000);set_filter_request(g,1,38'h20000);filter_tick("parallel_distinct",g,3);
            filter_reset();
            for(int p=0;p<filter_ports(g);p++)for(int n=0;n<4;n++)begin
                set_filter_request(g,p,38'h30000+38'(p)*256+38'(n)*8);filter_tick("fill_partition_slot",g,4'(1<<p));
            end
            if(allocated_seen[g]!==16'((32'h1<<slots(g))-1))`uvm_error("DTLB0504_ALLSLOTS","did not really allocate every physical slot")
            set_filter_request(g,0,38'h50000);filter_tick("full_group_reject",g,0);
            before_valid=valid_slots(g);await_filter_return(g,idx);part=idx/4;
            set_filter_request(g,part,38'h60000);filter_tick("full_release_same_cycle",g,0);
            if($countones(valid_slots(g))!=$countones(before_valid)-1)`uvm_error("DTLB0504_RELEASE","matched accepted response did not clear exactly one slot")
            set_filter_request(g,part,38'h60000);filter_tick("next_cycle_allocate",g,4'(1<<part));group_windows++;
        end
    endtask
    task merge_matrix();
        for(int g=0;g<3;g++)begin
            filter_reset();set_filter_request(g,0,38'h70000);filter_tick("first_entry",g,1);
            set_filter_request(g,0,38'h70000);filter_tick("old_entry_merge",g,0);
            for(int n=1;n<4;n++)begin set_filter_request(g,0,38'h70000+38'(n)*8);filter_tick("fill_low_partition",g,1);end
            set_filter_request(g,0,38'h70000);filter_tick("full_partition_old_merge",g,0);
            set_filter_request(g,0,38'h70008);filter_tick("unsent_old_entry_merge",g,0);
            set_filter_request(g,1,38'h70000,1);filter_tick("same_vpn_other_stage",g,2);
            for(int n=0;n<3;n++)begin set_filter_request(g,1,38'h76000+38'(n)*8);filter_tick("fill_other_partition",g,2);end
            set_filter_request(g,0,38'h70000);set_filter_request(g,1,38'h77000);filter_tick("full_old_merge_and_other_full_reject",g,0);
            filter_reset();set_filter_request(g,0,38'h71000);set_filter_request(g,1,38'h71000);filter_tick("same_cycle_new_merge",g,1,1);
            filter_reset();for(int n=0;n<4;n++)begin set_filter_request(g,0,38'h72000+38'(n)*8);filter_tick("fill_low_full",g,1);end
            set_filter_request(g,0,38'h73000);set_filter_request(g,1,38'h73000);filter_tick("low_full_high_free",g,0,1);
            filter_reset();for(int n=0;n<4;n++)begin set_filter_request(g,1,38'h74000+38'(n)*8);filter_tick("fill_high_full",g,2);end
            set_filter_request(g,0,38'h75000);set_filter_request(g,1,38'h75000);filter_tick("low_free_high_full",g,1,1);
            if(old_sent_seen[g]!=2'b11)`uvm_error("DTLB0504_SENT","old merge must observe both unsent and sent physical entries")
            group_windows++;
        end
    endtask
    // 抽象职责：本地一拍请求/响应配对，真实记录PTW/replay；返回拍数按请求身份保持。
    task local_issue(input int g,input int p,input bit [49:0] va,input string ident,
                     input bit with_current_return=0,input int expected_q=-1,input int capture_token=0);
        string u;uvm_hdl_data_t qvalid,replay,miss;
        u=unit_name(g);target_request(g,p,va,g==1?1:0,0,0);@(posedge clock_vif.clk);
        @(negedge clock_vif.clk);drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
        if(with_current_return)drive({u,".io_ptw_resp_valid"},1);
        @(posedge clock_vif.clk);qvalid=read_hdl($sformatf("%s.io_ptw_req_%0d_valid",u,p));
        replay=(g==0&&p<3)?read_hdl($sformatf("%s.io_tlbreplay_%0d",u,p)):0;miss=read_hdl($sformatf("%s.io_requestor_%0d_resp_bits_miss",u,p));
        if(read_hdl($sformatf("%s.io_requestor_%0d_resp_valid",u,p))!==uvm_hdl_data_t'(1)||
           read_hdl({indexed(u,"req_out",p),"_vaddr"},50)!==uvm_hdl_data_t'(va))`uvm_error("DTLB0504_LOCAL","real request association lost")
        if(expected_q>=0 && qvalid!==uvm_hdl_data_t'(expected_q))`uvm_error("DTLB0504_Q",$sformatf("%s g=%0d p=%0d expected=%0d got=%0h",ident,g,p,expected_q,qvalid))
        if(capture_token==1)begin
            if(token_valid||qvalid!==uvm_hdl_data_t'(1))`uvm_fatal("DTLB0504_TOKEN","miss token not observable")
            if(read_hdl($sformatf("%s.io_ptw_req_%0d_bits_vpn",u,p),38)!==uvm_hdl_data_t'(va[49:12]))`uvm_fatal("DTLB0504_TOKEN",$sformatf("actual VPN mismatch expected=%0h actual=%0h req_out=%0h",va[49:12],read_hdl($sformatf("%s.io_ptw_req_%0d_bits_vpn",u,p),38),read_hdl({indexed(u,"req_out",p),"_vaddr"},50)))
            token_valid=1;token_vpn=va[49:12];token_stage=0;token_group=g;
        end
        if(capture_token==2)begin
            if(extra_token_valid || qvalid!==uvm_hdl_data_t'(1) || read_hdl($sformatf("%s.io_ptw_req_%0d_bits_vpn",u,p),38)!==uvm_hdl_data_t'(va[49:12]))`uvm_fatal("DTLB0504_TOKEN","second local Miss credential invalid")
            extra_token_valid=1;extra_token_vpn=va[49:12];extra_token_group=g;
        end
        samples_new++;
        `uvm_info("DTLB0504_SAMPLE",$sformatf("%s g=%0d p=%0d va=%0h q=%0h replay=%0h miss=%0h n=%0d",ident,g,p,va,qvalid,replay,miss,samples_new),UVM_LOW)
        @(negedge clock_vif.clk);
    endtask
    task window_matrix();
        string u;bit [49:0] va,other;va=50'h80000000;other=50'h90000000;
        for(int g=0;g<3;g++)for(int p=0;p<ports(g);p++)begin
            configure_context(g);u=unit_name(g);
            local_issue(g,p,va,"miss_initial",0,1,1);
            return_fields(g,va,48'h40000000,6'h3f,0,5'h1f,0);
            local_issue(g,p,va,"current_matching_return",1,0);token_valid=0;refills++;
            drive({u,".io_ptw_resp_valid"},0);
            local_issue(g,p,other,"previous_return_other_vpn",0,1);
            local_issue(g,p,other+4096,"older_return_other_vpn",0,1);
            configure_context(g);local_issue(g,p,va,"retry_owner_miss",0,1,1);
            return_fields(g,va,48'h40000000,6'h3f,0,5'h1f,0);
            local_issue(g,p,va,"retry_owner_matching_return",1,0);token_valid=0;refills++;
            drive({u,".io_ptw_resp_valid"},0);cycles(8);
            local_issue(g,p,va,"same_transaction_retry_hit",0,0);
            configure_context(g);local_issue(g,p,va,"context_A_miss",0,1,1);
            return_fields(g,va,48'h40000000,6'h3f,0,5'h1f,0);
            local_issue(g,p,other,"return_different_vpn",1,1);token_valid=0;refills++;
            drive({u,".io_ptw_resp_valid"},0);
            configure_context(g);local_issue(g,p,other,"multi_reason_A_miss",0,1,1);
            local_issue(g,p,va,"multi_reason_B_miss",0,1,2);
            return_fields(g,other,48'h50000000,6'h3f,0,5'h1f,0);
            // B查找边沿只返回不同VPN的A，下一响应边沿返回B；两笔都来自真实Miss通知。
            target_request(g,p,va,g==1?1:0,0,0);drive({u,".io_ptw_resp_valid"},1);
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);refills++;token_valid=0;
            if(!extra_token_valid || extra_token_group!=g)`uvm_fatal("DTLB0504_TOKEN","missing actual second credential")
            token_valid=1;token_vpn=extra_token_vpn;token_group=g;token_stage=0;extra_token_valid=0;
            return_fields(g,va,48'h40000000,6'h3f,0,5'h1f,0);drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
            @(posedge clock_vif.clk);
            if(read_hdl($sformatf("%s.io_requestor_%0d_resp_valid",u,p))!==uvm_hdl_data_t'(1) || read_hdl($sformatf("%s.io_ptw_req_%0d_valid",u,p))!==uvm_hdl_data_t'(0) || read_hdl($sformatf("%s.io_requestor_%0d_resp_bits_miss",u,p))!==uvm_hdl_data_t'(1))`uvm_error("DTLB0504_MULTI","consecutive real returns must suppress cold B Miss")
            samples_new++;`uvm_info("DTLB0504_SAMPLE",$sformatf("two_accepted_returns_current_B_previous_A g=%0d p=%0d q=0 miss=1 n=%0d",g,p,samples_new),UVM_LOW)
            @(negedge clock_vif.clk);token_valid=0;refills++;drive({u,".io_ptw_resp_valid"},0);
            drive({u,".reset"},1);cycles(2);group_windows++;
        end
    endtask
    // 抽象职责：对相邻响应分别验证A->B和A->A；记录合法返回/旁路与模型历史资格的差异。
    task history_matrix();
        string u;bit [49:0] va,nextva;uvm_hdl_data_t q,miss;
        va=50'h80000000;
        for(int g=0;g<3;g++)for(int p=0;p<ports(g);p++)for(int same=0;same<2;same++)begin
            configure_context(g);u=unit_name(g);nextva=same?va:50'h90000000;
            local_issue(g,p,va,"history_initial_A_miss",0,1,1);
            return_fields(g,va,48'h40000000,6'h3f,0,5'h1f,0);
            target_request(g,p,va,g==1?1:0,0,0);@(posedge clock_vif.clk);@(negedge clock_vif.clk);
            drive({u,".io_ptw_resp_valid"},1);target_request(g,p,nextva,g==1?1:0,0,0);
            @(posedge clock_vif.clk);
            if(read_hdl($sformatf("%s.io_ptw_req_%0d_valid",u,p))!==uvm_hdl_data_t'(0))`uvm_error("DTLB0504_HISTORY","current matching A return must suppress A Miss")
            samples_new++;`uvm_info("DTLB0504_SAMPLE",$sformatf("history_current_A g=%0d p=%0d same=%0d n=%0d",g,p,same,samples_new),UVM_LOW)
            @(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);token_valid=0;refills++;
            @(posedge clock_vif.clk);q=read_hdl($sformatf("%s.io_ptw_req_%0d_valid",u,p));miss=read_hdl($sformatf("%s.io_requestor_%0d_resp_bits_miss",u,p));
            if(q!==uvm_hdl_data_t'(!same) || miss!==uvm_hdl_data_t'(!same) || read_hdl({indexed(u,"req_out",p),"_vaddr"},50)!==uvm_hdl_data_t'(nextva))`uvm_error("DTLB0504_HISTORY","older A must allow B; same A must use actual bypass/storage")
            samples_new++;`uvm_info("DTLB0504_SAMPLE",$sformatf("history_previous_A_current_%s g=%0d p=%0d q=%0h miss=%0h n=%0d",same?"A":"B",g,p,q,miss,samples_new),UVM_LOW)
            @(negedge clock_vif.clk);drive({u,".reset"},1);cycles(2);group_windows++;
        end
    endtask
    // 抽象职责：实际模块组合形成同VPN、不同ASID返回过滤；不驱动filter valid或任何返回。
    task integration_matrix();
        string u;bit observed;bit [49:0] va;int quiet;
        va=50'h80000000;u=unit_name(0);observed=0;
        configure_context(0,0,0,0);target_request(0,0,va,0,0,0);
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_requestor_0_req_valid"},0);
        for(int n=0;n<80 && live_owner.accepted_count==0;n++)@(negedge clock_vif.clk);
        if(live_owner.accepted_count==0)`uvm_fatal("DTLB0504_INTEG","initial Miss was not accepted by formal owner")
        // 请求时snapshot仍为ASID0；局部DTLB切ASID2，模拟旧上下文在途返回与新请求重叠。
        configure_context(0,0,2,0,0);target_request(0,0,va,0,0,0);
        for(int n=0;n<1200;n++)begin
            @(posedge clock_vif.clk);
            if(read_hdl({u,".io_ptw_req_0_valid"})===uvm_hdl_data_t'(1) &&
               read_hdl({u,".io_ptw_resp_valid"})===uvm_hdl_data_t'(1) &&
               read_hdl({DUT,".inner_dtlbRepeater.load_filter_load_entry.io_tlb_req_0_valid"})===uvm_hdl_data_t'(0))begin
                if(read_hdl({u,".io_ptw_resp_bits_s1_entry_asid"},16)===uvm_hdl_data_t'(2))`uvm_error("DTLB0504_INTEG","response must keep older request ASID")
                observed=1;samples_new++;
                `uvm_info("DTLB0504_SAMPLE",$sformatf("integration_real_ignore_asid q=1 local_return=1 filter_input=0 response_asid=%0h csr_asid=%0h n=%0d",read_hdl({u,".io_ptw_resp_bits_s1_entry_asid"},16),read_hdl({u,"._csr_delay_io_out_satp_asid"},16),samples_new),UVM_LOW)
                @(negedge clock_vif.clk);break;
            end
            @(negedge clock_vif.clk);
        end
        drive({u,".io_requestor_0_req_valid"},0);
        if(!observed)`uvm_error("DTLB0504_INTEG","real integration return filtering was not observed")
        group_windows++;
    endtask
    virtual task body();
        int expected;bit live;
        require_virtual_sqr();seq_csr_common::init();live=!(scenario() inside {"window","history"});
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))`uvm_fatal("DTLB0504_CLOCK","missing real clock")
        if(seq_csr_common::get_l2tlb_seq_en()!=live || memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)`uvm_fatal("DTLB0504_OWNER","scenario startup topology mismatch")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        if(live)begin
            `include "dtlb_0501_0504_live_defaults.svh"
            // 正式owner只使用monitor运行时CSR；不能只改局部DTLB配置。
            drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_satp_mode",9);
            drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_vsatp_mode",9);
            drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_hgatp_mode",9);
            drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_satp_asid",0);
            drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_vsatp_asid",0);
            drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_hgatp_vmid",0);
            drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_priv_dmode",1);
            drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_priv_imode",1);
            cycles(12);
        end else begin
            `include "dtlb_0501_0504_input_defaults.svh"
            // 真正reset过滤器状态，不钳住其输出或上游请求，局部DTLB仍在真实时钟运行。
            drive({DUT,".inner_dtlbRepeater.reset"},1);
        end
        initialize_lookup_ports();
        if(live)begin
            live_owner=memblock_l2tlb_base_sequence::type_id::create("dtlb05_formal_owner");
            live_adapter=dispatch_monitor_event_adapter::type_id::create("dtlb05_fence_adapter");
            live_owner_done=0;adapter_stop=0;
            fork
                begin live_owner.start(p_sequencer.L2tlb_sqr);live_owner_done=1;end
                begin
                    while(!adapter_stop)begin
                        @(negedge clock_vif.clk);
                        live_adapter.service_l2tlb_sfence_events();
                        // orchestration只申请已有完整证据门控的grant，不写owner/monitor状态。
                        void'(memblock_sync_pkg::grant_l2tlb_final_release());
                    end
                end
            join_none
            cycles(12);
            if(!memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)`uvm_fatal("DTLB0504_OWNER","formal owner did not start")
            if(scenario()!="integration")begin
                for(int p=0;p<8;p++)drive($sformatf("%s.inner_dtlbRepeater.io_tlb_req_%0d_valid",DUT,p),0);
                for(int g=0;g<3;g++)begin
                    string u;u=filter_path(g);
                    if(g==0)begin `include "dtlb_0501_0504_load_live.svh" end
                    else begin `include "dtlb_0501_0504_small_live.svh" end
                end
            end
        end
        case(scenario())
            "allocation":begin allocation_matrix();expected=44;end
            "merge":begin merge_matrix();expected=69;end
            "window":begin window_matrix();expected=96;end
            "history":begin history_matrix();expected=48;end
            "integration":begin integration_matrix();expected=1;end
            default:`uvm_fatal("DTLB0504_SCENARIO",scenario())
        endcase
        if(live)begin
            // 清除夹具请求后真实fence取消剩余token，再按正式stop API自然release。
            if(scenario()!="integration")filter_reset();
            else begin
                string q;q="top_tb.u_memblock__fence_agent_if.io_ooo_to_mem_sfence";
                drive({q,"_bits_rs1"},1);drive({q,"_bits_rs2"},1);drive({q,"_bits_addr"},0);drive({q,"_bits_id"},0);drive({q,"_bits_hv"},0);drive({q,"_bits_hg"},0);drive({q,"_bits_flushPipe"},0);
                drive({q,"_valid"},1);cycles(1);drive({q,"_valid"},0);cycles(16);
            end
            begin common_data_transaction data;data=common_data_transaction::get();data.request_global_stop_if_done();end
            for(int n=0;n<100 && !live_owner_done;n++)@(negedge clock_vif.clk);
            if(!live_owner_done || memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)begin
                common_data_transaction data;data=common_data_transaction::get();
                `uvm_fatal("DTLB0504_OWNER",$sformatf("formal owner did not naturally release stop=%0d raw=%0d pending=%0d close=%0d final=%0d",data.is_global_stop_requested(),memblock_sync_pkg::raw_monitor_queue_size(),live_owner.outstanding_count(),live_owner.release_close_requested,live_owner.final_item_sent))
            end
            if(live_owner.accepted_count!=live_owner.completed_count+live_owner.flush_canceled_count+live_owner.reset_canceled_count)`uvm_error("DTLB0504_ACCOUNT","formal accepted/completed/canceled mismatch")
            adapter_stop=1;wait fork;
            `uvm_info("DTLB0504_OWNER_FINAL",$sformatf("accepted=%0d completed=%0d flush_canceled=%0d reset_canceled=%0d",live_owner.accepted_count,live_owner.completed_count,live_owner.flush_canceled_count,live_owner.reset_canceled_count),UVM_LOW)
        end
        if(token_valid||extra_token_valid||samples_new!=expected||initialization_checks!=8)
            `uvm_error("DTLB0504_INCOMPLETE","target/init/lifecycle completion mismatch")
        cleanup();
        `uvm_info("DTLB0504_COMPLETE",$sformatf("scenario=%s requests=%0d returns=%0d windows=%0d initialization=%0d filter_return_checks=%0d",scenario(),samples_new,refills,group_windows,initialization_checks,filter_returns_observed),UVM_LOW)
    endtask
endclass
`define DTLB0504_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_dtlb_0501_0504_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`");super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB0504_CLASS(memblock_dtlb_05_allocation_vseq,"allocation")
`DTLB0504_CLASS(memblock_dtlb_05_merge_vseq,"merge")
`DTLB0504_CLASS(memblock_dtlb_05_window_vseq,"window")
`DTLB0504_CLASS(memblock_dtlb_05_integration_vseq,"integration")
`DTLB0504_CLASS(memblock_dtlb_05_history_vseq,"history")
`undef DTLB0504_CLASS
`endif
