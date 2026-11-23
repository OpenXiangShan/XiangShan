`ifndef MEMBLOCK_DTLB_0701_0705_VSEQ__SV
`define MEMBLOCK_DTLB_0701_0705_VSEQ__SV
// 抽象职责：在DTLB声明输入边界验证完整GPA生命周期，所有返回均消费真实PTW输出凭据。
// 正式responder关闭且Repeater保持reset；只驱动声明输入，不改内部状态或覆盖模型。
class memblock_dtlb_0701_0705_vseq extends memblock_dtlb_0401_0402_vseq;
    `uvm_object_utils(memblock_dtlb_0701_0705_vseq)
    typedef struct {int p; int stage; bit [37:0] vpn; bit getgpa; bit [8:0] rob;} local_token_t;
    // 每组最多512个在途凭据。capture从真实Valid输出入队，返回逐项删除，reset取消剩余项。
    // 此边界是Valid接口而非ready握手；不声称覆盖Repeater/Filter合并后的返回吞吐。
    local_token_t pending_ld[$],pending_st[$];
    // 仅累计本次场景真实请求、返回、取消及独立检查，结束时核对非空合同。
    int unsigned requests_07,returns_07,cancelled_07,checks_07,windows_07;
    function new(string name="memblock_dtlb_0701_0705_vseq");super.new(name);endfunction
    virtual function string scenario();return "qualify";endfunction
    function string suffix07(input int p);return p==0?"":$sformatf("_%0d",p);endfunction
    function int pending_size(input int g);return g==0?pending_ld.size():pending_st.size();endfunction
    function local_token_t pending_at(input int g,input int i);return g==0?pending_ld[i]:pending_st[i];endfunction
    // 抽象职责：严格保留四态，比较真实状态/输出与由输入场景给定的参考值。
    task expect07(input string ident,input string path,input int width,input uvm_hdl_data_t expected);
        uvm_hdl_data_t actual;actual=read_hdl(path,width);checks_07++;
        if(actual!==expected)`uvm_error("DTLB07_CHECK",$sformatf("%s path=%s expected=%h actual=%h",ident,path,expected,actual))
    endtask
    // 抽象职责：记录一次真实PTW Valid及完整VPN/stage/getGpa/ROB，不从模型产生凭据。
    task capture07(input int g,input int p,input bit [49:0] va,input bit [8:0] rob,input int expected_q=-1);
        string u,q;local_token_t t;uvm_hdl_data_t valid;
        u=unit_name(g);q=$sformatf("%s.io_ptw_req_%0d",u,p);valid=read_hdl({q,"_valid"});
        if(expected_q>=0)expect07("PTW_VALID",{q,"_valid"},1,expected_q);
        if(valid===uvm_hdl_data_t'(1))begin
            expect07("PTW_VPN",{q,"_bits_vpn"},38,va[49:12]);
            expect07("PTW_STAGE",{q,"_bits_s2xlate"},2,active_stage[g]);
            if(pending_size(g)>=512)`uvm_fatal("DTLB07_BOUND","512-token bound exceeded")
            t.p=p;t.stage=active_stage[g];t.vpn=va[49:12];t.getgpa=1'(read_hdl({q,"_bits_getGpa"}));t.rob=rob;
            if(g==0)pending_ld.push_back(t);else pending_st.push_back(t);
            `uvm_info("DTLB07_TOKEN",$sformatf("ACCEPT g=%0d p=%0d vpn=%h stage=%0d getGpa=%b rob=%h pending=%0d",g,p,t.vpn,t.stage,t.getgpa,t.rob,pending_size(g)),UVM_LOW)
        end
    endtask
    task rob_fields07(input int g,input int p,input bit [8:0] rob,input bit mabuf=0);
        string q;q=$sformatf("%s.io_requestor_%0d_req_bits",unit_name(g),p);
        drive({q,"_debug_robIdx_flag"},rob[8]);drive({q,"_debug_robIdx_value"},rob[7:0]);drive({q,"_frm_mabuf"},mabuf);
    endtask
    task redirect07(input int g,input bit valid,input bit [8:0] rob=0,input bit level=0);
        string u;u=unit_name(g);drive({u,".io_redirect_bits_robIdx_flag"},rob[8]);
        drive({u,".io_redirect_bits_robIdx_value"},rob[7:0]);drive({u,".io_redirect_bits_level"},level);drive({u,".io_redirect_valid"},valid);
    endtask
    // 抽象职责：发一次真实端口请求，在响应采样沿检查身份/异常/地址，并捕获当拍PTW凭据。
    // variant: 3预取、4当前redirect、5前拍redirect、6响应拍late-kill，其余不施加取消。
    task query07(input string ident,input int g,input int p,input bit [49:0] va,
                 input int miss=-1,input int q=-1,input bit [8:0] rob=9'h012,input bit mabuf=0,
                 input int variant=0,input bit [63:0] full=0,input bit check_gpa=0,
                 input bit [63:0] expected_gpa=0,input int nonleaf=-1,input bit prefault07=0);
        string u,r;u=unit_name(g);r=$sformatf("%s.io_requestor_%0d_resp",u,p);
        target_request(g,p,va,g==1?1:0,0,0);rob_fields07(g,p,rob,mabuf);
        if(full!=0)drive($sformatf("%s.io_requestor_%0d_req_bits_fullva",u,p),full);
        // 无预异常miss的异常位诊断限制已单独保留FAIL；替代窗口只验证真实miss_read与Q门控。
        if(prefault07)begin
            drive($sformatf("%s.io_requestor_%0d_req_bits_fullva",u,p),64'(va)^64'h8000000000000000);
            drive($sformatf("%s.io_requestor_%0d_req_bits_checkfullva",u,p),1);
        end
        if(g==0)drive($sformatf("%s.io_requestor_%0d_req_bits_isPrefetch",u,p),variant==3);
        if(variant==5)redirect07(g,1,9'h010);
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);
        drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
        if(variant==5)redirect07(g,0);
        if(variant==4)redirect07(g,1,9'h010);
        if(g==0&&p<3)drive($sformatf("%s.io_requestor_%0d_req_kill",u,p),variant==6);
        @(posedge clock_vif.clk);
        expect07(ident,{r,"_valid"},1,1);expect07(ident,{indexed(u,"req_out",p),"_vaddr"},50,va);
        if(miss>=0)expect07(ident,{r,"_bits_miss"},1,prefault07?0:miss);
        if(prefault07)begin
            expect07("PREFAULT_RAW_MISS",{u,".miss_read_",$sformatf("%0d",p)},1,1);
            if(actual_exception(r)!==(g==1?6'h02:6'h01))`uvm_error("DTLB07_PREFAULT",ident)
        end
        if(check_gpa)begin
            expect07(ident,{r,"_bits_gpaddr_0"},64,expected_gpa);
            if(actual_exception(r)!==(g==1?6'h08:6'h04))`uvm_error("DTLB07_FAULT",ident)
        end
        if(nonleaf>=0&&!(g==0&&p==3))expect07(ident,{r,"_bits_isForVSnonLeafPTE"},1,nonleaf);
        if(ident=="address_start"||ident=="range_start"||ident=="counter_request")begin
            expect07("GPF_STORAGE_HIT",$sformatf("%s._entries_io_r_resp_%0d_bits_hit",u,p),1,1);
            expect07("GPF_REQUEST_KIND",$sformatf("%s.io_ptw_req_%0d_bits_getGpa",u,p),1,1);
        end
        if(variant==2)expect07("NONHEAD_REPLAY",$sformatf("%s.need_replay_%0d",u,p),1,1);
        capture07(g,p,va,rob,q);requests_07++;
        `uvm_info("DTLB07_QUERY",$sformatf("%s g=%0d p=%0d va=%h rob=%h stage=%0d miss=%h fault=%h ng=%h ready=%h q=%h",ident,g,p,va,rob,active_stage[g],1'(read_hdl({r,"_bits_miss"})),actual_exception(r),1'(read_hdl({u,".need_gpa"})),1'(read_hdl({u,".resp_gpa_refill"})),1'(read_hdl($sformatf("%s.io_ptw_req_%0d_valid",u,p)))),UVM_LOW)
        @(negedge clock_vif.clk);redirect07(g,0);
        if(prefault07)drive($sformatf("%s.io_requestor_%0d_req_bits_checkfullva",u,p),0);
        if(g==0&&p<3)drive($sformatf("%s.io_requestor_%0d_req_kill",u,p),0);
    endtask
    // 抽象职责：消费已接受的唯一凭据设置PTE输入；返回页范围必须覆盖该凭据的原VPN。
    // kind=0叶、1有效nonleaf、2fakePTE；tag_delta仅允许同一大页/NAPOT范围内改变原始tag。
    task payload07(input int g,input int index=0,input int level=0,input bit napot=0,
                   input int kind=0,input bit [47:0] s1pa=48'h12345000,input bit [49:0] tag_delta=0);
        local_token_t t;string u;bit [49:0] va,tagva;int saved;
        if(index>=pending_size(g))`uvm_fatal("DTLB07_TOKEN","response without live request")
        t=pending_at(g,index);u=unit_name(g);va={t.vpn,12'b0};tagva=va^tag_delta;
        if((tagva>>(napot?16:level==0?12:12+9*level))!=(va>>(napot?16:level==0?12:12+9*level)))`uvm_fatal("DTLB07_RANGE","response outside originating token page")
        saved=active_stage[g];active_stage[g]=t.stage;token_valid=1;token_group=g;token_vpn=t.vpn;token_stage=t.stage;
        return_fields(g,va,48'h40000000,kind==0?6'h3f:6'h38,0,5'h1f,1);
        token_valid=0;active_stage[g]=saved;
        drive({u,".io_ptw_resp_bits_getGpa"},t.getgpa);
        drive({u,".io_ptw_resp_bits_s1_entry_tag"},tagva[49:15]);drive({u,".io_ptw_resp_bits_s1_addr_low"},tagva[14:12]);
        drive({u,".io_ptw_resp_bits_s2_entry_tag"},tagva[49:12]);
        drive({u,".io_ptw_resp_bits_s1_entry_level"},level);drive({u,".io_ptw_resp_bits_s2_entry_level"},level);
        drive({u,".io_ptw_resp_bits_s1_entry_v"},kind!=2);drive({u,".io_ptw_resp_bits_s1_entry_ppn"},s1pa[47:15]);
        drive({u,".io_ptw_resp_bits_s1_entry_n"},napot);drive({u,".io_ptw_resp_bits_s2_entry_n"},napot);
        if(napot)drive({u,".io_ptw_resp_bits_s1_entry_ppn"},41'(s1pa[47:15])|41'b1);
        for(int i=0;i<8;i++)begin
            drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",u,i),i==tagva[14:12]);
            drive($sformatf("%s.io_ptw_resp_bits_s1_ppn_low_%0d",u,i),s1pa[14:12]);
        end
    endtask
    task retire07(input int g,input int index=0);
        local_token_t t;t=pending_at(g,index);
        if(g==0)pending_ld.delete(index);else pending_st.delete(index);
        returns_07++;`uvm_info("DTLB07_TOKEN",$sformatf("RETURN g=%0d p=%0d vpn=%h stage=%0d getGpa=%b rob=%h pending=%0d",g,t.p,t.vpn,t.stage,t.getgpa,t.rob,pending_size(g)),UVM_LOW)
    endtask
    // 抽象职责：真实返回后独立核对共享状态、事件计数及普通表写入抑制。
    task respond07(input int g,input int index=0,input int level=0,input bit napot=0,
                   input int kind=0,input bit [47:0] s1pa=48'h12345000,input bit [49:0] tag_delta=0,
                   input int ready=1,input int count=-1,input bit red=0);
        string u;u=unit_name(g);payload07(g,index,level,napot,kind,s1pa,tag_delta);
        drive({u,".io_ptw_resp_valid"},1);if(red)redirect07(g,1,9'h010);#1ps;
        if(read_hdl({u,".need_gpa"})===uvm_hdl_data_t'(1))expect07("WAIT_REFILL_SUPPRESSED",{u,".entries.io_w_valid"},1,0);
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);redirect07(g,0);retire07(g,index);
        if(ready>=0)expect07("RETURN_READY",{u,".resp_gpa_refill"},1,ready);
        if(count>=0)expect07("EVENT_COUNTER",{u,".need_gpa_count"},9,count);
        cycles(2);
    endtask
    // 抽象职责：复位取消本窗口未返回凭据，然后经真实miss返回建立GPF和普通hit映射。
    task window07(input int g,input int stage=3,input bit save_ordinary=0,input bit save_wrong_stage=0,input int gpf_kind=0,input int gpf_level=0,input bit [49:0] gpf_va=50'h80000000);
        string u;u=unit_name(g);drive({u,".reset"},1);cancelled_07+=pending_size(g);
        if(pending_size(g)>0)`uvm_info("DTLB07_TOKEN",$sformatf("RESET_CANCEL g=%0d count=%0d",g,pending_size(g)),UVM_LOW)
        if(g==0)pending_ld.delete();else pending_st.delete();token_valid=0;
        for(int p=0;p<ports(g);p++)begin
            drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);rob_fields07(g,p,9'h012);
        end
        drive({u,".io_ptw_resp_valid"},0);drive({u,".io_ptw_resp_bits_getGpa"},0);redirect07(g,0);configure_context(g,stage);
        drive({u,".io_robPendingPtr_flag"},0);drive({u,".io_robPendingPtr_value"},8'h12);
        if(save_wrong_stage)begin configure_context(g,0,1,1,0);query07("older_host_token",g,0,50'hc0000000,1,1);configure_context(g,stage,1,1,0);end
        if(save_ordinary)query07("older_normal_token_prefault",g,0,50'h80000000,1,1,9'h012,0,0,0,0,0,-1,1);
        if(gpf_kind==0)begin
        prepare_miss(g,50'h90000000);return_fields(g,50'h90000000,48'h50000000,6'h3f,0,5'h1f,0);
        drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);token_valid=0;cycles(8);
        end
        prepare_miss(g,gpf_va);return_fields(g,gpf_va,48'h40000000,gpf_kind==0?6'h3f:6'h38,0,5'h1f,1);
        drive({u,".io_ptw_resp_bits_s1_entry_v"},gpf_kind!=2);
        drive({u,".io_ptw_resp_bits_s1_entry_level"},gpf_level);drive({u,".io_ptw_resp_bits_s2_entry_level"},gpf_level);
        for(int i=0;i<8;i++)drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",u,i),i==gpf_va[14:12]);
        drive({u,".io_ptw_resp_valid"},1);#1ps;expect07("ORIGINAL_GPF_WRITE",{u,".entries.io_w_valid"},1,1);
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);token_valid=0;cycles(8);windows_07++;
    endtask
    task owner07(input int g,input bit [49:0] va,input bit [8:0] rob,input bit ng=1);
        string u;u=unit_name(g);expect07("OWNER_NG",{u,".need_gpa"},1,ng);
        expect07("OWNER_VALID",{u,".need_gpa_robidx_valid"},1,1);
        expect07("OWNER_VPN",{u,".need_gpa_vpn"},38,va[49:12]);expect07("OWNER_STAGE",{u,".need_gpa_s2xlate"},2,3);
        expect07("OWNER_FLAG",{u,".need_gpa_robidx_flag"},1,rob[8]);expect07("OWNER_VALUE",{u,".need_gpa_robidx_value"},8,rob[7:0]);
    endtask
    task qualify_matrix07();
        for(int g=0;g<2;g++)for(int p=0;p<ports(g);p++)for(int v=0;v<7;v++)begin
            bit start;string u;u=unit_name(g);if((v==3&&g!=0)||(v==6&&(g!=0||p==3)))continue;
            window07(g);query07($sformatf("qualification_%0d",v),g,p,50'h80000000,-1,v<2?1:v==2||v==6?0:-1,v==1||v==2?9'h013:9'h012,v==1,v);
            start=v<2;expect07("QUALIFICATION_NG",{u,".need_gpa"},1,start);
            if(start)owner07(g,50'h80000000,v==1?9'h013:9'h012);
            cycles(2);
        end
    endtask
    task wait_matrix07();
        string u;bit [43:0] saved;
        for(int g=0;g<2;g++)for(int p=0;p<ports(g);p++)begin
            u=unit_name(g);window07(g,3,1,1);
            query07("start",g,p,50'h80000000,1,1);owner07(g,50'h80000000,9'h012);
            query07("same_page_wait",g,p,50'h80000000,1,1);
            query07("other_page_wait_prefault",g,p,50'ha0000000,1,0,9'h012,0,0,0,0,0,-1,1);
            query07("ordinary_hit_wait",g,p,50'h90000000,0,0);
            query07("owner_flag_reuse",g,p,50'h90000000,0,0,9'h112);
            respond07(g,0,0,0,0,48'h12345000,0,0,0); // 旧Host返回不匹配，保持ready=0。
            respond07(g,0,0,0,0,48'h12345000,0,1,0); // 旧普通返回匹配，getGpa=0也保存GPA。
            query07("other_page_ready_prefault",g,p,50'ha0000000,1,1,9'h012,0,0,0,0,0,-1,1);
            saved=44'(read_hdl({u,".resp_gpa_gvpn"},44));
            respond07(g,0,0,0,0,48'h12345000,0,1,1);
            expect07("SAVED_GVPN",{u,".resp_gpa_gvpn"},44,48'h12345000>>12);
            // 同VPN换stage不得消费旧快照；返回凭据仍保存旧stage，不由新CSR重标。
            configure_context(g,1,1,1,0);query07("same_vpn_new_stage",g,p,50'h80000000,1,-1);
            expect07("OLD_STAGE_NOT_CONSUMED",{u,".need_gpa"},1,1);configure_context(g,3,1,1,0);
            // 同页剩余getGpa返回先完成，异页与新stage返回在消费前分别检查不覆盖元数据。
            respond07(g,0,0,0,0,48'h12345000,0,1,2);
            respond07(g,0,0,0,0,48'h22345000,0,1,2);
            expect07("MISMATCH_GVPN_HOLD",{u,".resp_gpa_gvpn"},44,saved);
            if(pending_size(g)>0)begin respond07(g,0,0,0,0,48'h32345000,0,1,2);expect07("STAGE_MISMATCH_GVPN_HOLD",{u,".resp_gpa_gvpn"},44,saved);end
            query07("consume",g,p,50'h80000000,0,0,9'h012,0,0,0,!(g==0&&p==3),64'h12345000,0);
            expect07("CONSUMED_NG",{u,".need_gpa"},1,0);
            query07("cached_gpa_no_restart",g,p,50'h80000000,0,0,9'h012,0,0,0,!(g==0&&p==3),64'h12345000,0);
        end
    endtask
    task address_matrix07();
        bit [49:0] va,delta;bit [63:0] full,expected;bit [47:0] pa;int level,kind,mode;
        va=50'h80000000;pa=48'h12345000;
        for(int g=0;g<2;g++)for(int p=0;p<ports(g);p++)begin
            if(g==0&&p==3)continue;
            for(int v=0;v<11;v++)begin
                level=v>=5&&v<=7?v-4:0;kind=v>=8?2:v>=5?1:0;mode=v==8?8:9;
                va=v>=5?(v==8?50'h1234567000:50'h12345678000):50'h80000000;
                window07(g,v<2?2:3,0,0,kind,level,va);
                if(mode==8)begin drive({unit_name(g),".io_csr_vsatp_mode"},8);cycles(6);end
                full=v==0||v==2?64'h80000ffc:v==1||v==3?64'h7ffffffc:64'(va);
                if(v<2)begin expected=full[12]==va[12]?full:{14'd0,va};query07("onlyS2_full_address",g,p,va,0,0,9'h012,0,0,full,1,expected,0);end
                else begin
                    query07("address_start",g,p,va,1,1);owner07(g,va,9'h012);
                    respond07(g,0,level,0,kind,pa);
                    if(kind==0)expected={16'b0,pa[47:12],12'b0}|(full[12]==va[12]?64'(full[11:0]):64'(va[11:0]));
                    else begin expected={16'b0,pa[47:12],12'b0};expected|=kind==2?64'((va>>(mode==8?30:39))&9'h1ff)<<3:64'((va>>(12+9*(level-1)))&9'h1ff)<<3;end
                    query07($sformatf("gpa_address_%0d",v),g,p,va,0,0,9'h012,0,0,full,1,expected,kind!=0);
                end
            end
            // 大页/NAPOT使用不同低位原始tag，但仍必须属于原请求页范围。
            va=50'h80000000;for(int l=0;l<4;l++)begin
                window07(g);query07("range_start",g,p,va,1,1);delta=l==0?50'h1000:50'h8000;
                respond07(g,0,l,l==0,0,48'h20000000,delta);
                expected=l==0?64'h20000000:(64'h20000000&~((64'h1<<(12+9*l))-1))|(64'(va)&((64'h1<<(12+9*l))-1));
                query07("range_consume",g,p,va,0,0,9'h012,0,0,0,1,expected,0);
            end
        end
    endtask
    // 抽象职责：为并发候选独立建立真实GPF sector，不能把全局启动mask当逐口命中证明。
    task add_owner_sector07(input int g,input bit [49:0] va);
        string u;u=unit_name(g);prepare_miss(g,va);
        return_fields(g,va,48'h40000000+48'(va[14:12])*4096,6'h3f,0,5'h1f,1);
        drive({u,".io_ptw_resp_bits_getGpa"},0);
        for(int i=0;i<8;i++)drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",u,i),i==va[14:12]);
        drive({u,".io_ptw_resp_valid"},1);#1ps;expect07("OWNER_SECTOR_WRITE",{u,".entries.io_w_valid"},1,1);
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);token_valid=0;cycles(8);
    endtask
    // 抽象职责：验证PF两口真实GPF预取抑制；该组没有ROB/GPA地址输入，不伪造能力。
    task pf_suppression07();
        string u;u=unit_name(2);configure_context(2,3);drive({u,".io_ptw_resp_bits_getGpa"},0);
        prepare_miss(2,50'h80000000);return_fields(2,50'h80000000,48'h40000000,6'h3f,0,5'h1f,1);
        for(int i=0;i<8;i++)drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",u,i),i==0);
        drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);token_valid=0;cycles(8);
        for(int p=0;p<2;p++)begin
            target_request(2,p,50'h80000000,0,0,0);drive($sformatf("%s.io_requestor_%0d_req_bits_isPrefetch",u,p),1);
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
            @(posedge clock_vif.clk);expect07("PF_GPF_HIT",$sformatf("%s._entries_io_r_resp_%0d_bits_hit",u,p),1,1);
            if(actual_exception($sformatf("%s.io_requestor_%0d_resp",u,p))!==6'h04)`uvm_error("DTLB07_PF_GPF","missing genuine prefetch GPF")
            expect07("PF_SUPPRESS",{u,".need_gpa_wire"},1,0);expect07("PF_NO_PTW",$sformatf("%s.io_ptw_req_%0d_valid",u,p),1,0);
            @(negedge clock_vif.clk);expect07("PF_NO_OWNER",{u,".need_gpa"},1,0);requests_07++;
        end
        drive({u,".reset"},1);cycles(2);windows_07++;
    endtask
    // 抽象职责：同拍消费旧owner并重试另一GPF，独立验证该拍不能新建、下一次重试才接管。
    // 当ng=1时所有start分支被抑制，保留07.05.002仓并记录真实时序，不force不可达组合。
    task consume_race07(input int g);
        string u;u=unit_name(g);window07(g);add_owner_sector07(g,50'h80001000);
        query07("race_old_start",g,0,50'h80000000,1,1);respond07(g);
        for(int p=0;p<2;p++)begin
            target_request(g,p,50'h80000000+50'(p)*4096,g==1?1:0,0,0);rob_fields07(g,p,9'(18+p),1);
        end
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);
        for(int p=0;p<2;p++)drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
        @(posedge clock_vif.clk);expect07("RACE_OLD_NG",{u,".need_gpa"},1,1);expect07("RACE_NO_SAME_EDGE_START",{u,".need_gpa_wire"},1,0);
        for(int p=0;p<2;p++)begin
            expect07("RACE_REAL_GPF_HIT",$sformatf("%s._entries_io_r_resp_%0d_bits_hit",u,p),1,1);
            expect07("RACE_MISS",$sformatf("%s.io_requestor_%0d_resp_bits_miss",u,p),1,p==1);
            capture07(g,p,50'h80000000+50'(p)*4096,9'(18+p),p==1);requests_07++;
        end
        expect07("RACE_OLD_GPA",{u,".io_requestor_0_resp_bits_gpaddr_0"},64,64'h12345000);
        @(negedge clock_vif.clk);expect07("RACE_CONSUME_EXIT",{u,".need_gpa"},1,0);
        query07("race_new_retry",g,1,50'h80001000,1,1,9'h013,1);owner07(g,50'h80001000,9'h013);
        respond07(g);query07("race_new_consume",g,1,50'h80001000,0,0,9'h013,1,0,0,1,64'h12345000,0);
        expect07("RACE_NEW_EXIT",{u,".need_gpa"},1,0);
    endtask
    task owner_matrix07();
        string u;int highest;
        for(int g=0;g<2;g++)begin
            u=unit_name(g);highest=ports(g)-1;window07(g);
            for(int p=1;p<ports(g);p++)add_owner_sector07(g,50'h80000000+50'(p)*4096);
            for(int p=0;p<ports(g);p++)begin target_request(g,p,50'h80000000+50'(p)*4096,g==1?1:0,0,0);rob_fields07(g,p,9'(18+p),1);end
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);
            for(int p=0;p<ports(g);p++)drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
            @(posedge clock_vif.clk);for(int p=0;p<ports(g);p++)begin expect07("OWNER_CANDIDATE_HIT",$sformatf("%s._entries_io_r_resp_%0d_bits_hit",u,p),1,1);
                expect07("OWNER_CANDIDATE_GPF",$sformatf("%s.hasGpf_%0d",u,p),1,1);
                expect07("OWNER_CANDIDATE_ELIGIBLE",$sformatf("%s.canGetGpa_%0d",u,p),1,1);
                expect07("OWNER_CANDIDATE_GETGPA",$sformatf("%s.io_ptw_req_%0d_bits_getGpa",u,p),1,1);
                capture07(g,p,50'h80000000+50'(p)*4096,9'(18+p),1);requests_07++;end
            @(negedge clock_vif.clk);owner07(g,50'h80000000+50'(highest)*4096,9'(18+highest));
            redirect07(g,1,9'h030);cycles(1);redirect07(g,0);expect07("OLDER_OWNER_SURVIVES",{u,".need_gpa"},1,1);
            // 不取消的旧owner返回与真正flush同时发生，清理优先；返回在该沿前仍属于有效凭据。
            respond07(g,0,0,0,0,48'h12345000,0,0,0,1);
            expect07("REDIRECT_NG",{u,".need_gpa"},1,0);expect07("REDIRECT_OWNER",{u,".need_gpa_robidx_valid"},1,0);expect07("REDIRECT_VPN",{u,".need_gpa_vpn"},38,0);
            // 剩余凭据因redirect取消，绝不再注入返回，复位窗口记录取消数量。
            window07(g,3,1);query07("fast_start",g,0,50'h80000000,1,1);
            // 此处已启动状态；fast启动同拍的专用窗口见下段。
            for(int p=0;p<ports(g);p++)begin
                window07(g,3,1);target_request(g,p,50'h80000000,g==1?1:0,0,0);rob_fields07(g,p,9'h012);
                @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
                payload07(g);drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);
                expect07("FAST_STORAGE_GPF",{u,".need_gpa_wire"},1,1);
                expect07("FAST_MATCH",{u,".readResult_p_hit_fast",suffix07(p)},1,1);
                requests_07++;@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);retire07(g);
                expect07("FAST_DELAY_CLEAR",{u,".need_clear_need_gpa"},1,1);cycles(2);expect07("FAST_EXIT",{u,".need_gpa"},1,0);
            end
            consume_race07(g);
            // 等待期间sfence仅清表，不直接清GPA；另表独立hit继续前进。
            window07(g);query07("fence_wait_start",g,0,50'h80000000,1,1);drive({u,".io_sfence_bits_rs1"},1);drive({u,".io_sfence_bits_rs2"},1);drive({u,".io_sfence_valid"},1);cycles(1);drive({u,".io_sfence_valid"},0);cycles(8);expect07("FENCE_WAIT_REMAINS",{u,".need_gpa"},1,1);
            window07(1-g);query07("other_table_progress",1-g,0,50'h90000000,0,0);expect07("INDEPENDENT_WAIT_REMAINS",{u,".need_gpa"},1,1);
        end
    endtask
    // 抽象职责：先收集511次真实同页getGpa Valid，再逐项返回，不造无主响应或写计数。
    // 这是DTLB模块输入边界压力验证；Filter合并路径与其容量不在此场景的结论范围。
    task counter_matrix07();
        string u;
        for(int g=0;g<2;g++)begin
            u=unit_name(g);window07(g);
            for(int n=0;n<511;n++)query07("counter_request",g,n%ports(g),50'h80000000,1,1);
            expect07("IDLE_NOT_TIME_COUNTER",{u,".need_gpa_count"},9,0);
            if(pending_size(g)!=511)`uvm_fatal("DTLB07_CREDENTIALS","511 actual PTW requests required")
            for(int n=1;n<=511;n++)begin
                payload07(g);drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);retire07(g);
                expect07("COUNTER_EVENT",{u,".need_gpa_count"},9,n);expect07("WAIT_UNCONSUMED",{u,".need_gpa"},1,1);
                if(n<511)begin cycles(2);expect07("COUNTER_IDLE_HOLD",{u,".need_gpa_count"},9,n);end
            end
            cycles(2);expect07("FULL_EXIT",{u,".need_gpa"},1,0);expect07("FULL_ZERO",{u,".need_gpa_count"},9,0);
        end
    endtask
    virtual task body();
        require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))`uvm_fatal("DTLB07_CLOCK","missing clock")
        if(seq_csr_common::get_l2tlb_seq_en()||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)`uvm_fatal("DTLB07_OWNER","local fixture requires formal responder disabled")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        `include "dtlb_0501_0504_input_defaults.svh"
        drive({DUT,".inner_dtlbRepeater.reset"},1);initialize_lookup_ports();
        case(scenario())
            "qualify":begin qualify_matrix07();pf_suppression07();end
            "wait":wait_matrix07();"address":address_matrix07();"owner":owner_matrix07();"counter":counter_matrix07();
            default:`uvm_fatal("DTLB07_SCENARIO",scenario())
        endcase
        for(int g=0;g<2;g++)begin drive({unit_name(g),".reset"},1);cancelled_07+=pending_size(g);end
        pending_ld.delete();pending_st.delete();
        if(checks_07==0||windows_07==0||initialization_checks!=8)`uvm_error("DTLB07_INCOMPLETE","scenario contract incomplete")
        cleanup();`uvm_info("DTLB07_COMPLETE",$sformatf("scenario=%s requests=%0d returns=%0d reset_cancelled=%0d checks=%0d windows=%0d initialization=%0d",scenario(),requests_07,returns_07,cancelled_07,checks_07,windows_07,initialization_checks),UVM_LOW)
    endtask
endclass
`define DTLB07_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_dtlb_0701_0705_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`" );super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB07_CLASS(memblock_dtlb_07_qualify_vseq,"qualify")
`DTLB07_CLASS(memblock_dtlb_07_wait_vseq,"wait")
`DTLB07_CLASS(memblock_dtlb_07_address_vseq,"address")
`DTLB07_CLASS(memblock_dtlb_07_owner_vseq,"owner")
`DTLB07_CLASS(memblock_dtlb_07_counter_vseq,"counter")
`undef DTLB07_CLASS
`endif
