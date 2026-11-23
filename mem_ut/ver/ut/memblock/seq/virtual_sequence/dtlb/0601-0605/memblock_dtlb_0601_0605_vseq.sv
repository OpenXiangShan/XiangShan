`ifndef MEMBLOCK_DTLB_0601_0605_VSEQ__SV
`define MEMBLOCK_DTLB_0601_0605_VSEQ__SV
// 抽象职责：在DTLB声明输入边界验证回填、页型合并和旁路，参考值仅来自请求/PTE。
// 正式L2TLB owner关闭，Repeater保持reset；局部Valid凭据不声称Filter端到端握手。
class memblock_dtlb_0601_0605_vseq extends memblock_dtlb_0701_0705_vseq;
    `uvm_object_utils(memblock_dtlb_0601_0605_vseq)
    // 计数在真实请求、返回及比较处增加；body按固定矩阵核验，不能替代独立VDB。
    int unsigned windows_06,checks_06,returns_06,prepared_06,bypass_06,storage_06;
    // 优先级窗口只额外保存一个已发出的同VPN凭据；首次返回消费它，第二次消费父类token。
    // 不创建全局owner或无界队列，不能跨context/reset继续使用。
    bit older_token_06;
    function new(string name="memblock_dtlb_0601_0605_vseq");super.new(name);endfunction
    virtual function string scenario();return "merge";endfunction

    // 抽象职责：从原始PTE与输入地址独立计算页内拼接，支持4KiB、64KiB和三级超级页。
    function bit [47:0] translated06(input bit [49:0] address,input bit [47:0] raw_pa,
                                     input int level,input bit napot);
        bit [49:0] mask;int bits_low;
        bits_low=napot?16:12+9*level;mask=(50'b1<<bits_low)-1;
        return (raw_pa&~48'(mask))|48'(address&mask);
    endfunction
    function logic [47:0] valid06(input int g);
        logic [47:0] value;
        // 仅在回填事件前后读48个物理way，检查范围不随历史事务增长。
        for(int w=0;w<48;w++)value[w]=1'(read_hdl($sformatf("%s.v_%0d",array_path(g),w)));
        return value;
    endfunction
    task check06(input string ident,input string path,input int width,input uvm_hdl_data_t expected);
        uvm_hdl_data_t actual;actual=read_hdl(path,width);checks_06++;
        if(actual!==expected)`uvm_error("DTLB06_CHECK",$sformatf("%s path=%s expected=%h actual=%h",ident,path,expected,actual))
    endtask
    task prepare06(input int g,input bit [49:0] va);
        prepare_miss(g,va);prepared_06++;
        `uvm_info("DTLB06_TOKEN",$sformatf("ACCEPT g=%0d vpn=%h stage=%0d",g,va[49:12],active_stage[g]),UVM_LOW)
    endtask

    // 抽象职责：把唯一实际请求凭据转换为合法S1/S2叶返回；先不置valid，允许安排并发查找。
    // fault:0正常，1=S1 PF，2=S1 AF，3=S2 GPF，4=S2 GAF；正常页型矩阵与异常独立。
    task mapping_fields06(input int g,input bit [49:0] va,input bit [47:0] s1base,input bit [47:0] s2base,
                          input int l1=0,input int l2=0,input bit n1=0,input bit n2=0,input int fault=0);
        string u;bit [47:0] raw1,raw2,gpa;bit [49:0] mask1,mask2;
        u=unit_name(g);mask1=(50'b1<<(n1?16:12+9*l1))-1;mask2=(50'b1<<(n2?16:12+9*l2))-1;
        raw1=s1base&~48'(mask1);raw2=s2base&~48'(mask2);
        if(n1)raw1|=48'h8000;if(n2)raw2|=48'h8000;
        gpa=active_stage[g]==2?48'(va):translated06(va,raw1,l1,n1);
        return_fields(g,va,raw2,6'h3f,fault==1,5'h1f,fault==3);
        drive({u,".io_ptw_resp_bits_getGpa"},0);
        drive({u,".io_ptw_resp_bits_s1_entry_ppn"},raw1[47:15]);
        drive({u,".io_ptw_resp_bits_s1_entry_level"},l1);drive({u,".io_ptw_resp_bits_s1_entry_n"},n1);
        drive({u,".io_ptw_resp_bits_s1_af"},fault==2);
        drive({u,".io_ptw_resp_bits_s2_entry_ppn"},raw2[47:12]);
        drive({u,".io_ptw_resp_bits_s2_entry_level"},l2);drive({u,".io_ptw_resp_bits_s2_entry_n"},n2);
        drive({u,".io_ptw_resp_bits_s2_entry_tag"},active_stage[g]==2?va[49:12]:38'(gpa>>12));
        drive({u,".io_ptw_resp_bits_s2_gaf"},fault==4);
        for(int i=0;i<8;i++)begin
            drive($sformatf("%s.io_ptw_resp_bits_s1_ppn_low_%0d",u,i),raw1[14:12]);
            drive($sformatf("%s.io_ptw_resp_bits_s1_valididx_%0d",u,i),i==va[14:12]);
            drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",u,i),i==va[14:12]);
        end
    endtask

    // 抽象职责：在有效响应拍核对身份、命令、异常、PA以及旁路中间GVPN，避免模型自比较。
    task check_bypass06(input string ident,input int g,input int p,input bit [49:0] va,
                        input bit [47:0] expected_pa,input bit [47:0] expected_gpa,
                        input bit storage_hit=0,input int fault=0);
        string u,r;logic [5:0] exception,expected_excp;
        u=unit_name(g);r=$sformatf("%s.io_requestor_%0d_resp",u,p);
        check06(ident,{r,"_valid"},1,1);check06(ident,{indexed(u,"req_out",p),"_vaddr"},50,va);
        check06(ident,{indexed(u,"req_out",p),"_cmd"},3,g==1?1:0);
        check06(ident,{r,"_bits_miss"},1,0);
        check06(ident,{u,".readResult_p_hit_last_REG",suffix07(p)},1,1);
        check06(ident,$sformatf("%s._entries_io_r_resp_%0d_bits_hit",u,p),1,storage_hit);
        check06(ident,{r,"_bits_paddr_0"},48,expected_pa);
        // V2只有LD0～2与ST两口保留独立GVPN；LD3/PF仍逐口严格比较最终PA。
        if(g==1 || (g==0 && p<3))
            check06(ident,{u,".readResult_p_gvpn",suffix07(p)},44,44'(expected_gpa>>12));
        check06(ident,{r,"_bits_pbmt_0"},2,0);
        // 当前接口load有两份PA、store有一份PA；按真实存在的输出逐份比较。
        if(uvm_hdl_check_path({r,"_bits_paddr_1"}))check06(ident,{r,"_bits_paddr_1"},48,expected_pa);
        expected_excp=fault==0?0:fault==2?(g==1?6'h08:6'h04):(g==1?6'h02:6'h01);
        exception=actual_exception(r);checks_06++;
        if(exception!==expected_excp)`uvm_error("DTLB06_FAULT",$sformatf("%s g=%0d p=%0d expected=%h actual=%h",ident,g,p,expected_excp,exception))
        bypass_06++;
        `uvm_info("DTLB06_BYPASS",$sformatf("%s g=%0d p=%0d stage=%0d va=%h expected_pa=%h expected_gpa=%h storage_hit=%0b",ident,g,p,active_stage[g],va,expected_pa,expected_gpa,storage_hit),UVM_LOW)
    endtask

    // 抽象职责：检查一次真实回填的合并元信息、写后entry和不同偏移复访。
    task merge_case06(input int g,input int l1,input int l2,input bit n1=0,input bit n2=0,input int fault=0);
        string u,a;bit [49:0] va,nextva;bit [47:0] s1base,s2base,gpa,pa;
        logic [47:0] before_v,added;int level,w;bit napot;
        u=unit_name(g);a=array_path(g);va=50'h123456789123;
        s1base=48'h120000000000;s2base=48'h250000000000;
        configure_context(g,3);prepare06(g,va);mapping_fields06(g,va,s1base,s2base,l1,l2,n1,n2,fault);
        level=(fault==1||fault==2)?l1:(l1<l2?l1:l2);
        napot=(n1&&l2>0)||(n2&&l1>0)||(n1&&n2);
        before_v=valid06(g);drive({u,".io_ptw_resp_valid"},1);#1ps;
        check06("MERGE_WRITE",{u,".entries.io_w_valid"},1,1);
        check06("MERGE_LEVEL",{a,".inner_level"},2,level);check06("MERGE_NAPOT",{a,".inner_n"},1,napot);
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);
        token_valid=0;returns_06++;refills++;cycles(8);added=valid06(g)&~before_v;w=-1;
        if($isunknown(added)||$countones(added)!=1)`uvm_fatal("DTLB06_WRITE",$sformatf("g=%0d new_valid=%h",g,added))
        for(int i=0;i<48;i++)if(added[i])w=i;
        check06("SAVED_LEVEL",$sformatf("%s.entries_%0d_level",a,w),2,level);
        check06("SAVED_NAPOT",$sformatf("%s.entries_%0d_n",a,w),1,napot);
        check06("SAVED_PF",$sformatf("%s.entries_%0d_perm_pf",a,w),1,fault==1);
        check06("SAVED_AF",$sformatf("%s.entries_%0d_perm_af",a,w),1,fault==2);
        check06("SAVED_GPF",$sformatf("%s.entries_%0d_g_perm_pf",a,w),1,fault==3);
        check06("SAVED_GAF",$sformatf("%s.entries_%0d_g_perm_af",a,w),1,fault==4);
        if(fault==0)begin
            nextva=(va&~50'hfff)|50'h567;
            gpa=translated06(nextva,s1base,l1,n1);pa=translated06(50'(gpa),s2base,l2,n2);
            probe("merge_revisit",g,0,nextva,pa);storage_06++;
            // 合并大页/NAPOT再跨一个4KiB子页；4KiB只检查同页的独立字节偏移。
            if(level>0||napot)begin
                nextva=va+50'h1000;gpa=translated06(nextva,s1base,l1,n1);pa=translated06(50'(gpa),s2base,l2,n2);
                probe("merge_subpage",g,0,nextva,pa);storage_06++;
            end
        end
        windows_06++;
        `uvm_info("DTLB06_MERGE",$sformatf("g=%0d l1=%0d l2=%0d n1=%0b n2=%0b fault=%0d level=%0d n=%0b way=%0d",g,l1,l2,n1,n2,fault,level,napot,w),UVM_LOW)
    endtask
    task merge_matrix06();
        for(int g=0;g<3;g++)begin
            for(int l1=0;l1<4;l1++)for(int l2=0;l2<4;l2++)merge_case06(g,l1,l2);
            merge_case06(g,0,1,1,0);merge_case06(g,1,0,0,1);merge_case06(g,0,0,1,1);
            merge_case06(g,0,0,1,0);merge_case06(g,0,0,0,1);
            merge_case06(g,2,0,0,0,1);merge_case06(g,2,0,0,0,2);
            merge_case06(g,1,2,0,0,3);merge_case06(g,1,2,0,0,4);
        end
    endtask

    // 抽象职责：让合法PTE与所有物理端口同拍查找，覆盖每种页型的首、非零偏移和末地址。
    task bypass_matrix06();
        bit [49:0] va,anchor,offset,mask;bit [47:0] s1base,s2base,gpa,pa;int level,steps;bit napot;
        s1base=48'h120000000000;s2base=48'h250000000000;
        for(int stage=0;stage<4;stage++)for(int shape=0;shape<5;shape++)begin
            napot=shape==1;level=shape<2?0:shape-1;mask=(50'b1<<(napot?16:12+9*level))-1;
            anchor=50'h123456789000&~mask;steps=napot?16:3;
            for(int sample_id=0;sample_id<steps;sample_id++)begin
                offset=napot?(50'(sample_id)*4096+(sample_id==0?0:sample_id==15?4095:291)):
                             sample_id==0?0:sample_id==2?mask:(mask > 50'h12345 ? 50'h12345 : 50'h123);
                va=anchor+offset;
                for(int g=0;g<3;g++)begin
                    string u;u=unit_name(g);configure_context(g,stage);prepare06(g,va);
                    mapping_fields06(g,va,s1base,s2base,level,level,napot&&stage!=2,napot&&stage>=2);
                    gpa=stage==2?48'(va):translated06(va,s1base,level,napot);
                    pa=stage>=2?translated06(50'(gpa),s2base,level,napot):gpa;
                    for(int p=0;p<ports(g);p++)target_request(g,p,va,g==1?1:0,0,0);
                    drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);@(negedge clock_vif.clk);
                    for(int p=0;p<ports(g);p++)drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
                    drive({u,".io_ptw_resp_valid"},0);token_valid=0;returns_06++;refills++;
                    @(posedge clock_vif.clk);
                    for(int p=0;p<ports(g);p++)check_bypass06("page_matrix",g,p,va,pa,gpa);
                    @(negedge clock_vif.clk);cycles(3);windows_06++;
                end
            end
        end
    endtask

    // 抽象职责：两个同映射Miss分别返回，验证第二个返回与真实storage命中共存时选旁路。
    task priority_matrix06();
        string u;bit [49:0] va;bit [47:0] pa;va=50'h80000123;pa=48'h40000123;
        for(int g=0;g<3;g++)begin
            u=unit_name(g);configure_context(g,0);prepare06(g,va);
            older_token_06=1;token_valid=0;prepare06(g,va);
            mapping_fields06(g,va,pa,pa);drive({u,".io_ptw_resp_valid"},1);
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);
            older_token_06=0;returns_06++;refills++;cycles(8);
            // 第二个真实凭据继续有效；两次PTE输入逐字段相同，不无fence改变映射。
            mapping_fields06(g,va,pa,pa);
            for(int p=0;p<ports(g);p++)target_request(g,p,va,g==1?1:0,0,0);
            drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);@(negedge clock_vif.clk);
            for(int p=0;p<ports(g);p++)drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
            drive({u,".io_ptw_resp_valid"},0);token_valid=0;returns_06++;refills++;
            @(posedge clock_vif.clk);
            for(int p=0;p<ports(g);p++)check_bypass06("storage_and_ptw",g,p,va,pa,pa,1);
            @(negedge clock_vif.clk);cycles(3);windows_06++;
        end
    endtask

    // 抽象职责：经实际GPF回填建立GPA等待窗口，同时保留另一VPN的普通在途请求。
    // 三组顺序执行；沿用07局部凭据，仅PF0有完整ROB字段，PF1不驱动不存在的字段。
    task gpa_window06(input int g);
        string u;u=unit_name(g);
        if(pending_size(g)!=0)`uvm_fatal("DTLB06_TOKEN","GPA window has unfinished returns")
        configure_context(g,3);drive({u,".io_robPendingPtr_flag"},0);drive({u,".io_robPendingPtr_value"},8'h12);
        query07("ordinary_before_gpa",g,0,50'ha0000000,1,1,9'h012,0,0,0,0,0,-1,1);
        prepare06(g,50'h80000000);return_fields(g,50'h80000000,48'h40000000,6'h3f,0,5'h1f,1);
        drive({u,".io_ptw_resp_bits_getGpa"},0);
        for(int i=0;i<8;i++)drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",u,i),i==0);
        drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);@(negedge clock_vif.clk);
        drive({u,".io_ptw_resp_valid"},0);token_valid=0;returns_06++;refills++;cycles(8);
    endtask
    // 抽象职责：分别隔离注册等待和组合启动两个抑制原因，并用有效响应证明受抑仍能旁路。
    task gpa_suppression06();
        string u;logic [47:0] before_v;
        for(int g=0;g<3;g++)for(int phase_id=0;phase_id<2;phase_id++)begin
            u=unit_name(g);gpa_window06(g);before_v=valid06(g);
            if(phase_id==0)begin
                query07("start_gpa06",g,0,50'h80000000,1,1);
                payload07(g,0,0,0,0,48'ha0000000);drive({u,".io_ptw_resp_bits_s2_gpf"},0);
                for(int p=0;p<ports(g);p++)target_request(g,p,50'ha0000000,g==1?1:0,0,0);
                drive({u,".io_ptw_resp_valid"},1);#1ps;
                check06("PENDING_ONLY",{u,".need_gpa"},1,1);check06("PENDING_NO_START",{u,".need_gpa_wire"},1,0);
                check06("PENDING_SUPPRESS",{u,".entries.io_w_valid"},1,0);
                @(posedge clock_vif.clk);@(negedge clock_vif.clk);
                drive({u,".io_ptw_resp_valid"},0);retire07(g,0);
                for(int p=0;p<ports(g);p++)drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
                @(posedge clock_vif.clk);
                for(int p=0;p<ports(g);p++)check_bypass06("suppressed_refill",g,p,50'ha0000000,48'h40000000,48'ha0000000);
                @(negedge clock_vif.clk);
            end else begin
                target_request(g,0,50'h80000000,g==1?1:0,0,0);rob_fields07(g,0,9'h012);
                @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_requestor_0_req_valid"},0);
                payload07(g,0,0,0,0,48'ha0000000);drive({u,".io_ptw_resp_bits_s2_gpf"},0);drive({u,".io_ptw_resp_valid"},1);#1ps;
                check06("START_NO_PENDING",{u,".need_gpa"},1,0);check06("START_ONLY",{u,".need_gpa_wire"},1,1);
                check06("START_SUPPRESS",{u,".entries.io_w_valid"},1,0);
                @(posedge clock_vif.clk);capture07(g,0,50'h80000000,9'h012,1);
                @(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);retire07(g,0);
                check06("ENTERED_WAIT",{u,".need_gpa"},1,1);
            end
            if(valid06(g)!==before_v)`uvm_error("DTLB06_SUPPRESS","suppressed response changed valid way set")
            respond07(g,0,0,0,0,48'h80000000);
            query07("consume_gpa06",g,0,50'h80000000,0,0,9'h012,0,0,0,g!=2,64'h80000000,g==2?-1:0);
            check06("GPA_EXIT",{u,".need_gpa"},1,0);
            if(pending_size(g)!=0)`uvm_fatal("DTLB06_TOKEN","GPA responses not exhausted")
            drive({u,".reset"},1);cycles(2);windows_06++;
        end
    endtask
    // 抽象职责：从顶层SFENCE通路形成真实延迟flush，与尚未取消的局部普通返回重叠。
    task flush_suppression06();
        string u,topq;bit found;logic [47:0] before_v;
        topq="top_tb.u_memblock__fence_agent_if.io_ooo_to_mem_sfence";
        // defaults曾隔离模块输入；此场景先释放三组fence输入，恢复顶层的真实延迟链。
        for(int g=0;g<3;g++)begin
            u=unit_name(g);
            if(!uvm_hdl_release({u,".io_sfence_valid"}))`uvm_fatal("DTLB06_RELEASE",u)
        end
        drive({topq,"_bits_rs1"},1);drive({topq,"_bits_rs2"},1);drive({topq,"_bits_addr"},0);
        drive({topq,"_bits_id"},0);drive({topq,"_bits_hv"},0);drive({topq,"_bits_hg"},0);drive({topq,"_bits_flushPipe"},0);
        for(int g=0;g<3;g++)begin
            u=unit_name(g);configure_context(g,0);prepare06(g,50'h80000000);
            mapping_fields06(g,50'h80000000,48'h40000000,48'h40000000);before_v=valid06(g);
            drive({topq,"_valid"},1);found=0;
            for(int t=0;t<12;t++)begin
                @(negedge clock_vif.clk);#1ps;
                if(read_hdl({u,"._sfence_delay_io_out_valid"})===uvm_hdl_data_t'(1))begin found=1;break;end
            end
            if(!found)`uvm_fatal("DTLB06_FLUSH","top-level fence did not reach DTLB")
            drive({u,".io_ptw_resp_valid"},1);#1ps;check06("FLUSH_SUPPRESS",{u,".entries.io_w_valid"},1,0);
            check06("FLUSH_NO_GPA",{u,".need_gpa"},1,0);check06("FLUSH_NO_START",{u,".need_gpa_wire"},1,0);
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);drive({topq,"_valid"},0);
            token_valid=0;returns_06++;
            if((valid06(g)&~before_v)!==48'b0)`uvm_error("DTLB06_FLUSH","flush-suppressed return allocated a way")
            cycles(12);windows_06++;
        end
    endtask
    virtual task body();
        int expected_windows,expected_returns,expected_bypass;
        require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))`uvm_fatal("DTLB06_CLOCK","missing clock")
        if(seq_csr_common::get_l2tlb_seq_en()||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)`uvm_fatal("DTLB06_OWNER","local fixture conflicts with formal owner")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        `include "dtlb_0501_0504_input_defaults.svh"
        drive({DUT,".inner_dtlbRepeater.reset"},1);initialize_lookup_ports();
        case(scenario())
            "merge":begin merge_matrix06();expected_windows=75;expected_returns=75;expected_bypass=0;end
            "bypass":begin bypass_matrix06();expected_windows=336;expected_returns=336;expected_bypass=896;end
            "priority":begin priority_matrix06();expected_windows=3;expected_returns=6;expected_bypass=8;end
            "suppress":begin gpa_suppression06();flush_suppression06();expected_windows=9;expected_returns=9;expected_bypass=8;end
            default:`uvm_fatal("DTLB06_SCENARIO",scenario())
        endcase
        if(token_valid||older_token_06||pending_ld.size()!=0||pending_st.size()!=0||windows_06!=expected_windows||
           returns_06!=expected_returns||bypass_06!=expected_bypass||checks_06==0||initialization_checks!=8)
            `uvm_error("DTLB06_INCOMPLETE",$sformatf("scenario=%s windows=%0d/%0d returns=%0d/%0d bypass=%0d/%0d init=%0d",scenario(),windows_06,expected_windows,returns_06,expected_returns,bypass_06,expected_bypass,initialization_checks))
        cleanup();
        `uvm_info("DTLB06_COMPLETE",$sformatf("scenario=%s windows=%0d prepared=%0d returns=%0d bypass=%0d storage=%0d checks=%0d gpa_requests=%0d gpa_returns=%0d initialization=%0d",scenario(),windows_06,prepared_06,returns_06,bypass_06,storage_06,checks_06,requests_07,returns_07,initialization_checks),UVM_LOW)
    endtask
endclass
`define DTLB06_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_dtlb_0601_0605_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`" );super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB06_CLASS(memblock_dtlb_06_merge_vseq,"merge")
`DTLB06_CLASS(memblock_dtlb_06_bypass_vseq,"bypass")
`DTLB06_CLASS(memblock_dtlb_06_priority_vseq,"priority")
`DTLB06_CLASS(memblock_dtlb_06_suppress_vseq,"suppress")
`undef DTLB06_CLASS
`endif
