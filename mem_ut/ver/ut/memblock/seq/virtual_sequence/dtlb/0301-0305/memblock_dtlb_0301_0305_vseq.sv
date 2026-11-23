`ifndef MEMBLOCK_DTLB_0301_0305_VSEQ__SV
`define MEMBLOCK_DTLB_0301_0305_VSEQ__SV
// 抽象职责：经真实请求/匹配返回建立表项，验证查找上下文、页级、sector与地址。
// 不运行父场景，不改RTL输出、内部阵列、hit或覆盖模型；所有驱动均为声明input。
class memblock_dtlb_0301_0305_vseq extends memblock_dtlb_0201_0204_vseq;
    `uvm_object_utils(memblock_dtlb_0301_0305_vseq)
    // 当前局部上下文由configure更新，仅本例持有；每次请求保存匹配token。
    int active_stage[3], active_asid[3], active_vmid[3];
    bit token_valid;bit [37:0] token_vpn;int token_group,token_stage;
    // 目标请求、返回、完成子矩阵均精确累计，不能用日志数量替代物理仓计数。
    int unsigned probes,refills,matrices,initialization_checks;bit [47:0] ways_seen[3];
    function new(string name="memblock_dtlb_0301_0305_vseq");
        super.new(name);token_valid=0;probes=0;refills=0;matrices=0;initialization_checks=0;
        for(int g=0;g<3;g++)ways_seen[g]=0;
    endfunction
    virtual function string scenario();return "ways";endfunction
    function int ports(input int g);return g==0?4:2;endfunction
    function string array_path(input int g);
        case(g)
            0:return {unit_name(g),".entries.page_ldtlb_storage_fa"};
            1:return {unit_name(g),".entries.page_sttlb_storage_fa"};
            2:return {unit_name(g),".entries.page_pftlb_storage_fa"};
        endcase
        return "";
    endfunction
    function logic [47:0] hit_vector(input int g,input int p);
        logic [47:0] value;string signal_path;
        for(int w=0;w<48;w++)begin
            signal_path=p==0?$sformatf("%s.hitVecReg_%0d",array_path(g),w):
                $sformatf("%s.hitVecReg_%0d_%0d",array_path(g),p,w);
            value[w]=1'(read_hdl(signal_path));
        end
        return value;
    endfunction
    // 抽象职责：在真实模块输入设置稳定翻译上下文；reset_table决定是否清除阵列。
    task configure_context(input int g,input int stage=0,input int asid=1,input int vmid=1,input bit reset_table=1);
        string u;u=unit_name(g);
        if(token_valid) `uvm_fatal("DTLB0305_TOKEN","context change with a pending local return")
        if(reset_table)drive({u,".reset"},1);
        drive({u,".io_csr_satp_mode"},stage==0?9:0);
        drive({u,".io_csr_vsatp_mode"},stage==1||stage==3?9:0);
        drive({u,".io_csr_hgatp_mode"},stage==2||stage==3?9:0);
        drive({u,".io_csr_satp_asid"},asid);drive({u,".io_csr_vsatp_asid"},asid);
        drive({u,".io_csr_hgatp_vmid"},vmid);drive({u,".io_csr_priv_virt"},stage!=0);
        drive({u,".io_csr_priv_dmode"},1);drive({u,".io_csr_priv_imode"},1);
        drive({u,".io_csr_priv_sum"},1);drive({u,".io_csr_priv_vsum"},1);
        active_stage[g]=stage;active_asid[g]=asid;active_vmid[g]=vmid;
        cycles(6);if(reset_table)begin drive({u,".reset"},0);cycles(4);end
    endtask
    task request_fields(input int g,input int p,input bit [49:0] va,input bit prefault);
        string q;bit [63:0] full;
        q=$sformatf("%s.io_requestor_%0d_req",unit_name(g),p);
        full={{14{va[49]}},va};if(prefault)full=full^64'h8000000000000000;
        drive({q,"_bits_vaddr"},va);drive({q,"_bits_cmd"},g==1?1:0);
        if(!(g==2&&p==1))begin
            drive({q,"_bits_fullva"},full);drive({q,"_bits_checkfullva"},prefault);
            drive({q,"_bits_hyperinst"},0);
        end else if(prefault) `uvm_fatal("DTLB0305_CAP","PF1 lacks fullva precheck")
        drive({q,"_valid"},1);
    endtask
    // 抽象职责：从一次真实miss的PTW输出保存凭据；不合并key，不无请求造返回。
    task prepare_miss(input int g,input bit [49:0] va);
        string u;u=unit_name(g);
        if(token_valid)`uvm_fatal("DTLB0305_TOKEN","local return already pending")
        request_fields(g,0,va,1);@(posedge clock_vif.clk);
        @(negedge clock_vif.clk);drive({u,".io_requestor_0_req_valid"},0);
        @(posedge clock_vif.clk);
        if(read_hdl({u,".io_ptw_req_0_valid"})!==uvm_hdl_data_t'(1)||
           read_hdl({u,".io_ptw_req_0_bits_vpn"},38)!==uvm_hdl_data_t'(va[49:12])||
           read_hdl({u,".io_ptw_req_0_bits_s2xlate"},2)!==uvm_hdl_data_t'(active_stage[g]))
            `uvm_fatal("DTLB0305_TOKEN",$sformatf("g=%0d va=%0h missing actual PTW request",g,va))
        token_valid=1;token_vpn=va[49:12];token_group=g;token_stage=active_stage[g];
        @(negedge clock_vif.clk);cycles(2);
    endtask
    // 抽象职责：按唯一真实token写入合法的S1/S2返回；不指定内部替换way。
    task return_mapping(input int g,input bit [49:0] va,input bit [47:0] pa,
                        input int level=0,input bit [7:0] valididx=8'hff,
                        input bit [7:0] pteidx=8'hff,input bit global_mapping=0,
                        input bit shuffle_ppn=0,input bit coexist_probe=0);
        string u;int stage;
        u=unit_name(g);stage=active_stage[g];
        if(!token_valid||token_group!=g||token_vpn!==va[49:12]||token_stage!=stage)
            `uvm_fatal("DTLB0305_TOKEN","response has no matching VPN/stage token")
        drive({u,".io_ptw_resp_bits_s2xlate"},stage);
        drive({u,".io_ptw_resp_bits_s1_entry_tag"},va[49:15]);
        drive({u,".io_ptw_resp_bits_s1_addr_low"},va[14:12]);
        drive({u,".io_ptw_resp_bits_s1_entry_ppn"},pa[47:15]);
        drive({u,".io_ptw_resp_bits_s1_entry_level"},level);
        drive({u,".io_ptw_resp_bits_s1_entry_asid"},active_asid[g]);
        drive({u,".io_ptw_resp_bits_s1_entry_vmid"},active_vmid[g]);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_g"},global_mapping);
        drive({u,".io_ptw_resp_bits_s1_entry_v"},1);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_d"},1);drive({u,".io_ptw_resp_bits_s1_entry_perm_a"},1);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_u"},1);drive({u,".io_ptw_resp_bits_s1_entry_perm_x"},1);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_w"},1);drive({u,".io_ptw_resp_bits_s1_entry_perm_r"},1);
        drive({u,".io_ptw_resp_bits_s2_entry_tag"},va[49:12]);
        drive({u,".io_ptw_resp_bits_s2_entry_ppn"},pa[47:12]);
        drive({u,".io_ptw_resp_bits_s2_entry_level"},level);
        drive({u,".io_ptw_resp_bits_s2_entry_vmid"},active_vmid[g]);
        drive({u,".io_ptw_resp_bits_s2_entry_perm_d"},1);drive({u,".io_ptw_resp_bits_s2_entry_perm_a"},1);
        drive({u,".io_ptw_resp_bits_s2_entry_perm_u"},1);drive({u,".io_ptw_resp_bits_s2_entry_perm_x"},1);
        drive({u,".io_ptw_resp_bits_s2_entry_perm_w"},1);drive({u,".io_ptw_resp_bits_s2_entry_perm_r"},1);
        for(int i=0;i<8;i++)begin
            drive($sformatf("%s.io_ptw_resp_bits_s1_valididx_%0d",u,i),valididx[i]);
            drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",u,i),pteidx[i]);
            drive($sformatf("%s.io_ptw_resp_bits_s1_ppn_low_%0d",u,i),shuffle_ppn?((i+3)%8):i);
        end
        if(coexist_probe)begin
            request_fields(g,1,{va[49:15],15'b0},0);
            cycles(2);
        end
        drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);
        @(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);
        token_valid=0;refills++;
        if(coexist_probe) begin
            // 已存在子页查询与真实返回写入相邻窗口，额外端口不驱动输出或内部状态。
            request_fields(g,1,{va[49:15],15'b0},0);
            repeat(8) begin
                @(posedge clock_vif.clk);
                if(read_hdl($sformatf("%s.io_requestor_1_resp_valid",u))===uvm_hdl_data_t'(1)) begin
                    if(read_hdl($sformatf("%s.io_requestor_1_resp_bits_miss",u))!==uvm_hdl_data_t'(0) ||
                       read_hdl($sformatf("%s.io_requestor_1_resp_bits_paddr_0",u),48)!==uvm_hdl_data_t'(pa))
                        `uvm_error("DTLB0305_COEXIST","coincident existing subpage query failed")
                end
            end
            @(negedge clock_vif.clk);drive($sformatf("%s.io_requestor_1_req_valid",u),0);cycles(2);
        end else cycles(8);
    endtask
    task insert_mapping(input int g,input bit [49:0] va,input bit [47:0] pa,
                        input int level=0,input bit [7:0] valididx=8'hff,
                        input bit [7:0] pteidx=8'hff,input bit global_mapping=0,
                        input bit shuffle_ppn=0);
        prepare_miss(g,va);return_mapping(g,va,pa,level,valididx,pteidx,global_mapping,shuffle_ppn);
    endtask
    // 抽象职责：对一个或多个真实端口查表，比较hit及响应地址；中拍CSR只驱动storage声明input。
    task probe(input string ident,input int g,input int p,input bit [49:0] va,
               input bit [47:0] expected_pa,input int hit_count=1,input bit prefault=0,
               input int simultaneous_ports=1,input bit mid_asid=0);
        string u,rsp;logic [47:0] hits;logic [5:0] exception;logic [5:0] exp_excp;
        bit expected_miss;
        u=unit_name(g);expected_miss=hit_count==0&&!prefault;
        for(int j=0;j<simultaneous_ports;j++)request_fields(g,p+j,va,prefault);
        @(posedge clock_vif.clk);
        @(negedge clock_vif.clk);
        for(int j=0;j<simultaneous_ports;j++)drive($sformatf("%s.io_requestor_%0d_req_valid",u,p+j),0);
        if(mid_asid)drive({array_path(g),".io_csr_satp_asid"},active_asid[g]+1);
        @(posedge clock_vif.clk);
        for(int j=0;j<simultaneous_ports;j++)begin
            rsp=$sformatf("%s.io_requestor_%0d_resp",u,p+j);hits=hit_vector(g,p+j);
            if($isunknown(hits)||$countones(hits)!=hit_count)
                `uvm_error("DTLB0305_HIT",$sformatf("%s g=%0d p=%0d expected=%0d hits=%0h",ident,g,p+j,hit_count,hits))
            if(read_hdl({rsp,"_valid"})!==uvm_hdl_data_t'(1)||
               read_hdl({indexed(u,"req_out",p+j),"_vaddr"},50)!==uvm_hdl_data_t'(va)||
               read_hdl({indexed(u,"req_out",p+j),"_cmd"},3)!==uvm_hdl_data_t'(g==1?1:0)||
               read_hdl({rsp,"_bits_miss"})!==uvm_hdl_data_t'(expected_miss))
                `uvm_error("DTLB0305_RESPONSE",ident)
            exception=actual_exception(rsp);exp_excp=prefault?(active_stage[g]==2?(g==1?6'h08:6'h04):(g==1?6'h02:6'h01)):0;
            if(exception!==exp_excp)`uvm_error("DTLB0305_EXCEPTION",$sformatf("%s exp=%0h got=%0h",ident,exp_excp,exception))
            if(hit_count>0&&!prefault&&read_hdl({rsp,"_bits_paddr_0"},48)!==uvm_hdl_data_t'(expected_pa))
                `uvm_error("DTLB0305_PA",$sformatf("%s g=%0d p=%0d va=%0h expected=%0h actual=%0h",ident,g,p+j,va,expected_pa,read_hdl({rsp,"_bits_paddr_0"},48)))
            ways_seen[g]|=hits;probes++;
            `uvm_info("DTLB0305_SAMPLE",$sformatf("%s g=%0d p=%0d va=%0h hit=%0h pa=%0h checks=%0d",ident,g,p+j,va,hits,read_hdl({rsp,"_bits_paddr_0"},48),probes),UVM_LOW)
        end
        @(negedge clock_vif.clk);
        if(mid_asid && !uvm_hdl_release({array_path(g),".io_csr_satp_asid"}))
            `uvm_error("DTLB0305_RELEASE","cannot release temporary storage ASID input")
        cycles(2);
    endtask
    // 抽象职责：以真实Bare查询初始化所有物理端口的读结果寄存器，避免未查询端口历史X污染聚合OR。
    // 不写内部寄存器；逐口验证真实响应、地址、miss与空表hit，再进入正式翻译矩阵。
    task initialize_lookup_ports();
        string u;string rsp;
        for(int g=0;g<3;g++)begin
            u=unit_name(g);drive({u,".reset"},1);
            drive({u,".io_csr_satp_mode"},0);drive({u,".io_csr_vsatp_mode"},0);drive({u,".io_csr_hgatp_mode"},0);
            drive({u,".io_csr_priv_dmode"},3);cycles(6);drive({u,".reset"},0);cycles(4);
            for(int p=0;p<ports(g);p++)request_fields(g,p,0,0);
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);
            for(int p=0;p<ports(g);p++)drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
            @(posedge clock_vif.clk);
            for(int p=0;p<ports(g);p++)begin
                rsp=$sformatf("%s.io_requestor_%0d_resp",u,p);
                if(hit_vector(g,p)!==48'b0 || read_hdl({rsp,"_valid"})!==uvm_hdl_data_t'(1) ||
                   read_hdl({rsp,"_bits_miss"})!==uvm_hdl_data_t'(0)||
                   read_hdl({rsp,"_bits_paddr_0"},48)!==uvm_hdl_data_t'(0)||actual_exception(rsp)!==6'b0)
                    `uvm_error("DTLB0305_INIT","Bare real-query initialization failed")
                initialization_checks++;
            end
            @(negedge clock_vif.clk);cycles(2);
        end
    endtask

    task ways_matrix();
        bit [49:0] va;
        for(int g=0;g<3;g++)begin
            configure_context(g);probe("cold",g,0,50'h100000,0,0);
            for(int n=0;n<128;n++)begin
                va=50'h100000+50'(n)*50'h8000;
                insert_mapping(g,va,48'h40000000);probe("fill_way",g,0,va,48'h40000000);
            end
            if(ways_seen[g]!==48'hffffffffffff)`uvm_error("DTLB0305_WAYS","did not visit all48 actual ways")
            for(int p=0;p<ports(g);p++)begin
                probe("alias_a",g,p,50'h4f0000,48'h40000000);
                probe("alias_b",g,p,50'h4f8000,48'h40000000);
            end
            probe("tag_low_miss",g,0,50'h9000000,0,0);
            probe("tag_mid_miss",g,0,50'h100100000,0,0);
            probe("tag_high_miss",g,0,50'h10000100000,0,0);matrices++;
        end
    endtask
    task context_matrix();
        bit [49:0] va;va=50'h80000000;
        for(int g=0;g<3;g++)begin
            configure_context(g,0,1,1);insert_mapping(g,va,48'h40000000);
            probe("asid_hit",g,0,va,48'h40000000);
            configure_context(g,0,2,1,0);probe("asid_reject",g,0,va,0,0,1);
            insert_mapping(g,va,48'h50000000);probe("asid2_mapping",g,0,va,48'h50000000);
            configure_context(g,0,1,1,0);probe("asid1_survives",g,0,va,48'h40000000,1,0,1,1);
            configure_context(g,0,1,1);insert_mapping(g,va,48'h40000000,0,8'hff,8'hff,1);
            configure_context(g,0,2,2,0);probe("global_cross_asid_host_ignore_vmid",g,0,va,48'h40000000);
            `uvm_info("DTLB0305_GLOBAL_TRACE",$sformatf("g=%0d asid=%0h entry_asid0=%0h entry_g0=%0h",g,read_hdl({array_path(g),".io_csr_satp_asid"},16),read_hdl({array_path(g),".entries_0_asid"},16),read_hdl({array_path(g),".entries_0_perm_g"})),UVM_LOW)
            configure_context(g,1,1,1);insert_mapping(g,va,48'h40000000);
            probe("guest_vmid_hit",g,0,va,48'h40000000);
            configure_context(g,1,1,2,0);probe("guest_vmid_reject",g,0,va,0,0,1);
            configure_context(g,0,1,1,0);probe("stage_reject",g,0,va,0,0,1);
            configure_context(g,2,1,1);insert_mapping(g,va,48'h40000000,0,8'h01,8'h01);
            configure_context(g,2,2,1,0);probe("onlyS2_ignore_asid",g,0,va,48'h40000000);matrices++;
        end
    endtask
    task pages_matrix();
        bit [49:0] base,va,offset;bit [47:0] pa;
        for(int g=0;g<3;g++)begin
            for(int level=0;level<4;level++)begin
                configure_context(g);base=50'h10000000000;pa=48'h20000000000;
                insert_mapping(g,base,pa,level);
                for(int k=0;k<3;k++)begin
                    offset=k==0?0:k==1?50'h123:(50'h1<<(12+9*level))-1;
                    va=base+offset;
                    for(int p=0;p<ports(g);p++)probe("page_offset",g,p,va,pa+48'(offset));
                end
                matrices++;
            end
        end
    endtask
    task sector_matrix();
        bit [49:0] base;bit [47:0] pa;base=50'h80000000;pa=48'h40000000;
        for(int g=0;g<3;g++)begin
            configure_context(g);insert_mapping(g,base,pa,0,8'h2d,8'h2d,0,1);
            for(int sub=0;sub<8;sub++)begin
                if((8'h2d>>sub)&1)probe("partial_sector_noncontiguous",g,0,base+50'(sub)*4096,pa+48'((sub+3)%8)*4096);
                else probe("invalid_subpage",g,0,base+50'(sub)*4096,0,0,1);
            end
            configure_context(g);insert_mapping(g,base,pa,0,8'h01,8'h01);
            prepare_miss(g,base+4096);return_mapping(g,base+4096,pa,0,8'h02,8'h02,0,0,1);
            probe("disjoint_sector_a",g,0,base,pa);probe("disjoint_sector_b",g,0,base+4096,pa+4096);
            configure_context(g);insert_mapping(g,base,pa);
            for(int sub=0;sub<8;sub++)probe("all_eight_sectors",g,0,base+50'(sub)*4096,pa+48'(sub)*4096);
            configure_context(g,2);insert_mapping(g,base+5*4096,pa+5*4096,0,8'hff,8'hff);
            probe("onlyS2_index",g,0,base+5*4096,pa+5*4096);matrices++;
        end
    endtask
    task pte_matrix();
        bit [49:0] base;bit [47:0] pa;base=50'h80000000;pa=48'h40000000;
        for(int g=0;g<3;g++)begin
            configure_context(g,3);insert_mapping(g,base,pa,0,8'h03,8'h01);
            probe("pte_match",g,0,base,pa);probe("pte_reject",g,0,base+4096,0,0,1);
            configure_context(g,1);insert_mapping(g,base,pa,0,8'h03,8'h01);
            probe("onlyS1_ignore_pte",g,0,base+4096,pa+4096);
            configure_context(g,3);insert_mapping(g,base,pa,1,8'h01,8'h01);
            probe("large_ignore_pte",g,0,base+4096,pa+4096);
            configure_context(g);insert_mapping(g,base,pa,0,8'h01,8'h01);
            insert_mapping(g,base+4096,pa,0,8'h03,8'h02);
            probe("expanded_two",g,0,base,pa,2);
            insert_mapping(g,base+8192,pa,0,8'h07,8'h04);
            probe("expanded_three",g,0,base,pa,3);matrices++;
        end
    endtask
    virtual task body();
        require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))
            `uvm_fatal("DTLB0305_CLOCK","missing real clock")
        if(seq_csr_common::get_l2tlb_seq_en()||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)
            `uvm_fatal("DTLB0305_OWNER","local fixture cannot share active L2TLB owner")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        `include "dtlb_0301_0305_input_defaults.svh"
        for(int p=0;p<8;p++)drive($sformatf("%s.inner_dtlbRepeater.io_tlb_req_%0d_valid",DUT,p),0);
        initialize_lookup_ports();
        case(scenario())
            "ways":ways_matrix();"context":context_matrix();"pages":pages_matrix();
            "sector":sector_matrix();"pte":pte_matrix();
            default:`uvm_fatal("DTLB0305_SCENARIO",scenario())
        endcase
        if(token_valid||probes==0||initialization_checks!=8||matrices!=(scenario()=="pages"?12:3))
            `uvm_error("DTLB0305_INCOMPLETE","matrix or local-token completion mismatch")
        cleanup();
        `uvm_info("DTLB0305_COMPLETE",$sformatf("scenario=%s requests=%0d returns=%0d matrices=%0d initialization=%0d",scenario(),probes,refills,matrices,initialization_checks),UVM_LOW)
    endtask
endclass
`define DTLB0305TEST_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_dtlb_0301_0305_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`");super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB0305TEST_CLASS(memblock_dtlb_03_ways_vseq,"ways")
`DTLB0305TEST_CLASS(memblock_dtlb_03_context_vseq,"context")
`DTLB0305TEST_CLASS(memblock_dtlb_03_pages_vseq,"pages")
`DTLB0305TEST_CLASS(memblock_dtlb_03_sector_vseq,"sector")
`DTLB0305TEST_CLASS(memblock_dtlb_03_pte_vseq,"pte")
`undef DTLB0305TEST_CLASS
`endif
