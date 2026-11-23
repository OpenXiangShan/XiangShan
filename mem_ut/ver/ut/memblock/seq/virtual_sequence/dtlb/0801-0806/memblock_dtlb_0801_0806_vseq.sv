`ifndef MEMBLOCK_DTLB_0801_0806_VSEQ__SV
`define MEMBLOCK_DTLB_0801_0806_VSEQ__SV
// 抽象职责：通过真实miss凭据建立映射，独立保存输入PTE身份，逐way核对刷新与恢复。
// 本地fixture只驱动DTLB声明输入，Repeater保持reset；正式owner场景在另一个类中运行。
class memblock_dtlb_0801_0806_vseq extends memblock_dtlb_0401_0402_vseq;
    `uvm_object_utils(memblock_dtlb_0801_0806_vseq)
    // 表项身份由每次真实回填前后valid差分定位；复位窗口清空，fence后只更新有效标志。
    // 固定48项，仅保存本窗口原始输入信息，不读取覆盖模型期望或内部元数据作为参考。
    bit saved_valid[48],saved_global[48],saved_n[48];
    int saved_stage[48],saved_asid[48],saved_vmid[48],saved_level[48];
    bit [49:0] saved_va[48];bit [47:0] saved_pa[48];bit [7:0] saved_mask[48];
    // 完成窗口、逐way比较、真实有向边由本序列累计，结束时与固定场景合同比较。
    int unsigned fence_windows,way_checks,stage_edges,recovery_windows;
    function new(string name="memblock_dtlb_0801_0806_vseq");super.new(name);fence_windows=0;way_checks=0;stage_edges=0;recovery_windows=0;endfunction
    virtual function string scenario();return "scope";endfunction
    function logic [47:0] physical_v(input int g);
        logic [47:0] result;for(int w=0;w<48;w++)result[w]=1'(read_hdl($sformatf("%s.v_%0d",array_path(g),w)));return result;
    endfunction
    task begin_window(input int g,input int stage=0);
        configure_context(g,stage,1,1);for(int w=0;w<48;w++)saved_valid[w]=0;
    endtask
    // 抽象职责：消费唯一实际PTW请求，设置合法页型/权限；不指定替换way。
    task fill_saved(input int g,input bit [49:0] va,input bit [47:0] pa,
                    input int level=0,input bit [7:0] mask=8'hff,input bit global_mapping=0,
                    input bit napot=0,input bit fault=0,input int s2level=-1);
        string u;logic [47:0] before_v,after_v,added;int w;bit [47:0] s1pa;
        u=unit_name(g);prepare_miss(g,va);return_fields(g,va,pa,6'h3f,fault,5'h1f,0);
        s1pa=active_stage[g]==3?va[47:0]:pa;
        drive({u,".io_ptw_resp_bits_s1_entry_level"},level);
        drive({u,".io_ptw_resp_bits_s2_entry_level"},s2level<0?level:s2level);
        drive({u,".io_ptw_resp_bits_s1_entry_perm_g"},global_mapping);
        drive({u,".io_ptw_resp_bits_s1_entry_n"},napot&&active_stage[g]!=2);
        drive({u,".io_ptw_resp_bits_s2_entry_n"},napot&&active_stage[g]>=2);
        if(napot)begin
            drive({u,".io_ptw_resp_bits_s1_entry_ppn"},41'({8'b0,s1pa[47:15]})|41'b1);
            drive({u,".io_ptw_resp_bits_s2_entry_ppn"},38'(pa[47:12])|38'h8);
        end
        for(int i=0;i<8;i++)begin
            drive($sformatf("%s.io_ptw_resp_bits_s1_valididx_%0d",u,i),mask[i]);
            drive($sformatf("%s.io_ptw_resp_bits_s1_pteidx_%0d",u,i),mask[i]);
            if(napot)drive($sformatf("%s.io_ptw_resp_bits_s1_ppn_low_%0d",u,i),0);
        end
        before_v=physical_v(g);drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);
        @(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);token_valid=0;refills++;cycles(8);
        after_v=physical_v(g);added=after_v&~before_v;w=-1;
        if($isunknown(added)||$countones(added)!=1)`uvm_fatal("DTLB0806_FILL",$sformatf("g=%0d before=%h after=%h",g,before_v,after_v))
        for(int i=0;i<48;i++)if(added[i])w=i;
        saved_valid[w]=1;saved_va[w]=va;saved_pa[w]=pa;saved_mask[w]=mask;saved_global[w]=global_mapping;
        saved_stage[w]=active_stage[g];saved_asid[w]=active_asid[g];saved_vmid[w]=active_vmid[g];
        saved_n[w]=napot;saved_level[w]=fault?level:(s2level<0?level:(level<s2level?level:s2level));
        if(read_hdl($sformatf("%s.entries_%0d_level",array_path(g),w),2)!==uvm_hdl_data_t'(saved_level[w]))`uvm_error("DTLB0806_LEVEL","saved page size differs from input mapping")
        `uvm_info("DTLB0806_FILL",$sformatf("g=%0d way=%0d va=%h pa=%h stage=%0d asid=%0d vmid=%0d level=%0d n=%0b global=%0b mask=%h fault=%0b",g,w,va,pa,saved_stage[w],saved_asid[w],saved_vmid[w],saved_level[w],napot,global_mapping,mask,fault),UVM_LOW)
    endtask
    // 抽象职责：以输入映射的页范围/ID独立推导失效集合；遵循当前V2接口范围合同。
    function bit selected(input int w,input bit rs1,input bit rs2,input bit [49:0] addr,input int id,
                          input bit hv,input bit hg,input bit virt,input int vmid,input bit bme,input bit cmode);
        bit address_match;int size_bits;
        if(hg)return saved_stage[w]!=0&&(rs2||saved_vmid[w]==id);
        if(hv||virt)return saved_stage[w]!=0&&saved_vmid[w]==vmid&&(rs2||(saved_asid[w]==id&&!saved_global[w]));
        if(rs1||(bme&&!cmode))return saved_stage[w]==0&&(rs2||(saved_asid[w]==id&&!saved_global[w]));
        size_bits=saved_n[w]?16:(saved_level[w]==0?15:12+9*saved_level[w]);
        address_match=(addr>>size_bits)==(saved_va[w]>>size_bits);
        if(saved_level[w]==0&&!saved_n[w])address_match&=saved_mask[w][addr[14:12]];
        return address_match&&(rs2||(saved_asid[w]==id&&!saved_global[w]));
    endfunction
    // 抽象职责：发出真实fence，等待storage实际生效沿，逐个旧有效way比较清除/保留。
    // 在同拍查表窗口保持真实请求，单独验证刷新后复访；不要求清除前锁存hit立即归零。
    task checked_fence(input string ident,input int g,input bit rs1,input bit rs2,input bit [49:0] addr,
                        input int id=1,input bit hv=0,input bit hg=0,input bit bme=0,input bit cmode=0,input bit lookup=0);
        string u,a;logic [47:0] before_v,expected,actual;bit found;int selected_count,retained_count;
        u=unit_name(g);a=array_path(g);before_v=physical_v(g);expected=before_v;selected_count=0;retained_count=0;
        for(int w=0;w<48;w++)if(before_v[w])begin
            if(!saved_valid[w])`uvm_fatal("DTLB0806_IDENTITY","valid way has no original input mapping")
            if(selected(w,rs1,rs2,addr,id,hv,hg,active_stage[g]!=0,active_vmid[g],bme,cmode))begin expected[w]=0;selected_count++;end else retained_count++;
        end
        if(before_v==0)`uvm_fatal("DTLB0806_EMPTY","fence must have a real populated table")
        drive({u,".io_csr_mbmc_BME"},bme);drive({u,".io_csr_mbmc_CMODE"},cmode);cycles(6);
        drive({u,".io_sfence_bits_rs1"},rs1);drive({u,".io_sfence_bits_rs2"},rs2);drive({u,".io_sfence_bits_addr"},addr);
        drive({u,".io_sfence_bits_id"},id);drive({u,".io_sfence_bits_hv"},hv);drive({u,".io_sfence_bits_hg"},hg);
        drive({u,".io_sfence_valid"},1);@(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_sfence_valid"},0);found=0;
        for(int t=0;t<16;t++)begin
            #1ps;if(read_hdl({a,".io_sfence_valid"})===uvm_hdl_data_t'(1))begin found=1;break;end
            @(negedge clock_vif.clk);
        end
        if(!found)`uvm_fatal("DTLB0806_FENCE","storage fence edge missing")
        if(lookup)request_fields(g,0,addr,0);
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);
        if(lookup)drive({u,".io_requestor_0_req_valid"},0);
        actual=physical_v(g);
        for(int w=0;w<48;w++)begin
            if(actual[w]!==expected[w])`uvm_error("DTLB0806_SCOPE",$sformatf("%s g=%0d way=%0d expected=%b actual=%b",ident,g,w,expected[w],actual[w]))
            if(before_v[w])way_checks++;saved_valid[w]=expected[w];
        end
        fence_windows++;`uvm_info("DTLB0806_FENCE",$sformatf("%s g=%0d before=%h expected=%h actual=%h selected=%0d retained=%0d",ident,g,before_v,expected,actual,selected_count,retained_count),UVM_LOW)
        cycles(8);drive({u,".io_csr_mbmc_BME"},0);drive({u,".io_csr_mbmc_CMODE"},0);cycles(6);
    endtask
    task scope_matrix();
        bit [49:0] va;va=50'h80000000;
        for(int g=0;g<3;g++)for(int variant=0;variant<14;variant++)begin
            bit rs1,rs2,hv,hg;int context_stage;bit bme,cmode;logic [47:0] window_v;
            begin_window(g);
            for(int n=0;n<10;n++)begin
                int st,asid,vmid;bit glob;bit [49:0] itemva;
                st=n<4?0:n<6?1:n<8?2:3;asid=n%2+1;vmid=n==5||n==7||n==9?2:1;glob=n==2||n==5||n==8;
                itemva=va+(n==3||n==7?50'h400000:0);
                // Host相同ASID下全局与非全局使用不同VA，避免测试自身制造同context重叠项。
                if(n==2)itemva=va+50'h8000;
                configure_context(g,st,asid,vmid,0);fill_saved(g,itemva,48'h40000000+48'(n)*48'h8000,0,8'hff,glob);
            end
            context_stage=variant==4||variant==5?1:0;configure_context(g,context_stage,1,1,0);
            rs1=variant<4?variant<2:0;rs2=variant<4?(variant%2==0):(variant%2==1);hv=variant==6||variant==7;hg=variant==8||variant==9;
            bme=variant==10||variant==11;cmode=variant==11;
            window_v=physical_v(g);
            checked_fence($sformatf("scope_%0d",variant),g,rs1,rs2,variant>=10&&variant<12?va+50'h800000:va,1,hv,hg,bme,cmode,1);
            // 对每种实际context复访；失效项用预异常查询限制后续无主miss，保留项独立验证原PA。
            for(int w=0;w<48;w++)if(window_v[w])begin
                configure_context(g,saved_stage[w],saved_asid[w],saved_vmid[w],0);
                probe(saved_valid[w]?"retained_mapping":"invalidated_mapping",g,0,saved_va[w],saved_pa[w],saved_valid[w]?1:0,!saved_valid[w]);
            end
            matrices++;
        end
    endtask
    task pages_matrix_08();
        bit [49:0] va;va=50'h10000000000;
        for(int g=0;g<3;g++)begin
            begin_window(g);fill_saved(g,va,48'h20000000000,0,8'h01);
            checked_fence("invalid_sector_retained",g,0,1,va+4096);probe("sector_retained",g,0,va,48'h20000000000);
            checked_fence("valid_sector_entire_entry",g,0,1,va);probe("sector_invalidated",g,0,va,0,0,1);
            for(int level=1;level<4;level++)for(int k=0;k<3;k++)begin
                bit [49:0] offset;begin_window(g);fill_saved(g,va,48'h20000000000,level);
                offset=k==0?50'h9000:k==1?((50'h1<<(12+9*level))-1):(50'h1<<(12+9*level));
                checked_fence("large_page_range",g,0,1,va+offset);
                probe("large_page_revisit",g,0,va,48'h20000000000,k==2?1:0,k!=2);matrices++;
            end
            for(int k=0;k<3;k++)begin
                begin_window(g);fill_saved(g,va,48'h20000000000,0,8'hff,0,1);
                checked_fence("napot64k_range",g,0,1,va+(k==0?50'h9000:k==1?50'hffff:50'h10000));
                probe("napot_revisit",g,0,va,48'h20000000000,k==2?1:0,k!=2);matrices++;
            end
            begin_window(g,3);fill_saved(g,va,48'h20000000000,2,8'hff,0,0,1,0);
            configure_context(g,0,1,1,0);checked_fence("s1_fault_large_actual_size",g,0,1,va+50'h10000000);matrices++;
        end
    endtask
    // 抽象职责：复位不插入两个上下文之间，验证四条漏边以及新context独立回填地址。
    task stage_matrix_08();
        int olds[4],news[4];bit [49:0] va;va=50'h80000000;olds='{3,0,1,2};news='{2,3,3,1};
        for(int g=0;g<3;g++)for(int e=0;e<4;e++)begin
            begin_window(g,olds[e]);fill_saved(g,va,48'h40000000);probe("old_stage_mapping",g,0,va,48'h40000000);
            configure_context(g,news[e],1,1,0);
            if(read_hdl({unit_name(g),".reset"})!==uvm_hdl_data_t'(0))`uvm_error("DTLB0806_EDGE","reset inserted into direct edge")
            probe("new_stage_reject_old",g,0,va,0,0,1);fill_saved(g,va,48'h50000000);probe("new_stage_mapping",g,0,va,48'h50000000);
            stage_edges++;`uvm_info("DTLB0806_EDGE",$sformatf("g=%0d old=%0d new=%0d reset=0 old_pa=40000000 new_pa=50000000",g,olds[e],news[e]),UVM_LOW)
        end
    endtask
    // 抽象职责：两个不同地址/ID的fence以0/1/2/4空拍间隔进入真实延迟链，逐个payload核验清除集合。
    task continuous_fence_pair(input int g,input int gap);
        string u,a;bit [49:0] va;logic [47:0] expected;int seen;
        u=unit_name(g);a=array_path(g);va=50'h90000000;seen=0;
        begin_window(g);fill_saved(g,va,48'h40000000);
        configure_context(g,0,2,1,0);fill_saved(g,va+50'h100000,48'h50000000);
        configure_context(g,1,1,2,0);fill_saved(g,va+50'h200000,48'h60000000);
        configure_context(g,0,1,1,0);expected=physical_v(g);
        drive({u,".io_sfence_bits_rs1"},0);drive({u,".io_sfence_bits_rs2"},0);drive({u,".io_sfence_bits_hv"},0);drive({u,".io_sfence_bits_hg"},0);
        for(int tick=0;tick<24;tick++)begin
            bit event_now;
            if(tick==0)begin drive({u,".io_sfence_bits_addr"},va);drive({u,".io_sfence_bits_id"},1);drive({u,".io_sfence_valid"},1);end
            if(tick==1)drive({u,".io_sfence_valid"},0);
            if(tick==gap+1)begin drive({u,".io_sfence_bits_addr"},va+50'h100000);drive({u,".io_sfence_bits_id"},2);drive({u,".io_sfence_valid"},1);end
            if(tick==gap+2)drive({u,".io_sfence_valid"},0);
            #1ps;event_now=read_hdl({a,".io_sfence_valid"})===uvm_hdl_data_t'(1);
            if(event_now)begin
                bit [49:0] wanted;int wanted_id;wanted=seen==0?va:va+50'h100000;wanted_id=seen+1;
                if(seen>=2||read_hdl({a,".io_sfence_bits_addr"},50)!==uvm_hdl_data_t'(wanted)||read_hdl({a,".io_sfence_bits_id"},16)!==uvm_hdl_data_t'(wanted_id))`uvm_error("DTLB0806_PAYLOAD","delayed fence payload/order mismatch")
                for(int w=0;w<48;w++)if(expected[w]&&selected(w,0,0,wanted,wanted_id,0,0,0,1,0,0))expected[w]=0;
                seen++;
            end
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);
            if(event_now&&physical_v(g)!==expected)`uvm_error("DTLB0806_PAIR","actual fence result differs from original mapping selection")
        end
        if(seen!=2||$countones(expected)!=1)`uvm_error("DTLB0806_PAIR","two distinct targets and retained guest not proved")
        `uvm_info("DTLB0806_PAIR",$sformatf("g=%0d gap=%0d events=%0d retained=%h",g,gap,seen,expected),UVM_LOW)
    endtask
    // 抽象职责：真实返回与内部flush竞争，验证写入抑制；随后同VA故障/映射均可恢复。
    task recovery_matrix_08();
        bit [49:0] va;va=50'h80000000;
        for(int g=0;g<3;g++)begin
            string u;bit found;u=unit_name(g);begin_window(g);fill_saved(g,va,48'h40000000);
            // CSR changed建立真实模型epoch；不把SFENCE冒充changed，不复位模型历史。
            drive({u,".io_csr_satp_changed"},1);cycles(1);drive({u,".io_csr_satp_changed"},0);cycles(8);
            for(int p=0;p<ports(g);p++)probe("epoch_old_mapping",g,p,va,48'h40000000);
            checked_fence("mapping_change",g,1,1,va);
            prepare_miss(g,va);return_fields(g,va,48'h50000000,6'h3f,0,5'h1f,0);
            target_request(g,0,va,g==1?1:0,0,0);drive({u,".io_ptw_resp_valid"},1);
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);drive({u,".io_requestor_0_req_valid"},0);token_valid=0;refills++;
            @(posedge clock_vif.clk);check_response_permission("mapping_new_bypass",g,0,va,48'h50000000,g==1?1:0,0,1);@(negedge clock_vif.clk);cycles(8);
            for(int p=0;p<ports(g);p++)probe("mapping_new_storage",g,p,va,48'h50000000);
            // 缓存PF后合法fence修复，返回与相同VA请求同拍，避免中间miss吞掉last_fault证据。
            begin_window(g);fill_saved(g,va,48'h40000000,0,8'hff,0,0,1);
            for(int p=0;p<ports(g);p++)begin
                target_request(g,p,va,g==1?1:0,0,0);@(posedge clock_vif.clk);@(negedge clock_vif.clk);drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
                @(posedge clock_vif.clk);check_response_permission("cached_pf",g,p,va,48'h40000000,g==1?1:0,1,0);@(negedge clock_vif.clk);cycles(2);
            end
            drive({u,".io_csr_satp_changed"},1);cycles(1);drive({u,".io_csr_satp_changed"},0);cycles(8);checked_fence("fault_repair",g,1,1,va);
            prepare_miss(g,va);return_fields(g,va,48'h60000000,6'h3f,0,5'h1f,0);
            for(int p=0;p<ports(g);p++)target_request(g,p,va,g==1?1:0,0,0);
            drive({u,".io_ptw_resp_valid"},1);@(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);token_valid=0;refills++;
            for(int p=0;p<ports(g);p++)drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);
            @(posedge clock_vif.clk);for(int p=0;p<ports(g);p++)check_response_permission("fault_repaired_bypass",g,p,va,48'h60000000,g==1?1:0,0,1);
            @(negedge clock_vif.clk);cycles(8);for(int p=0;p<ports(g);p++)probe("fault_repaired_storage",g,p,va,48'h60000000);
            // Host忽略VMID，连续请求中切换实际CSR ID，响应应保持相同独立PA。
            for(int p=0;p<ports(g);p++)target_request(g,p,va,g==1?1:0,0,0);
            for(int t=0;t<12;t++)begin
                if(t==2)drive({u,".io_csr_hgatp_vmid"},2);
                @(posedge clock_vif.clk);if(t>0)for(int p=0;p<ports(g);p++)check_response_permission("live_context_id",g,p,va,48'h60000000,g==1?1:0,0,0);@(negedge clock_vif.clk);
            end
            for(int p=0;p<ports(g);p++)drive($sformatf("%s.io_requestor_%0d_req_valid",u,p),0);cycles(4);
            begin_window(g);prepare_miss(g,va);return_fields(g,va,48'h70000000,6'h3f,0,5'h1f,0);
            drive({u,".io_sfence_bits_rs1"},1);drive({u,".io_sfence_bits_rs2"},1);drive({u,".io_sfence_bits_hv"},0);drive({u,".io_sfence_bits_hg"},0);drive({u,".io_sfence_valid"},1);
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_sfence_valid"},0);found=0;
            for(int t=0;t<16;t++)begin #1ps;if(read_hdl({u,"._sfence_delay_io_out_valid"})===uvm_hdl_data_t'(1))begin found=1;break;end @(negedge clock_vif.clk);end
            if(!found)`uvm_fatal("DTLB0806_OVERLAP","no actual internal flush window")
            drive({u,".io_ptw_resp_valid"},1);#1ps;
            if(read_hdl({u,".entries.io_w_valid"})!==uvm_hdl_data_t'(0))`uvm_error("DTLB0806_REFILL","flush must suppress actual storage write")
            @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);token_valid=0;refills++;cycles(8);
            if(physical_v(g)!==48'b0)`uvm_error("DTLB0806_REFILL","flushed return revived storage")
            for(int kind=0;kind<4;kind++)begin
                string changed;begin_window(g);prepare_miss(g,va);return_fields(g,va,48'h70000000,6'h3f,0,5'h1f,0);
                case(kind)0:changed=".io_csr_satp_changed";1:changed=".io_csr_vsatp_changed";2:changed=".io_csr_hgatp_changed";3:changed=".io_csr_priv_virt_changed";endcase
                drive({u,changed},1);cycles(1);drive({u,changed},0);found=0;
                for(int t=0;t<16;t++)begin
                    #1ps;if(read_hdl({u,kind==0?"._csr_delay_io_out_satp_changed":kind==1?"._csr_delay_io_out_vsatp_changed":kind==2?"._csr_delay_io_out_hgatp_changed":"._csr_delay_io_out_priv_virt_changed"})===uvm_hdl_data_t'(1))begin found=1;break;end
                    @(negedge clock_vif.clk);
                end
                if(!found)`uvm_fatal("DTLB0806_CSR","actual CSR changed window missing")
                drive({u,".io_ptw_resp_valid"},1);#1ps;
                if(read_hdl({u,".entries.io_w_valid"})!==uvm_hdl_data_t'(0))`uvm_error("DTLB0806_CSR","CSR change failed to suppress real return write")
                @(posedge clock_vif.clk);@(negedge clock_vif.clk);drive({u,".io_ptw_resp_valid"},0);token_valid=0;refills++;cycles(8);
                if(physical_v(g)!==48'b0)`uvm_error("DTLB0806_CSR","CSR-flushed return revived empty table")
            end
            for(int gap_index=0;gap_index<4;gap_index++)continuous_fence_pair(g,gap_index==0?0:gap_index==1?1:gap_index==2?2:4);
            recovery_windows++;
        end
    endtask
    virtual task body();
        require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))`uvm_fatal("DTLB0806_CLOCK","missing real clock")
        if(seq_csr_common::get_l2tlb_seq_en()||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)`uvm_fatal("DTLB0806_OWNER","local fixture requires responder disabled")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);cycles(48);
        `include "dtlb_0501_0504_input_defaults.svh"
        drive({DUT,".inner_dtlbRepeater.reset"},1);initialize_lookup_ports();
        case(scenario())
            "scope":scope_matrix();"pages":pages_matrix_08();"stage":stage_matrix_08();"recovery":recovery_matrix_08();
            default:`uvm_fatal("DTLB0806_SCENARIO",scenario())
        endcase
        if(token_valid||initialization_checks!=8||(scenario()=="scope"&&fence_windows!=42)||(scenario()=="pages"&&fence_windows!=45)||(scenario()=="stage"&&stage_edges!=12)||(scenario()=="recovery"&&recovery_windows!=3))`uvm_error("DTLB0806_INCOMPLETE","scenario contract incomplete")
        cleanup();`uvm_info("DTLB0806_COMPLETE",$sformatf("scenario=%s requests=%0d returns=%0d checks=%0d fences=%0d way_checks=%0d stage_edges=%0d recovery_windows=%0d initialization=%0d",scenario(),probes,refills,target_checks,fence_windows,way_checks,stage_edges,recovery_windows,initialization_checks),UVM_LOW)
    endtask
endclass
`define DTLB0806_CLASS(CLASS,SCENARIO) \
class CLASS extends memblock_dtlb_0801_0806_vseq; \
 `uvm_object_utils(CLASS) \
 function new(string name=`"CLASS`" );super.new(name);endfunction \
 virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB0806_CLASS(memblock_dtlb_08_scope_vseq,"scope")
`DTLB0806_CLASS(memblock_dtlb_08_pages_vseq,"pages")
`DTLB0806_CLASS(memblock_dtlb_08_stage_vseq,"stage")
`DTLB0806_CLASS(memblock_dtlb_08_recovery_vseq,"recovery")
`undef DTLB0806_CLASS
`endif
