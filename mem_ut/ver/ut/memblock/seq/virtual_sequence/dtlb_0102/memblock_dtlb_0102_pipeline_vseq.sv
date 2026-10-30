`ifndef MEMBLOCK_DTLB_0102_PIPELINE_VSEQ__SV
`define MEMBLOCK_DTLB_0102_PIPELINE_VSEQ__SV

// 仅复用01.01已有的HDL输入驱动、四态读取、时钟及reset清理helper。
// 本类body不调用父类body，不复用其单请求检查或启动额外responder。
class memblock_dtlb_0102_pipeline_vseq extends memblock_dtlb_0101_source_vseq;
    `uvm_object_utils(memblock_dtlb_0102_pipeline_vseq)
    typedef bit [49:0] addr_t;
    // 本次固定目标最多两个口；每拍只遍历所选物理口，不扫描全局主表。
    string tlb_name[2];
    int unsigned port_number[2];
    int unsigned target_count;
    // 上一拍/前两拍有效位及payload，只由逐拍采样任务更新。
    bit previous_valid[2], older_valid[2];
    addr_t previous_addr[2];
    bit [2:0] previous_cmd[2];
    bit [7:0] previous_rob[2];
    // 自建真实请求连续长度与完成响应计数，不读取覆盖模型的历史状态。
    int unsigned run_length[2], response_checks[2], bubble_checks[2], idle_cycles[2];
    bit [3:0] run_mask[2];
    // 当前有限窗口的预期地址与已观察集合，最多8拍*2口；排空后清理。
    bit [2:0] expected_cmd[addr_t];
    bit observed_addr[addr_t];
    int unsigned remaining_addresses;
    // driver每个新地址分配一个递增索引，避免预取过滤器把不同输入误合并。
    int unsigned address_serial;
    bit monitor_running;

    function new(string name="memblock_dtlb_0102_pipeline_vseq");
        super.new(name);
        target_count=1;
        address_serial=0;
        monitor_running=0;
        remaining_addresses=0;
        foreach (previous_valid[i]) begin
            previous_valid[i]=0; older_valid[i]=0; run_length[i]=0;
            response_checks[i]=0; bubble_checks[i]=0; idle_cycles[i]=0; run_mask[i]=0;
        end
    endfunction
    virtual function string scenario(); return "l2_query"; endfunction

    // 抽象职责：配置所选真实请求口，不改变任何connect/interface物理结构。
    function void select_targets();
        case (scenario())
            "l2_query": begin tlb_name[0]="inner_dtlb_prefetch_tlb_prefetch";port_number[0]=1;end
            "store_dual": begin
                target_count=2;
                tlb_name[0]="inner_dtlb_st_tlb_st";port_number[0]=0;
                tlb_name[1]="inner_dtlb_st_tlb_st";port_number[1]=1;
            end
            "stream": begin tlb_name[0]="inner_dtlb_ld_tlb_ld";port_number[0]=3;end
            "sms": begin tlb_name[0]="inner_dtlb_prefetch_tlb_prefetch";port_number[0]=0;end
            default: `uvm_fatal("DTLB0102_SCENARIO",scenario())
        endcase
    endfunction

    // 抽象职责：在一个posedge检查前拍请求对应的真实响应，记录连续长度和空泡。
    // 每次仅做有限宽度读取与当前窗口map查询；不人工修改DUT或覆盖率状态。
    function void sample_port(input int unsigned index);
        string prefix;
        uvm_hdl_data_t valid_raw, address_raw, cmd_raw, resp_raw, kill_raw, rob_raw;
        addr_t address;
        bit valid;
        prefix={DUT,".",tlb_name[index]};
        valid_raw=read_hdl($sformatf("%s.io_requestor_%0d_req_valid",prefix,port_number[index]));
        resp_raw=read_hdl($sformatf("%s.io_requestor_%0d_resp_valid",prefix,port_number[index]));
        if ($isunknown({valid_raw,resp_raw}))
            `uvm_fatal("DTLB0102_XZ","unknown request/response valid")
        valid=(valid_raw==1);
        if (resp_raw !== uvm_hdl_data_t'(previous_valid[index]))
            `uvm_error("DTLB0102_VALID",$sformatf("%s port=%0d response=%0h expected=%0d",tlb_name[index],port_number[index],resp_raw,previous_valid[index]))
        if (previous_valid[index]) begin
            if (read_hdl($sformatf("%s.req_out_%0d_vaddr",prefix,port_number[index]),50) !== uvm_hdl_data_t'(previous_addr[index]) ||
                read_hdl($sformatf("%s.req_out_%0d_cmd",prefix,port_number[index]),3) !== uvm_hdl_data_t'(previous_cmd[index]) ||
                read_hdl($sformatf("%s.io_requestor_%0d_resp_bits_paddr_0",prefix,port_number[index]),48) !== uvm_hdl_data_t'(previous_addr[index][47:0]) ||
                read_hdl($sformatf("%s.io_requestor_%0d_resp_bits_miss",prefix,port_number[index])) !== uvm_hdl_data_t'(0))
                `uvm_error("DTLB0102_PAYLOAD",$sformatf("%s port=%0d expected va=%0h cmd=%0d",tlb_name[index],port_number[index],previous_addr[index],previous_cmd[index]))
            if (scenario()!="l2_query" &&
                read_hdl($sformatf("%s.req_out_%0d_debug_robIdx_value",prefix,port_number[index]),8) !== uvm_hdl_data_t'(previous_rob[index]))
                `uvm_error("DTLB0102_ROB","ROB changed across request pipeline")
            response_checks[index]++;
        end
        if (valid) begin
            address_raw=read_hdl($sformatf("%s.io_requestor_%0d_req_bits_vaddr",prefix,port_number[index]),50);
            cmd_raw=read_hdl($sformatf("%s.io_requestor_%0d_req_bits_cmd",prefix,port_number[index]),3);
            kill_raw=(scenario()=="store_dual")?0:read_hdl($sformatf("%s.io_requestor_%0d_req_bits_kill",prefix,port_number[index]));
            if ($isunknown({address_raw,cmd_raw}) || kill_raw !== uvm_hdl_data_t'(0))
                `uvm_fatal("DTLB0102_REQUEST","unknown or killed target request")
            address=addr_t'(address_raw);
            if (!expected_cmd.exists(address) || cmd_raw !== uvm_hdl_data_t'(expected_cmd[address]))
                `uvm_error("DTLB0102_UNEXPECTED",$sformatf("%s port=%0d va=%0h cmd=%0h outside current window",tlb_name[index],port_number[index],address,cmd_raw))
            else if (!observed_addr.exists(address)) begin
                observed_addr[address]=1;remaining_addresses--;
            end
            if (older_valid[index] && !previous_valid[index]) begin
                bubble_checks[index]++;
                `uvm_info("DTLB0102_BUBBLE",$sformatf("scenario=%s port=%0d observed 1,0,1 with propagated response bubble",scenario(),port_number[index]),UVM_LOW)
            end
            previous_addr[index]=address;
            previous_cmd[index]=3'(cmd_raw);
            rob_raw=0;
            if (scenario()!="l2_query") rob_raw=read_hdl($sformatf("%s.io_requestor_%0d_req_bits_debug_robIdx_value",prefix,port_number[index]),8);
            previous_rob[index]=8'(rob_raw);
            run_length[index]++;
            idle_cycles[index]=0;
        end else begin
            if (run_length[index]>=2) begin
                if (run_length[index]==2) run_mask[index][0]=1;
                else if (run_length[index]==3) run_mask[index][1]=1;
                else if (run_length[index]<8) run_mask[index][2]=1;
                else run_mask[index][3]=1;
                `uvm_info("DTLB0102_RUN",$sformatf("scenario=%s port=%0d live_run=%0d",scenario(),port_number[index],run_length[index]),UVM_LOW)
            end
            run_length[index]=0;idle_cycles[index]++;
        end
        older_valid[index]=previous_valid[index];
        previous_valid[index]=valid;
    endfunction

    // 抽象职责：向实际来源输入提供一个新地址；空闲调用只撤销valid。
    // 预取region以1KB为单位，SMS tag使用同一region的稳定hash。
    task drive_beat(input bit valid);
        addr_t address;
        bit [39:0] region;
        bit [2:0] cmd;
        string unit_path,sta;
        if (!valid) begin
            case (scenario())
                "l2_query": drive("top_tb.io_l2_tlb_req_req_valid",0);
                "store_dual": begin
                    drive({ISSUE,".io_ooo_to_mem_issueSta_0_valid"},0);
                    drive({ISSUE,".io_ooo_to_mem_issueSta_1_valid"},0);
                end
                "stream": drive({DUT,".inner_l1PrefetcherOpt.pf_queue_filter.io_l1_prefetch_req_valid"},0);
                "sms": drive({DUT,".inner_prefetcherOpt.pf_filter.io_gen_req_valid"},0);
            endcase
            return;
        end
        for (int unsigned index=0;index<target_count;index++) begin
            address=50'h80000000+50'(address_serial)*50'h1000;
            address_serial++;
            cmd=scenario()=="store_dual" ? 1 : scenario()=="l2_query" ? 3'(address_serial%2) : 0;
            expected_cmd[address]=cmd;remaining_addresses++;
            case (scenario())
                "l2_query": begin
                    drive("top_tb.io_l2_tlb_req_req_bits_vaddr",address);
                    drive("top_tb.io_l2_tlb_req_req_bits_cmd",cmd);
                    drive("top_tb.io_l2_tlb_req_req_bits_kill",0);
                    drive("top_tb.io_l2_tlb_req_req_bits_no_translate",0);
                    drive("top_tb.io_l2_tlb_req_req_bits_isPrefetch",1);
                    drive("top_tb.io_l2_tlb_req_req_valid",1);
                end
                "store_dual": begin
                    sta=$sformatf("%s.io_ooo_to_mem_issueSta_%0d",ISSUE,index);
                    drive({sta,"_bits_src_0"},address);
                    drive({sta,"_bits_uop_fuType"},encode_and_fit_dut_futype(MEMBLOCK_FUTYPE_STU,get_type_name()));
                    drive({sta,"_bits_uop_fuOpType"},MEMBLOCK_LSUOP_SD);
                    drive({sta,"_bits_uop_imm"},0);
                    drive({sta,"_bits_uop_robIdx_value"},8'(address_serial));
                    drive({sta,"_bits_uop_sqIdx_value"},6'(address_serial));
                    drive({sta,"_valid"},1);
                end
                "stream": begin
                    region=40'(address>>10);
                    unit_path={DUT,".inner_l1PrefetcherOpt.pf_queue_filter"};
                    drive({unit_path,".io_enable"},1);
                    drive({unit_path,".io_l1_prefetch_req_bits_region"},region);
                    drive({unit_path,".io_l1_prefetch_req_bits_bit_vec"},1);
                    drive({unit_path,".io_l1_prefetch_req_bits_source_value"},3);
                    drive({unit_path,".io_l1_prefetch_req_valid"},1);
                end
                "sms": begin
                    region=40'(address>>10);
                    unit_path={DUT,".inner_prefetcherOpt.pf_filter"};
                    drive({unit_path,".io_gen_req_bits_region_addr"},region);
                    drive({unit_path,".io_gen_req_bits_region_tag"},{region[10:6]^region[15:11]^region[20:16],region[5:0]});
                    drive({unit_path,".io_gen_req_bits_region_bits"},1);
                    drive({unit_path,".io_gen_req_bits_paddr_valid"},0);
                    drive({unit_path,".io_gen_req_bits_decr_mode"},0);
                    drive({unit_path,".io_gen_req_valid"},1);
                end
            endcase
        end
    endtask

    // 抽象职责：以所有预期地址被观测、最后响应结束且口连续空闲为完成条件。
    // 256拍仅是失败诊断上限，不作为正常完成；只检查最多两个口和窗口计数。
    task wait_drained();
        bit done;
        for (int wait_count=0;wait_count<256;wait_count++) begin
            done=(remaining_addresses==0);
            for (int unsigned index=0;index<target_count;index++)
                done &= idle_cycles[index]>=8 && !previous_valid[index];
            if (done) return;
            @(negedge clock_vif.clk);
        end
        `uvm_fatal("DTLB0102_DRAIN",$sformatf("scenario=%s unobserved=%0d",scenario(),remaining_addresses))
    endtask

    task prepare_window();
        string source_path;
        wait_drained();
        // 每组预取节拍从空过滤器开始，隔离16槽容量替换和吞吐采样。
        // 只在前组地址已全部出现、响应结束且连续空闲后驱动真实模块reset输入。
        if (scenario()=="stream" || scenario()=="sms") begin
            source_path=(scenario()=="stream") ?
                {DUT,".inner_l1PrefetcherOpt.pf_queue_filter"} :
                {DUT,".inner_prefetcherOpt.pf_filter"};
            drive({source_path,".reset"},1);
            cycles(2);
            drive({source_path,".reset"},0);
            cycles(2);
        end
        expected_cmd.delete();observed_addr.delete();
        remaining_addresses=0;
    endtask

    task drive_burst(input int unsigned length);
        prepare_window();
        repeat (length) begin
            drive_beat(1);
            @(negedge clock_vif.clk);
        end
        drive_beat(0);
        wait_drained();
    endtask

    task drive_bubble();
        prepare_window();
        drive_beat(1);@(negedge clock_vif.clk);
        drive_beat(0);@(negedge clock_vif.clk);
        drive_beat(1);@(negedge clock_vif.clk);
        drive_beat(0);wait_drained();
    endtask

    virtual task body();
        require_virtual_sqr();seq_csr_common::init();select_targets();
        if (!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,
            "uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))
            `uvm_fatal("DTLB0102_CLOCK","cannot get issue clock")
        wait(memblock_sync_pkg::reset_backend_done===1'b1);
        cycles(48);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_satp_mode",0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_vsatp_mode",0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_hgatp_mode",0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_priv_virt",0);
        drive("top_tb.u_memblock__csr_ctrl_agent_if.io_ooo_to_mem_tlbCsr_priv_dmode",3);
        cycles(8);
        if (read_hdl({DUT,".inner_dtlb_ld_tlb_ld.io_csr_satp_mode"},4)!==uvm_hdl_data_t'(0))
            `uvm_fatal("DTLB0102_CONTEXT","Bare context not visible at DTLB")
        monitor_running=1;
        fork
            begin
                while (monitor_running) begin
                    @(posedge clock_vif.clk);
                    if (monitor_running)
                        for (int unsigned index=0;index<target_count;index++) sample_port(index);
                end
            end
        join_none
        drive_burst(2);drive_burst(3);drive_burst(4);drive_burst(8);drive_bubble();
        for (int unsigned index=0;index<target_count;index++) begin
            if (run_mask[index]!==4'b1111 || bubble_checks[index]==0 || response_checks[index]<19)
                `uvm_error("DTLB0102_COVERAGE",$sformatf("scenario=%s port=%0d runs=%b bubbles=%0d responses=%0d",scenario(),port_number[index],run_mask[index],bubble_checks[index],response_checks[index]))
            `uvm_info("DTLB0102_CHECK",$sformatf("scenario=%s port=%0d runs=%b bubbles=%0d responses=%0d",scenario(),port_number[index],run_mask[index],bubble_checks[index],response_checks[index]),UVM_LOW)
        end
        monitor_running=0;
        @(negedge clock_vif.clk);
        wait fork;
        cleanup();
        `uvm_info("DTLB0102_COMPLETE",$sformatf("scenario=%s request pipeline checks completed",scenario()),UVM_LOW)
    endtask
endclass

`define DTLB0102_CLASS(CLASS, SCENARIO) \
class CLASS extends memblock_dtlb_0102_pipeline_vseq; \
    `uvm_object_utils(CLASS) \
    function new(string name=`"CLASS`");super.new(name);endfunction \
    virtual function string scenario();return SCENARIO;endfunction \
endclass
`DTLB0102_CLASS(memblock_dtlb_0102_l2_query_vseq,"l2_query")
`DTLB0102_CLASS(memblock_dtlb_0102_store_dual_vseq,"store_dual")
`DTLB0102_CLASS(memblock_dtlb_0102_stream_vseq,"stream")
`DTLB0102_CLASS(memblock_dtlb_0102_sms_vseq,"sms")
`undef DTLB0102_CLASS
`endif
