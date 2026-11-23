`ifndef MEMBLOCK_DTLB_0804_LIVE_VSEQ__SV
`define MEMBLOCK_DTLB_0804_LIVE_VSEQ__SV
// 抽象职责：唯一正式L2TLB owner处理真实请求；顶层SFENCE与Filter分配/发射竞争。
// 不驱动ready、response、内部slot或覆盖变量，flush前后token全部保留正式记账。
class memblock_dtlb_0804_live_vseq extends memblock_dtlb_0501_0504_vseq;
    `uvm_object_utils(memblock_dtlb_0804_live_vseq)
    // 真实生效沿的交叠次数由只读观察累计；每个窗口结束清空Filter后重新开始。
    int flush_enq[3],flush_issue[3],flush_return[3],reset_checks[3];
    function new(string name="memblock_dtlb_0804_live_vseq");super.new(name);for(int g=0;g<3;g++)begin flush_enq[g]=0;flush_issue[g]=0;flush_return[g]=0;reset_checks[g]=0;end endfunction
    task top_fence(input bit valid);
        string q;q="top_tb.u_memblock__fence_agent_if.io_ooo_to_mem_sfence";
        drive({q,"_bits_rs1"},1);drive({q,"_bits_rs2"},1);drive({q,"_bits_addr"},0);drive({q,"_bits_id"},0);
        drive({q,"_bits_hv"},0);drive({q,"_bits_hg"},0);drive({q,"_bits_flushPipe"},0);drive({q,"_valid"},valid);
    endtask
    // 抽象职责：在实际flush沿比较下一拍v，并记录同拍enqueue/fire/return而不虚构交叠。
    task sample_flush(input int g);
        string u;bit flush,enq,issue,resp;u=filter_path(g);#1ps;
        flush=1'(read_hdl({u,".io_flush"}));enq=0;
        for(int p=0;p<filter_ports(g);p++)enq|=1'(read_hdl($sformatf("%s.enqvalid_%0d",u,p)));
        issue=read_hdl({u,".io_ptw_req_0_valid"})===uvm_hdl_data_t'(1)&&read_hdl({u,".io_ptw_req_0_ready"})===uvm_hdl_data_t'(1);
        resp=read_hdl({u,".io_ptw_resp_valid"})===uvm_hdl_data_t'(1)&&read_hdl({u,".io_refill"})===uvm_hdl_data_t'(1);
        @(posedge clock_vif.clk);@(negedge clock_vif.clk);
        if(flush)begin
            if(valid_slots(g)!==16'b0)`uvm_error("DTLB0804_VALID","real flush must clear all physical valid slots")
            flush_enq[g]+=int'(enq);flush_issue[g]+=int'(issue);flush_return[g]+=int'(resp);
            `uvm_info("DTLB0804_FLUSH",$sformatf("g=%0d enq=%0b issue_fire=%0b matched_return=%0b v=%h accepted=%0d canceled=%0d",g,enq,issue,resp,valid_slots(g),live_owner.accepted_count,live_owner.flush_canceled_count),UVM_LOW)
        end
    endtask
    task race_matrix();
        for(int g=0;g<3;g++)begin
            for(int offset=0;offset<10;offset++)begin
                filter_reset();top_fence(1);
                // 相对顶层事件逐拍平移真实请求，实际io_flush由RTL延迟链产生。
                if(offset==0)set_filter_request(g,0,38'hb0000+38'(g)*4096+38'(offset)*32);
                sample_flush(g);top_fence(0);
                for(int t=1;t<14;t++)begin
                    if(t==offset)set_filter_request(g,0,38'hb0000+38'(g)*4096+38'(offset)*32);
                    if(t==offset+1)clear_filter_requests(g);
                    sample_flush(g);
                end
                clear_filter_requests(g);cycles(8);filter_reset();group_windows++;
            end
            // 真实reset优先级用声明输入检查；模型矛盾的001仓仍保持原样。
            set_filter_request(g,0,38'hc0000+38'(g)*4096);cycles(2);clear_filter_requests(g);
            top_fence(1);drive({filter_path(g),".reset"},1);cycles(2);
            if(valid_slots(g)!==16'b0)`uvm_error("DTLB0804_RESET","actual reset did not clear filter")
            reset_checks[g]++;top_fence(0);drive({filter_path(g),".reset"},0);cycles(20);filter_reset();
            // reset后正式新token允许返回；不把已取消旧token重新送入。
            begin longint unsigned old_completed;bit done;old_completed=live_owner.completed_count;done=0;
                set_filter_request(g,0,38'hd0000+38'(g)*4096);cycles(1);clear_filter_requests(g);
                for(int t=0;t<1200;t++)begin @(negedge clock_vif.clk);#1ps;if(live_owner.completed_count>old_completed&&valid_slots(g)==0)begin done=1;break;end end
                if(!done)`uvm_error("DTLB0804_RECOVERY","new request after reset/flush did not complete")
            end
            if(flush_enq[g]==0)`uvm_error("DTLB0804_ENQ","no actual enqueue/flush overlap")
        end
    endtask
    virtual task body();
        require_virtual_sqr();seq_csr_common::init();
        if(!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,"uvm_test_top.env.u_lintsissue_agent_agent","vif",clock_vif))`uvm_fatal("DTLB0804_CLOCK","missing clock")
        if(!seq_csr_common::get_l2tlb_seq_en()||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)`uvm_fatal("DTLB0804_OWNER","unique explicit owner required")
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
        live_owner=memblock_l2tlb_base_sequence::type_id::create("dtlb0804_formal_owner");
        live_adapter=dispatch_monitor_event_adapter::type_id::create("dtlb0804_fence_adapter");live_owner_done=0;adapter_stop=0;
        fork
            begin live_owner.start(p_sequencer.L2tlb_sqr);live_owner_done=1;end
            begin while(!adapter_stop)begin @(negedge clock_vif.clk);live_adapter.service_l2tlb_sfence_events();void'(memblock_sync_pkg::grant_l2tlb_final_release());end end
        join_none
        cycles(12);for(int p=0;p<8;p++)drive($sformatf("%s.inner_dtlbRepeater.io_tlb_req_%0d_valid",DUT,p),0);
        for(int g=0;g<3;g++)begin string u;u=filter_path(g);if(g==0)begin `include "dtlb_0501_0504_load_live.svh" end else begin `include "dtlb_0501_0504_small_live.svh" end end
        race_matrix();filter_reset();
        begin common_data_transaction data;data=common_data_transaction::get();data.request_global_stop_if_done();end
        for(int n=0;n<100&&!live_owner_done;n++)@(negedge clock_vif.clk);
        if(!live_owner_done||memblock_sync_pkg::l2tlb_lifecycle_owner_claimed)`uvm_fatal("DTLB0804_OWNER","owner failed natural release")
        if(live_owner.accepted_count!=live_owner.completed_count+live_owner.flush_canceled_count+live_owner.reset_canceled_count)`uvm_error("DTLB0804_ACCOUNT","token accounting mismatch")
        adapter_stop=1;wait fork;
        for(int g=0;g<3;g++)`uvm_info("DTLB0804_GROUP",$sformatf("g=%0d flush_enq=%0d flush_issue=%0d flush_return=%0d reset_checks=%0d",g,flush_enq[g],flush_issue[g],flush_return[g],reset_checks[g]),UVM_LOW)
        if(group_windows!=30||initialization_checks!=8)`uvm_error("DTLB0804_INCOMPLETE","window count mismatch")
        `uvm_info("DTLB0804_OWNER_FINAL",$sformatf("accepted=%0d completed=%0d flush_canceled=%0d reset_canceled=%0d",live_owner.accepted_count,live_owner.completed_count,live_owner.flush_canceled_count,live_owner.reset_canceled_count),UVM_LOW)
        cleanup();`uvm_info("DTLB0806_COMPLETE",$sformatf("scenario=filter windows=%0d initialization=%0d",group_windows,initialization_checks),UVM_LOW)
    endtask
endclass
`endif
