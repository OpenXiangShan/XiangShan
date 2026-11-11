`ifndef SOFT_TEST_RM_CMO_REQUEST_SV
`define SOFT_TEST_RM_CMO_REQUEST_SV

// 中文注释：只替换只读身份来源和预期负向报告；真实请求关联/计数逻辑仍使用被测observer。
class rm_cmo_request_soft_observer_t extends rm_cmo_request_observer_t;
    owner_t fixture_owner;
    function new(); super.new(); fixture_owner='0; endfunction
    virtual function bit resolve_owner(input cmo_path_probe_t event_i, output owner_t owner);
        owner=fixture_owner;
        if(event_i.kind==CMO_PATH_SQ_SBUFFER && event_i.port_id==1) owner.cmo_class=0;
        return owner.valid;
    endfunction
    virtual function void fail(input string detail);
        error_count++;
    endfunction
endclass

class rm_cmo_wb_soft_observer_t extends memblock_rm_dut_writeback_observer;
    function new(); super.new("cmo_wb_audit_soft"); endfunction
    virtual function void report_cmo_audit_error(input string detail);
        cmo_late_error_count++;
    endfunction
endclass

class rm_cmo_request_soft_test_t;
    int unsigned checks;
    function new(); checks=0; endfunction
    function void expect_true(input bit condition,input string message);
        checks++;
        if(!condition) begin
            `uvm_fatal("RM_CMO_REQUEST_SOFT",message)
        end
    endfunction
    function rm_cmo_request_soft_observer_t make_observer(input bit [8:0] op=MEMBLOCK_LSUOP_CBO_ZERO);
        rm_cmo_request_soft_observer_t observer;
        observer=new();
        observer.fixture_owner='{valid:1,uid:0,epoch:1,rob:9'h103,sq:7'h41,op:op,cmo_class:1};
        return observer;
    endfunction
    function rm_ls_program_item_t make_item(input bit [8:0] op=MEMBLOCK_LSUOP_CBO_ZERO);
        rm_ls_program_item_t item;
        item=new(0); item.valid=1; item.op=op;
        item.rob='{flag:1,value:16'd3}; item.sq_flag=1; item.sq_value=1;
        item.dynamic_epoch=1; item.pa_by_byte[0]=64'h1008; item.pa_valid_mask=1;
        item.translation_valid=1; item.main_vaddr=64'h1008; item.computed_vaddr=64'h1008;
        expect_true(rm_ls_assign_cmo_data_semantics(item),"decode request-test CMO");
        item.cmo_path=op==MEMBLOCK_LSUOP_CBO_ZERO ? RM_CMO_PATH_CACHEABLE_ZERO : RM_CMO_PATH_DCACHE_CMO;
        return item;
    endfunction
    function cmo_path_probe_t event_record(input cmo_path_kind_e kind,
                                          input int unsigned port_id=0,
                                          input int unsigned slot=5);
        cmo_path_probe_t e;
        e='0; e.valid=1; e.kind=kind; e.port_id=8'(port_id); e.slot=slot;
        e.sq=7'h41; e.rob=9'h103; e.owner_valid=0;
        e.addr=64'h1000; e.id=slot;
        return e;
    endfunction
    function cmo_path_agent_xaction batch(input longint unsigned sample_seq);
        cmo_path_agent_xaction tr;
        tr=new("cmo_request_soft_batch"); tr.sample_seq=sample_seq;
        tr.reset_generation=0; tr.reset_active=0;
        return tr;
    endfunction
    function void drive_event(input rm_cmo_request_soft_observer_t observer,
                              input longint unsigned sample_seq,input cmo_path_probe_t e);
        cmo_path_agent_xaction tr;
        tr=batch(sample_seq); tr.events.push_back(e); observer.observe_batch(tr);
    endfunction
    function void seed_zero(input rm_cmo_request_soft_observer_t observer,
                            input bit simultaneous_same_line=0,input bit merging=0);
        cmo_path_agent_xaction tr;
        cmo_path_probe_t e;
        tr=batch(1);
        e=event_record(CMO_PATH_SQ_SBUFFER); e.addr=64'h1008;
        e.qual=merging ? 16'h7 : 16'h3; e.mask=64'hffff;
        tr.events.push_back(e);
        if(simultaneous_same_line) begin
            e=event_record(CMO_PATH_SQ_SBUFFER,1); e.addr=64'h1010;
            e.qual=16'h2; e.mask=64'hff; e.data=512'hab;
            tr.events.push_back(e);
        end
        observer.observe_batch(tr);
        tr=batch(2); e=event_record(CMO_PATH_SB_ACCESS); tr.events.push_back(e);
        if(simultaneous_same_line) begin
            e=event_record(CMO_PATH_SB_ACCESS,1); tr.events.push_back(e);
        end
        observer.observe_batch(tr);
    endfunction
    function void check_zero_replay_and_shared_slot();
        rm_cmo_request_soft_observer_t observer;
        rm_ls_program_item_t item;
        cmo_path_probe_t e;
        string detail;
        observer=make_observer(); seed_zero(observer,1);
        expect_true(observer.slot_generation[5]==1 && observer.zero_by_slot[5].size()==1 &&
                    observer.error_count==0,"dual same-line insert allocates one slot generation and preserves zero");
        e=event_record(CMO_PATH_SB_DCACHE_REQUEST); e.mask='1; e.data=512'h1234;
        drive_event(observer,3,e);
        e=event_record(CMO_PATH_SB_DCACHE_REPLAY); drive_event(observer,4,e);
        item=make_item();
        expect_true(!observer.validate(item,4,detail),"replay alone cannot prove zero success");
        e=event_record(CMO_PATH_SB_DCACHE_REQUEST); e.mask='1;
        drive_event(observer,5,e);
        e=event_record(CMO_PATH_SB_DCACHE_SUCCESS); drive_event(observer,6,e);
        expect_true(observer.validate(item,6,detail) && item.cmo_request_token!=0 &&
                    item.cmo_request_count==1 && observer.error_count==0,
                    "unaligned zero ROB flag=1 real SB success precedes final STA0");
        expect_true(!observer.validate(item,5,detail) && !observer.observation_pending(),
                    "real SB success after final STA0 is illegal, not a scheduling wait");
        expect_true(observer.validate(item,6,detail),"valid final boundary recovers request proof");
        item.cmo_completion_consumed=1;
        expect_true(observer.mark_consumed(item) && observer.check_drained(),"terminal consumes request proof once");
        e=event_record(CMO_PATH_SQ_SBUFFER); e.addr=64'h1008; e.qual=3; e.mask=64'hffff;
        drive_event(observer,7,e);
        expect_true(observer.error_count==1 && !observer.validate(item,7,detail),
                    "late extra zero of consumed owner must fail");
    endfunction
    function void check_mask_and_reset();
        rm_cmo_request_soft_observer_t observer;
        rm_ls_program_item_t item;
        cmo_path_probe_t e;
        cmo_path_agent_xaction tr;
        string detail;
        observer=make_observer();
        e=event_record(CMO_PATH_SQ_SBUFFER); e.qual=3; e.mask=64'hff;
        drive_event(observer,1,e);
        item=make_item();
        expect_true(observer.error_count==1 && !observer.validate(item,1,detail),"zero input mask must cover 16B interface");
        tr=batch(2); tr.reset_generation=1; tr.reset_active=1; observer.observe_batch(tr);
        tr=batch(3); tr.reset_generation=1;
        e=event_record(CMO_PATH_SQ_SBUFFER); e.addr=64'h1008; e.qual=3; e.mask=64'hffff;
        tr.events.push_back(e); observer.observe_batch(tr);
        expect_true(observer.error_count==1 && observer.requests.num()==2,
                    "same UID/epoch after reset belongs to separate request history");
        observer=make_observer(); seed_zero(observer);
        e=event_record(CMO_PATH_SB_DCACHE_REQUEST); e.mask=64'hff;
        drive_event(observer,3,e);
        expect_true(observer.error_count==1,"zero output must retain full 64B mask");
    endfunction
    function void check_nonzero_tl_proof();
        rm_cmo_request_soft_observer_t observer;
        rm_ls_program_item_t item;
        cmo_path_probe_t e;
        cmo_path_agent_xaction tr;
        longint unsigned token;
        string detail;
        observer=make_observer(MEMBLOCK_LSUOP_CBO_CLEAN);
        e=event_record(CMO_PATH_SQ_CMO_REQUEST); e.op=MEMBLOCK_LSUOP_CBO_CLEAN;
        e.id=0; e.owner_valid=1; drive_event(observer,1,e);
        item=make_item(MEMBLOCK_LSUOP_CBO_CLEAN);
        expect_true(!observer.validate(item,1,detail),"internal CMO alone cannot prove real TL request");
        mem_access_base_sequence::reset_l2_line_history();
        mem_access_base_sequence::register_clean_l2_access(48'h1000,2,10'd17,3);
        token=mem_access_base_sequence::begin_l2_response_history(48'h1000,2,10'd17,
            0,3,4'd12,0,1,0,0,0);
        expect_true(token!=0,"nonzero soft test creates independent accepted response token");
        e=event_record(CMO_PATH_DCACHE_TL_A); e.op=9'd12; e.id=17; e.state=6;
        drive_event(observer,2,e);
        expect_true(observer.validate(item,2,detail) && item.cmo_request_token==token &&
                    observer.error_count==0,"nonzero needs actual aligned TL CBO and exact response token");
        item.cmo_completion_consumed=1;
        expect_true(observer.mark_consumed(item) && observer.check_drained(),"nonzero terminal consumes proof");
        // 同拍producer先发布TL、后发布SQ也按真实因果顺序关联。
        mem_access_base_sequence::reset_l2_line_history();
        observer=make_observer(MEMBLOCK_LSUOP_CBO_CLEAN);
        mem_access_base_sequence::register_clean_l2_access(48'h1000,1,10'd17,3);
        token=mem_access_base_sequence::begin_l2_response_history(48'h1000,1,10'd17,0,3,4'd12,0,1,0,0,0);
        tr=batch(1);
        e=event_record(CMO_PATH_DCACHE_TL_A); e.op=9'd12; e.id=17; e.state=6;
        tr.events.push_back(e);
        e=event_record(CMO_PATH_SQ_CMO_REQUEST); e.op=MEMBLOCK_LSUOP_CBO_CLEAN;
        e.id=0; e.owner_valid=1; tr.events.push_back(e); observer.observe_batch(tr);
        item=make_item(MEMBLOCK_LSUOP_CBO_CLEAN);
        expect_true(observer.validate(item,1,detail) && observer.error_count==0,
                    "same-sample TL/SQ producer ordering cannot change request proof");
        // 只查请求证明；不以该响应的错误位或最终异常反推预期类型。
        mem_access_base_sequence::reset_l2_line_history();
        observer=make_observer(MEMBLOCK_LSUOP_CBO_CLEAN);
        e=event_record(CMO_PATH_SQ_CMO_REQUEST); e.op=MEMBLOCK_LSUOP_CBO_CLEAN;
        e.id=0; e.owner_valid=1; drive_event(observer,1,e);
        mem_access_base_sequence::register_clean_l2_access(48'h1000,2,10'd17,3);
        token=mem_access_base_sequence::begin_l2_response_history(48'h1000,2,10'd17,0,3,4'd12,0,1,0,0,0);
        e=event_record(CMO_PATH_DCACHE_TL_A); e.op=9'd12; e.id=17; e.state=6; e.addr=64'h1008;
        drive_event(observer,2,e);
        expect_true(observer.error_count==1 && !observer.validate(item,2,detail),
                    "nonzero TL CBO must use line-aligned address even when instruction offset is nonzero");
    endfunction
    function void check_no_cacheable_request();
        rm_cmo_request_soft_observer_t observer;
        rm_ls_program_item_t item;
        cmo_path_probe_t e;
        string detail;
        observer=make_observer(); item=make_item();
        item.cmo_path=RM_CMO_PATH_UNCACHE_ZERO;
        expect_true(!observer.validate_no_requests(item,1,detail),
                    "no-request check waits for independent monitor watermark");
        observer.observe_batch(batch(1));
        expect_true(observer.validate_no_requests(item,1,detail),
                    "Uncache zero owner has no cacheable requests");
        e=event_record(CMO_PATH_SQ_SBUFFER); e.qual=3; e.mask=64'hffff;
        drive_event(observer,2,e);
        expect_true(!observer.validate_no_requests(item,2,detail),
                    "Uncache zero rejects illegal cacheable owner request");
        observer=make_observer();
        e=event_record(CMO_PATH_SQ_SBUFFER); e.qual=2; e.mask=64'hff;
        drive_event(observer,1,e);
        expect_true(observer.error_count==1 && !observer.validate_no_requests(item,1,detail),
                    "CMO with illegal ordinary SQ->SB payload cannot bypass forbidden-path check");
        item.cmo_pre_access_exception=24'h80;
        expect_true(!observer.validate(item,1,detail),
                    "front-fault CMO rejects every cacheable owner request");
    endfunction
    // 抽象职责：在真实observer终态审计入口验证commit后重复，不依赖RM重新访问已推进UID。
    function void check_late_completion_audit();
        common_data_transaction data;
        main_control_transaction main_tr;
        status_transaction status_v;
        rm_cmo_wb_soft_observer_t observer;
        memblock_rm_dut_writeback_record_t completion,late_v;
        int unsigned saved_flush;
        data=common_data_transaction::get(); saved_flush=memblock_sync_pkg::dispatch_flush_epoch;
        data.main_trans_num=1; data.main_table_by_uid=new[1]; data.status_by_uid=new[1];
        main_tr=main_control_transaction::type_id::create("cmo_terminal_audit_main");
        status_v=status_transaction::type_id::create("cmo_terminal_audit_status"); status_v.reset(0);
        data.main_table_by_uid[0]=main_tr; data.status_by_uid[0]=status_v; data.uid_by_active_rob.delete();
        main_tr.op_class=MEMBLOCK_OP_CLASS_CBO; main_tr.fuType=MEMBLOCK_FUTYPE_STU;
        main_tr.fuOpType=MEMBLOCK_LSUOP_CBO_CLEAN;
        status_v.dynamic_epoch=4; status_v.robIdx_flag=0; status_v.robIdx_value=3;
        status_v.sqIdx_flag=1; status_v.sqIdx_value=2; status_v.sta_issue_epoch=9; status_v.replay_seq=2;
        status_v.sta_instance_flush_epoch_valid=1; status_v.sta_instance_flush_epoch=6;
        status_v.terminal_done=1; status_v.sta_writeback=1; status_v.active=0;
        completion='0; completion.valid=1; completion.source_kind=2; completion.lane=0;
        completion.rob_valid=1; completion.rob_flag_valid=1; completion.rob_value=3;
        completion.owner_valid=1; completion.owner_uid=0; completion.owner_dynamic_epoch=4;
        completion.exception_valid=1; completion.sample_seq=10; completion.sample_flush_epoch=6;
        memblock_sync_pkg::dispatch_flush_epoch=6;
        observer=new();
        expect_true(observer.mark_cmo_completion_consumed(completion),"normal CMO terminal audit registers once");
        late_v=completion; late_v.sample_seq=11; late_v.owner_valid=0;
        observer.audit_cmo_sta0(late_v);
        expect_true(observer.cmo_late_error_count==1,"normal CMO duplicate after commit is rejected");
        observer=new(); completion.exception_vec=24'h80;
        status_v.fault=1; status_v.sta_fault=1; status_v.exception_vec=24'h80;
        expect_true(observer.mark_cmo_completion_consumed(completion),"fault CMO terminal audit registers once");
        late_v=completion; late_v.sample_seq=11; late_v.owner_valid=0; late_v.exception_vec=0;
        observer.audit_cmo_sta0(late_v);
        expect_true(observer.cmo_late_error_count==0,"one proven fault CMO late normal is a legal nonarchitectural tail");
        late_v.sample_seq=12; observer.audit_cmo_sta0(late_v);
        expect_true(observer.cmo_late_error_count==1,"second late normal cannot reuse terminal tail exception");
        observer=new(); expect_true(observer.mark_cmo_completion_consumed(completion),"fault duplicate fixture registers");
        late_v=completion; late_v.sample_seq=11; late_v.owner_valid=0;
        observer.audit_cmo_sta0(late_v);
        expect_true(observer.cmo_late_error_count==1,"second fault after consumed CMO is rejected");
        observer=new(); expect_true(observer.mark_cmo_completion_consumed(completion),"provenance fixture registers");
        status_v.replay_seq=3; late_v.exception_vec=0; observer.audit_cmo_sta0(late_v);
        expect_true(observer.cmo_late_error_count==1,"late normal with wrong replay provenance is rejected");
        status_v.replay_seq=2;
        observer=new(); expect_true(observer.mark_cmo_completion_consumed(completion),"ROB reuse fixture registers");
        late_v.owner_valid=1; late_v.owner_uid=1; late_v.owner_dynamic_epoch=1;
        observer.audit_cmo_sta0(late_v);
        expect_true(observer.cmo_late_error_count==0 && !observer.cmo_terminal_by_rob.exists(9'd3),
                    "new frozen owner for reused ROB takes precedence over old CMO summary");
        observer=new(); late_v=completion; late_v.sample_seq=11; late_v.owner_valid=0; late_v.exception_vec=0;
        observer.sta0_history_by_rob[9'd3].push_back(late_v);
        late_v.sample_seq=12; observer.sta0_history_by_rob[9'd3].push_back(late_v);
        expect_true(observer.mark_cmo_completion_consumed(completion) && observer.cmo_late_error_count==1,
                    "late records captured before RM cursor terminal registration are audited once");
        observer.clear_history(); expect_true(observer.cmo_terminal_by_rob.num()==0,"reset clears consumed CMO audit summaries");
        memblock_sync_pkg::dispatch_flush_epoch=saved_flush;
    endfunction
    function void run();
        check_zero_replay_and_shared_slot();
        check_mask_and_reset();
        check_nonzero_tl_proof();
        check_no_cacheable_request();
        check_late_completion_audit();
        `uvm_info("RM_CMO_REQUEST_SOFT",$sformatf("CMO request checks passed: %0d",checks),UVM_LOW)
    endfunction
endclass
`endif
