`ifndef SOFT_TEST_RM_UNCACHE_ERROR_OBSERVER_SV
`define SOFT_TEST_RM_UNCACHE_ERROR_OBSERVER_SV
// 仅软件自测使用的负向报告接收器；真实observer不因测试提供容错开关。
class rm_uncache_expected_report_catcher extends uvm_report_catcher;
    string report_id;
    int unsigned caught;
    function new(string id_v); super.new("uncache_expected_report"); report_id=id_v; caught=0; endfunction
    virtual function action_e catch();
        if(get_id()==report_id && (get_severity()==UVM_ERROR || get_severity()==UVM_FATAL)) begin
            caught++; return CAUGHT;
        end
        return THROW;
    endfunction
endclass

class rm_uncache_error_soft_test_t;
    int unsigned checks;
    common_data_transaction data;
    rm_uncache_error_observer observer;
    function new(); checks=0; endfunction
    function void expect_true(input bit cond,input string detail);
        checks++;
        if(!cond) `uvm_fatal("RM_UNCACHE_ERROR_SOFT",detail)
    endfunction
    function cmo_path_agent_xaction batch(input longint unsigned sample);
        cmo_path_agent_xaction tr;
        tr=cmo_path_agent_xaction::type_id::create("soft_uncache_path");
        tr.sample_seq=sample; tr.reset_generation=0; tr.reset_active=0;
        return tr;
    endfunction
    function void setup(input bit is_zero);
        cmo_path_agent_xaction tr;
        cmo_path_probe_t event_v;
        data=common_data_transaction::get();
        data.main_trans_num=1; data.main_table_by_uid=new[1]; data.status_by_uid=new[1];
        data.main_table_by_uid[0]=main_control_transaction::type_id::create("uncache_soft_main");
        data.status_by_uid[0]=status_transaction::type_id::create("uncache_soft_status");
        data.status_by_uid[0].reset(0);
        data.main_table_by_uid[0].uid=0;
        data.main_table_by_uid[0].fuOpType=is_zero ? MEMBLOCK_LSUOP_CBO_ZERO : MEMBLOCK_LSUOP_SD;
        data.main_table_by_uid[0].robIdx_flag=0; data.main_table_by_uid[0].robIdx_value=3;
        data.main_table_by_uid[0].sqIdx_flag=0; data.main_table_by_uid[0].sqIdx_value=2;
        data.status_by_uid[0].dynamic_epoch=4; data.status_by_uid[0].active=1;
        data.status_by_uid[0].robIdx_flag=0; data.status_by_uid[0].robIdx_value=3;
        data.uid_by_active_rob.delete(); data.uid_by_active_rob[9'd3]=0;
        mem_access_base_sequence::reset_uncache_d_error_ledger();
        memblock_sync_pkg::reset_cache_error_control();
        observer=rm_uncache_error_observer::type_id::create("uncache_soft_observer");
        tr=batch(1); event_v='0; event_v.valid=1; event_v.kind=CMO_PATH_SQ_ENQUEUE;
        event_v.owner_valid=1; event_v.rob=3; event_v.sq=2; tr.events.push_back(event_v);
        observer.observe_batch(tr);
    endfunction
    function rm_ls_program_item_t make_item(input bit is_zero);
        rm_ls_program_item_t item;
        item=new(0); item.valid=1; item.dynamic_epoch=4; item.rob='{flag:0,value:3};
        item.sq_flag=0; item.sq_value=2; item.pa_valid_mask=16'hff;
        for(int i=0;i<8;i++) item.pa_by_byte[i]=64'h80001000+i;
        item.op=is_zero ? MEMBLOCK_LSUOP_CBO_ZERO : MEMBLOCK_LSUOP_SD;
        item.kind=is_zero ? RM_LS_KIND_CMO : RM_LS_KIND_STORE;
        item.size_bytes=8; item.store_data=64'h8877665544332211; item.store_byte_mask=16'hff;
        item.cmo_op=is_zero ? RM_CMO_OP_ZERO : RM_CMO_OP_NONE;
        item.cmo_path=is_zero ? RM_CMO_PATH_UNCACHE_ZERO : RM_CMO_PATH_NONE;
        item.uncache_path=1;
        return item;
    endfunction
    // 抽象职责：软件构造与实际producer同形的冻结A、独立写接受和D完成事实，再交真实observer解析。
    function longint unsigned segment_case(input int index,input bit denied,input bit backend_accepted,
        input bit zero_data=1,input bit nc=0,input bit error_enable=0,input bit inverse_output_enable=0,input int unsigned consume_delay=3);
        mem_access_base_sequence::uncache_d_error_owner_t owners[mem_access_base_sequence::UNCACHE_D_ERROR_MAX_OWNERS];
        mem_access_base_sequence::uncache_d_error_record_t fact;
        mem_access_base_sequence::uncache_write_fact_t write_fact;
        cmo_path_agent_xaction tr;
        cmo_path_probe_t event_v;
        longint unsigned generation;
        longint unsigned sq_sample,a_sample,d_sample,consume_sample;
        longint unsigned address;
        longint unsigned value_v;
        foreach(owners[i]) owners[i]='0;
        sq_sample=10+index*10; a_sample=sq_sample+1; d_sample=sq_sample+2;
        consume_sample=d_sample+consume_delay;
        address=64'h80001000+8*index; value_v=zero_data ? 0 : 64'h8877665544332211;
        tr=batch(sq_sample); event_v='0; event_v.valid=1; event_v.kind=CMO_PATH_SQ_UNCACHE_REQUEST;
        event_v.owner_valid=1; event_v.rob=3; event_v.id=2; event_v.addr=address;
        event_v.data=512'(value_v); event_v.mask=8'hff; event_v.qual=16'(nc);
        tr.events.push_back(event_v);
        event_v.kind=CMO_PATH_UNCACHE_ALLOC; event_v.op=1; event_v.slot=1; event_v.qual=16'({nc,1'b0});
        tr.events.push_back(event_v); observer.observe_batch(tr);
        expect_true(mem_access_base_sequence::begin_uncache_d_error_record(0,1,48'(address),3,a_sample,0,owners,generation),
            "soft Store A allocation failed");
        expect_true(mem_access_base_sequence::read_uncache_response_at_accept(a_sample,1,fact),"soft A token absent");
        fact.access_kind=nc ? mem_access_base_sequence::UNCACHE_ACCESS_NC_STORE : mem_access_base_sequence::UNCACHE_ACCESS_MMIO_STORE;
        fact.nc=nc; fact.mask=8'hff; fact.request_data=value_v; fact.denied=denied;
        fact.backend_denied=!backend_accepted; fact.backend_write_accepted=backend_accepted;
        if(backend_accepted) begin
            write_fact='0; write_fact.valid=1; write_fact.token=fact.token;
            write_fact.address=48'(address); write_fact.mask=8'hff; write_fact.accept_sample=a_sample;
            write_fact.commit_sample=a_sample+1; write_fact.committed=1;
            mem_access_base_sequence::uncache_write_fact_by_token[write_fact.token]=write_fact;
            fact.write_event_token=write_fact.token;
        end
        fact.response_frozen=1;
        mem_access_base_sequence::freeze_uncache_response_record(fact);
        tr=batch(a_sample); event_v='0; event_v.valid=1; event_v.kind=CMO_PATH_UNCACHE_TL_A;
        event_v.id=1; event_v.op=0; event_v.addr=address; event_v.mask=8'hff;
        event_v.data=512'(value_v); event_v.state=3; event_v.qual=16'(nc);
        tr.events.push_back(event_v); observer.observe_batch(tr);
        mem_access_base_sequence::commit_uncache_d_error_record(fact,d_sample);
        tr=batch(d_sample); event_v='0; event_v.valid=1; event_v.kind=CMO_PATH_UNCACHE_TL_D;
        event_v.id=1; event_v.op=0; event_v.state=3; event_v.raw={denied,1'b0};
        tr.events.push_back(event_v); observer.observe_batch(tr);
        mem_access_base_sequence::uncache_history_sample_watermark=d_sample;
        observer.check_until(d_sample);
        expect_true(observer.expected_notification.size()==0,
            "external D generated notification before independent internal consumption");
        // 外D进入DUT buffer后由真实内部消费事件关联，同一token不假定固定buffer延迟。
        tr=batch(consume_sample); event_v='0; event_v.valid=1; event_v.kind=CMO_PATH_UNCACHE_CONSUME_D;
        event_v.id=1; event_v.raw={denied,1'b0}; event_v.addr={address[63:6],6'b0};
        event_v.qual={14'b0,1'b1,denied}; tr.events.push_back(event_v); observer.observe_batch(tr);
        memblock_sync_pkg::publish_cache_error_control(consume_sample,0,error_enable);
        if(inverse_output_enable) memblock_sync_pkg::publish_cache_error_control(consume_sample+2,0,!error_enable);
        mem_access_base_sequence::uncache_history_sample_watermark=consume_sample;
        observer.check_until(consume_sample);
        return fact.token;
    endfunction
    function void finish_watermark(input longint unsigned sample);
        cmo_path_agent_xaction tr;
        other_ctrl_agent_agent_xaction actual_v;
        tr=batch(sample); observer.observe_batch(tr);
        mem_access_base_sequence::uncache_history_sample_watermark=sample;
        actual_v=other_ctrl_agent_agent_xaction::type_id::create("uncache_soft_empty_notification");
        actual_v.sample_seq=sample; actual_v.reset_epoch=0; actual_v.reset_active=0;
        actual_v.io_uncacheError_ecc_error_valid=0;
        observer.observe_notification(actual_v); observer.check_until(sample);
    endfunction
    function void check_zero_success_and_partial();
        rm_ls_program_item_t item;
        string detail;
        longint unsigned token;
        setup(1);
        for(int i=0;i<8;i++) token=segment_case(i,0,1);
        finish_watermark(100); item=make_item(1);
        expect_true(observer.validate_cmo_zero(item,100,detail) && item.cmo_segment_write_mask==8'hff &&
            item.cmo_segment_sent_mask==8'hff && item.expected_exception==0,"zero successful 8 segments failed");
        expect_true(observer.error_count==0,"zero success observer errors"); observer.final_check(100);
        setup(1); token=segment_case(0,0,1); token=segment_case(1,0,1); token=segment_case(2,1,1);
        finish_watermark(40); item=make_item(1);
        expect_true(observer.validate_cmo_zero(item,40,detail) && item.cmo_segment_write_mask==8'h07 &&
            item.cmo_segment_sent_mask==8'h07 && item.expected_exception[7],"zero denied after accepted write lost partial24B");
        expect_true(observer.error_count==0,"zero injected-denied observer errors");
        setup(1); token=segment_case(0,0,1); token=segment_case(1,1,0);
        finish_watermark(30); item=make_item(1);
        expect_true(observer.validate_cmo_zero(item,30,detail) && item.cmo_segment_write_mask==8'h01 &&
            item.cmo_segment_completed_mask==8'h03 && item.expected_exception[7],"zero backend rejection incorrectly wrote failing segment");
    endfunction
    function void check_zero_stop_negative();
        rm_ls_program_item_t item;
        rm_uncache_expected_report_catcher catcher;
        string detail;
        longint unsigned token;
        setup(1); token=segment_case(0,1,1); token=segment_case(1,0,1); finish_watermark(30);
        item=make_item(1); catcher=new("RM_UNCACHE_ERROR"); uvm_report_cb::add(null,catcher);
        expect_true(!observer.validate_cmo_zero(item,30,detail) && !observer.observation_pending(),
            "zero accepted later segment after first error");
        uvm_report_cb::delete(null,catcher); expect_true(catcher.caught==1,"first-error stop negative not detected");
    endfunction
    function void check_zero_late_after_terminal_negative();
        rm_ls_program_item_t item;
        rm_uncache_expected_report_catcher catcher;
        cmo_path_agent_xaction tr;
        cmo_path_probe_t event_v;
        string detail;
        longint unsigned token;
        setup(1); token=segment_case(0,1,1); finish_watermark(20); item=make_item(1);
        expect_true(observer.validate_cmo_zero(item,20,detail),"one-error zero terminal was not accepted");
        catcher=new("RM_UNCACHE_ERROR"); uvm_report_cb::add(null,catcher);
        tr=batch(21); event_v='0; event_v.valid=1; event_v.kind=CMO_PATH_SQ_UNCACHE_REQUEST;
        event_v.owner_valid=1; event_v.rob=3; event_v.id=2; event_v.addr=64'h80001008; event_v.mask=8'hff;
        tr.events.push_back(event_v); observer.observe_batch(tr);
        uvm_report_cb::delete(null,catcher); expect_true(catcher.caught==1,
            "post-terminal late zero segment escaped continuous request audit");
    endfunction
    function void check_mmio_and_nc_scope();
        rm_ls_program_item_t item;
        rm_uncache_expected_report_catcher catcher;
        sbuffer_mem_access_base_sequence responder;
        string detail;
        longint unsigned token;
        setup(0); token=segment_case(0,1,1,0); finish_watermark(20); item=make_item(0);
        expect_true(observer.apply_store_expectation(item,20,detail) && item.expected_exception[7] &&
            item.uncache_effect_valid && item.uncache_write_accepted,"MMIO injected denied lost accepted data effect");
        setup(0); token=segment_case(0,1,0,0); finish_watermark(20); item=make_item(0);
        expect_true(observer.apply_store_expectation(item,20,detail) && item.expected_exception[7] &&
            item.uncache_effect_valid && !item.uncache_write_accepted,"MMIO backend rejected write still applied data");
        responder=sbuffer_mem_access_base_sequence::type_id::create("nc_scope_responder");
        catcher=new("UNCACHE_NC_BACKEND_SCOPE"); uvm_report_cb::add(null,catcher);
        responder.validate_uncache_backend_scope(1,1,9,48'h80001000,8'hff,1,0);
        uvm_report_cb::delete(null,catcher); expect_true(catcher.caught==1,"NC backend reject did not terminate before response");
    endfunction
    function void check_notification_delay_enable();
        longint unsigned token;
        other_ctrl_agent_agent_xaction actual_v;
        cmo_path_agent_xaction tr;
        setup(0); token=segment_case(0,1,1,0,1,1,1);
        expect_true(observer.expected_notification.size()==1 && observer.expected_notification[0].sample==17 &&
            observer.expected_notification[0].address==48'h80001000,
            "external D12/internal D15 enable1 did not predict internal D+2 notification");
        actual_v=other_ctrl_agent_agent_xaction::type_id::create("uncache_gate_notification");
        actual_v.sample_seq=17; actual_v.reset_epoch=0; actual_v.reset_active=0;
        actual_v.io_uncacheError_ecc_error_valid=1; actual_v.io_uncacheError_ecc_error_bits=48'h80001000;
        observer.observe_notification(actual_v); tr=batch(17); observer.observe_batch(tr);
        mem_access_base_sequence::uncache_history_sample_watermark=17; observer.check_until(17);
        expect_true(observer.error_count==0 && observer.expected_notification.size()==0 &&
            observer.actual_notification.size()==0,"two-cycle NC Store notification did not match");
        setup(0); token=segment_case(0,1,1,0,1,0,1); finish_watermark(20);
        expect_true(observer.expected_notification.size()==0 && observer.error_count==0,
            "enable0 created an Uncache notification expectation");
    endfunction
    function void check_variable_buffer_consumption();
        longint unsigned token;
        int unsigned delays[3]='{0,3,7};
        foreach(delays[i]) begin
            setup(0); token=segment_case(0,1,1,0,1,1,0,delays[i]);
            expect_true(observer.expected_notification.size()==1 &&
                observer.expected_notification[0].sample==14+delays[i],
                "buffer delay was guessed from external D instead of observed internal consumption");
        end
    endfunction
    function void check_source_reuse_and_reset();
        longint unsigned token0,token1;
        mem_access_base_sequence::uncache_d_error_record_t fact0,fact1;
        setup(1); token0=segment_case(0,0,1); token1=segment_case(1,1,1);
        expect_true(mem_access_base_sequence::read_uncache_response_by_token(token0,fact0) &&
            mem_access_base_sequence::read_uncache_response_by_token(token1,fact1) && token0!=token1 &&
            fact0.generation==1 && fact1.generation==2 && !fact0.denied && fact1.denied,
            "source reuse overwrote earlier Uncache response identity");
        mem_access_base_sequence::abort_uncache_d_error_pending("soft reset");
        expect_true(!mem_access_base_sequence::read_uncache_response_by_token(token0,fact0),
            "old reset token authorized a new Uncache generation");
    endfunction
    function void check_sq_reuse_same_sample();
        cmo_path_agent_xaction tr;
        cmo_path_probe_t event_v;
        rm_uncache_error_observer::owner_t old_owner,new_owner;
        setup(0); old_owner=observer.sq_owner[2]; new_owner=old_owner;
        new_owner.sq=7'h42; new_owner.uid=1; new_owner.epoch=5;
        // 历史相反flag故意保留相同ROB，真实新enqueue必须清掉它而不污染本拍旧请求。
        observer.sq_owner[7'h42]=new_owner;
        observer.sq_owner[7'h42].valid=0;
        data.main_table_by_uid[0].sqIdx_flag=1;
        tr=batch(10); event_v='0; event_v.valid=1; event_v.owner_valid=1; event_v.rob=3;
        event_v.kind=CMO_PATH_SQ_ENQUEUE; event_v.sq=7'h42; tr.events.push_back(event_v);
        event_v.kind=CMO_PATH_SQ_UNCACHE_REQUEST; event_v.id=2; event_v.addr=64'h80001000;
        event_v.mask=8'hff; event_v.data=64'h8877665544332211; tr.events.push_back(event_v);
        observer.observe_batch(tr);
        expect_true(observer.error_count==0 && observer.waiting_sq_ids.size()==1 &&
            observer.segment_by_id[observer.waiting_sq_ids[0]].owner.sq==2 &&
            !observer.sq_owner[2].valid && observer.sq_owner[7'h42].valid,
            "same-sample SQ reuse lost old request or retained opposite flag history");
        event_v.kind=CMO_PATH_SQ_UNCACHE_REQUEST; event_v.id=2; event_v.rob=3;
        expect_true(observer.resolve_sq_request_owner(event_v,new_owner) && new_owner.sq==7'h42,
            "ROB wrap SQ flag history created ambiguous new owner");
    endfunction
    function void check_merged_nc_one_notification();
        cmo_path_agent_xaction tr;
        cmo_path_probe_t event_v;
        other_ctrl_agent_agent_xaction actual_v;
        mem_access_base_sequence::uncache_d_error_owner_t owners[mem_access_base_sequence::UNCACHE_D_ERROR_MAX_OWNERS];
        mem_access_base_sequence::uncache_d_error_record_t fact;
        longint unsigned generation;
        setup(0);
        // 第二个owner也来自独立真实enqueue链，两个提交NC请求合并同一slot。
        data.main_trans_num=2; data.main_table_by_uid=new[2](data.main_table_by_uid);
        data.status_by_uid=new[2](data.status_by_uid);
        data.main_table_by_uid[1]=main_control_transaction::type_id::create("nc_merge_main1");
        data.status_by_uid[1]=status_transaction::type_id::create("nc_merge_status1"); data.status_by_uid[1].reset(1);
        data.main_table_by_uid[1].uid=1; data.main_table_by_uid[1].fuOpType=MEMBLOCK_LSUOP_SD;
        data.main_table_by_uid[1].robIdx_value=4; data.main_table_by_uid[1].sqIdx_value=3;
        data.status_by_uid[1].dynamic_epoch=4; data.uid_by_active_rob[9'd4]=1;
        tr=batch(2); event_v='0; event_v.valid=1; event_v.kind=CMO_PATH_SQ_ENQUEUE;
        event_v.owner_valid=1; event_v.rob=4; event_v.sq=3; tr.events.push_back(event_v); observer.observe_batch(tr);
        for(int i=0;i<2;i++) begin
            tr=batch(10+i); event_v='0; event_v.valid=1; event_v.kind=CMO_PATH_SQ_UNCACHE_REQUEST;
            event_v.owner_valid=1; event_v.rob=3+i; event_v.id=2+i; event_v.addr=64'h80001000;
            event_v.mask=i==0 ? 8'h0f : 8'hf0; event_v.data=64'h8877665544332211; event_v.qual=1;
            tr.events.push_back(event_v); event_v.kind=CMO_PATH_UNCACHE_ALLOC; event_v.op=1;
            event_v.slot=1; event_v.qual=16'({1'b1,1'(i!=0)}); tr.events.push_back(event_v); observer.observe_batch(tr);
        end
        foreach(owners[i]) owners[i]='0;
        expect_true(mem_access_base_sequence::begin_uncache_d_error_record(0,1,48'h80001000,3,12,0,owners,generation),
            "merged NC producer A failed");
        expect_true(mem_access_base_sequence::read_uncache_response_at_accept(12,1,fact),"merged NC token absent");
        fact.access_kind=mem_access_base_sequence::UNCACHE_ACCESS_NC_STORE; fact.nc=1; fact.mask=8'hff;
        fact.request_data=64'h8877665544332211; fact.denied=1; fact.backend_write_accepted=1; fact.response_frozen=1;
        mem_access_base_sequence::freeze_uncache_response_record(fact);
        tr=batch(12); event_v='0; event_v.valid=1; event_v.kind=CMO_PATH_UNCACHE_TL_A;
        event_v.id=1; event_v.op=0; event_v.addr=64'h80001000; event_v.mask=8'hff;
        event_v.data=64'h8877665544332211; event_v.state=3; event_v.qual=1;
        tr.events.push_back(event_v); observer.observe_batch(tr);
        mem_access_base_sequence::commit_uncache_d_error_record(fact,13);
        tr=batch(13); event_v='0; event_v.valid=1; event_v.kind=CMO_PATH_UNCACHE_TL_D;
        event_v.id=1; event_v.op=0; event_v.raw=2'b10; event_v.state=3;
        tr.events.push_back(event_v); observer.observe_batch(tr);
        tr=batch(16); event_v='0; event_v.valid=1; event_v.kind=CMO_PATH_UNCACHE_CONSUME_D;
        event_v.id=1; event_v.raw=2'b10; event_v.addr=64'h80001000; event_v.qual=3;
        tr.events.push_back(event_v); observer.observe_batch(tr);
        memblock_sync_pkg::publish_cache_error_control(16,0,1);
        mem_access_base_sequence::uncache_history_sample_watermark=16; observer.check_until(16);
        expect_true(observer.expected_notification.size()==1 && observer.segments_by_owner.size()==2,
            "two merged NC owners created duplicate bus notifications");
        actual_v=other_ctrl_agent_agent_xaction::type_id::create("merged_nc_notification");
        actual_v.sample_seq=18; actual_v.io_uncacheError_ecc_error_valid=1;
        actual_v.io_uncacheError_ecc_error_bits=48'h80001000; observer.observe_notification(actual_v);
        tr=batch(18); observer.observe_batch(tr); mem_access_base_sequence::uncache_history_sample_watermark=18;
        observer.check_until(18); expect_true(observer.error_count==0,"merged NC one-response notification mismatched");
    endfunction
    function void check_notification_events();
        rm_uncache_error_observer observer_v;
        rm_uncache_error_observer::notification_t expected_v;
        other_ctrl_agent_agent_xaction actual_v;
        rm_uncache_expected_report_catcher catcher;
        observer_v=rm_uncache_error_observer::type_id::create("notification_soft_observer");
        observer_v.path_watermark=100; mem_access_base_sequence::uncache_history_sample_watermark=100;
        for(int i=0;i<2;i++) begin
            expected_v='{sample:50+i,reset_epoch:0,address:48'h80001000}; observer_v.expected_notification.push_back(expected_v);
            actual_v=other_ctrl_agent_agent_xaction::type_id::create("notification_duplicate_address");
            actual_v.sample_seq=50+i; actual_v.reset_epoch=0; actual_v.reset_active=0;
            actual_v.io_uncacheError_ecc_error_valid=1; actual_v.io_uncacheError_ecc_error_bits=48'h80001000;
            observer_v.observe_notification(actual_v);
        end
        observer_v.check_until(51); expect_true(observer_v.error_count==0 && observer_v.actual_notification.size()==0,
            "repeated same-address notification was deduplicated");
        catcher=new("RM_UNCACHE_ERROR"); uvm_report_cb::add(null,catcher);
        expected_v='{sample:52,reset_epoch:0,address:48'h80001000}; observer_v.expected_notification.push_back(expected_v);
        observer_v.notification_watermark=52; observer_v.check_until(52);
        actual_v.sample_seq=53; observer_v.observe_notification(actual_v); observer_v.check_until(53);
        expected_v='{sample:54,reset_epoch:0,address:48'h80001000}; observer_v.expected_notification.push_back(expected_v);
        actual_v.sample_seq=54; actual_v.io_uncacheError_ecc_error_bits=48'h80001040;
        observer_v.observe_notification(actual_v); observer_v.check_until(54);
        uvm_report_cb::delete(null,catcher); expect_true(catcher.caught==3,"missing/extra/wrong-address notification negatives lost");
    endfunction
    function void run();
        check_zero_success_and_partial(); check_zero_stop_negative(); check_zero_late_after_terminal_negative(); check_mmio_and_nc_scope(); check_notification_delay_enable(); check_variable_buffer_consumption(); check_source_reuse_and_reset(); check_sq_reuse_same_sample();
        check_merged_nc_one_notification(); check_notification_events();
        `uvm_info("RM_UNCACHE_ERROR_SOFT",$sformatf("%0d checks PASS",checks),UVM_LOW)
    endfunction
endclass
`endif
