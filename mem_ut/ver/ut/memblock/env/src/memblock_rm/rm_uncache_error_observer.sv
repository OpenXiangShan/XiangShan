`ifndef RM_UNCACHE_ERROR_OBSERVER_SV
`define RM_UNCACHE_ERROR_OBSERVER_SV
// 中文注释：唯一Uncache Store被动关联与通知比较器。只读框架事实，不驱动接口，不推进程序UID。
class rm_uncache_error_observer extends uvm_object;
    `uvm_object_utils(rm_uncache_error_observer)
    typedef struct packed {
        bit valid;
        memblock_uid_t uid;
        int unsigned epoch;
        bit [8:0] rob;
        bit [6:0] sq;
        bit [8:0] op;
    } owner_t;
    typedef bit [63:0] owner_key_t;
    typedef longint unsigned segment_id_t;
    typedef segment_id_t segment_ids_t[$];
    typedef struct {
        owner_t owner;
        mem_access_base_sequence::uncache_access_kind_e access_kind;
        cmo_path_probe_t request;
        longint unsigned sq_sample;
        longint unsigned alloc_sample;
        longint unsigned token;
        longint unsigned a_sample;
        longint unsigned d_sample;
        longint unsigned consume_sample;
        bit failed;
        string error;
    } segment_t;
    typedef struct {
        cmo_path_probe_t event_v;
        longint unsigned sample;
        segment_ids_t segment_ids;
    } tl_event_t;
    typedef struct packed {
        longint unsigned sample;
        int unsigned reset_epoch;
        bit [47:0] address;
    } notification_t;
    owner_t sq_owner[128];
    segment_t segment_by_id[segment_id_t];
    segment_ids_t segments_by_owner[owner_key_t];
    // 中文注释：已由RM确认终态的owner禁止任何迟到请求；正常重复查询不重复提交数据。
    longint unsigned terminal_sample_by_owner[owner_key_t];
    segment_ids_t slot_segments[16];
    segment_ids_t active_source_segments[16];
    // 中文注释：外D已实际接受，但尚未穿过DUT内TLBuffer到达Uncache；只按source原token关联。
    segment_ids_t delivered_source_segments[16];
    segment_ids_t waiting_sq_ids;
    tl_event_t pending_tl[$];
    notification_t expected_notification[$];
    notification_t actual_notification[$];
    bit notification_checked_by_token[longint unsigned];
    longint unsigned next_segment;
    longint unsigned path_watermark;
    longint unsigned notification_watermark;
    longint unsigned reset_generation;
    int unsigned notification_reset_epoch;
    bit path_reset_active;
    bit notification_reset_active;
    int unsigned error_count;
    bit query_pending;

    function new(string name="rm_uncache_error_observer");
        super.new(name);
        next_segment=1; path_watermark=0; notification_watermark=0;
        reset_generation=0; notification_reset_epoch=0; error_count=0;
        path_reset_active=1; notification_reset_active=1; query_pending=0;
        foreach(sq_owner[i]) sq_owner[i]='0;
    endfunction
    function bit observation_pending();
        return query_pending;
    endfunction
    function owner_key_t owner_key(input owner_t owner);
        return {32'(owner.uid),32'(owner.epoch)};
    endfunction
    function void fail(input string detail);
        error_count++;
        `uvm_error("RM_UNCACHE_ERROR",detail)
    endfunction
    function void reset_runtime();
        foreach(sq_owner[i]) sq_owner[i]='0;
        foreach(slot_segments[i]) begin slot_segments[i].delete(); active_source_segments[i].delete(); delivered_source_segments[i].delete(); end
        waiting_sq_ids.delete(); pending_tl.delete(); expected_notification.delete(); actual_notification.delete();
        segment_by_id.delete(); segments_by_owner.delete(); terminal_sample_by_owner.delete(); notification_checked_by_token.delete();
        next_segment=1;
    endfunction
    // 抽象职责：在真实SQ enqueue时冻结owner，NC提交后ROB映射消失仍使用该接受身份。
    function bit capture_sq_owner(input cmo_path_probe_t e, output owner_t owner);
        memblock_rm_readonly_api ro;
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        memblock_uid_t uid;
        owner='0; ro=memblock_rm_readonly_api::get();
        if(!e.owner_valid || !ro.try_read_uid_by_rob_for_rm('{flag:e.rob[8],value:e.rob[7:0]},uid) ||
           !ro.read_main_transaction_for_rm(uid,main_v) || !ro.read_status_for_rm(uid,status_v)) return 0;
        if({main_v.rob_idx_flag,main_v.rob_idx_value}!=e.rob ||
           {main_v.sq_idx_flag,main_v.sq_idx_value}!=e.sq) return 0;
        owner='{valid:1,uid:uid,epoch:status_v.dynamic_epoch,rob:e.rob,sq:e.sq,op:main_v.fu_op_type};
        return 1;
    endfunction
    function bit resolve_sq_request_owner(input cmo_path_probe_t e, output owner_t owner);
        int match_count;
        owner='0; match_count=0;
        // SQ id省略flag，仅检查常数两个完整SQ候选，禁止扫描主表或按地址猜owner。
        for(int flag_i=0;flag_i<2;flag_i++) begin
            int idx;
            idx={1'(flag_i),e.id[5:0]};
            if(sq_owner[idx].valid && sq_owner[idx].rob==e.rob) begin
                owner=sq_owner[idx]; match_count++;
            end
        end
        return match_count==1;
    endfunction
    // 抽象职责：固定批次按enqueue、SQ接受、真实slot分配、TL接受/完成的因果顺序解析。
    function void observe_batch(input cmo_path_agent_xaction tr);
        owner_t owner;
        owner_t enqueue_owner[6];
        bit [5:0] enqueue_valid;
        segment_t segment;
        segment_id_t id_v;
        owner_key_t key;
        tl_event_t event_v;
        if(tr==null) return;
        if(tr.reset_active) begin
            if(!path_reset_active || reset_generation!=tr.reset_generation) reset_runtime();
            path_reset_active=1; reset_generation=tr.reset_generation;
            path_watermark=tr.sample_seq; return;
        end
        path_reset_active=0; reset_generation=tr.reset_generation;
        enqueue_valid=0;
        foreach(tr.events[i]) if(tr.events[i].kind==CMO_PATH_SQ_ENQUEUE) begin
            if(tr.events[i].port_id>=6 || !capture_sq_owner(tr.events[i],owner))
                fail("SQ enqueue缺少可冻结的完整owner或物理port越界");
            else begin enqueue_owner[tr.events[i].port_id]=owner; enqueue_valid[tr.events[i].port_id]=1; end
        end
        // 中文注释：本拍旧请求先消费上一代SQ快照；当前enqueue在下方原子替换同value旧flag。
        // 后续slot/TL只保存已复制owner，不依赖已复用SQ历史。
        foreach(tr.events[i]) if(tr.events[i].kind==CMO_PATH_SQ_UNCACHE_REQUEST) begin
            if(!resolve_sq_request_owner(tr.events[i],owner)) begin
                fail("SQ->Uncache接受缺少同ROB/SQ冻结owner"); continue;
            end
            key=owner_key(owner);
            if(terminal_sample_by_owner.exists(key)) begin
                fail($sformatf("已终态Uncache owner收到迟到SQ请求 uid=%0d epoch=%0d terminal=%0d request=%0d",
                    owner.uid,owner.epoch,terminal_sample_by_owner[key],tr.sample_seq)); continue;
            end
            if(segments_by_owner.exists(key) &&
               segments_by_owner[key].size()>=(owner.op==MEMBLOCK_LSUOP_CBO_ZERO ? 8 : 1)) begin
                fail("同一owner的Uncache请求数超过合法边界"); continue;
            end
            segment.owner=owner;
            segment.access_kind=owner.op==MEMBLOCK_LSUOP_CBO_ZERO ? mem_access_base_sequence::UNCACHE_ACCESS_CMO_ZERO :
                (tr.events[i].qual[0] ? mem_access_base_sequence::UNCACHE_ACCESS_NC_STORE : mem_access_base_sequence::UNCACHE_ACCESS_MMIO_STORE);
            segment.request=tr.events[i]; segment.sq_sample=tr.sample_seq;
            segment.alloc_sample=0; segment.token=0; segment.a_sample=0; segment.d_sample=0; segment.consume_sample=0;
            segment.failed=0; segment.error="";
            id_v=next_segment++; segment_by_id[id_v]=segment;
            segments_by_owner[key].push_back(id_v); waiting_sq_ids.push_back(id_v);
        end
        foreach(enqueue_owner[i]) if(enqueue_valid[i]) begin
            sq_owner[enqueue_owner[i].sq ^ 7'h40]='0;
            sq_owner[enqueue_owner[i].sq]=enqueue_owner[i];
        end
        foreach(tr.events[i]) if(tr.events[i].kind==CMO_PATH_UNCACHE_ALLOC && tr.events[i].op==9'd1) begin
            int match_idx;
            int match_count;
            cmo_path_probe_t e;
            e=tr.events[i]; match_idx=-1; match_count=0;
            if(e.slot>=16) begin fail("Uncache分配slot越界"); continue; end
            foreach(waiting_sq_ids[j]) begin
                segment=segment_by_id[waiting_sq_ids[j]];
                if(segment.request.id==e.id && segment.sq_sample<=tr.sample_seq) begin match_idx=j; match_count++; end
            end
            if(match_count!=1) begin fail("Uncache Store分配缺少唯一SQ接受记录"); continue; end
            id_v=waiting_sq_ids[match_idx]; waiting_sq_ids.delete(match_idx); segment=segment_by_id[id_v];
            if(segment.request.addr!=e.addr || segment.request.data[63:0]!=e.data[63:0] ||
               segment.request.mask[7:0]!=e.mask[7:0] || segment.request.qual[0]!=e.qual[1]) begin
                segment.failed=1; segment.error="SQ->Uncache分配payload或nc错配"; fail(segment.error);
            end
            if(!e.qual[0]) begin
                if(slot_segments[e.slot].size()!=0) fail("Uncache新分配覆盖尚未发出的slot owner");
                slot_segments[e.slot].delete();
            end else if(slot_segments[e.slot].size()==0) begin
                fail("Uncache merge缺少原slot generation");
            end
            segment.alloc_sample=tr.sample_seq; segment_by_id[id_v]=segment;
            if(slot_segments[e.slot].size()>=128) fail("Uncache slot合并owner超过物理SQ窗口");
            else slot_segments[e.slot].push_back(id_v);
        end
        foreach(tr.events[i]) if(tr.events[i].kind==CMO_PATH_UNCACHE_TL_A && tr.events[i].op!=9'd4) begin
            cmo_path_probe_t e;
            e=tr.events[i];
            if(e.id>=16 || slot_segments[e.id].size()==0) begin fail("Uncache Store TL A缺少真实slot分配链"); continue; end
            if(active_source_segments[e.id].size()!=0) begin fail("Uncache TL source复用时旧响应未完成"); continue; end
            event_v.event_v=e; event_v.sample=tr.sample_seq; event_v.segment_ids=slot_segments[e.id];
            active_source_segments[e.id]=slot_segments[e.id]; slot_segments[e.id].delete(); pending_tl.push_back(event_v);
        end
        foreach(tr.events[i]) if(tr.events[i].kind==CMO_PATH_UNCACHE_TL_D && tr.events[i].op==9'd0) begin
            cmo_path_probe_t e;
            e=tr.events[i];
            if(e.id>=16 || active_source_segments[e.id].size()==0) begin fail("Uncache Store D缺少真实A source owner"); continue; end
            if(delivered_source_segments[e.id].size()!=0) fail("外D source仍有尚未被Uncache内部消费的旧generation");
            delivered_source_segments[e.id]=active_source_segments[e.id];
            event_v.event_v=e; event_v.sample=tr.sample_seq; event_v.segment_ids=active_source_segments[e.id];
            active_source_segments[e.id].delete(); pending_tl.push_back(event_v);
        end
        foreach(tr.events[i]) if(tr.events[i].kind==CMO_PATH_UNCACHE_CONSUME_D) begin
            cmo_path_probe_t e;
            e=tr.events[i];
            if(e.id>=16) begin fail("Uncache内部D消费source越界"); continue; end
            if(!e.qual[1]) begin
                if(delivered_source_segments[e.id].size()!=0) fail("外D Store generation在内部被错误识别为Load");
                continue;
            end
            if(delivered_source_segments[e.id].size()==0) begin
                fail("Uncache内部Store D消费缺少外D已经送达的source generation"); continue;
            end
            event_v.event_v=e; event_v.sample=tr.sample_seq; event_v.segment_ids=delivered_source_segments[e.id];
            delivered_source_segments[e.id].delete(); pending_tl.push_back(event_v);
        end
        path_watermark=tr.sample_seq;
    endfunction
    // 抽象职责：逐拍保留实际valid/bits和水位，相同地址连续两拍是独立事件。
    function void observe_notification(input other_ctrl_agent_agent_xaction tr);
        notification_t event_v;
        if(tr==null) return;
        if(tr.reset_active) begin
            if(!notification_reset_active || notification_reset_epoch!=tr.reset_epoch) begin
                actual_notification.delete(); expected_notification.delete();
            end
            notification_reset_active=1; notification_reset_epoch=tr.reset_epoch;
            notification_watermark=tr.sample_seq; return;
        end
        notification_reset_active=0; notification_reset_epoch=tr.reset_epoch;
        if(tr.io_uncacheError_ecc_error_valid) begin
            event_v='{sample:tr.sample_seq,reset_epoch:tr.reset_epoch,address:tr.io_uncacheError_ecc_error_bits};
            actual_notification.push_back(event_v);
        end
        notification_watermark=tr.sample_seq;
    endfunction
    // 抽象职责：在生产者与被动水位均覆盖后核对A/D事实，构造两拍延迟通知并逐事件比较。
    function void check_until(input longint unsigned sample);
        memblock_rm_readonly_api ro;
        mem_access_base_sequence::uncache_d_error_record_t fact;
        memblock_sync_pkg::memblock_cache_error_control_t ctrl;
        tl_event_t event_v;
        segment_t segment;
        notification_t expected_v,actual_v;
        longint unsigned limit;
        longint unsigned token;
        ro=memblock_rm_readonly_api::get();
        limit=sample;
        if(path_watermark<limit) limit=path_watermark;
        if(mem_access_base_sequence::uncache_history_sample_watermark<limit)
            limit=mem_access_base_sequence::uncache_history_sample_watermark;
        while(pending_tl.size()!=0 && pending_tl[0].sample<=limit) begin
            event_v=pending_tl[0];
            if(event_v.event_v.kind==CMO_PATH_UNCACHE_TL_A) begin
                if(!ro.read_uncache_response_at_accept_for_rm(event_v.sample,4'(event_v.event_v.id),fact)) begin
                    fail($sformatf("真实Uncache A缺少独立框架token sample=%0d source=%0d producer_watermark=%0d reset_epoch=%0d path_reset=%0d",
                        event_v.sample,event_v.event_v.id,mem_access_base_sequence::uncache_history_sample_watermark,
                        mem_access_base_sequence::uncache_reset_epoch,reset_generation)); pending_tl.pop_front(); continue;
                end
                if(fact.is_load || fact.address!=event_v.event_v.addr[47:0] ||
                   fact.mask!=event_v.event_v.mask[7:0] || fact.size!=event_v.event_v.state[2:0] ||
                   fact.request_data!=event_v.event_v.data[63:0] || fact.nc!=event_v.event_v.qual[0])
                    fail("Uncache A监测与框架冻结响应身份/payload不一致");
                foreach(event_v.segment_ids[i]) begin
                    segment=segment_by_id[event_v.segment_ids[i]];
                    segment.token=fact.token; segment.a_sample=event_v.sample;
                    if(segment.request.addr[63:3]!=event_v.event_v.addr[63:3] ||
                       (segment.request.mask[7:0] & fact.mask)!=segment.request.mask[7:0]) begin
                        segment.failed=1; segment.error="合并后的TL A不覆盖原SQ Store字节"; fail(segment.error);
                    end
                    if(segment.owner.op==MEMBLOCK_LSUOP_CBO_ZERO && fact.nc) begin
                        segment.failed=1; segment.error="Uncache zero错误归为已提交NC路径"; fail(segment.error);
                    end
                    segment_by_id[event_v.segment_ids[i]]=segment;
                end
            end else if(event_v.event_v.kind==CMO_PATH_UNCACHE_TL_D) begin
                segment=segment_by_id[event_v.segment_ids[0]]; token=segment.token;
                if(token==0 || !ro.read_uncache_response_by_token_for_rm(token,fact) || fact.d_sample!=event_v.sample) begin
                    fail($sformatf("真实Uncache Store D缺少同token完成事实 sample=%0d source=%0d token=%0d producer_watermark=%0d",
                        event_v.sample,event_v.event_v.id,token,mem_access_base_sequence::uncache_history_sample_watermark));
                    pending_tl.pop_front(); continue;
                end
                if(fact.source!=event_v.event_v.id || {fact.denied,fact.corrupt}!==event_v.event_v.raw ||
                    fact.size!=event_v.event_v.state[2:0]) fail("Uncache D raw/source/size与冻结响应不一致");
                foreach(event_v.segment_ids[i]) begin
                    segment=segment_by_id[event_v.segment_ids[i]];
                    if(segment.token!=token) fail("同一Uncache D关联多个不一致token");
                    segment.d_sample=event_v.sample; segment_by_id[event_v.segment_ids[i]]=segment;
                end
            end else begin
                segment=segment_by_id[event_v.segment_ids[0]]; token=segment.token;
                if(token==0 || !ro.read_uncache_response_by_token_for_rm(token,fact) || fact.d_sample==0 ||
                   fact.d_sample>event_v.sample || fact.source!=event_v.event_v.id || fact.is_load ||
                   {fact.denied,fact.corrupt}!==event_v.event_v.raw) begin
                    fail("Uncache内部D消费与外D冻结token/raw/source不一致"); pending_tl.pop_front(); continue;
                end
                if(event_v.event_v.qual[0]!==(fact.denied || fact.corrupt) ||
                   ((fact.denied || fact.corrupt) && event_v.event_v.addr[47:0]!=={fact.address[47:6],6'b0}))
                    fail("Uncache内部busError资格或行地址与独立响应事实不一致");
                foreach(event_v.segment_ids[i]) begin
                    segment=segment_by_id[event_v.segment_ids[i]];
                    if(segment.token!=token || segment.d_sample==0 || segment.d_sample>event_v.sample)
                        fail("Uncache内部消费缺少相同generation外D完成");
                    segment.consume_sample=event_v.sample; segment_by_id[event_v.segment_ids[i]]=segment;
                end
                if(!notification_checked_by_token.exists(token)) begin
                    // 顶层错误和CSR输入均延迟2拍，门控读取内部busError生成sample的CSR输入。
                    if(!memblock_sync_pkg::read_cache_error_control(event_v.sample,ctrl)) break;
                    if((fact.denied || fact.corrupt) && ctrl.enable) begin
                        expected_v='{sample:event_v.sample+2,reset_epoch:ctrl.reset_epoch,
                            address:{fact.address[47:6],6'b0}};
                        expected_notification.push_back(expected_v);
                    end
                    notification_checked_by_token[token]=1;
                end
            end
            pending_tl.pop_front();
        end
        if(pending_tl.size()!=0 && pending_tl[0].event_v.kind==CMO_PATH_UNCACHE_CONSUME_D &&
           pending_tl[0].sample+1<limit) limit=pending_tl[0].sample+1;
        if(notification_watermark<limit) limit=notification_watermark;
        // 两条FIFO都按真实sample递增，不以地址去重，不扫描累计历史。
        while((expected_notification.size()!=0 && expected_notification[0].sample<=limit) ||
              (actual_notification.size()!=0 && actual_notification[0].sample<=limit)) begin
            if(expected_notification.size()==0 || expected_notification[0].sample>limit) begin
                actual_v=actual_notification.pop_front(); fail($sformatf("额外Uncache通知 sample=%0d address=0x%0h",actual_v.sample,actual_v.address));
            end else if(actual_notification.size()==0 || actual_notification[0].sample>limit) begin
                expected_v=expected_notification.pop_front(); fail($sformatf("缺失Uncache通知 sample=%0d address=0x%0h",expected_v.sample,expected_v.address));
            end else begin
                expected_v=expected_notification.pop_front(); actual_v=actual_notification.pop_front();
                if(actual_v!==expected_v) fail($sformatf("Uncache通知错配 expected=%0d/0x%0h actual=%0d/0x%0h",
                    expected_v.sample,expected_v.address,actual_v.sample,actual_v.address));
            end
        end
    endfunction
    // 抽象职责：前置异常或其它CMO路径必须没有该owner的Uncache数据请求。
    function bit validate_no_cmo_requests(input rm_ls_program_item_t item,
        input longint unsigned completion_sample, output string detail);
        owner_key_t key;
        detail=""; query_pending=0;
        if(item==null) begin detail="CMO item为空"; return 0; end
        check_until(completion_sample);
        if(path_watermark<completion_sample || mem_access_base_sequence::uncache_history_sample_watermark<completion_sample) begin
            query_pending=1; detail="等待Uncache请求观察水位"; return 0;
        end
        key={32'(item.uid),32'(item.dynamic_epoch)};
        if(segments_by_owner.exists(key) && segments_by_owner[key].size()!=0) begin
            detail="CMO前置异常或非Uncache路径产生了非法Uncache数据请求"; return 0;
        end
        if(!terminal_sample_by_owner.exists(key)) terminal_sample_by_owner[key]=completion_sample;
        return 1;
    endfunction

    // 抽象职责：验证一个owner最多8段连续请求及首错停止，从独立后端接受事实形成zero效果。
    function bit validate_cmo_zero(input rm_ls_program_item_t item,
        input longint unsigned completion_sample, output string detail);
        owner_key_t key;
        segment_t segment;
        mem_access_base_sequence::uncache_d_error_record_t fact;
        mem_access_base_sequence::uncache_write_fact_t write_fact;
        memblock_rm_readonly_api ro;
        bit first_error;
        longint unsigned line;
        longint unsigned previous_d;
        detail=""; query_pending=0;
        if(item==null) begin detail="zero item为空"; return 0; end
        check_until(completion_sample);
        if(path_watermark<completion_sample || mem_access_base_sequence::uncache_history_sample_watermark<completion_sample) begin
            query_pending=1; detail="等待Uncache被动/响应水位"; return 0;
        end
        key={32'(item.uid),32'(item.dynamic_epoch)}; ro=memblock_rm_readonly_api::get();
        item.cmo_segment_sent_mask=0; item.cmo_segment_completed_mask=0; item.cmo_segment_write_mask=0;
        item.expected_uncache_denied=0; item.expected_uncache_corrupt=0;
        if(!segments_by_owner.exists(key) || segments_by_owner[key].size()==0) begin detail="zero缺少真实Uncache分段请求"; return 0; end
        line={item.pa_by_byte[0][63:6],6'b0}; first_error=0; previous_d=0;
        foreach(segments_by_owner[key][i]) begin
            segment=segment_by_id[segments_by_owner[key][i]];
            if(i>=8 || segment.access_kind!=mem_access_base_sequence::UNCACHE_ACCESS_CMO_ZERO ||
               segment.owner.rob!={item.rob.flag,8'(item.rob.value)} ||
               segment.owner.sq!={item.sq_flag,item.sq_value} || segment.failed || first_error ||
               segment.request.addr!=line+8*i || segment.request.mask[7:0]!=8'hff ||
               segment.request.data[63:0]!=0 || segment.request.qual[0] ||
               segment.sq_sample>completion_sample || (i!=0 && segment.sq_sample<=previous_d)) begin
                detail="zero分段owner/连续地址/8B零写/首错停止错误"; fail(detail); return 0;
            end
            if(segment.token==0 || !ro.read_uncache_response_by_token_for_rm(segment.token,fact) ||
               fact.d_sample==0 || fact.d_sample>completion_sample || segment.d_sample!=fact.d_sample ||
               segment.consume_sample==0 || segment.consume_sample>completion_sample) begin
                detail="zero分段独立完成事实缺失或晚于指令完成"; return 0;
            end
            if(fact.is_load || fact.nc || fact.address!=segment.request.addr[47:0] || fact.size!=3 || fact.mask!=8'hff) begin
                detail="zero分段协议身份或size错误"; fail(detail); return 0;
            end
            item.cmo_segment_sent_mask[i]=1; item.cmo_segment_completed_mask[i]=1;
            if(fact.backend_write_accepted) begin
                if(!ro.read_uncache_write_fact_for_rm(fact.write_event_token,write_fact) || !write_fact.committed) begin
                    detail="zero已接受写批次未生效"; return 0;
                end
                if(write_fact.address!=fact.address || write_fact.mask!=fact.mask || write_fact.commit_sample>completion_sample) begin
                    detail="zero写事件身份或生效时间错误"; fail(detail); return 0;
                end
                item.cmo_segment_write_mask[i]=1;
            end
            first_error=fact.denied || fact.corrupt; previous_d=segment.consume_sample;
            if(first_error) begin
                item.expected_uncache_denied=fact.denied; item.expected_uncache_corrupt=fact.corrupt;
                if(fact.denied) item.expected_exception[7]=1;
                else if(fact.corrupt) item.expected_exception[19]=1;
            end
        end
        if(!first_error && segments_by_owner[key].size()!=8) begin detail="正常zero未完成8段"; fail(detail); return 0; end
        item.cmo_effect_valid=1; item.cmo_requests_validated=1;
        item.cmo_request_count=segments_by_owner[key].size(); item.cmo_request_watermark=completion_sample;
        item.cmo_request_token=segment.token;
        if(!terminal_sample_by_owner.exists(key)) terminal_sample_by_owner[key]=completion_sample;
        return 1;
    endfunction
    // 抽象职责：同步MMIO Store接入同一响应与写接受事实，NC仅由独立后台通知检查。
    function bit apply_store_expectation(input rm_ls_program_item_t item,
        input longint unsigned completion_sample, output string detail);
        owner_key_t key;
        segment_t segment;
        mem_access_base_sequence::uncache_d_error_record_t fact;
        mem_access_base_sequence::uncache_write_fact_t write_fact;
        memblock_rm_readonly_api ro;
        detail=""; query_pending=0;
        if(item==null) return 0;
        check_until(completion_sample);
        if(path_watermark<completion_sample || mem_access_base_sequence::uncache_history_sample_watermark<completion_sample) begin
            query_pending=1; detail="等待Store被动/响应水位"; return 0;
        end
        ro=memblock_rm_readonly_api::get();
        key={32'(item.uid),32'(item.dynamic_epoch)};
        if(!segments_by_owner.exists(key) || segments_by_owner[key].size()!=1) begin detail="Store缺少唯一真实Uncache请求"; return 0; end
        segment=segment_by_id[segments_by_owner[key][0]];
        if(segment.failed || segment.owner.rob!={item.rob.flag,8'(item.rob.value)} ||
            segment.owner.sq!={item.sq_flag,item.sq_value}) begin detail="Uncache Store owner错配"; fail(detail); return 0; end
        if(segment.token==0 || !ro.read_uncache_response_by_token_for_rm(segment.token,fact) ||
           fact.d_sample==0 || fact.d_sample>completion_sample || segment.d_sample!=fact.d_sample ||
               segment.consume_sample==0 || segment.consume_sample>completion_sample) begin
            detail="Store响应独立事实缺失或晚于完成"; return 0;
        end
        if(fact.nc) begin detail="同步MMIO Store实际被标记为NC"; fail(detail); return 0; end
        begin
            bit [7:0] expected_mask;
            bit [63:0] expected_data;
            expected_mask=0; expected_data=0;
            if(item.size_bytes==0 || item.size_bytes>8 || !item.pa_valid_mask[0] ||
               segment.request.addr!=item.pa_by_byte[0] || fact.address!=item.pa_by_byte[0][47:0]) begin
                detail="MMIO Store地址/大小与独立翻译结果不一致"; fail(detail); return 0;
            end
            for(int i=0;i<item.size_bytes;i++) if(item.store_byte_mask[i]) begin
                if(!item.pa_valid_mask[i] || item.pa_by_byte[i][63:3]!=item.pa_by_byte[0][63:3]) begin
                    detail="MMIO Store物理字节不在同一8B请求"; fail(detail); return 0;
                end
                expected_mask[item.pa_by_byte[i][2:0]]=1;
                expected_data[8*item.pa_by_byte[i][2:0]+:8]=item.store_data[8*i+:8];
            end
            if(segment.request.mask[7:0]!=expected_mask || fact.mask!=expected_mask) begin
                detail="MMIO Store实际字节mask与独立指令期望不一致"; fail(detail); return 0;
            end
            for(int i=0;i<8;i++) if(expected_mask[i] &&
                (segment.request.data[8*i+:8]!==expected_data[8*i+:8] ||
                 fact.request_data[8*i+:8]!==expected_data[8*i+:8])) begin
                detail="MMIO Store实际写数据与独立指令期望不一致"; fail(detail); return 0;
            end
        end
        item.expected_uncache_denied=fact.denied; item.expected_uncache_corrupt=fact.corrupt;
        if(fact.denied) item.expected_exception[7]=1;
        else if(fact.corrupt) item.expected_exception[19]=1;
        item.uncache_effect_valid=1; item.uncache_write_accepted=fact.backend_write_accepted;
        if(fact.backend_write_accepted &&
           (!ro.read_uncache_write_fact_for_rm(fact.write_event_token,write_fact) || !write_fact.committed)) begin
            detail="MMIO写批次未生效"; return 0;
        end
        if(fact.backend_write_accepted && (write_fact.address[47:3]!=fact.address[47:3] ||
           write_fact.mask!=fact.mask || write_fact.commit_sample>completion_sample)) begin
            detail="MMIO写事件地址/mask/生效时间错配"; fail(detail); return 0;
        end
        if(!terminal_sample_by_owner.exists(key)) terminal_sample_by_owner[key]=completion_sample;
        return 1;
    endfunction
    function void final_check(input longint unsigned sample);
        check_until(sample);
        if(pending_tl.size()!=0 || expected_notification.size()!=0 || actual_notification.size()!=0 || waiting_sq_ids.size()!=0)
            fail("Uncache结束检查仍有未解析协议/通知/分配事实");
        foreach(slot_segments[i])
            if(slot_segments[i].size()!=0 || active_source_segments[i].size()!=0 || delivered_source_segments[i].size()!=0)
                fail("Uncache结束检查仍有slot/source owner未完成");
    endfunction
endclass
`endif
