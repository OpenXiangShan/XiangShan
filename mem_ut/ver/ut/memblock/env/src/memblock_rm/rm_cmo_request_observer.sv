`ifndef RM_CMO_REQUEST_OBSERVER_SV
`define RM_CMO_REQUEST_OBSERVER_SV
// 被动请求账：只关联真实接口，不生成请求、不改变框架状态或重建cache算法。
class rm_cmo_request_observer_t;
    typedef struct packed {
        bit valid;
        memblock_uid_t uid;
        int unsigned epoch;
        bit [8:0] rob;
        bit [6:0] sq;
        bit [8:0] op;
        bit cmo_class;
    } owner_t;
    typedef struct packed {
        bit valid;
        owner_t owner;
        cmo_path_probe_t request;
        longint unsigned sample_seq;
    } accepted_t;
    typedef struct {
        owner_t owner;
        int unsigned sq_count;
        int unsigned tl_count;
        longint unsigned first_sample;
        longint unsigned success_sample;
        longint unsigned token;
        longint unsigned token_accept_sample;
        bit [63:0] line;
        bit failed;
        bit consumed;
        bit cancelled;
        string error;
    } request_state_t;
    typedef bit [127:0] owner_key_t;
    typedef owner_key_t owner_key_q_t[$];
    request_state_t requests[owner_key_t];
    accepted_t sb_input[2];
    // 真实SQ enqueue时冻结身份，避免SQ->SB消费者稍晚服务时active map已经释放。
    owner_t sq_owner_by_value[64];
    longint unsigned next_request_token;
    owner_key_q_t zero_by_slot[16];
    longint unsigned slot_generation[16];
    bit slot_request_valid[16];
    bit [31:0] slot_request_id[16];
    longint unsigned slot_request_generation[16];
    accepted_t cmo_waiting[$];
    longint unsigned watermark;
    longint unsigned reset_generation;
    int unsigned error_count;
    bit pending_evidence;

    function new();
        watermark=0; reset_generation=0; error_count=0; next_request_token=1;
        pending_evidence=0;
        foreach(sq_owner_by_value[i]) sq_owner_by_value[i]='0;
        foreach(sb_input[i]) sb_input[i]='0;
        foreach(slot_generation[i]) begin
            slot_generation[i]=0; slot_request_valid[i]=0; slot_request_id[i]=0;
            slot_request_generation[i]=0;
        end
    endfunction
    function owner_key_t key(input owner_t owner);
        return {reset_generation,32'(owner.uid),32'(owner.epoch)};
    endfunction
    virtual function void fail(input string detail);
        error_count++;
        `uvm_error("RM_CMO_REQUEST",detail)
    endfunction
    // 抽象职责：请求接受当拍按真实ROB或SQ查询唯一动态owner，并冻结完整身份。
    virtual function bit resolve_owner(input cmo_path_probe_t event_i, output owner_t owner);
        memblock_rm_readonly_api ro;
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        memblock_uid_t uid;
        int match_count;
        owner='0; ro=memblock_rm_readonly_api::get(); match_count=0;
        if(event_i.owner_valid) begin
            if(!ro.try_read_uid_by_rob_for_rm(
                '{flag:event_i.rob[8],value:event_i.rob[7:0]},uid) ||
               !ro.read_main_transaction_for_rm(uid,main_v) ||
               !ro.read_status_for_rm(uid,status_v)) return 0;
            owner='{valid:1,uid:uid,epoch:status_v.dynamic_epoch,rob:event_i.rob,
                    sq:{main_v.sq_idx_flag,main_v.sq_idx_value},op:main_v.fu_op_type,
                    cmo_class:main_v.fu_type==MEMBLOCK_FUTYPE_STU &&
                              rm_ls_decode_cmo_final_op(main_v.fu_op_type)!=RM_CMO_OP_NONE};
            return main_v.valid && status_v.valid && !status_v.flushed &&
                   {main_v.rob_idx_flag,main_v.rob_idx_value}==event_i.rob;
        end
        // 优先使用已冻结的实际enqueue；只有基线首拍尚无镜像才做两key有界只读查找。
        if(sq_owner_by_value[event_i.sq[5:0]].valid) begin
            owner=sq_owner_by_value[event_i.sq[5:0]];
            return 1;
        end
        // DataBuffer优化掉flag；常数两次key查询，无主表扫描。
        for(int flag_i=0;flag_i<2;flag_i++) begin
            if(ro.try_read_uid_by_sq_for_rm(
                '{flag:1'(flag_i),value:event_i.sq[5:0]},uid) &&
               ro.read_main_transaction_for_rm(uid,main_v) &&
               ro.read_status_for_rm(uid,status_v) &&
               status_v.active_sq_mapped && !status_v.flushed &&
               main_v.sq_idx_value==event_i.sq[5:0]) begin
                match_count++;
                owner='{valid:1,uid:uid,epoch:status_v.dynamic_epoch,
                        rob:{main_v.rob_idx_flag,main_v.rob_idx_value},
                        sq:{main_v.sq_idx_flag,main_v.sq_idx_value},op:main_v.fu_op_type,
                        cmo_class:main_v.fu_type==MEMBLOCK_FUTYPE_STU &&
                                  rm_ls_decode_cmo_final_op(main_v.fu_op_type)!=RM_CMO_OP_NONE};
            end
        end
        return match_count==1;
    endfunction
    function bit is_cmo(input owner_t owner);
        return owner.valid && owner.cmo_class && rm_ls_decode_cmo_final_op(owner.op)!=RM_CMO_OP_NONE;
    endfunction
    function void note_request(input accepted_t accepted);
        request_state_t state_v;
        owner_key_t k;
        k=key(accepted.owner);
        if(requests.exists(k)) state_v=requests[k];
        else begin
            state_v.owner=accepted.owner; state_v.sq_count=0; state_v.tl_count=0;
            state_v.first_sample=accepted.sample_seq; state_v.success_sample=0;
            state_v.token=next_request_token++; state_v.token_accept_sample=accepted.sample_seq;
            state_v.line={accepted.request.addr[63:6],6'b0};
            state_v.failed=0; state_v.consumed=0; state_v.cancelled=0; state_v.error="";
        end
        state_v.sq_count++;
        if(state_v.sq_count!=1 || state_v.owner!==accepted.owner) begin
            state_v.failed=1; state_v.error="CMO请求重复或owner变化";
            fail($sformatf("uid=%0d %s",accepted.owner.uid,state_v.error));
        end
        requests[k]=state_v;
    endfunction
    // 抽象职责：按同拍批次的真实因果顺序维护SQ->SB->DCache与SQ->CBO A关联。
    function void observe_batch(input cmo_path_agent_xaction tr);
        accepted_t next_sb[2];
        accepted_t accepted;
        request_state_t state_v;
        owner_key_t k;
        longint unsigned token;
        bit [15:0] allocated_slots;
        if(tr==null) return;
        if(tr.reset_active) begin
            foreach(sb_input[i]) sb_input[i]='0;
            foreach(sq_owner_by_value[i]) sq_owner_by_value[i]='0;
            foreach(zero_by_slot[i]) zero_by_slot[i].delete();
            foreach(slot_request_valid[i]) slot_request_valid[i]=0;
            // 历史取消只在reset代次边界执行一次，长reset不逐拍扫描累计账本。
            if(reset_generation!=tr.reset_generation || watermark==0) begin
                foreach(requests[old_key]) begin
                    state_v=requests[old_key];
                    if(!state_v.consumed) begin state_v.cancelled=1; requests[old_key]=state_v; end
                end
            end
            cmo_waiting.delete(); reset_generation=tr.reset_generation;
            watermark=tr.sample_seq; return;
        end
        if(tr.reset_generation!=reset_generation && watermark!=0) begin
            fail("CMO monitor reset代际变化但未观察reset批次"); return;
        end
        if(tr.sample_seq<=watermark) begin
            fail("CMO monitor批次sample重复或倒退"); return;
        end
        foreach(next_sb[i]) next_sb[i]='0;
        allocated_slots='0;
        // accessIdx来自上一拍。先关联旧SQ接受，再采本拍SQ输入。
        foreach(tr.events[i]) begin
            cmo_path_probe_t e;
            e=tr.events[i];
            if(e.kind==CMO_PATH_SB_ACCESS && e.port_id<2 && e.slot<16) begin
                accepted=sb_input[e.port_id];
                if(!accepted.valid || accepted.sample_seq+1!=tr.sample_seq) begin
                    fail("SB accessIdx没有上一拍SQ接受事实"); continue;
                end
                // 双端同tag的新插入可指向同slot，canMerge均为0；同批只分配一次generation。
                if(!accepted.request.qual[2] && !allocated_slots[e.slot]) begin
                    if(zero_by_slot[e.slot].size()!=0 || slot_request_valid[e.slot])
                        fail("SB新slot覆盖了尚未完成zero来源或传输");
                    slot_generation[e.slot]++;
                    zero_by_slot[e.slot].delete();
                    slot_request_valid[e.slot]=0;
                    allocated_slots[e.slot]=1'b1;
                end
                if(accepted.request.qual[0] && accepted.request.qual[1])
                    zero_by_slot[e.slot].push_back(key(accepted.owner));
            end
        end
        // 同拍SQ内部接受先于TL CBO匹配，结果不依赖producer向events入队的顺序。
        for(int phase=0;phase<2;phase++) begin
        foreach(tr.events[i]) begin
            cmo_path_probe_t e;
            e=tr.events[i];
            if((e.kind==CMO_PATH_DCACHE_TL_A)!=(phase==1)) continue;
            case(e.kind)
                CMO_PATH_SQ_SBUFFER: begin
                    accepted='0; accepted.valid=1; accepted.request=e;
                    accepted.sample_seq=tr.sample_seq;
                    // 所有真实SQ->SB接受都解析owner：即使DUT错误地漏发wline，
                    // 也必须记录CMO走过该路径，不能让Uncache/front-fault的禁止请求检查漏检。
                    if(resolve_owner(e,accepted.owner) && is_cmo(accepted.owner)) begin
                        note_request(accepted);
                        if(rm_ls_decode_cmo_final_op(accepted.owner.op)!=RM_CMO_OP_ZERO ||
                           !e.qual[0] || !e.qual[1] || e.data!==0 ||
                           e.mask!==64'h0000_0000_0000_ffff) begin
                            k=key(accepted.owner); state_v=requests[k];
                            state_v.failed=1; state_v.error="CMO SQ->SB类型/wline/vecValid/零数据/输入mask错误";
                            requests[k]=state_v; fail(state_v.error);
                        end
                    end else if(e.qual[0]) begin
                        fail("SQ wline没有唯一cbo.zero owner"); continue;
                    end
                    if(e.port_id<2) next_sb[e.port_id]=accepted;
                end
                CMO_PATH_SQ_CMO_REQUEST: begin
                    accepted='0; accepted.valid=1; accepted.request=e;
                    accepted.sample_seq=tr.sample_seq;
                    if(!resolve_owner(e,accepted.owner) || !is_cmo(accepted.owner) ||
                       rm_ls_decode_cmo_final_op(accepted.owner.op)==RM_CMO_OP_ZERO) begin
                        fail("SQ CMO请求缺少唯一nonzero owner"); continue;
                    end
                    note_request(accepted);
                    if(e.op!=accepted.owner.op || e.addr[5:0]!=0 ||
                       e.id[1:0]!=accepted.owner.op[1:0]) begin
                        k=key(accepted.owner); state_v=requests[k];
                        state_v.failed=1; state_v.error="内部CMO opcode/line地址错误";
                        requests[k]=state_v; fail(state_v.error);
                    end
                    cmo_waiting.push_back(accepted);
                end
                CMO_PATH_SB_DCACHE_REQUEST: begin
                    if(e.slot>=16) begin fail("SB请求slot越界"); continue; end
                    if(slot_request_valid[e.slot])
                        fail("SB上一attempt尚未成功或replay便重复发射");
                    slot_request_valid[e.slot]=1; slot_request_id[e.slot]=e.id;
                    slot_request_generation[e.slot]=slot_generation[e.slot];
                    foreach(zero_by_slot[e.slot][j]) begin
                        k=zero_by_slot[e.slot][j]; state_v=requests[k];
                        if(e.addr[63:6]!=state_v.line[63:6] || e.addr[5:0]!=0 ||
                           e.mask!==64'hffff_ffff_ffff_ffff) begin
                            state_v.failed=1; state_v.error="zero SB请求物理行或整行mask错误";
                            fail(state_v.error);
                        end
                        state_v.tl_count++; requests[k]=state_v;
                    end
                end
                CMO_PATH_SB_DCACHE_SUCCESS,CMO_PATH_SB_DCACHE_REPLAY: begin
                    if(e.slot>=16) begin fail("SB响应slot越界"); continue; end
                    if(!slot_request_valid[e.slot] || slot_request_id[e.slot]!=e.id ||
                       slot_request_generation[e.slot]!=slot_generation[e.slot]) begin
                        fail("SB成功/replay没有匹配的真实请求attempt"); continue;
                    end
                    slot_request_valid[e.slot]=0;
                    if(e.kind==CMO_PATH_SB_DCACHE_SUCCESS) begin
                        foreach(zero_by_slot[e.slot][j]) begin
                            k=zero_by_slot[e.slot][j]; state_v=requests[k];
                            state_v.success_sample=tr.sample_seq; requests[k]=state_v;
                        end
                        zero_by_slot[e.slot].delete();
                    end
                end
                CMO_PATH_DCACHE_TL_A: if(e.op inside {9'd12,9'd13,9'd14}) begin
                    bit [8:0] expected_op;
                    if(cmo_waiting.size()==0) begin fail("TL CBO A缺少SQ CMO请求"); continue; end
                    accepted=cmo_waiting.pop_front(); k=key(accepted.owner); state_v=requests[k];
                    expected_op=rm_ls_decode_cmo_final_op(accepted.owner.op)==RM_CMO_OP_CLEAN ? 12 :
                                rm_ls_decode_cmo_final_op(accepted.owner.op)==RM_CMO_OP_FLUSH ? 13 : 14;
                    if(e.op!=expected_op || e.addr[63:6]!=state_v.line[63:6] ||
                       e.addr[5:0]!=0 || e.state[2:0]!=6 || tr.sample_seq<accepted.sample_seq) begin
                        state_v.failed=1; state_v.error="TL CBO opcode/line/size/顺序错误";
                        fail(state_v.error);
                    end
                    if(!mem_access_base_sequence::find_l2_response_at_accept(
                        tr.sample_seq,10'(e.id),0,token)) begin
                        state_v.failed=1; state_v.error="TL CBO A缺少独立响应token";
                        fail(state_v.error);
                    end else begin
                        mem_access_base_sequence::l2_response_owner_t bound_owner;
                        bound_owner='{valid:1,uid:accepted.owner.uid,
                            dynamic_epoch:accepted.owner.epoch,rob_flag:accepted.owner.rob[8],
                            rob_value:accepted.owner.rob[7:0],sq_flag:accepted.owner.sq[6],
                            sq_value:accepted.owner.sq[5:0]};
                        if(!mem_access_base_sequence::bind_l2_response_owner(token,tr.sample_seq,bound_owner))
                            fail("TL CBO响应token owner绑定失败");
                    end
                    state_v.token=token; state_v.token_accept_sample=tr.sample_seq;
                    state_v.tl_count++; requests[k]=state_v;
                end
                default: ;
            endcase
        end
        end
        // 先消费本拍旧SQ请求，再冻结本拍新enqueue；满队列同slot出队/复用时不能覆盖旧owner。
        foreach(tr.events[i]) begin
            cmo_path_probe_t e;
            owner_t enqueue_owner;
            e=tr.events[i];
            if(e.kind!=CMO_PATH_SQ_ENQUEUE) continue;
            if(!resolve_owner(e,enqueue_owner) || enqueue_owner.sq!=e.sq ||
               enqueue_owner.op!=e.op) begin
                fail("SQ enqueue身份与只读owner不匹配"); continue;
            end
            sq_owner_by_value[e.sq[5:0]]=enqueue_owner;
        end
        foreach(sb_input[i]) sb_input[i]=next_sb[i];
        watermark=tr.sample_seq;
    endfunction
    function bit observation_pending(); return pending_evidence; endfunction
    // 抽象职责：提交时检查owner自己的真实请求集合；响应错误交给统一入口，绝不读取异常当预期。
    function bit validate(input rm_ls_program_item_t item,
                          input longint unsigned completion_sample, output string detail);
        owner_key_t k;
        request_state_t state_v;
        detail=""; pending_evidence=0;
        if(item==null) begin detail="CMO请求检查缺少item"; return 0; end
        k={reset_generation,32'(item.uid),32'(item.dynamic_epoch)};
        if(watermark<completion_sample) begin
            pending_evidence=1; detail="CMO请求水位尚未覆盖completion"; return 0;
        end
        if(item.cmo_pre_access_exception!=0) begin
            if(!validate_no_requests(item,completion_sample,detail)) return 0;
            item.cmo_requests_validated=1; item.cmo_request_watermark=watermark; return 1;
        end
        if(!requests.exists(k)) begin detail="CMO缺少实际SQ请求"; return 0; end
        state_v=requests[k];
        if(state_v.failed || state_v.cancelled || state_v.consumed ||
           state_v.sq_count!=1 || state_v.first_sample>completion_sample ||
           state_v.owner.op!=item.op || state_v.owner.rob!={item.rob.flag,8'(item.rob.value)} ||
           |item.rob.value[15:8] ||
           state_v.owner.sq!={item.sq_flag,item.sq_value} ||
           state_v.line[63:6]!=item.pa_by_byte[0][63:6]) begin
            detail=$sformatf("CMO请求身份/计数/地址失败：%s",state_v.error); return 0;
        end
        if(item.cmo_path==RM_CMO_PATH_CACHEABLE_ZERO) begin
            if(state_v.tl_count==0 || state_v.success_sample==0 || state_v.success_sample>completion_sample) begin
                detail="cacheable zero最终STA0之前缺少实际SB请求及成功（replay不算完成）"; return 0;
            end
        end else if(item.cmo_path==RM_CMO_PATH_DCACHE_CMO) begin
            if(state_v.tl_count!=1 || state_v.token==0 || state_v.token_accept_sample>completion_sample) begin
                detail="nonzero CMO缺少completion之前唯一TL CBO请求"; return 0;
            end
        end else begin detail="Uncache zero须调用统一分段验证入口"; return 0; end
        item.cmo_requests_validated=1; item.cmo_request_count=state_v.sq_count;
        item.cmo_request_token=state_v.token; item.cmo_request_watermark=watermark;
        return 1;
    endfunction
    // 抽象职责：前置异常或Uncache zero不得同时产生cacheable SQ/SB/CMO请求；
    // 只检查对应动态owner的账项，并等monitor水位覆盖最终completion。
    function bit validate_no_requests(input rm_ls_program_item_t item,
                                      input longint unsigned completion_sample,
                                      output string detail);
        owner_key_t k;
        detail=""; pending_evidence=0;
        if(item==null) begin detail="CMO禁止请求检查缺少item"; return 0; end
        if(watermark<completion_sample) begin
            pending_evidence=1; detail="CMO请求水位尚未覆盖completion"; return 0;
        end
        k={reset_generation,32'(item.uid),32'(item.dynamic_epoch)};
        if(requests.exists(k)) begin
            detail="CMO在禁止的cacheable SQ/SB/CMO路径产生了请求";
            return 0;
        end
        return 1;
    endfunction
    // 抽象职责：只有核心唯一commit成功才标请求已消费；validate轮询不消费队列。
    function bit mark_consumed(input rm_ls_program_item_t item);
        owner_key_t k;
        request_state_t state_v;
        if(item==null || !item.cmo_requests_validated || !item.cmo_completion_consumed) return 0;
        k={reset_generation,32'(item.uid),32'(item.dynamic_epoch)};
        if(item.cmo_pre_access_exception!=0) return !requests.exists(k);
        if(!requests.exists(k)) return 0;
        state_v=requests[k];
        if(state_v.failed || state_v.cancelled || state_v.consumed) return 0;
        state_v.consumed=1; requests[k]=state_v;
        return 1;
    endfunction

    // 抽象职责：最终drain允许一次历史检查，捕获已接受却没有对应CMO终态的请求。
    function bit check_drained();
        bit clean;
        clean=1;
        foreach(requests[k]) begin
            if(!requests[k].cancelled && !requests[k].consumed) begin
                clean=0;
                fail($sformatf("CMO请求未消费 uid=%0d epoch=%0d",requests[k].owner.uid,requests[k].owner.epoch));
            end
        end
        if(cmo_waiting.size()!=0) begin clean=0; fail("CMO内部请求仍等待真实TL A"); end
        foreach(zero_by_slot[i])
            if(zero_by_slot[i].size()!=0) begin clean=0; fail("zero请求仍等待真实SB完成"); end
        return clean;
    endfunction
endclass
`endif
