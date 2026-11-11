# Uncache denied/corrupt 与部分写效果实现 Review（2026-11-11）

## 1. 术语与抽象功能说明

| 英文术语 | 当前中文含义 | 代码落点 | 示例 |
|---|---|---|---|
| owner / epoch | 当前动态指令身份及版本 | SQ enqueue冻结UID/ROB/SQ | NC提交后继续使用旧冻结身份 |
| token / generation | 唯一响应身份及source占用代次 | response history | source复用创建新token |
| backend / write-event | 共享内存后端及独立写接受/生效事实 | write fact map与batch | 第3段先写后denied仍保留24B |
| MMIO / NC / CMO_ZERO | 同步Store、已提交后台Store、等待响应的zero分段 | protocol kind与被动segment kind | zero实际nc=0 |
| raw / denied / corrupt | 实际D响应的拒绝及损坏位 | response_frozen | AccessAck仅合法denied |
| slot / merge | DUT真实Uncache entry与合并 | ALLOC.slot/qual | 多NC owner合并到同source |
| sample / watermark | 全局采样号及已完成观察边界 | path/producer/notification | 外D12、内部消费15对应通知17 |
| notification / CSR gate | Uncache错误地址输出及独立enable门控 | other_ctrl与sync_pkg | enable0不期待valid |
| pending / readonly / FIFO | 等采样、无副作用值查询、按新增事件处理队列 | query_pending和服务队列 | 水位已覆盖缺证据必须FAIL |

关联plan：`AI_DOC/plan/rm_plan/do/memblock_nonvector_d_error_path_classification_plan_20261110.md`；协同CMO和L2首次行方案。本review只覆盖本阶段Uncache及readonly首段/STA接口，由root统一review、编译/仿真和提交；本轮最新soft与真实Uncache zero denied验收已通过，证据见第7节。

## 2. 修改前后、字段生命周期与正确性边界

旧Uncache完成循环只按Load owner保存；Store owner_count=0导致无历史，也无独立后端写接受事实。修改后真实A先分配唯一token并冻结协议kind/nc/payload/backend结果，D完成无条件保存Store/Load；Load保留原owner索引。响应token与write-event token分别管理，注入denied不能回滚已接受数据。

NC真正backend拒绝在注错、入队和D驱动前明确fatal；MMIO/zero可合法拒绝并无写。AccessAck不能注入corrupt，权重只作用于AccessAckData，启动打印能力。

新增source entry nc只读旁路连接固定16个真实entries_n_nc，producer据source读取；被动TL_A另外采nc交叉核对。动态zero类型由SQ冻结主表finalop细分；协议history不猜Put对应哪条zero。

写批次接受分配write-event token并保存sample/time/mask，原commit应用数据后置committed及生效sample/time。普通reset仍按既有shared batch先提交再清runtime，不取消已接受写；新testcase整体清空。

独立observer只处理新增有限事件和当前物理slot来源集合，不扫描累计主表/响应历史；普通NC不挡UID，MMIO与zero由当前owner查询合法终态。

## 3. 源码与中文伪代码

### 3.1 `freeze_uncache_response_record()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:991`。

抽象功能描述：在A阶段固定完整独立响应事实。

```systemverilog
function void mem_access_base_sequence::freeze_uncache_response_record(input uncache_d_error_record_t record);
    uncache_d_error_record_t previous_record;
    if (!record.valid || !uncache_response_history_by_token.exists(record.token)) begin
        report_l2_d_error_fatal("Uncache freeze missing A token"); return;
    end
    previous_record = uncache_response_history_by_token[record.token];
    if (record.source != previous_record.source || record.generation != previous_record.generation ||
        record.reset_epoch != previous_record.reset_epoch || record.a_sample != previous_record.a_sample ||
        record.d_sample != 0 || previous_record.response_frozen || !record.response_frozen ||
        record.is_load != (record.access_kind == UNCACHE_ACCESS_LOAD)) begin
        report_l2_d_error_fatal("Uncache response freeze identity mismatch"); return;
    end
    uncache_response_history_by_token[record.token] = record;
    uvm_pkg::uvm_report_info("UNCACHE_RESPONSE_IDENTITY",$sformatf(
        "freeze token=%0d reset=%0d source=%0d generation=%0d A_sample=%0d addr=0x%0h raw=%0d/%0d write_token=%0d",
        record.token,record.reset_epoch,record.source,record.generation,record.a_sample,
        record.address,record.denied,record.corrupt,record.write_event_token),UVM_HIGH);
endfunction:freeze_uncache_response_record
```

中文伪代码：本段承担上述职责。查询已存在A token；验证source/generation/reset/sample及尚未冻结，要求新快照显式response_frozen且kind与is_load一致；失败报错返回，成功覆盖同token唯一历史。后续D调度只能搬运该快照。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.2 `validate_uncache_backend_scope()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:5913`。

抽象功能描述：在NC后端拒绝且响应尚未生成前终止不支持配置。

```systemverilog
function void sbuffer_mem_access_base_sequence::validate_uncache_backend_scope(input bit nc,
    input bit [3:0] source, input longint unsigned generation, input bit [47:0] address,
    input bit [7:0] mask, input bit backend_denied, input bit backend_corrupt);
    if (nc && (backend_denied || backend_corrupt))
        `uvm_fatal("UNCACHE_NC_BACKEND_SCOPE", $sformatf(
            "NC Store backend拒绝属于不支持配置 source=%0d generation=%0d address=0x%0h mask=0x%0h nc=%0d backend=%0d/%0d",
            source,generation,address,mask,nc,backend_denied,backend_corrupt))
endfunction:validate_uncache_backend_scope
```

中文伪代码：本段承担上述职责。只有真实nc且backend denied/corrupt非零才fatal，打印source-generation、地址、mask、nc和后端raw；MMIO/zero不套此限制。函数不回滚已提交指令，不构造伪通知。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.3 `create_uncache_response_record()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:5923`。

抽象功能描述：真实A接受建立协议身份、后端效果和一次错误响应。

```systemverilog
task sbuffer_mem_access_base_sequence::create_uncache_response_record(
    input sbuffer_agent_agent_xaction req_xact, input longint unsigned accept_cycle,
    input int unsigned owner_count, input uncache_d_error_owner_t owners[UNCACHE_D_ERROR_MAX_OWNERS]);
    bit corrupt, denied, nc;
    bit [63:0] load_data;
    uncache_response_kind_e response_kind;
    uncache_response_record_t response_record;
    uncache_d_error_record_t fact;
    if (!has_uncache_response_capacity())
        `uvm_fatal(get_type_name(), "Uncache A.fire exceeded response capacity")
    response_kind = decode_uncache_a_opcode(req_xact);
    response_record = '{default:'0};
    response_record.kind = response_kind;
    response_record.eligible_cycle = accept_cycle + 1;
    response_record.accept_cycle = accept_cycle;
    response_record.size = req_xact.auto_inner_buffers_out_a_bits_size;
    response_record.source = req_xact.auto_inner_buffers_out_a_bits_source;
    response_record.address = req_xact.auto_inner_buffers_out_a_bits_address;
    response_record.a_sample = memblock_sync_pkg::peek_current_dut_global_sample();
    response_record.owner_count = owner_count;
    foreach (response_record.owners[i]) response_record.owners[i] = owners[i];
    if (response_kind == UNCACHE_RESPONSE_LOAD_DATA && owner_count == 0)
        `uvm_fatal(get_type_name(), "Uncache Load A.fire has empty armed owner snapshot")
    if (response_kind == UNCACHE_RESPONSE_STORE_ACK && owner_count != 0)
        `uvm_fatal(get_type_name(), "Uncache Store cannot borrow Load owner snapshot")
    if (!begin_uncache_d_error_record(response_kind == UNCACHE_RESPONSE_LOAD_DATA,
            response_record.source,response_record.address,response_record.size,response_record.a_sample,
            owner_count,owners,response_record.generation))
        `uvm_fatal(get_type_name(), "failed to begin Uncache provenance record")
    if (!read_uncache_response_at_accept(response_record.a_sample,response_record.source,fact))
        `uvm_fatal(get_type_name(), "Uncache accepted token missing")
    nc = 0;
    if (uncache_owner_vif != null) begin
        if ($isunknown(uncache_owner_vif.mon_cb.uncache_top_nc[response_record.source]))
            `uvm_fatal(get_type_name(), "Uncache source entry nc contains X/Z at A.fire")
        nc = uncache_owner_vif.mon_cb.uncache_top_nc[response_record.source];
    end else if (response_kind == UNCACHE_RESPONSE_STORE_ACK) begin
        `uvm_fatal(get_type_name(), "Uncache Store A.fire缺少真实nc侧带")
    end
    last_uncache_write_token = 0;
    sbuffer_mem_access_task(response_record.address,response_kind == UNCACHE_RESPONSE_STORE_ACK,
        req_xact.auto_inner_buffers_out_a_bits_mask,req_xact.auto_inner_buffers_out_a_bits_data,
        corrupt,denied,load_data);
    fact.access_kind = response_kind == UNCACHE_RESPONSE_LOAD_DATA ? UNCACHE_ACCESS_LOAD :
        (nc ? UNCACHE_ACCESS_NC_STORE : UNCACHE_ACCESS_MMIO_STORE);
    fact.nc = nc;
    fact.mask = req_xact.auto_inner_buffers_out_a_bits_mask;
    fact.request_data = req_xact.auto_inner_buffers_out_a_bits_data;
    fact.backend_denied = denied;
    fact.backend_corrupt = corrupt;
    fact.backend_write_accepted = response_kind == UNCACHE_RESPONSE_STORE_ACK && !(denied || corrupt);
    fact.write_event_token = fact.backend_write_accepted ? last_uncache_write_token : 0;
    if (response_kind == UNCACHE_RESPONSE_STORE_ACK) begin
        validate_uncache_backend_scope(nc,response_record.source,response_record.generation,
            response_record.address,fact.mask,denied,corrupt);
        if (fact.backend_write_accepted && fact.write_event_token == 0)
            `uvm_fatal(get_type_name(), "Uncache backend接受写但缺少独立write-event token")
    end
    response_record.data = response_kind == UNCACHE_RESPONSE_STORE_ACK ? 0 : load_data;
    apply_uncache_d_error_injection(response_kind,denied,corrupt,response_record.denied,response_record.corrupt);
    fact.denied = response_record.denied;
    fact.corrupt = response_record.corrupt;
    fact.response_frozen = 1;
    response_record.error_fact = fact;
    freeze_uncache_response_record(fact);
    note_uncache_source_accept(response_record.source,response_record.address,accept_cycle);
    uncache_rsp_q.push_back(response_record);
endtask:create_uncache_response_record
```

中文伪代码：本段承担上述职责。先检查容量并解码，填Asample/source/payload并校验Load owner集合，随后建立唯一token/generation。按source读取真实entry nc后正常访问共享后端；保存backend拒绝、write accepted及write-event token。NC真正后端拒绝在注错和入队前fatal；正常已写却无write token也fatal。合并既有权重的合法raw，置response_frozen，发布快照，最后占source并入响应队列。注入错误不取消已接受写。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.4 `process_uncache_d_fire()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:6140`。

抽象功能描述：实际D完成保留包含Store的不可变响应事实。

```systemverilog
function void sbuffer_mem_access_base_sequence::process_uncache_d_fire();
    uncache_d_error_record_t completed_record;

    if (!current_d_valid) begin
        `uvm_fatal(get_type_name(), "Uncache D.fire observed without a current response record")
    end
    completed_record = current_d_record.error_fact;
    completed_record.valid       = 1'b1;
    completed_record.is_load     = current_d_record.kind == UNCACHE_RESPONSE_LOAD_DATA;
    completed_record.source      = current_d_record.source;
    completed_record.generation  = current_d_record.generation;
    completed_record.address     = current_d_record.address;
    completed_record.size        = current_d_record.size;
    completed_record.a_sample    = current_d_record.a_sample;
    completed_record.denied      = current_d_record.denied;
    completed_record.corrupt     = current_d_record.corrupt;
    completed_record.owner_count = current_d_record.owner_count;
    foreach (completed_record.owners[idx]) begin
        completed_record.owners[idx] = current_d_record.owners[idx];
    end
    commit_uncache_d_error_record(
        completed_record,
        memblock_sync_pkg::peek_current_dut_global_sample());
    note_uncache_source_complete(current_d_record.source, service_cycle);
    current_d_valid  = 1'b0;
    current_d_record = '{default:'0};
    d_hold_cycles = 0;
    d_hold_timeout_reported = 1'b0;
endfunction:process_uncache_d_fire
```

中文伪代码：本段承担上述职责。从current error_fact复制完整冻结事实，追加当次Dsample并交公共完成入口检查token/reset/generation；Load额外沿原owner索引入历史，Store也留唯一token记录。随后执行既有source完成计数及当前D清理，未制造同步异常。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.5 `read_dut_cmo_writeback_for_rm()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv:675`。

抽象功能描述：只读取同动态实例唯一真实STA0写回。

```systemverilog
function bit memblock_rm_readonly_api::read_dut_cmo_writeback_for_rm(input memblock_uid_t uid,
    input memblock_rob_key_t key, input int unsigned dynamic_epoch,
    output dut_writeback_view_t view, output bit duplicate);
    status_view_t status_view;
    int unsigned flush_epoch;
    view = '0;
    duplicate = 0;
    void'(get());
    if (!read_status_for_rm(uid,status_view) || !status_view.valid ||
        status_view.dynamic_epoch != dynamic_epoch || status_view.rob_idx_flag != key.flag ||
        status_view.rob_idx_value != key.value) return 0;
    if (status_view.sta_instance_flush_epoch_valid) flush_epoch = status_view.sta_instance_flush_epoch;
    // fault恢复可能尚未置STA专属标记；只允许同一status动态epoch已有的active代际。
    else if (status_view.active_instance_flush_epoch_valid) flush_epoch = status_view.active_instance_flush_epoch;
    else return 0;
    return m_dut_writeback_observer.read_cmo_sta0_by_rob(key,flush_epoch,uid,dynamic_epoch,view,duplicate);
endfunction:read_dut_cmo_writeback_for_rm
```

中文伪代码：本段承担上述职责。先清输出并初始化只读观察对象；核对UID status、动态epoch和ROB。优先使用STA flush代际；不存在时仅允许同status的active代际，否则返回缺失。调用底层按ROB、flush代际及采样冻结UID/epoch精确选择，返回值型记录与duplicate。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.6 `try_read_first_split_access_context_for_rm()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv:1532`。

抽象功能描述：把首段结构/执行上下文与错误来源判断分开。

```systemverilog
function bit memblock_rm_readonly_api::try_read_first_split_access_context_for_rm(
    input memblock_uid_t uid,
    input int unsigned dynamic_epoch,
    input memblock_rob_key_t rob_key,
    input bit [63:0] original_vaddr,
    input longint unsigned bound_load_sample,
    output memblock_first_split_access_context_t context_view,
    output bit observation_pending
);
    common_data_transaction data;
    status_transaction status;
    memblock_first_split_access_context_t candidate;
    bit [63:0] expected_low_vaddr;

    context_view = '{default:'0};
    observation_pending = 1'b0;
    if (bound_load_sample == 0 || !try_get_common_data(data)) return 1'b0;
    // 必须先等消费边界，不能把尚未建立的context当作已经观察过但不存在。
    if (data.split_observed_sample_seq == 0 ||
        data.split_observed_reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch() ||
        data.split_observed_sample_seq < bound_load_sample) begin
        observation_pending = 1'b1;
        return 1'b0;
    end
    if (!data.is_valid_uid(uid) || data.status_by_uid[uid] == null ||
        data.main_table_by_uid[uid] == null || data.main_table_by_uid[uid].vaddr != original_vaddr) return 1'b0;
    // V2 mabuf的低段可以向下对齐，但不能换成同页另一条line。
    expected_low_vaddr = original_vaddr;
    case (data.main_table_by_uid[uid].fuOpType[1:0])
        2'd1: ;
        2'd2: if (original_vaddr[1:0] == 2'd1) expected_low_vaddr--;
        2'd3: begin
            case (original_vaddr[2:0])
                3'd1, 3'd2, 3'd3: expected_low_vaddr -= original_vaddr[2:0];
                3'd5: expected_low_vaddr--;
                default: ;
            endcase
        end
        default: return 1'b0;
    endcase
    status = data.status_by_uid[uid];
    candidate = status.first_split_access;
    if (!candidate.valid || candidate.uid != uid || status.dynamic_epoch != dynamic_epoch ||
        candidate.dynamic_epoch != dynamic_epoch || candidate.rob_key != rob_key ||
        status.get_rob_key() != rob_key || candidate.original_vaddr != original_vaddr ||
        candidate.reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch() ||
        candidate.request_vaddr[63:12] != original_vaddr[63:12] ||
        candidate.request_vaddr != expected_low_vaddr ||
        candidate.request_vaddr[63:6] != original_vaddr[63:6] ||
        candidate.response_vaddr != candidate.request_vaddr ||
        candidate.request_sample_seq == 0 ||
        candidate.data_sample_seq <= candidate.request_sample_seq ||
        candidate.response_sample_seq <= candidate.data_sample_seq ||
        candidate.response_sample_seq > bound_load_sample ||
        candidate.later_page_seen || !candidate.data_cache_eligible ||
        candidate.response_nc || candidate.response_mmio || candidate.response_need_replay) return 1'b0;
    context_view = candidate;
    return 1'b1;
endfunction:try_read_first_split_access_context_for_rm
```

中文伪代码：本段承担上述职责。先等待split观察水位，再检查UID、主表vaddr及上下文动态身份；根据当前指令尺寸计算允许的低段向下对齐地址，逐项核对请求/数据/响应sample、cache资格、重放、MMIO/NC和后页事实。通过仅返回结构上下文；不查询PA错误。上下文无PA字段，删除无效物理line输入。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.7 `capture_sq_owner()`

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_uncache_error_observer.sv:89`。

抽象功能描述：在SQ实际enqueue冻结NC后续提交后仍可用的owner。

```systemverilog
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
```

中文伪代码：本段承担上述职责。按真实ROB单key探测readonly active map，取得主表及status并核对ROB/SQ完全一致。成功冻结UID、epoch、ROB、SQ与主表最终op。不得用SQ请求的旧uncacheUop op猜NC类型。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.8 `resolve_sq_request_owner()`

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_uncache_error_observer.sv:102`。

抽象功能描述：沿已冻结SQ身份定位同步或已提交Store。

```systemverilog
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
```

中文伪代码：本段承担上述职责。SQ请求id只给value，检查常数两个flag候选；只计入valid且真实ROB相等的记录，必须恰好一项；不扫描全主表或按地址匹配owner。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.9 `observe_batch()`

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_uncache_error_observer.sv:116`。

抽象功能描述：唯一维护SQ到Uncache slot及TL请求响应的被动因果链。

```systemverilog
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
```

中文伪代码：本段承担上述职责。reset边界清活动和未到期状态，记录新reset代次；正常批次先暂存SQ enqueue；SQ接受先使用上一代owner，再原子应用暂存enqueue并清相反flag，建立每owner最多8段zero或1段Store。依实际id核对ALLOC payload及nc，用实际slot/merge维护来源集合，再把TL A source关联该集合并排入待核对队列；外TL D只引用同source原A集合并保留已送达generation，内部D消费才释放该集合，核对Store资格并排入同一事实队列。最后推进path观察水位。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.10 `observe_notification()`

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_uncache_error_observer.sv:225`。

抽象功能描述：采集逐拍实际Uncache通知及完整观察水位。

```systemverilog
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
```

中文伪代码：本段承担上述职责。reset边界删除未到期通知；正常item的valid为1才入实际事件队列，每条保留sample/reset/address，同地址不去重。无事件也更新notification水位，允许独立checker判断缺失。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.11 `check_until()`

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_uncache_error_observer.sv:243`。

抽象功能描述：在双方水位覆盖后核对协议事实并独立比较通知。

```systemverilog
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
```

中文伪代码：本段承担上述职责。以调用sample、path水位和producer水位最小值处理待核对TL事件。A按精确sample/source取token，核对payload/nc并绑定段；D按段原token核对sample/raw/source/size并保存段完成。外D只保存同generation已送达集合；内部D消费沿原token核对raw和Store资格，再交叉检查实际busError。读取内部消费sample独立CSR gate，错误且enable时建立内部消费+2行地址期望；CSR未到则保留事件不判缺失。再与实际notification水位取交集，按事件sample顺序一对一核对，缺失、多余或地址/拍数/reset错配报错，不按地址去重、不推进UID。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.12 `validate_no_cmo_requests()`

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_uncache_error_observer.sv:342`。

抽象功能描述：检查前置fault及其它CMO路径没有非法Uncache请求。

```systemverilog
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
```

中文伪代码：本段承担上述职责。先清query_pending，推进独立checker；仅path/producer水位未覆盖completion时返回等待。按owner单key查分段集合，存在则返回明确失败，无记录返回成功；不要求Uncache正常请求。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.13 `validate_cmo_zero()`

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_uncache_error_observer.sv:360`。

抽象功能描述：由真实连续分段和独立写生效事实形成zero错误与部分效果。

```systemverilog
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
```

中文伪代码：本段承担上述职责。先等观察水位；清输出mask，要求owner存在最多8段。每段核对独立owner、zero类型、行首+8*i、maskff、零值、前段内部D已消费及首错后无请求；按token确认该段同地址size3、外D完成及内部D消费均不晚于终态。后端接受写则要求对应write-event已真实提交，核对地址mask及生效sample后置write mask；拒绝段不置位。denied优先映射bit7，corrupt-only映射bit19；无错误必须8段。通过后置请求及效果合法，核心唯一commit应用这些mask。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.14 `apply_store_expectation()`

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_uncache_error_observer.sv:424`。

抽象功能描述：对同步MMIO Store独立比较请求、错误和写效果。

```systemverilog
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
```

中文伪代码：本段承担上述职责。等待观察水位，按owner要求唯一请求/完成；核对ROB/SQ且实际nc不能为1。由item翻译后的每byte PA、独立store数据构造mask及lane值，分别比SQ请求与TL请求。raw映射bit7/19，设置effect_valid与backend accepted；已接受写还要求write-event提交且时间地址mask一致。失败仅水位不足为pending，证据错配是确定失败。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

### 3.15 `final_check()`

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_uncache_error_observer.sv:488`。

抽象功能描述：结束时检测未完成协议、通知及slot来源残留。

```systemverilog
    function void final_check(input longint unsigned sample);
        check_until(sample);
        if(pending_tl.size()!=0 || expected_notification.size()!=0 || actual_notification.size()!=0 || waiting_sq_ids.size()!=0)
            fail("Uncache结束检查仍有未解析协议/通知/分配事实");
        foreach(slot_segments[i])
            if(slot_segments[i].size()!=0 || active_source_segments[i].size()!=0 || delivered_source_segments[i].size()!=0)
                fail("Uncache结束检查仍有slot/source owner未完成");
    endfunction
```

中文伪代码：本段承担上述职责。先把sample范围内事件处理到水位，再检查待解析TL、期望/实际通知、等待ALLOC队列及固定16个slot/source/已送达未消费集合；任何残留报错，不清除队列伪造通过。 对应只读查询只返回事实，生产helper只固定协议/内存事实，未让RM反驱DUT。

## 4. Monitor 与只读接入

other_ctrl transaction增加monitor专属sample/reset字段；monitor保留原XZ检查，仅发布Uncache valid/bits与空采样水位，删除旧未启用大事务发布模板，不恢复无关traffic。根RM是唯一FIFO消费者，调用observer服务，独立通知比较不挡程序UID。

readonly新增A接受及token响应、write-event事实查询，返回值型副本；CMO真实STA0按采样冻结UID/epoch和flush代际选择。首段API只结构检查，不再以PA-only错误oracle授权异常。旧Load软件fixture取得已存在A token后显式冻结，生产D完成无缺token兼容。

## 5. 与 plan 不一致的实现

协议kind与动态segment kind分别表达协议路由和指令语义；producer缺最终zero owner，因此由被动SQ身份细分CMO_ZERO，禁止按Put猜。这是IMPLEMENTATION_DELTA中记录的最小必要分层，其余语义与plan一致。

## 6. Plan 未说明但实现补充的细节

真实entry nc旁路保证NC后端拒绝能在响应入队前判断；response_frozen防重复改写；精确source token及write-event独立事实；STA0采样owner防ROB环绕错配；首段context无PA故移除物理line参数。均已写入plan IMPLEMENTATION_DELTA。

## 7. 测试、CSV与待验证项

新增`rm_uncache_error_soft_test_t.run()`，由root薄testcase `soft_test_tc_rm_uncache_error` 调用；覆盖8段正常、两种部分写、首错停止与迟到请求、MMIO已写/拒写、NC后端范围fatal、内部消费+2/enable0、可变buffer延迟、SQ环绕、source复用/reset、NC合并、重复地址与通知缺失/多余/地址错误。CSV为`AI_DOC/seq/memblock_uncache_error_soft_framework_testcases_20261111.csv`。

已执行git diff空白检查和旧接口/保留字检索。主agent以最新编译产物完成 `soft_test_tc_rm_uncache_error`：`/tmp/cmo_uncache_run_20261111.log` 共103项检查通过，UVM_ERROR=0、UVM_FATAL=0；CSV的17个独立场景均更新为通过。真实Uncache zero denied用例 `/tmp/cmo_uncache_zero_denied_run_20261111.log` 显示TEST_PASS、UVM_ERROR=0、UVM_FATAL=0，CMO异常比较及5项最终Store-table比较通过，错误通知无错配。真实DUT CSV、其余真实场景与最终归档由主agent统一维护。
## 8. 独立review与运行补修

已修终态后迟到分段漏检：terminal_sample_by_owner冻结zero/MMIO/无请求CMO合法终态，后续同动态owner的SQ请求立即报错；soft先验证首错完成再发迟到段。

已修SQ环绕关联：本拍旧请求使用上一代owner，当前enqueue先临时缓存、随后应用并清相反flag同value；后续已接受slot携带复制owner，无历史双flag伪匹配。补source复用/reset、SQ同sample复用、两NC owner合并一通知soft。

CSR门控复核：输入CSR与内部busError均由MemBlock延迟两拍，因此选择内部D消费sample的CSR输入；soft另把内部消费与内部消费+2输入enable设为相反，避免误用外D、latest或未来值。

首次真实binary t0运行出现通知monitor早于CSR采样锚点的复位启动竞态；other_ctrl reset条件加入reset_backend_done，t0不请求不存在锚点。全局sample严格校验保持。远端重编/复测已由root完成，最新结果见第7节。

独立agent `review_three_plans` 已复查最新源码与新增soft：终态迟到、SQ同sample/环绕、source/reset、两NC合并一通知、后端接受/拒绝分离、NC后台语义、Store禁corrupt、逐事件通知及有界扫描均无新增必改项。本结论的静态范围涵盖上述链路；最新soft及真实Uncache zero denied结果已在第7节登记，其余真实路径执行结果仍由主agent统一记录。

## 9. 真实Uncache zero最终Store-table验收补充

抽象功能描述：`tc_rm_cmo_uncache_final::check_phase()`只在现有basicTest拓扑结束时验证真实Uncache数据，不增加激励或修改生产RM读取gate。静态Sv39的DISABLED控制拓扑不能插入尾CHECK_STORE marker，因此专项在全部writer实际Uncache且responder已完成后，复用原Store-table比较算法完成必要验收。

源码位置：`mem_ut/ver/ut/memblock/tc/src/tc_rm_cmo_uncache_final.sv`。

```systemverilog
    virtual function void check_phase(uvm_phase phase);
        memblock_rm_readonly_api ro;
        memblock_rm_observer_cache::store_final_entry_view_t actual_byte;
        rm_ls_model_t model;
        rm_ls_program_item_t item;
        rm_ls_store_cache_entry_t entry;
        bit [127:0] actual_data,expected_data;
        bit [15:0] actual_valid_mask,actual_corrupt_mask;
        int unsigned zero_count,entry_count,checked_count;
        bit pass;
        super.check_phase(phase);
        if(env==null || env.rm==null || env.rm.ls_model==null) begin
            `uvm_error("CMO_UNCACHE_FINAL","最终Store-table检查缺少真实env/RM模型")
            return;
        end
        model=env.rm.ls_model;
        if(!model.configured || model.current_uid!=model.main_count || model.main_count==0) begin
            `uvm_error("CMO_UNCACHE_FINAL",$sformatf("程序尚未完整比较 configured=%0d uid=%0d count=%0d",
                model.configured,model.current_uid,model.main_count))
            return;
        end
        if(!memblock_sync_pkg::sbuffer_responder_done || !memblock_sync_pkg::dcache_responder_done) begin
            `uvm_error("CMO_UNCACHE_FINAL","最终Store-table读取前真实memory responder尚未drain")
            return;
        end
        zero_count=0; pass=1;
        // 终场只扫描完整item集合一次；先检查支持范围，不能读完数据后再猜路径。
        foreach(model.items[uid]) begin
            item=model.items[uid];
            if(item==null || !item.valid) begin
                `uvm_error("CMO_UNCACHE_FINAL",$sformatf("uid=%0d缺少合法最终item",uid))
                pass=0; continue;
            end
            if(item.kind==RM_LS_KIND_STORE ||
               (item.kind==RM_LS_KIND_ATOMIC && item.expected_memory_write) ||
               (item.kind==RM_LS_KIND_CMO && item.cmo_op==RM_CMO_OP_ZERO)) begin
                if(!item.uncache_path) begin
                    `uvm_error("CMO_UNCACHE_FINAL",$sformatf("uid=%0d writer并非Uncache，禁止绕过普通cacheable比较门槛",uid))
                    pass=0;
                end
            end
            if(item.kind==RM_LS_KIND_CMO && item.cmo_op==RM_CMO_OP_ZERO) begin
                zero_count++;
                if(item.cmo_path!=RM_CMO_PATH_UNCACHE_ZERO || !item.cmo_completion_consumed ||
                   !item.cmo_requests_validated || !item.cmo_effect_valid || !item.cmo_effect_applied) begin
                    `uvm_error("CMO_UNCACHE_FINAL",$sformatf("uid=%0d zero分段/终态/唯一效果尚未完成",uid))
                    pass=0;
                end
            end
        end
        if(zero_count==0) begin
            `uvm_error("CMO_UNCACHE_FINAL","专项没有真实Uncache zero指令")
            pass=0;
        end
        if(model.sc_no_write_obligation_count()!=0) begin
            `uvm_error("CMO_UNCACHE_FINAL","Uncache zero专项存在SC无写义务，超出本测试最终比较范围")
            pass=0;
        end
        if(!pass) return;
        ro=memblock_rm_readonly_api::get();
        entry_count=model.expected_store_entry_count(); checked_count=0;
        if(entry_count==0) begin
            `uvm_error("CMO_UNCACHE_FINAL","Uncache zero专项没有任何已接受写效果entry")
            return;
        end
        for(int unsigned index=0;index<entry_count;index++) begin
            if(!model.read_expected_store_entry(index,entry)) begin
                `uvm_error("CMO_UNCACHE_FINAL",$sformatf("expected entry %0d不可读",index))
                pass=0; continue;
            end
            if(!model.items.exists(entry.uid) || model.items[entry.uid]==null ||
               !model.items[entry.uid].uncache_path) begin
                `uvm_error("CMO_UNCACHE_FINAL",$sformatf("entry %0d writer uid=%0d不是已验证Uncache路径",index,entry.uid))
                pass=0; continue;
            end
            actual_data=0; actual_valid_mask=0; actual_corrupt_mask=0;
            for(int unsigned byte_index=0;byte_index<entry.size_bytes;byte_index++) begin
                actual_byte='0;
                if(ro.read_store_final_for_rm(entry.pa_by_byte[byte_index],actual_byte) &&
                   actual_byte.valid && actual_byte.pa==entry.pa_by_byte[byte_index]) begin
                    actual_data[8*byte_index+:8]=actual_byte.value;
                    actual_valid_mask[byte_index]=actual_byte.byte_valid;
                    actual_corrupt_mask[byte_index]=actual_byte.corrupt;
                end
            end
            if(!model.compare_overlay_store_entry(entry,actual_valid_mask,actual_corrupt_mask,actual_data,expected_data)) begin
                `uvm_error("CMO_UNCACHE_FINAL",$sformatf("entry=%0d effect_uid=%0d first_pa=0x%0h actual=0x%0h expected=0x%0h reason=%s",
                    index,entry.uid,entry.pa_by_byte[0],actual_data,expected_data,model.last_error_text))
                pass=0;
            end else begin
                checked_count++;
                `uvm_info("CMO_UNCACHE_FINAL",$sformatf("STORE_COMPARE PASS entry=%0d uid=%0d size=%0d PA=0x%0h data=0x%0h",
                    index,entry.uid,entry.size_bytes,entry.pa_by_byte[0],actual_data),UVM_LOW)
            end
        end
        if(pass && checked_count==entry_count)
            `uvm_info("CMO_UNCACHE_FINAL",$sformatf("Uncache zero最终Store-table比较通过 zero_count=%0d entries=%0d",zero_count,checked_count),UVM_LOW)
    endfunction
```

中文伪代码：本段承担真实Uncache零写最终数据验收。先执行父类检查，确认env/RM存在、程序游标已完成全部item及两memory responder结束。终场只扫描item一次，所有写者必须实际uncache_path，zero必须完成真实请求、写回和唯一分段效果且至少存在一条zero；拒绝超出范围的SC义务。任何范围检查失败停止读取。随后遍历既有expected entries，只接受同Uncache writer owner；逐byte沿readonly复制真实overlay的值、valid和corrupt，传给原compare_overlay_store_entry从当前RM overlay构造期望并比较。失败报告原核心reason；成功统计实际比较条数。未写入的失败分段根本没有entry，不强制比较整行。调用原底座只比较，不复制写入算法；普通cacheable gate不变。

验证记录：首轮旧binary的69项soft检查只作为历史调试记录。内部消费关联与所有边界补修后，最新 `/tmp/cmo_uncache_run_20261111.log` 已重跑103项检查通过，UVM_ERROR=0、UVM_FATAL=0；真实denied `/tmp/cmo_uncache_zero_denied_run_20261111.log` 的5项最终Store-table比较和完整测试通过。normal及其它真实路径的最终CSV状态由主agent结合对应日志维护，不能用soft替代真实DUT验收。

## 10. 首轮真实Uncachezero采样失败与修复

真实normal运行`/tmp/cmo_uncache_zero_run_20261111.log`在731.3ns开始出现A token缺失，随后D token缺失。producer记录日志在725.3ns同边沿处理A，部分sample因执行顺序碰巧对齐，说明不是地址或source协议错误。源码核对发现producer在drv_cb后立即peek全局sample，而cmo_path monitor等待当前边沿CSR锚点；CSR同posedge晚发布时两侧差一拍。

抽象功能描述：两个memory responder的body统一把真实边沿握手身份锚定到同拍CSR发布。中文伪代码：复制clocking快照并判reset；非reset先调用wait_for_l2tlb_sample_anchor，仅同边沿NBA/delta等待，不跨clock；随后处理shared batch/A/D并发布该edge_sample水位。reset未就绪不请求锚点。token仍严格按exact接受sample/source查询，未引入PA fallback或相邻sample容忍。新增token freeze高verbosity日志及missing sample/source/reset/watermark诊断。

主agent已使用修复后的最新binary原seed复测；真实denied和soft均通过，首轮失败仅保留根因记录。

新专项最终Store-table testcase已由独立agent只读复查无必改项；它证明expected写覆盖集合的最终值。未触及字节依赖真实分段请求与首错/终态后迟到请求审计，二者组合完成部分写验收。

## 11. 真实denied通知的内部消费端修复

首轮denied实际最终Store-table5项通过，但通知期望sample181、实际184。源码确认MemBlockInlined外部Uncache D经过两级TLBuffer、xbar、再一级TLBuffer才到内部Uncache；busError在内部D消费当拍生成，随后MemBlock固定两拍输出。原“D.fire+2”必须指内部消费端，不是外边界。

抽象功能描述：新增被动内部消费probe，只证明同一source响应已经穿过实际DUT buffer并被Uncache消费，同时交叉核对内部busError；不使用顶层实际通知反造期望。

源码位置：`mem_ut/ver/ut/memblock/tb/cmo_path_agent_connect.sv`。

```systemverilog
assign cmo_path_if.probes[47] = '{
    valid: 1'(U_MEMBLOCK.inner_uncache.auto_client_out_d_valid),
    kind: cmo_path_agent_dec::CMO_PATH_UNCACHE_CONSUME_D,
    port_id: 8'd0,
    id: 32'(U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source),
    raw: {U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_denied,U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_corrupt},
    addr: 64'(U_MEMBLOCK.inner_uncache.io_busError_ecc_error_bits),
    qual: 16'({1'((U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd0) ? (U_MEMBLOCK.inner_uncache.entries_0_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd1) ? (U_MEMBLOCK.inner_uncache.entries_1_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd2) ? (U_MEMBLOCK.inner_uncache.entries_2_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd3) ? (U_MEMBLOCK.inner_uncache.entries_3_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd4) ? (U_MEMBLOCK.inner_uncache.entries_4_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd5) ? (U_MEMBLOCK.inner_uncache.entries_5_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd6) ? (U_MEMBLOCK.inner_uncache.entries_6_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd7) ? (U_MEMBLOCK.inner_uncache.entries_7_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd8) ? (U_MEMBLOCK.inner_uncache.entries_8_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd9) ? (U_MEMBLOCK.inner_uncache.entries_9_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd10) ? (U_MEMBLOCK.inner_uncache.entries_10_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd11) ? (U_MEMBLOCK.inner_uncache.entries_11_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd12) ? (U_MEMBLOCK.inner_uncache.entries_12_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd13) ? (U_MEMBLOCK.inner_uncache.entries_13_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.auto_client_out_d_bits_source == 4'd14) ? (U_MEMBLOCK.inner_uncache.entries_14_cmd == 5'd1) : (U_MEMBLOCK.inner_uncache.entries_15_cmd == 5'd1)),U_MEMBLOCK.inner_uncache.io_busError_ecc_error_valid}),
    default: '0
};
```

中文伪代码：本段为每次内部D.valid采集source/raw和Store cmd；当前V2内部D.ready固定1，故valid等于真实消费。实际busError valid/address仅放在资格和地址字段供交叉检查。observer沿外D已保存的source-generation/token取得冻结raw和地址，要求内部raw及busError资格/地址一致，再以内部消费sample+2生成顶层期望；读取内部消费sample CSR输入，因为CSR也延迟两拍。外部传输延迟不作固定假设。

状态边界：外D完成后由delivered_source_segments保留旧generation直至内部D消费；source不能用新owner覆盖尚未消费上下文。zero/MMIO终态要求对应consume_sample存在且不晚于completion，下一zero段必须晚于上一段内部消费。reset清未到达集合，最终检查覆盖该集合残留。

soft同步：默认外D12→内部D15→输出17；可变buffer延迟0/3/7逐项检验输出随真实内部消费变化，禁固定外D+5或±3容差。两NC合并仍只有一个内部消费和一个通知，原输入enable相反测试改以内D sample和内D+2输入为界。最新17项CSV已由最新binary复测通过，共103项检查、UVM_ERROR=0、UVM_FATAL=0。

补充静态检查：48个probe赋值覆盖0～47且无重复；内部busError资格/有效地址使用四态严格比较，不能因X折叠静默通过。soft还先处理外D并确认无通知期望，之后才发布独立内部消费事件，证明未把实际top通知或固定buffer延迟当作答案。


## 12. 最终 Plan 对齐复查

复查关联plan：`AI_DOC/plan/rm_plan/do/memblock_nonvector_d_error_path_classification_plan_20261110.md`。普通NC Store保留架构提交overlay，不等待后台总线响应或补同步trap；独立observer按真实slot/source/token关联至内部D消费，并按该sample的CSR输入和两拍输出延迟逐事件比较通知。MMIO Store按独立翻译、主表数据、owner请求及响应raw比较STA与terminal，仅在backend已接受时应用确定性效果。Uncache Load复用UID/epoch、ROB/LQ、物理范围及sample的完成历史，包括CLEAN，denied映射位5、仅corrupt映射位19，不套原子或通知的CSR门控；异常匹配后不执行普通Load数据读取。

### 12.1 实现与 Plan 不一致项

协议kind与动态segment kind的分层、真实内部D消费观察及同拍CSR锚点均已通过IMPLEMENTATION_DELTA回写对应plan。除这些已说明并同步的实现调整外，未发现待修复的功能不一致项。源码片段及中文控制流已同步最新源码；NC真正backend拒绝仍按plan限制在响应入队前fatal，不声称该配置受支持。

### 12.2 Plan 未说明但 Coding 落实的细节

响应冻结、STA采样owner、SQ同sample替换、终态后迟到请求持续审计、外D到内部D的可变buffer时延等细节已分别记录于第3、8、10、11节及plan IMPLEMENTATION_DELTA；未发现尚未回写plan的额外语义。最新静态复查未发现Uncache路径阻塞项。

### 12.3 非本次修改的逻辑分析

本review覆盖Uncache producer、独立观察器、相关readonly/STA接口、Uncache通知monitor、内部Uncache消费probe和Uncache专项soft/最终比较testcase。工作区其余DCache错误来源、原子消费和CMO请求观察/核心提交属于同一三方案的其它特性，由主agent和对应实现agent独立review；本review不替代其源码与真实场景验收。真实DUT CSV、plan路径移动及本地提交由主agent统一处理。
