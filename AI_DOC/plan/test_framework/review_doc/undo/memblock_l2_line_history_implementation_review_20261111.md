# L2 首次行选择与逐 beat 历史实现 Review（2026-11-11）

## 1. 术语、范围与验证边界

| 术语 | 中文含义与代码落点 |
|---|---|
| line choice / CLEAN / DENIED / CORRUPT | 唯一 `l2_line_history_by_line` 的首次正常/拒绝/损坏选择，不是当前指令异常判断 |
| token / generation / reset epoch | 单笔响应身份、source占用代次和DUT复位隔离代际 |
| beat / keyword / mask / sample | 单次传输、关键词先后、物理字节覆盖与全局采样序号 |
| reqSource / owner | DUT已有请求来源编码与独立冻结的UID/ROB/SQ/动态版本身份 |
| PA-only / oracle / consumption_proven | 仅地址诊断、错误推断真源、实际指定beat已送达证明 |
| Probe / Release / GrantAck / CBOAck | B/C探测、C通道释放、E完成确认和CMO响应类别 |
| readonly / pending / lifecycle | 无副作用值查询、未完成上下文、testcase共享内存生命周期 |
| overlay / backend / Store-origin | 期望/实际写覆盖数据、共享内存底座、Store及cacheable zero来源 |

关联 plan：`AI_DOC/plan/test_framework/plan/undo/memblock_l2_grantdata_first_line_weighted_error_plan_20261110.md`。开始 hash：`b9c65cf26015d71a420b787458f0f866cc1b99f2`。本 review 只覆盖框架记录与旧接口迁移；统一错误 RM 和 CMO RM 的最终接入由各阶段独立闭环。

状态：代码已落，等待主 agent 独立 review 与远端编译/仿真；未把检查通过或带错误 RM 支持作为既成事实。

## 2. 替换对象、字段生命周期及修改清单

旧 live/pending 两张错误选择表替换为含CLEAN的单一首次行map；旧只有末GrantData完成后可见的行错误改为唯一token逐beat历史。正常D.fire也记录。永久PA+sample不能证明当前操作消费了错误。

旧sticky参数、getter、解析、cfg及关闭分支均删除；既有四个L2错误权重不新增开关且默认0。`ledger_ready`只说明初始化，不能关闭记录。所有cfg删除仅为同一旧key，没有改变其他参数。

新增响应记录字段 `response_token` 在真实接受设置，贯穿延期/CBO等待/调度/D hold，临时记录释放不删除历史。source代次在接受递增、末D释放活跃槽、普通reset只清活跃槽并增加reset代际；testcase初始化清空全部。Probe子token保留父A关联与B/C方向，E沿Grant token引用。

普通cacheable Store/cacheable zero只影响本笔raw编码，不覆盖已冻结行选择、不取消原数据写批次；真实C数据路径仍按原有方向错误规则处理。

公共来源枚举放在`memblock_dispatch_types.sv`，5位值2/3/4/6；`capture_dcache_a_xaction()`原本已完整复制该字段，未增加地址猜测。

## 3. 核心函数源码与中文伪代码

### 3.1 `reset_l2_line_history()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:557`。

抽象功能描述：开始新的testcase行选择与历史生命周期。

```systemverilog
function void mem_access_base_sequence::reset_l2_line_history();
    l2_line_history_by_line.delete();
    l2_response_history_by_token.delete();
    l2_probe_history_by_token.delete();
    l2_response_token_by_accept.delete();
    l2_d_error_next_generation = 1;
    l2_next_response_token = 1;
    l2_response_reset_epoch = 0;
    foreach (l2_active_response_token[d, i]) begin
        l2_active_response_token[d][i] = 0;
        l2_source_generation[d][i] = 0;
    end
    l2_d_error_ledger_ready = 1'b1;
endfunction:reset_l2_line_history
```

中文伪代码：清空唯一行选择、响应历史、Probe历史和精确接受索引；将选择generation和token分配器归一为1，reset代际归零；遍历固定2×64个source槽清空活跃token及代次，最后置ledger_ready。此入口不在逐笔访问或普通DUT reset调用。

### 3.2 `abort_l2_response_pending()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:572`。

抽象功能描述：普通DUT reset终止未完成传输并隔离旧token。

```systemverilog
function void mem_access_base_sequence::abort_l2_response_pending();
    if (!l2_d_error_ledger_ready) return;
    l2_response_reset_epoch++;
    foreach (l2_active_response_token[d, i]) l2_active_response_token[d][i] = 0;
endfunction:abort_l2_response_pending
```

中文伪代码：未初始化直接返回；已初始化则只递增reset代际并清空固定source活跃索引，不删除行选择、完成历史或source代次。后续token查询拒绝旧reset代际。

### 3.3 `lookup_l2_line_choice()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:579`。

抽象功能描述：以单个物理行key完成首次选择查找及原子登记。

```systemverilog
function bit mem_access_base_sequence::lookup_l2_line_choice(
    input mem_addr_t line_addr, input longint unsigned sample, input bit [9:0] source,
    input int unsigned kind, input bit [4:0] req_source,
    input bit candidate_valid, input l2_d_error_state_e candidate,
    output bit needs_candidate, output l2_d_error_line_record_t record);
    dcache_line_addr_t key;
    record = '0;
    needs_candidate = 0;
    if (!l2_d_error_ledger_ready || sample == 0) return 0;
    key = line_addr[47:6];
    if (l2_line_history_by_line.exists(key)) begin
        record = l2_line_history_by_line[key];
        return record.valid;
    end
    if (!candidate_valid) begin
        needs_candidate = 1;
        return 1;
    end
    if (!(candidate inside {L2_D_ERROR_NONE, L2_D_ERROR_CORRUPT, L2_D_ERROR_DENIED}) ||
        l2_d_error_next_generation == 0) begin
        report_l2_d_error_fatal("invalid first line choice/generation");
        return 0;
    end
    record.valid = 1;
    record.state = candidate;
    record.generation = l2_d_error_next_generation++;
    record.accept_sample = sample;
    record.first_kind = kind;
    record.source_valid = 1;
    record.source = source;
    record.req_source = req_source;
    l2_line_history_by_line[key] = record;
    return 1;
endfunction:lookup_l2_line_choice
```

中文伪代码：先清输出，未初始化或sample为零返回失败。已有行直接复制且不读取候选；缺行且未提供候选只返回需要候选。提供候选时检查其属于三类合法状态及generation非零，填入首次类别、来源、sample和generation并插入唯一map。CLEAN不再被当成缺失。

### 3.4 `register_clean_l2_access()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:615`。

抽象功能描述：将无注入能力的真实访问纳入首次行选择。

```systemverilog
function void mem_access_base_sequence::register_clean_l2_access(input mem_addr_t line_addr,
    input longint unsigned sample, input bit [9:0] source, input int unsigned kind);
    bit needs_candidate;
    l2_d_error_line_record_t record;
    if (!lookup_l2_line_choice(line_addr, sample, source, kind, 0, 1,
                              L2_D_ERROR_NONE, needs_candidate, record))
        report_l2_d_error_fatal("clean access before ledger/sample initialization");
endfunction:register_clean_l2_access
```

中文伪代码：把CLEAN作为有效候选交给行选择入口；已有行保留原选择，缺行立即登记；失败报生命周期fatal。此helper没有随机、内存访问或数据阻断。

### 3.5 `begin_l2_response_history()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:625`。

抽象功能描述：在真实接受点建立不可复用的响应身份。

```systemverilog
function longint unsigned mem_access_base_sequence::begin_l2_response_history(
    input mem_addr_t line_addr, input longint unsigned sample, input bit [9:0] source,
    input bit is_release, input int unsigned kind, input bit [3:0] opcode,
    input bit [4:0] req_source, input bit injection_allowed,
    input bit denied, input bit corrupt, input bit echo_is_keyword);
    l2_response_history_t record;
    l2_accept_key_t key;
    if (!l2_d_error_ledger_ready || source >= 64 || sample == 0 ||
        !l2_line_history_by_line.exists(line_addr[47:6]) || l2_next_response_token == 0) begin
        report_l2_d_error_fatal("response begin without valid line/source/sample");
        return 0;
    end
    if (l2_active_response_token[is_release][source] != 0) begin
        report_l2_d_error_fatal($sformatf("source reused before D completion source=%0d release=%0d", source, is_release));
        return 0;
    end
    record = '0;
    record.valid = 1;
    record.token = l2_next_response_token++;
    record.reset_epoch = l2_response_reset_epoch;
    record.source_generation = ++l2_source_generation[is_release][source];
    record.line_generation = l2_line_history_by_line[line_addr[47:6]].generation;
    record.line_addr = {line_addr[47:6], 6'b0};
    record.is_release = is_release;
    record.kind = kind;
    record.opcode = opcode;
    record.source = source;
    record.req_source = req_source;
    record.accept_sample = sample;
    record.injection_allowed = injection_allowed;
    record.denied = denied;
    record.corrupt = corrupt;
    record.echo_is_keyword = echo_is_keyword;
    key = {l2_response_reset_epoch, sample, is_release, source};
    if (l2_response_token_by_accept.exists(key)) begin
        report_l2_d_error_fatal("duplicate accepted response identity");
        return 0;
    end
    l2_response_history_by_token[record.token] = record;
    l2_response_token_by_accept[key] = record.token;
    l2_active_response_token[is_release][source] = record.token;
    return record.token;
endfunction:begin_l2_response_history
```

中文伪代码：检查账本、source范围、sample、行选择存在和token未溢出；同方向source仍活跃则fatal。分配token及source新代次，复制行代次、地址、类别、opcode、reqSource、raw和关键词。按reset代际、sample、方向及source建立精确索引；重复接受身份fatal。最后原子发布记录并占用source槽。

### 3.6 `publish_l2_response_beat_history()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:670`。

抽象功能描述：发布本次真实D.fire的不可变beat事实。

```systemverilog
function void mem_access_base_sequence::publish_l2_response_beat_history(
    input longint unsigned token, input int unsigned beat_idx, input longint unsigned sample);
    l2_response_history_t record;
    l2_response_beat_t beat_record;
    bit high_half;
    if (!l2_response_history_by_token.exists(token)) begin
        report_l2_d_error_fatal("D.fire references missing response token");
        return;
    end
    record = l2_response_history_by_token[token];
    if (record.reset_epoch != l2_response_reset_epoch || sample < record.accept_sample ||
        beat_idx > 1 || record.beat_valid[beat_idx] || record.full_complete ||
        l2_active_response_token[record.is_release][record.source] != token) begin
        report_l2_d_error_fatal("D.fire response epoch/beat/source mismatch");
        return;
    end
    beat_record = '0;
    beat_record.valid = 1;
    beat_record.sample = sample;
    beat_record.denied = record.denied;
    beat_record.corrupt = record.corrupt;
    high_half = record.echo_is_keyword ^ (beat_idx == 1);
    beat_record.byte_mask = record.kind == 2 ?
        (high_half ? 64'hffff_ffff_0000_0000 : 64'h0000_0000_ffff_ffff) : 64'hffff_ffff_ffff_ffff;
    if (beat_idx == 0) record.beat0 = beat_record;
    else record.beat1 = beat_record;
    record.beat_valid[beat_idx] = 1;
    l2_response_history_by_token[token] = record;
endfunction:publish_l2_response_beat_history
```

中文伪代码：要求token存在；核对reset、sample、beat范围、未重复发布和活跃source。建立有效beat，复制raw及当次sample；GrantData依据冻结关键词与beat序号计算高/低32B范围，Ack覆盖整行。保存对应beat后置有效mask并发布，不要求另一beat完成。

### 3.7 `complete_l2_response_history()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:700`。

抽象功能描述：在末beat封闭完整响应并释放source。

```systemverilog
function void mem_access_base_sequence::complete_l2_response_history(input longint unsigned token);
    l2_response_history_t record;
    bit [1:0] required_mask;
    if (!l2_response_history_by_token.exists(token)) begin
        report_l2_d_error_fatal("completion references missing token");
        return;
    end
    record = l2_response_history_by_token[token];
    required_mask = record.kind == 2 ? 2'b11 : 2'b01;
    if (record.full_complete || record.beat_valid != required_mask ||
        record.reset_epoch != l2_response_reset_epoch ||
        l2_active_response_token[record.is_release][record.source] != token) begin
        report_l2_d_error_fatal("response completion before expected beats or duplicate completion");
        return;
    end
    record.full_complete = 1;
    record.complete_sample = record.kind == 2 ? record.beat1.sample : record.beat0.sample;
    l2_response_history_by_token[token] = record;
    l2_active_response_token[record.is_release][record.source] = 0;
endfunction:complete_l2_response_history
```

中文伪代码：要求token存在；根据响应类别确定预期mask，拒绝重复完成、缺beat、旧reset或source错配。标记full_complete，保留每beat原sample，仅以末beat记录完整完成sample；写回历史后释放活跃source槽。历史不随临时响应删除。

### 3.8 `query_l2_line_history_at_sample()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:722`。

抽象功能描述：提供无副作用的首次选择诊断。

```systemverilog
function bit mem_access_base_sequence::query_l2_line_history_at_sample(
    input mem_addr_t line_addr, input longint unsigned sample,
    output l2_d_error_line_record_t record);
    record = '0;
    if (!l2_d_error_ledger_ready || sample == 0) return 0;
    if (l2_line_history_by_line.exists(line_addr[47:6]) &&
        l2_line_history_by_line[line_addr[47:6]].accept_sample <= sample)
        record = l2_line_history_by_line[line_addr[47:6]];
    return 1;
endfunction:query_l2_line_history_at_sample
```

中文伪代码：清空输出；未初始化或sample零返回失败。仅当行已登记且接受sample不晚于查询才复制选择；未见行正常返回空。不会查询数据、随机或创建记录。

### 3.9 `query_l2_response_history()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:734`。

抽象功能描述：按token及sample核对本次已送达beat。

```systemverilog
function bit mem_access_base_sequence::query_l2_response_history(
    input longint unsigned token, input mem_addr_t line_addr,
    input longint unsigned sample, input bit [1:0] required_beats,
    output l2_response_history_t record, output bit beats_available);
    record = '0;
    beats_available = 0;
    if (!l2_d_error_ledger_ready || sample == 0 || required_beats == 0) return 0;
    if (!l2_response_history_by_token.exists(token)) return 1;
    record = l2_response_history_by_token[token];
    if (record.line_addr[47:6] != line_addr[47:6] || record.reset_epoch != l2_response_reset_epoch) return 0;
    if (record.beat0.sample > sample) begin record.beat_valid[0] = 0; record.beat0 = '0; end
    if (record.beat1.sample > sample) begin record.beat_valid[1] = 0; record.beat1 = '0; end
    if (record.complete_sample > sample) record.full_complete = 0;
    beats_available = (record.beat_valid & required_beats) == required_beats;
    return 1;
endfunction:query_l2_response_history
```

中文伪代码：清输出并默认无可用beat；未初始化、sample零、mask空返回失败。token不存在正常返回空；存在则复制并检查物理行与当前reset代际。过滤晚于查询sample的各beat，同时收敛完整完成标志，最后比较请求mask是否全部已送达。未送达不等价于CLEAN。

### 3.10 `find_l2_response_at_accept()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:751`。

抽象功能描述：给被动采样者提供精确接受到token的只读索引。

```systemverilog
function bit mem_access_base_sequence::find_l2_response_at_accept(
    input longint unsigned sample, input bit [9:0] source, input bit is_release,
    output longint unsigned token);
    l2_accept_key_t key;
    key = {l2_response_reset_epoch, sample, is_release, source};
    token = 0;
    if (!l2_d_error_ledger_ready || !l2_response_token_by_accept.exists(key)) return 0;
    token = l2_response_token_by_accept[key];
    return 1;
endfunction:find_l2_response_at_accept
```

中文伪代码：以当前reset代际、指定接受sample、方向和source构造key；清token输出，未初始化或未命中返回失败；命中仅复制token，不借当前主表猜owner。

### 3.11 `bind_l2_response_owner()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:762`。

抽象功能描述：把被动观察冻结的owner绑定到同一次真实接受。

```systemverilog
function bit mem_access_base_sequence::bind_l2_response_owner(
    input longint unsigned token, input longint unsigned accept_sample,
    input l2_response_owner_t owner);
    l2_response_history_t record;
    if (!owner.valid || !l2_response_history_by_token.exists(token)) return 0;
    record = l2_response_history_by_token[token];
    if (record.accept_sample != accept_sample || record.owner.valid ||
        record.reset_epoch != l2_response_reset_epoch) return 0;
    record.owner = owner;
    l2_response_history_by_token[token] = record;
    return 1;
endfunction:bind_l2_response_owner
```

中文伪代码：要求owner有效且token存在；记录的接受sample必须精确相等、owner尚未绑定、reset代际仍匹配，否则返回失败。满足后一次性复制owner并更新该token。采样水位推进与owner来源由调用方负责。

### 3.12 `classify_response_error_injection()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:3546`。

抽象功能描述：由既有协议类别与reqSource确定注入资格及权重。

```systemverilog
function bit dcache_mem__access_base_sequence::classify_response_error_injection(
    input dcache_agent_agent_xaction req_xact, output int unsigned kind,
    output int unsigned denied_weight, output int unsigned corrupt_weight);
    denied_weight = 0;
    corrupt_weight = 0;
    case (req_xact.auto_inner_dcache_client_out_a_bits_opcode)
        TL_A_OPCODE_CBO_CLEAN, TL_A_OPCODE_CBO_FLUSH, TL_A_OPCODE_CBO_INVAL: begin
            kind = DCACHE_PENDING_D_CBO_ACK;
            denied_weight = seq_csr_common::get_l2_cbo_ack_denied_wt();
            corrupt_weight = seq_csr_common::get_l2_cbo_ack_corrupt_wt();
            return 1;
        end
        TL_A_OPCODE_ACQUIRE_PERM: begin
            kind = DCACHE_PENDING_D_GRANT;
            if (req_xact.auto_inner_dcache_client_out_a_bits_user_reqSource != MEMBLOCK_REQ_CPU_STORE)
                `uvm_fatal(get_type_name(), "AcquirePerm requires Store reqSource")
            return 0;
        end
        TL_A_OPCODE_ACQUIRE_BLOCK: kind = DCACHE_PENDING_D_GRANT_DATA;
        default: begin
            `uvm_fatal(get_type_name(), "unsupported coherent A opcode in error classification")
            return 0;
        end
    endcase
    case (req_xact.auto_inner_dcache_client_out_a_bits_user_reqSource)
        MEMBLOCK_REQ_CPU_STORE: return 0;
        MEMBLOCK_REQ_CPU_LOAD, MEMBLOCK_REQ_CPU_ATOMIC, MEMBLOCK_REQ_L1_PREFETCH: begin
            denied_weight = seq_csr_common::get_l2_grantdata_denied_wt();
            corrupt_weight = seq_csr_common::get_l2_grantdata_corrupt_wt();
            return 1;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unknown reqSource=%0d at coherent A.fire",
                req_xact.auto_inner_dcache_client_out_a_bits_user_reqSource))
            return 0;
        end
    endcase
endfunction:classify_response_error_injection
```

中文伪代码：先把权重清零。CBO选择CBOAck和既有两权重；AcquirePerm要求Store来源并排除；AcquireBlock选择GrantData。Store来源直接排除；Load/Atomic/Prefetch读取既有GrantData权重；未知编码或不支持opcode明确fatal。

### 3.13 `reserve_l2_a_history()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:3586`。

抽象功能描述：在真实A接受且任何延期前冻结一次选择和响应身份。

```systemverilog
function longint unsigned dcache_mem__access_base_sequence::reserve_l2_a_history(
    input dcache_agent_agent_xaction req_xact);
    bit allowed;
    bit needs_candidate;
    bit denied;
    bit corrupt;
    int unsigned kind;
    int unsigned denied_weight;
    int unsigned corrupt_weight;
    longint unsigned sample;
    l2_d_error_state_e candidate;
    l2_d_error_line_record_t choice;
    mem_addr_t line_addr;
    line_addr = line_addr64(req_xact.auto_inner_dcache_client_out_a_bits_address);
    sample = current_l2_d_error_sample("coherent A.fire");
    allowed = classify_response_error_injection(req_xact, kind, denied_weight, corrupt_weight);
    if (!lookup_l2_line_choice(line_addr, sample,
            {4'b0,req_xact.auto_inner_dcache_client_out_a_bits_source}, kind,
            req_xact.auto_inner_dcache_client_out_a_bits_user_reqSource,
            0, L2_D_ERROR_NONE, needs_candidate, choice))
        `uvm_fatal(get_type_name(), "first line lookup failed")
    if (needs_candidate) begin
        candidate = L2_D_ERROR_NONE;
        if (allowed) begin
            if (sample_d_error_enable(denied_weight, "L2 first line denied")) candidate = L2_D_ERROR_DENIED;
            else if (sample_d_error_enable(corrupt_weight, "L2 first line corrupt")) candidate = L2_D_ERROR_CORRUPT;
        end
        if (!lookup_l2_line_choice(line_addr, sample,
                {4'b0,req_xact.auto_inner_dcache_client_out_a_bits_source}, kind,
                req_xact.auto_inner_dcache_client_out_a_bits_user_reqSource,
                1, candidate, needs_candidate, choice) || needs_candidate)
            `uvm_fatal(get_type_name(), "first line reservation failed")
    end
    denied = allowed && choice.state == L2_D_ERROR_DENIED;
    corrupt = allowed && (choice.state == L2_D_ERROR_CORRUPT ||
                           (choice.state == L2_D_ERROR_DENIED && kind == DCACHE_PENDING_D_GRANT_DATA));
    return begin_l2_response_history(line_addr, sample,
        {4'b0,req_xact.auto_inner_dcache_client_out_a_bits_source}, 0, kind,
        req_xact.auto_inner_dcache_client_out_a_bits_opcode,
        req_xact.auto_inner_dcache_client_out_a_bits_user_reqSource, allowed,
        denied, corrupt, req_xact.auto_inner_dcache_client_out_a_bits_echo_isKeyword);
endfunction:reserve_l2_a_history
```

中文伪代码：取对齐行与DUT采样序号，再由分类helper返回资格/权重。第一次查行只获取是否需要候选；缺行时排除类直接CLEAN，允许类先按denied权重、未命中再按corrupt权重选择并立即登记。随后只按本笔能力编码raw：Store永远0/0，CBO denied不附带corrupt，GrantData denied附带corrupt。交给响应历史入口分配token。

### 3.14 `start_alias_conflict()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:3131`。

抽象功能描述：保存原A请求及已冻结token，交由原Probe流程处理冲突。

```systemverilog
task dcache_mem__access_base_sequence::start_alias_conflict(
    input dcache_agent_agent_xaction req_xact,
    input longint unsigned           accept_cycle,
    input longint unsigned           response_token
);
    bit [47:0] line_addr;
    bit [9:0] reserved_sink;
    dcache_probe_token_t probe_token;
    dcache_cached_line_record_t old_line_record;
    dcache_cached_line_record_t line_record;

    if (!is_alias_conflict_request(req_xact)) begin
        `uvm_fatal(get_type_name(), "start_alias_conflict called for a non-conflicting A request")
    end
    line_addr = line_addr64(req_xact.auto_inner_dcache_client_out_a_bits_address);
    if (has_probe_for_line(line_addr) || !reserve_deferred_acquire_resources(reserved_sink)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("A.fire alias conflict cannot reserve Probe/Acquire resources line=0x%0h", line_addr))
    end
    old_line_record = cached_line_by_addr[line_addr];
    line_record = old_line_record;
    line_record.deferred_acquire = dcache_agent_agent_xaction::type_id::create("deferred_alias_acquire");
    line_record.deferred_acquire.copy(req_xact);
    line_record.deferred_acquire_valid      = 1'b1;
    line_record.deferred_response_reserved  = 1'b1;
    line_record.deferred_sink               = reserved_sink;
    line_record.deferred_accept_cycle       = accept_cycle;
    line_record.deferred_response_token     = response_token;
    line_record.lifecycle_state             = DCACHE_LINE_ALIAS_CONFLICT;
    cached_line_by_addr[line_addr]          = line_record;
    if (!submit_probe(line_addr,
                      TL_CAP_TON,
                      DCACHE_PROBE_OWNER_ALIAS_CONFLICT,
                      0,
                      probe_token)) begin
        cached_line_by_addr[line_addr] = old_line_record;
        release_deferred_acquire_resources(line_record);
        `uvm_fatal(get_type_name(),
                   $sformatf("A.fire alias conflict cannot create Probe record line=0x%0h", line_addr))
    end
endtask:start_alias_conflict
```

中文伪代码：要求真实别名冲突，检查同line不存在另一个Probe并预留响应/sink资源；复制完整request到行上下文，保存接受cycle和响应token，再设alias-conflict并创建Probe。创建失败恢复旧行并释放预留后fatal。后续恢复显式传回原token，不二次登记。

### 3.15 `enqueue_pending_cbo_ack()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:3173`。

抽象功能描述：把已固定的CBO上下文转成可调度Ack。

```systemverilog
task dcache_mem__access_base_sequence::enqueue_pending_cbo_ack(input longint unsigned complete_cycle);
    dcache_response_record_t response_record;

    if (!cbo_context_valid || !cbo_response_reserved) begin
        `uvm_fatal(get_type_name(), "CBOAck creation requires an active CBO response reservation")
    end
    response_record                = '{default:'0};
    response_record.kind           = DCACHE_PENDING_D_CBO_ACK;
    response_record.eligible_cycle = complete_cycle + 3;
    response_record.beat_count     = 1;
    response_record.size           = TL_LINE_SIZE;
    response_record.source         = pending_cbo_probe_source;
    response_record.line_addr      = pending_cbo_probe_line;
    response_record.cbo_opcode     = pending_cbo_probe_opcode;
    response_record.denied         = pending_cbo_ack_denied;
    response_record.corrupt        = pending_cbo_ack_corrupt;
    response_record.response_token = pending_cbo_response_token;

    // 将 CBO A.fire 时已经占用的一笔容量原子转换为可调度的 D response record。
    cbo_response_reserved = 1'b0;
    enqueue_dcache_response(response_record);
endtask:enqueue_pending_cbo_ack
```

中文伪代码：要求CBO owner及响应预留仍有效；填单beat Ack、原opcode/source/line、raw和原token，先撤销预留再调用既有响应入队，避免重复占容量。不补随机或重选。

### 3.16 `publish_probe_c_history()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:3107`。

抽象功能描述：记录DUT到L2的Probe回复方向与子token。

```systemverilog
function void dcache_mem__access_base_sequence::publish_probe_c_history(
    input dcache_probe_record_t probe_record, input int unsigned beat_idx,
    input bit is_data, input bit corrupt);
    l2_probe_history_t history;
    l2_response_beat_t beat_record;
    if (!l2_probe_history_by_token.exists(probe_record.history_token))
        `uvm_fatal(get_type_name(), "Probe C.fire missing history token")
    history = l2_probe_history_by_token[probe_record.history_token];
    if (history.reset_epoch != l2_response_reset_epoch || history.b_sample == 0 ||
        beat_idx > 1 || history.c_beat_valid[beat_idx] || history.full_complete)
        `uvm_fatal(get_type_name(), "Probe C.fire epoch/beat mismatch")
    beat_record = '0;
    beat_record.valid = 1;
    beat_record.sample = current_l2_d_error_sample("Probe C.fire");
    beat_record.corrupt = corrupt;
    beat_record.byte_mask = is_data ? (beat_idx == 0 ? 64'h0000_0000_ffff_ffff :
        64'hffff_ffff_0000_0000) : 64'hffff_ffff_ffff_ffff;
    if (beat_idx == 0) history.c_beat0 = beat_record;
    else history.c_beat1 = beat_record;
    history.c_beat_valid[beat_idx] = 1;
    history.full_complete = !is_data || beat_idx == 1;
    l2_probe_history_by_token[probe_record.history_token] = history;
endfunction:publish_probe_c_history
```

中文伪代码：查子历史并核对reset、B已fire、beat范围及不重复完成；建立实际C beat sample、corrupt和物理范围，保存到对应元素后更新mask和完整状态。该corrupt独立于L2注入选择，不能成为L2错误期望。

### 3.17 `process_d_fire()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:3854`。

抽象功能描述：把真实D握手接入逐beat发布并保留既有协议收敛。

```systemverilog
function void dcache_mem__access_base_sequence::process_d_fire();
    dcache_response_record_t completed_record;
    dcache_grant_ack_record_t grant_ack_record;

    if (!current_d_valid) begin
        `uvm_fatal(get_type_name(), "D.fire observed without a current DCache response record")
    end
    publish_l2_response_beat_history(current_d_record.response_token,
        current_d_record.beat_idx, current_l2_d_error_sample("D.fire"));
    if ((current_d_record.kind == DCACHE_PENDING_D_GRANT_DATA) &&
        ((current_d_record.beat_idx + 1) < current_d_record.beat_count)) begin
        current_d_record.beat_idx++;
        return;
    end

    completed_record = current_d_record;
    complete_l2_response_history(completed_record.response_token);
    case (completed_record.kind)
        DCACHE_PENDING_D_GRANT_DATA: begin
            grant_ack_record.line_addr = completed_record.line_addr;
            grant_ack_record.line_alias = completed_record.line_alias;
            grant_ack_record.sink      = completed_record.sink;
            grant_ack_record.response_token = completed_record.response_token;
            grant_ack_wait_q.push_back(grant_ack_record);
            mark_cached_line_grant_wait(completed_record.line_addr, completed_record.line_alias);
        end
        DCACHE_PENDING_D_GRANT: begin
            grant_ack_record.line_addr = completed_record.line_addr;
            grant_ack_record.line_alias = completed_record.line_alias;
            grant_ack_record.sink      = completed_record.sink;
            grant_ack_record.response_token = completed_record.response_token;
            grant_ack_wait_q.push_back(grant_ack_record);
            mark_cached_line_grant_wait(completed_record.line_addr, completed_record.line_alias);
        end
        DCACHE_PENDING_D_CBO_ACK: begin
            if (!cbo_context_valid || cbo_response_reserved || pending_cbo_probe_valid ||
                (completed_record.cbo_opcode != pending_cbo_probe_opcode) ||
                (completed_record.line_addr != pending_cbo_probe_line) ||
                (completed_record.source != pending_cbo_probe_source)) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("CBOAck D.fire does not match active CBO context source=%0d line=0x%0h opcode=%0d",
                                     completed_record.source,
                                     completed_record.line_addr,
                                     completed_record.cbo_opcode))
            end
            // CBO hit 的 toB/toN line 更新已在 Probe C 完成时完成；direct miss 没有 line 可删除。
            clear_cbo_context();
        end
        DCACHE_PENDING_D_RELEASE_ACK: begin
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("process_d_fire with invalid kind=%0d", completed_record.kind))
        end
    endcase
    clear_current_d_state();
endfunction:process_d_fire
```

中文伪代码：确认当前D有效，先发布该token当前beat；GrantData尚非末beat仅推进beat序号返回。末beat封闭历史后按原类别推进GrantAck/CBO/Release生命周期；GrantAck wait额外保留原token，CBO先校验上下文再释放；最后清临时D。

### 3.18 `process_e_fire()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:3911`。

抽象功能描述：沿原sink队列验证已经完成的Grant身份。

```systemverilog
function void dcache_mem__access_base_sequence::process_e_fire();
    logic [9:0] raw_observed_sink;
    bit [9:0] observed_sink;

    if (grant_ack_wait_q.size() == 0) begin
        `uvm_fatal(get_type_name(), "unexpected E.valid when no GrantAck is pending")
    end
    // 中文注释：严格 X/Z 检查开启时，先保留 DUT E.sink 的四态值；未知 payload
    // 只能报告错误，不能被折叠成 0 后误消费 GrantAck owner。
    raw_observed_sink = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink;
    if (seq_csr_common::get_hard_xz_check_en() &&
        $isunknown(raw_observed_sink)) begin
        `uvm_error(get_type_name(), "GrantAck E.bits.sink sampled as X/Z on E.fire")
        return;
    end
    // hard X/Z 检查关闭时保留原有 logic 到 bit 的转换和匹配行为。
    observed_sink = raw_observed_sink;
    foreach (grant_ack_wait_q[i]) begin
        if (grant_ack_wait_q[i].sink == observed_sink) begin
            if (!l2_response_history_by_token.exists(grant_ack_wait_q[i].response_token) ||
                !l2_response_history_by_token[grant_ack_wait_q[i].response_token].full_complete ||
                l2_response_history_by_token[grant_ack_wait_q[i].response_token].reset_epoch != l2_response_reset_epoch)
                `uvm_fatal(get_type_name(), "E.fire references missing or stale completed Grant")

            record_cached_line(grant_ack_wait_q[i].line_addr, grant_ack_wait_q[i].line_alias);
            grant_ack_wait_q.delete(i);
            return;
        end
    end
    `uvm_fatal(get_type_name(),
               $sformatf("GrantAck sink=%0d does not match any pending Grant owner", observed_sink))
endfunction:process_e_fire
```

中文伪代码：无等待Grant则fatal；保留四态sink并按既有严格模式检测未知。有限队列中命中sink后要求原token存在、完整完成且属于当前reset，再更新缓存行并删除原等待项；没有匹配sink则fatal。E不创建首次行选择。

## 4. 其余接入点与只读接口

`start_c_assembly()`在首C.fire先调用CLEAN登记；Release在此建立响应token，ReleaseData把token保存在assembly，末beat构造Ack复制；`consume_c_beat()`沿原固定两beat检查，并为Probe实际C回复逐beat记录。`complete_release_c_assembly()`保留原数据写入与错误策略，仅把原token传给Ack。

`process_probe_b_fire()`在真实B.fire登记/引用行选择并设置子token的B sample；`submit_probe()`保留固定最多16条队列，分配子token并按原owner指向CBO或alias原响应。`complete_probe_record()`的alias恢复显式传回旧token。以上不在高频路径扫描历史map。

`read_l2_d_error_for_rm()`清输出并只返回line_seen/state等诊断，始终不设置denied/corrupt或消费证明。新增`read_l2_response_for_rm()`按唯一token查询，已送达mask全部满足后才输出raw；查询错误和正常缺失区分。历史owner可通过基类精确接受查询和bind接口接入。

新增`try_read_uid_by_rob_for_rm()/try_read_uid_by_sq_for_rm()`按既有map单key探测，未初始化或缺key只返回false且UID清零，不生成UVM_ERROR；严格read接口维持原语义。

RM旧日志字段由sticky_enabled改为ledger_ready，尚未把旧Load/Atomic PA调用伪装成token消费；该切换是统一错误plan后续阶段责任。

## 5. 软件测试迁移与入口

新增`soft_test_l2_line_history_sequence`与`memblock_l2_line_history_directed_vseq`并接入seq_pkg；不新增生产API供测试控制。定向验证Store硬排除、已有错误行Store、CLEAN冻结、关键词首beat、sample过滤、Atomic/Prefetch能力、CBO编码及精确owner、跨响应类别、Release、PA不能授权错误、source/reset和新testcase清理。

原`soft_test_rm_provenance_followup_sequence`的边界检查改成token/beat查询；旧generation=0直接commit构造迁移为soft-local显式choice/token/beat fixture；同一行升级测试改用独立物理行。原first-split几何/owner/终止正向断言保留，等待统一消费观察接入，当前不能标记通过。Uncache隔离测试改用独立L2 token事实，保持原Uncache owner查询。

软件CSV：`AI_DOC/seq/memblock_l2_line_history_soft_framework_testcases_20261111.csv`；本阶段未新增真实DUT testcase。

## 6. 与 plan 不一致的实现

原plan没有约定位宽，实际reqSource使用5位而非截断为4位；这是IMPLEMENTATION_DELTA，不改变枚举值或资格策略。其余框架行为与plan一致。

## 7. Plan 未说明但实现补充的细节

精确接受索引包含reset代际，防止source复用或软件定向同sample复位后发生身份碰撞。owner绑定只能精确匹配接受sample。Probe保留单独子上下文历史，没有复制payload或第二份选择表。被动map探测使用不报错try helper。以上已在plan的IMPLEMENTATION_DELTA说明。

## 8. 静态检查、仿真与剩余依赖

已执行本阶段源码的`git diff --check`与删除项检索；尚未记录远端编译或仿真通过。主agent完成验证后更新本节和CSV，不能把仅编译当作所有路径验证。

建议入口：`make eda_compile tc=basicTest ts=memblock_l2_line_history_directed_vseq mode=base_fun`，之后运行同入口；基础virtual_base_sequence及真实dispatch smoke按执行规则运行。

阶段依赖：现有RM只地址查询不再得到永久错误；权重0基础可执行，带注错Load/Atomic及first-split正向必须由统一错误plan切换真实消费token/slot版本后验证。CBO owner被动采集绑定和Uncache通知属于后续阶段。本阶段未移动plan到do、未提交代码。


## 12. 主 agent 独立复核与实际验证

主 agent 已逐项阅读暂存层 diff、首次选择、A延期、D逐beat、Store排除和soft断言；独立 subagent 复核同一暂存层未发现必改项。远端 `eda01` VCS Q-2020.03-SP2 完整编译通过，定向入口 `basicTest / memblock_l2_line_history_directed_vseq`、seed `666666` 实际运行通过，`UVM_ERROR=0`、`UVM_FATAL=0`，仿真时间265.300ns。日志位于 `mem_ut/ver/ut/memblock/sim/cmo_plans_20261111/log/tc=basicTest_ts=memblock_l2_line_history_directed_vseq_cfg=default_seed=666666_rtl.log`。

原base_fun目录的旧VCS数据库损坏，故使用独立输出目录 `mode=cmo_plans_20261111`，没有修改编译脚本和RTL。首次软件用例与新增monitor复位边界问题已修正，并重新编译和运行取得上述结果。基础/真实DUT联调、统一消费observer、Uncache扩展属于同任务后续阶段，本提交不宣称三份plan均已完成，plan继续保留undo，待完整闭环后归档。

基础场景 `basicTest / virtual_base_sequence` 与真实 DUT `basicTest / memblock_dispatch_real_smoke_vseq / tc_dispatch_real_smoke` 均于同一mode、seed666666运行完成，UVM_ERROR与UVM_FATAL均0。真实smoke覆盖1条页权限fault并完成RM异常比较、terminal与responder drain，不能据此声称已覆盖普通cache命中或错误Load；这些由后续专项继续验证。
