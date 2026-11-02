# L2TLB 可路由物理页随机覆盖实现评审

日期：2026-11-02；版本：V2。范围：本轮方案对应源码、配置、专项和文档；结论为本方案源码静态评审及16次计划验收通过；额外混合NC迟到写回问题尚待分类，边界见第5.4节。
关联方案：`AI_DOC/plan/test_framework/plan/do/memblock_l2tlb_routed_ppn_coverage_plan_20261102.md`。
独立方案评审：`AI_DOC/plan/test_framework/review_doc/undo/memblock_l2tlb_routed_ppn_plan_review_20261102.md`。
本文记录独立源码审查与主执行者核对的最终证据，不把尚未分类的额外失败写成通过。

## 1. 术语与抽象功能说明

本改动在新翻译构造时生成实际可到达外部响应端的物理映射，复用PMA配置版本，保留合法cacheable候选，并由上游意图覆盖小于一页的设备窗口。现有RM仍独立判断最终访问结果。

| 英文术语 | 当前中文含义 | 对应代码对象或落点 | 示例 |
|---|---|---|---|
| DUT / RTL | 被测MemBlock及生成的硬件代码 | `build/rtl/MemBlock.sv` | UT仍包含内部地址译码 |
| PPN / PA / VPN | 物理页号、物理字节地址、虚拟页号 | `tlb_map_builder` | PA结合PPN及低12位偏移 |
| PMA / PMP | 物理属性表和物理权限表 | `memblock_pma_pmp_model` | c=1不增加设备接收者 |
| PBMT / NC / IO / MMIO | 翻译访问属性、不可缓存主存、IO属性、设备访问分类 | S1/S2 PBMT及现有RM | PBMT00+c0保持c0类别 |
| CSR / sample / replay | 配置寄存器、采样序号、观测写事件回放 | 公共数据owner | C采集写于C+2生效 |
| generation / snapshot / span | 配置版本、固定版本副本、半开字节区间 | `pma_history`及导出结构 | 上界可表达2^48 |
| canonical / sector | 公共翻译编码及S1页扇区 | `memblock_tlb_entry` | 新页选择后重建sector |
| shape / NAPOT | 页覆盖形状及连续页编码 | level/N字段 | 64KiB范围整体检查 |
| allStage / GPA / tag | 两阶段翻译、中间客户物理地址、响应匹配标识 | S1输出及S2输入 | 最终地址仅修改S2 |
| intent | 主表在发布前登记的目标页及偏移约束 | `routed_ppn_intent_by_vpn` | UART偏移0..127 |
| reuse / history / FIFO | 已完成响应页复用、有界历史、顺序队列 | 公共PPN历史 | 新entry重查资格 |
| token / owner / epoch | 一次真实请求、唯一状态管理者、重执行代际 | L2 responder及公共状态 | 不新增第二队列owner |
| strict / fault / fallback | 严格范围、预期异常、一次4KiB降级 | 范围开关及builder | 显式大页冲突不降级 |
| vseq / main / barrier / drain | 场景调度、主表序列、启动屏障、自然排空 | real-smoke派生场景 | 静态CSR就绪后issue |
| A.fire / RM | 外部请求真实握手、参考模型 | sbuffer接口及原RM | 选择记录不能代替到达证据 |
| fixture / override | 软件测试夹具、类型覆盖机制 | soft专项及UVM工厂 | 不用全局类型覆盖替换main |

## 2. 修改前后与调用职责

修改前：sparse候选全部被压入512GiB DRAM；普通cacheable与NC/IO共用该限制。PBMT在各stage页号生成后选择，主表不表达窄设备偏移；PPN复用没有新路由过滤。

修改后：请求owner先回放成熟CSR并取固定版本，准备阶段冻结PBMT；普通候选按最终stage及完整shape判断，安全cacheable候选保持，其他候选从同类可路由集合选择。窄专项先生成合法偏移和独立VPN；旧fault/权限/strict路径保持兼容。

| 顺序 | 入口 | 职责与后续影响 |
|---|---|---|
| 1 | `capture_fired_request` | 保存真实sample并传公共查询；不从后来的CSR替换请求上下文 |
| 2 | `get_or_create_l2tlb_entry_by_req_with_snapshot` | 构造前回放；已有命中不改物理映射，miss才构造 |
| 3 | `configure_builder_routed_context` | 传版本和debug、只读意图，不消费FIFO |
| 4 | `read_pma_attribute_spans`及policy配置 | 按原匹配规则导出值型区间，不建立第二PMA真源 |
| 5 | prepare/finalize及最终页策略 | 冻结属性、编码阶段、选择物理页并维护字段一致性 |
| 6 | reuse过滤和publish | 对history重新校验，只发布一次新entry |
| 7 | 窄main/vseq | 在issue前固定偏移，复用真实agent生命周期并记录实际握手 |

## 3. 核心源码逐点核对

### 3.1 `read_pma_attribute_spans()`

抽象功能描述：供新翻译构造读取指定PMA历史版本，返回互不重叠的值型属性区间，不回放写事件、不改UID权限上下文。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_pma_pmp_model.sv:1478`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
function bit memblock_pma_pmp_model::read_pma_attribute_spans(
    input longint unsigned generation,
    input bit debug,
    output memblock_pma_attribute_span_t spans[$]
);
    pma_pmp_table_snapshot snapshot;
    bit [48:0] boundaries[$];
    bit [48:0] unique_boundaries[$];
    bit [48:0] first;
    bit [48:0] limit;
    pma_pmp_entry_t entry;
    pma_pmp_entry_t matched_entry;
    memblock_pma_attribute_span_t span;
    int signed hit_index;
    int last_index;

    spans.delete();
    if (!ready) return 1'b0;
    snapshot = get_table_snapshot(1'b1, generation);
    if (snapshot == null) return 1'b0;
    boundaries.push_back(49'd0);
    boundaries.push_back(49'h1_0000_0000_0000);
    boundaries.push_back({1'b0, PMA_PMP_DEBUG_START});
    boundaries.push_back({1'b0, PMA_PMP_DEBUG_END} + 49'd1);
    for (int unsigned idx = 0; idx < PMA_PMP_ENTRY_COUNT; idx++) begin
        entry = snapshot.entries[idx];
        // OFF 的 compare_addr 仍是后一 TOR 项的下界，不得跳过此边界。
        boundaries.push_back({1'b0, entry.compare_addr});
        if (!entry.valid) continue;
        case (entry.a)
            PMA_PMP_A_NA4: begin
                first = {1'b0, entry.addr_raw, 2'b00};
                boundaries.push_back(first);
                boundaries.push_back(first + 49'd4);
            end
            PMA_PMP_A_NAPOT: begin
                first = {1'b0, (entry.compare_addr & ~entry.match_mask)};
                limit = first + {1'b0, entry.match_mask} + 49'd1;
                boundaries.push_back(first);
                boundaries.push_back(limit);
            end
            default: begin end
        endcase
    end
    boundaries.sort();
    foreach (boundaries[idx]) begin
        if (unique_boundaries.size() == 0 ||
            unique_boundaries[unique_boundaries.size()-1] != boundaries[idx])
            unique_boundaries.push_back(boundaries[idx]);
    end
    for (int unsigned idx = 0; idx + 1 < unique_boundaries.size(); idx++) begin
        span = '{default:'0};
        span.first = unique_boundaries[idx];
        span.limit = unique_boundaries[idx+1];
        // 无匹配时 c/r/w/x/atomic 保持原模型的全零默认值。
        if (find_first_match(snapshot, span.first[47:0], 1, debug,
                             hit_index, matched_entry)) begin
            span.c = matched_entry.c;
            span.r = matched_entry.r;
            span.w = matched_entry.w;
            span.x = matched_entry.x;
            span.atomic = matched_entry.atomic;
        end
        last_index = spans.size() - 1;
        if (last_index >= 0 && spans[last_index].limit == span.first &&
            spans[last_index].c == span.c && spans[last_index].r == span.r &&
            spans[last_index].w == span.w && spans[last_index].x == span.x &&
            spans[last_index].atomic == span.atomic) begin
            spans[last_index].limit = span.limit;
        end else begin
            spans.push_back(span);
        end
    end
    return spans.size() != 0;
endfunction:read_pma_attribute_spans
```
中文伪代码：此函数把固定配置版本转换为地址选择输入。先清输出并拒绝未就绪或缺失版本；收集物理上下界、debug保护区和各项比较边界，OFF项边界仍供后一TOR项使用；NA4/NAPOT再加入精确边界。排序去重后逐相邻区间用现有find_first_match按优先级判断属性，未匹配保留原模型全零默认；相邻完全相同属性合并，最后返回是否获得区间。get_table_snapshot只找历史版本；find_first_match保留debug和TOR/NAPOT规则，不复制新的权限算法。


### 3.2 `configure()`

抽象功能描述：为一次构造冻结属性区间副本，后续选择和复用共同读取该版本。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_routed_ppn_policy.sv:33`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit configure(input memblock_pma_pmp_model model,
                           input longint unsigned generation,
                           input bit debug);
        memblock_pma_attribute_span_t attributes[$];
        c_span_t span;
        int last_index;
        configured = 1'b0;
        c_spans.delete();
        pma_generation = 0;
        if (model == null ||
            !model.read_pma_attribute_spans(generation, debug, attributes))
            return 1'b0;
        foreach (attributes[idx]) begin
            span.first = attributes[idx].first;
            span.limit = attributes[idx].limit;
            span.c = attributes[idx].c;
            last_index = c_spans.size()-1;
            if (last_index >= 0 && c_spans[last_index].limit == span.first &&
                c_spans[last_index].c == span.c)
                c_spans[last_index].limit = span.limit;
            else c_spans.push_back(span);
        end
        pma_generation = generation;
        configured = 1'b1;
        return 1'b1;
    endfunction:configure
```
中文伪代码：先清configured、旧区间和版本号；读取指定版本失败即返回失败，使旧快照不可继续使用。逐项复制属性并合并相邻同c区间，允许shape跨越权限不同但c相同的边界；最后保存版本、置配置有效并返回成功。read_pma_attribute_spans只提供值型区间，模型本体不变。


### 3.3 `is_external_span()`

抽象功能描述：供普通页和窄意图共同判断完整字节区间是否由外部响应端接收。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_routed_ppn_policy.sv:95`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    static function bit is_external_span(input bit [63:0] first,
                                         input bit [63:0] limit);
        bit [48:0] route_first;
        bit [48:0] route_limit;
        if (first >= limit || limit > 64'h0001_0000_0000_0000) return 1'b0;
        for (int unsigned idx = 0; idx < 4; idx++) begin
            if (get_external_region(idx, route_first, route_limit) &&
                first >= {15'b0, route_first} && limit <= {15'b0, route_limit}) return 1'b1;
        end
        return 1'b0;
    endfunction:is_external_span
```
中文伪代码：先拒绝空区间、反向区间和超过2^48的上界，再遍历固定四段外部区间；全部字节被某一段包含则成功，否则失败。get_external_region把profile常量读为宽边界，不访问或修改DUT。内部控制器128B及APLIC空洞自然不在集合内。


### 3.4 `query_span_c()`

抽象功能描述：按已配置快照判断一个完整区间是否具有单一c属性，不查询运行中可变表。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_routed_ppn_policy.sv:107`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit query_span_c(input bit [63:0] first,
                              input bit [63:0] limit,
                              output bit cacheable);
        cacheable = 1'b0;
        if (!configured || first >= limit ||
            limit > 64'h0001_0000_0000_0000) return 1'b0;
        foreach (c_spans[idx]) begin
            if (first >= {15'b0, c_spans[idx].first} &&
                limit <= {15'b0, c_spans[idx].limit}) begin
                cacheable = c_spans[idx].c;
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:query_span_c
```
中文伪代码：先把输出初始化为c0；拒绝未配置、空区间和超位宽；逐固定属性区间寻找完整包含，命中则返回该c及成功，否则失败。query_c以一个字节区间调用此函数，因而不会用大页大小冒充scalar访问尺寸。


### 3.5 `check_mapping()`

抽象功能描述：供初选和reuse校验完整页形状，给出纯读取资格结果。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_routed_ppn_policy.sv:136`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit check_mapping(input bit [43:0] ppn,
                               input int unsigned shape_shift,
                               input bit [1:0] pbmt,
                               input bit target_c);
        bit [63:0] first;
        bit [63:0] limit;
        bit [63:0] shape_bytes;
        bit actual_c;
        if (!configured || !valid_shape(shape_shift) || pbmt == 2'b11)
            return 1'b0;
        shape_bytes = 64'd1 << shape_shift;
        first = {8'b0, ppn, 12'b0};
        // 在宽位检查前不截断PA；清页形状低位只影响页内重组位。
        if (first >= 64'h0001_0000_0000_0000) return 1'b0;
        first &= ~(shape_bytes-1);
        limit = first + shape_bytes;
        if (!is_external_span(first, limit)) return 1'b0;
        if (pbmt == 2'b00)
            return query_span_c(first, limit, actual_c) && actual_c == target_c;
        return 1'b1;
    endfunction:check_mapping
```
中文伪代码：拒绝未配置、非支持形状或PBMT11；使用64位构造PA并在截断前拒绝超48位；按shape清低位得到覆盖基址及上界。外部区间不完整包含则失败；PBMT00必须由同一c区间完整包含且等于目标c，NC/IO只要求外部路由。valid_shape限制五种支持尺寸；is_external_span检查路由；query_span_c检查属性，不修改entry或历史。


### 3.6 `select_ppn()`

抽象功能描述：把候选映射保留或选择为配置允许的完整shape地址，无候选返回失败供调用者一次降级。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_routed_ppn_policy.sv:161`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit select_ppn(input bit [43:0] candidate_ppn,
                            input bit [43:0] anchor_resolved_ppn,
                            input int unsigned shape_shift,
                            input bit [1:0] pbmt,
                            input int unsigned random_wt,
                            input int signed fixed_id,
                            output bit [43:0] selected_ppn,
                            output int signed region_id);
        candidate_span_t candidates[$];
        candidate_span_t candidate;
        int signed available_regions[$];
        bit [48:0] region_first;
        bit [48:0] region_limit;
        bit [48:0] route_first;
        bit [48:0] route_limit;
        bit [63:0] first;
        bit [63:0] limit;
        bit [63:0] aligned_first;
        bit [63:0] aligned_limit;
        bit [63:0] shape_bytes;
        bit [63:0] candidate_pa;
        bit [63:0] anchor_pa;
        bit [63:0] total_slots;
        bit [63:0] slot_pick;
        bit [63:0] selected_pa;
        int unsigned random_pick;
        int unsigned region_pick;
        int signed selected_region;
        int unsigned count_before;
        int unsigned span_count;
        bit target_c;
        bit constrain_c;
        bit candidate_ok;

        selected_ppn = '0;
        region_id = -1;
        if (!configured || !valid_shape(shape_shift) || pbmt == 2'b11 ||
            random_wt > 100 || fixed_id < -1 || fixed_id > 6) return 1'b0;
        shape_bytes = 64'd1 << shape_shift;
        candidate_pa = {8'b0, candidate_ppn, 12'b0};
        anchor_pa = {8'b0, anchor_resolved_ppn, 12'b0};
        target_c = 1'b0;
        constrain_c = pbmt == 2'b00 &&
                      query_c(anchor_pa, target_c);
        candidate_ok = check_mapping(candidate_ppn, shape_shift, pbmt,
                                     target_c);
        // 超48bit anchor不能截断来创造c类别，合法性仍由route限制。
        if (pbmt == 2'b00 && !constrain_c) candidate_ok = 1'b0;
        if (fixed_id == -1 && candidate_ok) begin
            if (pbmt == 2'b00 && target_c) begin
                selected_pa = candidate_pa & ~(shape_bytes-1);
                selected_ppn = selected_pa[55:12];
                region_id = 0;
                return 1'b1;
            end
            random_pick = 100;
            if (random_wt != 0 && random_wt != 100 &&
                !std::randomize(random_pick) with { random_pick < 100; })
                return 1'b0;
            if (random_wt == 0 || (random_wt != 100 && random_pick >= random_wt)) begin
                selected_pa = candidate_pa & ~(shape_bytes-1);
                selected_ppn = selected_pa[55:12];
                region_id = 0;
                return 1'b1;
            end
        end
        for (int signed id = 0; id <= 6; id++) begin
            if (fixed_id >= 0 && id != fixed_id) continue;
            if (!get_region(id, region_first, region_limit)) continue;
            count_before = candidates.size();
            for (int unsigned route = 0; route < 4; route++) begin
                if (!get_external_region(route, route_first, route_limit)) continue;
                span_count = constrain_c ? c_spans.size() : 1;
                for (int unsigned idx = 0; idx < span_count; idx++) begin
                    if (constrain_c && c_spans[idx].c != target_c) continue;
                    first = region_first > route_first ? {15'b0, region_first} : {15'b0, route_first};
                    limit = region_limit < route_limit ? {15'b0, region_limit} : {15'b0, route_limit};
                    if (constrain_c) begin
                        if (first < {15'b0, c_spans[idx].first}) first = {15'b0, c_spans[idx].first};
                        if (limit > {15'b0, c_spans[idx].limit}) limit = {15'b0, c_spans[idx].limit};
                    end
                    if (first >= limit) continue;
                    aligned_first = (first + shape_bytes-1) & ~(shape_bytes-1);
                    aligned_limit = limit & ~(shape_bytes-1);
                    if (aligned_first >= aligned_limit) continue;
                    candidate.first = aligned_first;
                    candidate.slots = (aligned_limit-aligned_first) >> shape_shift;
                    candidate.region_id = id;
                    candidates.push_back(candidate);
                end
            end
            if (candidates.size() != count_before) available_regions.push_back(id);
        end
        if (available_regions.size() == 0) return 1'b0;
        region_pick = 0;
        if (available_regions.size() > 1 && !std::randomize(region_pick) with {
                region_pick < available_regions.size();
            }) return 1'b0;
        selected_region = available_regions[region_pick];
        total_slots = 0;
        foreach (candidates[idx]) begin
            if (candidates[idx].region_id == selected_region)
                total_slots += candidates[idx].slots;
        end
        slot_pick = 0;
        if (total_slots > 1 && !std::randomize(slot_pick) with {
                slot_pick < total_slots;
            }) return 1'b0;
        foreach (candidates[idx]) begin
            if (candidates[idx].region_id != selected_region) continue;
            if (slot_pick < candidates[idx].slots) begin
                selected_pa = candidates[idx].first + (slot_pick << shape_shift);
                selected_ppn = selected_pa[55:12];
                region_id = selected_region;
                return check_mapping(selected_ppn, shape_shift,
                                     constrain_c ? pbmt : 2'b01, target_c);
            end
            slot_pick -= candidates[idx].slots;
        end
        return 1'b0;
    endfunction:select_ppn
```
中文伪代码：先清输出并校验配置、形状、PBMT、概率及窗口ID。宽地址原anchor可查询时固定目标c，不能表示则不创造截断c类别。用check_mapping验证原候选；无定向窗口时，安全c1候选立即保留，其他安全候选按概率保留。需重选时按七窗口、四路由及固定PMA区间枚举交集；PBMT00仅纳入目标c，按shape向内对齐并计算槽数，空集合跳过。先等权选有候选的窗口，再按该窗口可用槽数选择位置，最后再次校验完整映射并返回。所有循环有固定上界，没有无限重试；get_region只读命名窗口，query_c与check_mapping共享同一快照。


### 3.7 `configure_routed_ppn()`

抽象功能描述：接收公共owner准备的固定版本与意图，为本次builder设置只读策略。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:35`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function void configure_routed_ppn(input memblock_pma_pmp_model model,
                                       input longint unsigned generation,
                                       input bit debug,
                                       input memblock_routed_ppn_intent_t intent);
        routed_policy = memblock_routed_ppn_policy::type_id::create("routed_ppn_policy");
        if (model == null || !routed_policy.configure(model, generation, debug))
            `uvm_fatal("L2TLB_ROUTED_PPN", "PMA snapshot unavailable for routed PPN construction")
        routed_intent = intent;
    endfunction:configure_routed_ppn
```
中文伪代码：创建策略对象，以模型、版本和debug配置；模型为空或配置失败立即报告不可构造；随后复制意图。policy.configure导出固定版本，不消费CSR队列；失败不能继续用旧属性。


### 3.8 `routed_policy_active()`

抽象功能描述：决定本次entry是否参与普通地址策略，保留已有专项语义。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:46`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit routed_policy_active(input memblock_tlb_entry entry);
        return entry != null && !seq_csr_common::get_main_mem_ranges_en() &&
            !memblock_sync_pkg::csr_special_sequence_active &&
            !entry.has_effective_fault() && !entry.pmaAF &&
            (!entry.s1_stage_active || entry.s1_pte_mode_at_build ==
                memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL) &&
            (!entry.s2_stage_active || entry.s2_pte_mode_at_build ==
                memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL);
    endfunction:routed_policy_active
```
中文伪代码：仅在非空、sparse、非CSR权限专项、无有效翻译fault和PMA AF，且active stage均为LEGAL模式时返回真；其他情况返回假。此资格被新构造和reuse共同使用，避免只保护一条路径。


### 3.9 `effective_pbmt()`

抽象功能描述：从冻结stage属性确定最终访问属性，不重新随机。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:56`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit [1:0] effective_pbmt(input memblock_tlb_entry entry);
        case (entry.s2xlate)
            2'd0, 2'd1: return entry.s1_entry_pbmt;
            2'd2: return entry.s2_entry_pbmt;
            default: return entry.s1_entry_pbmt != 0 ?
                entry.s1_entry_pbmt : entry.s2_entry_pbmt;
        endcase
    endfunction:effective_pbmt
```
中文伪代码：S1-only返回S1，S2-only返回S2，allStage优先S1非零否则S2；结果交给地址策略。stage是否允许非零值已经由prepare的CSR gate决定。


### 3.10 `select_final_routed_ppn()`

抽象功能描述：在新entry最终stage决定合法页及一次降级，保持其他stage已冻结的映射。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:75`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit select_final_routed_ppn(input memblock_tlb_entry entry,
                                         input bit s1,
                                         input bit [37:0] input_vpn,
                                         input bit directed_shape,
                                         ref bit [43:0] canonical_ppn,
                                         output bit fell_back);
        bit [1:0] level;
        bit napot;
        bit [43:0] anchor, selected_ppn;
        int signed region;
        int unsigned shift;
        bit [63:0] intent_first, intent_limit;
        fell_back = 1'b0;
        if (!routed_policy_active(entry)) return 1'b1;
        if (routed_policy == null) begin
            memblock_pma_pmp_model model;
            longint unsigned pmp_gen, pma_gen;
            model = memblock_pma_pmp_model::get();
            if (!model.is_ready())
                `uvm_fatal("L2TLB_ROUTED_PPN", "routed PPN requires initialized PMA model")
            model.get_current_generations(pmp_gen, pma_gen);
            configure_routed_ppn(model, pma_gen, 1'b0, '{default:'0});
        end
        level = s1 ? entry.s1_level : entry.s2_level;
        napot = s1 ? entry.s1_pte_n : entry.s2_pte_n;
        if (routed_intent.valid) begin
            intent_first = {8'b0, routed_intent.ppn, 12'b0} + routed_intent.first_offset;
            intent_limit = {8'b0, routed_intent.ppn, 12'b0} + routed_intent.last_offset + 1;
            if (level != 0 || napot ||
                !memblock_routed_ppn_policy::is_external_span(intent_first, intent_limit))
                `uvm_fatal("L2TLB_ROUTED_INTENT", "narrow intent has invalid shape or route")
            canonical_ppn = routed_intent.ppn;
            return 1'b1;
        end
        for (int attempt = 0; attempt < 2; attempt++) begin
            shift = routed_shape_shift(level, napot);
            anchor = resolve_ppn_from_raw(canonical_ppn, input_vpn, level, napot);
            if (routed_policy.select_ppn(canonical_ppn, anchor, shift,
                    effective_pbmt(entry), seq_csr_common::get_l2tlb_ppn_region_random_wt(),
                    seq_csr_common::get_l2tlb_ppn_region_id(), selected_ppn, region)) begin
                canonical_ppn = selected_ppn;
                apply_legal_napot_ppn_encoding(s1, level, napot, canonical_ppn, entry);
                `uvm_info("L2TLB_ROUTED_PPN",
                    $sformatf("vpn=0x%0h s2xlate=%0d final_stage=%s ppn=0x%0h pbmt=%0d window=%0d shift=%0d fallback=%0b",
                        entry.lookup_key.vpn, entry.s2xlate, s1 ? "S1" : "S2",
                        canonical_ppn, effective_pbmt(entry), region, shift, fell_back), UVM_LOW)
                return 1'b1;
            end
            if (directed_shape || (level == 0 && !napot)) return 1'b0;
            level = 0; napot = 0;
            if (s1) begin entry.s1_level = 0; entry.s1_pte_n = 0; end
            else begin entry.s2_level = 0; entry.s2_pte_n = 0; end
            // 中文注释：保留第一次resolved anchor作4K候选，冻结PBMT00原c类别。
            // 重新hash会改变地址，可能把原c0权限域静默变为c1。
            canonical_ppn = anchor;
            fell_back = 1'b1;
        end
        return 1'b0;
    endfunction:select_final_routed_ppn
```
中文伪代码：先清降级结果；旁路场景直接保留原候选。无显式策略的兼容软件入口只取已初始化当前版本，不重置模型；读取当前level/N。若有意图，要求4KiB/N0且声明偏移全部外部可达，复制指定页并结束。普通路径最多尝试两次：解析本次anchor，按最终PBMT及窗口策略选择；成功则更新canonical并完成NAPOT编码、记录日志返回。失败时显式shape或已是4KiB则返回失败；否则只把最终stage降为level0/N0，将第一次resolved anchor保留为4KiB候选，标记降级后再试一次。resolve_ppn_from_raw执行页拼接；policy.select_ppn只选地址；apply_legal_napot_ppn_encoding恢复合法raw标记。


### 3.11 `make_canonical_ppn()`

抽象功能描述：生成确定性raw候选，最终可达性由后续策略决定。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:264`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit [43:0] make_canonical_ppn(input bit [37:0] vpn,
                                           input bit [43:0] root_ppn,
                                           input bit [1:0] level,
                                           input bit s2,
                                           input bit legacy_sparse = 1'b0);
        bit [43:0] ppn;
        bit [63:0] page_count;
        bit [63:0] page_offset;

        if (seq_csr_common::get_main_mem_ranges_en()) begin
            page_count = seq_csr_common::get_paddr_range() >> 12;
            if (page_count == 0) begin
                `uvm_fatal("L2TLB_PAYLOAD_PPN", "configured paddr range has no page")
            end
            page_offset = {26'b0, vpn} % page_count;
            ppn = (seq_csr_common::get_paddr_base() >> 12) + page_offset;
        end
        else begin
            // 中文注释：普通sparse恢复确定性候选；最终PA由select_final_routed_ppn合法化。
            // 权限/fault旁路仍使用旧B08窗口，保持该专项原始响应构造语义。
            ppn = root_ppn ^ {6'b0, vpn} ^
                  (s2 ? 44'h0000_005a_5a5 : 44'h0000_003c_3c3);
            if (legacy_sparse) begin
                page_count = MEMBLOCK_SPARSE_PADDR_RANGE >> 12;
                page_offset = ppn % page_count;
                ppn = (MEMBLOCK_SPARSE_PADDR_BASE >> 12) + page_offset;
            end
        end
        case (level)
            2'd1: ppn[8:0]   = '0;
            2'd2: ppn[17:0]  = '0;
            2'd3: ppn[26:0]  = '0;
            default: begin end
        endcase
        return ppn;
    endfunction:make_canonical_ppn
```
中文伪代码：strict按配置范围页数映射并拒绝空范围；sparse先计算原root/VPN/stage盐的XOR，仅legacy旁路才压入旧512GiB窗口；再按level清低位，返回候选。它不把candidate当最终可访问地址，也不改PBMT和权限。


### 3.12 `check_reused_routed_ppn()`

抽象功能描述：按新entry与同版本PMA判断历史PPN可否复用，不改历史记录。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:348`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit check_reused_routed_ppn(input memblock_tlb_entry entry,
                                         input bit [43:0] candidate_ppn);
        bit target_c;
        bit [43:0] current_ppn;
        bit [63:0] current_pa;
        if (!routed_policy_active(entry)) return 1'b1;
        if (routed_policy == null) begin
            memblock_pma_pmp_model model;
            longint unsigned pmp_gen, pma_gen;
            model = memblock_pma_pmp_model::get();
            model.get_current_generations(pmp_gen, pma_gen);
            configure_routed_ppn(model, pma_gen, 1'b0, '{default:'0});
        end
        if (routed_intent.valid) return 1'b0;
        current_ppn = entry.s2_stage_active ? entry.s2_resolved_ppn : entry.s1_resolved_ppn;
        current_pa = {8'b0, current_ppn, 12'b0};
        if (!routed_policy.query_c(current_pa, target_c)) return 1'b0;
        return routed_policy.check_mapping(candidate_ppn, 12, effective_pbmt(entry), target_c);
    endfunction:check_reused_routed_ppn
```
中文伪代码：旁路场景保持原资格；缺少策略的兼容入口只读当前模型配置；意图场景拒绝复用。选择当前最终stage解析后的页，查询它的c，查询失败拒绝；随后对历史候选做4KiB完整路由与目标类别检查。旧can_apply_reused_final_ppn仍先负责shape、fault、leaf及接口资格，本函数只增加地址策略。


### 3.13 `register_routed_ppn_intent()`

抽象功能描述：在主表发布前登记上游输入约束，后续builder只读，不建立RM期望表。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:936`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit register_routed_ppn_intent(input bit [37:0] vpn,
                                            input memblock_routed_ppn_intent_t intent);
        bit [63:0] first, limit;
        if (!intent.valid || intent.first_offset > intent.last_offset ||
            seq_csr_common::get_main_mem_ranges_en() ||
            seq_csr_common::get_l2tlb_ppn_reuse_en()) return 1'b0;
        first = {8'b0, intent.ppn, 12'b0} + intent.first_offset;
        limit = {8'b0, intent.ppn, 12'b0} + intent.last_offset + 1;
        if (!memblock_routed_ppn_policy::is_external_span(first, limit)) return 1'b0;
        if (routed_ppn_intent_by_vpn.exists(vpn) &&
            routed_ppn_intent_by_vpn[vpn] != intent) return 1'b0;
        routed_ppn_intent_by_vpn[vpn] = intent;
        return 1'b1;
    endfunction:register_routed_ppn_intent
```
中文伪代码：拒绝无效或反向偏移、strict和reuse组合；用宽PA形成完整声明偏移范围并检查外部可达；同VPN不同内容拒绝，相同内容保持幂等；随后存入值型map并成功。reset_all_tables统一清此map，普通场景不创建它。


### 3.14 `configure_builder_routed_context()`

抽象功能描述：向builder传固定模型版本、debug和可选意图，不自行消费raw队列。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:952`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function void configure_builder_routed_context(input tlb_map_builder builder,
        input memblock_tlb_lookup_key_t key, input mmu_csr_runtime_state csr,
        input longint unsigned query_sample = 0);
        longint unsigned pmp_gen, pma_gen;
        memblock_routed_ppn_intent_t intent;
        if (seq_csr_common::get_main_mem_ranges_en() ||
            memblock_sync_pkg::csr_special_sequence_active) return;
        if (builder == null || csr == null || pma_pmp_model == null || !pma_pmp_model.is_ready())
            `uvm_fatal("L2TLB_ROUTED_PPN", "routed construction has no initialized PMA owner")
        if (query_sample != 0) begin
            if (!pma_pmp_model.get_generations_before_sample(query_sample, pmp_gen, pma_gen))
                `uvm_fatal("L2TLB_ROUTED_PPN", "no effective PMA generation for request sample")
        end else pma_pmp_model.get_current_generations(pmp_gen, pma_gen);
        intent = '{default:'0};
        if (routed_ppn_intent_by_vpn.exists(key.vpn[37:0]))
            intent = routed_ppn_intent_by_vpn[key.vpn[37:0]];
        builder.configure_routed_ppn(pma_pmp_model, pma_gen, csr.priv_debug, intent);
    endfunction:configure_builder_routed_context
```
中文伪代码：strict和CSR专项直接旁路；其他情况必须具有builder、CSR及初始化模型。带sample查询该边界生效版本，缺失立即报告；没有sample的既有构造入口使用当前已提交版本。查by-VPN意图后调用builder配置，生成期间不再读latest表。get_generations_before_sample实际采用effective sample小于等于请求sample。


### 3.15 `check_routed_ppn_intent_hit()`

抽象功能描述：发现已发布映射与输入意图不一致时报告构造冲突，不改旧entry。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:8194`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit check_routed_ppn_intent_hit(input memblock_tlb_lookup_key_t key,
                                               input memblock_tlb_entry entry);
        bit [43:0] actual_ppn;
        if (!routed_ppn_intent_by_vpn.exists(key.vpn[37:0])) return 1'b1;
        if (entry == null || entry.s2_stage_active || entry.s1_level != 0 || entry.s1_pte_n)
            `uvm_fatal("L2TLB_ROUTED_INTENT", "narrow intent conflicts with existing translation shape")
        actual_ppn = entry.s1_resolved_ppn;
        if (!entry.s1_resolved_ppn_valid ||
            actual_ppn != routed_ppn_intent_by_vpn[key.vpn[37:0]].ppn)
            `uvm_fatal("L2TLB_ROUTED_INTENT", "narrow intent conflicts with existing physical translation")
        return 1'b1;
    endfunction:check_routed_ppn_intent_hit
```
中文伪代码：没有意图即成功；有意图则必须为S1普通4KiB且解析结果有效，并与意图页一致，否则报告错误。检查只在显式意图场景发生，不为普通hit增加全表扫描。


### 3.16 `get_or_create_l2tlb_entry_by_req_with_snapshot()`

抽象功能描述：在真实请求冻结上下文下取得共享翻译，只有miss创建和发布新entry。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:8209`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function bit get_or_create_l2tlb_entry_by_req_with_snapshot(
        input bit [37:0] vpn,
        input bit [1:0] s2xlate,
        input mmu_csr_runtime_state csr_snapshot,
        input bit ppn_reuse_en,
        input int unsigned ppn_reuse_wt,
        output memblock_tlb_lookup_key_t request_key,
        output memblock_tlb_lookup_key_t entry_anchor_key,
        output memblock_tlb_lookup_result_e lookup_result,
        output memblock_tlb_entry entry,
        output bit created,
        input longint unsigned pma_query_sample = 0);
        bit reused_ppn;

        if (csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "get_or_create_l2tlb_entry_by_req_with_snapshot got null csr_snapshot")
        end
        if (pma_query_sample != 0 &&
            !apply_pma_pmp_csr_writes_before_request(pma_query_sample)) return 1'b0;
        request_key = csr_snapshot.make_lookup_key({26'b0, vpn}, s2xlate);
        entry_anchor_key = '{default:'0};
        lookup_result = MEMBLOCK_TLB_LOOKUP_MISS_BUILD;
        if (has_tlb_entry(request_key)) begin
            entry = tlb_entry_by_key[request_key];
            if (entry.range_index_keys.size() == 0) begin
                `uvm_fatal("COMMON_DATA", "exact L2TLB hit has no registered range index ownership")
            end
            if (!check_routed_ppn_intent_hit(request_key, entry)) return 1'b0;
            entry.last_hit_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            entry_anchor_key = request_key;
            lookup_result = MEMBLOCK_TLB_LOOKUP_EXACT_HIT;
            created = 1'b0;
            return 1'b1;
        end
        if (find_tlb_range_hit_by_req(request_key, csr_snapshot,
                                      entry_anchor_key, entry)) begin
            if (!check_routed_ppn_intent_hit(request_key, entry)) return 1'b0;
            entry.last_hit_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            lookup_result = MEMBLOCK_TLB_LOOKUP_RANGE_HIT;
            created = 1'b0;
            return 1'b1;
        end
        entry = build_tlb_entry_for_key_with_csr(request_key, csr_snapshot, pma_query_sample);
        reused_ppn = 1'b0;
        if (ppn_reuse_en) begin
            if (routed_ppn_intent_by_vpn.exists(vpn))
                `uvm_fatal("L2TLB_ROUTED_INTENT", "narrow PPN intent cannot use completed PPN reuse")
            try_apply_l2tlb_ppn_reuse_to_new_entry(entry, ppn_reuse_wt, reused_ppn,
                                                csr_snapshot, pma_query_sample);
        end
        publish_live_tlb_entry(request_key, entry, csr_snapshot);
        entry_anchor_key = request_key;
        lookup_result = MEMBLOCK_TLB_LOOKUP_MISS_BUILD;
        created = 1'b1;
        if (reused_ppn) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("L2TLB miss reuses completed PPN vpn=0x%0h s2xlate=%0d generation=%0d",
                                request_key.vpn, request_key.s2xlate,
                                entry.entry_generation), UVM_LOW)
        end
        return 1'b1;
    endfunction:get_or_create_l2tlb_entry_by_req_with_snapshot
```
中文伪代码：先检查CSR；有sample则由既有request回放服务应用成熟写，失败返回。构造请求key，exact命中检查意图并更新访问时间后返回；range命中也保持只读物理映射。miss传sample构造新entry；打开reuse时意图组合报错，否则按同一CSR/sample资格选择历史页；最后只发布一次，记录创建和复用结果。build入口负责PMA快照，publish负责原canonical索引，不建立第二entry owner。


### 3.17 `try_apply_l2tlb_ppn_reuse_to_new_entry()`

抽象功能描述：仅在插表前从有界完成历史选取可复用PPN，保留FIFO写入和消费合同。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:8866`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function void try_apply_l2tlb_ppn_reuse_to_new_entry(
        ref memblock_tlb_entry entry,
        input int unsigned ppn_reuse_wt,
        output bit reused_ppn,
        input mmu_csr_runtime_state csr_snapshot = null,
        input longint unsigned pma_query_sample = 0);
        tlb_map_builder builder;
        int unsigned candidate_indices[$];
        int unsigned candidate_count;
        int unsigned candidate_choice;
        bit choose_reuse;

        reused_ppn = 1'b0;
        if (entry == null) begin
            `uvm_fatal("COMMON_DATA", "PPN reuse got null new entry")
        end
        if (ppn_reuse_wt > 100) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("PPN reuse weight=%0d is outside [0:100]", ppn_reuse_wt))
        end
        if (l2tlb_ppn_history_q.size() == 0 || ppn_reuse_wt == 0) begin
            return;
        end
        builder = tlb_map_builder::type_id::create("tlb_builder_ppn_reuse");
        if (builder == null) begin
            `uvm_fatal("COMMON_DATA", "failed to create PPN reuse builder")
        end
        if (csr_snapshot != null)
            configure_builder_routed_context(builder, entry.lookup_key, csr_snapshot, pma_query_sample);
        if (!builder.can_apply_reused_final_ppn(entry, '0)) begin
            return;
        end
        foreach (l2tlb_ppn_history_q[idx]) begin
            if (l2tlb_ppn_history_q[idx].ppn_valid &&
                builder.can_apply_reused_final_ppn(entry,
                                                   l2tlb_ppn_history_q[idx].ppn) &&
                builder.check_reused_routed_ppn(entry,
                                                l2tlb_ppn_history_q[idx].ppn)) begin
                candidate_indices.push_back(idx);
            end
        end
        candidate_count = candidate_indices.size();
        if (candidate_count == 0) begin
            return;
        end
        choose_reuse = 1'b1;
        if (ppn_reuse_wt < 100) begin
            if (!std::randomize(choose_reuse) with {
                    choose_reuse dist {1'b1 := ppn_reuse_wt,
                                       1'b0 := 100 - ppn_reuse_wt};
                }) begin
                `uvm_fatal("COMMON_DATA", "failed to randomize L2TLB PPN reuse decision")
            end
        end
        if (!choose_reuse) begin
            return;
        end
        candidate_choice = 0;
        if (candidate_count > 1 && !std::randomize(candidate_choice) with {
                candidate_choice < candidate_count;
            }) begin
            `uvm_fatal("COMMON_DATA", "failed to randomize L2TLB PPN reuse candidate")
        end
        builder.apply_reused_final_ppn(
            l2tlb_ppn_history_q[candidate_indices[candidate_choice]].ppn, entry);
        reused_ppn = 1'b1;
    endfunction:try_apply_l2tlb_ppn_reuse_to_new_entry
```
中文伪代码：先清复用结果并检查权重/entry/历史资格；创建builder并在有CSR时配置相同固定版本。先确认目标形状可复用，再只扫描有界history，把既有shape/leaf/接口条件和新路由条件都通过的记录加入候选池。没有候选直接返回，按权重决定是否使用并随机选一个，最后调用apply_reused_final_ppn更新新entry，置结果真。该操作不删除完成历史、不改已发布hit或冻结token。


### 3.18 `finalize_tlb_payload_shape_and_payload()`

抽象功能描述：在既有occupancy允许形状中生成完整响应字段并完成最终页合法化，不发布live entry。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv:998`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function memblock_tlb_entry finalize_tlb_payload_shape_and_payload(
        input memblock_tlb_entry prepared_entry,
        input memblock_tlb_lookup_key_t key,
        input mmu_csr_runtime_state csr_state,
        input bit [4:0] allowed_shape_mask,
        output bit pte_n_overridden,
        output bit forced_4k,
        input bit directed_shape_override_valid,
        input memblock_tlb_range_coverage_rank_e directed_shape_override);
        memblock_tlb_shape_bucket_t buckets[$];
        memblock_tlb_shape_bucket_t selected_bucket;
        longint unsigned total_positive_weight;
        longint unsigned weighted_pick;
        longint unsigned cumulative_weight;
        bit clear_s1_n;
        bit clear_s2_n;
        bit selected;
        bit [43:0] s1_canonical_ppn;
        bit [43:0] s2_canonical_ppn;
        bit [37:0] s2_input_vpn;
        bit        s1_resolvable;
        bit        s2_resolvable;
        bit        s2_request_in_range;
        bit [1:0]  directed_level;
        bit        directed_napot;
        bit        address_fallback;

        if (prepared_entry == null || csr_state == null ||
            prepared_entry.lookup_key != key ||
            prepared_entry.s2xlate != key.s2xlate ||
            prepared_entry.payload_phase !=
                memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_PREPARED) begin
            `uvm_fatal("TLB_BUILDER", "finalize got inconsistent prepared payload")
        end
        pte_n_overridden = 1'b0;
        forced_4k = 1'b0;
        clear_s1_n = 1'b0;
        clear_s2_n = 1'b0;
        if (directed_shape_override_valid) begin
            if (!allowed_shape_mask[directed_shape_override]) begin
                `uvm_fatal("TLB_BUILDER",
                           "directed shape override is absent from allowed mask")
            end
            directed_napot = 1'b0;
            case (directed_shape_override)
                MEMBLOCK_TLB_COVERAGE_4K: directed_level = 2'd0;
                MEMBLOCK_TLB_COVERAGE_64K: begin
                    directed_level = 2'd0;
                    directed_napot = 1'b1;
                end
                MEMBLOCK_TLB_COVERAGE_2M: directed_level = 2'd1;
                MEMBLOCK_TLB_COVERAGE_1G: directed_level = 2'd2;
                MEMBLOCK_TLB_COVERAGE_512G: directed_level = 2'd3;
                default: `uvm_fatal("TLB_BUILDER", "invalid directed shape override")
            endcase
            if (directed_shape_override == MEMBLOCK_TLB_COVERAGE_512G &&
                ((prepared_entry.s1_stage_active &&
                  prepared_entry.s1_translation_mode_at_build != MEMBLOCK_SV48_MODE) ||
                 (prepared_entry.s2_stage_active &&
                  prepared_entry.s2_translation_mode_at_build != MEMBLOCK_SV48_MODE))) begin
                `uvm_fatal("TLB_BUILDER",
                           "directed 512GB shape requires Sv48 on every active stage")
            end
            selected_bucket = '{default:'0};
            selected_bucket.shape = directed_shape_override;
            selected_bucket.s1_level = prepared_entry.s1_stage_active ?
                                       directed_level : 2'd0;
            selected_bucket.s2_level = prepared_entry.s2_stage_active ?
                                       directed_level : 2'd0;
            selected_bucket.s1_pte_n = prepared_entry.s1_stage_active ?
                                       directed_napot : 1'b0;
            selected_bucket.s2_pte_n = prepared_entry.s2_stage_active ?
                                       directed_napot : 1'b0;
            total_positive_weight = 1;
        end
        else begin
            build_shape_candidate_buckets(prepared_entry, allowed_shape_mask,
                                          clear_s1_n, clear_s2_n,
                                          buckets, total_positive_weight);
        end
        if (!directed_shape_override_valid && total_positive_weight == 0 &&
            prepared_entry.s2xlate == 2'd3) begin
            if (prepared_entry.s2_pte_n) begin
                clear_s2_n = 1'b1;
                build_shape_candidate_buckets(prepared_entry, allowed_shape_mask,
                                              clear_s1_n, clear_s2_n,
                                              buckets, total_positive_weight);
            end
            if (total_positive_weight == 0 && prepared_entry.s1_pte_n) begin
                clear_s1_n = 1'b1;
                clear_s2_n = 1'b0;
                build_shape_candidate_buckets(prepared_entry, allowed_shape_mask,
                                              clear_s1_n, clear_s2_n,
                                              buckets, total_positive_weight);
            end
            if (total_positive_weight == 0 &&
                prepared_entry.s1_pte_n && prepared_entry.s2_pte_n) begin
                clear_s1_n = 1'b1;
                clear_s2_n = 1'b1;
                build_shape_candidate_buckets(prepared_entry, allowed_shape_mask,
                                              clear_s1_n, clear_s2_n,
                                              buckets, total_positive_weight);
            end
        end
        else if (!directed_shape_override_valid && total_positive_weight == 0) begin
            clear_s1_n = prepared_entry.s1_stage_active && prepared_entry.s1_pte_n;
            clear_s2_n = prepared_entry.s2_stage_active && prepared_entry.s2_pte_n;
            if (clear_s1_n || clear_s2_n) begin
                build_shape_candidate_buckets(prepared_entry, allowed_shape_mask,
                                              clear_s1_n, clear_s2_n,
                                              buckets, total_positive_weight);
            end
        end
        pte_n_overridden = total_positive_weight != 0 &&
                           (clear_s1_n || clear_s2_n);

        if (directed_shape_override_valid) begin
            selected = 1'b1;
        end
        else if (total_positive_weight == 0) begin
            if (!allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K]) begin
                `uvm_fatal("TLB_BUILDER", "ordinary 4KB fallback is unexpectedly blocked")
            end
            selected_bucket = '{default:'0};
            selected_bucket.shape = MEMBLOCK_TLB_COVERAGE_4K;
            selected_bucket.s1_level = 2'd0;
            selected_bucket.s2_level = 2'd0;
            selected_bucket.s1_pte_n = 1'b0;
            selected_bucket.s2_pte_n = 1'b0;
            forced_4k = 1'b1;
        end
        else begin
            weighted_pick = 0;
            if (total_positive_weight > 1 && !std::randomize(weighted_pick) with {
                    weighted_pick < total_positive_weight;
                }) begin
                `uvm_fatal("L2TLB_PAYLOAD_RANDOM", "failed to select legal TLB shape")
            end
            cumulative_weight = 0;
            selected = 1'b0;
            foreach (buckets[idx]) begin
                cumulative_weight += buckets[idx].weight;
                if (!selected && weighted_pick < cumulative_weight) begin
                    selected_bucket = buckets[idx];
                    selected = 1'b1;
                end
            end
            if (!selected) begin
                `uvm_fatal("L2TLB_PAYLOAD_RANDOM", "weighted TLB shape selection lost candidate")
            end
        end

        prepared_entry.s1_level = selected_bucket.s1_level;
        prepared_entry.s2_level = selected_bucket.s2_level;
        prepared_entry.s1_pte_n = prepared_entry.s1_stage_active ?
                                  selected_bucket.s1_pte_n : 1'b0;
        prepared_entry.s2_pte_n = prepared_entry.s2_stage_active ?
                                  selected_bucket.s2_pte_n : 1'b0;
        s2_request_in_range = 1'b1;

        if (prepared_entry.s1_stage_active) begin
            prepared_entry.s1_addr_low = key.vpn[2:0];
            prepared_entry.s1_tag = key.vpn[37:3];
            s1_canonical_ppn = make_canonical_ppn(key.vpn[37:0],
                                                   prepared_entry.s1_root_ppn_at_build,
                                                   prepared_entry.s1_level, 1'b0,
                                                   !routed_policy_active(prepared_entry));
            if (prepared_entry.s2_stage_active && routed_policy_active(prepared_entry)) begin
                // 中文注释：GPA只受S2输入位宽约束，不按最终PA路由窗口收窄。
                if (prepared_entry.s2_translation_mode_at_build == MEMBLOCK_SV39_MODE)
                    s1_canonical_ppn[43:29] = '0;
                else s1_canonical_ppn[43:38] = '0;
            end
            apply_legal_napot_ppn_encoding(1'b1, prepared_entry.s1_level,
                                            prepared_entry.s1_pte_n,
                                            s1_canonical_ppn, prepared_entry);
            if (!prepared_entry.s2_stage_active) begin
                if (!select_final_routed_ppn(prepared_entry, 1'b1, key.vpn[37:0],
                        directed_shape_override_valid, s1_canonical_ppn, address_fallback))
                    `uvm_fatal("L2TLB_ROUTED_PPN", "no legal physical mapping for selected S1 shape/window")
                forced_4k |= address_fallback;
            end
            build_s1_sector_payload(s1_canonical_ppn, prepared_entry);
            if (!prepared_entry.has_effective_fault()) begin
                reject_unsupported_valid_s1_nonleaf(prepared_entry);
                s1_resolvable = prepared_entry.s1_pte_v &&
                                (prepared_entry.s1_pte_r ||
                                 prepared_entry.s1_pte_w ||
                                 prepared_entry.s1_pte_x) &&
                                s1_request_fits_mode(key.vpn[37:0],
                                    prepared_entry.s1_translation_mode_at_build) &&
                                napot_raw_ppn_is_model_resolvable(1'b1,
                                    prepared_entry.s1_pte_n,
                                    s1_canonical_ppn, prepared_entry);
                prepared_entry.s1_resolved_ppn_valid = s1_resolvable;
                if (s1_resolvable) begin
                    prepared_entry.s1_resolved_ppn = resolve_ppn_from_raw(
                        s1_canonical_ppn, key.vpn[37:0],
                        prepared_entry.s1_level, prepared_entry.s1_pte_n);
                end
            end
        end

        if (prepared_entry.s2_stage_active) begin
            if (key.s2xlate == 2'd2) begin
                s2_request_in_range = s2_request_fits_mode(
                    key.vpn, prepared_entry.s2_translation_mode_at_build);
            end
            if (key.s2xlate != 2'd2 &&
                !prepared_entry.has_effective_fault()) begin
                // This is the protocol raw anchor.  It deliberately uses raw
                // S1 fields even when a non-LEGAL NAPOT cannot form a model
                // resolved PPN.
                s2_input_vpn = derive_allstage_raw_s2_tag(
                    prepared_entry, key.vpn[37:0],
                    prepared_entry.s2_translation_mode_at_build);
            end
            else begin
                // A fault response does not claim a normal allStage GVPN.
                // s2.tag is nevertheless an encodable payload field.
                s2_input_vpn = key.vpn[37:0];
            end
            prepared_entry.s2_tag = s2_input_vpn;
            s2_canonical_ppn = make_canonical_ppn(s2_input_vpn,
                                                   prepared_entry.s2_root_ppn_at_build,
                                                   prepared_entry.s2_level, 1'b1,
                                                   !routed_policy_active(prepared_entry));
            apply_legal_napot_ppn_encoding(1'b0, prepared_entry.s2_level,
                                            prepared_entry.s2_pte_n,
                                            s2_canonical_ppn, prepared_entry);
            if (!select_final_routed_ppn(prepared_entry, 1'b0, s2_input_vpn,
                    directed_shape_override_valid, s2_canonical_ppn, address_fallback))
                `uvm_fatal("L2TLB_ROUTED_PPN", "no legal physical mapping for selected S2 shape/window")
            forced_4k |= address_fallback;
            prepared_entry.s2_entry_ppn_raw = encode_s2_entry_ppn(
                s2_canonical_ppn);
            if (!prepared_entry.has_effective_fault()) begin
                s2_resolvable = (key.s2xlate != 2'd2 || s2_request_in_range) &&
                                napot_raw_ppn_is_model_resolvable(1'b0,
                                    prepared_entry.s2_pte_n,
                                    s2_canonical_ppn, prepared_entry);
                prepared_entry.s2_resolved_ppn_valid = s2_resolvable;
                if (s2_resolvable) begin
                    prepared_entry.s2_resolved_ppn = resolve_ppn_from_raw(
                        s2_canonical_ppn, s2_input_vpn,
                        prepared_entry.s2_level, prepared_entry.s2_pte_n);
                end
            end
        end

        prepared_entry.create_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        prepared_entry.last_hit_cycle = prepared_entry.create_cycle;
        prepared_entry.payload_phase =
            memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED;
        prepared_entry.check_inactive_stage_defaults("BUILD");
        prepared_entry.validate_s1_sector_payload_consistency("BUILD");
        return prepared_entry;
    endfunction:finalize_tlb_payload_shape_and_payload
```
中文伪代码：校验prepared与CSR/key一致，清统计；显式shape必须在allowed mask且符合mode。普通构造由固定stage权重建立bucket，必要时按既有规则清N后再枚举；无正候选沿用普通4KiB合同，否则一次加权选shape。固定level/N后生成S1候选，allStage只收窄中间GPA到S2输入位宽并完成NAPOT；S1-only调用最终地址策略，随后重建sector和resolved字段。生成S2时先从S1 raw解析tag（故不能重写S1链），计算S2候选并调用最终策略，成功后才进行38位响应编码，最后按实际level/N构造resolved字段。设置创建时间、payload阶段并检查inactive和sector一致性。prepare已冻结PBMT，本函数不二次随机；fallback只改最终stage，range索引按最终字段推导。


### 3.19 `build_random_main_table()`

抽象功能描述：在真实issue前构造带合法设备偏移的独立VPN主表，发布顺序由本序列负责。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_routed_ppn_sequence.sv:32`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    virtual task build_random_main_table(input int unsigned main_trans_num_i);
        memblock_rob_key_t rob_key;
        int unsigned order[$];
        bit [63:0] va_base;
        bit [63:0] va_limit;

        if (data == null) data = common_data_transaction::get();
        data.reset_all_tables(main_trans_num_i);
        rob_key = choose_rob_start_key();
        va_base = (seq_csr_common::get_main_vaddr_base() + 4095) & ~64'hfff;
        va_limit = seq_csr_common::get_main_vaddr_base() + seq_csr_common::get_main_vaddr_range();
        if (main_trans_num_i < 6 || va_limit < va_base ||
            va_base + (64'(main_trans_num_i) << 12) > va_limit ||
            va_limit > 64'h0000_0040_0000_0000) begin
            `uvm_fatal(get_type_name(), "routed PPN narrow smoke needs >=6 unique Sv39 positive VPNs inside MAIN_VADDR")
        end
        for (int unsigned idx = 0; idx < main_trans_num_i; idx++) begin
            memblock_uid_t uid;
            main_control_transaction tr;
            memblock_routed_ppn_intent_t intent;
            bit [63:0] first, limit, pa, page_first, page_limit, offset_first, offset_limit;
            int signed region_id;
            string label;
            int unsigned target;

            if (order.size() == 0) begin
                for (int unsigned n = 0; n < 6; n++) order.push_back(n);
                order.shuffle();
            end
            target = order.pop_front();
            narrow_region(target, first, limit, label, region_id);
            pa = first + (64'($urandom_range(int'((limit-first)/8)-1, 0)) << 3);
            page_first = pa & ~64'hfff;
            page_limit = page_first + 4096;
            offset_first = ((first > page_first) ? first : page_first) - page_first;
            offset_limit = ((limit < page_limit) ? limit : page_limit) - page_first;
            uid = data.alloc_uid();
            tr = main_control_transaction::type_id::create($sformatf("routed_narrow_uid_%0d", uid));
            tr.uid = uid;
            tr.op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_LOAD;
            tr.fuType = MEMBLOCK_FUTYPE_LDU;
            tr.fuOpType = MEMBLOCK_LSUOP_LD;
            tr.numLsElem = memblock_num_ls_elem_t'(1);
            tr.robIdx_flag = rob_key.flag;
            tr.robIdx_value = rob_key.value;
            tr.src_0 = va_base + (64'(idx) << 12) + pa[11:0];
            tr.imm = 0;
            tr.boundary_profile = MEMBLOCK_BOUNDARY_PROFILE_ALIGNED;
            tr.boundary_size_bytes = 8;
            tr.send_pri = 50;
            tr.send_pri_std = 50;
            tr.update_vaddr();
            validate_main_table_entry(tr, $sformatf("narrow uid=%0d", uid));
            intent = '{default:'0};
            intent.valid = 1;
            intent.ppn = 44'(pa >> 12);
            intent.first_offset = 12'(offset_first);
            intent.last_offset = 12'(offset_limit - 1);
            intent.region_id = region_id;
            if (!data.register_routed_ppn_intent(tr.vaddr[49:12], intent)) begin
                `uvm_fatal(get_type_name(), $sformatf("narrow intent registration failed uid=%0d", uid))
            end
            data.set_main_transaction(uid, tr);
            `uvm_info("ROUTED_PPN_NARROW_INPUT", $sformatf("uid=%0d window=%s va=0x%0h pa=0x%0h allowed_offsets=0x%0h..0x%0h", uid, label, tr.vaddr, pa, intent.first_offset, intent.last_offset), UVM_LOW)
            rob_key = rob_order_util::rob_advance(rob_key, 1);
        end
        init_status_for_main_table();
        data.check_main_table_complete();
    endtask
```
中文伪代码：确保公共数据并reset主表，选择ROB起点，验证MAIN_VADDR可容纳至少六个独立Sv39正地址页。按随机排列循环挑六类窗口，窗口内均匀选择8字节对齐PA；计算该页与窗口交集的偏移范围。分配UID、构造普通LD及ROB字段，VA使用独立页并复制PA低12位，更新和验证地址。登记物理页及允许偏移意图，成功才写主表；全部完成后初始化status并一次发布ready。narrow_region只提供六窗口边界；register只存输入约束；原RM仍检查真实访问。


### 3.20 `body()`

抽象功能描述：限制窄专项配置并复用父场景完整生命周期，同时被动记录外部请求到达。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_routed_ppn_vseq.sv:16`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    virtual task body();
        bit finished;
        seq_csr_common::init();
        if (seq_csr_common::get_main_mem_ranges_en() ||
            seq_csr_common::get_l2tlb_level_weight_en() ||
            seq_csr_common::get_l2tlb_ppn_reuse_en() ||
            seq_csr_common::get_addr_reuse_en_1_wt() != 0 ||
            seq_csr_common::get_boundary_profile_gen_en() ||
            seq_csr_common::get_use_manual_main_table() ||
            seq_csr_common::get_csr_control_enable() ||
            seq_csr_common::get_sfence_control_enable() ||
            !uses_static_mmu_sv39_csr() ||
            seq_csr_common::get_l2tlb_pte_1_wt(1, 7) != 0 ||
            seq_csr_common::get_l2tlb_fault_1_wt(1, 0) != 0 ||
            seq_csr_common::get_l2tlb_fault_1_wt(1, 1) != 0 ||
            seq_csr_common::get_l2tlb_pbmt_wt(1, 1) != 0 ||
            seq_csr_common::get_l2tlb_pbmt_wt(1, 2) != 0) begin
            `uvm_fatal(get_type_name(), "narrow routed PPN scenario requires sparse/static Sv39 S1 4K N0 PBMT00, no faults/control/address or PPN reuse")
        end
        if (!uvm_config_db#(virtual sbuffer_agent_agent_interface)::get(
                null, "uvm_test_top.env.u_sbuffer_agent_agent*", "vif", narrow_vif)) begin
            `uvm_fatal(get_type_name(), "cannot observe narrow-window external A.fire interface")
        end
        finished = 0;
        fork
            begin
                super.body();
                finished = 1;
            end
            begin
                observe_external_a_fires(finished);
            end
        join
        foreach (actual_a_fire_count[i]) begin
            bit [63:0] first, limit;
            string label;
            int signed region_id;
            memblock_main_dispatch_routed_ppn_sequence::narrow_region(i, first, limit, label, region_id);
            `uvm_info("ROUTED_PPN_NARROW_REACHED", $sformatf("window=%s actual_A_fire_count=%0d", label, actual_a_fire_count[i]), UVM_LOW)
        end
    endtask
```
中文伪代码：先初始化配置，拒绝strict、大页随机、reuse、手工表、动态控制、非静态Sv39及不兼容fault/PBMT/N权重；取得外部接口，失败报错。并行运行父body和被动观测，父流程完成后置finished；两者自然结束后逐窗口打印真实A.fire数量。父body拥有responder启动、主流程drain及end_test_check；观测函数不提供RM结果。


### 3.21 `start_core_dispatch_flow()`

抽象功能描述：保持父场景各agent及静态CSR启动链，只把main类型换为窄窗口生成器。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_routed_ppn_vseq.sv:91`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    virtual task start_core_dispatch_flow();
        memblock_lsqenq_dispatch_base_sequence lsqenq_seq;
        memblock_issue_dispatch_base_sequence issue_seq;
        memblock_lsqcommit_dispatch_base_sequence lsqcommit_seq;
        memblock_l2tlb_base_sequence l2tlb_seq;
        memblock_main_dispatch_routed_ppn_sequence main_seq;
        memblock_mmu_sv39_csr_sequence mmu_sv39_csr_seq;
        fork
            begin
                `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
            end
            begin
                wait_for_explicit_l2tlb_start_barrier();
                `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
            end
            begin
                `uvm_do_on(lsqcommit_seq, p_sequencer.lsqcommit_sqr)
            end
            begin
                wait_for_explicit_l2tlb_start_barrier();
                `uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr)
            end
            begin
                `uvm_do_on(main_seq, p_sequencer)
            end
            begin
                `uvm_do_on(mmu_sv39_csr_seq, p_sequencer.csr_ctrl_sqr)
            end
        join
    endtask
```
中文伪代码：并行启动lsqenq、issue、commit、L2 responder、专用main和静态Sv39 CSR；issue和L2都先等待既有CSR屏障，再用标准宏启动到真实sequencer；main在virtual sequencer上启动，所有子序列自然join。无全局factory覆盖、无新pending owner或ready来源。


### 3.22 `observe_external_a_fires()`

抽象功能描述：被动保存实际外部请求到达证据，确保专项不会在重置后继续旧主表意图。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_routed_ppn_vseq.sv:60`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    virtual task observe_external_a_fires(ref bit finished);
        bit started;
        bit [63:0] pa;
        started = 0;
        while (!finished) begin
            @(narrow_vif.mon_cb);
            if (common_data_transaction::get().main_table_ready && narrow_vif.rst_n === 1'b1 &&
                memblock_sync_pkg::reset_backend_done === 1'b1) started = 1;
            if (started && (narrow_vif.rst_n !== 1'b1 ||
                            memblock_sync_pkg::reset_backend_done !== 1'b1)) begin
                `uvm_fatal(get_type_name(), "runtime reset aborts narrow PPN scenario; rerun with full initialization")
            end
            if (narrow_vif.rst_n !== 1'b1 ||
                narrow_vif.mon_cb.auto_inner_buffers_out_a_valid !== 1'b1 ||
                narrow_vif.mon_cb.auto_inner_buffers_out_a_ready !== 1'b1) continue;
            pa = {16'b0, narrow_vif.mon_cb.auto_inner_buffers_out_a_bits_address};
            for (int unsigned i = 0; i < 6; i++) begin
                bit [63:0] first, limit;
                string label;
                int signed region_id;
                memblock_main_dispatch_routed_ppn_sequence::narrow_region(i, first, limit, label, region_id);
                if (pa >= first && pa < limit) begin
                    actual_a_fire_count[i]++;
                    `uvm_info("ROUTED_PPN_NARROW_A_FIRE", $sformatf("window=%s pa=0x%0h", label, pa), UVM_LOW)
                end
            end
        end
    endtask
```
中文伪代码：每个monitor边界判断主表及reset条件，首次就绪后置started；之后出现reset立即报告专项终止。非真实A握手跳过；握手时零扩展PA，按六个窗口判断命中，增加到达计数并打印。finished由父流程自然结束后设置，本函数下一采样结束，不强杀驱动，不代替数据或异常checker。


### 3.23 `vseq_starts_l2tlb()`

抽象功能描述：让testcase为声明会启动L2 responder的场景准备唯一正确拓扑。

源码位置：`mem_ut/ver/ut/memblock/tc/src/basicTest.sv:21`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
      function bit vseq_starts_l2tlb(input string vseq_name);
          return vseq_name == "memblock_l2tlb_routed_ppn_vseq" ||
                 vseq_name == "memblock_dispatch_real_smoke_vseq" ||
                 vseq_name == "memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq" ||
                 vseq_name == "memblock_dispatch_manual_control_vseq" ||
                 vseq_name == "memblock_csr_random_config_vseq" ||
                 vseq_name == "memblock_csr_scalar_load_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_store_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_mixed_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_store_sta_std_10k_vseq" ||
                 vseq_name == "memblock_dispatch_real_cancel_reconcile_vseq" ||
                 vseq_name == "memblock_l2tlb_pbmt_response_fault_vseq";
      endfunction:vseq_starts_l2tlb
```
中文伪代码：对既有白名单增加真实窄vseq名称，其他名称判断不变；软件vseq没有加入，因此不会因默认开关误启动真实responder。调用者据结果配置dispatch-active和explicit启动，派生继承关系不会替代名称登记。


### 3.24 `vseq_owns_static_mmu_csr()`

抽象功能描述：声明由场景独占静态CSR producer，避免旧agent默认序列并发污染配置。

源码位置：`mem_ut/ver/ut/memblock/tc/src/basicTest.sv:50`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
      function bit vseq_owns_static_mmu_csr(input string vseq_name);
          return (vseq_name == "memblock_l2tlb_routed_ppn_vseq" ||
                  vseq_name == "memblock_dispatch_real_smoke_vseq" ||
                  vseq_name == "memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq" ||
                  vseq_name == "memblock_l2tlb_pbmt_response_fault_vseq") &&
                 !memblock_sync_pkg::uses_control_barrier_topology();
      endfunction:vseq_owns_static_mmu_csr
```
中文伪代码：加入真实窄vseq名称，并仍要求未启用control-barrier拓扑；其他场景保持旧条件。调用者将旧CSR/Fence producer替换为空闲基类；实际静态Sv39配置仍由窄vseq唯一启动。


### 3.25 `reset_fixture()`

抽象功能描述：为纯软件检查重新建立合法非零公共容量，再关闭实际主表事务数。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_routed_ppn_sequence.sv:32`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function void reset_fixture();
        data.reset_all_tables(1);
        data.main_trans_num = 0;
    endfunction
```
中文伪代码：先调用reset_all_tables(1)，满足公共API非零容量和reset域前提，再把main_trans_num设为0；不发布ready、不发射事务。公共reset同时清意图和翻译历史，测试不得直接用0调用被禁止的reset。


### 3.26 `check_mature_csr_snapshot()`

抽象功能描述：通过公共回放服务证明C+1旧表、C+2新表及旧版本不可变，不宣称真实DUT配置覆盖。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_routed_ppn_sequence.sv:138`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function void check_mature_csr_snapshot();
        memblock_sync_pkg::dispatch_raw_pma_pmp_csr_write_t event_item;
        longint unsigned saved_sample, old_generation, pmp_generation;
        bit cacheable;
        reset_fixture();
        data.pma_pmp_model.apply_csr_write(12'h7c8,64'h0000_0000_2400_0000,10);
        configure_current();
        old_generation = generation;
        saved_sample = memblock_sync_pkg::lifecycle_event_published_seq;
        event_item = memblock_sync_pkg::make_empty_raw_pma_pmp_csr_write();
        event_item.valid = 1;
        event_item.sample_seq = 20;
        event_item.sample_time = 1;
        event_item.reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        event_item.addr = 12'h7c0;
        event_item.data = 64'h0000_0000_0000_006f;
        memblock_sync_pkg::push_raw_pma_pmp_csr_write(event_item);
        memblock_sync_pkg::lifecycle_event_published_seq = 21;
        data.service_csr_pmp_pma_write_observations();
        configure_current();
        check(generation == old_generation && policy.query_c('h31100000,cacheable) && !cacheable,
              "C+1 query must see old c0 and unconsumed writes");
        check(memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 1,"C+1 must retain immature FIFO prefix");
        memblock_sync_pkg::lifecycle_event_published_seq = 22;
        data.service_csr_pmp_pma_write_observations();
        configure_current();
        check(generation > old_generation && policy.query_c('h31100000,cacheable) && cacheable,
              "C+2 query must see new c1");
        check(memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 0,"C+2 consumes mature prefix once");
        check(policy.configure(data.pma_pmp_model,old_generation,0) &&
              policy.query_c('h31100000,cacheable) && !cacheable,"old generation remains immutable");
        check(policy.configure(data.pma_pmp_model,generation,0), "configure debug-disabled PMA view");
        expect_cacheability('h3801ffff, 1, "debug boundary predecessor retains c1");
        expect_cacheability('h38020000, 0, "debug boundary first byte is protected c0");
        expect_cacheability('h38020fff, 0, "debug boundary last byte is protected c0");
        expect_cacheability('h38021000, 1, "debug boundary successor restores c1");
        check(policy.configure(data.pma_pmp_model,generation,1), "configure debug-enabled PMA view");
        expect_cacheability('h38020000, 1, "debug context restores PMA table match");
        memblock_sync_pkg::lifecycle_event_published_seq = saved_sample;
        reset_fixture();
    endfunction
```
中文伪代码：重置夹具后先用独立sample写基础边界，冻结旧代数；保存发布sample，构造raw C=20配置写并入队。发布21调用公共服务，验证代数不变及队列保留；发布22再服务，验证新c1和队列消费一次。随后直接配置旧版本验证仍c0，配置新版本检查debug保护区两侧与内部精确边界，debug真时恢复表匹配。恢复原发布sample并重置夹具。基础地址写与配置写不复用同一个sample，符合模型单调记录合同。


### 3.27 `check_bounded_fallback()`

抽象功能描述：构造单一4KiB c0岛，检验普通一次降级保持c0，以及allStage不回改S1。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_routed_ppn_sequence.sv:236`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    function void check_bounded_fallback();
        mmu_csr_runtime_state csr;
        tlb_map_builder builder;
        memblock_tlb_entry entry;
        memblock_tlb_lookup_key_t key;
        bit [43:0] candidate, s1_before;
        bit [34:0] tag_before;
        bit fell_back, cacheable;
        reset_fixture();
        data.pma_pmp_model.apply_csr_write(12'h7c0,0,2);
        data.pma_pmp_model.apply_csr_write(12'h7c8,64'h0c44_01ff,3);
        data.pma_pmp_model.apply_csr_write(12'h7c9,64'h1fff_ffff_ffff,4);
        data.pma_pmp_model.apply_csr_write(12'h7c0,64'h7f3f,5);
        configure_current();
        expect_cacheability('h31100000, 0, "fallback island first byte is c0");
        expect_cacheability('h31100fff, 0, "fallback island last byte is c0");
        expect_cacheability('h310fffff, 1, "fallback island predecessor is c1");
        expect_cacheability('h31101000, 1, "fallback island successor is c1");
        csr = mmu_csr_runtime_state::type_id::create("fallback_csr");
        csr.reset(); csr.satp_mode = 8; csr.satp_ppn = 44'h80000;
        csr.vsatp_mode = 8; csr.vsatp_ppn = 44'h80000;
        csr.hgatp_mode = 8; csr.hgatp_ppn = 44'h80000;
        csr.m_pbmt_en = 1; csr.h_pbmt_en = 1;
        for (int unsigned all_stage = 0; all_stage < 2; all_stage++) begin
            csr.priv_virt = all_stage[0];
            key = csr.make_lookup_key(64'h100,all_stage ? 2'd3 : 2'd0);
            builder = tlb_map_builder::type_id::create("fallback_builder");
            builder.configure_routed_ppn(data.pma_pmp_model,generation,0,'{default:'0});
            entry = builder.prepare_tlb_payload_context(key,csr);
            entry.s1_entry_pbmt = 0;
            if (all_stage) begin
                entry.s2_entry_pbmt = 0; entry.s2_level = 1; entry.s2_pte_n = 0;
                entry.s1_entry_ppn_raw = 44'h12345; entry.s1_tag = 35'h345;
            end else begin entry.s1_level = 1; entry.s1_pte_n = 0; end
            s1_before = entry.s1_entry_ppn_raw;
            tag_before = entry.s1_tag;
            candidate = 44'h31000;
            check(!builder.select_final_routed_ppn(entry,!all_stage,38'h100,1,candidate,fell_back) &&
                  !fell_back,"explicit impossible shape must fail without downgrade");
            candidate = 44'h31000;
            check(builder.select_final_routed_ppn(entry,!all_stage,38'h100,0,candidate,fell_back) &&
                  fell_back && candidate == 44'h31100 &&
                  policy.check_mapping(candidate,12,0,0),"ordinary fallback remains on original c0 island");
            if (all_stage) check(entry.s2_level == 0 && entry.s1_entry_ppn_raw == s1_before &&
                entry.s1_tag == tag_before,"allStage fallback must preserve S1 mapping/tag");
            else check(entry.s1_level == 0,"S1-only fallback selects 4K");
        end
        reset_fixture();
    endfunction
```
中文伪代码：依次在不同sample关闭、写地址和启用测试PMA，建立c0岛并验证两侧；对S1-only和allStage准备entry，冻结S1字段，给大页候选。先显式shape调用应返回失败且不降级，再普通调用应只降一次得到原c0岛；检查最终stage为4KiB，allStage的S1 raw和tag保持原值。最后reset；此测试只调用构造API，没有伪造DUT完成。


### 3.28 `body()`

抽象功能描述：顺序执行纯软件策略专项，最终只报告软件检查完成。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_routed_ppn_sequence.sv:319`。功能简析：本函数直接承担本轮构造或验证职责，以下完整函数保留相关分支和状态副作用。

```systemverilog
    virtual task body();
        ensure_dispatch_runtime_helpers();
        check(!seq_csr_common::get_main_mem_ranges_en() &&
              !seq_csr_common::get_l2tlb_ppn_reuse_en(),"routed software cfg must use sparse/no-reuse");
        reset_fixture();
        check_route_boundaries();
        check_policy_selection();
        check_pma_scalar_equivalence();
        check_mature_csr_snapshot();
        check_builder_stages_and_shapes();
        check_bounded_fallback();
        check_intent_lifecycle();
        `uvm_info("ROUTED_PPN_SOFT", "routed PPN software checks completed", UVM_LOW)
    endtask
```
中文伪代码：准备公共helper，要求sparse无reuse；重置夹具后依次检查路由边界、选择策略、span/scalar等价、成熟CSR版本、三stage五shape三PBMT矩阵、一次fallback及意图生命周期。每个check失败即fatal，全部返回才报告完成。主体不启动真实agent、不以软件PASS代替外部A.fire覆盖。


### 3.29 prepare阶段PBMT冻结的具体修改


抽象功能描述：准备阶段为active stage冻结一次PBMT，供后续地址策略及响应共同读取。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv`。以下片段证明对应差异的具体落点。

```systemverilog
        // 中文注释：PBMT只在prepare随机一次，最终PA选择读取同一冻结值。
        if (entry.s1_stage_active) begin
            entry.s1_entry_pbmt = choose_pbmt(1'b1, entry.s2xlate, csr_state);
            check_pbmt_build_csr_compatibility(1'b1, entry.s2xlate, csr_state, entry.s1_entry_pbmt);
        end
        if (entry.s2_stage_active) begin
            entry.s2_entry_pbmt = choose_pbmt(1'b0, entry.s2xlate, csr_state);
            check_pbmt_build_csr_compatibility(1'b0, entry.s2xlate, csr_state, entry.s2_entry_pbmt);
        end
        entry.check_inactive_stage_defaults("PREPARE");
```
中文伪代码：S1 active时只随机一次S1 PBMT并检查CSR gate；S2同理。choose_pbmt读取冻结请求CSR，禁用阶段强制00；兼容性检查防止保留编码或disabled非零；随后inactive默认检查继续执行。finalize不再二次随机，因此选择地址与最终response属性同源。

## 4. 其余辅助函数、字段和逐文件覆盖

### 4.1 辅助函数职责与状态边界

| 对象 | 输入、输出和内部逻辑 | 副作用与调用影响 |
|---|---|---|
| `get_external_region/get_region` | index或ID映射为profile半开区间，非法ID返回false | 固定常量查询，无状态 |
| `query_c/valid_shape` | 单字节属性查询；只接受12/16/21/30/39位尺寸 | 不消费CSR，不修改模型 |
| `routed_shape_shift` | stage的level/N映射为完整字节跨度 | 与当前五种形状一致 |
| prepare中的PBMT块 | 对active stage一次choose及CSR gate校验 | 字段在shape/PPN之前冻结；inactive默认检查仍保留 |
| `build_tlb_entry_for_key_with_csr` | 接收sample后先配置builder，再prepare、occupancy、finalize | generation/index发布流程不变；降级日志涵盖地址过滤 |
| `capture_fired_request` | 新增向公共get/create传真实sample_seq | 冻结响应和token合同不变 |
| `narrow_region` | 给六类真实外部目标；partial从0x38022080开始 | 最小LD8起点0x80，最大0xff8，完整访问避开内部128B |
| soft路由/策略检查 | 边界、超位宽、固定窗口及无候选 | 只有测试断言，不改变生产算法 |
| soft形状矩阵 | S1/S2/allStage × 五shape × 三PBMT共45组合 | 每次新builder冻结同版本；不驱动接口 |
| soft意图检查 | 重复幂等、冲突拒绝、首次构造、exact hit和reset后替换 | 检查的是构造生命周期 |
| soft vseq | 显式启动软件序列到virtual sequencer | 无memory responder或CSR producer |
| 既有reuse软件测试 | raw候选断言改为原XOR确定性及level对齐 | 最终路由验收交新专项；未删除history验证 |

新字段：`memblock_pma_attribute_span_t`为值型宽边界及属性；policy每次configure清旧c_spans/configured后重新建立；builder策略和意图只属于本次构造；公共意图map在reset_all_tables清除、main写、builder读。窄vseq计数在new清零、仅监测A.fire增加；专项禁止runtime reset后继续旧主表，未引入跨reset静态快照缓存。

### 4.2 文件与diff覆盖表

| 文件（相对memblock根） | 有意义修改及审查结论 |
|---|---|
| `seq/base_seq_help/memblock_pma_pmp_model.sv` | 新只读span结构/API；first-match/default/debug语义保持 |
| `seq/base_seq_help/memblock_routed_ppn_policy.sv` | 固定规模交集、整shape检查、先窗口再槽位；无无限随机重试 |
| `seq/base_seq_help/memblock_routed_ppn_profile_v2.svh` | 4个外部路由、7个采样窗口；独立静态脚本精确核对RTL |
| `seq/base_seq_help/memblock_routed_ppn_intent.svh` | VPN键及值型意图定义，无RM期望字段 |
| `seq/base_seq_help/tlb_map_builder.sv` | 普通候选恢复、PBMT前移、最终stage策略、编码及复用过滤 |
| `seq/base_seq_help/common_data_transaction.sv` | sample/版本传递、意图注册/清理/命中检查及复用接入 |
| `seq/base_seq/memblock_l2tlb_base_sequence.sv` | 真实sample传入公共构造，其他生命周期不变 |
| `seq/base_seq/memblock_main_dispatch_routed_ppn_sequence.sv` | 窄偏移上游生成、独立VPN、ready前意图 |
| `seq/virtual_sequence/memblock_l2tlb_routed_ppn_vseq.sv` | 静态CSR/真实调度、被动到达计数、runtime reset终止 |
| `seq/base_seq/soft_test/soft_test_l2tlb_routed_ppn_sequence.sv` | 纯软件边界、45矩阵、版本、fallback、意图专项 |
| `seq/virtual_sequence/soft_test/memblock_l2tlb_routed_ppn_soft_vseq.sv` | 仅软件调度，无真实请求覆盖声明 |
| `seq/base_seq/soft_test/soft_test_l2tlb_ppn_reuse_sequence.sv` | 旧固定窗口断言替换为raw候选合同 |
| `env/plus.sv` | 两个参数定义及load_int接入 |
| `seq/base_seq_help/seq_csr_common.sv` | 初始化冻结、getter、概率0..100和ID -1..6校验 |
| `seq/plus_cfg/default.cfg` | 默认50/-1与plus/getter一致 |
| `seq/plus_cfg/tc_l2tlb_routed_ppn_narrow.cfg` | 60笔真实静态S1/4KiB/N0/普通LD/PBMT00配置 |
| `seq/plus_cfg/tc_l2tlb_routed_ppn_soft.cfg` | 软件专用零事务及stage合法字段配置 |
| `seq/seq.f` | 仅include目录/本轮文件注释索引，新增类仍统一由seq_pkg编译 |
| `seq/plus_cfg/tc_l2tlb_ppn_reuse_smoke.cfg` | 关闭独立自愿窗口重选，隔离reuse强断言，不关闭B08保护 |
| `seq/seq_pkg.sv` | 模型→policy/intent→builder→common；base先于派生vseq |
| `tc/src/basicTest.sv` | 两个能力白名单补登记真实窄vseq；不改软vseq |
| `scripts/check_memblock_v2_routed_ppn_profile.py` | 解析生成RTL表达式精确归并，不用随机抽点冒充全集证明 |

文档同步覆盖：本轮plan、独立plan review、新增及既有flow、L2 responder源码分析、framework地址合同分析、额外NC迟到写回观察、参数管理文档，以及旧B08 plan/review的历史注记。母问题总review新增103行章节10为任务开始前已有用户修改，不属于本任务，不应stage或据此扩大本轮review范围。

### 4.3 性能和风险边界

构造策略最多32项PMA及固定边界、7窗口×4路由×固定区间，复用只扫既有有界history；没有在每拍或每个monitor事件全表扫主表。真实请求版本历史查找每次构造冻结一次，不在每个候选反复取latest。route profile只描述当前V2生成RTL，重生成后须重新运行静态脚本。普通地址重选不承诺RWX/PMP与原候选等价，既有RM按最终地址判断；动态配置和专门fault范围走既有专项边界。

## 5. 最终验证记录与范围边界

### 5.1 编译及静态检查

最终源码使用VCS在eda01编译，命令为：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=basicTest ts=virtual_base_sequence mode=routed_ppn_20261102 wave=off ccov=off fcov=off
```

r6退出码0，日志`/tmp/routed_ppn_compile_20261102_r6.log`，VCS编译摘要保存在`sim/routed_ppn_20261102/log/vcs_compile_rtl.log`。后续仅文档编辑，验证所用生产源码无变化。额外波形复现由独立mode重新编译，不覆盖此simv。

路由profile精确比对PASS；APLIC M结束、S结束及内部128B结束三处负向突变均被脚本拒绝。`git diff --check`通过；implementation review的35个SV源码块均在结束后5行内紧跟对应中文伪代码。seq.f不重复编译package内的类。

### 5.2 最终源码的计划验收

结果根为`mem_ut/ver/ut/memblock/sim/routed_ppn_20261102_results/`。下表每行的结果目录均保留`config.txt`、`effective_plusargs.txt`、`summary.log`、`results.tsv`以及`status/seed_<seed>.final.log`。

所有16次计划验证均三层核对：summary的result=PASS，全部计划任务完成；results行均PASS/EXIT_CONFIRMED/rc=0/uvm_count=0；实际日志UVM_ERROR/FATAL=0且正常$finish。每批的remaining/not_run/infra/incomplete/aborted/pending/remote_unknown_jobs/simv_changed均为0。

| 结果目录 | testcase / 场景 / cfg | seed | 验证内容与结果 |
|---|---|---|---|
| `final_reuse` | `tc_l2tlb_ppn_reuse_smoke` / `virtual_base_sequence` / `tc_l2tlb_ppn_reuse_smoke` | `21102026` | 旧PPN reuse原强断言，PASS |
| `final_soft_multi` | `basicTest` / `memblock_l2tlb_routed_ppn_soft_vseq` / `tc_l2tlb_routed_ppn_soft` | `21102027` | 软件45组stage/shape/PBMT、PMA版本/边界/fallback/意图，PASS |
| `final_soft_multi` | `basicTest` / `memblock_l2tlb_routed_ppn_soft_vseq` / `tc_l2tlb_routed_ppn_soft` | `21102028` | 软件45组stage/shape/PBMT、PMA版本/边界/fallback/意图，PASS |
| `final_narrow_multi` | `basicTest` / `memblock_l2tlb_routed_ppn_vseq` / `tc_l2tlb_routed_ppn_narrow` | `21102027` | 60笔真实窄窗口LD，每六窗口各10次A.fire，PASS |
| `final_narrow_multi` | `basicTest` / `memblock_l2tlb_routed_ppn_vseq` / `tc_l2tlb_routed_ppn_narrow` | `21102028` | 60笔真实窄窗口LD，每六窗口各10次A.fire，PASS |
| `final_baseline` | `basicTest` / `virtual_base_sequence` / `default` | `21102026` | 默认启动骨架约200ns；不冒充真实访存回归，PASS |
| `final_pma_fifo` | `soft_test_tc_pma_pmp_csr_fifo` / `virtual_base_sequence` / `default` | `21102026` | 既有PMA CSR FIFO软件时序，PASS |
| `final_historical_b08` | `basicTest` / `memblock_dispatch_real_smoke_vseq` / `tc_dispatch_real_smoke` | `1242164593` | 原入口+MAIN_TRANS_NUM=1000，PASS |
| `final_historical_b08` | `basicTest` / `memblock_dispatch_real_smoke_vseq` / `tc_dispatch_real_smoke` | `165489856` | 原入口+MAIN_TRANS_NUM=1000，PASS |
| `final_historical_b08` | `basicTest` / `memblock_dispatch_real_smoke_vseq` / `tc_dispatch_real_smoke` | `898041954` | 原入口+MAIN_TRANS_NUM=1000，PASS |
| `final_historical_b08` | `basicTest` / `memblock_dispatch_real_smoke_vseq` / `tc_dispatch_real_smoke` | `37617171` | 原入口+MAIN_TRANS_NUM=1000，PASS |
| `final_cacheable` | `basicTest` / `memblock_dispatch_real_smoke_vseq` / `tc_dispatch_real_smoke` | `21102100` | 原real_smoke+120笔+PBMTE开启+PBMT00，PASS |
| `final_soft_anchor` | `basicTest` / `memblock_l2tlb_routed_ppn_soft_vseq` / `tc_l2tlb_routed_ppn_soft` | `21102026` | 软件同上，PASS |
| `final_narrow_anchor` | `basicTest` / `memblock_l2tlb_routed_ppn_vseq` / `tc_l2tlb_routed_ppn_narrow` | `21102026` | 60笔真实窄窗口LD，每六窗口各10次A.fire，PASS |
| `final_nc_pure_load` | `basicTest` / `memblock_dispatch_real_smoke_vseq` / `tc_l2tlb_routed_ppn_narrow` | `21102101` | 静态CSR纯LD，120笔，120份新映射PBMT=01，PASS |
| `final_io_pure_load` | `basicTest` / `memblock_dispatch_real_smoke_vseq` / `tc_l2tlb_routed_ppn_narrow` | `21102102` | 静态CSR纯LD，120笔，120份新映射PBMT=10，PASS |

所有运行统一使用run_eda_regression.sh、--no-compile --no-wave --no-coverage --monitor --record；多seed组jobs=4，显式seeds即表中种子。real_smoke cacheable额外参数为MAIN_TRANS_NUM=120、M/H_PBMTE_EN=1、S1_PBMT_0/1/2_WT=100/0/0；NC/IO使用同一real_smoke入口加窄cfg（其中关闭地址复用、固定权限、仅LD），分别覆盖S1_PBMT权重为0/100/0和0/0/100。窄vseq本身仅PBMT00意图专项，未用它冒充NC/IO。

窄窗口三个seed 21102026/21102027/21102028各发60笔，UART128B、APLIC_M、APLIC_S、TIMER、PLIC、EXTERNAL_PARTIAL_PAGE各10笔，总180笔。独立审查逐条核对实际A.fire地址及完整8B访问末地址均在对应窗口。普通未知偏移路径仍按完整页形状约束；小窗口覆盖依赖上游意图。

baseline仅验证默认启动骨架；真实访存验收来自四个历史1000笔、三个窄窗口60笔和三个120笔属性场景。

软件专项明确断言PBMT00+c1地址0x20001234000在旧512GiB窗口外仍保持原PPN；历史0x311368c0所在页强制同c0合法重选，即使自愿重选权重为0也不允许地址洞。S1/S2/allStage、4K/64K/2M/1G/512G及00/01/10共45组合均有检查，不能据此声称任意未来PMA和RTL配置已穷尽验证。

逐日志SHA256如下，用于区分运行证据，不能替代结果核对：

| 目录 / seed | final.log SHA256 |
|---|---|
| `final_reuse/21102026` | `7d5921ffa55f349f677763e4e3d1a0da6c3fc9b73ee4c4e1cf9afc7ea9f8d42c` |
| `final_soft_multi/21102027` | `b428fb3e851d825d5baccfc7ae7be368bd693c4f6bebafe0fa7a35698e5b113d` |
| `final_soft_multi/21102028` | `feadc4c2798d7b8b34381c4d49246c9fc611b77f0d6513ee338e82444b37f0d6` |
| `final_narrow_multi/21102027` | `015c2422a891b5eda5e80ebe4d12feb3a4626a50bf9f498c12e8234289370597` |
| `final_narrow_multi/21102028` | `e039aff9cceab095ac3ae3c13546200a94a935934bc7121f8df56656ce3bed26` |
| `final_baseline/21102026` | `169d9e9282d958824658921c09ec2295345ed99e2891ea85ae2e792ce32e37ff` |
| `final_pma_fifo/21102026` | `d18291701e026f52b167b164b242b23da400628265b7e50dcc3dff1629372988` |
| `final_historical_b08/1242164593` | `f88d4abdb47c7436e93de1d916093527f7db886a13a3d032503ff16f4137d69a` |
| `final_historical_b08/165489856` | `7cfab0bfc5ecada7dca4ce2d00993e5182a7e9d27ef49b5262ae00d55e99a34d` |
| `final_historical_b08/898041954` | `9d050f3127ae6ed9e4706d0deeba10b562896dd471874c43cacb849d9e3e41a3` |
| `final_historical_b08/37617171` | `4116da04401034f179d464adaf01a08947f30353603c0e2b86c9f3f0beff320f` |
| `final_cacheable/21102100` | `527a46432ed9e842010f8d5a89b25567608a12e5f1108413651b3a257bfc826a` |
| `final_soft_anchor/21102026` | `245e800e420ed99bfabd178d7a4fcb1c80ce516e286efca3595ab00aa58f354e` |
| `final_narrow_anchor/21102026` | `156b3ebc9c53ac613e50cdbfe60ddd71e108d0254305705a6b43ba8fdb803b2f` |
| `final_nc_pure_load/21102101` | `c6942e5f63a95e49aae77a0e537db285cef3052d598d67fa8067f592c10b7d01` |
| `final_io_pure_load/21102102` | `a848ceb365211eac01b48b81897de24dfc14d5e37f840e5be12d6f85fc2a5a4d` |

### 5.3 开发过程中失败及修正

这些历史失败保留原结果，不计入上表最终16次通过：

| 历史尝试 | 现象 | 处理及复测 |
|---|---|---|
| r2编译 | 新软件夹具变量edge触发SV关键字语法错误 | 改edge_idx；r3及最终r6编译通过 |
| 初始软件/soft_r2 | cfg的MAIN_TRANS_NUM=0违反现有非零配置合同 | cfg设1，夹具reset_all_tables(1)后内部零事务；最终三seed通过 |
| soft_r3 | debug边界断言混用多次query_c输出变量 | 每个边界独立查询/独立断言；日志确认PMA值，soft_r4及最终三seed通过 |
| narrow_r1 | L2 responder不在启用拓扑 | 补basicTest两个能力白名单；narrow_r2及最终三seed通过 |
| reuse | WT0对比两个独立构造PPN受新50%窗口重选影响 | 旧专项固定窗口重选权重0并加配置断言；保留原强断言，final_reuse原seed通过 |

### 5.4 额外混合NC场景的未分类问题

另跑原real_smoke cfg、seed21102101、120笔、PBMT=01时，`final_nc`在7467.8ns出现`INT_WB_ATTACH no active uid for ROB key=0/49`。原参数在独立mode `routed_ppn_nc_debug_20261102`、结果`nc_debug`打开波形后同时间同错误复现。两次均为FAIL，未将其计入计划验证PASS，也不把改变配置后的纯LD通过当作该原seed原配置已修复。

只读独立分析发现该cfg仍启用地址复用，可把Load改为Store；S1 PTE U默认权重11%，会产生大量PF/redirect。UID49自身为合法DRAM NC映射且A请求已到Uncache responder；随后发生异常redirect、LQ释放以及迟到integer WB无法关联active owner。本路径与B05 STD value-only双候选不同，不能直接认定B05复发，也未证明是RTL缺陷或新route策略缺陷。

这项额外混合异常场景仍为PAUSED_ANALYSIS，需要独立完成WB/redirect归属根因闭环；本轮不修改adapter/RTL，不登记未经分类的正式Buglist。方案内静态普通load的NC/IO均有真实120笔通过证据；对完整混合异常回归仍保留此明确限制。波形实际确认7435.3ns redirect到DUT、7460.3ns LDA2输出ROB49、7465.3ns monitor采样；完整外部返回与该动态实例的一一归属仍待确认。详细原参数、信号路径和确认/待定边界见`AI_DOC/analysis/framework_design/memblock_nc_redirect_late_writeback_observation_20261102.md`。原值摘录在`nc_debug/wave_observation.tsv`，不将sim产物纳入源码提交。

## 6. Plan对齐检查

本评审区分执行前方案和执行中增补；IMPLEMENTATION_DELTA不被当成原plan早已说明。除下列五项记录外，未发现尚未说明的架构行为差异。

## 实现与Plan不一致项（与plan不一致的实现）

### D1. 显式覆写主调度替代临时factory override

原plan：临时覆盖main类型后恢复工厂。当前实现：窄vseq覆写start_core_dispatch_flow，显式启动专用main，其余agent/CSR屏障保持父链。原因：标准type override删除/恢复不适合作无副作用局部切换。源码和完整中文伪代码见第3章对应函数。处理：接受已标记delta，保持当前实现。



抽象功能描述：窄vseq在原生命周期中显式调度专用main，避免全局类型覆盖。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_routed_ppn_vseq.sv`。以下片段证明对应差异的具体落点。

```systemverilog
    virtual task start_core_dispatch_flow();
        memblock_lsqenq_dispatch_base_sequence lsqenq_seq;
        memblock_issue_dispatch_base_sequence issue_seq;
        memblock_lsqcommit_dispatch_base_sequence lsqcommit_seq;
        memblock_l2tlb_base_sequence l2tlb_seq;
        memblock_main_dispatch_routed_ppn_sequence main_seq;
        memblock_mmu_sv39_csr_sequence mmu_sv39_csr_seq;
        fork
            begin
                `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
            end
            begin
                wait_for_explicit_l2tlb_start_barrier();
                `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
            end
            begin
                `uvm_do_on(lsqcommit_seq, p_sequencer.lsqcommit_sqr)
            end
            begin
                wait_for_explicit_l2tlb_start_barrier();
                `uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr)
            end
            begin
                `uvm_do_on(main_seq, p_sequencer)
            end
            begin
                `uvm_do_on(mmu_sv39_csr_seq, p_sequencer.csr_ctrl_sqr)
            end
        join
    endtask
```
中文伪代码：先定义原agent序列和专用main，并行启动原lsqenq、issue、commit、L2及静态CSR；issue与L2等待现有CSR屏障，main只替换类型并运行于virtual sequencer，最后join所有自然结束的任务，不安装或恢复工厂状态。

### D2. allStage仅最终stage降级

原plan：active stage全部level0/N0。当前实现：S2失败只改S2，S1 GPA和tag不回改。原因：S2为4KiB已经令最终范围为4KiB，重写S1将破坏已经冻结的S2输入链。源码和伪代码见select_final_routed_ppn；既有derive_allstage_lookup_shape从最终level/N推导索引。处理：接受delta，以软件fallback及真实邻接验证闭环。



抽象功能描述：最终页选择者仅在普通形状无地址候选时改变当前最终stage。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv`。以下片段证明对应差异的具体落点。

```systemverilog
            if (directed_shape || (level == 0 && !napot)) return 1'b0;
            level = 0; napot = 0;
            if (s1) begin entry.s1_level = 0; entry.s1_pte_n = 0; end
            else begin entry.s2_level = 0; entry.s2_pte_n = 0; end
            // 中文注释：保留第一次resolved anchor作4K候选，冻结PBMT00原c类别。
            // 重新hash会改变地址，可能把原c0权限域静默变为c1。
            canonical_ppn = anchor;
            fell_back = 1'b1;
```
中文伪代码：显式形状或普通4KiB已经无解则立即失败；否则清局部level/N，只更新当前最终stage字段。把本次resolved anchor作下一轮canonical候选，并标记一次降级；S2分支不修改S1 GPA、tag或sector，第二轮仍无候选则结束失败。

## Plan未说明但Coding落实的细节（plan未说明但实现补充的细节）

### D3. fallback冻结第一次resolved anchor

原plan：4KiB重新构造候选，没有明确冻结anchor。当前实现：使用原resolved anchor作降级候选。原因：重新hash可能改变PMA c类别，把原MMIO悄然变cacheable。源码位置为select_final_routed_ppn降级分支，中文伪代码已逐步说明；check_bounded_fallback构造单一4KiB c0岛验证。处理：接受delta，宽地址不截断约束不变。



抽象功能描述：降级时保留本次地址类别，防止另一次hash改变MMIO含义。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv`。以下片段证明对应差异的具体落点。

```systemverilog
            // 中文注释：保留第一次resolved anchor作4K候选，冻结PBMT00原c类别。
            // 重新hash会改变地址，可能把原c0权限域静默变为c1。
            canonical_ppn = anchor;
```
中文伪代码：将已经按原形状解析的anchor直接赋给4KiB候选；下一次尝试不重新hash，因此可表示anchor的PMA c保持一致，超位宽anchor也不被截断为另一个合法地址。

### D4. basicTest能力登记

原plan：描述vseq继承和调度，未写名称能力表。当前实现：只给真实窄vseq增加L2启动和静态CSR两项能力。原因：继承并不会自动进入testcase名称白名单；遗漏会导致场景未进入active topology，或CSR producer冲突。源码和中文伪代码见两个basicTest函数。处理：接受已标记delta，软件vseq维持无真实responder。



抽象功能描述：testcase用名称能力表准备L2显式请求响应拓扑。

源码位置：`mem_ut/ver/ut/memblock/tc/src/basicTest.sv`。以下片段证明对应差异的具体落点。

```systemverilog
      function bit vseq_starts_l2tlb(input string vseq_name);
          return vseq_name == "memblock_l2tlb_routed_ppn_vseq" ||
                 vseq_name == "memblock_dispatch_real_smoke_vseq" ||
                 vseq_name == "memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq" ||
                 vseq_name == "memblock_dispatch_manual_control_vseq" ||
                 vseq_name == "memblock_csr_random_config_vseq" ||
                 vseq_name == "memblock_csr_scalar_load_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_store_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_mixed_10k_vseq" ||
                 vseq_name == "memblock_csr_scalar_store_sta_std_10k_vseq" ||
                 vseq_name == "memblock_dispatch_real_cancel_reconcile_vseq" ||
                 vseq_name == "memblock_l2tlb_pbmt_response_fault_vseq";
      endfunction:vseq_starts_l2tlb
```
中文伪代码：新增真实窄vseq名称，其他既有名称不变；返回真假被生命周期初始化读取，软件vseq未列入，不会误启真实responder。


抽象功能描述：testcase避免静态CSR和旧默认producer同时拥有接口。

源码位置：`mem_ut/ver/ut/memblock/tc/src/basicTest.sv`。以下片段证明对应差异的具体落点。

```systemverilog
      function bit vseq_owns_static_mmu_csr(input string vseq_name);
          return (vseq_name == "memblock_l2tlb_routed_ppn_vseq" ||
                  vseq_name == "memblock_dispatch_real_smoke_vseq" ||
                  vseq_name == "memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq" ||
                  vseq_name == "memblock_l2tlb_pbmt_response_fault_vseq") &&
                 !memblock_sync_pkg::uses_control_barrier_topology();
      endfunction:vseq_owns_static_mmu_csr
```
中文伪代码：在静态CSR名称集合加入真实窄vseq，并保留无control-barrier拓扑条件；匹配后仅使旧默认序列转安全空闲，不增加第二个producer。

### 测试夹具和状态细节

本轮测试采用reset_all_tables(1)建立合法容量后将实际事务数置0；CSR直接setup使用独立sample，避免同sample被模型视作歧义。这些是专项夹具对既有公共合同的适配，不修改生产协议。意图冲突为输入生命周期合法性检查，无意图的普通命中不执行该检查。实际A.fire计数仅提供激励到达证据，不新增数据或异常checker。

### D5. 软件专项隔离与目录接入

执行前plan未说明软件旧reuse专项会用重复独立构造作基线。默认新窗口重选权重50使同一个c0候选的两次构造可选不同窗口，导致WT0断言比较了不同随机状态；这是测试隔离问题，不能删除强断言。当前reuse cfg固定REGION_RANDOM_WT=0、REGION_ID=-1，序列增加getter断言，其他reuse disabled/WT0强断言不变；非法候选仍由生产route策略强制重选。新软件base和vseq分别放在base_seq/soft_test和virtual_sequence/soft_test；seq_pkg对后者使用相对include，seq.f仅补文件索引注释，无重复编译。

处理结论：接受测试工程delta。首次reuse邻接WT0失败应保留为历史失败并记录修复后同seed结果，不能把旧失败日志当最终PASS。目录调整后以r6最终源码编译和结果为准。



抽象功能描述：旧reuse专项在执行前确认地址策略随机源已被隔离，维持原强断言。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_ppn_reuse_sequence.sv`。以下片段证明对应差异的具体落点。

```systemverilog
function void soft_test_l2tlb_ppn_reuse_sequence::check_smoke_cfg_snapshot();
    // 中文注释：专项 cfg 必须经过 plus -> seq_csr_common -> responder 配置快照；
    // 此处不替代后续各定向分支的局部 policy，只验证 testcase preset 的真实消费链。
    expect_true(seq_csr_common::get_l2tlb_ppn_reuse_en() == 1'b1 &&
                seq_csr_common::get_l2tlb_ppn_reuse_history_size() == 1 &&
                seq_csr_common::get_l2tlb_ppn_reuse_wt() == 100,
                "PPN reuse smoke cfg must reach seq_csr_common getters");
    expect_true(responder.ppn_reuse_en == 1'b1 &&
                responder.ppn_reuse_history_size == 1 &&
                responder.ppn_reuse_wt == 100,
                "PPN reuse smoke cfg must be frozen by responder.configure_from_plus");
    expect_true(seq_csr_common::get_l2tlb_level_weight_en() == 1'b0,
                "PPN reuse smoke cfg must force deterministic 4KB page shape");
    expect_true(seq_csr_common::get_main_mem_ranges_en() == 1'b0,
                "PPN reuse smoke cfg must exercise sparse PPN generation");
    // 中文注释：保持合法原候选的地址策略稳定，使两次miss构造的差异只来自reuse。
    // 地址洞保护仍生效；不能通过关闭路由策略来让历史非法PPN通过。
    expect_true(seq_csr_common::get_l2tlb_ppn_region_random_wt() == 0 &&
                seq_csr_common::get_l2tlb_ppn_region_id() == -1,
                "PPN reuse smoke must disable independent voluntary region reselection");
endfunction:check_smoke_cfg_snapshot
```
中文伪代码：先确认reuse开关、history容量、reuse权重、4KiB形状及sparse模式符合旧专项，再确认新区域自愿重选权重为0且ID为-1。此检查不关闭非法地址强制重选；失败说明配置不能隔离reuse实验，应报错而不放松WT0预期。

## 7. 非本次修改的逻辑分析

评审开始已执行git status和diff；以下已存在改动不属于本轮物理页策略，保持原内容且不纳入本review正确性结论。

| 类别 | 文件 | 说明 |
|---|---|---|
| 其他RTL/接口知识文档 | `AI_DOC/analysis/interface/v2/agents/int_writeback_agent.md`、`AI_DOC/analysis/rtl/v2/flows/rob_compress_and_backend_instruction_flow.md`、`AI_DOC/analysis/rtl/v2/index.md`、`AI_DOC/analysis/rtl/v2/flows/backend_memory_writeback_flow.md` | 属于后端/写回知识维护，非本轮源码修改 |
| 其他缺陷/准入方案 | `AI_DOC/analysis/framework_design/memblock_b05_rob_sq_lifecycle_recheck_20261031.md`、`AI_DOC/plan/test_framework/plan/undo/memblock_lsq_rtl_equivalent_admission_formal_plan_20261031.md`、`AI_DOC/plan/test_framework/review_doc/undo/memblock_lsq_admission_b05_evidence_review_20261102.md` | B05/LSQ方向，另行review |
| 既有母问题review | `AI_DOC/plan/test_framework/review_doc/undo/memblock_mother_tb_rm_bug_final_solution_review_20261031.md` | 103行章节10为既有用户修改，不stage，不把本轮review与其混合 |
| 集成状态文档 | `AI_DOC/project_management/memblock_mother_integration_status_20261028.md` | 已有集成记录，不整体替其正确性背书 |
| 其他参数测试 | `mem_ut/ver/ut/memblock/scripts/tests/test_runtime_parameter_priority.sh` | 参数优先级专项，非新窗口策略 |
| 仿真配置/生成产物 | `mem_ut/ver/ut/memblock/signal.rc`及sim结果目录 | 波形和运行产物，不加入生产源码评审 |

## 8. 结论

源码静态评审未发现本地址方案必须修改项；五个执行中变更已有明确理由和源码落点。16次计划验收通过；额外混合NC异常路径仍保留第5.4节未分类风险，不能据此宣称全部混合异常回归通过。独立结果复核已逐项确认16条日志SHA和零ERROR/FATAL；额外失败不构成已确认的地址策略回改依据。允许在本节明确限制下归档plan并本地提交，不能把它宣称为整体混合异常环境无错误。
