# V2 L2TLB completed-response PPN reuse 实现评审

关联 plan：
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_ppn_reuse_response_history_coding_plan_20260908.md`。

评审日期：2026-09-08

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| PPN | 页号；S1/only-S1 取 request-specific S1 final PPN，S2/allStage 取 request-specific S2 final PPN。 | `request_s1_resolved_ppn`、`request_s2_resolved_ppn` | range hit 不能直接记录 anchor raw PPN。 |
| PPN history | 最近已完成 response 的有界 FIFO；每次 completion 保留一个 record。 | `common_data_transaction.l2tlb_ppn_history_q` | `M=2` 时第三个 completion 淘汰最早 record。 |
| response completion | 已驱动 response 在 DUT 下一采样边界确认可见的时刻，不等同于 response select。 | `complete_driving_response()` | `driving_valid && sampled_resp_valid` 为真才允许入 FIFO。 |
| frozen token | request fire 后保存 request-time CSR、entry snapshot、derived PPN 和 response 时序的独立记录。 | `memblock_l2tlb_pending_req` | completion 只能读取 token snapshot，不回读 live TLB 表。 |
| provenance | record 的审计来源字段，不参与 key 或 candidate 选择。 | `response_token`、`complete_sample_seq` | 首个合法 token 是 `0`，它仍可作为 provenance。 |
| invalid record | 占据一次“最近返回”位置但不可复用的 history item。 | `ppn_valid=0` | fault、PMA AF、unresolvable response 都写 invalid item。 |
| miss build | exact/range hit 都失败后建立新的 canonical live entry 的分支。 | `MEMBLOCK_TLB_LOOKUP_MISS_BUILD` | 只有该分支可改 final PPN。 |
| leaf PTE | R/W/X 至少一个为 1 的最终翻译 PTE；R/W/X 全零的 S2 payload 是 non-leaf。 | `s2_pte_r/w/x` | non-leaf S2 不能成为 reuse target。 |
| allStage | 同时经过 S1 与 S2 的 `s2xlate=3` 翻译；final target 为 S2。 | `memblock_tlb_entry.s2xlate` | 只覆写 S2 final PPN，不改 S1 raw mapping。 |
| runtime reset | L2TLB 生命周期的新 epoch；它清除跨代状态。 | `reset_l2tlb_sfence_state()` | 清 live entry 和 PPN history，普通 SFENCE/HFENCE 不清 history。 |

## 2. 评审范围与结论

本次评审覆盖以下实现类别：

- 三个 runtime plus 参数、`seq_csr_common` 校验/getter 与 default/preset cfg。
- completed-response FIFO、miss-build reuse wrapper、S1/S2 final PPN encoding 和 runtime reset clear。
- L2TLB responder 的 request capture/completion 接入。
- software-only smoke、testcase、package/filelist 接入。
- 参数规则、L2TLB 规则、源码分析和历史设计文档同步。

结论：实现满足用户要求的 enable 门控、每次真实返回收集、最近 `M` 项 FIFO、`WT` 随机 reuse 和默认完全旁路语义。独立复查发现的 token `0` 与 S2/allStage non-leaf 风险均已修正并被专项 smoke 覆盖。当前无阻塞代码问题。

## 3. 参数链与默认旁路

### 3.1 `plus.sv` 与 `default.cfg`

修改前逻辑：L2TLB responder 没有 PPN history/reuse 的 runtime 输入，无法以默认关闭方式控制该行为。

修改后逻辑：`plus.sv` 声明并解析三个公共参数，`default.cfg` 明确给出 `EN=0/M=5/WT=40`；测试用例只通过 cfg 传入参数，不在 testcase 源码直接赋 `plus::` 字段。

正确性检查：enable 是保守默认值；容量和权重仍保持 signed input 到公共 getter 的校验链，关闭时允许 `M=0` 而不触发 history path。

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，参数定义与 `reload_from_cmdline()`。

```systemverilog
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2TLB_PPN_REUSE_EN, bit, 1'b0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2TLB_PPN_REUSE_HISTORY_SIZE, int, 5)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2TLB_PPN_REUSE_WT, int, 40)

    load_bit("MEMBLOCK_L2TLB_PPN_REUSE_EN", MEMBLOCK_L2TLB_PPN_REUSE_EN);
    load_int("MEMBLOCK_L2TLB_PPN_REUSE_HISTORY_SIZE", MEMBLOCK_L2TLB_PPN_REUSE_HISTORY_SIZE);
    load_int("MEMBLOCK_L2TLB_PPN_REUSE_WT", MEMBLOCK_L2TLB_PPN_REUSE_WT);
```

中文伪代码：该段先建立默认关闭的 reuse 开关、最近返回条数和百分比权重；仿真启动时从命令行或 cfg 读取同名 plus 覆盖默认值。它只完成输入解析，不在这里创建 FIFO、选择 PPN 或影响接口连接。

### 3.2 `seq_csr_common::check_l2tlb_ppn_reuse_cfg()`

抽象功能描述：该函数在公共 plus 快照完成后、responder 开放 ready 前拒绝不可定义的容量和权重组合；它不读取 history，也不改变 TLB entry。

修改前逻辑：L2TLB response 参数已有统一加载入口，但没有 PPN reuse 的范围和依赖校验。

修改后逻辑：M 使用 `get_non_negative_int()` 防止负值转换；`M<=256`、`WT<=100`，且 enable 时 `M` 必须非零。三个 getter 使 sequence 不直接长期读取 `plus::`。

正确性检查：`WT=0` 是合法的“从不复用”行为；`EN=0,M=0` 是合法的完全旁路；非法 `M/WT` 在启动阶段 fail-fast。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`，函数 `check_l2tlb_ppn_reuse_cfg()`。

```systemverilog
    if (l2tlb_ppn_reuse_history_size > 256) begin
        `uvm_fatal("SEQ_CSR_CFG",
                   $sformatf("MEMBLOCK_L2TLB_PPN_REUSE_HISTORY_SIZE=%0d exceeds framework bound=256",
                             l2tlb_ppn_reuse_history_size))
    end
    if (l2tlb_ppn_reuse_wt > 100) begin
        `uvm_fatal("SEQ_CSR_CFG",
                   $sformatf("MEMBLOCK_L2TLB_PPN_REUSE_WT=%0d must be within [0:100]",
                             l2tlb_ppn_reuse_wt))
    end
    if (l2tlb_ppn_reuse_en && l2tlb_ppn_reuse_history_size == 0) begin
        `uvm_fatal("SEQ_CSR_CFG",
                   "MEMBLOCK_L2TLB_PPN_REUSE_HISTORY_SIZE must be non-zero when reuse is enabled")
    end
```

中文伪代码：先拒绝超过 256 的软件 FIFO 容量，再拒绝不能表达百分比的权重；最后只有在开关已经开启时要求 FIFO 至少容纳一个 completion。任何检查失败都在 response 生命周期开始前终止，成功时不产生随机数或状态副作用。

调用关系：

| 调用顺序 | 函数 | 在本流程中的功能 |
|---|---|---|
| 1 | `seq_csr_common::load_from_plus()` | 把 plus 输入冻结为公共 runtime 快照。 |
| 2 | `seq_csr_common::validate_and_clamp()` | 在公共初始化阶段完成范围检查。 |
| 3 | `memblock_l2tlb_base_sequence::configure_from_plus()` | 再执行纯校验并复制 getter 到 responder 私有字段。 |

## 4. Completed-response history 与 miss reuse

### 4.1 History record 和 FIFO 生命周期

抽象功能描述：`record_l2tlb_completed_ppn_history()` 仅把已完成 token 的 final PPN 或 invalid 占位追加到 FIFO，并裁剪到调用者冻结的 `M`；它不 lookup live table，也不选择新 mapping 的 PPN。

修改前逻辑：没有 completed-response FIFO，L2TLB live entry 的存在不能表达真实 response 完成顺序。

修改后逻辑：`common_data_transaction` 维护轻量 record queue，构造时和 `reset_all_tables()` 清空；fault/PMA AF/unresolvable 仍进入 FIFO 但 `ppn_valid=0`。

正确性检查：history 的真源是 completion，而不是 request fire 或 response select。response token 只用于日志和审计，首个合法 token `0` 不会被误判为未初始化。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，函数 `record_l2tlb_completed_ppn_history()`。

```systemverilog
        if (entry == null || complete_sample_seq == 0 || history_size == 0) begin
            `uvm_fatal("COMMON_DATA",
                       "PPN history record requires entry, non-zero sample and non-zero capacity")
        end
        record = '{default:'0};
        record.s2xlate = s2xlate;
        record.response_token = response_token;
        record.complete_sample_seq = complete_sample_seq;
        if (!entry.has_effective_fault() && !entry.pmaAF && request_derived_valid) begin
            case (s2xlate)
                2'd0,
                2'd1: begin
                    record.ppn = request_s1_resolved_ppn;
                    record.ppn_valid = 1'b1;
                end
                2'd2,
                2'd3: begin
                    record.ppn = request_s2_resolved_ppn;
                    record.ppn_valid = 1'b1;
                end
            endcase
        end
        l2tlb_ppn_history_q.push_back(record);
        while (l2tlb_ppn_history_q.size() > history_size) begin
            void'(l2tlb_ppn_history_q.pop_front());
        end
```

中文伪代码：先确认存在 frozen entry、当前 completion 有有效 sample 且 FIFO 容量非零；不检查 token 是否等于零，因为 token `0` 是第一笔真实 request 的合法编号。随后默认构造 invalid record，只有无 effective fault、无 PMA AF 且 derived PPN 有效时，依 `s2xlate` 写入 request-specific S1 或 S2 final PPN。最后把 record 放入队尾，并反复从队首删除最老项直到长度不超过 `M`。

### 4.2 Responder 专用 lookup wrapper

抽象功能描述：`get_or_create_l2tlb_entry_by_req_with_snapshot()` 复用既有 exact/range hit 和 canonical entry 建表流程，仅为 miss build 加入一次可选 PPN policy；通用 lookup API 保持无 reuse 的原有语义。

修改前逻辑：所有 request 都经过通用 lookup wrapper，无法在不影响其它 software test 的前提下给 responder 新建 entry 加策略。

修改后逻辑：通用 wrapper 以 `reuse_en=0` 委托给 responder wrapper；responder wrapper 对 hit 早退，只有 builder 已构造、尚未插表的 miss entry 调用 reuse helper。

正确性检查：hit 分支不会读取 FIFO 或消耗 reuse 随机数；entry 插入后不再改写 PPN，因而不污染 frozen token 或已存在 mapping。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，函数 `get_or_create_l2tlb_entry_by_req_with_snapshot()`。

```systemverilog
        if (has_tlb_entry(request_key)) begin
            entry = tlb_entry_by_key[request_key];
            entry_anchor_key = request_key;
            lookup_result = MEMBLOCK_TLB_LOOKUP_EXACT_HIT;
            created = 1'b0;
            return 1'b1;
        end
        if (find_tlb_range_hit_by_req(request_key, csr_snapshot,
                                      entry_anchor_key, entry)) begin
            lookup_result = MEMBLOCK_TLB_LOOKUP_RANGE_HIT;
            created = 1'b0;
            return 1'b1;
        end
        entry = build_tlb_entry_for_key_with_csr(request_key, csr_snapshot);
        if (ppn_reuse_en) begin
            try_apply_l2tlb_ppn_reuse_to_new_entry(entry, ppn_reuse_wt, reused_ppn);
        end
        insert_tlb_entry(request_key, entry);
        if (!register_tlb_range_index(request_key, entry)) begin
            tlb_entry_by_key.delete(request_key);
            `uvm_fatal("COMMON_DATA", "failed to register new canonical TLB entry in range index")
        end
```

中文伪代码：先检查 exact hit，命中后直接返回旧 entry；再检查 range hit，命中后同样直接返回。两个 hit 都不会进入 history 或随机路径。只有两者均未命中时，builder 基于冻结 CSR 构造新 entry；开关为一时才在插入 table 前尝试复用 final PPN；成功或不成功都按既有方式插入 canonical table 并注册 range index。

### 4.3 有界候选扫描与随机选择

抽象功能描述：`try_apply_l2tlb_ppn_reuse_to_new_entry()` 在新 entry 插表前，从最多 `M<=256` 个 history record 中收集可编码 valid PPN，并按 `WT` 决定是否覆盖 target stage。

修改前逻辑：不存在 history candidate，也没有 PPN 覆写路径。

修改后逻辑：无 history 或 `WT=0` 立即返回；有候选时保留重复 record 的次数权重，`WT=100` 必选一个候选，其他权重才随机决定 reuse。

正确性检查：扫描对象是有明确上界的 FIFO，而不是每拍主表扫描；该 helper 只由离散 miss build 调用。factory builder 和 candidate queue 的额外 allocation 是已知的低频开销，长压力回归可继续观察。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，函数 `try_apply_l2tlb_ppn_reuse_to_new_entry()`。

```systemverilog
        if (l2tlb_ppn_history_q.size() == 0 || ppn_reuse_wt == 0) begin
            return;
        end
        if (!builder.can_apply_reused_final_ppn(entry, '0)) begin
            return;
        end
        foreach (l2tlb_ppn_history_q[idx]) begin
            if (l2tlb_ppn_history_q[idx].ppn_valid &&
                builder.can_apply_reused_final_ppn(entry,
                                                   l2tlb_ppn_history_q[idx].ppn)) begin
                candidate_indices.push_back(idx);
            end
        end
        if (candidate_indices.size() == 0) begin
            return;
        end
        if (ppn_reuse_wt < 100) begin
            if (!std::randomize(choose_reuse) with {
                    choose_reuse dist {1'b1 := ppn_reuse_wt,
                                       1'b0 := 100 - ppn_reuse_wt};
                }) begin
                `uvm_fatal("COMMON_DATA", "failed to randomize L2TLB PPN reuse decision")
            end
        end
```

中文伪代码：FIFO 为空或权重为零时立即保持 builder 原 PPN，既不扫描候选也不消耗新的随机数。然后先判断新 entry 自身是否为合法 target；逐项扫描有效 history record，只把对当前 stage 可编码的 PPN 放进候选列表。候选为空时保持原值；权重小于 100 才对“复用或不复用”做分布随机，权重为 100 则直接进入候选选择。候选下标仍按 record 存在次数保存，所以重复 PPN 自然拥有更高选择概率。

## 5. Target PPN 编码与 leaf 边界

### 5.1 `tlb_map_builder::can_apply_reused_final_ppn()`

抽象功能描述：该函数只判断一个新建 entry 是否能够安全承载指定 final PPN；它不管理 queue、不随机、不插表。

修改前逻辑：builder 没有 PPN reuse eligibility。初版实现虽检查 S2 resolved PPN、level 和 NAPOT，但没有显式拒绝 S2 non-leaf PTE。

修改后逻辑：S1 要求 active、level 0、非 NAPOT 且 resolved valid；S2/allStage 额外要求 R/W/X 至少一个为一，且候选 PPN 的高 6 bit 为零以适配 V2 38-bit wire。

正确性检查：allStage 只作用于 S2 target，不覆盖 S1 raw mapping。non-leaf、superpage、NAPOT、fault、PMA AF 和不可编码 S2 PPN 均保持 builder 原结果。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv`，函数 `can_apply_reused_final_ppn()`。

```systemverilog
            2'd2: begin
                return !entry.s1_stage_active && entry.s2_stage_active &&
                       entry.s2_level == 2'd0 && !entry.s2_pte_n &&
                       entry.s2_resolved_ppn_valid &&
                       (entry.s2_pte_r || entry.s2_pte_w || entry.s2_pte_x) &&
                       !|ppn[43:38];
            end
            2'd3: begin
                return entry.s1_stage_active && entry.s2_stage_active &&
                       entry.s2_level == 2'd0 && !entry.s2_pte_n &&
                       entry.s2_resolved_ppn_valid &&
                       (entry.s2_pte_r || entry.s2_pte_w || entry.s2_pte_x) &&
                       !|ppn[43:38];
            end
```

中文伪代码：only-S2 时必须只有 S2 stage active；allStage 时必须同时存在 S1/S2 stage，但 target 仍是 S2。两种情况都先要求 4KB、非 NAPOT、resolved PPN 有效，再确认 S2 PTE 至少有一项 R/W/X 权限，从而排除尚需下游 page walk 的 non-leaf。最后检查 PPN 高位为零，保证能写入 38-bit S2 response field。

### 5.2 Final PPN 写入

抽象功能描述：`apply_reused_final_ppn()` 在 eligibility 已成立后只重编码目标 stage 的 PPN 表示，不改 key、权限、tag、ASID/VMID 或 index owner。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv`，函数 `apply_reused_final_ppn()`。

```systemverilog
            2'd0,
            2'd1: begin
                build_s1_sector_payload(ppn, entry);
                entry.s1_resolved_ppn = ppn;
                entry.s1_resolved_ppn_valid = 1'b1;
            end
            2'd2,
            2'd3: begin
                entry.s2_entry_ppn_raw = encode_s2_entry_ppn(ppn);
                entry.s2_resolved_ppn = ppn;
                entry.s2_resolved_ppn_valid = 1'b1;
            end
```

中文伪代码：S1 target 通过既有 sector helper 同时重建 raw PPN 与低位数组，然后同步更新 resolved PPN；S2/allStage target 通过 38-bit encoder 写 S2 raw 字段并更新 S2 resolved PPN。两条路径都不修改 PTE 权限和 lookup 身份，因此 reuse 只改变新 mapping 的地址结果。

## 6. Responder 与 reset 接入

### 6.1 Request capture 与 response completion

抽象功能描述：`capture_fired_request()` 在 request fire 以冻结 CSR 建表或命中 entry；
`complete_driving_response()` 在真实 sample completion 记录 history，随后继续既有 UID 回填和 token 释放。

修改前逻辑：capture 只调用通用 lookup；completion 不维护 completed-response PPN history。

修改后逻辑：sequence 启动时冻结 EN/M/WT，capture 调用 responder 专用 wrapper 并只传 EN/WT；completion 在 `driving_req` 仍可用时传入 request-specific PPN、entry snapshot、token 和 `M`。

正确性检查：现有 `send_l2tlb_cycle()` 先处理 completion 后处理同 sample request capture，因此刚完成的 record 可立即提供给后续 miss；关闭 enable 时两个新路径均旁路。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，函数 `capture_fired_request()` 与 `complete_driving_response()`。

```systemverilog
    if (!data.get_or_create_l2tlb_entry_by_req_with_snapshot(pending.vpn,
                                                              pending.s2xlate,
                                                              pending.csr_snapshot,
                                                              ppn_reuse_en,
                                                              ppn_reuse_wt,
                                                              pending.request_lookup_key,
                                                              pending.entry_anchor_key,
                                                              pending.lookup_result,
                                                              live_entry,
                                                              created) ||
        live_entry == null) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("failed to get/create L2TLB entry vpn=0x%0h s2xlate=%0d",
                             pending.vpn, pending.s2xlate))
    end
```

中文伪代码：request fire 后，sequence 使用该 token 冻结的 VPN、s2xlate 和 CSR snapshot 调用专用 wrapper。enable 和 weight 同样是启动时冻结的私有配置；wrapper 返回有效 live entry 才能继续复制 entry snapshot、推导 request-specific PPN 和入 pending queue，失败立即报告 lifecycle 无法继续的 fatal。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，函数 `complete_driving_response()`。

```systemverilog
    if (ppn_reuse_en) begin
        data.record_l2tlb_completed_ppn_history(
            driving_req.s2xlate,
            driving_req.request_derived_valid,
            driving_req.request_s1_resolved_ppn,
            driving_req.request_s2_resolved_ppn,
            driving_req.entry_snapshot,
            driving_req.request_token,
            complete_sample_seq,
            ppn_reuse_history_size);
    end
    record_update_count = data.complete_waiting_uid_records_by_response(
        driving_req.entry_snapshot,
        actual_response_csr_snapshot);
```

中文伪代码：在该函数已确认 response completion 和 payload/CSR 一致后，若 enable 为一，就用未释放的 frozen driving token 追加一条 history。record 写完才执行已有的 UID completion；因此 history provenance 和 UID 回填来自同一个 completed response。enable 为零时跳过整个 record 调用，不改变默认的随机和状态写入顺序。

### 6.2 Runtime reset 与 SFENCE/HFENCE 分离

抽象功能描述：adapter 在确认新的 runtime reset epoch 后清除 live entry 和 completed-response history；它不把 history clear 混入普通 SFENCE/HFENCE C4 delete helper。

修改前逻辑：reset 只清 package adapter state 和 live TLB 状态，没有 PPN history 生命周期。

修改后逻辑：`reset_l2tlb_sfence_state()` 的既有 per-epoch 去重分支内增加 dedicated clear；`clear_dispatch_l2tlb_live_entries()` 保持 live-only 职责。

正确性检查：同一 reset epoch 不重复清 FIFO；普通 C4 删除仍可保留刚完成 PPN，满足“最近请求返回”的历史语义。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，函数 `reset_l2tlb_sfence_state()`。

```systemverilog
            if (adapter_reset_serviced_epoch != reset_epoch) begin
                memblock_sync_pkg::reset_l2tlb_adapter_runtime_state(reset_epoch);
                data.clear_dispatch_l2tlb_live_entries();
                data.clear_l2tlb_ppn_history();
                adapter_reset_serviced_epoch = reset_epoch;
            end
```

中文伪代码：adapter 只有在发现当前 reset epoch 尚未服务时才进入清理分支；先执行 package-owned adapter reset，再删除 live table 和 range index，接着仅调用 history 专用 clear 删除 completed response FIFO，最后记录已服务 epoch。后续同一 epoch 的重复 monitor 观察不会再次改变这两类状态。

## 7. Software smoke、testcase 与编译接入

### 7.1 软件 completion harness

抽象功能描述：`soft_test_l2tlb_ppn_reuse_sequence` 只构造公共 API 和未启动 responder 对象，验证 completion/history/reuse 数据流；它不 claim lifecycle owner，也不驱动真实 DUT wire。

修改前逻辑：无专项 directed closure，无法覆盖 token `0`、non-visible response 或 S2 non-leaf target。

修改后逻辑：smoke 先调用 `configure_from_plus()` 断言专项 cfg 到达 getter 和 responder snapshot，再构造 frozen response token；其中第一条 enable completion 使用 token `0`，另建 `completion_visible=0` 路径检查 FIFO 不变。

正确性检查：该 harness 用真实 `complete_driving_response()` 而不是仅直接写 FIFO；因此覆盖 history 写入的 responder 调用点。S2/allStage non-leaf clone 只调用 miss helper，证明 eligibility 抑制 PPN 覆写而不需要不合法 DUT request。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_ppn_reuse_sequence.sv`，函数 `check_smoke_cfg_snapshot()`。

```systemverilog
    expect_true(seq_csr_common::get_l2tlb_ppn_reuse_en() == 1'b1 &&
                seq_csr_common::get_l2tlb_ppn_reuse_history_size() == 1 &&
                seq_csr_common::get_l2tlb_ppn_reuse_wt() == 100,
                "PPN reuse smoke cfg must reach seq_csr_common getters");
    expect_true(responder.ppn_reuse_en == 1'b1 &&
                responder.ppn_reuse_history_size == 1 &&
                responder.ppn_reuse_wt == 100,
                "PPN reuse smoke cfg must be frozen by responder.configure_from_plus");
```

中文伪代码：该段先确认 testcase cfg 中的 EN、M、WT 已被公共 getter 读取，再确认 responder 的启动快照与 getter 完全一致。失败时立即 fatal，避免测试只靠手写 literal policy 通过而没有覆盖 plus 参数链。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_ppn_reuse_sequence.sv`，函数 `check_completion_history_and_same_sample_reuse()`。

```systemverilog
    complete_frozen_response(source_entry, 38'h000_0000_500, 2'd0, csr_snapshot,
                             0, 1, 1'b1, 1'b1, derived_valid, source_ppn);
    expect_true(derived_valid && data.l2tlb_ppn_history_q.size() == 1 &&
                data.l2tlb_ppn_history_q[0].ppn_valid &&
                data.l2tlb_ppn_history_q[0].ppn == source_ppn,
                "token-0 normal completion must record its request-specific S1 PPN");
    complete_frozen_response(invisible_entry, 38'h000_0000_540, 2'd0, csr_snapshot,
                             1, 1, 1'b0, 1'b1, derived_valid, ignored_ppn);
    expect_true(data.l2tlb_ppn_history_q.size() == 1 &&
                data.l2tlb_ppn_history_q[0].ppn == source_ppn,
                "non-visible response must not alter completed PPN history");
```

中文伪代码：先以 token `0` 完成一笔正常 S1 response，并确认 FIFO 写入该 request-specific PPN；随后构造一个 response 尚未被 sample 的 token。第二条 token 不会调用 completion record，因此 FIFO 长度和原始 PPN 必须保持不变。该顺序证明 token zero 合法且 completion gate 生效。

### 7.2 Package/filelist 与 testcase

| 文件 | 修改后职责 | 正确性检查 |
|---|---|---|
| `seq/seq_pkg.sv`、`seq/seq.f` | include `soft_test_l2tlb_ppn_reuse_sequence.sv` 和对应 include 提示。 | sequence 在 `tc_pkg` 前完成编译。 |
| `tc/tc_pkg.sv`、`tc/tc.f` | include `soft_test_tc_l2tlb_ppn_reuse.sv`。 | testcase class 可由 `+UVM_TESTNAME=tc_l2tlb_ppn_reuse_smoke` 找到。 |
| `seq/plus_cfg/tc_l2tlb_ppn_reuse_smoke.cfg` | 提供 `EN=1/M=1/WT=100` 的确定性专项 preset。 | smoke 运行时打印并断言三项配置。 |

`soft_test_tc_l2tlb_ppn_reuse` 继承现有 `soft_test_tc_dispatch_smoke`，只替换其 software sequence hook；它不引入第二个 L2TLB lifecycle owner 或新的 DUT agent topology。

## 8. 文档与规则同步

以下文档已同步当前语义：

- `AI_DOC/project_management/mem_ut_parameter_management.md`。
- `mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`。
- `mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md`。
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`。
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_l2tlb_base_sequence.md`。
- `AI_DOC/plan/test_framework/plan/do/l2tlb_base_seq_plan_20260614.md`。
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_framework_design_20260614.md`。
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`。
- `AI_DOC/analysis/framework_design/dispatch_backend_interface_closure_code_changes.md`。

同步结论：L2TLB agent 仍被描述为 DTLB 上游 responder，不依赖 request `paddr`，不表示 L2Cache/PTW/memory 下游模型。历史 plan 中的旧目录或串行描述均附有当前实现补充，不作为本 feature 的有效行为。

## 9. 验证结果

| 检查 | 结果 | 关键证据 |
|---|---|---|
| `make eda_compile tc=tc_l2tlb_ppn_reuse_smoke mode=l2tlb_ppn_reuse_token0_20260908 cfg=tc_l2tlb_ppn_reuse_smoke` | 通过 | `simv` 生成；VCS/Verdi KDB 日志为 `0 error(s), 0 warning(s)`。 |
| `make eda_batch_run tc=tc_l2tlb_ppn_reuse_smoke mode=l2tlb_ppn_reuse_token0_20260908 cfg=tc_l2tlb_ppn_reuse_smoke` | 通过 | 日志显示 cfg `EN=1/M=1/WT=100`、`record ... token=0`、同 sample miss reuse、`TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`。 |
| `make eda_batch_run tc=basicTest ts=virtual_base_sequence mode=l2tlb_ppn_reuse_token0_20260908 cfg=default` | 通过 | 默认 cfg 打印 `EN=0/M=5/WT=40`，空壳 vseq 正常完成，`TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`。 |
| `git diff --check` 与 PPN reuse 关键字检索 | 通过 | 已覆盖源码、cfg、plan、规则和分析文档，无空白错误。 |

专项 smoke 的 `UVM_WARNING=2`、默认空壳场景的 `UVM_WARNING=14` 均来自既有 agent “no default_sequence”
提示；两组日志均无 PPN reuse assertion、编译、UVM error 或 fatal。它们已保留在日志中，不因本 feature
静默过滤或降级。

`tc_sanity` 不作为本轮通过结论：该 testcase 在当前环境没有 dispatch 主表拓扑，历史尝试显示 `main_trans_num=0` 后 LSQ sequence 持续等待，已人工停止。该现象早于本 feature，且本次使用新编译产物的 `basicTest` 默认场景和专项 smoke 均已通过。未将该未完成运行写成验收通过。

## 10. Plan 对齐检查

关联 plan 的原始正文覆盖参数链、completion FIFO、miss-only reuse、S1/S2 编码、SFENCE/reset 分离、software smoke、package/filelist 和文档同步。实现逐项已落实；执行中的必要修正保留在 plan 的 `IMPLEMENTATION_DELTA`，不伪装为执行前原始设计。

### 10.1 与 Plan 不一致的实现

#### 10.1.1 token `0` provenance

Plan 原有逻辑：plan 描述 record 保存 response token，但没有规定 token `0` 的合法性。

当前源码逻辑：保留 existing token 从 `0` 开始分配的行为，并允许 history record 保存该 token。

不一致原因：coding review 发现把 token `0` 当成非法值会使 enable 后第一笔真实 completion fatal。

源码位置：`common_data_transaction::record_l2tlb_completed_ppn_history()`。

```systemverilog
        if (entry == null || complete_sample_seq == 0 || history_size == 0) begin
            `uvm_fatal("COMMON_DATA",
                       "PPN history record requires entry, non-zero sample and non-zero capacity")
        end
        record.response_token = response_token;
```

中文伪代码：函数只拒绝缺少 entry、没有 completion sample 或没有容量的调用；任何合法的 unsigned token 值，包括零，都会复制到 record 的审计字段。它不改变 token 分配顺序，也不把 token 用于 reuse key。

处理结论：保持当前实现，并已在 plan `IMPLEMENTATION_DELTA` 与 token-zero smoke 中明确记录。

#### 10.1.2 S2/allStage non-leaf eligibility

Plan 原有逻辑：只对 normal 4KB target 复用 PPN。

当前源码逻辑：将 “normal” 细化为 S2/allStage PTE 必须具备至少一个 R/W/X leaf 权限。

不一致原因：S2 resolved PPN 可解析并不表示 PTE 为 leaf；不补此条件时 MIXED/EXCEPTION_BIASED 配置可能误覆写 non-leaf target。

源码位置：`tlb_map_builder::can_apply_reused_final_ppn()`。

```systemverilog
                       entry.s2_resolved_ppn_valid &&
                       (entry.s2_pte_r || entry.s2_pte_w || entry.s2_pte_x) &&
                       !|ppn[43:38];
```

中文伪代码：S2 target 先满足可解析和 wire 宽度条件，再要求 PTE 至少有一个读、写或执行权限；全零权限表示尚未走完 page walk，函数返回不可复用，entry 保留 builder 原 PPN。

处理结论：保持当前实现，并已在 plan `IMPLEMENTATION_DELTA`、source analysis 与 S2/allStage smoke 中明确记录。

### 10.2 Plan 未说明但 Coding 落实的细节

#### 10.2.1 专项 cfg 的真实消费断言

细节功能：software smoke 显式调用 `configure_from_plus()` 并检查 getter 与 responder snapshot。

为什么 plan 未覆盖：plan 已定义参数链，但未要求 harness 必须对 cfg 消费本身建立断言。

在本特性中的作用：避免测试仅用手写 `reuse_en/reuse_wt/history_size` literal 而掩盖 plus 链断裂。

源码位置：`soft_test_l2tlb_ppn_reuse_sequence::check_smoke_cfg_snapshot()`。

```systemverilog
    expect_true(responder.ppn_reuse_en == 1'b1 &&
                responder.ppn_reuse_history_size == 1 &&
                responder.ppn_reuse_wt == 100,
                "PPN reuse smoke cfg must be frozen by responder.configure_from_plus");
```

中文伪代码：在开始定向 history 场景前，smoke 比对 responder 已冻结的三个字段与 preset 期望值；不匹配就立即报告配置链失败，不继续把错误的 policy 用于后续 API 调用。

是否需要回写 plan：已回写到 plan 的 `IMPLEMENTATION_DELTA`。

#### 10.2.2 非可见 response 的 completion guard

细节功能：专项 harness 对 `completion_visible=0` token 保持 FIFO 不变。

为什么 plan 未覆盖：原 plan 已提出语义，但初版 smoke 只有 guard 分支而未真正触发。

在本特性中的作用：证明 history 的唯一写入边界是实际 sample completion，不是 pending token 的存在。

源码位置：`soft_test_l2tlb_ppn_reuse_sequence::complete_frozen_response()`。

```systemverilog
    if (responder.driving_valid && responder.sampled_resp_valid) begin
        responder.complete_driving_response();
    end else begin
        expect_true(data.l2tlb_ppn_history_q.size() == history_size_before,
                    "non-visible response must not append PPN history");
    end
```

中文伪代码：只有 driving slot 存在且 monitor 表明 response 可见时才调用真实 completion helper；否则直接断言 FIFO 长度没有变化。该分支不释放或伪造 response，它只验证 harness 不把未完成 token 写为 history。

是否需要回写 plan：已回写到 plan 的 `IMPLEMENTATION_DELTA`。

## 11. 非本次修改的逻辑分析

### 11.1 git status 对比结论

本次 review 覆盖所有 PPN reuse 相关源码、cfg、规则、分析、plan 和本 implementation review。下列工作区内容不纳入本 feature 的功能正确性分析，也不会加入本次提交：

| 类别 | 文件/目录 | 判断 | 原因 |
|---|---|---|---|
| Verdi 会话配置 | `mem_ut/ver/ut/memblock/sim/verdiLog/.35885eda01.conf` | 非本次逻辑 | 用户已有 Verdi GUI/会话生成物，与 PPN reuse 无关。 |
| 仿真生成产物 | `mem_ut/ver/ut/memblock/sim/base_fun/` | 非源码 review | 既有/重建的 VCS 编译输出，不应提交。 |
| 仿真生成产物 | `mem_ut/ver/ut/memblock/sim/l2tlb_ppn_reuse_token0_20260908/` | 非源码 review | 本次干净编译和专项 smoke 的 `simv`、coverage、wave、log 输出，不应提交。 |

历史 annotated review `AI_DOC/plan/test_framework/review_doc/do/dispatch_plan_v2_review_annotated.md` 已按 L2TLB 规则检索，属于历史评审记录；本次不改写其原始结论，以本 review 和关联专项 plan 为准。

## 12. 最终结论

- PPN reuse 默认关闭；enable 后仅由真实 response completion 收集最近 `M` 次返回。
- exact hit、range hit 和 miss build 都可贡献 history；只有新 normal 4KB leaf miss entry 可按 `WT` 复用 valid PPN。
- token `0`、S2/allStage non-leaf、不可见 response、invalid record、S1 split/S2 encoding、FIFO 裁剪和 SFENCE 保留均有专项检查。
- VCS 编译、专项 smoke 和默认配置 smoke 均通过；未把不具备 dispatch topology 的 `tc_sanity` 挂起误报为通过。
- 无已知 blocker；剩余验证边界是未运行真实 DTLB request stream 下的 runtime-reset adapter 端到端场景，当前由源码审查和 software lifecycle closure 覆盖。
