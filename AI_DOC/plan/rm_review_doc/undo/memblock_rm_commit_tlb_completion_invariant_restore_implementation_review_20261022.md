# memblock RM 提交边界 TLB 完成不变量恢复 implementation review（2026-10-22）

## 1. Review 范围与术语说明

本文复查 plan `AI_DOC/plan/rm_plan/do/memblock_rm_commit_tlb_completion_invariant_restore_plan_20261022.md` 的 coding 结果，范围包括 RM 的 pending 删除、非冻结 translation lookup、相关 directed test 调整，以及远端真实 seed 验证。

### 1.1 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
| --- | --- | --- | --- |
| `RM` | Reference Model，依据测试框架记录构造期望并与 DUT 结果比较 | `memblock_rm.sv` | 在 UID commit 后构造 Load/Store 期望 |
| `UID` | 主表事务的唯一编号 | `memblock_uid_t`、`uid_tlb_record_by_uid` | RM 按 UID 顺序检查 commit |
| `ROB commit` | UID 已满足框架提交条件，允许 RM 进行最终检查 | `status_view.rob_commit` | commit 成立后翻译缺失必须暴露 |
| `WAITING` | UID 的 translation record 已建立，但尚未完成 response 回填 | `MEMBLOCK_UID_TLB_WAITING` | 本轮不再把它转换成 RM retry |
| `completed response` | 真实 L2TLB response 已采样并回填 UID record 的事实 | `record_completed_tlb_response()` | completed UID 可使用 response payload |
| `issue-hit snapshot` | UID issue 时保存的可见命中记录，只作为 provenance/debug 信息 | `issue_hit_snapshot_valid`、`issue_hit_payload` | 不能在当前 live lookup miss 后作为 RM fallback |
| `非冻结查询` | RM 查询时读取当前合法 live entry；不使用 issue 时的历史快照覆盖当前状态 | `resolve_tlb_entry_key_for_rm()` | entry 被删除后应 query miss |
| `query miss` | 没有唯一可用 translation entry 的只读查询失败 | `report_query_miss()` | 由 directed test catcher 吸收预期错误 |
| `跨页访问` | 一个 UID 的字节范围跨越两个 VPN，需要分别解析两个 page | `observer_build_commit_item()` 的 byte loop | 第二页不能复用第一页快照 |

### 1.2 总体结论

实现恢复了 commit 边界强检查：`rob_commit` 已成立后，如果 UID translation 尚未完成或当前 live entry 不存在，RM 当次直接失败，不再依据 `get_l2tlb_idle_stop_cycle()`等待后续 poll。已完成的真实 response payload 仍可用于正常 RM compare；被删除的 issue-time snapshot 不再把当前缺失伪装成成功。

## 2. `memblock_rm.sv`：删除 pending 状态机

### 2.1 修改前后行为

修改前，RM 在发现 `WAITING + request fire + !pte_valid` 时设置本地 pending 状态，下一轮 poll 重试，并把 responder 的 idle-stop 参数当作 RM timeout。该等待没有统一的时间基准，也会延迟暴露 response 丢失、UID completion 未回填或 commit barrier 提前放行等问题。

修改后，RM 只等待 `rob_commit` 等正常 status；一旦进入 commit 检查，translation 解析失败就沿用现有 `RM_LS_COMPARE` 错误路径。删除的字段、helper 和 `translation_pending` 输出没有兼容残留。

### 2.2 `observer_build_commit_item()`

抽象功能描述：该函数在 RM commit 检查阶段，根据 main/status view、UID context 和当前 translation 结果构造一个 RM item；构造失败只表示当前 UID 不能进入 compare，不负责建立 L2TLB response 等待状态。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_build_commit_item()`。

```systemverilog
if (!ro.resolve_tlb_entry_for_rm(item.uid, item.computed_vaddr[63:12],
                                 tlb_context, entry)) begin
    ls_model.set_error(RM_LS_ERR_TLB_ENTRY_NOT_READY,
                       $sformatf("uid=%0d translation entry is not ready", item.uid));
    return 1'b0;
end
```

中文伪代码：本段在当前 UID 的首 VPN解析阶段执行只读查询；查询失败时把 RM 错误分类为 translation entry 不可用，立即返回失败；不检查或设置 pending 标志，不调用 retry/timeout helper。`resolve_tlb_entry_for_rm()`负责查询和 query-miss 诊断，`set_error()`只更新当前 RM item 构造错误，返回值交给上层 commit 流程处理。

源码位置：同上，函数：`observer_build_commit_item()` 的跨页 byte loop。

```systemverilog
for (int unsigned byte_index = 0; byte_index < item.size_bytes; byte_index++) begin
    byte_va = item.computed_vaddr + byte_index;
    if (!ro.resolve_tlb_entry_for_rm(item.uid, byte_va[63:12],
                                     tlb_context, entry)) begin
        ls_model.set_error(RM_LS_ERR_TLB_ENTRY_NOT_READY,
                           $sformatf("uid=%0d byte=%0d translation entry is not ready",
                                     item.uid, byte_index));
        return 1'b0;
    end
end
```

中文伪代码：对访问范围内的每个 byte 计算实际 VPN并独立查询；任意一个 VPN没有合法 entry 就立即设置错误并退出，后续 byte 不再继续。这样首 VPN和跨页 VPN各自使用自己的 live lookup，不能用一份 UID issue snapshot代表整笔访问。

### 2.3 `observer_process_current_commit()`

抽象功能描述：该函数是 RM commit cursor 的单次处理入口；在 `rob_commit` 已发布后构造 item、加入 RM model 并采集实际结果。它不负责调度 L2TLB response，也不负责等待 translation pending。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，函数：`observer_process_current_commit()`。

```systemverilog
if (!observer_build_commit_item(main_view, status_view, item)) begin
    if (!observer_mismatch_reported) begin
        `uvm_error("RM_LS_COMPARE", ls_model.last_error_text)
        observer_mismatch_reported = 1'b1;
    end
    return 1'b0;
end
```

中文伪代码：先调用 item 构造函数；如果首 VPN或跨页 VPN解析失败，只在尚未报告过时输出一次 `RM_LS_COMPARE`，设置 mismatch 保护位并立即返回失败；不记录 pending UID、不增加 RM poll 计数、不等待 idle-stop cycle。只有 item 成功构造后，主流程才继续 `add_item()`、实际值采集和 compare。

### 2.4 删除对象与状态真源

| 删除对象 | 原行为 | 删除原因 | 修改后真源 |
| --- | --- | --- | --- |
| `observer_translation_pending_active` | 保存 RM 是否正在等待某 UID 的翻译 | 把 RM 变成第二个 translation completion owner | UID record、completed response 和 live lookup |
| `observer_translation_pending_uid` | 记录等待中的 UID | 只能覆盖单个 UID，不能表达跨页和多 request 生命周期 | 当前 commit UID 的只读查询结果 |
| `observer_translation_pending_poll_count` | 以 RM `#1ns` poll 次数模拟 L2TLB timeout | 与 L2TLB idle cycle 不是同一时间基准 | commit barrier 与 response completion 的真实状态 |
| `observer_note_translation_pending()` | 暂缓错误并重试 | 会隐藏 commit 顺序或 response 回填问题 | `observer_build_commit_item()` 失败即报告 |
| `observer_clear_translation_pending()` | 成功后清理本地 pending | 删除本地 pending 状态后不再需要 | 无对应兼容状态 |

## 3. `memblock_rm_readonly_api.sv`：恢复非冻结 lookup

### 3.1 修改前后行为

修改前，`resolve_uid_issue_hit_entry_for_rm()`会重新校验 UID 保存的 issue-time snapshot，并在 WAITING 首 VPN查询时把该快照作为成功结果。这样即使 live entry 已被删除或替换，RM 仍可能使用历史 entry继续 compare。

修改后，`COMPLETED + pte_valid`仍使用真实 response completion 回填的 UID payload；WAITING 首 VPN和所有跨页 VPN都通过当前 `resolve_tlb_entry_key_for_rm()`和 `read_tlb_entry_for_rm()`查询 live 表。issue snapshot 保留在 record 中用于 provenance/diagnostic，但不再是 RM 成功 fallback。

### 3.2 `resolve_tlb_entry_for_rm()`

抽象功能描述：该函数为 RM 解析一个请求 VPN的 translation，并返回 entry/context；它区分真实 completed response、当前 live lookup 和缺失/取消/context mismatch 等失败，不修改公共 TLB owner 状态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv`，函数：`resolve_tlb_entry_for_rm()`。

```systemverilog
else if (record.record_valid && !record.pte_valid &&
         record.uid_tlb_wait_state ==
             memblock_uid_tlb_record::MEMBLOCK_UID_TLB_WAITING) begin
    if (!resolve_tlb_entry_key_for_rm(uid, request_vpn, context_view) ||
        !read_tlb_entry_for_rm(context_view.entry_key, entry_view)) begin
        return 1'b0;
    end
end
else begin
    return report_query_miss(
        "uid_tlb_entry",
        $sformatf("uid=%0d request VPN=0x%0h has no completed/current payload",
                  uid, request_vpn));
end
```

中文伪代码：如果 UID record 是 WAITING，无论 request 是否已经 fire，都根据本次 request VPN重新生成当前 lookup key，再读取当前 live entry；任一步失败就返回 query miss，不从 issue snapshot恢复。对于其它既没有 completed payload、也不是合法 WAITING 的状态，直接报告没有 completed/current payload。`resolve_tlb_entry_key_for_rm()`负责候选 key/context 解析，`read_tlb_entry_for_rm()`负责读取当前 entry，两者都不改变表项生命周期。

### 3.3 跨页和 issue snapshot 风险检查

非对齐 UID最多涉及两个 VPN。当前实现由 RM byte loop逐 byte传入实际 VPN，因此第一页和第二页分别查询；不存在“保存第一份还是第二份快照”的 UID 级二选一语义。若第二页 entry缺失，当前查询返回 miss，即使第一页存在 completed payload 或 issue snapshot，也不会替代第二页。

本轮 directed provenance test还验证了两点：live entry被替换时 RM使用替换后的 generation；live entry被删除后 RM产生 query miss，而不是回退到 issue snapshot。这正是方案 A相较冻结快照的预期功能。

## 4. Directed test 修改复查

### 4.1 `soft_test_tc_rm_pbmt_effective_fault.sv`

抽象功能描述：该 testcase 直接调用生产 RM item 构造路径，覆盖 commit 边界 fired WAITING、跨页缺失、completed response、PBMT/PMA/PMP 和普通 query miss。它使用 report catcher 只吸收预期的 readonly query miss，不改变生产 RM 的错误语义。

源码位置：`mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_rm_pbmt_effective_fault.sv`，task：`check_rm_production_commit_path()`。

```systemverilog
uvm_report_cb::add(null, catcher);
if (env.rm.observer_build_commit_item(main_view, status_view, item)) begin
    uvm_report_cb::delete(null, catcher);
    `uvm_fatal(get_type_name(), "fired incomplete translation unexpectedly succeeded")
end
uvm_report_cb::delete(null, catcher);
if (catcher.caught_count != 1) begin
    `uvm_fatal(get_type_name(),
               "fired incomplete translation did not produce one readonly query miss")
end
```

中文伪代码：先安装只匹配 `RM_READONLY_API` query miss 的 catcher；调用生产 item 构造，若 fired WAITING竟然成功则 testcase fatal；调用结束后移除 catcher，并要求恰好捕获一次预期 miss。跨页负向场景采用同样结构，但首 VPN先使用 completed payload，随后在第二 VPN查询时确认立即失败；completed response场景则确认仍能正常构造 item。

### 4.2 `soft_test_rm_provenance_followup_sequence.sv`

抽象功能描述：该 sequence验证 issue provenance的生命周期和 RM 非冻结语义，包括 live replacement、live deletion、重新发布 completed response 后的 reissue capture，以及 overlap 场景的当前 live generation。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_rm_provenance_followup_sequence.sv`，task：`check_l1_hit_issue_snapshot_lifecycle()`。

```systemverilog
expect_true(memblock_rm_readonly_api::get().resolve_tlb_entry_for_rm(
                TEST_UID, TEST_VPN, context_view, entry_view) &&
            entry_view.entry_generation == overwritten_live_entry.entry_generation &&
            entry_view.entry_generation != completed_generation,
            "RM did not use the current live entry while issue provenance was present");
```

中文伪代码：在 issue provenance仍存在时先替换 live entry；调用 RM 查询并要求返回替换后的 generation，而不是旧 completed generation。随后删除 live entry，再通过预期错误 catcher要求查询失败，证明 issue snapshot不能恢复已删除 entry。由于删除公共 live owner会同步清理同 key completed cache，测试重新发布 sampled response后才继续检查 reissue capture，保持测试前置条件独立且不改变 RM 真源。

源码位置：同文件，task：`check_l1_hit_overlap_resolution()`。

```systemverilog
expect_true(memblock_rm_readonly_api::get().resolve_tlb_entry_for_rm(
                TEST_UID, TEST_VPN, context_view, entry_view),
            "RM current-live resolver failed with issue provenance present");
expect_true(entry_view.entry_generation == current_live_entry.entry_generation &&
            entry_view.entry_generation != exact_pf_entry.entry_generation &&
            !entry_view.fault_effective_s1_pf && context_view.request_translation_valid,
            "RM used frozen exact PF instead of the current live entry");
```

中文伪代码：先建立当前 live entry，再查询同一 UID/VPN；要求返回当前 generation、当前 fault 状态和当前 request-derived translation 标志。这样验证了 RM 不会因为旧 issue snapshot记录了 PF而继续使用旧 PF结果；删除当前 live entry后的第二次查询还必须产生一次 readonly query miss。

## 5. Plan 对齐检查

关联 plan：`AI_DOC/plan/rm_plan/do/memblock_rm_commit_tlb_completion_invariant_restore_plan_20261022.md`。

### 5.1 与原始 plan 一致的部分

- `memblock_rm.sv` 中 pending 字段、pending helper、pending output 和首/跨页 pending 分支均已删除。
- `observer_process_current_commit()`恢复为 item 构造失败即 `RM_LS_COMPARE`，不再按 L2TLB idle-stop cycle重试。
- `memblock_rm_readonly_api.sv`删除 issue-hit RM resolver 和 pending readiness helper。
- completed response payload仍被保留；WAITING首 VPN和跨页 VPN使用当前 live lookup。
- PBMT directed test已从 pending retry/timeout改为立即失败和跨页缺失负向检查。
- provenance directed test已从冻结快照断言改为当前 live generation、删除后 query miss和 overlap lifecycle检查。

### 5.2 `IMPLEMENTATION_DELTA` 已覆盖的补充

Plan 中的 `## 执行中补充/修正（IMPLEMENTATION_DELTA）`记录了四项 coding期间必要补充：两个 directed query-miss catcher、删除 live entry后重新发布 completed response、`request_translation_valid`按当前 live entry重新推导，以及 fired WAITING/跨页缺失分别捕获一次 readonly query miss。review确认这些补充没有改变方案 A的真源选择。

### 5.3 与原始 plan 不一致项

未发现会改变方案语义的不一致项。上述四项属于 plan 已明确标注的执行中补充，不能视为执行前 plan原文已覆盖，但均已在本 review中单独说明。

### 5.4 Plan 未说明但实现补充的细节

除 `IMPLEMENTATION_DELTA` 已列明内容外，未发现新的未记录架构行为。`RM_READONLY_API` catcher只用于测试吸收预期报告，不改变生产路径。

## 6. 静态检查与验证结果

执行了以下检查：

```text
git diff --check -- AI_DOC mem_ut/ver/ut/memblock
rg -n "observer_note_translation_pending|observer_clear_translation_pending|uid_tlb_translation_pending_for_rm|resolve_uid_issue_hit_entry_for_rm|translation_pending" mem_ut/ver/ut/memblock/env/src/memblock_rm mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv mem_ut/ver/ut/memblock/tc/src/soft_test mem_ut/ver/ut/memblock/seq/base_seq/soft_test
rg -n "observer_build_commit_item" mem_ut/ver/ut/memblock
```

旧 pending生产接口在源码调用范围内无残留；plan/历史说明中的文字记录不属于生产接口残留。

### 6.1 专项验证

1. `memblock_rm_provenance_followup_directed_vseq`，seed `37617171`：编译通过，仿真通过，`UVM_ERROR=0`，`UVM_FATAL=0`。
2. `tc_rm_pbmt_effective_fault/virtual_base_sequence`，seed `2026102011`：`TEST CASE PASSED`，`UVM_ERROR=0`，`UVM_FATAL=0`；3个预期 readonly error均被 catcher捕获。跨页负向 case为首 VPN completed、第二 VPN缺失。
3. `memblock_dispatch_real_smoke_vseq`，cfg `tc_l2tlb_fault_2pct_no_trigger`，seed `37617171`：正常结束，`RM_LS_COMPARE`完成10000条，`UVM_ERROR=0`，`UVM_FATAL=0`。
4. `tc_l2tlb_range_lookup_smoke/virtual_base_sequence`，seed `2026102011`：`TEST CASE PASSED`，`UVM_ERROR=0`，`UVM_FATAL=0`；2个 live-overlap expected fatal由 catcher捕获。

真实 seed日志：`mem_ut/ver/ut/memblock/sim/rm_nonfreeze_real_20261022/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl.log`。

## 7. 风险与最终结论

主要残余风险是：如果未来证明 DUT/验证框架协议确实允许 `rob_commit`先于 L2TLB response completion，则应修复 commit barrier、monitor 或 completion owner，并重新定义协议；当前 RM 不再用没有统一时间基准的 `#1ns` retry掩盖该问题。

本轮实现与验证无 blocker。方案 A已完成：RM恢复 commit 边界翻译完成不变量，采用非冻结 live lookup，保留真实 completed response payload，删除 pending retry/timeout和 issue snapshot RM fallback。Plan 已按项目规则从 `rm_plan/undo`归档到 `rm_plan/do`；review 文档按 review 工作流保留在 `rm_review_doc/undo`。
