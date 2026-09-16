# Fault head redirect recovery implementation review（2026-09-16）

| 项目 | 内容 |
| --- | --- |
| 关联 plan | `AI_DOC/plan/test_framework/plan/do/memblock_fault_head_redirect_recovery_plan_20260915.md` |
| 版本 | V2，`mem_ut_uvm_v2`，基线 `156762832` |
| 结论 | 通过：fault head 已在 real dispatch terminal 后驱动 `level=1` redirect；原始失败 seed 通过。 |
| 代码范围 | `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv` |
| 文档范围 | `fault_exception_flow.md`、`rob_commit_lq_sq_deq_flow.md` |
| 非目标 | RTL/Scala、DUT interface、RM、L2TLB、plusarg/cfg、generic redirect owner、LQ/SQ direct cleanup。 |

## 术语与抽象功能说明

| 术语 | 当前含义 | 代码落点 | 本次验证示例 |
| --- | --- | --- | --- |
| `fault token` | fault head 已发送 commit 语义后等待 terminal recovery 的私有状态。 | `fault_head_waiting` 及关联字段。 | UID5 等待 SQ deq。 |
| `stable ROB anchor` | token 建立时保存的完整 ROB key，不受 cursor 后续移动影响。 | `fault_head_rob_key`。 | UID5 固定为 `0/5`。 |
| `covered redirect` | 已有 active redirect 已覆盖 terminal fault anchor 的事实。 | `fault_head_redirect_covered`。 | 防止在旧 owner 完成后重复申请。 |
| `level=1 redirect` | V2 scalar exception 驱动给 DUT 的 flush 类 redirect。 | `request_fault_head_redirect()` payload。 | `level=1`、`flush_itself=1`。 |
| `terminal skip` | active-window flush 不重新建立已经 terminal 的 fault UID。 | `apply_redirect_flush_range()` 既有逻辑。 | UID5 不 reissue。 |
| `re-admission` | redirect 后重新构造 younger UID 的新 dynamic instance。 | `prepare_uid_for_redirect_reissue()`。 | UID6 起恢复。 |
| `cancel record` | redirect owner 保存 drive、monitor anchor 和软件 cancel 事实的唯一记录。 | `common_data_transaction::cancel_record_q`。 | 本改动只复用，不直接修改。 |

抽象功能描述：本次实现只在 real dispatch topology 中补全 fault terminal 到 redirect owner 的调度连接。fault 的 writeback、RM compare、commit/deq 和 `success=0` terminal 不变；新逻辑只在 terminal 后创建一次 `level=1` payload，随后由既有 FIFO、driver、monitor anchor 和 active-window flush 完成年轻 UID recovery。

## 修改前问题

UID5 于 `747.7ns` 产生 STA fault、于 `1747.7ns` 完成 `success=0` terminal，但旧 `sync_modeled_head_after_fault_terminal()` 只推进 cursor，未产生 `io_redirect_valid`。UID6 及之后的旧 LSQ/issue 状态继续留在 DUT；UID11 保留旧 unaligned store 的 data-invalid blocker，UID15 持续 replay，原始诊断最终无进展。

本次不把 `level=0` / `flushAfter` 作为 workaround。V2 scalar exception 对 DUT 的输入要求是 `level=1`；软件侧的 fault 不重发由 `terminal_done` scan skip 保证，而不是通过把 fault anchor 排除在 flush 范围外。

## 代码复核

### `fault_head_rob_key`、`fault_head_redirect_covered` 与 token cleanup

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`

抽象功能描述：新增字段保存 terminal 后仍可靠的 DUT anchor，以及已有 redirect 已覆盖该 anchor 的结果。`clear_fault_head_token()` 只清 handler 私有字段，不干预公共 status、LQ/SQ map 或 fault sideband latch。

```systemverilog
memblock_rob_key_t fault_head_rob_key;
bit                fault_head_redirect_covered;

function void clear_fault_head_token();
    fault_head_waiting = 1'b0;
    fault_head_uid = 0;
    fault_head_dynamic_epoch = 0;
    fault_head_rob_key = '{default:'0};
    fault_head_redirect_covered = 1'b0;
endfunction
```

文字伪代码：

```text
建 token 时保存 UID、dynamic epoch 与完整 ROB key；该 key 不由后续 cursor 推导。
token 被旧 redirect 杀掉、software-only terminal 完成、已有覆盖 redirect 完成，或本 fault redirect 成功申请后，统一调用 cleanup 清私有字段。
cleanup 不清 latched_is_store_exception，保持已有 level sideband 生命周期。
```

复核结论：所有新增字段均有 constructor/reset、写入、读取和清理路径；没有重复维护公共 LQ/SQ 所有权。

### `has_pending_monitor_redirect()` 与 `request_fault_head_redirect()`

抽象功能描述：前者仅在 fault terminal 等待阶段检查 recovery queue 是否已有未仲裁的真实 redirect；后者在无冲突时把 stable anchor 转为既有 redirect owner 的 payload。两者都不直接驱动 DUT interface。

```systemverilog
if (data.active_redirect.valid || data.has_pending_redirect_drive() ||
    data.issue_blocked_by_global_flush() || has_pending_monitor_redirect()) begin
    return 1'b0;
end

redirect.valid = 1'b1;
redirect.rob_key = fault_head_rob_key;
redirect.flush_itself = 1'b1;
redirect.level = 1'b1;
redirect.is_vls_exception = 1'b0;
data.request_redirect_flush(redirect);
data.push_redirect_drive(redirect);
```

文字伪代码：

```text
先确认 real topology 且 MEMBLOCK_REDIRECT_SEQ_EN=1；否则对 real fault fail-fast。
若 recovery queue、active redirect、drive FIFO 或 global flush 已有 owner，则返回失败，fault token/cursor 保持不变。
无冲突时构造 level=1、flush_itself=1、is_vls_exception=0 的 payload；
request_redirect_flush 创建 cancel record、冻结 issue 并登记 active redirect；
push_redirect_drive 将 payload 放入已有 responder FIFO，driver 后续才实际写 DUT 输入。
```

复核结论：queue scan 只在 fault token terminal 等待分支执行，不进入 normal commit/admission 高频路径；没有新增 main-table 全表扫描。

### `mark_fault_rob_commit_uid()` 与 `sync_modeled_head_after_fault_terminal()`

抽象功能描述：mark 函数仍在 lsqcommit 已发送后建立 token；sync 函数在 terminal 后按 topology/owner 状态选择旧兼容 rebase、defer 或一次 fault redirect。它不直接回收 active map，继续复用 common-data owner。

```systemverilog
fault_head_rob_key = status.get_rob_key();
fault_head_redirect_covered = 1'b0;
data.try_retire_committed_uid(uid);

if (!memblock_sync_pkg::dispatch_real_smoke_active) begin
    finish_fault_head_terminal();
    return 1'b1;
end
if (has_pending_monitor_redirect()) return 1'b0;
if (data.active_redirect.valid) begin
    if (rob_order_util::rob_need_flush(fault_head_rob_key, data.active_redirect))
        fault_head_redirect_covered = 1'b1;
    return 1'b0;
end
if (fault_head_redirect_covered || request_fault_head_redirect())
    finish_fault_head_terminal();
```

文字伪代码：

```text
fault commit 已发送时保存 stable anchor，再等待真实 LQ/SQ deq 使 consume_fault_retire 产生 non-success terminal。
software-only topology 没有 redirect driver/monitor，因此保持原有 cursor=fault_uid+1 rebase。
real topology 先让 pending monitor event 和 active redirect 完成；active redirect 覆盖 anchor 时只记录 covered，绝不并发建第二个 cancel record。
覆盖 owner 完成后直接收口 fault token；否则申请本 fault 的 level=1 redirect，且仅在申请成功后移动 cursor。
finish_fault_head_terminal 清 token 后 rebase 到 UID6；active redirect 的已有全局 freeze 在 apply 完成前阻止 admission/issue。
```

复核结论：fault UID 在 terminal 后不 reissue；`apply_redirect_flush_range()` 对其 `terminal_done` status 直接跳过。UID6 及以后仍由 `prepare_uid_for_redirect_reissue()` 和 `rollback_max_enqueued_uid()` 处理，未引入 direct deq 或 map delete。

## Plan 对齐检查

执行前 plan：`AI_DOC/plan/test_framework/plan/do/memblock_fault_head_redirect_recovery_plan_20260915.md`。

| Plan 项 | 实现检查 | 结果 |
| --- | --- | --- |
| 保存 stable fault ROB anchor | `fault_head_rob_key` 在 mark 时赋值，在 cleanup/reset 清除。 | 通过 |
| 防止 overlapping redirect owner 竞争 | pending queue、active redirect、pending drive 与 global flush 均阻塞新申请；覆盖状态单独保存。 | 通过 |
| DUT payload 为 `level=1` | request helper 固定 `level=1`、`flush_itself=1`、`is_vls_exception=0`。 | 通过 |
| fault UID 不 reissue | fault 已 terminal 后才申请；既有 scan skip terminal UID。 | 通过 |
| UID6 起重新 admission | terminal 后 cursor 置 `fault_uid+1`，既有 flush rollback 处理 younger UID。 | 通过 |
| software-only 保持旧行为 | topology gate 为 0 时直接 finish/rebase。 | 通过 |
| 不改 common-data redirect owner | `request_redirect_flush()`、`push_redirect_drive()`、`apply_redirect_flush_range()` 均复用。 | 通过 |

## 与 Plan 不一致的实现

未发现。执行前 plan 已明确 `fault_head_redirect_covered`、pending monitor redirect 仲裁和 `level=1` payload；源码按该定义落实。

## Plan 未说明但 Coding 额外落实的细节

未发现需要新增的实现细节。`clear_fault_head_token()` 和 `finish_fault_head_terminal()` 仅将 plan 中要求的统一清理/推进顺序收敛为局部 helper，不改变 plan 定义的状态语义或外部行为。

## 文档同步复核

- `AI_DOC/mem_ut_flow_doc/fault_exception_flow.md` 新增术语表、terminal redirect 分支和 end-to-end terminal skip/re-admission 链路。
- `AI_DOC/mem_ut_flow_doc/rob_commit_lq_sq_deq_flow.md` 新增 stable anchor、covered redirect、`level=1` 和 UID6 起恢复语义。
- 两份 flow 均继续说明 `handle_fault_event()` 不发 redirect，fault 必须先到 modeled ROB head 并完成 terminal。

## 验证记录

### 静态检查

```text
git diff --check -- lsq_commit_handler.sv fault_exception_flow.md rob_commit_lq_sq_deq_flow.md
结果：通过。

关键检索：新增字段/函数均有 reset、设置、读取与清理；fault helper 中不存在 level=0 或 flush_itself=0 payload。
```

### 编译

```text
mode=fault_head_redirect_20260916_r1
MEMBLOCK_XS_HOME=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/test-rtl
```

编译日志：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_head_redirect_20260916_r1/log/vcs_compile_rtl.log`

结果：VCS/KDB 成功，`0 error(s), 0 warning(s)`；日志解析的 RTL 路径为 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/test-rtl/build/rtl`。

### 基础回归

```text
tc=basicTest
ts=virtual_base_sequence
cfg=default
seed=666666
mode=fault_head_redirect_20260916_r1
```

日志：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_head_redirect_20260916_r1/log/tc=basicTest_ts=virtual_base_sequence_cfg=default_seed=666666_rtl_base_smoke.log`

结果：`TEST_PASS`，`UVM_ERROR=0`，`UVM_FATAL=0`。

### 原始复现 seed

```text
tc=basicTest
ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq
cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k
seed=856436350
+MEMBLOCK_MAIN_TRANS_NUM=100
+MEMBLOCK_HARD_XZ_CHECK_EN=0
+MEMBLOCK_CHECK_TRIGGER_EN=0
mode=fault_head_redirect_20260916_r1
```

日志：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_head_redirect_20260916_r1/log/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=856436350_rtl_fault_seed856436350.log`

关键证据：

```text
747.7ns   UID5 STA fault feedback, exception_vec=0x80
1716.2ns  UID5 RM commit compare PASS
1747.7ns  UID5 fault retire, success=0, terminal_done=1
1747.7ns  request scalar fault redirect uid=5 rob=0/5 level=1 flush_itself=1
1752.7ns  monitor batch drops UID6 event because active redirect covers ROB0/6
1767.7ns  redirect_flush retires old UID6/9/11/15 and other younger instances
1867.7ns  UID6 reaches success=1 terminal_done=1 after recovery
6592.7ns  TEST_PASS, UVM_ERROR=0, UVM_FATAL=0
```

此 run 中 UID5 的 request 日志只有一条；没有 UID5 re-admission/issue 记录。UID11 的旧 active instance 在 redirect flush 时被清理，测试不再停在 `terminal_done_uid=11`；UID15 的 replay 可继续合法出现，但 tombstone 保持 coalesce，未触发 overflow。

### Software-only 回归

```text
tc=tc_dispatch_fault_smoke
ts=virtual_base_sequence
cfg=default
seed=666666
mode=fault_head_redirect_20260916_r1
```

日志：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_head_redirect_20260916_r1/log/tc=tc_dispatch_fault_smoke_ts=virtual_base_sequence_cfg=default_seed=666666_rtl_software_fault.log`

结果：`TEST CASE PASSED`，`UVM_ERROR=0`，`UVM_FATAL=0`。fault load/store smoke 保持 software-only terminal/rebase，未申请 redirect。

### Pending-MMIO software-only 回归

```text
tc=basicTest
ts=memblock_pending_mmio_directed_vseq
cfg=default
seed=666666
mode=fault_head_redirect_20260916_r1
```

日志：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_head_redirect_20260916_r1/log/tc=basicTest_ts=memblock_pending_mmio_directed_vseq_cfg=default_seed=666666_rtl_pending_mmio.log`

结果：`TEST_PASS`，报告服务器 `UVM_ERROR=0`、`UVM_FATAL=0`。该 sequence 的一个预期 MMIO ownership fatal 被 testcase catcher 捕获，属于 directed assertion，不是本次实现失败。

## 验证缺口与风险

- 未新增独立的“已有 redirect 覆盖/不覆盖 fault anchor”定向 testcase；源码已实现 `fault_head_redirect_covered` 和 defer 路径，原始随机 seed 覆盖了 own redirect 主路径。
- `make eda_run` 会先隐式重新编译；一次重复编译在 VCS/Verdi KDB 内部发生 `SIGABRT`，无 SystemVerilog `Error-*` 或语法诊断。使用新 isolation mode 重新 `eda_compile` 后，`eda_batch_run` 的基础和目标回归均通过。该工具稳定性问题不影响本次通过结果，但后续应避免对同一 mode 立刻重复使用 `eda_run`。
- 本地 FSDB reader 因 Verdi runtime 缺失 `libpng12.so.0` 无法独立解码波形；日志仍通过 active redirect 的 monitor drop 与 `redirect_flush` recovery 证明 drive/anchor/apply 链路已实际完成。

## 非本次修改的逻辑分析

### git status 对比结论

本次 review 覆盖：

- `lsq_commit_handler.sv`
- fault/ROB flow 文档
- 本 plan 和本 implementation review

工作区中已有的 `common_data_transaction.sv` tombstone coalescing 改动不属于本 plan 本轮新增逻辑，未修改、未回滚；本次 target run 使用了它，因此 `STA_LATE_TOMBSTONE` 连续 replay 保持 history=1 并未 overflow。RTL/Scala 以及生成 RTL 均未改动。

## 最终结论

本实现满足 plan 的核心行为：fault 先 terminal、real dispatch 后产生一次 `level=1` redirect、fault UID terminal skip、UID6 及以后恢复。原始阻塞 seed 与两条 software-only 回归均通过，未发现 RTL 问题或本实现 blocker。
