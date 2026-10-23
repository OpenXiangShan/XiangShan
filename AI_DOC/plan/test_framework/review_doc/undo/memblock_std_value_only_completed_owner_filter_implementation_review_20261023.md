# MemBlock STD value-only 已完成 owner 候选过滤实现 Review

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象 | 使用场景/示例 |
|---|---|---|---|
| `value-only` | STD raw 只有 ROB value，没有 ROB wrap flag | `dispatch_raw_int_wb_t` | value=12 需要尝试两个完整 ROB key |
| `active owner` | 仍存在于 active ROB map 的 UID | `uid_by_active_rob` | ROB commit 后等待 SQ deq 时仍可能 active |
| `completed owner` | 已经消费 STD real-WB 或已进入 pass/fault 的 owner | `std_writeback/std_pass/std_fault` | UID492 已完成但尚未释放 SQ |
| `fault barrier` | STA/STD 两个真实写回都到达前的 store fault 中间阶段 | `pending_store_fault` | `std_writeback=1` 但 `std_fault=0` |

抽象功能描述：`probe_std_candidate()` 为 value-only STD raw 的单个 ROB flag 提供只读候选过滤，
确保已完成 owner 不会再次消费 raw，同时把未完成候选交给既有 snapshot 校验。它不拥有 retire、SQ
deq、redirect 或 RM 比较职责。

## 2. Review 范围与关联计划

关联 plan：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/AI_DOC/plan/test_framework/plan/do/memblock_std_value_only_completed_owner_filter_plan_20261023.md
```

本轮源码修改：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
```

逻辑范围只有 `probe_std_candidate()`：active map 命中后读取 status，若
`std_writeback || std_pass || std_fault` 任一置位则返回不命中，否则保持既有
`fill_current_issue_snapshot()` 校验。

## 3. 修改前后逻辑

修改前只要 owner active、STD dispatched、epoch/issue/SQ 校验通过，就把 owner 计入候选；已真实
STD 写回但尚未 SQ deq 的旧 owner因此仍可与新 ROB wrap owner同时命中。

修改后先检查 target consumption 状态：

```text
active map 无 owner -> 返回不命中
std_writeback/std_pass/std_fault 任一为 1 -> 返回不命中
否则 -> 调用 fill_current_issue_snapshot()，通过才返回命中
```

中文伪代码：该逻辑先用完整 ROB key 查询 active owner；查询不到时不修改任何状态并结束。查询到 owner
后读取其 status，真实 STD writeback 已记录、正常 pass 已记录或 fault 已记录任一成立，都说明该 owner
不能再次消费当前 raw，因此排除它。只有未消费 owner才继续进入既有 snapshot helper，由 helper验证 active、
dispatch、采样 epoch、issue epoch 和 SQ identity；helper失败则该 flag不命中，成功才返回 candidate。

## 4. 源码实现检查

抽象功能描述：`probe_std_candidate()` 在 `resolve_std_uid_by_rob_value_only()` 的两个 flag probe
中被调用，负责将 raw 的不完整 ROB identity约束为一个仍未完成的 current STD owner；它只读 status 和
active map，不执行状态推进。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，函数
`probe_std_candidate()`。

```systemverilog
        if (!data.lookup_active_uid_by_rob(rob_key, uid)) begin
            return 1'b0;
        end
        begin
            status_transaction status;

            status = data.get_status(uid);
            if (status.std_writeback || status.std_pass || status.std_fault) begin
                return 1'b0;
            end
        end
        return fill_current_issue_snapshot(candidate, uid, rob_key,
                                           sample_flush_epoch_valid,
                                           sample_flush_epoch,
                                           1'b0);
```

中文伪代码：`lookup_active_uid_by_rob()` 将当前 flag/value 映射到 active UID；无映射立即返回不命中。
有映射时读取该 UID status；三种 target-consumption 标志任一置位就返回不命中，覆盖正常完成和 fault
barrier 中间态。未完成 owner继续调用 `fill_current_issue_snapshot()`，该 helper负责原有生命周期、
dispatch、epoch、issue epoch和SQ owner校验，返回值直接决定该 flag是否进入上层零/唯一/双候选判断。

## 5. 正确性与边界检查

- 原始失败 `seed=1545815340`：UID492 在 `18168.200ns` 已 STD real-WB，`19195.700ns` ROB commit
  时仍因 SQ mapping 保留 active；新 UID652 在 `19315.700ns` fire，旧 owner被过滤后不再产生双候选。
- 历史同簇 seed `1396766687` 在有效 issue-enable配置下通过。
- `std_writeback` 先于 `std_fault` 的 fault barrier 已被显式纳入过滤，不依赖 `target_entry_done()` 当前
  仅判断 pass/fault 的语义。
- 两个 owner 都未完成且 snapshot 合法时，`resolve_std_uid_by_rob_value_only()` 仍保持双候选 fatal。
- 零候选仍保持 fatal，redirect covered、fault-owned、tombstone stale raw 的已有前置分支顺序不变。
- 不删除 active ROB/SQ map，不推进 SQ deq，不改变 DUT issue/response，固定两个 map 查询，运行复杂度仍为 O(1)。

## 6. Plan 对齐检查

### 实现与 Plan 不一致项

无。实现严格采用修订后的 `std_writeback || std_pass || std_fault` 候选过滤和既有 snapshot helper。

### Plan 未说明但 Coding 落实的细节

仅增加了源码内中文注释，说明 `std_writeback` 可能早于 `std_fault` 的 fault barrier 原因；该注释不改变
运行时行为。

## 7. 专项仿真结果

所有仿真均在每次 EDA 前加载 `.envrc`，并使用
`mem_ut/ver/ut/memblock/scripts/run_eda_regression.sh`，源码修改后重新编译。

| 批次 | seed | 结果 |
|---|---|---|
| 原始/历史专项 | `1545815340,1396766687` | `2/2 PASS` |
| 新随机专项 | `1967109724,1006518208,1868321970,1531581512,2139080182,1365826704,1689799034,1565004615,1681530011,1661553988` | `10/10 PASS` |

结果目录：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/mem_ut/ver/ut/memblock/sim/std_owner_filter_fix_20261023_original2
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/mem_ut/ver/ut/memblock/sim/std_owner_filter_fix_20261023_random10
```

两批次 `summary.log` 均为 `result=PASS`，`remote_unknown_jobs=0`，`UVM_ERROR=0`、`UVM_FATAL=0`；原始
seed 日志包含 `TEST CASE PASSED`。

### Fault barrier 专项边界

使用 `tc_dispatch_fault_smoke`、seed `20261023` 重新编译运行。日志确认：

```text
store fault helper semantics passed sta_epoch=1 std_epoch=2
```

该检查覆盖 `std_writeback=1`、`std_pass=0`、`std_fault=0` 的 fault barrier 中间态契约。
同一 smoke 随后在既有 STA tombstone 同 epoch 去重检查处触发：

```text
same-epoch duplicate failed feedback re-enqueued replay history=1 events=1 seq=1
```

该失败没有出现 `probe_std_candidate` 或 `INT_WB_STD_KEY`，说明没有进入本次 value-only STD adapter
路径；它是另一个测试框架问题，不能作为本修复动态 adapter PASS/FAIL 的证据。因此本修复对 fault
barrier 的验证边界为：源码状态契约检查加 helper 专项通过，未声称该 smoke 动态覆盖 adapter raw。

## 8. Review 结论

代码范围、状态所有权、零/双候选 fail-fast、fault barrier 和专项结果均与修订 plan 对齐，未发现已知
blocker。该问题可以归档并独立提交。

**FINAL PASS**
