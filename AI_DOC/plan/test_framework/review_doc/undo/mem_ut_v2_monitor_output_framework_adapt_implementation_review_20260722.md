# mem_ut V2 Monitor Output 适配 Implementation Review

| 项目 | 内容 |
|---|---|
| 关联 plan | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_execution_plan_20260708.md` |
| review 日期 | 2026-07-22 |
| review 范围 | monitor output 职责分类与 scalar-only vector writeback gate |
| 当前结论 | 最终独立 review 通过（`FINAL PASS`） |

## 1. 术语与抽象功能说明

### 术语表

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `monitor` | 在 DUT 时钟采样边界读取接口 output 的被动组件 | `io_mem_to_ooo_vec_wb_agent_agent_monitor::mon_data()` | 每个 `mon_cb` 采样 vector WB valid |
| `scalar-only flow` | 本轮只允许标量 load/store/writeback 的公共测试流程 | V2 dispatch flow 与 vector WB monitor gate | `writebackVldu` 只能为 0 |
| `fail-fast` | 发现当前 flow 明确不支持的有效接口行为后立即 `uvm_fatal` | `MEMBLOCK_VEC_WB_UNSUPPORTED` | valid 为 1 或 X/Z 时终止 |
| `analysis producer deferred` | RM analysis consumer 已连接，但当前 monitor 不调用 `mon_item_port.write()` 生产 analysis transaction | monitor output 分类表与 `mon_data()` | 修改前后都没有 analysis transaction |
| `owner` | 对一类运行期状态或事件生命周期拥有唯一写入、推进和清理职责的对象 | monitor output plan 的职责分类 | 本 monitor 只拥有 vector-WB unsupported gate，不拥有 scalar raw/event 或 LSQ sideband 状态 |
| `semantic raw/event` | 会进入 dispatch 语义批次并可能推进公共状态的 monitor 原始事实或归一化事件 | `memblock_sync_pkg` semantic raw queue、`dispatch_monitor_event_adapter` | 本 vector-WB monitor 修改前后都不生产此类 raw/event |
| `sideband` | 不属于 semantic raw batch 的独立时序采样事实 | LSQ 专项的 cancel snapshot/redirect anchor | 本 review 不拥有其状态写者 |

### 抽象功能描述

`io_mem_to_ooo_vec_wb_agent_agent_monitor::mon_data()` 是 vector writeback output 的被动采样入口，保留全部既有接口字段采样。原 `xz_sw` 分支为空，不承担有效检查；本次新增的 gate 是该 monitor 唯一针对 `writebackVldu` valid 的 `1/X/Z` 检查。该 task 修改前后都不生产 scalar semantic raw/event；修改后只在 reset 完成且观察到 unsupported valid 时新增 `uvm_fatal` 副作用，不修改公共 status、pass/fail 或 terminal。

## 2. 修改前后逻辑对比

### 修改前逻辑

- monitor 把 vector WB interface 的既有字段复制到局部采样变量。
- `xz_sw` 条件分支体为空，因此没有对 `writebackVldu` valid 执行任何有效的 `1/X/Z` 检查；DUT vector output 越界不会显式暴露。
- transaction 创建和 `mon_item_port.write()` 代码保持注释状态，因此该 monitor 原本不生产 scalar semantic raw/event 或 RM analysis transaction。

### 修改后逻辑

- monitor 在 `rst_n===1` 且 `reset_backend_done===1` 后检查两个 vector WB valid。
- valid 值只要不是确定的 `0`，包括 `1`、`X` 或 `Z`，都报告 `uvm_fatal`。
- 两个 valid 都为确定 `0` 时只继续下一拍采样，不创建 raw/event、不写 status，不影响 scalar 主流程。
- cancel snapshot、redirect anchor、ctrl raw 和 MMIO tag 不在本专项重复实现；它们由 LSQ/MMIO 相关 owner 管理。

## 3. `mon_data()` 的实现检查

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_vec_wb_agent_agent/src/io_mem_to_ooo_vec_wb_agent_agent_monitor.sv`，task：`io_mem_to_ooo_vec_wb_agent_agent_monitor::mon_data()`。

抽象功能描述：该 task 在每个 monitor clocking sample 后读取 vector writeback interface，并在公共 scalar flow 已完成 reset 初始化时执行 unsupported gate。它只拥有既有字段采样和新增失败报告，不拥有 scalar semantic raw/event、analysis transaction 或公共状态更新。

函数功能简析：全部 interface 到局部变量的采样赋值保持不变。以下源码证明原 `xz_sw` 分支仍为空，紧随其后的新增 gate 才是唯一针对两个 `writebackVldu` valid 的有效检查。

修改后源码：

```systemverilog
if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin

end
if(this.vif.rst_n===1'b1 && memblock_sync_pkg::reset_backend_done===1'b1) begin
    if (io_mem_to_ooo_writebackVldu_0_valid !== 1'b0 ||
        io_mem_to_ooo_writebackVldu_1_valid !== 1'b0) begin
        `uvm_fatal("MEMBLOCK_VEC_WB_UNSUPPORTED",
                   $sformatf("scalar-only flow observed writebackVldu valid: port0=%b port1=%b",
                             io_mem_to_ooo_writebackVldu_0_valid,
                             io_mem_to_ooo_writebackVldu_1_valid))
    end
end
```

中文伪代码：

```text
该逻辑在 scalar-only monitor flow 中承担 vector output 边界检查，不承担 writeback 状态更新。
每次 monitor 完成既有字段采样后，原 xz_sw 条件分支不执行任何检查或状态更新。随后判断 DUT 已退出
reset 且 backend reset 已完成；如果尚未完成，跳过新增 gate，避免把 reset 期间的未知值误报为业务
错误。进入 gate 后，分别读取两个 writebackVldu valid；只要任一值不是确定的 0，就调用 uvm_fatal，
打印两个端口的值并终止仿真。若两个值均为确定 0，则不创建 monitor transaction，不调用 adapter，
不写 raw/event 或 status/pass/fail/terminal，并继续下一拍采样。本段不调用任何 raw、adapter 或 status
helper；$sformatf 只格式化错误消息，uvm_fatal 是新增 gate 的唯一对外副作用。
```

### 正确性检查

- 原 `xz_sw` 分支为空，不存在既有有效检查；新增 `!== 1'b0` gate 是唯一检查，并同时覆盖有效 1 和 X/Z。
- reset 条件使用 case equality，避免 reset 未知时误进入业务检查。
- 既有 interface 字段到局部变量的采样赋值保持不变，本次没有增删或重解释 payload 字段。
- transaction 创建和 analysis port write 仍为注释代码，新增 gate 也不调用 raw queue、adapter 或公共状态 setter；因此该 monitor 修改前后都不生产 scalar semantic raw/event。

### 调用关系

| 调用顺序 | 函数或边界 | 在本流程中的功能 |
|---|---|---|
| 1 | `io_mem_to_ooo_vec_wb_agent_agent_monitor::run_phase()` | 进入该 agent 的 monitor 运行线程，并调用 `mon_data()`；不创建 semantic event。 |
| 2 | `io_mem_to_ooo_vec_wb_agent_agent_monitor::mon_data()` 采样边界 | 在无限循环中等待 `mon_cb`，按既有逻辑把 DUT vector WB interface 全部字段复制到局部变量。 |
| 3 | 空 `xz_sw` 分支 | 条件命中时也不执行检查、transaction 创建或状态更新；它不是有效 X/Z 检查。 |
| 4 | scalar-only unsupported gate | reset 完成后判断两个 valid 是否为确定的 0；非 0 或 X/Z 时执行 `uvm_fatal`。 |
| 5 | 正常返回到采样循环 | 两个 valid 都为 0 时不调用 adapter、raw queue、analysis port 或 status setter，继续等待下一拍。 |

## 4. 与 plan 的一致性

| plan 条目 | 实现结果 | 结论 |
|---|---|---|
| 20-agent runtime/RM 职责分类 | plan 中已完成分类表，本次不新增第二套 consumer | 一致 |
| `writebackVldu` scalar-only gate | reset 完成后对两个 valid 做 `!== 0` 检查并 fatal | 一致 |
| 不生成 vector 或 scalar raw/event/status | valid 为 0 时无输出对象；valid 非 0 直接 fatal；修改前后均无 raw/event producer | 一致 |
| cancel snapshot/redirect anchor producer | 由 LSQ/MMIO-status plan 唯一拥有，本 review 不纳入 | 一致 |
| DCache/L2TLB/RM/coverage | 未修改 | 一致 |

## 5. 与原测试框架逻辑对比及修改类型

### 字段/职责适配

- monitor 的 runtime role 与 RM/analysis role 在 plan 分类表中分离。
- 既有 vector WB interface 字段采样不变；不改变任何 scalar transaction 字段、status 字段、pass/fail 或 terminal owner。

### 新增功能

- 新增 scalar-only vector writeback unsupported gate。
- 新增行为类型不匹配时的 `uvm_fatal` 失败策略，防止 unsupported output 被静默丢弃。

### 保持不变的主体逻辑

- monitor 时钟采样和全部既有 interface 字段赋值保持不变；原 `xz_sw` 分支仍为空，不能描述为既有有效检查。
- transaction 创建与 `mon_item_port.write()` 仍保持注释，且没有 raw queue 写入；该 vector-WB monitor 修改前后都不生产 scalar raw/event，也不存在由本 monitor 维持的 scalar writeback raw/event 链。
- 本 plan 不负责 MMIO tag、cancel 对账、redirect/replay、ROB/LSQ commit/deq 或 RM/checker。

## 6. 实现与 Plan 不一致项

未发现实现与 Plan 不一致项；当前 coding 行为与对应 plan 保持一致。

## 7. Plan 未说明但 Coding 落实的细节

未发现 Plan 未说明但 Coding 额外落实的细节；当前 coding 细节均已在对应 plan 中覆盖。

## 8. 剩余风险与验证状态

- 已完成 V2 全量远端 compile/elaboration/link，结果为 `0 error(s), 0 warning(s)`；该编译包含本 monitor gate。
- 已完成真实 scalar store smoke，日志为
  `mem_ut/ver/ut/memblock/sim/watermark_fun/log/tc=basicTest_ts=memblock_dispatch_real_cancel_reconcile_vseq_cfg=tc_dispatch_real_cancel_reconcile_smoke_seed=666666_rtl_watermark_fix.log`。
  日志到达 `TEST_PASS`，且 `UVM_ERROR=0`、`UVM_FATAL=0`，没有触发
  `MEMBLOCK_VEC_WB_UNSUPPORTED`，证明正常 scalar 路径不会被误杀。
- 本轮没有通过 force 分别注入 valid=`1/X/Z` 的动态 expected-fatal；`!== 1'b0` 的四态语义已由源码静态检查确认。该缺口只影响错误路径的动态证据，不影响正常 scalar flow。
- 最后一轮独立 subagent review 已返回 `FINAL PASS`，本专项满足归档和提交条件。

## 9. 非本次修改的逻辑分析

### 9.1 `git status` 对比结论

本轮使用 `git status --short --untracked-files=all` 复核到 71 个状态项：39 个 `M`、12 个 `D`、20 个 `??`。其中只有以下 2 项属于本专项：

| 状态 | 文件 | 本专项/非本专项 | 依据 |
|---|---|---|---|
| `M` | `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_vec_wb_agent_agent/src/io_mem_to_ooo_vec_wb_agent_agent_monitor.sv` | 本专项 | 唯一源码增量是 scalar-only `writebackVldu` unsupported gate。 |
| `??` | `AI_DOC/plan/test_framework/review_doc/undo/mem_ut_v2_monitor_output_framework_adapt_implementation_review_20260722.md` | 本专项 | 本 implementation review 文档。 |

关联 plan `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_execution_plan_20260708.md` 是本 review 的对齐基线，但当前内容干净，未出现在上述 71 个状态项中。其余 69 项均为非本专项，完整分类如下。

### 9.2 非本专项文档、规则与 Plan

| 状态与数量 | 完整文件集合 | 本专项/非本专项 | 归属说明 |
|---|---|---|---|
| `M`，10 项 | `AGENTS.md`；`AI_DOC/mem_ut_flow_doc/{lsq_admission_flow.md,redirect_flow.md,rob_commit_lq_sq_deq_flow.md,virtual_sequence_unified_dispatch_flow.md,writeback_function_call_flow.md}`；`AI_DOC/project_management/{mem_ut_code_review_document_rule.md,mem_ut_flow_document_rule.md,mem_ut_test_framework_logic_build_rule.md,mem_ut_test_framework_plan_review_rule.md}` | 非本专项 | 项目规则以及 LSQ admission、redirect、commit/deq、统一 dispatch、writeback flow 的并行维护；不是 vector-WB gate 的源码或专项 review。 |
| `M`，1 项 | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_lsq_mmio_status_framework_adapt_execution_plan_20260708.md` | 非本专项 | LSQ MMIO/status plan 的执行中补充，记录最后 normal commit batch 后 `pendingPtr` watermark；不属于 monitor-output gate。 |
| `M`，1 项 | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md` | 非本专项 | pending-MMIO load sideband plan 的 coding 记录及 collector/reconcile、singleton adapter、directed helper 补充；不属于 monitor-output gate。 |

### 9.3 非本专项 Review 搬迁与新 Review

以下 11 个 basename 各自对应一个 `undo` 路径 `D` 和一个 `do` 路径 `??`，共 22 个状态项。已用旧文件 `HEAD` blob 与新文件 hash 核对，内容完全相同，因此准确归类为非本专项的纯路径搬迁：

- `ai_doc_mem_ut_uvm_v2_sync_review_20260706.md`
- `dispatch_100k_performance_optimization_code_review.md`
- `dispatch_plan_v2_implementation_review.md`
- `dispatch_plan_v2_review_annotated.md`
- `main_table_boundary_addr_reuse_integration_implementation_review_20260703.md`
- `mem_ut_uvm_v2_branch_migration_implementation_review_20260706.md`
- `mem_ut_v2_compile_param_and_width_adapt_implementation_review_20260713.md`
- `memblock_tlb_entry_review.md`
- `nonblocking_issue_drive_fired_mask_implementation_review_20260618.md`
- `redirect_flush_batch_path_review.md`
- `sfence_hfence_tlb_invalidate_review_checklist.md`

另外 3 个 review 状态项必须单独分类：

| 状态 | 文件 | 本专项/非本专项 | 归属说明 |
|---|---|---|---|
| `D` + `??` | `AI_DOC/plan/test_framework/review_doc/{undo,do}/mem_ut_v2_lsq_enqueue_framework_adapt_implementation_review_20260716.md` | 非本专项 | 搬迁到 `do` 的同时还把文内 CSR control plan 引用从 `undo` 更新为 `do`，不是内容不变的纯搬迁。 |
| `??` | `AI_DOC/plan/test_framework/review_doc/undo/mem_ut_v2_pending_mmio_load_sideband_implementation_review_20260722.md` | 非本专项 | 新增且仍位于 `undo` 的 pending-MMIO load sideband implementation review，是并行专项的待验证 review，不是归档搬迁。 |

### 9.4 非本专项 Agent、编译参数与公共 Raw/Sideband

| 状态与数量 | 完整文件集合 | 本专项/非本专项 | 归属说明 |
|---|---|---|---|
| `M`，3 项 | `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/{io_mem_to_ooo_ctrl_agent_agent_interface.sv,io_mem_to_ooo_ctrl_agent_agent_monitor.sv,io_mem_to_ooo_ctrl_agent_agent_xaction.sv}` | 非本专项 | pending-MMIO output accessor/raw producer、SQ deq 适配和 LSQ cancel snapshot 采样，分别由 pending-MMIO 与 LSQ MMIO/status owner 负责。 |
| `M`，1 项 | `mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv` | 非本专项 | 缓存并在 idle 周期保持 `pendingPtr/pendingst/pendingMMIOld` level sideband，属于 LSQ MMIO/status 驱动合同。 |
| `M`，1 项 | `mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_monitor.sv` | 非本专项 | 生产 redirect sample anchor 供 cancel 对账使用，不生产第二个 recovery event；属于 LSQ MMIO/status timing sideband。 |
| `M`，1 项 | `mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh` | 非本专项 | 新增 SQ deq/cancel count width，以及 cancel observe latency 和队列容量的编译期合同。 |
| `M`，1 项 | `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv` | 非本专项 | 同时承载 pending-MMIO ctrl raw 字段与 cancel snapshot/redirect anchor 队列、sample sequence；目标 plan 明确规定这些源码由其他 owner 唯一实现。 |

### 9.5 非本专项 Sequence、Helper 与测试配置

以下 11 个已跟踪 `M` 文件属于 LSQ MMIO/status、commit head sideband、redirect cancel 对账及其 smoke 适配，不属于 vector-WB monitor gate：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_redirect_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_fault_smoke_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/exception_redirect_replay_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`

以下 8 个已跟踪 `M` 文件属于 pending-MMIO tag/raw/owner 主线及其与 timing sideband service 的共享集成，不属于 vector-WB monitor gate：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/status_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/seq.f`
- `mem_ut/ver/ut/memblock/seq/seq_pkg.sv`

以下 6 个 `??` 文件是两个并行专项的新 sequence/helper/config，不属于 vector-WB monitor gate：

| 完整文件集合 | 本专项/非本专项 | 归属说明 |
|---|---|---|
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_cancel_reconcile_sequence.sv`；`mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_cancel_reconcile_smoke.cfg`；`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_cancel_reconcile_vseq.sv` | 非本专项 | redirect cancel 对账 sequence、专项配置与 virtual sequence。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`；`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_op_behavior_util.sv`；`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_pending_mmio_directed_vseq.sv` | 非本专项 | pending-MMIO directed sequence、统一行为派生 helper 与 virtual sequence。 |

### 9.6 覆盖闭环

分类数量为：本专项 2 项 + 非本专项文档/Plan 12 项 + 非本专项 review 25 项 + 非本专项 agent/参数/公共包 7 项 + 非本专项 sequence/helper/config 25 项 = 71 项，与 `git status` 快照一致。当前状态中没有仿真/生成产物，也没有未知或待归属项；上述非本专项文件均不进入本 review 的功能正确性结论，本专项也不修改或提交它们。
