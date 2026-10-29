# MemBlock CSR Redirect Preserved Barrier Prefix Implementation Review Round 2

**日期**：2026-10-27
**Reviewer**：独立 implementation reviewer
**结论**：`REVIEW_FAIL`
**范围**：只审查 CSR redirect preserved barrier prefix/control-barrier 修复；不审查 stale TLB/RM root、residency、issue snapshot 修改。
**工作树**：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025`

## Findings

### P0：terminal lifecycle 的正常 commit 段仍可在部分 mutation 后 fatal，不满足 plan 的原子性合同

- 正式 plan 明确要求：生产 mutation 可能失败时，必须先完成全部无副作用 precheck，terminal commit 段不得再 fatal：
  - `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/AI_DOC/plan/test_framework/plan/undo/memblock_csr_redirect_preserved_barrier_prefix_invariant_fix_plan_20261025.md:786-791`
  - `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/AI_DOC/plan/test_framework/plan/undo/memblock_csr_redirect_preserved_barrier_prefix_invariant_fix_plan_20261025.md:814`
- 当前 `try_retire_control_committed_uid()` 只完成 lifecycle count/predecessor precheck，随后先写 `success=1`、`terminal_done=1`，再调用通用 `retire_active_uid()`：
  - `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:7210-7251`
- `retire_active_uid()` 不是 no-fatal commit。它在清 uncache retry、建立 late tombstone、删除 issue/PTW work、清 STA tombstone之后，才检查 active ROB map；后续 LQ/SQ map检查也仍可 fatal：
  - `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:6972-7027`
- 因此，一旦 active ROB map 不一致等错误被 expected-fatal catcher 捕获，caller 会返回到测试流程，但 `success`、`terminal_done`、terminal frontier、issue/PTW/tombstone状态已经可能被部分修改，`terminal_count` 又尚未 record。这正是 plan 禁止的“fatal正确但生产状态已部分提交”。
- r19 duplicate-terminal case无法覆盖该风险，因为它把 `control_audit_terminal_count` 预置为1，fatal发生在 lifecycle precheck，根本不会进入正常 terminal commit：
  - `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_rob_admission_window_sequence.sv:583-610`

**必须整改**：在任何 terminal 状态写入前增加完整、只读的 control-retire precheck，至少覆盖 active ROB identity、LQ/SQ neutral及map一致性、将被删除的 issue/PTW/late-context条件和 terminal frontier可推进条件；随后使用不含 `uvm_fatal`/report-capable lookup 的专用 commit，或把通用 retire 严格拆成 `precheck_retire_active_uid()` + no-fatal `commit_retire_active_uid()`。新增正常 `terminal_count=0` 的 caught-fatal原子性 case，注入 active ROB map不一致，逐项证明 success/terminal/frontier、ROB/LQ/SQ maps、issue/PTW/tombstone、barrier、owner/generation和全部audit counts不变。

### P1：duplicate lifecycle directed 已走生产入口，但未达到 plan 定义的完整负向快照与 marker/action 矩阵

- 本轮已确认测试不再直接调用底层 precheck，而是调用真实生产入口：
  - owner bind：`memblock_control_barrier_service::bind_control_owner()`，`soft_test_memblock_rob_admission_window_sequence.sv:482-492`
  - CSR enqueue：`common_data_transaction::enqueue_csr_control_action()`，`soft_test_memblock_rob_admission_window_sequence.sv:506-527`
  - CSR start：`common_data_transaction::try_pop_csr_control_action()`，`soft_test_memblock_rob_admission_window_sequence.sv:529-551`
  - CSR completion：`memblock_control_barrier_service::complete_csr_runtime_snapshot()`，`soft_test_memblock_rob_admission_window_sequence.sv:553-581`
  - terminal：`common_data_transaction::try_retire_control_committed_uid()`，`soft_test_memblock_rob_admission_window_sequence.sv:583-610`
  - barrier release：`common_data_transaction::release_control_barrier_after_terminal()`，`soft_test_memblock_rob_admission_window_sequence.sv:612-633`
- 这些入口的 duplicate 分支确实在 mutation 前调用 `precheck_control_lifecycle_event()`；`record_control_lifecycle_event()` 本身只置审计count并输出日志，没有第二个 fatal检查：
  - `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:2218-2383`
  - CSR enqueue/start：`common_data_transaction.sv:2740-2811`
  - barrier release：`common_data_transaction.sv:2648-2715`
- 但正式验收要求快照 CSR、SFence、L2 ASSERT、L2 RELEASE 全部queue的size/front/完整顺序，以及 flushSb live字段、active/terminal、ROB/LQ/SQ map和所有相关status字段：
  - `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/AI_DOC/plan/test_framework/plan/undo/memblock_csr_redirect_preserved_barrier_prefix_invariant_fix_plan_20261025.md:822-835`
- 当前 directed 只构造单元素 CSR queue，并比较 CSR queue的size/front；没有保存完整queue数组，也没有覆盖 SFence、L2 ASSERT、L2 RELEASE 的 duplicate enqueue/start/completion生产入口。terminal case没有建立和快照真实 `uid_by_active_rob` identity，release case也没有快照三类queue/request及 attached/active/completed flushSb live状态：
  - `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_rob_admission_window_sequence.sv:437-641`
- 因此 r19 PASS只证明当前六个 CSR/control duplicate点的已选择字段未变，不能证明 plan 所写的完整 action矩阵和所有潜在副作用载体均不变。

**必须整改**：增加统一 lifecycle atomic snapshot helper，保存并比较四类queue完整元素顺序、ROB/LQ/SQ maps、flushSb attached/active/completed与request状态、terminal frontier、control runtime snapshot payload/seq、owner/generation/state/action字段和全部audit counts。至少用双元素sentinel验证 start失败后完整顺序不变，并对 SFence、check-store L2 ASSERT、L2 RELEASE 的真实 enqueue/start/completion入口分别构造 duplicate caught-fatal。不得用直接改count后调用底层precheck代替生产入口。

## r19 证据核对

- r19 compile产物已完成VCS stitch/link并生成 `simv`；compile日志：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/sim/csr_prefix_fix_compile_20261027_r19/log/vcs_compile_rtl.log`。
- directed日志：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/sim/csr_prefix_fix_compile_20261027_r19/log/tc=basicTest_ts=memblock_rob_admission_window_directed_vseq_cfg=default_seed=2601272_rtl_control_production_entry_directed_r19.log`。
- 日志核对结果：`TEST_PASS`位于2025行；caught `UVM_FATAL=9`位于2044行；最终 `UVM_ERROR=0/UVM_FATAL=0`位于2055-2056行，正常进入报告收尾。该证据有效关闭“duplicate测试没有调用真实生产入口”的旧finding，但不关闭上述正常 terminal commit原子性和完整负向矩阵缺口。

## 边界结论

- 未发现本问题A修改 DUT/RTL/Scala/Chisel。
- 未发现通过放宽 checker、跳过 lifecycle count或吞掉未预期fatal来获得PASS；catcher只捕获指定ID。
- redirect preserved barrier prefix 主链的既有 precheck/bit传播不在本轮新增 blocker中。
- stale TLB/RM root、residency、issue snapshot相关文件和方案明确排除，不作为本review结论依据。

## Verdict

`REVIEW_FAIL`

在 P0 terminal no-fatal commit 和 P1 完整 duplicate矩阵/快照关闭并重新完成 compile + directed前，不得将该 control-only implementation 标记为 `REVIEW_PASS`，不得据此归档 plan或提交单issue commit。
