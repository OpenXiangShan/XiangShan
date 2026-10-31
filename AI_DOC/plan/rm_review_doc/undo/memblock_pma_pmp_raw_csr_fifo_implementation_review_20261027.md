# PMA/PMP Raw CSR FIFO Final Independent Implementation Review

## 术语与版本边界（2026-10-31补记）

`FIFO`为有序CSR写事实队列；`ticket`为冻结前缀/版本的原子提交凭据；`replay`为按生效时刻更新RM模型；`C+2`为捕获后两拍生效边界。
本原review签核clean r13/91d隔离实现，不能用于宣称后来5c没有request/issue调用。当前mother在`5c131a18de`恢复共用`prepare_pma_pmp_csr_replay()`/`commit_pma_pmp_csr_replay()`的请求边界wrapper，持续service与wrapper共享同一破坏性提交入口，不是第二个裸pop消费者。
当前最终方案同步于`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/AI_DOC/plan/rm_plan/do/memblock_pma_pmp_raw_csr_fifo_single_consumer_fix_plan_20261027.md`第1.1节。5c集成review为同仓库`AI_DOC/plan/test_framework/review_doc/undo/memblock_l2tlb_pbmt_response_fault_production_integration_review_20261028.md`；此处不改变原Kant结论或审核日期。

### Plan对齐与证据保全

原“service callers限于持续入口”是91d时点；5c新增边界helper是后续实现差异，按Git如实记录，不倒签成原方案已覆盖。r13原工作区及原日志已删除，原绝对路径保留为历史标识，尚未确认同hash备份；Git tree与当前mother本review可读。母后续PBMT和350项结果是后续复验，不替代缺失的原日志。

### 非本次修改的逻辑分析

本次仅文档补记，无源码改变；四项既有V2知识库脏文档及signal.rc不属于本次。以下原英文正文保持原审查时点，不据其中report-free/WIP叙述推导当前mother行为。

## Review Input

- Worktree: `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fifo_cached_verify_20261027_r13`
- Base: `045011bf6b`
- HEAD: `169a34a5aca8236d11c12aca88ab5267d75f9397`
- Tree: `b5639d4028e26ca470ad5097c61c57c59bfb4508`
- Diff SHA256: `5f1f7c09cdf50cbfa4fe34e2a33ad36678e1170c279d156753c117583e61b6cb`
- Worktree status: clean
- `git diff --check 045011bf6b HEAD`: PASS
- Static checker: PASS (`check_pma_pmp_csr_fifo.py`, 2069 function bodies)
- Review mode: independent, read-only. No repository source or index was modified and EDA was not rerun.

## Scope Isolation

PASS.

- The diff contains the approved PMA/PMP raw CSR FIFO plan/review, producer capture, FIFO state/API, continuous replay owner, startup/dynamic completion gates, PMA/PMP batch model, dedicated checker/test, and package/testcase registration.
- No simulation or coverage output is included in the Git diff.
- The r11-to-r13 delta modifies only `soft_test_pma_pmp_csr_fifo_sequence.sv`; production implementation is unchanged while the two previous verification blockers are closed.
- No stale-TLB lifecycle, issue-residency, ROB-admission, provenance, or issue-fire production behavior is introduced. Related text is limited to documented exclusions and checker allowlists.

## Production Contracts

### Producer and reset domain

PASS.

- The CSR monitor captures `reset_epoch` with the raw write before calling the producer API (`csr_ctrl_agent_agent_monitor.sv:514-527`).
- `push_raw_pma_pmp_csr_write()` calls the report-free `precheck_raw_pma_pmp_csr_write_push()` before queue/count/revision mutation. The production wrapper retains `uvm_fatal` on invalid input, so the fatal contract is not weakened (`memblock_sync_pkg.sv:2763-2790`).
- Startup epoch zero is accepted only for an initialized lifecycle with reset inactive; active reset requires the matching nonzero epoch (`memblock_sync_pkg.sv:1041-1059`).
- Producer count, raw revision, service revision, model revision, applied count, generations, ticket revisions, capture/effective samples, and expected counts use 64-bit unsigned state or explicit unsigned conversion.

### Atomic testcase/runtime reset

PASS.

- `reset_all_tables()` runs the aggregate model/raw/service/reset-domain precheck before PMA/PMP reset mutation and calls `reset_raw_pma_pmp_csr_writes()` once (`common_data_transaction.sv:393-406,541-546`).
- `clear_raw_monitor_queues()` does not reset or flush the PMA/PMP raw FIFO (`memblock_sync_pkg.sv:3170-3189`). One logical testcase reset therefore advances the raw revision once.
- `reset_pma_pmp_csr_replay_for_runtime_epoch()` requires active nonzero current epoch, rejects an already-serviced epoch, performs the same aggregate report-free precheck, and only then resets model, raw FIFO, watermark/count, epoch, and service revision (`common_data_transaction.sv:700-734`).
- The adapter invokes runtime replay reset before acknowledging its runtime-reset responsibility (`dispatch_monitor_event_adapter.sv:41-63`).

### Single consumer, C+2, and exactly-once

PASS.

- `prepare_pma_pmp_csr_replay()` scans only the continuous mature raw prefix, rejects invalid/out-of-domain/nonmonotonic events and capture values above `MAX_U64-2`, and converts capture sample C to effective model sample C+2 without mutating shared state (`common_data_transaction.sv:8071-8107`).
- `commit_pma_pmp_csr_replay()` revalidates ticket consumption state, epoch, service revision, service capacity, exact raw prefix/revision, model revision/generations, and applied-count capacity before entering the mutation-only section (`common_data_transaction.sv:8110-8142`).
- `commit_pma_pmp_csr_replay()` is the only production owner combining model batch commit and destructive raw-prefix commit. The legacy raw pop API has no callers.
- Model and outer replay tickets are marked consumed, so stale or duplicate commits are rejected. Directed tests also verify stale-prefix and duplicate-ticket zero-side-effect behavior.
- Continuous production service is limited to runtime collection, startup wait, and control/dynamic completion paths. It treats a nonempty FIFO with no C+2-mature prefix as normal no-work.

### Outcomes, generations, and model-off behavior

PASS.

- PMPADDR/PMAADDR changed detection compares the pre-write address before assignment (`memblock_pma_pmp_model.sv:680-709`).
- CFG replay distinguishes accepted changed, accepted same-value, all-locked, unsupported, and unknown outcomes. Any accepted group write creates exactly one corresponding generation/history event, including accepted same-value writes (`memblock_pma_pmp_model.sv:629-678,711-875`).
- Mixed-lock handling skips locked slots, applies unlocked slots, aggregates `value_changed`, and creates one group generation/history event when at least one slot is accepted.
- PMA/PMP model enable does not gate producer, consumer, or history maintenance. The model-off directed case verifies FIFO drain, produced/applied conservation, generation advancement, and C+2 history (`soft_test_pma_pmp_csr_fifo_sequence.sv:254-277`).

### Completion and end audit

PASS.

- Startup and dynamic write plans calculate 64-bit expected counts before driving writes and fail before drive/stage on invalid epoch or overflow (`memblock_csr_initial_config_sequence.sv:31-67`; `memblock_dynamic_csr_change_sequence.sv:23-51,145-161`).
- Completion requires expected count reached, applied equals produced, FIFO empty, and profile/runtime snapshot agreement (`common_data_transaction.sv:615-657,680-686`; `memblock_control_barrier_service.sv:312-340`).
- `end_test_check()` only audits produced/applied conservation and pending FIFO state. It does not service, commit, pop, reset, or flush this FIFO (`common_data_transaction.sv:9535-9558`).

## Previous Blockers B1-B4

### B1: PMPADDR/PMAADDR changed versus same-value

Closed.

- Production compares the old address before assignment.
- Directed assertions explicitly cover changed and same-value outcomes for PMPADDR and PMAADDR, plus PMPCFG and PMACFG (`soft_test_pma_pmp_csr_fifo_sequence.sv:155-187`).

### B2: Testcase reset clearing raw FIFO twice

Closed.

- The generic raw-monitor clear excludes this FIFO.
- The dedicated testcase reset clears it once after aggregate precheck.
- Directed verification confirms one logical testcase reset advances the raw revision exactly once (`soft_test_pma_pmp_csr_fifo_sequence.sv:613-619`).

### B3: Runtime reset partial mutation before raw failure

Closed.

- Production performs the aggregate report-free precheck before the first participant mutation.
- Under a valid active nonzero runtime epoch, directed tests force model, raw, and service revision exhaustion separately, call `precheck_pma_pmp_csr_replay_reset(1'b1, runtime_epoch)`, require rejection, and compare complete snapshots of FIFO, tables, history, counts, revisions, generations, and watermark to prove zero shared mutation (`soft_test_pma_pmp_csr_fifo_sequence.sv:447-520`).

### B4: Directed acceptance matrix completeness

Closed.

The directed test now covers:

- C/C+1 no-work, C+2 ordered application, exactly-once, continuous no-request drain, stale/duplicate tickets, and audit-only end state (`soft_test_pma_pmp_csr_fifo_sequence.sv:661-723`).
- PMP/PMA ADDR and CFG changed/same-value, ignored locked/unsupported/unknown, and generation accounting (`soft_test_pma_pmp_csr_fifo_sequence.sv:155-252`).
- Mixed-lock changed and same-value writes with explicit locked-slot preservation, unlocked-slot result, exactly one generation/history/snapshot increment, producer/applied increments, and empty FIFO (`soft_test_pma_pmp_csr_fifo_sequence.sv:194-246`).
- Model-off FIFO/history behavior (`soft_test_pma_pmp_csr_fifo_sequence.sv:254-277`).
- Applied count, raw/service/model revisions, PMP/PMA generations, capture `MAX_U64-2` acceptance, capture `MAX_U64-1` rejection, producer overflow, and zero-mutation rejection paths (`soft_test_pma_pmp_csr_fifo_sequence.sv:279-445,522-611`).
- Valid nonzero runtime reset success and runtime-mode model/raw/service revision-exhaustion zero-mutation prechecks (`soft_test_pma_pmp_csr_fifo_sequence.sv:447-520`).
- No catcher, severity demotion, or expected UVM error/fatal path is used by the directed sequence.

## Verification Evidence

PASS.

- Existing eda01 independent compile artifact completes compilation, stitching, elaboration, and link successfully.
- `results.tsv`: seed 1 is `PASS`, `EXIT_CONFIRMED`, rc=0, UVM count 0.
- `summary.log`: planned=1, completed=1, pass=1, fail/infra/incomplete/aborted/pending/not_run=0, `remote_unknown_jobs=0`.
- Directed log records mixed-lock changed at capture 53 with outcome 0 and PMP generation 2, then mixed-lock same-value at capture 56 with outcome 1 and PMP generation 3.
- Directed completion message, `TEST CASE PASSED`, and `$finish` are present.
- Final raw UVM summary: `UVM_ERROR=0`, `UVM_FATAL=0`.
- Report catcher summary: caught and demoted UVM ERROR/FATAL are all zero.

## Documentation Consistency

PASS.

The implementation and directed acceptance matrix match the approved simplified plan and round4 plan review for capture/effective timing, reset domains, report-free prechecks, 64-bit capacity rules, outcome semantics, continuous single-consumer service, completion gates, model-off history, and audit-only end-test behavior.

## Conclusion

No implementation or verification blockers remain within the reviewed PMA/PMP raw CSR FIFO scope. B1-B4 and all requested production contracts are closed by code, static checks, and strict directed evidence.

REVIEW_PASS
