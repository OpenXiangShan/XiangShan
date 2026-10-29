# Memblock mother integration status - 2026-10-29

## Mother

- Worktree: `.codex_mother_integration_20261028`
- Branch: `codex/memblock-csr-prefix-fix-20261025`
- 本轮已验收源码提交：`4948d291e`；CSR closure 文档提交：`201901c3d`。
- 工作区状态按 `git status --short` 实时检查；文档提交不改变本轮已编译源码。

## Integrated Bug Commits

| Commit | Bug | Mother verification | Source worktree |
|---|---|---|---|
| `91d403eb1` | PMA/PMP CSR raw FIFO continuous prefix service | integrated before this continuation; later PBMT integration preserved ticket/revision/prefix semantics | preserved WIP source worktree remains untouched |
| `fb9f04abe` | L2TLB PPN reuse smoke determinism | specialized 22/22 plus adjacent Sv39 1/1 PASS | deleted; branch retained |
| `a13d10814` | Sv39 later-split mabuf response provenance | compile; original1; known5+random30 35/35; closure10 10/10; adjacent2; review-fix5; post-commit original1, all strict PASS | deleted; branch retained |
| `5c131a18d` | PBMT response-fault directed audit | compile; main production 44/44 strict PASS; NC/IO gate 20/20 clean legacy PASS; post-commit original1 strict PASS | deleted; branch retained |
| `9a2df3d7e` | CSR scalar 独立 cfg 前置条件 | 四 cfg dry-run 门禁通过；与 prefix 修复共同合入后，四个 10k 原始 seed 全部 strict PASS | source worktree 不再作为后续修改目标 |
| `4948d291e` | CSR redirect preserved barrier prefix invariant | mother full compile；prefix directed 1/1 PASS；四个 CSR scalar 10k 原始 seed 4/4 strict PASS | 既有 `.codex_csr_prefix_fix_20261025` 已停止开发，待清理 |

Sv39 source commit `52f2d0a57` and PBMT source commit `43669b2d3` were not cherry-picked because each included
excluded software-only changes. Their production logic was reconstructed on the mother branch and independently
reviewed before commit.

## Worktree Cleanup

Deleted after integration/retest/review:

- `.codex_sv39_split_uncache_rm_fix_20261025`
- `.codex_pbmt_response_fault_fix_20261025`
- `.codex_fifo_cached_verify_20261027`
- `.codex_fifo_cached_verify_20261027_r11`
- `.codex_fifo_cached_verify_20261027_r12`
- `.codex_fifo_cached_verify_20261027_r13`

The four FIFO verify worktrees were clean detached cached-tree checkpoints. Their final production files are
byte-identical to integrated commit `91d403eb1`; intermediate revisions were not merged.

## 2026-10-29 CSR Prefix Mother Verification

所有任务均在 mother worktree 串行执行，mode=`mother_csr_prefix_merge_20261029`，没有并行 seed。

| tc | ts | cfg | seed | Result | Strict log audit |
|---|---|---|---:|---|---|
| `basicTest` | `memblock_rob_admission_window_directed_vseq` | `default` | `2601277` | 1/1 PASS | `TEST_PASS=1`，caught fatal=19（专项预期），final error/fatal=0，正常 `$finish` |
| `basicTest` | `memblock_csr_scalar_load_10k_vseq` | `tc_csr_scalar_load_10k` | `1684208666` | 1/1 PASS | `TEST_PASS=1`，caught error/fatal=0，final error/fatal=0，正常 `$finish` |
| `basicTest` | `memblock_csr_scalar_store_10k_vseq` | `tc_csr_scalar_store_10k` | `1945381911` | 1/1 PASS | 同上 |
| `basicTest` | `memblock_csr_scalar_mixed_10k_vseq` | `tc_csr_scalar_mixed_10k` | `288884785` | 1/1 PASS | 同上 |
| `basicTest` | `memblock_csr_scalar_store_sta_std_10k_vseq` | `tc_csr_scalar_store_sta_std_10k` | `195710061` | 1/1 PASS | 同上 |

五个 controller summary 均为 `completed=pass=1`，且
`fail/infra/incomplete/aborted/not_run/remote_unknown_jobs=0`。权威 summary 目录：

- `mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_default_20261029_094013_1439875_18664`
- `mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_tc_csr_scalar_load_10k_20261029_094541_1527032_789`
- `mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_tc_csr_scalar_store_10k_20261029_100020_1762864_20307`
- `mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_tc_csr_scalar_mixed_10k_20261029_101217_1952662_25705`
- `mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_tc_csr_scalar_store_sta_std_10k_20261029_102811_2195279_10269`

## Preserved / Not Merged

- `.codex_csr_prefix_fix_20261025`: prefix Bug commit 已通过既有错误 worktree形成并合入 mother；该 worktree
  不再承接修改，待确认无未合并独立问题后删除。
- CSR scalar cfg 前置修复已合入 mother；后续 CSR scalar 回归和修复只在 mother worktree 顺序执行。
- `.codex_l2tlb_stage_verify`, `_r3`, `_r4`, `_r7`, `_r8`: dirty historical adaptation worktrees; not part of the
  current Bug integration and therefore not deleted.
- `.codex_sparse_ppn_isolation_20261025`: dirty detached worktree with soft-test/cfg changes; not deleted.

## Current Decision

CSR prefix Bug 与 CSR scalar cfg Bug 已完成 mother 集成和原失败用例复测。后续不再创建或扩展 Bug
worktree。2026-10-29 用户允许并发仿真加速：配置逐组推进，组内最多并发 10 个 seed，同一 mode 只运行
一个 controller，保持 monitor/record 和首错联停。代码修改仍在 mother worktree 中逐个 Bug 顺序完成：
记录、方案、修改、专项验证和单 Bug commit；不修复无关 `soft_test`。

## BasicTest 原始矩阵逐组进度

以下为源码 `4948d291e` 上的原始 12 组、120 个 seed 的核验台账；CSR scalar 四项属于额外专项，
不计入这 120 项。每组 seed 和临时参数来自原始 `functional_cfg_cov_20261023_results/<cfg>/config.txt`
及 `seeds.txt`；使用 mother 中已经修正的 cfg。历史其他源码的 PASS 不计入本表。

| 序号 | tc | ts | cfg | 本轮状态 | 已验收/计划 |
|---:|---|---|---|---|---:|
| 1 | `basicTest` | `memblock_dispatch_manual_control_vseq` | `csr_sfence_check_store_manual_control` | `PASS` | 10/10 |
| 2 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `csr_sfence_check_store_rob_control` | `NOT_RUN` | 0/10 |
| 3 | `basicTest` | `memblock_dispatch_real_cancel_reconcile_vseq` | `tc_dispatch_real_cancel_reconcile_smoke` | `NOT_RUN` | 0/10 |
| 4 | `basicTest` | `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k` | `NOT_RUN` | 0/10 |
| 5 | `basicTest` | `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16` | `NOT_RUN` | 0/10 |
| 6 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_dispatch_real_mmu_sv39_smoke` | `NOT_RUN` | 0/10 |
| 7 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_dispatch_real_mmu_sv39_strict_nc_100k` | `NOT_RUN` | 0/10 |
| 8 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_l2tlb_fault_2pct_no_trigger` | `NOT_RUN` | 0/10 |
| 9 | `basicTest` | `memblock_l2tlb_pbmt_response_fault_vseq` | `tc_l2tlb_pbmt_response_fault` | `NOT_RUN` | 0/10 |
| 10 | `basicTest` | `memblock_csr_random_config_vseq` | `tc_memblock_csr_random_config` | `NOT_RUN` | 0/10 |
| 11 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_sfence_stress_10k` | `NOT_RUN` | 0/10 |
| 12 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_sfence_stress_1k` | `NOT_RUN` | 0/10 |

第 1 组结果目录：
`mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_csr_sfence_check_store_manual_control_20261029_105234_2388929_29884`。
controller 为 `planned=completed=pass=10`，所有失败/未完成分类为 0，`remote_unknown_jobs=0`。
10 个 final log 均有一次 `TEST_PASS`、正常 `$finish`，raw/caught/demoted UVM ERROR/FATAL 六项均为 0。
`seeds.txt` 与原始归档逐行一致；实际激励参数保留 `MAIN_TRANS_NUM=1000`、`CHECK_TRIGGER_EN=0`。

其余 14 组 functional matrix、fault3 100-seed 专项和最终覆盖率尚未完成。当前 10/10 只关闭第一组，
不能解释为母任务全部完成；发生源码修复后需要重新评估已通过组的相邻复测范围和最终统一版本验收。
