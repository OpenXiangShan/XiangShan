# Memblock mother integration status - 2026-10-29

## Mother

- Worktree: `.codex_mother_integration_20261028`
- Branch: `codex/memblock-csr-prefix-fix-20261025`
- 当前生产源码提交：`c9763bd7d`；此前集成基线为 `4948d291e`，CSR closure 文档提交为 `201901c3d`。
- 当前执行状态：`STOPPED_RTL_CONFIRMED`；strict-NC RTL Bug 等待 coding 授权，无活动仿真，母任务未完成。
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
| `c9763bd7d` | redirect 提交遗漏 later-split cancel | 修复前定向稳定 FAIL；修复后定向 PASS，真实 DUT 固定/随机 39/39 严格 PASS，独立 `REVIEW_PASS` | 直接在 mother 完成 |

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

以下为母任务原始 12 组、120 个 seed 的核验台账：前六组首先在源码 `4948d291e` 上通过；
第 4/5/6 组又在 later-split cancel 修复后分别通过 10/10，后续使用修复后的源码。CSR scalar 四项属于额外专项，
不计入这 120 项。每组 seed 和临时参数来自原始 `functional_cfg_cov_20261023_results/<cfg>/config.txt`
及 `seeds.txt`；使用 mother 中已经修正的 cfg。历史其他源码的 PASS 不计入本表。

| 序号 | tc | ts | cfg | 本轮状态 | 已验收/计划 |
|---:|---|---|---|---|---:|
| 1 | `basicTest` | `memblock_dispatch_manual_control_vseq` | `csr_sfence_check_store_manual_control` | `PASS` | 10/10 |
| 2 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `csr_sfence_check_store_rob_control` | `PASS` | 10/10 |
| 3 | `basicTest` | `memblock_dispatch_real_cancel_reconcile_vseq` | `tc_dispatch_real_cancel_reconcile_smoke` | `PASS` | 10/10 |
| 4 | `basicTest` | `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k` | `PASS` | 10/10 |
| 5 | `basicTest` | `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16` | `PASS` | 10/10 |
| 6 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_dispatch_real_mmu_sv39_smoke` | `PASS` | 10/10 |
| 7 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_dispatch_real_mmu_sv39_strict_nc_100k` | `FAIL/STOPPED_RTL_CONFIRMED` | 0/10 |
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

第 2 组结果目录：
`mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_csr_sfence_check_store_rob_control_20261029_105807_2447673_20585`。
10 颗 seed 并发执行，controller 为 `planned=completed=pass=10`，`peak_active=10`，所有失败/未完成分类为 0，
`remote_unknown_jobs=0`；逐日志严格验收与第 1 组相同，seed 清单与原始归档逐行一致。

第 3 组结果目录：
`mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_tc_dispatch_real_cancel_reconcile_smoke_20261029_110111_2464759_19771`。
controller 为 `planned=completed=pass=10`，失败/未完成分类和 `remote_unknown_jobs` 均为 0；
逐日志严格验收通过，seed 清单与原始归档逐行一致。

第 4 组结果目录：
`mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_20261029_110312_2474804_26405`。
controller 10/10 PASS、无其余异常分类，逐日志严格验收和原始 seed 清单比对通过。
cfg 名称含 `100k`，本轮仍保留原始归档的 `MAIN_TRANS_NUM=1000` 覆盖参数。

第 5 组结果目录：
`mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16_20261029_110607_2486962_27098`。
10/10 controller 和逐日志严格验收均 PASS，seed 清单与原始归档一致，所有异常分类为 0。

第 6 组结果目录：
`mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_tc_dispatch_real_mmu_sv39_smoke_20261029_111009_2539662_24995`。
原始 seed `212889620` 及原先 9 颗被联停的 seed 全部严格 PASS，seed 清单与原始归档一致，
controller 无失败/未完成/远端未知任务。该结果未触发静态审查发现的旧 later-split completion 迟到场景；
该后续问题现已按 `memblock_redirect_later_split_cancel_integration_plan_20261029.md` 完成确定性复现、修复和专项验证，
提交为 `c9763bd7d`；当前暂停原因是第 7 组 strict-NC 的独立新问题，不是 later-split cancel 尚未修复。

其余 14 组 functional matrix、fault3 100-seed 专项和最终覆盖率尚未完成。当前共 60/120 关闭前六组，
不能解释为母任务全部完成；发生源码修复后需要重新评估已通过组的相邻复测范围和最终统一版本验收。

## 当前单 Bug 闭环

| 问题 | 责任与复现 | 当前修改 | 状态与恢复条件 |
|---|---|---|---|
| redirect 集成遗漏 later-split cancel | 静态 review 发现；定向 seed `2601029` 在 201ns 报 `LATER_SPLIT_TLB completion provenance mismatch`，修复前 1/1 FAIL | mother 中恢复旧 epoch 的现有 cancel 调用，补 4 个确定性场景及前置失败原子性检查 | `FINAL_PASS`；定向 PASS、真实 DUT 固定/随机 39/39 严格 PASS；独立最终 `REVIEW_PASS`，单 Bug commit=`c9763bd7d` |
| strict-NC 写回找不到活动 UID | `basicTest / memblock_dispatch_real_smoke_vseq / tc_dispatch_real_mmu_sv39_strict_nc_100k / seed=846339364`，787.300ns `INT_WB_ATTACH no active uid for ROB key=0/14`；两位独立 reviewer 确认 NC staging 漏取消为 RTL 责任 | 尚无代码修改；原 seed debug 在相同时刻复现；局部 NC valid/data 寄存器修复方案已 review，等待 RTL coding 授权 | `STOPPED_RTL_CONFIRMED`；原批次 1 FAIL、5 ABORTED、4 NOT_RUN，unknown=0；debug compile PASS、单 seed FAIL，所有 controller 已退出 |

修复前结果目录：
`mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_default_20261029_112338_2626771_13498`。
正式记录在 W44 Buglist 的“Redirect 提交 helper 集成遗漏 later-split 旧记录取消”章节。
没有创建新 worktree；生产修改始终在本 mother worktree 顺序完成。
原计划 34 个真实 DUT 固定 seed 和追加新随机 5-seed 均已严格通过，合计 39/39；fresh5 结果目录为
`mem_ut/ver/ut/memblock/sim/mother_split_cancel_fresh5_20261029_results`；最终 review 已通过。

第 7 组首轮结果目录：`mem_ut/ver/ut/memblock/sim/mother_basic_matrix_20261029_results/tc_dispatch_real_mmu_sv39_strict_nc_100k`。
ABORTED seeds：`874181294,2113203111,202201584,1595761652,203579979`；未启动 seeds：
`1733858606,1807727296,1728147241,1940125520`。首错 seed 先专项闭环，后续恢复清单按执行规则记录。
debug 复现目录：`mem_ut/ver/ut/memblock/sim/mother_strict_nc_wb_debug_20261029_results`。

波形确认：NC lane1 的输出寄存器从 705.3ns 起持有 ROB0/14；755.3ns 仅顶层发出 redirect，
760.3ns uncacheBuffer/LoadUnit 收到局部 redirect 后，输出寄存器仍未取消旧返回；阻塞解除后进入流水线，780.3ns 产生旧 LDA 写回，
787.3ns 框架报错。源码候选为 `LoadQueueUncache.scala` 中 NC 输出 `AddPipelineReg(..., false.B)`。
NC 允许投机执行，不能把“未等 ROB 队首”直接认作激励错误；也不能仅凭 adapter fatal 认定 RTL。
完整证据与责任复核入口：
`AI_DOC/analysis/source_sv/dispatch_framework_sv/strict_nc_redirect_late_lda_seed846339364_analysis_20261029.md`。
当前没有运行中的仿真；未放宽 checker、未修改 RTL、未启动第 8–12 组。

2026-10-29 两轮独立责任 review 已完成：Galileo 核对框架合同/身份和后端消解，Dewey 核对 RTL
取消链并 review 同拍 replacement 方案。正式记录与具体源码方案位于
`AI_DOC/buglist/rtl/v2/rtl_buglist_2026-W44_20261026.md`。
本轮按规则以 `STOPPED_RTL_CONFIRMED` 停止后续回归，不表示母任务完成。继续条件为明确授权修复
该 NC staging RTL（或接收 RTL owner 修复）；原 seed 专项通过后，才恢复五颗 ABORTED 和四个待替换
NOT_RUN 名额，再推进后续组。仿真仍可组内并发；源码修改仍只在 mother 逐 Bug 顺序执行。

## 其余 14 组原始矩阵

以下均待 BasicTest 阶段结束后执行本轮统一版本回归。集成时的局部专项结果仍保留在前表，
不自动替代这里每组原计划的 10 个 seed。

| tc | ts | cfg | 本轮状态 | 已验收/计划 |
|---|---|---|---|---:|
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `default` | `NOT_RUN` | 0/10 |
| `tc_boundary_addr_reuse_gate` | `virtual_base_sequence` | `tc_boundary_addr_reuse_store_cross8_gate` | `NOT_RUN` | 0/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_l2cache_model` | `NOT_RUN` | 0/10 |
| `tc_dispatch_real_mixed_sta_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_mixed_sta_wb_smoke` | `NOT_RUN` | 0/10 |
| `tc_dispatch_real_mixed_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_mixed_wb_smoke` | `NOT_RUN` | 0/10 |
| `tc_dispatch_real_multi_store_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_multi_store_wb_smoke` | `NOT_RUN` | 0/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_smoke` | `NOT_RUN` | 0/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_smoke_flushsb` | `NOT_RUN` | 0/10 |
| `tc_dispatch_real_store_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_smoke` | `NOT_RUN` | 0/10 |
| `tc_dispatch_real_store_sta_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_sta_wb_smoke` | `NOT_RUN` | 0/10 |
| `tc_dispatch_real_store_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_wb_smoke` | `NOT_RUN` | 0/10 |
| `tc_l2tlb_pbmt_csr_gate` | `virtual_base_sequence` | `tc_l2tlb_pbmt_csr_gate_io` | `NOT_RUN` | 0/10 |
| `tc_l2tlb_pbmt_csr_gate` | `virtual_base_sequence` | `tc_l2tlb_pbmt_csr_gate_nc` | `NOT_RUN` | 0/10 |
| `tc_l2tlb_ppn_reuse_smoke` | `virtual_base_sequence` | `tc_l2tlb_ppn_reuse_smoke` | `NOT_RUN` | 0/10 |

最终交付还需完成独立 fault3 100-seed、统一稳定源码的覆盖率数据库与合并报告、问题/方案总 review。
