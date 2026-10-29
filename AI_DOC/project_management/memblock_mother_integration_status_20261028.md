# Memblock mother integration status - 2026-10-30

## Mother

- Worktree: `.codex_mother_integration_20261028`
- Branch: `codex/memblock-csr-prefix-fix-20261025`
- 当前母分支实现提交：`3bd0e9934`（PBMT回归profile修复），SV/TB/DUT输入仍为`60dcca7c6`的B1集成版本；源B1为5a4dc2df4d。
- 当前执行状态：旧WIP门禁已随60dcca7c6关闭；非strict-NC覆盖率回归已恢复，当前组10运行中。
  strict-NC单组保持`STOPPED_RTL_CONFIRMED`，母任务全部回归和最终覆盖率仍未完成。
- 工作区状态按 `git status --short` 实时检查；文档提交不改变本轮已编译源码。

## 续接入口（2026-10-29）

**最新用户指令覆盖下文旧恢复顺序**：先审计 source 剩余内容；必要修复在既有 source worktree
逐项完成专项、独立提交，再合入 mother 并复测；不需要的给出证据。该门禁全部完成后，mother
批量回归先跳过 strict-NC 已确认 RTL Bug 的组，执行其他组；不等待该 RTL 修改授权才推进其他组。
不新建 worktree；仿真允许并发，源码修改和集成必须顺序。审计权威台账：
`AI_DOC/analysis/framework_design/memblock_csr_prefix_wip_audit_20261029.md`。
下文 strict-NC 原 seed/恢复列表继续保留，但不代表可跳过旧 WIP 门禁直接恢复其他批次。

### 本轮门禁结论（2026-10-29）

- 剩余WIP已逐文件审计：23个原跟踪源码/测试文件中3个净零，另4个原未跟踪测试文件，全部有必要性处置。
  A0/FIFO/prefix已有集成不重复合；A1旧过滤及A2–A7额外机制不采用，证据和原WIP完整保留。
- 唯一必要新修复B1：CSR SATP映射切换缺真实HS全SFENCE完成门禁，源单Bug`5a4dc2df4d`，精确合入mother，
  保留a13d/c976/91d/prefix/current-live，不引ledger或RTL修改，没有新建worktree。
- mother新编译后真实DUT14/14严格PASS（原8＋Sv39/PBMT safe/SFENCE各2），协议及15运输负例单列PASS；
  独立`FINAL_DYNAMIC_REVIEW_PASS`，全部controller退出、unknown0；随本提交落地关闭旧WIP门禁。
- 母专项目录：`mem_ut/ver/ut/memblock/sim/mother_csr_hs_fence_gate_20261029_results`，含验收JSON、2540项实际输入及六批结果。
  source旧Sv39邻接FAIL/ABORTED保留，独立确认为缺母已修a13d；母本轮相同seed和参数再次PASS。
- 原矩阵BasicTest现为80/120、其他0/140；门禁专项不替代固定原参数10-seed矩阵。
  下一步组8–12，跳过strict-NC组7；随后其他非strict-NC组及最终统一覆盖率。
- source保持证据存档，不表示仍有应合未合Bug。后续修改只在mother顺序完成；仿真可并行。
  当前唯一远端为公共上游origin，未指定任务推送目标前只本地提交。

### 门禁后批量恢复

母集成提交`60dcca7c6`已落地，旧WIP门禁关闭；从组8起依次恢复的新coverage mode为
`mother_post_wip_cov_20261029`，原10 seed并发、MAIN1000/CHECK_TRIGGER0/timeout1000000000ns。
结果根目录：`mem_ut/ver/ut/memblock/sim/mother_post_wip_cov_20261029_results`；
`campaign_state.tsv`逐组维护，组7仍延期不运行。新coverage编译已PASS，组8原10 seed已严格10/10 PASS，
组9旧错参首轮曾联停收敛：8 FAIL、2 NOT_RUN、unknown0，均为发送前`PBMT RF cfg snapshot does not match directed contract`。
现已确认任务参数/矩阵profile缺陷：当前PBMT固定B/C两UID，统一MAIN1000覆盖了正确cfg2。
原seed原MAIN1000带波形仍201ns fatal；只修两个shell后，单元和原seed cfg2正向严格PASS，独立实现review PASS。
本Bug已单独提交`3bd0e9934`，组9修正profile后的新批次10/10严格PASS：原8FAIL加新26012919/26012920，
原2个NOT_RUN名额已替换，原失败归档保留。所有有效plusargs相比旧批次仅MAIN1000→2不同。
现启动组10原10seed，MAIN1000/CHECK0恢复普通随机合同；组11–12尚未启动。
SV/cfg/RM/RTL不改，不将MAIN2通过说成原1000参数通过。方案/证据见W44 Buglist最新章节。
组8的summary/results、逐日志六项计数、TEST_PASS/finish均已核验，
报告见各组目录`strict_acceptance.json`；本轮coverage矩阵已验收20 seed，不冒称最终完整覆盖率已完成。

后续继续母任务时，直接进入以下 worktree，不创建新的 Bug worktree：

```bash
cd /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028
git status --short
git log -1 --oneline
```

历史续接注记（非当前状态）：早前记录前HEAD为`027a35355`、生产源码基线c976，彼时仅补文档且未启动仿真。
本轮B1集成与验收已在上文更新。后续源码修改必须在本 mother worktree 中逐个问题顺序完成；不同 cfg
或 seed 的仿真可以并发，但同一 `mode` 只能有一个 controller。

以下为strict-NC单组的历史停止点，不代表母任务其他组必须停止：

```text
status=STOPPED_RTL_CONFIRMED
bug=NC 返回跨 redirect 后仍产生旧 LDA 写回
tc=basicTest
ts=memblock_dispatch_real_smoke_vseq
cfg=tc_dispatch_real_mmu_sv39_strict_nc_100k
root_seed=846339364
error=787.300ns INT_WB_ATTACH no active uid for ROB key=0/14
```

该问题已经完成两轮责任 review、Buglist 登记和方案 review，但**尚未取得 RTL coding 授权，未修改 RTL，
不能把“继续”理解为 RTL 授权，不能把该组记作PASS**。最新用户已授权门禁关闭后绕开该组恢复其他组。
只有重跑strict-NC本组时，才需先获得授权或接收 RTL owner 修复，按 RTL 生成规则处理
`src/main/scala/xiangshan/mem/lsqueue/LoadQueueUncache.scala` 的 NC 输出 staging，重新生成 RTL，
使用新 `mode` 编译；不能复用旧的 `mother_strict_nc_wb_debug_20261029` simv。

strict-NC本组获得修复后的历史执行顺序（当前其他组恢复顺序以上文最新指令为准）：

1. 原始 seed `846339364` 专项编译仿真，严格确认通过。
2. 重新执行第 7 组恢复批次：原 `ABORTED` seed
   `874181294,2113203111,202201584,1595761652,203579979` 必须重跑；原 `NOT_RUN`
   seed `1733858606,1807727296,1728147241,1940125520` 不重跑，重新构造 4 个不重复随机 seed。
3. 第 7 组 10/10 收敛后，按第 8–12 组顺序推进 BasicTest，每组原始 10 seed 并发执行。
4. BasicTest 12 组全部收敛后，执行其余 14 组 functional matrix，再执行 fault3 100-seed 专项。
5. 在最终稳定源码上重新编译 coverage 版本，收集并合并覆盖率，完成总问题/方案 review。

上述BasicTest批次保留tc/ts/cfg/monitor/record、timeout1000000000ns及CHECK_TRIGGER0。
普通随机项保留MAIN1000；**PBMT组9例外使用fixed_cfg，保留当前cfg两UID，不追加MAIN覆盖**。
旧统一MAIN1000是本轮已确认的参数路由Bug，原失败记录保留，不能继续把它作为PBMT恢复前置。
其余 14 组必须使用下表各自的 tc/ts/cfg 和原始归档参数，不能全部改成 basicTest；fault3 也需先核对原专项记录。
恢复时必须为新空结果目录，使用显式 `--seeds`；保留每轮 `campaign_state.tsv`，不得直接使用
`failed_seeds.txt` 混合恢复。完整 Bug 证据和源码级方案见：

- `AI_DOC/buglist/rtl/v2/rtl_buglist_2026-W44_20261026.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/strict_nc_redirect_late_lda_seed846339364_analysis_20261029.md`
- `AI_DOC/plan/test_framework/plan/do/memblock_redirect_later_split_cancel_integration_plan_20261029.md`

不修复无关 `soft_test`；不新增 worktree；每个实际修复问题仍需单独专项验证并单独 Git commit。

### 续接时的验收边界

- 母任务目标仍为：逐个修复本轮失败并单 Bug 提交，完成全部回归、Bug 分析/方案记录、总 review，
  最终交付所有用例通过后的完整覆盖率目录。本轮完成B1门禁不表示整个母任务完成，RTL仍无coding授权。
- 原始 functional matrix 共 26 组、每组 10 seed；其中 BasicTest 为 12 组。当前 BasicTest 80/120
  已通过，其余 14 组 0/140 完成本轮矩阵验收。不同版本专项 PASS 不代替最终统一源码验收。
- 已修复集成和证据见下面提交表；最新 `c9763bd7d` 的 later-split cancel 已通过定向和真实 DUT 39/39
  专项，不能继续当成待修问题。统一最终版本、fault3 100-seed、覆盖率、总 review 均未完成。
- 原始 cfg/seed 清单目录：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/functional_cfg_cov_20261023_results`。
  保留原归档；当前 strict-NC 恢复状态在第 7 组结果目录的 `campaign_state.tsv`，当前为 `STOPPED_RTL_CONFIRMED`。
- 真实 DUT 每个日志严格要求：一次 TEST_PASS、正常 `$finish`、无 raw UVM ERROR/FATAL，final/caught/demoted
  ERROR/FATAL 六项均为 0；controller rc=0、result=PASS、全 seed PASS、unknown=0 且无未完成分类。
  ROB 负向定向的 caught fatal=19 是独立预期，不可当成真实 DUT 零错误验收。
- 不再依赖旧 subagent 会话恢复；独立 review 结论已落盘。重新执行前先读本仓 AGENTS.md、Buglist/
  回归执行规则和适用的 eda-sim skill；涉及 RTL 生成还须读 V2 RTL profile。
- `.codex_csr_prefix_fix_20261025`保留未采用WIP和原始证据；必要B1已合入，不再表示应合未合Bug。历史l2tlb工作区保留。
  不批量合并、不删除脏 worktree，不回到旧工作区并发改代码。当前 mother 分支是任务分支，不是主线。

## Integrated Bug Commits

| Commit | Bug | Mother verification | Source worktree |
|---|---|---|---|
| `60dcca7c6`；源`5a4dc2df4d` | CSR非虚拟映射切换真实HS全SFENCE及C4门禁 | fresh compile；原8+Sv39/PBMT/SFENCE各2共14/14严格PASS；协议/15运输负例单列；最终独立review PASS | 原source WIP保留存档，后续不在其编码 |
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

## 保留工作区（不是待合Bug清单）

- `.codex_csr_prefix_fix_20261025`：prefix及必要B1已合入mother，逐文件不迁移证据已review。
  不再承接编码；暂保留原始WIP/仿真/波形绝对路径作证据，不能删除后让Buglist链接失效。
- CSR scalar cfg 前置修复已合入 mother；后续 CSR scalar 回归和修复只在 mother worktree 顺序执行。
- `.codex_l2tlb_stage_verify`, `_r3`, `_r4`, `_r7`, `_r8`: dirty historical adaptation worktrees; not part of the
  current Bug integration and therefore not deleted.
- 历史状态列过`.codex_sparse_ppn_isolation_20261025`，本轮实际目录/`git worktree list`均不存在，不计现存工作区。
  不能仅因路径消失推断补丁已合或过时；必要PPN修复以本文件Integrated Bug Commits的提交/专项证据为准。

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
| 8 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_l2tlb_fault_2pct_no_trigger` | `PASS` | 10/10 |
| 9 | `basicTest` | `memblock_l2tlb_pbmt_response_fault_vseq` | `tc_l2tlb_pbmt_response_fault` | `PASS r2 fixed_cfg`；旧错参失败保留 | 10/10 |
| 10 | `basicTest` | `memblock_csr_random_config_vseq` | `tc_memblock_csr_random_config` | `RUNNING` | 0/10 |
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
提交为`c9763bd7d`；第7组strict-NC仍因独立RTL问题延期，不是later-split cancel未修，也不阻塞已授权的其他组。

其余14组functional matrix、fault3 100-seed专项和最终完整覆盖率尚未完成。当前BasicTest80/120，已关闭前六组及组8/9，
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
