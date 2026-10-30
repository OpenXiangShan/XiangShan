# Memblock母任务集成与回归状态（更新至2026-10-31）

## 最新验收结论（2026-10-31，优先于后文历史台账）

- 唯一后续修改工作区：`.codex_mother_integration_20261028`；任务分支`codex/memblock-csr-prefix-fix-20261025`，不是主线；本轮未创建新worktree。
- 最新单Bug实现提交：`50e80d275eb17750df5fb69f021d2ddfd7fd6e26`，修复取消窗口被无关延迟出队阻塞；仅4个SV和7份对应文档，无RTL修改。
- **绕开已确认RTL组的回归子任务：COMPLETE，350/350已严格/clean验收。** 矩阵25组250＋fault3完整10000笔压力100；310条真实DUT，40条原计划软件别名单列。专项与邻接额外记录不增加350分子。
- 原矩阵整体仍为250/260：BasicTest110/120、其他14组140/140。再加fault3压力100，母任务计划360个名额中350个PASS；strict-NC10个名额未通过，不能写成母任务全部PASS或COMPLETE。
- **母任务整体：STOPPED_RTL_CONFIRMED。** 剩余入口为`basicTest / memblock_dispatch_real_smoke_vseq / tc_dispatch_real_mmu_sv39_strict_nc_100k`；原1FAIL、5ABORTED、4NOT_RUN，不计PASS。RTL取消链问题已两轮独立确认，未获RTL coding授权。
- 所有本轮controller及coverage collector均已退出，rc0、remote_unknown_jobs0；没有运行中的仿真，也不需要再重启已完成350项。
- 最终稳定源码覆盖率已生成且独立review PASS：DUT层次`top_tb.U_MEMBLOCK`的line80.88%、condition60.54%、toggle38.56%、FSM9.24%、score47.30%；fcov与branch关闭，不是功能覆盖率闭环结论。
- 2026-10-30完成仿真，23:48 CST完成fault3整批验收；2026-10-31完成覆盖率及独立review。结果目录日期20261030是任务标识，不表示10月31日之前已生成最终报告。

### 最终证据与交付入口

以下均相对mother仓库根目录，完整交付目录为`mem_ut/ver/ut/memblock/sim/mother_final_cancel_nonrtl_pass_coverage_20261030`：

| 产物 | 路径与验收 |
|---|---|
| 本轮问题与方案总review | `AI_DOC/plan/test_framework/review_doc/undo/memblock_mother_regression_problem_solution_review_20261030.md`，保持undo，不因实现完成擅自归档 |
| 新源码矩阵250 | `mem_ut/ver/ut/memblock/sim/mother_post_cancel_matrix_20261030_results`；25组250/250，Boole独立MATRIX_250_INDEPENDENT_REVIEW_PASS |
| fault3原551根1 | `mem_ut/ver/ut/memblock/sim/mother_cancel_dependency_original551576067_20261030_results`；MAIN10000严格PASS |
| fault3最终99 | `mem_ut/ver/ut/memblock/sim/mother_post_cancel_fault3_final99_20261030_results`；summary99PASS，strict_acceptance99PASS，source_commit50e80d275 |
| fault3跨轮总100 | `mem_ut/ver/ut/memblock/sim/mother_post_wip_cov_20261029_results/fault3_final100_20261030/campaign_state.tsv`；最终100/100 COMPLETE，历史错误及联停行保留 |
| 最终350独立回归review | `mem_ut/ver/ut/memblock/sim/mother_post_cancel_fault3_final99_20261030_results/final_350_independent_review.json`；FINAL_350_REGRESSION_INDEPENDENT_REVIEW_PASS，SHA256=`87add3d0867515c409d49e37c34ac5e3ee79c9dc36b47833f4f12c5d2cbc7acd` |
| 最终合并数据库与报告 | 交付目录内`merged.vdb`、聚合测试`accepted350`、`urgReport/dashboard.html`及TXT报告；350 tests/350 testrecords与白名单精确一致 |
| 覆盖率验收及独立review | 交付目录内`coverage_acceptance.json`和`independent_coverage_review.json/.md`；FINAL_350_COVERAGE_INDEPENDENT_REVIEW_PASS，review JSON SHA256=`cefce3f094c7404e52e6931465515edb4df58c4422862423a6004abc175a2ca0` |

URG自身ERROR/WARNING均0；三个远端调用另有12条shell初始化提示，独立review已分类保留，不把完整console描述为零warning。
最终回归、collector与独立review完成前7份文档一直冻结；完成后才进行本次结果更新。已审原文另保存在交付目录`frozen_documents_50e80d275/`并与Git提交逐份核对。冻结gate只证明50e80d275时点，不修改旧receipt/hash来适配新文档；后续重跑必须重新建立绑定，不能直接沿用只认原文档的旧启动门禁。
生产2540项编译输入与两份simv仍按50e80d275验收，无新源码修改；四项已有知识库脏文件及未跟踪`mem_ut/ver/ut/memblock/signal.rc`不修改、不暂存、不提交。只本地提交，未指定私有目标，不向公共origin推送。

### 当前源码矩阵逐项状态

前25个非strict-NC组各10个seed全部PASS；末行strict-NC单独列为未通过，不包含在这250项中。具体seed、原参数和逐日志hash在上述矩阵目录的`manifest.json`及各cfg子目录验收文件中；本表不使用省略的ts或cfg。

| tc | ts | cfg | 结果/类型 |
|---|---|---|---|
| basicTest | memblock_dispatch_manual_control_vseq | csr_sfence_check_store_manual_control | 10/10，真实DUT |
| basicTest | memblock_dispatch_real_smoke_vseq | csr_sfence_check_store_rob_control | 10/10，真实DUT |
| basicTest | memblock_dispatch_real_cancel_reconcile_vseq | tc_dispatch_real_cancel_reconcile_smoke | 10/10，真实DUT |
| basicTest | memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq | tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k | 10/10，真实DUT |
| basicTest | memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq | tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16 | 10/10，真实DUT |
| basicTest | memblock_dispatch_real_smoke_vseq | tc_dispatch_real_mmu_sv39_smoke | 10/10，真实DUT |
| basicTest | memblock_dispatch_real_smoke_vseq | tc_l2tlb_fault_2pct_no_trigger | 矩阵10/10，真实DUT；另有MAIN10000压力100/100 |
| basicTest | memblock_l2tlb_pbmt_response_fault_vseq | tc_l2tlb_pbmt_response_fault | 10/10，真实DUT，保留cfg两UID |
| basicTest | memblock_csr_random_config_vseq | tc_memblock_csr_random_config | 10/10，真实DUT |
| basicTest | memblock_dispatch_real_smoke_vseq | tc_sfence_stress_10k | 10/10，真实DUT |
| basicTest | memblock_dispatch_real_smoke_vseq | tc_sfence_stress_1k | 10/10，真实DUT |
| tc_dispatch_real_smoke | virtual_base_sequence | default | 10/10，真实DUT |
| tc_boundary_addr_reuse_gate | virtual_base_sequence | tc_boundary_addr_reuse_store_cross8_gate | 10/10，软件 |
| tc_dispatch_real_smoke | virtual_base_sequence | tc_dispatch_real_l2cache_model | 10/10，真实DUT |
| tc_dispatch_real_mixed_sta_wb_smoke | virtual_base_sequence | tc_dispatch_real_mixed_sta_wb_smoke | 10/10，真实DUT |
| tc_dispatch_real_mixed_wb_smoke | virtual_base_sequence | tc_dispatch_real_mixed_wb_smoke | 10/10，真实DUT |
| tc_dispatch_real_multi_store_wb_smoke | virtual_base_sequence | tc_dispatch_real_multi_store_wb_smoke | 10/10，真实DUT |
| tc_dispatch_real_smoke | virtual_base_sequence | tc_dispatch_real_smoke | 10/10，真实DUT |
| tc_dispatch_real_smoke | virtual_base_sequence | tc_dispatch_real_smoke_flushsb | 10/10，真实DUT |
| tc_dispatch_real_store_smoke | virtual_base_sequence | tc_dispatch_real_store_smoke | 10/10，真实DUT |
| tc_dispatch_real_store_sta_wb_smoke | virtual_base_sequence | tc_dispatch_real_store_sta_wb_smoke | 10/10，真实DUT |
| tc_dispatch_real_store_wb_smoke | virtual_base_sequence | tc_dispatch_real_store_wb_smoke | 10/10，真实DUT |
| tc_l2tlb_pbmt_csr_gate | virtual_base_sequence | tc_l2tlb_pbmt_csr_gate_io | 10/10，软件 |
| tc_l2tlb_pbmt_csr_gate | virtual_base_sequence | tc_l2tlb_pbmt_csr_gate_nc | 10/10，软件 |
| tc_l2tlb_ppn_reuse_smoke | virtual_base_sequence | tc_l2tlb_ppn_reuse_smoke | 10/10，软件 |
| basicTest | memblock_dispatch_real_smoke_vseq | tc_dispatch_real_mmu_sv39_strict_nc_100k | 未通过，STOPPED_RTL_CONFIRMED；原1FAIL/5ABORTED/4NOT_RUN |

### 后续任务门禁

1. 非strict-NC350项回归、最终覆盖率和问题总review已完成，不再作为待跑任务。所有修复均在已有mother顺序完成、逐Bug专项通过后单独提交，仿真允许并行。
2. 只剩strict-NC RTL问题：取得RTL coding授权后，按原Buglist方案及独立review修复，fresh compile，原seed846339364专项通过后恢复原5个ABORTED及为4个NOT_RUN新构造seed；不得将它们算作本次350已通过。
3. 该组全部通过且在新源码上重验必要范围后，重新形成包含它们的准确覆盖率白名单及最终review；当前报告明确不含该组，不能称为全部母任务覆盖率闭环。
4. 旧CSR工作区仅证据存档，无新必合修复；历史适配工作区不在本轮新建/自动删除范围。不新开工作区、不并行修改源码。

以下章节保留历史阶段事实；其中“当前/尚未/待执行”均指各节当时状态，不覆盖本节最新验收结论。

## Mother

- Worktree: `.codex_mother_integration_20261028`
- Branch: `codex/memblock-csr-prefix-fix-20261025`
- 最近已提交实现：`bba3008ba`（rollback同拍来源绑定，单Bug commit）；前一RM修复`1343dbce3`、PBMT profile `3bd0e9934`、旧SV基线`60dcca7c6`均保留。
  1343修复的2540项编译输入中仅计划内10个SV变化；bba3008修复相对1343另变2个SV。RTL未变，四项无关知识库脏文档未纳入提交。
- 当前执行状态：旧WIP门禁已关闭；原矩阵非strict-NC共25组250/250通过，strict-NC10项延期；25组通过SV基线覆盖率已过滤合并。
  新fault3问题已独立确认为RM并通过方案review。r5 fresh compile及相同临时参数最小真实DUT用例严格PASS，
  原seed1992896882完整10000笔及真实DUT专项/邻接合计21/21严格PASS，四类必要软件专项另计通过；
  本BugTuring最终独立review PASS并已提交；1343版fault3恢复99曾出现新ROLLBACK_XMR并进入`PAUSED_ANALYSIS`，该新Bug现已专项闭环并提交bba3008；
  1343基线矩阵前5组50/50严格通过，第6组因新首错联停9PASS/1ABORTED、0功能FAIL。新rollback责任/方案/最终实现review均PASS，软件1/1及真实DUT20/20专项通过（18邻接加两个原根各10000笔），已提交bba3008ba。
  bba版矩阵25组250/250已全部严格/clean通过，controller rc0退出并获独立review PASS；fault3恢复98出现新cancel-depth首错551576067，已联停收敛为20PASS/1FAIL/19ABORTED/58NOT_RUN、unknown0。原参波形及只读观察均复现并退出，正式review确认测试框架全局deferred依赖过宽；正式方案已review PASS并完成顺序coding，软件四组合+18负例、三个原根各10000笔、8邻接及基础两入口全部通过。统一动态门禁12真实DUT＋1空virtual PASS，全部controller退出。Boole最终独立实现review PASS，随本记录单Bug提交后恢复；尚未恢复批量或收集最终coverage。
  strict-NC单组保持`STOPPED_RTL_CONFIRMED`，母任务全部回归和最终覆盖率仍未完成。
- 工作区状态按 `git status --short` 实时检查；文档提交不改变本轮已编译源码。

### 2026-10-30续接：551576067修复门禁

| 任务 | 当前状态 | 下一步与验收 |
|---|---|---|
| 根因与责任分类 | 已完成，独立review PASS | 证据仍为原波形及只读observer，不重复归因于瞬时staging |
| 正式方案 | Boole独立PLAN_REVIEW_PASS、主agent复核通过，coding/文档/动态验收完成后归档do | `AI_DOC/plan/test_framework/plan/do/memblock_cancel_window_deferred_dependency_formal_plan_20261030.md`；补齐三项资源闭环断言后通过，不跳过门禁 |
| 顺序coding及契约测试 | 三生产文件及本Bug测试task已修改；四组合+18只读负例PASS，Boole独立SOFTWARE_CONTRACT_REVIEW_PASS | r3 fresh compile、controller rc0/unknown0、391ns finish、六项错误0；r2 fixture表数量变化失败保留，仅修fixture不改生产/RM |
| 原seed及邻接验证 | 原551576067、1705742557、1992896882各完整10000笔及8邻接全部严格PASS；基础两入口通过 | 原mode=`mother_cancel_dependency_original551576067_20261030`；邻接结果`mother_cancel_dependency_focused_20261030_results`；统一动态门禁12真实DUT＋1空virtual、六项错误0，所有controller退出、unknown0 |
| 单Bug提交与批量恢复 | 已提交50e80d275；新源码矩阵250及fault3压力100全部完成并独立验收 | 350/350及最终覆盖率已独立review PASS，strict-NC10另计未通过；详见文首最新结论 |

没有新增worktree；50e80d275提交前完成专项门禁，提交后恢复批量，10月31日最终350及覆盖率全部验收，无活动仿真。
最终源码重验清单已经完成：原551当前coverage1＋两个前原根重验2＋旧版本20PASS重验＋19原ABORTED＋58新seed＝fault3总100；另外固定矩阵25组250全部重验通过。没有把旧252/350搬作新版本通过。

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
- 原矩阵BasicTest现为110/120、其他140/140；门禁专项不替代固定原参数10-seed矩阵。
  组8–12已完成，跳过strict-NC组7；下一步其他非strict-NC组及最终统一覆盖率。
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
组10原10seed、MAIN1000/CHECK0已10/10严格PASS，B1在原CSR random矩阵合同中未再出现失败。
组11 `memblock_dispatch_real_smoke_vseq + tc_sfence_stress_10k` 原10seed已严格10/10 PASS，controller已退出、unknown0。
组12同ts、cfg=`tc_sfence_stress_1k`原10seed也已严格10/10 PASS，controller已退出、unknown0。
BasicTest非strict-NC矩阵阶段完成；其余14组从真实DUT的`tc_dispatch_real_smoke / virtual_base_sequence / default`开始。
SV/cfg/RM/RTL不改，不将MAIN2通过说成原1000参数通过。方案/证据见W44 Buglist最新章节。
组8的summary/results、逐日志六项计数、TEST_PASS/finish均已核验，
报告见各组目录`strict_acceptance.json`或`clean_legacy_acceptance.json`；本轮coverage矩阵已验收190 seed，不冒称最终完整覆盖率已完成。

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
- 原始 functional matrix 共 26 组、每组 10 seed；其中 BasicTest 为 12 组。当前 BasicTest 110/120
  已通过，其余 14 组 140/140 完成本轮矩阵验收。不同版本专项 PASS 不代替最终统一源码验收。
- 已修复集成和证据见下面提交表；最新 `c9763bd7d` 的 later-split cancel 已通过定向和真实 DUT 39/39
  专项，不能继续当成待修问题。统一最终版本、fault3 100-seed、覆盖率、总 review 均未完成。
- 原始 cfg/seed 清单目录：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/functional_cfg_cov_20261023_results`。
  保留原归档；当前 strict-NC 恢复状态在第 7 组结果目录的 `campaign_state.tsv`，当前为 `STOPPED_RTL_CONFIRMED`。
- BasicTest真实DUT日志严格要求：一次TEST_PASS、正常`$finish`、无raw UVM ERROR/FATAL，final/caught/demoted
  ERROR/FATAL六项均为0；真实legacy改用一次`TEST CASE PASSED`、对应core完成与ready/RM数量，其他零错误条件不变。
  controller rc=0、result=PASS、全seed PASS、unknown=0且无未完成分类。
  ROB 负向定向的 caught fatal=19 是独立预期，不可当成真实 DUT 零错误验收。
- 不再依赖旧 subagent 会话恢复；独立 review 结论已落盘。重新执行前先读本仓 AGENTS.md、Buglist/
  回归执行规则和适用的 eda-sim skill；涉及 RTL 生成还须读 V2 RTL profile。
- `.codex_csr_prefix_fix_20261025`保留未采用WIP和原始证据；必要B1已合入，不再表示应合未合Bug。历史l2tlb工作区保留。
  不批量合并、不删除脏 worktree，不回到旧工作区并发改代码。当前 mother 分支是任务分支，不是主线。

## Integrated Bug Commits

| Commit | Bug | Mother verification | Source worktree |
|---|---|---|---|
| `50e80d275` | 取消窗口被无关延迟出队阻塞 | 软件四组合+18负例、12真实DUT+1空virtual专项、提交后350/350及最终覆盖率独立review PASS | 只在mother顺序修改，4SV+7文档单Bug提交 |
| `bba3008ba` | Rollback同ROB跨拍来源误报冲突 | 两原根各10000笔及18邻接、软件专项通过；后续同源码最终重验见文首 | 只在mother顺序完成 |
| `1343dbce3` | 跨页首段数据错误终止未建模 | 原seed10000笔及21真实DUT专项、四类软件专项通过；后续同源码最终重验见文首 | 只在mother顺序完成 |
| `3bd0e9934` | PBMT矩阵统一MAIN1000覆盖两UID定向合同 | 原错参带波形仍fatal；shell单测/dry-run及原seed cfg2严格PASS；修正矩阵10/10严格PASS，独立review PASS | 只在mother顺序完成，旧FAIL保留 |
| `60dcca7c6`；源`5a4dc2df4d` | CSR非虚拟映射切换真实HS全SFENCE及C4门禁 | fresh compile；原8+Sv39/PBMT/SFENCE各2共14/14严格PASS；协议/15运输负例单列；最终独立review PASS | 原source WIP保留存档，后续不在其编码 |
| `91d403eb1` | PMA/PMP CSR raw FIFO continuous prefix service | integrated before this continuation; later PBMT integration preserved ticket/revision/prefix semantics | preserved WIP source worktree remains untouched |
| `fb9f04abe` | L2TLB PPN reuse smoke determinism | specialized 22/22 plus adjacent Sv39 1/1 PASS | deleted; branch retained |
| `17e2f2d67` | sparse PPN约束到可路由memory manager，独立于smoke fixture修复 | 当前/隔离compile、software directed、历史4seed、隔离随机20/20 PASS；独立review PASS | 已在mother历史；原隔离目录本次不存在，不以目录存在性判断是否已合入 |
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
| 10 | `basicTest` | `memblock_csr_random_config_vseq` | `tc_memblock_csr_random_config` | `PASS` | 10/10 |
| 11 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_sfence_stress_10k` | `PASS` | 10/10 |
| 12 | `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_sfence_stress_1k` | `PASS` | 10/10 |

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

其余14组functional matrix已140/140干净通过，含真实DUT100和软件40；当前BasicTest110/120，已关闭前六组及组8–12。
原矩阵累计250/260，strict-NC10项延期；fault3 100-seed和最终统一源码覆盖率尚未完成，不能解释为母任务全部完成。
前六组现已在B1集成后的统一SV版本coverage mode复跑60/60通过，见末尾独立表；后续如再修源码，仍须重新评估最终版本验收范围。

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
历史停止点当时没有运行中的仿真，尚未启动第8–12组；当前这五组已完成，见上文最新状态。未放宽checker、未修改RTL。

2026-10-29 两轮独立责任 review 已完成：Galileo 核对框架合同/身份和后端消解，Dewey 核对 RTL
取消链并 review 同拍 replacement 方案。正式记录与具体源码方案位于
`AI_DOC/buglist/rtl/v2/rtl_buglist_2026-W44_20261026.md`。
该组按规则以`STOPPED_RTL_CONFIRMED`延期，不表示母任务完成；用户最新指令已允许绕开此组继续其他组。
该NC组恢复条件为明确授权修复NC staging RTL（或接收RTL owner修复）；原seed专项通过后，才恢复五颗ABORTED
和四个待替换NOT_RUN名额。其他组不再等待此条件；仿真可组内并发，修改仍只在mother逐Bug顺序执行。

## 其余 14 组原始矩阵

BasicTest非strict-NC阶段已完成，下表14组本轮原seed矩阵也已全部通过；不是拿集成时的局部专项替代。

| tc | ts | cfg | 本轮状态 | 已验收/计划 |
|---|---|---|---|---:|
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `default` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_boundary_addr_reuse_gate` | `virtual_base_sequence` | `tc_boundary_addr_reuse_store_cross8_gate` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_l2cache_model` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_dispatch_real_mixed_sta_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_mixed_sta_wb_smoke` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_dispatch_real_mixed_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_mixed_wb_smoke` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_dispatch_real_multi_store_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_multi_store_wb_smoke` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_smoke` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_smoke_flushsb` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_dispatch_real_store_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_smoke` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_dispatch_real_store_sta_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_sta_wb_smoke` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_dispatch_real_store_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_wb_smoke` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_l2tlb_pbmt_csr_gate` | `virtual_base_sequence` | `tc_l2tlb_pbmt_csr_gate_io` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_l2tlb_pbmt_csr_gate` | `virtual_base_sequence` | `tc_l2tlb_pbmt_csr_gate_nc` | `CLEAN_LEGACY_PASS` | 10/10 |
| `tc_l2tlb_ppn_reuse_smoke` | `virtual_base_sequence` | `tc_l2tlb_ppn_reuse_smoke` | `CLEAN_LEGACY_PASS` | 10/10 |

### Legacy与软件别名验收边界

独立review确认其余14组中10组为真实DUT legacy，4组为软件别名：boundary cross8、PBMT CSR IO/NC、PPN reuse。
均使用`TEST CASE PASSED`而不是BasicTest的`TEST_PASS`，不能因marker不同误判，也不能仅凭marker通过。
每日志要求一次成功marker、核心sequence完成、正常finish、raw及final/caught/demoted ERROR/FATAL全0，controller退出且unknown0。
软件四组无预期caught错误，运行结果与真实DUT分开统计；不把软件通过计成DUT有效激励或DUT覆盖贡献，失败不修soft_test。
default及l2cache组各10/10已有`clean_legacy_acceptance.json`及1000笔RM比较/核心完成证据，全部controller退出。
独立`CORE_MAPPING_REVIEW_PASS`已确认14入口完成标志映射。两个mixed组每seed固定Load/Store各一条、共2条，
即使命令保留原MAIN1000也不是1000条随机激励；验收记录明确ready=2、RM比较=2，不能夸大激励规模。
结果工具已补原归档tc/ts/cfg/timeout校验、真实legacy核心完成与ready/RM数量、六种错误计数名称唯一检查；
这些仅修改ignored结果目录工具，不修改TB/DUT，不改变仿真行为；此前已完成六个legacy组60份日志重新验收通过。

### 最终覆盖率的数据隔离准备

当前main coverage数据库曾运行PBMT错参失败，不能直接作为最终全PASS交付。另启动独立仿真mode
`mother_pbmt_pass_only_cov_20261030`，仍在同一个mother worktree、同一SV/TB源码上fresh compile，
使用修正profile的两UID cfg及恢复批次同10 seed，monitor/record开启；结果目录为
`mem_ut/ver/ut/memblock/sim/mother_pbmt_pass_only_cov_20261030_results`。当前状态：`COMPLETE`，fresh compile及10/10严格PASS，
同seed/参数比对通过，unknown0。simv SHA256=`6f3832add78ee25b544b9238f53afcb970ed062949520b8c3a5159a6c941b17b`。
允许此独立mode与原矩阵并行仿真，各mode仍只有一个controller，不修改生产代码、不新增worktree。
后续汇总应排除main数据库中全部旧PBMT测试记录，改用该干净mode通过记录；其他组也必须明确通过清单。
这是覆盖率数据隔离，不是又修一个Bug，未通过前不称覆盖率完成。line/condition/FSM/toggle已开启，fcov仍关闭。
URG单通过记录筛选探针已PASS：必须使用工具`-show availabletests`输出的带数据库限定路径名称，不能只写裸测试名。
探针报告恰好1 test/1 record，位于main结果根目录`urg_single_pass_probe_r2_report`，不把探针覆盖率当作最终矩阵覆盖率。

25组250个通过记录的**本次SV基线覆盖率**现已完成过滤/合并：
`mem_ut/ver/ut/memblock/sim/mother_matrix_pass_coverage_20261030`，有效入口`urgReport_r2/dashboard.html`与`merged_r2.vdb`。
白名单与250份日志/配置/summary hash、工具availabletests和最终250 tests/250 records逐项对应，排除main全部PBMT及fault3记录；
PBMT只取新干净mode。本目录含`selection_manifest.json`、`coverage_acceptance.json`、两编译指纹和2540输入hash。
DUT层次`top_tb.U_MEMBLOCK`：line80.38%、condition59.51%、toggle37.90%、FSM3.55%；未启用branch/fcov。
有效r2使用默认形状检查、无flex/drop、`-strict`拒绝损坏数据库，工具日志无ERROR/WARNING。
首轮`-merge_strict`因编译未开strict_check而警告，保留排查证据但不用作有效入口；不冒称做了严格编译对象校验。
这是25组通过版本的覆盖率基线，不含未完成fault3，也不表示strict-NC已修；若后续修复源码，需重新评估最终稳定版本边界。

### 前六组最终版本coverage补测

复用已经完成PBMT的干净mode=`mother_pbmt_pass_only_cov_20261030`，每组仍用原10seed、MAIN1000/CHECK0及原tc/ts/cfg。
与fault3不同mode并行，但本mode内逐组一个controller，每组最多10seed；源码不改，不新增worktree。
结果根目录：`mem_ut/ver/ut/memblock/sim/mother_final_basic6_cov_20261030_results`。
下表独立记录最终版本coverage，不抹除上文已有PASS及其旧源码边界，重复验证不增加原矩阵250/260分子。
本表已60/60严格PASS，包括原Sv39 seed212889620；本mode controller全部退出、unknown0。

| tc | ts | cfg | 最终coverage状态 | 已验收/计划 |
|---|---|---|---|---:|
| `basicTest` | `memblock_dispatch_manual_control_vseq` | `csr_sfence_check_store_manual_control` | `STRICT_PASS` | 10/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `csr_sfence_check_store_rob_control` | `STRICT_PASS` | 10/10 |
| `basicTest` | `memblock_dispatch_real_cancel_reconcile_vseq` | `tc_dispatch_real_cancel_reconcile_smoke` | `STRICT_PASS` | 10/10 |
| `basicTest` | `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k` | `STRICT_PASS` | 10/10 |
| `basicTest` | `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16` | `STRICT_PASS` | 10/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_dispatch_real_mmu_sv39_smoke` | `STRICT_PASS` | 10/10 |

问题/方案汇总阶段review：`AI_DOC/plan/test_framework/review_doc/undo/memblock_mother_regression_problem_solution_review_20261030.md`。
其中明确已有修复的提交/专项证据、WIP不迁移依据、strict-NC延期和剩余覆盖率/fault3缺口，不冒称母任务全完成。

### fault3最终版本专项

| tc | ts | cfg | 请求数/seed数 | 状态 | 已验收/计划 |
|---|---|---|---|---|---:|
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_l2tlb_fault_2pct_no_trigger` | 每seed10000笔，原100seed | PAUSED_ANALYSIS：新rollback来源冲突 | 1/100 |

原seed来源：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault3_100seed_8e45befc1_20261019_results/seeds.txt`，
SHA256=`c785d5cebc36b850aa31adc1b076ad11aad21a6f18e63afd7ef44a5b7bddd196`，无新增/替换seed。
mother当前cfg与最后历史16-seed通过批次冻结cfg逐字节一致：MAIN10000、CHECK_TRIGGER0、ranges0、PMA/PMP model1。
沿用历史timeout100000000ns，无临时plusarg覆盖；本轮按现行规则monitor/record开启，coverage开启、无波形。
结果目录：`mem_ut/ver/ut/memblock/sim/mother_post_wip_cov_20261029_results/fault3_final100_20261030`。
仍复用未改SV输入的已编译main mode；20个seed并发（EDA节点256核，启动前负载约23），同mode一个controller。
这不是1000笔矩阵组8的重复计数；要求每份日志RM比较摘要10000、正常TEST_PASS/finish、全部六种错误计数唯一且0。
首错仍联停，按责任分类/Buglist/方案review/顺序coding/专项/单Bug提交闭环，不绕过新错误。

本轮首错seed=`1992896882`，602001.100ns UID2401报`tlb_entry_exact_or_range query miss`，vpn0x8ab4e、s2xlate0。
controller已退出：0PASS、1FAIL、19ABORTED、80NOT_RUN，unknown0；不能将19个联停或80个未启动归为功能FAIL。
首错阶段曾等待波形责任确认；现已完成原入口波形复现及独立RM责任review，见下文，不再处于未分类阶段。
其他并行coverage验证仍是未改源码的既定专项，最后组收敛后不再启动新的批量；原seed专项闭环前不恢复fault3。
待确认分析：`AI_DOC/analysis/framework_design/memblock_fault3_cross_page_query_miss_seed1992896882_20261030.md`。
波形专项fresh compile PASS、原现象同602001.1ns/UID2401/VPN复现，1FAIL/unknown0，controller已退出。
NPI确认ROB1/121低段cache corrupt响应后mabuf清未发送高段并转写回，三个LU均无第二页请求；独立D-error line与首PA匹配。
权威trace/断言报告位于debug结果目录`uid2401_npi_window.json`与`uid2401_wave_acceptance.json`；
Turing独立责任分类为`REVIEW_PASS`，已登记W44 RM周文件“跨页Load首段数据错误终止后RM仍强求未执行第二页翻译”。
方案为`AI_DOC/plan/rm_plan/do/memblock_first_split_data_error_termination_fix_plan_20261030.md`。
多轮方案review关闭高页已执行标记的单调生命周期、纯cache/非replay资格、连续消费完成水位及deferred有界重试，Turing最终`REVIEW_PASS`。
已在mother顺序完成首轮coding，fresh compile/最小真实DUT专项及独立实现review进行中；没有修改RTL，也没有恢复fault3批量。
实现独立静态review补严低段几何/同line、首次高页和同拍顺序后PASS；r4 fresh compile与必要软件定向PASS，真实最小用例仍FAIL。
波形进一步定位资格误读无效MSHR选择payload，valid=0时payload不得否决purecache；修正方向独立review通过，r5 fresh compile及最小真实DUT严格PASS。
最小PASS目录`mother_first_split_compact_post_fix_r5_20261030_results`，与修前effective plusargs逐字节一致，
严格日志验收、NPI门禁前后比较及2540项输入快照均已落盘。simv SHA256为`ed06da6e336aab91b015aa1f7254a5deb35b202f3351fdf71d97cc1b5e057d31`。
原seed无临时参数10000笔目录为`mother_first_split_original1992896882_r5_20261030_results`，完整STRICT_PASS；
独立mode=`mother_first_split_pass_only_cov_20261030`重新编译同源码，denied专项目录`mother_first_split_denied_r5_20261030_results`，允许并行仿真不并行修改。
各开发编译失败和r4真实FAIL均保留，不计新增矩阵PASS；原seed10000笔及全部邻接专项已完成，Turing最终review PASS，随本记录单Bug提交。
最小修复必须保留独立错误预测与事件时序身份，不用DUT异常直接兜底。

短小真实DUT修前复现：同tc/ts/cfg、seed26013001，临时固定LH跨4KB、level0、PTE U/R/W/A/D/V有效、corrupt100%，
3498ns UID0查询第二VPN825ce失败，结果`mother_first_split_compact_pre_fix_u_20261030_results`为1FAIL/unknown0、controller已退出。
先前`mother_first_split_compact_pre_fix_20261030_results`的U=0参数只造成合法PF，虽PASS但不计本Bug复现或修复验收。

### 首段错误专项最新验收与后续顺序（2026-10-30）

真实DUT21/21严格PASS：原seed1、LH corrupt1/denied1/clean3、LW3、LD3、Sv393、CSR2、PBMT2、SFENCE2。
每组`strict_acceptance.json`均已核对原入口、summary/results、完成标志、六项错误计数及unknown0；原seedRM比较10000次。
四类必要软件专项provenance/cancel/protocol r2/priority分别PASS，预期caught ERROR/FATAL=2/5、0/19、1/0、3/0；
不计真实DUT严格PASS。协议r2补充admission-settled断言，证明连续消费缺口确实在生产发布分支冻结而非前置早退。

1. 本Bug最终独立review已通过，plan移do、Buglist/状态同步，随本记录完成唯一单Bug commit；不混入四项既有知识库脏文档。
2. fault3恢复99项：19颗原ABORTED原seed，加80颗未与历史seed重复的新seed替换原NOT_RUN，保持原tc/ts/cfg/MAIN10000/timeout1e8/无临时plusarg。
3. 同一修复SV基线上重验25组250项非strict-NC原矩阵；使用独立仿真mode可与fault3并行，各mode仅一controller。
4. 最终非RTL验收350项=310真实DUT+40软件，strict-NC10项继续延期；只从通过清单合并最终稳定输入覆盖率并完成总review。

默认恢复并发20；提高并发的询问未获答复前不按80启动。没有新增worktree或并行修改。旧source仅保全证据，全部必要项已集成。
恢复清单与逐名额替换映射：`mem_ut/ver/ut/memblock/sim/mother_fault3_recovery_plan_20261030`，
含400份历史seed来源/hash、新99 seed清单及25组矩阵manifest；已启动执行，未把计划清单计作PASS。
恢复99结果：`mem_ut/ver/ut/memblock/sim/mother_fault3_recovery99_r5_20261030_results`，mode=`mother_first_split_pass_only_cov_20261030`。
修复后25组矩阵：`mem_ut/ver/ut/memblock/sim/mother_stable_matrix_r5_20261030_results`，mode=`mother_stable_matrix_r5_20261030`；
首组`basicTest / memblock_dispatch_manual_control_vseq / csr_sfence_check_store_manual_control` fresh compile PASS并10/10严格通过，后续组各10并发。
两处`campaign_state.tsv`逐轮/逐组保留状态，原失败和旧250通过记录不覆盖；1343基线当时矩阵50/250、fault3 1/100，共51项验收，专项邻接不替代计划矩阵名额。
随后rollback修复改变2个SV输入；这些旧基线结果不计新SV最终350项，新版最终批量尚未恢复。

### 1343dbce3修复后矩阵逐组状态

以下专门记录新SV版本，不覆盖旧250/250历史结果。每组结果目录为`mother_stable_matrix_r5_20261030_results/<cfg>`；
最终以对应验收JSON及根`campaign_state.tsv`为准。新mode simv SHA256=`d9c0e87813abc6ad2aeeabb0fadaa5ecb3e8b0bf4af3633523f54dbf4b4019bc`。

| tc | ts | cfg | 修复后状态 | 已验收/计划 |
|---|---|---|---|---:|
| `basicTest` | `memblock_dispatch_manual_control_vseq` | `csr_sfence_check_store_manual_control` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `csr_sfence_check_store_rob_control` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_cancel_reconcile_vseq` | `tc_dispatch_real_cancel_reconcile_smoke` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_dispatch_real_mmu_sv39_smoke` | 首错管理联停：9PASS/1ABORTED，0功能FAIL | 0/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_l2tlb_fault_2pct_no_trigger` | 待执行 | 0/10 |
| `basicTest` | `memblock_l2tlb_pbmt_response_fault_vseq` | `tc_l2tlb_pbmt_response_fault` | 待执行 | 0/10 |
| `basicTest` | `memblock_csr_random_config_vseq` | `tc_memblock_csr_random_config` | 待执行 | 0/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_sfence_stress_10k` | 待执行 | 0/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_sfence_stress_1k` | 待执行 | 0/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `default` | 待执行 | 0/10 |
| `tc_boundary_addr_reuse_gate` | `virtual_base_sequence` | `tc_boundary_addr_reuse_store_cross8_gate` | 待执行 | 0/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_l2cache_model` | 待执行 | 0/10 |
| `tc_dispatch_real_mixed_sta_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_mixed_sta_wb_smoke` | 待执行 | 0/10 |
| `tc_dispatch_real_mixed_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_mixed_wb_smoke` | 待执行 | 0/10 |
| `tc_dispatch_real_multi_store_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_multi_store_wb_smoke` | 待执行 | 0/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_smoke` | 待执行 | 0/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_smoke_flushsb` | 待执行 | 0/10 |
| `tc_dispatch_real_store_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_smoke` | 待执行 | 0/10 |
| `tc_dispatch_real_store_sta_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_sta_wb_smoke` | 待执行 | 0/10 |
| `tc_dispatch_real_store_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_wb_smoke` | 待执行 | 0/10 |
| `tc_l2tlb_pbmt_csr_gate` | `virtual_base_sequence` | `tc_l2tlb_pbmt_csr_gate_io` | 待执行 | 0/10 |
| `tc_l2tlb_pbmt_csr_gate` | `virtual_base_sequence` | `tc_l2tlb_pbmt_csr_gate_nc` | 待执行 | 0/10 |
| `tc_l2tlb_ppn_reuse_smoke` | `virtual_base_sequence` | `tc_l2tlb_ppn_reuse_smoke` | 待执行 | 0/10 |

### 新首错：Rollback来源关联冲突

seed1705742557于724655.800ns，`ROLLBACK_XMR cannot correlate rollback output rob=1/126 level=1 sample=144894 conflict=1`。
99项恢复批次最终0PASS/1FAIL/19ABORTED/79NOT_RUN，unknown0；controller已退出，原失败/联停/未启动记录均保留。
并行矩阵controller已收敛，第6组9PASS/1ABORTED（212889620）无功能FAIL；第7–25组未启动，不继续批量或修改生产源码。
原参数带波形专项：`mother_rollback_1705742557_debug_20261030_results`，同724655.8ns/ROB/sample复现，1FAIL/unknown0、controller退出。
分析入口：`AI_DOC/analysis/framework_design/memblock_rollback_provenance_conflict_seed1705742557_20261030.md`。
Turing正式责任review PASS：测试框架跨拍来源误关联，已登记W44测试框架章节；144892 winner4与144894 winner5输出各自正确，旧缓存错误要求跨拍payload一致。
正式方案：`AI_DOC/plan/test_framework/plan/do/memblock_rollback_same_sample_provenance_fix_plan_20261030.md`，Turing独立方案review PASS后已完成coding，不把已修首段错误Bug重新标记失败。
必要专项已全部完成：原1705742557完整10000笔fresh compile与严格验收PASS；原1992896882完整10000笔独立coverage mode fresh compile与严格验收PASS；
Sv393、CSR2、PBMT2、SFENCE2、cancel2、safe2、新随机fault3 MAIN1000五项共18项真实DUT邻接严格通过；软件精确同拍专项另计PASS。
当前Bug相对1343仅一个生产逻辑文件sync package及一个相关软件task两个编译SV变化；未修改RTL/RM/cfg。软件mode含未执行fixture类，核心2540输入一致，不贡献最终coverage。
Turing最终独立`FINAL_IMPLEMENTATION_REVIEW_PASS`，无源码阻塞；20真实DUT、1软件、核心输入及三个simv均独立核验。
专项/独立最终review/单Bug提交完成前不恢复批量；当前新版最终350项尚未验收，不能沿用1343的51项或旧250覆盖率。

### Rollback同拍修复专项台账

本表是当前Bug门禁，不替代最终350项。源码冻结：一个生产逻辑文件及一个相关软件定向文件，Turing静态实现review PASS。
软件协议1/1已通过，六项错误计数全0，不计DUT PASS。真实DUT共20个专项名额（两个原10k根seed加18个邻接）；20/20已严格验收。
原1705742557已越过724655.8ns旧失败点，两次来源均以精确同拍age0成功关联，证据`failure_point_acceptance.json`；不把过点等同完整10000笔PASS。
两个10k目录分别为`mother_rollback_same_sample_original1705742557_20261030_results`和
`mother_rollback_same_sample_first_split_neighbor1992896882_20261030_results`；其他组位于`mother_rollback_same_sample_neighbors_20261030_results/<cfg>`，
其中新随机组子目录为`fault3_fresh5_main1000`。根`campaign_state.tsv`逐组更新，完整入口与seed来源在`manifest.json`。

| tc | ts | cfg | seed范围 | 专项状态 | 已验收/计划 |
|---|---|---|---|---|---:|
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_l2tlb_fault_2pct_no_trigger` | 原1705742557，MAIN10000 | STRICT_PASS，rc0/unknown0，六项错误计数0 | 1/1 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_l2tlb_fault_2pct_no_trigger` | 原1992896882，MAIN10000 | STRICT_PASS，rc0/unknown0，六项错误计数0 | 1/1 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_dispatch_real_mmu_sv39_smoke` | 212889620及原邻接2颗 | STRICT_PASS | 3/3 |
| `basicTest` | `memblock_csr_random_config_vseq` | `tc_memblock_csr_random_config` | 原邻接2颗 | STRICT_PASS | 2/2 |
| `basicTest` | `memblock_l2tlb_pbmt_response_fault_vseq` | `tc_l2tlb_pbmt_response_fault` | 原邻接2颗，保留cfg两UID | STRICT_PASS | 2/2 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_sfence_stress_1k` | 原邻接2颗 | STRICT_PASS | 2/2 |
| `basicTest` | `memblock_dispatch_real_cancel_reconcile_vseq` | `tc_dispatch_real_cancel_reconcile_smoke` | 原矩阵2颗 | STRICT_PASS | 2/2 |
| `basicTest` | `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16` | 原矩阵2颗 | STRICT_PASS | 2/2 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_l2tlb_fault_2pct_no_trigger` | 新随机5颗，MAIN1000 | STRICT_PASS | 5/5 |
最终交付还需完成独立 fault3 100-seed、统一稳定源码的覆盖率数据库与合并报告、问题/方案总 review。

原1705742557完整日志SHA256=`24b07012037609494916f60d7c29d58944ad92e0114aa1f64cd14a8080d39db4`，
`strict_acceptance.json`已落盘；只完成首错过点的旧证据不再代替全seed验收。
最终批量启动/覆盖率artifact独立review发现二进制绑定、精确350项和验收异常状态记录三处缺口；
已补ignored脚本，Turing独立`READINESS_REVIEW_PASS`，三项全部关闭；汇总门禁也已`FINAL_ROLLBACK_IMPLEMENTATION_GATE_PASS`。
未修改生产SV或提前启动批量，不把这些证据工具问题另算DUT/RM Bug；旧source四项快照hash再次全部OK。

### bba3008修复后最终批量台账（最新）

单Bug提交`bba3008baa01b1faf50cc0037053cd54693e6792`已在mother，源码冻结，无新worktree、无RTL修改。
回归前`FINAL_ROLLBACK_IMPLEMENTATION_GATE_PASS`及独立`READINESS_REVIEW_PASS`均通过。
两个原根完整10000笔PASS占最终fault3 100项的2项；18项邻接和1软件专项不占最终350名额。
fault3余98入口：`basicTest / memblock_dispatch_real_smoke_vseq / tc_l2tlb_fault_2pct_no_trigger`，MAIN10000、timeout1e8、无临时plusarg；
最新19颗ABORTED原seed＋79颗未重复新seed均在`mother_rollback_same_sample_gate_fixture_20261030/recovery98_manifest.json`。
结果目录`mem_ut/ver/ut/memblock/sim/mother_post_rollback_fault3_recovery98_20261030_results`，jobs20、当前运行中。
其跨轮`campaign_state.tsv`保留在`mother_post_wip_cov_20261029_results/fault3_final100_20261030`，开始记录为2PASS/98剩余。

矩阵结果根目录`mem_ut/ver/ut/memblock/sim/mother_post_rollback_matrix_20261030_results`，根manifest及campaign逐组写入实际入口、seed、源码commit及失败原因；
共25组250项，含210真实DUT与40原计划软件用例。仅执行这些既有软件别名，不扩展修复无关soft_test。
下表为当前已验收状态：25组250/250通过，controller rc0退出；每组strict/clean验收及根campaign均已落盘。
其中210真实DUT＋40软件，两个原10k根seed另计；Turing独立`MATRIX_250_INDEPENDENT_REVIEW_PASS`，不等于余98或最终coverage已完成。

| tc | ts | cfg | bba3008状态 | 已验收/计划 |
|---|---|---|---|---:|
| `basicTest` | `memblock_dispatch_manual_control_vseq` | `csr_sfence_check_store_manual_control` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `csr_sfence_check_store_rob_control` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_cancel_reconcile_vseq` | `tc_dispatch_real_cancel_reconcile_smoke` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_dispatch_real_mmu_sv39_smoke` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_l2tlb_fault_2pct_no_trigger` | STRICT_PASS（矩阵原10颗、MAIN1000） | 10/10 |
| `basicTest` | `memblock_l2tlb_pbmt_response_fault_vseq` | `tc_l2tlb_pbmt_response_fault` | STRICT_PASS（保留cfg2） | 10/10 |
| `basicTest` | `memblock_csr_random_config_vseq` | `tc_memblock_csr_random_config` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_sfence_stress_10k` | STRICT_PASS | 10/10 |
| `basicTest` | `memblock_dispatch_real_smoke_vseq` | `tc_sfence_stress_1k` | STRICT_PASS | 10/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `default` | CLEAN_LEGACY_PASS | 10/10 |
| `tc_boundary_addr_reuse_gate` | `virtual_base_sequence` | `tc_boundary_addr_reuse_store_cross8_gate` | CLEAN_LEGACY_PASS（软件） | 10/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_l2cache_model` | CLEAN_LEGACY_PASS | 10/10 |
| `tc_dispatch_real_mixed_sta_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_mixed_sta_wb_smoke` | CLEAN_LEGACY_PASS | 10/10 |
| `tc_dispatch_real_mixed_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_mixed_wb_smoke` | CLEAN_LEGACY_PASS | 10/10 |
| `tc_dispatch_real_multi_store_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_multi_store_wb_smoke` | CLEAN_LEGACY_PASS | 10/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_smoke` | CLEAN_LEGACY_PASS | 10/10 |
| `tc_dispatch_real_smoke` | `virtual_base_sequence` | `tc_dispatch_real_smoke_flushsb` | CLEAN_LEGACY_PASS | 10/10 |
| `tc_dispatch_real_store_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_smoke` | CLEAN_LEGACY_PASS | 10/10 |
| `tc_dispatch_real_store_sta_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_sta_wb_smoke` | CLEAN_LEGACY_PASS | 10/10 |
| `tc_dispatch_real_store_wb_smoke` | `virtual_base_sequence` | `tc_dispatch_real_store_wb_smoke` | CLEAN_LEGACY_PASS | 10/10 |
| `tc_l2tlb_pbmt_csr_gate` | `virtual_base_sequence` | `tc_l2tlb_pbmt_csr_gate_io` | CLEAN_LEGACY_PASS（软件） | 10/10 |
| `tc_l2tlb_pbmt_csr_gate` | `virtual_base_sequence` | `tc_l2tlb_pbmt_csr_gate_nc` | CLEAN_LEGACY_PASS（软件） | 10/10 |
| `tc_l2tlb_ppn_reuse_smoke` | `virtual_base_sequence` | `tc_l2tlb_ppn_reuse_smoke` | CLEAN_LEGACY_PASS（软件） | 10/10 |

最终PASS白名单应精确350条＝310真实DUT＋40软件，strict-NC10继续`STOPPED_RTL_CONFIRMED`，不计PASS。
最终覆盖率尚未生成；旧`mother_matrix_pass_coverage_20261030`不能代替新版本。仅当新350项全验收才执行最终collector及独立报告review。
当前只本地提交：没有指定私有push目标，不向公共upstream origin上传。

2026-10-30矩阵独立复核：250份日志/六计数/正常finish、25组成对campaign状态、精确入口/seed/参数/冻结cfg、2540输入与bba提交/已验二进制绑定全部PASS。
fault3 98条恢复队列已结束：20PASS/1FAIL/19ABORTED/58NOT_RUN，unknown0；20条controller PASS尚未完成整批严格验收，不加入252/350最终验收数。
新首错seed551576067在411428.500ns报local cancel snapshot depth15>14；原参数独立debug fresh compile已启动。
分析入口：`AI_DOC/analysis/framework_design/memblock_cancel_snapshot_depth_seed551576067_20261030.md`。
此段为首错时历史状态，彼时只做原seed复现及责任分析；最新修复及验证见文首551576067门禁表。不修改深度、不放宽检查、不新建worktree；单Bug提交前不继续批量。

最新责任门禁：原参波形/observer均同411428.5ns复现且退出，469443条关键生产事件hash完全相同。
record1658已有anchor/scan/sw2/6；旧UID1679等待STD使deferred FIFO不空，全局门禁阻止当前record收口，local目标快照82232之后逐拍积压至15。
Turing正式`RESPONSIBILITY_REVIEW_PASS`，确认测试框架责任；已登记W44 Buglist。责任确认时方案待review、未coding，此为历史状态。
当前cancel生产修复已完成，12真实DUT＋1空virtual及软件契约统一动态门禁PASS，Boole最终独立review PASS，随本记录单Bug提交；不能把历史“仍未修复”作为当前结论。

覆盖率交付工具核对：旧基线实际数据库在sim同级`mother_matrix_pass_coverage_20261030.vdb`，r2聚合测试名为`merged_r2`，不是旧README所写的目录内`merged_r2.vdb`；数据库未丢失。
本机URG help和实际单记录探针确认`-dbname 目录/测试名`语义；最终collector已改用`<输出>/merged/accepted350`并检查`<输出>/merged.vdb`及URG读回唯一聚合测试。
探针不计作最终350项或最终coverage，旧目录仍只是旧SV历史基线。
