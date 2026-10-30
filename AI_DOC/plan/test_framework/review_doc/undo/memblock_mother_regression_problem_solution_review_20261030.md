# 母任务回归问题与方案汇总 Review

创建日期：2026-10-30；最终结果更新：2026-10-31。状态：**非strict-NC350项回归与覆盖率验收完成，母任务整体仍为STOPPED_RTL_CONFIRMED**。本文汇总已落盘的专项review与本轮集成/仿真证据，
不替代每个Bug的源码级implementation review，不声称对全部历史函数重新做了一轮源码review。

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应对象或落点 | 使用场景/示例 |
|---|---|---|---|
| mother worktree | 本轮唯一后续修改和集成工作区 | `.codex_mother_integration_20261028` | 源工作区必要Bug专项通过后精确合入此处 |
| WIP | 未完成独立提交/验收的混合改动，不等于必合修复 | 旧CSR source快照 | A2–A7只服务未采用机制，保留证据而不迁移 |
| strict PASS | BasicTest零错误、正常结束及controller退出的联合验收 | `strict_acceptance.json` | caught fatal非0的负向专项不能算真实DUT严格通过 |
| clean legacy PASS | 旧testcase的零错误验收，使用不同完成标志 | `clean_legacy_acceptance.json` | `TEST CASE PASSED`加核心sequence完成，不要求`TEST_PASS` |
| software-only | 不通过真实DUT接口发送本测试请求的软件自测 | boundary、PBMT CSR IO/NC、PPN四组别名 | 四组40 seed通过不能宣称DUT40次有效激励 |
| profile | 回归脚本选择cfg目录和是否覆盖请求数的规则 | `fixed_cfg` | PBMT两条定向事务保留cfg计数2，不注入MAIN1000 |
| prefix | 按顺序已处理的连续事件范围 | CSR raw FIFO/prefix模型 | 不能越过未消费的CSR写入宣告屏障完成 |
| epoch/token | 区分同UID重发实例及TLB应答实例的身份 | later-split状态与TLB provenance | redirect后旧实例不能继续完成新实例 |
| coverage mode | 独立编译/运行/覆盖率产物命名空间，不是Git工作区 | 两个mother仿真mode | 允许并行仿真，但不并行改代码 |

## 2. 审计门禁与必要性结论

权威逐文件审计：`AI_DOC/analysis/framework_design/memblock_csr_prefix_wip_audit_20261029.md`。
23个原跟踪源码/测试文件中20个净改、3个净零；另4个原未跟踪测试文件及61个文档/工具均已有处置依据。
必要新修复仅B1：CSR非虚拟SATP映射切换没有等待真实HS全SFENCE完成。源单Bug`5a4dc2df4d`已经精确合入
mother `60dcca7c6`，新编译后真实DUT14/14严格通过、协议与运输负向专项单列通过，独立最终review通过。

A0/FIFO/prefix已集成部分不重复合并；旧A1过滤候选全专项7/8，不采用；A2–A7依赖未采用的residency/deferred/
capture等机制，或无mother独立失败证据，逐文件证据关闭而不迁移。不能把这些存档文件说成“仍有应合未合Bug”。
source只保留证据，不承接后续编码；2026-10-30重新核对快照四项SHA256全部一致。
历史适配工作区不在本轮必要Bug集成范围，未擅自删除；本轮没有新建Git worktree。

## 3. 已修复问题、方案与提交

以下是已完成专项的集成索引；详细控制流、方案对齐、代码片段及独立review见各条所引文档。

| 问题 | 修改前与根因 | 修改后方案与保留边界 | mother提交 | 已有验证证据 |
|---|---|---|---|---|
| PMA/PMP CSR raw FIFO处理不及时 | CSR写入积在raw FIFO，模型与实际CSR状态不同步 | 持续消费有效顺序前缀，保留ticket/revision及原子消费语义 | `91d403eb1` | 原FIFO专项与后续CSR/PBMT集成验证；详见RM FIFO implementation review |
| PPN reuse smoke不确定 | 软件定向fixture依赖不稳定的随机构造 | 收紧该已有定向构造，不放宽生产匹配 | `fb9f04abe` | 22/22及Sv39邻接1/1；详见PPN smoke determinism review |
| sparse PPN落入无manager路由空洞 | ranges0旧全空间hash可生成APLIC粗范围中的真实decode空洞，DUT请求等待不到响应 | 生成端确定性限制到可路由外部memory范围，保留hash/level对齐及ranges1路径，不加fallback responder | `17e2f2d67` | 当前/隔离compile、software directed、历史4seed、隔离随机20/20 PASS，独立review PASS；与前项fixture修复分开 |
| Sv39 later-split异常预期不一致 | 拆分返回的来源/样本优先级不完整，可能选到错误上下文 | 按实际later-split provenance解析异常，保留严格异常向量比较 | `a13d10814` | 原seed、已知/随机35、closure10及邻接/补充专项；详见Sv39 RM review |
| PBMT response-fault生产审计非原子 | B/C阶段、CSR前置及TLB请求/完成依据可能不一致 | 两独立requestor、两真实token、阶段/CSR/完成联合审计；不夹带旧软件候选 | `5c131a18d` | 真实44/44 strict、软件NC/IO20/20 clean；详见production integration review |
| CSR scalar独立cfg遗漏前置 | cfg不继承其他preset，漏关闭L2TLB level-weight | 四个独立cfg显式补前置，不修改RTL | `9a2df3d7e` | 四项dry-run及四个原10k seed严格通过 |
| CSR redirect保留屏障前缀断言不正确 | redirect保留owner时前缀边界处理不匹配 | 保留owner身份与有效前缀边界，维持消费一致性 | `4948d291e` | prefix定向及四CSR scalar原10k seed严格通过 |
| redirect遗漏later-split旧记录取消 | 集成helper未调用现有取消逻辑，旧完成可遗留 | redirect集成恢复旧epoch取消；先检查前置再修改公共状态 | `c9763bd7d` | 修前确定性FAIL，修后定向及真实39/39严格通过，独立review通过 |
| CSR映射切换缺真实SFENCE完成 | target/PMP后直接进入candidate；CSR changed/inflight处理不等于DUT storage失效，未发送并等待真实全SFENCE；不混同未采用的A1删模型候选 | 限非虚拟special映射切换，目标CSR稳定后真实HS全SFENCE，等同owner/reset的C0/C4再放行 | `60dcca7c6` | 源12/12、母14/14严格通过及两类负向专项；独立最终review通过 |
| PBMT矩阵请求数路由错误 | 统一MAIN1000覆盖当前两UID定向cfg，发送前合同检查fatal | 新增fixed_cfg路由，仅此组保留cfg2；不改SV/cfg/RM/checker | `3bd0e9934` | 原MAIN1000波形仍复现拒绝；单测/dry-run、原seed cfg2、恢复10/10严格通过 |
| 跨页Load首段数据错误提前终止漏建模 | RM先要求全部页翻译，DUT首页corrupt合法取消高页时先query miss | 固定首段身份/实际S2与连续消费证明，跨页前按独立错误形成严格异常；不造高页、不改RTL | `1343dbce3` | 原seed10000笔及21/21真实DUT、四类软件专项PASS，Turing最终实现review PASS |
| Rollback来源缓存跨拍误报冲突 | 不同拍同ROB/level的合法winner4→5变化被当作同事件payload冲突 | 精确同sample绑定，保留完整ROB/level及同拍重复证明冲突检查；无本拍证明仍拒绝 | `bba3008ba` | 两个原10000笔根seed及18邻接，20/20真实DUT严格PASS；软件1/1，Turing最终实现及恢复readiness均PASS |
| 取消窗口被无关延迟出队阻塞 | 旧store等STD真实WB使全局FIFO不空，阻止无关record冻结，目标快照积到15超过14 | 唯一LSQ handler只读顺推deferred FIFO覆盖区间，相关或不明身份仍阻塞，无关live owner不阻塞；保留STD屏障、完整raw原子性及严格对账 | `50e80d275` | 四软件组合+18只读负例、12真实DUT+1空virtual专项通过；提交后同源码350/350及最终覆盖率独立review PASS |

主要专项review入口：

- `AI_DOC/plan/rm_review_doc/undo/memblock_pma_pmp_raw_csr_fifo_implementation_review_20261027.md`
- `AI_DOC/plan/rm_review_doc/undo/memblock_rm_sv39_later_split_uncache_fault_priority_implementation_review_20261026.md`
- `AI_DOC/plan/test_framework/review_doc/undo/memblock_l2tlb_ppn_reuse_smoke_determinism_implementation_review_20261025.md`
- `AI_DOC/plan/test_framework/review_doc/undo/memblock_sparse_ppn_manager_reachability_implementation_review_20261025.md`
- `AI_DOC/plan/test_framework/review_doc/undo/memblock_l2tlb_pbmt_response_fault_production_integration_review_20261028.md`
- `AI_DOC/plan/test_framework/review_doc/undo/memblock_csr_scalar_functional_matrix_integration_fix_implementation_review_20261025.md`
- `AI_DOC/plan/test_framework/review_doc/undo/memblock_csr_redirect_preserved_barrier_prefix_invariant_fix_implementation_review_round3_20261027.md`
- `AI_DOC/plan/test_framework/review_doc/undo/memblock_redirect_later_split_cancel_implementation_review_20261029.md`
- `AI_DOC/plan/test_framework/review_doc/undo/memblock_csr_hs_mapping_fence_gate_review_20261029.md`
- `AI_DOC/plan/test_framework/review_doc/undo/memblock_pbmt_matrix_fixed_cfg_profile_review_20261030.md`
- `AI_DOC/plan/test_framework/review_doc/undo/memblock_rollback_same_sample_provenance_implementation_review_20261030.md`
- `AI_DOC/plan/rm_review_doc/undo/memblock_first_split_data_error_termination_implementation_review_20261030.md`
- `AI_DOC/plan/test_framework/review_doc/undo/memblock_cancel_window_deferred_dependency_implementation_review_20261030.md`

## 4. 仍未修复的RTL问题

strict-NC组保持`STOPPED_RTL_CONFIRMED`，不是通过，也不再阻塞用户已授权的其他组。
`basicTest / memblock_dispatch_real_smoke_vseq / tc_dispatch_real_mmu_sv39_strict_nc_100k / seed846339364`：
NC返回已进入输出暂存寄存器，redirect后旧返回未被取消，之后仍产生ROB0/14的LDA写回。
框架787.300ns报无活动UID是传播结果，首个RTL取消链异常及后端消解分析见W44 RTL Buglist。
已有两轮独立责任review及源码级方案，无RTL coding授权，未改RTL、未屏蔽checker。
原1 FAIL、5 ABORTED、4 NOT_RUN全部保留，10个名额均不记PASS。

## 5. 回归证据与当前缺口

### 5.1 最终350项及覆盖率结论（2026-10-31，当前权威状态）

生产实现提交`50e80d275eb17750df5fb69f021d2ddfd7fd6e26`后，2540项核心输入冻结不变，两个coverage mode并行仿真、每mode唯一controller；没有新建worktree或并行源码修改。最终清单350条全部重验通过，不复用旧源码PASS：

| 范围 | 数量/类型 | 证据目录（相对mother的`mem_ut/ver/ut/memblock/sim`） |
|---|---|---|
| 原矩阵非strict-NC25组 | 250/250；210真实DUT＋40原计划软件别名 | `mother_post_cancel_matrix_20261030_results`，独立MATRIX_250_INDEPENDENT_REVIEW_PASS |
| 原551576067根 | 1/1；真实DUT、MAIN10000 | `mother_cancel_dependency_original551576067_20261030_results` |
| fault3最终99 | 99/99；真实DUT、MAIN10000 | `mother_post_cancel_fault3_final99_20261030_results`，strict_acceptance99及source_commit50e80d275 |
| 总有效白名单 | 350/350；310真实DUT＋40软件 | fault99目录`final_350_independent_review.json`；FINAL_350_REGRESSION_INDEPENDENT_REVIEW_PASS |
| 未通过且排除 | strict-NC10名额：1FAIL/5ABORTED/4NOT_RUN | 第4节已确认RTL，STOPPED_RTL_CONFIRMED，不计PASS |

fault100逐项保留原`basicTest / memblock_dispatch_real_smoke_vseq / tc_l2tlb_fault_2pct_no_trigger`入口、MAIN10000、timeout100000000ns及无临时覆盖参数；每项10000次RM比较、正常完成、六项最终/捕获/降级ERROR/FATAL均0、rc0/unknown0。最终99由旧原根重验2＋旧源码20PASS重验＋19ABORTED原seed＋58新seed组成；原551单列1，不重复计数。

Boole最终独立回归review JSON SHA256：`87add3d0867515c409d49e37c34ac5e3ee79c9dc36b47833f4f12c5d2cbc7acd`。
最终覆盖率目录：`mem_ut/ver/ut/memblock/sim/mother_final_cancel_nonrtl_pass_coverage_20261030`，包含`merged.vdb`、`urgReport`的HTML/TXT、精确350条`selected_tests.txt`与`selection_manifest.json`、2540编译输入快照、指纹和独立验收。
数据库唯一聚合测试`accepted350`，350 tests/350 testrecords精确匹配真实日志SHA和白名单；不混入旧源码、FAIL/debug、额外专项或本Bug软件fixture。

| DUT层次`top_tb.U_MEMBLOCK`指标 | 值 |
|---|---:|
| line | 80.88% |
| condition | 60.54% |
| toggle | 38.56% |
| FSM | 9.24% |
| score | 47.30% |

仅`line+cond+fsm+tgl`，fcov/branch关闭，不能称为功能覆盖率收敛或全部测试空间穷尽；全设计dashboard数值不是本表DUT层次数值。URG自身ERROR/WARNING均0，三个远端console另有12条shell初始化提示已保留并分类，不宣称完整console零warning。
Boole于2026-10-31给出`FINAL_350_COVERAGE_INDEPENDENT_REVIEW_PASS`，产物内`independent_coverage_review.json` SHA256=`cefce3f094c7404e52e6931465515edb4df58c4422862423a6004abc175a2ca0`。

所有controller和collector均已rc0退出；本子任务COMPLETE，母任务整体仍为STOPPED_RTL_CONFIRMED。原矩阵250/260＋额外fault压力100＝350/360名额已PASS，未解决项仅strict-NC10，不以软件40冒充DUT40。所有旧Bug原失败、波形、联停记录保留，后续不得改写。
结果文档在collector及独立review结束后才更新；七份原冻结文档已逐份与50e80d275验证并留存于覆盖率目录`frozen_documents_50e80d275/`。旧gate为历史时点证明，当前结果续记不重新使用旧启动脚本，也不回填旧哈希。review保持undo，未擅自归档。

### 5.2 历史执行过程（非当前待执行任务）

以下“当前/待执行/尚未”等字样仅表示当时阶段；本轮最新验收和剩余问题以5.1及第4节为准。

原矩阵26组×10：BasicTest110/120，其余14组140/140，合计250/260；后者包含真实DUT100和软件40。
本轮门禁后19组190日志已按增强验收全部重新核对通过，实际2540项编译输入hash未变。
前六个BasicTest组现已在B1集成后的同一SV输入上新coverage复跑60/60严格通过，原先版本边界仍保留在历史记录。
两个mixed组每seed固定2条定向事务；普通legacy组每seed1000条；PBMT定向为2条；各自计数分别验收。

干净PBMT coverage专项已fresh compile并10/10严格通过、unknown0，不再列为待完成。
前六组最终版本coverage复跑也已60/60严格通过。fault3首轮现为1FAIL、19ABORTED、80NOT_RUN、0PASS、unknown0，
seed1992896882的UID2401在602001.1ns报第二页TLB查询缺失；原seed同时间同UID波形已复现，
NPI事实提取/断言确认低段corrupt后取消高段，正式RM责任分类及多轮方案review均PASS，已登记W44 RM Buglist。
修复在mother顺序完成r5 coding与fresh compile。原seed1992896882完整10000笔、原参数无临时覆盖严格PASS；
真实DUT专项/邻接合计21/21，四类必要软件专项另计PASS，Turing最终独立实现review PASS；单Bug提交`1343dbce3`后已恢复99项并独立重验矩阵250项。
专项实现review入口：`AI_DOC/plan/rm_review_doc/undo/memblock_first_split_data_error_termination_implementation_review_20261030.md`。
分析入口：`AI_DOC/analysis/framework_design/memblock_fault3_cross_page_query_miss_seed1992896882_20261030.md`。
1343修复时的历史fault3跨轮为1/100，原1FAIL/19ABORTED/80NOT_RUN保留；当时待恢复99和重验250，后续已执行并遇到下一Bug，非当前待办。
恢复批次现已收敛为0PASS/1FAIL/19ABORTED/79NOT_RUN，unknown0，新首错seed1705742557在724655.8ns报ROLLBACK_XMR来源冲突；
不是首段异常修复复发。新矩阵前5组50/50严格通过，第6组由主agent首错管理联停为9PASS/1ABORTED、0功能FAIL。
原参数带波形已同时间/身份复现；NPI证明sample144892 winner4与144894 winner5各自本拍输出正确，旧resolver错误地比较跨拍payload。
Turing正式责任分类和正式方案review均PASS：测试框架问题，不是RM或RTL仲裁缺陷。
修复已在mother顺序完成，仅修改resolver精确sample条件及一个相关软件定向task，未修改RTL/RM/cfg或请求生成。
fresh compile、软件专项及20/20真实DUT专项通过（18邻接加原1705742557与1992896882各10000笔），Turing独立静态实现review PASS；所有专项controller已退出、unknown0。
Turing最终独立`FINAL_IMPLEMENTATION_REVIEW_PASS`，本Bug方案移do，随本记录单Bug提交；当前修复版350项最终批量仍待执行，不把旧源码的51项或250项历史PASS迁移到新版本验收。
历史bba恢复计划为当时两根完整PASS后，保留19颗ABORTED原seed并构造79颗新seed恢复98项；该轮现已执行并产生551576067新问题，不再作为当前恢复清单。
当前专项review：`AI_DOC/plan/test_framework/review_doc/undo/memblock_rollback_same_sample_provenance_implementation_review_20261030.md`。
实际单Bug提交`bba3008ba`已在mother；随后恢复98和250两个独立mode，当前矩阵25组250/250严格/clean通过（210真实DUT＋40软件），controller rc0退出，Turing独立`MATRIX_250_INDEPENDENT_REVIEW_PASS`。
fault3恢复98随后新首错551576067于411428.500ns报cancel snapshot depth15>14，最终20PASS/1FAIL/19ABORTED/58NOT_RUN、unknown0；当时只做原参debug与责任分析，当前修复验证进展见下文，不改写原失败。
后续原参波形及只读observer均同点复现并退出，469443生产关键事件hash一致；Turing正式责任review PASS确认测试框架全局deferred门禁阻止record1658收口，不是瞬时staging或缺anchor。
责任确认时无活动仿真；之后正式方案已获Boole独立`PLAN_REVIEW_PASS`，母worktree顺序完成三个生产修改及本Bug软件专项。
软件fresh compile后四组合严格零错误通过；原551576067的coverage fresh compile及完整10000笔原参数严格PASS。
静态实现review通过，18负例已在r3补齐并获独立SOFTWARE_CONTRACT_REVIEW_PASS；8邻接及三个10000笔原根严格PASS、基础两入口通过，统一动态门禁12真实DUT＋1空virtual PASS、所有controller退出，Boole最终独立review PASS，随本记录单Bug提交后才恢复批量。
保留旧bba版252/350严格验收和20条controller PASS的区别；新源码变化后这些记录只作为历史，不冒称新版本通过。
分析入口：`AI_DOC/analysis/framework_design/memblock_cancel_snapshot_depth_seed551576067_20261030.md`；不把旧legacy topology根因或早期staging猜测当作本次正式结论。
旧bba版最终清单验收252/350（上述250＋两个原根），BasicTest非RTL11组110/110全通过；本次cancel依赖修复新版本尚未建立最终350验收。没有新建worktree或修改RTL。
分析入口：`AI_DOC/analysis/framework_design/memblock_rollback_provenance_conflict_seed1705742557_20261030.md`。
历史bba版25组250个通过记录的SV基线覆盖率已过滤合并，目录`mem_ut/ver/ut/memblock/sim/mother_matrix_pass_coverage_20261030`，
有效`urgReport_r2`对应实际`sim/mother_matrix_pass_coverage_20261030.vdb`、聚合测试`merged_r2`，确认250 tests/records及精确白名单一致，工具日志无ERROR/WARNING；
DUT line80.38%、condition59.51%、toggle37.90%、FSM3.55%，fcov关闭、不启用branch。
该基线排除所有fault3及main旧PBMT，不能冒称新Bug修复后稳定版本的最终母任务覆盖率或原260/260通过。
最新任务状态以`AI_DOC/project_management/memblock_mother_integration_status_20261028.md`及结果根目录
`campaign_state.tsv`为准；每组均保留原summary/results/seed/plusargs和日志，旧FAIL不改写为PASS。

## 6. Plan对齐检查

本轮已执行的B1方案为`AI_DOC/plan/test_framework/plan/do/memblock_csr_hs_mapping_fence_gate_plan_20261029.md`，
PBMT路由方案为`AI_DOC/plan/test_framework/plan/do/memblock_pbmt_matrix_fixed_cfg_profile_plan_20261029.md`。
具体实现对齐结论引用各独立implementation review，不在本索引中虚构另一轮逐函数审查。
执行约束保持：先关旧WIP门禁，再绕过已确认RTL组恢复其他组；只在mother顺序修改，每Bug专项通过单独提交；
仿真可并发，同mode一个controller；不修复无关soft_test，不向未指定任务remote推送。

### 实现与Plan不一致项

未在本轮集成/结果核对中发现新增实现不一致项；各Bug的逐函数结论见对应review。
第5.1节非strict-NC350项验证与覆盖率均已完成；总体母目标只剩第4节RTL阻塞，不把方案编码完成或部分范围通过标为全母任务完成。

### Plan未说明但Coding落实的细节

最初汇总阶段没有生产coding；本次cancel依赖修复的生产改动、专项及执行中补充另见
`AI_DOC/plan/test_framework/review_doc/undo/memblock_cancel_window_deferred_dependency_implementation_review_20261030.md`；该实现现已完成专项及350项/覆盖率验收，详见第5.1节。
结果目录验收工具增加legacy完成标志/数量、原入口及六种计数唯一性检查，
仅用于证据核验，不改变TB或DUT行为；不作为新的生产Bug修复。
阶段独立文档review首轮发现四处事实/索引问题：遗漏17e2f2d67、Basic/legacy marker混写、B1修前混入A1、
已完成PBMT仍列待完成；已逐项修订，Turing复核四项全部关闭，结论`PASS`。这不等于批准fault3新问题coding或母任务最终关闭。

## 7. 非本次修改的逻辑分析

母工作区四项知识库脏文档属于已有后端/写回分析，不纳入此review、修改或提交：
`AI_DOC/analysis/interface/v2/agents/int_writeback_agent.md`、
`AI_DOC/analysis/rtl/v2/flows/rob_compress_and_backend_instruction_flow.md`、
`AI_DOC/analysis/rtl/v2/index.md`、`AI_DOC/analysis/rtl/v2/flows/backend_memory_writeback_flow.md`。
仿真/覆盖率产物位于ignored sim目录；source旧混合WIP继续保全，不整包暂存或覆盖。
未跟踪`mem_ut/ver/ut/memblock/signal.rc`是本次续接观察到的独立本地文件，不修改、不暂存、不提交；未作为本次2540项编译输入变更。结果文档更新不包含任何生产源码修改。
