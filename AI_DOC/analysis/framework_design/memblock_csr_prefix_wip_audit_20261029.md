# CSR prefix 旧工作区剩余内容审计与集成台账

日期：2026-10-29。状态：**必要性审计与B1专项/独立最终review全部通过；随本提交完成mother集成并关闭旧WIP门禁**。
源单Bug提交`5a4dc2df4d`；mother真实DUT14/14严格PASS及两项软件专项PASS。下一步只在mother恢复非strict-NC批量，
原矩阵仍未因此增加PASS名额；完整历史过程、未采用项证据和原FAIL/ABORTED保留如下。

## 1. 术语与执行约束

- mother：`.codex_mother_integration_20261028`，母任务集成与最终回归工作区。
- source：`.codex_csr_prefix_fix_20261025`，本次剩余必要修复的唯一代码修改工作区。
- WIP：尚未完成独立提交及验收的改动，不代表一定应迁移到 mother。
- 必要性门禁：每项须证明已等价合入、应修复合入、仅由未采用的 WIP 引入，或有充分证据不需迁移。
- 专项诊断：用历史失败 seed 检查 mother 是否存在同一缺陷，不计作旧 WIP 门禁完成后的矩阵恢复。

不新建 worktree；不并行修改代码。source 中逐 Bug 完成方案、独立 review、编码、实际提交源码的
重新编译和原 seed/邻接专项、单 Bug commit，再集成 mother 并原 seed 复测。下一 Bug 的代码修改
必须等待上一 Bug 完成独立提交。无关 soft_test 不扩展；必要的 Bug 定向验证可以使用既有辅助测试。
RTL/Scala 不在本轮修改范围，strict-NC 不伪造 PASS；用户已明确要求旧 WIP 门禁完成后跳过该组继续其他组。

## 2. 基线与保护

- mother 记录前 HEAD=`9e6603725`；生产源码=`c9763bd7d`。
- source HEAD=`e2ed0f0cb`；存在 index/working tree 两层不同修改及未跟踪文件，禁止整包提交。
- source 完整快照：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/sim/wip_audit_snapshot_20261029`。
  包含 HEAD/index/worktree 三份 binary patch、状态、未跟踪路径、改动文件 tar.gz 和 SHA256SUMS。
- mother 已存在的 4 项知识库文档脏改动不属于本次，不覆盖、不暂存、不提交。
- 唯一远端为公共上游 `origin=git@github.com:OpenXiangShan/XiangShan.git`；已询问用户 push 目标，
  未确认前只做本地提交，不能擅自把验证工作区内容推送到公共上游。

## 3. 逐项台账

| 编号 | 问题/内容 | 初始事实 | 必要性与后续证据 | 状态 |
|---|---|---|---|---|
| A0 | CSR redirect/prefix 修复 | source `e2ed0f0cb` 已通过冲突处理集成为 mother `4948d291e`，prefix 定向及四个 CSR scalar 10k 原 seed 通过 | 已集成部分不再次提交；其余差异单列，不称整个 source 已完成 | 已有关闭证据 |
| A1/B1 | stale-TLB mapping context 与映射切换协议 | mother原规模seed1494768720在UID546 context mismatch；A1过滤候选8例7PASS/1FAIL | A1不采用；B1源5a4dc2df4d精确集成，母真实14/14及软件协议/运输专项通过，独立最终review PASS | **必要修复已完成；随本提交合入，门禁关闭** |
| A2 | STD accepted-fire 延迟 owner materialization | source deferred queue 路径曾在 seed `1410608193` 一拍 STD WB 前未物化 owner；source implementation review 未闭环 | mother 使用直接 watcher + `mark_fired_items()`，不存在该 deferred/residency API；失败只存在于 source 新路径，software directed 不能证明母分支缺陷 | **不迁移；证据关闭** |
| A3 | issue sample residency 基础设施 | source 引入 raw/enriched ledger、ticket、watermark 和 producer override；model-off directed 在 anchor consumer suppression 处失败，model-on无独立 PASS/commit | 这是 source 后续 exact-fire consumer 的公共前置，不是 mother当前功能依赖；迁移会引入未闭环 infrastructure及其测试边界 | **不迁移；证据关闭** |
| A4 | CSR 初始化 residency service 晚注册 | source seed `1410608193` 在新增 ledger 中 ticket floor=50而ledger为空，方案首轮 `REVIEW_FAIL` | mother不注册residency service，且同seed原入口1000请求严格PASS；这是A3未采用时的source特有症状 | **不迁移；证据关闭** |
| A5 | PMA/PMP capture ticket 幂等/原子性 | source r28 known10 的 `1494768720`、`1764880593` 在新增 capture ticket 路径分别 fatal；source r11另有1000请求 root mismatch | mother使用原 capture 路径，对相同 seed、同 cfg、24请求专项 `2/2 PASS`，最终/caught/demoted ERROR/FATAL均0；source failure证明未采用WIP路径不稳定，不是母分支缺陷 | **不迁移；证据关闭** |
| A6 | 已合入 FIFO replay、prefix 相关残余差异 | source混有已提交逻辑、位置移动、删除旧plan和未验证WIP | FIFO/prefix已集成；report-free helper仅见负向测试用途，额外completion门禁无mother早完成证据；两文件index/worktree互抵无净diff，细节见5.5 | **已有部分已集成；额外WIP不迁移，证据关闭** |
| A7 | 辅助测试、parser、历史文档/seed清单 | source有未跟踪soft_test、analysis、plan/review、seed和工具文件 | 没有对应母任务必要生产修复或已闭环Bug；保留source快照供追溯，不加入mother；本轮不修复无关soft_test | **不迁移；证据关闭** |


## 4. 验证与恢复门禁

历史 CSR random 入口为 `tc=basicTest / ts=memblock_csr_random_config_vseq / cfg=tc_memblock_csr_random_config`。
各 seed 必须按其原日志确认请求数、timeout、trigger、ranges 和 mixed/store 权重；原矩阵实际为1000请求，
不能将所有旧专项统一假定成同一配置。mother 上的专项诊断保持来源说明，不预先计为矩阵 PASS。

待审计结论和逐项修复门禁全部关闭后，才恢复 mother 第8–12组等未完成矩阵。第7组
`basicTest / memblock_dispatch_real_smoke_vseq / tc_dispatch_real_mmu_sv39_strict_nc_100k`
保持已知 RTL 未修复/延期，原 seed 和 ABORTED/NOT_RUN 名额保留，不从全量计划中删除。
并发仿真不允许同 mode 多 controller；代码变化必须重新 compile；日志按 TEST_PASS/$finish、
raw ERROR/FATAL及final/caught/demoted六项ERROR/FATAL、controller和remote_unknown严格验收。

## 5. 本轮进展（历史过程，关闭结论见5.10；逐文件处置见第6节；执行现状见最新status）

- 已完成两工作区状态和分支确认、source 完整差异快照；尚无 source 生产代码修改。
- 已启动两个只读独立审计：A1/A5 与 A2/A3/A4。主 agent 负责完整文件归类、基线诊断及后续串行执行。
- 批量回归尚未恢复，RTL未修改，尚无本轮 Bug 修复提交。

### 5.1 当前母分支专项诊断结果（不计批量矩阵）

两次有效仿真均使用当前 mother 未修改生产源码、同一已编译
`mode=mother_wip_audit_baseline_20261029`，入口为
`basicTest / memblock_csr_random_config_vseq / tc_memblock_csr_random_config`，
`+MEMBLOCK_MAIN_TRANS_NUM=1000`、`timeout_ns=100000000`、`UVM_MEDIUM`，开启 monitor/record，
关闭波形/覆盖率，未增加 trigger override。

| seed | 控制器结果 | 日志验收 | 结果目录（相对 mother） |
|---|---|---|---|
| `903730789` | rc=0、1 PASS、其余状态/remote_unknown 均0 | TEST_PASS、正常 `$finish` 于93099ns，raw及final/caught/demoted ERROR/FATAL均0 | `mem_ut/ver/ut/memblock/sim/mother_wip_audit_20261029_results/stale_tlb_original903` |
| `1410608193` | rc=0、1 PASS、其余状态/remote_unknown 均0 | TEST_PASS、正常 `$finish` 于88183ns，raw及final/caught/demoted ERROR/FATAL均0 | `mem_ut/ver/ut/memblock/sim/mother_wip_audit_20261029_results/issue_residency_original141_retry` |

`issue_residency_original141` 为一次错误选择新 mode 却使用 `--no-compile` 的基础设施尝试：
缺失 simv、退出码5、1 NOT_RUN，没有进行功能仿真；纠正为复用已编译 mode 后重新建立上述 `_retry`
结果目录。不得将该尝试记为功能 FAIL，也不得覆盖其原始结果。

### 5.2 A1 尚未关闭的行为合同冲突

mother 的 `soft_test_rm_provenance_followup_sequence::check_l1_hit_issue_snapshot_lifecycle()`
及 `check_l1_hit_overlap_resolution()` 明确要求 WAITING UID 使用 current-live canonical entry；
completed-response issue snapshot 只作诊断信息，canonical entry 删除后应报告 query miss。
该合同来自已提交 `7632adee72`，不是本轮新增的测试预期。

旧 WIP 将上述行为改为 issue-hit payload 冻结，并修改定向测试以匹配新预期。静态看见 mother
未读取 snapshot，不能单独证明 mother 存在 Bug。必须先审清原始失败是否已被 active-root context
兼容性修复 `5c131a18de` 消除，及新的冻结是否具有实际 DUT residency 证据。
当前 `903730789` 原规模已通过，只能证明此 seed 当前未复现；不能证明所有潜在所有权场景均正确。
已请求独立 reviewer 复查该反证，结论前不修改 resolver、不引入整套 residency infrastructure。
后续结论见5.3/5.4：必须修mode/root复用，但不反转current-live合同。

### 5.3 原规模复测推翻初步排除结论

独立审计支持 A2/A3/A4 的失败依赖 source 自身新增的 deferred/residency 机制，A5的24请求capture失败
也依赖source新增机制。上述排除不等于A1已关闭。主agent进一步执行同seed的1000请求原规模专项，
结果为 `1494768720` FAIL、`1764880593` PASS；新结果目录为
`mem_ut/ver/ut/memblock/sim/mother_wip_audit_20261029_results/pma_root_original1000`。

FAIL日志首错在45755.200ns：`uid 546 byte 0 TLB entry does not match frozen context`；该UID的
issue和commit均使用 generation178，但 response-derived字段多次提示 S1 mode 与 raw payload不符。
当前mother源码中未找到预期的完整active-root兼容helper，先前将`5c131a18d`等同于该完整修复的判断
缺少依据，现予以撤回。A1/A6已重新打开，须完成新的独立根因/方案review后再判必要迁移范围。
不得把24请求capture PASS冒充1000请求stale-context PASS；批量回归仍受门禁约束。

source混合WIP继续快照保全，未reset、整包merge或删除。发现该失败时尚无新生产代码修改或Bug提交；
后续隔离实施进度见5.4。

### 5.4 必要修复实施进展

独立reviewer Turing已确认Sv48→Sv39旧mode canonical复用为RM责任。当前最小方案二轮
`REVIEW_PASS`，方案hash=`9b77264cccdaba2fcf1d5a58c536c99eab928003e88045f002323aa8aad76544`。
只修复mode/root兼容过滤，保留current-live，不引入旧snapshot fallback、residency、STD或capture ticket。

为不覆盖source混合WIP，在该既有worktree的`sim/wip_audit_tlb_context_candidate_20261029`导出Git基线归档，
以独立index构造必要补丁；没有新建Git worktree，不改source HEAD/真实index/原生产文件。
候选tree=`f6f5c3120e6e5503e88d03929ef523673739b55c`，patch SHA256=
`7c7b9b2f220ccd606712694b403b1b54d2e861df8cabd919bc233aad5f2b1700`，修改3个生产文件和既有provenance
定向文件，已启动全新编译及16格mode/root/stage/exact-range矩阵；尚无实现验收结论。
该归档只用于精确待提交tree专项，不是另一个开发worktree。

第一版全新compile已PASS；`basicTest / memblock_rm_provenance_followup_directed_vseq / default / seed2601290`
完成16格矩阵，controller rc0、result PASS、正常finish。该software负向定向有预期caught ERROR=18、FATAL=2，
final和demoted均0；这是负向合同检查，绝不计作真实DUT严格零错误PASS。结果位于source归档
`mem_ut/ver/ut/memblock/sim/tlb_context_results/directed_r1`。
同版已启动原4个CSR random seed的1000请求专项（jobs4），结果未验收，implementation review也尚未完成。
该轮随后完成4/4严格PASS，四日志raw和final/caught/demoted ERROR/FATAL均0。
review首轮发现两个验证缺口：final root checker未实际触发、输入清单错误为空hash。已补既有PBMT定向专用
S1/S2 COMPLETED root检查和20格模式矩阵，修正为479个具名TB/config输入清单并使用新mode重新编译。
第二版compile和20格provenance定向已PASS；实际final-root定向仍在专项中，不能提前关闭。
候选r2 tree=`ee232660c40324a2fec8ecf7f5e40daeca650523`，patch SHA256=
`6c0a77a4c253484910b26699ef4ae91556ed22ef6038cfbec0e8d0facff00e7d`；输入清单hash=
`192b03e2307c54f63df95d9328610be7c403e7afcd159113b0633586efe326e8`。
新compile实际2539个具名文件路径经检查仅来自archive、固定RTL及EDA工具目录，无原mixed TB回读。

### 5.5 A1候选未通过全专项，转根因前置协议B1

r2两项定向均PASS，原4＋fresh4共8颗1000请求为7PASS/1FAIL：fresh `26012903` 的UID941在73336.200ns
query miss；全部任务已收敛，unknown0。mother原生产同seed重跑也FAIL，但首错是UID912在71246.200ns
旧模式不匹配，不能称两边复现相同query miss。故A1目前没有提交、不合入，不恢复批量。

独立review核实：CSR changed的inflight flush不等同L1 storage有效位清除；global条目可跨ASID保留。
模型自行删除旧mapping不代表DUT已删除。A1过滤只处理新请求内容，不能独自保证场景的唯一mapping真源。
下一步以B1“CSR mapping switch缺少真实失效完成门禁”修复当前nonvirtual场景根因，A1代码冻结保留为未采用候选。

B1最小范围review通过：special且oldvirt=targetvirt=0，SATP mode/root变化并且至少一侧paged时，target/PMP
写完并由monitor确认稳定后驱动真实HS全地址全ASID SFENCE，等待同CSR owner/event/reset的C0/C4再sendover。
保留原CSR owner/kind/audit及snapshot/PMA完成依据，不通过固定延时/清模型代替实际fence；pureASID/permission
不发fence，虚拟切换在任何副作用前拒绝。B1必须在不含A1的精确基线上独立编译、原8seed和邻接全部通过，
独立implementation review后单Bug提交再集成。正式方案位于source
`AI_DOC/plan/test_framework/plan/undo/memblock_csr_hs_mapping_fence_gate_plan_20261029.md`，正文review进行中。

A6独立审计：Uncache/ROB的report-free helper主要配合负向测试去catcher，未发现mother正常运行缺陷；
CSR completion额外三行是实际生产门禁变化，但mother已有持续FIFO service和candidate expected-count检查，
暂无独立早完成证据，因此不迁移。`status_transaction.sv`、`exception_redirect_replay_handler.sv`及
`soft_test_memblock_pending_mmio_directed_sequence.sv`的index/worktree差异互抵，相对HEAD无净diff；
不得误报为待合入Bug。前期仅列前两个不完整，最终逐文件复核见第6节。

debug重新编译开启波形后原1494768720仍在45755.200ns同首错复现，远端已收敛。波形目录为mother
`mem_ut/ver/ut/memblock/sim/mother_wip_tlb_context_debug_20261029/wave`，具体文件路径已登记source W44 Buglist。

门禁完成后的执行顺序（尚未开始）：

1. mother 批量回归跳过 `basicTest / memblock_dispatch_real_smoke_vseq / tc_dispatch_real_mmu_sv39_strict_nc_100k`；
2. 先执行 BasicTest 尚未完成的第8至第12组，随后执行其余非 strict-NC functional groups；
3. 每批仿真仍保留 monitor/record，使用新空结果目录和独立 mode；仿真可并发，代码修改和集成仍串行；
4. 任何新失败先停止对应后续批次，按责任分类、Buglist、方案 review、专项验证和单 Bug commit闭环。

### 5.6 接续核对与B1 r3验证

已再次核对source混合WIP和mother脏文档：未回滚、整包暂存、建新worktree、提交或合入。
B1 r3冻结tree=`8d605e2cf351b735a34018e740f2030b1e7d32eb`，补齐具体transport类、
阻塞期间owner/reset/watchdog和显式成功返回；当前独立实现review及协议专项仍在进行。
r3新mode已重新编译，27项trigger PASS（既有software负向预期caught ERROR/FATAL各2，不计真实DUT）。
原4+fresh4各1000请求8/8严格PASS，controller rc0、unknown0，其余未完成分类均0；每日志TEST_PASS
一次、正常finish、raw及final/caught/demoted ERROR/FATAL全0。真实C4完成次数分别79/73/69/73/74/71/79/77，
合计595次且due-anchor均为4。结果目录为source归档内`mem_ut/ver/ut/memblock/sim/b1_mapping_results/original4_fresh4_r3`。
原WIP快照四项hash和r3具名输入hash已再次核对通过；尚待完整协议定向、邻接及集成复测，批量矩阵未恢复。

### 5.7 r6专项与邻接未关闭项

r4定向首轮FAIL为测试fixture漏publish live，已修；r5编译失败为测试结构比较缺显式类型，未进行功能仿真。
独立review另要求首次target前冻结原control/L2 reset并监督special mapping的target/PMP阻塞，r6已实现。
r6重新编译PASS；原4+fresh4各1000请求8/8 PASS；软件协议定向PASS，预期caught ERROR/FATAL=2/4；
独立UVM transport负例7/7 PASS，预期caught FATAL=7，final/demoted=0，不能计作真实DUT功能PASS。
尚待生产实现review最终通过及CSR write监护等边界补测。

Sv39邻接未通过：source r6入口`basicTest / memblock_dispatch_real_smoke_vseq / tc_dispatch_real_mmu_sv39_smoke`，
MAIN_TRANS_NUM=1000、CHECK_TRIGGER_EN=0、seed212889620在120327ns UID462报
`Load exception expected=0x2000 status=0x10 actual=0x10`；seed26012911为ABORTED，unknown0。
这是非special路径，不能直接归因B1；source基线e2ed缺mother现有later-split等已修提交。
已启动mother当前生产同seed/同参数对照，分类前不修改其他Bug、不提交B1、不关闭门禁。
source结果在archive内`mem_ut/ver/ut/memblock/sim/b1_mapping_results/adj_sv39_r6`；mother对照在
`mem_ut/ver/ut/memblock/sim/mother_wip_audit_20261029_results/adj_sv39_212889620_baseline`。

### 5.8 最终候选与旧邻接分类

Turing独立确认B1生产静态及r8 fixture `REVIEW_PASS`。r7负例失败为reset清CSR hold后旧fixture未重发target，
r8改为等真实ACK/ready→真实重发clean→等新full92→创建下一case新action；不改生产方案、不扩大15例矩阵。
r8 fresh compile、27 trigger/完整软件协议、隔离15项UVM负例均PASS；15项预期caught FATAL，不计真实DUT功能PASS。
最终源码tree=`90adb0e2b570b75678e6c62b04778284f0b9da38`，原4+fresh4千请求8/8已完成，邻接继续验收。

Sv39 source邻接已独立分类：source缺mother已集成的`a13d10814` later-split异常优先级修复，签名与历史相同。
mother本轮同seed212889620、MAIN1000、CHECK_TRIGGER0对照严格PASS，六项ERROR/FATAL均0。
`c9763bd7d`是redirect later-split取消维护，不混称此次exception优先级根因。B1 disabled非special路径不可达。
故source此项FAIL保留、seed26012911保留ABORTED，不在B1夹带该已修Bug；mother合B1后必须再次新编译邻接确认。

### 5.9 单Bug提交与mother集成验收进行中

source单Bug提交=`5a4dc2df4d`，独立ref=`codex/bug-csr-hs-mapping-fence-20261029`，parent=e2ed0f0cb；
source HEAD/真实index保持原状。本提交只含B1代码/必要定向及plan/review/flow/单Bug周章节，
未包含source混合W44 startup/residency问题。源侧最终真实DUT12/12严格PASS，隔离定向单列。
完整提交tree=`90c21e798e25604b32d63f9f6fc7c8fa27c489fe`，mem_ut subtree与已验r8完全一致。

mother已精确应用B1差异，Buglist只追加一章，不覆盖原周文件；尚未形成mother集成commit。
新mode=`mother_csr_hs_fence_gate_20261029`正在重新编译并复测原8千请求。随后同源码运行Sv39/PBMT/SFENCE、
protocol及隔离negative；全部验收并独立review后才提交集成/关闭审计门禁。批量矩阵仍未恢复。
结果根目录：mother `mem_ut/ver/ut/memblock/sim/mother_csr_hs_fence_gate_20261029_results`。
未推远端：当前只存在公共upstream origin，已询问用户任务远端/分支，未明确前只本地提交。

### 5.10 最终门禁关闭结论

Turing于2026-10-29给出`FINAL_DYNAMIC_REVIEW_PASS`：母fresh compile、真实14/14严格PASS、两项软件专项
及2540项输入/指纹/日志一致，controller全部退出、unknown0，无剩余生产/动态blocker。
B1随本提交完成mother集成，已显式纳入Buglist、plan/do、review/undo、flow和本审计/状态表；
原mother四项无关知识库脏文档不暂存、不提交，source原WIP/HEAD/index/快照均保留。

本轮必要内容只剩B1且已闭环；A0/A6已集成部分不重复合，A1旧过滤与A2–A7额外WIP均有不迁移证据。
这些保留文件不表示仍有应合未合Bug。source保留为证据存档，后续代码修改只在mother顺序进行，
不再开新worktree。旧WIP门禁在本提交落地后关闭，恢复组8–12及其他非strict-NC组；组7仍延期不记PASS。
最终母结果目录见5.9，`acceptance.json`明确14真实与2软件分开；没有把专项误计入原矩阵或声称已有最终覆盖率。

## 6. 冻结WIP逐文件处置

Kuhn独立复核snapshot：23个已跟踪源码/测试文件，其中20个净改、3个HEAD→worktree净零；另4个未跟踪测试文件。
下表路径相对`mem_ut/ver/ut/memblock/`；“不迁移”仅指原冻结WIP，不包括同文件独立B1 hunk。

| 文件 | 项目 | 必要性/处置证据 |
|---|---|---|
| `agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv` | A3 | 仅producer override跳过采样，依赖未采用ledger；不迁移。 |
| `agent/fence_agent_agent/src/fence_agent_agent_monitor.sv` | A3 | 同上，不是母侧fence缺陷修复；不迁移。 |
| `agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv` | A3 | exact fire timestamp/ticket/publication链配套；母侧直接watcher不需要。 |
| `agent/lintsissue_agent_agent/src/lintsissue_agent_agent_xaction.sv` | A3 | 新增fire/ticket字段只服务上述链；不迁移。 |
| `common/memblock_common/src/memblock_sync_pkg.sv` | A3/A6 | raw/enriched ledger、ticket/revision/override不采用；FIFO已有91d，不重复迁移。 |
| `env/src/memblock_rm/memblock_rm.sv` | A1 | 旧root检查候选非B1依赖，A1全专项未过；不迁移。 |
| `seq/base_seq/memblock_issue_dispatch_base_sequence.sv` | A2/A3 | deferred fire/materialization依赖A3；母直接watcher不存在该新链缺陷。 |
| `seq/base_seq/memblock_l2tlb_base_sequence.sv` | A1 | response accept/fanout provenance扩展属于旧issue-hit ownership方案；不迁移。 |
| `seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv` | A6/A7 | **净零**：index/worktree互抵，无待迁移补丁。 |
| `seq/base_seq/soft_test/soft_test_memblock_rob_admission_window_sequence.sv` | A6/A7 | 去catcher/report-free辅助重写无母正常失败证据；不迁移。 |
| `seq/base_seq/soft_test/soft_test_rm_provenance_followup_sequence.sv` | A1/A5/A6/A7 | 原冻结snapshot/capture/去catcher不迁移；只提取独立B1协议测试。 |
| `seq/base_seq_help/common_data_transaction.sv` | A0/A1/A3/A5/A6 | A0/FIFO已有；其余ledger/snapshot/capture/report-free不迁移；B1只合精确arm/finish/完成门禁。 |
| `seq/base_seq_help/exception_redirect_replay_handler.sv` | A0/A6 | **净零**，prefix已合4948，无待迁移净补丁。 |
| `seq/base_seq_help/issue_queue_scheduler.sv` | A3 | `*_with_residency`wrapper依赖A3，不是独立scheduler修复。 |
| `seq/base_seq_help/mem_base_sequence.sv` | A6 | Uncache report-free precheck辅助去catcher，无母正常失败证据。 |
| `seq/base_seq_help/memblock_control_barrier_service.sv` | A6 | 额外produced-count全局门禁无独立必要证据，母已有持续FIFO/candidate count检查。 |
| `seq/base_seq_help/memblock_pma_pmp_model.sv` | A5/A3 | capture ticket/revision/prepare-commit链新增症状，不是FIFO遗漏。 |
| `seq/base_seq_help/memblock_rm_readonly_api.sv` | A1/A6 | WAITING冻结payload改变current-live合同，report-free查询辅助；不迁移。 |
| `seq/base_seq_help/memblock_tlb_entry.sv` | A1/A5 | issue residency/fanout/PMA owner扩展属旧方案组合；不迁移。 |
| `seq/base_seq_help/status_transaction.sv` | A0/A6 | **净零**，两层差异互抵；不是剩余Bug。 |
| `seq/seq_pkg.sv` | A2/A3/A7 | 不合旧materialization/residency include；只合B1 negative VSEQ include。 |
| `tc/src/soft_test/soft_test_tc_rm_pbmt_effective_fault.sv` | A1/A6/A7 | fanout fixture/去catcher/precheck重写不构成必要生产修复。 |
| `tc/tc_pkg.sv` | A2/A3/A7 | 仅注册未采用的两项辅助testcase；不迁移。 |
| `seq/base_seq/soft_test/soft_test_memblock_issue_fire_materialization_sequence.sv`（原未跟踪） | A2/A7 | 未采用deferred链的定向，实现review round2未闭环。 |
| `tc/src/soft_test/soft_test_tc_issue_fire_materialization.sv`（原未跟踪） | A2/A7 | 上述辅助入口非母任务依赖。 |
| `seq/base_seq/soft_test/soft_test_memblock_issue_residency_infrastructure_sequence.sv`（原未跟踪） | A3/A7 | model-off有anchor失败，model-on未闭环；非母矩阵前置。 |
| `tc/src/soft_test/soft_test_tc_issue_residency_infrastructure.sv`（原未跟踪） | A3/A7 | 上述基础设施入口无独立必要性。 |

原未跟踪清单为65路径＝61个AI_DOC文件＋上表4测试文件。以下通配只针对冻结`untracked_paths.zlist`，
不外推后续新增B1文档；全部保留快照，不整包迁移或删除：

| 文档/工具路径组 | 项目与处置证据 |
|---|---|
| `AI_DOC/plan/test_framework/{plan,review_doc}/undo/memblock_csr_redirect_preserved_barrier_prefix_invariant_fix*` | A0/A6，冻结含同路径删除重建；功能已合，不迁移删除动作，不覆盖母正式文档。 |
| `AI_DOC/plan/rm_plan/undo/memblock_rm_stale_tlb_issue_snapshot_context_fix_plan_20261027.md`及`AI_DOC/plan/rm_review_doc/undo/memblock_rm_stale_tlb_issue_snapshot_context_fix_plan*` | A1/A5/A7旧snapshot/capture方案；round17 Plan PASS不是实现闭环，保留历史。 |
| `AI_DOC/plan/test_framework/{plan,review_doc}/undo/memblock_issue_sample_residency_infrastructure_plan*` | A3/A7，Plan PASS不证明母侧必要性或实现验收；不迁移。 |
| `AI_DOC/plan/test_framework/{plan,review_doc}/undo/memblock_std_accepted_fire_deferred_owner_fix*` | A2/A7，implementation round2仍缺frozen路径/seed/双端口完整门禁。 |
| `AI_DOC/analysis/memblock_csr_initial_residency_service_late_registration_classification_20261028.md`及`AI_DOC/plan/test_framework/{plan,review_doc}/undo/memblock_csr_initial_residency_service_handoff_fix*` | A4/A7，只依赖未采用A3，Plan REVIEW_FAIL，原冻结patch没有startup实现。 |
| `AI_DOC/analysis/rm/*_20261027.md`及`AI_DOC/plan/rm_review_doc/undo/memblock_{pma_pmp_raw_csr_fifo,raw_pma_pmp_csr_fifo}_*` | A6/A7，历史FIFO隔离过程；91d已合，不把旧条件候选当待合代码。 |
| `AI_DOC/analysis/memblock_mother_regression_status_review_20261028.md`、`AI_DOC/seed/memblock_csr_prefix_fix_fresh30_20261026.txt` | A7旧版本/批次记录，不覆盖当前状态、不计矩阵。 |
| `AI_DOC/tools/check_issue_fire_materialization.py` | A2/A7，仅检查未采用deferred audit；review所需额外path manifest不在冻结清单，不能称seed门禁齐全。 |
| 原`AI_DOC/buglist/rm/v2/rm_buglist_2026-W43_20261019.md`、原未跟踪W44 | 不整文件覆盖母侧；只追加经独立验证B1章节，不夹带未采用startup/residency结论。 |

B1另外4源码文件为`seq/base_seq/{memblock_csr_control_base_sequence,memblock_dynamic_csr_change_sequence}.sv`及
`seq/virtual_sequence/{memblock_csr_mapping_transport_negative_vseq,memblock_csr_random_config_vseq}.sv`，
连同flow/plan/review/Buglist见独立提交`5a4dc2df4d`；不能据此声称原WIP已整体迁移。
