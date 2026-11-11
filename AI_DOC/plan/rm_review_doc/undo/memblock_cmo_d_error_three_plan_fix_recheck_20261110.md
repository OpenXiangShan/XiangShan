# V2 CMO、denied/corrupt 与行历史三方案修复及闭环复审

- 日期：2026-11-10。
- 版本与分支：V2 / `mem_ut_uvm_v2`。
- 状态：`READY_FOR_CODING`，仅表示方案可执行；实现尚未开始，三份plan继续位于undo。
- 方式：主agent修订原三文档，独立subagent两轮源码对照复审；主agent最终交叉复核。
- 结论：原5项问题及R1新增的2组时序契约均闭环，最终R2无P1/P2/P3必改项。
- 原始评审：`AI_DOC/plan/rm_review_doc/undo/memblock_cmo_d_error_three_plan_minimal_adaptation_review_20261110.md`；其原基线及问题正文保留，当前状态以本报告为准。
- 本轮仅修改方案及review文档，不修改实现、不编译仿真、不提交或移动到do；本评审不扩大coding授权。

## 1. 术语与抽象功能说明

| 英文术语 | 本文中文含义 | 落点 | 示例 |
|---|---|---|---|
| beat / fire | 一次响应数据传输 / 实际握手接受 | L2同token的beat事实 | 首beat已消费错误不等末beat完成 |
| reqSource / source generation | 既有来源编码 / 总线槽本次占用代次 | A.user及既有A/D生命周期 | Store来源含合并Load也保守正常 |
| owner / epoch / attempt | 动态指令身份 / 实例代际 / 一次执行尝试 | 被动请求与错误消费事实 | replay尝试不能冒充最终响应 |
| origin / slot version | 独立错误来源 / 实际物理槽来源版本 | 统一被动observer | clean refill建立raw=0新版本，首次行选择保留 |
| S0/S1 / bypass | 请求/响应选择阶段 / 真实写旁路 | errorArray及LoadPipe/MainPipe | s0写旁路优先s1，MainPipe首S1锁存后保持 |
| raw / input mask | 原始denied/corrupt / 错误输入资格集合 | 实际输入与独立ledger核对 | 数据全forward不自动屏蔽其它有效错误输入 |
| SC shadow / overlay / entry | 失败SC检查被后续写覆盖 / 期望字节 / 最终比较记录 | commit_memory_effect底座 | zero子效果保留三项既有副作用 |
| NC / backend / scope | 普通非缓存路径 / 模拟后端 / 当前支持范围 | Uncache响应生成 | NC后端接受后参数denied支持；真实拒绝明确fatal |
| FIFO / consumer / watermark | 事件队列 / 唯一消费服务 / 已完整归档水位 | env既有队列与adapter | 水位覆盖后缺事实FAIL，不无限等 |
| R1/R2 / P1/P2/P3 | 本轮第一次/第二次复审 / 必改项严重度 | 本报告各轮记录 | R1两组契约修复后R2无必改项 |

## 2. 最终方案基线

本表hash对应subagent R2通过且主agent最终确认的原文，后续修改功能契约需要重新review。

| 方案 | 路径 | 行数 | SHA-256 |
|---|---|---:|---|
| CMO RM | `AI_DOC/plan/rm_plan/undo/memblock_cmo_blackbox_rm_coding_plan_20261103.md` | 820 | `033ee6519da6a915dde7d71861d8fa40ac2db9d5b0b5adc3fc0bda88828324ab` |
| 统一错误 | `AI_DOC/plan/rm_plan/undo/memblock_nonvector_d_error_path_classification_plan_20261110.md` | 642 | `17e6107147624df2d98b47d6d23a9c7bcaa694a43bea01293aec7107f15ccff3` |
| 行历史 | `AI_DOC/plan/test_framework/plan/undo/memblock_l2_grantdata_first_line_weighted_error_plan_20261110.md` | 473 | `4ad1005126f96ddebad293671bb39b1f5f281c5f32d6a173267375b2d311bc85` |

## 3. 原五项修复与最小适配

| 问题 | 修改前 | 修改后 | 修订落点 | 验证场景 |
|---|---|---|---|---|
| P1-1首beat错误 | 到末beat才激活，本次早写回查不到错 | token内每beat真实fire即发布，完整完成另标 | 行历史4.2/4.3、6.5/6.6/6.9；统一4.1、6.2 | 第二beat长反压、第一beat已fault写回 |
| P1-2永久错误 | PA及首次activation永久支配未来访问 | 本次错误输入关联token/beat或被动slot版本；正常新回填有新0版本 | 统一4.8、6.2/6.3、6.13/6.14；行历史4.1/6.6 | 错误回填移出后Store正常回填，后Load正常 |
| P1-3 SC覆盖遗漏 | zero/MMIO另写byte与entry，漏失败SC shadow | 整组预检查后8B子item复用commit_memory_effect，保留shadow/byte/entry | CMO8.1；统一6.10/6.11 | SC失败后zero/MMIO覆盖部分或全部字节 |
| P2-1 NC数据契约 | 后端拒绝无写却继续保留提交overlay | NC限定后端已接受后参数denied；真实backend拒绝为不支持配置并立即fatal；MMIO/zero仍完整支持拒绝 | 统一4.7/6.15；CMO8.2 | NC正常参数注错、后端拒绝负向fatal、无悬挂/伪DUT错 |
| P2-2来源过重 | 基础注错强制重建全部MSHR合并消费者 | 复用已接出reqSource，Store-origin保守clean，复用A/D代次与FIFO唯一consumer | 行历史2.3/6.8；统一3.3/4.9/6.8；CMO4.3/5.6 | 四类来源、合并Store、source复用与未知编码 |

不以最小化为由删掉真实请求证明、cacheable zero的DCache成功链、Uncache逐段效果或错误通知比较。slot来源索引只跟随实际事件、保存身份和独立raw来源，不预测cache数据、命中或替换，不新增第二个agent。

## 4. 修复—复审轮次

| 轮次 | 独立结论 | 主agent处理 | 当前状态 |
|---|---|---|---|
| 原始review | 3 P1、2 P2，不宜直接coding | 按第3章在原三文档修复 | 全部闭环 |
| R1 | 旧五项实质修复；两组P2观察时序契约仍需明确，无新P1 | 精确写入errorArray的S0/S1旁路/延迟写、MainPipe保持、无非refill源不变量，以及原子miss/replay/最终响应、双raw OR | 全部闭环 |
| R2 | 无P1/P2/P3必改项，可按当前V2源码coding | 主agent复核全文、代码依据、跨文档职责及最终hash | 通过 |

### 4.1 R1 两组契约及处理

**错误来源读取：** S0实际读接受记录idx和每way来源候选，当拍s0写优先前拍s1写；写接受与array延迟提交分别跟踪。LoadPipe无寄存旁路时按实际S1消费的committed来源选择；MainPipe首S1按实际命中way锁存，停顿保持后传S2。失效/替换只撤销旧有效身份，不擅自清实际errorArray，也不删除已经冻结的引用。同批旧身份先结束再建立新版本。当前非refill error写没有独立来源，明确证据FAIL，不复制actual raw自证。

**原子响应：** miss且replay重新请求；miss且不replay继续等待；只有真实resp.fire且非miss形成最终consume fact。保留原始owner/attempt及MainPipe miss_id/ack链，refill raw和读时slot flag raw分别来源核对、逐bit OR后才按denied优先和CSR enable映射异常；enable=0仍核对raw及正常数据/效果。

### 4.2 R2 独立复审全文

下文为subagent最终复审结论，基线与第2章一致；只调整标题层级和路径说明，不改变其判断。

#### 3. 上一轮问题闭环

##### 3.1 首beat错误可消费，不再等末beat

行历史方案第4章、6.5/6.6/6.9明确同一token内每beat有效mask、raw、sample、物理覆盖和keyword顺序；首beat实际D.fire发布，最后beat仅封闭full_complete。无token的行查询只返回诊断，不输出本次异常结论。该契约与MissQueue按beat转发、DCacheWrapper的D/MSHR转发和LoadUnit错误输入相容，原末beat/WB sample误配已闭环。

##### 3.2 永久行选择与实际错误来源分离

首次CLEAN/CORRUPT/DENIED选择仍不重抽；当前消费必须给出owner/epoch/attempt及token/beat或slot版本。clean refill建立独立raw=0的新来源，失效不被伪装成错误数组清零，zero hit不擅自清error flag。错误选择历史与缓存当前来源身份各司其职，原首次activation永久污染后续正常Load的问题已闭环。

##### 3.3 SC shadow与字节效果共用既有底座

CMO方案第8.1节及统一方案第6.10/6.11节先完整预检查，再构造独立8B子item调用既有commit_memory_effect；shadow、byte/valid、entry三项副作用均保留。两类zero只新增同一RM_MEMORY_EFFECT_CMO_ZERO枚举，MMIO复用STORE，不复制overlay算法，不逐子item推进cursor/history。失败SC后合法zero/MMIO覆盖不再产生旧无写承诺误报。

##### 3.4 NC后端拒绝范围明确

NC支持后端先接受写再按既有权重注入denied，因此保留提交overlay与后台通知语义一致。真正backend拒绝是本轮不支持的配置/激励组合，在响应入队/驱动前uvm_fatal终止，不能仅return悬挂或误报DUT；MMIO/Uncache zero仍支持后端拒绝和部分效果。此范围仍满足各类协议合法、参数化注错需求，未把NC denied支持删掉。

##### 3.5 reqSource与既有FIFO复用

reqSource权威枚举当前CPULoadData/CPUStoreData/CPUAtomicData/L1DataPrefetch为2/3/4/6，与utility枚举及已有interface/xaction/monitor/connect一致；只补responder快照复制及公共常量。Store-origin含混合Load保守正常，不强制重建完整MSHR消费者；其余类型保留对应注错权重。env已有mon2rm FIFO和RM唯一get路径用来归档，不新增竞争consumer或无对应actual的通用scoreboard流量。

#### 4. R1两组观察契约复审

##### 4.1 错误数组真实读写/旁路/版本生命周期

统一方案第6.13节明确：

1. S0真实error-array read.fire保存idx与每way候选，当前s0写优先于上一拍s1写；当前V2仅一个物理写口，数量来自实际接口宽度。
2. 写接受只建立pending origin，真实s1写提交才改变committed slot索引，来源raw须独立从L2 token的已送达beat取得并核对actual写值。
3. LoadPipe S1按真实hit way消费；寄存旁路来源保持，无旁路按实际消费时array(latched_idx)的committed来源，不能S0统一冻结旧数组版本。
4. MainPipe按GatedValidRegNext(s0_fire)首个S1响应拍及真实s1_tag_ecc_match_way选来源，之后与s1_extra_meta一样保持，不等待s1_fire才取latest。
5. 同批替换/失效先撤销旧行身份再建立新来源，避免新版本被同拍清理；实际errorArray来源仅随写提交变化。已经冻结的读/响应引用保留至attempt/terminal及安全消费水位，再单调回收。有效hit/资格mask限定了版本查询范围，无真实匹配或明确无该输入贡献时取0，不要求虚构slot版本。
6. 当前V2 error_flag_write来自有效miss/refill链；非refill error写作为来源不变量失败，不用实际错误位反推一个未定义来源。

这与AsynchronousMetaArray的s0/s1旁路优先及延迟数组更新、LoadPipe请求/响应阶段、MainPipe的s1_extra_meta保持逻辑一致。观察器仅保留实际slot身份、pending origin与不可变引用；不做缓存数据、tag搜索、命中或替换算法，功能边界与最小适配目标相容。

##### 4.2 原子最终响应与独立raw合成

统一方案第6.3/6.14节现已精确区分AtomicsUnit三类真实响应：miss&&replay重试，miss&&!replay等待，只有resp.fire&&!miss锁存最终结果。原始owner/attempt链及MainPipe ack_miss_queue/s3_req.miss、miss_id在最终响应前保留。

最终expected raw分别从本次refill origin和读时slot flag origin取得，按bit OR后核对actual tl_error，之后才按denied优先和CSR enable映射异常。这与MainPipe.scala:882和AtomicsUnit.scala:347-394一致，不会因最终resp.miss=0误判纯hit，也不会把两项来源先独立映射而错误生成两个cause。SC失败不会作为没有消费错误的依据，enable=0仍检查raw、返回值、CAS/SC及合法效果。

#### 5. 三方案整体一致性与最小适配确认

- CMO仍必须从真实monitor队列核对zero/clean/flush/inval请求，owner/token只能由实际锁存身份和传输关联建立。cacheable zero保留wline→真实SBuffer slot→DCache接受与成功链，允许合并和replay，不要求命中路径出现TL A。
- 数据来源与错误输入资格分开；全字节SQ/SBuffer forwarding不自动屏蔽同时有效的DCache错误。每项资格来自真实有效/kill/replay/active/预取控制，期望raw来自独立origin，最终exception不作为推导输入。
- Uncache zero完整保留8B分段连续前缀、逐响应停止、首错后迟到额外段检查和已接受写事件效果。正常8段与fault部分效果使用同一写底座一次应用；错误段先被后端接受后注错时仍纳入，不整行回滚。
- denied/corrupt获取、映射、Uncache部分效果及通知比较只有统一方案一份实现；CMO负责请求、权限、STA0/flushPipe/terminal和唯一commit。两类zero效果入口互斥，无重复entry或双重消费真源。
- Uncache通知按实际Store D.fire独立构造期望，核对两拍sample、enable、地址、重复次数和reset代际；该服务不阻塞程序cursor，不模拟BEU。
- RM不生成redirect、UID+1恢复、epoch或重发；fault终态不等normal pass。普通cacheable Store/zero在生产端硬排除注错，历史错误不能绕过。
- CMO后续Load验收已收窄为无独立异常时比较程序顺序数据；zero命中可能保留旧error flag的情况交统一错误来源规则，不强制后Load正常。

#### 6. 评审状态与后续验证边界

P1必改项：无。P2必改项：无。P3必改项：无。

原五项及R1两组补充契约均已在原三个方案中落实，无需另开设计重写。按当前方案coding时应执行文中既定编译、soft定向与真实DUT验证，覆盖首/末beat、同拍旁路/槽位替换、原子三类响应、SC覆盖、NC配置fatal和Uncache首/中/末段fault；结果按原方案记录CSV后才能登记实现通过。本轮未生成或修改测试代码，无实现与方案对齐通过声明。

## 5. 主agent最终复核与静态检查

主agent重读R1/R2反馈及对应方案，复核errorArray真实旁路、MainPipe S1保持、AtomicsUnit最终响应和commit_memory_effect三项写副作用。确认三份方案的函数归属、source/token/slot版本与生命周期、normal/fault效果互斥一致。

已完成：三份plan及本报告标题/代码围栏、函数抽象描述、中文正文、引用存在性、尾随空白检查；新文件用no-index检查，避免未跟踪文档绕过git diff；最终hash与R2基线核对。`git diff --check -- AI_DOC AGENTS.md mem_ut/ver/ut/memblock/rule`通过。

未执行：源码修改、EDA编译、soft/真实DUT仿真、CSV通过状态更新。方案可coding不等同这些验证已通过；实现后按三份方案的定向矩阵和远端流程验证。

## 6. Plan对齐检查

本轮仅修订原三份plan，原五项与R1两组契约均落实，最终独立复审与主agent复核无已知必改项。无需新建替代plan，不删除原始review基线；plan保留undo，避免与已实现状态混淆。

## 7. 实现与Plan不一致项

本轮未coding，不作当前实现与修订plan一致性通过声明。旧源码仍为待执行基线，不能把计划尚未执行记为本轮实现遗漏。

## 8. Plan未说明但Coding落实的细节

本轮未coding，此项为空；所有本轮设计修复均已回写三份方案正文。

## 9. 非本次修改的逻辑分析

`git status --short`中其它既有修改保持原样：RTL知识索引与CBO分析文档、其它框架/原子/虚拟化plan及旧review、框架设计DOCX/审计文档、图像等均非本轮修复范围。本轮未修改RTL知识文档、测试配置、脚本或仿真/生成产物，不将这些用户既有文件纳入功能通过结论。
