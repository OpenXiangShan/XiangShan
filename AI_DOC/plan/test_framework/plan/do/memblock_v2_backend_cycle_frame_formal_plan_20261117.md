# MemBlock V2 后端周期帧同步驱动与异常地址采样正式方案

- 正式稿创建日期：2026-11-17；初稿日期：2026-11-16。
- 更新日期：2026-11-17。
- 适用分支：`mem_ut_uvm_v2`，使用 V2 profile。
- 状态：本项 coding、文档同步、必要专项验证及独立 review 已完成；2026-11-17归档。未穷举的交错见末章验证边界，全 basicTest 回归在三项完成后统一执行。
- 本轮授权：用户明确要求顺序执行三方案 coding、分项本地提交及专项和 basicTest 回归；本项不修改 RTL。
- 定位：在现有公共框架中模拟后端各来源级数对应的周期 ROB 控制和 redirect 输出，并记录其实际输入和异常地址消费边界。
- 文件入口：新建正式版本；原初稿保留供对照。
- 关联方案：[MemBlock V2 虚拟化指令公共测试框架支持方案](../undo/memblock_virtualization_framework_support_formal_plan_20261117.md)。

- 评审记录：[独立评审闭环记录](../../review_doc/undo/memblock_v2_virtualization_backend_formal_plan_review_20261117.md)。

## 1. 专有名词与抽象功能说明

### 1.1 专有名词

| 名词 | 本方案中的含义与代码落点 | 示例 |
|---|---|---|
| DUT、RTL、profile | 被测 MemBlock、硬件实现及版本差异基线；本方案使用 V2 | 驱动端口以 `build/rtl/MemBlock.sv` 为准 |
| UVM、agent | 验证框架及接口组件 | 保留 LSQ commit、redirect、ctrl agent |
| transaction、item、projection | driver 传输对象及完整快照中属于一个 agent 的字段投影 | 同一帧投影成两个 item |
| sequence、vseq、driver | 运输序列、顶层调度序列及接口驱动组件 | vseq 在真实 agent sequencer 上启动两个运输序列 |
| clocking block、`drv_cb`、`mon_cb` | interface 的驱动与采样边界，包含调度区及输入/输出偏移 | driver 回执不等于 DUT 实际采样 |
| ROB、LQ、SQ、LSQ | 重排序队列、Load 队列、Store 队列及访存队列总称 | 同步异常本身不计入正常 scommit |
| ROB key | `flag/value` 组成的完整 ROB 标识 | `pendingPtr` 必须与 pending 队首一致 |
| UID、`dynamic_epoch` | 主表逻辑指令编号及其动态执行代次 | 同 UID 重发后的事件不能与旧样本关联 |
| reset epoch、`reset_epoch` | 复用当前 redirect 生命周期的 `get_l2tlb_current_reset_epoch()`；与独立 control topology 的 reset 编号不混比 | 一侧 ready 后发生 reset，应取消整帧 |
| backend cycle frame、周期快照 | 同一 MemBlock 输入周期的不可变 ROB 控制与 redirect 组合，分别保存来源决策拍，计划类型 `memblock_backend_cycle_frame_t` | 同步 fault 帧中 redirect 有效、正常提交数量为零 |
| `frame_id` | 周期快照的运输身份，与 reset 代次合用 | 两个 driver 的回执必须指向同一帧 |
| candidate、prepared、ready | 尚未提交的候选快照、已装载待发投影、driver 已保留该投影的准备确认 | 两侧 ready 之前不发布新字段 |
| armed、committed、drive edge | 已准备驱动、已经最终确定目标边沿、实际驱动边沿 | 不能在一侧已经驱动后单独撤销另一侧 |
| T0、T1、R | T0 为帧最终确认拍，T1 为共同输出边沿；R 为讨论真实后端 rollback 的输出周期 | 同步 fault 两投影同源；rollback 的 LSQ 投影可来自 R-1 |
| tail、强制尾拍 | rollback发布单元中预先装载的紧邻下一周期阻塞输出，随主帧一起准备和提交 | R+1不用重新等待双侧ready |
| source decision、pipeline、rollback | 字段来源决策拍、有界寄存级状态、访存违例冲刷重发；计划用 `lsq_source_seq/redirect_source_seq` 记录 | rollback 不建立 AF/PF fault token |
| AF、PF、GPF | 访问错误、页错误和 Guest 页错误；以真实 WB exception 为准 | 单有预期 PF 配置不能生成 fault redirect |
| control head、batch、reservation、preflight | 控制指令队首、一组正常提交、准备预占、发布前只读检查 | Fence 完成后通过 control 分支推进 cursor |
| recovery eligible、address source | 允许恢复扫描的记账门槛、实际地址优先来源 | fault sampled-applied 后才放行恢复；AMO 不读裸 LSQ 样本 |
| S、`sample_seq`、anchor | DUT 实际消费输入的公共采样编号及观测锚点 | 两个输入 monitor 的样本必须属于同一个 S |
| S+2、地址消费窗口 | 当前 V2 普通 LSQ 路径、边沿前采样约定下的目标地址样本 | 不用邻拍或 latest 替代指定样本 |
| fault head、fault token | 异常队首及框架保存的该次身份/生命周期 | token 保存 UID、ROB key 和执行代次 |
| STA、STD、WB | Store 地址、Store 数据与真实写回 | 准备 Store 异常帧仍需满足既有真实 WB 门槛 |
| redirect、payload、intent | 冲刷控制、硬件字段集合、待仲裁请求 | 继续由原最老仲裁选出获胜请求 |
| exception event | 与 fault token 及 intent 绑定的异常上下文 | 地址记录关联到具体异常代次 |
| sideband、level、pulse | 附随控制、电平保持、单拍脉冲 | 等待时保留 pendingPtr，但不能重复发送 scommit |
| flushSb、Fence | StoreBuffer 排空输入及其上游控制单元 | flushSb 不因异常帧存在而自动置位 |
| monitor、adapter、handler、scheduler | 原始采样、样本关联、状态处理和本方案周期发布协调器 | scheduler 不新增一套 redirect 年龄仲裁 |
| snapshot、CSR | 冻结状态及控制寄存器上下文 | 引用已有 issue CSR snapshot，不另建镜像 |
| active map、owner、cursor | 当前资源索引、资源所有者和顺序推进位置 | 回收 mapping 后仍需保留不可变事件记录 |
| queue、map、inflight | 待处理队列、关联索引、已发出未完成状态 | 有界保存帧和短期原始样本 |
| terminal、recovery、replay | 最终完成、异常恢复及重试 | reset/覆盖取消与正常恢复分别收口 |
| VA、GVA、GPA、PTE | 虚拟地址、Guest 虚拟地址、Guest 物理地址、页表项 | GPA 也可能表示隐式 VS 页表访问地址 |
| HLV、HLVX、HSV | 特殊 Guest 读取、按执行权限读取、特殊 Guest 写入 | 对应公共标量路由具备后复用异常帧 |
| AMO、segment、VLS | 原子操作、向量 segment 和向量访存 | 独立异常地址来源不能直接套普通 LSQ 来源假设 |
| RM、checker、scoreboard、coverage | 独立参考结果、正确性判断与覆盖率组件 | 本方案只记录实际样本，不推导 expected GPA |

### 1.2 关键组件抽象职责

| 组件 | 抽象功能职责 |
|---|---|
| `lsq_commit_handler` | 选择正常提交与异常队首，形成候选 ROB 状态，区分异常准备、发布记账和恢复收尾 |
| `exception_redirect_replay_handler` | 复用原年龄规则仲裁 redirect，提供唯一获选请求及其上下文 |
| `memblock_backend_cycle_scheduler`（计划新增） | 将一致的 ROB 状态和获选 redirect 组成周期快照，协调双投影准备和共同边沿发布 |
| `common_data_transaction` | 保存帧、不可变上下文、回执及既有资源状态，不重复实现 ROB/LSQ allocator |
| 两个 agent sequence | 将同一帧的各自投影运输到真实 sequencer，不独立重算后端决定 |
| LSQ commit driver | 唯一驱动原有 LSQ 控制和 `isStoreException`；独立合成 flushSb |
| redirect driver | 唯一驱动原有 `io_redirect_valid/bits`，与另一侧在同一目标边沿发布 |
| 输入 monitor、ctrl monitor | 记录实际输入快照、地址输出和公共采样序号，不推导 UID 或结果正确性 |
| `dispatch_monitor_event_adapter` | 关联两侧输入回执、实际样本及地址窗口，发布记录完成或明确取消状态 |

## 2. 目标、边界与当前实现

### 2.1 目标

本方案在 MemBlock 输入边界按完整周期帧同步发布 ROB 相关控制和 redirect。普通 ROB 队首同步 fault 的两组输出来自同一决策拍；访存违例 rollback 的 redirect 与 ROB 寄存输出有不同来源级数，必须分别保存来源拍，再组合为同一目标输入周期，不能声称所有来源均为同一决策拍。T0 表示最终确认帧的拍，T1 表示双方共同输出边沿。

这里的“同拍”是同一目标采样边界上的一致状态，不要求所有信号同时为 1，也不要求所有电平都跳变。类别提前正确并保持在功能上可以成立，但默认整核时序模拟应发布一份完整、可归属的周期快照。

本版不采用单信号驱动归属迁移。`isStoreException` 继续属于 `lsqcommit_agent`，redirect 继续属于 `redirect_agent`；同步由公共周期调度保证。

### 2.2 当前实现的问题边界

2026-11-16 检查的现有公共路径是：

```text
build_lsqcommit_xaction
  → LSQ item finish_item 返回
  → mark_fault_rob_commit_uid
  → request_fault_head_redirect / submit_redirect_intent
  → 原异常处理器仲裁
  → redirect sequence / driver 后续发布
```

因此，当前可以先发布类别及 LSQ 状态，后发布异常 redirect。不能仅据此判定 DUT 功能错误，但该路径不满足本方案声明的整组后端同周期发布目标。

如果直接在这个顺序上增加“LSQ driver 等待 redirect driver ready”，会因 redirect 请求尚未生成而循环等待。准备阶段必须在双边正式发布之前完成。

### 2.3 实际 DUT 端口边界

当前 V2 生成的 `build/rtl/MemBlock.sv` 中，本方案驱动的 ROB 相关端口是：

- `io_ooo_to_mem_lsqio_scommit`。
- `io_ooo_to_mem_lsqio_pendingMMIOld`。
- `io_ooo_to_mem_lsqio_pendingst`。
- `io_ooo_to_mem_lsqio_pendingPtr_flag/value`。
- `io_ooo_to_mem_isStoreException`。
- `io_redirect_valid` 及当前实际暴露的 `bits`。

Scala 中存在 `lcommit/commit/pendingld/pendingVst/pendingPtrNext` 等成员，但当前生成顶层没有对应端口。本方案可在模型内部表达必要语义，不添加虚构连接，不修改 RTL 以恢复这些端口。

`io_ooo_to_mem_flushSb` 物理上仍由 LSQ commit agent 驱动，逻辑上属于独立 Fence 请求，不是 ROB 异常帧字段。

### 2.4 范围

实现公共快照构造、双边运输、共同边沿发布、实际输入观测、普通 LSQ 异常地址记录和既有恢复衔接。不新增指令集合、SPVP/fence 权重、RM 地址推导、功能覆盖率模型或 RTL 代码。

HLV/HLVX/HSV 在公共路由支持后复用对应标量规则。AMO、segment、misalign overwrite 的独立地址来源与 VLS 特殊恢复按既有约束处理。地址记录按来源分开配置内部生命周期：仅已证明普通 LSQ 来源需要本方案 S+2 记录；其余来源标记为本轮不记录，不创建永远不能完成的地址等待，不宣称已支持对应地址检查。

## 3. 后端状态快照的源码合同

本章只记录测试框架方案需要的接口依据，不更新 V2 RTL 知识库。

| 依据 | 源码落点 | 框架约束 |
|---|---|---|
| ROB 同步异常建立 `exceptionHappen`，类别用 `RegEnable` 保存 | `src/main/scala/xiangshan/backend/rob/Rob.scala:651` | 只有本次已确定异常才能更新类别；其余保持 |
| ROB flush valid/bits 在 CtrlBlock 寄存一拍 | `src/main/scala/xiangshan/backend/CtrlBlock.scala:104` | 对应异常 redirect 和类别在 T1 提供同一事件 |
| `lcommit/scommit/commit/pending*/pendingPtr` 各自由同一 ROB 拍寄存 | `src/main/scala/xiangshan/backend/rob/Rob.scala:838` | 正常提交、pending 和指针必须来自一致决策快照 |
| 普通同步异常阻止正常提交 | `src/main/scala/xiangshan/backend/rob/Rob.scala:781` | 异常帧的正常提交数量和对应 pending 标志为 0 |
| 类别直接取异常包 commitType 的低位 | `src/main/scala/xiangshan/backend/Backend.scala:838`、`src/main/scala/xiangshan/package.scala:174` | 不把该位当独立异常 valid，不脱离访存异常语境解释 |
| 顶层连接没有再增加独立 LSQ 输入延迟 | `src/main/scala/xiangshan/XSCore.scala:235` | 以 MemBlock 顶层输入定义同步边界 |
| flushSb 来自 Fence 接口 | `src/main/scala/xiangshan/XSCore.scala:228` | 不与 ROB fault 的发生或完成互相替代 |
| pendingst 和 pendingPtr 被联合比较 | `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:851` | 不得 pending 描述 A、指针却描述 B |
| 类别寄存后选异常缓冲，地址再寄存输出 | `src/main/scala/xiangshan/mem/lsqueue/LSQWrapper.scala:253`、`src/main/scala/xiangshan/mem/MemBlock.scala:1996` | 普通地址记录使用明确采样合同 |
| redirect 输入在 MemBlock 内再寄存 | `src/main/scala/xiangshan/mem/MemBlock.scala:405` | 缓冲失效与输出保存的先后关系不能混淆 |

## 4. 各类周期快照的字段规则

### 4.1 普通同步访存异常

```text
pendingPtr        = 故障队首 ROB key
isStoreException  = 故障访存操作类别
scommit           = 0
pendingst         = 0
pendingMMIOld     = 0
redirect.valid    = 1
redirect.robIdx   = 故障队首 ROB key
redirect.level    = flush
```

模型内部如保存 `lcommit/commit`，该普通同步异常拍同样为 0。这里不把异常指令计入正常提交，也不通过 pending 标志触发异常访存继续执行。

### 4.2 正常提交与 pending 等待

| 场景 | 快照规则 |
|---|---|
| 正常提交 | 按既有正常提交 batch 生成实际 scommit 数量；pending 与 pendingPtr 来自同一队首快照 |
| 队首 Store 尚未完成 | 可存在 pendingst=1、scommit=0；不能把 pendingst 当作正常提交完成 |
| MMIO Load 等待 | pendingMMIOld 与 pendingPtr 对应同一待处理队首 |
| 没有新异常 | `isStoreException` 保持上一次已发布值，不跟随普通 Load/Store 候选改变 |
| 无新驱动帧的空闲拍 | 保持必要电平，清除单拍脉冲；不重复提交上一帧 |

### 4.3 访存违例 rollback 帧

当前框架的 `MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION` 是冲刷重发，不是 AF/PF trap。即使来源名称相同，仍须保留其 `rollback_subtype` 和真实 `level/flush_itself`，不得统一改成 flush-itself。来源包括 LoadUnit、uncache 和 RAW nuke，不能只处理 Store/Load RAW 一种。

真实后端 R 周期中，`CtrlBlock.scala:764、772` 将同一个 redirect 送到 ROB 和 MemBlock，`Backend.scala:792` 直连输出；ROB 因 R 的 redirect 阻止当拍提交，但 `Rob.scala:843–853` 的 LSQ 输出是寄存值。因此必须区分：

| MemBlock 输入周期 | redirect 投影 | LSQ 投影及框架约束 |
|---|---|---|
| R | 原 memoryViolation 仲裁出的 payload | 来自 R-1 的 ROB 决策，可保留已经合法决定的 older commit；pending/head 也取其对应来源拍 |
| R+1 | 无新请求时 valid=0，类别保持 | 体现 R 的提交阻塞，scommit=0、pendingst=0、pendingMMIOld=0，pendingPtr 为 R 的队首 |
| 后续 walk/recovery | 服从既有恢复 owner | 在恢复门槛解除前不重新构造新正常提交；独立 flushSb 继续服务 |

scheduler 保存一个有界的前级 ROB 决策槽及来源编号。R 帧直接消费已保存的 R-1 决策，不在看到 redirect 后用 R 的 current head 重算替换；当前没有合法前级提交时可以是零提交，但不能为追求全零而取消已经实际驱动或已合法锁定的 older batch。最终发布前逐项确认该 batch 不在 `rob_need_flush()` 范围，失败则在任何输出前取消候选、重新仲裁；不得先发 scommit 再只撤销软件记账。

R+1 强制尾拍随 R 主帧作为同一个双边运输 item 的两段内容预装载，最终 COMMITTED 前两侧 ready 均确认主帧和尾拍齐备。主帧取共同目标边沿，尾拍固定取其下一物理边沿，不重新排队或等待第二轮 ready；普通候选不能覆盖尾拍。尾拍带自己的 `frame_id`、主帧引用及目标 sample，用于独立观测和排空；不生成第二次 redirect intent/accepted/cancel owner。其 LSQ 内容为零 scommit/零 pending、冻结的 R 决策队首；redirect.valid=0、类别保持。两个 driver 在尾拍驱动完成后才各自结束本运输 item；主帧的回执和采样记账仍按主帧时刻发布，不等待 item_done。reset 撤销整个发布单元。尾拍之后若仍处于 recovery/walk，driver保持阻塞投影的电平直至合法新帧替换。

尾拍指针从 R 来源决策的权威队首取得；存在 R-1 older batch 时，按该批决策后的 active status/ROB key 映射保存下一队首，不能复制 R 主帧的旧 pendingPtr、读取尚未应用的软件 cursor 或按 UID 算术推导 ROB key。尾拍不携带主帧的提交 UID 列表、control/fault 状态更新；尾拍采样仅完成运输观测，不再次执行 `mark_rob_commit*`、fault token 更新或恢复 owner 激活。

rollback 帧不要求 redirect UID 成为 fault head，不创建 fault token、不设置该 UID 的 fault `rob_commit/terminal`，不更新 `isStoreException`，不预约 S+2 异常地址。按原 redirect 范围释放/保留 owner，需重发的 UID 使用原动态代次推进和 issue 恢复。

### 4.4 控制指令与其他来源

control head 保留独立候选和采样后提交分支：沿用 `select_control_head_candidate()` 的终态资格，发布不产生正常 Store scommit 的控制帧，随后按原 `mark_control_rob_commit_uid()` 的语义推进 cursor。不能因改造旧 LSQ sequence 而删除该分支；Fence 的 flushSb 请求仍单独服务。

branch、纯 flushPipe 和中断不在本轮新增周期复刻范围内；保留既有专用语义，若进入共同运输必须先提供对应来源映射，不能默认为 AF/PF 或 rollback。中断不强行消费 LSU 地址。

### 4.5 AF/PF 异常与地址来源

普通 Load/Store AF/PF 从真实 WB 的异常信息进入 status，等待原正常提交前沿到达该 UID，再准备同步 fault 帧；只有预期配置不能代替 DUT 真实 exception。Store 的 STA/STD 真实 WB 门槛继续保留；尚缺另一侧 dispatch 时遵循 `head_pending_ptr_blocked()` → `store_fault_blocks_pending_ptr()` 的既有例外，使缺侧可继续入队和写回，不因单侧 fault 提前全局冻结。来源为普通标量 LSQ 且无高优先地址覆盖时，`address_record_required=1`，以实际 S 记录 S+2。

AMO 沿用当前框架 load-like 类别，即 `fault_uid_is_store_exception()` 返回 0；不得因为 exceptionVec 是 storeAF/storePF 就反推类别 1。其控制帧和恢复可复用同步 fault 分支，地址来源标为 ATOMIC。当前 V2 Load/StoreMisalignBuffer 的 overwriteExpBuf.valid 固定为 false，标量非对齐仍为 LSQ 来源；segment 单列来源。上述独立来源在本轮无明确观测合同时使用 `address_record_required=0`、`address_record_status=NOT_CAPTURED_BY_THIS_PLAN`，继续原异常恢复，不伪造普通 LSQ 地址记录；后续若扩展观测需另补来源与时序合同。

本范围标量不能确认普通 LSQ 来源时报告框架错误，不得默认声明记录成功或以未支持结案。`address_source` 必须来自既有路由/可确认的来源事实，无法判定则标 UNKNOWN、本轮不记录；这属于明确支持边界，不作为地址正确性通过。


## 5. 公共数据与所有权

### 5.1 周期快照

计划在公共 sequence types 中增加 `memblock_backend_cycle_frame_t`，包含：

| 字段组 | 语义 |
|---|---|
| `frame_id`、reset 代次 | 运输身份，拒绝复位前后的旧消息串接 |
| 决策类型、上下文版本、来源拍 | 区分正常提交、pending、control、同步 fault、memoryViolation rollback；分别保存 `lsq_source_seq/redirect_source_seq` |
| ROB 队首 key、提交 UID 列表 | 保存本帧决策时的身份，不从发布后的 cursor 重新读取 |
| LSQ 投影 | scommit、pendingst、pendingMMIOld、pendingPtr、isStoreException |
| redirect 投影 | valid、robIdx、level、isVlsException 等实际端口字段 |
| 可选 fault 上下文 | UID、动态代次、既有 intent 身份及已有 CSR/issue 上下文引用 |
| 地址记录状态 | `address_source`、`address_record_required` 和完成/未支持状态，独立于恢复资格 |
| 恢复发布状态 | 原 intent/cancel owner 身份、`sampled_applied`、`recovery_eligible`，不另建第二套 owner |
| 目标驱动边沿 | 两侧准备完成后最终确定，提交后不可各自修改 |
| rollback强制尾拍 | 同一运输item预装主帧与尾拍；尾拍另有frame身份及冻结R队首，目标固定为主帧下一边沿 |

业务字段在形成候选投影后不可由任一 driver 修改。若重新仲裁或队首变化导致内容失效，取消未提交帧并建立新 `frame_id`，不原地修改已经准备好的投影。目标边沿在最终提交时只填写一次。

### 5.2 下层运输元数据

两个 agent 的 xaction 增加不随机化的同步元数据：

```text
frame_valid
frame_id
reset_epoch
projection_kind
```

目标边沿及双边状态通过公共运输记录关联。底层 driver 只接触基础运输身份和自己的端口字段，不引用 `seq_pkg` 中的 UID 或复杂上下文，避免 package 依赖环。

`reset_epoch` 统一取当前 redirect 恢复采用的 `get_l2tlb_current_reset_epoch()`；已有事件字段虽名为 `control_reset_epoch`，此路径也按该公共编号匹配，不能调用仅限 control topology 的 getter 或将两个 reset 域数值比较。

共同物理驱动边沿由现有唯一主 service 中的 scheduler helper 依据公共物理 sample 锚点派生。它不是 dispatch service cycle，也不是给 CSR/L2TLB monitor 另建一套采样编号；实际 DUT 观测仍复用既有 `sample_seq`。

### 5.3 发布生命周期

```text
CANDIDATE：候选状态和获选控制原因已经组成一致快照
  → PREPARING：投影运输到两个 driver
  → READY：两侧均已保留待发数据，没有写到 DUT
  → COMMITTED：重新核对身份后，固定下一共同驱动边沿
  → DRIVEN：该边沿两侧均实际执行驱动并返回回执
  → SAMPLED：同一个实际 sample 观察到两侧一致输入
  → RECORDED / DONE：需要的地址记录及原恢复均收口
```

PREPARING/READY 仅占用轻量 reservation，不调用会建立 active_redirect、flush epoch 和 cancel record 的 `request_redirect_flush()`，不发送 accepted；因此最终提交前取消只撤销准备状态和 intent 预约。

最终确认和原 owner 激活在唯一 scheduler 的同一次无阻塞状态转换内完成：完成来源/年龄/提交 preflight 后，恰好调用一次原 `request_redirect_flush()`，绑定既有 intent/cancel record，并送 accepted，再置 COMMITTED。最终确认失败则不分配 active owner；不得为取消 prepared 帧回滚一个已经激活的真实恢复 owner。

READY 之前或最终提交之前可以整体取消。COMMITTED 后不能因迟到请求只撤销一侧；除 reset 等使整个边界失效的情况外，应完成该次发布，再按后续帧/恢复规则处理新事件。单侧缺失属于框架错误，不能把另一侧延后一拍补发并声称成功。

### 5.4 数据保存边界

`common_data_transaction` 保存完整帧和不可变业务上下文；公共 sync 通道保存底层 ready、共同边沿、驱动回执和 raw 采样。原有 redirect owner、cancel record、LSQ allocator 仍是资源处理权威。

准备中的帧通过轻量 reservation 阻止同一提交候选重复使用，但不提前把它写成实际已发布/已采样。不得在等待 driver 时推进正常提交 cursor 或向 RM 声称实际输入已经生效。

获选 redirect reservation 同时填补 intent 离开队列到 COMMITTED 激活 owner 之间的空窗：现有 `has_pending_monitor_redirect()` 等门槛必须能读到它，禁止新提交跨过待冲刷边界。route/issue 对其覆盖范围采用既有 pending redirect 的抑制语义；真实 WB、monitor 收集、原取消事实及独立 flushSb 仍持续服务，不能将观察事件丢弃。已经合法保存且不被覆盖的前级 older batch 保留。最终提交前 arrival 的更老 intent 仍进入原仲裁并可整体取消旧准备；未提交 reservation 不递增 flush epoch、不改变 dynamic_epoch。


## 6. 目标功能流程

### 6.1 流程图

```mermaid
flowchart TD
    A["公共 monitor service：消费真实 WB 与状态"] --> B["lsq_commit_handler：准备 ROB 控制候选"]
    B --> C["原 exception/redirect handler：年龄仲裁"]
    C --> D["backend_cycle_scheduler：形成一致候选帧，计划新增"]
    D --> E["LSQ 投影 → lsqcommit sequence / driver"]
    D --> F["redirect 投影 → redirect sequence / driver"]
    E --> G["保存待发投影并确认 ready，不写 DUT"]
    F --> H["保存待发投影并确认 ready，不写 DUT"]
    G --> I{"两侧 ready 且身份仍有效"}
    H --> I
    I -->|失效| X["取消整帧并回收准备状态"]
    I -->|有效| J["最终确认 T0，固定下一共同驱动边沿 T1"]
    J --> K["一次激活原 redirect owner；两个 driver 在 T1 各写自己的端口"]
    K --> L["两侧驱动回执 + 同一 sample 的实际输入"]
    L --> M["adapter 确认整帧采样，单次发布记账"]
    M --> N["已确认普通 LSQ fault：独立保存 S+2 地址样本"]
    M --> O["记账后放行既有 redirect / cancel / deq 恢复"]
    N --> Z["恢复和所需记录均收口后最终排空"]
    O --> Z
    P["独立 flushSb 请求服务"] --> Q["LSQ driver 每拍输出合成"]
    Q --> K
```

**函数调用流程图整体文字伪代码：**

```text
1. 主 service 按完整 sample 收集事实，先关联已驱动帧并完成其采样记账，
   再处理依赖这些状态的 deq/恢复扫描；同 sample 真实 WB semantic batch 在应用帧之前按原顺序更新 status。
2. commit handler 准备正常提交或 fault head 候选；准备只保留资格和身份，
   不先驱动 LSQ item，也不以 item_done 作为产生 redirect 的前提。
3. 原异常处理器完成年龄仲裁；只预占获选请求。scheduler 按来源级数构造帧：
   同步 fault 使用同源状态，rollback 使用有界前级 LSQ 决策，control 保留独立分支。
4. 两个现有 sequence 向各自真实 sequencer 运输同一帧的投影。
   两个 driver 保存待发字段并返回 ready，等待共同发布，不提前改变输出。
5. scheduler 在共同边沿前重新检查 reset、代次、候选、提交范围和仲裁结果。
   失效就取消准备；有效则一次激活原 redirect owner，固定下一共同驱动边沿，记为 T0。
6. 两个 driver 在下一边沿 T1 执行各自投影，保持单一端口写入者并分别发布回执。
7. monitor 记录实际输入 sample。adapter 配对同一帧和同一 sample，
   满足后才完成该帧唯一的实际发布/采样记账，不重复应用提交数量。
8. 先以 sampled-applied 放行恢复；地址记录独立以实际 S 保存指定样本。
   不等待 S+2 才记账；最终收尾等待原恢复和需要的地址记录都完成。
9. flushSb 请求由独立公共服务持续推进，只在 LSQ driver 最终输出合成时汇入，
   不因异常帧存在而自动产生，也不因等待同拍准备而停掉请求服务。
```

### 6.2 双边准备和共同边沿

共同边沿必须在两侧 ready 后选择，不能发送两个 item 后凭经验假定它们会在同一拍被取走。

现有唯一主 service 在两侧 ready 后的共同 `negedge` 完成最终确认，并固定下一 `posedge` 为目标。driver 已经处于逐拍服务待发状态，因此该目标边沿是未来边沿，不依赖同一 delta 内的事件唤醒顺序。两侧 `drv_cb` 使用同一物理时钟和相同输出偏移。

两侧 item 的随机 pre/post gap 不再改变同步帧的目标边沿；需要延迟的运输只能发生在最终提交之前。driver 的等待采用逐拍非阻塞状态处理，不在主循环里阻塞等待另一侧，从而允许 reset、idle 及独立 flushSb 持续服务。

### 6.3 发布与实际采样

T1 表示 driver 输出边沿，S 表示后续 monitor 实际观察到 DUT 消费该组输入的编号。driver 回执记录边沿时间及帧身份；adapter 根据既有物理采样锚点把该时间映射到预期输入 sample，拒绝驱动之前的旧电平记录。

同一 frame 的两侧输入必须属于同一 S。只看到两侧数值最终相同但 sample 不同，不能判定同步发布成功。连续两帧字段完全相同也必须根据运输身份及目标 sample 分别处理，不依赖输入跳变。

## 7. 关键函数与 task 修改逻辑

新增名称均标为计划接口，不表示当前代码已经存在。各函数采用公共 map/queue 或当前活动窗口，不扫描整个主表。

### 7.1 `lsq_commit_handler::prepare_backend_cycle()`（计划新增）

**抽象功能描述：** 在模拟后端决策准备阶段，根据当前提交前沿产生可用于仲裁和快照构造的 ROB 候选状态，保留对应身份，不执行接口发布或最终提交记账。

**输入与副作用：** 输入当前 status、cursor、已确认 WB 和控制状态；输出正常 batch 或 fault 候选及其上下文版本，建立必要的候选 reservation。

**源码级伪代码：**

```text
读取当前合法提交前沿，按原规则选择正常 batch、control head 或 fault head。
维护有界前级 ROB 决策槽，用于 rollback 对应的 LSQ 寄存输出；不全表重算。
普通 batch 保存 UID 列表、旧 head key 和本次提交数量。
fault 候选检查异常资格、ROB key、执行代次和 Store 双真实 WB。
Store 尚缺另一侧 dispatch/WB 时，不创建阻塞它的 fault reservation；
  沿用原 pending_store_fault barrier 和缺侧 dispatch 例外，继续发布合法 pending/head。
  保留 SQ owner、STA late tombstone 和动态身份，不提前 terminal。
AMO 沿用 final WB/atomic feedback/required pieces fired/atomic finalized 资格。
将上下文标记为 prepared，关闭该候选被重复选择的入口。
control 候选冻结原完成资格及 UID/key，保留后续独立提交分支。
为 fault 提交既有 redirect intent；不要求先完成 LSQ item。
返回候选；不推进正常 cursor，不发布新类别，不记录 actual sampled。
```

**中文文字伪代码：** 当前 `mark_fault_rob_commit_uid()` 中资格检查与身份保存需拆到准备阶段。原来“先完成 LSQ item 才能生成 redirect”的依赖删除。异常候选保存期间只保留必要状态，最终是否发布由仲裁及共同边沿确认决定；候选被取消时回收 reservation，不能留下虚假的提交完成。

### 7.2 `exception_redirect_replay_handler::process_pending_events()`

**抽象功能描述：** 继续按既有年龄规则处理控制请求，将唯一获选 redirect 及其原因交给周期调度器，并维护既有恢复 owner 边界。

**输入与副作用：** 输入原事件队列及 prepared fault 上下文；输出获选请求，不再独立推动本次 redirect 提前发送。

**源码级伪代码：**

```text
按原 reset、动态代次、年龄规则过滤并选择 redirect。
Store fault 仍需真实 WB 和合法 fault head 资格。
将原 store_redirect_waits_fault_commit 门槛拆解：
  准备资格未满足时继续等待；
  仅 UID、ROB key、dynamic_epoch 全部匹配且真实 WB 齐备的 prepared fault 可放行，
    不再等待尚未发生的 LSQ item_done 或正式发布记账；不能用全局prepared位代替匹配。
  其余 Store redirect 保持原资格门槛。
获选结果绑定原 intent/ROB key 和 prepared 上下文，交给 scheduler。
本阶段只保存获选 intent 与 prepared 身份，不调用 request_redirect_flush、
  不置 active_redirect/flush epoch、不分配 cancel record、不发 accepted。
COMMITTED 时才激活原 owner；advance_active_redirect 除原 drive/anchor/cancel 门槛外，
  还须等待本帧 sampled_applied/recovery_eligible。
不创建第二条绕过 frame 的 redirect 驱动请求。
```

**中文文字伪代码：** 修改的关键是消除循环等待，同时保留 Store fault 不能绕过真实完成门槛的约束。外部 monitor redirect 的历史覆盖、原子恢复和 covered fault 继续使用既有记录，不能为了形成帧而重建第二份 cancel owner。仲裁结果在正式提交前变化时，应取消未提交候选并重建。

### 7.3 `memblock_backend_cycle_scheduler::build_cycle_frame()`（计划新增）

**抽象功能描述：** 将一致状态版本的 ROB 候选和已获选控制请求组合成完整快照，为两个 driver 提供属于同一输入周期的两侧投影。

**输入与副作用：** 输入候选 ROB 状态、仲裁结果、已发布类别及版本信息；输出一份新 frame 及两侧投影，不修改实际端口。

**源码级伪代码：**

```text
确认候选与仲裁结果属于同一有效状态版本。
普通同步 fault 使用异常帧：故障 head、正确类别、零正常提交及零对应 pending。
正常 batch/pending/control 使用冻结资格和对应来源拍的队首快照。
memoryViolation 使用 rollback 帧及已保存前级 LSQ 决策，不要求 fault head；
  冻结 level/flush_itself/subtype/dynamic_epoch，并预约下一拍的阻塞输出。
其他已支持来源按专用映射合成；未定义来源不能冒充本轮支持。
按实际路由标记 address_source；仅明确普通 LSQ 来源预约 S+2。
没有新异常包时沿用已发布类别。
生成新 frame_id，复制业务字段与上下文引用。
分别生成 LSQ、redirect 投影，保留相同 frame_id/reset。
不将 flushSb 请求身份或脉冲并入 ROB 决策字段。
```

**中文文字伪代码：** pendingPtr 从构造该帧时保存的 head 取得，而不是等 driver 发送时再读已移动 cursor。旧类别由“上一次真实已发布帧”提供，不能由未获选 fault 候选覆盖。改变已 prepared 内容必须换新帧，不能让两个投影在不同时间看到不同值。

### 7.4 `memblock_backend_cycle_scheduler::service_frame_publish()`（计划新增）

**抽象功能描述：** 协调同一帧两侧准备状态，选择共同未来边沿，并确认发布回执；它不直接写 DUT，也不另行仲裁 redirect。

**输入与副作用：** 输入两侧 ready、reset 与状态版本；更新完整帧状态和共同目标边沿，产生提交或取消通知。

**源码级伪代码：**

```text
缺任一 ready 时维持 PREPARING，不发布任何半帧。
两侧 ready 后，在最终确认点复核 reset、动态代次、head 和获选请求。
失效则发布整帧 cancel，等待两侧解除准备后回收该帧。
有效则完成 batch 覆盖范围 preflight；有 redirect 时在同一无阻塞转换中
  一次调用原 request_redirect_flush，绑定该 frame/intent/cancel owner 并发 accepted。
确定下一共同驱动边沿，更新为 COMMITTED；该边沿不可由单侧调整。
收集两侧实际驱动回执，要求 frame/reset/edge 一致。
rollback主帧与预装尾拍分别确认：两者固定相邻边沿，尾拍不得再次等待ready。
主帧驱动回执收齐即可进入主帧采样和恢复门槛；最终排空还需尾拍确认。
若只有一侧发布、目标边沿被错过或身份不符，报告框架错误，不补发邻拍。
两侧真实驱动回执收齐后，统一路径仅调用一次 mark_redirect_drive_done，
  保留原 anchor deadline、cancel record 和真实驱动时刻；无redirect则不调用。
两侧驱动完成后进入实际输入 sample 确认阶段；原 sequence不再单独mark drive done。
```

**中文文字伪代码：** T0 是最终决定所属拍，准备等待不被计作已经完成的后端决定。只要帧尚未最终提交，更老事件可以使它失效；帧提交后新事件由后续帧或合法 reset 边界处理，不能半途只撤销一个 driver。正常流水可准备后续候选，但必须受现有前沿和有界运输容量约束，不能重复使用尚未确认的提交项。

### 7.5 两个现有公共运输 sequence

涉及 `memblock_lsqcommit_dispatch_base_sequence::send_lsqcommit_cycle()` 及 `memblock_redirect_dispatch_base_sequence::drive_redirect_payload()` 等入口。

**抽象功能描述：** 两个 sequence 负责将 scheduler 提供的投影运输到各自真实 agent，不再各自决定本拍控制行为。

**输入与副作用：** 输入 frame 投影；输出对应 xaction，保留相同运输身份。原有主表和异常状态更新改由统一发布路径完成一次。

**源码级伪代码：**

```text
取得自己负责的一个 frame 投影。
复制实际端口字段和 frame_id/reset/projection_kind。
同步 item 不随机改变业务字段，也不通过随机 gap 改变目标边沿。
通过各自 start_item/finish_item 运输。
item_done 仅在该侧发布单元全部边沿驱动完成或明确取消后配对一次；ready不提前完成。
rollback主帧回执在主帧边沿发布，尾拍回执在紧邻边沿发布，item_done在尾拍之后。
item_done 不单独推进提交 cursor、fault terminal 或 mark_redirect_drive_done。
删除旧的“LSQ finish_item 后才生成本次 redirect”分支。
取消或 reset 后，不把旧投影当普通 item 继续发送。
```

**中文文字伪代码：** 两个 sequence 仍运行在 `p_sequencer.lsqcommit_sqr` 和 `p_sequencer.redirect_sqr`。实际字段选择在 scheduler 中完成，因此不需要把类别迁移到 redirect xaction。双方仍能打印自己的投影，但最终提交应用、故障资格与资源收尾不再因其中一侧先返回而提前推进。

### 7.6 两个 driver 的准备与逐拍驱动

涉及两个 `main_phase()`、`send_pkt()`、active idle 和 reset 路径，计划增加各自的 prepared frame 状态。

**抽象功能描述：** 每个 driver 保留自己的唯一端口写入权，暂存待发投影，并在共同目标边沿执行一次输出。等待期间继续提供必要的空闲和独立控制服务。

**输入与副作用：** 输入投影及公共发布状态；输出本 agent 的端口、ready 与实际驱动回执。

**源码级伪代码：**

```text
收到同步投影时保存字段；rollback投影必须连同强制尾拍完整保存才发布ready。
不立即 send_pkt；普通帧一个发布边沿，rollback发布单元两个连续边沿。
每个 drv_cb 都处理 reset/cancel、待发状态及本 agent 本拍输出。
若当前是已提交帧的目标边沿：
  将本投影字段一次性写入各自 clocking 输出；
  发布 frame_id/reset/实际边沿回执；
  确保同一帧只执行一次。
若当前是预装的 rollback 尾拍目标边沿：
  两侧写入该尾拍各自投影并发布其独立回执；主帧到尾拍之间不再等待运输。
  此尾拍优先于新普通候选，不能被idle保持旧pending值的逻辑覆盖。
否则：
  保持已发布的必要 level；
  正常提交数量和 redirect.valid 等 pulse 使用安全空闲值。
LSQ driver 同一最终赋值位置合成独立 flushSb 值。
reset 使两个 prepared 状态和公共帧一起失效，输出安全默认值。
```

**中文文字伪代码：** driver 不能阻塞在等待另一侧的 task 中，也不能在同一边沿先驱动 prepared 数据又被旧 idle 分支覆盖。每个端口每个边沿只有一次最终赋值。目标边沿和 clocking 输出偏移相同，因此两侧在 DUT 边界同步；metadata 仅用于框架运输，不增加 DUT 端口。

类别缓存仍保留在 LSQ driver，只在实际异常帧发布或 reset 时更新。清理 fault token、等待仲裁、普通 redirect 和 idle 都不清零已经发布的类别。

### 7.7 `service_flushsb_request()`（计划公共服务入口）

**抽象功能描述：** 继续运输既有独立 flushSb 请求，并向 LSQ driver 提供每拍待合成值，不依赖后端帧是否就绪。

**输入与副作用：** 输入既有 flushSb 请求队列和当前在途状态；输出独立请求的本拍值及驱动回执，不改变 ROB 帧。

**源码级伪代码：**

```text
沿用现有请求唯一 owner 和队列顺序。
没有请求时本拍 flushSb 为 0。
有合法待发请求时准备独立发送记录，交给 LSQ driver 本拍输出合成。
等待双边帧 ready 时，该服务仍由持续公共 service 推进。
收到实际发送回执后，沿用原等待 sbIsEmpty 和请求完成流程。
同一请求不得因 frame cancel、retry 或同一电平保持而重复发送。
```

**中文文字伪代码：** 当前 `send_lsqcommit_cycle()` 内部附加 flushSb 的逻辑需要解耦，避免该 sequence 阻塞于同步 item 时把 Fence 排空请求一起停掉。公共 service 只准备请求，LSQ driver 仍是物理写入者；不在其他 task 增加第二个 force 或直接写 interface。

### 7.8 monitor 原始发布与 `service_backend_cycle_samples()`（计划 adapter 入口）

**抽象功能描述：** monitor 保存实际输入事实，adapter 将同一个 frame 的双边回执与同一目标 sample 配对，完成生成/运输自洽确认。

**输入与副作用：** 输入两个 `mon_cb` 样本、公共采样编号及回执；输出 frame sampled 状态，不判断 DUT 运算结果。

**源码级伪代码：**

```text
LSQ monitor 每拍发布 ROB 输入字段，包括零提交数和保持电平。
redirect monitor 发布本拍 valid/bits，并支持同步帧的零 valid 观测。
两个 monitor 都使用既有公共 sample anchor，不各自启动计数器。
每路 raw 标记本 sample 已发布/已完成；adapter 未到齐时有界等待，
  不能因第一次service只收到一侧就报错；双方已完成却缺失或sample越界才报错。
不修改 L2TLB 专用 producer barrier 的参与者位图；新增两路完成水位单独管理。
adapter 先聚合同一 sample 的事实，再根据 frame 回执确定预期输入 sample。
要求两侧同 frame/reset/sample，字段符合冻结目标。
不以 callback 先后顺序、字段跳变或 latest 值替代身份和采样匹配。
确认后先按被冻结候选应用一次发布记账，再开放本帧 recovery_eligible。
同 sample 的 deq/cancel raw 先缓存，不能在发布记账前回收该帧需要的 owner。
缺失或错拍报告框架运输错误，不用邻拍事实补齐。
```

**中文文字伪代码：** 正常帧也可能 redirect.valid=0，因此不能只在 redirect 有效时观察。连续两帧同值时，监控应通过运输记录和目标 sample 区分，而不是靠边沿识别。当前两个通用 monitor transaction 的部分发布被注释，需要补齐轻量 raw 通道，避免全量无关字段每拍复制。

### 7.9 `apply_sampled_backend_cycle()`（计划公共入口）

**抽象功能描述：** 按确定的实际采样事实收口本帧提交记账，保持 cursor、fault token 和原恢复流程单次、有序推进。

**输入与副作用：** 输入已确认 frame 及其原候选身份；完成正常 batch 或 fault 的实际发布状态，解除对应 reservation，保留后续恢复所需记录。

**源码级伪代码：**

```text
确认该帧未应用过，reset 和冻结身份一致。
普通标量及HLV/HLVX/HSV正常batch每个UID和fault在rob_commit更新前，
  调用RM正式方案freeze_access_reference_for_rm复制已完成首后两页输入；
  control/AMO/CMO不走本新增snapshot；复制不等待RM判pass。
正常 batch 使用最终 preflight 的冻结 UID/key/代次和原 survivor 提交语义，
  冻结 batch 在COMMITTED前已验证，采样后只应用；不重新发信号、不以新candidate重选；此处不得再调用可失败的覆盖资格校验。
control 按原 mark_control_rob_commit_uid 终态语义应用一次，推进 cursor。
fault 使用已验证的准备 token 设置 rob_commit/已采样状态，保持 STA/STD 门槛；
  拆出原 mark_fault_rob_commit_uid 的状态更新，不经过其 global-freeze早退。
  只允许本帧已绑定的 freeze，其他 owner 冲突不能笼统绕过。
rollback 仅提交原先合法的 older batch（如有），不将 rollback UID标为fault终态。
资格检查位于准备阶段；不能发布后又因当前 head 已改变而重选另一个 UID。
释放准备 reservation 并记录 applied，一次调用只消费一帧。
置 sampled_applied/recovery_eligible，之后才允许 apply_redirect_flush/range scan。
现有全局冻结可能已由本帧 redirect 建立，不能因此拒绝已合法发布帧的回执。
```

**中文文字伪代码：** `mark_fault_rob_commit_uid()` 需拆为准备与发布阶段，原 Store fault 门槛和 RM 提交观察时机应分别保持明确语义，不能把准备状态误用为真实采样完成。准备中的正常候选也不能提前推进实际提交前沿。已驱动但尚未采样确认的状态独立保留，不能通过重复驱动补做记账。

### 7.10 普通 LSQ 地址记录与 fault 收尾

涉及计划 adapter 地址关联入口及既有 `sync_modeled_head_after_fault_terminal()`、`finish_fault_head_terminal()`、`clear_fault_head_token()`。

**抽象功能描述：** 为已采样的普通异常帧保存指定窗口的地址观测，并在原资源恢复完成后安全清理其身份。

**输入与副作用：** 输入 frame 的实际 S、独立地址 raw 样本、冻结 fault 身份及原 recovery 状态；输出记录完成/明确取消和最终可收尾状态，不推导 expected 地址。

**源码级伪代码：**

```text
ctrl monitor 连续发布 reset/sample_seq/VA/GPA/VS 标记的轻量 raw 样本。
仅 address_record_required 的普通 LSQ fault 在输入确认后建立 sample=S+2。
AMO/segment范围外独立来源不建立LSQ等待；当前V2标量misalign覆盖固定关闭，仍必须LSQ S+2；范围内UNKNOWN报告来源错误。
精确取该样本，一次性保存整组输出及 frame/UID/ROB key/执行代次。
sampled-applied 后原 cancel/deq 和 owner 回收继续进行，事件记录独立存活；
  不等 S+2 才设置 fault rob_commit，不推迟原 cancel 快照。
reset 或确无本次独立消费的合法覆盖明确标记取消。
原恢复完成且所需记录完成或明确取消后，才最终清理 token/上下文。
不因数值未变化、地址为零或服务迟到而改取邻拍。
```

**中文文字伪代码：** 地址 raw 无 UID，归属来自已确认的同步帧和指定采样合同。S+2 是本基线固定的普通 LSQ 采样合同；实现验收用波形验证该合同，不在coding时自由选偏移；不能按 sequence 任意等待两次 posedge 后读线。AMO、segment 等实际独立来源须使用明确来源合同，不能自动套此记录流程。原始样本历史按最早未完成目标 sample 和有界延迟保留；目标样本缺失须报记录错误，不无限等候或用 latest 替代。

## 8. 调度接入、取消与性能约束

### 8.1 原主 service 内的唯一协调 helper

新增 `memblock_backend_cycle_scheduler` 有界 helper，由现有唯一 dispatch service 持有并逐拍调用；不新增并发协调 task、sequence 或第二个状态 owner。共同未来边沿由已有公共物理 sample anchor 派生，不使用 service tick 充当物理周期。两个运输 sequence 仍由既有 vseq 在真实 sequencer 上启动。

两个 agent 的场景 sequence 仍在真实 sequencer 上运行。不能同时启用旧自主生成驱动路径与新的 frame 消费路径；安全默认/专项旧入口如需兼容，必须明确互斥启动边界，不能在同一 active flow 中争抢端口。

### 8.2 取消、复位与迟到事件

- 最终提交前，reset、代次失效或更老请求使候选无效时，取消整帧、回收两侧准备状态并配对完成两侧 UVM item；本阶段尚无 active cancel owner。
- 取消期间不推进 cursor、不重复 normal commit、不泄露新异常类别。
- frame key 与 reset 代次联合识别，旧 ready、回执和 sample 不能重启新帧。
- COMMITTED 后单侧缺失不是可正常补发的情况，应报告错误；不能转到相邻边沿声称同步。
- reset 对两侧 prepared、公共调度状态、inflight、回执及原始样本等待有统一清理边界。
- 已经被更老 redirect 覆盖的 fault 使用原覆盖与重发逻辑，不伪造它自己的异常帧或地址消费。

### 8.3 正常退出与性能

继续使用已有 terminal prefix 和全局排空。退出条件补齐未消费帧、prepared、COMMITTED、回执及地址记录均排空或明确取消。固定 `max_cycles` 不作为正常退出条件，无推进使用既有诊断与 UVM timeout。

每拍只访问当前活动帧、有界队列及 map。帧和样本窗口受实际未完成量约束，不保留主表全历史。正常后续候选无需等待上一笔 fault 的全部地址语义检查；实际发布仍受已有恢复、资源和顺序门槛约束。实现时应检查常态发送能力，避免每帧临时创建跨 agent 阻塞等待而增加不必要空拍。

基础运输合法性检查属于框架自洽；额外重复诊断按既有 `MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN` 门控，不增设一套 debug 参数。

## 9. 配置与修改清单

本方案不新增 runtime 同步偏移、权重、地址来源开关或随机 gap 配置；地址来源及是否记录属于运行期路由状态，不归参数系统。共同边沿是默认后端周期合同，物理端口和宽度直接服从 V2 compile profile。继续使用已有 LSQ commit、redirect 开关；本同步 topology 缺少任一必要 driver/sequence 时在启动检查中明确报错。

下列路径以 `mem_ut/ver/ut/memblock` 为基准。

| 修改对象 | 计划修改 |
|---|---|
| `seq/base_seq_help/memblock_dispatch_types.sv` | 周期 frame、投影类型和候选/提交/采样状态 |
| `seq/base_seq_help/memblock_backend_cycle_scheduler.sv`（计划新增） | 一致快照构造、双侧准备、共同边沿及取消协调 |
| 既有 dispatch service | 持有 scheduler helper，在原主 service 内推进，不新增协调 task/sequence |
| `seq/base_seq_help/common_data_transaction.sv` | 帧/候选 reservation、事件上下文、回执、最终激活原 cancel owner、sampled-applied恢复门槛和排空 |
| `seq/base_seq_help/lsq_commit_handler.sv` | 拆分准备、发布记账和恢复，保留 normal/control/fault 三分支和 STA/STD资格 |
| `seq/base_seq_help/exception_redirect_replay_handler.sv` | 保留年龄仲裁，消除 fault prepare 与发布之间循环等待，输出获选帧原因 |
| 两个现有公共 dispatch sequence | 从独立生成转为运输同一 frame 的各自投影 |
| 两个 agent 的 xaction/driver/monitor | 增加非随机化运输元数据、逐拍 prepared 处理、同边沿发布及实际输入观测 |
| `common/memblock_common/src/memblock_sync_pkg.sv` | 基础运输状态、ready、边沿约定、raw 回执/样本及 reset 清理 |
| 独立 flushSb 公共服务 | 从阻塞的 LSQ item 路径解耦，保留原请求 owner 和 empty 握手 |
| ctrl monitor、`dispatch_monitor_event_adapter.sv` | 同 sample 输入帧关联、地址 raw 记录和生命周期衔接 |
| `seq_pkg.sv`、`seq.f`、必要公共包/filelist | 新增 helper/task 的依赖与编译接入，避免 agent 引用 seq_pkg |
| 现有 vseq 公共并发 hook | 保留两个运输sequence与原唯一主service；检查旧自主模式互斥，不启动新协调任务 |

`isStoreException` 的 interface/connect 归属不迁移，也不新增第二个 force。仅在检查共同 clocking 时确认现有两个 interface 的时钟及输出偏移一致。若实际需要新增公共 interface 组件，再按 agent 完整结构规则另行展开，不随意散落组件。

本方案涉及编译接入或拓扑变更时，应同步检查 env handle、sequencer 连接和旧模式互斥，不在 `virtual_sequencer` 中保存场景状态或直接驱动。

## 10. 框架验收与后续实施要求

### 10.1 验收矩阵

本表是后续验证目标，不表示本轮新增或执行了测试。

| 验证点 | 生成与运输应满足的结果 |
|---|---|
| 普通正常提交 | 两侧表达同一帧，scommit 只作用一次，pending/head 不混用新 cursor |
| Store/MMIO pending 等待 | pending 与指针对应同一 head，不把 pending 误记为正常提交 |
| Load/Store fault 交替及连续同类 fault | 异常帧类别正确；零正常提交控制与异常 redirect 在同一输入 sample |
| 一侧 sequence/driver 晚准备 | 最终提交前整帧等待；无类别提前更新、半帧或提交 pulse 泄露 |
| 仲裁变化与旧帧取消 | 取消两侧 prepared 状态，不能旧投影配新 redirect |
| reset 发生于准备/提交边界 | 旧回执和旧样本不跨代配对，退出无遗留等待 |
| 访存违例 R/R+1 | 主帧/尾拍预装且连续发布，R保留合法前级older batch及原level，R+1零pending/提交；不建立fault token/地址等待 |
| fault与rollback竞争 | 原年龄仲裁单一获胜；被覆盖fault不再自发redirect，不能误当fault终态 |
| 非head rollback及两种level | 保留flush-self/flush-after的原范围，按原规则重发，不等待fault head |
| prepared reservation窗口 | intent已出队仍阻止年轻提交越界，允许必要WB和更老请求到达并重新仲裁 |
| Store缺侧dispatch/WB | 资格不足时不冻结缺侧服务，不提前释放SQ owner或推进fault终态 |
| Uncache NACK帧取消重建 | 只对最终获选有效实例应用原retry boost，不因取消重复增加 |
| 单侧monitor先到 | 等两侧sample完成水位后判定，无callback顺序误报 |
| control head/Fence | 保留control采样后提交及cursor推进；flushSb服务不依赖帧ready |
| fault采样与恢复同窗口 | 先写fault rob_commit/token，再开放range scan；保持cancel原采样锚点 |
| 标量非对齐与独立来源 | 当前V2标量misalign必须LSQ S+2；AMO/segment范围外来源保留原恢复并明确不新增记录 |
| 普通redirect与正常提交交错 | 按明确来源时序生成，不用任意redirect统一清零模板 |
| flushSb 单独发生或与帧相邻 | 独立服务持续推进，不自动触发、不重复发送、不因双边等待自锁 |
| 两帧值完全相同 | 按 frame 和 sample 区分，不依赖信号跳变 |
| 地址样本迟到消费 | 读取指定历史 sample，原 owner 回收后仍保留归属 |
| 全局停止 | 两侧运输、frame、回执、恢复和需保存的记录一起排空 |

### 10.2 实施前与实施后验证

1. 正式化时复核 normal/control/pending、同步 fault、rollback R/R+1来源映射和共同采样服务顺序；独立评审记录见下节，本正式方案仍需本轮独立复审收口。
2. 获得明确 coding 授权后，按项目 plan 执行规则修改，先检查现有工作区并保护无关修改。
3. 静态检查端口唯一写入者、同步 metadata、字段 copy/compare/print、reset/gap/idle 分支和循环等待。
4. 在远端真实编译/仿真流程中，用实际波形确认 T0/T1、driver 输出偏移、同一输入 S 及普通地址 S+2。
5. 补必要的框架验证和已有正常/异常回归，不把邻拍或最终同值当作同步成功。
6. 后续新增真实 DUT 或 soft/framework 独立验证场景时，按规则同步分类 CSV；本轮文档替换不提前登记未实现用例。
7. 本轮未执行编译、仿真、波形验证或源码修改。

### 10.3 关联文档

原虚拟化支持方案第 10.2 节关于 fault head 地址记录与恢复的表述，在两个方案共同实施时必须同步为“统一后端帧、同拍输入、实际 S 和指定地址样本”合同。该方案继续负责指令、CSR 和 fence 扩展，本文件负责本次控制输入边界，不混入另一套指令生成逻辑。

实际 coding 时同步所涉 flow、源码 review 和公共驱动使用说明。当前仅替换本方案文档，不更新 V2 RTL 知识库，不移动 plan 到 do。

### 10.4 当前两类 redirect 的接入依据与评审边界

下列源码路径以 `mem_ut/ver/ut/memblock` 为基准；结论来自静态源码分析，尚未通过编译和波形验证。

| 路径 | 当前处理链与依据 | 接入本方案的必要改变 |
|---|---|---|
| 访存违例 | `dispatch_monitor_event_adapter.sv:1594` 转换真实 memoryViolation并冻结UID/代次/level；`exception_redirect_replay_handler.sv:265` 仲裁；`common_data_transaction.sv:5966` 建立owner | 保留原事件及重发语义；增加rollback来源级数；准备期只预约，最终提交才激活owner |
| Load AF/PF | `writeback_status_handler.sv:73` 处理真实异常；`lsq_commit_handler.sv:490` 选择fault head；现有LSQ sequence在 `:163` 先记fault后申请redirect | 将资格、驱动和记账拆开；fault帧同步发布；采样后记账先于恢复扫描 |
| Store AF/PF | 上述fault链并受 `store_fault_real_wb_pending()` 与 `store_redirect_waits_fault_commit()` 门控 | 保留STA/STD真实WB，等待条件改为prepared资格，不能等待本帧发布完成再准许仲裁 |
| AMO AF/PF | `lsq_commit_handler.sv:590` 使用load-like类别；`common_data_transaction.sv:6207` 保留已提交fault head，原子恢复另有门槛 | 复用控制发布，保留atomic barrier与原恢复；本轮不声称捕获独立atomic地址 |
| 控制队首 | 现有LSQ sequence `:135、165` 有control候选和提交；`lsq_commit_handler.sv:887` 推进控制指令终态 | 新帧路径保留该分支，防止Fence/CSR等永久停在commit cursor |

rollback的强制尾拍属于同一预装发布单元，保证真实下一物理输入周期的阻塞；不能只把它排为一个以后再准备的普通帧。

必须保留的既有恢复顺序是：真实驱动回执 → 实际输入anchor S → 本帧采样记账 → 原cancel/deq条件与range scan → 原恢复完成。地址记录是独立分支；仅最终清理同时等待其完成，不改变原cancel快照时刻。已有 `terminal_fault_head` 判定要求 `status.rob_commit`，所以交换记账和range scan会导致AF/PF被误判为普通重发，AMO还可能触发不可重复发射保护。

## 11. 固定接入点、物理周期及提交不可撤销合同

### 11.1 唯一 service 调用顺序

**抽象功能描述：** `memblock_dispatch_base_sequence::collect_monitor_event_batch()` 聚合同一采样周期事实，先建立本帧业务状态，再允许其依赖的资源回收；新增 helper 由它及现有主 service 使用，不运行第二条循环。

**目的与输入输出：** 高频路径输入 raw monitor batch、frame 回执及原 status；输出真实 WB 更新、采样应用结果和原 deferred ctrl 消费；访问当前批次及有界活动帧。

**源码级伪代码：**

```text
原collect_runtime_context_events→service_l2tlb_sfence_events→reset门禁保持
原collect_monitor_event_batch内部:
  collect_writeback_events_batch: 收集真实WB/feedback
  collect_ctrl_redirect_events_batch: 收集真实违例；暂存deq/cancel
  process_monitor_event_batch: 保留同sample redirect-first仲裁，更新真实status
  monitor_adapter.service_backend_cycle_samples: 配对双输入、连续地址raw及完成水位
  scheduler.apply_sampled_backend_cycle: 冻结RM输入，单次应用已采样业务帧
  apply_deferred_ctrl_updates_batch: 此后才允许旧owner的deq/cancel回收
  原STD tombstone维护
原主service继续finalize_pending_store_faults及exception_redirect_replay_task
scheduler.service_frame_publish: 读本物理sample的候选、原仲裁，推进准备/确认
原控制barrier、WB与恢复服务继续；不在helper内部等待driver或future sample
```

**中文详细伪代码：** 原 monitor batch 先完成同拍真实 WB 和违例的归属，保证不会把 raw 到达先后当作后端顺序。接着将指定 sample 的两组实际输入与已驱动 frame 配对，应用其冻结提交列表或 fault 状态。最后才处理可能删除 owner 的 deferred deq/cancel。正常 fault 在 applied 前复制 RM 所需值型上下文，恢复可随后推进，不等待 RM 检查通过。主 service 后面的原异常仲裁仍唯一；scheduler 使用其获选 intent，不另起年龄排序。任何等待均保存状态后返回，不能阻塞本 service。若同 sample 新违例影响未来提交，只取消尚未 COMMITTED 的候选，不能否定已采样 frame。

### 11.2 物理周期状态与 rollback 前级槽

新增 `memblock_rob_decision_slot_t` 在 scheduler 内保存：`valid/source_sample/reset_epoch/reservation_id/head_rob_key/commit_uids/control_uid/lsq_projection/consumed`。只是原 ROB 决策的一个有界槽，不承载 LSQ admission/source-offer 协议。frame 完整业务结构在 `memblock_dispatch_types.sv`；agent 只使用 `memblock_sync_pkg` 的基础运输结构。

**抽象功能描述：** `memblock_backend_cycle_scheduler::advance_rob_source_slot(sample)` 把当前物理周期决策推进到下一周期的 LSQ 寄存输出来源，确保 rollback 使用相邻真实周期的结果。

**源码级伪代码：**

```text
同一sample已服务则返回；sample比前次跳过时，对遗漏周期推进零提交/保持head决策
最终COMMITTED前候选不绑定source_sample；固定E后令S=E+1
发布单元的LSQ来源绑定S-1，rollback来源绑定S；prev_slot仅消费此绑定的一次性候选
该槽已运输/已采样消费则不能再次作为提交来源；使用零提交的同源保持投影
原commit资格不能满足、运输未准备或前沿被reservation占用：本拍生成零提交决定
合法正常候选在最终确认前通过preflight，随目标E绑定为S-1寄存候选
rollback R使用prev_slot未消费且不被flush覆盖的older列表；无合法列表则零提交
保存R的阻塞决定及冻结后的next_head，形成R+1强制尾拍
```

**中文详细伪代码：** 周期由公共物理采样编号推进，idle 和运输空拍也计时。上一笔非零 frame 可能在数拍前发生，不能当作 R−1。一个非零列表一旦绑定实际输出，只能消费一次；后来相同 head 保持电平时 scommit 为零。未获得足够运输提前量的普通候选不被追认成先前已经决定的提交，本拍按零提交处理，因此允许框架保守少提交但不伪造后端时序。R+1 的 head 来自冻结 older 列表之后 active ROB 映射中的合法前沿，不读尚未应用 cursor，不按 UID 算术生成 key。

准备阶段只保存有资格的候选与reservation，不把service C记成已发生的后端来源拍。COMMITTED时固定未来E，再绑定实际消费S=E+1、LSQ来源S-1和rollback来源S；身份/资格无效才取消，不因运输等待使来源拍自发过期。绑定后的非零列表只消费一次，idle周期不会重发。物理C与E/S使用同一公共编号轴的周期位置，不是新计数器。

固定例子：主service在C=10的negedge完成确认，两driver在E=11的posedge后输出，输入在S/R=12的posedge消费；`lsq_source_seq=11`、`redirect_source_seq=12`，预装尾拍在E=12输出并在S=13消费，其零提交决定来源为12。准备可能早于C=10，但在最终确认之前不声明它已在11或12产生。同步fault使用同源异常决定的两侧寄存结果，不套用rollback直连来源偏移。

### 11.3 提交资格前移与不可撤销应用

**抽象功能描述：** `lsq_commit_handler::check_backend_cycle_commit_eligibility(frame)` 在最终确认之前检查冻结 batch；`apply_sampled_backend_cycle(frame)` 在实际采样后执行已确认状态更新。前者不更新业务状态，后者不重新选择指令。

**源码级伪代码：**

```text
check_backend_cycle_commit_eligibility:
  核对reset、UID/ROB/dynamic、原normal/control/fault资格和当前获选intent
  对正常older列表逐项调用原rob_need_flush，任何被覆盖项返回false
  Store fault保留STA/STD真实WB与缺侧dispatch例外
  成功只返回true；失败取消整帧准备，不发scommit
最终确认:
  两侧ready且合法→一次request_redirect_flush/accepted（仅需redirect时）
  冻结frame和预装尾拍、目标未来边沿、commit_approved=true
apply_sampled_backend_cycle:
  核对采样身份；若applied则返回幂等结果
  使用commit_approved冻结列表执行原mark_rob_commit_batch的纯更新部分
  control执行原控制终态更新；fault执行原fault更新、保存fault身份
  不调用旧函数中可返回失败的资格/覆盖检查；不可取消已发pulse或重选UID
  更新cursor、applied、recovery_eligible；原range scan此后才可运行
```

**中文详细伪代码：** 当前 `mark_rob_commit_batch()` 中“检查资格”和“写入状态”须拆开，检查阶段所有失败都发生在驱动前。实际采样后仅消费已经确认的列表，不能因为本帧刚激活的 redirect freeze 而早退，也不能因更年轻的新请求取消已送入 DUT 的提交。若实际输入与冻结 frame 不一致，报告运输错误并停止，不伪造软件回滚来掩盖硬件收到的 scommit。只有 reset 使整个边界失效时按原 reset 合同取消，不产生正常采样成功。

### 11.4 精确边沿与双 driver item 生命周期

当前两个 interface 的 `mon_cb` 均在有效上升沿前采样，driver 于该上升沿的 `drv_cb` 输出。令 E 为驱动边沿，S 为下一上升沿前首次看到新输入的公共 sample；DUT 在 S 的上升沿消费它。LSQ 类别选择寄存与 MemBlock 地址输出寄存之后，普通地址为 S+2 的边沿前样本。本合同固定，不新增可调拍偏移。

```mermaid
sequenceDiagram
  participant O as 唯一dispatch service
  participant L as LSQ driver
  participant R as redirect driver
  participant D as DUT与monitor
  O->>L: PREPARING 主帧与可选尾拍
  O->>R: 同frame投影
  L-->>O: ready 保存成功
  R-->>O: ready 保存成功
  O->>O: 最终资格检查并一次激活原owner
  O->>L: COMMITTED 未来边沿E
  O->>R: 相同未来边沿E
  L->>D: E 输出LSQ控制
  R->>D: E 输出redirect
  D-->>O: S 同拍输入样本
  O->>O: 冻结参考记录与applied，开放原恢复
  D-->>O: S+2 普通LSQ整组地址
```

图的中文文字伪代码：两个 driver 先持有数据并确认，唯一 service 才确定未来 E。双方在 E 输出，S 是实际消费锚点；S 的记账先于恢复。地址 raw 始终连续采样，S+2 到达后从历史按编号关联，不能临时抓 latest。

**抽象功能描述：** 两个现有 driver 的 `main_phase()/send_pkt()/drive_active_idle()` 在每个物理边沿合成一次输出，并按准备状态消费 UVM item，不在内部阻塞等另一侧。

**源码级伪代码：**

```text
逐物理周期try_next_item；单个prepared槽空才接收
同步item保存主/尾投影，发布ready，不item_done
每拍优先级: reset > COMMITTED目标主帧 > 紧邻强制尾拍 > 原active idle
  每个端口仅一次赋值；LSQ最后合成独立flushSb值
主帧驱动即发布回执；尾拍下一边沿不等新的ready
完成整个item或准备被取消后恰好一次item_done
同步frame忽略pre/post随机gap；延迟只在COMMITTED前发生
无fault/reset时isStoreException保持最近已发布类别，不随普通候选改变
```

**中文详细伪代码：** 一侧未就绪时两边都保持原电平并清提交/redirect脉冲；原 flushSb 请求仍由 LSQ driver 每拍合成。已提交后不能换边沿，rollback 两段 item 的主回执不能等尾拍 item_done 才发布。reset 撤销两段并清旧代次；最终确认前取消也配对结束 item。启动时检查两个 driver、sequencer、monitor 和相同 clocking 均存在，旧自主生成模式与 frame 模式互斥；缺能力立即报告配置错误，不能等 ready 死锁。

### 11.5 范围内地址的完整记录与收尾

本次全功能范围是普通标量/Guest 标量及 HLV/HLVX/HSV 的普通异常地址链。检查当前生成 RTL 中两个 MisalignBuffer 的覆盖 valid 为零后，标量非对齐同样必须设置 `address_source=LSQ/address_record_required=1`；不能因 misalign 标签设为未支持。AMO/segment 的现有执行与恢复继续，本次不新增其独立异常地址检查。

字段固定：`memblock_fault_address_event_t` 保存 `frame_id/reset_epoch/uid/rob_key/dynamic_epoch/issue_epoch/replay_seq/exception_vec/issue_csr/input_sample/target_sample/address_source/record_status/vaddr/gpaddr/isForVSnonLeafPTE`。引用原实际 issue 上下文；类型若底层包无法依赖复杂上下文，只存基础运输身份，完整值存在 common data。`record_status` 为 `WAITING/CAPTURED/CANCELLED_RESET/CANCELLED_COVERED/ERROR_MISSING_SAMPLE/NOT_CAPTURED_BY_THIS_PLAN`。

**抽象功能描述：** `dispatch_monitor_event_adapter::service_fault_address_samples()` 将连续 raw 历史与已确认 fault 帧配对；`finish_fault_head_terminal()` 结合原恢复完成释放 fault token。

**源码级伪代码：**

```text
每sample记录ctrl地址raw和producer完成水位；无fault时也连续记录
fault frame确认S: 冻结身份，插入target=S+2等待槽
水位未到target: 保持等待，服务返回；水位已到且raw缺失: ERROR并global-stop
命中target且reset匹配: 整组复制地址，标记CAPTURED
合法reset/原覆盖证明未独立消费: 标记相应CANCELLED
原恢复完成且required记录已CAPTURED或合法取消: 清token并推进终态一次
独立RM快照与actual值型事件保留到RM消费；不依赖已释放resource map
```

**中文详细伪代码：** 持续 raw 环形历史至少保留未完成 target 和从驱动到 service 的有界运输窗口；有未配对 frame 时不得提前删除其可能目标，消费者水位阻止静默覆盖。队列容量受 active ROB/fault 数量和现有服务延迟上界约束，容量不足报告错误，不能丢最老事件继续通过。S+2 记录完成只说明采样完成，expected 地址由 RM 独立计算。必需样本缺失或范围内来源 UNKNOWN 均失败，不作为 CSR 放行或全局成功退出依据。


## 12. RM 协同支持

**本 plan 不需要实现 RM/checker/scoreboard。** 对外可提供 frame 身份、决策原因、正常提交 UID 列表、fault UID/ROB key/动态代次、reset 代次、实际输入 S、独立 flushSb 回执和异常地址记录。

框架确认输入字段与生成快照一致，属于激励运输自洽；VA/GPA/VS 标记是否正确，需要后续组件的独立参考结果。不得从 DUT 输出回填 expected，或把“地址已记录”当作 RM pass。现有 RM 对提交事件的接入点需随准备/发布拆分核对，但本方案不展开 RM 算法。

## 13. 功能覆盖率协同支持

**本 plan 不需要实现 coveragent/covergroup。** 后续可使用正常提交、pending、fault、redirect 来源、类别切换、双边准备等待、取消/reset、同值连续帧、flushSb 独立发生及地址来源等标签，不实现 coverage bin，也不以覆盖率命中替代本帧运输完成。

## 与初步 plan 差异说明

本章只供评审。原初稿保留以核对合理功能、参数和恢复规则。

| 修改目的 | 修改前中文伪代码 | 修改后中文伪代码 | 影响 |
|---|---|---|---|
| 避免新增并发owner | 新协调task与主service分别推进 | 原唯一service调用有界scheduler helper | 少一个sequence/task及启动状态 |
| 真实相邻周期 | 取上一运输frame作R−1 | 每物理sample推进前级槽，空拍也推进，已消费batch不能再用 | 保留R合法older commit和强制R+1阻塞 |
| 不撤销已发提交 | 发完再执行可失败mark_rob_commit_batch | COMMITTED前只读检查，采样后只应用冻结批准列表 | 不再有硬件收到提交而软件拒绝记账 |
| 状态先于恢复 | callback到达就可能清owner | 同拍WB batch→frame applied→deferred ctrl→原恢复 | AF/PF及HSV缺侧门槛完整 |
| 范围内地址不降级 | misalign/UNKNOWN都可未支持 | 当前V2标量misalign必需S+2；UNKNOWN报错 | 保持标量全功能要求 |

`advance_rob_source_slot()` 是新增纯协调 helper，抽象职责是维护相邻物理周期决策来源，添加原因是运输frame间可能有空拍。详细中文伪代码：先按公共sample去重；补推进遗漏周期的零提交决定；仅保留相邻前级；被绑定过实际输出的列表不再复用；形成当前阻塞/正常决定并记录来源sample；返回供下一个周期组成LSQ投影，不驱动端口。

`check_backend_cycle_commit_eligibility()` 的抽象职责是最终发布前验证冻结资格，添加原因是旧mark函数允许发布后失败。详细中文伪代码：核对实例与reset，检查Store双真实WB，逐项按获选redirect范围检查older列表；任一失败返回false、不改状态；成功返回true，由scheduler固定approved，之后采样只应用。

修改 `collect_monitor_event_batch()` 前：WB/违例semantic batch处理后立即应用deferred ctrl。修改后：在两者之间配对已采样frame、冻结RM输入并应用其业务记账，再放行deferred ctrl；抽象职责仍是同拍事实有序消费。

修改driver前：收到item按gap发送并item_done，独立路径可提前改变类别。修改后：先保留投影和ready，每拍合成唯一输出，等共同未来边沿发布；有尾拍连续发，结束后一次item_done。保留原两个agent和端口归属，不把source-offer/admission改造成提交通道。

`service_fault_address_samples()` 的抽象职责是精确采样关联，添加原因是raw地址无UID/valid。详细中文伪代码：连续入队带reset/sample的raw；对已采样fault保存target=S+2；目标未到返回等待，目标到达后一次复制整组，目标水位越过仍缺失则报错；合法取消独立记录；实际值供后续RM读取，不计算期望。原故障收尾改为等待此记录与恢复双收口，RM结果不阻塞恢复。


## 执行中补充/修正（IMPLEMENTATION_DELTA）

### 1. 有界来源槽与准备候选分开保存

[IMPLEMENTATION_DELTA]

来源：coding 中对照现有唯一 service 与双边运输等待，减少重复业务状态。

原 plan：来源槽同时列出 head、提交 UID 列表、control UID 与基础投影；完整 frame 名义上置于 dispatch types。

实现调整：完整 `memblock_backend_cycle_frame_t` 置于 `common_data_transaction.sv` 前，复用既有 CSR class 可见顺序。来源槽仅保存有效位、消费位、来源 sample、reset、reservation 和投影；未绑定 older 列表保存在 `reserved_older_decision` 引用的 frame。准备等待不声明历史决定已发生；双 ready 且最终资格成立后固定未来 E/S，再绑定 LSQ=S−1、rollback redirect=S 的来源位置并恰好消费一次。原已发布提交不会再次进入 older 候选。

原因：agent 基础包只需要固定宽度运输对象，不能依赖复杂 CSR/主表类型；同一在途 frame 已保存完整业务身份，来源槽再存一份会重复维护。允许空拍和运输等待导致的保守少提交，不模拟整个 ROB 内部流水。

影响范围：scheduler、common data、review 及 flow。生产 helper 检查 older 前缀、主尾和幂等应用；真实 driver 专项检查未来 E、实际 S、紧邻尾拍及准备取消。来源编号本身不构成验证证据。

### 2. 异常地址记录复用不可变 frame

[IMPLEMENTATION_DELTA]

原 plan：新增独立 `memblock_fault_address_event_t` 保存完整故障身份及地址。

实现调整：`backend_fault_address_by_uid` 保存已批准 frame；frame 内具备原计划全部身份、输入 sample、target sample、来源、状态、CSR 副本和整组 raw 地址。与运输中 `backend_frame` 指针分离，清运输不删除已保存记录，reset 明确取消等待。

原因：完整身份和冻结 CSR 已在 frame，不额外复制同义事件；后续 RM 通过该独立保存点读取 actual，expected 仍须独立冻结和计算。

影响范围：common data 的地址等待及 fault terminal 收口。当前普通标量包含非对齐路径必须采 S+2；AMO 标为独立来源，本项不新增其地址检查。主表与公共行为入口当前拒绝所有 vector/segment，故 `ADDRESS_SEGMENT` 为不可达预留，不新增向量功能。

### 3. normal 最终执行身份冻结

[IMPLEMENTATION_DELTA]

原 plan：规定核对 UID/ROB/动态及执行代次，未展开正常批次的逐目标字段布局。

实现调整：正常批次同步冻结 load/STA/STD issue epoch 和 replay sequence，最终批准及 rollback older 前缀重建逐项核对。fault 保留独立 issue/replay/exception 核对。

原因：准备等待期间若 replay 完整重试且再次正常完成，正常提交本身可以安全选择当前结果，但严格冻结执行身份可避免旧准备对象承接新 attempt；失配只在输出前取消并重建，不撤销已经输出的 scommit。

影响范围：frame、scheduler、LSQ handler、helper 自测；未引入历史 attempt 账本或新调度 owner。


## 本项实施验证记录（2026-11-17）

源码基线在独立 worktree `codex/memblock-virtualization-20261117`。`compile_backend12.log` 完整编译成功，mode=`backend_transport_20261117`，KDB 0 errors。

- 生产 helper：seed6111742，rollback older 前缀/尾拍、幂等 apply、normal/fault 旧执行身份拒绝、精确 S+2 零VA及非叶标记关联、reset取消通过。
- 真实双 driver：seed6111740，81项断言通过，覆盖共同未来E、S=E+1、紧邻尾拍、prepared取消、两帧同值、结束idle；不生成非零scommit，不冒充真实违例根因测试。
- 真实正常双 Store：seed6111743，两条 exception=0，RM compare PASS。
- 真实 Store PF：seed6111741，真实STA/STD WB，exception=0x8000，RM compare PASS。
- 真实 flushSb：seed6111744；基础 virtual_base_sequence：seed6111745。均 TEST_PASS，UVM_ERROR/FATAL=0。

波形依据：Store PF 的 `U_MEMBLOCK.io_ooo_to_mem_isStoreException` 与 `U_MEMBLOCK.io_redirect_valid` 同在1030.850ns驱动边沿E变为1，redirect在1035.600ns下个驱动边沿清零。DUT输入采样S为1035.600ns边沿前；地址输出在1040.350ns边沿后成为0x8e711a80，因此1045.100ns边沿前S+2连续raw可见。类别保持为1。注意top_tb同名reg在force连接下不是DUT实际值，波形必须看U_MEMBLOCK真实端口；本轮已按真实端口核对。

证据边界：没有宣称穷举全部交错。真实违例产生且带非零older Store提交、reset发生于主尾中间、flushSb逐个等待窗口及标量misalign端到端均需后续回归继续扩展；当前用生产helper与真实运输分层验证共同合同。全basicTest回归按用户要求在三项完成后统一执行。
