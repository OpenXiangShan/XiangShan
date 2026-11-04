# MemBlock AMO 纯激励最小适配方案

## 1. 计划定位

### 1.1 计划信息

- 创建日期：2026-10-22
- 当前版本：v2.0
- 最后更新：2026-11-04
- 最新复核基线：分支`mem_ut_uvm_v2`，HEAD `4acf3cc3c381671cb7bb4dc34eeaec9287825c80`
- Review 状态：九轮方案 review 和一轮独立 implementation review 已完成；implementation review 结论为 Blocker、Major、Minor 均无，残余项均为扩展测试覆盖缺口
- Coding 状态：`已完成 coding、基础动态验证和归档`。2026-11-04 在独立 worktree `codex/memblock-amo-adaptation-20261103` 完成 P1-P3；完整 VCS/KDB 编译、LSQ/AMO 定向 soft test、普通 real-dispatch smoke 和 AMO real-dispatch smoke 均通过，`UVM_ERROR=0`、`UVM_FATAL=0`。P4 中未勾选的 one-hot、多 seed、异常和交叉场景仍作为后续专项回归，不宣称已完成全部覆盖矩阵。
- 适用范围：`mem_ut/ver/ut/memblock` real-dispatch 测试框架
- 目标：不增加抽象模型、RM、scoreboard 和结果 checker，使框架能够生成、调度、发送并收敛当前 RTL 支持的全部标量原子指令。
- 设计原则：功能完整是前提；在此基础上使用单 atomic owner、固定物理 pipe 0、每拍最多一片子 uop、基于 remaining mask 的参数化动态选择和既有 responder，避免通用多事务乱序状态机。
- 状态约定：`[ ]` 表示待完成，`[x]` 表示已完成。

## 实现补充/修正（IMPLEMENTATION_DELTA）

2026-11-03 coding 阶段按 RTL 重新固化隔离语义：单 atomic owner 不是测试框架为减少改动而采用的吞吐简化，而是当前 MemBlock 单一 `AtomicsUnit` FSM 与后端 `blockBackward` 的强制行为。AMO/LR/SC 等待全部 older ROB 事务完成；同一架构原子指令的全部 STA/STD 子 uop 进入后端和 MemBlock，且都归属于同一个 owner；barrier 阻止 younger 普通访存、control 和下一条独立 AMO。AtomicsUnit busy 期间额外 STA 只允许是同一 `AMOCAS.Q` 的 STA slot2。owner terminal/commit、反馈和必要 recovery 收口前不得释放 barrier。

固定物理 pipe0、每拍最多发送一片仍是驱动实现选择，但不能改变上述严格隔离。文中所有“单 owner 仅为最小吞吐路径”“未复制整核最大吞吐”的表述均以本节为准：允许简化的是端口选择和单拍发送宽度，不允许简化或参数化关闭 atomic 隔离；CAS.W/D/Q 的多个子 uop 始终属于同一 owner。

2026-11-04 实现收口补充：source offer 改为只在真实 driver launch 后发布；reset 会撤销尚未 DUT-visible 的 LSQ reservation 和 pre-fire atomic token；AMO smoke 固定用户态 PTE.U 和支持 atomic 的主存物理范围，避免把环境权限/PMA fault 误判为 AMO 生命周期错误。独立 implementation review 未发现实现问题，残余缺口见 P4 和最终 review 文档。

### 1.2 “纯激励功能完整”的边界

本方案中的功能完整表示：

1. 可以生成当前 `AtomicsUnit` 支持的全部 LR、SC、AMO 和 AMOCAS opcode。
2. 可以按 RTL 要求发送每条指令的全部 STA/STD 子 uop。
3. 可以观察并聚合全部 atomic final writeback 或 fault，使 normal/fault commit、terminal 和 barrier 正常收敛。
4. 可以复用现有 TLB、DCache、MissQueue、SBuffer、Uncache 和 redirect worker，不因测试框架缺少生命周期状态而卡死。
5. 不计算返回值、SC 成败、reservation、CAS 比较结果或最终内存内容；这些属于后续 RM/checker。
6. 可以按参数随机生成 W/D/Q 对齐或非对齐地址，并让架构异常复用普通访存 fault/redirect/terminal 流程。
7. 可以保留独立 SC opcode 随机生成，同时让随机到的 LR 按参数在未来 UID 预约一个同宽度伴随 SC。

因此，“不含 checker”不等于只发出接口 valid。本方案仍必须维护足够的 owner、子 uop、writeback 和 terminal 状态，才能完整发送下一条激励。

### 1.3 术语

| 名词 | 通俗含义 | 代码落点 |
| --- | --- | --- |
| admission | 一条主表事务成为 active ROB owner，并准备进入真实 IQ。 | `memblock_lsqenq_dispatch_base_sequence`、`lsq_ctrl_model` |
| atomic non-LSQ admission | 建立 ROB/status owner，但不分配 LQ/SQ entry；索引取shadow response snapshot并预置`lsq_deq=1`。 | 计划新增`commit_atomic_non_lsq_admission()` |
| registered canAccept | `LsqEnqCtrl` 根据旧 shadow credit、当前接受量和 redirect 流水计算后寄存的全局 Dispatch 许可；AMO 虽不分配 LSQ，仍受该许可门控。 | `memblock_lsq_rtl_equivalent_admission_formal_plan_20261031.md` |
| atomic owner | 当前唯一允许进入 MemBlock 的动态原子指令。 | 待加入 `common_data_transaction` |
| atomic barrier | owner 建立后，阻止年轻事务 admission 和非 owner issue 的软件后端规则。 | admission + IQ scheduler |
| atomic slot | `fuOpType[8:6]` 保存的原子子 uop 编号。它不是物理 issue pipe。 | `AtomicsUnit.scala` |
| issue pipe | 本拍实际驱动的 STA/STD 物理端口编号。 | 本方案仍固定为 pipe 0 |
| issue order | 同一 atomic owner 的 STA/STD 子 uop 实际 fire 次序。 | target 由 `MEMBLOCK_AMO_ISSUE_ORDER_MODE` 控制，STD slot 由 `MEMBLOCK_AMO_STD_SHUFFLE_EN` 控制 |
| STD next-slot selection | 每次准备发送一片 STD 时，从尚未 fire 的 STD slot 中选择下一片。 | 由 `MEMBLOCK_AMO_STD_SHUFFLE_EN` 控制 |
| paired SC reservation | LR 生成完成后，在未来尚未生成的主表 UID 上预约一条同宽度 SC；预约只替换该 UID 原本的普通随机生成，不扩表、不移动 UID。 | `build_random_main_table()`、`build_control_auto_main_table()` |
| atomic final WB | `AtomicsUnit.out` 经 `writebackLda_0` 返回的最终结果或异常事件。 | `AtomicWBPort=0` |
| normal failure | 指令执行成功结束，但操作结果表示“没有写成功”，例如 SC 返回失败或 CAS 比较失败；它不是架构异常。 | final WB 的 `exceptionVec==0` |
| UID+1 重发 | 本方案针对 AMO 架构异常的无操作系统恢复策略：异常 UID 完成 fault terminal 后，将 commit/admission 游标推进到 `fault_uid+1`，从下一条主表指令继续发射。它不是把异常 AMO 改成新 UID 再执行。 | `finish_fault_head_terminal()` |
| dynamic epoch | 普通 redirect 覆盖某条仍需重新执行的年轻事务时，同一静态 UID 的动态实例代号加一。它是通用 redirect 恢复机制，不是本方案 AMO fault owner 的恢复方式。 | `prepare_uid_for_redirect_reissue()` |
| terminal | writeback/fault、ROB commit 和 active owner 回收全部完成。 | `terminal_done`、`retire_active_uid()` |

## 2. 支持范围与非目标

### 2.1 支持的完整原子集合

```text
LR.W / LR.D
SC.W / SC.D
AMOSWAP.W / AMOSWAP.D
AMOADD.W / AMOADD.D
AMOXOR.W / AMOXOR.D
AMOAND.W / AMOAND.D
AMOOR.W / AMOOR.D
AMOMIN.W / AMOMIN.D
AMOMAX.W / AMOMAX.D
AMOMINU.W / AMOMINU.D
AMOMAXU.W / AMOMAXU.D
AMOCAS.W / AMOCAS.D / AMOCAS.Q
```

子 uop 和 final WB 数量：

| 指令类型 | STA slot | STD slot | final WB 数 |
| --- | --- | --- | ---: |
| LR/SC/非 CAS AMO | `0` | `0` | 1 |
| AMOCAS.W/D | `0` | `0,1` | 1 |
| AMOCAS.Q | `0,2` | `0,1,2,3` | 2 |

### 2.2 明确不做

- 不支持向量指令。
- 不新增 AMO ALU、LR reservation、SC 成功率或内存语义模型。
- 不检查 old value、new value、SC 返回码、CAS 成功与否和最终内存内容。
- 不支持把非对齐 AMO 拆成多个对齐访问；当前 RTL 对非对齐原子直接返回 misaligned exception。
- 不新增 RM、scoreboard、checker 或 coverage。
- 不把 `stIsEmpty`、SBuffer/Uncache 空、TLB 完成、DCache ready、miss/refill 或 reservation 作为 driver issue 前置条件。
- 不增加 `aq/rl` 控制。当前 MemBlock issue 接口没有独立 `aq/rl` 输入字段。
- 不覆盖 barrier 规则之外的“AMO 已启动时由更老/更年轻普通指令触发 external redirect”场景；当前合法激励不会生成该场景。

## 3. 历轮 Review 结论

原 v1.1 只能支持 22 条基础指令，不能称为当前 RTL 原子功能完整。第二轮发现的 blocker 为：

1. 排除了 `AMOCAS.W/D/Q`。
2. `AMOCAS.Q` 的第二次 final WB 未建模。
3. MOU 被普通 store helper 误分类，会进入 SQ、store 双 real-WB 和 store-fault 生命周期。
4. LDA0 atomic 分流晚于普通 LOAD/LQ attach 会触发 `INT_WB_LDA0_AMO`。
5. atomic STA feedback 没有 SQ owner，现有 SQ-only 绑定会 fatal。
6. `uop_index` 同时表示 slot 和 pipe，不能表达 Q 的 slot 2/3。
7. 现有 queue delete 会删除同 UID/target 的全部 item，第一片 fire 后会误删剩余 CAS 子 uop。
8. 现有 target 级 `queued/dispatched/issue_epoch` 只能表达一片，不能直接作为 CAS 多片完成条件。
9. normal、fault、batch、redirect 和 runtime drain 没有完整的 atomic 聚合条件。

v1.2 以下方案逐项修正这些问题。第三轮根据“必须支持 STA/STD 乱序并提供参数控制”的新增要求继续审查，发现固定 `STD -> STA` 顺序不满足覆盖需求，且仅把 expected counter 改成随机选择会产生 backpressure 下 valid 抖动、重复 slot 和 LR 迟到 STD 风险。

v1.3 采用“owner 建立时一次性生成合法发送表”的最小修改：

1. 增加 `STD_FIRST`、`STA_FIRST`、`RANDOM_LEGAL` 三种参数模式。
2. 发送表保存 `target + slot`，每片仍只发送一次，fire 后精确推进。
3. `RANDOM_LEGAL` 允许 STA/STD 任意交错，但保持 CAS.Q `STA slot0 < STA slot2`。
4. LR 因 RTL 的 DCache 请求不等待 `data_valid`，强制保持 `STD slot0 < STA slot0`；请求 STA-first/random 时对 LR 做合法化并记录日志。
5. ready 为 0 时锁存当前 schedule entry，禁止重新随机选择。

第三轮结论：可控乱序需求已纳入，未引入第二 atomic owner、同周期多片发射或动态物理端口分配；方案仍保持最小改动。

第四轮根据“还需要独立控制 STD 之间乱序”的新增要求继续审查。原 v1.3 的 `RANDOM_LEGAL` 可以顺带打乱 STD，但无法做到：

- 保持 STA/STD 组间模式不变，只单独开关 STD slot 乱序；
- 在 `STD_FIRST` 或 `STA_FIRST` 模式下覆盖 `STD1 -> STD0`、`STD2 -> STD0 -> STD3 -> STD1`；
- 明确区分“STA/STD 交错随机”与“STD 内部排列随机”。

v1.4 增加独立布尔参数 `MEMBLOCK_AMO_STD_SHUFFLE_EN`。schedule builder 分成两步：先构造并可选 shuffle `std_slot_order[]`，再按 `MEMBLOCK_AMO_ISSUE_ORDER_MODE` 把 STA 列表与该 STD 列表组合。第四轮结论：两个参数职责正交，关闭新开关时完全保持 v1.3 的 STD 升序兼容行为。

第五轮根据“每次发射之前都重新随机一次”的要求修订实现时机。v1.5 不再在 owner 建立时预生成完整 `std_slot_order[]`，而是在每次即将选择下一片 STD、且当前没有 held entry 时，从 `required_std_mask & ~fired_std_mask` 中随机选择一片。选择后必须锁存到 fire；fire 后更新 mask，下一片发射前再重新随机。这样既满足逐片动态随机，又不破坏 Decoupled backpressure 语义。

第六轮根据新增需求补齐两项功能。v1.6 在每条 AMO 主事务生成地址时，使用 `MEMBLOCK_AMO_MISALIGN_PERMILLE` 独立抽样一次，默认以千分之一概率生成非对齐地址；抽样结果和最终地址保存在主事务中，普通 redirect 重发时不重新随机。final WB 处理显式区分“带架构 exception 的 fault”和“SC/CAS 正常失败”：前者复用普通访存 fault redirect，后者按 normal writeback/commit 收敛，不触发 redirect。

第七轮根据无操作系统场景修正异常后的推进方式。v1.7 将 AMO 架构异常路径明确写为“UID+1 重发”：异常 AMO 完成 fault redirect 和 terminal 后，commit/admission 游标推进到 `fault_uid+1`，释放 atomic barrier，并从下一条主表事务继续 admission、route 和 issue。这样不会等待软件异常处理，也不会反复执行原异常地址。这里的 UID+1 表示跳过已终结的 fault UID 后继续发送下一条指令，不是把异常 AMO 的内容复制到一个新 UID 再执行。普通 redirect 对其他需要重放的年轻事务仍使用同 UID、`dynamic_epoch+1`，但它与 AMO fault owner 的 UID+1 推进是两类不同机制。

第八轮补齐主表随机生成策略。v1.8 将所有随机生成或预约生成的 AMO 的 `send_pri/send_pri_std` 固定为合法最大值 100；随机到 LR.W/LR.D 后，可按参数在未来 UID 预约同宽度 SC.W/SC.D。预约 SC 覆盖该 UID 原本的普通 opcode 随机，但不覆盖 CSR、SFence、末尾 `check_store` 或其他 LR 已预约的 SC。现有 SC.W/SC.D opcode 权重路径继续保留，因此“独立 SC”和“LR 伴随 SC”可同时存在。

第九轮结合 `memblock_lsq_rtl_equivalent_admission_formal_plan_20261031.md` 和最新源码重新审查 admission。`NewDispatch.scala:447-451` 表明 AMO 与其他指令一样受全局、寄存后的 `lsqCanAccept` 门控；`680-681` 表明 AMO 不做 LQ/SQ 分项 free 比较；`688-706` 表明 AMO 的 `needAlloc=0` 且不产生 `enqLsq.req.valid`。因此 v1.8 的“直接读取软件 LQ/SQ free 并比较 6/4”不是 RTL 等价实现。v1.9 改为：AMO 作为隔离的 non-LSQ dispatch token 经过正式 LSQ shadow 的 D/R/Q 投影，只有旧 `shadow_can_accept` 为真且 ROB/IQ 条件满足时才 admission；admission 不改变 LQ/SQ 资源，之后 STA/STD issue 不重复检查该许可。fault redirect 同步改为统一提交 redirect intent，并等待真实受理、驱动锚点和恢复流水收口后再释放 barrier、推进到 UID+1。

## 4. 最小实现总览

```text
主表按 25 个 opcode 权重生成 AMO，独立SC继续按原权重生成
  -> 所有AMO统一覆盖send_pri=100、send_pri_std=100
  -> 随机到LR时按pair概率选择是否预约未来SC
  -> 按五档距离权重得到dist，目标UID=lr_uid+dist+1
  -> 预约命中时在目标UID强制生成同宽度SC，不扩表、不移动其他UID
  -> same-address命中时复制LR最终地址来源字段，否则为SC独立生成合法地址
  -> 每条AMO生成地址时按permille抽样一次，固定为aligned或misaligned
  -> 等待全部更老 UID terminal
  -> 作为隔离AMO token经过正式LSQ shadow的D/R/Q投影
  -> 仅在旧registered shadow_can_accept为真时完成non-LSQ admission
  -> non-LSQ admission，不改变 LQ/SQ pointer/free count
  -> route 前建立唯一 atomic owner/barrier
  -> 按 opcode 生成全部 STA/STD slot item
  -> aligned按issue-order mode决定下一片应取STA还是STD
  -> misaligned强制全部STD先发送，再发送STA0，Q最后发送STA2
  -> 若取 STD，则在该片发射前按 shuffle 开关从 remaining STD mask 选择 slot
  -> 固定 pipe 0、每拍最多一片，支持 STD-first、STA-first 或随机合法交错
  -> 每片 fire 后按 owner+target+epoch+slot 精确删除
  -> atomic STA feedback 绑定唯一 owner，只做诊断/消费
  -> MemBlock 内部完成 TLB、SBuffer 排空、DCache hit/miss/refill
  -> LDA0 返回 1 次或 2 次 atomic final WB
  -> 全部 final WB 到齐后按exceptionVec分成normal或fault
  -> SC失败/CAS比较失败按normal commit；架构异常走fault redirect/terminal
  -> fault terminal后游标推进到fault_uid+1，释放barrier并继续发射下一条主表指令
```

不新增独立 AMO direct-drive sequence。现有 real-dispatch 已经启动 TLB、DCache、commit、redirect 和 monitor worker，局部扩展的改动更小。

## 5. 详细功能方案

### 5.1 参数、opcode 和主表

修改 `plus.sv`、`seq_csr_common.sv` 和 cfg：

1. 删除 `MEMBLOCK_OP_CLASS_AMO_WT` 必须为 0 的 fatal。
2. 保留已有 22 个基础 opcode 权重。
3. 新增 3 个权重：

```text
MEMBLOCK_AMO_FUOP_AMOCAS_W_WT
MEMBLOCK_AMO_FUOP_AMOCAS_D_WT
MEMBLOCK_AMO_FUOP_AMOCAS_Q_WT
```

4. 三个新权重默认值为 0，避免仅打开旧 AMO 配置时意外扩大场景；full AMO smoke 中设为 1。
5. `MEMBLOCK_OP_CLASS_AMO_WT>0` 时，校验 25 个 opcode 权重之和非零。
6. 主表接受 25 个 opcode，要求 `fuType=MOU`、`lsq_flow=ATOMIC`、`numLsElem=0`。
7. 访问大小和自然对齐：W 为 4B、D 为 8B、Q 为 16B；通过 `MEMBLOCK_AMO_MISALIGN_PERMILLE` 控制是否故意生成非对齐地址。
8. 新增 `MEMBLOCK_AMO_ISSUE_ORDER_MODE`，合法值为 0、1、2；其他值在 cfg load 阶段 fatal。
9. 新增 `MEMBLOCK_AMO_STD_SHUFFLE_EN`，合法值为 0、1；其他值在 cfg load 阶段 fatal。
10. 新增 `MEMBLOCK_AMO_MISALIGN_PERMILLE`，合法值为 0 到 1000，默认值为 1；0 表示关闭非对齐激励，1000 表示每条 AMO 都生成非对齐地址。
11. AMO 使能时要求 `MEMBLOCK_STA_PIP_NUM_LIMIT>=1` 且 `MEMBLOCK_STD_PIP_NUM_LIMIT>=1`；本方案固定使用各自 pipe 0。
12. 所有 random/AUTO 建表生成的 AMO 主事务在普通 priority 随机之后统一覆盖 `send_pri=100`、`send_pri_std=100`。这包括独立 SC 和 LR 预约生成的伴随 SC；manual 主表继续尊重用户显式配置。
13. 新增 LR/SC 配对、同地址和距离选择参数；百分比参数范围为 0 到 100，固定距离范围为 0 到 100，距离权重为非负整数。
14. 独立 SC 保留：`MEMBLOCK_AMO_FUOP_SC_W_WT` 和 `MEMBLOCK_AMO_FUOP_SC_D_WT` 仍参加 25 个 opcode 的普通加权随机，不因增加 LR 伴随 SC 而删除或降权。

不增加 `MEMBLOCK_AMO_ENABLE`。`MEMBLOCK_OP_CLASS_AMO_WT` 已经是类别开关。

每条 AMO 地址生成时只抽样一次：

```text
make_misaligned = random_integer(0..999) < MEMBLOCK_AMO_MISALIGN_PERMILLE
alignment_bytes = W ? 4 : D ? 8 : 16

if make_misaligned:
    address低位选择1..alignment_bytes-1中的非零offset
else:
    address低位清零到自然对齐
```

生成器还必须保证完整的 4B/8B/16B 访问位于 `MEMBLOCK_MAIN_VADDR_BASE/RANGE` 内，不因制造非对齐而越出合法虚拟地址窗口。主事务至少保存 `amo_expect_misaligned` 和 `amo_alignment_bytes`，或者保存等价的已生成地址属性。redirect 重发复用同一主事务地址，不能重新抽样或改地址，否则同一静态指令的架构语义会在不同 dynamic epoch 之间变化。

#### LR 预约伴随 SC

LR.W/LR.D 通过现有 opcode 权重正常随机生成。LR 的最终地址完成普通地址生成、地址复用和边界修正后，再执行一次伴随 SC 决策：

```text
pair_enable = random(0..99) < MEMBLOCK_AMO_LR_SC_PAIR_EN_1_WT

if pair_enable:
    dist = weighted_select(
        FIXED_VALUE,
        0,
        random(1..5),
        random(6..10),
        random(11..100)
    )
    sc_uid = lr_uid + dist + 1
```

`dist` 表示 LR 和 SC 中间隔着多少个 UID：`dist=0` 时 SC 紧跟 LR；`dist=3` 时 LR 与 SC 中间有 3 个 UID。固定值档被选中时直接使用 `MEMBLOCK_AMO_LR_SC_DIST_FIXED_VALUE`；固定值落在其他区间内是合法的，相当于提高该距离的总概率。

主表生成 sequence 内维护最小局部预约表 `sc_reservation_by_uid`，不把配对状态扩散到通用 transaction、RM 或 checker。每项至少保存：

```text
valid
lr_uid
sc_uid
sc_fuOpType          // LR.W->SC.W，LR.D->SC.D
same_addr
lr_src_0 / lr_imm / lr_vaddr
selected_dist
```

生成目标 UID 时的优先级为：

```text
CSR control > SFence control > paired SC reservation > 普通随机 transaction
```

普通随机主表没有控制槽位，因此只有“paired SC reservation > 普通随机”。AUTO 模式末尾固定 `check_store` 不在可预约范围内。预约 SC 只替换尚未生成 UID 的普通随机内容，不改变 `main_trans_num`，不插入新 UID，也不移动后续 UID。例如 `UID10=LR.W, dist=3` 时预约 `UID14=SC.W`，UID11/12/13 仍正常随机。

目标选择必须排除越界、已有 SC 预约、当前已知 CSR/SFence 预约和 AUTO 末尾 `check_store`。如果抽到的距离冲突，则从仍合法的距离候选重新选择；有限次重试后仍无合法目标时，放弃本次伴随 SC 并记录 `lr_uid/dist/原因`。若控制预约在后续重排时才与已有 SC 预约冲突，控制槽位优先，取消该 SC 预约并记录日志，不能覆盖控制事务。因此靠近表尾或控制事务密集时，实际配对率允许低于配置百分比。

同地址决策只在预约成功时抽样一次：

```text
same_addr = random(0..99) < MEMBLOCK_AMO_LR_SC_SAME_ADDR_1_WT
```

命中同地址时，SC 复制 LR 最终的 `src_0` 和 `imm`，调用 `update_vaddr()` 后断言 `sc.vaddr==lr.vaddr`；如果实现还保存边界/地址来源属性，也同步复制等价字段。不能只改缓存的 `vaddr` 而让 `src_0+imm` 不一致。LR 为非对齐地址时，同地址 SC 也保持同一非对齐地址，用于覆盖异常路径。未命中时，SC 按自身 W/D 宽度独立生成合法地址。

预约生成的 SC 不再参加普通 opcode 抽样，但仍执行 AMO 地址属性、priority 覆盖、校验和后续 admission/issue 流程。它即使对应 SC opcode 权重为 0 也可生成；反过来，独立 SC 仍可通过 SC.W/SC.D opcode 权重正常生成。LR 与 SC 之间允许普通访存破坏 reservation，这是需要保留的有效测试场景。

### 5.2 Dispatch/admission 与 registered canAccept

本节以 `memblock_lsq_rtl_equivalent_admission_formal_plan_20261031.md` 为唯一 LSQ admission 时序依据，不再增加 `dispatch_lsq_can_accept()`，也不得用 `lsq_ctrl_model.lq_free_count/sq_free_count` 组合计算 AMO 是否可 admission。

源码合同如下：

1. `NewDispatch.scala:447-451` 将全局 `lsqCanAccept` 同时用于 AMO 的 ready/fire。
2. `LsqEnqCtrl` 的许可是寄存状态，语义为旧 `shadow_can_accept`，不是当前软件 free 的组合比较。
3. 对隔离 AMO，当前接受量 `A_L=A_S=0`；下一份许可计算仍保留 `credit_L>=6 && credit_S>=4` 和 redirect `U2` 条件，但本拍 AMO 能否 fire 只读取旧许可。
4. `NewDispatch.scala:680-681` 对 AMO 不执行 LQ/SQ 分项 `free > flowTotal` 检查；隔离单 owner 下 `store/AMO<=4` 结构限制天然满足。
5. `NewDispatch.scala:688-706` 对 AMO 输出 `needAlloc=0` 且 `enqLsq.req.valid=0`，所以它不进入 MemBlock LSQ enqueue VIF，也没有 LSQ response key。

正式 LSQ 方案为 AMO 增加“隔离 non-LSQ dispatch token”投影：AMO 仍经过 D→R→Q 固定 lane，等待 ROB/IQ 和旧 registered-canAccept 后形成一次 dispatch fire；该 fire 由唯一 service 结算，而不是由空 LSQ driver item 的 `item_done` 冒充。

AMO 与普通 load/store 共用统一 source-dispatch gate。候选能够进入 D/R/Q 或从 Q fire 前，至少同时满足：

```text
control settled
shadow baseline valid
no active redirect/recovery block
no older-terminal block
no conflicting atomic barrier
ROB admission room
```

其中 `shadow_can_accept` 是 Q fire 使用的旧寄存许可，不是当前软件 free count 的组合条件；IQ 接受条件仍在 Q fire 时检查。AMO 单独占一个 source bundle，不能与年轻 load/store、control 或另一条 AMO 同拍 fire，从而保证 atomic barrier 建立前不存在年轻事务越过窗口。

AMO admission 顺序：

1. 等待所有更老 UID terminal。
2. 若已有 atomic owner，继续等待。
3. 按上述统一 source-dispatch gate 确认 control settled、shadow reset 基线有效、无 pending R/Q token、frozen offer、pending LSQ sample、redirect recovery 或冻结合同冲突。
4. 将当前 AMO 作为唯一 D token 放入正式 D/R/Q 投影；不得同时带入年轻事务。
5. Q 中 AMO 仅在旧 `shadow_can_accept=1`、ROB/IQ 可接受且无生效 redirect 时 fire；许可为 0 时保留原 lane 等待，不能根据本拍 deq 或软件 free 提前放行。
6. service 观察该 dispatch fire 后调用新增的 atomic non-LSQ admission wrapper；该调用不得由 zero-time `admit_non_lsq_if_ready()` 提前执行。
7. wrapper 使用本拍 shadow response 的 LQ/SQ index 快照填入 uop 字段，但不建立 owner map；不能直接复用会按软件 reservation pointer 填索引的普通 `commit_allocate()` 路径。随后立即设置 `status.lsq_deq=1`，避免 non-LSQ fault terminal 永久等待不存在的物理 deq。
8. 在发布 `complete_admission()`、允许 route 前建立 atomic barrier，保证年轻 admission 和非 owner issue 看不到中间窗口。
9. route 全部 atomic 子 uop。

AMO admission 后：

- `need_alloc=0`，不驱动 LSQ enqueue valid；
- 不改变 LQ/SQ pointer 和 free count；
- 不建立 LQ/SQ owner map；
- 不创建 LSQ reservation/pending sample，也不等待 LSQ response；
- `lqIdx/sqIdx` 仅是 dispatch 时刻的非 owning shadow pointer 快照，不能用于查询 LQ/SQ owner；
- admission 后必须满足 `active_lq_mapped=0`、`active_sq_mapped=0`、`lsq_reservation_state=NONE`、`lsq_deq=1`；
- STA/STD 已进入 IQ 后，不再用 registered-canAccept、LQ/SQ credit 或软件 free 阻止 issue。

### 5.3 Atomic owner/barrier 最小状态

在 `common_data_transaction` 增加一个全局唯一 owner record，至少包含：

```text
valid
uid + rob_key + dynamic_epoch
base_fuOpType
required_sta_count / fired_sta_count
required_std_count / fired_std_count
required_final_wb_count / seen_final_wb_count
required_feedback_count / seen_feedback_count
required_sta_slot_mask / fired_sta_slot_mask
required_std_slot_mask / fired_std_slot_mask
remaining_std_slot_mask（可由required/fired实时计算，也可缓存）
next_legal_sta_slot / sta_order_state
issued_std_slot_history[0:3] / issued_std_count（仅日志和验证，可选）
held_atomic_piece_valid
held_atomic_piece_target + held_atomic_piece_slot
held_atomic_piece_payload + held_atomic_piece_issue_epoch
primary_sta_issue_epoch
piece_issue_epoch[target][slot]（只需固定小数组或等价字段）
pending_fault_valid + pending_exception_vec
```

STA slot 序列不能由 count 直接推导，必须显式定义：

```text
普通、CAS.W/D：0
CAS.Q：         0,2
```

STD slot 序列为 `0..N-1`。

最小 helper：

1. `establish_atomic_barrier()`：保存 owner、opcode 和所需数量。
2. `is_current_atomic_owner()`：按 UID、ROB key、dynamic epoch 完整比较。
3. `atomic_barrier_blocks_admission()`：阻止年轻事务 admission。
4. `select_next_atomic_piece()`：当前无 held entry 时，按 issue-order mode 选择 target；若为 STD，再从 remaining mask 中选择 slot。
5. `atomic_piece_is_expected()`：检查 target 和 slot 是否等于 held piece，且对应 fired bit 尚未置位。
6. `mark_atomic_piece_fired()`：设置对应 fired bit、清除 held piece 并推进 count；下一片将在下次选择时重新随机。
7. `record_atomic_feedback()`：消费 STA feedback，但不完成指令。
8. `record_atomic_final_wb()`：聚合 normal/fault final WB。
9. `release_atomic_barrier()`：正常路径要求 terminal 且预期 atomic feedback 已全部消费；fault 路径还要求公共 redirect recovery 已完成。普通 runtime reset 不得强制清除已向 AtomicsUnit fire 过任意 piece 的 owner；只有真正断言 MemBlock/AtomicsUnit reset 的物理 reset 才能同步清 DUT 状态和软件 owner。

`runtime_drain_complete()` 必须使用与正式 LSQ 方案共享的统一 drain predicate，同时确认：LSQ shadow D/R/Q 为空、没有 frozen offer、没有 pending LSQ sample、没有未受理 redirect intent 或未完成 redirect recovery、没有 atomic owner/barrier、没有 atomic held piece，且 atomic bitmap/counter 已清空。不得由 LSQ 和 AMO 两侧各自维护一套不完整的“已排空”判断。

### 5.4 STA/STD route、slot 与物理 pipe 解耦

`memblock_issue_q_item_t` 增加：

```text
uop_index        // atomic slot，写入 fuOpType[8:6]
issue_pipe_index // 实际物理 pipe，本方案固定 0
uop_count        // 同 target 总片数
```

修改 `assign_issue_items()`：

- 不再执行 `fired_item.uop_index = pipe_idx`；
- 将 `pipe_idx` 写入 `issue_pipe_index`；
- `port_idx_for_item()` 使用 `issue_pipe_index`；
- field assigner 使用 `uop_index` 形成 atomic `fuOpType`。

route 时一次性建立全部 item：

```text
基础原子：STA[0] + STD[0]
CAS.W/D： STA[0] + STD[0,1]
CAS.Q：   STA[0,2] + STD[0,1,2,3]
```

普通 STA/STD 继续要求 SQ owner。atomic item 不要求 SQ owner，只要求 atomic owner 身份匹配，且 `has_lqIdx=0`、`has_sqIdx=0`。

现有 `delete_issue_queue_entry(target, uid, replay_seq)` 会删除同 target 全部 item，不能用于 CAS。增加 exact delete：

```text
uid + target + replay_seq + dynamic_epoch + uop_index
```

redirect/reset 清理整个 UID 时仍可复用原有批量删除 API。

现有 target 级 `queued/dispatched` 位对 atomic 只表示聚合状态：

- 任一片未 fire 时，target 保持 queued；
- 全部该 target 片 fire 后，才设置 target dispatched；
- atomic eligibility 由 owner 当前 held piece、合法偏序状态和 fired mask 判断，不能因第一片已 fire 就阻止后续片。

现有 `mark_issue_snapshot(uid, target, epoch)` 只有一份 target epoch，不能对每个 CAS 片照搬调用，否则后一片会覆盖前一片。atomic fire 使用专用记账分支：

1. 每片仍分配唯一 issue epoch，并按 `target + slot` 保存在 owner record，便于诊断和 stale 检查。
2. STA slot 0 的 epoch 同时保存为 `primary_sta_issue_epoch`，供 final WB 实例绑定。
3. 普通 status target snapshot 最多发布一次聚合快照，不能被 STA slot 2 或后续 STD slot 覆盖。
4. 不为每片重复调用会注册普通 TLB/target 生命周期的 helper。

### 5.5 参数化合法乱序 scheduler

barrier active 时只允许唯一 owner 的一片 atomic 子 uop 参与仲裁。每拍最多发送一片，固定使用物理 pipe 0，不和普通 load/store 同周期混发。

新增参数：

```text
MEMBLOCK_AMO_ISSUE_ORDER_MODE=0  // STD_FIRST，默认
MEMBLOCK_AMO_ISSUE_ORDER_MODE=1  // STA_FIRST
MEMBLOCK_AMO_ISSUE_ORDER_MODE=2  // RANDOM_LEGAL
MEMBLOCK_AMO_STD_SHUFFLE_EN=0    // STD slot按最小编号选择，默认
MEMBLOCK_AMO_STD_SHUFFLE_EN=1    // 每片STD发射前从remaining集合随机选择
```

#### STD 下一片动态选择

owner 只保存 required/fired mask，不预生成完整 STD 排列：

```text
remaining_std_mask = required_std_slot_mask & ~fired_std_slot_mask
```

当 issue-order mode 判定下一片应发送 STD，并且当前没有已经锁存的 held entry 时：

- shuffle=0：选择 remaining mask 中编号最小的 slot；
- shuffle=1：从 remaining mask 中等概率随机选择一个 slot；
- 将选中的 `target=STD + slot + payload + issue_epoch` 锁存在 held entry 中；
- held entry 一旦建立，必须保持到该片真实 fire；
- fire 后设置对应 fired bit、清除 held entry；
- 下一次需要发送 STD 时，再基于新的 remaining mask 重新随机。

因此 CAS.Q 可以逐步形成：

```text
第一次选择：remaining={0,1,2,3}，随机选2
STD2 fire

第二次选择：remaining={0,1,3}，随机选0
STD0 fire

第三次选择：remaining={1,3}，随机选3
STD3 fire

第四次选择：remaining={1}，只能选1
STD1 fire
```

也可能依次选出 `0,1,2,3`。开关打开代表每片发射前重新抽取 remaining slot，不保证最终一定非升序。

最小伪代码：

```text
if (!held_valid && next_target_is_std) {
    remaining = required_std_mask & ~fired_std_mask;
    if (!std_shuffle_en)
        selected_slot = lowest_set_bit(remaining);
    else
        selected_slot = random_one_of_set_bits(remaining);

    held_entry = find_exact_std_item(owner, selected_slot);
    held_valid = 1;
}

if (held_valid && held_entry.valid && port_ready) {
    fire held_entry;
    fired_std_mask[selected_slot] = 1;
    held_valid = 0;
}
```

这里的“每次发射前随机”指每个新 STD item 被选入 held entry 之前随机一次，而不是在等待同一个 fire 的每个周期都重新随机。否则 `ready=0` 时 payload 会变化，违反握手协议。

#### 模式 0：STD_FIRST

只要 remaining STD 不为空，下一目标始终为 STD；所有 STD fire 后再发送 STA：

```text
基础原子：动态选择STD0 -> STA0
CAS.W/D： 动态选择两个STD -> STA0
CAS.Q：   动态选择四个STD -> STA0 -> STA2
```

shuffle=1 时 CAS.W/D 的最终 STD 顺序可以为 `[0,1]` 或 `[1,0]`；CAS.Q 可以形成全部 24 种排列。

#### 模式 1：STA_FIRST

先按合法 STA 顺序发送，再逐片动态选择 STD：

```text
普通 AMO/SC：STA0 -> 动态选择STD0
CAS.W/D：    STA0 -> 动态选择两个STD
CAS.Q：      STA0 -> STA2 -> 动态选择四个STD
LR：         STD0 -> STA0（安全例外）
```

LR 只有一个 STD，不涉及 STD 间乱序。RTL 对 `M_XLR` 请求不等待 `data_valid`，所以仍保持 STD0 先于 STA0。

#### 模式 2：RANDOM_LEGAL

每次没有 held entry 时，先在“当前合法 STA head”和“remaining STD 集合”之间随机选择本次 target：

- 如果选 STA，使用合法 STA head；CAS.Q 保持 `STA0 -> STA2`；
- 如果选 STD，再按 `MEMBLOCK_AMO_STD_SHUFFLE_EN` 从 remaining STD 中选择具体 slot；
- LR 在 STD0 未 fire 前不能选择 STA0。

例如 CAS.Q 可以动态形成：

```text
选择STD2并fire
选择STA0并fire
选择STD0并fire
选择STA2并fire
选择STD3并fire
选择STD1并fire
```

与预生成完整 schedule 不同，每一次 fire 完成后，下一片的 target/STD slot 都会基于当前 remaining 状态重新随机。

#### 反压与稳定性

无论哪种模式：

1. 随机只发生在 `held_valid=0`、准备选择新片时。
2. 一旦 held entry 建立，即使对应端口 `ready=0`，也不能重新随机。
3. valid、target、slot、payload、owner identity 和 issue epoch 必须保持稳定到 fire。
4. 只有 fire 后才能更新 fired mask 并为下一片重新随机。
5. 每个 required slot 恰好 fire 一次，不得重复、遗漏或跨 owner 混片。

测试框架无需读取 AtomicsUnit 内部 `data_valid`、FSM、`stIsEmpty` 或 DCache 状态。

#### 非对齐 AMO 的顺序合法化

RTL 在 STA 地址进入 AtomicsUnit、TLB 返回且发现地址非对齐后，会直接进入 `s_finish` 返回异常，不会等待所有 STD 数据到齐，也不会发出 DCache atomic request。因此非对齐事务若采用 STA-first，可能出现“STA 已触发异常并使 FSM 完成，迟到 STD 又污染下一条原子事务”的风险。

测试框架对 `amo_expect_misaligned=1` 的 owner 必须覆盖用户配置并强制：

```text
LR/SC/普通AMO：全部STD -> STA0
AMOCAS.W/D：   全部STD -> STA0
AMOCAS.Q：     全部STD -> STA0 -> STA2
```

这里“全部 STD”内部仍受 `MEMBLOCK_AMO_STD_SHUFFLE_EN` 控制，所以 CAS.W/D 可以是 `[0,1]` 或 `[1,0]`，CAS.Q 仍可形成任意合法 STD permutation；只是最后一片 STD fire 之前禁止选择 STA。aligned AMO 继续按 mode 0/1/2 工作。该规则属于测试框架输入协议保护，不是等待 DUT 内部状态的 driver gate。

### 5.6 Issue 字段和 CAS 数据

所有 atomic 子 uop 的实际 opcode：

```text
fuOpType = base_fuOpType[5:0] | (uop_index << 6)
```

字段规则：

- 所有片共享 `uid + rob_key + dynamic_epoch`。
- STA 携带同一个地址；Q 的两个 STA 地址相同。
- STA slot 0 使用主 `pdest`；Q 的 STA slot 2 使用另一个合法且不同的 `pdest`。
- STD 不伪造 LQ/SQ owner。
- 基础原子的 STD slot 0 复用现有数据生成。
- CAS 数据槽语义必须匹配 RTL：

```text
STD0 = compare low  = X(rd)
STD1 = new low      = X(rs2)
STD2 = compare high = X(rd+1)   // 仅 Q
STD3 = new high     = X(rs2+1)  // 仅 Q
```

纯激励不关心这些值能否比较成功，因此用主 transaction 和 slot 的确定性 helper 派生即可，不需要给 transaction 新增四个随机字段。

### 5.7 Atomic STA feedback

`AtomicsUnit` 对每个 STA 输入两拍后产生 `feedbackSlow`，并固定 `hit=1`。该 feedback：

- 不是 final WB；
- 不能设置 UID `writeback/pass`；
- 不能调用普通 `mark_target_normal_pass()`；
- 不受 `MEMBLOCK_STA_REAL_WB_PASS_EN` 控制；
- AMO 不分配 SQ，不能用 `sqIdx` 查 owner。

最小绑定方法：

1. barrier active 且反馈来自固定物理 STA0 port；发送顺序可乱序，但本方案物理 pipe 仍固定为 0；
2. owner 尚有 expected atomic STA feedback；
3. 以唯一 atomic owner 和反馈序号绑定，不使用无 owner 的 SQ 值；
4. 从 owner 的 STA slot FIFO/计数取出对应 per-slot issue epoch，并补齐 `uid/rob/replay_seq/issue_epoch`，满足现有 normalized event 合同；
5. 要求 `hit=1`，否则 fatal；
6. 记录并消费诊断事件，Q 共消费两次。

普通 STA feedback 保持原有 SQ-owner 路径。atomic 分流必须先于普通 SQ-only 校验。
feedback 不决定 atomic 运算是否完成，但 owner 释放前必须确认 `seen_feedback_count == required_feedback_count`，避免释放后出现无法绑定的迟到 feedback。

### 5.8 Atomic final WB 分流与聚合

RTL 路径：

```text
AtomicsUnit.out -> writebackLda_0
AMO writebackStd 被屏蔽
AMOCAS.Q：s_finish 返回低半，s_finish2 返回高半
```

在 `convert_raw_int_wb()` 中，SCALAR_LDA port 0 必须先按 ROB owner/main transaction 判断是否为 MOU，再进入普通 LOAD/LQ attach。atomic 事件归一化为已有枚举：

```text
source = MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB
target = MEMBLOCK_ISSUE_TARGET_STA
owner  = atomic uid + rob_key + dynamic_epoch
```

atomic final WB 不要求 LQ/SQ key。adapter 从 owner/status 补齐 `has_uid/has_rob/has_replay_seq/has_issue_epoch`，其中 issue epoch 使用 `primary_sta_issue_epoch`。身份使用唯一 owner，并要求主 `STA slot0` 已真实 fire；不能要求 WB 到达时所有 STD/STA 子 uop 都已 fire。

原因是乱序模式下 CAS.Q 的 `STA slot2` 可能因 ready backpressure 晚于低半 final WB。第一笔 WB 可以先按唯一 owner 暂存，但 UID 级完成条件必须同时满足：

```text
all_required_pieces_fired
and seen_final_wb_count == required_final_wb_count
```

聚合规则：

- 基础原子和 CAS.W/D：第 1 次 final WB 后完成聚合。
- CAS.Q：第 1 次只累计并暂存 fault；第 2 次到齐后才完成聚合。
- 超过 required count、owner 不匹配、主 STA 尚未 fire 或旧 dynamic epoch 的事件均 fatal/drop，不能绑定新实例。
- final WB 合法早到但尚有 required piece 未 fire 时只暂存，不得提前设置 UID `writeback/pass/fault`。

normal 完成：全部 final WB 到齐且全部 required piece 已 fire 后，一次性设置 UID 级 `status.writeback=1`、`status.pass=1`。不要伪造 `sta_writeback/std_writeback`。

fault 完成：任一 final WB 带 exception 时先暂存；全部 required final WB 到齐且全部 required piece 已 fire 后，再设置 UID 级 `status.writeback=1`、`status.pass=0`、`fault/exception_pending/exception_vec`。`writeback=1` 表示 atomic final event 已完整收齐，不表示普通 STA real-WB。atomic fault 不进入普通 store 双 real-WB pending barrier。

final WB 的分类只看架构异常信息，不把“操作结果失败”误判成 fault：

```text
exceptionVec != 0，或等价的架构 trigger/fault 标记
    -> atomic fault，进入普通访存 fault commit/redirect/terminal 流程

exceptionVec == 0
    -> atomic normal completion
       SC reservation失败、地址不匹配或超时返回失败码：正常提交，不redirect
       CAS比较失败、没有更新内存：正常提交，不redirect
       TLB miss、DCache miss/replay、SBuffer等待：尚未final WB，只是DUT内部等待，不redirect
```

纯激励方案不解析 SC 返回数据和 CAS old value 来判断成功与否；只要 final WB 没有架构异常，就视为 normal writeback/pass。这既满足“失败直接写回后提交”，也避免在没有 checker/RM 的方案中新增结果语义模型。

batch handler 和 `writeback_status_handler::handle_event()` 必须显式路由 `ATOMIC_WB` 到 atomic 聚合分支；不能落入 default/drop，也不能要求 atomic 通过 `is_scalar_store_uid()`。atomic STA feedback 同样走 dedicated handler，不进入普通 STA feedback 的 SQ-owner/pass 路径。

### 5.9 普通 store helper 与 atomic 生命周期剥离

当前 `is_scalar_store_uid()` 返回 `STU || MOU`，必须修正为仅 STU。审计其全部调用点，确保 MOU 不再进入：

- 普通 store STA/STD 双 real-WB 条件；
- pending store fault barrier；
- SQ release/dequeue；
- store redirect defer；
- store batch 合法性判断。

`required_targets_done(uid)` 对 MOU 使用 atomic owner counters：

```text
fired_sta_count == required_sta_count
and fired_std_count == required_std_count
```

它不再只看单个 `status.sta_dispatched/std_dispatched`，也不要求虚假的 STA/STD real-WB。

### 5.10 Commit、terminal、redirect 和 reset

normal 路径：

1. 全部 atomic final WB 到齐，形成 UID 级 writeback/pass。
2. `required_targets_done()` 确认所有子 uop 已 fire。
3. 复用 normal ROB commit；AMO 不增加 `scommit/pendingst`。
4. terminal retire 且预期 atomic feedback 已消费后释放 atomic barrier。

fault 路径：

1. 聚合完全部 required final WB 后才建立 fault/exception pending。
2. 复用现有 fault commit/redirect/terminal。
3. 保持 `fault_uid_is_store_exception()` 对 atomic 返回 false；MOU 不驱动普通 store exception、`scommit` 或 `pendingst` 语义。
4. fault terminal 后进入 barrier 释放候选；只有 fault feedback、redirect drive/anchor和公共 recovery也完成后才实际释放。

fault redirect 与 UID+1 重发规则：

1. atomic final WB 聚合出架构异常后，等该 UID 到 modeled ROB head，再调用正式 LSQ 方案定义的 `submit_redirect_intent()`，提交 `flush_itself=1` 的 scalar fault intent。producer 只标记 pending，不能直接调用 `request_redirect_flush()`、`push_redirect_drive()` 或提前标记 covered。
2. intent 由唯一 service 在 `control_eligible_seq` 到期、单飞 redirect owner 可用时受理；只有建立真实 cancel/redirect record 后才标记 fault-head covered，随后由原 responder 驱动 redirect，并等待 monitor anchor。
3. AMO 不占 LQ/SQ，因此软件取消量为 0；但 registered-canAccept 的 redirect t1/t2/t3 流水、真实驱动脉冲、anchor 和公共恢复阻塞仍必须按正式 LSQ 方案收口，不能因为“没有 LSQ entry”而跳过。
4. 发生异常的 AMO 完成 fault commit/terminal；不再执行该 UID 的原子内容。非对齐地址、页故障或访问故障若原样重试仍会再次异常，因此测试框架不能停留在该 UID。
5. 只有 fault terminal、预期 atomic feedback 已消费、redirect intent/drive/anchor 已完成且正式 LSQ `redirect_recovery_complete` 为真后，才执行 `set_commit_cursor_uid(fault_uid + 1)`、释放 atomic barrier，并允许下一次 admission。
6. 文档将上述动作称为“UID+1 重发”；它不是调用 `alloc_uid()`，也不是把 fault AMO 的 opcode、地址和数据复制到 UID+1。UID+1 是主表中已有的下一条独立指令。
7. 通用 redirect 若覆盖其他需要重新执行的年轻事务，仍可复用同 UID、`dynamic_epoch+1` 的通用机制；不得与 fault AMO 的 UID+1 推进混用。

redirect 约束：

- admission 前所有更老 UID 已 terminal，barrier 又阻止年轻 UID，因此合法主动激励不会在 AMO in-flight 时产生另一条指令的 redirect。
- 尚无任何 STA/STD piece fire 时，覆盖 owner 的普通 redirect 可以清软件 owner和未发 item；一旦任意 STD 或 STA 已 fire，AtomicsUnit 已可能锁存 data、`stdCnt`、pdest 或 FSM 状态，普通 redirect 必须 fail-fast，不能软件清理后重发。
- atomic 自身 fault redirect 只在全部 final WB 已收齐后提交，此时 AtomicsUnit 已通过 `io.out.fire` 正常 `resetFSM()`，因此可以继续 fault redirect 收口。
- 尚未 dispatch fire、只停留在正式 LSQ D/R/Q shadow 中的 AMO token，由 reset 代际切换撤销，不得创建 non-LSQ owner或 barrier。
- admission 后只有真正断言 MemBlock/AtomicsUnit 硬件 reset 的物理 reset 才能清 DUT 原子状态，并与软件 owner/barrier 清理同步。L2TLB reset、control runtime reset 或单纯软件 epoch 变化不复位 AtomicsUnit，覆盖 in-flight owner 时必须 fail-fast。

为避免 fault 收口形成环，公共 `redirect_recovery_complete` 的定义不得等待 atomic barrier 释放，也不得等待不存在的 atomic LQ/SQ owner 被 redirect 删除。fault atomic 的 owner、barrier 和 terminal token 由 AMO 侧继续持有；公共 recovery 只收口 redirect drive/anchor、t1/t2/t3、cancel record 和普通 LSQ owner。AMO 观察公共 recovery 完成后，再结合 fault terminal/feedback 释放 barrier。

## 6. 测试框架输入约束与 DUT 内部行为

### 6.1 测试框架必须保证

1. AMO admission 前所有更老 UID 已 terminal。
2. admission 使用正式 LSQ shadow 发布的旧 registered-canAccept；不得直接用软件 free count 或当前拍 deq 重算许可，同时不分配 LQ/SQ。
3. barrier 在任何 atomic item 进入真实 IQ 前建立。
4. 同时最多一个 atomic owner。
5. barrier active 时不 admission 年轻事务，不 issue 非 owner 事务。
6. 所有子 uop 的 UID、ROB key 和 dynamic epoch 一致。
7. slot、发送次序与物理 pipe 分开；所有片固定 pipe 0，但 STA/STD 次序由参数控制。
8. 每个 required STA/STD slot 恰好 fire 一次；不得重复、遗漏或跨 owner 混片。
9. CAS.Q 始终保证 `STA0` 早于 `STA2`；LR 始终保证 `STD0` 早于 `STA0`。
10. `STD_SHUFFLE_EN=0` 时，每次从 remaining STD mask 中选择最小编号 slot；为 1 时，每次准备发送新的 STD 片都从当前 remaining mask 中重新随机选择。
11. `RANDOM_LEGAL` 先独立选择本次 target 是 STA 还是 STD；若选择 STD，再按 shuffle 开关选择具体 remaining slot，不保存完整 `std_slot_order[]`。
12. 每片 fire 后只删除该 slot，不能删除同 target 的剩余片。
13. ready 为 0 时保持当前 held piece 的 target、slot、payload 和 issue epoch，不得重新随机或换片。
14. AMO dispatch fire 并进入 IQ 后不重复检查 registered-canAccept、shadow credit 或软件 LQ/SQ free。
15. atomic feedback 只做消费/诊断，不作为 final completion。
16. Q 必须等待两次 final WB；其他原子等待一次。
17. CAS 每片具有独立 issue epoch；普通 target snapshot 不被后续 slot 覆盖。
18. 合法早到的 atomic final WB 可以暂存；全部 required piece fire 前不得完成 UID。
19. atomic fault 聚合完成后设置 UID 级 writeback，但不伪造 STA/STD real-WB。
20. normal 等到 terminal 且预期 atomic feedback 已消费后释放 barrier；fault 还必须等待 redirect intent/drive/anchor 和公共 recovery 完成。
21. runtime drain 必须确认 barrier 和 owner 已清空。
22. 每条 AMO 地址生成时只按 `MEMBLOCK_AMO_MISALIGN_PERMILLE` 抽样一次，结果和地址保存到主事务；redirect 重发不得重新随机地址。
23. W/D/Q 分别按 4B/8B/16B 判断自然对齐，非对齐地址仍必须保证完整访问位于配置的虚拟地址窗口内。
24. 非对齐 AMO 强制所有 STD 先于 STA0；CAS.Q 继续保证 STA0 先于 STA2。STD 之间仍可按 shuffle 参数乱序。
25. final WB 只有带架构 exception/trigger fault 时才进入 fault redirect；SC 失败和 CAS 比较失败属于 normal completion，不触发 redirect。
26. fault AMO terminal 后必须将 commit/admission 游标推进到 `fault_uid+1`，释放 barrier，并从 UID+1 对应的下一条主表事务继续发射；不得停留在 fault UID 等待操作系统。
27. 所有 random/AUTO 生成 AMO 的 `send_pri` 和 `send_pri_std` 必须在普通随机赋值之后覆盖为 100；manual 主表不强制覆盖。`MEMBLOCK_SEND_PRI_MODE_EN=0` 时字段仍为 100，但调度器按现有规则不比较 priority。
28. LR 配对命中时，`SC.W/SC.D` 必须分别匹配 `LR.W/LR.D`，并占用不同的未来主表 UID；`sc_uid=lr_uid+dist+1`。
29. paired SC 预约只替换未来普通随机槽位，不扩展主表、不移动后续 UID，也不能覆盖 CSR、SFence、末尾 `check_store` 或另一条 LR 已占用的 SC 预约。
30. paired SC 的同地址命中必须同步复制能决定地址的字段并重新计算 `vaddr`，不能只覆写缓存的 `vaddr`。
31. 独立 SC 继续由 `MEMBLOCK_AMO_FUOP_SC_W_WT/SC_D_WT` 参与普通 opcode 加权随机；paired SC 与独立 SC 是并存的两条生成路径。
32. fault redirect 必须提交 intent 并等待正式 LSQ 控制阶段受理；AMO 的零软件 cancel 数不能跳过 redirect 流水和公共恢复完成条件。
33. reset 前未 fire 的 AMO shadow token不得生成 owner；已 admission 但尚无任何 piece fire 的 owner可按普通 redirect 清理。任意 piece 已 fire 后，只有 MemBlock/AtomicsUnit 物理 reset可同步清 DUT 和软件状态，L2TLB/control runtime reset或软件 epoch变化必须 fail-fast。
34. atomic non-LSQ admission 必须预置 `lsq_deq=1`，并断言无 LQ/SQ map 和 LSQ reservation；携带的 LQ/SQ index 仅为 non-owning snapshot。
35. paired SC reservation 只存在于主表构造阶段；`main_table_ready` 后它已固化为普通 SC 主表项，与运行期 LR reservation、LSQ reservation 无关。
36. source-dispatch gate 和 runtime drain 必须调用 LSQ/AMO 共用 predicate；不得让 AMO 绕过 control settled、shadow baseline、redirect recovery、older-terminal、ROB room，也不得在 atomic owner/held piece 尚存时报告 drain complete。
37. 公共 `redirect_recovery_complete` 不等待 atomic barrier；atomic fault owner不加入 redirect deleted LQ/SQ owner等待，避免 recovery 与 barrier 互等。

### 6.2 DUT 内部行为，不作为 driver gate

以下由 MemBlock/AtomicsUnit/DCache 自己处理：

- 等待 SBuffer、Uncache 或内部 store path 排空；
- `stIsEmpty` 的寄存和聚合延迟；
- TLB/PMP/PMA/PBMT 检查和轮询；
- DCache atomic request ready；
- cache hit、miss、MissQueue、refill 和重试；
- LR reservation 建立、SC 成功或失败；
- 非对齐检测后直接返回 `loadAddrMisaligned` 或 `storeAddrMisaligned`，且不发送 DCache atomic request；
- GrantData denied/corrupt 转换为访问异常或硬件错误；
- `aq/rl` 语义。

测试框架只需正常驱动现有 TLB/DCache responder，并观察最终 WB/fault，不能先等这些内部条件成立再发 STA/STD。

## 7. 参数汇总

### 7.1 必须新增的参数

| 参数 | 默认值 | full AMO smoke | 作用 |
| --- | ---: | ---: | --- |
| `MEMBLOCK_AMO_FUOP_AMOCAS_W_WT` | 0 | 1 | CAS.W 生成权重 |
| `MEMBLOCK_AMO_FUOP_AMOCAS_D_WT` | 0 | 1 | CAS.D 生成权重 |
| `MEMBLOCK_AMO_FUOP_AMOCAS_Q_WT` | 0 | 1 | CAS.Q 生成权重 |
| `MEMBLOCK_AMO_ISSUE_ORDER_MODE` | 0 | 分别回归 0/1/2 | 0=STD-first，1=STA-first，2=合法随机交错 |
| `MEMBLOCK_AMO_STD_SHUFFLE_EN` | 0 | 分别回归 0/1 | 0=选择最小 remaining STD slot，1=每次准备发送新 STD 片时从 remaining mask 随机选择 |
| `MEMBLOCK_AMO_MISALIGN_PERMILLE` | 1 | 分别回归 0/1/1000 | 每条 AMO 地址生成时产生非对齐地址的千分比，范围 0..1000；默认千分之一 |
| `MEMBLOCK_AMO_LR_SC_PAIR_EN_1_WT` | 80 | 分别回归 0/80/100 | LR 生成后预约伴随 SC 的百分比，范围 0..100；未命中概率自动为 `100-value` |
| `MEMBLOCK_AMO_LR_SC_SAME_ADDR_1_WT` | 90 | 分别回归 0/90/100 | paired SC 与 LR 使用相同地址的百分比，范围 0..100 |
| `MEMBLOCK_AMO_LR_SC_DIST_FIXED_WT` | 0 | 定向设为非零 | 固定距离档权重；与其余四档共同参与加权随机 |
| `MEMBLOCK_AMO_LR_SC_DIST_FIXED_VALUE` | 0 | 覆盖 0 和边界值 100 | 固定距离档选中时使用的 `dist`，范围 0..100 |
| `MEMBLOCK_AMO_LR_SC_DIST_0_WT` | 1 | 1 | `dist=0` 档权重，即 SC 紧跟 LR |
| `MEMBLOCK_AMO_LR_SC_DIST_1_5_WT` | 1 | 1 | `dist=1..5` 档权重；档内等概率随机 |
| `MEMBLOCK_AMO_LR_SC_DIST_6_10_WT` | 1 | 1 | `dist=6..10` 档权重；档内等概率随机 |
| `MEMBLOCK_AMO_LR_SC_DIST_11_100_WT` | 1 | 1 | `dist=11..100` 档权重；档内等概率随机 |

`MEMBLOCK_AMO_ISSUE_ORDER_MODE` 只控制同一 atomic owner 内部 STA/STD 子 uop 的先后关系，不控制物理 pipe，不允许多 owner 交叉，也不改变 barrier、admission 或 final WB 行为。

`MEMBLOCK_AMO_STD_SHUFFLE_EN` 只控制同一 owner 内下一片 STD slot 的选择。默认值 0 保持兼容；值为 1 时，每当上一片已经 fire、当前没有 held piece 且调度器准备选择下一片 STD，就从尚未发送的 slot 集合重新随机一次。CAS.W/D 最终可形成 `[0,1]` 或 `[1,0]`；CAS.Q 通过四次逐步选择最终可形成 24 种 permutation 中的任意一种，包括 `[0,1,2,3]`。随机性使用现有 sequence seed，不增加独立 seed 参数。

`MEMBLOCK_AMO_MISALIGN_PERMILLE` 同时承担开关和权重功能，避免再增加一个 enable：0 等价于关闭，1 为默认千分之一，1000 为全部非对齐。随机只在主事务首次生成地址时执行一次；同一 UID 后续 dynamic epoch 重发复用原地址。

`MEMBLOCK_AMO_LR_SC_PAIR_EN_1_WT` 和 `MEMBLOCK_AMO_LR_SC_SAME_ADDR_1_WT` 使用单一“1 的权重”参数表达布尔概率，不增加对应的 `_0_WT`。四个区间距离权重默认均为 1，因此 `DIST_FIXED_WT=0` 时四档各占 25%。固定档启用后和四档一起参加加权选择；所有五档权重同时为 0 时 cfg load 阶段 fatal。paired SC 不依赖 SC opcode 权重，因此即使独立 SC 权重为 0，LR 仍可按配对参数生成 SC。

### 7.2 复用的生成参数

- `MEMBLOCK_MAIN_TRANS_NUM`
- `MEMBLOCK_OP_CLASS_AMO_WT`
- 已有 22 个 `MEMBLOCK_AMO_FUOP_*_WT`
- 其余 op-class 权重；纯 AMO smoke 中设为 0
- `MEMBLOCK_MAIN_VADDR_BASE/RANGE`
- `MEMBLOCK_BOUNDARY_PROFILE_GEN_EN`；普通边界 profile 继续工作，AMO 的自然对齐/非对齐由专用 permille 参数在合法地址窗口内决定

总 opcode 权重为 25 项。单 opcode directed smoke 使用 one-hot 权重，不能依赖一次随机 seed 自然覆盖全部 opcode。

### 7.3 复用的调度和流程参数

| 参数 | 建议 smoke 值 | 说明 |
| --- | ---: | --- |
| `MEMBLOCK_STA_PIP_NUM_LIMIT` | 1 | atomic 固定 STA0 |
| `MEMBLOCK_STD_PIP_NUM_LIMIT` | 1 | atomic 固定 STD0，slot 通过 `fuOpType[8:6]` 区分 |
| `MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN` | 0 | 首轮 smoke 使用阻塞握手；实现需兼容 0/1 |
| `MEMBLOCK_DISPATCH_READY_TIMEOUT` | 5000 | issue ready 超时 |
| `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` | 60000 | 长 miss/refill 场景告警 |

完整生命周期要求以下 worker 保持开启：

```text
MEMBLOCK_LSQENQ_SEQ_EN=1
MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1
MEMBLOCK_LSQCOMMIT_SEQ_EN=1
MEMBLOCK_REDIRECT_SEQ_EN=1
MEMBLOCK_L2TLB_SEQ_EN=1
```

DCache/SBuffer/Uncache responder 继续使用现有 vseq 启动方式，不增加 AMO 专用 enable。

### 7.4 明确不新增

```text
MEMBLOCK_AMO_ENABLE
MEMBLOCK_AMO_MAX_ACTIVE
MEMBLOCK_AMO_STA_PORT_MASK
MEMBLOCK_AMO_WAIT_STISEMPTY_EN
MEMBLOCK_AMO_WAIT_TLB_EN
MEMBLOCK_AMO_WAIT_DCACHE_EN
MEMBLOCK_AMO_AQ_*
MEMBLOCK_AMO_RL_*
```

## 8. 文件修改范围

### 8.1 必改文件

| 文件 | 最小修改 |
| --- | --- |
| `env/plus.sv` | 新增 3 个 CAS 权重、issue order mode、STD shuffle、misalign permille 和 8 个 LR/SC 配对参数并加载。 |
| `seq_csr_common.sv` | 读取/校验 25 个权重、order mode、shuffle enable、misalign permille、pair/same-address 百分比、固定距离范围和五档距离权重，解除 AMO blanket fatal。 |
| `default.cfg`、AMO smoke cfg | CAS 默认 0；order mode/shuffle 默认 0；misalign 默认 1；pair 默认 80%、同地址默认 90%、固定档默认关闭、四个随机距离档默认各 1；full smoke 覆盖关键组合。 |
| `memblock_dispatch_types.sv` | `issue_pipe_index`；必要的 atomic owner/event 字段。 |
| `memblock_dispatch_base_sequence.sv` | 生成并校验 25 opcode；AMO priority 覆盖为 100；按 permille 生成地址；在 random/AUTO 建表循环中维护局部 `sc_reservation_by_uid`，处理配对、距离、同地址和控制槽位冲突。 |
| `lsq_ctrl_model.sv` | 复用正式 LSQ 方案的 registered-canAccept、D/R/Q shadow 和 reset/recovery 状态；增加 atomic non-LSQ wrapper，写 non-owning index snapshot、预置 `lsq_deq=1`，不改 reservation pointer/count。 |
| `memblock_lsqenq_dispatch_base_sequence.sv` | 按正式 LSQ 方案扩展隔离 AMO token；dispatch fire 后按“commit non-LSQ -> 建 barrier -> complete admission”顺序发布。 |
| `common_data_transaction.sv` | owner/barrier、required/fired bitmap、held piece、子 uop/final WB counters、exact delete、STU/MOU 剥离、runtime drain。 |
| `issue_queue_scheduler.sv` | route 全部 slot、按 target/remaining mask 动态选择下一片；非对齐 owner 强制 STD-all-before-STA；atomic 聚合 queued/dispatched。 |
| `issue_field_assigner.sv` | slot opcode、CAS 数据和 Q 第二 pdest。 |
| `memblock_issue_dispatch_base_sequence.sv` | slot/pipe 解耦，fire 后精确记账。 |
| `dispatch_monitor_event_adapter.sv` | atomic feedback 和 LDA0 `ATOMIC_WB` 前置分流。 |
| `writeback_status_handler.sv` | atomic final WB 聚合；按 exception/trigger fault 区分架构异常与 SC/CAS normal failure。 |
| `dispatch_monitor_batch_handler.sv` | atomic batch 不走普通 store 分类。 |
| `lsq_commit_handler.sv` | atomic 架构异常提交 fault redirect intent；等待实际受理和公共 recovery 完成后推进到 `fault_uid+1`。 |
| `exception_redirect_replay_handler.sv` | atomic fault/redirect 不走 store 双 WB defer；按正式 LSQ 方案统一受理 intent，配合 UID+1 恢复点清除旧 fault 动态状态。 |

是否修改 `status_transaction.sv` 由实现选择决定。优先把多片 counter 保存在唯一 atomic owner record，避免扩大每 UID status；只在现有 API 无法表达 UID 级 final 状态时增加最少字段。

### 8.2 原则上不改

- 不改 RTL/Scala。
- 不改 agent interface、driver 端口和 DUT wrapper。
- 不新增 testcase class 或 virtual sequence。
- 不改 DCache、SBuffer、Uncache、L2TLB responder 算法。
- 不改 RM、scoreboard、checker 和 coverage。

现有 DCache responder 已支持 `AcquireBlock/AcquirePerm/GrantData/GrantAck` 和 denied/corrupt 注入；atomic miss 走 DUT 的普通 TileLink 流程，无需 AMO 专用 responder。

## 9. Coding 计划与状态

### P0：方案与源码确认

状态：[x] 已完成

- [x] 确认完整 25 opcode 和 W/D/Q 访问大小。
- [x] 确认 1/2/4 个 STD、Q 的 STA slot 0/2。
- [x] 确认 Q 产生两次 LDA0 final WB。
- [x] 确认 atomic feedback 固定 hit 且 AMO 无 SQ owner。
- [x] 确认 DCache responder 无需新增专用协议。

### P1：配置、生成和 admission

状态：[x] 已完成

- [x] 增加 3 个 CAS plus 权重并扩展为 25 项校验。
- [x] 增加并校验 `MEMBLOCK_AMO_ISSUE_ORDER_MODE=0/1/2`。
- [x] 增加并校验 `MEMBLOCK_AMO_STD_SHUFFLE_EN=0/1`。
- [x] 增加并校验 `MEMBLOCK_AMO_MISALIGN_PERMILLE=0..1000`，默认 1。
- [x] 增加并校验 8 个 LR/SC 参数：pair/same-address 百分比、固定距离权重和值、四个距离区间权重；五档权重不得全零。
- [x] 接受全部 25 opcode，并在每条主事务地址生成时一次性产生 W/D/Q aligned 或 misaligned 地址。
- [x] 在普通 priority 随机之后，将所有 random/AUTO AMO 的 `send_pri/send_pri_std` 覆盖为 100；manual 主表保持显式配置。
- [x] 在 random/AUTO 建表循环中维护局部 `sc_reservation_by_uid`；LR 命中 pair 后按 `sc_uid=lr_uid+dist+1` 预约同宽度 SC。
- [x] 预约命中 UID 时跳过普通 opcode 随机并构造 SC；保留独立 SC.W/SC.D 的现有 opcode 权重生成路径。
- [x] 同地址命中时复制 LR 的 `src_0/imm` 和等价地址属性，重新计算并校验 SC `vaddr`；未命中时独立生成合法地址。
- [x] 排除越界、已预约 SC、CSR、SFence 和末尾 `check_store`；有限重试失败后安全放弃配对并记录日志。
- [x] 保存地址对齐属性，保证 redirect 重发复用原地址且完整访问不越出 MAIN_VADDR window。
- [x] 依赖正式 LSQ 方案实现 registered-canAccept 和 D/R/Q shadow，不新增软件 free-count 组合 gate。
- [x] 将 AMO 从 zero-time `admit_non_lsq_if_ready()` 移入隔离 shadow token 路径，旧 registered-canAccept 为真时才形成 dispatch fire。
- [x] dispatch fire 后由 atomic non-LSQ wrapper 写 shadow index snapshot、建立无 LQ/SQ map 的 ROB owner并设置 `lsq_deq=1`；再按建立 barrier、`complete_admission()` 的顺序发布。
- [x] 复用 LSQ/AMO 统一 source-dispatch gate，组合 control settled、shadow baseline、redirect recovery、older-terminal、atomic barrier、ROB room 和 Q 级 IQ/旧 canAccept 条件；AMO 独占 source bundle。

### P2：多子 uop route 与 issue

状态：[x] 已完成

- [x] 将 atomic slot 与物理 pipe 解耦。
- [x] route 全部 STA/STD slot。
- [x] 增加 exact queue delete。
- [x] 实现 `STD_FIRST/STA_FIRST/RANDOM_LEGAL` 的下一 target 动态选择，不预生成完整 schedule。
- [x] 实现 remaining STD mask；shuffle 关闭时选择最低 set bit，打开时从 set bits 中随机选择一项。
- [x] RANDOM_LEGAL 将“选择 STA/STD target”和“选择具体 STD slot”拆成两步。
- [x] 实现 owner-only、每拍最多一片、held piece 在 ready backpressure 下 target/slot/payload/epoch 稳定的发送逻辑。
- [x] 仅在上一 held piece fire 并清空后，才为下一片重新随机；禁止 ready=0 时逐拍重选。
- [x] 使用 required/fired bitmap 拒绝重复、遗漏和跨 owner slot。
- [x] 强制 CAS.Q `STA0 < STA2`，强制 LR `STD0 < STA0`。
- [x] 对非对齐 owner 强制全部 required STD 先于 STA0，CAS.Q 再按 `STA0 < STA2`；STD 内部仍支持 shuffle。
- [x] 增加 atomic per-slot issue epoch，避免覆盖普通 target snapshot。
- [x] 确认第一片 fire 不会设置整个 target 完成或删除剩余片。

### P3：feedback、final WB 与生命周期

状态：[x] 已完成

- [x] atomic feedback 在普通 SQ-owner 校验前分流并消费。
- [x] LDA0 atomic 在普通 LOAD/LQ attach 前归一化为 `ATOMIC_WB`。
- [x] normal/fault 聚合 1 或 2 次 final WB。
- [x] 只把 exception/trigger fault 标记的 final WB 归为 fault；SC失败、CAS比较失败归为 normal pass。
- [x] atomic fault 通过 `submit_redirect_intent()` 进入统一控制阶段；terminal、feedback 和 redirect recovery 全部收口后，以 `fault_uid+1` 作为下一 admission/issue 起点。
- [x] 支持 CAS.Q 低半 final WB 早于 STA2 fire 的暂存，禁止提前完成 UID。
- [x] MOU 从普通 store helper、双 WB 和 SQ 生命周期剥离。
- [x] pre-fire shadow token reset 不建 owner；owner尚无piece fire时允许普通redirect清软件状态；任意 piece fire 后普通 redirect、L2TLB/control runtime reset均 fail-fast；只有 MemBlock 物理 reset 同时复位 DUT FSM 和软件 owner；runtime drain 检查无残留。
- [x] 公共 redirect recovery 不等待 atomic barrier或不存在的 atomic LSQ owner；AMO 侧在公共 recovery、fault terminal和feedback均完成后再释放 barrier。

### P4：验证

状态：基础验证已完成；完整专项矩阵待后续回归

- [x] 静态检查和完整 VCS/KDB 编译。
- [x] `tc_lsq_amo_admission_soft` 定向测试通过，`UVM_ERROR=0`、`UVM_FATAL=0`。
- [x] 普通 real-dispatch smoke 通过，确认默认 AMO 权重为 0 时原标量路径未回归。
- [x] AMO real-dispatch smoke 通过，观察到 LR/SC pair、7 个 atomic owner 完成 feedback/final WB 和 global-stop 收口。
- [ ] 22 条基础指令 one-hot smoke。
- [ ] CAS.W、CAS.D、CAS.Q one-hot smoke。
- [ ] 验证 CAS.W/D 的 2 STD，Q 的 4 STD、2 STA、2 final WB。
- [ ] 对 mode 0 验证固定 STD-first 顺序。
- [ ] 对 mode 1 验证非 LR 的 STA-first 顺序，并验证 LR 自动合法化为 STD-first。
- [ ] 对 mode 2 使用多 seed 验证合法 STA/STD 交错、slot 完整唯一和 CAS.Q/LR 偏序。
- [ ] 在 mode 0 + shuffle 1 的多 seed 回归中验证 CAS.W/D 同时出现 `[0,1]` 和 `[1,0]`，CAS.Q 同时出现升序和非升序排列。
- [ ] 在 mode 1 + shuffle 1 下验证 STA 组先发、STD 组内部乱序。
- [ ] 在 mode 2 + shuffle 0 下验证 STA/STD 可交错但 STD 相对顺序仍为 `0,1[,2,3]`。
- [ ] 在 mode 2 + shuffle 1 下验证每次选到 STD target 后都基于当时的 remaining mask 重新选择 slot。
- [ ] 记录并检查每次选择前 remaining mask、selected slot、held piece、fire 和 fire 后 remaining mask。
- [ ] 在 issue ready backpressure 下验证随机只在 held piece 为空时发生一次；等待期间不切换，fire 后下一片才重新随机。
- [ ] 定向压住 CAS.Q STA2 ready，验证低半 WB 早到时被暂存，STA2 和第二笔 WB 完成后才 finalize。
- [ ] 验证普通 load/store 与 AMO 前后 barrier。
- [ ] 验证旧 registered-canAccept=0 时 AMO 即使 `needAlloc=0` 也不能 admission；本拍 deq 不能组合旁路，后续寄存许可恢复后才 fire。
- [ ] 验证 AMO fire 时所有 LSQ VIF `req.valid=0/needAlloc=0`，软件和 shadow LQ/SQ pointer/count 均不因该 AMO 改变。
- [ ] 验证 AMO owner 建立后 `lsq_deq=1`、无 active LQ/SQ map、无 LSQ reservation；fault AMO 不因等待物理 deq 卡死。
- [ ] 验证 AMO 的 `lqIdx/sqIdx` 来自 dispatch shadow response 快照，但不能解析出 LQ/SQ owner。
- [ ] 验证 AMO wrapper 不使用软件 reservation pointer代替 shadow response snapshot；shadow pointer与reservation pointer暂时不一致时仍写入正确的 non-owning index。
- [ ] 验证 hit、miss/refill、denied、corrupt 的 normal/fault terminal。
- [ ] 验证无 piece fire 的 owner可被普通 redirect清理；任意 STD/STA fire 后普通 external redirect及非物理 runtime reset fail-fast；MemBlock物理reset可同步清DUT和软件状态。
- [ ] 验证统一 runtime drain 同时等待 shadow D/R/Q、frozen offer、pending LSQ sample、redirect intent/recovery、atomic owner/barrier/held piece/counter 清空。
- [ ] 验证 fault atomic 不加入 redirect deleted LQ/SQ owner等待，且 `redirect_recovery_complete` 可在 barrier仍有效时完成，随后由 AMO 侧释放 barrier，无环等待。
- [ ] 验证 misalign permille=0 时无非对齐 AMO，=1000 时全部非对齐，=1 时使用多 seed 观察低概率注入且统计口径正确。
- [ ] 分别验证 W/D/Q 非对齐低位不为零且完整访问不越出 MAIN_VADDR window。
- [ ] 验证非对齐 LR 返回 load-address-misaligned，SC/AMO/CAS 返回 store-address-misaligned，且不产生 DCache atomic request。
- [ ] 验证非对齐 owner 无论 order mode 为何均被合法化为 STD-all-before-STA；STD shuffle 仍可产生不同 permutation。
- [ ] 验证架构异常 final WB 到 ROB head 后只提交一次 redirect intent；实际受理、drive、anchor、t1/t2/t3 和 recovery 收口后 commit/admission 游标才变为 `fault_uid+1`。
- [ ] 验证 atomic barrier 释放后首先 admission/issue 的主表事务是 UID+1，不会再次发射 fault UID。
- [ ] 验证 SC失败和 CAS比较失败 final WB 无 exception 时不触发 redirect，正常 writeback、commit、terminal。
- [ ] 验证所有 random/AUTO 独立或 paired AMO 的 `send_pri/send_pri_std` 都为 100；manual 主表不被覆盖；`SEND_PRI_MODE_EN=0` 时字段不变但不要求仲裁优先级生效。
- [ ] pair 权重 0/100 定向验证完全关闭/尽可能为每条有合法目标的 LR 预约 SC。
- [ ] same-address 权重 0/100 定向验证独立地址/相同 `vaddr`，并检查 `src_0+imm` 与缓存 `vaddr` 一致。
- [ ] 分别定向选择 fixed、0、1..5、6..10、11..100 五类距离；确认 `dist=0` 得到 `sc_uid=lr_uid+1`。
- [ ] 验证 LR.W 只配 SC.W、LR.D 只配 SC.D，且 paired SC 与 LR 使用不同 UID。
- [ ] 验证预约 SC 覆盖目标 UID 的普通随机内容但不改变主表长度或移动其他 UID。
- [ ] 将独立 SC opcode 权重设为非零，确认独立 SC 仍可生成；再设为 0，确认 paired SC 仍可由 LR 生成。
- [ ] 验证 paired SC 不覆盖 CSR、SFence、末尾 `check_store` 或其他 SC 预约；表尾无合法目标时安全放弃且建表完整。
- [ ] 验证 LR 与 paired SC 之间可以保留普通访存，允许 reservation 被破坏而不影响激励流程收敛。

### P5：方案 Review

状态：[x] 已完成

- [x] 第一轮 review 找出基础顺序和 redirect 风险。
- [x] 第二轮 review 推翻 v1.1 的“功能完整”结论。
- [x] v1.2 补齐 CAS、多片、Q 双 WB、feedback 和 fault/terminal。
- [x] 第三轮增加 STA/STD 可控乱序，并识别 LR 与 CAS.Q 的必要偏序。
- [x] 第三轮采用预生成 schedule，避免每拍随机导致 Decoupled payload 不稳定。
- [x] 第四轮增加独立 STD slot shuffle 开关并完成两阶段 schedule review。
- [x] 第四轮确认 shuffle 覆盖全部合法排列、merge 不破坏 STD 子序列、反压期间 entry 稳定。
- [x] v1.4 结论：无剩余方案级 blocker，可以进入 coding。
- [x] 第五轮将“owner 建立时生成完整排列”改为“每片 fire 后按 remaining mask 重新选择下一片”。
- [x] 第五轮确认随机点与 Decoupled 反压兼容：held piece 建立后保持到 fire，不能等待期间逐拍重选。
- [x] v1.5 结论：无剩余方案级 blocker，可以进入 coding。
- [x] 第六轮加入 `MEMBLOCK_AMO_MISALIGN_PERMILLE`，确认随机点为每条主事务地址生成一次而非每次重发。
- [x] 第六轮确认非对齐 AMO 必须强制 STD-all-before-STA，避免 AtomicsUnit 提前 finish 后出现迟到 STD。
- [x] 第六轮将 final WB 分成架构 fault 与 SC/CAS normal failure，只有前者触发 fault redirect。
- [x] 第六轮核对普通 redirect 的同 UID、`dynamic_epoch+1` 通用重放机制。
- [x] v1.6 结论：新增需求功能完整，无剩余方案级 blocker，可以进入 coding。
- [x] 第七轮根据无操作系统环境，将 AMO fault 恢复明确为 terminal 后按 `fault_uid+1` 继续发射下一条主表指令。
- [x] 第七轮区分“fault UID+1 推进”和“普通年轻事务同 UID、dynamic_epoch+1 重放”，避免测试框架停留在永久异常指令。
- [x] v1.7 结论：无操作系统异常推进语义明确，无剩余方案级 blocker，可以进入 coding。
- [x] 第八轮增加所有 AMO 的最高 priority 覆盖，以及 LR 预约伴随 SC 的参数化生成流程。
- [x] 第八轮确认独立 SC 保留、paired SC 不依赖 SC opcode 权重，并完成控制槽位冲突、表尾降级和同地址字段一致性 review。
- [x] v1.8 结论：LR/SC 主表生成需求功能完整，保持最小局部改动，无新增 RM/checker 或通用 transaction 状态。
- [x] 第九轮结合正式 LSQ admission 方案复核 AMO 的全局 registered-canAccept、needAlloc=0 和 `enqLsq.req.valid=0` 源码语义。
- [x] 第九轮删除软件 free-count 组合 gate，增加隔离 AMO D/R/Q token、统一 redirect intent，并区分 pre-fire撤销、无piece-fire软件清理与piece已fire后的物理reset收口。
- [x] 修订后由 subagent 进行最终只读复核，结论为 Blocker 无、Major 无；未运行编译或仿真。
- [x] v1.9 结论：与正式 LSQ 方案交叉 blocker 已清除，两份方案可按依赖顺序进入 coding；尚未编译或动态验收。
- [x] 2026-11-04 独立 implementation review 完成，结论为 Blocker、Major、Minor 均无；残余项均为 P4 扩展测试缺口。

## 10. 验收标准

1. 25 个 opcode 均可通过 one-hot cfg 生成，不在参数、主表或 field assign 阶段 fatal。
2. AMO admission 只读取正式 LSQ shadow 的旧 registered-canAccept；不得直接读取软件 free count。AMO fire 不产生 LSQ enqueue valid/needAlloc，不改变 shadow 或软件 LQ/SQ pointer/count。
3. shuffle 0 时，任意 issue-order mode 中 STD 子序列都保持 `0,1[,2,3]`。
4. shuffle 1 时，多 seed 下 CAS.W/D 同时覆盖 `[0,1]` 和 `[1,0]`；CAS.Q 允许包括 `[0,1,2,3]` 在内的全部 24 种排列，并至少观察到一种升序和一种非升序结果。
5. mode 0 下，只要 remaining STD 非空就动态选择下一片 STD；全部 STD fire 后再执行 `STA0 [-> STA2]`。
6. mode 1 下非 LR 先执行 STA 组，再对 remaining STD 逐片选择；LR 被合法化为 `STD0 -> STA0`。
7. mode 2 下 STA/STD 可合法交错；每次选择到 STD target 时，具体 slot 都由当前 remaining mask 决定，不依赖预生成顺序。
8. 任意参数组合下，每个 required STA/STD slot 恰好 fire 一次；无重复、遗漏或跨 owner 混片。
9. CAS.Q 始终 `STA0 < STA2`；LR 始终 `STD0 < STA0`。
10. ready 为 0 时当前 valid、target、slot、payload 和 issue epoch 保持稳定，直到 fire。
11. shuffle 1 时，每个新 STD held piece 的选择都使用当时的 `required_std_mask & ~fired_std_mask`；上一片 fire 后必须产生新的选择动作，不能提前保存后续完整顺序。
12. 全部 STD 完成时 `fired_std_slot_mask == required_std_slot_mask`；全部 STA 完成时 `fired_sta_slot_mask == required_sta_slot_mask`。
13. 所有片使用物理 pipe 0，但 `fuOpType[8:6]` 保留正确 slot。
14. 第一片 fire 后剩余同 target item 仍在队列中。
15. barrier active 期间没有年轻 UID admission 或非 owner issue fire。
16. atomic feedback 不要求 SQ owner、不形成 final completion，Q 正确消费两次。
17. atomic feedback 和 final WB 都补齐 normalized event 所需的 ROB、replay seq、issue epoch 元数据。
18. LDA0 atomic 事件不进入 LOAD/LQ lifecycle，source 为 `ATOMIC_WB`。
19. Q 的第一次 final WB 不会 commit、fault finalize 或释放 barrier；即使此时 STA2 尚未 fire，也能绑定当前 owner并暂存。
20. Q 的 STA0/STA2 和各 STD slot issue epoch 不互相覆盖；final WB 使用 primary STA epoch 绑定。
21. atomic fault 完成设置 UID `writeback/fault`，但 `sta_writeback/std_writeback` 保持未伪造状态。
22. MOU 不进入普通 store 双 real-WB、SQ release 和 store-fault barrier。
23. normal/fault 均能 commit、terminal；预期 feedback 消费后释放 barrier。
24. hit、miss/refill、denied、corrupt 场景均不会因测试框架生命周期缺口卡死。
25. 任何 final WB 只有在全部 required piece 已 fire 后才能形成 UID 级完成。
26. sequence 退出时 atomic owner/barrier/held piece/bitmap/counter 全部清空。
27. `MEMBLOCK_AMO_MISALIGN_PERMILLE` 只接受 0..1000，默认 1；0/1000 定向用例分别生成全对齐/全非对齐 AMO。
28. 每条 AMO 主事务只进行一次 misalign 抽样；同 UID 的 redirect reissue 地址和 `amo_expect_misaligned` 不变。
29. W/D/Q 的自然对齐分别为 4B/8B/16B；misaligned 地址低位必须违反对应对齐且完整访问不越出合法地址窗口。
30. 非对齐 AMO 的所有 required STD 均先于 STA0 fire；CAS.Q 的 STA2 仍晚于 STA0。shuffle 开启时 STD 内部仍可乱序。
31. 非对齐地址在 RTL final WB 中产生正确的 load/store address misaligned exception，且该动态实例没有 DCache atomic request。
32. atomic final WB 带架构 exception/trigger fault 时只提交一次 redirect intent；只有 intent 受理、drive、monitor anchor、公共 redirect recovery 和 fault terminal 均完成后，commit/admission 游标才推进到 `fault_uid+1`。
33. barrier 只能在 fault feedback 和公共 redirect recovery 均收口后释放；之后必须从 UID+1 对应的下一条主表事务继续 admission/route/issue，且 fault UID 不得再次进入 IQ。
34. SC失败和 CAS比较失败在 `exceptionVec==0` 时形成 normal pass，正常 commit/terminal，且不产生 redirect。
35. 所有 random/AUTO AMO（含独立 SC 和 paired SC）的 `send_pri/send_pri_std` 均为 100；manual 主表保持显式值；priority 模式关闭时只验证字段，不强求仲裁行为。
36. `PAIR_EN_1_WT=0` 时不产生 paired SC；为 100 时，每个存在合法预约目标的 LR 都产生同宽度 paired SC。
37. paired SC 使用 `sc_uid=lr_uid+dist+1`；`dist=0` 时 SC 紧跟 LR，其他区间的 UID 间隔与所选 dist 一致。
38. `SAME_ADDR_1_WT=100` 时 SC 的 `src_0/imm/vaddr` 与 LR 地址语义一致；为 0 时 SC 独立生成合法地址。
39. LR.W 只预约 SC.W，LR.D 只预约 SC.D；paired SC 和 LR 占不同 UID，主表长度和其他 UID 顺序不变。
40. fixed、0、1..5、6..10、11..100 五类距离都可通过参数定向选中；五档全零在配置加载时 fatal。
41. paired SC 不覆盖 CSR、SFence、AUTO 末尾 `check_store` 或既有 SC 预约；冲突/越界且无替代目标时安全放弃并记录日志。
42. 独立 SC 仍可由 `SC_W_WT/SC_D_WT` 生成；即使这两个权重为 0，paired SC 仍可由 LR 配对参数生成。
43. AMO token 必须经过 D→R→Q shadow 且只在旧 registered-canAccept 为真时 fire；当前拍 deq 只能影响后续许可，不能旁路放行本拍 AMO。
44. AMO 已 admission 后，shadow credit 或 registered-canAccept 后续变低不得阻止其 STA/STD issue。
45. reset 清理区分 pre-fire token、无 piece fire 的 owner和已 fire owner：pre-fire token不产生 owner；无 piece fire owner可由普通 redirect清理；任意 piece已 fire后只有 MemBlock/AtomicsUnit物理reset可同步清DUT和软件状态，非物理 runtime reset必须fail-fast。
46. AMO source-dispatch 必须通过统一 gate，且单独占 source bundle；control未settled、shadow baseline无效、redirect recovery未完成、older未terminal、atomic barrier冲突或ROB/IQ无空间时均不得fire。
47. atomic non-LSQ wrapper使用shadow response的LQ/SQ pointer snapshot，立即设置`lsq_deq=1`，且不建立LQ/SQ map、reservation或pending sample。
48. runtime drain只有在LSQ shadow D/R/Q、frozen offer、pending sample、redirect intent/recovery和atomic owner/barrier/held piece/counter全部清空后成立。
49. `redirect_recovery_complete`不等待atomic barrier或atomic LQ/SQ owner；fault AMO在公共recovery完成后再释放barrier，定向验证不存在环等待。

这些标准验证激励功能和生命周期完整性，不验证原子计算结果正确性。

## 11. 风险与最小处理

| 风险 | 最小且完整的处理 |
| --- | --- |
| CAS 扩大通用多 uop 复杂度 | 只为唯一 atomic owner 保存固定 bitmap、STA 顺序状态和一个 held piece，不建设通用多 owner 乱序状态机。 |
| STD shuffle 与全局 target random 互相覆盖 | 分两级决策：先选择本次 target；若 target 为 STD，再从当前 remaining mask 选择 slot。 |
| shuffle 产生重复或遗漏 slot | 只允许从 `required_std_mask & ~fired_std_mask` 选择；fire 后置 fired bit，最终断言 fired mask 等于 required mask。 |
| shuffle enable 打开后单条用例仍得到原序 | 原序是合法随机结果；通过固定多 seed 回归同时覆盖原序和非原序，不修改随机结果。 |
| “每片重新随机”被误实现为“每拍重新随机” | 只在 `held_valid=0` 时选择一次并锁存；ready=0 时保持，fire 后清 held，下一片才进行新的随机选择。 |
| 重复 slot 让 `stdCnt` 误计数 | required/fired bitmap + exact delete 双重阻止重复 fire。 |
| LR STA-first 导致迟到 STD 污染下一事务 | 所有模式都强制 LR `STD0 < STA0`，并打印模式合法化日志。 |
| CAS.Q STA2 先于 STA0破坏主 uop识别 | STA 顺序状态只在 STA0 fire 后开放 STA2，并在选择和 fire 两处断言。 |
| CAS.Q 低半 WB 早于受阻的 STA2 | 按唯一 owner 暂存早到 WB；piece 和 WB 两组条件同时满足后再 finalize。 |
| slot 与 pipe 冲突 | 增加一个 `issue_pipe_index`，不改现有 driver 接口。 |
| 第一片误删剩余片 | 增加 exact delete，保留批量 API 给 redirect/reset。 |
| Q 第一 WB 被当 terminal | owner 保存 required/seen final WB count，必须等 2。 |
| feedback 无 SQ owner | barrier active 时按唯一 owner和固定 STA0 port绑定。 |
| MOU 污染普通 store 生命周期 | `is_scalar_store_uid()` 改为 STU-only，atomic 使用专用聚合。 |
| 用软件 `lq_free_count/sq_free_count` 代替 registered-canAccept | 删除本地组合 gate；AMO 只消费正式 LSQ shadow 的旧寄存许可。 |
| 将 AMO `needAlloc=0` 误解为无需全局 LSQ gate | 源码仍在 `fromRename.ready/fire` 上统一与 `lsqCanAccept`；隔离 AMO token 必须等待该许可。 |
| AMO 通过 zero-time non-LSQ helper 越过 D/R/Q | `admit_non_lsq_if_ready()` 不处理 MOU；AMO dispatch fire 由唯一 shadow service 结算。 |
| non-LSQ wrapper用软件 reservation pointer填写AMO索引 | 新增atomic专用wrapper，显式接收shadow response snapshot；索引仅为non-owning uop字段，不建立owner map。 |
| atomic fault等待不存在的LSQ deq而卡死 | atomic owner建立时立即置`lsq_deq=1`，并断言无LQ/SQ map和reservation。 |
| LSQ recovery等待barrier、barrier又等待recovery | 公共recovery不包含atomic barrier和atomic LSQ owner；AMO侧单向等待公共recovery后释放barrier。 |
| 两侧分别实现drain导致遗漏在途状态 | 使用共享runtime drain predicate，一次性覆盖shadow、offer、pending sample、redirect和atomic状态。 |
| atomic miss 需要新 responder | 复用已有 TileLink responder，只补 hit/miss/error smoke。 |
| redirect 清软件状态但 DUT FSM 未复位 | 合法 barrier 场景不生成该 redirect；异常出现时 fail-fast。 |
| misalign 抽样在 redirect 重发时再次执行 | 只在主事务首次生成地址时抽样并保存，dynamic epoch 重发复用原地址。 |
| 非对齐 STA 先到导致 AtomicsUnit 先 finish、STD 迟到 | 对 misaligned owner 覆盖 order mode，强制全部 STD 先于 STA0；CAS.Q 继续 STA0 后 STA2。 |
| 把 SC/CAS 的结果失败误当架构异常 | final WB 仅依据 exception/trigger fault 分类；无 exception 的 SC/CAS 结果一律 normal commit。 |
| 无操作系统时停留在 fault UID | fault terminal 后显式推进 commit/admission 到 `fault_uid+1`，释放 barrier 后继续发送下一条主表事务。 |
| 将“UID+1重发”误解为复制异常 AMO | UID+1 是主表中已存在的下一条独立指令；不得复制 fault UID 的 opcode、地址或数据，也不调用 `alloc_uid()`。 |
| 混淆 UID+1 与 dynamic epoch | AMO fault owner 使用 UID+1 推进；只有普通 redirect 中确实需要重放的其他事务才使用同 UID、`dynamic_epoch+1`。 |
| AMO 无 LSQ entry 就提前结束 redirect recovery | 软件 cancel 数可为 0，但 drive/anchor/t1-t3/单飞 recovery 仍必须收口后才能释放 barrier。 |
| reset 仅清软件 owner而 DUT AtomicsUnit 仍运行 | 无piece fire前可按普通redirect清理；任意piece已fire后仅接受真正断言MemBlock/AtomicsUnit reset的物理reset，并同步清DUT和软件状态。 |
| 只覆盖 `send_pri`、遗漏 `send_pri_std` | AMO 模板完成后同时覆盖两个字段为 100，并在主表校验中断言。 |
| `SEND_PRI_MODE_EN=0` 时误认为 100 必须改变仲裁 | 字段仍固定为 100，但调度器是否比较 priority 继续由现有总开关决定。 |
| `dist=0` 被实现成 LR 与 SC 相同 UID | 统一公式 `sc_uid=lr_uid+dist+1`，并断言 `sc_uid>lr_uid`。 |
| paired SC 覆盖控制事务或末尾 `check_store` | 控制槽位优先；预约前排除已知控制 UID，后续冲突则取消 SC 预约并记录日志。 |
| 多条 LR 预约同一 UID | `sc_reservation_by_uid` 写入前检查占用；冲突后从剩余合法距离重选，失败则放弃本次 pair。 |
| paired SC 被普通随机再次覆盖 | 建表循环先检查预约表；命中预约后直接构造 SC 并跳过普通 opcode 随机。 |
| 同地址只复制 `vaddr` 导致事务校验失败 | 复制 `src_0/imm` 和等价地址属性，再调用 `update_vaddr()` 并断言与 LR `vaddr` 一致。 |
| paired SC 错误依赖独立 SC opcode 权重 | 预约路径直接指定 SC.W/SC.D；独立 SC 权重只影响普通随机路径。 |
| 表尾或控制事务密集导致实际配对率低于配置 | 有限重试合法距离；没有合法未来 UID 时允许放弃并统计，不扩表、不覆盖保留槽位。 |

## 12. Review 修订记录

### 12.1 第一轮

1. 将基础原子的发送顺序固定为 STD0 后 STA0，避免 LR 先完成后出现迟到 STD。
2. 对 STA 已 fire、final WB 前的外部 redirect 改为 fail-fast，因为 redirect 不直接复位 AtomicsUnit FSM。

### 12.2 第二轮

1. 将支持范围从 22 条基础指令扩展为当前 RTL 的完整 25 条原子指令。
2. 增加 CAS.W/D 的 2 STD、CAS.Q 的 4 STD、2 STA 和 2 final WB。
3. 增加 slot/pipe 解耦、exact delete 和多片聚合规则。
4. 使用已有 `MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB`，并要求 LDA0 atomic 前置分流。
5. 增加无 SQ owner 的 atomic feedback 专用绑定。
6. 将 MOU 从普通 store 双 WB、SQ 和 store-fault 生命周期中剥离。
7. 补齐 normal/fault/batch/redirect/reset/runtime-drain 的终态条件。
8. 新增 3 个必要 CAS 权重；其余 AMO 调度行为保持固定，不增加模式参数。

### 12.3 第三轮

1. 根据新增要求取消“所有 AMO 固定 STD-first”的唯一调度策略。
2. 新增 `MEMBLOCK_AMO_ISSUE_ORDER_MODE=0/1/2`，分别覆盖 STD-first、STA-first 和合法随机交错。
3. 使用每 owner 预生成 schedule，而不是每拍随机选择，保证 ready backpressure 下 Decoupled 输入稳定。
4. owner 增加 required/fired STA/STD bitmap，防止重复 slot 使 AtomicsUnit `stdCnt` 误计数。
5. 保留 CAS.Q `STA0 < STA2` 的 RTL 协议约束。
6. 将 LR `STD0 < STA0` 定义为所有模式下的必要安全约束；STA-first/random 对 LR自动合法化。
7. 保持物理 STA0/STD0、单 owner和每拍最多一片不变，避免为乱序覆盖引入动态端口和多发射改造。
8. 修正 final WB 聚合条件：CAS.Q 允许低半 WB 早于 STA2，先暂存，不能因“尚有 piece 未 fire”而 fatal。

### 12.4 第四轮

1. 增加独立 `MEMBLOCK_AMO_STD_SHUFFLE_EN`，不再要求通过全局 RANDOM_LEGAL 才能覆盖 STD 内部乱序。
2. schedule 构造拆成“STD slot 排列”和“STA/STD merge”两阶段。
3. shuffle 关闭时保持 STD 升序，确保默认配置与 v1.3 完全兼容。
4. shuffle 打开时仅在 owner 建立阶段执行一次标准 Fisher-Yates，并锁存 `std_slot_order[]`；原序与非原序都合法。
5. RANDOM_LEGAL merge 必须保持 STD 子序列顺序，避免第二次随机破坏独立开关语义。
6. 增加 mode 0/1/2 与 shuffle 0/1 的组合验证和验收标准。

### 12.5 第五轮

1. 根据新增要求，取消 v1.4“owner 建立时一次生成完整 STD 排列”的当前实现方案。
2. owner 只保存 required/fired STD mask 和一个 held piece，不保存 `std_slot_order[]`。
3. 每次调度器需要新的 STD 片、且 `held_valid=0` 时，从当前 remaining mask 选择具体 slot。
4. shuffle 关闭时选择最低编号 remaining slot；shuffle 打开时对当前 remaining set 做一次随机选择。
5. 选择出的 slot、payload 和 issue epoch 锁存到 fire；端口反压期间不得重新随机。
6. fire 后更新 fired mask 并清除 held piece，下一次需要 STD 时再执行新的随机选择。
7. RANDOM_LEGAL 分离 target 随机和 STD slot 随机，避免预生成或固化完整 STD 子序列。
8. 第五轮 review 确认不需要新增端口、多发射或第二 owner，v1.5 无方案级 blocker。

### 12.6 第六轮

1. 新增单参数 `MEMBLOCK_AMO_MISALIGN_PERMILLE`，范围 0..1000、默认 1，同时表达开关和千分比权重。
2. 明确每条 AMO 主事务地址生成时只抽样一次；redirect reissue 不改变地址或对齐属性。
3. W/D/Q 分别按 4B/8B/16B 生成自然对齐或故意非对齐地址，并保证完整访问不越出地址窗口。
4. 根据 RTL 的非对齐提前 finish 行为，对 misaligned owner 强制全部 STD 先于 STA0；STD 内部 shuffle 继续有效。
5. final WB 以架构 exception/trigger fault 为 redirect 判据；SC reservation失败和 CAS比较失败仍是 normal completion。
6. 架构 fault 复用普通访存 fault-head redirect/terminal；fault AMO 自身不重复执行原子内容。
7. 第六轮曾将用户所说的“UID+1”解释为普通 redirect 的同 UID、`dynamic_epoch+1` 重放；第七轮根据无操作系统环境要求修正该解释。
8. 第六轮关于非对齐生成、顺序保护和 final WB 分类的结论继续有效；异常后的恢复点以第七轮为准。

### 12.7 第七轮

1. 将 AMO 架构异常后的测试框架恢复统一定义为“UID+1 重发”。
2. fault AMO 完成 redirect/terminal 后，执行 `set_commit_cursor_uid(fault_uid+1)`，并将下一 admission 起点同步推进到 UID+1。
3. 释放 atomic barrier 后，从主表 UID+1 对应的下一条独立指令继续 admission、route 和 issue，不等待操作系统修复异常上下文。
4. fault UID 不再次进入真实 IQ，避免非对齐、页故障或权限故障在无操作系统环境中永久循环。
5. UID+1 重发不复制 fault AMO，也不调用 `alloc_uid()`；UID+1 是建表时已经存在的下一条事务。
6. 普通 redirect 若确实需要重放其他事务，仍保留同 UID、`dynamic_epoch+1` 的通用机制；不得与 AMO fault owner 的 UID+1 推进混用。
7. 新增定向验收：fault UID=N 后下一次 admission/issue 必须为 UID=N+1，且 UID=N 不再 route。
8. v1.7 结论：无操作系统异常恢复不会停留在 fault 指令，满足继续运行后续激励的要求。

### 12.8 第八轮

1. 保留 SC.W/SC.D 的原有 opcode 权重随机路径，独立 SC 不因 LR 伴随 SC 功能被删除。
2. random/AUTO 生成的全部 AMO 在普通 priority 随机后统一覆盖 `send_pri=100`、`send_pri_std=100`；manual 主表保持用户显式值。
3. LR 生成并完成最终地址修正后，按 `MEMBLOCK_AMO_LR_SC_PAIR_EN_1_WT` 决定是否预约伴随 SC。
4. 距离统一定义为中间 UID 数量，目标公式为 `sc_uid=lr_uid+dist+1`；固定距离档和四个区间档共同参加加权随机。
5. paired SC 通过建表 sequence 的局部 `sc_reservation_by_uid` 保存，不增加通用 transaction、RM 或 checker 状态，不扩展主表长度。
6. AUTO 模式保持 `CSR > SFence > paired SC > 普通随机`；末尾 `check_store` 永不被预约覆盖。后续控制预约冲突时取消 paired SC 并记录日志。
7. 同地址命中时复制 `src_0/imm` 和等价地址属性后重新计算 `vaddr`，避免只写缓存地址导致事务校验失败。
8. paired SC 不依赖 SC opcode 权重；因此独立 SC 权重为 0 时仍可配对生成，而独立 SC 权重非零时两条路径可以同时产生 SC。
9. review 修正了文件修改表中的重复 `env/plus.sv` 项，并明确 priority 强制仅针对 random/AUTO 建表，避免破坏 manual 主表语义。
10. v1.8 结论：功能完整前提下只增加配置读取和建表局部预约逻辑，无需新增主表长度管理、运行期配对状态或结果 checker。

### 12.9 第九轮

1. 交叉 review `memblock_lsq_rtl_equivalent_admission_formal_plan_20261031.md`，确认 v1.8 的软件 free-count 组合 gate 与正式逐拍 shadow 方案冲突。
2. 根据 `NewDispatch.scala:447-451`，确认 AMO 尽管不分配 LSQ，仍受全局、寄存后的 `lsqCanAccept` 门控。
3. 根据 `NewDispatch.scala:680-681,688-706`，确认 AMO 不做 LQ/SQ 分项 free 检查，`needAlloc=0` 且不产生 `enqLsq.req.valid`。
4. 删除 `dispatch_lsq_can_accept()` 设想；AMO 改为正式 LSQ D/R/Q shadow 中的隔离 non-LSQ token，旧 registered-canAccept 为真时才 fire。
5. AMO dispatch fire 后按 `commit_atomic_non_lsq_admission -> establish_atomic_barrier -> complete_admission` 发布，不创建 LSQ reservation、response key 或 pending sample。
6. 明确 registered-canAccept 只管 admission；AMO 已进真实 IQ 后，STA/STD issue 不再检查 shadow credit、软件 free 或后续许可变化。
7. fault redirect 改用 `submit_redirect_intent()`；零 LSQ cancel 不能跳过实际受理、drive、anchor、t1/t2/t3 和单飞 recovery 收口。
8. barrier 释放和 UID+1 推进增加 `redirect_recovery_complete` 条件，防止恢复流水未排空时年轻事务提前 admission。
9. reset 区分 pre-fire shadow token、尚无 piece fire 的 owner和已 fire owner：前者按 reset epoch 撤销，中间状态可由普通 redirect清理，后者仅允许 MemBlock/AtomicsUnit物理reset同步复位DUT和软件状态。
10. 增加atomic专用non-LSQ wrapper：使用shadow response index snapshot、预置`lsq_deq=1`，不复用可能读取软件reservation pointer的普通分配路径。
11. 统一LSQ/AMO source-dispatch gate和runtime drain predicate，补齐control settled、shadow baseline、redirect recovery、ROB/IQ、frozen offer、pending sample及atomic held状态。
12. 明确公共`redirect_recovery_complete`不等待atomic barrier或atomic LSQ owner，fault AMO单向等待公共recovery后释放barrier，消除互等风险。
13. v1.9复审结论：AMO 与 LSQ admission 两份方案接口已统一，无剩余方案级 blocker，可按“先 LSQ shadow 基础、再 AMO token/owner、最后 fault/reset 联调”的顺序 coding。

## 13. 后续协同

本计划不实现 RM/checker/scoreboard。后续正确性组件可消费 atomic owner、opcode、地址、STD slot 数据、STA/STD fire、final WB 数据/异常和 terminal 事件；old/new value、SC 成败、reservation 和 CAS 比较结果必须在独立 checker 方案中推导，不能回填到纯激励生命周期。

## 14. 实现与基础验证结果

最终验证目录：

```text
mem_ut/ver/ut/memblock/sim/amo_plan_verify13_20261104/log/
```

| 验证项 | 结果 |
|---|---|
| 完整 VCS/KDB 编译 | 0 error；Verdi KDB 成功生成 |
| `tc_lsq_amo_admission_soft` | PASS；覆盖 registered admission、pending/reset 和 held-piece 基础合同 |
| 普通 real-dispatch smoke | TEST_PASS；默认 `OP_CLASS_AMO_WT=0` 路径无回归 |
| AMO real-dispatch smoke | TEST_PASS；观察到 LR/SC pair、7 个 atomic owner 完成 feedback/final WB 和 global-stop |
| 独立 implementation review | Blocker 0、Major 0、Minor 0 |
| `git diff --check` | PASS |

本轮完成的是功能 coding 和基础可运行性验证，不把下列未覆盖场景标为已完成：

- CAS.Q 两次真实 final WB 的 DUT 端到端定向场景。
- AMO fault redirect、公共 recovery 和 `fault_uid+1` 的干净端到端场景。
- 非对齐 AMO、三种 order mode、STD shuffle 多 seed 和 CAS.Q STA2 backpressure。
- LR/SC 五档距离、同/异地址、独立 SC 和预约冲突矩阵。
- reset/redirect/global-stop 与 D/R/Q、held piece、部分已 fire owner 的系统交叉回归。

这些项目继续保留在 P4，属于后续覆盖增强，不影响当前实现进入扩展回归阶段。
