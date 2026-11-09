# MemBlock AMO 与 LSQ Admission 交叉 Review（第九轮）

状态：`方案 review 已完成，后续 coding 与 implementation review 已通过，已归档`

## 1. Review 信息

- Review 日期：2026-11-03
- Review 环境：分支 `mem_ut_uvm_v2`，HEAD `4acf3cc3c381671cb7bb4dc34eeaec9287825c80`
- Review 对象：
  - `AI_DOC/plan/test_framework/plan/do/memblock_amo_stimulus_only_adaptation_plan_20261022.md`
  - `AI_DOC/plan/test_framework/plan/do/memblock_lsq_rtl_equivalent_admission_formal_plan_20261031.md`
- Review 方式：主流程源码复核 + subagent Hypatia 独立交叉 review + 主流程回填修订
- 本轮范围：只完善方案文档，不修改生产代码，不运行编译或仿真

## 2. 修订前结论

两份方案单独阅读时方向基本正确，但交叉后不可直接 coding，存在四个方案级 blocker：

1. AMO admission 一度使用软件 LQ/SQ free count，而 RTL 实际使用旧的寄存 `lsqCanAccept`；AMO 又不能走现有 zero-time non-LSQ 快速路径。
2. atomic non-LSQ owner 没有物理 LSQ entry，若不在 admission 时预置 `lsq_deq=1`，fault terminal 可能永久等待不存在的 deq。
3. AMO fault redirect 必须进入 LSQ 方案的统一 intent/唯一控制写者流程，不能由 fault producer 直接建立 flush record或驱动 redirect。
4. AtomicsUnit 任意 STD fire 都会写 data并推进 `stdCnt`；任意 STA/STD piece 已 fire 后，普通 redirect或非物理 runtime reset都不能只清软件 owner。

此外还有四项必须在 coding 前统一的接口问题：source-dispatch gate、runtime drain、shadow index snapshot ownership，以及 redirect recovery 与 atomic barrier 的无环关系。

## 3. 源码事实

本轮重新确认：

1. `NewDispatch.scala:447-451`：AMO 与其他指令一样受全局 `lsqCanAccept` 门控。
2. `NewDispatch.scala:680-681`：AMO 不参与 LQ/SQ 分项 free 前缀检查。
3. `NewDispatch.scala:688-706`：AMO `needAlloc=0` 且不产生 `enqLsq.req.valid`。
4. `LSQWrapper.scala:405-415`：`lsqCanAccept` 是寄存许可，本拍 deq不能组合旁路成本拍许可。
5. `AtomicsUnit.scala:159-167`：任意 STD fire都会更新原子数据状态和`stdCnt`。
6. `AtomicsUnit.scala:420-428`：普通 redirect不会复位AtomicsUnit FSM、data或`stdCnt`。
7. `lsq_commit_handler.sv:934`：fault terminal需要`terminal_done && lsq_deq`，因此non-LSQ atomic必须显式闭合该条件。

## 4. 已完成修订

### 4.1 AMO 方案

1. AMO改为隔离的source-dispatch non-LSQ token，经过正式D→R→Q投影并读取旧`shadow_can_accept`。
2. 增加统一source-dispatch gate：control settled、shadow baseline、redirect recovery、older terminal、atomic barrier、ROB room和Q级IQ条件共同决定fire。
3. AMO独占source bundle，不与年轻load/store、control或另一条AMO同拍fire。
4. 增加atomic专用non-LSQ wrapper：显式接收shadow response的LQ/SQ index snapshot，不建立LQ/SQ map或reservation，并立即设置`lsq_deq=1`。
5. fault redirect统一调用`submit_redirect_intent()`；等待实际受理、drive、monitor anchor、t1/t2/t3、公共recovery、fault terminal和feedback后才推进UID+1并释放barrier。
6. reset边界改为：pre-fire token可撤销；owner尚无piece fire时可由普通redirect清理；任意piece已fire后只有MemBlock/AtomicsUnit物理reset可同步清理，其他runtime reset必须fail-fast。
7. 增加共享runtime drain条件，覆盖LSQ shadow、frozen offer、pending sample、redirect和atomic全部在途状态。

### 4.2 LSQ 正式方案

1. 增加3.3.1统一source-dispatch gate与共享drain predicate，普通load/store和隔离AMO不得各自实现不同门控。
2. 5.6改用`commit_atomic_non_lsq_admission(...)`计划接口，明确shadow pointer snapshot是non-owning字段，不能按软件reservation pointer填写或用于owner查询。
3. 明确wrapper完成后的不变量：

```text
active_lq_mapped == 0
active_sq_mapped == 0
lsq_reservation_state == NONE
lsq_deq == 1
```

4. 明确AMO不建立LSQ pending sample，不等待LSQ response，也不改变shadow或软件LQ/SQ pointer/count。
5. 明确公共`redirect_recovery_complete`不等待atomic barrier、atomic terminal token或不存在的atomic LQ/SQ owner；AMO侧单向等待公共recovery后释放barrier，避免互等。
6. 验收矩阵增加non-owning snapshot、fault terminal、piece-fire后reset/redirect、统一drain和无环recovery场景。

## 5. Coding 结论

修订完成后，subagent Hypatia 对两份最终文本进行了第二次只读复核，结论为：

```text
Blocker：无
Major：无
```

复核确认 registered-canAccept、atomic non-LSQ `lsq_deq`、redirect唯一写者、piece-fire后物理reset边界，以及公共recovery与atomic barrier的无环关系均已闭合。

修订后，两份方案在当前限定范围内已达到方案级可 coding：

1. LSQ方案可实现标量load/store的RTL等价admission，以及单owner、隔离标量AMO token接口。
2. AMO方案可在该接口上实现25个opcode的生成、多STA/STD piece发射、final WB聚合和normal/fault生命周期。
3. 不支持AMO与load/store混合source batch、多AMO同批、多个atomic owner或完整后端最大吞吐；这些不属于本轮功能范围。

建议coding顺序固定为：

```text
LSQ shadow与D/R/Q
-> control settled、redirect intent与公共recovery
-> 统一source gate和drain predicate
-> atomic non-LSQ wrapper
-> AMO owner/barrier与多piece issue
-> final WB、fault/UID+1与reset联调
```

## 6. 动态验证声明

本轮只完成静态方案review和文档修订，没有修改生产代码，没有运行编译、仿真或回归。因此“可 coding”只表示设计接口和约束已闭合，不表示实现已经PASS。coding完成后必须按两份方案的专项矩阵验证registered-canAccept逐拍行为、fault `lsq_deq`、redirect唯一写者、物理reset边界和recovery/barrier无环收敛。
