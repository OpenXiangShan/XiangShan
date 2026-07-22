# IntER 普通整数指令 Writeback-Resolved 优化实施计划

## Goal Description

在当前 non-speculative integer early register release（IntER）实现上，把满足严格白名单和运行时安全条件的普通整数指令，从“实际 normal commit 时才 `resolved`”优化为“最终、未被取消且不携带异常/重定向的 writeback 被 ROB 接受后 `resolved`”。目标覆盖：

- 简单 ALU：`FuType.alu`；
- 乘除法：`FuType.mul`、`FuType.div`；
- 其它普通整数执行：`FuType.bku`、`FuType.i2f`、`FuType.i2v`。

优化只前移 ROB speculation tracker（ST）对 redefiner 的 non-speculative 证明，不改变程序序 commit、RAT/RAB 恢复、UCA consumer/producer 证明、FreeList conventional release 或 Difftest 的架构提交语义。

当前实现中：

```plain
safeToCross(entry) = entry.intER.resolved || commitSafeNow(entry)

entry.intER.resolved 初始为 false
entry.intER.resolved 只在 actual normal commit 时置 true
```

优化后：

```plain
safeToCross(entry) = entry.intER.resolved || commitSafeNow(entry)

entry.intER.resolved =
    commit-resolved
 || eligible-final-writeback-resolved
```

ST 保持严格程序序：任何更老的 Branch、Load/Store、AMO、CSR、Fence、VSet、FP/Vector、compressed/multi-uop 或其它未证明安全的 entry 仍会阻挡年轻整数 entry。因此本任务不会把 memory、branch 或系统指令的恢复风险转嫁给普通整数路径。

### 当前性能依据

`mydocs/new-er/task34/int-er-bottleneck-performance-counter-report.md` 的完整 emu-basic 聚合结果给出：

| 指标 | 当前值 | 含义 |
| --- | ---: | --- |
| ST valid-frontier blocker cycles | 14,419,612 | 有效 ROB entry 阻挡 ST 的总周期。 |
| `other_integer` blocker cycles | 2,197,927 | 占 valid-frontier blocker 的 15.243%。 |
| `other_integer / not_resolved` | 422,617 | 已完成 writeback、仅因当前 resolved 边界保守而阻挡的分类上界，占 valid-frontier blocker 的 2.931%。 |
| `other_integer / not_writebacked` | 1,775,310 | 尚未最终 writeback，不能由本优化消除。 |

422,617 个周期是当前 workload 和配置下的机会口径，不是 IPC 提升承诺；其中还可能包含本计划明确排除的 VSet 等 `other_integer` 成员，因此实际可消除部分只会小于等于该值。实现后的硬性门槛是功能正确、counter 闭包成立和完整 emu-basic 通过；IPC、cycle 和 free-list pressure 只作趋势比较，不设置强制正收益阈值。

## Proposed Design

### 1. 配置开关

在 `IntEarlyReleaseParams` 中增加独立开关，例如：

```scala
enableOtherIntegerWritebackResolve: Boolean = false
```

并增加对应 `HasXSParameter` accessor。参数默认值保持关闭，避免所有临时单测配置自动改变语义；`WithIntEarlyReleaseFunctional` 显式置为 `true`，因此最终 `DefaultConfig` 中该优化开启。`IntERDisabledConfig` 仍不生成 IntER 元数据和逻辑。

该开关用于：

- correctness bisect；
- 同一 RTL 下做 commit-resolved 与 writeback-resolved A/B；
- emu-basic 失败时快速判断是否与新边界相关。

### 2. 功能白名单与性能分类分离

不能直接使用：

```plain
entry.intER.instClass == other_integer
```

作为功能白名单。当前 `FuType.isInt` 还包含 `jmp/brh/csr/fence/vsetiwi/vsetiwf`；虽然 Branch、CSR、Fence 被 `classifyInst` 的更高优先级排除，VSet 仍可能落入 `other_integer` 性能分类。性能分类只能回答“谁在阻塞”，不能证明“谁可以提前 resolved”。

在 enqueue 时由完整 `EnqRobUop` 计算独立的 `writebackResolveEligible`，并保存到 ROB IntER metadata。功能白名单如下：

| 类别 | FuType | 处理 |
| --- | --- | --- |
| simple ALU | `alu` | 满足全部结构和控制条件时允许 final-WB resolved。 |
| MUL | `mul` | 满足全部条件时允许。 |
| DIV | `div` | 只有最终、非 replay、非 exception writeback 被接受后允许。除零是正常 RISC-V 结果，不是同步异常。 |
| other integer | `bku` | 按普通无副作用整数结果处理。 |
| other integer | `i2f` | 单 uop 最终 writeback 后允许；fflags/FS 仍按原 commit 路径更新。 |
| other integer | `i2v` | 单 uop 最终 writeback 后允许；向量映射和提交语义不变。 |
| VSet | `vsetiwi/vsetiwf/vsetfwf` | 明确排除，继续 commit-resolved。 |
| Branch/Jump | `brh/jmp` | 明确排除，继续 commit-resolved。 |
| Load/Store/AMO | `ldu/stu/mou` | 明确排除。 |
| CSR/Fence | `csr/fence` | 明确排除。 |
| FP/Vector arithmetic | 其它 FP/Vector FuType | 明确排除。 |

### 3. Enqueue eligibility

`writebackResolveEligible` 必须同时满足：

```plain
feature enabled
&& firstUop
&& lastUop
&& numUops == 1
&& numWB == 1
&& commitType == NORMAL
&& fuType in {alu, mul, div, bku, i2f, i2v}
&& !hasException
&& !exceptionVec.orR
&& !singleStep
&& trigger is not debug-mode action
&& !flushPipe
&& !replayInst
&& !waitForward
&& !blockBackward
&& !isMove
&& !isXSTrap
&& !isVset
```

设计选择说明：

- `numUops == 1`、`firstUop && lastUop` 排除 fusion、ROB compression 和 multi-uop entry；当前 ROB 只有 entry-level `resolved`，不能表达 entry 内每条指令的独立安全状态。
- `numWB == 1` 排除 eliminated move、零 writeback 和需要多个 completion 的 entry。
- 特殊控制位即使理论上与某个 FuType 组合很少出现，也必须显式排除，避免未来 decoder 扩展无意扩大功能范围。
- eligibility 在 enqueue 锁存，writeback 时不依赖 debug-only `debug_fuType`。

### 4. ROB metadata 与身份

在 `IntERRobUopMeta` 中增加最小、可综合且与 debug 配置无关的状态：

| 字段 | 建议宽度 | 用途 |
| --- | ---: | --- |
| `writebackResolveEligible` | 1 bit | 功能白名单结果。 |
| `writebackResolveRobFlag` | 1 bit | 与 entry index 一起重建完整 `RobPtr`，拒绝 ROB wrap 后的 stale writeback。 |
| `writebackResolveClass` | 2 bits | `alu/mul/div/other`，用于 resolution counter 归因。 |
| `resolvedByWriteback` | 1 bit | 区分 commit-resolved 与 WB-resolved，支持断言、窗口计数和 debug。 |

`connectEnq` 对新 entry 明确初始化：

```plain
resolved = false
resolvedByWriteback = false
writebackResolveEligible = computed eligibility
writebackResolveRobFlag = robEnq.robIdx.flag
writebackResolveClass = enqueue FU class
```

ROB slot 重用必须由 enqueue 初始化覆盖旧状态；任何同周期 metadata 更新都要显式以 `enqHit` 为高优先级屏蔽，不能让旧 owner 的 writeback 或 commit pulse 污染新 owner。

### 5. Final accepted writeback 判定

writeback-resolved 只能来自 CtrlBlock 已做 older-redirect 过滤的 writeback completion，不使用未经 kill 过滤的 raw writeback valid 作为安全证明。

对 ROB entry `e`，定义：

```plain
fullRobIdx(e) = {e.writebackResolveRobFlag, entryIndex}

acceptedWbMatch =
    writebackNums(port).valid
 && exuWriteback(port).robIdx == fullRobIdx(e)

acceptedWbCount = selected writebackNums.bits

finalAcceptedWriteback =
    acceptedWbCount != 0
 && e.uopNum == acceptedWbCount
```

最终事件为：

```plain
writebackResolvedNow(e) =
    e.valid
 && e.writebackResolveEligible
 && !e.resolved
 && finalAcceptedWriteback
 && !e.needFlush
 && !sameCycleNeedFlushWriteback
 && !fullRobIdx(e).needFlush(io.redirect)
 && !io.flushOut.valid
 && ROB state == idle
 && !sameCycleEnqueueReuse
```

其中 `sameCycleNeedFlushWriteback` 必须覆盖 writeback exception、`flushPipe`、replay 和 debug trigger。若 raw writeback 与 older redirect 同周期到达，redirect-filtered `writebackNums.valid` 或完整 `RobPtr` kill 检查必须阻止 resolve。

第一版不增加 writeback 到 ST 的同周期组合旁路：

```plain
周期 N：ROB 接受最终 writeback，周期末 resolved := true
周期 N+1：ST 观察寄存后的 resolved 并尝试跨越
```

这会损失至多一个周期的理论窗口，但显著简化优先级、时序和验证。

### 6. Resolved 状态更新优先级

所有对 `entry.intER.resolved` 和 `resolvedByWriteback` 的写入应集中或通过单一 next-state helper 合并，避免多个分散 `when` 的文本顺序成为隐含优先级：

```plain
1. 新 entry enqueue / slot reuse：resolved=false, resolvedByWriteback=false
2. killed / invalid / needFlush：不得产生 writeback-resolved event
3. writebackResolvedNow：resolved=true, resolvedByWriteback=true
4. commitSafeNow：resolved=true；若此前不是 WB-resolved，则保留 resolvedByWriteback=false
```

`safeToCross` 继续使用寄存的 `resolved || commitSafeNow`。commit path 不改变，并作为所有不支持类型以及所有被 eligibility 拒绝场景的保守 fallback。

### 7. 程序序恢复证明

writeback-resolved 安全性依赖以下 ownership chain：

```plain
older ROB entries
  -> ST 严格按程序序检查 safeToCross
  -> 当前普通整数 entry 的最终、无异常 WB
  -> guard decrement
  -> UCA 在 producerReady + usersDone + guardDone 后 early-free
```

证明要点：

1. 更老 Branch 未 actual commit 时仍 unresolved，ST 不能越过，因此它未来的 redirect 不会冲刷已经放行的年轻 redefiner。
2. 更老 Load/Store/AMO/CSR/Fence/VSet/FP/Vector 或 multi-uop entry 仍保持保守边界；其 exception、replay、memory violation、flush 或副作用风险未关闭时会阻挡 ST。
3. 当前白名单 entry 的所有同步 exception/trigger/replay/flush 信息必须与最终 WB 同周期到达并被 `sameCycleNeedFlushWriteback` 排除；白名单中不得存在 WB 后才由另一路径产生的新同步 redirect。
4. 更年轻 instruction 的 redirect 只能冲刷自己及更年轻 entry，不能冲刷更老、已经 WB-resolved 的普通整数 entry。
5. ROB slot 和 WB 必须匹配完整 generation flag，防止 wrap/reuse 后 stale completion 修改新 owner。

### 8. 异步中断 drain

仅靠“当前没有 pending interrupt”不足以保护提前发出的 guard：异步中断可以在未来到达，并从 ROB head 冲刷尚未 commit 的年轻 redefiner。当前已有硬断言要求 guard-emitted redefiner 不得被 redirect flush，因此 writeback-resolved 必须同时解决该窗口。

最小侵入方案是在 ROB 计算：

```plain
hasOutstandingIntERGuard =
  any entry:
       entry.valid
    && entry.intER.redef.valid
    && entry.intER.guardEmitted
```

实际接收中断的条件增加：

```plain
intrEnable = existingIntrEnable && !hasOutstandingIntERGuard
```

行为为：

- `intrBitSetReg` 一旦 pending，ST 仍立即停止，不再发出新 guard；
- 如果已有 guard-emitted redefiner 尚未提交，ROB 暂不生成 interrupt flush，normal commit 继续按程序序 drain；
- 当前 `allowOnlyOneCommit` 在 interrupt pending 时仍可限制为每周期一条，保证行为保守；
- 最后一个 outstanding guard 对应 entry commit 并失效后，下一周期允许中断；
- 继续保留“redirect 不得 flush guard-emitted redefiner”断言，作为程序序证明或 interrupt drain 漏洞的 fail-fast 边界。

该方案可能增加少量 interrupt latency，但只在已经发出不可撤销 guard 的短 drain 窗口内发生。不要仅检查“已经 early-free”的 UCA entry，因为 ROB 在 guard 发出周期不能假设 UCA 最终是否立即释放；在没有精确 ack 接口时，`guardEmitted` 是正确的保守边界。

### 9. 性能计数器

新增计数器应形成可校验闭包，并能区分机会、成功和安全阻塞：

| Counter | 口径 |
| --- | --- |
| `int_er_rob_wb_resolve_eligible_enq` | enqueue 时满足静态 eligibility 的 entry 数。 |
| `int_er_rob_wb_resolve_final_candidate` | 观察到完整身份匹配、最终 accepted WB 的 eligible entry 数。 |
| `int_er_rob_resolved_by_writeback` | 实际从 unresolved 转为 WB-resolved 的 entry 数。 |
| `int_er_rob_resolved_by_writeback_alu` | ALU 成功数。 |
| `int_er_rob_resolved_by_writeback_mul` | MUL 成功数。 |
| `int_er_rob_resolved_by_writeback_div` | DIV 成功数。 |
| `int_er_rob_resolved_by_writeback_other` | `bku/i2f/i2v` 成功数。 |
| `int_er_rob_wb_resolve_blocked_need_flush` | final candidate 因已有或同周期 exception/replay/trigger/flush 被拒绝。 |
| `int_er_rob_wb_resolve_blocked_redirect_recovery` | 因 redirect、flushOut 或 ROB walk/recovery 被拒绝。 |
| `int_er_rob_wb_resolve_rejected_identity_reuse_raw` | raw completion 只命中 entry index、但完整 ROB identity 或 slot owner 不匹配的诊断事件；它不是 accepted final candidate，不参与 outcome 闭包，正常应接近 0，并配套断言。 |
| `int_er_rob_wb_resolved_entry_cycle` | live `resolvedByWriteback` entry 的累计 entry-cycle，衡量提前窗口。 |
| `int_er_rob_interrupt_deferred_for_guard_cycle` | pending interrupt 因 outstanding guard 被延迟的周期。 |
| `int_er_rob_interrupt_deferred_for_guard_episode` | 延迟窗口开始次数。 |
| `int_er_rob_outstanding_guard_sum` | outstanding guard 数量的周期累计。 |

必须满足的闭包：

```plain
resolved_by_writeback
  == alu + mul + div + other

final_candidate
  == resolved_by_writeback
   + blocked_need_flush
   + blocked_redirect_recovery
```

若某个 accepted candidate 同时命中多个阻塞原因，按固定优先级只归入一个 primary reason，另可保留 raw overlap counter，但 raw counter 和 identity/reuse rejection 都不参与闭包。

现有 ST `class x reason` 计数继续保留。优化后重点比较：

- `other_integer / not_resolved`；
- `other_integer / not_writebacked`；
- ST pending-work 和 valid-frontier blocker；
- guard decrement、early-free、commit suppress；
- integer free-list stall 和 average free register；
- interrupt deferred 周期与 episode。

## Acceptance Criteria

- AC-1: 增加可独立控制的配置参数，并在目标配置开启。
  - Positive Tests（预期 PASS）：
    - `IntEarlyReleaseParams()` elaboration 保持 writeback-resolve 关闭。
    - `WithIntEarlyReleaseFunctional` 显式打开 writeback-resolve。
    - `DefaultConfig` 同时满足 `enable=true`、`observeOnly=false`、writeback-resolve=true。
    - `IntERDisabledConfig` 不生成相关 ROB metadata/logic。
  - Negative Tests（预期禁用或失败）：
    - 不能依赖 debug-only FuType 字段实现功能判断。
    - accessor 缺失或 functional config 未显式开启时，配置测试必须失败。

- AC-2: 功能白名单与 `other_integer` 性能分类严格分离。
  - Positive Tests（预期 PASS）：
    - `alu/mul/div/bku/i2f/i2v` 在所有结构和控制条件满足时 eligible。
  - Negative Tests（预期 not eligible）：
    - `brh/jmp/ldu/stu/mou/csr/fence` 不 eligible。
    - `vsetiwi/vsetiwf/vsetfwf` 不 eligible，即使性能分类为 `other_integer`。
    - FP、Vector arithmetic 和未知 FuType 不 eligible。

- AC-3: 特殊和非单 entry 情况保持 commit-resolved。
  - Positive Tests（预期 PASS）：
    - single-uop、single-entry、`numWB==1` 普通整数 entry 可以进入 WB resolve path。
  - Negative Tests（预期 not eligible）：
    - compressed、fusion、multi-uop、`numWB==0`、`numWB>1` 不 eligible。
    - exception、single-step、debug trigger、`flushPipe`、replay、`waitForward`、`blockBackward`、move、XSTrap、VSet 任一条件存在时不 eligible。

- AC-4: 只有最终、redirect-filtered、完整身份匹配的 writeback 可以置 resolved。
  - Positive Tests（预期 PASS）：
    - ALU、MUL、DIV、other 各至少一个 probe 在 final accepted WB 后一周期观察到 `resolved=true` 和 `resolvedByWriteback=true`。
    - 非最终 completion 不置 resolved。
    - 同周期多个 WB count 的 final 判定与 `uopNum` 一致。
  - Negative Tests（预期不置 resolved）：
    - `writebackNums.valid=false` 的 killed raw WB。
    - ROB flag 不同但 entry index 相同的 stale WB。
    - entry invalid 或同周期 slot reuse。
    - 已有 `needFlush` 或同周期 exception/replay/trigger/flush。
    - redirect 确实冲刷该 `RobPtr`、`flushOut` 或 recovery walk。

- AC-5: ST 仍严格按程序序，且不新增同周期长旁路。
  - Positive Tests（预期 PASS）：
    - final WB 周期不直接发 guard；下一周期才能使用寄存的 resolved。
    - 一串 eligible ALU/MUL/DIV completion 后，ST 可以按 `stWalkWidth` 连续跨越。
    - 当前 entry 的 guard 仍携带完整 `trackId + trackGen + oldPdest + redefinerRobIdx`。
  - Negative Tests（预期停止）：
    - 更老 unresolved Branch、Load/Store、CSR、Fence、VSet 或 multi-uop entry 阻止年轻 WB-resolved entry 的 guard。
    - `needFlush` entry 不能被 ST 跨越。

- AC-6: 异步中断不能冲刷 guard-emitted redefiner。
  - Positive Tests（预期 PASS）：
    - pending interrupt 且无 outstanding guard 时，原中断接收行为不变。
    - pending interrupt 且存在 outstanding guard 时，`intrEnable=false`，normal commit 可以继续 drain。
    - 最后一个 outstanding guard entry commit 后，中断在后续周期被接收。
  - Negative Tests（预期 assertion 或禁止）：
    - `intrEnable && hasOutstandingIntERGuard` 永远不能成立。
    - pending interrupt 不能导致 guard-emitted redefiner redirect-flush assertion。

- AC-7: 断言和 counter 闭包完整。
  - Positive Tests（预期 PASS）：
    - writeback resolution 类型闭包成立。
    - accepted final candidate outcome 闭包成立，identity/reuse raw rejection 独立统计。
    - `resolvedByWriteback` 只能由 eligible final accepted WB 产生。
    - 原 guard-emitted redirect fail-fast 断言继续存在并通过正反测试。
  - Negative Tests（预期 assertion）：
    - stale ROB generation 置 resolved。
    - needFlush 与 writeback-resolved 同 entry 同周期同时成立。
    - guard outstanding 时实际接收 interrupt。

- AC-8: 定向 Chisel 测试和受影响回归通过。
  - Positive Tests（预期 PASS）：
    - 扩展 `IntEarlyReleaseRobTest`，覆盖 eligibility、final WB、identity、exception/redirect/reuse、ST 下一周期放行和 interrupt drain。
    - 扩展 `IntEarlyReleaseBundlesTest`，覆盖参数、metadata shape 和 Default/Disabled config。
    - 运行完整 IntER 受影响 suite：`IntSparseUCATest`、`IntEarlyReleaseBundlesTest`、`IntEarlyReleaseFreeListTest`、`IntEarlyReleaseDataPathTest`、`IntEarlyReleaseRobTest`。
    - 运行 `difftest.PreprocessTest`，防止 direct integer shadow 路径回归。
  - Negative Tests（预期不能接受）：
    - 只通过 source-string 测试而没有行为 probe。
    - 单个 focused case 通过但 suite-level 复跑失败。

- AC-9: `DefaultConfig` clean build 与完整 emu-basic 通过。
  - Positive Tests（预期 PASS）：
    - 构建前运行 `scripts/xiangshan.py --clean`，防止复用旧 config emulator。
    - 使用 `DefaultConfig`、Verilator 和 `--trace-fst` 构建。
    - 检查 `build/generated-src/difftest_profile.json` 确认实际生成配置为 `DefaultConfig` 且 writeback-resolve 开启。
    - 完整运行 12 个 workload：`cputest`、`riscv-tests`、`misc-tests`、`rvh-tests`、`microbench`、`coremark`、`linux-hello-opensbi`、`iopmp-test`、`povray`、`copy_and_run`、`f16_test`、`zcb-test`。
    - 除既有 `povray --max-instr` 特例外，每个测试最终出现 `HIT GOOD TRAP` 才判 PASS。
    - 日志中间出现 self-check `failed`，但最终 `HIT GOOD TRAP` 时仍判 PASS。
  - Negative Tests（预期进入 debug 流程）：
    - 无 `HIT GOOD TRAP`、非零退出、assertion、Difftest mismatch、hang 或 waveform 中协议异常。

- AC-10: 任何失败都按历史驱动的 debug 协议闭环，直至 emu-basic 全通过。
  - Positive Tests（预期 PASS）：
    - 本任务固定使用 `taskTag = other-integer-wb-resolved`。
    - 每一轮分析或修改前重新读取 `mydebug/new-er/README.md`，并读取 `mydebug/new-er/records/` 下匹配本任务 tag 的全部已有记录。
    - 新记录列出本任务全部历史；若本任务尚无记录，明确写 `Prior records for this task: none`。
    - 默认不批量读取其它任务记录；只有本任务已有记录显式引用、且新记录写明与当前症状的直接相关性时，才允许读取对应跨任务记录。
    - 失败 artifact 保存 exact command、配置、commit、stdout、stderr、counter、`.fst/.vcd` 波形和首个坏 cycle/PC/ROB/preg。
    - 每轮记录 waveform 证据、复用的历史结论、被否定假设、当前 hypothesis、root cause、fix、validation 和 next action。
    - 运行 `mydebug/new-er/check_protocol.py` 检查协议文档，并检查新记录字段完整性。
  - Negative Tests（预期不允许继续盲改）：
    - 未完整读取本任务历史就开始下一轮 debug。
    - 无差别读取 `mydebug/new-er/records/*.md`，把其它任务的假设或结论自动带入本任务。
    - 失败时只保留终端摘要、不保留日志和可用波形。
    - 因 rvh 内部打印 `failed` 而把最终 hit-good-trap 错判为失败。

- AC-11: 形成设计与验证报告。
  - Positive Tests（预期 PASS）：
    - 更新当前方案文档中“所有类型只在 commit resolved”的描述。
    - 新报告记录代码版本、配置、12 项结果、新 counter 闭包、优化前后 ST blocker、interrupt defer、IPC/cycle/free-list 指标。
    - 若经历失败，报告链接对应 `mydebug/new-er/records/` 和 artifact。
  - Negative Tests（预期不接受）：
    - 把性能趋势当作 correctness 证据。
    - 只报告聚合 IPC，不报告 writeback-resolve event 和 ST blocker 变化。

## Path Boundaries

### Upper Bound (Maximum Scope)

本任务最多允许：

- 在 `IntEarlyReleaseParams`、`HasXSParameter` 和 functional config 中增加独立参数；
- 扩展 ROB IntER metadata、enqueue eligibility helper、writeback final-event helper、ST/interrupt protection和性能计数器；
- 扩展 IntER ROB/Bundle Chisel tests 和受影响 regression；
- 必要时小范围调整 CtrlBlock 提供的 redirect-filtered writeback信息，但优先复用现有 `writebackNums`/`writebackNeedFlush`；
- 生成 `mydocs/new-er/` 下的结果报告，以及失败时 `mydebug/new-er/` 下的记录和 artifact。

### Lower Bound (Minimum Scope)

最小可接受实现必须：

- 只对白名单 single-entry 指令在 final accepted WB 后置 resolved；
- 排除 VSet、特殊控制、exception/replay/redirect 和 ROB identity mismatch；
- 保持 ST 程序序及原 commit fallback；
- 解决异步中断对 guard-emitted redefiner 的冲刷风险；
- 提供行为级定向测试和闭包计数器；
- clean 构建并通过完整 DefaultConfig emu-basic；
- 失败时逐轮执行历史优先 debug 协议并保存波形。

### Allowed Choices

- 可以新增小型 `RobIntEROps` helper/probe，避免在 ROB 主循环复制 eligibility 和 final-WB 规则。
- 可以用 1 个注册周期换取清晰的 WB-to-resolved 边界，不要求同周期 ST bypass。
- 可以先跑单 case、完整 unit suite、smoke，再跑 12 项 matrix；最终验收仍是完整 emu-basic。
- 可以复用 `mydocs/new-er/task34/run_emu_basic_matrix.py`，但 artifact root 必须指向本任务的新目录。
- 不能直接用 `IntERInstClass.otherInteger` 作功能白名单。
- 不能提前 resolve Branch、Load/Store、AMO、CSR、Fence、VSet、FP/Vector 或 compressed/multi-uop entry。
- 不能移除或弱化 guard-emitted redefiner redirect assertion。
- 不能通过关闭 Difftest、关闭 functional early-free 或切换 observe-only 来宣称验收通过。
- 不能修改 UCA identity、FreeList suppress 或 direct Difftest shadow 协议，除非波形和历史记录证明是完成本任务所必需。
- 不能把 `mydocs/`、`mydebug/` 的忽略状态当成不生成报告或不保存失败 artifact 的理由。

## Dependencies and Sequence

### Milestone 0: 固化基线与历史

1. 记录 git HEAD、dirty status、Difftest HEAD 和 DefaultConfig IntER 参数。
2. 固定本任务 tag 为 `other-integer-wb-resolved`，读取 `mydebug/new-er/README.md` 和该 tag 下的全部已有记录，建立 task-scoped history manifest；若不存在则记录 `none`。
3. 保存 task34 的基线 counter：尤其是 422,617 个 `other_integer/not_resolved` 周期。
4. 明确旧结果只作 baseline；新实现必须重新 clean build 和完整运行。

### Milestone 1: 先写 eligibility 和 final-WB 行为测试

1. 在 `IntEarlyReleaseRobTest` 增加 eligibility probe，覆盖完整正负白名单。
2. 增加 final accepted WB probe，覆盖 full `RobPtr`、计数、exception/redirect/reuse 优先级。
3. 在 `IntEarlyReleaseBundlesTest` 增加参数和 metadata shape 测试。
4. 先确认新增测试在旧实现上按预期失败，再开始 RTL 修改。

### Milestone 2: 参数和 metadata

1. 修改 `src/main/scala/xiangshan/Parameters.scala`。
2. 修改 `src/main/scala/top/Configs.scala`，只在 functional mixin 显式开启。
3. 修改 `src/main/scala/xiangshan/backend/IntEarlyReleaseBundles.scala`，加入 eligibility、ROB flag、resolve class 和 source bit。
4. 修改 `RobBundles.connectEnq`，集中计算、锁存和初始化 metadata。

### Milestone 3: ROB writeback-resolved

1. 在 `RobIntEROps` 增加功能白名单与 final-WB helper。
2. 在 ROB writeback accounting 附近复用 redirect-filtered completion，生成每个 entry 的 `writebackResolvedNow`。
3. 合并 resolved 状态更新优先级，显式屏蔽 enqueue reuse、needFlush、redirect、flushOut 和 walk。
4. 保持 ST 下一周期观察寄存的 resolved，不增加组合旁路。
5. 加入类型、outcome 和窗口 counter 及闭包断言。

### Milestone 4: Interrupt drain 与恢复断言

1. 计算 `hasOutstandingIntERGuard`。
2. 用它门控实际 `intrEnable`，但 raw pending interrupt 继续停止 ST。
3. 添加 deferred cycle/episode/outstanding sum counter。
4. 添加 interrupt drain probe 和断言。
5. 保留并复跑 guard-emitted redirect 正反测试。

### Milestone 5: 定向验证与编译

依次运行：

```bash
mill -i xiangshan.test.testOnly xiangshan.backend.IntEarlyReleaseRobTest
mill -i xiangshan.test.testOnly xiangshan.backend.IntEarlyReleaseBundlesTest
mill -i xiangshan.test.testOnly \
  xiangshan.backend.IntSparseUCATest \
  xiangshan.backend.IntEarlyReleaseBundlesTest \
  xiangshan.backend.IntEarlyReleaseFreeListTest \
  xiangshan.backend.IntEarlyReleaseDataPathTest \
  xiangshan.backend.IntEarlyReleaseRobTest
mill -i difftest.test.testOnly difftest.PreprocessTest
```

任何 suite failure 都先判断是实现错误、test harness 目录冲突还是环境问题；若进入系统级 debug 或需要波形定位，先重新执行历史读取协议。

### Milestone 6: DefaultConfig clean build 和完整 emu-basic

1. 新建本任务 artifact root。
2. 执行 clean，随后以 `DefaultConfig --trace-fst` 构建。
3. 检查 `difftest_profile.json` 和生成参数，确认没有 config cache 污染。
4. 使用固定 seed 运行 task34 runner 的 12 项 workload。
5. 按最终 `HIT GOOD TRAP` 判定，忽略最终 good trap 之前正常出现的 self-check `failed`。
6. 解析并校验新增闭包及既有 IntER counter 闭包。

### Milestone 7: 失败 debug 循环

对每一个失败、每一轮修复严格重复：

```plain
读取 README + 本任务 tag 的全部历史 records
  -> 创建 timestamped record/artifact 目录
  -> 固化 command/config/log/wave/counter
  -> 定位首个错误而非最后一个 Difftest 扩散点
  -> 结合历史提出 hypothesis
  -> 检查波形并记录支持/反对证据
  -> 最小修复
  -> focused test
  -> 原失败 workload 同 seed 重跑
  -> 完整受影响 suite
  -> 完整 emu-basic
```

失败目录格式：

```plain
mydebug/new-er/artifacts/YYYYMMDD-HHMMSS-other-integer-wb-resolved-<test>/
mydebug/new-er/records/YYYYMMDD-HHMMSS-other-integer-wb-resolved-<test>.md
```

### Milestone 8: 报告和文档收口

1. 更新 `mydocs/new-er/int-er-key-design.md` 的 resolved 分类表和恢复说明。
2. 新建本任务 emu-basic/counter 报告，记录每个 workload 的最终 trap、cycle、IPC 和 counter。
3. 对比 task34 基线，分别报告“直接消除的 not_resolved 机会”和“不受影响的 not_writebacked 部分”。
4. 报告 interrupt defer 成本，不能只报告 ST 收益。
5. 检查源码 diff 只包含任务范围，文档和 debug artifact 路径完整。

## Feature Map / Capability Map

```plain
Decode/Rename EnqRobUop
  |  FuType + single-entry + special-control checks
  v
ROB enqueue metadata
  |  eligible + robFlag + resolveClass
  v
redirect-filtered final writeback ---- same-cycle exception/replay/trigger
  |                                  |
  | full RobPtr match                +----> reject / keep commit-resolved
  v
entry.resolvedByWriteback
  |  registered boundary
  v
ordered ST safeToCross
  |  blocked by every older unresolved entry
  v
guard decrement
  |                         pending interrupt
  |                               |
  v                               v
UCA early-free            outstanding guard drain
  |                               |
  v                               v
commit suppress           normal commit -> interrupt accept
```

## Implementation Notes

- 源码中不要出现 `AC-*` 或 `Milestone` 等 plan 术语；这些只属于实施文档。
- 复用 CtrlBlock 已有 redirect-filtered writeback 语义，不能重新从 raw WB 猜测 kill 状态。
- 匹配 ROB completion 时使用完整 `RobPtr`，不能只比较 `.value`。
- `writebackResolveEligible` 是功能位，`instClass` 是性能归因，两者不可互换。
- 第一版选择 next-cycle ST visibility；若后续计数证明 1-cycle gap 显著，再单独设计旁路和时序验证。
- `i2f/i2v` 虽由 integer scheduler 发起，目的寄存器和提交状态不同；必须保留各自原有 RF、fflags/dirty 和 recovery 路径，只前移 ROB entry 的 ST 安全边界。
- `div` 的 DataPath “uncertain read path” fallback 与 ROB 最终 writeback-resolved 是不同问题；本任务只在 divider 最终 completion 被 ROB 接受后 resolve。
- interrupt defer 是正确性机制，不是性能优化；任何删除它的方案必须先提供可恢复已 early-free oldPdest 的新协议。
- 每轮只自动继承 `other-integer-wb-resolved` 任务历史中已经确认的证据和被否定假设；其它任务记录不得自动进入本任务 hypothesis，必须由当前任务记录显式引用并说明关联性。
- 最终报告必须同时说明收益、未覆盖部分和新增 interrupt latency，避免只展示有利指标。
