# V2 Memory Trigger Flow

## 版本元数据

| 项目 | 内容 |
|---|---|
| RTL 版本 | V2 |
| 分支 | `mem_ut_uvm_v2` |
| 核验 commit | `bd813bc3ed5b39581be966c6518788852890ff6f` |
| 设计基线 | `2acbf327cf7fb514593acc00d4c41117ec499e08`，见 V2 `branch_policy.md` |
| 权威源码 | `src/main/scala/xiangshan`；DUT 生成基线见 V2 `memblock_rtl_profile.md` |
| 最后核验日期 | `2026-07-17` |

## Flow 范围

本文解释 memory trigger 配置从 CSR 进入 MemBlock，在 Load/Store 地址阶段匹配，形成 `DynInst.trigger`，再经写回和 ROB 产生 breakpoint exception 或 Debug Mode 的 flow。

## 核心结论

`trigger` 不是 Bool，不能用“整体置高”描述。它是 4-bit `TriggerAction`：

| 编码 | 动作 |
|---:|---|
| `0` | `BreakpointExp` |
| `1` | `DebugMode` |
| `2` | `TraceOn`（当前 memory trigger generator 尚未实现） |
| `3` | `TraceOff`（当前 memory trigger generator 尚未实现） |
| `4` | `TraceNotify`（当前 memory trigger generator 尚未实现） |
| `15` | `None` |

`FuConfig.trigger = true` 仅决定执行单元是否具有可选 trigger 输出字段。真正动作由 MemBlock 根据 CSR 下发配置和 Load/Store S1 虚拟地址动态生成。

`trigger` 的中性值是 `TriggerAction.None = 15`，不是 0。0 是
`BreakpointExp`，1 是 `DebugMode`。因此判断“是否有 trigger 动作”必须使用
`TriggerAction.isNone(trigger)` 或显式比较 4'hf，不能用 `trigger != 0`。

所以“`trigger` 被置高”需要按动作解释：当前 V2 memory path 只有
`DebugMode=1` 是数值为 1 的有效动作；`BreakpointExp=0` 虽然数值为 0，仍然是
有效 breakpoint 动作，并且必须同时置 `exceptionVec(breakPoint)`。当前
`TriggerUtil.triggerActionGen` 和 `BaseTrigger` 只实现 DebugMode、BreakpointExp、
None 三种输出，Trace 三种编码只是定义保留值。

## 写回端口中的 trigger 能力

V2 生成的 MemBlock split writeback 端口中，LDA0/1/2 和 STA0/1 保留
`uop.trigger[3:0]`，STD0/1 没有该字段。原因是：

- `LduCfg.trigger=true`、`StaCfg.trigger=true`，Load/Store 地址阶段需要做 memory trigger 匹配；
- `StdCfg.trigger=false`，STD 只搬运 store data，不重新做地址匹配，trigger 由 STA 地址阶段产生一次即可；
- `ExeUnitParams` 的 `trigger` 是 compile-time capability，最终顶层字段还会受具体 lane 的连接和常量传播影响，字段存在不等于运行时动作一定非 `None`。

STA0 还复用 StoreQueue 的 `mmioStout`/CBO 写回源。StoreQueue 在锁存 uncache uop 时将
`uncacheUop.trigger` 清为全 0；这不是标准的 `None=15` 编码，但由于 ROB 只用
`TriggerAction.isDmode(trigger)` 判定 Debug Mode，且 breakpoint 仍由
`exceptionVec(breakPoint)` 单独表示，所以该路径的 0 在没有 breakpoint bit 时行为上等价于
“无 trigger”。验证或适配代码不能只看 trigger 数值，必须结合来源和 breakpoint exception bit。

## 主流程图

```mermaid
flowchart TD
    A[CSR memTrigger 配置] --> B[MemBlock 分发到 Load/Store Unit]
    B --> C[MemTrigger 读取 vaddr、tdata、enable、debugMode]
    C --> D{地址与 load/store 类型匹配}
    D -->|否| E[TriggerAction.None = 15]
    D -->|是| F{chain/timing 可以 fire}
    F -->|否| E
    F -->|是| G{action}
    G -->|DebugMode| H[uop.trigger = 1]
    G -->|Breakpoint && canRaise| I[uop.trigger = 0 + exceptionVec.BP]
    G -->|Breakpoint 被屏蔽| E
    H --> J[MemExuOutput -> Backend -> ExceptionGen]
    I --> J
    J --> K[ROB head 精确异常/Debug entry + flush]
```

## 主流程文字伪代码

```text
1. CSR 模块把 memory trigger 的 tdata、enable、debugMode 和 triggerCanRaiseBpExp 分发到 MemBlock。
2. LoadUnit/StoreUnit 在 S1 使用当前访问虚拟地址调用 MemTrigger：
   Load 只检查 load-enabled trigger，并排除 prefetch；
   Store 只检查 store-enabled trigger；CBO 按 cache line 地址比较。
3. 每个 trigger entry 还必须满足 select==0、当前不在 debug mode、地址 match、chain/timing 合法。
4. 有 DebugMode action 可 fire 时输出 TriggerAction.DebugMode；
   否则有 Breakpoint action 且 triggerCanRaiseBpExp 时输出 BreakpointExp；
   否则输出 None。
5. Load/Store 把 action 写入 uop.trigger；BreakpointExp 同时置 exceptionVec(breakPoint)。
6. MemBlock writeback 把 uop.trigger 送入后端 ExceptionGen；
   ROB 把 breakpoint exception 或 DebugMode action 当作精确异常，在该指令成为最老指令时处理并清年轻流水。
```

## 1. CSR 配置与地址匹配

`MemTrigger.getTriggerHitVec()` 对每个 entry 检查：

- `tdata.select` 为 false。
- 当前不在 Debug Mode。
- Load prefetch 为 false。
- `tEnableVec(i)` 有效。
- Load 使用 `tdata.load`，Store 使用 `tdata.store`。
- `TriggerCmp(vaddr, tdata2, matchType, enable)` 命中。

Vector unit-stride 使用 high/low mask 匹配；CBO Store 使用 cache-line 地址粒度比较。命中后还要通过 `TriggerCheckCanFire` 的 chain/timing 约束。

动作优先级为 DebugMode 高于 BreakpointExp。BreakpointExp 还受 `triggerCanRaiseBpExp` 限制；该信号由当前 privilege/delegation/interrupt-enable 状态计算，防止特定中断关闭条件下抛 breakpoint exception。

## 2. Load/Store S1 生成动作

LoadUnit：

```scala
loadTrigger.io.fromLoadStore.vaddr := s1_vaddr
s1_out.uop.trigger := s1_trigger_action
s1_out.uop.exceptionVec(breakPoint) := TriggerAction.isExp(s1_trigger_action)
```

StoreUnit：

```scala
storeTrigger.io.fromLoadStore.vaddr := s1_in.vaddr
s1_out.uop.trigger := s1_trigger_action
s1_out.uop.exceptionVec(breakPoint) := TriggerAction.isExp(s1_trigger_action)
```

因此 memory trigger 是 MemBlock 根据执行期地址重新计算的，不是 Backend issue-to-MemBlock 直接复制 frontend trigger 的结果。Backend 构造 `MemExuInput.uop` 时先清零整个 uop，并未给 `uop.trigger` 单独赋值；Load/Store S1 随后覆盖为 memory trigger action。

## 3. 写回和 ROB 消费

MemBlock 写回连接执行：

```scala
sink.bits.trigger.foreach(_ := source.bits.uop.trigger)
```

ExceptionGen 捕获 action。ROB 判断异常时包含：

```scala
exceptionVec.asUInt.orR || singleStep || TriggerAction.isDmode(trigger)
```

所以：

- `BreakpointExp=0` 通过同步置位的 `exceptionVec(breakPoint)` 进入断点异常。
- `DebugMode=1` 即使不依赖普通 exceptionVec，也被 ROB 视为异常条件并进入 Debug Mode。
- `None=15` 不触发异常。

对 memory writeback，规范路径中 `BreakpointExp` 必须同时伴随
`exceptionVec(breakPoint)=1`；STA0 的 StoreQueue uncache/CBO 路径可能出现
`trigger=0` 且 breakpoint bit 为 0 的非规范默认值，此时不会触发 ROB 异常。

处理发生在 ROB 头，从而保持精确异常语义；MemBlock 地址一命中时不会直接异步清除全核流水。

## 状态、字段和优先级

| 字段/条件 | 生产者 | 有效条件 | 抑制条件 | 消费者 | 优先级 |
|---|---|---|---|---|---|
| `tEnableVec/tdataVec` | CSR Debug 模块 | CSR trigger 配置有效 | entry disabled | MemTrigger | 每 entry 独立 |
| trigger address hit | MemTrigger | 类型、地址、select、enable 匹配 | debugMode；Load prefetch | `TriggerCheckCanFire` | chain/timing 后生效 |
| `DebugMode` action | BaseTrigger | 可 fire entry action=DebugMode | 无匹配 | Load/Store uop | 高于 BreakpointExp |
| `BreakpointExp` action | BaseTrigger | 可 fire且 `triggerCanRaiseBpExp` | privilege/interrupt 条件屏蔽 | exceptionVec/ROB | 低于 DebugMode |
| `None` | BaseTrigger | 无可执行动作 | 被有效动作覆盖 | 后端 | 默认值 15 |
| Trace action (`2/3/4`) | `TriggerAction` 定义 | 编码存在 | 当前 generator TODO 未实现 | 当前 memory path 不应生成 | 保留编码 |
| StoreQueue uncache `trigger=0` | StoreQueue MMIO/CBO FSM | 锁存 pending SQ uop | 下一笔 uncache uop 覆盖 | STA0 writeback/ROB | 非规范中性值；无 breakpoint bit 时不产生异常 |

## 关联文档

- [memory flush pipe flow](memory_flush_pipe_flow.md)：trigger 异常与 `flushPipe` 在 ROB 的不同处理方式。
- [Int writeback agent 接口知识](../../../interface/v2/agents/int_writeback_agent.md)：LDA/STA/STD 顶层 trigger capability 和 lane 差异。
- [V2 RTL flow 索引](../index.md)。
- `AI_DOC/analysis/interface/v2/mem_ut_v2_agent_interface_signal_matrix_20260709.md`：V2 顶层 CSR trigger 和 writeback trigger 信号矩阵；该文件当前有用户未提交修改，本轮未编辑。

## V2/V3 差异

本文只核验 V2。V3 是否保持相同 action 编码、privilege gating 和 pipeline stage，需要在 V3 分支/profile 下独立追踪。

## 源码证据

- `src/main/scala/xiangshan/Bundle.scala:761-772`：`TriggerAction` 编码。
- `src/main/scala/xiangshan/backend/fu/FuConfig.scala:415-445`：LDU/STA capability 配置。
- `src/main/scala/xiangshan/backend/exu/ExeUnitParams.scala:65-76`：`trigger` capability 聚合和 `needExceptionGen` 条件。
- `src/main/scala/xiangshan/backend/fu/NewCSR/Debug.scala:222-273`：hit、chain/timing、action 生成和优先级。
- `src/main/scala/xiangshan/backend/fu/NewCSR/Debug.scala:286-315`：Load/Store/CBO 地址匹配条件。
- `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala:1181-1183`：`triggerCanRaiseBpExp` 条件。
- `src/main/scala/xiangshan/mem/MemBlock.scala:827,1039,1176,1266`：CSR trigger 分发到 memory units。
- `src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala:1132-1152`：Load S1 action 和 breakpoint exception。
- `src/main/scala/xiangshan/mem/pipeline/StoreUnit.scala:341-408`：Store S1 action 和 breakpoint exception。
- `src/main/scala/xiangshan/backend/Backend.scala:671-703`：MemBlock trigger 写回后端。
- `src/main/scala/xiangshan/backend/rob/Rob.scala:578-609`：ROB 精确异常判断。
- `src/main/scala/xiangshan/mem/MemBlock.scala:73-75,515-543`：LDA lane 复用 Atomics/Misaligned/Uncache，STA 写回连接。
- `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:841-849,1054-1060`：STA0 的 uncache/CBO 路径清零 trigger 并动态设置 flushPipe。
- `build_memblock/rtl/MemBlock.sv:831-950,30250-30510`：V2 split writeback 顶层字段保留差异。

## 知识修订记录

| 日期 | commit | 旧结论 | 新结论 | 修订原因 | 影响范围 |
|---|---|---|---|---|---|
| 2026-07-11 | `5b35ddd8d774b5f11d61333dfbe7638a3f362fad` | 首次建立，无同版本长期 flow 旧结论 | 建立 action 编码、地址匹配、写回和 ROB 消费关系 | 用户要求将本轮源码分析沉淀为 V2 知识 | V2 CSR/MemBlock/Load/Store/ROB |
| 2026-07-17 | `bd813bc3ed5b39581be966c6518788852890ff6f` | 只说明 trigger 编码和 Load/Store 生成，未区分 split writeback capability，也未说明 0 与 None 的边界 | 明确 LDA/STA 有 trigger、STD 无 trigger；`None=15`；补充 STA0 StoreQueue 非规范 0 值及必须联合 exceptionVec 判断的限制 | 结合 V2 Scala、生成 RTL 和 split writeback 端口追踪 plan 中 metadata guard | V2 MemBlock writeback/Trigger/ROB |
| 2026-07-17 | `bd813bc3ed5b39581be966c6518788852890ff6f` | 文档未明确“置高”不能用于判断 trigger 动作，且未区分 Trace 编码与当前实现 | 补充 0/1/2/3/4/15 全部编码，明确当前 memory generator 只产生 0/1/15，以及 DebugMode/Breakpoint 的数值语义 | 用户追问 `trigger` 什么场景下被置高 | V2 MemTrigger/LoadUnit/StoreUnit/ROB |

## 待确认项

- 本轮未核验 V3 对应实现。
