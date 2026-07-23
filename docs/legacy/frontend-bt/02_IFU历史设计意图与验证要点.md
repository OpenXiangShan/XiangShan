# IFU 历史设计意图与验证要点（归档摘要）

> **资料性质**：历史归档摘要，不是 node031 新环境的最新 RTL/spec 事实源。本文压缩整理旧 `uc_agent_workspace` 中的设计说明、V3-refresh spec 与验证分析，保留“为什么这样设计、曾经哪里出过问题、复核时应看什么”。
>
> **时间边界**：主要材料来自 V3 设计草稿、2026-03-03 左右的 V3 测试点、2026-03-10～13 的风险分析，以及 2026-03-25 的 `V3-refresh` 文档快照。代码、接口和覆盖口径可能已经变化；使用前必须对照 node031 当前 RTL、当前 coverage model 和当前测试环境。

## 0. 阅读约定

- **设计意图**：当时希望实现的架构行为；不能据此断言今天的 RTL 已实现。
- **关键边界**：历史资料反复出现、容易漏测或影响接口契约的条件。
- **验证建议**：可迁移成参考模型、断言或定向 testcase 的检查思路。
- **当前 RTL 复核点**：旧资料中已知的实现差异、FIXME 或疑似 bug；这里只列检查入口，不给出“现 RTL 一定有 bug”的结论。

## 1. 版本脉络与保留价值

V2 到 V3 的结构变化决定了历史资料为什么仍有追溯价值：

| 变化 | 历史意图/影响 | 使用时的注意事项 |
| --- | --- | --- |
| MMIO 状态机由约 11 状态收敛为 `Idle/WaitLastCommit/SendReq/WaitResp` | 简化控制、把跨页拼接上移 IFU；NC 可直接发起，MMIO 原则上等待最旧提交 | 快照 RTL 的 `WaitLastCommit` 仍有直通 FIXME，不能把目标等待行为当作现状 |
| PredChecker 增加 `JalrFault` | 对未被预测 taken 的非-ret JALR 及时 remask，修复 V2 隐蔽漏检；RET 由独立的 `RetFault` 处理 | V2 的 JalrFault 分析是 bug 证据，不应直接当作当前 RTL 结论 |
| PreDecoder 不再负责边界推导 | 边界逻辑独立到 `InstrBoundary`，预译码路径更短；RVC 扩展放到 S3 的 `RvcExpander` | 旧 `PreDecoder.md` 仍描述“双有效向量”，那是迁移前语义 |
| `InstrBoundary` 后半段两路并行假设 | 用面积换时序，最后由前半段结果选择一路 | 必须和串行 reference model 对照，尤其是半条 RVI |
| 新增 `InstrCompact` | 在进入 IBuffer 前把稀疏 raw slot 压成连续指令序号，减轻 IBuffer 选择压力 | `2-taken` 场景有明确历史 FIXME |
| 流水级命名由 F0–F3 规范为 S0–S2 + S3 + WB | 便于描述 FTQ/ICache/IBuffer 的时序关系 | 旧 testcase 可能仍用 F 级名称，迁移时先做映射 |
| Trigger 规范迁移 | 历史设计曾讨论 `timing/chain/action`；V3 调试规范移除可写 `timing` | 以当前生成 RTL 与 backend 契约为准，不能只看旧 Scala 注释 |

保留这些材料的主要理由是它们记录了真实问题输入：C.MOP.1～15 非法判定、MMIO `corrupt` 未上报、MMIO 推测执行、V2 `JalrFault` 漏检，以及跨块/跨页/flush 的风险边界。它们不应替代最新规格，但可作为回归用例的 provenance 和审查清单。

## 2. IFU 顶层

### 2.1 设计意图

IFU 位于 BPU/FTQ 与 ICache 之后，负责把一个或两个预测块整理成可供 IBuffer 消费的指令流，并将可早判的预测错误写回 FTQ。历史规划中的主路径为：

`S0` 接收 FTQ 请求、计算范围/PC 低位及 two-fetch 组织；`S1` 消费 ICache 返回，做 2B 边界推导、紧密排列并维护跨块 half-RVI；`S2` 按 IBuffer bank 规则对齐并做轻量预译码；`S3` 做 RVC 扩展、异常关联、PredChecker、trigger pre-mark 及必要的 uncache 处理；`WB` 写回预测检查结果并产生 redirect。MMIO/uncache 另有局部 FSM。

设计目标包括：RVC/RVI 混排、两个预测块合并、跨预测块半条 RVI、非 MMIO 与 MMIO/uncache 两条路径、RVC 非法指令标记、ICache 异常与指令绑定、IBuffer 入队减压，以及在时序允许时尽早修正预测范围。IFU 与 IBuffer 的历史约定是先输出连续有效指令，再按 `enqPtr % 4` 做最多 3 槽位的对齐，以避免 IBuffer 端大范围稀疏选择。

### 2.2 关键边界

- **two-fetch**：历史草稿以最多 64B 的合并宽度为约束；不同时处理含 uncache 数据的两个 fetch，且曾限制第一块末尾为 half-RVI 的组合。两个块拼接后，第二块的 2B 槽位、PC 低位、结束位置都要重新偏移。
- **half-RVI 状态**：`firstInstrIsHalfRvi`、`firstFetchBlockLastInstrIsHalfRvi`、`lastInstrIsHalfRvi` 决定跨预测块续接。redirect、空块、连续两个 half-RVI 以及跨 cacheline/page 时不能沿用旧状态。
- **uncache 64B/页边界**：对于 uncache 返回，跨 64B 对齐边界与跨页物理地址不连续是两类不同问题；前者可由 uncache 通路发起两次请求，后者需要 IFU 暂存并拼接 `crossPage` 数据。这不能泛化为普通 ICache cacheline 的处理规则。
- **推测性**：NC/非 MMIO 可以按系统契约推测，MMIO 取指原则上必须等更旧指令提交；第一条指令没有更旧依赖，可绕过等待。
- **冲刷优先级**：S2/S3 BPU override、checker redirect、backend redirect 可能同时到达；backend 重定向通常具有更高优先级，但具体比较/保留规则必须以当前接口实现为准。
- **异常与 backpressure**：ICache/uncache 异常必须绑定到真实指令的 PC/偏移；IBuffer `ready=0`、连续 valid hold、reset 与 flush 组合不能丢指令或重复入队。

### 2.3 验证建议

1. 建立 2B 槽位级 golden model，同时覆盖单 cacheline、双 cacheline/two-fetch、地址高位折返、末条指令贴边、RVC/RVI 混排和跨块 half-RVI。
2. 把 `instrValid → compact → IBuffer 对齐 → enq` 作为一条端到端链路检查；单独覆盖对齐偏移 0/1/2/3、空块和稀疏 raw slot。
3. 组合驱动 S2/S3 override、checker/backend redirect、接口反压、reset 和异常，检查各级 valid/状态/指针恢复。
4. 交错驱动 NC、MMIO、跨页 MMIO 及 ICache 异常，记录 MMIO 请求的 commit 依赖链；分开注入 `corrupt/denied`（异常）与 `incomplete`（跨页拼接状态）。
5. 长顺序程序、高误预测率程序和混合 MMIO 程序做稳定性回归，并将历史高风险点（JalrFault、TargetFault-only、2-taken、MMIO speculation）设为专项。

### 2.4 当前 RTL 复核点

- 重新确认 S0 合并 `instrRange/endPos` 的位宽和溢出保护，以及 `s1_prevLastIsHalfRvi` 在 `redirect` 与 `s0_fire` 同拍时的优先级。
- 确认 uncache 数据实际在哪一级产生；不能把依赖 S1/S2 指令数据的旧假设直接套到 S3 返回路径。
- 检查 valid hold、IBuffer 前置指针预测、flush 后状态清理和异常偏移优先级。
- 对照当前源码确认三个已知差异：`TargetFault` 是否只记 `perfFaultType`、MMIO oldest-commit 是否已启用、two-fetch/`instrEndOffset` FIXME 是否仍存在。

## 3. PreDecode

### 3.1 设计意图

迁移前的 `PreDecoder.md` 把 17×2B 输入切成 16 个 4B 窗口，并并行产生“首槽为新指令”和“首槽为上一条 RVI 后半段”两套有效向量；同时输出 RVC、CFI 类型（branch/jal/jalr/call/ret）和 direct 跳转偏移。V3 的拆分意图是把边界判断完全交给 `InstrBoundary`，PreDecode 只保留后级最小需要的 `valid/isRVC/brAttribute/jumpOffset`，保持组合逻辑、避免完整译码。

RVC 扩展和非法压缩指令处理由 S3 的 `RvcExpander` 完成：合法 C 指令转成 32 位 I 指令，非法 C 指令保留原始编码并带 `ill`，以便异常原因可追溯。

### 3.2 关键边界

- `jumpOffset` 只有在 branch/jal 场景才具有有效的 PC-relative 语义；其他槽位仍可能产生位值，不能把它当作 jalr 的真实目标。
- `valid/isRVC` 应透传上游边界和对齐结果；PreDecode 不重新推导指令起点，也不负责完整非法指令判断。
- 历史 V3 快照中 `BranchAttribute.decode` 使用整拍 `req.valid`，而 `instrValid` 是逐槽位信号；要覆盖“整拍有效但局部槽位无效”。
- C 扩展关闭/`fsIsOff`、非法 RVC、跨 cacheline RVC 及 C.MOP 扩展是异常边界。

### 3.3 验证建议

- 分别覆盖 RVC、RVI、混排，以及 direct branch/jal、indirect jalr、call、ret、非 CFI；检查属性与 PC-relative offset 的符号扩展/位宽。
- 对局部 `instrValid=0`、空块、整拍 `valid=0` 做透传检查；验证 `jumpOffset` 不参与 jalr target 比对。
- 在 `HasCExtension`、`fsIsOff` 和非法 C 指令条件下检查 `ill`、原始数据保持和异常传递。
- 回归历史 KMH22-515（C.MOP.1～15 不应误报非法），并确认当前扩展集合与 testcase 生成器一致。

### 3.4 当前 RTL 复核点

- 当前 PreDecode 是否仍只输出四类轻量字段并透传 `resp.instr` 原始指令，边界向量是否已完全移出；不要让旧双向量模型误报差异。
- 核对 `BranchAttribute.decode` 的 valid 输入粒度、RVC 扩展发生的流水级，以及非法 RVC 与 ICache 异常的合并优先级。
- 对照当前指令集配置确认 C.MOP、浮点压缩指令和 `fsIsOff` 行为，不以旧表格推断最新支持范围。

## 4. PredChecker

### 4.1 设计意图

PredChecker 的目的不是完整重新预测，而是利用 IFU 已知的指令属性，尽早发现能确定的方向/范围错误：jal、jalr、ret 未被预测 taken，非 CFI 被预测 taken，预测位置落在无效槽位。Stage1 选最早 fault、裁剪 `fixedTwoFetchRange` 并修正 taken；Stage2/WB 打拍输出 `checkerRedirect`、错误位置、目标、属性和性能故障类型。

历史错误类型从 V2 的五类扩展为 V3 的 `JalFault/JalrFault/RetFault/NotCfiFault/InvalidTaken/TargetFault`（另有 `NoFault`，共 7 个枚举值）。这是枚举层面的历史契约；当前实现实际可产生哪些 fault、`TargetFault` 是否仍只保留为枚举，必须按 active RTL 复核。设计折中是：范围/方向错误可触发 remask/redirect；direct target mismatch 可以记账，但不一定单独重定向，以避免拉长关键路径和重复纠正预测器。

### 4.2 关键边界

- remask 集合通常为 `JalFault/JalrFault/RetFault/NotCfiFault/InvalidTaken`；`TargetFault-only` 在历史快照中只应进入 `perfFaultType`，不应假定 `checkerRedirect.valid=1`。
- `mispredIdx` 使用优先级编码，多个 fault 时一般取最低槽位；`ignore` 会屏蔽对应槽位。位置 0、最后一槽、空块、跨第二 fetch block 都必须单独验证。
- `fixedRange` 与 `fixedTwoFetchTaken` 是 block 维度输出，不能只检查单条指令；`selectFetchBlock` 的边界会影响第一/第二块 taken 标记。
- Stage1 与 Stage2 有寄存器边界，scoreboard 要按拍建模；`endOffset` 在 half-RVI、invalidTaken、双 taken 等特殊场景只适合弱检查。

### 4.3 验证建议

1. 为六类 fault 各建最小 testcase，并覆盖 RVC/RVI、位置 0/末位、预测 taken/不 taken、`ignore` 和两块组合；另加 `NoFault` 基线。
2. 重点验证 target-only、target 与 remask 同时发生、多 fault 同位置/不同位置时的优先级，以及 redirect 的 target/misIdx/taken/isRVC/attribute 字段。
3. 用串行参考模型检查 `fixedRange` 的位操作，尤其 remaskIdx=0、全无效、全 fault 和跨块边界；记录 Stage1 中间向量便于定位。
4. 回归 V2 JalrFault 场景：未被预测的 jalr 位于预测 taken 的 branch/ret 之前，且 jalr 实际目标恰好等于预测块目标；V3 预期应在 IFU 侧 remask/redirect，而不能靠后端“巧合兜底”。

### 4.4 当前 RTL 复核点

- 复核 `fixedTarget` 是否把 jalr 排除在 jump target 之外（历史风险分析认为可能错误地使用顺序目标）；以当前 `PredChecker.scala` 与后端协议重新判定。
- 复核 `invalidTaken` 对 `fixedTwoFetchTaken` 的影响、第二块起始 remask、`stage1Fault` 与 `remaskFault` 集合是否一致。
- 复核地址比较位宽/高位截断、faultType 优先级和 Stage2 延迟；不要把旧文档的五类 fault 或“TargetFault 一定 redirect”带入新环境。

## 5. InstrBoundary

### 5.1 设计意图

以 2B 为最小粒度，`InstrBoundary` 根据 `maybeRvc`、`firstInstrIsHalfRvi` 和有效范围，生成“指令开始”`instrValid`、“指令结束”`instrEndVec` 及 `isRvc`。RISC-V 低两位可区分 16/32 位长度：RVI 的后半槽位不是新起点，RVC 每 2B 槽位结束。为缩短串行路径，历史实现先计算前半（如 0～15），再并行假设后半从“延续 RVI”或“新指令”开始，最后按前半实际末槽选择一路。

### 5.2 关键边界

- `instrValid`（起始）与 `instrEndVec`（结束）语义不同，不能用一个向量替代。
- `firstInstrIsHalfRvi=1` 表示输入第 0 槽是上一块 RVI 的后半；`firstFetchBlockLastInstrIsHalfRvi` 和 `lastInstrIsHalfRvi` 是 two-fetch 续接依据。
- `instrRange/endPos` 先裁剪有效区域；范围末尾、第一块末尾、空槽和跨预测块末尾可能出现半条 RVI。
- `maybeRvc` 是上游提示，不准确时整个边界链都会错；`HasCExtension` 是历史前提。

### 5.3 验证建议

- 用独立串行 2B reference model 覆盖纯 RVC、纯 RVI、RVC/RVI 混排、起始 half-RVI=0/1、第一块/总范围末尾 half-RVI。
- 重点覆盖前半最后槽位为 RVI 末尾、RVC 起点、无效范围末位、两块同时 half-RVI，以及 `maybeRvc` 与真实编码不一致的约束违例。
- 对并行合并结果同时检查 `instrValid/instrEndVec/isRvc` 和两个 half 状态，不只看最终指令数。

### 5.4 当前 RTL 复核点

- 核对后半选择条件（历史代码形态为 `boundary(mid-1) && !isRvc(mid-1)`）是否与当前边界定义一致，特别是中点槽位恰为 RVI 末尾或无效槽位时。
- 检查 `instrRange` 掩码、`endPos` 位宽、跨块拼接和 `maybeRvc` 来源；将并行结果与串行模型做随机等价比较。

## 6. InstrCompact

### 6.1 设计意图

`InstrCompact` 把 raw 2B slot 中稀疏的有效指令压成连续的第 0、1、2… 条，供取数和 IBuffer bank 对齐使用。`instrCountBeforeCurrent(i)` 给出槽位 i 之前的有效数；对每个紧凑索引用 one-hot 条件和 `Mux1H` 选出同源的 `instrIndex`、`instrIsRvc`、`selectBlock`、`instrPcLower`、`instrEndOffset`。字段必须绑定同一个 raw slot，不能只验证其中一列。

### 6.2 关键边界

- raw slot 可能有空洞，但 `instrCountBeforeCurrent` 必须与 `rawInstrValid` 一致；无命中/多命中是模型错误或边界输入。
- `selectBlock` 决定从第一/第二预测块取数据；`instrPcLower`、`instrEndOffset` 必须随块偏移和 RVC 长度同步。
- IBuffer 对齐最多偏移 3 槽，需覆盖 shift 0/1/2/3、块切换和空块。
- 历史代码明确标注 two-fetch + 双 taken 时 `instrEndOffset` 可能错误；跨块 RVI 截断/重复是重点风险。

### 6.3 验证建议

- 先用 raw-slot golden map 检查每个 compact index 的来源，再检查字段原子一致性；覆盖纯 RVC、纯 RVI、混排、稀疏有效和跨两块。
- 将 `instrEndOffset` 与 Boundary 的 end 向量交叉检查，普通场景严格比对，双 taken/half-RVI 场景记录风险而不做过强假设。
- 注入空块、重复 count 和索引超界，确认不会产生幽灵指令；将 `2-taken` 列为高优先级回归。`InstrCompact` 是纯组合、无 `ready` 端口的单元，IBuffer `ready` 反压应放在 IFU 集成测试中验证。

### 6.4 当前 RTL 复核点

- 检查 `Mux1H` 的候选范围裁剪、无命中默认值、count 位宽和 two-fetch 偏移；确认所有输出字段来自同一 raw slot。
- 重新定位历史 `2-taken` FIXME 是否已修复；若未修复，应在当前 coverage 中显式标成已知限制，而不是默认为功能通过。

## 7. InstrUncache（IfuUncacheUnit）

### 7.1 设计意图

历史架构把 uncache/MMIO 取指做成四状态 FSM：`Idle` 接收请求并锁存物理地址/PBMT/MMIO 属性；NC 直接到 `SendReq`，MMIO 到 `WaitLastCommit` 等待更旧指令提交；`SendReq` 等总线握手，`WaitResp` 等返回并向 IFU 给出数据、异常和 `crossPage`。`isFirstInstr` 表示没有更旧指令，可绕过 MMIO 等待。任意状态的 flush 都回 `Idle`，因为总线侧也同步冲刷。

对于 uncache 返回，跨 64B 对齐边界可由 uncache 通路发起两次请求；跨页时两个物理页可能不连续，通路返回 `crossPage`，由 IFU 暂存半条指令并在下一预测块拼接，或在预测跳转时发 redirect 重新取指。这是历史设计目标；同一时期的 `IFU-V3.md` 快照还记录过 uncache 完成后可能总发顺序 redirect，二者必须并列交给当前 RTL/owner 复核，不能把目标行为当作已实现行为。目标约束是不让 MMIO 推测执行，但保留 NC 的性能路径。

### 7.2 关键边界

- MMIO/NC 入口分流、`isFirstInstr` 旁路、`ifuStall` 对 `toUncache.valid` 的抑制，以及 `mmioCommitRead.mmioFtqPtr = ftqIdx - 1`。
- `fromUncache.fire` 与 `resp.valid` 在历史快照中相差一拍；scoreboard 不能按同拍响应建模。
- 在旧快照接口中，TileLink `corrupt/denied` 应映射到明确的 `ExceptionType`，而 `incomplete` 表示 `crossPage`/拼接状态而不是异常源；当前接口可能改用 `needResend` 等命名，必须按 active RTL 复核。跨页半条 RVI、MMIO/非 MMIO 交错和 redirect/flush 竞争仍需单列。
- MMIO commit 查询有请求/响应延迟，不能用上一请求或更早请求的 commit 结果放行当前请求。

### 7.3 验证建议

1. 逐状态覆盖 `Idle→WaitLastCommit→SendReq→WaitResp→Idle` 与 NC 快路径，覆盖首条指令、stall、请求/响应 backpressure。
2. 对 commit 信息注入 0～3 拍延迟、错序/交错块和前一块未提交场景，监控 MMIO 请求是否提前发出；同时验证 flush 在四个状态均能清空。
3. 分别注入 `corrupt/denied` 与 `incomplete`：前者检查 `ExceptionType`、PC/偏移和后续请求恢复，后者检查 `crossPage`、半条指令拼接和 redirect/flush 后的状态清理。
4. 回归历史 KMH22-3444（MMIO 推测执行）与 KMH22-2818（MMIO Bridge `corrupt` 未上报），必要时提升到 IT/全核观察真实副作用。

### 7.4 当前 RTL 复核点

- 历史 V3-refresh 快照的 `WaitLastCommit` 有 FIXME/直通，`mmioCommitRead` 可观察但未真正决定停留；确认当前版本是否已经接入 oldest-commit。
- 核对 `resp.valid` 延迟、`corrupt/denied` 映射、`crossPage`/`needResend` 锁存和 flush 清理，防止 stale data/exception 泄漏到下一请求；同时确认 active IFU 路径对跨页返回采用的 redirect/重取策略。
- 复核跨页拼接所在层级和 two-fetch 同时含 uncache 的限制；不能把旧设计图的“上层拼接”当作接口保证。

## 8. FrontendTrigger

### 8.1 设计意图

FrontendTrigger 是前端 pre-mark，不是最终调试异常仲裁器。历史实现本地保存 4 项 trigger 配置：`tUpdate.valid` 时更新指定 `tdata`，每拍镜像 enable；对送入 IBuffer 窗口的 PC 按 matchType 做等于/大于/小于比较，再结合 `select`、debug 模式、enable、chain 和 action 生成每条指令的动作预标记，最终由 backend pre-match/调试逻辑决定是否真正触发。

### 8.2 关键边界

- 当前快照的命中对象只有 `pc`；`pds`、`data/rawInst` 是预留输入，不应被当作命中条件。
- `tdata.select=1`、`debugMode=1` 或 trigger 未使能时，前端应屏蔽 pre-mark；配置写使能与整体 enable 是两条不同控制路径。
- chain 成功/失败、多 trigger 同时命中、动作归并和无命中默认优先级要单独观察。
- 旧设计说明仍提到 `timing`；V3 调试规范迁移记录称该位移除/不可写或生成 RTL 不带此位。必须以 node031 当前 backend 契约为准。

### 8.3 验证建议

- 覆盖配置写入/替换、四个 trigger 槽位、PC equal/greater/less、enable/select/debug 屏蔽、chain pass/fail 和多命中动作归并。
- 固定 PC 而随机改变 `pds/data`，确认结果不受预留输入影响；固定配置而改变 PC 窗口边界，检查每条输出与对应 PC 对齐。
- 把 `triggerHitVec → canFire → action → triggered` 作为三级观测，避免只看最终 pre-mark；系统级再单独验证 backend 最终 trap/进入 debug 的优先级。

### 8.4 当前 RTL 复核点

- 核对当前 trigger 工具是否仍使用 `timing`、chain 是否有隐式 timing 相等条件，以及生成 Verilog 与 Scala 字段是否一致。
- 检查 `tUpdate` 地址越界/同拍更新与 enable 镜像时序；确认无 trigger fire 时动作归并的默认项不会造成误报。
- 不要把 `triggered` 直接当作最终异常；必须检查 backend 消费接口和 debugMode 约束。

## 9. 历史高优先级复核清单

以下项目是旧资料中最值得迁移到新环境的“问题证据”，不是对当前 RTL 的定性：

| 主题 | 历史证据 | 最小复核问题 |
| --- | --- | --- |
| JalrFault | V2 漏掉未预测 jalr，特定目标巧合时后端也不 redirect；V3 将其加入 remask | 当前 `jalrFault` 是否进入 remask、fixedRange 和 redirect？ |
| MMIO speculation | KMH22-3444：commit 查询延迟错位可能提前发 MMIO | 请求发射是否严格依赖对应 FTQ entry 的提交，而非旧结果？ |
| MMIO corrupt | KMH22-2818：`corrupt` 路径未明确上报 | corrupt/denied 是否映射为正确 instruction access fault，且不污染下一请求？ |
| C.MOP/RVC | KMH22-515：Zcmop 支持后仍按非法 C 指令处理 | 当前 RVC decoder、`fsIsOff` 和异常路径是否与扩展配置一致？ |
| Boundary/Compact | half-RVI、并行合并、two-fetch + 2-taken 的 FIXME | 串行边界模型、compact 字段同源性和 endOffset 是否都被覆盖？ |
| Flush/指针 | 历史分析关注多级 flush、ValidHold、IBuffer 指针恢复 | redirect/flush/反压同时发生时是否无丢失、重复、死锁？ |

## 10. 来源清单

以下路径是整理前的本地来源，将在迁移后删除。完整原文由 node031 的
`archive/frontend-bt-legacy-source-20260723`（`da4e22041`）固定；这里保留路径只用于定位历史文件名。

### 10.1 设计/spec 快照

- `uc_agent_workspace/build-ifu/AI_bosc_IFU/unity_test/bosc_IFU_spec.md`
- `uc_agent_workspace/build-ifu/AI_bosc_IFU/unity_test/bosc_IFU_spec_PreDecode.md`
- `uc_agent_workspace/build-ifu/AI_bosc_IFU/unity_test/bosc_IFU_spec_PredChecker.md`
- `uc_agent_workspace/build-ifu/AI_bosc_IFU/unity_test/bosc_IFU_spec_InstrBoundary.md`
- `uc_agent_workspace/build-ifu/AI_bosc_IFU/unity_test/bosc_IFU_spec_InstrCompact.md`
- `uc_agent_workspace/build-ifu/AI_bosc_IFU/unity_test/bosc_IFU_spec_InstrUncache.md`
- `uc_agent_workspace/build-ifu/AI_bosc_IFU/unity_test/bosc_IFU_spec_FrontendTrigger.md`
- `uc_agent_workspace/build-ifu/AI_bosc_IFU/bosc_IFU/doc/IFU-V3.md`
- `uc_agent_workspace/build-ifu/AI_bosc_IFU/bosc_IFU/doc/PreDecoder.md`
- `uc_agent_workspace/build-ifu/AI_bosc_IFU/bosc_IFU/doc/IFU_V3测试点.md`

### 10.2 验证与历史问题

- `uc_agent_workspace/doc/verification/ifu/01_验证策略及方案/IFU_验证规划历史.md`
- `uc_agent_workspace/doc/verification/ifu/02_测试点分解/IFU_验证覆盖点清单_V3.md`
- `uc_agent_workspace/doc/verification/ifu/02_测试点分解/IFU_测试点_V3.md`
- `uc_agent_workspace/doc/verification/ifu/05_历史问题与风险分析/IFU_历史问题覆盖性分析_V2.md`
- `uc_agent_workspace/doc/verification/ifu/05_历史问题与风险分析/IFU_潜在风险分析_V3.md`
- `uc_agent_workspace/doc/verification/ifu/05_历史问题与风险分析/IFU_JalrFault_Bug分析_V2.md`
- `uc_agent_workspace/doc/verification/ifu/05_历史问题与风险分析/IFU_Bug_Scenarios_Analysis.md`

### 10.3 使用优先级

1. node031 当前 RTL、当前验证入口和当前 coverage model；
2. node031 中明确标注的历史归档；
3. 本文及其来源文件，仅用于设计意图、问题 provenance 和回归复核。
