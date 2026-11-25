# 香山昆明湖处理器前端模块 Block Test 验证方案

适用于 `src/test/python/Frontend/` 下的 Python / Toffee 验证环境。
模块和场景矩阵用于组织验证范围，具体端口、参数及采样相位按当前源码和
所选 DUT manifest 核对。执行入口见 [Frontend README](../../README.md)。

## 一、前端功能概述

香山昆明湖处理器前端模块负责指令获取和预处理，采用解耦的前端架构设计。核心流程为：BPU进行分支预测生成取指请求，通过FTQ缓存预测信息和协调取指流程；IFU从ICache/ITLB获取指令并进行预译码、分支预测检查；最终通过IBuffer将有效指令分发到后端。前端支持多级流水线、跨页指令处理、MMIO取指、指令压缩扩展等功能，实现高性能的指令供给，与后端通过重定向信号维持正确的指令流执行路径。

---

## 二、测试场景分析

### 2.1 模块拆解与测试场景矩阵

#### 2.1.1 BPU (Branch Prediction Unit) 分支预测单元

**模块功能**：
- 实现多级分支预测，包含uBTB、aBTB、uTage、mBTB、Tage、ITTage、SC、RAS等预测器
- 管理全局历史寄存器GHR和路径历史寄存器PHR
- 生成预测块信息并传递给FTQ

**测试场景**：

| 场景编号 | 场景名称 | 场景描述 | 关键信号 |
|---------|---------|---------|---------|
| BPU-001 | 基本分支预测 | 顺序执行、无条件跳转、条件分支预测 | toFtq.pred.valid, pred.bits.taken |
| BPU-002 | 多预测器协同 | uBTB、Tage、SC等多预测器预测结果融合 | predBits.pred_source |
| BPU-003 | RAS返回地址栈 | 函数调用/返回的地址预测 | ras.io.out.target |
| BPU-004 | 历史寄存器管理 | GHR/PHR的更新和恢复 | ghr.io.update, phr.io.update |
| BPU-005 | BPU训练更新 | 后端重定向后的预测器训练 | fromFtq.commit, fromFtq.redirect |
| BPU-006 | 预测器使能控制 | CSR控制各预测器的开关 | io.ctrl.ubtbEnable等 |
| BPU-007 | 流水线冲刷 | BPU内部S1/S2/S3级的flush处理 | s1_flush, s2_flush, s3_flush |
| BPU-008 | 预测错误恢复 | 分支预测错误后的状态恢复 | redirect.valid, redirect.bits |

**接口交互**：
- 与FTQ：发送预测结果(BpuToFtqIO)、接收训练信息(FtqToBpuIO)
- 与后端：间接通过FTQ接收重定向信号
- 与CSR：接收控制信号(io.ctrl)

---

#### 2.1.2 FTQ (Fetch Target Queue) 取指目标队列

**模块功能**：
- 缓存BPU的预测信息，协调前端流水线
- 管理预测、取指、不可撤销和提交边界指针
- 处理IFU预测错误重定向和后端重定向
- 关联 resolve、entry commit 与保存的 BPU/RAS meta

**测试场景**：

| 场景编号 | 场景名称 | 场景描述 | 关键信号 |
|---------|---------|---------|---------|
| FTQ-001 | 预测块入队 | BPU预测结果写入FTQ | io.fromBpu.enq |
| FTQ-002 | 预测块出队 | IFU从FTQ获取取指请求 | io.toIfu.req |
| FTQ-003 | 指针管理 | 预测、取指和提交指针的移动与回绕 | bpuPtr, fetchPtr, pnrPtr, commitPtr |
| FTQ-004 | IFU重定向处理 | IFU预测错误导致的流水线冲刷 | io.fromIfu.wbRedirect |
| FTQ-005 | 后端重定向处理 | 后端分支解析导致的重定向 | io.fromBackend.redirect |
| FTQ-006 | 队列满/空 | FTQ满导致BPU暂停、空导致IFU暂停 | isFull, isEmpty |
| FTQ-007 | 分支提交 | 后端 FTQ entry 提交与 meta 回收 | 后端 commit、FTQ commitPtr 与 metaQueueCommit |
| FTQ-008 | 异常处理 | 指令缺页、访问异常的处理 | backendException |

**接口交互**：
- 与BPU：接收预测(BpuToFtqIO)、发送训练(FtqToBpuIO)
- 与IFU：发送取指请求(FtqToIfuIO)、接收写回(IfuToFtqIO)
- 与ICache：发送预取请求(FtqToICacheIO)
- 与后端：接收重定向/提交(CtrlToFtqIO)、发送性能信息(FtqToCtrlIO)

---

#### 2.1.3 IFU (Instruction Fetch Unit) 取指单元

**模块功能**：
- 从ICache/InstrUncache获取指令数据
- 进行指令边界检测(InstrBoundary)、预译码(PreDecode)
- 分支预测检查(PredChecker)、RVC指令扩展(RvcExpander)
- MMIO取指处理、Trigger检查

**测试场景**：

| 场景编号 | 场景名称 | 场景描述 | 关键信号 |
|---------|---------|---------|---------|
| IFU-001 | ICache取指 | 正常的ICache取指流程 | io.fromICache.fetchResp |
| IFU-002 | MMIO取指 | MMIO地址空间的指令获取 | io.toUncache.req |
| IFU-003 | 跨页指令处理 | 指令跨越页边界的处理 | crossPage信号 |
| IFU-004 | 预测错误检测 | 预译码发现的分支预测错误 | io.toFtq.wbRedirect |
| IFU-005 | 指令边界检测 | RVC/RVI混合指令的边界识别 | instrBoundary.io |
| IFU-006 | RVC指令扩展 | 16位压缩指令扩展为32位 | rvcExpanders |
| IFU-007 | Trigger触发 | 前端Trigger匹配触发 | frontendTrigger.io |
| IFU-008 | 异常处理 | ICache异常(缺页、访问错误)的处理 | exceptionType |

**接口交互**：
- 与FTQ：接收取指请求(FtqToIfuIO)、发送写回(IfuToFtqIO)
- 与ICache：发送请求(IfuToICacheIO)、接收响应(ICacheToIfuIO)
- 与InstrUncache：发送MMIO请求(IfuToInstrUncacheIO)
- 与IBuffer：发送指令(DecoupledIO[FetchToIBuffer])

---

#### 2.1.4 ICache (Instruction Cache) 指令缓存

**模块功能**：
- 缓存指令数据，提供低延迟的指令访问
- 支持预取、缺失处理、替换策略
- 与L2 Cache通过TileLink协议交互
- ITLB地址翻译、PMP权限检查

**测试场景**：

| 场景编号 | 场景名称 | 场景描述 | 关键信号 |
|---------|---------|---------|---------|
| ICACHE-001 | Cache命中 | ICache命中的取指流程 | io.toIfu.fetchResp.valid |
| ICACHE-002 | Cache缺失 | ICache缺失的处理流程 | missUnit.io |
| ICACHE-003 | 预取 | 指令预取机制 | prefetchPipe.io |
| ICACHE-004 | 替换 | Cache替换策略 | replacer.io |
| ICACHE-005 | TileLink交互 | 与L2 Cache的协议交互 | clientNode |
| ICACHE-006 | ITLB翻译 | 指令地址翻译 | io.itlb |
| ICACHE-007 | PMP检查 | 物理内存保护检查 | io.pmp |
| ICACHE-008 | 异常处理 | 访问异常、缺页异常 | exceptionType |
| ICACHE-009 | Fence.i | 指令缓存刷新 | io.fencei |

**接口交互**：
- 与FTQ：接收预取请求(FtqToICacheIO)
- 与IFU：发送指令数据(ICacheToIfuIO)
- 与ITLB：地址翻译请求(TlbRequestIO)
- 与PMP：权限检查请求(PmpCheckBundle)
- 与L2 Cache：TileLink协议交互(clientNode)

---

#### 2.1.5 ITLB (Instruction TLB) 指令地址翻译

**模块功能**：
- 指令虚拟地址到物理地址的翻译
- 支持多端口翻译请求
- 页表遍历(PTW)与L2 TLB交互
- TLB刷新、权限检查

**测试场景**：

| 场景编号 | 场景名称 | 场景描述 | 关键信号 |
|---------|---------|---------|---------|
| ITLB-001 | TLB命中 | 地址翻译命中 | io.requestor(0).resp.bits.hit |
| ITLB-002 | TLB缺失 | 地址翻译缺失，触发PTW | io.ptw.req |
| ITLB-003 | 多端口翻译 | 多个翻译请求的并发处理 | io.requestor |
| ITLB-004 | 权限检查 | 页面权限检查(读、执行) | io.requestor(0).resp.bits.excp |
| ITLB-005 | TLB刷新 | SFENCE.VMA指令刷新TLB | io.sfence |
| ITLB-006 | CSR控制 | SATP、PRIV等CSR的影响 | io.csr |

**接口交互**：
- 与ICache：接收翻译请求(TlbRequestIO)
- 与PTW：发送页表遍历请求(VectorTlbPtwIO)
- 与CSR：接收CSR配置(TlbCsrBundle)

---

#### 2.1.6 PMP (Physical Memory Protection) 物理内存保护

**模块功能**：
- 物理地址范围的访问权限检查
- 支持多个PMP配置
- 与CSR交互更新配置

**测试场景**：

| 场景编号 | 场景名称 | 场景描述 | 关键信号 |
|---------|---------|---------|---------|
| PMP-001 | 权限检查 | 读/写/执行权限检查 | pmpChecker.io.resp |
| PMP-002 | 多端口检查 | 多个检查请求的并发处理 | pmpRequestor |
| PMP-003 | CSR配置更新 | PMP配置CSR的更新 | pmp.io.distribute_csr |
| PMP-004 | 地址范围匹配 | PMP地址范围的匹配 | pmp.io.pmp |

**接口交互**：
- 与ICache：接收权限检查请求(PmpCheckBundle)
- 与CSR：接收PMP配置更新(distribute_csr)

---

#### 2.1.7 IBuffer (Instruction Buffer) 指令缓冲区

**模块功能**：
- 缓存从IFU接收的指令
- 提供多端口读出接口给后端解码
- 支持flush、反压、bypass等操作
- 指令分发与暂停原因跟踪

**测试场景**：

| 场景编号 | 场景名称 | 场景描述 | 关键信号 |
|---------|---------|---------|---------|
| IBUF-001 | 指令入队 | IFU指令写入IBuffer | io.in |
| IBUF-002 | 指令出队 | 后端从IBuffer读取指令 | io.out |
| IBUF-003 | 队列满/空 | IBuffer满/空的处理 | io.full, empty |
| IBUF-004 | Flush | 流水线冲刷IBuffer清空 | io.flush |
| IBUF-005 | 多端口读 | DecodeWidth端口并发读 | io.out(i) |
| IBUF-006 | 暂停跟踪 | 暂停原因跟踪(stallReason) | io.stallReason |

**接口交互**：
- 与IFU：接收指令(DecoupledIO[FetchToIBuffer])
- 与后端：发送指令(Vec[DecoupledIO[CtrlFlow]])
- 与Frontend：接收flush信号

---

### 2.2 模块间交互测试场景

#### 2.2.1 完整取指流程

| 场景编号 | 场景名称 | 涉及模块 | 场景描述 |
|---------|---------|---------|---------|
| INT-001 | 正常取指流程 | BPU→FTQ→IFU→ICache→ITLB→PMP→IBuffer | 完整的顺序取指流程 |
| INT-002 | 分支预测正确 | BPU→FTQ→IFU→IBuffer→Backend | 分支预测正确，无重定向 |
| INT-003 | 分支预测错误(IFU检测) | IFU→FTQ→BPU | IFU预译码检测到预测错误 |
| INT-004 | 分支预测错误(后端检测) | Backend→FTQ→BPU→IFU | 后端解析分支发现预测错误 |
| INT-005 | ICache缺失处理 | ICache→L2 Cache→ICache→IFU | Cache缺失到填充的完整流程 |
| INT-006 | MMIO取指流程 | IFU→InstrUncache→MMIO | MMIO地址空间的取指 |
| INT-007 | TLB缺失处理 | ICache→ITLB→PTW→ITLB | 地址翻译缺失的处理 |

#### 2.2.2 重定向场景

**重定向是验证的重点场景**，需要详细分析：

| 重定向类型 | 来源 | 触发条件 | 影响范围 | 关键信号 |
|-----------|------|---------|---------|---------|
| IFU预测错误重定向 | IFU | 预译码发现预测错误 | 冲刷IFU流水线、FTQ指针回退 | io.toFtq.wbRedirect |
| 后端分支解析重定向 | Backend | 分支执行结果与预测不符 | 冲刷整个前端流水线 | io.backend.toFtq.redirect |
| 异常重定向 | Backend | 指令异常(缺页、访问错误) | 冲刷前端，跳转到异常处理 | io.backend.toFtq.redirect.bits.interrupt |
| 中断重定向 | Backend | 外部中断 | 冲刷前端，跳转到中断处理 | io.backend.toFtq.redirect.bits.interrupt |

重定向 payload 按当前 `Redirect` 和 Frontend 顶层 bundle 绑定，核对源指令、
完整 FTQ pointer、offset、target、level、backend fault 和 satpFlush。
实际端口与采样相位以所选 DUT 的 signal contract 为准。

### 2.3 IFU 集成验证约束与设计评审

#### 2.3.1 事务与场景约束

将以下实现约束与 ISA、协议要求及人工认可的 golden 交叉核对。

| 关注点 | 当前源码依据（仓库根目录相对路径） | 验证要求 |
| --- | --- | --- |
| 聚合 ICache 响应 | `src/main/scala/xiangshan/frontend/icache/Bundles.scala` 的 `MainPipeToIfuReq` | `firstRange/totalRange/maybeRvcMap` 使用统一 fetch 坐标；`info(0/1)` 承载 block 身份。覆盖单/双块、req1 无效、SRAM/MSHR 混合、跨 line、taken 截断及 stall/flush，不能沿用独立 per-block range/map 合同。 |
| IFU 跨拍一致性 | `src/main/scala/xiangshan/frontend/ifu/Ifu.scala` 的 S1 对齐和 S2 `RegEnable` | 从 S0 response fire 关联事务，在 S1 检查 index、raw-data 取数/拼接与 predecode，在 S2 检查已注册的 instruction/PC/predecode 同属该事务；覆盖 valid hole 和逐级 flush，不能把同名 S2 predecode 当同拍组合计算。 |
| 跨块与上一窗口 half-RVI | `Ifu.scala` 的 `s1_prevEndHalfRviInfo`、`ftqPtr` 和 `wbRedirect` | half-RVI 的 valid/data/PC 是原子状态。raw `blockSel` 用于取数和 half-RVI 来源；IBuffer/FTQ 与 checker redirect 的 effective owner 为 `blockSel || isCrossBlockInstr`。检查 instruction、PC、ftqPtr、endOffset、enqEnable、redirect payload 及恢复后无旧副作用，不能全局替换 raw selector。 |
| invalidTaken 与指令有效位 | `Ifu.scala` 的 `s1_firstInstrCount/s1_instrValid`、`s1_invalidTakenMask`；`PredChecker.scala` 的 `remaskFault/stage1Fault` | 不完整的块尾 RVI 可以 `instrVec.valid=0, invalidTaken=1`，仍参与 PredChecker 修正优先级；普通 JAL/JALR/Non-CFI fault 则必须 valid。采样不可先按 valid 丢弃 invalidTaken，也不可把该槽当完整 taken CFI 或正常入队指令。BIN-989 的定向用例检查同请求较早 JAL、块尾 raw instruction/PC/预测 offset、逐槽裁剪及下一拍 redirect/FTQ 身份和 backend 无年轻交付；负例覆盖缺探针、普通无效槽、逆序和跨请求组合。 |
| JALR 修正与架构目标 | `PredChecker.scala` 的 `fixedIsJump/fixedTarget`；ISA 的 AUIPC/JALR 寄存器与立即数语义 | 非 return JALR 未预测 taken 时，PredChecker 用 seqNextAddr（RVI 为 PC+4）修正，不知道寄存器决定的架构目标。BIN-983/990 定向场景用 AUIPC x6 + JALR x0,x6 构造独立 ISA oracle：检查同请求 JALR 优先级、完整入队 mask、PC/FTQ/offset，随后检查 source-bound backend redirect 和正确目标路径。oracle 应在目标 S2 请求到达 IBuffer 之前接入；不能因迟接 trace 产生的额外回退而误判 DUT 行为。 |
| backend/checker 同拍仲裁 | `ftq/BackendRedirectReceiver.scala`、`IfuRedirectReceiver.scala`、`Ftq.scala`；`Ifu.scala` 的 `wbEnable` 和 backend 清理优先级 | BIN-949/995 的 IFU internal/outbound checker candidate 可以同时 valid，真正抑制在 FTQ：backend 优先选中，下一拍 IFU redirect/resolve 不生效。区分 ahead bypass 与 registered backend payload，核对源指令、FTQ/offset/target 和 checker effective owner。`wbEnable` 不依赖 IBuffer ready；被反压的年轻指令可产生候选写回，不能强求先入队。独立检查 flush、half-RVI bits/状态、enqPtr 及恢复后无旧交付；cfVec 恢复保护窗口不得屏蔽专用仲裁采样，reset 必须丢弃 pending。路径 canary 在同一采样相位比较可读的内部端口、alias 与 `__Vtogcov__`；缺探针、错误身份、残留副作用及非邻拍清理均不得命中。 |
| 非 CFI 误预测训练 | `Ifu.scala` 的 `notCfiTaken` / `canTrain` | 跟踪 `notCfiTaken -> wbRedirect.canTrain -> FTQ resolve` 的同一身份；只看 redirect 出现不足以证明训练。 |
| 第一块修正裁剪第二块 | `PredChecker.scala` 的 first-remask-fault prefix；`Ifu.scala` 的 `s2_fixedInstrValid` 和 IBuffer enqEnable | BIN-874 要求第二块 meta 有效且裁剪前确有 raw blockSel=1 有效槽，实际 fire 只交付完整第一块前缀。定向激励保留前序训练循环，仅替换后段双块窗口中第一块 JAL，fence.i/redirect 后从入口重启并通过 refill latency/反压形成 runahead；不要替换全部训练分支破坏目标双块历史。独立检查两块 PC/data/predecode、effective owner、FTQ/offset、下一拍第一块 redirect 和恢复后连续顺序；一个接受事务只计一次，缺 owner 或 enqEnable 泄漏不能命中。 |
| valid 呈现与实际消费 | `ibuffer/IBuffer.scala` 的 outputEntries、decodeCanAccept；生成 `IBuffer.sv` / `Frontend.sv` | monitor.observations 包含反压期间重复 valid，不能直接作为已消费 trace。IBuffer 消费按 decodeCanAccept 与逐槽 valid 判定；定向 checker 在 pre-drive 相位读取真实 decodeCanAccept 与逐槽 valid，并明确要求 resumingVType=0，再按 ISA 程序顺序检查已接受记录。不能补造 ready=1，也不能按预期 PC 过滤掉错误路径或重复消费。 |
| BPU override 与 target 比较 | `src/main/scala/xiangshan/frontend/bpu/Bpu.scala`、`bpu/mbtb/Bundles.scala` 的 `compareBits` | all-not-taken 且 first-taken one-hot 为空时不应伪 override；mBTB WriteBuffer 比较 attribute、可存储 target lower 及配置存在时的 carry；BTB target diff 与 ITTAGE/RAS full-target diff 分开检查，不能使用统一全位 target 模型。 |
| 共用预取深度 | `src/main/scala/xiangshan/frontend/FrontendParameters.scala`、`ftq/Ftq.scala`、`icache/ICacheWayLookup.scala` | DefaultConfig 的 `PrefetchDepth=32` 限制 BPU runahead 和 WayLookup 容量，不要求两个 occupancy 逐拍相等；覆盖 31/32、full/backpressure、flush-tail 和 wrap。 |
| 旧事务自然完成与丢弃 | `src/main/scala/xiangshan/frontend/instruncache/InstrUncacheEntry.scala` 及 IFU uncache 路径 | BIN-1104 关联第二 beat A/D、resending、旧 response 和独立恢复身份，不能把 flush 当作总线响应已取消；BIN-1067 覆盖旧 cacheable checker redirect 与年轻 NC 的内部请求竞争，检查无旧 TL A、response、交付副作用。 |

#### 2.3.2 测试点前提核对

以下条目用于检查测试点定义与实际集成边界。定义、映射和状态统一维护在
测试点主表及 canonical registry，语义或适用性修改需经过设计评审。

| 关注范围 | 需要确认的前提与检查 |
| --- | --- |
| BIN-814 | 核对 ICache S1 BPU flush 对输出 valid 的门控，以及 IFU 观察边界。定义合法的 kill/无交付条件，证明同拍组合可达性。 |
| BIN-904 | 关联 FTQ 双请求、两项 WayLookup、有效 NC 属性与实际单块接受。构造上下文切换时，核对 SFENCE 的 VA/ASID 范围和源寄存器标志。 |
| BIN-908/909 | 核对双块共享 ITLB/PMP 结果、exception 合并及 per-line 到 instruction-lane 的归属。second-only 异常需要合法生产序列与独立检查。 |
| BIN-927/928、996–1004 | 按 manifest 检查 Trigger 配置字段、timing 和 PC/data match 能力。逐点声明必要探针，缺失字段保留未知值。核对 data 接零和 PC match 的集成语义，区分合法 chain、固定 timing、行为不变性及 helper 输入空间。 |
| BIN-978 | 明确“位置15”使用的坐标、fetch 大小与跨块规则，检查 raw selector、effective owner、较早 JAL 和目标 taken CFI。 |
| BIN-940 | 从 invalidTaken writeback 建立上下文，关联恢复 S0 接受、S1 half 拼接和 S2 实际交付，检查取消与采样连续性。 |
| BIN-951/920/960 | 按真实寄存使能核对 redirect 后 half 状态的有效性和恢复事务，确认旧数据不会被有效复用。 |
| BIN-973 | 区分架构执行和 predicted-taken JALR/CALL/RET，核对同事务无 PredChecker fault 的候选及累计规则。 |
| BIN-979/984 | 联合核对 invalidTaken、isPredTaken 与同槽 JAL/JALR fault 的门控，确认集成前提是否可同时成立。 |
| BIN-1095/1096 | 检查 TL user 字段在当前构建中的存在性，在实际 A valid 且 ready 为低的窗口检查同请求属性稳定。 |

#### 2.3.3 Reset、异常与 Trigger 合同

`hasSatpFlush` 与 ExceptionType 独立，正常上下文切换交付也需被观察。
BIN-954 按定义分别保存 backend exception、satpFlush、跨页异常、GPF 地址及
VS nonleaf 的完整事务 witness，联合场景要求在同一个 run 中满足。
GPF 地址检查使用实际 Gpf 类型、FTQ 与地址身份，跨页检查包含页末 RVI half
和下一页窗口。

硬件 reset 后，Python backend 的 commit pointer、FTQ/PC 映射和待决
commit/resolve/redirect 需与新硬件 epoch 同步。reset 期间停止事务规划与采样，
清除旧单拍反馈和在途上下文。monitor 保留错误、观察和累计统计，translation
oracle 保留场景的完成判据。golden trace/cursor、RNG 和配置按合同保留，程序
重入由用例显式选择。分别检查 epoch-local 与累计 commit 统计。

reset 负例覆盖旧 PC 映射、已排队 redirect、commit/resolve/RAS 反馈，以及新
程序连续交付。PTW/Uncache 的在途 reset 行为按各自协议独立验证。
入口包括 `tests/py/environment/test_backend_hardware_reset_contract.py` 和
`tests/py/jiabowen/test_backend_reset_epoch_v3_dut.py`。

Trigger 场景核对配置更新与非目标稳定、同槽 instruction/PC/predecode/FTQ/
endOffset、enable/select/debug、action 和断点许可。必要字段按测试点逐项
声明，缺失 timing 保留为 `None`。在 redirect/flush 场景中持续采样 held S2
身份，覆盖 backend cfVec recovery guard 窗口，并检查实际 flush 后的新路径交付。
缺探针诊断记录在运行产物中。

## 三、验证策略

### 3.1 环境与检查职责

`FrontendEnv` 调度 DUT reset/CSR、ICache/Uncache/PTW/Backend agent、monitor、
参考模型、checker 和功能覆盖采样。MemoryModel 提供程序字节，翻译与权限
模型提供独立预期，BackendModel 根据观察和 golden trace 规划后端反馈。

检查按指令语义、接口与恢复合同、行为覆盖和高风险局部参考模型组织。
BPU 已接入模型的范围见 [参考模型说明](../../rm/README.md)，具体 predictor
的 counter/provider/replacement 细节按专项需求建模。

### 3.2 激励与预期

- 汇编与 bin-trace 使用真实指令布局、NEMU trace 和 DUT 连续交付比较。
- Python 定向用例通过现有 agent 配置反压、延迟、异常、权限、上下文和恢复。
- 随机场景记录 base seed、scenario key、ordinal 和实际参数及输入哈希。
- 异常与恢复同时核对触发条件、FTQ/PC 身份、实际交付和后续正确路径。
- 模型单测验证判定逻辑，接口门禁验证当前构建能力，真实 DUT 用例验证可达性。

### 3.3 覆盖率与验收

测试点区分 Condition、Checkpoint 和 Object，并唯一映射到 canonical
coverage point/bin。SampleHub 与 Toffee 记录有效 Condition，checker、
assertion 和 trace/reference 比较检查 Checkpoint。

有效 Condition 可形成 HIT。DUT Checkpoint 失败需单独记录，CLOSED 要求正确性
证据与人工语义验收。采样、协议激励或事务归属错误会使 Condition 证据失效。
代码覆盖率按 line/branch/expr/toggle 独立统计。
规则见 [闭环规范](../03_funcov_model/skills.md)，实现见
[funcov README](../../env/funcov/README.md)。

## 四、执行与交付

1. 核对设计、测试点、所选 DUT manifest、接口和必要探针。
2. 准备 testcase、程序/trace、独立预期和目标覆盖映射。
3. 执行适用的模型/接口检查和真实 DUT 场景，保留完整检查条件。
4. 审查同一 run 的 Condition、事务归属、checker/monitor 和完成状态。
5. 将命令、seed、输入/源码/构建哈希、日志、波形与结果保存到独立运行产物中。

设计更新后的刷新与验收遵循
[DUT 构建流程](../../scripts/DUT_BUILD_WORKFLOW.md)。
长期文档维护验证范围、接口和方法，具体运行结果随对应 artifact 保存。
