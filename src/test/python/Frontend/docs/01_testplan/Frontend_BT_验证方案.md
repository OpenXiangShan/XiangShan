# 香山昆明湖处理器前端模块Block Test验证方案

> 迁移说明
>
> 本文件是从旧文档体系迁入的 V3 策略基线，保留模块拆解、测试场景矩阵和验证范围规划。
> 当前真实执行环境已演进为 `src/test/python/Frontend/` 下的 Python / toffee 黑盒验证环境。
> 因此：
> - 本文中的模块划分和测试场景仍可继续使用
> - 本文中的 UVM 资源/排期表述只保留为历史规划基线，不作为当前执行口径

**文档版本**: V1.0 基线迁入版  
**创建日期**: 2026-03-13  
**适用对象**: 香山昆明湖处理器前端模块  
**验证层级**: Block Test (BT)  
**当前执行方法学**: Python / toffee 黑盒验证  

---

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
- 管理多个指针(bpuPtr、pfPtr、ifuPtr、ifuWbPtr、commitPtr)
- 处理IFU预测错误重定向和后端重定向
- 维护分支解析队列和提交队列

**测试场景**：

| 场景编号 | 场景名称 | 场景描述 | 关键信号 |
|---------|---------|---------|---------|
| FTQ-001 | 预测块入队 | BPU预测结果写入FTQ | io.fromBpu.enq |
| FTQ-002 | 预测块出队 | IFU从FTQ获取取指请求 | io.toIfu.req |
| FTQ-003 | 指针管理 | bpuPtr、ifuPtr、commitPtr的正确移动 | bpuPtr, ifuPtr, commitPtr |
| FTQ-004 | IFU重定向处理 | IFU预测错误导致的流水线冲刷 | io.fromIfu.wbRedirect |
| FTQ-005 | 后端重定向处理 | 后端分支解析导致的重定向 | io.fromBackend.redirect |
| FTQ-006 | 队列满/空 | FTQ满导致BPU暂停、空导致IFU暂停 | isFull, isEmpty |
| FTQ-007 | 分支提交 | 后端提交分支信息用于BPU训练 | io.fromBackend.commitInfo |
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

**重定向信号详细分析**：

```scala
// 来自后端的重定向信号
io.backend.toFtq.redirect: Valid[Redirect]
  .valid: Bool                          // 重定向有效
  .bits.ftqIdx: FtqPtr                  // 重定向的FTQ索引
  .bits.ftqOffset: UInt                 // 重定向的FTQ偏移
  .bits.cfiUpdate.isMisPred: Bool       // 是否为预测错误
  .bits.interrupt: Bool                 // 是否为中断
  .bits.hasBackendFault: Bool           // 是否有后端异常
  .bits.debugIsCtrl: Bool               // 是否为控制流重定向
  .bits.debugIsMemVio: Bool             // 是否为内存违例重定向
```

---

### 2.3 IFU 集成验证约束与待评审项

本节承接旧迁移、producer 审计和可达性评审中的可复用结论。源码核对点为 verification `1045f5761a423ffb20b45d9ec3b69886d243f39f`、V3 design `3448f4ad4e381f1ede51a34a6d5cad39bc5daaed`、DefaultConfig；这是源码/合同复核，不是新版 DUT 运行验收。设计或配置变化后须重新核对，不沿用旧报告中的覆盖率快照、缺信号结论或“迁移完成”状态。

#### 2.3.1 事务与场景约束

以下为 RTL 实现约束和验证关注点，不代替 ISA/协议要求或人工认可的 golden：

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
| valid 呈现与实际消费 | `ibuffer/IBuffer.scala` 的 outputEntries、decodeCanAccept；生成 `IBuffer.sv` / `Frontend.sv` | monitor.observations 包含反压期间重复 valid，不能直接作为已消费 trace。当前 IBuffer 由 decodeCanAccept 控制消费，未使用的 cfVec ready 已被优化；定向 checker 在 pre-drive 相位读取真实 decodeCanAccept 与逐槽 valid，并明确要求 resumingVType=0，再按 ISA 程序顺序检查已接受记录。不能补造 ready=1，也不能按预期 PC 过滤掉错误路径或重复消费。 |
| BPU override 与 target 比较 | `src/main/scala/xiangshan/frontend/bpu/Bpu.scala`、`bpu/mbtb/Bundles.scala` 的 `compareBits` | all-not-taken 且 first-taken one-hot 为空时不应伪 override；mBTB WriteBuffer 比较 attribute、可存储 target lower 及配置存在时的 carry；BTB target diff 与 ITTAGE/RAS full-target diff 分开检查，不能使用统一全位 target 模型。 |
| 共用预取深度 | `src/main/scala/xiangshan/frontend/FrontendParameters.scala`、`ftq/Ftq.scala`、`icache/ICacheWayLookup.scala` | DefaultConfig 的 `PrefetchDepth=32` 限制 BPU runahead 和 WayLookup 容量，不要求两个 occupancy 逐拍相等；覆盖 31/32、full/backpressure、flush-tail 和 wrap。 |
| 旧事务自然完成与丢弃 | `src/main/scala/xiangshan/frontend/instruncache/InstrUncacheEntry.scala` 及 IFU uncache 路径 | BIN-1104 关联第二 beat A/D、resending、旧 response 和独立恢复身份，不能把 flush 当作总线响应已取消；BIN-1067 覆盖旧 cacheable checker redirect 与年轻 NC 的内部请求竞争，检查无旧 TL A、response、交付副作用。 |

#### 2.3.2 测试点待评审与关闭条件

下表只保留问题与下一项可证实的动作，不是第二份状态表。当前定义、映射与状态以测试点 CSV 和 pilot 为准；任何语义、适用性或分母修改先与用户/design owner review，再同步主表、模型和用例。本次整理不修改这些内容。

| 测试点 | 已核对的边界或未决问题 | 建议 review / 验证动作 |
| --- | --- | --- |
| BIN-814 | `ICacheMainPipe` 的匹配 S1 BPU flush 会压低 `io.toIfu.req.valid`；IFU `s0_flushFromBpu` 又由此 valid 限定，原 IFU 侧同拍 conjunction 互斥。已有源码/生成 RTL 合同检查，不是缺探针。 | 讨论测试点是否应落在 ICache kill 边界及 IFU 无交付，而不是要求 IFU 看见已被上游抑制的 valid；未经批准不替换原 HIT 条件。 |
| BIN-904 | FTQ raw dual 要求同虚拟页；PrefetchPipe 将同一 `isMmio/itlbPbmt` 复制到两个 WayLookup entry。当前 DUT 的冷 NC 循环有两项 NC WayLookup，但 FTQ req1=0；cacheable 训练后，通过真实 live CFI/FTQ 绑定 redirect 和全局 SFENCE 切到 PBMT.NC，可观察 req1=1、WayLookup1=1、两块 NC、realTwoFetchValid=0，旧 sampler 探针均可读。 | `test_ifu_nc_dual_suppression_v3_dut.py` 以固定 ISA trace、路径 canary 和输入/单块响应身份检查闭环；软件预取短暂占用合法高优先级入口，使首个匹配响应在相邻周期被 IFU s0 实际接受。绝对路径 artifact `ctrl_bin904_nc_exact_20260915_11` 通过 checker/monitor 和 exact-target 审计，已回标 HIT。SFENCE rs1/rs2 是“源寄存器为x0”的布尔值，全 VA/ASID 需1/1，默认0/0且addr=0不失效此高地址页。 |
| BIN-908 | 双预取共享 iTLB 结果，WayLookup 仅保存首个 exception，FTQ 还限制同页及匹配 backend fault 的双块组合；MainPipe 无独立 req1 PMP 检查口。 | design owner 确认 second-only ITLB 的合法生产序列；独立第二块 PMP 必须单独讨论接口/适用性，不能由 OR-reduction 的单测代替。 |
| BIN-909 | MainPipe 的 `s1_exceptionOut` 复制到各 `info(i).icacheMeta`，IFU late-fault 路径存在合并语义；“观察到异常”不足以证明第二 cacheline 精确 lane 归属。 | 保留 second-line-only denied/corrupt/parity、stall/flush 诊断；要求明确的 per-line 到 instruction-lane 归属证据或 RTL/testpoint 处置，不因接口迁移完成而提升状态。 |
| BIN-1000 | 本节 SHA 的 `full-rtl-picker` 构建先生成完整 XSTop，再抽取 Frontend。`backend/fu/NewCSR/Debug.scala` 经 `MatchTriggerIO.GenTdataDistribute` 将 timing 固定为 false；CSR wrapper、Backend、XSCore 将该值接到 Frontend，生成 RTL 已消除 timing 输入/寄存器及差异比较。旧 sampler 的 `chain_pass + timing_block` 联合条件不是当前集成环境的合法场景；helper 源码保留 timing 参数不代表生成 DUT 可改变它。 | 与用户/design owner 讨论：当前集成点验证合法 chain 命中/阻断及固定 timing 语义；可变 timing 的 helper 单元合同单独保留。批准前不修改测试点、producer 或历史 HIT，不将缺探针默认成 0。BIN-927/928、996 至 1003 共用该配置读取入口，需同步迁移并真实重跑，不能因它们的旧 HIT 而放行新构建。 |
| BIN-1004 | `Ifu.scala` 将 trigger data 接零；`ifu/FrontendTrigger.scala` 明确只支持 PC match，data/pds 保留未来使用。 | 讨论目标是“PC/config 不变时预译码变化不影响结果”的行为不变性，还是未来 data-match feature。前者需真实等价变体与 checker，后者需设计支持；优化掉的信号不是 HIT。 |
| BIN-978 | 主表写“Taken位置15跨块且之前已有JAL”，现有 producer 仅检查较早 JAL 与较年轻有效跨块 taken CFI，未校验位置15。当前 DefaultConfig 及旧 c0ca46459 均为 FetchBlockSize=64，并非最近才从32B扩宽。当前 canary 的跨块 RVI 位于0x8000005e，第一块从0x8000003e开始、第二块从0x80000060开始：相对第一块位置16、相对其32B对齐起点位置31、32B块内低半字位置15、effective-owner第二块endOffset=0；这些坐标不可互换。 | 先由用户/design owner 确认“位置15”指哪一种坐标，或批准参数化为真实 fetch 边界；批准前不修改主表、分母和producer，不将普通跨块候选当BIN-978。非页尾截短的64B/32B-half-align fallthrough，其第一块末半字相对起点范围为16..31；只有该边界的证明不能推导所有设计路径不可达。`test_ifu_cross_taken_position_contract.py` 与无目标回标的 `test_ifu_cross_taken_position_v3_dut.py` 保留坐标、真实raw/effective owner和完整固定ISA trace证据；该canary没有较早JAL，不是该leaf命中。 |
| BIN-940 | “invalidTaken 恢复保留所需 halfRviInfo”是跨拍因果关系，不要求原 invalidTaken 请求和恢复事务的 half-valid 同拍成立。producer 从已核对 raw selector/effective owner、half PC/data 和 FTQ payload 的 checker writeback 建立上下文，经过恢复 S0 fire、S1 拼接/predecode、S2 同身份实际交付后才命中；取消、缺探针、采样断拍和载荷变化均丢弃证明。 | 恢复指令的 endOffset 是新 fetch block 的 0，不是原 redirect offset 或 cacheline 半字索引；`Ifu.scala` 的 saved-half 拼接显式赋 0。定向 checker 验证完整指令/PC/FTQ/offset/mask、S1/S2 路径 canary 和固定 ISA trace 的 backend 恢复；允许 backend 修正前的合法推测交付，但不得把高半字作为额外指令交付。只凭保存半字或一次 S1 观察的旧证据不能闭环。 |
| BIN-951 / BIN-920 / BIN-960 | `Ifu.scala` backend redirect 立即清除 `s0_prevEndIsHalfRvi`、S1/S2 stage valid、S1 half-RVI PC/data 和 enqueue pointer；`s1_prevEndHalfRviInfo.valid` 仅在 `s0_fire` 时寄存 `s0_prevEndIsHalfRvi`，不是 backend 直接清零。当前 DUT 的 live-half 定向证据中，redirect 后该 bit 仍为 1，但 S1/S2 已无效；下一次 S0 接收后 bit=0，恢复路径连续 16 条 C.NOP 正确。逐 bit“valid/data/PC 同拍清零”的旧假定与实现不同，暂不能据此认定功能 bug。 | 与用户/design owner review“清理”是否应定义为旧状态不被有效事务复用，并在首个恢复 S1 事务检查更新后的 half-valid、instruction/PC/FTQ。保留 `test_ifu_backend_half_state_v3_dut.py` 的未决字面 checkpoint 失败；BIN-951 尚缺 producer，不升级；BIN-920/960 的历史 HIT 不作为此新场景已通过的依据。批准前不改主表状态/分母、不用零 PC/data 替代 valid。 |
| BIN-973 | 当前 producer 在单次 run 内累计无同事务 PredChecker fault 的 predicted-taken JALR/CALL/RET。架构执行过间接跳转不等于预测 taken；旧 head/tail 对齐及训练迭代尝试未能闭环。 | 分开记录 raw taken form 和合格 candidate，检查 BPU resolve/training 到预测结果；累计规则与 RVI/RVC 要求须同测试点一致，不能用实验次数或动态指令数替代目标命中。 |
| BIN-979/984 | 本节 SHA 的 `Ifu.scala` 将 `s1_invalidTakenMask(i)` 定义为受 invalid 条件门控的 `s1_predTakenMask(i)`，合并、相同位移及同一 `s1_fire` 寄存后仍满足 `invalidTaken -> isPredTaken`。`PredChecker.scala` 的同槽 `jalFaultVec/jalrFaultVec` 均要求 `!isPredTaken`；因此当前完整 IFU 中“同槽 JAL/JALR fault 与 invalidTaken 并存”的前提互斥。FakeDut 将 invalidTaken=1、predTaken=0 可以测试 helper 优先级，却不是合法集成刺激。 | 请与用户/design owner review 是否将这两点归入 PredChecker 独立模块输入空间，或修订集成测试点适用性；当前不改主表状态/分母。不同槽的更早 JAL/JALR 与后续 invalidTaken 是 BIN-989/990 的独立场景，不能代替同槽要求。 |
| BIN-1095/1096 | `InstrUncacheEntry` 通过 `user.lift(MemBackTypeMM/MemPageTypeNC)` 条件连接 TL user 字段；旧 build 缺失不代表新 build 仍缺失。 | 按当前构建 inventory/实际 path 复核，在真实 A.valid 且 !ready 区间检查相同请求的 user 位稳定性。缺失时保留明确诊断并评审配置/观测能力，不能由地址类型或默认值反推。 |

#### 2.3.3 结论落地与追溯

BIN-954 的 `hasSatpFlush` 与 ExceptionType 相互独立：`Ftq.scala` 分别寄存 backend fault 和 satpFlush，`Ifu.scala` 直接传递 meta(0).hasSatpFlush；正常无异常的 satp/context-change 交付不能被 `exceptionType != 0` 过滤。模型按主表“分别构造”保留 backend exception、satpFlush、跨页异常、GPF 地址及 VS nonleaf 五类已完成事务的独立 witness，需同一个 run 全部满足才标记目标；每类记录 cycle、FTQ 和实际 path。GPF 写入必须是 ExceptionType.Gpf（编码 2），跨页需 page-tail RVI half 与下一页窗口相邻，缺失探针不默认赋值。独立 component DUT 通过不等价于该叶子 HIT。

串行多阶段回归必须验证环境 reset 合同：硬件 reset 后 Python backend 的 commit pointer、FTQ/PC 历史、待决 commit/resolve/redirect 必须与新硬件 epoch 同步。BIN-954 组合回归曾因这些旧状态残留，选取上一阶段已提交 FTQ 身份而触发 stale-context 检查；已通过独立 epoch 初始化修复，不放宽该检查。reset 期间不采样/规划 backend 事务并清除旧单拍反馈，monitor 只清 transient recovery 状态，不清错误、观察或统计；translation oracle 不在 reset 周期采样，也不自动宣告旧场景完成。`commit_count` 内部为 epoch-local，以保持初始 FTQ=0 的边界语义；`get_stats()` 另给出累计 commit、epoch commit 和 reset 次数。golden trace/cursor、RNG 和配置保留，reset 后程序重入必须由用例显式选择 trace/cursor，不能自动 seek。可执行入口为 `test_backend_hardware_reset_contract.py`、`test_backend_reset_epoch_v3_dut.py` 及 BIN-954 单 run 组合回归。

reset 负例应同时覆盖旧 PC 映射、已排队但未驱动的 redirect、commit/resolve/RAS 反馈和新程序连续交付。`callRetCommit.valid` 可携带普通指令（rasAction=0），不能把 valid 本身当旧副作用；须检查实际 RAS 动作和 reset 后重新发布的 FTQ 身份。以上证据证明环境修正及当前元数据场景，不等同全环境随机 reset-inflight 验证，PTW/Uncache 在途 reset 策略仍需各协议回归独立证明。

Trigger 当前构建的执行边界：两条旧用例曾因写不存在的 timing 输入失败，sampler 曾因四个 `tdataVec_*_timing` 不可读而拒绝整个入口。迁移后 BIN-927/928、996–999、1001–1003 只要求其必要的五个配置字段，缺失 timing 显式保留为 `None`，不以默认 0 代替。当前 PC 合同和 held-trigger flush 用例分别声明目标并检查配置更新/非目标稳定、同槽 instruction/PC/predecode/FTQ/endOffset、enable/select/debug、action 和断点许可，以及实际 flush 后的新路径交付。BIN-1003 的原完整 PC 调试输出已裁掉，身份改由 S2 PC、实际 foldpc 和 FTQ 联合核对；专用采样必须覆盖 backend cfVec skip 窗口。仅 ABI canary 的通过不等价于完整叶子闭环；缺探针信息仍保存于 `sampler_diagnostics.frontend_trigger_config_gap`，不能只依赖有界 recent-risk 列表。

九项迁移的当前证明入口为上述 directed testcase、对应 model/negative tests 和逐例 exact-target artifact，最新证据写回主表，不另建状态表。BIN-1000 保留原 chain+timing 判据和显式 legacy-timing-contract 用例；该分支在当前 DUT 仍因缺 timing 输入失败，不能用其他分支的 PASS 放行。待 review 决定是把 timing-mismatch 放入可驱动它的独立 Trigger 环境，还是调整集成测试点适用性并补同 timing 的 chain 正反例。未经 review 不改该测试点、状态或分母，也不删除失败分支营造全绿回归。

采样实现和已有负例入口见 [funcov 实现说明](../../env/funcov/README.md)；版本迁移与审计规则见 [闭环规范](../03_funcov_model/skills.md)。源码 review、模型单测、真实 DUT 命中分别记账，不将其中一类替代另一类。

旧八份报告的正文、原 run_id、波形位置和未命中尝试保留在 Git 历史，读取方法见 [文档索引](../README.md)。只保留有新信息的失败/replay artifact，不在交付目录重复维护逐次运行日志或阶段覆盖率数字。

## 三、验证策略

### 3.1 验证架构框图

```mermaid
graph TB
    subgraph "验证环境 Testbench"
        TB[Testbench Top]
        ENV[UVM Environment]
        
        subgraph "激励生成"
            BIN[Binary Generator<br/>Sting/Force]
            L2VIP[L2 Cache VIP]
            REDIR[Redirect Generator<br/>重点激励源]
        end
        
        subgraph "参考模型"
            RM[Reference Model]
            SCORE[Scoreboard]
        end
        
        subgraph "监控器"
            MON[Monitor]
            COV[Coverage Collector]
        end
    end
    
    subgraph "待测设计 DUT: Frontend"
        BPU[BPU<br/>分支预测单元]
        FTQ[FTQ<br/>取指目标队列]
        IFU[IFU<br/>取指单元]
        ICACHE[ICache<br/>指令缓存]
        ITLB[ITLB<br/>指令TLB]
        PMP[PMP<br/>物理内存保护]
        IBUF[IBuffer<br/>指令缓冲区]
        UNC[InstrUncache<br/>MMIO处理]
    end
    
    subgraph "外部接口"
        L2[L2 Cache]
        BACKEND[Backend<br/>后端]
        CSR[CSR<br/>控制寄存器]
    end
    
    BIN --> L2VIP
    L2VIP --> ICACHE
    L2VIP --> UNC
    
    REDIR --> BACKEND
    BACKEND --> FTQ
    
    CSR --> BPU
    CSR --> ITLB
    CSR --> PMP
    
    BPU <--> FTQ
    FTQ <--> IFU
    IFU <--> ICACHE
    IFU --> IBUF
    ICACHE <--> ITLB
    ICACHE <--> PMP
    IFU <--> UNC
    
    TB --> ENV
    ENV --> BIN
    ENV --> L2VIP
    ENV --> REDIR
    ENV --> RM
    ENV --> SCORE
    ENV --> MON
    ENV --> COV
    
    MON --> SCORE
    RM --> SCORE
    COV --> TB
    
    style BPU fill:#e1f5ff
    style FTQ fill:#e1f5ff
    style IFU fill:#e1f5ff
    style ICACHE fill:#fff4e1
    style IBUF fill:#e8f5e9
    style REDIR fill:#ffebee
```

### 3.2 验证方法

#### 3.2.1 激励策略

**指令激励生成**：
1. **Binary Generator (Sting/Force)**：
   - 生成bin/elf格式的测试用例
   - 包含各种分支模式、函数调用序列
   - 覆盖不同的控制流场景

2. **L2 Cache VIP**：
   - 模拟L2 Cache的行为
   - 存储指令数据
   - 支持可配置的延迟、命中率
   - 支持TileLink协议

**重定向激励生成（重点）**：
```verilog
// 重定向激励关键要点
1. 重定向时机：
   - 与前端流水线状态同步
   - 在不同流水级触发重定向
   - 支持连续重定向、嵌套重定向

2. 重定向类型：
   - IFU预测错误重定向（预译码检测）
   - 后端分支解析重定向（执行结果检测）
   - 异常重定向（缺页、访问错误）
   - 中断重定向

3. 重定向参数：
   - ftqIdx: 随机选择已分配的FTQ条目
   - ftqOffset: 随机选择条目内的指令位置
   - isMisPred: 随机设置预测错误标志
   - 其他随机参数：确保覆盖率

4. 重定向频率：
   - 低频重定向（0-10%）：模拟正常执行
   - 中频重定向（10-50%）：测试重定向处理能力
   - 高频重定向（50-100%）：压力测试
```

**其他信号激励**：
- CSR控制信号：随机配置，确保功能覆盖
- PMP配置：随机更新，测试权限检查
- TLB配置：支持刷新、更新等操作

#### 3.2.2 参考模型设计

**参考模型架构**：
```
Reference Model
├── BPU Model
│   ├── Branch Predictor Models
│   └── History Register Models
├── FTQ Model
│   ├── Entry Queue
│   ├── Pointer Management
│   └── Redirect Handler
├── IFU Model
│   ├── Instruction Fetch Logic
│   ├── Pre-decoder
│   └── Branch Checker
├── ICache Model
│   ├── Cache Array
│   ├── Replacement Policy
│   └── Miss Handler
├── ITLB Model
│   ├── TLB Entries
│   └── Page Table Walker
├── PMP Model
│   └── Permission Checker
└── IBuffer Model
    ├── Buffer Array
    └── Read/Write Logic
```

#### 3.2.3 检查策略

**自动检查**：
1. **指令流检查**：IBuffer输出的指令序列与参考模型一致
2. **预测正确性检查**：分支预测结果与实际执行结果对比
3. **指针一致性检查**：FTQ各指针的正确移动
4. **重定向正确性检查**：重定向后的状态恢复

**断言检查**：
```systemverilog
// 示例断言
assert property (@(posedge clk) disable iff (!rst_n)
    (ftq.full |-> !bpu.req.ready))
    else $error("FTQ full but BPU still sending");

assert property (@(posedge clk) disable iff (!rst_n)
    (redirect.valid |-> ##1 (bpu.flush && ftq.flush))
    else $error("Redirect not flushed properly");
```

#### 3.2.4 覆盖率策略

**功能覆盖率**：
- 分支预测覆盖：各种分支类型、预测器组合
- 重定向覆盖：不同类型、频率、时机的重定向
- 流水线状态覆盖：满、空、各级流水状态
- 异常覆盖：各种异常类型

**代码覆盖率**：
- 行覆盖率：目标100%
- 条件覆盖率：目标100%
- 状态机覆盖率：目标100%
- 翻转覆盖率：目标95%以上

---

## 四、验证计划

### 4.1 验证阶段规划

| 阶段 | 任务 | 预期结果 | 依赖文档 | 参与人员 | 时间周期 |
|-----|------|---------|---------|---------|---------|
| **阶段1：环境搭建** | | | | | |
| 1.1 | UVM框架搭建 | 完整的UVM验证环境框架 | UVM方法学文档 | 负责人A | 2周 |
| 1.2 | DUT集成与接口连接 | DUT正确集成到验证环境 | Frontend RTL文档 | 负责人A | 1周 |
| 1.3 | L2 Cache VIP集成与配置 | VIP正确工作，支持TileLink | L2 VIP手册 | 负责人B | 1周 |
| 1.4 | 基础测试用例编写 | 能够运行简单的取指流程 | 子模块UT用例 | 负责人C | 1周 |
| **阶段2：参考模型开发** | | | | | |
| 2.1 | BPU参考模型 | 能够正确预测分支 | BPU设计文档 | 负责人D | 3周 |
| 2.2 | FTQ参考模型 | 能够正确管理预测队列 | FTQ设计文档 | 负责人A | 2周 |
| 2.3 | IFU参考模型 | 能够正确处理指令获取 | IFU设计文档 | 负责人C | 2周 |
| 2.4 | ICache/ITLB/PMP参考模型 | 能够正确模拟缓存和翻译 | 各模块设计文档 | 负责人B | 2周 |
| 2.5 | IBuffer参考模型 | 能够正确缓存和分发指令 | IBuffer设计文档 | 负责人C | 1周 |
| **阶段3：测试场景梳理** | | | | | |
| 3.1 | 测试点分解文档 | 详细的测试点列表 | 验证方案文档 | 全员参与 | 2周 |
| 3.2 | 测试场景优先级划分 | P0/P1/P2级测试场景 | 测试点文档 | 负责人A | 1周 |
| **阶段4：用例开发** | | | | | |
| 4.1 | P0级测试用例开发 | 核心功能验证通过 | 测试点文档 | 全员参与 | 4周 |
| 4.2 | P1级测试用例开发 | 重要功能验证通过 | 测试点文档 | 全员参与 | 3周 |
| 4.3 | P2级测试用例开发 | 边界条件验证通过 | 测试点文档 | 全员参与 | 2周 |
| **阶段5：功能覆盖率开发** | | | | | |
| 5.1 | 功能覆盖点定义 | 覆盖率模型定义完成 | 验证方案文档 | 负责人D | 1周 |
| 5.2 | 覆盖率收集实现 | 能够收集功能覆盖率 | 覆盖点定义文档 | 负责人D | 2周 |
| 5.3 | 覆盖率分析与改进 | 达到100%功能覆盖率 | 覆盖率报告 | 全员参与 | 持续 |
| **阶段6：仿真调试** | | | | | |
| 6.1 | 用例调试与修复 | 所有测试用例通过 | 测试报告 | 全员参与 | 4周 |
| 6.2 | 回归测试 | 建立持续回归机制 | 回归脚本 | 负责人B | 1周 |
| **阶段7：子模块整合调试** | | | | | |
| 7.1 | BPU-FTQ-IFU整合 | 三模块协同验证通过 | 整合测试文档 | 负责人A | 2周 |
| 7.2 | ICache-ITLB-PMP整合 | 缓存子系统验证通过 | 整合测试文档 | 负责人B | 2周 |
| 7.3 | 完整前端整合 | 所有子模块协同验证通过 | 整合测试文档 | 全员参与 | 2周 |
| **阶段8：环境参数化设计** | | | | | |
| 8.1 | 参数化配置设计 | 支持不同配置的验证 | 参数化需求文档 | 负责人A | 2周 |
| 8.2 | 多配置回归验证 | 所有配置验证通过 | 参数化设计文档 | 全员参与 | 2周 |
| **阶段9：持续回归与覆盖率** | | | | | |
| 9.1 | 自动化回归平台 | 建立自动化回归流程 | 回归平台文档 | 负责人B | 1周 |
| 9.2 | 覆盖率分析与报告 | 定期生成覆盖率报告 | 覆盖率报告 | 负责人D | 持续 |
| 9.3 | 问题追踪与修复 | 所有问题得到解决 | 缺陷追踪系统 | 全员参与 | 持续 |

### 4.2 里程碑与交付物

| 里程碑 | 时间点 | 交付物 | 验收标准 |
|-------|--------|--------|---------|
| M1: 环境搭建完成 | 第5周 | UVM验证环境、基础测试用例 | 能够运行简单取指流程 |
| M2: 参考模型完成 | 第15周 | 完整的参考模型 | 能够与DUT对比检查 |
| M3: P0用例完成 | 第23周 | P0级测试用例、测试报告 | P0级功能验证通过 |
| M4: 覆盖率达标 | 第30周 | 覆盖率报告 | 功能覆盖率100%、代码覆盖率>95% |
| M5: 验证完成 | 第35周 | 验证报告、回归测试报告 | 所有测试用例通过、覆盖率达标 |

---

## 五、验证人员安排

### 5.1 人员需求

| 角色 | 人数 | 主要职责 | 技能要求 | 工作量占比 |
|-----|------|---------|---------|-----------|
| 验证负责人 | 1 | 项目管理、架构设计、技术决策 | UVM专家、前端架构理解 | 100% |
| 高级验证工程师 | 2 | 参考模型开发、复杂场景验证 | UVM熟练、系统级验证经验 | 100% |
| 中级验证工程师 | 2 | 用例开发、调试、覆盖率分析 | UVM基础、调试能力 | 100% |
| 协调支持 | 1 | 环境维护、工具支持、资源协调 | 工具熟练、沟通能力 | 50% |

**总人力**: 6人（全职5人，兼职1人）

### 5.2 时间估算

| 阶段 | 时间周期 | 累计时间 | 备注 |
|-----|---------|---------|------|
| 环境搭建 | 5周 | 5周 | 包括VIP集成 |
| 参考模型开发 | 10周 | 15周 | 并行开发，需协调 |
| 测试场景梳理 | 3周 | 18周 | 与参考模型并行 |
| 用例开发 | 9周 | 27周 | 分优先级开发 |
| 覆盖率开发 | 3周 | 30周 | 与用例开发并行 |
| 仿真调试 | 4周 | 34周 | 问题修复时间 |
| 整合调试 | 6周 | 40周 | 子模块整合 |
| 参数化设计 | 4周 | 44周 | 多配置支持 |
| 回归与报告 | 持续 | - | 直至覆盖率达标 |

**总验证周期**: 约11个月（44周）

### 5.3 人员分工

#### 验证负责人（负责人A）
- 验证环境架构设计
- FTQ参考模型开发
- 测试点分解与优先级划分
- BPU-FTQ-IFU整合调试
- 参数化配置设计
- 技术决策与问题解决

#### 高级验证工程师1（负责人B）
- L2 Cache VIP集成与配置
- ICache/ITLB/PMP参考模型开发
- 回归测试平台搭建
- ICache-ITLB-PMP整合调试
- 自动化回归流程建立

#### 高级验证工程师2（负责人D）
- BPU参考模型开发
- 功能覆盖点定义与实现
- 覆盖率分析与改进
- 覆盖率报告生成

#### 中级验证工程师1（负责人C）
- 基础测试用例编写
- IFU参考模型开发
- IBuffer参考模型开发
- P0/P1/P2级测试用例开发

#### 中级验证工程师2（负责人E）
- 测试用例开发
- 仿真调试与问题修复
- 代码覆盖率分析

#### 协调支持（负责人F，兼职）
- 验证环境维护
- 工具支持（EDA工具、服务器资源）
- 跨部门协调（与RTL团队、后端团队）

### 5.4 协作机制

1. **周例会**：每周一次，同步进度、讨论问题
2. **技术评审**：关键节点进行技术评审（参考模型、测试点、覆盖率）
3. **文档管理**：统一文档管理平台，版本控制
4. **问题追踪**：使用缺陷追踪系统，记录和跟踪所有问题
5. **代码评审**：所有代码变更需经过代码评审

---

## 六、风险管理

### 6.1 风险识别

| 风险项 | 风险等级 | 影响 | 概率 | 缓解措施 |
|-------|---------|------|------|---------|
| 参考模型复杂度高 | 高 | 延长开发周期 | 中 | 分阶段开发，优先完成核心功能 |
| BPU预测逻辑复杂 | 高 | 验证难度大 | 高 | 与RTL团队密切合作，详细文档 |
| 重定向场景复杂 | 高 | 状态空间大 | 高 | 重点设计重定向激励，充分测试 |
| 子模块UT各自为战 | 中 | 整合问题多 | 高 | 统一接口规范，提前整合测试 |
| L2 VIP功能限制 | 中 | 激励能力受限 | 中 | 提前评估VIP能力，准备备选方案 |
| 人力不足 | 中 | 进度延迟 | 中 | 合理分工，提前培训 |

### 6.2 应对策略

1. **技术风险应对**：
   - 提前进行技术预研
   - 与RTL团队建立定期沟通机制
   - 准备技术备选方案

2. **进度风险应对**：
   - 建立缓冲时间
   - 优先完成P0级功能验证
   - 并行开展不依赖的任务

3. **质量风险应对**：
   - 建立严格的代码评审机制
   - 定期进行覆盖率分析
   - 引入第三方验证（如形式验证）

---

## 七、附录

### 7.1 参考文档

1. 香山昆明湖处理器设计文档
2. Frontend模块设计规格文档
3. 子模块设计文档（BPU/FTQ/IFU/ICache/ITLB/PMP/IBuffer）
4. UVM验证方法学文档
5. TileLink协议规范
6. L2 Cache VIP使用手册
7. 子模块UT测试点文档

### 7.2 术语表

| 术语 | 英文全称 | 中文含义 |
|-----|---------|---------|
| BPU | Branch Prediction Unit | 分支预测单元 |
| FTQ | Fetch Target Queue | 取指目标队列 |
| IFU | Instruction Fetch Unit | 取指单元 |
| ICache | Instruction Cache | 指令缓存 |
| ITLB | Instruction TLB | 指令地址翻译缓冲 |
| PMP | Physical Memory Protection | 物理内存保护 |
| IBuffer | Instruction Buffer | 指令缓冲区 |
| BTB | Branch Target Buffer | 分支目标缓冲 |
| RAS | Return Address Stack | 返回地址栈 |
| GHR | Global History Register | 全局历史寄存器 |
| PHR | Path History Register | 路径历史寄存器 |
| RVC | RISC-V Compressed | RISC-V压缩指令 |
| MMIO | Memory-Mapped I/O | 内存映射I/O |
| PTW | Page Table Walker | 页表遍历器 |
| VIP | Verification IP | 验证知识产权核 |

### 7.3 修订历史

| 版本 | 日期 | 修订人 | 修订内容 |
|-----|------|--------|---------|
| V1.0 | 2026-03-13 | 验证团队 | 初始版本 |

---

**文档结束**
