# V2-only 顶层端口 Scala 语义分析

## 1. 分析范围

本文基于以下文档中的 `V2-only` 顶层端口清单，阅读 V2 与 V3 Scala 源码后，分析这些 V2-only 端口对应的源码位置、功能逻辑，以及 V3 是否具备同含义顶层信号。

- `AI_DOC/analysis/interface/v2/memblock_v2whole_v3_port_diff_detail_20260707.md`
- `AI_DOC/analysis/interface/v2/memblock_v2whole_v3_memblock_interface_delta_20260707.md`

源码基准：

| 版本 | 路径 | 说明 |
| --- | --- | --- |
| V2 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan` | 当前目录，V2 整核源码 |
| V3 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan` | 用户指定的 V3 版本源码 |

本文按 Scala 逻辑信号族折叠分析。`exceptionVec_0..23`、多路 `issueLda_0..2`、`writebackLda_0..2` 等扁平 Verilog 端口均来自同一个 Scala bundle 或 Vec 展开，逐位/逐路端口在“覆盖端口”中列出。

## 2. 总体结论

V2-only 端口不是一类问题，主要分为四组：

| 类别 | V2-only 端口族 | V3 同含义判断 | 结论 |
| --- | --- | --- | --- |
| 旧 backend/memblock 拆分 issue/writeback 接口 | `issueLda`、`issueSta`、`issueStd`、`issueVldu`、`writebackLda`、`writebackSta`、`writebackStd`、`writebackVldu` | V3 顶层没有同名同结构端口，但内部仍按 FU 类型 filter 出近似逻辑 | 不是简单改名。V3 顶层改为 `intIssue/vecIssue` 与 `intWriteback/vecWriteback` 聚合接口。 |
| V2 ROB/LSQ MMIO 细粒度状态 | `loadMmio/loadMmioUop/storeMmio/storeMmioUop`、`pendingMMIOld/pendingst/scommit` | V3 顶层只保留 `mmioBusy` 和较少 ROB-LSQ 控制输入 | V3 不具备同粒度同含义端口。 |
| V2 旧 CSR/BPU/TLB/top 控制字段 | `bp_ctrl_*_enable`、`tlbCsr_priv_debug`、`backendToTopBypass_cpuHalted`、`outer_cpu_halt` | V3 有部分相近字段，但命名、字段集合或语义粒度变化 | 需要版本化映射，不能直接按名字替换。 |
| V2 vector partial replay 扩展字段 | `isVecPartReplay`、`vecReplayMask`、`vecReplayMbIdx`、`flowNum`、`vdIdx/vdIdxInField` | V3 内部仍有 vector replay/merge 概念，但 V3 顶层 issue/writeback 组织变化 | V3 不是没有功能，而是被包进新的 `ExuInput/ExuOutput/MemWriteBack` 结构。 |

最关键的源码证据是：

- V2 `src/main/scala/xiangshan/mem/MemBlock.scala` 中 `ooo_to_mem` 直接定义 `issueLda/issueSta/issueStd/issueVldu`，`mem_to_ooo` 直接定义 `writebackLda/writebackSta/writebackStd/writebackVldu`。
- V3 `src/main/scala/xiangshan/mem/MemBlock.scala` 中 `ooo_to_mem` 改为 `intIssue/vecIssue`，`mem_to_ooo` 改为 `intWriteback/vecWriteback`，随后在 `MemBlockInlinedImp` 内部通过 `params.hasLoadFu/hasStoreAddrFu/hasStdFu/hasVLoadFu` filter 回内部执行单元输入输出。
- 因此 V2-only issue/writeback 端口的功能仍可能存在于 V3 内部，但 V3 不再把这些拆分接口作为同含义顶层端口暴露。

## 3. 逐信号族分析

### 3.1 `csrCtrl.bp_ctrl.*_enable`

覆盖端口：

- `io_ooo_to_mem_csrCtrl_bp_ctrl_btb_enable`
- `io_ooo_to_mem_csrCtrl_bp_ctrl_ras_enable`
- `io_ooo_to_mem_csrCtrl_bp_ctrl_sc_enable`
- `io_ooo_to_mem_csrCtrl_bp_ctrl_tage_enable`
- `io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable`

V2 源码：

- `src/main/scala/xiangshan/frontend/BPU.scala` 定义 `BPUCtrl`：`ubtb_enable`、`btb_enable`、`bim_enable`、`tage_enable`、`sc_enable`、`ras_enable`、`loop_enable`。
- `src/main/scala/xiangshan/Bundle.scala` 的 `CustomCSRCtrlIO` 将 `bp_ctrl` 作为 CSR 状态下发给前端和 memblock。
- `src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala` 从 `sbpctl` CSR 位生成这些 enable。
- `src/main/scala/top/MemBlockTop.scala` 对 `ubtb_enable/btb_enable/tage_enable/sc_enable` 做 `dontTouch`，所以整核生成时会保留成 MemBlock 顶层端口。

功能逻辑：

- 这些信号是分支预测器各子预测器的 CSR 使能位。
- `ubtb_enable` 控制 micro BTB 类预测。
- `btb_enable` 控制 V2 FTB/BTB 类预测。
- `tage_enable` 和 `sc_enable` 控制 TAGE 与 statistical corrector。
- `ras_enable` 控制 return address stack 预测。

V3 对比：

- V3 `BpuCtrl` 位于 `src/main/scala/xiangshan/frontend/bpu/Bundles.scala`，字段改为 `ubtbEnable/abtbEnable/mbtbEnable/tageEnable/scEnable/ittageEnable/rasEnable`。
- V3 `NewCSR.scala` 从 `sbpctl` 生成 `UBTB/ABTB/MBTB/TAGE/SC/ITTAGE/RAS` enable。

结论：

- `ubtb_enable`、`tage_enable`、`sc_enable`、`ras_enable` 在 V3 有近似同义字段，但命名改为 camelCase。
- `btb_enable` 在 V3 没有同含义单一字段，V3 拆成 `abtbEnable` 与 `mbtbEnable` 等更细粒度 BTB 控制。
- 这些端口是 CSR/BPU 结构演进导致的 V2-only 顶层端口，不应在 V2 适配中直接映射到 V3 端口名。

### 3.2 `csrCtrl.hd_misalign_*_enable`

覆盖端口：

- `io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable`
- `io_ooo_to_mem_csrCtrl_hd_misalign_st_enable`

V2 源码：

- `src/main/scala/xiangshan/Bundle.scala` 的 `CustomCSRCtrlIO` 定义 `hd_misalign_ld_enable` 与 `hd_misalign_st_enable`。
- 字段属于 Memory Block 控制类 CSR 下发信号。

功能逻辑：

- 这两个信号用于控制 hardware data misalign load/store 相关逻辑是否启用。
- 端口从 OOO/CSR 侧输入 MemBlock，影响 load/store misalign buffer 或相关路径是否按硬件方式处理 misalign 访问。

V3 对比：

- V3 `CustomCSRCtrlIO` 仍保留 `hd_misalign_ld_enable` 与 `hd_misalign_st_enable`。
- 它们出现在 V2-only 清单，是因为本轮 V2 整核 `MemBlock.sv` 与 V3 `build_memblock/rtl/MemBlock.sv` 的顶层保留策略不同，而不是 V3 Scala 完全没有该控制。

结论：

- Scala 语义上 V3 仍具备近似控制字段。
- 但从当前生成 Verilog 顶层看，它们不是 V3 `MemBlock` 同名顶层端口，因此 UVM 顶层连接不能只按 Scala 字段存在来判定可连。

### 3.3 `tlbCsr.priv.debug`

覆盖端口：

- `io_ooo_to_mem_tlbCsr_priv_debug`

V2 源码：

- `src/main/scala/xiangshan/Bundle.scala` 的 `TlbCsrBundle.priv` 定义 `debug`。
- `src/main/scala/xiangshan/mem/MemBlock.scala` 将 `io.ooo_to_mem.tlbCsr` 延迟后下发给 DTLB/L2TLB 相关逻辑。

功能逻辑：

- `priv.debug` 表示当前特权状态包含 debug mode 信息。
- TLB 侧可用它判断访问权限、特权态翻译或 debug 模式下的特殊行为。

V3 对比：

- V3 `TlbCsrBundle.priv` 不再定义 `debug` 字段，只保留 `mxr/sum/vmxr/vsum/virt/virt_changed/spvp/imode/dmode`。

结论：

- 这是明确的 V2-only 语义字段。
- V3 当前源码不具备同名或同粒度 `TlbCsrBundle.priv.debug` 顶层输入，适配时不能用其他 priv bit 代替。

### 3.4 `vstuIqFeedback.feedbackSlow` 的 vector partial replay 字段

覆盖端口：

- `io_mem_to_ooo_vstuIqFeedback_{0,1}_feedbackSlow_bits_isVecPartReplay`
- `io_mem_to_ooo_vstuIqFeedback_{0,1}_feedbackSlow_bits_vecReplayMask`
- `io_mem_to_ooo_vstuIqFeedback_{0,1}_feedbackSlow_bits_vecReplayMbIdx`

V2 源码：

- `src/main/scala/xiangshan/Bundle.scala` 的 `RSFeedback(isVector = true)` 定义 `isVecPartReplay`、`vecReplayMask`、`vecReplayMbIdx`。
- `MemRSFeedbackIO(isVector = true)` 将其用于 `feedbackSlow/feedbackFast`。
- `src/main/scala/xiangshan/mem/MemBlock.scala` 中 `mem_to_ooo.vstuIqFeedback` 是 `Vec(VstuCnt, new MemRSFeedbackIO(isVector = true))`。
- `src/main/scala/xiangshan/mem/vector/VMergeBuffer.scala` 在需要部分 replay 时生成这些字段：`isVecPartReplay` 表示只 replay 部分 vector flow，`vecReplayMask` 表示需要 replay 的 byte/flow mask，`vecReplayMbIdx` 表示 merge buffer entry index。

功能逻辑：

- vector store/load 拆成多个 flow 后，merge buffer 可以只要求 issue queue replay 部分 flow，而不是整条 vector uop。
- `isVecPartReplay` 是部分 replay 标志。
- `vecReplayMask` 标识需要重发的 vector flow 子集。
- `vecReplayMbIdx` 标识原 merge buffer 条目，后续 replay 可回到同一 merge context。

V3 对比：

- V3 源码仍有 vector replay/merge 概念，并且 `RSFeedback(isVector = true)`、`EntryBundles`、`IssueQueue` 中仍可看到 `isVecPartReplay/vecReplayMask/vecReplayMbIdx`。
- 这些字段没有以同一个 V2 顶层展开形态出现在 V3 `vstuIqFeedback` 对比集合中，是因为 V3 feedback bundle 和顶层生成字段集合已变化。

结论：

- 功能在 V3 内部仍有近似逻辑，但当前 V3 顶层不具备 V2 同名同结构端口。
- 适配时要按 V3 `MemRSFeedbackIO` 实际生成端口重新连接，不能把 V2 `vstuIqFeedback` 三个字段直接视为 V3 缺失功能。

### 3.5 `enqLsq.req.bits.exceptionVec_*`

覆盖端口：

- `io_ooo_to_mem_enqLsq_req_{0..5}_bits_exceptionVec_{0..23}`

V2 源码：

- `src/main/scala/xiangshan/mem/lsqueue/LSQWrapper.scala` 定义 `LsqEnqIO.req = Vec(LSQEnqWidth, Flipped(ValidIO(new DynInst)))`。
- `src/main/scala/xiangshan/backend/Bundles.scala` 的 `DynInst` 定义 `exceptionVec`。
- `src/main/scala/xiangshan/XSCore.scala` 将 `backend.io.mem.lsqEnqIO <> memBlock.io.ooo_to_mem.enqLsq`。
- `src/main/scala/xiangshan/backend/dispatch/NewDispatch.scala` 将 `io.fromRename(i).bits` 整包赋给 `enqLsqIO.req(i).bits`。
- `LSQWrapper` 将 `enq.req.bits` 分别写入 load queue 和 store queue 的入队请求；LSQ 入队分配控制主要使用 `valid/fuType/numLsElem/needAlloc/iqAccept` 等字段。

功能逻辑：

- 这些端口是 `LsqEnqIO.req` 承载完整 `DynInst` 后被 Verilog 展开的结构字段，不表示 LSU 访存异常已经在 LSQ 入队前产生。
- `DynInst.exceptionVec` 在 LSQ enqueue 之前可能包含前端/解码阶段异常，例如取指页错误、取指访问错误、非法指令、虚拟化非法指令等；ROB enqueue 侧通过 `ExceptionNO.selectFrontend(io.enq.req(i).bits.exceptionVec)` 记录这类前端异常。
- 真实 load/store/atomic/vector 访存异常仍在 LSU 后续流水线产生。例如 `LoadUnit.scala` 根据 TLB response 写 `loadPageFault/loadAccessFault/loadGuestPageFault`，`StoreUnit.scala` 写 `storePageFault/storeAccessFault/storeGuestPageFault/storeAddrMisaligned/breakPoint`，`AtomicsUnit.scala` 和 `VSegmentUnit.scala` 也会在执行阶段生成对应异常。
- `XSCore.scala` 在 issue 到功能单元入口调用 `issueUops.map(_.bits.uop.clearExceptions())`，源码注释说明“instructions do not have exceptions when they enter the function units”。这进一步说明 LSU 执行异常不是由 LSQ enqueue 端口预先输入，而是在功能单元流水线内重新生成并写回。
- 每个 lane 对应一个 dispatch/LSQ enqueue slot；每个 `exceptionVec_N` 是 `DynInst.exceptionVec` 展开后的一位。

置 0 影响分析：

- 对 memblock 内部 LSU 功能而言，`enqLsq.req.bits.exceptionVec_*` 不是功能性异常注入接口。若这些位在 memblock V2 接口适配中恒置 0，不会导致 LQ/SQ 分配、load/store 地址生成、TLB/PMP/cache error、misalign、MMIO/uncache、replay 或 exception buffer 等 memblock 内部功能测不到。
- `VirtualLoadQueue` 入队只保存 `robIdx/uopIdx/isvec` 等字段，不保存 `enq.req.bits.exceptionVec`；load 异常由 `LoadUnit` 后续流水线通过 `ldin/ldout/vecFeedback` 等路径产生并回填。
- `StoreQueue` 入队会保存整包 `uop`，但入队时显式将 `hasException(i)` 清为 `false.B`；store entry 真正进入异常状态来自后续 `storeAddrInRe.hasException` 或 vector feedback，而不是来自 LSQ enqueue 的 `exceptionVec`。
- ROB 前端异常路径与 memblock 内部功能不同。`Rob.scala` 在 ROB enqueue 侧通过 `ExceptionNO.selectFrontend(io.enq.req(i).bits.exceptionVec)` 记录前端异常；如果要覆盖取指页错误、取指访问错误、非法指令等前端异常，应在 ROB/前端异常路径单独验证，不能依赖 memblock 的 `enqLsq.req.bits.exceptionVec_*`。

V3 对比：

- V3 仍有 `LsqEnqIO.req` 和 `DynInst`，但 `DynInst`/LSQ enqueue 顶层展开字段集合与 V2 不一致。
- V3-only 清单中也有 `enqLsq_req` 的新增字段，说明不是简单删除，而是 `DynInst` 结构和生成保留字段变化。

结论：

- 这是 V2 LSQ enqueue 事务整包 `DynInst` 的旧展开形态。
- 从 memblock 内部 LSU 行为看，这组端口可视为结构性残留字段；恒置 0 不影响 memblock 内部访存异常、replay、misalign、MMIO/uncache 等功能覆盖。
- 从整核异常行为看，前端异常仍需要由 ROB enqueue/exceptionGen 路径覆盖，不能把本结论扩展为“整核不需要前端异常向量”。
- V3 不具备同名同位宽的顶层端口集合；适配时必须按 V2 `DynInst` 字段驱动，不应套用 V3 transaction。

### 3.6 `enqLsq.req.bits.flushPipe/fuOpType/lastUop/trigger`

覆盖端口：

- `io_ooo_to_mem_enqLsq_req_{0..5}_bits_flushPipe`
- `io_ooo_to_mem_enqLsq_req_{0..5}_bits_fuOpType`
- `io_ooo_to_mem_enqLsq_req_{0..5}_bits_lastUop`
- `io_ooo_to_mem_enqLsq_req_{0..5}_bits_trigger`

V2 源码：

- `DynInst` 中定义 `fuOpType`、`flushPipe`、`trigger`、`lastUop`。
- `fuOpType` 标识 load/store/atomics/vector memory 子操作类型。
- `flushPipe` 表示该指令提交时需要清空流水线。
- `lastUop` 表示多 uop 指令的最后一个 uop。
- `trigger` 携带 trigger action，用于断点/触发器相关异常处理。

功能逻辑：

- LSQ enqueue 时保留这些字段，是为了后续 load/store queue、异常处理、ROB 和提交阶段能识别访存类型、流水线刷新需求和 trigger 行为。
- `fuOpType` 直接影响 load/store/vector 内存操作分类。
- `lastUop` 影响多 uop 指令完成与提交边界。

V3 对比：

- V3 `DynInst` 仍有类似控制信息，但顶层展开不再与 V2 一致。
- V3 `enqLsq_req` 中存在别的 V3-only 字段，说明接口结构已演进。

结论：

- 这些是 V2 LSQ enqueue 必需输入字段。
- V3 没有同名同结构顶层端口，不能按 V3 `enqLsq_req` 直接复用。

### 3.7 `lsqio.loadMmio/loadMmioUop/storeMmio/storeMmioUop`

覆盖端口：

- `io_mem_to_ooo_lsqio_loadMmio_{0..2}`
- `io_mem_to_ooo_lsqio_loadMmioUop_{0..2}_robIdx_value`
- `io_mem_to_ooo_lsqio_storeMmio`
- `io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value`

V2 源码：

- `src/main/scala/xiangshan/mem/MemBlock.scala` 的 `mem_to_ooo.lsqio` 定义 `loadMmio/loadMmioUop/storeMmio/storeMmioUop`。
- `src/main/scala/xiangshan/mem/lsqueue/LSQWrapper.scala` 从 load queue/store queue 汇总这些信号到 `io.rob`。
- `src/main/scala/xiangshan/backend/rob/Rob.scala` 用这些信号给 ROB entry 标记 `mmio`。
- `src/main/scala/xiangshan/XSCore.scala` 将这些信号接到 `backend.io.mem.robLsqIO`。

功能逻辑：

- `loadMmio[i]` 表示第 i 条 load pipeline 发现对应 load 是 MMIO。
- `loadMmioUop[i].robIdx` 指出需要标记的 ROB entry。
- `storeMmio` 和 `storeMmioUop.robIdx` 对 store MMIO 做同样标记。
- ROB 后续用 `mmio` entry 产生 `pendingMMIOld` 等状态，控制 MMIO 串行化和提交顺序。

V3 对比：

- V3 `mem_to_ooo.lsqio` 不再输出 load/store MMIO uop 细节，只输出 `mmioBusy`。
- V3 `XSCore.scala` 将 `backend.io.mem.robLsqIO.mmioBusy := memBlock.io.mem_to_ooo.lsqio.mmioBusy`。

结论：

- V3 不具备 V2 同粒度的 `loadMmioUop/storeMmioUop` 顶层语义。
- V2 可精确告诉 ROB 哪个 ROB entry 是 MMIO；V3 顶层更像整体忙状态摘要。

### 3.8 `ooo_to_mem.lsqio.pendingMMIOld/pendingst/scommit`

覆盖端口：

- `io_ooo_to_mem_lsqio_pendingMMIOld`
- `io_ooo_to_mem_lsqio_pendingst`
- `io_ooo_to_mem_lsqio_scommit`

V2 源码：

- `MemBlock.scala` 的 `ooo_to_mem.lsqio` 定义 `scommit`、`pendingMMIOld`、`pendingst`。
- `XSCore.scala` 从 `backend.io.mem.robLsqIO` 接入这些信号。
- `MemBlock.scala` 将这些字段分发给 `loadMisalignBuffer`、`storeMisalignBuffer` 和 `lsq.io.rob`。
- `StoreQueue.scala` 使用 `pendingst && uop(deqPtr).robIdx === pendingPtr` 判断 ROB 当前 pending store 与 SQ entry 的关系。
- `LoadQueueUncache.scala` 使用 `pendingMMIOld` 跟踪 pending MMIO load。

功能逻辑：

- `scommit` 表示 ROB 本周期提交的 store 数量。
- `pendingst` 表示 ROB head 或相关提交状态中存在 pending store。
- `pendingMMIOld` 表示存在 pending MMIO load。
- 这些信号用于 LSQ、misalign buffer 和 uncache/MMIO 路径与 ROB 提交流程同步，避免 MMIO、store、misalign split 操作越过提交顺序。

V3 对比：

- V3 `ooo_to_mem.lsqio` 保留 `lcommit/scommit/commit/pendingPtr/pendingPtrNext`，删除了 `pendingMMIOld/pendingld/pendingst/pendingVst`。
- V3 用 `mmioBusy` 等新路径替代部分旧 pending 信号。

结论：

- `pendingMMIOld` 与 `pendingst` 是明确 V2-only ROB/LSQ 控制输入。
- `scommit` 在 V3 Scala 中仍存在，但本轮 Verilog 对比显示 V2 顶层展开与 V3 端口集合不一致，连接时仍要以生成 RTL 为准。

### 3.9 `issueLda`

覆盖端口：

- `io_ooo_to_mem_issueLda_{0..2}_valid`
- `io_ooo_to_mem_issueLda_{0..2}_ready`
- `io_ooo_to_mem_issueLda_{0..2}_bits_src_0`
- `io_ooo_to_mem_issueLda_{0..2}_bits_uop_*`

V2 源码：

- `MemBlock.scala` 的 `ooo_to_mem.issueLda = MixedVec(Seq.fill(LduCnt)(Flipped(DecoupledIO(new MemExuInput))))`。
- `XSCore.scala` 将 `memBlock.io.ooo_to_mem.issueLda <> backend.io.mem.issueLda`。
- `MemBlock.scala` 将 `issueLda(i)` 接到 `loadUnits(i).io.ldin`，并用 `issueLda(i).bits.uop.pc` 做预取训练 PC。
- `MemExuInput` 定义 `uop`、`src`、`iqIdx`、`isFirstIssue` 等字段。

功能逻辑：

- 这是 V2 scalar load issue 通道。
- `valid/ready` 是 backend issue queue 与 load unit 的 Decoupled 握手。
- `src_0` 通常作为地址基址寄存器数据。
- `uop.fuOpType/imm/lqIdx/sqIdx/robIdx/pc/pdest/rfWen/fpWen/loadWait*` 等字段共同决定 load 地址生成、LSQ entry、写回目的寄存器、内存依赖等待和异常/重放行为。

V3 对比：

- V3 顶层没有 `issueLda`，而是 `ooo_to_mem.intIssue`。
- V3 `MemBlock.scala` 内部执行 `val issueLda = intIssue.filter(_.bits.params.hasLoadFu)`，再接到 `newLoadUnits(i).io.ldin`。

结论：

- V3 仍有 load issue 语义，但顶层语义不再是 V2 的 `issueLda[N]`。
- 适配必须从 V3 scheduler 参数和 FU 类型过滤关系恢复 lane 映射，不能直接端口改名。

### 3.10 `writebackLda`

覆盖端口：

- `io_mem_to_ooo_writebackLda_{0..2}_valid`
- `io_mem_to_ooo_writebackLda_{0..2}_bits_data`
- `io_mem_to_ooo_writebackLda_{0..2}_bits_debug_isMMIO/isNCIO/isPerfCnt`
- `io_mem_to_ooo_writebackLda_{0..2}_bits_isFromLoadUnit`
- `io_mem_to_ooo_writebackLda_{0..2}_bits_uop_*`

V2 源码：

- `MemBlock.scala` 的 `mem_to_ooo.writebackLda = Vec(LduCnt, DecoupledIO(new MemExuOutput))`。
- `MemBlock.scala` 将 `io.mem_to_ooo.writebackLda <> ldaExeWbReqs`。
- `XSCore.scala` 将 `backend.io.mem.writebackLda <> memBlock.io.mem_to_ooo.writebackLda`。
- `MemExuOutput` 定义 `uop`、`data`、`isFromLoadUnit`、`debug`。

功能逻辑：

- scalar load 完成后通过该通道写回 backend。
- `data` 是 load 结果。
- `debug_isMMIO/isNCIO/isPerfCnt` 标记访问属性。
- `uop.exceptionVec/flushPipe/replayInst/trigger/robIdx/pdest/rfWen/fpWen` 用于 ROB、寄存器写回、异常和 replay。

V3 对比：

- V3 顶层没有 `writebackLda`，改为 `intWriteback` 聚合。
- V3 内部 `val writebackLda = intWriteback.filter(_.params.hasLoadFu)`，仍将 load writeback 分发给 load 路径。

结论：

- V3 有 load writeback 功能，但无 V2 同名同结构顶层端口。
- V2 `MemExuOutput` 与 V3 `MemWriteBack`/`ExuOutput` 的层级不同，monitor/RM 需要版本化 transaction。

### 3.11 `issueSta`

覆盖端口：

- `io_ooo_to_mem_issueSta_{0,1}_valid`
- `io_ooo_to_mem_issueSta_{0,1}_ready`
- `io_ooo_to_mem_issueSta_{0,1}_bits_src_0`
- `io_ooo_to_mem_issueSta_{0,1}_bits_uop_fuOpType/fuType/imm/pdest/rfWen/robIdx/sqIdx`

V2 源码：

- `MemBlock.scala` 定义 `issueSta = MixedVec(Seq.fill(StaCnt)(Flipped(DecoupledIO(new MemExuInput))))`。
- `XSCore.scala` 将其连接到 `backend.io.mem.issueSta`。
- `MemBlock.scala` 将 `issueSta(i)` 接到 store address unit，并在 atomics 路径中用 `FuType.storeIsAMO(issueSta(i).bits.fuType)` 判断 AMO。

功能逻辑：

- store address issue 通道负责计算 store 地址、检查 TLB/PMP、更新 SQ 地址有效状态。
- `src_0` 和 `imm` 参与地址生成。
- `sqIdx/robIdx` 定位 SQ/ROB entry。
- `fuType/fuOpType` 区分普通 store、AMO、CBO 等 store 类操作。

V3 对比：

- V3 顶层没有 `issueSta`，内部从 `intIssue.filter(_.bits.params.hasStoreAddrFu)` 得到。

结论：

- V3 有 store address issue 功能，但没有 V2 顶层拆分端口。
- 该族需要通过 V3 `intIssue` 的 FU 参数映射，不可机械替换。

### 3.12 `writebackSta`

覆盖端口：

- `io_mem_to_ooo_writebackSta_{0,1}_valid`
- `io_mem_to_ooo_writebackSta_{0,1}_bits_debug_isMMIO/isNCIO`
- `io_mem_to_ooo_writebackSta_{0,1}_bits_uop_exceptionVec_*`
- `io_mem_to_ooo_writebackSta_{0,1}_bits_uop_flushPipe/robIdx/trigger`

V2 源码：

- `MemBlock.scala` 定义 `writebackSta = Vec(StaCnt, DecoupledIO(new MemExuOutput))`。
- `MemBlock.scala` 将 `writebackSta` 连接到 `storeUnits.map(_.io.stout)`，并把 `writebackSta ++ writebackHyuSta` 合成 store 输出序列。
- `XSCore.scala` 将该通道接回 backend。

功能逻辑：

- store address 单元完成后写回 store 地址阶段结果。
- 主要用于异常、MMIO/NCIO 属性、ROB 状态和 store 地址执行完成反馈。
- 对 store 指令通常不写普通 RF data，所以 V2-only 展开多集中在 debug、exception、robIdx、trigger。

V3 对比：

- V3 顶层没有 `writebackSta`，内部由 `intWriteback.filter(_.params.hasStoreAddrFu)` 得到。

结论：

- V3 有 store address writeback 类内部逻辑，但顶层接口已聚合。
- V2 `writebackSta` 不能直接接 V3 `intWriteback` 某一路，必须按 FU 参数和 writeback group 建映射。

### 3.13 `issueStd`

覆盖端口：

- `io_ooo_to_mem_issueStd_{0,1}_valid`
- `io_ooo_to_mem_issueStd_{0,1}_ready`
- `io_ooo_to_mem_issueStd_{0,1}_bits_src_0`
- `io_ooo_to_mem_issueStd_{0,1}_bits_uop_fuOpType/fuType/robIdx/sqIdx`

V2 源码：

- `MemBlock.scala` 定义 `issueStd = MixedVec(Seq.fill(StdCnt)(Flipped(DecoupledIO(new MemExuInput))))`。
- `MemBlock.scala` 将 `stdExeUnits(i).io.in <> issueStd(i)`，之后把 store data 写入 LSQ/SQ。

功能逻辑：

- store data issue 通道负责把 store 数据送入 store data 执行单元和 SQ。
- `src_0` 是待写 store data 或其处理前数据。
- `sqIdx` 定位 SQ entry，`robIdx` 定位 ROB entry。

V3 对比：

- V3 顶层没有 `issueStd`，内部从 `intIssue.filter(_.bits.params.hasStdFu)` 得到。

结论：

- V3 有 store data issue 功能，但没有 V2 顶层拆分端口。

### 3.14 `writebackStd`

覆盖端口：

- `io_mem_to_ooo_writebackStd_{0,1}_valid`
- `io_mem_to_ooo_writebackStd_{0,1}_bits_uop_robIdx_value`

V2 源码：

- `MemBlock.scala` 定义 `writebackStd = Vec(StdCnt, DecoupledIO(new MemExuOutput))`。
- `MemBlock.scala` 中 `writebackStd.zip(stdExeUnits)` 连接 store data 执行单元输出。

功能逻辑：

- 表示 store data 执行单元完成一次 store data 路径操作。
- 展开端口只保留 `valid` 与 `robIdx`，说明 V2 整核顶层对该 writeback 的可观察字段很少，主要用于完成/状态反馈，而不是 RF data 写回。

V3 对比：

- V3 顶层没有 `writebackStd`，内部从 `intWriteback.filter(_.params.hasStdFu)` 得到。

结论：

- V3 有内部 store data writeback/完成语义，但顶层接口不同。

### 3.15 `issueVldu`

覆盖端口：

- `io_ooo_to_mem_issueVldu_{0,1}_valid`
- `io_ooo_to_mem_issueVldu_{0,1}_ready`
- `io_ooo_to_mem_issueVldu_{0,1}_bits_flowNum`
- `io_ooo_to_mem_issueVldu_{0,1}_bits_isVecPartReplay`
- `io_ooo_to_mem_issueVldu_{0,1}_bits_vecReplayMask`
- `io_ooo_to_mem_issueVldu_{0,1}_bits_vecReplayMbIdx`
- `io_ooo_to_mem_issueVldu_{0,1}_bits_src_{0..4}`
- `io_ooo_to_mem_issueVldu_{0,1}_bits_uop_*`

V2 源码：

- `MemBlock.scala` 定义 `issueVldu = MixedVec(Seq.fill(VlduCnt)(Flipped(DecoupledIO(new MemExuInput(isVector=true)))))`。
- `MemExuInput(isVector=true)` 增加 `flowNum/isVecPartReplay/vecReplayMask/vecReplayMbIdx`，并把 `src` 扩到 5 个 VLEN 宽源操作数。
- `VSplit.scala` 使用 `flowNum`、`isVecPartReplay`、`vecReplayMask`、`vecReplayMbIdx` 把 vector load/store uop 拆成 flow 或 replay 部分 flow。
- `MemBlock.scala` 用 `VlduType.isVecLd` 和 `VstuType.isVecSt` 将 `issueVldu` 分流到 vector load split 或 vector store split。

功能逻辑：

- 这是 V2 vector load/store issue 通道。
- `flowNum` 表示该 vector uop 拆成的 flow 数。
- `isVecPartReplay/vecReplayMask/vecReplayMbIdx` 支持 merge buffer 触发的部分 replay。
- `src_0..4` 分别承载 vector memory 地址、stride、vs3/mask/vl 等 VLSU 所需源操作数。
- `uop.vpu_*` 字段携带 vector 指令控制信息，例如 `vm/vl/vstart/vsew/vlmul/nf/veew/vuopIdx`。

V3 对比：

- V3 顶层没有 `issueVldu`，改为 `vecIssue` 聚合。
- V3 `MemBlock.scala` 内部 `val issueVldu = vecIssue.filter(_.bits.params.hasVLoadFu)`。
- V3 `VSplit.scala` 中 flow 语义仍存在，但字段名从 V2 `flowNum` 演进为 `numLsElem` 等 V3 `ExuInput` 字段。

结论：

- V3 仍具备 vector memory issue 与 split/replay 逻辑，但没有 V2 同结构顶层端口。
- 这是 V2/V3 VLSU 接口适配中风险最高的一组，需要按 V3 `ExuInput` 和 vector scheduler 参数重建 transaction。

### 3.16 `writebackVldu`

覆盖端口：

- `io_mem_to_ooo_writebackVldu_{0,1}_valid`
- `io_mem_to_ooo_writebackVldu_{0,1}_bits_data`
- `io_mem_to_ooo_writebackVldu_{0,1}_bits_debug_*`
- `io_mem_to_ooo_writebackVldu_{0,1}_bits_uop_*`
- `io_mem_to_ooo_writebackVldu_{0,1}_bits_vdIdx`
- `io_mem_to_ooo_writebackVldu_{0,1}_bits_vdIdxInField`

V2 源码：

- `MemBlock.scala` 定义 `writebackVldu = Vec(VlduCnt, DecoupledIO(new MemExuOutput(isVector = true)))`。
- `MemExuOutput(isVector=true)` 增加 VLEN 宽 `data`、可选 `mask`、`vdIdx`、`vdIdxInField` 和 vector debug 信息。
- `VSegmentUnit.scala` 和 `VMergeBuffer.scala` 生成 `vdIdx/vdIdxInField`，用于 vector writeback 合并与 mask/tail 处理。
- `backend/datapath/VldMergeUnit.scala` 使用 `vdIdxInField` 给 VPU mask generation unit 定位当前写回的是一个 vector register group 的哪一段。

功能逻辑：

- vector load 完成后，将 VLEN 宽数据和 vector uop 控制信息写回 backend。
- `vdIdx` 表示当前写回对应第几个 vd。
- `vdIdxInField` 表示 segment/nf 场景下当前 field 内的 vd index。
- `uop.vpu_*` 字段帮助 ROB/向量寄存器写回和异常恢复处理 vector 指令状态。

V3 对比：

- V3 顶层没有 `writebackVldu`，改为 `vecWriteback`。
- V3 `MemBlock.scala` 内部 `val writebackVldu = vecWriteback.filter(_.bits.params.hasVLoadFu)`。
- V3 `ExuOutput` 中仍有 `vls.vdIdx/vdIdxInField` 类字段，但顶层层级变为 `vecWriteback_*_bits_vls_*`。

结论：

- V3 有 vector load writeback 功能，但顶层结构已从 V2 `MemExuOutput(isVector=true)` 变为 V3 `ExuOutput`/`vecWriteback`。
- 这是同功能、不同组织方式，不是同名端口缺失可简单忽略。

### 3.17 `backendToTopBypass.cpuHalted` 与 `outer_cpu_halt`

覆盖端口：

- `io_ooo_to_mem_backendToTopBypass_cpuHalted`
- `io_outer_cpu_halt`

V2 源码：

- `src/main/scala/xiangshan/backend/Backend.scala` 的 `BackendToTopBundle` 定义 `cpuHalted`。
- `Backend.scala` 将 `io.toTop.cpuHalted := ctrlBlock.io.toTop.cpuHalt`。
- `MemBlock.scala` 将 `io.outer_cpu_halt := RegNext(io.ooo_to_mem.backendToTopBypass.cpuHalted)`。
- `XSCore.scala` 将 `io.cpu_halt := memBlock.io.outer_cpu_halt`。
- `top/MemBlockTop.scala` 将 `outer_cpu_halt` 暴露为顶层输出。

功能逻辑：

- 这是 V2 backend 告诉 top/tile “CPU halt” 状态的旁路信号。
- MemBlock 在整核拆分形态中承接 backend-to-top bypass，再输出到 core/top。

V3 对比：

- V3 `BackendToTopBundle` 字段改为 `cpuWfi`。
- V3 `MemBlock.scala` 输出 `outer_cpu_wfi := RegNext(io.ooo_to_mem.backendToTopBypass.cpuWfi)`。
- V3 `XSCore.scala` 输出 `io.cpu_wfi := memBlock.io.outer_cpu_wfi`。

结论：

- V2 `cpuHalted/outer_cpu_halt` 与 V3 `cpuWfi/outer_cpu_wfi` 语义相近但不完全同名；V3 更明确表示 WFI 状态。
- 后续适配需要确认上层环境期望的是 halt 还是 WFI，不能只做名字替换。

### 3.18 `sfence.bits.flushPipe`

覆盖端口：

- `io_ooo_to_mem_sfence_bits_flushPipe`

V2 源码：

- `src/main/scala/xiangshan/Bundle.scala` 的 `SfenceBundle.bits` 定义 `flushPipe`。
- `MemBlock.scala` 将 `io.ooo_to_mem.sfence` 延迟后接入 TLB 和相关 flush 路径。

功能逻辑：

- 表示该 sfence 请求是否需要 flush pipeline。
- 与 `sfence.valid/rs1/rs2/addr/id/hv/hg` 一起描述地址翻译相关 flush 请求。

V3 对比：

- V3 `SfenceBundle.bits.flushPipe` 仍存在。
- 该字段出现在 V2-only 清单，多半来自本轮生成顶层端口保留和 V3 `MemBlock` 顶层暴露差异，而不是 V3 Scala 删除语义。

结论：

- Scala 语义不是 V2 独有，但当前 Verilog 顶层端口集合中没有 V3 同名端口。
- 连接适配仍必须以 Verilog 顶层为准。

### 3.19 `isStoreException`

覆盖端口：

- `io_ooo_to_mem_isStoreException`

V2 源码：

- `MemBlock.scala` 的 `ooo_to_mem` 定义 `isStoreException = Input(Bool())`。
- `XSCore.scala` 从 `backend.io.mem.isStoreException` 接入。

功能逻辑：

- 指示当前 backend/ROB 异常上下文中是否涉及 store exception。
- MemBlock/LSQ 可据此辅助异常地址、store queue 或异常 flush 处理。

V3 对比：

- V3 `ooo_to_mem` 仍定义 `isStoreException`。
- 如果它在 Verilog 对比中表现为 V2-only，说明当前 V3 生成入口没有保留为同名顶层端口或端口组织不同。

结论：

- Scala 语义在 V3 仍存在，但顶层端口不可按源码字段存在直接假定。

### 3.20 `reset_backend`

覆盖端口：

- `io_reset_backend`

V2 源码：

- `MemBlock.scala` 顶层 IO 定义 `reset_backend = Output(Reset())`。
- `XSCore.scala` 在配置启用时使用 `backend.reset := memBlock.io.reset_backend`。
- `MemBlock.scala` 中该信号与 clock/reset cell node 相关；未启用对应 clock gate/reset 形态时赋 `DontCare`。
- `MemBlockTop.scala` 将其转为 Bool 顶层输出。

功能逻辑：

- 这是 MemBlock 拆分/低功耗或 clock gate/reset 管理相关的 backend reset 输出。
- 用于整核拆分后从 MemBlock 控制 backend reset。

V3 对比：

- V3 也有 `reset_backend` Scala IO，并在 `MemBlockTop.scala` 暴露。
- 本轮清单中它是 V2-only，说明 V3 `build_memblock/rtl/MemBlock.sv` 顶层和 V2 整核 `build/rtl/MemBlock.sv` 对该端口保留不一致。

结论：

- 不能作为功能性 LSQ/issue 端口处理。
- 适配时需要按当前生成 Verilog 决定是否连接或由 wrapper 处理。

### 3.21 `auto_inner_frontendBridge_icache_out_a_bits_user_needHint`

覆盖端口：

- `auto_inner_frontendBridge_icache_out_a_bits_user_needHint`

V2 源码判断：

- 这是 Diplomacy/TileLink 自动生成端口，来自 frontend bridge 到 icache 方向的 TileLink `a` channel user 字段。
- `needHint` 属于请求 user metadata，用于提示 cache/prefetch/hint 类行为。

功能逻辑：

- 该信号不是手写 `MemBlockIO` 字段，而是总线 bundle 展开后的 user bit。
- 语义依赖具体 TL bundle user 字段定义和 bridge 参数。

V3 对比：

- V3 TileLink/auto 端口集合与 V2 有大量差异，包含 user 字段和 channel 字段变化。

结论：

- 这类端口必须以生成后 Verilog 和 Diplomacy 参数为准。
- 不应由 memblock UVM agent 手工等价映射到某个 V3 user 字段，除非确认当前 V2/V3 TL user schema 完全一致。

## 4. V3 不具备同含义顶层端口的代码清单

| V2 端口族 | V2 关键源码 | V3 变化 | 判断 |
| --- | --- | --- | --- |
| `issueLda` | V2 `MemBlock.scala` 直接定义 `ooo_to_mem.issueLda`，`XSCore.scala` 直接连接 `backend.io.mem.issueLda` | V3 顶层改为 `ooo_to_mem.intIssue`，内部 `filter(_.bits.params.hasLoadFu)` 得到 load issue | V3 不具备同结构顶层端口，只具备内部等价功能。 |
| `issueSta` | V2 `MemBlock.scala` 直接定义 `ooo_to_mem.issueSta` | V3 顶层改为 `intIssue`，内部 `filter(_.bits.params.hasStoreAddrFu)` | V3 不具备同结构顶层端口。 |
| `issueStd` | V2 `MemBlock.scala` 直接定义 `ooo_to_mem.issueStd` | V3 顶层改为 `intIssue`，内部 `filter(_.bits.params.hasStdFu)` | V3 不具备同结构顶层端口。 |
| `issueVldu` | V2 `MemBlock.scala` 直接定义 `ooo_to_mem.issueVldu`，`MemExuInput(isVector=true)` 携带 `flowNum/isVecPartReplay/vecReplayMask/vecReplayMbIdx` | V3 顶层改为 `vecIssue`，内部 `filter(_.bits.params.hasVLoadFu)`，字段进入 V3 `ExuInput` 体系 | V3 不具备 V2 同结构顶层端口。 |
| `writebackLda` | V2 `mem_to_ooo.writebackLda = Vec(LduCnt, DecoupledIO(new MemExuOutput))` | V3 顶层改为 `intWriteback`，内部 `filter(_.params.hasLoadFu)` | V3 不具备同结构顶层端口。 |
| `writebackSta` | V2 `mem_to_ooo.writebackSta = Vec(StaCnt, DecoupledIO(new MemExuOutput))` | V3 顶层改为 `intWriteback`，内部 `filter(_.params.hasStoreAddrFu)` | V3 不具备同结构顶层端口。 |
| `writebackStd` | V2 `mem_to_ooo.writebackStd = Vec(StdCnt, DecoupledIO(new MemExuOutput))` | V3 顶层改为 `intWriteback`，内部 `filter(_.params.hasStdFu)` | V3 不具备同结构顶层端口。 |
| `writebackVldu` | V2 `mem_to_ooo.writebackVldu = Vec(VlduCnt, DecoupledIO(new MemExuOutput(isVector=true)))` | V3 顶层改为 `vecWriteback`，内部 `filter(_.bits.params.hasVLoadFu)` | V3 不具备 V2 `MemExuOutput(isVector=true)` 顶层结构。 |
| `lsqio.loadMmio/loadMmioUop/storeMmio/storeMmioUop` | V2 `mem_to_ooo.lsqio` 输出具体 MMIO load/store uop，ROB 用 `robIdx` 标记 entry | V3 `mem_to_ooo.lsqio` 只输出 `mmioBusy` | V3 不具备同粒度同含义端口。 |
| `lsqio.pendingMMIOld/pendingst` | V2 `ooo_to_mem.lsqio` 接收 ROB pending 状态，LSQ/uncache/misalign 使用 | V3 删除这些 pending 输入，保留 `commit/pendingPtr` 等较少字段 | V3 不具备同含义控制输入。 |
| `tlbCsr.priv.debug` | V2 `TlbCsrBundle.priv.debug` | V3 `TlbCsrBundle.priv` 删除 `debug` | V3 源码不具备同字段。 |
| `csrCtrl.bp_ctrl.btb_enable` | V2 `BPUCtrl.btb_enable` | V3 拆成 `abtbEnable/mbtbEnable` 等 BTB 类控制 | V3 不具备单一同义字段。 |
| `backendToTopBypass.cpuHalted/outer_cpu_halt` | V2 `BackendToTopBundle.cpuHalted`，`MemBlock.outer_cpu_halt` | V3 改为 `cpuWfi/outer_cpu_wfi` | 语义相近但不是同名同粒度 halt 端口。 |
| `auto_*_needHint` | V2 Diplomacy/TileLink user 字段展开 | V3 TL/auto user/channel 字段集合变化 | 不能判为同含义端口，需按 TL user schema 单独确认。 |

## 5. 对后续 V2 DUT 适配的建议

1. issue/writeback 类端口必须做版本化 transaction。
   V2 使用 `issueLda/Sta/Std/Vldu` 与 `writebackLda/Sta/Std/Vldu`，V3 使用 `intIssue/vecIssue` 与 `intWriteback/vecWriteback`。二者是功能相近但结构不同的接口。

2. LSQ enqueue 和 ROB/LSQ MMIO 状态不要按字段名凑齐。
   V2 的 `loadMmioUop/storeMmioUop/pendingMMIOld/pendingst` 体现了更细粒度的 ROB-LSQ 协议，V3 当前顶层没有同粒度信号。

3. CSR/BPU 字段只能做显式映射。
   `ubtb/sc/tage/ras` 有近似映射，`btb` 到 V3 `abtb/mbtb` 不是一一对应，`tlbCsr.priv.debug` 在 V3 已删除。

4. vector partial replay 不能忽略。
   V2 `issueVldu` 和 `vstuIqFeedback` 中的 replay mask/mbIdx 是 vector memory replay 正确性的关键字段。V3 内部仍有相关机制，但顶层位置不同。

5. 顶层连接最终仍以生成后 Verilog 为准。
   本文解释 Scala 语义，不替代 `build/rtl/MemBlock.sv` 与 `build_memblock/rtl/MemBlock.sv` 的端口方向、位宽和存在性判断。
