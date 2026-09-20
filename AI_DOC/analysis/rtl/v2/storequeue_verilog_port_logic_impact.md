# V2 StoreQueue Verilog 端口对内部逻辑影响分析

## 版本元数据

| 项目 | 内容 |
|---|---|
| RTL 版本 | V2 |
| 权威端口清单 | `build/rtl/StoreQueue.sv` 顶层端口（共 **798** 个） |
| 语义来源 | `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala`、`StoreQueueData.scala` |
| 配套接口说明 | `AI_DOC/analysis/interface/v2/agents/storequeue_agent.md`（偏字段含义/时序） |
| 本文目标 | **每个 Verilog 端口（按展平模式）→ SQ 内部状态 / 指针 / FSM 的读写影响** |
| 日期 | 2026-09-17 |

> 说明：Chisel Bundle 展平后同构端口很多（如 `exceptionVec_0..23`、`stAddrReadyVec_0..55`）。本文对同构位做“模式级覆盖”，每个模式对应真实 Verilog 端口名；不是省略，而是避免把相同逻辑影响重复写几十次。

---

## 0. 内部状态速查（影响分析的“靶点”）

### 0.1 每 entry 状态（`StoreQueueSize=56`）

| 状态 | 含义 | 主要写来源 |
|---|---|---|
| `allocated` | 槽位占用 | `enq` 置 1；`brqRedirect`/`deq` 清 0 |
| `addrvalid` | 地址有效 | `storeAddrIn`（+ `storeAddrInRe.hasException` OR） |
| `datavalid` | 数据有效 | `storeDataIn` 后一拍 |
| `allvalid` | `addrvalid && datavalid` | wire |
| `committed` | ROB 已提交 | `rob.pendingPtr` 提交游标 |
| `completed` | 可物理释放 | sbuffer/uncache/mmioStout/cboZero/NC+exc |
| `pending` / `mmio` | MMIO 待发 | `storeAddrInRe`；发 uncache 后清 `pending` |
| `nc` | NC 属性 | `storeAddrIn.bits.nc` |
| `hasException` | 异常标记 | `storeAddrInRe` |
| `waitStoreS2` | 等 S2 分类 | enq 置 1；`storeAddrInRe` 清 0 |
| `unaligned` / `cross16Byte` | 非对齐 / 跨 16B | `storeAddrIn`（`!isFrmMisAlignBuf`） |
| `isVec` / `vecLastFlow` / `vecMbCommit` | 向量相关 | enq / `vecFeedback` / `maControl.withSamePtr` |
| `prefetch` / `memBackTypeMM` | 预取提示 / MM 属性 | `storeAddrInRe` |
| `uop` | 指令元数据 | enq 初值；`storeAddrIn` 覆盖 |

### 0.2 指针 / FSM

| 名称 | 角色 |
|---|---|
| `enqPtrExt` | 下一分配尾 |
| `cmtPtrExt` | commit 游标 |
| `rdataPtrExt` | 读出/ensbuffer/uncache 头 |
| `deqPtrExt` | 释放头（`allocated&&completed`） |
| `addrReadyPtrExt` / `dataReadyPtrExt` | RS ready 前沿 |
| `mmioState` | `idle→req→resp→wb→wait` |
| `ncState` | `idle→req→req_ack→(resp\|idle)` |
| `vecExceptionFlag` | 向量异常后同 robIdx 后续 flow 禁写 SB |
| `noPending` | WFI 安全（无 outstanding MMIO/CMO） |

### 0.3 完成（`completed=1`）来源

1. `io_sbuffer_*.fire && sqNeedDeq && !wline`
2. NC `idResp/resp` 触发 `ncDeqTrigger`
3. `io_mmioStout.fire` / `io_vecmmioStout.fire`（后者 Verilog 侧已优化掉，Scala 绑 false）
4. `io_cboZeroStout.fire`
5. commit 时 `nc && hasException`（tombstone，不发 uncache）

---

## 1. 端口组总览（以生成 Verilog 为准）

| 端口组 | 约端口数 | 方向 | 协议 | 对 SQ 的核心影响 |
|---|---:|---|---|---|
| `clock/reset` | 2 | in | - | 时序/复位 |
| `io_enq_req_<0..5>_*` | 216 | in | Valid-only | 分配 entry、初始化 flags、推进 enqPtr |
| `io_brqRedirect_*` | 4 | in | Valid-only | 取消未提交、enqPtr rewind、ready 指针回退 |
| `io_vecFeedback_<0..1>_*` | 32 | in | Valid-only | 置 `vecMbCommit`；FLUSH 进 exceptionBuffer |
| `io_storeAddrIn_<0..1>_*` | 94 | in | Valid-only | 写地址 CAM、`addrvalid/nc/unaligned/cross16`、更新 uop |
| `io_storeAddrInRe_<0..1>_*` | 38 | in | sideband | 写 `pending/mmio/hasException/prefetch`，解除 `waitStoreS2` |
| `io_storeDataIn_<0..1>_*` | 10 | in | Valid-only | 写 dataModule；+1 拍置 `datavalid` |
| `io_storeMaskIn_<0..1>_*` | 6 | in | Valid-only | 写 byte-mask（不改 valid 旗标） |
| `io_forward_<0..2>_*` | 153 | 双向 | 1 拍查询 | 只读 SQ 状态做 CAM/forward；输出 replay 原因 |
| `io_sbuffer_<0..1>_*` | 16 | 双向 | Decoupled | 交付 cacheable store；fire 可置 `completed` |
| `io_rob_*` | 6 | 双向 | sideband | 启动 MMIO；驱动 `committed`/`cmtPtr` |
| `io_uncache_*` + `io_uncacheOutstanding` | 19 | 双向 | Decoupled+事件 | MMIO/NC 请求与完成 |
| `io_cmoOpReq/Resp` | 8 | 双向 | Decoupled | CBO clean/flush/inval |
| `io_cboZeroStout_*` | 30 | 双向 | Decoupled | cbo.zero 回写 ROB；fire 置 `completed` |
| `io_mmioStout_*` | 8 | 双向 | Decoupled | MMIO/CMO WB；fire 置 `completed` |
| `io_exceptionAddr_*` | 5 | out | sideband | 最老异常地址输出 |
| `io_flushSbuffer_*` | 2 | 双向 | sideband | CBO/cbo.zero 前冲刷 SB |
| `io_maControl_*` | 10 | 双向 | sideband | 跨页 ensbuffer / 强制 `vecMbCommit` |
| `io_wfi_*` | 2 | 双向 | sideband | 门控 uncache/CMO；`wfiSafe` |
| status (`sq*/st*/force_write`) | 129 | out | 状态 | 占用、ready 前沿、commit/deq 统计 |
| `io_perf_*` | 8 | out | 性能计数 | 不影响功能逻辑 |

### Scala 有、本生成 Verilog 无的接口（优化/旁路）

| Scala IO | 生成顶层 | 影响 |
|---|---|---|
| `enq.canAccept / needAlloc / lqCanAccept / resp` | **无** | 分配握手在上游完成；SQ 只吃 `enq.req` 镜像 |
| `hartId` / `diffStore` | **无** | Difftest 相关，功能路径无关 |
| `vecmmioStout` | **无** | Scala 绑 `valid=false` |
| `exceptionAddr.vstart/vl` | **无** | Scala 有，本产物未引出 |
| `sqDeqPtr` | **无** | Scala 有 `io.sqDeqPtr`；本产物未引出（有 `sqDeq/sqDeqIsVec`） |
| `sbuffer.bits.prefetch/sqNeedDeq` 等 | 部分未引出 | 内部 `DataBufferEntry` 使用；顶层只见 `vaddr/data/mask/addr/wline/vecValid` |

---

## 2. `io_enq_req_<d=0..5>_*`：Dispatch 分配写入

协议：**Valid-only**。`valid=1` 时按 `sqIdx .. sqIdx+numLsElem` 范围写多个 entry。

| Verilog 端口模式 | 方向/宽 | 对 SQ 逻辑影响 |
|---|---|---|
| `io_enq_req_<d>_valid` | in/1 | 本拍是否采样该 dispatch 槽。同拍被 redirect kill 则不形成 live entry。 |
| `..._bits_sqIdx_{flag,value}` | in/1+6 | **写入口索引下界**。决定哪些物理槽 `allocated:=1`。 |
| `..._bits_numLsElem[4:0]` | in/5 | 向量 flow 数；决定写入口范围与 `enqPtr` 增量。 |
| `..._bits_robIdx_{flag,value}` | in/1+8 | 写入 `uop.robIdx`；后续 commit/redirect/cancel 比较依据。 |
| `..._bits_uopIdx[6:0]` | in/7 | 写入 `uop.uopIdx`；向量 feedback/异常 oldest 选择用。 |
| `..._bits_lastUop` | in/1 | 与 `numLsElem` 上界一起决定 `vecLastFlow`。 |
| `..._bits_fuType[34:0]` | in/35 | `FuType.isVStore` → `isVec`。 |
| `..._bits_fuOpType[8:0]` | in/9 | 存入 `uop`；后续 CBO/cbo.zero 判定用。 |
| `..._bits_exceptionVec_<0..23>` | in/1×24 | 存入 `uop.exceptionVec` 初值（后续 STA/uncache 可覆盖相关位）。 |
| `..._bits_trigger[3:0]` | in/4 | 存入 `uop.trigger`。 |
| `..._bits_flushPipe` | in/1 | 存入 `uop.flushPipe` metadata；**不直接触发 SQ flush**。 |

**同拍副作用（`entryCanEnq` 为真时）**：

- `allocated=1`
- 清：`completed/datavalid/addrvalid/unaligned/cross16Byte/committed/pending/prefetch/nc/mmio/vecMbCommit/hasException`
- 置：`waitStoreS2=1`，`isVec`，`vecLastFlow`（仅范围上界且 `lastUop`）

**指针**：`enqPtrExt += Σ validVStoreFlow`（redirect 下一拍会抑制）。

---

## 3. `io_brqRedirect_*`：Redirect / 恢复

| 端口 | 方向/宽 | 影响 |
|---|---|---|
| `io_brqRedirect_valid` | in/1 | 启动 cancel/指针恢复。 |
| `io_brqRedirect_bits_robIdx_{flag,value}` | in/1+8 | cancel 边界。 |
| `io_brqRedirect_bits_level` | in/1 | `needFlush` 语义：0=flush-after，1=含边界自身。 |

**状态影响**：

```text
needCancel(i) =
  allocated(i) && !committed(i) &&
  Mux(vecExceptionFlag.valid,
      isAfter(uop(i).robIdx, redirect.robIdx) && valid,
      uop(i).robIdx.needFlush(redirect))

when needCancel: allocated:=0; completed:=0
```

**指针影响**：

- `enqPtr`：按 cancel 数 **延迟 2 拍 rewind**
- `addrReadyPtrExt` / `dataReadyPtrExt`：立刻 snap 到 `max(cmtPtr, deqPtrNext)`
- `sqCancelCnt` 输出 cancel 计数
- 同拍/下一拍抑制新 enq 的 flow 计数

`exceptionBuffer` 也吃同一 redirect，冲刷其中未提交异常候选。

---

## 4. `io_vecFeedback_<j=0..1>_*`：向量 MergeBuffer 反馈

| 端口模式 | 影响 |
|---|---|
| `..._valid` | 本拍是否有 feedback。 |
| `..._bits_robidx_{flag,value}` + `..._uopidx` | 匹配 `uop(i).robIdx/uopIdx`。 |
| `..._bits_feedback_<k>` | `isCommit/isFlush`：匹配且 allocated → `vecMbCommit(i)=1`。FLUSH 还进 exceptionBuffer。 |
| `..._bits_vaddr/vaNeedExt/gpaddr/isForVSnonLeafPTE` | FLUSH 异常地址侧带。 |
| `..._bits_exceptionVec_*` | FLUSH 异常向量写入 exceptionBuffer。 |

**逻辑后果**：

- 向量 store 的 `committed` 还需 `vecMbCommit`
- ensbuffer 对向量入口看 `vecMbCommit` 而非单纯 `allvalid`
- addr/data ready 前沿对向量可用 `vecMbCommit` 放宽

---

## 5. `io_storeAddrIn_<p=0..1>_*`：STA S1 地址回写

索引：`stWbIndex = bits.uop.sqIdx.value`（生成顶层**无 sqIdx.flag**）。

| 端口模式 | 影响 |
|---|---|
| `..._valid` | Valid-only 事件；真正写状态还需下方限定。 |
| `..._bits_uop_sqIdx_value[5:0]` | 目标槽。 |
| `..._bits_updateAddrValid` | 与 `!miss` 一起决定是否置 `addrvalid`。 |
| `..._bits_miss` | `1`：不写 CAM、不置 `addrvalid`；S2 可用其置 `prefetch`。 |
| `..._bits_nc` | `addrvalid` 路径上写 `nc(stWbIndex)`。 |
| `..._bits_paddr/vaddr/mask/wlineflag` | 写 `paddrModule/vaddrModule`（`!miss && !isFrmMisAlignBuf` 时）。 |
| `..._bits_isMisalign` | `!isFrmMisAlignBuf` 时写 `unaligned`。 |
| `..._bits_misalignWith16Byte` | `unaligned && !misalignWith16Byte` → `cross16Byte`。 |
| `..._bits_isFrmMisAlignBuf` | `1`：不改 unaligned/cross16，也不走普通 CAM 写分支（保持 MAB 再入前状态）。 |
| `..._bits_uop_*`（exceptionVec/trigger/fuOpType/uopIdx/robIdx） | `fire` 时覆盖 `uop(stWbIndex)`。 |
| `..._bits_fullva/vaNeedExt/gpaddr/isHyper/isForVSnonLeafPTE/isvec` | 主要喂 exceptionBuffer（`fire && !miss && !isvec`）。 |

**直接状态写**：

- `addrvalid := 1`（`fire && updateAddrValid && !miss`）
- `nc := bits.nc`
- `unaligned/cross16Byte`（见上）
- `uop` 覆盖

---

## 6. `io_storeAddrInRe_<p=0..1>_*`：STA S2 分类 sideband

无独立 `valid`。有效条件（Scala）：

```text
storeAddrInFireReg =
  RegNext(storeAddrIn(p).fire && !miss) && storeAddrInRe(p).updateAddrValid
```

| 端口模式 | 影响 |
|---|---|
| `..._updateAddrValid` | 打开 S2 写使能。 |
| `..._mmio` | `pending:=mmio`，`mmio:=mmio`。 |
| `..._memBackTypeMM` | 写 `memBackTypeMM`（CMO 使能/diff 分类）。 |
| `..._hasException` | `hasException:=1`；`addrvalid \|= 1`（异常也要 ready，避免卡死）。 |
| `..._af` | 进 exceptionBuffer 时改写 `exceptionVec(storeAccessFault)`。 |
| `..._miss`（Scala 有；本生成顶层**未引出**，由内部 RegNext 路径用上一拍 miss） | 置 `prefetch`。本产物中 `prefetch` 仍由内部 `storeAddrInRe.miss` 语义驱动；若 adapter 看不到该脚，需在上层复现。 |
| `..._uop_robIdx/uopIdx/exceptionVec_*` + `fullva/gpaddr/.../isvec` | 再入 exceptionBuffer（AF 等）。 |

**关键清除**：`waitStoreS2 := 0` → 标量才允许进入 commit 判定。

---

## 7. `io_storeDataIn_<p=0..1>_*` / `io_storeMaskIn_<p=0..1>_*`

### 数据

| 端口 | 影响 |
|---|---|
| `..._valid` | 启动 dataModule 写。 |
| `..._bits_uop_sqIdx_value` | 写地址。 |
| `..._bits_uop_fuType` | 判向量 vs 标量数据整形。 |
| `..._bits_uop_fuOpType` | `cbo_zero` → 写 0；标量 `genVWdata`。 |
| `..._bits_data[127:0]` | 写入 dataModule。 |

**`datavalid`**：`RegNext(fire) && allocated(lastIndex) → datavalid=1`（2 拍写路径的 s1）。

### 掩码

| 端口 | 影响 |
|---|---|
| `..._valid` | 写 mask。 |
| `..._bits_sqIdx_value` | 目标槽。 |
| `..._bits_mask[15:0]` | byte-enable；**不改** `datavalid/addrvalid`。 |

---

## 8. `io_forward_<q=0..2>_*`：Store-to-Load Forward

**只读查询**，不改 SQ entry 状态；结果依赖 `allocated/addrvalid/datavalid/unaligned` + 地址 CAM。

### 输入

| 端口模式 | 影响 |
|---|---|
| `..._valid` | 查询有效。 |
| `..._vaddr[49:0]` / `..._paddr[47:0]` | VAddr/PAddr CAM 查询键。 |
| `..._mask[15:0]` | 与 store mask 重叠判断。 |
| `..._sqIdx_{flag,value}` + `..._sqIdxMask[55:0]` | 年龄窗口：只看比 load 更老、且仍在 SQ 的 store。 |
| `..._uop_sqIdx_*` | 部分年龄/匹配辅助（与 `sqIdx` 配套）。 |
| `..._uop_loadWaitBit/loadWaitStrict/waitForRobIdx_*` | `addrInvalid` 判定（SSID/wait 语义）。 |

### 输出（逻辑含义）

| 端口模式 | 含义/影响下游 |
|---|---|
| `..._forwardMask_<n=0..15>` | byte n 命中可前递。 |
| `..._forwardData_<n>[7:0]` | 对应前递数据（仅 mask=1 有意义）。 |
| `..._dataInvalid` | 地址命中但缺 data，或窗口内存在 `unaligned&&allocated` → load 需 replay。 |
| `..._addrInvalid` | waitBit/SSID 指示应等的更老 store 尚无 `addrvalid`。 |
| `..._matchInvalid` | vaddr CAM 与 paddr CAM 不一致。 |
| `..._*InvalidSqIdx_*` | 指出阻塞者 SQ 索引，供 replay 唤醒。 |

> 注：本生成顶层未见 `forwardMaskFast`；以实际端口为准。

---

## 9. `io_sbuffer_<s=0..1>_*`：向 SBuffer 交付

| 端口 | 影响 |
|---|---|
| `..._valid` | 来自内部 `dataBuffer.deq.valid`。 |
| `..._ready` | 回灌 `dataBuffer.deq.ready`；`fire` 才完成交付。 |
| `..._bits_addr/vaddr/data/mask` | 交付载荷（可能是 cross16 拆分后的半段）。 |
| `..._bits_wline` | cbo.zero 整行写标记；**`wline=1` 时 fire 不置 `completed`**（改走 `cboZeroStout`）。 |
| `..._bits_vecValid` | `0` 表示异常/向量异常旗标路径：仍可推进 SQ 释放相关逻辑载体，但不应对 SB 产生有效写语义。 |

**内部前提（ensbuffer enq → dataBuffer）**受这些入口状态门控：

- `allocated && committed`
- 标量：`allvalid || hasException`；向量：`vecMbCommit`
- `!mmioStall && !ncStall`
- 非对齐/cross16/cross4K + `maControl` 条件

**`completed`**：`fire && sqNeedDeq && !wline`（`sqNeedDeq` 在内部 entry；cross16 仅第二拍为 1）。

---

## 10. `io_rob_*`：ROB sideband

| 端口 | 影响 |
|---|---|
| `io_rob_pendingst` | 与 `pendingPtr`、head entry 状态一起，**延迟一拍**启动 `mmioState: idle→req`。 |
| `io_rob_pendingPtr_{flag,value}` | 1) MMIO 启动对齐；2) commit 条件 `robIdx <= RegNext(pendingPtr)`。 |
| `io_rob_scommit[3:0]` | 寄存为 `scommit`；`mmioState==s_wait` 时 `scommit>0` → 回 `idle`；也参与 commit 门控。 |
| `io_rob_storeMmio` | 输出：正在发 **MMIO（nc=0）** uncache req。 |
| `io_rob_storeMmioUop_robIdx_value` | 正在处理的 MMIO uop 的 robIdx.value（无 flag）。 |

**commit 写 `committed`**（游标 `cmtPtrExt`）：

```text
allocated && robIdx <= RegNext(pendingPtr) && !needCancel
&& (!waitStoreS2 || isVec)
&& (isVec ? vecMbCommit : true)
&& (i==0 ? mmioState∈{idle, wait&scommit>0} : 前缀连续)
```

附加：`isCommit && nc && hasException → completed=1`（NC 异常 tombstone）。

---

## 11. Uncache / NC / Outstanding

### `io_uncacheOutstanding`

| 端口 | 影响 |
|---|---|
| `io_uncacheOutstanding` | NC 在 `idResp` 后：`1`→直接 `nc_idle` 并可完成；`0`→进 `nc_resp` 等最终 resp。 |

### `io_uncache_req_*`（SQ→下游）

| 端口 | 影响 |
|---|---|
| `req_valid/ready` | MMIO 优先于 NC 仲裁；`fire` 分别产生 `mmioDoReq` / `ncDoReq`。 |
| `bits_nc` | `0` MMIO，`1` NC。 |
| `bits_addr/vaddr/data/mask/id/robIdx_*/memBackTypeMM` | 请求载荷；`id` 常用 `rdataPtr` 槽号。 |

副作用：

- `mmioDoReq`：`pending(deqPtr)=0`，`noPending=0`，`mmioState→resp`
- `ncDoReq`：`ncState→req_ack`

### `io_uncache_idResp_*`

| 端口 | 影响 |
|---|---|
| `idResp_valid` + `bits_nc` | `ncSlaveAck`；配合 outstanding 决定是否完成。 |
| `bits_mid` | NC 完成指针 `ncPtr`（outstanding 模式）。 |

### `io_uncache_resp_*`

| 端口 | 影响 |
|---|---|
| `resp_valid` + `bits_nc` | 区分 MMIO/NC 响应。 |
| `bits_denied` | 置 `storeAccessFault`，MMIO 进 `s_wb`。 |
| `bits_corrupt` | 非 denied 时置 `hardwareError`。 |

NC 完成：`ncDeqTrigger → completed(ncPtr)=1`。

---

## 12. CMO / CBO.zero / MMIO writeback / flush

### `io_cmoOpReq_*` / `io_cmoOpResp_*`

| 端口 | 影响 |
|---|---|
| `cmoOpReq_valid/ready/opcode/address` | `deqCanDoCbo && cboFlushedSb && mmioState==req && !wfiReq` 时发 CMO。 |
| `cmoOpResp_ready/valid/denied/corrupt` | `s_resp` 收响应；denied/corrupt 改 `uncacheUop.exceptionVec`；成功 → `s_wb`，`noPending=1`。 |

`deqCanDoCbo` 依赖：head 是 CBO、`allocated && addrvalid && !hasException && memBackTypeMM`。

### `io_flushSbuffer_*`

| 端口 | 影响 |
|---|---|
| `flushSbuffer_valid` | CBO 发请求前冲刷，或 cbo.zero 写入 SB 后冲刷。 |
| `flushSbuffer_empty` | 冲刷完成：置 `cboFlushedSb` / 解除 `cboZeroWaitFlushSb`。 |

### `io_mmioStout_*`

| 端口 | 影响 |
|---|---|
| `valid/ready` | `mmioState==s_wb && !isVec(deqPtr)`；fire → `completed(deqPtr)=1`，并进 exceptionBuffer.last。 |
| `bits_uop_exceptionVec_*` / `flushPipe` / `robIdx_*` / `debug_isMMIO` | 回写 ROB；CMO 时 `flushPipe=1`。 |

### `io_cboZeroStout_*`

| 端口 | 影响 |
|---|---|
| `valid/ready` | SB 接受 wline 且 flush 完成后回写；fire → `completed(cboZeroSqIdx)=1`。 |
| `bits_uop_*` | 回写 ROB 的 uop/异常/trigger/robIdx。 |

---

## 13. `io_exceptionAddr_*`

| 端口 | 影响 |
|---|---|
| `vaddr/vaNeedExt/isHyper/gpaddr/isForVSnonLeafPTE` | 只读输出：`StoreExceptionBuffer` 当前最老异常地址。 |

输入来源（内部，非本端口）：`storeAddrIn` S1、`storeAddrInRe` AF、`vecFeedback` FLUSH、`mmioStout.fire`。

---

## 14. `io_maControl_*`：与 MisalignBuffer 协作

### MAB → SQ

| 端口 | 影响 |
|---|---|
| `toStoreQueue_crossPageWithHit` | 当前 `rdataPtr` 命中 MAB 跨页项。 |
| `toStoreQueue_crossPageCanDeq` | 允许跨页高半段 ensbuffer。 |
| `toStoreQueue_paddr` | 高页 paddr，写入 dataBuffer 第二拍 `addr`。 |
| `toStoreQueue_withSamePtr` | **强制** `vecMbCommit(rdataPtr)=1`。 |

### SQ → MAB

| 端口 | 影响 |
|---|---|
| `toStoreMisalignBuffer_sqPtr_*` | 持续输出 `rdataPtrExt(0)`。 |
| `..._doDeq` | `crossPageWithHit && crossPageCanDeq && dataBuffer.enq(0).fire`。 |
| `..._uop_*` | 当前头 uop 身份。 |

---

## 15. `io_wfi_*`

| 端口 | 影响 |
|---|---|
| `wfi_wfiReq` | 门控：`mmioReq/ncReq/cmoOpReq` 的 valid。 |
| `wfi_wfiSafe` | `RegNext(noPending && wfiReq)`；`noPending` 在 MMIO/CMO outstanding 期间为 0。 |

---

## 16. Status 输出（逻辑只读投影）

| 端口模式 | 来源逻辑 |
|---|---|
| `io_sqEmpty` | 无 live allocated（enq/deq 关系）。 |
| `io_sqFull` | 占用到不可再接受 dispatch 宽度。 |
| `io_stIssuePtr_{flag,value}` | `enqPtrExt(0)`。 |
| `io_stAddrReadySqPtr_*` | `addrReadyPtrExt`：第一个不满足 `allocated && (mmio\|\|addrvalid\|\|vecMbCommit)` 的前沿。 |
| `io_stAddrReadyVec_<0..55>` | 每槽 addr-ready 位（RegNext）。 |
| `io_stDataReadySqPtr_*` | `dataReadyPtrExt`：`addrvalid&&(mmio\|\|datavalid)\|\|vecMbCommit` 且 `!unaligned`。 |
| `io_stDataReadyVec_<0..55>` | 每槽 data-ready。 |
| `io_sqCommitPtr_*` / `sqCommitUopIdx` / `sqCommitRobIdx_*` | `cmtPtrExt(0)` 及该槽 uop。 |
| `io_sqCancelCnt[5:0]` | redirect cancel 计数。 |
| `io_sqDeq[1:0]` | 本拍释放条数 `sqDeqCnt`。 |
| `io_sqDeqIsVec` | `isVec(deqPtr)`。 |
| `io_force_write` | 占用水位滞回，逼迫向 SB 推进。 |
| `io_perf_<0..7>_value` | 性能计数，不影响功能。 |

---

## 17. 端口 → 状态影响矩阵（压缩）

| 端口组 | allocated | addrvalid | datavalid | committed | completed | pending/mmio/nc | unaligned/cross16 | vecMbCommit | 指针/FSM |
|---|---|---|---|---|---|---|---|---|---|
| enq | W1 | W0 | W0 | W0 | W0 | W0 | W0 | W0 | enqPtr++ |
| brqRedirect | W0(cancel) | - | - | - | W0 | - | - | - | enq rewind; ready snap |
| storeAddrIn | - | W1 | - | - | - | nc W | W | - | - |
| storeAddrInRe | - | OR1(exc) | - | - | - | pending/mmio W | - | - | waitS2↓ |
| storeDataIn | - | - | W1(+1) | - | - | - | - | - | - |
| storeMaskIn | - | - | - | - | - | - | - | - | - |
| vecFeedback | - | - | - | - | - | - | - | W1 | - |
| rob | - | - | - | W1 | W1(nc+exc) | - | - | - | cmtPtr; mmioFSM |
| uncache | - | - | - | - | W1(nc) | pending↓ | - | - | mmio/nc FSM |
| sbuffer | - | - | - | - | W1(!wline) | - | - | - | rdata/deq |
| mmio/cboZero stout | - | - | - | - | W1 | - | - | - | mmioFSM |
| cmo/flush/wfi | - | - | - | - | - | - | - | - | mmioFSM/noPending |
| maControl | - | - | - | - | - | - | 影响拆分 | W1(samePtr) | ensbuffer |
| forward | R | R | R | - | - | - | R(blocker) | - | - |
| status/exceptionAddr | R投影 | R | R | R | R | R | R | R | R |

`W1/W0`=写 1/0；`R`=只读；`-`=不直接改。

---

## 18. 主通路时序（便于把端口串起来）

```text
enq.req
  → allocated / waitStoreS2 / isVec
storeAddrIn (S1)
  → addr CAM, addrvalid, nc, unaligned/cross16, uop
storeAddrInRe (S2)
  → mmio/pending/hasException/prefetch, waitStoreS2=0
storeDataIn / storeMaskIn
  → data/mask, datavalid(+1)
rob.pendingPtr (+ vecFeedback for vec)
  → committed / cmtPtr
  ├─ cacheable: ensbuffer → sbuffer.fire → completed → deq
  ├─ mmio: mmioFSM → uncache(nc=0) → mmioStout → completed → deq
  ├─ nc: ncFSM → uncache(nc=1) → idResp/resp → completed → deq
  └─ nc+exc @commit: completed tombstone → deq
brqRedirect
  → cancel !committed, rewind enqPtr, snap ready ptrs
```

---

## 19. 与 `storequeue_agent.md` 的关系

| 文档 | 侧重点 |
|---|---|
| `interface/v2/agents/storequeue_agent.md` | 端口存在性、位宽、时序契约、driver/RM 约束 |
| **本文** | 端口对 **SQ 内部状态机/指针/完成条件** 的因果影响 |

二者应一起使用：agent 文档回答“怎么合法驱动”，本文回答“驱动后 SQ 里面会发生什么”。
