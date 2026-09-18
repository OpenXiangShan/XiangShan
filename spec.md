# Store Prefetch（从 sbuffer 到 L2 的专用 store 预取）实现规范

本文档描述本次实现的“store 指令提交到 sbuffer 时产生 store 预取请求、经 L1 转发、
在 L2 侧 mini buffer 中合并并在替换时发出”的完整机制。

---

## 1. 背景与目标

### 1.1 现状

* store 数据在 ROB 提交时只写入 **sbuffer**，dcache/L2 直到 sbuffer
  行满 / `force_write` / flush 时才会看到（`Sbuffer.scala` 的 `M_XWR` 通路，
  miss 时以 `reqSource = CPUStoreData` 发 acquire 到 L2）。
* 仓库原有的 store 预取能力（默认全关）：
  * `EnableStorePrefetchAtIssue / AtCommit / SPB`（`Parameters.scala`，默认 `false`）
    从 sbuffer 产生 `StorePrefetchReq`，**重新注入 store 流水线**
    （`StoreUnitS0` 的最低优先级入口），在 L1 dcache 发 `M_PFW`；
  * `EnableStorePrefetchSMS`（默认 `false`）只影响 RS 发射宽度；
    SMS 的 `st_in` 一直训练 store；
  * L2 BOP/VBOP/PBOP 通过 acquire 的 `needHint`（`MissQueue` 的 `PrefetchKey`，
    默认对所有请求为真）对 store 训练，并**自行发出预测地址的预取请求**。

### 1.2 目标

1. store 提交进 sbuffer 时，产生一条 **以块地址为粒度** 的 store 预取请求，
   经 `PrefetcherWrapper` 的 `l2_pf_req` 通道送到 L2 prefetch receiver。
2. 该请求**不能直接进 L2 预取流水线**，必须在 L2 侧经过一个**mini buffer**
   （类似 sbuffer，但只服务预取）按块地址 **merge**，被替换出来时才发出。
   mini buffer 容量小于 sbuffer，因此替换更早、及时性更好；发出的地址就是
   被替换 entry 的块地址（**地址不做任何预测/变换**）。
3. 有了精确的 store 预取后，**BOP/PBOP 对 store 只训练、不发预取请求**。

---

## 2. 总体数据流

```
store commit -> sbuffer enq (Sbuffer.io.in.req(i).fire)
   │  过滤: vecValid && !prefetch(hw 预取 store) && !wline(cbo.zero)
   │         (NC/MMIO 不会进入 sbuffer；其它 CBO 也不写 sbuffer)
   │  生成: 块地址 + 64bit 行 byte mask
   ▼
StorePrefetchToL2Gen (L1, 16 项 FIFO, 2 写 1 读)
   │  Decoupled { addr, mask }
   ▼
PrefetcherWrapper.fromStoreBuffer
   │  l2_pf_arb 的第 prefetcherNum 路（最低优先级）
   │  source = MemReqSource.Prefetch2L2Store, mask 随 L2PrefetchReq 下传
   ▼
l2_pf pipeline (depth 2) -> io.l1_pf_to_l2 (PrefetchRecv{addr, pf_source, mask, ...})
   │  BundleBridge (与 SMS/Stream/Stride/Berti 共用同一通道)
   ▼
L2 Prefetcher: io.recv_addr 延迟 2 拍后按来源分流
   ├─ 非 store (SMS/Stream/Stride/Berti) ──► PrefetchReceiver ──► pftQueue (行为不变)
   └─ Prefetch2L2Store ─────────────────► StorePrefetchBuffer (L2 mini buffer)
                                            4 项 × 64B，按块地址 merge
                                            被替换的块 -> 预取请求
                                            (全 mask 的块直接丢弃，不发请求)
                                            最近 32 个已发射地址 -> FIFO shadow buffer（仅统计）
                                            └──► pftQueue 第 7 路（最低优先级）
                                                 -> pipe -> SinkA(Hint/PREFETCH_WRITE)
                                                 -> MSHR -> L2/L3
```

---

## 3. 接口与数据结构

### 3.1 新增/扩展的 bundle

| Bundle | 位置 | 字段 | 说明 |
| --- | --- | --- | --- |
| `StorePrefetchToL2Req` | `src/main/scala/xiangshan/mem/sbuffer/StorePrefetchToL2.scala` | `addr: UInt(PAddrBits.W)`, `mask: UInt(dcacheParameters.blockBytes.W)` | L1→L2 的 store 预取请求；`addr` 为**块地址**（低 6bit 为 0），`mask` 为 **64bit 行 byte mask** |
| `L2PrefetchReq`（扩展） | `src/main/scala/xiangshan/mem/prefetch/BasePrefecher.scala` | 新增 `mask: UInt(dcacheParameters.blockBytes.W)` | 在 L1 的 `l2_pf_arb`/pipeline 中携带 mask；SMS/Berti/StreamStride 等非 store 来源固定为 0 |
| `PrefetchRecv`（扩展） | `XSCache/src/main/scala/coupledL2/Common.scala` | 新增 `mask: UInt(64.W)` | L1↔L2 BundleBridge 上的 mask；只有 `pf_source = Prefetch2L2Store` 时有意义，其它来源置 0 |
| `PrefetchRecvAddr`（新增） | `XSCache/src/main/scala/coupledL2/Common.scala` | `addr`, `pfSource`, `mask` | L2 内部 receiver 入口 bundle（替换原来的匿名 bundle），供 `PrefetchReceiver` 与 `StorePrefetchBuffer` 共用 |

### 3.2 新增 source id

`utility/src/main/scala/utility/TLUtils/BusKeyField.scala`：

* 新增 `MemReqSource.Prefetch2L2Store = Value("Prefetch2L2Store")`，
  并计入 `MemReqSource.isL2Prefetch`。
* `ReqSourceCount.id` 由 22 增至 23，`reqSourceBits` 仍为 5bit（不影响任何已有编码宽度）。
* L2 内部 `PfSource.fromMemReqSource(Prefetch2L2Store)` 落到 `NoWhere`，
  不参与 `PrefetchController` 的 degree 反馈统计（store 预取不做 L1 degree 反馈）。

---

## 4. L1 侧规格

### 4.1 取样点

`Sbuffer.io.in.req(i).fire`（即 store queue / vSegmentUnit 写入 sbuffer 成功的当拍），
`i ∈ [0, EnsbufferWidth)`。

### 4.2 过滤规则（不产生 store 预取请求）

| 情况 | 判据 | 原因 |
| --- | --- | --- |
| 无效 vector 元素 | `vecValid == false` | 该元素不写内存 |
| 硬件预取 store | `prefetch == true` | 来自 SPB/AtCommit 的注入请求，否则会自激 |
| `cbo.zero` | `wline == true` | 整行写，不需要行内旧数据（`wline` 由 SQ 对 `cbo.zero` 置位） |
| NC / MMIO store | —— | 这类 store 不会写入 sbuffer（SQ 的 `uncacheStall`），天然被排除 |
| 其它 CBO（clean/flush/inval） | —— | 同样不写 sbuffer，天然被排除 |

代码上等价于 `enq.valid && enq.vecValid && !enq.prefetch && !enq.wline`。

> 配套修改：`VSegmentUnit` 对 vector store 显式给
> `sbufferOut.bits.wline := false.B` / `bits.prefetch := false.B`，避免原来
> `DontCare` 参与上述判据（同时消除原有的 X 传播隐患）。

### 4.3 mask 生成

* sbuffer 写请求的 `mask` 是 16bit，语义是 **以地址所在的 16B 对齐窗口为单位**
  （`genVWmask128`：`sizeMask << addr[3:0]`）。
* 行内 16B 窗口序号为 `addr[5:4]`（与 `Sbuffer` 的 `getVWord(pa)=pa[..:4]`
  及 `SbufferData` 的 `vwordOffset(VWordsWidth-1,0)` 一致）。
* 生成规则：

  ```
  lineMask = (storeMask << (addr[5:4] * 16))[63:0]
  ```

  `lineMask` 的 bit *i* 表示该 64B 块的第 *i* 个 byte 被这条 store 写。
* 跨 16B / 跨行的非对齐 store 已由 LSU 拆分（`cross16Byte` / unalign 机制），
  因此单条 sbuffer 写请求的 mask 一定落在同一个 16B 窗口内，上述移位不会溢出。
* 多条 store 落在同一块时，mask 在 L2 侧按位或合并（见 5.3）。

### 4.4 暂存队列（16 项）

`StorePrefetchToL2Gen`：

* 容量 `SIZE = 16`，entry = `{块地址, 64bit mask}`；
* `EnsbufferWidth`（默认 2）个写口、1 个读口，写口之间保持 sbuffer 的入队顺序；
* 读口为 `Decoupled`，直接连到 `PrefetcherWrapper`（低优先级仲裁），因此有背压；
* 队列满且本拍无出队时丢弃新请求（计入 `store_pf_to_l2_drop`）；
* 队列**不做 merge**（merge 只在 L2 侧做），L1 只负责“不丢事件 + 转发”。

### 4.5 出队与转发

`PrefetcherWrapper`：

* 新端口 `fromStoreBuffer: Flipped(DecoupledIO(StorePrefetchToL2Req))`；
* `l2_pf_arb` 宽度由 `prefetcherNum` 扩为 `prefetcherNum + 1`，
  store 请求接在**最后一个输入**（Chisel `Arbiter` 低 index 优先 ⇒ store 优先级最低，
  不会抢 SMS/Stream/Stride/Berti 的 L2 带宽）；
* `bits.source := MemReqSource.Prefetch2L2Store`，`bits.mask` 原样透传；
* `io.l1_pf_to_l2.mask := l2_pf_req.bits.mask`；L3 通道 `mask := 0`；
* L2 预取 trace（ChiselDB `L2PrefetchTrace`）新增 `L2Store` 分类。

> 该通道与 SMS/Stream/Stride/Berti 共用一个 1 拍宽的 pipeline（`depth = 2`），
> 因此 L1 到 L2 的 store 预取稳态带宽 ≤ 1 条/拍。

---

## 5. L2 侧规格

### 5.1 按来源分流（receiver 入口）

`XSCache/src/main/scala/coupledL2/prefetch/Prefetcher.scala`：

* `io.recv_addr` 经 `ValidIODelay(_, 2)` 后（与 `PrefetchReceiver` 原有的
  2 拍接收延迟一致）按 `pfSource` 分流：
  * `pfSource != Prefetch2L2Store` → `PrefetchReceiver`（原路径，直通 `pftQueue`）；
  * `pfSource == Prefetch2L2Store` → `StorePrefetchBuffer`；
* 原 receiver 的 directional assert（只允许 SMS/Stream/Stride/Berti）保持成立，
  因为 store 请求已被分流，不再进入 `PrefetchReceiver`；
* `pfRcv_en = RegNextN(l2_pf_master_en && l2_pf_recv_en, 2)` 同时门控 receiver 与
  mini buffer（含 `Constantin "storePfBuffer_enable<hartId>"`，init=1）。

### 5.2 mini buffer 结构

`XSCache/src/main/scala/coupledL2/prefetch/StorePrefetchBuffer.scala`：

| 部件 | 规格 |
| --- | --- |
| merge buffer | `SIZE = 4` way（等价容量 4 × 64B = 256B）；每 way = `{块地址, 合并 mask}` + valid |
| 输入 FIFO | `IN_FIFO_SIZE = 4`，吸收 L2 预取队列仲裁造成的停顿；满则丢弃（`store_pf_buf_in_drop`） |
| 受害 FIFO | `VICTIM_FIFO_SIZE = 4`，缓存“已替换、待发出”的块地址，使 merge buffer 不必等 `pftQueue` |
| 替换策略 | `ValidPseudoLRU(4)`（与 L1 sbuffer 一致：优先选最老的 valid way） |
| shadow buffer | 32 项、64bit 块地址、FIFO；记录已实际发射（`req.fire`）的 victim 地址，仅用于统计后续 L1 输入是否命中 |
| 吞吐 | 每拍最多 **1 次 alloc**（含 hit-merge、miss-alloc），最多 **1 条**预取请求发出 |

### 5.3 合并规则

* 命中（`entry.addr == in.blockAddr`）：`entry.mask |= in.mask`，刷新 PLRU，
  **不分配新 entry、不发请求**；因此同一个块在 buffer 内只会产生一条请求。
* 未命中：用 PLRU 选 victim 分配新 entry；若 victim 原本 valid，则将其块地址
  作为预取请求发出（“**替换出来 = 发出请求**”，地址保持不变）。

### 5.4 全 mask（整块写完）规则

* 若被替换的 entry 满足 `mask == 64'hFFFF_FFFF_FFFF_FFFF`：
  **不发任何预取请求**，entry 直接失效（丢弃）；计入 `store_pf_buf_full_block_drop`。
* 依据：整块会被 store 完全覆盖，不需要从下级取旧数据；mini buffer 纯粹是预取结构，
  **不涉及任何缓存一致性动作**——不向 L2/L1 发 invalidate/CMO，也不修改 L2 的副本状态。
* 全 mask 的 entry 在被替换之前仍留在 buffer 中继续参与合并，
  这样后续对同一块的 store 不会重新申请 entry、也不会再产生预取请求。

### 5.5 发出请求的语义

发出的请求进入 L2 `pftQueue`，字段如下：

| 字段 | 值 | 说明 |
| --- | --- | --- |
| `tag` / `set` | 由块地址解析 | 地址不变 |
| `needT` | `true` | **store 预取权限**：`SinkA` 中 `task.param = Mux(req.needT, PREFETCH_WRITE, PREFETCH_READ)`，即发 CHI Hint 的 **PREFETCH_WRITE**（写预取），后续 L1 回写需要写权限 |
| `pfSource` | `MemReqSource.Prefetch2L2Store` | 用于统计与 L2 内部 `pfsrc` 归因 |
| `source` | `0` | 与 `PrefetchReceiver` 保持一致的约定（sourceId 0 = dcache） |
| `vaddr` | `0` | store 预取只用物理块地址 |
| `cdpPfDepth` | `0` | 与 CDP 无关 |

### 5.6 仲裁优先级

`Prefetcher.scala` 中预取源序号固定为
`rcv, NL, VBOP, PBOP, TP, CDP, store`（`SRC_NUM = 7`），
`select/selectOH` 使用 `PriorityEncoderOH` ⇒ **store 预取为最低优先级**，
其 `reqsAllowed` 恒为 `true`（受 5.1 的 enable 门控）。

---

## 6. BOP 改动（只训练不发）

`XSCache/src/main/scala/coupledL2/prefetch/BestOffsetPrefetch.scala`：

* 判据：`train.bits.reqsource == MemReqSource.CPUStoreData`（store 的 L1 acquire 来源）；
* **VBestOffsetPrefetch**：`s1_req_valid` 在 `s0_fire` 时增加 `&& !s0_isStoreTrain`，
  即 store train 不会进入 `reqFilter`/`io.req`；
* **PBestOffsetPrefetch**：`req_valid` 增加 `&& !isStoreTrain`；
* 两者的 `scoreTable` / `delayQueue`（offset 学习）**照常接收 store train**，
  即“只训练、不发预取请求”；
* 新增计数 `bop_drop_for_store_train`（VBOP/PBOP 各一份）。
* TP / NL / CDP 不受影响；L1 SMS 的 `st_in` 训练也不变。

---

## 7. 使能、复位与丢弃

| 项目 | 规格 |
| --- | --- |
| CSR | **不新增任何 CSR bit**。L2 侧沿用 `pfCtrlFromCore.l2_pf_master_en && l2_pf_recv_en`（经 2 拍寄存）；L1 侧不做额外门控（`l2_pf_enable` 原本只驱动未被 L2 使用的 `pf_en` 字段） |
| 复位 | 所有队列/valid/PLRU 复位为空 |
| flush | 无 flush 通路：mini buffer 与 L1 队列不响应 sbuffer flush/redirect，entry 只通过替换自然老化（预取本身是 hint，不要求精确性） |
| 丢弃点 | ① L1 16 项队列满（`store_pf_to_l2_drop`）；② L2 输入 FIFO 满（`store_pf_buf_in_drop`） |
| 背压 | L1 队列出队受 `l2_pf_arb` 输入 ready 反压；L2 merge buffer 在“需发受害且受害 FIFO 满”时停住输入 FIFO 的出队（不丢已入队请求） |

---

## 8. 性能计数器

**L1（`StorePrefetchToL2Gen`）**

`store_pf_to_l2_enq_valid`、`store_pf_to_l2_enq_fire`、`store_pf_to_l2_deq`、
`store_pf_to_l2_full`、`store_pf_to_l2_drop`、`store_pf_to_l2_occupancy`

**L2（`StorePrefetchBuffer`）**

`store_pf_buf_in_valid`、`store_pf_buf_in_drop`、`store_pf_buf_alloc`、
`store_pf_buf_merge`、`store_pf_buf_send`、`store_pf_buf_full_block_drop`、
`store_pf_buf_valid_num`、`store_pf_buf_shadow_hit`、
`store_pf_buf_alloc_to_send_latency`（从 mini-buffer entry 分配到首次
`io.req.fire` 的延迟分布，分桶为 `0~128/4`、`128~512/16`、
`512~4096/64`，以及 `>=4096` 的粗粒度桶）

**Sbuffer**

`sbuffer_alloc_to_dcache_latency`（从 Sbuffer line 首次 alloc 到该 entry
第一次 `io.dcache.req.fire` 的延迟分布；同一 entry 后续重试不重复计数。
分桶为 `0~128/4`、`128~512/16`、`512~4096/64`，以及 `>=4096` 的粗粒度桶）

**L2 预取仲裁 / BOP**

`prefetch_req_fromStore`、`prefetch_req_selectStore`、`bop_drop_for_store_train`

**trace**：L1 `L2PrefetchTrace` 新增 `L2Store` 分类（ChiselDB）。

---

## 9. 变更文件清单

### XiangShan（主仓）

| 文件 | 变更 |
| --- | --- |
| `src/main/scala/xiangshan/mem/sbuffer/StorePrefetchToL2.scala` | **新增**：`StorePrefetchToL2Req`、`StorePrefetchToL2Gen`（过滤 + mask 生成 + 16 项队列） |
| `src/main/scala/xiangshan/mem/sbuffer/Sbuffer.scala` | 新增 IO `store_prefetch_to_l2`，例化并连接生成器 |
| `src/main/scala/xiangshan/mem/MemBlock.scala` | `sbuffer.io.store_prefetch_to_l2 <> prefetcher.io.fromStoreBuffer`；L2/L3 sender 增加 `mask` |
| `src/main/scala/xiangshan/mem/prefetch/PrefetcherWrapper.scala` | 新端口 `fromStoreBuffer`；`l2_pf_arb` 扩 1 路（最低优先级）；`l1_pf_to_l2.mask`；trace 分类 |
| `src/main/scala/xiangshan/mem/prefetch/BasePrefecher.scala` | `L2PrefetchReq` 增加 `mask` |
| `src/main/scala/xiangshan/mem/prefetch/SMSPrefetcher.scala` | `l2_req.bits.mask := 0.U` |
| `src/main/scala/xiangshan/mem/prefetch/Berti.scala` | `l2_req.bits.mask := 0.U` |
| `src/main/scala/xiangshan/mem/prefetch/L1PrefetchComponent.scala` | L2/L3 仲裁输入 `mask := 0.U` |
| `src/main/scala/xiangshan/mem/vector/VSegmentUnit.scala` | vector store 的 `wline`/`prefetch` 显式置 `false` |

### XSCache（submodule）

| 文件 | 变更 |
| --- | --- |
| `src/main/scala/coupledL2/Common.scala` | `PrefetchRecv` 增加 `mask`；新增 `PrefetchRecvAddr` |
| `src/main/scala/coupledL2/CoupledL2.scala` | `pf_recv_node` 透传 `mask` |
| `src/main/scala/coupledL2/prefetch/PrefetchReceiver.scala` | 使用 `PrefetchRecvAddr` |
| `src/main/scala/coupledL2/prefetch/Prefetcher.scala` | 按来源分流；例化 mini buffer；`SRC_NUM=7`（store 最低优先级）；计数 |
| `src/main/scala/coupledL2/prefetch/StorePrefetchBuffer.scala` | **新增**：L2 侧 mini buffer |
| `src/main/scala/coupledL2/prefetch/BestOffsetPrefetch.scala` | VBOP/PBOP：store 只训练不发预取请求 |

### utility（submodule）

| 文件 | 变更 |
| --- | --- |
| `src/main/scala/utility/TLUtils/BusKeyField.scala` | 新增 `MemReqSource.Prefetch2L2Store` 并计入 `isL2Prefetch` |

---

## 10. 验证

| 步骤 | 命令 | 结果 |
| --- | --- | --- |
| Scala 编译 | `mill -i xiangshan.compile`、`mill -i XSCache.compile` | 通过 |
| 完整 elaboration（CHIRRTL） | `NOOP_HOME=<dir> make -B sim-chirrtl CONFIG=DefaultConfig` | 通过；生成的 FIRRTL 含 `StorePrefetchBuffer`、`StorePrefetchToL2Gen` |
| Verilog 生成 | `NOOP_HOME=<dir> make -B verilog CONFIG=DefaultConfig JVM_XMX=64G` | 通过；`build/rtl/StorePrefetchBuffer.sv`、`build/rtl/StorePrefetchToL2Gen.sv`、`build/rtl/Sbuffer.sv`、`build/rtl/Prefetcher.sv` 等已生成 |

> 说明：`NOOP_HOME` 仅用于满足 difftest 文件收集的环境依赖（会写
> `$NOOP_HOME/build/generated-src`），与本次改动无关。

---

## 11. 已知限制与后续工作

1. **只在 L2 侧**：store 预取不产生 L3 请求（`l1_pf_to_l3.mask` 固定为 0，mini buffer
   只接 L2 `pftQueue`）。
2. **mask 只用于合并与整块判定**：L2 `PrefetchReq` 不带 mask，因此不会做
   sector 粒度取数、也不影响 L2 的插入/替换策略。
3. **L1 队列不合并**：同一块的多次 store 会在 L1 队列里各占一项，靠 L2 merge 消除；
   若后续想降低 L1→L2 带宽，可在 L1 队列加按块地址合并。
4. **无 CSR 开关**：目前只受 L2 `pf_recv` 使能控制；如需独立开关可加 `spfctl` bit
   与 `Constantin`（mini buffer 已预留 `Constantin` 记录）。
5. **丢弃语义**：L1 队列/L2 输入 FIFO 满时丢弃（靠后续对同一块的 store 补救），
   已分别计数。L2 shadow buffer 只记录最近 32 个实际发射的地址，满时按 FIFO 覆盖最老项，
   不参与主路径背压或丢弃。
6. **`l2_pf_store_only`（`spfctl[17]`，默认 0）未改动**：置 1 时 L2 只对 store 训练，
   而 store 预取已由 mini buffer 产生、BOP 又对 store 不发请求 ⇒ 等价于关闭 BOP
   的预取输出（这是该 bit 与本次改动的已知交互，必要时后续清理）。
7. **L1 侧原有 store 预取路径未动**：`EnableStorePrefetchAtIssue/AtCommit/SPB`
   仍默认关闭；SMS 对 store 的训练保持不变。
