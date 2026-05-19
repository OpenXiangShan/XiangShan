# Frontend InstrUncache 边界行为说明

本文记录 frontend 指令 uncache fetch 相关的边界行为。内容基于以下
Chisel 源码：

- `src/main/scala/xiangshan/frontend/instruncache/`
- `src/main/scala/xiangshan/frontend/ifu/IfuUncacheUnit.scala`
- `src/main/scala/xiangshan/frontend/ifu/Ifu.scala`

构造 MMIO / non-cacheable frontend 用例，或者调试 bin-trace mismatch 时，
先用本文作为检查清单。

## 源码位置

- `instruncache/Parameters.scala`
  - `MmioBusWidth = 64`
  - `MmioBusBytes = 8`
  - `nMmioEntry = 1`
- `instruncache/Bundles.scala`
  - `InstrUncacheReq.addr`
  - `InstrUncacheReq.memBackTypeMM`
  - `InstrUncacheReq.memPageTypeNC`
  - `InstrUncacheResp.data`
  - `InstrUncacheResp.corrupt`
  - `InstrUncacheResp.denied`
  - `InstrUncacheResp.incomplete`
- `instruncache/InstrUncacheEntry.scala`
  - 单个 entry 处理一个 uncache request
  - 负责 TileLink Get、返回数据抽取、resend、`incomplete`
- `instruncache/InstrUncacheImp.scala`
  - 分配 entry 并仲裁 response
- `ifu/IfuUncacheUnit.scala`
  - 串行化 IFU 请求/响应处理和 MMIO commit 顺序
- `ifu/Ifu.scala`
  - 消费 uncache data，处理跨页拼接、RVC 展开、redirect/writeback

## 地址单位

`InstrUncacheReq.addr` 是 pruned physical address 类型。在当前生成 RTL 中，
`InstrUncacheEntry` 将它保存为 `reqReg_addr_addr`。`incomplete` 判断使用的是
半字地址单位：

```verilog
assign io_resp_bits_incomplete = &(reqReg_addr_addr[10:0]);
```

例如 byte PC 为 `0x10002ffe` 时，对应 halfword address 是：

```text
0x10002ffe >> 1 = 0x80017ff
```

`0x80017ff` 的低 11 位全为 1，所以 uncache response 会标记
`incomplete`。

调试时要区分三类地址：

- byte PC
- halfword request address
- bus-aligned TileLink address

它们不是同一个量。

## 8B Bus Beat 边界

InstrUncache 从默认 8B 对齐的 MMIO bus 中取足够的数据，用来形成一个
32-bit 指令窗口。

Chisel 中的关键逻辑：

```scala
alignedAddr = reqReg.addr(reqReg.addr.getWidth - 1, log2Ceil(MmioBusBytes))
crossBusBoundary = reqReg.addr(log2Ceil(MmioBusBytes) - 1, 1).andR
```

在 `MmioBusBytes = 8` 时，`crossBusBoundary` 表示请求 halfword 位于一个
8B bus beat 的最后一个 2B 槽位。换成 byte address 理解，就是从该地址开始
取 4B 指令窗口会跨过 8B bus beat 边界。

如果返回的低 2B 解码为 RVC，则指令已经完整，不需要 resend。如果它是 32-bit
指令，并且没有跨页，则 entry 会 resend，继续取下一个 bus beat。

## 4KB Page 边界

InstrUncache 还会计算 page 边界：

```scala
crossPageBoundary = reqReg.addr(PageOffsetWidth - 1, 1).andR
io.resp.bits.incomplete := crossPageBoundary
```

这是一个保守标记：当指令窗口从一个 page 的最后一个 halfword 开始时，如果该
指令是 32-bit，它需要访问下一页。由于下一页物理地址未必连续，entry 不会
自行跨页发第二次请求。

重要结论：

- RVC 位于 page 最后一个 halfword 时，仍然会产生
  `resp.bits.incomplete = 1`。
- IFU 后续会结合实际指令长度和异常状态处理这个标记。
- RVC 放在 `...2ffe` 这类地址时，`incomplete` 拉高是预期行为，本身不是 bug。

## Resend 条件

`InstrUncacheEntry` 中 resend 由以下条件控制：

```scala
needResend =
  crossBusBoundary &&
  !crossPageBoundary &&
  !respCorrupt &&
  !respIsRvc &&
  !resending
```

以下情况不会 resend：

- 指令没有跨 8B bus beat
- 指令是 RVC
- response corrupt
- 指令跨 page
- 当前已经处于 resend 流程

resend 时，entry 保留第一次 response 的低半部分，并用第二次 grant 填充高半部分。

注意：当前 RTL 的 `needResend` 不检查 `denied`。因此跨 8B beat 的 32-bit
指令第一拍如果 `denied = 1`、`corrupt = 0`，entry 仍会继续发第二拍请求；
第二拍正常返回时，最终送到 IFU 的 response 以第二拍的 `denied/corrupt` 为准。
这和第一拍 `corrupt = 1` 会立即抑制 resend 不同。

## Corrupt / Denied 响应

TileLink 的 `corrupt` 和 `denied` 会被捕获到 uncache response，并在
`IfuUncacheUnit` 中转换成 frontend exception：

```scala
ExceptionType.fromTileLink(fromUncache.bits.corrupt, fromUncache.bits.denied)
```

如果第一次 grant 已经 corrupt，即使地址本来需要 resend，entry 也不会 resend。
IFU 会将 bus exception 作为主导结果。当前实现中，第一拍 denied 不抑制 resend；
如果第二拍正常，第一拍 denied 不会在最终 response 中形成 frontend exception。

## Flush 边界

每个 entry 维护 `needFlush`。

- flush 到来时，如果 entry 正在处理请求且尚未进入 `SendResp`，则设置
  `needFlush`。
- entry 到达 `SendResp` 后，如果 `needFlush` 为真，则 suppress response valid，
  并回到 `Invalid`。

这意味着 flush 后，pending 的 TileLink transaction 仍可能在内部完成，但 stale
frontend response 会被丢弃。

## WFI 边界

InstrUncache 在 WFI pending 时阻止新的 TileLink Acquire：

```scala
io.mmioAcquire.valid := state === RefillReq && !io.wfi.wfiReq
```

当没有等待 L2 response 时，它报告 WFI-safe：

```scala
io.wfi.wfiSafe := state =/= RefillResp
```

wrapper 层面要求所有 entry 都 safe 才 safe。当前 `nMmioEntry = 1`，所以等价于
单个 entry 的状态。

## Entry 数量和顺序

`nMmioEntry = 1`。`Parameters.scala` 中的注释指出 MMIO region 不能 speculative
fetch，因此多个 MMIO entry 预期没有实际意义。

实际影响：

- 同一时间只有一个 MMIO instruction fetch outstanding
- 只有单个 entry 处于 `Invalid` 时 request 才 ready
- response arbiter 存在，但当前只仲裁一个 entry

## IFU MMIO Commit 顺序

`IfuUncacheUnit` 使用以下 FSM 状态串行化 MMIO 请求：

- `Idle`
- `WaitLastCommit`
- `SendReq`
- `WaitResp`

对于标记为 `isMmio` 的请求，发送前需要等待，除非满足以下任一条件：

- 当前是第一条指令
- 观察到 `mmioCommitRead.mmioLastCommit`

这用于避免 MMIO 指令取指相对更老的 MMIO commit 顺序过度超前。

## IFU Stall 边界

`IfuUncacheUnit` 只有在 IFU 未 stall 时才发起 uncache request：

```scala
toUncache.valid := state === SendReq && !ifuStall
```

response 侧始终 ready：

```scala
fromUncache.ready := true.B
```

因此 request issue 会受到 IBuffer ready/backpressure 影响；request 发出后，
response 一旦返回就会被消费。

## IFU 中的跨页拼接

`Ifu.scala` 维护以下状态：

- `prevUncacheCrossPage`
- `prevUncacheData`
- `uncachePc`
- `uncacheCrossPageCheck`

最终用于解码的数据为：

```scala
s3_uncacheData =
  Mux(prevUncacheCrossPage, Cat(uncacheData(15, 0), prevUncacheData), uncacheData)
```

所以跨页指令不会从单个 uncache response 中完整消费。第一次 response 标记
cross-page/incomplete 并保存低半部分，后续 response 提供下一半。

## Cross-Page Fault 边界

IFU 计算：

```scala
uncacheCheckFault =
  uncacheCrossPage && !uncacheCrossPageCheck && uncacheUnit.io.resp.valid
```

如果 frontend 看到 incomplete uncache response，但预期的跨页序列检查不匹配，
则进入 check fault 路径。此时：

- `uncacheRedirect.instrCount` 为 `0`
- 仍然可能发出 redirect/writeback 来恢复控制流
- 该指令不会作为正常完成的 uncache instruction 计数

## RVC 边界

IFU 在组装 `s3_uncacheData` 后判断是否为 RVC：

```scala
uncacheIsRvc = s3_uncacheData(1, 0) =/= "b11".U
```

RVC 情况：

- target 为 `startVAddr + 2`
- `instrEndOffset` 为 `0`
- 除非 RVC expander 标记 illegal，否则使用 RVC expander 输出

32-bit 指令情况：

- target 为 `startVAddr + 4`
- `instrEndOffset` 为 `1`
- 如果跨 bus beat 且不跨 page，entry 可能 resend

## IBuffer 可见性

uncache 场景中，IFU 有意只 enqueue 一条指令：

```scala
io.toIBuffer.bits.valid     := s3_alignBlockStartPos.asUInt
io.toIBuffer.bits.enqEnable := s3_alignBlockStartPos.asUInt
```

这不同于普通 cache fetch。普通路径可能从一个 fetch block 中 compact/enqueue
多条指令。

## Redirect 和 FTQ Writeback

uncache fetch 会产生 writeback/redirect 路径：

```scala
uncacheFlushWb.valid :=
  s3_valid && io.toIBuffer.ready && s3_reqIsUncache &&
  !backendRedirect && (s3_uncacheCanGo || uncacheCheckFault)
```

target 规则：

- RVC、previous cross-page、check fault：`startVAddr + 2`
- 普通 32-bit uncache instruction：`startVAddr + 4`

`uncacheRedirect.instrCount` 在 check fault 时为 `0`，其他情况下为 `1`。

## 验证环境要求

frontend Python uncache agent 必须匹配 RTL contract：

- 返回数据应从请求 uncache address 开始，而不是总是从所在 32B frontend fetch
  block 起点开始。
- 返回数据需要包含足够的后续字节，用于形成 32-bit 指令窗口。
- 测试使用的地址范围必须被建模为 MMIO/non-cacheable fetch range。
- 测试 exception 路径时要保留 `corrupt` / `denied` 语义。

定向用例建议：

- byte offset `...xxx6` 可覆盖 8B bus-beat resend 的 32-bit 指令场景。
- byte offset `...xffe` 可覆盖 page-boundary `incomplete`。
- RVC 位于 `...xffe` 时仍应设置 `incomplete`；IFU 应把它解析为合法 2B 指令路径，
  不应直接当成错误。
- 32-bit 指令位于 `...xffe` 时需要跨页处理，行为还取决于下一页的 exception 情况。
- 第一拍 corrupt 应 suppress resend 并产生 `hwe`。
- 第一拍 denied 当前不会 suppress resend；第二拍正常时不产生 `af`。
- `RefillReq` / `RefillResp` 中 flush 应丢弃最终 response。
- `RefillReq` 中 WFI 应延迟 Acquire；`RefillResp` 中 WFI 不应 safe。

### `.S` 和端口 directed 的边界

不要把所有 uncache 边界都强行构造成 `.S`：

- 指令地址/指令宽度相关场景适合 `.S` 或 bin，例如 8B bus beat、RVC/RVI
  offset matrix、32B fetch block、page near-tail。
- `corrupt` / `denied`、WFI、uncache `a_ready` backpressure、response 延迟后
  flush、IBuffer/backend backpressure 这类场景应使用 Python env 直接控制端口
  或 agent 行为，不需要生成 `.S`。
- 当前 `...xffe` 取指路径有独立风险时，通过型 case 应先避开真实执行
  `...xffe` 起点；该类 incomplete 行为单独用端口/waveform directed case 分析。

端口 directed 覆盖建议：

- 用 `UncacheAgent.set_a_ready(0/None)` 制造 uncache A 通道 backpressure。
- 用 `UncacheAgent.inject_next_response_fault(denied=..., corrupt=...)` 覆盖
  TileLink D 通道异常响应。
- 用 `UncacheAgent.configure(mmio_latency=...)` 拉长 response，再配合 backend
  redirect/flush 覆盖 stale response 被丢弃。
- 用 `BackendModel.set_wfi_req(1/0)` 覆盖 WFI pending 对 Acquire 的阻塞。
- 用 `BackendModel.set_can_accept(0/1)` 覆盖 backend/IBuffer backpressure。

### 已确认的端口 directed 语义

`a_ready` backpressure case 必须先让 DUT 完成至少一笔真实 MMIO uncache
request/response，再拉低 `a_ready` 去阻塞后续 request。不要在 reset 前一直拉低
`a_ready` 后直接期待 `a_valid` 出现；那可能只是没有把 DUT 推到 uncache request
发起点。有效窗口应在 VCD 中看到：

```text
auto_inner_instrUncache_client_out_a_ready = 0
auto_inner_instrUncache_client_out_a_valid = 1
auto_inner_instrUncache_client_out_a_bits_address = <MMIO fetch address>
```

`corrupt` / `denied` case 不能只断言 env agent 注入成功，必须断言 DUT 可见结果。
当前确认的 DUT 表现：

- `d_bits_corrupt = 1`：backend `cfVec` 上对应指令标记
  `exceptionVec_19 = 1`，functional coverage bin 为
  `frontend_exception_type = hwe`。
- `d_bits_denied = 1`：backend `cfVec` 上对应指令标记 access fault，
  functional coverage bin 为 `frontend_exception_type = af`，并且 MMIO 路径应命中
  `fetch_path_x_exception = mmio_x_af`。
- 跨 8B beat resend 场景中，TileLink A 地址是 8B 对齐 beat 地址，不是原始
  指令 PC。例如 PC `0x10001006` 的第一拍 A 地址为 `0x10001000`，第二拍 resend
  A 地址为 `0x10001008`。
- 跨 8B beat 场景中，第一拍 `corrupt=1` 会抑制 resend，并产生 `hwe`；第一拍
  `denied=1` 不抑制 resend，若第二拍正常，则不会产生 `af`。第二拍
  `corrupt/denied` 则分别产生 `hwe/af`。

这类 case 的最小有效断言应包括：

- agent 统计确认只注入目标 fault，例如 `corrupt_resp_count == 1` 或
  `denied_resp_count == 1`。
- `env.monitor.exception_mark_count > 0`。
- functional coverage 命中对应 `frontend_exception_type`。
- `not env.monitor.get_errors()`。

`WFI` directed case 应在 `io_backend_wfi_wfiReq = 1` 期间确认没有新的 uncache
A channel request handshake；释放为 `0` 后应能观察到新的 request。

### Functional Coverage

uncache 通路的 functional coverage 定义在
`src/test/python/Frontend/docs/frontend_bt_functional_coverage_pilot.csv`，由
`FunctionalCoverageRecorder` 从 env event 和 DUT-visible 端口采样。

当前 uncache covergroup：

- `uncache_req_state`
  - `normal_fire`：uncache A 通道完成一次 request handshake。
  - `a_ready_backpressure`：A 通道 `valid=1` 且 `ready=0`。
  - `wfi_blocked`：`backend.wfi_req=1` 窗口内没有新增 uncache A request。
- `uncache_resp_type`
  - `clean`：D 通道返回时 `corrupt=0` 且 `denied=0`。
  - `corrupt`：D 通道返回 `corrupt=1`。
  - `denied`：D 通道返回 `denied=1`。
- `uncache_resend_flow`
  - `first_denied_resend`：首拍注入 `denied=1` 后仍观察到下一 8B beat
    request。
  - `second_beat_fault`：resend 后第二拍返回 `corrupt` 或 `denied`。
- `uncache_flush_flow`
  - `redirect_flush_pending`：backend redirect 到来时 uncache agent 仍有
    pending response。
  - `redirect_flush_fault`：redirect 后短窗口内收到 stale fault response，且不
    应形成前端异常。
  - `consecutive_redirect_pending`：pending uncache 上连续 backend redirect。

这些 coverpoint 的目标是覆盖端口级通路行为，不替代 `.S/bin` 对正常指令流地址
边界的覆盖。`.S/bin` 仍负责 8B beat、RVC/RVI、fetch-block、page-near-tail 等
自然取指场景。

### MMIO / Non-MMIO 双区域 Bin

`src/test/python/Frontend/tests/asm_cases/fe_mmio_nonmmio_toggle.S` 是一个双地址
区域 case：

- `_start` 位于 MMIO fetch 区域 `0x10001000`。
- 第一跳转目标是 non-MMIO 区域 `0x80000000`。
- 后续在 `0x100010xx` 和 `0x800000xx` 之间多次 `jalr` 切换。
- 最终在 `0x80000054` 命中 good trap。

这个 case 不能按普通单段 `.S -> padded .bin` 处理。它使用两个显式 section：

```text
.mmio_text    -> 0x10001000
.normal_text  -> 0x80000000
```

示例链接参数：

```bash
-Wl,--section-start=.mmio_text=0x10001000
-Wl,--section-start=.normal_text=0x80000000
-Wl,-e,_start
```

当前生成的 runnable bin 是：

```text
src/test/python/Frontend/tests/asm_cases/generated/fe_mmio_nonmmio_toggle.bin
```

它是稀疏文件：

```text
logical size = 1879048284 bytes
disk usage   = about 72K
```

原因是 raw bin 需要同时覆盖 NEMU memory base `0x10000000`、MMIO 入口
`0x10001000` 和 normal 目标 `0x80000000`，逻辑跨度约 `0x7000005c`。

frontend_bt NEMU 配置确认：

```text
CONFIG_MBASE=0x10000000
CONFIG_PC_RESET_OFFSET=0x1000
CONFIG_MSIZE=0x7ff80000000
CONFIG_USE_SPARSEMM is not set
```

因此该 bin 可以由
`~/project/frontend_bt/NEMU/build/riscv64-nemu-interpreter -b` 跑到 good trap，
但 NEMU raw loader 会按逻辑大小读取整个文件。已确认 NEMU 运行结果：

```text
NEMU will start from pc 0x10001000
HIT GOOD TRAP at pc = 0x0000000080000054
total guest instructions = 38
```

对应 jsonl：

```text
NEMU/logs/fe_mmio_nonmmio_toggle.trace.jsonl
```

bin-trace pipeline 运行要求：

```bash
TB_LOG_LEVEL=INFO \
TB_SKIP_NEMU=1 \
TB_BASE_ADDR=0x10000000 \
TB_RESET_VECTOR=0x10001000 \
TB_PYTEST_TIMEOUT_SECS=600 \
src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh \
src/test/python/Frontend/tests/asm_cases/generated/fe_mmio_nonmmio_toggle.bin \
NEMU/logs/fe_mmio_nonmmio_toggle.trace.jsonl
```

已确认该命令通过：

```text
cursor=38 entries=38 cycles=1008 monitor_errors=0
1 passed in about 340s
```

耗时主要来自 Python env 加载逻辑大小 1.8G 的稀疏 bin。终端在加载阶段可能长时间
没有输出；这不是 DUT 死锁。`TB_PYTEST_TIMEOUT_SECS=600` 对当前机器足够，但余量
不大。

## Debug 检查清单

调试 uncache 失败时：

1. 每次查看 VCD 前，都先从当前 FST 重新转换 VCD。
2. 检查 `auto_inner_instrUncache_client_out_a_bits_address`。
3. 检查 `auto_inner_instrUncache_client_out_d_bits_data`。
4. 检查 `inner_instrUncache` 的 `reqReg_addr_addr`、`resending`、
   `io_toIfu_resp_bits_incomplete`。
5. 检查 IFU 的 `uncacheCrossPage`、`prevUncacheCrossPage`、
   `uncacheCheckFault`、`uncacheIsRvc`、`s3_uncacheData`。
6. 检查 backend 可见的 `cfVec` PC、instruction、`isRvc`。
7. 仔细对齐地址单位：byte PC、halfword request address、bus-aligned
   TileLink address 不是同一个量。
