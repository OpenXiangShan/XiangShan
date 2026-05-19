# Frontend MMIO / Non-MMIO 切换风险点

本文记录 frontend 在 MMIO fetch 与 non-MMIO fetch 之间切换时的 Chisel 级风险点。
典型 directed case 是：

```text
0x10001000 MMIO _start
  -> 0x80000000 non-MMIO
  -> 0x10001018 MMIO
  -> 0x80000018 non-MMIO
  -> ...
```

对应源码 case：

```text
src/test/python/Frontend/tests/asm_cases/fe_mmio_nonmmio_toggle.S
```

该 case 的 runnable bin 是大逻辑尺寸 sparse image，使用前先看
`docs/agents/frontend-uncache-boundaries.md` 中 “MMIO / Non-MMIO 双区域 Bin”
小节。

## 关键源码

- `src/main/scala/xiangshan/frontend/ifu/Ifu.scala`
- `src/main/scala/xiangshan/frontend/ifu/IfuUncacheUnit.scala`
- `src/main/scala/xiangshan/frontend/instruncache/InstrUncacheEntry.scala`
- `src/main/scala/xiangshan/frontend/instruncache/InstrUncacheImp.scala`
- `src/main/scala/xiangshan/frontend/ftq/Ftq.scala`
- `src/main/scala/xiangshan/frontend/icache/ICacheMainPipe.scala`

## 高风险状态

### 1. MMIO fetch 会产生 IFU 内部 redirect

`Ifu.scala` 中，uncache fetch 返回后会产生 `uncacheRedirect`：

```scala
uncacheRedirect.valid :=
  s3_valid && io.toIBuffer.ready && s3_reqIsUncache && (s3_uncacheCanGo || uncacheCheckFault)
```

这会 flush IFU 前级：

```scala
s2_flush := backendRedirect || uncacheRedirect.valid || wbRedirect.valid
s1_flush := s2_flush || s1_flushFromBpu(0)
s0_flush := s1_flush || s0_flushFromBpu(0)
```

风险：

- MMIO 返回后必须压掉旧的 s0/s1/s2 speculative fetch。
- MMIO 跳到 non-MMIO 时，旧 uncache response、旧 ICache response、旧 FTQ entry
  可能在相邻周期同时存在。
- 如果 flush 覆盖不完整，后续 non-MMIO 路径可能消费到旧 MMIO 上下文。

覆盖状态：

- 已覆盖真实控制流切换。`fe_mmio_nonmmio_toggle.S` 通过 MMIO 和 normal 区域
  多次 `jalr` 交替，覆盖 MMIO response 后 IFU 内部 redirect 和前级 flush。
- 已覆盖 normal redirect 相邻场景。`fe_mmio_normal_redirect_stress.S` 在 normal
  区域放必 taken branch，再回 MMIO，覆盖 normal redirect 与 MMIO uncache
  相邻时旧 speculative fetch 的清理。
- 已覆盖 pending ICache response 场景。
  `test_uncache_redirect_to_mmio_while_icache_response_pending` 拉长 ICache latency，
  在 normal ICache response pending 时 redirect 到 MMIO，检查旧 ICache response
  晚回来后 monitor 无 PC/异常错误。

### 2. backend redirect 与 uncache writeback 同周期优先级

`Ifu.scala` 中 backend redirect 会 flush s3：

```scala
backendRedirect := fromFtq.redirect.valid
s3_flush := backendRedirect || (wbRedirect.valid && !s3_wbNotFlush)
```

uncache writeback 被 backend redirect mask：

```scala
uncacheFlushWb.valid :=
  s3_valid && io.toIBuffer.ready && s3_reqIsUncache &&
  !backendRedirect && (s3_uncacheCanGo || uncacheCheckFault)
```

风险：

- MMIO instruction 本身是 `jalr` 时，frontend 只能先写顺序 target，真实 target
  通常由 backend redirect 修正。
- 如果 backend redirect 与 uncache response / uncache writeback 同周期到达，
  uncache writeback 会被屏蔽。
- 需要确认 FTQ、IBuffer、backend model 对该条 MMIO 指令的“完成”和“被修正”
  语义一致，否则会出现 cursor 对不上或重复 flush。

覆盖状态：

- 已覆盖相邻周期压力。`fe_mmio_nonmmio_toggle.S` 和
  `fe_mmio_normal_redirect_stress.S` 都在 MMIO 上执行 `lla + jalr`，frontend
  先看到顺序 target，随后由 backend golden mismatch redirect 修正真实 target。
- 已覆盖被 flush 的 fault response 不上报。
  `test_uncache_flushed_corrupt_response_does_not_report_exception` 对旧 pending
  MMIO response 注入 corrupt 后 redirect，检查该 response 被 flush 后不产生
  `hwe`。
- 尚未精确覆盖同周期优先级。还需要能按 cycle 安排 redirect 的 Python helper，
  将 backend redirect 放在 MMIO corrupt/denied response 前后 0/1 cycle。

### 3. MMIO 与 non-MMIO uncache 的 commit gating 不同

`IfuUncacheUnit.scala` 中，真实 MMIO 请求先进入 `WaitLastCommit`：

```scala
uncacheState := Mux(reqIsMmio, UncacheFsmState.WaitLastCommit, UncacheFsmState.SendReq)
```

如果是第一条指令，或者 FTQ 报告更老 MMIO 已 commit，才进入 `SendReq`：

```scala
when(isFirstInstr) {
  uncacheState := UncacheFsmState.SendReq
}.otherwise {
  uncacheState := Mux(io.mmioCommitRead.mmioLastCommit, UncacheFsmState.SendReq, UncacheFsmState.WaitLastCommit)
}
```

commit 查询只对 `isMmio` 生效：

```scala
io.mmioCommitRead.valid := uncacheValid && isMmio
```

风险：

- MMIO 请求需要顺序约束；PBMT NC / non-MMIO uncache 不需要同样等待。
- MMIO/non-MMIO 快速切换时，`isMmio` 必须准确对应当前请求。
- 如果 `isMmio` 沿用旧请求，可能错误等待 `mmioLastCommit`，或错误绕过 MMIO
  commit 顺序。

覆盖状态：

- 已覆盖真实连续 MMIO fetch。`fe_mmio_commit_gate_burst.S` 在 MMIO 区域放
  straight-line burst，使非首条 MMIO fetch 必须经过 commit gating。
- 已覆盖 Python 端口级 commit gating。
  `test_uncache_mmio_commit_order_waits_last_commit` 通过 `set_can_accept(0)`
  阻止 backend commit，检查 `mmioLastCommit` 未满足前不会发起新的 MMIO request。
- 已间接覆盖 MMIO/non-MMIO 快速切换。三个双区域 bin case 都会从 normal 回到
  MMIO，检查当前请求的 `isMmio` 能重新建立。
- 已补充 PBMT/PMA 对照测试。`test_uncache_cacheable_non_mmio_uses_icache_path`
  覆盖 non-MMIO + PBMT PMA 仍走 ICache；`test_uncache_pbmt_nc_non_mmio_uses_uncache_path`
  覆盖 non-MMIO + PBMT NC 走 InstrUncache 且不等 MMIO commit gating。当前
  frontend-only DUT 中 PBMTE 不作为 `Frontend.sv` 的有效输入，该 case 不通过驱动
  `io_tlbCsr_mPBMTE` 构造，而是由 PTW model 返回 `pf=0,pbmt=NC`。原因见下文
  “当前实现状态”。
- PBMT NC 正向用例必须证明“取出了正确 PC 的正确指令”。只看到
  `cfVec.valid` 和某个 exception bit，而 `cfVec.pc` 全为 0，不能算有效覆盖。
  当前 `test_uncache_pbmt_nc_non_mmio_uses_uncache_path` 已按这个标准收紧断言：
  PTW/env stimulus 返回 `pf=0,pbmt=NC` 后，必须看到非 MMIO 物理地址通过
  InstrUncache 请求，并在 monitor 中观察到 `_NORMAL_BASE` 对应的指令。
- PBMT NC CFI 的 commit 时机按 backend 正确路径语义处理：当前 cfVec 中的
  正确路径指令被观察后，必须先完成 right-path resolve，之后才允许进入 ROB
  commit；如果该 CFI 预测错误，frontend 收到对应 redirect 后只是解除
  redirect source FTQ 的额外 recovery 等待，不能替代 resolve。source FTQ
  不需要等待下一笔 recovery cfVec 出现；等待 recovery target 只应阻塞更年轻
  FTQ entry。
- 在 S-mode SV39 directed case 中还必须显式配置 PMP/PMA：PMP 允许目标物理页
  `X/R/W`，PMA 将同一页标成 `X/R/W/cacheable`。否则 frontend 看到的是 PMP/PMA
  access fault，`s2_reqIsUncache` 可能已拉高，但 `s3_useUncacheFetch` 会被
  exception 压掉，最终只能得到 `cfVec.pc=0` 的无效覆盖。

### 4. flush reset 没清所有属性寄存器

`IfuUncacheUnit.uncacheReset()` 清了：

```scala
uncacheState
uncacheData
uncacheException
uncacheCrossPage
uncachePAddr
uncacheFinish
```

但没有清：

```scala
isMmio
itlbPbmt
```

风险：

- 正常下一笔 request 会重新覆盖 `isMmio` / `itlbPbmt`。
- 但在 flush 边界，组合输出可能短暂看到旧属性。
- 需要重点看 `mmioCommitRead.valid`、`toUncache.bits.memBackTypeMM`、
  `toUncache.bits.memPageTypeNC` 是否只在有效请求窗口内被消费。

覆盖状态：

- 已间接覆盖。`fe_mmio_commit_gate_burst.S` 和 `fe_mmio_nonmmio_toggle.S`
  都在 MMIO/normal 间反复切换，若旧 `isMmio` / `itlbPbmt` 在有效窗口外被消费，
  容易表现为错误等待 commit 或路径切换错误。
- 尚未直接断言。还需要 Python case 在 flush 后采样
  `mmioCommitRead.valid`、`toUncache.valid` 与属性信号关系，确认旧属性只在
  valid 请求窗口内被消费。

### 5. pending uncache response 必须不能污染新路径

`InstrUncacheEntry.scala` 中，flush 到来时不会取消已经发出的 TL transaction，而是
记录 `needFlush`：

```scala
when(io.flush && (state =/= State.Invalid) && (state =/= State.SendResp)) {
  needFlush := true.B
}
```

到 `SendResp` 后 suppress response：

```scala
io.resp.valid := state === State.SendResp && !needFlush
```

风险：

- MMIO 跳到 non-MMIO 后，旧 MMIO response 可能晚回来。
- 该 stale response 必须被丢弃，不能进入 IFU data path。
- 如果 flush 与 `SendResp` 边界处理错误，可能出现旧 MMIO 数据被当作新路径指令。

覆盖状态：

- 已覆盖 pending response 被 redirect flush。
  `test_uncache_pending_response_flushed_by_redirect` 拉长 `mmio_latency`，在
  uncache request 发出后 redirect 到新 PC，检查旧 response 返回后不污染 monitor。
- 已覆盖带 fault 的 stale response。
  `test_uncache_flushed_corrupt_response_does_not_report_exception` 对旧 pending
  response 注入 corrupt，确认被 flush 后不产生 exception。
- 已覆盖连续 redirect 覆盖旧 pending fetch。
  `test_uncache_consecutive_redirects_drop_older_pending_fetch` 对同一 pending
  MMIO fetch 连续注入两个 redirect，检查更老路径被丢弃。
- 尚未逐状态覆盖 `SendReq` / `WaitResp` / `SendResp` 精确 flush 落点。现有 case
  覆盖 pending response 的主要路径，但还没有按 uncache entry 状态枚举。

### 6. ICache 与 uncache 路径切换

ICache main pipe 会识别 MMIO / PBMT uncache：

```scala
s1_isMmio = s1_pmpMmio || Pbmt.isUncache(s1_itlbPbmt)
```

并避免对 MMIO 路径正常 ICache fetch：

```scala
s1_exception.isNone && !s1_isMmio
```

风险：

- non-MMIO 路径仍可能有 ICache response。
- MMIO 路径同时可能有 pending uncache response。
- 快速切换时要确认旧 ICache response 和旧 uncache response 都被对应 flush 规则覆盖。

覆盖状态：

- 已覆盖真实路径切换。三个双区域 bin case 都在 `0x10001000` MMIO 和
  `0x80000000` normal 区域之间切换，覆盖 ICache/uncache path handoff。
- 已覆盖 normal ICache pending 后切 MMIO。
  `test_uncache_redirect_to_mmio_while_icache_response_pending` 拉长 ICache latency，
  在旧 ICache response pending 时 redirect 到 MMIO，并确认 MMIO uncache request
  正常发出。
- 尚未覆盖 MMIO pending 与 normal ICache pending 双 pending。该场景需要同时
  拉长 `mmio_latency` 和 ICache latency，让两条 memory path 的 response 都晚到。

### 7. uncache 指令的 frontend target 只是顺序 target

`Ifu.scala` 中 uncache target 由指令长度决定：

```scala
uncacheTarget =
  Mux(uncacheIsRvc || prevUncacheCrossPage || uncacheCheckFault,
    s3_alignFetchBlock(0).startVAddr + 2.U,
    s3_alignFetchBlock(0).startVAddr + 4.U)
```

风险：

- 对 MMIO 上的 `jalr`，frontend 不知道真实 target，只能先走顺序 target。
- 真实 target 需要 backend redirect 修正。
- 因此 MMIO/non-MMIO toggle case 中看到多次 `golden_first_mismatch_redirect` 是
  预期压力点，不应直接判断为 DUT bug。

覆盖状态：

- 已覆盖。`fe_mmio_nonmmio_toggle.S`、`fe_mmio_commit_gate_burst.S` 和
  `fe_mmio_normal_redirect_stress.S` 都在 MMIO 区域使用 `lla + jalr` 跳转。
  frontend 对该 uncache 指令只能生成顺序 target，backend model 根据 golden trace
  注入 `golden_first_mismatch_redirect` 修正真实 target。
- 覆盖方式不是检查单个信号，而是 bin-trace 端到端完成：golden cursor 走完、
  monitor 无错误，说明顺序 target、backend redirect recovery 和 FTQ 状态能一致
  收敛到真实 target。

## 波形检查清单

每次查看 VCD 前先从当前 FST 重新转换。

重点信号：

```text
IFU:
s3_reqIsUncache
s3_useUncacheFetch
s3_flush
uncacheRedirect.valid
uncacheFlushWb.valid
uncacheBusy
prevUncacheCrossPage
uncachePc

IfuUncacheUnit:
uncacheState
isMmio
itlbPbmt
toUncache.valid
toUncache.ready
fromUncache.valid
fromUncache.ready
mmioCommitRead.valid
mmioCommitRead.mmioLastCommit

InstrUncache:
state
needFlush
reqReg.addr
resending
io.resp.valid
io.resp.bits.corrupt
io.resp.bits.denied
io.resp.bits.incomplete

FTQ:
redirect.valid
ifuPtr
bpuPtr
commitPtr
mmioCommitRead.mmioFtqPtr
mmioCommitRead.mmioLastCommit

DUT output cfVec:
io_backend_cfVec_0_valid ... io_backend_cfVec_7_valid
io_backend_cfVec_0_bits_pc ... io_backend_cfVec_7_bits_pc
io_backend_cfVec_0_bits_instr ... io_backend_cfVec_7_bits_instr
io_backend_cfVec_0_bits_isRvc ... io_backend_cfVec_7_bits_isRvc
io_backend_cfVec_0_bits_exceptionVec_1 ... io_backend_cfVec_7_bits_exceptionVec_1
io_backend_cfVec_0_bits_exceptionVec_12 ... io_backend_cfVec_7_bits_exceptionVec_12
```

历史无效波形：

- `src/test/python/Frontend/data/20260518/test_uncache_pbmt_nc_without_pbmte_does_not_use_cacheable_path.fst`
  中 `io_ptw_resp_bits_s1_entry_pbmt` 返回过 `01`，但 `cfVec_0.pc` 全为 0。
  这说明该 case 没有证明取出了有意义的 frontend 指令输出。
- 该波形只能说明“没有误走普通 ICache/cacheable path”的弱现象，不能作为 PBMT NC
  正向 uncache 覆盖，也不能作为 frontend 异常正确性的证据。
- PBMT NC 是 non-cacheable 的正常取指路径；当 PTW response 给出
  `pf=0,gpf=0,entry.pbmt=NC` 时，DUT 应经 InstrUncache 取回正确指令，并输出
  对应 PC/instr。

## 覆盖策略

优先用 `.S -> bin -> jsonl` 覆盖真实控制流形态，例如 MMIO `jalr` 到
non-MMIO、normal 分支后回 MMIO、连续 MMIO fetch burst。只要关键条件依赖
ready/valid 周期、TileLink response 属性、WFI、backend stall 或 flush 精确落点，
就用 Python 端口级 directed case。

所有双区域 `.S` case 使用相同链接布局：

```text
.mmio_text   -> 0x10001000
.normal_text -> 0x80000000
entry        -> _start
```

生成 NEMU 可运行 bin 时，最终 bin 的 base 仍按 NEMU `CONFIG_MBASE=0x10000000`
组织；也就是 `0x10001000` 的第一条指令位于 bin offset `0x1000`。
如果 case 同时包含 `0x80000000` 区域，需要生成 sparse bin，避免真实写出 1.8G
以上的连续文件内容。

## PBMT NC 构造方法

PBMT NC 与普通 non-MMIO 的区别：

| 类型 | 地址属性 | PBMT | 取指路径 | MMIO commit gating |
| --- | --- | --- | --- | --- |
| 普通 non-MMIO | 非 MMIO | cacheable | ICache | 不需要 |
| PBMT NC | 非 MMIO | uncacheable | InstrUncache | 不应该需要 |
| 真 MMIO | MMIO | 任意/通常 uncache | InstrUncache | 需要 |

PBMT NC 不能只靠 `.S/bin` 稳定构造，因为 `.S` 只能决定指令地址和内容，不能决定
TLB/PTW 返回的 PBMT。该场景应使用 Python directed case，通过 PTW/TLB response
把某个非 MMIO 页标成 NC。

真实 `.bin` 本身也不携带 frontend 非 baremode 所需的 VA->PA 映射信息。非 baremode
运行真实 bin 时，测试环境必须先生成一组 VA/PA 页映射：把 bin payload 加载到随机
或指定的非 MMIO 物理页，再把 reset vector 设成对应虚拟页。当前
`_prepare_sv39_mapped_pbmt_nc_cfi_stream(..., bin_path=...)` 支持这种模式：若未显式
传入 `vaddr/paddr`，会用固定 seed 随机生成 non-MMIO `vaddr -> paddr` 映射，
返回 `Sv39Mapping` 供 reset、PMP/PMA 配置和断言使用。

VA page 应按程序布局连续推进；PA page 不应从 bin 推导，而应由测试配置决定。
helper 支持 `paddr_pages=(...)`，用于把连续 VA page 映射到可配置的、不一定连续的
物理页。加载 payload 时也必须按 4KB page 切片写入对应 PA page，而不是把整个
bin 假设为一段连续物理内存。

推荐构造：

1. 选择 non-MMIO fetch 虚拟地址，例如 `0x80000000`。不要把物理地址加入
   `env.memory.mmio_ranges`，保证它不是真 MMIO。
2. 选择独立物理页，例如 `0x80001000`，在该地址加载一段指令流。正向用例不应只放
   单条指令，也不应只用纯 `c.nop`；当前用例在第一个 64B fetch block 内放入
   多条 `c.nop`，并在 `0x80000010` 放一条 `jal x0, +4`
   (`0x0040006f`) 作为 CFI，目标为 fall-through `0x80000014`，后续继续 `c.nop`。
3. 在 reset 后通过 `io_csrCtrl_distribute_csr_w_*` 配置目标物理页：
   `pmpcfg0=0x1f,pmpaddr0=napot_4k(0x80001000)` 允许 S-mode 取指；
   `pmacfg0=0x7f,pmaaddr0=napot_4k(0x80001000)` 让 PMA 返回
   `X/R/W/cacheable` 且 `pmpMmio=0`。没有这一步时，case 测到的是 PMP/PMA
   access fault，而不是 PBMT NC 正常取指。
4. 配置 PTW/TLB response，让 `0x80000000` 所在页返回 PBMT NC。目标是让 frontend
   看到 `itlbPbmt = NC/uncache` 且 `pmpMmio = 0`。
5. 从 `0x80000000` reset，且不要使用 bare mode；需要走 TLB/PTW 才能观察 PBMT。
6. PMP/PMA 写入后 redirect 回 `0x80000000`，确保新权限下重新发起 fetch。
7. 断言该 fetch 走 InstrUncache，但不触发 MMIO commit gating。

建议 case：

```text
test_uncache_pbmt_nc_uses_uncache_without_mmio_commit_gating
test_uncache_true_mmio_waits_commit_gating_as_control
```

PBMT NC case 的关键断言：

```text
地址不在 mmio_ranges
toUncache.valid == 1
mmioCommitRead.valid == 0
uncache_agent.request_addrs 包含 0x80001000 对应物理请求地址
uncache A 通道覆盖该 64B block 对应的 8 个 8B beat：0x80001000..0x80001038
monitor/cfVec 观察到 block 内连续 PC，其中 0x80000010 为 jal CFI，后续继续到 0x80000014
```

PBMT NC 的 cfVec 输出粒度有一个容易误判的边界：无反压时通常观察到一拍一条
NC 指令，但这不是 DUT 对外的绝对保证。`test_uncache_pbmt_nc_after_ibuffer_backpressure_can_output_multiple_cfvec_lanes`
先将 backend `canAccept` 拉低制造 IBuffer/后端反压，再释放反压；当前波形中
同一拍可以看到多个 `cfVec` lane 同时有效，例如 `0x80000000/0x80000002/0x80000004`
在同一 cycle 输出。因此 PBMT NC 的正确性断言应检查 PC/instr 序列和路径属性，
不能假设“NC 永远一拍只出一条 cfVec”。

真 MMIO 对照 case 的关键断言：

```text
地址在 mmio_ranges
toUncache.valid == 1
mmioCommitRead.valid == 1
backend commit 被卡住时，新的 MMIO request 不应继续发出
```

当前实现状态：

- `PageTableModel.map_page(..., pbmt=...)` 和 `map_stage2_page(..., pbmt=...)`
  已支持 per-page PBMT override。
- `PTE.pbmt` 会透传到 PTW response 的 `s1_entry_pbmt` / `s2_entry_pbmt`。
- Chisel 源码中的字段名是 `TlbCsrBundle.mPBMTE` / `TlbCsrBundle.hPBMTE`，定义在
  `src/main/scala/xiangshan/Bundle.scala`。PBMTE 的真实语义消费点在 PTW 侧：
  `PageTableWalker.scala`、`PageTableCache.scala`、`L2TLB.scala` 会把 PBMTE 传入
  `PteBundle.isPf/isGpf/canRefill/onlyPf`，用于判定 PBMT 非零 PTE 在 PBMTE 关闭时
  是否应成为 page fault。
- 当前 `build-frontend/rtl/` 的 frontend-only DUT 没有生成
  `PageTableWalker.sv`、`PageTableCache.sv`、`L2TLB.sv`。Frontend 只实例化
  `TLB`、`PTWFilter`、`PTWRepeaterNB`，并通过外部 `io.ptw.resp` 接收已经形成的
  PTW response。
- `TLB.scala` 的 frontend 路径只消费 `ptw.resp.bits.s1.entry.pbmt` /
  `ptw.resp.bits.s2.entry.pbmt`，并在 `pbmt_check` 中把 PBMT 合成到
  `resp.bits.pbmt`；它不读取 `csr.mPBMTE/hPBMTE` 来重新判定 page fault。
  `PTWFilter` / `PTWRepeaterNB` 也只用 CSR 的 `satp/vsatp/hgatp/asid/vmid` 和
  `changed/virt_changed` 等字段做 hit/flush，不消费 PBMTE。
- 因此在当前 frontend-only 生成物中，PBMTE 从 `Frontend` 逻辑角度是未使用输入。
  FIRRTL/Verilog 优化后，`build-frontend/rtl/Frontend.sv` 模块端口列表只保留实际
  消费的部分 `io_tlbCsr_*` 字段：`satp/vsatp/hgatp`、`mbmc.BME/CMODE`、
  `priv.virt/virt_changed/imode`，没有 `io_tlbCsr_mPBMTE` /
  `io_tlbCsr_hPBMTE`。
- `build-frontend/rtl/FrontendTop.sv` 顶层端口列表中虽然有
  `frontend_io_tlbCsr_mPBMTE` / `frontend_io_tlbCsr_hPBMTE`，但这两个端口只出现在
  顶层端口声明里。`Frontend` 实例化连接里没有 `.io_tlbCsr_mPBMTE(...)` /
  `.io_tlbCsr_hPBMTE(...)`，因为 `Frontend.sv` 本身没有对应端口；所以这两个
  顶层输入是悬空/无效端口，对当前 DUT 行为没有影响。
- Python pylib wrapper `build-frontend/pylib/Frontend/Frontend_top.sv` 与
  `Frontend.sv` 一致，也没有 `io_tlbCsr_mPBMTE` / `io_tlbCsr_hPBMTE` 的 DPI
  getter/setter。因此不能通过 Python directed case 直接“打开 DUT 的 PBMTE”。
- 在 PBMTE 未打开时，PBMT 非零 PTE 不能作为“NC uncache path”证明。若要覆盖
  非法 PTE 场景，PTW/env stimulus 必须返回明确的 `pf/gpf=1`，且 cfVec 的 PC
  必须对应触发异常的 fetch PC；`pc=0` 的 exception-only 输出不算有效覆盖。
- 对 frontend-only DUT，PBMTE 应作为 env/PTW stimulus 侧策略建模：PBMTE 关闭且
  PTE PBMT 非零时，PTW model 应返回 `s1_pf/gpf`；PBMTE 开启且 PBMT=NC 时，PTW
  model 返回 `pf=0` 且 `entry.pbmt=NC`，Frontend 再根据 PTW response 中的 PBMT
  选择 InstrUncache。也就是说当前可控输入是 `io_ptw_resp_bits_*_entry_pbmt`
  和 `io_ptw_resp_bits_*_pf/gpf`，不是 `io_tlbCsr_*PBMTE`。
- 因此 frontend uncache 分析/波形检查不应把 `tlbCsr.mPBMTE/hPBMTE` 当作关键
  DUT 信号；应关注 PTW response 结果和 frontend 输出行为：
  `io_ptw_resp_bits_s1_entry_pbmt`、`io_ptw_resp_bits_s2_entry_pbmt`、
  `io_ptw_resp_bits_s1_pf`、`io_ptw_resp_bits_s2_gpf`、
  `auto_inner_instrUncache_client_out_a_valid/address`、
  `auto_inner_icache_client_out_a_valid/address`、`io_backend_cfVec_*_valid` 与
  `io_backend_cfVec_*_bits_exceptionVec_*`。
- 该场景的核心不是“是否出现 uncache request”，而是“非 MMIO + PBMT NC 走
  uncache，但不走 MMIO commit gating”。当前 wrapper 限制消除前，这一点仍是
  条件覆盖，不是当前 DUT 的已通过覆盖。

## 已覆盖 Bin Case

| Case | 构造要点 | 覆盖重点 |
| --- | --- | --- |
| `fe_mmio_nonmmio_toggle.S` | `_start` 在 `0x10001000`，MMIO 和 `0x80000000` normal 区域通过 `lla + jalr` 多次交替 | 风险 1/2/3/5/6/7：内部 redirect、backend 修正、commit gating、旧 response 隔离、ICache/uncache 切换 |
| `fe_mmio_commit_gate_burst.S` | MMIO straight-line burst -> normal -> MMIO burst -> normal good trap | 风险 3/4/6/7：连续 MMIO commit gating、flush 后旧属性间接覆盖、路径切换 |
| `fe_mmio_normal_redirect_stress.S` | MMIO `jalr` -> normal taken branch/JALR -> MMIO -> normal | 风险 1/2/5/6/7：normal redirect 和 MMIO uncache 相邻、旧 response 隔离 |

`fe_mmio_nonmmio_toggle` 已确认：

```text
NEMU:
  HIT GOOD TRAP at pc = 0x80000054
  total guest instructions = 38

bin-trace pipeline:
  cursor=38 entries=38 cycles=1008 monitor_errors=0
  1 passed in about 340s
```

这类 sparse bin 的逻辑大小约 1.8G，运行时间主要花在 Python env 加载程序镜像。

## 已覆盖 Python Case

这些 case 位于：

```text
src/test/python/Frontend/tests/test_instr_uncache_port_boundaries.py
```

| 类别 | Case | 构造要点 | 覆盖重点 |
| --- | --- | --- | --- |
| A 通道 backpressure | `test_uncache_a_ready_backpressure_holds_request` | 先完成一笔 MMIO request/response，再 `set_a_ready(0)` 卡下一笔 request | `a_valid` 保持、`a_bits_address` 不变、ready 恢复前 request count 不增长 |
| Response fault | `test_uncache_corrupt_response_injection` / `test_uncache_denied_response_injection` | 对下一笔 uncache response 注入 `corrupt` 或 `denied` | DUT 可见 `hwe` / `af`，不是只检查 agent 统计 |
| WFI | `test_uncache_wfi_req_blocks_new_acquire` / `test_uncache_wfi_during_refill_resp_reports_not_safe` | request 前或 response pending 中拉高 `wfiReq` | WFI 阻止新 acquire，pending response 期间 `wfiSafe` 不提前为 1 |
| Pending response flush | `test_uncache_pending_response_flushed_by_redirect` | 拉长 `mmio_latency`，request 后 redirect 到新 PC | 旧 response 被 `needFlush` suppress |
| Flushed fault response | `test_uncache_flushed_corrupt_response_does_not_report_exception` | 旧 pending response 注入 corrupt 后 redirect | 被 flush 的 corrupt response 不能上报 `hwe` |
| Consecutive redirect | `test_uncache_consecutive_redirects_drop_older_pending_fetch` | 对同一 pending MMIO fetch 连续注入两个 redirect | 更老 pending fetch 被丢弃 |
| ICache pending -> MMIO | `test_uncache_redirect_to_mmio_while_icache_response_pending` | normal 区域启动，拉长 ICache latency，pending 后 redirect 到 MMIO | 旧 normal ICache response 晚回来不能污染 MMIO uncache path |
| Cacheable non-MMIO 对照 | `test_uncache_cacheable_non_mmio_uses_icache_path` | SV39 映射 `0x80000000 -> 0x80001000`，PTE PBMT=PMA；显式写 PMP allow 和 PMA `X/R/W/cacheable`；物理地址不加入 `mmio_ranges` | non-MMIO + PBMT PMA 应能经 ICache 正常提交，且不产生 uncache request |
| PBMT NC 非法 PTE 保护 | `test_uncache_pbmt_nc_without_pbmte_does_not_use_cacheable_path` | 历史 case：SV39 映射后 PTE PBMT=NC，但未严格建模 PBMTE 关闭的 `pf/gpf` 策略 | 已标记 skip；历史波形中 `cfVec.pc=0`，不能证明有效 frontend 输出 |
| PBMT NC 正向路径 | `test_uncache_pbmt_nc_non_mmio_uses_uncache_path` | SV39 映射 `0x80000000 -> 0x80001000`，PTW model 返回 `pf=0,gpf=0,entry.pbmt=NC`；显式写 PMP allow 和 PMA `X/R/W/cacheable`；block 内包含 `jal x0,+4` CFI，而不是驱动 `io_tlbCsr_mPBMTE` | 已覆盖 non-MMIO + PBMT NC 走 InstrUncache，A 通道覆盖 64B block 的 8 个 8B beat，cfVec 逐条输出 block 内指令并包含 CFI |
| PBMT NC 反压释放多 lane | `test_uncache_pbmt_nc_after_ibuffer_backpressure_can_output_multiple_cfvec_lanes` | 在 PBMT NC 正向路径基础上先 `set_can_accept(0)`，等待 uncache response 后再释放 | 覆盖反压后 IBuffer/后端可在同一 cycle 输出多个 PBMT NC `cfVec` lane，不能把“一拍一条”作为不变量 |
| PBMT NC 真实 bin 随机映射 | `test_uncache_pbmt_nc_real_bin_uses_random_sv39_mapping` | 把真实 bin payload 读入物理页，helper 随机生成 `vaddr -> paddr`，reset 使用返回的 `mapping.vaddr` | 覆盖 bin 不携带地址信息时，非 baremode 由 env 构造 VA/PA 映射并验证 PBMT NC 正常取指 |
| PBMT NC 真实 bin 可配置 PA page | `test_uncache_pbmt_nc_real_bin_can_use_configured_phys_pages` | 构造跨 4KB payload，传入 `paddr_pages=(...)` | 覆盖连续 VA page 映射到可配置 PA page，且 payload 按页写入对应物理地址 |
| Resend fault | `test_uncache_resend_first_beat_corrupt_suppresses_resend` | reset 到 `0x10001006`，第一拍 corrupt | 第一拍 corrupt 抑制 resend 并产生 `hwe` |
| Resend denied | `test_uncache_resend_first_beat_denied_allows_resend` | reset 到 `0x10001006`，第一拍 denied | 当前 RTL 仍 resend；第二拍正常则不产生 `af` |
| Second beat fault | `test_uncache_resend_second_beat_fault_reports_exception` | reset 到 `0x10001006`，第二拍 corrupt/denied | 最终 exception 来自第二拍 |
| Backend/commit gating | `test_uncache_backend_backpressure_blocks_new_mmio_request` / `test_uncache_mmio_commit_order_waits_last_commit` | `set_can_accept(0)` 卡 backend 接收或 commit | backpressure/commit 未满足前不发新 MMIO request |

已生成的重点波形：

```text
src/test/python/Frontend/data/20260515/test_uncache_redirect_to_mmio_while_icache_response_pending.fst
src/test/python/Frontend/data/20260515/test_uncache_a_ready_backpressure_holds_request.fst
src/test/python/Frontend/data/20260518/test_uncache_cacheable_non_mmio_uses_icache_path.fst
src/test/python/Frontend/data/20260518/test_uncache_pbmt_nc_without_pbmte_does_not_use_cacheable_path.fst  # 历史无效覆盖：cfVec.pc=0
src/test/python/Frontend/data/20260518/test_uncache_pbmt_nc_non_mmio_uses_uncache_path.fst  # 有效覆盖：PBMT NC 正常取指
src/test/python/Frontend/data/20260518/test_uncache_pbmt_nc_after_ibuffer_backpressure_can_output_multiple_cfvec_lanes.fst
```

## 待覆盖 Corner

| 优先级 | Corner | 推荐方式 | 构造草案 | 阻塞/备注 |
| --- | --- | --- | --- | --- |
| P0 | `isMmio` / `itlbPbmt` flush 后属性不外泄 | Python | flush 后采样 `mmioCommitRead.valid`、`toUncache.valid` 与属性信号关系 | Chisel 中 `uncacheReset()` 未清这两个寄存器，bin 只能间接覆盖 |
| P0 | flush 精确落在 `SendReq` / `WaitResp` / `SendResp` | Python | 等待指定状态后 inject redirect，检查 response suppress 和 request drop | 需要可靠状态观察或等价 ready/latency 卡点 |
| P0 | `a_valid=1 && a_ready=0` 时 backend redirect | Python | 先让 MMIO 流稳定，再卡 A 通道未握手 request，并注入合法 redirect | 直接 `backend_model.inject_redirect` 会触发 stale FTQ context 保护 |
| P1 | MMIO pending + normal ICache pending 双 pending | Python | 拉长 `mmio_latency` 制造旧 MMIO pending，redirect 到 normal 后拉长 ICache latency | 检查两条 memory path 的晚到 response 都不污染新路径 |
| P1 | MMIO exception 与 backend redirect 同周期/相邻周期 | Python | 对 pending MMIO response 注入 `corrupt/denied`，把 redirect 安排到 response 前后 0/1 cycle | 需要按 cycle 安排 redirect 的 helper |
| P1 | PBMT NC 与真 MMIO commit gating 区分 | Python | 按“PBMT NC 构造方法”配置非 MMIO 页为 PBMT NC，比较 `toUncache.valid` 与 `mmioCommitRead.valid` | 不关注 frontend 内部 PBMTE 信号；正向 case 应由 PTW model 返回 `pf=0,pbmt=NC`，非法 PTE case 返回 `pf/gpf=1` |
| P2 | 连续 MMIO burst 中间插入 WFI | Python | 连续 MMIO stream 中拉高 `wfiReq`，分别覆盖 request 前和 response pending | 是 WFI 与 commit gating 的组合覆盖 |
| P2 | page 最后 halfword 的 32-bit 指令跨页 | Python | reset 到 page end，构造 32-bit 低半部，观察 incomplete/check fault | `...xffe` 取指已有独立风险，不适合作为稳定 bin regression |
| P2 | FTQ wrap / commit pointer wrap 下的 MMIO commit read | bin 或 Python | 长 MMIO/non-MMIO stream 超过 FTQ 深度，或用 backend backpressure 控 pointer | 长流稳定性优先级低于端口级 response/flush 边界 |

不建议优先新增的 bin case：

- 更多 MMIO/non-MMIO `jalr` 交替：已有三个 bin case 覆盖，边际收益低。
- 单纯加长 MMIO burst：`fe_mmio_commit_gate_burst.S` 已覆盖真实指令流形态；
  需要更强断言时应使用 Python。
- `...xffe` page-end RVC：当前已知该区域可能触发未修复问题，不适合作为稳定通过
  的新 regression。
