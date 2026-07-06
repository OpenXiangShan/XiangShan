# MemBlock 未 Agent 化系统 IO TODO

## 1. 背景

当前 mem_ut 测试框架已经为 LSQ enq、issue、commit、redirect、DCache TileLink、SBuffer、L2TLB/PTW responder 等主要接口建立了 agent 和 sequence。

本次检查发现，MemBlock 顶层仍有部分外部系统侧输入没有进入任何 agent，而是在 `dut_inst.sv` 中直接 tie 0。这些信号不会被 testcase 或 sequence 主动驱动，也不会形成可配置场景。

主要源码依据：

```systemverilog
.io_l2_hint_valid ( '0 ),
.io_l2_hint_bits_sourceId ( '0 ),
.io_l2_hint_bits_isKeyword ( '0 ),
.io_l2_tlb_req_req_valid ( '0 ),
.io_l2_tlb_req_req_bits_vaddr ( '0 ),
.io_l2_tlb_req_req_bits_fullva ( '0 ),
.io_l2_tlb_req_req_bits_checkfullva ( '0 ),
.io_l2_tlb_req_req_bits_cmd ( '0 ),
.io_l2_tlb_req_req_bits_hyperinst ( '0 ),
.io_l2_tlb_req_req_bits_hlvx ( '0 ),
.io_l2_tlb_req_req_bits_kill ( '0 ),
.io_l2_tlb_req_req_bits_memidx_is_ld ( '0 ),
.io_l2_tlb_req_req_bits_memidx_is_st ( '0 ),
.io_l2_tlb_req_req_bits_memidx_idx ( '0 ),
.io_l2_tlb_req_req_bits_isPrefetch ( '0 ),
.io_l2_tlb_req_req_bits_no_translate ( '0 ),
.io_l2_tlb_req_req_bits_pmp_addr ( '0 ),
.io_l2_tlb_req_req_bits_debug_robIdx_flag ( '0 ),
.io_l2_tlb_req_req_bits_debug_robIdx_value ( '0 ),
.io_l2_tlb_req_req_bits_debug_isFirstIssue ( '0 ),
.io_l2_tlb_req_req_kill ( '0 ),
.io_l2_flush_done ( '0 ),
```

路径：`mem_ut/ver/ut/memblock/tb/dut_inst.sv`

## 2. 待支持信号清单

### 2.1 L2 cache hint 输入

信号：

```text
io_l2_hint_valid
io_l2_hint_bits_sourceId
io_l2_hint_bits_isKeyword
```

当前状态：未被 agent 包含，`dut_inst.sv` 中 tie 0。

功能理解：

`io.l2_hint` 是 L2 cache 给 MemBlock/DCache/LSQ 的提示信号。Scala 中会进入：

```text
MemBlock.io.l2_hint
  -> dcache.io.l2_hint
  -> lsq.io.l2_hint
  -> LoadQueueReplay.io.l2_hint
```

它的作用是通知某个 DCache miss 对应的 L2 数据快返回了，从而 wakeup 因 DCache miss 阻塞的 load replay entry。

需要支持的原因：

- 当前 DCache TileLink Hint/HintAck 已支持，但这是 TileLink A/D 通道语义，不等价于 `io.l2_hint`。
- `io.l2_hint` 会影响 `LoadQueueReplay` 中 DCache miss load 的重放选择和优先级。
- 如果不支持，测试框架无法覆盖 L2 hint wakeup replay load 的场景。

建议优先级：高。

初步实现方向：

- 新增 `l2_hint_agent`，或扩展现有 L2/cache 系统侧 agent。
- 在 connect 中把 `io_l2_hint_valid/sourceId/isKeyword` 从 tie 0 改为 agent drive。
- sequence 中根据 DCache miss sourceId 构造 hint，至少支持：
  - idle drive
  - 指定 sourceId drive
  - `isKeyword=0/1` 两类场景
- 后续和 LoadQueueReplay / miss source 记录联动，避免随机 sourceId 无意义。

### 2.2 L2 TLB request 输入

信号：

```text
io_l2_tlb_req_req_valid
io_l2_tlb_req_req_bits_vaddr
io_l2_tlb_req_req_bits_fullva
io_l2_tlb_req_req_bits_checkfullva
io_l2_tlb_req_req_bits_cmd
io_l2_tlb_req_req_bits_hyperinst
io_l2_tlb_req_req_bits_hlvx
io_l2_tlb_req_req_bits_kill
io_l2_tlb_req_req_bits_memidx_is_ld
io_l2_tlb_req_req_bits_memidx_is_st
io_l2_tlb_req_req_bits_memidx_idx
io_l2_tlb_req_req_bits_isPrefetch
io_l2_tlb_req_req_bits_no_translate
io_l2_tlb_req_req_bits_pmp_addr
io_l2_tlb_req_req_bits_debug_robIdx_flag
io_l2_tlb_req_req_bits_debug_robIdx_value
io_l2_tlb_req_req_bits_debug_isFirstIssue
io_l2_tlb_req_req_kill
```

当前状态：未被 agent 包含，`dut_inst.sv` 中 tie 0。

功能理解：

这是 L2 侧向 MemBlock/MMU 发起的 TLB request 输入。它和当前已有的 `L2tlb_agent` 不是同一个接口。

当前 `L2tlb_agent` 代替的是 L2TLB/PTW responder，连接在 MemBlock 内部 DTLB 到上游 L2TLB/PTW 的位置；而 `io_l2_tlb_req` 是 MemBlock 顶层系统侧输入。

需要支持的原因：

- 如果后续要覆盖 L2 侧发起翻译请求、prefetch translation 或系统侧 TLB 请求流，需要该接口。
- 当前 tie 0 表示这类流完全不可达。

建议优先级：中。

初步实现方向：

- 单独新增 `l2_tlb_req_agent`，不要混入现有 `L2tlb_agent`，避免职责混淆。
- 建立 request transaction，包含 valid、vaddr/fullva/cmd/hyperinst/hlvx/kill/memidx/isPrefetch/no_translate/pmp/debug fields。
- 第一版默认 idle，按 testcase 显式启用。

### 2.3 L2 flush done 输入

信号：

```text
io_l2_flush_done
```

当前状态：未被 agent 包含，`dut_inst.sv` 中 tie 0。

功能理解：

这是外部 L2 flush 完成反馈。MemBlock 内部可能通过 CSR 或控制流触发 L2 flush，`io_l2_flush_done` 用于表示外部 L2 已经完成 flush。

需要支持的原因：

- 当前如果测试需要覆盖 flush L2 相关 CSR/control flow，done 永远为 0，可能无法覆盖完整闭环。
- 若当前 testcase 不启用 L2 flush，该 tie 0 不影响普通 LSQ/issue/writeback flow。

建议优先级：中。

初步实现方向：

- 可和 L2 control/system agent 合并，也可新增轻量 `l2_flush_agent`。
- 支持 idle、固定延迟 done、随机延迟 done 三种模式。
- 参数应放入 seq/test framework 参数管理，默认关闭。

### 2.4 其他低优先级 tie-off 系统输入

当前还看到部分非访存主流程输入 tie 0，例如：

```text
io_resetInFrontendBypass_fromFrontend
io_wfi_wfiReq
io_topDownInfo_fromL2Top_l2Miss
io_topDownInfo_fromL2Top_l3Miss
部分 traceCoreInterfaceBypass 输入
outer_hc_perfEvents 输入
```

当前状态：未形成专门 agent flow。

功能理解：

这些信号主要面向系统集成、trace、WFI、topdown/perf、frontend reset bypass 等场景，不是当前 memblock LSQ/issue/writeback 主验证闭环的核心输入。

建议优先级：低。

初步实现方向：

- 暂不进入第一批实现。
- 如果后续 testcase 需要相关功能，再按独立 agent 或 other_ctrl 扩展方式接入。

## 3. 推荐实现顺序

1. `io_l2_hint`：优先支持，因为它直接影响 LoadQueueReplay 中 DCache miss load 的 hint wakeup/replay 行为。
2. `io_l2_flush_done`：用于补齐 L2 flush 控制闭环。
3. `io_l2_tlb_req`：用于覆盖 L2 侧 TLB request 输入，注意不能和现有 `L2tlb_agent` 混淆。
4. 其他系统/trace/topdown/WFI 输入：按 testcase 需求后续补齐。

## 4. 实施约束

- 新增或修改 agent/connect/sequence 时，需要同步测试框架网页文档和对应 flow 文档。
- 涉及测试框架核心字段或新 transaction 字段时，需要添加中文注释说明字段含义和作用。
- `L2tlb_agent` 的职责边界保持不变：它代替 L2TLB/PTW responder，不负责 `io_l2_tlb_req` 这类顶层 L2 侧输入。
- 默认配置应保持现有测试行为不变：新增 agent 默认 idle/关闭，只有 testcase 或 cfg 显式开启才发送有效请求。
