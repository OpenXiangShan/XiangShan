# V2 L2TLB 接口规则 profile

## 语义边界

公共 L2TLB 规则仍然适用：mem_ut `L2TLB_agent` 建模的是上游
DTLB 到 L2TLB 的 request 路径，以及 L2TLB 到 DTLB 的 response 路径。
不得把它作为 L2Cache、PTW 或 memory 下游模型使用。

## 当前状态

V2 生成后的 `build_memblock/rtl/MemBlock.sv` 已暴露顶层
`io_l2_tlb_req_*` request/response 端口和 `io_l2_pmp_resp_*` 端口。顶层
`io_l2_tlb_req_*` 是 L2/L2Cache 侧向
MemBlock 内部 TLB/L2TLB 发起地址查询的 requestor 口，不是 mem_ut
`L2TLB_agent` 的接管点。

当前 mem_ut `L2TLB_agent` 的 V2 接管点是生成后 `MemBlock.sv` 内部
`dtlbRepeater` 与 `inner_ptw`/L2TLB 的交接信号：

```text
request:
  _inner_dtlbRepeater_io_ptw_req_0_valid
  _inner_dtlbRepeater_io_ptw_req_0_bits_vpn
  _inner_dtlbRepeater_io_ptw_req_0_bits_s2xlate

response:
  _inner_ptw_io_tlb_1_req_0_ready
  _inner_ptw_io_tlb_1_resp_*
```

其中 `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u` 必须由
`L2TLB_agent` 的 transaction/sequence 真实驱动。当前实现链路为：

```text
memblock_tlb_entry.pte_g/pte_u
  -> memblock_l2tlb_base_sequence::fill_dtlb_resp_from_entry()
  -> L2tlb_agent_agent_xaction.io_ptw_resp_bits_s2_entry_perm_g/u
  -> L2tlb_agent_agent_driver / L2tlb_agent_agent_interface
  -> L2tlb_agent_connect.sv active 分支
  -> _inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u
```

不得在 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` 的 active 接管路径中把
`s2_entry_perm_g/u` 固定为常量 0。

## 多 Outstanding 与 Response 次序

V2 `MemBlock.scala` 在该接管点使用 `PTWNewFilter`。当前配置的 load/store/prefetch filter
分别保存 16/8/8 个 entry，`l2tlbParams.dfilterSize=32`；前一笔 response 返回前可以继续产生
后续 request fire，因此该接口不能按 single-outstanding 建模。

`PtwReq` 只有 `vpn/s2xlate`，没有 request ID。`PtwRespS2` 返回后，filter 对全部有效 entry
按 `s2xlate + hit(vpn, asid, vasid, vmid)` 匹配，不只比较 FIFO head。真实 L2TLB response
又可来自 page-cache hit、PTW FSM 和 LLPTW/miss queue，不同路径通过 per-source `mergeArb`
直接仲裁，没有按 request 接收顺序重排。因此 V2 支持按 response 内容命中 outstanding request，
验证 responder 可以提供默认顺序和显式乱序两种回复模式。

response 侧没有暴露 ready，Scala 将 `io.ptw.resp.ready` 固定为 true。验证环境驱动
`resp_valid=1` 的 sample 边界即完成该 response，但仍必须保存 driving slot 到该边界后才能从
outstanding 账本删除。

outstanding 的权威单位是该接口上的每次 request fire，不是唯一 lookup key。同一个 filter 内的重复
key 在到达本接口前已合并；跨 load/store/prefetch filter 的相同 key 仍可分别 fire。真实 L2TLB 对每次
fire/response 独立计数，LLPTW 只共享重复请求的 memory wait，不合并其 entry 或最终 output。因此
验证 responder 必须为每次 accepted fire 保存独立 token；正常未被 reset/flush cancel 的 token 各返回
一次，不得按 key 合并，canceled token则必须显式记账。latency 档只定义 `due_sample` 的最早可响应拍；
ordered head blocking 或单 response 端口竞争可以让实际
`complete_sample` 更晚，但不得早于 due。

V2 顶层 CSR/fence agent 的观测点到 DTLB filter 清空不是2拍。`MemBlock.scala` 先把
`io.ooo_to_mem.sfence/tlbCsr` 经过两级 `RegNext`，`PTWNewFilter` 内部再使用
`ldtlbParams.fenceDelay=2`，因此 responder 从顶层 monitor event 开始必须按4拍总延迟 hold ready。
L2TLB lifecycle 专项 coding 后使用 `MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES=4` 表达该
observer-to-filter 合同；在专项落地前该宏尚未实现。实现时不得把内部 `fenceDelay` 单独作为 ready
恢复边界。`MEMBLOCK_DUT_L2TLB_DFILTER_SIZE=32` 和该hold宏均须在公共dispatch types中建立typed
localparam，业务逻辑不直接散落展开compile宏。

ready已经开放后，新flush event的monitor `sample_time`必须等于sequence当前sample；迟到event表示
sideband服务合同失效，必须在任何queue/counter变化前fatal，不能从当前拍重新锚定并误杀flush后
request。只有reset/startup且ready从未开放时，才允许把较早latest event作为baseline并从当前拍
保守hold 4拍。

CSR monitor必须在post-reset sample无条件发布non-destructive runtime CSR latest snapshot；原semantic
raw capture gate保持不变，两条latest视图共享统一snapshot sequence和公共`mmu_csr_state`。这样legacy
`tc_base`无需主表flow先打开capture gate也能取得CSR。responder取得首份有效snapshot之前必须保持
ready为0，不能用未初始化ASID/VMID生成lookup key；该等待期不能触发idle-stop，但仍处理flush和global
stop。monitor的逐拍CSR baseline必须独立于capture gate更新，semantic latest clear后下一gate sample必须
重新发布。legacy `tc_base` agent default sequence和
`basicTest + VSEQ_MAIN`显式sequence是两种分别合法的启动拓扑；同一testcase混用时必须拒绝第二个
lifecycle owner。

详细源码依据见：

- `AI_DOC/analysis/rtl/v2/flows/dtlb_l2tlb_request_response_ordering_flow.md`
- `AI_DOC/analysis/interface/v2/agents/l2tlb_agent.md`

这些是生成后 Verilog 的内部 wire 名，随 RTL 重新生成可能变化。后续更新 V2
RTL 后必须重新检查权威 `build_memblock/rtl/MemBlock.sv` 中这些内部信号是否仍存在。

已观察到的顶层端口族：

```text
build_memblock/rtl/MemBlock.sv
  io_l2_tlb_req_req_ready
  io_l2_tlb_req_req_valid
  io_l2_tlb_req_req_bits_vaddr
  io_l2_tlb_req_resp_valid
  io_l2_tlb_req_resp_bits_paddr_*
  io_l2_pmp_resp_ld/st/instr/mmio/atomic
```

## 后续必做项

1. 更新 V2 RTL 后重新定位 DTLB 到 L2TLB 的 request/response 路径。
2. 将该路径与 `mem_ut/ver/ut/memblock/tb/*L2tlb*_connect.sv` 对比。
3. 不得把顶层 `io_l2_tlb_req_*` 重新作为 `L2TLB_agent` 接管点。
4. 只有在专项 V2 DUT 适配 plan 下，才允许更新 interface、xaction、driver 或 monitor。
5. 多 outstanding、ordered/reorder、加权 latency 和 reset/flush/stop 生命周期统一由
   `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_l2tlb_response_permission_adapt_execution_plan_20260708.md`
   执行；不得在其它 plan 再建第二个 L2TLB request queue 或 ready owner。
