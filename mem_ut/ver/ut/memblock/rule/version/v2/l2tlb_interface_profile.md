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
