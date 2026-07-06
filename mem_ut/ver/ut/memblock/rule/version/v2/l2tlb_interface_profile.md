# V2 L2TLB 接口规则 profile

## 语义边界

公共 L2TLB 规则仍然适用：mem_ut `L2TLB_agent` 建模的是上游
DTLB 到 L2TLB 的 request 路径，以及 L2TLB 到 DTLB 的 response 路径。
不得把它作为 L2Cache、PTW 或 memory 下游模型使用。

## 当前状态

V2 生成后的 Verilog 已暴露顶层 `l2_tlb_req_*` request/response 端口和
`l2_pmp_resp_*` 端口。本次迁移 plan 不完成 V2 L2TLB agent 适配。

已观察到的顶层端口族：

```text
build_memblock/rtl/MemBlockTop.sv
  l2_tlb_req_req_ready
  l2_tlb_req_req_valid
  l2_tlb_req_req_bits_vaddr
  l2_tlb_req_resp_valid
  l2_tlb_req_resp_bits_paddr_*
  l2_pmp_resp_ld/st/instr/mmio/atomic
```

## 后续必做项

1. 在生成后的 Verilog 中定位 DTLB 到 L2TLB 的 request/response 路径。
2. 将该路径与 `mem_ut/ver/ut/memblock/tb/*L2tlb*_connect.sv` 对比。
3. 只有在专项 V2 DUT 适配 plan 下，才允许更新 interface、xaction、driver 或 monitor。
