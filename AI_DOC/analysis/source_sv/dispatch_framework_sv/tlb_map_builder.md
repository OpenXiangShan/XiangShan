# tlb_map_builder.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/tlb_map_builder.sv`

## 1. 文件定位与使用场景

TLB entry 生成 helper。它把 DTLB -> L2TLB request 中的 `vpn/s2xlate` 和当前 CSR 上下文变成 L2TLB responder 可用的 by-key TLB entry。

输入是 request `vpn/s2xlate`、PTE 权重和 runtime CSR。输出是 `memblock_tlb_entry`；公共 owner 将 entry 存入 `tlb_entry_by_key[key]`。uid 只通过 `uid_tlb_record_by_uid[]` 做发射上下文和 PTE 回填追踪。

控制逻辑字段是 lookup key；PTE bit、PPN、PBMT 主要是 response payload。`tlb_mapped` 不在 builder 内部更新，而是在 `common_data_transaction::update_uid_tlb_records_by_entry()` 成功把 entry 回填到 uid record 后更新。

函数：

- `build_tlb_entry_for_req(vpn,s2xlate,csr_state)`：按 request 构造虚拟页地址，调用 `choose_paddr()`，随机 PTE bit，合法化，应用传入的 runtime CSR，并返回 by-key entry。
- `choose_paddr(vaddr)`：按 `paddr_base/paddr_range` 将 VPN 映射到物理页，并保留 page offset。
- `randomize_pte_bits(tlb_tr)`：按 `seq_csr_common` 权重生成 PTE bit。
- `choose_weighted_bit(one_wt,zero_wt)`：权重选择。

## 2. 字段与函数/task 设计原理

`tlb_map_builder` 负责把 request 地址和传入的 CSR 上下文转换成 by-key TLB entry。它不直接驱动 L2TLB agent，也不维护 uid 索引；公共数据层需要建表时统一调用这个 helper，避免在多个 class 中散落重复的 PTE 随机和 paddr 选择逻辑。

| 字段/函数 | 参数 | 功能和设计原理 |
|---|---|---|
| `build_tlb_entry_for_req(vpn,s2xlate,csr_state)` | DTLB request 的 VPN、二阶段翻译类型、runtime CSR 快照 | 选择 paddr，随机 PTE，修正合法性，应用 runtime CSR，返回 `memblock_tlb_entry`。 |
| `choose_paddr(vaddr)` | 虚拟地址 | 在 `paddr_base/range` 内按 VPN 混合选择物理页，保留页内 offset。这样地址稳定可复现，同时覆盖多物理页。 |
| `randomize_pte_bits(entry)` | TLB entry | 按 `seq_csr_common` 中 PTE 权重生成权限位。 |
| `choose_weighted_bit(one_wt,zero_wt)` | 1/0 权重 | 小型权重随机工具，全 0 时 fatal，避免无定义随机。 |
