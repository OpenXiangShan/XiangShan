# mmu_csr_runtime_state.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/mmu_csr_runtime_state.sv`

## 1. 文件定位与使用场景

运行时 MMU CSR 镜像，不是 plus 配置。它存在的原因是 L2TLB request 只带 `vpn/s2xlate`，要正确查到哪个 uid，还需要当前 `asid/vmid/priv` 等 CSR 上下文。这个上下文来自 DUT monitor，而不是 testcase 静态配置。

输入是 `csr_ctrl_agent_agent_xaction` 或 `memblock_sync_pkg::dispatch_raw_csr_t`。输出是 `make_lookup_key(vpn,s2xlate)` 和当前 CSR getter，供 TLB builder 注册 lookup key、L2TLB responder 查表。

控制逻辑字段是 `update_seq`：CSR 一变就递增，用于调试和追踪 CSR runtime 镜像是否更新。当前 TLB lookup 不再因为 `update_seq` 自动清索引或拒绝命中，进程/阶段差异由 lookup key 中的 ASID/VMID/S2xlate 区分。

字段：

- `satp/vsatp/hgatp` 的 mode/asid/vmid/ppn。
- privilege 和权限相关位：`priv_virt`、`priv_spvp`、`priv_imode`、`priv_dmode`、`priv_mxr/sum/vmxr/vsum`、`m_pbmt_en/h_pbmt_en`。
- `update_seq`：CSR 变化序列号，用于 TLB lookup 一致性。

函数：

- `reset()`：恢复 baseline。
- `update_from_csr_ctrl(csr_tr)`：从 `csr_ctrl_agent_agent_xaction` 更新。
- `update_from_raw_csr(raw)`：从 `memblock_sync_pkg::dispatch_raw_csr_t` 更新。
- `current_s2xlate_enabled()`、`current_asid(s2xlate)`、`current_vmid()`、`current_priv_mode(use_dmode)`：提供当前翻译上下文。
- `make_lookup_key(vpn,s2xlate)`：生成 `{vpn,asid,vmid,s2xlate}`。

## 2. 字段与函数/task 设计原理

`memblock_tlb_entry` 是按 lookup key 唯一的 live TLB/PTE 表项，`memblock_uid_tlb_record` 保存每个 uid 的发射上下文和 PTE 回填历史，`mmu_csr_runtime_state` 是运行时 MMU CSR 快照。三者分开是因为 entry 命中、uid 追踪和 CSR 真值属于不同生命周期。

`mmu_csr_runtime_state` 关键字段和函数：

| 字段/函数 | 含义 | 设计原理 |
|---|---|---|
| `satp_*`、`vsatp_*`、`hgatp_*` | S1/VS/G stage 翻译上下文 | L2TLB lookup 必须使用 DUT 当前上下文，而不是 testcase 初始配置。 |
| `priv_virt/spvp/imode/dmode/mxr/sum/vmxr/vsum` | 当前权限状态 | 权限位会影响翻译和 response，保留运行时快照便于后续专项扩展。 |
| `m_pbmt_en/h_pbmt_en` | PBMT 相关开关 | 配合主表 PBMT 字段，为后续 PMA/PBMT 专项保留入口。 |
| `update_seq` | CSR 更新版本号 | 每次 CSR 变化递增；公共 TLB lookup index 会在版本变化后清空，避免 stale hit。 |
| `reset()` | 初始化 CSR 默认状态 | 保证每个 testcase 从确定状态开始。 |
| `update_from_csr_ctrl(csr_tr)`、`update_from_raw_csr(raw)` | 从 agent transaction 或 raw monitor 更新 CSR | 支持直接 agent 路径和同步包 raw event 路径，变更时递增 `update_seq`。 |
| `current_s2xlate_enabled()`、`current_asid(s2xlate)`、`current_vmid()`、`current_priv_mode(use_dmode)` | 查询当前翻译上下文 | TLB builder 不直接理解 CSR 字段布局，只通过这些 API 获取语义值。 |
| `make_lookup_key(vpn, s2xlate)` | 构造查表 key | 统一 key 生成规则，避免 builder 和 responder 对 ASID/VMID 组合理解不一致。 |
