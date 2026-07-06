# mem_base_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/mem_base_sequence.sv`

## 1. 文件定位与使用场景

该文件提供真实 DUT flow 中的简化 memory responder。它不消费 dispatch 主表，也不参与 `uid`、ROB、LQ/SQ 状态推进；它的职责是响应 DUT 通过 DCache 或 SBuffer agent 发出的 memory 访问，让真实 load/store 路径有可返回的数据或 ack。

文件内有三层：

- `mem_access_base_sequence`：公共稀疏 memory model，保存 line 数据、地址范围和读写访问工具。
- `dcache_mem__access_base_sequence`：挂在 `u_dcache_agent_agent` 上，响应 DCache/TileLink A 通道请求并驱动 D 通道 response。
- `sbuffer_mem_access_base_sequence`：挂在 `u_sbuffer_agent_agent` 上，响应 SBuffer A 通道请求并驱动 D 通道 response。

## 2. 调度关系与参数数据流

`tc_base.sv` 默认把 `dcache_mem__access_base_sequence` 挂到 `env.u_dcache_agent_agent.sqr.main_phase`，把 `sbuffer_mem_access_base_sequence` 挂到 `env.u_sbuffer_agent_agent.sqr.main_phase`。它们和 dispatch 的 LSQENQ/LINTSISSUE/LSQCOMMIT/L2TLB sequence 并行运行。

这两个 responder 不依赖 `MEMBLOCK_LSQENQ_SEQ_EN`、`MEMBLOCK_DISPATCH_ISSUE_SEQ_EN`、`MEMBLOCK_LSQCOMMIT_SEQ_EN` 或 `MEMBLOCK_L2TLB_SEQ_EN`。它们主要依赖：

- agent virtual interface 是否能从 `uvm_config_db` 取得。
- `rst_n == 1'b1`。
- `memblock_sync_pkg::reset_backend_done == 1'b1`。
- DUT 对应 A 通道是否 valid。

数据流如下：

| 阶段 | 输入 | 处理 | 输出 |
|---|---|---|---|
| 空闲驱动 | 无有效 A 请求 | 发送 idle xaction，保持 response 侧无效或 ready 默认值 | 不改变 memory |
| 接收请求 | DUT A 通道 opcode/size/source/address/mask/data | 从 vif 采样请求字段，按 opcode 判断 load/store/acquire | request xaction 快照 |
| 访问 memory | address、mask、store data | 用 line address 访问 `main_mem[]`，不存在的 line lazy 初始化 | load data、denied、corrupt |
| 发送 response | request source/size/opcode、load data | 构造 D 通道 response，等待 DUT ready | D 通道 xaction |

## 3. dcache responder

`dcache_mem__access_base_sequence` 面向 DCache agent 的 TileLink 类接口。它支持基本 opcode 映射：

| A opcode | 语义 | D opcode |
|---|---|---|
| `0/1` | PutFullData / PutPartialData | AccessAck |
| `4` | Get | AccessAckData |
| `5` | Hint | HintAck |
| `6` | AcquireBlock | GrantData |
| `7` | AcquirePerm | Grant |

AcquireBlock 可能按 size 拆成多个 32B beat。每个 beat 都从 32B 对齐地址读取或写入稀疏 memory，并在 D 通道 ready 后继续下一 beat。

关键函数/task：

- `build_dcache_idle_xaction()`：构造 idle response。
- `capture_dcache_a_xaction()`：从 `dcache_vif` 采样 A 通道请求。
- `dcache_d_opcode()`：把 A opcode 转成 D opcode。
- `dcache_d_beats()`：计算 acquire 需要返回的 beat 数。
- `dcache_mem_access_task()`：把 32B DCache beat 映射到公共 memory line 访问。
- `dcache_mem_access_xaction()`：构造 D 通道 response xaction。
- `body()`：持续 idle，发现 A valid 后 accept 请求并发送 response。

## 4. sbuffer responder

`sbuffer_mem_access_base_sequence` 面向 SBuffer agent。它处理 8B 粒度访问：

- store opcode 返回 ack，不带 load data。
- load opcode 返回 64bit load data。
- 地址按 8B 对齐后访问公共 sparse memory。

关键函数/task：

- `build_sbuffer_idle_xaction()`：构造 idle response。
- `capture_sbuffer_a_xaction()`：从 `sbuffer_vif` 采样 A 通道请求。
- `sbuffer_mem_access_task()`：把 8B SBuffer beat 映射到公共 memory line 访问。
- `sbuffer_mem_access_xaction()`：构造 D 通道 response xaction。
- `body()`：持续 idle，发现 A valid 后 accept 请求并等待 D ready。

## 5. 与 dispatch 主流程的边界

该文件只保证真实 DUT 访存路径不因为外部 memory response 缺失而卡住。它不会：

- 生成 dispatch 主表。
- 分配或释放 `lqIdx/sqIdx`。
- 更新 `status_by_uid[]`。
- 判断 load/store 是否 pass。
- 处理 replay/redirect/commit。

这些状态仍由 `memblock_lsqenq_dispatch_sequence`、`memblock_lintsissue_dispatch_sequence`、monitor adapter、writeback handler 和 `memblock_lsqcommit_dispatch_sequence` 负责。
