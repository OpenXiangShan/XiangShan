# T03 Uncache 与 PTW Agent 设计

## 1. 目标

实现两个独立代理：

- `UncacheAgent`：处理 `auto_inner_instrUncache_client_out_*`。
- `PTWAgent`：处理 `io_ptw_req_0_*` / `io_ptw_resp_*`。

二者都遵循 `bind(dut) + on_clock_edge()` 驱动模式。

## 2. UncacheAgent

### 2.1 接口

- `bind(dut) -> None`
- `configure(latency=2) -> None`
- `on_clock_edge(cycle: int) -> None`

### 2.2 请求与响应

请求条件：`uncache_a.valid && uncache_a.ready`

采集字段：

- `bits_address`
- `bits_mask`

返回：单 beat 32B（256-bit）

- `uncache_d.valid = 1`
- `bits_data = read_block(addr_aligned_32B, 32)`
- `bits_denied = 0`, `bits_corrupt = 0`

### 2.3 MMIO 规则

- `MemoryModel.is_mmio(addr)` 为真时走 uncache 路径。
- 可配置 `mmio_latency`（默认大于普通 uncache）。

## 3. PTWAgent

### 3.1 接口

- `bind(dut) -> None`
- `configure(latency=3, mode="bare") -> None`
- `on_clock_edge(cycle: int) -> None`

### 3.2 请求处理

请求条件：`ptw_req.valid && ptw_req.ready`

输入字段：

- `io_ptw_req_0_bits_vpn`
- `io_ptw_req_0_bits_s2xlate`

输出字段（最小集）：

- `io_ptw_resp_valid`
- `io_ptw_resp_bits_s1_entry_v`
- `io_ptw_resp_bits_s1_entry_ppn`
- `io_ptw_resp_bits_s1_entry_level`
- `io_ptw_resp_bits_s1_entry_perm_x`
- `io_ptw_resp_bits_s1_entry_perm_r`

### 3.3 模式语义

- Bare：`ppn = vpn`, `v=1`（恒等映射）。
- Sv39：调用 `PageTableModel.build_ptw_resp(vpn)`。

## 4. 共享调度结构

两个 Agent 都维护：

```text
pending_responses: deque[{ready_cycle, payload}]
```

每周期：

1. 默认拉高 `ready`（可配置背压概率）。
2. 采集新请求并入队。
3. 若队首到期，驱动对应 `*_d`/`ptw_resp`。

## 5. 验收标准

1. Uncache 请求后 `latency` 周期返回单 beat 数据。
2. PTW 请求后 `latency` 周期返回合法页表项。
3. Bare 与 Sv39 切换后响应语义正确。
4. 同时存在 Uncache 与 PTW 压力时无互锁。
