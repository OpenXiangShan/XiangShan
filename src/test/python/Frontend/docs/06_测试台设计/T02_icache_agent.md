# T02 ICache Agent 设计

## 1. 目标

实现 `ICacheAgent`，驱动 `auto_inner_icache_client_out_*` 接口，模拟 TileLink 指令缓存响应：

- A 通道请求握手采集。
- D 通道按 2-beat 返回 64B cacheline（每 beat 32B）。
- 支持命中/缺失延迟与缺失概率配置。

## 2. 接口定义

- `bind(dut) -> None`
- `configure(hit_latency=1, miss_latency=20, miss_rate=0.0) -> None`
- `on_clock_edge(cycle: int) -> None`
- `get_stats() -> dict`

依赖：`MemoryModel`。

## 3. 协议建模

### 3.1 A 通道采集

触发条件：`a.valid == 1 and a.ready == 1`

采集字段：

- `source = auto_inner_icache_client_out_a_bits_source`
- `addr   = auto_inner_icache_client_out_a_bits_address`

行为：

- `addr` 对齐到 64B。
- 读取 cacheline，拆分为 `beat0/beat1`。
- 入队 pending 响应。

### 3.2 D 通道发送

每个请求发送两个 beat：

- beat0：低 32B
- beat1：高 32B

D 通道字段约定：

- `bits_opcode = 1` (AccessAckData)
- `bits_source = request.source`
- `bits_denied = 0`
- `bits_corrupt = 0`

## 4. 时序状态机

```text
IDLE -> WAIT_LATENCY -> SEND_BEAT0 -> SEND_BEAT1 -> IDLE
```

- `WAIT_LATENCY` 周期数取决于命中/缺失。
- 同周期只发送 1 beat，保证时序可观测。

## 5. 配置与随机化

`configure()` 支持：

- `hit_latency`：命中返回延迟。
- `miss_latency`：缺失返回延迟。
- `miss_rate`：0.0~1.0，按请求粒度采样。

可选扩展参数（预留）：

- `max_outstanding`：限制 pending 深度。
- `seed`：固定随机序列可复现。

## 6. 数据结构

```text
pending: deque[
  {
    source: int,
    addr: int,
    beat0: int,
    beat1: int,
    ready_cycle: int,
    beat_idx: int
  }
]
```

统计项：

- `req_count`, `resp_beat_count`, `resp_line_count`
- `miss_count`, `max_pending_depth`

## 7. StepRis 注册建议

由 `FrontendEnv` 统一注册顺序：

1. `ICacheAgent.on_clock_edge`
2. `UncacheAgent/PTWAgent.on_clock_edge`
3. `Monitor/BackendModel.on_clock_edge`

确保请求采集早于 monitor 统计，减少 off-by-one。

## 8. 错误处理

- pending 超限：记录 `agent_overflow` 并 fail-fast。
- source 冲突（同 source 未完成又发起新请求）：记录协议告警。
- 1000 周期无响应进展：触发死锁告警。

## 9. 验收标准

1. 对单次请求准确返回 2 beat，顺序正确。
2. `hit_latency`/`miss_latency` 生效，统计值可观测。
3. 1000 周期随机流量无死锁，pending 可回落到 0。
