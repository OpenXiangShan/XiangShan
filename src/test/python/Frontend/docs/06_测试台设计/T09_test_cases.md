# T09 测试用例分层组织设计

## 1. 目标

形成可回归、可扩展、可定位问题的前端黑盒测试集。

## 2. 测试文件规划

- `test_basic_fetch.py`
- `test_icache_behavior.py`
- `test_branch_prediction.py`
- `test_exception_interrupt.py`
- `test_redirect.py`
- `test_stress.py`

## 3. 分层策略

### 3.1 L0 基础功能（快速）

- 顺序取指
- 跨 cacheline
- RVC 与 reset vector

目标：秒级反馈，阻断基础回归失败。

### 3.2 L1 行为功能（中速）

- ICache 命中/缺失延迟
- Fence.I / SFence
- redirect 注入与恢复

目标：覆盖主要控制流与接口交互。

### 3.3 L2 复杂场景（慢速）

- 分支密集 + MPKI 检查
- 异常/中断时序检查
- 混合随机压力

目标：验证鲁棒性和性能指标。

## 4. 每文件核心用例

## 4.1 `test_basic_fetch.py`

- `test_sequential_fetch`
- `test_cross_cacheline_fetch`
- `test_rvc_fetch`
- `test_reset_vector_fetch`

## 4.2 `test_icache_behavior.py`

- `test_icache_hit_latency`
- `test_icache_miss_recovery`
- `test_miss_rate_impact`
- `test_fencei_invalidate`

## 4.3 `test_branch_prediction.py`

- `test_conditional_branch_mix`
- `test_nested_call_ret`
- `test_branch_mpki_threshold`

## 4.4 `test_exception_interrupt.py`

- `test_illegal_instruction_exception`
- `test_page_fault_redirect`
- `test_exception_response_latency`

## 4.5 `test_redirect.py`

- `test_mispredict_redirect`
- `test_nested_redirect`
- `test_redirect_priority`

## 4.6 `test_stress.py`

- `test_million_instruction_random`
- `test_miss_storm`
- `test_redirect_storm`
- `test_mixed_stress`

## 5. 回归分组建议

- `smoke`：L0 + 部分 L1（5~10 分钟）
- `daily`：L0+L1 全量 + 轻量 L2（30~60 分钟）
- `weekly`：全量 + 百万指令压力（小时级）

## 6. 验收门禁

- `basic/icache/redirect` 用例通过率 100%
- `MPKI < 10`
- 异常响应延迟 `<= 5` 周期
- 百万指令场景无 monitor error

## 7. 失败定位输出

每个用例失败时至少输出：

- 最近 32 条观测 PC/指令
- 最近 16 条 backend 事件（commit/redirect/resolve）
- 关键覆盖组命中摘要
