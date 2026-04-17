# T08 API 层设计

## 1. 目标

构建面向测试用例的稳定 API，屏蔽底层时序细节，遵循 `api_{DUT}_{function}` 命名规范。

## 2. FrontendEnv 分层

建议层次：

1. `dut` fixture：创建 DUT、初始化时钟、注册覆盖率采样。
2. `env` fixture：聚合 Memory/Agents/Monitor/Backend/Trace。
3. `full_env` fixture：预加载程序和默认页表，直接可跑场景。

## 3. 核心 API

### 3.1 `api_Frontend_load_program`

```python
api_Frontend_load_program(env, bin_data, base_addr, max_cycles=1000)
```

行为：

- 写入 `MemoryModel`。
- 必要时更新页表映射。
- 推进若干周期完成生效。

### 3.2 `api_Frontend_run_until_commit`

```python
api_Frontend_run_until_commit(env, target_count, max_cycles=10000) -> int
```

行为：

- 循环 `env.Step(1)`。
- 调用 `BackendModel.wait_for_commits` 判断达标。
- 返回实际 commit 数。

### 3.3 `api_Frontend_inject_redirect`

```python
api_Frontend_inject_redirect(env, target_pc, reason, max_cycles=100) -> bool
```

行为：

- 调用 `BackendModel.inject_redirect()`。
- 在超时前轮询 monitor 是否观察到目标 PC。

### 3.4 `api_Frontend_check_pc_sequence`

```python
api_Frontend_check_pc_sequence(env, expected_pcs, max_cycles=5000) -> bool
```

行为：

- 按顺序消费 monitor 观测 PC。
- 与 `expected_pcs` 对齐，超时返回 False。

### 3.5 `api_Frontend_get_branch_stats`

```python
api_Frontend_get_branch_stats(env) -> dict
```

返回：

- `total_branch`, `mispredict`, `mpki`, `by_type`。

## 4. 设计约束

- API 第一个参数固定为 `env`。
- 对时序等待类 API，最后参数固定 `max_cycles`。
- API 内部统一做 timeout 保护，超时抛 `TimeoutError` 或返回 False。

## 5. 错误模型

- `ValueError`：参数非法（地址未对齐、target_count<0）。
- `TimeoutError`：周期耗尽未达预期。
- `RuntimeError`：检测到 monitor 严重错误（协议死锁等）。

## 6. 单元测试验收

每个核心 API 至少 1 个独立测试：

1. 正常路径成功。
2. 超时路径可被捕获。
3. 错误输入能稳定报错。
