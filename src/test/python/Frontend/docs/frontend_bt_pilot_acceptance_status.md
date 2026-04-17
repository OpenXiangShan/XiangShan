# Frontend BT Pilot Acceptance Status

本文档用于给出 `BIN-004` 与 `BIN-012` 在当前主线中的经理可验收状态口径。

统计时间：

- 2026-04-17

适用目录：

- `src/test/python/Frontend/`

## 1. 证据口径

本轮状态判断同时基于以下三类证据：

1. 主线 testcase 与 funcov recorder 的静态核对
   - `src/test/python/Frontend/tests/test_functional_coverage_pilot.py`
   - `src/test/python/Frontend/env/functional_coverage.py`
2. 当前 active CSV
   - `src/test/python/Frontend/docs/frontend_bt_functional_coverage_pilot.csv`
3. 当前主控目录真实 DUT 产物
   - `/nfs/home/jiabowen/ai_workspace/XiangShan_frontend_bt_jiabowen/src/test/python/Frontend/data/test_bin004_backend_ctrl_redirect_pilot.*`
   - `/nfs/home/jiabowen/ai_workspace/XiangShan_frontend_bt_jiabowen/src/test/python/Frontend/data/test_bin012_itlb_miss_walk_refill_pilot.*`

## 2. 总体结论

| Bin | 当前主线 testcase | 当前状态 | 结论 |
| --- | --- | --- | --- |
| BIN-004 | `test_bin004_backend_ctrl_redirect_pilot` | 主线真实 DUT 证据已存在 | 可验收 |
| BIN-012 | `test_bin012_itlb_miss_walk_refill_pilot` | 主线真实 DUT 证据已存在，但带 checker 边界说明 | 可验收 |

这里的结论不再以 doc worktree 自己是否可执行为准，而是以主控目录真实 DUT 证据为准。

## 3. BIN-004 状态

### 3.1 范围

- 主 bin：`BIN-004 redirect_type::ctrl`
- 当前 testcase：`test_bin004_backend_ctrl_redirect_pilot`

### 3.2 按设计应形成的证据链

1. 加载一段最小程序镜像。
2. DUT 至少产生 6 次 commit。
3. 注入一次 backend ctrl redirect。
4. monitor 观察到 redirect 目标 PC。
5. monitor 无错误。
6. funcov 命中 `redirect_type::ctrl`。

### 3.3 当前主线证据

当前主控目录 raw funcov 关键证据：

- `redirect_type::ctrl`
  - `hits = 2`
  - `first_cycle = 583`
- monitor 统计
  - `redirect_count = 1`
  - `error_count = 0`
- backend 统计
  - `commit_count = 11`
- branch 统计
  - `jump = 1`

因此，当前 `BIN-004` 已经具备：

1. 合法 testcase 激励
2. 预期 bin 命中
3. monitor 无错误
4. 可回查的 waveform / funcov / dat 产物

### 3.4 验收判定

- 当前判定：通过
- 当前口径：完整场景闭环

## 4. BIN-012 状态

### 4.1 范围

- 主 bin：`BIN-012 itlb_ptw_flow::miss_walk_refill`
- 联动 L1 bin：
  - `BIN-109 itlb_result_type::miss`
  - `BIN-110 ptw_resp_type::leaf_pte`
  - `BIN-203 itlb_state_x_ptw_resp::single_miss_x_leaf_pte`
- 当前 companion 命中：
  - `BIN-008 frontend_exception_type::af`
- 当前 testcase：`test_bin012_itlb_miss_walk_refill_pilot`

### 4.2 按设计应形成的证据链

1. 以 sv39 模式配置 identity mapping。
2. DUT 至少产生 6 次 commit。
3. 至少观察到 1 次 PTW request。
4. 至少观察到 1 次 PTW response。
5. funcov 命中：
   - `itlb_result_type::miss`
   - `ptw_resp_type::leaf_pte`
   - `itlb_ptw_flow::miss_walk_refill`
6. 当前 testcase 还要求显式暴露已知边界：
   - monitor 报错存在
   - 且错误类型全部为 `INSTR_MISMATCH`

### 4.3 当前主线证据

当前主控目录 raw funcov 关键证据：

- `frontend_exception_type::af`
  - `bin_id = BIN-008`
  - `hits = 5`
- `itlb_ptw_flow::miss_walk_refill`
  - `bin_id = BIN-012`
  - `hits = 1`
  - `first_cycle = 567`
- `itlb_result_type::miss`
  - `bin_id = BIN-109`
  - `hits = 2`
- `ptw_resp_type::leaf_pte`
  - `bin_id = BIN-110`
  - `hits = 1`
- `itlb_state_x_ptw_resp::single_miss_x_leaf_pte`
  - `bin_id = BIN-203`
  - `hits = 1`

运行统计：

- `ptw.req_count = 1`
- `ptw.resp_count = 1`
- `backend.commit_count = 6`
- `monitor.error_count = 144`

当前 `errors` 中记录的是 `INSTR_MISMATCH`。因此当前应这样描述这个点：

1. miss -> walk -> refill 路径已经打通。
2. `BIN-012` 主 bin 与联动 L1 bin 已命中。
3. 当前 raw funcov 中伴随命中的异常类型是 `BIN-008 frontend_exception_type::af`，不是 `BIN-007 pf`。
4. 这个 `af` 命中当前只作为 `BIN-012` 场景下的 companion 证据保留，不单独升级为 `BIN-008` 的独立验收结论。
5. 当前仍带 translation-aware checker 边界，不能表述成完整指令值正确性闭环。

### 4.4 当前稳定话术

后续文档在描述 `BIN-012` 时，统一使用下面口径：

1. `BIN-012` 已通过功能/协议闭环。
2. 当前主线证据已证明：
   - ITLB miss 发生
   - PTW request / response 成立
   - leaf pte 返回成立
   - refill 路径成立
3. 当前 raw funcov 伴随命中的异常类型是 `frontend_exception_type::af`。
4. 在环境 agent 给出新结论前，不把这个点写成 `pf`。
5. 当前 `INSTR_MISMATCH` 仍按 translation-aware checker 边界处理。

### 4.5 验收判定

- 当前判定：通过
- 当前口径：功能/协议闭环通过，完整指令值正确性闭环待 checker 继续完善

## 5. 当前仍依赖环境 agent 的事项

这两个 pilot 点当前已经具备验收口径，但仍有后续环境结论需要继续回灌：

1. `BIN-012` 的 `INSTR_MISMATCH` 边界何时从 checker 已知限制收敛为完整正确性闭环。
2. 后续新的 L0/L1 bin 打通后，对应 evidence 如何持续补入当前目录的闭环文档。
