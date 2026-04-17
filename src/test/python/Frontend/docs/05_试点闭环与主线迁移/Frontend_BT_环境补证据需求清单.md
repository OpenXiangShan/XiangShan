# Frontend BT 环境补证据需求清单

本文面向环境 agent，目标只有一个：

- 说明当前哪些 bin 还没有升级到“主线已验收”
- 每个 bin 最少需要补什么 evidence

## 1. 使用原则

环境侧给文档侧补证据时，至少满足下面三项：

1. 有 testcase 名称
2. 有 raw funcov 产物
3. 有可以引用的运行统计或 waveform

如果缺这三项中的任意一项，文档侧一般不会把该点升级成“主线已验收”。

## 2. 当前优先级

### 2.1 第一优先级

这四个点当前已经有历史 standalone evidence，只差主线回放升级：

- `BIN-001`
- `BIN-002`
- `BIN-003`
- `BIN-014`

### 2.2 第二优先级

这两个点当前还处在候选阻塞状态：

- `BIN-007`
- `BIN-013`

### 2.3 第三优先级

当前还处在规划中、没有直接 evidence 的点：

- `BIN-005`
- `BIN-006`
- `BIN-009`
- `BIN-010`
- `BIN-011`

## 3. 每个 bin 需要的最小 evidence

### 3.1 BIN-001

目标：

- `reset_boot_path::seen`

最小 evidence：

1. 主线 testcase
2. raw funcov 命中：
   - `reset_boot_path::seen`
3. 运行统计至少包含：
   - `backend.commit_count >= 1`
   - `monitor.error_count = 0`

### 3.2 BIN-002

目标：

- `fetch_path_type::icache_seq`

最小 evidence：

1. 主线 testcase
2. raw funcov 命中：
   - `fetch_path_type::icache_seq`
3. 运行统计至少包含：
   - `icache.req_count >= 1`
   - `monitor.error_count = 0`

### 3.3 BIN-003

目标：

- `fetch_path_type::mmio_uncache`

最小 evidence：

1. 主线 testcase
2. raw funcov 命中：
   - `fetch_path_type::mmio_uncache`
3. 运行统计至少包含：
   - `backend.commit_count >= 1`
   - `monitor.error_count = 0`

### 3.4 BIN-014

目标：

- `backend_accept_mode::all_block_short`

最小 evidence：

1. 主线 testcase
2. raw funcov 命中：
   - `backend_accept_mode::all_block_short`
3. evidence 中能看到：
   - `blocked_cycles`
4. 运行统计至少包含：
   - `backend.commit_count >= 1`
   - `monitor.error_count = 0`

### 3.5 BIN-007

目标：

- `frontend_exception_type::pf`

最小 evidence：

1. 主线 testcase
2. raw funcov 命中：
   - `frontend_exception_type::pf`
   - `ptw_resp_type::pf`
3. 最好一并命中：
   - `fetch_path_x_exception::icache_x_pf`
4. 需要说明：
   - 当前 monitor / checker 对该点的结论

### 3.6 BIN-013

目标：

- `pmp_exec_result::deny`

最小 evidence：

1. 主线 testcase
2. raw funcov 命中：
   - `pmp_exec_result::deny`
3. 最好一并命中：
   - `pmp_exec_type::deny`
4. 运行统计或日志里能说明：
   - fault 已沿前端异常链路传播

### 3.7 BIN-005 / BIN-006

目标：

- `redirect_type::memVio`
- `redirect_type::interrupt`

最小 evidence：

1. 主线 testcase
2. raw funcov 命中对应 bin
3. 运行统计至少包含：
   - `backend.commit_count >= 1`
   - `monitor.error_count = 0`

### 3.8 BIN-009 / BIN-010 / BIN-011

目标：

- `frontend_exception_type::gpf`
- `frontend_exception_type::ill`
- `frontend_exception_type::hwe`

最小 evidence：

1. 主线 testcase
2. raw funcov 命中对应异常 bin
3. 最好给出：
   - 对应 `cause`
   - 对应 `pc`
   - monitor/异常链路结论

## 4. 当前环境侧最划算的补证顺序

如果只看“投入最小、文档收益最大”，建议环境侧按下面顺序补：

1. `BIN-001`
2. `BIN-002`
3. `BIN-003`
4. `BIN-014`
5. `BIN-013`
6. `BIN-007`

原因：

1. 前四个点已经有历史闭环基础，最容易升级成“主线已验收”。
2. `BIN-013` 和 `BIN-007` 虽然重要，但当前一个缺 testcase / artifact，一个 testcase 路径没打通，投入更高。

## 5. 当前结论

文档侧当前最希望环境侧补的，不是再扩一批全新点，而是先把这四个历史闭环点升级成主线闭环：

- `BIN-001`
- `BIN-002`
- `BIN-003`
- `BIN-014`

只要这四个点升级完成，当前 active `L0` bin 的主线已验收数量就能从 `2` 提升到 `6`。
