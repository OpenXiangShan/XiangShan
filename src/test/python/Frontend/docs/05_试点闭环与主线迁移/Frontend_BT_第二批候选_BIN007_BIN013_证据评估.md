# Frontend BT 第二批候选证据评估

本文用于评估第二批常被优先提到、但当前还不能直接写成闭环通过的两个点：

- `BIN-007 frontend_exception_type::pf`
- `BIN-013 pmp_exec_result::deny`

结论先写在前面：

1. `BIN-007` 当前不是“没有想法”，而是已经有历史 testcase，但 evidence 没成立。
2. `BIN-013` 当前连可引用的 testcase / artifact 都还没真正落地。
3. 因此这两个点当前都不应写成通过。

## 1. BIN-007 评估

### 1.1 目标定义

当前 active pilot CSV 中，`BIN-007` 定义为：

- `frontend_exception_type::pf`
- 目标场景：instruction page fault 异常并被前端正确传播

## 1.2 已存在的历史 testcase

旧 standalone `frontend_bt` 环境中，确实存在对应 testcase：

- `test_funcov_itlb_page_fault`

但这个 testcase 在历史代码中已经被显式标成：

- `xfail`

原因说明写得很直接：

- `sv39 PTW page-fault exception path is not yet fully driven by the current env setup`

### 1.3 当前历史 evidence 状态

可引用的历史 artifact：

- `test_funcov_itlb_page_fault.funcov.json`
- `test_funcov_itlb_page_fault.funcov.summary.csv`
- `test_funcov_itlb_page_fault.fst`

但这个 raw funcov 的实际结果是：

1. `hits` 里只有：
   - `ibuffer_state::full`
2. `frontend_exception_type`
   - `Hit_Bins = 0`
3. `ptw_resp_type`
   - `Hit_Bins = 0`
4. `fetch_path_x_exception`
   - `Hit_Bins = 0`
5. `itlb_result_type`
   - `Hit_Bins = 0`
6. 运行统计：
   - `monitor.error_count = 0`
   - `ptw.req_count = 0`
   - `ptw.resp_count = 0`
   - `backend.commit_count = 0`

这说明该 testcase 当前连最基本的 page-fault 路径都没有真正建立起来。

### 1.4 当前结论

`BIN-007` 当前应归类为：

- 已有 testcase 雏形
- 但证据未成立
- 不能写成闭环通过

更准确地说，当前阻塞点不是文档，而是环境路径没有把 `pf` 场景真正驱起来。

## 2. BIN-013 评估

### 2.1 目标定义

当前 active pilot CSV 中，`BIN-013` 定义为：

- `pmp_exec_result::deny`
- 目标场景：PMP deny 导致取指 fault，并沿前端异常链路传播

### 2.2 当前 testcase / artifact 状态

本轮检索结果是：

1. 在旧 standalone `frontend_bt` 的 CSV 中，`BIN-013` 仍然引用：
   - `tc_pmp_deny_exec`
2. 但当前代码检索中，没有找到对应的实际 testcase 实现。
3. 当前也没有找到对应的 raw artifact，例如：
   - `test_funcov_pmp_deny*.funcov.json`
   - `test_bin013*.funcov.json`

换句话说，`BIN-013` 当前还停留在：

- CSV 中有规划
- 但 testcase 和 evidence 尚未真正落地

### 2.3 当前结论

`BIN-013` 当前应归类为：

- 规划存在
- testcase 未落地
- artifact 不存在
- 不能写成闭环通过

## 3. 当前对这两个点的统一口径

后续文档在提到 `BIN-007` / `BIN-013` 时，统一使用下面说法：

### 3.1 BIN-007

- 已有历史 testcase 尝试
- 当前 testcase 为 `xfail`
- raw funcov 没有打到 `pf`
- 当前不具备验收条件

### 3.2 BIN-013

- 当前只有 CSV 级规划
- 尚未看到落地 testcase 和 raw evidence
- 当前不具备验收条件

## 4. 对环境 agent 的最小需求

要把这两个点推进到下一阶段，最少需要环境 agent 给出：

### 4.1 BIN-007

1. 一个真正能触发 `frontend_exception_type::pf` 的 testcase
2. 对应 raw funcov，至少命中：
   - `frontend_exception_type::pf`
   - `ptw_resp_type::pf`
   - 最好还有 `fetch_path_x_exception::icache_x_pf`

### 4.2 BIN-013

1. testcase 实现
2. 对应 raw funcov，至少命中：
   - `pmp_exec_result::deny`
   - 最好还有 `pmp_exec_type::deny`

## 5. 当前结论

第二批候选里：

1. `BIN-007`
   - 属于“已有 testcase，但路径没打通”
2. `BIN-013`
   - 属于“CSV 有规划，但 testcase / artifact 还没落地”

因此，这两个点当前都只适合写成：

- 候选闭环对象
- 当前未通过
- 待环境 agent 补证据
