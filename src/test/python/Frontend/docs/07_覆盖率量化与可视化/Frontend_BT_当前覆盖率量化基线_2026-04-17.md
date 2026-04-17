# Frontend BT 当前覆盖率量化基线

统计时间：

- 2026-04-17

本文给出当前 Frontend BT 文档可直接引用的一组量化基线，分为两层：

1. 当前主线已验收 pilot 的量化结果
2. 历史 standalone `frontend_bt` suite 的量化结果

## 1. 当前主线已验收 pilot 基线

当前主线已验收 pilot：

- `BIN-004`
- `BIN-012`

### 1.1 BIN-004 单用例 summary

证据文件：

- `test_bin004_backend_ctrl_redirect_pilot.funcov.summary.csv`

关键覆盖结果：

| Coverage_Group | Coverage_Pct | Hit_Bin_Names |
| --- | --- | --- |
| `redirect_type` | `33.33%` | `ctrl` |
| `fetch_path_type` | `50.00%` | `icache_seq` |
| `reset_boot_path` | `100.00%` | `seen` |

关键运行统计：

- `monitor.error_count = 0`
- `backend.commit_count = 11`
- `monitor.redirect_count = 1`

### 1.2 BIN-012 单用例 summary

证据文件：

- `test_bin012_itlb_miss_walk_refill_pilot.funcov.summary.csv`

关键覆盖结果：

| Coverage_Group | Coverage_Pct | Hit_Bin_Names |
| --- | --- | --- |
| `itlb_ptw_flow` | `100.00%` | `miss_walk_refill` |
| `itlb_result_type` | `50.00%` | `miss` |
| `ptw_resp_type` | `50.00%` | `leaf_pte` |
| `itlb_state_x_ptw_resp` | `100.00%` | `single_miss_x_leaf_pte` |
| `frontend_exception_type` | `20.00%` | `af` |
| `fetch_path_type` | `50.00%` | `icache_seq` |
| `reset_boot_path` | `100.00%` | `seen` |

关键运行统计：

- `ptw.req_count = 1`
- `ptw.resp_count = 1`
- `backend.commit_count = 6`
- `monitor.error_count = 144`

说明：

- `monitor.error_count = 144` 当前不按 DUT 功能错误解释，而按 translation-aware checker 边界处理。

## 2. 历史 standalone suite 基线

证据文件：

- `frontend_bt_pilot_suite.funcov.summary.csv`

当前旧 standalone suite 汇总结果如下：

| Coverage_Group | Coverage_Pct | Hit_Bin_Names |
| --- | --- | --- |
| `backend_accept_mode` | `100.00%` | `all_block_short` |
| `fetch_path_type` | `100.00%` | `icache_seq,mmio_uncache` |
| `itlb_ptw_flow` | `100.00%` | `miss_walk_refill` |
| `itlb_state_x_ptw_resp` | `100.00%` | `single_miss_x_leaf_pte` |
| `reset_boot_path` | `100.00%` | `seen` |
| `bpu_basic_pred_type` | `50.00%` | `seq_no_cfi,direct_jmp` |
| `itlb_result_type` | `50.00%` | `miss` |
| `ptw_resp_type` | `50.00%` | `leaf_pte` |
| `redirect_type` | `33.33%` | `ctrl` |
| `ibuffer_state` | `33.33%` | `full` |
| `frontend_exception_type` | `20.00%` | `af` |
| `fetch_path_x_exception` | `0.00%` | `` |
| `ftq_queue_state` | `0.00%` | `` |
| `ibuffer_state_x_backend_mode` | `0.00%` | `` |
| `pmp_exec_result` | `0.00%` | `` |
| `pmp_exec_type` | `0.00%` | `` |

## 3. 当前可直接对外汇报的数字

如果当前要给一个“简短但量化”的汇报口径，建议直接引用：

### 3.1 主线已验收闭环

- 已验收主线 pilot：`2` 个
  - `BIN-004`
  - `BIN-012`

### 3.2 当前活跃 L0 状态分布

当前 active `L0` bin 总数：`14`

- `A 主线已验收`：`2`
- `B 历史闭环已完成`：`4`
- `C 伴随命中`：`1`
- `D 候选阻塞`：`2`
- `E 规划中`：`5`

详细分布见：

- `../05_试点闭环与主线迁移/Frontend_BT_当前活跃L0_Bin状态总表.md`

### 3.3 历史闭环基础

- 历史 standalone suite 中已达到 `100%` 的 coverage groups：`5` 个
  - `backend_accept_mode`
  - `fetch_path_type`
  - `itlb_ptw_flow`
  - `itlb_state_x_ptw_resp`
  - `reset_boot_path`

### 3.4 当前异常类覆盖状态

- `frontend_exception_type`
  - 当前仅 `af` 命中
  - `Coverage_Pct = 20.00%`
- `fetch_path_x_exception`
  - 当前仍 `0.00%`

### 3.5 当前 PMP 相关覆盖状态

- `pmp_exec_result`
  - 当前 `0.00%`
- `pmp_exec_type`
  - 当前 `0.00%`

## 4. 当前解读

从这组数字可以看出：

1. 当前最成熟的是：
   - 启动 / 主取指路径
   - redirect
   - ITLB miss / PTW / refill
   - backend 短时全反压
2. 当前最薄弱的是：
   - PF / GPF / 非法异常类
   - PMP deny
   - 交叉异常覆盖
   - FTQ 边界覆盖

## 5. 当前建议的跟踪方式

后续建议每轮都维护同一组指标：

1. 主线已验收 pilot 数量
2. 当前 active pilot CSV 中：
   - 已验收 bin 数
   - 历史闭环已完成 bin 数
   - 未通过/未落地 bin 数
3. funcov summary 中各 `Coverage_Group` 的 `Coverage_Pct`

这样后面即使暂时没有 EDA 风格的 branch/toggle/FSM coverage 页面，也已经有一套可持续累计的量化记录基线。
