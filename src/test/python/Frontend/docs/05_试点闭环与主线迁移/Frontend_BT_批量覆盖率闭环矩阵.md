# Frontend BT 批量覆盖率闭环矩阵

## 1. 目的

本文用于把当前已经具备证据的 bin 分成一批可持续收敛的闭环对象，避免后续每次只盯单个点而缺少整体节奏。

当前矩阵分三类状态：

- `A 主线已验收`
  - 当前主控目录已有真实 DUT 证据，可直接作为当前经理口径。
- `B 历史证据充分`
  - 旧 `frontend_bt` 独立环境已有完整 funcov / waveform / summary 证据，文档可先写成“历史闭环已完成，待主线回放升级”。
- `C 暂不闭环`
  - 当前还没有足够干净的命中或口径仍不稳定，不先写成通过。

## 2. 第一批闭环对象

### 2.1 A 主线已验收

| Bin | 当前证据 | 结论 | 备注 |
| --- | --- | --- | --- |
| BIN-004 | `test_bin004_backend_ctrl_redirect_pilot` | 已验收 | 完整场景闭环 |
| BIN-012 | `test_bin012_itlb_miss_walk_refill_pilot` | 已验收 | 功能/协议闭环通过，仍带 checker 边界 |

### 2.2 B 历史证据充分

| Bin | 历史证据文件 | 关键事实 | 当前建议口径 |
| --- | --- | --- | --- |
| BIN-001 | `test_funcov_boot_and_icache_seq.funcov.json` | `reset_boot_path::seen` 命中 1 次；`backend.commit_count = 8`；`monitor.error_count = 0` | 可先写成历史闭环已完成 |
| BIN-002 | `test_funcov_boot_and_icache_seq.funcov.json` | `fetch_path_type::icache_seq` 命中 139 次；`icache.req_count = 11`；`monitor.error_count = 0` | 可先写成历史闭环已完成 |
| BIN-003 | `test_funcov_mmio_uncache_path.funcov.json` | `fetch_path_type::mmio_uncache` 命中 64 次；`backend.commit_count = 4`；`monitor.error_count = 0` | 可先写成历史闭环已完成 |
| BIN-014 | `test_funcov_short_backpressure.funcov.json` | `backend_accept_mode::all_block_short` 命中；`blocked_cycles = 12`；`backend.commit_count = 8`；`monitor.error_count = 0` | 可先写成历史闭环已完成 |

这四个点的详细写法已经补在：

- `Frontend_BT_首批L0闭环样例_BIN001_BIN002_BIN003_BIN014.md`

### 2.3 当前可一并引用的 supporting bins

这些 bin 当前不作为本轮经理口径主目标，但已经有可引用证据，可作为主 bin 的陪审信息：

| Bin | 证据来源 | 说明 |
| --- | --- | --- |
| BIN-101 | `test_funcov_boot_and_icache_seq.funcov.json` / `test_funcov_short_backpressure.funcov.json` | `seq_no_cfi` 已多次命中 |
| BIN-102 | `test_funcov_ctrl_redirect_and_branch_bins.funcov.json` | `direct_jmp` 已命中 |
| BIN-109 | `test_bin012_itlb_miss_walk_refill_pilot.funcov.json` | `miss` 已命中 |
| BIN-110 | `test_bin012_itlb_miss_walk_refill_pilot.funcov.json` | `leaf_pte` 已命中 |
| BIN-116 | 多个历史 raw funcov | `ibuffer_state::full` 已稳定命中，但当前不是经理主目标 |
| BIN-203 | `test_bin012_itlb_miss_walk_refill_pilot.funcov.json` | 关键交叉已命中 |

## 3. 第一批闭环建议顺序

建议按下面顺序继续把文档补齐：

1. `BIN-012`
   - 先锁死当前 `af` / `INSTR_MISMATCH` 口径。
2. `BIN-001` + `BIN-002`
   - 作为启动路径和主取指路径的最小基础闭环。
3. `BIN-003`
   - 形成 MMIO 取指闭环样例。
4. `BIN-014`
   - 形成 backend 全反压短阻塞闭环样例。

这样能较快形成一批对外可讲的 L0 闭环集合：

- 复位启动
- ICache 主路径
- MMIO 路径
- backend ctrl redirect
- ITLB miss / PTW / refill
- backend 短时全反压

## 4. 当前不建议写成通过的点

根据旧 `frontend_bt` 独立 suite 汇总结果，下面这些点当前仍不建议直接写成闭环通过：

- `BIN-005`
- `BIN-006`
- `BIN-007`
- `BIN-009`
- `BIN-010`
- `BIN-011`
- `BIN-013`
- `BIN-103`
- `BIN-104`
- `BIN-105`
- `BIN-106`
- `BIN-107`
- `BIN-108`
- `BIN-111`
- `BIN-112`
- `BIN-113`
- `BIN-114`
- `BIN-115`
- `BIN-201`
- `BIN-202`
- `BIN-204`

原因是这些点在旧 standalone suite 汇总里仍处于未命中，或者当前语义边界还不稳定。

其中第二批最值得单独跟踪的是：

- `BIN-007`
  - 已有历史 testcase，但当前 raw funcov 未命中 `pf`
- `BIN-013`
  - 当前只有 CSV 规划，尚未看到 testcase / artifact 落地

详细评估见：

- `Frontend_BT_第二批候选_BIN007_BIN013_证据评估.md`
- `Frontend_BT_环境补证据需求清单.md`

## 5. 本轮结论

本轮可以把 Frontend BT 的“批量覆盖率闭环”理解为：

1. 主线已验收：
   - `BIN-004`
   - `BIN-012`
2. 历史证据充分、可先写成闭环材料的第一批：
   - `BIN-001`
   - `BIN-002`
   - `BIN-003`
   - `BIN-014`

后续如果环境 agent 给出这些点在主线的新证据，再把这四个点从 `B 历史证据充分` 升级到 `A 主线已验收`。
