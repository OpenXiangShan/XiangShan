# Frontend BT 当前活跃 L0 Bin 状态总表

本文按当前 active pilot CSV 中的全部 `L0` bin，给出一张统一状态表。

状态定义：

- `A 主线已验收`
  - 当前主控目录已有真实 DUT 证据，可直接作为经理口径。
- `B 历史闭环已完成`
  - 历史 standalone `frontend_bt` 已有完整 evidence，当前文档可引用，但主线仍待回放升级。
- `C 伴随命中`
  - 在别的主 bin 场景中有伴随 evidence，但不足以单独升级为该 bin 的独立验收结论。
- `D 候选阻塞`
  - 已有 testcase 尝试或明确目标，但当前证据未成立。
- `E 规划中`
  - 当前 CSV 有规划，但还没有足够 evidence 或 testcase 落地。

## L0 总表

| Bin_ID | Coverage_Group | Bin_Name | 当前状态 | 当前证据来源 | 当前结论 |
| --- | --- | --- | --- | --- | --- |
| BIN-001 | `reset_boot_path` | `seen` | `B 历史闭环已完成` | `test_funcov_boot_and_icache_seq.funcov.json` | 可写历史闭环，主线待升级 |
| BIN-002 | `fetch_path_type` | `icache_seq` | `B 历史闭环已完成` | `test_funcov_boot_and_icache_seq.funcov.json` | 可写历史闭环，主线待升级 |
| BIN-003 | `fetch_path_type` | `mmio_uncache` | `B 历史闭环已完成` | `test_funcov_mmio_uncache_path.funcov.json` | 可写历史闭环，主线待升级 |
| BIN-004 | `redirect_type` | `ctrl` | `A 主线已验收` | `test_bin004_backend_ctrl_redirect_pilot.funcov.json` | 完整场景闭环 |
| BIN-005 | `redirect_type` | `memVio` | `E 规划中` | 当前无命中 evidence | 不写成通过 |
| BIN-006 | `redirect_type` | `interrupt` | `E 规划中` | 当前无命中 evidence | 不写成通过 |
| BIN-007 | `frontend_exception_type` | `pf` | `D 候选阻塞` | `test_funcov_itlb_page_fault` 为 `xfail`，raw funcov 未命中 | 不具备验收条件 |
| BIN-008 | `frontend_exception_type` | `af` | `C 伴随命中` | `test_bin012_itlb_miss_walk_refill_pilot.funcov.json` | 只作为 BIN-012 companion evidence 保留 |
| BIN-009 | `frontend_exception_type` | `gpf` | `E 规划中` | 当前无命中 evidence | 不写成通过 |
| BIN-010 | `frontend_exception_type` | `ill` | `E 规划中` | 当前无命中 evidence | 不写成通过 |
| BIN-011 | `frontend_exception_type` | `hwe` | `E 规划中` | 当前无命中 evidence | 不写成通过 |
| BIN-012 | `itlb_ptw_flow` | `miss_walk_refill` | `A 主线已验收` | `test_bin012_itlb_miss_walk_refill_pilot.funcov.json` | 功能/协议闭环通过，带 checker 边界 |
| BIN-013 | `pmp_exec_result` | `deny` | `D 候选阻塞` | 当前只有 CSV 规划，未见 testcase / artifact | 不具备验收条件 |
| BIN-014 | `backend_accept_mode` | `all_block_short` | `B 历史闭环已完成` | `test_funcov_short_backpressure.funcov.json` | 可写历史闭环，主线待升级 |

## 当前数量统计

按上表统计：

- `A 主线已验收`：`2`
- `B 历史闭环已完成`：`4`
- `C 伴随命中`：`1`
- `D 候选阻塞`：`2`
- `E 规划中`：`5`

当前 active `L0` bin 总数：

- `14`

## 当前最合理的推进顺序

1. 先把 `B 历史闭环已完成` 的四个点继续往主线升级：
   - `BIN-001`
   - `BIN-002`
   - `BIN-003`
   - `BIN-014`
2. 继续锁定 `BIN-012` 的稳定口径，不让 `af/pf` 语义漂移。
3. 对 `BIN-007` 和 `BIN-013`：
   - 保持“候选阻塞”口径
   - 等环境 agent 补 testcase / evidence
4. 其余 `E 规划中` 的点，在出现第一份可复核 evidence 前，不提前写成通过。

## 当前关联文档

这张总表应与下面几份文档配套阅读：

- `frontend_bt_pilot_acceptance_status.md`
- `Frontend_BT_批量覆盖率闭环矩阵.md`
- `Frontend_BT_首批L0闭环样例_BIN001_BIN002_BIN003_BIN014.md`
- `Frontend_BT_第二批候选_BIN007_BIN013_证据评估.md`
- `Frontend_BT_当前覆盖率量化基线_2026-04-17.md`
