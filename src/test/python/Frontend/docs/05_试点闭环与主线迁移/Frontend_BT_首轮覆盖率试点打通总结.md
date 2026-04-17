# Frontend BT 首轮覆盖率试点打通总结

## 1. 目的

本文给出当前 Frontend BT 文档在“首轮覆盖率 bin 打通尝试”上的落地状态。

这里的“打通”不是指所有 L0/L1 bin 都已验收完成，而是指：

1. 测试点文档已经向 funcov 建模打通。
2. funcov 建模已经向主线 testcase 打通。
3. testcase 已能在真实 DUT 环境下产出：
   - waveform
   - line coverage dat
   - funcov json
   - funcov summary/unhit
4. 至少有一批代表性 bin 已形成经理可读的证据链。

## 2. 首轮闭环基线

V3 覆盖率建模阶段建议优先打通 6 类基础场景：

1. reset 启动 + 非 MMIO 顺序取指
2. backend ctrl redirect 恢复
3. 单次 MMIO 取指
4. ITLB 单次 miss + PTW 正常返回
5. PMP deny 导致 access fault
6. backend canAccept 反压 + IBuffer 恢复

当前主线已经至少对其中最关键的两个 pilot 点形成了真实 DUT 证据：

- `BIN-004 redirect_type::ctrl`
- `BIN-012 itlb_ptw_flow::miss_walk_refill`

同时，旧 standalone `frontend_bt` 证据还支持继续向下一批 L0 点推进：

- `BIN-001 reset_boot_path::seen`
- `BIN-002 fetch_path_type::icache_seq`
- `BIN-003 fetch_path_type::mmio_uncache`
- `BIN-014 backend_accept_mode::all_block_short`

## 3. 当前证据目录

当前主线验收使用的原始证据位于：

- `/nfs/home/jiabowen/ai_workspace/XiangShan_frontend_bt_jiabowen/src/test/python/Frontend/data/`

本轮重点证据文件：

- `test_bin004_backend_ctrl_redirect_pilot.funcov.json`
- `test_bin004_backend_ctrl_redirect_pilot.funcov.summary.csv`
- `test_bin004_backend_ctrl_redirect_pilot.fst`
- `test_bin012_itlb_miss_walk_refill_pilot.funcov.json`
- `test_bin012_itlb_miss_walk_refill_pilot.funcov.summary.csv`
- `test_bin012_itlb_miss_walk_refill_pilot.fst`

## 4. 已打通的代表性 bin

| Bin | testcase | 当前结论 | 关键证据 |
| --- | --- | --- | --- |
| BIN-004 | `test_bin004_backend_ctrl_redirect_pilot` | 已通过 | `redirect_type::ctrl` 命中 2 次；`monitor.error_count = 0`；`backend.commit_count = 11` |
| BIN-012 | `test_bin012_itlb_miss_walk_refill_pilot` | 已通过，但带 checker 边界说明 | `itlb_ptw_flow::miss_walk_refill` 命中；`itlb_result_type::miss` 命中；`ptw_resp_type::leaf_pte` 命中；`ptw.req_count = 1`；`ptw.resp_count = 1` |

## 5. BIN-004 结论

`BIN-004` 当前已形成完整场景闭环，核心证据如下：

1. `redirect_type::ctrl`
   - `hits = 2`
   - `first_cycle = 583`
2. 监控统计
   - `monitor.redirect_count = 1`
   - `monitor.error_count = 0`
3. 后端统计
   - `backend.commit_count = 11`
4. 分支统计
   - `branch.by_type.jump = 1`

因此，`BIN-004` 当前可作为“后端 ctrl redirect 闭环已打通”的标准样例。

## 6. BIN-012 结论

`BIN-012` 当前已经打通“功能/协议闭环”，但保留一个已知 checker 边界。

当前 raw funcov 关键证据如下：

1. `frontend_exception_type::af`
   - `hits = 5`
2. `itlb_ptw_flow::miss_walk_refill`
   - `hits = 1`
   - `first_cycle = 567`
3. `itlb_result_type::miss`
   - `hits = 2`
4. `ptw_resp_type::leaf_pte`
   - `hits = 1`
5. `itlb_state_x_ptw_resp::single_miss_x_leaf_pte`
   - `hits = 1`

运行统计：

- `ptw.req_count = 1`
- `ptw.resp_count = 1`
- `backend.commit_count = 6`
- `monitor.error_count = 144`

当前 `errors` 中记录的是 `INSTR_MISMATCH`，因此这一个点的文档口径必须保持：

1. `BIN-012` 已证明 miss -> PTW -> refill 路径打通。
2. 当前 raw funcov 中的异常类型命中是 `af`，不是 `pf`。
3. 当前仍不能把它写成“完整指令值正确性闭环”；更准确的表述是：
   - 已通过功能/协议闭环
   - 仍带 translation-aware checker 边界

## 7. 当前达到的阶段

截至 2026-04-17，可以认为 Frontend BT 文档已经达到了“首轮覆盖率 bin 打通尝试已完成”的阶段，理由是：

1. 旧的策略、测试点、覆盖率建模和 testbench 设计文档已经迁入当前 `docs/`。
2. 当前主线 pilot CSV 已对齐到实际 testcase。
3. `BIN-004` / `BIN-012` 已有真实 DUT 产物与可引用证据。
4. 文档 worktree 已具备继续围绕 L0/L1 bin 扩展的基础，不再需要回头依赖旧目录。

## 8. 下一步

后续文档演进建议继续按下面顺序推进：

1. 优先把 `BIN-001`、`BIN-002`、`BIN-003`、`BIN-014` 收成第一批批量闭环文档。
2. 继续把 L1 关键 bin 与 testcase 的映射写实。
3. 将新的环境结论持续回灌到：
   - `frontend_bt_pilot_acceptance_status.md`
   - `frontend_bt_functional_coverage_pilot.csv`
   - `03_功能覆盖率建模/` 下的 V3 基线文档
4. 详细分批状态见：
   - `Frontend_BT_批量覆盖率闭环矩阵.md`
   - `Frontend_BT_首批L0闭环样例_BIN001_BIN002_BIN003_BIN014.md`
