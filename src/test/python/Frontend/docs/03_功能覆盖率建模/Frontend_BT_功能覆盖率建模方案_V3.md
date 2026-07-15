# Frontend BT 功能覆盖率建模方案 V3

本文件已迁入当前 `src/test/python/Frontend/docs/03_功能覆盖率建模/`。

当前使用方式如下：

1. 本文件保留 V3 阶段的建模原则和全量推进规则。
2. `frontend_bt_functional_coverage_pilot.csv` 是唯一 active coverage registry；文件名为兼容现有工具保留 `pilot`，不再表示试点阶段。
3. `../02_测试点分解/Frontend_testpoint_0525_coverage_backannotated.csv` 是唯一叶子测试点、状态、testcase 和 evidence 的事实源。
4. `Frontend_BT_功能覆盖率映射_初版_V3.csv` 与 `Frontend_BT_功能覆盖率试点清单_V3.csv` 已完成历史使命并已移除；其中可执行 bin 已合并到 active registry，粗粒度规划不作为已建仓覆盖项保留。

## Batch 0: 2-fetch 反标约束

2-fetch 首批 `BIN-501..BIN-541` 使用以下 active 资产：

- 测试点主表：`../02_测试点分解/Frontend_testpoint_0525_coverage_backannotated.csv`
- 建仓清单：`frontend_bt_functional_coverage_pilot.csv`
- 自动回标工具：`../../tools/backannotate_funcov.py`

每个已建仓叶子测试点只能拥有一条 `coverage` 反标，且必须精确到：

`covergroup <group>, coverpoint <point>, bins <bin> (BIN-xxx)`

同一个 BIN 只能由一个叶子测试点占用。多个观察条件共同构成验证目标时，必须建立独立 cross 覆盖项和独立叶子场景，不能把多个普通 coverpoint 并列反标到同一叶子。

pilot CSV 的 `Coverpoint` 列与 `Coverage_Group`、`Bin_Name` 一起构成建仓唯一键。测试点主表额外维护：

- `status`：`UNMAPPED`、`MODELED`、`PARTIAL`、`HIT`、`CLOSED`、`BLOCKED`、`N-A`
- `testcase`：该 leaf 的计划或实际 testcase
- `evidence`：模型或真实 DUT artifact 的简短证据索引

`Bin_ID` 在 active registry 中全局唯一。历史 artifact 若使用已退休编号，registry 通过可选的 `Legacy_Bin_ID` 保存其迁移来源；历史 JSON、波形和日志保持只读，新回归只生成当前 `Bin_ID`。

自动回标只按 artifact 的运行统计区分模型与真实 DUT：模型/FakeDut 只能维持或提升到 `MODELED`；真实 DUT 命中可提升到 `HIT`；`CLOSED` 只能由人工验收写入，工具会保留该状态而不自动覆盖。

## 1. 目标

本文件用于把测试点主表向下衔接到 Python 验证环境中的功能覆盖率建模。基础链路已完成验证；当前目标是按叶子测试点持续扩展建仓、汇编 testcase、真实 DUT 回归和 evidence 回标，而不是维护独立的试点清单。

当前闭环目标为：

1. 从测试点主表的叶子场景建立一对一的 coverage item。
2. 用汇编/bin testcase 或既有环境 testcase 在真实 DUT 上执行。
3. 保存 JSON、波形和日志 evidence，并自动回标为 `MODELED`、`PARTIAL` 或 `HIT`。
4. 以未命中 bin 反推 testcase、观测或测试点缺口；`CLOSED` 只在人工验收后写入。

## 2. 当前输入

当前功能覆盖率建模的主要输入包括：

- `../02_测试点分解/Frontend_testpoint_0525_coverage_backannotated.csv`
- `frontend_bt_functional_coverage_pilot.csv`
- Kunminghu V3 Frontend/IFU/FTQ/BPU/ICache/ITLB/PTW/PMP/IBuffer 相关 Chisel 源码
- `../../env/coverage_def.py`、`../../env/funcov.py` 和 `../../env/functional_coverage.py`

其中：

- 测试点主表决定 leaf 的验证语义和回标状态。
- active registry 决定唯一的 `Coverage_Group`、`Coverpoint`、`Bin_Name` 和 testcase 对应关系。

## 3. 建模原则

### 3.1 全量推进，但不伪造建仓

当前工作不再以“先跑通少量试点”为目标。所有叶子测试点都应进入 `UNMAPPED`、`MODELED`、`PARTIAL`、`HIT`、`CLOSED`、`BLOCKED` 或 `N-A` 的明确状态之一，并按模块持续收敛。

新增 coverage item 必须先具备唯一 leaf、可解释的采样条件和独立 bin；环境尚不能可靠观测、或 testcase 尚不能构造时，保留为 `UNMAPPED` 或 `BLOCKED`，不得以粗粒度聚合条目代替叶子建仓。

### 3.2 功能覆盖率对象不是 RTL 内部所有细节

第一阶段 coverage item 应优先选取：

- 顶层输入输出可见事件
- 前端关键阶段状态切换
- 验证环境能稳定观测到的 transaction 级事件
- 能直接映射 testcase 是否覆盖到该场景的离散事件

而不是一开始就依赖大量难以稳定获得的内部信号。

### 3.3 覆盖项必须能回标到测试点

每个 coverage item 至少要能回答三个问题：

1. 它对应哪个测试点路径？
2. 它在环境里如何采样？
3. 它覆盖到了什么离散场景？

## 4. 推荐的覆盖率分层

### 4.1 L0 基础闭环覆盖

用于验证环境链路是否打通，建议首批必须完成：

- reset 启动覆盖
- 非 MMIO 顺序取指覆盖
- MMIO 取指覆盖
- backend redirect 覆盖
- 前端异常传播覆盖
- 基本 ITLB miss / PTW walk 覆盖
- backend canAccept 反压覆盖
- coverage 文件输出与统计汇总覆盖

### 4.2 L1 关键功能覆盖

建议第二步补齐：

- BPU 基本预测方向覆盖
- FTQ 入队/出队/空满边界覆盖
- IFU 非 MMIO / MMIO 路径覆盖
- ICache hit/miss/fencei 覆盖
- ITLB hit/miss/sfence 覆盖
- PMP allow/deny 覆盖
- IBuffer 出队/反压/flush 覆盖

### 4.3 L2 细粒度覆盖

后续再按当前测试点文档继续铺开，包括：

- BPU 多预测器协同
- RAS / 历史维护 / 训练更新
- IFU PredChecker fault 类型
- ICache 预取、替换、tag 冲突
- PTW 返回类型细分
- 多模块交叉场景

## 5. Python 环境中的覆盖数据模型建议

建议在 Python 环境中把 coverage item 分成四类：

### 5.1 场景覆盖

回答“某个测试点场景是否至少触发过一次”。

示例：

- `frontend_reset_boot_seen`
- `frontend_non_mmio_seq_seen`
- `frontend_mmio_seen`
- `frontend_backend_redirect_seen`

### 5.2 类型覆盖

回答“某个分类下的离散类型是否都覆盖到”。

示例：

- `redirect_type`: `ctrl / memVio / interrupt / ifu_checker`
- `fetch_path_type`: `icache / mmio_uncache`
- `frontend_exception_type`: `pf / gpf / af / ill / hwe`
- `itlb_result_type`: `hit / miss / refill / fault`

### 5.3 边界覆盖

回答“关键边界是否打到”。

示例：

- `fetch_block_pos`: `front_half / back_half / block_tail`
- `ftq_queue_state`: `empty / near_full / full / recovered`
- `backend_accept_mode`: `all_accept / all_block / partial_accept`

### 5.4 交叉覆盖

回答“关键场景组合是否出现过”。

第一阶段建议只做少量强相关交叉：

- `fetch_path_type x frontend_exception_type`
- `redirect_type x inflight_resp_type`
- `itlb_result_type x ptw_resp_type`
- `ibuffer_state x backend_accept_mode`

## 6. Python 环境中的接入约定

### 6.1 建议的 recorder 接口

```python
cov.hit('frontend_reset_boot_seen')
cov.sample('redirect_type', 'ctrl')
cov.sample('fetch_path_type', 'mmio_uncache')
cov.cross('redirect_type', 'inflight_resp_type', 'ctrl', 'icache_resp')
```

### 6.2 推荐的最小能力

建议同事的 Python 验证环境至少支持：

- `hit(name)`：命中单场景覆盖
- `sample(group, bin_name)`：命中离散 bin
- `cross(group_a, group_b, bin_a, bin_b)`：命中交叉 bin
- `dump(path)`：输出 coverage 原始结果
- `report()`：输出 group 级覆盖率和未命中 bins

### 6.3 推荐的输出格式

单次 testcase 运行后，建议至少保留三类结果：

- `coverage_raw.json`：原始命中记录
- `coverage_summary.csv`：按 group 汇总的覆盖率
- `coverage_unhit.csv`：未命中 bins 清单

### 6.4 建议的采样时机

优先选 transaction 级事件：

- reset 释放后首个 fetch 建立
- FTQ 发起 fetch request
- IFU 选择 ICache 或 uncache 路径
- backend redirect 到达
- ITLB miss 发起 PTW 请求
- IBuffer 向 backend 发射 cfVec
- exception/flush/fence 事件发生

## 7. 第一阶段推荐的基本验证闭环

为了尽快向领导证明“前端 BT 验证策略可以形成闭环”，建议先挑选一批可以稳定跑通的场景：

1. reset 启动 + 顺序非 MMIO 取指
2. backend ctrl redirect 恢复
3. 单次 MMIO 取指
4. ITLB 单次 miss + PTW 正常返回
5. PMP deny 导致 access fault
6. backend canAccept 反压 + IBuffer 恢复

这 6 类场景足以证明：

- 环境能激励前端主路径
- 环境能观测关键控制与异常事件
- 测试点文档可以向 coverage item 映射
- regression 后可以产生 coverage 统计结果

## 8. 当前建议的落地顺序

### 第一步：按模块选取未建仓叶子点

从测试点主表筛选当前模块的 `UNMAPPED` 叶子点，先确认设计语义、触发场景、可观测信号和独立 testcase。建立唯一 `covergroup / coverpoint / bin` 后写入 active registry 和建仓代码。

### 第二步：执行 testcase 并回标

每个 testcase 必须记录预期 bin、实际命中 bin、真实 DUT JSON、波形和日志位置。自动工具只能将真实 DUT 命中提升到 `HIT`；模型单测只证明 `MODELED`。

### 第三步：回归收敛

按模块汇总 `UNMAPPED/BLOCKED/MODELED/PARTIAL/HIT/CLOSED`，对未命中 bin 分别定位为激励、观测、checker 或测试点缺口。每个模块达到人工定义的验收条件后才将对应叶子置为 `CLOSED`。

## 9. 与测试点文档的关系

测试点主表始终是主输入；每个叶子点在建仓后必须拥有唯一 coverage 反标，并以真实 DUT evidence 驱动状态变化。coverage 结果可反向指导测试点、环境观测和 testcase 扩展。

## 10. 当前建议的下一步

1. 以模块为单位将 `UNMAPPED` 叶子点建仓并补齐 testcase。
2. 在真实 DUT 上执行回归，生成 coverage JSON、波形和日志 evidence。
3. 对 `HIT` 以外的状态按原因分类并持续收敛。
4. 根据未命中 bins 反推：
   - 激励缺口
   - 观测缺口
   - 测试点映射缺口
5. 逐步完成 BPU、FTQ、IFU、ICache、ITLB/PTW/PMP、IBuffer 的细粒度覆盖和人工验收。
