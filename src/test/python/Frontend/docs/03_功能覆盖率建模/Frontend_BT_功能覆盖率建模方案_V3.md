# Frontend BT 功能覆盖率建模方案 V3

本文件已迁入当前 `src/test/python/Frontend/docs/03_功能覆盖率建模/`。

当前使用方式如下：

1. 本文件保留 V3 阶段对 Frontend BT funcov 的分层设计原则。
2. 当前 active 的主线试点 CSV 以顶层文件 `../frontend_bt_functional_coverage_pilot.csv` 为准。
3. 当前目录下的 `Frontend_BT_功能覆盖率映射_初版_V3.csv` 和 `Frontend_BT_功能覆盖率试点清单_V3.csv` 作为规划基线继续维护。

## 1. 目标

本文件用于把当前已经完成的 `Frontend_功能测试点分解_层级版_V3.csv` 向下衔接到 Python 验证环境中的功能覆盖率建模，优先解决“验证闭环是否能跑通”的问题，而不是在第一阶段就追求 330 个叶子测试点全部建模完成。

当前闭环目标为：

1. 基于前端 BT 测试点文档，选取一批具备代表性的功能场景。
2. 在 Python 验证环境中为这些场景建立可采样的 coverage item。
3. 用已构建的激励 agent 和前端 DUT 交互，完成 testcase 运行。
4. 采集 coverage 数据并形成覆盖率报告。
5. 用 coverage 结果反推测试点缺口、环境观测缺口和激励缺口。

## 2. 当前输入

当前功能覆盖率建模的主要输入包括：

- `Frontend_功能测试点分解_层级版_V3.csv`
- `Frontend_测试点分解_V3.csv`
- Kunminghu V3 Frontend/IFU/FTQ/BPU/ICache/ITLB/PTW/PMP/IBuffer 相关 Chisel 源码

其中：

- 当前 AI 生成的 Frontend BT 测试点已经能支持做第一版 funcov 映射。
- 当前 active pilot CSV 已经和主线 pytest testcase 名称完成对齐。

## 3. 建模原则

### 3.1 不一次性覆盖全部叶子测试点

当前功能叶子测试点约 330 个。第一阶段不建议把全部叶子点直接一比一映射成 coverpoint，否则会出现：

- Python 环境观测点不足
- coverage item 命名混乱
- 采样逻辑过早复杂化
- 报告不可读

建议分三层推进：

- `L0 基础闭环覆盖`：先验证环境、采样、统计、报告链路跑通
- `L1 关键功能覆盖`：覆盖前端主路径、主控制路径、主异常路径
- `L2 细粒度覆盖`：按测试点表逐步扩充到模块细粒度场景

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

### 第一步：跑通试点覆盖项

直接对接 `Frontend_BT_功能覆盖率试点清单_V3.csv`，优先实现其中的 L0 与首批 L1 覆盖项，不要求第一轮就覆盖全部 330 个叶子测试点。

### 第二步：形成 testcase 到 coverage 的映射

每个 testcase 至少回答两个问题：

- 它预期命中哪些 coverage group/bin
- 它实际命中了哪些 coverage group/bin

### 第三步：打通回归与报告

需要具备：

- 单 testcase coverage dump
- 多 testcase merge
- 未命中 bins 列表
- 从未命中 bins 反推激励缺口/观测缺口/测试点缺口

## 9. 与测试点文档的关系

当前建议不是“所有测试点立即建模”，而是：

- 先从测试点文档中选关键叶子点建立 coverage item
- 跑通环境和报告链路
- 再按模块逐步扩展 coverage item
- 用 coverage 反向指导测试点文档补充和 testcase 扩展

因此测试点文档仍是主输入，但 funcov 建模应分阶段实施。

## 10. 当前建议的下一步

1. 在 Python 环境中先实现 `Frontend_BT_功能覆盖率试点清单_V3.csv` 中的覆盖项。
2. 先跑通 6 个基本验证场景，不追求首轮高覆盖率。
3. 补齐 coverage dump / merge / report 流程。
4. 首轮回归后根据未命中 bins 反推：
   - 激励缺口
   - 观测缺口
   - 测试点映射缺口
5. 再把 BPU、FTQ、ITLB/PTW/PMP、IBuffer 的细粒度覆盖项继续展开。
