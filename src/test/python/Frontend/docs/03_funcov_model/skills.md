---
name: frontend-bt-verification-closure
description: 统一 Frontend BT 从设计分析、测试点、环境与 checker、汇编和 agent 激励、功能覆盖率建模、真实 DUT 回归、代码覆盖率到自动反标和人工验收的工作流。
---

# Frontend BT 测试点驱动验证与覆盖率闭环规范

## 1. 目标与边界

Frontend BT 的唯一主流程是：

`设计分析 -> 叶子测试点 -> 环境与 checker -> testcase -> 功能覆盖率 -> 真实 DUT 回归 -> artifact -> 自动反标 -> 人工 CLOSED`

测试点是所有验证活动的输入。功能覆盖率用于证明目标场景已经被激励并采样，checker、assertion、协议检查或 trace 对比用于证明 DUT 行为正确。二者缺一不可。

当前不建设完整 cycle-accurate golden model。能够使用 NEMU trace 的指令语义场景继续进行 DUT/golden 对比；其他场景使用协议级 scoreboard、跨周期 invariant、checker 和 assertion 检查正确性。

禁止用以下结果代替真实闭环：

- 测试点或 bin 已存在。
- FakeDut 或模型单测命中。
- testcase 曾经运行但没有目标场景证据。
- 功能 bin 命中但 checker、monitor 或 trace 失败。
- line、branch、expr 或 toggle 代码覆盖率上升。

## 2. 需求与 golden 来源

设计没有完整详细文档时，RTL 代码分析只是需求提取手段，不是独立 golden。测试点预期至少结合以下来源交叉确认：

- RISC-V ISA、特权架构、TileLink 和相关接口协议。
- 当前设计代码、参数和生成 RTL。
- 设计 PR 的目标、评审意见、bug-fix 和 timing-fix 说明。
- 历史 issue、bug、波形、验证经验和已有 testcase。
- 前端设计 owner 或模块 owner 的人工 review。

AI 可以分析代码、提出测试点和预期，但不能独立批准从 DUT 代码推导出的行为为 golden。高风险、异常优先级、flush/redirect、跨周期状态和设计语义有歧义的测试点必须人工 review。

设计来源、适用 DUT SHA 和未确认假设必须保留在测试点、registry 或 evidence 中，不能只存在于临时对话。

## 3. 唯一事实源

只维护以下 canonical 输入：

- 测试点主表：`../02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv`
- coverage registry：`frontend_bt_functional_coverage_pilot.csv`
- 功能覆盖率 recorder、调度和 event 采样：`../../env/functional_coverage.py`
- 模块级周期采样 predicate：`../../env/funcov.py`
- 自动反标：`../../tools/backannotate_funcov.py`
- Python testcase：`../../tests/`
- 汇编 testcase：`../../tests/asm_cases/`
- 回归入口：`../../scripts/`

`pilot` 仅为历史兼容文件名，不表示仍处于试点阶段。原 `docs/frontend_bt_functional_coverage_pilot.csv` 重复副本已删除，不得重新建立平行 registry。

功能覆盖率只允许一套 runtime 链：fixture 装配一个 `FunctionalCoverageRecorder`，由 `functional_coverage.py` 统一调度 event/cycle 采样，模块 predicate 放在 `funcov.py` 或其后续按模块拆分并注册到同一个 recorder。不得再通过 `coverage_def.py`、toffee `CovGroup`、SV covergroup 或其他 Python 文件并行维护相同 group/point/bin 的第二份命中逻辑。VCS/Verdi 功能覆盖率可以用于临时调试和交叉检查，但不能作为 canonical 反标证据。

registry 中只有 `Coverpoint` 完整、已反标到唯一叶子且已有 sampler 映射的行才是 active model。保留的历史规划行在迁移完成前只算 `UNMAPPED`，即使旧 predicate 偶然命中也不能自动反标或计入闭环分子。

## 4. 叶子测试点契约

一行测试点只要已经是层级末端，并且具有 `Condition`、`Checkpoint` 和 `Object`，即视为叶子；不要求必须拆到第五级。

每个 active 叶子必须具备：

- 清晰且不重复的层级归属。
- 独立、可执行的验证场景描述。
- `Condition`：地址属性、指令序列、输入状态、时序关系、反压、异常、redirect 等激励成立条件。
- `Checkpoint`：证明行为正确所需的事务结果、信号值、顺序、PC、异常、指针或状态变化。
- `Object`：驱动对象、采样对象和关键接口，不得为空。
- checker、assertion、scoreboard 或 trace 检查方式。
- 唯一 coverage 反标：`covergroup <group>, coverpoint <point>, bins <bin> (BIN-xxx)`。
- 主责任 testcase 和真实 DUT evidence。

Condition 只描述如何构成场景，Checkpoint 只描述如何证明结果。不得把“检查某信号正确”写成测试场景，也不得把激励步骤写入 Checkpoint。

同级叶子应互补地拆解上级功能，不应只是同一场景的重复改写。新增、合并、删除测试点时必须说明设计依据和对兄弟测试点的影响。

后续应为 active 叶子建立稳定 `TP_ID`。在 `TP_ID` 完成前，覆盖率引用必须依靠完整层级路径和唯一 `Bin_ID`，禁止通过易变化的 CSV 行号建立长期关系。

## 5. 环境与检查能力

测试点建立后，必须检查现有验证环境是否具备场景所需能力：

- 汇编程序和指令存储内容。
- ICache、InstrUncache、PTW、CSR、backend、redirect 和反压激励。
- 地址属性、页表、PMP、PBMT、异常和错误注入。
- driver 的 ready/valid、payload 保持和跨周期时序。
- monitor 对事务边界、flush、恢复和错误路径的正确采样。
- checker、assertion、reference relation 或 NEMU trace 对比。

环境能力缺失时先补环境，不得通过放宽 testcase 或覆盖判定绕过。若当前 DUT 或环境无法构造场景，测试点保持 `BLOCKED` 并记录明确 blocker。

没有 cycle golden model 时，优先实现以下检查：

- ready/valid 协议和 payload 稳定性。
- 请求、响应、flush、redirect、commit 的顺序和归属。
- PC、FTQ 指针、异常、blockSel 和 lane 的一致性。
- 请求不丢失、不重复、不串项。
- recovery 后旧路径不得产生交付、训练或错误状态。
- NEMU trace 与 backend 可见指令流的一致性。

功能覆盖率 predicate 不得承担 checker 职责。即使 predicate 观察到目标信号组合，输出行为错误仍必须由 checker 报错。

## 6. Testcase 规则

一个 testcase 可以覆盖多个叶子，但每个叶子必须有一个主责任 testcase。testcase 必须显式列出目标 TP_ID/Bin_ID，不能依赖运行后偶然命中解释覆盖意图。Python directed test 使用 `@pytest.mark.funcov_bins(...)` / `funcov_tps(...)`；通用 bin-trace testcase 由 registry 中 `建议试点用例` 与汇编/bin stem 的精确匹配生成目标，必要时通过 `TB_FUNCOV_TARGET_BINS` 显式覆盖。

标准场景由两部分共同构成：

1. 汇编指令 pattern：RVI/RVC、分支、跳转、页边界、fetch block 边界和目标地址布局。
2. agent 激励：PTW、CSR、PMP/PBMT、ICache/Uncache、backend commit、canAccept、redirect、错误注入和反压。

标准汇编链路：

`case.S -> RISC-V gcc/objcopy -> case.bin -> NEMU log -> golden trace.jsonl -> DUT bin-trace pytest`

单个汇编用例入口：

```bash
src/test/python/Frontend/scripts/run_baremode_asm_bin_trace.sh \
  src/test/python/Frontend/tests/asm_cases/<case>.S
```

已有 bin 的入口：

```bash
src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh <case.bin>
```

Python testcase 使用 `env/sequences/`、`env/api.py` 和现有 agent 构造额外激励。优先扩展语义兼容的长期 testcase；只有现有 testcase 无法清楚表达场景时才新增。

禁止缩短 trace、降低目标 cursor、隐藏 monitor error 或放宽 checker 将失败包装为通过。

## 7. 功能覆盖率建模

coverage registry 定义 `Bin_ID -> Coverage_Group -> Coverpoint -> Bin_Name`，`functional_coverage.py` 负责加载定义、统一调度、event 采样、记录命中和输出 artifact，`funcov.py` 维护模块级周期采样 predicate。二者共同组成唯一可执行功能覆盖率实现。

建模规则：

- 一个叶子只绑定一个 group/point/bin。
- 一个 Bin_ID 只归属一个叶子。
- `(group, point, bin)` 全局唯一。
- 需要多个条件联合时建立独立 cross point、cross bin 和对应叶子。
- ready/valid 接口优先在 `fire` 采样。
- cross 条件必须来自同一事务或有明确的跨周期关联状态。
- reset、redirect、flush 和 recovery 后按真实寄存时序 gating。
- 不得用缺失信号的默认值制造 hit 或永久 unhit。
- sampler 必须保存首次命中 cycle 和关键事务 evidence。

模型单测负责证明 predicate、状态机和边界判定可执行；生成 DUT 的 signal contract 测试负责证明采样信号存在；真实 DUT testcase 负责证明场景可达。

## 8. 覆盖率口径

### 8.1 功能覆盖率

`FunctionalCoverageRecorder` 输出：

- `<tag>.funcov.json`
- `<tag>.funcov.summary.csv`
- `<tag>.funcov.unhit.csv`

只有通过真实 DUT 证据门禁的 JSON artifact 可以自动反标测试点。

### 8.2 代码覆盖率

DUT 通过 Verilator coverage 生成 `.dat`，pytest 使用 toffee `set_line_coverage()` 接入 line coverage 报告。现有脚本负责 line、branch、expr、toggle 汇总和 HTML：

```bash
python src/test/python/Frontend/scripts/report_raw_code_coverage.py --data-dir <run-dir>/coverage
src/test/python/Frontend/scripts/gen_coverage_html.sh <run-dir>/coverage
```

功能覆盖率主链不依赖 toffee `CovGroup`。删除 toffee 功能覆盖率不得删除 `dut.SetCoverage()`、`set_line_coverage()`、`.dat` 或 HTML 生成链路。

代码覆盖率只用于发现 RTL 空洞和评估回归广度，不能直接修改测试点状态。Verilator `.dat` 与 VCS VDB 不能混合，不同 DUT build 的 `.dat` 也不能合并。

## 9. 标准 artifact 与版本门禁

每次回归必须使用唯一 `run_id` 目录。可用于反标的 run 至少保存：

- testcase、汇编/bin 路径和目标 TP_ID/Bin_ID。
- 目标声明 `coverage_targets`（至少包含 `bin_ids`，必要时含 `tp_ids`）。
- design source SHA、DUT build SHA 和 generated RTL hash。
- registry SHA、sampler SHA、testcase SHA 和 signal contract hash。
- build config、工具版本、命令、退出码、pytest outcome 和 seed。
- funcov JSON、summary、unhit。
- `.dat`、代码覆盖率 summary、case log 和波形。
- checker、monitor、reference/trace 统计。
- 首次命中 cycle 和关键 evidence。

只有以下兼容性签名完全一致的 artifact 才允许默认合并：

`design SHA + DUT build hash + build config + registry SHA + sampler SHA + signal contract hash + toolchain`

不同版本默认拒绝合并。显式迁移必须证明旧 bin 语义未变、给出一对一映射、保留新增 bin 为 unhit，并记录迁移审计信息。

当前 JSON 已记录 registry、定义、sampler、DUT 模型库/Python 扩展、generated RTL tree、signal contract、build config 和 toolchain 签名；`merge_raw_files()` 会重新计算每个输入的 compatibility signature 和 definitions hash，签名不一致或字段被静默修改时直接失败。`make frontend` 成功后生成 design-build manifest，运行时重新核对 manifest 与实际产物哈希；源码脏、manifest 缺失或产物不匹配均不得进入自动 `HIT`。标准 runner 同时生成唯一 run 目录并保存 pytest outcome/退出码和 checker 状态。汇编 suite 会对逐 case artifact 执行只读反标审计，并输出明确标记为 diagnostic `observed` 的合并报告；该 aggregate 不能代替逐 case DUT evidence。

反标门禁必须实际检查同一 run 下声明的 waveform、raw `.dat`、case log 和 funcov 文件，而不是只检查路径字符串。waveform、`.dat` 和 funcov 文件为空或不存在时不得进入自动 `HIT`；安静用例允许 case log 为空，但文件必须存在。
反标时还必须重算当前 canonical registry 和 sampler 的 SHA，并重算 artifact 内 definitions hash。registry/sampler 已更新或 definitions 被改写的历史 artifact 只能作诊断证据，不得对当前测试点升级为 `HIT`。
反标时必须从声明路径重读 testcase、汇编源（存在时）、bin 和 golden trace，并与 artifact 记录的 SHA 逐项比较。输入文件丢失、路径非绝对路径或内容漂移时不得自动 `HIT`。

## 10. 状态与反标

状态只使用：

- `UNMAPPED`：没有 coverage 模型。
- `MODELED`：已建模，只有静态或模型验证，尚无有效真实 DUT 命中。
- `PARTIAL`：真实 DUT 已运行但目标未命中、语义不完整、testcase 失败或旧证据待重验。
- `HIT`：当前版本真实 DUT 回归通过且目标 bin 命中。
- `CLOSED`：人工完成测试点语义、checker 和波形/trace 证据验收。
- `BLOCKED`：明确的 DUT、design 或 environment blocker。
- `N-A`：评审确认当前设计不适用。

自动升级到 `HIT` 必须同时满足：

1. 使用编译后的真实 DUT。
2. 版本兼容性签名完整且匹配。
3. pytest PASS，退出码为 0。
4. monitor、checker、assertion 和 reference/trace 无未豁免错误。
5. 目标 `(group, point, bin)` 在同一 run 中命中。
6. 日志、波形、funcov 和 codecov artifact 属于同一 run。

bin 被触发但 testcase 失败时不得标记 `HIT`。`CLOSED` 只能人工写入，自动工具不得生成、降级或覆盖。

自动反标必须处理整个 active registry，不得写死 `BIN-5*` 等批次前缀，并对重复叶子、重复 bin、registry 漂移、版本不一致和缺失 artifact 直接失败。

## 11. 每周设计刷新

Frontend 设计每周更新时执行固定流程：

1. 冻结旧 baseline，记录新旧 design SHA。
2. 汇总 Frontend 相关 feature、bug-fix、timing-fix 和接口变化。
3. 建立设计文件/信号到测试点、bin、checker 和 testcase 的影响映射。
4. 增加、修改、合并或删除受影响测试点，并记录原因。
5. 同步修改 Condition、Checkpoint、Object、sampler、checker 和 testcase。
6. 重新编译 DUT，生成 build manifest 和 signal inventory。
7. 执行全量 signal contract、模型单测和受影响 testcase。
8. 运行当前版本 active 回归，独立生成 funcov 和 codecov 报告。
9. 旧版本受影响的 `HIT/CLOSED` 在重验前标记 `PARTIAL`，evidence 注明版本失效原因。
10. 自动反标后由人工完成新版本验收。

设计新增测试点会改变分母，覆盖率短期下降是正常现象。不得为了保持百分比单调而沿用失效证据或删除有效未覆盖点。

## 12. 三人协作与代码组织

IFU、ICache、iTLB/PTW/BPU/FTQ 等模块按负责人推进，各自维护对应 testcase、checker 和 sampler；公共 fixture、artifact schema、registry、Bin_ID 分配和最终反标由 ctrl 统一收口。

协作规则：

- 三人使用同一测试点主表、registry、runner、状态和 artifact schema。
- 不建立个人平行测试点表或个人 coverage registry。
- 模块代码可以分文件维护，但必须注册到唯一 recorder。
- Bin_ID 由统一 registry 分配，禁止个人占用重叠区间。
- 修改公共 sampler 或 fixture 时运行所有模块的一致性测试。
- canonical CSV 的机械更新应由工具按 TP_ID/Bin_ID 执行，避免整表格式改写和冲突。

## 13. 一个月推进目标与汇报指标

约 1000 个 active 叶子在一个月内全部完成人工语义验收、定向用例、真实 DUT 命中和波形关闭不应作为无条件承诺。一个月内必须优先完成：

- 100% active 叶子盘点、层级 review 和状态分类。
- 100% 叶子建立责任人、设计来源和 coverage/testcase 规划。
- P0 叶子优先完成模型、checker、testcase 和真实 DUT 闭环。
- P1/P2 按模块批量推进，BLOCKED/N-A 保持真实状态。

每周、双周和月度报告至少同时给出：

- Active 叶子数及本期新增、修改、删除数量。
- 已 review、已建模、已有 testcase、真实 DUT HIT、人工 CLOSED 的数量和比例。
- UNMAPPED、PARTIAL、BLOCKED、N-A 数量。
- P0/P1/P2 风险覆盖率。
- line、branch、expr、toggle 代码覆盖率，注明 DUT build。
- 本期新增覆盖模块、重新失效和重新关闭数量。
- 发现 bug、design blocker、environment blocker 和已释放风险。

原始百分比必须同时展示分子和分母变化。功能覆盖率增长、代码覆盖率增长和风险关闭是三类不同指标，不得混为一个数字。

## 14. 当前收口顺序

1. 保留唯一 JSON funcov 主链，移除重复的 toffee/SV 功能覆盖率采样。
2. 将 Coverpoint 纳入运行时定义、artifact key 和一致性检查。
3. 补齐唯一 run_id、build/registry/sampler manifest 和严格合并门禁。
4. 将 signal contract 扩展到所有 active 模型，缺失信号直接失败。
5. 将 funcov merge、代码覆盖率汇总和反标接入统一 regression runner。
6. 迁移三人现有模型和 testcase，隔离旧 registry 与历史 artifact。
7. 固化每周设计刷新和量化报告生成。

任何阶段都不得为提高命中率放宽 checker、伪造 hit、复用失败 artifact、合并不兼容版本，或把代码覆盖率当成功能闭环证据。
