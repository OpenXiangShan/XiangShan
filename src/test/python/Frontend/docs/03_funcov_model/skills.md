---
name: frontend-bt-verification-closure
description: 统一 Frontend BT 从设计分析、测试点、环境与 checker、汇编和 agent 激励、功能覆盖率建模、真实 DUT 回归、代码覆盖率到只读 HIT 审计和人工验收的工作流。
---

# Frontend BT 测试点驱动验证与覆盖率闭环规范

## 1. 目标与边界

Frontend BT 的唯一主流程是：

`设计分析 -> 叶子测试点 -> 环境与 checker -> testcase -> 功能覆盖率 -> 真实 DUT 回归 -> artifact -> 只读 HIT 审计 -> 人工 CLOSED`

测试点是所有验证活动的输入。功能覆盖率用于证明目标场景已经被激励并采样；当前阶段的 `HIT` 只以这一激励与采样证据为目标。checker、assertion、协议检查或 trace/reference 对比用于证明 DUT 行为正确，属于后续 `CLOSED` 的语义验收证据。已启用的检查不得出现未豁免错误，但某个叶子尚未建设 checker、assertion 或 reference，不得阻塞其 `HIT`。

当前不建设完整 cycle-accurate golden model。能够使用 NEMU trace 的指令语义场景继续进行 DUT/golden 对比；其他场景使用协议级 scoreboard、跨周期 invariant、checker 和 assertion 检查正确性。

禁止用以下结果代替真实闭环：

- 测试点或 bin 已存在。
- FakeDut 或模型单测命中。
- testcase 曾经运行但没有目标场景证据。
- 功能 bin 命中，但该 run 中已启用且适用的 checker、monitor、assertion 或 trace/reference 检查失败。
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
- 功能覆盖率实现地图：`../../env/funcov/README.md`
- Python testcase：`../../tests/`
- 汇编 testcase：`../../tests/asm_cases/`
- 环境、artifact 布局和回归入口：`../../README.md`

`pilot` 仅为历史兼容文件名，不表示仍处于试点阶段。原 `docs/frontend_bt_functional_coverage_pilot.csv` 重复副本已删除，不得重新建立平行 registry。

功能覆盖率只允许一套正式 runtime 链：fixture 装配 `FrontendFuncovSampleHub`，挂接
`ToffeeCoverageSink` / native `CovGroup`，各功能域共用一个周期级 DUT snapshot。每个 case
的 native groups 在 teardown 累计到同一 pytest session；启用 functional coverage 时只在 session finish 写出
`funcov/toffee.funcov.json`。失败 case 的已采样 bin 同样计入该原生 aggregate。旧 `FunctionalCoverageRecorder` JSON
ledger、`merge_funcov.py` 以及 `TB_ENABLE_TOFFEE_FUNCOV` / `TB_ENABLE_FUNCOV_AUDIT` /
`TB_ENABLE_TOFFEE_FUNCOV_PILOT` 已删除，不得再恢复为正式路径。不得通过其他 Python 文件或
SV covergroup 并行维护相同 group/point/bin 的第二套正式命中逻辑。VCS/Verdi 功能覆盖率
可以用于临时调试和交叉检查，但不能与 Toffee functional coverage 合并为同一个分子。

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
- 唯一 coverage 反标。叶子粒度与整个 coverpoint 一致时使用 `covergroup <group>, coverpoint <point>`；叶子已经细化到具体取值组合时使用 `covergroup <group>, coverpoint <point>, bins <bin> (BIN-xxx)`。
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

Python directed testcase 通过上述 marker 声明目标，也可以通过
`TB_FUNCOV_TARGET_BINS`、`TB_FUNCOV_TARGET_TP_IDS`、`TB_FUNCOV_TARGET_TESTCASES`
为当前运行补充目标。当前 runtime 使用这些声明校验目标并配置 sampler，但原生
`toffee.funcov.json` 不保存 `coverage_targets`，因此不能仅从该报告反推出 testcase
归属。通用 bin-trace 可以不声明 `bin_ids`，不得仅根据 bin 文件名推断 coverage 所有权；
一旦声明目标，未知 Bin_ID 或 testcase 名必须使 run 失败。directed testcase 可以专门覆盖
目标 bin，但必须按叶子测试点定义构造真实 DUT 场景，不得伪造 coverage event、force 内部
状态，或绕过该 run 中已启用的检查；`HIT` 阶段不要求每个叶子已有完整 reference model
或语义 checker。

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

Python testcase 使用 `env/sequences/`、`env/api/` 和现有 agent 构造额外激励。优先扩展语义兼容的长期 testcase；只有现有 testcase 无法清楚表达场景时才新增。

禁止缩短 trace、降低目标 cursor、隐藏 monitor error 或放宽 checker 将失败包装为通过。

## 7. 功能覆盖率建模

coverage registry 定义 `Bin_ID -> Coverage_Group -> Coverpoint -> Bin_Name`。
`FrontendFuncovSampleHub` 加载定义、协调 event/cycle 采样并维护共享 snapshot；Toffee sink /
native `CovGroup` 负责命中记账和 canonical coverage report。原有功能域 predicate 继续复用，避免
在迁移中建立第二份判定逻辑。

建模规则：

- 一个叶子只绑定一个 coverage 落脚点：整个 `(group, point)`，或更细粒度的 `(group, point, bin)`。
- 同一个 coverpoint 只能采用一种反标粒度：由一个叶子拥有整个 point，或由多个兄弟叶子分别拥有其唯一 bin；禁止 point 级和 bin 级归属重叠。
- point 级叶子在该 coverpoint 收到至少一次有效采样时命中；其子 bins 只用于展示取值分布，不要求全部命中。若每个子 bin 都是独立验证要求，必须拆成兄弟叶子并分别使用 bin 级反标。
- 禁止一个叶子绑定多个独立 point/bin，也禁止用未在测试点定义中声明的多个 bin 的 AND、OR 或聚合命中结果定义该叶子的 `HIT`。
- 一个 Bin_ID 只归属一个 bin 级叶子，或归属于唯一 point 级叶子的分布明细。
- `(group, point, bin)` 全局唯一。
- 需要多个条件联合时建立独立 cross point、cross bin 和对应叶子。
- ready/valid 接口优先在 `fire` 采样。
- cross 条件必须来自同一事务或有明确的跨周期关联状态。
- reset、redirect、flush 和 recovery 后按真实寄存时序 gating。
- 不得用缺失信号的默认值制造 hit 或永久 unhit。
- sampler 可以在运行时保留首次命中 cycle 和关键事务 evidence 供 checker/诊断查询，但原生
  Toffee report 不输出这些字段，不能把内存中的 detail 当作持久签核证据。
- `bins` 按需独立记账的可观察场景划分；单 bin coverpoint 合法，不得为形式完整加入补集或占位 bin。
- event encoding 只用于天然互斥的类别。独立并发条件必须使用独立 coverpoint 或 bin，不得因 `if`/`else if` 优先级丢失低优先级观察。
- 优先使用直接、可读的 coverpoint predicate；不得仅为减少 coverpoint 数量引入难以审查的 `always_comb` event selector。

### 7.1 Snapshot 与采样相位

SampleHub 在每个 DUT cycle 建立共享 read-once snapshot；同一周期内对同一内部信号的 coverage
读取复用首次值，避免多个 predicate 重复访问 DUT。该 snapshot 表示 coverage callback 所在的
固定采样相位，不是任意时刻都重新读取的 runtime view。

testcase 或时序 canary 若需要观察 agent drive 之后的 TL、redirect、ready/valid 等边界，必须从
已绑定 bundle 或当前生成 DUT inventory 中验证过的 runtime alias 直接读取，并明确使用
pre-drive 或 post-drive observer。禁止把 SampleHub 的早期缓存值与同周期 post-drive 接口值混合
后声称它们天然同相；跨相位组合必须有明确的时序合同。

模型单测负责证明 predicate、状态机和边界判定可执行；生成 DUT 的 signal contract 测试负责证明采样信号存在；真实 DUT testcase 负责证明场景可达。

### 7.2 Condition 可观察性与跨周期关联

- 统计和审查测试点时使用 `csv.reader` 读取逻辑记录。物理行号只用于定位文件位置；只有层级末端且同时具有 `Condition`、`Checkpoint`、`Object` 的记录才计为可执行叶子，不能把标题行或继承行计入分母。
- 在标记 `MODELED` 前，将 `Condition` 的每个必要子条件逐项映射到当前选定 simulator 的 DUT object、generated RTL 或 signal contract。源 RTL 中存在、但当前生成 DUT package/bind 未暴露的信号，不得用同名猜测、默认值或旁路信号替代；应保持 `UNMAPPED` 或 `BLOCKED`，并记录具体缺失接口。
- Coverpoint/bin 只证明场景的触发条件和输入状态。`cfVec` 结果、异常交付、flush 后清空、恢复后取指正确性等属于 `Checkpoint`，必须交给 checker、assertion、scoreboard 或 trace 对比；不能用结果信号反推未观察到的触发原因。
- 跨周期场景必须保存并匹配最小充分的事务身份，例如 requestor/source、FTQ pointer 与 offset、VPN 或 transaction tag，再将 response、fault 或 redirect 归属于同一事务。全局 pending bit、任意下一次 `valid` response 或仅凭相邻周期不能证明事务关联。
- 将 `PARTIAL` 或 `UNMAPPED` 时，说明缺失的是哪一个 Condition 信号、时序关系或事务身份；不要把尚未证明的 Checkpoint 当作模型缺口。Condition 建模完成但尚无真实 DUT 命中时应为 `MODELED`，只有同一版本、同一 run 的完整证据才能升级为 `HIT`。

### 7.3 Producer 审计与诊断边界

场景已描述、registry 已映射、runtime producer 可执行、当前 DUT 场景命中是不同证据。审计必须沿 `Bin_ID -> registry -> sampler/event source -> 实际采样条件` 检查，包括动态循环和跨周期 pending 状态；只搜索 registry key 或存在 `mark` 调用不能证明合法运行路径可达。已有 producer 缺口检查位于 `tests/py/jiabowen/test_functional_coverage_pilot_schema.py`，不在过程文档里另维护一份数量清单。

发现缺口时记录具体阶段和所缺信号、身份或条件，不能仅凭缺少专用 producer 就自动改写测试点状态；是否需修订定义、建模或适用性须 review。`__Vtogcov__`、扁平 alias 与子模块端口需要按当前 build 验证语义及采样相位，并保存实际采用路径；缺失、不可读、跨拍错配的负例不得产生目标命中。

未命中运行仅用于诊断。跨事务分别观察到子条件、raw candidate、少量 seed 失败或更换布局仍未命中，均不证明目标组合已覆盖，也不证明 RTL 全局不可达。可复用检查沉淀为 unit/contract/canary，未决设计前提保留在验证方案待评审项中；已结束的实验日志和阶段数字留在 artifact/Git 历史。

## 8. 覆盖率口径

### 8.1 功能覆盖率

正式 runtime 启用 functional coverage 时，每个 pytest session 只输出一份原生 Toffee report：

- `funcov/toffee.funcov.json`

该 report 只统计 CovGroup bin hints，不包含 per-case outcome、checker、provenance、Bin_ID 或
cycle evidence；失败 case 也会贡献 hints。因此它不能作为 `HIT` 审计证据。
`scripts/run_pytest_with_log.sh` 默认在同一 `funcov/` 目录再写一份 toffee-test
`funcov.html`，用于按用例查看 bin hints；它同样不是 `HIT` 证据，可用
`TB_ENABLE_TOFFEE_HTML_REPORT=0` 关闭。旧 `<tag>.funcov.json` / summary / unhit
ledger 已退出正式路径。

### 8.2 代码覆盖率

DUT 通过 Verilator coverage 生成 `.dat`，pytest 使用 toffee `set_line_coverage()` 接入 line coverage 报告。现有脚本负责 line、branch、expr、toggle 汇总和 HTML：

```bash
python src/test/python/Frontend/scripts/report_raw_code_coverage.py --data-dir <run-dir>/coverage
src/test/python/Frontend/scripts/gen_coverage_html.sh <run-dir>/coverage
```

Toffee functional coverage 与 Verilator code coverage 是两条独立链路。关闭或重构 Toffee
`CovGroup` 不得删除 `dut.SetCoverage()`、`set_line_coverage()`、`.dat` 或 HTML 生成链路。

代码覆盖率只用于发现 RTL 空洞和评估回归广度，不能直接修改测试点状态。Verilator `.dat` 与 VCS VDB 不能混合，不同 DUT build 的 `.dat` 也不能合并。

## 9. 标准 artifact 与版本门禁

每次回归必须使用唯一 `run_id` 目录。当前 runner 分别保存：

- session 级原生 `funcov/toffee.funcov.json`，以及可选的 `funcov.html`；
- pytest 总日志和每个 DUT case 的 case log；
- Verilator `.dat` 与 FST/VCD，或 VCS VDB 与 FSDB；
- bin-trace runner 声明的 bin、golden trace、运行命令、seed 和 pipeline 结果。

这些产物彼此独立。原生 Toffee JSON 只包含 Toffee group、point、bin、hints 和采样统计，
不包含 `coverage_targets`、pytest outcome、checker/monitor 结果、manifest、版本 SHA、路径 hash、
first/last cycle 或 evidence。`funcov.html` 也是查看页，不补充这些签核字段。

`merge_toffee_funcov.py` 只读取各输入的 `groups` 并通过 Toffee reporter 聚合，不执行旧
Frontend sidecar 的 compatibility/provenance gate，也不排除失败 case 已产生的采样。因此 suite
aggregate 只能回答“这些子进程累计观察到哪些 bin”，不能回答“哪个通过的 testcase 对哪个
测试点形成 HIT”。不同 DUT build、registry 或 sampler 的报告不得因为工具当前未检查这些身份
就被当作可签核合并；需要跨版本比较时，必须在报告之外人工核对版本和模型语义。

`make frontend` 仍生成 design-build manifest；pytest outcome、checker/monitor、日志、波形、
代码覆盖率和输入文件仍是判断运行有效性的独立证据。当前 runner 不会把这些证据封装进原生
Toffee report，也不会自动生成 testcase 级 HIT audit artifact。需要 HIT 审计时，必须从同一
run 的独立产物重新核对真实 DUT、manifest、pytest/checker 结果、目标声明和输入身份；不能把
session aggregate 本身当作签核结论。

## 10. 状态与反标

状态只使用：

- `UNMAPPED`：没有 coverage 模型。
- `MODELED`：已完成建模，但尚无当前版本真实 DUT 命中。
- `PARTIAL`：模型、场景刺激或当前运行证据仍不完整，例如 testcase 失败、目标未命中、采样/归属不清或旧证据待重验。它是中间状态，不代表功能覆盖率建模完成，不能当作最终目标或验收完成态。
- `HIT`：当前版本真实 DUT 回归通过，且目标 bin 在该场景中命中；不以 reference model 或语义 checker 是否已建设为前提。
- `CLOSED`：人工完成测试点语义正确性验收；按该测试点适用性审阅 checker、assertion、reference、波形或 trace 证据。
- `BLOCKED`：明确的 DUT、design 或 environment blocker。
- `N-A`：评审确认当前设计不适用。

人工或独立只读审计判定 `HIT` 必须同时满足：

1. 使用编译后的真实 DUT。
2. 从独立 manifest、registry 和 sampler 证据核对的 DUT/覆盖率模型身份完整且匹配。
3. pytest PASS，退出码为 0。
4. 该 run 中已启用且适用的 monitor、checker、assertion、reference 或 trace 检查无未豁免错误；未建设或未适用某类检查本身不阻塞 `HIT`。
5. 同一 run 的原生 Toffee report 中，point 级目标至少产生一次有效子 bin 采样，或 bin 级目标的指定 `(group, point, bin)` 命中。
6. 日志、波形、funcov 和 codecov artifact 属于同一 run。
7. 若结论需要归属到具体 testcase，另有 testcase marker/目标声明及同 run 运行证据；session aggregate 本身不能证明该归属。

bin 被触发但 testcase 失败时不得判定为 `HIT`。`CLOSED` 只能人工写入；审计工具不得修改测试点 CSV 的 `status/testcase/evidence`。

全局基线只读审计必须处理整个 active registry，不得写死 `BIN-5*` 等批次前缀，并对重复叶子、重复 bin、registry 漂移、版本不一致和缺失 artifact 直接失败。测试点主表只保存静态映射；动态 HIT 结论和运行 evidence 不写入原生 Toffee report。如需机器可读结论，应由独立只读 audit/projection artifact 承载，且不得修改测试点 CSV 的动态状态。

## 11. 每周设计刷新

Frontend 设计每周更新时执行固定流程：

1. 冻结旧 baseline，记录新旧 design SHA。
2. 汇总 Frontend 相关 feature、bug-fix、timing-fix 和接口变化。
3. 建立设计文件/信号到测试点、bin、checker 和 testcase 的影响映射。
4. 增加、修改、合并或删除受影响测试点，并记录原因。
5. 同步修改 Condition、Checkpoint、Object、sampler、checker 和 testcase。
6. 仅当 DUT 编译输入或编译配置发生变化时重新编译 DUT；否则复用已有 DUT build，并刷新/校验 build manifest 和 signal inventory。
7. 执行全量 signal contract、模型单测和受影响 testcase。
8. 运行当前版本 active 回归，独立生成 funcov 和 codecov 报告。
9. 旧版本受影响的 `HIT/CLOSED` 在重验前标记 `PARTIAL`，evidence 注明版本失效原因。
10. 只读 HIT 审计后由人工完成新版本验收。

设计新增测试点会改变分母，覆盖率短期下降是正常现象。不得为了保持百分比单调而沿用失效证据或删除有效未覆盖点。

### 11.1 迁移门的复用边界

每次设计刷新把影响分为探针/采样语义迁移、场景重跑、仅 provenance 更新和不受影响四类。记录 design baseline、编译 source/implementation、配置与产物哈希，不能将设计分支 SHA、验证分支 SHA 和 build hash 混为一个版本。

先核对当前 manifest 和 signal inventory，再验证 alias/bind、采样相位、跨周期身份及缺探针负例，随后执行受影响的真实 DUT 场景。聚焦回归通过不等于全量 Python/ASM 已通过；通过、失败、skip 和 artifact eligibility 必须分开报告。旧基线的“迁移完成”、历史 HIT 数字或一次缺信号结论均不能放行新版 DUT；测试点增删或状态调整须先 review，不静默改变分母。

## 12. 三人协作与代码组织

IFU、ICache、iTLB/PTW/BPU/FTQ 等模块按负责人推进，各自维护对应 testcase、checker 和 sampler；公共 fixture、artifact 布局与证据规则、registry、Bin_ID 分配和最终审计由 ctrl 统一收口。

协作规则：

- 三人使用同一测试点主表、registry、runner、状态、artifact 布局和证据规则。
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

1. 正式路径固定为 SampleHub + Toffee；启用 functional coverage 时，每个 pytest session 只输出原生 `toffee.funcov.json`。
2. 失败 case 的采样与通过 case 同样累计；不再维护逐 case Frontend signoff gate 或 sidecar merge。
3. 不恢复 CSV 动态写回。
4. 在目标规模 DUT 回归中取得完整 pytest summary 和 session report。
5. 清理文档与入口中对已删除 legacy recorder / audit / fallback 开关的残留描述。
6. 固化每周设计刷新和量化报告生成。

任何阶段都不得为提高命中率放宽 checker、伪造 hit、复用失败 artifact、合并不兼容版本，或把代码覆盖率当成功能闭环证据。
