# LSQ准入方案与B05动态证据复核

日期：2026-11-02。仿真日期：2026-10-31。主审：当前主agent。
结论：动态证据可采信；方案仍有两项阻断，不是coding准入PASS。本次没有生产代码修改，不是实现验收。

## 1. 术语与抽象功能说明

| 术语 | 当前含义及代码落点 | 例子 |
|---|---|---|
| ROB / UID | 后端位置标签 / 框架请求编号 | UID492与652的ROB value均12，但flag不同 |
| LQ / SQ / LSQ | Load队列 / Store队列 / 两者统称 | 实际容量72/56，不是本拍请求数量 |
| committed / completed | 已允许Store不可撤销 / 已完成SQ后续接收条件 | STD写回不等于SQ completed |
| STD / WB / SBuffer | Store数据发射 / 写回 / Store缓冲 | SBuffer背压使已提交Store继续占SQ |
| admission / lane | 上游准入 / 固定物理槽位 | 部分接受只清该槽valid，不移动剩余请求 |
| credit / shadow | 模拟上游寄存信用 / 该状态的框架副本 | 真实sqDeq原拍回信用，不等软件释放 |
| reservation / owner | 软件预占资源 / 对应请求实例 | 延迟业务消费只释放软件预占，不能重复回shadow |
| redirect / epoch | 硬件取消事件 / 软件执行代际 | 软件epoch变化不能直接替代硬件逐拍redirect |
| raw / settled | 尚未业务消费的采样事实 / 当前采样已完成状态结算 | 先收齐事实，再构造下一边沿输入 |
| prefix / R / Q / D | 混合访存前缀 / decode到rename状态 / rename到dispatch状态 / 前级输入 | 新组经D到R再到Q，不凭空补入Q |

本文复核观测事实与计划的职责边界，不新增checker、RM或实现函数。逐函数方案见下列plan；动态解析方法与观测缺口见取证报告。

## 2. 范围及依据

工作区：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028`，HEAD=`e275e8819b11b2329f01f9788c47655e8711d107`。

- 方案：`AI_DOC/plan/test_framework/plan/undo/memblock_lsq_rtl_equivalent_admission_formal_plan_20261031.md`。
- 取证：`AI_DOC/analysis/framework_design/memblock_b05_rob_sq_lifecycle_recheck_20261031.md`。
- 原入口：`tc_dispatch_real_smoke / virtual_base_sequence / default / seed1545815340`，1000笔、CHECK_TRIGGER=0、ISSUE=1。
- 结果：`mem_ut/ver/ut/memblock/sim/mother_b05_rob_sq_lifecycle_20261031_results/summary.log`。
- 数据：`mem_ut/ver/ut/memblock/sim/mother_b05_rob_sq_lifecycle_20261031/analysis/lifecycle_evidence.json`及带行号的目标UID日志。
- 源码：`NewDispatch.scala`、`PipeGroupConnect.scala`、`CtrlBlock.scala`、`Rename.scala`、`LSQWrapper.scala`、`VirtualLoadQueue.scala`、`StoreQueue.scala`，及当前LSQ sequence/driver/service。

本次主审读取有效JSON并独立按完整flag/value重算指针距离，核对目标入队和SQ状态；没有重做整个FSDB转换，也未声称独立重放了全部仿真。

## 3. 动态证据结论

| 核对点 | 主审核验 |
|---|---|
| 仿真收口 | summary为1PASS/rc0/unknown0；本次没有新增仿真 |
| UID身份 | 入队为492=ROB1/12、SQ1/20；652=ROB0/12、SQ0/15，与独立UID日志匹配 |
| 首个门槛违例 | 365.700ns：LQ指针0/68减0/1，占67，余5；SQ占48，余8；无redirect，请求有效 |
| UID652入队 | 19295.700ns：LQ占66余6；SQ指针0/15减1/17，占54余2；旧492仍allocated/committed且未completed |
| 全程计数 | 有效解析1042次入队，含一次redirect后的42次重入队；不满足联合门槛的请求边沿132个，不是132个独立Bug |
| 旧492出队 | 19500.700ns接收后completed置位，19505.700ns真实出队；19513.200ns软件释放，晚7.5ns |

联合门槛按队列指针占用计算，不用当前req数量，也不以allocated总和替代。canAccept是由实测指针和源码公式推导，不是本次直接dump的wire。UID652时cancel输出29/13是旧redirect保持水平；实际redirect与目标取消为0，不能当作同拍回收量。

两个现象分别解释：旧Store已经提交，但SQ前面仍有请求且SBuffer未ready，因此仍占SQ；框架只检查本批需求装得下，未实现联合保留余量及上游寄存准入，所以又发送新组。前者不证明ROB重复分配，后者是本轮已证实的激励准入缺口。

生成RTL中没有对应must-accept可执行断言；因此运行PASS和门槛违例可以同时出现。原历史FAIL日志不可读，不能以本次PASS追认其激励合法，也不能仅凭本次无合法门控的轨迹证明B05过滤的完整必要性。

## 4. 方案评审

原行为：候选按分类实际需求不超过软件free选取；软件释放时机与物理出队不同，没有完整上游寄存信用或联合许可。

计划行为：逐采样模拟`LsqEnqCtrl`信用、许可、输出寄存与取消回补；保留软件reservation原所有权；固定lane按旧混合前缀选择，R/Q合法捕获，partial不压紧；物理出队原拍回shadow，业务延迟释放不再影响该次硬件信用。

已修订的方案问题：

- collector先保存同拍事实，再推进shadow，禁止读到旧历史末项。
- settled使用持久seq/epoch条件，offer在目标边沿前冻结，不能事后补造输入。
- partial保留lane空洞，按`PipeGroupConnect`规则捕获；不把renameIn永远置零却凭空生成新bundle。
- 无效R的旧Store bits仍按源码影响RenamePrefix，reset未知payload不能被假设为零。
- 当前VCS的setup/hold=1step及posedge采旧、negedge service静态相位已有依据；动态首尾连续性仍列专项验收。

两项阻断尚未关闭：

| 编号 | 缺口与必须补齐的交付 |
|---|---|
| P1-01 | 明确现有flush/epoch abort发生于源端未fire还是寄存输出已接受之后；提供冻结offer、实际redirect采样、launch、sample及软件取消逐拍映射。不能为对账回撤已接受输出 |
| P1-02 | RTL规则已确定为旧t3为真时消费上一拍K；仍需给出框架连续样本、软件记录归属及恢复边界的端到端映射。不能用最后非零值、level差分或软件新事件替代硬件使能；只拒绝复杂轨迹不等于已满足当前回归支持范围 |

P1-01已有具体源码风险，不是泛泛等待测试：`common_data_transaction.sv:5127–5128`在请求flush时立即置freeze并增加epoch；主service在shadow递推之后才处理业务redirect，`memblock_dispatch_real_cancel_reconcile_vseq.sv:175,250`又可在同一negedge独立发flush。若shadow helper提前发布settled并允许下一offer冻结，同一negedge后续flush可使该offer在posedge被旧driver撤销。仅有持久seq防丢唤醒不能消除这个phase竞态。最终方案须统一offer可构造时机及flush请求归属，明确已launch与尚未fire两条路径；不能假定只有一个flush写者或直接忽略epoch检查。

Mencius于2026-11-02完成独立只读复核，结论同为非coding-ready：保留原两项阻断，不新增第三项。其特别指出，只把settled移到service尾部仍未覆盖独立vseq的同negedge producer；必须覆盖所有flush生产者以及未fire、已接受输出、已采样pending三种阶段。该复核没有修改文件或追加仿真。

Carson补充源码符号递推：内部R首拍E0，needAlloc持续全零时，R=101使E3/E5回补K[2]/K[4]，R=1110使E3/E4/E5回补K[2]/K[3]/K[4]。计数不必变化，held值是否消费只看t3；方案已删除软件new_count使能含义。此项关闭的是RTL公式疑问，不是框架端到端实现/验证门禁，因此仍保留P1-02。

两项关闭前，正式文件只是待完善方案，不允许标coding-ready。现有普通单脉冲时序表不能替代这两项证明。本次没有因该缺口自动扩大到RTL修改、B05过滤修改或全量回归。

## 5. 验收边界与下一步

顺序为：补齐两项映射并独立复核；获得当前方案coding授权；只在mother顺序修改；fresh compile；原seed及余量边界、混合lane、空拍、取消和延迟消费专项；据新证据决定回归与覆盖率重验范围。每个实际Bug修复专项通过后单独提交，文档分析不冒充已修复提交。

历史350项仍是原检查口径的运行PASS，不改写冻结结果。它们尚未按新增准入合法性条件重新验收；不得用该覆盖率宣称LSQ等价闭环。当前strict-NC问题责任也不因本次B05重审自动改变。

## 6. 实现与Plan不一致项

没有本轮实现改动，不能出具实现对齐PASS；当前生产代码仍是待替换的旧准入逻辑。本次评审仅确认计划及证据，未执行计划中的专项矩阵。

## 7. Plan未说明但Coding落实的细节

本轮未coding，不存在额外实现细节。新增方案中的函数和字段尚未落地，不能写成已有实现。

## 8. 非本次修改的逻辑分析

本次主题文件为方案、B05取证、本文与mother状态表。已核对`git status --short`；`git diff -- mem_ut src`为空。

| 类别 | 路径 | 处理 |
|---|---|---|
| 原有接口知识库修改 | `AI_DOC/analysis/interface/v2/agents/int_writeback_agent.md` | 未改、不暂存，不作为本轮实现 |
| 原有RTL知识库修改 | `AI_DOC/analysis/rtl/v2/flows/rob_compress_and_backend_instruction_flow.md`、`AI_DOC/analysis/rtl/v2/index.md`、未跟踪`backend_memory_writeback_flow.md` | 不覆盖、不回滚，不自动更新V2知识库 |
| 原有仿真配置 | `mem_ut/ver/ut/memblock/signal.rc` | 未跟踪，保持原样，未用于新增修改 |
| 仿真/分析产物 | ignored mode/results及波形转换文件 | 保留证据，不提交为生产代码 |

没有发现本轮生产源码或cfg变更；未git add/commit，未向远端push。
