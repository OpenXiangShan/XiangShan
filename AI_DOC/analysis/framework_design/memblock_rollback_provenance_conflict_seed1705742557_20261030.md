# Rollback跨拍来源误关联根因分析

日期：2026-10-30。状态：责任、正式方案及最终实现review PASS；已顺序coding、fresh compile、软件专项及20/20真实DUT专项PASS，方案移do，随本记录单Bug提交；提交后再恢复批量。

## 1. 原始合同与首错

- 母工作区：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028`，V2。
- 生产修复基线：`1343dbce3`，随后仅有状态文档提交；首段数据错误Bug已独立验证并提交，不能混为当前新错误。
- `tc=basicTest`、`ts=memblock_dispatch_real_smoke_vseq`、`cfg=tc_l2tlb_fault_2pct_no_trigger`、seed=`1705742557`。
- 每seed10000笔，timeout100000000ns、UVM_LOW、无临时plusarg；ranges0、PMA/PMP model1；monitor/record/coverage开启，无波形。
- 恢复批次：`mem_ut/ver/ut/memblock/sim/mother_fault3_recovery99_r5_20261030_results`。
- 首错：724655.800ns，`ROLLBACK_XMR cannot correlate rollback output rob=1/126 level=1 sample=144894 conflict=1`。
- 稳定日志：上述结果目录`/status/seed_1705742557.final.log`；monitor记录已先于TERM提交。
- 批次最终：0 PASS、1 FAIL、19 ABORTED、79 NOT_RUN，unknown0。不是99个功能失败；母fault3跨轮仍为1/100。

## 2. 并行矩阵的收敛

矩阵mode=`mother_stable_matrix_r5_20261030`已新编译。前5组各10/10严格验收，共50/250。
新错误出现后主agent向当前矩阵controller发送TERM，禁止继续启动后续组；该信号是首错联停管理动作，不是用户要求停止母任务。
当前第6组为`basicTest / memblock_dispatch_real_smoke_vseq / tc_dispatch_real_mmu_sv39_smoke`，
结果9 PASS、1 ABORTED（seed212889620）、0 FAIL，unknown0；原始summary的`stop_reason=user signal TERM`是脚本的通用信号标签。
该组尚未整体验收，不把它误记为Sv39功能复发或10/10通过。第7–25组未启动。

## 3. 静态候选与尚缺证据

`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv:1291`的
`resolve_rollback_output_provenance()`扫描输出sample之前最多4拍的全部同ROB/level输入；
两份输入的完整诊断payload不同即返回conflict。比较范围包含未获选候选的valid/ROB/level和附加诊断状态。
`agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv:736`将其解释为无法关联输出并报错。

候选解释：不同输入拍的合法候选变化，可能被窗口内全payload一致性要求误当为同一个rollback事件冲突。
这只是静态候选，不证明DUT输出正确；必须以实际winner、输出寄存级、采样区域、ROB/level和原因信号波形核对。
禁止直接改成“取最近一条”、忽略conflict或调大窗口；首先要证明输出对应哪个输入sample，以及重复/失效/reset边界。

## 4. 当前复现与独立分析

原参数带波形复现目录：`mem_ut/ver/ut/memblock/sim/mother_rollback_1705742557_debug_20261030_results`。
复用已编译、wave+coverage能力具备且2540输入与当前生产源码一致的mode=`mother_first_split_fix_20261030`，
simv SHA256=`ed06da6e336aab91b015aa1f7254a5deb35b202f3351fdf71d97cc1b5e057d31`，不覆盖旧结果目录。
显式保留UVM_LOW、原tc/ts/cfg/seed/请求数/timeout，不新增临时plusarg；不因开启波形而改激励。
Turing只读独立核对缓存算法、monitor/connect及RTL寄存级；目前无coding/正式责任结论。

### 独立静态review与采样合同

Turing于2026-10-30对精确same-sample候选方案静态`REVIEW_PASS`，不替代本seed正式责任分类。
主agent独立复核`src/main/scala/xiangshan/mem/MemBlock.scala:1438–1454`及当前生成`build/rtl/MemBlock.sv:30512`：
六个候选端口经组合winner/Mux1H直连memoryViolation，无额外顶层寄存级。
monitor的输出赋值（551行）与`sample_rollback_diagnostics()`均读取同一`mon_cb`；connect也指向同一RTL_PATH。
编译命令明确`+define+DEF_SETUP_TIME=1step`，精度1ps，不能按interface未覆盖默认值误算成提前1ns。
原失败日志在724645.8ns成功关联sample144892、age0；724655.8ns的sample144894出现conflict。
扫描该失败日志未发现任何age1–4成功关联，但此项不能替代波形证明。

候选最小方案仅收紧resolver为`entry.sample_seq == output_sample_seq`，保留完整ROB/level及同sample多记录payload冲突检查；
缺本拍证据仍失败，不选latest、不退旧样本、不删除payload_equal字段。
只有当前Bug所需的pending-MMIO软件定向合同需要同步，禁止扩展修理无关softcase；生产最小范围为一个sync package。
直接用本拍snapshot虽然更直观，但会重构monitor调用链并使既有cache接口闲置，本轮优先不扩范围。
历史review14也已承认当前RTL组合、旧问题来自clocking与边沿后HDL读取混用；不能用“未来pipeline”支撑当前跨拍宽容。
正式责任分类必须补波形中144894本拍winner/候选与输出一致、与144892相比变化字段的证据。
RAW当前storeIn只作附加诊断，不据此断言流水之前的致因store身份。

复现`effective_plusargs.txt`与失败批次已用`cmp`确认逐字节一致。NPI抽取脚本已备于debug结果目录
`trace_rollback.py`，按每拍posedge前1ps、保留原始位宽/X态提取144888–144896，当前等待仿真收敛后读取。

后续：等待原时间/现象复现，提取NPI输入/输出窗口；独立责任review通过后更新对应W44 Buglist，
编写并review最小方案，再在母工作区顺序coding、fresh compile、原seed与必要邻接/负向专项，单Bug提交后恢复批次。
strict-NC已确认RTL组仍跳过，不修改RTL，不新建worktree，不修无关soft_test。

## 5. 最终波形事实与责任结论

第1–4节保留发现/候选过程，不是当前仍待复现的状态。原参数带波形在724655.8ns同ROB1/126、sample144894、conflict1精确复现，
结果1FAIL、unknown0，controller退出。VCS仍用原版本，NPI读取使用具备内置Python3.6的Verdi V-2023.12-SP1；
R-2020.12-SP1没有该Python路径，最初抽取没有产生有效数据，改用已验证NPI版本后成功，未改仿真或工具安装。

| 采样编号 | 边沿前1ps取值点 | winner | candidate valid | 输出 |
|---|---:|---|---|---|
| 144892 | 724645799ps | 4，RAW-nuke0 | 0x10 | ROB1/126，level1，本拍匹配 |
| 144893 | 724650799ps | 无有效获选 | 0 | valid0 |
| 144894 | 724655799ps | 5，RAW-nuke1 | 0x20 | ROB1/126，level1，本拍匹配 |

两次有效事件的winner_known均0x37；非活动uncache未知payload没有被当成有效来源，活动获选payload和完整诊断均已知。
这不是非winner小变化，而是两个独立输入来源针对同一ROB先后发出的合法事件。
旧resolver允许age2旧记录参加本拍判断，再由`rollback_candidate_valid`的0x10≠0x20即可使完整payload比较失败。
本拍完整证据确实存在，无需伪造输入、删除比较字段、最新样本兜底或跨拍fallback。

权威结果：debug目录的`rollback_native_npi_trace.json`和`rollback_wave_acceptance.json`；断言脚本`check_wave.py`已由主agent与Turing独立执行PASS。
trace SHA256=`957f52e2626ecb3ddd1cd6e6b1d37ccd6f4bf5853d6923ab9d199c59a9e5c305`，同时记录原生packed二进制与十六进制，避免窄位宽/X态失真。
Turing正式责任`REVIEW_PASS`：测试框架rollback provenance跨拍误关联，非RM、非RTL仲裁；不扩展结论为RAW致因store全链均已证明。
已登记W44 RM目录中的测试框架章节，方案为`AI_DOC/plan/test_framework/plan/do/memblock_rollback_same_sample_provenance_fix_plan_20261030.md`。
正式方案review已PASS，实施与专项最新状态见第6节；不能仅凭责任通过记修复PASS或恢复批量。

## 6. 修复执行与验证进展

生产逻辑仅收紧resolver的sample比较为精确相等并补中文注释；相关软件task同步测试winner4→5、本拍缺失、
错ROB/level、跨拍非winner变化、同拍一致重复/不一致冲突、未来、reset及过期清除。没有修改其他softcase、RTL、RM或cfg。
原2540输入相对1343只有sync package和该测试task两个SV变化，逐路径hash已保存；原失败源码也已归档并核对hash，旧NPI断言可在修后重跑。

`mother_rollback_same_sample_protocol_20261030_results` fresh compile/软件专项PASS，basicTest只启动外加vseq调用目标task，
没有启动整个软件sequence的body；唯一marker/VSEQ_BODY/TEST_PASS/finish及六项零错误已验收，不计DUT PASS。
原1705742557 10000笔fresh compile及原参数完整STRICT_PASS，目录`mother_rollback_same_sample_original1705742557_20261030_results`；
首段原1992896882 10000笔在另一coverage mode重新编译并完整STRICT_PASS，18项真实DUT邻接用独立protocol mode全部严格通过。
所有均为当前Bug必要专项，不代表已恢复99/250批量；专项/最终review通过后才单Bug提交。
18项邻接现已全部STRICT_PASS，controller退出/unknown0；软件专项另计PASS。
原1705742557于724655.8ns已成功同拍关联，另有`failure_point_acceptance.json`保存前缀hash；
两颗完整10000笔根seed现已独立严格验收：正常结束、RM比较10000次、六项错误计数0、rc0/unknown0；Turing最终独立实现review PASS。
原170与199完整日志SHA256分别为24b07012037609494916f60d7c29d58944ad92e0114aa1f64cd14a8080d39db4、
c9a3e9c0b34c94e4dc83fdf9190ebea2a0e9efaacee6b04eb55fddd649a147c5；各目录strict_acceptance.json保留入口及hash。

补充旧工作区审计：只读核对`.codex_csr_prefix_fix_20261025`同名resolver（1008行），仍使用相同的age0–4过滤；
该文件HEAD到WIP差异无rollback相关修改，没有“旧source已修但未合入”的证据。本Bug是门禁关闭后新回归发现的问题，
不是必须恢复旧residency/deferred机制才能修复。source未改，母工作区继续顺序闭环。
