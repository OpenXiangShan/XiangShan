# fault3新seed的cancel快照深度失败分析

创建日期：2026-10-30；更新：2026-10-31。当前状态：本Bug验证通过，修复提交`50e80d275`，方案/实现/最终350项回归及覆盖率独立review均PASS，无活动仿真。母任务因独立strict-NC RTL组保持`STOPPED_RTL_CONFIRMED`。第2–6节保留原取证阶段，其待确认/待修改措辞不代表当前状态。

## 1. 术语与证据边界

| 术语 | 当前含义 | 对象 |
|---|---|---|
| raw/local queue | monitor原始队列与公共data对账缓存，两者不是DUT硬件队列 | `raw_cancel_snapshot_q / cancel_snapshot_history_q` |
| snapshot | 每个采样编号的LQ/SQ取消计数，含零值 | `dispatch_raw_cancel_snapshot_t` |
| anchor | redirect实际在DUT接口被采到的时序身份 | `dispatch_raw_redirect_anchor_t` |
| cancel record | 框架某次redirect的软件取消与DUT观测对账记录 | `cancel_record_q` |
| baseline | 非取消目标拍应保持的计数 | `cancel_held_*` |
| software_count_finalized | 对应redirect的框架victim扫描已结束，可与目标拍比较 | record字段 |

报错来自框架缓存深度检查，不代表已经证明RTL队列溢出。必须区分搬运前未消费、缺anchor和软件计数未完成等不同根因。
不因历史相同报错就直接复用旧修复，也不提高14、压缩未证明可丢弃的snapshot或放宽strict比较。

## 2. 原始失败与批次

- 源码：母工作区`.codex_mother_integration_20261028`，生产提交`bba3008baa01b1faf50cc0037053cd54693e6792`；后续HEAD仅文档变化。
- 入口：`tc=basicTest / ts=memblock_dispatch_real_smoke_vseq / cfg=tc_l2tlb_fault_2pct_no_trigger / seed=551576067`。
- 参数：MAIN10000、timeout100000000ns、无临时plusarg，PMA/PMP model1、ranges0、monitor/record/coverage开启。
- 首错：411428.500ns，`LSQ_CANCEL local cancel snapshot depth=15 exceeds compile bound=14`，UVM_FATAL1、caught/demoted均0。
- 结果目录：`mem_ut/ver/ut/memblock/sim/mother_post_rollback_fault3_recovery98_20261030_results`；root日志`status/seed_551576067.final.log`。
- 最终批次：20PASS/1FAIL/19ABORTED/58NOT_RUN，remote_unknown_jobs0，controller退出；原始summary/results/monitor提交及联停信号记录保留。
- bba版25组250矩阵已全部通过且独立review PASS；此新错误不撤销其历史结果，但母任务最终350项及coverage未完成。

## 3. 当前源码事实与待确认项

`common_data_transaction::add_cancel_snapshot()`在4436–4462行检查sample单调、计数合法后入队，超过14立即fatal。
adapter `drain_lsq_timing_sidebands()`在1575行先搬完snapshot，再搬anchor；reconcile是单独调用入口。
`service_cancel_reconcile()`在4562行附近：未锚定record可能阻止baseline消费，精确目标到达但软件计数未finalize也会等待。
原日志最后阶段持续出现UID1679 `STORE_FAULT_BARRIER defer SQ deq until STD real-WB ... std_dispatch=0 std_wb=0 pending_fault=1`；
此项目前仅是上下文，未证明它就是取消缓存阻塞根因。

W43旧`memblock_redirect_responder_topology_fix_plan_20261023.md`确认的是legacy testcase覆盖default responder；
本次basicTest显式VSEQ拓扑不同，不能仅凭相同fatal复用旧结论。更早“搬一条就reconcile”的初步建议也不是本次已确认方案。
待原波形确认：首个未消费snapshot、对应record/anchor、software_count_finalized、redirect驱动与观测sample、raw/local队列的实际增长过程。
Turing只读候选分析已启动；正式责任review必须等待本次复现证据，不提前登记Buglist。

## 4. 当前执行

独立mode=`mother_cancel_depth_551576067_debug_20261030`，结果目录`mother_cancel_depth_551576067_debug_20261030_results`；fresh compile PASS、rc0，原seed正在运行。
从原批次frozen cfg读取，原seed、MAIN10000、timeout和UVM_LOW保持一致，仅增加波形，fresh compile，不覆盖已有稳定simv。
两目录`effective_plusargs.txt`已用cmp确认逐字节一致。
未新建worktree、未修改RTL或测试框架生产代码。复现后按责任review、同周Buglist、最小方案独立review、授权coding、专项及单Bug提交闭环。

## 5. 原参数复现与波形事实

上述debug已完整复现同411428.500ns/depth15，1FAIL、unknown0，controller退出；不是仅相似症状或不同请求数复现。
波形绝对路径：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/mem_ut/ver/ut/memblock/sim/mother_cancel_depth_551576067_debug_20261030/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=551576067_rtl_eda_reg_20261030_094031_1533629_26089.fsdb`。
NPI按posedge前1ps保存原生位宽binary/hex；trace及断言在debug结果目录，trace SHA256为f529a7b9aa795b5d35697b390aaa8086bc99bb6c0641b0467ee68f12ef9e2e69。

| 时间 | 波形事实 |
|---|---|
| 411336ns | DUT redirect开始为valid1，ROB1/77、level1、非VLS |
| 411340.999ns | 下一posedge前1ps，redirect本拍有效，完整payload已知 |
| 411351ns | DUT LQ/SQ cancel从42/56更新为2/6，之后保持 |
| 全抽样窗口 | DUT cancel输出与ctrl interface镜像完全一致 |

这削弱“没有实际驱动redirect”的候选，但尚未证明本地record已绑定anchor或software count状态，不能仅据波形判定RTL或框架根因。
Turing独立候选分析更偏向C：旧UID1679等待STD使持久deferred FIFO非空，而owner窗口收尾以整个FIFO为空为前置，可能阻止当前record的software_count_finalized。
本次窗口UID1679从411283.5ns起连续29次deq屏障等待，早于新rollback411328.5ns；这个旧等待与当前record的因果关系仍需运行期状态证明。

已准备并独立review通过只读`cancel_depth_observer.sv`：仅在411304–411424ns每5ns读取已有singleton、队列首尾、record与UID1679；
不new、不randomize、不调用变更方法、不写接口或已有状态。通过既有TB_LIST bootstrap追加到独立mode，核心2540生产输入不变。
observer mode=`mother_cancel_depth_551576067_observer_20261030`，结果`mother_cancel_depth_551576067_observer_20261030_results`，fresh compile启动。
此项是必要只读取证，不计修复或正式方案；缺runtime证据前保持PAUSED_ANALYSIS，不正式登记Buglist。

## 6. 最终责任分类与修复边界

第3–5节保留取证过程；现在observer已fresh compile并在相同411428.500ns复现，controller退出、unknown0。
原批次与observer的469443条生产关键事件逐字节hash一致，SHA256为c5e8a4fcc4f483b0748a0b049624c8ce709f6dc064d991d78cf257aab3ae230d；
observer只增加打印，未改变该seed已观察的事件轨迹。runtime_acceptance.json及observer_event_parity_acceptance.json位于observer结果目录。

运行态明确：record1658在sample82229绑定anchor，82230完成scan，软件取消2/6；compare82232、窗口到期82233。
旧deferred队首来自411281ns的SQ deq，关联UID1679/epoch83/ROB0/146/SQ0/10，仍等待STD；队列共两项。
到期后全局deferred非空使`service_redirect_deleted_owner_window()`直接return，record的finalized/applied/observed始终0。
reconcile保留精确目标82232的2/6快照，之后深度逐拍1→14，下一拍15 fatal；raw snapshot/anchor及local anchor均为空。
因此不是本次单轮搬运瞬时峰值，也不是缺redirect驱动或anchor。

Turing于2026-10-30独立执行三份机器断言并复核源码，正式`RESPONSIBILITY_REVIEW_PASS`：测试框架收尾依赖过宽。
结论不证明UID1679的STD等待本身正确，不泛称所有RTL无Bug；只确认本次取消记录被不相关全局FIFO状态阻塞的直接因果。
`WAVE_FACTS_REVIEW_PASS`与`COVERAGE_PATH_READINESS_PASS`另计，不当作本Bug修复验收。

下一步必须形成并独立review最小方案，精确区分仍可能影响当前record计数的迟到deq与不相关阻塞raw；不能直接删除全局保护。
必须保留完整record/epoch/anchor、窗口时序、相关deq归属/计数扣减、联合预检原子性、FIFO事实及exactly-once释放，
保留STA/STD真实WB屏障、精确target与取消计数比较，不提高14、不丢snapshot、不用DUT数反填软件。
已正式登记W44测试框架章节。上述为责任确认时的历史状态；最新修复进展见第7节，母任务未完成。

## 7. 修复执行进展（2026-10-30）

正式方案`AI_DOC/plan/test_framework/plan/do/memblock_cancel_window_deferred_dependency_formal_plan_20261030.md`已经Boole独立review PASS并由主agent复核，coding/文档/动态验证完成后归档do。
生产三处顺序修改：LSQ handler只读顺推FIFO覆盖位置，公共窗口收尾接收关联依赖，service在原事件消费顺序接入；不改真实FIFO/head/free、STD屏障或严格取消对账。
相关或不能证明身份的出队仍阻塞；零deq事实不遮住后续依赖；无关旧store不再阻止当前取消数冻结。

Bug专项软件契约在`mother_cancel_dependency_protocol_20261030_results` fresh compile通过四组合，六项ERROR/FATAL全零，371ns结束，controller rc0/unknown0。
这验证FIFO无副作用、相关扣减、剩余取消、完整raw原子性及旧store后续释放，不代表原seed或母回归通过。
原551576067完整10000笔coverage fresh compile及原参严格验收已PASS，结果`mother_cancel_dependency_original551576067_20261030_results/strict_acceptance.json`，2524588.5ns finish、六项错误0、controller rc0/unknown0；Boole最终独立实现review PASS，随本记录单Bug提交。
补充负例在独立fixture进行，r2因表数量变化触发既有RM合同拒绝，修fixture保持6条后r3通过四组合及18只读探针，391ns结束、六项错误0、unknown0；Boole独立`SOFTWARE_CONTRACT_REVIEW_PASS`。
8个真实Sv39/CSR/PBMT/SFENCE/cancel邻接及前两原10000笔根1705742557/1992896882全部严格PASS；基础virtual与MAIN1 real smoke亦通过。统一动态门禁12真实DUT＋1空virtual＋四软件组合＋18只读负例PASS，全部controller退出、unknown0；Boole最终独立FINAL_IMPLEMENTATION_REVIEW_PASS，随本记录单Bug提交后恢复非RTL批量。

## 8. 提交后最终验收（2026-10-31）

单Bug提交`50e80d275eb17750df5fb69f021d2ddfd7fd6e26`后重新验证同一2540项核心输入：矩阵25组250/250及fault3压力100/100全部通过，350条包含310真实DUT＋40既有软件别名，不混入本Bug软件fixture或额外专项。fault3每项MAIN10000、10000次RM比较、六项错误零、正常结束；所有controller/collector已退出、unknown0。
Boole最终独立回归和覆盖率review均PASS；证据为母sim下`mother_post_cancel_fault3_final99_20261030_results/final_350_independent_review.json`及`mother_final_cancel_nonrtl_pass_coverage_20261030/independent_coverage_review.json`。
最终覆盖率数据库位于后者目录`merged.vdb`，聚合测试`accepted350`，DUT line80.88%、condition60.54%、toggle38.56%、FSM9.24%；fcov/branch关闭。这里只关闭本Bug及非strict-NC子任务，不把独立strict-NC10个未通过名额算为PASS。
文档结果更新在回归、collector及独立review结束后进行；原七份门禁绑定文档已与50e80d275核对，保存在覆盖率目录`frozen_documents_50e80d275/`。原失败日志、波形和Bug责任事实不改写。
