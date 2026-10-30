# fault3新seed的cancel快照深度失败分析

日期：2026-10-30。状态：`PAUSED_ANALYSIS`；批量已联停收敛，原参数带波形fresh compile专项已启动，尚未正式责任分类或修改生产源码。

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
