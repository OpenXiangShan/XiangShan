# Redirect取消窗口延迟出队依赖修复方案（2026-10-30）

状态：2026-10-30 Boole（`01a18b81-6d04-7c10-98f0-7396395ccb47`）独立`PLAN_REVIEW_PASS`，coding/文档/基础及专项验证完成，主agent复核无blocker，归档do。最终独立实现review凭据与单Bug提交门禁另行绑定，不把归档当作母任务完成。母任务已授权测试框架修复，禁止RTL修改、新建worktree及并行源码修改。

## 1. 定位、专有名词与抽象功能说明

本方案修复真实dispatch事件消费的生命周期依赖，不实现新的DUT checker或覆盖率模型。

| 术语 | 当前中文含义及代码落点 | 场景示例 |
|---|---|---|
| redirect / cancel record | 重定向及对应取消记录，`cancel_record_q` | record1658需完成软件取消数后才能对账 |
| owner / deleted owner | LSQ位置对应的动态实例及redirect删除前保存的实例，`redirect_deleted_*_owner_by_key` | SQ0/10为保留的旧store，与年轻victim不同 |
| epoch | 动态实例代号或重定向代号，`old_dynamic_epoch/redirect_epoch` | 同一UID重新执行不能认作旧实例 |
| key / head / tail | 含回绕位的位置、软件出队位置、分配位置 | SQ63推进后需翻转flag，不能只比较value |
| raw / deferred FIFO | monitor完整事实及等待原子应用的持久先进先出队列，`deferred_raw_ctrl_q` | 旧store等STD写回时整条raw保留 |
| count-only / ptr-next | 仅数量的SQ接口、携带出队后位置的LQ接口 | V2 SQ必须由软件head按FIFO数量顺推 |
| preflight / commit | 无副作用联合预检、预检成功后的正式提交 | 同raw任一侧阻塞，两侧均不释放 |
| sample / anchor / snapshot | DUT采样序号、真实redirect锚点、取消计数采样 | 原例目标82232，窗口82233到期 |
| finalize / reconcile | 冻结软件取消数、与精确目标采样严格对账 | 冻结不能再由后到deq修改软件数 |
| STA / STD / real-WB | store地址、数据及真实写回 | 地址故障不代替数据真实写回 |

`lsq_commit_handler::deferred_ctrl_blocks_deleted_owner_window()`负责在已有出队资源模型上判断待处理事实是否仍影响当前取消记录，只返回依赖结论。
`common_data_transaction::service_redirect_deleted_owner_window()`负责窗口到期和记录生命周期转换，接收依赖结论，不访问LSQ模型。
`memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once()`继续负责同拍消费顺序，将两者接起来。

## 2. 根因、范围和不变量

原入口：`basicTest / memblock_dispatch_real_smoke_vseq / tc_l2tlb_fault_2pct_no_trigger`，seed551576067，MAIN10000，timeout100000000ns，无临时plusarg。
责任review已PASS，证据见`AI_DOC/analysis/framework_design/memblock_cancel_snapshot_depth_seed551576067_20261030.md`及W44原章节。
旧全局FIFO非空条件让无关旧SQ事件阻塞record1658冻结，随后精确目标快照逐拍积压到15。

仅改三个生产文件：`seq/base_seq_help/lsq_commit_handler.sv`、`common_data_transaction.sv`及`seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv`（均相对`mem_ut/ver/ut/memblock`）。
专项复用现有`soft_test_memblock_pending_mmio_directed_sequence.sv`添加本Bug场景；不修其它soft_test，不增加cfg、参数、filelist或状态所有者。

保留窗口anchor+4采样时限、完整record/epoch/owner身份、14上限、精确目标及严格计数比较；保留完整raw FIFO与LQ/SQ联合原子提交、STA/STD真实WB屏障及资源只释放一次。
软件取消仍仅回退分配tail并恢复free count，不推进deq head。不参考DUT取消数判断依赖。

## 3. 主流程

抽象功能描述：`service_monitor_once()`在本拍写回/出队事实消费后，判断当前取消窗口是否可以收尾；不更改任何事件驱动时序。

```systemverilog
// 原有timing采集、semantic事件消费与完整deferred raw尝试提交保持原顺序。
data.service_redirect_deleted_owner_window(
    memblock_sync_pkg::peek_current_dut_global_sample(),
    monitor_commit_handler.deferred_ctrl_blocks_deleted_owner_window());
// 后续store fault finalize、exception处理、第二次sideband采集、一次reconcile均保持原位。
```

中文伪代码：在已完成本轮deq尝试后，调用已绑定的唯一commit handler取得依赖结论；将当前采样号与结论交给公共表的窗口收尾函数；后续仍由既有enqueue sequence应用取消数，既有对账函数消费精确快照。查询不替代正式出队预检，不驱动接口。

## 4. 只读依赖查询

抽象功能描述：`deferred_ctrl_blocks_deleted_owner_window()`使用当前LSQ出队位置和未处理FIFO，判断其数量所覆盖的物理位置是否与当前保存的deleted owner相交。相关或无法证明安全返回1，确定无关返回0；不提交、弹队、更新状态或调用带副作用的正式preflight。

输入：公共owner窗口及映射、FIFO完整raw、已绑定`lsq_ctrl`的head/free count；输出仅一个bit。

```text
若无活动窗口或FIFO为空：返回0。
按当前窗口record id查找取消记录；记录缺失或不合法时保守返回1，保留收尾函数原fatal。
若记录已finalized，或当前DUT sample尚未到期：返回0，由收尾函数保持原生命周期屏障。
读取现有LQ/SQ head及allocated=size-free_count到局部变量。
逐条读取FIFO，不重排：
  若raw无效，指针能力不符、count非法或累计count超过当前allocated：返回1。
  非零LQ：用raw的ptr-next回退count得到start；必须等于局部LQ head。
  非零SQ：V2使用局部SQ head；有指针版本从ptr-next回退count并与局部head一致。
  对该raw两侧覆盖位置逐项查询：
    若命中当前窗口的deleted owner：返回1。
    若没有唯一live owner，或live status不再active/mapped/完整key匹配：返回1。
  用该raw count推进两个局部head，同时扣除局部allocated，继续下一raw。
遍历完未相交：返回0。
```

中文伪代码：本函数只证明延迟事实与取消条目无关。先在无窗口或空FIFO时快速退出；其余按FIFO顺序重建尚未消费的连续出队区间。复用`lq_deq_start_key/sq_deq_start_key`解释出队后指针，复用`advance_lq_key/advance_sq_key`处理flag翻转。查询`lookup_redirect_deleted_*_owner`只认可当前窗口record/epoch；查询`lookup_active_uid_by_*`与status确保剩余位置有当前活实例。两者同时命中、均未命中或status不匹配均不能证明无关。任一raw、任一侧涉及deleted owner，整个依赖保留，禁止只放行LQ或SQ的一半。

该查询不调用`store_fault_blocks_sq_deq()`作为放行依据：旧store即使等待STD仍是无关的live owner，真正出队继续由既有preflight屏障阻塞。不写UID/epoch/map或镜像pointer。
无需从`cycle=$time`换算sample：位置与当前record身份确定是否依赖；原有采样窗口只在到期后允许finalize。后续窗口外异常出队仍经过原严格preflight，不能借此消费或放宽。

性能：仅到期且尚未finalized的活动窗口、FIFO非空时扫描；没有窗口、窗口未到期或已冻结时均不扫描。ctrl monitor在MMIO、rollback或flushsb等待时也能产生零deq raw；这些raw不推进局部head，不能假设FIFO条数等于entry数。一次查询复杂度为O(FIFO条数+当前LQ/SQ已分配数量)，累计entry超过容量时立即保守阻塞。扫描对象仅持久FIFO而非历史主表，原问题FIFO为2条；保留零deq事实，禁止为性能丢弃。若未来出现长FIFO性能问题，另行按事实消费水位评估，不增加本Bug无关缓存、第二所有者或人工截断阈值。

## 5. 窗口收尾

抽象功能描述：`service_redirect_deleted_owner_window(current_sample, deferred_deq_blocks)`在原采样边界满足且没有相关未处理出队事实时冻结软件数，资源实际回退完成后清理保存的owner。

```systemverilog
if (!record.software_count_finalized && current_sample >= expire_sample) begin
    if (deferred_deq_blocks) return;
    record.software_count_finalized = 1'b1;
    check_cancel_pending_aggregate();
end
// 原software_applied屏障、map清理及record cleanup保持不变。
```

中文伪代码：无活动窗口直接返回；按record id查找并保留原基础合法性fatal；窗口未到期不冻结；到期时仅由关联依赖阻塞冻结，随后仍检查软件pending aggregate守恒；只有软件取消已应用才能清理owner。`check_cancel_pending_aggregate()`核对各记录未应用计数之和，不修正计数；`cleanup_completed_cancel_records()`只删除已完成且不再被窗口或fault资源引用的记录。

## 6. 验证与验收

1. 独立方案review PASS后才coding。`git diff --check`、静态调用和无关文件范围检查。
2. 专项契约：空FIFO、无窗口、无关旧SQ等待STD、相关LQ、相关SQ、同raw混合两侧、相关raw位于后续FIFO项、flag回绕、错指针/超容量/缺owner保守阻塞、记录身份错误拒绝；查询前后FIFO/head/free/map/count不变。
3. 真实owner窗口用生产admission/reservation/redirect API建立；原seed场景不通过直接写window/map伪造。专项检查窗口到期前不冻结、无关延迟raw下到期可冻结，相关raw不冻结、正式联合preflight失败不部分释放，真正STD完成后才允许旧store出队。
   相关晚deq解除阻塞后通过生产full-raw提交，断言record与pending cancel同量扣减、对应deleted owner只消费一次；再次service不重复扣减或释放。
   冻结后由生产`memblock_lsqenq_dispatch_base_sequence::apply_pending_lsq_cancels()`仅对剩余取消量回退tail并恢复free，断言cancel不推进head；随后无关旧store完成真实WB并出队，容量合法且无重复释放。
   在无关队首与相关后续raw之间插入零deq raw，仍须发现后续依赖并阻塞冻结，零deq事实和所有head/free/count均不因查询变化。
4. fresh compile新mode，首先原551576067原cfg10000笔和原timeout零覆盖参数运行；严格一次TEST_PASS、finish、最终/捕获/降级六项ERROR/FATAL零、rc0、unknown0。debug或缩短用例不能代替原参通过。
5. 原1705742557、1992896882完整10000笔及本Bug邻接seed至少8个；基础`virtual_base_sequence/default`与真实smoke/`tc_dispatch_real_smoke`。软件契约独立计数，不算真实DUT通过。
6. 最终独立review；同步Buglist、analysis、母状态、总review及必要flow说明，plan移do，单Bug本地commit。原错误及controller归档保留。
7. 修复通过并提交后才能恢复非strict-NC批量；源码已变化，最终350须重新绑定统一源码和编译输入，旧250矩阵只作历史通过。最新19ABORTED原seed保留，58NOT_RUN重新取不重复seed；不直接盲跑旧recovery98脚本。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

`IMPLEMENTATION_DELTA`：专项测试需要直接调用生产enqueue的取消应用函数，但现有package先include软件专项、后include enqueue类。在原专项文件增加同package的`typedef class memblock_lsqenq_dispatch_base_sequence`前置声明，仅解决类型依赖，不调整filelist/include顺序、不新增运行期所有者。独立fixture vseq只调用本Bug四种参数组合，不执行原软件用例簇。该细节须纳入最终实现review。

`IMPLEMENTATION_DELTA`：最终静态review要求补齐缺owner、错误record/epoch、live位置/映射错误、跨raw累计容量及独立快速退出负例。为保持正在运行的原seed编译输入冻结，新增测试只放ignored独立fixture的派生软件sequence，不再修改生产2540项。每次探针比较FIFO、head/tail/free、record/pending、临时及live map与相关status前后状态，不改变公共API或真实flow。

## 与初步 plan 差异说明

初步方向仅为“依赖当前记录相关deq”；本正式方案将其落实为由已有LSQ owner使用局部head推算完整FIFO覆盖区间，不新增common-data到LSQ反向依赖。原全局非空判断替换为只读关联结论，其余取消生命周期不变。
本文件首次形成正式方案，不覆盖早期分析证据。独立review要求补齐三项资源闭环断言，已在第6节第3项落实并再次review PASS；性能部分明确零deq事实及实际复杂度。任何执行中修改必须以`IMPLEMENTATION_DELTA`明确记录。
执行结果（更新2026-10-31）：coding、文档、12真实DUT及1空virtual、四软件组合与18只读负例均完成；Boole最终独立FINAL_IMPLEMENTATION_REVIEW_PASS，已单Bug提交`50e80d275`。提交后矩阵250及fault3压力100已同源码重验350/350，最终覆盖率已精确合并且独立review PASS，不沿用旧版本PASS。
最终证据为母sim下`mother_post_cancel_fault3_final99_20261030_results/final_350_independent_review.json`及`mother_final_cancel_nonrtl_pass_coverage_20261030/independent_coverage_review.json`，实际数据库为后者目录`merged.vdb`、聚合测试`accepted350`。310条真实DUT与40条既有软件别名单列，专项不重复计数。
本plan所涉Bug及非strict-NC回归子任务已完成；母任务独立strict-NC10名额仍STOPPED_RTL_CONFIRMED，未修改RTL、不计PASS。结果更新发生于collector及独立review完成后；原七份冻结文档与提交核对并另存于coverage目录`frozen_documents_50e80d275/`，旧门禁只保留历史时点证明，不重写哈希。
