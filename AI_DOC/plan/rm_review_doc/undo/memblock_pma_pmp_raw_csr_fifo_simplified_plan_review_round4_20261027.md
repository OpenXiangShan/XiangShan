# V2 PMA/PMP raw CSR FIFO 简化方案第四轮独立评审

日期：2026-10-27

评审对象：`AI_DOC/plan/rm_plan/undo/memblock_pma_pmp_raw_csr_fifo_single_consumer_fix_plan_20261027.md`

前轮评审：

- `AI_DOC/plan/rm_review_doc/undo/memblock_pma_pmp_raw_csr_fifo_single_consumer_fix_plan_review_round2_20261027.md`
- `AI_DOC/plan/rm_review_doc/undo/memblock_pma_pmp_raw_csr_fifo_simplified_plan_review_round3_20261027.md`

评审基线 SHA256：`437a6173d652f8c53f51623aa8d16ca84321c0be9e276def4d3dfb8432b48f41`

评审结论：`REVIEW_PASS`

本轮是全新独立只读 plan review。除创建本文外，未修改目标 plan、已有 review、实现源码、测试源码、脚本或其他已有文件；未执行编译、仿真或回归。本结论只表示当前 plan 已具备无歧义 coding 条件，不评价当前半成品实现是否通过测试。

## 1. 评审范围与 review 链

本轮以第三轮简化方案 review 明确列出的 B1、B2、B3、B4、caller 白名单和混合 worktree cached-tree 独立提交门禁为强制关闭项。`memblock_pma_pmp_raw_csr_fifo_single_consumer_fix_plan_review_round3_20261027.md` 属于此前另一套含 residency composite 的历史方案链，其 blocker 不适用于当前已明确删除 prospective generation helper 和 residency/replay composite transaction 的简化方案；本轮没有把该旧方案合同重新带回目标 plan。

只读源码核对范围包括：

- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_pma_pmp_model.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_control_barrier_service.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_csr_initial_config_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dynamic_csr_change_sequence.sv`
- `mem_ut/ver/ut/memblock/scripts/checks/check_pma_pmp_csr_fifo.py`

源码仅用于确认 plan 的落点、现有 lifecycle/caller 结构和静态门禁可实现，不用于判定当前实现或测试状态。

## 2. Round3 blocker 关闭结论

### 2.1 B1：startup lifecycle、epoch 0 与负例

结论：已关闭。

目标 plan 已固定唯一顺序：testcase `end_of_elaboration_phase` 先调用 `initialize_l2tlb_testcase_lifecycle()`；之后既有 runtime owner 调用 `reset_all_tables()`，在 reset inactive 状态建立合法 epoch 0 replay baseline；write-plan preflight 再依次执行统一 epoch helper 和 checked-count helper；全部成功后才能 drive 第一笔 write。producer、prepare、commit 和 completion 共用 `pma_pmp_csr_epoch_is_current()`，没有第二套 `epoch==0` 特判，也没有新增第二个 reset owner。

未初始化 lifecycle 与 epoch 0 的负例也已写成可执行门禁：使用纯函数 `pma_pmp_csr_epoch_state_is_current()`验证非法组合，在 drive/stage 前失败，并检查 producer count、FIFO、model/history、candidate 和 expected 均不变。software directed 明确复用 testcase 已建立的 lifecycle，不得直接改 lifecycle flag、current epoch 或 reset-active。第一次 runtime reset 从 0 进入 1，后续 epoch 非零单调且旧 epoch 0 状态不得跨代。

### 2.2 B2：64 位转换、非法哨兵和全部 overflow writer

结论：已关闭。

目标 plan 已把 expected、producer/applied count、raw/service/model revision、PMP/PMA generation 和 capture/effective sample 固定为 `longint unsigned` 64 位语义。所有 queue/dynamic-array `size()` 必须先无损转换为命名的 `longint unsigned delta64`，再参与 `MAX_U64-delta64`、加法和比较，禁止依赖 `int`、unsized literal 或隐式上下文转换。

`MAX_U64` 已被固定为 raw/service/model revision 与 generation 的永久非法哨兵。plan 逐项列出 revision writer：raw push、raw prefix commit、raw reset；service commit、runtime reset；model commit、model reset。所有单步 writer 仅允许 current `<=MAX_U64-2` 时发生 mutation，live 结果最大为 `MAX_U64-1`；current 为 `MAX_U64-1` 时，下一次 mutation 必须在第一处 live write 前失败。generation 在 prepare private post-state 中按每个 accepted write 检查，批内任一 overflow 均使整个 live batch 零 mutation。

count 与 sample 边界同样完整：expected overflow 在 drive/stage 前 fatal；producer overflow 在入队前 fatal；applied batch、revision、model/generation overflow 在 prepare/precheck 阶段失败并由持续 service 升级为 fatal；capture 必须 `<=MAX_U64-2`，effective sample 只允许由已验证的 C+2 产生。directed 矩阵逐项覆盖 expected add、producer push、applied batch、raw/service/model revision、PMP/PMA generation、capture=`MAX_U64-2` 成熟和 capture=`MAX_U64-1` 拒绝，并要求 FIFO、table、history、counter、revision、candidate 均无半更新。

### 2.3 B3：outcome enum、地址分类、mixed-lock 与结果矩阵

结论：已关闭。

目标 plan 已定义稳定可观测的 `pma_pmp_csr_replay_outcome_e`：`ACCEPTED_CHANGED`、`ACCEPTED_SAME_VALUE`、`IGNORED_LOCKED`、`IGNORED_UNSUPPORTED`、`IGNORED_UNKNOWN`。audit row 保存根因 outcome；`ignored_noop` 仅作为后三类 ignored outcome 的日志汇总名，不再承担根因分类。

地址 classifier 已固定实现地址和 family 边界：CFG 实现地址为 `{3a0,3a2,3a4,3a6}` 与 `{7c0,7c2,7c4,7c6}`；ADDR 实现地址为 `3b0..3cf` 与 `7c8..7e7`；CFG family hole 以及 PMPADDR architecture family 中 `3d0..3ef` 归 `IGNORED_UNSUPPORTED`；family 外归 `IGNORED_UNKNOWN`。实现地址再依据 lock/TOR-lock 和 accepted slot 的 pre/post 值区分 locked、changed 与 same-value。

mixed-lock 合同唯一：至少一个 unlocked slot accepted 时，整笔 CSR write 只建立一个对应 generation/history；locked slot 保持不变，unlocked slot 按写值更新；所有 accepted slot 最终同值仍为 `ACCEPTED_SAME_VALUE`。plan 已给出逐场景 directed 结果矩阵，分别规定 ADDR/CFG same-value、mixed-lock changed、mixed-lock same-value、ignored locked/unknown/unsupported 的 table、generation/history、produced/applied、FIFO、completion 和 outcome 结果，验收不再依赖测试自行解释。

### 2.4 B4：空成熟 prefix normal no-work

结论：已关闭。

持续 service 的控制流已固定为：reset 或 FIFO 空返回；published 为 0 返回；prepare 失败 fatal；prepare 成功但 `ticket.events.size()==0` 时作为正常 no-work 返回；只有非空 ticket 才进入 commit，commit 失败再 fatal。空 ticket 必须为 non-null，冻结 reset/service/raw revision，`model_ticket=null`，且不得交给 commit。

C/C+1 directed 明确覆盖 FIFO 非空但 prefix 未成熟的正常等待状态，要求无 fatal、所有 revision 和共享状态不变；C+2 只提交一次。因此 prepare 允许空 prefix 与 commit 拒绝空 ticket 不再构成控制流矛盾。

## 3. 其余强制门禁关闭结论

### 3.1 唯一 owner 与 caller 白名单

结论：已关闭。

目标 plan 把 `commit_pma_pmp_csr_replay()`固定为唯一生产 destructive owner。raw producer 的生产路径仅允许 CSR monitor，software directed 是显式测试例外；production service caller 仅允许 `collect_runtime_context_events()`、startup wait 和 control/dynamic completion；raw prefix commit 与 model batch commit 的生产 caller 仅允许统一 commit owner。`apply_csr_write()`只保留 model 兼容 wrapper 和已列 software provenance 测试用途，不获得 raw FIFO ownership。

issue residency 与 `end_test_check()`明确禁止调用 service/prepare/commit/pop 或裸 raw queue mutation。checker 要同时扫描 function、task、全文件 API 引用和裸 queue mutation，不以单一字符串或局部函数体代替完整 caller 审计。该合同与当前 package、common-data、startup、control barrier 和逐拍 collection 的代码落点一致，实施者无需在 coding 时重新选择 owner 或 caller 集合。

### 3.2 Barrier、model-off 与 end audit

结论：已关闭。

startup 和 dynamic 均使用发送前 producer count 加计划写数形成的 64 位 expected 门槛。release 必须同时满足同 epoch、expected 达成、produced/applied 守恒、FIFO 空和 profile/full-runtime snapshot 匹配；无 PMA/PMP 改动也不得绕过已有 pending FIFO。dynamic candidate 在 checked-add 成功后才能 stage，barrier 释放前由持续 service 推进成熟写入。

model enable 只控制 RM 是否使用 PMA/PMP 权限结果，不关闭 producer、consumer、generation/history 维护。`end_test_check()`保持只读审计：只报告 FIFO 残留或 produced/applied 不守恒，不补消费、不清 FIFO。上述三项不存在第二条完成或修复路径。

### 3.3 cached-tree 与独立 commit 门禁

结论：已关闭。

目标 plan 已恢复混合 worktree 的完整隔离要求：新增文件使用显式白名单；共享大文件仅逐 hunk stage，禁止整文件 `git add`；commit 前必须审计 `git diff --cached --name-only` 和完整 cached diff，排除 stale-TLB、control、issue-fire、ROB、provenance 和无关文档 hunk；checker、`git diff --check` 及目标编译/仿真必须对应 cached tree，而不是混合未暂存工作树；implementation review 记录 cached diff SHA256；最终仅创建本问题独立本地 commit，不 push，且不 stage sim/coverage 产物。

该门禁已经能够唯一判断提交内容、验证对象和 implementation review 基线，不再把混合工作树中的半成品状态当作本问题完成证据。

## 4. 原子性、时序和性能复核

prepare 只扫描成熟 pending prefix 与第一个 future head，不扫描 UID 主表或完整 history；空 FIFO 为 O(1)。prepare 在 private state 构造 C+2 effective events 和 model post-state；commit 前复核 epoch、service/raw/model revision、完整 FIFO prefix 及所有容量；全部通过后才进入无等待、无分配、无可失败分支的 mutation 区。重复、stale 或失败 ticket 均要求零共享副作用。

capture C、effective C+2 和 published W 三个时间轴保持分离：raw/audit 保存 C，model history 保存 C+2，W 只证明 producer-complete 成熟边界。issue residency 只读已提交 history，不再承担 FIFO 消费或 prospective generation 推导。以上结构没有新增每拍全表扫描、第二 owner 或 request 触发式消费依赖。

## 5. 最终结论

`REVIEW_PASS`

Round3 B1、B2、B3、B4，以及 caller 白名单、唯一 owner、startup/dynamic barrier、model-off、end audit 和 cached-tree 独立 commit 门禁均已关闭。当前 plan 没有剩余可 coding blocker，可以进入后续 coding 流程。

本结论不表示当前半成品实现、compile、simulation 或 regression 已通过；这些结果必须由后续执行和 implementation review 单独证明。
