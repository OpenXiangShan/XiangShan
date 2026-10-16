# V2 Stranded SQ Cancel Owner Coding Plan 独立评审

| 项目 | 内容 |
| --- | --- |
| 评审对象 | `AI_DOC/plan/test_framework/plan/undo/memblock_v2_stranded_sq_cancel_owner_coding_plan_20261012.md` |
| 关联源码 | `common_data_transaction.sv`、`memblock_lsqenq_dispatch_base_sequence.sv` |
| 真实证据 | seed `676087581`，cancel record `3896` |
| 评审日期 | `2026-10-12` |
| 首轮结论 | **存在 3 个语义 blocker，当前不可 coding。** |
| 第二轮结论 | **3 个 blocker 已全部关闭，正式 plan 可coding。** |

## 术语与抽象功能说明

| 术语 | 当前中文含义 | 代码落点 | 示例 |
| --- | --- | --- | --- |
| `admission prefix` | 测试框架按 UID 连续记录的 LSQ enqueue 上界。 | `dispatch_progress.max_enqueued_uid` | redirect 可把该上界回退到最老被 flush UID 之前。 |
| `stranded SQ owner` | 仍在 `uid_by_sq` 中且已对 DUT 可见，但当前 UID 连续扫描范围无法覆盖的 SQ owner。 | `uid_by_sq`、`status_by_uid` | seed 中的 UID 4116。 |
| `LSQ cutoff` | 本次 redirect 在 LSQ 生效的采样边界。 | `redirect_lsq_sample_seq` | reservation sample 晚于该边界时不能计入本次 cancel。 |
| `fault attribution` | 区分普通 cancel owner 与 fault head SQ candidate 的计数职责。 | `note_lsq_cancel_for_uid()`、`resolve_fault_sq_candidate_from_snapshot()` | record 3896 为普通 8、fault 1、遗漏 owner 1。 |

抽象功能说明：本评审检查 plan 是否准确描述 stranded owner 的形成时序，以及补扫是否只处理本次
redirect 确实覆盖的普通 SQ owner；不修改测试框架源码。

## 已确认事实

真实日志证明 record 3896 的 DUT SQ cancel 为 10，软件普通计数为 8，另有一个 fault SQ candidate。
UID 4116 保持 `active=1`、`active_sq_mapped=1`、reservation 为 `DUT_VISIBLE`，完整 ROB key 为 `0/76`，
完整 SQ key 为 `1/35`，且被 record 3896 的 redirect 覆盖，因此它是缺少的一个普通 SQ owner。

plan 选择在 redirect 中频事件遍历有限 `uid_by_sq`、使用完整 ROB/SQ key、`dynamic_epoch` 和 LSQ cutoff
进行证明，这个总体修复方向正确；没有引入每拍全表扫描，也没有放宽 cancel reconcile。

## 首轮必须修改项（历史记录）

### 1. 根因时序与 admission prefix 的描述错误

当前 plan 把根因描述为 redirect 使 `complete_admission()` 被跳过，因此 `max_enqueued_uid` 未推进到
UID 4116。源码实际不是这样：`lsq_ctrl_model::commit_allocate()` 先调用
`set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1)`，该调用立即通过 `mark_uid_enqueued()` 推进
`max_enqueued_uid`；`complete_admission()` 只负责开放 issue route，不推进该前缀。

真实链路应改为：UID 4116 已完成 allocate/enq 并成为 `DUT_VISIBLE`；前一轮 redirect 3895 扫描并回退
连续 UID 前缀，但 UID 4116 的 ROB `0/76` 不被 redirect ROB `1/75` 覆盖，因此 owner 保持 active，随后落在
回退后的 prefix 外；下一轮 redirect 3896 的 ROB `1/76` 覆盖该 owner，但原 prefix scan 已无法访问它。

plan 必须据此重写问题描述、目标行为和测试构造。不能用“手工把 max_enqueued_uid 停在前一 UID”代替真实的
“前一轮 redirect 回退 prefix，同时保留未被覆盖的 DUT-visible owner”时序，否则定向测试不能证明修复了
真实根因。

### 2. prefix 外 owner 的普通/fault 分类没有闭合

plan 对 prefix 外快照匹配成功后直接调用 `prepare_uid_for_redirect_reissue()`，但没有明确保证该候选只能是
当前 V2 scalar SQ allocation 路径产生的普通 owner。仅依赖“fault head 应位于原 prefix”不足以保护未来状态
异常或 owner 生命周期漂移。

修订版必须明确：补扫候选需要保持 `active && enq && active_sq_mapped`、完整 ROB/SQ key 与
`dynamic_epoch` 一致、reservation 在 cutoff 前已经 `DUT_VISIBLE`、`numLsElem==1`，并且不是 control、
不是 committed/terminal fault owner。若 prefix 外候选带有 fault/exception/ROB commit 语义，应 fatal 并输出
归因，不能按普通 owner 计数。当前 V2 LSQ enqueue 还强制 LQ/SQ 二选一，因此 plan 中“LQ/SQ 双资源 UID”
不是本路径的合法边界，不应据此扩大补扫语义。

此外，prefix 外候选若仍 active/mapped，却已经记录当前 `lsq_cancel_accounted_epoch`，不应静默返回 0；该状态
表示重复记账或生命周期不一致，应明确 fatal。正常的 prefix 已处理候选应通过 active/map 已删除来跳过。

### 3. 定向测试入口和场景不满足执行规则

plan 只写“新增 software sequence/test”，并使用 `tc=<新增soft testcase> ts=virtual_base_sequence`，既未给出
具体文件、类、package 接入和主流程伪代码，也不符合新场景优先使用 `basicTest + 专项 vseq` 的规则。

修订版必须指定测试落点，并以真实两轮 redirect 场景覆盖：第一轮回退 prefix 但保留一个未覆盖的
`DUT_VISIBLE` SQ owner，第二轮覆盖该 owner并验证只补计一次；同时覆盖未覆盖 owner保持 active、完整 key
wrap、防重复 epoch、fault candidate 独立归因和无 stranded owner 的旧行为。若坚持新增 testcase，必须说明
为何专项 vseq 无法表达该场景。

## 其他检查结论

- 性能方案可接受：redirect 中频路径最多扫描 `MEMBLOCK_SQ_SIZE` 个 active SQ owner。
- 完整 ROB/SQ key、`dynamic_epoch` 和 LSQ cutoff 是必要防护，应保留。
- 必须清除现有 UID/record seed 特化诊断，且不得把 `sim/.eda_remote/*.pid` 纳入修改。
- 现有前序诊断源码修改应保留到 coding 阶段按 plan 统一整理，本次评审未修改源码。

## 首轮结论（历史记录）

**当前 plan 不可 coding。** 完成以上三项修改后需要再次独立 review；只有根因时序、普通/fault owner 边界和
真实定向测试入口全部闭合，才能给出“可 coding”结论。

## 第二轮独立评审

第二轮聚焦首轮三个 blocker 是否关闭。修订后的正式 plan 已逐项补齐真实根因、owner 分类与 fail-fast
边界，以及 `basicTest + 专项 vseq` 的具体接入和测试伪代码。

| 首轮 blocker | 第二轮核查结果 | 状态 |
| --- | --- | --- |
| 真实 3895 -> 3896 两轮 redirect 根因 | plan 已明确 `commit_allocate()` 通过 ENQ setter 推进 `max_enqueued_uid`，不再错误归因于 `complete_admission()`；第一轮 record 3895 回退 prefix，但因 ROB `1/75` 不覆盖 UID 4116 的 ROB `0/76` 而保留该 `DUT_VISIBLE` owner；第二轮 record 3896 的 ROB `1/76` 覆盖 UID 4116，而原 prefix scan 已无法到达它。定向测试禁止直接改 `max_enqueued_uid`，改为通过两次正式 redirect scan 自然复现。 | **已关闭** |
| ordinary/fault/fail-fast 边界 | 补扫只接受 `active && enq && active_sq_mapped` 的 scalar SQ owner，并要求 `numLsElem==1`、完整 ROB/SQ key、`dynamic_epoch`、`DUT_VISIBLE` 与 cutoff 证明一致。prefix 外 control、commit、terminal、fault、exception owner，以及当前 epoch 已 accounted 但仍 active/mapped 的 owner均 fail-fast；未被 redirect 覆盖或 cutoff 前尚不可见的合法 owner保持 active且不计数。fault candidate继续由既有独立路径归因。 | **已关闭** |
| `basicTest + 专项 vseq` 接入与测试伪代码 | plan 已明确新增 soft sequence、`memblock_stranded_sq_cancel_directed_vseq`、`seq_pkg.sv`、`seq.f`、`tb.f` 的接入点，并复用 `basicTest` 的 `+VSEQ_MAIN` 启动机制。vseq 主流程采用 `require_virtual_sqr()` 与 `uvm_do_on(directed_seq, p_sequencer)`；两轮 redirect、计数、owner 生命周期、完整 key、epoch、防重复、fault 独立归因及旧行为回归均有具体伪代码或用例要求。 | **已关闭** |

### 第二轮补充检查

- 性能边界保持为 redirect 中频路径最多遍历 `MEMBLOCK_SQ_SIZE` 个 active SQ owner，不引入每拍全表扫描。
- 完整 ROB/SQ key、`dynamic_epoch`、reservation sample 和 LSQ cutoff 共同约束动态实例与可见性，未放宽 reconcile。
- ordinary owner补扫与 fault candidate解析职责分离，真实 record 3896 的目标仍是 ordinary `8 -> 9`，fault独立贡献 `1`，合计匹配 DUT SQ cancel `10`。
- plan 要求移除 UID/record/seed 特化诊断，保留通用归因打印；不纳入 `sim/.eda_remote/*.pid`。
- 测试覆盖专项 software-only 场景、真实失败 seed、邻近 seed和 compile，验收条件包含 `UVM_ERROR=0`、`UVM_FATAL=0` 与 `TEST CASE PASSED`。

## 第二轮最终结论

**第二轮无 blocker，正式 plan 可coding。**
