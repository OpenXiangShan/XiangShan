# V2 向量 Admission 与 LSQ 入队 Flow

本文定义 V2 MemBlock 测试框架把一个主表 `V_LOAD` 或 `V_STORE` macro 激活、拆成 data-uop 并送入
`enqLsq` 的目标 flow。本文是向量支持的设计/plan 文档，不是当前已实现行为：当前 scalar flow 会拒绝 vector
`fuType`。文中的“向量分支”“token promotion”及其 `vector_*()` helper 都是后续 coding 的目标接口；可以在调用图和
伪代码中按普通函数调用表示，不要求当前源码已存在对应函数。

**文档定位：** 本文是 `flow 逻辑文档`，用于后续 coding 和实现级 review。它定义 Admission/LSQ 入队阶段的
真实入口、状态所有权、launch/promotion 时序、helper 边界和异常分支；不能用生命周期摘要代替本文的实现规则。
供人工阅读全链路的独立 `flow 描述文档` 见
[V2 向量测试框架生命周期 Flow 描述文档](vector_lifecycle_flow_description.md)。

权威约束来自 [向量测试框架需求文档](../analysis/framework_design/向量测试框架需求文档.md) 第 5.5、5.6 节和
[向量测试框架生命周期决策记录](../analysis/framework_design/向量测试框架生命周期决策记录_20260826.md)。现有
scalar admission 的真实链路见 [LSQ Admission Flow](lsq_admission_flow.md)。

## 1. 术语与抽象功能说明

| 英文术语 | 当前 flow 中的中文含义 | 代码对象/状态落点 | 示例 |
| --- | --- | --- | --- |
| `admission` | 测试框架把一个主表项变成对 DUT 可见的 LSQ reservation 的过程 | `enqLsq` launch、LSQ software mirror、runtime 状态 | data-uop 发出后不能立刻 issue。 |
| `activation` | 把当前 UID 标记为唯一 active ROB owner，并初始化公共/向量运行状态；此时尚未占用 LQ/SQ entry | `common_data_transaction::activate_uid(uid,0,0)`、`vec_runtime_table[uid]` | 激活后才能为第一个 data-uop 预览 range。 |
| `active map` | 记录当前 ROB/LQ/SQ key 到活动 UID 或向量 uop owner 的索引 | `uid_by_active_rob`、`vec_range_owner_by_lq/sq` | monitor 先查 map，再更新 runtime。 |
| `owner` | 某个物理 ROB/LQ/SQ key 当前归属的 UID 或 `{uid,vuopIdx}` | active/range map | 冲突 owner 使本拍 admission 失败。 |
| `batch` | 同一个 driver sample 边界内一起处理的 candidate、pending sample 或 cancel 记录集合 | admission 临时队列 | 向量初版每拍只包含一个 data-uop 请求。 |
| `epoch` | 动态实例版本，用于过滤 redirect 前后的同 UID 事件；唯一可变真源是公共 `status_by_uid[uid].dynamic_epoch` | 公共 status；token/item/tombstone 只保存不可变快照，runtime 字段仅为镜像 | re-admission 后递增。 |
| `cursor` | admission 当前等待的 UID 或 macro 内下一个 vuopIdx 的单调位置 | `next_enq_vuopIdx`、公共 admission prefix | 资源不足时保持不变。 |
| `macro` | 一个 UID 对应的一条向量宏指令 | `main_table_by_uid[uid].vec_table` | 一条 `LMUL=m4` 指令有多个 data-uop。 |
| `data-uop` | 宏指令中真正访存、需要普通 LQ/SQ range 的 uop | `vec_table.uop[vuopIdx]` | `vuopIdx=0..D-1`。 |
| `F` | 一个 data-uop 预留的 entry 数，等于 `numLsElem` 与 `flowNum` | 静态 layout | strided e32 的 `F=4`。 |
| `range` | 从 LQ/SQ 起始指针开始、长度为 F 的连续物理 entry | vector uop runtime | `sqStart=S, F=2` 占用 `S,S+1`。 |
| `launch` | driver 已在 clocking 边界将 `enqLsq.valid` 写给 DUT | LSQ driver 采样事实 | launch 建立 provisional token；正式 owner 仍等 sample/promotion。 |
| `launch token` | 一次 launch 到下一采样边界之间的暂存分配记录；可见性由 `visible` 标志区分 | `vec_runtime_table[uid].uop[i].launch_token` | `visible=0` 时只占用软件 provisional reservation。 |
| `provisional reservation` | 为防止同一采样窗口重复分配而临时推进的 enqueue pointer 边界和自身 entry delta，尚未写正式 owner map | launch token | redirect 在 sample 前只撤销该 token 自身占用。 |
| `promotion` | sample 确认 DUT 已采样后，将 token 原子提升为正式 range owner 的动作 | `promote_vector_range_allocation()` | 写入全部 F 个 key 和 `lqStart/sqStart`。 |
| `pending sample` | 已 launch 并已在软件预留、但尚未经过下一采样边界确认可 issue 的批次 | 现有 scalar flow 的概念，向量使用同样边界 | C0 launch，C1 才开放 issue。 |
| `redirect LSQ transition` | 一个 redirect 对共享 LQ/SQ pointer、free count 和 aggregate cancel record 的唯一串行处理段 | 公共 redirect service、`redirect_epoch` writer gate | 先稳定尾部 token，再汇总并回退完整 cancel record。 |
| `redirect transition plan` | redirect coordinator 在开始资源事务前建立的临时只读/暂存计划 | `redirect_transition_plan[redirect_epoch]` | 接收 sample 失效 token，保存被覆盖 UID、domain 与待提交清理动作。 |
| `DEFER` | writer gate 暂不可取得或更早 cancel record 尚未完成时的可重试结果 | active redirect/freeze + 原 batch | 保持状态，等待前序服务完成后重试。 |
| `range map` | 每个 LQ/SQ entry 指向 `{uid,vuopIdx}` 的 vector 反向索引 | `vec_range_owner_by_lq/sq` | SQ feedback 只有 `sqIdx` 时反查 data-uop。 |
| `allocation fact` | 只能在运行时确定的真实起始指针和 range | `vec_runtime_table` | 主表生成时不能预知 redirect 后的 pointer。 |
| `canAccept guard` | 在构造本拍请求前，对两侧 LSQ 计数、enqueue width、redirect 延迟更新和待处理 reservation 的统一资源预检 | `lsq_ctrl_model` 软件镜像及 admission pending token | `free_count >= 本拍需求 + width 保留量`，而不是只比较 `free_count >= F`。 |

## 2. Flow 边界和设计状态

现有标量入口是 `memblock_lsqenq_dispatch_base_sequence::send_lsqenq_cycle()`；它用
`lsq_ctrl_model::commit_allocate()` 为 scalar UID 分配一个起始 key。该实现目前只支持一个 UID 对应一个
LQ/SQ key，且 `validate_main_table_entry()` 会拒绝 vector。向量 flow 不能直接调用它完成 range 账本。

向量目标 flow 保留以下公共部分：

```text
main_table_by_uid
uid_by_active_rob
lsq_ctrl_model 的 enqueue/dequeue pointer 与 free count
现有 driver clocking 边界、redirect freeze、cancel_record
redirect_epoch 对应的 LQ/SQ writer gate（仅 redirect transition 存在时有效）
```

向量新增且只服务向量的状态为：

```text
vec_runtime_table[uid]
  .active
  .next_enq_vuopIdx
  .enq_done
  .initial_issue_route_pending / .initial_issue_routed
  .uop[vuopIdx].lqStart/sqStart
  .uop[vuopIdx].range_remaining_entries
  .uop[vuopIdx].enq_pending/enq_accepted
  .uop[vuopIdx].launch_token {visible, epoch, predicted_keys}

vec_range_owner_by_lq[lqKey] = {uid, vuopIdx}
vec_range_owner_by_sq[sqKey] = {uid, vuopIdx}
vector_route_ready_q = {uid, dynamic_epoch}

// 仅 mixed/vector redirect 的 gate 后 deferred deq 保护使用；属于公共物理资源状态，
// 可以记录被取消的 scalar 或 vector owner，不能只存到 vector runtime。
redirect_deq_tombstone_by_lq[lqKey] = {redirect_epoch, cut_line, uid, old_dynamic_epoch, expire_sample_seq}
redirect_deq_tombstone_by_sq[sqKey] = {redirect_epoch, cut_line, uid, old_dynamic_epoch, expire_sample_seq}

// 每个物理 key 的 owner 预检必须同时覆盖 scalar 和 vector 两套索引；tombstone 也属于暂时占用。
active_lq_owner = scalar uid_by_lq 或 vector vec_range_owner_by_lq
active_sq_owner = scalar uid_by_sq 或 vector vec_range_owner_by_sq
```

`status_by_uid[uid]` 对 vector UID 只保存公共生命周期字段，例如 `active`、`enq`、`issue_ready`、
`rob_commit`、`lsq_deq`、`terminal_done` 和 ROB key。向量不得使用它的 scalar LOAD/STA/STD target 字段。

本 flow 中所有正文的 `robIdx` map 下标都指完整 `rob_key={flag,value}`；访问公共 ROB map 前先调用
`rob_order_util::rob_to_map_key()`。`status_by_uid[uid].dynamic_epoch` 是唯一可变 epoch 真源，
`vec_runtime_table[uid]` 中的同名字段只作同步镜像；token、queue item、cancel record 和 tombstone 的 epoch
是创建时快照，发现镜像不一致时停止本次 flow 并报告模型错误。

本 flow 的普通 profile 前置条件为 `vec_static.vtypeIllegal=0`。需求文档保留 illegal/vill 输入位及其权重，
但直接 `vtypeIllegal=1` 只能由尚待实现的专用异常模板处理；当前普通 admission 不能把它送入“正常
writeback/commit/terminal”链路。当前 V2 standalone `issueVldu` 没有可驱动的 `vill` 输入，`src_4` 也只
承载 VL，因此 builder 始终使用 `src_4[7:0]=VL、src_4[127:8]=0`，不能把 illegal 元数据编码到 `src_4`。默认
`MEMBLOCK_VEC_VTYPE_ILLEGAL_0_WT=100`，非默认配置而未选择专用模板时在主表构造期报错或筛除候选。

## 3. 函数调用 Flow 图

图中同时允许出现当前源码接入点与规划 helper。前者用于说明复用/扩展位置，后者直接定义后续 coding 的调用边界；
规划 helper 即使当前尚未实现，也按普通函数调用节点表示。

```mermaid
flowchart TD
    A[memblock_lsqenq_dispatch_base_sequence::send_lsqenq_cycle] --> B{next UID is V_LOAD/V_STORE?}
    B -->|no| C[现有 scalar candidate flow]
    B -->|yes| D[向量 activation 分支]
    D --> E{UID 已 active?}
    E -->|no| F[调用 common_data_transaction::activate_uid(uid,0,0)]
    E -->|yes| G[向量 candidate 分支]
    F --> G
    G --> H[选择当前 data-uop]
    H --> I{V2 canAccept guard 通过?}
    I -->|no| J[drive idle and retain enq_pending]
    I -->|yes| K[构造向量 enq slot]
    K --> L[start_item / finish_item]
    L --> M[现有 LSQ driver clocking launch]
    M --> N[建立 provisional token]
    N --> O[pending launch token]
    O --> P[next driver boundary]
    P --> Q{DUT-visible sample and epoch unchanged?}
    Q -->|no, token 未可见| R[撤销 token并恢复本地 reservation]
    Q -->|yes| S[将 token promotion 为正式 range]
    Q -->|epoch 已变且 range 已可见| T[按实际 remaining 进入 redirect cancel]
    S --> U[完成向量 sample]
    U --> V{all D data-uop accepted?}
    V -->|no| G
    V -->|yes| W[push vector_route_ready_q once]
    W --> X[现有 route_all_issue_queues 的向量分支]
    X --> Y[vector_issue_q D 个初始 item]
```

### 3.1 函数调用 Flow 图整体文字伪代码

```text
1. 每拍 admission 入口：
   现有 send_lsqenq_cycle 先沿用 redirect cancel、driver sample 和 scalar/non-LSQ 的原处理。
   读取 next admission UID；若其 op_class 不是 V_LOAD/V_STORE，完全走原 scalar 路径。

2. activation：
   vector_activation_service 只处理 admission 前缀的当前 UID；若尚未 active，调用
   common_data_transaction::activate_uid(uid,0,0)，建立唯一 uid_by_active_rob、公共 active 状态和空的
   vec_runtime_table。activation 不分配 LQ/SQ，也不推进 enq 前缀。

3. 选择 vector data-uop：
   vector_admission_service 只读取当前 macro runtime 的 next_enq_vuopIdx。
   若同一 macro 尚有 data-uop，则选最小未 accepted 的 vuopIdx；不扫描完整主表，也不跳到后续 UID。
   FOF fix-VL tail 不属于本阶段，绝不生成 enqLsq 请求。

4. 预览与驱动：
   使用共享 lsq_ctrl 的当前 pointer/free count 和 V2 canAccept guard 预览一个长度为 F 的 LQ 或 SQ range。
   guard 同时考虑本拍需求、LSQLdEnqWidth/LSQStEnqWidth 保留量、redirect 延迟更新和两侧队列；资源不足、
   redirect freeze 或当前 driver 不能 launch 时，不消费 vuopIdx、不修改 pointer，发 idle 或等待。
   有资源时只构造一个 vector data-uop 的完整 enqLsq slot，其他 slot 对该 vector admission 保持 idle。

5. launch 与 sample：
   driver launch 时调用 reserve_vector_launch_token：根据当前 pointer 预览完整 F 个 key，建立 provisional
   reservation token，并临时推进软件 enqueue pointer、扣减自身 reservation delta；token 标记为 `visible=0`，本拍不允许 issue。
   下一 driver 边界确认 DUT 是否采样该 valid。若 redirect epoch 在 token 尚未可见前失效，sample helper 只把 token
   交给 redirect coordinator；coordinator 取得 writer gate 后才按 token 自身 pointer 边界和 reservation delta 撤销。helper
   先校验 pointer==token.after，失败时保持 token、pointer、free count 原样并 fatal，成功后才回退 pointer、加回自身 delta。
   gate 会暂停同 domain 的 deq/cancel/admission writer；整个 provisional 取消不写 vector range map，也不产生 cancel record。
   若采样确认有效，调用 promote_vector_range_allocation：把 token 提升为正式 range owner，标记 `visible=1`，
   不重复扣减 pointer/free count；若 redirect 在 range 已可见后到达，才按实际 remaining entry 写 tombstone 和
   cancel record。

6. sample 后开放：
   下一 driver 边界 complete_vector_admission_sample 将已 promote 的 uop 记为 enq_accepted。
   直到 D 个 data-uop 都 enq_accepted，macro.enq_done 才置 1，并把所有 data-uop 写入已经固定的
   vector_route_ready_q；随后 route_all_vector_issue_queues 只消费该轻量队列，为 D 个 data-uop 各写入一个
   已固定 lane 的 vector issue item。该 macro 标记 initial_issue_routed 后不得重复建立初始 item。scalar UID 的
   route、queue 和 status 字段不参与这一过程。
```

## 4. 现有入口：`send_lsqenq_cycle()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv`。

抽象功能描述：该 task 是当前每拍 LSQ admission 的真实入口。它先处理 redirect cancel 和上一批 pending
sample，再构造 scalar candidate 并在 launch 后调用 `commit_allocate()`；它目前没有 vector range 语义。

真实逻辑摘要：

```systemverilog
apply_pending_lsq_cancels();
collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys);
start_item(tr);
finish_item(tr);
complete_v2_pending_sample(has_progress);
confirm_lsq_candidates(tr, uids, trs, behaviors, lq_keys, sq_keys, has_progress);
```

文字伪代码：

```text
现有 task 先处理已经定案的 cancel record，保证 pointer/free count 对本拍候选正确。向量扩展后该入口必须先查询
redirect_epoch writer gate：gate 有效时只能由匹配 owner 应用当前 aggregate record；foreign record 和任何候选都保留原
batch，不能让 `apply_pending_lsq_cancels()` 在 redirect aggregate 尚未 finalized 时抢先改 pointer/free count；
然后在 driver 完成一个 item 后结算上一批 sample；
当前 scalar candidate 只有在 launch 成功后才 commit allocation；
向量扩展必须在“选择 scalar candidate”之前按 op_class 分流，不能把一个 vector macro 塞进
现有的 scalar uid/behavior 一元组后再依赖 scalar commit_allocate。
```

## 5. `send_lsqenq_cycle()` 的向量 Activation 分支

扩展位置：`memblock_lsqenq_dispatch_base_sequence::send_lsqenq_cycle()` 内，位于现有 scalar candidate 选择之前。

抽象功能描述：该分支负责把 admission 前缀中的向量 UID 从“尚未 active”转换为唯一 active ROB 实例，并初始化
向量宏运行表。它只建立公共 ROB ownership 和向量运行状态，不分配 LQ/SQ entry，也不创建 issue queue item。

文字伪代码：

```text
读取公共 admission cursor 指向的 UID；若 UID 不是 V_LOAD/V_STORE，返回“非向量”。
若 UID 已 active：从 `main_table_by_uid[uid].op_class` 确认它仍是 V_LOAD/V_STORE，直接进入 vector_admission_service；
  `status_by_uid` 不保存第二份 op_class 真源。
若 UID 尚未 active：
  若这是 redirect 后 re-admission：先确认旧实例的 vector queue/range owner 与 `vector_route_ready_q` 中旧 epoch item 已删除、公共 uid_by_active_rob 已由
  `retire_active_uid(uid)` 删除、`clear_uid_dispatch_result(uid)` 已清 active_instance_flush_epoch_valid，且相关 tombstone
  已过期；若旧 redirect 产生了非零 `redirect_cancel_record_ref`，还必须确认该 shared aggregate record 已由
  redirect gate owner 的 `apply_guarded_lsq_cancel_record()` 标记 `software_applied=1`；零 cancel 时该条件自动满足。若
  `redirect_old_epoch_valid=1`，还要按 `{uid,redirect_old_dynamic_epoch}` 确认旧
  `fof_send_reservation`、`active_fof_owner` 和 `fault_head_kind=VECTOR` 的 shared fault-head token 均不存在；不能要求
  全局 FOF owner 或 scalar fault-head 为空。任一条件不满足时仅等待，
  不调用 activate_uid。
  调用 common_data_transaction::activate_uid(uid, 1'b0, 1'b0)：检查 UID 未终态、ROB key 未被占用，
  写入统一 uid_by_active_rob，并置公共 active/activation epoch；因两个 map 参数为 0，不写标量 LQ/SQ map。
  以公共 `status_by_uid[uid].dynamic_epoch` 为唯一 epoch 真源，原子重置公共状态：active=1、flushed=0、redirect_pending=0、enq=0、issue_ready=0、
  success=0、rob_commit=0、lsq_deq=0、terminal_done=0、fault=0、exception_pending=0、公共 issue_killed=0；同时创建或重置
  vec_runtime_table[uid]，按 D 建立 uop runtime，next_enq_vuopIdx=0、enq_pending=1、fault_pending=0；清零
  runtime 镜像 issue_killed、所有旧 issue/replay 状态和 route-once 状态。此处不再次递增 epoch：redirect transition
  已经为重入实例产生新 epoch；设置 `next_enq_vuopIdx=0`、`fof_next_vuopIdx=0`，并清除
  `redirect_old_epoch_valid`、旧 epoch 快照和 `redirect_cancel_record_ref`（置为 invalid）。
  按当前 `v_form` 重新初始化 FOF tail：`fof_tail_pending=(v_form==fof)`，其余
  `fof_tail_issued/fof_tail_written_back/fof_tail_canceled=0`。旧 FOF scheduler owner 只能通过带
  `{uid,dynamic_epoch}` 的 `release_active_fof_owner()` 或 `kill_vector_driver_pending()` 清理，不能无条件清除其它
  macro 的 owner。只有在旧 dynamic_epoch 的 cancel/tombstone 已完成后才允许此重置。
激活失败时保留 admission cursor，不构造 enqLsq transaction，并按基础合法性错误报告 fatal。
```

`activate_uid()` 是当前源码中已有的公共函数；向量只复用它的 ROB/公共状态职责，不能调用
`activate_uid_by_behavior()`，因为该 helper 会尝试从标量 behavior 建立单一 LQ/SQ owner。

## 6. `send_lsqenq_cycle()` 的向量 Candidate 与 Enqueue 分支

扩展位置：`memblock_lsqenq_dispatch_base_sequence::send_lsqenq_cycle()` 的 candidate/slot 构造阶段。

抽象功能描述：该分支为当前主表头部的 vector macro 选择唯一的下一个 data-uop，并构造一次最小的
`enqLsq` 驱动请求。它不拥有 DUT ready，不负责 issue，也不处理 FOF tail。

输入/输出：

- 输入：`uid`、静态 `D/F`、`vec_runtime_table[uid]`、共享 `lsq_ctrl_model` pointer/free count、当前 redirect epoch。
- 输出：一个待 launch 的 vector enq descriptor，或“本拍没有 vector candidate”。
- 状态副作用：成功 launch 后建立 provisional reservation，并在 sample 后 promotion 为正式 runtime range；
  预览阶段零副作用。

文字伪代码：

```text
确认 uid 的 op_class 是 V_LOAD 或 V_STORE；否则返回“不是 vector candidate”。
读取 macro runtime；若 macro 已 canceled、enq_done、flushed 或全局 issue/admission freeze 有效，返回无候选。
若当前 `redirect_epoch` writer gate 覆盖本 uop 的 LQ/SQ domain，返回无候选并保留同一 cursor；不能先预览 key、
扣 free count 或创建 token。公共 scalar candidate 入口也必须在调用 `commit_allocate()` 前做同样检查。
即使 writer gate 已释放，公共 scalar/vector allocation preflight 仍必须查询按物理 key 建立的
`redirect_deq_tombstone_by_lq/sq`：命中时本拍返回“暂不可分配”，保留 cursor/pointer/free count，等待关联
gate-owned deferred envelope 完成 provenance 分类、tombstone 达到 output-drain 过期条件后重试；不能覆盖 tombstone
建立新 owner。该 quarantine 只会由 `vector_involved=1` 的 mixed/vector redirect 建立，因此 `vector_involved=0` 的
scalar-only fast path 不新增检查结果或时序变化。
读取 next_enq_vuopIdx；该值必须小于 D，且对应 uop.enq_pending=1。
从静态 layout 读取 F；要求 1 <= F <= 16，不能因 VL=0、mask-off 或最后一个 uop 缩小 F。
执行向量 `canAccept` 预检：按 V2 `LsqEnqCtrl` 的保守规则计算本拍两侧需求：
  `batch_lq_need` = 本拍所有有效 load（此初版只有当前 vector uop）的 F 之和；
  `batch_sq_need` = 本拍所有有效 store（此初版只有当前 vector uop）的 F 之和；
  `lq_guard` = (`lq_free_count >= batch_lq_need + LSQLdEnqWidth`)；
  `sq_guard` = (`sq_free_count >= batch_sq_need + LSQStEnqWidth`)；
  guard 还必须确认 redirect/t2_update 未阻塞，且 pending sample 不会重复计入本拍需求。
  `LsqEnqCtrl` 的总 `canAccept` 是 `lq_guard && sq_guard`，即使当前 uop 只使用一侧，也不能跳过另一侧检查。
guard 不通过时保留 enq_pending，不推进 UID 或 enqueue pointer。
guard 通过后，预览的每个 LQ/SQ key 还必须同时不属于 scalar owner、vector owner、未过期 redirect-deq tombstone、
其它 feedback/writeback tombstone 或 pending allocation token；命中 redirect-deq tombstone 时等待 key 可复用，不能把它
当作 owner 漏失；已被活动 owner 占用或两类 owner 同时命中则报告模型错误。
构造完整向量 payload，并返回给既有 LSQ driver。
```

向量 `canAccept` 预检不读取一个尚未采样的“未来 canAccept”值，也不把 `free_count >= F` 当作充分条件。
V2 `LsqEnqCtrl` 的真实计算先把本拍有效请求折算为 `lqAllocNumber/sqAllocNumber`，再分别要求
`counter >= allocNumber + LSQLdEnqWidth/LSQStEnqWidth`，并通过 `RegNext` 产生对 dispatch 可见的
`canAccept`。由于当前 standalone testbench 没有把该输出作为独立输入接回 sequence，软件模型必须镜像这个
保守公式并使用 pending-sample token 对齐一个周期；若后续接出真实 `canAccept`，仍应把它作为最终 launch gate，
软件预检只能提前拒绝，不能绕过硬件 gate。

## 7. `confirm_lsq_candidates()` 与 `complete_v2_pending_sample()` 的向量 Reservation 扩展

扩展位置：现有 launch 确认和下一 sample 结算阶段，复用 `lsq_ctrl_model` 的 pointer/free-count 所有权。

抽象功能描述：launch 分支为一个 data-uop 暂存完整的 F-entry provisional reservation，防止在下一个 sample
边界前重复使用 key；sample 分支确认 DUT 已采样后把 token 提升为正式 vector range owner。两段逻辑共同拥有
向量 range 的分配边界，但都不进入 scalar `status_transaction` target 字段。

输入/输出：

- 输入：`uid`、`vuopIdx`、方向、`F`、运行期当前 LQ/SQ enqueue pointer、launch epoch。
- 输出：pending token（预测的 `lqStart/sqStart` 和完整 key 列表），以及 sample 后正式的 vector entry owner map（公共 active ROB map 已在 activation 阶段建立）。
- 副作用：token 建立时临时推进对应 enqueue pointer、从当前 free count 扣除自身 `reserved_entries=F`，并记录该 delta；promotion 不重复扣减；token
  在未可见时取消则只撤销自身 delta，不覆盖其它 deq/cancel 对当前 free count 的变化。

文字伪代码：

```text
launch 后建立 provisional token：
确认 uid 已由 vector_activation_service 置为 active；若未 active，返回错误并不消费任何 LSQ pointer。
记录 launch 时的 `dynamic_epoch`，完整预检 F 个 key；任何 active owner、pending token 或未过期 tombstone
冲突都使本次 launch 不成立，不修改 pointer/free count。
不在本 helper 中重复建立或覆盖 uid_by_active_rob；该 map 的唯一写者是公共 activation API。
选择 lq_enq_ptr 或 sq_enq_ptr 作为 token 起点；将 token 标记 `visible=0`，临时推进对应 enqueue pointer、从当前 free count 扣除
`F`，并记录自身 `reserved_entries=F`。
对 offset=0..F-1：
  用现有环形 advance_lq_key/advance_sq_key 取得真实 entry key；
  对 LQ key 同时查询 scalar `uid_by_lq`、vector `vec_range_owner_by_lq`、pending allocation token 和
  未过期 vector tombstone；SQ key 对应查询四类 SQ 索引。
写入 pending token，不写正式 vector range map；uop 保持 `enq_pending=1`。

下一 sample 后 promotion token：
在下一 sample 边界确认 token 的 launch epoch 仍有效且 DUT 已采样后，原子地把 token 中全部 key 写入
`vec_range_owner_by_lq/sq`，将 token 标记 `visible=1`，写入 runtime 的 `lqStart/sqStart` 和
`range_remaining_entries=F`，清 `enq_pending` 并保留 `enq_accepted=0`。promotion 不再次推进 pointer/free count。
若 sample 前 epoch 已失效，`complete_v2_pending_sample()` 必须先原子地把该 token 移入
`redirect_transition_plan[redirect_epoch].pending_tokens`，确认 handoff 成功后才清本拍 pending-sample 包装并返回 recovery
path；它不得自行调用 `cancel_vector_launch_token()`。随后 `run_redirect_lsq_transition()` 取得 writer gate 后才取消 token：
helper 先确认当前 enqueue pointer 等于 token 的 `after` 边界；校验失败则 fatal，token、pointer 和当前 free count 均保持不变；
校验成功后才回退到 `before`、对当前 free count 加回 token 的 `reserved_entries`，最后删除 token；不写 owner map、不产生
LSQ cancel record。若 token 已 `visible=1` 后才被 redirect 覆盖，则由 redirect handler 按实际 remaining entry 统计 cancel。
只有 promotion 完成后才推进 macro.next_enq_vuopIdx。
```

`uid_by_active_rob` 也要参与 allocation 前的生命周期检查：同一 ROB key 若仍由其它 active UID 占用，立即
报告重复 owner；若只命中本 UID 的 redirect tombstone，则等待 tombstone 到期及 redirect flush drain 后再重新
`activate_uid`。因此“同 UID re-admission”不能绕过 tombstone 检查，也不能因为静态 ROB key 相同而覆盖旧实例。

初版 admission 每次只 launch 一个 data-uop，并且复用现有 dispatcher 的全局单深度 `pending_sample_valid`：在该
uop 的 sample/promotion 或 redirect cancel 完成前，不允许任意 scalar/vector 再 launch allocation。因而初版全系统
至多一个 `visible=0` vector token，且它必定位于相应 enqueue pointer 的尾部；这比“每个 macro 至多一个”更强，
是 `cancel_vector_launch_token()` 能直接回退 pointer 的必要条件。

redirect 由 `run_redirect_lsq_transition(redirect_epoch)` 在公共 redirect scan 前一次性取得同 domain writer gate，
并在 gate 内对 token 做 after/before、pointer 和 owner 的整体预检。gate 不可取得或有更早 pending cancel record 时返回
DEFER，不修改本 token 或任何 vector lifecycle 状态；只有预检不一致才 fatal。gate 成功后，所有普通
admission/reservation、DUT dequeue commit 和 foreign cancel apply 均缓存原 batch，不能写入相同 domain 的
pointer/free count；待 redirect aggregate cancel record 完整回退并释放 gate 后才按原顺序继续。

若未来扩大 admission slot，不能仅按 macro 内顺序取消多个 token。必须先引入覆盖 scalar/vector 的统一 allocation
ledger，并按每个 domain 的全局逆 allocation 顺序预检、取消和回退；在此之前多个 `visible=0` vector token 属于
不支持状态并报告 fatal。

本 helper 的检查和写入必须按“先完整预检、后一次性提交”实现。不能写入前几个 entry 后在第 k 个 entry 发现冲突，
再尝试部分回滚；这样会让标量/向量 owner map 与共享 pointer 失去一致性。

## 8. `complete_v2_pending_sample()` 的向量完成分支

抽象功能描述：该分支在真实 driver sample 边界后消费上一个 launch token，并把已经 promotion 的 vector
data-uop 变为 DUT 可见。
它决定宏指令何时 `enq_done`，但不向 scalar issue queue 写 item。

文字伪代码：

```text
读取 pending vector sample；若 token 尚未 promotion 且该 sample 的 redirect epoch 已失效：
  不在 sample helper 中直接回退 pointer；先把 token 原子移入 `redirect_transition_plan[redirect_epoch].pending_tokens`，
  handoff 成功后才清本拍 pending-sample 包装并返回 recovery path。redirect coordinator 取得 writer gate 后才调用
  `cancel_vector_launch_token`；其先校验 pointer==token.after，失败时保持 token、
  pointer、free count 不变并 fatal，成功后才按 token 的 pointer 边界和 reservation delta 撤销本地 provisional reservation。
  不置 enq_accepted，不写 owner map，也不产生 cancel record。
若 token 尚未 promotion 且 sample 有效：
  调用 promote_vector_range_allocation，把 token 原子提升为正式 range owner；再将对应 uop.enq_accepted 置 1。
若 token 已 promotion 且 sample 有效：
  将对应 uop.enq_accepted 置 1。
若 token 已 promotion 但随后被 redirect 覆盖：
  不在此处重复回退；由 vector redirect handler 按 `visible=1` 的实际 remaining entry 写 tombstone 和 cancel record；
  本次 helper 返回 recovery path。
对于有效的 enq_accepted 更新，检查该 macro 从 0 到 D-1 的 data-uop 是否都 enq_accepted。
若尚未全部完成：
  返回 admission loop，继续同一 UID 的下一个 data-uop。
若全部完成：
  置 macro.enq_done=1，并通过公共 `set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1'b1)` 推进连续 admission 前缀；
  仅在 enq_done 的 0->1 上升沿置 macro.initial_issue_route_pending=1，并向 vector_route_ready_q 写入
  {uid,dynamic_epoch}；不得每拍扫描所有 enq_done macro 或反复插入同一 D 个 item。
  后续向量专用 route_all_vector_issue_queues 消费该 ready item，才把 D 个轻量 item 加入该 macro 固定的
  vector_issue_q，并在 route 成功后清 route_pending、置 initial_issue_routed=1 和公共 issue_ready=1。
  不调用当前 scalar `complete_admission()`，因为它会进入 `issue_sched.prepare_issue_route_for_uid()` 并访问标量 target 字段。
```

## 9. 静态字段、运行期字段和合法关系

| 信息 | 所有者 | 何时确定 | 说明 |
| --- | --- | --- | --- |
| `D`、`F`、`vuopIdx`、`lastUop`、load/store、`flowNum` | 静态向量表 | 主表初始化 | 都由已随机的 VTYPE/形式派生。 |
| `lqStart/sqStart`、range key、range remaining | vector runtime | token promotion 后 | 依赖真实 pointer，不能预随机；promotion 前只存在 provisional token。 |
| `enq_pending/enq_accepted/enq_done` | vector runtime | admission/sample | 只表示测试框架的外部入队阶段。 |
| `initial_issue_route_pending/routed`、`vector_route_ready_q` | vector runtime + 小队列 | 全部 D 个 sample 后 | 防止高频 service loop 对同一 macro 重复插入 D 个初始 issue item。 |
| `uid_by_active_rob` | 公共 active map | activation | 一个完整 ROB key 只有一个 UID；range allocation 不重复写入。 |
| `vec_range_owner_by_lq/sq` | vector runtime 索引 | 每个 range allocation | 每个物理 entry 一条映射，deq/cancel 后删除；分配前与 scalar `uid_by_lq/sq` 联合预检。 |
| `vector allocation token` | launch 到下一采样边界之间的暂存 reservation，含 `visible=0/1` | admission pending sample | `visible=0` 被 redirect 取消时只恢复本地 reservation；`visible=1` 才按实际 remaining 进入 cancel。 |
| `vector tombstone` | 旧动态实例删除后仍不可复用的短期 key 记录 | `vec_tombstone_by_rob/lq/sq` 与 `vec_tombstone_by_fof_tail[rob_key]` | 到期前新实例只能等待，不能覆盖旧 event 保护窗口。 |

普通 data-uop 必须保持：

```text
enqLsq.req.numLsElem == static F == issueVldu.flowNum
enqLsq.uopIdx         == issueVldu.uop.vpu.vuopIdx
enqLsq.lastUop        == issueVldu.uop.vpu.lastUop
enqLsq.robIdx         == issueVldu.uop.robIdx
```

FOF fix-VL tail 的 `flowNum=0`、`numLsElem=0` 只表示“不创建普通 range”；它不进入本 flow。

## 10. plus 控制与边界

本 flow 直接复用：

- `MEMBLOCK_LSQENQ_SEQ_EN`：关闭时全部 admission 停止，向量不建立半成品 range。
- `MEMBLOCK_LSQENQ_READY_TIMEOUT`：沿用 driver/admission 的无推进诊断，不作为正常退出条件。
- `MEMBLOCK_ENQ_PER_CYCLE` 及其随机模式：只控制现有 scalar candidate 的总 slot 行为；向量初版固定每次一个
  data-uop，不把该参数误解释为“一次分配多少 vector flow”。向量 admission 仍必须满足 V2
  `LSQLdEnqWidth/LSQStEnqWidth` 的 `canAccept` 保留量，不能用该参数绕过 width guard。
- 向量 `op_class/v_mode/v_form/VTYPE` 权重：只决定静态宏指令形状，不改变本 flow 的分配时机。

初版不新增 vector admission 宽度参数。若未来确实需要一个 macro 同拍多个 vector data-uop，除了证明多 slot
payload、range 连续性和 scalar 混合 batch 外，还必须先实现统一 allocation ledger，证明跨 UID/类别的全局逆序
redirect rollback 正确后才允许扩展。

## 11. 端到端行为总结

```text
正常 vector load/store：
  主表 vector macro
  -> next_enq_vuopIdx=0
  -> 单个 data-uop enqLsq launch
  -> 建立 visible=0 的 launch token
  -> 下一 sample promotion 为 visible=1 的 vector range
  -> 置 enq_accepted
  -> 重复至 D-1
  -> macro.enq_done
  -> vector_route_ready_q（只写一次）
  -> route_all_vector_issue_queues
  -> vector_issue_q 的 D 个初始 item

资源不足或 `canAccept` guard 未通过：
  vector data-uop enq_pending
  -> 不改变 pointer/range map/uid，也不消费 pending sample
  -> idle 或等待下一 admission cycle
  -> 资源恢复后重试相同 vuopIdx

redirect 覆盖 launch 后状态：
  公共 `run_redirect_lsq_transition(redirect_epoch)` 先取得同 domain writer gate；gate 不可取得/有更早 pending
  cancel record 时保留 redirect freeze 和 raw batch，返回 DEFER，不能把合法竞争误报 fatal
  -> 在 gate 内取消初版唯一、位于尾部的 token.visible=0 reservation；先校验 pointer==token.after，成功才回退 pointer、
     按当前计数加回自身 reservation delta、删除 token；不写 tombstone/cancel_record
  -> 对 token.visible=1 的正式 range 写 tombstone，按实际 remaining entry 只累加到当前 redirect aggregate cancel record
  -> 按旧 `{uid,dynamic_epoch}` 删除 vector range map/issue item；已发未写回的 FOF tail 另写 tail tombstone；记录
     `redirect_old_epoch_valid=1` 与 `redirect_old_dynamic_epoch=dynamic_epoch`
  -> 等所有 scalar/vector 覆盖 UID 均计数完成后，coordinator 在 gate 内应用完整 aggregate record 并置 software_applied
  -> 调用 retire_active_uid(uid) 删除公共 ROB owner，再调用 clear_uid_dispatch_result(uid) 清旧 activation epoch
  -> status 置 flushed/redirect_pending、terminal_done=0，并只递增公共 `status_by_uid[uid].dynamic_epoch` 一次；释放 gate
  -> 等待 redirect/monitor drain 和 tombstone drain
  -> 同一 UID 重新 activate，重新从 vuopIdx=0 admission

redirect 发生在 activation 之后但尚未 launch：
  只有公共 ROB owner 和空的 vector runtime
  -> 不产生 LSQ cancel count
  -> 调用 retire_active_uid(uid) 删除公共 ROB owner，再调用 clear_uid_dispatch_result(uid) 清旧 activation epoch
  -> 保留 UID，置 flushed/redirect_pending、terminal_done=0
  -> redirect 完成后同一 UID 重新 activate，不把该 UID 当作 terminal

redirect 发生在 launch 之后但 token 尚未 DUT-visible：
  必须由 redirect coordinator 持有 writer gate 后调用 `cancel_vector_launch_token()`；先校验 pointer==token.after。校验失败时
  token、pointer、free count 均不变并 fatal；gate 不可取得时只 DEFER。校验成功后才回退 enqueue pointer、按自身
  reservation delta 加回当前 free count、删除 token
  -> 不写 vector range owner、不产生 LSQ cancel count
  -> 保留 UID，等待 redirect drain 后同一 UID 重新 admission
```

端到端文字伪代码：

```text
正常场景中，框架先把整个 vector macro 的 D 个 data-uop 逐个变成 DUT 已见的 range，才允许任何一个 uop 进入
vector issue queue。这样 range 起点、F 和后续 issue payload 可以逐项核对。

资源不足不是失败，也不消耗 uid 或 vuopIdx；它只让该 macro 停在当前 uop，保持 scalar 流程的资源模型不变。

redirect 覆盖已经 launch 的 range 时，框架记录“该 range 确实已预约”并交给现有 cancel owner 回退，不把它误当成
从未发生的 enq；静态 source、D/F 和 UID 保持不变，旧 range 清理且 tombstone 排空后再重新分配运行期起始指针。
```
