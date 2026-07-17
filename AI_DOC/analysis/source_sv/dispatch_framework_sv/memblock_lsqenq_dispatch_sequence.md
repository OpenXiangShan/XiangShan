# `memblock_lsqenq_dispatch_base_sequence` 源码分析

## 1. 职责与边界

源码：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv`

该 sequence 负责把公共主表中的 scalar load/store 按 uid 顺序驱动到 V2
`io_ooo_to_mem_enqLsq_*` 接口，并维护测试框架的软件 LSQ allocation 镜像。它不负责 issue fire、
writeback、ROB commit、LSQ deq、pass/fail 或 terminal 判定。

本轮只支持 scalar LS：

- load：`needAlloc=2'b01`，占用 LQ。
- store：`needAlloc=2'b10`，占用 SQ。
- LQ `fuOpType`只允许普通load `0..6`和software prefetch `8/9/10`；SQ只允许普通store `0..3`。
- `uopIdx=0`、`numLsElem=1`、`lastUop=1`。
- `exceptionVec/trigger/flushPipe=0`，本轮不构造 enqueue 前 directed 异常。
- vector LS、`issueVldu`、MOU/AMO 和 CBO 不在该 V2 admission 路径中静默兼容。

## 2. V2 时序模型

V2 顶层没有 LSQ enqueue `canAccept/response`。driver 使用 clock-first streaming：

```text
C0 边界：DUT 采样旧 idle；driver 把 batch A 写到 VIF；sequence 立即预留 A 的软件 LSQ 资源。
C1 边界：DUT 采样 A；driver 把 batch B 写到 VIF；sequence 先开放 A 的 issue route，再预留 B。
C2 边界：DUT 采样 B；driver 把 batch C 写到 VIF；sequence 先开放 B 的 issue route，再预留 C。
```

`finish_item()` 返回只表示 driver 已处理本 item 的 launch，不表示当前 item 已被 DUT 采样。为此
sequence 保存单深度 pending-sample batch，把 allocation reservation 与 `issue_ready` 分开一个 driver
边界。

## 3. 主调用链

```text
body
  -> seq_csr_common::init
  -> configure_from_plus
  -> ensure_helpers
  -> wait_for_main_table
  -> drive_lsqenq_loop
       -> send_lsqenq_cycle
            -> apply_pending_lsq_cancels
            -> pending sample后遇non-LSQ时先send_idle_lsqenq_boundary
            -> admit_non_lsq_if_ready
            -> collect_lsq_candidates
            -> 无candidate时send_idle_lsqenq_boundary
            -> clear_lsqenq_xaction
            -> assign_lsqenq_slot / set_req_fields
            -> start_item / finish_item
            -> complete_v2_pending_sample
            -> confirm_lsq_candidates
```

## 4. Candidate 收集

`collect_lsq_candidates()` 每拍只调用一次 `seq_csr_common::get_enq_per_cycle()`。固定模式返回
`MEMBLOCK_ENQ_PER_CYCLE`；随机模式按 ZERO/MIDDLE/MAX 三类权重返回
`0..MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM`。

返回 0 时立即返回空 candidate，不读取或修改 uid、pointer、free count、主表、状态表或 map；
`send_lsqenq_cycle()` 随后发送一拍全零 idle。返回非零时从
`common_data_transaction::get_next_new_admit_uid()` 开始只预览连续 uid 前缀：

```text
初始化本拍 load_elem_count=0、store_elem_count=0；
复制 LQ/SQ enqueue pointer 和 free count 到局部变量；
按 uid 顺序循环，最多收集 total_slot_limit 个 candidate：
  遇到主表尾部、已处理状态、non-LSQ 或 global flush 时停止；
  derive_op_behavior 推导该 uid 使用 LQ 还是 SQ；
  scalar 路径要求 num_ls_elem=1，否则 fatal；
  计算加入该 uid 后的 load/store element 累计值；
  若 load 超过编译期 6、store 超过编译期 4，或超过实际 free count，则停止；
  保存 uid、transaction、behavior 和预测 key；
  只推进局部 pointer，不修改真实软件 LSQ 镜像。
```

6-load/4-store 是本拍物理端口上限，不是必须预留的空项数。函数不要求 LQ/SQ 在 batch 前始终保留
6/4 个空项，也不使用 `tentative + 6/4`，因此队列尾部容量仍可被正常使用。

## 5. Request 构造

`set_req_fields()` 是 slot request qualifier/payload 的唯一 setter。它在进入 `case (slot)` 写字段前执行
以下直接入口检查，任何不满足项都先 `uvm_fatal`：

- `tr` 必须非 `null`，`slot` 必须小于编译期 `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM`。
- active slot 要求 `main_tr` 非 `null`；`main_tr.numLsElem` 和 `behavior.num_ls_elem` 都必须为 1；
  load 必须严格满足 `need_alloc=2'b01 && uses_lq && !uses_sq`，store 必须严格满足
  `need_alloc=2'b10 && !uses_lq && uses_sq`。
- idle slot 要求 `main_tr == null`、`behavior` 完整等于
  `lsq_ctrl_model::make_default_behavior()`，且 LQ/SQ key 的 flag 和 value 全部为 0，不能只满足
  `need_alloc==0`。

active slot 的 key 入口语义由相邻两个 fail-fast 边界补齐：LQ/SQ key 必须是
`collect_lsq_candidates()` 保存的当前软件 pointer preview，setter 把两组 candidate key 原样写入当前 slot；
driver 在首次写 VIF 前检查 key value 不越过真实 ROB/LQ/SQ size，`confirm_lsq_candidates()` 在建立公共
reservation 前再比较 behavior 实际使用的 key 是否仍等于 preview。前者失败时不会写 VIF，后者失败时
不会调用 `commit_allocate()` 修改公共状态。

通过入口检查后，active scalar slot 从 `main_tr`、`behavior` 和预测 LQ/SQ key 构造以下字段：

| 字段 | active scalar | idle slot |
|---|---|---|
| `valid` | 1 | 0 |
| `needAlloc` | load=`01`，store=`10` | 0 |
| `fuType` | `encode_and_fit_dut_futype()` 的 V2 编码 | 0 |
| `fuOpType` | `main_tr.fuOpType` | 0 |
| `uopIdx` | 0 | 0 |
| `numLsElem` | 1 | 0 |
| `lastUop` | 1 | 0 |
| `robIdx` | `main_tr.get_rob_key()` | 0 |
| `lqIdx/sqIdx` | candidate 预测 key | 0 |
| `exceptionVec/trigger/flushPipe` | 0 | 0 |

setter 只写传入 `tr` 的当前 `slot` request 字段，不修改主表、状态表、active map、pending-sample、软件
pointer 或 free count；`needAlloc` 由同一 slot 的 `set_need_alloc()` 单独写入。active key 越界会在 driver
写 VIF 前 fatal，实际使用 key 漂移会在 `commit_allocate()` 修改公共状态前 fatal。

`clear_lsqenq_xaction()` 遍历编译期 slot 数，通过同一个 setter 清除所有 qualifier 和 payload，避免
active、idle、redirect abort 之间保留上一 item 的 V2 extra 字段。

## 6. Driver launch

源码：`mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`

driver 主循环每轮先等待 `@vif.drv_mp.drv_cb`，再把 `req` 置空并调用 `try_next_item()`。无 item 时
驱动 `DRV_0`；有 item 时：

```text
要求 pre_pkt_gap/post_pkt_gap 都为 0；
清 request_launched 和 aborted_by_redirect；
若 active item 的 flush epoch 已失效：
  标记 aborted_by_redirect=1；
  驱动全零 idle；
否则：
  validate_v2_scalar_item 在首次 VIF 写入前检查全部 slot；
  send_pkt 只发送一次当前 item；
  active item 标记 request_launched=1；
立即 item_done，不在本 item 内等待 canAccept/response，也不立即撤销 active request。
```

`lsqenq_agent_agent_xaction`先约束通用default sequence随机item：active LQ的`fuOpType`只允许load `0..6`
和prefetch `8/9/10`，active SQ只允许store `0..3`，并由`c_v2_batch_enqueue_width`限制active
load/store数量不超过compile 6/4。`validate_v2_scalar_item()`再要求inactive slot的qualifier和全部payload为零；
active slot的`needAlloc/FuType/fuOpType`必须匹配支持范围，整个batch的load/store计数仍不超过compile 6/4，
并检查key value不越过ROB/LQ/SQ实际size。这样关闭约束或手工构造的directed item也不能绕过scalar opcode
和4-store边界。driver build阶段要求`drv_mode==DRV_0`，防止reset、无item或redirect abort时随机驱动valid/X。

## 7. Launch reservation 与 sample completion

`confirm_lsq_candidates()` 只处理当前 item 的 launch reservation：

```text
如果 driver 未 launch 且不是 redirect abort/epoch 失效，fatal；
如果 launch 和 abort 同时为 1，fatal；
如果当前 flush/epoch 已失效，不建立 reservation；
逐 uid 重新 preview 实际使用的 LQ 或 SQ key；
预测 key 漂移时在修改状态前 fatal；
调用唯一 allocation owner lsq_ctrl_model::commit_allocate：
  写主表 LQ/SQ key；
  建立 active/enq 和 key map；
  推进软件 enqueue pointer 并扣减 free count；
把本批 uid 和 epoch 保存为 pending_sample；
此时不设置 issue_ready。
```

下一次 `finish_item()` 返回后，`complete_v2_pending_sample()` 处理上一批：

- `pending_sample_flush_epoch == memblock_sync_pkg::dispatch_flush_epoch` 且没有 global flush：逐 uid 调用 `complete_admission()`，由
  `issue_queue_scheduler::prepare_issue_route_for_uid()` 设置 `issue_ready` 并进入 LOAD/STA/STD issue queue。
- 保存 epoch 与当前 epoch 不等或 global flush 阻塞：不调用 `complete_admission()`、不开放 issue，清本地
  pending-sample；资源回退仍由全局 redirect handler 累计 cancel，
  再由 `apply_pending_lsq_cancels()` 调用 `cancel_lq/cancel_sq()` 完成，不建立第二个 cancel owner。

## 8. non-LSQ 与末批边界

non-LSQ admission 本身不经过 driver 时钟。若上一 LSQ batch 仍 pending，而下一 uid 是 non-LSQ，
sequence 先发送一个全零 idle item，使上一批经过真实 sample 边界，再调用原
`commit_non_lsq_admission()` 和 `complete_admission()`。

global stop 到来时，如果仍有 pending LSQ batch，sequence 同样发送 trailing idle item并完成或按最新
epoch 作废该 batch，然后退出。这样最后一批不会停在“已预留但未进入 issue route”的中间态。

## 9. Redirect 分工

| 时点 | 本 sequence/driver 行为 | 原 recovery owner 行为 |
|---|---|---|
| launch 前 redirect | driver 不发送 active request，不建立 reservation | 下一轮从原 uid 重试 |
| launch 后、sample 前 redirect | pending batch 不开放 issue | redirect handler 根据 active mapping 累计 cancel，LSQ sequence 回退资源 |
| sample/issue-ready 后 redirect | 不增加本地特殊路径 | 继续使用全局 redirect/reissue 生命周期 |

`collect_lsq_candidates()`、driver launch 和 confirm 分别检查已有 global flush/epoch 状态。本轮不增加固定
5-cycle retry guard，也不让 `flushPipe` 字段自行暂停 LSQ driver。

## 10. 参数与编译期结构

- 编译期：`MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM=6`、`MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH=6`、
  `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH=4`、V2 key/FuType/uopIdx/numLsElem 宽度。
- runtime：`MEMBLOCK_ENQ_PER_CYCLE` 和 `MEMBLOCK_ENQ_PER_CYCLE_RAND_EN` 只控制 testcase 每拍使用量。
- runtime 权重：`MEMBLOCK_ENQ_PER_CYCLE_ZERO_WEIGHT`、`MIDDLE_WEIGHT`、`MAX_WEIGHT` 控制随机模式
  选择 0、中间值或物理最大值的类别概率。
- 公共参数层仍解析 `MEMBLOCK_LSQENQ_READY_TIMEOUT` 并检查非负；V2 sequence 不读取该 getter，
  也不等待 ready/response，因为接口不存在对应端口。

## 11. 状态结果

成功 sample 后的关键状态顺序为：

```text
launch reservation：active=1，enq=1，LQ/SQ map建立，pointer/free count更新，issue_ready仍为0；
下一 driver 边界：issue_ready=1，对应 LOAD/STA/STD issue queue入队；
后续 issue/writeback/commit/deq：由各自 flow owner推进；
terminal：仍由公共 terminal owner判断，本 sequence不直接置位。
```

## 12. 当前限制

- 不支持 vector LS、`issueVldu`、segment/fixVl 和 multi-element chunk。
- 不支持 enqueue 前 directed `exceptionVec/trigger/flushPipe`。
- 不实现 DUT backpressure、issue hold、压力模式或 boundary vseq。
- LSQ enqueue monitor 本轮只同步位宽和本地采样，不恢复 analysis-port transaction 发布。
