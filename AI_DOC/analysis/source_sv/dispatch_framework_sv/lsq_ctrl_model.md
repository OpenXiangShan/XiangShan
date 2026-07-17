# `lsq_ctrl_model` 源码分析

## 1. 角色

源码：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv`

`lsq_ctrl_model` 是测试框架的软件 LSQ allocation 镜像，不是完整 LSQ RTL 参考模型。它维护唯一一组
LQ/SQ enqueue/dequeue pointer 和 free count，用于回答当前 scalar transaction 是否占用 LQ/SQ、预期
分配哪个 key，以及 commit/deq/redirect 后软件资源如何释放或回退。

## 2. 关键状态

| 状态 | 含义 | 更新者 |
|---|---|---|
| `lq_enq_ptr/sq_enq_ptr` | 下一笔分配的 LQ/SQ key | `commit_allocate()` 前进，`cancel_lq/sq()` 回退 |
| `lq_deq_ptr/sq_deq_ptr` | 已释放资源的队头边界 | `release_lq/sq()` 前进 |
| `lq_free_count/sq_free_count` | 当前可分配 element 数 | allocation 扣减，deq/cancel 增加 |

key 包含 wrap flag 和 value。value 位宽来自 compile macro，合法范围仍由真实 LQ/SQ size 检查。

## 3. 操作分类

`derive_op_behavior()` 从 `main_control_transaction` 推导 admission 和 issue route：

- scalar LDU/load/prefetch：`uses_lq=1`、`need_alloc=01`、`num_ls_elem=1`、route LOAD。
- scalar STU/store：`uses_sq=1`、`need_alloc=10`、`num_ls_elem=1`、route STA 和 STD。
- vector LS：当前 fail-fast，不允许按 scalar 静默处理。
- MOU/AMO/CBO：保留原分类边界，但不属于本轮 V2 scalar LSQ enqueue 闭环。

`memblock_op_behavior_t::num_ls_elem` 使用 `memblock_num_ls_elem_t`，与 V2 `numLsElem` 编译期宽度一致。

## 4. 预览与唯一 allocation owner

`preview_allocate()` 只读取 pointer/free count：

```text
返回当前 lq_enq_ptr 和 sq_enq_ptr 作为预测 key；
如果 behavior 需要的 element 超过对应 free count，uvm_fatal；
不写主表、状态表、map、pointer 或 free count。
```

`commit_allocate()` 是唯一 allocation owner：

```text
检查 transaction 非空且 uid 一致；
调用 preview_allocate 得到当前实际 key；
把 key 和 numLsElem 写回主表 transaction；
调用 common_data_transaction::activate_uid 建立 active 和 LQ/SQ map；
设置 MEMBLOCK_STATUS_ENQ；
按 behavior.num_ls_elem 推进使用中的 enqueue pointer并扣减 free count；
unused 队列不推进。
```

V2 `memblock_lsqenq_dispatch_base_sequence::confirm_lsq_candidates()` 在 driver 确认 request 已 launch 后直接
调用该函数建立 reservation；`issue_ready` 要等下一 driver sample 边界，由 sequence 的
`complete_v2_pending_sample()` 单独开放。

## 5. `commit_allocate_with_resp()`

该函数保留给有真实 enqueue response 的接口版本，但不再复制 allocation 状态更新代码。当前逻辑为：

```text
检查 transaction、uid 和 LSQ behavior；
调用 preview_allocate 得到 expected key；
只比较 behavior 实际使用的 key：load 比 LQ，store 比 SQ；
unused key 不参与比较；
匹配后调用唯一 owner commit_allocate 完成状态和资源更新。
```

V2 顶层没有 LSQ enqueue response，因此 V2 sequence 不调用该 wrapper，也不把软件预测 key伪装成 DUT
response。

## 6. non-LSQ、deq 与 redirect

`commit_non_lsq_admission()` 要求 `need_alloc=0` 且不使用 LQ/SQ，然后复用 `commit_allocate()` 建立
active/enq。因为 `uses_lq/sq=0`，不会建立 LSQ map或修改 LSQ pointer/free count。

`release_lq/sq(count)` 用于真实 DUT deq：推进 dequeue pointer并恢复 free count。

`cancel_lq/sq(count)` 用于 redirect reissue：回退 enqueue pointer并恢复 free count。全局 redirect handler
先根据 active mapping 累计 `pending_lq_cancel_count/pending_sq_cancel_count`；LSQ admission sequence 在下一轮
candidate 前消费计数。pending-sample helper不直接释放资源，避免出现第二个 cancel owner。

## 7. 正确性边界

- candidate 阶段只预览，driver launch 成功后才调用 allocation owner。
- V2 launch reservation 与 issue-ready 分两个边界，但 pointer/free count 在 launch 后立即更新，保证下一
  packet 的预测 key连续且不重复。
- response wrapper、V2 confirm 和 non-LSQ 路径最终都复用 `commit_allocate()`，状态更新公式只有一个权威。
- 本轮 scalar setter只接受 `num_ls_elem=1`；vector/multi-element range map、逐 element deq 和 chunk progress
  仍是后续专项。
