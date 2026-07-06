# lsq_ctrl_model.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv`

## 1. 文件定位与使用场景

软件 LSQ 分配镜像。它存在的原因是 LSQ enqueue 接口由 DUT 返回 LQ/SQ index，但 TB 也需要提前知道“应该分到哪里”，这样才能填 req、校验 resp，并在 deq 时维护资源数。

输入是主表 transaction 和 DUT enqueue response。输出给 `common_data_transaction::activate_uid()`，同时更新软件 `lq/sq_enq_ptr`、free count 和主表中的 LQ/SQ key。

控制逻辑字段是 LQ/SQ enq/deq pointer 和 free count。`derive_op_behavior()` 返回的 behavior 也是控制核心，因为它决定当前操作是否需要分配 LSQ、进入哪些 issue queue。主表中的 fuType/fuOpType 是输入 payload，但会被这里解释成控制行为。

字段：

- `lq_enq_ptr/sq_enq_ptr`、`lq_deq_ptr/sq_deq_ptr`：软件 LQ/SQ 环形指针。
- `lq_free_count/sq_free_count`：剩余资源。
- `data`：公共数据单例。

函数：

- `reset()`：重置所有指针和 free count。
- `is_*_fuoptype()`：识别 load/prefetch/store/CBO/AMO/AMOCAS。
- `derive_op_behavior(tr)`：核心分类。LDU load/prefetch 需要 LQ 并 route load；STU store/CBO 需要 SQ 并 route STA/STD；MOU AMO 不分配 LSQ，但 route STA/STD，AMOCAS 按类型设置 `atomic_sta_uop_count/atomic_data_uop_count`。vector LS 直接 fatal。
- `advance_lq_key/advance_sq_key`、`rewind_lq_key/rewind_sq_key`：环形推进/回退。
- `can_allocate()`、`preview_allocate()`：检查和预览 LQ/SQ 分配。
- `commit_allocate()`：软件 smoke 路径使用预期 key 激活 uid。
- `commit_allocate_with_resp()`：真实 LSQ 路径用 DUT 返回 key 反校验软件预期，再激活 uid。
- `commit_non_lsq_admission()`：AMO 等不分配 LSQ 的 admission。
- `release_lq/release_sq()`：deq 后释放 free count。
- `cancel_lq/cancel_sq()`：回退 enq pointer，用于 redirect reissue 等取消场景。`common_data_transaction::prepare_uid_for_redirect_reissue()` 会累计待取消 LQ/SQ 数量，`memblock_lsqenq_dispatch_sequence::apply_pending_lsq_cancels()` 在恢复 admission 前调用这里回退软件 LSQ 镜像。

## 2. 字段与函数/task 设计原理

`lsq_ctrl_model` 是软件侧 LSQ 分配镜像，不是完整 LSQ RTL 模型。它只回答：这条操作是否占 LQ/SQ、占几个元素、预期 index 是什么、DUT deq 后软件 free count 如何变化。

关键字段：

| 字段 | 含义 | 设计原理 |
|---|---|---|
| `m_inst` | 单例句柄 | LSQ 指针必须全局唯一，不能每个 sequence 一份。 |
| `data` | 公共数据 owner | 分配成功后需要写回主表、状态表和 active map。 |
| `lq_enq_ptr/sq_enq_ptr` | 软件预期入队指针 | LSQ enqueue 前 preview，DUT resp 返回后比对。 |
| `lq_deq_ptr/sq_deq_ptr` | 软件预期出队指针 | DUT deq monitor 返回后检查释放顺序。 |
| `lq_free_count/sq_free_count` | 软件剩余资源 | 入队前判断是否有足够 LQ/SQ 资源，deq 后释放。 |

主要函数：

| 函数 | 参数 | 功能和设计原理 |
|---|---|---|
| `get()`、`reset()` | 无 | 获取单例并重置 LSQ 指针/free count，保证 testcase 从空 LSQ 开始。 |
| `is_vector_ls_futype(fuType)` | fuType | 当前简化模型不支持 vector LS，先识别并 fatal，避免误按 scalar 处理。 |
| `is_load_fuoptype()`、`is_store_fuoptype()`、`is_prefetch_fuoptype()`、`is_cbo_fuoptype()`、`is_amo_fuoptype()`、`is_amocas_*()` | fuOpType | 集中维护操作类型识别，主表生成、行为推导和校验共用同一套分类。 |
| `make_default_behavior()` | 无 | 返回全 0 安全默认行为，后续只打开需要的位，避免字段沿用旧值。 |
| `derive_op_behavior(tr)` | 主表 transaction | 把 fuType/fuOpType 转成 `memblock_op_behavior_t`，统一描述 need_alloc、uses_lq/sq、route_load/sta/std、commit 类型、atomic uop 数。 |
| `advance_lq_key/base`、`advance_sq_key/base`、`rewind_lq_key/base`、`rewind_sq_key/base` | 起始 key、步数 | 统一处理 LQ/SQ 环形指针前进和回退，release/cancel 都用同一规则。 |
| `can_allocate(behavior)` | op behavior | 根据 `uses_lq/uses_sq/num_ls_elem` 判断资源是否足够。 |
| `preview_allocate(behavior,lq_key,sq_key)` | behavior、输出 key | 在真实 enqueue 前预测 DUT 应返回的 LQ/SQ key。 |
| `commit_allocate(uid,behavior,tr)` | uid、behavior、主表 tr | software smoke 或非真实 resp 路径使用，直接按软件预期写回主表和 active map。 |
| `commit_allocate_with_resp(uid,behavior,tr,dut_lq_key,dut_sq_key)` | uid、behavior、主表 tr、DUT 返回 key | real LSQ 入队路径使用，先比对 DUT resp 与软件预期，再写回主表/状态表。 |
| `commit_non_lsq_admission(uid,behavior,tr)` | uid、behavior、主表 tr | AMO 等不分配普通 LQ/SQ 的路径仍可进入公共 active 生命周期。 |
| `release_lq/sq(count)`、`cancel_lq/sq(count)` | 元素数 | deq 时释放资源，flush/cancel 时回退 enqueue 资源。redirect reissue 中，公共数据层先累计 `pending_lq_cancel_count/pending_sq_cancel_count`，LSQ admission sequence 再调用 `cancel_lq/cancel_sq` 消费，确保同 uid 重入队时软件预测 key 和 DUT response key 仍一致。 |
