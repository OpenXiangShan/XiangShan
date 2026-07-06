# memblock_dispatch_types.sv 类型与字段分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`

该文件定义 dispatch 框架的公共“类型语言”：容量参数、DUT 编码常量、ROB/LQ/SQ key、主表抽象操作类型、发射目标、writeback 事件、状态字段和发射队列轻量项。它本身不保存运行状态，也不直接驱动 DUT；真正的行为由 `lsq_ctrl_model`、`issue_queue_scheduler`、`writeback_status_handler`、`lsq_commit_handler` 等 helper 使用这些类型共同完成。

## 1. 使用阶段总览

| 阶段 | 主要使用的类型 | 作用 |
|---|---|---|
| 主表生成 | `memblock_uid_t`、`memblock_op_class_e`、`memblock_lsq_flow_e`、`MEMBLOCK_FUTYPE_*`、`MEMBLOCK_LSUOP_*` | 生成每条 transaction 的高层操作意图和合法 `fuType/fuOpType`。 |
| LSQ 入队 | `memblock_op_behavior_t`、`memblock_rob_key_t`、`memblock_lq_key_t`、`memblock_sq_key_t` | 判断是否需要 LQ/SQ，分配并记录 `robIdx/lqIdx/sqIdx`。 |
| TLB 查表 | `memblock_tlb_lookup_key_t` | 用 runtime CSR snapshot 和 DTLB request 信息构造 `{vpn, asid, vmid, s2xlate}` 查表 key。 |
| 发射队列 | `memblock_issue_target_e`、`memblock_issue_q_item_t` | 把一个 uid 拆成 LOAD/STA/STD 目标队列项，控制 ready delay、优先级和 target-aware replay 序号。 |
| 地址复用 | `memblock_addr_reuse_kind_e` | 随机主表生成期选择当前 transaction 要按哪类 load/store after 场景复用窗口内地址。 |
| 流水线反馈 | `memblock_wb_event_source_e`、`memblock_wb_event_t` | 把 monitor 采集到的 writeback、fault、replay、redirect 统一成事件。 |
| 公共进度 | `memblock_dispatch_progress_t` | 记录连续 terminal_done 前缀和连续有效 LSQ admission 高水位，给 admission、route、redirect flush 共享扫描边界。 |
| 状态更新 | `memblock_status_field_e` | 给 `common_data_transaction` 提供统一状态字段 id。 |
| commit/deq/retire | `memblock_op_behavior_t`、ROB/LQ/SQ key、status field | 判断 required target 是否完成，等待 ROB commit 和 LQ/SQ deq 后 retire。 |

## 2. 容量与位宽参数

| 字段 | 含义 | 使用场景和约束 |
|---|---|---|
| `MEMBLOCK_ROB_SIZE = 352` | 当前模型/DUT 期望的 ROB 槽位数量。 | ROB 指针推进、年龄比较、active ROB map、redirect flush 判断都要以该容量为边界。 |
| `MEMBLOCK_LQ_SIZE = 72` | 当前模型/DUT 期望的 LQ 槽位数量。 | load/prefetch 入队资源检查、`lqIdx` 分配、DUT `lqDeq/cancel` 释放都依赖它。 |
| `MEMBLOCK_SQ_SIZE = 56` | 当前模型/DUT 期望的 SQ 槽位数量。 | store/CBO 入队资源检查、`sqIdx` 分配、DUT `sqDeq/cancel` 释放都依赖它。 |
| `MEMBLOCK_COMMIT_WIDTH = 8` | 每拍最多向 LSQ commit 侧推进的 ROB commit 数。 | `lsq_commit_handler::select_rob_commit_batch()` 用它限制 commit batch。 |
| `MEMBLOCK_ROB_VALUE_W = 9` | `robIdx.value` 字段位宽，不含 wrap `flag`。 | 9 bit 只表示编码空间可到 `0..511`，真实合法值仍必须小于 `MEMBLOCK_ROB_SIZE`。 |
| `MEMBLOCK_LQ_VALUE_W = 7` | `lqIdx.value` 字段位宽，不含 wrap `flag`。 | 7 bit 只表示编码空间可到 `0..127`，真实合法值仍必须小于 `MEMBLOCK_LQ_SIZE`。 |
| `MEMBLOCK_SQ_VALUE_W = 6` | `sqIdx.value` 字段位宽，不含 wrap `flag`。 | 6 bit 只表示编码空间可到 `0..63`，真实合法值仍必须小于 `MEMBLOCK_SQ_SIZE`。 |

关键约束：

- `MEMBLOCK_*_VALUE_W` 是接口 value 位宽，`MEMBLOCK_*_SIZE` 是真实容量，二者不能混用。
- 环形指针比较不能只比较 `value`。`flag/value` 必须作为完整 key 使用。
- 指针从最后一个合法槽位回到 0 时需要翻转 `flag`。
- ROB 顺序判断、redirect flush、commit pointer 推进、active map lookup 都必须使用公共 key helper，不能手写裸 value 比较。

## 3. FuType 与 LSUOpType 常量

### 3.1 `MEMBLOCK_FUTYPE_*`

| 字段 | 含义 | 使用场景 |
|---|---|---|
| `MEMBLOCK_FUTYPE_LDU` | 标量 load/prefetch 功能单元类型。 | `derive_op_behavior()` 识别后会设置 `uses_lq=1`、`route_load=1`、`commit_is_load=1`。 |
| `MEMBLOCK_FUTYPE_STU` | 标量 store/CBO 功能单元类型。 | `derive_op_behavior()` 识别后会设置 `uses_sq=1`、`route_sta=1`、`route_std=1`、`commit_is_store=1`。 |
| `MEMBLOCK_FUTYPE_MOU` | atomic/MOU 功能单元类型。 | 当前简化模型识别为 atomic，路由到 STA/STD，但不分配 LQ/SQ。 |
| `MEMBLOCK_FUTYPE_VLDU` | vector load 功能单元类型。 | 当前初版 `lsq_ctrl_model` 不完整支持 vector LS，命中会 fatal。 |
| `MEMBLOCK_FUTYPE_VSTU` | vector store 功能单元类型。 | 当前初版不完整支持，默认不应生成。 |
| `MEMBLOCK_FUTYPE_VSEGLDU` | vector segment load 功能单元类型。 | 当前初版不完整支持，默认不应生成。 |
| `MEMBLOCK_FUTYPE_VSEGSTU` | vector segment store 功能单元类型。 | 当前初版不完整支持，默认不应生成。 |

约束关系：

- `fuType` 不能和 `fuOpType` 独立完全随机。
- `LDU` 只能搭配 load/prefetch 类 `fuOpType`。
- `STU` 只能搭配 store/CBO 类 `fuOpType`。
- `MOU` 只能搭配 AMO 类 `fuOpType`。
- 当前最小合法模块约束应由 op template 或 `derive_op_behavior()` 统一检查。

### 3.2 `MEMBLOCK_LSUOP_*`

| 类别 | 字段 | 含义 | 使用场景 |
|---|---|---|---|
| load | `LB/LH/LW/LD/LBU/LHU/LWU` | 标量整数/浮点 load 共用的 LSU op 编码集合。 | 搭配 `fuType=LDU`，进入 load queue 和 load pipe。是否写 int/fp RF 由 `rfWen/fpWen` 控制，不由这些编码单独决定。 |
| store | `SB/SH/SW/SD` | 标量 store op 编码集合。 | 搭配 `fuType=STU`，拆入 STA/STD 队列。 |
| prefetch | `PREFETCH_I/PREFETCH_R/PREFETCH_W` | prefetch op 编码集合。 | 搭配 `fuType=LDU`，按 load-like admission 处理，占 LQ，但通常不写 RF。 |
| CBO | `CBO_ZERO/CBO_CLEAN/CBO_FLUSH/CBO_INVAL` | cache block operation 编码集合。 | 搭配 `fuType=STU`，行为更接近 store-like SQ 流程，但 completion/异常语义需要按 CBO 专项处理。 |
| AMO W | `LR_W/SC_W/AMOSWAP_W/.../AMOCAS_W` | word 宽度 atomic 编码。 | 搭配 `fuType=MOU`，当前简化模型按 atomic 流程进入 STA/STD。 |
| AMO D | `LR_D/SC_D/AMOSWAP_D/.../AMOCAS_D` | doubleword 宽度 atomic 编码。 | 搭配 `fuType=MOU`，当前简化模型按 atomic 流程进入 STA/STD。 |
| AMO Q | `AMOCAS_Q` | quadword AMOCAS 编码。 | AMOCAS Q 会影响 atomic 拆分 uop 计数。 |
| AMOCAS low bits | `AMOCAS_W_LO/D_LO/Q_LO` | AMOCAS 低位匹配常量。 | 用于识别 AMOCAS 变体并决定 atomic STA/STD uop 拆分数量。 |

## 4. 基础索引类型

### 4.0 `memblock_dispatch_progress_t`

`memblock_dispatch_progress_t` 是 10 万笔 dispatch 优化的公共进度结构。

| 字段 | 含义 | 使用场景 |
|---|---|---|
| `terminal_done_uid` | 从 0 开始连续 `terminal_done=1` 后的第一个 uid。 | route、redirect flush、debug 扫描都从这里开始，不能跨过未进入终态的 flushed/redirect_pending/replay uid。 |
| `max_enqueued_uid` | 当前连续有效 LSQ admission 高水位。 | LSQ admission 下一条 uid 为 `max_enqueued_uid + 1`；redirect flush 后回退到最老 flushed uid 的前一个 uid。 |
| `max_enqueued_uid_valid` | `max_enqueued_uid` 是否有效。 | 还没有 uid admission 成功时为 0，此时 active scan end 等于 success prefix。 |

该结构只描述 TB 公共进度，不是 DUT 接口字段。是否被 redirect flush 仍必须用 ROB 顺序 helper 判断，不能用 uid 大小直接判断。

### 4.1 `memblock_uid_t`

`memblock_uid_t` 是 TB 内部主键，类型为 `int unsigned`。

| 使用阶段 | 作用 |
|---|---|
| 主表生成 | 每条主表 transaction 分配一个不回绕 uid。 |
| 入队 | 用 uid 索引主表、状态表和 TLB 表。 |
| 发射 | issue queue item 只保存 uid 和少量快照，实际 transaction 内容通过 uid 从公共表读取。 |
| 回写/异常 | monitor event 可直接携带 uid；如果没有 uid，则通过 ROB/LQ/SQ active map 反查 uid。 |
| retire | `success` 后释放 active map，但主表仍保留用于追溯。 |

### 4.2 `memblock_rob_key_t`、`memblock_lq_key_t`、`memblock_sq_key_t`

这三个 struct 都是 `{flag, value}` 形式的环形指针 key。

| 类型 | 字段 | 含义 | 使用场景 |
|---|---|---|---|
| `memblock_rob_key_t` | `flag`、`value[ROB_W-1:0]` | ROB wrap flag 和槽位 value。 | ROB active map、redirect flush、commit pointer、writeback event 反查。 |
| `memblock_lq_key_t` | `flag`、`value[LQ_W-1:0]` | LQ wrap flag 和槽位 value。 | load/prefetch active map、load writeback event 反查、LQ deq 释放。 |
| `memblock_sq_key_t` | `flag`、`value[SQ_W-1:0]` | SQ wrap flag 和槽位 value。 | store/CBO active map、store/STD event 反查、SQ deq 释放。 |

使用约束：

- `value` 表示槽位号，`flag` 表示回绕代际。
- 相同 `value` 在不同 `flag` 下不是同一条 active transaction。
- 入队写回主表和状态表时必须保存完整 key。
- event 反查时如果 ROB/LQ/SQ 三种 key 同时存在，解析出的 uid 必须一致，否则 fatal。

### 4.3 `memblock_rob_map_key_t`、`memblock_lq_map_key_t`、`memblock_sq_map_key_t`

这三个类型是 `{flag, value}` 的 packed 关联数组 key。

| 类型 | 用途 |
|---|---|
| `memblock_rob_map_key_t` | `active_rob_map` 的 key，用于 ROB event 反查 uid。 |
| `memblock_lq_map_key_t` | `active_lq_map` 的 key，用于 LQ event 和 deq 反查 uid。 |
| `memblock_sq_map_key_t` | `active_sq_map` 的 key，用于 SQ event 和 deq 反查 uid。 |

它们不是 DUT 字段，只是 TB 内部查表格式。

在 TB 环境中的实际使用方式：

```systemverilog
uid_by_active_rob[memblock_rob_map_key_t] = uid;
uid_by_lq[memblock_lq_map_key_t]          = uid;
uid_by_sq[memblock_sq_map_key_t]          = uid;
```

`map_key` 的生成统一走 `rob_order_util`：

```systemverilog
rob_to_map_key(key) = {key.flag, key.value};
lq_to_map_key(key)  = {key.flag, key.value};
sq_to_map_key(key)  = {key.flag, key.value};
```

也就是说，`memblock_rob/lq/sq_map_key_t` 是把 struct 形式的 `{flag,value}` 压成 packed bit 后作为 SystemVerilog 关联数组索引。这样做的目的不是新增一套索引，而是让 monitor 事件中的 DUT 指针可以快速反查到 TB 内部唯一 `uid`。

生命周期：

| 阶段 | 使用方式 |
|---|---|
| 表清空/新 testcase 开始 | `common_data_transaction::reset_all_tables()` 删除 `uid_by_active_rob`、`uid_by_lq`、`uid_by_sq`。 |
| LSQ/admission 成功 | `common_data_transaction::activate_uid()` 从主表读取 `robIdx/lqIdx/sqIdx`，转成 map key 后建立 `map_key -> uid`。所有 active uid 都建立 ROB map；只有 `behavior.uses_lq=1` 时建立 LQ map；只有 `behavior.uses_sq=1` 时建立 SQ map。 |
| monitor feedback 到来 | `common_data_transaction::resolve_uid_for_event()` 根据 event 中有效的 ROB/LQ/SQ key 调 `lookup_active_uid_by_rob/lq/sq()` 反查 uid。 |
| 事件一致性检查 | 如果 event 同时携带 `uid`、ROB key、LQ key、SQ key，反查出的 uid 必须一致，否则 `WB_UID_MISMATCH` fatal。 |
| LQ/SQ deq | `release_uid_lq_mapping()` / `release_uid_sq_mapping()` 根据状态表保存的 LQ/SQ key 删除对应 active map，并更新 `active_lq_mapped/active_sq_mapped`。 |
| retire 或 flush | `retire_active_uid()` 删除 ROB map，并删除仍 active 的 LQ/SQ map，最后清 `status.active`。 |
| end check | `end_test_check()` 要求 `uid_by_active_rob`、`uid_by_lq`、`uid_by_sq` 最终都为空。 |

三个 map 的区别：

| map | 建立条件 | 主要用途 | 释放点 |
|---|---|---|---|
| `uid_by_active_rob[memblock_rob_map_key_t]` | 所有 active uid 都建立。 | ROB writeback、redirect、memory violation、通用 event 反查 uid。 | `retire_active_uid()`。 |
| `uid_by_lq[memblock_lq_map_key_t]` | load/prefetch 等 `uses_lq=1` 的 uid 建立。 | load writeback event、LQ deq、带 LQ key 的 feedback 反查 uid。 | `release_uid_lq_mapping()` 或 `retire_active_uid()`。 |
| `uid_by_sq[memblock_sq_map_key_t]` | store/CBO 等 `uses_sq=1` 的 uid 建立。 | store/STD/SQ feedback、SQ deq、带 SQ key 的 feedback 反查 uid。 | `release_uid_sq_mapping()` 或 `retire_active_uid()`。 |

为什么不能只用 `value`：

- ROB/LQ/SQ 都是环形队列，同一个 `value` 会在回绕后再次出现。
- `value=3` 可能是上一轮还没清完的 entry，也可能是回绕后的新 entry。
- 只有 `{flag,value}` 才能区分不同代际，避免旧 writeback、旧 replay 或旧 deq 错配到新的 transaction。
- 因此所有 active map lookup 都必须使用 `rob_to_map_key/lq_to_map_key/sq_to_map_key()`，不能手写裸 `value` 作为关联数组 key。

## 5. redirect 与 TLB lookup key

### 5.1 `memblock_redirect_payload_t`

| 字段 | 含义 | 使用场景 |
|---|---|---|
| `valid` | redirect payload 是否有效。 | `request_redirect_flush()` 和 `apply_redirect_flush()` 要求为 1。 |
| `flush_itself` | redirect 点自身是否也需要 flush。 | `rob_need_flush()` 判断 flush 范围时使用。 |
| `rob_key` | redirect 发生位置的 ROB key。 | 用于决定 active transaction 是否在 redirect 之后，需要清理或保留。 |

生命周期：

- monitor 采集 redirect/memory violation 后填入 `memblock_wb_event_t.redirect`。
- writeback handler 将 redirect 事件推入 feedback queue。
- exception/redirect handler 请求 issue freeze，并在安全边界调用 flush。
- `common_data_transaction::apply_redirect_flush()` 根据 ROB 顺序清理受影响 uid。

### 5.2 `memblock_tlb_lookup_key_t`

| 字段 | 含义 | 来源与使用场景 |
|---|---|---|
| `vpn[51:0]` | 虚拟页号。 | 来自 DTLB/L2TLB request 采集。 |
| `asid[15:0]` | 地址空间 id。 | 来自 runtime CSR snapshot，不应只读初始 CSR。 |
| `vmid[15:0]` | 虚拟机 id。 | 来自 runtime CSR snapshot，不应只读初始 CSR。 |
| `s2xlate[1:0]` | stage2 translation 模式/请求属性。 | 优先从 DTLB request 或 runtime CSR 相关状态确定。 |

使用约束：

- TLB 表不能只用 VPN 做唯一索引。
- L2TLB responder 查表应先构造 `{vpn, asid, vmid, s2xlate}`，再直接查 `tlb_entry_by_key` 得到 live TLB entry。
- uid 只用于发射上下文追踪和 PTE 回填记录，不作为 TLB entry 主索引，也不建立 `key -> uid` 强绑定。
- 所有基于 CSR 的 lookup 条件都应使用 runtime CSR snapshot，而不是 `csr_csr_common.sv` 中的初始值。

### 5.3 `memblock_sfence_payload_t`

`memblock_sfence_payload_t` 是公共数据层内部使用的 sfence/hfence 失效 payload。fence monitor 采到的是接口 raw 字段，公共数据层先把 raw 字段 decode 成这个 payload，再遍历 `tlb_entry_by_key` 做 entry 级删除。

| 字段 | 含义 | 来源与使用场景 |
|---|---|---|
| `valid` | 当前 payload 是否有效。 | 来自 `io_ooo_to_mem_sfence_valid`。无效 payload 不做任何删除。 |
| `ignore_addr` | 是否忽略地址匹配。 | 由 raw `rs1` decode 得到；为 0 时按 `addr` 和 entry `level` 匹配 VPN tag。 |
| `ignore_id` | 是否忽略 ASID/VMID id 匹配。 | 由 raw `rs2` decode 得到；为 0 时普通 sfence/hfence_v 检查 ASID，hfence_g 检查 VMID。 |
| `addr[49:0]` | fence 指定的虚拟地址。 | `ignore_addr=0` 时参与 `sfence_vpn_match()`。 |
| `id[15:0]` | fence 指定的 ASID 或 VMID。 | 普通 sfence/hfence_v 作为 ASID 使用，hfence_g 作为 VMID 使用。 |
| `hv` | 当前 fence 是否按 `hfence_v` 语义处理。 | 用于匹配 VS/G-stage 相关 entry；`g` 位只保护 ASID 精确 flush。 |
| `hg` | 当前 fence 是否按 `hfence_g` 语义处理。 | 用于匹配 stage2/G-stage entry，忽略 ASID。当前实现若 `hv/hg` 同时为 1，优先按 `hg` 处理。 |
| `cycle` | monitor 采样时的 dispatch service cycle。 | 只用于日志和 debug，不参与匹配。 |

设计约束：

- `memblock_sfence_payload_t` 不保存 uid，因为 sfence/hfence 的真实失效对象是 TLB entry cache，而不是某一条 transaction。
- 删除动作只作用于 `tlb_entry_by_key`；`uid_tlb_record_by_uid` 保留，用于已经发射 uid 的历史追踪。
- 当前 level 匹配函数支持按 `entry.level` 比较 VPN tag；默认 builder 主要生成 `level=0`，大页 directed 覆盖仍需要后续补强 testcase。

## 6. 操作类型枚举

### 6.1 `memblock_op_class_e`

`op_class` 是主表生成层使用的高层操作类别，用于让 directed/random 表不直接手写所有 `fuType/fuOpType` 组合。

| 枚举值 | 含义 | 入队 | 进入流水线 | 出流水线 | 出整个流程 |
|---|---|---|---|---|---|
| `MEMBLOCK_OP_CLASS_UNKNOWN` | 默认/非法占位。 | 不应入队。 | 不应发射。 | 不应有反馈。 | 有效主表中出现应 fatal。 |
| `MEMBLOCK_OP_CLASS_INT_LOAD` | 整数 load。 | `fuType=LDU`，分配 LQ。 | 进入 LOAD queue/pipe。 | 等 load writeback/pass。 | ROB commit 后等 LQ deq retire。 |
| `MEMBLOCK_OP_CLASS_FP_LOAD` | 浮点 load。 | 与 INT load 同样分配 LQ。 | 进入 LOAD queue/pipe。 | 等 load writeback/pass；`fpWen` 决定后端写 FP RF。 | ROB commit 后等 LQ deq retire。 |
| `MEMBLOCK_OP_CLASS_STORE` | store。 | `fuType=STU`，分配 SQ。 | 拆成 STA/STD queue/pipe。 | STA/STD 两侧完成后整体 pass。 | ROB commit 后等 SQ deq retire。 |
| `MEMBLOCK_OP_CLASS_PREFETCH` | prefetch。 | `fuType=LDU`，按 load-like 流程占 LQ。 | 进入 LOAD queue/pipe。 | 等 prefetch 对应 pass/fault，不写 RF。 | 按 load commit/LQ deq 资源释放。 |
| `MEMBLOCK_OP_CLASS_AMO` | atomic。 | `fuType=MOU`，当前简化模型不分配 LQ/SQ。 | 进入 atomic 使用的 STA/STD 路径。 | 等 atomic pass/fault；如后续建模 replay，只按 LOAD/STA replay 语义处理，不把 STD 作为真实 backend replay 源。 | 当前按 normal ROB commit，不等 LQ/SQ deq。 |

### 6.2 `memblock_lsq_flow_e`

`lsq_flow` 是主表里的抽象 LSQ 流程类型，用于表达这条 transaction 应该按哪类 LSQ/admission 生命周期处理。它不是 DUT 端口字段；DUT 真实 payload 仍是 `fuType/fuOpType/robIdx/lqIdx/sqIdx` 等字段。

当前代码中，最终资源分配、队列路由和 commit 行为主要由 `lsq_ctrl_model::derive_op_behavior()` 根据 `fuType/fuOpType` 推导；`lsq_flow` 主要用于模板一致性检查、directed testcase 可读性和后续方案描述。

| 枚举值 | 入队 | 进入流水线 | 出流水线 | 出整个流程 |
|---|---|---|---|---|
| `MEMBLOCK_LSQ_FLOW_NONE` | 默认/无效占位，不应进入正常入队。 | 不应进入 LOAD/STA/STD 任一队列。 | 不应有 writeback/pass。 | 不参与 commit/deq；有效主表保持该值应报错。 |
| `MEMBLOCK_LSQ_FLOW_LOAD` | 分配 `lqIdx`，占用 LQ；`sqIdx` 作为当前 store 边界信息携带。 | 进入 load issue queue，发往 load pipe。 | 等待 load writeback/pass；异常、replay、redirect 会更新状态表。 | 走 load commit 语义，ROB commit 后还要等 DUT `lqDeq/cancel` 释放 LQ，之后 retire。 |
| `MEMBLOCK_LSQ_FLOW_STORE` | 分配 `sqIdx`，占用 SQ；同时携带当前 `lqIdx` 边界。 | 拆成 STA queue 和 STD queue，分别发往地址流水线和数据流水线。 | STA/STD 两侧都需要完成；STA replay 只影响 STA target，STD 只作为 pass/fault/完成反馈，不建真实 backend replay。 | 走 store commit 语义，ROB commit 后等 DUT `sqDeq/cancel` 释放 SQ，之后 retire。 |
| `MEMBLOCK_LSQ_FLOW_ATOMIC` | 当前简化模型不分配 LQ/SQ，按 MOU/atomic 流程处理。 | 进入 STA/STD 相关路径；AMOCAS 可能拆成多个地址/数据 uop。 | 等 atomic 对应 writeback/pass；异常按状态表处理，replay 语义只按可 replay 的 LOAD/STA target 建模。 | 当前按 normal ROB commit，不走 `lcommit/scommit`，也不等 LQ/SQ deq。 |
| `MEMBLOCK_LSQ_FLOW_CBO` | 语义上接近 store 类，占用 SQ；当前代码主要通过 `fuType=STU + CBO fuOpType` 识别。 | 通常走 STA/STD 路径。 | 等 CBO/store 侧反馈，不应按普通 load 写回理解。 | 更接近 store commit/SQ release 流程；后续应明确模板是否直接使用 `lsq_flow=CBO`。 |

实现注意：

- 当前 `memblock_op_class_e` 没有单独的 CBO op class，`MEMBLOCK_LSQ_FLOW_CBO` 更像预留的语义标签。
- 如果后续 directed 主表允许直接配置 CBO，需要同步补充 op template 和 `check_main_transaction()` 检查规则。
- 如果保持 CBO 仍归入 store op class，则可以继续用 `fuOpType` 区分 CBO，不必强制让 `lsq_flow` 变成 CBO。

### 6.3 `memblock_addr_reuse_kind_e`

`memblock_addr_reuse_kind_e` 是主表随机生成期的地址复用场景枚举。它不是 DUT 接口字段，也不会写入 transaction；它只在 `memblock_dispatch_base_sequence::apply_addr_reuse_window()` 里用于决定当前 transaction 最终修正成 load 还是 store，以及从 recent load queue 还是 recent store queue 中挑参考地址。

| 枚举值 | 当前项最终类型 | 参考候选 | 默认权重 | 用途 |
|---|---|---|---|---|
| `MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE` | load | 窗口内历史 store | `1` | 构造前序 store、后续 load 同地址，提升 RAW/违例/转发相关场景概率。 |
| `MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD` | load | 窗口内历史 load | `0` | 构造 load-load 同地址复用，默认关闭，后续可按 testcase 打开。 |
| `MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD` | store | 窗口内历史 load | `1` | 构造前序 load、后续 store 同地址，增加地址相关覆盖。 |
| `MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE` | store | 窗口内历史 store | `0` | 构造 store-store 同地址复用，默认关闭，后续可按 testcase 打开。 |

使用流程：

1. `build_random_main_table()` 先随机生成基础 transaction。
2. `prune_recent_uid_q()` 按 uid 距离窗口淘汰过期 load/store 候选。
3. `apply_addr_reuse_window()` 按 `MEMBLOCK_ADDR_REUSE_EN_*` 决定是否启用本条复用，再按本枚举的四类权重选择场景。
4. `set_transaction_ls_kind()` 根据枚举把当前项修正成 load 或 store，并重新套合法 `fuType/fuOpType/lsq_flow/numLsElem`。
5. `fixup_after_addr_reuse()` 在选到参考项时复制 `src_0/imm`，随后重算 `vaddr` 并校验。
6. 当前项写入主表后，`push_recent_uid()` 按最终类型把 uid 放回 recent load/store queue，供后续 uid 使用。

fallback 规则：

- `LOAD_AFTER_STORE` 找不到 store 候选时，当前项按 store fallback，不复制地址。
- `LOAD_AFTER_LOAD` 找不到 load 候选时，当前项保持 load，不复制地址。
- `STORE_AFTER_LOAD` 找不到 load 候选时，当前项按 load fallback，不复制地址。
- `STORE_AFTER_STORE` 找不到 store 候选时，当前项保持 store，不复制地址。

为什么要放在公共 types 文件：

- 该枚举被 base sequence、源码分析文档和网页 callgraph 共同引用。
- 四类场景的编码必须稳定，避免 plus 权重顺序和 `case` 分支解释不一致。
- 后续若新增地址复用场景，应先扩展该枚举，再同步 `select_addr_reuse_kind()` 和 review/web 文档。

### 6.4 `memblock_issue_target_e`

| 枚举值 | 含义 | 使用场景 |
|---|---|---|
| `MEMBLOCK_ISSUE_TARGET_NONE` | 无有效发射目标。 | 默认值、redirect 事件、无 target 事件。 |
| `MEMBLOCK_ISSUE_TARGET_LOAD` | load 发射目标。 | load/prefetch 进入 `load_issue_q`，最终驱动 load pipe。 |
| `MEMBLOCK_ISSUE_TARGET_STA` | store address 发射目标。 | store/CBO/atomic 的地址侧进入 `sta_issue_q`。 |
| `MEMBLOCK_ISSUE_TARGET_STD` | store data 发射目标。 | store/CBO/atomic 的数据侧进入 `std_issue_q`。 |

阶段行为：

- 入队后，`issue_queue_scheduler::route_uid()` 根据 `memblock_op_behavior_t.route_*` 生成对应 target item。
- 发射时，LOAD/STA/STD 三类队列可以并行仲裁。
- writeback/pass/fault 状态按 target 分开记录，避免 store 的 STA 和 STD 互相覆盖。

## 7. 操作行为与反馈事件关系总览

`memblock_op_behavior_t`、`memblock_wb_event_t`、`memblock_wb_event_source_e` 三者经常一起出现，但职责不同：

| 类型 | 核心作用 | 主要使用阶段 | 典型使用者 |
|---|---|---|---|
| `memblock_op_behavior_t` | 描述一条 transaction 应该怎么走 LSQ、issue 和 commit 流程。 | 入队前、发射路由前、commit 前。 | `lsq_ctrl_model`、`issue_queue_scheduler`、`lsq_commit_handler`。 |
| `memblock_wb_event_t` | 描述 DUT monitor 或框架反馈回来的一次完成、异常、replay 或 redirect 事件。 | writeback/feedback/replay/redirect 处理阶段。 | `dispatch_monitor_event_adapter`、`writeback_status_handler`、`exception_redirect_replay_handler`。 |
| `memblock_wb_event_source_e` | 标记 `memblock_wb_event_t.source`，说明反馈事件来自哪里。 | 构造和分类 `memblock_wb_event_t` 时。 | monitor adapter、software smoke、issue accept pass、writeback/recovery handler。 |

一句话区分：

```text
memblock_op_behavior_t       = 这条指令应该怎么走
memblock_wb_event_t          = DUT/框架反馈回来发生了什么
memblock_wb_event_source_e   = 这个反馈事件来自哪里
```

### 7.1 前向控制：`memblock_op_behavior_t`

`memblock_op_behavior_t` 是前向调度控制结果。它由 `lsq_ctrl_model::derive_op_behavior()` 根据主表中的 `fuType/fuOpType` 推导，后续不再让每个 sequence 自己重新解码 FU 类型。

它主要回答这些问题：

- 是否需要 LQ/SQ 分配：`uses_lq`、`uses_sq`、`need_alloc`。
- 是否进入 LOAD/STA/STD 发射队列：`route_load`、`route_sta`、`route_std`。
- commit 时按 load、store 还是 normal 处理：`commit_is_load`、`commit_is_store`、`commit_is_normal`。
- 是否属于 prefetch、CBO、atomic 特殊类型。
- 本条操作需要几个 LSQ element 或 atomic uop。

典型例子：

| 操作 | 关键 behavior |
|---|---|
| 普通 load | `uses_lq=1`、`route_load=1`、`commit_is_load=1`、`num_ls_elem=1`。 |
| 普通 store | `uses_sq=1`、`route_sta=1`、`route_std=1`、`commit_is_store=1`、`num_ls_elem=1`。 |
| 当前简化 atomic | `uses_lq=0`、`uses_sq=0`、`route_sta=1`、`route_std=1`、`commit_is_normal=1`。 |

### 7.2 反馈事件：`memblock_wb_event_t`

`memblock_wb_event_t` 是统一反馈事件结构。DUT monitor 采集到的 load writeback、store feedback、IQ feedback、redirect、memory violation，或者框架内部构造的 synthetic pass/replay，都会先变成这个结构，再进入统一 handler。

它主要回答这些问题：

- 这次事件是否有效：`valid`。
- 事件来自哪里：`source`。
- 事件作用到哪个 target：`target`，即 LOAD/STA/STD。
- 事件属于哪条 transaction：`uid` 或 ROB/LQ/SQ key。
- 是否是当前有效发射版本：`issue_epoch`、`replay_seq`。LOAD/STA 使用 replay_seq 过滤 stale event；STD 不用 replay_seq 作为失效条件。
- 是真实 writeback pass/fault、IQ feedback、replay 还是 redirect：`real_wb_valid`、`iq_feedback_*`、`has_exception/exception_vec`、`replay_valid`、`redirect_valid/redirect`。

`memblock_wb_event_t` 的处理结果最终落到 `status_transaction`：

- 正常 pass 更新 target writeback/pass。
- fault 更新 target fault 和 exception pending。
- LOAD/STA replay 清对应 target 的 dispatched/writeback/pass，并重新入队；STD 不建真实 backend replay。
- redirect 触发 freeze/flush/recovery，清理受影响 uid。

### 7.3 来源标签：`memblock_wb_event_source_e`

`memblock_wb_event_source_e` 本身不是完整事件，只是 `memblock_wb_event_t.source` 的枚举标签。它说明事件来源，例如：

- `MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB`：load writeback。
- `MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB`：atomic writeback 预留来源，当前公共路径不使用。
- `MEMBLOCK_WB_EVENT_SOURCE_STORE_WB`：store writeback。
- `MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK`：STA issue feedback。
- `MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK`：STD issue feedback。
- `MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION`：memory violation。
- `MEMBLOCK_WB_EVENT_SOURCE_BACKEND_REPLAY`：后端 replay 来源标签。
- `MEMBLOCK_WB_EVENT_SOURCE_REDIRECT`：redirect。

需要注意：`source` 只能说明“从哪里来”，不能单独决定最终动作。最终分类必须看显式行为字段：`real_wb_valid` 表示真实 writeback pass，`iq_feedback_*` 表示 IssueQueue response，`has_exception/exception_vec` 表示 fault，`replay_valid` 表示 replay，`redirect.valid` 表示 redirect，再结合 `target`、ROB/LQ/SQ key 和 active uid 解析结果。`redirect_valid` 是兼容/显式标志，必须与 canonical payload 位 `redirect.valid` 一致；分类 helper 检测到二者不一致时应 fatal。

### 7.4 两条主线

这三个类型可以按两条主线理解：

```text
主表 fuType/fuOpType
  -> memblock_op_behavior_t
  -> LSQ 分配 / issue queue 路由 / commit 候选判断

DUT monitor raw event 或框架 synthetic event
  -> memblock_wb_event_t.source = memblock_wb_event_source_e
  -> memblock_wb_event_t
  -> pass / fault / replay / redirect / flush 状态更新
```

因此：

- `memblock_op_behavior_t` 影响 transaction 进入哪条路径。
- `memblock_wb_event_t` 影响 transaction 当前状态如何被反馈推进。
- `memblock_wb_event_source_e` 辅助解释反馈事件来源，但不是完整控制决策。

## 8. writeback 事件来源枚举

### 8.1 `memblock_wb_event_source_e`

| 枚举值 | 含义 | target 关系 | 主要处理 |
|---|---|---|---|
| `MEMBLOCK_WB_EVENT_SOURCE_NONE` | 默认/无效来源。 | 通常 target 为 NONE。 | 不应触发状态变化。 |
| `MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB` | load writeback 来源。 | 通常 target 为 LOAD。 | 标记 load writeback/pass 或 fault。 |
| `MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB` | atomic writeback 预留来源。 | 后续 AMO/LR/SC 专项定义。 | reserved，当前公共路径不赋值、不使用。 |
| `MEMBLOCK_WB_EVENT_SOURCE_STORE_WB` | store writeback 来源。 | 可对应 STA 或 STD。 | 标记 store 地址/数据侧完成或 fault。 |
| `MEMBLOCK_WB_EVENT_SOURCE_SQ_WB` | SQ writeback/释放来源预留。 | 当前公共路径不赋值。 | reserved，SQ 释放当前走 ctrl deq/commit 路径。 |
| `MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK` | STA IssueQueue feedback。 | target 为 STA。 | `hit=1` 只表示 issue feedback success；没有 real STA writeback 时才作为兼容 pass 来源。 |
| `MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK` | STD IssueQueue feedback。 | target 为 STD。 | `hit=1` 只表示 issue feedback success；没有 real STD writeback 时才作为兼容 pass 来源。STD miss warning/drop。 |
| `MEMBLOCK_WB_EVENT_SOURCE_EXCEPTION_INFO` | ExceptionInfoGen 类来源预留。 | 当前公共路径不赋值。 | reserved，异常当前统一由 `has_exception/exception_vec` 表达。 |
| `MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION` | memory violation 来源。 | 通常 target 为 NONE。 | 必须构造 `redirect.valid=1` 的 payload，并保持 `redirect_valid == redirect.valid`，才触发 redirect。 |
| `MEMBLOCK_WB_EVENT_SOURCE_BACKEND_REPLAY` | 后端 replay 来源标签。 | 必须能定位 LOAD/STA target。 | 必须同时设置 `replay_valid=1` 才触发 replay，并重新入对应发射队列；STD target replay request 会被公共数据层 warning 后忽略。 |
| `MEMBLOCK_WB_EVENT_SOURCE_REDIRECT` | redirect 来源。 | target 通常为 NONE。 | 必须构造 `redirect.valid=1` 的 payload，并保持 `redirect_valid == redirect.valid`，才请求 freeze/flush。 |

处理约束：

- 非 redirect 事件必须有合法 target：LOAD、STA 或 STD。
- redirect/memory violation 可以没有 target，但必须能通过 ROB key 定位 flush 范围。
- replay/fault/pass 事件需要通过 uid 或 active ROB/LQ/SQ key 解析到 active uid。
- `MEMBLOCK_WB_EVENT_SOURCE_SQ_WB` 和 `MEMBLOCK_WB_EVENT_SOURCE_EXCEPTION_INFO` 当前不作为独立行为入口；枚举值保留，避免后续扩展或显式数值变化。

### 8.2 `memblock_wb_event_source_e` 的实际应用路径

`memblock_wb_event_source_e` 不是单独驱动状态机的字段，它是 `memblock_wb_event_t.source` 的来源标签。框架用它回答“这个 feedback event 从哪里来”，再结合 `target`、`real_wb_valid`、`iq_feedback_*`、`replay_valid`、canonical `redirect.valid`、`exception_vec` 等字段判断这次事件应该变成真实 pass、issue feedback success、fault、backend replay 还是 redirect。

整体路径如下：

| 阶段 | 文件/API | 如何使用 `source` |
|---|---|---|
| 事件初始化 | `common_data_transaction::make_empty_wb_event()` | 默认置 `MEMBLOCK_WB_EVENT_SOURCE_NONE`，表示当前 event 没有有效来源。 |
| monitor raw 转换 | `dispatch_monitor_event_adapter::*` | 根据 raw monitor 来源和 port，把事件标成 `LOAD_WB`、`STORE_WB`、`STA_FEEDBACK`、`STD_FEEDBACK`、`REDIRECT`、`MEMORY_VIOLATION`。 |
| software smoke 构造 pass | `soft_test_memblock_dispatch_smoke_sequence::make_pass_wb_event()` | 不依赖真实 monitor，按 issue target 手工构造 `LOAD_WB/STORE_WB`，并设置真实 writeback pass 语义。 |
| issue accept pass | `memblock_lintsissue_dispatch_sequence::make_issue_accept_pass_event()` | 对普通 store STD，在未开启真实 STD writeback pass 时，构造 `STD_FEEDBACK` IQ hit 兼容 pass。 |
| batch/writeback 分类 | `dispatch_monitor_batch_handler::process_monitor_event_batch()`、`writeback_status_handler::handle_real_writeback_event/handle_issue_feedback_event()` | batch handler 先处理 redirect 仲裁；writeback handler 只处理放行后的真实 writeback 或 IQ feedback。 |
| event normalize | `common_data_transaction::feedback_event_is_redirect/feedback_event_is_replay/feedback_event_has_action()` | 只按显式行为字段判断 event 是否需要处理，并在 normalize 时解析 active uid。 |
| recovery 分类 | `exception_redirect_replay_handler::event_is_redirect/event_is_replay()` | 从 feedback queue 中按 canonical `redirect.valid` 和 `replay_valid` 决定 flush 或重新入队；`redirect_valid` 不一致时 fatal。 |

当前源码中的赋值关系：

| `source` | 当前主要赋值位置 | 典型 target | 后续行为 |
|---|---|---|---|
| `MEMBLOCK_WB_EVENT_SOURCE_NONE` | `make_empty_wb_event()` | `NONE` | 默认空事件，不应触发状态变化。 |
| `MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB` | `dispatch_monitor_event_adapter::convert_raw_int_wb()` port 0/1/2；software smoke load pass | `LOAD` | 若 `real_wb_valid=1` 且无 exception/replay/redirect，更新 `LOAD_WRITEBACK/LOAD_PASS`；若有 exception，更新 load fault。 |
| `MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB` | 当前公共路径不赋值 | 后续 AMO/LR/SC 专项定义 | reserved 扩展点；当前公共路径不得用它驱动状态变化。 |
| `MEMBLOCK_WB_EVENT_SOURCE_STORE_WB` | `dispatch_monitor_event_adapter::convert_raw_int_wb()` port 3/4、5/6；software smoke store pass | `STA` 或 `STD` | 若正常，则更新对应 STA/STD writeback/pass；若 exception，则更新对应 target fault。 |
| `MEMBLOCK_WB_EVENT_SOURCE_SQ_WB` | 当前公共路径不赋值 | 后续 SQ 专项定义 | reserved；当前 SQ 释放/commit/deq 由 LSQ commit/control 路径表示。 |
| `MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK` | `convert_raw_iq_feedback()` 的 STA feedback | `STA` | `hit=1` 置 `iq_feedback_*`，真实 STA writeback pass 开启时只标记 issue feedback success；关闭时作为兼容 pass。`hit=0` 通过 `replay_valid` 触发 replay。 |
| `MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK` | `convert_raw_iq_feedback()` 的 STD feedback；issue accept pass | `STD` | `hit=1` 置 `iq_feedback_*`，真实 STD writeback pass 开启时只标记 issue feedback success；关闭时作为兼容 pass。STD miss warning/drop。 |
| `MEMBLOCK_WB_EVENT_SOURCE_EXCEPTION_INFO` | 当前公共路径不赋值 | 后续异常专项定义 | reserved；当前异常由 `has_exception/exception_vec` 表示。 |
| `MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION` | `dispatch_monitor_event_adapter::convert_raw_memory_violation()` | `NONE` | adapter 同时构造 `redirect.valid=1`、`level=memoryViolation.bits.level`，并按当前 RTL/Scala 语义用 `level(0)` 派生 `flush_itself`，后续按显式 redirect valid 走 flush。 |
| `MEMBLOCK_WB_EVENT_SOURCE_BACKEND_REPLAY` | 当前公共路径主要用于软件注入 LOAD/STA replay event，后续可接真实 backend replay 接口 | LOAD/STA | 只表示来源；必须同时设置 `replay_valid=1`，recovery handler 才会调用 `mark_replay_pending()` 并重新入队。STD 没有 MemBlock 执行后返回后端的 replay feedback 通路，STD replay request 会 warning 后返回 0。 |
| `MEMBLOCK_WB_EVENT_SOURCE_REDIRECT` | reserved；当前公共路径不由 redirect input monitor 赋值 | `NONE` | 保守保留来源枚举。`io_redirect_*` 是 TB 驱动 DUT 的 input 接口，不再回采后触发 recovery；当前 redirect recovery 来源是 `MEMORY_VIOLATION` 等 DUT output ctrl event。 |

应用规则：

- `source` 说明事件来源，但 normal pass 不只看 `source`。`writeback_status_handler::event_is_normal_pass()` 要求 `real_wb_valid=1`，且不是 redirect、不是 replay、没有 exception；IQ feedback hit 不设置这两个字段。
- replay 只由 `replay_valid=1` 标记。`MEMBLOCK_WB_EVENT_SOURCE_BACKEND_REPLAY` 只是软件注入/后续真实接口的来源标签，不能单独触发 replay。当前 IQ feedback 路径只有 STA `!hit` 会按 XiangShan IssueQueue 语义置 `replay_valid`；STD `!hit` 会丢弃/告警，不作为 backend replay 来源。`flush_state` 只作为 PTW-back/状态元信息，不单独触发 replay。
- redirect 只由 canonical `redirect.valid` 标记。`redirect_valid` 作为兼容/显式标志必须与 `redirect.valid` 一致，不一致应 fatal；`MEMORY_VIOLATION/REDIRECT` source 只是来源标签，不能单独触发 redirect，当前 adapter 会同步设置两个 valid 位。
- fault 不主要依赖 `source`，而是依赖 `has_exception || exception_vec != 0`。因此 `LOAD_WB/STORE_WB/STA_FEEDBACK/STD_FEEDBACK` 都可能在带异常时转成 fault。
- 非 redirect 事件必须有合法 `target`，因为最终状态更新是按 LOAD/STA/STD target 分字段写入的。
- `source`、`target`、ROB/LQ/SQ key 若同时存在，必须能解析到同一个 active uid；否则 normalize/resolve 阶段会丢弃或 fatal。

## 9. 操作行为派生类型

### 9.1 `memblock_op_behavior_kind_e`

| 枚举值 | 含义 | 来源 |
|---|---|---|
| `MEMBLOCK_OP_BEHAVIOR_UNKNOWN` | 默认/非法占位。 | 不应进入正常调度。 |
| `MEMBLOCK_OP_BEHAVIOR_LOAD` | 普通 load 行为。 | `fuType=LDU` 且 `fuOpType` 为 load。 |
| `MEMBLOCK_OP_BEHAVIOR_PREFETCH` | prefetch 行为。 | `fuType=LDU` 且 `fuOpType` 为 prefetch。 |
| `MEMBLOCK_OP_BEHAVIOR_STORE` | 普通 store 行为。 | `fuType=STU` 且 `fuOpType` 为 store。 |
| `MEMBLOCK_OP_BEHAVIOR_CBO` | CBO 行为。 | `fuType=STU` 且 `fuOpType` 为 CBO。 |
| `MEMBLOCK_OP_BEHAVIOR_ATOMIC` | atomic 行为。 | `fuType=MOU` 且 `fuOpType` 为 AMO。 |

### 9.2 `memblock_op_behavior_t`

`memblock_op_behavior_t` 是 `lsq_ctrl_model::derive_op_behavior()` 的返回结果。它把 `fuType/fuOpType` 解码成框架后续阶段可直接消费的控制位。

| 字段 | 含义 | 使用场景 |
|---|---|---|
| `kind` | 行为分类。 | debug、fatal 信息、专项分支。 |
| `need_alloc[1:0]` | 是否需要 LSQ admission 资源。 | `00` 表示非 LQ/SQ 分配；`01` 表示 LQ 类；`10` 表示 SQ 类。 |
| `uses_lq` | 是否占用并维护 active LQ map。 | load/prefetch 为 1。 |
| `uses_sq` | 是否占用并维护 active SQ map。 | store/CBO 为 1。 |
| `route_load` | 是否生成 LOAD queue item。 | load/prefetch 发射到 load pipe。 |
| `route_sta` | 是否生成 STA queue item。 | store/CBO/atomic 地址侧发射。 |
| `route_std` | 是否生成 STD queue item。 | store/CBO/atomic 数据侧发射。 |
| `commit_is_load` | commit 语义是否属于 load。 | load/prefetch 需要走 load commit/LQ deq。 |
| `commit_is_store` | commit 语义是否属于 store。 | store/CBO 需要走 store commit/SQ deq。 |
| `commit_is_normal` | commit 语义是否不依赖 LQ/SQ deq。 | 当前 atomic 简化模型为 1。 |
| `is_prefetch` | 是否 prefetch。 | 区分 load-like 但不写 RF 的路径。 |
| `is_cbo` | 是否 CBO。 | 区分 store-like 的 CBO completion/异常语义。 |
| `is_atomic` | 是否 atomic。 | 控制 atomic STA/STD uop 拆分和发射。 |
| `num_ls_elem[4:0]` | 本条操作占用的 LSQ 元素数。 | 标量 load/store 通常为 1，当前 atomic 为 0；vector 初版不支持。 |
| `atomic_sta_uop_count[2:0]` | atomic 地址侧 uop 数。 | AMOCAS Q/D/W 等会影响 STA 发射条数。 |
| `atomic_data_uop_count[2:0]` | atomic 数据侧 uop 数。 | AMOCAS Q/D/W 等会影响 STD 发射条数。 |

当前派生关系：

| `fuType/fuOpType` | behavior 结果 |
|---|---|
| `LDU + load` | `uses_lq=1`、`route_load=1`、`commit_is_load=1`、`num_ls_elem=1`、`kind=LOAD`。 |
| `LDU + prefetch` | `uses_lq=1`、`route_load=1`、`commit_is_load=1`、`num_ls_elem=1`、`kind=PREFETCH`。 |
| `STU + store` | `uses_sq=1`、`route_sta=1`、`route_std=1`、`commit_is_store=1`、`num_ls_elem=1`、`kind=STORE`。 |
| `STU + CBO` | `uses_sq=1`、`route_sta=1`、`route_std=1`、`commit_is_store=1`、`num_ls_elem=1`、`kind=CBO`。 |
| `MOU + AMO` | `uses_lq=0`、`uses_sq=0`、`route_sta=1`、`route_std=1`、`commit_is_normal=1`、`num_ls_elem=0`、`kind=ATOMIC`。 |

## 10. 发射队列项

### 10.1 `memblock_issue_q_item_t`

该结构是 LOAD/STA/STD 三个发射队列中的轻量项。它不复制完整 transaction，只保存调度和发射所需的最小信息；实际 payload 仍通过 `uid` 从主表读取。

| 字段 | 含义 | 使用场景 |
|---|---|---|
| `uid` | 主表和状态表索引。 | 发射前通过 uid 读取完整 transaction。 |
| `rob_key` | ROB key 快照。 | 仲裁、debug、driver 字段补充和 event 关联。 |
| `target` | 发射目标：LOAD/STA/STD。 | 决定进入哪个队列、哪个 driver port、哪个状态字段。 |
| `send_pri` | 当前 target 的发射优先级。 | `send_pri_mode_en=1` 时参与队列内 priority 比较；`sample_global_send_pri_en()` 命中时参与跨队列最大 priority 过滤；STD 使用 `send_pri_std`。 |
| `ready_cycle` | 软件 delay 剩余周期。 | 每拍递减，归零后才可被仲裁发射。 |
| `replay_seq` | replay 版本号快照。 | 防止旧 LOAD/STA 队列项或旧 LOAD/STA 反馈污染新一轮 replay；STD 旧队列项不因 STA bump replay_seq 失效。 |
| `has_lqIdx` | 是否携带有效 LQ key。 | load-like 发射和 event 反查使用。 |
| `lq_key` | LQ key 快照。 | 驱动 load pipe 的 `lqIdx`，或用于 debug/校验。 |
| `has_sqIdx` | 是否携带有效 SQ key。 | store-like 发射和 event 反查使用。 |
| `sq_key` | SQ key 快照。 | 驱动 STA/STD 的 `sqIdx`，或用于 debug/校验。 |
| `numLsElem` | LSQ 元素数快照。 | 发射字段和一致性检查使用。 |
| `uop_index` | 当前 issue item 本拍映射到 target 内第几个 pipe/port。 | `make_issue_item()` 中先清 0；真实发射前由 `assign_issue_items()` 用候选数组下标覆盖。LOAD 0/1/2 映射 `intIssue_0/1/2`，STA 0/1 映射 `intIssue_3/4`，STD 0/1 映射 `intIssue_5/6`；`mark_fired_items()` 用它匹配 `fired_mask`。 |
| `uop_count` | 当前 target 理论需要的 uop 数。 | 普通标量 load/store 为 1；atomic/AMOCAS 会记录地址侧或数据侧 uop 数。当前 issue queue 尚未按该字段展开多个 queue item，因此它是 atomic 多 uop 的记录/预留字段。 |

生命周期：

- `issue_queue_scheduler::route_target()` 创建 item 并 push 到对应队列。
- `advance_issue_queue_delays()` 每拍更新 `ready_cycle`。
- issue sequence 按 pipe 数和 `send_pri` 仲裁 eligible item。
- 发射成功后删除该 queue item，追溯依靠主表和状态表。
- LOAD/STA replay 会先清理同 uid/target 的旧项，再按新 `replay_seq` 重新入队。STA replay 不清同 uid 的 STD pending 项。

## 11. writeback/replay/redirect 事件

### 11.1 `memblock_wb_event_t`

该结构把不同 monitor 来源统一成一个反馈事件，使 writeback、IQ feedback、fault、backend replay、redirect 可以走同一套 uid 解析和状态更新入口。`source` 只用于 debug/追踪来源，状态机动作由 `real_wb_valid`、`iq_feedback_*`、`has_exception/exception_vec`、`replay_valid`、canonical `redirect.valid` 决定；`redirect_valid` 只作为必须与 payload valid 一致的兼容/显式标志。

| 字段 | 含义 | 使用场景 |
|---|---|---|
| `valid` | 事件是否有效。 | 无效事件直接丢弃。 |
| `source` | 事件来源。 | 只用于 debug/追踪来源，不单独决定 pass、fault、replay、redirect。 |
| `port_id` | 来源端口编号。 | debug 和多 pipe 事件定位。 |
| `target` | 对应 LOAD/STA/STD target。 | 非 redirect 事件必须合法，用于更新分 target 状态。 |
| `uid` | 事件直接携带的 uid。 | 如果 `has_uid=1`，优先用于解析。 |
| `has_uid` | uid 是否有效。 | 防止默认 uid=0 被误用。 |
| `rob_key` | 事件携带的 ROB key。 | 通过 active ROB map 反查 uid，redirect 也依赖它。 |
| `has_rob` | ROB key 是否有效。 | 控制是否参与 uid 解析。 |
| `lq_key` | 事件携带的 LQ key。 | load feedback 通过 active LQ map 反查 uid。 |
| `has_lq` | LQ key 是否有效。 | 控制是否参与 uid 解析。 |
| `sq_key` | 事件携带的 SQ key。 | store/SQ feedback 通过 active SQ map 反查 uid。 |
| `has_sq` | SQ key 是否有效。 | 控制是否参与 uid 解析。 |
| `issue_epoch` | 发射版本快照。 | 防止旧 feedback 更新新发射状态。 |
| `has_issue_epoch` | issue epoch 是否有效。 | 对 LOAD/STA，replay 之后若缺失 epoch，该事件会被丢弃或 warning；STD raw feedback 可在 `std_dispatched=1` 后由公共数据层补当前 `std_issue_epoch`。 |
| `replay_seq` | replay 序号快照。 | 防止旧 LOAD/STA replay/pass/fault 污染新 replay。 |
| `has_replay_seq` | replay seq 是否有效。 | replay 后 LOAD/STA 必须提供有效快照；STD event 主要依靠 issue_epoch 防迟到。 |
| `real_wb_valid` | 真实 int writeback/pass/fault 是否有效。 | 真实 writeback normal pass 的必要条件之一；IQ feedback 不写该字段。 |
| `iq_feedback_valid` | IssueQueue feedback 是否有效。 | STA/STD IQ feedback 或 issue-accept 兼容路径。 |
| `iq_feedback_hit` | IssueQueue feedback finalSuccess/hit。 | 真实 writeback pass 开启时只标记 issue feedback success，关闭时可兼容转 pass。 |
| `iq_feedback_failed` | IssueQueue feedback failed。 | STA failed 转 replay，STD failed warning/drop。 |
| `iq_feedback_flush_state` | IQ feedback flush_state 原始状态位。 | PTW/TLB back replay 等调试和派生语义来源；不代表真实 writeback。 |
| `has_exception` | 是否携带异常。 | 与 `exception_vec` 一起判断 fault。 |
| `exception_vec[23:0]` | 异常向量。 | 非零时置 target fault 和 exception pending。 |
| `replay_valid` | 是否是 replay 事件。 | LOAD/STA 置 replay pending 并重新入队对应 target；STD 不作为真实 backend replay 源。 |
| `redirect_valid` | 是否是 redirect 事件。 | 进入 redirect/flush 流程。 |
| `redirect` | redirect payload。 | 保存 redirect ROB key 和 flush 规则。 |
| `vector_ls` | 是否 vector LS feedback。 | 当前初版不支持，命中会 fatal。 |
| `uop_index` | 事件对应的端口/uop index。 | issue accept 兼容事件中来自发射端口编号；vector/atomic 拆分场景后续可复用。当前 vector 不支持，atomic 多 uop 尚未展开。 |
| `cycle` | 事件发生周期。 | 写入状态表 `last_event_cycle`，便于 debug。 |

处理流程：

1. `dispatch_monitor_event_adapter` 将各类 raw monitor 数据转换成 `memblock_wb_event_t`，只收集到本轮 batch，不直接改状态。
2. `dispatch_monitor_batch_handler` 调用 `common_data_transaction::normalize_feedback_event()` 解析 active uid、补齐 ROB/epoch，并执行 active redirect 过滤和同批 redirect-first 仲裁。
3. 未被 redirect 覆盖的真实 writeback/IQ feedback 才进入 `writeback_status_handler` 更新 pass/fault/issue feedback 状态。
4. 普通 pass 更新 target writeback/pass；当 required target 全部完成后置全局 `writeback/pass`。
5. fault/replay/redirect 推入 feedback queue，由异常、replay、redirect handler 后续处理。

一致性约束：

- 同一个事件若同时带 uid、ROB、LQ、SQ，解析出来的 uid 必须一致。
- 非 redirect 事件 target 必须是 LOAD/STA/STD。
- replay 后 LOAD/STA 反馈必须带正确 `issue_epoch/replay_seq`，否则可能是旧反馈，应丢弃；STD 反馈不因 STA replay bump replay_seq 被丢弃，若 raw feedback 缺 issue_epoch，必须在 `std_dispatched=1` 后补当前 `std_issue_epoch`，否则丢弃。
- redirect 事件可以 target 为 NONE，但必须能提供或推导 redirect ROB key。

## 12. 状态字段枚举

### 12.1 `memblock_status_field_e`

该枚举是 `common_data_transaction::set_status_field()` 和相关 helper 的字段 id。它让不同 task 不直接散写状态表字段名，减少重复 case 逻辑。

| 枚举值 | 含义 | 主要更新阶段 |
|---|---|---|
| `MEMBLOCK_STATUS_ACTIVE` | uid 当前处于 active 生命周期。 | LSQ 入队/active map 建立时置 1，retire/flush 后清理。 |
| `MEMBLOCK_STATUS_ENQ` | 已完成 LSQ/admission 入队。 | LSQ enq 成功后置 1。 |
| `MEMBLOCK_STATUS_TLB_MAPPED` | 已建立真实 TLB 表项并完成 uid PTE 回填。 | L2TLB responder 建/查 entry 后，在 `update_uid_tlb_records_by_entry()` 匹配 uid record 成功时置 1。 |
| `MEMBLOCK_STATUS_ISSUE_READY` | uid 已完成 admission，可以进入 issue queue。 | LSQ enq/admission 成功后置 1；issue scheduler 用它作为 route/eligible 前置条件。 |
| `MEMBLOCK_STATUS_QUEUED_LOAD` | 已进入 LOAD 发射队列。 | route LOAD target 后置 1。 |
| `MEMBLOCK_STATUS_QUEUED_STA` | 已进入 STA 发射队列。 | route STA target 后置 1。 |
| `MEMBLOCK_STATUS_QUEUED_STD` | 已进入 STD 发射队列。 | route STD target 后置 1。 |
| `MEMBLOCK_STATUS_LOAD_DISPATCHED` | LOAD target 已驱动到流水线。 | issue 发射成功后置 1。 |
| `MEMBLOCK_STATUS_STA_DISPATCHED` | STA target 已驱动到流水线。 | issue 发射成功后置 1。 |
| `MEMBLOCK_STATUS_STD_DISPATCHED` | STD target 已驱动到流水线。 | issue 发射成功后置 1。 |
| `MEMBLOCK_STATUS_WRITEBACK` | required targets 已整体 writeback。 | 所需 target 都 pass 且无异常/replay/redirect 后置 1。 |
| `MEMBLOCK_STATUS_PASS` | required targets 已整体 pass。 | 和全局 writeback 同步置 1。 |
| `MEMBLOCK_STATUS_FAULT` | 该 uid 出现 fault。 | target fault 或异常事件处理时置 1。 |
| `MEMBLOCK_STATUS_EXCEPTION_PENDING` | 有待处理异常。 | fault 事件进入异常处理前置 1。 |
| `MEMBLOCK_STATUS_REPLAY_PENDING` | 有待处理 replay。 | `replay_valid=1` 的 replay 事件处理时置 1。 |
| `MEMBLOCK_STATUS_REDIRECT_PENDING` | 有待处理 redirect。 | redirect/flush 相关处理时置 1。 |
| `MEMBLOCK_STATUS_FLUSHED` | 已被 flush。 | redirect flush 覆盖到该 uid 时置 1。 |
| `MEMBLOCK_STATUS_ROB_COMMIT` | ROB commit 已推进到该 uid。 | LSQ commit port 驱动或模型 commit 时置 1。 |
| `MEMBLOCK_STATUS_LSQ_DEQ` | LQ/SQ 资源已释放或不需要释放。 | DUT deq/cancel 或无 LSQ mapping 时置 1。 |
| `MEMBLOCK_STATUS_SUCCESS` | 该 uid 生命周期完成。 | ROB commit 和 LSQ deq 条件都满足后置 1。 |
| `MEMBLOCK_STATUS_LOAD_WRITEBACK` | LOAD target writeback。 | load feedback pass/fault 时更新。 |
| `MEMBLOCK_STATUS_STA_WRITEBACK` | STA target writeback。 | STA feedback 或 store address 侧完成时更新。 |
| `MEMBLOCK_STATUS_STD_WRITEBACK` | STD target writeback。 | STD feedback 或 store data 侧完成时更新。 |
| `MEMBLOCK_STATUS_LOAD_PASS` | LOAD target pass。 | load 正常完成后置 1。 |
| `MEMBLOCK_STATUS_STA_PASS` | STA target pass。 | STA 正常完成后置 1。 |
| `MEMBLOCK_STATUS_STD_PASS` | STD target pass。 | STD 正常完成后置 1。 |
| `MEMBLOCK_STATUS_LOAD_FAULT` | LOAD target fault。 | load 异常反馈后置 1。 |
| `MEMBLOCK_STATUS_STA_FAULT` | STA target fault。 | STA 异常反馈后置 1。 |
| `MEMBLOCK_STATUS_STD_FAULT` | STD target fault。 | STD 异常反馈后置 1。 |

状态使用原则：

- 发射相关状态按 target 分开记录，避免 store 的 STA/STD 生命周期互相覆盖。
- 全局 `writeback/pass` 只能在 required target 全部完成后置位。
- LOAD/STA replay 会清除对应 target 的 dispatched/writeback/pass，并 bump `replay_seq`；STA replay 不清 STD pending，STD 不作为真实 DUT backend replay 源建模。
- redirect/flush 会清理受影响 uid 的发射结果并 retire active map。
- commit candidate 必须同时满足 active、writeback、pass、required target done、无 fault/replay/redirect/flushed/issue_killed。

## 13. 字段控制强度分类

| 分类 | 字段/类型 | 说明 |
|---|---|---|
| 强控制流字段 | `fuType/fuOpType`、`memblock_op_behavior_t`、`memblock_issue_target_e`、ROB/LQ/SQ key、`memblock_status_field_e` 中生命周期字段 | 直接决定是否能入队、路由到哪里、何时完成、何时 commit/retire。 |
| 中等控制字段 | `op_class`、`lsq_flow`、`send_pri`、`ready_cycle`、`replay_seq`、`issue_epoch` | 不一定直接发 DUT，但会影响模板选择、调度顺序、replay 去旧和 stale feedback 过滤。 |
| payload/查表字段 | `memblock_tlb_lookup_key_t`、`port_id`、`cycle`、`uop_index`、`exception_vec` | 主要用于查表、debug、异常分类或专项覆盖；其中异常类字段触发后会转成强控制流状态。 |
| 保留/初版禁用字段 | vector FuType、`vector_ls`、部分 CBO flow 语义 | 当前初版不完整支持，默认应禁止或低权重，命中 unsupported path 应 fatal。 |
