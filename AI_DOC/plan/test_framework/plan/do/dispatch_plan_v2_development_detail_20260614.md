# dispatch_plan_v2 开发执行细节

## 1. 开发规则

本文件用于记录 `dispatch_plan_v2_review_annotated.md` 的实际开发拆分、实现细节、疑问处理和验收结果。每完成一个任务，都需要补充本文件中的实现说明、验收记录和后续风险。

执行节奏：

1. 按任务粒度实现，单个任务尽量保持可独立编译和提交。
2. 每个任务完成后由 subagent 独立 review。
3. review 通过后执行该任务验收点。
4. 验收通过后只提交该任务相关文件。
5. 若实现过程中发现方案疑问，优先结合 Scala 源码、Verilog RTL 和当前 UVM 环境采用最简合法方案，并把问题和解决方案记录在本文。

## 2. 工程映射

方案中的 `memblock_base_sequence.sv` 在当前工程中不能直接映射到已有 `mem_base_sequence.sv`。当前 `mem_base_sequence.sv` 内的 `mem_access_base_sequence` 已作为 dcache/sbuffer memory responder 基类使用，并被 `tc_base.sv` 作为默认 sequence 入口引用。

实际开发采用以下落点：

| 方案角色 | 当前实现文件 | 说明 |
|---|---|---|
| 公共主控 base sequence | `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv` | 新增，不影响已有 memory responder |
| 公共参数封装 | `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv` | 从 `plus.sv` 读取解析后的参数，并做校验 |
| 公共数据单例 | `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv` | 保存主表、状态表、TLB 表、发射队列和运行状态 |
| 现有 plus 入口 | `mem_ut/ver/ut/memblock/env/plus.sv` | 后续任务扩展参数 |
| package 接入 | `mem_ut/ver/ut/memblock/tc/tc_pkg.sv` | 按依赖顺序 include 新增 sequence/helper |

## 3. 任务拆分与验收点

### Task1：可编译骨架与入口命名对齐

目标：建立最小可编译框架，不连接 DUT、不发 transaction、不改变默认 testcase 行为。

实现文件：

- `seq/base_seq/seq_csr_common.sv`
- `seq/base_seq/common_data_transaction.sv`
- `seq/base_seq/memblock_dispatch_base_sequence.sv`
- `tc/tc_pkg.sv`

实现 class：

- `seq_csr_common`
  - 字段：
    - `initialized`：plus 参数是否已完成初始化。
    - `init_sem`：保护初始化临界区，避免多个 sequence 并发重复解析。
    - `main_trans_num`：本轮主表条目数，Task1 先提供最小配置入口。
  - 函数/task：
    - `init()`：初始化入口，幂等执行，调用 `load_from_plus()` 和 `validate_and_clamp()`。
    - `is_initialized()`：返回初始化状态。
    - `check_initialized(caller)`：未初始化时 fatal。
    - `load_from_plus()`：Task1 使用现有 `plus::plus_memblock_demo_depth` 作为临时兼容入口，后续 Task2 替换为正式 `MEMBLOCK_*` 参数。
    - `validate_and_clamp()`：Task1 只保证 `main_trans_num` 非 0，完整权重检查留到 Task2。
    - `get_main_trans_num()`：读取主表数量。

- `common_data_transaction`
  - 字段：
    - `m_inst`：公共数据单例句柄。
    - `main_trans_num`：当前测试主表容量。
    - `next_uid`：下一个可分配 uid。
  - 函数/task：
    - `get()`：返回单例。
    - `reset_all_tables(main_trans_num_i)`：重置当前公共数据骨架，Task1 只清 `next_uid` 和容量字段。
    - `alloc_uid()`：分配连续 uid，并检查不超过 `main_trans_num`。
    - `end_test_check()`：检查 uid 分配数量与本轮容量一致。

- `memblock_dispatch_base_sequence`
  - 字段：
    - `data`：指向 `common_data_transaction::get()` 的单例。
  - 函数/task：
    - `new(name)`：标准 UVM 构造。
    - `pre_body()`：调用 `seq_csr_common::init()`，并获取公共数据单例。
    - `body()`：Task1 暂为空流程，只打印 no-op 信息。
    - `post_body()`：Task1 暂为空。

验收点：

```bash
rg -n "class seq_csr_common|class common_data_transaction|class memblock_dispatch_base_sequence" mem_ut/ver/ut/memblock
rg -n "seq_csr_common.sv|common_data_transaction.sv|memblock_dispatch_base_sequence.sv" mem_ut/ver/ut/memblock/tc/tc_pkg.sv
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

### Task2：plus 参数与 `seq_csr_common.sv` 完整化

目标：把主表规模、流水线数量、send priority、地址复用、TLB/PTE 权重等正式参数落到 `env/plus.sv`、`seq/plus_cfg/default.cfg` 和 `seq_csr_common.sv`。

验收点：`rg` 检查正式参数、远端编译、带最小 plusarg 的 smoke run。

实现文件：

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`

实现内容：

- `plus` class 新增正式 `MEMBLOCK_*` 静态字段，字段名与 plusarg 名保持一致。
  - 主表与模式：`MEMBLOCK_MAIN_TRANS_NUM` 控制本轮主表容量，`MEMBLOCK_USE_MANUAL_MAIN_TABLE` 控制后续主表生成是否使用手动导入模式。
  - 入队与流水线配置：`MEMBLOCK_ENQ_PER_CYCLE`、`MEMBLOCK_LOAD_PIP_NUM`、`MEMBLOCK_STA_PIP_NUM`、`MEMBLOCK_STD_PIP_NUM` 是测试框架每拍最多尝试数量；`MEMBLOCK_REAL_*` 是当前 DUT/agent 真实上限，用于 clamp。
  - op class 权重：`MEMBLOCK_OP_CLASS_*_WT` 后续由主表 transaction 约束读取，用于选择 load/store/prefetch/AMO 类操作。
  - 发射优先级：`MEMBLOCK_GLOBAL_SEND_PRI_EN`、`MEMBLOCK_SEND_PRI_*_WT`、`MEMBLOCK_SEND_PRI_STD_*_WT` 控制 `send_pri/send_pri_std` 的生成和调度是否按优先级选择。
  - 地址复用：`MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT`、`MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT`、`MEMBLOCK_MANUAL_ADDR_REUSE_EN` 控制主表生成后是否注入 load/store 地址复用。
  - delay 与 TLB/PTE 权重：`MEMBLOCK_DELAY_*_WT`、`MEMBLOCK_TLB_PTE_*_WT` 供后续字段约束和 TLB 表生成读取。
  - 地址范围：`MEMBLOCK_PADDR_BASE`、`MEMBLOCK_PADDR_RANGE` 约束后续 TLB/PTE 物理地址范围。
- `plus` class 新增 helper：
  - `load_bit(name, dst)`：读取正式 bit 参数，只接受字符串 `"0"` 或 `"1"`，非法值 fatal，避免 bit 截断。
  - `load_hex64(name, dst)`：读取 64-bit hex 参数，检查空值、负数字符串、非法 hex 字符和超过 16 个有效 hex digit 的宽度溢出。
  - `is_legal_hex_string(value)`：检查 `0x/0X` 前缀、下划线和 hex 字符合法性。
  - `get_hex_digit_count(value)`：统计有效 hex digit 数量，供 `load_hex64()` 做 64-bit 宽度防护。
- `seq_csr_common` 从 `plus::MEMBLOCK_*` 读取最终值，并将 sequence 内部使用的字段保持为小写稳定变量。
- `seq_csr_common` getter 统一使用 `check_initialized()` 保护；后续生成器、transaction 和 helper 只允许通过 getter 读取配置，不直接读取 `plus::MEMBLOCK_*`。
- `validate_and_clamp()` 实现以下检查：
  - `main_trans_num`、`paddr_range`、真实端口数量非 0。
  - 所有整型 plus 参数先检查非负，避免负数进入 unsigned 后变成大正数。
  - `send_pri_default/send_pri_std_default` 和地址复用概率 clamp 到 0 到 100。
  - `load_pip_num/sta_pip_num/std_pip_num` clamp 到真实端口数量范围；`enq_per_cycle` 固定模式必须落在 `[1:real_enq_width]` 内，超出直接 fatal。随机模式由 `MEMBLOCK_ENQ_PER_CYCLE_RAND_EN` 控制，每次 `get_enq_per_cycle()` 在 `[1:real_enq_width]` 内均匀随机。
  - `send_pri`、`send_pri_std`、delay、op class、TLB/PTE bit 权重组全 0 fatal。
- `MEMBLOCK_PADDR_BASE` 和 `MEMBLOCK_PADDR_RANGE` 使用 `plus::load_hex64()` 解析，负数字符串直接 fatal。

调度关系：

- `plus` 对象仍由现有环境构造并完成 plusarg 解析。
- `memblock_dispatch_base_sequence::pre_body()` 调用 `seq_csr_common::init()`。
- `seq_csr_common::init()` 幂等执行：先 `load_from_plus()` 从 `plus` 静态字段镜像配置，再 `validate_and_clamp()` 做集中合法化。
- 后续 Task3 起，transaction `pre_randomize()`、主表生成 task、TLB builder、scheduler 和 LSQ model 都必须在 `seq_csr_common::init()` 之后通过 getter 读取配置。

### Task3：主表/状态表/TLB 表 transaction 骨架

目标：新增 `main_control_transaction`、`status_transaction`、`tlb_transaction`，并接入 `common_data_transaction` 动态数组。

验收点：class 和表字段 `rg` 命中，远端编译通过。

实现文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/main_control_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/status_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/tlb_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`

公共 typedef：

- `memblock_uid_t`：主表唯一索引，当前为 `int unsigned`，由 `common_data_transaction::alloc_uid()` 连续分配。
- `memblock_rob_key_t`：ROB 查找 key，包含 `flag` 和 9-bit `value`。`flag=0` 表示该 key 当前无效。
- `memblock_lq_key_t`：LQ 查找 key，包含 `flag` 和 7-bit `value`。
- `memblock_sq_key_t`：SQ 查找 key，包含 `flag` 和 6-bit `value`。
- `memblock_op_class_e`：主表的操作大类，先覆盖 `UNKNOWN/INT_LOAD/FP_LOAD/STORE/PREFETCH/AMO`。
- `memblock_lsq_flow_e`：后续 LSQ 资源模型使用的流向大类，先覆盖 `NONE/LOAD/STORE/ATOMIC/CBO`。
- `memblock_issue_target_e`：issue queue、writeback/replay event 和 target 级状态更新使用的发射目标枚举，覆盖 `NONE/LOAD/STA/STD`；状态表不再保存 uid 级 `issue_target` 快照。

`main_control_transaction`：

- 字段：
  - `uid`：主表唯一索引。主表以 uid 为权威 key，ROB/LQ/SQ index 只作为 DUT 语义字段和反查辅助。
  - `op_class`、`lsq_flow`：后续行为分类和路由使用的抽象字段。
  - `fuType`、`fuOpType`：发往流水线 transaction 的功能类型字段。当前宽度按已有 lintsissue/RTL 接口保留为 36-bit 和 9-bit。
  - `src_0`、`imm`、`vaddr`：地址生成字段。`vaddr = src_0 + sign_extend_imm12(imm)`，用于 TLB 表和地址复用。
  - `robIdx_flag/value`、`lqIdx_flag/value`、`sqIdx_flag/value`：后续和 DUT 接口匹配的 ROB/LQ/SQ 标识。ROB 使用 9-bit，LQ 使用 7-bit，SQ 使用 6-bit。
  - `numLsElem`：向量或多元素访存的简化元素数量字段，后续 Task6/Task8 用于入队资源估算。
  - `tlbAF`、`tlbPF`、`tlbGPF`、`PBMT`、`pmaAF`：翻译、访问和 PMA/PBMT 控制字段。`pmaAF` 功能保留，后续可继续细化。
  - `corrupt`、`denied`：DCache/TL 返回路径异常控制字段，按方案允许组合异常。
  - `delay`：后续调度延迟字段。
  - `send_pri`、`send_pri_std`：优先发射调度字段，取值范围 0 到 100。
- 函数：
  - `new(name)`：初始化默认值。
  - `pre_randomize()`：检查 `seq_csr_common::init()` 已完成，避免后续随机约束读取未初始化配置。
  - `post_randomize()`：更新 `vaddr` 并调用 `validate_main_transaction()`。
  - `post_manual_config(recompute_vaddr)`：手动主表导入后使用，默认重新计算 `vaddr` 并检查合法性。
  - `update_vaddr()`：根据 `src_0/imm` 生成 `vaddr`。
  - `validate_main_transaction()`：检查 `vaddr` 派生关系和 `send_pri/send_pri_std` 范围。
  - `get_rob_key()`、`get_lq_key()`、`get_sq_key()`：返回对应 key struct，供公共数据 API 和后续 monitor 反查使用。
  - `sign_extend_imm12(imm_i)`：按 12-bit 立即数做符号扩展。

`status_transaction`：

- 字段：
  - `uid`：状态表所属主表 uid。
  - `active`：该 uid 是否已进入本轮有效流程。后续只在 active 条目中做 ROB/LQ/SQ 匹配。
  - `enq`、`tlb_mapped`：入队和 TLB 表生成状态。
  - `queued_load/queued_sta/queued_std`：是否已进入对应发射队列。
  - `load_dispatched/sta_dispatched/std_dispatched`：是否已发往对应流水线。
  - `writeback`、`pass`、`fault`：写回和结果状态。
  - `exception_pending`、`replay_pending`、`redirect_pending`：异常、后端 replay、redirect 待处理状态。
  - `flushed`：redirect/flush 后被杀掉的状态。
  - `commit`、`rob_commit`、`lsq_deq`：ROB commit 和 LSQ 资源释放状态。
  - `success`、`access`：最终 summary 和是否实际访问过流水线。
  - `robIdx/lqIdx/sqIdx flag/value`：从主表快照得到的索引字段。
  - `load_issue_epoch/sta_issue_epoch/std_issue_epoch`、`replay_seq`、`issue_killed`：后续 target 级条件更新和迟到写回过滤使用。
- 函数：
  - `new(name)`：构造后调用 `reset(0)`。
  - `reset(uid_i)`：重置状态字段并绑定 uid。
  - `snapshot_from_main(tr)`：从主表复制 ROB/LQ/SQ 快照。
  - `get_rob_key()`：返回 ROB key。

`tlb_transaction`：

- 字段：
  - `uid`：TLB 表所属主表 uid。
  - `vaddr/paddr/vpn/ppn`：虚实地址和页号字段，当前按 4KB 页切分。
  - `pte_r/w/x/u/g/a/d/n/v`：PTE 权限和属性位，后续由 TLB builder 按 plus 权重随机并在 `post_randomize()` 或 builder 中做合法化。
  - `pbmt`：PBMT 属性。
  - `tlbAF/tlbPF/tlbGPF/pmaAF`：从主表继承的异常控制字段。
  - `asid/vmid/s2xlate/priv_mode/csr_update_seq`：CSR runtime/TLB responder 后续扩展字段。
- 函数：
  - `new(name)`：构造后调用 `reset(0)`。
  - `reset(uid_i)`：重置地址、PTE、异常和 CSR 相关字段。
  - `update_addr_fields(vaddr_i, paddr_i)`：更新地址并同步 `vpn/ppn`。
  - `copy_control_from_main(tr)`：从主表复制 `uid/vaddr/vpn/tlbAF/tlbPF/tlbGPF/pmaAF/PBMT`。

`common_data_transaction` 新增 API：

- 动态数组：
  - `main_table_by_uid[]`：主控制表，按 uid 存放 `main_control_transaction`。
  - `status_by_uid[]`：状态表，按 uid 存放 `status_transaction`。
  - `tlb_table_by_uid[]`：TLB 表，按 uid 存放 `tlb_transaction`。
- 函数：
  - `reset_all_tables(main_trans_num_i)`：设置本轮容量，清零 `next_uid`，并重新分配三张表。
  - `is_valid_uid(uid)`：检查 uid 是否落在当前容量内。
  - `check_uid(uid, caller)`：非法 uid 时 fatal。
  - `set_main_transaction(uid, tr)`：写入主表并强制 `tr.uid=uid`。
  - `get_main_transaction(uid)`：读取主表，未初始化条目 fatal。
  - `init_status_for_uid(uid)`：创建并初始化状态表条目；若主表存在，则复制索引快照。
  - `get_status(uid)`：读取状态表，未初始化条目 fatal。
  - `set_tlb_transaction(uid, tr)`：写入 TLB 表并强制 `tr.uid=uid`。
  - `get_tlb_transaction(uid)`：读取 TLB 表，未初始化条目 fatal。

调度关系：

- `memblock_dispatch_base_sequence::pre_body()` 仍只负责初始化 `seq_csr_common` 并获取 `common_data_transaction` 单例。
- 后续主表生成入口在调用 `reset_all_tables(main_trans_num)` 后，使用 `alloc_uid()` 分配 uid，再调用 `set_main_transaction()`、`init_status_for_uid()` 和 `set_tlb_transaction()` 填充三张表。
- 第二类字段和第三类字段不进入主表；后续发射前由独立赋值 task 写入发往 agent 的 transaction。

### Task4：公共数据生命周期 API 与 ROB helper

目标：实现状态字段级更新、active map、uid 查找、retire 清理和 ROB 环形比较 helper。

验收点：API `rg` 命中，远端编译通过。

实现文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/rob_order_util.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/status_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`

实现 class/API：

- `rob_order_util`
  - `rob_to_map_key(key)`、`lq_to_map_key(key)`、`sq_to_map_key(key)`：把 `{flag,value}` struct 压成 packed key，用于关联数组索引；其中 `rob_to_map_key()` 会检查 `value < MEMBLOCK_ROB_SIZE`。
  - `rob_advance(base, step)`：按 ROB 环形语义推进指针。`value` 到 `MEMBLOCK_ROB_SIZE-1` 后回到 0，并翻转 `flag`。
  - `rob_is_after(left, right)`：按 XiangShan `CircularQueuePtr.>` 规则判断 `left` 是否 younger，即 `(left.flag ^ right.flag) ^ (left.value > right.value)`。
  - `rob_need_flush(uop_rob, redirect)`：封装 `flushItself || isAfter` 语义；`redirect.valid=0` 时返回 0。

- `common_data_transaction`
  - 新增 active 映射：
    - `uid_by_active_rob[memblock_rob_map_key_t]`：当前 active ROB key 到 uid 的映射。
    - `uid_by_lq[memblock_lq_map_key_t]`：当前 active LQ key 到 uid 的映射。
    - `uid_by_sq[memblock_sq_map_key_t]`：当前 active SQ key 到 uid 的映射。
  - 新增全局控制状态：
    - `flush_in_progress`：后续 redirect/flush 协同发射时使用。
    - `active_redirect`：当前 redirect payload 快照。
    - `global_issue_epoch`：每次发射分配的时代编号，用于迟到反馈过滤。
    - `issue_freeze_ack`：后续发射冻结握手状态。
  - `reset_all_tables(main_trans_num_i)`：除重建三张表外，初始化每个 `status_by_uid[uid]` 为 inactive，清空 active ROB/LQ/SQ 映射，并清零全局 flush/issue 状态。
  - `is_valid_lq_key(key)`、`is_valid_sq_key(key)`：只检查 value 是否落在容量范围内。`flag` 是环形回绕位，不是 valid 位。
  - `ensure_status_exists(uid, caller)`：保证状态表条目存在。
  - `set_status_field(uid, field, value)`：字段级状态更新 API。`active` 不允许通过该 API 修改，必须走 `activate_uid()`/`retire_active_uid()`。
  - `get_status_field(uid, field)`：字段级读取 API。
  - `conditional_set_target_status_field(uid, field, value, target, issue_epoch, replay_seq)`：target 级条件更新 API。只有条目仍 active、未被 kill、对应 target 的 `issue_epoch/replay_seq` 匹配时才更新，用于后续写回/异常迟到反馈过滤。
  - `alloc_issue_epoch()`：递增并返回全局 issue epoch。
  - `mark_issue_snapshot(uid, target, issue_epoch)`：发射时把 issue epoch 记录到对应 target，并清 `issue_killed`。
  - `bump_replay_seq(uid)`：后续 replay 重新入队前递增 replay 序号。
  - `activate_uid(uid, map_lq, map_sq)`：入队/admission 成功后建立 active 映射。必须检查同一 ROB key 当前没有其他 active uid；只有 `map_lq/map_sq` 为 1 时才建立 LQ/SQ 映射。
  - `lookup_active_uid_by_rob(rob_key, uid)`、`lookup_active_uid_by_lq(lq_key, uid)`、`lookup_active_uid_by_sq(sq_key, uid)`：monitor 反查入口，返回 bit 表示是否查到；查到后会校验映射没有 stale。
  - `get_active_uid_by_rob(rob_key)`：强制 ROB 反查入口，查不到直接 fatal。
  - `retire_active_uid(uid)`：commit/deq 或 flush/cancel 确认后释放 active ROB/LQ/SQ 映射，并把状态表 `active` 清零；不删除主表、状态表或 TLB 表历史。
  - `end_test_check()`：扩展为检查连续 uid 分配、所有状态无 active/pending、active 映射为空、全局 flush/redirect 控制为空闲。

- `status_transaction`
  - 新增 `active_lq_mapped`、`active_sq_mapped`：记录该 uid 是否真的建立过 LQ/SQ active 映射。原因是 LQ/SQ `flag` 是环形回绕位，不能作为“是否分配”的 valid 位。

调度关系：

- 主表生成阶段仍只分配 uid 和初始化 status，不建立 active 映射。
- 入队 fire 或 non-LSQ admission 成功时调用 `activate_uid()`；后续 Task6 会根据 op 行为决定 `map_lq/map_sq`。
- 发射 task 调用 `alloc_issue_epoch()` 和 `mark_issue_snapshot()`，写回/异常 task 后续必须通过 `conditional_set_target_status_field()` 更新当前 target 发射实例，避免 redirect/replay 后旧反馈污染新状态。
- commit/deq 或 flush/cancel 完成时调用 `retire_active_uid()` 释放 active 映射；历史追溯表保留到 testcase 结束。

疑问与解决：

- 疑问：`robIdx/lqIdx/sqIdx` 的 `flag` 是否能当 valid 位使用。
- 结论：不能。Scala 源码 `utility/CircularQueuePtr.scala` 中 `flag` 是环形队列回绕位，比较规则也基于 `flag/value` 组合；因此 Task4 不再用 `flag=0` 判断 key 无效。是否建立 LQ/SQ 映射由 `activate_uid(map_lq,map_sq)` 和状态表 `active_lq_mapped/active_sq_mapped` 明确记录。
- 疑问：`rob_is_after()` 如何实现才和 DUT 一致。
- 结论：参考 `CircularQueuePtr.>` 和 `RobPtr.needFlush()`，实现为 `(left.flag ^ right.flag) ^ (left.value > right.value)`；redirect flush 判断封装到 `rob_need_flush()`。

### Task5：主表生成 task

目标：实现随机生成、手动导入入口和地址复用注入 task。

验收点：主表生成 task `rg` 命中，远端编译通过；若新增 smoke vseq，则运行最小主表条目用例。

拆分子任务：

- Task5.1：集中定义最小 FuType/LSUOpType 常量。
  - 验收点：`MEMBLOCK_FUTYPE_LDU/STU/MOU` 和标量 LSUOpType 常量只在 `memblock_dispatch_types.sv` 集中定义。
- Task5.2：实现随机主表生成入口。
  - 验收点：`build_main_table()`、`build_random_main_table()`、`randomize_main_transaction()`、`select_op_class_by_weight()`、`apply_minimal_op_template()` 编译通过。
- Task5.3：实现手动主表导入入口。
  - 验收点：`manual_main_table_by_rob`、`set_manual_main_transaction()`、`clear_manual_main_table()`、`import_manual_main_table()` 编译通过，导入时仍重新分配连续 uid。
- Task5.4：实现地址复用后处理。
  - 验收点：`inject_ls_addr_reuse_by_fuoptype()`、`select_load_addr_ref()`、`select_prior_store_addr_ref()` 编译通过，修改 `src_0/imm` 后重新计算 `vaddr`。
- Task5.5：实现主表生成完成轻量检查。
  - 验收点：`check_main_table_complete()` 编译通过，不调用会检查 active/pending 的 `end_test_check()`。

实现细节：

- `memblock_dispatch_types.sv`
  - `MEMBLOCK_FUTYPE_LDU/STU/MOU`：来自 `FuType.scala` 的 one-hot 编码，分别对应 bit16、bit17、bit18。
  - `MEMBLOCK_FUTYPE_VLDU/VSTU/VSEGLDU/VSEGSTU`：vector LS one-hot 编码，仅用于 Task5 显式拒绝。
  - `MEMBLOCK_LSUOP_LB/LH/LW/LD/LBU/LHU/LWU`：标量 load `fuOpType` 最小集合。
  - `MEMBLOCK_LSUOP_SB/SH/SW/SD`：标量 store `fuOpType` 最小集合。
  - `MEMBLOCK_LSUOP_PREFETCH_I/R/W`：software prefetch 最小集合，按源码属于 `FuType.ldu`。
  - `MEMBLOCK_LSUOP_LR/SC/AMOADD_{W,D}`：AMO 最小集合，按源码属于 `FuType.mou`。

- `common_data_transaction`
  - `check_main_table_complete()`：只检查 `main_trans_num != 0`、`next_uid == main_trans_num`、`main_table_by_uid[uid]` 和 `status_by_uid[uid]` 非空。该函数用于主表生成结束检查，不检查 active/pending，因此不会和后续发射生命周期冲突。

- `memblock_dispatch_base_sequence`
  - 字段 `manual_main_table_by_rob[int unsigned]`：手动配置入口表。key 仅表示外部配置顺序/ROB 配置入口，导入后权威 key 仍是连续 `uid`。
  - `build_main_table()`：主表统一入口。根据 `seq_csr_common::get_use_manual_main_table()` 选择随机生成或手动导入。
  - `build_random_main_table(main_trans_num_i)`：调用 `reset_all_tables(main_trans_num_i)`，从 ROB `{flag=0,value=0}` 开始按 `rob_advance()` 连续生成条目；每条分配 uid、随机/模板修正 transaction 并写入主表；最后执行地址复用、初始化状态并调用 `check_main_table_complete()`。
  - `import_manual_main_table()`：检查手动表非空，按手动 key 排序后稳定导入；每条通过 `alloc_uid()` 重新分配 uid，调用 `post_manual_config()` 和 `validate_main_table_entry()`，再写入公共主表；只有 `MEMBLOCK_MANUAL_ADDR_REUSE_EN=1` 时才执行地址复用，地址复用之后统一初始化状态。
  - `set_manual_main_transaction(rob_key, tr)`：供 testcase/派生 sequence 配置手动条目。
  - `clear_manual_main_table()`：清空手动表。
  - `has_manual_main_table()`：返回手动表是否已有配置。
  - `randomize_main_transaction(tr, uid, rob_key)`：先调用 transaction `randomize()` 获得普通随机字段，再覆盖 uid、ROB key、LQ/SQ 占位、标量 LS 默认 `numLsElem=1`、异常控制字段默认 0、op 模板、send_pri、delay，并重新计算 `vaddr`。Task6 后 AMO 模板会把 `numLsElem` 修正为 0。
  - `select_op_class_by_weight()`：按 `seq_csr_common` 的 `op_class_*_wt` 选择 int load、fp load、store、prefetch、AMO。
  - `apply_minimal_op_template(tr)`：根据 `op_class` 成对设置 `fuType/fuOpType/lsq_flow`；load/fp load 使用 `ldu + load op`，store 使用 `stu + store op`，prefetch 使用 `ldu + prefetch op`，AMO 使用 `mou + AMO op`。
  - `random_load_fuoptype()`、`random_store_fuoptype()`、`random_prefetch_fuoptype()`、`random_amo_fuoptype()`：从当前 Task5 最小合法集合中均匀选择 `fuOpType`。
  - `randomize_send_pri_value(is_std)`：`MEMBLOCK_GLOBAL_SEND_PRI_EN=0` 时使用默认值；开启时按 low/mid/high 权重生成 0-33、34-66、67-100。
  - `randomize_delay_value()`：按 delay 权重生成 0、1-20、21-50。
  - `rand_weighted3()`、`rand_weighted5()`、`rand_percent_hit()`：生成器内部通用随机 helper。
  - `is_load_main_tr(tr)`、`is_store_main_tr(tr)`：基于 `op_class` 判断主表 load-like/store-like，避免只看 `fuOpType` 时被 load/store 重叠编码误判。
  - `is_load_fuoptype()`、`is_store_fuoptype()`、`is_prefetch_fuoptype()`、`is_amo_fuoptype()`：只用于结合 `op_class/fuType` 做模板合法性校验。
  - `select_load_addr_ref(cur_uid, ref_uid)`：为 store 地址复用选择任意已生成 load-like 条目。
  - `select_prior_store_addr_ref(cur_uid, ref_uid)`：为 load 地址复用只选择更早 uid 的 store 条目，用于提升前序 store 与后续 load 同地址概率。
  - `init_status_for_main_table()`：在主表生成/导入和可选地址复用全部完成后，为每个 uid 调用 `data.init_status_for_uid(uid)`，保证后续如果 status 快照扩展地址或异常字段，也不会与最终主表内容不一致。
  - `validate_main_table_entry(tr, caller)`：校验 `vaddr` 派生、ROB 范围、非 vector LS、`numLsElem` 与行为分类一致，以及 `op_class/fuType/fuOpType/lsq_flow` 成对合法。

调度关系：

- `pre_body()` 仍只初始化 `seq_csr_common` 和 `common_data_transaction`，不自动生成主表。
- 派生 testcase 或后续主 dispatch flow 在需要启用新框架时调用 `build_main_table()`。
- 主表生成阶段只初始化主表和 status，不建立 active ROB/LQ/SQ 映射；入队/admission 成功仍由后续 Task6 调用 `activate_uid()`。
- 地址复用是主表级后处理，必须在完整主表生成/导入后执行。

疑问与解决：

- 疑问：`FuType` 是否能直接写普通枚举编号。
- 解决：不能。参考 `FuType.scala`，`FuType` 是 `ChiselOHEnum` one-hot，Task5 把 `ldu/stu/mou` 集中定义为 bit16/17/18，后续若源码顺序变化只改 `memblock_dispatch_types.sv`。
- 疑问：load/store/AMO 的 `fuOpType` 低位编码有重叠，地址复用和合法性判断是否能只看 `fuOpType`。
- 解决：不能。Task5 的主类型判断以 `op_class` 为主，`fuOpType` 只在对应 `fuType/op_class` 分支里校验。
- 疑问：主表生成结束能否直接调用 `end_test_check()`。
- 解决：不能。`end_test_check()` 会检查 active/pending 全部空闲，适合 testcase 收尾；Task5 新增 `check_main_table_complete()` 只检查主表生成完整性。
- 疑问：异常相关字段当前是否按权重随机。
- 解决：Task5 先保留并贯通字段，但随机主表默认把 `tlbAF/tlbPF/tlbGPF/PBMT/pmaAF/corrupt/denied` 清 0，避免基础主表生成把正常 smoke 变成大量异常场景；TLB/PMA/DCache 异常权重在 Task7/Task11 结合具体路径再实现。
- 疑问：手动表 key 是否必须等于 transaction 内部 `robIdx_value`。
- 解决：当前不强制。`manual_main_table_by_rob` 的 key 只作为外部配置顺序入口，导入时按 key 排序以稳定分配 uid；真正的 ROB 语义来自 transaction 内部 `robIdx_flag/value`，后续 active admission 会检查同一 ROB key 不可重叠 active。
- 疑问：status 初始化应该在地址复用前还是后。
- 解决：放在地址复用后。当前 status 只快照 ROB/LQ/SQ，前后等价；但后续若 status 扩展保存地址或异常字段，地址复用后初始化可以避免快照与主表不一致。

Subagent review：

- 结论：无 blocker，可以提交。
- medium：手动导入未校验 key 与 transaction 内部 ROB key 一致，也不检查重复 ROB key。处理：当前设计中手动 key 只是排序入口，重复 active ROB key 由后续 admission/`activate_uid()` 检查；本文档补充该边界。
- medium：随机主表原本先初始化 status 再地址复用，未来 status 若扩展地址字段可能不一致。处理：已修改为地址复用后统一调用 `init_status_for_main_table()`。
- medium：LR/SC 是否完全按 `needAlloc=0` 处理需要 Task6 行为表确认。处理：Task5 只保留 `MOU + ATOMIC` 最小模板，LSQ 资源行为放入 Task6。
- low：`body()` 文案仍是 no-op skeleton。处理：当前不启动默认 flow，暂不影响功能；后续接入主 dispatch flow 时更新。

验收结果：

- `git diff --check` 已通过。
- `rg` 已确认 `build_main_table`、`build_random_main_table`、`import_manual_main_table`、`inject_ls_addr_reuse_by_fuoptype`、`check_main_table_complete`、FuType/LSUOpType 常量均命中。
- `make eda_compile tc=tc_sanity mode=base_fun` 已通过。
- 通过日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log` 中显示 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。

### Task8 记录

状态：已完成。

实现文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`

实现 class/API：

- `memblock_dispatch_types.sv`
  - 新增 `memblock_issue_q_item_t`，作为 load/STA/STD 三条发射队列的轻量调度项。
  - 字段：
    - `uid`：主表/状态表/TLB 表权威索引。
    - `rob_key`：ROB 环形 key 快照，仅用于年龄比较、flush 边界和 debug，不作为删除权威 key。
    - `target`：当前队列目标，取 `LOAD/STA/STD`。
    - `send_pri`：该队列项实际发射优先级；load/STA 来自主表 `send_pri`，STD 来自主表 `send_pri_std`。
    - `ready_cycle`：调度延迟计数，Task8 用主表 `delay` 初始化；后续每拍可由 `advance_issue_queue_delays()` 递减。
    - `replay_seq`：入队时状态表 replay 序号，用于 replay/redirect 后过滤旧队列项。
    - `has_lqIdx/lq_key`、`has_sqIdx/sq_key`：可选缓存的 LQ/SQ key；是否有效来自状态表 `active_lq_mapped/active_sq_mapped`。
    - `numLsElem`：主表行为派生的 LS 元素数快照。
    - `uop_index/uop_count`：atomic/AMOCAS 后续多 uop 拆分预留；Task8 只记录 count，暂不展开多个队列项。

- `common_data_transaction`
  - 新增字段：
    - `load_issue_q[$]`、`sta_issue_q[$]`、`std_issue_q[$]`：三条轻量发射队列，只保存 pending 调度项，不承担历史追溯。
  - 新增函数：
    - `clear_issue_queues()`：清空三条队列，`reset_all_tables()` 中调用。
    - `issue_queue_contains(target, uid, replay_seq)`：按 `(target, uid, replay_seq)` 做幂等查重。
    - `delete_issue_queue_entry(target, uid, replay_seq, match_replay_seq=1)`：删除指定目标队列中的项；`match_replay_seq=0` 时删除该 uid 所有 replay_seq 的残留项。
    - `remove_uid_from_issue_queues(uid)`：从三条队列删除该 uid 残留项，并清状态表 queued 位。
    - `push_issue_queue_item(item)`：按 target 插入队列，已存在同 `(target, uid, replay_seq)` 时直接返回。
    - `get_issue_queue_size(target)`：返回目标队列长度。
  - 生命周期更新：
    - `reset_all_tables()` 清空三条队列。
    - `retire_active_uid(uid)` 先调用 `remove_uid_from_issue_queues(uid)`，避免 commit/deq 或 flush/cancel 后残留队列项。
    - `end_test_check()` 要求三条队列为空。

- `issue_queue_scheduler`
  - 字段：
    - `data`：`common_data_transaction::get()` 句柄。
  - 函数：
    - `ensure_data()`：兜底获取公共数据单例。
    - `make_empty_item()`：构造安全默认队列项，避免 enum 字段裸 0 初始化 warning。
    - `make_issue_item(uid, target, behavior)`：根据主表、状态表和行为模板构造轻量队列项。
    - `is_uid_route_ready(uid)`：判断 uid 是否满足 `active/enq/tlb_mapped` 且没有异常、redirect、replay 阻塞。
    - `target_already_queued_or_done(status, target)`：判断某目标是否已排队或已 dispatch/完成，避免重复路由。
    - `set_target_queued(uid, target, value)`：更新 `queued_load/queued_sta/queued_std`。
    - `set_target_dispatched(uid, target, value)`：更新 `load_dispatched/sta_dispatched/std_dispatched`。
    - `route_target(uid, target, behavior)`：清理旧 pending 项、构造队列项、插入目标队列并置 queued 位。
    - `route_uid(uid)`：调用 `lsq_ctrl_model::derive_op_behavior()`，按 `route_load/route_sta/route_std` 把 uid 路由到对应队列。
    - `route_all_ready_uids()`：扫描主表容量内所有 ready uid 并路由。
    - `advance_issue_queue_delays()`：三条队列的 `ready_cycle` 非 0 时递减。
    - `is_issue_item_eligible(item)`：发射前二次检查 active、enq、tlb_mapped、replay_seq、ready_cycle、异常/redirect/replay 阻塞和目标完成状态。
    - `item_is_older(left, right)`：用 `rob_order_util::rob_is_after()` 做 ROB 年龄比较，同 ROB key 时用 uid 稳定打破平局。
    - `item_is_better(candidate, best, compare_pri)`：优先级模式下先比 `send_pri`，相同则比 ROB 年龄；非优先级模式下只比 ROB 年龄。
    - `select_target_candidates(target, max_count, use_global_pri, global_pri, selected)`：从单目标队列中选择最多 `max_count` 个 eligible 候选。
    - `find_global_max_send_pri(max_pri)`：扫描三条队列 eligible 项，找全局最大 `send_pri`。
    - `select_issue_candidates(load_items, sta_items, std_items)`：统一候选选择入口；`global_send_pri_en=1` 时只选择全局最大优先级候选，允许同最大优先级跨 load/STA/STD 并发；`global_send_pri_en=0` 时三队列完全并行，各自受 `load_pip_num/sta_pip_num/std_pip_num` 限制。
    - `mark_issue_fire(item)`：Task10 真实 drive 成功后调用；二次检查 eligible，通过后分配 issue epoch、删除队列项、清 queued 位并置对应 dispatched 位。

- `memblock_dispatch_base_sequence`
  - 新增字段：
    - `issue_sched`：`issue_queue_scheduler` 句柄，`pre_body()` 中创建。
  - 新增 task：
    - `route_issue_queue_for_uid(uid)`：路由单个 uid。
    - `route_all_issue_queues()`：扫描并路由所有 ready uid。
    - `select_issue_candidates(load_items, sta_items, std_items)`：返回本轮软件候选项，供后续发射 task 使用。
    - `mark_issue_item_fire(item, fired)`：后续真实发射成功时标记状态并删除队列项。
  - 调整：
    - `build_tlb_table_for_active_uid()` 在 TLB 表生成后立即调用 `route_issue_queue_for_uid(uid)`，形成 “入队/admission -> TLB 映射 -> 队列路由” 的连续流程。

调度关系：

- Task8 不 drive 真实 agent，只提供队列和候选选择模型。
- 后续 Task10 的预期调用顺序为：每拍先调用 `route_all_issue_queues()` 吸收新 ready uid，再调用 `select_issue_candidates()` 获得 load/STA/STD 候选，真实 agent drive fire 成功后调用 `mark_issue_item_fire()`。
- 队列项 fire 后立即删除；追溯记录仍依赖主表、状态表和 TLB 表。
- `global_send_pri_en=1` 时，scheduler 在三条队列中先找全局最大 `send_pri`，三个目标队列只从这个优先级中选候选；同优先级跨队列可以并发，同队列超过 pipe 数按 ROB 年龄选 older 项。
- `global_send_pri_en=0` 时，load/STA/STD 三条队列各自独立选择 eligible 项，并分别受 `load_pip_num/sta_pip_num/std_pip_num` 限制。
- `flush_in_progress=1` 时 `select_issue_candidates()` 不选择新候选，并置 `issue_freeze_ack=1`；完整 redirect/flush 清理和 replay 重新入队在后续 Task11 实现。

问题与解决：

- 问题：队列是否需要维护 `robIdx -> queue_index` 辅助索引。
- 解决：不维护。当前队列只保存 pending 项，删除和查重按 `(target, uid, replay_seq)` 顺序扫描，符合之前确认的“追溯通过主表和状态表，队列发射后删除”方案，避免易失效索引。
- 问题：STU store 是否拆成 STA/STD 两个队列项。
- 解决：是。`derive_op_behavior()` 对 store/CBO 设置 `route_sta=1` 和 `route_std=1`，Task8 将同一 uid 分别路由到 STA 和 STD；STA 使用 `send_pri`，STD 使用 `send_pri_std`。
- 问题：ATOMIC 是否进入 load 队列。
- 解决：不进入 load 队列。Task6 行为模板对 `FuType.mou` 设置 `route_sta/route_std`，Task8 保持该路由，后续 Task10 由 STA/STD 专项发射逻辑映射到 AtomicsUnit/atomicData。
- 问题：AMOCAS 多 uop 是否现在拆成多条队列项。
- 解决：Task8 暂不展开多条队列项，只在 `uop_count` 缓存 `atomic_sta_uop_count/atomic_data_uop_count`。后续真实 agent drive 时再决定是否拆多 beat，避免当前队列层提前复制完整 transaction。
- 问题：delay 字段如何表示。
- 解决：队列项用 `ready_cycle` 保存主表 `delay`，提供 `advance_issue_queue_delays()` 做每拍递减。真实时钟调度接入放到 Task10。

Subagent review：

- 结论：无 blocker，可以提交 Task8。
- medium：`flush_in_progress` 时 `select_issue_candidates()` 会置 `issue_freeze_ack=1`，Task8 没有恢复/清除路径。处理：本任务只建立发射队列和软件选择器，redirect/flush 清理放到 Task11；文档已明确后续异常恢复 task 必须清 `flush_in_progress/issue_freeze_ack`。
- medium：`end_test_check()` 要求三条 issue queue 为空，若运行型 testcase 只入队不消费会 fatal。处理：保持该一致性检查，Task10 接真实发射时必须在 fire 后调用 `mark_issue_item_fire()`，commit/flush/cancel 路径通过 `retire_active_uid()` 或后续恢复 task 清队列。
- low：`advance_issue_queue_delays()` 未在 Task8 暴露到每拍调度循环。处理：Task8 仅提供 helper API，真实时钟调度和 delay 递减接入放到 Task10。

验收结果：

- `git diff --check` 已通过；新建 `issue_queue_scheduler.sv` 也通过 `diff --no-index --check`。
- `make eda_compile tc=tc_sanity mode=base_fun` 已通过。
- 通过日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log` 中显示 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。

### Task9 记录

状态：已完成。

实现文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/issue_field_assigner.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_field_assigner.sv`
- `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_xaction.sv`
- `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`

实现 class/API：

- `issue_field_assigner`
  - 字段：
    - `data`：`common_data_transaction::get()` 句柄，所有字段赋值以主表、状态表和队列项为权威来源。
  - 函数：
    - `ensure_data()`：兜底获取公共数据单例。
    - `deterministic_percent_hit(uid, salt, percent)`：按 uid 和 salt 生成稳定百分比命中结果，用于 MDP/RVC 权重，避免 replay 时重新随机导致同一 uid 字段不一致。
    - `is_valid_pipe_idx(target, pipe_idx)` / `check_pipe_idx(target, pipe_idx, caller)`：校验 target 到 lintsissue 端口映射是否合法；load 支持 pipe 0/1/2，STA/STD 支持 pipe 0/1。
    - `select_prior_store_for_load(uid, wait_rob_key)`：为 load 的 MDP 等待字段选择 active 且已 enq 的前序 store/CBO 等 SQ 路径项，返回其 ROB key；没有合法前序 store 时不打开等待。
    - `compute_pc(uid)`：按 `MEMBLOCK_PC_BASE + uid * MEMBLOCK_PC_STRIDE` 派生 50bit PC。
    - `compute_is_rvc(uid)`：按 `MEMBLOCK_RVC_WT` 稳定派生 `isRVC`。
    - `compute_ftq_flag(uid)`、`compute_ftq_value(uid)`、`compute_ftq_offset(uid)`：派生前端 FTQ 元信息；初版 flag 固定为 1，value/offset 由 uid 和 plus base 派生。
    - `derive_wen(main_tr, rfWen, fpWen)`：根据 `op_class` 最小模板派生写回使能；integer load/AMO 为 `rfWen=1`，FP load 为 `fpWen=1`，store/prefetch/CBO 为 0，且强制互斥。
    - `compute_pdest(uid, has_wb)`：有写回时按 `MEMBLOCK_PDEST_BASE` 和 `MEMBLOCK_PDEST_RANGE` 派生 8bit pdest；无写回时置 0。
    - `clear_lintsissue_xaction(tr)`：清 7 个 intIssue valid 和 payload，供后续 Task10 每拍构造 xaction 时使用。
    - `assign_main_issue_fields(tr, item, pipe_idx)`：先把主表第一类字段写入 lintsissue xaction 对应端口，load 映射到 0/1/2，STA 映射到 3/4，STD 映射到 5/6。
    - `assign_issue_dep_fields(tr, item, pipe_idx)`：补第二类字段。load 端口写 `loadWaitBit/waitForRobIdx/storeSetHit/loadWaitStrict`；STA 端口写 `isFirstIssue/storeSetHit/ssid`；STD 当前无对应第二类字段，保持 no-op。
    - `assign_backend_meta_fields(tr, item, pipe_idx)`：补第三类字段。load 端口写 `pdest/rfWen/fpWen/pc/isRVC/ftqIdx/ftqOffset`；STA 端口写 `pdest/isRVC/ftqIdx/ftqOffset`；STD 当前无这些端口，保持 no-op。
    - `assign_issue_item_fields(tr, item, pipe_idx)`：组合入口，依次调用主表字段赋值、第二类字段赋值和第三类字段赋值。

- `seq_csr_common`
  - 新增参数快照和 getter：
    - `mdp_load_wait_wt` / `get_mdp_load_wait_wt()`：load 打开 `loadWaitBit` 并选择前序 store 的权重。
    - `mdp_storeset_hit_wt` / `get_mdp_storeset_hit_wt()`：独立打开 `storeSetHit` 的权重。
    - `load_wait_strict_wt` / `get_load_wait_strict_wt()`：在已有 wait store 时打开 `loadWaitStrict` 的权重。
    - `rvc_wt` / `get_rvc_wt()`：`isRVC` 权重。
    - `pc_base/pc_stride`、`ftq_idx_base`、`pdest_base/pdest_range` 及对应 getter。
  - `validate_and_clamp()` 对百分比权重钳制到 0 到 100，对 `pc_stride/pdest_range` 做非 0 检查，对 `ftq_idx_base/pdest_base/pdest_range` 做范围钳制。

- `memblock_dispatch_base_sequence`
  - 新增字段：
    - `field_assigner`：`issue_field_assigner` 句柄，`pre_body()` 中创建。
  - 新增 task：
    - `assign_main_issue_fields(tr, item, pipe_idx)`。
    - `assign_issue_dep_fields(tr, item, pipe_idx)`。
    - `assign_backend_meta_fields(tr, item, pipe_idx)`。
    - `assign_issue_item_fields(tr, item, pipe_idx)`。

调度关系：

- Task9 不 drive 真实 agent，只提供发射前 xaction 字段补齐函数。
- 后续 Task10 每拍构造 `lintsissue_agent_agent_xaction` 时，预期流程是：创建/清空 xaction -> 按调度结果选择 port -> 对每个 `memblock_issue_q_item_t` 调 `assign_issue_item_fields(tr, item, pipe_idx)` -> driver fire 成功后调 `mark_issue_item_fire()`。
- 第二类字段不写入主表，第三类字段也不写入主表；若需要 debug，可由 monitor/status 后续记录实际发射快照。

问题与解决：

- 问题：MDP 等待字段是否可以随机指向任意 ROB。
- 解决：不可以。Task9 只允许 `waitForRobIdx` 指向当前 uid 之前、active 且 enq 的 store/SQ 路径项；没有合法前序 store 时不打开 `loadWaitBit`，保证最小激励合法。
- 问题：发射前字段使用 `$urandom` 是否可接受。
- 解决：不采用即时随机。`pc/isRVC/ftq/pdest/MDP` 均按 uid、salt 和 plus 参数稳定派生，保证 replay 或 redirect 后同一 uid 重发时字段可复现。
- 问题：STD 端口没有第二类/第三类字段怎么办。
- 解决：保持 no-op。STD intIssue_5/6 端口只有 `fuType/fuOpType/src_0/robIdx/sqIdx` 等主表字段；写回和前端元信息不在该端口出现。
- 问题：`rfWen/fpWen` 是否跟 `fuOpType` 独立随机。
- 解决：不独立随机。当前按 `op_class` 最小模板派生；FP load 与 integer load 即使 `fuOpType` 共用 `lh/lw` 编码，也通过 `op_class` 区分 `fpWen/rfWen`。

Subagent review：

- 结论：无 blocker，可以提交 Task9。
- medium：AMO 当前由 `derive_wen()` 派生 `rfWen=1`，但 lintsissue STA 端口 3/4 没有 `rfWen/fpWen` 字段，只能写 `pdest/isRVC/ftq*`。处理：这是当前 Verilog 接口形态决定的 Task9 边界，Task10 接真实 driver 和特殊访存行为时需要明确 AMO 写回元信息来源，或先约束 AMO directed 场景。
- medium：写回使能按 `op_class` 派生，而路由/LSQ 行为按 `fuType/fuOpType` 派生；手动主表若配置不一致，可能导致 helper 赋值与路由语义不一致。处理：随机生成路径已保持一致；后续手动主表导入校验需要补 `op_class` 与 `fuType/fuOpType` 一致性检查。
- low：Task9 初版 `clear_lintsissue_xaction()` 只清 valid，不清 payload。处理：Task10 已扩展为清 valid 和 payload 全字段，避免 dispatch sequence 未随机化 xaction 时出现未赋 payload。
- low：文档 review/验收结果提交前仍为待执行。处理：本节已更新。

验收结果：

- `git diff --check` 已通过；新建 `issue_field_assigner.sv` 也通过 `diff --no-index --check`。
- `rg` 已确认 `class issue_field_assigner`、`assign_issue_dep_fields`、`assign_backend_meta_fields`、第二/三类字段、`MEMBLOCK_MDP_LOAD_WAIT_WT`、`MEMBLOCK_PC_BASE`、`MEMBLOCK_PDEST_RANGE`、`assign_issue_item_fields` 和 `field_assigner` 均命中。
- `make eda_compile tc=tc_sanity mode=base_fun` 已通过。
- 通过日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log` 中显示 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。

### Task6：LSQ 行为分类与入队资源模型

目标：实现 `derive_op_behavior()` 和 `lsq_ctrl_model`，先完成软件资源模型。

子任务与验收点：

- Task6.1：补齐 `memblock_op_behavior_t` 行为模板，验收 `rg "memblock_op_behavior_t|MEMBLOCK_OP_BEHAVIOR"` 命中。
- Task6.2：实现 `lsq_ctrl_model` 行为分类、环形 LQ/SQ 指针和资源镜像，验收 `rg "class lsq_ctrl_model|derive_op_behavior|commit_allocate|release_lq|cancel_lq"` 命中。
- Task6.3：把 `memblock_dispatch_base_sequence` 的主表校验切到统一 `derive_op_behavior()`，验收 AMO `numLsElem=0`、load/store/prefetch `numLsElem=1` 的校验路径存在。
- Task6.4：更新本文档并完成 subagent review、`git diff --check`、远端编译。

验收点：行为分类和 LSQ 模型 `rg` 命中，远端编译通过。

### Task7：TLB 表生成与 CSR runtime 骨架

目标：实现 per-uid TLB 表、lookup key、CSR runtime 镜像骨架和最小 TLB map builder。

子任务与验收点：

- Task7.1：补齐 TLB lookup key 和 per-uid TLB transaction 派生字段，验收 `rg "memblock_tlb_lookup_key_t|lookup_key|valididx|ppn_low"` 命中。
- Task7.2：实现 `mmu_csr_runtime_state`，保存 CSR monitor 采样后的 MMU runtime 快照，验收 `rg "class mmu_csr_runtime_state|update_from_csr_ctrl|make_lookup_key"` 命中。
- Task7.3：实现 `tlb_map_builder`，按主表 `vaddr`、plus 地址范围和 PTE 权重生成 TLB 表项，验收 `rg "class tlb_map_builder|build_tlb_for_uid|randomize_pte_bits"` 命中。
- Task7.4：在 `common_data_transaction` 中维护 `uid_by_tlb_key` lookup 索引和 `lookup_tlb_uid()`，验收 `rg "uid_by_tlb_key|register_tlb_lookup|lookup_tlb_uid|clear_tlb_lookup_index"` 命中。
- Task7.5：在 `memblock_dispatch_base_sequence` 中提供 `build_tlb_table_for_active_uid()` 调度入口，验收该 task 编译通过。
- Task7.6：更新本文档并完成 subagent review、`git diff --check`、远端编译。

验收点：TLB builder、lookup key、runtime CSR 字段 `rg` 命中，远端编译通过。

### Task8：发射队列路由和调度器

目标：实现 load/STA/STD 三个轻量队列和软件调度器。

子任务与验收点：

- Task8.1：新增 `memblock_issue_q_item_t` 轻量队列元素，验收 `rg "memblock_issue_q_item_t|load_issue_q|sta_issue_q|std_issue_q"` 命中。
- Task8.2：在 `common_data_transaction` 中维护三条发射队列和增删查 API，验收 `rg "push_issue_queue_item|delete_issue_queue_entry|remove_uid_from_issue_queues"` 命中。
- Task8.3：实现 `issue_queue_scheduler` helper，覆盖按行为路由、去重、eligible 检查、send_pri 仲裁和 fire 后删除，验收 `rg "class issue_queue_scheduler|route_uid|select_issue_candidates|mark_issue_fire"` 命中。
- Task8.4：在 `memblock_dispatch_base_sequence` 中接入路由和候选选择入口，验收 `rg "route_issue_queue_for_uid|route_all_issue_queues|mark_issue_item_fire"` 命中。
- Task8.5：更新本文档并完成 subagent review、`git diff --check`、远端编译。

验收点：队列字段、scheduler、send_pri 调度 `rg` 命中，远端编译通过。

### Task9：发射前字段赋值

目标：实现 `assign_issue_dep_fields()` 和 `assign_backend_meta_fields()`，集中处理第二类和第三类字段。

子任务与验收点：

- Task9.1：新增 `issue_field_assigner` helper，验收 `rg "class issue_field_assigner|assign_issue_dep_fields|assign_backend_meta_fields"` 命中。
- Task9.2：补齐第二类字段赋值：`loadWaitBit/waitForRobIdx/storeSetHit/loadWaitStrict/isFirstIssue`，验收对应字段在 helper 中命中。
- Task9.3：补齐第三类字段赋值：`pc/isRVC/ftqIdx/ftqOffset/pdest/rfWen/fpWen`，验收对应字段在 helper 中命中。
- Task9.4：通过 `plus.sv -> seq_csr_common.sv -> default.cfg` 增加 MDP、RVC、PC、FTQ、pdest 控制参数，验收 `rg "MEMBLOCK_MDP_LOAD_WAIT_WT|MEMBLOCK_PC_BASE|MEMBLOCK_PDEST_RANGE"` 命中。
- Task9.5：在 `memblock_dispatch_base_sequence` 中暴露发射前字段赋值 wrapper，验收 `rg "assign_issue_item_fields|field_assigner"` 命中。
- Task9.6：更新本文档并完成 subagent review、`git diff --check`、远端编译。

验收点：字段赋值 task `rg` 命中，远端编译通过。

### Task10：真实 agent 驱动接入

目标：把调度结果发送到现有 lintsissue/相关 agent，不大改接口。

实现文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_base.sv`
- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`

新增 class：

- `memblock_lintsissue_dispatch_sequence`
  - 继承 `lintsissue_agent_agent_default_sequence`，兼容
    `u_lintsissue_agent_agent.sqr.main_phase` 使用的 agent sequencer。
  - 默认接入 `tc_base.sv` 的 lintsissue default_sequence；当
    `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0` 时直接调用 `super.body()`，保持
    `tc_sanity/base_fun` 现有默认随机/空闲行为。

关键字段：

- `common_data_transaction data`：Task1-9 共享数据入口。
- `issue_queue_scheduler issue_sched`：每拍执行
  `route_all_ready_uids()`、`select_issue_candidates()`、fire 后
  `mark_issue_fire()`，并在轮末 `advance_issue_queue_delays()`。
- `issue_field_assigner field_assigner`：对每个候选调用
  `assign_issue_item_fields()` 填充 lintsissue xaction。
- `enable`：来自 `seq_csr_common::get_dispatch_issue_seq_en()`，默认 0。
- `ready_timeout`：来自 `seq_csr_common::get_dispatch_ready_timeout()`，默认
  1000；dispatch 专用 xaction 等待 DUT ready 的最大周期数。
- `no_progress_warn_cycles`：来自
  `seq_csr_common::get_active_seq_no_progress_warn_cycles()`；连续无 issue fire 时只
  warning，不作为退出条件。

`lintsissue_agent_agent_xaction` 新增 dispatch 专用控制字段：

- `memblock_dispatch_wait_ready`：非随机控制位。默认 0，保持普通默认 sequence
  一拍发送行为；dispatch sequence 置 1，要求 driver 等待真实 valid/ready。
- `memblock_dispatch_ready_timeout`：等待 ready 的超时周期数，防止非法
  backpressure 导致 sequence 永久阻塞。

`lintsissue_agent_agent_driver` 新增握手辅助：

- `wait_dispatch_issue_ready(tr)`：仅当 `tr.memblock_dispatch_wait_ready=1` 时调用。
  driver 先打一拍 xaction，然后每拍检查各 valid 端口对应 ready，ready 后清掉
  该端口 valid 并继续保持未握手端口，直到所有端口完成握手后才
  `item_done()`。
- `has_dispatch_issue_pending(tr)`：判断 xaction 内是否还有未握手 valid 端口。
- `clear_ready_dispatch_issue_ports(tr)`：根据 clocking block 采到的 ready 清除
  已握手端口的 valid。

关键 task/function：

- `pre_body()`：调用父类 objection 逻辑，不做 dispatch helper 初始化，避免
  默认 testcase 在 main phase 前额外操作公共状态。
- `body()`：先初始化 `seq_csr_common` 并读取
  `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN`、ready timeout 和统一 no-progress warning 阈值；默认未使能时调用
  `super.body()`；使能时
  创建/绑定 `common_data_transaction`、`issue_queue_scheduler`、
  `issue_field_assigner`，再进入 `drive_dispatch_issue_loop()`。
- `drive_dispatch_issue_loop()`：每轮先路由 ready uid，再发送一个
  lintsissue xaction，最后递减 issue queue delay；正常退出只看顶层置位的
  `global_stop_requested`。连续无 fire 达到 no-progress 阈值只 warning，不再调用
  `issue_queue_scheduler::has_pending_issue_work()` 做每拍退出扫描。
- `send_issue_cycle()`：创建 `lintsissue_agent_agent_xaction`，先
  `clear_lintsissue_xaction()` 清 7 个 valid 和 payload，再选择 load/STA/STD
  候选，按 load 端口 0/1/2、STA 端口 3/4、STD 端口 5/6 的映射填字段，通过
  `start_item/finish_item` 发给 driver，并设置 dispatch 专用 ready 等待标记。
  无候选时仍发送 all-valid-0 idle xaction，driver 侧按已有时钟消费，避免
  sequence delta-cycle 死循环。
- `mark_fired_items()`：`finish_item()` 返回后对已填入 xaction 的候选调用
  `mark_issue_fire()`，删除队列项、清 queued 状态并置 dispatched 状态。
  由于 dispatch 专用 xaction 会让 driver 等待 valid/ready 全部完成后才
  `item_done()`，这里的 `finish_item()` 返回点等价于本 xaction 内候选已被 DUT
  接收。

参数含义：

- `+MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0/1`：是否启用真实 lintsissue dispatch
  sequence；默认 cfg 中为 0。
- `+MEMBLOCK_DISPATCH_READY_TIMEOUT=<N>`：dispatch 专用 lintsissue item 等待
  DUT ready 的最大周期数；启用 dispatch sequence 且值为 0 时由
  `seq_csr_common` clamp 到 1。计数从首拍 drive 之后的下一次 ready 采样周期开始，
  因此 timeout=1 表示首拍之后再允许 1 个采样周期。
- `+MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES=<N>`：主动主流程统一 no-progress
  warning 阈值；不控制退出。

调度关系：

- 本任务只消费 Task8/Task9 的 issue queue 与字段赋值 helper。
- 不实现完整主任务入队、写回、commit、flush/replay 恢复；这些仍由
  Task11 之后处理。
- 当前 fire 判定是“dispatch 专用 xaction 已由 driver 等到 valid/ready 全部握手
  完成并返回 `item_done()`”。后续 Task11 接 monitor 后可进一步用 monitor 事实
  更新写回/异常状态。

本任务疑问处理：

- 是否默认替换 lintsissue default_sequence：已替换，但通过 plus 默认关闭并
  fall back 到 `super.body()`，因此默认 `tc_sanity/base_fun` 行为不变。
- dispatch sequence 参数是否可以直接读 `plus::`：不直接读。按方案约束，测试框架
  参数统一由 `seq_csr_common` 镜像和合法化，sequence 只通过 getter 读取。
- 无候选时如何避免死循环：发送 all-valid-0 idle xaction，由现有 driver
  每拍消费；正常退出由顶层 `global_stop_requested` 控制，连续无 fire 只打印
  no-progress warning。
- xaction 未随机化时 payload 是否会带 X：`clear_lintsissue_xaction()` 已从
  只清 valid 扩展为 valid 和 payload 全清零，再由 `assign_issue_item_fields()`
  覆盖被选端口，降低未赋字段打到 DUT 的风险。
- `finish_item()` 后立即 `mark_issue_fire()` 是否会误删队列：初版确有风险。
  解决方案是在 `lintsissue_agent_agent_xaction` 中加入 dispatch 专用等待 ready
  标记，driver 对该类 item 按 valid/ready 等待并在全部端口握手后才
  `item_done()`；普通默认 sequence 不置该标记，保持原行为。
- driver 等待 ready 时会清掉 xaction 内已握手端口的 valid：这是 dispatch 专用
  flow 的内部实现，`mark_fired_items()` 使用的是 `send_issue_cycle()` 里独立保存的
  `fired_items` 队列，不依赖 driver 返回后的 xaction valid mask，因此不会丢追踪信息。
- AMO/多 uop 是否展开：不在 Task10 展开，沿用 Task8 队列项和 Task9 字段
  映射，后续真实 AMO 数据/写回语义继续在专项 task 处理。

验收点：

- `rg -n "memblock_lintsissue_dispatch_sequence|MEMBLOCK_DISPATCH_ISSUE_SEQ_EN" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 命中。
- `git diff --check` 通过。
- 远端编译：`cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_sanity mode=base_fun`，最终 `Verdi KDB elaboration done ... 0 error(s), 0 warning(s)`。
- 默认回归：`cd mem_ut/ver/ut/memblock/sim && make eda_run tc=tc_sanity mode=base_fun`，最终 `TEST CASE PASSED`，`UVM_ERROR=0`，`UVM_FATAL=0`。
- subagent 复审：初版指出 `finish_item()` 后立即 `mark_issue_fire()` 缺少 DUT
  ready 事实握手；修复后复审无 blocker。

### Task11：写回、异常、replay/redirect 状态处理

目标：接 monitor 事实，按 uid 条件更新状态，并处理 replay/redirect 同步。

实现文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/status_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/writeback_status_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`

新增/扩展类型：

- `memblock_wb_event_source_e`：标准化事件来源，覆盖 load/store 写回、
  STA/STD feedback、memoryViolation、backend replay 和 redirect；`ATOMIC_WB`
  保留为 AMO/LR/SC 后续扩展点。`SQ_WB/EXCEPTION_INFO` 保留枚举值但当前公共路径不赋值，
  SQ 释放走 LSQ commit/control 路径，异常统一由 `has_exception/exception_vec` 表达。
- `memblock_wb_event_t`
  - `valid/source/port_id`：事件有效、来源和端口编号，用于同拍排序/debug。
  - `target`：事件作用入口，当前支持 `LOAD/STA/STD`。
  - `uid/has_uid`：若 adapter 已经知道 uid，可直接携带。
  - `rob_key/has_rob`、`lq_key/has_lq`、`sq_key/has_sq`：用于通过
    `common_data_transaction` 活跃映射反查 uid；多个 key 同时存在时必须一致。
  - `issue_epoch/has_issue_epoch`、`replay_seq/has_replay_seq`：条件更新快照；未携带
    时从当前状态表目标入口读取。
  - `real_wb_valid/has_exception/exception_vec/replay_valid/redirect_valid`：区分
    normal pass、fault、replay 和 redirect。带异常的 writeback 不置 normal pass。
  - `redirect`：redirect/flush 边界 payload。
  - `vector_ls/uop_index/cycle`：向量 LS 标记、uop 编号和事件周期；初版
    `vector_ls=1` 直接 fatal。

`status_transaction` 扩展字段：

- 入口级结果：`load_writeback/sta_writeback/std_writeback`、
  `load_pass/sta_pass/std_pass`、`load_fault/sta_fault/std_fault`。
- replay 状态：`replay_pending/replay_target_load/replay_target_sta/
  replay_target_std/replay_seq`。
- redirect 状态：`redirect_pending/flushed`。
- 条件更新快照：`load_issue_epoch/sta_issue_epoch/std_issue_epoch`，解决同一 store
  STA/STD 不同入口反馈不能共用单一 `issue_epoch` 的问题。
- 异常记录：`exception_vec/exception_vaddr/exception_gpaddr/last_event_cycle`。
- 函数：
  - `get_target_issue_epoch(target)`：读取目标入口 epoch。
  - `set_target_issue_epoch(target, issue_epoch_i)`：发射成功时记录目标入口 epoch。

`common_data_transaction` 新增 API：

- `exception_event_q[$]`：保存待异常/replay/redirect 处理的标准事件队列。
- `target_writeback_field(target)`、`target_pass_field(target)`、
  `target_fault_field(target)`：目标入口到状态字段的映射。
- `target_entry_done(status, target)`、`required_targets_done(uid)`：判断该 uid 的
  必需入口是否完成。初版按 `fuType` 最小分类：LDU 只需 load，STU/MOU 需要 STA
  和 STD。
- `conditional_set_target_status_field(uid, field, value, target, issue_epoch,
  replay_seq)`：入口级条件更新；要求 active、未 kill、目标 epoch 和 replay_seq
  匹配。
- `mark_target_normal_pass(uid, target, issue_epoch, replay_seq, cycle)`：设置入口
  writeback/pass；所有必需入口完成且无 fault/replay/redirect pending 时置总
  `writeback/pass`。
- `mark_target_fault(uid, target, issue_epoch, replay_seq, exception_vec, cycle)`：
  设置入口 fault、总 fault 和 `exception_pending`，保存异常向量。
- `mark_replay_pending(uid, target, issue_epoch, replay_seq, cycle)`：清对应入口
  dispatched/writeback/pass，置 `replay_pending` 和目标 replay mask，递增
  `replay_seq`。
- `replay_target_requested(status, target)`、`replay_targets_empty(status)`、
  `clear_replay_target_after_fire(uid, target)`：replay 重新入队和重发成功后的
  mask 清理。
- `request_redirect_flush(redirect)`：设置 `flush_in_progress/active_redirect`。
- `apply_redirect_flush(redirect)`：按 `rob_need_flush()` 清 younger 或
  flushItself 条目的队列残留、dispatch/writeback/pass，设置 `flushed`，释放
  active ROB/LQ/SQ 映射；`redirect_pending` 只作为 flush 过程态，flush 完成后清零。
- `make_empty_wb_event()`、`push_feedback_event()`、`pop_feedback_event()`、
  `clear_feedback_events()`：事件队列管理。`push_feedback_event()` 会先调用
  `normalize_feedback_event()`，不允许绕过 uid/epoch/replay_seq 标准化直接入队；
  `make_empty_wb_event()` 逐字段初始化，避免 enum 字段被 `'{default:'0}` 触发
  VCS `ENUMASSIGN`。
- `feedback_event_is_redirect()`、`feedback_event_is_replay()`、
  `feedback_event_has_fault()`、`feedback_event_has_action()`、
  `feedback_event_target_is_valid()`：标准事件分类 helper。
- `normalize_feedback_event(event, normalized_event)`：统一标准化入口，按
  uid/rob/lq/sq 反查 active uid，补齐 `uid/has_uid`、`rob_key/has_rob` 和
  `replay_seq/has_replay_seq`；非 redirect 事件同时要求 target 为 load/STA/STD，
  并补齐目标入口 `issue_epoch/has_issue_epoch`。若该 uid 已经发生过 replay
  (`status.replay_seq != 0`)，非 redirect 反馈必须自带 issue-time
  `issue_epoch/replay_seq` 快照，否则直接丢弃，避免 replay 后旧写回被归到新发射实例。
  redirect 事件不读取 target epoch，允许 `target=NONE`。
- `resolve_uid_for_event(event, uid)`：按 `uid/rob/lq/sq` 多路径反查并交叉校验；
  不一致 fatal，查不到返回 0。
- `get_event_issue_epoch(event, uid)`、`get_event_replay_seq(event, uid)`：统一取得
  条件更新快照。

新增 class：

- `writeback_status_handler`
  - 字段：`data`，指向 `common_data_transaction::get()`。
  - 函数：
    - `event_has_fault(event)`：检查 `has_exception` 或 `exception_vec != 0`。
    - `event_is_redirect(event)`：以 canonical `redirect.valid` 识别
      redirect/memoryViolation；`redirect_valid` 只作为兼容/显式标志，必须与
      `redirect.valid` 一致，不一致 fatal；`source` 只保留来源标签，不能单独触发
      redirect。
    - `event_is_replay(event)`：只通过 `replay_valid` 识别 replay-only/backend
      replay，`source == BACKEND_REPLAY` 不能单独触发 replay。
    - `event_is_normal_pass(event)`：只有 raw writeback 且无 redirect/replay/fault
      时为 normal pass。
    - `handle_event(event)`：标准事件入口；normal pass 直接条件更新状态，
      fault/replay/redirect 推入 `exception_event_q`，vector LS fatal；fault/replay/
      redirect 入队前固化 `issue_epoch/replay_seq`，redirect 事件缺 `rob_key` 时
      先通过 uid/lq/sq 反查并补齐。
  - 已删除旧的 `process_event_queue()` 二次整理 task；`exception_event_q` 中的
    fault/replay/redirect 只由异常恢复 helper 消费。

- `exception_redirect_replay_handler`
  - 字段：`data`，指向 `common_data_transaction::get()`。
  - 函数：
    - `redirect_from_event(event)`：从 canonical `redirect.valid=1` 的事件提取
      `memblock_redirect_payload_t`；payload 无效时 fatal，不靠 ROB key 单独合成
      redirect。
    - `redirect_event_is_older(candidate, best)`：按 ROB 环形顺序选择更老 redirect。
    - `select_oldest_redirect(events, selected)`：同批事件中选最老 redirect。
    - `handle_replay_event(event)`：根据条件快照调用 `mark_replay_pending()`。
    - `handle_fault_event(event)`：消费 fault recovery event；fault target 状态已由 writeback handler 首次调用 `mark_target_fault()` 落表，这里只解析 uid/epoch/replay_seq。
  - task：
    - `process_pending_events()`：从 `exception_event_q` 取出待处理事件；若有
      redirect，先 request freeze，再当前最小实现中直接置 `issue_freeze_ack=1`
      并调用 `apply_redirect_flush()`；随后处理 replay-only 和 fault 事件。

`issue_queue_scheduler` replay 补充：

- `is_uid_route_ready()`：允许 `replay_pending` 的 uid 重新参与队列路由，并由
  replay target mask 限定目标入口。
- `route_target()`：replay 状态下只允许 replay target mask 指定的入口重新入队。
- `mark_issue_fire()`：重发成功后调用 `clear_replay_target_after_fire()`，目标 mask
  清空后清 `replay_pending`。
- `is_issue_item_eligible()`：普通 item 仍禁止非目标 replay；replay item 在
  target mask 命中且 `replay_seq` 匹配时允许再次 fire。

`memblock_dispatch_base_sequence` 新增 task：

- `collect_writeback_events()`：monitor adapter hook，当前 base 实现只打印 high 级别
  信息，不直接采样 monitor。
- `collect_exception_and_redirect_events()`：monitor adapter hook，当前 base 实现为空。
- `exception_redirect_replay_task()`：调用
  `exception_redirect_replay_handler::process_pending_events()`。
- `submit_writeback_event(event)`：directed/unit flow 可直接提交标准事件给
  `writeback_status_handler::handle_event()`。

调度关系：

- 后续真实 monitor adapter 的预期路径为：monitor 采样接口 -> 构造
  `memblock_wb_event_t` -> `writeback_status_handler::handle_event()` ->
  统一经 `normalize_feedback_event()` 标准化 -> normal pass 立即落表，
  fault/replay/redirect 入队 -> `exception_redirect_replay_task()` 处理 replay/fault/redirect。
- replay-only 不走 flush；只清失败入口状态并重新入对应 issue queue。
- redirect/memoryViolation 先置 `flush_in_progress`，issue scheduler 在 flush 期间
  不选新候选并置 `issue_freeze_ack`；real flow 会把 redirect payload 投递给
  `memblock_redirect_dispatch_sequence` 真实驱动 `io.redirect`，等待 drive done 后再
  `apply_redirect_flush()`。TB 自己驱动的 redirect pulse 会在 raw queue 入口过滤，避免
  redirect recovery 触发源来自 DUT output ctrl monitor 的 memoryViolation，不再由 redirect monitor 回采 input redirect 触发。
- 本任务不驱动 LSQ commit、不释放 LQ/SQ 资源；这些仍由 Task12 的 commit/deq
  task 处理。

问题与解决：

- 问题：同一 store 的 STA/STD 反馈可能不是同一拍，也不能共用单个 uid 级
  issue epoch；必须按 `status.get_target_issue_epoch(target)` 读取对应 target 的
  issue epoch。
- 解决：状态表新增 `load_issue_epoch/sta_issue_epoch/std_issue_epoch`，发射成功时
  同步记录目标入口 epoch，写回/异常按目标入口条件更新。
- 问题：replay 只置 `replay_pending` 会让 scheduler 因 `replay_pending=1` 永远
  不再路由该 uid。
- 解决：新增 replay target mask 路由分支；只有指定入口可重新入队；同时放开
  `is_issue_item_eligible()` 中 replay target item 的 fire 条件，
  重发 fire 后清对应 mask。`mark_replay_pending()` 会先调用
  `remove_uid_from_issue_queues()` 清掉同 uid 旧 replay_seq 队列残留和 queued flag，
  避免 STA/STD 或 load 旧项卡住重新入队。
- 问题：fault/replay/redirect 事件若延后处理且未携带 epoch/replay_seq，后续从
  当前状态表读取会把旧事件应用到新发射实例。
- 解决：`common_data_transaction::normalize_feedback_event()` 在事件入队前统一解析
  uid，并把当前 `issue_epoch/replay_seq` 写回事件；`submit_writeback_event()` 和
  直连 `push_feedback_event()` 都走这一路径，exception handler 只消费事件携带的快照。
- 问题：subagent 复审指出 replay 发生后，若旧 issue 的 late writeback 没有携带原始
  `issue_epoch/replay_seq`，normalize 会用 replay 后的新状态补齐，可能误把旧写回
  标记为新 replay 实例 pass。
- 解决：保留首次 issue 的简化补齐能力；一旦 `status.replay_seq != 0`，非 redirect
  反馈必须显式携带 issue-time `issue_epoch` 和 `replay_seq`，否则
  `normalize_feedback_event()` warning 后丢弃。后续真实 monitor adapter 接入时必须在
  发射侧保存并回传这两个快照。
- 问题：redirect 事件可能只携带 uid/lq/sq，不一定携带 rob_key。
- 解决：redirect 事件也统一走 `resolve_uid_for_event()`，缺 `rob_key` 时用
  `status.get_rob_key()` 补齐；若事件已经带 `redirect.valid/redirect.rob_key` 但未置
  `has_rob`，则先从 redirect payload 补 `has_rob`。redirect 标准化不读取 target
  epoch，因此允许 `target=NONE`；`redirect_from_event()` 只处理已补齐的标准事件。
- 问题：早期实现把 `source == BACKEND_REPLAY`、`source == REDIRECT/MEMORY_VIOLATION`
  作为 replay/redirect 的行为触发条件，和“source 只表示来源”的设计冲突。
- 解决：`common_data_transaction`、`writeback_status_handler` 和
  `exception_redirect_replay_handler` 的 replay/redirect 分类统一改为只看显式行为字段：
  replay 只看 `replay_valid`，redirect 以 canonical `redirect.valid` 为准。
  `redirect_valid` 作为兼容/显式标志必须与 `redirect.valid` 一致，不一致 fatal。
  `BACKEND_REPLAY`、`REDIRECT`、`MEMORY_VIOLATION` 只用于 debug/追踪来源，不能单独触发
  replay/redirect；adapter 构造 redirect/memoryViolation 时必须同步设置两个 redirect valid 位。
- 问题：redirect flush 后若只清队列、不释放 active 映射，后续 ROB/LQ/SQ 复用或
  迟到反馈可能撞到旧 uid。
- 解决：`apply_redirect_flush()` 对需要 flush 的 uid 设置 `flushed`，再调用
  `retire_active_uid()` 释放 active ROB/LQ/SQ 映射；`clear_uid_dispatch_result()`
  清 dispatch/writeback/pass/replay 过程态，不删除主表、TLB 表和状态表记录。
- 问题：未知 feedback 事件若反复 push 回 `exception_event_q`，可能造成
  recovery handler 自循环或长期残留。
- 解决：`writeback_status_handler::handle_event()` 对无法分类为 normal pass、
  fault、replay 或 redirect 的事件打印 high 级别信息并丢弃，不重新入队。
- 问题：`common_data_transaction` 是否能调用 `lsq_ctrl_model::derive_op_behavior()`
  判断必需入口。
- 解决：不能直接调用，否则 `tc_pkg.sv` include 顺序会出现公共数据 owner 依赖后续
  helper 的环。Task11 在 `required_targets_done()` 中只用 `fuType` 做最小判断：
  LDU 需要 load，STU/MOU 需要 STA+STD。
- 问题：真实 monitor 是否已经能产出 writeback/redirect/replay event。
- 解决：还不能。subagent 和本地搜索确认 `io_mem_to_ooo_int_wb`、
  `io_mem_to_ooo_ctrl`、`io_mem_to_ooo_iq_feedback` monitor 的
  `mon_item_port.write(mon_tr)` 仍在 `xxxTODOxxx` 注释块中。Task11 因此只完成标准
  事件/API 和恢复骨架；真实功能闭环需要后续小任务优先接 int WB、ctrl
  memoryViolation、STA IQ feedback monitor adapter。
- 问题：标准事件是否用 packed struct。
- 解决：使用 unpacked struct，因为含 enum 和 packed key 但不含 queue/string/class
  handle；可安全放入 queue。初始化逐字段完成，减少 VCS 聚合赋值风险。

验收点：

- `rg -n "writeback_status_handler|exception_redirect_replay_handler|memblock_wb_event_t|mark_target_normal_pass|apply_redirect_flush|submit_writeback_event" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 命中。
- `git diff --check` 通过。
- 远端编译：`cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_sanity mode=base_fun`
  通过，VCS/Verdi KDB 结果为 `0 error(s), 0 warning(s)`。
- 默认回归：`cd mem_ut/ver/ut/memblock/sim && make eda_run tc=tc_sanity mode=base_fun`，
  最终修复后通过 `TEST CASE PASSED`，`UVM_ERROR=0`、`UVM_FATAL=0`。

Subagent review：

- Faraday 复审指出 payload-only redirect 未被两个 handler 识别为 redirect；已修复
  `event_is_redirect()` 并重跑编译/仿真。
- Newton 复审指出 replay 后缺少 issue-time epoch/seq 的 late writeback 可能污染新
  发射实例；已在 `normalize_feedback_event()` 中增加 replay 后快照必需规则。
- Kepler 终审确认无 blocker/high，可以提交。

残余风险：

- 真实 monitor adapter 尚未接入，`io_mem_to_ooo_int_wb`、`io_mem_to_ooo_ctrl`、
  `io_mem_to_ooo_iq_feedback` 等 monitor 仍未产出标准 `memblock_wb_event_t`。Task11
  先完成标准事件、状态更新和 replay/redirect 恢复骨架，后续任务需要接真实 monitor
  事实并确保 adapter 携带 issue-time `issue_epoch/replay_seq`。

### Task12：LSQ commit 驱动与 deq 清理

目标：实现 ROB commit 语义驱动和 `lqDeq/sqDeq` 资源释放。

验收点：commit task `rg` 命中，远端编译通过，最小正常流仿真通过。

实现文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_base.sv`

接口边界确认：

- Scala 源码中 `ooo_to_mem.lsqio` 包含 `lcommit/scommit/commit/pendingPtr/pendingPtrNext`。
- 当前生成的 `build_memblock/rtl/MemBlock.sv` 和 UT `dut_inst.sv` 只暴露并连接
  `io_ooo_to_mem_lsqio_pendingPtr_flag/value`，没有 `lcommit/scommit/commit/pendingPtrNext`
  可驱动端口。
- 当前 `lsqcommit_agent` 还驱动 `io_ooo_to_mem_flushSb`，但它属于
  `ooo_to_mem.flushSb`，不是 LSQ commit bundle 字段。
- 当前可观察的 LSQ 释放事实来自 `io_mem_to_ooo_lqDeq/sqDeq` 和
  `io_mem_to_ooo_lqDeqPtr/sqDeqPtr`。因此 Task12 不伪造缺失端口，只实现
  `pendingPtr` 驱动和 deq 事实清理 helper。

新增常量：

- `MEMBLOCK_COMMIT_WIDTH=8`：最小 ROB commit 批次宽度，参考源码 `CommitWidth`。
  本任务用于限制每轮 pendingPtr 推进最多覆盖的连续已 pass uid。

`lsq_ctrl_model` 调整：

- 新增 `static lsq_ctrl_model m_inst` 和 `static function lsq_ctrl_model get()`：
  为入队、commit/deq helper 提供共享 LSQ 软件资源镜像。
- `memblock_dispatch_base_sequence::pre_body()` 从 `lsq_ctrl_model::type_id::create()`
  改为 `lsq_ctrl_model::get()`，并继续在主 dispatch flow 入口调用 `reset()`。这样
  Task12 的 commit/deq helper 读取的 head/free count 与 Task6 入队侧维护的是同一份状态。

`common_data_transaction` 新增 API：

- `release_uid_lq_mapping(uid)`：
  - 参数 `uid`：主表/状态表权威索引。
  - 功能：仅释放该 uid 的 active LQ 映射，清 `active_lq_mapped`；`lsq_deq`
    只有在 `active_lq_mapped=0` 且 `active_sq_mapped=0` 时才置 1，不清 ROB active
    映射，不删除主表或状态历史。
- `release_uid_sq_mapping(uid)`：
  - 参数 `uid`：主表/状态表权威索引。
  - 功能：仅释放该 uid 的 active SQ 映射，清 `active_sq_mapped`；`lsq_deq`
    同样表示该 uid 的所有 LSQ 资源均已释放。
- `try_retire_committed_uid(uid)`：
  - 参数 `uid`：主表/状态表权威索引。
  - 功能：只有 `active=1`、`rob_commit=1` 且 LQ/SQ active 映射均已释放时，才置
    `success` 并调用 `retire_active_uid(uid)` 释放 ROB active 映射。这样避免 load
    在 ROB commit 后但 LQ deq 尚未出现时被过早 retire。

新增 class：`lsq_commit_handler`

- 字段：
  - `common_data_transaction data`：公共数据单例，读取主表/status 并更新 commit/deq 状态。
  - `lsq_ctrl_model lsq_ctrl`：LSQ 软件资源镜像，用于 `release_lq/release_sq`
    更新 head/free count。
  - `commit_cursor_uid`：ROB commit 扫描游标。按当前一轮测试主表 uid 与 ROB 顺序一致的
    简化假设，从低 uid 向高 uid 选择连续可 commit 项。
  - `last_pending_ptr`：上一轮驱动的 pendingPtr；空闲轮次保持该值，避免无候选时随机改变
    store commit 边界。
- 函数：
  - `bind_lsq_ctrl(ctrl)`：绑定外部 LSQ 资源模型；参数 `ctrl` 是 `lsq_ctrl_model`
    句柄，不能为空。
  - `ensure_handles()`：兜底获取 `common_data_transaction`，并在没有绑定时获取
    `lsq_ctrl_model::get()` 单例；commit/deq helper 必须和入队侧共享同一个 LSQ
    软件资源镜像，不能创建独立模型。
  - `report_deq_mismatch(msg)`：deq monitor 事实无法反查 active uid 时的统一处理。
    默认 `uvm_fatal`，只有 `MEMBLOCK_LSQ_RESYNC_ON_MISMATCH=1` 时允许 warning 并跳过
    本批释放，用于后续调试 monitor 时序。
  - `uid_is_commit_candidate(uid)`：判断 uid 是否可 ROB commit。要求 active、总
    `writeback/pass=1`、所有必需入口完成、未 commit、无 fault/replay/redirect/flush。
  - `advance_commit_cursor_past_done()`：跳过已 `rob_commit/success/flushed` 的 uid。
  - `select_rob_commit_batch(uids)`：从 `commit_cursor_uid` 开始选择最多
    `MEMBLOCK_COMMIT_WIDTH` 个连续可 commit uid；遇到第一个不可 commit 项停止，保持最小
    ROB head 顺序语义。
  - `clear_lsqcommit_xaction(tr)`：清 `flushSb`，并把 pendingPtr 设为
    `last_pending_ptr`。
  - `build_lsqcommit_xaction(tr, commit_uids, has_commit)`：创建
    `lsqcommit_agent_agent_xaction`，若有 commit 批次则把 pendingPtr 推到本批最后一条的
    ROB key；输出 `commit_uids` 供 driver item 完成后更新状态。
  - `mark_rob_commit_uid(uid)`：置 `commit/rob_commit/last_event_cycle`。若该 uid
    没有 LQ/SQ 映射，则同时置 `lsq_deq=1` 并尝试 retire。
  - `mark_rob_commit_batch(uids)`：按批次逐项调用 `mark_rob_commit_uid()`。
  - `lq_deq_start_key(deq_ptr, count, ptr_is_next)` 和
    `sq_deq_start_key(deq_ptr, count, ptr_is_next)`：把 DUT deq pointer 转换为本批释放起点。
    当前默认 `ptr_is_next=1`，因为源码中 `lqDeq/sqDeq` 是寄存后的释放计数，
    `lqDeqPtr/sqDeqPtr` 输出释放后的 head。
  - `apply_dut_lq_deq(count, deq_ptr, ptr_is_next=1)`：根据 DUT monitor 采到的
    `lqDeq` 和 `lqDeqPtr` 释放软件 LQ 资源。实现先逐个 LQ key 反查 active uid，
    并先检查计算出的释放起点必须等于 `lsq_ctrl.lq_deq_ptr`；只有 head 一致且整批 key
    都能匹配 active uid 后，才调用 `lsq_ctrl.release_lq(count)` 并逐项
    `release_uid_lq_mapping()`；若中途发现 stale/double/out-of-order deq，不修改软件资源镜像。
  - `apply_dut_sq_deq(count, deq_ptr, ptr_is_next=1)`：根据 DUT monitor 采到的
    `sqDeq` 和 `sqDeqPtr` 释放软件 SQ 资源。实现同样先检查释放起点等于
    `lsq_ctrl.sq_deq_ptr` 并整批预校验，再 release SQ count 和状态映射，避免 flush
    后旧 deq 或非 head deq 污染 head/free count。
  - `apply_raw_ctrl_deq(lq_count, lq_ptr, sq_count, sq_ptr, ptr_is_next=1)`：从 raw ctrl
    monitor sync 路径输入的 `lqDeq/sqDeq/*DeqPtr` 调用上述两个 deq helper。参数
    `ptr_is_next` 用于后续 monitor 时序校准。

新增 class：`memblock_lsqcommit_dispatch_sequence`

- 继承 `lsqcommit_agent_agent_default_sequence`，兼容现有 lsqcommit agent sequencer。
- 字段：
  - `data`：公共数据单例。
  - `commit_handler`：`lsq_commit_handler` 句柄。
  - `lsqcommit_vif`：从 `uvm_config_db` 获取的 lsqcommit agent virtual interface，
    用于把启动等待和主表等待对齐到 `@(posedge lsqcommit_vif.clk)`，避免 `#1`
    time-step 空转。
  - `enable`：来自 `seq_csr_common::get_lsqcommit_seq_en()`，默认 0。
  - `no_progress_warn_cycles`：来自
    `seq_csr_common::get_active_seq_no_progress_warn_cycles()`；连续没有 commit/flushSb
    progress 时只 warning，不作为退出条件。
- task/function：
  - `pre_body()`：沿用父类 objection。
  - `body()`：初始化 `seq_csr_common` 并读取 plus；未使能时调用 `super.body()`，保持
    默认 testcase 行为；使能后创建 helper 并进入 `drive_lsqcommit_loop()`。
  - `drive_lsqcommit_loop()`：global stop 前常驻运行，每拍尝试 commit batch 或
    pending `flushSb`；没有 progress 时只按统一 no-progress 阈值 warning。global stop
    之后如果还有 scheduled/pending/waiting `flushSb`，继续 drain，完成后退出。
  - `send_lsqcommit_cycle(cycle_idx, has_commit)`：调用 handler 构造 xaction，通过
    `start_item/finish_item` 发给 driver；driver item 完成后调用
    `mark_rob_commit_batch()` 更新状态。
  - `configure_from_plus()`：读取 `MEMBLOCK_LSQCOMMIT_SEQ_EN`、统一 no-progress
    warning 阈值和 flushSb 参数。
  - `ensure_helpers()`：获取公共数据并创建 commit handler，同时把
    `lsq_ctrl_model::get()` 绑定给 handler，保证入队与 deq 共享单例资源模型。
  - `ensure_lsqcommit_vif()`：优先按当前 sequence full name 从 `uvm_config_db` 获取
    `vif`，失败后按 `uvm_test_top.env.u_lsqcommit_agent_agent*` 回退查找，保证本
    sequence 能直接等待 agent 接口时钟。
  - `wait_clock_tick()`：统一执行 `@(posedge lsqcommit_vif.clk)`，用于替代旧的
    `#1` time-step 等待。
参数：

- `+MEMBLOCK_LSQCOMMIT_SEQ_EN=0/1`：是否启用 lsqcommit pendingPtr sequence，默认 0。
- `+MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES=<N>`：主动主流程统一 no-progress
  warning 阈值；不控制退出。

调度关系：

- Task12 不自动生成主表、不执行入队、TLB、发射或写回；只消费 Task11 已置
  `writeback/pass` 的状态。
- 正常流预期顺序为：写回 handler 置 `writeback/pass` -> lsqcommit sequence 选择连续
  ready uid 并驱动 pendingPtr -> `mark_rob_commit_batch()` 置 `rob_commit` ->
  后续 `io_mem_to_ooo_ctrl` raw monitor adapter 调用 `apply_raw_ctrl_deq()` ->
  deq helper 释放 LQ/SQ active 映射 -> `try_retire_committed_uid()` 在所有资源释放后
  retire active ROB 映射。
- 默认 `tc_base.sv` 已把 lsqcommit default sequence 替换为
  `memblock_lsqcommit_dispatch_sequence`，但 plus 默认关闭时会 fall back 到原
  `lsqcommit_agent_agent_default_sequence`。

问题与解决：

- 问题：方案要求“按源码提交逻辑驱动 LSQ commit 端口”，但当前 RTL/TB 没有
  `lcommit/scommit/commit/pendingPtrNext` 端口。
- 解决：不改 DUT 连接、不伪造端口。Task12 只驱动当前可达的 `pendingPtr`，把完整
  ROB commit bundle 覆盖记录为残余风险。后续 RTL/top 重新暴露端口后再扩展 agent。
- 问题：lsqcommit agent default sequence 与主 dispatch flow 在 main_phase 并行启动，
  可能先于主表生成/写回事实启动。
- 解决：启用 `memblock_lsqcommit_dispatch_sequence` 后先等待 `main_table_ready`，之后在
  global stop 前常驻运行；未出现 commit candidate 时只发 idle/无 commit item 或等待下一拍，
  不通过 start timeout/idle stop 提前退出。误使能或卡死由 no-progress warning 和
  testcase/UVM timeout 暴露。
- 问题：`lqDeq/sqDeq` 是否能在软件 commit 时提前释放。
- 解决：不能。参考 Scala 源码，LQ deq 来自连续 allocated+committed entry，SQ deq
  来自 store queue 真正出队到 sbuffer/uncache 完成后的事实，并且输出有寄存延迟。
  Task12 只在 DUT monitor 事实到达后调用 `release_lq/release_sq` 和 active LQ/SQ
  映射释放。
- 问题：deq 事实如果与软件 active 映射不一致，是否可以先更新 LSQ free count 再跳过
  状态项。
- 解决：不可以。Task12 先检查 DUT deq 起点等于软件 head，再整批预校验 deq key ->
  active uid 映射，全部匹配后才修改 `lsq_ctrl_model` 和状态表；默认 mismatch 是 fatal，
  调试场景可通过 `MEMBLOCK_LSQ_RESYNC_ON_MISMATCH=1` 降级为 warning 并跳过本批释放。
- 问题：commit 后是否立刻清 `active`。
- 解决：不立刻清。`rob_commit` 与 `lsq_deq` 分阶段记录；只有 LQ/SQ 映射均释放后才
  调 `retire_active_uid()`。非 LSQ atomic 或没有 LQ/SQ 映射的条目 commit 后可直接 retire。
- 问题：deq pointer 表示释放前 head 还是释放后 head。
- 解决：源码中 `lqDeq/sqDeq` 是寄存后的 deq count，`lqDeqPtr/sqDeqPtr` 输出当前
  queue head。Task12 默认按释放后 head 处理，即根据 `count` rewind 得到释放起点。
  helper 保留 `ptr_is_next` 参数，后续真实 monitor 对齐若发现端口语义不同，可切换为
  `ptr_is_next=0`。

残余风险：

- `io_mem_to_ooo_ctrl_agent` monitor 当前通过 raw monitor sync 路径交给 adapter 统一消费，
  Task12 保留 `apply_raw_ctrl_deq()` 标准入口；真实 deq 处理不再走 agent xaction 直连入口。
- 当前 pendingPtr 只能近似 store 达到 ROB head 的边界，不能表达每周期 ROB commit
  数量、load commit 数量或 `pendingPtrNext`。
- LQ deq 主要依赖 load writeback 或 vector load commit 内部状态，lsqcommit agent
  不能单独制造完整 LQ deq 场景。

验收记录：

- subagent 复审确认 Task12 的 `lsq_ctrl_model` 单例共享、deq 整批预校验、mismatch
  默认 fatal/resync skip、`lsq_deq` 全资源语义、lsqcommit 启动 gate 和文档补充均已闭环，
  无新的 high/medium finding。
- `git diff --check` 覆盖 Task12 文件集通过。
- `cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_sanity mode=base_fun`
  通过，日志显示 `Verdi KDB elaboration done ... 0 error(s), 0 warning(s)`。
- `cd mem_ut/ver/ut/memblock/sim && make eda_run tc=tc_sanity mode=base_fun`
  通过，`base_fun/log/tc_sanity_666666_rtl_.log` 显示 `TEST CASE PASSED`，
  `UVM_ERROR=0`、`UVM_FATAL=0`。

### Task13：端到端最小场景与回归验收

目标：启动最小 dispatch flow，完成主表生成、入队、TLB、队列、发射、写回、异常/commit 的串联验收。

验收点：最小 1 条 load、store、AMO directed case 仿真通过；默认 `tc_sanity/base_fun` 不回退。

实现初稿：

- 新增 `mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv`，继承 `memblock_dispatch_base_sequence`，作为默认不启用的软件侧端到端 smoke vseq。
- 新增 `mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_dispatch_smoke.sv`，继承 `tc_smoke` 的基础环境配置，在 testcase `build_phase` 中把相关 agent 的 `main_phase default_sequence` 覆盖为已有空 `tcnt_default_sequence_base#(xaction)`，保留 driver idle 驱动但不发真实随机 xaction；在 `main_phase` 中显式 `start(null)` 启动 smoke vseq。
- 更新 `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`，在 `memblock_dispatch_base_sequence.sv` 后 include `soft_test_memblock_dispatch_smoke_sequence.sv`，并在 `tc_smoke.sv` 后 include `soft_test_tc_dispatch_smoke.sv`。

流程串联：

- directed 主表固定 3 条：uid0 为 1 条 load，uid1 为 1 条 store，uid2 为 1 条 AMO；ROB 顺序为 0、1、2，地址分别落在不同 4KB 页。
- LSQ admission 复用 `lsq_ctrl_model::commit_allocate()` 和 `commit_non_lsq_admission()`；load 分配 LQ，store 分配 SQ，AMO 走非 LSQ allocation admission。
- TLB build 复用 `build_tlb_table_for_active_uid()`，并由该入口触发 issue queue routing；后续再调用 `route_all_issue_queues()` 保证队列状态收敛。
- issue 选择和发射复用 `select_issue_candidates()`、`mark_issue_item_fire()`；`tc_dispatch_smoke` 已覆盖默认 agent sequence 为空 sequence，不驱动真实 `lintsissue/lsqcommit/lsqenq` 等 agent 随机 transaction，只更新软件模型中的 dispatched/epoch 状态。
- writeback event 复用 `submit_writeback_event()`，为每个 fired issue item 注入 normal pass event，按 target 设置 load/STA/STD 来源并携带 uid、ROB、issue_epoch、replay_seq。
- ROB commit 与 LQ/SQ deq 复用 `lsq_commit_handler::build_lsqcommit_xaction()`、`mark_rob_commit_batch()`、`apply_dut_lq_deq()`、`apply_dut_sq_deq()`；最终检查 active ROB/LQ/SQ map 清空、LSQ free count 恢复、每个 uid 的 commit/success/target pass 状态。

疑问和解决：

- 疑问：Task13 是否需要拉起真实 load/STA/STD agent sequence。
  解决：本阶段目标是软件侧端到端 smoke，不驱动真实 DUT load/STA/STD agent；真实 agent 联动保留给后续回归验证。
- 疑问：是否新增 plus 参数控制 smoke。
  解决：不新增 plus 参数；`tc_dispatch_smoke` 是独立 testcase，默认 `tc_sanity` 不会启动该 vseq。
- 疑问：AMO 是否需要 LQ/SQ deq。
  解决：沿用现有 `lsq_ctrl_model::derive_op_behavior()`，AMO 为 MOU、STA/STD issue target、无 LSQ allocation；ROB commit 后由 `try_retire_committed_uid()` 直接 retire。
- 疑问：继承 `tc_smoke/tc_base` 后是否还会有 agent default sequence 并行驱动 DUT。
  解决：`tc_dispatch_smoke::configure_software_smoke_default_sequences()` 将 `tc_base` 配置的相关 agent default sequence 覆盖为已有空 `tcnt_default_sequence_base#(xaction)`。这样 sequencer/driver 仍存在，driver 可保持 idle 驱动，但不会发送默认随机 xaction，软件侧 smoke 的状态闭环不会被真实 agent stimulus 污染。

静态验收点：

- `git diff --check` 通过。
- `rg -n "soft_test_memblock_dispatch_smoke_sequence|tc_dispatch_smoke" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 命中新增 class、include 和文档记录。
- subagent 复审发现 `tc_dispatch_smoke` 初版未隔离继承来的 agent default sequence；已通过空 default sequence 覆盖修复。
- `make eda_compile tc=tc_sanity mode=base_fun` 通过，`Verdi KDB elaboration done ... 0 error(s), 0 warning(s)`。
- `make eda_run tc=tc_dispatch_smoke mode=base_fun` 初版通过，日志显示 `dispatch software smoke sequence completed`、`TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`。
- `make eda_run tc=tc_sanity mode=base_fun` 初版防回退通过，`TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`。
- 修复 default sequence 隔离后复跑 `make eda_run tc=tc_dispatch_smoke mode=base_fun` 通过，日志显示 `dispatch software smoke sequence completed`、`TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`。
- 修复 default sequence 隔离后复跑 `make eda_run tc=tc_sanity mode=base_fun` 通过，日志显示 `TEST CASE PASSED`、`UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。

### Task14：真实 LSQ 入队 admission sequence

目标：补齐真实 `lsqenq_agent` admission 路径。Task6 只有软件 LSQ 分配模型，Task13 软件 smoke 也不驱动真实 LSQ enqueue 端口；Task14 新增默认关闭的 `memblock_lsqenq_dispatch_sequence`，在 plus 使能后按公共主表驱动 LSQ 入队，并在真实发送完成点更新主表、状态表、TLB 表和 issue queue。

实现文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv`
- `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_base.sv`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`

实现 class/API：

- `memblock_lsqenq_dispatch_sequence`
  - 字段：
    - `data`：`common_data_transaction::get()` 单例句柄，读取主表、状态表和写 issue queue。
    - `lsq_ctrl`：`lsq_ctrl_model::get()` 单例句柄，负责 LSQ 资源预览、DUT resp 校验和正式指针推进。
    - `tlb_builder`：admission 成功后为该 uid 生成 TLB 表。
    - `issue_sched`：admission 和 TLB 表完成后把 uid 路由到 load/STA/STD 发射队列。
    - `enable`：由 `MEMBLOCK_LSQENQ_SEQ_EN` 控制，默认 0。真实 dispatch smoke cfg 显式置 1，普通 testcase 不生成主表时保持安全默认 sequence。
    - `ready_timeout`：driver 等待 `io_ooo_to_mem_enqLsq_canAccept` 的最大周期数，对应 `MEMBLOCK_LSQENQ_READY_TIMEOUT`。
    - `no_progress_warn_cycles`：来自 `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`，用于等待主表或 admission loop 连续无进展时周期性 warning，不作为退出条件。
    - admission 起点不再由本 sequence 保存本地 `next_admit_uid`；每轮从 `common_data_transaction::get_next_new_admit_uid()` 推导。这样 redirect flush 回退公共 `max_enqueued_uid` 后，老 uid 会自然重新成为下一条 admission 候选。
  - task/function：
    - `body()`：初始化 `seq_csr_common`，读取 plus；未使能时回退 `lsqenq_agent_agent_default_sequence::body()`。
    - `wait_for_main_table()`：使能后先等待 `common_data_transaction.main_table_ready=1`，防止 agent default sequence 早于主控 vseq 建表完成时读到半成品主表。
    - `drive_lsqenq_loop()`：forever 调度 admission，每轮调用 `send_lsqenq_cycle()`；LSQENQ 不再有专用 max_cycles/idle stop/start timeout，正常退出只由顶层 `global_stop_requested` 控制。
    - `send_lsqenq_cycle(cycle_idx, has_progress)`：优先处理 `needAlloc=0` 的 non-LSQ admission；若当前 uid 需要 LSQ entry，则收集一批候选、填充 `lsqenq_agent_agent_xaction`、交给 driver，返回后确认状态。
    - `next_uid_needs_lsq_admission(uid, main_tr, behavior)`：跳过已经 `enq` 的 uid，拒绝 active/exception/replay/redirect/flushed 状态项，返回下一条待 admission 主表项和行为模板。
    - `collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys)`：从 `common_data_transaction::get_next_new_admit_uid()` 推导 admission 起点，按 uid 顺序最多收集 `seq_csr_common::get_enq_per_cycle()` 条 LSQ allocation 候选；固定模式返回 `MEMBLOCK_ENQ_PER_CYCLE`，随机模式在 `[1:real_enq_width]` 内均匀随机。函数使用临时 LQ/SQ tail/free-count 做同拍预览，避免同一拍多条候选复用同一个 `lqIdx/sqIdx`。
    - `clear_lsqenq_xaction(tr)`：清 8 个 `needAlloc/req` 槽位，避免未使用 slot 残留随机值。
    - `assign_lsqenq_slot(tr, slot, uid, main_tr, behavior, lq_key, sq_key)`：把候选主表字段填入对应 LSQ enqueue slot，包括 `needAlloc`、`valid`、`fuType`、`uopIdx`、`robIdx`、预览的 `lqIdx/sqIdx` 和 `numLsElem`。
    - `set_need_alloc()`、`set_req_fields()`、`get_resp_keys()`：8 槽扁平接口字段的集中读写 helper。
    - `confirm_lsq_candidates(tr, uids, trs, behaviors, has_progress)`：driver 返回后读取 xaction 中采样的 `resp.lqIdx/sqIdx`，逐条调用 `lsq_ctrl.commit_allocate_with_resp()`，再调用 `complete_admission()`。
    - `complete_admission(uid)`：调用 `tlb_builder.build_tlb_for_uid(uid)` 生成 TLB 表，并调用 `issue_sched.route_uid(uid)` 路由发射队列。
    - `admit_non_lsq_if_ready(has_progress)`：对 AMO/LR/SC 等 `needAlloc=0` 条目调用 `commit_non_lsq_admission()`，不驱动 LSQ enqueue 端口。
    - `admission_blocked_by_flush()`：检查 `common_data_transaction.flush_in_progress`、`active_redirect.valid` 和 `memblock_sync_pkg::dispatch_flush_in_progress`。若 flush/redirect 已开始，则不选择新的 LSQ admission 候选。

- `common_data_transaction`
  - 新增字段：
    - `main_table_ready`：主表生成完成标志。`reset_all_tables()` 清 0，`check_main_table_complete()` 完整检查通过后置 1；Task14 等该位，而不是只等 `next_uid == main_trans_num`。
  - redirect 同步：
    - `reset_all_tables()` 清 `memblock_sync_pkg::dispatch_flush_in_progress` 和 `dispatch_flush_epoch`。
    - `request_redirect_flush()` 置 `dispatch_flush_in_progress=1` 并递增 `dispatch_flush_epoch`。
    - `apply_redirect_flush()` 完成后清 `dispatch_flush_in_progress`。

- `memblock_sync_pkg`
  - 新增字段：
    - `dispatch_flush_in_progress`：agent driver 可见的轻量 flush/redirect 同步门控，避免 agent 包依赖 `common_data_transaction`。
    - `dispatch_flush_epoch`：每次 redirect/flush 请求递增。LSQ enqueue item 发送前保存 epoch，确认前若 epoch 变化，则判定本次 admission 与 redirect 发生竞争，不更新软件状态。

- `lsqenq_agent_agent_xaction`
  - 新增字段：
    - `memblock_dispatch_wait_can_accept`：dispatch sequence 专用等待标志；为 0 时 driver 保持原单拍发送行为。
    - `memblock_dispatch_ready_timeout`：等待 `canAccept` 的最大周期数；0 表示不设超时。
    - `memblock_dispatch_aborted_by_redirect`：driver 等待 `canAccept` 期间检测到 flush/redirect epoch 变化时置 1，sequence 据此跳过 LSQ 状态确认。
    - `memblock_dispatch_flush_epoch`：sequence 发送 item 前记录的 flush epoch 快照。

- `lsqenq_agent_agent_driver`
  - 新增 task：
    - `wait_lsq_can_accept(tr)`：当 `memblock_dispatch_wait_can_accept=1` 时，driver 保持当前 item，直到 DUT `io_ooo_to_mem_enqLsq_canAccept=1`；等待期间若 `dispatch_flush_in_progress=1` 或 `dispatch_flush_epoch` 与 xaction 快照不一致，则置 `memblock_dispatch_aborted_by_redirect=1`、拉空端口并返回；超过 timeout fatal。
    - `sample_lsqenq_resp(tr)`：dispatch item 发送后采样 `canAccept` 和 8 个 `resp.lqIdx/sqIdx` 字段回填到 xaction，供 sequence 在 `finish_item()` 返回后校验和确认状态。
  - 原默认随机 item 不设置 `memblock_dispatch_wait_can_accept`，因此原有 default sequence 行为不变。

- `lsq_ctrl_model`
  - 新增函数：
    - `commit_allocate_with_resp(uid, behavior, tr, dut_lq_key, dut_sq_key)`：真实 LSQ enqueue fire 后调用；先用当前本地指针 `preview_allocate()` 得到期望 key，再与 DUT resp 比较；一致后回填主表、`activate_uid()`、置 `enq=1` 并推进本地 LQ/SQ 指针和 free count。

- `seq_csr_common`
  - 新增字段/getter：
    - `lsqenq_seq_en/get_lsqenq_seq_en()`
    - `lsqenq_ready_timeout/get_lsqenq_ready_timeout()`
    - `active_seq_no_progress_warn_cycles/get_active_seq_no_progress_warn_cycles()`
  - `validate_and_clamp()`：`MEMBLOCK_LSQENQ_SEQ_EN=1` 时 `ready_timeout` 为 0 会 clamp 到 1；`real_lsq_enq_max` 和 `real_enq_width` clamp 到 1 到 8，且两者必须相等，避免 LSQ enqueue 扁平接口 slot 越界。`MEMBLOCK_ENQ_PER_CYCLE` 固定值不再自动 clamp，配置为 0 或超过 `real_enq_width` 时 fatal，避免 testcase 以为发 16 条但实际被静默压成 8 条。

调度关系：

- 上游必须先完成主表生成和 `status_by_uid[]` 初始化；Task14 不自己生成主表。
- `tc_base` 将 `u_lsqenq_agent_agent` 默认 sequence 替换为 `memblock_lsqenq_dispatch_sequence`，该 sequence 默认 `enable=0`，普通 testcase 回退原 `lsqenq_agent_agent_default_sequence`；真实 dispatch smoke 通过 cfg 显式打开后执行真实 LSQ admission。
- 使能后 sequence 会先等待 `common_data_transaction.main_table_ready=1`，避免和主控 vseq 并行启动时读到半成品主表。
- 使能后流程为：主表 uid 扫描 -> 行为分类 -> non-LSQ admission 或 LSQ 候选批量预览 -> 记录 `dispatch_flush_epoch` -> driver 在每个发送采样点先检查 flush/epoch，再保持 item 等 `canAccept` -> 若等待期间没有 flush/redirect epoch 变化，则采样 DUT `resp.lqIdx/sqIdx` -> `commit_allocate_with_resp()` 更新主表/状态/LSQ 模型 -> 生成 TLB 表 -> 路由 issue queue。
- 如果 LSQ 候选发送后遇到 flush/redirect，driver 置 `memblock_dispatch_aborted_by_redirect=1`，sequence 不确认 LSQ 分配；redirect/flush 处理函数负责清理被覆盖 uid 状态并回退公共 admission 边界。后续 `get_next_new_admit_uid()` 会重新返回最老需要 re-admission 的 uid。
- 如果当前 uid 处于 active、exception、replay、redirect 或 flushed 状态，Task14 不重新 admission；replay/redirect 仍由后续 issue queue/异常恢复 task 处理。

问题与解决：

- 问题：现有 `lsqenq_agent_agent_driver` 只单拍发送，不能 hold valid 等 ready。
  解决：不改默认行为，新增 xaction 标志 `memblock_dispatch_wait_can_accept`。只有 dispatch item 设置该标志时，driver 会保持当前 `needAlloc/req` item，直到采样到 `io_ooo_to_mem_enqLsq_canAccept=1` 的 fire 周期，再采样 resp 并 idle。
- 问题：`lsqenq` default sequence 与主控 vseq 并行启动，可能早于主表生成。
  解决：启用 Task14 后先调用 `wait_for_main_table()`，等待 `common_data_transaction.main_table_ready=1`。该位只在 `check_main_table_complete()` 完成后置 1，可覆盖最后一项 `set_main_transaction()`、地址复用修正和 `init_status_for_main_table()` 全部完成的事实；等待期间只按 `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` 周期性 warning，真正卡死由 testcase/UVM timeout 兜底。
- 问题：Scala 里真实 LSQ enqueue fire 受 redirect 抑制，不能只用 `canAccept` 作为软件确认条件。
  解决：新增 `memblock_sync_pkg::dispatch_flush_in_progress/dispatch_flush_epoch`。redirect 恢复 task 在 `request_redirect_flush()` 时置位并递增 epoch；LSQ xaction 发送前保存 epoch，driver 等待期间若看到 flush 进行中或 epoch 变化则 abort；sequence 确认前再次比较 epoch，若变化则跳过 `commit_allocate_with_resp()`，由 redirect/flush 状态处理统一回退公共 admission 边界。
- 问题：`lsqenq` resp 没有 valid bit，如何反查有效返回。
  解决：sequence 自己维护本拍 slot -> uid 候选列表，只读取本拍实际填 valid/needAlloc 的 slot；未使用 slot 的 resp 忽略。
- 问题：同拍多条 LSQ 候选如何避免重复预览同一个 LQ/SQ tail。
  解决：`collect_lsq_candidates()` 使用临时 `lq_tmp/sq_tmp/lq_free_tmp/sq_free_tmp` 做批量预览；真实 `lsq_ctrl_model` 指针只在 driver 返回后逐条 `commit_allocate_with_resp()` 时推进。
- 问题：AMO/LR/SC 是否驱动 LSQ enqueue。
  解决：不驱动。沿用 Task6 行为，`needAlloc=0` 只建立 ROB active 映射和 `enq=1`，随后生成 TLB 并进入 STA/STD issue queue。
- 问题：`lqCanAccept/sqCanAccept` 是否需要单独监测。
  解决：当前 `lsqenq_agent` 只暴露全局 `canAccept`，没有独立 `lqCanAccept/sqCanAccept` 输入。Task14 使用 DUT 全局 `canAccept` 加本地 `lsq_ctrl_model` free-count 作为最小合法 admission 条件。

验收计划：

- `git diff --check` 覆盖 Task14 文件集。
- `rg -n "memblock_lsqenq_dispatch_sequence|MEMBLOCK_LSQENQ|commit_allocate_with_resp|memblock_dispatch_wait_can_accept" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`
- subagent 独立 review Task14 文件，重点检查 handshake、同拍批量索引、状态更新点和默认行为。
- `cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_sanity mode=base_fun`
- `cd mem_ut/ver/ut/memblock/sim && make eda_run tc=tc_sanity mode=base_fun`

Subagent review：

- 初审 high：`wait_for_main_table()` 只等 `next_uid == main_trans_num` 可能早于主表最终修正和 status 初始化。处理：新增 `main_table_ready`，由 `check_main_table_complete()` 置位，Task14 改为等待该位。
- 初审 high：LSQ enqueue 确认只看 `canAccept`，未协同 redirect/flush。处理：新增 `dispatch_flush_epoch` 和 xaction abort 字段，driver/sequence 双侧过滤 redirect 竞争窗口。
- 复审 medium：dispatch item 首拍原本在 `main_phase` 中先 `send_pkt()`，下一拍才进入 flush/epoch 检查，仍可能让 DUT 看到一拍非法 admission。处理：dispatch 专用 item 不再走 `main_phase` 直接首拍发送，改为进入 `wait_lsq_can_accept()` 后先在 clocking block 采样点检查 flush/epoch，再决定是否驱动 item。
- 后续重构：已删除 `MEMBLOCK_LSQENQ_START_TIMEOUT`，等待主表期间只按
  `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` 周期性 warning；真正卡死由 testcase/UVM
  timeout 兜底。
- 最终复审：上述 high/medium 均已解决，限定范围内无剩余 blocker/medium。

验收结果：

- `git diff --check` 覆盖 Task14 文件集，通过。
- `make eda_compile tc=tc_sanity mode=base_fun` 通过，KDB 生成 0 error/0 warning。
- `make eda_run tc=tc_sanity mode=base_fun` 通过，日志显示 `TEST CASE PASSED`，`UVM_ERROR=0`，`UVM_FATAL=0`。

### Task15：L2TLB/PTW responder sequence

目标：实现默认关闭的 L2TLB/PTW response sequence，使其在后续接入真实 L2TLB/DTLB 请求后可以消费 `common_data_transaction.sv` 中的 TLB 表回填 response transaction。本任务不修复当前占位 `L2tlb_agent_connect.sv`，也不把 L2TLB agent 强行接入 `memblock_connect.sv`。

实现文件：

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`
- `mem_ut/ver/ut/memblock/cfg/tb.f`
- `mem_ut/ver/ut/memblock/env/memblock_env_pkg.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_base.sv`

实现 class/API：

  - `seq_csr_common`
  - 参数：
    - `l2tlb_seq_en`：是否启用 L2TLB responder，默认 1。
    - `l2tlb_min_latency/l2tlb_max_latency`：request fire 到 response valid 的随机延迟范围。
    - `l2tlb_idle_stop_cycle`：responder 连续空闲停止阈值，默认 5000。L2TLB responder 不再使用固定 `max_cycles` 退出。
  - getter：`get_l2tlb_seq_en()`、`get_l2tlb_min_latency()`、`get_l2tlb_max_latency()`、`get_l2tlb_idle_stop_cycle()`。
  - 校验：`max_latency < min_latency` 时把 max 修正为 min；使能时 `idle_stop_cycle=0` 修正为 1。

- `common_data_transaction`
  - `lookup_tlb_uid_by_req(vpn, s2xlate, uid)`：L2TLB responder 的 request 查表入口。先用 `mmu_csr_state.make_lookup_key({26'b0, vpn}, s2xlate)` 和当前 `csr_update_seq` 查询 `uid_by_tlb_key[]`；未命中时扫描 `tlb_table_by_uid[]` 中同 `vpn[37:0]`、同 `s2xlate`、同 CSR 代际的表项作为兜底。

- `memblock_l2tlb_base_sequence extends L2tlb_agent_agent_default_sequence`
  - 字段：
    - `data`：公共数据单例。
    - `l2tlb_vif`：L2TLB agent virtual interface，用于采样 request valid/vpn/s2xlate。
    - `enable/min_latency/max_latency/idle_stop_cycle`：从 `seq_csr_common` 获取的运行参数。
    - `send_count`：只在 responder loop 内部维护的“成功服务请求计数器”，用于日志和 xaction 命名，不参与协议或状态判断。
  - task/function：
    - `body()`：初始化 `seq_csr_common`，默认关闭时直接返回，不发送 L2TLB item；使能时获取公共数据和 vif 后进入响应循环。
    - `drive_l2tlb_loop()`：不再按固定 `max_cycles` 退出；每拍调用 `send_l2tlb_cycle(send_count, has_progress)`，有 progress 时 `send_count++` 并清零 `idle_count`，只有连续 idle 超过 `idle_stop_cycle` 后退出。
    - `send_l2tlb_cycle(send_count, has_progress)`：采样 request valid；若有请求则发送 ready item、按 `latency` 给 response item 设置 `pre_pkt_gap`、查/建 TLB 表并发送 response item。
    - `send_l2tlb_item(tr)`：封装 `start_item()/finish_item()`，所有 ready/response item 统一通过该入口发给 driver。
    - `request_fire()`：判断 reset 释放、`reset_backend_done=1` 且 `req_valid=1`。由于本 sequence 默认持续驱动 `req_ready=1`，不再读取 vif 上前一拍 ready 值作为 fire 条件。
    - `create_l2tlb_xaction(name)`：创建并清零 L2TLB xaction，默认填 `req_ready=1`、`resp_valid=0`。
    - `sample_request_fields(vpn, s2xlate)`：从 vif 采样 request 字段；采样结果写入 response item 的 debug 字段。
    - `lookup_tlb_entry(vpn, s2xlate, tlb_tr)`：调用 `common_data_transaction::lookup_tlb_uid_by_req()`，返回 per-uid `tlb_transaction`。
    - `fill_response_from_tlb(tr, vpn, s2xlate, tlb_tr)`：把 `tlb_transaction` 中的 PTE 权限、PPN、PBMT、N 位、fault 位、index 派生字段回填到 `L2tlb_agent_agent_xaction`。
    - `choose_latency()`：在 `min_latency..max_latency` 内选择延迟。

调度关系：

- Task7/Task14 生成和注册 per-uid TLB 表；Task15 只消费 TLB 表，不自行随机生成响应关键字段。
- 后续真实接入流程预期为：DTLB/L2TLB request fire -> `memblock_l2tlb_base_sequence` 采样 `vpn/s2xlate` -> 查询 `common_data_transaction` TLB 表 -> 构造 response -> driver 发给 DUT。
- Task15 完成时，`L2tlb_agent_connect.sv` 仍是占位误连，`memblock_connect.sv` 没有实例化 L2TLB interface，`memblock_env` 也没有实例化 `u_L2tlb_agent_agent`。因此 Task15 只把 `L2tlb_agent_agent` 加入 `tb.f` 编译、在 `tc_pkg.sv` include sequence，并在 `tc_base.sv` 预置 `env.u_L2tlb_agent_agent.sqr.main_phase` 的 default sequence。Task18 已把 env/connect 补齐，并将 connect 改为真实 DTLB/L2TLB 交互点。

问题与解决：

- 问题：需求中提到“根据采集到的 paddr 请求查询索引”，但现有 `L2tlb_agent_agent_interface` 只有 `vpn/s2xlate`，没有 paddr。
  解决：本任务采用最小合法实现，先按 runtime CSR 生成 `{vpn, asid, vmid, s2xlate}` 查 `uid_by_tlb_key[]`；再扫描 per-uid TLB 表作为兜底。paddr context 留给后续 DTLB request monitor/agent 接入任务。
- 问题：是否应修复并启用 `L2tlb_agent_connect.sv`。
  解决：不在 Task15 中处理。Task15 阶段该 connect 明显是占位/误连，方案也要求不能无确认启用。真实端口接入已在 Task18 中完成，连接点为 `inner_dtlbRepeater.io_ptw_req_0` 到 `inner_ptw.io_tlb_1`。
- 问题：miss 时是否随机返回合法 TLB entry，或保留 fallback fault response。
  解决：不随机，也不再补 fallback response。当前实现查不到 TLB entry 时直接 `uvm_fatal`，这样可以把问题明确压回到前面的建表链路、runtime CSR snapshot 或 lookup key 生成逻辑上。
- 问题：subagent review 指出未设置 L2TLB default sequence 时，后续真实 env 接入可能仍跑 agent 原始随机 default sequence。
  解决：在 `tc_base.sv` 预置 `u_L2tlb_agent_agent` 的 default sequence 为 `memblock_l2tlb_base_sequence`。由于当前 env 尚未实例化该 agent，此配置不会改变 `tc_sanity/base_fun` 行为；后续接入真实 agent/vif 后，默认关闭路径会 no-op。
- 问题：subagent review 指出原实现用同一个 transaction 的 `pre_pkt_gap` 表达 response latency，会在延迟期间同时延迟 `req_ready`，不符合“请求握手后延迟响应”。
  解决：拆分为 ready/idle item、wait item 和 response item。每个周期都通过 driver 发送独立 item，延迟期间持续 `req_ready=1`、`resp_valid=0`。
- 问题：subagent review 指出 `request_fire()` 读取 vif 上旧的 ready 值，可能依赖上一拍状态。
  解决：本 sequence 的 ready 策略固定为持续 ready，先发一拍 ready/idle item 后再采样 request；`request_fire()` 只检查 reset/backend done/request valid。

验收计划：

- `git diff --check` 覆盖 Task15 文件集。
- `rg -n "memblock_l2tlb_base_sequence|MEMBLOCK_L2TLB|lookup_tlb_uid_by_req" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`
- subagent 独立 review Task15 文件，重点检查默认关闭行为、lookup/miss 策略、L2TLB connect 边界和 package 编译顺序。
- `cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_sanity mode=base_fun`
- `cd mem_ut/ver/ut/memblock/sim && make eda_run tc=tc_sanity mode=base_fun`

Subagent review：

- 第一轮 review 指出三项 blocker：
  - L2TLB default sequence 未预置，后续 env 接入后可能仍跑原始随机 default sequence。
  - `request_fire()` 依赖 vif 上旧 ready 值。
  - response latency 用同一个 transaction 的 `pre_pkt_gap` 表达，会同时延迟 `req_ready`。
- 处理：
  - 在 `tc_base.sv` 预置 `env.u_L2tlb_agent_agent.sqr.main_phase` default sequence 为 `memblock_l2tlb_base_sequence`。
  - `request_fire()` 改为只检查 reset、backend done 和 request valid；ready 由 sequence 持续发 ready/idle item 保证。
  - response latency 拆成 ready/idle item、wait item 和 response item，不再使用 `pre_pkt_gap` 延迟 ready。
- 复审结论：无必须修复项。当前满足“可编译 responder sequence + 默认不影响 tc_sanity + 暂不接真实 DUT”的 Task15 边界。后续真实接入时还需补 `memblock_env.sv` 中 L2TLB agent 实例、cfg、fifo/RM 连接，并在确认真实 DUT 端口路径后重写或启用 `L2tlb_agent_connect.sv`。

验收结果：

- `git diff --check` 覆盖 Task15 文件集，通过。
- `git diff --no-index --check /dev/null mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv` 无 whitespace 输出。
- `rg -n "memblock_l2tlb_base_sequence|MEMBLOCK_L2TLB|lookup_tlb_uid_by_req" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md AI_DOC/plan/test_framework/plan/do/l2tlb_base_seq_plan_20260614.md` 已确认新增入口。
- `make eda_compile tc=tc_sanity mode=base_fun` 通过，KDB 生成 0 error/0 warning。
- `make eda_run tc=tc_sanity mode=base_fun` 通过，日志显示 `TEST CASE PASSED`，`UVM_ERROR=0`，`UVM_FATAL=0`。

### Task16：真实 DUT 事实 monitor adapter 接入一期

目标：在不引入 package cycle 的前提下，把真实 monitor 中低风险事实接入现有 dispatch
状态机。agent/env 侧只依赖 `memblock_sync_pkg` 的 raw struct 和 queue，不引用
`tc_pkg` 中的 `common_data_transaction` 或 `memblock_wb_event_t`；`tc_pkg` 内新增 adapter
负责 raw fact 到标准事件/LSQ helper 的转换。默认 capture 关闭，只有
`common_data_transaction::reset_all_tables()` 后打开，`end_test_check()` 关闭并清残留队列。

实现文件：

- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv`
- `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv`
- `mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_monitor.sv`
- `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_monitor.sv`

新增 raw 类型和 helper：

- `dispatch_monitor_capture_en`：raw monitor capture 总开关，默认 0。dispatch 表 reset 后置 1，
  testcase 结束检查时置 0。
- `dispatch_raw_int_wb_t/raw_int_wb_q`：记录 int/load writeback raw fact。字段包括
  `valid/port_id/rob/lq/sq/exception_vec/cycle`。
- `dispatch_raw_iq_feedback_t/raw_iq_feedback_q`：记录 scalar STA/STD IQ feedback raw fact。
  字段包括 `valid/port_id/is_sta/is_std/rob/lq/sq/hit/flush_state/source_type/vector_feedback/cycle`。
- `dispatch_raw_ctrl_t/raw_ctrl_q`：记录 ctrl deq 和 memoryViolation raw fact。字段包括
  `lq_deq/sq_deq/lq_deq_ptr/sq_deq_ptr/memory_violation_rob/target/level/cycle`。
- helper：`make_empty_raw_*()`、`push_raw_*()`、`pop_raw_*()`、`clear_raw_monitor_queues()`、
  `raw_monitor_queue_size()`。所有 helper 只使用基本类型和 queue，不引用 agent class 或
  `tc_pkg` 类型。

新增 class：`dispatch_monitor_event_adapter`

- 归属：`seq/base_seq`，由 `tc_pkg.sv` 在 `lsq_commit_handler.sv` 后、
  `memblock_dispatch_base_sequence.sv` 前 include。
- 字段：
  - `common_data_transaction data`：用于 active ROB/LQ/SQ uid 解析和标准事件初始化。
  - `lsq_commit_handler monitor_commit_handler`：用于 ctrl deq raw fact 转 LSQ 释放 helper。
- 转换函数：
  - `convert_raw_int_wb()`：将 raw int writeback 转为 `MEMBLOCK_ISSUE_TARGET_LOAD`、
    `MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB`、`real_wb_valid=1` 的标准事件，携带
    `rob/lq/exception_vec/cycle`。无法通过 active 映射解析 uid 时低级 log 后丢弃。
  - `convert_raw_iq_feedback()`：一期只接 `staIqFeedback[0/1]` 标量反馈。当前实现按
    XiangShan IssueQueue 语义用 `hit` 判定 pass/replay：`hit=1` 转 STA pass，
    `hit=0` 转 STA replay/fail 事实；`flush_state` 来自 TLB/PTW-back 状态，不单独触发
    replay。其它无法确认的标量 feedback 丢弃并低级 log。vector feedback 不接标准事件，
    避免触发 Task11 的 vector fatal。
  - `convert_raw_memory_violation()`：将 ctrl memoryViolation 转为
    `MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION` redirect 事件，必须带可解析的 ROB key。
  - `apply_raw_ctrl_deq()`：从 raw ctrl 的 `lq_deq/sq_deq/*DeqPtr` 构造基本 key，调用
    `lsq_commit_handler::apply_raw_ctrl_deq()`，不构造 agent xaction。
- drain task：
  - `drain_writeback_events()`：drain raw int wb 和 raw scalar IQ feedback，并调用
    `writeback_status_handler::handle_event()`。
  - `drain_exception_and_redirect_events()`：drain raw ctrl；ctrl 先应用
    deq helper，再把 memoryViolation 转标准事件。

base sequence 调度关系：

- `memblock_dispatch_base_sequence` 新增 `monitor_adapter` 和 `monitor_commit_handler` 句柄。
- `collect_writeback_events()` 调用 adapter drain raw writeback/feedback，adapter 直接
  调用 `writeback_status_handler::handle_event()` 分类；普通 pass 立即落表，异常/replay/redirect 入队。
- `collect_exception_and_redirect_events()` 调用 adapter drain raw ctrl，再由现有
  `exception_redirect_replay_task()` 处理 redirect/replay/fault queue。
- `lsq_commit_handler` 新增 `apply_raw_ctrl_deq(lq_count, lq_ptr, sq_count, sq_ptr)`，复用
  `apply_dut_lq_deq()` 和 `apply_dut_sq_deq()` 的整批预校验。

monitor 最小接入：

- `io_mem_to_ooo_int_wb_agent_agent_monitor`：只采集 writeback port 0/1/2 中
  `valid && toRob.valid` 的 raw int wb，记录 ROB、LQ 和 24-bit exceptionVec。不启用
  `mon_item_port.write()`。
- `io_mem_to_ooo_ctrl_agent_agent_monitor`：当 `lqDeq/sqDeq` 非 0 或
  `memoryViolation.valid=1` 时 push raw ctrl，记录 deq count/pointer 和
  memoryViolation ROB/target/level。
- `redirect_agent_agent_monitor`：`io_redirect_*` 是 TB/sequence 驱动 DUT 的 input 接口；
  monitor 只保留 X/Z 检查和预留采样，不 push raw redirect，不反馈 dispatch recovery。
- `io_mem_to_ooo_iq_feedback_agent_agent_monitor`：只采集 `staIqFeedback[0/1]` 标量反馈；
  `vstu/vldu` vector feedback 延期，不 push 标准事件。

问题与解决：

- 问题：agent package 不能依赖 `tc_pkg`，否则会和 sequence/common data 形成 cycle。
  解决：agent monitor 只调用 `memblock_sync_pkg::push_raw_*()`，raw struct 位于独立编译的
  common package 前置文件，不含 class handle。
- 问题：真实 monitor 事实可能早于软件 active 映射或晚于 retire。
  解决：adapter 必须先用现有 active ROB/LQ/SQ 映射解析 uid，查不到时低级 log 丢弃，
  不 fatal。
- 问题：IQ feedback 的 `sourceType/hit/flushState` 语义需要和 XiangShan 源码对齐。
  解决：当前以 `Region.scala` 的 `finalSuccess := feedBack.valid && feedBack.bits.hit`
  为准，`hit=1` 作为 pass，`hit=0` 作为 replay/fail；`flushState` 对应 TLB/PTW-back
  状态元信息，不单独触发 replay。其余标量 feedback 丢弃，vector feedback 延期。
- 问题：ctrl deq helper 原入口依赖 agent xaction。
  解决：新增 raw deq API，直接传 count 和 pointer key，复用 Task12 已有整批预校验。

延期项：

- `csr_ctrl_agent` monitor 尚未接入，CSR/exception 类事实后续单独确认语义后再接。
- `vldu/vstu` vector IQ feedback 暂不转换，避免 Task11 vector LS fatal。
- IQ feedback sourceType 到 STA/STD/replay 类型的完整映射仍需对照 RTL/Scala 后扩展。
- STA pass 当前可由 IQ feedback hit 兼容闭环，也可在严格模式下等待真实 writeback
  fact；STD pass 仍按对应 strict/synthetic 配置处理。
- redirect `flush_itself` 当前由 `level(0)` 派生；`level` 已从 memoryViolation payload
  透传到 `io.redirect` 回灌。

Subagent review：

- 第一轮实现后主 agent 发现 `memblock_dispatch_base_sequence` 新增 `commit_handler` 与
  `soft_test_memblock_dispatch_smoke_sequence` 已有字段重名风险。处理：base sequence 字段重命名为
  `monitor_commit_handler`，只作为 monitor adapter 的 LSQ deq helper 句柄，派生类原有
  `commit_handler` 保持不变。
- IQ feedback 文档边界补充：当前口径已按 XiangShan IssueQueue 语义修正为
  `hit=1` 作为 STA pass、`hit=0` 作为 STA replay/fail；`flush_state` 仅记录
  TLB/PTW-back 状态，不单独触发 replay。
- 主 agent 复审结论：无 blocker。当前实现满足“一期 raw monitor fact 桥接 + 默认不影响
  tc_sanity”的边界。

验收结果：

- `git diff --check` 通过。
- `rg` 已确认 `dispatch_monitor_event_adapter`、`dispatch_monitor_capture_en`、
  `raw_*_q`、`monitor_commit_handler` 和 Task16 文档均命中。
- `make eda_compile tc=tc_sanity mode=base_fun` 通过，最终日志显示
  `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。
- `make eda_run tc=tc_sanity mode=base_fun` 通过，日志显示 `TEST CASE PASSED`，
  `UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。

### Task17：CSR runtime monitor adapter 接入一期

目标：把真实 `csr_ctrl_agent` monitor 中影响 TLB key 和 MMU runtime state 的字段接入
dispatch 公共数据。agent 侧仍只依赖 `memblock_sync_pkg` 的 raw struct 和 queue，不引用
`tc_pkg`、`common_data_transaction` 或任何 sequence class。默认 capture 关闭，只有
`reset_all_tables()` 后打开；CSR 稳定不变时不重复入队，避免 raw queue 每拍增长。

实现文件：

- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/mmu_csr_runtime_state.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`

新增 raw 类型和 helper：

- `dispatch_raw_csr_t/latest_raw_csr/latest_raw_csr_seq`：记录最新 MMU CSR runtime 快照。字段包括
  `valid`、`satp_mode/asid/ppn/changed`、`vsatp_mode/asid/ppn/changed`、
  `hgatp_mode/vmid/ppn/changed`、`priv_mxr/sum/vmxr/vsum/virt/virt_changed/spvp`、
  `priv_imode/dmode`、`m_pbmt_en/h_pbmt_en` 和 `cycle`。字段宽度直接对齐
  `csr_ctrl_agent_agent_xaction` 和 `mmu_csr_runtime_state`。
- `make_empty_raw_csr()`：返回清零 raw CSR item。
- `raw_csr_payload_changed(prev, cur)`：判断是否需要 push 新 raw。稳定字段变化时返回 1；
  `satp/vsatp/hgatp/priv_virt_changed` 只在新高电平脉冲时返回 1，避免 changed 信号保持高
  多拍导致重复入队。
- `push_raw_csr()`：受 `dispatch_monitor_capture_en` 控制，覆盖 latest CSR snapshot 并递增
  `latest_raw_csr_seq`，不再进入 FIFO。
- `get_latest_raw_csr()`：返回当前 latest CSR snapshot 和 seq，供 adapter 同步到公共 runtime CSR。
- `clear_raw_monitor_queues()` 会清空 latest CSR valid/seq；`raw_monitor_queue_size()` 不再统计
  CSR，因为 CSR runtime 是状态快照，不是待排空事件。

新增和扩展 class/API：

- `csr_ctrl_agent_agent_monitor`
  - 新增局部字段：
    - `raw_csr`：当前拍 CSR raw 快照。
    - `last_raw_csr`：上一条已 push 的 CSR raw 快照，用于去重。
    - `has_last_raw_csr`：本轮是否已有可比较快照。
    - `last_capture_en`：上一拍 capture 开关，用于检测新一轮 capture 开始。
  - 采样行为：
    - 每拍沿用原有 monitor 对 `tlbCsr_*` 的采样和 X/Z 检查。
    - 仅在 `rst_n=1`、`reset_backend_done=1` 且 `dispatch_monitor_capture_en=1` 时构造
      `dispatch_raw_csr_t`。
    - capture 从 0 到 1 或 reset/backend 未完成时清 `has_last_raw_csr`，保证每轮
      `reset_all_tables()` 后首次 CSR 快照一定入队。
    - 首次采样、稳定字段变化或 changed 位新脉冲时调用 `push_raw_csr()`；每拍都会更新
      monitor 本地快照，因此 changed 位下降沿不会刷新 latest snapshot，但后续再次拉高会被识别为新脉冲。
      原有大块 `mon_item_port.write()` 注释路径保持不启用。

- `mmu_csr_runtime_state`
  - 新增函数：
    - `update_from_raw_csr(raw)`：从 raw CSR 快照更新 `satp/vsatp/hgatp/priv/PBMT` runtime
      字段。只有字段变化或 changed 位为 1 时递增 `update_seq`。
  - 原 `update_from_csr_ctrl(csr_tr)` 保留，用于后续若恢复完整 xaction/RM 路径时复用。

- `common_data_transaction`
  - 新增函数：
    - `apply_raw_csr_runtime(raw, raw_csr_seq)`：公共数据侧的 CSR 更新入口。先用
      `last_applied_raw_csr_seq` 判断同一个 latest CSR snapshot 是否已经应用过，避免多个
      adapter 或慢速 drain 方重复应用同一个 changed pulse；确认是新 seq 后确保
      `mmu_csr_state` 存在，并调用 `update_from_raw_csr(raw)`。该函数不因 `update_seq`
      变化清 TLB entry 或 uid record。

- `dispatch_monitor_event_adapter`
  - 新增函数：
    - `drain_csr_events()`：读取 `latest_raw_csr` 和 `latest_raw_csr_seq`，并调用
      `common_data_transaction::apply_raw_csr_runtime(raw, raw_csr_seq)`；重复 seq 过滤放在
      `common_data_transaction` 中统一处理。
  - 调度扩展：
    - `drain_writeback_events()` 和 `drain_exception_and_redirect_events()` 开头先调用
      `drain_csr_events()`，保证 monitor fact drain 时顺带同步 runtime CSR。

- `memblock_dispatch_base_sequence`
  - 新增函数：
    - `collect_csr_runtime_events()`：创建/复用 `dispatch_monitor_event_adapter`，调用
      `drain_csr_events()`。
  - 调度扩展：
    - `build_tlb_table_for_active_uid(uid, s2xlate)` 在构建 TLB 表前先调用
      `collect_csr_runtime_events()`，保证 TLB key 使用生成前最新 CSR runtime state。

- `memblock_lsqenq_dispatch_sequence`
  - 新增字段：
    - `dispatch_monitor_event_adapter monitor_adapter`：真实 LSQ admission sequence 私有
      adapter 句柄。
  - 新增函数：
    - `drain_csr_runtime_events()`：创建/复用 adapter 并调用 `drain_csr_events()`。
  - 调度扩展：
    - `complete_admission(uid)` 在 `tlb_builder.build_tlb_for_uid(uid, 2'd0)` 前先 drain CSR，
      覆盖真实 LSQ admission 直接调用 TLB builder、未经过 base wrapper 的路径。

- `memblock_l2tlb_base_sequence`
  - 新增字段：
    - `dispatch_monitor_event_adapter monitor_adapter`：L2TLB responder 私有 adapter 句柄。
  - 新增函数：
    - `drain_csr_runtime_events()`：创建/复用 adapter 并调用 `drain_csr_events()`。
  - 调度扩展：
    - `lookup_tlb_entry(vpn, s2xlate, tlb_tr)` 在调用 `lookup_tlb_uid_by_req()` 前先 drain CSR，
      覆盖 CSR 变化发生在 TLB 表生成之后、PTW request 之前的路径。

问题与解决：

- 问题：agent package 不能依赖 `tc_pkg`，CSR monitor 如何更新 `mmu_csr_runtime_state`。
  解决：monitor 只 push `memblock_sync_pkg::dispatch_raw_csr_t`，由 `tc_pkg` 内
  `dispatch_monitor_event_adapter` drain raw queue 后更新公共数据，避免 package cycle。
- 问题：CSR monitor 每拍采样，如果每拍 push raw 会让 raw queue 增长并污染运行效率。
  解决：monitor 保存上一条已 push 快照，只在首次采样、稳定字段变化或 changed 位新脉冲
  时 push。
- 问题：同一 testcase 多轮或多次 `reset_all_tables()` 后，monitor 本地去重状态可能导致
  新一轮首个 CSR 快照不入队。
  解决：monitor 记录 `last_capture_en`，capture 开关变化或 reset/backend 未完成时清
  `has_last_raw_csr`。
- 问题：CSR 是运行时状态，不是事件流；若用 FIFO 或每个 adapter 自己去重，慢速 drain 方可能读到旧 CSR，多个 adapter 也可能重复应用同一 changed pulse。
  解决：`push_raw_csr()` 只保留 latest snapshot 和递增 seq，`apply_raw_csr_runtime(raw, raw_csr_seq)`
  在公共数据侧用 `last_applied_raw_csr_seq` 统一去重。TLB entry/uid record 不因 CSR `update_seq`
  自动清除，真实失效后续由 `sfence/hfence` entry 级逻辑处理。
- 问题：真实 LSQ admission sequence 直接调用 `tlb_builder.build_tlb_for_uid()`，不会经过
  base sequence 的 `build_tlb_table_for_active_uid()`。
  解决：在 `memblock_lsqenq_dispatch_sequence::complete_admission()` 中也先 drain CSR raw。
- 问题：L2TLB responder lookup 可能发生在 TLB 表生成之后，如果这期间有 CSR raw event 未
  drain，`lookup_tlb_uid_by_req()` 会使用旧 `mmu_csr_state`。
  解决：在 `memblock_l2tlb_base_sequence::lookup_tlb_entry()` 开头 drain CSR raw。

验收计划：

- `git diff --check` 覆盖 Task17 文件集。
- `rg -n "dispatch_raw_csr_t|drain_csr_events|apply_raw_csr_runtime|update_from_raw_csr|collect_csr_runtime_events" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`
- subagent 独立 review Task17 文件，重点检查 package cycle、CSR 去重、TLB lookup index
  清理和 TLB build 前 drain 时机。
- `cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_sanity mode=base_fun`
- `cd mem_ut/ver/ut/memblock/sim && make eda_run tc=tc_sanity mode=base_fun`

Subagent review：

- 初始方案 review 建议已采纳：raw struct 只放最小 MMU CSR 字段；monitor 只在首次、字段
  变化或 changed 位新脉冲时 push；adapter 通过 common data wrapper 更新 CSR runtime；
  CSR raw 采用 latest snapshot + seq，不进入 FIFO；`common_data_transaction` 通过
  `last_applied_raw_csr_seq` 统一过滤重复 snapshot；TLB build 前必须 drain CSR raw。
- 实现后复审指出：
  - high：changed 位高脉冲后下降沿未更新本地快照，会漏掉后续再次拉高的脉冲。处理：
    monitor 在 capture 打开且 reset/backend 完成时每拍更新 `last_raw_csr`，只在首次、稳定
    字段变化或 changed 位新高脉冲时 push。
  - medium：L2TLB responder lookup 前未 drain CSR raw。处理：
    `lookup_tlb_entry()` 开头调用 `drain_csr_runtime_events()`。
  - low：文档需和 changed 位去重实现保持一致。处理：本节已同步。

验收结果：

- `git diff --check` 覆盖 Task17 文件集，通过。
- `rg -n "dispatch_raw_csr_t|drain_csr_events|apply_raw_csr_runtime|update_from_raw_csr|collect_csr_runtime_events" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 通过，确认 raw CSR 类型、adapter drain、公共数据更新和 TLB build 前同步入口均已接入。
- subagent 复审通过；复审提出的 changed 位重复脉冲、L2TLB lookup 前 CSR drain 和文档一致性问题已全部修正。
- `cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_sanity mode=base_fun` 通过，日志显示 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。
- `cd mem_ut/ver/ut/memblock/sim && make eda_run tc=tc_sanity mode=base_fun` 通过，日志显示 `TEST CASE PASSED`，`UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。

### Task18：L2TLB agent env 集成与 DTLB/L2TLB 实连接

目标：收敛 Task15 留下的 L2TLB responder 接入缺口。当前 `L2tlb_agent_agent` 已加入
`tb.f` 并已有 `memblock_l2tlb_base_sequence`，但 `memblock_env` 未实例化该 agent，
`tc_base.sv` 中设置的 default sequence 实际没有落点；旧版 `L2tlb_agent_connect.sv`
曾把字段误接到 `io_dcacheError/io_memInfo` 等无关端口，不能使用。

Task18 当时实现为：把 L2TLB agent 接入 env/cfg/RM/common xaction，并把
`L2tlb_agent_connect.sv` 连接到 DUT 内部 DTLB 与 L2TLB 的交互点。该连接点是
`PTWRepeaterNB inner_dtlbRepeater` 和 `L2TLBWrapper inner_ptw.io_tlb_1` 之间的
request/response 通道。当前最新实现已把 connect 接管改为编译期宏
`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 控制，默认值为 1；`MEMBLOCK_L2TLB_SEQ_EN`
只控制 responder sequence 是否主动回包。需要只观察 DUT 原始 response 时，应编译期覆盖
`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0`。

实现文件：

- `mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv`
- `mem_ut/ver/ut/memblock/tb/memblock_connect.sv`
- `mem_ut/ver/ut/memblock/env/src/memblock_env_cfg.sv`
- `mem_ut/ver/ut/memblock/env/src/memblock_env.sv`
- `mem_ut/ver/ut/memblock/env/src/memblock_rm.sv`
- `mem_ut/ver/ut/memblock/common/memblock_common/memblock_common_pkg.sv`
- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_common_xaction.sv`
- `mem_ut/ver/ut/memblock/cfg/user_cfg.sv`
- `mem_ut/ver/ut/memblock/cfg/user_cfg.local.default.sv`
- `mem_ut/ver/ut/memblock/rule/memblock_user_ctrl_usage.md`
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`

计划实现：

- `memblock_env_cfg`
  - 新增字段：`rand L2tlb_agent_agent_cfg u_L2tlb_agent_agent_cfg`。
  - `new()`：创建该 cfg。
  - `post_randomize()`：当前默认设置 `sqr_sw=ON`、`drv_sw=ON`、`mon_sw=ON`、
    `xz_sw=OFF`、`channel_id=19`。agent 运行期组件开关由 `memblock_env_cfg` 和
    `user_cfg.local.sv` 管理，不再由 `MEMBLOCK_L2TLB_SEQ_EN` 间接修改。
    `MEMBLOCK_L2TLB_SEQ_EN` 只控制 `memblock_l2tlb_base_sequence` 是否主动回包。
  - `apply_user_cfg()`：支持 `all_agent_ctrl` 和 `u_L2tlb_agent_agent_ctrl` 覆盖，便于后续
    directed 调试时手动打开。

- `memblock_env`
  - 新增字段：`L2tlb_agent_agent u_L2tlb_agent_agent` 和
    `uvm_tlm_analysis_fifo #(L2tlb_agent_agent_xaction) L2tlb_agent_mon2rm_fifo`。
  - `build_phase()`：创建 fifo 和 agent，并通过 config DB 设置
    `u_L2tlb_agent_agent_cfg`。
  - `connect_phase()`：RM blocking port 始终连接到 L2TLB fifo；只有
    `u_L2tlb_agent_agent_cfg.mon_sw==ON` 且 agent monitor analysis port 存在时，才把
    agent monitor analysis port 接入该 fifo。默认关闭时 fifo 没有生产者，RM 分支阻塞等待，
    不影响仿真结束。

- `memblock_rm`
  - 新增字段：`uvm_blocking_get_port #(L2tlb_agent_agent_xaction)
    L2tlb_agent_mon_item_port`。
  - `build_phase()`：创建该 port。
  - `main_process()`：新增 L2TLB monitor 分支，只在端口接通并收到 transaction 时 pack 到
    common xaction。默认 `mon_sw=OFF` 时该分支阻塞等待，不影响仿真结束。

- `memblock_common_xaction`
  - 新增字段：`L2tlb_agent_agent_xaction L2tlb_agent_tr`。
  - 新增函数：`pack_L2tlb_agent(tr)`，把 L2TLB monitor transaction 保存到 common xaction。
  - `psdisplay()` 和 `compare()` 增加 `channel_id==19` 分支。

- `memblock_user_cfg`
  - 新增字段：`memblock_user_agent_ctrl u_L2tlb_agent_agent_ctrl`。
  - `new()`、`uvm_object_utils` 和 `user_cfg.local.default.sv` 同步添加默认模板，所有 valid 位
    仍默认为 0。

- `L2tlb_agent_connect.sv`
  - 保留 macro 和 vif config DB 设置。
  - request 方向连接为 DTLB -> L2TLB agent，直接采样 DUT 内部：
    `RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_valid`、
    `RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_bits_vpn`、
    `RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_bits_s2xlate`。
  - response 方向是否由 L2TLB agent -> DTLB 接管，由编译期宏
    `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 控制，默认值为 1。
  - takeover 为 1 时，force DUT 内部 `RTL_PATH._inner_ptw_io_tlb_1_req_0_ready` 和
    `RTL_PATH._inner_ptw_io_tlb_1_resp_*` 到 interface driver 输出。
  - takeover 为 0 时，只把 `RTL_PATH._inner_ptw_io_tlb_1_*` 采样回 interface，便于后续
    monitor/debug，不覆盖 DUT 原始 response 通路。
  - 初始化 interface driver 输出为 0，避免 active 模式启动前出现 X；`MEMBLOCK_L2TLB_SEQ_EN=0`
    时 driver 保持 idle，不主动响应 request。

- `memblock_connect.sv`
  - include `L2tlb_agent_connect.sv`，并在 `MEMBLOCK_CONNECT` 中展开
    `MEMBLOCK__L2TLB_AGENT_CONNECT(u_memblock__L2tlb_agent_if,
    ENV_PATH.u_L2tlb_agent_agent, RTL_PATH)`。
  - 默认 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` 时由 agent 接管 L2TLB 侧响应端口；
    需要纯观察 DUT 原始 response 时，编译期覆盖该宏为 0。

问题与解决：

- 问题：能否直接把 `L2tlb_agent_connect.sv` 接到当前 DUT。
  解决：旧连接不能直接用，因为它把 L2TLB 字段误接到 `io_dcacheError`、`io_memInfo`
  等无关端口。根据生成后的 `build_memblock/rtl/MemBlock.sv`，真实 DTLB/L2TLB 交互点为
  `inner_dtlbRepeater.io_ptw_req_0` 到 `inner_ptw.io_tlb_1`。Task18 已改为连接该内部
  层级，模拟的是 L2TLB 对 DTLB 的 ready/response 端口。
- 问题：如果只在 env 创建 L2TLB agent 但没有 vif，是否可行。
  解决：不可行。`tcnt_agent_base::build_phase()` 无条件从 config DB 获取 vif，缺失时 fatal。
  因此 Task18 必须在 `memblock_connect.sv` 中展开 L2TLB connect 宏。
- 问题：默认是否打开 L2TLB agent monitor。
  解决：默认关闭。当前 `memblock_l2tlb_base_sequence` 自己通过 vif 采样 request 并发
  response，不依赖 monitor。monitor/RM/common xaction 路径已接入，后续需要 scoreboard
  或覆盖率时再通过 user_cfg 定向打开。
- 问题：`MEMBLOCK_L2TLB_SEQ_EN` 默认改为 1 后，如何避免非 dispatch testcase 被
  L2TLB responder 等待主表影响。
  最新状态：`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 默认 1，connect 默认接管
  DTLB/L2TLB response 通路；`MEMBLOCK_L2TLB_SEQ_EN` 默认 1 时 responder sequence
  主动回包。非 dispatch testcase 若不建立 `main_table_ready`，需要通过 cfg 显式设置
  `MEMBLOCK_L2TLB_SEQ_EN=0`，否则 sequence 会等待主表直到 timeout。若需要纯观察 DUT
  原始 ready/response，应编译期覆盖 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0`。

验收计划：

- `git diff --check` 覆盖 Task18 文件集。
- `rg -n "u_L2tlb_agent_agent|pack_L2tlb_agent|MEMBLOCK__L2TLB_AGENT_CONNECT|channel_id = 19" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`
- review Task18 文件，重点检查 request/response 方向、默认关闭行为、
  env/RM/common xaction 集成完整性和 `tc_sanity/base_fun` 默认风险。
- `cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_sanity mode=base_fun`
- `cd mem_ut/ver/ut/memblock/sim && make eda_run tc=tc_sanity mode=base_fun`

Review：

- review 结论：当前连接方向符合 L2TLB agent 规则，request 从 DTLB Repeater 采样，
  ready/response 由 connect-time takeover 决定是否交给 agent 驱动。最新规则下
  `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 默认 1，`MEMBLOCK_L2TLB_SEQ_EN` 默认 1，
  responder sequence 默认主动回包；显式设置 `MEMBLOCK_L2TLB_SEQ_EN=0` 才表示
  responder sequence 不主动回包。
- 默认接管且 `MEMBLOCK_L2TLB_SEQ_EN=1` 路径下，`tc_sanity/base_fun` 这类非 dispatch
  testcase 需要单独 cfg 关闭 L2TLB sequence，或保证主表已建立；若需要只观察真实 DUT
  信号，使用编译期 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0`。
- 保留风险：当前仅接入 DTLB Repeater 汇总后的 `io_ptw_req_0`/`io_tlb_1` 通道，不单独
  建模 load/store/prefetch 各 TLB 子请求的 `getGpa/vector` 扩展字段。该实现满足当前
  L2TLB responder 最小合法需求；若后续 directed case 要区分子 TLB 源，需要扩展
  interface/transaction 和 connect 字段。

验收结果：

- `git diff --check` 覆盖 Task18 文件集，通过。
- `rg -n "u_L2tlb_agent_agent|pack_L2tlb_agent|MEMBLOCK__L2TLB_AGENT_CONNECT|channel_id = 19" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 通过，确认 env/cfg/RM/common xaction/connect/user_cfg/documentation 关键接入点存在。
- `cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_sanity mode=base_fun` 通过，日志显示 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。
- `cd mem_ut/ver/ut/memblock/sim && make eda_run tc=tc_sanity mode=base_fun` 通过，日志显示 `TEST CASE PASSED`，`UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。

### Task19：真实 DUT dispatch 联动 smoke 入口

目标：在 Task14-18 已完成真实 LSQ admission、lintsissue 发射、LSQ commit、monitor
fact adapter 和 L2TLB responder 接入后，补齐一个可启动的真实 DUT 最小联动 testcase。
Task19 不模拟复杂后端 replay/redirect 策略，只提供最小合法 orchestrator：生成主表，
等待各 agent default sequence 并行完成真实端口驱动，持续 drain 真实 monitor 事实并
检查公共状态表最终收敛。

实现文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`
- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/mem_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/writeback_status_handler.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv`
- `mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`

新增 class/API：

- `memblock_dispatch_real_smoke_sequence`
  - 字段：
    - `service_vif`：复用 `lintsissue_agent_agent_interface` 的 `clk/rst_n` 作为
      real smoke 主控 service clock。
  - task/function：
    - `body()`：调用 `build_main_table()` 生成主表并置 `main_table_ready`；随后进入
      `service_real_dispatch_flow()`，最终调用 `data.end_test_check()`。
    - `service_real_dispatch_flow()`：每个 service clock 下降沿调用 `service_monitor_once()`、
      `route_all_issue_queues()`，并以 `all_transactions_success()` 作为唯一退出条件。
    - `service_monitor_once()`：依次 drain CSR、writeback、异常/redirect/deq 事实，并调用
      `exception_redirect_replay_task()` 消费 pending recovery 事件，更新公共状态表。
    - `all_transactions_success()`：要求三类发射队列为空、active ROB/LQ/SQ map 为空、全局
      flush/redirect idle，并且所有 uid `success=1`。
    - `report_unfinished_status()`：timeout 时打印每个未完成 uid 的关键状态位和队列/map 汇总。

- `tc_dispatch_real_smoke`
  - 字段：
    - `real_smoke_cfg`：本 testcase 的 `memblock_env_cfg`。
  - task/function：
    - `build_phase()`：调用 `seq_csr_common::reload_from_plus()`，把 Makefile `cfg=<cfg_name>`
      和用户 `plus_arg` 已解析出的 plus 参数刷新到公共快照；随后创建并设置 env cfg，
      调用 `super.build_phase()`，最后配置本 testcase 的 default sequence 覆盖和 timeout。
    - testcase 源码不再维护 `configure_real_smoke_plus_defaults()`、
      `get_user_int_plusarg()` 或 `get_user_bit_plusarg()`；最小 load-only 场景由
      `seq/plus_cfg/tc_dispatch_real_smoke.cfg` 提供，运行命令显式写
      `cfg=tc_dispatch_real_smoke`。
    - `configure_real_smoke_env_cfg(cfg)`：关闭各 agent X/Z 检查，避免真实最小联动阶段被无关
      passive 状态输出阻塞。
    - `configure_real_smoke_default_sequences()`：把与本任务无关的 backend/fence/csr/vec/redirect/
      itlb/other_ctrl agent 覆盖为空 default sequence；LSQ enqueue、lintsissue、lsqcommit、
      L2TLB、dcache/sbuffer responder 保持 `tc_base` 中的真实 default sequence。
    - `main_phase()`：显式创建并启动 `memblock_dispatch_real_smoke_sequence`。

- `memblock_lintsissue_dispatch_sequence`
  - 新增 task：
    - `wait_for_main_table()`：使能后先等待 `common_data_transaction.main_table_ready=1`，防止
      lintsissue default sequence 早于主控 vseq 建表完成时进入空转。等待期间只按统一
      no-progress 阈值 warning。

- `memblock_lsqcommit_dispatch_sequence`
  - 新增 task：
    - `wait_for_main_table()`：使能后先等待 `main_table_ready=1`，之后再进入原有
      commit loop，避免主表未生成前读取空状态。

- `memblock_l2tlb_base_sequence`
  - 新增保护：
    - 若 `MEMBLOCK_L2TLB_SEQ_EN=1` 但 connect 层没有激活 L2TLB responder takeover 路径，
      sequence 直接 fatal，避免静默等待一个不会被驱动的 DTLB/L2TLB 连接。
  - 当前调度：
    - 不等待 `main_table_ready`。使能和 connect takeover 检查通过后，直接进入 L2TLB
      request polling；只有 DUT request 到来时才查/建 TLB entry 并回包。

- `memblock_sync_pkg/L2tlb_agent_connect.sv`
  - 新增 `l2tlb_responder_active`：connect 层 time-0 设置该标志，sequence 侧用它检查
    L2TLB responder takeover 路径是否真的激活；testcase 不直接写该标志，避免绕过真实
    connect 激活检查。
  - 历史实现曾用 `+MEMBLOCK_L2TLB_SEQ_EN=1` 或 `+UVM_TESTNAME=tc_dispatch_real_smoke`
    激活 responder 接管路径。最新实现改为编译期
    `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 控制 connect-time 接管，避免 runtime plusarg
    和 testcase 名称影响 time-0 connect 结构。

- `plus.sv/seq_csr_common.sv/default.cfg`
  - 旧版曾新增 `MEMBLOCK_DISPATCH_ISSUE_START_TIMEOUT`。当前已删除，等待主表期间只按
    `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` 周期性 warning，真正卡死由 testcase/UVM
    timeout 兜底。
  - 已取消 `MEMBLOCK_L2TLB_START_TIMEOUT`：L2TLB responder 不再等待主表 ready。
  - 已取消 `MEMBLOCK_REAL_SMOKE_MAX_SERVICE_STEPS`：真实联动 smoke 主控不再使用软件
    轮询上限参数，改由 service clock 下降沿 `forever` loop 等待收敛。

- `dcache_mem__access_base_sequence`
  - 新增 helper：
    - `is_acquire_opcode(opcode)`：识别 TileLink `AcquireBlock/AcquirePerm`。
    - `dcache_d_opcode(opcode)`：按 A 通道 opcode 选择 D 通道 opcode，`AcquireBlock`
      返回 `GrantData`，`AcquirePerm` 返回 `Grant`。
    - `dcache_d_beats(opcode, size)`：按 32B beat 计算 `AcquireBlock` 需要返回的 D beat 数，
      `size=6` 时返回 2 beat。
    - `dcache_beat_mask(opcode, req_mask)`：`AcquireBlock/AcquirePerm` 按整 beat 访问内存，
      其他请求沿用 A 通道 mask。
  - `body()` 改为看到 A valid 后先发送一拍 A ready 接受请求，再按请求类型发送一拍或多拍
    D response；对 `AcquireBlock` 逐 beat 读取 backing memory 并打印 `accept DCache A` /
    `send DCache D beat` 诊断日志。
  - `sbuffer_mem_access_base_sequence` 同步改为先用一拍 A ready 接受请求，再等待 D ready
    返回响应，避免 idle ready 常开造成同一 valid 被重复采样。

调度关系：

- `tc_dispatch_real_smoke` 不在 build 阶段设置 plus 默认值；最小 load-only 参数由运行命令
  指定的 `tc_dispatch_real_smoke.cfg` 提供。如果用户 runtime 传入同名 `plus_arg`，则以
  用户值为准。
- main phase 中，agent default sequence 与 testcase 主控 vseq 并行启动。LSQ enqueue、
  lintsissue、lsqcommit sequence 会等待 `main_table_ready`，global stop 前常驻运行，
  避免主表未生成前提前退出；L2TLB responder 不等待主表，只按 DUT request 触发查表/回包。
- 主控 vseq 只负责主表生命周期和 monitor fact drain；真实端口驱动仍由
  `memblock_lsqenq_dispatch_sequence`、`memblock_lintsissue_dispatch_sequence`、
  `memblock_lsqcommit_dispatch_sequence` 和 `memblock_l2tlb_base_sequence` 完成。
- 默认场景为 1 条 int load，用于最小验证 LQ 分配、TLB response、load issue、load
  writeback、ROB commit 和 LQ deq 的闭环；store/AMO 可通过 runtime plusarg 调整权重后
  继续扩展。

问题与解决：

- 问题：已有 Task13 是软件 smoke，不能证明真实 DUT 端口联动可启动。
  解决：新增 `tc_dispatch_real_smoke`，保留真实 agent sequence，主控 vseq 只生成主表和
  drain 状态，形成真实联动入口。
- 问题：`memblock_lintsissue_dispatch_sequence` 原来没有等待主表 ready，可能在 agent
  default sequence 并行启动时进入旧版空转/提前退出路径。
  解决：新增 `wait_for_main_table()`；只有使能 dispatch issue sequence 后才等待，不影响
  默认关闭路径。当前已删除专用 start timeout，等待期间只按统一 no-progress 阈值 warning。
- 问题：真实联动需要小规模默认，但又不能阻止用户 directed plusarg 覆盖。
  解决：当前改为 Makefile `cfg=<cfg_name>` 选择 testcase preset cfg，例如
  `cfg=tc_dispatch_real_smoke`。标准 flow 先把 `${plus_file}/${cfg}.cfg` 内容展开成
  runtime plusargs；展开时过滤掉用户 `plus_arg` 已经指定的同名 key，然后再拼接用户
  `plus_arg`，因此用户命令行同名 plusarg 可覆盖 testcase preset。
- 问题：主控 loop 的退出条件如果只看 `success`，可能忽略残留队列或 active map。
  解决：当前公共 success prefix 达到 `main_trans_num` 后只负责置位
  `global_stop_requested`，残留 issue queue、active ROB/LQ/SQ map、flush/redirect/raw queue
  等由最终一致性检查兜底报错。
- 问题：subagent review 指出 build phase 写 `plus::MEMBLOCK_L2TLB_SEQ_EN` 无法影响
  `L2tlb_agent_connect.sv` 的 time-0 force gate。
  最新解决：connect 层不再解析 runtime plusarg 或 testcase 名称，统一由编译期
  `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 设置真实激活状态并写入
  `memblock_sync_pkg::l2tlb_responder_active`；L2TLB sequence 启动时检查该标志，不一致则
  fatal。
- 问题：subagent review 指出 L2TLB 和 lsqcommit sequence 缺少严格主表 ready gate。
  最新处理：lsqcommit 保留 `wait_for_main_table()`；L2TLB responder 已取消主表等待，
  只保留 connect takeover 激活检查，并在 DUT request 到来时按 req/runtime CSR 查表或建表。
- 问题：真实 load issue fire 后，`tc_dispatch_real_smoke` 卡在 writeback/pass/commit/deq
  未完成。日志显示 LoadUnit 已发起 DCache A 请求，opcode 为 `AcquireBlock`。
  解决：原 `dcache_mem__access_base_sequence` 对所有非 store A 请求只返回单拍
  `AccessAckData`，不能满足 `AcquireBlock` 的多 beat refill 语义；已改为按 TileLink
  opcode 返回 `GrantData/Grant/AccessAckData/HintAck`，并对 `AcquireBlock size=6` 返回
  2 个 32B D beat。修复后真实 load miss 可以 refill 并继续写回。

验收计划：

- `git diff --check` 覆盖 Task19 文件集。
- `rg -n "memblock_dispatch_real_smoke_sequence|tc_dispatch_real_smoke|MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- subagent 独立 review Task19，重点检查 main_phase 并行调度、cfg/plusarg 覆盖策略、主表 ready gate、
  默认 `tc_sanity` 风险和真实最小联动退出条件。
- `cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_sanity mode=base_fun`
- `cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke`
- `cd mem_ut/ver/ut/memblock/sim && make eda_run tc=tc_sanity mode=base_fun`
- `cd mem_ut/ver/ut/memblock/sim && make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke`

Subagent review：

- 第一轮 review 结论：存在 blocker。主要问题是 `tc_dispatch_real_smoke` 在 build phase
  写 `plus::MEMBLOCK_L2TLB_SEQ_EN` 不能影响 `L2tlb_agent_connect.sv` 的 time-0 接管选择；
  同时 L2TLB 和 lsqcommit sequence 缺少严格主表 ready gate。
- 处理结果：已增加 `l2tlb_responder_active` 同步标志，并最终把 connect 激活路径收敛为
  编译期 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN`，同时保留 L2TLB active mismatch fatal；
  lsqcommit 保留 `wait_for_main_table()`，L2TLB 不再等待主表。
- 第二轮 review 结论：blocker 已解决，可以进入远端 compile/run；剩余 medium 建议是
  `l2tlb_responder_active` 应只由 connect 层写，避免 testcase 绕过真实 connect 激活检查。
  处理：已删除 testcase 对该 flag 的直接赋值；`tc_dispatch_real_smoke` 不再写
  `plus::MEMBLOCK_L2TLB_SEQ_EN`，运行命令通过 `cfg=tc_dispatch_real_smoke` 打开 runtime
  L2TLB sequence。
- 第三轮 review 结论：无 blocker，可以提交。review 确认 DCache responder 对
  `AcquireBlock opcode=6 size=6` 返回 2 beat `GrantData opcode=5` 不再阻塞当前
  `tc_sanity` 和 `tc_dispatch_real_smoke`；L2TLB active gate 和三类 sequence 的
  `main_table_ready` gate 没有新的硬伤。保留风险是 DCache responder 仍是最小 TileLink
  manager 模型，不等价于完整协议覆盖，后续专项再扩展。
- 最终 review 结论：无 blocker，可以提交。后续参数管理规则已进一步收敛为
  Makefile `cfg=<cfg_name>` 指定 testcase preset，不再依赖 testcase 内部 plus 覆盖 helper。

验收结果：

- `git diff --check` 覆盖 Task19 文件集，无 whitespace error。
- `rg` 已确认 `memblock_dispatch_real_smoke_sequence`、`tc_dispatch_real_smoke`、
  `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`、
  `is_acquire_opcode`、`dcache_d_beats` 和 `GrantData` 相关实现均命中。
- `make eda_compile tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke` 通过。
- `make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke` 通过，日志
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_dispatch_real_smoke_666666_rtl_.log`
  显示 `L2TLB responder loop start`、`dispatch issue fire`、
  `accept DCache A opcode=6 size=6`、`send DCache D beat=1/2 opcode=5`、
  `send DCache D beat=2/2 opcode=5`、`TEST CASE PASSED`，且
  `UVM_ERROR=0`、`UVM_FATAL=0`。
- `make eda_run tc=tc_sanity mode=base_fun` 通过，日志
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_sanity_666666_rtl_.log`
  显示 `TEST CASE PASSED`，且 `UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。
  该 run 目标按当前 Makefile flow 执行远端编译/stitch 后再仿真，用于确认 DCache
  responder 修改没有破坏基础 sanity。

### Task20：真实 DUT scalar store smoke 闭环

目标：在 Task19 的 load-only 真实联动基础上，补齐 scalar store 的最小真实闭环。该任务
不扩展 AMO/vector/CBO/MMIO，只验证普通 scalar store 能经过 LSQ admission、STA issue、
STD issue、STA feedback/STA writeback pass、STD issue-accept pass、ROB pendingPtr 推进
和 SQ deq/retire。STD 侧当前采用最小合法闭环：普通 store 的 STD port valid/ready
fire 后，sequence 合成一条带 epoch/replay 快照的 normal pass 事件；真实 port 5/6
writeback 仍会被 monitor 采集，但若晚于合成 pass 到达，会被状态 API 的重复完成保护丢弃。

实现文件：

- `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_store_smoke.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`

新增/修改 class/API：

- `io_mem_to_ooo_int_wb_agent_agent_monitor`
  - 新增 raw fact 采集：
    - port 3/4：采集 STA writeback `robIdx/sqIdx/exceptionVec`，写入
      `dispatch_raw_int_wb_t`，`port_id=3/4`。
    - port 5/6：采集 STD writeback `robIdx/sqIdx`，写入 `dispatch_raw_int_wb_t`，
      `port_id=5/6`。
  - 保持 port 0/1/2 load writeback 采集不变，仍采集 `robIdx/lqIdx/exceptionVec`。

- `dispatch_monitor_event_adapter`
  - `convert_raw_int_wb(raw, wb_event)` 按 `raw.port_id` 区分 target：
    - 0/1/2 -> `MEMBLOCK_ISSUE_TARGET_LOAD`、`MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB`，
      使用 LQ key 反查 uid。
    - 3/4 -> `MEMBLOCK_ISSUE_TARGET_STA`、`MEMBLOCK_WB_EVENT_SOURCE_STORE_WB`，
      使用 SQ key 反查 uid。
    - 5/6 -> `MEMBLOCK_ISSUE_TARGET_STD`、`MEMBLOCK_WB_EVENT_SOURCE_STORE_WB`，
      使用 SQ key 反查 uid。
  - `convert_raw_iq_feedback(raw, wb_event)` 对 scalar STA feedback 已按源码语义支持
    `hit=1` 转 normal pass、`hit=0` 转 replay。依据是 `Region.scala` 中
    `finalSuccess := feedBack.valid && feedBack.bits.hit`，以及 `NewStoreUnit.scala` 中
    `feedBackSlow.bits.flushState := tlbResp.bits.ptwBack` 只表示 TLB/PTW-back 状态元信息。
    本文 Task16 中“STA hit=1 不接 pass”的描述已过期，Task20 以当前代码和源码语义为准。
  - store port 的 active uid 解析改为 SQ key；load port 仍用 LQ key。无法解析到 active
    uid 时只低级 log 后丢弃，不污染状态表。

- `memblock_lintsissue_dispatch_sequence`
  - 新增字段：
    - `issue_accept_wb_handler`：专供 issue-accept 合成 pass 使用的
      `writeback_status_handler` 句柄。
  - 新增函数：
    - `item_needs_issue_accept_pass(item)`：只在 `item.target == MEMBLOCK_ISSUE_TARGET_STD`、
      `op_class == MEMBLOCK_OP_CLASS_STORE`、`fuType == MEMBLOCK_FUTYPE_STU` 且
      `lsq_ctrl_model::is_store_fuoptype(fuOpType)` 时返回 1。该过滤避免 CBO 或其它特殊
      STU/manual case 被普通 store 的合成 STD pass 收敛。
    - `make_issue_accept_pass_event(item)`：构造 `MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK`
      normal pass 事件，写入 `uid/rob_key/sq_key/issue_epoch/replay_seq/cycle`，并设置
      `real_wb_valid=1`。
    - `submit_issue_accept_pass(item)`：调用 `writeback_status_handler::handle_event()`，
      复用现有 fault/replay/redirect 优先级和 epoch/replay 条件更新保护。
  - `mark_fired_items()` 只有在 `issue_sched.mark_issue_fire(item)` 成功后才调用
    `submit_issue_accept_pass()`。因此 synthetic pass 的时机在 driver 完成 valid/ready
    fire、scheduler 记录对应 target issue epoch 之后。

- `tc_dispatch_real_store_smoke`
  - 继承 `tc_dispatch_real_smoke`。
  - 当前规则下不再在 testcase build phase 写 plus 默认值；运行命令通过
    `cfg=tc_dispatch_real_store_smoke` 选择 `seq/plus_cfg/tc_dispatch_real_store_smoke.cfg`。
  - 该 cfg 设置 `MEMBLOCK_MAIN_TRANS_NUM=1`，关闭 `INT_LOAD/FP_LOAD/PREFETCH/AMO`
    权重，只打开 `MEMBLOCK_OP_CLASS_STORE_WT=1`。
  - 其它真实联动默认值、agent cfg、主控 sequence 均复用 `tc_dispatch_real_smoke`。

- `tc_dispatch_real_smoke`
  - 当前不再维护 `get_user_int_plusarg()` 或 testcase cfg loader。用户覆盖由 Makefile
    参数展开规则保证：cfg 展开时过滤用户 `plus_arg` 已指定的同名 key，之后再追加
    `plus_arg`，因此命令行 `plus_arg` 优先级高于 testcase preset cfg。

- `L2tlb_agent_connect.sv`
  - 历史实现曾通过 `UVM_TESTNAME` gate 增加 `tc_dispatch_real_store_smoke`。最新规则下
    connect 层不再维护 testcase 名称 gate，真实接管由
    `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 编译期宏统一控制。

- `plus_demo_migration_plan.md`
  - 最新规则改为运行命令通过 Makefile `cfg=<cfg_name>` 指定
    `seq/plus_cfg/<cfg_name>.cfg`，由该 cfg 打开 `MEMBLOCK_L2TLB_SEQ_EN`；connect 层默认
    接管由 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` 提供。

调度关系：

- `tc_dispatch_real_store_smoke` 自身不再改 plus 默认值；op class 权重由运行命令指定的
  `tc_dispatch_real_store_smoke.cfg` 提供，其余仍复用 Task19 的真实联动入口。
- 主控 `memblock_dispatch_real_smoke_sequence` 生成 1 条 store 主表项；LSQ enqueue
  sequence 为该 uid 分配 SQ/LQ 快照并建 TLB 表；scheduler 将同一 uid 拆入 STA 和 STD
  队列。
- lintsissue sequence 分别驱动 STA port 3/4 与 STD port 5/6；`mark_issue_fire()` 为
  STA/STD 各自记录独立 `issue_epoch`。
- STA 侧通过 IQ feedback 或 STA writeback 标记 `sta_pass`；STD 侧在普通 store 的
  STD fire 后提交 synthetic issue-accept pass，真实 port 5/6 writeback 若随后到达则作为
  late duplicate 被 `mark_target_normal_pass()` 的 `target_entry_done()` 保护丢弃。STA/STD
  两者均完成后 `required_targets_done()` 置总 `writeback/pass`。
- LSQ commit sequence 推进 `pendingPtr`，DUT SQ deq monitor 到达后
  `apply_dut_sq_deq()` 释放 SQ active map，最后 `try_retire_committed_uid()` 设置
  `success` 并 retire active ROB。

问题与解决：

- 问题：Task16 阶段文档写明 STA `hit=1` 不作为 pass，但当前代码已将
  `real_wb_valid = hit`。是否合理？
  解决：合理。源码 `Region.scala` 把 `feedBack.valid && feedBack.bits.hit` 接到
  IssueQueue 的 `finalSuccess`，`NewStoreUnit.scala` 中 `feedBackHit` 表示 store S1
  TLB 命中或 unalign tail 命中。因此当前实现保留 `hit=1` pass、`hit=0` replay；
  `flush_state/ptwBack` 只保留为状态元信息，不单独触发 replay。
- 问题：原 `convert_raw_int_wb()` 把所有 int writeback 都当 load，store 的 STD
  writeback 会污染 load target 或被 LQ 反查丢弃。
  解决：按 `intWriteback` flatten 顺序和源码 `BackendParams.scala`/`MemBlock.scala`
  确认端口映射：0/1/2 为 LDU，3/4 为 STA fake/store address writeback，5/6 为
  STD writeback；adapter 按端口选择 target 和 LQ/SQ 反查方式。
- 问题：是否需要新增完整 agent 或修改 connect。
  解决：不需要。本任务只扩展已有顶层端口 monitor 的 raw fact 采集，agent 结构和
  `tb.f/env` 连接不变。
- 问题：STD 是否必须等真实 port 5/6 writeback 后才能置 pass。
  解决：当前普通 scalar store smoke 采用最小合法闭环。STD data uop 的关键合法性是
  lintsissue port 5/6 已经 valid/ready fire，fire 后 store data 进入 DUT 内部 SQ data
  路径；本任务不验证完整 store data memory completion，因此合成 issue-accept pass 足够
  支撑主控状态收敛。该事件仍走 `writeback_status_handler`，受 issue epoch、replay seq、
  fault/replay/redirect 优先级保护；真实 STD writeback 若晚到只会被当作重复 pass 丢弃。
- 问题：子 agent review 提示 CBO 或特殊 STU manual case 可能被合成 STD pass。
  解决：`item_needs_issue_accept_pass()` 增加 `lsq_ctrl_model::is_store_fuoptype()` 判断，
  只接受 SB/SH/SW/SD 普通 store，CBO/MMIO/NC/AMO 等特殊路径后续专项覆盖。
- 问题：`tc_dispatch_real_store_smoke` 需要 L2TLB responder，历史 connect 只识别
  `tc_dispatch_real_smoke`。
  解决：该阶段曾扩展 `UVM_TESTNAME` gate；当前最新规则已改为
  `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 编译期宏统一控制 connect-time 接管，testcase
  只通过 cfg/plus 打开 runtime `MEMBLOCK_L2TLB_SEQ_EN`。
- 问题：旧实现中 `tc_dispatch_real_smoke::get_user_int_plusarg()` 使用出现次数判断用户
  显式覆盖，语义复杂且依赖 default cfg 顺序。
  最新解决：删除 testcase 内部覆盖 helper，统一由 Makefile `cfg=<cfg_name>` 提供 preset，
  并由后续 `plus_arg` 覆盖同名参数。

Subagent review：

- 第一轮 review 指出 `convert_raw_int_wb()` 新增 `target=%0d` 日志后参数列表可能缺少
  `wb_event.target`。当前实现已包含该参数，`git diff --check` 和后续 VCS compile 覆盖该
  分支的格式检查风险。
- review 同时指出 synthetic STD pass 应避免覆盖 CBO/特殊 STU。处理：已在
  `item_needs_issue_accept_pass()` 中增加 `lsq_ctrl_model::is_store_fuoptype()`，只允许
  SB/SH/SW/SD 普通 store。
- 第二轮 review 结论：无必须修复项。review 确认 `$sformatf` blocker 已解决，
  `lsq_ctrl_model::is_store_fuoptype()` 限制有效，include 顺序和默认 testcase 行为未见
  `tc_sanity` 回退风险。保留风险是 STD pass 当前仍是 issue-accept synthetic 收敛，不等价
  于完整 store data/writeback 全路径覆盖；该边界已在本文目标和问题处理里说明。

验收计划与结果：

- `git diff --check` 覆盖 Task20 文件集，通过。
- `rg -n "tc_dispatch_real_store_smoke|MEMBLOCK_OP_CLASS_STORE_WT|port_id = 6|MEMBLOCK_ISSUE_TARGET_STD|item_needs_issue_accept_pass|issue_accept_wb_handler" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 通过，确认 testcase、端口采集、STD target 和 synthetic pass 入口均命中。
- `make eda_compile tc=tc_dispatch_real_store_smoke mode=base_fun cfg=tc_dispatch_real_store_smoke` 通过，命令输出显示
  `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。
- `make eda_run tc=tc_dispatch_real_store_smoke mode=base_fun cfg=tc_dispatch_real_store_smoke` 通过，日志
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_dispatch_real_store_smoke_666666_rtl_.log`
  显示 `L2TLB_CONNECT active=1`、STA/STD `dispatch issue fire port=3/5`、
  `issue-accept STD pass uid=0`、`real dispatch smoke sequence completed`、
  `TEST CASE PASSED`，且 `UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。
  同日志中 `drop stale pass uid=0 target=3/2` 为 late duplicate pass 的 INFO 级保护行为，
  不影响最终状态。
- `make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke` 通过，日志
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_dispatch_real_smoke_666666_rtl_.log`
  显示 load issue fire、DCache refill 两拍返回、`TEST CASE PASSED`，且
  `UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。
- `make eda_run tc=tc_sanity mode=base_fun` 通过，历史日志
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_sanity_666666_rtl_.log`
  曾显示 `L2TLB_CONNECT active=0`、`TEST CASE PASSED`，且
  `UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。当前默认宏改为
  `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` 后，未显式覆盖时预期 active=1。

### Task21：真实 DUT scalar store STD writeback 严格闭环

目标：收敛 Task20 的主要保留风险。Task20 为了让普通 scalar store smoke 快速闭环，
在 STD valid/ready fire 后允许 issue sequence 合成 `STD_FEEDBACK` normal pass；但日志已
证明真实 `io_mem_to_ooo_int_wb` port 5/6 会随后返回 STD writeback。Task21 保留 Task20
默认兼容行为，同时新增严格模式：关闭 synthetic STD pass，要求普通 store 的 `std_pass`
只能由真实 writeback/feedback monitor fact 置位。

实现文件：

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/writeback_status_handler.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_store_wb_smoke.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`

新增/修改 class/API：

- `plus`
  - 新增 bit 参数 `MEMBLOCK_STD_REAL_WB_PASS_EN`。Task21 初始默认 0；当前默认已调整为 1，默认等待真实 STD writeback/pass。
  - `new()` 中通过 `load_bit("MEMBLOCK_STD_REAL_WB_PASS_EN", ...)` 解析，只接受 0/1。

- `seq_csr_common`
  - 新增字段 `std_real_wb_pass_en`：记录是否启用 STD 真实写回严格 pass。
  - `load_from_plus()` 从 `plus::MEMBLOCK_STD_REAL_WB_PASS_EN` 镜像配置。
  - 新增 getter `get_std_real_wb_pass_en()`，供 lintsissue dispatch sequence 使用。

- `memblock_lintsissue_dispatch_sequence`
  - `item_needs_issue_accept_pass(item)` 增加参数门控：当
    `seq_csr_common::get_std_real_wb_pass_en()==1` 时直接返回 0，不再为普通 STD fire
    合成 issue-accept pass。
  - 当参数显式为 0 时保留兼容路径，仍只对普通 SB/SH/SW/SD store 合成 STD pass。

- `writeback_status_handler`
  - `handle_event()` 在 normal pass 成功更新状态后打印
    `normal pass uid/target/source/port/rob/lq/sq`。该日志不改变状态行为，用于严格
    store writeback smoke 验收真实 STD pass 的来源。

- `tc_dispatch_real_store_wb_smoke`
  - 继承 `tc_dispatch_real_store_smoke`。
  - 当前不再覆盖 plus 配置函数；严格 STD writeback 参数由
    `seq/plus_cfg/tc_dispatch_real_store_wb_smoke.cfg` 提供。
  - 该 cfg 设置 `MEMBLOCK_STD_REAL_WB_PASS_EN=1`。该 testcase 的语义就是严格 STD
    writeback 闭环；需要 synthetic pass 兼容路径时使用
    `cfg=tc_dispatch_real_store_smoke`。

- `L2tlb_agent_connect.sv`
  - 历史实现曾通过 `UVM_TESTNAME` gate 增加 `tc_dispatch_real_store_wb_smoke`。
    当前最新规则下，该 testcase 不再依赖名称 gate；connect-time 接管由
    `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` 默认提供。

调度关系：

- 当前默认路径使用真实 STD writeback：STD issue fire 后只记录 dispatch/issue epoch，
  等待真实 port 5/6 writeback。若 testcase cfg 或用户 `plus_arg` 显式设置
  `MEMBLOCK_STD_REAL_WB_PASS_EN=0`，才回到 Task20 兼容路径，STD issue fire 后合成
  issue-accept pass，真实 port 5/6 writeback 若晚到会被重复 pass 保护丢弃。
- `tc_dispatch_real_store_wb_smoke` 通过
  `cfg=tc_dispatch_real_store_wb_smoke` 设置 `MEMBLOCK_STD_REAL_WB_PASS_EN=1`，因此 STD fire 后只记录 dispatch/issue epoch，不更新
  `std_pass`。
- 主控 `memblock_dispatch_real_smoke_sequence` 继续 drain `io_mem_to_ooo_int_wb` raw fact；
  `dispatch_monitor_event_adapter::convert_raw_int_wb()` 已在 Task20 中把 port 5/6 映射为
  `MEMBLOCK_ISSUE_TARGET_STD` 并通过 SQ key 反查 active uid。
- 真实 STD writeback 到达后，`writeback_status_handler::handle_event()` 通过
  `mark_target_normal_pass(uid, STD, issue_epoch, replay_seq, cycle)` 设置 `std_pass`；
  STA pass 与 STD pass 都完成后，`required_targets_done()` 设置总 `writeback/pass`，
  LSQ commit 和 SQ deq 继续沿用 Task20 的真实闭环。

问题与解决：

- 问题：是否应该直接删除 synthetic STD pass。
  解决：不直接删除。该 synthetic pass 仍是普通 store smoke 的快速兼容路径，避免真实
  port5/6 时序变化导致基础 smoke 不稳定；严格路径用新增 testcase 显式覆盖。
- 问题：参数命名应该表达“是否合成 pass”还是“是否要求真实 writeback”。
  解决：采用 `MEMBLOCK_STD_REAL_WB_PASS_EN`。Task21 初始默认 0 表示不强制真实 writeback，
  保持 Task20 行为；当前默认已调整为 1，表示 STD pass 默认必须来自真实 monitor fact。
  如需 Task20 兼容行为，显式置 0。
- 问题：真实 port5/6 writeback 是否已经有 monitor 和 adapter 支持。
  解决：Task20 已补齐 port5/6 raw int writeback 采集和 SQ key 反查。Task20 日志中
  synthetic pass 后 1ns 出现 `drop stale pass uid=0 target=3`，说明真实 STD writeback
  已被转换成 STD normal pass，只是当时被 synthetic pass 先完成后丢弃。
- 问题：严格用例如何证明 STD pass 来自真实 port5/6，而不是其它反馈路径。
  解决：`writeback_status_handler` 增加 normal pass 成功日志，包含 `target/source/port`。
  严格用例验收要求看到 `target=3`、`source=3` 且 `port=5` 或 `port=6`，其中
  `source=3` 对应 `MEMBLOCK_WB_EVENT_SOURCE_STORE_WB`。
- 问题：`default.cfg` 中已有 `+MEMBLOCK_STD_REAL_WB_PASS_EN=0`，是否会覆盖严格用例。
  解决：严格 testcase 不在 build phase 写 plus，而是运行命令显式指定
  `cfg=tc_dispatch_real_store_wb_smoke`；该 cfg 中把该参数置 1。当前 `default.cfg`
  也已调整为 1，默认路径同样等待真实 STD writeback；兼容路径需要显式置 0。

Subagent review：

- 预审建议：Task20 后最适合作为下一项的是参数化 STD issue-accept synthetic pass，并新增
  真实 port5/6 writeback 严格 smoke。原因是范围最小、直接收敛 Task20 保留风险；AMO/CBO、
  scoreboard 和 LSQ commit 精确端口更适合作为后续较大专项。
- 实现后 review 结论：无 blocker，可以提交。
- medium：新增 testcase 文件处于 untracked 状态，需要提交时显式 `git add`。处理：提交前
  单独 stage `tc_dispatch_real_store_wb_smoke.sv`。
- medium：严格用例文档需要明确检查真实 `normal pass target=3 source=3 port=5/6`。
  处理：验收计划和结果已补充该检查点。
- low：`source` 日志当前打印枚举数值而不是字符串。处理：接受，严格用例只需要区分
  `MEMBLOCK_WB_EVENT_SOURCE_STORE_WB` 对应的数值来源。

验收计划：

- `git diff --check` 覆盖 Task21 文件集。
- `rg -n "MEMBLOCK_STD_REAL_WB_PASS_EN|get_std_real_wb_pass_en|tc_dispatch_real_store_wb_smoke" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 命中。
- `make eda_compile tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke` 通过。
- `make eda_run tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke` 通过，日志应看到
  `L2TLB_CONNECT active=1`、STA/STD issue fire、真实 port5/6 对应的
  `normal pass uid=0 target=3 source=3 port=5` 或 `port=6`，且不应出现
  `issue-accept STD pass`。
- 回归 `tc_dispatch_real_store_smoke`，确认默认兼容路径仍通过并仍可看到
  `issue-accept STD pass`。
- 回归 `tc_dispatch_real_smoke` 和 `tc_sanity`，确认 load-only 真实 smoke 与默认 sanity
  不退化。

验收结果：

- `git diff --check` 覆盖 Task21 文件集通过。
- `rg -n "MEMBLOCK_STD_REAL_WB_PASS_EN|get_std_real_wb_pass_en|tc_dispatch_real_store_wb_smoke|normal pass uid=.*source" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 通过。
- `make eda_compile tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke` 通过，远端 VCS/KDB
  日志显示 `0 error(s), 0 warning(s)`。
- `make eda_run tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke` 通过。日志
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_dispatch_real_store_wb_smoke_666666_rtl_.log`
  显示 `L2TLB_CONNECT active=1`、STA/STD `dispatch issue fire port=3/5`、
  `normal pass uid=0 target=3 source=3 port=5`、`TEST CASE PASSED`，
  且 `UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`；严格用例未出现
  `issue-accept STD pass`。
- `make eda_run tc=tc_dispatch_real_store_smoke mode=base_fun cfg=tc_dispatch_real_store_smoke` 通过。日志显示默认兼容路径
  仍有 `issue-accept STD pass uid=0`，后续真实 port5/6 返回被 stale pass 保护丢弃，
  `TEST CASE PASSED`，UVM warning/error/fatal 均为 0。
- `make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke` 通过。日志显示 load issue fire、
  DCache refill beat、`normal pass uid=0 target=1 source=1 port=0`、`TEST CASE PASSED`，
  UVM warning/error/fatal 均为 0。
- `make eda_run tc=tc_sanity mode=base_fun` 通过。历史日志
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_sanity_666666_rtl_.log` 曾显示
  `L2TLB_CONNECT active=0`、`TEST CASE PASSED`，且 `UVM_WARNING=0`、`UVM_ERROR=0`、
  `UVM_FATAL=0`。当前默认宏为 1 时，默认期望 active=1。

### Task22：真实 DUT multi-store 严格 writeback smoke

目标：在 Task21 的单条 store 严格 STD writeback 闭环基础上，增加一个固定两条
普通 scalar store 的真实联动 smoke，用来验证连续 `uid/robIdx/sqIdx` 的最小运行能力。
本任务不扩展 AMO/CBO/MMIO/NC，也不验证复杂乱序或 redirect；重点是确保两条 store
都能完成 LSQ admission、STA/STD issue、真实 STA/STD writeback/pass、ROB commit、
SQ deq 和 active map retire。

实现文件：

- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_multi_store_wb_smoke.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`

新增 class/API：

- `tc_dispatch_real_multi_store_wb_smoke`
  - 继承 `tc_dispatch_real_store_wb_smoke`。
  - `seq/plus_cfg/tc_dispatch_real_multi_store_wb_smoke.cfg` 固定：
    - `MEMBLOCK_MAIN_TRANS_NUM=2`。
    - 只打开普通 store op class 权重，关闭 load/fp-load/prefetch/AMO 权重。
    - `MEMBLOCK_ENQ_PER_CYCLE=1`、`MEMBLOCK_STA_PIP_NUM=1`、`MEMBLOCK_STD_PIP_NUM=1`，
      让两条 store 顺序进入 LSQ 和 STA/STD 发射，避免本任务同时覆盖多端口并发压力。
    - `MEMBLOCK_STD_REAL_WB_PASS_EN=1`，继续要求 STD pass 来自真实 port5/6 monitor
      fact，不走 issue-accept synthetic pass。

- `L2tlb_agent_connect.sv`
  - 历史实现曾通过 `UVM_TESTNAME` gate 增加 `tc_dispatch_real_multi_store_wb_smoke`。
    当前最新规则下，该 testcase 不再依赖名称 gate；connect-time 接管由
    `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` 默认提供。

调度关系：

- `cfg=tc_dispatch_real_multi_store_wb_smoke` 固定生成两条 store 主表项，随后
  `memblock_dispatch_real_smoke_sequence` 仍复用现有主控流程。
- LSQ enq sequence 每拍最多 admission 一条 store，回填对应 `lqIdx/sqIdx` 并建立 active
  ROB/SQ 映射。
- lintsissue sequence 每拍最多发射一个 STA 和一个 STD；严格模式下 STD fire 不合成 pass。
- monitor adapter 从 `io_mem_to_ooo_int_wb` port5/6 生成 STD normal pass，从
  IQ feedback 或 port3/4 生成 STA normal pass。
- 两个 uid 均满足 STA/STD pass 后，LSQ commit sequence 按 uid/ROB 顺序推进；DUT
  `sqDeq` monitor 释放 SQ active map 后，`try_retire_committed_uid()` 设置 success。

问题与解决：

- 问题：直接通过 Makefile `plus_arg="+MEMBLOCK_MAIN_TRANS_NUM=2"` 探测时，旧实现会被
  testcase 内部默认值覆盖，导致 real smoke 日志仍显示 `main_trans_num=1`。
  解决：Task22 不在 testcase build phase 写 `plus::MEMBLOCK_MAIN_TRANS_NUM`，而是通过
  `cfg=tc_dispatch_real_multi_store_wb_smoke` 选择同名 cfg preset；该 cfg 设置
  `MEMBLOCK_MAIN_TRANS_NUM=2`，并由 `seq_csr_common::reload_from_plus()` 镜像配置。
- 问题：两条 store 是否需要同时打满 STA/STD 端口。
  解决：本任务先验证连续条目基础生命周期，不引入多端口并发压力；因此固定
  `MEMBLOCK_ENQ_PER_CYCLE/STA_PIP_NUM/STD_PIP_NUM` 为 1。并发压力后续另做专项。

Subagent review：

- 结论：可以进入编译仿真验收。
- 无 blocker/high/medium 问题。
- low 提示：
  - 新增 testcase 文件提交前必须 `git add`，避免 `tc_pkg.sv` include 在干净 checkout 下缺文件；
    本任务提交时纳入 stage。
  - 本 Task22 文档段需要从“待执行”更新为实际 review 结论；已在本文档中同步。
- review 当时确认新增 testcase 继承严格 store writeback 路径、固定两条普通 store，并且
  按当时 `UVM_TESTNAME` gate 机制激活 L2TLB responder 的实现范围合理。当前最新规则已由
  编译期 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 取代该 gate。

验收计划：

- `git diff --check` 覆盖 Task22 文件集。
- `rg -n "tc_dispatch_real_multi_store_wb_smoke|MEMBLOCK_MAIN_TRANS_NUM = 2|MEMBLOCK_ENQ_PER_CYCLE = 1" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 命中。
- `make eda_compile tc=tc_dispatch_real_multi_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_multi_store_wb_smoke` 通过。
- `make eda_run tc=tc_dispatch_real_multi_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_multi_store_wb_smoke` 通过，日志应看到
  `main_trans_num=2`、uid0/uid1 的 `normal pass target=3 source=3 port=5/6`、uid0/uid1
  的 STA normal pass、`TEST CASE PASSED`，且 UVM warning/error/fatal 均为 0。
- 回归 `tc_dispatch_real_store_wb_smoke` 和 `tc_sanity`，确认单条严格 store 和默认 sanity
  不退化。

验收结果：

- `git diff --check -- AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md
  mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_multi_store_wb_smoke.sv
  mem_ut/ver/ut/memblock/tc/tc_pkg.sv mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv
  mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` 通过。
- `rg -n "tc_dispatch_real_multi_store_wb_smoke|MEMBLOCK_MAIN_TRANS_NUM = 2|MEMBLOCK_ENQ_PER_CYCLE = 1"
  mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 命中。
- `make eda_compile tc=tc_dispatch_real_multi_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_multi_store_wb_smoke` 通过，远端
  VCS/KDB 日志显示无 error/warning。
- `make eda_run tc=tc_dispatch_real_multi_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_multi_store_wb_smoke` 通过。日志
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_dispatch_real_multi_store_wb_smoke_666666_rtl_.log`
  显示 `L2TLB_CONNECT active=1`、`main_trans_num=2`、uid0/uid1 的
  `normal pass target=3 source=3 port=5`、uid0/uid1 的 STA normal pass、
  `TEST CASE PASSED`，且 `UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。
- `make eda_run tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke` 通过。日志显示
  `L2TLB_CONNECT active=1`、`normal pass uid=0 target=3 source=3 port=5`、
  `TEST CASE PASSED`，且 `UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`；
  额外检查确认严格用例没有出现 `issue-accept STD pass`。
- `make eda_run tc=tc_sanity mode=base_fun` 通过。历史日志曾显示 `L2TLB_CONNECT active=0`、
  `TEST CASE PASSED`，且 `UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`；当前默认宏为 1 时
  默认期望 active=1。

### Task23：真实 DUT load/store 混合严格 writeback smoke

目标：在 Task19 load-only、Task21/22 store-only 严格路径之后，增加一个固定一条 load
和一条普通 scalar store 的真实联动 smoke。该任务验证同一个 testcase 内 LQ/SQ 同时分配、
load 真实写回、store STD 真实写回、ROB commit、LQ/SQ deq 和 active map retire 的最小组合
闭环；不扩展 AMO/CBO/MMIO/NC、vector 或 replay/redirect 压力。

实现文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_mixed_smoke_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_mixed_wb_smoke.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`

新增/修改 class/API：

- `memblock_dispatch_real_mixed_smoke_sequence`
  - 继承 `memblock_dispatch_real_smoke_sequence`。
  - `body()` 不走随机主表，改为调用 `build_directed_mixed_main_table()`，随后复用
    `service_real_dispatch_flow()`、`data.end_test_check()` 和现有真实 monitor drain/commit/deq
    收敛逻辑。
  - `build_directed_mixed_main_table()`：清空手动主表，固定导入 uid0 load、uid1 store。
  - `make_directed_transaction(tr_name, op_class, rob_value, base_addr)`：构造最小合法主表项。
    load 使用 `LDU + LD + numLsElem=1`，store 使用 `STU + SD + numLsElem=1`，地址分别固定到
    `0x8000_1000` 和 `0x8000_2000`，避免同地址相关性影响本 smoke 目标。

- `tc_dispatch_real_smoke`
  - 新增 virtual task `run_real_smoke_sequence()`，默认创建并启动
    `memblock_dispatch_real_smoke_sequence`。
  - `main_phase()` 保持 objection、`dispatch_real_smoke_active` 和 `super.main_phase()` 时序不变，
    只把原内联 sequence 启动逻辑改为调用该 hook，便于子 testcase 复用真实联动环境并替换
    主控 sequence。

- `tc_dispatch_real_mixed_wb_smoke`
  - 继承 `tc_dispatch_real_store_wb_smoke`，继续启用严格 STD 真实 writeback pass。
  - `seq/plus_cfg/tc_dispatch_real_mixed_wb_smoke.cfg` 固定：
    - `MEMBLOCK_MAIN_TRANS_NUM=2`。
    - `MEMBLOCK_USE_MANUAL_MAIN_TABLE=1`，表达该 testcase 使用手动主表语义。
    - `MEMBLOCK_ENQ_PER_CYCLE=1`、`MEMBLOCK_LOAD_PIP_NUM=1`、
      `MEMBLOCK_STA_PIP_NUM=1`、`MEMBLOCK_STD_PIP_NUM=1`，避免本任务同时覆盖多端口压力。
    - `MEMBLOCK_STD_REAL_WB_PASS_EN=1`，禁止 STD issue-accept synthetic pass。
  - 覆盖 `run_real_smoke_sequence()`，启动 `memblock_dispatch_real_mixed_smoke_sequence`。

- `L2tlb_agent_connect.sv`
  - 历史实现曾通过 `UVM_TESTNAME` gate 增加 `tc_dispatch_real_mixed_wb_smoke`。当前最新
    规则下，该 testcase 不再依赖名称 gate；connect-time 接管由
    `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` 默认提供。

调度关系：

- testcase build phase 复用真实联动默认环境配置、LSQ enqueue、lintsissue、lsqcommit、
  L2TLB responder 和 monitor adapter。
- main phase 中，父类 `main_phase()` 仍负责设置 `dispatch_real_smoke_active` 并启动环境默认
  sequence；子类只替换主控 vseq。
- mixed vseq 导入两条手动主表后，真实 LSQ enqueue sequence 按 uid 顺序 admission：
  load 分配 LQ，store 分配 SQ，并分别构建 TLB 表和路由发射队列。
- load 通过 load issue 和 DCache/L2TLB 路径完成真实 writeback；store 通过 STA/STD issue，
  严格模式下 STD pass 必须来自 `io_mem_to_ooo_int_wb` port5/6。随后 LSQ commit sequence
  推进 ROB commit，DUT lqDeq/sqDeq monitor 事实释放 active LQ/SQ 映射。

问题与解决：

- 问题：只通过 load/store 权重随机生成两条主表，不能稳定保证每次都是一条 load 和一条
  store。
  解决：新增 mixed vseq 使用手动主表固定 uid0 load、uid1 store，保持 testcase 可重复。
- 问题：子 testcase 如果直接重写 `main_phase()`，容易复制父类 objection 和
  `dispatch_real_smoke_active` 逻辑。
  解决：在 `tc_dispatch_real_smoke` 中抽出 `run_real_smoke_sequence()` hook，父类
  `main_phase()` 只调用 hook；mixed testcase 只覆盖 hook。
- 问题：手动主表是否需要依赖 `MEMBLOCK_USE_MANUAL_MAIN_TABLE`。
  解决：mixed vseq 直接调用 `import_manual_main_table()`，不依赖 `build_main_table()` 的模式分支；
  testcase 仍置 `MEMBLOCK_USE_MANUAL_MAIN_TABLE=1`，用于日志和配置语义对齐。

验收计划：

- `git diff --check` 覆盖 Task23 文件集，新建文件用 `git diff --no-index --check` 检查。
- `rg -n "memblock_dispatch_real_mixed_smoke_sequence|tc_dispatch_real_mixed_wb_smoke|run_real_smoke_sequence|MEMBLOCK_USE_MANUAL_MAIN_TABLE" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 命中。
- subagent 独立 review Task23 文件，重点检查 hook 时序、手动主表合法性、严格 STD
  writeback、L2TLB gate 和默认 testcase 回退风险。
- `make eda_compile tc=tc_dispatch_real_mixed_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_wb_smoke` 通过。
- `make eda_run tc=tc_dispatch_real_mixed_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_wb_smoke` 通过，日志应看到
  `main_trans_num=2`、load `normal pass target=1 source=1`、store
  `normal pass target=3 source=3 port=5/6`、LQ/SQ deq 后 `TEST CASE PASSED`，且
  UVM warning/error/fatal 均为 0；严格用例不应出现 `issue-accept STD pass`。
- 回归 `tc_dispatch_real_smoke` 和 `tc_sanity`，确认 hook 和 L2TLB gate 修改不退化。

Subagent review：

- 结论：无 blocker/high/medium，可以进入仿真验收。
- review 确认 `run_real_smoke_sequence()` hook 没有改变父类 objection、
  `dispatch_real_smoke_active` 和 default sequence 时序；mixed testcase 只替换主控 vseq。
- review 确认手动主表按 key 0/1 稳定导入 uid0 load、uid1 store，字段组合为
  `INT_LOAD/LDU/LD` 和 `STORE/STU/SD`，能通过现有主表校验。
- review 确认 strict STD writeback 仍生效：mixed testcase 继承严格 store writeback 路径并
  显式设置 `MEMBLOCK_STD_REAL_WB_PASS_EN=1`，`item_needs_issue_accept_pass()` 在该开关下
  不会合成 STD pass。
- review 当时确认 include 和 L2TLB gate 完整，默认 `tc_dispatch_real_smoke` 仍启动原主控
  sequence，且 `tc_sanity` 未被当时的 `UVM_TESTNAME` gate 自动打开。当前最新规则已由
  编译期 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 取代名称 gate。
- low：新增 mixed sequence/testcase 文件提交前仍是 untracked，若漏 stage 会导致 clean checkout
  下 `tc_pkg.sv` include 缺文件。处理：最终提交时将新增文件纳入 Task23 path-limited stage。
- low：Task23 文档 review/验收结果提交前仍为待执行。处理：本节已更新。

验收结果：

- 静态检查通过：
  - `git diff --check -- AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv mem_ut/ver/ut/memblock/tc/tc_pkg.sv mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
  - `git diff --no-index --check /dev/null mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_mixed_smoke_sequence.sv`
  - `git diff --no-index --check /dev/null mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_mixed_wb_smoke.sv`
  - `rg -n "memblock_dispatch_real_mixed_smoke_sequence|tc_dispatch_real_mixed_wb_smoke|run_real_smoke_sequence|MEMBLOCK_USE_MANUAL_MAIN_TABLE" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`
- `make eda_compile tc=tc_dispatch_real_mixed_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_wb_smoke` 通过。第一次编译在 VCS
  stitch 阶段报 `pcf.sdb corrupted`，已确认不是 SV 编译错误；将生成目录
  `sim/base_fun/partitionlib/MemBlock_TNcMkb` 备份为
  `MemBlock_TNcMkb.corrupt_task23_20260519_1605` 后重跑通过。
- `make eda_run tc=tc_dispatch_real_mixed_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_wb_smoke` 通过，日志：
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_dispatch_real_mixed_wb_smoke_666666_rtl_.log`。
  关键证据：`[L2TLB_CONNECT] active=1`、`main_trans_num=2`、uid1 store STD
  `normal pass target=3 source=3 port=5`、uid1 store STA `normal pass target=2 source=5`、
  uid0 load `normal pass target=1 source=1 port=0`、`real mixed dispatch smoke sequence completed`、
  `TEST CASE PASSED`，且 `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0。
- 严格 STD 检查通过：mixed smoke 日志中未出现 `issue-accept STD pass`，说明 store STD pass
  来自真实 writeback monitor，不是 issue-accept synthetic pass。
- 回归 `make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke` 通过，日志：
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_dispatch_real_smoke_666666_rtl_.log`。
  关键证据：`[L2TLB_CONNECT] active=1`、uid0 load `normal pass target=1 source=1 port=0`、
  `TEST CASE PASSED`，且 `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0。
- 回归 `make eda_run tc=tc_sanity mode=base_fun` 通过，日志：
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_sanity_666666_rtl_.log`。
  关键证据：`[L2TLB_CONNECT] active=0`、`TEST CASE PASSED`，且
  `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0。

### Task24：真实 DUT scalar store STA/STD writeback 严格闭环

目标：在 Task21 已要求 STD pass 来自真实 `io_mem_to_ooo_int_wb` port5/6 后，补齐
普通 scalar store 的 STA 严格路径。默认 store smoke 仍允许 STA IQ feedback hit 作为
STA normal pass；本任务新增严格 testcase，要求 STA pass 必须来自真实 int writeback
port3/4，同时继续要求 STD pass 来自真实 port5/6。本任务不扩展 AMO/CBO/MMIO/NC、
vector 或 replay/redirect 压力。

实现文件：

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_store_sta_wb_smoke.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`

新增/修改 class/API：

- `plus`
  - 新增 bit 参数 `MEMBLOCK_STA_REAL_WB_PASS_EN`。Task24 初始默认 0；当前默认已调整为 1，默认等待真实 STA writeback/pass。
  - `new()` 中通过 `load_bit("MEMBLOCK_STA_REAL_WB_PASS_EN", ...)` 解析，只接受 0/1。

- `seq_csr_common`
  - 新增字段 `sta_real_wb_pass_en`：记录是否启用 STA 真实写回严格 pass。
  - `load_from_plus()` 从 `plus::MEMBLOCK_STA_REAL_WB_PASS_EN` 镜像配置。
  - 新增 getter `get_sta_real_wb_pass_en()`，供 monitor adapter 使用。

- `dispatch_monitor_event_adapter`
  - `convert_raw_iq_feedback()` 在 `MEMBLOCK_STA_REAL_WB_PASS_EN=1` 时过滤
    `raw.is_sta && raw.hit` 的 STA IQ feedback pass。
  - `raw.hit=0` 的 replay 类 STA feedback 不被该开关过滤，避免严格 pass 模式误吞异常恢复事实。
    `flush_state` 对应 TLB/PTW-back 状态元信息，不再单独触发 replay。
  - 真实 STA writeback 仍由 `convert_raw_int_wb()` 的 port3/4 路径转换成
    `MEMBLOCK_ISSUE_TARGET_STA`、`MEMBLOCK_WB_EVENT_SOURCE_STORE_WB`。

- `lsq_commit_handler`
  - `mark_rob_commit_uid(uid)` 在 ROB commit 状态更新后打印
    `rob commit uid=... rob=flag/value lq_mapped=... sq_mapped=...`，用于确认
    ROB commit 后是否仍需等待 LQ/SQ deq。
  - `apply_dut_lq_deq()` 和 `apply_dut_sq_deq()` 在整批 deq 起点和 active uid
    预校验通过后，分别打印 `dut lqDeq accept ...` 和 `dut sqDeq accept ...`。

- `common_data_transaction`
  - `release_uid_lq_mapping()`、`release_uid_sq_mapping()` 在删除 active LQ/SQ map 后打印
    `release lq/sq mapping ... lsq_deq=...`。
  - `try_retire_committed_uid()` 在满足 ROB commit 且 LQ/SQ map 均释放后打印
    `try retire committed uid=... success=... rob_commit=... lsq_deq=...`。
  - `retire_active_uid()` 在删除 active ROB map 前打印最终 retire 摘要。以上日志只用于
    Task24 和后续真实 smoke 验收，不改变状态机行为。

- `tc_dispatch_real_store_sta_wb_smoke`
  - 继承 `tc_dispatch_real_store_wb_smoke`，复用 store-only 主表、真实 LSQ enq、
    lintsissue、lsqcommit、L2TLB 和严格 STD writeback 默认配置。
  - `seq/plus_cfg/tc_dispatch_real_store_sta_wb_smoke.cfg` 设置：
    - `MEMBLOCK_STA_REAL_WB_PASS_EN=1`，禁止 STA IQ feedback normal pass 收敛。
    - `MEMBLOCK_STD_REAL_WB_PASS_EN=1`，继续禁止 STD issue-accept synthetic pass。

- `L2tlb_agent_connect.sv`
  - 历史实现曾通过 `UVM_TESTNAME` gate 增加 `tc_dispatch_real_store_sta_wb_smoke`。当前最新
    规则下，该 testcase 不再依赖名称 gate；connect-time 接管由
    `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` 默认提供。

调度关系：

- `cfg=tc_dispatch_real_store_sta_wb_smoke` 固定打开 STA/STD 两个严格开关。
- STA/STD issue 仍由 `memblock_lintsissue_dispatch_sequence` 驱动；STD fire 不合成 pass。
- STA IQ feedback hit 被 adapter 丢弃，不能置 `sta_pass`；真实 port3/4 writeback 到达后，
  `writeback_status_handler` 才能通过 `mark_target_normal_pass(uid, STA, ...)` 设置
  `sta_pass`。
- STD 仍沿用 Task21 的真实 port5/6 writeback 严格路径。STA/STD 均完成后，LSQ commit
  sequence 和 DUT sqDeq monitor 推进 ROB commit、SQ deq 和 active map retire。
- LSQ deq 和 retire 仍由原 monitor adapter、commit handler 和 common data API 完成；
  新增日志只在原状态更新边界打印事实，便于验收确认 SQ deq 和 active map retire。

问题与解决：

- 问题：Task23 的 mixed 日志中 store STA pass 仍是 `source=5`，即 STA IQ feedback，而不是
  真实 int writeback port3/4。
  解决：新增 `MEMBLOCK_STA_REAL_WB_PASS_EN`，严格 testcase 打开后过滤 STA IQ feedback
  normal pass。
- 问题：是否应默认删除 STA IQ feedback pass。
  解决：不删除。STA feedback 是当前普通 store smoke 的兼容收敛路径；严格验证通过新增
  testcase 显式覆盖，默认 `MEMBLOCK_STA_REAL_WB_PASS_EN=0` 保持原行为。
- 问题：严格 STA 开关是否会影响 replay。
  解决：只过滤 `hit=1` 的 STA IQ feedback pass；`hit=0` replay 类 feedback 仍允许进入
  异常恢复路径。`flush_state` 不再单独触发 replay，避免 `hit=1 && flush_state=1`
  被误判为 replay。
- 问题：严格 STA/STD writeback 通过后，日志中仍缺少 SQ deq 和 active map retire 的直接证据。
  解决：在 commit/deq/retire 原路径补充 `UVM_LOW` 日志，验收时直接检查 `dut sqDeq accept`、
  `release sq mapping`、`try retire committed` 和 `retire active`，不再只依赖
  `end_test_check()` 没有 fatal 来间接判断。

验收计划：

- `git diff --check` 覆盖 Task24 文件集，新建 testcase 用 `git diff --no-index --check`。
- `rg -n "MEMBLOCK_STA_REAL_WB_PASS_EN|get_sta_real_wb_pass_en|tc_dispatch_real_store_sta_wb_smoke|dut sqDeq accept|release sq mapping|try retire committed|retire active uid" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md` 命中。
- subagent 独立 review Task24 文件，重点检查 STA feedback 过滤是否只影响 normal pass、
  STD 严格路径是否不退化、L2TLB gate 和 include 是否完整、deq/retire 日志是否不改变状态机行为。
- `make eda_compile tc=tc_dispatch_real_store_sta_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_sta_wb_smoke` 通过。
- `make eda_run tc=tc_dispatch_real_store_sta_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_sta_wb_smoke` 通过，日志应看到：
  `normal pass uid=0 target=2 source=3 port=3/4`、`normal pass uid=0 target=3 source=3 port=5/6`、
  `TEST CASE PASSED`，且 `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0。
- 严格日志不应出现 `issue-accept STD pass`；STA IQ feedback pass 被过滤时应看到
  `drop STA iq feedback pass`。
- 同一日志还应看到 `rob commit uid=0`、`dut sqDeq accept`、`release sq mapping`、
  `try retire committed uid=0` 和 `retire active uid=0`，确认严格 store smoke 完成
  ROB commit、SQ deq 和 active map retire。
- 回归 `tc_dispatch_real_mixed_wb_smoke` 和 `tc_sanity`，确认默认 mixed smoke 与 sanity
  不退化。

Subagent review：

- 子 agent 已独立审查 Task24 文件集。
- 结论：无 blocker/high/medium 问题。
- low：新增 `tc_dispatch_real_store_sta_wb_smoke.sv` 仍为 untracked，提交前需要显式
  stage。处理：本任务提交时纳入目标文件列表。
- 已确认 plus 参数链路完整，STA IQ feedback 过滤只影响 `hit=1` 的
  normal pass，不吞 `hit=0` replay；新增 testcase 保留 STD 严格路径；`tc_pkg.sv` include
  和 `L2tlb_agent_connect.sv` gate 完整；commit/deq/retire 日志仅观察事实，不改变状态机。

验收结果：

- 静态检查通过：
  `git diff --check` 覆盖 Task24 已修改文件，
  `git diff --no-index --check /dev/null tc_dispatch_real_store_sta_wb_smoke.sv`
  对新 testcase 无 whitespace error。
- `make eda_compile tc=tc_dispatch_real_store_sta_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_sta_wb_smoke` 通过，VCS/KDB
  compile 均为 0 error。
- `make eda_run tc=tc_dispatch_real_store_sta_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_sta_wb_smoke` 通过。日志显示：
  `L2TLB_CONNECT active=1`，STD 真实写回 `normal pass uid=0 target=3 source=3 port=5`，
  STA IQ feedback 被过滤 `drop STA iq feedback pass`，STA 真实写回
  `normal pass uid=0 target=2 source=3 port=3`，并且 `TEST CASE PASSED`、
  `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0。
- 严格 STA smoke 日志未出现 `issue-accept STD pass`，说明 STD 仍未走 synthetic pass。
- 同一日志显示 `rob commit uid=0`、`dut sqDeq accept`、`release sq mapping`、
  `try retire committed uid=0`、`retire active uid=0`，确认普通 store 完成 ROB commit、
  SQ deq 和 active map retire。
- 回归 `make eda_run tc=tc_dispatch_real_mixed_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_wb_smoke` 通过。日志显示
  mixed 默认路径仍保留 store STA feedback pass `target=2 source=5`，同时 load 真实写回
  和 STD 真实写回均完成，证明新增严格开关未改变默认 mixed 行为。
- 回归 `make eda_run tc=tc_sanity mode=base_fun` 通过。历史日志曾显示
  `L2TLB_CONNECT active=0`、`TEST CASE PASSED`，且
  `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0；当前默认宏为 1 时默认期望 active=1。

### Task25：真实 DUT load/store mixed STA/STD writeback 严格闭环

目标：在 Task23 的 mixed load/store smoke 和 Task24 的 store-only STA 严格路径基础上，
增加一个 mixed 场景的全严格 testcase。该 testcase 固定一条 load 和一条普通 scalar store，
要求 load pass 来自真实 load writeback，store STA pass 来自真实 port3/4 writeback，
store STD pass 来自真实 port5/6 writeback。本任务不扩展 AMO/CBO/MMIO/NC、vector、
replay/redirect 或多端口并发压力。

实现文件：

- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_mixed_sta_wb_smoke.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`

新增/修改 class/API：

- `tc_dispatch_real_mixed_sta_wb_smoke`
  - 继承 `tc_dispatch_real_mixed_wb_smoke`，复用 Task23 的手动 mixed 主表、
    `run_real_smoke_sequence()` hook、真实 LSQ admission、lintsissue、lsqcommit 和
    L2TLB responder。
  - `seq/plus_cfg/tc_dispatch_real_mixed_sta_wb_smoke.cfg` 保留
    `MEMBLOCK_MAIN_TRANS_NUM=2`、`MEMBLOCK_USE_MANUAL_MAIN_TABLE=1`、
    `MEMBLOCK_ENQ_PER_CYCLE=1`、`MEMBLOCK_LOAD_PIP_NUM=1`、
    `MEMBLOCK_STA_PIP_NUM=1`、`MEMBLOCK_STD_PIP_NUM=1` 和
    `MEMBLOCK_STD_REAL_WB_PASS_EN=1`。
  - 额外强制 `MEMBLOCK_STA_REAL_WB_PASS_EN=1`，使 store STA pass 只能来自真实
    `io_mem_to_ooo_int_wb` port3/4 monitor fact。

- `tc_pkg.sv`
  - include `tc_dispatch_real_mixed_sta_wb_smoke.sv`，位置放在
    `tc_dispatch_real_mixed_wb_smoke.sv` 之后，保证父类已定义。

- `L2tlb_agent_connect.sv`
  - 历史实现曾通过 `UVM_TESTNAME` gate 增加 `tc_dispatch_real_mixed_sta_wb_smoke`。当前最新
    规则下，该 testcase 不再依赖名称 gate；connect-time 接管由
    `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` 默认提供。

- `plus_demo_migration_plan.md`
  - 记录该 testcase 属于真实联动专用 testcase，并说明其同时打开 STA/STD 两个严格开关。

调度关系：

- build phase 使用父类 mixed 默认配置生成固定 load/store 两条主表，并打开真实联动
  LSQ/L2TLB/lintsissue/lsqcommit sequence。
- main phase 仍由父类 hook 启动 `memblock_dispatch_real_mixed_smoke_sequence`。
- store 的 STA IQ feedback hit 会被 Task24 的 adapter 过滤；只有 port3/4 真实 writeback
  能设置 `sta_pass`。
- store 的 STD issue fire 不合成 pass；只有 port5/6 真实 writeback 能设置 `std_pass`。
- load 仍通过 load issue、L2TLB response、DCache responder 和 port0/1/2 load writeback
  完成 pass。三入口均完成后，LSQ commit sequence 推进 ROB commit，DUT lqDeq/sqDeq
  monitor 事实释放 active LQ/SQ 映射。

问题与解决：

- 问题：Task23 mixed smoke 中 store STA pass 仍来自 `source=5` 的 STA IQ feedback，
  只能证明 mixed 默认兼容路径，不能证明 mixed 场景下 STA 真实 writeback 也可闭环。
  解决：新增 mixed 严格 STA testcase，只在该 testcase 打开
  `MEMBLOCK_STA_REAL_WB_PASS_EN`，不改变默认 mixed smoke。
- 问题：是否直接修改 `tc_dispatch_real_mixed_wb_smoke`。
  解决：不修改。保留 Task23 mixed smoke 作为默认兼容回归；新增 testcase 覆盖全严格路径，
  便于区分回归失败来自 STA 严格路径还是 mixed 基础路径。
- 问题：新增 testcase 是否需要新主控 sequence。
  解决：不需要。主表形态、LSQ 资源、发射调度和收敛逻辑与 Task23 完全一致，仅 pass
  来源约束更严格，因此继承父类 testcase 更稳妥。

验收计划：

- `git diff --check` 覆盖 Task25 文件集，新建 testcase 用 `git diff --no-index --check`。
- `rg -n "tc_dispatch_real_mixed_sta_wb_smoke|MEMBLOCK_STA_REAL_WB_PASS_EN|tc_dispatch_real_mixed_wb_smoke" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` 命中。
- subagent 独立 review Task25 文件，重点检查继承配置是否保留 mixed 手动主表和 STD 严格路径、
  STA 严格开关是否生效、`tc_pkg.sv` include 顺序、L2TLB gate 和文档是否完整。
- `make eda_compile tc=tc_dispatch_real_mixed_sta_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_sta_wb_smoke` 通过。
- `make eda_run tc=tc_dispatch_real_mixed_sta_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_sta_wb_smoke` 通过，日志应看到：
  `main_trans_num=2`、load `normal pass target=1 source=1`、store STA
  `normal pass target=2 source=3 port=3/4`、store STD
  `normal pass target=3 source=3 port=5/6`、`drop STA iq feedback pass`、
  `TEST CASE PASSED`，且 `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0。
- 严格日志不应出现 `issue-accept STD pass`，并应看到 LQ/SQ deq 与 active map retire。
- 回归 `tc_dispatch_real_mixed_wb_smoke` 和 `tc_sanity`，确认默认 mixed 兼容路径和基础
  sanity 不退化。

Subagent review：

- 结论：PASS。
- 检查点：继承 `tc_dispatch_real_mixed_wb_smoke` 的方式正确，保留 mixed 手动主表、
  L2TLB responder、真实 LSQ/lintsissue/lsqcommit 调度和 STD 严格路径；新增 testcase
  只额外打开 `MEMBLOCK_STA_REAL_WB_PASS_EN`，默认 `tc_dispatch_real_mixed_wb_smoke`
  行为不受影响。
- 检查点：`tc_pkg.sv` include 顺序正确，L2TLB connect-time 接管由
  `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1` 默认提供，不再按 testcase 名称 gate；文档验收点覆盖 strict STA/STD、load writeback、LQ/SQ deq/retire 和默认回归。

验收结果：

- 静态检查通过：
  `git diff --check -- AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md mem_ut/ver/ut/memblock/tc/tc_pkg.sv mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
  无输出；新建 testcase 使用
  `git diff --no-index --check /dev/null mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_mixed_sta_wb_smoke.sv`
  检查通过。
- `rg -n "tc_dispatch_real_mixed_sta_wb_smoke|MEMBLOCK_STA_REAL_WB_PASS_EN|tc_dispatch_real_mixed_wb_smoke" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
  命中。
- 首次 run 前遇到 VCS 旧生成物问题：
  `Can not open tt file ... vcs_paramclassrepository_TNcMkb.tt`。按最小范围清理
  `mem_ut/ver/ut/memblock/sim/base_fun/exec` 和 `base_fun/partitionlib` 后重新编译。
- `make eda_compile tc=tc_dispatch_real_mixed_sta_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_sta_wb_smoke` 通过。
- `make eda_run tc=tc_dispatch_real_mixed_sta_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_sta_wb_smoke` 通过。日志显示
  `L2TLB_CONNECT active=1`、`main_trans_num=2`、`drop STA iq feedback pass`、
  store STD `normal pass uid=1 target=3 source=3 port=5`、store STA
  `normal pass uid=1 target=2 source=3 port=3`、load
  `normal pass uid=0 target=1 source=1 port=0`、`dut lqDeq accept`、
  `dut sqDeq accept`、`retire active uid=0/1`、`TEST CASE PASSED`，且
  `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0。
- 回归 `make eda_run tc=tc_dispatch_real_mixed_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_wb_smoke` 通过。日志显示
  默认 mixed 路径仍保留 store STA feedback pass `normal pass uid=1 target=2 source=5`
  并丢弃迟到真实 STA pass，`TEST CASE PASSED`，且
  `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0。
- 回归 `make eda_run tc=tc_sanity mode=base_fun` 通过。历史日志曾显示
  `L2TLB_CONNECT active=0`、`TEST CASE PASSED`，且
  `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0；当前默认宏为 1 时默认期望 active=1。

### Task26：软件级 replay 重新入队闭环 smoke

目标：在不依赖真实 DUT replay 触发的前提下，验证公共状态机中的 replay-only 路径：
捕获 STA replay 事件、清理旧 STA dispatch/pass 状态、递增 `replay_seq`、重新放回 STA
发射队列、过滤旧 `issue_epoch/replay_seq` 的迟到 pass、完成第二次 STA pass，并最终
走 ROB commit、SQ deq 和 active map retire。本任务只覆盖软件级最小 replay 链路，
不扩展真实 DUT memory violation redirect、flush cancel、load replay queue 或后端
ROB replayInst。

实现文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_replay_smoke_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_dispatch_replay_smoke.sv`
- `mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_dispatch_smoke.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md`

新增/修改 class/API：

- `soft_test_memblock_dispatch_replay_smoke_sequence`
  - 继承 `soft_test_memblock_dispatch_smoke_sequence`，复用 Task13 的 directed 三条主表：
    load、store、AMO。
  - `body()` 调度顺序：
    1. `build_directed_main_table()` 建表。
    2. `admit_lsq_and_route_issue()` 完成 LSQ/non-LSQ admission、TLB 映射和发射队列路由。
    3. `fire_all_issue_items()` 首次发射全部目标入口。
    4. 对除 store STA 外的所有 fired item 注入 normal pass。
    5. 对 store STA 注入 `replay_valid` feedback，并调用 `exception_redirect_replay_task()`。
    6. 检查 `replay_pending/replay_target_sta`、`replay_seq` 和 STA 状态清理。
    7. 重新路由并只允许一个 STA replay candidate 发射。
    8. 注入旧 `issue_epoch`、当前 `replay_seq` 的 STA pass，确认被条件更新 API 忽略。
    9. 注入旧 `issue_epoch/replay_seq` 的 STA pass，确认旧快照反馈也会被忽略。
    10. 注入 replay 后 STA pass，确认 store 三入口状态重新收敛。
    11. 复用 `commit_and_deq_lsq()` 和 `check_final_status()` 完成提交与资源释放验收。
  - `make_replay_wb_event(item)`：构造 `MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK` 的
    replay-only 事件，带 `uid/rob/sq/issue_epoch/replay_seq` 快照。
  - `make_pass_wb_event_with_snapshot(item, issue_epoch, replay_seq)`：构造旧快照 pass，
    用于验证 replay 后迟到旧反馈不会污染当前状态。
  - `check_replay_pending_state()`、`check_stale_pass_ignored()`、
    `check_replay_final_status()`：分别检查 replay pending、旧反馈过滤和重发后收敛。

- `tc_dispatch_smoke`
  - 新增 `run_dispatch_smoke_sequence()` hook，原有 `main_phase()` 只负责 objection 和调用 hook。
  - 默认 hook 行为不变，仍启动 `soft_test_memblock_dispatch_smoke_sequence`。

- `tc_dispatch_replay_smoke`
  - 继承 `tc_dispatch_smoke`，只覆盖 `run_dispatch_smoke_sequence()`，启动
    `soft_test_memblock_dispatch_replay_smoke_sequence`。
  - build phase 继续复用 `configure_software_smoke_default_sequences()`，所有真实 agent
    default sequence 保持空闲安全默认。

调度关系：

- replay 事件仍经过 `submit_writeback_event()` -> `writeback_status_handler` ->
  `exception_event_q` -> `exception_redirect_replay_task()`，不直接手改状态表。
- `mark_replay_pending()` 会调用 `remove_uid_from_issue_queues()`，清除旧队列残留和对应
  queued 位，仅清被 replay 的目标入口状态；STD 已完成状态保留。
- replay 后 `route_all_issue_queues()` 基于 `replay_target_sta` 只重新生成 STA 队列项；
  `mark_issue_item_fire()` 成功后清该 replay target，并在目标 mask 为空时清
  `replay_pending`。
- 迟到旧 pass 携带旧 `issue_epoch/replay_seq`，`mark_target_normal_pass()` 通过
  `conditional_set_target_status_field()` 校验失败后忽略，不能设置 `sta_pass/pass`。
- replay 后 pass 使用新的 STA `issue_epoch` 和当前 `replay_seq`，最终与已完成的 STD
  pass 合并为 store 总 `writeback/pass`，再进入 commit/deq 流程。

问题与解决：

- 问题：真实 DUT replay/redirect 场景牵涉 memory violation、flush、cancel count 和
  多 agent 同步，当前直接做真实用例风险和范围都过大。
  解决：Task26 先做软件级 replay-only smoke，验证公共状态机核心合法性；真实 DUT
  replay/redirect 后续单独拆任务。
- 问题：新增 testcase 如果直接覆盖 `main_phase()`，容易重复或跳过父类 objection/default
  sequence 配置。
  解决：在 `tc_dispatch_smoke` 中抽出 `run_dispatch_smoke_sequence()` hook，保持原
  testcase 行为不变，新 testcase 只覆盖 hook。
- 问题：旧发射 item 的迟到 pass 是否可能错误更新新状态。
  解决：分别注入旧 `issue_epoch` + 当前 `replay_seq` 的 pass，以及旧
  `issue_epoch/replay_seq` pass，并在状态检查中确认 `sta_pass/pass` 未被置位，
  覆盖 REVIEW-11 的条件更新语义。

验收计划：

- `git diff --check` 覆盖修改文件，新建 sequence/testcase 用 `git diff --no-index --check`。
- `rg -n "soft_test_memblock_dispatch_replay_smoke_sequence|tc_dispatch_replay_smoke|run_dispatch_smoke_sequence" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` 命中。
- subagent 独立 review Task26，重点检查 replay 事件是否通过正式 handler 进入状态机、
  replay 后只重发 STA、旧快照 pass 是否被条件更新过滤、父类 smoke 行为是否保持不变、
  include 顺序是否正确。
- `make eda_compile tc=tc_dispatch_replay_smoke mode=base_fun` 通过。
- `make eda_run tc=tc_dispatch_replay_smoke mode=base_fun` 通过，日志应看到
  `replay pending`、`stale STA pass ignored`、`replay final`、`dispatch replay smoke sequence completed`、
  `TEST CASE PASSED`，且 `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0。
- 回归 `make eda_run tc=tc_dispatch_smoke mode=base_fun`，确认原软件 smoke hook 改造不退化。

Subagent review：

- 初审结论：PASS，无 blocker/high/medium，可以进入编译仿真验收。
- low：建议额外覆盖“旧 `issue_epoch` + 当前 `replay_seq`”的迟到 pass，避免只验证旧
  `issue_epoch/replay_seq` 快照。处理：已增加第一条 stale pass 注入，再注入旧
  `issue_epoch/replay_seq` 快照，两次均要求 `sta_pass/pass` 不被置位。
- 增量复审结论：PASS，无 blocker/high/medium。复审确认 replay 事件仍通过正式
  `exception_redirect_replay_task()` 进入状态机，replay 后只重新发射 STA，父类
  `tc_dispatch_smoke` hook 行为保持兼容，include 顺序正确。

验收结果：

- 静态检查通过：
  `git diff --check -- AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md mem_ut/ver/ut/memblock/tc/tc_pkg.sv mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_dispatch_smoke.sv mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
  无输出；新建 sequence/testcase 用
  `git diff --no-index --check /dev/null mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_replay_smoke_sequence.sv`
  和
  `git diff --no-index --check /dev/null mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_dispatch_replay_smoke.sv`
  检查无 whitespace error。
- `rg -n "soft_test_memblock_dispatch_replay_smoke_sequence|tc_dispatch_replay_smoke|run_dispatch_smoke_sequence" mem_ut/ver/ut/memblock AI_DOC/plan/test_framework/plan/do/dispatch_plan_v2_development_detail_20260614.md mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
  命中。
- `make eda_compile tc=tc_dispatch_replay_smoke mode=base_fun` 通过。
- `make eda_run tc=tc_dispatch_replay_smoke mode=base_fun` 通过。日志
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_dispatch_replay_smoke_666666_rtl_.log`
  显示 `replay pending uid=1 old_epoch=2 old_seq=0 new_seq=1`、两次
  `stale STA pass ignored uid=1 replay_seq=1`、`replay final uid=1 sta_epoch=6 replay_seq=1`、
  `dispatch replay smoke sequence completed`、`TEST CASE PASSED`，且
  `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0。
- 回归 `make eda_run tc=tc_dispatch_smoke mode=base_fun` 通过。日志
  `mem_ut/ver/ut/memblock/sim/base_fun/log/tc_dispatch_smoke_666666_rtl_.log`
  显示 `dispatch software smoke sequence completed`、`TEST CASE PASSED`，且
  `UVM_WARNING/UVM_ERROR/UVM_FATAL` 均为 0。
- 回归过程中曾遇到 VCS partcomp 生成库不一致，现象为
  `Can not open tt file ... vcs_paramclassrepository_kHM6N.tt`。确认无相关
  `vcs/simv` 进程后，将 `base_fun/partitionlib/MemBlock_kHM6N`、
  `base_fun/exec/simv.daidir` 和 `base_fun/exec/simv` 移动到
  `base_fun/stale_vcs_20260519_173220/` 备份目录，再重新
  `make eda_compile tc=tc_dispatch_smoke mode=base_fun` 恢复生成库并完成回归。

## 4. 实施记录

### Task1 记录

状态：已完成。

问题与解决：

- 问题：方案中的 `memblock_base_sequence.sv` 与当前工程已有 `mem_base_sequence.sv` 不一致。
- 解决：不复用 `mem_base_sequence.sv`，新增 `memblock_dispatch_base_sequence.sv` 作为本框架主控入口，避免影响已有 dcache/sbuffer memory responder。
- 问题：第一次远端编译时，VCS 在 `seq_csr_common.sv` 中不接受 `int unsigned'(...)` cast 写法。
- 解决：Task1 不需要复杂类型转换，改为在 `plus::plus_memblock_demo_depth > 0` 后直接赋值给 `main_trans_num`，后续 Task2 再统一实现正式 plus 参数解析和负数防护。

Subagent review：

- 结论：可以进入编译验收。
- 低风险提示：Task1 no-op 骨架未调用 `reset_all_tables()`，这是当前边界内的预期行为；后续真正启用 dispatch flow 前必须在主表生成入口补齐生命周期初始化。
- 低风险提示：提交前需要把本文 Task1 状态从“进行中”更新为实际结果，已处理。

验收结果：

- `rg` 已确认新增 class 和 `tc_pkg.sv` include。
- `make eda_compile tc=tc_sanity mode=base_fun` 第一次失败于 VCS cast 语法，修正后第二次通过。
- 通过日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log` 中显示 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。

### Task2 记录

状态：已完成。

问题与解决：

- 问题：初版只把正式 `MEMBLOCK_*` 字段读入 `seq_csr_common`，但没有为 `real_*`、`op_class_*_wt`、`send_pri` 权重、delay 权重和 `tlb_pte_*_wt` 补齐 getter。
- 解决：补齐全部正式配置 getter，并要求后续代码只通过 `seq_csr_common` getter 读取配置。
- 问题：`load_hex64()` 初版只检查空值和负号，超过 64-bit 的 hex 文本可能在 `$sscanf` 时被截断。
- 解决：增加 `is_legal_hex_string()` 和 `get_hex_digit_count()`，非法字符、空 digit 和超过 16 个有效 hex digit 均 fatal。
- 问题：正式 bit plusarg 使用通用 `%0d` 读取时，`2` 或 `-1` 可能被 bit 截断。
- 解决：增加 `load_bit()`，正式 bit 参数只接受 `0/1`。
- 问题：第一次远端编译失败于 `seq_csr_common::clamp_int()`，VCS 报 `Illegal ref port connection`，原因是 `ref int unsigned value` 后面的 `min_v/max_v` 未显式写 `input`，被工具按 ref 方向继承。
- 解决：将函数声明改为 `clamp_int(input string name, ref int unsigned value, input int unsigned min_v, input int unsigned max_v)`。

Subagent review：

- 初审结论：发现 1 个 blocker 和 1 个 medium；blocker 是 getter 不完整，medium 是 64-bit hex 解析缺少宽度溢出检查。
- 复审结论：getter 和 hex 宽度检查修复后，可以进入编译验证。

验收结果：

- `rg` 已确认 `MEMBLOCK_MAIN_TRANS_NUM`、`MEMBLOCK_GLOBAL_SEND_PRI_EN`、地址复用参数、TLB/PTE 权重和新增 getter 均命中。
- `make eda_compile tc=tc_sanity mode=base_fun` 第一次失败于 `clamp_int()` 参数方向，修复后第二次通过。
- `make eda_run tc=tc_sanity mode=base_fun plus_arg="+MEMBLOCK_MAIN_TRANS_NUM=8"` 通过，日志显示 `TEST CASE PASSED`，`UVM_ERROR=0`，`UVM_FATAL=0`。
- smoke 命令行确认参数顺序为 cfg 展开的默认 `+MEMBLOCK_MAIN_TRANS_NUM=100`
  在前、用户 `plus_arg="+MEMBLOCK_MAIN_TRANS_NUM=8"` 在后，即 runtime `plus_arg`
  可以覆盖默认 cfg。

### Task3 记录

状态：已完成。

问题与解决：

- 问题：主表需要解决 ROB index 回绕和后续 replay/redirect 追溯问题，不能把 `robIdx` 作为权威索引。
- 解决：Task3 以连续分配的 `uid` 作为主表、状态表和 TLB 表的唯一 key；ROB/LQ/SQ index 只保存在 transaction 和 status 快照中，后续仅在 `active` 条目中用于反查。
- 问题：第二类字段和第三类字段是否应提前进入主表。
- 解决：Task3 不把第二类字段和第三类字段放入 `main_control_transaction`。这些字段后续由发射前专门赋值 task 写入 agent transaction，避免主表承担派生字段和后端元信息。
- 问题：`main_control_transaction` 手动导入模式需要复用合法性检查。
- 解决：增加 `post_manual_config(recompute_vaddr)`，手动配置后显式调用该函数完成 `vaddr` 更新和范围检查。

Subagent review：

- 结论：未发现需要在 Task3 修复的 blocker。
- 审查点：SystemVerilog/VCS 语法、`tc_pkg.sv` include 顺序、三张表 API 边界、开发细节文档一致性均通过。
- 验证缺口：subagent 只做静态审查，远端编译由本 agent 执行。

验收结果：

- `git diff --check` 已通过。
- `rg` 已确认 `main_control_transaction`、`status_transaction`、`tlb_transaction`、`main_table_by_uid/status_by_uid/tlb_table_by_uid` 和 `tc_pkg.sv` include 均命中。
- `make eda_compile tc=tc_sanity mode=base_fun` 通过。
- 通过日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log` 中显示 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。

### Task4 记录

状态：已完成。

问题与解决：

- 问题：初始实现曾考虑用 `flag=0` 表示 ROB/LQ/SQ key 无效。
- 解决：参考 `utility/CircularQueuePtr.scala` 和 `RobPtr.needFlush()` 后确认 `flag` 是环形回绕位，不是 valid 位。Task4 改为只检查 `value` 是否在容量范围内；LQ/SQ 是否真正建立映射由 `activate_uid(map_lq,map_sq)` 和状态表 `active_lq_mapped/active_sq_mapped` 显式记录。
- 问题：后续写回/异常迟到反馈可能在 redirect/replay 后污染新状态。
- 解决：新增 `global_issue_epoch`、`alloc_issue_epoch()`、`mark_issue_snapshot()` 和 `conditional_set_target_status_field()`，后续写回类 task 必须在 `active=1`、`issue_killed=0`、target 级 `issue_epoch/replay_seq` 匹配时才更新状态。
- 问题：commit/deq 后是否删除主表或状态表。
- 解决：`retire_active_uid()` 只释放 active ROB/LQ/SQ 映射并清 `active`，不删除 `main_table_by_uid/status_by_uid/tlb_table_by_uid` 历史。
- 问题：公共 API 未来如果误用，在 active 条目上调用 `set_main_transaction()` 或 `init_status_for_uid()` 会覆盖主表或重置状态，但 active 映射仍存在。
- 解决：这两个 API 均增加 active 保护，active 条目被覆盖或重置时直接 fatal。

Subagent review：

- 结论：无 blocker。
- 审查点：`rob_order_util::rob_is_after()` 与 `CircularQueuePtr.>` 规则一致；`activate_uid/lookup_active_uid_by_rob/retire_active_uid` 未把 `flag` 当 valid；LQ/SQ 映射只在 `map_lq/map_sq` 为 1 时建立，只在 `active_lq_mapped/active_sq_mapped` 为 1 时删除。
- 说明：前两轮 subagent 审查长时间未返回，已关闭；第三轮极窄范围复审通过。

验收结果：

- `git diff --check` 已通过。
- `rg` 已确认 `rob_order_util`、`rob_advance`、`rob_is_after`、`rob_need_flush`、`uid_by_active_rob`、`conditional_set_target_status_field`、`activate_uid`、`retire_active_uid` 和 `active_lq_mapped/active_sq_mapped` 均命中。
- `make eda_compile tc=tc_sanity mode=base_fun` 两次通过；第二次是在补充 active 条目保护后重新执行。
- 通过日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log` 中显示 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。

### Task5 记录

状态：已完成。

实现 class/API：

- `memblock_dispatch_types.sv`
  - 新增 FuType/LSUOpType 常量，覆盖 `ldu/stu/mou`、标量 load/store、software prefetch 和基础 AMO/LR/SC。
- `common_data_transaction`
  - `check_main_table_complete()`：检查主表 uid 分配数量和主表/status 表条目完整性。
- `memblock_dispatch_base_sequence`
  - 字段：
    - `manual_main_table_by_rob[int unsigned]`：手动主表导入入口，key 只用于 directed 配置排序，最终权威索引仍为 `uid`。
  - task/function：
    - `build_main_table()`：根据 `MEMBLOCK_USE_MANUAL_MAIN_TABLE` 选择随机生成或手动导入。
    - `build_random_main_table(main_trans_num_i)`：按 uid 顺序生成随机主表，ROB key 按环形规则递增。
    - `import_manual_main_table()`：按手动关联数组导入主表，不调用 randomize。
    - `inject_ls_addr_reuse_by_fuoptype()`：按 plus 权重把 load/store 的 `src_0/imm` 做地址复用。
    - `randomize_main_transaction(tr, uid, rob_key)`：填充 uid、ROB、LSQ 占位、op 模板、send_pri、delay 和派生 `vaddr`。
    - `select_op_class_by_weight()`：按 plus 权重选择 INT load、FP load、store、prefetch、AMO。
    - `apply_minimal_op_template(tr)`：按 op_class 设置 `fuType/fuOpType/lsq_flow`。
    - `random_*_fuoptype()`：生成各类最小合法 `fuOpType`。
    - `select_load_addr_ref()`、`select_prior_store_addr_ref()`：为地址复用选择候选。
    - `init_status_for_main_table()`：主表后处理完成后初始化 status 快照。
    - `validate_main_table_entry(tr, caller)`：校验派生字段、ROB 范围、非 vector LS、op 模板一致性。

调度关系：

- `pre_body()` 仍只初始化配置和公共数据句柄。
- 真正启用 dispatch flow 的派生 sequence 后续调用 `build_main_table()`。
- 主表生成完成后只初始化 status，不建立 active ROB/LQ/SQ 映射；入队 fire 或 non-LSQ admission 成功时才由后续 task 调用 active API。

问题与解决：

- 问题：手动主表导入时是否需要按手动 key 建立 `robIdx -> uid`。
- 解决：不在主表阶段建立 active 映射。手动 key 只用于导入排序，重复活跃 ROB key 留给 admission/`activate_uid()` 检查。
- 问题：异常注入字段是否在 Task5 按权重随机。
- 解决：Task5 默认清零异常控制字段，保留字段和贯通位置；TLB/PTE、异常权重和写回事实处理放到后续任务。
- 问题：AMO/LR/SC 是否消耗 LSQ entry。
- 解决：Task5 只生成 `MOU + ATOMIC` 模板，真实资源行为放入 Task6。

Subagent review：

- 结论：无 blocker。
- 审查点：主表随机/手动两条路径、uid 分配、地址复用后 `vaddr` 重算、非 vector LS 限制、`tc_pkg.sv` include 顺序均可接受。
- 风险：Task5 仍不启动默认 flow，属于预期边界。

验收结果：

- `git diff --check` 已通过。
- `rg` 已确认 `build_main_table`、`build_random_main_table`、`import_manual_main_table`、`inject_ls_addr_reuse_by_fuoptype`、`check_main_table_complete`、FuType/LSUOpType 常量均命中。
- `make eda_compile tc=tc_sanity mode=base_fun` 两次通过。
- 通过日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log` 中显示 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。

### Task6 记录

状态：已完成。

实现文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`

实现 class/API：

- `memblock_dispatch_types.sv`
  - 新增常量：
    - `MEMBLOCK_LSUOP_CBO_ZERO/CLEAN/FLUSH/INVAL`：CBO `fuOpType` 常量，来自 `LSUOpType` 的低位编码。
    - 普通 AMO/LR/SC 和 AMOCAS W/D/Q 常量：用于完整识别合法 `FuType.mou` directed/manual case；随机主表初版只生成 LR/SC 和普通 AMO，不默认生成 AMOCAS。
    - `MEMBLOCK_LSUOP_AMOCAS_W_LO/D_LO/Q_LO`：AMOCAS 低 6-bit 编码，用于 atomic 子 uop 数量派生。
  - 新增 enum：
    - `memblock_op_behavior_kind_e`：统一行为子类，覆盖 `UNKNOWN/LOAD/PREFETCH/STORE/CBO/ATOMIC`。
  - 新增 struct：
    - `memblock_op_behavior_t.kind`：行为子类。
    - `need_alloc[1:0]`：对应源码 `Dispatch.scala` 到 LSQ 的 `needAlloc`，load 为 `2'b01`，store/CBO 为 `2'b10`，atomic 为 0。
    - `uses_lq/uses_sq`：是否实际消耗 LQ/SQ entry。
    - `route_load/route_sta/route_std`：后续发射队列路由 mask。
    - `commit_is_load/commit_is_store/commit_is_normal`：后续 LSQ commit task 的 commitType 分类。
    - `is_prefetch/is_cbo/is_atomic`：debug 和专项路径标识。
    - `num_ls_elem`：本条 transaction 消耗 LSQ entry 数；初版标量 load/store/CBO 为 1，atomic 为 0。
    - `atomic_sta_uop_count/atomic_data_uop_count`：atomic 发射队列需要生成的 STA/data uop 数量。

- `lsq_ctrl_model`
  - 字段：
    - `data`：`common_data_transaction::get()` 句柄。
    - `lq_enq_ptr/sq_enq_ptr`：本地预测的 LQ/SQ 入队尾指针。
    - `lq_deq_ptr/sq_deq_ptr`：本地预测的 LQ/SQ 释放头指针。
    - `lq_free_count/sq_free_count`：本地 LSQ 空闲资源镜像。
  - 函数：
    - `reset()`：重置 LQ/SQ 指针和 free count。
    - `is_vector_ls_futype(fuType)`：识别并拒绝初版不支持的 vector LS。
    - `is_load_fuoptype(fuOpType)`、`is_store_fuoptype(fuOpType)`、`is_prefetch_fuoptype(fuOpType)`、`is_cbo_fuoptype(fuOpType)`、`is_amo_fuoptype(fuOpType)`：集中维护 `fuOpType` 分类。
    - `is_amocas_q_fuoptype()`、`is_amocas_wd_fuoptype()`：派生 AMOCAS 子类 uop 数。
    - `derive_op_behavior(tr)`：从 `fuType/fuOpType` 派生统一行为模板；非法组合直接 fatal。
    - `advance_lq_key()`、`advance_sq_key()`：按环形队列推进 key，`flag` 是回绕位。
    - `rewind_lq_key()`、`rewind_sq_key()`：redirect/cancel 预测恢复时回退 tail 指针。
    - `can_allocate(behavior)`：检查本地 free count 是否能容纳该行为。
    - `preview_allocate(behavior, lq_key, sq_key)`：返回当前 LQ/SQ 边界或待分配 key，不推进指针。
    - `commit_allocate(uid, behavior, tr)`：LSQ 入队 fire 后确认分配，回填 `lqIdx/sqIdx/numLsElem`，写回主表，建立 active 映射，置 `enq=1`，推进指针和 free count。
    - `commit_non_lsq_admission(uid, behavior, tr)`：atomic 等 `needAlloc=0` 的 admission 确认入口，不消耗 LQ/SQ。
    - `release_lq(count)`、`release_sq(count)`：后续 DUT deq monitor 确认释放后更新 head/free count。
    - `cancel_lq(count)`、`cancel_sq(count)`：后续 redirect/cancel monitor 确认取消后回退 tail/free count。

- `common_data_transaction`
  - `activate_uid_by_behavior(uid, behavior)`：按行为模板决定是否建立 LQ/SQ active 映射。当前 `lsq_ctrl_model` 仍直接调用 `activate_uid(uid, behavior.uses_lq, behavior.uses_sq)`；该 API 作为后续 task 的稳定入口保留。

- `memblock_dispatch_base_sequence`
  - 字段：
    - `lsq_ctrl`：`lsq_ctrl_model` 句柄，`pre_body()` 中创建并 reset。
  - 函数：
    - `derive_op_behavior(tr)`：转调 `lsq_ctrl_model::derive_op_behavior()`，作为 base sequence 内统一行为分类入口。
    - `is_load/store/prefetch/amo_fuoptype()`：改为复用 `lsq_ctrl_model` 静态分类，避免多处判断分叉。
    - `apply_minimal_op_template()`：AMO 模板显式设置 `numLsElem=0`。
    - `validate_main_table_entry()`：不再硬编码 `numLsElem=1`，改为比较 `derive_op_behavior().num_ls_elem`。

调度关系：

- Task6 只提供软件模型，不驱动真实 `lsqenq_agent`。
- 后续入队 task 的预期调用顺序是：按 uid 取主表项 -> `derive_op_behavior(tr)` -> 读取 DUT `canAccept/lqCanAccept/sqCanAccept` -> `preview_allocate()` 做本地资源检查 -> 真正 LSQ enq fire 后调用 `commit_allocate()`；atomic 等 non-LSQ admission 成功后调用 `commit_non_lsq_admission()`。
- `release_lq/release_sq/cancel_lq/cancel_sq` 后续只能由 DUT monitor 采样到 deq/cancel 事实后调用，不能在发射或写回时提前释放资源。

问题与解决：

- 问题：load 和 store 是否都需要携带 `lqIdx/sqIdx`。
- 解决：参考 `LSQWrapper.scala`，LSQ enq response 同时返回 `lqIdx` 和 `sqIdx`。Task6 在确认分配时总是回填两个字段；但 `uses_lq/uses_sq` 只表示实际消耗哪个队列 entry。
- 问题：AMO/LR/SC 是否进入 LSQ。
- 解决：参考 `Dispatch.scala`，`FuType.mou` 的 AMO 不通过 LSQ enq 分配，Task6 设置 `need_alloc=0`、`num_ls_elem=0`，只建立 ROB active 映射。
- 问题：CBO 是否独立 op_class。
- 解决：初版不新增主表 op_class；CBO 作为 `fuType=stu` 的 `fuOpType` 子类由 `derive_op_behavior()` 派生为 `MEMBLOCK_OP_BEHAVIOR_CBO`。随机主表仍不默认生成 CBO，后续可通过 directed/manual case 或更细权重开放。
- 问题：AMOCAS.W/D/Q 的 atomic 发射数量如何处理。
- 解决：参考 `LSUOpType` 低 6-bit 编码，Task6 先派生 `atomic_sta_uop_count` 和 `atomic_data_uop_count`，供后续队列路由使用；普通 AMO/LR/SC 为 1/1，AMOCAS.W/D 为 1/2，AMOCAS.Q 为 2/4。随机主表初版不默认生成 AMOCAS，manual/directed case 可通过分类函数识别。
- 问题：subagent review 指出 `is_amo_fuoptype()` 只覆盖 LR/SC、AMOADD 和 AMOCAS pattern，合法普通 AMO directed/manual case 会 fatal。
- 解决：补全 AMOSWAP/AMOXOR/AMOAND/AMOOR/AMOMIN/AMOMAX/AMOMINU/AMOMAXU 的 W/D 编码常量和识别；随机 AMO 模板同步扩展到 LR/SC 和普通 AMO W/D，但仍不默认生成 AMOCAS 多 uop。
- 问题：本地 LSQ free count 释放是否可以靠模型预测。
- 解决：不可以。Task6 只实现 mirror API，文档约束后续必须在 DUT deq/cancel monitor 确认事实后调用 release/cancel API。
- 问题：第一次远端编译通过但 VCS 报 `ENUMASSIGN`，原因是 `memblock_op_behavior_t` packed struct 使用 `'{default:'0}` 初始化时，enum 字段也被裸 `0` 赋值。
- 解决：`make_default_behavior()` 改为逐字段显式初始化，避免 enum 类型 warning。

Subagent review：

- 结论：无 blocker，可以进入远端编译。
- medium：`is_amo_fuoptype()` 普通 AMO 覆盖不完整。处理：已补全普通 AMO W/D 编码和随机模板。
- low：Task6 文档的 review/验收结果待收尾。处理：本节已更新 review 结果，编译后补验收结果。

验收结果：

- `git diff --check` 已通过。
- `rg` 已确认 `memblock_op_behavior_t`、`MEMBLOCK_OP_BEHAVIOR_*`、`class lsq_ctrl_model`、`derive_op_behavior`、`commit_allocate`、`release_lq`、`cancel_lq`、`activate_uid_by_behavior` 和 `tc_pkg.sv` include 均命中。
- `make eda_compile tc=tc_sanity mode=base_fun` 第一次通过但出现 Task6 新代码 `ENUMASSIGN` warning；修复逐字段初始化后第二次远端编译通过。
- 最终通过日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log` 中显示 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。

### Task7 记录

状态：已完成。

实现文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/mmu_csr_runtime_state.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/tlb_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/tlb_map_builder.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`

实现 class/API：

- `memblock_dispatch_types.sv`
  - `memblock_tlb_lookup_key_t.vpn`：TLB lookup 页号 key，来自 `vaddr[63:12]` 的低 52 bit。
  - `memblock_tlb_lookup_key_t.asid`：S1 或 VS-stage ASID。参考 `TLBStorage.scala`，有 S2 翻译时使用 `vsatp.asid`，否则使用 `satp.asid`。
  - `memblock_tlb_lookup_key_t.vmid`：H-stage VMID，来自 `hgatp.vmid`。
  - `memblock_tlb_lookup_key_t.s2xlate`：2-bit 翻译阶段 key，保留 `noS2xlate/onlyStage1/onlyStage2/allStage` 的区分。

- `mmu_csr_runtime_state`
  - 字段：
    - `satp_mode/asid/ppn`、`vsatp_mode/asid/ppn`、`hgatp_mode/vmid/ppn`：TLB key 和后续 L2TLB reply 需要的 MMU CSR 快照。
    - `priv_virt/spvp/imode/dmode/mxr/sum/vmxr/vsum`：权限和虚拟化状态快照。
    - `m_pbmt_en/h_pbmt_en`：PBMT 使能快照，Task7 只保存，后续异常/PBMT 精细化使用。
    - `update_seq`：影响翻译或权限判断的 runtime CSR 代际。只有字段变化或 CSR xaction 中 `satp/vsatp/hgatp/priv.virt_changed` 置位时递增，普通重复采样不递增。
  - 函数：
    - `reset()`：复位 runtime CSR 镜像，默认 M-mode、非虚拟化、`update_seq=0`。
    - `update_from_csr_ctrl(csr_tr)`：从 `csr_ctrl_agent_agent_xaction` 更新 runtime CSR 镜像；后续 CSR monitor 调用该 API。
    - `current_s2xlate_enabled()`：返回当前是否启用二阶段翻译的简化判断。
    - `current_asid(s2xlate)`：根据 `s2xlate` 选择 `satp.asid` 或 `vsatp.asid`。
    - `current_vmid()`：返回 `hgatp.vmid`。
    - `current_priv_mode(use_dmode)`：返回 data 或 instruction privilege mode，Task7 TLB builder 使用 data mode。
    - `make_lookup_key(vpn, s2xlate)`：生成 `{vpn, asid, vmid, s2xlate}` lookup key。

- `tlb_transaction`
  - 新增字段：
    - `lookup_key`：本表项注册到 lookup 索引使用的 key。
    - `addr_low`：`vaddr[14:12]`，用于 8 个 index 派生字段的最小模型。
    - `ppn_low[8]`、`valididx[8]`、`pteidx[8]`：L2TLB reply 后续需要的轻量索引派生字段；Task7 先按页号低位和 `addr_low` 生成一致数据。
    - `level`：PTE level 简化字段，Task7 默认 0。
  - 函数：
    - `update_addr_fields(vaddr_i, paddr_i)`：更新 `vaddr/paddr/vpn/ppn/addr_low`，并调用 `derive_index_fields()`。
    - `apply_csr_state(csr_state, s2xlate_i)`：从 runtime CSR 镜像写入 `asid/vmid/priv_mode/csr_update_seq/s2xlate/lookup_key`。
    - `derive_index_fields()`：设置唯一有效 `valididx[addr_low]`，并派生 `pteidx/ppn_low/level`。
    - `fixup_pte_legal()`：PTE 位合法化；`V=0` 时清 `R/W/X/A/D`，`W=1` 时强制 `R=1`，`R/W/X` 全 0 时强制 `R=1`，`A=0` 时清 `D`。

- `tlb_map_builder`
  - 字段：
    - `data`：`common_data_transaction::get()` 句柄。
  - 函数：
    - `build_tlb_for_uid(uid, s2xlate=0)`：读取主表，创建 per-uid TLB 表项，复制主表异常/PBMT 控制，选择物理地址，按 plus 权重生成 PTE bit，合法化后应用 runtime CSR，写入 `tlb_table_by_uid[uid]`，注册 lookup key，并置 `status.tlb_mapped=1`。
    - `lookup_tlb_uid(key, csr_update_seq, uid)`：转调公共数据 lookup API。
    - `choose_paddr(vaddr)`：把虚拟页号稳定映射到 `seq_csr_common::get_paddr_base/range` 指定的物理地址窗口，保留页内 offset。
    - `randomize_pte_bits(tlb_tr)`：使用 `seq_csr_common` 中的 `MEMBLOCK_TLB_PTE_*_WT` getter 生成 PTE `R/W/X/U/G/A/D/N/V`。
    - `choose_weighted_bit(one_wt, zero_wt)`：按权重返回 bit，权重全 0 fatal。

- `common_data_transaction`
  - 新增字段：
    - `mmu_csr_state`：公共 runtime CSR 镜像，由后续 CSR monitor 维护。
    - `uid_by_tlb_key[memblock_tlb_lookup_key_t]`：lookup key 到当前可用 uid 的索引；只做运行时加速，不替代 per-uid TLB 历史表。
  - 新增函数：
    - `register_tlb_lookup(key, uid)`：注册当前 uid 的 TLB key。若同 key 已存在其他 uid，要求旧/新表项 `ppn` 和 `csr_update_seq` 一致，否则 fatal。比较 `ppn` 而不是完整 `paddr`，因为 key 是页级，页内 offset 不属于映射冲突。
    - `lookup_tlb_uid(key, csr_update_seq, uid)`：返回当前 key 对应 uid，并检查表项 `csr_update_seq` 一致；不一致按 miss 返回 0。
    - `clear_tlb_lookup_index()`：只清 lookup 索引，不删除 `tlb_table_by_uid[]` 历史。

- `memblock_dispatch_base_sequence`
  - 字段：
    - `tlb_builder`：`tlb_map_builder` 句柄，`pre_body()` 中创建。
  - task：
    - `build_tlb_table_for_active_uid(uid, s2xlate=0)`：若 `data/tlb_builder` 为空则兜底创建；要求 `status.active=1` 且 `status.enq=1`，然后调用 `tlb_builder.build_tlb_for_uid()`。后续入队/admission task 成功后调该入口。

调度关系：

- Task7 仍不接真实 DTLB/L2TLB agent，只完成表生成和 lookup API。
- 预期流程是：LSQ 入队 fire 或 non-LSQ admission 成功 -> `activate_uid/commit_allocate/commit_non_lsq_admission` 置 `active/enq` -> `build_tlb_table_for_active_uid(uid, s2xlate)` 生成 per-uid TLB 表 -> 后续 Task8 根据 `tlb_mapped=1` 路由到发射队列。
- `mmu_csr_runtime_state` 初始由 `reset_all_tables()` 复位；后续 CSR monitor 只调用 `update_from_csr_ctrl()` 更新 runtime 镜像。TLB builder 禁止直接从 `seq_csr_common`、plus 参数快照或静态初始配置获取 runtime CSR。
- `tlb_table_by_uid[]` 保留到 testcase 结束；`uid_by_tlb_key[]` 可在 CSR/SFENCE/redirect/flush 或显式重建时清空，不影响历史追溯。

问题与解决：

- 问题：`s2xlate` 是否可以沿用 1 bit。
- 解决：不可以。参考 `TLB.scala/TLBStorage.scala`，`s2xlate` 区分 no S2、only S1、only S2 和 all-stage。Task7 将 `tlb_transaction.s2xlate` 和 lookup key 均保持为 2 bit，避免 onlyStage2/allStage 被截断。
- 问题：runtime CSR `update_seq` 是否每次 CSR monitor 采样都递增。
- 解决：不递增。`update_seq` 表示影响翻译/权限的代际，只在值变化或 CSR xaction 显式 changed 位拉高时递增，避免普通重复采样导致 L2TLB request/reply 之间误判 miss。
- 问题：同一 TLB key 的冲突检查比较完整 `paddr` 还是 `ppn`。
- 解决：比较 `ppn`。lookup key 是页级 `{vpn, asid, vmid, s2xlate}`，同一页内不同 offset 的 `paddr` 低 12 bit 可以不同，不应被当成冲突。
- 问题：PTE 位合法化是否完全覆盖 RISC-V/S1/S2 权限规则。
- 解决：Task7 做最小合法化，只保证基础组合不明显非法；S1/S2 分阶段权限、Svnapot N 位、PBMT/PMA 精细行为在后续异常和 L2TLB reply task 中继续细化。
- 问题：同一 key 被多个 uid 复用时是否引入 canonical uid。
- 解决：不引入。每个 uid 都有自己的 `tlb_table_by_uid[uid]` 追溯记录；lookup 索引只指向当前可用于新请求的 uid。

Subagent review：

- 第一轮 review 超时，已关闭。
- 第二轮窄范围 review 结论：无 blocker，可以提交 Task7。
- medium：`build_tlb_table_for_active_uid()` 直接使用 `data.get_status(uid)`，若绕过 `pre_body()` 直接调用可能空句柄。处理：已在 task 内增加 `data == null` 时 `common_data_transaction::get()` 的兜底。
- low：Task7 文档状态和验收结果待更新。处理：本节已更新。

验收结果：

- `git diff --check` 已通过；新建 `mmu_csr_runtime_state.sv` 和 `tlb_map_builder.sv` 也通过 `diff --no-index --check`。
- `rg` 已确认 `memblock_tlb_lookup_key_t`、`class mmu_csr_runtime_state`、`class tlb_map_builder`、`uid_by_tlb_key`、`register_tlb_lookup`、`lookup_tlb_uid` 和 `build_tlb_table_for_active_uid` 均命中。
- `make eda_compile tc=tc_sanity mode=base_fun` 已通过。
- 通过日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log` 中显示 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。

### Task28：sequence/testcase package 管理结构调整

状态：已完成。

目标：将 sequence 和 testcase 的 package/filelist 管理职责拆开，避免 `tc_pkg.sv` 同时管理 helper、sequence 和 testcase。

实现：

- 新增 `mem_ut/ver/ut/memblock/seq/seq_pkg.sv`：
  - 统一 include `seq/base_seq` 下的公共类型、transaction、helper、base sequence。
  - 统一 include `seq/virtual_sequence` 下真实 dispatch sequence。
  - 统一 include `seq/virtual_sequence/soft_test` 下 software-only smoke sequence。
- 新增 `mem_ut/ver/ut/memblock/seq/seq.f`：
  - 维护 `./base_seq`、`./virtual_sequence`、`./virtual_sequence/soft_test` include path。
  - 编译 `seq_pkg.sv`。
- 更新 `mem_ut/ver/ut/memblock/cfg/tb.f`：
  - 在 `-F ../tc/tc.f` 之前增加 `-F ../seq/seq.f`，保证 `seq_pkg` 先于 `tc_pkg` 编译。
- 更新 `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`：
  - 删除对 `seq` 目录下 SV 文件的直接 include。
  - 增加 `import seq_pkg::*`。
  - 只保留 testcase 文件 include。
- 更新 `mem_ut/ver/ut/memblock/tc/tc.f`：
  - 保留 `./src` 和 `./src/soft_test` include path。
  - 不再包含 `../seq/*` include path。

后续规则：

- 新增或迁移 sequence：修改 `seq/seq_pkg.sv` 和 `seq/seq.f`，必要时确认 `cfg/tb.f` 编译顺序。
- 新增或迁移 testcase：修改 `tc/tc_pkg.sv` 和 `tc/tc.f`。
- soft_test sequence 放在 `seq/virtual_sequence/soft_test`；soft_test testcase 放在 `tc/src/soft_test`，二者分别归各自 package/filelist 管理。
