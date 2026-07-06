# main_control_transaction.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/main_control_transaction.sv`

## 1. 文件定位与使用场景

这是“主表”的一行。框架先生成很多 `main_control_transaction`，每一行代表一条将来要送进 DUT 的访存操作。后续 LSQ 入队、TLB 建表、issue 字段赋值、commit 检查都从这张表取基础信息。

输入来自随机生成或手动 directed 配置。输出给三类消费者：`lsq_ctrl_model` 根据它判断是否需要 LQ/SQ，`tlb_map_builder` 根据 `vaddr` 建 TLB 表，`issue_field_assigner` 把它转成 lintsissue xaction。

控制逻辑字段包括 `uid/op_class/lsq_flow/fuType/fuOpType/numLsElem/delay/send_pri/send_pri_std`，这些字段会决定分配、路由、发射时机和优先级。随接口携带的 payload 字段包括 `src_0/imm/robIdx/lqIdx/sqIdx/tlbAF/tlbPF/tlbGPF/PBMT/pmaAF/corrupt/denied`。

字段：

- `uid`：TB 内部唯一主键，不发 DUT。
- `op_class`、`lsq_flow`：抽象操作类和 LSQ 流向。`op_class` 描述测试框架希望生成哪类操作；`lsq_flow` 描述这条操作应走哪类 LSQ/admission 生命周期，例如 INT/FP load 和 prefetch 都是 `LOAD` flow，store 是 `STORE` flow，AMO 是 `ATOMIC` flow。它不是 DUT 端口字段，主要用于模板一致性检查和 directed 配置可读性。
- `fuType`、`fuOpType`：发往 issue/LSQ 的 DUT 字段。
- `src_0`、`imm`、`vaddr`：地址生成字段，`vaddr = src_0 + sign_extend_imm12(imm)`。
- `robIdx_flag/value`、`lqIdx_flag/value`、`sqIdx_flag/value`：DUT 环形索引字段。
- `numLsElem`：当前初始模型只支持标量 load/store 为 1，AMO 为 0；vector 会 fatal。
- `tlbAF`、`tlbPF`、`tlbGPF`、`PBMT`、`pmaAF`：TLB/PMA/PBMT 控制。
- `corrupt`、`denied`：保留给 cache/TL 返回异常，当前 dispatch 闭环没有完整专项。
- `delay`：进入 issue queue 后的 ready delay。
- `send_pri`、`send_pri_std`：调度优先级；STD 使用 `send_pri_std`，其他 target 使用 `send_pri`。

函数：

- `new(name)`：设置安全默认值。
- `pre_randomize()`：确认 `seq_csr_common` 已初始化。
- `post_randomize()`：更新 `vaddr` 并校验。
- `post_manual_config(recompute_vaddr)`：手动表导入后调用。
- `update_vaddr()`：用 12-bit sign extend imm 更新 `vaddr`。
- `validate_main_transaction()`：校验 `vaddr` 派生关系和优先级范围。
- `get_rob_key()`、`get_lq_key()`、`get_sq_key()`：生成 key struct。
- `sign_extend_imm12(imm_i)`：静态函数，12-bit 立即数符号扩展。

## 2. 字段与函数/task 设计原理

`main_control_transaction` 是主控制表的一行，描述一条将来要送入 MemBlock 的访存操作。它只保存“生成时确定的静态字段”，不保存运行中是否发射、是否回写等状态。

关键字段：

| 字段 | 含义 | 设计原理 |
|---|---|---|
| `uid` | TB 内部唯一编号 | 不回绕，作为主表、状态表、TLB 表的索引。 |
| `op_class`、`lsq_flow` | 框架抽象操作类型和 LSQ 流程类型 | 让 testcase 用更高层语义描述 load/store/AMO，再由 helper 转成具体 `fuType/fuOpType` 和行为；`lsq_flow` 是内部流程标签，不直接发 DUT，用于标记 LOAD/STORE/ATOMIC/CBO admission 生命周期并校验模板一致性。 |
| `fuType`、`fuOpType` | DUT 真实功能单元类型和操作类型 | 发往 DUT 的核心控制字段，必须由合法 op_class 推导或校验，不能完全随机。 |
| `src_0`、`imm`、`vaddr` | 地址源操作数、立即数、最终虚拟地址 | 框架统一用 `src_0 + sign_extend_imm12(imm)` 生成 `vaddr`，后续 TLB 和 issue 字段使用同一个地址。 |
| `robIdx_flag/value` | ROB 指针 | DUT 侧年龄判断和 monitor 反查使用；主表保存初始值，状态表会快照。 |
| `lqIdx_flag/value`、`sqIdx_flag/value` | LQ/SQ 分配结果 | 主表生成时可以为空，LSQ 入队后由 `lsq_ctrl_model` 按 DUT response 写回。 |
| `numLsElem` | LSQ 元素数 | 当前 scalar 简化下多为 1；vector/多元素场景留字段但模型暂不完整支持。 |
| `tlbAF/tlbPF/tlbGPF/PBMT/pmaAF` | 翻译和访问异常控制 | TLB entry builder 会复制到 `memblock_tlb_entry`，用于构造 L2TLB response。`pmaAF` 字段功能保留，后续可完善专项行为。 |
| `corrupt`、`denied` | DCache/TL 返回异常控制 | 当前作为主表可控字段保留，完整返回路径专项未闭环。 |
| `delay` | 进入 issue queue 后等待拍数 | 让调度器可以测试 ready delay 和排队逻辑。 |
| `send_pri`、`send_pri_std` | LOAD/STA 与 STD 发射优先级 | STD 单独字段是因为 store 拆成 STA/STD 两个 target，二者可分别建压测场景。 |

主要函数：

| 函数 | 参数 | 功能和设计原理 |
|---|---|---|
| `pre_randomize()` | 无 | 随机前检查 `seq_csr_common` 已初始化，防止权重/范围还没加载就生成主表。 |
| `post_randomize()` | 无 | 随机后更新 `vaddr` 并做合法性检查，保证进入主表的数据已经自洽。 |
| `post_manual_config(recompute_vaddr)` | 是否重新计算 `vaddr` | 手动导入主表也必须走同样校验；参数允许 directed case 保留手动指定地址。 |
| `update_vaddr()` | 无 | 统一计算 `vaddr = src_0 + sign_extend_imm12(imm)`，避免多个 helper 各算一遍。 |
| `validate_main_transaction()` | 无 | 检查 `vaddr` 自洽、优先级范围合法。后续可继续扩展 fuType/fuOpType 合法性。 |
| `get_rob_key()`、`get_lq_key()`、`get_sq_key()` | 无 | 把 flag/value 打包成统一 key 类型，供 active map 和排序 helper 使用。 |
| `sign_extend_imm12(imm_i)` | 原始 imm | 按 12 bit 立即数规则符号扩展，保证地址计算与访存立即数语义一致。 |
