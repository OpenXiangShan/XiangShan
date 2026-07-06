# Atomic 多 uop 支持 TODO

本文记录 mem_ut dispatch 测试框架中 atomic/AMOCAS 多 uop 支持的背景、当前缺失逻辑、风险和后续需要修改的文件/函数。

## 1. 场景背景

当前测试框架已经在 `lsq_ctrl_model::derive_op_behavior()` 中识别 `MOU/atomic` 类型，并为不同 atomic fuOpType 设置了抽象 uop 数：

```systemverilog
if (is_amocas_q_fuoptype(tr.fuOpType)) begin
    behavior.atomic_sta_uop_count  = 3'd2;
    behavior.atomic_data_uop_count = 3'd4;
end else if (is_amocas_wd_fuoptype(tr.fuOpType)) begin
    behavior.atomic_sta_uop_count  = 3'd1;
    behavior.atomic_data_uop_count = 3'd2;
end else begin
    behavior.atomic_sta_uop_count  = 3'd1;
    behavior.atomic_data_uop_count = 3'd1;
end
```

这些字段表达的是：atomic 不是一个简单的 load/store uop，尤其 AMOCAS 需要地址侧和数据侧多份 uop 协同。当前代码已经记录了这个数量，但还没有把 `uop_count` 展开成多个 issue queue item 或多个真实 issue 发射。

## 2. 背景知识补充

### 2.1 CAS 在体系结构中的含义

CAS 是 Compare-And-Swap，也就是“比较并交换”。在计算机体系结构中，它属于硬件提供的原子读-改-写操作，通常用于多核/多线程同步。

CAS 的抽象行为可以写成：

```c
old = *addr;
if (old == expected) {
    *addr = new_value;
}
return old;
```

其中：

- `addr`：要访问的内存地址。
- `expected`：软件期望内存里当前应该保存的旧值。
- `new_value`：如果旧值等于 `expected`，就写入的新值。
- `old`：硬件从内存中真正读出的旧值，通常需要返回给寄存器。

关键点是：读内存、比较、条件写回这几个动作在体系结构上必须表现为一个不可被其他核打断的原子操作。否则多个核同时修改同一个锁、队列头指针或引用计数时，会出现普通 load/compare/store 无法避免的竞态。

从流水线角度看，CAS 同时具备 load 和 store 的特征：

- 像 load：需要读 cache line，并把旧值返回给目的寄存器。
- 像 store：比较成功时需要把新值写回内存。
- 像同步操作：需要遵守原子性、内存一致性、异常、replay、cache miss 等约束。

因此 CAS 不能简单归类为普通 load 或普通 store。在 MemBlock 中，AMO/LR/SC/AMOCAS 由 `AtomicsUnit` 接管，普通 store pipeline 只提供进入 atomic 单元所需的地址侧和数据侧 uop。

### 2.2 AMOCAS 的操作数语义

在当前 XiangShan RTL 中，AMOCAS 使用 `rd` 作为 compare value，使用 `rs2` 作为 new/swap value。

对应 `AtomicsUnit.scala` 中的关键赋值：

```scala
pipe_req.amo_data := genWdataAMO(rs2, LSUOpType.size(uop.fuOpType))
pipe_req.amo_cmp  := genWdataAMO(rd,  LSUOpType.size(uop.fuOpType))
```

含义是：

- `amo_cmp`：比较值，来自 `rd`。
- `amo_data`：比较成功后要写入的新值，来自 `rs2`。
- response data：cache 返回的旧内存值，之后写回目的寄存器。

普通 AMO 只需要地址 `rs1` 和一个数据操作数 `rs2`。AMOCAS 额外需要 compare value，因此数据侧 uop 数会比普通 AMO 更多。

### 2.3 AMOCAS_Q 需要的数据规模

`AMOCAS_Q` 中的 `Q` 是 Quadword。在当前 `XLEN=64` 的体系下，一个 quadword 是：

```text
128-bit = 16B
```

所以 `AMOCAS_Q` 真正对内存执行 CAS 的访问宽度是 128-bit。它需要两组 128-bit 数据：

- compare value：`{X(rd+1), X(rd)}`
- new value：`{X(rs2+1), X(rs2)}`

也就是说，从数据输入角度看，`AMOCAS_Q` 需要收齐 4 个 64-bit 数据片段：

```text
uopIdx 0: X(rd)      compare low  64-bit
uopIdx 1: X(rs2)     new low      64-bit
uopIdx 2: X(rd+1)    compare high 64-bit
uopIdx 3: X(rs2+1)   new high     64-bit
```

这 4 个数据片段合计是 256-bit 的源操作数输入，但这不代表一次访问 256-bit 内存。真正的 atomic memory operation 宽度仍然是 128-bit。多出来的 128-bit 是 CAS 的 compare value。

`AMOCAS_Q` 还需要两个 STA uop，因为 128-bit 结果需要写回两个整数目的寄存器：

```text
STA uopIdx 0: 提供低 64-bit 返回值对应的 pdest
STA uopIdx 2: 提供高 64-bit 返回值对应的 pdest
```

因此当前测试框架中：

```systemverilog
atomic_sta_uop_count  = 2
atomic_data_uop_count = 4
```

对应的是 RTL 对 `AMOCAS_Q` 的真实拆分需求。

### 2.4 当前访存带宽

当前默认配置下，MemBlock 访存发射/执行资源可以按 load、store address、store data 三类理解：

```text
Load 流水线：3 条 / cycle
STA  流水线：2 条 / cycle
STD  流水线：2 条 / cycle
```

对应源码默认参数：

```scala
LoadPipelineWidth:  Int = 3
StorePipelineWidth: Int = 2
```

mem_ut 当前测试框架中对应的真实流水线参数是：

```text
MEMBLOCK_REAL_LOAD_PIPE_NUM = 3
MEMBLOCK_REAL_STA_PIPE_NUM  = 2
MEMBLOCK_REAL_STD_PIPE_NUM  = 2
```

这个带宽会直接影响 AMOCAS 多 uop 能否一拍送完。例如 `AMOCAS_Q` 需要 4 个 STD/data uop，而当前 STD 只有 2 条流水线，所以理论上至少需要 2 拍才能收齐全部 data uop。它还需要 2 个 STA uop，当前 STA 带宽是 2，所以地址侧在资源允许时可以一拍送完。

### 2.5 当前访存位宽

当前标量普通 load/store 支持的语义访问宽度是：

```text
B = 8-bit  = 1B
H = 16-bit = 2B
W = 32-bit = 4B
D = 64-bit = 8B
```

atomic 普通 AMO/LR/SC 支持：

```text
W = 32-bit
D = 64-bit
```

AMOCAS 支持：

```text
AMOCAS.W = 32-bit
AMOCAS.D = 64-bit
AMOCAS.Q = 128-bit
```

当前核心参数中：

```scala
XLEN = 64
VLEN = 128
```

因此可以总结为：

```text
普通标量 load/store 最大语义访存宽度：64-bit
AMOCAS_Q 最大 atomic 语义访存宽度：128-bit
向量/LSU 数据通路粒度：128-bit
DCache cache line：64B = 512-bit
```

需要注意，“语义访存宽度”和“数据通路/cache line 宽度”不是同一个概念。普通 `ld/sd` 语义上只访问 64-bit；DCache 内部可以用更宽的数据通路或 cache line 组织数据；`AMOCAS_Q` 语义上访问 128-bit，但为了 CAS 还需要额外的 compare value 输入。

## 3. RTL 背景架构知识

RTL 中 atomic 由 `AtomicsUnit` 接管，入口在 MemBlock 的 STA issue 侧，同时数据来自 STD 执行单元。

相关源码：

- `src/main/scala/xiangshan/mem/MemBlock.scala`

关键逻辑：

```scala
val st_atomics = Seq.tabulate(StaCnt)(i =>
  issueSta(i).valid && FuType.storeIsAMO((issueSta(i).bits.fuType))
)

for (i <- 0 until StaCnt) when(st_atomics(i)) {
  issueSta(i).ready := atomicsUnit.io.in.ready
  storeUnits(i).io.stin.valid := false.B
  state := s_atomics(i)
}

atomicsUnit.io.in.valid := st_atomics.reduce(_ || _)
atomicsUnit.io.in.bits := Mux1H(st_atomics, issueSta.map(_.bits))

atomicsUnit.io.storeDataIn.zipWithIndex.foreach { case (stdin, i) =>
  stdin := stdExeUnits(i).io.atomicData
}
```

从这段逻辑可以看出：

- atomic 从 `issueSta` 侧进入 `AtomicsUnit`。
- atomic 会接管 store address 相关控制，`issueSta(i).ready` 由 `atomicsUnit.io.in.ready` 决定。
- atomic 数据来自多个 `stdExeUnits(i).io.atomicData`。
- atomic 执行期间会进入 `s_atomics` 状态，阻塞/覆盖部分普通 store 控制。
- atomic 还会使用 load_0 的 TLB 端口，并在 atomic 状态下关闭硬件预取。

因此，测试框架不能把所有 atomic 都简单看成一个普通 STA + 一个普通 STD。对于 CAS 类操作，特别是更宽的 `AMOCAS_Q`，数据侧需要更多片段，地址侧也可能需要更多 uop 描述。

## 4. 当前测试框架已有抽象

相关文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`

### 4.1 行为结构字段

`memblock_op_behavior_t` 中已有字段：

```systemverilog
bit                         is_atomic;
bit [2:0]                   atomic_sta_uop_count;
bit [2:0]                   atomic_data_uop_count;
```

含义：

- `is_atomic`：该 transaction 是否按 atomic/MOU 行为处理。
- `atomic_sta_uop_count`：地址侧 STA 路径理论需要几个 uop。
- `atomic_data_uop_count`：数据侧 STD/data 路径理论需要几个 uop。

### 4.2 atomic 行为派生

`lsq_ctrl_model::derive_op_behavior()` 中，MOU/atomic 当前被设置为：

```systemverilog
behavior.kind             = MEMBLOCK_OP_BEHAVIOR_ATOMIC;
behavior.need_alloc       = 2'b00;
behavior.route_sta        = 1'b1;
behavior.route_std        = 1'b1;
behavior.commit_is_normal = 1'b1;
behavior.is_atomic        = 1'b1;
behavior.num_ls_elem      = 5'd0;
```

当前抽象含义：

- atomic 不走普通 LQ/SQ allocation：`need_alloc=0`
- atomic 当前不占普通 LSQ entry：`num_ls_elem=0`
- atomic 需要走 STA issue 和 STD issue 路径：`route_sta=1`、`route_std=1`
- atomic commit 分类暂按 normal/non-LSQ commit 处理：`commit_is_normal=1`

### 4.3 当前 uop_count 写入点

`issue_queue_scheduler::make_issue_item()` 中会把 atomic uop count 写入 issue queue item：

```systemverilog
item.uop_count = 1;
if (behavior.is_atomic && target == MEMBLOCK_ISSUE_TARGET_STA) begin
    item.uop_count = behavior.atomic_sta_uop_count;
end else if (behavior.is_atomic && target == MEMBLOCK_ISSUE_TARGET_STD) begin
    item.uop_count = behavior.atomic_data_uop_count;
end
```

当前实际效果：

- STA issue queue item 会记录 `uop_count=atomic_sta_uop_count`。
- STD issue queue item 会记录 `uop_count=atomic_data_uop_count`。
- 但后续选择和发射逻辑没有根据 `uop_count` 展开多个 item。

## 5. 当前缺失逻辑

### 5.1 未按 uop_count 展开 issue queue item

当前 `route_target()` 只生成一个 `memblock_issue_q_item_t`：

```systemverilog
item = make_issue_item(uid, target, behavior);
data.push_issue_queue_item(item);
```

缺失点：

- `atomic_sta_uop_count=2` 时，应该生成 2 个 STA issue item 或等价的多 uop 发射描述。
- `atomic_data_uop_count=4` 时，应该生成 4 个 STD/data issue item 或等价的多 data 发射描述。
- 当前只生成 1 个 STA item 和 1 个 STD item。

### 5.2 `uop_index/uop_count` 未形成完整语义

当前 `memblock_issue_q_item_t` 有：

```systemverilog
int unsigned uop_index;
int unsigned uop_count;
```

但实际使用中：

- `uop_count` 仅被赋值，没有作为展开循环的边界。
- `uop_index` 主要由 issue sequence 按 pipe index 设置，用于映射 issue port。
- 对 atomic 来说，`uop_index` 应该表示该 atomic 拆分后的第几个 atomic uop；当前还没有这个语义。

### 5.3 完成条件仍按 target 一次完成处理

当前 `mark_issue_fire()` 标记 target 已 dispatched：

```systemverilog
set_target_dispatched(item.uid, item.target, 1'b1);
```

缺失点：

- 如果一个 atomic STD 需要 4 个 data uop，不能第 1 个 data uop fire 后就认为整个 STD target dispatched。
- 应该记录每个 atomic target 已发射 uop 数，等全部 `uop_count` 个 uop fire 后再置 target dispatched/pass。

### 5.4 writeback/pass/replay 归属未区分 atomic 子 uop

当前 writeback event 中有 `uop_index` 字段，但 atomic 子 uop 的 issue_epoch、replay_seq、pass/replay/fault 归属还没有定义。

需要明确：

- atomic 的 STA 多 uop 是否每个都有独立 feedback。
- STD/data 多 uop 是否需要独立 pass，还是只作为数据准备，最终由 AtomicsUnit writeback。
- AMOCAS_Q 的 4 个 data uop 是否都需要进入 STD issue agent，还是只需要驱动 `stdExeUnits(i).io.atomicData` 对应端口。

## 6. 当前风险评估

### 6.1 如果当前不测试 atomic

如果当前 memblock dispatch 框架目标主要是：

- load
- store
- CBO
- 普通 LSQ admission/issue/writeback/deq/commit

那么 atomic 多 uop 缺失短期不影响主路径。建议：

- 将 AMO/atomic 生成权重设为 0 或极低。
- 文档注明 AMO/CAS 目前是行为占位，不属于完整支持。
- 避免把 AMOCAS_Q 覆盖率当成真实有效覆盖。

### 6.2 如果测试普通 AMO/LR/SC

普通 AMO/LR/SC 当前抽象为：

```systemverilog
atomic_sta_uop_count  = 1;
atomic_data_uop_count = 1;
```

理论上可以先做最小简化支持。但仍需确认：

- AtomicsUnit 是否需要真实 atomicData。
- STD path 是否能正确提供 atomicData。
- 是否需要等待 AtomicsUnit writeback，而不是简单 issue-accept pass。

### 6.3 如果测试 AMOCAS_W/D/Q

当前逻辑不完整，风险较高：

- AMOCAS_Q 理论需要 2 个 STA uop、4 个 data uop，但 TB 只发 1 个 STA/STD item。
- DUT 可能等待更多 data 或进入 atomic state 后无法按预期完成。
- 状态表可能提前标记 STA/STD dispatched。
- 生成了 AMOCAS_Q transaction，但激励并不等价于合法 RTL atomic flow。

## 7. 需要修改的测试框架文件和函数

### 7.1 `memblock_dispatch_types.sv`

路径：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`

涉及类型：

- `memblock_op_behavior_t`
- `memblock_issue_q_item_t`
- `memblock_wb_event_t`

可能需要补充：

- 明确 `atomic_sta_uop_count` / `atomic_data_uop_count` 注释。
- 明确 `uop_index` 对 atomic 子 uop 的语义。
- 如果需要区分 atomic 子 uop 类型，可增加字段，例如 `atomic_uop_kind` 或 `atomic_piece_index`。

### 7.2 `lsq_ctrl_model.sv`

路径：

- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv`

涉及函数：

- `derive_op_behavior()`
- `is_amocas_q_fuoptype()`
- `is_amocas_wd_fuoptype()`
- `is_amo_fuoptype()`

需要确认/完善：

- 当前 `atomic_sta_uop_count/data_uop_count` 是否和 RTL 最新 AMOCAS 拆分规则一致。
- 当前 atomic `need_alloc=0/num_ls_elem=0/commit_is_normal=1` 是否适用于所有 AMO/LR/SC/AMOCAS。
- 如果 atomic 需要真实 LSQ/commit 行为，需要重新定义 `need_alloc/uses_lq/uses_sq/commit_is_*`。

### 7.3 `issue_queue_scheduler.sv`

路径：

- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`

涉及函数：

- `make_issue_item()`
- `route_target()`
- `route_uid()`
- `select_target_candidates()`
- `mark_issue_fire()`
- `mark_issue_fire_already_accepted()`

需要修改：

- 在 `route_target()` 中，如果 `behavior.is_atomic` 且 `uop_count > 1`，按 count 展开多个 `memblock_issue_q_item_t`。
- 每个 item 需要设置稳定的 `uop_index`，表示 atomic 子 uop index。
- `mark_issue_fire()` 不应在第一个子 uop fire 后直接把整个 target 标成 dispatched。
- 需要维护 per uid/target 的 atomic 子 uop fire 计数或 bitmask。

### 7.4 `common_data_transaction.sv`

路径：

- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`

涉及函数/状态：

- `push_issue_queue_item()`
- `issue_queue_contains()`
- `delete_issue_queue_entry()`
- `mark_issue_snapshot()`
- `set_status_field()`
- `try_retire_committed_uid()`

需要修改：

- 当前 `issue_queue_contains(target, uid, replay_seq)` 可能会阻止同一 uid/target 多个 atomic 子 uop 入队。
- 查重 key 需要加入 `uop_index` 或 atomic piece 信息。
- 删除 queue entry 也需要支持删除指定子 uop，而不是只按 uid/target/replay_seq 删除。
- 状态表需要能表达 target 已发射部分子 uop，而不是简单 boolean。

### 7.5 `memblock_lintsissue_dispatch_sequence.sv`

路径：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`

涉及函数：

- `assign_issue_items()`
- `mark_fired_items()`
- `make_issue_accept_pass_event()`
- `submit_issue_accept_pass()`

需要修改：

- `assign_issue_items()` 当前把 `fired_item.uop_index = pipe_idx`，这会覆盖 atomic 子 uop index。
- 需要区分 issue port index 和 atomic 子 uop index。
- `mark_fired_items()` 中 port 映射仍可用 pipe index，但状态更新需要使用 atomic 子 uop index。
- `make_issue_accept_pass_event()` 中 `wb_event.uop_index = item.uop_index` 需要明确传的是 atomic 子 uop index 还是 issue port index。

### 7.6 `issue_field_assigner.sv`

路径：

- `mem_ut/ver/ut/memblock/seq/base_seq/issue_field_assigner.sv`

涉及函数：

- `assign_issue_item_fields()`
- 各 target 对应字段填充逻辑

需要确认/修改：

- atomic 子 uop 是否需要不同的 `uopIdx`、source/data 字段或 fuOpType 修正。
- AMOCAS_Q 的多个 data uop 是否需要不同数据片。
- STA 子 uop 和 STD/data 子 uop 是否需要不同 issue 字段。

### 7.7 writeback/replay/feedback 相关 handler

路径：

- `mem_ut/ver/ut/memblock/seq/base_seq/writeback_status_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv`

需要确认/修改：

- atomic final writeback 来源是 AtomicsUnit toRob，而不是普通 STD issue-accept pass。
- 多子 uop 的 feedback 是否需要逐个处理。
- replay/fault 是否按整个 atomic transaction 处理，还是按子 uop 处理。

## 8. 建议实现步骤

### Step 1：先限制 AMO 生成范围

在未实现完整 atomic 多 uop 前，建议：

- 默认关闭 AMO/atomic 权重，或
- 只允许普通 1/1 AMO/LR/SC，禁止 AMOCAS_W/D/Q。

目标是避免当前不完整激励误进入 directed/random regression。

### Step 2：定义 atomic 子 uop 数据结构语义

明确：

- `uop_index` 表示 atomic 子 uop index，还是 issue port index。
- 是否需要单独字段表示 issue port index。
- STA 和 STD 的子 uop count 是否分别维护。
- 完成条件是“所有子 uop fire”还是“AtomicsUnit final writeback”。

### Step 3：实现 queue 展开和查重

修改：

- `route_target()`
- `push_issue_queue_item()`
- `issue_queue_contains()`
- `delete_issue_queue_entry()`

使同一 uid/target/replay_seq 可以有多个不同 `uop_index` 的 item。

### Step 4：实现 fire 计数和状态完成条件

修改：

- `mark_issue_fire()`
- `mark_issue_fire_already_accepted()`
- `status_transaction`

增加 per target atomic fire bitmask 或计数。只有所有子 uop fire 后，才置 `sta_dispatched/std_dispatched`。

### Step 5：定义 atomic writeback/replay 闭环

确认 RTL monitor 能否采集 AtomicsUnit final writeback，并将其转换成正确 `memblock_wb_event_t`。

需要明确：

- 普通 AMO/LR/SC 的 pass 条件。
- AMOCAS_W/D/Q 的 pass 条件。
- replay/fault/redirect 时如何清理多个子 uop。

## 9. 当前结论

当前测试框架已经具备 atomic 多 uop 的行为描述字段：

- `atomic_sta_uop_count`
- `atomic_data_uop_count`
- `uop_count`

但这些字段目前主要是记录信息，没有真正驱动多 uop 展开。

如果当前阶段不验证 atomic/CAS，该缺口短期可以接受，但应关闭或降低 AMO 权重并在计划中标注未完整支持。

如果后续要验证 AMO/CAS，尤其 AMOCAS_W/D/Q，则必须完善 issue queue 展开、子 uop index、fire 计数、状态完成条件和 atomic writeback/replay 闭环。
