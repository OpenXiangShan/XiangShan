# TLB Lookup 问题追溯与最终重构方案

## 0. 当前落地状态

2026-06-01 已按本方案完成首版代码落地：

- `common_data_transaction.sv` 的 live TLB 主存储改为 `tlb_entry_by_key[memblock_tlb_lookup_key_t]`。
- per-uid 追踪改为 `uid_tlb_record_by_uid[memblock_uid_t]`，uid 发射后预登记，L2TLB req 建表/命中后按 key 回填所有未完成 record。
- `memblock_l2tlb_base_sequence.sv` 主路径改为 `req -> get_or_create_tlb_entry_by_req(vpn,s2xlate) -> fill_dtlb_resp_from_entry() -> update_uid_tlb_records_by_entry()`。
- admission 不再提前按 uid 建 TLB entry，只把 uid 标记为可进入 issue queue；`sfence/hfence` 后续只应删除 live `tlb_entry_by_key`，不得清 `uid_tlb_record_by_uid`。

## 1. 问题背景

当前 mem_ut 的 TLB 建模存在两个根本问题：

1. TLB 映射是按 `uid` 建的，不是按真实 lookup key 建的。
2. CSR runtime 变化通过 `update_seq/csr_update_seq` 粗暴失效旧映射，导致 `A -> B -> A` 切回后无法复用原映射。

旧模型的核心问题是：

- `tlb_table_by_uid[]` 把一条 transaction 和一条 TLB entry 绑定成了一对一关系
- `uid_by_tlb_key[]` 只是辅助索引，不是真正主存储
- `csr_update_seq` 被用于控制 lookup 是否有效

这样会导致：

- 同一进程、同一 `vpn/s2xlate` 的多个请求重复建表
- 旧表项即使还在，也会因为 epoch 变化变得逻辑不可达
- `sfence/hfence` 无法按 entry 粒度做精确失效

## 2. 问题根因

### 2.1 `uid` 不是 TLB entry 的唯一性来源

真实决定一条 TLB entry 唯一性的不是 `uid`，而是：

- `vpn`
- `asid`
- `vmid`
- `s2xlate`

因此，同一进程上下文下，相同 `vpn/s2xlate` 应对应同一条 entry，而不是不同 `uid` 各建一份。

### 2.2 建表时机过早

旧方案是围绕 `uid` 或主表阶段建 TLB 表项。
但 `asid/vmid` 是 runtime CSR 决定的，应该在采到 RTL 的 DTLB -> L2TLB req 时，再结合实时 CSR 建 key 和建表。

### 2.3 epoch 机制过保守

旧方案通过：

- `update_seq` 变化时清索引
- `csr_update_seq` 不匹配时拒绝命中

实现了“全局粗暴失效”。
这会阻止 `A -> B -> A` 切回后复用原 entry，即使 key 已经恢复一致。

## 3. 最终重构目标

最终方案只保留一套新逻辑，不保留兼容层，不保留旧接口，不保留旧字段。

目标如下：

1. TLB 主存储改为 `by_key`
2. 建表时机改为 DTLB -> L2TLB req 到来时
3. 只保留一个 entry 类型文件
4. `uid` 只用于 transaction 跟踪与 key 绑定，不再决定 entry 唯一性
5. `sfence/hfence` 显式按 entry 失效
6. entry 被失效后，req 再来 miss 时自动重建

## 4. 唯一保留的 entry 类型

只保留一个文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_tlb_entry.sv`

不再保留：

- `tlb_transaction.sv`
- `tlb_transaction` 类

文件名和类名都直接表达最终语义：它是一条 TLB entry，不是一条 transaction。

### 4.1 `memblock_tlb_entry` 的字段要求

只保留新方案真正需要的字段：

```systemverilog
class memblock_tlb_entry extends uvm_object;

    memblock_tlb_lookup_key_t lookup_key;

    bit [63:0] vaddr;
    bit [63:0] paddr;
    bit [63:0] vpn;
    bit [63:0] ppn;

    bit pte_r;
    bit pte_w;
    bit pte_x;
    bit pte_u;
    bit pte_g;
    bit pte_a;
    bit pte_d;
    bit pte_n;
    bit pte_v;
    bit [1:0] pbmt;

    bit tlbAF;
    bit tlbPF;
    bit tlbGPF;
    bit pmaAF;

    int unsigned asid;
    int unsigned vmid;
    bit [1:0]    s2xlate;
    bit [1:0]    priv_mode;
    bit [1:0]    level;

    bit [2:0]    addr_low;
    bit [2:0]    ppn_low[8];
    bit          valididx[8];
    bit [2:0]    pteidx[8];

    longint unsigned create_cycle;
    longint unsigned last_hit_cycle;

    `uvm_object_utils(memblock_tlb_entry)

endclass
```

### 4.2 必须删除的旧字段

不再保留：

- `uid`
- `csr_update_seq`

理由：

- `uid` 属于 transaction 跟踪层，不属于 entry 本体
- `csr_update_seq` 不再参与命中和失效，不属于 entry 唯一性

### 4.3 允许保留的成员函数

只保留 entry 级函数：

- `reset()`
- `update_addr_fields()`
- `apply_csr_state()`
- `derive_index_fields()`
- `fixup_pte_legal()`

不再保留任何“从 uid transaction 拷贝”这一类旧语义函数。

## 5. 新的主存储模型

### 5.1 主表

TLB 主表改为：

```text
tlb_entry_by_key[key] = entry
```

建议定义在：

- `common_data_transaction.sv`

```systemverilog
memblock_tlb_entry tlb_entry_by_key[memblock_tlb_lookup_key_t];
```

### 5.2 `uid` 到 TLB 的追踪表

TLB 主表仍然按 key 唯一存储，不按 `uid` 建表。

为了后续上下文恢复、debug 和每条 transaction 的 PTE 追溯，需要额外维护一张
`uid -> TLB/PTE` 的追踪表。该表不是 TLB 命中表，也不是反向索引，不能用来从
TLB key 反推唯一 uid。

建议新增：

```systemverilog
memblock_uid_tlb_record uid_tlb_record_by_uid[memblock_uid_t];
```

`memblock_uid_tlb_record` 建议保存：

- `uid`
- `record_valid`：该 uid 是否已经登记追踪记录
- `pte_valid`：该 uid 对应的 PTE/TLB entry 是否已经回填完成
- `vpn`
- `s2xlate`
- `is_hypervisor_inst`
- `asid`
- `vmid`
- `lookup_key`
- `csr_snapshot`
- `pte` 或 `memblock_tlb_entry` 关键字段备份
- `issue_cycle`
- `pte_update_cycle`

维护原则：

- uid 追踪表按 uid 索引，只能 `uid -> record`
- 不建立 `key -> uid` 的强关联函数
- 同一个 TLB key 可以对应多个 uid record
- `sfence/hfence` 只影响 live TLB entry cache，不清除 uid 追踪记录
- uid 追踪记录只用于 debug、上下文恢复和结果追溯，不参与 L2TLB req 建表仲裁

### 5.3 必须删除的旧主存储

删除：

- `tlb_table_by_uid[]`
- `uid_by_tlb_key[]`
- `uid_to_tlb_key[]`
- `uid_has_tlb_key[]`

这些结构都属于旧模型，不再保留。

## 6. 建表时机改造

建表时机改为：

- 在采到 RTL 的 DTLB -> L2TLB req 时，再根据 req 和实时 CSR 建 key、查 entry、或新建 entry

这里要明确：

- 当前 `L2TLB_agent` 替代的是 L2TLB 对上游 DTLB 的 responder 功能
- 因此“req 到来时建表”是指：
  - DTLB -> L2TLB req 到来时建表
  - 不是 L2TLB -> PTW 下游请求到来时建表

### 6.1 `s2xlate` 与进程上下文选择

建表逻辑只以 DTLB -> L2TLB req 上的 `s2xlate` 作为当前请求的翻译阶段输入。
`make_tlb_key_by_req()` 不重新推导 `s2xlate`。这样做的原因是：当前 L2TLB
responder 的职责是响应已经到达 DTLB/L2TLB 边界的请求，建表必须和接口实际请求保持一致。

`asid/vmid` 是否进入 key，需要由 req 上的 `s2xlate` 决定：

- `noS2xlate`：使用 `satp.asid`，`vmid` 归零
- `onlyStage1`：使用 `vsatp.asid`，`vmid` 归零
- `onlyStage2`：使用 `hgatp.vmid`，`asid` 归零
- `allStage`：同时使用 `vsatp.asid` 与 `hgatp.vmid`

因此“进程切换”不需要单独维护一个 flag，而是体现在 key 中有效上下文字段变化上。
无效字段必须归零，避免同一语义的请求因为无效字段残留不同而生成不同 key。

### 6.2 建表主入口

只保留一个主入口函数：

```systemverilog
function bit get_or_create_tlb_entry_by_req(
    input bit [37:0] vpn,
    input bit [1:0]  s2xlate,
    output memblock_tlb_lookup_key_t key,
    output memblock_tlb_entry entry,
    output bit created
);
```

语义固定为：

1. 根据 req 的 `vpn/s2xlate` 和当前 `mmu_csr_state` 生成 key
2. 若 `tlb_entry_by_key` 命中：
   - 返回旧 entry
   - `created = 0`
3. 若 miss：
   - 创建新 entry
   - 插入 `tlb_entry_by_key`
   - `created = 1`

这就是新的 TLB entry 建表闭环入口。它不接收 `uid`，不更新 uid 追踪表。

建表完成后，L2TLB responder 需要另行调用 uid 追踪表回填函数，把本次 entry
按 key 回填到所有匹配的未激活 uid record 中。

### 6.3 `uid -> TLB/PTE` 追踪表维护流程

`uid -> TLB/PTE` 追踪表的建立和 live TLB entry 建表分离。

#### 1. uid 发射到流水线后预登记

当某个 uid 被发射到 load/STA/STD 流水线后，由发射路径调用预登记函数：

```systemverilog
function void register_uid_tlb_record_on_issue(input memblock_uid_t uid);
```

该函数需要：

1. 从 `main_table_by_uid[uid]` 读取 `vaddr/fuType/fuOpType`
2. 计算 `vpn = vaddr[49:12]`
3. 根据 opcode 判断该指令是否属于 hypervisor load/store 类型，写入 `is_hypervisor_inst`
4. 从 `common_data_transaction` 中的 runtime CSR 单例读取当前 `satp/vsatp/hgatp/priv` 快照
5. 结合 `is_hypervisor_inst`、runtime CSR 和最终请求会使用的翻译上下文，计算并记录：
   - `s2xlate`
   - `asid`
   - `vmid`
   - `lookup_key`
6. 保存完整 CSR 快照，作为该 uid 发射时的上下文备份
7. 设置 `record_valid=1`
8. 设置 `pte_valid=0`

这里的 `pte_valid=0` 表示该 uid 已经预登记，但还没有被 L2TLB req 建表结果回填。

#### 2. runtime CSR 统一来源

`satp/vsatp/hgatp/priv` 等 CSR 实时值会被多个 sequence/helper 使用，不能各自维护副本。

要求：

- `common_data_transaction` 持有唯一的 `mmu_csr_state`
- CSR monitor/adapter 负责更新 `common_data_transaction::mmu_csr_state`
- 其他 sequence/helper 通过 `common_data_transaction::get()` 获取单例
- 其他 sequence/helper 只读 CSR snapshot，不直接写 CSR runtime state

建议新增只读 snapshot 函数：

```systemverilog
function void get_mmu_csr_snapshot(output mmu_csr_runtime_state snapshot);
```

如果使用 class object 保存 snapshot，必须做字段拷贝，不能直接把单例 handle 暴露给调用方修改。

#### 3. L2TLB req 建表后回填 uid 追踪记录

L2TLB responder 采到 req 后仍按 live TLB 主路径建表：

```text
req.vpn/req.s2xlate -> get_or_create_tlb_entry_by_req() -> tlb_entry_by_key[key]
```

建表或命中 entry 后，调用回填函数：

```systemverilog
function int unsigned update_uid_tlb_records_by_entry(
    input memblock_tlb_lookup_key_t key,
    input memblock_tlb_entry entry
);
```

该函数匹配条件：

- `record_valid == 1`
- `pte_valid == 0`
- `record.vpn == key.vpn`
- `record.s2xlate == key.s2xlate`
- `record.asid == key.asid`
- `record.vmid == key.vmid`

匹配成功后，对所有匹配的未激活 record 执行：

- 写入 `lookup_key`
- 备份 entry/PTE 关键字段
- 设置 `pte_valid=1`
- 记录 `pte_update_cycle`

如果没有任何未激活 record 匹配到本次 key，应报错。因为当前测试框架期望：
进入 L2TLB 的请求都来自已经发射到流水线的 uid，理论上在发射时已经预登记了
`uid -> TLB/PTE` 追踪记录。

如果匹配到多个 uid record，不报错，应该全部回填同一份 entry/PTE。原因是多个 uid
访问同一 `{vpn, asid, vmid, s2xlate}` 时共享同一条 TLB entry 是合法行为。

#### 4. fence/flush 对 uid 追踪记录的影响

`sfence/hfence` 会失效 live TLB cache：

```text
tlb_entry_by_key.delete(key)
```

但不清除 `uid_tlb_record_by_uid[]` 中已经完成的追踪记录。

原因：

- live TLB entry 表示当前可命中的翻译缓存
- uid 追踪记录表示该 uid 曾经使用过的 PTE/context 备份
- 追踪记录用于后续上下文恢复和 debug，不应该被 fence/flush 抹掉历史信息

如果后续需要标记“该 uid 记录对应的 live entry 已被 fence 失效”，可额外增加
debug 字段，但首版不需要用 fence 修改 `pte_valid`。

## 7. `common_data_transaction.sv` 修改要求

文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`

### 7.1 保留的成员

保留：

- `mmu_csr_state`
- `main_table_by_uid[]`
- `status_by_uid[]`

说明：

- `mmu_csr_state` 继续作为 runtime CSR 镜像
- `main_table_by_uid[]` 继续作为 transaction 主表
- `status_by_uid[]` 继续作为状态表

### 7.2 删除的成员

删除：

- `tlb_table_by_uid[]`
- `uid_by_tlb_key[]`

### 7.3 新增的成员

新增：

```systemverilog
memblock_tlb_entry tlb_entry_by_key[memblock_tlb_lookup_key_t];
memblock_uid_tlb_record uid_tlb_record_by_uid[memblock_uid_t];
```

### 7.4 必须新增的函数

新增：

1. `make_tlb_key_by_req(vpn, s2xlate)`
2. `has_tlb_entry(key)`
3. `get_tlb_entry(key)`
4. `insert_tlb_entry(key, entry)`
5. `get_or_create_tlb_entry_by_req(...)`
6. `register_uid_tlb_record_on_issue(uid)`
7. `update_uid_tlb_record_context(uid, vpn, s2xlate, is_hypervisor_inst, csr_snapshot)`
8. `update_uid_tlb_records_by_entry(key, entry)`
9. `get_uid_tlb_record(uid)`
10. `get_mmu_csr_snapshot(snapshot)`
11. `decode_raw_sfence()` / `apply_sfence_invalidate()` / `apply_raw_sfence()`

### 7.5 必须删除的旧函数

删除：

- `set_tlb_transaction()`
- `get_tlb_transaction()`
- `register_tlb_lookup()`
- `lookup_tlb_uid()`
- `lookup_tlb_uid_by_req()`
- `clear_tlb_lookup_index()`
- `bind_uid_to_tlb_key()`
- `clear_uid_tlb_binding()`

这些函数都绑定旧模型，不再保留。

### 7.6 `apply_raw_csr_runtime()` 的最终要求

保留：

- `mmu_csr_state.update_from_raw_csr(raw)`

删除：

- 任何 `update_seq` 变化后清索引的逻辑

要求：

- `update_seq` 只允许作为 debug 计数
- 不允许再参与 entry 命中和失效控制

## 8. `tlb_map_builder.sv` 修改要求

文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/tlb_map_builder.sv`

### 8.1 只保留一个 builder 入口

只保留：

```systemverilog
function memblock_tlb_entry build_tlb_entry_for_req(
    input bit [37:0] vpn,
    input bit [1:0]  s2xlate
);
```
### 8.2 实现要求

内部流程：

1. 新建 `memblock_tlb_entry`
2. 按 `vpn` 构造基础地址，页内 offset 置 0
3. 调用：
   - `choose_paddr()`
   - `randomize_pte_bits()`
   - `fixup_pte_legal()`
   - `apply_csr_state()`
4. 填好 `lookup_key`
5. 返回 entry

### 8.3 必须删除的旧入口

删除：

- `build_tlb_for_uid()`

## 9. `memblock_l2tlb_base_sequence.sv` 修改要求

文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`

### 9.1 新主路径

主路径固定改为：

```text
req -> key -> get_or_create_tlb_entry_by_req -> entry -> resp
```

### 9.2 必须执行的步骤

1. 采到 req
2. 提取：
   - `vpn`
   - `s2xlate`
3. 调 `get_or_create_tlb_entry_by_req(...)`
4. 用返回的 `entry` 直接填 response
5. 调 `update_uid_tlb_records_by_entry(key, entry)` 回填所有匹配的未激活 uid 追踪记录

### 9.3 必须新增的 response 填充 helper

新增：

```systemverilog
function void fill_dtlb_resp_from_entry(
    input memblock_tlb_entry entry,
    ref l2tlb_resp_xaction resp
);
```

要填的字段包括：

- `paddr/ppn`
- `pte perm`
- `pbmt`
- `pf/af/gpf`
- `level`
- `addr_low/ppn_low/valididx/pteidx`

## 10. `sfence/hfence` 失效方案

### 10.1 为什么必须显式建模

重构后 entry 是按 key 存的，旧的“CSR epoch 变化间接失效”方式已经不成立。
因此 `sfence/hfence` 必须显式遍历 `tlb_entry_by_key`，按 RTL/Scala 语义失效 entry。

### 10.2 相关接口字段

必须考虑：

```text
io_ooo_to_mem_sfence_valid
io_ooo_to_mem_sfence_bits_rs1
io_ooo_to_mem_sfence_bits_rs2
io_ooo_to_mem_sfence_bits_addr
io_ooo_to_mem_sfence_bits_id
io_ooo_to_mem_sfence_bits_hv
io_ooo_to_mem_sfence_bits_hg
```

结合源码语义：

- `hv=1`：`hfence_v`
- `hg=1`：`hfence_g`
- `rs1`：忽略地址
- `rs2`：忽略 ID

### 10.3 已实现的内部 payload

已在 `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv` 中新增：

```systemverilog
typedef struct {
    bit               valid;
    bit               ignore_addr;
    bit               ignore_id;
    bit [49:0]        addr;
    bit [15:0]        id;
    bit               hv;
    bit               hg;
    longint unsigned  cycle;
} memblock_sfence_payload_t;
```

并约定：

- `ignore_addr = rs1`
- `ignore_id = rs2`

### 10.4 已实现的函数

已在 `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv` 中实现：

1. `decode_raw_sfence()`
2. `sfence_vpn_match()`
3. `sfence_match_entry()`
4. `apply_sfence_invalidate()`
5. `apply_raw_sfence()`

### 10.5 匹配语义要求

#### 普通 `sfence`

- 按 ASID 语义
- `ignore_id=0` 时检查 ASID
- 若 `entry.pte_g=1`，则按 ASID 精确 flush 时不应命中该 entry
- `ignore_addr=0` 时按地址匹配

#### `hfence_v`

- 按 VMID + 虚拟 ASID
- `g` 位只影响 ASID 相关判断，不影响 VMID 相关判断

#### `hfence_g`

- 按 VMID
- 忽略 ASID
- `ignore_id=1` 时忽略 VMID，扩大 flush 范围

### 10.6 `g` 位处理原则

必须保留：

- 按 ASID 精确 flush 时，`g=1` 的 entry 不能被误删

但不能错误保留：

- 全局 flush
- 按 VMID flush

### 10.7 地址匹配要求

`sfence_vpn_match()` 不能只比较完整 VPN，必须按 `entry.level` 做 tag 级匹配。

如果首版只支持 4KB 页面，必须在代码和文档里明确说明：

- 当前只完整支持 `level=0`
- 大页匹配后续补齐

### 10.8 实际失效动作

`apply_sfence_invalidate()` 的动作固定为：

1. 遍历 `tlb_entry_by_key`
2. `sfence_match_entry()` 判断是否命中
3. 命中的 entry：
   - `tlb_entry_by_key.delete(key)`

注意：

- 这里只删除 TLB entry cache
- 不删除 `main_table_by_uid[]`
- 不删除 `uid_tlb_record_by_uid[]`
- 不直接改 transaction 业务状态

## 11. `sfence` 后 req miss 重建闭环

这是最终方案必须保证的行为。

entry 被 `sfence/hfence` 删掉后，req 再来时必须：

1. 重新形成 key
2. 查 `tlb_entry_by_key`
3. 若 miss，则自动创建新 entry
4. 用新 entry 正常回包

因此主入口必须是：

```text
get_or_create_tlb_entry_by_req(...)
```

不能是只查不建的 lookup 函数。

建议代码里显式记录：

- `hit_existing_entry`
- `created_new_entry`

用于日志、coverage 和后续 directed 验证。

## 12. `seq_pkg.sv` / `seq.f` / service loop 修改要求与当前实现

### 12.1 filelist 接入

必须同步接入：

- `memblock_tlb_entry.sv`

当前未单独新增 `memblock_sfence_handler.sv`。sfence/hfence 的失效逻辑集中放在
`common_data_transaction.sv`，原因是它只操作公共数据 owner 内的 live
`tlb_entry_by_key`，不需要额外 handler 持有状态。

### 12.2 service loop 接入

当前实现路径：

1. fence monitor 采 raw sfence
2. 放入公共 raw queue
3. `memblock_dispatch_base_sequence::collect_runtime_context_events()` 在统一 service loop 中先调用 `dispatch_monitor_event_adapter::drain_csr_events()` 同步 CSR runtime
4. 同一入口随后显式调用 `dispatch_monitor_event_adapter::drain_sfence_events()` 消费 sfence queue
5. `drain_sfence_events()` 调 `common_data_transaction::apply_raw_sfence()`
6. `apply_raw_sfence()` decode 成 `memblock_sfence_payload_t` 后调用 `apply_sfence_invalidate()`

保持与 writeback/redirect 一致的统一服务循环风格。

## 13. 参数管理与字段约束补充

新方案下，TLB entry 已经从“按 uid 的 transaction 衍生对象”变成“按 key 唯一的共享 entry 对象”，因此参数管理和字段约束也必须同步收敛，不能继续沿用旧的 transaction 级散落写法。

### 13.1 参数归属要求

#### 1. 测试框架级参数

继续统一走：

```text
env/plus.sv -> seq_csr_common.sv -> getter
```

适用于：

- `paddr_base`
- `paddr_range`
- PTE 各权限位权重
- `pbmt` 权重
- `tlbAF/tlbPF/tlbGPF/pmaAF` 权重
- 是否允许大页
- `sfence/hfence` directed 开关与权重

这些参数不允许散落写在 `memblock_l2tlb_base_sequence.sv`、`common_data_transaction.sv` 或 builder 局部常量里。

#### 2. testcase 个性化参数

仍通过：

```text
seq/plus_cfg/<cfg>.cfg
```

由 Makefile `cfg=<cfg>` 指定。

适用于：

- 特定 testcase 想固定某一类 `paddr` 分布
- 特定 testcase 想拉高 `pte_g/pte_u/pte_x` 权重
- 特定 testcase 想强制只用 4KB 页或打开大页
- 特定 testcase 想高频注入 `sfence/hfence`

#### 3. 环境组件参数

若后续新增 L2TLB responder、自定义 fence monitor、monitor service loop 的开关，仍应放到：

- `memblock_env_cfg`

不应混入 `seq_csr_common`。

### 13.2 `memblock_tlb_entry` 字段约束要求

#### 1. `lookup_key`

必须由实时 req + 实时 CSR 决定，不允许随机：

- `vpn` 来自 req
- `s2xlate` 来自 req
- `asid/vmid` 来自 `mmu_csr_state`

`asid/vmid` 的有效性由 req 的 `s2xlate` 决定：

- `noS2xlate` 使用 `satp.asid`
- `onlyStage1` 使用 `vsatp.asid`
- `onlyStage2` 使用 `hgatp.vmid`
- `allStage` 同时使用 `vsatp.asid/hgatp.vmid`

未参与当前 `s2xlate` 模式的字段必须归零。

#### 2. `paddr`

`paddr` 不应完全无约束随机，必须保持可控映射行为。

建议继续保留 builder 中的：

```systemverilog
choose_paddr(vaddr)
```

并明确它的约束来源：

- `seq_csr_common::get_paddr_base()`
- `seq_csr_common::get_paddr_range()`

建议行为：

1. `paddr` 与 `vaddr` 保持页内 offset 一致
   即：
   - `paddr[11:0] == vaddr[11:0]`

2. 页号在给定范围内受控分布
   即：
   - 通过 `paddr_base/paddr_range` 控制物理页落点

3. 若 testcase 需要提高地址别名或冲突概率
   应通过 plus 参数控制 `paddr_range` 缩小，不能在 builder 中写死特殊分布

#### 3. `ppn/vpn`

- `vpn = vaddr[63:12]`
- `ppn = paddr[63:12]`

这两个字段必须由地址字段推导，不允许独立随机。

#### 4. PTE 权限位

以下字段允许按权重随机，但必须经过合法性修正：

- `pte_r`
- `pte_w`
- `pte_x`
- `pte_u`
- `pte_g`
- `pte_a`
- `pte_d`
- `pte_n`
- `pte_v`

要求：

- 权重来源统一由 `seq_csr_common` getter 提供
- 合法性修正在 `fixup_pte_legal()` 中完成
- 不允许完全无约束随机

#### 5. 异常位

以下字段可由 testcase 权重控制：

- `tlbAF`
- `tlbPF`
- `tlbGPF`
- `pmaAF`

但要求：

- 其行为必须和 response 组包逻辑一致
- 若异常位置高，对应 response 中的权限/地址字段不能再给出自相矛盾语义

#### 6. `pbmt`

`pbmt` 不应写死，也不应完全自由随机。

要求：

- 通过 plus/`seq_csr_common` 权重控制
- 与 testcase 目标一致时可以定向指定

#### 7. `level`

当前若首版仅支持 4KB page，则必须明确：

- `level` 固定或强约束为 0
- 大页相关逻辑先不生成或低权重禁用

若后续要支持大页：

- `level` 的随机权重也必须通过 plus 控制
- `sfence_vpn_match()` 必须同步支持对应匹配规则

### 13.3 builder 行为约束要求

`build_tlb_entry_for_req()` 不能只是“凑字段”，它必须满足：

1. req 决定 `vpn/s2xlate`
2. runtime CSR 决定 `asid/vmid`
3. `choose_paddr()` 决定受控 `paddr`
4. `update_addr_fields()` 推导 `vpn/ppn/addr_low/...`
5. `randomize_pte_bits()` 只负责权重随机
6. `fixup_pte_legal()` 负责合法性修正

因此 builder 中不应出现：

- 直接写死 `paddr`
- 独立随机 `ppn`
- 独立随机 `lookup_key`
- 将 testcase 特定行为写死在 builder 中

### 13.4 `sfence/hfence` 参数化要求

如果后续测试中需要主动构造 `sfence/hfence` 行为，建议参数也统一管理：

- `sfence_enable`
- `sfence_global_wt`
- `sfence_asid_wt`
- `sfence_addr_wt`
- `hfence_v_enable`
- `hfence_g_enable`

这些参数应进入：

- `env/plus.sv`
- `seq_csr_common.sv`
- `seq/plus_cfg/default.cfg`

## 14. 旧调用点适配要求

本方案完成后，不只是新增新类和新函数，所有旧方案调用点都必须同步适配修改，不能保留旧调用链。

### 14.1 必须整体替换的旧调用路径

旧路径：

```text
uid -> tlb_table_by_uid[uid]
req -> lookup_tlb_uid_by_req -> uid -> get_tlb_transaction(uid)
uid -> register_tlb_lookup()
```

必须统一替换为：

```text
req -> key -> get_or_create_tlb_entry_by_req -> entry
uid issue -> uid_tlb_record_by_uid[uid] 预登记
entry -> update_uid_tlb_records_by_entry(key, entry) -> 回填匹配的 uid record
```

### 14.2 必须同步检查的文件

完成重构后，必须全局搜索并修改所有旧调用点，至少包括：

- `common_data_transaction.sv`
- `tlb_map_builder.sv`
- `memblock_l2tlb_base_sequence.sv`
- `memblock_dispatch_base_sequence.sv`
- 任何引用 `get_tlb_transaction()` / `set_tlb_transaction()` / `lookup_tlb_uid_by_req()` / `register_tlb_lookup()` 的 sequence 或 helper
- 相关设计文档和流程文档

建议执行以下全局检查：

```bash
rg -n "tlb_table_by_uid|uid_by_tlb_key|get_tlb_transaction|set_tlb_transaction|lookup_tlb_uid_by_req|lookup_tlb_uid\\(|register_tlb_lookup|build_tlb_for_uid|csr_update_seq" mem_ut AI_DOC
```

所有残留调用都必须处理，不能保留“先能跑再说”的旧路径。

### 14.3 文档同步要求

方案落地后，以下文档也必须同步适配：

- 测试框架设计文档中关于 TLB 表的章节
- L2TLB responder 方案文档
- 参数管理说明文档中 TLB 相关参数部分
- 任何仍写着 `tlb_transaction` / `tlb_table_by_uid[]` / `uid_by_tlb_key[]` 的说明

## 15. 建议实施顺序

1. 新增 `memblock_tlb_entry.sv`
2. 在 `common_data_transaction.sv` 中加入：
   - `tlb_entry_by_key`
   - `uid_tlb_record_by_uid`
3. 删除旧成员：
   - `tlb_table_by_uid[]`
   - `uid_by_tlb_key[]`
   - `uid_to_tlb_key[]`
   - `uid_has_tlb_key[]`
4. 新增：
   - `make_tlb_key_by_req()`
   - `has_tlb_entry()`
   - `get_tlb_entry()`
   - `insert_tlb_entry()`
   - `get_or_create_tlb_entry_by_req()`
   - `register_uid_tlb_record_on_issue()`
   - `update_uid_tlb_records_by_entry()`
   - `get_uid_tlb_record()`
5. 删除旧函数：
   - `set_tlb_transaction()`
   - `get_tlb_transaction()`
   - `register_tlb_lookup()`
   - `lookup_tlb_uid()`
   - `lookup_tlb_uid_by_req()`
   - `clear_tlb_lookup_index()`
   - `bind_uid_to_tlb_key()`
   - `clear_uid_tlb_binding()`
6. 修改 `tlb_map_builder.sv`
   - 删除 `build_tlb_for_uid()`
   - 只保留 `build_tlb_entry_for_req()`
7. 修改 `memblock_l2tlb_base_sequence.sv`
   - 主路径改成 `req -> key -> entry -> resp`
   - response 后调用 `update_uid_tlb_records_by_entry(key, entry)`
8. 接入 `sfence/hfence` invalidate
9. 验证：
   - 同 key 复用
   - A -> B -> A 复用
   - `sfence` 后 miss
   - miss 后自动重建
   - `g` 位不被按 ASID 误删

## 16. 方案落地后的问题解决情况

按本最终方案落地后，原问题会被一起解决：

1. `A -> B -> A`
   - 由 key 唯一性自然支持，不再依赖 epoch 粗暴失效

2. 同进程同 `vpn` 重复建表
   - 不再按 uid 重复建表，而是同 key 复用同一 entry

3. `sfence/hfence` 失效
   - 显式按 entry 精确失效

4. `sfence` 后再次请求
   - miss 后自动重建新 entry，闭环完整

## 17. 结论

最终方案只保留一套新逻辑：

- 只保留一个 entry 类型文件：`memblock_tlb_entry.sv`
- TLB 主表只保留：`tlb_entry_by_key`
- `uid` 只保留为 transaction 到 key 的绑定关系
- 建表时机只保留：DTLB -> L2TLB req 到来时
- `sfence/hfence` 只保留显式 entry 级失效
- req miss 后只保留自动重建逻辑

不再保留：

- `tlb_transaction`
- `tlb_table_by_uid[]`
- `uid_by_tlb_key[]`
- `csr_update_seq` 命中控制
- 任何兼容层或旧接口

这样方案才真正和当前最新目标一致，没有冗余逻辑、冗余字段和旧模型残留。

## 18. 实施 Checklist

### 18.1 类型与文件

- 新增 `memblock_tlb_entry.sv`
- 删除 `tlb_transaction.sv`
- 更新 `seq_pkg.sv`
- 更新 `seq.f`

### 18.2 `memblock_tlb_entry` 字段

- 只保留：`lookup_key/vaddr/paddr/vpn/ppn`
- 只保留：`pte_r/w/x/u/g/a/d/n/v`
- 只保留：`pbmt/tlbAF/tlbPF/tlbGPF/pmaAF`
- 只保留：`asid/vmid/s2xlate/priv_mode/level`
- 只保留：`addr_low/ppn_low/valididx/pteidx`
- 只保留：`create_cycle/last_hit_cycle`
- 删除：`uid`
- 删除：`csr_update_seq`

### 18.3 `common_data_transaction.sv`

- 新增 `tlb_entry_by_key[memblock_tlb_lookup_key_t]`
- 新增 `uid_tlb_record_by_uid[memblock_uid_t]`
- 删除 `tlb_table_by_uid[]`
- 删除 `uid_by_tlb_key[]`
- 删除 `uid_to_tlb_key[]`
- 删除 `uid_has_tlb_key[]`
- 新增 `make_tlb_key_by_req()`
- 新增 `has_tlb_entry()`
- 新增 `get_tlb_entry()`
- 新增 `insert_tlb_entry()`
- 新增 `get_or_create_tlb_entry_by_req()`
- 新增 `register_uid_tlb_record_on_issue()`
- 新增 `update_uid_tlb_record_context()`
- 新增 `update_uid_tlb_records_by_entry()`
- 新增 `get_uid_tlb_record()`
- 新增 `get_mmu_csr_snapshot()`
- 新增 `decode_raw_sfence()` / `apply_sfence_invalidate()` / `apply_raw_sfence()`
- 删除 `set_tlb_transaction()`
- 删除 `get_tlb_transaction()`
- 删除 `register_tlb_lookup()`
- 删除 `lookup_tlb_uid()`
- 删除 `lookup_tlb_uid_by_req()`
- 删除 `clear_tlb_lookup_index()`
- 删除 `bind_uid_to_tlb_key()`
- 删除 `clear_uid_tlb_binding()`
- 修改 `apply_raw_csr_runtime()`：不再因 `update_seq` 清索引

### 18.4 `tlb_map_builder.sv`

- 新增唯一入口 `build_tlb_entry_for_req(vpn, s2xlate)`
- 保留 `choose_paddr()`
- 保留 `randomize_pte_bits()`
- 保留 `fixup_pte_legal()`
- 修改 `apply_csr_state()`：不再写 `csr_update_seq`
- 删除 `build_tlb_for_uid()`

### 18.5 `memblock_l2tlb_base_sequence.sv`

- 主路径改为 `req -> key -> get_or_create_tlb_entry_by_req -> entry -> resp`
- 提取 req 的 `vpn/s2xlate`
- response 后调用 `update_uid_tlb_records_by_entry(key, entry)` 回填 uid 追踪记录
- 新增 `fill_dtlb_resp_from_entry()`
- 删除一切 `req -> uid -> tlb_table_by_uid` 旧路径

### 18.6 `sfence/hfence`

- 在 `memblock_dispatch_types.sv` 新增 `memblock_sfence_payload_t`
- 新增 `decode_raw_sfence()`
- 新增 `sfence_vpn_match()`
- 新增 `sfence_match_entry()`
- 新增 `apply_sfence_invalidate()`
- 明确支持：
  - 普通 `sfence`
  - `hfence_v`
  - `hfence_g`
  - `g` 位按 ASID flush 不误删
  - 按 `level` 做地址匹配

### 18.7 参数管理

- `paddr_base/paddr_range` 统一走 `plus.sv -> seq_csr_common.sv`
- PTE 位权重统一走 `seq_csr_common`
- `pbmt` 权重统一走 `seq_csr_common`
- `tlbAF/tlbPF/tlbGPF/pmaAF` 权重统一走 `seq_csr_common`
- `sfence/hfence` testcase 参数同步加到 `plus.sv/default.cfg/plus_cfg`

### 18.8 约束行为

- `lookup_key` 不随机，只能由 req + runtime CSR 生成
- `lookup_key.s2xlate` 直接来自 req，不由测试框架重新推导
- `lookup_key.asid/vmid` 按 req 的 `s2xlate` 选择有效字段，无效字段归零
- `paddr[11:0] == vaddr[11:0]`
- `vpn = vaddr[63:12]`
- `ppn = paddr[63:12]`
- PTE 位只能“带权随机 + fixup 修正”
- `level` 若首版只支持 4KB，固定或强约束为 0

### 18.9 旧调用点清理

- 全局清理 `tlb_table_by_uid`
- 全局清理 `uid_by_tlb_key`
- 全局清理 `get_tlb_transaction`
- 全局清理 `set_tlb_transaction`
- 全局清理 `lookup_tlb_uid_by_req`
- 全局清理 `lookup_tlb_uid(`
- 全局清理 `register_tlb_lookup`
- 全局清理 `build_tlb_for_uid`
- 全局清理 `csr_update_seq` 参与命中判断

### 18.10 推荐全局检查

```bash
rg -n "tlb_table_by_uid|uid_by_tlb_key|get_tlb_transaction|set_tlb_transaction|lookup_tlb_uid_by_req|lookup_tlb_uid\\(|register_tlb_lookup|build_tlb_for_uid|csr_update_seq" mem_ut AI_DOC
```

### 18.11 验证项

- 同 key 多次 req 只建一条 entry
- `A -> B -> A` 切回后复用旧 entry
- `sfence` 后旧 entry 被删
- `sfence` 后 req 再来 miss
- miss 后自动重建新 entry
- 指定 ASID flush 时 `g=1` entry 不误删
- 指定 VMID / addr flush 行为正确

### 18.12 当前实现与验证状态

本轮已完成 `sfence/hfence` 对 `tlb_entry_by_key` 的 entry 级失效代码实现：

- `fence_agent_agent_monitor.sv` 采集 `io_ooo_to_mem_sfence_*`，生成 raw sfence event。
- `memblock_sync_pkg.sv` 新增 `dispatch_raw_sfence_t` 和 `raw_sfence_q`。
- `dispatch_monitor_event_adapter.sv` 在同步 runtime CSR 后 drain raw sfence queue。
- `common_data_transaction.sv` 新增 `decode_raw_sfence()`、`sfence_vpn_match()`、`sfence_match_entry()`、`apply_sfence_invalidate()`、`apply_raw_sfence()`。
- `apply_sfence_invalidate()` 只删除命中的 live `tlb_entry_by_key` entry，不删除 `main_table_by_uid[]`、`status_by_uid[]` 或 `uid_tlb_record_by_uid[]`。

本轮已完成基础验证：

```bash
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

结果：

- 编译通过。
- `tc_sanity/base_fun` 仿真通过，日志显示 `TEST CASE PASSED`。
- `UVM_ERROR=0`，`UVM_FATAL=0`。

当前验证只证明该实现不破坏基础编译和 sanity flow；`sfence/hfence` directed 行为仍需后续补 testcase 覆盖，包括指定 ASID、指定 VMID、指定地址、`g=1` entry 保护和失效后 miss 自动重建。
