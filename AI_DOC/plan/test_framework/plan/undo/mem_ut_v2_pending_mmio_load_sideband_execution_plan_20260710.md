# mem_ut V2 MMIO ROB tag 与 `pendingMMIOld` sideband 最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 主要入口 | `io_mem_to_ooo_ctrl_agent_agent_monitor::mon_data()`、`common_data_transaction` MMIO tag API、LSQ owner 的 `pendingPtr/pendingMMIOld` builder |
| 适配原则 | 本 plan 只拥有 MMIO raw producer、active uid MMIO tag 和 `uid_is_mmio_load()` query；不复制 LSQ owner 的 commit/deq、modeled head、fault rebase 或 driver hold 主流程 |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只整理 V2 MMIO ROB tag 与 `pendingMMIOld` sideband 支持需要解决的问题。每个问题均说明 V2 问题、修改原因、最终方案、修改的原有逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- DUT `loadMmio/loadMmioUop`、`storeMmio/storeMmioUop` output 到 raw ctrl 的唯一 producer。
- V2 扁平 MMIO 物理端口到参数化 packed mirror 的 `sample_mmio_outputs()` profile accessor。
- 可选 SQ deq pointer presence 到 `sq_deq_ptr_valid` 的 `sample_sq_deq_ptr()` profile accessor。
- raw MMIO output 按 ROB value、raw sample epoch 和 active instance provenance 归一化到唯一 active uid。
- `status_transaction` 中 runtime ROB MMIO tag 字段、canonical setter/clear/query API。
- `uid_is_mmio_load()` / `uid_is_mmio_store()` 只读 query，供 LSQ MMIO/status owner 构造 `pendingMMIOld` sideband。
- `memblock_op_behavior_util.sv` 作为唯一 LOAD/STORE/AMO/CBO 行为矩阵真源。
- `basicTest + memblock_pending_mmio_directed_vseq` software-only directed 验证入口。

本轮不支持：

- MMIO response checker、RM、coverage 或 MMIO 地址/属性生成策略。
- store MMIO tag 直接改变 pass/fail/terminal。
- 本 plan 内实现 `pendingPtr/pendingst/scommit`、modeled ROB head、normal commit、fault convergence、V2 SQ count-only deq 或 LSQ commit/deq driver hold。
- 在 ctrl raw producer 中推进 SQ/LSQ 状态、active SQ map、commit/deq pointer 或 terminal。
- 在 directed soft test 中直接写 `status_transaction`、复制 IQ/LSQ 主流程或绕过 owner 合同。

主要落点：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_op_behavior_util.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/status_transaction.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv
mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv
mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_pending_mmio_directed_vseq.sv
mem_ut/ver/ut/memblock/seq/seq_pkg.sv
mem_ut/ver/ut/memblock/seq/seq.f
```

执行前必须确认当前 V2 RTL 权威输入存在：

```bash
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

## 2. 问题一：MMIO raw producer 与 SQ pointer presence 职责混在一起

### V2 问题

V2 ctrl interface 暴露的是扁平 `loadMmio_0/1/2`、`loadMmioUop_0/1/2`、`storeMmio` 和 `storeMmioUop`，不是可循环索引的 interface 数组。同时 V2 没有 SQ deq pointer 成员，monitor 若用普通运行期 `if` 引用可选成员，编译期仍会解析不存在的端口。

### 修改原因

raw ctrl producer 是 MMIO output、SQ deq count 和可选 SQ pointer 的唯一 interface 事实来源。它必须只负责采样与 raw 入队，不能顺手推进 LSQ/SQ 状态；profile 差异也必须在 interface accessor 内隔离，不能散落在 monitor 主流程里。

### 修改方案与修改逻辑

`dispatch_raw_ctrl_t` 新增 MMIO mirror 和 pointer valid：

```systemverilog
bit [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0] load_mmio_valid;
bit [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0][`MEMBLOCK_DUT_ROB_VALUE_W-1:0] load_mmio_rob_value;
bit store_mmio_valid;
bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] store_mmio_rob_value;
int unsigned mmio_flush_epoch;
bit sq_deq_ptr_valid;
```

`make_empty_raw_ctrl()` 显式清零新增字段、`sq_deq_ptr_valid` 和既有 pointer payload。

在 `io_mem_to_ooo_ctrl_agent_agent_interface.sv` 中新增两个职责正交的 profile accessor：

- `sample_mmio_outputs()` 只把当前 profile 的真实 MMIO 扁平端口映射到 packed mirror。V2 物理 load port 数固定为 3，并在 elaboration 期检查 `MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM` 一致。
- `sample_sq_deq_ptr(sampled_sq_deq, valid, flag, value)` 只隔离可选 SQ pointer presence。V2 函数体清零输出后返回，不声明、不引用不存在的 pointer 成员；有 pointer 的版本仅在 `sampled_sq_deq!=0` 时读取真实 pointer。

`io_mem_to_ooo_ctrl_agent_agent_monitor::mon_data()` 只消费 accessor 返回值：valid 为 1 时才检查对应 ROB value 的 X/Z 并写 raw；任一 MMIO valid、deq、memoryViolation 或 flushSb waiting 成立时才创建 raw 并 push。monitor 不调用 `apply_raw_ctrl_deq()`，不查询 active SQ map，不推进 status、commit/deq 或 terminal。

### 文字伪代码

```text
ctrl monitor 每拍：
  调用 vif.sample_mmio_outputs(load_valid, load_value, store_valid, store_value)；
  accessor 先清空 mirror，再显式映射 V2 loadMmio_0/1/2 和 storeMmio；
  accessor 不做 X/Z、raw push、uid 解析或状态推进；

  采样 sqDeq count；
  调用 vif.sample_sq_deq_ptr(sampled_sq_deq, ptr_valid, ptr_flag, ptr_value)；
  V2 accessor 返回 valid=0/flag=0/value=0，且不引用 pointer 成员；
  monitor 检查 capability 与 ptr_valid 是否一致；
  ptr_valid=1 时才检查 pointer payload X/Z；

  遍历 packed load mirror，循环范围为 MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM；
  对每个 port 只在 load_valid[port]=1 时检查并复制 load_value[port]；
  只在 store_valid=1 时检查并复制 store_value；

  如果 deq、memoryViolation、flushSb waiting 或任一 MMIO valid 成立：
    raw = make_empty_raw_ctrl()；
    复制 ptr_valid 和 pointer payload；
    复制 MMIO valid/value；
    任一 MMIO valid 时 raw.mmio_flush_epoch = 当前 dispatch_flush_epoch；
    raw.valid=1，写入当前 cycle；
    push_raw_ctrl(raw)；
  否则不创建 raw；

  producer 到此结束，不修改 SQ/LSQ map、status、pointer 或 terminal。
```

## 3. 问题二：MMIO output 只有 ROB value，需要归一化到唯一 active uid

### V2 问题

DUT MMIO output 给出的是 ROB value。测试框架 active ROB map 使用完整 ROB key，且 redirect 后同一 ROB value 可能出现旧 raw 与新 active instance 重叠。如果只按 value 或全表扫描 uid 反查，容易把 stale raw 写到错误 uid。

### 修改原因

MMIO tag 必须绑定到“当前动态实例”。raw sample epoch 只能说明 monitor 采样拍的版本，不能单独证明该事实仍然属于当前 instance；必须结合 active map 和 activation provenance 判定 current、可证明 stale drop 或 fatal 歧义。

### 修改方案与修改逻辑

`status_transaction` 新增 active provenance 与 MMIO tag：

```systemverilog
bit active_instance_flush_epoch_valid;
int unsigned active_instance_flush_epoch;
bit mmio_tag_valid;
bit is_mmio_load;
bit is_mmio_store;
memblock_mmio_tag_source_e mmio_tag_source;
int unsigned mmio_tag_dynamic_epoch;
```

不再维护重复的 `is_mmio`。统一 query 语义为：

```text
uid_is_mmio_load  = mmio_tag_valid && is_mmio_load && !is_mmio_store
uid_is_mmio_store = mmio_tag_valid && is_mmio_store && !is_mmio_load
```

`active_instance_flush_epoch{_valid}` 的唯一运行期写者是 `activate_uid()` 和 `clear_uid_dispatch_result()`；`status_transaction::reset()` 清零。MMIO tag 的唯一写者是 `common_data_transaction` 中的 canonical setter/clear API，monitor、adapter、sequence 和 testcase 都不得直接写 status 字段。

新增 `resolve_mmio_uid_by_rob_value(rob_value, expected_kind, raw_sample_flush_epoch, uid, stale_reason)`。helper 只 probe `{flag=0,value}` 和 `{flag=1,value}` 两个完整 key，并通过 `uid_by_active_rob` associative map 查找，不扫描主表。raw epoch 大于 current epoch 时先 fatal；raw epoch 小于 current 也不能整条 raw 直接丢弃，必须逐 port 归一化。只有能证明 raw 早于全部当前实例 activation，或 old raw 两个 key 都无 active instance 时，才返回 `STALE_DROP`；其它歧义 fatal。

对同一 raw 的多个 MMIO port 先做 staging：按 uid 去重、检查 kind 冲突、用 setter 的 dry-run/preflight 模式预检全部 unique tag，全部通过后再按首 port 顺序 commit。directed tag 后收到同 kind monitor tag 允许升级 source；load/store kind 冲突 fatal。

### 文字伪代码

```text
apply_raw_ctrl_mmio_tags(raw)：
  如果 raw.mmio_flush_epoch > current dispatch_flush_epoch：
    在查询 active map 前 fatal；

  对每个 valid load port：
    调用 resolve_mmio_uid_by_rob_value(value, LOAD, raw.mmio_flush_epoch)；
    CURRENT 则把 uid/kind/source 暂存到 staging；
    STALE_DROP 则记录 drop reason，只跳过该 port；

  如果 store_mmio_valid：
    调用 resolve_mmio_uid_by_rob_value(value, STORE, raw.mmio_flush_epoch)；
    按同样规则 staging 或 drop；

  staging 完成后：
    按 uid 去重；
    同一 uid 同一 kind 幂等；
    同一 uid load/store 冲突 fatal；
    对所有 unique tag 先调用 canonical setter 的 apply_update=0 预检；
    全部预检成功后，再按首 port 顺序 apply_update=1 提交；

resolve_mmio_uid_by_rob_value：
  构造 key={flag=0,value} 和 key={flag=1,value}；
  对每个 key 调用 lookup_active_uid_by_rob，使用 associative map，不扫描主表；
  命中后读取 status/main transaction；
  要求 active_instance_flush_epoch_valid=1；
  要求 activation epoch 不大于 current epoch；
  LOAD 要求当前 active instance 是 scalar load 且 load 已 dispatch；
  STORE 要求当前 active instance 是 scalar store 且 STA/STD 已 dispatch；
  如果 raw epoch 不早于 activation epoch，记为合法 CURRENT 候选；
  如果两个 key 出现多个合法 CURRENT 候选，fatal；
  如果没有 CURRENT，但能证明 raw 早于所有当前 activation，返回 STALE_DROP；
  如果 current epoch 下没有 active instance 且 raw 是旧 epoch，返回 STALE_DROP；
  其它无法证明归属的情况 fatal；

setter/query：
  set_uid_mmio_tag(uid, kind, source) 只修改该 uid 的 tag 字段；
  directed 与 monitor 同 kind 时允许 source 升级为 monitor；
  kind 冲突 fatal；
  uid_is_mmio_load/store 只读 tag，不提交、不出队、不推进 head。
```

## 4. 问题三：`pendingMMIOld` sideband 必须交给 LSQ owner 构造

### V2 问题

V2 ROB 语义中，`pendingMMIOld` 表示当前 modeled ROB head 是 valid MMIO load，并且该 sideband 允许 LoadQueueUncache 发出普通 MMIO load request。它不能绑定到 writeback/pass 后的 commit candidate，否则 MMIO load 会形成“等 sideband 才能发 request、等 request 才能 pass”的循环等待。

### 修改原因

`pendingPtr/pendingMMIOld` 属于 LSQ MMIO/status plan 的 modeled ROB head 与 commit/deq flow。本 plan 只能提供 MMIO tag query，不能复制 LSQ owner 的 normal commit、fault convergence、driver hold 或 head rebase 主流程。

### 修改方案与修改逻辑

本 plan 只发布 `uid_is_mmio_load(uid)` / `uid_is_mmio_store(uid)` query。LSQ MMIO/status owner 在自己的 builder 中：

- 取得当前非 fault modeled head uid 和 `pendingPtr`。
- 对非 fault head 调用 `uid_is_mmio_load(head_uid)`。
- 若返回 1，则每拍输出 `pendingMMIOld=1`，并保持 `pendingPtr` 指向 modeled head ROB key。
- fault head 不调用 query，`pendingMMIOld=0`。
- normal commit、fault token、terminal/deq rebase、driver hold 和下一 head 发布全部由 LSQ owner 实现。

本 plan 不复制 `mark_rob_commit_batch()`、`build_lsqcommit_xaction()`、`apply_raw_ctrl_deq()`、fault sync helper 或 LSQ deq 主流程。raw ctrl collector 只把完整 raw 延后交给 LSQ owner 消费；即使 MMIO tag normalize 后没有 semantic event，也必须保留 deferred full raw 的 pop 顺序。

### 文字伪代码

```text
本 plan 发布 query：
  uid_is_mmio_load(uid)：
    读取 uid 对应 status；
    返回 status.mmio_tag_valid && status.is_mmio_load && !status.is_mmio_store；
    不修改 status、map、queue、head、commit 或 terminal；

LSQ owner builder 使用方式：
  读取自己的 modeled ROB head；
  如果当前 head 是 fault/convergence head：
    不调用 MMIO query；
    pendingMMIOld=0；
    由 fault rebase flow 决定何时发布下一 head；
  否则：
    pendingPtr = modeled head ROB key；
    pendingMMIOld = uid_is_mmio_load(head_uid)；
    pendingst/scommit 和 head 推进仍按 LSQ owner 自己的合同执行；

postcondition：
  非 fault tagged load head 在成为 modeled head 后连续输出 pendingMMIOld=1；
  该输出不等待 writeback/pass/normal commit batch；
  fault head 抑制 MMIO sideband；
  store MMIO tag 不直接改变 pass/fail/terminal。
```

## 5. 问题四：LOAD/STORE/AMO/CBO 行为矩阵存在重复和 package 依赖风险

### V2 问题

`common_data_transaction` 需要在 tag resolver/setter 等早期逻辑中推导 op behavior，但现有 `lsq_ctrl_model::derive_op_behavior()` 定义在 `common_data_transaction.sv` 之后；直接调用会形成 package 顺序问题。把 `lsq_ctrl_model.sv` 提前又会与它持有的 common data 状态形成反向依赖。若临时复制一份分类逻辑，会出现两个行为矩阵。

### 修改原因

LOAD/PREFETCH/STORE/CBO/AMOCAS/AMO 的分类是无 runtime state 的纯行为矩阵，应有唯一真源。common data 和 lsq ctrl model 都应该复用同一 helper，而不是各自维护常量列表和字段赋值。

### 修改方案与修改逻辑

新增 `memblock_op_behavior_util.sv`，固定 include 在 `main_control_transaction.sv` 之后、`status_transaction.sv` 和 `common_data_transaction.sv` 之前。`seq.f` 只更新注释/清单以反映真实 include inventory，仍由 `seq_pkg.sv` 编译。

本轮不新增 `memblock_fuop_category_e`，也不增加通用 `is_fuop_in_category()` 分发层。
`memblock_op_behavior_util` 直接迁移 `lsq_ctrl_model` 现有具名 classifier、默认 behavior 构造和
`derive_op_behavior()`；迁移时保持现有常量集合、判断优先级、behavior 字段赋值和 fatal 边界，不重新设计
分类协议。

`memblock_op_behavior_util` 提供：

```systemverilog
static function bit is_vector_ls_futype(bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] fuType);
static function bit is_load_fuoptype(bit [8:0] fuOpType);
static function bit is_prefetch_fuoptype(bit [8:0] fuOpType);
static function bit is_store_fuoptype(bit [8:0] fuOpType);
static function bit is_cbo_fuoptype(bit [8:0] fuOpType);
static function bit is_amocas_q_fuoptype(bit [8:0] fuOpType);
static function bit is_amocas_wd_fuoptype(bit [8:0] fuOpType);
static function bit is_amo_fuoptype(bit [8:0] fuOpType);
static function memblock_op_behavior_t make_default_behavior();
static function memblock_op_behavior_t derive_op_behavior(main_control_transaction tr);
```

`lsq_ctrl_model` 保留上述既有静态 API 名称和签名，但每个函数体只一行转发到
`memblock_op_behavior_util`，不保留常量列表、case 或 behavior 字段赋值。新 MMIO resolver/setter 直接
调用 util，避免 common data 依赖后定义的 stateful `lsq_ctrl_model`。

本方案不采用 `typedef class common_data_transaction` 加 `extern` 方法拆分：该语法虽然可行，但会让
`common_data_transaction` 与 `lsq_ctrl_model` 保持循环依赖，并要求把访问 common data 的构造/分配方法体
移到完整 class 定义之后；对本轮纯分类共享没有必要。

### 文字伪代码

```text
seq_pkg include 顺序：
  include memblock_dispatch_types.sv；
  include main_control_transaction.sv；
  include memblock_op_behavior_util.sv；
  include status_transaction.sv；
  后续 include common_data_transaction.sv 和 lsq_ctrl_model.sv；

memblock_op_behavior_util::derive_op_behavior(tr)：
  tr为空则 fatal；
  如果 fuType 命中 vector LS，按本轮不支持边界 fatal；
  behavior = make_default_behavior()；
  如果 fuType 是 LDU：
    先用 is_prefetch_fuoptype()，再用 is_load_fuoptype() 区分 PREFETCH 和 LOAD；
    填写 LQ、route、commit 和 num_ls_elem；
  如果 fuType 是 STU：
    先用 is_cbo_fuoptype()，再用 is_store_fuoptype() 区分 CBO 和普通 STORE；
    填写 SQ、STA/STD route、commit 和 num_ls_elem；
  如果 fuType 是 MOU：
    要求 is_amo_fuoptype() 命中；
    按 is_amocas_q_fuoptype()/is_amocas_wd_fuoptype()/其它 AMO 填写 uop 数和 route；
  其它 fuType fatal；
  返回完整 behavior；
  全函数不访问 common_data、status、queue、map、driver 或 monitor；

lsq_ctrl_model wrapper：
  is_vector_ls_futype、各具名fuOpType classifier、make_default_behavior和derive_op_behavior均保留签名；
  每个旧 API 只一行转发到 util；
  不保留第二份分类常量、case或字段赋值；
  后续调用方可逐步迁移，但行为真源只有 util。
```

## 6. 问题五：directed soft test 需要验证交接合同但不能复制 owner 主流程

### V2 问题

MMIO tag 和 `pendingMMIOld` 的关键风险在跨 owner 交接：raw producer、active uid tag、LSQ owner sideband builder、IQ redirect token 和 fault rebase 都属于不同专项。如果 directed test 直接写 status 或复制 LSQ/IQ 主流程，测试会绕开真实合同。

### 修改原因

本轮需要 software-only directed 验证 MMIO tag API 与 LSQ sideband query 的可用性，但依赖的 admission、issue fire、redirect token closure、modeled head、fault terminal/deq rebase 必须由各 owner 提供公开合同。本 plan 不应临时补同名 helper。

### 修改方案与修改逻辑

新增 `soft_test_memblock_pending_mmio_directed_sequence` 和顶层 `memblock_pending_mmio_directed_vseq`，通过 `basicTest + ts` 启动。directed sequence 只使用本 plan 的 MMIO setter/query/raw API 以及其它 owner 已公开的合同：

- 通过 owner API 构造或激活必要 uid，不直接写 `status_transaction` 字段。
- 通过本 plan canonical setter 预置 directed load/store MMIO tag，或通过 raw ctrl API 注入 monitor-like raw 事实。
- 通过 LSQ owner 合同检查非 fault tagged load head 能输出 `pendingMMIOld=1`。
- 通过 fault owner 合同检查 fault head 抑制 sideband，并等待 LSQ owner 的 terminal/deq rebase 发布下一 head。
- 不实现 MMIO 地址生成、response checker、coverage 或 RM。

三条正向 directed 仿真必须在 compile/width plan、IQ redirect token closure plan 和 LSQ MMIO/status plan 的对应 owner 功能完成后运行；依赖未完成时，测试入口可以保留为待启用，但不得在本 plan 里复制缺失 owner 主流程。

### 文字伪代码

```text
directed vseq：
  只在 basicTest 下启动；
  获取 virtual sequencer 和 soft_test sequence；
  不配置业务 agent default sequence；
  不直接写 status 字段；

soft directed sequence：
  场景一：directed load tag
    通过 admission/activation owner 创建 active scalar load uid；
    调用 common_data canonical setter 标记该 uid 为 directed MMIO load；
    调用 LSQ owner 公共接口把该 uid 发布为非 fault modeled head；
    观察 owner builder 输出 pendingMMIOld=1；

  场景二：monitor raw tag
    构造 raw_ctrl，其中 load_mmio_valid 对应某 active uid 的 ROB value；
    通过 raw ctrl adapter 走 apply_raw_ctrl_mmio_tags；
    验证 uid_is_mmio_load(uid)=1；
    再交给 LSQ owner 验证 sideband；

  场景三：fault head suppress
    通过 fault/terminal owner 合同构造 fault head；
    不直接改 head 或 status；
    LSQ owner 对 fault head 不调用 query，pendingMMIOld=0；
    terminal/deq rebase 后由 LSQ owner 发布下一 head；

失败策略：
  owner 合同缺失时测试 fatal 或保持 disabled，不在本 sequence 临时实现 owner 逻辑；
  tag kind 冲突、active provenance 缺失、raw epoch 歧义均按本 plan API fatal；
  directed test 不成为 RM/checker/coverage 交付物。
```
