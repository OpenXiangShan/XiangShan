# mem_ut V2 MMIO ROB tag 与 `pendingMMIOld` sideband 最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `do`，coding、`stale_reason` 修复、最终回归和独立终审均已完成并归档 |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 主要入口 | `io_mem_to_ooo_ctrl_agent_agent_monitor::mon_data()`、`common_data_transaction` MMIO tag API、LSQ owner 的 `pendingPtr/pendingMMIOld` builder |
| 适配原则 | 本 plan 只拥有 MMIO raw producer、active uid MMIO tag 和 `uid_is_mmio_load()` query；不复制 LSQ owner 的 commit/deq、modeled head、fault rebase 或 driver hold 主流程 |
| 创建/修订日期 | 2026-07-15 |

## 1. Plan 定位、术语与抽象功能说明

### 1.1 Plan 定位

本 plan 是 V2 测试框架中 MMIO output 采样、动态实例标签和 `pendingMMIOld` 查询交接的 coding plan。
它负责把 DUT raw fact 归一化为唯一 active uid，并向 LSQ owner 提供只读查询；它不拥有 ROB head、
normal commit、LSQ deq、fault convergence 或 RM/pass/fail。

### 1.2 专有名词与状态所有权

| 术语 | 本 plan 中的含义 | 代码落点/唯一写者 | 示例 |
|---|---|---|---|
| `raw` | monitor 在 DUT sample 边界采集的一拍原始事实，不代表软件状态已更新 | `dispatch_raw_ctrl_t`；ctrl monitor 写入 raw queue | `loadMmio_0=1` 只先进入 raw queue |
| `observation epoch` | monitor 观察 MMIO output 时保存的环境 flush epoch，只描述观察时环境状态，不证明 DUT 脉冲由哪个请求产生 | `dispatch_raw_ctrl_t::mmio_flush_epoch` | 旧请求的迟到脉冲可在新 observation epoch 被观察到 |
| `producer provenance` | MMIO output 所在 DUT sample 的单调序号，用于和 redirect sample anchor 建立时序来源关系 | `dispatch_raw_ctrl_t::mmio_sample_seq`、`get_dut_sample_seq()` | loadMmio 在 redirect sample `R` 或 `R+1` 出现时进入 overlap 判定 |
| `redirect sample anchor` | redirect 输入被 DUT monitor 采到的 sample 序号及完整 redirect payload | `cancel_record_q[*].redirect_sample_seq`、`redirect_anchor_history_q` | `R` 表示 anchor sample，`R+1` 表示下一 DUT sample |
| `epoch` | redirect/flush 后动态实例版本号，用于区分旧、新 active 实例 | `dispatch_flush_epoch`、`status.dynamic_epoch`；redirect owner 更新 | 旧实例 activation epoch 小于 redirect epoch，新实例不小于 redirect epoch |
| `active map` | 完整 ROB/LQ/SQ key 到当前 uid 的关联索引 | `common_data_transaction` 的 associative map；activate/retire 写入和删除 | `{rob.flag,value}` 唯一命中 uid |
| `dynamic instance` | 同一 uid 在一次 admission/reissue 生命周期中的一次活动实例 | `status_transaction` 的 `active_instance_flush_epoch`、`dynamic_epoch` | redirect 后旧实例无效，新实例重新建立 provenance |
| `canonical tag` | MMIO load/store 类型、来源和动态 epoch 的唯一状态表示 | `set_uid_mmio_tag()`/`clear_uid_mmio_tag()` 唯一写入 | monitor fact 可把 directed tag 的来源升级为 monitor |
| `staging` | 同一 raw 中多个 MMIO port 在写 status 前的临时去重列表 | `apply_raw_ctrl_mmio_tags()` 局部队列 | 两个 port 指向同 uid 时只提交一次 |
| `owner` | 对某类状态拥有唯一推进权的模块/handler，不是新增 DUT 模块 | LSQ owner 推进 head/deq；本 plan 只提供 tag query | adapter 不创建第二个 LSQ commit owner |
| `sideband` | 与主 commit/deq payload 分开的 level/pulse 控制字段 | `pendingPtr/pendingMMIOld` 由 LSQ owner driver 发送 | tagged head 即使无新 commit 也保持 `pendingMMIOld` |

必要时序例子：redirect 在 sample `R` 被 DUT 采到，旧 scalar load 的 `loadMmio` 仍可能在 `R` 或
`R+1` 被 monitor 观察。此时 `mmio_flush_epoch` 只是观察 epoch；resolver 必须使用
`mmio_sample_seq` 扫描未完成 redirect provenance，只有唯一旧 load owner 可证明被 redirect 覆盖时才能
`STALE_DROP`，不能把迟到脉冲写到 redirect 后复用相同 ROB value 的新实例。

### 1.3 关键函数抽象功能说明

| 函数/任务 | 抽象职责、输入和输出 | 明确不负责的相邻职责 |
|---|---|---|
| `sample_mmio_outputs()` | 从 V2 扁平端口生成 packed MMIO mirror；输入是当前 interface sample，输出是 valid/value mirror | 不做 X/Z 检查、uid 反查或 status 更新 |
| `mon_data()` | 在 monitor callback 采样 accessor、构造 raw ctrl 并入队 | 不消费 raw、不推进 LSQ/ROB/terminal |
| `resolve_mmio_uid_by_rob_value()` | 用两个完整 ROB flag key、active map、op/dispatch、observation epoch 与 sample provenance 得到唯一 uid、stale 或 fatal；LOAD 额外处理 `R/R+1` overlap | 不写 MMIO tag、不修改 map；STORE 不套用 LOAD overlap 规则 |
| `set_uid_mmio_tag()` | 对单个已验证 active uid 执行 MMIO tag dry-run 或 canonical commit | 不改变 pass/fail、commit/deq 或 terminal |
| `apply_raw_ctrl_mmio_tags()` | 对完整 raw 的 MMIO facts 做逐 port resolve、去重、全量 preflight 和原子提交 | 不消费 LSQ deq、不代替 singleton LSQ owner |
| `build_lsqcommit_xaction()` | 由 LSQ owner 构造 `pendingPtr/pendingMMIOld/scommit` transaction | 本 plan 不实现该函数的 head/cursor/deq 算法 |

## 2. 范围与边界

本 plan 只整理 V2 MMIO ROB tag 与 `pendingMMIOld` sideband 支持需要解决的问题。每个问题均说明 V2 问题、修改原因、最终方案、修改的原有逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- DUT `loadMmio/loadMmioUop`、`storeMmio/storeMmioUop` output 到 raw ctrl 的唯一 producer。
- V2 扁平 MMIO 物理端口到参数化 packed mirror 的 `sample_mmio_outputs()` profile accessor。
- 可选 SQ deq pointer presence 到 `sq_deq_ptr_valid` 的 `sample_sq_deq_ptr()` profile accessor。
- raw MMIO output 按 ROB value、observation epoch、DUT sample provenance 和 active instance provenance
  归一化到唯一 active uid。
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

## 3. 问题一：MMIO raw producer 与 SQ pointer presence 职责混在一起

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
longint unsigned mmio_sample_seq;
bit sq_deq_ptr_valid;
```

中文伪代码：

1. 定义 raw 的 MMIO packed valid/value、observation epoch、DUT sample provenance 和可选 SQ pointer
   presence 字段。
2. 这些字段只保存 monitor 事实；后续 adapter 依据 valid 逐项解析，未置 valid 的 payload 不得被消费。
3. `mmio_flush_epoch` 只表示 monitor 观察时的环境 epoch；`mmio_sample_seq` 才能和 redirect sample
   anchor 对齐，两者不得互相替代。
4. `sq_deq_ptr_valid` 只表示 pointer capability 是否存在，不改变 `sq_deq` count 的宽度或语义。

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
    任一 MMIO valid 时：
      raw.mmio_flush_epoch = 当前 dispatch_flush_epoch，作为 observation epoch；
      raw.mmio_sample_seq = get_dut_sample_seq($time)，冻结本 monitor sample provenance；
    MMIO 全 invalid 时保持 mmio_sample_seq=0，不由 adapter 在消费拍补写；
    raw.valid=1，写入当前 cycle；
    push_raw_ctrl(raw)；
  否则不创建 raw；

  producer 到此结束，不修改 SQ/LSQ map、status、pointer 或 terminal。
```

## 4. 问题二：MMIO output 只有 ROB value，需要归一化到唯一 active uid

### V2 问题

DUT MMIO output 给出的是 ROB value。测试框架 active ROB map 使用完整 ROB key，且 redirect 后同一 ROB value 可能出现旧 raw 与新 active instance 重叠。如果只按 value 或全表扫描 uid 反查，容易把 stale raw 写到错误 uid。

### 修改原因

MMIO tag 必须绑定到“当前动态实例”。`mmio_flush_epoch` 只能说明 monitor 观察脉冲时的环境版本，不能证明
该脉冲由哪个 DUT request 产生。尤其 LOAD 的 s1 后一拍 MMIO 脉冲可能与 redirect sample `R/R+1`
重叠，因此必须结合 `mmio_sample_seq`、未完成 redirect provenance、active map 和 activation provenance
判定 current、可证明 stale drop 或 fatal 歧义。

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

中文伪代码：

1. 为每个 status 保存当前动态实例的 activation provenance，以及 canonical MMIO load/store tag。
2. `activate_uid()` 建立 provenance，redirect/deq 清理旧实例；只有 canonical setter/clear API 能写 tag。
3. query 只读取同一动态实例的 canonical 位，不驱动 commit、deq、pass/fail 或 terminal。

不再维护重复的 `is_mmio`。统一 query 语义为：

```text
uid_is_mmio_load  = mmio_tag_valid && is_mmio_load && !is_mmio_store
uid_is_mmio_store = mmio_tag_valid && is_mmio_store && !is_mmio_load
```

`active_instance_flush_epoch{_valid}` 的唯一运行期写者是 `activate_uid()` 和 `clear_uid_dispatch_result()`；`status_transaction::reset()` 清零。MMIO tag 的唯一写者是 `common_data_transaction` 中的 canonical setter/clear API，monitor、adapter、sequence 和 testcase 都不得直接写 status 字段。

新增 `resolve_mmio_uid_by_rob_value(rob_value, expected_kind, raw_sample_flush_epoch,
raw_sample_seq, uid, stale_reason)`。helper 只 probe `{flag=0,value}` 和 `{flag=1,value}` 两个完整 key，
并通过 `uid_by_active_rob` associative map 查找，不扫描主表。raw observation epoch 大于 current epoch、
sample provenance 为 0 或大于 monitor 最新 watermark 时 fatal。

LOAD 额外扫描有编译期深度上限的全部未完成 anchored cancel record，以及尚未绑定 record 的 redirect
anchor FIFO。只有 raw sample 等于某个唯一 redirect anchor 的 `R` 或 `R+1`，且唯一 active 候选是已经
`load_dispatched` 的 scalar load、其完整 ROB key 被该 redirect 覆盖、activation epoch 早于 redirect
epoch时，才能返回 `STALE_DROP`。只命中新实例、无 owner、多个 record/anchor、不兼容 owner、anchor 与
record 不匹配或任何其它无法证明场景均以 `MMIO_RESOLVE` fatal。STORE 不扫描该 LOAD s1/s2 overlap，
继续按 observation epoch 与 active provenance 的普通规则解析。

对同一 raw 的多个 MMIO port 先做 staging：按 uid 去重、检查 kind 冲突、用 setter 的 dry-run/preflight 模式预检全部 unique tag，全部通过后再按首 port 顺序 commit。directed tag 后收到同 kind monitor tag 允许升级 source；load/store kind 冲突 fatal。

### 文字伪代码

```text
apply_raw_ctrl_mmio_tags(raw)：
  如果 raw.mmio_flush_epoch > current dispatch_flush_epoch：
    在查询 active map 前 fatal；

  对每个 valid load port：
    调用 resolve_mmio_uid_by_rob_value(value, LOAD, raw.mmio_flush_epoch,
                                        raw.mmio_sample_seq)；
    CURRENT 则把 uid/kind/source 暂存到 staging；
    STALE_DROP 则记录 drop reason，只跳过该 port；

  如果 store_mmio_valid：
    调用 resolve_mmio_uid_by_rob_value(value, STORE, raw.mmio_flush_epoch,
                                        raw.mmio_sample_seq)；
    按同样规则 staging 或 drop；

  staging 完成后：
    按 uid 去重；
    同一 uid 同一 kind 幂等；
    同一 uid load/store 冲突 fatal；
    对所有 unique tag 先调用 canonical setter 的 apply_update=0 预检；
    全部预检成功后，再按首 port 顺序 apply_update=1 提交；

resolve_mmio_uid_by_rob_value：
  要求 raw_sample_seq 非0且不大于 monitor 最新 DUT sample watermark；
  如果 expected_kind=LOAD：
    扫描全部未完成且已绑定 anchor 的 cancel record；
    再扫描未绑定 redirect anchor FIFO，并按 FIFO 顺序和未绑定 record 配对；
    只接受 raw_sample_seq=R 或 R+1；
    若重叠多个 record/anchor、anchor 无 record或 payload 不一致，MMIO_RESOLVE fatal；
  构造 key={flag=0,value} 和 key={flag=1,value}；
  对每个 key 调用 lookup_active_uid_by_rob，使用 associative map，不扫描主表；
  命中后读取 status/main transaction；
  要求 active_instance_flush_epoch_valid=1；
  要求 activation epoch 不大于 current epoch；
  LOAD overlap 要求唯一 active instance 是 scalar load、commit_is_load=1且load已dispatch；
  只有完整key被redirect覆盖且activation epoch早于redirect epoch时STALE_DROP；
  新owner、无owner、多个owner、不兼容owner或无法证明覆盖均MMIO_RESOLVE fatal；
  非 overlap LOAD 要求当前 active instance 是 scalar load 且 load 已 dispatch；
  STORE 要求当前 active instance 是 scalar store 且 STA/STD 已 dispatch；
  STORE 不执行 R/R+1 overlap 特例；
  如果 raw observation epoch 不早于 activation epoch，记为合法 CURRENT 候选；
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

## 5. 问题三：`pendingMMIOld` sideband 必须交给 LSQ owner 构造

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

## 6. 问题四：LOAD/STORE/AMO/CBO 行为矩阵存在重复和 package 依赖风险

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

中文伪代码：

1. util 提供唯一的 LOAD/STORE/AMO/CBO 行为分类和完整 behavior 构造函数。
2. 调用者传入 main transaction；util 按既定 fuType/fuOpType 优先级返回 uses_lq/uses_sq、commit kind 和元素数。
3. util 不访问 common data、status、queue 或 driver；`lsq_ctrl_model` 只转发旧 API，避免出现第二份分类表。

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

## 7. 问题五：directed soft test 需要验证交接合同但不能复制 owner 主流程

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

directed 验证必须在 compile/width plan、IQ redirect token closure plan 和 LSQ MMIO/status plan 的对应
owner 功能完成后运行；依赖未完成时，测试入口可以保留为待启用，但不得在本 plan 里复制缺失 owner 主流程。

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

  场景三/四：LOAD redirect overlap stale
    建立已dispatch的唯一旧scalar load owner与完整ROB key；
    建立redirect record和DUT sample anchor R；
    分别注入mmio_sample_seq=R与R+1的load raw；
    resolver返回STALE_DROP，且旧uid与任何新uid均不写tag；

  场景五：新owner overlap精确fatal
    在同一redirect epoch内建立复用ROB key的新load实例；
    只在一次resolver调用期间安装report catcher；
    catcher仅捕获ID=MMIO_RESOLVE且消息匹配cannot prove LOAD MMIO stale ownership的fatal；
    其它fatal继续THROW；要求精确捕获一次且不写tag；

  watermark等待：
    wait_for_dut_sample_watermark只读取peek_latest_dut_sample_seq并等待dut_sample_seq变化；
    helper不得调用get_dut_sample_seq，不得由sequence伪造或推进monitor watermark；

  后续场景：fault head suppress与global-stop raw drain
    通过 fault/terminal owner 合同构造 fault head；
    不直接改 head 或 status；
    LSQ owner 对 fault head 不调用 query，pendingMMIOld=0；
    terminal/deq rebase 后由 LSQ owner 发布下一 head；

失败策略：
  owner 合同缺失时测试 fatal 或保持 disabled，不在本 sequence 临时实现 owner 逻辑；
  tag kind 冲突、active provenance缺失、observation epoch或sample provenance歧义均按本plan API fatal；
  directed test 不成为 RM/checker/coverage 交付物。
```

## 8. 执行记录（2026-07-22）

本轮已完成以下 coding：

- `dispatch_raw_ctrl_t` 已加入 MMIO packed mirror、`mmio_flush_epoch`、`mmio_sample_seq` 和
  `sq_deq_ptr_valid`，empty raw 已完整复位。
- V2 ctrl interface 已提供 `sample_mmio_outputs()` 和 `sample_sq_deq_ptr()`；monitor
  只通过 accessor 采样，按 valid 检查 payload，并在 MMIO-only 周期创建 raw；仅在任一 MMIO valid
  时冻结同拍 DUT sample seq。
- `status_transaction` 已改为单一 canonical tag，并记录 tag 的 `dynamic_epoch`；
  `activate_uid()` 与 `clear_uid_dispatch_result()` 分别建立、清除 active instance provenance。
- `common_data_transaction` 已提供 canonical setter/clear/query、双 ROB flag active-map resolver、
  LOAD `R/R+1` redirect overlap provenance 判定和 stale/future/ambiguous 处理；STORE 不套用 LOAD
  overlap 特例。
- adapter 已实现同 raw staging、UID 去重、全量 preflight 和原子 commit；MMIO normalize
  位于完整 raw deq 前，完整 `dispatch_raw_ctrl_t` 直接交给 singleton
  `lsq_commit_handler::apply_raw_ctrl_deq()`。
- LOAD/STORE/AMO/CBO 行为矩阵已迁移到 `memblock_op_behavior_util`；`lsq_ctrl_model`
  只保留转发 API。
- directed soft sequence 与 `memblock_pending_mmio_directed_vseq` 已加入 package/filelist，覆盖 inactive
  head、directed tag、normal monitor-like raw、`R/R+1` stale drop、精确 expected-fatal、fault-head
  suppress、terminal/deq rebase和global-stop raw drain；watermark helper只等待monitor推进。

已完成检查：

```text
git diff --check：通过。
verible-verilog-syntax：新增文件及除生成式 monitor 旧语法入口外的核心修改文件通过。
旧双 tag/source/mark API 残留检索：无结果。
singleton/full-raw/reconcile 调用点检索：符合当前 owner 合同。
```

2026-07-22 实际远端验证记录：

```text
testcase：basicTest
ts：memblock_pending_mmio_directed_vseq
环境问题：partcomp 使用的旧专项生成库损坏；该问题不是代码失败。
处理方式：删除专项生成库，并设置 partcmp_op=off 后重新编译。
VCS/KDB compile：最终 KDB 摘要为0 error / 0 warning；完整 transcript 含一条工具自身的
`LCA_FEATURES_ENABLED` usage warning。
batch 日志：mem_ut/ver/ut/memblock/sim/pending_mmio_v2_fun/log/tc=basicTest_ts=memblock_pending_mmio_directed_vseq_cfg=default_seed=666666_rtl_.log
日志检查点：VSEQ_BODY start/complete、R stale、R+1 stale、directed completion、caught fatal=1、UVM_ERROR=0、UVM_FATAL=0、TEST_PASS。
```

2026-07-23 `stale_reason` 修复后最终独立 mode 复验：

```text
mode：v2_lsq_mmio_cbo_final_20260723。
VCS/KDB compile：最终 KDB 摘要为0 error(s), 0 warning(s)；完整 transcript 另有一条工具自身的
LCA_FEATURES_ENABLED usage warning。
pending-MMIO directed：TEST_PASS，UVM_ERROR=0，UVM_FATAL=0，caught UVM_FATAL=1。
日志：mem_ut/ver/ut/memblock/sim/v2_lsq_mmio_cbo_final_20260723/log/tc=basicTest_ts=memblock_pending_mmio_directed_vseq_cfg=default_seed=666666_rtl_.log
相邻real smoke：TEST_PASS，UVM_ERROR=0，UVM_FATAL=0。
相邻real cancel reconcile：TEST_PASS，UVM_ERROR=0，UVM_FATAL=0。
```

caught fatal 是新 owner overlap 场景精确安装 catcher 后捕获的预期 `MMIO_RESOLVE` fatal；最终 report 中
未捕获 `UVM_FATAL` 为0。日志同时出现 normal raw、`R` stale、`R+1` stale、directed completion和
`VSEQ_BODY` start/complete。本轮不要求单独执行 LSQ driver `DRV_X/DRV_RAND/DRV_LST` 模式专项回归，
也不覆盖本 plan 明确排除的 MMIO response checker、RM、coverage 和地址生成策略。

## 9. 执行中补充/修正（IMPLEMENTATION_DELTA）

### 9.1 timing sideband collector 与 reconcile 拆分

[IMPLEMENTATION_DELTA]

- 来源：coding 收尾期间的并行 review 发现 `service_monitor_once()` 每拍两次调用
  `drain_lsq_timing_sidebands()`，旧函数每次都会执行 `service_cancel_reconcile()`。
- 原 plan：pending-MMIO plan 未定义 cancel timing sideband 的 service 次数。
- 实现调整：`drain_lsq_timing_sidebands()` 只采集 raw snapshot/anchor；新增
  `service_lsq_timing_reconcile()`，在 exception/redirect 处理和第二次 drain 后每个 service tick
  只调用一次。
- 原因：允许同拍前后两次 collector drain，同时避免 reconcile record/deadline 生命周期一拍推进两次。
- 影响范围：`dispatch_monitor_event_adapter.sv`、
  `memblock_main_dispatch_auto_build_main_table_base_sequence.sv`。

### 9.2 adapter fallback 强制使用 singleton

[IMPLEMENTATION_DELTA]

- 来源：并行 owner review 要求 adapter 不得创建第二个 `lsq_commit_handler`。
- 原 plan：要求不复制 LSQ owner，但未明确 adapter fallback 的构造方式。
- 实现调整：`ensure_handles()` 使用 `lsq_commit_handler::get()`；deferred ctrl consumer 把完整 raw
  直接传给该 singleton。
- 原因：独立 handler 会复制 commit cursor、modeled head 和 deq pointer owner 状态。
- 影响范围：`dispatch_monitor_event_adapter.sv`。

### 9.3 directed body 幂等初始化 helper

[IMPLEMENTATION_DELTA]

- 来源：directed 仿真启动检查发现，`uvm_do_on` 启动子 sequence 时可能不执行继承的
  `memblock_dispatch_base_sequence::pre_body()`，导致 `body()` 首个场景看到 `data`、`lsq_ctrl` 或
  `monitor_adapter` 为空并 fatal。
- 原 plan：只规定 vseq 启动 soft directed sequence 和 owner API 边界，没有定义跳过 `pre_body()` 时的
  helper 初始化合同。
- 实现调整：新增幂等 task `ensure_directed_helpers()`，并在 directed `body()` 首行以 task 调用。helper 复用当前
  base sequence 的 singleton/factory/bind API，初始化 `seq_csr_common`、data、LSQ、issue、writeback、
  batch、exception、commit 和 adapter helper；`commit_handler` 与 `monitor_commit_handler` 最终都指向
  `lsq_commit_handler::get()`。
- 原因：directed sequence 必须在 UVM macro 是否调用 `pre_body()` 两种启动方式下都满足相同 handle
  合同，同时不能手工调用 phase callback 或复制状态机。
- 影响范围：
  `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`。
- 状态副作用：helper 只建立并绑定 handle，不调用 `lsq_ctrl.reset()`、
  `reset_lsqcommit_runtime_state()` 或任何 status/map/queue 更新；场景运行期 reset 仍由
  `reset_directed_owner_state()` 按原 owner 合同执行。

### 9.4 directed helper function-to-task 编译修正

[IMPLEMENTATION_DELTA]

- 来源：VCS 编译在 `ensure_directed_helpers()` 调用 `seq_csr_common::init()` 处报告 `SV-DOSIF`。
- 原实现：helper 的 extern 和定义使用 function/`endfunction`，但 function 不允许调用 task。
- 编译修正：extern 改为 `extern virtual task ensure_directed_helpers()`，定义改为 task/`endtask`；
  `body()` 继续按 task 方式调用并等待 helper 完成。
- 约束保持：`seq_csr_common::init()` 仍按原合同执行，不改成 function，也不绕过初始化。
- 影响范围：
  `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`。

### 9.5 LOAD MMIO sample provenance 与 redirect overlap 修正

[IMPLEMENTATION_DELTA]

- 来源：coding 中的 RTL provenance review 发现，V2 `loadMmio` 是 load uncache 路径的 s1 后一拍脉冲；
  request 可在后续 redirect gate 被杀掉，但旧脉冲仍可能在 redirect sample `R` 或 `R+1` 被观察。
- 原 plan：只使用 `mmio_flush_epoch` 和 active instance activation epoch。该字段是 monitor observation
  epoch，不是 DUT producer epoch；若 ROB value 在 redirect 后复用，单靠它无法证明迟到脉冲属于旧实例。
- 实现调整：`dispatch_raw_ctrl_t` 新增 `mmio_sample_seq`，ctrl monitor 仅在 MMIO valid 时冻结同拍
  `get_dut_sample_seq($time)`。LOAD resolver 扫描有界未完成 anchored record 与未绑定 anchor FIFO，按
  `R/R+1` 判定 overlap；只有唯一旧 scalar load owner、`load_dispatched=1`、完整 ROB key 被 redirect
  覆盖且 activation epoch 早于 redirect epoch时返回 `STALE_DROP`。
- fail-fast：新 owner、无 owner、多个 record/anchor、不兼容 owner、anchor/record 不匹配或无法证明覆盖
  均以 `MMIO_RESOLVE` fatal；STORE 保持普通 observation-epoch resolver，不套用 LOAD overlap 特例。
- directed 补充：新增 `R`、`R+1` stale drop和新 owner精确 expected-fatal；catcher只捕获指定
  `MMIO_RESOLVE` 消息，watermark helper只等待 monitor推进，不调用 sample-seq accessor。
- 影响范围：`memblock_sync_pkg.sv`、ctrl monitor、`common_data_transaction.sv`、
  `dispatch_monitor_event_adapter.sv`、pending-MMIO directed sequence，以及本 plan、对应 flow和
  implementation review。

### 9.6 `basicTest` 显式启动 `+VSEQ_MAIN`

[IMPLEMENTATION_DELTA]

- 来源：directed 运行日志虽然确认 `+VSEQ_MAIN=memblock_pending_mmio_directed_vseq` 已被读取、
  `env.vsqr.main_phase.default_sequence` resource 已写入，但日志没有目标 vseq、空
  `virtual_base_sequence` 或 directed child 的 body 输出，expected-fatal catcher 计数也为 0。该次
  `TEST_PASS` 因而不能证明 directed 场景执行过。
- UVM 1.2 检查：标准 `uvm_sequencer_base::start_phase_sequence()` 会在 phase started 时读取
  `<sequencer>.main_phase.default_sequence`，理论上不要求 testcase 自己实现 `main_phase()`；旧日志中的
  `CFGNRD` 又产生于 `connect_phase` 的提前检查，单独不能证明 runtime phase 未读取。当前证据只能确认
  项目旧的“base wrapper + name override + phase default”间接入口没有形成可观察启动闭环，不能把该次
  空运行作为验证结果。
- 原 plan：由 `basicTest` 对 `virtual_base_sequence` 设置 name-based factory override，再把 base wrapper
  写入 `env.vsqr.main_phase.default_sequence`。
- 实现调整：`build_phase()` 保留 `+VSEQ_MAIN=<class>`，使用
  `uvm_factory::find_wrapper_by_name()` 取得目标类真实 wrapper，未注册立即 `uvm_fatal`；不再设置 base
  override 或 phase default resource。`basicTest::main_phase()` 使用该 wrapper 创建对象，要求创建成功且
  可以 cast 为 `virtual_base_sequence`，随后调用 `set_starting_phase(phase)` 并在 `env.vsqr` 上
  `start()`，不允许 `start(null)`。
- objection 合同：testcase 在调用 `start()` 前 raise objection，并在 `start()` 完整返回后 drop；该
  objection 覆盖目标 vseq 的整个 `pre_body()/body()/post_body()` 生命周期。派生 vseq 的
  `pre_body()/post_body()` 或 automatic objection 可以保留局部 drain/兼容行为，但顶层 main phase 保活
  不依赖这些 callback 的 objection。
- 可观察性：目标 sequence 使用 `VSEQ_BODY` 在 `start()` 前和 body 返回后各输出一条 `UVM_INFO`；只有
  completion 日志与 pending-MMIO directed completion 同时出现，才能证明目标 body 实际执行完成。
- 影响范围：`mem_ut/ver/ut/memblock/tc/src/basicTest.sv`。不新增 testcase，不改变具体 vseq 的 child
  sequence 调度、`p_sequencer` 类型或 pending-MMIO 功能逻辑。

抽象功能描述：`basicTest::main_phase()` 只负责把命令行选择的顶层 virtual sequence 创建并启动到
`memblock_env.vsqr`，把 UVM phase 与 `p_sequencer` 上下文交给继承自 `virtual_base_sequence` 的场景；
它不实现 pending-MMIO 场景，也不直接启动 agent sequence。

文字伪代码：

```text
build_phase：
  读取 +VSEQ_MAIN，默认 virtual_base_sequence；
  按名称查询 factory wrapper；
  wrapper 不存在则 BASIC_VSEQ_FACTORY fatal；

main_phase：
  检查 env.vsqr 和 wrapper 非空；
  通过 wrapper 创建对象，创建失败则 BASIC_VSEQ_CREATE fatal；
  cast 到 virtual_base_sequence，失败则 BASIC_VSEQ_TYPE fatal；
  testcase raise 顶层vseq生命周期 objection；
  set_sequencer(env.vsqr)并reseed，使randomize阶段也可使用正确p_sequencer；
  set_starting_phase(main phase)仅提供phase上下文，不把派生pre_body/post_body objection作为保活前提；
  randomize 目标 vseq，失败则 fatal；
  输出 VSEQ_BODY starting；
  start(env.vsqr)，由 UVM 设置 m_sequencer/p_sequencer 并执行 pre_body/body/post_body；
  body 返回后输出 VSEQ_BODY completed；
  start完整返回后，testcase drop顶层vseq生命周期 objection。
```

### 9.7 expected-fatal 的 `stale_reason` 日志修复

[IMPLEMENTATION_DELTA]

- 来源：2026-07-22 batch 已确认新 owner overlap 的 `MMIO_RESOLVE` expected-fatal 被 catcher 精确捕获，
  但该 fatal 分支没有把同一条诊断文本写入输出参数 `stale_reason`，导致后续按 resolver 输出记录原因时为空。
- 原实现：在 `cannot prove LOAD MMIO stale ownership` fatal 前直接把 `$sformatf(...)` 作为 fatal message，
  然后返回 `MEMBLOCK_MMIO_RESOLVE_STALE_DROP`；`stale_reason` 保持初始空字符串。
- 实现调整：先用完全相同的格式化内容赋给 `stale_reason`，再把该变量传给 `uvm_fatal`。
- 原因：让 expected-fatal 被 catcher 捕获后仍保留完整 owner 计数和 sample/redirect 上下文，便于日志定位。
- 行为边界：不改变 fatal ID、fatal message 内容、返回值、tag 写入、pass/fail、terminal 或 catcher 的
  捕获条件；正常 current/stale/tag 路径不变。
- 影响范围：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，以及对应 plan、review
  的验证记录。该一行级别修复后的 compile/仿真必须重新复验，当前没有把它写成已通过。

抽象功能描述：`resolve_mmio_uid_by_rob_value()` 在无法证明 LOAD MMIO 归属时，构造并发布统一的
expected-fatal 诊断文本；本补充只同步输出参数和 fatal message，不改变 resolver 的判定结果或状态副作用。

文字伪代码：

```text
进入 LOAD overlap 的无法证明分支后，按原有 sample、redirect sample、active、old/new/uncovered/incompatible
计数格式化诊断字符串；
先把该字符串写入stale_reason，再以同一变量调用ID为MMIO_RESOLVE的uvm_fatal；
保持原有return路径和无tag提交结果；catcher仍按同一ID和消息模式捕获，后续日志读取stale_reason时不为空。
```

### 9.8 deferred full raw 的 resync 保留边界

[IMPLEMENTATION_DELTA]

- 来源：LSQ owner独立终审发现，full raw在resync preflight mismatch时会从automatic deferred列表消失；
  这会连带丢失本plan已经完成MMIO normalization、但尚未完成LSQ deq的raw生命周期。
- owner边界：持久FIFO、success返回值、LQ/SQ重试和runtime drain由LSQ MMIO/status plan实现；本plan仍只
  拥有MMIO raw producer、tag normalization和query，不复制deq owner。
- 集成调整：`dispatch_monitor_event_adapter::apply_raw_ctrl_deq()`返回singleton handler的success；
  `apply_deferred_ctrl_updates_batch()`把本拍完整raw追加到`memblock_sync_pkg::deferred_raw_ctrl_q`，只有
  full-raw owner成功才pop。失败队首保留，后续raw不越过。
- MMIO边界：MMIO normalization仍发生在deq删除active map之前；同一raw因resync重试时，canonical setter
  对同kind/同dynamic instance是幂等的。stale port仍按provenance丢弃，不会在新实例上复活旧tag。
- stop边界：`raw_monitor_queue_size()`包含持久deferred FIFO；等待重试的raw禁止global stop。

文字伪代码：

```text
本拍ctrl raw先完成memoryViolation semantic投影；semantic batch处理后进入持久deferred FIFO；
adapter对FIFO队首先做MMIO resolve/preflight/commit，再调用singleton LSQ owner：
  owner成功则弹出该raw；
  owner在resync模式返回失败则保留队首到下一service tick；
  strict模式仍fatal；
本plan不修改LQ/SQ pointer、free count、pass/fail或terminal，只保证MMIO处理顺序和tag幂等边界。
```
