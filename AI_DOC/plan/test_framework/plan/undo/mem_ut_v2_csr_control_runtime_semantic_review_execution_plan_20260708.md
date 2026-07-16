# mem_ut V2 CSR/control runtime 语义适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 主要入口 | `csr_ctrl_agent_agent_monitor::mon_data()`、`fence_agent_agent_driver::send_pkt()`、`memblock_l2tlb_base_sequence::send_l2tlb_cycle()` |
| 适配原则 | snapshot-only 字段只进入 raw/runtime snapshot；`flushPipe=1` 只允许 `basicTest + memblock_sfence_flushpipe_directed_vseq` 独占 standalone；不改变 sequence、pass/fault、LSQ/issue/terminal 主流程 |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只整理 V2 CSR/control runtime 语义适配需要解决的问题。每个问题均说明 V2 问题、修改原因、最终方案、修改的原有逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- `hd_misalign_ld_enable`、`hd_misalign_st_enable` 和 `tlbCsr_priv_debug` 的 monitor -> raw CSR -> `mmu_csr_runtime_state` snapshot 保存链路。
- 三个 snapshot-only 字段的 xaction soft 默认、driver idle 默认、monitor X/Z 检查和 runtime copy。
- `sfence_bits_flushPipe` 的默认 `0` 约束、monitor payload X/Z 检查、driver 驱动前 fail-fast gate。
- `flushPipe=1` 的唯一 directed standalone 场景：`tc=basicTest ts=memblock_sfence_flushpipe_directed_vseq`。
- quiescent provider 桥接、standalone cfg 校验、L2TLB request acceptance gate 和 strict single-outstanding tracking。

本轮不支持：

- snapshot-only 字段进入 sequence、主表构建、异常 directed 激励、pass/fault、terminal 或 L2TLB lookup key。
- `tlbCsr_priv_debug` 的 debug-mode PMP/PMA/权限差异建模。
- `sfence_bits_flushPipe` 进入 `dispatch_raw_sfence_t` 或 `decode_raw_sfence()`，也不实现年轻 uid kill、pipeline flush、epoch 回滚和 terminal 重收敛。
- branch predictor enable/control 字段进入 TLB lookup、权限或异常模型。
- 通过 `seq_csr_common` plus/cfg/user cfg 替代 runtime CSR 真值。
- 通过 fence child/vseq/driver 写 L2TLB gate，或新增跨 producer 的全局 admission 状态。

主要落点：

```text
mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_sfence_quiescent_provider.sv
mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_default_sequence.sv
mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_sfence_flushpipe_directed_sequence.sv
mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_sfence_flushpipe_directed_vseq.sv
mem_ut/ver/ut/memblock/env/src/memblock_env_cfg.sv
mem_ut/ver/ut/memblock/env/src/memblock_env.sv
mem_ut/ver/ut/memblock/tc/src/basicTest.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv
mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv
```

执行前必须确认当前 V2 RTL 权威输入存在：

```bash
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

## 2. 问题一：CSR/control 字段分类和 snapshot-only 链路不完整

### V2 问题

V2 `csr_ctrl_agent` 已能看到 `hd_misalign_ld_enable`、`hd_misalign_st_enable` 和 `tlbCsr_priv_debug`，但 raw CSR、payload change、runtime snapshot 和 snapshot copy 链路没有形成闭环。driver idle/default 还可能把两个 misalign enable 驱成 `0`，导致 monitor 保存的 runtime 真值与本轮默认边界不一致。

### 修改原因

这三个字段当前只需要保存真实 runtime 观测值。`hd_misalign_ld/st_enable` 是后续 misalign directed plan 的输入边界；`tlbCsr_priv_debug` 是后续 debug-mode 权限专项的输入边界。本轮如果让 sequence 或 pass/fault 消费它们，会提前引入未建模的异常语义。

### 修改方案与修改逻辑

将三个字段统一归类为 snapshot-only runtime CSR：

| 字段 | 默认值 | 本轮保存位置 | 本轮禁止消费位置 |
|---|---:|---|---|
| `hd_misalign_ld_enable` | `1` | raw CSR、runtime snapshot、copy snapshot | sequence、主表、misalign directed、pass/fault |
| `hd_misalign_st_enable` | `1` | raw CSR、runtime snapshot、copy snapshot | sequence、主表、misalign directed、pass/fault |
| `tlbCsr_priv_debug` / `priv_debug` | `0` | raw CSR、runtime snapshot、copy snapshot | 权限/异常 pass/fault、L2TLB lookup、debug-mode 行为建模 |

修改 `dispatch_raw_csr_t`、`make_empty_raw_csr()`、`raw_csr_payload_changed()`、`csr_ctrl_agent_agent_monitor::mon_data()`、`mmu_csr_runtime_state::reset/update_from_raw_csr/update_from_csr_ctrl/copy_from()`，让字段从 monitor 采样到 runtime snapshot 完整传递。

修改 `csr_ctrl_agent_agent_xaction` soft constraint 和 `csr_ctrl_agent_agent_driver::drive_idle(DRV_0)`，保持 DUT 默认看到 `1/1/0`。monitor 对三个字段做 X/Z 检查，禁止四态 interface 静默折叠成二态 bit。

不新增 plus、cfg、user cfg、preset 或 `seq_csr_common` getter。branch predictor enable 字段继续作为旁路观察项，不进入 runtime CSR。

### 文字伪代码

```text
构造 raw CSR 默认值：
  make_empty_raw_csr() 设置 hd_misalign_ld_enable=1；
  设置 hd_misalign_st_enable=1；
  设置 priv_debug=0；
  其它既有 CSR 字段保持原默认语义；

驱动 CSR 默认值：
  xaction soft constraint 默认 hd_misalign_ld/st_enable=1、priv_debug=0；
  drive_idle(DRV_0) 对同三个字段驱动 1/1/0；
  directed item 可以显式覆盖 soft 默认，但本 plan 不新增 testcase consumer；

采样 raw CSR：
  monitor 每拍采样三个 V2 interface 字段；
  对采样值做 X/Z 检查；
  raw.hd_misalign_ld_enable = sampled ld enable；
  raw.hd_misalign_st_enable = sampled st enable；
  raw.priv_debug = sampled tlbCsr_priv_debug；
  raw_csr_payload_changed() 比较这些字段；
  如果 payload 变化，沿现有 raw CSR queue 推送；

更新 runtime snapshot：
  apply_raw_csr_runtime() 调用 mmu_csr_runtime_state.update_from_raw_csr(raw)；
  update 只保存字段并返回 changed 结果；
  copy_from() 复制三个字段，保证 get_mmu_csr_snapshot() 不丢字段；

禁止消费：
  sequence、主表构建、异常生成、pass/fault、terminal 和 L2TLB lookup 不读取这三个字段；
  如果后续需要让字段影响激励或判断，停止并转入独立专项 plan。
```

## 3. 问题二：`sfence_bits_flushPipe` 缺少可执行的 V2 边界

### V2 问题

`sfence_bits_flushPipe` 是 V2 fence payload 的真实字段，但当前公共状态只实现 TLB entry invalidation，不实现 flush pipe 后对年轻 request、inflight miss、redirect/replay 和 terminal 的收敛。若业务 default sequence 随机出 `flushPipe=1`，测试框架会把未建模语义当作普通 sfence 驱动。

### 修改原因

Scala TLB 用 `sfence.valid && flushPipe` 区分会丢弃 pipe request 的 SFENCE 与不 flush pipe 的 Svinval。当前测试环境没有全局 pipeline flush owner，因此本轮只能把 `flushPipe=1` 收敛为独占 directed standalone 场景，并在驱动前确认环境静默。

### 修改方案与修改逻辑

默认业务路径固定 `flushPipe=0`：

- `fence_agent_agent_xaction` 增加 `soft flushPipe == 1'b0`，`psdisplay()` 打印字段。
- `fence_agent_agent_default_sequence::body()` 每笔随机 item 显式约束 `flushPipe == 0`。
- `fence_agent_agent_driver::drive_idle(DRV_0)` 继续驱 `0`。
- `fence_agent_agent_monitor::mon_data()` 每拍检查 `sfence_valid` X/Z；仅当 `valid===1` 时检查 payload，payload 包含 `flushPipe`；raw sfence 仍不保存 `flushPipe`。

`flushPipe=1` 只允许新增 directed 路径：

- `memblock_sfence_flushpipe_directed_sequence` 只生成一笔 `valid=1/flushPipe=1/pre_pkt_gap=0/post_pkt_gap=1` item。
- `memblock_sfence_flushpipe_directed_vseq` 是唯一正式 caller，只允许通过 `tc=basicTest ts=memblock_sfence_flushpipe_directed_vseq` 启动。
- `basicTest::build_phase()` 在创建、随机化和下发 env cfg 前解析 `VSEQ_MAIN`；精确命中该 vseq 时设置非 user 的 `sfence_flushpipe_standalone_mode=1`。
- `memblock_env::build_phase()` 在 `apply_user_cfg()` 后、创建任何子组件前检查最终 effective cfg 和 agent default-sequence 拓扑；失败在 main phase 前 `uvm_fatal`，不静默改配置。
- 顶层 vseq 只做无副作用 fail-fast 检查，不等待 drain、不清 queue/map/status、不调用 reset。V2 默认 `l2tlb_responder_active=1` 是 compile-time takeover 状态，不是 standalone 失败条件。

### 文字伪代码

```text
默认 fence 业务路径：
  xaction randomize 默认 flushPipe=0；
  fence default sequence 每笔 item 显式 randomize with flushPipe=0；
  drive_idle 把 flushPipe 驱0；
  monitor 先检查 valid；
  valid为1时检查 rs1/rs2/addr/id/hv/hg/flushPipe payload；
  raw sfence 只写既有 invalidation 字段，不写 flushPipe；

basicTest build_phase：
  先解析 VSEQ_MAIN；
  如果字符串精确等于 memblock_sfence_flushpipe_directed_vseq：
    在 cfg 下发和 env 创建前设置 sfence_flushpipe_standalone_mode=1；
  否则保持 mode=0；
  mode 不是 user cfg、plus 或 preset 字段；

memblock_env build_phase：
  apply_user_cfg() 后读取最终 cfg；
  mode=0 时直接继续原 build；
  mode=1 时检查 fence agent driver/sequencer 可用且 drv_mode=DRV_0；
  检查其它业务 agent 不存在显式 main_phase default_sequence，或 driver 明确关闭；
  允许 TCNT 空 fallback 启动，因为其 body 为空且不产生 item；
  任一 user override 破坏 standalone 拓扑时 fatal；

顶层 directed vseq：
  检查 p_sequencer 和 fence_sqr 非空；
  检查 common data 存在，main_trans_num/next_uid/main_table_ready 表示未构建业务表；
  检查 active map、issue/recovery/raw/redirect/flushSb 状态为空；
  检查 L2TLB request gate=0 且 outstanding count=0；
  不读取 takeover 作为失败条件；
  通过后只调用一次 child sequence；
  失败时 fatal，不等待、不清理、不重试。
```

## 4. 问题三：fence driver 无法直接查询 common data 静默状态

### V2 问题

`fence_agent` package 编译早于 `seq_pkg`，driver 不能直接引用 `common_data_transaction`。如果把 provider 注册放在 dispatch sequence 或 testcase connect 中，`basicTest` standalone 路径可能没有 provider，或者 main phase 并发启动时注册顺序不确定。

### 修改原因

`flushPipe=1` 的最终合法性必须在 driver 写 interface 前实时确认。这个检查必须覆盖所有创建 `memblock_env` 的 testcase，并且不能引入早编译 package 到晚编译 package 的反向依赖。

### 修改方案与修改逻辑

在 `memblock_sync_pkg.sv` 中定义 provider base、唯一 handle、register/query API。该文件是独立 compile unit，必须自行在 package 前 include `uvm_macros.svh`，在 package 内 import `uvm_pkg::*`，不依赖后续 package 的 UVM 可见性。

在 `seq_pkg.sv` 中紧跟 `common_data_transaction.sv` include 新增 `memblock_sfence_quiescent_provider.sv`。concrete provider 保存 live `common_data_transaction` handle，并把 query 实时转发给 `common_data_transaction::is_sfence_flushpipe_quiescent()`。

`memblock_env::connect_phase()` 是唯一 runtime owner：先保留原 analysis FIFO、RM/scoreboard、virtual sequencer 连接，再取得 common data singleton，创建并保存 provider 成员，调用 register。`tc_base`、`basicTest`、dispatch `pre_body()`、directed child/vseq 均不得注册 provider。

`fence_agent_agent_driver::send_pkt()` 在任何 interface 赋值前调用 `check_sfence_flushpipe_drive_allowed(tr)`。`valid=0` 或 `flushPipe=0` 直接通过；只有 `valid=1/flushPipe=1` 才 query provider。provider 未注册、common data 不可用或状态非静默都 fail closed。

`is_sfence_flushpipe_quiescent()` 先做 O(1) map/queue/counter/state 检查；只有这些全部通过后，才在显式 `flushPipe=1` 低频路径按 `main_trans_num` 做有界 `status_by_uid` 全表扫描。默认 `flushPipe=0` 路径不扫描。

### 文字伪代码

```text
memblock_sync_pkg：
  定义 sfence_quiescent_provider_base::is_quiescent(reason)；
  register(provider)：
    provider为空则 fatal；
    首次注册保存 handle；
    同一 handle 重复注册直接返回；
    不同 handle 冲突则 fatal；
  query(reason)：
    未注册时 reason="provider is not registered"，返回0；
    已注册时实时调用 provider.is_quiescent(reason)，不缓存结果；

memblock_env::connect_phase：
  调用 super 并保留原连接顺序；
  完成所有 analysis/vsqr 连接；
  data = common_data_transaction::get()；
  data 为空则 fatal；
  provider 成员为空时 new(data)；
  调用 memblock_sync_pkg::register_sfence_quiescent_provider(provider)；

fence driver send_pkt：
  在第一处 vif 赋值前调用 check_sfence_flushpipe_drive_allowed(tr)；
  tr为空则 fatal；
  valid=0 或 flushPipe=0 时返回；
  valid=1/flushPipe=1 时 query provider；
  query 返回 false 时 fatal，且不产生部分 interface 驱动；
  query 返回 true 后立即驱动 valid 和全部 payload；
  query 返回到第一处 vif 赋值之间不得插入 @/#/wait、日志或状态更新；

common_data_transaction::is_sfence_flushpipe_quiescent：
  先检查 active ROB/LQ/SQ map 是否为空；
  检查 load/sta/std issue queue、exception/PTW replay、raw event、flushSb、redirect/flush/freeze 状态；
  检查 l2tlb_ptw_outstanding_count==0；
  不读取 l2tlb_request_accept_enable、l2tlb_ptw_tracker_started 或 l2tlb_responder_active；
  O(1) 检查全通过后，确认 status_by_uid.size()==main_trans_num；
  仅此时扫描 uid=0..main_trans_num-1，要求 status 非空且 status.active=0；
  任一失败返回 false+首个 reason；
  全部通过返回 true；
  函数只读状态，不清 queue/map/counter，不推进 terminal。
```

## 5. 问题四：L2TLB request acceptance 与 outstanding 生命周期不受统一约束

### V2 问题

仅检查 quiescent 还不能保证 L2TLB responder 不会在 sequence 未准备好、response 正在处理或 idle-stop 过渡时继续把 `ready` 打开。旧 `ready_tr` 或多个 driver mode 也可能绕过统一 ready 控制，使第二笔 request fire 后没有 consumer。

### 修改原因

`flushPipe=1` standalone 需要确认当拍没有 L2TLB/PTW outstanding；正常 L2TLB responder 也必须保证 strict single-outstanding。acceptance gate、tracker started 和 outstanding count 必须各自表达不同语义，不能混成 cfg 或 reset 自动清理。

### 修改方案与修改逻辑

在 `memblock_sync_pkg` 新增 package 初值为 `0` 的 `l2tlb_request_accept_enable` 及 setter/query。唯一写者是 L2TLB driver reset 和 L2TLB base sequence 生命周期；fence driver、fence child/vseq、standalone cfg、common data reset 均不得写 gate。

在 `common_data_transaction` 维护 `l2tlb_ptw_outstanding_count` 和 `l2tlb_ptw_tracker_started`。count 严格只允许 `0/1`，不是可累加 counter。`reset_all_tables()`、sequence kill、reset、provider 注册或 testcase 切换都不得清 count 或打开 gate。

`L2tlb_agent_agent_driver` 新增唯一 ready 写点 `drive_l2tlb_request_ready(requested_ready)`。所有 `send_pkt()` 和 `drive_idle()` mode 都通过该 helper；helper 使用四态 `logic` 保留 X/RAND 语义，再统一应用 `l2tlb_responder_active && l2tlb_request_accept_enable` 和 current-fire 抑制。未知 driver mode 在任何 interface 写入前 fatal。

`memblock_l2tlb_base_sequence` 在 body 开始清 gate；完成 enable、context、takeover 和 tracker start 后才置 gate=1。每次 true `request_fire()` 后立即 sample payload、begin `0->1`、gate=0；response `finish_item()` 返回后 done `1->0`，只有非 stopping 才 re-arm gate。begin 后 fatal 不做自动 done/re-arm，保留 gate=0/count=1 便于定位。

### 文字伪代码

```text
L2TLB driver reset_phase：
  首先 set_l2tlb_request_accept(0)；
  再执行原 super/reset wait/idle/objection 流程；

ready helper：
  输入 requested_ready；
  current_fire = vif.valid && 当前ready；
  next_ready = requested_ready；
  如果 takeover未开启或 gate=0，则 next_ready=0；
  如果 current_fire=1，则 next_ready=0，防止连续接受第二笔；
  将 next_ready 作为唯一 ready 写入；
  所有 driver mode 恰好调用一次 helper；
  unknown mode 在写 interface 前 fatal 并返回；

L2TLB sequence body：
  入口 set_l2tlb_request_accept(0)，stopping=0；
  如果 MEMBLOCK_L2TLB_SEQ_EN=0，直接返回且 gate保持0；
  完成 context、takeover 和 tracker_start 检查；
  成功后 set_l2tlb_request_accept(1)；
  进入循环；

send_l2tlb_cycle：
  只有 true request_fire 分支处理请求；
  fire 后立即 sample request payload；
  调用 l2tlb_ptw_begin()，要求旧 count=0 并置1；
  立即 set_l2tlb_request_accept(0)；
  删除 ready_tr，不允许 fire 后再提交 ready=1 item；
  发送 response，finish_item 返回后调用 l2tlb_ptw_done()，要求旧 count=1 并置0；
  如果 stopping=0，重新 set_l2tlb_request_accept(1)；
  如果 stopping=1，不重新打开 gate；

关闭与失败：
  idle-stop 先置 stopping=1，再清 gate；
  只有 ready为0且 count=0 时 loop/body 返回；
  begin 后任何 fatal 不执行 finally-style 清理；
  count=1 留给后续 start/restart fatal，不能被 reset_all_tables 静默抵消。
```
