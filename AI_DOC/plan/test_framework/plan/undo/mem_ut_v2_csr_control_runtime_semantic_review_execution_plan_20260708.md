# mem_ut V2 CSR/control runtime 语义适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 主要入口 | `csr_ctrl_agent_agent_monitor::mon_data()`、`fence_agent_agent_xaction`、`fence_agent_agent_driver::send_pkt()`、`fence_agent_agent_monitor::mon_data()` |
| 适配原则 | snapshot-only 字段只进入 raw/runtime snapshot；`sfence_bits_flushPipe` 只做接口构造、透明驱动和观测采样，默认值为 `0`，取值 `0/1` 均不改变测试框架行为 |
| 创建/修订日期 | 2026-07-16 |

## 1. 范围与边界

本 plan 只整理 V2 CSR/control runtime 语义适配需要解决的问题。每个问题均说明 V2 问题、修改原因、最终方案、修改的原有逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- `hd_misalign_ld_enable`、`hd_misalign_st_enable` 和 `tlbCsr_priv_debug` 的 monitor -> raw CSR -> `mmu_csr_runtime_state` snapshot 保存链路。
- 三个 snapshot-only 字段的 xaction soft 默认、driver idle 默认、monitor X/Z 检查和 runtime copy。
- `sfence_bits_flushPipe` 的 xaction 默认 `0`、transaction/debug 展示、driver 原值透明驱动和 monitor payload X/Z 检查。
- `flushPipe=1` 允许由 fence transaction 的 directed item 显式覆盖默认值后直接驱动 DUT，不增加 standalone 场景或运行期合法性 gate。

本轮不支持：

- snapshot-only 字段进入 sequence、主表构建、异常 directed 激励、pass/fault、terminal 或 L2TLB lookup key。
- `tlbCsr_priv_debug` 的 debug-mode PMP/PMA/权限差异建模。
- `sfence_bits_flushPipe` 进入 `dispatch_raw_sfence_t`、`decode_raw_sfence()` 或任何行为状态；也不实现年轻 uid kill、pipeline flush、epoch 回滚、terminal 重收敛、quiescent 检查或保护窗口。
- branch predictor enable/control 字段进入 TLB lookup、权限或异常模型。
- 通过 `seq_csr_common` plus/cfg/user cfg 替代 runtime CSR 真值。
- 因 `sfence_bits_flushPipe` 修改 L2TLB responder、request gate、outstanding tracker、redirect、LSQ admission 或跨 producer 全局状态。

主要落点：

```text
mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/mmu_csr_runtime_state.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv
mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_monitor.sv
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

## 3. 问题二：`sfence_bits_flushPipe` 接口默认值和观测链不完整

### V2 问题

`sfence_bits_flushPipe` 是 V2 fence payload 的真实字段，interface、xaction 和 driver 已有同名字段，
但 xaction 没有默认 `0` 约束，`psdisplay()` 和 custom `compare()` 没有覆盖该位，monitor 虽然读取了 interface，
却没有在 `sfence_valid=1` 时对该 payload 做 X/Z 检查。默认 sequence 直接使用通用随机化时，
该位可能无意随机为 `1`，不符合当前业务默认值约定。

### 修改原因

V2 MemBlock 的 load/store/prefetch DTLB 均为 non-blocking DTLB，当前生成 RTL 不使用
`sfence_bits_flushPipe` 决定 DTLB hit/miss；当前 UVM L2TLB responder 和软件 TLB 失效流程也不读取
该位。TLB entry invalidation 仍由 `sfence_valid` 和现有 rs1/rs2/addr/id/hv/hg payload 决定。

因此该字段只需要完成接口构造、默认值、透明驱动和观测采样，不需要新增 pipeline flush owner、
standalone vseq、quiescent provider、保护窗口、redirect/LSQ gate 或 L2TLB 生命周期约束。

### 修改方案与修改逻辑

`fence_agent_agent_xaction` 新增
`default_io_ooo_to_mem_sfence_bits_flushPipe_cons`，使用 soft constraint 把默认值设为 `0`，
作为默认值唯一权威；现有
`fence_agent_agent_default_sequence` 继续使用通用 `uvm_do(req)`，不重复增加第二个 hard constraint。
directed sequence 后续需要驱动 `1` 时，可用 inline constraint 或直接赋值覆盖 soft 默认，不需要新增
专用 testcase、vseq、cfg 或 plus 参数。

`fence_agent_agent_driver::send_pkt()` 保持现有透明驱动：transaction 是 `0` 就驱 `0`，是 `1` 就驱
`1`，不查询 common data，不判断环境静默，也不修改任何全局状态。`drive_idle(DRV_0)` 继续驱 `0`。

`fence_agent_agent_monitor::mon_data()` 保持每拍采样该字段；先检查 `sfence_valid`，仅当
`valid===1'b1` 时把 `flushPipe` 与 rs1/rs2/addr/id/hv/hg 一起做 payload X/Z 检查。
`dispatch_raw_sfence_t` 和 `memblock_sfence_payload_t` 不增加该字段，`push_raw_sfence()`、
`decode_raw_sfence()`、`apply_sfence_invalidate()` 保持原行为，因此采样值只服务接口观测和 debug，
不进入软件 TLB、L2TLB lookup 或其他行为判断。

`fence_agent_agent_xaction::psdisplay()` 增加该字段，保证 driver item 日志能看到实际驱动值。
custom `compare()` 在现有逐字段比较中增加 `flushPipe`，避免两个 transaction 仅该位不同时被误判相等；
这只补齐 transaction 保真，不新增 coverage、checker 或 pass/fail 逻辑。

### 文字伪代码

```text
xaction 构造：
  保留 rand bit io_ooo_to_mem_sfence_bits_flushPipe；
  声明并实现 default_io_ooo_to_mem_sfence_bits_flushPipe_cons；
  约束内容为 soft io_ooo_to_mem_sfence_bits_flushPipe == 1'b0；
  psdisplay() 打印该字段；
  custom compare() 比较该字段，不等时返回compare失败并打印双方值；
  普通 default sequence 复用 soft 默认得到0；
  directed item优先在randomize()中使用hard inline constraint覆盖为1；
  如果使用显式赋值，则必须在randomize成功后、finish_item()前赋1，后续不得再次randomize覆盖；

driver 驱动：
  send_pkt(tr) 按 tr.io_ooo_to_mem_sfence_bits_flushPipe 原值驱动 DUT；
  不因取值为1查询 provider、等待 drain、检查 quiescent 或设置全局 flush；
  drive_idle(DRV_0) 继续把该位驱0；

monitor 采样：
  每拍读取 sfence_valid 和全部 payload；
  先对 sfence_valid 做 X/Z 检查；
  如果 sfence_valid===1'b1：
    对 rs1/rs2/addr/id/hv/hg/flushPipe 做 X/Z 检查；
    构造 raw_sfence 时仍只保存 valid/rs1/rs2/addr/id/hv/hg/cycle；
    push_raw_sfence() 继续把事件交给既有 TLB entry invalidation flow；
  如果 sfence_valid不是1：
    不消费 payload，不产生 raw sfence；

行为保持：
  flushPipe为0或1都不修改测试框架 queue、map、status、epoch、redirect、LSQ或L2TLB状态；
  在sfence_valid和其它payload相同的前提下，只切换flushPipe不改变DTLB hit/miss结果；
  DTLB hit继续按hit处理，DTLB miss继续进入现有L2TLB responder流程；
  sfence_valid仍是软件TLB失效事件的唯一入口。

最小验证：
  默认创建并randomize一笔fence item，检查flushPipe=0；
  通过hard inline constraint randomize第二笔item为flushPipe=1；
  分别发送两笔item，检查driver interface和monitor采样值与item一致；
  构造仅flushPipe不同的两个xaction，检查custom compare返回不相等；
  检查raw_sfence类型、decode_raw_sfence()和apply_sfence_invalidate()仍无flushPipe字段或分支；
  检查flushPipe=1路径不要求standalone、provider、quiescent、redirect、LSQ或L2TLB gate；
  完成静态检查后运行V2远端compile和基础smoke，确认0/1透传不引入UVM_ERROR/UVM_FATAL。
```
