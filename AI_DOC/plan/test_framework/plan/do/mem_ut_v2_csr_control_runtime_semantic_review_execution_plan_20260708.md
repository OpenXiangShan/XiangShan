# mem_ut V2 CSR/control runtime 语义适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `do`，coding、验证和最终 review 已完成并归档 |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 主要入口 | `csr_ctrl_agent_agent_monitor::mon_data()`、`fence_agent_agent_xaction`、`fence_agent_agent_driver::send_pkt()`、`fence_agent_agent_monitor::mon_data()` |
| 适配原则 | snapshot-only 字段只进入 raw/runtime snapshot；`sfence_bits_flushPipe` 只做接口构造、透明驱动和观测采样，默认值为 `0`，取值 `0/1` 均不改变测试框架行为 |
| 创建/修订日期 | 2026-07-16 |

## 1. 范围与边界

### 1.1 专有名词与抽象功能说明

| 英文术语 | 本 plan 中的中文含义 | 对应代码对象/落点 | 典型场景 |
|---|---|---|---|
| `snapshot-only` | 只保存并复制、当前不参与行为判断的 runtime 字段 | `dispatch_raw_csr_t`、`mmu_csr_runtime_state` 中的 misalign enable 和 `priv_debug` | 字段由 `1/1/0` 变为 `0/1/1` 时更新快照，但不改变 TLB key 或 terminal |
| `latest snapshot` | 只保留最近一份 CSR raw 值，不按拍排队 | `memblock_sync_pkg::latest_raw_csr` | monitor 发现 payload 改变时覆盖旧快照并递增 `latest_raw_csr_seq` |
| `re-arm epoch` | 清空 latest 后要求 monitor 丢弃本地去重基线并重新发布首份快照的代号 | `raw_csr_rearm_epoch` | `reset_all_tables()` 前后 payload 相同，下一 monitor 边界仍发布完整 CSR snapshot |
| `semantic field` | 会改变翻译/权限上下文、应影响 runtime 语义版本的字段 | satp/vsatp/hgatp、既有 privilege/PBMT 字段 | 这些字段变化才递增 `mmu_csr_runtime_state::update_seq` |
| `valid gate` | 只有 valid 明确为 1 才消费关联 payload | `fence_agent_agent_monitor::mon_data()` | `sfence_valid` 为 0 或 X/Z 时不生成 raw sfence event |
| `transparent drive` | driver 不解释字段，仅把 transaction 原值写入 DUT interface | `fence_agent_agent_driver::send_pkt()` | directed `flushPipe=1` 直接驱动 1，不启动 standalone flush 状态机 |

### 1.2 重点函数的抽象功能

| 函数/task | 抽象功能描述 |
|---|---|
| `csr_ctrl_agent_agent_monitor::mon_data()` | 每拍采样 CSR interface；X/Z 诊断由 xz/reset/backend 条件控制，只有 capture 开启且 backend ready 时才在首次采样、re-arm 或 payload 变化时发布 latest raw snapshot；不直接更新主表或 status。 |
| `memblock_sync_pkg::clear_raw_monitor_queues()` | 清空 raw queue/latest CSR，并递增 re-arm epoch；不伪造 CSR event，也不修改 TLB entry。 |
| `common_data_transaction::apply_raw_csr_runtime()` | 按 latest snapshot 序号幂等更新公共 runtime mirror；不把 snapshot-only 字段转成异常或终态事件。 |
| `mmu_csr_runtime_state::update_from_raw_csr()` | 保存完整 CSR snapshot，并只用 semantic field 变化决定 `update_seq`；不生成 lookup key 之外的行为。 |
| `fence_agent_agent_monitor::mon_data()` | 按 `sfence_valid` 过滤有效 fence raw event，并对有效 payload 做 X/Z 诊断；不把 `flushPipe` 写入 raw sfence。 |

本 plan 只整理 V2 CSR/control runtime 语义适配需要解决的问题。每个问题均说明 V2 问题、修改原因、最终方案、修改的原有逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- `hd_misalign_ld_enable`、`hd_misalign_st_enable` 和 `tlbCsr_priv_debug` 的 monitor -> raw CSR -> `mmu_csr_runtime_state` snapshot 保存链路。
- 三个 snapshot-only 字段的 xaction soft 默认、driver idle 默认、monitor X/Z 检查和 runtime copy。
- raw/latest snapshot 清空后的 `raw_csr_rearm_epoch`，保证相同 payload 也会重新发布首份 runtime snapshot。
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

修改 `csr_ctrl_agent_agent_xaction` soft constraint 和 `csr_ctrl_agent_agent_driver::drive_idle(DRV_0)`，保持 DUT 默认看到 `1/1/0`。monitor 对三个字段做 X/Z 诊断。当前 `TCNT_CHECK_SIG_XZ` 只产生 `uvm_error`，不阻断 raw 发布；非法四态样本仍可能在二态 raw struct 中折叠，但 testcase 会带 UVM error 失败。本轮保持既有诊断策略，不新增 drop/fatal 分支。

`clear_raw_monitor_queues()` 清除 latest CSR 时递增 `raw_csr_rearm_epoch`。CSR monitor 记录最近看到的 epoch；epoch 变化时清除 `has_last_raw_csr` 和本地 baseline，确保 `reset_all_tables()` 即使保持 capture enable 为 1、CSR payload 也未变化，下一 monitor 边界仍会重新发布完整 snapshot。

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
  DRV_LST 按通用 agent 的收尾/全零模式驱动 0/0/0，是 DRV_0 公共默认的显式例外；
  directed item 可以显式覆盖 soft 默认，但本 plan 不新增 testcase consumer；

采样 raw CSR：
  monitor 每拍采样三个 V2 interface 字段；
  对采样值做 X/Z 诊断；诊断报错不作为 raw 发布 gate；
  raw.hd_misalign_ld_enable = sampled ld enable；
  raw.hd_misalign_st_enable = sampled st enable；
  raw.priv_debug = sampled tlbCsr_priv_debug；
  raw_csr_payload_changed() 比较这些字段；
  如果 payload 变化，沿现有 latest CSR snapshot 发布路径更新全局 snapshot；

更新 runtime snapshot：
  apply_raw_csr_runtime() 调用 mmu_csr_runtime_state.update_from_raw_csr(raw)；
  update 是 void 函数：保存字段并在内部计算 changed；
  changed 为真时递增 update_seq，调用者不接收返回值；
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

本轮验收口径：
  静态核对 xaction soft 默认、driver 原值透传、monitor 采样和 custom compare 的字段链；
  检查 raw_sfence 类型、decode_raw_sfence() 和 apply_sfence_invalidate() 仍无 flushPipe 字段或行为分支；
  检查 flushPipe=1 的实现方案不要求 standalone、provider、quiescent、redirect、LSQ 或 L2TLB gate；
  完成静态检查后运行 V2 远端 compile 和基础 smoke，确认现有公共环境无 UVM_ERROR/UVM_FATAL。

本轮不执行独立 directed fence testcase：不动态发送并断言 flushPipe=0/1 的 interface/monitor round-trip，
也不新增重复 reset 或 snapshot-only 字段切换 testcase；这些场景保留为后续专项的 residual coverage，不能把
`tc_sanity` 描述成已经覆盖。若后续需要该动态验收，应在独立 testcase/专项 plan 中补充。
```

## 4. 修改类型与主体逻辑对比

| 修改项 | 修改前逻辑 | 修改后逻辑 | 修改类型 | 是否改变主体控制行为 |
|---|---|---|---|---|
| CSR transaction 默认值 | 三个字段已存在，但无确定 soft 默认；`DRV_0` 把两个 misalign enable 驱成 `0` | xaction 默认 `1/1/0`，`DRV_0` 同步驱动 `1/1/0`，其它 idle mode 显式赋值 | 字段默认值适配 | 否 |
| CSR raw/runtime snapshot | monitor 只采样 interface 局部变量，raw/runtime/copy 不保存 | raw latest snapshot、payload change、runtime update/copy 完整保存三个字段 | 新增 snapshot 功能 | 仅新增观测状态，不进入主表或行为判断 |
| CSR reset 后首份 snapshot | `clear_raw_monitor_queues()` 清全局 latest，但 monitor 本地去重基线可能仍有效 | clear 时递增 `raw_csr_rearm_epoch`，monitor 见 epoch 变化后强制重发首份 snapshot | reset 生命周期修复 | 不改变正常 payload change 行为 |
| `update_seq` | 只统计现有翻译/权限语义字段变化 | 继续只统计现有语义字段；snapshot-only 字段单独变化只更新快照 | 语义边界澄清 | 否，不改变 TLB 生命周期 |
| CSR transaction 保真 | UVM field macro 有字段，但手工 `psdisplay()`/`compare()` 漏字段 | debug 文本和手工 compare 覆盖三个字段 | 字段保真补齐 | 否 |
| fence `flushPipe` | 字段可驱动，但随机默认不确定、debug/compare/XZ 链不完整 | 默认 `0`、原值透明驱动、有效 payload 才做 X/Z、debug/compare 可见 | 接口字段适配 | 否 |
| sfence 行为 | raw sfence 只携带 rs1/rs2/addr/id/hv/hg 并执行 entry invalidation | 完全保持原逻辑，raw/decode/invalidate 不增加 `flushPipe` | 保持不变 | 否 |

主体逻辑保持不变：主表生成、misalign directed 激励、PMP/PMA/debug 权限、pass/fault/terminal、
redirect/replay、LSQ admission、L2TLB request/response 和 sfence entry invalidation 均未新增读取分支。

## 5. 执行中补充/修正（IMPLEMENTATION_DELTA）

### 5.1 CSR transaction 手工保真补齐

[IMPLEMENTATION_DELTA]

- 来源：coding 中核对当前源码发现三个字段虽然已注册 UVM field macro，但手工 `psdisplay()` 和
  `compare()` 没有覆盖。
- 原 plan：只明确要求 CSR xaction soft 默认和 driver/monitor/raw/runtime 链路。
- 实现调整：同时把三个字段加入 CSR transaction 的 debug 展示和手工 compare。
- 原因：当前 custom compare 在 `super.compare()` 失败后会重新执行手工逐字段比较；若手工列表漏字段，
  仅这三个字段不同时可能被重新判为相等。
- 影响范围：只影响 transaction 调试和比较保真，不新增行为 consumer。

### 5.2 snapshot 更新与语义版本分离

[IMPLEMENTATION_DELTA]

- 来源：coding 中需要明确 snapshot-only 字段变化是否递增 `mmu_csr_runtime_state::update_seq`。
- 原 plan：要求 raw payload change 能观察三个字段变化，但未明确 `update_seq` 是否统计这些变化。
- 实现调整：`raw_csr_payload_changed()` 比较三个字段并发布新 latest snapshot；`update_from_*()` 保存字段，
  但 `changed`/`update_seq` 继续只统计既有翻译和权限语义字段。
- 原因：三个字段本轮没有 TLB key、权限、异常或 pass/fault consumer，不能让纯观测字段变化伪装成
  TLB runtime 语义版本变化。
- 影响范围：runtime snapshot 可以实时读取新值，但 TLB key 和当前行为不变。

### 5.3 fence valid gate 的四态边界补充

[IMPLEMENTATION_DELTA]

- 来源：coding 后复查 `fence_agent_agent_monitor::mon_data()` 时，需要明确 valid 为 X/Z 时不能生成 raw sfence。
- 原 plan：只要求 valid=1 时检查 payload，并保持 raw sfence 字段集合不变。
- 实现调整：raw sfence 入队条件使用 `io_ooo_to_mem_sfence_valid===1'b1`；只有 case-equality 明确为 1 才构造 raw event。
- 原因：普通 `==` 在四态 valid 下可能形成不明确的条件结果；case equality 能保证非法 valid 不会静默生成 raw event。
- 影响范围：合法 0/1 行为不变，只收紧 X/Z 输入边界；不增加 flushPipe consumer 或 pipeline flush 行为。

### 5.4 latest CSR reset re-arm 闭环

[IMPLEMENTATION_DELTA]

- 来源：首轮 subagent review 发现 `reset_all_tables()` 清空全局 latest CSR 后，monitor 本地 `has_last_raw_csr` 可能仍为 1。
- 原 plan：只覆盖 capture enable 变化和 DUT reset 对本地去重基线的清理，未覆盖 capture 保持 1 的软件表重置。
- 实现调整：`clear_raw_monitor_queues()` 递增 `raw_csr_rearm_epoch`；monitor 跟踪该 epoch，变化时清除 `has_last_raw_csr` 和 `last_raw_csr`。
- 原因：如果 reset 前后 CSR payload 相同，只有 generation/re-arm sideband 能明确要求 monitor 重新发布首份 snapshot。
- 影响范围：只影响 raw/latest CSR reset 生命周期；不会清 TLB entry、修改 lookup key 或增加 CSR FIFO。

### 5.5 X/Z 诊断语义澄清

[IMPLEMENTATION_DELTA]

- 来源：首轮 subagent review 核对 `TCNT_CHECK_SIG_XZ` 宏，确认它只报告 `uvm_error`，不会自动 drop/fatal。
- 原 plan：部分文字把 X/Z 检查描述成 raw 发布前的阻断条件。
- 实现调整：保持既有 error-only 诊断逻辑，文档明确非法 payload 仍可能折叠到二态 raw；含 UVM error 的 testcase 仍判失败。
- 原因：本 plan 不改变公共 monitor X/Z 策略，也不为这三个字段单独建立第二套 drop/fatal 行为。
- 影响范围：源码行为不变，只修正文档契约；`sfence_valid===1'b1` 仍是 raw sfence 的事件 gate。

### 5.6 active web callgraph 摘要同步

[IMPLEMENTATION_DELTA]

- 来源：末轮 review 检查到 `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md` 仍把
  `clear_raw_monitor_queues()` 描述成只清队列，也把 `update_from_raw_csr()` 简化成任意变化都递增
  `update_seq`。
- 原 plan：只明确同步 flow/analysis 文档，没有列出 active web callgraph 的摘要更新。
- 实现调整：同步网页函数表中的 re-arm epoch、snapshot-only 字段和 semantic `update_seq` 说明，并删除“L2TLB lookup 依赖 `update_seq`”的旧表述；lookup key 明确只使用 `vpn/asid/vmid/s2xlate`。
- 同步补充：修正 `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_l2tlb_base_sequence.md` 的 runtime CSR 表，将 `update_seq` 标为语义追踪值，只把 ASID/VMID 列为 lookup key 的 CSR 部分。
- 同步补充：修正 `AI_DOC/plan/test_framework/plan/undo/mem_ut_test_framework_todo_20260614.md` 的 S1/S2 TODO，明确 `csr_update_seq` 不属于 lookup key，仅用于 runtime 语义变化追踪。
- 原因：网页 callgraph 是当前有效的控制流入口，继续保留旧摘要会与源码和本 plan 冲突。
- 影响范围：仅文档说明，不增加源码 consumer、状态字段或控制分支。

### 5.7 flow 文档结构和四态语义同步

[IMPLEMENTATION_DELTA]

- 来源：末轮 review 按最新 flow 文档规则检查时发现，CSR/sfence flow 的首章缺少术语表，部分函数章节缺少独立的抽象功能描述，sfence 片段还把 `valid===1'b1` 简化成了普通比较。
- 原 plan：要求同步 flow 文档，但未逐项列出文档结构和四态 gate 的写法。
- 实现调整：为两个 flow 增加首章术语/抽象功能表，为源码章节补充抽象功能描述，并把 sfence monitor 摘要改为真实的 `===1'b1` gate；同时明确 monitor 连续采样、X/Z 诊断和 capture 发布是三个独立条件。
- 原因：避免 flow 文档把近似伪代码误读成源码，或把 capture 错写成采样/XZ 诊断的前置条件。
- 影响范围：只修正文档可读性和语义准确性，不改变任何测试框架源码行为。

### 5.8 active L2TLB 文档调用链和 key owner 纠正

[IMPLEMENTATION_DELTA]

- 来源：第三轮 subagent review 对 active web callgraph 和 CSR runtime analysis 进行源码反查时发现，网页仍保留不存在的 `uid_by_tlb_key`、把 L2TLB 列为 `main_table_ready` 等待者，CSR analysis 仍把 runtime CSR 描述成“查到哪个 uid”。源码证据为 `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:26-27,1585-1601,1798-1828` 和 `mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv:58-74`。
- 原 plan：只要求同步 `update_seq` 不参与 lookup key，没有逐项审计 active 文档中的 key owner、main-table 前置条件和 uid record 回填链路。
- 实现调整：web 只保留真实的 `tlb_entry_by_key` 与 `uid_tlb_record_by_uid`；明确 L2TLB responder 不等待 `main_table_ready`；CSR analysis 改为“runtime CSR 参与 live TLB entry 查/建，uid record 由公共数据层按 key 扫描回填”。
- 原因：避免把不存在的 key->uid 强绑定或错误的主表依赖重新引入 L2TLB responder 语义。
- 影响范围：仅修正 `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md:1151-1164`、`AI_DOC/analysis/source_sv/dispatch_framework_sv/mmu_csr_runtime_state.md:9-11`、`AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_l2tlb_base_sequence.md:64-71` 及 review 文档，不增加源码索引、主表等待或新的 responder 状态机。

## 6. 执行结果与归档状态

本子计划 coding 已完成，实际修改覆盖：

- CSR transaction 的默认值、`psdisplay()`、手工 `compare()` 和五种 idle drive mode 的字段保真；
- `dispatch_raw_csr_t`、latest snapshot 发布、`mmu_csr_runtime_state` reset/update/copy 的三个字段链路；
- `raw_csr_rearm_epoch` 和 monitor 去重基线 re-arm，闭环软件表 reset 后首份 snapshot；
- fence transaction 的 `flushPipe` 默认值、透明驱动、有效 payload X/Z 检查和 debug/compare 展示；
- flow、源码分析、TODO 和本 plan 的边界同步。

验证结果：

- `git diff --check`：通过；
- re-arm 修复后的 V2 远端 VCS compile/elaboration/link：通过，退出码 0；
- `make eda_batch_run tc=tc_sanity mode=base_fun`：通过，`TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`、退出码 0；
- smoke 日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=tc_sanity_ts=virtual_base_sequence_cfg=default_seed=666666_rtl_.log`。

review 文档：
`AI_DOC/plan/test_framework/review_doc/undo/mem_ut_v2_csr_control_runtime_semantic_review_implementation_review_20260722.md`。

本轮仍明确保留的边界：snapshot-only 字段不参与异常激励、权限判断、pass/fail、terminal 或 TLB key；
`flushPipe` 不进入 raw sfence 和 standalone 全局 pipeline flush。monitor analysis port 输出仍由 monitor
output 专项负责，本子计划只处理 raw/latest snapshot 和接口字段保真。

专项 directed 覆盖边界：本轮不新增独立 testcase 去动态断言 `flushPipe=1`、snapshot-only 字段切换或
`copy_from()`；最终验证采用字段链静态审计、全量 compile 和基础 smoke。该覆盖缺口记录为 residual risk，
不把 `tc_sanity` 描述成这些 directed 场景已经命中。最终独立 reviewer `Archimedes`
（agent `019f8961-378e-7320-87f9-2c2218acbb71`）已明确输出 `FINAL PASS`，本 agent 完成独立复核。
