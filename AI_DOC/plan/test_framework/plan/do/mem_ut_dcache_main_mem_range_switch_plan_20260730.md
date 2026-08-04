# DCache `main_mem` 范围开关与共享写覆盖层专项 Plan

## 修改目标

为 DCache 轻量 L2 responder 增加 runtime plus 参数
`MEMBLOCK_MAIN_MEM_RANGES_EN`，在 `seq_csr_common` 中保存为
`main_mem_ranges_en`。该参数默认值为 `1`。同时把当前 sequence 实例内的稀疏主存和覆盖层
统一为测试级共享 memory store。

本修改控制 memory-facing responder 访问稀疏主存时是否启用 `main_mem_ranges` 物理地址范围检查，
并使 DCache coherent port 和 Uncache TL-UL port 使用同一份 backing memory 与写覆盖层。当前
Uncache responder 的代码仍位于历史命名的 `sbuffer_agent`，但本 plan 一律按 Uncache 端口语义描述；
不需要新建第二个 Uncache agent。SBuffer 到 DCache 是内部连接，不是独立外部 memory channel。
本修改不改变 DCache TileLink 协议、TLB 建模、主表虚拟地址生成或 `cached_alias_by_line` 的职责。

## 新增功能

| 参数值 | memory responder 行为 |
|---:|---|
| `1`，默认 | 启动时把 `MEMBLOCK_PADDR_BASE/RANGE` 注册为 `main_mem_ranges`；DCache A/C 和 Uncache A 物理地址访问必须位于该窗口，完整 64B line 越界保持现有 fatal 行为。 |
| `0` | DCache 与 Uncache 均不注册 `main_mem_ranges`；所有能由对应物理地址接口表达、且满足既有协议对齐要求的地址均可访问共享 `main_mem`。首次访问仍按 1KB backing line 懒分配并填入确定性初值。 |

共享 memory store 保留两层已有结构：

| 数据层 | 行为 |
|---|---|
| `main_mem` | backing memory。仅保存确定性懒初始化数据，不接收 DUT memory-facing write 数据覆盖；现有 helper 可在首次读或首次写的访问预检时建立该数据。 |
| `write_overlay_mem` 与 byte valid | 写覆盖层。DCache C-channel data writeback 和 Uncache store 更新此层；读时逐 byte 优先命中此层，未命中时回退 `main_mem`。 |

## 修改原因

`main_mem` 是关联数组式稀疏主存，首次访问 line 时自动建立数据，因此固定
`MEMBLOCK_PADDR_BASE/RANGE` 不是其分配或读写的必要条件。关闭范围检查后，随机或 directed
场景可以访问 DCache 物理地址接口可表达的任意地址，而不会因测试框架人为主存窗口而失败。

默认继续对 DCache 和 Uncache 开启该检查，用于发现 TLB 映射、物理地址拼接或 DCache A/C、Uncache
地址不一致的问题。对原先 Uncache 可访问范围外 sparse memory 的场景，这是有意的默认行为变化；需要
全物理位宽懒分配时必须显式设置 `MEMBLOCK_MAIN_MEM_RANGES_EN=0`。

当前 `main_mem`、`prog_mem` 和 byte valid 属于各自 sequence 实例。DCache responder 与
Uncache responder 独立启动，不能天然观察同一份写数据。将现有结构提升为唯一 shared memory
store 后，两个实际 memory-facing port 才能使用同一个原始 backing 数据和写覆盖视图。

## 修改后逻辑

```text
启动测试框架 memory store：
  清空 backing memory、写覆盖层和 byte valid；
  若 main_mem_ranges_en == 1：
    注册 paddr_base .. paddr_base + paddr_range - 1；
  若 main_mem_ranges_en == 0：
    保持 range 未配置；

处理 DCache coherent port 或 Uncache port 的读写：
  保留现有地址位宽、64B line 对齐和 TileLink 字段检查；
  读：在真实 A.fire 的采样边界先取 byte-valid 覆盖层，未命中 byte 再读取 backing main_mem，
      并将合并结果固化到对应 GrantData/AccessAckData response record；
  写：先复用现有 backing read 完成预检和懒初始化，再只更新覆盖层数据和对应 byte valid；
      不用 DUT 写数据覆盖 backing main_mem；
  range 已配置时按现有规则检查每个有效 byte；
  range 未配置时跳过窗口检查，按访问地址懒分配/读取 backing main_mem。
```

唯一 memory lifecycle owner 是启动后台 memory responder 的 virtual sequence。该 sequence 启动时，
在 fork DCache 和 Uncache responder 之前完成 shared memory store 的清空和范围配置；
各 responder 启动、reset、replacement、Probe、CBO 或退出时均不得清空 backing 或写覆盖层。

写覆盖层的更新边界如下：DCache `ReleaseData` 与 `ProbeAckData` 均仅在完整两个 C beat 完成握手、且
所有 data beat `corrupt=0` 后更新；任一 data beat `corrupt=1` 时只完成协议收敛，不更新 overlay，
也不在本专项修改 alias `data_valid`。Uncache port 仅在 store A-channel 真实握手后更新。store issue、
DCache dirty 标记、未握手的 C valid、无数据 `ProbeAck` 均不得更新覆盖层。

### 同一采样边界的读写可见性

共享 memory store 以统一的测试框架采样边界提交两个 responder 的 memory event，避免依赖两个
sequence 的执行先后或 SystemVerilog delta-cycle 顺序：

```text
采样边界开始：
  固定本轮 read view = 上一轮已经提交的 main_mem + write_overlay_mem；
  DCache C writeback 完整收敛事件、Uncache store A.fire 事件进入本轮 write batch；
  DCache 需要数据的 AcquireBlock A.fire 使用本轮 read view 建立 GrantData 快照；
  Uncache load A.fire 使用本轮 read view 建立 AccessAckData 快照；
  两类读都不读取本轮新写入，也不能在 response delay 期间重新读取 live overlay。

采样边界结束：
  先按 DCache C writeback 事件更新 write_overlay_mem；
  再按 Uncache store 事件更新 write_overlay_mem；
  同一 byte 同拍同时被两侧写入时，Uncache store 的值覆盖 DCache writeback 的值；
  下一采样边界的 read view 才能看到本轮写入。
```

该顺序只解决共享软件 store 的确定性，不改变 DUT 的 A/B/C/D/E 握手；DCache/Uncache 的 response
delay 仍由各自 response pipeline 管理。各通道内部同拍存在多个写事件时，沿用该通道已确认的 fire
顺序提交，不能用关联数组遍历顺序作为隐含排序规则。

## 接入范围

- DCache `GrantData` 从 merged view 读取数据；完整且全部 data beat `corrupt=0` 的 `ReleaseData`
  与 `ProbeAckData` 写入 overlay。任一 data beat `corrupt=1` 时，完整 C assembly 仍按原协议收敛，
  但不得写入 overlay；本专项不修改 alias `data_valid`，该字段及其生命周期由后续 alias state 专项负责。
- Uncache load 从 merged view 读取数据；Uncache store 在 A-channel fire 后写入 overlay。当前该端口
  的 agent/sequence 文件仍沿用 `sbuffer_agent` 名称，coding 时只迁移其 memory access helper，不重命名
  agent、interface、connect 或 UVM topology。
- DCache `AcquireBlock` 的 GrantData 也从 A.fire 时固化的 merged view 快照生成；`AcquirePerm`、CBOAck
  等无数据 response 不建立 data read snapshot。
- 历史名为 `sbuffer_agent` 的顶层端口实际承担 Uncache TL-UL A-to-D responder；它有独立的 shared
  memory wrapper，Uncache load/store 分别从 merged view 读取/向 overlay 写入。cacheable store 的外部
  写回仍由 DCache C-channel 路径记录；本专项不重命名 agent、interface、connect 或 UVM topology。

## 参数与作用域

- 参数链路：`env/plus.sv -> seq_csr_common::load_from_plus() -> getter -> shared memory store`。
- `MEMBLOCK_PADDR_BASE/RANGE` 保持为 TLB map builder 的物理映射窗口；关闭
  `MEMBLOCK_MAIN_MEM_RANGES_EN` 不改变其值、TLB entry 内容或其他 consumer。
- 现有 `main_mem`、`prog_mem`、`prog_mem_byte_valid` 和逐 byte merge 行为作为实现基础；覆盖层可
  统一改名为 `write_overlay_mem` 和对应 valid，避免与程序镜像语义混淆。
- DCache 和 Uncache responder 只保留薄访问 wrapper，全部委托同一 shared memory store；不得再各自
  保存私有 memory copy。
- shared memory store 的清空只由 memory lifecycle owner sequence 在每个 testcase 启动时执行一次；
  任何 responder sequence 不得在自身 `body()` 启动时重复清空。
- `cached_alias_by_line` 继续是 DCache 私有的 Probe 候选表。cache replacement、Probe、CBO 或 alias
  表项删除均不得清除写覆盖层。

## 预期影响

- 默认 `MEMBLOCK_MAIN_MEM_RANGES_EN=1` 同时限制 DCache 与 Uncache；这是对原 Uncache 范围行为的有意适配变化。
- 关闭开关后，范围外地址不再因 responder 的主存窗口检查产生 `denied` 或 DCache line-range fatal。
- DCache writeback 和 Uncache store 的已完成写不再污染 backing `main_mem`，后续读统一看到 overlay
  优先、backing 回退的合并数据。
- 首次写是否建立 backing line 维持现有 helper 行为；本专项不为“仅 read miss 才初始化”新增控制分支。
- DUT/测试框架其他地址、权限、异常检查仍可独立报错；该开关不把协议非法或字段截断的地址变为合法。
- 严格地址映射测试应显式保持 `MEMBLOCK_MAIN_MEM_RANGES_EN=1`；仅需要稀疏物理地址数据服务的
  smoke 或 directed testcase 可设为 `0`。

## 与原测试框架逻辑对比和修改类型总结

原逻辑由 DCache 和 Uncache responder 分别访问各自 sequence 内的 memory 状态，Uncache 可能绕过
`main_mem_ranges`，同拍跨通道写读的结果依赖 sequence 执行时序。

修改后统一为：

```text
DCache/Uncache -> shared memory store
同拍读 -> 读取上一轮 committed view
同拍写 -> DCache C writeback 先提交，Uncache store 后提交
下一轮读 -> 看到本轮 overlay 更新
```

本次包含两类变化：`main_mem_ranges_en` 同时扩展到 Uncache 的范围约束，以及共享 store 的同拍
读写提交顺序；`main_mem`/overlay 的物理 line、byte-valid 和懒分配职责保持不变。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### 1. 每个 responder 时钟边界推进 shared write batch

`[IMPLEMENTATION_DELTA]`

- 来源：coding review 发现只在 `shared_mem_access_task()` 中推进 sample 时，最后一次已确认的
  DCache C writeback 或 Uncache store 若后续没有任何 memory access，将一直滞留在 batch。
- 原 plan：定义“下一采样边界”提交写，但没有指定空闲拍由哪个调用点推进该边界。
- 实现调整：DCache 和 Uncache responder 的每个 `drv_cb` 循环在采样 DUT 信号前均调用
  `begin_shared_mem_sample($time)`；该 helper 对同一个 `$time` 幂等，时间推进时固定先提交
  DCache batch、再提交 Uncache batch。
- 原因：保证最后一笔真实完成写在下一拍也能对后续读、drain 和终态检查可见，同时不改变 A/B/C/D/E
  handshake、response delay 或 responder 仲裁。
- 影响范围：`mem_base_sequence.sv`、DCache/SBuffer memory responder flow、源码分析和实现 review。

```text
每个 DCache 或 Uncache drv_cb：
  begin_shared_mem_sample($time)；
  若 time 与上一 sample 不同：
    先提交 DCache write batch；
    再提交 Uncache write batch；
  再采样本拍 handshake，并把本拍新确认的写加入新 batch；
```

### 2. legacy topology 的惰性 lifecycle 初始化

`[IMPLEMENTATION_DELTA]`

- 来源：现有 testcase 仍可通过 agent default sequence 启动 responder，不一定经过 real-smoke vseq。
- 原 plan：virtual sequence 是唯一 lifecycle owner。
- 实现调整：real-smoke vseq 仍是正常路径的唯一 owner；只有 static
  `shared_mem_lifecycle_initialized=0` 时，先启动的 legacy responder 才调用同一个
  `initialize_shared_memory_state()` 兜底。第二个 responder 仅复用已初始化状态，reset/stop 不清空它。
- 原因：保持现有 default-sequence topology 可运行，同时避免两个 responder 各自清空 shared store。
- 影响范围：`mem_base_sequence.sv` 和 real-smoke vseq；不新增第二个 memory owner。

### 3. 未使用的 `prog_mem` 与 alias `data_valid` 边界

`[IMPLEMENTATION_DELTA]`

- 来源：执行前源码中的 `prog_mem`/`prog_mem_byte_valid` 没有任何 consumer；当前
  `cached_alias_by_line` 也尚未具备 lifecycle 或 `data_valid` 字段。
- 实现调整：shared store 只保留实际 memory consumer 所需的 backing `main_mem`、write overlay、byte
  valid 和 write batch；本专项在 `corrupt=1` 的 ProbeAckData 情况只跳过 overlay 写并完成当前协议
  收敛，不虚构 alias `data_valid` 字段。
- 原因：`data_valid` 的表项生命周期属于后续 alias state 专项，不能在本专项引入半套 alias directory。
- 影响范围：本专项不改变 alias table 语义；后续
  `mem_ut_dcache_multi_probe_alias_state_plan_20260803.md` 负责补齐 alias lifecycle/data-valid。

### 4. Uncache A.fire 的统一时钟快照与四态保护

`[IMPLEMENTATION_DELTA]`

- 来源：独立 review 发现 Uncache 流程用 `drv_cb` 采样 A.valid 确认 fire，却用裸 interface 读取
  payload。连续 A request 在 edge 后切换时会把下一笔 payload 错当成已 fire 的那笔；同时 X/Z valid
  会被折叠为无请求。
- 原 plan：规定 store 只在真实 A.fire 后更新 overlay，但未展开 fire payload 的采样域和四态保护。
- 实现调整：`capture_sbuffer_a_xaction()` 全部从 `sbuffer_vif.drv_cb` 复制 A payload；在非 reset
  周期对 A.valid、D.ready 以及 opcode/param/size/source/address/mask/data/corrupt 全部 A payload 的
  X/Z 直接 `uvm_fatal`，检查必须发生在复制到二态 xaction 前。确认的 fire snapshot 才能建立 D response
  或加入 Uncache write batch。
- 原因：让 A.fire 判断、稳定性检查、D response 和 overlay 写使用同一时钟边界，避免已握手 store
  被误报、遗漏或被下一笔请求覆盖。
- 影响范围：只修正 Uncache responder 的采样细节，不改变其单笔串行控制或 TileLink handshake。

```text
drv_cb 边界：
  非 reset 时 A.valid/D.ready 为 X/Z -> fatal；
  capture A snapshot 前，任一 A payload 为 X/Z -> fatal；
  armed A 且 sampled A.valid=1：
    用 drv_cb payload 生成 fired snapshot；
    与 armed snapshot 比较；
    用 fired snapshot 建立 D response/Uncache write batch；
```

### 5. 派生 real-smoke vseq 的 testcase lifecycle 初始化

`[IMPLEMENTATION_DELTA]`

- 来源：终审发现 `memblock_dispatch_real_cancel_reconcile_vseq` 覆盖父类 `body()` 后直接 fork
  responder，没有执行父类 real-smoke 的初始化调用；同一仿真中重复启动 vseq 时可能复用旧 static store。
- 原 plan：只说明 normal real-smoke vseq 是 lifecycle owner，未列出覆盖 `body()` 的派生场景。
- 实现调整：cancel-reconcile vseq 在设置 scenario active 后、fork background responder 前调用继承的
  `initialize_shared_memory_store()`。该调用复用同一 static-store 初始化 helper，而不是复制数据结构
  或创建第二个 owner。
- 原因：保证每个 testcase/scenario 的 backing、overlay、write batch 与 range 从干净状态开始。
- 影响范围：cancel-reconcile vseq 和 unified virtual-sequence flow；普通 real-smoke 与 legacy
  default topology 的职责不变。

### 6. Uncache driver 与 responder 的 lockstep 输出边界

`[IMPLEMENTATION_DELTA]`

- 来源：第二轮独立 review 发现 `sbuffer_agent_agent_driver` 在获得 sequence item 后额外等待一个
  `drv_cb` 才调用 `send_pkt()`，但 `sbuffer_mem_access_base_sequence` 已按 DCache 的 `last_cycle_xact`
  合同在下一边界计算 A/D fire。
- 原 plan：只规定 Uncache store 必须在真实 A.fire 后更新 overlay，没有定义 driver 对 item 的实际输出
  边界。
- 实现调整：SBuffer/Uncache driver 改为阻塞 `get_next_item()`、立即 `send_pkt()`、立即 `item_done()`；
  responder item 的 `pre_pkt_gap/post_pkt_gap` 必须均为 0，非零直接 `uvm_fatal`。该合同与当前 DCache
  driver 一致。
- 原因：若 driver 在 item 后再等待一个 `drv_cb`，sequence 会先看到自己保存的 `A.ready/D.valid`，但 DUT
  尚未真正看到该输出，进而提前创建 Uncache write batch 或提前撤销 D response。
- 影响范围：`sbuffer_agent_agent_driver.sv`、Uncache responder flow、源码 analysis、implementation
  review；不增加 response queue/outstanding，也不改变单笔串行协议模型。

```text
Uncache driver 主循环：
  阻塞获取下一个 responder item；
  若 item 为空，立即 fatal；
  若 pre/post gap 非零，立即 fatal；
  不等待新的 drv_cb，立刻写入 clocking output 并 item_done；

Uncache sequence 下一 drv_cb：
  用上一 item 已实际驱动的 A.ready/D.valid 与当前 DUT sampled valid/ready 计算真实 fire；
  只有真实 A.fire 才建立 Uncache write batch，只有真实 D.fire 才清 pending D。
```

### 7. global stop 后新 Uncache A 请求的 fail-fast 边界

`[IMPLEMENTATION_DELTA]`

- 来源：第三轮独立 review 发现 global stop 已置位、但出现一笔未被前拍 `A.ready` 接受的新
  Uncache `A.valid` 时，旧 stop 分支会持续保持 `A.ready=0` 并等待 `A.valid` 降低，可能永久卡住。
- 原 plan：只说明全局 stop 后需要排空已接受请求，没有规定新、未 fire A 请求的失败策略。
- 实现调整：在处理上一拍已 armed 的 A.fire 和 pending D.fire 后，若仍观察到
  `global_stop_requested && sampled_a_valid && !a_fire`，立即 `uvm_fatal`。
- 原因：已 fire 的请求可以正常建立 D response 并 drain；没有 fire 的新 A 没有合法 lifecycle owner，
  不得通过无限 backpressure 伪装为正常退出。
- 影响范围：仅 Uncache responder 的 terminal 边界和相关 flow/review 文档；不改变正常 A/D handshake、
  overlay 写入、response 时序或 global stop 的发起条件。

```text
Uncache drv_cb：
  先处理上一拍已 armed 的 A 与 D；
  若 A.fire：正常建立 response/写 batch，允许后续 drain；
  若 global stop 已请求且当前仍有未 fire 的 A.valid：fatal，打印该请求不是已接受 inflight；
  否则：pending D、armed A 与当前 A.valid 均清空时发送 safe idle 并退出。
```

### 8. DCache C.fire 的四态快照与 assembly 消费一致性

`[IMPLEMENTATION_DELTA]`

- 来源：最终独立 review 发现 DCache C payload 的 interface 字段是四态 `logic`，而 responder xaction
  使用二态 `bit`。若在复制前不检查，`ProbeAckData`/`ReleaseData` 的 `corrupt=X/Z` 会静默变成 `0`，
  进而错误写入 overlay；同时原 C.fire 分支虽然建立了已确认的 fired snapshot，后续 assembly 仍消费
  较早的 armed snapshot。
- 原 plan：只约束完整 C data 且所有 beat `corrupt=0` 才能写 overlay，未展开二态复制防护和最终
  assembly 应消费哪个 snapshot。
- 实现调整：新增 `check_dcache_c_payload_known()`，在任何 C payload 复制到二态 xaction 前执行。
  对所有 C opcode 检查 `opcode/param/size/source/address`；仅对 `ProbeAckData`、`ReleaseData` 再检查
  `data/corrupt`，无数据 `ProbeAck`/`Release` 的 don't-care data/corrupt 不检查。确认 C.fire 后，
  `start_c_assembly()` 与 `consume_c_beat()` 统一消费当前 `fired_c_req_xact`；armed snapshot 只用于
  valid 等待 ready 期间的稳定性比较。
- 原因：确保“所有 data beat `corrupt=0` 才写 overlay”的判断来自真实已握手、已检查的 C payload，
  不让四态值折叠为合法零值，也不让早期 snapshot 变成第二套 data 真源。
- 影响范围：只修正 DCache C-channel snapshot/assembly 的采样细节，不改变 C opcode 支持范围、两拍
  assembly、Probe/Release owner、overlay 提交时机或 alias 生命周期。

```text
C.valid 需要被 arm：
  从 drv_cb 读取 C header；header 含 X/Z -> fatal；
  若 opcode 为 ProbeAckData/ReleaseData：data 或 corrupt 含 X/Z -> fatal；
  复制到 armed 二态 snapshot，并在下一 item 打开 C.ready；

下一 drv_cb 形成 C.fire：
  从同一 drv_cb 重新执行四态检查并复制 fired snapshot；
  用 armed 与 fired 做稳定性比较；
  用 fired snapshot 启动或推进 C assembly；
  收齐两个 data beat且所有 corrupt=0 -> 加入 DCache overlay batch；
  否则只完成协议收敛，不写 overlay。
```
