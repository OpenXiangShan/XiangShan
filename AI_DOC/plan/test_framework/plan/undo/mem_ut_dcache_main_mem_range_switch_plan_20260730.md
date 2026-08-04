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

写覆盖层的更新边界如下：DCache `ReleaseData` 仅在完整两个 C beat 均完成握手后更新；
`ProbeAckData` 也必须收齐两 beat，但 `corrupt=1` 时不更新 overlay，只发布协议收敛事件；Uncache
port 仅在 store A-channel 真实握手后更新。store issue、DCache dirty 标记、未握手的 C valid、无数据
`ProbeAck` 均不得更新覆盖层。

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

- DCache `GrantData` 从 merged view 读取数据；完整 `ReleaseData` 与完整且 `corrupt=0` 的
  `ProbeAckData` 写入 overlay；`corrupt=1` 的完整 `ProbeAckData` 仅发布协议收敛事件并由 alias
  状态置 `data_valid=0`，不得写入 overlay。
- Uncache load 从 merged view 读取数据；Uncache store 在 A-channel fire 后写入 overlay。当前该端口
  的 agent/sequence 文件仍沿用 `sbuffer_agent` 名称，coding 时只迁移其 memory access helper，不重命名
  agent、interface、connect 或 UVM topology。
- DCache `AcquireBlock` 的 GrantData 也从 A.fire 时固化的 merged view 快照生成；`AcquirePerm`、CBOAck
  等无数据 response 不建立 data read snapshot。
- SBuffer 仅经 `sbuffer.io.dcache` 向 DCache 发送内部 store request，不建立独立的 memory wrapper；
  cacheable store 的外部写回仍由 DCache C-channel 路径记录。

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
