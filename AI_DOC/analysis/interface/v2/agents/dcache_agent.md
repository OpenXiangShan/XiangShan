# V2 DCache TileLink 接口与字段语义

## 版本元数据

| 项目 | 内容 |
|---|---|
| RTL 版本 | V2 |
| 分支 | `mem_ut_uvm_v2` |
| 核验 commit | `f3bdd04b3763147e714a786d078e0cb90460a31d` |
| 设计基线 | `2acbf327cf7fb514593acc00d4c41117ec499e08`，见 V2 `branch_policy.md` |
| 权威源码 | `build_memblock/rtl/MemBlock.sv`、`src/main/scala/xiangshan/cache/dcache`、`coupledL2/src/main/scala/coupledL2` |
| 最后核验日期 | `2026-08-03` |

## 1. Agent 职责与边界

本文描述 MemBlock 顶层 `auto_inner_dcache_client_out_*` 接口及其在 V2 DCache/L2
交互中的字段语义。这里的测试框架角色是轻量 L2/TileLink responder：

```text
DUT DCache -> 测试框架：A、C、E 请求
测试框架 -> DUT DCache：B、D 回复
```

本文不把该 agent 扩展为完整 CoupledL2 directory、MSHR 或 set/way 模型。L2
hint、flush done 和完整 TileLink 权限状态机的内部 flow 分别见：

- [DCache-L2 refill hint 与 L2 flush done flow](../../../../rtl/v2/flows/dcache_l2_refill_hint_and_flush_done_flow.md)
- [L2 内侧 TileLink 请求、权限与回复 flow](../../../../rtl/v2/flows/l2_inner_tilelink_request_response_flow.md)

## 2. 顶层接口方向

V2 顶层端口位于 `build_memblock/rtl/MemBlock.sv:203-255`。

| 通道 | 方向 | 主要职责 | 测试框架处理方式 |
|---|---|---|---|
| A | DUT -> responder | DCache 发起 coherent Acquire 或 CBO 请求 | 按 `valid && ready` 接收并建立请求记录 |
| B | responder -> DUT | L2 对 DCache 发起 Probe | 由 Probe record 生成并保持 B payload |
| C | DUT -> responder | DCache 返回 ProbeAck、ProbeAckData 或 Release | 按 source/address/opcode 匹配 Probe/Release 生命周期 |
| D | responder -> DUT | L2 返回 Grant、GrantData、CBOAck 或 ReleaseAck | 按 opcode、source/sink 和请求记录生成 response |
| E | DUT -> responder | DCache 对 Grant 完成 GrantAck | 按 DUT 采样到的 sink 完成对应 Grant 生命周期 |

所有通道都遵守 TileLink `Decoupled` 语义：只有 `valid && ready` 才算请求或回复
真正传输。`valid=1 && ready=0` 时，payload 必须保持稳定。

## 3. A 通道字段

### 3.1 普通 TileLink 字段

| 字段 | 作用 |
|---|---|
| `opcode` | 请求类型，例如 `AcquireBlock`、`AcquirePerm`、`Get`、`Hint` |
| `param` | 请求权限/事务参数，不能由 responder 随意改写 |
| `size` | 请求大小 |
| `source` | DCache MSHR/source 标识，用于匹配 D response |
| `address` | 物理地址，DCache cache line 请求按 line 对齐 |
| `mask` | 字节有效掩码 |
| `data` | A 通道写数据或相关请求数据 |
| `corrupt` | 请求数据损坏标志 |

### 3.2 DCache user/echo 字段

| 字段 | 来源/用途 | 是否决定 L2 hit |
|---|---|---:|
| `user_alias` | `AliasKey`，记录 DCache VIPT alias | 否 |
| `user_vaddr` | 请求虚拟地址，用于 alias/critical-half 等语义 | 否 |
| `user_reqSource` | 请求来源标识 | 否 |
| `user_needHint` | `PrefetchKey`，表示该 `Acquire/Get` 是否参与 L2 预取训练 | 否 |
| `echo_isKeyword` | 关键 32-byte refill half 标志，D response 会 echo | 否 |

`user_needHint` 的准确语义不是“请求 L2 提供 refill hint”。源码链路为：

```text
A.user.PrefetchKey
    -> SinkA.task.needHint
    -> MainPipe.prefetchTrain.valid 条件
```

它只影响预取训练事件是否生成；真实的 L2 目录命中由
`dirResult_s3.hit` 决定，不能使用 `user_needHint` 推导 `l2_hit`。

另一个容易混淆的 `io_l2_hint_valid` 是 L2 到 DCache 的 GrantData 提前通知，由
`CustomL1Hint` 根据 GrantData hint queue 独立产生。它可以来自 L2 hit，也可以来自
MSHR 完成后的 miss 路径，不是 `A.user_needHint` 的直接响应。

### 3.3 A 通道按 opcode 的字段合同

| A opcode | 有效字段 | 无效字段/当前约束 | responder 处理要求 |
|---|---|---|---|
| `AcquireBlock`、`AcquirePerm` | `opcode`、`param`、`size`、`source`、`address`、`mask`、全部 `user_*` 及 `echo_isKeyword` | `corrupt=0`；`data` 不承载请求 payload | 保存 `source` 和请求元数据；对 load miss 保留 `isKeyword`，用于后续 GrantData beat 顺序 |
| `CBOClean`、`CBOFlush`、`CBOInval` | `opcode`、`size`、`source`、`address` | Scala `CacheBlockOperation()` 对 `param`、`mask`、`data`、`user_*`、`echo_*` 赋 `DontCare`，仅 `corrupt=0` | 不得用保留字段识别 CBO 类型或建立地址关联；为了避免测试框架 X 传播，本地记录可将这些字段规范化为 0 |
| `Get`、`Hint` | TileLink 通用协议定义字段 | 当前 V2 DCache coherent producer 的本路径不是该文档承诺的常规请求源 | standalone responder 不应凭顶层 bundle 存在就把它们纳入当前 DCache 正常 flow；需要支持时另行确认实际 producer 和 response opcode |

这里的“`DontCare`”不是硬件保证为 0。它表示该 opcode 不消费该字段；测试框架只能
为自身驱动/比较选择已知值，不能把该已知值反向解释为 DUT 功能语义。

## 4. Alias 语义

`user_alias` 是 DCache VIPT set/index 所需的 alias 补充信息，不能当作普通数据字段
或物理地址的一部分处理。

基本链路为：

```text
DCache A.user_alias
    -> CoupledL2 SinkA.task.alias
    -> L2 Directory MetaEntry.alias
    -> SourceB Probe b_data[2:1]
    -> DCache 根据 physical address + alias 重构 probe.vaddr
```

当同一 physical line 发现新旧 alias 冲突时，Probe 指向的是当前 DCache 中已经存在
的旧 alias。因此：

```text
B.b_data[2:1] = 被 Probe 的旧 alias
```

不能填新 Acquire 携带的新 alias。新 alias 仍保存在新 A 请求的 `user_alias` 中，待
旧 alias 的 Probe/Release 生命周期完成后再建立新状态。

DCache 使用 `b_data[2:1]` 重构 Probe 虚拟地址，其目的在于定位 VIPT cache 的正确
set；alias 错误时，即使物理 line 地址正确，也可能查错 cache set。

## 5. B 通道字段与 `b_data`

### 5.1 普通字段

| 字段 | 作用 |
|---|---|
| `opcode` | `Probe` 请求类型 |
| `param` | `toN`、`toB` 等 coherence 状态转换参数 |
| `size` | Probe 事务大小 |
| `source` | Probe source 标识 |
| `address` | 被 Probe 的物理 cache line 地址 |
| `mask` | B 通道字节掩码 |
| `corrupt` | Probe payload 损坏标志，当前模型应保持已知值 |

### 5.2 `b_data[2:1]`

DCache 在 `Probe.scala` 中读取：

```scala
val alias_addr_frag = io.mem_probe.bits.data(2, 1)
```

并与 B 通道物理地址拼接，得到用于 Probe 查找的 `vaddr`。因此它是 alias/index
补充字段，不是 Probe 数据返回内容。

测试框架必须从当前 Probe owner 的旧 cache line 状态生成：

```text
b_data[2:1] = probe_record.old_alias
```

同一个 B 请求在 backpressure 期间，该字段必须保持不变。

### 5.3 `b_data[0]`

DCache 将该位接收为：

```scala
req.needData := io.mem_probe.bits.data(0)
```

语义上它表示该 Probe 是否要求 DCache 返回 ProbeAckData：

```text
b_data[0] = 1：Probe 请求数据
b_data[0] = 0：Probe 不主动请求数据
```

但 `b_data[0]=0` 不是“禁止返回 ProbeAckData”。如果 DCache 当前 line 是 Dirty，
DCache 仍可能为了保持数据一致性返回 `ProbeAckData`。

当前 V2 CoupledL2 的实际 SourceB 赋值为：

```scala
b.data := Cat(task.alias.getOrElse(0.U), 0.U(1.W))
```

因此当前 V2 源码中：

```text
b_data[2:1] = task.alias
b_data[0]   = 0
```

测试框架本轮应保持该实际行为，不能因为 Probe 类型或 `toN/toB` 随意随机设置
`b_data[0]`。无论发送 `b_data[0]=0` 还是未来支持 `1`，都必须同时接受：

```text
ProbeAck
ProbeAckData
```

只有完整接收 ProbeAckData 的全部 C beat 后，才能释放对应 Probe owner。

### 5.4 B 通道字段合同

| 字段 | 当前 V2 是否有效 | 当前固定值或语义 |
|---|---|---|
| `opcode`、`param`、`size`、`source`、`address` | 有效 | 分别为 `Probe`、`toT/toB/toN`、line 大小、Probe source 和目标 line |
| `mask` | 当前 SourceB 固定 | 全字节为 1；DCache 不应把它作为 Probe 数据选择条件 |
| `data[2:1]` | 有效 | old alias |
| `data[0]` | 顶层字段存在，但当前 SourceB 固定 | 固定 0；语义上为 `needData`，但不能据此禁止 Dirty line 产生 `ProbeAckData` |
| `corrupt` | 当前 SourceB 固定 | 固定 0 |
| `isKeyword` | 不存在 | B bundle 没有该字段，不能构造或检查 keyword |

## 6. C 通道与 Probe 完成

C 通道由 DCache 输出，典型回复包括：

```text
ProbeAck
ProbeAckData
Release
ReleaseData
```

测试框架应按 Probe record 的 physical line、旧 alias、source 和 coherence 参数匹配
回复。对于 `ProbeAckData`，必须收集完整多 beat 数据后再更新 memory overlay 或清除
Probe 生命周期；不能在第一拍到达时提前释放 owner。

`Probe(toN)` 与 `Probe(toB)` 都使用被 Probe 的旧 alias。区别在于 `param` 描述的
coherence 状态转换；不能用 alias 新旧关系替代 `param` 语义。

### 6.1 C 通道有效字段、固定扩展字段与 `corrupt` 解释

虽然 V2 顶层展开了 `C.user_alias`、`C.user_vaddr`、`C.user_reqSource`、
`C.user_needHint` 和 `C.echo_isKeyword`，DCache 的 C 输入 TLBuffer 入队只写入
`opcode/param/size/source/address/data/corrupt`，其余 53 bit 被写为 0。因此当前实际
输出合同为：

| C 字段 | 当前 V2 合同 |
|---|---|
| `opcode`、`param`、`size`、`source`、`address` | 有效，用于 Probe/Release 生命周期和 D `ReleaseAck` 匹配 |
| `data` | 仅 `ProbeAckData`、`ReleaseData` 有效；无数据 opcode 不得将其作为 payload 消费 |
| `corrupt` | `ProbeAckData`、`ReleaseData` 中表示 corrupt；`ProbeAck`、`Release` 中同一 wire 被 SinkC 解释为 denied |
| `user_alias`、`user_vaddr`、`user_reqSource`、`user_needHint`、`echo_isKeyword` | 当前实际固定 0；不是 C 请求可传递的 alias、hint 或 keyword 语义 |

因此 responder 不得从 C 的 `echo_isKeyword` 关联 refill，也不得从 C 的 user 字段重建
virtual/alias 状态。C 的 `corrupt` 必须按 opcode 解码，不能无差别当作数据损坏。

## 7. D/E 通道与请求匹配

D 通道 response 至少需要保留：

```text
opcode, param, size, source, sink, denied, corrupt, echo_isKeyword, data
```

`source` 用于匹配 A 请求；`echo_isKeyword` 用于 DCache 两个 32-byte refill beat 的
关键半行排序；`denied/corrupt` 不能被默认当成普通成功回复。

E 通道只携带 `sink`，用于确认需要 GrantAck 的 D response。若测试框架支持多个
outstanding Grant，必须为每个 Grant 保留独立 sink 生命周期；不能固定使用一个 sink
并覆盖已有记录。

### 7.1 D 通道按 opcode 的字段合同

V2 DCache coherent port 只接收 `Grant`、`GrantData`、`CBOAck` 和 `ReleaseAck`。
其中前三者送入 MissQueue，`ReleaseAck` 送入 WritebackQueue；`AccessAckData` 不是当前
DCache coherent port 的合法 responder 回复。

| D opcode | 有效字段 | 固定值或保留字段 | 测试框架约束 |
|---|---|---|---|
| `GrantData` | `param`、`size`、`source`、`sink`、`denied`、`corrupt`、`data`、`echo_isKeyword` | 无 | `echo_isKeyword` 必须与原 A 请求一致；该值决定两个 32-byte half 的回填顺序 |
| `Grant` | `param`、`size`、`source`、`sink`、`denied`、`corrupt` | `data`、`echo_isKeyword` 不承载 payload/关键 half 语义；本地 responder 应驱动已知 0 | 用于权限完成或不带数据的 grant，随后等待对应 E `sink` 的 GrantAck |
| `CBOAck` | `size`、`source`、`denied/corrupt` | `param`、`sink`、`data`、`echo_isKeyword` 不能作为 CBO 类型或地址关联 | 以 A `source` 和已保存的 CBO record 关联；当前 CHI/CoupledL2 内部实现不构成这些保留位的固定值承诺 |
| `ReleaseAck` | `size`、`source`、`denied` | `param=0`、`sink=0`；`data`、`echo_isKeyword` reserved | 按 C `source` 完成 Release/Writeback 生命周期；不等待 E |

`GrantBuffer.toTLBundleD()` 的通用构造函数先将 `echo_isKeyword` 置 0，但真实 Grant
输出路径在发送前以 task 的 keyword 覆盖该值。因此不能只看该函数默认值而把
GrantData keyword 误判为固定 0。

### 7.2 `Grant/GrantData.param` 的最终权限赋值

`A.param` 和 `D.param` 的编码空间不同，不能逐位回显或直接比较：

```text
A.param: NtoB / NtoT / BtoT，表示 DCache 至少需要增长到的权限。
D.param: toB / toT，表示 L2 最终实际授予 DCache 的权限上限（cap）。
```

CoupledL2 MSHR 的实际赋值逻辑如下，merge 的 A 请求复用同一规则：

```text
if A.param == NtoB:
    D.param = req_promoteT ? toT : toB
else if A.param == NtoT or A.param == BtoT:
    D.param = toT
else:
    不是当前合法 coherent DCache Acquire 组合
```

| D 最终 cap | A 请求及 L2 条件 | 实际含义 |
|---|---|---|
| `toB` | `NtoB && !req_promoteT` | 普通读缺失。L2 仅授予 shared/Branch 权限，DCache 可读但不可修改该 line。 |
| `toT` | `NtoT` 或 `BtoT` | 写、AMO 或已有 Branch line 的写升级必须获得独占 Trunk 权限。 |
| `toT` | `NtoB && req_promoteT` | 读请求原本只需 Branch，但 L2 可安全多授予 Trunk；DCache 接收后按 `onGrant()` 进入 Trunk。 |

`req_promoteT` 的真实条件是以下任一项成立，且请求属于 Acquire/Get/Hint：

```text
1. directory hit，L2 没有其他 client copy，且该 line 处于 TIP；
2. directory miss，但下游 CHI/L3 返回了 Unique/Trunk 权限；
3. directory hit 且这是 aliasTask，L2 line 处于 TRUNK 或 TIP。
```

因此 `NtoB -> toT` 是 L2 权限优化，不表示 DCache 的 A 请求错误。反过来，`NtoT` 和
`BtoT` 不能回复 `toB`，否则写意图没有获得所需权限。`toN` 虽是 TileLink cap 编码之一，
但当前 coherent DCache 的 `Grant/GrantData` 合法合同不允许它。

对轻量 standalone responder：若只验证普通读缺失，可稳定使用 `NtoB -> toB`；一旦要
模拟独占 line、alias 或下游 unique grant，就必须保存最终 cap 并允许 `NtoB -> toT`，不能
仅根据 A opcode 或 load/store 来源硬编码 D param。

### 7.3 E 通道字段合同

| E 字段 | 当前 V2 合同 |
|---|---|
| `sink` | 唯一有效字段；DCache 由 `Grant/GrantData` 的 sink 生成 `GrantAck` |
| 其他字段 | 不存在；E 不携带 `isKeyword`、`source`、地址、数据或错误位 |

`CBOAck` 与 `ReleaseAck` 不要求 E。测试框架不得以 E 收到与否决定 CBO 或 Release 的完成。

### 7.4 `denied/corrupt` 的通道边界与最小 responder 规则

这两个字段不是通用随机位。`denied` 只存在于 D 通道，`corrupt` 存在于 A/B/C/D；E 没有
任何错误字段。按 MemBlock 顶层方向分类如下：

| 通道 | DUT 方向 | 字段 | 当前 V2 合同与测试框架处理 |
|---|---|---|---|
| A | DUT 输出 | `corrupt` | 当前 coherent DCache 的 `AcquireBlock/AcquirePerm` 及 CBO 构造均为 0；responder 只采样并检查稳定性。非 0 不是可由 responder 解释的请求语义，应作为 DUT/protocol 异常记录。 |
| B | DUT 输入 | `corrupt` | Probe 合法值只能为 0，且 DCache ProbeQueue 不消费该字段；轻量 responder 必须固定驱动 0，不能随机注入。 |
| C | DUT 输出 | `corrupt` | `ProbeAckData/ReleaseData` 表示 data corrupt；DCache WritebackQueue 从 tag/data error 生成该值。无数据 `ProbeAck/Release` 的标准 TileLink 合同要求为 0，但当前 CoupledL2 SinkC 会把同一 wire 解释为 denied，因此轻量 responder 必须采样、保持握手闭环，不能自行改写。 |
| D | DUT 输入 | `denied`、`corrupt` | 仅在与原 A/C request 匹配的合法 D opcode 上消费，具体约束见下表。默认 smoke 对两者固定 0；D-error 专项可在 response record 创建时按权重生成合法组合。 |
| E | DUT 输出 | 无 | 不存在 `denied/corrupt`。 |

D 通道错误字段必须按 opcode 驱动，不能随意组合：

| D opcode | `denied` | `corrupt` | 最小 responder 规则 |
|---|---|---|---|
| `GrantData` | DCache 会累计并向 refill/forward error 路径传递 | DCache 会累计并标记返回数据错误；若 `denied=1`，TileLink 合同要求 `corrupt=1` | 默认两者均为 0。`MEMBLOCK_L2_GRANTDATA_*_WT` 只在对应 response record 创建时采样一次，并在全部 beat 与 D hold 中保持不变。 |
| `Grant` | DCache 会累计该错误，但当前轻量模型不注入 | 合法 Grant 必须为 0 | 正常模式均为 0；不把 `Grant.corrupt=1` 当作合法随机激励。 |
| `CBOAck` | CMOUnit 传给 LSQ `CMOResp.denied` | CMOUnit 传给 LSQ `CMOResp.corrupt` | 默认两者均为 0。`MEMBLOCK_L2_CBO_ACK_*_WT` 在单拍 CBOAck record 创建时独立采样，仍发送匹配 CBO source 的 Ack，使 CMO FSM 能完成。 |
| `ReleaseAck` | 必须为 0 | 必须为 0 | 轻量 responder 固定 0；非 0 属于协议违例，不能用作 writeback error 注入。 |

当前 `dcache_mem__access_base_sequence` 的默认 normal 模式仍将 D 错误位驱为 0；只有显式设置
六个 D-error runtime 权重时才生成错误 response。DCache `GrantData` 与 `CBOAck` 在 coherent
response record 创建时保存错误快照，Uncache `AccessAckData/AccessAck` 则在
`sbuffer_mem_access_base_sequence::create_uncache_response_record()` 创建 TL-UL response record
时处理。scheduler、D hold、GrantAck/E、主表、LSQ、pass/fail 与 terminal 不重新解释或重采样这些位。

DUT C `ProbeAckData/ReleaseData` 的 `corrupt` 仍被逐 beat 汇总；任一 beat corrupt 时跳过向测试框架
memory overlay 写回，但仍完成 Probe/Release 生命周期。该 D-error 能力只构造合法 response stimulus，
不建立 L2 directory、下游错误原因模型、RM 或 scoreboard。

因此本轮约束为：

```text
需要支持正常 MemBlock 功能：
  B.corrupt = 0；D Grant/GrantData/CBOAck 的 denied=0、corrupt=0；
  D ReleaseAck 的 denied=0、corrupt=0。

看到 DUT 输出 A/C corrupt：
  只能 monitor/sample，不能由测试框架赋值；
  C data opcode 的 corrupt 数据不写入本地 memory overlay，但 handshake 和 owner 收敛继续完成。

已实现的错误注入：
  `GrantData` denied 命中强制 corrupt=1；非 denied 的 corrupt 由独立权重决定；
  `CBOAck` 的 denied/corrupt 独立采样；`AccessAckData` denied 命中强制 corrupt=1；
  无数据 `AccessAck` 只允许 denied，corrupt 固定 0。所有采样都只发生一次，不能用逐拍随机 bit 翻转替代。
```

## 8. 测试框架最小处理规则

```text
1. A fire：保存 source、physical address、user_alias、user_vaddr、needHint、isKeyword。
2. A.user_needHint：只进入预取训练相关状态，不改变 L2 hit 判断。
3. A.user_alias：作为当前 physical line 的 alias 元数据保存。
4. 发送 B：b_data[2:1] 使用被 Probe 的旧 alias，当前 V2 b_data[0] 固定为 0。
5. B backpressure：保持整个 B payload 稳定。
6. 收到 C：按 Probe owner 匹配；ProbeAckData 收齐全部 beat 后再结束 Probe。
7. 生成 D：按 A source 匹配，GrantData 正确 echo isKeyword，并维护 sink。
8. 收到 E：按 sink 完成对应 Grant，不影响其他 source 的生命周期。
9. alias 冲突：先处理旧 alias 的 Probe/Release，再允许新 alias 状态生效。
10. C：只消费标准 C 字段；C 的 `user_*` 和 `echo_isKeyword` 一律按固定 0 检查或忽略。
11. D：按 opcode 消费字段；仅 `GrantData` 使用 `data` 和 `echo_isKeyword`，不得将
    `CBOAck`/`ReleaseAck` 的保留字段加入 source/地址关联。
12. 错误字段：B `corrupt`、正常 D reply `denied/corrupt` 固定 0；C data response 的
    `corrupt` 仅阻止本地 memory overlay 写入，不阻止协议生命周期结束。
```

## 9. 与测试框架的边界

本接口文档不要求 standalone responder 实现完整 L2 directory、所有 set/way、完整
coherence replacement 或真实 L2 prefetch predictor。当前必须保证的是：

- alias 作为 Probe 定位信息正确传递，不能把新 alias 当旧 alias 使用；
- `b_data[0]` 遵守当前 V2 固定 0 的发送行为，同时兼容 Dirty line 的 ProbeAckData；
- `user_needHint` 不被错误用作 L2 hit 或 refill response valid；
- A/B/C/D/E 的 owner、source、sink 和多 beat 生命周期不因 backpressure 提前结束。

## 10. 源码证据

| 结论 | 源码位置 |
|---|---|
| 顶层 DCache A/B/C/D/E 字段和方向 | `build_memblock/rtl/MemBlock.sv:203-255` |
| A.user 字段进入 L2 TaskBundle | `coupledL2/src/main/scala/coupledL2/SinkA.scala` |
| `PrefetchKey` 的固定名称 | `coupledL2/HuanCun/src/main/scala/huancun/HCCacheParameters.scala` |
| DCache A load/store miss 产生/合并 `isKeyword` | `src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala:218-241,561-562,606-609` |
| A `isKeyword` 写入 Acquire echo | `src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala:271-272,850-851` |
| `user_needHint` 参与预取训练 | `coupledL2/src/main/scala/coupledL2/tl2tl/MainPipe.scala:450-460` |
| DCache 读取 B.data[2:1]/B.data[0] | `src/main/scala/xiangshan/cache/dcache/mainpipe/Probe.scala:146-161` |
| B.data[2:1] 重构 Probe vaddr | `src/main/scala/xiangshan/cache/dcache/DCacheWrapper.scala:1560-1570` |
| 当前 SourceB 固定 b_data[0]=0 | `coupledL2/src/main/scala/coupledL2/SourceB.scala:55-64` |
| Dirty/needData 决定 ProbeAckData | `coupledL2/src/main/scala/coupledL2/tl2tl/MainPipe.scala:268-295` |
| C 扩展字段由 Queue 固定写 0 | `build_memblock/rtl/Queue2_TLBundleC_1.sv:58-165`、`build_memblock/rtl/TLBuffer_21.sv:228-260` |
| SinkC 对 C alias/vaddr/keyword 置 0，按 opcode 解码 corrupt/denied | `coupledL2/src/main/scala/coupledL2/SinkC.scala:67-82` |
| GrantData keyword 改变两 beat 顺序并回写 D echo | `coupledL2/src/main/scala/coupledL2/GrantBuffer.scala:158-232` |
| CoupledL2 将 A grow param 映射为 D final cap | `coupledL2/src/main/scala/coupledL2/tl2chi/MSHR.scala:163-170,255-260,739-752,824-831` |
| A grow/D cap 编码及 DCache `onGrant()` 后状态 | `rocket-chip/src/main/scala/tilelink/Bundles.scala:107-137`、`rocket-chip/src/main/scala/tilelink/Metadata.scala:52-114` |
| DCache 对 D opcode 的接收分流 | `src/main/scala/xiangshan/cache/dcache/DCacheWrapper.scala:1627-1642` |
| DCache 累积 Grant 错误并传递 forward/refill error | `src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala:657-695,817-821,930-938` |
| CBOAck 的 denied/corrupt 传递至 CMO/LSQ | `src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala:299-370,1229-1237` |
| DCache C response 的 corrupt 来源 | `src/main/scala/xiangshan/cache/dcache/mainpipe/MainPipe.scala:997-1004`、`src/main/scala/xiangshan/cache/dcache/mainpipe/WritebackQueue.scala:226-270` |
| TileLink 对 B Probe、C no-data、D Grant/ReleaseAck 错误字段的约束 | `rocket-chip/src/main/scala/tilelink/Monitor.scala:165-215,242-276,304-338` |
| 当前轻量 responder 对 B/D 错误位和 C data corrupt 的处理 | `mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv:529-551,964-1003,1063-1173,1528-1534` |
| A CBO、D Grant/ReleaseAck 的 TileLink 构造字段 | `rocket-chip/src/main/scala/tilelink/Edges.scala:343-402,679-707` |
| E 仅使用 sink 完成 GrantAck | `rocket-chip/src/main/scala/tilelink/Edges.scala:469-475`、`src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala:876-879` |
| L2 hint 传递 keyword 至 MemBlock LSQ | `coupledL2/src/main/scala/coupledL2/CustomL1Hint.scala:72-121`、`coupledL2/src/main/scala/coupledL2/CoupledL2.scala:530-540`、`src/main/scala/xiangshan/mem/MemBlock.scala:1010-1014` |
| L2 refill hint 独立产生 | `coupledL2/src/main/scala/coupledL2/CustomL1Hint.scala`、`coupledL2/src/main/scala/coupledL2/CoupledL2.scala:520-545` |

## 知识修订记录

本次将以下容易混淆的描述统一修订为当前 V2 源码语义：

```text
旧描述：A.user_needHint 是请求 L2 提供 refill hint。
新描述：A.user_needHint 是预取训练标志；io_l2_hint_valid 是独立的 L2->DCache GrantData 提前通知。

旧描述：b_data[0] 可按 Probe needData 动态设置。
新描述：当前 V2 CoupledL2 SourceB 实际固定 b_data[0]=0；DCache 仍可能因 Dirty line 返回 ProbeAckData。

新增：C 顶层虽含 user/echo 展开端口，但当前 TLBuffer/Queue 将其固定为 0；仅 A 和
GrantData D 的 isKeyword 参与关键 half 语义，B/E 没有该字段。

新增：按 A/D opcode 划分有效字段。CBO 构造中的 DontCare 与 C 通道扩展字段的实际固定 0
严格区分，测试框架不得将前者作为 DUT 功能契约。

新增：D Grant/GrantData 的 param 是最终 cap。NtoB 通常返回 toB，但在 L2 可独占、下游
返回 unique 或 alias 收敛时可提升为 toT；NtoT/BtoT 必须返回 toT。

新增：B corrupt 和 ReleaseAck denied/corrupt 固定 0；D GrantData/CBOAck 的错误位会被
DCache 消费。轻量 responder 默认只驱动正常 0，DUT C data corrupt 时跳过本地内存写回并继续收敛。

新增：D-error 专项已实现六个公共 runtime 权重。DCache `GrantData`、`CBOAck` 和 Uncache
`AccessAckData/AccessAck` 只在各自 response record 创建点生成并保存合法错误组合；错误位不会进入
主表、LSQ commit/deq、pass/fail 或 terminal 的软件判断。
```
