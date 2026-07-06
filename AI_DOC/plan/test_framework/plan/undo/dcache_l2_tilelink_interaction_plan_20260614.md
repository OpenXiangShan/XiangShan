# DCache 与 L2Cache TileLink-C 交互支持计划

## 1. 目标

本计划用于指导后续完善 `dcache_mem__access_base_sequence`，使 mem_ut 环境中的 dcache agent 不再只模拟简单 A 到 D 的地址/数据访问，而是按 XiangShan DCache 与 L2Cache 的 TileLink-C 行为模拟完整 L2 侧 responder。

本轮只总结方案，不修改 sequence 代码。

后续实现范围：

1. 继续使用 `mem_access_base_sequence` 中的主内存模型作为 L2 数据来源。
2. 在 `dcache_mem__access_base_sequence` 中支持 dcache agent 现有 A/B/C/D/E 五个 channel 字段。
3. 让 DCache 发起的 `AcquireBlock`、`AcquirePerm`、CBO 请求收到合法 D 响应。
4. 让环境能够按 L2 行为主动向 DCache 发送 B `Probe`。
5. 让环境正确消费 DCache 返回的 C `ProbeAck/ProbeAckData`、`Release/ReleaseData`。
6. 让环境正确跟踪 D `Grant/GrantData` 后的 E `GrantAck`。
7. 可选协调 `l2_hint` 顶层输入，但该信号不是 dcache TileLink agent 内部字段。

## 2. 参考源码

DCache 侧主要依据：

1. `src/main/scala/xiangshan/cache/dcache/DCacheWrapper.scala`
2. `src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala`
3. `src/main/scala/xiangshan/cache/dcache/mainpipe/Probe.scala`
4. `src/main/scala/xiangshan/cache/dcache/mainpipe/WritebackQueue.scala`
5. `src/main/scala/xiangshan/cache/dcache/mainpipe/MainPipe.scala`

L2Cache 侧主要依据：

1. `coupledL2/src/main/scala/coupledL2/SinkA.scala`
2. `coupledL2/src/main/scala/coupledL2/SourceB.scala`
3. `coupledL2/src/main/scala/coupledL2/SinkC.scala`
4. `coupledL2/src/main/scala/coupledL2/GrantBuffer.scala`
5. `coupledL2/src/main/scala/coupledL2/CustomL1Hint.scala`
6. `coupledL2/src/main/scala/coupledL2/CoupledL2.scala`
7. `coupledL2/src/main/scala/coupledL2/tl2tl/MainPipe.scala`
8. `coupledL2/src/main/scala/coupledL2/tl2chi/MainPipe.scala`

TileLink 常量主要依据：

1. `rocket-chip/src/main/scala/tilelink/Bundles.scala`
2. `rocket-chip/src/main/scala/tilelink/Edges.scala`
3. `rocket-chip/src/main/scala/tilelink/Monitor.scala`

当前 UVM 侧字段主要依据：

1. `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_xaction.sv`
2. `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/dcache_agent_agent_interface.sv`
3. `mem_ut/ver/ut/memblock/seq/base_seq/mem_base_sequence.sv`

## 3. 当前 sequence 缺口

当前 `dcache_mem__access_base_sequence` 位于：

```text
mem_ut/ver/ut/memblock/seq/base_seq/mem_base_sequence.sv
```

当前实现只做了简化行为：

1. 采集 A channel。
2. 根据 A opcode 粗略判断读写。
3. 调用主内存 256bit 同宽 task。
4. 用 D channel 返回 `AccessAck` 或 `AccessAckData`。
5. B channel 固定不发请求。
6. C channel 只给 ready，不解析 `ProbeAck/Release`。
7. E channel 只给 ready，不跟踪 `GrantAck`。

该行为不足以模拟真实 DCache 和 L2Cache 的 coherent TileLink-C 交互。DCacheWrapper 中 D channel 明确只期望：

1. `Grant`
2. `GrantData`
3. `CBOAck`
4. `ReleaseAck`

因此后续 coherent 模式下不能继续把 dcache A 请求统一响应成 `AccessAck/AccessAckData`。

## 4. TileLink 常量

后续实现应在 sequence 中集中定义常量，不在逻辑里散落 magic number。

### 4.1 Opcode

A channel:

| 名称 | 数值 | 说明 |
| --- | --- | --- |
| `PutFullData` | 0 | dcache coherent 路径不应作为主路径使用 |
| `PutPartialData` | 1 | dcache coherent 路径不应作为主路径使用 |
| `ArithmeticData` | 2 | dcache coherent 路径不应作为主路径使用 |
| `LogicalData` | 3 | dcache coherent 路径不应作为主路径使用 |
| `Get` | 4 | dcache coherent 路径不应作为主路径使用 |
| `Hint` | 5 | prefetch hint 类请求 |
| `AcquireBlock` | 6 | DCache miss 取数据或取权限 |
| `AcquirePerm` | 7 | DCache 权限升级，常见于 full overwrite/store grow |
| `CBOClean` | 12 | 自定义 CBO clean |
| `CBOFlush` | 13 | 自定义 CBO flush |
| `CBOInval` | 14 | 自定义 CBO inval |

B channel:

| 名称 | 数值 | 说明 |
| --- | --- | --- |
| `Probe` | 6 | L2 向 DCache 查询、降权或要求回写数据 |

C channel:

| 名称 | 数值 | 说明 |
| --- | --- | --- |
| `ProbeAck` | 4 | DCache 对 Probe 的无数据响应 |
| `ProbeAckData` | 5 | DCache 对 Probe 的带数据响应 |
| `Release` | 6 | DCache 主动释放 cacheline，无数据 |
| `ReleaseData` | 7 | DCache 主动释放 cacheline，带数据 |

D channel:

| 名称 | 数值 | 说明 |
| --- | --- | --- |
| `Grant` | 4 | L2 授权，无数据，需要 E `GrantAck` |
| `GrantData` | 5 | L2 授权并返回数据，需要 E `GrantAck` |
| `ReleaseAck` | 6 | L2 确认 C `Release/ReleaseData`，不需要 E |
| `CBOAck` | 8 | L2 确认 CBO 请求，不需要 E |

E channel:

| 名称 | 数值 | 说明 |
| --- | --- | --- |
| `GrantAck` | 0 | DCache 确认 D `Grant/GrantData` 的 sink |

### 4.2 Permission param

Cap 类型，B `Probe` 与 D `Grant/GrantData` 使用：

| 名称 | 数值 | 说明 |
| --- | --- | --- |
| `toT` | 0 | 授予或要求保持 Trunk 上限 |
| `toB` | 1 | 授予或要求降到 Branch 上限 |
| `toN` | 2 | 要求降到 Nothing，上 D `Grant/GrantData` 不应使用 |

Grow 类型，A `AcquireBlock/AcquirePerm` 使用：

| 名称 | 数值 | 说明 |
| --- | --- | --- |
| `NtoB` | 0 | 从 Nothing 获取 Branch |
| `NtoT` | 1 | 从 Nothing 获取 Trunk |
| `BtoT` | 2 | 从 Branch 升级到 Trunk |

Report/Shrink 类型，C `ProbeAck/Release` 使用：

| 名称 | 数值 | 说明 |
| --- | --- | --- |
| `TtoB` | 0 | 从 Trunk 降到 Branch |
| `TtoN` | 1 | 从 Trunk 降到 Nothing |
| `BtoN` | 2 | 从 Branch 降到 Nothing |
| `TtoT` | 3 | 仍保持 Trunk |
| `BtoB` | 4 | 仍保持 Branch |
| `NtoN` | 5 | 当前无权限 |

## 5. DCache 到 L2 的 A channel 行为

### 5.1 AcquireBlock

来源：

1. `MissQueue.scala` 中 miss entry 生成 `edge.AcquireBlock`。
2. 用于 load miss、store miss 非 full overwrite、AMO miss、prefetch miss 等需要数据的场景。

关键字段：

| UVM 字段 | 规则 |
| --- | --- |
| `a_bits_opcode` | `AcquireBlock` |
| `a_bits_param` | `NtoB`、`NtoT` 或 `BtoT` |
| `a_bits_size` | cacheline 大小，默认 `log2(64B)=6` |
| `a_bits_source` | miss queue MSHR id，默认范围 `0..nMissEntries-1` |
| `a_bits_address` | cacheline 地址，应按 64B 对齐处理 |
| `a_bits_user_alias` | L2 用于别名处理 |
| `a_bits_user_vaddr` | L2 prefetch/train 使用的 vaddr 高位 |
| `a_bits_user_reqSource` | load/store/amo/prefetch 来源 |
| `a_bits_user_needHint` | L2 hint 相关 |
| `a_bits_echo_isKeyword` | L2 返回 GrantData 时需要 echo |

响应要求：

1. 默认返回 D `GrantData`。
2. 数据来自主内存对应 64B cacheline。
3. D `source` 必须等于 A `source`。
4. D `param` 应为 `toB` 或 `toT`，不允许 `toN`。
5. 64B cacheline 在当前 256bit D channel 上分 2 beat 返回。
6. D `sink` 需要分配有效 sink id，并等待 E `GrantAck` 回收。
7. `denied=1` 时 D `corrupt` 必须同时为 1，符合 TileLink monitor 约束。

### 5.2 AcquirePerm

来源：

1. `MissQueue.scala` 中 `full_overwrite` 场景选择 `edge.AcquirePerm`。
2. 常见于只需要权限、不需要从 L2 获取数据的权限升级。

关键字段：

| UVM 字段 | 规则 |
| --- | --- |
| `a_bits_opcode` | `AcquirePerm` |
| `a_bits_param` | 通常为 `NtoT` 或 `BtoT`，`NtoB` 对 `AcquirePerm` 不合法 |
| `a_bits_size` | cacheline 大小，默认 6 |
| `a_bits_source` | MSHR id |
| `a_bits_address` | cacheline 地址 |

响应要求：

1. 默认返回 D `Grant`。
2. D `source` 匹配 A `source`。
3. D `param` 使用 `toT` 或 `toB`，但不能使用 `toN`。
4. 无有效 data。
5. 仍然需要 E `GrantAck`。

### 5.3 CBOClean/CBOFlush/CBOInval

来源：

1. `CMOUnit` 通过 A channel 发送 `edge.CacheBlockOperation`。
2. CBO source 在 Scala 中使用 `cfg.nMissEntries + 1`。

关键字段：

| UVM 字段 | 规则 |
| --- | --- |
| `a_bits_opcode` | `CBOClean`、`CBOFlush` 或 `CBOInval` |
| `a_bits_size` | cacheline 大小，默认 6 |
| `a_bits_source` | CMO source |
| `a_bits_address` | CBO 地址 |

响应要求：

1. 返回 D `CBOAck`。
2. D `source` 匹配 A `source`。
3. D `param` 置 0。
4. 不返回有效 data。
5. 不需要 E `GrantAck`。

### 5.4 非 coherent A opcode

DCacheWrapper 侧 D channel 分发只接受 `Grant/GrantData/CBOAck/ReleaseAck`。因此在 dcache coherent sequence 中：

1. `Get/Put/Arithmetic/Logical/Hint` 不作为默认合法 dcache 请求响应路径。
2. 如果后续需要兼容旧 sequence 行为，可单独加 `legacy_uncached_response_enable`，默认关闭。
3. 默认 coherent 模式下遇到这些 opcode 应报 warning 或 error，而不是返回 `AccessAck/AccessAckData`。

## 6. L2 到 DCache 的 B channel Probe 行为

### 6.1 触发来源

L2 可能在以下场景向 DCache 发 B `Probe`：

1. 其他 agent 或下游一致性请求要求当前 DCache 降权或失效。
2. L2/CHI snoop 需要 DCache 回报权限状态。
3. L2 需要 DCache 回写 dirty 数据。
4. CBO 或替换流程派生上行 Probe。

`coupledL2.SourceB` 中 Probe 会经过内部 4 entry probe buffer，并且会避开同地址未完成 GrantAck 的窗口。

### 6.2 B 字段规则

| UVM 字段 | 规则 |
| --- | --- |
| `b_valid` | 环境主动发送 Probe |
| `b_ready` | DCache 输出，ProbeQueue 有空 entry 时接收 |
| `b_bits_opcode` | 必须为 `Probe` |
| `b_bits_param` | `toT`、`toB` 或 `toN` |
| `b_bits_size` | cacheline 大小，默认 6 |
| `b_bits_source` | 指向被 probe 的 client source 范围。当前 coupledL2 对 dcache 使用 dcache source range 起点 |
| `b_bits_address` | 64B 对齐 cacheline 地址 |
| `b_bits_mask` | 全 1，对应一个 beat 的 byte mask |
| `b_bits_data[0]` | DCache ProbeQueue 解释为 `needData` |
| `b_bits_data[2:1]` | DCache ProbeQueue 用作 alias/vindex fragment |
| `b_bits_corrupt` | 固定 0 |

当前 `coupledL2.SourceB` 对 dcache path 默认把 `data[0]` 置 0，只传 alias。DCache 和 HuanCun 风格接口都保留了 `needData` 语义，因此后续 sequence 需要支持可配的 `needData=1` 场景，但默认保持 0。

### 6.3 Probe 合法性约束

后续 `dcache_mem__access_base_sequence` 发 B Probe 时必须满足：

1. `opcode` 只能为 `Probe`。
2. 地址必须在主内存合法 range 内。
3. 地址按 64B cacheline 对齐。
4. 同一 cacheline 不能有未完成的 Probe。
5. 同一 cacheline 如果刚发出 D `Grant/GrantData` 且 E `GrantAck` 未收到，不能发 B Probe。
6. 可选模拟 `blockProbeAfterGrantCycles=8` 的冷却窗口，在 GrantAck 后延迟若干周期再允许同 block Probe。
7. B valid 必须保持到 `b_ready` 握手，不允许 0 时间脉冲。
8. Probe 发送后需要等待 C `ProbeAck/ProbeAckData`，再释放 pending 状态。

## 7. DCache 到 L2 的 C channel 行为

### 7.1 ProbeAck/ProbeAckData

来源：

1. DCache 收到 B `Probe`。
2. `ProbeQueue` 将 B 转成 mainpipe probe request。
3. `MainPipe` 根据当前 coherence 状态、dirty 状态和 `probe_need_data` 决定是否需要带数据。
4. `WritebackQueue` 通过 C channel 发 `ProbeAck` 或 `ProbeAckData`。

关键字段：

| UVM 字段 | 规则 |
| --- | --- |
| `c_valid` | DCache 输出 |
| `c_ready` | 环境驱动，默认可为 1 |
| `c_bits_opcode` | `ProbeAck` 或 `ProbeAckData` |
| `c_bits_param` | `TtoB/TtoN/BtoN/TtoT/BtoB/NtoN` |
| `c_bits_size` | cacheline 大小，默认 6 |
| `c_bits_source` | DCache WritebackQueue 生成的合法 client source |
| `c_bits_address` | 被 Probe 的 cacheline 地址 |
| `c_bits_data` | `ProbeAckData` 时有效，2 beat 组成 64B line |
| `c_bits_corrupt` | data 类响应的 corrupt 标记 |

处理规则：

1. `ProbeAck/ProbeAckData` 是 B Probe 的响应，不需要 D `ReleaseAck`。
2. sequence 需要按 address 或 pending probe 表匹配 ProbeAck，不能强依赖 C source 等于 B source。
3. `ProbeAckData` 两个 256bit beat 连续出现，按 beat 顺序拼成 64B 数据。
4. `ProbeAckData` 且 `corrupt=0` 时，可用该 line 更新主内存。
5. `ProbeAckData` 且 `corrupt=1` 时，不更新主内存，只记录错误统计。
6. `ProbeAck` 无数据，只更新本地权限影子状态。

### 7.2 Release/ReleaseData

来源：

1. DCache 替换、回写或主动释放 cacheline。
2. `MainPipe` 产生 writeback 请求。
3. `WritebackQueue` 通过 C channel 发 `Release` 或 `ReleaseData`。

关键字段：

| UVM 字段 | 规则 |
| --- | --- |
| `c_bits_opcode` | `Release` 或 `ReleaseData` |
| `c_bits_param` | shrink/report param |
| `c_bits_size` | cacheline 大小，默认 6 |
| `c_bits_source` | release entry source |
| `c_bits_address` | release cacheline 地址 |
| `c_bits_data` | `ReleaseData` 时有效，2 beat 组成 64B line |
| `c_bits_corrupt` | data 类 release 的 corrupt 标记 |

处理规则：

1. `Release/ReleaseData` 必须收到 D `ReleaseAck`。
2. D `ReleaseAck.source` 必须等于 C `source`。
3. D `ReleaseAck.size` 使用 C `size`。
4. D `ReleaseAck.param=0`。
5. D `ReleaseAck.denied=0`，`corrupt=0`。
6. `ReleaseData` 且 `corrupt=0` 时，用 C 数据更新主内存。
7. `ReleaseData` 且 `corrupt=1` 时，不更新主内存，但仍返回 `ReleaseAck`，否则 DCache writeback queue 会卡住。

## 8. L2 到 DCache 的 D channel 行为

### 8.1 GrantData

适用于 A `AcquireBlock`。

字段规则：

| UVM 字段 | 规则 |
| --- | --- |
| `d_valid` | 环境驱动 |
| `d_ready` | DCache 输出 |
| `d_bits_opcode` | `GrantData` |
| `d_bits_param` | `toB` 或 `toT`，不能 `toN` |
| `d_bits_size` | A `size`，默认 6 |
| `d_bits_source` | A `source` |
| `d_bits_sink` | sequence 分配，等待 E ack |
| `d_bits_denied` | 默认 0，错误注入时可置 1 |
| `d_bits_echo_isKeyword` | 复制 A `echo_isKeyword` |
| `d_bits_data` | 主内存 line 的当前 beat |
| `d_bits_corrupt` | 默认 0；若 `denied=1` 必须为 1 |

beat 规则：

1. DCache line 为 64B。
2. dcache agent D data 为 256bit，也就是 32B。
3. 一个 `GrantData` cacheline 需要 2 beat。
4. `echo_isKeyword=0` 时，先发低 32B，再发高 32B。
5. `echo_isKeyword=1` 时，参考 `GrantBuffer` 行为，先发高 32B，再发低 32B，同时 DCache 内部用 `refill_count ^ isKeyword` 还原。
6. 两个 beat 的 `source/sink/size/param/denied/corrupt/echo` 保持一致。
7. 最后一个 beat 完成后等待 E `GrantAck`。

### 8.2 Grant

适用于 A `AcquirePerm`。

字段规则：

1. `d_bits_opcode=Grant`。
2. `d_bits_source` 等于 A `source`。
3. `d_bits_sink` 分配有效 sink。
4. `d_bits_param` 使用 `toT` 或 `toB`。
5. 不返回有效 data。
6. 需要 E `GrantAck`。

### 8.3 CBOAck

适用于 A `CBOClean/CBOFlush/CBOInval`。

字段规则：

1. `d_bits_opcode=CBOAck`。
2. `d_bits_source` 等于 A `source`。
3. `d_bits_param=0`。
4. 不需要 E `GrantAck`。

### 8.4 ReleaseAck

适用于 C `Release/ReleaseData`。

字段规则：

1. `d_bits_opcode=ReleaseAck`。
2. `d_bits_source` 等于 C `source`。
3. `d_bits_size` 等于 C `size`。
4. `d_bits_param=0`。
5. `d_bits_denied=0`。
6. `d_bits_corrupt=0`。
7. `d_bits_sink=0`。
8. 不需要 E `GrantAck`。

## 9. DCache 到 L2 的 E channel 行为

来源：

1. DCache 收到 D `Grant`。
2. DCache 收到 D `GrantData`。
3. `MissQueue` 通过 `edge.GrantAck(io.mem_grant.bits)` 生成 E channel。

字段规则：

| UVM 字段 | 规则 |
| --- | --- |
| `e_valid` | DCache 输出 |
| `e_ready` | 环境驱动，默认可为 1 |
| `e_bits_sink` | 必须等于此前 D `Grant/GrantData` 分配的 sink |

处理规则：

1. sequence 默认 `e_ready=1`。
2. D `Grant/GrantData` 发出后，在 `pending_grant_by_sink` 中登记 sink、source、addr。
3. 捕获 E handshake 后释放该 sink。
4. sink 未释放前不能复用。
5. 如果长时间未收到 E，应报 UVM warning 或 error，避免隐藏死锁。

## 10. l2_hint 侧带行为

`l2_hint` 不是 TileLink A/B/C/D/E 字段，而是 XiangShan 自定义的 L2 到 L1 hint：

```scala
class L2ToL1Hint {
  val sourceId = UInt(log2Up(cfg.nMissEntries).W)
  val isKeyword = Bool()
}
```

L2 侧规则：

1. `CustomL1Hint` 只在即将发送 `GrantData` 时产生有效 hint。
2. `CoupledL2` 只把 source 属于 dcache source range 的 hint 发给 core。
3. 输出到 core 前会把全局 source 减去 dcache source range start，变成 DCache 本地 MSHR id。
4. 连续 hint 至少隔一个周期，因为 `GrantData` 通常需要 2 beat。

后续 UT 处理建议：

1. dcache agent 内没有 `l2_hint` 字段，不能直接在 `dcache_mem__access_base_sequence` 内驱动该信号。
2. 若需要完全模拟 L2 行为，应由 virtual sequence 协调 `other_ctrl` 或对应顶层 agent 驱动 `io_l2_hint_valid/sourceId/isKeyword`。
3. 基础 coherent TL 支持可先不驱动 `l2_hint`，DCache 仍可通过 D `GrantData` 完成 refill。
4. 需要验证 load replay 提前唤醒时，再开启 hint 场景。

## 11. 后续控制状态变量

建议在 `dcache_mem__access_base_sequence` 或其 helper 中维护以下状态。

### 11.1 配置开关

| 变量 | 默认 | 说明 |
| --- | --- | --- |
| `coherent_tl_enable` | 1 | 启用 coherent dcache TL-C responder |
| `legacy_uncached_response_enable` | 0 | 兼容旧 AccessAck/AccessAckData 行为 |
| `probe_enable` | 0 | 是否主动向 DCache 发 B Probe |
| `probe_need_data_enable` | 0 | 是否允许 B `data[0]=1` |
| `cbo_enable` | 1 | 是否响应 CBOAck |
| `l2_hint_enable` | 0 | 是否联动顶层 l2_hint agent |
| `error_inject_enable` | 0 | 是否注入 denied/corrupt |
| `grant_backpressure_enable` | 0 | 是否随机拉低 A/C/E ready 或 D valid |

默认场景应保持稳定、低风险：`probe_enable=0`、`error_inject_enable=0`、`l2_hint_enable=0`。

### 11.2 协议常量

| 变量 | 建议值 | 说明 |
| --- | --- | --- |
| `dcache_line_bytes` | 64 | DCache blockBytes |
| `dcache_beat_bytes` | 32 | dcache TL data width 256bit |
| `dcache_line_beats` | 2 | 64B / 32B |
| `dcache_lg_line_size` | 6 | TL size |
| `n_miss_entries` | 16 | 默认来自 Parameters.scala |
| `n_probe_entries` | 8 | 默认来自 Parameters.scala |
| `n_release_entries` | 18 | 默认来自 Parameters.scala |
| `release_id_base` | `n_miss_entries + 1` | DCache release source 起点 |

### 11.3 Outstanding 表

| 状态 | key | 说明 |
| --- | --- | --- |
| `pending_a_by_source` | A source | 已握手但未完成 D 响应的 Acquire/CBO |
| `pending_d_beat` | source/sink | 正在发送的多 beat GrantData |
| `pending_grant_by_sink` | D sink | 已发 Grant/GrantData，等待 E GrantAck |
| `pending_probe_by_line` | cacheline address | 已发 B Probe，等待 C ProbeAck |
| `pending_release_by_source` | C source | 已收到 C Release，等待发送 D ReleaseAck |
| `c_line_assembly` | C source 或 address | 收集 ReleaseData/ProbeAckData 两个 beat |
| `shadow_perm_by_line` | cacheline address | sequence 侧 L2 视角的 DCache 权限影子状态 |
| `shadow_dirty_by_line` | cacheline address | sequence 侧 dirty 影子状态 |
| `sink_free_bitmap` | sink id | D sink 分配与回收 |
| `probe_cooldown_by_line` | cacheline address | GrantAck 后 Probe 冷却窗口 |

## 12. 主流程状态机

后续 sequence body 不应只在 A handshake 后立即生成单个 D transaction，而应拆成多个并行服务循环或一个周期级调度器。

每个周期建议执行：

1. 构造默认 idle xaction：`a_ready=1`、`c_ready=1`、`e_ready=1`、`b_valid=0`、`d_valid=0`。
2. 采集 A handshake：`a_valid && a_ready`。
3. 采集 C handshake：`c_valid && c_ready`。
4. 采集 E handshake：`e_valid && e_ready`。
5. 根据 pending 队列选择本周期是否发送 D。
6. 根据 probe 配置和合法性选择本周期是否发送 B。
7. 设置 transaction 的 `pre_pkt_gap/post_pkt_gap`，时序 delay 仍由 driver 消费。

优先级建议：

1. 如果已有 D 多 beat 正在发送，优先继续发送 D。
2. `ReleaseAck` 优先于新 `GrantData`，避免 DCache writeback queue 长时间占用。
3. 新 A 响应入队，不一定同周期返回 D。
4. B Probe 只在没有同 block hazard 时发送。

## 13. A 请求处理计划

### 13.1 AcquireBlock

处理步骤：

1. 检查 source 是否已有 pending A。若有，报错。
2. 检查地址是否在主内存 range 内。越界时生成 denied GrantData。
3. 从主内存读取 64B line。
4. 按 `a_bits_echo_isKeyword` 计算 2 beat 返回顺序。
5. 分配 sink。
6. 入队 2 个 D `GrantData` beat。
7. 记录 `pending_grant_by_sink`，等待 E。
8. 更新 `shadow_perm_by_line` 为 `toB` 或 `toT` 对应状态。

### 13.2 AcquirePerm

处理步骤：

1. 检查 source pending。
2. 分配 sink。
3. 入队单 beat D `Grant`。
4. 记录 `pending_grant_by_sink`。
5. 更新 `shadow_perm_by_line` 为 T/B。

### 13.3 CBO

处理步骤：

1. 识别 `CBOClean/CBOFlush/CBOInval`。
2. 若地址越界，可按后续错误策略置 denied/corrupt。
3. 入队 D `CBOAck`。
4. 不分配 sink，不等待 E。

## 14. B Probe 生成计划

Probe 默认关闭。开启后按以下策略生成：

1. 从已知 `shadow_perm_by_line` 中选择一个有权限的 cacheline。
2. 也允许从主内存合法 range 中选择地址，但若该 line 从未被 DCache acquire，预计 DCache 返回 `NtoN`。
3. 根据配置选择 `param=toN/toB/toT`。
4. 默认 `needData=0`，开启 `probe_need_data_enable` 后可置 1。
5. `b_bits_data[2:1]` 填 alias fragment，默认来自最近 A `user_alias`。
6. 发送后登记 `pending_probe_by_line`。
7. 等待 C `ProbeAck/ProbeAckData`。

禁止发 Probe 的条件：

1. 同 line 已有 pending Probe。
2. 同 line 有 pending Grant 等待 E。
3. 同 line 有 probe cooldown 未结束。
4. DCache reset 未结束。
5. 主内存 range 未配置或目标地址越界。

## 15. C 响应处理计划

### 15.1 ProbeAck/ProbeAckData

处理步骤：

1. 根据 `c_bits_address` 查找 `pending_probe_by_line`。
2. 若找不到，报 warning。为避免卡死仍接收。
3. 根据 `c_bits_param` 更新 `shadow_perm_by_line`。
4. 如果 opcode 是 `ProbeAckData`，收集 2 beat。
5. 数据完整且 `corrupt=0` 时写入主内存对应 64B。
6. 清除 pending probe。
7. 不发送 D 响应。

### 15.2 Release/ReleaseData

处理步骤：

1. 如果 opcode 是 `ReleaseData`，收集 2 beat。
2. 数据完整且 `corrupt=0` 时写入主内存对应 64B。
3. 根据 `c_bits_param` 更新 `shadow_perm_by_line`。
4. 入队 D `ReleaseAck`。
5. `ReleaseAck.source` 使用 C source。
6. `ReleaseAck` 发出后清除 `pending_release_by_source`。

## 16. E GrantAck 处理计划

处理步骤：

1. 每周期采集 `e_valid && e_ready`。
2. 查 `pending_grant_by_sink[e_bits_sink]`。
3. 命中则释放 sink。
4. 对应 line 写入 `probe_cooldown_by_line`，可配置冷却周期。
5. 未命中则报 warning 或 error。

## 17. 与主内存模型的关系

DCache coherent TL 侧访问主内存时以 64B cacheline 为粒度，但底层 `main_mem_access_task` 仍是 1KB line sparse memory。

转换规则：

1. A `AcquireBlock` 从主内存读取 64B，byte mask 为 64 个 1。
2. C `ReleaseData/ProbeAckData` 向主内存写入 64B，byte mask 为 64 个 1。
3. D `GrantData` 每个 beat 从 64B line 中裁剪出 32B。
4. C data 每个 beat 256bit，两个 beat 拼成 64B。
5. 主内存返回 `denied/corrupt` 后传递到 D `GrantData`，其中 denied 必须带 corrupt。

## 18. UVM 字段映射

### 18.1 环境驱动字段

后续 sequence 需要驱动：

1. `auto_inner_dcache_client_out_a_ready`
2. `auto_inner_dcache_client_out_b_valid`
3. `auto_inner_dcache_client_out_b_bits_opcode`
4. `auto_inner_dcache_client_out_b_bits_param`
5. `auto_inner_dcache_client_out_b_bits_size`
6. `auto_inner_dcache_client_out_b_bits_source`
7. `auto_inner_dcache_client_out_b_bits_address`
8. `auto_inner_dcache_client_out_b_bits_mask`
9. `auto_inner_dcache_client_out_b_bits_data`
10. `auto_inner_dcache_client_out_b_bits_corrupt`
11. `auto_inner_dcache_client_out_c_ready`
12. `auto_inner_dcache_client_out_d_valid`
13. `auto_inner_dcache_client_out_d_bits_opcode`
14. `auto_inner_dcache_client_out_d_bits_param`
15. `auto_inner_dcache_client_out_d_bits_size`
16. `auto_inner_dcache_client_out_d_bits_source`
17. `auto_inner_dcache_client_out_d_bits_sink`
18. `auto_inner_dcache_client_out_d_bits_denied`
19. `auto_inner_dcache_client_out_d_bits_echo_isKeyword`
20. `auto_inner_dcache_client_out_d_bits_data`
21. `auto_inner_dcache_client_out_d_bits_corrupt`
22. `auto_inner_dcache_client_out_e_ready`

### 18.2 DUT 输出采集字段

后续 sequence 或 monitor FIFO 需要采集：

1. A channel 全字段，用于构造 Grant/GrantData/CBOAck。
2. B ready，用于完成 Probe 握手。
3. C channel 全字段，用于处理 ProbeAck/Release。
4. D ready，用于完成 D 响应握手。
5. E channel sink，用于回收 Grant sink。

## 19. 后续文件落地建议

当前 `dcache_mem__access_base_sequence` 仍在 `seq/base_seq/mem_base_sequence.sv` 内。后续代码阶段可选两种方式：

1. 小步扩展：先在当前文件内扩展 `dcache_mem__access_base_sequence`，降低编译接入风险。
2. 结构整理：将 `dcache_mem__access_base_sequence` 单独迁移到 `mem_ut/ver/ut/memblock/seq/virtual_sequence/dcache_mem__access_base_sequence.sv`，并同步 `tc_pkg.sv` include。

若选择结构整理，必须遵循：

1. `mem_access_base_sequence` 继续放在 `seq/base_seq`。
2. dcache/sbuffer responder sequence 放在 `seq/virtual_sequence`。
3. 同步 `tc/tc.f` include path。
4. 同步 `tc/tc_pkg.sv` include 顺序。
5. 同步更新 `AI_DOC/plan/test_framework/plan/do/mem_design_plan_20260614.md`。
6. 同步检查 `mem_ut/ver/ut/memblock/rule/memblock_sequence_add_rule.md`。

## 20. 验证计划

后续实现后至少覆盖：

1. `AcquireBlock` 返回 2 beat `GrantData`，DCache 发送 E `GrantAck`。
2. `AcquireBlock` 且 `echo_isKeyword=1`，D beat 顺序符合 L2 `GrantBuffer` 行为。
3. `AcquirePerm` 返回 `Grant`，DCache 发送 E `GrantAck`。
4. CBOClean/CBOFlush/CBOInval 返回 `CBOAck`。
5. DCache 发 `Release`，环境返回 `ReleaseAck`。
6. DCache 发 `ReleaseData`，环境写主内存并返回 `ReleaseAck`。
7. 环境发 B `Probe toB`，DCache 返回 `ProbeAck` 或 `ProbeAckData`。
8. 环境发 B `Probe toN`，DCache 返回合法 report param。
9. B `needData=1` 时能接收 `ProbeAckData`。
10. 同 line pending Grant 未收到 E 时，Probe 被禁止。
11. 同 line pending Probe 未收到 C 时，不重复发 Probe。
12. `denied/corrupt` 注入符合 D `GrantData` 约束。
13. `ReleaseAck` 永远 `denied=0/corrupt=0/param=0`。
14. 主内存越界访问触发 denied，不更新主内存。
15. `l2_hint_enable=1` 时，hint sourceId 与 A source 匹配，且只对 `GrantData` 场景发 hint。

## 21. 默认假设

1. DCache cacheline 为 64B。
2. dcache TL data width 为 256bit，即 32B。
3. `GrantData`、`ReleaseData`、`ProbeAckData` 都是 2 beat。
4. DCache coherent bus 的正常 A 请求以 `AcquireBlock/AcquirePerm/CBO` 为主。
5. `Get/Put/AccessAckData` 不是 dcache coherent responder 的默认路径。
6. `probe_enable` 默认关闭，避免基础 sanity 被主动 Probe 干扰。
7. D/C response timing delay 继续通过 transaction 的 `pre_pkt_gap/post_pkt_gap` 完成，不在 memory task 内 wait。
8. 后续如新增 plus 参数，默认使能类参数必须为 0，并同步 `plus.sv`、`seq/plus_cfg/default.cfg` 和相关规则文档。
