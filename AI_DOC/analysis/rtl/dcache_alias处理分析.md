# DCache alias 处理分析

## 1. 背景

DCache 访问时为了时序，会先用虚拟地址 VA 的低位选择 DCache set，同时 TLB 并行翻译得到物理地址 PA。对于 4KB page，VA[11:0] 和 PA[11:0] 一定相同；但如果 DCache 的 set index 使用了 VA[12] 或更高位，这些位翻译后不一定和 PA 对应位相同。

因此源码把超出 4KB page offset、但仍参与 DCache set index 的 VA 位称为 alias bits / color bits。

相关源码：

- `src/main/scala/xiangshan/cache/dcache/DCacheWrapper.scala`
  - `aliasBitsOpt`：当 `nSets * blockBytes > 4KB` 时启用 alias bits。
  - `get_alias()`：从 VA 中提取 alias bits。
  - `is_alias_match()`：比较两个 VA 的 alias bits 是否一致。
- `src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala`
  - miss merge 时同时检查 PA block 和 alias bits。
  - DCache 向 L2 发 Acquire 时通过 `AliasKey` 携带 alias bits。
- `coupledL2/src/main/scala/coupledL2/tl2tl/MainPipe.scala`
  - L2 directory 命中后比较旧 alias 和新请求 alias，判断是否触发 `cache_alias`。
- `coupledL2/src/main/scala/coupledL2/SourceB.scala`
  - L2 发 Probe 时在 B channel `data` 中携带旧 alias bits。

## 2. DCache alias bits 怎么来

以常见配置为例：

```text
DCache line = 64B
line offset = 6 bit
DCache nSets = 128
set index = 7 bit
set index + line offset = 13 bit
4KB page offset = 12 bit
```

也就是说 DCache set index 需要用到 VA[12]。VA[12] 不属于 4KB page offset，不保证和 PA[12] 相同，所以 VA[12] 就是 alias bit。

源码抽象逻辑：

```text
if blockOffBits + idxBits > 12:
  alias_bits = vaddr[blockOffBits + idxBits - 1 : 12]
else:
  alias_bits = 0
```

文字伪代码：

```text
提取 alias bits：
  计算 DCache line offset 位宽。
  计算 DCache set index 位宽。
  如果 line offset + set index 没有超过 4KB page offset：
    当前 DCache 不存在 VA alias 风险，alias bits 固定为 0。
  否则：
    从 VA 中取出超过 4KB page offset、但仍参与 set index 的位。
    这些位作为 alias bits。
```

## 3. DCache 发现 alias 的原理

DCache 判断 alias 冲突不能只看：

```text
VA alias bits 是否等于 PA 对应 bit
```

真正的 cache alias 冲突条件是：

```text
同一个 PA cacheline
并且已经存在于 L1D/DCache 中
并且旧 alias bits != 新请求 alias bits
```

DCache MissQueue 中的直接行为是：

```text
block_match = 两个请求 PA cacheline 相同
alias_match = 两个请求 VA alias bits 相同

如果 block_match && alias_match：
  可以 merge 到同一个 miss 请求。

如果 block_match && !alias_match：
  不允许 merge。
  让新请求继续发到 L2，由 L2 directory 做 alias 仲裁。
```

文字伪代码：

```text
DCache miss merge 检查：
  比较已有 MSHR 请求和新请求的 PA cacheline。
  如果 PA cacheline 不同：
    这是不同物理 cacheline，不是 alias 冲突，不进行 alias merge 判断。
  如果 PA cacheline 相同：
    再比较两个请求的 VA alias bits。
    如果 alias bits 相同：
      两个请求会落到 DCache 同一个 alias 位置，可以安全 merge。
    如果 alias bits 不同：
      两个请求是同一份物理数据，但会落到 DCache 不同 set。
      DCache 不 merge，避免一份 PA line 在 L1D 中形成两个副本。
```

## 4. 不同场景说明

### 4.1 VA alias=00，PA 对应位=01

这个条件本身不能说明发生 alias 冲突。它只说明：

```text
新请求的 VA alias bits 和 PA 对应 bit 不一致。
```

是否需要处理 alias，要继续看 L2 directory 中是否已经有同一个 PA cacheline 的旧 alias。

#### 场景 A：同 PA line 之前不在 L1D

```text
VA alias = 00
PA 对应位 = 01
L2 directory 没有记录该 PA line 已在 L1D
```

这不是 alias 冲突。

处理流程：

```text
DCache miss 到 L2。
L2 directory miss 或没有 L1D client 记录。
L2 正常返回 Grant。
DCache 把该 cacheline 填入 VA alias=00 对应的 set。
L2 directory 记录该 PA line 当前 alias=00。
```

#### 场景 B：同 PA line 已经以 alias=00 在 L1D

```text
VA alias = 00
PA 对应位 = 01
L2 directory 记录旧 alias = 00
```

这也不是 alias 冲突。

处理流程：

```text
如果 DCache 已经命中：
  直接 hit。
如果当前走 miss/refill 路径：
  MissQueue 可以和同 PA、同 alias 的请求 merge。
  L2 directory 看到旧 alias 和新 alias 一致，正常处理。
```

#### 场景 C：同 PA line 已经以 alias=01 在 L1D

```text
VA alias = 00
PA 对应位 = 01
L2 directory 记录旧 alias = 01
```

这才是 alias 冲突。

处理流程：

```text
DCache 新请求 miss 到 L2，并携带新 alias=00。
L2 查 directory，发现同一个 PA line 已经在 L1D 中，旧 alias=01。
L2 判断旧 alias != 新 alias，触发 cache_alias。
L2 分配 MSHR aliasTask。
L2 向 DCache 发 Probe，Probe 中携带旧 alias=01。
DCache 用 PA + 旧 alias=01 定位旧 set。
DCache 清理或降级旧 alias 对应的 cacheline。
DCache 返回 ProbeAck。
L2 给新请求 Grant。
DCache 把该 cacheline 填入新 alias=00 对应的 set。
L2 directory 更新 alias=00。
```

### 4.2 VA alias=00，PA 对应位=00

这个条件说明新请求的 VA alias bits 和 PA 对应位一致，但仍不能单独作为 alias 判断标准。

#### 场景 A：同 PA line 之前不在 L1D

```text
正常 miss/refill。
L2 directory 记录 alias=00。
```

#### 场景 B：同 PA line 已经以 alias=00 在 L1D

```text
正常 hit 或 merge。
不触发 alias 处理。
```

#### 场景 C：同 PA line 已经以 alias=01 在 L1D

```text
仍然是 alias 冲突。
原因不是 VA alias 是否等于 PA 对应位，
而是同一个 PA line 的旧 alias=01，新 alias=00。
```

处理流程和 4.1 场景 C 相同。

## 5. L2 如何完成 alias 仲裁

L2 directory 记录每个 PA cacheline 的 client 状态和 alias bits。新请求进入 L2 后，L2 判断：

```text
cache_alias =
  当前请求是 Acquire
  && directory hit
  && 该 PA line 已在 L1D client 中
  && directory 旧 alias != 新请求 alias
```

文字伪代码：

```text
L2 处理 DCache Acquire：
  读取请求 PA line 和请求 alias bits。
  查询 directory。
  如果 directory miss：
    正常向下级获取数据，并在返回后记录 alias bits。
  如果 directory hit，但该 PA line 不在 L1D client：
    正常处理，并更新 client/alias 状态。
  如果 directory hit，且该 PA line 已在 L1D client：
    比较旧 alias 和新 alias。
    如果 alias 相同：
      正常处理。
    如果 alias 不同：
      触发 cache_alias。
      先 Probe 旧 alias 对应的 L1D line。
      等旧 line 清理完成后，再允许新 alias refill。
```

## 6. Probe 如何定位旧 alias line

L2 发 Probe 时，把旧 alias bits 放入 B channel `data` 字段。DCache 收到 Probe 后，用：

```text
PA address + Probe 携带的 alias bits
```

重构旧 line 在 DCache 中的 vindex，从而定位需要清理的 set。

文字伪代码：

```text
DCache 处理 L2 Probe：
  读取 Probe 的 PA line 地址。
  读取 Probe 携带的旧 alias bits。
  如果当前 DCache 配置存在 alias 风险：
    用 PA 高位、旧 alias bits、PA 低位拼出用于查 DCache set 的 vaddr/index。
  如果当前 DCache 配置不存在 alias 风险：
    直接使用 PA 低位定位 set。
  查找并清理旧 alias 对应的 line。
  返回 ProbeAck。
```

## 7. 总结

最重要的判断规则：

```text
VA alias bits 是否等于 PA 对应 bit，不是 alias 冲突判断标准。

真正的 alias 冲突是：
  同一个 PA cacheline
  已经在 L1D/DCache 中存在
  并且旧 alias bits != 新请求 alias bits
```

整体处理分工：

```text
DCache：
  提取 alias bits。
  MissQueue merge 时检查 PA block 和 alias bits。
  同 PA、同 alias 可以 merge。
  同 PA、不同 alias 不 merge。
  收到 L2 Probe 后，根据 Probe 中的旧 alias bits 清理旧 set。

L2：
  在 directory 中记录 PA line 对应的 L1D alias bits。
  发现同 PA line 的新旧 alias 不一致时触发 cache_alias。
  先 Probe 清掉旧 alias。
  再 Grant 新请求并更新 directory alias。
```
