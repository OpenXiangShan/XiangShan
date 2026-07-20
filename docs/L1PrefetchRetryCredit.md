# L1 预取重试 Credit 机制

## 1. 背景

L1 预取请求经过 `PrefetcherWrapper` 仲裁后，通过一级 Decoupled
Pipeline 进入 DCache MainPipe。请求在 MainPipe s2 完成 tag 命中判断，并在
miss 时尝试进入 MissQueue。

当 MissQueue 无法接收请求时，MainPipe 通过单拍 `prefetch_nack` 返回失败的
预取请求。该返回通路是 Valid 接口，不支持反压。如果 Retry Queue 已满，nack
请求只能被丢弃。

原有实现通过固定重试间隔和队列满时的 fast-drain 模式缓解溢出，但它只能在
队列接近或已经满时作出反应，无法覆盖已经进入 Wrapper Pipeline 和 MainPipe
的首次预取请求。这些在途请求仍可能连续产生 nack。

## 2. 设计目标

- Retry Queue 不丢弃仍有重试次数的 nack。
- 每条请求最多重发两次；第二次重发再次 nack 后丢弃。
- 在首次预取进入不可撤回的流水通路之前，为潜在 nack 预留队列空间。
- Retry Queue 严格优先于其他 L1 预取器。
- MSHR 满时不发射首次请求或重试请求。
- 不使用固定重试间隔、fast-drain、burst 或 cooldown 状态。
- 不通过 MainPipe 到预取入口的组合旁路同拍复用 credit。

## 3. 完成事件

MainPipe 对每条进入 s2 的 L1 预取请求产生唯一完成事件：

```scala
prefetchDone := s2_fire && s2_isPrefetch
```

同一拍的 `prefetch_nack.valid` 表示该请求未被 MissQueue 接收：

```scala
nackPrefetch :=
  s2_valid && s2_can_go_to_mq && !io.miss_req.ready && s2_isPrefetch
```

`prefetchDone` 覆盖 L1 hit、MSHR allocate/merge 成功和 nack。它和
`prefetch_nack` 在 `PrefetcherWrapper` 中使用相同的一级寄存，保证 Arbiter
看到的 done 和 nack 对齐。

不能根据请求发射后的固定拍数推断成功。MainPipe s1/s2 是可反压流水级，请求
可能停留多拍；必须使用实际的 `s2_fire` 完成事件。

## 4. Credit 不变量

Retry Queue 深度为 `depth`，定义：

```text
reserved = retryQueue.count + inflightCount
```

其中：

- `retryQueue.count` 是已经返回 nack、等待重试的请求数。
- `inflightCount` 是已经被 Retry Arbiter 输出端接受、但尚未返回 done 的请求数。

系统始终保持：

```text
reserved <= depth
```

credit 是聚合计数，不需要为请求分配实际编号。Retry Queue 保存请求的地址、
来源等 payload；credit 只负责提前预留存储容量。

## 5. 两级处理

### 5.1 第一级：完成与 nack

每个 `prefetchDone` 将对应请求从在途状态移除：

```text
inflightCount -= 1
```

如果同拍 nack 的请求仍有重试次数，则请求进入 Retry Queue：

```text
inflightCount -= 1
retryQueue.count += 1
reserved 不变
```

如果请求成功，或者第二次重发再次 nack 并被丢弃，则不入队：

```text
inflightCount -= 1
reserved -= 1
```

状态在时钟沿更新，第二级不会在 done 当拍旁路使用刚释放的 credit。

### 5.2 第二级：发射

Retry Queue 采用严格优先策略：

```text
Retry Queue 非空且 MSHR 非满：发射队首重试请求
Retry Queue 非空且 MSHR 满：不发射任何 L1 预取
Retry Queue 为空：允许其他预取器申请空闲 credit
```

重试请求出队并进入在途状态时：

```text
retryQueue.count -= 1
inflightCount += 1
reserved 不变
```

因此，重试请求即使在 `reserved == depth` 时也允许发射。它只是把已经持有的
Queue credit 转移到流水线，不申请新的 credit。

其他预取器请求只有在以下条件全部满足时才能发射：

```text
Retry Queue 为空
MSHR 非满
reserved < depth
下游 ready
```

首次请求被输出端实际接收后：

```text
inflightCount += 1
reserved += 1
```

## 6. 同拍状态变化

发射和 done 可以在同一拍发生，`inflightCount` 按净变化更新：

```scala
inflightNext := inflightCount + issue.asUInt - done.asUInt
```

Retry Queue 本身负责同拍 enqueue/dequeue 的 count 更新。典型状态变化如下：

| 事件 | Retry Queue | Inflight | Reserved |
| --- | ---: | ---: | ---: |
| 首次请求发射 | 0 | +1 | +1 |
| 第一次或第二次重发 | -1 | +1 | 0 |
| 请求成功 | 0 | -1 | -1 |
| 首次请求或第一次重发 nack | +1 | -1 | 0 |
| 第二次重发 nack | 0 | -1 | -1 |

请求使用两位 `retry_vec` 记录重发历史：

| `retry_vec` | 含义 | nack 后处理 |
| --- | --- | --- |
| `00` | 原始请求 | 入队，下一次发射为 `01` |
| `01` | 第一次重发 | 入队，下一次发射为 `11` |
| `11` | 第二次重发 | 达到上限，丢弃并释放 credit |

`10` 是非法状态，通过断言检查。

## 7. 断言与性能计数器

至少保留以下断言：

```scala
assert(retryQueue.count + inflightCount <= depth.U)
assert(!prefetchDone || inflightCount =/= 0.U)

when(retryableNack) {
  assert(retryQueue.io.enq.ready)
}
```

建议记录以下事件，用于检查 credit 是否过度限制预取：

- 可重试 nack 入队数。
- Retry Queue 发射数。
- 第一次重发 nack 数。
- 达到最大重发次数后的丢弃数。
- credit full 周期数。
- Retry Queue 非空周期数。
- 因 Retry Queue 或 credit 阻塞其他预取器的周期数。

## 8. Done 协议与异常处理

credit 机制要求每个被 Retry Arbiter 输出端接受的请求最终产生且只产生一个
`prefetchDone`。第二次重发产生的 nack 同样必须产生 done；它不会再次入队，
因此该 done 会真正释放 reservation。

如果后续修改在接受请求后丢失 done，`inflightCount` 将无法减少。硬件采用
fail-closed 策略：停止接受新的首次预取，而不是猜测请求已经完成后超额释放
credit。盲目使用固定超时释放并不安全，因为 MainPipe 是可反压流水线；原请求
可能在超时后才返回首次 nack，此时 Retry Queue 的预留空间可能已经被复用。

本实现通过以下机制暴露协议错误：

- `nack` 必须与 `done` 同拍。
- `done` 时 `inflightCount` 必须非零。
- `reserved` 不得超过 Retry Queue 深度。
- `l1PfRetryCreditFullNoDoneCycles` 记录 credit 满且没有完成事件的周期。

如果未来必须在硬件中从丢失 done 自动恢复，需要扩展协议，为请求携带 credit
ID 和 generation，或者由 MainPipe 返回可重建的绝对流水占用。仅增加超时计数
无法同时保证自动恢复和 Retry Queue 不溢出。
