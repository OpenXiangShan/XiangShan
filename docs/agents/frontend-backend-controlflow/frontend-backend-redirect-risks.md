# Frontend Risks Around Backend Redirect

## 主题结论

frontend 直接消费 backend redirect 的主路径是明确的，但当前实现里有几处功能正确性风险，尤其集中在时序配对、异常归属和 redirect 后训练隔离。

这里的“风险”分两类：

- 代码里已有注释、`FIXME`、`XSError` 线索支持的高置信风险
- 从当前实现方式推导出的设计脆弱点

## 主路径概览

backend redirect 由 frontend 顶层直接送进 FTQ：

```scala
ftq.io.fromBackend <> io.backend.toFtq
```

在 FTQ 中，redirect 入口分成两路：

- `receiveBackendRedirect(io.fromBackend)`
- `receiveIfuRedirect(io.fromIfu.wbRedirect, specTopAddr)`

随后用固定优先级仲裁：

```scala
redirect = Mux(backendRedirect.valid, backendRedirect, ifuRedirect)
```

仲裁后的 redirect 影响：

- FTQ 指针回退：`bpuPtr` / `ifuPtr` / `pfPtr`
- `toBpu.redirect`
- `toICache.redirectFlush`

而发给 IFU 的 valid 则直接取 `backendRedirect.valid`，并不是最终仲裁后的 `redirect.valid`。

## backend redirect 接收端的关键假设

`BackendRedirectReceiver` 的当前设计依赖一个时序约定：

- backend 可以提前 1 拍给出某些 redirect 的 `ftqIdx`
- frontend 先用这个 `ftqIdx` 去读元数据
- 真正 redirect 到达时，如果上一拍收到了 ahead 索引，就直接配对使用
- 否则把 redirect 再寄存 1 拍

当前实现还带有三个值得记住的特征：

- 只使用 `ftqIdxAhead(0)`
- `ftqIdxSelOH` 没有在 frontend 侧参与选择
- 用于强校验 ahead 索引与真实 redirect 配对关系的 `XSError` 被注释掉了

## backend exception 注入路径

backend redirect 可能携带：

- `backendIPF`
- `backendIGPF`
- `backendIAF`

FTQ 会把它们转换成 frontend 可见异常，并缓存在：

- `backendException`
- `backendExceptionPtr`

随后只在 pointer 命中时注入到：

- `prefetchReq.bits.backendException`
- `fetchReq.bits.isBackendException`

这条路径的核心假设是：redirect 到来时，异常应归属到 `ifuWbPtr(0)` 指向的 FTQ entry。

## IFU 和 ResolveQueue 的影响

IFU 把 backend redirect 当成最高优先级 flush 条件之一，并在 redirect 时清空半条 RVI / IBuffer 相关的跨拍状态。  
`ResolveQueue` 则使用固定 3 拍窗口隔离 redirect 之后的 resolve / train。

这两点都直接影响功能正确性：

- IFU 是否会在 redirect 后残留旧 block 上下文
- BPU 是否会接收到本应被 flush 的 branch 训练

## 主要风险列表

### R1. `ftqIdxAhead` 与真实 redirect 缺少强校验

优先级：高

现象：

- frontend 依赖 `ftqIdxAhead(0)` 与下一拍真实 redirect 严格配对
- 但相应的强校验断言被注释掉了

主要后果：

- 用错 FTQ entry 读取 redirect 元数据
- BPU redirect 绑到错误 entry
- prefetch / topdown / 统计信息错绑
- 严重时恢复到错误控制流位置

### R2. backend redirect 无条件压过同拍 IFU redirect

优先级：中

现象：

- FTQ 用固定优先级 `Mux(backendRedirect.valid, backendRedirect, ifuRedirect)`
- 没有年龄比较
- 没有被覆盖 redirect 的保留或重放机制

主要后果：

- IFU 更老的 redirect 理论上可能被更年轻的 backend redirect 覆盖
- 最终恢复点不一定等于“真实最老错误点”

### R3. backend fault 归属使用 `ifuWbPtr(0)` 而不是 redirect 自带 `ftqIdx`

优先级：高

现象：

- fault 归属并未直接绑定 `backendRedirect.bits.ftqIdx`
- 而是依赖 redirect 到来时 `ifuWbPtr(0)` 恰好指向同一个 entry

主要后果：

- exception 被注入到错误 fetch block
- 正确 fault entry 漏掉异常标记
- fault 解释时机前后错位

### R4. redirect 后 prefetch 依赖固定 1 拍延迟

优先级：中

现象：

- `prefetchReq` 使用 `redirectNext`
- 源码已明确标注 `FIXME`

主要后果：

- redirect 后的首批 prefetch 可能跑在旧上下文或不完整上下文上
- fault 注入窗口与真实目标 entry 错位

### R5. `ResolveQueue` 的 quarantine 窗口固定为 3 拍

优先级：高

现象：

- `RedirectDelay = 3`
- 源码直说这是经验值，不是严格证明出来的边界

主要后果：

- 已被 redirect 冲掉的 branch resolve 可能在 3 拍后才回来
- 无效 branch 仍进入训练路径
- BPU 学到错误路径，造成持续性预测污染

### R6. frontend 对 redirect 扩展接口支持不完整

优先级：低

现象：

- 只使用 `ftqIdxAhead(0)`
- `ftqIdxSelOH` 未真正参与
- `io.toIfu.redirect.bits := DontCare`

主要后果：

- 当前不一定立刻出错
- 但接口扩展时容易 silently wrong

## 建议优先验证的方向

建议优先做下面几类检查：

1. 恢复或重写 `BackendRedirectReceiver` 的配对断言，验证 `ftqIdxAhead` 和真实 redirect 的一一对应关系。
2. 构造 backend redirect 与 IFU redirect 同拍到达场景，确认固定优先级是否符合真实设计意图。
3. 在带 backend IPF / IAF / IGPF 的场景中，验证 `ifuWbPtr(0)` 与 `backendRedirect.bits.ftqIdx` 是否总能对齐。
4. 拉长 resolve 返回延迟，检查 `ResolveQueue` 的 3 拍隔离窗口是否足够。
5. 在 CSR / fence / 地址翻译切换场景下，检查 `redirectNext` 驱动的 prefetch 是否仍安全。

## Current Env Status

当前 `src/test/python/Frontend/env` 只能对其中一部分风险做硬约束，而且这些约束都必须来自当前黑盒边界可观察的信息：

- env 现在明确拒绝未建模的 `ftqIdxAhead(0)` 使用；如果 drive path 试图依赖它，会直接 fail fast
- env 现在要求 backend-fault redirect 提供足够的 FTQ 上下文，否则直接 fail fast
- env 现在会在 redirect flush 时先裁掉本应被冲掉的 commit-visibility 状态；如果 flush 后仍有残留，则直接 fail fast

但下面这些风险仍主要属于 DUT / frontend RTL 侧验证问题，而不是 env 可以凭当前观测面完全证明的性质：

- `BackendRedirectReceiver` 内部 ahead 配对是否真的和下一拍 redirect 一一对应
- backend fault 最终是否一定落在 frontend RTL 使用的正确 FTQ entry 上
- `ResolveQueue` 固定 3 拍 quarantine 是否对所有微结构时序都充分
- backend redirect 与 IFU redirect 的固定优先级是否符合真实设计意图

换句话说，env 现在会拒绝“自己已知做不对”的路径，但不会假装拥有 RTL 内部不可见状态来替 DUT 做证明。

如果这里对 env 的描述与 `docs/agents/frontend-backend-agent.md` 的规范语义有冲突，以
`docs/agents/frontend-backend-agent.md` 为准；本页只讨论 redirect 消费路径上的 RTL 风险和
当前黑盒验证边界。

## 源文档

- `docs/frontend_backend_redirect_risks.md`
