# Frontend Risks Around Backend Redirect

本文整理 redirect 接收路径的验证关注点。下列条目
用于组织场景和证据，当前版本是否存在功能缺陷需由同版本运行判定。

## 主路径与源码入口

- `src/main/scala/xiangshan/frontend/ftq/BackendRedirectReceiver.scala`
- `src/main/scala/xiangshan/frontend/ftq/IfuRedirectReceiver.scala`
- `src/main/scala/xiangshan/frontend/ftq/Ftq.scala`
- `src/main/scala/xiangshan/frontend/ftq/ResolveQueue.scala`

backend redirect 与 IFU writeback 分别进入 receiver，FTQ 优先选择 backend
redirect。最终 redirect 恢复 `bpuPtr/pfPtr/fetchPtr/pnrPtr`，驱动 BPU
恢复和 ICache flush。IFU 的 redirect valid 来自 backend receiver。

## R1. Ahead 匹配与寄存路径

`ftqIdxAhead` 为单个 `Valid[FtqPtr]`。receiver 将其寄存，只有前一拍 ahead
有效且 pointer 等于当前 redirect 的 `ftqIdx` 时才走直接路径，其余实际
redirect 经寄存路径处理。实际 redirect 与 ahead 同拍时优先读取实际索引。

验证需要覆盖 ahead 匹配、不匹配、缺失、连续 redirect 和 FTQ wrap，确认
meta 的提前读取与实际 redirect payload 属于同一个动态 entry。

## R2. Backend 与 IFU redirect 优先级

FTQ 固定优先选择 backend redirect。IFU receiver 还会在 backend redirect
有效时抑制 IFU redirect 和 IFU resolve。需要用明确的年龄及路径关系构造
同拍冲突，检查恢复目标和被抑制的训练事务。

## R3. Backend fault 与 satpFlush 归属

FTQ 保存 `backendException`、`hasSatpFlush` 和 `backendFlagPtr`。
当 backend redirect 带 fault 或 satpFlush 时，标记 pointer 取自
`backendRedirect.bits.newFtqIdx`。prefetch/main-fetch 按该 pointer 匹配标记。

`fetchPtr` 领先标记 entry 至少三个 fetch block 后清除标记。应覆盖 source
flush-itself/flush-after、BPU override、连续 fault、satpFlush 与双 fetch
降级，检查异常和标记只进入对应的新路径事务。

## R4. Redirect 与预取、双 fetch

当前预取经过 `PrefetchQueue`，main fetch 使用 `toMainPipe`。redirect 恢复
pointer，S2/S3 override 也会分别回退 prefetch/fetch pointer。第二个候选需
越过 `pnrPtr`，且不能带 backend flag。

应检查这些事件相邻或重叠时的候选取消、首个恢复请求、标记归属和重复请求。

## R5. ResolveQueue 的三周期隔离

`ResolveQueue` 使用 `RedirectDelay=3` 传播 backend redirect。FTQ 的 trainCache
还会检查当拍及前一拍 redirect，过滤 younger train。队列中的 branch meta
与被复用的 FTQ slot 需要保持归属一致。

延长 resolve 延迟、制造 slot wrap/reuse，并让 BPU train backpressure 与
redirect 重叠，检查被冲掉的分支不会进入后续有效训练。

## R6. IFU Redirect payload 的使用边界

`io.toIfu.redirect.bits` 在 FTQ 中赋为 `DontCare`，其 valid 用于 flush。
接口消费者若新增 payload 依赖，需要同时更新驱动端、bundle 和观测契约。

## 当前环境边界

`env/bundles/backend.py` 绑定标量 ahead 的 valid、flag 和 value。
BackendModel 要求显式 ahead 的 flag/value 成对提供、redirect 具有 source FTQ
上下文，且调度时间至少留出一拍。backend-fault redirect 也需合法的 source
身份，已提交 FTQ entry 受 stale-context 检查保护。

环境检查需要结合 DUT 波形验证 receiver、meta 和队列内部时序。
BackendModel 的 queue、flush 和 commit 语义见
[BackendModel 规范](../frontend-backend-model.md)，FTQ 路径见
[FTQ 说明](../submodule-design/frontend-ftq.md)。
