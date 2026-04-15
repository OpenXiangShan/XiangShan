# Backend Commit And CallRetCommit Notes

## 主题结论

backend 发给 frontend 的 `commit` 和 `callRetCommit` 同源于 ROB commit，但职责不同、粒度不同、flush 下的行为也不同。

- `commit`：推进 frontend / FTQ 的整体提交边界
- `callRetCommit`：把已提交指令里的 call/ret 信息送给 frontend，再经 BPU / RAS 消费

## 接口职责

`CtrlBlock` 中对 frontend 的接口为：

```scala
val commit = Valid(new FtqPtr)
val callRetCommit = Vec(CommitWidth, Valid(new CallRetCommit))
```

`CallRetCommit` 只带两部分信息：

- `ftqPtr`
- `rasAction`

因此它不是“推进 FTQ commit 指针”的信号，而是“某条已提交指令是否需要作为 call/ret 上报”的信号。

## 普通 commit 的发送条件

普通 `commit` 的生成条件是：

```scala
val frontendCommit = rob.io.commits.commitValid.reduce(_ || _) &&
  rob.io.commits.isCommit && !s0_robFlushRedirect.valid
io.frontend.toFtq.commit.valid := RegNext(frontendCommit)
```

结论：

- ROB 当拍确实发生 commit
- commit group 至少有一条有效提交
- 同拍没有 ROB flush redirect
- 满足后，下一拍 frontend 才看到 `toFtq.commit.valid`

`bits` 则是本拍 commit group 中选出的一个 `ftqIdx`，frontend 用它推进 `commitPtr`。

## callRetCommit 的发送条件

`callRetCommit` 按 ROB commit 槽位逐条产生：

```scala
val vld = rob.io.commits.isCommit && rob.io.commits.commitValid(i)
crc.valid := GatedValidRegNext(vld)
crc.bits.ftqPtr := RegEnable(blk.bits.ftqIdx.get, vld)
crc.bits.rasAction := RegEnable(..., vld)
```

直接结论：

- 某条指令在 ROB commit 的下一拍，`callRetCommit(i)` 就可能发给 frontend
- 它不需要等待 frontend 自己的 `commitPtr` 推进
- 它也不要求对应 FTQ entry 已经在 frontend 视角“整体 committed”

## rasAction 的意义

`callRetCommit.valid` 不等于“这一定是 call/ret”。

backend 的行为是：

- 只要某个 ROB commit 槽位有效，下一拍该槽位的 `callRetCommit.valid` 就会拉高
- 是否真的是 call/ret，要看 `rasAction`

因此：

- call / ret / coroutine-swap：`valid = 1` 且 `rasAction != None`
- 普通指令：`valid = 1` 但 `rasAction = None`

frontend 随后再过滤掉 `rasAction = None` 的项。

## frontend 如何消费

frontend 中：

```scala
commitQueue.io.backendCommit := io.fromBackend.callRetCommit
```

`CommitQueue` 只接收真正的 call/ret：

```scala
instr.valid && instr.bits.rasAction =/= BranchAttribute.RasAction.None
```

然后再逐项送给 BPU / RAS：

- 更新 `toBpu.commit.valid`
- 带上 `metaQueueCommit(...)`
- 带上 `attribute.rasAction`

所以完整链路是：

1. ROB commit
2. 下一拍 backend 发出 `callRetCommit`
3. frontend 过滤出真正 call/ret
4. `CommitQueue` 串行吐给 BPU / RAS

## commit 与 callRetCommit 的时序关系

在没有 flush 时，两者通常同拍到达 frontend。

共同点：

- 都来自 ROB 本拍 commit 结果
- 都打一拍后发送到 frontend

因此常见时间线是：

1. T：ROB 某拍发生 commit
2. T+1：`toFtq.commit.valid` 可能拉高
3. T+1：对应槽位的 `toFtq.callRetCommit(i).valid` 也可能拉高

## 为什么两者不严格绑定

关键差别是：普通 `commit` 受 ROB flush 抑制，而 `callRetCommit` 不受这个条件抑制。

普通 `commit` 要求：

```scala
!s0_robFlushRedirect.valid
```

`callRetCommit` 没有这个条件。

所以在“ROB commit 与 ROB flush 同拍”时，可能出现：

- `toFtq.commit.valid` 不发
- `toFtq.callRetCommit(i).valid` 仍在下一拍发出

这就是为什么不能把两者理解成严格绑定的一组事件。

## 一个常见误解

可以说：

- `callRetCommit` 可以早于 frontend 视角的 FTQ entry commit

但不能说：

- `callRetCommit` 早于 ROB commit

真实关系是：

- 它一定由 ROB commit 触发
- 只是它对应的是“指令级已提交信息”
- 而 frontend 的 FTQ entry commit 更接近“FTQ 边界推进”

这两个概念不是同一个粒度。

## 当前配置下的宽度理解

当前配置里 `CommitWidth = 8`，所以可以把系统理解成：

- ROB 每拍最多同时 commit 8 条指令
- `callRetCommit` 每拍最多同时出现 8 个有效槽位

但要注意：

- “最多 8 个 `callRetCommit.valid`”不等于“最多 8 个真正 call/ret”
- 其中很多槽位可能只是普通指令，最终会被 frontend 过滤掉

## Verification Model

对验证环境来说，这一节只提供“不要偏离真实职责分工”的一般建议。

真正的 Backend Agent 语义约束以
`docs/agents/frontend-backend-agent.md` 为准，尤其是：

- `commit` 必须保持 FTQ entry 粒度
- `commit` 必须严格保序
- `commit` 只应在对应 entry 已满足语义提交条件时出现
- `callRetCommit` 必须保持指令粒度
- 某条指令一旦 ROB commit，就可以独立地产生对应 `callRetCommit`
- 只有 `call` / `ret` 指令的 `rasAction` 具有非 `None` 语义

因此最需要避免的是：

- 把 `commit` 建模成无条件随机推进的边界脉冲
- 让后面的 FTQ entry 先于前面的 FTQ entry `commit`
- 把 `callRetCommit` 错当成 FTQ-entry-granular 事件
- 把 `callRetCommit.valid` 误解成“这一定是 call/ret”

## Current Env Mapping

若需要查看当前 Python env 的具体实现，请直接阅读
`src/test/python/Frontend/env/` 下的代码；但代码实现细节不应覆盖
`docs/agents/frontend-backend-agent.md` 定义的语义约束。

阅读顺序建议是：

1. 先按本文理解 RTL 中 `commit` 与 `callRetCommit` 的职责差异
2. 再按 `docs/agents/frontend-backend-agent.md` 理解验证环境必须保持的行为语义
3. 最后再去核对当前 env 的具体实现是否满足这些语义

## 源文档

- `docs/backend_frontend_callret_commit_timing.md`
- `docs/rob_flush_redirect_conditions.md`
