# Backend Redirect Notes

## 主题结论

backend 中的 `redirect` 表示：当前 backend 判断前端取指路径必须被修正或刷新。  
它是控制动作，不是单纯的信息上报。

和 `resolve` 的区别可以压缩成一句话：

- `resolve` 问“这条 CFI 实际发生了什么”
- `redirect` 问“backend 现在是否必须立刻改正前端路径”

## 来源概览

frontend 最终看到的 backend redirect 只来自两条主路径：

1. ROB flush 路径
2. `RedirectGenerator` 路径

而 `RedirectGenerator` 只在两类输入中选最老者：

- `oldestExuRedirect`
- `loadReplay`

因此可把 backend redirect 来源拆成三类实际源头：

- EXU redirect：branch / jump / CSR xret
- memory violation / rollback redirect
- ROB flush redirect

## EXU redirect

对 branch 和 jump，局部 redirect 都来自执行单元输出。

条件分支：

```scala
redirect.valid := io.out.valid && (isMisPred || redirect.bits.hasBackendFault)
```

跳转：

```scala
redirectValid := io.in.valid && !jumpDataModule.io.isAuipc &&
  (needRedirect || redirect.hasBackendFault)
```

这里没有“必须等更老 branch resolve 完成后才能发 redirect”的门控。  
只要这条指令先完成、且自己满足 redirect 条件，它的 EXU redirect 就可能先产生。

## redirect 仲裁的真实规则

backend 不是在“所有尚未 resolve 的 branch”里等待最老者，  
而是在“当前已经产生出来的 redirect 候选”里选择最老者。

`Region` 的关键选择逻辑是：

```scala
val oldestOneHot = Redirect.selectOldestRedirect(exuRedirects)
val oldestExuRedirect = Mux1H(oldestOneHot, exuRedirects)
```

`RedirectGenerator` 继续在当前输入候选里仲裁：

```scala
val allRedirect: Vec[ValidIO[Redirect]] = VecInit(oldestExuRedirect, loadRedirect)
val oldestOneHot = Redirect.selectOldestRedirect(allRedirect)
```

再往上，ROB flush 还能覆盖 EXU / loadReplay redirect：

```scala
private val s1_s3_redirect = Mux(s1_robFlushRedirect.valid, s1_robFlushRedirect, s3_redirectGen)
```

直接结论：

- 更年轻的 CFI 可以先出现 `resolve`
- 它也可以先出现并触发自己的 `redirect`
- 只有当更老的 redirect 候选已经进入仲裁集合时，older-first 才会优先选择更老者
- ROB flush 的优先级高于 EXU / load replay redirect

## resolve ordering 与 redirect ordering

这两者不等价，但差异点不是“年轻 redirect 一定要等老分支 resolve”。

更准确的说法是：

- `resolve` 反映执行完成顺序
- `redirect` 反映当前有效 redirect 候选中的 oldest 仲裁结果

因此：

- 年轻 CFI 可以先 `resolve`
- 也可能先 `redirect`
- backend 不会因为还有更老 CFI 尚未 resolve 就自动压住它

## robFlushRedirect 的触发条件

`robFlushRedirect` 来自 `ROB.io.flushOut`。  
其 `valid` 条件可概括为：

1. ROB 处于 `s_idle`
2. ROB 头指令有效
3. 头指令满足下列任一事件：
   - 可安全响应中断
   - 在提交点确认发生异常 / 单步 / debug trigger
   - 提交时触发 `flushPipe` 或 `replayInst`
4. 上一拍没有刚发过 flush

这条路径本质上是“ROB 头指令已经走到必须由 ROB 发起前端 redirect / pipeline flush 的提交点”。

## robFlushRedirect 对 commit 的影响

ROB flush 不只是发 redirect，还会抑制普通 commit。

ROB 侧：

```scala
io.commits.isCommit := state === s_idle && !blockCommit
```

`CtrlBlock` 侧发给 frontend 的普通 `commit` 也显式屏蔽 `s0_robFlushRedirect.valid`。

因此一旦 `robFlushRedirect` 生效，常见现象是：

- ROB commit 被挡住
- frontend 的普通 `commit` 也被挡住

## redirect.level 语义

`level` 是一位：

- `0 = RedirectLevel.flushAfter`
- `1 = RedirectLevel.flush`

语义区别是：

- `flushAfter`：保留当前指令，flush 当前指令之后的内容
- `flush`：连当前指令自身一起 flush

## 哪些情况会产生 `level = 1`

会产生 `level = 1` 的只有下面几类：

- ROB flush 中的 replay instruction
- ROB flush 中的 interrupt
- ROB flush 中的 exception
- `NewLoadUnit.matchInvalid`
- `LoadQueueRAW` rollback
- `LoadQueueUncache` rollback

不会产生 `level = 1` 的常见路径：

- branch mispredict
- jump redirect
- CSR xret
- 单纯 `flushPipe`
- `NewLoadUnit.rarViolation`

## Verification Model

验证环境里的 `redirect` 不应被建模成“完全独立的随机延迟通道”。

更接近真实 backend 的做法是：

- `redirect` 可以有 delay
- 但这个 delay 必须受原因和仲裁语义约束

推荐遵守下面的规则：

- `redirect` 不能早于它的触发原因成立
- 如果某个 `redirect` 来自某条 mispredict `resolve`，它不能早于对应 `resolve`
- 不要人为加入一种规则：“更年轻 redirect 必须等更老 unresolved branch”
- 但也不要用随机 delay 打破“当前 ready 候选里 oldest 优先”的语义
- `ROB flush` / exception / watchdog 之类控制事件应优先于普通 commit 路径生效

因此，对 testcase 来说，合理的随机化方式是：

- 在事件 ready 之后，给 `redirect` 一个受约束的小范围 delay
- 或者把 delay 放在“事件排队 ready 之前”，而不是在 drive 前再任意漂移

不合理的方式是：

- 让 `redirect` 早于 cause 出现
- 用随机 delay 强行让更年轻 redirect 永远晚于尚未 resolve 的更老 CFI
- 让 `redirect` 穿透已经排队的更高优先级控制事件

如果讨论的是 Backend Agent 的规范语义，还需要额外满足
`docs/agents/frontend-backend-agent.md` 中的要求：

- 第一条与 golden trace 失配的位置定义错误路径的开始
- 之后必须在某个时刻发出 `redirect`，把路径恢复到正确路径
- `redirect` 生效时必须 flush 掉 queue 中所有错误路径指令

## Redirect Flush 实现规则

下面的规则约束的是 env 在 `redirect` drive 与 wrong-path flush 之间必须保持的
实现语义，用来避免“旧 wrong-path 没清干净”或“把已恢复的新 correct-path 误删”。

### 1. flush 的对象按观测时间划分，不按当前 queue 头部猜测

- 一条 `redirect` 只清除“在它对应的 wrong-path episode 中观测到的指令”
- 不能因为当前 queue 头已经漂移到更年轻 FTQ，就回头把更早 episode 的 flush
  边界重新解释
- 也不能因为看到了某个 `target_pc`，就把更早拍已经采到的 younger `cfVec`
  自动当作已恢复正确路径

实现上应先回答两个问题：

- 这条 `redirect` 归因到哪一个动态 CFI 实例
- 在该实例之后、恢复 target 真正重新建立 correct-path 对齐之前，哪些 `cfVec`
  采样仍属于同一段 wrong-path

### 2. redirect drive 当拍观察到的 `cfVec` 不可能由该拍 redirect 修正得到

当前 env 的时序是：

1. 先采样 DUT / `cfVec`
2. 再规划 cycle actions
3. 最后 `drive_redirect`

因此：

- 与某条 `redirect` 同拍观测到的 `cfVec`，不能被解释为“这条 redirect 已生效后的恢复结果”
- 若同拍观测值的 FTQ / PC 上下文与预期恢复路径不一致，应优先把它视作旧 wrong-path residue

这个规则尤其用于排除一种常见误判：

- 某拍发出 `redirect(pc=A,target=T)`
- 同拍或下一极近拍看到了 `pc=T`
- 于是直接把该样本判成“恢复后的正确路径”

若没有额外的 cycle-accurate 证明，这种判法默认不成立。

### 3. recovery target 的首次重新对齐必须同时满足 PC 与 FTQ 语义

看到 `target_pc` 本身还不够；是否已经恢复到 correct-path，至少要同时满足：

- PC 与该动态 CFI 的 golden target 一致
- 该样本的 FTQ 上下文与当前 redirect 的 flush / reuse 语义一致

不能接受下面这种情况：

- PC 恰好等于 `target_pc`
- 但 FTQ idx 已经漂移到明显更年轻的 slot
- 仍把它判成恢复后的正确路径

若 `redirect` 并不允许复用同一 FTQ slot，则恢复 target 的首个正确路径样本必须落在
预期的 younger FTQ 上，而不是任意更年轻的漂移 slot。

### 4. flush 范围必须覆盖“旧 redirect 已恢复之后，新 wrong-path 再次扩散”的所有 younger 样本

对于 back-to-back redirect / nested recovery 风险，后一个 `redirect` 的 flush 范围不能只看：

- 该拍 queue 里标成 `wrong` 的当前后缀

还必须覆盖：

- 在上一条 redirect 恢复出的 correct-path 起点之后
- 到当前这条 redirect 真正重新建立 correct-path 对齐之前
- 所有已采到的 younger wrong-path `cfVec`

否则会出现：

- 老 wrong-path 已被 earlier redirect 清过
- 新 wrong-path 又在恢复后继续扩散
- 但 flush 只裁了其中一小段，留下更年轻 residue

### 5. 推荐实现步骤

对每条 ready redirect，推荐按下面顺序做 flush 判定：

1. 绑定触发它的动态 CFI 实例：`pc`、`target`、`ftq idx`、`ftqOffset`
2. 找到该实例在当前 semantic queue 中的 wrong-path 起点
3. 以“首次重新建立 correct-path 对齐”为终点求 flush 区间
4. 终点判定同时检查 PC 与 FTQ 语义，不只看 `target_pc`
5. 删除该区间全部 younger wrong-path 样本
6. 对保留下来的 suffix 重新标成 `UNKNOWN` / replay，而不是沿用旧 path_state

### 6. fail-fast 条件

出现下面任一现象时，不应静默继续：

- 同一 redirect 之后，queue 中仍残留比恢复点更年轻的旧 wrong-path 样本
- 首次命中 `target_pc` 的样本 FTQ 明显过年轻，却仍被接受为恢复点
- 需要依靠“下一条 redirect 再补清一次”才能收口上一条 redirect 漏掉的 residue
- flush 之后 `commit` 仍只能通过跳过中间 FTQ hole 才能推进

这些都应视为 redirect flush 语义错误，而不是可接受的中间态。

## Current Env Mapping

若需要查看当前 Python env 的具体实现，请直接阅读
`src/test/python/Frontend/env/` 下的代码；但实现细节不应覆盖
`docs/agents/frontend-backend-agent.md` 定义的语义约束。

阅读顺序建议是：

1. 先按本文理解 RTL 中 `redirect` 的来源、仲裁和 `level` 语义
2. 再按 `docs/agents/frontend-backend-agent.md` 理解验证环境必须保持的 `redirect` 语义
3. 最后再核对当前 env 的实现是否满足这些语义

## 一个实用判断框架

如果某条 backend redirect 看起来“来得太早”或“覆盖了别的 redirect”，优先按下面顺序判断：

1. 它来自 EXU、load replay 还是 ROB flush
2. 当前拍有哪些 redirect 候选已经真正进入仲裁集合
3. 是否有更高优先级的 ROB flush 覆盖了 EXU redirect
4. `level` 是 `flushAfter` 还是 `flush`

## 源文档

- `docs/backend_resolve_redirect_relationship.md`
- `docs/backend_resolve_redirect_ordering_case.md`
- `docs/rob_flush_redirect_conditions.md`
- `docs/backend_redirect_level.md`
