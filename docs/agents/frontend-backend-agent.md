# Frontend Backend Agent 语义说明

本文档描述 Frontend 验证环境中 Backend Agent 的语义要求。

这里约束的是行为语义，不约束具体实现细节。实现可以使用不同的数据结构、调度方式或内部状态，
但对外行为必须与本文档等价。

## 总体模型

首先，采集 DUT 的输出 `cfVec` 信号，并将其中所有有效指令按出现顺序依次放入一个逻辑 queue 中。

由于 Frontend 中的 BPU 只负责预测，因此 `cfVec` 中的指令流可能因为 BPU 预测错误而与 golden trace
产生偏差。Backend Agent 的职责，就是基于这个 queue 与 golden trace 的比较结果，生成语义等价的：

- `redirect`
- `resolve`
- `commit`
- `callRetCommit`

## Queue 中每条指令需要保存的信息

queue 中的每条指令至少需要具备以下语义信息：

- `cfVec` 的完整信息
- 该指令当前位于正确路径还是错误路径
- 若该指令是 CFI，则标记它是否已经被 `resolve` 过
- 该指令是否已经具有对应的 `callRetCommit`
- 该指令属于哪个 FTQ entry（例如由 `ftq_flag` / `ftq_value` 标识）

其中：

- “正确路径”表示这条指令与 golden trace 对齐
- “错误路径”表示这条指令属于某次错误预测之后、尚未被 redirect 清除的路径

## FTQ Entry 与 Queue 的关系

对 Backend Agent 而言，`cfVec` 中携带的 FTQ pointer（例如 `ftq_flag` / `ftq_value`）的主要作用，是把指令归属到 queue 中某个 FTQ entry。

这里的语义边界需要明确：

- 一个 FTQ entry 只要仍然在 queue 中存在未出队指令，就说明它在语义上尚未结束
- 在这种情况下，后续再次观测到相同 FTQ pointer 的 `cfVec` 指令，应首先解释为该 FTQ entry 的后续观测
- 不能因为“这个 FTQ pointer 之前见过”就把当前观测自动解释成一条新的独立 entry

换句话说，对 env 来说：

- “这是不是同一个 active FTQ entry”

主要取决于：

- queue 中是否仍然存在该 FTQ entry 的未出队指令

而不取决于：

- 历史上是否见过相同 FTQ pointer

因此：

- 若 queue 中仍存在某个 FTQ pointer 对应的旧指令，则后续相同 FTQ pointer 的观测仍属于该 active entry
- 只有当该 FTQ entry 已经在语义上结束，并且其相关指令已经从 queue 中清除之后，后续再次出现相同 FTQ pointer，才可以被解释为另一条新的 entry

这里的“已经从 queue 中清除”只允许有两种合法原因：

- 收到该 FTQ entry 对应的 `commit`
- 该 FTQ entry 中相关指令属于 wrong-path，并被某次 `redirect` flush 清除

如果实现中出现下面这种现象：

- 某个 FTQ entry 已经被视为结束
- 但 queue 中其实仍残留该 FTQ pointer 的旧指令
- 同时后续又把相同 FTQ pointer 当成新的 entry 重新建模

则这应被视为实现语义错误，而不是合法行为。

进一步地，若实现只有通过“重新接纳一个已经落后于当前 `commit_ptr` 的旧 FTQ entry”才能解释后续观测结果，
则正确的语义结论应当是：

- 之前某次 `commit` 的发送条件判断错误
- 该 `commit` 发早了

而不应把这种情况解释为：

- 旧 FTQ entry 在语义上又重新变成 active
- 或者该旧 FTQ entry 可以在 `commit` 之后合法回到 queue 中

## `redirect` 的产生语义

环境需要不断从 queue 中取指令，并按程序顺序与 golden trace 对比。

对比时只有两类结果：

### 1. 对比成功

如果某条指令与 golden trace 对比成功，则说明该指令位于正确路径上。

### 2. 对比失败

如果某条指令第一次与 golden trace 对比失败，则这通常意味着：

- 第一条失败的指令不一定是 CFI 指令
- 但在常见的控制流错误预测场景里，它的上一条正确路径指令应当是触发该偏差的 CFI 指令
- BPU 在该处发生了预测错误
- 从这条指令开始，后续已经入队的一系列指令都会落在错误路径上

此时，环境必须在之后的某一个时刻发送 `redirect`，将执行路径恢复到正确路径。

这里需要强调 queue 的持续接收语义：

- 一旦出现第一条 mismatch，不能停止接收后续 `cfVec`
- 后续 `cfVec` 仍然要继续按观测顺序入队
- 在后续 `redirect` 生效之前，这些新入队指令都处于本次错误路径语义之下
- 环境不应因为进入某种“等待目标 PC”状态，就跳过正常入队或绕过 queue 的路径标记语义

也就是说，mismatch 之后的正确处理不是“暂停 queue，等待目标 PC 再继续比较”，而是：

- 继续接收并入队
- 将这段指令视作尚未被恢复的错误路径
- 等待后续 `redirect` 统一清除这段错误路径
- 清除之后，再从剩余 queue 头重新开始与 golden trace 当前未消费位置匹配

`redirect` 一旦生效，同时还必须完成以下语义动作：

- flush queue 中所有属于错误路径的指令

也就是说，`redirect` 不只是“通知 frontend 改变目标”，还承担“清除当前错误路径指令”的语义责任。

更具体地说，`redirect` 对 queue 的清除语义应当满足：

- 错误路径的起点是“本次第一条 mismatch 在 queue 中的位置”
- 当用于恢复本次错误路径的 `redirect` 生效时，queue 中自该位置起、属于本次错误路径的那一段必须被清除
- 更老的正确路径指令不能因为 `redirect` 被一并删除，它们仍应保留在 queue 中等待后续 `commit`

因此，`redirect` 清除 wrong-path 的本质不是“简单按 FTQ idx 删除某几个 entry”，而是：

- 以本次第一条 mismatch 为语义起点
- 清除当前 queue 中尚存的这段错误路径

### `redirect` 后仍观测到旧 wrong-path 内容时的处理

`redirect` 发出后，DUT 可能不会在下一拍立刻恢复到正确路径。

可能出现的现象是：

- `redirect` 生效后的下一拍或后续若干拍，`cfVec` 仍然输出旧 wrong-path 的一部分残留内容
- 再过若干拍后，`cfVec` 才真正回到 correct-path

对于这种场景，环境的语义处理应当是：

- 这些 post-redirect 的旧 wrong-path 输出，应视为同一次恢复过程中的残留内容
- 它们不应被再次当成一轮新的错误路径起点
- 它们也不应推动 golden trace 向前消费
- 环境应继续等待，直到真正观察到恢复后的 correct-path 再重新进入正常匹配流程

也就是说：

- redirect 之前的 wrong-path：正常入 queue，等待本次 `redirect` 清除
- redirect 之后、恢复完成之前再次出现的旧 wrong-path：属于恢复残留，不应重新开启新的 mismatch / flush 周期
- 只有当恢复后的 correct-path 真正被观测到时，环境才重新从 queue 头继续与 golden trace 匹配

更具体地说，如果实现已经知道某个恢复目标 PC，但该 PC 在当前 queue 中并不位于 queue 头部，
则其前面的那些指令应当解释为：

- 上一次 `redirect` 恢复过程中的残留内容

而不是：

- 新的正确路径前缀
- 或新一轮独立 mismatch 的起点

因此，在这种场景下：

- 位于该目标 PC 之前的这段前缀不推动 golden trace 前进
- 它们不触发新一轮 redirect / flush 周期
- 它们继续按“上次 redirect 的恢复残留”语义处理，直到真正恢复到 queue 头正确对齐为止

如果在合理窗口内始终没有观察到恢复后的 correct-path，则这应被视为 recovery failure，而不是简单地把所有 post-redirect 残留内容再建模成一次新的错误路径。

因此，对于 queue 与 golden trace 的关系，应始终满足：

- queue 反映 DUT 实际观测到的取指流
- golden trace 只在“当前 queue 头成功匹配”时才向前消费
- 如果当前 queue 头不匹配，则 golden trace 保持在当前未消费位置，直到错误路径被 `redirect` 清除

## `resolve` 的产生语义

`resolve` 的信息不要求保序。

环境需要从 queue 中查找 CFI 指令，并按以下规则生成 `resolve`：

### 1. 正确路径上的 CFI

对于正确路径上的每一条 CFI，都必须发送对应的 `resolve` 信息。

### 2. 错误路径上的 CFI

对于错误路径上的 CFI，在它们尚未被 `redirect` flush 掉之前：

- 可以发送 `resolve`
- 也可以不发送 `resolve`

两种行为都符合语义要求。

因此，`resolve` 的核心约束是：

- 正确路径 CFI 必发
- 错误路径 CFI 可发可不发
- `resolve` 本身不要求严格保序

## `commit` 的产生语义

`commit` 是以 FTQ entry 为粒度的。

当 `commit` 有效时，表示一个 FTQ entry 中的所有指令都已经被 ROB 提交完成。

ROB 的提交顺序必须与 golden trace 一致，因此能够进入 `commit` 语义的这些指令，必然都位于正确路径上。

### `commit` 生效后的 queue 语义

当 DUT 收到某个 `commit` 后，queue 中与该 FTQ entry 对应的所有相关指令都必须出队，并且这些指令必须满足：

- 全部都位于 queue 头部

如果相关指令不在 queue 头部，则应视为语义错误。

这里的职责边界也需要明确：

- “与 golden trace 匹配成功”只说明该指令被标记为正确路径
- 匹配成功本身不会让指令立即出队
- queue 的真实出队时机只能由后续 `commit` 决定

因此正确路径指令在 queue 中的生命周期应为：

- 先入队
- 再与 golden trace 对比并标记为正确路径
- 继续留在 queue 中等待对应 FTQ entry 的 `commit`
- 直到 `commit` 到来时，才从 queue 头整体出队

需要强调的是，正确路径指令的出队原因只能是：

- 对应 FTQ entry 收到 `commit`

而 wrong-path 指令的出队原因只能是：

- 被某次 `redirect` flush 清除

实现不应引入第三种语义出队原因，来在 queue 中“静默删除”一个 FTQ entry。

### `commit` 的发送时机

一个 FTQ entry 可以发送 `commit`，至少需要满足以下语义条件：

- 该 FTQ entry 当前位于 queue 头部
- 它位于正确路径上
- 其中相关的 CFI 指令已经被 `resolve`
- 该 FTQ entry 中不存在仍需触发的 `redirect`
- `commit` 必须严格保序发送

上面这条“当前 FTQ entry 中不存在仍需触发的 `redirect`”，在语义上通常已经隐含在
“该 FTQ entry 全部位于正确路径上”之中；如果该 entry 内仍有会触发恢复动作的错误路径
控制流，它就不应被视为可提交。

这里的“严格保序”表示：

- 后面的 FTQ entry 不能先于前面的 FTQ entry 提交
- `commit` 的对外顺序必须与 ROB / golden trace 所代表的正确提交顺序一致
- queue 头如果仍然被更老 FTQ entry 占据，则更年轻 FTQ entry 不能因为自身局部状态“看起来 ready”就先发 `commit`

### 延迟语义

若实现中为 `commit`、`resolve`、`redirect` 等行为加入 delay，该 delay 的语义应为：

- 先满足该行为的发送条件
- 再额外等待若干周期
- 等待结束后才真正对外发送

也就是说，delay 表示“已经具备发送资格之后的附加延迟”，而不是“用延迟替代发送条件本身”。

## `callRetCommit` 的产生语义

`callRetCommit` 是以指令为粒度的，不是以 FTQ entry 为粒度的。

只要某条指令已经被 ROB 提交，对应的 `callRetCommit` 就可以有效。

但在语义上只有 `call`、`ret` 指令的 `rasAction` 不为 `None`。

因此：

- `callRetCommit` 的有效条件是“该指令已经被 ROB 提交”
- `callRetCommit` 的语义粒度是“单条指令”
- 只有 `call` / `ret` 指令会携带有意义的 `rasAction`

## 四类信号之间的关系

为了避免歧义，可以把四类行为理解为：

- `redirect`：负责把错误路径拉回正确路径，并清除错误路径指令
- `resolve`：描述 CFI 的解析结果；对正确路径 CFI 是必需事件
- `commit`：描述一个 FTQ entry 已经整体完成 ROB 提交
- `callRetCommit`：描述某条已提交指令在 call/ret 语义上的提交事件

它们之间的关键约束如下：

- 错误路径最终必须被 `redirect` 清除
- 正确路径上的每条 CFI 最终必须被 `resolve`
- 一个 FTQ entry 只有在满足提交条件后才能 `commit`
- 某条指令只要已经被 ROB 提交，就可以独立地产生 `callRetCommit`

## 实现自由度与不可改变的语义边界

实现时可以改变的只有“怎么做”：

- 可以不真的使用物理 queue，只要行为等价
- 可以使用更复杂的内部状态或缓存
- 可以采用不同的调度策略来决定何时实际发信号

但下面这些语义要求不能改变：

- `cfVec` 逻辑上必须被视为按顺序进入同一条指令流
- 第一条与 golden trace 失配的位置定义了错误路径的开始
- 该第一条失配本身可以不是 CFI，但其前一条正确路径指令在语义上应当对应本次偏差的 CFI 起点
- `redirect` 必须承担恢复正确路径并清空错误路径的职责
- 正确路径上的每条 CFI 必须有 `resolve`
- 错误路径上的 CFI 是否有 `resolve` 不作强制要求
- `commit` 必须是 FTQ entry 粒度且严格保序
- `callRetCommit` 必须是指令粒度

如果新的实现不能满足上述语义，即使内部结构更复杂，也不应视为与本文档等价。

## 相关实现方案

若需要按照本文语义改造现有环境，参见：

- `docs/superpowers/specs/2026-04-13-frontend-backend-agent-semantic-alignment-plan.md`
