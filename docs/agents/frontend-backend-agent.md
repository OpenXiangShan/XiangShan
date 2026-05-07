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

## 双队列规范（强约束）

Backend Agent 的行为语义应等价于两个逻辑队列：

- `cfVec_queue`：接收 DUT `cfVec` 的观测流，承担路径判定和 wrong-path flush 语义
- `commit_queue`：只承载正确路径上的可提交指令，承担 `commit` / `callRetCommit` 的派生语义

### `cfVec_queue` 语义

- 所有有效 `cfVec` 指令按观测顺序入队
- queue 头与 golden trace 对比；匹配成功表示该指令位于正确路径
- 首次 mismatch 标记唯一的 wrong-path 起点，并在后续某个时刻触发 `redirect`
- 任意时刻只允许存在一段 active wrong-path episode；在该 episode 被 `redirect` 清除之前，不得把后续观测重新切成第二段 wrong-path，也不得因为中途看到某个临时 target、wait 条件或 FTQ pointer 变化而重置 wrong-path 起点
- mismatch 之后仍需继续接收并入队后续 `cfVec`，这些指令在语义上全部属于同一段 wrong-path，直到被 `redirect` 清除
- `redirect` 生效后，必须从这段 active wrong-path 的起点开始清除 `cfVec_queue`；更老正确路径前缀必须保留
- wrong-path 的清除边界必须由“恢复后重新建立 correct-path 对齐”的语义决定，而不是由中途某个临时 `target_pc`、`ftqidx`、等待态命中或类似局部现象决定
- `redirect` 发出后不得立即 flush active wrong-path；必须先等属于该 redirect 的第一条 recovery 指令以匹配的 `target_pc` 和预期 FTQ 身份真正进入 `cfVec_queue`，再从 wrong-path 起点 flush 到 recovery 边界

### `commit_queue` 语义

- 只接收来自 `cfVec_queue` 的正确路径指令，wrong-path 指令不得进入
- 指令进入 `commit_queue` 后，按程序序等待提交条件满足再出队
- 指令粒度的 `callRetCommit` 从 `commit_queue` 中“已提交”的指令派生
- FTQ-entry 粒度的 `commit` 从 `commit_queue` 头部连续、已提交且同 FTQ entry 的指令聚合派生

### 队列边界

- `redirect` 负责清除 wrong-path 的对象是 `cfVec_queue` 语义段，而不是“任意待提交状态”
- `commit_queue` 不允许通过“golden trace 已前进”直接推进，必须通过独立提交建模推进
- 任何实现如果等价地违背上述双队列职责边界，都应视为语义错误

## `commit_queue` source-of-truth 重构计划

适用背景：当 backend model 的 `commit` / `recovery` bookkeeping 同时维护
`_cfvec_queue`、`_commit_queue`、`rob_commit_state`、`commit_ptr`、`ftq_entries`
等多份可推进状态时，`redirect` 或 recovery 裁剪后容易出现队列头、queue
索引与 FTQ bookkeeping 漂移。对这类问题，后续实现应收敛到：

- `_cfvec_queue` 是唯一指令流 source-of-truth
- `commit` 候选每拍都从当前 `_cfvec_queue` 头部连续正确路径前缀现算
- 头部一旦出现 `wrong` 或 `unknown`，`commit` 必须阻塞，不得越过
- `redirect` / recovery flush 只裁剪 `_cfvec_queue` 及其配套的 pending
  `queue_index`
- `_commit_queue` 应删除，或最多降级为只读 debug view；不得再承担运行时主语义

### 目标语义

- 正确路径前缀的定义只来自当前 `_cfvec_queue` 的实时内容，不来自历史快照
- `commit`、`callRetCommit`、FTQ entry `commit` 都从这个实时前缀派生
- `commit` 资格链必须明确分层：instruction commit -> `callRetCommit`
  pending/scheduled/visible -> FTQ entry `commit`；后层事件不得绕过前层语义直接产生
- FTQ entry `commit` 只能由 queue head 连续、同 FTQ、全部 `correct`、全部
  `rob committed`、对应 CFI `resolve` 已 emitted、且 entry 边界已经确定的指令段聚合派生
- `wrong-path` / recovery 后缀只能通过 `redirect` 语义清除；不能靠旧
  `commit_ptr`、fallback commit 或 stale cleanup 偷偷跳过
- queue 裁剪后，所有 `queue_index` 类状态都必须重新解释为“指向当前
  `_cfvec_queue` 的位置”，不能再依赖已失效的历史位置
- `_cfvec_queue` 中的 entry 必须继续承载指令级与 FTQ 级语义注解，至少包括：
  `path_state`、`resolve_state`、`rob_commit_state`、`call_ret_commit_state`、
  `call_ret_ras_action`、`golden_index` / `target_pc`、`is_last_in_entry`、
  `ftq flag/value/offset`；不得把 `_cfvec_queue` 简化成“原始 `cfVec` 包队列”
- 分层必须保持：`_cfvec_queue` 是 instruction truth；`commit_ptr`、
  `pending_level0_target_ftq`、`ftq_entries` / `current_ftq_entry` 只允许作为
  FTQ-order / gating metadata，不能删除，也不能拿来绕过 semantic queue
- golden-trace 模式下 fallback commit 默认禁止；若保留极少数例外，必须同时满足
  `_cfvec_queue` 为空、无 active wrong-path / recovery、无 pending resolves、
  无 pending / visible / scheduled `callRetCommit`，且能证明对应语义流已通过
  redirect / commit 完整清空

### 分步实现计划

1. 收口语义边界
   验证：列出当前哪些状态真正参与 `commit` 候选、FTQ commit 聚合、recovery
   裁剪；确认运行时主语义只允许由 `_cfvec_queue` 和少量 pending
   `queue_index` 承载。
- 明确 `_commit_queue`、`commit_ptr`、`rob_commit_state`、`ftq_entries`
  里哪些是主状态，哪些只能保留为派生视图
- 若某个状态无法从 `_cfvec_queue` 当前内容重建，就先把它视为高风险漂移点
- 显式盘点 `callRetCommit` 的跨拍状态承载：`_pending_queue_call_ret_commit_indices`、
  `_scheduled_queue_call_ret_commit_groups`、`_visible_queue_call_ret_commit_group`
  必须保留，并纳入 flush/remap/invalidate 规则，而不是隐式塞回 `_commit_queue`

2. 把 `commit` 候选改为现算
   验证：在无 redirect 的正常 case 中，现算结果与旧行为等价；在出现
   `wrong` / `unknown` 头部时，`commit` 明确停住而不是跳过。
- 每拍从 `_cfvec_queue` 头部扫描连续正确路径前缀
- 仅在该前缀内判定 resolve 完成、call/ret 已提交、FTQ entry 是否可聚合
- 禁止从历史 `_commit_queue` 或旧 `commit_ptr` 直接恢复候选集合
- 先得到 instruction commit，再派生 `callRetCommit` pending/scheduled/visible，
  最后再判定 FTQ entry `commit`；不得把 `callRetCommit` 或 FTQ `commit` 当作反向驱动源

3. 把 recovery / redirect 裁剪改为只裁 `_cfvec_queue`
   验证：发生 flush 后，队列前缀、pending `queue_index`、FTQ commit
   聚合边界同步收口；不存在“queue 已裁掉，但 commit 侧还保留旧条目”。
- flush 只按 wrong-path episode 语义裁剪 `_cfvec_queue`
- 所有仍需保留的 pending `queue_index` 在 flush 后同步重定位或失效
- 不再维护独立的 `_commit_queue` flush / repair 路径
- recovery 不是简单 `remove range`：应裁掉 `[queue_start, target)`，把保留下来的
  wrong suffix 从 `WRONG` 复位为 `UNKNOWN`，必要时重新挂接 `resolve`，然后从
  queue head 按 golden match 语义 replay
- `callRetCommit` 相关跨拍状态在 flush 后必须显式处理：指向被删除 queue entry 的
  group 直接删除；保留 entry 的 `queue_index` 必须重定位；不得向已 flush 指令发出旧 group

4. 删除或降级 `_commit_queue`
   验证：主流程不再读取 `_commit_queue` 决策；若保留 debug view，它与当前
   `_cfvec_queue` 前缀可双向核对，但不会反向影响行为。
- 优先删除运行时读写入口
- 若短期保留 debug view，只能现算生成，只读输出

5. 收尾并补最小回归验证
   验证：307-entry `cfi_random_5inst_case` 这类 bin-trace 问题中，
   不再出现 `_commit_queue` / `commit_ptr` / `_cfvec_queue` 漂移驱动的假性
   commit 或 recovery bookkeeping 漂移。
- 检查 commit、callRetCommit、FTQ commit、redirect/recovery 的日志与状态
  解释都只围绕单一 semantic queue
- 若仍需额外 bookkeeping，必须证明它可由 `_cfvec_queue` 单向派生
- 最小回归类别至少覆盖：正常 correct path 的多拍 instruction->FTQ commit、
  first mismatch 后 redirect flush 与 recovery replay、recovery target 之后
  suffix `UNKNOWN -> MATCHED`、call/ret commit 后 scheduled/visible 正常发出且
  flush 后不会重发旧 group、307-entry `cfi_random_5inst_case`

### 禁止事项

- 不得通过修改 bin、裁短运行窗口、跳过问题区间等方式掩盖该漂移问题
- 不得放宽 completion、monitor 通过条件、stagnant/timeout 判定来换取“跑完”
- 不得引入 fallback commit、stale cleanup commit、按 pointer 排名补提交通道，
  绕过 golden semantic queue
- 不得把 `callRetCommit` 的 pending/scheduled/visible 跨拍状态折叠成“一拍现算即发”
  或在 flush 后继续复用旧 group
- 不得把 recovery 实现成只删除一段 queue 而不重置保留 suffix 的路径状态、
  resolve 挂接和 replay 对齐
- 不得把 `_cfvec_queue` 降级成仅存原始 `cfVec` 的包缓存，再把主语义搬到其他镜像结构
- 不得让 `_commit_queue` 继续作为与 `_cfvec_queue` 并列的第二份运行时真相
- 不得在主链路保留“flush 后再局部修补旧 commit 状态”的兼容逻辑；若现有状态
  无法自洽，应 fail fast 暴露语义错误

## 实现一致性最小检查项

下面的检查项按语义覆盖推导，条目数不预设；新增语义边界时应增补相应检查项。

### 必须项（违反即语义错误）

- `cfVec_queue` 入队严格按 DUT 观测顺序，不因等待目标 PC 而暂停或跳过包。
- 除非该指令在语义上被某次 `redirect` 作为 wrong-path flush 清除，否则任何已观测到的 `cfVec` 包都不得被丢弃、跳过采样或绕过入队。
- 首次 mismatch 只定义一个 wrong-path 起点，并沿该起点向后标记同一段 wrong-path。
- 任意时刻只允许存在一个 active wrong-path episode；在该 episode 被 `redirect` 清除之前，不得重新开第二个 wrong-path episode。
- mismatch 后继续接收 `cfVec`，不得进入“暂停构队列”等待模式。
- `redirect` 必须从 active wrong-path 起点开始清除 `cfVec_queue`，并保留更老 correct-path 前缀。
- `redirect` 的 flush 范围不得因为临时 `target_pc` 可见、waiting-target 命中、`ftqidx` 数值变化或类似局部条件而被拆成多段。
- `redirect` 发出本身只允许把系统带入 recovery-wait 状态；在 recovery target 真实入队前，不得提前执行 wrong-path queue flush。
- 某条 CFI 一旦已经与 golden 的某个动态实例匹配，其后续用于 `redirect` 的恢复目标必须绑定到该动态实例自身的 golden 语义，不得从一个可能已经漂移的全局 golden cursor 临时推导。
- 某条 `redirect` 的 `target`、`pc`、FTQ 上下文必须来自同一个动态实例；不得把 `target` 绑定到当前实例、却把 FTQ idx / offset 绑定到另一条更老或已失效的实例。
- 已被 earlier `redirect` 在语义上清除的 wrong-path 指令，即使暂时仍残留在内部结构中，也不得再参与后续 `redirect` 的归因、FTQ 上下文选择或 flush 范围计算。
- `commit_queue` 应由当前 queue 中连续的正确路径前缀重新派生，不应长期保留一个依赖旧 queue 索引的历史快照再靠局部修补维持；当 active wrong-path episode、recovery 或 queue 裁剪改变前缀边界时，`commit_queue` 必须随正确路径前缀同步收口
- `commit` 的候选范围只允许来自当前正确路径前缀；任何 `unknown` 或 `wrong` 后缀都不得通过“旧索引仍在 commit_queue 中”或类似历史残留的方式参与 commit 计划
- 进入 `commit_queue` 的仅为 correct-path 指令；wrong-path 指令不得进入 `commit_queue`。
- `commit_queue` 严格按程序序推进，不跳过更老未提交指令。
- 正确路径 CFI 在 `resolve` 完成后才允许对应指令进入 committed。
- `callRetCommit` 从“已提交指令”派生，且保持指令粒度。
- FTQ-entry `commit` 仅由 `commit_queue` 头部连续、同 FTQ、已提交指令聚合派生。
- FTQ entry 出队原因仅有两类：被 `commit` 退休，或被 `redirect` 作为 wrong-path 清除。
- 不得因为 stale cleanup、fallback commit、pointer-rank 裁剪、queue 中暂时不可见或类似内部整理路径，静默删除 wrong-path 指令或 FTQ entry bookkeeping。
- 若实现检测到这类“只能靠静默清账才能继续推进”的 stale 状态，应优先 fail fast 暴露语义错误，而不是在后台自动修补后继续运行。
- 禁止“已提交旧 FTQ entry 复活”为 active 来解释后续观测。
- delay 只作用于“已满足发送资格后的附加延迟”，不替代资格条件。

### 建议项（不满足时优先排查）

- `redirect` 之后的恢复残留优先视作同一恢复过程，而非直接开启新一轮 mismatch。

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
- `ftqidx` 只能作为“这条指令归属哪个 FTQ entry”的标签，不能被当成 queue 中条目的身份、位置索引或可复用槽位编号；queue 的唯一顺序语义来自 `cfVec` 观测顺序和 active wrong-path / correct-path 边界，而不是 FTQ pointer 数值本身

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

进一步地，`redirect` 的 wrong-path 清除边界是语义边界，不是“只覆盖 redirect 之前已经入 queue 的包”的物理时刻边界。因此：

- `redirect` 发出当拍仍然观测到的旧 wrong-path `cfVec`
- `redirect` 发出后若干拍内继续观测到的同一段旧 wrong-path residual `cfVec`

都必须继续归属于这一次 active wrong-path episode，并在语义上视为这次 `redirect` 要负责清除的对象。

换句话说：

- `redirect` 不得只清除发出前那一截 wrong-path，而把当拍或后几拍仍属于同一旧 wrong-path 的 `cfVec` 重新解释成新的 episode
- 这些 residual `cfVec` 可以在实现上先按观测顺序进入 queue，但语义上仍属于同一次 `redirect` 的清除范围
- 只有真正恢复到 correct-path 后，后续观测才可以重新参与新的 correct-path / wrong-path 判定
- 若某条正确路径 CFI 已经证明下一条 golden PC 不是其顺序后继，并且同拍后续 slot 中第一个有效 `cfVec` 不是该恢复目标，则 active wrong-path episode 必须在这个“同拍后续的第一个非目标 slot”处立即开始；不得等到下一拍或下一次局部 mismatch 再补记起点
- 只要系统仍处于“等待恢复目标重新建立对齐”的阶段，queue 中正确路径前缀之后的未知后缀也必须被并入这同一条 active wrong-path episode；不得把它们长期保留为 `unknown`，否则会错误阻塞 commit，并在后续恢复或再次重定向时形成非法中间态
- active wrong-path episode 的起点必须锚定在“当前正确路径前缀之后的第一个非正确条目”；不得因为更老正确路径仍留在 queue 中，就让 wrong-path 起点在这些更老前缀之前或之中漂移
- 实现中应优先用一个显式的 active wrong-path episode 状态来承载“起点、归因 CFI、恢复目标、是否仍在等待恢复完成”等主语义；不要再把这些主语义分散寄存在多个松散的 wait/recovery 辅助字段上，再靠条件分支拼接
- 任何“是否仍在 wrong-path / 是否仍在等待恢复 / 是否允许 commit fallback / 当前 wrong-path 起点在哪里”之类的全局判断，都应优先从这个显式 active episode 状态出发，而不是分别读取若干局部辅助字段再临时拼接
- 一旦 active wrong-path episode 已经建立，后续属于该 episode 的 mismatch / residual 处理应优先沿用 episode 中已经确定的归因 CFI 与恢复目标；不得在每次局部 mismatch 时重新向前搜索“前一条正确路径 CFI”，否则会在中途残留或恢复窗口中错误改写同一条 episode 的归因
- 在实现结构上，建议先把“如何为当前 active episode 推导归因 CFI / 恢复目标”的逻辑独立成单独步骤或 helper，再让 queue 状态更新、redirect 排队、recovery 进入等动作消费这个结果；不要把这些步骤混在一次局部 mismatch 处理函数里相互覆盖
- 对应地，即使“发现需要首次建账”的检测点暂时仍有多处，这些检测点也应尽量只负责发现条件；episode 的 origin/target/context 组装与真正建账动作应集中到统一 helper，而不是在每个检测点各自拼装一套状态
- 更进一步地，若多个路径本质上都在判断同一条规则，例如“某条正确路径 CFI 的下一条应为 `target_pc`，但实际看到的第一条有效 `cfVec` 不是它”，则这些路径也应尽量共享同一个判定 helper，而不是在 replay、采样、mismatch 等函数里各自实现一份近似逻辑
- 同样，首次建立 active wrong-path episode 的入口应尽量收敛到“正确路径 replay 发现 control-flow 后继不再顺序一致”的那个检测点；不要同时在多个局部 slot 检查、wait 命中或 residual 入口重复建账，否则同一条 episode 很容易被重复开启或被不同入口写出不一致状态
- `golden_wait` 一类状态只能辅助表达“恢复目标尚未重新观测到”这一事实，不得再主导 wrong-path episode 的起点选择、切分、扩展或终止；这些主语义必须统一由 active wrong-path episode 状态负责
- 因此，`golden_wait` 不应再额外携带或隐式承担“source FTQ identity”这类会反向影响主语义切分的职责；它只负责记录恢复目标是否已经重新观测到
- 同样，waiting-prefix 是否并入 wrong-path episode，应由“当前是否已有 active episode / 当前是否处于恢复阶段”决定，而不应再由 `golden_wait_requires_redirect` 这类单独的 wait 开关直接主导
- 对应地，未知后缀是否需要被收口进 active episode，也应由“当前是否已有 active episode / 当前是否仍处于恢复阶段”决定；不能再因为某个单独 wait 开关为真或为假而改变 wrong-path episode 的收口时机
- 同样，wait-target 是否命中这类辅助观测不应继续反向写入 current FTQ 的主 bookkeeping（例如作为决定 episode 边界或恢复完成的附加标志）；主链路应尽量只依赖 active episode 与 recovery target 本身
- 同理，`pending_redirect_origin_index` 这类字段应逐步降级为 active episode 的内部实现细节；外层逻辑不应继续把它当作对外公开的主状态直接读写，而应通过“当前 active episode 是否存在、其起点在哪里、其恢复目标是什么”这类 helper 访问
- 更具体地，运行时主状态应优先收敛到一个显式 episode 对象或等价单一承载体；若暂时保留 `_pending_redirect_origin_index` 一类旧字段，也只允许把它们作为镜像/兼容输出存在，而不再作为主读写源
- 对应地，一旦外围调试/测试/状态同步都已经切到统一的 active episode view，就应继续删除这类旧镜像字段在中间状态结构中的冗余存储，避免它们再次被误用成主状态来源
- 一旦主链路已经完成切换，禁止继续保留“兼容旧实现/旧状态机/旧字段读法”的运行时代码；后续修改应直接删除旧桥接，而不是再加一层兼容兜底
- 与此同时，若实现内部仍保留某些旧字段作为过渡期细节，则它们必须与显式 active episode 状态保持同步；不能出现“origin 已更新、但恢复目标或归因上下文还停留在旧值”的分裂状态
- 更进一步地，外层流程应优先通过统一的 active episode view / helper 读取“当前起点、恢复目标、归因上下文”，而不是在不同函数里分别直接读取多个底层字段后自行拼装；否则同一条 episode 很容易再次被不同入口读成不一致状态
- 同样，若 `pending_level0_target_ftq` 仍需要保留少量配套观测状态（例如目标 FTQ entry 中是否已经看到恢复目标 PC），这些状态也应收敛到专门 helper 中，且只能服务于 commit-side 辅助约束；不得在采样主链路中散落多处直接写入，再反向影响 episode 边界或恢复主判定
- active episode 本身还应显式区分至少两个阶段：\n  1. 已归因但尚未 drive redirect\n  2. redirect 已发，正在等待恢复目标重新建立对齐\n  不要再把这两个阶段分别塞给 `golden_wait`、`semantic_recovery`、`post_redirect` 等旧辅助状态去间接表达
- 一旦 active episode 进入“redirect 已发、等待恢复”阶段，后续流程需要的恢复目标 PC、resolve frontier fallback、是否仍在恢复中的判断，都应优先从 episode 读取；旧的 `golden_wait` / `semantic_recovery` / `post_redirect` 字段只能作为过渡兼容，不应继续成为主读取源
- 更具体地，若当前 active episode 已处于 recovery 阶段，则 `resolve` frontier、恢复目标读取以及相关 commit/cleanup 判定都应先读取 episode 中的 recovery target；不得先以全局 golden cursor 为主、仅在其缺失时才回退到 recovery target
- `pending_level0_target_ftq` 这类字段若仍保留，应尽量只服务于 commit 次序或 dispatch 辅助约束，不应再进入 wrong-path episode 的主判定、起点建立、切分或恢复控制流
- 更具体地，`pending_level0_target_ftq` 若存在，只表示“redirect 之后 commit 不得越过哪一个目标 FTQ entry”；它只能阻塞更年轻的 commit 候选，不能把这个目标 entry 自身也一起挡住
- 同一恢复阶段在控制流上应只有一个主入口：围绕当前 active episode 的 recovery target 展开。不要再让 `post_redirect`、`semantic_recovery`、`golden_wait` 各自拥有独立的优先分流入口，否则同一条 episode 会再次被多路状态机拆开
- 与之对应，恢复阶段的主读取入口也应统一：凡是需要回答“当前恢复目标是谁、现在是否仍在恢复中”的逻辑，都应优先通过一个统一 helper / view 从 active episode 读取，而不是直接读取多个旧字段再临时拼接
- 同样，进入恢复阶段的写入口也应统一：redirect 真正 drive 后，应通过一个集中 helper 一次性写入“恢复目标、恢复起点、阶段切换”等主状态，而不是在多个函数里分别散写 `semantic_recovery` / `post_redirect` / wait 相关字段
- 进一步地，对外围流程来说，“当前是否处于恢复阶段”也应只有一个统一 helper；commit gating、stale 清理、pending work 统计等外围逻辑不应再分别读取若干旧字段后拼接出恢复状态
- 对称地，退出恢复阶段的清空路径也应统一：当恢复完成、reset、queue 清空或显式注入重置主链路时，应通过一个集中 helper 一次性清空 recovery 相关旧字段，避免不同调用点只清一部分导致旧兼容状态残留
- 在过渡期内，旧的 `semantic_recovery` / `post_redirect` / `golden_wait` 字段若仍保留，也应尽量只作为兼容存储存在，而不再被外围流程主动读取；主读路径应优先走统一 helper，再逐步压缩这些旧字段的存在感
- 对 `golden_wait` 的读取也应遵守同一原则：外围流程应通过统一 helper 读取“当前 wait target 是否存在、其目标 PC 是什么”，而不要在多处直接读取 `_golden_wait_pc` / `_golden_wait_requires_redirect` 后自行组合含义
- 若实现中仍暂时保留 `_semantic_recovery_target_pc`、`_semantic_recovery_queue_start`、`_golden_wait_requires_redirect` 之类旧字段以服务调试输出、兼容观测或渐进迁移，它们也只能做“镜像/兼容写入”；主链路不得再把它们当成 recovery target、recovery start 或 wrong-path 存在性的主读取源
- 一旦主链路已经切到 active episode 语义，就不应再继续保留这些旧字段或旧接口作为运行时兼容兜底；后续重构应直接删除旧读写入口，而不是再加一层兼容桥接
- 当某些局部观测中间量（例如围绕 FTQ 指针是否“stale”或当前 queue 中是否已出现某个 FTQ pointer 的局部布尔量）已经不再参与主决策时，应及时从主流程中删除，避免旧模型的观察视角继续残留在核心路径里

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

## 正确路径匹配与 ROB 提交的边界

`golden trace` 在验证环境中的职责，是判定 queue 中哪些指令属于 correct-path，
哪些指令已经进入 wrong-path；它不是 backend ROB commit 的直接替代物。

因此：

- 一条指令与 golden trace 匹配成功，只表示它已经被证明位于正确路径
- 这条“匹配成功”本身，不等价于“backend 已经 ROB commit 了这条指令”
- 环境不应把 `golden trace` 的前进直接投影成 `rob_commit_state = committed`
- 环境必须再经过一个独立的 backend 提交建模阶段，才能把 correct-path 指令推进到 committed

对实现来说，这意味着：

- `golden match` 负责提供“未来可提交资格”
- 真正的 `rob commit` 需要由独立的提交前沿来决定
- `callRetCommit` 与 FTQ-entry `commit` 都应从这个独立的提交前沿派生

如果实现把“与 golden trace 对齐”直接当成“已经 ROB commit”，则它虽然可能还能跑通部分 testcase，
但语义上已经不再是在“模拟 backend 提交”，而是在用 golden trace 直接驱动 backend 事件。

## 独立的 backend 提交前沿（`commit_queue`）

为了尽可能模拟真实 backend，环境应在 `cfVec_queue` 之上维护一个独立的、顺序的
instruction commit frontier（语义上即 `commit_queue`）。

这个 frontier 的最小要求是：

- 只允许从 queue 头开始按程序顺序推进
- 不能跳过更老且尚未提交的 correct-path 指令，去提交后面的指令
- wrong-path 指令不能进入 committed
- 正确路径上的 CFI，必须在对应 `resolve` 已经完成之后，才能进入 committed

因此，一个最小等价实现应满足：

- `golden match` 先把指令标成 correct-path
- 独立的提交调度器再把 queue 头连续前缀中的可提交指令推进到 committed
- `callRetCommit` 从这些“已经 committed 的单条指令”派生
- FTQ-entry `commit` 从这些“已经 committed 且位于 queue 头部的整 entry”派生

实现可以使用不同的数据结构，但不能绕过这条独立 frontier，例如：

- 不能在 `golden match` 的同一动作里顺手把指令直接标成 committed
- 不能因为某个 FTQ entry 的局部状态看起来 ready，就跳过更老未提交指令
- 不能用一个内部 shortcut，把多个 FTQ entry 当成一次对外 `commit`

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
- 环境不应把 `callRetCommit` 建模成“golden trace 对齐事件”
- 环境应把它建模成“独立 backend 提交前沿推进后，按指令派生出来的事件”

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
