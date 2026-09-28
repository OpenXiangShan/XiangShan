# Frontend BackendModel 设计与维护规范

本文档是 Frontend 验证环境中 BackendModel 的唯一设计与维护入口。

这里约束的是行为语义，不约束具体实现细节。实现可以使用不同的数据结构、调度方式或内部状态，
但对外行为必须与本文档等价。

除明确标记为 `[实现]` 的章节外，本文内容均为稳定语义规范：

- `[规范]`：修改实现时必须保持的外部行为和 fail-fast 边界；
- `[实现]`：当前 `backend_model.py` 的代码映射，随实现变化更新；

## [规范] 总体模型

首先，采集 DUT 的输出 `cfVec` 信号，并将其中所有有效指令按出现顺序依次放入一个逻辑 queue 中。

由于 Frontend 中的 BPU 只负责预测，因此 `cfVec` 中的指令流可能因为 BPU 预测错误而与 golden trace
产生偏差。Backend Agent 的职责，就是基于这个 queue 与 golden trace 的比较结果，生成语义等价的：

- `redirect`
- `resolve`
- `commit`
- `callRetCommit`

## [规范] Backend mode

运行模式由 testcase/environment 的 `BackendConfig.backend_mode` 明确配置：

- `auto_bin`：bin-trace/golden-trace 自动模式。加载 golden trace 时由
  `set_golden_trace()` 切换到该模式；模型负责 golden mismatch 和 fetch-fault recovery，
  并禁止 testcase 额外显式注入与自动策略竞争。
- `auto_python`：普通 Python testcase 默认模式。没有 golden trace 时，观测到的普通 cfVec
  由模型维护 queue、resolve 和 commit；testcase 可以显式构造 redirect 等额外场景。
- `manual_python`：Python directed 模式。模型仍维护 queue、resolve、commit 和合法性检查，
  但不自动排 golden wrong-path redirect，由 testcase 显式决定 recovery 动作。

本文后续所有“queue 与 golden trace 比较后自动产生 redirect”的规则主要约束
`auto_bin`。不带 golden trace 的 Python 模式仍必须遵守相同的 queue、FTQ identity、
redirect flush、resolve 和 commit 合法性边界，但路径归属和 recovery 可以由 testcase 的显式
场景合同提供。

exception-marked cfVec 在所有模式下都必须保留异常身份，且不能当作普通 CFI、resolve 或
commit 候选。`auto_bin` 对 fetch fault 自动排 backend recovery redirect；`auto_python` 和
`manual_python` 保持 source live，由 directed testcase 决定是否以及何时显式 recovery。

## [规范] 指令队列与提交（强约束）

BackendModel 只持久保存一条指令队列。instruction commit 和 FTQ-entry commit
都从这条队列的头部现场计算，不另存一份提交队列或提交边界：

- `cfVec_queue`：接收 DUT `cfVec` 的观测流，承担路径判定和 wrong-path flush 语义
- instruction commit：每周期从 `cfVec_queue` 头部连续 correct-path 前缀中选出可提交指令
- FTQ-entry commit：从队列头连续、同 FTQ、已提交且 entry 边界已确定的指令聚合派生

### `cfVec_queue` 语义

- 所有有效 `cfVec` 指令按观测顺序入队
- queue 头与 golden trace 对比；匹配成功表示该指令位于正确路径
- 首次 mismatch 标记唯一的 wrong-path 起点，并在后续某个时刻触发 `redirect`
- 任意时刻只允许存在一段 active wrong-path episode；在该 episode 被 `redirect` 清除之前，不得把后续观测重新切成第二段 wrong-path，也不得因为中途看到某个临时 target、wait 条件或 FTQ pointer 变化而重置 wrong-path 起点
- mismatch 之后仍需继续接收并入队后续 `cfVec`，这些指令在语义上全部属于同一段 wrong-path，直到被 `redirect` 清除
- `redirect` 生效后，必须从这段 active wrong-path 的起点开始清除 `cfVec_queue`；更老正确路径前缀必须保留
- wrong-path 的清除边界必须由“恢复后重新建立 correct-path 对齐”的语义决定，而不是由中途某个临时 `target_pc`、`ftqidx`、等待态命中或类似局部现象决定
- 模型在周期 `T` 计划 redirect 时，`T` 的 `cfVec` 已经是 DUT 在 redirect 输入生效前的输出，必须按正常语义采样；不得因为本周期稍后会 drive redirect 而提前丢弃它
- 令 `M` 为 DUT 首次看到 `redirect_valid=1` 的上升沿。`M` 和 `M+1` 观测到的 `cfVec` 不得采样入队；从 `M+2` 起进入 recovery，出现的第一条有效 recovery `cfVec` 必须是该 redirect 的 target

### Instruction commit 与 FTQ commit

- instruction commit 只允许从 `cfVec_queue` 头部按程序序选出连续可提交指令
- `wrong`、`unknown`、exception 或未完成 resolve 的 CFI 都会停止本周期的候选扫描
- 指令粒度的 `callRetCommit` 从已经标记为 committed 的指令派生
- FTQ-entry 粒度的 `commit` 从 `cfVec_queue` 头部连续、已提交且同 FTQ entry 的指令聚合派生

### 队列边界

- `redirect` 负责清除 wrong-path 的对象是 `cfVec_queue` 语义段，而不是“任意待提交状态”
- instruction commit 不允许通过“golden trace 已前进”直接推进，必须通过独立提交建模推进
- 任何实现如果把 golden match 直接当成 commit，或另存一份会漂移的提交队列，都应视为语义错误

## [实现] 当前 BackendModel 映射

当前实现位于 `src/test/python/Frontend/env/core/backend_model.py`：

- 核心状态包括 FTQ 生命周期、`_cfvec_queue`、`_pending_resolves` 和 `pending_events`。
- 周期入口是 `plan_cycle_actions()`：采样 cfVec、更新 resolve、从 `_cfvec_queue`
  计算 instruction commit 候选、选择 FTQ-entry commit、选择 redirect，并输出 callRetCommit。
- `_sample_cfvec()` 负责“DUT 观测 -> queue entry -> 路径/异常语义”；
  exception-marked entry 保留身份，但不分类为普通 CFI。
- `_ready_resolves_for_cycle()` 处理 resolve ready 条件；
  `_plan_instruction_commits_for_cycle()` 推进 instruction commit；
  `_plan_commit_entry_for_cycle()` 决定 FTQ-entry commit。
- mismatch 或显式注入形成 redirect event，`_ready_redirect_for_cycle()` 选择 ready
  event，`_plan_redirect_payload()` 执行对外驱动和 flush/recovery。
- `set_golden_trace()` 切换到 `auto_bin`，并将 resolve delay 切到 golden-trace
  固定区间（当前 `[3,5]`）。

### 当前代码阅读顺序

1. `__init__()`、`set_backend_mode()` 和 `plan_cycle_actions()`；
2. `_sample_cfvec()`；
3. `_ready_resolves_for_cycle()`；
4. `_plan_instruction_commits_for_cycle()` 和 `_plan_commit_entry_for_cycle()`；
5. `_ready_redirect_for_cycle()` 和 `_plan_redirect_payload()`；
6. `_cfvec_queue_flush_wrong_path()`、`_cfvec_queue_pop_head()` 和
   `_cfvec_queue_remove_range()`。

### 当前实现高风险点

- ready redirect 不得因为 target PC 已经被观察到而静默丢弃；它仍需经过正常
  redirect drive 和语义 flush。
- callRetCommit 回写应以 `ftq_flag/ftq_value/ftq_offset` 等稳定身份为主，不能把会随
  queue 裁剪变化的 `queue_index` 当作语义主键。
- `pending_work_count()` 必须覆盖 callRetCommit 等跨拍 pending 状态，否则 quiescent
  判断会过早结束。
- recovery 进行中即使 queue 暂空，也不能通过 fallback commit 绕过恢复门控。
- instruction commit 候选由当拍扫描 `_cfvec_queue` 得到；遇到非 correct、exception、
  非 pending、当拍新入队或 CFI 尚未 resolve 的指令即停止，不另存一份可校验的候选队列。
- redirect 的 `level` 同时决定内部 `flush_itself` 和对外驱动值：bit0 为 1 时连
  source 一起 flush。golden mismatch 使用 `level=0`，fetch-fault redirect 使用
  `level=1`，驱动 payload 不再改写这个值。
- RVC 的 `cfVec.instr` 已是 32-bit 扩展指令；`is_rvc` 只负责原始宽度、PC 步进和
  expected raw16 扩展语义。

### Instruction commit 与 FTQ commit 当前实现

- `_cfvec_queue` 是唯一持久指令流；不存在第二份持久的提交指令索引容器。
- instruction commit 候选每拍从 `_cfvec_queue` 头部连续 correct-path 前缀计算；
  `wrong`、`unknown`、exception 或未完成 resolve 的 CFI 都会阻塞后续提交。
- callRetCommit 的 pending/scheduled/visible 状态继续作为真实跨周期状态保留；redirect、
  recovery 和 queue 裁剪必须同步删除或重定位其中的 `queue_index`。
- FTQ-entry commit 直接检查 `_cfvec_queue` 头部同一 FTQ span 的 path、ROB commit、
  resolve 和 entry-boundary 状态。
- Debug snapshot输出 `correct_prefix_len` / `correct_prefix_head`，在读取时从当前
  `_cfvec_queue` correct前缀即时派生。

## [验证] 实现一致性最小检查项

下面的检查项按语义覆盖推导，条目数不预设；新增语义边界时应增补相应检查项。

### 必须项（违反即语义错误）

- `cfVec_queue` 入队严格按 DUT 观测顺序，不因等待目标 PC 而暂停或跳过包。
- 除非该指令位于 DUT 首次看到 redirect valid 的周期 `M` 或下一拍 `M+1` 的 skip 窗口，或在语义上被某次 `redirect` 作为 wrong-path flush 清除，否则任何已观测到的 `cfVec` 包都不得被丢弃、跳过采样或绕过入队。
- 首次 mismatch 只定义一个 wrong-path 起点，并沿该起点向后标记同一段 wrong-path。
- 任意时刻只允许存在一个 active wrong-path episode；在该 episode 被 `redirect` 清除之前，不得重新开第二个 wrong-path episode。
- mismatch 后继续接收 `cfVec`，不得进入“暂停构队列”等待模式。
- `redirect` 必须从 active wrong-path 起点开始清除 `cfVec_queue`，并保留更老 correct-path 前缀。
- `redirect` 的 flush 范围不得因为临时 `target_pc` 可见、waiting-target 命中、`ftqidx` 数值变化或类似局部条件而被拆成多段。
- `redirect` drive 后必须清除已知 wrong-path queue 后缀；DUT 首次看到 redirect valid 的周期 `M` 和下一拍 `M+1` 的 `cfVec` 必须被丢弃，之后进入 recovery-wait 状态，第一条采样到的 recovery `cfVec` 必须是 redirect target。
- exception-marked `cfVec` entry 必须保留异常标记，但不能按普通指令参与 CFI 分类、resolve 生成、golden replay 或 instruction commit。
- `auto_bin` 可以基于 live exception source 自动产生 fetch-fault recovery redirect；
  `auto_python` / `manual_python` 不得在 testcase 显式 recovery 之前抢先消费同一个 source。
- 某条 CFI 一旦已经与 golden 的某个动态实例匹配，其后续用于 `redirect` 的恢复目标必须绑定到该动态实例自身的 golden 语义，不得从一个可能已经漂移的全局 golden cursor 临时推导。
- 某条 `redirect` 的 `target`、`pc`、FTQ 上下文必须来自同一个动态实例；不得把 `target` 绑定到当前实例、却把 FTQ idx / offset 绑定到另一条更老或已失效的实例。
- 已被 earlier `redirect` 在语义上清除的 wrong-path 指令，即使暂时仍残留在内部结构中，也不得再参与后续 `redirect` 的归因、FTQ 上下文选择或 flush 范围计算。
- 某条已经成功发出过 `redirect` 的正确路径 CFI，仍必须先完成 right-path
  `resolve`，之后才允许被 `commit` 正常退休；`commit` 之后若仍需把后续
  mismatch 归因到这条 CFI，必须通过“最近一次已 commit 正确路径 CFI”
  之类的独立回退记录完成，不能再要求该 CFI 继续滞留在 queue 中。
- instruction commit 候选必须每周期从当前 queue 头部连续正确路径前缀现场计算，不得保留依赖旧 queue 索引的历史快照。
- `commit` 候选只允许来自当前正确路径前缀；任何 `unknown` 或 `wrong` 后缀都不得参与 commit 计划。
- instruction commit 严格按程序序推进，不跳过更老未提交指令。
- 正确路径 CFI 在 `resolve` 完成后才允许对应指令进入 committed。
- 正确路径 CFI 若预测错误，`redirect` 已经发给 frontend 后，该 CFI 所在的
  FTQ entry 可以继续按正常条件退休；不得为了等待下一笔 recovery `cfVec`
  或恢复目标 FTQ 而继续阻塞 redirect source FTQ 自己。
- `callRetCommit` 从“已提交指令”派生，且保持指令粒度。
- FTQ-entry `commit` 仅由 `cfVec_queue` 头部连续、同 FTQ、已提交指令聚合派生。
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
- `instr` 为 frontend 输出的完整 32-bit 指令；若原始指令是 RVC，则已经是扩展后的 32-bit 形式，`isRvc` 只描述原始指令宽度和 PC 步进语义
- 该指令当前位于正确路径还是错误路径
- 若该指令是 CFI，则标记它是否已经被 `resolve` 过
- 该指令是否带 frontend exception 标记以及具体 exception bits；带 exception 标记的 entry 不应再被当作普通 CFI 解析
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

对于这种场景，当前环境的语义处理应当是：

- redirect drive 时立即 flush 当前 queue 中已知的 wrong-path 后缀
- DUT 首次看到 redirect valid 的周期 `M` 和下一拍 `M+1` 的 cfVec 处于 skip 窗口，不采样入队，也不推动 golden trace
- `M+2` 起进入 recovery 状态；从该周期起出现的第一条有效 recovery cfVec 必须是该 redirect 的 target
- 如果窗口后的第一条采样 cfVec 仍不是 target，应视为 recovery failure，而不是重新开启一轮新的错误路径

进一步地，`redirect` 的 wrong-path 清除边界是语义边界，但当前实现不再把 DUT 首次看到 redirect pin 的 `M` 当拍或 skip 窗口内的残留 cfVec 先入队再裁剪。因此：

- DUT 在 `M` 首次看到 redirect valid 当拍仍然观测到的旧 wrong-path `cfVec`
- `M+1` 继续观测到的旧 wrong-path residual `cfVec`

都必须继续归属于这一次 active wrong-path episode，但在实现上通过 skip 窗口直接丢弃，不进入 `_cfvec_queue`。

换句话说：

- `redirect` 不得只清除发出前那一截 wrong-path，而把 skip 窗口内仍属于同一旧 wrong-path 的 `cfVec` 重新解释成新的 episode
- skip 窗口内的 residual `cfVec` 不应进入 queue；窗口后第一条采样结果必须证明恢复到了 target
- 只有 skip 窗口后真正恢复到 target，后续观测才可以重新参与新的 correct-path / wrong-path 判定
- 若某条正确路径 CFI 已经证明下一条 golden PC 不是其顺序后继，并且同拍后续 slot 中第一个有效 `cfVec` 不是该恢复目标，则 active wrong-path episode 必须在这个“同拍后续的第一个非目标 slot”处立即开始；不得等到下一拍或下一次局部 mismatch 再补记起点
- 只要系统仍处于“等待恢复目标重新建立对齐”的阶段，queue 中正确路径前缀之后的未知后缀也必须被并入这同一条 active wrong-path episode；不得把它们长期保留为 `unknown`，否则会错误阻塞 commit，并在后续恢复或再次重定向时形成非法中间态
- active wrong-path episode 的起点必须锚定在“当前正确路径前缀之后的第一个非正确条目”；不得因为更老正确路径仍留在 queue 中，就让 wrong-path 起点在这些更老前缀之前或之中漂移
- 主状态是一个 `ActiveWrongPathEpisode`。它承载起点、归因 CFI、恢复目标、是否已经 drive redirect，以及是否仍在等待恢复完成。任意时刻最多一条。
- “是否仍在 wrong-path / 是否仍在等待恢复 / 是否允许 commit fallback / 当前 wrong-path 起点在哪里”都从这条 episode 读取，不从局部辅助字段拼接。
- episode 建立之后，后续 mismatch 和 residual 沿用其中已经确定的归因 CFI 与恢复目标。不得在每次局部 mismatch 时重新向前搜索“前一条正确路径 CFI”。
- 归因 CFI 和恢复目标由统一 helper 推导。queue 更新、redirect 排队和 recovery 进入都消费这个结果，不在局部 mismatch 处理里各自覆盖。
- 建账检测点可以有多处，但只负责发现条件。origin、target 和 context 的组装集中在统一 helper。
- “正确路径 CFI 的下一条应为 `target_pc`，但实际看到的第一条有效 `cfVec` 不是它”这条规则共用同一个判定 helper，不在 replay、采样和 mismatch 里各写一份。
- 首次建账入口是正确路径 replay 发现 control-flow 后继不再顺序一致。不在局部 slot 检查、wait 命中或 residual 入口重复建账。
- 未知后缀是否收进当前 episode，只由“是否已有 active episode / 是否仍处于恢复阶段”决定。
- episode 用 `redirect_driven` 区分两个阶段：

  1. 已归因，但尚未 drive redirect；
  2. redirect 已发，正在等待恢复目标重新建立对齐。

- redirect 已发之后，恢复目标 PC、resolve 回退目标和“是否仍在恢复中”都从 episode 读取，不以全局 golden cursor 为主。
- `pending_level0_target_ftq` 只服务 commit 次序：redirect 之后，commit 不得越过这个目标 FTQ entry。它只能挡住更年轻的候选，不能挡住目标 entry 自身，也不参与 episode 起点、切分或恢复判定。
- 恢复阶段只有一个入口，围绕当前 episode 的 recovery target。读取“当前恢复目标是谁、现在是否仍在恢复中”走统一 helper。
- redirect drive 后，由一个集中 helper 写入恢复目标、恢复起点和阶段切换。恢复完成、reset、queue 清空或显式重置时，由同一个清空 helper 清掉这条 episode。
- commit gating、stale 清理和 pending work 统计通过这个统一 helper 判断是否处于恢复阶段。

也就是说：

- redirect 之前的 wrong-path：正常入 queue，等待本次 `redirect` 清除
- redirect 之后、skip 窗口内再次出现的旧 wrong-path：属于恢复残留，应被丢弃，不应重新开启新的 mismatch / flush 周期
- 只有当 skip 窗口后恢复 target 真正被观测到时，环境才重新从 queue 头继续与 golden trace 匹配

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
- 真正的 `rob commit` 需要由独立的顺序提交建模来决定，不能由 golden match 直接标成 committed
- `callRetCommit` 与 FTQ-entry `commit` 都应从已经 committed 的指令派生

如果实现把“与 golden trace 对齐”直接当成“已经 ROB commit”，则它虽然可能还能跑通部分 testcase，
但语义上已经不再是在“模拟 backend 提交”，而是在用 golden trace 直接驱动 backend 事件。

## 独立的 backend 顺序提交

为了尽可能模拟真实 backend，环境应在 `cfVec_queue` 之上独立计算顺序的
instruction commit。代码里没有名为 commit frontier 的状态；当前实现每周期调用
`_queue_instruction_commit_candidate_indices()` 扫描 `_cfvec_queue`，再由
`_plan_instruction_commits_for_cycle()` 把候选标成 committed。FTQ-entry 边界由
`_queue_head_ftq_commit_span()` 从同一队列头部现场计算。

顺序提交的最小要求是：

- 只允许从 queue 头开始按程序顺序推进
- 不能跳过更老且尚未提交的 correct-path 指令，去提交后面的指令
- wrong-path 指令不能进入 committed
- 正确路径上的 CFI，必须在对应 `resolve` 已经完成之后，才能进入 committed

因此，一个最小等价实现应满足：

- `golden match` 先把指令标成 correct-path
- 独立的提交调度器再把 queue 头连续前缀中的可提交指令推进到 committed
- `callRetCommit` 从这些“已经 committed 的单条指令”派生
- FTQ-entry `commit` 从这些“已经 committed 且位于 queue 头部的整 entry”派生

实现可以使用不同的数据结构，但不能绕过这条独立的顺序提交，例如：

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
- 该 FTQ entry 中不存在尚未发给 frontend 的 `redirect`
- `commit` 必须严格保序发送

上面这条“当前 FTQ entry 中不存在仍需触发的 `redirect`”，在语义上通常已经隐含在
“该 FTQ entry 全部位于正确路径上”之中；如果该 entry 内仍有会触发恢复动作的错误路径
控制流，它就不应被视为可提交。

如果该 FTQ entry 中的正确路径 CFI 已经完成 right-path `resolve`，且由该 CFI
触发的预测错误 `redirect` 已经发给 frontend，则该 redirect source FTQ 自己
不应再被 active redirect context 或 pending recovery target 阻塞。这里
`resolve` 是 commit 的前置条件，`redirect` 已发只解除额外的 recovery 等待，
不能替代 `resolve`。后续等待 recovery `cfVec` 只约束更年轻的 FTQ entry，
不能要求 source FTQ 一直滞留在 queue 中。

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
- 环境应把它建模成“instruction commit 完成后，按指令派生出来的事件”

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

## 相关文档

修改或调试当前实现时，同时参见：

- `docs/agents/frontend-debugging.md`
- `docs/agents/frontend-backend-controlflow/README.md`
