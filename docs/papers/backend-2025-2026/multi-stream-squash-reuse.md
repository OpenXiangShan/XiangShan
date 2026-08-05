# Multi-Stream Squash Reuse：复用错误路径上控制无关的计算结果

- 论文：Qingxuan Kang, Trevor E. Carlson, *Multi-Stream Squash Reuse for Control-Independent Processors*
- 会议：MICRO 2025
- 本地论文：[multi-stream-squash-reuse-micro-2025.pdf](multi-stream-squash-reuse-micro-2025.pdf)
- 公开版本：[作者 PDF](https://www.comp.nus.edu.sg/~tcarlson/pdfs/kang2025msrfcp.pdf)

## 一句话结论

分支预测错误后，传统乱序核会杀掉该 branch 之后的全部动态指令；即使其中一段计算不依赖 branch 的方向，并且正确路径稍后会重新走到同一段静态代码，也必须重新 rename、分配物理寄存器、发射并执行。Multi-Stream Squash Reuse（下文简称 MSR）把这些已被 squash、但已完成的结果暂存为多个候选 stream。当正确路径重新汇合到其中一个 stream 时，若当前指令的每个输入仍是当时的同一版本，就直接复用旧物理寄存器中的值。

它减少的是一次错误预测已经造成的重复后端工作，不是提高分支预测准确率，也不是让错误路径的 store 或其他副作用生效。

先建立一个正确的心智模型：MSR **不会保留错误路径的 ROB、RAT 或控制流**。redirect 后这些状态仍按原有规则恢复。它唯一试图留下的是少量已经算完的 PRF value，以及能够证明“当前指令正好需要这个 value”的元数据。当前正确路径中的指令仍是全新的动态指令，仍要占用当前的 ROB 位置并按当前路径顺序提交。

## 1. 为什么普通 branch recovery 会浪费这部分工作

考虑下面的控制流。`P` 是预测走错的分支，机器预测跳到 `Wrong`，但真实方向是 `Correct`。`Wrong` 这条路径到 `Join` 更快；在 branch 结果还没回来时，机器已经沿错误路径执行到了 `Join` 中的静态指令 `I0`、`I1`。随后 branch 解析，机器 redirect 到 `Correct`；正确路径最终也会到达**同一份静态代码** `Join`。

```text
                    P: conditional branch
                   /                         \
     predicted ---/                           \--- actual
                v                                   v
             Wrong: ...                         Correct: ...
                |                                   |
                |                                   |
                +---------------+   +---------------+
                                |   |
                                v   v
                              Join: I0 = add x12, x1, x2
                                    I1 = xor x13, x12, x3
                                    use x13

错误路径先到达 Join：I0/I1 已执行。branch 之后被 squash。
正确路径随后到达 Join：再次看到完全相同 PC 的 I0/I1。
```

传统 OoO 核的语义是：`P` 解析为错误后，`P` 之后所有 ROB entry 都不再属于当前动态程序。即使错误路径 `Join.I0` 已经执行完、值也正确，其目的物理寄存器也会随 recovery 失去所有权，最终可被再次分配。正确路径到达 `Join` 时，rename 会重新给 `I0` 分配一个物理寄存器，`I0`、`I1` 再次经过 IQ、功能单元和 writeback。

这里的关键不是静态指令地址相同，而是这次动态执行是否相同：

```text
静态指令相同  AND  所有输入值版本相同  AND  旧结果已完成且仍被保留
```

只有三者同时成立，错误路径上的旧结果才等价于正确路径当前指令应产生的结果。论文把这种结果称为可复用的控制无关计算结果（control-independent result）。

## 2. 为什么叫 Multi-Stream，而不是普通的 squash reuse

最直接的 squash reuse 只保留“最近一次”被错误预测杀掉的路径。它只能处理正确路径很快回到这一个路径的情况。

真实执行里会连续发生 redirect，或者存在嵌套分支。后续的正确 stream 可能不是和最近那条错误 stream 汇合，而是和更早保留下来的 stream 汇合：

```text
stream 0:  branch 0 错误后保留下来
             \__________________ Join 0

stream 1:          branch 1 错误后保留下来
                      \____________ Join 1

current:    redirect 后的当前取指流 -----> Join 0
```

如果只记住 stream 1，`Join 0` 的复用机会会丢失。MSR 因而同时维护多个已 squash stream，并在当前 fetch 流与这些 stream 比较时选择候选。论文的选择策略优先使用最近更新的 stream；若同一个 branch 下有多个可汇合位置，则取离该 branch 最近的汇合点。这个限制避免一条当前路径同时以互相矛盾的方式复用多段旧路径。

多 stream 不等于同时“恢复”多条错误路径。处理器始终只有一条当前的架构正确路径；其余 stream 只是保存已完成值和依赖版本的只读候选记录。

### 2.1 一个 stream 在硬件中究竟是什么

这里的 stream 不是 SMT hardware thread，也不是另一份独立的 ROB。它是一段被标上顺序位置的历史记录，至少有两种粒度：

```text
WPB stream S0，fetch-block 粒度：
  block 0: PC range / fetch block tag
  block 1: PC range / fetch block tag
  block 2: PC range / fetch block tag

SL stream S0，uop 粒度：
  offset 0: old I0 的依赖版本和结果 PReg
  offset 1: old I1 的依赖版本和结果 PReg
  offset 2: old I2 的依赖版本和结果 PReg
```

WPB 负责回答“当前 fetch 的第几个 block 对应旧 stream 的第几个 block”；SL 负责回答“该 block 中当前 decode 的第几个可复用 uop 对应旧 stream 的哪个结果”。因此需要一个确定的 block-to-uop 位置换算。对香山而言，RVC 长度、指令展开、fusion，以及向量指令拆成多个 uop 都会使这个换算复杂；第一版限制到 scalar、非 fusion、一个指令一个 uop，可以把这个映射简化为静态 PC 顺序加 uop offset。

### 2.2 多 stream 的查找和选择

当当前 fetch block 到达时，reconvergence detector 并行或分级检查它是否出现在任一有效 WPB stream 中。命中后不是立刻认为整个 stream 都相同，而是启动一个有限窗口的连续比对：

```text
当前 block 序列：C0, C1, C2, C3
旧 S0 block 序列：A0, A1, C0, C1, C2, D0

首先在 C0 命中 S0[2]，因此 candidate offset 从 S0[2] 对齐。
C1、C2 继续匹配：继续为对应 uop 提供 reuse candidate。
C3 != D0：静态 stream 再次分岔，停止给后续 uop 提供 candidate。
```

如果当前 block 同时命中多个历史 stream，硬件不能把所有 stream 的 PReg 混合起来用。它必须选定一个候选 stream，并维持该 stream 的 position cursor；论文采用以最近更新 stream 和距离分支最近的汇合点为主的选择规则。即便前端选错了“看起来相同”的历史 stream，rename 中的 RGID 判断仍会拒绝不等价的动态实例；前端选择主要影响机会率和比较开销，而不是直接决定正确性。

## 3. 整体结构：前端找汇合，rename 判等并复用

论文把职责拆成两半：

| 结构 | 所在位置 | 记录什么 | 要解决的问题 |
| --- | --- | --- | --- |
| Wrong-Path Buffer（WPB） | fetch 侧 | 每条已 squash fetch stream 的 PC/block 范围、有效位和读写指针 | 当前重新取到的静态 stream 是否与旧 stream 汇合，以及从旧 stream 的哪个位置开始 |
| Squash Log（SL） | rename/后端边界 | 对应旧 stream 的逐 uop 记录：有效/已完成状态、各源 RGID、目的 RGID、目的物理寄存器 | 同一位置的动态指令是否仍使用同一组输入版本；若是，从哪个 PReg 读结果 |
| RGID state | rename、RAT、RAT checkpoint、ROB | 每个架构寄存器映射的版本号 | 将“输入是否仍相同”的判断与偶然变化的 PReg 编号解耦 |

数据流如下：

```text
branch redirect
    |
    +--> 被杀掉的 fetch block 写入 WPB
    +--> 被杀掉、且已执行的 uop 元数据写入 SL；其 PReg 暂不归还 freelist

正确路径重新 fetch
    |
    +--> WPB 比较 PC stream，发现 reconvergence，给 rename 一个 old-stream offset

rename 当前 uop
    |
    +--> 取当前 src RGID，与 SL[offset] 中旧 src RGID 逐一比较
           |
           +--> 全部相等且旧 uop 已完成：绑定旧 pdest，跳过分配和执行
           |
           +--> 任一不等：走原有 rename -> 分配 pdest -> dispatch -> issue -> writeback
```

WPB 的匹配只说明“是同一段静态代码”；真正保证数值正确的是 rename 侧的 RGID 比较。将两者分开很重要：只按 PC 复用会在 loop 多次迭代、或汇合点之前某个寄存器被改写时读到过期值。

## 4. RGID：为何不能只比较物理寄存器号

### 4.1 定义

RGID 是 Rename Mapping Generation ID，可以把它看成“某架构寄存器当前映射版本”的编号。每个架构寄存器有一个递增的 generation counter；每次 rename 给该架构寄存器建立新的定义时，生成一个新的 RGID。RAT 除了保存：

```text
architectural register -> physical register
```

还要保存：

```text
architectural register -> current RGID
```

例如：

```text
初始：x10 -> p8,  RGID 41
旧错误路径记录：add x12, x10, x11，src RGID = [41, 19]，dest = p37

当前正确路径：x10 -> p8,  RGID 41；x11 -> p24, RGID 19
=> 输入版本完全相同，p37 可复用。

若中间执行过：addi x10, x10, 1
当前：x10 -> p52, RGID 42
=> 即使静态 add 相同，RGID [42, 19] != [41, 19]，必须重算。
```

### 4.2 为什么 PReg equality 不够

直接比较当前 `psrc` 和旧 `psrc` 看似可行，但 redirect 后 RAT、freelist 和物理寄存器回收会改变 PReg 命名。相同的逻辑版本可能被恢复到另一组 PReg，反过来，被回收再分配的 PReg 号也可能碰巧相同。PReg 编号表达的是资源分配历史，不能稳定地表达架构值版本。

RGID 的作用是把“这个 source 是哪一次对 xN 的定义”变成显式元数据。只要当前 RAT 和 checkpoint 同时回滚 PReg 映射及 RGID 映射，错误路径留下的记录可以跨之后的 rename/redirect 被安全检查。

### 4.3 它不是纯粹的 value prediction

RGID 相等并不是在猜测数值相等。它证明当前指令看到的是同一组动态定义；再结合 WPB 已将候选定位到相同的静态指令，处理器可以确定该计算应产生同一个值。旧的 PReg 已经有真实执行写回的值，MSR 只是改用该真实结果。

当然，有限位数 RGID 会回绕。实现必须保证一个旧 Squash Log entry 存活期间，不会让同一逻辑寄存器发生足以回绕的重命名次数；否则相等比较会产生假阳性。论文配置使用 6-bit RGID，实际落地应依据“最多保留 stream 数 x 每 stream 最大存活周期 x rename 速率”做上界证明，或者引入 epoch/失效机制。

## 5. 从一条具体指令看到底发生了什么

下面只考虑单目的、无异常、无副作用的两个整数 ALU uop。这正是最适合第一版 RTL 验证的范围。PReg 和 RGID 数字只是例子，不代表香山实际编码。

### 5.1 branch 解析前：错误路径已经把公共代码算完

在 branch `P` 之前，rename 的当前映射为：

| 架构寄存器 | PReg | RGID | 含义 |
| --- | --- | --- | --- |
| `x1` | `p5` | `g17` | `x1` 的第 17 个版本 |
| `x2` | `p9` | `g31` | `x2` 的第 31 个版本 |
| `x3` | `p11` | `g8` | `x3` 的第 8 个版本 |
| `x12` | `p15` | `g50` | redirect checkpoint 中原先的 `x12` |
| `x13` | `p18` | `g51` | redirect checkpoint 中原先的 `x13` |

错误预测的 stream 先走到 `Join`。它的动态 rename 和实际执行如下：

| 错误路径 uop | 静态 PC | source PReg / RGID | 分配的 `pdest` / dest RGID | 执行状态 |
| --- | --- | --- | --- | --- |
| `I0: add x12, x1, x2` | `0x800` | `p5/g17`, `p9/g31` | `p41/g53` | 已 writeback，`p41` 中有真实结果 |
| `I1: xor x13, x12, x3` | `0x804` | `p41/g53`, `p11/g8` | `p42/g54` | 已 writeback，`p42` 中有真实结果 |

此时错误路径的 speculative RAT 暂时会是 `x12 -> p41/g53`、`x13 -> p42/g54`。但 branch `P` 仍未解析，所以这只是一份可被推翻的 speculative 映射；对应 ROB entry 也都比 `P` 年轻。

### 5.2 branch 解析：旧 ROB 被杀掉，旧 PReg 不能直接 free

branch `P` 最终发现预测错误。普通实现会恢复 `P` 的 rename checkpoint：

```text
恢复后的当前 RAT：x12 -> p15/g50，x13 -> p18/g51
错误路径 ROB entry：I0、I1 都被 squash，永远不会 commit
普通 freelist 语义：p41、p42 最终重新变成可分配 PReg
```

MSR 前两条与普通 recovery 完全相同；不同的只有最后一条。它在 old ROB entry 被销毁前抽取下面的 Squash Log 记录：

| SL offset | 对应旧 PC | old src RGID | old dest RGID | old `pdest` | completed | PReg ownership |
| --- | --- | --- | --- | --- | --- | --- |
| 0 | `0x800` | `[g17, g31]` | `g53` | `p41` | 1 | `SQUASH_HOLD` |
| 1 | `0x804` | `[g53, g8]` | `g54` | `p42` | 1 | `SQUASH_HOLD` |

请注意三个容易混淆的点：

1. `I0`、`I1` 的**旧 ROB entry 确实已经不存在**，不能提交、不能产生异常、不能再发 redirect。
2. `p41`、`p42` 中的**数据位并没有消失**。MSR 只是在 redirect 后禁止 freelist 重新分配这两个 PReg，因此其中的值不会被覆盖。
3. checkpoint 恢复后的 RAT 并不指向 `p41/p42`。它们处于 `SQUASH_HOLD`，对当前正确路径不可见，直到某条当前 uop 通过 RGID 检查后显式 claim 它们。

所以 MSR 不是“错误路径继续活着”，而是“错误路径死掉后留下两个带标签的、暂时不可分配的 PRF 槽位”。

### 5.3 当前路径到达 Join：前端只能给候选，rename 才能决定

正确路径稍后 fetch 到 `0x800`。WPB 发现当前 fetch block 序列与保留 stream 的 block 序列重合，因而给随后的 decode/rename uop 附上：

```text
reuse_candidate_valid = 1
candidate_stream_id   = S0
candidate_offset      = 0        // 对应 SL[0]，即旧 I0
```

这个信号的语义仅是“当前 PC 位置可以尝试读 `SL[S0][0]`”，不是“必定复用”。当前 `I0` 到 rename 时，使用当前 RAT 读到：

```text
current I0 source RGID = [g17, g31]
logged  I0 source RGID = [g17, g31]
old PReg               = p41, completed = 1, owner = SQUASH_HOLD
```

全部条件为真，rename 进行一次 **claim**：

```text
当前 I0 的 pdest      := p41       // 不向 int freelist 请求新 PReg
当前 I0 的 dest RGID  := g53       // 不新建版本，而是采用等价旧定义的版本号
当前 RAT 的 x12       := p41/g53
当前 I0 的 oldPdest   := p15       // 当前路径提交时仍应释放 checkpoint 中旧 x12 映射
p41 ownership         : SQUASH_HOLD -> CURRENT_SPEC
当前 I0 ROB entry     : 新建，且完成状态必须为 ready
```

这里 `I0` 的“新”是指当前路径的新动态实例；它的值来源是旧实例。当前 ROB entry 的 `oldPdest = p15` 必须按当前提交语义记录。否则 `I0` 提交时不会释放当前路径覆盖掉的旧 `x12` 映射，PRF 会泄漏。

### 5.4 紧随其后的依赖指令：必须走同拍 rename bypass

若 `I0`、`I1` 在同一个 rename group 中，`I1` 不能从本拍开始时的 RAT 得到 `x12 -> p15/g50`，而应看到刚刚对 `I0` 做出的当前路径映射：

```text
I1 current source RGID = [g53, g8]   // x12 来自同拍 I0 的 rename bypass
I1 logged  source RGID = [g53, g8]
```

因此 `I1` 也能 claim `p42/g54`，当前 RAT 随后更新为 `x13 -> p42/g54`。由于 `p41/p42` 在旧路径已经 writeback，它们在 BusyTable 看来应是 ready；`I0/I1` 的当前消费者不应等待一个永远不会再发生的 EXU writeback。

这也是实现中的一个硬性要求：普通 rename 往往会把每个新 `pdest` 标为 busy，等待未来 writeback 清除该位。复用 uop 没有未来 writeback，故必须在同拍绕过这次 busy set，或等价地将它标成 ready。只改 `uops(i).pdest` 而不改 ready/busy 协议，后继 uop 会永久等待。

### 5.5 一个 source 改了时，为什么必须整条退回普通执行

把正确路径改为：它在到达 `Join` 前又执行了一条对 `x1` 的定义。此时：

```text
current I0 source RGID = [g18, g31]  // x1 已经从 g17 变成 g18
logged  I0 source RGID = [g17, g31]
```

`I0` 立即失败，正常分配例如 `p60/g55` 并送入 IQ 执行。`I1` 从当前的 rename bypass 得到 `x12 -> p60/g55`，而旧记录需要 `g53`，所以 `I1` 也失败并正常执行。这个级联失败正是期望行为：旧 `p41/p42` 对应的是旧 `x1` 的值，不能混入当前路径。

之后若没有别的候选继续需要 `S0`，其 `SQUASH_HOLD` PReg 可以在 timeout、stream 淘汰或明确失配后返还 freelist。它们绝不能因本次 `I0` 失败就马上无条件释放，除非已经证明同一 stream 后面的所有潜在复用点也不可能再被当前路径到达。

## 6. 一次 redirect 到复用的完整时序

以下从一条普通控制预测错误展开，忽略异常和 memory violation 等非控制 redirect。

### 阶段 A：branch 解析与原有 recovery

1. branch 的执行单元产生 redirect，ROB 选择最老 redirect，前端转向正确 target。
2. 原有逻辑恢复 rename RAT、free list head、snapshot 等状态，并开始对年轻 ROB entry 执行 squash/walk。
3. 当前错误路径不再有提交资格；MSR 不改变这一条精确异常和顺序提交原则。

同拍 writeback 是一个必须先规定优先级的边界条件。若一个年轻 uop 在 redirect 到达的同拍也宣称 writeback，第一版应保守地不把它放入 SL：现有 redirect kill/flush 逻辑可能已经禁止该 writeback 写入 ROB 或 PRF。只有在明确证明 PRF 数据已写入、没有被更老 redirect kill，并且 capture 看到的完成位与 PRF ready 状态一致时，才能把它作为可复用完成结果。

### 阶段 B：把错误路径变成候选 stream

4. fetch 侧把这次被 kill 的 fetch block 序列写入 WPB，记录 stream 的 PC 边界。
5. 对被 squash 的每个 uop，SL 从 ROB/rename 元数据取得其 source RGID、destination RGID、目的 PReg 和执行完成状态。
6. 未执行完成的 uop 没有可用结果，不能复用；其目的 PReg 可按原策略归还。已完成且满足可保留条件的 PReg 则从“立即回收”改为“由该 Squash Log entry 持有”。

这一步不保存结果副本：值仍在 PRF 中。WPB/SL 保存的是“这个 PReg 对哪一条旧动态指令有效，以及它需要的输入版本是什么”。

### 阶段 C：当前 fetch 流寻找 reconvergence

7. 前端从正确 target 重新取指。Reconvergence detector 将当前 fetch block 的 PC 序列与各 WPB stream 比较。
8. 命中某条旧 stream 后，向 rename 提供该 stream 和对应的 instruction offset；随后继续比对后续 block。若后续静态路径再次分岔，则停止复用测试并回到普通流。

前端匹配可流水化，因为它不改变当拍取指的正确性；它只是在发现后给 rename 标记“后面若干条 uop 可尝试复用”。论文将此检测拆为多级，而非把大规模 WPB 比较放入前端关键路径。

### 阶段 D：rename 逐条作动态等价性判断

9. 当前 uop 到 rename，使用 offset 读相应 SL entry。
10. 检查：旧 entry 有效、旧 uop 已完成、目的 PReg 仍被 reserve，并对每一个真实寄存器 source 比较 `current_src_rgid == logged_src_rgid`。
11. 全部命中时，当前 uop 的 destination 映射为旧 PReg，RAT 的 destination RGID 也设置为 logged destination RGID；不用从 freelist 申请新的 PReg，也不进入普通 dispatch/issue/writeback 链。
12. 任一条件失败时，当前 uop 与普通 uop 完全一样：分配新 PReg，生成新的 RGID，正常 dispatch。此时后续 uop 看到的 RGID 也自然变化，更多旧 entry 将因版本不匹配而失效。

### 阶段 E：当前路径仍需顺序提交

复用只消除了执行，不会把旧路径 ROB entry “复活”。当前正确路径的动态指令仍需要自己的 ROB 记录，才能与其他当前路径指令按程序顺序 commit、产生精确 trap，并维护 commit-time 的架构状态。对香山而言，最自然的实现是为当前 uop 正常分配 ROB entry，但在入 ROB 时标成结果已就绪，且让其 `pdest` 指向保留的旧 PReg。

对标量单 uop 指令，这可表示为一个显式的 `completedByReuse`，它在入 ROB 时提供完成事件。不要伪造一个普通 EXU writeback，因为该 writeback 会同时影响 wakeup、异常和 redirect 仲裁。对于一条 ROB 指令包含多个 uop 的情况，不能因为其中一个结果复用就把整个 ROB entry 标为完成；ROB 的完成计数必须把“复用完成的 uop 数”和“仍等待正常 writeback 的 uop 数”合并。因而第一版只覆盖“一条指令恰好一个可复用 scalar uop”最安全。

### 阶段 F：复用候选接口应该长什么样

论文并不要求香山使用某个固定 Bundle 名称；但在 RTL 边界上至少要传递如下信息，才能避免把 front-end PC 匹配和 rename 依赖判断混在一起：

| 从哪里到哪里 | 最小信息 | 说明 |
| --- | --- | --- |
| WPB/reconvergence detector -> rename | `candidateValid`、`streamId`、`uopOffset` | 表示当前 uop 有一个位置对齐的旧 entry 可读，不表示必定复用 |
| SL read -> rename | `oldValid`、`oldCompleted`、`oldSrcRGID[]`、`oldDestRGID`、`oldPdest`、`oldClass` | 旧候选的身份、依赖和物理结果位置 |
| 当前 rename 内部 | `currentSrcRGID[]`、同拍 producer 的 RGID bypass | 必须与当前 `psrc` 选择使用相同的 bypass 优先级 |
| rename -> ROB/dispatch | `isReuse`、claim 后的 `pdest`、`oldPdest`、完成状态 | 当前路径的新 entry 如何跳过 EXU 但仍能 commit |
| rename/SL -> freelist | `reserve`、`claim`、`release` | 维护 PReg 只有一个 owner，防止 free、hold、current 三种状态重叠 |

`candidateValid` 可以因重复 PC、loop 或前端只匹配到一小段 block 而出现假候选；这本身不影响正确性，因为最终必须经过 RGID、完成状态、指令类别和 PReg ownership 的判断。前端的责任是尽可能快地提供位置对齐，rename 的责任才是决定是否真的复用。

## 7. 宽 rename 中的特殊问题：同拍 producer-consumer

宽 rename 不是逐条独立查 RAT。假设同一个 rename group 中有：

```text
uop 0: add  x5, x1, x2
uop 1: add  x6, x5, x3
```

对 `uop 1`，`x5` 应读到同拍 `uop 0` 新生成的 mapping，而不是拍开始时 RAT 中旧的 `x5`。原有 rename 已有 register compare/bypass 网络处理此事。

MSR 不能绕过这条规则：

1. 先用原有同拍 destination-to-source compare 找到 `uop 1` 的 `x5` 来自 `uop 0`。
2. 若 `uop 0` 自身被复用，那么 `uop 1` 的 source RGID 应使用 `uop 0` 的 logged destination RGID，而非旧 RAT RGID。
3. 若 `uop 0` 没有复用、走普通 rename，则 `uop 1` 应使用新产生的 RGID；这样 `uop 1` 对旧 SL 的匹配通常失败，避免把依赖于新计算的值误判为旧值。

论文的思路是复用现有的同拍寄存器比较链，再附加 RGID 比较，而不是为每条 uop 增加一套独立的串行依赖搜索。RTL 上最需要关注的是：`RGID` 必须跟 PReg 一样走所有 intra-group bypass、snapshot、redirect recovery 路径，否则只有同拍相关的代码会出错。

## 8. 严格的可复用条件与必须排除的情况

可以把候选判断写成下面的逻辑条件：

```text
reuse(i) = stream_match(i)
        && old_entry_valid(i)
        && old_entry_completed(i)
        && old_pdest_reserved(i)
        && same_static_instruction(i)
        && forall src j: current_rgid(i, j) == old_src_rgid(i, j)
        && instruction_class_is_reusable(i)
```

其中前五项决定“这是不是一个有真实值的相同候选”，RGID 全匹配决定“当前计算的输入是否仍等价”。最后一项是实现边界，绝不能省略。

`forall src` 必须理解为“该 uop 的全部会影响结果的架构输入”，而不是机械地只比较两个整数 `psrc`。对第一版整数 `add`，它就是 `rs1`/`rs2` 的 RGID；`x0` 是固定零，不需要 version。立即数和当前 PC 因为已经要求同一条静态指令而天然相同。浮点指令还可能依赖 rounding mode 和浮点异常语义；向量指令还依赖 `vl`、`vtype`、mask、tail/mask policy 等状态；这些附加输入若未被记录和比较，就不能加入 reusable instruction class。

| 类型 | 第一版是否应复用 | 原因 |
| --- | --- | --- |
| 纯整数 ALU、逻辑、移位 | 可以作为最小试点 | 无副作用；有明确 register result |
| 标量乘除、浮点 FMA 等 | 之后再逐类放开 | 值仍可复用，但需覆盖长延迟、异常标志和多目的/特殊 writeback |
| Load | 初版应排除 | 即使地址输入等价，也要先确认 cache miss/replay、memory violation、异常和 load value 的生命周期处理 |
| Store | 必须排除 | 错误路径 store 绝不能获得任何架构可见副作用；没有“结果 PReg”可直接代替当前 store 的地址/数据/提交语义 |
| CSR、fence、原子指令、I/O | 必须排除 | 存在架构状态变化、顺序约束或不可重复的外部影响 |
| 可能异常的指令 | 初版应排除 | 复用不仅要复用数值，还必须精确复制异常条件、异常优先级和 ROB 提交时机 |
| 向量拆分 uop | 后续单独设计 | 一个架构指令可能对应多个 uop、多个结果或 mask/tail 语义；不能套标量单目的规则 |

论文的核心是通用的“输入版本相同则复用结果”框架；但将它移植到香山时，最安全的最小集合应先限于无副作用、单目的、无异常的整数 register-result uop。上表是香山实现建议，不应误读为论文已经逐项给出了所有 XiangShan 指令类别的实现细节。

## 9. 物理寄存器生命周期为何是最大侵入点

普通 rename/free-list 的大致语义是：

```text
allocate pdest -> uop writeback -> commit 后释放旧 pdest
                    |
                若被 redirect squash，年轻路径的 pdest 随 recovery 被撤销并可再次分配
```

MSR 要把最后一项改成：

```text
redirect squash 后：
  未完成的 pdest                 -> 正常撤销/回收
  已完成且记录到 Squash Log 的 pdest -> 进入 reserved-for-reuse，暂时不可分配
  命中复用后                     -> 作为当前路径 pdest 继续存活
 stream 超时、失配或被淘汰后     -> 从 reserve 归还 freelist
```

因此它绝非仅给 ROB 增加 done-bit 的优化。它改变了 PReg 的所有权模型：同一个 PReg 在一段时间内既不属于当前 RAT 的普通 speculative allocation，也不能被 freelist 再分配，但又需要被后续正确路径重新接管。

### 9.1 PReg 的所有权状态机

对第一版单 stream 实现，可以将每个可复用 PReg 的归属理解为下面的有限状态机：

```text
FREE
  | rename 普通分配
  v
CURRENT_SPEC ------------------------> CURRENT_ARCH
  | redirect 将其 squash，且结果已完成      | 后续新定义提交时释放
  |                                             v
  |                                       FREE
  v
SQUASH_HOLD
  |  当前路径 RGID 匹配并 claim        | timeout / stream 淘汰 / 证明不再需要
  v                                    v
CURRENT_SPEC                         FREE
```

状态机的关键不变量是：每个 PReg 在任何时刻只能有一个写入所有者。`SQUASH_HOLD` 的 PReg 已经 ready、不能被新的 EXU 写入，也不能被 freelist 分配；`CURRENT_SPEC` 的 PReg 则由当前 ROB entry 管理，后续 redirect 时要按当前路径的普通 speculative destination 处理。

`SQUASH_HOLD -> CURRENT_SPEC` 不是复制数值，而是所有权转移。SL 在 claim 后必须丢掉对该 PReg 的释放权，避免 later timeout 再把已经作为当前 RAT 映射的 PReg 放回 freelist。

### 9.2 为什么香山现有的 freelist pointer recovery 不能直接复用

香山的基础 freelist 在 redirect 后从 snapshot/arch head pointer 恢复分配位置。错误路径最初分配 `p41` 时，head 已前移；redirect 回退到 branch snapshot 后，`p41` 会重新落回“理论上可被分配”的那段环形 free list。若只增加一份 SL、却不改变 allocator，下一拍 rename 就可能再次取到 `p41`，把保存的结果覆盖。

这要求实现有一个独立于 head pointer 的 ownership 机制。概念上有两类做法：

1. freelist 在选取 candidate 时跳过所有 `SQUASH_HOLD` PReg，并保证环形队列中的洞不会造成重复或永久饥饿；
2. redirect 后把要保留的 PReg 从可分配集合中真实移走，另存于 hold pool，失效时再以唯一一次的方式归还。

两类做法都需要处理“claim 后又发生下一次 redirect”。被 claim 的 `p41` 此时已经是当前 ROB entry 的 `pdest`，下一次 redirect 必须把它按当前动态指令回收，而不能仍把它当作旧 SL 的 hold register。仅靠原有 free-list head pointer 无法表达这类非 FIFO 的借用/归还关系；需要额外的 owner bit、引用计数或明确的 hold/claim/release 协议。

### 9.3 BusyTable 的 ready 状态同样必须转移

香山 BusyTable 的普通接口有两类事件：rename allocation 会把 `pdest` 标 busy，writeback/wakeup 会把它清为 ready。复用 `p41` 时，`p41` 已在错误路径 writeback，因此没有新的 EXU writeback 会到来。

故 reused uop 不能像普通 uop 一样走 `allocPregs` 的 busy-set 路径；否则 BusyTable 会认为 `p41` 未就绪，而 current-path consumer 会一直等待。可行的协议是：reuse claim 不发普通 allocate-busy 请求，并明确保持/重建 `p41` 的 ready 状态。还必须检查同拍 read bypass：若 BusyTable 将“本拍 allocation”一律视为未 ready，则不能把 reuse claim 伪装为 ordinary allocation 再期望同拍 writeback 抵消它。

这会带来两个相反的性能效应：

- 正向：命中复用时少一次 PReg allocation、少进入 IQ/EXU，也缩短依赖链。
- 负向：保留多个错误 stream 会减少可用 PReg，可能使 rename 因 freelist 压力停顿，抵消甚至超过复用收益。

论文选择有限深度的 WPB/SL 和超时回收，正是为了限制这个压力。香山若同时研究 ATR 一类提前释放机制，二者会在 PReg 归属和回收优先级上强耦合，不能分别“各改一点”后直接叠加。

## 10. MSR 与几种容易混淆的技术的区别

| 技术 | 是否保留错误路径结果 | 判定依据 | 主要目标 |
| --- | --- | --- | --- |
| 分支预测器 | 否 | 预测 branch target/direction | 减少 redirect 次数 |
| Value prediction | 不一定 | 预测数值，之后验证 | 隐藏生产者延迟 |
| 通用 value cache | 通常不保留 squash 上下文 | 指令/输入 tag 或值的 associative lookup | 减少重复计算 |
| 单 stream squash reuse | 是 | 最近错误路径的 stream 对齐和依赖检查 | 回收最近一次错误路径工作 |
| MSR | 是，多个 stream | WPB 的 stream 汇合 + SL 中 RGID 的严格匹配 | 捕获跨多个 redirect 的重汇合机会 |

MSR 的优点是：旧结果不是放在一个全局 associative value cache 里猜测命中，而是有明确的“旧 stream 位置”与“输入 version”上下文。代价则是 redirect、PRF、rename snapshot 都需要了解这些额外的生命周期。

## 11. 论文结果与其含义

论文报告在其模拟配置上，SPECint2006 平均 IPC 提升 2.2%，其中 `astar` 最大为 8.9%；对 SPECint2017 平均提升 0.8%，对 GAP 平均提升 2.4%。这些数字说明机会存在，但也说明它不是每个 workload 都有高收益的通用后端加速器。

受益通常需要同时满足：

1. 有足够多的控制错误或会被 squash 的执行；
2. 错误和正确路径能在较短时间内重新进入同一段静态计算；
3. 汇合处的输入没有被路径上的定义改变；
4. 已执行结果在 PReg 压力、超时或 stream 替换前仍被保留。

高度可预测的程序，或者分支后两个路径不重汇合、汇合后输入版本已变化的程序，几乎没有可用机会。故不能将该论文结果直接外推为香山跑 SPEC 的预期增益。

## 12. 映射到香山：应改哪里

现有香山已具备该方向所需的基本恢复骨架：`Rename.scala` 中连接 `redirect`、各类 freelist 以及 rename table；`Snapshot.scala`/snapshot port 维护 rename 相关 checkpoint；`Rob.scala` 接收 redirect 并驱动 ROB walk/recovery。MSR 需要在这些已有边界旁边增加状态，而不是改变 ROB 的指令粒度。

| 香山位置 | 现有职责 | MSR 需要增加的职责 |
| --- | --- | --- |
| 前端 redirect/fetch | 从正确 PC 重取，flush 错误 fetch stream | 将被 squash 的 fetch block 记录入 WPB；重新取指时检测与 WPB 的 stream 汇合，并向 rename 传递 candidate stream/offset |
| `Rename.scala` | source/destination PReg rename、同拍 bypass、分配请求 | 维护/旁路 RGID；进行 SL 比较；命中时抑制普通 PReg allocation 与 dispatch，并把旧 PReg 作为当前 `pdest` |
| `RenameTable.scala` 与 snapshot | RAT 映射及 recovery | 给每个 RAT mapping 增加 RGID，并把 RGID 纳入所有 checkpoint、restore 和同拍更新 |
| `Rob.scala` | 保留 uop 的完成、异常、redirect、提交信息 | 在 squash 时向 SL 提供已完成状态和 uop 元数据；为当前复用 uop 建立正常的完成/提交记录 |
| `BaseFreeList.scala`、`MEFreeList.scala`、`StdFreeList.scala` | 分配、redirect 回滚、提交释放 | 增加 reserved PReg 的 ownership，防止 redirect 后被复用候选的 PReg 回到 allocation pool；在 SL 失效时归还 |
| BusyTable/issue | 跟踪 PReg ready 并调度 uop | 复用的旧 PReg 必须对当前消费者表现为 ready；当前复用 uop 自身不得再占用 issue/EXU 带宽 |

这里最容易犯的错误是只在 rename 将 `pdest` 改为旧 PReg，而未让 freelist 和 recovery 知道该 PReg 被 SL 持有。这样一次新的 allocation 就可能覆盖旧值，随后复用会读到另一个动态指令的结果。

## 13. 香山的推荐实现顺序

这不是近期 ROB 时序优化，而是包含 fetch、rename、ROB、PRF 的控制恢复研究项目。建议按下面顺序推进。

1. **只统计，不复用。** 在 redirect 时记录候选 uop 的 PC、指令类别、`psrc`/逻辑 source 和完成时间；当前路径汇合时离线统计 RGID 等价机会、距离和 PReg 占用峰值。先确认 SPEC 上确有足够机会。
2. **单 stream、整数纯 ALU。** 不做 multi-stream，不碰 load/store/CSR/向量；保留极小 SL，并用独立 reserve bitmap 防止 PReg 回收。这一阶段验证 redirect、RAT checkpoint 和 commit 语义。
3. **加入宽 rename 同拍依赖。** 给 RGID 完整接入 intra-group bypass，并构造同拍 producer-consumer、redirect 紧邻 rename、back-to-back redirect 的定向测试。
4. **扩为多 stream。** 加 WPB、reconvergence detector、stream 替换和 timeout。测量 fetch 侧比较的频率/时序和 PReg 压力。
5. **逐类扩大可复用指令。** 每放开一种指令类别，都要同时定义异常、replay、多个 writeback 和提交语义；不要把“数值已有”误当成“指令语义已经完成”。

第一阶段建议至少增加以下计数器：每次 redirect 保留的 completed uop 数、static reconvergence 数、RGID 全匹配数、实际复用数、各类失配原因、reserved PReg 占用周期和由 reserve 导致的 freelist stall。只有这些量都清楚，才知道瓶颈在机会率、前端检测、rename 判定还是 PReg 压力。

## 14. 对时序与面积的判断

这项工作不适合当作“压 ROB 到 240 ps 以下”的直接手段。它通常不会缩短现有 ROB writeback、IQ wakeup/select 或 execution critical path，反而可能引入：

- rename 的额外 RGID 读出、比较和同拍 bypass；
- RAT/checkpoint/ROB 每个 source/destination 的额外元数据位；
- WPB 的多 stream PC 比较和 SL 读端口；
- freelist reserve 的状态和回收仲裁。

论文将 reconvergence detection 流水化，避免直接置于 fetch 关键路径；其分析还指出，宽 rename 下同一逻辑寄存器在一个 group 内被多次定义时，RGID generation/increment 可能成为实现压力点。香山是否存在同样的时序热点必须以本地综合报告为准，不能用论文的工艺和宽度直接推断。

## 15. 论文给出的硬件开销

论文有给出开销评估，但需要分成三部分看：元数据容量、两个关键逻辑的综合结果，以及它没有覆盖到的整核代价。不能把其中任意一个数字直接当作“香山实现该方案只需要这么多面积”。

### 15.1 固定 RGID 元数据：论文配置为 2.30 KiB

论文采用 256-entry ROB、64 个架构寄存器、32 个 RAT checkpoint、每个 RGID 6 bit 的配置。RGID 额外存储为：

```text
ROB:             4 x 6 x 256  =  6,144 bit
当前 RAT:            6 x 64   =    384 bit
32 个 RAT checkpoint: 6 x 64 x 32 = 12,288 bit
-----------------------------------------------
固定 RGID 元数据总计              18,816 bit = 2.30 KiB
```

`4` 表示每个 ROB entry 记录三个 source RGID 和一个 destination RGID。这个 2.30 KiB 不包含 WPB、Squash Log、PReg hold/owner 状态及其控制逻辑；它只是“为了比较 rename mapping version，现有状态需要多带多少 bit”。

### 15.2 随 stream 深度增长的 WPB 和 Squash Log 存储

论文的每个 Squash Log uop entry 需要保存有效位、三个 source RGID、一个 destination RGID 和 destination PReg。按其配置，约为：

```text
SL entry = 1 valid + 3 x 6 src RGID + 1 x 6 dest RGID + 8-bit PReg
         = 33 bit / squashed uop
```

WPB 以 fetch block 记录旧 stream 的位置；论文的估算式中每个 WPB entry 是 23 bit，每个 stream 另有 36-bit 虚页号等 stream 元数据。若有 `N` 条 stream、每条有 `M` 个 WPB entry、`P` 个 SL entry，则主要存储可近似写成：

```text
N x (23 x M + 33 x P + 36) bit
```

再加少量 stream pointer、valid bit 和索引位。论文在参数扫描中使用每条 stream 64 或 128 个 Squash Log entry，并比较 1、2、4 条 stream；WPB entry 数取对应 SL stream 深度的四分之一，假设平均每个 basic block 有四条指令。

例如用“2 stream x 64 SL entry、每 stream 16 个 WPB entry”估算，只有 WPB+SL payload 约为：

```text
2 x (23 x 16 + 33 x 64 + 36) = 5,032 bit = 629 B
```

加上前节的 2.30 KiB 固定 RGID 元数据，仍未到 3 KiB；但这只是 metadata bits。真正 RTL 还需要寄存器阵列读写端口、比较器、状态机、PReg reserve/owner bits、连线和时序优化，不能按 bit 数直接推面积。

### 15.3 原文 Table 4 的 post-synthesis 结果

论文用 Synopsys Design Compiler、2 GHz 约束，对两个关键逻辑分别综合。下面是其 Table 4 的完整数字。

**A. 前端 reconvergence detection（WPB 比较逻辑）**

| WPB 配置 | 最大逻辑级数 | 面积 | 功耗，0.7 V |
| --- | ---: | ---: | ---: |
| 4 stream x 16 entry | 13 | 2,682 um^2 | 1.508 mW |
| 4 stream x 32 entry | 19 | 5,283 um^2 | 2.984 mW |
| 4 stream x 64 entry | 20 | 10,369 um^2 | 5.909 mW |

作者将这块匹配分摊在三个流水级，因此最大的 `4 x 64` 配置虽然有 20 级逻辑，仍没有把全部比较串进一个 fetch cycle。面积/功耗随 WPB 深度大致线性增长。

**B. rename reuse-test（RGID 比较、同拍依赖处理）**

| rename 宽度 | 最大逻辑级数 | 面积 | 功耗，0.7 V |
| --- | ---: | ---: | ---: |
| 4-wide | 28 | 3,201 um^2 | 3.039 mW |
| 6-wide | 32 | 4,803 um^2 | 4.333 mW |
| 8-wide | 41 | 6,256 um^2 | 5.509 mW |

这部分比 WPB 更值得警惕，因为它贴近宽 rename 的同拍 destination-to-source bypass。论文指出最差情况不是 ROB 容量，而是同一个架构寄存器在一个宽 rename group 内连续被重定义时，RGID 需要在同拍累加多次；作者建议预计算和缩短 RGID bitwidth 来缓解。

逻辑级数不是延迟的直接单位，不能用“41 级”直接乘一个固定 ps 得出时序结论。它只说明 8-wide reuse test 的组合深度明显高于 4-wide，香山必须用自己的标准单元库、rename 宽度、PVT 和布线约束重新综合。

### 15.4 作者没有给出的成本，以及对香山意味着什么

论文给出的两张表是**两个关键逻辑块的单独综合**，不是 MSR 开关前后完整核心的 area/power delta。它没有直接给出：

- WPB/SL 具体实现为 flop、SRAM 还是 latch 后的全核面积差；
- 为保留错误路径结果而增加多少 PRF 容量，或 reserve PReg 导致多少 rename stall；
- freelist hold/claim/release、ROB reuse-completion、checkpoint 扩展、跨模块连线的面积和功耗；
- 带入实际时钟树、布局布线、SRAM macro 后的频率变化；
- 包含 load replay、memory violation recovery 后的全系统能效。

因此最严谨的结论是：论文证明了前端比较和 rename reuse test 在其 2 GHz 目标下可以综合，且关键逻辑是数千平方微米、数毫瓦量级；但它**没有证明**把完整机制接入香山后的总体面积、功耗和频率成本也只有这个量级。

论文还以相近工艺节点下估算的香山 YXH 核约 `3.4 mm^2`、`5 W` 作为背景参考。即使把 Table 4 中最大 WPB 检测逻辑和 8-wide reuse-test 逻辑的数值相加，也只能得到约 `0.0166 mm^2` 和 `11.4 mW` 的两个逻辑块量级，不能称为 MSR 的总开销，也不能据此报告百分比 PPA。

### 15.5 对当前香山最应先量化的四个量

在动 RTL 前，建议先用参数化的统计/原型回答：

1. 每千条指令平均有多少 PReg-cycles 被 `SQUASH_HOLD` 占用，freelist 低水位是否恶化；
2. RGID 在现有 rename 宽度和同拍 bypass 上新增多少组合延迟；
3. WPB/SL 需要几个 read/write port，综合后面积主要来自 storage 还是比较器；
4. 实际可复用 uop 中 load、向量和特殊指令占比是多少。若纯整数单 uop 占比很低，复杂的完整机制就不划算。

## 16. 验证重点

MSR 的 bug 通常不会表现为简单的 branch 后死锁，而是低概率读到旧 PReg 或异常精确性被破坏。最低限度应覆盖：

- 相同 PC 但 source RGID 已变化：必须拒绝复用。
- 旧候选尚未 writeback：必须拒绝复用。
- 被保留 PReg 在 SL 有效期间：freelist 不得重新分配。
- redirect 恢复 snapshot 后：RAT 的 PReg 和 RGID 必须一起恢复。
- 同拍 producer-consumer，其中 producer 复用/不复用两种情况。
- 连续两个 redirect，当前路径回到较早而非最近的 stream。
- stream timeout、替换、被再次 squash 时：所有 reserve PReg 必须恰好释放一次。
- 排除的 store/CSR/异常 uop 永不进入 reuse 成功路径。
- 当前复用 uop 即使无需执行，也必须以当前路径 ROB 顺序提交。

## 参考

1. Qingxuan Kang and Trevor E. Carlson, "Multi-Stream Squash Reuse for Control-Independent Processors," MICRO 2025. [公开 PDF](https://www.comp.nus.edu.sg/~tcarlson/pdfs/kang2025msrfcp.pdf)
2. 本目录总览：[README.md](README.md)
