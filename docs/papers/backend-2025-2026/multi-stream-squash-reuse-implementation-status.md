# Multi-Stream Squash Reuse 2x128：实现方案、当前进度与后续工作

## 1. 当前结论

截至 2026-08-10，`feat-redirec2-64` 分支上的实现已经从性能计数器版本推进到真实结果复用路径：

- Squash Log 配置为 `2 streams x 128 instruction entries`；
- RGID 为 6 bit，`0` 为 NULL；
- 正确路径通过 `streamId + streamGeneration + instructionOffset` 直接选择日志项；
- 已完成且满足安全条件的错误路径整数 ALU 结果会保留其 PReg；
- 正确路径命中后会 claim 该 PReg 和旧 `destRgid`，不申请新的 PReg/RGID；
- reused 指令仍进入 ROB/RAB 并按序提交，但不置 BusyTable、不进入 IQ、不执行、不写回；
- stream 替换、静态路径分歧、RGID overflow 和 claim 均有对应的 release/所有权处理；
- 对 ROB compression 和 fusion 增加了原始指令位置 metadata，Squash Log offset 按原始指令而不是压缩 ROB entry 计数。

当前代码已经通过：

- `git diff --check`；
- `make check-format`；
- Scala 2.13 compiler 的 `-Ystop-after:parser`，即所有修改文件通过语法解析。

按照用户要求，本轮没有继续执行类型编译、RTL elaboration、`make emu` 或 CoreMark。因此当前准确表述是：

> 真实复用数据通路已经接入，Scala 语法无错误，但仍需要完整编译和仿真验证后，才能认为功能完成。

代码只保留在本地，没有推送远程。

## 2. 目标配置与复用边界

| 项目 | 当前配置 | 说明 |
| --- | ---: | --- |
| Squash Log stream | 2 | 保存最近两条错误动态流 |
| 每个 stream 的 SL entry | 128 | 总计 256 条原始指令记录 |
| 每个 stream 的 WPB entry | 32 | 保存 fetch block 顺序和 SL 起始 offset |
| RGID | 6 bit | `0` 为 NULL，非零值表示映射版本 |
| stream generation | 32 bit | stream 槽位替换后拒绝旧 candidate |
| 最大 held PReg | 32 | 防止 Squash Log 长时间占满整数 freelist |
| freelist 保留量 | 至少 `RenameWidth` | 防止 hold 导致 Rename 无法前进 |

第一版只允许复用确定性、单 uop、单整数目的寄存器的普通整数 ALU 指令。完整命中条件是：

```text
candidate streamId/generation/instructionOffset 有效
&& 指定 SL entry 有效且未被消费
&& 当前 PC 和指令编码与日志相同
&& 当前指令没有参与 fusion 或 ROB compression
&& 旧指令已经完成
&& 当前和日志项都是可复用整数 ALU
&& source-used mask 相同
&& 每个使用的整数源 RGID 非 NULL 且相同
&& 日志项仍拥有旧结果 PReg
&& 当前指令无异常、trigger、single-step、flush 或副作用
```

当前明确不复用 load、store、AMO、branch、jump、CSR、fence、系统指令、浮点、向量、多 uop、move elimination、fusion 指令，以及任何异常或副作用指令。

## 3. 总体数据流

```text
错误预测 redirect
    |
    +--> ROB sidecar 按原始指令保存 PC/instr/FTQ/RGID/pdest/class
    |
    +--> 按 ROB 年龄和 instruction slot 生成 128-entry stream
    |
    +--> 已完成 reusable ALU：
           CURRENT_SPEC -> SQUASH_HOLD
           PReg 仍在 circular freelist 中，但普通 allocation 必须跳过

正确路径重新取指
    |
    +--> WPB 选择唯一 streamId + streamGeneration + block offset
    |
    +--> 用 block 内指令位置得到 instructionOffset
    |
    +--> 直接读取一个 SL entry，PC/instr 只作 guard

Rename
    |
    +--> 比较全部源 RGID、完成状态、指令类别和 PReg owner
           |
           +--> 失败：普通 PReg/RGID allocation 和正常执行
           |
           +--> 成功：claim 旧 PReg + 旧 destRgid
                     SQUASH_HOLD -> CURRENT_SPEC
                     numWB = 0，msrReused = 1

Dispatch / ROB
    |
    +--> 不把 reused PReg 重新置 busy
    +--> 不生成 IQ valid，不进入 EXU/WB
    +--> 仍建立 ROB/RAB 项，正常 commit 或 redirect recovery
```

## 4. 已完成的实现

### 4.1 6-bit RGID

已经实现：

1. `MsrRgid.Width = 6`，`MsrRgid.Null = 0`；
2. 每个整数架构寄存器维护独立 `NextRGID`；
3. `NextRGID` 不随 redirect 回滚；
4. speculative RAT、architectural RAT 和 snapshot 都保存 RGID；
5. 同周期 producer-consumer 旁路同时传递 `pdest` 和 `destRgid`；
6. 同一个 rename group 多次写同一逻辑寄存器时分配不同 RGID；
7. reused mapping 继承日志中的 `destRgid`，不分配新 generation；
8. generation 耗尽后进入 quarantine，清空 stream/held 状态，经过 ROB-sized commit drain 后 reset；
9. NULL source RGID 不允许命中复用。

### 4.2 2x128 stream 和 WPB 定位

已经实现：

1. 两个 stream，每个 128 条原始指令；
2. 每个 stream 32 个 WPB block；
3. stream 槽位替换时 generation 增加；
4. candidate 携带 `streamId + streamGeneration + instructionOffset`；
5. SL 读取使用 tuple 直接索引，不再按 PC 扫描 256 项；
6. PC 和指令编码仅用于 tuple 选中后的静态身份检查；
7. WPB discovery 只接受上下文过滤后恰好一个位置的 stream；
8. 同一 stream 内循环 PC 出现多个候选位置时拒绝，而不是用 PriorityEncoder 任意挑一个；
9. entry 消费、stale generation、跨 stream 和同拍重复选择均有断言。

### 4.3 原始指令粒度的 ROB sidecar

香山支持多个标量指令压缩到一个 ROB entry。仅按 ROB entry 捕获会导致论文中的 instruction offset 错位。

当前 sidecar 为每个 ROB entry 保留最多 `RenameWidth` 个 instruction slot，并记录：

```text
PC / instruction bits / FTQ pointer / FTQ offset
source-used mask / source RGID
dest RGID / pdest
ALU / reusable / owns-int-PReg / reused
```

捕获顺序先遍历同一 ROB entry 内的 instruction slot，再进入下一个 ROB entry。因此：

- 普通 ROB compression 不再丢失后续原始指令位置；
- fusion 被消除的第二条架构指令仍额外占一个不可复用 metadata slot；
- fusion 第二条不会 claim PReg，但后续 instruction offset 不会少 1；
- 只有整个压缩 ROB entry 已完成时，其中的 reusable ALU 才允许 hold，完成判断保守但安全；
- redirect 与最后一次 writeback 同拍时，如果本拍 writeback 会把 `uopNum` 清零，也视为已完成。

### 4.4 PReg hold、release 和 claim

整数 freelist 增加 `heldMask`，所有权规则是：

```text
普通 speculative allocation：free -> current instruction
redirect 捕获：              free-after-recovery -> Squash Log hold
正确路径 claim：             Squash Log hold -> current instruction
失配/替换/overflow：         Squash Log hold -> ordinary free
```

实现采用“PReg 仍留在 circular queue 中，但 allocation 跳过 held 项”的方式。原因是原 freelist 的 redirect snapshot 只保存 head pointer，如果把任意 held PReg 物理删除，会破坏 snapshot 恢复。

普通 allocation 的选择过程是：

1. 从当前 head 开始扫描第一个非 held PReg；
2. 把它与当前 head 位置交换；
3. head 前进一项；
4. consumed 区间仍准确保存 redirect 后应回收的分配记录。

claim 的选择过程类似，但扫描目标是指定的 held PReg。目标被交换到 head 后随 head 前进而成为当前正确路径的 allocation，同时 `heldMask` 清除对应 bit。

这里把 claim 分成两个信号：`claimReq` 在 Rename 输出阻塞时仍保持稳定，用于稳定选择 PReg；`claimFire` 只在 Rename 真正握手时更新 `heldMask`。这样不会形成 `ready -> PReg 输出` 的不稳定路径，也不会在未发送指令时提前转移所有权。

release 不在同一拍立即影响普通 allocation，而是在时钟沿后生效。这样避免以下组合环：

```text
ROB Enq -> release -> freelist canAllocate -> Rename fire -> ROB Enq
```

另外，hold admission 同时受两个条件限制：

- 总 held 数不超过 32；
- `当前可用 + 本次 squash 回收 + 本次 release - 新 hold >= RenameWidth`。

这保证正确路径至少能继续 Rename 一个完整 group，不会因为所有回收 PReg 都被 hold 而死锁。

### 4.5 Rename 最终命中和执行消除

Rename 在同周期旁路完成后检查最终源 RGID，命中后：

1. `pdest := logged_pdest`；
2. `destRgid := logged_destRgid`；
3. 关闭普通 PReg allocation；
4. 关闭新 RGID allocation；
5. 向 freelist 和 ROB 发送 claim；
6. `msrReused := true`；
7. `numWB := 0`；
8. 同 rename group 的后续消费者看到复用后的 PReg 和 RGID。

Dispatch 对 `msrReused` 做以下处理：

- 不向整数 BusyTable发送 alloc，旧 PReg 保持 ready；
- 不让 RegCacheTagTable 将旧 PReg 当成新 allocation 失效；
- IQ 端口计数忽略 reused lane；
- `fromRenameUpdate.valid = false`，因此没有 IQ、issue、execute 和 writeback；
- ROB/RAB enqueue 仍正常发生，commit 顺序和 old mapping 释放不变。

### 4.6 恢复与所有权断言

已经覆盖：

- claim 与普通 release 同拍互斥；
- 两个 lane 不能 claim 同一 SL entry/PReg；
- stream replacement 只释放仍由日志持有的 PReg；
- claim 后日志立即 consumed，并失去 release 权；
- reused 指令再次被 squash 时，freelist snapshot 能回收 PReg；
- 如果是新的 misprediction stream，可再次 hold 该结果；
- overflow 和静态路径分歧释放 held PReg；
- held PReg 不允许被普通 allocation 选中；
- PReg 不能同时由两个日志项持有；
- Squash Log retained mask 必须与 freelist held mask 完全一致；
- reused 指令必须 `numWB == 0`、不得置 BusyTable、不得产生 IQ valid。

## 5. 新增和关键性能计数器

### 5.1 实际复用结果

| 计数器 | 含义 |
| --- | --- |
| `msr_entry_claimed` | SL entry 成功把 held PReg 转给正确路径的次数 |
| `msr_reused_inst` | Dispatch 实际接收的 reused 指令数 |
| `msr_reused_committed_inst` | 最终正常提交的 reused 指令数 |
| `msr_reused_then_squashed_inst` | claim 后又被后续 redirect squash 的指令数 |
| `msr_saved_dispatch` | 省掉的 IQ dispatch/enqueue 操作数 |
| `msr_saved_issue` | 省掉的 issue 操作数 |
| `msr_saved_execute` | 省掉的 execute 操作数 |
| `msr_saved_writeback` | 省掉的 writeback 操作数 |

当前四个 saved 计数在第一版中都等于 `msr_reused_inst`，因为只允许单 uop 整数 ALU。

### 5.2 PReg 生命周期和压力

| 计数器 | 含义 |
| --- | --- |
| `msr_entry_hold_attempt` | 已完成 reusable ALU 的 hold 尝试数 |
| `msr_entry_held` | 实际接纳的 held entry 数 |
| `msr_entry_hold_reject_pressure` | 因 held 上限或 freelist 预算拒绝的数量 |
| `msr_entry_hold_reject_low_watermark` | 会违反 freelist 最低保留量的数量 |
| `msr_entry_released` | 所有原因合计的 PReg release 数 |
| `msr_entry_released_on_match` | 位置被消费但未 claim 时的 release 数 |
| `msr_entry_evicted` | stream 替换导致的 release 数 |
| `msr_entry_released_on_divergence` | 静态路径分歧时释放的数量 |
| `msr_entry_released_on_rgid_overflow` | RGID overflow 时释放的数量 |
| `msr_held_preg_occupancy` | held 数逐周期累计，可用于求平均占用 |
| `msr_held_preg_peak` | 运行期间 held PReg 峰值 |
| `msr_held_preg_cycle` | 至少有一个 held PReg 的周期数 |
| `msr_freelist_hold_blocked_cycle` | held 压力导致 Rename 可用量不足的周期数 |

### 5.3 候选拒绝原因

| 计数器 | 含义 |
| --- | --- |
| `msr_candidate_ambiguous_position` | WPB 中出现多个动态位置候选 |
| `msr_candidate_ambiguous_reject` | 因没有唯一位置而拒绝 discovery |
| `msr_candidate_fusion_reject` | 当前 candidate 参与 fusion，拒绝复用 |
| `msr_rgid_null_reject` | 使用源的当前或日志 RGID 为 NULL |
| `msr_rgid_mismatch_reject` | 非 NULL 源 RGID 不相等 |
| `msr_instruction_class_reject` | 静态命中但不是允许的单 uop 整数 ALU |
| `msr_position_static_divergence` | tuple 选中项与当前 PC/指令编码不同 |
| `msr_redirect_writeback_completed_inst` | redirect 同拍才完成的 ROB entry 数 |

旧的 `msr_static_hit_inst`、`msr_completed_*` 等 profiling 计数仍保留，但 `msr_squashed_inst` 现在按原始指令计数；新增 `msr_squashed_rob_entry` 用于保留 ROB-entry 粒度对照。旧 `2*64cnt.log` 与新日志比较时必须注明语义变化。

## 6. 当前还缺什么

### 6.1 完整类型编译和 elaboration

本轮只做了 parser-only 语法检查。下一步必须运行：

```bash
mill -i xiangshan.compile
make emu -j96
```

重点关注：

- 新 Bundle 字段是否全部通过 Chisel 类型连接；
- nested Vec 动态写是否能正常 elaboration；
- freelist 的选择性交换逻辑是否产生 width 或组合环报错；
- 2x128 SL 加 ROB instruction sidecar 后的 elaboration 规模和时序代价。

### 6.2 定向功能验证

至少需要覆盖：

1. 相同 PC 的不同循环迭代出现多个 WPB 位置时必须拒绝；
2. 仅一个 source RGID 改变时不得 claim；
3. 同周期 producer-consumer 的 RGID 和 PReg 旁路；
4. 同一 rename group 两次写同一逻辑寄存器；
5. redirect 与最后一次 ALU writeback 同拍；
6. stream replacement 与 claim 同拍；
7. claim 后再次 redirect；
8. RGID overflow、quarantine、ROB-sized drain 和 reset；
9. move、x0、fusion、ROB compression 和异常指令；
10. freelist 低水位时只 profiling、不 hold；
11. 多 lane claim 不同 held PReg；
12. 长时间运行中 held/free/current owner 始终互斥。

### 6.3 性能和面积风险

为了在压缩 ROB 中保持原始指令 offset，当前实现增加了每 ROB entry 最多 `RenameWidth` 个 metadata slot。该方案直接、保守，但硬件开销明显，需要在 elaboration 后检查：

- sidecar 寄存器数量；
- redirect 捕获的动态索引和组合深度；
- freelist 每 lane 扫描/交换逻辑；
- ROB 到 Rename 的 candidate/reuseInfo 组合路径。

如果时序或面积不可接受，下一版应把 sidecar 改成独立的 instruction history ring，并对 candidate 和 hold 做流水化，而不是改变正确性条件。

### 6.4 最终仿真和数据对比

编译通过后运行：

```bash
./build/emu \
  -i ready-to-run/coremark-2-iteration.bin \
  --diff ready-to-run/riscv64-nemu-interpreter-so
```

新日志不能覆盖：

- `2*64cnt.log`；
- `2*128cnt.log`；
- `2*64cnt-rgid.log`。

建议保存为：

```text
2x128-msr-reuse-coremark.log
```

最终分析至少报告：

```text
hold acceptance = msr_entry_held / msr_entry_hold_attempt
claim rate       = msr_entry_claimed / msr_entry_held
commit rate      = msr_reused_committed_inst / msr_entry_claimed
squash-after-use = msr_reused_then_squashed_inst / msr_entry_claimed
average held     = msr_held_preg_occupancy / cycles
saved work       = msr_saved_execute 等实际计数
```

## 7. 主要修改文件

| 文件 | 当前职责 |
| --- | --- |
| `xiangshan/Bundle.scala` | 6-bit RGID 和 2x128/held 配置 |
| `backend/Bundles.scala` | candidate、reuseInfo、claim 和 reused/fusion metadata |
| `backend/rename/Rename.scala` | 最终安全比较、PReg/RGID claim、同拍旁路 |
| `backend/rename/freelist/MEFreeList.scala` | held mask、选择性 allocation、claim/release、低水位 |
| `backend/dispatch/Dispatch.scala` | BusyTable/RegCache/IQ 抑制和 saved-work 计数 |
| `backend/rob/MultiStreamSquashReuse.scala` | stream/WPB/SL、instruction sidecar、owner 和统计 |
| `backend/rob/Rob.scala` | MSR 接口 |
| `backend/CtrlBlock.scala` | Rename、ROB、freelist 的 MSR 连线 |

## 8. 完成标准

只有同时满足以下条件，才能把实现标记为完成：

1. `mill -i xiangshan.compile` 通过；
2. `make emu -j96` 通过；
3. CoreMark+difftest 正常结束；
4. `msr_entry_claimed`、`msr_reused_inst` 和 `msr_saved_execute` 语义一致；
5. reused 指令确实没有 IQ/EXU/WB 活动；
6. held PReg 从未被普通 freelist allocation；
7. claim、redirect、replacement、overflow 下没有 assertion；
8. 最终提交结果与 NEMU 一致；
9. 新计数器与旧 2x64/2x128 profiling 数据完成带语义说明的对比；
10. 所有实现以本地 commit 保存，不推送远程。

当前完成度可概括为：**实现已接通，语法已检查，完整编译和仿真尚未验证。**
