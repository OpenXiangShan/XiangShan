# MemBlock AMO 非对齐与异常 Redirect 方案第六轮 Review

> 状态说明：本轮关于非对齐生成、顺序保护和 final WB 分类的结论继续有效；“UID+1”的解释已被第七轮 review 修正。当前方案以 v1.7 的“fault terminal 后推进到 fault_uid+1”作为无操作系统恢复语义。

## 1. Review 信息

- Review 日期：2026-10-27
- Review 对象：`memblock_amo_stimulus_only_adaptation_plan_20261022.md` v1.5/v1.6
- 新增需求一：每条 AMO 地址生成时按可配置随机权重决定对齐或非对齐，默认千分之一
- 新增需求二：监测 atomic final WB；需要 redirect 的异常复用普通访存 fault 流程，不需要 redirect 的 SC/CAS 失败正常提交
- Review 边界：纯激励和既有生命周期适配，不增加 RM、scoreboard、结果 checker 或 RTL 修改
- 最终结论：v1.6 功能完整；采用单个 permille 参数、非对齐 STD-first 合法化、exception-based 分流和既有 dynamic epoch 重发机制，无方案级 blocker

## 2. 源码结论

### R6-1：W/D/Q 的自然对齐要求固定

`AtomicsUnit.scala` 根据 `fuOpType[1:0]` 检查：

```text
W：vaddr[1:0] == 0，4B对齐
D：vaddr[2:0] == 0，8B对齐
Q：vaddr[3:0] == 0，16B对齐
```

LR 非对齐设置 `loadAddrMisaligned`；SC、普通 AMO 和 CAS 非对齐设置 `storeAddrMisaligned`。

### R6-2：非对齐是 final WB fault，不是拆分访问

TLB response 检测到非对齐后，AtomicsUnit 直接进入 `s_finish`：

```text
不进入PMP/DCache原子访问流程
不产生DCache atomic request
通过writebackLda_0返回带misaligned exception的final WB
CAS.Q仍按s_finish/s_finish2产生两笔final WB
```

因此测试框架只需生成地址、完整发送所需子 uop、监测 final WB 和驱动 fault 生命周期，不实现非对齐拆分。

### R6-3：非对齐 AMO 不能使用 STA-first

非对齐检查不等待 `data_valid`。如果 STA0 先启动 AtomicsUnit，非 CAS 原子可能在迟到 STD 进入前已经完成并复位 FSM，造成迟到 STD 污染下一条原子事务。

最小且安全的输入规则：

```text
misaligned LR/SC/普通AMO：全部STD -> STA0
misaligned CAS.W/D：      全部STD -> STA0
misaligned CAS.Q：        全部STD -> STA0 -> STA2
```

STD 内部仍可按 `MEMBLOCK_AMO_STD_SHUFFLE_EN` 乱序。该规则只覆盖非对齐 owner；aligned owner 继续支持三种 issue-order mode。

### R6-4：SC/CAS 失败不是架构异常

SC reservation 失败、地址不匹配或超时通过返回数据表示失败，`exceptionVec` 不因此置位。CAS 比较失败返回旧值且不更新内存，也不置异常。

因此 final WB 分类必须是：

```text
带架构exception或等价trigger fault -> fault redirect/terminal
无架构exception                    -> normal writeback/commit
```

纯激励不解析返回数据，不判断 SC/CAS 成功率；无 exception 即正常完成。

### R6-5：fault redirect 不是发现 WB 的同拍立即 redirect

正确流程为：

```text
atomic final WB聚合exception
-> UID进入fault/exception_pending
-> 等该UID到modeled ROB head
-> request_fault_head_redirect(flush_itself=1)
-> redirect/cancel收敛
-> fault UID terminal
```

这与普通访存精确异常流程一致。

### R6-6：当前框架的重发身份不是新静态 UID

`alloc_uid()` 只在主表初始建表时递增 `next_uid`。普通 redirect 恢复通过 `prepare_uid_for_redirect_reissue()` 对被 flush 的事务执行：

```text
静态UID保持不变
dynamic_epoch++
清除旧动态实例状态
重新admission / route / issue
```

因此需求中的“UID+1 发射”在当前框架应实现为动态实例代号 `dynamic_epoch+1`，不能再次调用 `alloc_uid()`。

### R6-7：异常 AMO 自身不应重新发射

fault head 使用 `flush_itself=1` 产生精确异常 redirect，但现有 commit 收口明确不把 fault UID 重新路由。这样才符合架构异常终结语义。

如果将同一非对齐地址、页故障或权限故障的 AMO 自身重新发射，它仍会产生同一异常并形成永久循环。因此：

```text
fault AMO自身：fault terminal，不重发
被redirect清除的年轻事务：同UID、dynamic_epoch+1重发
```

## 3. 参数 Review

采用单参数而不是 enable 加 weight 两个参数：

```text
MEMBLOCK_AMO_MISALIGN_PERMILLE=1
```

语义：

```text
范围0..1000
0    = 关闭非对齐生成
1    = 默认千分之一
1000 = 所有AMO都生成非对齐地址
```

优点是改动最小，并且 0 自然承担关闭开关。随机性复用现有 sequence seed，不增加独立 seed。

## 4. 地址生成 Review

每条 AMO 主事务首次生成地址时只抽样一次：

```text
make_misaligned = random(0..999) < permille
```

然后：

1. 根据 opcode 得到 alignment bytes：W=4、D=8、Q=16。
2. aligned 时清零对应低位。
3. misaligned 时从 `1..alignment-1` 选择非零 offset。
4. 保证完整 4B/8B/16B 访问不越出 `MEMBLOCK_MAIN_VADDR_BASE/RANGE`。
5. 保存最终地址和 `amo_expect_misaligned`。
6. redirect reissue 复用原地址，不重新随机。

如果每个 dynamic epoch 都重新抽样，会让同一静态事务在 redirect 前后改变架构输入，不能接受。

## 5. Final WB 与 Redirect Review

最小处理规则：

```text
if atomic_final_wb_has_arch_fault:
    聚合完整final WB
    设置UID writeback/fault/exception_pending
    等到ROB head后复用普通fault redirect
    fault owner terminal，不重新issue
else:
    设置UID writeback/pass
    正常ROB commit和terminal
```

其中 `atomic_final_wb_has_arch_fault` 包括 misaligned、page/guest-page/access fault、PMP/PMA/PBMT拒绝、cache denied/corrupt 在配置允许时形成的异常，以及需要精确异常处理的 breakpoint/trigger fault。TLB miss、DCache miss/replay、SBuffer/Uncache等待不是 final fault，不触发 redirect。

## 6. 验收要求

1. permille 只接受 0..1000，默认值为 1。
2. permille=0/1000 分别得到全对齐/全非对齐 AMO。
3. W/D/Q 非对齐地址分别违反 4B/8B/16B 对齐，同时完整访问不越出地址窗口。
4. 同一 UID redirect reissue 前后地址和 misalign 属性完全一致。
5. 非对齐 LR 返回 load-address-misaligned；其他原子返回 store-address-misaligned。
6. 非对齐动态实例不产生 DCache atomic request。
7. 非对齐 owner 所有 STD 先于 STA0；CAS.Q 保持 STA0 先于 STA2；STD shuffle 仍有效。
8. 架构 fault 等到 ROB head 后触发既有 scalar fault redirect。
9. fault AMO 自身 terminal，不重新 route/issue。
10. 被 redirect 清除的年轻事务静态 UID 不变，`dynamic_epoch+1` 后重新 admission/issue。
11. SC失败、CAS比较失败无 exception 时不触发 redirect，正常 commit/terminal。
12. CAS.Q 的 fault 仍聚合两笔 final WB，不因第一笔异常提前释放 barrier。

## 7. 最终结论

v1.6 已把非对齐地址生成、顺序保护、final WB 分类、fault redirect 和动态重发身份纳入同一套最小方案。新增功能复用现有地址生成器、atomic owner、writeback adapter、fault commit 和 redirect worker，不需要新端口、第二 atomic owner、RM/checker 或 RTL 改动。
