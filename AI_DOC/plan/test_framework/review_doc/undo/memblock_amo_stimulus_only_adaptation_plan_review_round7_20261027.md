# MemBlock AMO 无操作系统 UID+1 恢复方案第七轮 Review

## 1. Review 信息

- Review 日期：2026-10-27
- Review 对象：`memblock_amo_stimulus_only_adaptation_plan_20261022.md` v1.6/v1.7
- 修正需求：AMO 架构异常发生后，测试框架不能停留在原 fault UID 等待操作系统处理，需要按 UID+1 继续发射
- 最终结论：fault AMO 完成 redirect/terminal 后推进到 `fault_uid+1`，释放 barrier，并从下一条主表事务继续执行；普通 redirect 的 `dynamic_epoch+1` 仅保留给其他需要重放的事务

## 2. 为什么需要 UID+1

真实处理器发生地址非对齐、页故障或权限异常后，通常由操作系统异常处理程序修改执行环境，再决定返回原指令或终止程序。

当前 MemBlock 测试框架没有操作系统。如果异常 AMO 一直停留在原 UID，或者原样重新发射同一 opcode 和地址：

```text
非对齐地址仍然非对齐
页表或权限状态没有被软件修复
同一指令会再次产生相同异常
测试将永久循环在原fault UID
```

因此纯激励框架采用测试环境恢复策略：记录并完成该异常，然后跳过 fault UID，从主表中的下一条 UID 继续运行。

## 3. UID+1 的准确含义

假设发生异常的是：

```text
fault_uid = 10
```

异常收敛后：

```text
UID 10：fault terminal，不再进入IQ
commit/admission cursor：推进到11
下一条发射：主表中原本的UID 11
```

所以文档中的“UID+1 重发”表示：

```text
跳过已终结的异常UID，从下一条主表事务继续发射
```

它不表示：

```text
把UID 10的AMO复制成UID 11再执行
重新调用alloc_uid()创建一条事务
修改fault AMO的UID字段后重试原地址
```

## 4. 与 dynamic_epoch+1 的区别

两套机制必须分开：

| 场景 | 恢复方式 |
| --- | --- |
| AMO 自身产生架构异常，且测试环境没有操作系统 | fault UID terminal，推进到 `fault_uid+1` |
| 普通 redirect 覆盖了一条仍需重新执行的事务 | 静态 UID 不变，`dynamic_epoch+1` 后重新 admission/issue |

`dynamic_epoch+1` 表示“同一条主表事务的新动态实例”；`fault_uid+1` 表示“开始执行主表中的下一条事务”。

## 5. 执行流程

```text
atomic final WB带架构exception
-> 聚合完整final WB
-> UID进入fault/exception_pending
-> 等fault UID到modeled ROB head
-> request_fault_head_redirect(flush_itself=1)
-> 清理fault UID旧动态状态和相关redirect资源
-> fault UID完成terminal
-> set_commit_cursor_uid(fault_uid+1)
-> admission起点同步到fault_uid+1
-> 消费完迟到atomic feedback并释放barrier
-> UID+1按普通规则admission / route / issue
```

如果 UID+1 已经超出 `main_trans_num`，则正常进入 sequence drain/结束，不再创建新事务。

## 6. 实现约束

1. fault UID 必须先完成 final WB 聚合和 fault terminal，不能一发现 exception 就立即跳过。
2. CAS.Q 即使第一笔 final WB 已带异常，仍需按方案完成两笔 final WB 聚合后再形成 UID 级 fault。
3. `set_commit_cursor_uid(fault_uid+1)` 与下一 admission 起点必须一致，不能 commit cursor 前进但 dispatch 仍指向 fault UID。
4. barrier 必须在 fault terminal 和预期 STA feedback 收敛后释放，避免迟到 feedback 绑定到 UID+1。
5. fault UID 的 IQ item、held piece、owner bitmap、pending WB/fault 状态必须清空。
6. UID+1 使用其主表中原有 opcode、地址、数据和生成属性。
7. 不调用 `alloc_uid()`，不修改 `main_trans_num`，不复制 fault transaction。
8. SC reservation失败和 CAS比较失败没有架构 exception，仍走 normal commit，不执行 UID+1 fault 恢复。

## 7. 验收要求

1. 定向构造 UID=N 的非对齐 AMO，确认其产生 misaligned final WB 和 fault redirect。
2. 确认 UID=N 完成 terminal 后不再进入 STA/STD IQ。
3. 确认 commit cursor 和 admission 起点都变为 N+1。
4. 确认 barrier 释放后第一条新 admission/issue 是主表 UID=N+1。
5. 确认 UID=N+1 使用自身原有事务内容，不继承 fault AMO 的 opcode、地址或 STD 数据。
6. 连续构造多个 fault UID，确认每次都向下一 UID 推进，不发生死循环。
7. fault UID 为最后一条主表事务时，确认框架进入 drain 并正常结束。
8. SC失败和 CAS比较失败用例确认不触发 fault UID+1 路径。

## 8. 最终结论

v1.7 使用 UID+1 作为无操作系统环境下 AMO 架构异常的恢复点，能够保证异常被记录和收敛后继续运行后续测试激励。该方案复用现有 `finish_fault_head_terminal()` 的 commit cursor 推进逻辑，只需要确保 admission 起点、atomic barrier 和旧 owner 清理与其同步，不需要增加操作系统模型或异常修复模型。
