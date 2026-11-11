# MemBlock AMO 纯激励最小适配方案第三轮 Review

## 1. Review 信息

- Review 日期：2026-10-26
- Review 对象：`memblock_amo_stimulus_only_adaptation_plan_20261022.md` v1.2
- Review 轮次：第三轮
- 新增需求：支持同一原子指令内部 STA/STD 乱序发送，并提供参数控制
- Review 边界：仅激励生成、发送和生命周期收敛；不包含 RM、scoreboard、结果 checker 和 coverage
- 最终结论：v1.2 的固定顺序不满足新增需求；修订为 v1.3 后，无剩余方案级 blocker

## 2. Review Findings

### R3-1：固定 STD-first 不满足参数化乱序需求

严重级别：P0 blocker。

v1.2 固定执行 `全部 STD -> STA0 -> CAS.Q STA2`，只能验证一种合法到达顺序，无法覆盖 AtomicsUnit 在地址先到、部分数据先到和 STA/STD 交错情况下的等待与收集行为。

修正：增加 `MEMBLOCK_AMO_ISSUE_ORDER_MODE`：

```text
0 = STD_FIRST
1 = STA_FIRST
2 = RANDOM_LEGAL
```

默认保持 mode 0，避免改变旧 smoke；新增 cfg 分别覆盖三种模式。

### R3-2：不能在每拍重新随机选择子 uop

严重级别：P0 blocker。

若 scheduler 每拍从 remaining item 中随机选择，在目标端口 `ready=0` 时可能改变 valid 对应的 target、slot、payload 或 issue epoch，违反 Decoupled 输入在等待握手期间保持稳定的要求，也可能造成重复删除和错误记账。

修正：owner 建立时只生成一次最多 6 项的固定 schedule。当前 entry 保持到 `valid && ready`，fire 后才 exact delete 并推进 position。

### R3-3：counter 不能证明 slot 完整唯一

严重级别：P0 blocker。

AtomicsUnit 的 `stdCnt` 只统计接收数量。重复 slot 仍会增加 count，并覆盖同一个数据寄存器。测试框架若也只保存 fired count，无法证明 required slot 没有重复或遗漏。

修正：owner 增加 required/fired STA bitmap 和 STD bitmap。schedule 构造后检查集合相等，fire 前检查对应 bit 未置位，fire 后置位。

### R3-4：CAS.Q 只允许部分乱序

严重级别：P0 blocker。

CAS.Q 的 `STA slot0` 是主输入，负责锁存标准 opcode、地址和第一个 pdest；`STA slot2` 只补充第二个 pdest。slot2 若在 AtomicsUnit 空闲时先到，会把带 slot 高位的 `fuOpType` 当主 uop，破坏精确 `isAMOCASQ` 判断。

修正：所有模式强制 `STA0 < STA2`。STD0..3 可任意交错，STA2 可在 STA0 之后与 STD 任意交错。

### R3-5：LR 不能安全执行真正的 STA-first

严重级别：P0 blocker。

LR 的 DCache `M_XLR` 请求不等待 `data_valid`。若 STA 先到而 STD 长时间受阻，LR 可能完成并复位 AtomicsUnit，迟到 STD 随后进入空闲单元并污染下一事务。

修正：所有模式对 LR 强制 `STD0 < STA0`。mode 1 或 mode 2 生成 LR schedule 时进行合法化并打印日志。这是 RTL 输入安全约束，不是随机覆盖缺失。

### R3-6：CAS.Q 第一笔 final WB 可能早于 STA2

严重级别：P0 blocker。

乱序模式下若 STA2 被 ready backpressure 压住，CAS.Q 的低半 final WB 可能先到。若 adapter 要求“WB 到达时所有 piece 已 fire”，会把合法事件误判为 fatal。

修正：只要求主 STA0 已 fire 即可绑定唯一 owner。早到 WB 先暂存，UID 级完成必须同时满足：

```text
all_required_pieces_fired
and seen_final_wb_count == required_final_wb_count
```

### R3-7：不需要同时引入动态物理端口或同周期多发射

严重级别：非 blocker，最小改动确认。

新增需求是 STA/STD 到达次序可控，不等于必须随机选择物理 STA/STD port，也不要求同周期发送多片。

处理：继续固定物理 STA0/STD0、单 atomic owner、每拍最多一片。slot 与物理 pipe 已解耦，因此可以覆盖时序乱序而不扩大 driver 和 feedback 绑定范围。

## 3. 修订后的调度规则

```text
mode 0 STD_FIRST：
  STD升序 -> STA0 -> CAS.Q STA2

mode 1 STA_FIRST：
  普通AMO/SC/CAS.W/D：STA0 -> STD升序
  CAS.Q：STA0 -> STA2 -> STD升序
  LR：  STD0 -> STA0

mode 2 RANDOM_LEGAL：
  required STA/STD集合执行一次受约束shuffle
  CAS.Q保持STA0 < STA2
  LR保持STD0 < STA0
```

乱序范围仅限同一 owner 的子 uop。atomic barrier、单 owner、无年轻事务 admission、无非 owner issue 的规则保持不变。

## 4. 必要验证

1. mode 0/1/2 分别运行 directed smoke。
2. mode 2 使用固定的多 seed 集合，确认至少出现两种不同合法 permutation。
3. 对 CAS.W/D/Q 检查 required/fired bitmap 最终完全相等。
4. 对 issue ready 注入 backpressure，确认当前 entry 和 payload 稳定。
5. 定向压住 CAS.Q STA2，确认第一笔 WB 可暂存且不会提前 finalize。
6. 检查 LR 在 mode 1/2 下始终被合法化为 STD-first。
7. 检查 CAS.Q 任意 seed 下都满足 STA0 早于 STA2。

## 5. 最终结论

v1.3 已在不增加多 owner、动态物理端口、同周期多片或 checker 的前提下支持可参数控制的 STA/STD 乱序发送。

采用预生成 schedule、required/fired bitmap、CAS.Q/LR 偏序和早到 WB 暂存后，新增需求与当前 AtomicsUnit 输入协议兼容。第三轮没有剩余方案级 blocker，可以进入 coding；最终仍需实现级 review、编译和上述模式仿真确认。
