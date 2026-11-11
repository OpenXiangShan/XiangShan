# MemBlock AMO STD 内部乱序方案第四轮 Review

## 1. Review 信息

- Review 日期：2026-10-27
- Review 对象：`memblock_amo_stimulus_only_adaptation_plan_20261022.md` v1.3
- 新增需求：STD slot 之间需要支持独立乱序，并提供开关参数
- Review 边界：只修改纯激励发送方案，不增加 checker、RM、多 owner 或动态物理端口
- 最终结论：增加独立 STD shuffle 参数并采用两阶段 schedule 后，方案可进入 coding

## 2. 主要发现

### R4-1：RANDOM_LEGAL 不能替代独立 STD 乱序开关

v1.3 的 `MEMBLOCK_AMO_ISSUE_ORDER_MODE=2` 会同时改变 STA/STD 交错和 STD 到达顺序，无法回答“只打乱 STD，保持 STD-first 或 STA-first”的场景。

修正：新增：

```text
MEMBLOCK_AMO_STD_SHUFFLE_EN=0  // STD slot升序
MEMBLOCK_AMO_STD_SHUFFLE_EN=1  // 多STD指令从全部合法slot排列中随机选择
```

它与 `MEMBLOCK_AMO_ISSUE_ORDER_MODE` 正交。

### R4-2：schedule 必须分两阶段构造

若先把所有 STA/STD 放在一起随机，再尝试判断 STD 是否乱序，无法保证 shuffle 开关关闭时 STD 保持升序，也无法保证开关打开后 merge 不会二次改变 STD 顺序。

修正：

1. 根据 opcode 建立 required STD list。
2. 根据 shuffle enable 生成并锁存 `std_slot_order[]`。
3. 根据 issue order mode，把 STA list 与 STD list 做拼接或稳定随机 merge。
4. merge 只能从两个 list 的 head 取元素，保持各自内部顺序。

### R4-3：shuffle enable 应允许全部合法排列

标准 Fisher-Yates 可能随机得到原序，这也是合法结果。开关语义应是“启用随机排列”，而不是“强制每个 owner 都非升序”。

修正：不对 Fisher-Yates 结果做二次修改。由此：

- CAS.W/D 可得到 `[0,1]` 或 `[1,0]`；
- CAS.Q 可得到全部 24 种 permutation，包括 `[0,1,2,3]`；
- 基础 AMO/LR/SC 只有 STD0，开关无可见影响。

### R4-4：不能改变 slot 数据语义

乱序只改变 fire 顺序：

```text
slot0仍是rd_l/基础AMO数据
slot1仍是rs2_l
slot2仍是rd_h
slot3仍是rs2_h
```

field assigner 继续按 slot 生成数据，scheduler 只调整携带这些 slot 的 item 次序。

### R4-5：反压期间不能重新 shuffle

`issueStd_0_ready=0` 时，当前 valid 对应的 slot、payload 和 issue epoch 必须保持稳定。

修正：shuffle 和 merge 只在 owner 建立时执行一次；当前 schedule entry 只有 fire 后才推进。

## 3. 参数组合

| ISSUE_ORDER_MODE | STD_SHUFFLE=0 | STD_SHUFFLE=1 |
| --- | --- | --- |
| STD_FIRST | STD升序后STA | STD随机排列后STA，允许原序 |
| STA_FIRST | STA后STD升序 | STA后STD随机排列，允许原序 |
| RANDOM_LEGAL | STA/STD交错，STD子序列升序 | STA/STD交错，STD子序列使用固化乱序 |

CAS.Q 始终保持 `STA0 < STA2`；LR 始终保持 `STD0 < STA0`。这些约束优先级高于两个随机参数。

## 4. 最小实现结构

owner record 增加：

```text
std_slot_order[4]
std_slot_order_len
issue_schedule[6]
issue_schedule_len / pos
required/fired_std_slot_mask
```

不增加新的 IQ、agent、driver port 或物理 pipe选择器。所有 STD 仍通过物理 STD0 分拍发送，`fuOpType[8:6]` 区分 slot。

## 5. 验证要求

1. shuffle=0 时所有 CAS 的 STD 子序列保持升序。
2. shuffle=1 的多 seed 回归中，CAS.W/D 同时出现 `[0,1]` 和 `[1,0]`。
3. shuffle=1 的多 seed 回归中，CAS.Q 至少出现一次 `[0,1,2,3]` 和一次非升序 permutation。
4. mode 0 + shuffle 1 验证“仅 STD 内部乱序”。
5. mode 1 + shuffle 1 验证“STA-first 且 STD 内部乱序”。
6. mode 2 + shuffle 0/1 分别验证 merge 不破坏固化 STD 子序列。
7. ready backpressure 下当前 entry 不改变，且每个 slot 恰好 fire 一次。

## 6. 最终结论

独立参数加两阶段 schedule 是满足 STD 内部乱序的最小完整方案。它不扩大 atomic 并发度或物理端口范围，也不会改变 AtomicsUnit 的 slot 协议。v1.4 无剩余方案级 blocker；实现完成后需进行编译、参数组合 smoke 和 backpressure 定向验证。
