# MemBlock AMO STD 逐片动态随机方案第五轮 Review

## 1. Review 信息

- Review 日期：2026-10-27
- Review 对象：`memblock_amo_stimulus_only_adaptation_plan_20261022.md` v1.4/v1.5
- 新增需求：每次准备发射下一片 STD 时重新随机，而不是 owner 建立时一次生成完整顺序
- Review 边界：只修改纯激励发送方案，不增加 checker、RM、多 owner、动态物理端口或同拍多发射
- 最终结论：使用 remaining mask 逐片选择，并用 held piece 跨反压锁存后，方案功能完整且没有计划级 blocker

## 2. 主要发现

### R5-1：预生成完整排列不满足新需求

v1.4 在 owner 建立时生成一次 `std_slot_order[]`，后续只按该数组推进。它能够覆盖不同排列，但后续每片的选择已经被提前决定，不符合“每次发下一片前重新随机”的要求。

修正：删除当前方案中的完整 STD 排列。每次需要新的 STD 片时，重新计算：

```text
remaining_std_mask = required_std_slot_mask & ~fired_std_slot_mask
```

然后只从该集合中选择一项。

### R5-2：随机单位是“新 STD 片”，不是“等待 fire 的每一拍”

如果 `ready=0` 时每拍重新随机，驱动器可能在 `valid=1` 期间改变 slot、payload 或 issue epoch，违反 Decoupled 握手稳定性。

正确行为：

1. 当前没有 held piece，并且调度器决定下一目标为 STD。
2. 从当前 remaining mask 随机选择一个 slot。
3. 将 target、slot、payload 和 issue epoch 锁存为 held piece。
4. 在 `ready=0` 的所有周期保持不变。
5. `valid && ready` fire 后更新 fired mask 并清除 held piece。
6. 下一次需要 STD 时再做一次新的随机选择。

### R5-3：最终仍能形成全部合法排列

CAS.W/D 首次从 `{0,1}` 中选择，第二次只剩另一项，因此最终可能为 `[0,1]` 或 `[1,0]`。

CAS.Q 每次从不断缩小的 remaining set 中选择，因此最终可以形成全部 24 种 permutation，包括原始升序 `[0,1,2,3]`。开关打开不代表必须得到非升序结果。

示例：

```text
remaining={0,1,2,3} -> 选择2 -> STD2 fire
remaining={0,1,3}   -> 选择0 -> STD0 fire
remaining={1,3}     -> 选择3 -> STD3 fire
remaining={1}       -> 选择1 -> STD1 fire
```

### R5-4：RANDOM_LEGAL 需要两级选择，不需要完整 schedule

`MEMBLOCK_AMO_ISSUE_ORDER_MODE=2` 时：

1. 先在当前合法 STA head 和“存在 remaining STD”之间选择本次 target。
2. 如果选择 STA，发送当前合法 STA slot。
3. 如果选择 STD，再按 `MEMBLOCK_AMO_STD_SHUFFLE_EN` 选择具体 remaining slot。

这样 target 交错随机和 STD 内部随机彼此独立，不需要固化 `std_slot_order[]`，也不需要预生成完整 STA/STD schedule。

### R5-5：required/fired bitmap 是完整性约束

动态选择必须同时满足：

- 只能选择 required 且尚未 fired 的 STD slot；
- held piece fire 前不能选择下一片；
- fire 后只删除当前 exact slot；
- 最终 `fired_std_slot_mask == required_std_slot_mask`；
- 不得重复、遗漏或跨 owner 选择。

## 3. 参数语义

```text
MEMBLOCK_AMO_STD_SHUFFLE_EN=0
  每次选择编号最小的 remaining STD slot。

MEMBLOCK_AMO_STD_SHUFFLE_EN=1
  每次准备发送新的 STD 片时，从当前 remaining STD slots 随机选择一项。
```

`MEMBLOCK_AMO_ISSUE_ORDER_MODE` 仍只决定下一片的 target 类别和必要偏序：

- `STD_FIRST`：remaining STD 非空时持续选择 STD，全部完成后发送 STA。
- `STA_FIRST`：先按合法顺序发送 STA，再逐片选择 STD；LR 仍强制 `STD0 < STA0`。
- `RANDOM_LEGAL`：动态选择 STA 或 STD；CAS.Q 仍强制 `STA0 < STA2`。

## 4. 最小状态

owner 只需要：

```text
required/fired STA masks
required/fired STD masks
STA合法顺序状态
held piece valid + target + slot + payload + issue epoch
可选的issued STD history，仅用于日志和验证
```

不再需要：

```text
std_slot_order[]
完整issue_schedule[]
Fisher-Yates预生成排列
```

所有 STA/STD 仍固定通过物理 pipe 0 分拍发送，每拍最多一片。

## 5. 验证要求

1. shuffle=0 时，每次选择都是 remaining mask 的最低 set bit。
2. shuffle=1 的多 seed 回归中，CAS.W/D 同时出现 `[0,1]` 和 `[1,0]`。
3. shuffle=1 的多 seed 回归中，CAS.Q 同时观察到升序和非升序结果。
4. 每次选择记录选择前 remaining mask、selected slot、held piece、fire 和选择后 remaining mask。
5. `ready=0` 时 held target/slot/payload/epoch 保持稳定，期间不产生新的随机选择。
6. 前一片 fire 后，下一片选择必须基于更新后的 remaining mask 再执行一次随机。
7. 任意模式下每个 required slot 恰好 fire 一次，最终 fired mask 与 required mask 相等。
8. RANDOM_LEGAL 中分别验证 target 选择和 STD slot 选择，不将两者合并为预生成 schedule。

## 6. 最终结论

逐片 remaining-mask 选择满足“每次发射下一片之前重新随机”的新要求，同时保持 Decoupled 接口在反压期间稳定。该修订不增加物理端口、不增加 atomic 并发度，也不需要 checker/RM 改动。v1.5 无剩余方案级 blocker，可以进入 coding。
