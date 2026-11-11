# MemBlock AMO LR/SC 主表生成方案第八轮 Review

## 1. Review 信息

- Review 日期：2026-10-27
- Review 对象：`memblock_amo_stimulus_only_adaptation_plan_20261022.md` v1.7/v1.8
- 新增需求：AMO 使用最高发送优先级；LR 可按参数预约未来 SC；独立 SC 必须继续保留
- 最终结论：使用建表 sequence 局部预约表即可功能完整实现，不需要扩展主表长度、增加运行期配对状态或引入 RM/checker

## 2. 最终生成模型

SC 有两条并存的生成路径：

```text
独立 SC：继续由 SC.W/SC.D opcode 权重正常随机生成
伴随 SC：随机到 LR 后，按 pair 概率预约未来 UID 并强制生成同宽度 SC
```

伴随 SC 不依赖独立 SC opcode 权重。因此：

- SC opcode 权重非零时，独立 SC 和伴随 SC 都可以出现；
- SC opcode 权重为零时，独立 SC 关闭，但 LR 仍可生成伴随 SC；
- LR.W 只配 SC.W，LR.D 只配 SC.D。

## 3. UID 与距离语义

距离统一表示 LR 与 SC 中间隔着多少个 UID：

```text
sc_uid = lr_uid + dist + 1
```

示例：

```text
dist=0：UID10=LR，UID11=SC
dist=3：UID10=LR，UID11/12/13正常生成，UID14=SC
```

伴随 SC 使用未来已有 UID 槽位，不调用额外 `alloc_uid()`，不扩展 `main_trans_num`，不移动其他事务。

## 4. 参数 Review

新增参数：

```text
MEMBLOCK_AMO_LR_SC_PAIR_EN_1_WT=80
MEMBLOCK_AMO_LR_SC_SAME_ADDR_1_WT=90
MEMBLOCK_AMO_LR_SC_DIST_FIXED_WT=0
MEMBLOCK_AMO_LR_SC_DIST_FIXED_VALUE=0
MEMBLOCK_AMO_LR_SC_DIST_0_WT=1
MEMBLOCK_AMO_LR_SC_DIST_1_5_WT=1
MEMBLOCK_AMO_LR_SC_DIST_6_10_WT=1
MEMBLOCK_AMO_LR_SC_DIST_11_100_WT=1
```

Review 结论：

1. pair 和 same-address 使用单一 `_1_WT` 百分比参数即可，未命中概率自动为 `100-value`，无需增加 `_0_WT`。
2. 固定距离档与四个区间档共同参加加权随机。
3. `DIST_FIXED_WT=0` 且其余四档均为 1 时，四档各占 25%。
4. `DIST_FIXED_VALUE` 合法范围为 0..100，默认 0。
5. 五个距离权重同时为 0 时必须在 cfg load 阶段 fatal。

## 5. 主表预约与控制槽位

最小实现是在 `build_random_main_table()` 和 `build_control_auto_main_table()` 内维护局部 `sc_reservation_by_uid`，记录目标 UID、SC 类型、LR 地址来源、同地址选择和距离。

AUTO 模式生成优先级：

```text
CSR control > SFence control > paired SC reservation > 普通随机 transaction
```

末尾固定 `check_store` 不进入可预约范围。多条 LR 不能预约同一 UID。若距离目标越界或冲突，则重新抽取仍合法目标；有限重试后没有合法目标时允许放弃本次配对并记录日志。

控制预约可能在后续命中后再次重排，因而新目标可能与更早的 SC 预约相撞。此时控制事务优先，必须删除对应 SC 预约并记录取消原因，不能覆盖 CSR/SFence。

## 6. 地址一致性

同地址抽样发生在 SC 预约成功时，并只执行一次。命中同地址时不能只复制 `vaddr`，因为现有事务校验要求：

```text
vaddr == src_0 + sign_extend_imm12(imm)
```

因此最小正确做法是复制 LR 最终的 `src_0`、`imm` 和等价地址属性，再调用 `update_vaddr()`，最后断言 SC 与 LR 的 `vaddr` 相同。若 LR 地址非对齐，paired SC 同样保持该非对齐地址。

未命中同地址时，SC 独立生成合法 W/D 地址。LR 与 SC 之间的普通访存可以破坏 reservation，这是预期的测试覆盖，不应被建表逻辑禁止。

## 7. Priority Review

当前 `send_pri/send_pri_std` 合法范围为 0..100，调度器在 priority 模式开启时数值越大优先级越高。因此 random/AUTO 建表生成 AMO 后统一执行：

```text
send_pri     = 100
send_pri_std = 100
```

Review 发现原始表述“所有 AMO 强制 100”可能误覆盖 manual 主表的用户显式配置。v1.8 已修正为：

- random/AUTO 生成的独立 AMO、独立 SC 和 paired SC 都固定为 100；
- manual 主表保持用户显式 priority；
- `MEMBLOCK_SEND_PRI_MODE_EN=0` 时字段仍可为 100，但现有调度器不比较 priority，不应要求优先仲裁效果。

## 8. 风险检查

| 风险 | 处理 |
| --- | --- |
| `dist=0` 被实现为相同 UID | 固定使用 `sc_uid=lr_uid+dist+1` 并断言目标更年轻 |
| SC 预约被普通随机覆盖 | 当前 UID 先查预约表，命中后跳过普通 opcode 随机 |
| 多 LR 预约同一 UID | 写入预约表前检查占用，冲突重选或放弃 |
| 覆盖 CSR/SFence/check_store | 控制槽位优先，冲突时取消 SC 预约 |
| 同地址字段不一致 | 复制 `src_0/imm` 后重算并校验 `vaddr` |
| paired SC 错误依赖 SC opcode 权重 | 预约路径直接指定 SC.W/SC.D |
| 只覆盖 `send_pri` | 同时覆盖 `send_pri_std` 并增加验收检查 |
| manual 主表 priority 被破坏 | priority 覆盖仅用于 random/AUTO 建表 |
| 表尾导致配对率不足 | 无合法未来 UID 时允许放弃并统计，不扩表 |

## 9. 验收结论

v1.8 已增加以下定向验收：

1. pair 概率 0/100；
2. same-address 概率 0/100；
3. fixed、0、1..5、6..10、11..100 五类距离；
4. `dist=0` 紧邻 UID；
5. W/D 类型严格匹配；
6. 主表长度和后续 UID 不变化；
7. 独立 SC 保留，且 paired SC 不依赖独立 SC 权重；
8. 不覆盖 CSR、SFence 和 `check_store`；
9. 表尾无合法目标时安全放弃；
10. random/AUTO AMO 两类 priority 都为 100，manual 主表不被覆盖。

## 10. 最终结论

本轮需求可以通过“参数读取 + 建表局部 SC 预约 + AMO priority 覆盖”完成。该方案保留独立 SC，完整支持 LR 伴随 SC 的概率、距离和地址关系，同时不修改主表容量模型、不增加运行期 LR/SC 配对状态，也不引入结果 checker，符合功能完整前提下的最小改动目标。
