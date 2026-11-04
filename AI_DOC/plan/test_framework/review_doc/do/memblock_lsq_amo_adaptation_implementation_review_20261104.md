# MemBlock LSQ Admission 与 AMO 激励适配 Implementation Review

状态：`已完成 review，可归档`

日期：2026-11-04

关联计划：

- `AI_DOC/plan/test_framework/plan/do/memblock_lsq_rtl_equivalent_admission_formal_plan_20261031.md`
- `AI_DOC/plan/test_framework/plan/do/memblock_amo_stimulus_only_adaptation_plan_20261022.md`

实现分支：`codex/memblock-amo-adaptation-20261103`

## 1. Review 结论

```text
Blocker: 0
Major:   0
Minor:   0
```

实现具备进入扩展回归阶段的条件。review 未发现已确认的功能或生命周期问题；残余项均为尚未执行的专项测试覆盖，不是本轮已复现缺陷。

## 2. 审查范围

本轮重点检查：

- LSQ D/R/Q、registered credit/canAccept、redirect t1/t2/t3 和 cancel/deq 回补。
- source offer、driver launch、DUT sample、pending reservation 和 reset 撤销边界。
- 单 atomic owner/barrier、non-LSQ admission、STA/STD/CAS.Q 多片发送和 held-piece 稳定性。
- atomic feedback、LDA0 final WB、normal/fault terminal、redirect recovery 和 `fault_uid+1`。
- plus 参数默认兼容性和禁止修改范围。
- 编译、定向 soft test、普通 smoke、AMO smoke 及 global-stop 收口。

未修改 RTL/Scala、RM、scoreboard、checker、coverage/fcov。

## 3. 关键检查结论

### 3.1 LSQ admission

- D/R/Q 固定 lane、partial 原位保持和 registered credit/canAccept 更新由 `lsq_ctrl_model` 统一维护。
- raw deq/cancel 只更新硬件 shadow；软件 reservation 由原业务 owner 路径释放，未发现双回补。
- reset 会撤销尚未 DUT-visible 的 pending reservation，并同步回滚 admission 前缀。
- global-stop 统一等待 shadow D/R/Q、frozen offer、pending sample、redirect/recovery 和 atomic 在途状态排空。

### 3.2 Atomic admission 与 issue

- AMO 等待 older UID terminal，经过共享 D/R/Q 和旧 registered canAccept 后才建立 non-LSQ owner。
- AMO admission 不创建 LQ/SQ owner或 reservation，不改变 LQ/SQ pointer/count，并预置 `lsq_deq=1`。
- owner 建立后 barrier 阻止 younger 普通事务和第二条 AMO；同一 owner 的 CAS 多片仍可继续发送。
- CAS.Q 使用 STA slot 0/2、STD slot 0..3；STA2 晚于 STA0。所有片固定物理 pipe0、每拍最多一片。
- STD shuffle 只从 remaining mask 选择；held piece 在 ready 低期间保持 target/slot/payload/epoch 不变。

### 3.3 Feedback、fault 与 reset

- atomic STA feedback 在普通 SQ-owner 校验前分流；LDA0 atomic final WB 在普通 LOAD/LQ attach 前分流。
- normal completion、SC/CAS 正常失败和架构 fault 使用不同收口条件。
- fault atomic 只允许 owner 自身 fault redirect；公共 recovery 不等待 atomic barrier，避免互相等待。
- fault terminal 后从 `fault_uid+1` 继续；物理 reset、pre-fire redirect 和正常 terminal 均有明确 barrier 释放路径。

### 3.4 默认兼容性

- `MEMBLOCK_OP_CLASS_AMO_WT` 默认仍为 0，原有测试默认不生成 AMO。
- CAS 权重默认 0；新增 order、shuffle、misalign 和 LR/SC 参数均有范围校验。
- 普通 real-dispatch smoke 通过，未观察到默认路径回归。

## 4. Review 中已收口的问题

1. source offer 曾在 driver 尚未真实 launch 时提前发布，现改为以真实 launch 为唯一锚点。
2. reset 曾只回滚 pending LQ/SQ pointer/owner，遗漏顺序 admission 前缀；现同时调用 `rollback_max_enqueued_uid(uid)`。
3. soft test 曾在 atomic non-LSQ admission 后重复调用 `mark_uid_enqueued(0)`；现删除重复标记。
4. AMO smoke 曾因 U-mode `PTE.U` 和 routed PPN/PMA 范围产生 PF/AF；现固定合法用户页权限和支持 atomic 的主存物理范围。

## 5. 验证结果

验证目录：

```text
mem_ut/ver/ut/memblock/sim/amo_plan_verify13_20261104/log/
```

| 项目 | 结果 |
|---|---|
| 完整 VCS/KDB 编译 | 0 error；Verdi KDB 成功生成 |
| `tc_lsq_amo_admission_soft` | PASS；`UVM_ERROR=0`、`UVM_FATAL=0` |
| 普通 real-dispatch smoke | TEST_PASS；`UVM_ERROR=0`、`UVM_FATAL=0` |
| AMO real-dispatch smoke | TEST_PASS；`UVM_ERROR=0`、`UVM_FATAL=0` |
| `git diff --check` | PASS |

AMO smoke 观察到 LR/SC pair、7 个 atomic owner 完成 STA feedback/final WB，global-stop 在 quiet window 后正常提交。

## 6. 残余测试缺口

- CAS.Q 两次 final WB 当前主要由模型和 soft test 检查；通过的 AMO smoke 未实际出现 `count=1/2`、`count=2/2`。
- AMO fault redirect、公共 recovery 和 `fault_uid+1` 尚缺干净的 DUT 端到端通过用例。
- LR/SC 尚未系统覆盖五档距离、同/异地址、独立 SC 和预约冲突。
- 非对齐 AMO、三种 order mode、STD shuffle 多 seed、CAS.Q STA2 backpressure 尚未系统回归。
- reset/redirect/global-stop 与 D/R/Q、held piece、部分已 fire owner 的交叉当前主要依赖 soft test。

这些缺口已保留在 AMO plan 的 P4，不影响当前 coding 和基础验证完成结论，也不能被当前三项 PASS 视为已覆盖。
