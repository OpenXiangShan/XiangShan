# 问题2后续：RM FP Load NaN-boxing mismatch 分析

日期：2026-10-15

## 结论

r2 的 9 条 `Load data/data_valid mismatch` 属于 RM 期望值建模缺失，不是 DUT 数据错误，也不是 Uncache retry starvation。

9 笔请求全部满足：

- `op_class=2`，即 `MEMBLOCK_OP_CLASS_FP_LOAD`；
- `fu_op=1`（LH，7 笔）或 `fu_op=2`（LW，2 笔）；
- 低 16/32 位 expected 与 DUT 完全一致；
- DUT 仅把高位填 1：LH 为 `0xffffffffffff_xxxx`，LW 为 `0xffffffff_xxxxxxxx`。

这正是 DUT `rdataHelper()` 在 `uop.fpWen=1` 时对窄浮点 load 执行的 NaN-boxing。RM 当前只依据 `fuOpType` 把 LH/LW 当成整数有符号 load，没有使用主表已经稳定保存的 `op_class=FP_LOAD`，所以错误地生成普通 sign-extension expected。

## r2 证据

日志：

```text
mem_ut/ver/ut/memblock/sim/fault2_uncache_retry_boost_real_seed37617171_20261015_r2/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_uncache_retry_single_winner_real_seed37617171_r2.log
```

| UID | op_class | fu_op | RM expected | DUT actual |
| ---: | ---: | ---: | --- | --- |
| 867 | 2 | 1 | `0x0000000000007ce7` | `0xffffffffffff7ce7` |
| 959 | 2 | 1 | `0x000000000000124e` | `0xffffffffffff124e` |
| 1111 | 2 | 1 | `0x00000000000060b3` | `0xffffffffffff60b3` |
| 1625 | 2 | 1 | `0x0000000000004f97` | `0xffffffffffff4f97` |
| 1762 | 2 | 1 | `0x00000000000037f5` | `0xffffffffffff37f5` |
| 1858 | 2 | 1 | `0x0000000000003739` | `0xffffffffffff3739` |
| 1945 | 2 | 1 | `0x0000000000006ffe` | `0xffffffffffff6ffe` |
| 3482 | 2 | 2 | `0x00000000531a5309` | `0xffffffff531a5309` |
| 3570 | 2 | 2 | `0x00000000671c0f03` | `0xffffffff671c0f03` |

所有请求均完成 normal pass、ROB commit 和 terminal/retire。错误发生在 RM 最终数据比较，不影响 scheduler 前进。

## 代码闭环

DUT：`src/main/scala/xiangshan/mem/lsqueue/LoadQueue.scala` 的 `rdataHelper()`：

```text
LH + fpWen -> FPU.box(..., FPU.H) -> 高 48 位全 1
LW + fpWen -> FPU.box(..., FPU.S) -> 高 32 位全 1
LD + fpWen -> FPU.box(..., FPU.D) -> 64 位原值
```

测试框架：`issue_field_assigner.sv::derive_wen()` 已按 `main_tr.op_class` 正确生成：

```text
INT_LOAD -> rfWen=1, fpWen=0
FP_LOAD  -> rfWen=0, fpWen=1
```

RM：`memblock_rm.sv::observer_build_commit_item()` 当前只设置：

```text
item.is_signed = fu_op is LB/LH/LW/LD
```

随后 `rm_ls_core.sv::build_load_expected()` 仅做整数 sign-extension。虽然 `main_transaction_view_t` 已包含 `op_class`，RM item 没有保存 `fp_wen/is_fp_load`，因此丢失了 NaN-boxing 语义。

## 建议修改

最小且规范的修改：

1. 在 `rm_ls_program_item_t` 增加稳定字段 `fp_wen` 或 `is_fp_load`。
2. 在 `observer_build_commit_item()` 中从 `main_view.op_class == MEMBLOCK_OP_CLASS_FP_LOAD` 赋值；不要从 DUT actual data 或 actual `fpWen` 反推 expected。
3. 在 `build_load_expected()` 完成逐 byte 数据拼接后，先处理 FP NaN-boxing，再处理普通整数 sign-extension：

```text
FP LH: upper[63:16] = all 1
FP LW: upper[63:32] = all 1
FP LD: 保持完整 64 位
非 FP signed load: 保持现有 sign-extension
非 FP unsigned load: 保持现有 zero-extension
```

4. 可把写回 monitor 已采到的 `uop_fpWen` 作为 identity/diagnostic 一致性检查，但不能用它构造 expected。
5. 日志补充 `op_class/fp_wen/size/is_signed`，让 NaN-boxing 与整数扩展可直接区分。

## 专项验收

directed 至少覆盖：

- FP LH：payload bit15 为 0 和 1，expected 高 48 位始终全 1；
- FP LW：payload bit31 为 0 和 1，expected 高 32 位始终全 1；
- FP LD：64 位数据不额外改变；
- INT LH/LW：继续按符号位扩展；
- LHU/LWU：继续零扩展；
- exception load：保持 data incomparable，不因 NaN-boxing 新增比较。

修复后使用新的唯一 mode 重跑 `seed=37617171`，不能复用或重启 r2 mode。预期 9 条 data mismatch 消失；UID98 的 `expected exception=0x2000, DUT=0` 是独立 translation/PMA expectation 问题，需单独修复后才能完成 10000 事务的最终全通过验收。
