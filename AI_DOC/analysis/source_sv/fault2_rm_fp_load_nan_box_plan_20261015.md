# 问题2后续：RM FP Load NaN-box expected-data修复方案

日期：2026-10-15

## 1. 现象与归因

主阻塞R2真实seed `37617171`自然结束于`1125011.900ns`，共有10个UVM error：
UID98是既有独立translation exception provenance问题，另9个均为Load data mismatch。

| UID | op_class | fu_op | RM expected | DUT actual | Uncache D-error |
| ---: | ---: | ---: | --- | --- | --- |
| 867 | FP_LOAD | LH | `0x0000000000007ce7` | `0xffffffffffff7ce7` | clean |
| 959 | FP_LOAD | LH | `0x000000000000124e` | `0xffffffffffff124e` | clean |
| 1111 | FP_LOAD | LH | `0x00000000000060b3` | `0xffffffffffff60b3` | clean |
| 1625 | FP_LOAD | LH | `0x0000000000004f97` | `0xffffffffffff4f97` | clean |
| 1762 | FP_LOAD | LH | `0x00000000000037f5` | `0xffffffffffff37f5` | clean |
| 1858 | FP_LOAD | LH | `0x0000000000003739` | `0xffffffffffff3739` | clean |
| 1945 | FP_LOAD | LH | `0x0000000000006ffe` | `0xffffffffffff6ffe` | clean |
| 3482 | FP_LOAD | LW | `0x00000000531a5309` | `0xffffffff531a5309` | clean |
| 3570 | FP_LOAD | LW | `0x00000000671c0f03` | `0xffffffff671c0f03` | clean |

全部9笔均满足：`translation_path=PAGED_ENTRY`、`path=UNCACHE`、ledger按完整
UID/epoch/ROB/LQ命中、`A.sample <= D.sample < load WB sample`、`denied=0`、
`corrupt=0`、expected/status/actual exception均为0。低16位或低32位完全一致，
仅高位扩展不同，因此与owner/source/generation、redirect迟到和D-error异常映射无关。

## 2. 根因

XiangShan的整数LH/LW与FLH/FLW共用`fuOpType`，由`uop.fpWen`区分结果格式：

```text
LH + fpWen=0 -> 符号扩展16位
LH + fpWen=1 -> 高48位全1的NaN-box
LW + fpWen=0 -> 符号扩展32位
LW + fpWen=1 -> 高32位全1的NaN-box
LD + fpWen=1 -> 原64位数据
```

测试框架`issue_field_assigner`稳定规定`MEMBLOCK_OP_CLASS_FP_LOAD -> fpWen=1`。
RM当前只冻结`fu_op/size/is_signed`，`build_load_expected()`把FP LH/LW当作整数
signed load，因而当低位符号位为0时错误地产生零扩展高位。

## 3. 修复

1. `rm_ls_program_item_t`新增`fp_wen`，从主表`op_class == FP_LOAD`冻结；禁止从
   DUT写回的`fpWen`或actual data反推expected。
2. `build_load_expected()`镜像DUT架构语义：FP LH高48位全1，FP LW高32位全1，
   FP LD不变；其余整数load继续走原符号/零扩展。
3. `RM_LS_TRACE_MAIN`打印`fp_wen`，便于区分共享fuOp的整数/FP load。
4. RM directed增加FP LH、FP LW、FP LD及整数LH/LW对照，防止把所有LH/LW
   无条件补1。

## 4. Review矩阵

| 场景 | 期望 |
| --- | --- |
| FP LH，bit15=0/1 | 高48位始终全1 |
| INT LH，bit15=0/1 | 正常符号扩展 |
| FP LW，bit31=0/1 | 高32位始终全1 |
| INT LW，bit31=0/1 | 正常符号扩展 |
| FP LD | 64位原值 |
| LBU/LHU/LWU | 原零扩展不变 |
| 任意异常Load | 不比较data，异常流程不变 |
| Uncache denied/corrupt | ledger只修改exception，不参与数据扩展 |

Review结论：修复只改变`FP_LOAD + LH/LW + 无异常`的expected data，高位行为与
DUT `FPU.box`一致；不放宽compare，不依赖DUT actual控制，也不修改ledger生命周期。

## 5. 验证计划

1. `git diff --check`和唯一mode编译。
2. 扩展后的RM directed自然结束，要求`UVM_ERROR=0/UVM_FATAL=0`。
3. 原始cfg、seed `37617171`使用新唯一mode完整自然重跑；确认上述9个UID不再
   产生data mismatch，记录UID98及任何新错误，禁止手动中止。
4. 现有RM c和旧r1保持运行，不clean、不删除输出。

## 6. 最终验证结果

唯一mode `fault2_rm_fp_nanbox_uid98_20261015_c`编译成功。Ledger/NaN-box directed日志：

```text
mem_ut/ver/ut/memblock/sim/fault2_rm_fp_nanbox_uid98_20261015_c/log/
tc=basicTest_ts=memblock_rm_uncache_d_error_ledger_directed_vseq_cfg=tc_dispatch_real_smoke_seed=888888_rtl_rm_ledger_nanbox_directed_20261015_c.log
```

directed覆盖FP LH/LW/LD、整数LH/LW和异常Load对照，自然结束于`200.100ns`，
`TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`。

真实seed日志：

```text
mem_ut/ver/ut/memblock/sim/fault2_rm_fp_nanbox_uid98_20261015_c/log/
tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_rm_uid98_overlap_real_seed37617171_20261015_c.log
```

原9个失败UID全部PASS：867、959、1111、1625、1762、1858、1945的FP LH高48位
均为1；3482、3570的FP LW高32位均为1。UID3570最终expected与DUT均为
`0xffffffff671c0f03`。完整真实seed自然结束于`3282013.400ns`，
`RM_LS_TRACE_COMPARE=10000`、`Quit count=0/10`、`UVM_ERROR=0`、`UVM_FATAL=0`。

自review确认：格式化helper由RM core和directed共用，没有复制第二套算法；`fp_wen`
只来自主表`op_class`，不读取DUT写回data/fpWen来构造expected。整数符号扩展、无符号
零扩展、FP LD和异常Load行为保持不变。
