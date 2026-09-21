# 最新 `kunminghu-v2` RTL 历史缺陷复测总结

## 结论

工作分支已执行 `git fetch origin kunminghu-v2`、`git rebase FETCH_HEAD`、`make init`，并在
commit `6646f8f53c8e37791fbbf5b854427a452292d17a` 生成 V2 整核 Verilog。测试框架继续通过
`$MEMBLOCK_XS_HOME/build/rtl/filelist.f` 使用当前 worktree 的 `build/rtl`；生成后顶层端口与
`dut_inst.sv` 的集合比对无差异。

本轮四项历史 RTL 问题的动态结论如下：

| 问题 | 最新动态结果 | 结论 |
| --- | --- | --- |
| cacheable cross-16B fault store 无法 drain | UID517/SQ1/49 再次卡死 | 未解除，RTL bug 仍存在 |
| NC cross-16B `rdataPtrExt` 双推进 | 同入口先卡在 UID517，未进入原触发组合 | 不能判定动态修复 |
| SBuffer forwarding 导致 LDA2 X | 已越过旧首错 `690.6ns`，运行至约 `305us` 无 X/Z fatal | 旧首错点未复现；因未完成 1 万笔，不宣称完全修复 |
| MAB 到 STA0 的 trigger 元数据 | `727.8ns` 同样 fatal | 未解除，RTL bug 仍存在 |

本任务没有修改 RTL、UVM 测试框架或 RM。

## 基线与生成记录

执行顺序：

```bash
git fetch origin kunminghu-v2
git rebase FETCH_HEAD
make init
scripts/generate_memblock_rtl.sh
```

生成产物：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/build/rtl/MemBlock.sv
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/build/rtl/filelist.f
```

VCS 编译在三个专项 mode 均完成，且没有 DUT 顶层端口、层级或 agent interface 编译错误。

## 复测一：cacheable 跨 16B fault store

命令参数：

```text
tc=basicTest
ts=memblock_dispatch_real_smoke_vseq
cfg=tc_dispatch_real_mmu_sv39_smoke
seed=666666
+MEMBLOCK_MAIN_TRANS_NUM=10000
mode=kunminghu_v2_latest_20260920_smoke
```

UID517（`ROB=1/150`、`SQ=1/49`）仍是 cacheable 的跨 16B scalar store。其 fault 后框架发出
level-1、`flush_itself=1` redirect；DUT cancel snapshot 不包含该已 committed 的 SQ，因此框架正确等待
真实 `sqDeq`。波形显示：

```text
128840.3ns  unaligned_49=1, cross16Byte_49=1, addrvalid_49=1
128845.3ns  hasException_49=1
128850.3ns  committed_49=1
之后         datavalid_49=0, completed_49=0, rdataPtrExt_0=deqPtrExt_0=1/49,
             DataBuffer enq valid=0, io_sqDeq=0
```

最新源码中的 cross-16B priority 分支仍把 scalar 条件写成 `allvalid`，没有允许
`hasException` 绕过缺失的 STD data；该 entry 同时失去 cancel 与 exception-drain 出路。独立 review
确认这是 RTL bug，详细记录已更新至：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/buglist/rtl/v2/rtl_buglist_2026-W38_20260914.md`。

日志与波形：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/kunminghu_v2_latest_20260920_smoke/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/kunminghu_v2_latest_20260920_smoke/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.fsdb
```

## 复测二：NC 跨 16B `rdataPtrExt`

最新 `StoreQueue.scala` 已包含 `dataBufferReadGo` 对 `enq.bits.sqPtr === rdataPtrExt(i)` 的匹配，
用于避免 split store 的重复 `rdataPtrExt` 推进。但本次动态 smoke 在到达可观察 NC 原始触发组合前，已被
前述独立的 UID517 cacheable fault-store deadlock 截断。因此本轮只能记录源码包含上游修改，不能以未看到
`+2` 跳步为由判定 RTL bug 已修复。

## 复测三：SBuffer X 传播

命令参数：

```text
tc=basicTest
ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq
cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k
seed=710006
+MEMBLOCK_MAIN_TRANS_NUM=10000
+MEMBLOCK_HARD_XZ_CHECK_EN=1
+MEMBLOCK_CHECK_TRIGGER_EN=0
mode=kunminghu_v2_latest_20260920_sbuffer_xprop
```

历史版本在 `690.6ns` 就产生 `LDA2 valid is X/Z`。本轮同 seed 已运行到约 `305us`，日志未出现
`LDA2`、`writeback valid is X/Z`、`UVM_FATAL` 或 `UVM_ERROR`。这证明旧首错点没有复现；但为了转向下一项
严格 trigger 专项，该 1 万笔 run 被人工终止，未得到 `TEST CASE PASSED`，故结论是“旧首错点未复现”，
而不是“全量验证已修复”。

日志：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/kunminghu_v2_latest_20260920_sbuffer_xprop/log/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl.log
```

## 复测四：跨 16B Store 的 STA0 trigger 元数据

命令参数：

```text
tc=basicTest
ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq
cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k
seed=666666
+MEMBLOCK_MAIN_TRANS_NUM=100000
+MEMBLOCK_CHECK_TRIGGER_EN=1
mode=kunminghu_v2_latest_20260920_trigger
```

最新 RTL 在 `727.8ns` 再次触发：

```text
UVM_FATAL [INT_WB_STA0_TRIGGER_PROVENANCE]
STA0 trigger=0 without breakpoint needs uncache/CBO provenance
```

这与旧波形同一 seed、同一时间一致，说明 MAB 仍保存早期 `s1_in.uop.trigger=0` 并把它直接用于
parent final writeback，而非使用经过 `MemTrigger` 计算的 `s1_out`。该问题已更新到本周 RTL buglist。

日志与波形：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/kunminghu_v2_latest_20260920_trigger/log/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=666666_rtl.log
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/kunminghu_v2_latest_20260920_trigger/wave/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=666666_rtl.fsdb
```
