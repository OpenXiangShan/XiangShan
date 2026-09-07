# 严格 PBMT=00/non-NC 10k 回归中 DCache E 通道 X 传播 RTL 问题补充诊断

## 结论

在严格 `PBMT=00/non-NC` 的 seed `710006` 中，关闭硬 X/Z 检查后进行的诊断性重跑没有
通过。`745.6ns` 报出的

```text
E.valid observed without a pending GrantAck owner
```

不是 RM compare 失败，也不是 DCache responder 漏建 GrantAck owner。独立 review 已确认：

1. 在该报错之前，DUT 的 LoadUnit 2 到 DCache miss 请求控制链已经出现 X；
2. DCache E 通道的 `valid` 与 `sink` 是 DUT 输出，测试框架只驱动 `E.ready`，因此 TB 不能向
   这两个 DUT 输出注入 X；
3. `735.6ns` 的 `E.valid=1, E.sink=00x` 是协议上不可用的 GrantAck payload；
4. `745.6ns` 的 UVM_FATAL 是上述 DUT X 传播被 responder 账本检测到后的次生表现。

因此本问题按 RTL X-propagation 分类。尚未证明它与
[SBuffer forwarding X 传播问题](memblock_pbmt0_non_nc_10k_sbuffer_forward_xprop_rtl_failure_20260906.md)
具有同一个最初根因；二者只能称为同一 seed 中可能相关的 DUT X 症状，不能合并归因。

本任务未修改 RTL、RM 或测试框架；发现 RTL 问题并经独立 review 确认后，不继续后续 seed。

## 诊断场景

```text
tc=basicTest
ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq
cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k
mode=pbmt0_non_nc_10k_multiseed_20260906
seed=710006

+MEMBLOCK_MAIN_TRANS_NUM=10000
+MEMBLOCK_CHECK_TRIGGER_EN=0
+MEMBLOCK_HARD_XZ_CHECK_EN=0
```

虽然 cfg 文件名含 `100k`，本次命令行覆盖后的实际目标是 10,000 笔。PBMT、内存和操作类型
约束不变：`mPBMTE=0`、`hPBMTE=0`、S1/S2 PBMT 固定 `00`，地址处于
`0x8000_0000..0x9000_0000` 的可缓存正常内存；只生成 scalar integer Load/Store，允许普通
非对齐访问，不生成 NC/MMIO、AMO、CBO、进程切换、CSR 或 SFENCE。

关闭硬 X/Z 检查并不是为了把本用例判为通过，只是让仿真越过原先的 LDA2 X 报错，以确认 X
继续传播后产生的真实协议后果。

## 时序证据

下表以关闭硬 X/Z 检查后的诊断 FSDB 为准。`700.6ns` 是本次 DCache/E 锥内最早已确认的
异常点，不是对整个设计最早 X 的声明；原始硬检查 run 在更早的 `690.6ns` 已报告 LDA2
writeback valid 为 X。

| 时间 | 波形观察 | 判断 |
| --- | --- | --- |
| `700.6ns` | `top_tb.U_MEMBLOCK.io_ooo_to_mem_issueLda_2_ready` 变为 X；TB 驱动的 LDA2 `valid` 与 payload 保持确定值。 | X 已到达 DUT 顶层输出方向，不能归因于 issue driver 的输入。 |
| `710.6ns` | `inner_dcache.dcache.ldu_2.io_miss_req_valid`、`missReqArb.io_in_3_valid`、`missReqArb.io_out_valid` 和 `missQueue.entries_4.io_req_valid` 变为 X。 | X 已进入 DCache 的 load-miss 控制与 MissQueue 入队路径，发生在 E fatal 之前。 |
| `720.6ns` / `725.6ns` | TB 仍以确定的 D response 驱动 `GrantData`，`source=4`、`sink=0`，且 D handshake 正常。 | 这笔 GrantData 不是 TB X 注入。 |
| `730.6ns` | responder 已为 `sink=0` 建立 GrantAck owner；`entries_4.io_mem_finish_valid=1`、候选 `sink=0`，但 entry 内部 finish 仲裁/状态已有 X。 | owner 创建时序正常，MissQueue 控制状态已被前级 X 污染。 |
| `735.6ns` | DUT 顶层 E 输出 `valid=1`、`sink=00x`；该拍 `E.ready=1`。 | DUT 给出了带 X 的 GrantAck payload，而不是可匹配的已知 `sink=0`。 |
| `740.6ns` / `745.6ns` | DUT 继续保持 `E.valid=1`，`sink` 随后为 `1`；responder 已无 owner，`745.6ns` 报 fatal。 | fatal 是异常 E payload/状态持续后的后果，不是首因。 |

## 为什么 `745.6ns` 的 fatal 是次生表象

正常闭环是：TB 在最后一个 `GrantData` 的 D.fire 后创建 `sink=0` 的 GrantAck owner；DUT 随后
通过 E 通道返回同一 `sink` 的 GrantAck，TB 据此删除 owner。

本次在 `735.6ns`，DUT 输出的是 `E.sink=00x`。DCache responder 的
`process_e_fire()` 使用二态变量 `bit [9:0] observed_sink` 接收 E.sink。硬 X/Z 检查关闭时，
这会将四态 `00x` 在赋值时二态化，并可能按 `0` 匹配、删除本应保留的 owner。之后 DUT 继续
输出 E.valid，账本已经没有 owner，才在 `mem_base_sequence.sv:4025` 报出
`E.valid observed without a pending GrantAck owner`。

这暴露了测试框架在关闭硬 X/Z 检查时会把 malformed E payload 静默二态化的健壮性缺口；它应在
后续独立的测试框架修复中保留四态 payload 或无条件把 E.fire 的未知 `sink` 报为错误并禁止清账。
但它不能产生 DUT 的 `E.valid/E.sink` X，因而不是本次 RTL 问题的根因，也不能通过吞掉 fatal
作为 RTL 修复。

## DUT/TB 方向与排除项

`build/rtl/MemBlock.sv:242-255` 明确了 D/E 端口方向：

```text
D.valid / D.bits.* : TB -> DUT
D.ready            : DUT -> TB
E.ready            : TB -> DUT
E.valid / E.sink   : DUT -> TB
```

诊断窗口中 D response 是确定的 `GrantData(source=4, sink=0)`，而 E 的 `valid/sink` 是 DUT
输出。因此：

- 不是 `RM_LS_COMPARE`；该 run 的唯一终止报告是 DCache responder 的 protocol fatal。
- 不是 responder 未在 D.fire 后建立 owner；波形中 owner 已先建立。
- 不是 TB 通过 `E.ready` 注入 `E.valid` 或 `E.sink` 的 X；这两个信号在 DUT 顶层定义为输出。

## 已核验的 RTL/Scala 收敛路径

下列源码说明异常信号经过的正常功能路径；它们说明 E 是 DCache MissQueue 的输出，但尚不足以
单独证明 `700.6ns` 的最初 X 产生位置。

| 位置 | 已核验关系 |
| --- | --- |
| `src/main/scala/xiangshan/cache/dcache/loadpipe/LoadPipe.scala:384-445` | `s2_miss_req_valid = s2_valid && s2_can_send_miss_req`，并驱动 `io.miss_req.valid`。 |
| `src/main/scala/xiangshan/cache/dcache/DCacheWrapper.scala:1475-1533` | `ldu(2).io.miss_req` 接入 `missReqArb`，仲裁输出连接 `missQueue.io.req`。 |
| `src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala:697-699,876-879` | `io.mem_finish.fire` 置 `s_grantack`；收到 D grant 后形成 GrantAck payload，`io.mem_finish.valid := !s_grantack && w_grantfirst`。 |
| `src/main/scala/xiangshan/cache/dcache/mainpipe/MissQueue.scala:1254-1256` | `TLArbiter.lowest` 将各 entry 的 `io.mem_finish` 仲裁为 MissQueue E 输出。 |
| `src/main/scala/xiangshan/cache/dcache/DCacheWrapper.scala:1551-1552` | `bus.e <> missQueue.io.mem_finish`，将 MissQueue E 接到 DCache 顶层。 |
| `build/rtl/DCache.sv:22864-22866`、`build/rtl/MissQueue.sv:7231-7249`、`build/rtl/MissEntry.sv:334,687,769,977-978` | 生成 RTL 对应 E ready、entry valid/sink 与仲裁连接。 |

可见的 DUT 传播路径为：

```text
LoadUnit_2 / LDA2 控制出现 X
  -> DCache ldu_2.io_miss_req_valid
  -> missReqArb.io_in_3_valid / io_out_valid
  -> MissQueue.entries_4.io_req_valid
  -> entries_4 的 GrantAck/finish 控制
  -> MissQueue E 仲裁
  -> auto_inner_dcache_client_out_e_valid / e_bits_sink
```

最优 RTL 修复方向是先从 `LoadPipe` 在 `700.6ns` 变 X 的 `s2_valid`、
`s2_can_send_miss_req` 及其上游 kill/replay/forward 控制反向追踪，而不是只在 E 通道掩盖
结果。修复需要保证无效、kill 或未选中的请求路径不会把 X 带入 `valid`、`ready` 或仲裁选择；
同时应保证 E.valid/E.sink 只能由已握手且字段确定的 D Grant 生成。当前证据不能严格断言这与
SBuffer invalid-forwarding 锥是同一个源头，RTL owner 应在该边界继续定位。

## 波形和日志路径

| 类型 | 绝对路径 |
| --- | --- |
| 诊断 FSDB | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_10k_multiseed_20260906/wave/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl_xzoff_e_diag_20260907.fsdb` |
| 诊断日志 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_10k_multiseed_20260906/log/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl_xzoff_e_diag_20260907.log` |
| 原始硬 X/Z 检查 FSDB | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_10k_multiseed_20260906/wave/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl_xz_wave.fsdb` |
| 原始硬 X/Z 检查日志 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_10k_multiseed_20260906/log/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl_xz_wave.log` |

建议在诊断 FSDB 中按下列绝对信号路径逐拍观察：

```text
top_tb.U_MEMBLOCK.io_ooo_to_mem_issueLda_2_ready
top_tb.U_MEMBLOCK.inner_dcache.dcache.ldu_2.io_miss_req_valid
top_tb.U_MEMBLOCK.inner_dcache.dcache.missReqArb.io_in_3_valid
top_tb.U_MEMBLOCK.inner_dcache.dcache.missReqArb.io_out_valid
top_tb.U_MEMBLOCK.inner_dcache.dcache.missQueue.entries_4.io_req_valid
top_tb.U_MEMBLOCK.inner_dcache.dcache.missQueue.entries_4.s_grantack
top_tb.U_MEMBLOCK.inner_dcache.dcache.missQueue.entries_4.io_mem_finish_valid
top_tb.U_MEMBLOCK.auto_inner_dcache_client_out_d_valid
top_tb.U_MEMBLOCK.auto_inner_dcache_client_out_d_bits_sink
top_tb.U_MEMBLOCK.auto_inner_dcache_client_out_e_ready
top_tb.U_MEMBLOCK.auto_inner_dcache_client_out_e_valid
top_tb.U_MEMBLOCK.auto_inner_dcache_client_out_e_bits_sink
```

## 回归停止状态

- seed `710001` 至 `710005` 已各完成 10,000 笔并通过。
- seed `710006` 原始 run 由硬 X/Z 检查发现 LDA2 X；本次关闭检查的诊断 run 又确认了后续
  DCache E 通道 X 传播与协议后果。
- 已满足“发现 RTL 问题须由独立 subagent review 确认后可停止”的条件。
- 不继续 `710007..710100`，不以关闭 X/Z 检查的 run 作为通过结果；待 RTL 修复后应先重跑
  seed `710006`，再决定是否恢复多 seed 回归。
