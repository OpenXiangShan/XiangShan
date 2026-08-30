# V2 StoreQueue NC 跨 16B 双计数跳过 fault SQ 表项问题（2026-08-30）

| 项目 | 内容 |
|---|---|
| 结论 | 已由独立复核确认的 RTL 问题，不是 RM、UVM `pendingPtr` 或 `isStoreException` sideband 问题。 |
| RTL 版本 | V2 |
| 分支 | `mem_ut_uvm_v2` |
| 核验 commit | `88dec8f6eb51d94b7cd9521dd84bc9278fe5ae9c` |
| testcase | `basicTest` |
| VSEQ | `memblock_dispatch_real_smoke_vseq` |
| cfg / seed | `tc_dispatch_real_mmu_sv39_smoke` / `666666` |
| 目标规模 | `MEMBLOCK_MAIN_TRANS_NUM=10000` |
| 实际终态 | `terminal_done_uid=11/10000`，`UVM_ERROR=10`，无新的 RM compare mismatch。 |
| RTL 修改 | 未修改，按任务约束停止于问题记录。 |

## 1. 复现产物

日志：

```text
mem_ut/ver/ut/memblock/sim/rm_sv39_10k_sta_terminal_iq_drop_20260828/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log
```

波形：

```text
mem_ut/ver/ut/memblock/sim/rm_sv39_10k_sta_terminal_iq_drop_20260828/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.fsdb
```

关键波形层级：

```text
/top_tb/U_MEMBLOCK/io_ooo_to_mem_isStoreException
/top_tb/U_MEMBLOCK/io_ooo_to_mem_lsqio_pendingPtr_flag
/top_tb/U_MEMBLOCK/io_ooo_to_mem_lsqio_pendingPtr_value
/top_tb/U_MEMBLOCK/io_mem_to_ooo_sqDeq
/top_tb/U_MEMBLOCK/inner_lsq/storeQueue/rdataPtrExt_0_value
/top_tb/U_MEMBLOCK/inner_lsq/storeQueue/rdataPtrExt_1_value
/top_tb/U_MEMBLOCK/inner_lsq/storeQueue/allocated_4
/top_tb/U_MEMBLOCK/inner_lsq/storeQueue/hasException_4
/top_tb/U_MEMBLOCK/inner_lsq/storeQueue/committed_4
/top_tb/U_MEMBLOCK/inner_lsq/storeQueue/completed_4
/top_tb/U_MEMBLOCK/inner_lsq/storeQueue/dataBuffer/io_enq_0_valid
/top_tb/U_MEMBLOCK/inner_lsq/storeQueue/dataBuffer/io_enq_1_valid
/top_tb/U_MEMBLOCK/inner_lsq/storeQueue/io_storeAddrInRe_0_hasException
/top_tb/U_MEMBLOCK/inner_lsq/storeQueue/io_storeAddrInRe_0_af
```

## 2. 已确认时序

UID 11 对应 `ROB=0/124`、`SQ=0/4`。它的 STD 先完成，随后 STA fault 回填。该发射次序不是 UVM 凭空构造的非法字段组合：V2 Scheduler 只在入队时成对接受 STA/STD，两个 IQ 的后续发射可以独立进行。

| 时间 | 事实 | 结论 |
|---|---|---|
| `510.3ns` | `issueStd_1` 对 `ROB=0/124,SQ=0/4` fire。 | SQ4 已得到 data。 |
| `520.3ns` | `issueSta_0` 对同一 `ROB/SQ` fire。 | STA/STD identity 与 SQ 表项一致。 |
| `920.3ns` | `rdataPtrExt` 从物理 SQ `3/4` 前跳为 `5/6`。 | SQ4 在自身 fault 回填前被 read pointer 越过。 |
| `975.3ns` | UID 11 的 STA0 `io_lsq_valid=1`，`ROB=0/124,SQ=0/4`。 | 输入 sideband 和 SQ identity 正确。 |
| `980.3ns` | StoreUnit replenish 输出 `hasException=1, af=1`；SQ4 随后 `addrvalid=1`。 | fault 已实际进入 DUT StoreQueue 路径。 |
| `985.3ns` | `StoreQueue.hasException_4=1`。 | 异常回填成功。 |
| `990.3ns` | `StoreQueue.committed_4=1`。 | `pendingPtr` 相关 commit sideband 已正确生效。 |
| 后续至结束 | `completed_4=0`，`io_mem_to_ooo_sqDeq=0`。 | SQ4 未再进入 DataBuffer/SBuffer 完成通路，无法释放。 |

对照 UID 2 的普通 scalar store fault：`hasException_0` 在 `305.3ns` 已置位，`committed_0` 在 `385.3ns` 置位，随后 `390.3ns` 进入 DataBuffer、`395.3ns` 置 `completed_0`、`400.3ns` 产生 `sqDeq=1`。因此 `isStoreException`、`pendingPtr` 与 UVM fault-head 提交链在本次运行中能够正常驱动 StoreQueue 的标准 fault drain，UID 11 的差异来自更早的 read-pointer 跳过。

## 3. RTL 根因

跳过发生前的 SQ3 同时满足：`nc=1`、`unaligned=1`、`cross16Byte=1`、`completed=1`。

`StoreQueue` 的跨 16B 分支会让 DataBuffer 的两个 lane 服务同一个 SQ3。lane1 的
`dataBuffer.io.enq(1).fire && sqNeedDeq` 为 1；与此同时，`readyReadGoVec` 的 lane0 又将
同一 SQ3 的 `allocated && completed && nc` 作为独立推进条件。`PopCount` 将两个条件相加，得到
`sqReadCnt=2`，于是 `rdataPtrExt` 从 SQ3 跨过 SQ4 直接跳到 SQ5。

关键源代码如下：

```scala
// StoreQueue.scala:320-331
val readyReadGoVec = WireInit(VecInit((0 until EnsbufferWidth).map(i =>
  if(i == 0) {
    dataBuffer.io.enq(i).fire && dataBuffer.io.enq(i).bits.sqNeedDeq ||
    allocated(rdataPtrExt(i).value) && completed(rdataPtrExt(i).value) && nc(rdataPtrExt(i).value) ||
    io.mmioStout.fire || io.vecmmioStout.fire
  } else {
    dataBuffer.io.enq(i).fire && dataBuffer.io.enq(i).bits.sqNeedDeq ||
    allocated(rdataPtrExt(i).value) && completed(rdataPtrExt(i).value) && nc(rdataPtrExt(i).value)
  }
)))
sqReadCnt := PopCount(readyReadGoVec)
```

跨 16B 的 `misalignToDataBufferValid` 分支位于 `StoreQueue.scala:1209-1217`，该分支没有与普通分支
`StoreQueue.scala:1219-1224` 相同的 `!ncStall` 约束。因此已完成 NC store 同时触发 DataBuffer lane
和 NC completed 推进，形成对同一物理表项的双计数。生成 RTL 对应的组合加法见
`build/rtl/StoreQueue.sv:4089-4111`，跨 16B DataBuffer valid 选择见
`build/rtl/StoreQueue.sv:11888-11931`。

SQ4 被跨过后，即使后续成功得到 `hasException_4` 和 `committed_4`，DataBuffer 的输入 pointer
已指向 SQ5/SQ6。`completed_4` 只会由 SBuffer handshake、NC response 或 MMIO/CBO 路径置位，
而本例没有任何一条路径再选择 SQ4。`sqDeq` 只对连续 `allocated && completed` 的队头项计数，
所以 SQ4 永久占用并最终阻塞后续请求。

## 4. 归属结论

以下事实排除了 RM 和测试框架根因：

- UID 11 的 STA input、`sqIdx`、fault replenish、`hasException_4`、`committed_4` 都能在同一条 DUT 路径上闭合。
- UID 2 证明相同的 `pendingPtr/isStoreException` 驱动可以产生正常的 fault drain 和 `sqDeq`。
- UVM 的 `std_q=1` 是 SQ4 未释放之后无法继续调度的次生症状，不是造成 pointer 跳过的原因。
- 独立 RTL 复核确认以上组合条件、`PopCount` 和 generated RTL 网络一致。

因此本问题归类为 **V2 `StoreQueue` 的 NC 跨 16B read-pointer 双计数 RTL 缺陷**。

## 5. 建议的 RTL 修改方向

本任务不修改 RTL。后续 RTL owner 应使每个物理 SQ entry 在一个周期内最多贡献一次
`rdataPtrExt` 增量。最小修复方向是：

1. 在 `firstWithMisalign && firstWithCross16Byte` 的 DataBuffer valid 条件中排除已完成的 NC store，保持 NC 只通过 NC completion 路径推进。
2. 在 `readyReadGoVec` 中显式去重同一 `SqPtr` 的 DataBuffer 与 `completed && nc` 原因，作为结构性保护，避免未来新增路径重复计数。
3. 增加定向回归：前一笔 `nc && unaligned && cross16Byte` store，后一笔 scalar store 的 STD 先到、STA fault 后到，断言后者最终产生 `sqDeq` 或合法 redirect cancel，且 `rdataPtr` 不跨过未完成表项。

## 6. 关联长期知识

本问题补充到 [ROB 压缩与后端指令信息流](flows/rob_compress_and_backend_instruction_flow.md) 的 StoreQueue completion 边界。该 flow 已在索引中覆盖 `StoreQueue`、`completed`、`sqDeq`、NC 与 fault store 关键词，因此不新增重复索引项。
