# 严格 PBMT=00/non-NC 10k 多 seed 回归的 SBuffer forwarding X 传播 RTL 问题记录

## 结论

严格 `PBMT=00/non-NC` 的 10k 多 seed 回归已按 RTL 问题停止，而不是继续更换 seed。

- seed `710001` 至 `710005` 各完成 10,000 笔请求，均为 `TEST_PASS`，
  `UVM_ERROR=0`、`UVM_FATAL=0`，并且每份都有 10,000 条 `RM_LS_COMPARE ... PASS`。
- seed `710006` 在 `720.6ns` 因硬 X/Z 检查累计 10 个 `UVM_ERROR` 结束；日志中没有
  `RM_LS_COMPARE`，所以它不是 RM 比较失败。
- 独立 subagent 已复核波形、DUT 生成 RTL、相关 Scala 源码以及 TB 连接方向，确认根因为
  DUT 内部 SBuffer 到 LoadUnit 的无效 forwarding payload 未被可靠门控，造成 X 沿
  `forwardMask -> s2_full_fwd -> replay/RAW query -> LDA2 valid` 传播。
- 本轮未修改 RTL、RM 或测试框架；seed `710007` 及后续 seed 均未启动。回归状态为
  “5 次 10k 通过；第 6 次发现 RTL X-propagation 问题后停止”。
- 为确认关闭硬 X/Z 检查后是否只是测试框架误报，曾对同一 seed 做一次仅诊断用途的
  `MEMBLOCK_HARD_XZ_CHECK_EN=0` 重跑。该 run 没有通过，随后暴露出 DCache E 通道的
  第二个 DUT X 传播表象；详见
  [DCache E 通道 X 传播补充诊断](memblock_pbmt0_non_nc_10k_dcache_e_xprop_rtl_failure_20260907.md)。
  该补充证据不能证明它与本文件的 SBuffer forwarding X 是同一个最初根因。

这是一项 RTL 仿真健壮性缺陷：当 producer 的 `valid=0` 时，payload 本可以不具备数据语义，
但 consumer 不能让该 payload 未门控地参与控制逻辑。本问题已经使 DUT 的顶层 LDA2 writeback
`valid` 变为 X，因此不是单纯的无效 payload 可忽略现象。

## 回归场景

```text
tc=basicTest
ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq
cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k
mode=pbmt0_non_nc_10k_multiseed_20260906
seed=710001..710006
runtime override=+MEMBLOCK_MAIN_TRANS_NUM=10000 +MEMBLOCK_CHECK_TRIGGER_EN=0
```

关键约束：

- `mPBMTE=0`、`hPBMTE=0`，S1/S2 PBMT 权重固定为 `00`；普通地址位于
  `0x8000_0000..0x9000_0000` 的可缓存 DDR 范围。
- 只生成 scalar integer Load/Store；不生成 NC/MMIO、AMO、CBO、Prefetch、CSR/SFence
  或进程切换。
- 保留普通非对齐及跨 16B、同一 cache line 的访问。
- `MEMBLOCK_CHECK_TRIGGER_EN=0` 仅关闭输出 trigger 专项检查；
  `MEMBLOCK_HARD_XZ_CHECK_EN=1`，因此本次有效地检出了 DUT 的 X 输出。

## 失败表象

第一个可见报错发生在 `690.6ns`：

```text
RM writeback valid is X/Z: source_kind=1 lane=2
LDA2 valid is X/Z
```

后续 `issueLda_2_ready` 也成为 X，并在 `720.6ns` 达到 UVM error quit count。这里
`memblock_rm_dut_writeback_observer` 只是采样 DUT 的 LDA2 输出；它遇到 X 时不构造 RM
写回记录，因而既没有产生 golden compare，也没有反向驱动 DUT。该 reporter 名称中的 `RM`
不表示“RM 算错”。

## 已确认的致错锥

下表中的时间是 FSDB 观察窗口中的近似时刻。`565.6ns` 是当前已观察到的最早相关 X，
不是对全设计“绝对第一处 X”的声明。

| 时间 | 观察 | 结论 |
| --- | --- | --- |
| 约 `565.6ns` | `LoadUnit_2.s0_rep_stall=X`；`io_replay_valid=0` 但组合表达式仍读取 replay payload 的 `lqIdx`。 | 这是同类“无效 payload 未完全隔离”的早期证据，但尚未作为本次最终致命链的唯一首因。 |
| 约 `580ns` | `LoadUnit_2.io_sbuffer_valid=0`，同时 `io_sbuffer_forwardMask[15:0]=X`；LSQ/UBuffer 的对应 mask 为已知 `0`，`io_lsq_forward_dataInvalid=0`，待执行 load 的 `s2_in_r_mask=16'h8000`。 | X 已位于 DUT 内部 `Sbuffer -> LoadUnit_2` forwarding 接口。 |
| 随后 | `LoadUnit` 直接 OR 三路 forwarding mask，得到 `s2_fwd_mask=X`，继而 `s2_full_fwd=X`。 | consumer 未用 producer 的 `valid` 屏蔽 SBuffer mask。 |
| 约 `615.6ns` | DCache 的原始 `s2_bank_conflict=1`，而表达式同时依赖 `~s2_full_fwd`。 | `s2_bank_conflict` 被 X 污染；这不是 DCache responder 驱入 X。 |
| 随后 | `LoadQueueRAW query2 ready=X` → `io_lsq_stld_nuke_query_req_ready=X` → `s2_out_rep_info_cause_8=X` → `s2_safe_wakeup`/`s3_safe_writeback`/`s3_out_valid=X`。 | X 进入 LoadUnit 的 replay/写回控制链。 |
| `690.6ns` 起 | `io_mem_to_ooo_writebackLda_2_valid=X`，monitor 与 RM observer 报告硬 X/Z 错误。 | 已到达 DUT 顶层输出，导致本次仿真失败。 |

生成 RTL 中的关键组合关系为：

```text
io_sbuffer_forwardMask[*]
  -> s2_fwd_mask[*] = lsq_mask[*] | sbuffer_mask[*] | ubuffer_mask[*]
  -> s2_full_fwd
  -> s2_bank_conflict / stld_nuke_query
  -> s2_out_rep_info_cause_8
  -> s2_safe_wakeup / s3_safe_writeback
  -> s3_out_valid
  -> io_mem_to_ooo_writebackLda_2_valid
```

源码和生成 RTL 的对应位置：

- `src/main/scala/xiangshan/mem/sbuffer/Sbuffer.scala:807-845`：forwarding 候选值和 tag match
  由 `RegEnable(..., forward.valid)` 保存；在 `forward.valid=0` 时，寄存器内容本身不保证
  为确定的零值，但输出 `forwardMask` 没有以 `forward.valid` 统一清零。
- `src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala:1387-1399`：三个 producer 的
  `forwardMask` 被直接 OR，并用于 `s2_full_fwd`。
- `src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala:1369-1385`：`s2_full_fwd` 进入
  LoadQueue 的 st-ld nuke query data-valid/control 路径。
- `build/rtl/LoadUnit.sv:1617-1677`、`2179-2189`、`2985-2994`、`3928`：上述组合、
  RAW query replay cause、安全写回和顶层 LDA valid 的生成链。
- `build/rtl/LoadUnit.sv:951-960`：`s0_rep_stall` 对 replay payload 的未充分 valid-gate
  读取，是需要一并审查的早期 X 泄漏点。

## 为什么不是 RM 或测试框架问题

1. `io_mem_to_ooo_writebackLda_2_valid` 与 `io_ooo_to_mem_issueLda_2_ready` 是 DUT 输出；
   writeback monitor、RM observer 只采样，不能向该锥反向驱动。
2. 根因信号 `io_sbuffer_forwardMask[*]` 是 `MemBlock` 内部由 SBuffer 连到 LoadUnit 的连接，
   见 `src/main/scala/xiangshan/mem/MemBlock.scala:921-926`；它不是 L2TLB、DCache responder
   或 issue agent 的顶层输入。
3. 故障锥中已抽样的 TB→DUT 输入为已知 0/1，尤其 LSQ/UBuffer forwarding mask、
   `dataInvalid` 和 DCache 原始 bank-conflict 均非 X。当前波形范围不能证明“所有设计输入永远
   无 X”，但已足以排除本致错锥由 TB 直接注入 X 的解释。
4. `s2_fwd_vp_match_invalid` 的三个输入在故障拍均为 `0`，已排除该 OR 链作为本次根因。

## 证据路径

| 类型 | 路径 |
| --- | --- |
| 失败日志 | `mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_10k_multiseed_20260906/log/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl_xz_wave.log` |
| FSDB | `mem_ut/ver/ut/memblock/sim/pbmt0_non_nc_10k_multiseed_20260906/wave/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=710006_rtl_xz_wave.fsdb` |
| 临时 VCD 抽取 | `eda01:/tmp/fsdb710006_lixiangrui/lu2_early2/lu2.vcd`；该文件用于本次 review，非长期归档物。 |
| 通过日志 | 同目录中 seed `710001` 至 `710005` 的 `*_rtl.log`。 |

## 与 unaligned-head wakeup 补丁的边界

上游 `93209f233c` 的主题是 LoadQueueReplay 对非对齐 head 的 DCache wakeup 历史保持。
当前分支并未包含该 commit，但本次问题的致错锥是 `Sbuffer forwardMask -> LoadUnit s2_full_fwd`，
与该 wakeup-history 锥无交集。因此是否合入该补丁不能作为本问题的归因或修复依据。

## 建议的 RTL 修复方向（未实施）

推荐优先保证 forwarding 接口在 `valid=0` 时输出确定的零 mask；等效地，也可在 LoadUnit 汇聚
`LSQ/SBuffer/UBuffer` forwarding mask 时以各 producer 的 `valid` 显式门控。修复必须同时确认
forward data、`matchInvalid`、`dataInvalid` 等同类 payload/control 的无效周期行为，避免只消除
这一条 mask 的 X。

另外应审查 `s0_rep_stall`：当 `io_replay_valid=0` 时不应让 `io_replay_bits.uop.lqIdx` 的无效
payload 影响组合控制。上述建议仅记录 RTL 修复方向；本任务没有修改 RTL。

## 停止边界

- 已满足“发现 RTL 问题须经 subagent 独立 review 确认后结束”的条件。
- 没有 RM 问题需要记录方案、修改或重跑。
- 未修改 RTL；关闭硬 X/Z 检查只用于观察同一失败 run 的后续传播，未被当作通过结果，
  反而确认了额外的 DUT E 通道 X 传播。
- 不继续 seed `710007` 至 `710100`，待 RTL 修复后应从 seed `710006` 重新验证，再决定是否恢复多 seed 回归。
