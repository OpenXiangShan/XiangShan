# 问题2：RM Load exception `0x0` vs DUT `0x20` 波形根因分析

日期：2026-10-15

## 1. 结论

当前根因高度明确：**测试框架 RM 只建模了 L2/DCache GrantData 的 denied/corrupt，漏掉了 Uncache D response 的 denied/corrupt。**

真实 seed 中的失败 Load 都是翻译成功、地址对齐、PMA-MMIO、走 Uncache 返回的访问。Responder 允许按配置随机注入 Uncache `denied`；DUT 按设计把该位转换成 Load Access Fault，即 `exceptionVec[5] = 1`，所以 DUT/status 返回 `0x20`。RM 却查询了与 MMIO/Uncache 无关的 L2 sticky D-error ledger，得到 `denied=0, corrupt=0`，因此仍期望 `0x0`，最终报 compare mismatch。

责任倾向：

| 模块 | 判断 | 依据 |
| --- | --- | --- |
| DUT | 暂无功能错误证据 | RTL 明确规定 Uncache `denied -> loadAccessFault(bit5)`；可用波形已验证同一通路的 `corrupt -> hardwareError(bit19)` 逐级传递正确 |
| Responder | 错误注入行为符合当前配置，但可观测性不足 | `MEMBLOCK_UNCACHE_DENIED_WT=1`、`MEMBLOCK_UNCACHE_CORRUPT_WT=1`；response record 保存并驱动 denied/corrupt，但没有 UID/ROB/D.fire provenance ledger |
| RM | 主要责任 | Load D-error expectation 只调用 `read_l2_d_error_for_rm()`，没有 Uncache 查询接口和 owner 关联 |
| 接口采样 | 不倾向采样错位 | WB observer 同一拍原子采样 valid、ROB、exception、debug；WB status 与 RM actual 对同一 ROB 都得到 `0x20` |

准确性边界：review29 完整运行没有 FSDB，因此 UID584/939/1018/1547 对应的同一次 TL D `denied=1` 目前是由“真实日志 + 配置 + RTL/RM 调用链”闭合推断，不是直接波形位值。现有 FSDB 中没有任何有效 D beat 的 `denied=1`，不能把别的信号冒充成这四笔请求的 denied 证据。

## 2. 证据文件

### 2.1 完整真实 seed 日志

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/review29_sq_deq_guard_real_20261015_u/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_review29_sq_deq_guard_real_1_98_1_seed37617171_20261015_u.log
```

该运行自然结束，最终 `UVM_ERROR=10`、`UVM_FATAL=0`。其中四个错误属于本文分析的 `expected=0x0, actual/status=0x20` 同类模式。

关键配置：

```text
MEMBLOCK_UNCACHE_DENIED_WT = 1
MEMBLOCK_UNCACHE_CORRUPT_WT = 1
```

配置打印位于日志第 223、224 行，命令行也明确带有：

```text
+MEMBLOCK_UNCACHE_DENIED_WT=1
+MEMBLOCK_UNCACHE_CORRUPT_WT=1
```

### 2.2 可用真实 seed FSDB

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_r2_survivor_real_1_98_1_20261014_c/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_real_1_98_1_seed37617171_20261014_c.fsdb
```

FSDB 信息：

```text
FSDB version: 5.9
timescale: 1ps
min time: 0
max time: 159970.900ns
unique vars: 206802
status: finished
```

该运行在 `159970.900ns` 因另一处旧的 `LSQ_COMMIT` fatal 结束：

```text
UVM_FATAL @ 159970.900ns [LSQ_COMMIT] normal commit uid=464 was not applied
```

所以它不覆盖首个本文同类失败 UID584 的最终写回 `187883.400ns`，更不覆盖 UID939/1018/1547。

FSDB 全时段扫描结果：

- 没有任何 `auto_client_out_d_valid=1 && denied=1` 的 D beat。
- 有一笔有效 `corrupt-only` D beat，对应 UID14，可用于完整验证 Uncache D error 到 LDA exception 的结构通路。

### 2.3 不能作为本文直接证据的旧 FSDB

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_mmio_r2_fix_v2_20261012/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl.fsdb
```

该文件虽然结束时间达到 `566250.900ns`，但对应旧运行长期阻塞在 `uid=0` 的 commit pending 状态，没有复现 review29 中 UID584 的指令流，因此不能按绝对时间套用到后续真实运行。

## 3. 真实失败 UID 的共同特征

| UID | DUT fault feedback | RM compare | ROB/LQ | 访问 | PA | 分类 | RM L2 ledger | 实际 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 584 | `187883.400ns` | `187891.900ns` | ROB `0/15`, LQ `0/32` | 4B，对齐 | `0x04f03154..157` | `allPmaMmio=1`, `allNormalCacheable=0` | denied=0, corrupt=0 | LDA2 `0x20` |
| 939 | `6112853.400ns` | `6112881.900ns` | ROB `0/50`, LQ `1/36` | 4B，对齐 | `0x00966368..36b` | `allPmaMmio=1`, `allNormalCacheable=0` | denied=0, corrupt=0 | LDA2 `0x20` |
| 1018 | `6137803.400ns` | `6137811.900ns` | ROB `0/129`, LQ `1/17` | 1B，对齐 | `0x08eae639` | `allPmaMmio=1`, `allNormalCacheable=0` | denied=0, corrupt=0 | LDA2 `0x20` |
| 1547 | `6335688.400ns` | `6335696.900ns` | ROB `0/18`, LQ `0/21` | 1B，对齐 | `0x09ba3fff` | `allPmaMmio=1`, `allNormalCacheable=0` | denied=0, corrupt=0 | LDA2 `0x20` |

共同点：

1. 都是 scalar Load，且地址按实际访问大小对齐，不是 misalign exception。
2. 地址翻译成功，`expected_exception=0x0`。
3. 都被 PMA 分类为 MMIO，不是 normal cacheable，所以数据返回走 Uncache 通路。
4. 都从固定的 Uncache 写回端口 LDA lane2 返回。
5. WB status 和 RM 捕获的 DUT writeback exception 都是 `0x20`，不是只有一个 observer 看错。
6. RM 打印的 `RM_LS_D_ERROR` 全部来自 L2 sticky ledger，结果均为 denied=0、corrupt=0。

UID1547 的完整日志片段含义：

```text
6335688.400ns WB_STATUS: uid=1547 rob=0/18 lq=0/21 exception_vec=0x20
6335696.900ns TRANSLATION: PA=0x9ba3fff, allPmaMmio=1, expected_exception=0x0
6335696.900ns RM_LS_D_ERROR: line=0x9ba3fc0 denied=0 corrupt=0
6335696.900ns DUT_LOAD_WB: source=LDA lane=2 exception=0x20
6335696.900ns COMPARE: expected=0x0 status=0x20 actual=0x20
```

这里 `RM_LS_D_ERROR denied=0` 不能证明 Uncache D 没有 denied。它只说明被查询的 **L2 cache-line ledger** 没有错误；这正是 RM 查错数据源的表现。

## 4. 完整波形证据：UID14 的 Uncache corrupt 对照链

UID14 不是本文的 bit5/denied 样本，而是同一结构通路的 bit19/corrupt 对照。它的价值是直接验证：Responder 的 D error 会被 DUT 保存、送入 LoadQueueUncache、形成 MMIO writeback，并最终出现在 LDA2；没有发生 ROB、lane 或 exception 位采样错位。

UID14：

```text
VA = 0x8b93a1c6
PA = 0x0b93a1c6..1c7
ROB = 0/85
LQ = 0/37
PMA-MMIO
```

波形时间链：

| 时间 | 层级/信号 | 值 | 含义 |
| --- | --- | --- | --- |
| `3855.900ns` | `inner_uncache/auto_client_out_d_valid` | 1 | 外部 Uncache D beat 有效 |
| `3855.900ns` | `.../d_bits_source` | 0 | source=0 |
| `3855.900ns` | `.../d_bits_denied` | 0 | 不是 denied |
| `3855.900ns` | `.../d_bits_corrupt` | 1 | 本次注入 corrupt |
| `3860.900ns` | `inner_uncache/io_lsq_resp_valid` | 1 | Uncache 将响应送回 LSQ |
| `3860.900ns` | `.../resp_bits_id` | 0 | 返回 owner id=0 |
| `3860.900ns` | `.../denied, corrupt` | 0, 1 | 错误位保持 |
| `3865.900ns` | `uncacheBuffer/io_uncache_resp_valid` | 1 | LoadQueueUncache 收到响应 |
| `3865.900ns` | `.../denied, corrupt` | 0, 1 | 错误位保持 |
| `3875.900ns` | `uncacheBuffer/io_mmioOut_2_valid` | 1 | 形成 MMIO writeback |
| `3875.900ns` | `.../robIdx` | `0/85` | owner 与 UID14 一致 |
| `3875.900ns` | `.../exceptionVec_5` | 0 | 无 Load Access Fault |
| `3875.900ns` | `.../exceptionVec_19` | 1 | corrupt 转成 hardwareError |
| `3895.900ns` | `top_tb/io_mem_to_ooo_writebackLda_2_valid` | 1 | 最终 LDA2 写回 |
| `3895.900ns` | `.../robIdx` | `0/85` | ROB 未错位 |
| `3895.900ns` | `.../exceptionVec_5` | 0 | bit5 保持 0 |
| `3895.900ns` | `.../exceptionVec_19` | 1 | bit19 保持 1 |
| `3895.900ns` | `.../debug_isMMIO` | 1 | 确认是 MMIO/Uncache 写回 |

随后日志在 `3903.400ns` 报 UID14 WB status exception=`0x80000`，RM 在 `3911.900ns` 对同一 ROB 也捕获到 LDA2 exception=`0x80000`。

这条波形证明 DUT 的 Uncache error 通路和测试框架的 LDA2 采样在 owner、lane、exception 位上是一致的。bit5 与 bit19 的唯一区别是 RTL 对 denied/corrupt 的映射条件，不是另一条接口。

## 5. 波形反例：UID98 没有 Uncache error 时不产生异常

UID98 用于排除“LDA2 或 MMIO 路径会无条件产生异常”的可能性：

| 时间 | 事件 | 关键值 |
| --- | --- | --- |
| `31380.900ns` | Uncache D valid | denied=0, corrupt=0 |
| `31385.900ns` | `inner_uncache/io_lsq_resp_valid` | denied=0, corrupt=0 |
| `31390.900ns` | `uncacheBuffer/io_uncache_resp_valid` | denied=0, corrupt=0 |
| `31400.900ns` | `io_mmioOut_2_valid` | ROB `1/9`, bit5=0, bit19=0 |
| `31415.900ns` | LDA2 valid | ROB `1/9`, bit5=0, bit19=0, isMMIO=1 |

旧运行中 UID98 的 RM 期望 `0x2000`、DUT 返回 0，属于另一类 translation/PMA 建模问题，不是本文的 Uncache denied 漏建模。

## 6. DUT 行为链

### 6.1 外部 TL D error 被 Uncache 保存

文件：`src/main/scala/xiangshan/cache/dcache/Uncache.scala`

```scala
resp_nderr := x.denied || x.corrupt
resp_denied := x.denied
resp_corrupt := x.corrupt
```

返回 LSQ 时继续携带：

```scala
r.nderr := resp_nderr
r.denied := resp_denied
r.corrupt := resp_corrupt
```

### 6.2 LoadQueueUncache 保存响应并生成异常

文件：`src/main/scala/xiangshan/mem/lsqueue/LoadQueueUncache.scala`

```scala
when (io.uncache.resp.fire) {
  uncacheData := io.uncache.resp.bits.data
  nderr := io.uncache.resp.bits.nderr
  denied := io.uncache.resp.bits.denied
  corrupt := io.uncache.resp.bits.corrupt
}
```

MMIO 写回明确规定：

```scala
io.mmioOut.bits.uop.exceptionVec(hardwareError) := corrupt && !denied
io.mmioOut.bits.uop.exceptionVec(loadAccessFault) := denied
io.mmioOut.bits.debug.isMMIO := true.B
```

因此对 MMIO/Uncache Load：

- `denied=1` 时必须得到 bit5=`loadAccessFault`，即 `0x20`。
- `denied=0, corrupt=1` 时得到 bit19=`hardwareError`，即 `0x80000`。
- denied 优先于 corrupt；即使 responder 让 `corrupt=1` 跟随 denied，DUT 仍只置 bit5，不置 bit19。

NC 路径使用相同映射规则。

### 6.3 固定从 LDA lane2 写回

`UncacheWBPort=2`。LoadQueueUncache 的 `mmioOut` 经 pipeline register 送到 `io.mmioOut(2)`，再经 LoadUnit 的 MMIO pipeline 送到 `mem_to_ooo.writebackLda(2)`。

这与四个真实失败全部显示 `source=LDA lane=2` 一致。

## 7. Responder 行为链

文件：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`

创建 Uncache response record 时只随机一次：

```systemverilog
inject_denied = sample_d_error_enable(
    seq_csr_common::get_uncache_denied_wt(), "Uncache denied");
d_denied = backend_denied || inject_denied;

inject_corrupt = sample_d_error_enable(
    seq_csr_common::get_uncache_corrupt_wt(), "Uncache corrupt");
d_corrupt = backend_corrupt || inject_corrupt || d_denied;
```

随后 denied/corrupt 固定保存在 `uncache_response_record_t`，scheduler 和 D hold 只搬运 record，不会重随机：

```systemverilog
rsp_xact.auto_inner_buffers_out_d_bits_denied  = current_d_record.denied;
rsp_xact.auto_inner_buffers_out_d_bits_corrupt = current_d_record.corrupt;
```

现有 owner 诊断只保存：

- source
- address
- A.fire cycle
- D.fire cycle/count

现有 record 虽保存 denied/corrupt，但 D.fire 后立即清空。没有可供 RM 查询的历史 ledger，也没有把该 response 稳定关联到 UID/ROB/LQ owner。

## 8. RM 错误逻辑

文件：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`

`observer_capture_commit_actual()` 获取 DUT LDA writeback 后调用：

```systemverilog
observer_apply_load_d_error_expectation(item, load_view.sample_seq)
```

但 `observer_apply_load_d_error_expectation()` 对所有 Load 都按 PA cache line 调用：

```systemverilog
ro.read_l2_d_error_for_rm(line_addr, access_sample, error_view)
```

然后仅依据 L2 ledger 设置：

```systemverilog
if (item.expected_dcache_denied)
    item.expected_exception[5] = 1'b1;
if (item.expected_dcache_corrupt)
    item.expected_exception[19] = 1'b1;
```

`read_l2_d_error_for_rm()` 位于：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv
```

该 API 只读取 `query_l2_d_error_at_sample()` 的 L2 line record。当前代码没有：

- `read_uncache_d_error_for_rm()`
- Uncache D completion history
- source generation 防复用信息
- source/idResp 到 UID/ROB/LQ 的 owner 关联

所以 PMA-MMIO Load 即使真的收到 Uncache denied，RM 仍会打印 L2 denied=0 并期望 exception=0。

## 9. 为什么不倾向接口采样错位

1. `memblock_rm_dut_writeback_observer` 在同一个 `mon_cb` 采样点同时读取 LDA valid、ROB flag/value、exceptionVec、data 和 debug 标志，不是分拍拼接。
2. RM actual 和 WB status 对 UID584/939/1018/1547 都得到相同的 `0x20`。
3. UID14 波形中 ROB `0/85`、bit19、isMMIO 从 `mmioOut_2` 到最终 LDA2 一致。
4. UID98 波形中无 error 的 MMIO response 最终 exception 也保持 0。

仍需注意一个独立风险：WB history 当前按“最新的相同 ROB flag/value”查找，长期运行存在 ROB wrap 和历史残留风险。但本文四个样本中 status exception 与 selected LDA actual 完全一致，且都具备 PMA-MMIO/LDA2 特征；没有证据表明本次 `0x20` 是选中了错误历史 owner。

## 10. 建议修复方案

### 10.1 正式方案：增加 Uncache D-error provenance ledger

新增只读历史记录，记录每一次真正完成的 Uncache transaction，而不是只看当前 D hold：

```text
request_generation
source
address
response_kind
A.fire sample/cycle
D.fire sample/cycle
denied
corrupt
is2lq/nc
mid/sid owner mapping
最终 UID/ROB/LQ owner
consumed/retired state
```

Owner 关联应使用 DUT 的 Uncache request/idResp 信息和框架已有 UID/ROB/LQ owner 表，至少形成：

```text
external source/sid + generation
    -> Uncache mid/entry
    -> LQ/ROB owner
    -> UID
```

不能只按 PA 查“最近一笔”，因为同地址复用、response reorder、source 复用和 redirect stale response 都可能造成误配。

### 10.2 RM 查询规则

在 translation/PMA 分类完成且确认是 PMA-MMIO 或 NC/Uncache Load 后：

1. 使用 UID/ROB owner 和实际 LDA `sample_seq` 查询唯一的 Uncache completion record。
2. `denied=1`：设置 expected bit5。
3. `denied=0 && corrupt=1`：设置 expected bit19。
4. 无错误：保持 expected exception 不变。
5. 若存在多条候选、owner generation 不唯一或 response 尚未完成，报告 provenance-not-ready，而不是猜测。

禁止以下简化：

- 不能看到 DUT actual bit5 后反推 expected bit5，这会形成循环比较。
- 不能对所有 PMA-MMIO Load 无条件置 bit5，正常 MMIO response 应无异常。
- 不能仅按地址或仅按 4-bit source 查找；source 会复用，地址也会复用。
- 不能把 L2 sticky ledger 与 Uncache one-shot response ledger 混为一类。

### 10.3 同时增强日志

在 Uncache A.fire、response record 创建和 D.fire 处打印或保存：

```text
generation, source, address, kind, denied, corrupt,
accept_sample, complete_sample, mid, sid, is2lq, nc,
rob, lq, uid
```

RM compare 日志应同时打印：

```text
error_source=L2|UNCACHE
matched_generation
matched_D_fire_sample
denied/corrupt
owner_match_reason
```

这能把目前“根据 RTL 调用链推断 denied”升级为每个 UID 的直接 provenance 证据。

### 10.4 临时规避

若只为暂时放行其他问题，可设置：

```text
+MEMBLOCK_UNCACHE_DENIED_WT=0
+MEMBLOCK_UNCACHE_CORRUPT_WT=0
```

这只是关闭测试覆盖，不是正式修复，不应作为最终方案。

## 11. 必须覆盖的遗漏场景

正式修复至少应增加以下定向测试：

1. MMIO Load，denied=1、corrupt=1，期望仅 bit5。
2. MMIO Load，denied=0、corrupt=1，期望仅 bit19。
3. MMIO Load，denied=0、corrupt=0，期望无异常。
4. NC Load 的以上三种组合。
5. 多 outstanding、response reorder。
6. source 重用但 generation 不同。
7. 同 PA 连续访问和并发访问。
8. redirect 后旧 response 到达，不得匹配新 owner。
9. ROB value wrap，flag 不同。
10. D.fire 到 LDA writeback 有延迟或 backpressure。
11. denied/corrupt 与 L2 sticky error 同时存在时，必须按实际访问通路选择来源。

## 12. 最终判断

本文问题不是“DUT 平白多报了 Load Access Fault”，而是：

> 测试环境自己允许 Uncache responder 注入 denied，DUT 正确地把 denied 转成了 `0x20`；RM 却只查 L2 cache 的错误记录，没有记录和查询 Uncache 这次返回的 denied，因此 expected 漏掉 bit5。

在补齐 UID/ROB 级 Uncache D completion provenance 后，四个 `0x20` mismatch 应由 RM 正确预测并通过 compare。修复后需要用同一 seed `37617171` 完整跑完，确认 UID584/939/1018/1547 不再报错，并检查剩余 UID14/336 bit19、UID98 bit13 和 data/data_valid mismatch，它们是独立问题，不能与本文混合归因。
