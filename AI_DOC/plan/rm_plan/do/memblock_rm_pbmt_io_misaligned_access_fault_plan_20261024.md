# MemBlock RM PBMT-IO 非对齐 Access Fault 修复计划

## 1. 问题与责任边界

`tc_dispatch_real_mmu_sv39_smoke` 的 `seed=212889620` 中，UID113 是非对齐 Load，DUT feedback、status
和 LDA writeback 均报告 Load Access Fault `0x20`，RM 却期望 Load Address Misaligned `0x10`。

责任属于 RM。DUT 已按照有效 memory type 区分 PBMT-NC 与 PBMT-IO；RM 虽然逐 byte 得到精确
`effective_pbmt`，但只聚合 `effective_pbmt == 0`，最终把 PBMT-NC (`01`) 和 PBMT-IO (`10`) 都折叠成
“非 PMA”，并统一送入 Address Misaligned 分支。后续随机 seed `399662848` 又暴露同一异常组合逻辑的
第二个缺陷：RM 把 TLB translation AF 与翻译成功后的 S2 PMA/PMP AF 聚合成同一个 `access_fault`，
导致 Load PBMT-NC misaligned 与 PMA/PMP AF 本应共存时只保留 AF；Store 则需要保持 AF 抑制
storeAddrMisaligned 的 DUT 语义。该问题不修改 DUT 激励、TLB token、
redirect/reissue 生命周期或 RTL。

## 2. 证据

原始日志：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/functional_cfg_cov_20261023/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=212889620_rtl_eda_reg_20261023_094331_224816_26358.log
```

关键事实：

- UID113 最终 VA 为 `0x8755b8f1`，多字节 Load 非对齐。
- `mPBMTE=1`、`hPBMTE=1`、`hdMisalign=1`。
- `allPbmtPma=0`、`allPmaMmio=0`。
- DUT feedback/status/LDA 异常均为 `0x20`。
- RM expected 为 `0x10`。

最终 issue 冻结的是当时最新已完成的 token 183；token 236 在最终 issue 之后才完成，不应替换已冻结
动态实例。异常从 DUT 早期反馈开始持续为 `0x20`，不支持旧 snapshot 偶发污染假设。

随机 seed `399662848` 的 FSDB 位于：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/mem_ut/ver/ut/memblock/sim/rm_pbmt_io_misaligned_debug_20261024/wave/
```

在 `155800.700ns`，UID625/ROB `0/101` 为 FP Load，`s2_pbmt=01`、`s2_in_r_nc=1`、
`s2_in_r_isMisalign=1`，同时 `io_pmp_ld=1`。DUT 独立生成 `s2_real_exceptionVec_4=1` 与
`s2_real_exceptionVec_5=1`，最终 status `0x30`；RM 仅预测 `0x20`。

## 3. 源码修改方案

修改文件：

```text
mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv
```

修改 `observer_build_commit_item()`：

1. 在现有 `all_bytes_pma_pbmt` 之外记录首个有效 byte 的精确 `first_effective_pbmt`。该字段只用于判定
   首页首字节是否因 PBMT-NC/PBMT-IO 立即终止，以及立即终止时的 exception cause；现有
   `all_bytes_pma_pbmt` 继续表达整笔 split access 是否全部为 PBMT-PMA。
   `first_effective_pbmt_valid` 初始化为 0，只有 byte 0 成功完成 effective PBMT 选择后才置 1；Bare
   identity 不读取该字段。
2. `observer_get_effective_pbmt()` 保持现有 S1/S2/PBMTE 选择语义，不从 raw stage payload 或 runtime CSR
   重算 memory type。
3. 当访问非对齐、当前没有更高优先级 Access/PF/GPF，且首个有效 memory type 为 PBMT-IO 时，预测：
   - Load Access Fault bit 5；
   - Store Access Fault bit 7。
4. PBMT-NC 保持 Address Misaligned：Load bit 4、Store bit 6。
5. PBMT-PMA 路径继续使用 PMA/PMP view：PMA MMIO 非对齐预测 Access Fault；普通 cacheable 且硬件
   misalign enable 时不预测 misaligned。
6. 拆分 `translation_access_fault` 与 `pma_pmp_access_fault`：TLB S1/S2 AF/PF/GPF 抑制 misaligned，
   因为此时物理地址和 PMA/PMP response 不可信；翻译成功后的 S2 PMA/PMP AF 不抑制 PBMT-NC
   misaligned。LoadUnit 中 S2 PMA/PMP AF 可与 PBMT-NC Load Address Misaligned bit 4 同时置位；
   StoreUnit 的 `s2_un_misalign_exception` 会让 S2 PMA/PMP AF bit 7 抑制 Store Address Misaligned bit 6。
   PBMT-IO misaligned 本身映射为 AF，因此与 PMA/PMP AF 合并到同一个 Load bit 5 / Store bit 7。
7. `dut_stops_after_first_page` 只在首字节为 PBMT-NC/PBMT-IO 时成立，避免查询 DUT 不会访问的第二页；
   停止原因必须保留精确 PBMT 类型，不能再用一个布尔量同时决定异常 cause。
8. 首字节为 PBMT-PMA 时继续查询后续 split 页；如果后续 byte 变为 PBMT-NC/PBMT-IO，整笔访问由
   Load/Store MisalignBuffer 的 uncache split 规则预测 Address Misaligned，而不是按后续 PBMT-IO 改写成
   Access Fault。只有首字节 PBMT-IO 的立即终止路径预测 Access Fault。

跨页矩阵：

| 首页首字节 PBMT | 后续 split 页 PBMT | 查询停止点 | 无更高优先级异常时的 cause |
| --- | --- | --- | --- |
| NC (`01`) | 不查询 | byte 0 | Load/Store Address Misaligned bit 4/6 |
| IO (`10`) | 不查询 | byte 0 | Load/Store Access Fault bit 5/7 |
| PMA (`00`) | NC (`01`) | 查询到 NC split | Load/Store Address Misaligned bit 4/6 |
| PMA (`00`) | IO (`10`) | 查询到 IO split | Load/Store Address Misaligned bit 4/6 |
| PMA (`00`) | PMA (`00`) | 完整查询 | 由 PMA-MMIO、权限和硬件 misalign enable 决定 |

伪代码：

```text
for each translated byte:
  effective_pbmt = select frozen S1/S2 effective PBMT
  if byte == 0:
    first_pbmt = effective_pbmt
    stop_after_first_page = misaligned && first_pbmt in {NC, IO}
  aggregate PMA/PMP and translation faults as before

emit translation AF/PF/GPF first
if no translation AF/PF/GPF:
  append PMA/PMP AF when present
  misaligned_maps_to_af = first_pbmt == IO or whole access is PMA-MMIO
  if misaligned_maps_to_af:
    append load ? LoadAccessFault : StoreAccessFault
  else if first_pbmt == NC or not all_bytes_pma_pbmt:
    if no PMA/PMP AF or (load and first_pbmt == NC):
      append load ? LoadAddrMisaligned : StoreAddrMisaligned
  else:
    use existing hardware-misalign-enable rule
```

## 4. Software test update

修改：

```text
mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_rm_pbmt_effective_fault.sv
```

当前 cross-page 检查错误地要求 PBMT-NC 和 PBMT-IO 都置 Load Address Misaligned bit 4。必须扩展 helper
或新增等价 Store view，强制覆盖以下四项：

- Load PBMT-NC：期望 bit 4，bit 5 不置位；
- Load PBMT-IO：期望 bit 5，bit 4 不置位；
- Store PBMT-NC：期望 bit 6，bit 7 不置位；
- Store PBMT-IO：期望 bit 7，bit 6 不置位；
- 四项都要求首字节停止、`pa_valid_mask == 8'h01` 且 `uncache_path=1`。

另外增加首页 PBMT-PMA、后续 split 页分别为 NC/IO 的 Load/Store 检查，要求继续解析到后续页并预测
Address Misaligned；增加 PBMT-IO 与已有 AF、PF、GPF 同时出现的 directed 检查，要求只保留既有异常，
不追加 PBMT-IO cause。

增加翻译成功后的物理权限专项：Load PBMT-NC + PMA/PMP Load AF 期望 `0x30`；Store PBMT-NC +
PMA/PMP Store AF 期望仅 bit 7 (`0x80`)。另保留 TLB S1 AF/PF、S2 GAF/GPF + PBMT-NC/IO 的互斥检查，
证明 translation fault 抑制 misaligned，并验证 Load/Store 的 S2 物理权限组合差异。

## 5. 保持边界

- 不修改 RTL/Scala、DUT interface、cfg PBMT 分布或 testcase 激励。
- 不改变 TLB response token、generation、dynamic epoch 和 frozen context 生命周期。
- `first_effective_pbmt` 只从当前 UID 已冻结的 TLB context/entry 选择结果写入，不读取 runtime CSR 或
  DUT exception/status，也不参与修改 token、generation、dynamic epoch。
- 不把 PBMT reserved `11` 当作 NC/IO；继续由现有 consistency 检查拒绝。
- 不用 DUT exception/status 反向驱动 RM expected。
- 不放宽 translation/PMA/PMP readonly query 失败；首字节提前 query 和完整尺寸 query 都必须在只读 API
  返回失败时设置 `RM_LS_ERR_TRANSLATION_NOT_READY` 并停止构造 expected。
- 不改变 aligned PBMT-NC/PBMT-IO 的路径分类。

## 6. 验证计划

1. 运行 `tc_rm_pbmt_effective_fault` software-only专项，强制确认 Load/Store NC/IO 四象限、首页立即停止、
   首页 PMA 后续 NC/IO split 的 Address Misaligned、PBMT-IO 与 translation AF/PF/GPF 的优先级，以及
   PBMT-NC 与 S2 PMA/PMP AF 的 Load 共存、Store AF 抑制 misaligned。
2. 原 testcase/cfg/VSEQ、`seed=212889620`、`MEMBLOCK_MAIN_TRANS_NUM=1000`、
   `MEMBLOCK_CHECK_TRIGGER_EN=0` 原样复测，要求 PASS。
   日志必须确认 UID113 最终 issue 仍绑定原动态实例的 token 183/generation，最终 issue 后完成的 token 236
   不反向替换冻结上下文；本轮不得修改 readonly token resolution、freeze/reissue API 或 dynamic epoch。
3. 原样复测随机发现 seed `399662848`，要求 UID625 expected 从 `0x20` 修正为 `0x30`。
4. `tc_dispatch_real_mmu_sv39_smoke` 至少 10 个新随机 seed，要求全部 PASS；若出现独立问题，按 Buglist
   规则另行闭环。
5. 复跑 PBMT-NC、PBMT-IO、strict-NC、PBMT response-fault 相关 cfg，确认异常优先级没有回归。
6. `git diff --check`、方案 review 和 implementation review 全部通过后独立 commit。

## 7. 验收标准

- 原 seed UID113 的 RM expected 从 `0x10` 修正为 `0x20`，且 DUT 三个观测点保持不变。
- 首字节 PBMT-NC 非对齐仍为 Address Misaligned，首字节 PBMT-IO 非对齐为 Access Fault；首页 PMA、
  后续 split 页为 NC/IO 时为 Address Misaligned。
- Load/Store bit 4/5/6/7 四象限 software-only 专项全部通过。
- TLB translation AF/PF/GPF 抑制 PBMT misaligned；S2 PMA/PMP AF 与 PBMT-NC Load misaligned 可共存，
  但 Store AF 抑制 Store misaligned。
- seed `399662848` 的 UID625 RM expected 为 `0x30`。
- software-only专项、原 seed和随机专项全部通过。
