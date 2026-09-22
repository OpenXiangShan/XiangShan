# V2 RM fault/PMA cause 建模修复方案

## 术语与抽象功能说明

| 术语 | 含义 | 代码落点 |
|---|---|---|
| `effective S1 AF` | L2TLB response 已明确选择的一级 Load/Store Access Fault | `fault_effective_s1_af` |
| `PMA hit` | PMA 表找到覆盖当前物理地址的 entry | `pma_hit` |
| `C 属性` | PMA entry 的 cacheable 位 | `pma_entry.c`、`normal_cacheable` |
| `Address Misaligned` | 硬件非对齐控制关闭或非 cacheable 路径下的地址未对齐异常 | `loadAddrMisaligned/storeAddrMisaligned` |

## 根因

当前 RM 在 effective S1 AF 已成立时继续根据随机 PTE 权限字段推导 PF，导致 `0x2020`；同时把
PMA C=0/模型状态未决抽象成 `dcache_fact_needed_for_c`，与 V2 PMA 直接决定 C 属性的 RTL 语义不一致，
导致 UID23 误报 `0x10`。

## 修改范围

只修改：

```text
mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_pma_pmp_model.sv
```

不修改 RTL/Scala、DUT 接口、激励 sequence 或本次 safe cfg。

## 具体逻辑

1. S1/S2 已选择 fault 时跳过同阶段 PTE permission fault 推导。
2. PMA hit 后直接使用 C 属性并将 `af_decided` 置 1。
3. PMA miss/权限拒绝直接生成 Access Fault。
4. 删除 DCache C fact 中间状态及 finalize helper。
5. 重新核对 V2 PMA source-order/reverse mapping，保证 `0x80000000` DDR 地址映射为 C=1。

## 首 seed 复测后补充结论

恢复正确 PMA 地址后，S1 AF/PF 叠加误报已消失，但首 seed 仍出现 UID23/88/102/239/251 的 Load
`expected=0x10, actual=0`，以及 UID43/117/169/170/195 的 Store `expected=0x40, actual=0`。
这些访问均为 PMA 命中、权限允许、PBMT=00 的 C=0 区域访问。V2 `LoadUnit.scala` 和 `StoreUnit.scala`
的实际条件表明，PMA C=0 不等同于 PBMT-NC；硬件只在 `s2_in.nc`（PBMT-NC）或实际 MMIO 路径下
产生地址未对齐异常，PMA C=0 本身仍由硬件非对齐处理逻辑拆分完成。

因此将 `observer_should_expect_addr_misaligned()` 的抑制条件从
`hd_misalign && all_bytes_pma_pbmt && all_bytes_normal_cacheable` 改为
`hd_misalign && all_bytes_pma_pbmt`。`all_bytes_normal_cacheable` 继续保留为诊断字段，
不再作为 C=0 非对齐异常的触发条件。该修改只影响 RM 期望构造，不改变 DUT 激励或 RTL。

## 验收标准

首个 seed `710006`：

- UID59/61/77/87 的 expected exception 变为 `0x20`，与 DUT 一致；
- PMA C=0 且 PBMT=00 的非对齐 load/store 不再误报 `0x10/0x40`；
- `UVM_ERROR=0`、`UVM_FATAL=0`；
- 自然结束并显示 `TEST CASE PASSED`。

## 首 seed 修复验证结果

RM 修复版本已自然运行至 `$finish`，100000 请求主流程完成；10 个 PMA C=0 非对齐误报全部消失，
RM 比较错误为 0。日志：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/rm_pma_c0_misalign_fix_run_20260921/log/tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16_seed=710006_rtl.log`。

仿真收尾另出现 1 个测试框架 `UVM_FATAL`（`DISP_MON_BATCH redirect_from_event requires redirect event`），
该错误不属于 RM 比较，也未观察到 RTL 异常；已单独进入测试框架 review，1000 seed 回归暂缓。

首 seed 通过后，再执行 1000 个不同 seed 的 100000 请求回归。遇到 RM 错误则按本 plan 继续定位；
遇到 RTL 候选必须重新开启两轮独立 RTL review，确认后才记录并停止。
