# MemBlock sparse PPN manager 可达性修复计划

> 2026-11-02 后续实现说明：本文保留旧B08固定512GiB窗口方案与其历史验证。当前扩展方案恢复安全cacheable候选，并按观测PMA版本与实际外部路由为NC/IO/MMIO选择合法窗口；小窗口由上游偏移意图专项覆盖。后续设计及验证以 `AI_DOC/plan/test_framework/plan/do/memblock_l2tlb_routed_ppn_coverage_plan_20261102.md` 及对应implementation review为准，不把旧PASS当新方案验收。

## 1. 问题、责任与边界

`tc_dispatch_real_smoke` 在 `MEMBLOCK_MAIN_MEM_RANGES_EN=0` 时由
`tlb_map_builder::make_canonical_ppn()` 使用 `root_ppn ^ vpn ^ salt` 生成全物理地址空间的
sparse PPN。该算法只保证确定性和页级对齐，不保证最终物理地址被 MemBlock 当前 TileLink
拓扑中的任一 manager 接收。

历史 seed `1242164593` 的 UID462、VPN `0x8d2f5` 生成 PPN `0x31136`，最终访问
`0x311368c0`。该地址位于 `SoC.APLICRange=0x31100000-0x3113ffff` 的粗粒度保留区内，
但实际只实例化两个 32 KiB APLIC domain：

- M domain：`0x31100000-0x31107fff`
- S domain：`0x31120000-0x31127fff`

因此 `0x311368c0` 落在 `0x31128000-0x3113ffff` 的无 manager 空洞。Uncache 的输入
`TLBuffer_22` 有空间，所以先接受 A request；下游 `TLXbar_6` 的 out0/out1 route predicate
均为 0，`auto_in_a_ready` 永久为 0，最终占住 source0 和 LoadQueueUncache slot14。

责任分类为测试框架地址生成问题，不修改 RTL/Scala。禁止增加 TB fallback responder；fallback
会绕过真实 manager decode、D-channel 仲裁和 source ownership，掩盖地址拓扑问题。若产品要求
空洞返回 denied，应由 RTL/Diplomacy 增加 `TLError` manager，超出本轮授权范围。

## 2. 修复方案

修改文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv
mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_ppn_reuse_sequence.sv
mem_ut/ver/ut/memblock/seq/plus_cfg/tc_l2tlb_ppn_reuse_smoke.cfg
```

### 2.1 sparse PPN 生成

固定选择外部 memory manager 和 cacheable PMA 同时覆盖的 512 GiB 窗口：

```text
base  = 0x0080_0000_0000
range = 0x0080_0000_0000
limit = 0x00ff_ffff_ffff
```

该窗口满足：

1. 高于 `0x80000000`，属于 `HaveAXI4MemPort.memRange`。
2. 低于 PMA cacheable 上限 `0x80000000000`。
3. base/range 均按 512 GiB 对齐，可在 level 0-3 对齐后保持完整 page shape 可服务。
4. 仍由 responder 稀疏分配 backing，不启用固定 main-memory table。

源码级伪代码：

```systemverilog
if (MAIN_MEM_RANGES_EN) begin
    keep existing configured-range mapping;
end else begin
    page_count = SPARSE_RANGE >> 12;
    page_offset = deterministic_hash(root_ppn, vpn, s2) % page_count;
    ppn = (SPARSE_BASE >> 12) + page_offset;
end
apply existing level alignment to ppn;
```

修改不改变 `MAIN_MEM_RANGES_EN=1` 路径，不引入 MMIO 随机命中；MMIO 地址覆盖应由显式 directed
场景产生，而不是依赖全空间 hash 偶然命中。

### 2.2 directed 软件检查

扩展已有 `soft_test_l2tlb_ppn_reuse_sequence`，并在其 cfg 显式设置
`MEMBLOCK_MAIN_MEM_RANGES_EN=0`。检查：

- 历史 VPN `0x8d2f5` 的结果确定性。
- S1 level 0-3 均位于 sparse DRAM 窗口。
- 2 MiB、1 GiB、512 GiB 对齐保持正确。
- S2 sparse PPN 同样位于可路由窗口。
- 不再生成历史 APLIC hole page `0x31136000`。

## 3. 验证门禁

1. `git diff --check` 和全量 VCS compile 通过。
2. software directed `tc_l2tlb_ppn_reuse_smoke` 通过。
3. 历史 seeds `1242164593,165489856,898041954,37617171` 在原 1000 transaction 条件下通过。
4. 从 HEAD 创建隔离 worktree，只应用上述三个文件，重新编译 directed 和历史代表 seed，排除
   exact-owner、retry guard 和诊断 XMR 对结果的影响。
5. `tc_dispatch_real_smoke` 至少 20 个新随机 seed 通过。
6. 删除定位用 `UNCACHE_SLOT14_SOURCE0_TRACE` 和 TLXbar XMR；不把诊断代码纳入本问题 commit。
7. 独立 reviewer 确认根因、方案和风险边界后再提交。

## 4. 风险与检查

- 低地址随机 MMIO 覆盖会下降；由独立 MMIO directed cfg 负责，不允许恢复全空间 hash。
- superpage/NAPOT 必须在 manager-safe 映射后继续执行既有对齐和编码；directed 覆盖 level 0-3，
  后续随机回归继续覆盖 NAPOT。
- PPN reuse 只能复用本修复后产生的可服务 PPN；本轮现有 reuse history 从当前运行内完成记录，
  不读取跨仿真的旧持久化 PPN。
- 本轮不修改 DUT 地址图，也不把 PMA 粗粒度范围当作 TL manager 真值。
