# memblock RM 标量非对齐异常语义修复方案（2026-09-05）

| 项目 | 内容 |
| --- | --- |
| 状态 | coding 已完成，专项回归进行中。 |
| 方案类型 | RM plan。 |
| 目标版本 | V2。 |
| 关联问题 | [`memblock_rm_scalar_misalign_exception_mismatch_analysis_20260905.md`](../../../analysis/framework_design/memblock_rm_scalar_misalign_exception_mismatch_analysis_20260905.md)。 |
| 修改授权 | 用户已明确要求按分析方案修改 RM 并重跑同一 testcase；RTL 保持不修改。 |
| 验收场景 | `basicTest` / `memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq` / `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k` / seed `666666`。 |

## 1. 术语与职责

| 术语 | 中文含义 | 代码落点 |
| --- | --- | --- |
| 有效 PBMT | DTLB response 实际给 Load/Store 的 PBMT，不等同于任意一个 raw stage 字段。 | V2 `TLB.pbmt_check()` 的 stage 选择语义。 |
| 普通可缓存事实 | PBMT=`00` 且每个实际 PA byte 均经 PMA 判定 C=1、无对应 AF 的值型结果。 | PMA/PMP readonly view；RM 逐 byte 合取。 |
| 硬件非对齐处理 | 由冻结 `hd_misalign_ld_enable/st_enable` 表示的 V2 MAB 处理能力。 | `tlb_request_context_view_t`。 |
| 保守 fallback | 当 RM 无法证明访问是普通可缓存内存时，保留旧的 address-misaligned 期望。 | RM exception 构建路径。 |

本方案只改变 scalar Load/Store 的 `expected_exception[4]` 和 `expected_exception[6]` 的生成条件。它不修改
TLB entry、CSR snapshot、主表、DUT transaction 或 RTL。

## 2. 目标与边界

### 2.1 目标

1. 普通可缓存且硬件非对齐处理已使能的 scalar 非对齐访问，不再被 RM 误判为 bit4/bit6。
2. 继续保留 NC/IO/PMA 非缓存、未知 PMA 事实和已有翻译/访问 fault 的保守处理。
3. V2 allStage 时以 TLB response 相同 PBMT 选择规则判断，而不根据测试主表 `PBMT` 字段猜测。
4. 新逻辑位于每次 commit 的既有最多 8 byte 翻译循环及其已有 PMA/PMP 调用内，不新增全表扫描。

### 2.2 明确不做

- 不修改 Scala/Chisel、生成 Verilog 或任何 RTL 信号。
- 不以 writeback 的 `debug.isMMIO/isNCIO` 或最终 exception 反推 RM expectation。
- 不改变 PBMT/PBMTE fault overlay、PMA/PMP access-fault、Load data 或 Store cache 的原有职责。
- 不在本轮固化 `HD_MISALIGN_*_ENABLE=0` 的普通内存行为；该模式需独立 directed 验证后再扩展。

## 3. 设计

### 3.1 PMA/PMP 窄事实

**抽象功能描述：** PMA/PMP model 在保持 AF-only 主 contract 的前提下，向 RM 返回一个只用于
scalar 非对齐语义的 `normal_cacheable` 值型事实。它不暴露 DUT 路由输出，也不要求 RM 比较 MMIO/NCIO。

在 `pma_pmp_af_view_t` 中增加 `normal_cacheable`。`make_base_af_view()` 仅在 PMA/PMP evaluation
有效且 translation eligible 时将其设为 `result.cacheable`。PMA/PMP model 关闭、translation 不可用或
任何 query miss 时该事实为 0，RM 使用保守 fallback。完整尺寸查询仍负责原有 AF 期望；为避免首地址
代表整笔访问，RM 另外对 `pa_by_byte[0:size_bytes-1]` 逐个发起 `size_bytes=1` 的 readonly query，
只有所有 byte 的 view 都有效、`af_decided=1`、无对应 load/store AF 且 `normal_cacheable=1` 时才形成
`all_bytes_normal_cacheable=1`。

### 3.2 有效 PBMT helper

**抽象功能描述：** RM helper 从已验证形状的 readonly TLB entry 和 UID 冻结 `s2xlate` 计算 TLB
response 可见的 PBMT；它不读当前 CSR、不修改 entry，也不查找额外表项。

伪代码：

```text
S1-only 或无二阶段：返回 S1 PBMT
S2-only：返回 S2 PBMT
allStage：S1 PBMT 非零时返回 S1，否则返回 S2
stage 形状与 s2xlate 不一致：返回 RM entry inconsistency
```

这与 V2 `TLB.pbmt_check()` 的 mux 保持一致。逐 byte TLB 循环将所有有效 PBMT 是否为 `00` 聚合为
`all_bytes_pma_pbmt`，保证跨页访问不会只根据首字节错误放宽。

### 3.3 地址异常 gate

**抽象功能描述：** `observer_should_expect_addr_misaligned()` 根据地址几何、冻结 CSR、有效 PBMT
聚合和 PMA 窄事实决定是否仍需要 RM 生成 bit4/bit6。它不改变 PF/GPF/AF 优先级，也不修改状态表。

伪代码：

```text
若地址自然对齐：返回 false
若已有 translation/access exception：调用方不进入本 gate
若冻结 hd_misalign 对应访问方向为 1，且所有 byte 的 effective PBMT=00，
且所有 byte 的 PMA/PMP normal_cacheable 都为 1：返回 false
其余情况：返回 true
```

因此，本轮只消除已证明的普通可缓存假阳性；未知或非普通路径继续保持现有保护强度。

## 4. 实施文件与顺序

| 顺序 | 文件 | 修改 |
| --- | --- | --- |
| 1 | `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_pma_pmp_model.sv` | 在 AF view 增加 `normal_cacheable` 并在 `make_base_af_view()` 填充。 |
| 2 | `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv` | 更新 readonly view 的职责注释，说明该窄事实仅供 RM 标量非对齐语义使用。 |
| 3 | `mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv` | 增加有效 PBMT 和 address-misaligned gate helper；在既有 byte loop 聚合 PBMT，在已有 PMA/PMP call 后替换无条件 bit4/bit6 写入。 |
| 4 | `AI_DOC/analysis/framework_design/...analysis_20260905.md` | 回填实际实现、复跑结果和后续错误分类。 |
| 5 | `AI_DOC/plan/rm_review_doc/undo/...implementation_review_20260905.md` | 记录实现 review、plan 对齐与验证。 |

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] 逐 byte PMA/PMP 证明

- 来源：subagent review 指出首地址 PMA 查询不能证明跨 PMA/PMP 边界的后续字节。
- 原 plan：把完整访问 PMA/PMP view 的 `normal_cacheable` 作为放宽 gate 的事实。
- 实现调整：保留完整尺寸 query 的既有 AF 语义，并对每个已解析 PA 以单字节 query 获取
  `normal_cacheable`、`af_decided` 和方向相关 AF，逐 byte 合取后传入
  `observer_should_expect_addr_misaligned()`。
- 原因：只有整笔访问的所有字节都被证明为普通可缓存，才能安全抑制 bit4/bit6；跨区域、未知
  或后续 query 失败时必须回到保守期望。
- 影响范围：仅 `memblock_rm.sv` 的 commit-item 构建路径和本计划的诊断字段；不改变 RTL、
  TLB entry 或完整尺寸 PMA/PMP exception 计算。

## 5. 验证计划

1. 静态检查：`git diff --check`，并检索确认旧的无条件 `if (item.expected_exception == '0 && misaligned)` 不再存在。
2. 使用新独立 mode 编译并运行同一 1000 笔 testcase，关闭本轮已知 trigger output check 以隔离 RM：

```text
make eda_compile tc=basicTest ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq \
  mode=rm_scalar_misalign_20260905 cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k

make eda_batch_run tc=basicTest ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq \
  mode=rm_scalar_misalign_20260905 cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k \
  seed=666666 plus_arg='+MEMBLOCK_MAIN_TRANS_NUM=1000 +MEMBLOCK_CHECK_TRIGGER_EN=0'
```

3. 若无 RM/RTL 错误，再按同一 cfg 和 seed 运行完整 100000 笔；若有新错误，先依据日志和波形重新分类。

## 6. 验收不变量

1. UID0 的 expected exception 从 `0x40` 改为 `0x0`，且数据/Store 结果仍被比较。
2. 所有原有 translation、permission、PBMT overlay 和 PMA/PMP AF bit 不被清除或降级。
3. `normal_cacheable=0` 或硬件非对齐处理关闭时，不产生本方案的放宽。
4. 每个 commit 最多继续遍历已有的 8 个 byte；不新增 UID/TLB/history 扫描。
5. RTL 文件保持零改动。
