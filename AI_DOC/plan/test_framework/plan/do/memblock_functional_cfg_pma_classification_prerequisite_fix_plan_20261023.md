# memblock functional cfg PMA/PMP 分类前置条件修复计划

## 1. 术语与抽象功能说明

| 术语 | 当前文档中的中文含义 | 代码或配置落点 | 示例 |
|---|---|---|---|
| `main memory ranges` | 由测试框架显式维护的主存物理地址范围，用于给 RM 提供 normal-cacheable 分类依据 | `MEMBLOCK_MAIN_MEM_RANGES_EN` | `default.cfg` 使用值 `1` |
| `PMA/PMP model` | 测试框架运行时地址属性与权限模型，用于生成 PMA/PMP 分类和异常预期 | `MEMBLOCK_PMA_PMP_MODEL_EN` | 本轮所有受影响 cfg 使用值 `1` |
| `sparse memory mode` | 不启用固定主存范围，由页表/地址模型在稀疏空间内工作 | `MEMBLOCK_MAIN_MEM_RANGES_EN=0` | direct smoke 与 sfence cfg |
| `classification` | RM 对 load/store 路径在 normal-cacheable、MMIO、uncache 之间的互斥分类 | `memblock_rm::reference_model()` | 普通 cacheable load 必须命中 normal-cacheable |
| `derived cfg` | 在其他场景基础上追加约束的派生 preset | `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16.cfg` | 禁止跨 16-byte 边界 |

本计划属于测试框架配置修复，不修改 DUT、RTL、RM 算法或激励 transaction 内容。

## 2. 问题与目标

`functional_cfg_cov_20261023` 中共有 130 个已实际运行的 seed 报
`Load path classification incomplete`。这些 cfg 没有显式打开 `PMA/PMP model`，RM 无法建立
normal-cacheable 分类。`safe_nocross16` 派生 cfg 还同时遗漏主存范围使能。

目标：

1. 所有依赖 RM 地址分类的 functional cfg 显式打开 `MEMBLOCK_PMA_PMP_MODEL_EN`。
2. 固定主存范围场景使用 `MAIN_MEM_RANGES_EN=1`；保留 level-weight 的 direct/sfence 场景使用
   sparse memory mode，即 `MAIN_MEM_RANGES_EN=0`。
3. `tc_memblock_csr_random_config` 同时关闭 level-weight，使其合法使用固定主存范围。
4. 不修改 request 数、dispatch 模式、异常权重或其他场景语义。

## 3. 配置修改矩阵

### 3.1 固定主存范围

以下 cfg 使用：

```text
+MEMBLOCK_MAIN_MEM_RANGES_EN=1
+MEMBLOCK_PMA_PMP_MODEL_EN=1
```

- `default.cfg`
- `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16.cfg`
- `tc_memblock_csr_random_config.cfg`

`tc_memblock_csr_random_config.cfg` 另增加：

```text
+MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=0
```

### 3.2 sparse memory mode

以下 cfg 使用：

```text
+MEMBLOCK_MAIN_MEM_RANGES_EN=0
+MEMBLOCK_PMA_PMP_MODEL_EN=1
```

- `csr_sfence_check_store_manual_control.cfg`
- `csr_sfence_check_store_rob_control.cfg`
- `tc_dispatch_real_cancel_reconcile_smoke.cfg`
- `tc_dispatch_real_l2cache_model.cfg`
- `tc_dispatch_real_mixed_sta_wb_smoke.cfg`
- `tc_dispatch_real_mixed_wb_smoke.cfg`
- `tc_dispatch_real_multi_store_wb_smoke.cfg`
- `tc_dispatch_real_smoke.cfg`
- `tc_dispatch_real_smoke_flushsb.cfg`
- `tc_dispatch_real_store_smoke.cfg`
- `tc_dispatch_real_store_sta_wb_smoke.cfg`
- `tc_dispatch_real_store_wb_smoke.cfg`
- `tc_sfence_stress_10k.cfg`
- `tc_sfence_stress_1k.cfg`

`tc_l2tlb_pbmt_response_fault.cfg` 保持其既有 `MAIN_MEM_RANGES_EN=0`，只把 PMA/PMP model 打开。

## 4. 执行 Flow

1. cfg 解析后，以 preset 中的显式值覆盖 `plus.sv` 默认值。
2. `seq_csr_common` 检查 `LEVEL_WEIGHT_EN && MAIN_MEM_RANGES_EN` 的非法组合；本计划中的 sparse
   场景保持 `MAIN=0`，固定范围场景明确关闭 level-weight。
3. PMA/PMP model 为每个有效物理字节提供属性，RM 据此完成 normal-cacheable/MMIO/uncache 分类。
4. 分类完成后继续进入现有 compare，不添加错误屏蔽或提前 PASS 条件。

## 5. 验证计划

1. 对 30 个 functional regression cfg 执行 runner `--dry-run`，确认 cfg 文件、testcase、vseq、
   plusarg 和组合检查全部可解析。
2. 复跑原 `safe_nocross16` 10 个 seed，要求 10/10 PASS。
3. 复跑五个 BasicTest cfg 各 10 个 seed，要求 50/50 PASS。
4. 对九个 direct cfg 和 `tc_memblock_csr_random_config` 复跑原回归 seed，确认不再出现
   `classification incomplete` 或 0ns cfg fatal；若进入新的独立首错，按新 Buglist 闭环处理。
5. cfg 闭环后恢复完整 functional regression。

## 6. 风险边界

- 不把 direct cfg 改为 `MAIN=1`，避免与默认 `L2TLB_LEVEL_WEIGHT_EN=1` 形成 0ns fatal。
- 不改变 `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN` 等无关激励开关。
- 打开 PMA/PMP model 只补齐 RM/框架已有地址属性数据源，不改变 DUT 输出。
- 后续暴露出的 exception、owner、queue depth 或 responder 问题属于独立问题，不在本计划中屏蔽。

