# memblock functional cfg PMA/PMP 分类前置条件修复 Implementation Review

## 1. 术语与抽象功能说明

| 术语 | 当前文档中的中文含义 | 对应对象 | 示例 |
|---|---|---|---|
| `preset` | 一个 testcase/vseq 使用的 plus cfg 配置集合 | `seq/plus_cfg/*.cfg` | `tc_dispatch_real_smoke.cfg` |
| `sparse memory mode` | 主存范围不开启、由稀疏页表地址空间工作的模式 | `MAIN_MEM_RANGES_EN=0` | direct smoke |
| `classification gate` | RM 在 compare 前要求访问路径唯一分类的检查 | `classification incomplete` | 修复后不再命中 |
| `next first error` | 修复当前首错后暴露的下一独立错误 | cancel depth、ROB admission prefix | 不属于本 cfg 修复 |

## 2. 功能特性与修改前逻辑

关联 plan：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/AI_DOC/plan/test_framework/plan/do/memblock_functional_cfg_pma_classification_prerequisite_fix_plan_20261023.md`

修改前，受影响 preset 依赖 `plus.sv` 的 `PMA_PMP_MODEL_EN=0` 默认值。RM 没有逐字节 PMA/PMP
属性，普通访问可能同时不属于 normal-cacheable、MMIO 和 uncache。`safe_nocross16` 还遗漏固定主存
范围，CSR random 则缺少合法的 `LEVEL_WEIGHT=0/MAIN=1` 组合。

## 3. 修改后逻辑

### 3.1 固定主存范围 cfg

`default`、`safe_nocross16` 和 CSR random 显式使用 `MAIN=1/PMA=1`。CSR random 同时设置
`L2TLB_LEVEL_WEIGHT_EN=0`，避免框架组合检查 fatal。

### 3.2 sparse memory cfg

direct smoke、store、mixed、cancel 和 sfence preset 显式使用 `MAIN=0/PMA=1`。这种组合保留既有
level-weight 地址生成，同时为 RM 打开 PMA/PMP 属性模型。

### 3.3 PBMT response fault cfg

该 directed cfg 保持 `MAIN=0`，只将 PMA/PMP model 从 0 改为 1。它仍有独立的 superpage fault
场景设计问题，本轮没有修改或屏蔽其 checker。

## 4. 正确性检查

| 检查 | 结果 |
|---|---|
| 30 个 functional cfg runner dry-run | 全部通过 |
| `safe_nocross16` 原 10 seed | 10/10 PASS |
| 五个 BasicTest cfg 原 seed | 50/50 PASS |
| direct cfg 修正后首错 | 已越过 classification，暴露 cancel depth/STD 等独立问题 |
| CSR random 原 10 seed | 全部越过旧 cfg gate，暴露独立 `ROB_ADMISSION_PREFIX` 问题 |
| classification/config fatal 复查 | 新专项完成日志中未再命中 |

已验证结果目录：

- `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/functional_cfg_pma_fix_retest_20261023_results/tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_safe_nocross16_all10`
- `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/basicTest_main_ranges0_retest_20261023_results`
- `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/mem_ut/ver/ut/memblock/sim/cfg_pma_prereq_fix_20261023_results_r2`
- `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/mem_ut/ver/ut/memblock/sim/cfg_pma_prereq_fix_20261023_csr_random`

## 5. 调用关系与风险边界

| 顺序 | 对象 | 功能 |
|---|---|---|
| 1 | plus cfg preset | 提供 testcase 的显式配置值 |
| 2 | `plus.sv` | 解析并发布最终 plusarg |
| 3 | `seq_csr_common` | 检查 level-weight/main-range 组合合法性 |
| 4 | PMA/PMP model | 为 UID/PA 提供冻结属性 |
| 5 | RM | 完成路径分类和原有 compare |

实现不修改 DUT 激励接口，不调整 queue/owner/redirect 生命周期，也不把后续首错改成 warning。

## 6. Plan 对齐检查

实现与 plan 的配置矩阵一致。没有 `IMPLEMENTATION_DELTA`，没有额外参数、字段或源码修改。

## 7. 实现与 Plan 不一致项

无。

## 8. Plan 未说明但实现补充的细节

无。`default.cfg` 的 dispatch issue 开关保持基线值 0；需要 real-dispatch 的专项由命令行临时打开，
避免把无关行为混入本提交。

## 9. Review 结论

cfg 前置条件修复通过实现 review，可以归档 plan 并独立提交。当前 full regression 仍有独立的 cancel、
STD owner、PBMT、ROB admission 等问题，必须按各自 Buglist 闭环后才能宣称全量通过。

