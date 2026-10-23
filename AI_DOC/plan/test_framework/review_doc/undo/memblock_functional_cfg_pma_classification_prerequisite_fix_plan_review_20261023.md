# memblock functional cfg PMA/PMP 分类前置条件修复计划 Review

## 1. 术语与抽象功能说明

| 术语 | 当前文档中的中文含义 | 对应落点 | 使用场景 |
|---|---|---|---|
| `classification incomplete` | RM 无法把访问归入 normal-cacheable、MMIO 或 uncache 的 fatal | `memblock_rm.sv` 的 load 分类检查 | 原回归 130 个 seed |
| `level weight` | L2TLB 页级随机权重控制，要求 sparse memory mode | `MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN` | direct smoke 默认打开 |
| `configuration prerequisite` | 运行某模型/sequence 前必须满足的配置组合 | plus cfg preset | PMA/PMP model 必须打开 |
| `first error` | 一个 seed 中最早能确定责任边界的错误 | 原始 sim log | 配置修复后允许暴露新的独立首错 |

## 2. Review 范围与依据

关联计划：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/AI_DOC/plan/test_framework/plan/do/memblock_functional_cfg_pma_classification_prerequisite_fix_plan_20261023.md`

reviewer：独立只读 Codex reviewer；日期：`2026-10-23`。

依据包括原始回归日志、18 个 cfg diff、`plus.sv` 默认值、`seq_csr_common.sv` 配置组合检查和已完成
的 60 个 seed 专项结果。

## 3. 主要 Review 发现

初版修改把九个 direct cfg 设置为 `MAIN_MEM_RANGES_EN=1`，与默认
`L2TLB_LEVEL_WEIGHT_EN=1` 冲突，会在 0ns 触发 `SEQ_CSR_CFG` fatal。计划已修订为：

- direct/sfence 场景：`MAIN=0, PMA=1`。
- 固定范围场景：`MAIN=1, PMA=1`，并在 CSR random cfg 显式设置 `LEVEL_WEIGHT=0`。
- 移除 `default.cfg` 中无关的 dispatch issue 开关变更。

`safe_nocross16` 的 `MAIN=1, PMA=1` 与场景目标一致；五个 BasicTest cfg 的 `MAIN=0, PMA=1`
已由 50/50 seed 通过证明可执行。

## 4. 正确性检查

| 检查项 | 结论 |
|---|---|
| 是否改变 DUT RTL | 否 |
| 是否改变 transaction 数或类型 | 否 |
| 是否屏蔽 RM 错误 | 否，补齐分类数据源后继续原 compare |
| 是否存在 level-weight/main-range 非法组合 | 修订后不存在 |
| 是否覆盖派生 cfg 遗漏 | 是 |
| 是否遗漏 CSR random 的前置条件 | 修订后已覆盖 |

## 5. Review 结论

修订后的方案改动最小，责任边界为测试框架配置，不会掩盖 RTL/RM 后续首错。计划通过 review，
可进入 cfg coding 和专项仿真。专项测试出现其他错误时必须独立登记，不能回退本配置修复或把新错误
归入 classification 问题。
