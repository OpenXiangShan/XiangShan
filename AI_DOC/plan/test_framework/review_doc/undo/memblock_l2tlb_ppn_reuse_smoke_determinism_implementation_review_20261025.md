# MemBlock L2TLB PPN Reuse Smoke 确定性修复 Implementation Review

**关联 Plan**：`AI_DOC/plan/test_framework/plan/do/memblock_l2tlb_ppn_reuse_smoke_determinism_plan_20261025.md`
**基线**：`045011bf6b`
**修改类别**：V2 测试框架 directed cfg 与配置有效性检查

## 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 代码落点 | 使用场景/示例 |
|---|---|---|---|
| page shape | L2TLB entry 的页级别和 leaf/non-leaf 形态 | `tlb_map_builder` finalize flow | 本 smoke 只允许 level-0 normal leaf |
| cfg snapshot | plusarg 经公共配置链冻结后供 sequence 读取的值 | `seq_csr_common` getter | sequence 开始时确认 level randomization 已关闭 |
| reuse history | 已完成 normal response 的 PPN 历史 | L2TLB responder history | 合法 4KB target 可选择历史 PPN |
| directed assertion | sequence 内针对确定场景的 `expect_true()` 检查 | `soft_test_l2tlb_ppn_reuse_sequence` | S2 不可编码 history 不得被选中 |

## 修改概述

修改前，专项 cfg 固定 reuse 开关、history size 和权重，但保留默认
`MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=1`。sequence 却把随机 entry 当成确定 4KB target，导致 superpage
或两次不同 page-shape 抽样触发 false FAIL。

修改后，只有该专项 cfg 关闭 level weighted randomization，并在 sequence 入口验证配置值确实通过
`plus -> seq_csr_common` 消费链。production PPN reuse candidate filter、history FIFO、builder 和 DUT/RM
逻辑均未修改。

## 专项 Cfg 修改

抽象功能描述：`tc_l2tlb_ppn_reuse_smoke.cfg` 为 software directed reuse 测试建立可重复的 4KB target
条件；它不改变其他 cfg 的全局默认值，也不放宽 production reuse 规则。

源码位置：`mem_ut/ver/ut/memblock/seq/plus_cfg/tc_l2tlb_ppn_reuse_smoke.cfg`

```text
+MEMBLOCK_L2TLB_PPN_REUSE_EN=1
+MEMBLOCK_L2TLB_PPN_REUSE_HISTORY_SIZE=1
+MEMBLOCK_L2TLB_PPN_REUSE_WT=100
+MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=0
+MEMBLOCK_MAIN_MEM_RANGES_EN=0
```

中文伪代码：testcase 读取专项 cfg 后，保持 PPN reuse 必选和 sparse PPN generation；新增配置只关闭
page level 的权重随机化，使 builder active stage 使用 level 0。其他 testcase 若显式配置 level weight，
仍按各自 cfg 工作。

正确性边界：没有修改 `plus.sv` 默认值，也没有修改 `default.cfg` 或 production builder；因此该变化只
作用于 `tc_l2tlb_ppn_reuse_smoke`。

## 配置快照检查

抽象功能描述：`check_smoke_cfg_snapshot()` 在 directed 分支执行前确认 preset 已被公共配置链消费；
它只检查激励配置有效性，不判断 DUT、RM 或 coverage 结果。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_ppn_reuse_sequence.sv`，
函数：`check_smoke_cfg_snapshot()`

```systemverilog
function void soft_test_l2tlb_ppn_reuse_sequence::check_smoke_cfg_snapshot();
    // 中文注释：专项 cfg 必须经过 plus -> seq_csr_common -> responder 配置快照；
    // 此处不替代后续各定向分支的局部 policy，只验证 testcase preset 的真实消费链。
    expect_true(seq_csr_common::get_l2tlb_ppn_reuse_en() == 1'b1 &&
                seq_csr_common::get_l2tlb_ppn_reuse_history_size() == 1 &&
                seq_csr_common::get_l2tlb_ppn_reuse_wt() == 100,
                "PPN reuse smoke cfg must reach seq_csr_common getters");
    expect_true(responder.ppn_reuse_en == 1'b1 &&
                responder.ppn_reuse_history_size == 1 &&
                responder.ppn_reuse_wt == 100,
                "PPN reuse smoke cfg must be frozen by responder.configure_from_plus");
    expect_true(seq_csr_common::get_l2tlb_level_weight_en() == 1'b0,
                "PPN reuse smoke cfg must force deterministic 4KB page shape");
    expect_true(seq_csr_common::get_main_mem_ranges_en() == 1'b0,
                "PPN reuse smoke cfg must exercise sparse PPN generation");
endfunction:check_smoke_cfg_snapshot
```

中文伪代码：

1. **功能职责**：该函数在 smoke sequence 进入各个 directed 分支之前，验证 reuse policy、responder
   冻结值、page level policy 和 sparse PPN policy 都来自预期 preset；它不生成 entry，也不判断 reuse
   行为是否正确。
2. **顺序控制流**：先调用第一组 `expect_true()` 检查 `seq_csr_common` 中的 reuse enable、history size
   和 reuse weight；不匹配时立即 fatal。随后检查 responder 冻结的同组三个值，避免 plus getter正确但
   responder使用旧配置。第三组读取 `get_l2tlb_level_weight_en()`，要求等于 0；该检查通过后，最后确认
   `get_main_mem_ranges_en()` 仍为 0，保证 sparse PPN 专项目标没有被确定性修复改变。任一检查失败都
   由 `expect_true()` 中止 sequence，后续 directed 分支不执行。
3. **子函数职责**：`seq_csr_common` getters 只读公共冻结配置；`expect_true()` 把 false condition 转为
   `uvm_fatal`，不修改 builder、responder history 或 testcase 状态；`responder` 字段证明
   `configure_from_plus()` 已实际消费 reuse preset。

调用关系：

| 顺序 | 对象 | 当前流程职责 |
|---:|---|---|
| 1 | `plus::load_all()` | 读取 cfg 中的 `MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=0` |
| 2 | `seq_csr_common::init_from_plus()` | 将 plus 值冻结为 sequence 公共配置 |
| 3 | `check_smoke_cfg_snapshot()` | 在 smoke body 开始时 fail-fast 验证 getter 值 |
| 4 | `tlb_map_builder` finalize flow | 在 level randomization 关闭时生成确定 level-0 target |

## 验证结果

- VCS compile：mode `ppn_reuse_determinism_fix_20261025`，成功生成 `simv`；Verdi KDB 报告
  `0 error(s), 0 warning(s)`。
- 原失败 seed：`1132075201`、`1408263762`，2/2 PASS。
- 固定新增 seed：`1701202601`--`1701202620`，20/20 PASS。
- 汇总：22/22 PASS，fail/infra/incomplete/aborted 均为 0；结果文件：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_ppn_reuse_fix_20261025/mem_ut/ver/ut/memblock/sim/ppn_reuse_determinism_fix_20261025_results/summary.log`。
- 每个原失败 seed 均正常打印 `TEST CASE PASSED`，`UVM_ERROR=0`、`UVM_FATAL=0`。
- 邻接 Sv39 level-randomized 检查：首选旧专项 seed `212889620` 在当前基线复现独立的
  `uid462 expected=0x2000/status=0x10` RM mismatch；该 seed 不读取本次 PPN reuse 专项 cfg，已单独
  启动根因分析，不归因于本 issue。随后选用本轮主回归已经 10/10 PASS 矩阵中的 seed `206535281`
  在新 simv 上复跑，1/1 PASS；该 cfg 显式保持 `MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=1`，证明专项 cfg
  修改没有改变全局 level-randomized 路径。结果文件：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_ppn_reuse_fix_20261025/mem_ut/ver/ut/memblock/sim/ppn_reuse_determinism_fix_20261025_adjacent_sv39_pass_results/summary.log`。
- `206535281` 的原始 10/10 PASS 矩阵证据：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/mem_ut/ver/ut/memblock/sim/functional_cfg_cov_closure_clean_20261025_resume_after_controller_fix_results/tc_dispatch_real_mmu_sv39_smoke/summary.log` 和同目录 `results.tsv`。

## Plan 对齐检查

实现与正式 plan 一致：专项 cfg 增加 `LEVEL_WEIGHT_EN=0`，snapshot checker 使用既有 getter，未修改
production reuse 逻辑。验证使用原 2 seed 和 20 个新增 seed；独立 Sv39 mismatch 已隔离，known-pass
邻接 seed 1/1 PASS。

## 实现与 Plan 不一致项

未发现实现与 Plan 不一致项；当前 coding 行为与对应 plan 保持一致。

## Plan 未说明但 Coding 落实的细节

没有新增 plan 未说明的 production 逻辑。编译使用同基线 closure worktree 的只读 `build/rtl` 产物，
只为避免重复生成 RTL；测试框架源码和 `simv` 均在本隔离 worktree 独立编译。

## Review 结论

当前源码改动范围、22-seed 专项结果和 Sv39 邻接 1/1 PASS 满足 plan；独立 implementation review
结论 `REVIEW_PASS`，无 blocking finding。

## 母分支合并复测

2026-10-28 在 clean mother worktree
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028` 上重新编译并复测：

- `tc=tc_l2tlb_ppn_reuse_smoke`、`ts=virtual_base_sequence`、
  `cfg=tc_l2tlb_ppn_reuse_smoke`：原失败 2 seed 与固定新增 20 seed 共 22/22 PASS；
  fail/infra/incomplete/aborted/not_run/remote_unknown 全为 0，raw/caught/demoted UVM ERROR/FATAL 全为 0，
  每个 seed 均有 pass marker 和正常 `$finish`。结果目录：
  `mem_ut/ver/ut/memblock/sim/mother_ppn_reuse_merge_20261028_r2_results/ppn_reuse`。
- 邻接 `tc=basicTest`、`ts=memblock_dispatch_real_smoke_vseq`、
  `cfg=tc_dispatch_real_mmu_sv39_smoke`、`seed=206535281` 按历史参数
  `MEMBLOCK_MAIN_TRANS_NUM=1000`、`MEMBLOCK_CHECK_TRIGGER_EN=0` 精确复测 1/1 PASS；结果目录：
  `mem_ut/ver/ut/memblock/sim/mother_ppn_reuse_merge_20261028_r2_results/adjacent_sv39_exact_1k`。

首次邻接启动遗漏 1K override，按 cfg 默认进入 10K 后由 controller 正常终止并记录为 ABORTED；该非权威
运行未作为验收证据，随后已用原参数重新复测通过。
