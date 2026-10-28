# Memblock L2TLB PPN Reuse Smoke 确定性修复 Plan

## 专有名词与抽象功能说明

- **page shape**：L2TLB builder 生成 entry 时选择的页级别和 leaf/non-leaf 形态；本专项只允许
  Sv39 level 0 normal leaf，即 4KB page。
- **reuse history**：responder 保存的已完成 normal response PPN 历史；后续 miss 只有满足目标 entry
  编码和 page-shape 约束时才允许复用。
- **cfg snapshot**：plusarg 经过 `plus.sv` 和 `seq_csr_common` 后，被 sequence 读取的冻结配置视图；
  smoke test 用它确认 preset 确实控制了被测生成逻辑。
- **software directed test**：不依赖 DUT dispatch request stream、直接调用 builder/responder helper 验证
  测试框架算法的 testcase。

`tc_l2tlb_ppn_reuse_smoke.cfg` 的抽象职责：为 PPN reuse software directed test 提供唯一且可重复的
生成条件；它不修改 production builder 算法，也不判断 DUT 行为。

`check_smoke_cfg_snapshot()` 的抽象职责：在 directed 分支开始前验证关键 preset 已经通过配置链被实际
消费；它只做激励配置有效性检查，不替代各分支的 reuse 行为断言。

## 问题与责任边界

当前 sequence 的 same-sample reuse 和 S2 unencodable history 分支都假设目标是 4KB normal leaf，
但 cfg 未关闭 `MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN`。默认 level 随机化会让 builder 偶发生成不满足
reuse 前置条件的目标，或让前后两次 baseline 构造消耗不同随机 shape，从而产生 false FAIL。

责任归类为测试框架 directed cfg 确定性缺陷。production `can_apply_reused_final_ppn()` 对 superpage、
non-leaf 和 S2 不可编码 PPN 的拒绝逻辑正确；本 plan 不修改 RTL、Scala、RM 预测或 production reuse
过滤规则。

## 配置修改 Flow

修改文件：

`mem_ut/ver/ut/memblock/seq/plus_cfg/tc_l2tlb_ppn_reuse_smoke.cfg`

在现有 reuse 参数旁增加：

```text
+MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=0
```

运行时 flow：

```text
读取 tc_l2tlb_ppn_reuse_smoke.cfg
  -> 关闭 level weighted randomization
  -> builder finalize 使用确定的 Sv39 level-0 normal leaf
  -> same-sample 分支只在合法 4KB target 上断言必然 reuse
  -> S2 unencodable 分支的 baseline 不再因随机 page shape 改变
```

保持 `MEMBLOCK_MAIN_MEM_RANGES_EN=0`，继续覆盖 sparse PPN generation；不通过打开 main memory
ranges 绕开原专项目标。

## 配置快照检查 Flow

修改文件：

`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_l2tlb_ppn_reuse_sequence.sv`

在 `check_smoke_cfg_snapshot()` 增加对
`seq_csr_common::get_l2tlb_level_weight_en() == 1'b0` 的断言。若 getter 名称或现有访问方式不同，必须
复用项目已经定义的公共 getter，不新增重复配置字段。

文字伪代码：

```text
进入 PPN reuse smoke sequence
  检查 reuse_en/history_size/reuse_wt
  检查 level_weight_en 等于 0
  检查 main_mem_ranges_en 等于 0
  任一 preset 未生效则立即 fatal，不继续执行可能产生歧义的 directed 断言
```

该检查只证明 cfg 消费链正确，不检查 DUT result、coverage bin 或 RM expected value。

## 验证计划

1. 静态检查：`git diff --check`；确认只改专项 cfg、snapshot checker 和对应文档。
2. 原始 seed：复跑 `1132075201`、`1408263762`，要求正常 `$finish`、`UVM_ERROR=0`、
   `UVM_FATAL=0`。
3. 随机专项：至少 20 个新 seed，要求 20/20 PASS；核对 same-sample reuse 日志存在，并通过
   sequence 既有 `expect_true()` 断言和正常终态证明 S2 unencodable history 没有被选中。production
   candidate-empty 路径当前不打印 rejection 日志，本 plan 不为验证便利新增 production 日志。
4. 邻接检查：运行一个默认 level-randomized L2TLB testcase，确认本专项 cfg 没有改变全局默认值。
5. 生成 implementation review，覆盖 cfg 修改、snapshot checker、验证证据和 plan 一致性。

## 文档与提交

- 更新当前周 `AI_DOC/buglist/rm/v2/rm_buglist_2026-W43_20261019.md` 的本问题状态和验证结果。
- implementation review 放入
  `AI_DOC/plan/test_framework/review_doc/undo/memblock_l2tlb_ppn_reuse_smoke_determinism_implementation_review_20261025.md`。
- 全部验证和 review 通过后，将本 plan 与 implementation review 分别归档到 `do/`。
- 单独提交一个 commit，不混入 PBMT response-fault、CSR redirect invariant 或 regression controller 修改。

## RM 与覆盖率协同支持

本 plan 不修改 RM/checker/scoreboard，也不新增功能覆盖率。same-sample reuse 日志、sequence 既有
`expect_true()` 断言和正常终态作为专项执行证据；后续 coverage 可复用现有 page level 与 PPN reuse
观测点。
