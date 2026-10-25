# MemBlock RM PBMT-IO 非对齐 Access Fault 方案首轮评审

## 1. 评审范围与术语

评审对象：`AI_DOC/plan/rm_plan/do/memblock_rm_pbmt_io_misaligned_access_fault_plan_20261024.md`。

| 术语 | 含义 | 代码落点 |
| --- | --- | --- |
| `first_effective_pbmt` | 访问首字节由冻结 TLB context/entry 选出的最终 PBMT 类型 | `observer_build_commit_item()` 待新增局部状态 |
| `all_bytes_pma_pbmt` | 已解析字节是否全部为 PBMT-PMA 的整笔聚合状态 | `observer_build_commit_item()` 现有局部状态 |
| split access | 非对齐访问被 MisalignBuffer 拆成多个页内子访问 | Load/Store MisalignBuffer |
| exception cause | RM 写入 `expected_exception` 的架构异常 bit | `rm_ls_program_item_t.expected_exception` |

## 2. 首轮结论

**REVIEW_FAIL**。

责任分类和主修复方向正确：首字节 PBMT-IO 非对齐应预测 Load/Store Access Fault bit 5/7，首字节
PBMT-NC 应预测 Address Misaligned bit 4/6；已有 AF/PF/GPF 必须优先，且不得修改 DUT 激励、TLB token、
generation、dynamic epoch 或冻结上下文生命周期。

但首轮计划存在两个 blocker，不能直接进入 coding：

1. 未定义“首页 PBMT-PMA、后续 split 页为 PBMT-NC/PBMT-IO”的行为。该路径由 MisalignBuffer 将
   uncache split 统一形成 Address Misaligned，不能只凭首字节 PBMT 决定全部跨页 cause。
2. Store PBMT-NC/PBMT-IO bit 6/7 的 directed 验证被写成可选项，不能验收共享 Load/Store RM 分支。

## 3. 必要修订

- `first_effective_pbmt` 只决定首字节 NC/IO 是否立即终止，以及立即终止时的 cause。
- 保留 `all_bytes_pma_pbmt`；首页 PMA、后续 split 页为 NC/IO 时预测 Address Misaligned。
- software-only 专项强制覆盖 Load NC/IO 和 Store NC/IO 四象限。
- 四象限都检查 exception bit 互斥、首字节停止、`pa_valid_mask` 和 `uncache_path`。
- 增加首页 PMA、后续 NC/IO 的 Load/Store 跨页检查。
- 增加 PBMT-IO 与 AF/PF/GPF 同时出现的优先级检查。

## 4. 已通过项与剩余风险

已通过核查：PBMT-NC/IO 的首字节 cause 方向、AF/PF/GPF 先构造后以零异常门控新增 cause、Store cause
bit 方向以及不修改 DUT/RTL/token lifetime 的边界。

剩余风险是跨页 mixed-PBMT 行为若未按 DUT MisalignBuffer 实际路径建模，会把后续页 PBMT-IO 错误提升为
Access Fault；Store 未做 directed 验证则可能出现 Load 正确、Store bit 错位而未被发现。

## 5. 修订状态

计划已按本评审补充跨页矩阵、Load/Store 四象限、首页 PMA 后续 NC/IO 以及 AF/PF/GPF 优先级专项。
修订后必须再次进行独立 review；复审通过前不得修改 RM 源码。

## 6. 修订方案复审

独立只读 reviewer `Pauli` 于 `2026-10-24` 完成复审，结论 **REVIEW_PASS**，无 blocker。复审确认：

- 跨页矩阵和 `first_effective_pbmt` / `all_bytes_pma_pbmt` 分工完整；
- Load/Store bit 4/5/6/7 四象限为强制专项；
- AF/PF/GPF 保持先于 PBMT misaligned cause；
- 不修改 token、generation、dynamic epoch、frozen context 或 DUT 激励。

复审后的实现约束进一步明确：`first_effective_pbmt_valid` 默认 0，仅 byte 0 选择成功后置位；原 seed 日志
必须核对 token 183/236 的 issue 前后关系，避免把 RM cause 修复误写成 token 生命周期修改。

## 7. 随机复测后第二轮修订

`seed=399662848` 的 FSDB 证明首轮方案中的“所有 Access Fault 都抑制 misaligned”不成立。UID625 在
`155800.700ns` 同时满足 PBMT-NC misaligned 与 `io_pmp_ld=1`，LoadUnit 独立生成 bit 4 和 bit 5，DUT
status 为 `0x30`。该 Access Fault 来自翻译成功后的 S2 PMA/PMP，而不是 TLB translation AF。

第二轮方案必须拆分：

- TLB S1/S2 AF/PF/GPF：抑制 misaligned，且禁止使用不可信 PMA/PMP response；
- S2 PMA/PMP AF：Load 与 PBMT-NC misaligned 可共存；Store AF 抑制 storeAddrMisaligned；
- PBMT-IO misaligned：映射到同一个 AF bit，不增加 misaligned bit。

第二轮独立 review 需重新检查 Load/Store RTL 和 directed matrix；首轮 `REVIEW_PASS` 不覆盖该新增组合。

## 8. 第二轮 review blocker 与修订

独立 reviewer `Cicero` 于 `2026-10-25` 对第二轮方案和实现检查，结论 `REVIEW_FAIL`。Blocker 为：

- PBMT-IO 已映射为 AF 后仍可能追加 Address Misaligned；
- Load 的 S2 PMA/PMP AF 共存条件过宽，只有 PBMT-NC misaligned 可共存；
- 首字节 PMA/PMP readonly query 失败没有传播；
- 方案伪代码未表达 Store AF 抑制 storeAddrMisaligned。

实现已新增 `misaligned_maps_to_access_fault`，只允许 PBMT-NC Load 与 S2 PMA/PMP AF 共存；Store AF
抑制 bit 6；PBMT-IO/PMA-MMIO 不再追加 misaligned；首字节 query 失败返回
`RM_LS_ERR_TRANSLATION_NOT_READY`。修订后重新提交最终独立复审。

## 9. 最终 implementation review

独立 reviewer `Hegel` 于 `2026-10-25` 完成最终只读复审，结论 **REVIEW_PASS**，无 blocker：

- directed matrix 覆盖 Load/Store × PBMT-NC/IO × S1 AF/S1 PF/S2 GAF/S2 GPF 共 16 项；
- S1-only 与 S2-only fault stage、expected exception vector 和 PBMT misaligned 抑制语义正确；
- readonly API 读取 completed payload 并进入 RM production commit path；
- translation AF/PF/GPF 会阻止 PMA/PMP 查询并抑制 PBMT misaligned；
- 未修改 RTL、token、generation、dynamic epoch 或 frozen context 生命周期。

剩余非 blocker 风险：矩阵直接构造 effective fault，未单独覆盖 raw fault 权重仲裁、`s2xlate=3` 和错误
`fault_stage_selected` 负向测试；最终集成随机测试已补充 response wiring 与 production path 证据。

## 10. 最终验证结论

最终 mode `rm_pbmt_final_20261025` 重新编译成功。software-only directed seeds `2026102501,2026102502`
为 `2/2 PASS`：

```text
mem_ut/ver/ut/memblock/sim/rm_pbmt_final_20261025_results/directed_20261025_131035/
```

原始/历史 seeds `212889620,399662848` 加 10 个新随机 seeds 共 `12/12 PASS`，且
`fail=0 infra=0 incomplete=0 aborted=0 remote_unknown_jobs=0`：

```text
mem_ut/ver/ut/memblock/sim/rm_pbmt_final_20261025_results/random12_20261025_131313/
```

关键日志核验：

- seed `212889620`，UID113 expected/DUT exception 均为 `0x20`，`result=PASS`；
- seed `399662848`，UID625 expected/DUT exception 均为 `0x30`，`result=PASS`。

本问题满足计划验收标准，结论 **FINAL PASS**。
