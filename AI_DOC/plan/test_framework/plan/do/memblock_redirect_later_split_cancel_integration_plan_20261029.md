# Redirect 集成后恢复 later-split 翻译记录取消

日期：2026-10-29。源码基线：`4948d291e`；工作区：`.codex_mother_integration_20261028`。
状态：已完成。旧版本定向复现、修复后编译/定向、真实 DUT 39-seed 全部完成，独立最终 review 为
`REVIEW_PASS`；本方案归档至 `plan/do`，与对应 Bug 一并形成独立提交。

方案审查：独立 reviewer `Galileo`（`01a1815b-d3fc-7c41-84b6-6fbe81f2332e`）于 2026-10-29 给出
`PLAN_REVIEW_PASS`；要求测试通过现有 observer/token 入口建立记录，保留旧句柄，并按值检查
state/token/epoch，而非只比较对象句柄。当前实现中的定向测试已按此落实。

## 1. 术语与职责

| 术语 | 含义与代码对象 | 场景 |
|---|---|---|
| later-split record | 跨页 Load 后续拆分请求的身份记录，`memblock_later_split_tlb_record` | 保存 UID、动态代次、ROB、VPN、真实 L2 token 和完成 payload |
| dynamic epoch | 同一 UID 重新发射的代次，`status.dynamic_epoch` | redirect 后递增，旧 completion 仍可能持有旧对象句柄 |
| canonical owner | `later_split_tlb_record_by_uid[uid]` 当前指向的记录 | 重新发射建立新记录后，不应被旧 completion 覆盖 |
| 提交阶段 | redirect 全部只读预检查成功后的 mutation 阶段 | 不允许引入新的可捕获 fatal 或部分失败 |

`commit_uid_for_redirect_reissue()` 在 redirect 已确认覆盖某个 UID 后，统一更新该旧动态实例的取消和
重新 admission 状态。`cancel_later_split_tlb_record_for_uid()` 只取消指定 UID、指定旧 epoch 的现有记录；
没有记录、epoch 不匹配或已经取消时保持不变。它没有 ERROR/FATAL 路径。

## 2. 问题与最小方案

`a13d10814` 的旧 reissue 路径会取消 later-split record；`4948d291e` 抽取 redirect 提交 helper 后遗漏了
该调用。单纯递增 status epoch 能阻止 RM 读取旧预测，但旧 L2 pending response 仍持有原记录句柄。
`complete_later_split_l2_response()` 只有看到旧记录为 `CANCELED` 且 UID inactive/旧 epoch 时，才合法
丢弃迟到 completion；否则会按 provenance mismatch 或非法 owner replacement 报错。

在 `commit_uid_for_redirect_reissue()` 保存旧 epoch 后、递增 epoch 前，恢复：

```systemverilog
cancel_later_split_tlb_record_for_uid(uid, old_dynamic_epoch, "redirect_reissue");
```

位置紧邻既有普通 UID-TLB record cancel，且仍在 redirect 全量只读预检查之后。复用现有 helper、
状态和 completion 拒绝条件；不增加新配置或修改 DUT。

## 3. 修改与文档范围

- 生产源码：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，仅恢复上述取消调用。
- 定向验证：在既有 `soft_test_memblock_rob_admission_window_sequence.sv` 内增加本 Bug 必需的 redirect
  集成场景，继续从 `basicTest + memblock_rob_admission_window_directed_vseq + default` 启动。
  不修复或扩展母任务范围外的辅助 soft-test。
- 状态台账、W44 Buglist（动态复现并完成责任分类后）、本方案及独立 implementation review。
- 同步 `AI_DOC/mem_ut_flow_doc/redirect_flow.md` 和
  `AI_DOC/analysis/source_sv/dispatch_framework_sv/common_data_transaction.md` 的取消职责说明。

## 4. 验证流程

1. 保留当前 Sv39 原始 10-seed 结果；若未命中该时序，先只加入定向测试，用未修复生产代码编译复现。
2. 定向测试构造真实主表和 active UID，建立处于 `WAIT_RESPONSE` 的后续拆分记录，并通过
   `request_redirect_flush()`、anchor 与 `apply_redirect_flush()` 生产链完成 redirect。
3. 检查当前旧记录变为 `CANCELED`，UID epoch 递增；分别在新 owner 尚未创建和新 epoch owner 已建立
   后提交旧 completion，确认无 ERROR/FATAL、不会写旧 payload/response sample 或覆盖新 owner。
4. 同时保留不被 redirect 覆盖的 UID record，确认状态、epoch、token 和 payload 不变；无记录、已取消和
   旧 epoch 不匹配场景仍按既有 helper 合同处理。
5. 在已有 before-scan 与 planned-prefix caught-fatal 原子性场景中加入 later-split record 状态检查，证明
   预检查失败时没有提前取消。原 19 次 expected fatal 门禁保持不变。
6. 旧生产代码上的定向失败形成动态证据后，独立责任分类 review、登记 W44 Buglist，再恢复取消调用。
7. 重新编译；定向测试、原始 Sv39 10、已知 later-split 5 seeds 与邻接 PBMT0/safe_nocross16 配置复测。
   仿真允许并发，代码只在当前 mother worktree 串行修改。
8. 原始 Sv39 与真实 DUT 专项要求 controller PASS、完整计划数完成、无远端未知任务，且逐日志一次
   `TEST_PASS`、正常 `$finish`、raw/caught/demoted ERROR/FATAL 全零；定向专项单独允许既有 19 次精确
   expected fatal，最终 ERROR/FATAL 为零。
9. 独立 implementation review 通过后，一 Bug 一 commit；更新状态表，再恢复母矩阵后续组。

## 5. 协同边界

本方案不修改 RM 异常预测、completion provenance 比较规则、token 生命周期、RTL 或 Scala。
不实现 covergroup；最终覆盖率在母任务统一稳定源码上收集。

## 6. 执行证据

- 修复前 test-first：`basicTest + memblock_rob_admission_window_directed_vseq + default + seed=2601029`。
  mode=`mother_split_cancel_repro_20261029`，compile PASS，controller 1/1 FAIL，infra/remote_unknown 为 0。
- 201ns 首错：`LATER_SPLIT_TLB completion provenance mismatch uid=1 epoch=0 state=3`；token=102、
  VPN、s2xlate、entry generation=7 均匹配。此时 status 已 inactive 且 epoch=1，旧 owner 未取消。
- 结果目录：`mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_default_20261029_112338_2626771_13498`。
- 独立 reviewer `Galileo` 复核动态日志与构造，确认测试框架集成遗漏，给出 `CLASSIFICATION_REVIEW_PASS`。
  日志 caught fatal=1 为既有 ROB boundary 负向测试；本 Bug 是额外未捕获 fatal，最终 fatal=1。
- 修复后：新 mode `mother_split_cancel_fix_20261029` full compile PASS；同 seed 定向 1/1 PASS，四个
  旧 completion 全部 canceled-stale drop，caught fatal 保持 19，最终 error/fatal=0，正常结束。
  结果目录：`mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_default_20261029_113423_2661125_17609`。
- Sv39 原始 10 加已知 5 去重为 14 个 seed，全部严格 PASS；目录：
  `mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_tc_dispatch_real_mmu_sv39_smoke_20261029_113745_2709279_12065`。
- PBMT0 和 safe_nocross16 各 10 个原始 seed，20/20 严格 PASS；目录分别位于
  `mem_ut/ver/ut/memblock/sim/mother_split_cancel_adj_20261029_results/` 的两个同名 cfg 子目录。
  两组 `seeds.txt` 与原始归档逐行一致，所有真实 DUT 日志的 raw/caught/demoted error/fatal 均为零。

## 7. 执行中验证补充（IMPLEMENTATION_DELTA）

为落实母任务的专项随机 seed 要求，在固定原始/已知/邻接门禁之外追加 5 颗新生成 Sv39 seed；
只扩大验证样本，不改变生产实现或原有门禁。seed 为
`165782506,1073016196,2101711169,1916481990,527231530`，结果目录为
`mem_ut/ver/ut/memblock/sim/mother_split_cancel_fresh5_20261029_results`，5/5 严格 PASS。
seed 与原始 260-seed 矩阵及已知 14-seed manifest 无重复；生产源码保持不变。
