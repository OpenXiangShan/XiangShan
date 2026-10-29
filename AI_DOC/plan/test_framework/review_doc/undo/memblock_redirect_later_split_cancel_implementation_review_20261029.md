# Redirect later-split 取消恢复实现审查

日期：2026-10-29。工作区：`.codex_mother_integration_20261028`。
当前结论：`REVIEW_PASS`。定向、Sv39、邻接和新随机验证均通过，允许形成单 Bug commit。

## 1. 术语与抽象功能说明

| 术语 | 当前含义 | 代码对象 | 例子 |
|---|---|---|---|
| dynamic epoch | UID 当前发射代次 | `status.dynamic_epoch` | redirect 把 0 推进为 1，旧 response 仍带 0 |
| owner | 后续拆分请求的身份记录 | `memblock_later_split_tlb_record` | pending response 保存 token=102 的旧对象句柄 |
| canonical owner | UID 关联表当前指向的记录 | `later_split_tlb_record_by_uid` | epoch=1 建立新 owner 后，旧 completion 不能覆盖它 |
| canceled-stale | 已取消且不属于当前活动代次的完成 | `complete_later_split_l2_response()` 的既有分支 | 旧 owner 标记取消后，迟到 completion 被丢弃 |
| precheck/commit | 全量只读检查后统一写状态 | redirect 两阶段处理 | 前置失败不能先取消 owner |

## 2. 生产修改

抽象功能描述：`common_data_transaction::commit_uid_for_redirect_reissue()` 在 redirect 全量检查成功后，
提交旧 UID 实例的资源和翻译记录取消，再推进 status 到等待重新 admission 的新代次。

修改前只取消普通 UID-TLB 等待记录，遗漏独立的 later-split 记录。新修改复用现有 exact UID/epoch
取消 helper；该 helper 只有早退、状态赋值与 INFO，不存在 ERROR/FATAL，因此不会在 commit 途中引入
第二个失败点。token 完成验证和 RM 预测算法均未改动。

源码：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，
`commit_uid_for_redirect_reissue()` 中的翻译记录清理位置。

```systemverilog
cancel_later_split_tlb_record_for_uid(uid, old_dynamic_epoch, "redirect_reissue");
commit_cancel_waiting_uid_tlb_record_for_redirect_range(uid);
```

中文伪代码：本段负责取消将被重新发射的旧 UID 翻译实例。先用进入本次提交时保存的旧 epoch，调用
既有 later-split cancel，只把匹配 UID/epoch 的记录标为取消；不存在、不匹配或已取消时保持原状。
随后执行原有普通 waiting UID-TLB 取消；后续状态重置才递增 epoch。旧 response 持有的句柄仍保留，
所以其 completion 能识别为已取消旧代次，而新 epoch 创建的 canonical owner 不会被该操作命中。

## 3. 定向验证

抽象功能描述：`soft_test_memblock_rob_admission_window_sequence::build_later_split_pending_owner()`
只服务本专项，通过现有 split observer 和 exact L2 token 入口建立可保留句柄的在途对象。
`run_redirect_later_split_cancel_checks()` 负责驱动生产 redirect wrapper，并检查旧 completion
对旧/新 owner 的可见状态；不驱动 DUT 或构造 RM 期望。

| 场景 | 期望与已观察结果 |
|---|---|
| 普通 reissue，旧 completion 先于新 owner | 旧 owner 取消，旧 payload/sample 不写入 |
| 普通 reissue，旧 completion 晚于新 owner | 新 canonical owner/epoch/token 不被旧 completion 覆盖 |
| 未提交 fault reissue，新 owner 创建前 | 旧 owner 通过相同提交 helper 取消，迟到 completion 丢弃 |
| 未提交 fault reissue，新 owner 创建后 | 同上，并保护新 canonical owner |
| 不相关 UID、epoch 不匹配、无记录、重复取消 | 不改不相关记录，不创建记录，不重复消费 |
| before-scan 与 planned-prefix 检查失败 | 按值检查 state/token/epoch/sample，不提前取消记录 |

修复前：只增加测试、保留原生产代码重新编译。seed `2601029` 在 201ns 首次未捕获 fatal 为
`LATER_SPLIT_TLB completion provenance mismatch`，token/key/generation 均匹配而旧 owner 仍为 state=3。
修复后：同 seed 1/1 PASS，四个旧 completion 均打印 `ignore canceled stale completion`，存在四场景
完成 marker，`TEST_PASS=1`，既有 caught fatal=19、最终 ERROR/FATAL=0，正常 `$finish @201ns`。

母工作区 `mem_ut/ver/ut/memblock/sim/.eda_regression/` 下证据：

- 修复前：`basicTest_default_20261029_112338_2626771_13498`。
- 修复后：`basicTest_default_20261029_113423_2661125_17609`。

## 4. 方案一致性与边界

实现符合 `memblock_redirect_later_split_cancel_integration_plan_20261029.md`。生产只有恢复既有调用及
必要中文注释；测试 helper 位于现有 soft-test sequence 内，无公共 test-only API、配置或 vseq 新增。
独立责任分类与方案 review 均通过。未发现实现偏离或额外机制。

独立 reviewer `Galileo`（`01a1815b-d3fc-7c41-84b6-6fbe81f2332e`）已完成当前源码及修复后定向日志
复核，结论 `STATIC_IMPLEMENTATION_REVIEW_PASS`，无静态 blocker；确认 4 次旧 completion 丢弃、
2 次新 epoch owner 建立、精确 19 次 expected fatal 和最终零错误均有动态证据。

真实 DUT 补充验证：Sv39 原始 10 加已知 5 去重后共 14/14 严格 PASS，每份日志一次 `TEST_PASS`、正常
`$finish`、raw/caught/demoted ERROR/FATAL 全零。controller 无失败/未完成/远端未知任务。
结果目录：`mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_tc_dispatch_real_mmu_sv39_smoke_20261029_113745_2709279_12065`。

PBMT0/safe_nocross16 邻接：各 10 个原始 seed，共 20/20 严格 PASS，seed 清单与原归档逐行一致，
controller 所有异常分类为零。结果根目录为
`mem_ut/ver/ut/memblock/sim/mother_split_cancel_adj_20261029_results/`。

独立 reviewer `Galileo` 对生产、测试、全部动态证据和长期文档给出最终 `REVIEW_PASS`。
方案归档至 `AI_DOC/plan/test_framework/plan/do/memblock_redirect_later_split_cancel_integration_plan_20261029.md`。
原始矩阵其它组和最终覆盖率仍由母任务继续负责。

## 5. 执行中验证补充

原方案固定样本之外追加 5 个新随机 seed，5/5 严格 PASS，证据目录为
`mem_ut/ver/ut/memblock/sim/mother_split_cancel_fresh5_20261029_results`。与原始矩阵及已知 14-seed
manifest 无重复。该项已作为 plan 的 `IMPLEMENTATION_DELTA` 标注，只增加验证样本，无生产行为变更。
本 Bug 总动态门禁为定向 1 项和真实 DUT 39 项（14+20+5），专项不重复计入母矩阵进度。
