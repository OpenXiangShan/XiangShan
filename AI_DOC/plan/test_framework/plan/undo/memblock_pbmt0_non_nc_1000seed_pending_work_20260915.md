# PBMT0 non-NC 1000-seed 回归待开展事项（2026-09-15）

| 项目 | 内容 |
| --- | --- |
| 状态 | 暂停，待用户恢复任务后开展 |
| 版本 | V2，`mem_ut_uvm_v2` |
| 测试入口 | `tc=basicTest ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k` |
| 固定运行期选项 | `+MEMBLOCK_MAIN_TRANS_NUM=100 +MEMBLOCK_HARD_XZ_CHECK_EN=0 +MEMBLOCK_CHECK_TRIGGER_EN=0` |
| 临时 RTL | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/test-rtl/build/rtl` |

## 专有名词与待开展范围

| 术语 | 当前含义 | 关联落点 | 示例 |
| --- | --- | --- |
| 定向 seed | 已稳定复现缺陷、必须先验证修复的随机种子。 | `seed=856436350` | STA fault 后缺少 redirect。 |
| 1000-seed 回归 | 使用 1000 个互不重复随机 seed 的串行或受控并发仿真。 | 目标 testcase | 每例均关闭 trigger/X 检查。 |
| RM/测试框架缺陷 | 验证环境状态、驱动、monitor、RM 或结束条件的问题。 | `AI_DOC/buglist/rm/v2` | tombstone overflow、fault-head redirect 缺失。 |
| RTL 候选 | 可能由 DUT 内部状态或输出首发异常导致的问题。 | `AI_DOC/buglist/rtl/v2` | 必须先经独立 RTL review。 |

## 已完成状态

1. 正确临时 RTL 的编译来源已核验：`pbmt0_non_nc_100k_test_rtl_20260915/log/vcs_compile_rtl.log` 指向 `test-rtl/build/rtl`。
2. 已定位并记录 STA tombstone overflow，方案和独立 review 已完成；当前 `common_data_transaction.sv` 已存在同 stable scope 合并的未验证修改。
3. fault head 未驱动 redirect 已定位为测试框架问题，非 RTL；fault recovery plan 已完成 coding、flow 同步及原始复现 seed 回归，计划已归档到 `plan/do`。generic redirect 与 terminal fault 的重叠专项定向覆盖仍是剩余验证项。
4. 尚未开始本轮 1000-seed 正式回归；不得把此前中止的 seed 运行计入通过数。

## 恢复后执行顺序

1. 已完成 `memblock_fault_head_redirect_recovery_plan_20260915.md` 的 coding、software-only 兼容和原始随机 seed 验证；后续只需补 generic redirect 与 terminal fault 重叠的专项定向覆盖：
   - `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/plan/test_framework/plan/undo/memblock_sta_tombstone_replay_coalescing_plan_20260915.md`
   - `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/plan/test_framework/plan/do/memblock_fault_head_redirect_recovery_plan_20260915.md`
2. 以显式 `MEMBLOCK_XS_HOME=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/test-rtl` 编译新独立 mode，并从 compile log 核验 `test-rtl/build/rtl` 来源。
3. 首先运行 `seed=856436350`：检查 tombstone history 不再溢出、UID5 terminal 后出现一次 `io_redirect_valid`、年轻 UID 走既有 cancel/reissue、测试完整结束。
4. 定向 seed 通过后，从 seed 列表中生成 1000 个互不重复随机 seed，逐例运行并登记每例日志、退出状态和首个错误。
5. 出现 RM/测试框架问题时，先按 buglist 周文件更新或新章节记录，再生成/评审专项方案、coding、重跑该 seed；出现 RTL 候选时，先启动独立 subagent review，确认 RTL 后只记录 RTL bug、首发点和波形路径并停止。
6. 1000 例全通过后，生成实现 review、更新关联 flow、将已完成 plan 归档到 `do`，并只提交本轮相关文件；除非用户明确要求，不 push。

## 暂停边界

当前按用户要求暂停在“方案和待开展事项已落文档”的阶段。不得在恢复前启动编译、仿真、回归、代码修改、plan 归档或提交。
