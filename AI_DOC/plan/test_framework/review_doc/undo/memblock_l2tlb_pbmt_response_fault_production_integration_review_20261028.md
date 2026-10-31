# PBMT response-fault production-only mother integration review

## 术语与最终实现边界（2026-10-31补记）

`B/C`为本定向两阶段，`requestor`为独立Load/Store请求来源，`ticket`为前缀/模型版本冻结后的原子提交凭据，`replay`为把成熟CSR事实有序应用到RM模型。
原review范围对应mother提交`5c131a18de6d781cad3474647dfc578c2183220d`。除下文B/C行为外，它恢复了issue/request wrapper，共用91d的prepare/commit事务；不能仅说“保留ticket”而让读者误以为仍禁止任何请求边界应用。当前稳定函数在`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:9248,9549,9698`：issue预检、成熟前缀replay、重建context plan、最终提交；L2请求也按精确sample进入同一wrapper。C+2、epoch/revision和model/FIFO一次性联合提交不变，没有第二个裸pop消费者。

### Plan对齐及审核时点

2026-10-28 Boyle原独立REVIEW_PASS和44真实＋20软件结果保留；本次补充Git最终调用链，不将91d隔离review倒签成5c事前批准。当前plan在同mother仓库`AI_DOC/plan/test_framework/plan/undo/memblock_l2tlb_pbmt_response_fault_production_integration_plan_20261028.md`，已补最终方案。原source失败日志未查获同hash备份的取证限制如实见W43原章，不用修后PASS补造原FAIL。
source Planck于2026-10-26的APPROVED可从Git对象`43669b2d3ed843a095d4e1f2c019b9d7f5dfb4e6:AI_DOC/plan/test_framework/review_doc/do/memblock_l2tlb_pbmt_response_fault_directed_fix_implementation_review_20261026.md`核对，blob=`ca3da472bf637479a3dbd922e36c1568db027912`。这是源版本33项负测/65项当时验收的文档，不是原ignored日志恢复，也不代表mother整包合入这些软件候选。Boyle原文明确ticket/原子性，不扩写成逐helper事前签核。

### 非本次修改的逻辑分析

本次只更新说明，无生产修改；已有四份知识库脏文件及signal.rc不属于本次。下文原英文review保留历史签核原文，中文补记解释其当前源码含义。

## Integrated Behavior

- Adds the PBMT response-fault phase graph, PMP bootstrap observation, B/C readiness and two-slot request/completion audit.
- Builds B as Store under disabled PBMTE and C as an independent Load after PBMTE enable, sharing one canonical
  level-1 entry while requiring two real L2TLB tokens.
- Keeps mother CSR replay tickets and prefix commits. The PBMT monitor wrapper sets `reset_epoch` and preserves
  produced-count/FIFO-revision checks before the generic event is committed.
- Keeps the previously integrated Sv39 later-split observer, token, redirect/flush cancellation and exact completion
  provenance.
- Excludes all software-only atomicity testcase/vseq code and all RTL/Scala/RM changes.

## Mother Evidence

Mode: `mother_pbmt_response_fault_prod_20261028`.

| Batch | Result | Result directory |
|---|---:|---|
| Original failure seeds | 4/4 strict PASS | `sim/mother_pbmt_response_fault_prod_20261028_results/original4_r1` |
| Authoritative seeds | 10/10 strict PASS | `sim/mother_pbmt_response_fault_prod_20261028_results/authoritative10` |
| Fresh random seeds | 30/30 strict PASS | `sim/mother_pbmt_response_fault_prod_20261028_results/random30` |
| Adjacent PBMT gate NC | 10/10 clean legacy PASS | `sim/mother_pbmt_response_fault_prod_20261028_results/gate_nc10` |
| Adjacent PBMT gate IO | 10/10 clean legacy PASS | `sim/mother_pbmt_response_fault_prod_20261028_results/gate_io10` |

The 44 main-scenario logs each have `rc=0`, exactly one `TEST_PASS`, normal `$finish`, UVM summary ERROR/FATAL=0,
and caught/demoted ERROR/FATAL=0. The 20 adjacent gate logs have the same zero-error and normal-finish evidence,
but their soft-test testcase wrapper does not print `TEST_PASS`; no soft-test change was made to alter that behavior.

## Independent Review

- Reviewer: `Boyle`, agent/session `01a181e7-0c4e-73c2-bfc4-1c79601e2f2f`.
- Review focus: CSR watermark/FIFO atomicity, mother replay-ticket preservation, later-split provenance preservation,
  global helper impact, B/C phase/request/completion/final audit, and forbidden paths.
- Result: `REVIEW_PASS`; no P0/P1/P2 blocker.

Final status: **REVIEW_PASS**.
