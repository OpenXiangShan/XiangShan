# PBMT response-fault production-only mother integration plan

## 术语与Git最终集成说明（2026-10-31补记）

`phase`为B/C两个测试阶段；`requestor`为分别发起DTLB请求的Load/Store通道；`ticket`为冻结版本/前缀后的原子提交凭据；`replay`为按采样生效时点更新PMA/PMP模型。
最终mother提交`5c131a18de6d781cad3474647dfc578c2183220d`，保留B Store/C Load、共享canonical及两个真实token，不整包合入source软件候选。CSR持续service外，当前issue和L2 request边界通过`apply_pma_pmp_csr_writes_before_request()`共用`prepare_pma_pmp_csr_replay()`/`commit_pma_pmp_csr_replay()`，不能沿用91d方案“没有request/issue调用”的描述。持续入口取producer发布sample，边界wrapper取issue/request sample，仅应用C+2已成熟的连续前缀，replay后重建context plan再提交。唯一破坏性model/FIFO联合提交仍在共同commit函数；不是第二套裸pop。
现存2026-10-28 Boyle母集成REVIEW_PASS见`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/AI_DOC/plan/test_framework/review_doc/undo/memblock_l2tlb_pbmt_response_fault_production_integration_review_20261028.md`，原44真实严格与20软件clean验收保持分开。source Planck的2026-10-26 review可通过`43669b2d3ed843a095d4e1f2c019b9d7f5dfb4e6:AI_DOC/plan/test_framework/review_doc/do/memblock_l2tlb_pbmt_response_fault_directed_fix_implementation_review_20261026.md`读取Git对象，不能将源33负测等同mother迁移/重跑；本次只同步最终方案，不倒签审批。plan保留undo不擅自归档。

## Scope

- Mother worktree: `.codex_mother_integration_20261028`
- Mother branch: `codex/memblock-csr-prefix-fix-20261025`
- Source commit: `43669b2d3 memblock: make PBMT response-fault audit atomic`
- Production testcase: `tc=basicTest`, `ts=memblock_l2tlb_pbmt_response_fault_vseq`,
  `cfg=tc_l2tlb_pbmt_response_fault`
- Excluded: software-only caught-fatal atomicity sequence/vseq, `seq_pkg` includes, RTL, Scala and RM changes.

## Integration Rules

1. Reconstruct PBMT phase/readiness/audit logic on top of the mother CSR replay-ticket and raw-prefix commit model.
2. Preserve raw CSR `reset_epoch`, produced count, FIFO revision and atomic prefix consumption.
3. Preserve Sv39 later-split sample ordering, token binding, flush/reset cancellation and completion provenance.
4. Keep B Store and C Load on independent DTLB requestors with exactly two accepted/completed L2TLB tokens.
5. Commit this Bug independently only after compile, original4, authoritative10, fresh30, NC10, IO10 and independent review.

## Acceptance

- Main production scenario: 44/44 strict PASS with `rc=0`, one `TEST_PASS`, normal `$finish`, and zero
  raw/caught/demoted `UVM_ERROR/UVM_FATAL`.
- Adjacent gate NC/IO: 20/20 clean legacy PASS (`rc=0`, normal `$finish`, zero raw/caught/demoted errors/fatals).
  These soft-test testcase wrappers do not emit `TEST_PASS`; they are not reported as strict PASS.
- Staged diff contains no `soft_test`, atomicity vseq, `seq_pkg`, RTL, Scala or RM path.
- Independent final result: `REVIEW_PASS`.

Status: completed on 2026-10-28.
