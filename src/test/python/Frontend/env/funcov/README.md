# Frontend Functional Coverage Code

This directory contains implementation predicates registered by
`env/funcov/recorder.py`. It does not define a second functional-coverage
methodology or coverage registry.

## Implementation map

- `env/funcov/recorder.py`: loads the canonical registry, coordinates
  recorder sampling, and writes functional-coverage artifacts.
- `env/funcov/__init__.py`: shared registration and common predicate entry
  points.
- `env/funcov/py/`: Python predicate packages, organized by observation
  domain.
- `env/funcov/sv/`: SystemVerilog observation/bind sources used by the
  simulator-specific coverage flow; they do not create a second canonical
  registry.

Use `src/test/python/Frontend/docs/03_funcov_model/skills.md` for the
canonical testpoint, recorder, testcase, artifact, and back-annotation rules.
Use `src/test/python/Frontend/README.md` for the source-tree layout and script
entrypoints.

## IFU maintenance checks

Keep the canonical mapping in
[`frontend_bt_functional_coverage_pilot.csv`](../../docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv).
Design constraints and unresolved testpoint premises live in section 2.3 of
the [verification plan](../../docs/01_testplan/Frontend_BT_验证方案.md), not in a
second status table or registry. The following existing checks retain lessons
from the retired migration and reachability reports:

| Concern | Implementation / executable check |
| --- | --- |
| Registry entries versus runtime producers | [pilot schema tests](../../tests/py/jiabowen/test_functional_coverage_pilot_schema.py), especially `test_modeled_jiabowen_runtime_producer_gap_inventory_is_current`; AST inventory is not runtime reachability proof. |
| Aggregate response, S1 index/stitch, registered S2 payload | [cacheable pipeline sampler](py/ifu/cacheable_pipeline_funcov.py) and [tests](../../tests/py/jiabowen/test_ifu_cacheable_pipeline_functional_coverage.py); retain missing-index/predecode, transaction-mismatch and flush negatives. |
| Raw selector versus effective cross-block owner | [two-fetch tests](../../tests/py/jiabowen/test_two_fetch_functional_coverage.py) and [compact sampler tests](../../tests/py/jiabowen/test_ifu_compact_functional_coverage.py); reject defaulted cross flags and ownerized raw selectors. |
| BIN-874 first-owner clipping | [DUT checker](../../tests/py/jiabowen/test_ifu_first_owner_clip_v3_dut.py) preserves trained preceding blocks and changes only the first owner's JAL in a late dual-fetch pair. Require valid preclip raw block-one slots, complete first-owner output and zero second-owner enqueue bits, then check held payload, first FTQ redirect, PC/instruction/predecode/offset and sequential recovery. [Negative contracts](../../tests/py/jiabowen/test_ifu_first_owner_clip_contract.py) reject partial owner observability, second-slot enqueue leakage, invalid metadata and duplicate marks. The legacy PredChecker node calls the same scenario. Generic monitor observations are valid presentations, not accepted transfers under backpressure: the independent recovery checker samples physical pre-drive decodeCanAccept, requires resumingVType=0, and checks every accepted lane against the fixed program sequence. Generated IBuffer ready ports are absent/unused, never defaulted. |
| BIN-814 mutually exclusive IFU observations | [BPU S3 flush contract](../../tests/py/jiabowen/test_ifu_bpu_s3_flush_v3_contract.py) checks source RTL, generated RTL and inventory; this does not count as a DUT HIT. |
| BIN-940 causal half-RVI recovery | [delivery negative/contract tests](../../tests/py/jiabowen/test_ifu_invalid_taken_half_delivery_funcov.py) and [DUT recovery test](../../tests/py/jiabowen/test_ifu_predchecker_v3_dut.py): checked invalidTaken writeback → S0 acceptance → S1 saved-half stitch/predecode → fired S2/IBuffer payload. Require continuous sampling and cancellation handling before the backend skip guard. Recovery endOffset is fetch-relative zero, not the original redirect offset. Generated S1 instruction.isRvc is the source of S2 pdInfo.isRVC; record actual paths, never synthesize missing pdInfo wires. |
| BIN-973 taken-form candidates | [compact sampler](py/ifu/compact_funcov.py): distinguish raw taken forms from same-transaction no-fault candidates; accumulate required JALR/CALL/RET forms within one run. Architectural execution alone is not a predicted-taken witness. |
| BIN-954 exception metadata | [Component/negative tests](../../tests/py/jiabowen/test_ifu_exception_metadata_funcov.py) and [DUT tests](../../tests/py/jiabowen/test_ifu_exception_metadata_v3_dut.py): normal satpFlush delivery must not require an exception; GPF address writes require actual GPF, matching FTQ/address/nonleaf metadata; cross-page proof requires a page-tail RVI half and the adjacent page. Store completed per-component transaction witnesses in `sampler_diagnostics.exception_metadata`, including actual paths and missing-probe diagnostics. All five components must occur in one run; independent component PASS artifacts cannot be combined into a HIT. The one-run regression requires hardware/software reset synchronization, guarded monitor clearing and separate phase checker results. |
| Backend reset epoch | [Reset unit/negative tests](../../tests/py/environment/test_backend_hardware_reset_contract.py) and [DUT reset canary](../../tests/py/jiabowen/test_backend_reset_epoch_v3_dut.py) exercise stale FTQ/PC and queued-action invalidation, new-epoch stale guards, preserved errors/config/RNG/trace cursor and cumulative statistics. Reset must not erase completed coverage witnesses, but must invalidate in-flight transaction state. Non-CFI semantic commits on callRetCommit carry rasAction=0 and require fresh FTQ identity, not blanket valid suppression. |
| FrontendTrigger capability versus closure | [Probe-gap negatives](../../tests/py/jiabowen/test_ifu_frontend_trigger_probe_gaps.py) cover all config fields/slots, absent versus unreadable handles, history cancellation and persistence beyond the 128-entry risk tail. Unknown timing remains `None`: the five real fields can support BIN-927/928,996–999,1001–1003, but BIN-1000 still requires observed timing. `sampler_diagnostics.frontend_trigger_config_gap` records missing fields, actual affected bins and attempted paths. [Directed tests](../../tests/py/jiabowen/test_ifu_frontend_trigger_v3_dut.py) separately declare the current PC contract and legacy timing contract, with independent config/lane/action/flush checkers. The [ABI canary](../../tests/py/jiabowen/test_ifu_frontend_trigger_capability_v3_dut.py) remains untargeted diagnostic evidence, never backannotation. |
| Trigger redirect sampling | The backend cfVec recovery guard must not hide held S2 trigger flush. Use registered S2 PC plus the real output foldpc and FTQ identity; the debug-only full-PC IBuffer output is pruned in the current build. `test_frontend_trigger_sampler_requires_held_pc_ftq_identity_at_redirect_flush` rejects wrong PC/foldpc/FTQ and exercises the guarded dispatcher with/without timing. Reset clears temporal Trigger state. Check new-path instruction/action delivery after the actual DUT flush, not merely after queuing an injected backend event. |
| TL-A user bits and late old responses | [InstrUncache producer tests](../../tests/py/jiabowen/test_ifu_instr_uncache_owner_functional_coverage.py) retain missing/changing user-field and stale-response negatives; [attribute DUT test](../../tests/py/jiabowen/test_ifu_instr_uncache_attribute_stability_v3_dut.py) must use actual exposed fields. |
| BPU and shared depth negative scenarios | [DUT canaries](../../tests/py/jiabowen/test_bpu_v3_negative_dut.py) cover all-not-taken, PrefetchDepth full/wrap/flush, mBTB compareBits and BTB lower-target semantics. Their presence is not a passing-run claim. |

For each build, resolve and record the actual semantic-port/flat-alias/fallback
path and sampling phase. A name in a source file or inventory alone does not
prove that a fallback has the same runtime meaning. Keep diagnostic events
out of primary HIT evidence; preserve exact target, current provenance and
the run's checker/monitor results when replaying a candidate.
