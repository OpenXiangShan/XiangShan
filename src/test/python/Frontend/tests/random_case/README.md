# Random-case tooling

For four-family executed-space metrics and construction priorities, see
[PROGRAM_EXPLORATION.md](PROGRAM_EXPLORATION.md).

The generator and scenarios cover four families: `cfg_layout_mix`,
`ras_indirect_history`, `ibuffer_pressure_cross`, and `translation_timing_cross`.
Generator/assembly/ProgramMap unit tests use temporary synthetic inputs; they do
not replace the campaign corpus or establish a DUT HIT.

Campaign evidence requires an identity index containing per-case membership,
program/timing seeds, parameters and source/bin/trace/ProgramMap/scenario hashes.
Retain the original inputs and verify their bindings before an audit or replay.

Run all offline tooling tests from `src/test/python/Frontend` with the RISC-V
GCC/binutils directory on `PATH` and the mcpgateway Python environment active:

```bash
PYTHONDONTWRITEBYTECODE=1 PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 TB_RUN_DUT=0 TB_ENABLE_DUT_TESTS=0 \
  python -m pytest -p xdist.plugin -p no:cacheprovider -q tests/random_case \
  --ignore=tests/random_case/test_random_bin_trace_dut.py
```

Only the actual DUT test file is ignored; no `-k` filtering is required. Assembly
and binary-backed ProgramMap tests require `riscv64-unknown-elf-gcc`, `objcopy`,
and `objdump` and must run in the submission gate.

To audit native evidence from an existing campaign, use the hash-pinned binding
index published by coord and a new report path:

```bash
python tests/random_case/run_random_campaign.py \
  --mode audit-native \
  --output-root /absolute/existing-campaign-root \
  --native-binding-index /absolute/coord-binding-index.json \
  --native-binding-index-sha256 COORD_PUBLISHED_INDEX_SHA256 \
  --native-audit-output /absolute/new-admission-report.json
```

`audit-native` is an offline, post-run evidence admission gate: it validates
artifact sidecar bindings and the frozen source/build/definitions/sampler/
registry/environment provenance. It creates only a fresh admission report. It
does not generate programs, invoke NEMU or DUT, repair failed qualification,
replace termination/checker/monitor/oracle/ProgramMap checks, or grant canonical
HIT. Missing or inconsistent evidence remains ineligible.

DUT execution is never part of the default offline gate. Do not use `--mode dut`
or enable `TB_RUN_DUT` without coord's loader-valid manifest/compatibility ledger
and serialized slot handoff. Random
evidence goes to ctrl for canonical review; audit does not edit canonical
coverage, registry, sampler, or ctrl contracts.

`fixtures/historical/*.sh.txt` are nonexecutable, SHA256-pinned test fixtures.
Tests inspect their command semantics as text.

## Coverage growth measurement (offline)

`coverage_growth_audit.py` measures an existing qualified single-family campaign
through the same pinned native-admission gate. Supply a reviewed, hash-pinned
scope JSON (`schema_version: 1`, one `family`, explicit unique `bin_ids`); it does
not change the full canonical denominator. Example, from Frontend:

```bash
python tests/random_case/coverage_growth_audit.py \
  --campaign /absolute/existing-campaign-root/campaign.json \
  --binding-index /absolute/coord-binding-index.json \
  --binding-index-sha256 COORD_PUBLISHED_INDEX_SHA256 \
  --scope /absolute/reviewed-family-scope.json \
  --scope-sha256 REVIEWED_SCOPE_SHA256 \
  --output /absolute/new-growth-report.json
```

All original cases must already have `qualification_pass=true`; admission never
upgrades them. Version 1 requires one native report per case and measured positive
`details.cycles_run` in its bound timeline. All runs consume budget, including
repeated seeds and runs with no new scoped bins. The report preserves the recorded
campaign order, full-definition counts, per-run scoped gain and cumulative cycles.
Optional increasing `--cycle-budget` values report completed-run lower bounds;
partial-run hits are unknown. No extrapolation or first-hit inference is allowed.

Simulation cycles are not wall-clock regression time. Within-run coverage growth,
wall time, semantic scene combinations and directed-test budget comparisons remain
explicitly unmeasured until their evidence is available. Final native counts alone
cannot establish saturation or a bug-finding advantage. The report is diagnostic;
semantic probe review and canonical HIT decisions remain with the proper owner.
No replay, corpus generation or DUT is invoked, and the fresh output is exclusive.

## IBuffer exploration audit (offline)

`ibuffer_exploration_audit.py` supplements the growth report with logged stimulus
combinations for `ibuffer_pressure_cross`. It uses the same CLI inputs as the
growth tool (`--campaign`, `--binding-index`, `--binding-index-sha256`, `--scope`,
`--scope-sha256`, `--output`), with an explicitly reviewed IBuffer bin scope.

It requires original case qualification, native evidence admission, hash-bound
ProgramMap/scenario/realization, and a complete matching frozen timing plan. The runner records
`scenario_realization_sha256` for future results, including failed runs. Older
manifests without this pin remain unready for the audit; no hashes are inserted
retroactively and no original qualification is upgraded. Queued and driven redirect records
must match unambiguously. Repeated indistinguishable redirects remain unresolved.

Combination axes use actual linked CFI kind/width/32B/64B position, pressure
duration band, driven redirect's logged phase in the window, and response beat /
logged timing relation. Equal log cycles are labeled `same_log_cycle`; they do
not prove same-edge RTL overlap. The report preserves zero-new-combination runs
and pressure costs, and retains exact sites separately from combination keys.
No Cartesian-product coverage percentage is inferred from these axes.

Current IBuffer defaults choose 4–8 long windows of 1200–1500 cycles triggered by
queued redirects. Short/medium-duration cells are not explored by that recipe.
Requested `cfi_offset` is padding before instruction preludes, not necessarily the
actual CFI position. Use linked ProgramMap PCs; seeds or metadata alone cannot
prove the desired layout or interaction. These limits are reported, not silently
changed by this audit. Actual fullness, held payload stability, flush/recovery,
wall time and within-run bin growth remain unmeasured without proper evidence.


`translation_exploration_plan.py`
constructs explicit stage1/onlyStage1/onlyStage2/allStage Python model scenarios
in memory, with bounded PTW ready/latency, context IDs, PBMT/PMA and fault plans.
It does not change the existing long-program wrapper, generate inputs or admit
runtime. Every plan reports `MODEL_ONLY`, `runtime_reachability=UNCONFIRMED`, and
`long_golden_trace_admitted=false`. Fault/recovery needs a separate reviewed
checker/continuation model; a NEMU normal trace cannot be silently reused.

Normal PTW response overrides keep s2xlate/getGpa/memidx identity immutable.
The agent response contract applies at the Python output boundary. Default fence
flush drops pending responses, and explicit preservation retains request identity.
A preserved response is not proof it passed the RTL PTWFilter or was consumed by
TLB. VMID values must already fit a reviewed width; the planner never silently
truncates them or claims to validate backend Fence implementation.


## Optional CFG and RAS program exploration

Defaults remain `--branch-history-mode loop_mask --ras-return-mode private`.
The optional modes change source/bin/scenario and generator identity; never attach
new hashes or qualification to old inputs. Both generator and campaign CLIs expose
the same flags, and campaign resume restores the originally frozen modes.

- CFG or translation `--branch-history-mode site8` gives each conditional site a
  deterministic 8-bit sequence derived from program seed, ordinal and site index,
  separately from layout RNG. Both outcomes occur over eight loop iterations;
  `--iterations` must be at least eight. It composes with `history4` and page-tail
  layouts. Patterns can collide and repeat every eight iterations; this is bounded
  per-site variety, not a claim of statistical independence or unlimited growth.
- RAS `--ras-return-mode shared_leaf` lets distinct nested call sites use the same
  x5-linked leaf and one shared return instruction, with separate legal return
  PCs. Each chain still saves/restores ra and sp. It cannot be combined with CFG
  branch mode in a campaign spanning incompatible families; preflight rejects it
  before creating artifacts. Select only the compatible family for each opt-in.

Program realization additionally requires actual consecutive committed PCs: both
linked branch successors at every tagged site, or distinct linked legal successors
at the same shared return. Declared trace targets alone cannot satisfy these gates.
The test-only `_bounded_program_model.py` interprets a small generated instruction
subset and checks complete good-trap/stack restoration. It is not NEMU, an ISA
oracle, hardware RAS/history evidence, native coverage or runtime qualification.
No standalone execution entry or default DUT activation is added.
