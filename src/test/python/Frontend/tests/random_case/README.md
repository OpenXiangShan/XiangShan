# Random-case tooling

The generator and scenarios cover four families: `cfg_layout_mix`,
`ras_indirect_history`, `ibuffer_pressure_cross`, and `translation_timing_cross`.
Generator/assembly/ProgramMap unit tests use temporary synthetic inputs; they do
not replace the campaign corpus or establish a DUT HIT.

The original 100-slot campaign identity index is still awaiting recovery. Restore
per-slot membership, program/timing seeds, parameters, and original
source/bin/trace/ProgramMap/scenario hashes (plus verified inputs for replay).
Aggregate family counts cannot reconstruct identities. Do not generate a
replacement corpus or replay closed historical inputs.

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
and explicit serialized slot handoff. Audit currently has no DUT slot. Random
evidence goes to ctrl for canonical review; audit does not edit canonical
coverage, registry, sampler, or ctrl contracts.

`fixtures/historical/*.sh.txt` are nonexecutable, SHA256-pinned archival text.
Tests inspect their historical semantics without running any `check` or `run`
branch. They are not current replay entries. The seven old strategy documents
remain historical references outside this current tooling delivery.
