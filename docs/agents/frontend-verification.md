# Frontend Verification Guide

## Scope

Treat the project root as `$NOOP_HOME`.

Most agent work in this repository is expected to target the frontend Python
verification stack under `src/test/python/Frontend/`, not the whole XiangShan
tree. Start here unless the task explicitly says otherwise.

## Entry Points

Use `src/test/python/Frontend/README.md` for the source-tree layout, script
index, and standard build/test commands. Its implementation map is the only
directory map maintained for this environment.

The harness-critical locations are:

- `src/test/python/Frontend/env/runtime/fixtures.py`: shared DUT fixture, artifact
  setup, and VCS batch finalization.
- `src/test/python/Frontend/conftest.py`: pytest session hook for VCS batch
  finalization.
- `src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh`: single-bin
  runner that creates the run identity and artifact root.
- `src/test/python/Frontend/docs/03_funcov_model/skills.md`: the sole
  functional-coverage modeling and back-annotation methodology.

## Source Of Truth

Use the most direct artifact that reflects real DUT behavior:

1. Observed DUT-facing IO in the Python environment and tests.
2. Generated artifacts under the selected frontend pylib directory, especially
   `Frontend_top.sv` and `signals.json`.
3. Generated RTL under `build-frontend/rtl/` when signal-level confirmation is needed.
4. Reference docs under `docs/testbench/Guide_Doc/`.

Do not treat host-side implementation files as runtime truth unless you have
confirmed the built DUT artifacts actually include that behavior.

All env-side signals and bundle fields must be based on the actual generated DUT
interface. If a signal is not present on the current DUT object or in the
generated `Frontend_top.sv` / `signals.json`, remove it from the bundle or
treat it as intentionally optional; do not keep historical or guessed signal
names in the active env contract.

The current Frontend package intentionally has two layers:

1. root-level facade modules such as `Frontend_api.py` and `Frontend_env.py`
2. the real implementation under `src/test/python/Frontend/env/`

Treat the root-level facade as part of the supported import contract unless the
task is explicitly about changing that contract. Do not silently bypass or
remove it as “just compatibility glue”;
`src/test/python/Frontend/tests/py/environment/test_layout_import_compat.py` exists to keep
that boundary stable.

## Working Rules

- Prefer the smallest path that satisfies the request.
- When adding a contract test for a script, wrapper, or runner, place it in a
  dedicated test file named after that entrypoint. Do not hide runner contracts
  inside an unrelated feature test file.
- Do not add low-signal tests under `tests/py/environment/` merely to assert
  script text, help output, option names, or a hard-coded testcase selection.
  Use direct shell syntax/help checks for those properties unless the runner
  implements a stable behavior that needs a meaningful regression contract.
- Every log printed by the verification environment must help debug a real
  failure and be as short as practical. Do not add noisy, redundant, or
  narrative logging.
- Follow `src/test/python/Frontend/docs/03_funcov_model/skills.md` for all
  testpoint, coverage-target, functional-coverage, and back-annotation rules.
- Extend an existing regression when it can express the scenario without
  weakening its semantic contract; otherwise add a focused new testcase.
- Build verification-environment APIs, monitors, and oracles around reusable
  DUT behaviors, not individual testpoint names or directed testcase shapes.
  Collect the default functional coverage through reproducible regressions;
  add a directed testcase only for the small set of semantics that cannot be
  reached or judged reliably by that regression contract.
- Keep only regressions with stable frontend semantics and explicit pass/fail
  assertions. Temporary edit checks and exploratory reproducers must not become
  permanent tests.
- Long-lived tests should cover a meaningful stream or boundary. Early-ending
  exception cases must assert the DUT-visible exception and suppress any illegal
  resend or follow-up request.
- Keep each testcase centered on one primary observable behavior. Make relevant
  PC boundaries and predictor training/check phases explicit in the instruction
  stream instead of hiding semantics in host-side stimulus.
- Randomized testcase generation must record enough seed and parameter data to
  reproduce the exact instruction stream.
- Do not frequently add low-signal or redundant cases to
  `src/test/python/Frontend/tests/py/environment/test_backend_model_unit.py`; only add a test
  there when it captures a distinct semantic contract, blocks a proven
  regression, or is the smallest meaningful reproducer for the root cause being
  fixed.
- Do not distort implementation semantics to satisfy an invalid expectation;
  prove the contract before updating the test.
- When a frontend/backend semantic refactor is still incomplete, do not run any
  testcase, regression, or bin-trace reproduction until the refactor owner
  judges the new model complete enough for validation; do not use intermediate
  failing runs as a substitute for finishing the rewrite.
- For each verification alignment, record the exact `frontend-bt` commit and the
  corresponding `design_baseline_sha` from the `kunminghu-v3` merge already present in
  that commit. Do not independently synchronize from `kunminghu-v3`.
- Before changing backend-agent semantics or related logic, run
  `docs/agents/frontend-backend-agent.md` section `实现一致性最小检查项`
  in order: `必须项` first, then `建议项`.
- When changing bundles, coverage points, or startup/control wiring, verify
  every signal name against the current DUT object and generated artifacts
  first. Required signals should fail fast when absent; signals not present on
  the DUT should not remain in the active contract.
- After changing code, rerun the relevant tests before giving a conclusion. If
  you have not rerun the relevant tests yet, say that explicitly and do not
  present the result as a validated conclusion.
- When DUT behavior is coupled to env-generated stimuli, first suspect env
  stimulus generation or timing before concluding there is an obvious DUT bug.
  Only escalate to a DUT-side diagnosis after the env stimulus path has been
  checked against waveforms and the semantic contract.
- For any DUT bin-trace failure, do not bypass or mask the failing condition
  with reduced step count, partial execution, relaxed completion criteria, or
  similar workarounds. Use `docs/agents/frontend-debugging.md` as the normative
  root-cause workflow before attempting another behavioral change.

## Commit Message Rules

Inspect the staged diff first. Use a single-line `type(frontend): summary`
subject that describes the concrete change; choose another lowercase scope only
when it is more accurate.

## Build And Test

Use the Frontend Python environment documented in
`src/test/python/Frontend/README.md`; do not mix in an incompatible
`libxspcomm.so`. The README and script `--help` output are the source of truth
for build and run commands.

The default build uses Verilator; the VCS build compiles FSDB support without
embedding a shared startup waveform path. Their packages coexist under
`build-frontend/pylib-<sim>/Frontend/` and are selected at runtime with
`TB_FRONTEND_SIM`.

Both build targets first run the `verilog` dependency when the Chisel/Scala
inputs have changed, with `BUILD_DIR=build-frontend`, so the full RTL is emitted
directly under `build-frontend/rtl/`. Picker selects
`build-frontend/rtl/Frontend.sv` with
`--sname Frontend --tname Frontend`; neither target runs `FrontendTopMain`.
The VCS target appends the funcov SV sources after the complete RTL list in
`build-frontend/full-rtl-picker.funcov.f`.

Frontend helper scripts disable `pytest_rerunfailures` in sandboxed runs. For
direct pytest, retain that behavior unless the plugin is intentionally needed.

- `TB_ENABLE_DUT_TESTS=1`: required for DUT integration cases guarded by the
  existing `_RUN_DUT` pattern.
- If a pytest result contains skips caused by `TB_ENABLE_DUT_TESTS` being unset,
  that result is not a final verification result. Immediately rerun the same
  selected cases with `TB_ENABLE_DUT_TESTS=1`; only skips caused by a separate,
  documented prerequisite (for example missing bin/trace input) may remain and
  must be reported explicitly.
- A DUT batch regression is complete only if pytest reaches the final summary
  and the selected/completed case count matches the intended target.

With `TB_FRONTEND_SIM=vcs`, `TB_SKIP_DUT_FINISH=1` reuses one DUT and calls
`dut.Finish()` once at pytest session teardown. It dumps the coverage accumulated
by all cases as `<run_id>_vcs_batch`. When `TB_ENABLE_DUT_COVERAGE=1`, the
compiled VDB is treated as a read-only template and copied to
`$TB_COVERAGE_DIR/Frontend.vdb`, or to
`build-frontend/artifacts/<run_id>/Frontend.vdb` without an override.
Concurrent processes must use different run roots. Merge their completed VDBs
with URG after all processes finish.

## Bin-Trace Workflow

Treat `src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh` as the
supported single-case bin-trace entrypoint for ready-to-run cases.
`run_bin_trace_suite.sh` is the supported curated regression wrapper and should
delegate each selected bin to the single-case pipeline.

When preparing binaries for the frontend_bt NEMU configuration whose memory
image starts at `0x10000000` while the frontend reset vector is `0x10001000`,
the runnable `.bin` must include the leading `0x1000` zero-byte padding. Name
the final runnable file as the case `.bin`; do not add `_padded` to the
filename. Keep ELF/map artifacts only for address inspection and disassembly.

Do not add a tracked default active-bin list. Keep the selected bin set explicit
on the command line or in a user-provided `--list-file <path>` outside the
commit unless the list itself is intentionally under review. Leave long-running
workloads out unless they are explicitly requested for that run.

Run bin cases only through `run_bin_trace_pipeline.sh` for one binary or
`run_bin_trace_suite.sh` for a selected list. The runners provide the required
pipeline gate, trace input, runtime bounds, and artifact layout; direct pytest
invocation is not a supported bin-case workflow.

Use the runners' `--help` output for paths, environment variables, logging, and
runtime controls. Bounded cursor, trace, instruction, or cycle limits are debug
evidence only and do not establish a complete bin-trace pass.

Although the runners use pytest internally for the DUT stage, a bin-trace case
is a bin/trace integration regression, not a Python testcase. Exclude bin-trace
cases from all-Python testcase selection and pass/fail counts; report them only
through their shell-runner result.

## Bin-Trace Requirements

Any DUT bin-trace case must meet the following operational requirements:

- the DUT stage must have a hard runtime upper bound
- every final evidence run must generate a waveform artifact
- every run must generate a readable log artifact
- artifacts must follow the unique run-root rules below

In addition, bin-trace runs must have explicit runtime observability. Do not
run them as opaque long-running jobs with no bounded diagnostics. A valid
bin-trace run must provide at least one of the following while it is running:

- progress checkpoints
- stall snapshots
- an equivalent explicit observation mechanism that can distinguish
  “still making progress” from “stuck”

If progress stops, the runner must report the stall and fail within its bounds;
do not leave the process in a silent loop. Keep progress, stall, and timeout
controls enabled during diagnosis before changing semantic logic.

## Artifact Naming

- Use one unique `run_id` for every invocation and keep its outputs under one
  run root, normally `build-frontend/artifacts/<run_id>/`.
- A suite must allocate a separate run ID and `cases/<case_stem>/` directory for
  every case. Cases must not write into one shared live directory.
- Non-bin tests use the run root directly; bin-trace runners separate coverage,
  waveforms, funcov, and logs into subdirectories.
- Waveform and log names identify the testcase or binary. Verilator waveforms
  use `.fst` or `.vcd` and coverage uses `.dat`; VCS waveforms use `.fsdb` and
  coverage uses run-local `Frontend.vdb`.
- Case logs are enabled by default through `TB_ENABLE_CASE_LOG=1`; set it to
  `0` only when intentionally suppressing a per-case log.
- `TB_WAVEFORM_PATH`, `TB_WAVEFORM_DIR`, `TB_CASE_LOG_PATH`, and
  `TB_COVERAGE_DIR` override default artifact locations. Every override used by
  a regression or suite must remain unique to that run.
- Historical date directories are read-only evidence and must not be reused as
  current run destinations.
- Keep wrapper and pipeline logs in the same run root as their DUT artifacts.

## Test Authoring Rules

- Add DUT-behavior Python regressions under
  `src/test/python/Frontend/tests/py/<author>/`; add environment-only tests
  that do not exercise DUT behavior under
  `src/test/python/Frontend/tests/py/environment/`; add assembly regressions
  under `src/test/python/Frontend/tests/asm_cases/<author>/`.
- Name tests `test_*.py`.
- Reuse fixtures from `src/test/python/Frontend/env/runtime/fixtures.py`.
- Follow `src/test/python/Frontend/README.md` for Verilator `.dat` reporting and
  reviewed ignore/omit handling.
- Do not commit transient logs, generated waveforms, or other temporary
  artifacts unless they are intentional fixtures.

## Deeper References

- `docs/testbench/Guide_Doc/dut_fixture.md`
- `docs/testbench/Guide_Doc/dut_api_instruction.md`
- `src/test/python/Frontend/docs/03_funcov_model/skills.md`
- `docs/testbench/testbench_stages.yaml`
