# Frontend Verification Guide

## Scope

Treat the project root as `$NOOP_HOME`.

Most agent work in this repository is expected to target the frontend Python
verification stack under `src/test/python/Frontend/`, not the whole XiangShan
tree. Start here unless the task explicitly says otherwise.

## File Map

- `src/test/python/Frontend/Frontend_api.py`: stable root-level DUT fixture
  and `api_Frontend_*` re-export facade.
- `src/test/python/Frontend/Frontend_env.py`: stable root-level
  `FrontendEnv` / fixture re-export facade.
- `src/test/python/Frontend/env/frontend_env.py`: top-level environment
  orchestration across DUT-facing collaborators.
- `src/test/python/Frontend/env/backend_model.py`: backend-side semantic model
  and resolve/redirect behavior.
- `src/test/python/Frontend/env/api.py`: public helper APIs used by tests and scripts.
- `src/test/python/Frontend/env/request_apis.py`: lower-level request helpers
  used by `env/api.py` for program load, redirect, and golden-trace execution.
- `src/test/python/Frontend/env/fixtures.py`: shared pytest fixtures and
  artifact setup. Prefer these over custom setup.
- `src/test/python/Frontend/env/dut_factory.py`: DUT construction entry used by
  fixtures.
- `src/test/python/Frontend/env/nemu_trace_pipeline.py`: NEMU trace generation
  helpers used by the API layer.
- `src/test/python/Frontend/env/functional_coverage.py`: functional coverage
  recorder and pilot-csv integration.
- `src/test/python/Frontend/env/coverage_def.py`: coverage definitions. Update
  when behavior changes introduce new scenario classes.
- `src/test/python/Frontend/env/agents/`: DUT-facing side agents such as
  ICache, PTW, uncache, and backend drive logic.
- `src/test/python/Frontend/env/model/`: semantic model helpers, golden trace
  state, FTQ scoreboards, and backend runtime state.
- `src/test/python/Frontend/env/monitor.py`: shared monitor-side data
  structures and helpers used by the active monitor layer.
- `src/test/python/Frontend/env/monitors/`: DUT observation and frontend-monitor logic.
- `src/test/python/Frontend/env/bundles/`: DUT interface bundle binding and signal contract layer.
- `src/test/python/Frontend/env/sequences/`: reusable env-side operational sequences.
- `src/test/python/Frontend/tools/nemu_bin_to_golden_trace.py`: build golden trace
  jsonl from a frontend bin through NEMU.
- `src/test/python/Frontend/tools/nemu_log_to_golden_trace.py`: convert existing
  NEMU logs into frontend golden trace json/jsonl.
- `src/test/python/Frontend/tools/run_dut_with_bin_trace.py`: load a bin and
  golden trace into the frontend DUT environment for bring-up/debug.
- `src/test/python/Frontend/scripts/fst_to_fsdb.sh`: convert a frontend `.fst`
  waveform to `.fsdb` through a temporary `.vcd`.
- `src/test/python/Frontend/tests/`: active frontend regressions. Put new tests here.
- `build-frontend/pylib/Frontend/`: generated Python bindings, shared objects,
  signal map, and `Frontend_top.sv`.
- `build-frontend/rtl/`: generated RTL artifacts useful for cross-checking DUT behavior.
- `ready-to-run/`: example DUT binaries used by frontend bin-trace investigations.
- `NEMU/logs/`: trace inputs used to reconstruct failing windows.

## Source Of Truth

Use the most direct artifact that reflects real DUT behavior:

1. Observed DUT-facing IO in the Python environment and tests.
2. Generated artifacts under `build-frontend/pylib/Frontend/`, especially
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
`src/test/python/Frontend/tests/test_layout_import_compat.py` exists to keep
that boundary stable.

## Working Rules

- If the requested path is not minimal, prefer the shorter path and explain the change in direction.
- When splitting batched commits, prefer `git add -p` first. Do not make direct documentation edits as part of the commit-splitting phase. If `git add -p` cannot express the desired split, stop and ask the user before changing files further.
- Never use `git push -f` under any circumstances.
- Every log printed by the verification environment must help debug a real
  failure and be as short as practical. Do not add noisy, redundant, or
  narrative logging.
- Any user-provided process constraint must be written to the relevant repo
  docs in the same turn; do not keep it only in chat memory.
- When maintaining frontend testpoint CSVs, keep the hierarchy meaningful:
  first-level testpoints are fixed by the source scope and must not be added
  casually; fourth-level testpoints should group a mechanism, path, state, or
  scenario class; fifth-level testpoints should be executable leaf cases with a
  concrete input combination, timing relation, boundary, or expected behavior.
  Do not leave a fourth-level and fifth-level testpoint identical after
  inherited CSV hierarchy is expanded. If an existing fourth-level name is
  already the leaf case, move it to the fifth level and rename the fourth level
  to a real category.
- Do not translate established frontend testpoint terms just to make the CSV
  read as Chinese prose. Keep domain terms such as `backend`, `redirect`,
  `flush`, `commit`, `fence.i`, `sfence`, `hit`, `miss`, `refill`, `MSHR`,
  `SRAM`, `BPU`, `FTQ`, `ICache`, `IBuffer`, and `InstrUncache` in English
  when that is the normal project vocabulary. This does not permit Chisel code
  snippets, signal-field expressions, or source references inside the testpoint
  text.
- When writing frontend testpoint descriptions, use natural language that is
  easy to understand. Do not invent awkward shorthand terms such as "clean
  return"; describe the observable behavior directly, for example "the uncache
  response returns instruction data without `corrupt` or `denied`".
- When filling a frontend testpoint CSV `Condition` column, write the trigger
  condition, input state, timing relation, or boundary setup needed to exercise
  the fifth-level testpoint. Derive that condition from the Chisel behavior
  under `src/main/scala/xiangshan/frontend` and frontend architecture knowledge,
  but do not put source paths, code snippets, or signal-field expressions in the
  CSV cell.
- Before constructing coverage for a new scenario, first check whether an
  existing regression case can be extended without weakening its semantic
  contract. If it can, extend or merge into that existing case; adding a new
  testcase is forbidden. Add a new testcase only when no existing case can
  encode the scenario cleanly.
- Do not change implementation code merely to satisfy an existing test when the
  test contradicts the intended frontend behavior. Update or remove the invalid
  expectation only after proving the semantic contract.
- Do not turn a minimal reproducer into a permanent frontend regression test
  unless it is suitable for continuous regression and carries real frontend
  semantics. Prefer fixing the root cause first and only keep regression
  coverage that matches the intended long-term contract.
- A testcase that only checks whether a code edit behaved as expected must stay
  temporary. Do not commit it under the regression test files; delete temporary
  tests once they have served the local validation purpose.
- Do not endlessly add minimal testcases to the regression suite. Any testcase
  written into versioned regression files or documented as an official case must
  be suitable for sustainable regression and must encode real frontend
  semantics. Tests that exist only to check whether a local code edit behaves as
  expected may be written as temporary tests, but they must not be placed in
  long-lived regression files and must be deleted periodically.
- Keep only functionally complete regression cases with clear pass/fail
  assertions. Delete or merge development-only tests, generated exploratory
  cases, cases that only send a few requests without proving DUT-visible
  behavior, and cases whose assertions do not encode a stable semantic
  contract.
- Long-lived regression tests must not merely prove that one request can pass.
  A kept testcase should cover a multi-request stream, a full multi-beat fetch
  block, or a multi-stage boundary such as request/response/flush/backpressure
  recovery. Exception cases that intentionally terminate early are acceptable
  only when they assert DUT-visible exception behavior and, when relevant, that
  an illegal resend or follow-up request does not occur.
- Do not frequently add low-signal or redundant cases to
  `src/test/python/Frontend/tests/test_backend_model_sync.py`; only add a test
  there when it captures a distinct semantic contract, blocks a proven
  regression, or is the smallest meaningful reproducer for the root cause being
  fixed.
- Do not abandon a semantically correct fix merely because existing tests turn
  red. When current tests are inconsistent with the intended frontend/backend
  contract, state that explicitly and update or remove the unreasonable tests
  instead of distorting the implementation to satisfy them.
- When a frontend/backend semantic refactor is still incomplete, do not run any
  testcase, regression, or bin-trace reproduction until the refactor owner
  judges the new model complete enough for validation; do not use intermediate
  failing runs as a substitute for finishing the rewrite.
- Before pushing frontend work, run `proxychains git fetch origin <current-branch>` and then
  `git rebase origin/<current-branch>`.
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

When a frontend change is committed, the commit subject must follow the same
format used by recent history under `src/test/python/Frontend/`:

- use `type(scope): summary`
- keep `type` and `scope` lowercase
- for frontend verification changes, use `frontend` as the default scope
- choose `type` from the actual change intent, for example:
  `fix`, `feat`, `refactor`, `docs`, `test`, `chore`

The summary must be derived from the staged file content, not from a vague
intention. Therefore:

- inspect the staged diff before writing the message
- describe the concrete behavior or artifact change in the subject
- do not use generic subjects such as `update frontend`, `misc fixes`, or
  `tune logic`
- if the commit mainly removes tests or checks that contradict the documented
  semantic contract, say so explicitly instead of pretending it is a feature
  change
- commit message must be a single line only; do not add a multi-line body

## Build And Test

Activate the environments before running frontend commands:

```bash
source /nfs/share/unitychip/activate
source /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate
cd "$NOOP_HOME"
```

In sandboxed runs, disable the environment-level `pytest_rerunfailures` plugin
by default. It opens a local socket during `pytest_configure` and otherwise
fails before the testcase starts. Frontend helper scripts in this tree already
do this by passing `-p no:rerunfailures`. If you invoke `pytest` directly,
include the same flag unless you intentionally need that plugin outside the
sandbox.

Run the default frontend regression flow from the repo root:

```bash
src/test/python/Frontend/scripts/run_pytest_with_log.sh
```

Run a narrower frontend test:

```bash
TB_ENABLE_DUT_TESTS=1 python -m pytest -p no:rerunfailures \
  src/test/python/Frontend/tests/test_multi_branch.py -v
```

Common direct-`pytest` arguments worth keeping consistent:

- `-p no:rerunfailures`: disable the environment-level rerun plugin in
  sandboxed/manual runs unless you intentionally need it.
- `-s`: keep live stdout/stderr visible while debugging DUT/env interaction.
- `-o log_cli=true --log-cli-level=INFO`: print env logs to the terminal; raise
  the level only when the extra noise is justified.
- `-v`: keep testcase nodeids visible so logs/artifacts can be matched back to
  the exact run.
- `TB_ENABLE_DUT_TESTS=1`: required for DUT integration cases guarded by the
  existing `_RUN_DUT` pattern.

`src/test/python/Frontend/scripts/run_pytest_with_log.sh` already sets the
logging-related pytest arguments above and disables `rerunfailures` by default.
Use direct `pytest` mainly when you need a narrower target or explicit env vars.

`src/test/python/Frontend/scripts/run_pytest_with_log.sh` also accepts these
script-level env vars:

- `TB_LOG_CLI_LEVEL=...`: override the CLI log level; defaults to
  `TB_ENV_LOG_LEVEL`, then `INFO`.
- `TB_PYTEST_DISABLE_RERUNFAILURES=0|1`: keep or disable
  `-p no:rerunfailures`; default is `1` in this tree.
- `TB_TRACE_START_INDEX=...`: start golden comparison from the given jsonl
  index when loading a bin trace.
- `TB_RESET_VECTOR=...`: start DUT fetch from the given PC instead of the
  default `0x80000000`; defaults to `TB_BASE_ADDR` in the bin-trace pipeline.
- `TB_REG_LOG_DIR=...`: override the default regression-log directory.
- `TB_REG_LOG_FILE=...`: write the tee'd regression log to an explicit file
  path instead of the timestamped default.

Run the fast frontend smoke guard used by the local change hook:

```bash
python src/test/python/Frontend/change_guard.py
```

Enable the versioned git hook so staged frontend changes run the smoke guard before commit:

```bash
git config core.hooksPath .githooks
```

## Bin-Trace Workflow

Treat `src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh` as the only
supported bin-trace entrypoint for ready-to-run cases.

When preparing binaries for the frontend_bt NEMU configuration whose memory
image starts at `0x10000000` while the frontend reset vector is `0x10001000`,
the runnable `.bin` must include the leading `0x1000` zero-byte padding. Name
the final runnable file as the case `.bin`; do not add `_padded` to the
filename. Keep ELF/map artifacts only for address inspection and disassembly.

### Standard Entry

The script generates the NEMU golden trace first, then runs
`tests/test_bin_trace_dut.py::test_bin_trace` with the pipeline-only
environment variables set consistently.

```bash
source /nfs/share/unitychip/activate
source /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate

BIN_TRACE_ENV=(
  TB_NEMU_EXEC=ready-to-run/riscv64-nemu-interpreter
)

timeout --foreground 1200 env "${BIN_TRACE_ENV[@]}" \
src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh ready-to-run/<case>.bin
```

`run_bin_trace_pipeline.sh` defaults both env logging and pytest CLI logging to
`INFO`, and defaults `PYTEST_ADDOPTS` to
`-s -o log_cli=true --log-cli-level=<cli-level>`. Use `TB_LOG_LEVEL=WARNING`
to quiet both env and pytest CLI logs, or use `TB_ENV_LOG_LEVEL` /
`TB_LOG_CLI_LEVEL` to override them separately.
It also defaults `TB_TRACE_STAGNANT_CYCLES_LIMIT` to `20000`,
`TB_TRACE_STALL_SNAPSHOT_INTERVAL` to `5000`, `TB_TRACE_TARGET_CURSOR` to `0`,
and `TB_PYTEST_TIMEOUT_SECS` to `6400`; set them explicitly only when a case
needs different observability, stop, or wall-clock timeout behavior.

### Variants

The script accepts optional explicit trace and NEMU-log paths after the
binary:

```bash
timeout --foreground 1200 env "${BIN_TRACE_ENV[@]}" \
src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh \
  ready-to-run/<case>.bin \
  NEMU/logs/<case>.trace.jsonl \
  NEMU/logs/<case>.nemu.log
```

Set `TB_RUN_DUT=0` only when you intentionally want to generate or refresh the
NEMU trace without running the DUT:

```bash
timeout --foreground 1200 env "${BIN_TRACE_ENV[@]}" TB_RUN_DUT=0 \
src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh ready-to-run/<case>.bin
```

Other `run_bin_trace_pipeline.sh` knobs worth recording:

- `TB_NEMU_MAX_INSTR=...`: pass `-I` to NEMU when you intentionally want an
  instruction cap during trace generation.
- `TB_TRACE_LIMIT=...`: truncate the converted golden trace when you
  intentionally want a shorter debug input.
- `TB_SKIP_NEMU=1`: skip NEMU trace generation and reuse the explicit or
  default JSONL trace path. Required when the trace was generated separately.
- `TB_LOG_LEVEL=...`: set both env and pytest CLI log levels; defaults to
  `INFO`. `INFO` prints `INFO`/`WARNING`/`ERROR`; `WARNING` prints only
  `WARNING`/`ERROR`.
- `TB_ENV_LOG_LEVEL=...`: override only the environment logger level.
- `TB_LOG_CLI_LEVEL=...`: override only pytest CLI log level.
- `TB_PYTEST_TARGET=...`: replace the default
  `tests/test_bin_trace_dut.py::test_bin_trace` nodeid.
- `TB_TRACE_MAX_CYCLES=...`: bound DUT execution cycles for a debug run; `0`
  keeps the run-to-completion behavior.
- `TB_TRACE_TARGET_CURSOR=...`: treat reaching this golden-trace cursor as pass.
  Use `0` to disable (default). When set to a positive integer, run stops with
  pass as soon as `cursor >= target`.
- `TB_PYTEST_TIMEOUT_SECS=...`: set the DUT-stage wall-clock timeout used by
  the pipeline script; default is `6400`.
- `TB_TRACE_STALL_SNAPSHOT_INTERVAL=...`: print stall snapshots every N
  stagnant cycles; default is `5000`, and `0` disables snapshot printing.
- `TB_TRACE_STAGNANT_CYCLES_LIMIT=...`: fail when the golden cursor is stagnant
  for this many cycles; default is `20000`.
- `PYTHON=...`: choose a non-default Python executable for the helper tools and
  pytest stage.

Use direct `pytest` only when the golden trace has already been prepared and
the pipeline-only environment variables are set explicitly:

```bash
timeout --foreground 900 env "${BIN_TRACE_ENV[@]}" \
TB_ENABLE_DUT_TESTS=1 \
TB_BIN_TRACE_PIPELINE=1 \
TB_BIN_PATH=ready-to-run/<case>.bin \
TB_TRACE_PATH=NEMU/logs/<case>.trace.jsonl \
TB_BASE_ADDR=0x80000000 \
TB_TRACE_TARGET_CURSOR=0 \
python -m pytest -p no:rerunfailures -v \
  src/test/python/Frontend/tests/test_bin_trace_dut.py::test_bin_trace
```

For this direct-`pytest` path, keep the required environment variables explicit:

- `TB_ENABLE_DUT_TESTS=1`: enable DUT integration instead of the fake-DUT path.
- `TB_BIN_TRACE_PIPELINE=1`: satisfy the pipeline-only gate on
  `test_bin_trace`.
- `TB_BIN_PATH=...`: point the testcase at the exact ready-to-run binary.
- `TB_TRACE_PATH=...`: point the testcase at the prepared golden trace file.
- `TB_BASE_ADDR=...`: keep the DUT load base consistent with the binary image.
- `TB_TRACE_TARGET_CURSOR=...`: optional cursor target for early pass; `0`
  disables it.
- `TB_TRACE_STAGNANT_CYCLES_LIMIT=...`: keep stagnant-cycle early-stop enabled
  so the run fails on real forward-progress stalls instead of hanging silently.

Do not treat the bare command
`TB_ENABLE_DUT_TESTS=1 python -m pytest ...::test_bin_trace`
as a complete bin-case workflow. The test is pipeline-gated and requires the
bin/trace environment above.

More generally: direct `pytest` is only valid for a bin-trace case when the
trace input, bin path, runtime bounds, and observability-related environment are
already set to match that case's run requirements.

### Artifacts

After a run, look for artifacts in the date-stamped frontend data directory,
for example `src/test/python/Frontend/data/<YYYYMMDD>/`. Bin-trace per-case
`.log` and `.fst` filenames include the binary stem and pytest case name:
`<case>_test_bin_trace.log` and `<case>_test_bin_trace.fst`. Non-bin DUT cases
also use the same date directory; their filenames are based on the pytest case
name.

Functional coverage artifacts are written separately under
`src/test/python/Frontend/data/funcov/`.

`src/test/python/Frontend/tests/test_bin_trace_dut.py::test_bin_trace` is a
run-to-completion entrypoint. Do not use it as a load-only or partial-step
smoke path. By default it runs until golden completion; set
`TB_TRACE_MAX_CYCLES` to a positive integer only when you intentionally want a
bounded debug run, or set `TB_TRACE_TARGET_CURSOR` to stop at a specific
golden cursor and treat it as pass. In bounded mode, exhausting the explicit
cycle budget is not itself a failure; only observed DUT/env misbehavior such as
monitor errors or stagnant-cycle early-stop should fail the run.

For direct `pytest`, use an outer `timeout` guard explicitly. The
`TB_PYTEST_TIMEOUT_SECS` knob is consumed by `scripts/run_bin_trace_pipeline.sh`; it
does not add a wall-clock bound by itself when you bypass the pipeline script.

## Bin-Trace Requirements

Any DUT bin-trace case, especially
`src/test/python/Frontend/tests/test_bin_trace_dut.py::test_bin_trace`, must
meet the following operational requirements:

- every run must have a hard runtime upper bound; unbounded bin runs are not
  allowed
- every run must generate a waveform artifact
- every run must generate a readable log artifact
- the default artifact location should be a date-stamped directory under
  `src/test/python/Frontend/data/` unless an explicit path override is given
- waveform and log filenames should be logically tied to the binary and test
  case, so the reproduction can be matched back to the exact run

In addition, bin-trace runs must have explicit runtime observability. Do not
run them as opaque long-running jobs with no bounded diagnostics. A valid
bin-trace run must provide at least one of the following while it is running:

- progress checkpoints
- stall snapshots
- an equivalent explicit observation mechanism that can distinguish
  “still making progress” from “stuck”

Therefore:

- a bin-trace case must not be allowed to sit in a silent apparent dead loop
  after progress has stopped
- if forward progress stalls, the run should surface that fact through logs or
  an equivalent observation channel instead of only hanging until external kill
  or manual interruption
- for DUT bin runs, set `TB_TRACE_STAGNANT_CYCLES_LIMIT` and fail early when
  golden-trace cursor stops advancing (pipeline default: `20000`)
- for pipeline runs, use `TB_PYTEST_TIMEOUT_SECS` to set the DUT-stage timeout;
  default is `900` seconds
- in interactive debug sessions, do not launch open-ended DUT bin runs; always
  keep `TB_TRACE_STAGNANT_CYCLES_LIMIT` and `TB_PYTEST_TIMEOUT_SECS` enabled
  and prefer wrapping the pipeline command with an outer `timeout` guard
- when debugging a stuck bin-trace case, prefer enabling progress/stall
  reporting before changing semantic logic
- for any bin-trace reproduction, prefer `scripts/run_bin_trace_pipeline.sh` with the
  NEMU executable available in the current tree; do not assume a historical
  `NEMU/build/...` path exists locally unless you have verified it

Implementation Notes:

- `scripts/run_bin_trace_pipeline.sh` enforces positive values for
  `TB_TRACE_STAGNANT_CYCLES_LIMIT` and `TB_PYTEST_TIMEOUT_SECS`, and enforces
  non-negative values for `TB_TRACE_TARGET_CURSOR` and
  `TB_TRACE_STALL_SNAPSHOT_INTERVAL`
- waveform dumping is enabled by default, but can still be disabled with
  `TB_ENABLE_FST_DUMP=0`
- `env/fixtures.py` accepts `TB_WAVEFORM_PATH`, `TB_WAVEFORM_DIR`, and
  `TB_CASE_LOG_PATH` as explicit artifact-path overrides
- golden progress is printed by the Python golden-trace runner as the cursor
  advances; `TB_TRACE_PROGRESS_INTERVAL` only enables additional logger
  checkpoints and is not required for normal progress visibility
- stall snapshots are printed every `TB_TRACE_STALL_SNAPSHOT_INTERVAL`
  stagnant cycles; the script default is `5000`, and `0` disables snapshots
- paired per-case log files are created by `env/fixtures.py` when `TB_BIN_PATH`
  is set

## Utilities

Start the frontend web console:

```bash
TB_ENV_LOG_LEVEL=INFO \
TB_WEB_HOST=127.0.0.1 \
TB_WEB_PORT=8000 \
src/test/python/Frontend/scripts/run_web_console.sh
```

Convert a frontend FST waveform to FSDB:

```bash
src/test/python/Frontend/scripts/fst_to_fsdb.sh path/to/wave.fst [path/to/wave.fsdb]
```

Rebuild the frontend Python DUT artifacts from the repo root:

```bash
source /nfs/share/unitychip/activate
cd "$NOOP_HOME"
make frontend -j
```

## Test Authoring Rules

- Add new regressions under `src/test/python/Frontend/tests/`.
- Name tests `test_*.py`.
- Gate DUT-only cases with `TB_ENABLE_DUT_TESTS=1` and existing `_RUN_DUT` patterns.
- Reuse fixtures from `src/test/python/Frontend/env/fixtures.py`.
- Update `src/test/python/Frontend/env/coverage_def.py` when introducing new
  fetch, branch, redirect, exception, or performance scenario coverage.
- Do not commit transient logs, generated waveforms, or other temporary
  artifacts unless they are intentional fixtures.

### Frontend Testcase Design

Keep frontend testcase design centered on controllable instruction flow rather
than host-side test scaffolding.

- Each testcase should target one primary frontend behavior such as fetch
  sequencing, branch direction, target prediction, return prediction,
  redirect recovery, or a boundary condition. Do not mix multiple unrelated
  goals into one case when a smaller case can isolate the behavior.
- When the testcase depends on frontend position semantics, make the PC shape
  intentional. Use labels, alignment, and padding so fetch-block boundaries,
  CFI offsets, and redirect targets are predictable from the source.
- For predictor-training cases, separate the stable training phase from the
  behavior-check phase. The testcase structure should make it obvious which
  instructions build predictor state and which instructions validate reuse,
  mispredict handling, or recovery.
- Prefer describing the testcase in terms of instruction stream and expected
  PC/control-flow behavior. Keep environment-side logic focused on load,
  control, observation, and comparison; do not hide testcase semantics inside
  custom host-side stimulus when the same behavior can be expressed in the
  program itself.
- Any randomized testcase generation must remain reproducible. Record the
  seed and generation parameters in the checked-in artifact or in a stable
  regeneration path, so the exact instruction stream can be reproduced later.

## Artifact Naming

- When you generate debugging waveforms under
  `src/test/python/Frontend/data/`, place them in a date folder instead of
  dumping them directly in `data/`.
- Use a stable date folder name such as `src/test/python/Frontend/data/20260414/`.
- Waveform filenames must be logically named from the reproduction context, for
  example: test name, binary name, seed, and purpose or fix tag.
- Prefer names in the form
  `<test-or-bin>_<seed-or-case>_<purpose>.<ext>`; once the file already lives
  under a date directory, do not repeat the date in the filename.
- Apply the same naming discipline to paired debug logs when you intentionally
  keep them under `data/`.

Current default implementation details in `env/fixtures.py`:

- waveform, case log, and coverage `.dat` default to a date-stamped
  subdirectory under `src/test/python/Frontend/data/`, regardless of whether
  `TB_BIN_PATH` is set
- non-bin DUT tests follow the same dated artifact layout as bin-trace tests;
  do not leave their `.fst`, `.log`, or `.dat` files directly under `data/`
- default waveform file name is `<bin-stem>_<test-name>.<wave-ext>`
- default `wave-ext` is `fst`
- if frontend is rebuilt with `FRONTEND_WAVEFORM_FORMAT=vcd`, default `wave-ext` becomes `vcd` and later `make frontend` runs keep using `vcd` until `FRONTEND_WAVEFORM_FORMAT=fst` is specified explicitly
- default case log file name is `<bin-stem>_<test-name>.log`
- when `TB_BIN_PATH` is unset, the artifact stem is the pytest case name
- when `TB_BIN_PATH` is set, the artifact stem is
  `<bin-stem>_<test-name>`
- case logs are enabled by default through `TB_ENABLE_CASE_LOG=1`; set it to
  `0` only when intentionally suppressing per-case logs
- `TB_WAVEFORM_PATH`, `TB_WAVEFORM_DIR`, `TB_CASE_LOG_PATH`, and
  `TB_COVERAGE_DIR` still override the default locations when explicitly set

## Deeper References

- `docs/testbench/Guide_Doc/dut_fixture.md`
- `docs/testbench/Guide_Doc/dut_api_instruction.md`
- `docs/testbench/Guide_Doc/dut_function_coverage_def.md`
- `docs/testbench/testbench_stages.yaml`
