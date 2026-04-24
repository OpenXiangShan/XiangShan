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
- `src/test/python/Frontend/env/fixtures.py`: shared pytest fixtures and
  artifact setup. Prefer these over custom setup.
- `src/test/python/Frontend/env/coverage_def.py`: coverage definitions. Update
  when behavior changes introduce new scenario classes.
- `src/test/python/Frontend/env/agents/`: DUT-facing side agents such as
  ICache, PTW, uncache, and backend drive logic.
- `src/test/python/Frontend/env/model/`: semantic model helpers, golden trace
  state, FTQ scoreboards, and backend runtime state.
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

- Start from first principles and confirm the real goal before editing code.
- If the requested path is not minimal, prefer the shorter path and explain the change in direction.
- Do not add compatibility shims, fallback logic, or speculative extensions.
- Do not rewrite absolute paths in any `source` command.
- Preserve the black-box verification boundary. Interact through DUT-facing
  agents, APIs, traces, and generated artifacts rather than assuming hidden RTL
  state.
- Before modifying files, inspect current local changes and avoid overwriting in-progress work.
- Any user-provided process constraint must be written to the relevant repo
  docs in the same turn; do not keep it only in chat memory.
- Do not hide or bypass a real failure just to make a test pass. When a test
  cannot pass, identify the true root cause first and only then change code. If
  the cause is still unproven, stop and discuss with the user before
  proceeding.
- Do not turn a minimal reproducer into a permanent frontend regression test
  unless the user explicitly asks for that. Prefer fixing the root cause first
  and only keep regression coverage that matches the intended long-term
  contract.
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
- After every `git commit`, run `git fetch origin <current-branch>` and then
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

## Build And Test

Activate the environments before running frontend commands:

```bash
source /nfs/share/unitychip/activate
source /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate
cd "$NOOP_HOME"
```

Run the default frontend regression flow from the repo root:

```bash
src/test/python/Frontend/scripts/run_pytest_with_log.sh
```

Run a narrower frontend test:

```bash
TB_ENABLE_DUT_TESTS=1 python -m pytest src/test/python/Frontend/tests/test_multi_branch.py -v
```

Run the fast frontend smoke guard used by the local change hook:

```bash
python src/test/python/Frontend/change_guard.py
```

Enable the versioned git hook so staged frontend changes run the smoke guard before commit:

```bash
git config core.hooksPath .githooks
```

For DUT bin-trace work, distinguish between:

- general run requirements that apply to any bin/trace reproduction
- the current checked-in example commands in this tree

The examples below use `microbench.bin` and
`tests/test_bin_trace_dut.py::test_bin_trace` because those assets exist in the
current tree. Do not mistake that example pair for the whole frontend
bin-trace contract.

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

## Bin-Trace Examples

Current checked-in example: run the standard DUT bin-trace pipeline for
`microbench.bin`:

```bash
source /nfs/share/unitychip/activate
source /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate
TB_NEMU_EXEC=ready-to-run/riscv64-nemu-interpreter \
TB_ENV_LOG_LEVEL=INFO \
TB_TRACE_PROGRESS_INTERVAL=50000 \
TB_TRACE_STALL_SNAPSHOT_INTERVAL=5000 \
TB_TRACE_STAGNANT_CYCLES_LIMIT=20000 \
TB_PYTEST_TIMEOUT_SECS=900 \
PYTEST_ADDOPTS='-s -o log_cli=true --log-cli-level=INFO' \
src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh ready-to-run/microbench.bin
```

Current checked-in example: use direct `pytest` only when the golden trace has
already been prepared and the pipeline-only environment variables are set
explicitly:

```bash
timeout --foreground 900 env \
TB_ENABLE_DUT_TESTS=1 \
TB_BIN_TRACE_PIPELINE=1 \
TB_BIN_PATH=ready-to-run/microbench.bin \
TB_TRACE_PATH=NEMU/logs/microbench.trace.jsonl \
TB_BASE_ADDR=0x80000000 \
TB_TRACE_PROGRESS_INTERVAL=50000 \
TB_TRACE_STALL_SNAPSHOT_INTERVAL=5000 \
TB_TRACE_STAGNANT_CYCLES_LIMIT=20000 \
python -m pytest -v src/test/python/Frontend/tests/test_bin_trace_dut.py::test_bin_trace
```

Do not treat the bare command
`TB_ENABLE_DUT_TESTS=1 python -m pytest ...::test_bin_trace`
as a complete bin-case workflow. The test is pipeline-gated and requires the
bin/trace environment above.

More generally: direct `pytest` is only valid for a bin-trace case when the
trace input, bin path, runtime bounds, and observability-related environment are
already set to match that case's run requirements.

`src/test/python/Frontend/tests/test_bin_trace_dut.py::test_bin_trace` is a
run-to-completion entrypoint. Do not use it as a load-only or partial-step
smoke path. By default it runs until golden completion; set
`TB_TRACE_MAX_CYCLES` to a positive integer only when you intentionally want a
bounded debug run. In that bounded mode, exhausting the explicit cycle budget is
not itself a failure; only observed DUT/env misbehavior such as monitor errors
or stagnant-cycle early-stop should fail the run.

For direct `pytest`, use an outer `timeout` guard explicitly. The
`TB_PYTEST_TIMEOUT_SECS` knob is consumed by `scripts/run_bin_trace_pipeline.sh`; it
does not add a wall-clock bound by itself when you bypass the pipeline script.

## Bin-Trace Run Requirements

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

This section mixes two kinds of statements:

- operational policy that frontend bin-trace runs are expected to satisfy
- behavior that the current scripts and fixtures already enforce automatically

Do not assume every requirement below is automatically enforced by the current
implementation. Verify the exact behavior in `scripts/run_bin_trace_pipeline.sh`,
`env/fixtures.py`, and `env/api.py` when the distinction matters.

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

Current implementation notes:

- `scripts/run_bin_trace_pipeline.sh` enforces positive values for
  `TB_TRACE_STAGNANT_CYCLES_LIMIT` and `TB_PYTEST_TIMEOUT_SECS`
- waveform dumping is enabled by default, but can still be disabled with
  `TB_ENABLE_FST_DUMP=0`
- progress checkpoints are only printed when `TB_TRACE_PROGRESS_INTERVAL` is set
- stall snapshots are only printed when `TB_TRACE_STALL_SNAPSHOT_INTERVAL` is
  set
- paired per-case log files are created by `env/fixtures.py` when `TB_BIN_PATH`
  is set

Start the frontend web console:

```bash
TB_ENV_LOG_LEVEL=INFO src/test/python/Frontend/scripts/run_web_console.sh
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

- when `TB_BIN_PATH` is unset, artifacts default to `src/test/python/Frontend/data/`
- when `TB_BIN_PATH` is set, waveform and case log default to a date-stamped
  subdirectory under `data/`
- default waveform file name is `<bin-stem>_<test-name>.vcd`
- default case log file name is `<bin-stem>_<test-name>.log`
- coverage `.dat` output currently stays under `data/` rather than the
  date-stamped subdirectory

## Deeper References

- `docs/testbench/Guide_Doc/dut_fixture.md`
- `docs/testbench/Guide_Doc/dut_api_instruction.md`
- `docs/testbench/Guide_Doc/dut_function_coverage_def.md`
- `docs/testbench/testbench_stages.yaml`
