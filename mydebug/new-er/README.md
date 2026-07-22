# Int Early Release Debug Protocol

This directory stores system-level debug history for the int-only early register release work. Every future observe-only smoke, functional early-free run, and emu-basics failure debug round must use this protocol before making a fix.

## Read History First

Before starting any new debug round:

1. Read this `README.md`.
2. Determine the stable task tag for the current implementation task.
3. List and read all prior records for that task tag in `mydebug/new-er/records/`.
4. Reuse task-scoped symptom notes, waveform conclusions, and rejected hypotheses before adding a new hypothesis.
5. Do not bulk-read records from other tasks. Read a cross-task record only when a current-task record explicitly references it and explains why it is relevant to the current symptom.
6. If no prior records exist for the current task tag, write `Prior records for this task: none` in the new record.

Do not debug a system-level failure from memory only. The first action must be reading the complete history for the current task, not the complete history of every IntER task.

## Debug Record Location

Use this layout:

```text
mydebug/new-er/
  README.md
  records/
    YYYYMMDD-HHMMSS-<task-tag>-<short-test-name>.md
  artifacts/
    YYYYMMDD-HHMMSS-<task-tag>-<short-test-name>/
      command.txt
      config.txt
      stdout.log
      stderr.log
      waves/
      counters/
```

Record filenames must be stable, sortable, and task-scoped. Use the local timestamp, a stable task tag shared by every round of the same implementation task, and a short testcase name, for example `20260622-013000-int-er-writeback-resolve-riscv-tests-add.md`.

## Hit-Good-Trap Rule

Classify a testcase by its final architectural result, not by isolated text inside the log.

- If the testcase reaches final `hit-good-trap`, the testcase passes.
- Some tests, especially rvh-test style self-checking tests, may print `failed` as part of their normal internal output. If the final result is `hit-good-trap`, treat that run as pass.
- If the testcase does not reach `hit-good-trap`, it fails even if the log contains expected intermediate self-check text.
- Always quote or summarize the final pass/fail line in the debug record.

## Wave And Log Capture

For every system-level failure, archive enough information for another engineer to reproduce the observation:

- command: exact build or run command, including `scripts/xiangshan.py` arguments and environment variables that affect output.
- configuration: relevant ER parameters, including `enable`, `observeOnly`, `trackEntries`, `earlyFreeWidth`, and conservative redirect policy.
- stdout: path to the captured standard output log.
- stderr: path to the captured standard error log.
- wave: path to generated `.fst` or `.vcd` files, plus the signal window inspected.
- counters: paths or excerpts for ER counters such as `int_er_uc_saturated_fallback`, DataPath read observation counters, ROB readDone counters, early-free counters, and suppress counters when available.
- symptom: concrete first failing symptom with cycle, PC, ROB index, physical register, logical register, or Difftest mismatch data when available.
- hypothesis: current technical explanation and why it fits the evidence.
- root cause: confirmed cause after waveform/log inspection, or `unknown` if not confirmed yet.
- fix: commit or patch summary that addresses the root cause, or `none yet`.
- validation: commands rerun after the fix and their results.
- next action: one concrete next debug or verification step.

Keep large generated logs and waves in `artifacts/`. Keep markdown records small enough to review.

## Per-Failure Template

Copy this template into each new record under `mydebug/new-er/records/`.

```markdown
# <test-name> Debug Record

## Summary

- Date:
- Owner:
- Task tag:
- Testcase:
- Result:
- Final trap line:

## Prior History

- Prior records for this task:
- Prior records read:
- Cross-task records read (optional):
- Cross-task relevance (required when used):
- Reused observations:
- Rejected old hypotheses:

## Command

```bash
<exact command>
```

## Configuration

- enable:
- observeOnly:
- trackEntries:
- earlyFreeWidth:
- conservative redirect policy:
- commit hash:
- difftest commit hash:

## Artifacts

- stdout:
- stderr:
- wave:
- counters:
- extra logs:

## Symptom

- First bad cycle:
- PC:
- ROB index:
- logical register:
- physical register:
- Difftest or assertion message:
- Other symptom:

## Wave Notes

- Signals inspected:
- Relevant cycle window:
- Producer state:
- Consumer/readDone state:
- ROB/ST state:
- Free-list/suppress state:
- Difftest state:

## Hypothesis

- hypothesis:
- supporting evidence:
- evidence against:

## Root Cause

- root cause:
- confidence:

## Fix

- patch or commit:
- files changed:
- reason the fix addresses the root cause:

## Validation

- command:
- result:
- hit-good-trap classification:
- remaining risk:

## Next Action

- next action:
```

## Counter And Signal Checklist

When applicable, inspect these areas before declaring a root cause:

- UCA: entry state, `trackId`, generation, `userCounter`, fallback, producer ready, redefiner seen, redefiner non-speculative, early-free issued.
- Rename and free list: source match, destination tracking, old physical destination, early-free lane, conventional free suppress.
- DataPath: raw readDone observation, fallback reason, replay-prone and uncertain-path classification.
- ROB/ST: raw readDone validation, duplicate/stale filtering, guard decrement, resolved/safe-to-cross state, redirect kill.
- Difftest: direct integer architectural shadow, direct commit data, skipped instruction event handling, final mismatch location.

## Completion Rule

A debug round is not complete until the record contains a stable task tag, the complete task-scoped prior-record list, command, configuration, stdout, stderr, wave or explicit no-wave reason, symptom, hypothesis, root cause or `unknown`, fix or `none yet`, validation, and next action. Any cross-task record must include an explicit relevance statement.
