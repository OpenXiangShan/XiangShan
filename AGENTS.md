# MemBlock Verification Agent Instructions

## Scope and intent

These instructions govern MemBlock verification work in this repository, primarily under `tests/memblock`. The purpose of this environment is **defensive functional verification** of XiangShan's generated Kunminghu-v2 `MemBlock`, especially LSU, DCache, translation, memory-side protocols, and related top-level contracts.

Direct user instructions always take precedence. For detailed project state, use these documents as the source of truth:

* `tests/memblock/README.md`: build, setup, and execution workflow.
* `tests/memblock/docs/IO_COVERAGE.md`: authoritative top-level IO, constrained-random mapping, implementation status, and remaining coverage gaps.
* `tests/memblock/docs/VERIFICATION_PLAN.md`: verification scope, implemented points, and explicit gaps.
* `tests/memblock/docs/ORACLES.md`: correctness contracts and independent reference-model rules.
* `tests/memblock/docs/CONSTRAINED_RANDOM.md`: canonical constrained-random interface and constraint profiles.
* `tests/memblock/docs/USER_PROMPTS.md`: historical provenance. It explains how these principles were derived, but should not replace the normalized rules in this file.

## 1. Verification priorities

Work in this order of priority:

1. **Complete and correct the verification environment before spending excessive time on regression.**
2. **Use stable, implementation-independent oracles.**
3. **Exercise broad legal input space with mixed constrained-random traffic.**
4. **Demonstrate that the UT can reproduce representative historical RTL bugs or equivalent mutations.**
5. **Run long, diverse regressions and search for new RTL bugs.**
6. **Continuously review the UT itself when progress stalls.**

A long passing regression is not evidence for functionality that is absent from the verification plan or lacks an oracle.

## 2. Use UVM-style responsibilities without depending on RTL internals

Keep the environment structured around UVM responsibilities: sequence/generation, driver, monitor, reference model, scoreboard, coverage, and assertions.

Treat `MemBlock` as a black box wherever practical:

* Derive legal stimulus from the top-level interface contract and architectural specification.
* Audit all relevant top-level IOs and transaction fields rather than testing only convenient paths.
* Explicitly model input assumptions and legal combinations.
* Use internal RTL signals only for debug, localization, or mutation analysis; do not make them the primary correctness oracle.
* Do not encode pipeline timing or internal state-machine details unless the external interface explicitly requires that timing.

## 3. Constrained random is the primary stress methodology

`random-mixed` should remain the canonical mixed workload generator. A verification direction should normally be expressed as a **constraint profile**, not as a separate hard-coded stress implementation.

Randomize broadly across legal dimensions, including where applicable:

* operation/instruction class;
* scalar/vector shape and addressing mode;
* address, alignment, page/cache-line boundaries, data, masks, and metadata;
* privilege, virtualization, translation mode, permissions, PMP/PMA/PBMT state;
* cache/TLB hit and miss behavior;
* manager response latency, refill spacing, ready/valid backpressure, and concurrency;
* redirects, queue pressure, forwarding, probes/coherence activity, errors, and recovery.

Maintain multiple useful distributions:

* **realistic / workload-like** traffic, where ordinary load/store behavior dominates and rare error injection remains rare;
* **coverage-oriented** traffic, which balances dimensions to close bins;
* **corner/error-heavy** traffic, which deliberately stresses low-probability boundaries and protocol/error paths.

When workload counters or representative performance data are available, use them to calibrate realistic profiles, but do not overfit the verification environment to one benchmark.

Do not overfit the generator to a known bug with a fixed address or offset. A known bug may have a focused reproducer, but the general constrained-random space should have a meaningful nonzero probability of reaching the same class of behavior.

If a supposedly useful long-running random campaign cannot reproduce a known reachable bug after substantial runtime, **stop extending runtime and review the generator, constraints, coverage, and oracle**.

## 4. Coverage closure is continuous work

`tests/memblock/docs/VERIFICATION_PLAN.md` must remain a comprehensive inventory of all testable MemBlock behavior and all known gaps.
`tests/memblock/docs/IO_COVERAGE.md` must remain the matching top-level IO inventory and semantic-status matrix.

While working:

* review the generated MemBlock top-level IO and relevant specification/code regularly;
* update `tests/memblock/docs/IO_COVERAGE.md` whenever an IO group, driver, legal-value class, or implementation status changes, and run `make -C tests/memblock check-docs`;
* add missing legal modes, instruction classes, protocol paths, boundary cases, and interactions to the plan;
* keep unsupported or integration-only behavior explicit rather than silently claiming coverage;
* when a gap is identified, prefer implementing the missing verification capability before launching another very long regression.

Focused directed tests are valuable for bring-up, exact contract checking, debugging, and deterministic reproduction. They are **not** a substitute for mixed constrained-random stress.

When practical, inspect historical MemBlock/LSU/DCache bug fixes and use representative revert/mutation experiments to check that the UT would detect real classes of RTL regressions. Historical bugs are a validation tool for the testbench, not the final target of the generator.

If regression runs for a long time without finding bugs, use that time to re-audit top-level IO, coverage holes, constraint distributions, and oracle weaknesses instead of assuming the design is bug-free.

## 5. Oracle rules

Correctness must be decided from transaction inputs, an independent architectural/reference model, and externally observable protocol behavior.

Follow these rules:

1. Compute expected architectural state independently of DUT internal state.
2. Compare externally visible effects while allowing all latency and ordering that are legal for the interface.
3. Match responses by architectural/transaction identity, not arrival order alone.
4. Require each accepted operation to terminate exactly once or be explicitly and legally canceled.
5. Reject forbidden side effects, duplicate completions, lost work, protocol instability, and incorrect architectural state.
6. Do not turn a historical failing value, a specific seed result, or an RTL implementation detail into the expected answer.

Use `tests/memblock/docs/ORACLES.md` for the detailed contract catalog.

If behavior is ambiguous, inspect the RISC-V specification, XiangShan design intent, and externally visible propagation before calling it a CPU bug. A suspicious internal event is not sufficient by itself if it cannot affect architectural or defined interface behavior.

## 6. Regression strategy

During development, use the shortest run that gives useful feedback, then increase stress as the harness matures.

For closure-quality regression:

* run mixed constrained-random traffic across multiple seeds and multiple constraint profiles;
* make individual seeds long enough to create meaningful queue/cache/TLB overlap rather than relying on many tiny tests;
* aim for very large transaction counts when practical (historically, order-of-million requests per seed was requested as a useful scale);
* after major planned functionality is implemented, run a long campaign with **at least roughly six hours of aggregate stress** when resources permit;
* use available parallelism when it improves throughput without changing semantics;
* record seeds, constraint profiles, transaction counts, RTL/tool revisions, and failures so every result is reproducible.

Runtime is a means to explore state space, not a success metric by itself.

## 7. RTL bug triage and documentation

Treat every failure first as a candidate.

Before declaring an RTL bug:

1. replay the failing seed/scenario deterministically;
2. minimize enough to understand the failure without replacing the general random stimulus;
3. exclude driver, memory-agent, reference-model, scoreboard, stale-build, and provenance errors;
4. identify the violated architectural or external-interface contract;
5. root-cause the RTL behavior and establish its externally visible impact.

Only confirmed CPU RTL bugs receive standalone documents named:

`tests/memblock/docs/CPU_BUG_<SHORT_NAME>.md`

A CPU bug document should contain the symptom, reproducer, oracle/contract, root cause, architectural or protocol impact, fix, and post-fix verification.

After confirming a CPU RTL bug, create the CPU bug document and also send a
concise notification to the DingTalk **debug** endpoint. Use the helper in the
fresh `OpenXiangShan/env-scripts` clone at
`/home/xuyinan/paseo-workspace/env-scripts`, and read only
`xiangshan_monitor.dingtalk.debug` from the existing local config at
`/home/xuyinan/tools/xsrobot/env-scripts/dingtalk_robot/config.json`.
Never read, call, or fall back to `xiangshan_monitor.dingtalk.release`; do not
use scheduled delivery, `--workday-delivery`, or
`--dingtalk-layer release` from this workflow.

```sh
PYTHONPATH=/home/xuyinan/paseo-workspace/env-scripts python3 - <<'PY'
import json
from pathlib import Path
from dingtalk_robot import robot

config = json.loads(Path(
    '/home/xuyinan/tools/xsrobot/env-scripts/dingtalk_robot/config.json'
).read_text())
debug = config['xiangshan_monitor']['dingtalk']['debug']
robot.send_text(
    debug['webhook'],
    debug['secret'],
    '[MemBlock CPU bug] <short symptom>; see tests/memblock/docs/CPU_BUG_<SHORT_NAME>.md',
)
PY
```

The debug endpoint was verified reachable on 2026-10-20 with
`errcode=0, errmsg=ok`. Do not include a webhook token or signing secret in logs,
documents, commit messages, or agent output.

Do **not** create standalone `*_BUG_*.md` reports for UT harness bugs, oracle mistakes, regression-controller issues, documentation mistakes, or normal testbench iteration. Record those in normal commits and consolidated documentation.

Keep CPU RTL fixes in logically separate commits from UT-framework changes whenever practical. If later design review shows that a candidate is legal behavior, re-evaluate the oracle and update or withdraw the bug claim rather than preserving a false report.

## 8. Engineering and reproducibility

Keep the environment reproducible from a clean checkout.

* Follow the build/provenance rules in `tests/memblock/README.md`.
* Do not hide interface mismatches by blindly regenerating tracked port manifests.
* Do not reuse stale generated RTL after configuration, generation-option, or RTL-commit changes.
* Keep tool and RTL revisions recorded for traceability. Do not reintroduce fixed Picker/xcomm commit pins or cryptographic file-hash acceptance gates unless the user explicitly requests them.
* Use current upstream defaults for Kunminghu-v2, Picker, and xcomm; record the resolved revisions after bootstrap rather than constraining them in source.
* New Picker/XS-MLVP debug capabilities may be used to diagnose failures, but do not introduce unnecessary runtime dependencies merely because a debug feature exists.
* Keep documentation sufficient for another developer or agent to reproduce the environment.
* Keep `tests/memblock/docs/IO_COVERAGE.md`, `tests/memblock/docs/VERIFICATION_PLAN.md`, and `tests/memblock/docs/CONSTRAINED_RANDOM.md` consistent with the implementation; `make -C tests/memblock check-rtl` verifies the generated IO inventory and constraint schema.
* Commit and push meaningful progress in logical, reviewable units when repository access permits.

Avoid documentation noise. Update the existing verification plan, oracle catalog, constrained-random guide, README, results, or a confirmed CPU bug report instead of creating a new Markdown file for every intermediate observation.

Maintain `tests/memblock/docs/USER_PROMPTS.md` as historical provenance when complete prompt history is available; do not invent missing prompts or timestamps.

## 9. Independent review and self-correction

After substantial changes to the verification plan, oracle model, or constrained-random strategy, perform an independent-context review when the available tooling supports it before starting the next closure-scale regression.

The review should ask:

* What top-level IO or legal transaction fields remain unexplored?
* Which verification-plan rows have weak or missing stimulus?
* Which oracle depends too much on implementation detail?
* Are constraint distributions actually reaching intended bins?
* Are realistic and corner-case profiles both represented?
* Can representative historical bugs still be detected?
* Is regression time being spent on useful state-space exploration?

Do not continue a low-value campaign merely because it is already running. Fix the verification strategy when evidence shows that the campaign is not exploring useful behavior.

## 10. Definition of useful progress

Useful progress is one or more of:

* a previously missing verification-plan row becomes executable;
* an oracle becomes more independent or more precise;
* constrained-random stimulus reaches a materially broader legal state space;
* a historical mutation becomes detectable;
* a false positive/negative is eliminated;
* a new confirmed RTL bug is reproduced and root-caused;
* regression quality or reproducibility improves.

The final objective is not “all tests pass.” The objective is a verification environment whose **coverage claims are explicit, whose oracles are trustworthy, whose random stimulus explores meaningful state space, and whose failures can distinguish RTL bugs from testbench bugs**.
