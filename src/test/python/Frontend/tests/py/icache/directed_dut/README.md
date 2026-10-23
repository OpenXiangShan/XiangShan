# Directed ICache DUT tests

Each `test_` function describes a hardware scenario. Keep its steps easy to
follow in this order when practical:

1. Declare the intended coverage bins with `funcov_bins` when the scenario has
   explicit bin targets. Keep parameter-specific targets on `pytest.param`.
2. Prepare the instruction stream, DUT state, and observers.
3. Apply the scenario stimulus and advance the DUT with bounded waits.
4. Check the hardware behavior, checker errors, and intended coverage evidence.
5. Restore changed test inputs in a fixture teardown or `finally` block.

This directory has two helper modules:

| Module | Responsibility |
| --- | --- |
| `support.py` | Startup plans, signal lookup, waits, MainPipe observations, transaction analysis, and runtime controls |
| `programs.py` | Instruction encoders, program layouts, and address constants |

`support.py` is ordered as signal lookup/coverage waits, MainPipe sampling and
transaction analysis, runtime controls/loading, then startup steps and plan
factories. Import program builders and address constants directly from
`programs.py`; avoid forwarding aliases and alternate startup entry points.

Complete execution flows stay in their owning DUT test modules, including
flows reused by several cases within one module. Random-position branch
execution belongs to the MainPipe hitpath tests; trained dual-refill execution
and the large-loop flow belong to the PrefetchPipe tests. Asymmetric corrupt
refill belongs to the WayLookup tests, and protection flows belong to the
MainPipe partial tests. Keep one local helper for each reused flow. Test
modules retain pytest parameters, coverage targets, scenario-specific sampling,
stimulus timing, timeout evidence, and assertions.

The generic DUT layer at `tests/py/support/dut_support.py` owns cycle-limit
parsing, cached DUT signal lookup, and bounded polling/waits. It has no
dependency on ICache or coverage. MainPipe's required lookup in `support.py`
deliberately re-resolves aliases and preserves its missing-signal diagnostics.
Recorder-based snapshots remain separate from direct live-pin reads. Boolean
`run_until` performs a final predicate check, including for a zero-cycle window;
exception-based `wait_until` retains the generic timeout behavior. These APIs
have distinct contracts and must not be interchanged during cleanup.

All explicit startup uses `env.runtime.frontend_init.initialize_frontend` with
a plan from `support.py`. Plan factories only construct configuration: they
do not receive an env, access the DUT, register observers, or advance time.
The common executor owns `before_reset`, `before_release`, and `after_reset`,
and records each step's phase and start/end cycle in `env.init_result.steps`.
Initialization results include post-reset warmup cycles and warmup failures.

```python
import pytest

from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from tests.py.icache.directed_dut.support import make_bpu_s3_init_config

@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
def test_example(env):
    initialize_frontend(env, make_bpu_s3_init_config(env.config))
    # Register observers at the scenario's required sampling boundary.
    # Apply stimulus, wait for behavior, and assert the coverage evidence.
```

`make_cacheable_init_config`, `make_nop_init_config`, `make_dual_refill_init_config`,
`make_missunit_init_config`, and `make_sv39_init_config` select the other shared
startup flows. Use `make_program_init_config` for scenario-owned images and
agent parameters. Its default `after_reset` explicitly clears the monitor and
sets the expected PC; pass `after_reset=()` to preserve existing evidence, or
provide an ordered tuple for a different post-reset sequence. The generic
`initialize_frontend` itself still preserves errors and accumulated coverage.

BPU program layout belongs to `programs.py`; its FTQ observation aliases and
bounded training predicate belong to `WarmBpuTraining` in `support.py`.
`make_bpu_training_init_config` supports load/reset alone or `warmup=True`.
`make_bpu_s3_init_config` warms before reconfiguring ICache and enabling the
predictors. Keep this order and the existing training predicates.

Runtime program loads, translation changes, agent reconfiguration and retraining
continue to use `run_initialization_steps`; they must not add a hardware reset.
Call the common executor explicitly and keep observer registration separate
from startup configuration. Startup wrappers and unused loader aliases have
been removed; unit tests exercise the plans, public initialization steps and
runtime helpers directly.

Build ordinary NOP payloads with `programs.build_nop_program`. The shared runtime
`load_nop_program` retains its one-cycle default; callers replacing a zero-cycle
loader must supply `step_cycles=0` and their original word count and base address.

Preparation remains at the scenario's existing boundary. Initial control steps
use phase `initial_controls`; `WaitCycles(300)` records the existing SRAM sweep
wait as `sram_init_wait`. These records belong to `env.init_preparation_history`.
They preserve the completed startup's `env.init_result` and completion event.

Translation preparation uses `PrepareTranslationScenario(arm=False)` with phase
`translation_prepare`, followed at the original activation boundary by explicit
`ClearMonitor`, `SetExpectedPc`, and `ArmTranslationScenario` steps with phase
`translation_activate`. Preserve each call's page selection and the order: arm
restores monitor translation context after clear. The arm step uses the current
testcase's state and retains the environment's epoch validation. Later rearming
and redirects remain part of the owning scenario's stimulus.

Existing fixture defaults and per-test `defer` markers are preserved. A test
already using fixture startup must retain that boundary during this migration;
removing an extra startup is a separate behavior change requiring DUT validation.
Do not make this directory deferred by default while existing tests rely on
fixture startup followed by runtime-only loading. Preserve observer boundaries
and loader-specific `step_cycles`, including the one-cycle NOP loader.

Keep the dependency direction as `test scenario -> local ICache support ->
shared environment/generic DUT support`. Do not import test modules from
another testcase directory or call their `test_` functions as helpers. The
generic layer must not import an ICache module. Unit tests can import the
owning DUT test module to probe local flow contracts and must patch that actual
owner. An integration test may compose stage-specific
snapshot and match predicates from its sibling scenarios when those predicates
are the evidence under test; those scenario modules are not general support.
A single scenario may target bins from more than one ICache block.

Random scenario identifiers retain their original strings because they feed
seed derivation and replay metadata; those strings are not module dependencies.
The 512-byte BPU training loop and 288-byte dual-refill loop are different
programs. Preserve their layouts, reset addresses, load-cycle counts, and
existing reset boundaries when reusing them.

When editing a test, preserve its pytest node ID, parameter cases, markers,
stimulus timing, and failure conditions unless the change explicitly intends
to alter test behavior. `funcov_closure_pending` tests are excluded by the
normal regression runner and exercised by its separate reachability mode.

`test_icache_data_ecc_ibuffer_dut.py` implements the four Data-bank parity
testpoints at CSV rows 832–835. Its 47 parameters cover:

| Request | Fault placement | Parameters |
| --- | --- | --- |
| Single line | Each bank 0–7, multiple banks, all eight banks | 11 |
| Cross line | Line 0: each selected bank 4–7, multiple banks, all four | 6 |
| Cross line | Line 1: each selected bank 0–3, including independent bank 1/2 faults, multiple/all banks | 6 |
| Cross line | Both lines: all 16 individual bank pairs and the remaining single/multiple/all combinations | 24 |

Each parameter warms real SRAM through ordinary fetch/refill, parks in a
separate loop, checks a clean SRAM refetch, then flips only parity bits and
checks the faulted refetch. The request address/endPosition determine expected
banks independently of S2 ECC selection. Live post-drive observations join
physical reads, SRAM data/code, delayed corrupt and the IBuffer handshake by
full FTQ pointer and PC. Hwe must belong to a valid, enabled target instruction;
the clean baseline uses IBuffer's valid/enqEnable exception gates.

Run with the standard runner and `TB_ENABLE_DUT_TESTS=1`; the module accepts
`-n 4` for parallel pytest workers. Per-case `checks/*.data_ecc.json` files
retain build/probe provenance and clean/faulted stage evidence alongside the
normal logs and waves. These leaves have no canonical coverage bins yet, so
checker success does not backannotate CSV coverage or status as HIT.
