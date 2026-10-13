# Directed ICache DUT tests

Each `test_` function describes a hardware scenario. Keep its steps easy to
follow in this order when practical:

1. Declare the intended coverage bins with `funcov_bins` when the scenario has
   explicit bin targets. Keep parameter-specific targets on `pytest.param`.
2. Prepare the instruction stream, DUT state, and observers.
3. Apply the scenario stimulus and advance the DUT with bounded waits.
4. Check the hardware behavior, checker errors, and intended coverage evidence.
5. Restore changed test inputs in a fixture teardown or `finally` block.

The generic DUT layer at `tests/py/support/dut_support.py` owns cycle-limit
parsing, ordered DUT signal lookup, and bounded polling/waits. It has no
dependency on ICache or coverage. This directory's `support.py` adds coverage
waits, soft-prefetch operations, predictor controls, and BPU stream operations.
`programs.py` owns instruction encoders, program layouts, and address constants.
`mainpipe_support.py` owns cacheable startup, shared MainPipe observations, and
dual-fetch transaction evidence. `refill_scenarios.py`, `branch_scenarios.py`,
and `protection_scenarios.py` own the reusable execution flows and their
behavior checks. Test modules keep their pytest parameters, coverage targets,
scenario-specific sampling, stimulus timing, timeout evidence, and assertions.

The shared BPU s3 stream is owned by `support.py` and `programs.py`, including
its instruction generator, FTQ observation aliases, and bounded training wait.
It loads through `run_initialization_steps` and starts through
`initialize_frontend`. Loading/reset and warmup also have separate entry points
for scenarios that configure agents between those operations.
Keep the load/reset/warmup/agent-configuration order and training predicates
when changing this stream.

Keep the dependency direction as `test scenario -> local ICache support ->
shared environment/generic DUT support`. Do not import test modules from
another testcase directory or call their `test_` functions as helpers. The
generic layer must not import an ICache module. A test
module may retain a small compatibility wrapper when its public helper name is
part of the existing test surface, but the implementation belongs in the
appropriate support layer. An integration test may compose stage-specific
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
