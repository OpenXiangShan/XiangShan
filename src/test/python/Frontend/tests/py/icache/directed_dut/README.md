# Directed ICache DUT tests

Each `test_` function describes a hardware scenario. Keep its steps easy to
follow in this order when practical:

1. Declare the intended coverage bins with `funcov_bins` when the scenario has
   explicit bin targets. Keep parameter-specific targets on `pytest.param`.
2. Prepare the instruction stream, DUT state, and observers.
3. Apply the scenario stimulus and advance the DUT with bounded waits.
4. Check the hardware behavior, checker errors, and intended coverage evidence.
5. Restore changed test inputs in a fixture teardown or `finally` block.

The shared code is split into two public support files. The generic DUT layer
at `tests/py/support/dut_support.py` owns cycle-limit parsing, ordered DUT
signal lookup, and bounded polling/waits. It has no dependency on ICache or
coverage. The ICache layer in this directory's `support.py` adds coverage
waits, soft-prefetch operations, predictor controls, and ICache-specific DUT
adapters. Test modules keep scenario signal maps, sampling windows, stimulus
timing, timeout evidence, and assertions.

Keep the dependency direction as `test scenario -> ICache support -> generic
DUT support`. Do not make the generic layer import an ICache module. A test
module may retain a small compatibility wrapper when its public helper name is
part of the existing test surface, but the implementation belongs in the
appropriate support layer. An integration test may compose stage-specific
snapshot and match predicates from its sibling scenarios when those predicates
are the evidence under test; those scenario modules are not general support.
A single scenario may target bins from more than one ICache block.

When editing a test, preserve its pytest node ID, parameter cases, markers,
stimulus timing, and failure conditions unless the change explicitly intends
to alter test behavior. `funcov_closure_pending` tests are excluded by the
normal regression runner and exercised by its separate reachability mode.
