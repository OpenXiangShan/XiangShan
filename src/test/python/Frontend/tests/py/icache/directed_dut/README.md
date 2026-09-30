# Directed ICache DUT tests

Each `test_` function describes a hardware scenario. Keep its steps easy to
follow in this order when practical:

1. Declare the intended coverage bins with `funcov_bins` when the scenario has
   explicit bin targets. Keep parameter-specific targets on `pytest.param`.
2. Prepare the instruction stream, DUT state, and observers.
3. Apply the scenario stimulus and advance the DUT with bounded waits.
4. Check the hardware behavior, checker errors, and intended coverage evidence.
5. Restore changed test inputs in a fixture teardown or `finally` block.

`support.py` contains operations whose behavior is shared across scenarios:
cycle-limit parsing, unified DUT signal lookup (`read_dut_signal`), bounded
polling, soft-prefetch cleanup, and predictor controls. Keep scenario-specific
signal paths, sampling windows, timeout diagnostics, and assertions in their
test modules. A single scenario may target bins from more than one ICache
block.

When editing a test, preserve its pytest node ID, parameter cases, markers,
stimulus timing, and failure conditions unless the change explicitly intends
to alter test behavior. `funcov_closure_pending` tests are excluded by the
normal regression runner and exercised by its separate reachability mode.
