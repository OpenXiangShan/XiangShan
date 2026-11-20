"""firtool options for Utility's clock gate."""

# Utility's ClockGate is CIRCT's clock gate (patches/0010): firtool folds
# the gates whose enable is constant and emits the rest as instances of an
# external module, named and ported here as Utility's black box was, so
# xs_icg.ys and the simulator see the module they always did. Its body,
# Utility's behavioural model verbatim, is ClockGate.sv, concatenated in
# below.
CLOCK_GATE_OPTS = [
    "--ckg-name=ClockGate",
    "--ckg-input=CK",
    "--ckg-output=Q",
    "--ckg-enable=E",
    "--ckg-test-enable=TE",
]
