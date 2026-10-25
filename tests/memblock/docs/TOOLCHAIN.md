# MemBlock Toolchain

## Build Inputs

The UT consumes split SystemVerilog from the XiangShan root generation flow.
Java17/Mill/Firtool support generation; Python3 and Make orchestrate the UT;
C++20, CMake, SWIG and Verilator build its native model/harness. The model is
two-state: X-sensitive storage behavior requires another verification method.

The current tool source is [toolchain.lock.json](../toolchain.lock.json).
bootstrap_picker.py resolves its Picker v2.1.0 and xcomm v1.0.0 refs to the
recorded commits, validates clean checkouts and rebuilds native dependencies
when resolved revisions change. Tool revisions are build inputs, not functional
acceptance proofs. Do not infer capabilities merely from a version number.

## Runtime Roles

Picker exports a typed C++ DUT wrapper and Verilated model. xcomm supplies
XData/XClock and native signal bindings. The harness drives/samples retained
top-level ports through dut_adapter. The export enables mem_direct read access
for diagnostics; top-level transactions and independent models decide PASS.
No VPI or temporary RTL printing is needed for routine localization.

prepare_rtl.py validates ordered filelists, module uniqueness and required
SRAM supplements. It does not prove all generated files correspond to the
current Scala/configuration. prepare_picker_harness.py synchronizes current
C++ copies. freeze_runtime.py copies executable/model/xspcomm/RTL metadata,
records resolved system dependencies and verifies runtime-local linkage.
Neither paths/sizes nor frozen permissions prove source-to-model identity.

## Debug Instrumentation and Rebuild Policy

C++-only changes rebuild the harness; real RTL/configuration changes regenerate
root RTL and rebuild the model. One owner controls shared outputs. Diagnostics
needing alternate sources use private worktrees/outputs. Do not replace
canonical RTL/model/runtime while a campaign reads it.

Use the reference-memory transcript first. mem_direct read access or an FST
waveform inspected with an appropriate waveform tool can localize internal
behavior. Internal names are diagnostic implementation details and may be
unavailable. Missing names must not change the architectural oracle. Keep
traces unique per process. See README reproduction and shared-state rules.

## Tool Changes

Change the lock intentionally, rebuild generation/model/harness as affected,
and run unit, check-rtl, smoke and the relevant functional scenarios. Record
resolved revisions, configuration and actual commands with qualification
artifacts. Existing CLI overrides PICKER_REF/XCOMM_REF select alternate tool
refs without making their results acceptance evidence automatically.
