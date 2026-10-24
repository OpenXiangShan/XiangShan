# Toolchain

The MemBlock UT pins its external verification tools in
[`toolchain.lock.json`](../toolchain.lock.json). The bootstrap verifies the
recorded commits and rebuilds Picker from a clean native build directory when
either Picker or xcomm changes.

| Tool | Locked revision | Role in this UT |
| --- | --- | --- |
| [Picker](https://github.com/XS-MLVP/picker) | `v2.1.0`, `7499f797` | Exports the RTL as a compiled C++ DUT model |
| [xcomm](https://github.com/XS-MLVP/xcomm) | `v1.0.0`, `a5f5dae` | Supplies `XData`, `XClock`, native signal access, triggers, and coverage runtime |

## UT-Relevant Features

**Picker**

- Generates C++ DUT wrappers from RTL and supports parallel model builds with
  `--build-threads`.
- `mem_direct` provides generated read access without enabling VPI. The current
  harness uses it only for diagnostics; architectural PASS/FAIL remains based on
  top-level observations.

**xcomm**

- `XData` supports four-state values, slices, callbacks, native-memory bindings,
  and direct observation of generated signals.
- `XClock` provides cycle control; trigger engines can match expressions,
  sequences, and FSMs and can produce temporal coverage bins.
- The 1.x trigger/coverage API is ABI incompatible with older generated native
  clients, so Picker, xcomm, the generated model, and the harness must be
  rebuilt together.

## Debug Instrumentation and Rebuild Policy

- Do **not** add temporary RTL `printf` or debug logic to diagnose a UT
  failure. Keep the RTL source unchanged while validating UT behavior.
- UT-only changes rebuild only the C++ harness (`make picker-harness`); the
  generated Picker model/runtime is reused. `make verilog` and
  `export-picker` are reserved for real RTL changes (and must then be followed
  by `prepare-rtl` and the Picker model/harness rebuild).
- For internal-signal diagnosis, use Picker's generated `mem_direct` read
  access from the C++ harness (the generated `mem_direct` offsets and the
  `dut_adapter`/`Environment` raw access). Architectural PASS/FAIL still comes
  from top-level observations; `mem_direct` values are diagnostics only.
- If C++ internal access is insufficient, emit an FST waveform from the
  Picker/xcomm simulation and inspect it with `pywellen`. Keep waveform and
  analysis artifacts in a private output path; do not add them to the tracked
  tree.
- Debug work that needs a build must use a separate worktree/private output and
  must not replace the shared main build, model, runtime, `build/rtl`, or
  Picker output while another task depends on them.

## Updating

Update `toolchain.lock.json` only after rebuilding and qualifying the generated
model. For a temporary compatibility check, override the locked refs without
changing the lock:

```sh
make -C tests/memblock bootstrap-picker JOBS=8 \
  PICKER_REF=<tag-or-commit> XCOMM_REF=<tag-or-commit>
```

The canonical revisions are recorded in `build/memblock/tools/picker.json`. Run
at least `unit`, `prepare-rtl`, `check-rtl`, and `smoke` after changing the lock.
