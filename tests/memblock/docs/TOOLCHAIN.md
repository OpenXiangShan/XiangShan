# Toolchain

The MemBlock UT pins its external verification tools in
[`toolchain.lock.json`](../toolchain.lock.json). The bootstrap verifies the
recorded commits and rebuilds Picker from a clean native build directory when
either Picker or xcomm changes.

| Tool | Locked revision | Role in this UT |
| --- | --- | --- |
| [Picker](https://github.com/XS-MLVP/picker) | `v2.1.0`, `7499f797` | Exports the RTL as a compiled C++ DUT and mem-direct model |
| [xcomm](https://github.com/XS-MLVP/xcomm) | `v1.0.0`, `a5f5dae` | Supplies `XData`, `XClock`, native signal access, triggers, and coverage runtime |
| [Toffee](https://github.com/XS-MLVP/toffee) | `master`, `f57dd6ff` | Optional Python environment layer; not imported by the current C++ harness |

The Toffee lock is a pinned master snapshot 15 commits after the last PyPI
release, `v0.3.1`. It is recorded for compatibility and optional Python work,
but `bootstrap-picker` does not install it.

## UT-Relevant Features

**Picker**

- Generates C++ or Python DUT wrappers from RTL and supports parallel model
  builds with `--build-threads`.
- `mem_direct` provides generated read access without enabling VPI. The current
  harness uses it only for diagnostics; architectural PASS/FAIL remains based on
  top-level observations.
- A Python export can share the same prepared RTL file list if a Toffee-based
  environment is added later. It should not replace the independent C++
  reference models and scoreboards.

**xcomm**

- `XData` supports four-state values, slices, callbacks, native-memory bindings,
  and direct observation of generated signals.
- `XClock` provides cycle control; trigger engines can match expressions,
  sequences, and FSMs and can produce temporal coverage bins.
- The 1.x trigger/coverage API is ABI incompatible with older generated native
  clients, so Picker, xcomm, the generated model, and the harness must be
  rebuilt together.

**Toffee**

- `Bundle` and `Agent` bias toward reusable interface drivers and periodic
  monitor methods; monitor results are queued for asynchronous collection.
- `Model` can automatically pair driver hooks with outputs for basic response
  comparison, but a MemBlock Python layer would still need independent queue,
  translation, protocol, and ordering oracles.
- `Bundle.process_requests` supports table-driven cycle sequences, and
  `detect_unconnected_signals`/`detect_multiple_connections` expose common
  interface wiring mistakes early.
- `CovGroup` supports function/bin coverage, while the `toffee-test` pytest
  plugin manages DUT resources, fixtures, reports, and waveforms.

The pinned optional Python layer can be installed separately when needed:

```sh
python3 -m pip install \
  "pytoffee @ git+https://github.com/XS-MLVP/toffee.git@f57dd6ffc295b9b8bad3e3b3ce040b8a849f5c91"
```

## Updating

Update `toolchain.lock.json` only after rebuilding and qualifying the generated
model. For a temporary compatibility check, override the locked refs without
changing the lock:

```sh
make -C tests/memblock bootstrap-picker JOBS=8 \
  PICKER_REF=<tag-or-commit> XCOMM_REF=<tag-or-commit>
```

The actual revisions and executable path are recorded in
`build/memblock/tools/picker.json`. Run at least `unit`, `prepare-rtl`,
`check-rtl`, and `smoke` after changing the lock.
