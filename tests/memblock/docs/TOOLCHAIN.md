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
