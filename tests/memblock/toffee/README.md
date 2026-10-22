# MemBlock Toffee Verification

This package is the Toffee-based verification layer for MemBlock. Hardware
details are confined to `memblock_toffee.adapters`; bundles, agents, models,
scoreboards, sequences, and tests use protocol-level names only.

The architecture and migration status are documented in
[`../../docs/TOFFEE_ARCHITECTURE.md`](../../docs/TOFFEE_ARCHITECTURE.md).

Local framework tests:

```sh
make -C tests/memblock toffee-unit
```

This target installs only the locked Python/Toffee environment; it does not
require a Picker export or generated DUT. Generated-DUT tests consume the
immutable runtime below `build/memblock/toffee/runtime`.

Generated-DUT smoke test:

```sh
make -C tests/memblock toffee-model
make -C tests/memblock toffee-smoke
```
