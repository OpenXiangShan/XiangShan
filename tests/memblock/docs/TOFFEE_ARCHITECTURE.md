# Toffee Verification Architecture

## Upstream Guidance

The structure follows the patterns used by XS-MLVP's maintained verification
projects rather than treating Toffee as a replacement for `XData`:

- Toffee recommends one `Bundle` and one `Agent` per logical interface, then an
  `Env` that composes agents and reference models. Tests should call semantic
  operations instead of assigning pins directly.
- [UnityChipForXiangShan](https://github.com/XS-MLVP/UnityChipForXiangShan)
  separates `bundle/`, `agent/`, `env/`, `test/`, and `util/` below each RTL
  block. Its tests use `toffee-test` fixtures, tags, functional coverage, line
  coverage, waveforms, and HTML reports.
- [env-xs-ov-00-bpu](https://github.com/XS-MLVP/env-xs-ov-00-bpu) wraps the DUT
  in a software-facing model that owns pipeline timing while pytest cases call
  operations such as read, update, or send.
- [toffee-test](https://github.com/XS-MLVP/toffee-test) owns DUT construction,
  clock startup, waveform/coverage filenames, and report cleanup. A test must not
  construct or close the DUT independently.
- The XS-MLVP [XReactor guides](https://github.com/XS-MLVP/xreactor/tree/main/docs/guides)
  provide a stricter reference for lifecycle ownership, independent reference
  models, transaction scoreboards, bounded completion, and per-run artifacts.
  XReactor is not a dependency of this environment.

## Layering

```text
pytest / toffee-test fixture
            |
       MemBlockEnv
       /    |     \
  Agent   Model  Scoreboard
    |
 Bundle (logical protocol fields)
    |
 Picker adapter (generated package, clock, pin map)
    |
 Picker Python DUT
```

Dependencies point downward only:

| Layer | May know | Must not know |
| --- | --- | --- |
| Test | Env API, test data, transaction identity | Pin names, generated classes, clock loops |
| Sequence | Agent operations and intent | Pin names, response queues |
| Agent | One logical Bundle and timing | Reference values or scoreboard internals |
| Model | Specification state and transactions | DUT, pins, clocks, Agents |
| Scoreboard | Immutable expected/observed transactions | DUT, clocks, generated names |
| Adapter/fixture | Generated DUT, pin map, clock ownership | Test intent or expected behavior |

The adapter is the only hardware-specific code. It initializes Picker's clock,
applies manifest input defaults, and converts raw `XData` into Toffee-compatible
pins. Adding or renaming an RTL port changes the adapter, not the tests.

## UVM Compatibility

The framework preserves UVM component responsibilities without reimplementing
the UVM library:

| UVM concept | This framework | Toffee |
| --- | --- | --- |
| Test | pytest test selected by marker/tag | `toffee-test` testcase |
| Sequence | `Sequence` containing transaction intent | async `Agent` calls |
| Sequencer | `Sequencer` coordinating driver, monitor, model, and scoreboard | asyncio |
| Driver | Protocol-specific `@driver_method` | `Agent` |
| Monitor | Passive pre-edge/post-edge sampling after the driver phase | none |
| Reference model | Pure Python state machine | none |
| Scoreboard | Independent identity-based comparison | pytest assertions/reports |
| Env | Composition root | `Env` |
| Config DB | Typed dotted-path configuration | none |
| Phases | fixture setup, test body, bounded finish, teardown | pytest + Toffee lifecycle |

Toffee is used for orchestration, not as the correctness oracle. Internal
`mem_direct` signals remain diagnostic-only.

## Lifecycle

1. The fixture asks `toffee_request` to create the raw Picker DUT.
2. The adapter calls `InitClock`, applies logical pin aliases, and initializes
   all inputs from the checked port manifest.
3. The fixture builds Bundles, Agents, the pure model, and scoreboards.
4. Exactly one Toffee clock loop is started.
5. Reset and all active stimulus use `await bundle.step()`. Tests never call
   `DUT.Step()` directly.
6. The sequencer assigns cycle identity before driving, asks the driver to
   prepare inputs, samples through a separate monitor, advances one cycle, and
   hands the immutable observation to the scoreboard.
7. The frozen runtime contains both the generated DUT package and a snapshot of
   `memblock_toffee`. Its files are read-only, and recorded sizes and hashes are
   validated once before importing the generated module.
8. The fixture stops the clock task, then `toffee-test` calls `Finish()` and
   writes coverage/waveform/report artifacts.

## Source Layout

```text
tests/memblock/toffee/
  src/memblock_toffee/
    adapters/       generated DUT and pin-name boundary
    bundle/         logical interfaces
    agent/          protocol drivers and monitors
    datadef/        immutable commands and observations
    model/          independent, pin-free reference state
    monitor/        passive pre/post-edge sampling
    scoreboard/     identity-based checking
    sequence/       reusable scenario intent
    env/            composition root
    fixtures.py     DUT/clock ownership
  tests/
    unit/           framework and model tests without RTL
    adapters/       generated-pin boundary tests only
    smoke/          reset and clock bring-up
    control/        migrated top-level control contracts
```

New subsystems follow the same boundary: add a Bundle, Agent, model or
scoreboard, fixture/environment wiring, and a focused pytest module. Do not add
new hardware fields to tests or reuse the legacy C++ `Environment` as a Python
API.

## Migration Status

The executable Toffee vertical slice currently covers:

- generated Python DUT bring-up and input initialization;
- Toffee clock ownership and reset;
- L2 flush enable pass-through plus pre-edge/post-edge completion timing;
- cycle-derived transaction identity and harness transaction-loss/duplication
  detection; this continuous interface has no DUT completion identifier;
- `toffee-test` pytest fixtures and report hooks.

[`suite_manifest.json`](../toffee/suite_manifest.json) assigns every existing
C++ leaf scenario to exactly one target Toffee suite. The manifest check
currently reports `migrated=2/90`; the remaining 88 scenarios are explicitly
`planned`, not implicitly covered. `toffee-unit` fails if a legacy scenario is
unassigned, assigned twice, or an implemented suite has no matching pytest
marker.

Each migrated scenario is tied to a concrete pytest node. The checker parses
that node, requires the Toffee testcase and suite marker decorators, and rejects
planned/implemented status mismatches. A directory or marker string alone
cannot claim a scenario as migrated.

The existing C++ scenarios remain the canonical regression baseline while
subsystems migrate in this order:

1. control/reset/front-end bridges;
2. scalar issue, DCache request/response, and scalar writeback;
3. PTW, Uncache, atomics, and CMO;
4. translation/vector flows;
5. canonical constrained-random profiles and closure campaigns.

Each migrated group must preserve its external oracle, replay seed, coverage
gates, and frozen-runtime provenance. C++-only acceptance rules are not reduced
because a Toffee test exists.

## Commands

```sh
make -C tests/memblock bootstrap-toffee JOBS=8
make -C tests/memblock toffee-unit
make -C tests/memblock toffee-model JOBS=8
make -C tests/memblock toffee-runtime
make -C tests/memblock toffee-smoke
make -C tests/memblock toffee-control
make -C tests/memblock toffee-regression
```

Generated Toffee artifacts stay below `build/memblock/toffee`; they do not
replace the canonical C++ export or runtime.

Waveform and line-coverage compilation are disabled by default because they
make the large MemBlock translation units unsuitable for routine development.
A report or closure run enables both explicitly with
`TOFFEE_ENABLE_WAVE=1 TOFFEE_ENABLE_COVERAGE=1`. Functional coverage and HTML
reports do not require either compiled feature.
