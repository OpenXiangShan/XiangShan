# Frontend Python Verification Package Migration Design

## Goal

Migrate the refactored Frontend Python verification environment from
`/tmp/frontend_bt_head_20260403_clean/testbench/` into this repository as a new
canonical package rooted at `src/test/python/Frontend/`, aligned with the
existing `src/test/python/MemBlock/` workspace shape.

This migration intentionally carries only the Frontend verification code and
tests. It does not migrate the original `testbench/docs/`, `testbench/web/`,
`testbench/tools/`, or helper shell scripts.

## User-Approved Constraints

- Target package name is `Frontend`, not `FrontendBT`.
- The package must live at `src/test/python/Frontend/`, parallel to
  `src/test/python/MemBlock/`.
- The package shape should follow the MemBlock workspace organization.
- Only the main verification code and tests are migrated:
  - source: `testbench/env/*`
  - tests: `testbench/tests/*`
- Internal module names and implementation structure should change as little as
  practical.
- Existing old Frontend entry points may be replaced when they conflict with the
  new package layout.
- Documentation is out of scope for this migration.

## Current Source of Truth

The migration source is the clean `frontend_bt` worktree at:

- `/tmp/frontend_bt_head_20260403_clean`

The relevant implemented surface in that worktree is:

- `testbench/env/`
- `testbench/tests/`

The source tree already reflects the MemBlock-style refactor completed in the
frontend workstream:

- `env/agents/`
- `env/monitors/`
- `env/model/`
- `env/sequences/`
- `env/api.py`
- `env/frontend_env.py`
- `env/request_apis.py`
- `env/env_config.py`
- `env/transactions.py`

## Target Layout

The migration target is a new package rooted at `src/test/python/Frontend/`.
The package should present a MemBlock-like top-level shape while preserving the
refactored Frontend internals.

```text
src/test/python/Frontend/
|-- __init__.py
|-- conftest.py
|-- Frontend_api.py
|-- Frontend_env.py
|-- env/
|   |-- __init__.py
|   |-- api.py
|   |-- frontend_env.py
|   |-- fixtures.py
|   |-- env_config.py
|   |-- request_apis.py
|   |-- transactions.py
|   |-- agents/
|   |-- monitors/
|   |-- model/
|   `-- sequences/
`-- tests/
    |-- conftest.py
    `-- test_*.py
```

## Architectural Decision

### Recommended approach

Create `src/test/python/Frontend/` as a new package root and copy the refactored
Frontend code into it with only path and import adjustments.

This keeps the migration aligned with MemBlock at the package boundary, while
preserving the already-clean internal layering from the `frontend_bt` refactor.

### Rejected alternatives

1. Copy the original `testbench/` tree wholesale under
   `src/test/python/Frontend/testbench/`.
   - Rejected because it would preserve the old top-level shape instead of
     matching the XiangShan Python verification layout.
2. Rewrite the Frontend package to fully imitate MemBlock's flatter root-module
   style.
   - Rejected because it would force broad renaming and increase behavior risk,
     violating the requirement to keep internal modules mostly unchanged.

## Package Boundary and Compatibility Strategy

### Canonical implementation

The canonical implementation should live under:

- `src/test/python/Frontend/env/`
- `src/test/python/Frontend/tests/`

### Root-level entry points

To match the MemBlock user experience without flattening the full codebase,
introduce thin root-level modules:

- `src/test/python/Frontend/Frontend_api.py`
- `src/test/python/Frontend/Frontend_env.py`
- `src/test/python/Frontend/conftest.py`

These files should be lightweight adapters that expose the new package through a
MemBlock-like top-level interface. They should not become a second
implementation layer.

### Old entry points in the repository

If the repository already contains older Frontend Python entry points under the
legacy `tests/` area and they conflict with the new canonical package, they may
be replaced by thin bridges or direct imports into `src/test/python/Frontend/`.
The new package becomes the source of truth.

## Shim Retention Policy

The source worktree still contains several migration-era compatibility modules,
including `backend_model.py`, `monitor.py`, `memory_model.py`, `trace.py`, and
`agents_legacy.py`.

During migration:

- retain a shim only when it still serves imports that remain alive after the
  move into `src/test/python/Frontend/`
- drop a shim when it only existed to support the old `testbench/` path layout
  and no longer reduces risk in XiangShan
- prefer one canonical import path for all newly moved tests

This keeps the package from carrying unnecessary compatibility debt forward.

## Test Migration Strategy

The migrated tests should live at:

- `src/test/python/Frontend/tests/`

Migration rules:

- move the current `testbench/tests/*` suite into the new package test root
- update imports so the tests resolve through `Frontend/` instead of
  `testbench/`
- preserve focused unit and import-compat coverage from the source tree
- treat the new package tests as the canonical Frontend verification test set

Legacy repository-root `tests/*` files are no longer the canonical home for the
Frontend verification environment. They may be bridged or replaced only where
needed to avoid broken entry points.

## Pytest and Discovery Model

The package should follow the same discoverability pattern used by MemBlock:

- `src/test/python/Frontend/tests/conftest.py` inserts the parent package root
  into `sys.path`
- pytest plugins should expose the Frontend fixtures and API modules from the
  package root
- `src/test/python/Frontend/conftest.py` should centralize any package-level
  pytest setup needed by the migrated tests

This keeps Frontend runnable in the same style as MemBlock without rewriting the
Frontend internals around a new test harness.

## Out of Scope

The following are intentionally excluded from this migration:

- `testbench/design.md`
- `testbench/docs/*`
- `testbench/web/*`
- `testbench/tools/*`
- helper shell scripts such as `run_pytest_with_log.sh` and `run_web_console.sh`
- broad behavioral cleanup unrelated to path migration
- making every historical root-level `tests/*` case green under every legacy
  invocation mode

## Acceptance Criteria

The migration is complete when all of the following are true:

1. `src/test/python/Frontend/` exists and is structurally aligned with the
   MemBlock package shape.
2. The canonical Frontend implementation lives under
   `src/test/python/Frontend/env/`.
3. The canonical Frontend tests live under `src/test/python/Frontend/tests/`.
4. `Frontend_api.py` and `Frontend_env.py` provide the expected top-level entry
   points without duplicating business logic.
5. The migrated package imports successfully from the XiangShan tree.
6. Focused Frontend tests pass from the new location.
7. Conflicting old Frontend entry points are either removed, replaced, or
   bridged to the new package.

## Risks and Mitigations

- Import breakage during path rewriting
  - Mitigation: migrate package root first, then repair imports with focused
    import-compat tests.
- Carrying too much `testbench/`-era compatibility baggage
  - Mitigation: re-evaluate each shim in the new tree instead of blindly
    copying all bridges.
- Accidentally mixing two Frontend sources of truth
  - Mitigation: make `src/test/python/Frontend/` canonical and replace
    conflicting old entry points.
- Pytest discovery mismatch between the old source tree and XiangShan layout
  - Mitigation: mirror the MemBlock `tests/conftest.py` discovery pattern.

## Implementation Notes

The migration should proceed as a package move and path-normalization task, not
as a behavior redesign. When a choice exists between preserving working logic
and polishing names, preserve working logic.
