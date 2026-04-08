# Frontend Top-Level Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `src/test/python/Frontend` present the same top-level packaging contract style as `MemBlock` while keeping the toffee implementation in `src/test/python/Frontend/env`.

**Architecture:** Treat the Frontend root as a stable facade. Keep DUT creation, fixtures, bundles, monitors, and models in `env/`, and expose only the root-level API/env entrypoints and directory documentation from the package root.

**Tech Stack:** Python, pytest, toffee-based DUT facade, existing Frontend env modules

---

### Task 1: Lock the root export contract with a failing test

**Files:**
- Create: `src/test/python/Frontend/tests/test_layout_import_compat.py`
- Test: `src/test/python/Frontend/tests/test_layout_import_compat.py`

- [ ] **Step 1: Write the failing test**

```python
def test_frontend_api_re_exports_env_api_and_fixtures():
    ...


def test_frontend_env_re_exports_env_objects():
    ...
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m pytest src/test/python/Frontend/tests/test_layout_import_compat.py -q`
Expected: FAIL because the current root wrappers do not yet expose the explicit contract the new test requires.

- [ ] **Step 3: Write minimal implementation**

Update the root adapter modules to use explicit imports and explicit `__all__` declarations, matching the objects asserted by the new test.

- [ ] **Step 4: Run test to verify it passes**

Run: `python -m pytest src/test/python/Frontend/tests/test_layout_import_compat.py -q`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/test/python/Frontend/tests/test_layout_import_compat.py src/test/python/Frontend/Frontend_api.py src/test/python/Frontend/Frontend_env.py
git commit -m "refactor: normalize frontend top-level adapters"
```

### Task 2: Document the stabilized root layout

**Files:**
- Create: `src/test/python/Frontend/README.md`
- Modify: `src/test/python/Frontend/conftest.py`

- [ ] **Step 1: Write the documentation change**

Document the responsibility split between the Frontend root facade and the real implementation under `env/`, following the style used in `MemBlock/README.md`.

- [ ] **Step 2: Run the adapter test again**

Run: `python -m pytest src/test/python/Frontend/tests/test_layout_import_compat.py -q`
Expected: PASS, confirming documentation-only changes did not disturb the package contract.

- [ ] **Step 3: Commit**

```bash
git add src/test/python/Frontend/README.md src/test/python/Frontend/conftest.py
git commit -m "docs: document frontend python verification layout"
```

### Task 3: Run focused regression checks

**Files:**
- Test: `src/test/python/Frontend/tests/test_layout_import_compat.py`
- Test: `src/test/python/Frontend/tests/test_bin_trace_dut.py`
- Test: `src/test/python/Frontend/tests/test_multi_branch.py`

- [ ] **Step 1: Run the new root-layout test**

Run: `python -m pytest src/test/python/Frontend/tests/test_layout_import_compat.py -q`
Expected: PASS

- [ ] **Step 2: Run the existing Frontend tests that are visible in the tree**

Run: `python -m pytest src/test/python/Frontend/tests/test_bin_trace_dut.py src/test/python/Frontend/tests/test_multi_branch.py -q`
Expected: SKIP when DUT env vars are unset, not import failures.

- [ ] **Step 3: Inspect the diff**

Run: `git diff -- src/test/python/Frontend docs/superpowers/specs/2026-04-08-frontend-top-level-refactor-design.md docs/superpowers/plans/2026-04-08-frontend-top-level-refactor.md`
Expected: Only the intended top-level Frontend packaging/doc/testing changes appear.
