# Frontend Package Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Migrate the refactored Frontend Python verification environment from `/tmp/frontend_bt_head_20260403_clean/testbench/` into `src/test/python/Frontend/` in XiangShan, including env/tests plus the remaining non-doc operational assets: web console, helper tools, shell entry points, and packaged bins.

**Architecture:** Create a new `src/test/python/Frontend/` package that mirrors MemBlock's outer workspace shape while preserving the already-refactored Frontend internals under `env/` and `tests/`. Use thin root adapters (`Frontend_api.py`, `Frontend_env.py`, package `conftest.py`), promote tool scripts and shell entry points into the Frontend package root, move the web console into `webui/`, and normalize imports/paths so everything resolves from the new location.

**Tech Stack:** Python 3, pytest, toffee/unitychip DUT bindings, XiangShan `src/test/python` package layout.

---

## File Structure Map

### New canonical package root

- Create: `src/test/python/Frontend/conftest.py`
- Create: `src/test/python/Frontend/Frontend_api.py`
- Create: `src/test/python/Frontend/Frontend_env.py`
- Create: `src/test/python/Frontend/nemu_bin_to_golden_trace.py`
- Create: `src/test/python/Frontend/nemu_log_to_golden_trace.py`
- Create: `src/test/python/Frontend/run_dut_with_bin_trace.py`
- Create: `src/test/python/Frontend/run_bin_trace_pipeline.sh`
- Create: `src/test/python/Frontend/run_pytest_with_log.sh`
- Create: `src/test/python/Frontend/run_web_console.sh`
- Create: `src/test/python/Frontend/bins/`
- Create: `src/test/python/Frontend/env/`
- Create: `src/test/python/Frontend/tests/`
- Create: `src/test/python/Frontend/webui/`

### Environment subtree copied from source worktree

Source root: `/tmp/frontend_bt_head_20260403_clean/testbench/env/`

- Create: `src/test/python/Frontend/env/api.py`
- Create: `src/test/python/Frontend/env/fixtures.py`
- Create: `src/test/python/Frontend/env/frontend_env.py`
- Create: `src/test/python/Frontend/env/env_config.py`
- Create: `src/test/python/Frontend/env/request_apis.py`
- Create: `src/test/python/Frontend/env/transactions.py`
- Create: `src/test/python/Frontend/env/logging_utils.py`
- Create: `src/test/python/Frontend/env/coverage_def.py`
- Create: `src/test/python/Frontend/env/exceptions.py`
- Create: `src/test/python/Frontend/env/rvc_decoder.py`
- Create: `src/test/python/Frontend/env/signal_utils.py`
- Create: `src/test/python/Frontend/env/nemu_trace_converter.py`
- Create: `src/test/python/Frontend/env/nemu_trace_pipeline.py`
- Create: `src/test/python/Frontend/env/backend_model.py`
- Create: `src/test/python/Frontend/env/memory_model.py`
- Create: `src/test/python/Frontend/env/monitor.py`
- Create: `src/test/python/Frontend/env/trace.py`
- Create: `src/test/python/Frontend/env/agents_legacy.py`
- Create: `src/test/python/Frontend/env/agents/__init__.py`
- Create: `src/test/python/Frontend/env/agents/backend_agent.py`
- Create: `src/test/python/Frontend/env/agents/icache_agent.py`
- Create: `src/test/python/Frontend/env/agents/ptw_agent.py`
- Create: `src/test/python/Frontend/env/agents/uncache_agent.py`
- Create: `src/test/python/Frontend/env/monitors/__init__.py`
- Create: `src/test/python/Frontend/env/monitors/frontend_monitor.py`
- Create: `src/test/python/Frontend/env/model/__init__.py`
- Create: `src/test/python/Frontend/env/model/backend_state.py`
- Create: `src/test/python/Frontend/env/model/branch_checker.py`
- Create: `src/test/python/Frontend/env/model/ftq_scoreboard.py`
- Create: `src/test/python/Frontend/env/model/golden_trace.py`
- Create: `src/test/python/Frontend/env/model/memory_model.py`
- Create: `src/test/python/Frontend/env/model/page_table_model.py`
- Create: `src/test/python/Frontend/env/sequences/__init__.py`
- Create: `src/test/python/Frontend/env/sequences/program_sequences.py`
- Create: `src/test/python/Frontend/env/sequences/redirect_sequences.py`
- Create: `src/test/python/Frontend/env/sequences/reset_sequences.py`
- Create: `src/test/python/Frontend/env/sequences/trace_sequences.py`

### Tests subtree copied from source worktree

Source root: `/tmp/frontend_bt_head_20260403_clean/testbench/tests/`

- Create: `src/test/python/Frontend/tests/conftest.py`
- Create: `src/test/python/Frontend/tests/test_agent_import_compat.py`
- Create: `src/test/python/Frontend/tests/test_api_unit.py`
- Create: `src/test/python/Frontend/tests/test_backend_agent_unit.py`
- Create: `src/test/python/Frontend/tests/test_backend_model_mispredict.py`
- Create: `src/test/python/Frontend/tests/test_backend_state_unit.py`
- Create: `src/test/python/Frontend/tests/test_bin_trace_dut.py`
- Create: `src/test/python/Frontend/tests/test_branch_checker_unit.py`
- Create: `src/test/python/Frontend/tests/test_env_config_unit.py`
- Create: `src/test/python/Frontend/tests/test_frontend_env_unit.py`
- Create: `src/test/python/Frontend/tests/test_layout_import_compat.py`
- Create: `src/test/python/Frontend/tests/test_memory_model.py`
- Create: `src/test/python/Frontend/tests/test_monitor_unit.py`
- Create: `src/test/python/Frontend/tests/test_multi_branch.py`
- Create: `src/test/python/Frontend/tests/test_nemu_trace_converter.py`
- Create: `src/test/python/Frontend/tests/test_nemu_trace_pipeline.py`
- Create: `src/test/python/Frontend/tests/test_sequence_unit.py`
- Create: `src/test/python/Frontend/tests/test_trace.py`

### Remaining non-doc assets copied from source worktree

Source roots:
- `/tmp/frontend_bt_head_20260403_clean/testbench/web/`
- `/tmp/frontend_bt_head_20260403_clean/testbench/tools/`
- `/tmp/frontend_bt_head_20260403_clean/testbench/run_*.sh`
- `/tmp/frontend_bt_head_20260403_clean/testbench/bins/`

- Create: `src/test/python/Frontend/webui/__init__.py`
- Create: `src/test/python/Frontend/webui/event_bus.py`
- Create: `src/test/python/Frontend/webui/runner.py`
- Create: `src/test/python/Frontend/webui/server.py`
- Create: `src/test/python/Frontend/webui/static/index.html`
- Create: `src/test/python/Frontend/webui/static/pc_trace_markers.js`
- Create: `src/test/python/Frontend/webui/README.md`
- Create: `src/test/python/Frontend/nemu_bin_to_golden_trace.py`
- Create: `src/test/python/Frontend/nemu_log_to_golden_trace.py`
- Create: `src/test/python/Frontend/run_dut_with_bin_trace.py`
- Create: `src/test/python/Frontend/run_bin_trace_pipeline.sh`
- Create: `src/test/python/Frontend/run_pytest_with_log.sh`
- Create: `src/test/python/Frontend/run_web_console.sh`
- Create: `src/test/python/Frontend/bins/microbench.bin`

---

### Task 1: Scaffold the new Frontend package root

**Files:**
- Create: `src/test/python/Frontend/conftest.py`
- Create: `src/test/python/Frontend/env/`
- Create: `src/test/python/Frontend/tests/`

- [ ] **Step 1: Create the failing structural check**

Add a tiny smoke assertion script or use shell checks to prove the package root does not exist yet.

Run:
```bash
test -d src/test/python/Frontend && echo unexpected || echo missing
```
Expected: `missing`

- [ ] **Step 2: Create the directory skeleton**

Run:
```bash
mkdir -p src/test/python/Frontend/env src/test/python/Frontend/tests
```

- [ ] **Step 3: Add package root bootstrap files**

Use this `src/test/python/Frontend/conftest.py` as the initial package bootstrap:

```python
# coding=utf-8
import os
import sys

_TEST_ROOT = os.path.dirname(os.path.abspath(__file__))
_REPO_ROOT = os.path.abspath(os.path.join(_TEST_ROOT, "..", "..", "..", ".."))
_DATA_DIR = os.path.join(_TEST_ROOT, "data")

for _path in (_TEST_ROOT,):
    if _path not in sys.path:
        sys.path.insert(0, _path)

os.makedirs(_DATA_DIR, exist_ok=True)
```

- [ ] **Step 4: Verify the skeleton exists**

Run:
```bash
find src/test/python/Frontend -maxdepth 2 -type d | sort
```
Expected to include:
- `src/test/python/Frontend`
- `src/test/python/Frontend/env`
- `src/test/python/Frontend/tests`

- [ ] **Step 5: Commit**

```bash
git add src/test/python/Frontend/conftest.py
git commit -m "refactor(frontend): scaffold XiangShan Frontend package root"
```

### Task 2: Copy the canonical env implementation into the new package

**Files:**
- Create: `src/test/python/Frontend/env/*`
- Source: `/tmp/frontend_bt_head_20260403_clean/testbench/env/*`

- [ ] **Step 1: Copy the env subtree without pycache artifacts**

Run:
```bash
rsync -a --exclude '__pycache__' /tmp/frontend_bt_head_20260403_clean/testbench/env/ src/test/python/Frontend/env/
```

- [ ] **Step 2: Verify the copied file set**

Run:
```bash
find src/test/python/Frontend/env -type f | sort
```
Expected to include `api.py`, `fixtures.py`, `frontend_env.py`, `agents/`, `monitors/`, `model/`, and `sequences/` files.

- [ ] **Step 3: Normalize the package docstring surface only where needed**

Check `src/test/python/Frontend/env/__init__.py` and keep it as the conservative package facade. Do not flatten the implementation into the root package.

- [ ] **Step 4: Run a syntax-only sanity pass**

Run:
```bash
python -m py_compile $(find src/test/python/Frontend/env -type f -name '*.py' | tr '\n' ' ')
```
Expected: no output

- [ ] **Step 5: Commit**

```bash
git add src/test/python/Frontend/env
git commit -m "refactor(frontend): migrate canonical env package"
```

### Task 3: Add thin MemBlock-style root adapters

**Files:**
- Create: `src/test/python/Frontend/Frontend_api.py`
- Create: `src/test/python/Frontend/Frontend_env.py`

- [ ] **Step 1: Write the adapter modules**

Use this `src/test/python/Frontend/Frontend_api.py` pattern:

```python
# coding=utf-8
"""Thin Frontend package API adapter."""

from env.api import *  # noqa: F401,F403
from env.fixtures import create_dut, dut  # noqa: F401
```

Use this `src/test/python/Frontend/Frontend_env.py` pattern:

```python
# coding=utf-8
"""Thin Frontend package env adapter."""

from env.fixtures import env, full_env  # noqa: F401
from env.frontend_env import FrontendEnv  # noqa: F401
```

- [ ] **Step 2: Verify imports fail before fixes if adapter references are wrong**

Run:
```bash
python - <<'PY'
import sys
sys.path.insert(0, 'src/test/python/Frontend')
import Frontend_api
import Frontend_env
print('ok')
PY
```
Expected after implementation: `ok`

- [ ] **Step 3: Keep adapters thin**

Do not move logic from `env/api.py`, `env/fixtures.py`, or `env/frontend_env.py` into these files.

- [ ] **Step 4: Commit**

```bash
git add src/test/python/Frontend/Frontend_api.py src/test/python/Frontend/Frontend_env.py
git commit -m "refactor(frontend): add package root adapters"
```

### Task 4: Migrate the test suite into the new package root

**Files:**
- Create: `src/test/python/Frontend/tests/*`
- Source: `/tmp/frontend_bt_head_20260403_clean/testbench/tests/*`
- Modify: `src/test/python/Frontend/tests/conftest.py`

- [ ] **Step 1: Copy the tests subtree**

Run:
```bash
rsync -a --exclude '__pycache__' /tmp/frontend_bt_head_20260403_clean/testbench/tests/ src/test/python/Frontend/tests/
```

- [ ] **Step 2: Rewrite `tests/conftest.py` to follow the MemBlock discovery pattern**

Use this content for `src/test/python/Frontend/tests/conftest.py`:

```python
# coding=utf-8
import os
import sys

_PARENT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if _PARENT not in sys.path:
    sys.path.insert(0, _PARENT)

pytest_plugins = [
    'Frontend_api',
    'Frontend_env',
]
```

- [ ] **Step 3: Confirm pytest discovers the moved test files**

Run:
```bash
python -m pytest src/test/python/Frontend/tests --collect-only -q
```
Expected: the migrated `test_*.py` files are collected from the new location.

- [ ] **Step 4: Commit**

```bash
git add src/test/python/Frontend/tests
git commit -m "refactor(frontend): migrate canonical test suite"
```

### Task 5: Normalize imports from `testbench` to `Frontend`

**Files:**
- Modify: `src/test/python/Frontend/env/*.py`
- Modify: `src/test/python/Frontend/env/**/*.py`
- Modify: `src/test/python/Frontend/tests/*.py`

- [ ] **Step 1: Find all stale import roots**

Run:
```bash
rg -n 'testbench\.|from testbench|import testbench' src/test/python/Frontend
```
Expected initially: matches in copied files.

- [ ] **Step 2: Replace testbench-root imports with Frontend-root or local-package imports**

Examples to apply carefully:

```python
from testbench.env.fixtures import *
```
becomes
```python
from env.fixtures import *
```

```python
from testbench.env.agents.backend_agent import BackendAgent
```
becomes either
```python
from env.agents.backend_agent import BackendAgent
```
or a relative import if the file already uses package-relative style.

- [ ] **Step 3: Normalize any hard-coded `testbench/data` assumptions**

Keep artifact output under `src/test/python/Frontend/data` or another package-local data directory, not under the removed `testbench/` tree.

- [ ] **Step 4: Re-run stale import search**

Run:
```bash
rg -n 'testbench\.|from testbench|import testbench' src/test/python/Frontend
```
Expected: no matches, unless a comment intentionally documents the source path.

- [ ] **Step 5: Commit**

```bash
git add src/test/python/Frontend
git commit -m "refactor(frontend): normalize moved package imports"
```

### Task 6: Re-evaluate and trim migration-era shims

**Files:**
- Review: `src/test/python/Frontend/env/backend_model.py`
- Review: `src/test/python/Frontend/env/monitor.py`
- Review: `src/test/python/Frontend/env/memory_model.py`
- Review: `src/test/python/Frontend/env/trace.py`
- Review: `src/test/python/Frontend/env/agents_legacy.py`
- Modify/Delete as needed: only the shims no longer justified in the new package

- [ ] **Step 1: Map which shims are still imported**

Run:
```bash
rg -n 'backend_model|agents_legacy|from env\.monitor|from env\.memory_model|from env\.trace' src/test/python/Frontend
```

- [ ] **Step 2: Remove only the shims that no longer reduce risk**

Rules:
- keep the shim if migrated tests or root adapters still use it
- delete the shim if it only served the old `testbench/` layout and nothing in `src/test/python/Frontend/` imports it

- [ ] **Step 3: Re-run import collection after each deletion**

Run:
```bash
python -m pytest src/test/python/Frontend/tests --collect-only -q
```
Expected: collection still passes

- [ ] **Step 4: Commit**

```bash
git add -A src/test/python/Frontend
git commit -m "refactor(frontend): trim obsolete migration shims"
```

### Task 7: Replace conflicting old Frontend entry points if any exist

**Files:**
- Review: repo-wide Frontend-related Python entry points
- Modify/Create/Delete: only conflicting legacy Frontend entry points discovered in the XiangShan tree

- [ ] **Step 1: Search for conflicting Frontend roots**

Run:
```bash
find . -type f | rg 'Frontend_api.py|Frontend_env.py|test_Frontend|frontend_env'
```

- [ ] **Step 2: If conflicts exist, replace them with thin bridges to `src/test/python/Frontend/`**

Example bridge pattern:

```python
from src.test.python.Frontend.Frontend_api import *  # noqa: F401,F403
```

Use this only if a legacy file must remain for compatibility. Prefer deleting duplicated dead entry points.

- [ ] **Step 3: If no conflicts exist, record that this task is a verified no-op**

Capture the empty search result in the task notes or commit message body.

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "refactor(frontend): unify repository entry points"
```

### Task 8: Run focused verification on the migrated package

**Files:**
- Test: `src/test/python/Frontend/tests/test_layout_import_compat.py`
- Test: `src/test/python/Frontend/tests/test_agent_import_compat.py`
- Test: `src/test/python/Frontend/tests/test_monitor_unit.py`
- Test: `src/test/python/Frontend/tests/test_api_unit.py`
- Test: `src/test/python/Frontend/tests/test_nemu_trace_converter.py`
- Test: `src/test/python/Frontend/tests/test_nemu_trace_pipeline.py`

- [ ] **Step 1: Run the focused import and package verification subset**

Run:
```bash
source /nfs/share/unitychip/activate && \
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 python -m pytest \
  src/test/python/Frontend/tests/test_layout_import_compat.py \
  src/test/python/Frontend/tests/test_agent_import_compat.py \
  src/test/python/Frontend/tests/test_monitor_unit.py \
  src/test/python/Frontend/tests/test_api_unit.py \
  src/test/python/Frontend/tests/test_nemu_trace_converter.py \
  src/test/python/Frontend/tests/test_nemu_trace_pipeline.py \
  -v
```
Expected: all pass from the new path.

- [ ] **Step 2: Fix only path-migration regressions**

If failures occur, limit fixes to import roots, package bootstrap, path assumptions, or package-local data paths. Do not broaden into unrelated behavior work.

- [ ] **Step 3: Commit**

```bash
git add src/test/python/Frontend
git commit -m "test(frontend): restore focused package verification"
```

### Task 9: Run broader canonical Frontend regression from the new home

**Files:**
- Test: `src/test/python/Frontend/tests/`

- [ ] **Step 1: Run the canonical Frontend package test suite**

Run:
```bash
source /nfs/share/unitychip/activate && \
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 python -m pytest src/test/python/Frontend/tests -v
```
Expected: migrated suite is collected and the previously focused subset remains green.

- [ ] **Step 2: Classify outcomes explicitly**

If there are failures, separate them into:
- path-migration regressions to fix now
- known refactor-stage skips / external-environment limitations to document

- [ ] **Step 3: Re-run after fixes until the migration claim is justified**

Do not claim completion until the new package path is proven by real pytest output.

- [ ] **Step 4: Commit**

```bash
git add src/test/python/Frontend
git commit -m "test(frontend): validate migrated canonical suite"
```

### Task 10: Final cleanup and integration review

**Files:**
- Review: `src/test/python/Frontend/`
- Review: any legacy Frontend bridges touched during migration

- [ ] **Step 1: Inspect the final diff for accidental carry-over**

Run:
```bash
git diff --stat HEAD~10..HEAD
```

- [ ] **Step 2: Verify no out-of-scope docs were copied**

Run:
```bash
find src/test/python/Frontend -type f | rg 'design\.md|/docs/'
```
Expected: no matches.

- [ ] **Step 3: Record final acceptance evidence**

Collect:
- focused test command and result
- broad package test command and result
- list of any retained shims and why they remain
- list of any replaced legacy entry points

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "Finalize Frontend package migration into XiangShan

Constraint: Frontend must live under src/test/python alongside MemBlock
Rejected: Keep the original testbench root shape inside XiangShan | conflicts with requested package layout
Confidence: medium
Scope-risk: moderate
Directive: Treat src/test/python/Frontend as canonical; do not reintroduce a second Frontend source tree
Tested: Focused Frontend package tests and canonical Frontend pytest suite
Not-tested: Out-of-scope docs migration"
```
