# Frontend Toffee Env Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Refactor `src/test/python/Frontend/env/` to use a phased Toffee-style `Bundle -> Agent -> Env` structure without breaking the existing Frontend pytest fixtures, API entry points, or current test invocation style.

**Architecture:** Introduce an explicit `env/bundles/` layer first, then narrow `FrontendEnv` into a bundle-backed assembly object that wires agents, monitor, and model instances together. Migrate the highest-value DUT-facing consumers incrementally: first the new bundle-backed env assembly, then ICache/PTW/Uncache stimulus agents, then backend/monitor attachment paths, while keeping `api_Frontend_*`, `Frontend_api.py`, `Frontend_env.py`, and pytest fixtures stable.

**Tech Stack:** Python 3.11, pytest, Toffee bundles/signals, Frontend DUT Python shared library from `make frontend`, existing Frontend models/monitors/sequences.

---

## File Structure Map

### New bundle package

- Create: `src/test/python/Frontend/env/bundles/__init__.py`
- Create: `src/test/python/Frontend/env/bundles/clock_reset.py`
- Create: `src/test/python/Frontend/env/bundles/icache.py`
- Create: `src/test/python/Frontend/env/bundles/uncache.py`
- Create: `src/test/python/Frontend/env/bundles/ptw.py`
- Create: `src/test/python/Frontend/env/bundles/backend_ctrl.py`
- Create: `src/test/python/Frontend/env/bundles/backend_observe.py`
- Create: `src/test/python/Frontend/env/bundles/csr_control.py`
- Create: `src/test/python/Frontend/env/bundles/backend_from_ftq.py`
- Create: `src/test/python/Frontend/env/bundles/frontend_info.py`

### Existing Frontend env assembly to narrow

- Modify: `src/test/python/Frontend/env/frontend_env.py`
- Review: `src/test/python/Frontend/env/signal_utils.py`

### Existing DUT-facing consumers to migrate

- Modify: `src/test/python/Frontend/env/agents/icache_agent.py`
- Modify: `src/test/python/Frontend/env/agents/ptw_agent.py`
- Modify: `src/test/python/Frontend/env/agents/uncache_agent.py`
- Modify: `src/test/python/Frontend/env/agents/backend_agent.py`
- Modify: `src/test/python/Frontend/env/backend_model.py`
- Modify: `src/test/python/Frontend/env/monitors/frontend_monitor.py`

### Fixtures and compatibility surfaces that must stay stable

- Review: `src/test/python/Frontend/env/fixtures.py`
- Review: `src/test/python/Frontend/Frontend_api.py`
- Review: `src/test/python/Frontend/Frontend_env.py`
- Review: `src/test/python/Frontend/env/api.py`

### Tests to create or update

- Create: `src/test/python/Frontend/tests/test_toffee_bundle_unit.py`
- Create: `src/test/python/Frontend/tests/test_toffee_env_assembly.py`
- Modify: `src/test/python/Frontend/tests/test_frontend_env_unit.py`
- Modify: `src/test/python/Frontend/tests/test_backend_agent_unit.py`
- Modify: `src/test/python/Frontend/tests/test_monitor_unit.py`
- Run: `src/test/python/Frontend/tests/test_api_unit.py`
- Run: `src/test/python/Frontend/tests/test_multi_branch.py::test_jal_forward_jump_observes_target_pc`

## Design Notes For Implementers

- `BackendCtrlBundle` covers backend control outputs that Frontend drives into the DUT:
  `canAccept`, `commit`, `redirect`, `resolve`, `callRetCommit`.
- `BackendObserveBundle` is intentionally split out even though it was not called out by name in the spec. It is required to migrate `FrontendMonitor` off raw DUT access because the monitor samples `io_backend_cfVec_*`.
- `BackendFromFtqBundle` is required in phase 1 because `BackendModel` reads `io_backend_fromFtq_wen`, `io_backend_fromFtq_ftqIdx`, and `io_backend_fromFtq_startPc_addr` directly.
- `FrontendInfoBundle` is required in phase 1 because `BackendModel` watches `io_frontendInfo_ibufFull`.
- Phase 1 should not attempt a full Toffee rewrite of model/sequences. The plan only moves the DUT interface boundary.
- Keep external names stable:
  - fixtures: `dut`, `env`, `full_env`
  - top-level adapters: `Frontend_api.py`, `Frontend_env.py`
  - API functions: `api_Frontend_*`
- Bundle construction must tolerate the reduced dummy DUTs used in unit tests. Follow MemBlock's optional-binding pattern where a phase-1 test stub does not expose every signal in a group.

### Task 1: Lock the phase-1 contract with failing structural tests

**Files:**
- Create: `src/test/python/Frontend/tests/test_toffee_bundle_unit.py`
- Create: `src/test/python/Frontend/tests/test_toffee_env_assembly.py`
- Modify: `src/test/python/Frontend/tests/test_frontend_env_unit.py`

- [ ] **Step 1: Add a failing bundle import-and-shape test**

Create `src/test/python/Frontend/tests/test_toffee_bundle_unit.py` with tests like:

```python
from env.bundles import (
    BackendFromFtqBundle,
    BackendCtrlBundle,
    BackendObserveBundle,
    CSRControlBundle,
    ClockResetBundle,
    FrontendInfoBundle,
    ICacheBundle,
    PTWBundle,
    UncacheBundle,
)


def test_bundle_modules_export_phase1_bundle_types():
    assert ClockResetBundle.__name__ == "ClockResetBundle"
    assert ICacheBundle.__name__ == "ICacheBundle"
    assert UncacheBundle.__name__ == "UncacheBundle"
    assert PTWBundle.__name__ == "PTWBundle"
    assert BackendCtrlBundle.__name__ == "BackendCtrlBundle"
    assert BackendObserveBundle.__name__ == "BackendObserveBundle"
    assert BackendFromFtqBundle.__name__ == "BackendFromFtqBundle"
    assert FrontendInfoBundle.__name__ == "FrontendInfoBundle"
    assert CSRControlBundle.__name__ == "CSRControlBundle"


def test_backend_observe_bundle_covers_current_monitor_signals():
    required = {
        "cfvec_valid",
        "cfvec_pc",
        "cfvec_instr",
        "cfvec_is_rvc",
        "cfvec_pred_taken",
        "cfvec_exception_vec",
        "redirect_valid",
        "redirect_bits_pc",
        "redirect_bits_target",
        "redirect_bits_taken",
    }
    assert required.issubset(set(dir(BackendObserveBundle())))


def test_backend_from_ftq_and_frontend_info_bundles_cover_backend_model_reads():
    assert hasattr(BackendFromFtqBundle(), "io_backend_fromFtq_wen")
    assert hasattr(BackendFromFtqBundle(), "io_backend_fromFtq_ftqIdx")
    assert hasattr(BackendFromFtqBundle(), "io_backend_fromFtq_startPc_addr")
    assert hasattr(FrontendInfoBundle(), "io_frontendInfo_ibufFull")
```

- [ ] **Step 2: Add a failing env assembly test for explicit bundles**

Create `src/test/python/Frontend/tests/test_toffee_env_assembly.py` with tests like:

```python
import env.frontend_env as frontend_env_module


class _DummyDut:
    def __init__(self):
        self.step_ris_callbacks = []
    def StepRis(self, callback):
        self.step_ris_callbacks.append(callback)
    def Step(self, cycles):
        return int(cycles)


def test_frontend_env_exposes_phase1_bundles():
    env = frontend_env_module.FrontendEnv(_DummyDut(), register_callbacks=False)
    assert env.clock_reset is not None
    assert env.icache_if is not None
    assert env.uncache_if is not None
    assert env.ptw_if is not None
    assert env.backend_ctrl_if is not None
    assert env.backend_observe_if is not None
    assert env.backend_from_ftq_if is not None
    assert env.frontend_info_if is not None
    assert env.csr_ctrl_if is not None


def test_frontend_env_binds_bundle_interfaces_to_dummy_pins():
    dut = _DummyDut()
    dut.reset = type("Pin", (), {"value": 0})()
    dut.clock = type("Pin", (), {"value": 0})()
    dut.io_reset_vector_addr = type("Pin", (), {"value": 0})()
    dut.io_fencei = type("Pin", (), {"value": 0})()

    env = frontend_env_module.FrontendEnv(dut, register_callbacks=False)

    env.clock_reset.drive_idle()

    assert int(dut.reset.value) == 1
    assert int(dut.clock.value) == 0
```

- [ ] **Step 3: Tighten existing env unit tests to assert bundle-backed assembly**

Extend `src/test/python/Frontend/tests/test_frontend_env_unit.py` so it checks that:

```python
assert env.icache_agent.interface is env.icache_if
assert env.uncache_agent.interface is env.uncache_if
assert env.ptw_agent.interface is env.ptw_if
assert env.monitor.interface is env.backend_observe_if
assert env.backend_model.drive_if is env.backend_ctrl_if
assert env.backend_model.from_ftq_if is env.backend_from_ftq_if
assert env.backend_model.frontend_info_if is env.frontend_info_if
```

- [ ] **Step 4: Run the new tests to verify they fail for the right reason**

Run:

```bash
source /nfs/share/unitychip/activate
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 python -m pytest \
  src/test/python/Frontend/tests/test_toffee_bundle_unit.py \
  src/test/python/Frontend/tests/test_toffee_env_assembly.py \
  src/test/python/Frontend/tests/test_frontend_env_unit.py -v
```

Expected:
- collection ERROR or test FAIL because `env.bundles` does not exist yet
- or FAIL because `FrontendEnv` does not expose/bind the new bundle attributes yet

- [ ] **Step 5: Commit**

```bash
git add src/test/python/Frontend/tests/test_toffee_bundle_unit.py \
        src/test/python/Frontend/tests/test_toffee_env_assembly.py \
        src/test/python/Frontend/tests/test_frontend_env_unit.py
git commit -m "test(frontend): lock Toffee env phase-1 contract"
```

### Task 2: Create the Toffee bundle package and bundle-backed env assembly

**Files:**
- Create: `src/test/python/Frontend/env/bundles/__init__.py`
- Create: `src/test/python/Frontend/env/bundles/clock_reset.py`
- Create: `src/test/python/Frontend/env/bundles/icache.py`
- Create: `src/test/python/Frontend/env/bundles/uncache.py`
- Create: `src/test/python/Frontend/env/bundles/ptw.py`
- Create: `src/test/python/Frontend/env/bundles/backend_ctrl.py`
- Create: `src/test/python/Frontend/env/bundles/backend_observe.py`
- Create: `src/test/python/Frontend/env/bundles/csr_control.py`
- Create: `src/test/python/Frontend/env/bundles/backend_from_ftq.py`
- Create: `src/test/python/Frontend/env/bundles/frontend_info.py`
- Modify: `src/test/python/Frontend/env/frontend_env.py`

- [ ] **Step 1: Implement the bundle modules with explicit signal grouping**

Use `src/test/python/MemBlock/MemBlock_env.py` as the style reference. Each bundle file should define only one DUT interface grouping.

Example skeleton for `clock_reset.py`:

```python
from toffee import Bundle, Signal, Signals


class ClockResetBundle(Bundle):
    clock = Signal()
    reset = Signal()
    io_reset_vector_addr = Signal()
    io_fencei = Signal()

    def drive_idle(self) -> None:
        self.reset.value = 1
        self.clock.value = 0
        self.io_reset_vector_addr.value = 0
        self.io_fencei.value = 0
```

Example skeleton for `backend_ctrl.py`:

```python
from toffee import Bundle, Signal, SignalList


class BackendCtrlBundle(Bundle):
    io_backend_canAccept = Signal()
    io_backend_toFtq_commit_valid = Signal()
    io_backend_toFtq_redirect_valid = Signal()
    io_backend_toFtq_resolve_valid = SignalList("io_backend_toFtq_resolve_#_valid", 3)
    io_backend_toFtq_callRetCommit_valid = SignalList("io_backend_toFtq_callRetCommit_#_valid", 8)
```

Example skeleton for `backend_observe.py`:

```python
from toffee import Bundle, SignalList


class BackendObserveBundle(Bundle):
    cfvec_valid = SignalList("io_backend_cfVec_#_valid", 8)
    cfvec_pc = SignalList("io_backend_cfVec_#_bits_pc", 8)
    cfvec_instr = SignalList("io_backend_cfVec_#_bits_instr", 8)
    cfvec_is_rvc = SignalList("io_backend_cfVec_#_bits_isRvc", 8)
    cfvec_pred_taken = SignalList("io_backend_cfVec_#_bits_predTaken", 8)
    cfvec_exception_vec = ...
    redirect_valid = ...
    redirect_bits_pc = ...
    redirect_bits_target = ...
    redirect_bits_taken = ...
```

Example skeleton for `backend_from_ftq.py`:

```python
from toffee import Bundle, Signal


class BackendFromFtqBundle(Bundle):
    io_backend_fromFtq_wen = Signal()
    io_backend_fromFtq_ftqIdx = Signal()
    io_backend_fromFtq_startPc_addr = Signal()
```

- [ ] **Step 2: Re-export the bundle package**

In `src/test/python/Frontend/env/bundles/__init__.py`, export all phase-1 bundles:

```python
from .backend_ctrl import BackendCtrlBundle
from .backend_from_ftq import BackendFromFtqBundle
from .backend_observe import BackendObserveBundle
from .clock_reset import ClockResetBundle
from .csr_control import CSRControlBundle
from .frontend_info import FrontendInfoBundle
from .icache import ICacheBundle
from .ptw import PTWBundle
from .uncache import UncacheBundle
```

- [ ] **Step 3: Make `FrontendEnv` instantiate bundles eagerly**

Modify `src/test/python/Frontend/env/frontend_env.py` to:

- import the bundle classes
- create attributes
  - `self.clock_reset`
  - `self.icache_if`
  - `self.uncache_if`
  - `self.ptw_if`
  - `self.backend_ctrl_if`
  - `self.backend_observe_if`
  - `self.csr_ctrl_if`
- narrow `_init_inputs()` so it delegates initialization through bundle methods instead of writing every signal directly via generic helpers

Use an assembly helper such as:

```python
def _create_interfaces(self) -> None:
    self.clock_reset = ClockResetBundle.from_prefix("").bind(self.dut)
    self.icache_if = ICacheBundle.from_prefix("").bind(self.dut)
    self.uncache_if = UncacheBundle.from_prefix("").bind(self.dut)
    self.ptw_if = PTWBundle.from_prefix("").bind(self.dut)
    self.backend_ctrl_if = BackendCtrlBundle.from_prefix("").bind(self.dut)
    self.backend_observe_if = BackendObserveBundle.from_prefix("").bind(self.dut)
    self.backend_from_ftq_if = BackendFromFtqBundle.from_prefix("").bind(self.dut)
    self.frontend_info_if = FrontendInfoBundle.from_prefix("").bind(self.dut)
    self.csr_ctrl_if = CSRControlBundle.from_prefix("").bind(self.dut)
```

Where a dummy DUT used by unit tests does not expose every signal in a bundle,
use an optional wrapper or defensive construction pattern rather than making
bundle creation unconditionally strict.

- [ ] **Step 4: Keep high-level env helpers stable**

Preserve external method names and behavior in `FrontendEnv`:

- `step`
- `Step`
- `reset`
- `initialize`
- waveform helpers
- program/trace loading helpers

Do not change fixture signatures or public API imports in this task.

- [ ] **Step 5: Run the assembly tests to verify they now pass**

Run:

```bash
source /nfs/share/unitychip/activate
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 python -m pytest \
  src/test/python/Frontend/tests/test_toffee_bundle_unit.py \
  src/test/python/Frontend/tests/test_toffee_env_assembly.py \
  src/test/python/Frontend/tests/test_frontend_env_unit.py -v
```

Expected:
- PASS

- [ ] **Step 6: Commit**

```bash
git add src/test/python/Frontend/env/bundles \
        src/test/python/Frontend/env/frontend_env.py \
        src/test/python/Frontend/tests/test_toffee_bundle_unit.py \
        src/test/python/Frontend/tests/test_toffee_env_assembly.py \
        src/test/python/Frontend/tests/test_frontend_env_unit.py
git commit -m "refactor(frontend): add Toffee bundle-backed env assembly"
```

### Task 3: Migrate ICache, PTW, and Uncache agents to bind bundles

**Files:**
- Modify: `src/test/python/Frontend/env/agents/icache_agent.py`
- Modify: `src/test/python/Frontend/env/agents/ptw_agent.py`
- Modify: `src/test/python/Frontend/env/agents/uncache_agent.py`
- Modify: `src/test/python/Frontend/tests/test_frontend_env_unit.py`
- Run: `src/test/python/Frontend/tests/test_api_unit.py`

- [ ] **Step 1: Add a failing env/agent binding assertion**

Extend `src/test/python/Frontend/tests/test_frontend_env_unit.py` with a test like:

```python
def test_frontend_env_passes_phase1_bundles_into_main_agents():
    dut = _DummyDut()
    env = frontend_env_module.FrontendEnv(dut, register_callbacks=False)
    assert env.icache_agent.interface is env.icache_if
    assert env.uncache_agent.interface is env.uncache_if
    assert env.ptw_agent.interface is env.ptw_if
```

- [ ] **Step 2: Run the test to verify it fails**

Run:

```bash
source /nfs/share/unitychip/activate
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 python -m pytest \
  src/test/python/Frontend/tests/test_frontend_env_unit.py::test_frontend_env_passes_phase1_bundles_into_main_agents -v
```

Expected:
- FAIL because agents still expose only raw `dut` binding

- [ ] **Step 3: Change the three main stimulus agents to store explicit interfaces**

In each of:

- `src/test/python/Frontend/env/agents/icache_agent.py`
- `src/test/python/Frontend/env/agents/ptw_agent.py`
- `src/test/python/Frontend/env/agents/uncache_agent.py`

replace `self.dut`-centric access with `self.interface`-centric access for the primary binding surface.

Pattern:

```python
class ICacheAgent:
    def __init__(self, memory):
        self.interface = None

    def bind(self, interface) -> None:
        self.interface = interface
```

Then in the logic, prefer `self.interface.<signal>.value` access instead of `get_sig/set_sig(self.dut, "...")`.

- [ ] **Step 4: Update `FrontendEnv` assembly to bind those agents with bundles**

In `src/test/python/Frontend/env/frontend_env.py`, bind:

```python
self.icache_agent.bind(self.icache_if)
self.uncache_agent.bind(self.uncache_if)
self.ptw_agent.bind(self.ptw_if)
```

Do not change fixture names or external API.

- [ ] **Step 5: Run the focused env and API regression tests**

Run:

```bash
source /nfs/share/unitychip/activate
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 python -m pytest \
  src/test/python/Frontend/tests/test_frontend_env_unit.py \
  src/test/python/Frontend/tests/test_api_unit.py -v
```

Expected:
- PASS

- [ ] **Step 6: Commit**

```bash
git add src/test/python/Frontend/env/agents/icache_agent.py \
        src/test/python/Frontend/env/agents/ptw_agent.py \
        src/test/python/Frontend/env/agents/uncache_agent.py \
        src/test/python/Frontend/env/frontend_env.py \
        src/test/python/Frontend/tests/test_frontend_env_unit.py
git commit -m "refactor(frontend): bind core agents through Toffee bundles"
```

### Task 4: Migrate backend and monitor attachment paths off raw DUT

**Files:**
- Modify: `src/test/python/Frontend/env/agents/backend_agent.py`
- Modify: `src/test/python/Frontend/env/backend_model.py`
- Modify: `src/test/python/Frontend/env/monitors/frontend_monitor.py`
- Modify: `src/test/python/Frontend/tests/test_backend_agent_unit.py`
- Modify: `src/test/python/Frontend/tests/test_monitor_unit.py`
- Modify: `src/test/python/Frontend/tests/test_frontend_env_unit.py`

- [ ] **Step 1: Add failing tests for bundle-backed backend and monitor binding**

Update tests so they assert:

```python
assert env.backend_model.drive_if is env.backend_ctrl_if
assert env.backend_model.observe_if is env.backend_observe_if
assert env.backend_model.from_ftq_if is env.backend_from_ftq_if
assert env.backend_model.frontend_info_if is env.frontend_info_if
assert env.monitor.interface is env.backend_observe_if
```

and adapt backend-agent unit setup to bind a backend control interface object rather than a raw DUT.

- [ ] **Step 2: Run focused tests to verify the new assertions fail**

Run:

```bash
source /nfs/share/unitychip/activate
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 python -m pytest \
  src/test/python/Frontend/tests/test_backend_agent_unit.py \
  src/test/python/Frontend/tests/test_monitor_unit.py \
  src/test/python/Frontend/tests/test_frontend_env_unit.py -v
```

Expected:
- FAIL because backend/monitor still use raw DUT binding

- [ ] **Step 3: Refactor `BackendAgent` to consume `BackendCtrlBundle`**

In `src/test/python/Frontend/env/agents/backend_agent.py`:

- replace `self.dut` with `self.interface`
- drive commit/redirect/resolve/call-ret valid/data through `BackendCtrlBundle`
- keep encoded address semantics identical

- [ ] **Step 4: Refactor `FrontendMonitor` to consume `BackendObserveBundle`**

In `src/test/python/Frontend/env/monitors/frontend_monitor.py`:

- replace the raw `get_sig()` access for `io_backend_cfVec_*` with reads from `self.interface`
- keep observation semantics and redirect suppression behavior unchanged

- [ ] **Step 5: Narrow `BackendModel`’s DUT-facing surface through `BackendAgent`**

In `src/test/python/Frontend/env/backend_model.py`:

- rename the primary bind target to explicit interface-oriented attributes
- ensure all backend drive paths go through the updated `BackendAgent`
- read `io_backend_fromFtq_*` through `BackendFromFtqBundle`
- read `io_frontendInfo_ibufFull` through `FrontendInfoBundle`
- keep raw-DUT-independent read-side access for `cfVec` through `BackendObserveBundle`
- do not change commit/redirect/resolve semantics in this task

- [ ] **Step 6: Bind backend and monitor bundles from `FrontendEnv`**

In `src/test/python/Frontend/env/frontend_env.py`, wire:

```python
self.backend_model.bind_interfaces(
    drive_if=self.backend_ctrl_if,
    observe_if=self.backend_observe_if,
    from_ftq_if=self.backend_from_ftq_if,
    frontend_info_if=self.frontend_info_if,
)
self.monitor.bind(self.backend_observe_if)
```

or the equivalent assembly path actually used by the file after refactor.

- [ ] **Step 7: Run focused unit regressions**

Run:

```bash
source /nfs/share/unitychip/activate
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 python -m pytest \
  src/test/python/Frontend/tests/test_backend_agent_unit.py \
  src/test/python/Frontend/tests/test_monitor_unit.py \
  src/test/python/Frontend/tests/test_frontend_env_unit.py -v
```

Expected:
- PASS

- [ ] **Step 8: Commit**

```bash
git add src/test/python/Frontend/env/agents/backend_agent.py \
        src/test/python/Frontend/env/backend_model.py \
        src/test/python/Frontend/env/monitors/frontend_monitor.py \
        src/test/python/Frontend/env/frontend_env.py \
        src/test/python/Frontend/tests/test_backend_agent_unit.py \
        src/test/python/Frontend/tests/test_monitor_unit.py \
        src/test/python/Frontend/tests/test_frontend_env_unit.py
git commit -m "refactor(frontend): move backend and monitor onto Toffee bundles"
```

### Task 5: Verify external compatibility, real DUT behavior, and waveform generation

**Files:**
- Review: `src/test/python/Frontend/env/fixtures.py`
- Review: `src/test/python/Frontend/Frontend_api.py`
- Review: `src/test/python/Frontend/Frontend_env.py`
- Run: `src/test/python/Frontend/tests/test_api_unit.py`
- Run: `src/test/python/Frontend/tests/test_layout_import_compat.py`
- Run: `src/test/python/Frontend/tests/test_agent_import_compat.py`
- Run: `src/test/python/Frontend/tests/test_multi_branch.py::test_jal_forward_jump_observes_target_pc`

- [ ] **Step 1: Run focused compatibility regressions on the refactored env**

Run:

```bash
source /nfs/share/unitychip/activate
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 python -m pytest \
  src/test/python/Frontend/tests/test_layout_import_compat.py \
  src/test/python/Frontend/tests/test_agent_import_compat.py \
  src/test/python/Frontend/tests/test_api_unit.py \
  src/test/python/Frontend/tests/test_frontend_env_unit.py \
  src/test/python/Frontend/tests/test_backend_agent_unit.py \
  src/test/python/Frontend/tests/test_monitor_unit.py -v
```

Expected:
- PASS

- [ ] **Step 2: Build the real Frontend DUT if needed**

Run:

```bash
mkdir -p build
source /nfs/share/unitychip/activate
make frontend
```

Expected:
- exit 0
- `build-frontend/pylib/Frontend/libUTFrontend.so` exists

- [ ] **Step 3: Run one real DUT-backed test with waveform dumping enabled**

Run:

```bash
rm -rf /tmp/frontend-fst
mkdir -p /tmp/frontend-fst
source /nfs/share/unitychip/activate
TB_ENABLE_DUT_TESTS=1 \
TB_ENABLE_FST_DUMP=1 \
TB_WAVEFORM_DIR=/tmp/frontend-fst \
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 \
python -m pytest -v \
  src/test/python/Frontend/tests/test_multi_branch.py::test_jal_forward_jump_observes_target_pc
```

Expected:
- PASS
- `/tmp/frontend-fst/test_jal_forward_jump_observes_target_pc.fst` exists

- [ ] **Step 4: Run the full Frontend suite as final phase-1 evidence**

Run:

```bash
source /nfs/share/unitychip/activate
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 python -m pytest src/test/python/Frontend/tests -v
```

Expected:
- same pass/skip profile as before, or clearly explained intentional changes only

- [ ] **Step 5: Commit**

```bash
git add src/test/python/Frontend
git commit -m "test(frontend): verify Toffee env phase-1 compatibility"
```

### Task 6: Clean up raw signal orchestration leftovers

**Files:**
- Modify: `src/test/python/Frontend/env/frontend_env.py`
- Review: `src/test/python/Frontend/env/agents/*.py`
- Review: `src/test/python/Frontend/env/monitors/frontend_monitor.py`
- Review: `src/test/python/Frontend/env/backend_model.py`

- [ ] **Step 1: Search for remaining raw-DUT orchestration hot spots**

Run:

```bash
rg -n "get_sig\\(|set_sig\\(|has_sig\\(" src/test/python/Frontend/env
```

Expected:
- remaining matches are limited to intentionally retained helpers or truly
  unconverted code paths

- [ ] **Step 2: Remove `FrontendEnv`-local raw signal initialization that is now redundant**

Delete or collapse code paths in `src/test/python/Frontend/env/frontend_env.py`
that still duplicate work already handled by bundles.

- [ ] **Step 3: Re-run the focused suite**

Run:

```bash
source /nfs/share/unitychip/activate
PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 python -m pytest \
  src/test/python/Frontend/tests/test_toffee_bundle_unit.py \
  src/test/python/Frontend/tests/test_toffee_env_assembly.py \
  src/test/python/Frontend/tests/test_frontend_env_unit.py \
  src/test/python/Frontend/tests/test_backend_agent_unit.py \
  src/test/python/Frontend/tests/test_monitor_unit.py \
  src/test/python/Frontend/tests/test_api_unit.py -v
```

Expected:
- PASS

- [ ] **Step 4: Commit**

```bash
git add src/test/python/Frontend/env \
        src/test/python/Frontend/tests
git commit -m "refactor(frontend): remove redundant raw DUT orchestration"
```

## Final Verification Checklist

- [ ] `env/bundles/` exists and is the canonical home for Frontend DUT bundle definitions
- [ ] `FrontendEnv` exposes explicit bundle attributes for phase-1 interfaces
- [ ] ICache/PTW/Uncache agents bind their primary interface through bundles
- [ ] backend drive paths use `BackendCtrlBundle`
- [ ] monitor sampling uses `BackendObserveBundle`
- [ ] fixtures and top-level Frontend API imports still work
- [ ] a real DUT-backed test still passes
- [ ] waveform dumping still produces `.fst`

## Review Handoff

After saving this plan:

1. Run a plan-document review against:
   - spec: `docs/superpowers/specs/2026-04-08-frontend-toffee-env-refactor-design.md`
   - plan: `docs/superpowers/plans/2026-04-08-frontend-toffee-env-refactor.md`
2. Fix issues from the review
3. Present the final plan path to the user for approval before implementation
