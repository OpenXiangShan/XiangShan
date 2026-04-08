# Frontend Toffee Env Refactor Design

## Goal

Refactor the canonical Frontend verification environment under
`src/test/python/Frontend/env/` to use Toffee's `Bundle -> Agent -> Env`
organization for DUT-facing orchestration, while preserving the existing
external test and API surface.

This is a new workstream independent from the earlier migration that moved the
Frontend verification stack into `src/test/python/Frontend/`.

## User-Approved Constraints

- This work is different from the prior migration effort and should be treated
  as a new refactor.
- Use Toffee as the architectural direction.
- Keep the external interface stable in phase 1:
  - existing pytest fixtures should remain usable
  - existing `Frontend_api.py` / `Frontend_env.py` entry points should remain
    usable
  - existing tests should keep their current invocation style where practical
- Execute the refactor in phases rather than doing a full rewrite in one step.
- All plans for this work must be written as documents.

## Current State

The current Frontend environment is centered on
`src/test/python/Frontend/env/frontend_env.py`. It directly:

- stores the raw DUT handle
- binds collaborators manually
- initializes many DUT signals by name
- registers `StepRis` callbacks directly
- mixes DUT port orchestration with environment assembly

Supporting logic is already reasonably separated into:

- `env/agents/`
- `env/monitors/`
- `env/model/`
- `env/sequences/`

So the main problem is not business-logic entanglement across the whole tree;
it is that the DUT-facing orchestration layer is still largely hand-wired.

## Reference Pattern

The in-repository reference for Toffee style is `src/test/python/MemBlock/`.
That codebase already uses Toffee bundle-centric DUT interface definitions and
lets the environment assemble behavior around explicit interface objects rather
than around raw signal-name access scattered across the env.

The Frontend refactor should follow that direction, but it should not blindly
copy MemBlock's file sizes or flatten Frontend's existing internal layering.

## Recommended Approach

Implement a phased Toffee refactor that introduces explicit DUT interface
bundles first, then migrates Frontend's environment assembly to consume those
bundles, while preserving the current external API and pytest fixtures.

This gives the project the architectural benefits of Toffee without forcing a
simultaneous rewrite of:

- API entry points
- pytest usage patterns
- model logic
- sequence logic
- webui behavior

## Rejected Alternatives

1. Full one-shot rewrite of Frontend into a completely new Toffee-native test
   stack.
   - Rejected because the regression surface is too wide and conflicts with the
     phased requirement.
2. Minimal DUT wrapper only, leaving `FrontendEnv` as mostly hand-wired raw
   signal orchestration.
   - Rejected because it would use Toffee superficially without materially
     improving the environment structure.
3. Copy MemBlock's organization directly into Frontend, even where Frontend
   already has clearer subdirectories.
   - Rejected because it would discard useful existing Frontend layering and
     create unnecessary churn.

## Phase 1 Scope

Phase 1 changes only the DUT-facing orchestration layer.

It includes:

- introducing Toffee bundle definitions for major Frontend DUT interfaces
- changing `FrontendEnv` from a raw-DUT orchestrator into an assembly layer
  over bundles, agents, monitor, and model objects
- updating the main Frontend agents and monitor entry points so they can work
  from bundles instead of directly binding a raw DUT where appropriate
- preserving the current `api_Frontend_*` entry points and pytest fixture
  names

It does not include:

- rewriting all tests into a new Toffee usage style
- rewriting all models into Toffee-specific abstractions
- redesigning webui
- changing user-facing command lines
- broad business-logic cleanup unrelated to the interface-layer refactor

## Target Internal Structure

Phase 1 should introduce a new `env/bundles/` package under
`src/test/python/Frontend/env/`.

Planned internal responsibilities:

- `env/bundles/`
  - Toffee `Bundle` definitions for grouped DUT interfaces
- `env/agents/`
  - Frontend stimulus/response drivers that consume bundles instead of relying
    on ad hoc raw-DUT signal lookup
- `env/monitors/`
  - observation logic that reads from explicit bundles where practical
- `env/model/`
  - pure Python behavior/state logic, largely unchanged in phase 1
- `env/frontend_env.py`
  - top-level assembly that creates bundles, agents, models, and monitor,
    wires them together, and preserves the current external API

## Initial Bundle Set

Phase 1 should start with the smallest useful set of bundles that covers the
current manual orchestration hot spots in `FrontendEnv`.

Recommended bundle groups:

- `ClockResetBundle`
  - `clock`, `reset`, `io_reset_vector_addr`, `io_fencei`
- `ICacheBundle`
  - ICache request/response-facing DUT signals currently managed by the env and
    ICache agent
- `UncacheBundle`
  - uncache request/response-facing DUT signals
- `PTWBundle`
  - PTW response plus SFence-facing DUT signals
- `BackendCtrlBundle`
  - backend-facing accept/commit/redirect/resolve/call-ret control signals
- `CSRControlBundle`
  - TLB/CSR/BPU control inputs initialized by the env

This set is intentionally limited. It is enough to remove the largest block of
stringly-typed DUT signal handling from `FrontendEnv` without forcing a full
signal taxonomy redesign in phase 1.

## FrontendEnv Refactor Direction

After phase 1, `FrontendEnv` should remain the canonical external environment
object, but its role should narrow.

It should:

- own the created bundles
- create and configure agents, monitor, and models
- connect collaborators
- expose the same high-level control helpers (`initialize`, `reset`, `step`,
  waveform helpers, trace/program loading helpers)

It should no longer be the primary place where dozens of DUT signals are
manually named and initialized one-by-one through generic raw-DUT helper logic.

## Agent and Monitor Migration Strategy

Phase 1 should migrate the Frontend environment consumers incrementally.

Priority order:

1. `ICacheAgent`
2. `PTWAgent`
3. `UncacheAgent`
4. `FrontendMonitor`
5. backend-facing DUT access points used by `BackendModel`

Migration rule:

- prefer changing the binding surface first
- keep internal behavior logic stable unless the new interface exposes a real
  bug
- allow small compatibility adapters during the phase, but do not keep long-
  term duplicate orchestration paths once the new path is proven

## Fixture and API Stability

The following external surfaces should continue to work during phase 1:

- `env/fixtures.py` fixture names:
  - `dut`
  - `env`
  - `full_env`
- `Frontend_api.py`
- `Frontend_env.py`
- existing `api_Frontend_*` functions
- current `src/test/python/Frontend/tests/` invocation style

This stability requirement is the key reason for using a phased refactor rather
than a clean-slate rewrite.

## Testing Strategy

Phase 1 verification should focus on structural safety and regression control.

Required verification classes:

1. Import and fixture compatibility
   - existing env/api fixture paths still import and initialize correctly
2. Assembly regression
   - new bundle-backed `FrontendEnv` still wires collaborators correctly
3. Agent/monitor unit stability
   - existing agent and monitor unit tests continue to pass
4. Focused DUT-backed smoke coverage
   - at least one real DUT-backed test still runs successfully
5. Waveform generation regression
   - a DUT-backed test still emits `.fst` output when waveform dumping is
     enabled

## Risks and Mitigations

- Risk: Interface churn leaks into tests and API unexpectedly
  - Mitigation: preserve fixtures and API names in phase 1; add explicit import
    and fixture regression tests
- Risk: Bundle boundaries are chosen poorly and simply move complexity around
  - Mitigation: start with a small bundle set aligned to existing agents and the
    heaviest manual signal groups
- Risk: Simultaneous migration of all Frontend collaborators creates a wide
  regression surface
  - Mitigation: migrate binding surfaces incrementally in a fixed priority order
- Risk: Toffee adoption becomes superficial and leaves `FrontendEnv` still
  acting as a raw signal registry
  - Mitigation: phase 1 must remove the main manual orchestration hot spots
    from `FrontendEnv`, not merely wrap them cosmetically

## Acceptance Criteria

Phase 1 is complete when all of the following are true:

1. `src/test/python/Frontend/env/bundles/` exists and is the canonical home for
   Frontend DUT interface bundles.
2. `FrontendEnv` assembles the environment around explicit bundles rather than
   primarily around ad hoc raw signal-name access.
3. The core Frontend DUT-facing agents/monitor use the new bundle interfaces at
   least for their primary binding surface.
4. Existing pytest fixture names and top-level Frontend API entry points still
   work.
5. Existing focused Frontend unit tests still pass.
6. At least one DUT-backed Frontend test still passes using the refactored env.
7. Waveform dumping still works for a DUT-backed test.

## Implementation Handoff

This design should be followed by a written implementation plan document in
`docs/superpowers/plans/` before code changes begin.
