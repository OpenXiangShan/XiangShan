# Trigger FSM Spec (v1)

## Scope

This spec defines the software FSM trigger engine and the `xbreak_fsm` command.
The FSM runs in C++ on every clock edge and uses the same expression engine as
`xbreak_expr` for its transition conditions (including `within/hold`).

## Goals

- Provide a state-machine trigger language for complex, multi-step sequences.
- No fixed limit on the number of states/flags/counters (software only).
- Use the expression engine for transition conditions.
- Support time-window helpers (`within`, `hold`) in transition conditions.
- Disable the clock when a trigger fires (same behavior as `xbreak`).

## Non-Goals (v1)

- X/Z propagation in arithmetic/bitwise operations.
- Hardware-like resource limits (ILA style counters/flags limits).
- Parallel FSM execution within a single instance (one active state at a time).

## User-Facing Commands

- `xbreak_fsm <fsm_file>`
  - Load an FSM program from a text file and arm it.
- `xbreak_fsm_status`
  - Show the current state and trigger status.
- `xbreak_fsm_clear`
  - Remove the FSM trigger and its callback.

## FSM Program Syntax

### Overview

- One program contains multiple `state` blocks.
- `start <state>` optionally selects the initial state (default: first state).
- `#` starts a comment (to end-of-line).
- Each non-empty line is one statement. Trailing `;` is optional.

### State Definition

```
state S0:
  if <expr> goto S1
  elif <expr> goto S2
  else goto S0
```

### Transitions

- `if <expr> goto <state>`
- `if <expr> trigger`
- `elif <expr> goto <state>`
- `else goto <state>`
- `else trigger`

Transitions are evaluated in order; the first matching condition is taken. If
no transition matches and no `else` exists, the FSM stays in the current state.

Unconditional transitions are also accepted:

- `goto <state>`
- `trigger`

### Actions

Actions run on every cycle while the state is active, before transition checks.

```
set $flag_done
clear $flag_done
inc $counter_timeout
reset $counter_timeout
```

- Flags are 1-bit, counters are 64-bit.
- Names must start with `$flag` or `$counter`.
- Values are updated immediately and visible to conditions in the same cycle.

### Conditions

Conditions use the expression syntax described in `docs/trigger_expr_spec.md`:

- Boolean logic: `and/or/not` or `&&/||/!`
- Bitwise/arithmetic/comparison operators
- `within(<cycles>, <expr>)`, `hold(<cycles>, <expr>)`
- Signal names via `XSignalCFG`
- `$flag*` and `$counter*` are valid identifiers inside expressions.

### Example

```
# Wait for A, then within 200 cycles see B for 4 consecutive cycles
start IDLE

state IDLE:
  reset $counter_timeout
  if sigA == 1 goto WAIT_B
  else goto IDLE

state WAIT_B:
  inc $counter_timeout
  if hold(4, sigB == 1) trigger
  elif $counter_timeout >= 200 goto IDLE
  else goto WAIT_B
```

## Semantics

- Actions execute first, then transitions are evaluated in order.
- The FSM keeps a single active state.
- `trigger` disables all bound clocks and records the triggered state.
- `within/hold` are stateful per node (see expression spec for details).
- Flags/counters are reset to 0 on program load and `xbreak_fsm_clear`.

## C++ Engine Design

- `ComUseFsmTrigger` subclasses `ComUseStepCb` and registers to `StepRis`.
- It owns:
  - `ExprEngine` for compiling transition conditions.
  - State table (`vector<FsmState>` with ordered transitions/actions).
  - Flag/counter storage and `XData` mirrors for expression access.
- On each cycle:
  1) Update engine cycle (`SetCycle`).
  2) Run current state actions (update flags/counters).
  3) Evaluate transitions; take first match or default.
  4) If `trigger`, disable clocks and mark triggered.

## Diagnostics

- Parse errors report the line number and reason.
- Unknown state targets produce a compile error.
- Using `set/clear` on a counter or `inc/reset` on a flag is rejected.
