# Trigger Expression Engine Spec (v1)

## Scope

This spec defines the v1 trigger expression engine and the `xbreak_expr`
command. The engine parses expressions and evaluates expression trees in C++
on every clock edge to avoid Python-side overhead. The FSM trigger engine
reuses this expression engine (see `docs/trigger_fsm_spec.md`).

## Goals

- Provide `xbreak_expr "<expr>"` with Python-style expression syntax.
- Parse and compile expressions in C++ using `XSignalCFG`.
- Evaluate expressions in C++ on every clock edge (fast path).
- Avoid 64-bit truncation for comparisons by using `XData` where possible.
- Disable wide (>64-bit) arithmetic/bitwise operations rather than silently
  truncating.
- Ignore X/Z propagation in arithmetic/bitwise/shift operations.
- Support time-window helpers for stateful triggers (`within`, `hold`).

## Non-Goals (v1)

- Full wide arithmetic/bitwise/shift for signals wider than 64 bits.
- X/Z propagation in arithmetic/bitwise/shift operations.
- FSM/sequence trigger language and loader (planned later).

## User-Facing Commands

- `xbreak_expr "<expr>"`
  - Compile an expression and arm it.
- `xunbreak_expr <key>`
  - Remove a compiled expression by key.
- `xbreak_expr_list`
  - List compiled expressions and their trigger status.

Key format (v1):
- Auto-generated key: `xexpr-<id>` where `<id>` is a monotonically increasing
  integer in the current session.

## Expression Syntax (v1)

Python-style expressions:

- Boolean logic: `and`, `or`, `not` (also `&&`, `||`, `!`)
- Bitwise: `& | ^ ~ << >>`
- Arithmetic: `+ - * / %`
- Comparisons: `== != > >= < <=`
- Parentheses for grouping
- Integer literals: decimal, `0x` hex, `0b` binary
- Signal names: `foo.bar.baz` (resolved via `XSignalCFG`)
- Time-window helpers:
  - `within(<cycles>, <expr>)`
  - `hold(<cycles>, <expr>)`

Example:

```
(sigA == 1 and sigB != 0) or ((sigC & 0x3) == 2)
```

## Semantics

- All arithmetic/bitwise/shift operations are evaluated as unsigned 64-bit.
- For comparisons:
  - If either operand is a signal wider than 64 bits, use `XData` full-width
    comparison and require both operands to have the same width.
  - In that case, the other operand must be a signal or a literal constant
    (no computed expressions), because wide arithmetic is disabled in v1.
  - If both operands are <=64-bit or constants, use unsigned 64-bit comparison.
- `and`/`or` short-circuit evaluation.
- `not` returns `1` if the operand is zero, else `0`.
- Negative constants are allowed; they are stored as unsigned 64-bit two's
  complement values.
- X/Z values are not propagated through arithmetic/bitwise/shift operations.
  Comparisons use `XData` semantics (X/Z may make equality false).
- `within(N, expr)` returns true if `expr` is true now, or was true within the
  last `N` cycles (inclusive). `within(0, expr)` is equivalent to `expr`.
- `hold(N, expr)` returns true if `expr` has been true for at least `N`
  consecutive cycles. `hold(0, expr)` and `hold(1, expr)` are equivalent to
  `expr`.
- `within/hold` are stateful: they update internal state only when evaluated.
  If they are placed under a short-circuit branch that is skipped, their state
  is not updated for that cycle.

## Width Rules and Validation

At compile time (C++ side):

- If an arithmetic/bitwise/shift operator is applied to a signal wider than
  64 bits, compilation fails with a clear error:
  `wide arithmetic/bitwise/shift is not supported in v1`.
- Comparisons with wide signals are allowed, but operands must be the same
  width (signal vs signal, or signal vs constant with width matching signal).

## C++ Engine Design

### Data Types

```
enum class ExprOp {
  CONST, SIGNAL,
  ADD, SUB, MUL, DIV, MOD,
  BAND, BOR, BXOR, BNOT,
  SHL, SHR,
  LAND, LOR, LNOT,
  EQ, NE, GT, GE, LT, LE,
  WITHIN, HOLD
};

struct ExprNode {
  ExprOp op;
  int lhs;       // child node id, -1 if unused
  int rhs;       // child node id, -1 if unused
  XData* sig;    // SIGNAL node
  uint64_t imm;  // CONST value
  uint32_t width; // signal width if SIGNAL, else 64
  bool is_signal;
  uint64_t window;          // for WITHIN/HOLD
  uint64_t last_true_cycle; // for WITHIN
  uint64_t last_hold_cycle; // for HOLD
  uint64_t hold_count;      // for HOLD
};
```

### ExprEngine API

```
class ExprEngine {
 public:
  int NewConst(uint64_t v);
  int NewSignal(XData* sig);
  int NewUnary(ExprOp op, int child);
  int NewBinary(ExprOp op, int lhs, int rhs);
  int NewCompare(ExprOp op, int lhs, int rhs);
  int NewCompareSigSig(ExprOp op, XData* lhs, XData* rhs);
  int NewCompareSigConst(ExprOp op, XData* lhs, uint64_t rhs);
  int NewCompareConstSig(ExprOp op, uint64_t lhs, XData* rhs);
  int NewWithin(int child, uint64_t window);
  int NewHold(int child, uint64_t window);
  uint64_t Eval(int root);
  void Clear();
};
```

### Value Handling

Evaluation returns `uint64_t` only. For comparisons that require full-width
signal semantics, the compare path uses `XData` directly instead of the numeric
value:

- If either child is a SIGNAL node with `width > 64`, compare via:
  - `lhs_sig.Comp(rhs_sig, opcode, eq)` when both are signals (widths must
    match).
  - `lhs_sig.Comp(const_xdata, opcode, eq)` when comparing to a constant.
- To compare a wide signal to a constant, create a temporary `XData` constant
  at compile time (owned by the engine) with matching width and fixed value.
  Constants are zero-extended to the signal width and truncated to that width
  if needed (two's complement for negative values).

### Trigger Evaluation

`ComUseExprCheck` is invoked on every clock edge (`StepRis`). It:

1) Clears previous trigger flags.
2) Evaluates expressions in registration order.
3) On the first true result:
   - Disables all bound clocks (`clk->Disable()`).
   - Marks the expression as triggered.
   - Stops evaluating remaining expressions.

Before evaluation, `ComUseExprCheck` sets the engine's current cycle to the
callback cycle (`ComUseStepCb::cycle`) so `within/hold` can measure time
windows in cycles.

### Trigger Tree Evaluation Order

Expressions are stored as node trees in a flat vector. Evaluation is
depth-first with short-circuit for boolean `and/or`. No memoization is
required in v1; if a subexpression appears multiple times, the compiler may
choose to share a node id (DAG) or duplicate it.

The engine may reorder `and/or` operands by estimated cost to improve
short-circuit efficiency. Reordering is skipped when a subtree contains
stateful `within/hold` nodes to preserve their update semantics. Evaluation is
implemented with an explicit stack machine (iterative) instead of recursion.

## C++ Parser (v1)

Parsing and compilation are done in C++ in v1. Python only forwards the
expression string and the DUT's `XSignalCFG` instance.

### Tokenization

- Identifiers: `[A-Za-z_][A-Za-z0-9_\.]*`
- Numbers:
  - `0x` hex, `0b` binary, or decimal
  - `_` separators are allowed and ignored
- Operators:
  - `== != >= <= << >> && ||`
  - `+ - * / % & | ^ ~ ! < >`
  - Parentheses `(` `)`
- Keywords: `and`, `or`, `not` (synonyms for `&&`, `||`, `!`)

### Precedence (high -> low)

1) Unary: `~`, `-`, `+`
2) Multiplicative: `* / %`
3) Additive: `+ -`
4) Shift: `<< >>`
5) Bitwise AND/XOR/OR: `& ^ |`
6) Comparisons: `== != > >= < <=` (supports chaining)
7) Logical NOT: `not` / `!`
8) Logical AND: `and` / `&&`
9) Logical OR: `or` / `||`

`within/hold` are parsed as function calls and can appear anywhere a primary
expression is allowed.

### Comparison Chaining

`a < b < c` is compiled as `(a < b) and (b < c)`.

### Signal Resolution

`XSignalCFG::NewXData(name)` is used to resolve signals. The engine caches
`XData` objects by name to avoid repeated allocation.

## API Surface (Python)

```
api_xbreak_expr(expr: str, name: str = "") -> str
api_xunbreak_expr(key: str) -> None
api_xbreak_expr_list() -> list[tuple]
```

Internally, `api_xbreak_expr` calls `ComUseExprCheck::CompileExpr(expr, cfg)`
and then registers the resulting root id.

`xbreak_expr` returns the generated key (e.g., `xexpr-3`).

## Diagnostics

- On compile error, print the reason and do not register the expression.
- On trigger, behave like existing xbreak (stop clock, report in console).

## Future Extensions (Not in v1)

- Wide arithmetic/bitwise/shift operations.
- X/Z propagation rules.
- More advanced time-window operators and stateful combinators.
