# Breakpoints & Trigger Examples

```text
# Signal breakpoint
xbreak SimTop_top.SimTop.timer eq 10000
xstep 100000

# Expression trigger
xbreak_expr within(SimTop_top.SimTop.timer == 1000, 20)

# FSM trigger
xbreak_fsm /abs/path/to/docs/XSPdb/examples/pc_sequence.fsm
```

More expression examples:

- xbreak_expr_examples.md
