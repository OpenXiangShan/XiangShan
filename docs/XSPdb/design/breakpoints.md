# Breakpoints and Triggers (Design)

## Purpose
Stop or trigger actions based on signal values, expressions, or FSM-style multi-step conditions.

## Notes
- `xbreak` registers signal breakpoints backed by `xclock` callbacks.
- `xbreak_expr` supports boolean expressions with `within()` and `hold()` time helpers.
- `xbreak_fsm` loads a trigger program for multi-step conditions.
- Detailed specs are in `trigger_expr_spec.md` and `trigger_fsm_spec.md`.

## Related commands
- `xbreak`, `xbreak_expr`, `xbreak_fsm`
- `xbreak_list`, `xbreak_clear`, `xbreak_update`
