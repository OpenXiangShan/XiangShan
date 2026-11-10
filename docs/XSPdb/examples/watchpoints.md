# Watchpoints & Visibility Examples

```text
xwatch SimTop_top.SimTop.timer
xwatch_commit_pc
xstep 1000
xunwatch SimTop_top.SimTop.timer
xunwatch_commit_pc

# Inspect or modify signals/registers
xprint SimTop_top.SimTop.timer
xset SimTop_top.SimTop.timer 123
xpc
```
