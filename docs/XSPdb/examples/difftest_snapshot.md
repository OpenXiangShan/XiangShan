# Difftest & Snapshot Examples

```text
# Export difftest state
xexpdiffstate
```

Snapshot is configured in the simulator layer (see `difftest/README.md`). Example flow:

```text
# Enable snapshot in build/config, then run with a fork interval
python3 scripts/emu.py --fork-interval 10 --image /path/to/bin
```
