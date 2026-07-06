# V3 memblock RTL profile

## Version Facts

| Item | V3 value |
|---|---|
| Build system | `build.mill` |
| Chisel version | `7.3.0` |
| Verified firtool | `/nfs/home/lixiangrui/.cache/llvm-firtool/1.135.0/bin/firtool` |
| Main class | `top.MemBlockTopMain` |
| Default config | `TLConfig` |
| Default issue | `E.b` |
| Default output | `build_memblock/rtl` |

## Expected Output

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/ClockGate.sv
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

The V3 script may patch `filelist.f` to include `ClockGate.sv` before
`MbistClockGateCell.sv` when needed.

## Known Failure Pattern

Do not use the shared old firtool:

```text
/nfs/home/share/firtool-1.56.1/bin/firtool
```

It is too old for Chisel 7.3 generated FIRRTL and can fail on verification layer
syntax.
