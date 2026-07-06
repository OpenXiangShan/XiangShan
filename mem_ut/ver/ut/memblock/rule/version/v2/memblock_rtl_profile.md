# V2 memblock RTL profile

## Version Facts

| Item | V2 value |
|---|---|
| Build system | `build.sc` |
| Mill module | `xiangshan` |
| Chisel version | `6.7.0` |
| firtool source | `build.sc` firtool resolver via `chisel3.BuildInfo.firtoolVersion` |
| Default top entry found in V2 | `top.TopMain` |
| V2 config found in source | `KunminghuV2Config` |
| Existing MemBlock class | `xiangshan.mem.MemBlock` |
| Existing standalone MemBlock main | Not present before this migration |

## RTL Generation Direction

V2 must not reuse the V3-only assumptions directly:

```text
top.MemBlockTopMain
TLConfig
Chisel 7.3.0
firtool 1.135.0 as a hard requirement
build.mill
```

The V2 RTL flow should use `build.sc`, Chisel 6.7.0 and a V2-specific MemBlock
harness/main. The default config is `KunminghuV2Config` unless later V2
verification proves a smaller config is required.

## Expected Output

The mem_ut environment still expects:

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

If V2 generation produces different equivalent names, the script and
`mem_ut/ver/ut/memblock/cfg/rtl.f` must be updated in the same change and this
profile must record the mapping.

## UVM RTL Path

V2 uses an independent worktree, so `mem_ut/ver/ut/memblock/cfg/rtl.f` must not
hardcode `$MEMBLOCK_PROJECT/XiangShan/...`.

The current V2 rule is:

```text
-F $MEMBLOCK_XS_HOME/build_memblock/rtl/filelist.f
```

`scripts/generate_memblock_rtl.sh` and `mem_ut/ver/ut/memblock/sim/eda01_entry.sh`
export `MEMBLOCK_XS_HOME` to the current V2 XiangShan worktree root by default.

## Current Status

V2 RTL generation has passed in this worktree after submodule repair and
standalone `MemBlockTop` wrapper migration. Generation result and historical
failure details are recorded in:

```text
AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md
```
