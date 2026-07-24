# Frontend Assembly Cases

These `.S` files are source-level frontend instruction streams for frontend
PC/CFI testcase design. They are kept separate from pytest regressions so each
case can later be compiled into an ELF/bin and paired with a golden trace by
the frontend bin-trace flow.

Current cases:

- `fe_instr_uncache_boundary_mix.S`: consolidated NEMU-runnable InstrUncache boundary stream.
- `fe_2fetch_rvi_cross_block.S`: repeated 32-bit instructions whose low and
  high halfwords straddle 32-byte-aligned FTQ blocks, with one site pinned to
  the DefaultConfig 64-byte fetch-span boundary; assembly-time assertions keep
  every stitched RVI at alignment offset 30.

Generated cases:

- `generate_cases.py`: deterministic offline generator.  Each generated `.S`
  records its seed, parameters, and branch target map in the file header.
- `generated/`: output directory for generated assembly cases.
