# Frontend Assembly Cases

These `.S` files are source-level frontend instruction streams for frontend
PC/CFI testcase design. They are kept separate from pytest regressions so each
case can later be compiled into an ELF/bin and paired with a golden trace by
the frontend bin-trace flow.

Current bin-trace suite:

`scripts/run_baremode_asm_suite.sh` runs every tracked `.S` below by default
(19 cases). All cases must compile to a non-empty bin, generate a non-empty
NEMU trace, and complete the DUT checker. The nine cases already named by the
canonical functional-coverage registry use directed coverage targets; the
other ten remain observed-only until their registry mappings are added.
There is no hidden active-list exclusion: when a tracked assembly case fails,
first diagnose and repair it so the default suite remains complete. Remove the
source from `asm_cases/` only when the case is no longer a maintained
regression.

- `jiabowen/fe_2fetch_cross_page_blocked.S`
- `jiabowen/fe_2fetch_mixed_rvc_rvi.S`
- `jiabowen/fe_2fetch_rvi_cross_block.S`: includes RVI stitching across a
  32-byte FTQ boundary, with a site at DefaultConfig's 64-byte fetch span.
- `jiabowen/fe_2fetch_size_blocked.S`
- `jiabowen/fe_2fetch_trained_short_blocks.S`
- `jiabowen/fe_ifu_cfi_decode_basic.S`
- `jiabowen/fe_ifu_mixed_rvc_rvi_boundary.S`
- `jiabowen/fe_ifu_rvc_seq_boundary.S`
- `jiabowen/fe_ifu_rvi_seq_boundary.S`
- `jiabowen/fe_baremode_cond_nt.S`
- `jiabowen/fe_baremode_direct_jmp.S`
- `jiabowen/fe_baremode_seq_icache_basic.S`
- `jiabowen/fe_jal_forward_jump_observes_target_pc.S`
- `jiabowen/fe_jal_resolve_drains_pending_queue.S`
- `jiabowen/fe_large_loop_multi_segment.S`
- `jiabowen/fe_multi_branch_dense_loop.S`
- `jiabowen/fe_multi_branch_random_positions.S`
- `jiabowen/fe_multi_cfi_per_ftq_entry.S`
- `zhaoxinran/fe_instr_uncache_boundary_mix.S`: consolidated NEMU-runnable
  InstrUncache boundary stream.

Generated cases:

- `generate_cases.py`: deterministic offline generator.  Each generated `.S`
  records its seed, parameters, and branch target map in the file header.
- `generated/`: output directory for generated assembly cases.
