# Frontend Assembly Cases

These `.S` files are source-level frontend instruction streams for frontend
PC/CFI testcase design. They are kept separate from pytest regressions so each
case can later be compiled into an ELF/bin and paired with a golden trace by
the frontend bin-trace flow.

Current cases:

- `fe_branch_sanity.S`: basic BRANCH/JAL/JALR/CALL/RET coverage.
- `fe_cfi_slot_3br.S`: three branch CFIs in one 32B fetch block.
- `fe_redirect_branch_flip.S`: trained-taken branch followed by not-taken exercise path.
- `fe_return_nested_4.S`: nested real call/ret sequence.
- `fe_fetch_boundary_rvc.S`: RVC padding near a 32B fetch boundary.
- `fe_mmio_rvc_at_0x10002ffe.S`: MMIO entry at 0x10001000 jumping to RVC at 0x10002ffe.
- `fe_instr_uncache_boundaries.S`: NEMU-runnable InstrUncache bus/page boundary stream.
- `fe_instr_uncache_bus_boundary.S`: NEMU-runnable InstrUncache bus-boundary stream without xffe fetch.
- `fe_instr_uncache_rvc_bus_tail.S`: RVC at the last halfword of an uncache bus beat.
- `fe_instr_uncache_page_end_rvi_safe.S`: 32-bit instruction ending at a page boundary without xffe fetch.
- `fe_instr_uncache_sequential_mmio.S`: sequential mixed-width MMIO uncache stream.
- `fe_instr_uncache_offset_matrix.S`: RVC/RVI at multiple offsets within an 8B uncache beat.
- `fe_instr_uncache_fetchblock_boundary.S`: RVC at the last halfword of a 32B frontend fetch block.
- `fe_instr_uncache_boundary_mix.S`: large mixed InstrUncache directed boundary stream.
- `fe_history_direction_00.S`: controlled history before a target branch.
- `fe_target_alias_replace.S`: repeated aligned direct-target pressure.

Generated cases:

- `generate_cases.py`: deterministic offline generator.  Each generated `.S`
  records its seed, parameters, and branch target map in the file header.
- `generated/`: output directory for generated assembly cases.
