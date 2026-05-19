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
- `fe_instr_uncache_boundary_mix.S`: consolidated NEMU-runnable InstrUncache boundary stream.
- `fe_mmio_nonmmio_toggle.S`: starts in MMIO, jumps to `0x80000000`, then alternates
  between MMIO and non-MMIO sections. Requires explicit section placement for
  `.mmio_text` and `.normal_text`; the generated runnable bin is a large logical-size
  sparse image because it spans `0x10001000` to `0x80000000`.
- `fe_mmio_commit_gate_burst.S`: consecutive MMIO straight-line fetches before and
  after a non-MMIO phase, targeting MMIO commit gating and stale attribute risks.
- `fe_mmio_normal_redirect_stress.S`: MMIO/non-MMIO switches with backend-corrected
  JALR redirects and a normal-region taken branch around the switching points.
- `fe_history_direction_00.S`: controlled history before a target branch.
- `fe_target_alias_replace.S`: repeated aligned direct-target pressure.

Generated cases:

- `generate_cases.py`: deterministic offline generator.  Each generated `.S`
  records its seed, parameters, and branch target map in the file header.
- `generated/`: output directory for generated assembly cases.
