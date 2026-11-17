# Four-family program exploration

The four families share one offline measure of *executed program space*.
`program_exploration_audit.py` reads existing committed traces and linked
ProgramMaps. No DUT, NEMU, assembler, generator, replay or corpus writes occur.
It cannot qualify a case or transfer historical runtime evidence to a new build.

## What grows

At explicit instruction-count prefixes it records unique PCs, actual CFI
successor edges, CFI-layout keys and their consecutive order, short branch
successor histories, indirect site/target pairs, program-page transitions and
call-hint stack observations. A prefix never uses the next unconsumed instruction.

A branch is labeled by its actual successor (`fallthrough_successor` or
`nonfallthrough_successor`), not declared `taken` or target metadata. Branch
history keys include the branch PC and the preceding bounded global relation
history. This is a program stimulus descriptor, not the hardware predictor GHR.
CFI-layout keys include kind, width, source offsets32/64 and target page relation.
Changing their order can matter even if final unique-PC/edge totals are unchanged.

Call/return hints use disassembly conventions and observed successors. A
return-like computed x5 jump may not match a preceding call. An unmatched jump
does not pop a known matching context. These diagnostics do not reconstruct
hardware RAS occupancy, speculation, exceptions or coroutine state.

Repeated execution can remain valuable for training and pressure. A flat curve
only says these particular descriptors stopped growing during that prefix. It
neither proves functional saturation nor ranks bug-finding effectiveness.

## Explicit existing inputs

Supply a SHA256-pinned JSON index with `schema_version: 1`, `case_name`, one of
the four `family` names, exact integer `program_seed`/`timing_seed`, and `inputs`.
The five `inputs` keys are `source`, `bin`, `trace`, `program_map`, `scenario_spec`;
each value contains `path` and `sha256`. Relative paths resolve against the index.
The index is an offline reference to retained originals, not a replacement campaign.
No automatic input discovery, inferred seed, rebuilt artifact or corpus membership.

ProgramMap must match source/binary hashes and actual binary text/instruction
bytes. Scenario identity/seeds must match the index. Trace indices must start at
zero and be contiguous; instruction PC/width/bytes must match the actual program.
Missing PCs, stale inputs and budgets beyond the retained trace fail the audit.
Hashes are checked again after reading. Only a fresh report can be created.

From Frontend, using existing retained input files:

```bash
python tests/random_case/program_exploration_audit.py \
  --input-index /absolute/existing-input-index.json \
  --input-index-sha256 REVIEWED_INDEX_SHA256 \
  --instruction-budget 1000 --instruction-budget 2000 \
  --history-bits 4 \
  --output /absolute/new-program-exploration-report.json
```

Only use prefix budgets that fit the full retained trace. Instruction count is
not simulation cycles or wall time. Functional bin growth, same-edge RTL
interactions and runtime source/build qualification remain unmeasured here.

## Family-specific construction priorities

| Family | Existing constraint | Next construction direction | Required separate proof |
| --- | --- | --- | --- |
| CFG | Seeded layout, loop-counter-masked branches, optional history4/page and C.J order | Independent bounded branch outcome schedules and useful target/layout histories | Actual executed histories first; predictor/VBTB occupancy/replacement from reviewed native evidence |
| RAS | Depth-bounded repeated chains, x1/x5 links, fixed indirect destinations | Shared callee with multiple legal return contexts and bounded call graph | Real return targets, stack/termination; speculative versus committed RAS and recovery |
| IBuffer | Dense bursts; queued-redirect pressure, currently4–8 long1200–1500cycle windows | Evidence-driven bounded shorter/event-adaptive pressure policy | Fullness, payload stability, flush and recovery at correct RTL sampling phase |
| Translation | CFG programs over executable Sv39 stage1 pages with PMA/PBMT/transport variation | Bounded nofault page layout/attribute plans; explicit capability metadata | Actual translated paths, PTW/oracle/context semantics and compatible runtime |

The public translation randomizer has stage2/superpage/sector names, but the
long-program wrapper explicitly normalizes those to Sv39 stage1 level0 nofault
and removes sector/permission probes. Ordinal/risk labels alone must not claim
these dimensions. Context/sfence *observation* also does not implement a runtime
mapping-change experiment. Fault/recovery needs its own supported checker model.

Default generator/source/seed behavior is unchanged by this evaluator. Future
construction changes must be explicit opt-ins, preserve old identities and keep
all full-program checker/monitor/oracle/termination obligations. No canonical or
ctrl sampler contract is changed here.
