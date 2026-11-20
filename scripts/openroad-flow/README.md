# XSTile through an open-source physical flow (experimental)

> **Status: not yet run end to end in this tree.** The flow here is ported
> from [bazel-orfs][bo], where it was built and measured. In this tree,
> loading and analysis of every target pass and the minimal configuration's
> Verilog builds; nothing else has been run yet. The
> [reference results](#reference-results) are bazel-orfs's.

This directory generates XiangShan's Verilog from this checkout and takes
XSTile (the core, its L1 caches and the L2) through synthesis, floorplan,
placement, clock tree synthesis and global route on the ASAP7 predictive
PDK with yosys and OpenROAD, and opens the result in OpenROAD's web
viewer. Nothing outside this directory changes: no RTL, no `build.mill`,
no `Makefile`.

**The terms.** This is an experimental flow:

- it is not supported and not in CI;
- issues and pull requests that fix it are welcome, but nobody is promised
  anything;
- it is stated plainly as experimental, so that nobody expects more of
  it than it gives.

**What it is not.** No substitute for a commercial flow, and not
comparable with XiangShan's own 7 nm results: a predictive PDK, stopped
at global route (no detailed route), a reduced configuration (integer-only
core, 512 KB L2), the register files generated rather than synthesised,
and structural patches to the RTL carried for synthesis runtime
([below](#xiangshan-through-this-flow)). What it offers is something
anyone can reproduce, open and ask questions of.

**Why Bazel.** Bazel is a convenient way to express and install every
dependency of the flow -- JDK, Scala, Chisel, firtool, espresso, yosys,
OpenROAD, ORFS and the PDK -- with one tool, and it is what OpenROAD and
bazel-orfs use. mill remains XiangShan's build; this directory does not
replace or change it.

The flow's rules, its scheduler, its debugging tools and most of what is
known about running it live in bazel-orfs, which this directory depends on
at commit [`e2f12648`][pin] (`MODULE.bazel`). Links into bazel-orfs below
are pinned to that commit.

## Machine requirements

Only [bazelisk](https://github.com/bazelbuild/bazelisk) is needed on the
host, and a checkout with its submodules (`make init`). Everything else is
downloaded and built: OpenROAD and yosys from source take 30 to 60 minutes
the first time, and some tens of GB of disk in Bazel's cache.

| What | Machine | Time, first build |
|---|---|---|
| Loading and analysis of every target | any | minutes (downloads OpenROAD's sources) |
| The minimal configuration's Verilog | 16 GB (not yet measured uncached) | 8.5 min here with most actions cached; uncached, compiling XiangShan's Scala dominates (not yet measured) |
| The flow's Verilog (FlowConfig) | 32 GB (the generator's JVM is capped at 24 GB) | not yet measured here |
| One block, FltRegionModule, to global route | 16 cores, 32 GB | about 2 h from nothing |
| The whole tile: five blocks, then the parent | 128 GB | days |

One parent build at a time: the parent's global route peaks at 53 to
85 GB. The scheduler (`//xstile:build`) packs the blocks into waves that
fit free memory and runs the parent alone.

## Run book

Run every command from this directory (`scripts/openroad-flow`). Each step
has a prediction (as of bazel-orfs `e2f12648`), a gate that says it
passed, and the bazel-orfs skill or document that owns its failures. The
skills are in bazel-orfs's [`.claude/skills/`][skills]; an agent gets them
locally ([Definition of done](#definition-of-done)).

**0. Sources.** `make init` in the XiangShan checkout. Gate: `rocket-chip`,
`rocket-chip/hardfloat`, `rocket-chip/cde`, `XSCache/OpenNCB` and
`XSCache/ZhuJiang` are populated. Without them the first Bazel command
stops with "run `make init`".

**1. Analysis.** `bazelisk build --nobuild //...` -- minutes the first
time. Gate: `Build completed successfully`. On a failure, the
[`analyze-before-push`][skills] skill.

**2. Verilog, minimal configuration.**
`bazelisk build //verilog:xiangshan_flat_minimal`. Gate: the file exists
and holds `module XSTile` (about 100 MB, 1,852 modules).

**3. Verilog, the flow's configuration, and the surgery.**
`bazelisk build //verilog:xiangshan_flat //xstile/surgery:xiangshan_surgered //xstile/arrays:xiangshan_extracted`
-- 32 GB; its time is not yet measured here. Gate: `bazelisk build //xstile/surgery:xiangshan_surgered_elaborates`
passes (slang elaborates the surgered core under XSTile), and
`bazelisk test $(bazelisk query 'tests(//xstile/arrays/...)')` passes: each
extraction proven equal to its module, each negative control different
(the Rob's proof takes over half an hour). On a surgery precondition
failure, xs_surgery.py names the entry; the [`surgery`][skills] skill.

**4. One block.** `bazelisk build //xstile:FltRegionModule_grt //xstile:FltRegionModule_kpi //xstile:FltRegionModule_gpl_driven`
-- about 2 h from nothing on 16 cores and 32 GB, the block's own global
route about 10 min at 7 GB. Gate: `FltRegionModule_gpl_driven` passes,
and the period in `bazel-bin/xstile/FltRegionModule_kpi.txt` is near the
reference's 1,033 ps. The global route log ending in `RSZ-0062 Unable to
repair all setup violations` is expected: repair gave up, the period is
what is measured. Then `bazelisk run //xstile:FltRegionModule_grt gui_grt`
opens the web viewer.

**5. Every block, then the parent.**

```sh
bazelisk run //xstile:build -- --plan-only   # the waves, nothing built
bazelisk run //xstile:build                  # days on 128 GB
bazelisk run //xstile:XSTile_grt gui_grt     # the web viewer
```

`--plan-only` says which unit is over the free memory. Gate: every
`<part>_gpl_driven` passes in its wave, every `<part>_kpi` is written, and
`resources.json` is refreshed (commit it). On a memory problem, the
[`scale-to-memory`][skills] skill; on a stage that grinds or fails,
[`openroad-field-guide`][skills] first, then [`openroad-debug`][skills];
on a global route repair that runs for hours, [`repair-timing-grt`][skills];
on a rebuild nobody expected, [`cache-miss`][skills]. A stage can be taken
out of Bazel and iterated on by hand in the dirty lane
([`dirty-lane`][skills]):
`bazelisk run @bazel-orfs//:dirty -- start //xstile:FltRegionModule grt`.

**6. Look and ask.** `bazelisk run //xstile:XSTile_grt_odb_debug` starts a
persistent OpenROAD session on the parent's global route: the
[`odb-debug`][skills] skill (macros, worst paths, legality, headless
images). The period's headroom: [`repair-headroom`][skills].

## What days of runs look like

Per stage, minutes and peak GB, from the scheduler's record
(`xstile/resources.json`, measured at bazel-orfs `25f75524`). bazel-orfs's
design README has later numbers for the parent: place up to about four
hours at 47 GB, global route up to 85 GB.

| part | floorplan | place | cts | grt |
|---|---|---|---|---|
| XSTile, the parent | 11, 10 | 163, 31 | 24, 40 | 62, 53 |
| MemBlock | 49, 10 | 100, 27 | 118, 28 | 42, 36 |
| Frontend | 24, 5 | 48, 14 | 24, 22 | 42, 26 |
| VecRegionModule | 10, 4 | 31, 11 | 6, 11 | 17, 15 |
| CoupledL2 | 13, 3 | 15, 7 | 25, 8 | 26, 11 |
| FltRegionModule | 1, 2 | 13, 5 | 1, 5 | 10, 7 |

Add synthesis (minutes per partition, the register files and memories as
macros of their own built in parallel), the abstracts, and the first build
of OpenROAD and yosys. A clean build of every part from scratch is days
on one 128 GB machine.

Signs of a healthy run:

- global placement's log (`3_3_place_gp.log`) shows timing-driven
  iterations (GPL-0100) and routability-mode iterations (GPL-1017); the
  `<part>_gpl_driven` gates check it, because neither driver says so in
  the exit status when it does not run;
- global route ends with the period read by `<part>_kpi`, with
  congestion allowed (see [What is deliberately broken](#what-is-deliberately-broken));
- each stage within about twice its time above. A stage far past that
  is watched, not waited on ([`dogfooding`][skills], rule 11); a global
  route past its estimate is not by itself a hang.

Where runs have ground or died, and what was done about each, is
bazel-orfs's [timing inventory][ideas] (63 entries, each a measurement
and what it implies) and [`openroad-field-guide`][skills]. The parent's
peak memory came down from 196 GB (the whole top in one process,
2026-10-30) to 61 GB (the parent, 2026-11-03) through carried OpenROAD
patches -- the global route backing-file series 0183-0198 among them --
and the carve into five blocks; without them the parent does not fit
128 GB (the design README's memory table).

## XiangShan through this flow

What this flow learned about XiangShan from the outside; corrections from
the people who know the design are the most welcome contribution.

**The boundary and the configuration.** XSTile is the unit hardened: one
clock, the core with its L1s and the L2, instanced per core in XSTop. The
generator (`verilog/src/FlowGenerator.scala`) elaborates XSTop and the
flow takes XSTile out of it. The configuration is DefaultConfig with the
FPU and VPU turned off, no bitmap check, no custom cache-op CSRs, no memory
BIST, a 512 KB L2 and a 1 MB LLC. `HasFPU = false` and `HasVPU = false`
gate the ISA, not the hardware: the floating-point and vector regions are
still built, and are hardened blocks. Debug, perf, difftest, ChiselDB and
Constantin apparatus is off (`FPGAPlatform = true`).

**The cut.** Five hardened blocks -- Frontend, MemBlock, CoupledL2,
VecRegionModule, FltRegionModule -- each implemented through global route
at its planned outline with its pins on the side its partners are, and
the parent, XSTile's own logic (the backend's control, the integer
region, decode, dispatch, rename, the ROB), placed between them. The plan
(`xstile/plan/`) was drawn by bazel-orfs's [`macro-selection`][skills]
tables from the RTL's interfaces in space and time. Backend hardened as
one block had its own loops at 9,038 ps; flattening a region or MemBlock
into the parent took the parent's global route past 119 GB.

**Register files and memories.** Nine multi-ported register files are
generated as macros rather than synthesised (yosys does not finish them at
this size): IntRegFile, FpRegFile, VfRegFile, the Ftq's four queues, the
rename buffer and the ROB's entry file (`xstile/*.regfile`). The last
five exist as modules only because of carried RTL patches (below). The
SRAMs become macros through ORFS's AUTO_MEMORIES on firtool's
`array_<depth>x<width>` modules; firtool is run without
`--repl-seq-mem`, which would turn 18 of 38 convertible memories into
blackboxes.

**Clock gating.** Utility's `ClockGate` becomes CIRCT's clock-gate
intrinsic (carried patch 0010), so firtool folds a gate whose enable is
constant: ExeUnit's CSR and fence units tie theirs high, and as gates
they left every register behind them untimed (OpenSTA#532). The rest map
onto ASAP7's integrated clock gate (`xstile/xs_icg.ys`,
`xstile/xs_icg_map.v`): E to ENA, TE to SE.

**What synthesis does to some modules, and the RTL patches.** XiangShan
writes per-entry logic as loops over the entries; synthesised flat, one
module becomes one long single-threaded synthesis partition. The carried
patches (`verilog/patches/`, each header says why) give the logic a module
to live in, behaviour unchanged:

| patch | module | before |
|---|---|---|
| 0002 | Ftq's four queues as register-file modules | 53 kbit of flops behind 64:1 read muxes |
| 0003, 0007, 0008 | Rob: one entry cell per entry, its inputs as one bus, its payload as a register file | one module of 214 k lines, about 40 min in ABC; 443 pins per cell |
| 0004 | the store queue: one entry cell per entry | 56 min, 36 of them in ABC |
| 0005 | the rename buffer as a register file | twenty 352:1 read muxes, a 20-minute partition |
| 0006 | the store queue's forwarding, one module per load pipeline | over 20 minutes in ABC |
| 0010 | Utility's ClockGate as the CIRCT intrinsic | gates with a constant enable survived |
| 0011 | `debug_runahead_checkpoint_id` in debug builds only | about 13 k flops in MemBlock and a 64-bit port on three blocks |

**Netlist surgery** (`xstile/surgery/`): before synthesis, the branch and
jump units' mispredict compares are rewritten without the adder, and
constants firtool leaves on the ports of the modules the flow keeps are
folded in five rounds, each entry's precondition checked exactly.
`XSTile_boundary_constants` finds them. **The array front end**
(`xstile/arrays/`) extracts the per-entry logic of the load and store
queues, the TLB and the ROB as one cell per entry, each extraction proven
equal to its module with a negative control.

**Where the period is.** Every block is far faster than the parent. The
parent's worst paths are wire between blocks and through the backend's
control: in the reference run the worst is MemBlock's load-cancel output
into the integer region's store-data issue queue. In earlier runs the
mass over 2,000 ps sat in one corridor, the floating-point region's pins
and the control block's decode, dispatch and rename (inventory 58). The one-flop-loop floor (a register
back to itself, which no skew or retiming passes) is at the decode
buffer's valid bits. bazel-orfs's [timing inventory][ideas], entries 58
and 63, has the numbers.

## Reference results

bazel-orfs main at #1627, 2026-11-18: a clean build of every block and the
parent, from scratch, minimum register-to-register period at global route
(the SDC period minus the worst slack):

| part | ps |
|---|---|
| XSTile, the parent | 2,024 |
| MemBlock | 1,172 |
| FltRegionModule | 1,033 |
| Frontend | 1,032 |
| VecRegionModule | 1,023 |
| CoupledL2 | 864 |

44 endpoints over 2,000 ps. About 7.9 M standard cells, 735 k flops, 338
macros, a die of about 2.1 x 2.7 mm. The history, from 6,200 ps: bazel-orfs's
[design README][design] and [`kpi.json`][kpi].

### What is deliberately broken

Global route skips past pin access (OpenROAD DRT-0073 on one pin per block,
inventory entry 12) and runs with congestion allowed: the period is what
is tracked, not a routable layout. The register files are generated, not
synthesised. The configuration is integer-only with a small LLC.

## Moratoriums

Things not to try, each with why and what would lift it. "Inventory" is
bazel-orfs's [timing inventory][ideas].

| # | Do not | Why | Lifted when |
|---|---|---|---|
| M1 | Rank alternatives before global route, by a synthesis, floorplan or placement number or an estimate model | Only global route says: ABC buffer trees that looked real at synthesis and place were worth nothing there; a keep A/B timed at synthesis pointed one way unbuffered and the other buffered ([`dogfooding`][skills], rule 14) | One can name why the earlier attempts failed and what is genuinely new; "a better estimate" is not new |
| M2 | Fix timing with multicycle or false-path exceptions, or by adding registers to the RTL | The flow's tools are what is measured; fix them, not the design ([`dogfooding`][skills], rule 1) | Never, for this flow |
| M3 | Clean the Bazel cache, partly or wholly | A day of builds is in it. A surprising rebuild is investigated through its keys ([`cache-miss`][skills]); to rebuild something, change code that reaches its key | Never |
| M4 | Quote a period from a part whose global placement ran without its timing or routability driver | Neither driver says so in the exit status; such data is invalid | Never; `<part>_gpl_driven` is the gate |
| M5 | Dissolve the hardened blocks into the parent for a shorter period | Failed three times, on period and, with every block dissolved, on runtime (inventory 51) | The items at the end of inventory 51 are fixed, and a one-block arm comes before an all-block one |
| M6 | Retry without new data: ordering the logic segment by logic position (+146 % overflow); a regenerated pin-partner dump (parent 9 %, regions 16 and 30 % worse); Backend hardened whole (its loops at 9,038 ps); flattening a region or MemBlock into the parent (global route past 119 GB); a block with a generated memory and no keep list (yosys cannot cost the blackbox) | Measured (inventory 35) | New data |
| M7 | Run one of the parked campaign arms without a reason | Each is a global route of about six hours (inventory 58, 63) | A reason why it is new is named |
| M8 | Retime | OpenROAD's retiming does forward moves only, no async set/reset flops (23 % of XSTile's), no hierarchical designs; its bound here is -163 ps | OpenROAD#12304 lands with those, and the hard blocks are fenced off |
| M9 | Give ABC a delay target | Frontend 7 % slower and 1.4 % larger at 473 ps (inventory 39) | New evidence on why |
| M10 | Use global placement's timing pull without its slack threshold | It pulled 138,000 of 156,682 endpoints and ground for 8 h 34 min (inventory 63) | Measured on the tail only (`XS_GPL_TAIL_PULL`) |
| M11 | Balance the blocks' clocks, move the blocks or tune CTS to cure the tail | Measured: not the cause; balancing makes the tail worse (inventory 63) | Global route data says otherwise |
| M12 | Read a synthesis-stage number before `repair_design` | Unbuffered netlists rank nothing (inventory method notes) | Never |
| M13 | Synthesise the register files | yosys does not finish them at this size | yosys does |
| M14 | Expect a routable layout | Pin access is skipped and congestion allowed (above) | DRT-0073 on the blocks' pins is understood and fixed (inventory 12) |
| M15 | Judge a change by WNS < 0, or in percent of WNS | The minimum period in ps, and the endpoints and excess over 2,000 ps, decide (inventory 55, 58) | Never |
| M16 | Run two parent builds on one 128 GB machine | The parent's global route peaks at 53 to 85 GB | More memory |
| M17 | Wait on a doomed baseline | Probe the scaling at small sizes and cap every point ([`no-doomed-baselines`][skills]) | Never |
| M18 | Run equivalence checking (kepler-formal) at full size | It works at module size | kepler-formal scales |
| M19 | Add power measurement to this directory | Out of scope ([Future work](#future-work)) | It is ported on its own |

## Things to fix

Tasks for whoever continues, each with how to check it and how it retires.

1. **Run it end to end here** (steps 2 to 6) and record the date, the
   XiangShan and bazel-orfs commits and the machine with the numbers.
   Check: the step gates. Retires: when the status line at the top is
   replaced.
2. **Rebase onto kunminghu-v3** (156 commits ahead of this branch's base
   when it was made). On it, 6 of the 7 patches on XiangShan do not apply
   as they are (0004 does; 0011 only with fuzz), the Utility patch does; the surgery's manifest, the arrays'
   module names and the plan's pins name ports and modules of this base.
   Check: steps 1 to 4 on the new base. Update `verilog/resources/gitStatus`.
3. **The carried OpenROAD, OpenSTA, ORFS, yosys and sv-elab patches**
   retire upstream. `bazelisk run @bazel-orfs//:bump` moves the pin;
   `@bazel-orfs//tools/carried_patches:check_carried_patches` against a
   pristine tree says ABSORBED for a patch upstream took (the
   [`carried-patches`][skills] skill). Copies here:
   `bazel-orfs-patches/` (38 OpenROAD, 11 OpenSTA, 3 sv-elab) and
   `patches/` (8 yosys, 1 boost.icl, 5 that quiet build warnings); ORFS's
   15 come through the module. The list and each patch's reason:
   bazel-orfs's [`patches/`][patches] and [`MODULE.bazel`][module].
4. **The RTL patches** (`verilog/patches/`) retire if XiangShan ever
   carries an equivalent change; until then the flow applies them to a copy
   of the sources. Check: the build without the patch reaches the same
   synthesis time and period.
5. **Regenerating the plan** needs a pin-partner dump that is not checked
   in (`probe_pin_partners.tcl` on `XSTile_synth_odb_debug`); needed after
   the rebase if ports moved. The [`macro-selection`][skills] skill.
6. **`verilog/resources/gitStatus`** is written by hand; generate it from
   the checkout.
7. **A Chinese translation of this README**, if wanted.
8. **Left out of this port**: bazel-orfs's checks that the L2's SRAMs
   reached the netlist as macros, the flat-tile variant, the pin-plan
   variants, the kit for other tools, the KPI history and charts, and the
   LEC proof of the branch-unit surgery (kepler-formal). Port one when it
   is needed.

## Keeping up with bazel-orfs

`bazelisk run @bazel-orfs//:bump` moves the bazel-orfs, OpenROAD and ORFS
pins and rewrites the OpenROAD, qt-bazel and sv-elab blocks of
`MODULE.bazel` and `bazel-orfs-patches/`. Everything else in the root-only
section of `MODULE.bazel`, `patches/` and `.bazelrc` is synced by hand:
diff bazel-orfs's `MODULE.bazel` and `patches/` between the old pin and the
new one and carry each change here with its reason. bazel-orfs's
[`docs/openroad.md`][openroad], "Root MODULE.bazel requirements".

## Definition of done

For whoever continues, a person or an AI agent. The work is complete when
all of these hold:

1. `bazelisk build --nobuild //...` passes.
2. `//verilog:xiangshan_flat`, `//xstile/surgery:xiangshan_surgered_elaborates`
   and `//xstile/arrays:xiangshan_extracted` build, and every test in
   `bazelisk query 'tests(//...)'` passes.
3. `bazelisk run //xstile:build` finishes: every block and the parent
   through global route, every `<part>_gpl_driven` gate passes, every
   `<part>_kpi` is written.
4. Each part's period is compared with the reference results and every
   difference over about 5 % is explained (a fix that exposed hidden paths
   is the first suspect of a regression).
5. `xstile/resources.json` is refreshed by the run and committed.
6. The status line at the top is replaced by a dated line: the XiangShan
   commit, the bazel-orfs commit, the machine (cores, memory) and what was
   run; the reference results gain this tree's numbers beside them.

The limits while getting there: the moratoriums above; no power
measurement in this directory; an RTL change is a patch in
`verilog/patches/` with a header saying why, function kept and checked.

No agent configuration or skills are part of this directory. bazel-orfs
has its own, for its own tools, and an agent can link them locally at the
version `MODULE.bazel` pins:

```sh
bazelisk run @bazel-orfs//:agent -- setup
```

run here, writes `.mcp.json`, `.claude/settings.json` (a session hook that
links the skills) and `CLAUDE.md` (which imports bazel-orfs's
[agent rules][rules]); this directory's `.gitignore` keeps them local. The
hook links bazel-orfs's whole source tree as `.claude/bazel-orfs`, so the
timing inventory and the design README are readable offline:
[`tools/agent/README.md`][agent].

## Reporting problems

A problem with the flow on XiangShan: an issue here, saying it concerns
this experimental flow. A problem in yosys, OpenROAD or ORFS that the flow
exposes: an issue in bazel-orfs, which carries the fix as a patch until
upstream has it. An untar-and-run reproducer of a stage:
`bazelisk run @bazel-orfs//:dirty -- archive //xstile:<part> <stage> <file.tar.gz>`.

## Future work

Activity-driven power for XiangShan (CoreMark per Joule) is being worked
on in bazel-orfs's [coremark_joule study][cmj] and may come here later.
Below 2,000 ps, the ranked list of what is next is inventory entry 63.

## Credits and licence

Ported from [bazel-orfs][bo] at `e2f12648`, where the flow, the plan, the
surgery and the measurements were developed; the files here keep
bazel-orfs's licence (`LICENSE` in this directory), the patches their
projects'.

[bo]: https://github.com/The-OpenROAD-Project/bazel-orfs
[pin]: https://github.com/The-OpenROAD-Project/bazel-orfs/tree/e2f1264831d67f78a6b797aedf5aaf6f3ea96cb9
[skills]: https://github.com/The-OpenROAD-Project/bazel-orfs/tree/e2f1264831d67f78a6b797aedf5aaf6f3ea96cb9/.claude/skills
[ideas]: https://github.com/The-OpenROAD-Project/bazel-orfs/blob/e2f1264831d67f78a6b797aedf5aaf6f3ea96cb9/ideas/xiangshan-timing.md
[design]: https://github.com/The-OpenROAD-Project/bazel-orfs/blob/e2f1264831d67f78a6b797aedf5aaf6f3ea96cb9/test/coremark_joule/designs/asap7/xiangshan/README.md
[kpi]: https://github.com/The-OpenROAD-Project/bazel-orfs/blob/e2f1264831d67f78a6b797aedf5aaf6f3ea96cb9/test/coremark_joule/designs/asap7/xiangshan/kpi.json
[patches]: https://github.com/The-OpenROAD-Project/bazel-orfs/tree/e2f1264831d67f78a6b797aedf5aaf6f3ea96cb9/patches
[module]: https://github.com/The-OpenROAD-Project/bazel-orfs/blob/e2f1264831d67f78a6b797aedf5aaf6f3ea96cb9/MODULE.bazel
[openroad]: https://github.com/The-OpenROAD-Project/bazel-orfs/blob/e2f1264831d67f78a6b797aedf5aaf6f3ea96cb9/docs/openroad.md#root-modulebazel-requirements
[rules]: https://github.com/The-OpenROAD-Project/bazel-orfs/blob/e2f1264831d67f78a6b797aedf5aaf6f3ea96cb9/docs/agent-rules.md
[agent]: https://github.com/The-OpenROAD-Project/bazel-orfs/blob/e2f1264831d67f78a6b797aedf5aaf6f3ea96cb9/tools/agent/README.md
[cmj]: https://github.com/The-OpenROAD-Project/bazel-orfs/blob/e2f1264831d67f78a6b797aedf5aaf6f3ea96cb9/test/coremark_joule/README.md
