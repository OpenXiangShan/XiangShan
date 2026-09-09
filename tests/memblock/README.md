# MemBlock Random Verification

This directory contains a block-level verification environment for the generated
Kunminghu-v2 `MemBlock`. It uses Picker/XS-MLVP for the C++ DUT wrapper and keeps
the testbench source independent from generated build products.

The current environment verifies modeled scalar and 128-bit vector load/store
behavior, all vector load and store address modes independently, software
prefetch, scalar/vector misalignment and cross-page splits, the virtual load and
store queues, Sv39/Sv48 and all four VS/G-stage two-stage translation pairs,
exact guest-page-fault metadata, DCache misses/refills and a byte-checked
dirty-pressure phase, manager-originated DCache probes, GrantAck handling, all
four store-forwarding directions, PBMT=NC, redirects, and queue pressure.
D-width AMOs (ADD/XOR/AND/OR/SWAP/MIN/MAX, signed and
unsigned) plus LR/SC are covered through the atomic unit, including old-value
writeback, AMOCAS compare success/failure, reservation success/failure, and
cache visibility. The L2-to-L1 DTLB and L2 hint input boundaries are checked for
legal miss/refill-hit/cancel behavior and exact translation/PMP/PMA metadata.
All three frontend bridge paths are driven
with concurrent legal TileLink traffic, randomized request/response backpressure,
source-credit-safe wrap, and field-exact request/response scoreboards. Uncache
denied and corrupt D-channel responses are checked through scalar exception
writeback. PBMT=IO MMIO metadata and error propagation are covered; MMIO device
side effects and CMO CLEAN/FLUSH/INVAL functional/error paths are covered; the
same CMO operations, line states, and younger-load cancellation are also
weighted dimensions in the common constrained-random generator. Legal CMO
denied/corrupt responses are independently constrained there as well, with a
zero rate in the SPEC-like preset.
`hypervisor-contracts` covers HLV/HLVX/HSV privilege, permission, fault, PMP
execute behavior, and each operation family across all four Sv39/Sv48 and
Sv39x4/Sv48x4 two-stage translation pairs. A five-case PBMT basis covers final
PMA/NC/IO classification and VS-over-G priority for every hypervisor operation
family, and cacheable misaligned HLV/HLVX/HSV exercise the scalar split paths.
The same five PBMT combinations are weighted in schema-24 `random-mixed` and
closed across every enabled operation family and SPVP value per seed. Exact
two-stage PA/data/store effects and DCache-versus-Uncache routing are checked;
NC/IO random actions are naturally aligned while PMA retains the schema-23
aligned/misaligned cross. Schema 25 adds the fixed SoC PMA device interval as a
weighted address class and closes every enabled HLV/HLVX/HSV x SPVP x
DDR/device bin. HLV/HSV require exact Uncache data or store effects; HLVX must
raise `LoadAccessFault`, cancel every speculative wakeup, and issue no data
manager request because that PMA interval is not executable.
Schema 26 adds seven physical-PMP relation weights: no-PMP control, first and
last naturally aligned accesses inside a 4-KiB NAPOT allow region, naturally
aligned accesses immediately below and above it, and accesses crossing either
edge. Every enabled relation is crossed with HLV/HLVX/HSV and SPVP=S/U. A
hand-built first-match PMP model independently predicts success or exact
load/store access fault and constrains DCache/Uncache side effects.
It also executes HLV, HLVX, and HSV from M-mode under SPVP=U/S with physical
PMP R-only, X-only, RW, and RX regions. The independent permission oracle
requires R for HLV, R+X for HLVX, and W for HSV, so an accidental M-mode PMP
bypass is observable as a wrong completion instead of a load/store access
fault. A separate two-stage mapping targets the SoC's fixed `c=0` PMA device
window: HLV and HSV must use Uncache with exact load/store data, while HLVX
must report `LoadAccessFault` because that physical region is not executable.
Vector FOF and unit-stride, strided, indexed-unordered, and
indexed-ordered segment load/store takeover are covered by focused tests; the
common constrained tail composes segment load/store direction with all four
addressing modes, EEW/SEW 8/16/32/64, fractional/integer LMUL and derived EMUL,
and NF 2..8. It enumerates the legal decoder space before weighted selection;
unsupported shapes are not silently randomized as ordinary LSQ traffic.

The directed segment matrix exhausts all legal NF 2..8 crosses with the 78
ELEN=64 EEW/SEW/LMUL/EMUL bases for all four addressing modes. Unit-stride and
strided use the decoder's `ceil(EMUL) * NF <= 8` limit and execute 676
configurations, 7,096 load/readback uops, and 3,548 store uops. Indexed modes
use `ceil(LMUL) * NF <= 8` and `max(ceil(LMUL) * NF, ceil(EMUL))` uops: their
676 configurations execute 7,264 load/readback and 3,632 store uops, including
252 index-only uops when EMUL exceeds the segmented data group. Strided cases
split evenly between positive and negative strides. Independent oracles model
field/Vd-chunk selection, the complete index register group, data, writeback
mask, and every element address.

`vector-segment-fof` contrasts first- and later-element page faults. A later
fault suppresses the exception and shortens VL, while a first-element fault
retains the exception and fault VA and leaves VL unchanged.

`pmp-contracts` programs the distributed PMP CSR input and checks data-side
TOR and NAPOT regions, exact lower/upper edges, R/W and AMO denial, overlapping
entry priority, M-mode unlocked bypass, locked-entry enforcement, and locked
address/config immutability. This build uses a 4-KiB PMP platform grain, so
NA4 is not independently selectable: an `A=2` write is WARL-coerced to NAPOT
and is checked as a 4-KiB minimum region. Instruction X permission, HLVX
physical R+X permission, M-mode/SPVP hypervisor PMP selection, and DebugModule
access are covered by focused tests. HLV/HLVX/HSV also cross one fixed PMA
device mapping, both sides of the exact `0x80000000` device-to-DDR PMA edge,
and locked R/RWX PMP entries. Schema 26 adds the seven-class 4-KiB NAPOT edge
cross; other PMP size/TOR/lock/permission/overlap-by-edge and PMA interval
matrices remain gaps.

The MemBlock-facing L2-to-L1 DTLB request/response boundary is also exercised.
`l2-tlb-contracts` checks read-request acceptance, ordinary and prefetch miss
responses, kill and `no_translate`, and exact cacheable, PBMT=NC, and PBMT=IO
no-fault PA/PBMT/PMP/PMA results after the requestor's own PTW refill. The miss
is returned immediately while the shared prefetch TLB fills internally; a
same-VA retry must hit without a new external PTW TileLink A request. The same
refill/hit contract is checked for stage-1 page fault, nested G-stage guest-page
fault, and PTW access-fault entries. Their fault bits must be exact, but address
and PMP fields are deliberately not constrained because the L2 consumer drops
the result on fault. PMP allow and a locked 4-KiB deny are checked on a real
cacheable translation. The separately registered PMP result is sampled one
cycle after the TLB response, matching `PMPChecker(leaveHitMux=true)`.

## Architecture

```text
load/store sequences -> generated lane adapters -> MemBlock
                                                 |       |
                              writeback monitors <-       -> TileLink agent
                                      |                       |
                                  scoreboards       independent reference
                                                           + bus memory
                                      |
                              functional coverage
```

The C++ harness is partitioned by verification responsibility while preserving
Picker's single-translation-unit build:

- `cpp/memblock_main.cpp` owns command-line parsing and scenario dispatch;
- `cpp/random/*.inc` owns constrained-random policy, profiles, validation,
  functional coverage models, and closure gates;
- `cpp/scenarios/*.inc` groups focused and random sequences by verification
  domain, so work on one family does not require loading the entire scenario
  catalog;
- `cpp/memblock_env.hpp` is the stable environment facade, while
  `cpp/environment/*.inc` separates the reference model, DCache/PTW/uncache
  agents, scoreboards, and top-level environment orchestration.

The `.inc` modules are intentional: Picker exports `memblock_main.cpp` as a
single generated `example.cpp`. `scripts/prepare_picker_harness.py` copies the
module trees beside that file, and the Makefile includes every module in build
dependencies and controller provenance hashes. This keeps compilation and ABI
behavior unchanged while making ownership and review boundaries explicit.

The reusable environment components provide:

- reset and cycle control with registered Picker clock;
- typed LSQ, scalar load/store, vector load/store, and software-prefetch drivers;
- passive six-lane LSQ enqueue monitoring with independent LQ/SQ vector-flow
  accounting and dispatch width/lane coverage;
- per-redirect sampling of the retained top-level LQ/SQ cancellation counts
  after their documented pipeline latency, with observed/unobserved accounting;
- distributed PMP CSR programming for packed config and address registers;
- coherent TileLink A/B/C/D/E memory agent with randomized ready/response delay;
- PTW TileLink agent with independent request and response backpressure;
- uncache TileLink agent with forced-first and randomized request/response stalls;
- TileLink B/C/E Probe/ProbeAck/GrantAck plus Release/ReleaseData capture,
  ReleaseAck, and byte-exact writeback;
- separate byte-addressed architectural reference and bus backing memories;
- ISA load formatting plus byte-exact checks on every dirty ReleaseData beat;
- scalar load/prefetch, scalar store, and vector memory scoreboards;
- byte-accurate vector masking, old-destination, and SQ-forwarding models;
- deterministic transaction and functional coverage reporting.

The correctness contracts are cataloged separately in
`docs/ORACLES.md`. `docs/VERIFICATION_PLAN.md` contains the complete test-point
inventory, including explicit planned gaps for broader device/error ordering,
reservation interference, wider multi-class CMO and full-core fence ordering,
ordered side-effecting indexed accesses, remaining PMP/PMA matrices,
coherence protocol negatives, remaining random error families, cross-cause/vector exception
priority, and four-state behavior. A passing
cacheable mixed campaign must not be interpreted as verification of those
planned rows.

Standalone bug reports are reserved for independently reproduced and
root-caused CPU RTL defects. Their filenames use the `CPU_BUG_*.md` prefix.
UT harness, oracle, regression-controller, and provenance fixes are recorded in
normal commits and consolidated documentation only; they do not receive a
per-fix Markdown report. The confirmed reports currently retained are
`CPU_BUG_UNCACHE_DCHANNEL_ERROR.md`, `CPU_BUG_ATOMIC_EXCEPTION_RF_WEN.md`,
`CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md`, `CPU_BUG_FP_EXCEPTION_FP_WEN.md`, and
`CPU_BUG_VECTOR_SEGMENT_TRIGGER_ADDRESS_LAG.md`,
`CPU_BUG_PTW_DCHANNEL_ERROR_IGNORED.md`, and
`CPU_BUG_CMO_DCHANNEL_ERROR.md`.

The structure follows UVM responsibilities without requiring a SystemVerilog
class runtime:

| UVM responsibility | MemBlock implementation |
| --- | --- |
| Sequence/sequencer | Focused scenarios and deterministic seeded generators in `cpp/scenarios/` |
| Driver | Generated typed LSQ, scalar, vector, prefetch, redirect, and commit adapters |
| Active agent | Coherent DCache, PTW, and uncache memory agents with independent backpressure |
| Monitor | Per-cycle writeback, queue-dequeue, TLB-feedback, and TileLink handshake sampling |
| Reference model | Byte-addressed `SparseMemory` plus scalar extension, vector mask, and forwarding functions |
| Scoreboard | Scalar load/prefetch, scalar store, and vector memory scoreboards with duplicate rejection |
| Coverage | Per-seed operation/lane/cache/TLB/queue/redirect/forwarding gates and JSON summaries |
| Assertions | Generated producer stability binds plus all-entry StoreQueue TLB-miss preservation checks |

`scripts/generate_cpp.py` derives repetitive lane accessors from the checked RTL
manifest. No test hard-codes a flattened port without that interface first being
present in `config/expected_ports.json`.

## Build From A Clean Checkout

The UT does not elaborate Chisel itself. It consumes the split SystemVerilog
generated by the XiangShan top-level build, so there are two distinct build
stages: generate XiangShan RTL from the repository root, then build the
MemBlock model from `tests/memblock`.

The environment used for the recorded results was Ubuntu 24.04 with JDK 17,
Mill 0.12.3, Python 3.12, GNU Make 4.3, GCC/G++ 13.3, CMake 3.28, and Verilator
5.048. These are a known-working baseline, not all strict minimum versions.
Picker requires SWIG 4.2 or newer; `bootstrap-picker` uses a system SWIG when
available and otherwise attempts a local extraction with `apt-get download`
and `dpkg-deb`.

From a clean clone of the branch under test, initialize the submodules and
generate exactly the RTL configuration used by this environment:

```sh
cd /path/to/XiangShan
git submodule sync --recursive
make init
make verilog CONFIG=DefaultConfig NUM_CORES=1 ISSUE=E.b JVM_XMX=40G
test -s build/rtl/MemBlock.sv
test -s build/rtl/filelist.f
```

`make verilog` invokes `top.TopMain`, resolves the matching Firtool through the
Mill build, emits split SystemVerilog under `build/rtl`, and generates the SRAM
wrapper files referenced by `filelist.f`. On the machine used for this work it
took roughly 3-5 minutes and reached 20-27 GiB resident memory. Allow at least
32 GiB of usable RAM (more is preferable with `JVM_XMX=40G`) and 10 GiB of free
disk for RTL, Picker, the Verilated model, and intermediate objects. Reducing
`JVM_XMX` is possible on another host, but has not been qualified here.

Then build the pinned Picker toolchain and the MemBlock simulator:

```sh
make -C tests/memblock bootstrap-picker JOBS=8
make -C tests/memblock prepare-rtl check-ports check-rtl unit
make -C tests/memblock smoke JOBS=8
```

The first `smoke` invocation performs the expensive Picker export and Verilator
compile. Later test targets reuse
`build/memblock/picker/UT_MemBlock/build/UTMemBlock_example` and normally only
rebuild the small C++ harness after a testbench source change. A successful
bootstrap records the exact Picker/xcomm revisions and executable path in
`build/memblock/tools/picker.json`; the generated RTL identity is recorded in
`build/memblock/rtl.json`.

For an existing Picker binary, skip `bootstrap-picker` and pass it explicitly:

```sh
make -C tests/memblock smoke PICKER=/path/to/picker JOBS=8
```

Do not reuse `build/rtl` after changing `CONFIG`, `ISSUE`, generation flags, or
the RTL commit. Those command-line choices are not all Make prerequisites.
Regenerate the RTL in a clean build tree, then rerun `prepare-rtl`; otherwise a
stale `filelist.f` can silently select the wrong design until the hash/port
checks reject it. Do not run `make ports` merely to make a mismatch disappear:
the tracked port manifest should change only when an intentional MemBlock
interface change has been reviewed.

Common setup failures are:

- `build/rtl/MemBlock.sv` or `build/rtl/filelist.f` missing: run the root-level
  `make verilog` command above, not only a target under `tests/memblock`.
- Mill/Chisel import or missing-module errors: rerun `make init` and confirm the
  checkout has the branch's recorded submodule commits.
- Java heap allocation or an OOM kill: free memory or move the RTL generation
  to a host with at least the capacity noted above; lowering `JOBS` does not
  materially lower the Chisel elaboration heap.
- `Picker requires SWIG >= 4.2`: install SWIG 4.2+ on `PATH`, or ensure
  `apt-get download` and `dpkg-deb` are available to the bootstrap script.
- Picker export reports a missing RTL file: inspect paths in
  `build/rtl/filelist.f`; relative entries are resolved from `build/rtl`, and
  all listed `.sv`/`.v` files must belong to the same generation.
- `check-ports` reports a stale manifest: first confirm the expected branch,
  `DefaultConfig`, and RTL commit. Treat a genuine port delta as a design
  review item rather than regenerating the checked manifest blindly.

## Focused Tests

```sh
make ports prepare-rtl check-ports check-rtl unit
make smoke PICKER="$PICKER" JOBS=8
make pin-space PICKER="$PICKER" JOBS=8
make frontend-bridge PICKER="$PICKER" JOBS=8 SEED=1 TRANSACTIONS=4096
make frontend-reset-recovery PICKER="$PICKER" JOBS=8
make single-load PICKER="$PICKER" JOBS=8
make load-feedback PICKER="$PICKER" JOBS=8
make topdown-contracts PICKER="$PICKER" JOBS=8
make l2-flush-contracts PICKER="$PICKER" JOBS=8
make top-control-contracts PICKER="$PICKER" JOBS=8
make trace-bridge-contracts PICKER="$PICKER" JOBS=8
make dft-bridge-contracts PICKER="$PICKER" JOBS=8
make memory-violation PICKER="$PICKER" JOBS=8
make rar-violation PICKER="$PICKER" JOBS=8
make ifetch-prefetch PICKER="$PICKER" JOBS=8
make hardware-prefetch PICKER="$PICKER" JOBS=8
make fp-loads PICKER="$PICKER" JOBS=8
make trigger-contracts PICKER="$PICKER" JOBS=8
make metadata-contracts PICKER="$PICKER" JOBS=8
make dcache-errors PICKER="$PICKER" JOBS=8
make dcache-coherence PICKER="$PICKER" JOBS=8
make uncache-errors PICKER="$PICKER" JOBS=8
make uncache-widths PICKER="$PICKER" JOBS=8
make uncache-outstanding PICKER="$PICKER" JOBS=8
make sbuffer-flush PICKER="$PICKER" JOBS=8
make sbuffer-timeout PICKER="$PICKER" JOBS=8
make mmio-contracts PICKER="$PICKER" JOBS=8
make cbo-zero-contracts PICKER="$PICKER" JOBS=8
make cmo-contracts PICKER="$PICKER" JOBS=8
make wfi-safety PICKER="$PICKER" JOBS=8
make reset-recovery PICKER="$PICKER" JOBS=8
make reset-tree-contracts PICKER="$PICKER" JOBS=8
make atomic-contracts PICKER="$PICKER" JOBS=8
make atomic-dchannel-errors PICKER="$PICKER" JOBS=8
make scalar-misaligned PICKER="$PICKER" JOBS=8
make misaligned-stores PICKER="$PICKER" JOBS=8
make exception-contracts PICKER="$PICKER" JOBS=8
make pmp-contracts PICKER="$PICKER" JOBS=8
make hypervisor-contracts PICKER="$PICKER" JOBS=8
make pointer-masking-contracts PICKER="$PICKER" JOBS=8
make mbmc-contracts PICKER="$PICKER" JOBS=8
make l2-tlb-contracts PICKER="$PICKER" JOBS=8
make ifetch-ptw-bridge PICKER="$PICKER" JOBS=8
make two-stage-translation PICKER="$PICKER" JOBS=8
make translation-matrix PICKER="$PICKER" JOBS=8
make translation-fence PICKER="$PICKER" JOBS=8
make translation-fence-all PICKER="$PICKER" JOBS=8
make translation-inflight-context-all PICKER="$PICKER" JOBS=8
make translation-context PICKER="$PICKER" JOBS=8
make translation-bare PICKER="$PICKER" JOBS=8
make translation-faults PICKER="$PICKER" JOBS=8
make translation-permissions PICKER="$PICKER" JOBS=8
make translation-pbmt PICKER="$PICKER" JOBS=8
make translation-superpages PICKER="$PICKER" JOBS=8
make scalar-guest-fault PICKER="$PICKER" JOBS=8
make vector-guest-fault PICKER="$PICKER" JOBS=8
make vector-load PICKER="$PICKER" JOBS=8
make vector-split-load PICKER="$PICKER" JOBS=8
make vector-fof PICKER="$PICKER" JOBS=8
make vector-segment PICKER="$PICKER" JOBS=8
make vector-segment-fof PICKER="$PICKER" JOBS=8
make vector-addressing PICKER="$PICKER" JOBS=8
make vector-store-forwarding PICKER="$PICKER" JOBS=8
make store-forwarding PICKER="$PICKER" JOBS=8
make store-rdata-order PICKER="$PICKER" JOBS=8
make store-tlb-miss-preserve PICKER="$PICKER" JOBS=8
make iq-slow-feedback PICKER="$PICKER" JOBS=8
make dcache-release PICKER="$PICKER" JOBS=8
make redirect PICKER="$PICKER" JOBS=8
make queue-pressure PICKER="$PICKER" JOBS=8
make random-mixed PICKER="$PICKER" JOBS=8 SEED=1 TRANSACTIONS=16384
make random-stress PICKER="$PICKER" JOBS=8 SEED=1 TRANSACTIONS=16384
```

`translation-matrix` exercises all four VS/G-stage combinations:
`Sv39->Sv39x4`, `Sv39->Sv48x4`, `Sv48->Sv39x4`, and `Sv48->Sv48x4`. It maps
the VS page-table pages through G-stage, checks a high-half Sv48 VA, and
requires the second access to reuse the cold translation without new PTW
requests.

`translation-fence` updates live stage-1 and nested leaves, then checks global
and selective `SFENCE.VMA`, selective `HFENCE.VVMA`, and global
`HFENCE.GVMA` visibility after the required refill. It also rebinds the same
host ASID, VS ASID, and VMID to distinct page-table roots, applies the matching
targeted fence, and checks distinct physical data after a new PTW refill. A
stage-1 race holds an old 1-GiB root-leaf response in the PTW manager, replaces
the PTE, and aligns `SFENCE.VMA` with the redirect. The canceled load must not
write back, and a same-identity survivor must refill and return only the new
physical data. `translation-fence-all` runs separate global and selective
processes for all four Sv39/Sv48 and Sv39x4/Sv48x4 mode pairs. Each process
applies its fence scope to stage-1, VS-only, G-only, fully nested VS, and fully
nested G delayed PTE responses. Every race waits for the exact target PTE
TileLink request with at least 12 cycles of assigned response latency before
replacing the PTE and fencing; the selective `HFENCE.GVMA` operand is encoded
as `GPA >> 2`.

`translation-inflight-context-all` holds the first PTW response for 256 cycles,
changes stage-1 root/ASID/MODE, both nested roots plus ASID/VMID/MODE, or `V`
in either host/nested direction, and redirects the younger old-context load. It
then reuses that load's ROB/LQ identity and checks that only the new mapping
writes back. Separate processes cover both Sv39/Sv48 directions and all four
VS/G starting mode pairs; keeping this matrix out of the already large fence
process avoids the DPI shared-library static-TLS instance limit.

`translation-context` checks five context families with 14 architectural
loads: direct Sv39-to-Sv48 mode/root switching, same-mode `satp` ASID/root,
`vsatp` ASID/root under a retained G context, `hgatp` VMID/root under a retained
VS context, and host/nested/host `virt` transitions. Each context maps the same
input address to distinct data so stale translation reuse is externally
observable.

`translation-superpages` walks 2 MiB and 1 GiB leaves in Sv39/Sv48 and their
Sv39x4/Sv48x4 G-stage equivalents, plus the Sv48 512 GiB leaf. It also builds
legal 64-KiB Svnapot mappings in Sv39, Sv48, Sv39x4, and Sv48x4 by installing
the identical `N=1, PPN[3:0]=8` PTE in all 16 leaf slots. Every Svnapot subpage
is checked with an independent VPN-low-bit address oracle, an initial load, a
committed scalar store, and an exact load readback. All four VS/G mode pairs
also cross VS-only NAPOT, final-G-only NAPOT, and simultaneous VS/G NAPOT
leaves while G-stage translations of the VS page-table pages remain ordinary
4-KiB mappings.

`translation-bare` covers stage-1 Bare, G-only, VS-only, and fully Bare
degenerations. It checks that the selected stage is bypassed exactly once and
that no stale page-table walk is required.

`translation-faults` executes valid high-half Sv39/Sv48 loads and both
sign-extension mismatch directions for each mode as scalar load/store page
faults, for ten canonical-boundary transactions. It also covers Sv39x4 and
Sv48x4 GPAs above their architectural limits, a malformed non-aligned Sv39
2 MiB leaf, and 52 PTE encoding cases split evenly across Sv39/Sv48 stage-1
and Sv39x4/Sv48x4 G-stage. Every encoding is exercised by both a scalar load
and a scalar store. Eighty-four additional transactions cross ordinary
stage-1/VS-only/G-stage, Sv39/Sv48, every PTE level, load/store, and the
lowest/highest PPN bits above the 48-bit physical-address width, for 202
architectural transactions before nested-GPA checks. Another 56 cross all four
Sv39/Sv48 VS/G mode pairs, every VS PTE level, load/store, and the lowest PPN
bit outside the selected 41/50-bit GPA plus PPN[43], bringing the total to 258.
The shared
encoding table crosses V/W/R, both ends of reserved bits 60:54, PBMT=3,
disabled PBMTE, exhausted L0, illegal non-leaf U/A/D/PBMT/N fields, and an
invalid NAPOT encoding. It checks the independent walk's failing PTE/level,
exact access- and stage-specific exception, load fault VA/GPA, zero faulting
DCache/Uncache data requests, exact PPN-fault PTW traffic, VS non-leaf marker,
and store SQ conservation. A noncanonical store
may still issue an implementation-dependent PTW request before the DTLB reports
the page fault; PTW request count is recorded as coverage, not correctness.

`translation-permissions` executes 194 independent cases: 16 stage-1 loads, 17
stage-1 stores, 75 two-stage loads, and 86 two-stage stores. Its table-driven
oracle covers Sv39/Sv48 U/S access, SUM, MXR, missing A/D, VS-stage VSUM/VMXR,
and G-stage R/A/D/U behavior for scalar loads and stores. Every nested load and
store permission variant runs under all four VS/G-stage mode pairs. An
additional 42-configuration cross applies X-only with MXR, U=0, and A=0 to the
G-stage mapping of every VS page-table level for both original load and store,
adding 84 fault transactions. It requires the access-specific implicit GPF,
exact faulting VS-PTE GPA, asserted `isForVSnonLeafPTE`, no VS-PTE memory read,
and no data-manager request. Four positive nested stores map every VS
page-table page R-only with W=0/D=0, proving that the implicit PTE read does
not inherit the original store's write permission requirement. Passing stores
require exact post-commit readback; faulting stores
must not reach DCache or Uncache and must retire or be explicitly canceled so
SQ accounting remains exact.

`translation-pbmt` crosses all four Sv39/Sv48 and Sv39x4/Sv48x4 mode pairs
with every VS-stage and G-stage `PMA/NC/IO` combination. The independent oracle
applies the architectural VS-stage override priority, then checks exact load
data, committed store readback, IO commit gating, DCache/Uncache selection, and
LSQ conservation.

`hypervisor-contracts` also translates HLV.D, HLVX.WU, and HSV.D to both sides
of the fixed `0x80000000` PMA boundary. The device-side cases use the last
naturally aligned access of the lower page and require HLV/HSV Uncache routing
plus HLVX `LoadAccessFault`; the DDR-side cases begin at or immediately above
the boundary and require cacheable HLV/HLVX/HSV completion with exact data or
store readback. This checks the physical PMA edge after both translation stages,
not merely through a bare-mode scalar access.

`fp-loads` exercises the separate FP destination-enable path for cacheable and
PBMT=IO 16-, 32-, and 64-bit load widths. The scoreboard requires exact FLH/FLW
NaN-boxing, exact FLD data, and no integer-register write. Cacheable FLH/FLW/FLD
also cross line and page boundaries through the misaligned split path. Page,
PMP access, stage-1 permission, G-stage guest-page, PBMT=NC misalignment, and
denied/corrupt MMIO faults require exact exception metadata while suppressing
both integer and FP register writes. Translation/PMP faults must reach neither
data manager, and every MMIO case must bypass DCache.

`trigger-contracts` programs all four memory-trigger slots through the
top-level CSR interface. A 25-case matrix covers EQ/GE/LT hit and miss
boundaries, enable/load/store/select gating, breakpoint-exception permission,
current-debug-mode suppression, a two-entry chain hit and predecessor miss,
breakpoint and DebugMode actions, aligned/misaligned scalar stores, and
vector load/store crosses over all four addressing modes and EEW values,
including two-field segment operations. Triggered operations must report the
exact action, exception, and vector `vstart`, suppress a breakpoint load's RF
write, and leave store images unchanged. The targeted access must not reach
DCache; a later-element or later-field trigger permits and counts only the
legal prefix refill. Every untriggered cold load must complete normally and
produce a DCache request. DebugMode action retains the incoming RF-enable field
but makes returned data architecturally irrelevant; the scoreboard permits
skipping that data comparison only for this exact action and rejects every
other data-oracle opt-out. LSQ enqueue
metadata (`exceptionVec`, trigger, and `flushPipe`) is driven explicitly on
every transaction; scalar load/store and vector writeback adapters compare the
observable flush, RF-enable, and MMIO/NCIO/perf debug metadata.

The segment field-address cross exposed and now guards the confirmed current-
address timing defect described in
[`docs/CPU_BUG_VECTOR_SEGMENT_TRIGGER_ADDRESS_LAG.md`](docs/CPU_BUG_VECTOR_SEGMENT_TRIGGER_ADDRESS_LAG.md).

`metadata-contracts` drives non-default RVC/FTQ/store-set/load-wait values on
the scalar issue interface and completes a load. The top-level `issueLda`
contract does not expose `exceptionVec`; LSQ retains enqueue exception bits for
its internal exception machinery, while page/access/guest-page faults are
recomputed from the TLB in S1. The generated enqueue adapter is covered by
unit tests for exception-vector bit mapping.

`single-load` drives two cold scalar loads whose virtual-address bit 5 values
are opposite. The DCache must emit one ordinary and one `isKeyword`
AcquireBlock, accept both 32-byte GrantData beat orders, return two exact
64-bit values, and emit one GrantAck for each refill. An independent phase
co-issues two offsets from one cold line while holding its GrantData response
for 128 cycles; both loads must complete from one merged AcquireBlock. A third
phase delays only the noncritical beat of an `isKeyword` refill by 128 cycles.
The target load must write back after the critical beat, before the second beat
arrives, and the eventual line drain must not duplicate that writeback. Five
additional loads drive `src + imm` with immediates 0, 1, 2047, -1, and -2048,
including cache-line crossings, while the independent oracle remains keyed by
the final effective address.

`load-feedback` observes all three backend scalar-load wakeup lanes and all
three `ld2Cancel` pins. It checks issue-time `rfWen`, `fpWen`, and `pdest`,
forces a cold miss on every lane, then repeats the accesses from resident cache
lines without a new TileLink request. The oracle permits any number of legal
replays but requires each completed normal load to have exactly one uncanceled
wakeup: `wakeup_delta = ld2Cancel_delta + 1`. It also issues three resident,
different-set loads to the same DCache bank in one cycle. That phase requires
only exact identity-matched data plus one writeback and LQ dequeue per accepted
load; its wakeup/cancel counts and manager traffic are diagnostics because
bank arbitration and prefetch activity are implementation choices. Independent
translated cases classify three more causes: an Sv39 load-page fault must
cancel every speculative wakeup without sending a DCache request, while
PBMT=IO MMIO and PBMT=NC loads must each retain exactly one uncanceled wakeup,
issue one Uncache request, and issue no DCache request. The MMIO case also
performs the backend `pendingMMIOld` handshake before completion. A forwarding
case issues a younger load after the matching store address but before its
data; it requires a pre-data cancellation, then exact store-data forwarding
with one final uncanceled wakeup and no DCache request after line warmup. A
PMP-denied S-mode Bare load must cancel every speculative wakeup while issuing
no PTW, DCache, or Uncache request.

`topdown-contracts` checks every MemBlock top-down output semantically. An
independent bit pattern proves the exact one-cycle L2/L3 miss delay. A cold
load with a delayed refill must assert both L1-miss and replay-allocation, 56
unissued stores must reach StoreQueue full, and 16 distinct committed lines
held behind a delayed refill must reach SBuffer full.
`l2-flush-contracts` checks the separate outer-L2 control bridge. The CSR
enable must pass through combinationally, while every L2 completion input must
appear at the backend bypass exactly one cycle later. The timing oracle remains
active during every monitored functional-test cycle; the focused test covers
all four enable/completion combinations and repeated rising/falling edges.
`top-control-contracts` checks the remaining simple top-level control bridges.
Hart ID and power-down enable are combinational; reset vector, CPU halted, and
CPU critical error must appear exactly one cycle later. The focused test covers
all eight power/halt/error combinations plus eight independent 6-bit hart-ID
and 48-bit reset-vector patterns. It also checks combinational MSI-ack and
frontend-reset bypasses, one-cycle MSI-info/CLINT/BEU metadata, all 67
observable hardware-counter event lanes, and the full two-cycle L2 prefetch
control bundle. Two hundred fifty-six bridge patterns exhaust all eight
interrupt-sink input bits, including the BEU-local/NMI0 OR mapping, while also
covering every combination of the five prefetch-control enables, all four
MSI/CLINT valid combinations, zero/maximum delay, and all 64 event values.
These timing oracles also remain active in every monitored functional scenario.
`trace-bridge-contracts` checks the 45-port trace bypass as a transport
contract. It verifies one-cycle encoder enable/stall feedback; unconditional
one-cycle group valid/type/retire-count and `mstatus`; valid-gated address,
last-size, and privilege holds; 50-bit `iaddr + (ftqOffset << 1)` arithmetic;
and trap-only cause/tval updates for Exception and Interrupt itypes. Thirty-two
patterns cover every three-group valid combination, all 16 itypes and FTQ
offsets, all eight privilege encodings, both trap classes, and hold intervals.
This checks the MemBlock bridge, not architectural trace generation.
`dft-bridge-contracts` holds the functional design in external reset while
exhausting all 1,024 combinations of the seven SRAM-broadcast and three
DFT-reset input bits. It checks the ten elaborated frontend outputs and four
backend outputs combinationally, restores the idle DFT values, then performs a
fresh functional reset. This checks MemBlock routing only; MBIST, scan, SRAM,
and physical SRAM behavior remain integration responsibilities.
`reset-tree-contracts` checks the observable `io_reset_backend` reset-tree
contract in a short standalone run. Three functional-reset pulse widths must
assert asynchronously and release after exactly two three-stage ResetGen
levels. DFT functional mode must isolate external reset and release after one
three-stage level, while scan mode must directly follow active-low
`lgc_rst_n`. The scenario restores functional DFT controls and performs a fresh
reset before checking idle behavior.
`random-mixed` keeps constant-space lane counters for diagnostics, but wakeup
and cancel presence or totals do not close the architectural regression. A
split/replayed load or hardware prefetch may change those pulses while the
identity-matched terminal load and queue behavior remains legal.

`iq-slow-feedback` records every valid STA and VSTU slow-feedback pulse with
its lane, cycle, hit result, queue identity, and vector replay fields. It
co-issues two cold translated scalar stores and requires same-cycle TLB-miss
feedback on both STA lanes, warms both translations, and then requires
same-cycle hit feedback with exact SQ identities. Two masked-off vector stores
exercise both VSTU lanes in one cycle with exact LQ/SQ identities and zero
partial-replay metadata. A separate misaligned strided vector store requires a
blocked VSTU response with an in-range SQ flow, exact LQ identity, a nonzero
partial replay mask, and a reusable merge-buffer index.

`memory-violation` first proves that same-line, byte-disjoint load/store traffic
does not redirect. It then leaves an older store address unresolved, completes
three younger same-byte loads whose ROB identities cross the 159-to-0 wrap,
and resolves the store address. It requires exactly one RAW replay redirect,
selects the oldest of the three loads across the circular pointer boundary,
and compares every exposed `memoryViolation` field against that load's
independently chosen ROB, FTQ, RVC, and flush-level metadata. A vector-load
flow is then completed behind an unresolved scalar store and must produce the
same exact RAW redirect contract. Finally, both scalar store-address lanes are
accepted in one cycle so that they produce distinct rollback candidates; the
candidate on lane 1 is intentionally older, proving the top-level arbiter uses
ROB age rather than port priority.
`rar-violation` enables the architectural load-load check, lets a younger load
complete first, drains an older store so the line is dirty, and forces a DCache
writeback/release with a Probe before issuing the older load. It requires the
RAR redirect to identify the older load and use `flushAfter`, unlike RAW's
self-flush. It then issues three older queries on different DCache banks in one
cycle after three corresponding younger loads have observed the same released
line. Their ROB identities cross 158, 159, and 0; the redirect must select 158
by circular ROB age rather than LoadUnit priority.

`ifetch-prefetch` drives software instruction-prefetch operations through all
three scalar load-unit lanes and checks the exact per-lane virtual-address
output to the frontend. Read/write data-prefetch operations are negative
controls: they must complete without generating an instruction-prefetch pulse.
The five initial instruction/data operations cross signed immediates 0, 1,
2047, -1, and -2048 and check the final `src + imm` address.
An independent Sv39 phase dispatches and issues three unmapped `prefetch.i`
requests in one cycle and requires one exact VA on each lane with no PTW or
DCache request. This reflects the RTL ownership boundary: LoadUnit marks
`prefetch.i` as `s0_tlb_no_query`, and the frontend performs any later fetch
translation. Two further batches mix `prefetch.i/r/w` across all lanes. Cold
unmapped data hints must complete without PTW or data-manager traffic. The
mapped batch first warms two TLB entries through ordinary loads, then requires
data-prefetch DCache traffic with no new PTW request; separate cold-line
`prefetch.r` and `prefetch.w` operations each require their own request. Data
prefetches must never emit an IFU-side pulse. A memory-type phase warms two
PBMT=NC and two PBMT=IO pages with ordinary loads and observes four Uncache
requests. With those TLB entries resident, NC `prefetch.r/w` follow the RTL's
intentional cache-hint policy and each issue one DCache request with no Uncache
request; IO `prefetch.r/w` are dropped before either data manager. The
`random-mixed` coverage schema also requires at least one observed
instruction-prefetch output in every seed.

`hardware-prefetch` drives fixed-PC stride, PC-varying spatial, and same-PC SMS
shapes while the ordinary load scoreboard checks every architectural result.
It records L2/L3 source, address, and count behavior for implementation and
performance characterization. Exact confidence thresholds, lookahead depth,
source selection, arbitration, and request cardinality are deliberately not
correctness gates. The scenario does retain the explicit output-disable
contract: after the relevant prefetch control is disabled, later traffic must
not emit that class of request. Direct AGT request generation and L3 enablement
remain build/integration properties rather than architectural claims.
The same fixed-PC stream shape is available through the common `random-mixed`
`stride-stream` constraint. It runs alongside the configured operation,
translation, miss/refill, Probe, and response-latency mix; L2 source/address
observations are reported for characterization and are not a PASS gate.

`dcache-errors` first injects one response-wide denied and one response-wide
corrupt DCache refill and checks the corresponding scalar load access-fault and
hardware-error writebacks with RF writes suppressed. Its backend feedback
oracle independently requires nonzero cancellation for each error and no
surviving normal wakeup. The same TileLink agent encodes every denied data
response with `corrupt=1` and rejects corrupt injection on data-less `Grant`,
matching the legal D-channel contract. The agent also rejects reuse of an A
source before its final D beat, measures maximum outstanding A requests, and
distinguishes an empty D-response queue from completed GrantAck obligations.

A separate six-case matrix crosses both critical-beat orders with denied on
both beats, corrupt on only the first beat, and corrupt on only the last beat.
The second beat is held for 256 cycles. During that gap, an opposite-half load
merges into the same MSHR and an unrelated cold load must allocate a second
outstanding MSHR. Every case requires exactly two AcquireBlocks, four
GrantData beats, two GrantAcks, exact per-beat error polarity, a faulting
resident hit on the poisoned line, and an exact zero-request hit on the healthy
line. Last-beat-only corrupt deliberately permits the primary critical-beat
load to finish with exact data before the unrelated half arrives corrupted.
Loads that see the error directly in stage 2 cancel their early wakeup; loads
that learn it later from installed line metadata instead rely on the terminal
exception writeback (`rfWen=0`) and redirect in this
`EnableAccurateLoadError=false` build.

The all-wakeups-canceled rule is also checked by `scalar-guest-fault` for a
G-stage fault
whose VA, GPA, and VS-non-leaf classification are independently modeled. The
scenario also fills two lines, waits for every refill beat and GrantAck, proves
both targets are resident with zero-request hits, and programs the
`L1DCacheCtrl` MMIO registers for one-shot tag and data ECC injection. The tag
path covers bank 0 with representative single- and double-bit masks; each case
must report the exact physical address to BEU, cancel its speculative wakeup,
produce no terminal writeback or new manager request, and permit a clean load
after redirect. The data path crosses all eight banks with spread single-bit
and adjacent double-bit masks. With this configuration's explicit
`EnableAccurateLoadError=false`, every data case must report the exact address
to BEU without cancellation and write back the independently predicted XOR
result; disabling injection must restore clean data. A separate same-cycle
pair injects bank 2 while a bank-5 companion remains clean, checks both load
lanes independently, and proves one-shot auto-clear through clean survivors
without an explicit disable write. The phase finishes only when all 19
injected cases have reported and both LQ and SQ satisfy
`allocated = dequeued + canceled`.

`dcache-coherence` fills a clean line, invalidates it with a manager Probe,
refills it, requests clean ProbeAckData, refills it again, dirties the line,
and requires dirty ProbeAckData even when the manager did not explicitly ask
for data. Every returned byte is compared with the independent line image and
written into bus memory before a post-probe cold load. The agent also assigns
and checks every Grant/GrantData sink on E-channel GrantAck and forces at least
one E-channel stall to check payload stability. A separate phase holds an
unrelated cold refill open while two clean resident lines receive different
Probe B-source IDs. Both B requests must be accepted before the cold load writes
back, the measured Probe depth must reach two, and both address-matched C-channel
responses must complete. C-source is intentionally not equated with B-source:
the DCache WritebackQueue allocates the former independently.

`ifetch-ptw-bridge` directly drives the IFU-originated PTW request across the
MemBlock top-level boundary. It checks valid and invalid Sv39/Sv48 stage-1
walks, PBMT=NC/IO leaves, all four Sv39/Sv48 x Sv39x4/Sv48x4 nested walks,
both Sv39/Sv48 forms of the VS-only and G-only degenerations, and all four
nested mode pairs crossed with VS-leaf, final-G-leaf, and implicit VS-page-table
G-stage faults. Six additional cases place `G` in stage-1 and G-stage leaf and
non-leaf PTEs. Stage-1 `G=0` is never allowed to become global, while a true
global mapping may be conservatively cached as ASID-specific; the observed
reported/demoted split is recorded. G-stage `G` is allowed to appear as raw
internal response metadata, but must not change translation, permissions, or
fault results because the architecture requires hardware to ignore it. It
reconstructs translated PPNs from the sector response, checks the active
stage's ASID/VMID, permissions, PBMT, fault level, and faults, requires a cold
PTW walk, and holds response ready low to verify stable payload under
backpressure. An eight-page Sv39 matrix exercises every sector index in one
leaf-PTE cacheline. Three deliberately interleaved PPN-high groups require
exact `valididx` masks `0x29`, `0x42`, and `0x94`, all eight selected
`ppn_low` fields, exact one-hot `pteidx`, and exact reconstructed PPNs. Its
first cold request requires three external PTW reads; the other seven sector
requests must add none. A
separate overlap case delays the IFU root PTE response for 256 cycles, issues a
cold scalar DTLB miss before retiring the IFU response, requires both leaf PTE
addresses and a PTW manager outstanding depth of at least two, and checks the
load writeback against the independent memory oracle. A duplicate case accepts
two same-VPN IFU requests before the delayed first response, requires two
identical responses, and proves they coalesce by observing only the three
memory requests of one cold Sv39 walk. Delayed-walk races then change the
stage-1 or nested root, mode, ASID, or VMID, or issue global and selective
`SFENCE.VMA`, `HFENCE.VVMA`, and `HFENCE.GVMA`. Each race requires a 1,024-cycle
quiet window with no stale response before an independent cold walk returns
the exact replacement stage-1 and G-stage mappings.
This is separate from the data-side TLB translation tests.

`ptw-errors` injects TileLink `denied` and `corrupt` on selected PTW block
responses. Sixteen cases cross Sv39/Sv48 stage-1, isolated Sv39x4/Sv48x4,
fully nested walks, bitmap reads, load/store, and root/intermediate/leaf
locations. The eight independent corrupt cases split evenly between first- and
last-beat corruption; denied remains fixed across the multibeat response and
also asserts corrupt as required for a TileLink data response. Each case
requires the original access fault, exact walk cutoff, and no DCache/Uncache
access. It then applies the matching stage-1 or hypervisor fence and requires a
clean same-address retry, an exact reread of the failed block, correct data, and
one DCache request. This distinguishes a permitted L1 fault-result entry from
forbidden refill of bad response data into the lower page-table caches.
Schema 19 lifts the same address-qualified oracle into `random-mixed`, with
weighted coverage over all five host/G/nested walk sites and every supported
translation-mode pair.

`uncache-errors` injects denied and independent-corrupt Uncache load responses
plus a denied store response. Loads check the exception contract through the
PBMT=NC adapter. The store also requires exactly one external Uncache error
report at the 64-byte-aligned physical address, no DCache error report, exact
exception metadata, SQ conservation, and unchanged initialized backing bytes.
Denied load data asserts `corrupt` as TileLink requires; corrupt injection on a
data-less store `AccessAck` is rejected as protocol-illegal. This test caught
and now
guards the LoadUnit S1 path that previously discarded response-generated
exception bits. MMIO uses a distinct S0-to-three-cycle metadata bypass and is
not implicated by this reproducer. The complete reproducer and root-cause analysis are in
[`docs/CPU_BUG_UNCACHE_DCHANNEL_ERROR.md`](docs/CPU_BUG_UNCACHE_DCHANNEL_ERROR.md).

`store-forwarding` crosses SB/SH/SW/SD forwarding with signed scalar-store
immediates 0, 1, 2047, -1, and -2048. Each following load checks the exact
forwarded bytes at the final effective address before external store drain.

`uncache-widths` exercises all seven scalar load opcodes at every legal byte
lane for 8-, 16-, 32-, and 64-bit Uncache transfers. The manager returns the
complete 8-byte beat, checks the generated size/address/mask contract, and
applies deterministic request and response backpressure before the scalar
scoreboard checks sign/zero extension.

`mmio-contracts` maps a page as PBMT=IO and checks the MMIO load's
response-independent ROB classification path: the load must bypass DCache,
report `isMMIO=1` and `isNCIO=0`, and preserve denied/corrupt response
exceptions through final writeback.
The harness independently samples all three ROB-facing `loadMmio` pulses and
their ROB indices. Single requests from each load-unit input prove that the
sorted interface compacts one valid request into output slot zero; a bare-mode
same-cycle PMA-MMIO triple permutes input lanes against ROB order, then proves
all three compacted output slots pulse together in exact ROB order. Normal,
denied, and corrupt PBMT=IO loads delay their external response
by 64, 128, and 256 cycles and require the classification pulse before the
Uncache request completes. A cacheable load plus PBMT=NC control must produce no
`loadMmio` pulse while still selecting DCache and Uncache respectively.
It also cold-misses and reissues a scalar PBMT=IO store, requiring one Uncache
Put, no DCache request, exact store writeback metadata, and SQ retirement. The
PBMT=IO page is backed by the DDR PMA region, so the store writeback's
`debug.isMMIO=0` denotes `memBackTypeMM=1`. A second bare-mode pair accesses the
SoC's non-DebugModule `c=0` PMA interval and requires physical PMA MMIO
classification for both load and store. The same scenario accesses the guarded
DebugModule PMA interval outside Debug Mode, requires an exact load access fault
with no manager request, and proves every speculative load wakeup is canceled.
Two exact bare-mode loads straddle the `0x80000000` PMA boundary: the last
aligned 64-bit device access below it must use Uncache, while the first DDR
access must use DCache.
The Uncache manager also provides a configurable side-effecting device window
with a structured request log. A seven-access bare-mode sequence first injects
denied and corrupt reads and proves that neither clears the device register,
then injects a denied write and proves that it changes no byte. Corrupt is
forbidden on the write's data-less `AccessAck`. Normal reads still implement
read-clear, and a successful 32-bit write at
byte offset four has the exact TileLink size, mask, and replicated bus data.
The final read returns that partial write. The log enforces request order,
response flags, no duplicate requests, and DCache bypass.
A separate phase issues three same-address device loads into the LQ before
allowing the first request. It holds the first two responses for 512 and 128
cycles and proves that MMIO stays serialized at one external request in flight
even when Uncache outstanding mode is enabled. The read-clear results must be
the initial value followed by two zeros.
A mixed phase pre-issues `load -> SW -> load` into the LQ/SQ. Two 256-cycle
response delays prove that the younger store and final load cannot reach the
device early. The exact device log must read and clear the initial value, write
only the selected high four bytes, then return and clear that stored beat.
`cbo-zero-contracts` drives the `CBO.ZERO` encoding through
the cacheable StoreQueue/SBuffer wline path under randomized DCache
backpressure, checks exact writeback metadata, and reads the resulting line
back before updating the reference mirror.

`cmo-contracts` models the custom DCache TileLink CMO exchange end to end.
It checks line-aligned size-64 `CBO.CLEAN`, `CBO.FLUSH`, and `CBO.INVAL` A
requests on fixed source 17, delays `CBOAck` by 1024 cycles, and completes the
manager-derived Probe first. The dirty cases leave a committed store buffered,
apply no direct flush, and require CMO execution to drain it before the request
and resulting Probe. Every operation is crossed with clean and dirty line
state: dirty transitions require byte-exact ProbeAckData, clean transitions
require no data, CLEAN retains a Branch line, and FLUSH/INVAL invalidate and
force a checked refill. Every operation is also crossed with denied and
independent-corrupt `CBOAck`, exact store-access-fault or hardware-error
writeback, `flushPipe=1`, no Uncache traffic, unchanged bus memory on failure,
and SQ conservation. A separate overlap holds `CBOAck`, accepts a younger cold
load into another MSHR, applies the CMO writeback's legal `flushAfter`, and
requires one exact LQ cancellation with no writeback from the delayed D beats.
This scenario found the confirmed RTL defect documented in
[`docs/CPU_BUG_CMO_DCHANNEL_ERROR.md`](docs/CPU_BUG_CMO_DCHANNEL_ERROR.md).

`reset-recovery` separately asserts reset with an accepted DCache refill, PTW
walk, and Uncache/MMIO request held outstanding by a 256-cycle response delay.
The harness synchronously resets the corresponding tile-local manager model,
accounts each canceled queue entry, changes the post-reset address or page-table
root, and requires three survivor loads to return the new data. Any stale
pre-reset manager response or architectural writeback fails the scenario.

`frontend-reset-recovery` accepts one request on each of the ICache,
instruction-Uncache, and ICache-control A paths while all three downstream
consumers are stalled, verifies their stable buffered payloads, then resets the
bridge and rejects any stale request. It separately buffers an ICache-control D
response behind upstream backpressure and proves reset removes it. Three
distinct post-reset requests must then emerge exactly once before a final clean
reset. The other two D paths have no top-level consumer-ready input and
therefore do not claim an unobservable stalled-response reset contract.

`scalar-misaligned` checks scalar splits within a 16-byte beat and across cache
lines and translated pages. Its queue-pressure phase first leaves three split
loads pending at the LQ/ROB head, then completes 60 younger aligned loads on
all three load lanes so their RAR state remains live behind the unfinished
head. The test advances `pendingPtr` through the three split loads in program
order and requires all 63 exact data writebacks, complete LQ drain, and no
spurious load-load violation.

`exception-contracts` checks exact scalar load exception bits, RF-write
suppression, software-prefetch fault suppression, and PBMT-NC misalignment. It
also reverses queue order against program order: three simultaneous load page
faults cross ROB 159-to-0 while LQ indices increase in the opposite direction,
and two simultaneous PBMT-NC misaligned stores cross the same ROB boundary
while SQ indices disagree. The final top-level exception VA must identify the
oldest ROB in both cases, independently of LQ/SQ index order. With both load
and store exception buffers populated, the test then switches
`isStoreException` in both directions and checks the exact retained address
after the two-register output path. A separate LMUL=2 vector pair shares one
ROB identity, issues `vuopIdx=1` before `vuopIdx=0`, and requires the older uop
to replace the younger uop's retained page-fault VA. A page fault and a
PBMT-NC misaligned fault then arrive together with queue order opposed to ROB
age. Two further pairs retain a younger scalar or vector page fault first and
require the later-arriving older fault from the other source to replace it.
Two store/vector pairs then populate the load and store exception buffers in
opposite arrival orders. Keeping the first source selected must preserve its
VA after the second fault arrives; toggling `isStoreException` in both
directions must return the exact vector-load or scalar-store page-fault VA.

`vector-addressing` checks exact unit-stride, strided, indexed-unordered, and
indexed-ordered data plus vector-store readback. Its ordinary LMUL=2 phase
drives two uops per instruction for unit-stride, negative-stride, and ordered
indexed loads. Unit/strided younger uops issue first; indexed uops carry
independent index vectors and obey ordered acceptance. Cross-uop masks and a
tail element are selected by global element number. All six writebacks are
checked against uop-aware addresses from the independent byte-memory model.

The same scenario now exhausts the legal ordinary unit-stride and strided
configuration spaces for ELEN=64: all four EEWs, all four SEWs, and LMUL from
fractional 1/8 through 8 are filtered by `LMUL >= SEW/ELEN` and
`EMUL = EEW/SEW * LMUL` remaining in fractional 1/8 through 8. The 78 legal
configurations per addressing mode generate 202 load uops, 202 store uops, and
202 readback uops. Strided cases alternate positive and negative strides.
The driver carries SEW independently from EEW, checks returned
`vsew/veew/vlmul`, and mirrors Rename's conservative allocation of two LSQ
flows per unit-stride uop plus `GenRealFlowNum` for strided uops. A reusable
instruction-window driver allows legal instructions larger than the queue's
conservative enqueue watermark to make progress without changing their ROB
identity. The run requires exact retirement of 808 LQ/404 SQ unit-stride and
2,304 LQ/1,152 SQ strided entries across queue and ROB pointer wraps.

An additional indexed matrix crosses ordered/unordered addressing with every
EEW for load, store, and load readback. Direct loads duplicate one index to
check aliasing; stores use a non-monotonic permutation without ambiguous
same-address writes. The sparse-memory oracle checks addresses spread across
cache lines and a Bare 4-KiB boundary, then requires all 120 LQ and 60 SQ
allocations to drain across queue-pointer wraps.

The indexed LMUL/EMUL phase separately exhausts all 78 legal ELEN=64
EEW/SEW/LMUL/EMUL configurations for each of ordered and unordered addressing.
It derives uop count from `max(LMUL, EMUL)`, applies the two
`GenRealFlowNum` branches, reuses index-register contents when LMUL exceeds
EMUL, and models the split offset plus shared destination register when EMUL
exceeds LMUL. Ordered uops issue in program order while unordered uops issue in
reverse order. Exact load/store/readback checks cover 1,016 load and 508 store
uops, including 56 special-index configurations, and drain 4,608 LQ plus 2,304
SQ allocations across ROB and queue wraps.

The same scenario also models `vlr`/`vsr` as distinct whole-register
operations and executes all 16 combinations of one/two/four/eight registers
and EEW 8/16/32/64. It deliberately supplies `vl=1`; the independent oracle
instead derives EVL as `(nf + 1) * VLENB / EEW`. Each register uop uses its
own 16-byte address window, exact data and metadata are checked, stores are
flushed and read back through whole-register loads, and 240 LQ plus 120 SQ
allocations must drain exactly across pointer wraps.

`pmp-contracts` drives `pmpaddr0..1` and packed `pmpcfg0` writes through the
MemBlock distributed CSR boundary. Hand-calculated TOR and NAPOT regions check
both exact edges, first-match priority, R/W plus AMO write denial, and absence
of DCache/Uncache traffic for every rejected operation. Separate M-mode
environments distinguish unlocked bypass from locked enforcement and prove
that later address/config writes cannot change a locked entry. Because
`PlatformGrain=12`, the requested NA4 encoding is checked for its specified
WARL conversion to a minimum 4-KiB NAPOT region rather than claimed as NA4.

`atomic-contracts` drives all currently exposed W/D-width AMOs, AMOCAS.W/D, and
LR/SC through the atomic store-address/data ports, checks old-value writeback,
compare success/failure, reservation success/failure, and then verifies cache
visibility through ordinary scalar loads. Its alignment matrix crosses all 24
exposed W/D LR, SC, AMO, and AMOCAS encodings with every illegal byte offset:
36 W-width plus 84 D-width cases. LR must report `loadAddrMisaligned`; all
other operations must report `storeAddrMisaligned`. Every case suppresses
exceptional `rfWen`, adds no DCache request, and crosses the 160-entry ROB
pointer boundary. A bare-mode `AMOADD.D` at the last naturally aligned device
address below DDR checks the fixed PMA `atomic=0` attribute:
it must report `StoreAccessFault`, preserve memory, and reach neither data
manager. Atomic uops are intentionally not counted as LSQ entries because the
RTL routes them through `AtomicsUnit`.

`atomic-dchannel-errors` injects denied and corrupt responses into cold misses
for all 22 refill-capable W/D operations: LR, the nine AMO ALU operations, and
AMOCAS. Every operation checks the exact exception class, suppressed `rfWen`,
and the expected miss/hit request-count delta. Both denied and corrupt refills
are installed as poisoned lines; a later scalar load must hit without new
traffic and re-report `loadAccessFault` or `hardwareError`. SC.W/D checks the
same cached error metadata after LR.W/LR.D, and clean AMO/readback sequences
after each error-kind batch guard AtomicsUnit error lifetime. Exceptional data
is deliberately not compared because it is non-architectural. SC itself cannot
receive a cold-miss D response in this implementation: a missing line or usable
reservation makes MainPipe return SC failure before issuing TileLink traffic.


`vector-guest-fault-split` is the deterministic regression for the historical
VS-non-leaf GPA bug. Before the RTL fix it reproduced `0x94001808` instead of
the independently walked `0x94001800`; the repaired RTL must report
`0x94001800`, and `known-bug-sentinel` checks that repaired state explicitly.

The first Picker build compiles the Verilated model. Subsequent targets reuse it
and only rebuild the small C++ harness when its sources change.

## Random Regression

The normal regression includes the focused scalar/vector random scenarios,
`random-mixed`, and `frontend-bridge`. The bridge scenario gives every seed
independent concurrent ICache, ICache-control, and instruction-Uncache traffic;
its offline acceptance gate checks exact request/response counts, field-check
volume, request/response stalls, and legal source-credit stalls.
`random-mixed` is one configurable generator: realistic, balanced-coverage,
and corner-heavy directions are selected with the `spec`, `coverage`, and
`corner` constraint presets, and every field can be overridden without adding
another scenario implementation. See
[`docs/CONSTRAINED_RANDOM.md`](docs/CONSTRAINED_RANDOM.md) for the complete
interface, preset values, SPEC counter calibration, and coverage gates.
The mixed scenario keeps heterogeneous transactions outstanding in one
simulation. Its constrained-random tail enqueues scalar load, scalar store,
vector load, vector store, and software prefetch in the same rolling window,
can add legal NC/MMIO load overlap, then randomizes issue order, store
address/data order, and vector mode before a bounded drain. Atomic traffic stays
in the same generator but is issued as a serializing action because MemBlock's
LR/SC/AMO path blocks the load pipeline while active. The generator constrains
AMO/LRSC/AMOCAS family, W/D width, D-channel error presence/kind, and atomic
Probe-burst depth, CMO
CLEAN/FLUSH/INVAL operation, clean/dirty line state, younger-load overlap, and
legal CBOAck error presence and kind, hypervisor PBMT/PMA/PMP relation,
NC/MMIO load/store direction plus legal
Uncache response-error
presence and kind,
PTW manager-error site, walk level, access direction, and denied/corrupt beat,
same-line scalar-load merge depth and same-address/same-beat/cross-beat pattern,
same-set clean/dirty pressure depth, access width, and set-index quartile,
Bare/Sv39/Sv48 and all four nested VS/G mode pairs, host-stage NAPOT plus
independent nested VS/G NAPOT placement, translation switch and legal fence
kind/scope, manager Probe rate/toB/need-data/overlap crosses, and DCache, PTW,
and Uncache latency independently. `stride-stream` also controls fixed-PC cold-load
training pressure on the L1 stride prefetcher. It includes
simultaneous scalar/vector issue, every scalar width, every vector EEW and every
load/store address mode independently, all legal ordinary vector
EEW/SEW/LMUL/derived-EMUL shapes, 1..8-uop expansion with capacity-bounded
queue windows, scalar/vector misalignment, software
`prefetch.i/r/w`, both cross-forwarding directions, randomized cold/warm
translation, a vector guest-page fault with exact VA/GPA metadata, PBMT=NC,
clean/dirty same-set replacement, redirect/reallocation, and randomized DCache/PTW/
uncache backpressure. Every seed drives all six LSQ dispatch lanes and widths,
checks committed scalar/vector stores through architectural readback, validates
dirty ReleaseData before updating the separate bus memory, and meets bounded
coverage plus final LSQ-accounting gates. The deterministic coherence scenario
covers its directed coherence state sequence. The random tail can follow a
completed dirty scalar store with a byte-exact manager Probe, independently
crosses toB/toN and requested/mandatory data, and cleans up retained toB lines.
The Probe overlap classes hold an unrelated refill open while zero to seven
clean auxiliary Probes and a dirty primary Probe with distinct B-source IDs are
queued together. The manager holds C unready until every selected B request is
accepted, then address-matches every response and requires the exact selected
accepted-outstanding depth from one through the configured eight-entry queue
before the delayed load writes back. Wider operation-class overlap remains
follow-on coverage. Across sequences, schema 34 walks the complete six-bit
DCache B-source space, permits an ID to reappear only after its prior response
has completed, and records every 63-to-0 wrap at the accepted B boundary.

For example, these commands run the same generator in two directions:

```sh
make random-mixed PICKER="$PICKER" SEED=1 TRANSACTIONS=65536 \
  CONSTRAINTS=spec
make random-mixed PICKER="$PICKER" SEED=2 TRANSACTIONS=32768 \
  CONSTRAINTS=corner \
  CONSTRAINT='translation-nested=250 translation-switch=750 tlb-flush=200 concurrent=750 atomic-lrsc=20 mmio-store=400 cmo=125 cmo-younger-overlap=750 stride-stream=750 ptw-latency=spec'
```

`extended-regression`, `final-regression`, and `long-final-regression` default
to `spec`; boundary hunts default to `corner`. `CONSTRAINTS` and `CONSTRAINT`
override those defaults. The ordinary `regression` and direct `random-mixed`
target default to `coverage` when no preset is supplied.

`random-stress` is the high-pressure constrained-random campaign. Each burst
builds one or two groups before any drain. A group contains independent and
forwarding scalar loads/stores, vector loads/stores, and a prefetch; issue order
is randomized subject only to the actual forwarding dependencies. The stress
driver checks byte overlays for younger scalar/vector loads, all scalar widths,
all vector EEWs, unit/strided/indexed-unordered modes, mask/vstart/vl shapes,
misaligned scalar addresses, both vector lanes, two cache regions, DCache
backpressure, and final LSQ conservation. Strided forwarding stores use
non-overlapping positive or negative strides; zero-stride loads remain covered
by the independent random-mixed vector-load phase because repeated-address
stores do not have a single deterministic forwarding order. Its coverage gate
uses independent SplitMix64-derived streams for traffic, shape, payload, and
scheduling, preserving exact seed replay while decoupling coverage dimensions. It
also requires at least ten simultaneous outstanding scoreboard entries and four nonzero
feature crosses derived from generated burst fields. Ordered-indexed vector issue remains in the
`random-mixed` baseline because the DUT requires older LSQ retirement before
that operation can be accepted.
This separate entry point is retained for compatibility with its historical
artifacts and burst-specific acceptance checks. New workload directions belong
in the common `random-mixed` constraint interface.

Schema 10 gives ordinary vector loads and stores one shared shape interface:
`vector-{unit-stride,strided,indexed-unordered,indexed-ordered}`,
`vector-eew{8,16,32,64}`, `vector-sew{8,16,32,64}`, and
`vector-{lmul,emul}-{mf8,mf4,mf2,m1,m2,m4,m8}`. Illegal or unreachable
combinations are rejected before cycle 0. Each action represents one complete
vector instruction even when it expands into multiple uops; the terminal
summary reports both instruction and uop counts and the offline verifier checks
both enabled load/store directions, every enabled shape class,
disabled-class zeros, and per-dimension conservation.

Schema 11 adds independent per-mille controls for `vector-masked`,
`vector-vma`, `vector-vta`, `vector-partial-vl`, and
`vector-nonzero-vstart`. Zero and 1000 are strict fixed values; intermediate
values require both classes in every accepted seed. The generator makes masked
body and tail elements observable when their corresponding agnostic policy is
enabled, while the scoreboard classifies each inactive byte from the uop's
global element number and accepts only the architectural retained/all-ones
outcomes. A fixed policy combination that cannot coexist with an enabled shape
is rejected before cycle 0.

Schema 12 adds `translation-stage1-napot`, `translation-vs-napot`, and
`translation-g-napot`. The first selects ordinary 4-KiB versus legal 64-KiB
Svnapot leaves for host translation; the latter two independently select the
VS-stage and G-stage leaves, producing all four nested leaf topologies. Each
NAPOT topology uses a distinct aligned work region, is checked by the
independent page-table walker before traffic starts, and has an observed
per-seed coverage gate. Zero and 1000 fix a stage to ordinary or NAPOT leaves;
intermediate values require both enabled classes.

Schema 13 adds CMO CLEAN/FLUSH/INVAL, clean/dirty line state, and optional
younger delayed-refill cancellation to the common operation constraints.
Schema 14 adds `probe-overlap`: zero and 1000 strictly disable or require the
class, while intermediate values require both classes. The terminal record and
offline verifier conserve the auxiliary Probe and require maximum Probe depth
two whenever overlap is observed.
Schema 15 adds `cmo-error` and `cmo-error-denied`. Error presence and kind are
independent per-mille constraints; every enabled CLEAN/FLUSH/INVAL x
corrupt/denied cross is required per seed. The CMO-specific injector matches
both line address and CBO opcode so a preceding same-line refill or permission
upgrade cannot consume the error. Error completion requires exact
HardwareError or StoreAccessFault, no manager Probe, unchanged backing memory,
and redirect cleanup, including an optional younger delayed load.

Schema 16 adds `uncache-error` and `uncache-load-error-denied`. Every enabled
NC/MMIO load direction must cover clean, denied, and independent-corrupt
responses; stores cover clean and denied because TileLink does not permit
`corrupt` on a data-less `AccessAck`. The oracle preserves the architectural
split between the two store classes: a PBMT=IO denied response must produce a
final StoreAccessFault writeback, while a committed PBMT=NC denied response is
reported only through `uncacheError`, must retire its SQ entry without a
redirect, and must not update manager-side memory. Error-response and D-beat
counters are conserved against the 12 NC/MMIO x load/store x outcome bins.
When Uncache uses the `spec` latency profile, the serial budget forces at least
four requests so every latency bucket is reachable even under extreme
load-only or store-only constraints.

Schema 17 adds `dcache-load-error` and `dcache-load-error-denied` for weighted
scalar-load actions. Error actions use a nonrepeating cold-line region mapped
in Bare, Sv39, Sv48, and all four nested mode pairs, then require exact
HardwareError or LoadAccessFault, self-redirect cleanup, unchanged backing
memory, two corrupt D beats, denied on both beats only for the denied class,
and one errored refill/GrantAck. Errored GrantAcks are attributed by sink, so a
simultaneous clean hardware-prefetch refill remains legal without weakening
the error transaction oracle. Preset error rates are `100/0/500` for
`coverage/spec/corner`; normal SPEC-like traffic therefore remains error-free.
An extreme 1000-per-mille run must set both `stride-stream=0` and
`concurrent=0`; every scalar action then redirects, so neither clean prefetch
training nor the fixed clean scalar members of a mixed issue window are legal.

Schema 18 adds `atomic-error` and `atomic-error-denied`. Atomic error actions
use a separate nonrepeating cold-line region mapped in Bare, Sv39, Sv48, and
all nested VS/G mode pairs. Every enabled AMO/LRSC/AMOCAS x W/D x
clean/corrupt/denied cross must be nonzero. Denied LR reports LoadAccessFault;
denied AMO/AMOCAS reports StoreAccessFault; corrupt reports HardwareError.
For `N` errors including `D` denied errors, the manager tuple must be exactly
`N/2D/2N/N/N` for error responses, denied beats, corrupt beats, errored
GrantAcks, and errored refills. Backing memory must be unchanged when the
exception is returned. MainPipe intentionally retains an errored refill as a
poisoned line and may apply the AMO/CAS transform to its private cache image;
the reference model separately predicts any later dirty ReleaseData without
treating exceptional data as an ISA-visible value. LRSC error actions issue LR
only because a cold SC without a usable reservation returns failure before a
D-channel request.

Schema 19 adds one address-qualified PTW manager-error action to the same
common generator. Five weighted sites cover host stage-1, G-only, implicit
G-stage translation of a VS PTE address, the VS PTE data read, and the final
nested G walk. Independent weights select load/store, root/intermediate/leaf,
Sv39/Sv48, Sv39x4/Sv48x4, and denied versus first- or last-beat corrupt. The
oracle identifies the target PTE address with its own page-table walk, requires
the precise access fault and exact walk cutoff, forbids a target DCache or
Uncache request, then fences the affected translation domains and requires a
clean same-address retry to reread the failed PTE block. Per-seed gates cover
90 site/direction/level/outcome bins, 20 site-specific mode bins, 20 target
level bins, and exact PTW response-beat accounting. Duplicate PTW requests for
the same faulting block remain legal and are counted rather than hidden.

Schema 20 adds `load-merge` to the common operation mix. Each action chooses a
two- or three-load same-cycle batch, both possible critical beats, and one of
three address patterns: exact same address, distinct addresses in one 32-byte
beat, or distinct addresses across both beats. It uses a nonrepeating cold line
from a 64-MiB region mapped identically in Bare, Sv39, Sv48, and every nested
VS/G mode pair. The independent oracle requires every scalar value and metadata
writeback, exactly one DCache request covering the target line, matched refill/
GrantAck counts, and no lost batch member. Extra matched refills are allowed
because the load batch may legally train and trigger the hardware prefetcher.
Per-seed gates cover all 12 depth/pattern/critical-beat bins and every enabled
translation regime. The SPEC preset keeps this operation rare and favors
two-way same-beat locality; coverage and corner presets exercise all shapes
uniformly. Existing `dcache-latency=spec` supplies the calibrated refill-delay
distribution, including the 100-400-cycle bucket.

Schema 21 adds `set-pressure` as a compound random operation. One action
commits nine or ten cold stores to distinct tags in one physical DCache set,
crossing SB/SH/SW/SD, both STA-before-SDA and SDA-before-STA issue orders,
all four set-index quartiles, and Bare/stage-1/nested translation. A 4-GiB
sparse identity-mapped region supplies at least one million nonrepeating
actions even when one set quartile is selected exclusively. The oracle requires
one target DCache request and one store writeback/SQ dequeue per store, at least
`depth - 8` target ReleaseData transactions, byte-exact immutable line images,
and matching manager-memory preservation. Global ReleaseData is independently
counted and byte-checked so eviction of older dirty workload lines remains
legal without weakening target attribution. Coverage and corner weight all
dimensions uniformly; SPEC keeps the operation rare, favors nine-line pressure,
and biases toward SW/SD.

Schema 22 adds `hypervisor-spvp-user` to the common hypervisor class. Every
enabled HLV/HLVX/HSV family is crossed with each enabled SPVP=S/U class per
seed. SPVP=U accesses use independent U=1 VS regions covering all four
4-KiB/Svnapot VS/G leaf combinations and mapping to separately known cacheable
physical regions. Load data and committed HSV bytes are therefore checked
against the physical reference image rather than inferred from DUT translation
state.

Schema 23 uses the existing `misaligned` constraint as a first-class
hypervisor dimension. Every enabled HLV/HLVX/HSV x SPVP=S/U pair is crossed
with aligned and misaligned addresses when both are enabled. Forced
misalignment selects only operations wider than one byte, checks the generated
address class before issue, and retains the independent two-stage physical
address/data oracle.

Schema 24 adds five relative PBMT-pair weights to the common hypervisor class:
`PMA/PMA`, `PMA/NC`, `PMA/IO`, `NC/IO`, and `IO/NC`, where the first value is
the VS-stage leaf and the second is the final G-stage leaf. These combinations
cover final PMA, NC, and IO selection plus both VS-over-G priority directions.
Every enabled HLV/HLVX/HSV x SPVP=S/U x PBMT-pair bin must execute per seed.
The reference walker independently derives the physical address and final PBMT;
loads and stores check exact physical bytes, PMA traffic must avoid Uncache,
and NC/IO traffic must issue exactly one Uncache request with no DCache request.
Non-PMA PBMT pairs are currently naturally aligned; fixed-PMA device boundaries
remain a separate verification gap.

Schema 25 adds `hypervisor-pma-device` as a per-mille selector between the
ordinary translated DDR aliases and an interior address in the SoC's fixed
`c=0`, R/W, X=0 PMA device interval. Every enabled HLV/HLVX/HSV x SPVP=S/U x
DDR/device bin must execute per seed. Device HLV and HSV actions require exactly
one Uncache request, no DCache request, and exact physical data or committed
bytes. Device HLVX actions require an exact `LoadAccessFault`, no DCache or
Uncache request, at least one cancel, and no newly observed wakeup left without
a later same-lane cancel. Device actions are naturally aligned and use PMA/PMA leaf
attributes, keeping fixed-PMA classification independent of PBMT composition
and unresolved non-PMA-misalignment priority.

Schema 26 adds relative weights `hypervisor-pmp-none`,
`hypervisor-pmp-first`, `hypervisor-pmp-last`, `hypervisor-pmp-below`,
`hypervisor-pmp-above`, `hypervisor-pmp-cross-lower`, and
`hypervisor-pmp-cross-upper`. Non-control actions use PMA/PMA leaves and a DDR
target so PMP is the only physical protection variable. The first/last cases
must complete at the exact allowed bytes; every active-PMP HSV is read back
through HLV after installing a global allow entry, so denied stores must also
prove byte preservation. Below/above/crossing cases must raise
the access-specific fault, suppress Uncache and architectural side effects,
and retire through checked redirect/queue recovery. A cross-upper load may
legally expose zero external request on a hit or one request for its allowed
prefix cache line before the final fault; every other denied relation forbids
a new data-manager request. Other PMP region sizes, TOR boundary composition,
and the full lock/permission/overlap-by-edge matrix remain separate gaps.

Schema 27 extends the existing `set-pressure` action with a clean/dirty line
state selected by `set-pressure-dirty`. Clean actions fill nine or ten fresh
same-set lines with unsigned loads, then revisit them in reverse order. The
independent oracle checks exact values and metadata twice, one initial target
request per line, at least `depth - 8` target revisit misses, and matching load
writeback/LQ-dequeue counts. A phase-local TileLink C monitor attributes
Release or ReleaseData by target line address: clean replacement must emit at
least `depth - 8` target Releases and no target ReleaseData, while every
background ReleaseData remains byte-checked. Dirty actions retain schema 21's
immutable line-image, manager-memory, store-writeback, SQ-dequeue, and both
issue-order checks. Coverage closes all 48 line-state x depth x width x
translation bins and rejects traffic in disabled endpoint bins. Coverage and
corner balance clean/dirty actions; SPEC uses a 50-per-mille dirty share.

Schema 28 adds `set-pressure-refill-overlap` and crosses both endpoint values
with every schema-27 line-state, depth, width, and translation bin, for 96 bins.
An overlap action reserves a cold line in a different physical set and holds
only that address-qualified D response; later sources remain free to respond.
The pressure target must emit at least `depth - 8` address-attributed Release or
ReleaseData transactions while the delayed load identity is still pending.
The response is then released and the auxiliary request, load writeback, and LQ
dequeue are conserved exactly. The DCache agent matches GrantAck by sink, so
legal cross-source D/E reordering is accepted without relaxing identity checks.
Coverage uses a 500-per-mille overlap share, SPEC 10, and corner 750.

Schema 29 adds `set-pressure-release-backpressure` and expands that cross to
192 line-state x refill-overlap x C-backpressure x depth x width x translation
bins. The memory agent searches by the action's target line set, lets unrelated
C transactions complete without consuming the target budget, and forces the
first attributed Release or ReleaseData to hold its complete payload for 16
valid cycles. Selected actions require exactly one stalled target release, 16
target stall cycles, and 16 independent payload-stability comparisons;
unselected actions require zero for all three. Every action must close exactly
one address-qualified window. For clean overlap, the eight baseline ways are
filled first, then the held refill is established before overflow tags are
issued, so replacement is causally concurrent with the pending refill without
exceeding LQ capacity. Coverage uses a 500-per-mille C-backpressure share, SPEC
10, and corner 750.

Schema 30 adds `set-pressure-dual-window` and expands the cross to 384 bins.
Selected actions allocate two independent pressure sets in opposite index
quarters and apply the same depth, width, translation, line-state, overlap, and
C-backpressure class to both. Refill-overlap actions hold two address-qualified
D responses at once; each target set must independently emit at least
`depth - 8` attributed Releases or ReleaseData while both load identities are
still pending. Dirty actions first commit and drain eight baseline lines in
each set, then issue the overflow tags, so the checked ReleaseData cannot be
satisfied by an earlier clean eviction. Target request, refill, writeback, and
queue accounting scales with the selected window count, while the
address-qualified C stall remains one transaction per action. Coverage uses a
500-per-mille dual-window share, SPEC 10, and corner 750.

Schema 31 adds `set-pressure-triple-window` and expands the cross to 576 bins.
Window selection is hierarchical: the triple-window probability is applied
first; when triple is not selected, `set-pressure-dual-window` selects between
one and two windows. Triple actions allocate a third distinct set from the
quarter adjacent to the first. The same address-qualified refill holding,
per-set replacement minimum, byte oracle, and window-weighted manager/queue
conservation now cover one, two, or three simultaneous windows. Before writing
new backing bytes, the allocator rejects any target window containing a line
seen in prior DCache request history; this prevents the testbench from changing
memory behind a potentially resident clean line. Coverage uses 333 per mille
for triple and 500 per mille for conditional dual selection, making the three
classes approximately balanced; SPEC uses 1/10 and corner 500/750 respectively.
At schema 31 the minimum `random-mixed` length became 1056 actions.
Four-or-more simultaneous replacement windows remained separate at that
revision.

Schema 32 adds `probe-triple-overlap` as a conditional depth selector after
`probe-overlap`. Non-overlap actions use one primary Probe, ordinary overlap
uses one clean auxiliary plus the primary, and triple overlap uses two clean
auxiliaries plus the primary. Every request is assigned a distinct active B
source and every ProbeAck(Data) is matched by source and address. The terminal
record preserves `actual_probe_overlap` for schema-14 compatibility and adds
the authoritative `actual_probe_depth=depth1,depth2,depth3`; the offline gate
also requires a measured maximum accepted-outstanding depth of three whenever
the triple class is enabled. Coverage/SPEC/corner use conditional triple shares
of 500/10/750 per mille. Four-or-more sources and composition with other
operation classes remain separate.

Schema 33 generalizes the deep class to the standard configuration's complete
eight-entry ProbeQueue capacity. `probe-depth3` through `probe-depth8` are
relative weights within the deep group; the coverage and corner profiles weight
them uniformly, while SPEC biases toward shallower bursts without disabling any
depth. For each sequence, the manager holds C unready until all selected B
requests have fired, so the measured accepted-but-unanswered depth is exact
rather than scheduler-dependent. `actual_probe_depth` now has eight fields and
`actual_probe_cross` records all 32 depth x toN/toB x requested/mandatory-data
bins. Simulator and offline gates require every enabled bin, exact projection
onto the legacy overlap/cap/data counters, and manager Probe conservation.
Cross-operation bursts and malformed responses remain separate.

Schema 34 makes the B-source lifecycle authoritative. The generated port
manifest fixes `tilelink.dcache_probe_source_bits=6`; the DCache agent records
unique accepted IDs, completed-ID reuses, and 63-to-0 transitions. For `N`
accepted manager Probes, both the simulator and the offline verifier require
`unique=min(N,64)`, `reuse=N-unique`, and
`wrap=(N == 0 ? 0 : (N - 1) / 64)`. The active-source scoreboard
continues to reject reuse before the earlier ProbeAck(Data) completes.

Schema 35 adds `set-pressure-quad-window` ahead of the existing
triple/dual hierarchy and expands the authoritative replacement cross to 768
bins. A four-window action uses all four set-index quartiles, may hold four
independently address-qualified D responses, and requires each target set to
reach its own Release or ReleaseData minimum while all selected loads remain
pending. Request, byte-image, writeback, dequeue, C-backpressure, and held-load
accounting continue to scale from the selected window count. Coverage uses
quad/triple/dual conditional shares of 250/333/500 per mille, SPEC uses 1/1/10,
and corner uses 750/500/750. At this revision the minimum `random-mixed` length
became 1248 actions. Five-or-more windows and replacement composition with
Probe/CMO traffic remain separate.

Schema 36 composes the CMO success path with the complete eight-entry
ProbeQueue. `cmo-probe-depth1` through `cmo-probe-depth8` are relative weights
for total B requests while a CLEAN/FLUSH/INVAL CBOAck is delayed. Each action
warms zero through seven distinct clean auxiliary lines, queues their toN
Probes plus the operation's address-qualified target Probe, and holds C unready
until every selected B request is accepted. The simulator and offline verifier
close all 48 operation x clean/dirty x depth bins, match every C response by
source/address, require exact target dirty data, and conserve successful CMO
Probe traffic separately from error CMOs, which still emit no Probe. Coverage
weights all depths equally, SPEC strongly favors depth one without disabling a
class, and corner weights deep bursts progressively. At schema 36 the minimum
`random-mixed` length became 1296 actions. Replacement/atomic Probe composition,
multiple simultaneous CMO sources, and malformed coherence traffic remain.

Schema 37 composes successful AMO, LR/SC, and AMOCAS actions at both W and D
widths with `atomic-probe-depth0` through `atomic-probe-depth8`. Depth zero
preserves ordinary atomic traffic. Depths one through eight hold a cold atomic
refill D response, queue distinct clean auxiliary manager Probes, and then
release D. The DCache may legally backpressure B while the atomic refill is
held; after D release, C remains unready until all selected B requests have
been accepted. Every ProbeAck is matched by source and address, no auxiliary
Probe may carry data, the atomic old-value/result is checked independently,
and the 54 family x width x depth bins must all close when enabled. Atomic
errors remain separate zero-Probe actions. Coverage weights all depths equally,
SPEC strongly favors depth zero without disabling any burst depth, and corner
progressively favors deep bursts. At schema 37 the minimum `random-mixed`
length was 1344 actions.

Schema 38 replaces the hierarchical single/dual/triple/quad replacement
selector with direct relative `set-pressure-window1` through
`set-pressure-window8` weights. It closes 1536 clean/dirty x refill-overlap x
C-backpressure x window-count x depth x width x translation bins. An
eight-window action uses eight distinct physical set indexes, may hold eight
independently address-qualified D responses, and requires every selected set
to satisfy its own replacement minimum before any response is released. The
tracked `queue.dcache_miss_entries=16` value is checked against the standard
Kunminghu-v2 configuration and bounds the generated concurrency below that
capacity, leaving eight MSHRs for replacement progress at the maximum window.
Coverage weights all window counts equally,
SPEC favors one window without disabling any class, and corner progressively
favors wider concurrency. At schema 38 the minimum `random-mixed` length was
2112 actions. Replacement composition with Probe/CMO/atomic traffic, multiple
simultaneous CMO sources, and malformed coherence traffic remain.

Schema 39 adds `miss-burst` to the same operation mix for the ordinary
multi-MSHR pressure visible in SPEC counters, without treating those counters
as a correctness oracle. `miss-burst-depth2` through `miss-burst-depth16`
select distinct never-repeated cold cache lines, and
`miss-burst-issue-width1` through `miss-burst-issue-width3` select how many
loads must issue together in the first cycle. Every selected line's complete
address-qualified D response is held until the DCache manager exposes the
requested number of outstanding A transactions. The online and offline gates
close every enabled depth x legal issue-width x Bare/stage-1/nested bin, then
require exactly one target request, scalar writeback, and LQ dequeue per load,
plus one GrantAck for every refill. Coverage and corner emphasize depth
closure; `spec` keeps the operation uncommon but nonzero and biases toward
shallower bursts. At schema 39 the minimum `random-mixed` length was 2304
actions.

Schema 40 adds the independently configurable `bank-conflict` per-mille
dimension to clean scalar-load actions. Each selected action warms fresh lines,
then issues two or three resident loads to one randomly selected 8-byte bank in
the same cycle. The coverage gate closes all 2/3-way x bank-0..7 x
Bare/stage-1/nested bins enabled by the translation profile. It requires exact
architectural data, one terminal writeback and LQ dequeue per load. Bank
identity, replay/cancel pulses, arbitration, and any prefetch traffic are
recorded only as implementation diagnostics; they do not determine PASS/FAIL.
SPEC performance counters motivated prioritizing this class, but are not used as
the correctness oracle. Coverage/SPEC/corner use rates 500/250/750 per mille.

Schema 41 adds a direction-specific ordinary-vector shape cross. For each
enabled load/store direction it closes all four addressing modes against every
legal EEW/SEW/LMUL combination, deriving EMUL from the RVV relation and
rejecting illegal combinations. The existing independent per-element data and
readback oracle remains the functional check; the cross only guarantees that
the legal stimulus space is reached. The SPEC preset now uses ordinary-vector
load/store weights `90/45` and vector-segment weight `20`, making the measured
SPEC vector event classes materially present while keeping scalar memory
traffic dominant. The minimum run is 3072 actions.

For a reproducible local pressure run:

```sh
make random-stress PICKER="$PICKER" JOBS=8 SEED=1 TRANSACTIONS=16384
```

For a long multi-seed campaign and machine-checked artifact:

```sh
make stress-regression PICKER="$PICKER" REGRESSION_JOBS=8 \
  DURATION_SECONDS=3600 STRESS_TRANSACTIONS=16384
make verify-stress-results PICKER="$PICKER" REGRESSION_JOBS=8 \
  MIN_DURATION_SECONDS=3600 STRESS_TRANSACTIONS=16384
```

```sh
make regression PICKER="$PICKER" \
  REGRESSION_JOBS=8 SEEDS=32 TRANSACTIONS=16384 FORWARD_TRANSACTIONS=48 \
  MIXED_TRANSACTIONS=16384
```

The JSON result defaults to `build/memblock/regression.json` and includes every
command, seed, scenario, simulator output, elapsed time, executable hash, full
ordered-filelist RTL hash, and aggregate status. The run fails if the hash
reported by any simulation differs from the prepared RTL metadata.

For a time-based campaign, the regression targets first freeze the executable,
Verilated model, pinned xspcomm library, and the exact prepared RTL metadata into
`build/memblock/runtime`. It verifies that both shared libraries resolve from
that directory, makes all four artifacts read-only, and records their SHA-256
values plus every resolved system-library hash in `runtime.json`. Duration
campaigns use the frozen `runtime/rtl.json`; rebuilding or removing the mutable
`build/memblock/rtl.json` cannot invalidate an otherwise unchanged run at
shutdown.

```sh
make extended-regression PICKER="$PICKER" \
  REGRESSION_JOBS=8 DURATION_SECONDS=21600 MIXED_TRANSACTIONS=8192 \
  EXTENDED_RESULT="$PWD/../../build/memblock/extended-mixed-frozen-6h-8192.json"
```

The final acceptance campaign is eight hours of the fully mixed scenario. Each
seed requests 16,384 actions and contains repeated five-class overlap windows:

```sh
make final-regression PICKER="$PICKER" \
  REGRESSION_JOBS=8 DURATION_SECONDS=28800 TRANSACTIONS=4096 \
  MIXED_TRANSACTIONS=16384 TIMEOUT_SECONDS=1800 \
  FINAL_RESULT="$PWD/../../build/memblock/final-frozen-8h-16384.json"
```

The runner stops submitting new work only after the requested duration has
elapsed. Already running seeds are allowed to finish, so the recorded wall time
is at least the requested duration. Any failing or timed-out seed stops further
submission but preserves all completed results in JSON. At launch, the output
is atomically replaced by a schema-2 `running` marker with a unique run id, so
an interrupted campaign cannot leave an older accepting artifact at that path.

The default scalar/vector random-load levels and mixed level are now 16,384
transactions per seed; this keeps ordinary regression pressure comparable to
the dedicated stress campaign. `FORWARD_TRANSACTIONS=48` remains the default
requested scalar forwarding level;
the vector forwarding scenario is capped at 24 transactions per invocation to
avoid reusing an LSQ pointer within a focused scenario. Long-duration pressure
comes from consecutive seeded invocations. The verifier separately checks the
requested command value and these bounded completed counts.

For within-seed endurance, the same `random-mixed` generator and constraint
interface can run one million actions per seed. This is deliberately a target
configuration, not a second generator:

```sh
make endurance-regression PICKER="$PICKER" REGRESSION_JOBS=8 \
  ENDURANCE_SEEDS=8 ENDURANCE_TRANSACTIONS=1000000 \
  ENDURANCE_TIMEOUT_SECONDS=28800 CONSTRAINTS=spec
make verify-endurance-results PICKER="$PICKER" REGRESSION_JOBS=8 \
  ENDURANCE_SEEDS=8 ENDURANCE_TRANSACTIONS=1000000
```

`make final-acceptance` requires both the time-based eight-hour campaign and
this finite eight-seed endurance campaign. The first supplies broad seed-space
coverage; the second requires one million actions within every seed so that
long-lived queue, cache, translation, and manager state cannot be replaced by
many short process restarts.

On the recorded host, the current Probe-enabled 16,384-action `spec` seed took
about 260 seconds alone and 279-298 seconds per worker in an eight-worker run.
Linear scaling therefore puts a one-million-action seed near 4.4 hours alone or
roughly 4.7-5.1 hours with eight concurrent workers, with host load and generated
traffic mix affecting the actual result. The endurance timeout is eight hours
per seed; using the ordinary 1,800-second timeout would turn the requested
pressure into systematic timeouts. Keep shorter multi-seed campaigns as well:
they explore more random initial states and localize a failure faster, while
million-action seeds provide much deeper pointer-wrap and long-lived state
pressure.

`make benchmark-tests` executes every leaf scenario once and writes both JSON
and Markdown inventories under `build/memblock/test-scale.*`. Its scenario
inventory is unit-checked against every C++ `--test` dispatch and is the
focused-scenario gate used by `make final-acceptance`; adding a new executable
scenario without adding it to that gate fails the unit suite. The table reports
wall time, cycles, generated actions, load/store completions, DCache A requests,
AcquireBlock refills, PTW and Uncache requests, Probe traffic, and ReleaseData
when the scenario exposes each metric. An action is a generator scheduling
unit, not a synonym for one load/store or one bus request.

Before and after the campaign, the runner verifies the frozen artifacts,
system libraries, runner source, RTL metadata, the runtime-freeze script, and
the C++/SVA/config controller files listed in `CONTROLLER_FILES`. A hash change
in any of them makes the result fail. The verifier also requires the recorded
worker count to be eight, at least 128 complete seeds, finite timestamps, and a
result completion after the duration deadline. It rejects conflicting terminal
summaries and scenario/seed/count mismatches. The runtime may also
be prepared and inspected directly:

```sh
make freeze-runtime PICKER="$PICKER"
LD_LIBRARY_PATH="$PWD/../../build/memblock/runtime" \
  ldd ../../build/memblock/runtime/memblock_sim
```

`stress-regression` and `verify-stress-results` use the same
`STRESS_TRANSACTIONS` value for both the command and verifier configuration.
This prevents a 16,384-action stress artifact from being rejected by a stale
4,096-action verifier default.

The old four-hour artifact was overwritten by a one-second development smoke
run and is intentionally non-accepting. `make verify-extended-results` should
reject the current stale file because its duration and provenance do not meet
the historical gate. The pre-review six-hour artifact and this eight-hour
artifact are historical evidence only after the current stress controller
changes. A new eight-hour mixed run must be generated before
`verify-final-results` can accept it:

```sh
make verify-final-results
```

## Reproduction

A load failure can be replayed directly:

```sh
build/memblock/picker/UT_MemBlock/build/UTMemBlock_example \
  --test random-loads --seed 17 --transactions 16384
```

A forwarding failure uses:

```sh
build/memblock/picker/UT_MemBlock/build/UTMemBlock_example \
  --test random-forwarding --seed 17 --transactions 48
```

Vector failures use the same seed contract with `random-vector-loads` or
`random-vector-forwarding`. A mixed failure is reproduced without any other
scenario:

```sh
build/memblock/picker/UT_MemBlock/build/UTMemBlock_example \
  --test random-mixed --seed 17 --transactions 16384
```

A campaign seed should be replayed from its recorded frozen runtime:

```sh
LD_LIBRARY_PATH="$PWD/../../build/memblock/runtime" \
  ../../build/memblock/runtime/memblock_sim \
  --test random-mixed --seed 17 --transactions 3072
```

## Complete Pin Audit

`config/expected_ports.json` is the exact machine-checked inventory: 749 inputs
and 586 outputs (1,335 pins, representing 7,155 input bits and 5,434 output
bits). `make pin-space` derives directly from that inventory and therefore has
no handwritten omission list. With primary reset held active, it drives and
reads back every non-clock/non-primary-reset input using zero, all-one, and 254
independently seeded mixed patterns. Every output is sampled after every pattern
and folded into the reported digest. Normal reset and clock operation exercise
the remaining two inputs.

This sweep proves structural connectivity and gives each input bit both binary
values plus broad multi-bit combinations. It does not claim that arbitrary
reset-held combinations are legal protocol transactions. Semantic claims come
from the scalar/vector drivers, protocol assertions, TileLink agent, and
scoreboards described above. The generated per-pin inventory is also rendered
in `docs/PORTS.md` for review.

Do not use `XData::CSelf()` to read signal values. It returns the address of the
`XData` object. The environment uses `U()`, `B()`, and `GetBytes()` instead.

See `docs/VERIFICATION_PLAN.md` for the test matrix and `docs/RESULTS.md` for
recorded campaigns. `docs/HISTORICAL_BUG_AUDIT.md` gives the status and evidence
level for every 2026 commit touching `src/main/scala/xiangshan/mem`.
