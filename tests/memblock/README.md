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
legal miss/cancel/metadata behavior. All three frontend bridge paths are driven
with concurrent legal TileLink traffic, randomized request/response backpressure,
source-credit-safe wrap, and field-exact request/response scoreboards. Uncache
denied and corrupt D-channel responses are checked through scalar exception
writeback. PBMT=IO MMIO metadata and error propagation are covered; MMIO device
side effects and CMO CLEAN/FLUSH/INVAL remain explicit boundary gaps.
`hypervisor-contracts` covers HLV/HLVX/HSV privilege, permission, fault, PMP
execute behavior, and each operation family across all four Sv39/Sv48 and
Sv39x4/Sv48x4 two-stage translation pairs. A five-case PBMT basis covers final
PMA/NC/IO classification and VS-over-G priority for every hypervisor operation
family, and cacheable misaligned HLV/HLVX/HSV exercise the scalar split paths.
Vector FOF and unit-stride, strided, indexed-unordered, and
indexed-ordered segment load/store takeover are covered by focused tests; the
common constrained tail mixes segment operations across load/store, EEW
8/16/32/64, and NF 1..7. Unsupported segment shapes are not silently randomized
as ordinary LSQ traffic.

`vector-segment-fof` contrasts first- and later-element page faults. A later
fault suppresses the exception and shortens VL, while a first-element fault
retains the exception and fault VA and leaves VL unchanged.

`pmp-contracts` programs the distributed PMP CSR input and checks data-side
TOR and NAPOT regions, exact lower/upper edges, R/W and AMO denial, overlapping
entry priority, M-mode unlocked bypass, locked-entry enforcement, and locked
address/config immutability. This build uses a 4-KiB PMP platform grain, so
NA4 is not independently selectable: an `A=2` write is WARL-coerced to NAPOT
and is checked as a 4-KiB minimum region. Instruction X permission,
HLVX physical execute denial, and DebugModule access are covered by focused
tests; a broader fixed-PMA region matrix remains a gap.

The MemBlock-facing L2-to-L1 DTLB request/response boundary is also exercised.
`l2-tlb-contracts` checks request-field acceptance, L1 miss responses for both
ordinary and prefetch requests, PBMT/fault-field legality, and the exported PMP
classification. A miss is intentionally handed back to the external L2 TLB;
the MemBlock top level has no refill response input for this port.

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

The reusable C++ components are in `cpp/memblock_env.hpp`:

- reset and cycle control with registered Picker clock;
- typed LSQ, scalar load/store, vector load/store, and software-prefetch drivers;
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
inventory, including explicit planned gaps for MMIO device side effects,
reservation interference and full atomic alignment crosses, CMO CLEAN/FLUSH/INVAL,
remaining segment LMUL/whole-register/redirect combinations, remaining PMP/PMA matrices,
coherence protocol negatives, error injection, same-ROB/vector exception
priority, and four-state behavior. A passing
cacheable mixed campaign must not be interpreted as verification of those
planned rows.

Standalone bug reports are reserved for independently reproduced and
root-caused CPU RTL defects. Their filenames use the `CPU_BUG_*.md` prefix.
UT harness, oracle, regression-controller, and provenance fixes are recorded in
normal commits and consolidated documentation only; they do not receive a
per-fix Markdown report. The confirmed reports currently retained are
`CPU_BUG_UNCACHE_DCHANNEL_ERROR.md`, `CPU_BUG_ATOMIC_EXCEPTION_RF_WEN.md`,
`CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md`, and
`CPU_BUG_FP_EXCEPTION_FP_WEN.md`.

The structure follows UVM responsibilities without requiring a SystemVerilog
class runtime:

| UVM responsibility | MemBlock implementation |
| --- | --- |
| Sequence/sequencer | Focused scenarios and deterministic seeded generators in `memblock_main.cpp` |
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
make single-load PICKER="$PICKER" JOBS=8
make load-feedback PICKER="$PICKER" JOBS=8
make topdown-contracts PICKER="$PICKER" JOBS=8
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
make wfi-safety PICKER="$PICKER" JOBS=8
make reset-recovery PICKER="$PICKER" JOBS=8
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
Sv39x4/Sv48x4 G-stage equivalents, plus the Sv48 512 GiB leaf. Each case is
checked against the independent leaf-address oracle and an architectural load.

`translation-bare` covers stage-1 Bare, G-only, VS-only, and fully Bare
degenerations. It checks that the selected stage is bypassed exactly once and
that no stale page-table walk is required.

`translation-faults` executes valid high-half Sv39/Sv48 loads and both
sign-extension mismatch directions for each mode as scalar load/store page
faults, for ten canonical-boundary transactions. It also covers Sv39x4 and
Sv48x4 GPAs above their architectural limits, a malformed non-aligned Sv39
2 MiB leaf, and 52 PTE encoding cases split evenly across Sv39/Sv48 stage-1
and Sv39x4/Sv48x4 G-stage. Every encoding is exercised by both a scalar load
and a scalar store, for 118 architectural transactions in total. The shared
encoding table crosses V/W/R, both ends of reserved bits 60:54, PBMT=3,
disabled PBMTE, exhausted L0, illegal non-leaf U/A/D/PBMT/N fields, and an
invalid NAPOT encoding. It checks the independent walk's failing PTE/level,
exact access- and stage-specific exception, load fault VA/GPA, zero faulting
DCache/Uncache data requests, and store SQ conservation. A noncanonical store
may still issue an implementation-dependent PTW request before the DTLB reports
the page fault; PTW request count is recorded as coverage, not correctness.

`translation-permissions` executes 58 independent cases. Its table-driven
oracle covers Sv39/Sv48 U/S access, SUM, MXR, missing A/D, VS-stage VSUM/VMXR,
all four VS/G-stage mode pairs, and G-stage R/A/D/U behavior for scalar loads
and stores. Passing stores require exact post-commit readback; faulting stores
must not reach DCache or Uncache and must retire or be explicitly canceled so
SQ accounting remains exact.

`translation-pbmt` crosses all four Sv39/Sv48 and Sv39x4/Sv48x4 mode pairs
with every VS-stage and G-stage `PMA/NC/IO` combination. The independent oracle
applies the architectural VS-stage override priority, then checks exact load
data, committed store readback, IO commit gating, DCache/Uncache selection, and
LSQ conservation.

`fp-loads` exercises the separate FP destination-enable path for cacheable and
PBMT=IO 16-, 32-, and 64-bit load widths. The scoreboard requires exact FLH/FLW
NaN-boxing, exact FLD data, and no integer-register write. Cacheable FLH/FLW/FLD
also cross line and page boundaries through the misaligned split path. Page,
PMP access, stage-1 permission, G-stage guest-page, PBMT=NC misalignment, and
denied/corrupt MMIO faults require exact exception metadata while suppressing
both integer and FP register writes. Translation/PMP faults must reach neither
data manager, and every MMIO case must bypass DCache.

`trigger-contracts` programs a memory breakpoint through the top-level CSR
trigger interface and checks the breakpoint exception bit, trigger action, and
suppressed register writeback. LSQ enqueue metadata (`exceptionVec`, trigger,
and `flushPipe`) is driven explicitly on every transaction; scalar load/store
and vector writeback adapters compare the observable flush, RF-enable, and
MMIO/NCIO/perf debug metadata. Vector and misaligned-store trigger fields are
observed but are not forced to equal the enqueue value when their RTL path
legally regenerates the action.

`metadata-contracts` drives non-default RVC/FTQ/store-set/load-wait values on
the scalar issue interface and completes a load. The top-level `issueLda`
contract does not expose `exceptionVec`; LSQ retains enqueue exception bits for
its internal exception machinery, while page/access/guest-page faults are
recomputed from the TLB in S1. The generated enqueue adapter is covered by
unit tests for exception-vector bit mapping.

`load-feedback` observes all three backend scalar-load wakeup lanes and all
three `ld2Cancel` pins. It checks issue-time `rfWen`, `fpWen`, and `pdest`,
forces a cold miss on every lane, then repeats the accesses from resident cache
lines without a new TileLink request. The oracle permits any number of legal
replays but requires each completed normal load to have exactly one uncanceled
wakeup: `wakeup_delta = ld2Cancel_delta + 1`. It also issues three resident,
different-set loads to the same DCache bank in one cycle. That phase requires
at least two cancellations, no TileLink traffic, exact metadata on the initial
wakeups, and the same conservation rule summed across lanes because legal
replays may migrate between LoadUnits. Independent translated cases classify
three more causes: an Sv39 load-page fault must cancel every speculative wakeup
without sending a DCache request, while PBMT=IO MMIO and PBMT=NC loads must each
retain exactly one uncanceled wakeup, issue one Uncache request, and issue no
DCache request. The MMIO case also performs the backend `pendingMMIOld`
handshake before completion. A forwarding case issues a younger load after the
matching store address but before its data; it requires a pre-data cancellation,
then exact store-data forwarding with one final uncanceled wakeup and no DCache
request after line warmup. A PMP-denied S-mode Bare load must cancel every
speculative wakeup while issuing no PTW, DCache, or Uncache request.

`topdown-contracts` checks every MemBlock top-down output semantically. An
independent bit pattern proves the exact one-cycle L2/L3 miss delay. A cold
load with a delayed refill must assert both L1-miss and replay-allocation, 56
unissued stores must reach StoreQueue full, and 16 distinct committed lines
held behind a delayed refill must reach SBuffer full.
`random-mixed` keeps constant-space lane counters and
requires both canceled and uncanceled wakeups on every lane. When hardware
stride prefetch is enabled, that backend gate is frozen before training begins
because prefetch traffic produces load-pipeline cancel pulses without backend
wakeups; full-run raw counters remain in the result.

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
An independent Sv39 phase dispatches and issues three unmapped `prefetch.i`
requests in one cycle and requires one exact VA on each lane with no PTW or
DCache request. This reflects the RTL ownership boundary: LoadUnit marks
`prefetch.i` as `s0_tlb_no_query`, and the frontend performs any later fetch
translation. The `random-mixed` coverage schema also requires at least one
observed instruction-prefetch output in every seed.

`hardware-prefetch` first isolates the L1 stride trainer, holds the load PC
constant, and issues cold misses at a 128-byte stride. Starting with the sixth
training access, it requires one `Prefetch2L2Stride` output per access at the
RTL-defined depth (`current address + 4096`), then disables the CSR and checks
that further accesses do not emit requests. An independent stream phase uses
different PCs for 12 cold lines in one 1-KiB region, preventing stride
confidence while requiring four exact `Prefetch2L2Stream` requests at the
configured 640-line lookahead. It then crosses into the active neighboring
region and trains a fixed-PC stride for six misses; stream requests must
continue while source 12 remains suppressed, checking the RTL's stream-over-
stride priority. The present build elaborates L3 stream prefetch disabled, so
the L3 output is monitored and required to remain idle instead of being
reported as positive functional coverage. SMS source-10 attribution remains
open: direct AGT generation is hard-disabled in this RTL and the PHT path has
no independent state observation at the MemBlock boundary.
The same fixed-PC stream is available through the common `random-mixed`
`stride-stream` constraint. It runs alongside the configured operation,
translation, miss/refill, Probe, and response-latency mix and requires an L2
source-12 observation in every enabled seed.

`dcache-errors` injects one denied and one corrupt DCache response and checks
the corresponding scalar load access-fault and hardware-error writebacks with
RF writes suppressed. Its backend feedback oracle independently requires
nonzero cancellation for each error and no surviving normal wakeup. The same
all-wakeups-canceled rule is checked by `scalar-guest-fault` for a G-stage fault
whose VA, GPA, and VS-non-leaf classification are independently modeled.

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
G-stage faults. It
reconstructs translated PPNs from the sector response, checks the active
stage's ASID/VMID, permissions, PBMT, fault level, and faults, requires a cold PTW walk,
and holds response ready low to verify stable payload under backpressure. A
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

`uncache-errors` injects denied and corrupt Uncache load and store responses.
Loads check the exception contract through the PBMT=NC adapter. Stores also
require exactly one external Uncache error report at the 64-byte-aligned
physical address, no DCache error report, exact exception metadata, and SQ
conservation. This test caught and now
guards the LoadUnit S1 path that previously discarded response-generated
exception bits. MMIO uses a distinct S0-to-three-cycle metadata bypass and is
not implicated by this reproducer. The complete reproducer and root-cause analysis are in
[`docs/CPU_BUG_UNCACHE_DCHANNEL_ERROR.md`](docs/CPU_BUG_UNCACHE_DCHANNEL_ERROR.md).

`uncache-widths` exercises all seven scalar load opcodes at every legal byte
lane for 8-, 16-, 32-, and 64-bit Uncache transfers. The manager returns the
complete 8-byte beat, checks the generated size/address/mask contract, and
applies deterministic request and response backpressure before the scalar
scoreboard checks sign/zero extension.

`mmio-contracts` maps a page as PBMT=IO and checks the MMIO load's direct
three-cycle metadata path: the load must bypass DCache, report `isMMIO=1` and
`isNCIO=0`, and preserve denied/corrupt response exceptions through writeback.
It also cold-misses and reissues a scalar PBMT=IO store, requiring one Uncache
Put, no DCache request, exact store writeback metadata, and SQ retirement. The
PBMT=IO page is backed by the DDR PMA region, so the store writeback's
`debug.isMMIO=0` denotes `memBackTypeMM=1`. A second bare-mode pair accesses the
SoC's non-DebugModule `c=0` PMA interval and requires physical PMA MMIO
classification for both load and store. A side-effecting device model remains
a planned boundary. `cbo-zero-contracts` drives the `CBO.ZERO` encoding through
the cacheable StoreQueue/SBuffer wline path under randomized DCache
backpressure, checks exact writeback metadata, and reads the resulting line
back before updating the reference mirror.

`reset-recovery` separately asserts reset with an accepted DCache refill, PTW
walk, and Uncache/MMIO request held outstanding by a 256-cycle response delay.
The harness synchronously resets the corresponding tile-local manager model,
accounts each canceled queue entry, changes the post-reset address or page-table
root, and requires three survivor loads to return the new data. Any stale
pre-reset manager response or architectural writeback fails the scenario.

`exception-contracts` checks exact scalar load exception bits, RF-write
suppression, software-prefetch fault suppression, and PBMT-NC misalignment. It
also reverses queue order against program order: three simultaneous load page
faults cross ROB 159-to-0 while LQ indices increase in the opposite direction,
and two simultaneous PBMT-NC misaligned stores cross the same ROB boundary
while SQ indices disagree. The final top-level exception VA must identify the
oldest ROB in both cases, independently of LQ/SQ index order. With both load
and store exception buffers populated, the test then switches
`isStoreException` in both directions and checks the exact retained address
after the two-register output path.

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
visibility through ordinary scalar loads. It also checks representative
misaligned D/W atomics for `storeAddrMisaligned`, suppressed exceptional
`rfWen`, and no DCache request. Atomic uops are intentionally not counted as
LSQ entries because the RTL routes them through `AtomicsUnit`.

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
AMO/LRSC/AMOCAS family and W/D width, NC/MMIO load/store direction, Bare/Sv39/
Sv48 and all four nested VS/G mode pairs, translation switch and legal fence
kind/scope, manager Probe rate/toB/need-data crosses, and DCache, PTW, and
Uncache latency independently. `stride-stream` also controls fixed-PC cold-load
training pressure on the L1 stride prefetcher. It includes
simultaneous scalar/vector issue, every scalar width, every vector EEW and every
load/store address mode independently, scalar/vector misalignment, software
`prefetch.i/r/w`, both cross-forwarding directions, randomized cold/warm
translation, a vector guest-page fault with exact VA/GPA metadata, PBMT=NC,
dirty same-set replacement, redirect/reallocation, and randomized DCache/PTW/
uncache backpressure. Every seed drives all six LSQ dispatch lanes and widths,
checks committed scalar/vector stores through architectural readback, validates
dirty ReleaseData before updating the separate bus memory, and meets bounded
coverage plus final LSQ-accounting gates. The deterministic coherence scenario
covers its directed coherence state sequence. The random tail can follow a
completed dirty scalar store with a byte-exact manager Probe, independently
crosses toB/toN and requested/mandatory data, and cleans up retained toB lines.
Concurrent Probe/refill overlap and multiple outstanding Probe sources remain
follow-on cross-coverage items.

For example, these commands run the same generator in two directions:

```sh
make random-mixed PICKER="$PICKER" SEED=1 TRANSACTIONS=65536 \
  CONSTRAINTS=spec
make random-mixed PICKER="$PICKER" SEED=2 TRANSACTIONS=32768 \
  CONSTRAINTS=corner \
  CONSTRAINT='translation-nested=250 translation-switch=750 tlb-flush=200 concurrent=750 atomic-lrsc=20 mmio-store=400 stride-stream=750 ptw-latency=spec'
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
  --test random-mixed --seed 17 --transactions 256
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
