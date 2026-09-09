# MemBlock Verification Results

## Current Repaired RTL

- Branch: `codex/memblock-ut-closure-20260905`
- CPU baseline commit: `0fa7bb8259a7922481289d8d5932797afce84030`
- CPU repair commits: `f5b553973` (VS-non-leaf vector fault GPA),
  `f8bb99518` (Uncache exception preservation), `e1424686a` (exceptional
  atomic `rfWen` suppression), `7045fa175` (exceptional scalar FP-load
  `fpWen` suppression), and `d159ebdbd` (current vector-segment trigger
  address selection), `39a7b9629` (PTW D-channel error propagation), and
  `42152f6ba` (CMO D-channel error propagation).
- Retracted RTL change: `8eedb3ad0` changed the intentional atomic D-channel
  poisoned-line policy and was reverted by `db6f6d844` after design review.
- Retracted segment redirect changes: `9feb8279e` and `73096f6b9` treated an
  active VSegment plus redirect as a legal MemBlock stimulus. Full-core review
  showed that decode/dispatch/ROB `waitForward`/`blockBackward` serialization
  and interrupt gating make that state unreachable; the RTL and UT changes
  were reverted by `d34dca150` and `af35d347c`, respectively.
- Verification harness baseline: `98bdebbe0777ef051fa8451bd36641eb45f81963`;
  subsequent harness changes are recorded in branch history.
- MemBlock top-file SHA-256: `2ff545f27393bb045d7470e4f13de24e872cf804cdfdd47bb2a366328ed3c646`
- Complete ordered RTL SHA-256: `27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`
- Current rebuilt and frozen UT executable SHA-256: `7f105d5d563e9306f35d37acb45ca85d8a45b558e0213c3854509aaca13d9c85`
- Historical frozen mixed-test executable SHA-256: `2254bb50285a4d0c05a45bd96f43582240b44a9b52d08a188a14b8396716c6d0`
- Current rebuilt and frozen Verilated model SHA-256: `1f9e1c54db04392bc585ad25fce1756ef578a37c75eccbcf0efb3fefca5815c7`
- Frozen xspcomm SHA-256: `0592b633c82eb884fc7a5accd3bfd5337d3f58cb69253db6a109f614ae6b9f74`
- Frozen RTL metadata SHA-256: `29aa19365fd7d772f9ec7889175360fbc2aa87c35ad4880a11e4357257025e69`
- Frozen runtime manifest SHA-256: `145066c38a2f307a20fcfc48b81fd81b4cd179f4824456ba6f52365dc45494f1`
- Frozen-runtime Picker commit: `c100874936aad4030d3bc4c8425ab652f2fbc7ad`
- Frozen-runtime xcomm commit: `23ba5c47310a74dab1567a4ca54ad85dec4512cb`
- Current bootstrap Picker pin: `5e9e38d7087006440ae1c533073b13e798a36927`
- Current bootstrap xcomm pin: `29c290bb1f14fa2a4a72c01ab746a10cff504b2c`

## Scalar Address Immediates And Bus Error Model

On 2026-09-07, the directed scalar address tests began driving the issue
interface's signed 12-bit immediate instead of leaving every operation at zero.
`single-load` passed five additional LD cases at immediates 0, 1, 2047, -1,
and -2048 in 224 aggregate cycles, with seven exact writebacks and seven
DCache requests. `store-forwarding` passed five SB/SH/SW/SD cases at the same
immediate values in 232 cycles. `ifetch-prefetch` retained its 840-cycle,
20-completion result while crossing the same five immediate values. Every
oracle used the final effective address, independently of the driven base.

The same UT-only change tightened manager behavior. Denied DCache data
responses now also assert corrupt, denied is propagated on data-less Grant,
and corrupt-only injection on data-less Grant is rejected. A denied Uncache
store no longer updates ordinary backing memory, and `uncache-errors`
requires all initialized bytes to remain unchanged. `dcache-errors`, all 44
atomic D-channel cases, and all three legal Uncache error cases pass this stricter
model. No CPU RTL defect was identified by this work.

## Floating-Point Load Exception Write Enable

On 2026-09-06, extending `fp-loads` exposed a CPU RTL defect: a denied PBMT=IO
FLW produced the correct `LoadAccessFault` and `rfWen=0`, but retained
`fpWen=1`. The backend FP writeback arbiter consumes that enable without an
exception gate. The focused repair in `7045fa175` applies the same final
exception-vector gate already used by scalar integer loads; details and the
original failing output are in `CPU_BUG_FP_EXCEPTION_FP_WEN.md`.

After full DefaultConfig re-elaboration and Picker rebuild, `fp-loads` passed
in 1,756 aggregate cycles on complete RTL SHA-256
`97b1339a74d458a48a1c58fad766a22cc9dac000cb297e303501311bf47d3b39`.
It checked 16 FP writebacks: aligned cacheable and normal PBMT=IO FLH/FLW/FLD,
cacheable misaligned FLH/FLW/FLD across line/page boundaries, denied FLW,
corrupt FLD, and independent page, PMP access, stage-1 permission, G-stage
guest-page, and PBMT=NC misalignment faults. Successful narrow loads were
exactly NaN-boxed. Every exceptional operation retained its exact exception
while suppressing both RF write enables; early faults produced no data-manager
request, and the guest fault retained exact VA/GPA/nonleaf metadata. The five
MMIO cases emitted five Uncache requests and zero DCache requests.

## Memory Trigger Matrix

On 2026-09-07, the expanded `trigger-contracts` scenario passed 25 cases in
1,433 aggregate cycles on complete RTL SHA-256
`a7ddd8577d0982b8e3a3581cf3f74873813008ed039146d06b04d6a308aa9173`.
All four enable slots produced a positive scalar-load breakpoint. EQ, GE, and
LT each exercised their qualifying boundary, while GE/LT misses plus disabled,
load/store-mismatched, `select=1`, breakpoint-gated, and current-debug-mode
controls produced eight normal cold-load completions. The two-entry chain
fired only when both address comparisons matched. A DebugMode-action load
returned `trigger=1` without a breakpoint exception and issued no DCache
request; its non-architectural data field was the only disabled data oracle.
Six breakpoint loads, aligned and cacheable-misaligned scalar stores, and
eight vector cases crossed load/store, all four EEWs, all four unit/strided/
indexed-unordered/indexed-ordered addressing modes, both actions, and two
two-field segment operations. Breakpoint loads suppressed RF write, every
trigger action, exception, and vector `vstart` matched exactly, and all
scalar/vector store images remained byte-exact. Element-zero hits issued no
external DCache request; later-element/field hits counted only their exact
legal prefix refill.

The indexed segment field-1 case originally exposed a confirmed CPU RTL bug:
the trigger compared the prior latched address, fired on the next access, and
returned `vstart=1` for a field in segment 0. Repair commit `d159ebdbd` connects
the trigger to the same current `tlbReqVaddr` used by the DTLB. The repaired
case returns `vstart=0` after one prefix refill. Full details are in
`CPU_BUG_VECTOR_SEGMENT_TRIGGER_ADDRESS_LAG.md`. Short controls on this same
model passed: `vector-segment` in 397 cycles, `vector-segment-fof` in 304,
`vector-load` in 163, and `misaligned-stores` in 1,200. The frozen runtime
replayed the complete 25-case trigger scenario with the same 1,433-cycle
summary.

## Side-Effecting MMIO Device Model

On 2026-09-07, `mmio-contracts` passed its extended side-effecting-device phase
in 799 cycles on
complete RTL SHA-256
`97b1339a74d458a48a1c58fad766a22cc9dac000cb297e303501311bf47d3b39`.
Denied and independent-corrupt 64-bit reads returned the exact
load-access/hardware-error
exceptions without clearing the initial `0x8877665544332211` register. A clean
read then returned that value and cleared it, and the next clean read returned
zero. A denied `SW` at byte offset four produced the exact store-access
exception without changing any byte. Corrupt on its data-less `AccessAck` is
protocol-illegal and is now rejected by the agent instead of counted as CPU
coverage. The clean
`SW` emitted size 2, mask `0xf0`, and replicated TileLink data
`0xa1b2c3d4a1b2c3d4`; the selected bytes produced the exact beat
`0xa1b2c3d400000000`, which the final read returned before clearing it. The
seven-entry structured log checked sequence, direction, address, request
fields, response data/error flags, and absence of duplicate requests. Every
access used the SoC PMA device interval and emitted no DCache request. No CPU
defect was observed.

A separate 803-cycle phase placed three same-address device loads in the LQ
before allowing the first request. The first two responses were delayed by 512
and 128 cycles. With Uncache outstanding mode enabled, MMIO still issued in ROB
order with maximum external outstanding depth one, and the read-clear values
were exactly `0xfedcba9876543210`, zero, and zero. No request escaped before its
matching `pendingMMIOld`/ROB-head indication, and no duplicate request was
observed.

A third 689-cycle phase pre-issued a device `load -> SW -> load` sequence into
the LQ/SQ. Both the older read and middle store responses were held for 256
cycles. The younger request never reached the device during either window,
maximum external outstanding depth remained one, and the exact log showed the
initial read-clear value, the offset-4 partial write, and the stored-beat
readback in ROB order. The final read cleared the device again.

## Outer L2 Flush Control Bridge

On 2026-09-07, `l2-flush-contracts` passed in 24 cycles on complete RTL
SHA-256
`4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4`.
It covered all four `flush_l2_enable`/`l2_flush_done` combinations, four enable
transitions, and five completion transitions. Ten cycle-by-cycle checks proved
that `outer_l2_flush_en` is a combinational copy of the CSR control and backend
`l2FlushDone` is exactly the previous-cycle completion input. The same monitor
now runs throughout every ordinary functional scenario. No CPU defect was
observed.

## Top-Level Control Bypasses

On 2026-09-07, `top-control-contracts` passed in 281 cycles on complete RTL
SHA-256
`4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4`.
It covered all eight power-down/halt/critical-error combinations while applying
eight independent hart-ID and reset-vector patterns. Cycle-by-cycle checks
proved that hart ID and power-down are combinational, while reset vector, CPU
halted, and CPU critical error are exactly one-cycle delayed. A further 256
patterns exhaustively covered all eight interrupt-sink input bits and checked
the seven one-cycle outputs, including the BEU-local/NMI0 OR mapping. They also
covered combinational MSI-ack/frontend-reset bypass, valid-gated
one-cycle MSI-info and CLINT delivery, one-cycle I-cache BEU metadata, all 67
shared hardware-counter event lanes, and all six two-cycle L2-prefetch-control
fields. The patterns covered all 32 prefetch-enable combinations, all four
MSI/CLINT valid combinations, zero and maximum prefetch delay, and all 64
six-bit event values. The elaborated output event lane 0 remained tied to zero;
the standalone boundary prunes outer lane 0 and inner lane 68. All 267 monitored
cycles passed. No CPU defect was observed.

## Trace Bypass Transport

On 2026-09-07, `trace-bridge-contracts` passed in 48 cycles on complete RTL
SHA-256
`4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4`.
Thirty-two patterns covered all eight valid combinations across three trace
groups, all 16 itypes and FTQ offsets, all eight privilege encodings, and all
four encoder enable/stall combinations. Thirty-four continuous checks proved
the unconditional one-cycle fields, valid-gated address/last-size/privilege
holds, exact 50-bit `iaddr + (ftqOffset << 1)` arithmetic including wraparound,
and cause/tval updates only for the Exception and Interrupt itypes. This result
claims MemBlock bridge transport only, not architectural trace-content
generation. No CPU defect was observed.

## DFT Bridge Transport

On 2026-09-07, `dft-bridge-contracts` passed in 30 cycles on complete RTL
SHA-256
`4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4`.
With external functional reset asserted, it exhaustively drove all 1,024
combinations of the seven SRAM-broadcast and three DFT-reset input bits. Every
combination matched the ten elaborated frontend outputs and four backend
outputs combinationally; the pattern digest was `0x5760755e9ffa3b83`.
The test restored idle DFT values and completed a fresh reset plus idle smoke
before reporting success. This result proves MemBlock bridge routing only, not
MBIST or physical SRAM behavior. No CPU defect was observed.

## Backend Reset Tree Contract

On 2026-09-07, `reset-tree-contracts` passed in 63 cycles on the mutable build
and the frozen runtime, on complete RTL SHA-256
`4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4`.
One-, two-, and five-cycle functional reset pulses all asserted
`io_reset_backend` without a clock edge, held it throughout the pulse, and
released it after exactly six rising edges through the two cascaded
three-stage ResetGen instances. DFT functional mode isolated external reset;
active-low `lgc_rst_n` then asserted the output asynchronously and released it
after exactly three rising edges. Scan mode directly followed both
`lgc_rst_n` transitions without synchronized-release latency. The test
restored all DFT controls, completed a fresh functional reset, and checked idle
outputs. No CPU defect was observed.

## Reset With Outstanding Manager Traffic

On 2026-09-06, `reset-recovery` passed three independent reset phases in 576
aggregate cycles on complete RTL SHA-256
`4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4`.
Each phase first accepted a DCache refill, PTW walk, or Uncache/MMIO request and
held its response for 256 cycles. Reset then canceled the corresponding LQ
entry and synchronously discarded the tile-local manager's transient link
state. Three post-reset survivor loads used a different physical address or
page-table root and returned the new expected data with no stale completion.

An earlier DUT-only reset experiment retained the external DCache model's old
queued response and therefore delivered old-line data after TileLink source
reuse. `XSTileWrap` drives both the core/MemBlock and tile-local L2 from the
same `childReset`, so retaining the manager response did not model the actual
reset domain. This was a UT environment correction, not a CPU RTL defect.

## Frontend Bridge Reset Recovery

On 2026-09-07, `frontend-reset-recovery` passed in 81 cycles on both the
mutable build and frozen runtime, on complete RTL SHA-256
`4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4`.
The scenario first accepted one ICache, instruction-Uncache, and
ICache-control A request, held all three downstream ready signals low until
the requests reached the outer buffer stage, and checked exact payloads for
four stalled cycles. Functional reset removed all three requests. It then
accepted an ICache-control D response behind upstream backpressure, checked
its exact payload for another four cycles, and proved reset removed that
response. Three different-address post-reset requests subsequently emerged
exactly once before a final clean reset and idle check. The other two D paths
do not expose a consumer-ready input at this top-level boundary, so no stalled
response-reset claim is made for them. No CPU defect was observed.

## Ordinary Vector Multi-Uop Addressing

On 2026-09-06, the expanded `vector-addressing` scenario passed in 1,067
aggregate cycles on complete RTL SHA-256
`4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4`.
Its new LMUL=2 phase checked six exact writebacks from three two-uop loads:
unit stride with 16-byte `vuopIdx` base advancement, negative stride with
elements-per-uop advancement, and ordered indexed addressing with an
independent index vector for each uop. Unit-stride and strided uop 1 issued
before uop 0; ordered indexed uops obeyed old-to-young acceptance. Global
element-number slicing selected mask holes across both unit/indexed uops and
suppressed the final negative-stride tail element. Together with the existing
cases, the scenario completed 15 vector-load and three vector-store writebacks
plus exact store readback while issuing 14 TileLink requests. No CPU defect was
observed.

On 2026-09-07, the same scenario added the complete legal whole-register
matrix: `vlr` and `vsr` each cross NF 1/2/4/8 registers with EEW 8/16/32/64.
The driver deliberately supplies `vl=1`, while the independent oracle derives
the effective VL from NF and EEW. Across load, store, flush, and whole-register
load readback, the run completed 120 whole-register load uops and 60 store
uops, allocated and drained 240 LQ plus 120 SQ entries across pointer wraps,
and checked exact data and metadata. The combined scenario completed at cycle
5719 with 135 vector-load writebacks, 63 vector-store writebacks, and 54
TileLink requests against complete RTL hash
`e3250bd4594a3f5594b2fe5e215ddf16b89dd121498a72953b2500eecf61fcf8`.
No CPU defect was observed.

Later on 2026-09-07, an indexed matrix crossed ordered/unordered operations
with all four EEWs. Eight direct loads used repeated and non-monotonic indices;
eight stores used non-monotonic unique indices and were flushed and read back
by another eight indexed loads. Addresses crossed cache lines and a Bare
4-KiB boundary, while 120 LQ and 60 SQ allocations drained exactly across
pointer wraps. The current combined scenario passed at cycle 7414 with 151
vector-load writebacks, 71 vector-store writebacks, and 84 TileLink requests.
No CPU defect was observed.

The next breadth-first extension separated instruction SEW from memory EEW and
exhausted all 78 legal ELEN=64 ordinary unit-stride EEW/SEW/LMUL combinations;
derived EMUL spans fractional 1/8 through 8. It checked 404 load/readback uops
and 202 store uops with exact data plus independent `vsew/veew/vlmul`
metadata. Following Rename's fixed worst-case allocation of two flows per
unit-stride uop, exactly 808 LQ and 404 SQ entries drained across pointer wraps,
and the instruction stream crossed the ROB wrap once. The combined scenario
passed at cycle 24,228 with 555 vector-load writebacks, 273 vector-store
writebacks, and 260 TileLink requests on complete RTL hash
`e3250bd4594a3f5594b2fe5e215ddf16b89dd121498a72953b2500eecf61fcf8`.
No CPU defect was observed.

A following short phase applied the same 78 legal combinations to strided
loads, stores, and readbacks, alternating 39 positive and 39 negative strides.
It also exercised a reusable conservative queue-window driver so legal
high-flow instructions keep one ROB identity while dispatching beyond a single
LQ/SQ watermark. Exact data and metadata passed for 404 load/readback uops and
202 store uops; 2,304 LQ and 1,152 SQ entries drained exactly. The combined
scenario passed at cycle 43,952 with 959 vector-load writebacks, 475
vector-store writebacks, and 580 TileLink requests on the same complete RTL
hash. No CPU defect was observed.

## Top-Down Status Boundary

On 2026-09-06, `topdown-contracts` passed on complete RTL SHA-256
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`.
Ten independent delay checks propagated four L2-miss and four L3-miss cycles
with exact one-cycle timing. A delayed cold load produced 138 L1-miss cycles
and 137 replay-allocation cycles. Filling the 56-entry StoreQueue produced
seven SQ-full cycles; holding 16 distinct committed store lines behind a
delayed DCache refill produced two SBuffer-full cycles.

The same build's extended `uncache-errors` scenario passed two load and one
store response-error cases. The denied store produced exactly one external
Uncache error pulse at the 64-byte-aligned physical line address, no DCache
error pulse, exact architectural exception metadata, and balanced SQ
accounting. It also preserved all eight initialized bytes in
the bus backing memory, closing a prior UT-agent false update on ordinary NC
memory. This was a harness correction, not a CPU RTL defect.

## Vector Segment Addressing

On 2026-09-06, the extended `vector-segment` scenario passed all four segment
address modes on complete RTL SHA-256
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`.
The original unit-stride two-field load/store case remained green. Strided,
indexed-unordered, and indexed-ordered cases added 12 checked load writebacks,
six checked store writebacks, and exact post-flush readback at every calculated
element/field address. Segment traffic allocated no LQ or SQ entries. No CPU
defect was observed.

The companion `vector-segment-fof` scenario also passed both architectural
fault positions. Its later-element page fault was suppressed and reduced VL
from 2 to 1. A separate empty-page-table case faulted on the first element:
both field uops retained `LoadPageFault`, the reported VA matched the first
element, one PTW and no DCache request occurred, and the fix-VL uop preserved
the original VL of 2.

On 2026-09-07, the same scenario added the complete legal non-indexed segment
matrix. Unit-stride and strided each covered 338 NF 2..8 x
EEW/SEW/LMUL/EMUL configurations, with 7,096 load/readback uops, 3,548 store
uops, 12 ROB wraps, zero segment LSQ allocations, and evenly split
positive/negative strided cases. The 456,785-cycle aggregate issued 2,576
TileLink requests and passed exact load/store/readback data and metadata on
complete RTL SHA-256 `e3250bd4594a3f5594b2fe5e215ddf16b89dd121498a72953b2500eecf61fcf8`.
No CPU defect was observed.

The next finite breadth run completed the indexed-segment matrix on the same
RTL. Indexed-unordered and indexed-ordered each covered 338 legal NF 2..8 x
EEW/SEW/LMUL/EMUL configurations. They produced 7,264 load/readback and 3,632
store uops, including 168 load-side and 84 store-side index-only uops from 32
configurations where the index EMUL exceeds the segmented data group. The
906,809-cycle aggregate issued 4,704 TileLink requests, covered 12 additional
ROB wraps, required zero segment LQ/SQ allocations, and passed exact data,
metadata, index-group, and post-store readback checks. No CPU defect was
observed.

## Hypervisor Memory Operation Mode Matrix

The expanded `hypervisor-contracts` scenario passed 82 directed cases in 18,986
aggregate cycles on complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`.
In addition to every exposed HLV/HLVX/HSV encoding and the existing privilege,
permission, fault, and PMP checks, it executed one HLV, one HLVX, and one HSV
under each of `Sv39->Sv39x4`, `Sv39->Sv48x4`, `Sv48->Sv39x4`, and
`Sv48->Sv48x4`. Five further PBMT combinations crossed every operation family
with final PMA, NC, and IO manager selection plus VS-over-G priority. The run
also completed misaligned cacheable HLV.D, HLVX.WU, and HSV.D through the
split/replay paths with exact data and store readback. Twelve new cases execute
HLV, HLVX, and HSV while the current mode is M and SPVP selects U or S, under
R-only, X-only, RW, and RX physical PMP entries. The independent oracle requires
R for HLV, both R and X for HLVX, and W for HSV. Six further cases crossed
locked R and locked RWX entries with all three operation families; nine matrix
denials plus the existing HLVX denial produced exact access faults. Three more cases mapped all
operation families to the SoC's fixed `0x35000000` `c=0` PMA device interval:
HLV and HSV used Uncache with exact data/commit/readback, while HLVX reported
`LoadAccessFault` because the physical region lacks X. Before the boundary
extension, those 76 cases observed 656 PTW requests and 48 data DCache requests.
Six additional cases placed HLV.D,
HLVX.WU, and HSV.D at the last naturally aligned device-side address and at the
first naturally aligned locations at or above `0x80000000`. Device-side HLV/HSV
used Uncache and HLVX faulted without a data-manager request; all three DDR-side
operations completed through DCache with exact load/store/readback data. The
final run observed 704 PTW requests and 51 data DCache requests. Ordinary PMP
denials reached neither data
manager; HLVX with R allowed/X denied exposed the current pipeline's single
early cacheable DCache request but produced no RF effect or Uncache request.
Successful stores were committed and read back through HLV. No confirmed CPU
defect was observed. Other PMA region/edge and hypervisor PMP edge cases
remain open.

## Scalar Load Feedback Boundary

The `load-feedback` scenario now samples every top-level scalar load wakeup and
`ld2Cancel` lane. On the current RTL it passed three cold misses followed by
three resident-line loads and three same-bank concurrent resident loads in 394
cycles: 32 wakeups, 23 cancellations, nine exact scalar writebacks, and three
total DCache TileLink requests. Each load had one final uncanceled wakeup, while
intermediate way-prediction/resource replays were allowed rather than assigned
a fixed count. The same-bank phase produced three cancellations without a new
TileLink request; its initial wakeups carried exact lane/destination metadata,
and its replayed wakeups satisfied the conservation rule across lanes because
legal replay can migrate to another LoadUnit. This closes basic lane,
destination, and read/read bank-conflict semantics.

Three independent translated runs then classified exception and Uncache
feedback. The empty Sv39 mapping raised an exact load-page fault in 93 cycles,
issued one PTW request and no DCache request, and canceled both speculative
wakeups without leaving a normal wakeup. PBMT=IO MMIO and PBMT=NC loads completed
in 246 and 248 cycles respectively; each observed two cancellations and one
final uncanceled wakeup with exact destination metadata, one page-table walk,
one Uncache request, and no DCache request. The MMIO run additionally completed
the backend `pendingMMIOld` handshake. The forwarding case then held matching
store data unavailable for 16 cycles after its address was known. Current RTL
produced one pre-data cancellation and completed in 123
cycles after the data arrived, returning the exact store value with one final
uncanceled wakeup and no post-warmup DCache request. A separate S-mode Bare
load was denied by a 4-KiB NAPOT PMP entry and canceled every speculative
wakeup without a PTW, DCache, or Uncache request. The existing error tests
now apply the same feedback oracle: DCache denied and corrupt responses each
produced two wakeups and two cancellations, while the independently modeled
G-stage guest-page fault produced three wakeups and three cancellations. Thus
none left a normal backend wakeup after its exceptional writeback. The guarded
fixed-PMA denial also canceled every speculative wakeup without issuing a
DCache or Uncache request.

The same short `dcache-errors` run now programs `L1DCacheCtrl` through its MMIO
store path after fully draining two refills and proving both target lines are
resident with zero-request hits. Bank-0 tag injection covered representative
single- and double-bit masks. Each case produced a wakeup/cancel, exactly one
BEU report at the target physical address, no terminal writeback or new
external DCache request, and a clean survivor after immediate redirect; the
first case also reused the canceled LQ index. Data injection crossed all eight
banks with spread single-bit masks and adjacent double-bit masks. Because this
build explicitly sets `EnableAccurateLoadError=false`, every data case produced
no cancel and one normal writeback containing the independently predicted XOR
result, while still producing exactly one BEU report and no external request.
Disabling injection restored exact clean data after every matrix case. A
separate same-cycle pair injected bit 23 into bank 2 while a bank-5 companion
returned clean data. Both lanes woke without cancellation, only the target
address was reported to BEU, neither load issued an external request, and two
clean survivors proved automatic one-shot clear without an explicit disable
write. The current phase passed in 6,494 cycles with 19 physical-ECC BEU reports, 22
wakeups, two cancels, 61 conserved LQ allocations (two canceled), and 56
conserved SQ allocations. No CPU RTL bug was identified by this closure.
The DCache agent now also emits TileLink-legal denied data responses with
`corrupt=1`, propagates denied on data-less Grant, and rejects an impossible
corrupt-only data-less Grant. The directed DCache and 44-case atomic error
matrices remain green under that stricter model.

## Store/Vector IQ Slow Feedback Boundary

The `iq-slow-feedback` scenario now records every valid STA and VSTU
slow-feedback pulse, including its lane, cycle, hit classification, queue
identity, and vector partial-replay fields. Current RTL passed the independent
field oracle: two cold Sv39 scalar stores issued on separate StoreUnits returned
same-cycle misses with exact SQ indices; after both translations were filled,
two new stores returned same-cycle hits on the same two lanes with exact new SQ
indices. Two masked-off vector stores then issued on separate vector lanes and
returned same-cycle hit feedback with exact LQ/SQ identities,
`isVecPartReplay=0`, and a zero replay mask. A separate misaligned strided
vector store produced one blocked partial replay with the expected LQ and SQ
flow identity, `vecReplayMask=0x2`, and merge-buffer index zero, then replayed,
completed, and committed. The scenario passed against complete RTL SHA-256
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`;
no CPU defect was observed.

## Concurrent Exception Priority

The expanded `exception-contracts` scenario passed in 982 aggregate cycles on
complete RTL SHA-256
`4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4`.
Three load page faults were issued together with increasing LQ indices but ROB
ages 0-after-wrap, 159, and 158; the retained top-level exception VA was
`0x50007000`, belonging to ROB 158. A separate phase warmed a PBMT-NC store
translation, then issued two misaligned stores together with increasing SQ
indices but ROB ages 0-after-wrap and 159. The store exception VA was
`0x50008011`, belonging to ROB 159. All five individual writebacks carried the
expected cause and exceptional loads suppressed RF writes. While both buffers
retained those results, a further load page fault was injected: selecting store
continued to report `0x50008011`, selecting load reported its exact
`0x5000a000` VA after the two-register path, and selecting store again restored
`0x50008011`. A further LMUL=2 vector pair used one ROB identity and shared
architectural base. Uop 1 faulted first at `0x52000018`; the later-issued but
older uop 0 then replaced the retained address with `0x52000008`. Both exact
vector exception writebacks completed. This closes the legal wrapped ROB,
disagreeing queue-order, and same-ROB `uopIdx` stimuli. It also co-issued an
unmapped page fault and an older PBMT-NC misaligned fault with reversed LQ
order; both exact writebacks completed and the retained VA selected the older
misaligned operation. Finally, a younger scalar page fault was retained before
an older vector fault replaced it, then a separate environment checked the
opposite vector-first/scalar-replacement direction. The complete scenario
then populated vector-load and scalar-store exception buffers in both arrival
orders, held the initially selected VA after the other source faulted, and
switched/restored the exact selected VA through `isStoreException`. It passed
in 1,351 aggregate cycles with 11 scalar-load, six vector-load, and six scalar
store writebacks. Additional cause pairs and same-operation multi-cause
priority remain open. No current RTL defect was observed.

## Data-Side PMP Contracts

`pmp-contracts` passed 17 hand-calculated cases in 708 aggregate cycles on
complete RTL SHA-256
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`.
Nine accesses were allowed and eight produced the exact load/store access
fault. TOR covered its inclusive lower and exclusive upper byte boundaries;
NAPOT covered the first and last aligned doublewords plus both outside edges.
A read-only TOR region rejected an ordinary store and AMO write without a
DCache or Uncache request. Overlapping 4-KiB deny and 8-KiB allow entries
selected the lower-numbered deny entry in the shared page and the allow entry
in the adjacent page.

The configured `PlatformGrain=12` makes NA4 unselectable, so an `A=2` write was
verified to behave as the specified minimum 4-KiB NAPOT region. Separate
M-mode runs observed unlocked-entry bypass, locked-entry enforcement, and
rejection of later address/config rewrites to a locked entry. The complete run
reported 15 load/atomic writebacks, two store writebacks, and eight permitted
DCache requests. No CPU defect was observed. The separate hypervisor scenario
now covers the SPVP-selected unlocked/locked R/R+X/W permission matrix under
M-mode execution; remaining instruction PMP, hypervisor PMP edges, and fixed-PMA cases stay
explicit gaps.

## RAW Memory Violation Boundary

The `memory-violation` scenario first completed a same-line byte-disjoint load
before resolving an older store and observed no redirect. It then completed
three same-byte candidate loads behind a store at ROB 158; the candidate ROB
identities crossed the circular boundary at 159, 0, and 1. It next completed a
vector-load flow behind an unresolved scalar store, then accepted both scalar
store-address lanes in one cycle so that two distinct loads became rollback
candidates. The lane-1 candidate was deliberately older than lane 0. Current
RTL passed all three redirect cases in 590 cycles, selected the oldest candidate
rather than the lower-numbered source, and preserved independently assigned
ROB, FTQ, RVC, and `flush` metadata. This closes scalar RAW non-overlap,
oldest-of-three selection, ROB wraparound, vector-flow participation,
cross-source arbitration, and all eight exposed output pins.

The companion `rar-violation` scenario enables `ldld_vio_check`, completes a
younger load, forces its dirty cache line through a one-Probe writeback/release,
and then executes the older load. Current RTL passed in 365 cycles with exactly
one redirect identifying `rob=0:30`, `ftq=0:43`, `ftqOffset=5`, `isRVC=1`, and
`level=flushAfter`. A second independent run issued three older loads on
different DCache banks in one cycle after their corresponding younger loads had
observed one released line. Their ROB identities crossed 158, 159, and 0;
current RTL selected `rob=0:158`, proving circular-age arbitration across three
simultaneous RAR sources rather than fixed LoadUnit priority. Other independently
classified cancellation causes remain open.

## DCache Probe Overlap Boundary

`dcache-coherence` retained its clean invalidate, requested clean data, mandatory
dirty data, refill, and exact E-channel GrantAck checks, passing in 481 cycles
with five refills, three Probe responses, two ProbeAckData responses, and five
GrantAcks. An independent 285-cycle phase held a third cold-load refill open
while the DCache accepted two Probes for unrelated resident lines. Both B
requests used distinct source IDs and were accepted before the cold load wrote
back; maximum accepted-but-unanswered Probe depth reached two, both C responses
matched their expected line addresses, and all three refills completed. The
scoreboard follows the RTL contract in which C-source is allocated by the
WritebackQueue rather than echoing B-source.

The `io_ifetchPrefetch_*` audit corrected the earlier direction/ownership
classification: these are three LoadUnit outputs carrying software
instruction-prefetch virtual addresses to the frontend, not IFU training
inputs. On the current RTL, `ifetch-prefetch` passed in 840 aggregate cycles
with 20 exact no-RF/no-exception prefetch completions. It first observed one exact
`prefetch.i` VA on each lane and no IFU pulse for sequential `prefetch.r/w`.
Those five operations crossed signed immediates 0, 1, 2047, -1, and -2048;
every IFU pulse and data-manager request was checked against the final effective
address rather than the issue base.
A separate empty-Sv39 run issued three unmapped `prefetch.i` requests together;
all three lanes emitted their VA with no PTW or DCache request, matching the
explicit `s0_tlb_no_query` path.

Two further same-cycle batches each mixed `prefetch.i/r/w` across the three
lanes. Cold unmapped data hints completed with zero PTW, DCache, or Uncache
requests. For the mapped batch, two ordinary same-page/different-line warmups
created the TLB entries using six PTW requests; the three-operation batch then
issued one best-effort DCache request and no new PTW request. Separate mapped
cold-line `prefetch.r` and `prefetch.w` operations each issued one additional
DCache request. Across all three mapped data requests, no data prefetch emitted
an IFU-side pulse. A final memory-type phase used ordinary loads to prove that
two PBMT=NC and two PBMT=IO pages generated four Uncache requests, then reused
the resident TLB entries for data hints. NC `prefetch.r/w` each generated one
DCache request and no Uncache request, matching the intentional
`6a3636fd2` policy; IO `prefetch.r/w` generated neither manager request. All
four completed without an exception, RF write, new PTW request, or IFU pulse.

`constraint_schema=5` also requires a positive observed instruction-prefetch
count in every `random-mixed` seed. A historical two-worker schema-5 check
passed seeds 912-913 for 512 mixed actions; the two seeds observed five and
three instruction-prefetch outputs respectively. The independently verified
artifact `/tmp/memblock-ifetch-schema5.json` has SHA-256
`d053e833417b505d8c7bdcb27be366ce33f1ddb9c491701f640067fea0e182ab`.

The five L2/L3 hardware-prefetch sender pins were then sampled directly. The
three isolated environments completed in 3,255 aggregate cycles.
`hardware-prefetch` first used eight fixed-PC, 128-byte-stride cold loads;
these produced exactly three L2 stride-prefetch outputs, each with source 12
and the independently calculated `current address + 4096` target. Three more
loads after CSR disable produced no output. An independent spatial-stream run
used distinct PCs for 12 cold cache lines in one 1-KiB region, preventing
stride confidence while producing exactly four source-11 requests. Their
final address was `0x8030a380`, the exact end of the RTL-defined four-line
window beginning 640 lines beyond the final training access. Six fixed-PC
misses in the now-active neighboring region then produced nine further stream
requests and no source-12 output, confirming stream-over-stride priority at
the exposed sender. A third environment held PHT output disabled while twelve
same-PC cold misses trained offsets 0..11, then enabled it and accessed offset
zero in a different region. That trigger produced exactly six unique
source-10 requests at relative offsets 5..10 (`bitmap=0x7e0`), ending at
`0x80501280`; source 11/12 remained absent in this isolated phase. Direct SMS
AGT generation is hard-disabled by the current RTL, but PHT source-10
causality is now positive executable coverage. L3 remained idle as required
because this RTL build's
`enableL3StreamPrefetch` elaboration constant is false; positive L3 coverage
requires a configuration built with that feature enabled.

Hardware stride training was next lifted into the common constraint interface
as `stride-stream`, with preset rates of 500, 100, and 750 per mille for
`coverage`, `spec`, and `corner`. Real 256-action RTL runs for seeds 921, 920,
and 922 passed and observed 4, 7, and 11 L2 source-12 outputs respectively.
The corner run also demonstrated why schema 6 separates architectural backend
load feedback from full-run counters: lane 1 had 50 wakeups/42 cancels before
prefetch training and 81 wakeups/124 cancels over the full run. Prefetch cancel
pulses have no backend wakeup by design, so the former window is the valid load
replay gate while both are retained in the artifact. A two-worker SPEC check
then passed seeds 923-924 for 512 actions each, observing 17 and 26 L2 stride
outputs. The independently verified artifact
`/tmp/memblock-stride-schema6-spec-2x512.json` has SHA-256
`e9498967687a4dc107565ebe611b4388458b52ae29ed881618b9fa6957aef021`.

## Translation Constraint Interface Validation

Commit `f6820b9ff` extends the common `random-mixed` generator with weighted
Bare/Sv39/Sv48 and nested Sv39/Sv48 x Sv39x4/Sv48x4 contexts, drained-boundary
context switching, legal global/selective SFENCE/HFENCE selection, and
independent cold-walk/reuse coverage. The terminal summary uses
`constraint_schema=2`; the offline verifier rejects any enabled translation,
nested-pair, fence, walk, or reuse class that was not observed.

The rebuilt model passed 120 Python contract/verifier tests and 256-action RTL
runs for `coverage`, `spec`, `corner`, Sv48-only, nested-only, and Bare-only
configurations. Representative seeds 41 and 47 covered all six legal fence
kind/scope points, all four nested pairs, both cold walks and TLB reuse, and
SPEC-style DCache/PTW/Uncache delays extending into the 100-400-cycle bucket.
After coverage closure was made to reselect the normal weighted context, SPEC
seeds 225 and 226 each passed 4,096 actions with actual Bare/stage-1/nested
counts `7/3960/28`; every rare mode and fence cross remained covered without
letting the mandatory nested prefix dominate the realistic tail. Coverage seed
227 also passed 4,096 actions with 378 PTW and 274 Uncache requests. An earlier
four-seed SPEC artifact covering seeds 201..204 and 16,384 total actions passed
the independent schema-2 artifact verifier; it is retained as pre-reselection
stress evidence rather than the final campaign result.
The complete RTL SHA-256 remained
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`.

Four failures found while enabling this matrix were classified as harness
issues: mixed vector replay feedback was initially routed to only one vector
transaction, nested PBMT traffic initially omitted `hPBMTE`, and NC/MMIO was
initially allowed in Bare even though this boundary supplies those attributes
only through PBMT. Corner seed 403 also exposed scalar SQ retirement accounting
that targeted `current_dequeue+1`; a cross-page store already at the ROB head
could legally retire before that helper ran, making it wait for the following
vector store. Recording the scalar store's enqueue-time retirement target, as
the vector path already did, made the same 4,096-action seed pass with balanced
`3844/3844` SQ accounting. All reproduced failures disappeared after correcting
the drivers while the RTL remained unchanged. They are not CPU bugs and
therefore do not have `CPU_BUG_*` documents.

The post-fix harness then completed provenance-checked SPEC seeds 304..311 and
corner seeds 403..410, each with 4,096 actions per seed and eight parallel
workers. Both 32,768-action artifacts passed the independent verifier with
complete RTL SHA-256
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`.
The SPEC artifact SHA-256 is
`45a78daf7fff758f038c48d7c1a5ef5a9ad0b55f0e5ecd5505b71d2886a0ea67`;
the corner artifact SHA-256 is
`a8e1e42b54297b0dce073b6e40906563d9f7b423bcd69be5762849309bf486bf`.
The generated artifacts are `/tmp/memblock-translation-spec-post-sq-target-verified-8x4096.json`
and `/tmp/memblock-translation-corner-post-sq-target-verified-8x4096.json`.

`translation-fence-all` was then extended with two distinct-page loads issued
before a shared drain. The first PTW D response is delayed, and the gate
requires both loads to remain pending after the first PTW A request plus two
independently observed leaf-PTE addresses and exact final data. All eight
Sv39/Sv48 x Sv39x4/Sv48x4 and global/selective variants passed, for 16 total
stage-1/nested double-walk cases. The exported PTW manager legally serialized
the requests (`concurrent_ptw_max_outstanding=1`); this is recorded rather than
misclassified as an RTL concurrency failure.

`translation-inflight-context` was added as a separate process-level matrix so
the directed race does not exceed the DPI shared-library static-TLS load limit
of the already large `translation-fence` process. Its four mode variants held
the first PTW response for 256 cycles, changed the stage-1 root/ASID/MODE,
both VS/G roots plus ASID/VMID/MODE, or `V` in either host/nested direction,
redirected the old load, and reused the same ROB/LQ identity. All 24 root-ID,
mode-switch, and virtualization-switch cases passed with exactly six valid
new-context writebacks per variant. The Sv39/Sv39x4, Sv48/Sv48x4,
Sv39/Sv48x4, and Sv48/Sv39x4 variants respectively made 21, 24, 22, and 23
external PTW requests in 3,415, 3,787, 3,817, and 3,852 aggregate cycles. All
runs used complete RTL SHA-256
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`.
The static-TLS limit was a UT process-structure issue, not an RTL failure.

## Vector Segment Constraint Interface

On 2026-09-07, schema 9 promoted segment addressing, EEW, SEW, LMUL, derived
EMUL, and NF into the common `random-mixed` constraint interface. It enumerates
only shapes accepted by the non-indexed `ceil(EMUL)*NF <= 8` or indexed
`ceil(LMUL)*NF <= 8` decoder rule, rejects any enabled but unreachable class
before simulation, and biases toward still-uncovered values before applying
the configured product weights. Indexed shapes carry the complete index group
and explicitly model index-only uops.

A finite 256-action `coverage` run at seed 1 passed in 15,335 cycles on complete
RTL SHA-256 `e3250bd4594a3f5594b2fe5e215ddf16b89dd121498a72953b2500eecf61fcf8`.
Its 11 segment instructions covered load/store 5/6, addressing modes 2/5/1/3,
EEW 3/1/4/3, SEW 4/2/2/3, LMUL 1/2/2/3/1/1/1, EMUL
1/2/2/1/2/2/1, and NF 2/2/1/1/1/1/3. All enabled schema-9 coverage and
conservation gates passed. No CPU defect was observed.

A second 256-action run constrained segment traffic to only unit-stride,
EEW=SEW=32, LMUL=EMUL=M1, and NF2. Seed 2 passed in 13,522 cycles with exactly
one segment load and one segment store; every disabled segment counter remained
zero. It also exposed and repaired a harness-only closure issue: the concurrent
prefix could mark scalar-store covered without producing a Probe candidate.
While a Probe subclass remains uncovered, the common scheduler now explicitly
selects a candidate-producing scalar store; both Probe cap and need-data counts
closed at 1/1. An indexed-only LMUL=M8 configuration, which cannot satisfy
`LMUL*NF <= 8` for NF2..8, was rejected before cycle 0. No CPU defect was
observed.

The weighted presets also passed finite 256-action checks. SPEC seed 3 ran
34,016 cycles with eight segment instructions; despite the `980/10/5/5`
addressing bias, its per-seed floor covered unit/strided/indexed-unordered/
indexed-ordered as 4/1/1/2 and every enabled EEW/SEW/LMUL/EMUL/NF class. Corner
seed 4 ran 37,520 cycles with 17 segment instructions and covered the same
classes under equal weights. Both long-tail profiles observed all four latency
buckets independently for DCache, PTW, and Uncache. No CPU defect was observed.
Frozen-runtime coverage seed 5 also passed 256 actions in 15,787 cycles with
all schema-9 segment dimensions and conservation gates complete.

## Ordinary Vector Constraint Interface

On 2026-09-07, schema 10 promoted ordinary vector addressing, EEW, SEW, LMUL,
and derived EMUL into the same `random-mixed` constraint interface. Each
selected operation is one architectural vector load or store and expands into
the complete 1..8-uop stream. Unit-stride uses the conservative two-flow
allocation contract; strided and indexed flows follow the directed matrix
rules; indexed `EMUL>LMUL` shapes share `vd` across index uops. Large flow
groups are streamed through capacity-bounded LQ/SQ windows without changing
ROB/uop identity. Illegal or enabled-but-unreachable shape classes are rejected
before simulation.

A frozen finite 256-action `coverage` run at seed 12 passed in 16,928 cycles on complete
RTL SHA-256 `e3250bd4594a3f5594b2fe5e215ddf16b89dd121498a72953b2500eecf61fcf8`.
Its constrained-tail load/store split was 10/9. The 19 instructions covered
addressing 4/5/7/3, EEW 6/6/4/3, SEW 6/7/3/3, LMUL 2/2/5/4/2/2/2, and
EMUL 2/1/6/4/2/2/2. They expanded to 43 uops, with six multi-uop
instructions. The frozen controller artifact passed the independent schema-10,
runtime/controller hash, backpressure, enable/disable, per-dimension
conservation, and 1..8-uop gates; artifact SHA-256 is
`2e45390b12f5ae22b98cb34f3ccfb11aa81e1a7e653b80478b4f7579db1be66b`.

A second 256-action run at seed 11 disabled vector stores and constrained the
remaining ordinary vector traffic to only unit-stride, EEW=SEW=32, and
LMUL=EMUL=M1. It passed in 15,244 cycles with a constrained-tail direction
count of 6/0, six shape operations, exactly six uops, and zero counts in every
disabled direction and shape class. A deliberately impossible
EEW8/SEW64/LMUL=mf8 constraint was rejected before cycle 0 with `vector
addressing constraint enables an unreachable class`. No CPU defect was
observed in these runs.

## Superseded Pre-Clarification Stress

### Provenance-rejected one-hour stress run

On 2026-09-05, 191/191 continuous `random-stress` seeds completed successfully
with eight workers and 16,384 actions per seed. The run executed 3,129,344
actions over 3,741.172444 seconds; every per-seed summary passed the independent
coverage, backpressure, queue-accounting, command, and RTL-hash checks. The
aggregate artifact is deliberately non-accepting because the mutable prepared
`build/memblock/rtl.json` disappeared before the shutdown controller hash.
The frozen binary/model/xspcomm and system dependencies remained unchanged,
and regenerating the metadata reproduced its launch hash exactly. The same
investigation found that `verify-stress-results` expected 4,096 generic actions
while the runner recorded 16,384. Both framework defects are fixed; the original
artifact remains unmodified evidence rather than being relabeled as a pass.

These runs used complete RTL hash `b69e387e...`, which contained the invalid
local atomic D-channel policy change later reverted by `db6f6d844`. They remain
useful framework/stress history but are not acceptance evidence for the current
RTL hash.

Artifact: `build/memblock/stress-frozen-a32d74a61-1h-16384.json` (generated,
not tracked), SHA-256
`0c4d8a1a68bc9325df35bd465f60a0a0ab4ed3cd368e7f80cd03ef7372ae04e1`.

These 16,384-action direct runs were completed on that superseded RTL; the
newer 32,768-action pair below is also historical:

- `random-stress --seed 29 --transactions 16384` completed 16,384 actions in
  357,217 cycles. It reached 12 outstanding entries, 10,923 vector load/store
  operations, 5,463 masked/unmasked operations, 1,020 scalar misaligned
  operations, 2,731 scalar/vector forwarding overlaps, all four required
  cross-feature bins, and nonzero DCache request stalls and response delays.
- `random-mixed --seed 31 --transactions 16384` completed 16,384 actions in
  467,369 cycles. It produced 7,032 scalar, 4,672 vector-load, 2,336
  vector-store, and 2,343 store writebacks, with 15 PTW requests, 2 Uncache
  requests, dirty ReleaseData, nested translation, exceptions, redirect,
  forwarding, and all six manager backpressure counters nonzero.
- `random-stress --seed 37 --transactions 16384` completed 16,384 actions in
  356,752 cycles with 12 outstanding entries, 2,731 forwarding overlaps,
  1,048 scalar misaligned operations, all required stress combinations, and
  nonzero DCache request stalls and response delays.
- `random-mixed --seed 41 --transactions 16384` completed 16,384 actions in
  467,199 cycles with 7,033 scalar, 4,671 vector-load, 2,336 vector-store,
  and 2,343 store writebacks; it exercised nested translation, exceptions,
  redirect, forwarding, dirty ReleaseData, and all six manager backpressure
  counters.

These are pre-clarification exploratory evidence and are not silently promoted
to current-RTL acceptance evidence.

The post-translation-matrix `random-stress` driver was exercised against the
current rebuilt binary with a finite multi-seed campaign:

- 8 continuous seeds (`1..8`) passed with eight workers;
- 32,768 stress actions completed (`4,096` actions per seed; the runner maps
  stress actions to `mixed_transactions_per_seed`);
- every seed built one- and two-group bursts with scalar/vector forwarding,
  mask/vstart/vl variation, vector EEW and addressing variation, queue pressure,
  and randomized issue order;
- every seed reached at least 12 outstanding scoreboard entries, all required
  stress combinations, both cache regions, both DCache backpressure classes,
  and balanced LQ/SQ accounting;
- each terminal summary recorded four independent SplitMix64-derived RNG
  streams for traffic, shape, payload, and scheduling;
- `verify_regression.py --allow-finite --require-backpressure` checked every
  recorded summary, continuous seed, command replay, RTL/controller hash,
  coverage field, and artifact integrity;
- the campaign completed in 68.134896 seconds with no scoreboard, assertion,
  timeout, or queue-accounting failures.

Artifact: `build/memblock/stress-current-translation-superpages.json` (generated,
not tracked). Artifact SHA-256:
`5ae6e1bfc90afe791d3e1e7034472905829626d0327b2a9aa62924f4370fabb`.
An earlier 32-seed exploratory run found two false positives from zero-stride
vector forwarding stores. The stress generator now constrains forwarding stores
to non-overlapping positive/negative strides, matching the deterministic byte
oracle; zero-stride independent loads remain covered by `random-mixed`, and
repeated-address stores remain in explicit overlap tests.
The duration-based acceptance target is `make stress-regression`, which uses
the frozen runtime and a one-hour minimum by default. The earlier
`stress-large-provenance-final.json` artifact predates the translation-matrix
controller change and is historical rather than current acceptance evidence.

## Sv48/Nested Translation Matrix

The current harness now exercises the four non-Bare mode pairs independently:

- `Sv39->Sv39x4`
- `Sv39->Sv48x4`
- `Sv48->Sv39x4`
- `Sv48->Sv48x4`

`make translation-matrix` passed all four pairs with DCache/PTW
backpressure, a high-half canonical Sv48 VA, G-stage mappings for every VS
page-table page, and cold/warm accesses. The run completed 1,129 cycles per
pair on average, with 40 PTW requests and four data TileLink requests total;
the second access in every pair reused the translation without an additional
PTW request. This is the 4-KiB nested path; Bare degenerations and the full
permission/fault matrix remain explicit boundary work.

`make translation-fence` also passed: a same-VA Sv39 leaf update stayed on the
old translation before the fence and refilled to the new physical page after a
global and selective `SFENCE.VMA`; nested VS and G-stage leaf updates refilled
after selective `HFENCE.VVMA` and global `HFENCE.GVMA` respectively. The
extended scenario also rebound one host ASID, one VS ASID, and one VMID to new
page-table roots under the matching targeted fence. All three same-ID cases
returned the distinct new-page data and produced a fresh PTW refill. A further
stage-1 race accepted and snapshotted the old 1-GiB root-leaf PTE, held its PTW
response for at least 12 cycles, replaced the PTE, and aligned a global
`SFENCE.VMA` with the younger-load redirect. The canceled request produced no
writeback; reuse of the same ROB/LQ identity forced a new PTW request and
returned only the new-page data. The same delayed-response construction also
passed in VS-only and G-only modes under global `HFENCE.VVMA` and
`HFENCE.GVMA`. Two fully nested Sv39/Sv39x4 cases used the PTW manager's
accepted request-address history to wait for the exact old VS root-leaf or
final G root-leaf beat before applying the corresponding fence. The global
scenario passed in 4,264 aggregate cycles with 89 PTW requests, 21 load
writebacks, three same-ID root reuses, and five outstanding-walk fence races:
stage-1, VS-only, G-only, fully nested VS, and fully nested G.

`make translation-fence-all` passed the same five delayed-response classes at
both global and selective scope for all four VS/G mode pairs. The isolated
stage-1 and VS/G tests select the corresponding mode from the pair; the fully
nested VS and G tests exercise the pair itself. Every target PTE request had at
least 12 assigned response-delay cycles before the PTE was replaced and the
fence was issued.

| VS/G modes | Scope | Cycles | PTW requests | Load writebacks |
| --- | --- | ---: | ---: | ---: |
| Sv39/Sv39x4 | Global | 4,264 | 89 | 21 |
| Sv39/Sv39x4 | Selective | 3,923 | 87 | 21 |
| Sv39/Sv48x4 | Global | 4,343 | 94 | 21 |
| Sv39/Sv48x4 | Selective | 3,811 | 90 | 21 |
| Sv48/Sv39x4 | Global | 4,556 | 97 | 21 |
| Sv48/Sv39x4 | Selective | 3,774 | 91 | 21 |
| Sv48/Sv48x4 | Global | 5,100 | 102 | 21 |
| Sv48/Sv48x4 | Selective | 3,901 | 94 | 21 |

The `HFENCE.GVMA` selective address was encoded as `GPA >> 2`, while
`SFENCE.VMA` and `HFENCE.VVMA` used the virtual address directly. In all 40
process-level race executions, the redirected old walk produced no writeback
and the reused identity returned only new-page data after a fresh PTW request.
Some stage-only executions intentionally repeat between pairs; the fully nested
portion covers all four distinct VS/G combinations.

`make translation-context` passed five context families and 14 same-address
loads: direct Sv39-to-Sv48 mode/root, same-mode host ASID/root, VS ASID/root,
G-stage VMID/root, and host/nested/host `virt` transitions. Every switch and
switch-back returned the independently selected physical-page data; 55 PTW
requests completed without a stale-context result.

`make translation-superpages` passed 266 deterministic leaf/subpage cases on
both the mutable build and frozen runtime.
Ten cover the original Sv39/Sv48 stage-1 and Sv39x4/Sv48x4 G-stage 2 MiB/1 GiB
leaves plus the Sv48 512 GiB leaves. Sixty-four cover every 4-KiB subpage of
legal 64-KiB Svnapot mappings in Sv39, Sv48, Sv39x4, and Sv48x4 stage-only
paths. Another 192 cover all four VS/G mode pairs crossed with VS-only NAPOT,
final-G-only NAPOT, and simultaneous VS/G NAPOT leaves. VS-NAPOT and G-NAPOT
each cover 128 nested subpages, including 64 where both stages are NAPOT; the
G-stage mappings used for implicit VS page-table accesses remain ordinary 4
KiB. The full NAPOT matrix completed 256 initial loads, 256 committed scalar
stores, and 256 exact readbacks against an independent VPN-low-four-bit address
oracle. It generated 142 PTW requests over 38,353 aggregate cycles, exercising
translation reuse across each 16-subpage region. The executable reported
complete RTL SHA-256
`97b1339a74d458a48a1c58fad766a22cc9dac000cb297e303501311bf47d3b39`.

`make translation-faults` passed 258 deterministic architectural transactions.
Ten canonical-boundary transactions cover valid high-half Sv39/Sv48 loads and
both sign-extension mismatch directions for each mode as scalar load and store
page faults. The other four original cases cover an invalid Sv39 root PTE,
Sv39x4/Sv48x4 GPA overflow, and a malformed Sv39 2 MiB leaf. The 26
fresh-environment Sv39/Sv48 encoding cases cross invalid V/W/R, reserved bits
60:54, PBMT=3, PBMTE off, exhausted L0, illegal non-leaf U/A/D/PBMT/N, and
invalid NAPOT encoding. Each case matched the independent failing PTE/level and
exact load page fault. The same 26 encodings also produced exact store page
faults with balanced SQ retirement. No faulting access issued a DCache or
Uncache data request.

Eighty-four added transactions cross ordinary stage-1, VS-only with
`hgatp=Bare`, G-stage, Sv39/Sv48, every PTE level, load/store, and PTE PPN bit
36/43, which form physical address bits 48/55 outside this configuration's
48-bit PA. The independent walker classified each valid encoding as an access
fault at the exact level and PTE. The 24 leaf, 36 intermediate, and 24 root
transactions issued a walk ending exactly at the faulting PTE; stores reused
shared legal ancestor PageTableCache entries before rereading that AF PTE, for
138 PTW block requests total, including 46 on the VS-only path. G-stage `gaf`
correctly mapped to the original load/store access fault rather than a
guest-page fault. No case issued a DCache/Uncache request, and every store
conserved its SQ entry.

A second 56-transaction width matrix crosses all four Sv39/Sv48 VS/G-stage
mode pairs, every VS PTE level, load/store, and the first illegal PPN bit for
the selected 41/50-bit GPA plus PPN[43]. The independent nested walker reports
the exact constructed GPA and classifies every case as a guest-page fault,
with `isForVSnonLeafPTE` set for the 40 intermediate/root transactions and
clear for the 16 leaf transactions. The DUT matched the exception, fault VA,
and marker in every case. Each load/store path accessed the faulting VS PTE
block exactly twice as required by the fully nested path, issued no
DCache/Uncache request, conserved every SQ entry, and generated 246 PTW block
requests in total.

The first canonical-boundary run incorrectly required a noncanonical store to
issue no PTW request. The RTL returned the exact `StorePageFault`, issued no
DCache/Uncache request, and conserved the SQ entry, but its store DTLB path
issued one PTW request before reporting the fault. That PTW-count requirement
was an over-constrained UT anti-oracle, not a CPU bug. The corrected test records
four such PTW requests as implementation coverage and keeps only architectural
exception, data-manager non-use, and queue conservation as correctness oracles.

The same 26-entry encoding table also passed through final-data G-stage walks,
split across Sv39x4 and Sv48x4. Every case produced the exact load guest-page
fault, fault VA/GPA, and `isForVSnonLeafPTE=0`; the paired store produced the
exact store guest-page fault and balanced its SQ entry. Neither access type
issued a DCache or Uncache data request. The G-stage data GPA deliberately uses
a different x4 root index from the VS page-table GPAs so root-level mutations
cannot turn the test into an earlier implicit-walk fault.

During this run, an unspecified scalar-load `debug.isNCIO` expectation caused
a test-only false positive on a faulting PBMT PTE. The scoreboard had collapsed
an absent `optional<bool>` to false. It now checks optional debug metadata only
when explicitly constrained; stable MMIO/NC cases remain strict. The store
extension likewise left debug MMIO/NCIO unconstrained for exceptional PTEs,
while keeping exact architectural exception and side-effect checks. The
118-transaction rerun and neighboring MMIO/translation regressions passed.

`make translation-permissions` passed 194 fresh-environment permission cases:
16 stage-1 loads, 17 stage-1 stores, 75 two-stage loads, and 86 two-stage
stores. The independent truth table covered Sv39/Sv48 U/S pages, SUM, MXR,
missing A/D, VSUM/VMXR, and G-stage R/A/D/U selection. Every nested load and
store permission variant ran under all four VS/G-stage mode pairs. Both
Sv39x4 and Sv48x4 rejected a store when `D=0` with the exact
`StoreGuestPageFault`. Passing stores committed and matched exact scalar
readback; faulting nested loads and all faulting stores issued no DCache/Uncache
request, and store cases retained exact SQ retirement/cancellation accounting.
The added 42-configuration implicit-walk cross rejected G-stage X-only even
with MXR, U=0, and A=0 at every VS PTE level under all four mode pairs. Each
configuration ran both original load and store, yielding 84 access-specific
GPF transactions. Every fault reported the original VA, exact PTE GPA, and
asserted implicit-access marker, stopped before reading the protected VS PTE,
issued no data request, and preserved SQ accounting. Four positive nested
stores succeeded with every VS page-table mapping R-only and W=0/D=0,
committed, and matched exact readback. This confirms that the G-stage access is
checked as an implicit load rather than as the original store. No CPU RTL
defect was observed.
Neighboring 4-pair mode-matrix, 258-case fault, and 266-case superpage/Svnapot
runs passed on the same complete RTL SHA-256
`97b1339a74d458a48a1c58fad766a22cc9dac000cb297e303501311bf47d3b39`.

`make translation-pbmt` passed all 36 valid two-stage PBMT combinations: each
of the four Sv39/Sv48 and Sv39x4/Sv48x4 mode pairs crossed VS-stage
`PMA/NC/IO` with G-stage `PMA/NC/IO`. The independent Svpbmt rule selected the
VS-stage type when nonzero and otherwise the G-stage type, yielding four PMA,
16 NC, and 16 IO combinations. Across 72 loads and 36 stores, the final type
matched DCache/Uncache selection and IO commit gating; every load and committed
store readback matched exact data, and all LQ/SQ entries retired. Neighboring
`translation-permissions`, `translation-matrix`, `translation-faults`,
`translation-superpages`, `mmio-contracts`, and `uncache-widths` passed on the
same RTL SHA-256.

The final frozen harness then passed an independently verified eight-seed
`spec` campaign with 8,192 actions per seed. All 65,536 constrained-random
actions completed over 2,365,067 simulated cycles in 124.585011 seconds, with
8,363 DCache TileLink requests, 1,309 PTW requests, 647 Uncache requests, 299
dirty ReleaseData beats, 1,150 TLB flushes, 153 scalar misaligned accesses, and
eight vector replays. DCache, PTW, and Uncache response latencies reached 398,
397, and 399 cycles respectively. The frozen runtime and all controller hashes
were unchanged across the run. The independently checked artifact is
`build/memblock/spec-final-8x8192.json`, SHA-256
`19bac99771d9da03dc6d6714150eb55dbed8735c0144328be1619dee5c0847d2`.

### Frozen Eight-Hour SPEC Campaign Before Translation-Context Merge

The frozen delivery harness at commit `e8ba63db9` completed a full duration
campaign from 2026-09-05 17:57 to 2026-09-06 02:00 Asia/Shanghai. All 956
continuous seeds passed with 16,384 `spec`-profile actions per seed: 15,663,104
actions and 552,367,478 simulated cycles in 28,985.093764 seconds. Aggregate
external traffic included 1,505,157 DCache TileLink requests, 295,803 PTW
requests, 150,203 Uncache requests, 136,409 dirty ReleaseData beats, and
276,214 TLB flushes. It also exercised 36,261 scalar misaligned accesses and
956 vector replays; maximum DCache, PTW, and Uncache response latency was 400
cycles and maximum scoreboard occupancy was seven.

The frozen executable SHA-256 was
`323cee26977a0d49b655750dd7d7ec9c35cd79c12abb404dd00400115998b6a8`.
The complete RTL SHA-256 remained
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`,
and every controller/runtime hash was unchanged. Direct invocation of the
independent artifact verifier accepted seeds 1 through 956. The artifact is
`build/memblock/spec-post-fence-final-8h-16384.json`, SHA-256
`278e00031a03baeee1f2d98a5f806d55b37e4eb8674cbe1aecaa5de6e259cffa`.

The composite `make verify-final-results` target stopped before this final
artifact check because the separate build artifact
`random-boundary-hunt.json` carries an older RTL hash. Its DCache release and
known-bug sentinel prerequisites passed, and the exact final artifact command
passed when run directly. This is stale prerequisite data, not an RTL failure.
The campaign also predates the isolated random-translation, concurrent-walk,
in-flight context, and canonical-boundary changes through `228840c8f`; after
those controller changes are merged, a newly frozen campaign is still required
for final acceptance of the merged harness.

### Frozen Eight-Hour SPEC Campaign Before Top-I/O Audit Merge

After the translation-context and canonical-boundary changes were merged, the
frozen harness at controller revision `97b9eb49f` ran from 2026-09-06 02:13 to 10:17
Asia/Shanghai. All 860 continuous seeds passed with 16,384 `spec` actions per
seed: 14,090,240 actions and 560,576,358 simulated cycles in 29,046.287660
seconds. Per-seed wall time ranged from 249.578 to 299.499 seconds, with a
268.264-second median and 268.988-second mean.

The campaign produced 9,436,547 scalar load writebacks, 3,658,050 scalar store
writebacks, 665,174 vector load writebacks, 333,582 vector store writebacks,
and 642,134 software-prefetch writebacks. Aggregate manager traffic was
1,342,718 DCache TileLink A requests, 1,850,164 PTW requests, 135,283 Uncache
requests, and 121,772 dirty ReleaseData beats. The rolling concurrent windows
issued 198,794 operations from each of their five producer classes. Every seed
exercised a vector replay; maximum live scoreboard occupancy was seven, and all
three manager response-latency profiles reached 400 cycles.

The complete RTL SHA-256 remained
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`.
Frozen binary/model/runtime, external libraries, RTL metadata, runner, and all
eight controller hashes were unchanged before and after the campaign. Direct
independent verification accepted seeds 1 through 860. The artifact is
`build/memblock/spec-merged-final-8h-16384.json`, SHA-256
`5b99eb4bea45b3e2491bb7e40a8077c87e4fa9e2877827dfde1b6e0c84aede07`.

The composite `make verify-final-results` prerequisite check found the shared
generated `build/memblock/rtl.f` had subsequently been prepared from the
isolated top-I/O audit worktree, so the exact artifact verifier was invoked
directly with the frozen metadata and controller set. It reported
`MEMBLOCK_REGRESSION_ARTIFACT_PASS`. This is a generated-path provenance
mismatch, not a simulation failure. The top-I/O audit, DCache B/C/E agent,
endurance controller, IFU PTW bridge, and randomized Probe constraints are
later controller changes, so this result remains valid pre-merge evidence and
a newly frozen campaign is still required for the merged harness.

### Post-Merge Probe Stability And Test Scale

After the top-I/O audit changes and randomized Probe constraint were merged,
the first eight-seed mixed matrix exposed a harness assumption at seed 5: SQ
retirement had moved a store into SBuffer, but a Probe sent only 64 cycles later
legally observed the line as N and returned NtoN. The test had incorrectly
required dirty TtoN data. This was a test-state setup issue, not a CPU bug. The
constraint now allows half of the bounded manager-completion window for older
SBuffer traffic to drain before requiring the candidate line to be dirty, and
the Probe mismatch diagnostic records both expected and observed opcode,
permission, size, address, corrupt bit, and beat.

With that correction, frozen-runtime seeds 1 through 8 each completed 16,384
`spec` actions: 131,072 actions and 5,631,112 simulated cycles in 298.499484
seconds. Aggregate traffic included 87,748 scalar load writebacks, 34,056
scalar store writebacks, 6,219 vector load writebacks, 3,095 vector store
writebacks, 6,060 prefetch writebacks, 12,490 DCache requests/GrantAcks, 17,532
PTW requests, 1,302 Uncache requests, and 1,142 ReleaseData beats. The 51
constrained Probe sequences issued 81 manager Probes including toB cleanup;
toN/toB coverage was 21/30 and no-need-data/need-data coverage was 23/28. The
independent artifact verifier accepted
`build/memblock/post-probe-fix-mixed-8x16384.json`, SHA-256
`34a296b963ff4b619bcde645f6a2b13cf8628f29e19aa64c131f981b7f18e7b9`.

The same frozen runtime then passed all 61 leaf scenarios in 825.279134 seconds.
At the common scale, `random-loads` completed 16,384 scalar loads in 356,934
cycles and 145.609 seconds; `random-vector-loads` completed 16,384 vector loads
in 427,968 cycles and 162.902 seconds; `random-stress` generated 16,262 DCache
refills in 359,885 cycles and 134.999 seconds. The `random-mixed` measurement
completed 16,384 actions in 705,714 cycles and 259.846 seconds, including
10,175 scalar loads, 4,290 scalar stores, 799 vector loads, 388 vector stores,
740 prefetches, 1,557 DCache refills/GrantAcks, 2,236 PTW requests, 158 Uncache
requests, 10 Probes, and 140 ReleaseData beats. The full per-scenario table is
`build/memblock/test-scale.md`; its JSON artifact SHA-256 is
`acfc2594634a2b2beb4ef174f1dfbdae286cddfd4b0b3b3d50517cf5d37f8c10`.

After PMP contracts, PMP load-cancel classification, and the final-acceptance
inventory repair, the current frozen executable passed all 68 registered leaf
scenarios in 816.650457 seconds. The inventory is now unit-checked against the
C++ dispatch and is invoked directly by `final-acceptance`, so newly registered
scenarios cannot be omitted by a stale hand-maintained target list. Focused
coverage included 17 PMP cases, a two-source Probe overlap with an unrelated
refill, 118 translation-fault cases, and 36 IFU-PTW bridge cases.

At the common random scale, scalar and vector load scenarios each completed
16,384 checked operations. `random-mixed` completed 16,384 actions in 710,326
cycles, including 10,237 scalar loads, 4,234 scalar stores, 802 vector loads,
381 vector stores, 736 prefetches, 2,614 DCache refills/GrantAcks, 2,566 PTW
requests, 158 Uncache requests, five Probes, and 309 ReleaseData beats.
`random-stress` completed another 16,384 actions with 16,262 refills and maximum
outstanding depth 12. The frozen executable SHA-256 is
`993422f6541f479a1d1be8842906843c4dbc549b48df58278257c112c47ec92f`;
the complete RTL SHA-256 remains
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`.
The JSON artifact is `build/memblock/test-scale.json`, SHA-256
`9ae14b4feed67b243117709ba06e47b8f5428ac5bbd970714a1d7cba1770afd7`.
No CPU defect was observed.

### Million-Action-Per-Seed Endurance Campaign

The post-Probe frozen runtime completed eight independent `spec` seeds from
2026-09-06 11:02 to 15:31 Asia/Shanghai. Every seed completed exactly
1,000,000 constrained-random actions, for 8,000,000 total actions and
325,904,665 simulated cycles in 16,159.999208 seconds. All eight results
passed with backpressure enabled; the DCache, PTW, and Uncache response-latency
models each reached 400 cycles (one seed reached 398/399 on the latter two).

Aggregate architectural traffic included 5,362,645 scalar-load writebacks,
2,081,291 scalar-store writebacks, 371,751 vector-load writebacks, 186,225
vector-store writebacks, and 364,932 software-prefetch writebacks. Manager
traffic included 402,722 DCache refills/GrantAcks, 1,031,235 PTW requests,
74,633 Uncache requests, 2,860 Probes across 1,902 constrained Probe sequences,
and 135,825 dirty ReleaseData beats. The run also exercised 17,436 scalar and
7,700 store misaligned cases plus 24 directed exception-prefix cases.

The complete RTL SHA-256 was
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`.
The frozen executable, model, xspcomm, resolved libraries, runtime metadata,
runner, and all eight controller inputs had identical before/after hashes.
`make verify-endurance-results` independently accepted seeds 1 through 8 and
all 8,000,000 actions. The artifact is
`build/memblock/endurance-spec-8x1000000.json`, SHA-256
`1393a96664b47a5cdeb3ba8624ee9c2b648afb7a4bc080b692ee1bda8198c321`.
No assertion, scoreboard, timeout, process, provenance, coverage-gate, or RTL
failure occurred, and this campaign did not expose a new CPU bug.

After the 57-case fault expansion, `random-mixed --seed 419 --transactions
4096 --constraints spec` passed in 152,052 cycles. It mixed 2,467 loads, 1,011
stores, 181 prefetches, vector traffic, atomics, MMIO/NC, translation faults,
99 PTW requests, and 45 Uncache requests. The latency model reached 398 cycles,
all manager backpressure classes were nonzero, maximum scoreboard occupancy was
seven, and every coverage and LQ/SQ accounting gate passed.

`make fp-loads` passed cacheable 32-bit and 64-bit FP destination transactions.
The 32-bit result was checked as a NaN-boxed 64-bit value, with integer RF
write disabled and FP write enabled in the observed writeback.

## Historical Frozen Eight-Hour Acceptance

This record predates the `random-stress` controller addition. It remains a
valid historical result for the frozen executable listed above; after any
controller change, `verify-final-results` must be regenerated before this
record is treated as the current acceptance artifact.

On 2026-09-04 (Asia/Shanghai), the reviewed harness completed the final
frozen `random-mixed` campaign:

- run id: `9d0fdae3136e4330b49dd4694cab3cb6`;
- requested duration: 28,800 seconds; measured elapsed time:
  28,960.984777 seconds;
- 1,201/1,201 continuous seeds passed, from seed 1 through seed 1,201;
- 19,677,184 mixed actions completed, with 16,384 actions per seed;
- every result used eight workers, backpressure enabled, and complete RTL
  SHA-256 `0b6c6aa34dc8aa148d0f6da91091df58e6622415136e60baf985d7560824e7e3`;
- no scoreboard, assertion, timeout, process, coverage-gate, or queue-accounting
  failure occurred.

Artifact: `build/memblock/final-frozen-8h-16384.json`, SHA-256
`c104f21bc2dc9198f5987ed626b046fd058519b18de4464b1809060c14a1b216`.
The independent verifier reported `MEMBLOCK_REGRESSION_ARTIFACT_PASS` for
seeds `1..1201` and verified the schema-2 completion marker, frozen runtime,
controller hashes, finite timing, continuous seeds, per-seed coverage,
backpressure, and exact LQ/SQ accounting. The final acceptance target also
passed the dedicated dirty ReleaseData, repaired known-bug sentinel, and
32-seed boundary-hunt gates.

## Focused Runs

Deterministic focused and constrained-random evidence are tied independently to
their recorded RTL/runtime hashes below.

## Current Harness and RTL Fixes

The rebuilt harness freezes all acceptance inputs after the stale-runtime issue
was removed. `random-mixed` randomizes scalar and
vector data, widths, masks, `vl/vstart`, legal address classes, vector modes,
issue order, store address/data order, and inter-window delays while preserving
explicit interface constraints. The boundary diagnostic records all generated
parameters and verifies the independent Sv39x4 GPA oracle.

Latest focused correction evidence:

- 116 Python unit tests and the complete port/SVA/filelist checks pass;
- `frontend-bridge` closes legal-transaction semantics for the 88 top-level
  frontend TileLink fields. Four seeds with 4,096 transactions on each path and
  a fifth seed with 16,384 transactions per path all passed. Across the
  concurrent ICache, ICache-control, and instruction-Uncache paths this is
  98,304 A requests, 131,072 D beats, 1,587,607 field comparisons, 44,164
  request stalls, 21,478 response stalls, and 207,826 source-credit stalls.
  The checks cover credit-safe source wrap,
  two-beat ICache data, Get/PutFull/PutPartial control traffic, all 1/2/4/8-byte
  sizes, full/partial masks, corrupt polarity, FIFO ordering, and final
  quiescence. The newly frozen runtime separately passed seed 29 with 4,096
  transactions per path;
- the regression runner and independent artifact verifier now include
  `frontend-bridge` in the normal six-scenario matrix. A frozen-runtime
  32-seed run at 4,096 transactions per path passed all 32 cases in 43.065253
  seconds, totaling 393,216 A requests, 524,288 D beats, 6,345,239 field
  comparisons, 175,708 request stalls, 86,636 response stalls, and 831,760
  source-credit stalls. Runtime and controller hashes were unchanged and the
  verifier accepted artifact SHA-256
  `dcf45dea560ea8d6689c2b214f7c34ec3cbad3a3282da99de74eb0a5fa82fcb0`;
- an eight-seed matrix then passed all 48 invocations of `random-loads`,
  `random-forwarding`, `random-vector-loads`, `random-vector-forwarding`,
  `random-mixed --constraints spec`, and `frontend-bridge`. It completed
  131,648 reported transactions in 186.104745 seconds on complete RTL
  `774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`;
  the independent verifier accepted exact commands, per-scenario coverage,
  backpressure, continuous seeds, and unchanged frozen/controller hashes.
  Artifact SHA-256:
  `33a2fc52e8c405c7161589accf284407704ee44963929b182b82db3cd6ae1de8`;
- after making mixed-window scalar addresses obey `misaligned` and supplying
  the required ROB-head pulse for the rare cross-page store, the exact former
  timeout command (`random-mixed --seed 42 --transactions 16384 --constraints
  spec`) passed 16,384 actions in 582,384 cycles. It observed 40 misaligned
  scalar loads and 17 misaligned scalar stores, including one split-store
  sample, rather than the window's previous unconstrained byte offsets;
- the expanded common constraint interface passed 256-action `coverage`,
  `corner`, and `spec` seeds. Every run covered enabled AMO/LRSC/AMOCAS families,
  W/D widths, NC/MMIO load/store directions, legal NC/MMIO overlap, and all
  required manager-latency buckets. Isolated NC-only and MMIO-only overlap runs
  also passed; an atomic-only request for `special-concurrent` was rejected
  before reset because atomics are pipeline-serializing at this boundary;
- an independent-latency run with DCache=`spec` and PTW/Uncache=`compact`
  observed DCache buckets `60,8,5,5` while both other managers stayed in their
  compact bucket. A separate constrained seed enabling only LR/SC.W within the
  atomic class observed exactly that family and width and passed;
- the rebuilt interface completed `spec` seeds 1-8 and `corner` seeds 101-108
  at 4,096 actions per seed: 65,536 constrained-random actions in total. Both
  eight-seed artifacts passed the independent verifier with exact queue
  accounting, backpressure, coverage, command, and RTL-hash checks. The
  `spec` artifact SHA-256 is
  `fcd1209c009e3fe5ca456ea483cce4e607ad159fe96d485377c05abfa696ca44`;
  the post-scheduler-fix `corner` artifact SHA-256 is
  `c1e5323a0cc513c7387d559d2660757a4d411d7f0df0908cfecd966d1f154e30`;
- the first one-hour `spec` launch stopped after seven complete 16,384-action
  seeds when seed 8 revisited a dirty-pressure line. This was a UT oracle
  lifetime defect: the directed replacement phase left exact initialization
  snapshots registered for cache lines that the random tail could legally
  modify. A later ReleaseData was compared against the stale snapshot instead
  of the live architectural reference image. The directed phase now discards
  only its surviving snapshots after checking its required releases; subsequent
  releases retain full byte checking against the live reference. The exact
  `random-mixed --seed 8 --transactions 16384 --constraints spec` replay then
  passed 16,384 actions in 579,296 cycles with 140 ReleaseData transactions,
  316 PTW requests, 143 Uncache requests, and exact LQ/SQ accounting. The
  stopped diagnostic artifact contains 114,688 passing actions and remains at
  `build/memblock/spec-interface-1h-16384.json`, SHA-256
  `f660ae1aab340b7df124ea4caf1bb6108449036ebebaee4118fcce12d5605e71`;
- the corrected frozen runtime then completed the replacement one-hour `spec`
  campaign: continuous seeds 1-125 all passed, totaling 2,048,000 actions in
  3,817.717522 seconds. The independent artifact verifier accepted every
  command, coverage gate, queue count, controller input, runtime dependency,
  and RTL hash. Artifact:
  `build/memblock/spec-interface-fixed-1h-16384.json`, SHA-256
  `a6aba912e56c3ad8e7cf00f6c6d9cb53f0778d7acba8c32c12cc0189a71b10c4`;
- the common constrained-random interface passed an override run whose tail
  enabled only scalar loads (`seed=29`, 256 actions): actual constrained
  operations were `155,0,0,0,0,0,0,0`, and all 155 locality selections used
  the requested hot set;
- the final frozen runtime passed the minimum accepted `spec` run (`seed=1`,
  256 actions) in 11,490 cycles. It produced all eight operation classes,
  exactly four heterogeneous overlap windows, six TLB flushes, all four DCache
  latency buckets, and a 387-cycle maximum response delay. The independent
  verifier accepted the frozen hashes, exact command options, coverage fields,
  and queue accounting; artifact SHA-256 is
  `be0646adb73e281ece4b2461f933972be8f4fba4abce2c0e03ecf808498102eb`;
- one final-frozen comparison used the same `random-mixed` generator for two
  4,096-action seeds under each shipped preset (24,576 total actions). All six
  seeds passed independent artifact, command-replay, backpressure, coverage,
  and queue-accounting verification. The constrained tails resolved to:
  `coverage=2281,1141,2279,1140,1140,3,4,2`,
  `spec=4916,2086,361,168,351,35,39,34`, and
  `corner=1671,1037,1619,1097,1041,473,533,519` for scalar load/store,
  vector load/store, prefetch, atomic, NC, and MMIO respectively. `coverage`
  formed 569 overlap windows per seed with compact delays; `spec` reached a
  400-cycle response and 158 TLB flushes; `corner` reached 396 cycles, 312 TLB
  flushes, and 413 dirty ReleaseData beats. Artifact SHA-256 values are
  `5655306119aa50a5e02cba60d1b00c4bc6086d8f6380a68486eb15b64d8f19f4`
  (`coverage`),
  `38e1b9628a8367086fd765fb354e7f7c3af40ba1d452b4e81133bde883ad59c7`
  (`spec`), and
  `01881180f71972ee4939ad0afd3dfeb6155995ff7d0efe0c56431d948df97290`
  (`corner`);
- `random-mixed --seed 31 --transactions 16384 --constraints spec` passed
  16,384 actions in 577,224 cycles. Its constrained tail produced 9,971 scalar
  loads, 4,187 scalar stores, 756 vector loads, 394 vector stores, 755
  prefetches, 86 atomics, 63 NC accesses, and 71 MMIO accesses. It observed 313
  TLB flushes and DCache latency buckets `1380,263,93,112`, with a 394-cycle
  maximum;
- `random-mixed --seed 37 --transactions 4096 --constraints corner` passed
  4,096 actions in 193,043 cycles. It included 223 atomics, 218 NC accesses,
  257 MMIO accesses, 144 TLB flushes, 218 dirty ReleaseData beats, and all four
  response-latency buckets, with a 396-cycle maximum;
- `atomic-dchannel-errors` passes on current complete RTL hash
  `774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`;
- neighboring `atomic-contracts`, `dcache-errors`, `uncache-errors`,
  `mmio-contracts`, `cbo-zero-contracts`, and `reset-recovery` also pass on
  that hash;
- older full-mode and long constrained-random runs are retained below as
  superseded evidence and will not be described as current-RTL acceptance.

The first attempts at the two constrained runs above found a UT reference-model
bug at dirty atomic line `0x802a0100`: AMO old-value checking and the local AMO
model advanced, but the architectural reference memory did not. A later
ReleaseData was therefore compared with stale initialization data. The fix
updates only the architectural reference after a successful AMO, leaving bus
memory unchanged until the checked ReleaseData arrives. Both original seeds
then passed without weakening the ReleaseData checker.

Frozen-boundary validation also exposed two UT framework defects, neither an
RTL failure. The old 128-action lower bound could not always fit four overlap
windows after the architectural prefix, and the offline verifier did not yet
recognize the new constraint command options. The lower bound is now 256 while
historical 128-action artifacts retain their enhanced coverage checks; command
options must exactly match campaign configuration.

Neighboring current-binary checks also pass: `vector-addressing` (including the
cross-16-byte ROB-head contract), `atomic-contracts`, `dcache-errors`,
`uncache-errors`, `mmio-contracts`, and a 4,096-action legacy `random-stress`
seed that reached 12 outstanding scoreboard entries.

The current RTL contains three confirmed CPU/MemBlock bug fixes. The first
described here concerns Uncache: denied or corrupt D-channel responses set
exception bits in `UncacheEntry`, but the
LoadUnit S1 NC path previously replaced those incoming bits with a TLB-only
exception value. NC requests do not query the TLB but still traverse S1, so the
response error was erased before scalar writeback. The adapter now preserves all
source-generated exception bits and S1 ORs them with TLB exceptions. MMIO is a
separate S0-to-three-cycle metadata bypass path and is not implicated by this
reproducer. The
`uncache-errors` scenario passes both denied and corrupt cases on the regenerated
RTL, and the generated MemBlock boundary now retains the corresponding
exception-vector fields. The full reproducer and root-cause evidence are in
[`CPU_BUG_UNCACHE_DCHANNEL_ERROR.md`](CPU_BUG_UNCACHE_DCHANNEL_ERROR.md).

The second bug was found by extending `atomic-contracts` with misaligned
`AMOADD.D` and `AMOOR.W` cases. `AtomicsUnit` returned the expected
`storeAddrMisaligned=0x40`, but propagated `uop.rfWen=1` on that exceptional
writeback. The output now masks `rfWen` whenever `exceptionVec` is nonzero;
the full finding, before/after behavior, and scope are recorded in
[`CPU_BUG_ATOMIC_EXCEPTION_RF_WEN.md`](CPU_BUG_ATOMIC_EXCEPTION_RF_WEN.md).

On 2026-09-07, that alignment coverage was expanded from two representative
operations to every exposed encoding and every illegal byte offset: 12
W-width operations x three offsets and 12 D-width operations x seven offsets,
for 120 cases. LR correctly reports `LoadAddressMisaligned`; SC, AMO, and
AMOCAS report `StoreAddressMisaligned`. All cases suppress RF write, add no
DCache request, and cross the ROB pointer wrap. The complete
`atomic-contracts` scenario passed at cycle 1656. No new CPU defect was found.

The apparent third atomic D-channel bug was a UT oracle error. MainPipe
intentionally installs denied and corrupt atomic refills together with
`DCacheExtraMeta.error`; later loads hit the poisoned line and re-report
`loadAccessFault` or `hardwareError`. Exceptional data is non-architectural and
cannot establish an architecturally visible atomic side effect. The corrected
`atomic-dchannel-errors` contract covers both errors across all 22
refill-capable W/D LR/AMO/AMOCAS operations, 44 poisoned-line readback hits,
SC.W/D hits on both metadata types, and two clean error-lifetime recoveries. It
passed in 7,108 cycles with the exact 46 cold TileLink requests.

During this iteration, a temporary harness defect changed the `mmio-contracts`
page mapping's PBMT=IO argument while adding the CBO.ZERO case. The existing
MMIO metadata oracle immediately rejected the mismatch (`isMMIO` expected 1,
actual 0); the call was restored, and the corrected scenario passed at cycle
818. This was a testbench configuration regression, not an RTL defect.

The first version of `l2-tlb-contracts` incorrectly assumed that warming an
ordinary DTLB port would also warm the independent L2-to-L1 requestor. Source
review and executable retry showed the precise contract instead: the first
request returns `miss=1` and starts a PTW transaction which refills the shared
prefetch TLB; after draining that walk, the same requestor hits. A separate
timing review found that `PMPChecker(leaveHitMux=true)` produces its retained
classification one cycle after the TLB response, so the helper now samples the
two payloads at their actual cycles. These were UT oracle defects, not CPU/RTL
defects.

The completed L2 boundary matrix additionally refills and retries PBMT=NC,
PBMT=IO, stage-1 page-fault, nested G-stage guest-page-fault, and PTW
access-fault results. Every retry hits without a new external PTW TileLink A
request. Exact PA/PBMT/PMP/PMA remain required for non-fault hits; fault hits
check only the fault result because the real L2 consumer drops their address
and protection payloads. The combined scenario passed in 1,967 cycles with 15
external PTW TileLink A requests. No CPU RTL defect was observed.

The first `store-rdata-order` run also exposed an oracle defect rather than an
RTL failure. Its exceptional StoreUnit address writeback carried the expected
`storeAddrMisaligned=0x40` and `TriggerAction.None=15`, but the test had
incorrectly expected zero (the zero-initialized value belongs to the separate
standalone store-data adapter). The expectation now uses `kTriggerNone`; the
current run passes at cycle 342 with both SQ entries dequeued. The failure did
not indicate a data-ordering regression.

The third confirmed repair is the historical VS-non-leaf GPA defect in
`VMergeBuffer`. It is repaired by leaving
the page-walk GPA unchanged when `isForVSnonLeafPTE` is asserted, while keeping
the first-active-element offset for the reported vector VA. The deterministic
sentinel and randomized boundary oracle both pass on the repaired hash.

On 2026-09-02, the mixed driver was corrected after reproducing two harness
false positives at long queue depths. Scalar store address/data writebacks are
now accepted only after the corresponding issue input handshake and issue
epoch; this handles the RTL's output-only `writebackStd` port, which has no
`ready` or ROB flag, without confusing stale valid pulses across ROB wrap.
The mixed-window commit boundary now holds `pendingPtr` at the last uop in the
window, preventing the next window's first scalar store from being implicitly
committed before its explicit commit operation. The contract suite gained two
checks for these rules.

Focused scenarios and 20 independent `random-mixed` seeds, each with 512
actions and backpressure enabled, passed after the fix. Seeds 1-20 completed
with no scoreboard, timeout, coverage, assertion, or queue-accounting failure.
These 20 short seeds are retained as historical pre-current-worktree evidence;
the current executable and RTL hashes are recorded at the top of this file and
in the direct-run sections below. The historical executable SHA-256 is
`1b689baead6f77c05488a8feab6f8a00bc19aebcae884b103d8404d79fbb2f29`;
the historical complete RTL SHA-256 is
`0b6c6aa34dc8aa148d0f6da91091df58e6622415136e60baf985d7560824e7e3`.

| Test | Result | Key observation |
| --- | --- | --- |
| Idle smoke | Pass | 38 cycles; registered DUT clock and internal reset release |
| Outer L2 flush bridge | Pass | 24 cycles; all four enable/done combinations, four enable transitions, five done transitions, and ten exact combinational/one-cycle timing checks |
| Top-level control and metadata bridges | Pass | 281 cycles and 267 continuous checks; eight power/halt/error combinations, all 256 interrupt-sink combinations, eight hart/reset patterns, all 32 L2-prefetch enable combinations, all four MSI/CLINT valid combinations, 67 shared perf-event lanes, and all 64 event values passed exact combinational/one-cycle/two-cycle timing oracles |
| Trace bypass transport | Pass | 48 cycles and 34 continuous checks; all eight three-group valid combinations, 16 itypes, 16 FTQ offsets, eight privileges, four encoder states, valid-gated holds, trap-only updates, and 50-bit address addition passed |
| DFT bridge transport | Pass | 30 cycles; all 1,024 combinations of seven SRAM-broadcast and three DFT-reset input bits matched ten frontend and four backend outputs; digest `0x5760755e9ffa3b83`; bridge routing only |
| Complete pin space | Pass | 749 inputs/7,155 bits and 586 outputs/5,434 bits; 256 patterns; digest `0xc36e86e25361ff60` |
| Cold-load refill, partial progress, and merge | Pass | Cycle 74 for two cold lines selecting opposite virtual-address bit-5 values: one ordinary and one `isKeyword` AcquireBlock, two exact 64-bit writebacks, and two GrantAcks. A separate same-line pair completed two exact loads in 172 cycles from one AcquireBlock held for 128 cycles. A partial-refill load wrote back at cycle 44 after only its critical beat; the delayed second beat drained by cycle 300 with no duplicate writeback |
| Vector loads | Pass | Four EEWs, both vector lanes, four exact 128-bit results |
| Vector addressing | Pass | Cycle 99,634; all 78 legal ordinary unit-stride and strided EEW/SEW/LMUL/EMUL configurations plus all 78 configurations for each of indexed-unordered and indexed-ordered passed exact load/store/readback. The indexed matrix covered 1,016 load and 508 store uops, 56 `EMUL>LMUL` shared-Vd configurations, ordered forward issue, unordered reverse issue, 4,608 LQ/2,304 SQ allocations, queue wrap, and two ROB wraps. Directed alias/non-monotonic indexed and all 16 whole-register NF/EEW cases remain included; 1,975 load and 983 store writebacks issued 1,064 TileLink requests |
| Vector segment | Pass | Cycle 905,375; all four addressing modes each covered 338 legal NF 2..8 x EEW/SEW/LMUL/EMUL configurations. The non-indexed pair produced 7,096 load/readback and 3,548 store uops; the indexed pair produced 7,264 load/readback and 3,632 store uops, including 252 index-only uops. Exact data/metadata/readback, 4,700 TileLink requests, zero segment LQ/SQ allocations, and 24 matrix ROB wraps passed. Active-segment redirect is intentionally excluded by the full-core serialization contract |
| Vector split load | Pass | Three checked writebacks including a split cold-load replay shape |
| Store forwarding | Pass | Four store widths and four matching scalar loads |
| Vector forwarding | Pass | Four vector stores and loads with byte-accurate SQ overlay |
| PBMT=NC store order | Pass | Two stores, two SQ dequeues, two PTW requests, one uncache request |
| Uncache D-channel errors | Pass | One denied and one corrupt response each reached scalar exception writeback; two uncache requests |
| Uncache widths/byte lanes | Pass | 29 scalar NC loads across all seven opcodes and legal 8-byte-beat lanes; 29 uncache requests, two request stalls, 90 response-delay cycles |
| MMIO metadata/error, PMA edge, and device side effects | Pass | PBMT phase cycle 1,258: one normal, one denied, and one independent-corrupt IO load plus one cold-TLB IO store. The three loads entered through all load-unit lanes and each produced exactly one ROB `loadMmio` pulse before its 64/128/256-cycle response, with no duplicate through final cleanup; a lane-permuted same-cycle bare PMA-MMIO triple covered all three compacted outputs in exact ROB order and one common cycle. Cacheable/PBMT-NC controls produced zero pulses and selected one DCache/one Uncache request. Bare PMA phase cycle 488: a non-DebugModule `c=0` load/store pair passed, guarded DebugModule access faulted with no manager request or uncanceled wakeup, and exact loads at `0x7ffffff8`/`0x80000000` selected one Uncache/one DCache request. A 799-cycle seven-access device log proved read-clear behavior, zero side effects for denied/independent-corrupt reads and a denied write, exact offset-`SW` fields, recovery, no duplicates, and zero DCache requests; corrupt data-less `AccessAck` is rejected as illegal. An 803-cycle phase queued three reads under 512/128-cycle delays; a 689-cycle phase pre-issued `load -> SW -> load` under two 256-cycle delays. Both required external depth one and exact device state/order |
| CBO.ZERO cache-line zeroing | Pass | Cycle 370; cacheable `0x7` CBO.ZERO used the StoreQueue/SBuffer `wline` path, survived one forced DCache A stall and four response-delay cycles, produced exact non-MMIO store metadata, and a pre-mirror cache readback returned an all-zero line; no Uncache request was emitted |
| Atomic operations and exception metadata | Pass | Main phase cycle 1,656: all 9 W-width and 9 D-width AMOs, AMOCAS.W/D compare success/failure, LR/SC success/failure, and a 120-case matrix crossing all 24 exposed encodings with every illegal W/D byte offset plus ROB wrap. LR returned load-misaligned while SC/AMO/AMOCAS returned store-misaligned; all suppressed `rfWen` and added no DCache request. A separate 39-cycle device-PMA `AMOADD.D` produced `StoreAccessFault`, suppressed `rfWen`, preserved memory, and emitted no DCache/Uncache request |
| Atomic D-channel errors | Pass | Cycle 7,108; 22 W/D LR/AMO/AMOCAS operations crossed with denied and corrupt; all 44 later loads hit poisoned lines and re-reported exact errors, four SC hits reported cached errors, two clean AMO recoveries passed, exceptional `rfWen` stayed suppressed, and exactly 46 cold requests were issued |
| Hypervisor memory operations | Pass | 82 cases in 18,986 aggregate cycles: all encodings, four nested mode pairs, five PBMT combinations, misaligned split paths, 18 M-mode/SPVP physical-PMP crosses including six locked R/RWX cases, three interior fixed-PMA device cases, and six exact `0x80000000` PMA-boundary cases. HLV required R, HLVX required R+X, and HSV required W; device-side HLV/HSV used Uncache while HLVX faulted, and DDR-side operations used DCache; 12 total access faults, 704 PTW requests, and 51 DCache requests matched the oracle |
| Concurrent exception priority | Pass | Cycle 1,351; wrapped/reversed queue age, same-ROB vector-uop order, cross-cause and scalar/vector replacement all passed. Two additional vector-load/store pairs populated the exception buffers in opposite arrival orders; toggling `isStoreException` selected and restored each exact source VA. Totals were 11 scalar-load, six vector-load, and six scalar-store writebacks |
| Data-side PMP contracts | Pass | 17 cases in 708 aggregate cycles: TOR/NAPOT exact edges, 4-KiB-grain NA4 WARL conversion, R/W and AMO denial, overlap priority, M-mode unlocked bypass, lock enforcement, and locked address/config immutability; 9 allowed and 8 denied with zero forbidden manager requests |
| L2-to-L1 DTLB boundary | Pass | 1,967 aggregate cycles; cold requests refilled then retried as exact cacheable, PBMT=NC, PBMT=IO, stage-1 PF, nested GPF, and PTW AF pftlb hits without an additional external PTW TileLink A request. The cacheable hit returned PA `0xa0056018`, PMP allow, and cacheable PMA classification; a locked 4-KiB NAPOT entry changed the resident hit to PMP deny without changing translation. A prefetch request returned a legal cold miss, `no_translate=1` exercised the pruned-zero-address MMIO contract, `kill=1` produced no response for 128 cycles, and all 16 source IDs x both L2-hint polarities produced no ghost traffic; total external PTW TileLink A requests were 15 |
| PTW D-channel errors | Pass | 16 fault cases plus 16 same-address clean recoveries in 3,466 aggregate cycles: eight stage-1, four isolated G-stage, two fully nested, and two bitmap reads; load/store and denied/independent-corrupt each split 8/8, with corrupt split 4/4 over first/last beat. Denied was fixed across both beats and also asserted corrupt as required for data responses. The 111 PTW requests proved exact fault cutoff and post-fence reread of every failed block; faulting accesses reached neither DCache nor Uncache, while every recovery returned exact data through one DCache request |
| IFU-to-Mem PTW bridge | Pass | 50 cases in 30,172 aggregate cycles: valid Sv39/Sv48, all four nested pairs, Sv39/Sv48 VS-only and G-only, PBMT=NC/IO, invalid L0 leaves, and all eight Sv39 sector indices. The sector matrix checked exact `valididx` masks `0x29/0x42/0x94`, all eight selected `ppn_low` values, one-hot `pteidx`, and reconstructed PPN; the first request issued exactly three PTW reads and the remaining seven issued none. Six stage-1/G-stage leaf/non-leaf `G` cases, all four nested pairs crossed with VS-leaf/final-G-leaf/implicit-page-table-G faults, a 256-cycle delayed IFU walk overlapped with a cold scalar DTLB walk, and two same-VPN requests coalesced into one three-request Sv39 walk with two exact responses. Stage-1 global input was reported in three leaf cases and conservatively demoted in three non-leaf cases; G-stage raw `G` was reported in two leaf cases and demoted in two non-leaf cases without changing functional results. Eight delayed-walk races cover stage-1 and nested context replacement, global/selective `SFENCE.VMA`, and global/selective `HFENCE.VVMA`/`HFENCE.GVMA`, suppressing every stale response for 1,024 cycles before checking the exact replacement mapping; 281 PTW requests, manager outstanding depth 2, exact active-stage/fault/load results, and 255 response-stall cycles passed; RTL SHA-256 `97b1339a74d458a48a1c58fad766a22cc9dac000cb297e303501311bf47d3b39` |
| Reset recovery | Pass | 576 aggregate cycles; three repeated-reset phases accepted and canceled DCache-refill, PTW-walk, and Uncache/MMIO traffic under 256-cycle delayed responses, then completed three distinct post-reset survivors with no stale response/writeback |
| Scalar misalignment under RAR pressure | Pass | The original five within-beat/line/page cases completed in 558 cycles. A separate legal queue-pressure phase held three split loads at the LQ head, completed 60 younger aligned loads on all three lanes, then advanced the split loads in ROB order; all 63 exact writebacks drained in 1,785 cycles with zero `memoryViolation` pulses |
| Store TLB-miss preservation | Pass | Cycle 156; two misses and two PTW requests; allocated SQ entry remained address-valid |
| DCache dirty release | Pass | Ten stores; two ReleaseData writebacks preserved |
| Redirect | Pass | Canceled miss suppressed; LQ slot reused |
| Queue pressure | Pass | Two legal 60-entry waves; 120 checked writebacks |
| Stateful mixed seed 1 | Pass | 96 actions; scalar/vector load/store, all scalar/vector address modes, `prefetch.i/r/w`, Sv39/Sv39x4, PBMT-NC, cross-forwarding, dirty ReleaseData, redirect recovery, six manager backpressure classes, exact LQ/SQ drain |

## Earlier Current-Worktree 32K Direct Runs

After the atomic exceptional-writeback fix and the store-rdata oracle
correction, the pre-matrix-summary executable completed an independent
32,768-action pair. Both runs used complete RTL SHA-256
`670cf5d399c55e40c9d51c70183315b4cdd730e73843543aec5789414558b846` and
executable SHA-256
`bb25d12211d34a93a666a2835a7e7c0db1f87b21e508b56341b5ca35a4c90d86`.

| Scenario | Result | Observed coverage |
| --- | --- | --- |
| `random-stress --seed 83 --transactions 32768` | Pass | 713,485 cycles; 32,298 TileLink requests; 12 maximum outstanding; 2,007 scalar misaligned operations; 5,461 forwarding overlaps; all required stress combinations; four RNG streams; 10,826 request-stall and 135,240 response-delay cycles; LQ/SQ `38072+0/38072`, `27150+0/27150` |
| `random-mixed --seed 89 --transactions 32768` | Pass | 933,358 cycles; 14,052 scalar, 4,670 prefetch, 4,683 scalar-store, 9,355 vector-load, and 4,677 vector-store writebacks; 15 PTW, two Uncache, and 10 ReleaseData transactions; nested translation, three exception waves, redirect, dirty data, four forwarding classes, all dispatch lanes, both vector stride signs, and all manager backpressure counters nonzero; LQ/SQ `71250+1/71251`, `33213+0/33213` |

## Superseded Pre-Clarification 64K Direct Runs

To extend the request count beyond the normal 16K/32K campaigns, the
pre-clarification executable completed a 65,536-action stress/mixed pair. Both runs
passed with complete RTL SHA-256
`b69e387eb081a3f311311079ade435206817c7c6a20bd8f3a5f11889ec1dcbf4`.
That RTL contained `8eedb3ad0`; these results are historical rather than
current-RTL acceptance evidence.

| Scenario | Result | Observed coverage |
| --- | --- | --- |
| `random-stress --seed 43 --transactions 65536` | Pass | 1,427,238 cycles; 64,740 TileLink requests; 12 maximum outstanding; 4,078 scalar misaligned operations; 10,923 scalar and 10,922 vector forwarding overlaps; four RNG streams; all required stress combinations; 21,278 request-stall and 271,007 response-delay cycles; LQ/SQ `76294+0/76294`, `54453+0/54453` |
| `random-mixed --seed 47 --transactions 65536` | Pass | 1,861,069 cycles; 28,098 scalar, 9,351 prefetch, 9,365 scalar-store, 18,714 vector-load, and 9,358 vector-store writebacks; 15 PTW, two Uncache, ten ReleaseData, nested translation, three exception waves, redirect, dirty data, all forwarding classes and dispatch lanes, both vector stride signs, and all six manager backpressure counters nonzero; LQ/SQ `143506+1/143507`, `66307+0/66307` |

## Superseded Pre-Clarification Full-Mode Sweep

All 46 scenarios then registered by the UT executable completed at least one
passing run on the superseded hash above: 39 deterministic modes and seven random
modes. The larger supplemental random runs were:

| Scenario | Result | Observed coverage |
| --- | --- | --- |
| `random-loads --seed 53 --transactions 8192` | Pass | 182,480 cycles; 8,192 checked writebacks across all seven scalar load operations and three issue lanes; 7,323 hits and 869 misses |
| `random-vector-loads --seed 59 --transactions 8192` | Pass | 218,030 cycles; 8,192 checked vector writebacks, 16,384 balanced LQ allocations/dequeues, 845 TileLink requests, 18 releases, all four EEWs, both lanes, mask states, `vstart`, full/partial `vl`, and aligned/split addresses |
| `random-boundary-hunt --seed 71 --transactions 4096` | Pass | 4,096 constrained-random VS-non-leaf boundary cases; zero failures |

The specialized scalar/vector forwarding scenarios also passed, but their
drivers intentionally clamp a single run to 48 and 24 transactions. High-count
forwarding pressure is supplied by `random-stress`, which observed 10,923
scalar and 10,922 vector forwarding overlaps in the 65,536-action run.

## Historical 32K Direct Runs

The rebuilt executable also completed one 32,768-action pair with independent
seeds and randomized manager backpressure. These are direct binary runs, not
short smoke tests. They used the pre-L2-contract harness binary
`74371d94790266646d4093a6c450fed4916cab85f0092593bad08e40829f3677`; the
complete RTL hash for that historical run was `c97a89cd...`. The subsequent
L2-only harness additions and the atomic exception fix changed the current
binary and RTL hash, which are recorded at the top of this file.

| Scenario | Result | Observed coverage |
| --- | --- | --- |
| `random-stress --seed 59 --transactions 32768` | Pass | 713,251 cycles; 32,289 DCache requests; 12 maximum outstanding; 2,051 scalar misaligned operations; 5,461 forwarding overlaps; 5,460 masked and 5,462 unmasked vector operations; four vector mode combinations; 10,766 request-stall and 134,830 response-delay cycles; LQ/SQ `38062+0/38062`, `27140+0/27140` |
| `random-mixed --seed 61 --transactions 32768` | Pass | 931,075 cycles; 14,056 scalar, 9,351 vector-load, 4,676 vector-store, and 4,684 store writebacks; 15 PTW and two Uncache requests; 10 ReleaseData writebacks; nested translation, three exception waves, redirect, five forwarding classes, four dirty lines, and all six manager backpressure counters nonzero; LQ/SQ `70554+1/70555`, `32874+0/32874` |

The early idle writeback observation was a harness error: the Picker clock had
not been registered. It is not an RTL defect.

## Historical Mutation Checks

Six 2026 LSU fixes were reverted independently from the baseline. The same
checked test sources and generated SVA were compiled against each mutant; the
executable-reported hash was required to match that mutant's complete RTL
metadata. Four mutants fail a stable oracle. Two pass the best currently legal
scenario and are retained as negative evidence, not claimed as reproduced.

| Reverted fix | Mutant complete RTL SHA-256 | Clean result | Revert result |
| --- | --- | --- | --- |
| `e541289b19a5661536d4e4a0d01d2abc9a37b1f0` (2026-08-10), preserve SQ address state on a TLB miss | `c525bef7dbb231ace69254e57b6fc7d7069dcb10ddea17176d548c9277565b8e` | `store-tlb-miss-preserve` passed at cycle 156 with two misses and two PTW requests | Failed at simulation tick 283: allocated entry 0 lost `addrvalid` on a TLB miss |
| `e12436c7cba86b195deec24981976d78bc263661` (2026-08-14), prevent out-of-order `rdataPtr` advance | `757dfc00827ed605d63db23a2d9bc995732628abf18b4b6cc203ef8190c49a30` | `store-rdata-order` passed at cycle 309 with two ordered SQ dequeues | Failed at cycle 309: the NC store used out-of-order SQ read data |
| `45318c5d` (2026-04-20), clear RF write enable on a load exception | `341dff7baa1442e4050c131cc4b2a9f864d1facbf584cdecdabfb81bb6c32ffb` | `exception-contracts` passed with exact page-fault and no RF write | Failed: exceptional scalar load requested a scalar RF write |
| `856b821f` (2026-08-11), propagate vector VS-non-leaf-PTE metadata | `3abf90d701fdb3252ec68d1426707affe4d0606bb002a1b02d710be577baa5a7` | `vector-guest-fault` passed at cycle 152 with marker 1 and exact VA/GPA | Failed at cycle 152: marker was 0 while VA/GPA stayed correct |
| `9ee7b335` (2026-08-10), misaligned vector-store progress | `9000f90adc416d1de6c7b4e8ce2b0129cf98564431ae835bcfc6d857f89e4df2` | `misaligned-stores` passed at cycle 879 | Mutant also passed; not reproduced |
| `fbb1e349` (2026-07-21), cross-page vector-store `s_block` progress | `603720f0cb797e679097244a107ca2892f00bbfe0392327fdc935ee2093b2594` | `misaligned-stores` passed at cycle 879 with three vector replays | Mutant also passed; not reproduced |

The TLB-miss scenario first establishes a hit on an allocated SQ entry without
sending store data or allowing dequeue. The SVA antecedent requires that entry
to be both allocated and address-valid, records its ROB/uop identity, and only
checks the next cycle if the same entry remains allocated. This excludes
dequeue, redirect cancellation, and same-slot reallocation false positives.

Every mutant RTL tree was regenerated from its independently reverted Scala
source. A full generated-RTL search found no temporary `SQPROBE`, `TLBCHK`, or
diagnostic `$display` instrumentation. Apart from the generated RTL hash
comment, mutants use the same final C++ harness and generated SVA semantics as
clean. `HISTORICAL_BUG_AUDIT.md` records the strict status and boundary contract
for all 58 commits in scope.

The source hashes below identify the harness revision used for the historical
results above. They are retained as historical provenance only; the previous
artifact did not enforce them:

- historical `memblock_main.cpp`: `2b8a372c88565e26231ecdd87f917335b0f1c2a2980bad7590cd0502e12acc10`;
- historical `memblock_env.hpp`: `f1a638a87547df505696a05200350e1c1b0fa79f15947f9325278a89d77563ad`;
- historical protocol SVA: `31feec579cf939d04f446114071e5860e17ed990e8c680a67f6f9b5c0c91ff6d`.

The previous runner did not hash these source files, so these values were not
independently enforced by the JSON artifact. The reviewed runner now records
and verifies source hashes as controller inputs. The current eight-hour
artifact above is provenance-complete for this reviewed harness revision.

The current mixed summary reports vector load and store address-mode coverage
separately (`vec_load_modes` and `vec_store_modes`), so a load cannot mask a
missing store mode. Each backpressure-enabled mixed seed also reports nonzero
DCache, PTW, and uncache request-stall and response-delay counts.

## Development Regression

On 2026-08-30, a four-process mixed run completed eight seeds in each of four
scenarios:

- 8,000 randomized scalar-load transactions;
- 8,000 randomized vector-load transactions with 16,000/16,000 LQ entries retired;
- 384 randomized scalar store-forwarding transactions;
- 192 randomized vector store-forwarding transactions;
- all scalar/vector widths and all scalar/vector issue lanes covered;
- randomized TileLink A/D delays enabled;
- zero scoreboard, assertion, timeout, or RTL failures.

Artifact: `build/memblock/regression.json` (generated, not tracked).

The enhanced stateful mixed test was then checked across seeds 1-12 with 64
actions per seed. All 768 actions passed per-seed coverage gates. Each seed had
three PTW requests, two uncache requests, at least five dirty ReleaseData
transactions, two simultaneous scalar/vector issue points, both cross-type
forwarding directions, all three software-prefetch operations, one redirect,
and exact final queue accounting. A four-seed all-scenario regression also
passed 20/20 scenario invocations and 960 reported actions.

Artifacts: `build/memblock/mixed-short.json` and
`build/memblock/regression-short.json` (generated, not tracked).

On 2026-09-01, the prior fully frozen runtime also passed all 12 focused scenarios
and an eight-seed, five-scenario matrix:

- 12/12 focused scenarios passed, including complete pin space, vector
  load/store forwarding, DCache dirty release, redirects, both StoreQueue
  historical-bug scenarios, and queue pressure;
- 40/40 random scenario invocations passed;
- 17,088 reported transactions covered scalar/vector loads, scalar/vector
  forwarding, and stateful mixed traffic;
- runtime, external-library, and complete RTL hashes were consistent.

Artifacts: `build/memblock/runtime-short.json` and
`build/memblock/runtime-matrix.json` (generated, not tracked).

## Historical Extended Campaign (Stale Artifact)

An earlier eight-process duration-based `random-mixed` campaign was intended to
run for four hours against the historical baseline RTL. Its original artifact
was overwritten during development by a one-second smoke artifact and is no
longer available for independent verification. The numbers below are retained
as historical notes only and must not be used as acceptance evidence:

- requested duration: 14,400 seconds;
- measured monotonic elapsed time: 14,400.633 seconds;
- 215,359/215,359 consecutive seeds passed, from seed 1 through seed 215,359;
- 13,782,976 stateful mixed actions completed (64 per seed);
- every result reported return code zero and the same complete RTL SHA-256
  `39709aa5225aa56ce6764569bbcbd20089ff25ff89eafaf0d3e7b9e3632ea815`;
- simulator, Verilated model, xspcomm, five resolved system libraries, runner
  source, runtime manifest, and RTL metadata had identical before/after hashes;
- no assertion, scoreboard, timeout, process, coverage-gate, or RTL failure was
  observed.

Artifact: `build/memblock/extended-mixed-frozen-4h.json` is currently a stale
one-second development artifact (not tracked), SHA-256
`3943cd27585ffc0b36c35b7d8ed3dd8c225da4bf0dc3e290bf66632c5523af0f`.
`make verify-extended-results` is expected to reject it because its duration and
provenance do not satisfy the four-hour gate. The current frozen eight-hour
campaign above is the duration artifact accepted for the repaired RTL.

## Earlier Extended Campaign

On 2026-09-01 (Asia/Shanghai), the mixed-test executable completed a
four-process duration-based campaign against the baseline RTL:

- requested duration: 14,400 seconds;
- measured monotonic elapsed time: 14,400.555 seconds;
- 100,132/100,132 seeds passed, with no failures, errors, or timeouts;
- 6,408,448 stateful mixed actions completed (64 per seed);
- every result reported `random-mixed`, return code zero, and the same complete
  RTL SHA-256 `39709aa5225aa56ce6764569bbcbd20089ff25ff89eafaf0d3e7b9e3632ea815`;
- the executable SHA-256 remained
  `30d9758abd3fc0a36e46d5b4ef24ac64e6e7ea7c6e2a8485f95c7ded7b1d8aff`.

Artifact: `build/memblock/extended-mixed-4h.json` (generated, not tracked),
SHA-256 `307e9006ac5bd316f2defebc756d13b2b141cb727e0beba768a0d9f7e86d9a6d`.

This earlier artifact hashes the executable and complete RTL, but the
executable dynamically loaded model and xspcomm shared libraries outside the
frozen directory. It is retained as behavioral evidence, not used as the final
immutable-runtime provenance result.

## Worker Scaling

On the 24-logical-CPU verification host, the same 256 `random-mixed` seeds
(16,384 actions) took 33.087 seconds with four workers and 17.190 seconds with
eight workers. Both runs passed and reported the same RTL SHA-256. Eight workers
were therefore selected as the default for normal and duration-based regression.

## Current Boundary Finding

The historical clean baseline had a deterministic vector guest-fault split
candidate: `vector-guest-fault-split` reported GPA `0x94001808`, while the
independent VS/G-stage page walk required `0x94001800`. The conditional
`VMergeBuffer` fix was regenerated into the current RTL; scalar, aligned-vector,
split-vector, and randomized boundary controls now all report the exact oracle
GPA. The clean-RTL failure remains in `CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md` as mutation
evidence, while the repaired test is part of the green sentinel gate.

The 2026-09-06 pre-merge eight-hour, 16,384-action-per-seed campaign passed its
independent final artifact check. Because later translation controller changes
alter the frozen harness, this remains provenance-complete pre-merge evidence;
final acceptance of the merged harness requires a new freeze and campaign.
Historical baseline and mutation results above remain evidence and are not
claims about the repaired RTL.

## Historical Pre-Review Six-Hour Campaign

On 2026-09-03 (Asia/Shanghai), the repaired frozen runtime completed the final
fully mixed campaign:

- requested duration: 21,600 seconds;
- measured monotonic elapsed time: 21,637.196513 seconds;
- 3,802/3,802 continuous seeds passed, from seed 1 through seed 3,802;
- 15,572,992 constrained-random mixed actions completed (4,096 per seed);
- every result used scenario `random-mixed`, return code zero, and complete RTL
  SHA-256 `0b6c6aa34dc8aa148d0f6da91091df58e6622415136e60baf985d7560824e7e3`;
- frozen binary, Verilated model, xspcomm, resolved system libraries, runtime
  manifest, controller inputs, and RTL metadata were unchanged before/after;
- no assertion, scoreboard, timeout, queue-accounting, process, or coverage-gate
  failure occurred.

Artifact: `build/memblock/final-frozen-6h.json`, SHA-256
`704c403d1470846143d24bdb90e0587c6b5e16840aa8be89741e3ed6af30a4e5`.
`make verify-final-results` independently validated the artifact and reported
`MEMBLOCK_REGRESSION_ARTIFACT_PASS` for seeds `1..3802`.

This is evidence that the repaired RTL satisfies the tested contracts under the
pre-review harness. It is not a proof that untested MemBlock boundary gaps are
bug-free, and it is not the acceptance artifact for the reviewed oracle/source
provenance changes described above.
That historical campaign did not expose an additional CPU bug. The later
current-worktree Uncache finding is documented separately in
`CPU_BUG_UNCACHE_DCHANNEL_ERROR.md`; historical mutant results remain the evidence for
the four independently reproduced LSU defects listed above. Any future failure
should be triaged from its recorded seed and runtime provenance rather than
treated as a known-good result.

## Schema 11 Vector Policy Closure

On 2026-09-07 the common `random-mixed` interface added independent ordinary
vector constraints for masking, `vma`, `vta`, partial VL, and nonzero `vstart`.
The scoreboard now classifies mask/tail inactive elements by global element
number across ordinary, indexed-special, and segment uops before applying the
retained-or-all-ones agnostic oracle.

Five intentionally short 256-action checks passed against complete RTL SHA-256
`e3250bd4594a3f5594b2fe5e215ddf16b89dd121498a72953b2500eecf61fcf8`:
coverage seed 13 hit both sides of every policy and observed mask/tail agnostic
loads `4,5`; load-only seed 16 fixed all five policies to zero and reported only
false classes; load-only seed 17 fixed all five to 1000 and reported only true
classes with agnostic observations `15,15`; seed 19 expanded every constrained
M8 instruction to eight uops (120 uops total) with both agnostic classes; seed
20 checked VTA on the MF8/VLMAX=2 fractional tail and observed `0,15`. An enabled
MF8/SEW8 shape combined
with fixed masked+partial-VL+nonzero-vstart was rejected before cycle 0 because
its VLMAX cannot satisfy the combination. No CPU RTL defect was observed.

The final frozen-runtime seed 21 completed 256 actions in 17,397 cycles and
7.071575 seconds. It covered 24 constrained ordinary vector instructions, 60
uops, 14 multi-uop instructions, all shape classes, both sides of all five
policy dimensions, and agnostic observations `4,8`. The independent schema-11
artifact verifier accepted `/tmp/memblock-schema11-seed21.json` with SHA-256
`afe2f9b81dd95b7931a22326a651d6296c71d7bd3992ac29b27035ef6cc26b34`;
the frozen executable, model, xspcomm, RTL metadata, controller sources, and
resolved libraries remained hash-stable.

## Independent LSQ Enqueue Accounting

On 2026-09-07 the environment began sampling all six top-level LSQ dispatch
lanes before each functional clock edge. It independently classifies LQ/SQ
allocation from `needAlloc`, adds the observed `numLsElem` flow count, rejects
invalid valid/allocation/zero-flow combinations, and uses the observed width
and lane histograms for `random-mixed` coverage. The existing driver counters
remain as an independent expectation and must equal the monitor totals before
any public operation can pass.

`make unit` passed 178 tests and `make smoke` passed in 38 cycles. Frozen
coverage seed 22 then completed 256 actions in 17,880 cycles and 7.254363
seconds. It observed LQ/SQ enqueue totals `442,271`, matching queue accounting
`441+1/442` and `271+0/271`; all six dispatch widths and lanes were nonzero.
The independent verifier accepted
`/tmp/memblock-lsq-monitor-seed22.json` with SHA-256
`5cc86c0770dbb0f4e0e16dc74d004b65e3e9be6381de0a52897ee99993a70719`
against complete RTL SHA-256
`e3250bd4594a3f5594b2fe5e215ddf16b89dd121498a72953b2500eecf61fcf8`.
The frozen executable SHA-256 was
`1c6fdb44860ed97d32e049bf3a89ab0b8d985added55296e789ce1d8c94e14fb`.
No CPU RTL defect was observed; this was verification-environment closure.

## Independent Redirect Cancellation Accounting

On 2026-09-07 the environment stopped inferring cancellation from the driver
after a redirect. MemBlock registers the redirect once, and the virtual load
and store queues publish retained cancellation counts two further clock edges
later. The environment now samples `lqCancelCnt/sqCancelCnt` exactly once at
that boundary, adds the observed values to queue conservation, and separately
labels any non-redirect exception/reset teardown that cannot use this oracle.
The Schema-2 mixed gate requires every cancellation to be observed.

`make unit` passed 178 tests and `make smoke` passed in 38 cycles. The directed
redirect case passed in 166 cycles with one redirect event and exactly one LQ
cancellation. DCache-error, trigger, and Uncache-error scenarios passed in
their short directed runs. Coverage seed 22 completed 256 actions in 17,885
cycles, reporting `redirect_cancels_observed=1,1,0`,
`unobserved_cancels=0,0`, LQ `441+1/442`, SQ `271+0/271`, and independently
observed enqueues `442,271`. The frozen-runtime run took 7.153940 seconds;
the independent verifier accepted
`/tmp/memblock-redirect-monitor-seed22.json` with SHA-256
`f9c0d083ead4d7f517485c08759fc718ba5efd2bb8b64f86da199ad95314a130`.
The frozen executable SHA-256 was
`b0072aab194f2e4af0b31bdd25367bd6fead52b0d24cb638dd270857e47c9555`.
No CPU RTL defect was observed.

## Schema 14 Random Probe/Refill Overlap Closure

On 2026-09-07 the common `random-mixed` interface added `probe-overlap`. An
overlap sequence warms a dedicated clean line, starts an unrelated cold scalar
load whose DCache response is held for 2048..4096 cycles, then queues the clean
auxiliary Probe and the existing dirty primary Probe without an intervening
cycle. The manager assigns distinct B-source IDs; the oracle requires both
address-matched C responses, byte-exact primary dirty data, no delayed-load
writeback before those responses, and an accepted-but-unanswered Probe depth
of at least two. The delayed refill and GrantAck are then fully drained.

Seven 256-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Overlap false/true | Probes | Maximum Probe depth | Maximum DCache latency |
| --- | ---: | ---: | --- | ---: | ---: | ---: |
| `coverage` | 14001 | 28,107 | 1/1 | 10 | 2 | 2,864 |
| `spec` | 14002 | 25,589 | 1/1 | 7 | 2 | 2,626 |
| `corner` | 14003 | 46,265 | 1/2 | 13 | 2 | 3,413 |
| `coverage`, overlap fixed to 0 | 14004 | 23,631 | 4/0 | 10 | 1 | 2,319 |
| `coverage`, overlap fixed to 1000 | 14005 | 32,573 | 0/2 | 11 | 2 | 3,325 |
| `coverage`, controller artifact | 14006 | 41,655 | 1/1 | 16 | 2 | 3,116 |
| `coverage`, frozen runtime | 14007 | 38,478 | 1/1 | 13 | 2 | 2,930 |

Schema 14 reports and conserves primary Probe sequences, toB cleanup Probes,
CMO-derived Probes, and one auxiliary Probe per overlap. Zero and 1000 enforce
strict disabled/enabled bins; intermediate rates require both. All 185 Python
unit tests passed. The independent verifier accepted the frozen artifact with
SHA-256
`f3b3e3f2a31baf102d70736efab583d60252f105da93e16582cb4d6bdd46e675`;
the frozen executable and runtime-manifest SHA-256 values are
`648db97b007d0855d733cd899e7019da8f88698dab3b11cbdaa832a1df9cc862`
and `d8580e9747c905675ef2e5ffddbf19ff6f91d9fdb27576fe9e717191fa1ba526`.
No CPU RTL defect was observed; this closes a verification-environment
concurrency gap.

## CMO Functional And Error Closure

On 2026-09-07 the DCache agent added the custom TileLink CMO operations
`CBOClean=12`, `CBOFlush=13`, `CBOInval=14`, and `CBOAck=8`. It checks fixed
source 17, size 64, line alignment, source lifetime through Ack, delayed
completion, and the manager-derived B/C permission transition. The focused
scenario delays CBOAck by 1,024 cycles so the expected Probe must complete
first. Every operation is crossed with clean and dirty state: CLEAN returns
TtoB ProbeAckData or no-data BtoB and retains a readable Branch line; FLUSH
and INVAL return dirty TtoN ProbeAckData or clean no-data BtoN, invalidate,
and force a refill. All three dirty cases begin with a committed store still
buffered and apply no
testbench flush; the exact Probe data and final empty boundary prove that CMO
execution drained the stores before completing.

The first denied case exposed a CPU RTL bug: StoreQueue mentioned
`cmoOpResp.denied/corrupt`, but those assignments were nested under an
unrelated Uncache response handshake. Pre-fix complete RTL hash
`97b1339a74d458a48a1c58fad766a22cc9dac000cb297e303501311bf47d3b39`
wrote back CLEAN with zero exception instead of StoreAccessFault. The root
cause, CPU impact, reproducer, and fix are recorded in
`CPU_BUG_CMO_DCHANNEL_ERROR.md`. The fixed complete RTL hash is
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`.

The fixed run produced:

```text
MEMBLOCK_CMO_CONTRACTS_PASS operations=3 line_state_cases=6 dirty_probe_data=3 automatic_sbuffer_drains=3 retained_hits=2 invalidation_refills=4 flushed_younger_loads=1 concurrent_cycles=5214 denied_cases=3 corrupt_cases=3 positive_cycles=8167 error_cycles=2455 cmo_ack_delay=1024 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057
```

All six operation/error crosses preserve bus memory, report the exact
StoreAccessFault or HardwareError, complete with `flushPipe=1`, bypass Uncache,
and conserve SQ entries. A separate legal overlap accepts a younger cold load
on another MSHR while CBOAck is delayed, then applies CMO's `flushAfter`,
observes exactly one LQ cancellation, drains the 2,048-cycle delayed response,
and rejects any resulting load writeback. `mmio-contracts`, `uncache-errors`,
`cbo-zero-contracts`, and `dcache-coherence` passed on the same regenerated
model. The RTL fix is isolated in commit `42152f6ba`.

## Schema 13 Random CMO Closure

On 2026-09-07 CMO became an operation class in the common `random-mixed`
constraint interface. CLEAN, FLUSH, and INVAL have independent weights, while
`cmo-dirty` and `cmo-younger-overlap` steer line state and whether a delayed
younger cold load occupies another MSHR. Every action checks the exact CMO A
request and operation/state-derived Probe response, holds CBOAck for a random
128..1024 cycles, and, when overlap is selected, delays the younger refill
beyond CBOAck and requires exactly one LQ cancellation with no terminal
writeback. CMO follows the active Bare/stage-1/nested context; translated cases
use identity-mapped 2-MiB leaves. The schema-13 terminal record and independent
verifier conserve all three CMO subclass counters against the CMO operation
count.

Five 256-action dynamic runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Cycle | CMO CLEAN/FLUSH/INVAL | Clean/dirty | No-overlap/overlap | Maximum DCache latency |
| --- | ---: | --- | --- | --- | ---: |
| `coverage`, seed 1 | 31,618 | 2/2/3 | 3/4 | 3/4 | 1,815 |
| `spec`, seed 2 | 40,833 | 1/1/1 | 2/1 | 2/1 | 1,651 |
| `corner`, seed 3 | 101,737 | 5/3/2 | 5/5 | 3/7 | 2,236 |
| CMO-only, seed 9 | 272,223 | 41/57/57 | 71/84 | 81/74 | 2,478 |
| Fixed CLEAN/clean/overlap, seed 14 | 453,186 | 155/0/0 | 155/0 | 0/155 | 2,455 |

An additional frozen-runtime coverage seed 15 completed at cycle 33,205 with
CMO counts `4/1/3`, line states `4/4`, and overlaps `5/3`. The regression
controller recorded the runtime manifest and all controller-file hashes; the
independent verifier accepted artifact SHA-256
`e3eb1fe2c489a71f649d58e4eb709abfa3502d6e2a250e660a0b070d061c5881`.
The frozen executable SHA-256 was
`cc5fcc37649d1792c96b6c34f58015f17a4cfbbec5c8f2547f6a3f08f2088be8`.

The CMO-only run issued 155 CMO requests and 155 derived Probes in its
constrained tail; 75 overlapping younger loads were canceled and LQ/SQ
accounting ended at `269+75/344` and `296+0/296`. Repeated CMO initially exposed
a UT driver defect: `commit_store()` skipped the backend `scommit` pulse when
the externally monitored SQ dequeue had already occurred, leaving the shared
CMO/MMIO StoreQueue state machine unable to accept the next request. The helper
now always supplies that architectural commit for CMO. This was a verification
environment correction, not a CPU RTL bug, and no standalone bug report was
created. No new CPU RTL defect was observed in these runs.

## DCache Per-Beat Error And MSHR Isolation Closure

On 2026-09-07 the DCache manager stopped treating independent `corrupt` as a
response-wide property. It can now select only the first or last GrantData
beat, while denied remains fixed over the response and forces corrupt on every
data beat. The agent tracks A-source lifetime through final D completion,
rejects premature source reuse, records maximum outstanding depth, and exposes
a true response-queue drain check distinct from the E-channel GrantAck queue.

`dcache-errors` added six directed cases crossing both `isKeyword` values with
denied, first-beat corrupt, and last-beat corrupt. Every case completes the
primary critical-beat access while the second beat is delayed 256 cycles, then
issues an unrelated cold load before that beat and requires outstanding A
depth two. An opposite-half load merges into the original MSHR. Exact gates
require two AcquireBlocks, four GrantData beats, two GrantAcks, one targeted
error response, correct per-beat denied/corrupt counts, one clean resident hit,
one poisoned resident hit, five terminal writebacks, and exact LQ conservation.
Last-beat-only corrupt returns the primary word normally because that word's
critical beat is clean; the merged opposite half and later resident access
report HardwareError. The unrelated line remains byte-exact.

The run also characterized the existing early-wakeup boundary. A cause visible
from the current D beat is known in stage 2 and cancels the early wakeup. The
two accesses that learn an installed line error through LoadPipe's delayed
stage-3 metadata retain an uncanceled wakeup, but their terminal exception
writebacks have `rfWen=0` and are followed by redirect. This is not classified
as a CPU functional bug under the current `EnableAccurateLoadError=false`
contract.

The rebuilt and frozen runtime both produced:

```text
MEMBLOCK_DCACHE_ERRORS_PASS cycle=290 denied=1 corrupt=1 denied_wakeups=2 denied_cancels=2 corrupt_wakeups=2 corrupt_cancels=2 multibeat_cases=6 multibeat_denied=2 multibeat_corrupt_first=2 multibeat_corrupt_last=2 multibeat_keyword=3 multibeat_nonkeyword=3 multibeat_concurrent_mshr=6 multibeat_poisoned_hits=6 multibeat_healthy_hits=6 multibeat_cycles=3300 tag_ecc=2 data_ecc=17 concurrent_ecc=1 ecc_error_reports=19 ecc_wakeups=22 ecc_cancels=2 ecc_lq_allocated=61 ecc_lq_canceled=2 ecc_sq_allocated=56 ecc_cycles=6494 rtl_sha256=97b1339a74d458a48a1c58fad766a22cc9dac000cb297e303501311bf47d3b39
```

`make unit` passed all 184 tests; `make check-ports check-rtl`, `single-load`,
`dcache-coherence`, and all 44 `atomic-dchannel-errors` cases also passed. The
reset-recovery, dirty-release, and smoke controls passed as well. Random-load
seed 37 completed 1,024 operations in 24,369 cycles with 230 refills, 794
resident hits, and exact completion of every load. The
frozen executable SHA-256 is
`302e7c119a39c656d310f13ab4b8306d5b7b2160f1ca312e36db18a61c24fdde`;
the runtime manifest SHA-256 is
`e6b8ffe8aa8b835c043438ce63ae7e73fdc121cd059aa027719c41c49dcb4deb`.
No CPU RTL defect was observed.

## Schema 12 Random Svnapot Closure

On 2026-09-07 the common `random-mixed` generator added independent host
stage-1, nested VS-stage, and nested G-stage Svnapot constraints. Four distinct
64-KiB-aligned work regions keep ordinary, VS-only, G-only, and simultaneous
VS/G NAPOT leaves externally distinguishable. The independent software walker
checks every prepared region under both Sv39/Sv48 stage-1 modes and all four
VS/G mode pairs before the first randomized request.

Coverage seed 1 completed 256 actions in 19,179 cycles against complete RTL
SHA-256
`97b1339a74d458a48a1c58fad766a22cc9dac000cb297e303501311bf47d3b39`.
It issued 145 PTW requests and 180 DCache refills. Observed host stage-1 leaf
counts were `33,15` for ordinary/NAPOT. Nested leaf-topology counts were
`20,11,20,10` for ordinary, G-only NAPOT, VS-only NAPOT, and simultaneous
VS/G NAPOT. The regression controller preserved the run in
`build/memblock/schema12-regression.json`; the independent verifier accepted
the artifact with SHA-256
`90c0218779891b2881c26dd45747f0d7f1168eae1bceb70f00972b9f06892726`.
All schema-12 per-seed coverage gates passed. No CPU RTL defect was observed;
this result closes a verification-environment gap.

SPEC-profile seed 2 also completed 256 actions, with the 1-per-mille targets
returning to ordinary traffic after mandatory closure: host leaf counts were
`41,7`, nested topology counts were `95,1,1,1`, PTW requests were 85, and the
long-tail DCache latency reached 384 cycles. A fixed-NAPOT seed 3 disabled Bare
translation and set all three NAPOT controls to 1000; it completed 256 actions
with host leaf counts `0,66` and nested topology counts `0,0,0,54`. This proves
that fixed constraints suppress disabled leaf classes rather than merely adding
NAPOT traffic to the pre-existing 4-KiB generator.

## Nested Implicit G-Stage Permission Closure

On 2026-09-07 `translation-permissions` increased from 106 to 194 independent
fresh-environment cases. The new 42-configuration matrix crosses Sv39/Sv48 VS
stage, Sv39x4/Sv48x4 G stage, every VS PTE level, and G-stage X-only+MXR, U=0,
and A=0 mappings. Each configuration runs both an original load and store, so
84 transactions require the access-specific guest-page fault, exact original
VA, exact faulting PTE GPA, asserted implicit-access marker, no protected-PTE
read, no DCache/Uncache request, and exact queue conservation. Four positive
stores map every VS page-table page R-only with W=0/D=0; all four commit and
return exact data, proving the implicit read does not inherit the original
store's W/D requirement. Runtime cardinality gates require exactly 194 total
cases, 42 configurations, 84 faults split 42/42 by load/store, and four
positive stores before PASS can be printed.

The frozen run produced:

```text
MEMBLOCK_TRANSLATION_PERMISSIONS_PASS cases=194 stage1_load_cases=16 stage1_store_cases=17 two_stage_load_cases=75 two_stage_store_cases=86 implicit_g_permission_faults=84 implicit_g_permission_configs=42 implicit_g_load_permission_faults=42 implicit_g_store_permission_faults=42 implicit_g_readonly_store_cases=4 rtl_sha256=97b1339a74d458a48a1c58fad766a22cc9dac000cb297e303501311bf47d3b39
```

`make unit` passed all 183 tests, and `make check-ports check-rtl` passed. The
frozen executable SHA-256 was
`ad3cc4a5deb4a62582b94869933d966cc660219ce4e24b1638959ef96971eefe`;
the runtime manifest SHA-256 was
`82d6e9063fd0d7d6590504efa063544f9f682a1e11805b330dcb3399323496f8`.
No CPU RTL defect was observed.

## Schema 15 Random CMO Error Closure

On 2026-09-07 the common `random-mixed` interface added independent
`cmo-error` and `cmo-error-denied` per-mille constraints. Success and error
actions use the same CMO scheduler, Bare/stage-1/nested mappings, clean/dirty
line preparation, randomized CBOAck delay, and optional younger delayed miss.
Every enabled seed records error presence, corrupt/denied kind, and all six
CLEAN/FLUSH/INVAL x error-kind crosses. Error CMO completion requires the exact
HardwareError or StoreAccessFault, `flushPipe=1`, no manager Probe, unchanged
bus memory, self redirect cleanup, and cancellation of an overlapping younger
load without writeback. Probe conservation subtracts exactly the failed CMO
count.

The presets use CMO error rates `100/0/500` for
`coverage/spec/corner`; the denied share is 500 in all three. Thus the
SPEC-like direction keeps rare manager errors disabled, while balanced and
corner runs close the same common-interface crosses at different pressure.

The following 256-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | CMO operations | Clean/dirty | No-error/error | Corrupt/denied |
| --- | ---: | ---: | --- | --- | --- | --- |
| `coverage` | 15041 | 47,759 | 5/4/4 | 9/4 | 6/7 | 4/3 |
| `spec`, error rate 500 | 15042 | 32,928 | 3/2/2 | 6/1 | 1/6 | 3/3 |
| `corner` | 15043 | 55,436 | 4/3/5 | 6/6 | 4/8 | 3/5 |
| all denied | 15044 | 32,075 | 3/1/2 | 4/2 | 0/6 | 0/6 |
| `spec`, default zero error | 15045 | 26,781 | 1/1/1 | 2/1 | 3/0 | 0/0 |
| `coverage`, error disabled | 15046 | 27,922 | 2/2/1 | 2/3 | 5/0 | 0/0 |
| all corrupt | 15047 | 33,827 | 1/3/4 | 3/5 | 0/8 | 8/0 |
| CMO disabled | 15048 | 17,805 | 0/0/0 | 0/0 | 0/0 | 0/0 |

Two CMO-only dirty-line runs, seeds 15031 and 15032, each completed 155
independent corrupt CBOAck actions with and without manager backpressure. The
focused `cmo-contracts` scenario also passed all three operations and both
error kinds.

Two verification-environment assumptions were corrected while minimizing the
new crosses. Address-only error injection could be consumed by the same-line
permission/refill response used to make a CMO target dirty; the dedicated
injector now also requires A opcode 12, 13, or 14. Separately, Probe overlap
used a fixed 256-cycle final settle even though corner traffic can retain a
longer unrelated D response; it now polls D-response and GrantAck state to a
bounded timeout. Both were testbench issues, not CPU RTL defects, so no
standalone bug document was created.

Frozen-runtime seed 15051 passed at cycle 33,533 with CMO operation counts
`5/3/2`, clean/dirty `4/6`, no-error/error `4/6`, corrupt/denied `3/3`, and all
six operation/error crosses nonzero. The independent verifier accepted the
controller-hashed frozen artifact:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=15051..15051 results=1 transactions=256 elapsed_seconds=13.259939 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=38c0453f6da813177e893e91e4de84e8c820ba3440c73fcb78bc22407acae2ef
```

The frozen executable and runtime-manifest SHA-256 values are
`c1d6796f1f79734f291ff99c84ab5c81e338e23230b369bdbd0063012837b00d`
and `cd8309dc3d298e972e1394b73007649b102bfeee2ba9426627cad33182b2ec91`.
All 185 Python unit tests passed. No new CPU RTL defect was observed.

## Schema 16 Random Uncache Error Closure

On 2026-09-07 the common `random-mixed` interface added independent
`uncache-error` and `uncache-load-error-denied` per-mille constraints. Each
enabled seed now forces and conserves 12 outcome bins across NC/MMIO,
load/store, and clean/corrupt/denied. Data responses obey the TileLink rule
that denied also asserts corrupt; data-less store `AccessAck` responses permit
denied but not corrupt. Exact agent-side error-response and D-beat counters are
checked in addition to architectural results.

The store oracle distinguishes two RTL contracts. PBMT=IO remains precise: its
initial address writeback may precede the Uncache transaction, but a denied
response must produce the final StoreAccessFault writeback. A PBMT=NC store is
already committed before its request; denied therefore produces one aligned
external `uncacheError`, leaves manager memory unchanged, and dequeues the SQ
entry without an architectural redirect or second writeback. Loads in both
memory classes require exact HardwareError or LoadAccessFault and redirect
cleanup after their exceptional writeback.

The presets use Uncache error rates `100/0/500` for
`coverage/spec/corner`, with a 500-per-mille denied share among error loads.
The SPEC-like preset consequently keeps external errors out of normal traffic,
while coverage and corner directions close every legal class. Extreme fixed
constraints and disabled-class gates also passed:

| Constraint direction | Seed | Cycle | NC load/store | MMIO load/store | Clean/error | Corrupt/denied |
| --- | ---: | ---: | --- | --- | --- | --- |
| `coverage` | 16001 | 44,394 | 4/4 | 6/3 | 11/6 | 2/4 |
| `spec` | 16002 | 28,572 | 2/2 | 2/1 | 7/0 | 0/0 |
| `corner` | 16003 | 52,727 | 6/4 | 9/4 | 11/12 | 3/9 |
| all denied loads | 16004 | 27,896 | 2/0 | 2/0 | 0/4 | 0/4 |
| all denied stores | 16005 | 28,738 | 0/2 | 0/3 | 0/5 | 0/5 |
| all corrupt loads | 16007 | 30,958 | 1/0 | 4/0 | 0/5 | 5/0 |
| error disabled | 16008 | 46,491 | 4/3 | 5/1 | 13/0 | 0/0 |
| MMIO only | 16009 | 32,213 | 0/0 | 8/3 | 8/3 | 1/2 |
| NC only | 16010 | 42,684 | 4/3 | 0/0 | 4/3 | 1/2 |

The four deterministic `spec` latency buckets are now included in the serial
action budget and forced operation selection. Thus 100-percent error
load-only/store-only configurations still produce enough Uncache requests to
cover short, medium, long, and 100-plus-cycle responses.

Two testbench assumptions were corrected during closure. NC store errors were
initially modeled as precise StoreAccessFaults even though the RTL intentionally
reports this already committed path asynchronously. Separately, a random CMO
overlap used a fixed post-redirect delay; a trailing ReleaseAck can carry its
own long-tail delay after the forced younger refill, so the CMO cleanup now
drains D responses and GrantAcks by state. Neither issue was a CPU RTL defect,
and no standalone CPU bug document was created.

The focused `uncache-errors`, `mmio-contracts`, and `cmo-contracts` scenarios,
all 185 Python unit tests, and `check-ports`/`check-rtl` passed. Frozen-runtime
coverage seed 16011 completed 256 actions at cycle 30,715 with NC direction
`4/3`, MMIO direction `7/5`, clean/error `11/8`, corrupt/denied `2/6`, and all
legal outcome bins nonzero. The independent verifier accepted the artifact:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=16011..16011 results=1 transactions=256 elapsed_seconds=12.173522 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=9d9fb517378cba50e7d8744cc0d536515249fce4411a1566c794383e8fed5363
```

The frozen executable and runtime-manifest SHA-256 values are
`770c46eb6281250647f4387933c35844dafa205a748991abe52ea79451f7d1b5`
and `667f4fee56e371bde93bee49e9d95a9cb6fdef9b87b2b543e4fb2eb96b11285c`.
No new CPU RTL defect was observed.

## Schema 17 Random DCache Load Error Closure

On 2026-09-07 the common `random-mixed` interface added
`dcache-load-error` and `dcache-load-error-denied` per-mille constraints for
ordinary scalar loads. Each error action uses a unique cold line mapped in
Bare, Sv39, Sv48, and all four nested VS/G mode pairs. Corrupt responses must
produce HardwareError; denied responses must produce LoadAccessFault. Both
paths suppress the architectural destination, preserve manager memory, perform
precise redirect and LQ cleanup, and drain the full refill plus GrantAck.

The manager agent attributes errored refills and GrantAcks independently from
global traffic. For `N` error refills including `D` denied refills, schema 17
requires exactly `N` error responses, `2D` denied beats, `2N` corrupt beats,
`N` errored GrantAcks, and `N` errored refills. This remains exact when a clean
hardware prefetch request is concurrent. The presets use error rates
`100/0/500` for `coverage/spec/corner`, with a 500-per-mille denied share;
SPEC-like traffic therefore retains no synthetic manager errors.

The following 256-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Clean/corrupt/denied | Error manager tuple |
| --- | ---: | ---: | --- | --- |
| `coverage` | 17003 | 40,061 | 30/2/4 | 6/8/12/6/6 |
| all corrupt, serial scalar | 17004 | 58,037 | 0/16/0 | 16/0/32/16/16 |
| all denied, serial scalar | 17005 | 56,952 | 0/0/17 | 17/34/34/17/17 |
| `spec`, default zero error | 17006 | 26,444 | 67/0/0 | 0/0/0/0/0 |
| frozen `coverage` | 17007 | 32,806 | 29/1/1 | 2/2/4/2/2 |

The focused `dcache-errors` scenario also passed its six-case per-beat refill
matrix, 19 physical tag/data ECC error reports, and concurrent-MSHR checks.
All 185 Python unit tests and `check-ports`/`check-rtl` passed. The independent
verifier accepted frozen-runtime seed 17007:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=17007..17007 results=1 transactions=256 elapsed_seconds=13.272454 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=fb898c49998fd726c5d2252c375e14647e244bd515168d67634fe775bad7ea6e
```

The frozen executable and runtime-manifest SHA-256 values are
`70ca94a91c132895c1b414d5c6c90e64640b33f72ca3b045f47daef463775131`
and `f585ab7277cb6599910e843e7836b15e4454eb795827cb251ba14975b95c1fc4`.
No CPU RTL defect was observed.

## Schema 18 Random Atomic D-Channel Error Closure

On 2026-09-07 the common `random-mixed` interface added `atomic-error` and
`atomic-error-denied`. The generator crosses AMO/LRSC/AMOCAS, W/D width, and
clean/corrupt/denied outcome, using unique cold lines mapped under Bare, Sv39,
Sv48, and every nested VS/G mode pair. Denied LR requires LoadAccessFault;
denied AMO/AMOCAS requires StoreAccessFault; corrupt requires HardwareError.
LRSC error actions issue LR only because a cold SC cannot produce a refill
response in this implementation.

For `N` errors including `D` denied errors, both the online gate and offline
artifact verifier require the exact manager tuple `N/2D/2N/N/N`: errored
responses, denied beats, corrupt beats, errored GrantAcks, and errored refills.
The backing store remains unchanged when the exception returns. The clarified
RTL policy intentionally retains a poisoned line and can apply the AMO/CAS
transform to its private cache data, so the reference model separately predicts
any later dirty ReleaseData. Exceptional data remains outside the ISA oracle.
The initial assumption that errored atomic cache data would never be released
was therefore corrected as a UT model issue; no CPU bug document was created.

The following 256-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Clean/corrupt/denied | Error manager tuple |
| --- | ---: | ---: | --- | --- |
| `spec`, default zero error | 18001 | 27,246 | 6/0/0 | 0/0/0/0/0 |
| `corner` | 18002 | 70,237 | 7/6/7 | 13/14/26/13/13 |
| all corrupt | 18003 | 35,826 | 0/9/0 | 9/0/18/9/9 |
| all denied | 18004 | 42,193 | 0/0/12 | 12/24/24/12/12 |
| frozen `coverage` | 18007 | 39,709 | 8/7/6 | 13/12/26/13/13 |

`atomic-dchannel-errors` also passed all 44 refill-capable W/D opcode/error
cases, 44 persistent poisoned-line load hits, four SC poisoned-hit checks, and
two clean recovery sequences in 7,182 cycles. Invalid `atomic=0` plus enabled
error injection and out-of-range per-mille constraints were rejected before
simulation. All 185 Python unit tests and `check-ports`/`check-rtl` passed. The
independent verifier accepted frozen-runtime seed 18007:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=18007..18007 results=1 transactions=256 elapsed_seconds=15.724905 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=c4c15ef212701ae250271025db3a04fbe3447cda84900946f9895dfc05744c42
```

The frozen executable and runtime-manifest SHA-256 values are
`eb14b54fdac43f0e515f482e1fa200445f786b2035be1d3aa0b41263e4b14bcb`
and `c52b4030cf1229105232103d1b3ac707e4614f047131725b99658e10f50878cf`.
No CPU RTL defect was observed.

## Schema 19 Random PTW Manager Error Closure

On 2026-09-08 the common `random-mixed` interface added address-qualified PTW
manager errors. Five sites cover host stage-1, G-only, implicit G translation
of a VS PTE address, the VS PTE read, and the final nested G walk. The common
weights independently cross load/store, root/intermediate/leaf, Sv39/Sv48,
Sv39x4/Sv48x4, and denied/first-beat-corrupt/last-beat-corrupt. Each action
derives the target PTE address and cutoff from an independent walk, requires
the precise original-access fault and no target DCache/Uncache request, then
fences every affected translation domain and requires a clean same-address
retry that rereads the failed block.

The coverage model has 90 site x direction x level-class x outcome bins, 20
site-specific mode bins, and 20 target-level bins. PTW request coalescing or
replay may legally produce more than one manager request for an action, so the
manager oracle conserves the actual error responses and D beats rather than
assuming one request per instruction. The following runs used the final
schema-19 binary and complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Requested actions | Cycle | PTW error actions | PTW requests | Error manager tuple |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| `spec` | 19007 | 256 | 24,414 | 0 | 91 | 0/0/0 |
| `coverage` frozen artifact | 19006 | 512 | 88,669 | 93 | 1,394 | 134/88/178 |
| `corner` | 19008 | 512 | 149,000 | 93 | 1,458 | 136/90/181 |
| PTW-only coverage | 19005 | 256 | 78,005 | 155 | 1,953 | 155/124/217 |

The `spec` preset deliberately leaves synthetic PTW errors disabled and
observed all-zero bins. The other three runs closed every enabled PTW outcome,
mode, and target-level bin. The PTW-only run kept all ordinary random operation,
translation, fence, Probe, and stride-stream counters at zero, proving that
feature-local constraints do not inherit unrelated coverage gates. Five
invalid site/level/stage-mode configurations were also rejected before cycle 0
with their specific constraint error.

Three harness issues were corrected during closure. Page-table construction
now uses collision-free reference mappings instead of XOR-derived 4-KiB keys.
After a PTW access fault, pending `SFENCE.VMA`, `HFENCE.VVMA`, and
`HFENCE.GVMA` domains are retained as a bitmask and flushed while the old
context is active, before another root is installed. Finally, the fixed vector
shape wave deterministically makes its nonzero-`vstart` and partial-VL classes
reachable rather than relying on a lucky seed. These were UT environment or
coverage defects, not CPU RTL defects, so no `CPU_BUG_*` document was created.

All 186 Python unit tests and `check-rtl` passed. The independent verifier
accepted the frozen-runtime coverage result:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=19006..19006 results=1 transactions=512 elapsed_seconds=34.832519 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=8170e32428ebc8a682b794536ede301ed30a33afab6c3aab81a382ab72435f10
```

The frozen executable and runtime-manifest SHA-256 values are
`f73bea1339de2a5592a275af53900becd16a9ab665bb66ada4c413fb79b365f7`
and `56d61697694583a6b6521875ec1173b984303212c91dc53a2472b97991e894f9`.
No CPU RTL defect was observed.

## Schema 20 Random Same-Line Load-Merge Closure

On 2026-09-08 the common `random-mixed` constraint interface added
`load-merge`, with independent weights for two- or three-load batches and
exact-address, same-beat, or cross-beat placement. Every batch issues its
members in the same cycle against a fresh cold line, crosses both critical
beats and Bare/stage-1/nested translation, and inherits the selected DCache
manager-latency distribution. The 64-MiB nonrepeating line permutation permits
up to 1,048,576 merge actions in one seed without silently reusing a resident
line.

The online and offline oracles require exact value and metadata for every
member, all 12 depth x pattern x critical-beat bins, every enabled translation
regime, and exactly one target-line DCache request per merge action. Global
refill and GrantAck deltas must remain equal and no smaller than the action
count; this admits legal hardware-prefetch traffic while retaining exact sink
conservation. Scalar writebacks must equal the depth-derived load total.

The following 512-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Merge actions | Bare/stage-1/nested | Target requests/refills/GrantAcks/writebacks |
| --- | ---: | ---: | ---: | --- | --- |
| `spec` | 20005 | 40,524 | 15 | 1/1/13 | 15/20/20/36 |
| `coverage` artifact | 20007 | 85,723 | 14 | 1/5/8 | 14/16/16/34 |
| `corner` | 20008 | 170,307 | 16 | 6/4/6 | 16/23/23/41 |
| merge-only `coverage` | 20006 | 25,429 | 411 | 110/158/143 | 411/411/411/1,033 |

The merge-only run spread every enabled shape across 25 to 46 actions and
proved that the feature-local constraint does not inherit unrelated operation,
locality, error, Probe, concurrency, or stride-stream coverage gates. The
universal minimum was raised from 256 to 512 actions so the architectural
prefix, existing 90-bin PTW-error closure, and all new merge bins can coexist
in one coverage or corner seed. The independent verifier accepted the finite
coverage artifact:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=20007..20007 results=1 transactions=512 elapsed_seconds=34.267200 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=01c80a98e17475508649397238b789182aa4110e4bcd6f0986504dc0689edfb0
```

All 186 Python unit tests and `check-rtl` passed. No CPU RTL defect was
observed.

## Schema 21 Random Dirty Set-Pressure Closure

On 2026-09-08 the common `random-mixed` constraint interface added
`set-pressure`. Each compound action selects one of the 128 DCache sets,
initializes nine or ten fresh same-set lines, and commits a byte-, half-,
word-, or doubleword-store to every line. The interface independently weights
depth, width, and four 32-set quarters, then crosses depth and width with
Bare/stage-1/nested translation under the common manager-latency profile. Both
STA-before-SDA and SDA-before-STA issue orders are exercised within each
action.

The online oracle requires one target request, one store writeback, and one SQ
dequeue per store. At least `depth - 8` current target lines must be evicted
from the eight-way set. Their ReleaseData beats are checked against immutable
whole-line snapshots, their stored bytes must reach manager memory, and every
background ReleaseData beat emitted during the action must also pass the
global line-data oracle. A 4-GiB collision-free address schedule permits
1,048,576 actions even when constraints select only one set quarter.

The following runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Actions/stores | Target/global verified ReleaseData | Set quarters | STA/SDA first |
| --- | ---: | ---: | ---: | ---: | --- | --- |
| set-pressure-only `coverage` | 21002 | 118,758 | 411/3,905 | 617/2,905 | 110/92/113/96 | 1,957/1,948 |
| `coverage` frozen artifact | 21003 | 92,846 | 25/238 | 38/57 | 8/3/5/9 | 120/118 |
| `spec` | 21004 | 56,802 | 25/237 | 37/51 | 7/7/7/4 | 120/117 |
| `corner` | 21005 | 163,597 | 30/286 | 46/59 | 6/10/6/8 | 143/143 |
| `coverage` final binary | 21006 | 99,825 | 29/275 | 43/56 | 9/5/8/7 | 138/137 |

All five runs covered every enabled depth x width x translation bin. The
set-pressure-only run generated 3,905 target DCache requests, 3,905 store
writebacks, and 3,905 SQ dequeues exactly; all 2,905 global ReleaseData lines
were byte-verified, including the 617 releases attributed to the current
target sets. The existing directed `dcache-release` scenario also passed with
10 stores, two ReleaseData lines, and two preserved manager-memory updates.

All 186 Python unit tests and `check-rtl` passed. The independent verifier
accepted the finite coverage artifact:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=21003..21003 results=1 transactions=512 elapsed_seconds=37.767780 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=145e8177b24dbf13673a0f6247360ca0b0162e75a82df8b66f7a13cf78ccb369
```

No CPU RTL defect was observed.

## Schema 22 Random Hypervisor SPVP Closure

On 2026-09-08 the common `random-mixed` constraint interface added
`hypervisor-spvp-user`. Hypervisor actions now choose SPVP=S or SPVP=U and
close every enabled HLV/HLVX/HSV x SPVP cross per seed. SPVP=S continues to
use supervisor identity mappings. SPVP=U uses independent U=1 VS regions for
all four 4-KiB/Svnapot VS/G leaf combinations, mapped to separately known
cacheable physical regions. `reference_two_stage_access` independently predicts
each PA, and scalar load/store scoreboards consume the physical
`oracle_address` for exact data and committed-byte checks.

The following 512-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Hypervisor actions | HLV/HLVX/HSV | SPVP S/U | Family x SPVP S/U |
| --- | ---: | ---: | ---: | --- | --- | --- |
| hypervisor-only `coverage` | 22001 | 68,432 | 411 | 131/137/143 | 208/203 | 73/58, 68/69, 67/76 |
| hypervisor-only SPVP=U | 22006 | 65,868 | 411 | 125/138/148 | 0/411 | 0/125, 0/138, 0/148 |
| `coverage` | 22002 | 93,434 | 9 | 4/2/3 | 4/5 | 2/2, 1/1, 1/2 |
| `spec` | 22003 | 58,011 | 6 | 2/2/2 | 3/3 | 1/1, 1/1, 1/1 |
| `corner` | 22004 | 168,868 | 8 | 2/2/4 | 5/3 | 1/1, 1/1, 3/1 |
| frozen `coverage` artifact | 1 | 106,316 | 7 | 3/2/2 | 4/3 | 2/1, 1/1, 1/1 |

The `spec` preset assigns only one per mille to SPVP=U, but the deficit-driven
cross scheduler still guarantees one U-mode action for each enabled family
before returning to the workload-like distribution. The hypervisor-only run
also demonstrates that the class remains independently constrainable without
inheriting unrelated operation coverage gates. The SPVP=U-only endpoint run
recorded 108/102/96/105 actions in the 4-KiB/4-KiB, 4-KiB/G-NAPOT,
VS-NAPOT/4-KiB, and VS-NAPOT/G-NAPOT leaf combinations respectively.

All 186 Python unit tests, `check-rtl`, a clean Picker C++ rebuild, and smoke
passed. The independent verifier accepted the frozen finite artifact with all
controller fragment and runtime hashes checked:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=512 elapsed_seconds=42.346380 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=737816f4f83ea34a702488cdef2eaff69f2df41f71691dbded1ef51608d1e33f
```

No CPU RTL defect was observed.

## Schema 23 Random Hypervisor Alignment Closure

On 2026-09-08 the existing common `misaligned` constraint became an explicit
hypervisor coverage dimension. The scheduler now closes every enabled
HLV/HLVX/HSV x SPVP=S/U x aligned/misaligned bin per seed. Misaligned actions
select only operations wider than one byte, self-check the generated address
class, and retain the independent two-stage PA plus exact load/store oracle.

The following final-binary 512-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Hypervisor actions | HLV/HLVX/HSV | SPVP S/U | Aligned/misaligned |
| --- | ---: | ---: | ---: | --- | --- | --- |
| hypervisor-only `coverage` | 23001 | 68,960 | 411 | 147/142/122 | 205/206 | 198/213 |
| hypervisor-only aligned | 23002 | 71,368 | 411 | 132/140/139 | 219/192 | 411/0 |
| hypervisor-only misaligned | 23003 | 67,164 | 411 | 132/146/133 | 221/190 | 0/411 |
| `spec` | 23004 | 58,179 | 14 | 6/4/4 | 8/6 | 8/6 |
| frozen `coverage` artifact | 1 | 108,068 | 15 | 5/6/4 | 8/7 | 7/8 |

The hypervisor-only mixed run covered all twelve enabled cross bins. The two
endpoint runs proved that disabled alignment classes remain exactly zero. The
`spec` preset assigns five per mille to misalignment, but deficit scheduling
still produced all twelve bins; its flattened family/SPVP/alignment counts were
`3/1,1/1,1/1,1/1,1/1,1/1`.

All 186 Python unit tests, `check-rtl`, a clean Picker C++ rebuild, smoke, and
the independent finite-artifact verifier passed. The verifier checked every
controller fragment and frozen runtime hash:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=512 elapsed_seconds=42.921017 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=3575aa22bff04be944400b4bf49deccd801cd929f3a41330affde7e011fb55cd
```

No CPU RTL defect was observed.

## Schema 24 Random Hypervisor PBMT Closure

On 2026-09-08 the common `random-mixed` hypervisor class added five weighted
VS/G leaf-PBMT pairs: `PMA/PMA`, `PMA/NC`, `PMA/IO`, `NC/IO`, and `IO/NC`.
Together they cover final PMA, NC, and IO selection plus both VS-over-G
priority directions. The scheduler requires every enabled HLV/HLVX/HSV x
SPVP=S/U x PBMT-pair bin per seed. Distinct VA, GPA, and PA aliases cover all
four 4-KiB/Svnapot VS/G leaf topologies; the independent walker supplies the
expected PA and PBMT, and the load/store scoreboards check exact physical bytes.
PMA requires no Hypervisor Uncache request, while NC/IO require exactly one
Uncache request and no DCache request. Non-PMA pairs are deliberately naturally
aligned until their misalignment exception-priority contract is modeled.

The following 512-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Hypervisor actions | HLV/HLVX/HSV | SPVP S/U | PBMT pair counts | Uncache requests |
| --- | ---: | ---: | ---: | --- | --- | --- | ---: |
| hypervisor-focused `coverage` | 24001 | 24,558 | 33 | 11/11/11 | 16/17 | 7/6/7/6/7 | 28 |
| full `coverage` | 24002 | 96,717 | 31 | 10/10/11 | 16/15 | 6/6/6/6/7 | 54 |
| full `spec` | 24003 | 65,594 | 31 | 11/10/10 | 16/15 | 7/6/6/6/6 | 32 |
| non-PMA PBMT pairs only | 24004 | 23,711 | 26 | 8/10/8 | 14/12 | 0/6/7/7/6 | 28 |
| PMA/PMA only | 24005 | 22,016 | 19 | 6/6/7 | 9/10 | 19/0/0/0/0 | 2 |
| full `corner` | 24006 | 173,639 | 36 | 13/13/10 | 20/16 | 6/7/7/7/9 | 62 |
| frozen `coverage` artifact | 1 | 103,245 | 31 | 11/10/10 | 15/16 | 7/6/6/6/6 | 48 |

Every fully enabled run covered all 30 family/SPVP/PBMT bins. The two endpoint
runs proved that disabled PBMT classes remained exactly zero. The PMA-only run
also covered all 12 family/SPVP/aligned-or-misaligned bins, while the non-PMA
endpoint accepted only aligned traffic as specified. Its 26 Hypervisor actions
account for 26 of 28 Uncache requests; the mandatory architectural prefix owns
the other two. Conversely, the PMA-only run's two Uncache requests both came
from that prefix, proving that its 19 Hypervisor actions added none.

All 186 Python unit tests, `check-rtl`, a clean Picker C++ rebuild, smoke, and
the independent finite-artifact verifier passed. The verifier checked all 30
schema-24 PBMT crosses plus frozen runtime, RTL, runner, and controller hashes:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=512 elapsed_seconds=40.613562 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=4485b4b5396aef4bc96eec08d2244ba2cda9ea41c00fb611b81a2afd3c60c001
```

No CPU RTL defect was observed.

## Schema 25 Random Hypervisor Fixed-PMA Closure

On 2026-09-08 the common `random-mixed` hypervisor class added
`hypervisor-pma-device`, a per-mille selector between the existing translated
DDR aliases and an interior address in the SoC's fixed `c=0`, R/W, X=0 PMA
device interval. Distinct VA/GPA/PA aliases retain all four 4-KiB/Svnapot VS/G
leaf topologies and both SPVP values. The scheduler requires every enabled
HLV/HLVX/HSV x SPVP=S/U x DDR/device bin per seed.

The independent two-stage walker supplies the expected PA. Device HLV and HSV
must issue exactly one Uncache request, no DCache request, and return or commit
the exact physical bytes. Device HLVX must report `LoadAccessFault`, issue no
DCache or Uncache request, and cancel every newly observed speculative wakeup.
Device actions are naturally aligned and use PMA/PMA leaves, isolating the
fixed physical PMA classification from PBMT and unresolved non-PMA
misalignment priority.

The following final-binary 512-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Hypervisor actions | HLV/HLVX/HSV | SPVP S/U | DDR/device | Uncache requests |
| --- | ---: | ---: | ---: | --- | --- | --- | ---: |
| hypervisor-focused `coverage` | 25001 | 85,606 | 408 | 120/138/150 | 198/210 | 224/184 | 300 |
| device disabled | 25002 | 79,278 | 407 | 149/136/122 | 217/190 | 407/0 | 318 |
| device only, PMA/PMA only | 25003 | 75,912 | 408 | 132/136/140 | 213/195 | 0/408 | 274 |
| full `spec` | 25004 | 66,900 | 36 | 12/12/12 | 18/18 | 30/6 | 37 |
| full `corner` | 25005 | 176,036 | 41 | 16/13/12 | 20/21 | 33/8 | 61 |
| frozen `coverage` artifact | 1 | 109,959 | 39 | 13/14/12 | 20/19 | 31/8 | 52 |

Every fully enabled run covered all 12 family/SPVP/address-class bins. The two
endpoint runs proved disabled classes remain exactly zero. In the device-only
run, the 408 hypervisor actions account for 272 Uncache requests from HLV/HSV;
the remaining two requests belong to the mandatory architectural prefix. Its
136 HLVX actions contributed no data-manager request and passed the precise
fault plus wakeup/cancel oracle.

All 186 Python unit tests, `check-rtl`, a clean Picker C++ rebuild, smoke, and
the independent finite-artifact verifier passed. The verifier checked all 12
schema-25 address-class crosses plus frozen runtime, RTL, runner, and
controller hashes:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=512 elapsed_seconds=42.859591 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=e486076c6799b637f5a0724ff0840ab852e5011a49112a31bb6636ab9e170d28
```

No CPU RTL defect was observed.

## Schema 26 Random Hypervisor PMP-Edge Closure

On 2026-09-08 the common `random-mixed` hypervisor class added seven weighted
physical-PMP relations: no-PMP control, the first and last naturally aligned
accesses in a 4-KiB NAPOT RWX region, naturally aligned accesses immediately
below and above it, and accesses crossing its lower and upper edges. The allow
entry is first match inside a 16-KiB deny entry; a final allow entry keeps page
table walks and unrelated traffic independent. Distinct VA/GPA/PA aliases
retain all four 4-KiB/Svnapot nested leaf topologies.

Every enabled HLV/HLVX/HSV x SPVP=S/U x relation bin is a per-seed obligation.
The independent two-stage walker supplies the physical address and hand-coded
PMP interval arithmetic supplies allow/fault. Allowed loads check exact data and
every active-PMP HSV is followed by an HLV readback after installing a global
allow entry, proving exact committed bytes on success and unchanged bytes on
denial. Denied actions require the exact load/store access fault, no Uncache
request, no architectural side effect, and checked redirect plus LSQ recovery.
A cross-upper load may
legally hit in cache or issue one exact allowed-prefix cache-line request before
the terminal fault; all other denied relations forbid a data-manager request.

The following final-binary 512-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Hypervisor actions | HLV/HLVX/HSV | SPVP S/U | PMP none/first/last/below/above/cross-lower/cross-upper |
| --- | ---: | ---: | ---: | --- | --- | --- |
| hypervisor-focused `coverage` | 26001 | 94,593 | 411 | 131/140/140 | 198/213 | 56/66/65/54/60/50/60 |
| full `coverage` | 26002 | 119,380 | 71 | 24/24/23 | 35/36 | 35/6/6/6/6/6/6 |
| full `spec` | 26003 | 66,577 | 71 | 23/24/24 | 36/35 | 35/6/6/6/6/6/6 |
| full `corner` | 26004 | 202,439 | 75 | 25/24/26 | 37/38 | 36/6/6/7/7/6/7 |
| no-PMP endpoint | 26005 | 80,086 | 411 | 134/124/153 | 198/213 | 411/0/0/0/0/0/0 |
| frozen `coverage` artifact | 1 | 115,382 | 73 | 24/24/25 | 37/36 | 36/6/6/7/6/6/6 |

All five fully enabled runs covered every one of the 42 family/SPVP/relation
bins and conserved each flattened cross against both its relation and
family/SPVP marginals. The no-PMP endpoint kept all 36 disabled edge bins at
zero while retaining the PBMT/PMA/translation paths. The SPEC preset assigns
weights `999994/1/1/1/1/1/1`; deficit scheduling still executed every rare
edge class without changing its workload-like steady-state distribution.

Focused split-fault testing exposed a UT oracle error rather than an RTL bug.
One replayed load can legally emit several lane-local cancel pulses, so the old
global requirement `wakeups > cancels` was implementation-dependent. The gate
now requires both event types on every lane without comparing aggregate counts,
while the action-local fault oracle still requires every newly observed wakeup
to have a later same-lane cancel. No standalone CPU bug report was created.

All 186 Python unit tests, `check-rtl`, smoke, the three complete constraint
presets, and the independent finite-artifact verifier passed. The verifier
checked schema-26 target/actual fields, all 42 crosses, disabled-bin and
conservation rules, plus frozen runtime, RTL, runner, and controller hashes:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=512 elapsed_seconds=45.210955 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=81d90c2030edb3d2d6c4dc6bca389d648af963529c36cad197c3b5d5abd7cc62
```

No CPU RTL defect was observed. Other NAPOT sizes, TOR boundary composition,
and the full lock/permission/overlap-by-edge matrix remain explicit gaps.

## Schema 27 Random Clean/Dirty Set-Replacement Closure

On 2026-09-08 `random-mixed` extended the schema-21 dirty `set-pressure`
action with a `set-pressure-dirty` line-state selector. Clean actions load nine
or ten fresh tags in one eight-way set and revisit them in reverse. The oracle
requires exact initial and revisit load results, one initial target request per
line, at least `depth - 8` revisit misses, and exact load-WB/LQ-dequeue
conservation. A compact TileLink C observation window is enabled only for one
clean action; it attributes target Releases by address without retaining
regression-length history, requires at least `depth - 8` target Releases, and
rejects any target ReleaseData. Dirty actions retain immutable line-image,
manager-memory, store-WB/SQ-dequeue, and issue-order checks. Both states cross
9/10 lines, B/H/W/D accesses, four set quarters, and Bare/stage-1/nested
translation, producing 48 required bins.

The following final-binary 512-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Clean/dirty actions | Clean target Release/ReleaseData | Dirty target/global ReleaseData |
| --- | ---: | ---: | ---: | ---: | ---: |
| full `coverage` | 27007 | 137,444 | 25/27 | 53/0 | 40/50 |
| clean-only endpoint | 27008 | 118,719 | 29/0 | 61/0 | 0/17 |
| full `spec` | 27009 | 88,457 | 24/24 | 50/0 | 36/55 |
| full `corner` | 27010 | 229,201 | 26/28 | 55/0 | 42/55 |
| dirty-only endpoint | 27011 | 118,052 | 0/29 | 0/0 | 43/60 |
| frozen `coverage` artifact | 1 | 130,229 | 24/24 | 53/0 | 36/42 |

Every fully enabled run hit all 48 state/depth/width/translation bins and every
set quarter. The endpoint runs kept all disabled-state crosses and accounting
at zero. In the frozen artifact, 228 clean initial loads generated 228 exact
target requests; 228 reverse revisits generated 36 target misses. Those clean
actions produced 53 address-attributed target Releases and zero target
ReleaseData, while all six background ReleaseData transactions were
byte-verified. Its 228 dirty stores generated exactly 228 target requests,
store writebacks, and SQ dequeues, with 36 target ReleaseData lines preserving
their immutable bytes.

The first deterministic `corner` replay of seed 27010 exposed a UT state-model
error, not an RTL defect. The Probe generator had accepted a hypervisor PMP
store as a dirty candidate even though that action performs a physical
readback; legal dirty eviction and clean refill can leave the line in Branch.
It then incorrectly expected TtoB `ProbeAckData` instead of the observed BtoB
`ProbeAck`. Probe candidates are now confined to committed scalar stores, as
the documented constraint promised, and overlap auxiliary lines are selected
from a different DCache set so window preparation cannot replace the primary
line. The same seed then completed all 512 actions and closed both values of
Probe cap, data request, and overlap at outstanding depth two. No standalone
CPU bug report was created.

All 186 Python unit tests, `check-rtl`, clean C++ rebuild, smoke, both endpoint
profiles, all three complete presets, and the independent finite-artifact
verifier passed. The frozen executable SHA-256 is
`32b3b29c89a561d527001e5dc5435b0a96abbdd5cd27e148ab1c8fc4e2a2405e`.
The accepted artifact is `build/memblock/schema27-coverage.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=512 elapsed_seconds=51.337290 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=f9053e14707ec97504ba2a53e2ecf3a1c6743e51c83070ae2012544de7730fca
```

No CPU RTL defect was observed. Wider simultaneous replacement/refill/release
pressure remains the next DCache breadth gap.

## Schema 28 Held-Refill Replacement Overlap Closure

On 2026-09-08 `random-mixed` added the
`set-pressure-refill-overlap` constraint and expanded set replacement to 96
required clean/dirty x no-overlap/held-refill-overlap x 9/10-line x B/H/W/D x
Bare/stage-1/nested bins. Each overlap action issues one cold load to a
reserved line in a different set and holds that address-qualified D response.
The target same-set replacement then has to produce at least `depth - 8`
address-attributed Release or ReleaseData transactions while the held load is
still pending. Only after this observation may the manager release the D
response. The auxiliary request, load writeback, and LQ dequeue are all
conserved exactly. GrantAck expectations are matched by sink identity so legal
later D sources may bypass the held response.

Dirty overlap keeps program order legal: all older stores are issued and
complete first, the younger refill is then made pending, and the stores are
committed in order to cause replacement. Clean overlap places the held load at
the ROB head before issuing the younger reverse revisits. Phase-local C-channel
observation bounds retained history independently of regression length. The
minimum `random-mixed` length is now 576 actions, leaving a 48-action random
tail after all newly mandatory cross bins have been scheduled.

The following final-binary 576-action runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Clean/dirty actions | No-overlap/held actions | Clean/dirty target releases | Releases while held |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| overlap-only `coverage` | 28001 | 139,641 | 26/28 | 0/54 | 56/40 | 79 |
| no-overlap `coverage` | 28002 | 126,998 | 30/26 | 56/0 | 65/39 | 0 |
| full `coverage` | 28003 | 144,855 | 49/53 | 51/51 | 110/79 | 75 |
| full `spec` | 28004 | 140,118 | 50/48 | 50/48 | 120/72 | 72 |
| full `corner` | 28005 | 278,601 | 50/52 | 48/54 | 124/78 | 81 |
| frozen `coverage` artifact | 1 | 151,668 | 53/53 | 53/53 | 119/81 | 80 |

Every fully enabled run hit all 96 cross bins and every set quarter. The two
endpoint runs kept every disabled overlap class exactly zero. In the frozen
artifact, 53 dirty actions generated 505 exact target requests, store
writebacks, and SQ dequeues; 81 target ReleaseData lines were byte-verified.
The 53 clean actions generated 504 exact initial target requests, at least 80
revisit misses, 119 target Releases, zero target ReleaseData, and 1,034 exact
load writebacks/LQ dequeues including the 53 auxiliary loads. All 53 held
loads generated one request, one writeback, and one dequeue, while 80 target
releases were observed before their responses were released.

Initial development exposed two UT issues rather than RTL defects. A fixed
six-kilocycle D-response delay expired before some replacements, so it was
replaced by the explicit address-qualified held-response queue. The first dirty
version also advanced the ROB head to the younger held load before committing
older stores, leaving an illegal pending-load sequence; stores are now issued
and completed first, then committed while the younger load remains pending.
Both failing seeds pass after these oracle/stimulus corrections, so no
standalone CPU bug report was created.

All 186 Python unit tests, `check-rtl`, smoke, `dcache-errors`,
`atomic-dchannel-errors`, `dcache-coherence`, both endpoints, all three complete
constraint presets, and the independent finite-artifact verifier passed. The
frozen executable SHA-256 is
`5a4d01d2826ee6a4db83a6c500e6dfa268ac71f76c8f46e3b3982e09f8371845`.
The accepted artifact is `build/memblock/schema28-coverage-1x576.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=576 elapsed_seconds=59.377165 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=fc04469c660d1027311b5a037c0b902d34f163ae722b2a2d7a50e4ba1f1d535d
```

No CPU RTL defect was observed. Release-channel backpressure and multiple
simultaneous replacement windows remain explicit DCache breadth gaps.

## Schema 29 Address-Qualified Release Backpressure Closure

On 2026-09-08 `random-mixed` added
`set-pressure-release-backpressure` and expanded set pressure to 192 required
clean/dirty x no-overlap/held-refill-overlap x no-C-stall/C-stall x 9/10-line
x B/H/W/D x Bare/stage-1/nested bins. The DCache agent now samples the complete
C payload and selects the forced-ready window only when Release/ReleaseData
belongs to the action's target-line set. ProbeAck and background Release traffic
are allowed to complete but cannot consume the target stall budget. A selected
action requires exactly one target release held for 16 valid cycles and 16
matching payload-stability comparisons; an unselected action requires zero
target stalls. Both classes require one completed window.

Clean held-refill actions were reordered so the address-qualified refill is
pending before the target set is filled. Dirty actions continue to issue and
complete all older stores before holding the younger refill and committing the
stores. This guarantees that the attributed replacement and the refill really
overlap without depending on residual C-channel timing. The minimum mixed
length is now 672 actions, leaving room after scheduling every new cross bin.

The following final-binary endpoint runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Clean/dirty actions | No-overlap/held actions | No-stall/stall actions | Stalled releases/cycles/checks |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| C-stall-only `coverage` | 29001 | 149,920 | 48/50 | 50/48 | 0/98 | 98/1,568/1,568 |
| no-C-stall `coverage` | 29002 | 154,932 | 49/48 | 48/49 | 97/0 | 0/0/0 |
| full `coverage` | 29003 | 196,128 | 96/101 | 98/99 | 100/97 | 97/1,552/1,552 |
| frozen `coverage` artifact | 1 | 183,161 | 99/100 | 99/100 | 99/100 | 100/1,600/1,600 |

Every fully enabled run hit all 192 cross bins and every set quarter. The two
endpoint runs kept the disabled backpressure class exactly zero. In the frozen
artifact, 199 set-pressure actions closed 199 target windows; the 100 selected
actions produced exactly 100 stalled target releases, 1,600 target stall
cycles, and 1,600 independent stability checks. The same artifact retained
schema-28 overlap conservation for 100 held refills and 149 target releases
while those refills were pending.

The first implementation exposed a UT timing assumption rather than an RTL
defect: opening the clean C-ready window after initial fill could miss a target
Release already emitted while the held refill was being established. Target
line qualification fixed background attribution, and placing the held refill
before clean pressure fixed the causal overlap. Seeds 29001..29003 all pass
after the correction, so no CPU bug report was created.

All 186 Python unit tests, `check-rtl`, rebuilt smoke, `dcache-errors`,
`dcache-coherence`, `atomic-dchannel-errors`, both C-backpressure endpoints,
the fully enabled coverage run, and the independent frozen-artifact verifier
passed. The frozen executable SHA-256 is
`0df01ca8e5809a73aec7e344726318d1b9282c78481d055a3a4ec4b374aea57a`.
The accepted artifact is `build/memblock/schema29-coverage-1x672.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=672 elapsed_seconds=70.968521 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=9a5ba6b129961992747b44323e567f5442baab6c74db8381fbbb344a2d7c0906
```

No CPU RTL defect was observed. Multiple simultaneous replacement windows are
the next explicit DCache breadth gap.

## Schema 30 Dual Replacement-Window Closure

On 2026-09-09 `random-mixed` added `set-pressure-dual-window` and expanded
set-pressure coverage to 384 required clean/dirty x no-overlap/held-refill x
no-C-stall/C-stall x single/dual-window x 9/10-line x B/H/W/D x
Bare/stage-1/nested bins. A dual action allocates two distinct physical sets in
opposite index quarters. When refill overlap is selected, the DCache agent holds
two complete D transactions by cache-line address. Both load identities must
remain pending until each target set independently emits at least `depth - 8`
attributed Release or ReleaseData transactions; only then are the two responses
released by address. Requests, writebacks, and queue dequeues are conserved by
window count. The target C-ready window remains action scoped and stalls exactly
one attributed transaction when selected.

Dirty actions now commit and drain the first eight lines in both sets before
issuing the ninth/tenth overflow lines. This establishes two dirty resident
baselines before replacement and prevents an earlier clean eviction from
satisfying the ReleaseData minimum. Clean actions retain exact forward-fill and
reverse-revisit data checks. The minimum mixed length is now 864 actions,
leaving room after every enabled cross bin has been scheduled.

The following final-binary runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Clean/dirty | No-overlap/held | No-stall/stall | Single/dual | Held windows/releases while held |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| dual-only `coverage` | 30001 | 278,456 | 97/99 | 98/98 | 98/98 | 0/196 | 196/295 |
| single-only `coverage` | 30002 | 203,796 | 96/96 | 96/96 | 96/96 | 192/0 | 96/144 |
| full `coverage` | 30003 | 354,897 | 194/193 | 193/194 | 194/193 | 193/194 | 292/443 |
| frozen `coverage` artifact | 1 | 345,125 | 193/195 | 193/195 | 194/194 | 192/196 | 294/448 |

Both endpoints hit all 192 reachable bins and kept the disabled window class
at zero. Both fully enabled runs hit all 384 bins and every set quarter. In the
frozen artifact, 388 set-pressure actions generated 2,796 exact dirty target
requests/store writebacks/SQ dequeues and 444 byte-verified target
ReleaseData transactions. Clean pressure generated 2,756 initial target
requests and exact revisits, at least 436 revisit misses, 741 attributed target
Releases, zero target ReleaseData, and 5,658 load writebacks/LQ dequeues. Its
195 overlap actions represented 294 held windows, each with one request,
writeback, and dequeue; 448 target releases completed while all corresponding
loads were still pending. The 194 selected C-backpressure actions produced
exactly 194 stalled target transactions, 3,104 stall cycles, and 3,104 payload
stability checks.

Development exposed three UT defects rather than RTL defects. First, issuing
all dirty lines before the initial commit allowed clean target evictions, so the
dirty baseline was split from overflow issue. Second, a fixed auxiliary overlap
tag could already be resident after a long mixed prefix, so overlap addresses
are now chosen from target-group lines with no prior observed DCache request.
Third, the DCache idle predicate omitted an in-progress multibeat
`ReleaseData`; this could close a target ready window while an unrelated final
C beat was pending. The predicate now includes that transaction state. The
original constrained failing seeds pass after these harness corrections, so no
CPU bug report was created.

All 186 Python unit tests, `check-rtl`, rebuilt smoke, `dcache-errors`,
`dcache-coherence`, `atomic-dchannel-errors`, the dirty dual-overlap reproducer,
both single/dual endpoints, the fully enabled coverage run, and the independent
frozen-artifact verifier passed. The frozen executable SHA-256 is
`66d1d463930de5c9ebb20d185a9d157073e80572f9b1e5c401e66d8d3d8bb42b`.
The accepted artifact is `build/memblock/schema30-coverage-1x864.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=864 elapsed_seconds=135.656291 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=b5b1e01384f907718c949e908b6af0890bb2dc72550741c45eefe41c21c03f90
```

No CPU RTL defect was observed. Three-or-more simultaneous replacement windows
and composition with Probe/CMO traffic remain explicit DCache breadth gaps.

## Schema 31 Triple Replacement-Window Closure

On 2026-09-09 `random-mixed` added `set-pressure-triple-window` and expanded
set-pressure coverage to 576 required clean/dirty x no-overlap/held-refill x
no-C-stall/C-stall x single/dual/triple-window x 9/10-line x B/H/W/D x
Bare/stage-1/nested bins. Selection is hierarchical: triple is chosen first;
otherwise the existing dual probability selects single or dual. A triple action
allocates a third distinct set in the quarter adjacent to the first. The
address-qualified D-response queue holds one complete refill per selected set,
and every set must independently reach `depth - 8` attributed target releases
while all corresponding loads remain pending before any response is released.
All target-request, writeback, dequeue, and overlap accounting scales with the
one/two/three-window class. The action-scoped C stall remains exactly one target
transaction when enabled.

Coverage uses a 333-per-mille triple selection followed by a 500-per-mille dual
selection, approximately balancing all three classes. SPEC uses 1/10 and corner
uses 500/750 respectively. The minimum mixed length is now 1056 actions,
leaving a random tail after all 576 enabled cross bins have been scheduled.

The following final-binary runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Clean/dirty | No-overlap/held | No-stall/stall | Single/dual/triple | Held windows/releases while held |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| single-only `coverage` | 31001 | 216,176 | 97/98 | 97/98 | 97/98 | 195/0/0 | 98/147 |
| dual-only `coverage` | 31002 | 278,900 | 97/101 | 100/98 | 99/99 | 0/198/0 | 196/296 |
| triple-only `coverage` | 31003 | 365,157 | 100/97 | 97/100 | 97/100 | 0/0/197 | 300/453 |
| full `coverage` | 31004 | 571,110 | 291/293 | 292/292 | 294/290 | 195/196/193 | 583/876 |
| full `spec` | 31005 | 1,183,616 | 289/288 | 289/288 | 289/288 | 193/192/192 | 576/864 |
| full `corner` | 31006 | 1,315,066 | 288/293 | 288/293 | 288/293 | 193/193/195 | 588/886 |
| frozen `coverage` artifact | 1 | 571,546 | 290/290 | 288/292 | 291/289 | 192/196/192 | 584/874 |

Each endpoint hit all 192 reachable bins and kept both disabled window classes
at zero. All four fully enabled runs hit all 576 bins and every set quarter. In the
frozen artifact, 290 dirty actions generated 5,508 exact target requests, store
writebacks, and SQ dequeues; 868 target ReleaseData lines were byte-verified.
The 290 clean actions generated 5,510 exact initial target requests and
revisits, at least 870 revisit misses, 1,526 attributed target Releases, zero
target ReleaseData, and 11,312 load writebacks/LQ dequeues including auxiliary
overlap loads. Its 292 overlap actions represented 584 held windows, each with
one request, writeback, and dequeue; 874 target releases completed while all
corresponding loads were pending. The 289 selected C-backpressure actions
produced exactly 289 stalled target transactions, 4,624 stall cycles, and 4,624
payload-stability checks.

The first `spec` and `corner` runs exposed a deterministic UT address-allocation
bug rather than an RTL defect. An earlier held-refill line could occupy the
same group/set/tag later chosen as a pressure target. The harness then rewrote
the backing memory directly while the old clean line could legally remain in
DCache; the returned mismatch exactly matched the earlier overlap fill byte.
The pressure allocator now skips any candidate window containing a line covered
by a prior DCache request. Seeds 31005 and 31006 pass their complete 1,056-action
runs with the corrected allocator, so no standalone CPU bug report was created.

All 186 Python unit tests, `check-rtl`, rebuilt smoke, `dcache-errors`,
`dcache-coherence`, `atomic-dchannel-errors`, the dirty triple-overlap
reproducer, all three window-count endpoints, all three constraint profiles,
and the independent frozen-artifact verifier passed. The frozen executable
SHA-256 is
`025c266cbab23194da3eb25a120d0644bcd37a649b8bae910c6b6ce9f011ee7f`.
The accepted artifact is `build/memblock/schema31-coverage-1x1056.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=1056 elapsed_seconds=222.686042 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=fb6dd5a96af8ac872a4477b46eeca3e18496bca79dd659e67e7516ac8974817d
```

No CPU RTL defect was observed. Four-or-more simultaneous replacement windows
and composition with Probe/CMO traffic remain explicit DCache breadth gaps.

## Schema 32 Triple Probe-Overlap Closure

On 2026-09-09 the common `random-mixed` generator extended Probe/refill
overlap from a binary class to hierarchical one/two/three-request depth.
`probe-overlap` first selects a standalone primary Probe or an overlapping
burst; within the burst, `probe-triple-overlap` selects one or two clean
auxiliary lines before the dirty primary line. All requests are queued without
an intervening simulation cycle, receive distinct active B-source IDs, and are
matched to ProbeAck(Data) responses by source and line address. The unrelated
held refill cannot write back until every Probe finishes, and the measured
accepted-but-unanswered depth must reach the selected burst size.

The schema retains `actual_probe_overlap` as the binary compatibility
projection and adds authoritative `actual_probe_depth=depth1,depth2,depth3`.
The simulator and offline verifier require every enabled depth, conserve depth
counts to generated sequences, project them exactly onto overlap counts, and
account for `depth - 1` auxiliary manager Probes per sequence. Coverage uses a
500-per-mille conditional triple share, SPEC uses 10, and corner uses 750.

The following final-binary runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Depth 1/2/3 | Maximum accepted outstanding |
| --- | ---: | ---: | ---: | ---: |
| depth-1-only `coverage` | 32002 | 122,929 | 2/0/0 | 1 |
| depth-2-only `coverage` | 32003 | 134,002 | 0/2/0 | 2 |
| depth-3-only `coverage` | 32001 | 128,899 | 0/0/2 | 3 |
| full `coverage` | 32004 | 591,929 | 1/2/1 | 3 |
| full `spec` | 32005 | 1,165,928 | 1/1/1 | 3 |
| full `corner` | 32006 | 1,322,762 | 1/1/1 | 3 |
| frozen `coverage` artifact | 1 | 575,357 | 1/1/1 | 3 |

The full coverage, SPEC, and corner seeds retained all 576 schema-31
replacement bins while closing all three Probe depths. The frozen seed ran
three primary sequences with one toN and two toB caps, one requested-data
class, two mandatory-data classes, three auxiliary Probes, two toB cleanup
Probes, and five successful CMO-derived Probes, for exactly 13 manager Probes.
Its set-pressure window counts were 194/194/192; clean/dirty,
no-overlap/held-refill, and no-stall/stall counts were respectively 290/290,
290/290, and 289/291.

All 186 Python unit tests, `check-rtl`, rebuilt smoke, `dcache-coherence`,
`dcache-errors`, `atomic-dchannel-errors`, all three Probe-depth endpoints, all
three complete constraint profiles, and the independent frozen-artifact
verifier passed. The frozen executable and runtime-manifest SHA-256 values are
`e93f003a84736d97b5bebc9cb515991a7167b5ffbb55e074e89e309d369db2ad`
and `d276144cf27531894555ebc8b94731b7553dab121689a6c1ad1517ab5f111a88`.
The accepted artifact is `build/memblock/schema32-coverage-1x1056.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=1056 elapsed_seconds=224.536673 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=bebcc0a7bc90f76c3f2dae21482dd977d98f8048c3a1b9362d545172fb03df51
```

No CPU RTL defect was observed. Four-or-more simultaneous Probe sources,
cross-operation Probe bursts, malformed coherence traffic, four-or-more
replacement windows, and replacement composition with Probe/CMO traffic remain
explicit DCache breadth gaps.

## Schema 33 Full Probe-Queue Depth Closure

On 2026-09-09 the common `random-mixed` generator extended Probe bursts to the
configured eight-entry DCache ProbeQueue capacity. The capacity is now a
checked generated-configuration dimension and a shared model constant rather
than a scenario-local assumption. Selection remains hierarchical:
`probe-overlap` chooses depth one versus a burst, `probe-triple-overlap`
chooses depth two versus the depth-three-through-eight group, and the new
`probe-depth3` through `probe-depth8` weights choose within that group.

Schema 33 replaces the separate depth/cap/data closure checks with 32
authoritative depth x toN/toB x no-data/need-data cross bins while retaining
the marginal counters as exact projections. The constraints own reachability,
the coverage model owns missing-bin selection, sampling, and conservation,
the scenario owns transaction generation, and the offline verifier independently
recomputes the same required projections from the serialized result. Before
each selected burst, the DCache manager holds C ready low until all B requests
have been accepted. The measured accepted-but-unanswered depth must therefore
reach the selected depth without depending on an internal DUT signal.

The following final-behavior runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Probe depth 1/2/3/4/5/6/7/8 | Maximum accepted outstanding |
| --- | ---: | ---: | ---: | ---: |
| depth-1-only `coverage` | 33001 | 125,788 | 5/0/0/0/0/0/0/0 | 1 |
| depth-8-only `coverage` | 33008 | 149,698 | 0/0/0/0/0/0/0/5 | 8 |
| Probe-disabled `coverage` | 33000 | 568,306 | 0/0/0/0/0/0/0/0 | 1 (CMO only) |
| Probe-only closure `coverage` | 33032 | 202,168 | 4/4/4/4/4/4/4/4 | 8 |
| full `coverage` | 33040 | 656,923 | 5/5/4/4/4/4/4/4 | 8 |
| full `spec` | 33041 | 1,277,771 | 4/4/4/4/4/4/4/4 | 8 |
| full `corner` | 33042 | 1,396,187 | 4/4/4/4/4/4/4/4 | 8 |
| frozen `coverage` artifact | 1 | 653,484 | 4/4/4/4/4/4/4/4 | 8 |

Every fully enabled run hit all 32 Probe cross bins. The full coverage, SPEC,
and corner seeds also retained all 576 schema-31 replacement bins. The frozen
seed generated 32 primary Probe sequences, balanced cap and data marginals at
16/16, projected depth one versus burst overlap at 4/28, and hit every cross
exactly once. Its 32 primary, 112 auxiliary, 16 toB cleanup, and one CMO
request account exactly for the reported 161 manager Probes. Its set-pressure
window counts were 192/192/193; clean/dirty,
no-overlap/held-refill, and no-stall/stall counts were respectively 288/289,
289/288, and 289/288.

The Probe-disabled seed closed the remaining common coverage with all Probe
sequence, depth, and cross counters at zero. Its two observed manager Probes
were independently generated successful CMO operations and satisfy the same
manager conservation rule; no disabled constrained Probe leaked into traffic.

The first frozen seed exposed a deterministic UT address-allocation defect,
not an RTL defect. The held-refill address was derived from the first auxiliary
clean line. When allocation skipped a clean line that aliased the primary set,
adjacent Probe sequences could derive the same supposed cold miss address. A
resident line then produced no new DCache request and the harness timed out
waiting for one. The held-refill allocator now derives a unique line from the
sequence block and line index, independently of the filtered auxiliary list.
The original seed passes at cycle 653,484 after this correction, so no
standalone CPU bug report was created.

All 186 Python unit tests, `check-rtl`, rebuilt smoke, `dcache-coherence`,
`dcache-errors`, `atomic-dchannel-errors`, both depth endpoints, the focused
32-bin run, the Probe-disabled profile, all three complete constraint profiles,
the original failing-seed replay, and the independent frozen-artifact verifier
passed. The frozen executable and runtime-manifest SHA-256 values are
`3fd2f6443a155f97914777ec8aae82ce2fdcc162484bdf7e205362f739aae361`
and `c504aaf039d2503206a72b6d7f9a80c7d6d3c152b4a833a9ef5e4534a04ff873`.
The accepted artifact is `build/memblock/schema33-coverage-1x1056.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=1056 elapsed_seconds=262.504200 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=91a09e2103b14e0209893816c50f2b0ef6747275d48b963ee5612c7bf8be0e87
```

No CPU RTL defect was observed. Cross-operation Probe bursts, malformed
coherence traffic, wider B-source wrap/reuse campaigns, four-or-more
replacement windows, and replacement composition with Probe/CMO traffic remain
explicit DCache breadth gaps.

## Schema 34 Full B-Source Lifecycle Closure

On 2026-09-09 the common `random-mixed` environment extended Probe coverage
from simultaneous ProbeQueue depth to the complete accepted B-source
lifecycle. The generated-port contract now records
`tilelink.dcache_probe_source_bits=6`, from which the manager derives a
64-ID source space. The agent samples IDs only on accepted B handshakes,
rejects an ID while an earlier same-source Probe response remains active,
counts its first appearance, permits reuse after completion, and records every
63-to-0 transition.

For `N` accepted manager Probes, the simulator requires
`unique=min(N,64)`, `reuse=N-unique`, and
`wrap=(N == 0 ? 0 : (N - 1) / 64)`. The serialized terminal record
contains the source-space size and all three lifecycle counts; the offline
verifier independently recomputes the tuple from the accepted manager-Probe
count. This keeps the oracle at the top-level B/C protocol boundary rather than
using internal ProbeQueue state.

The following final-behavior runs passed against complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`:

| Constraint direction | Seed | Cycle | Manager Probes | Unique/reuse/wrap | Maximum accepted outstanding |
| --- | ---: | ---: | ---: | ---: | ---: |
| Probe-focused `coverage` | 34001 | 209,300 | 164 | 64/100/2 | 8 |
| full `coverage` | 34002 | 662,335 | 165 | 64/101/2 | 8 |
| full `spec` | 34003 | 1,262,591 | 163 | 64/99/2 | 8 |
| full `corner` | 34004 | 1,412,728 | 164 | 64/100/2 | 8 |
| Probe-disabled `coverage` | 34005 | 574,513 | 4 | 4/0/0 | 1 (CMO only) |
| frozen `coverage` artifact | 1 | 653,484 | 161 | 64/97/2 | 8 |

The focused and all fully enabled runs retained all 32 depth x cap x data
crosses and every one-through-eight depth. Coverage, SPEC, and corner also
retained all 576 schema-31 replacement bins. The frozen seed hit every Probe
cross exactly once, balanced cap and data marginals at 16/16, and accounted
for its 32 primary, 112 auxiliary, 16 toB cleanup, and one successful CMO
Probe. Its replacement window counts were 192/192/193; clean/dirty,
no-overlap/held-refill, and no-stall/stall counts were respectively 288/289,
289/288, and 289/288. The Probe-disabled run proves that lifecycle accounting
also handles manager Probes originating only from successful CMO traffic.

All 187 Python unit tests, `check-rtl`, rebuilt smoke,
`dcache-coherence`, `dcache-errors`,
`atomic-dchannel-errors`, the focused and Probe-disabled runs, all three
complete constraint profiles, and the independent frozen-artifact verifier
passed. The frozen executable and runtime-manifest SHA-256 values are
`8415402d22e58ca608bbb0b98cf670aa89a7cc1edddf5b16bbbfe22b7fa47769`
and `b398534f1ed553677ae8bf7f7d4ba2ab208f01af356025870e1080f3972d2706`.
The accepted artifact is
`build/memblock/schema34-coverage-1x1056.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=1056 elapsed_seconds=253.945383 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=38bebbe18ea484b9e8923a94a203e15f34526a92a6923a0e87901f4ff3133cad
```

No CPU RTL defect was observed. Cross-operation Probe bursts, malformed
coherence traffic, four-or-more replacement windows, and replacement
composition with Probe/CMO traffic remain explicit DCache breadth gaps.

## Schema 35 Quad Replacement-Window Closure

Date: 2026-09-09

Schema 35 extends the set-pressure replacement oracle from one through three
simultaneous eight-way sets to one through four. The new
`set-pressure-quad-window` constraint is resolved hierarchically ahead of the
triple and dual selectors, so each profile has an explicit, reproducible
distribution. A fourth address quartile selects another independent set, and
the online and offline oracles now close all 768 state x overlap x
backpressure x window-count x dirty-depth x width x latency-regime bins.
Compatibility output still projects the old dual counter as
`non-dual,dual`; quad windows are included in `non-dual`.

| Direction | Seed | Final cycle | 1/2/3/4-window actions | Held-refill windows |
| --- | ---: | ---: | ---: | ---: |
| Quad-only coverage endpoint | 35004 | 508,376 | 0/0/0/195 | 384 |
| Coverage profile | 35005 | 957,523 | 192/193/193/193 | 964 |
| Spec profile | 35006 | 2,025,194 | 192/192/192/192 | 960 |
| Corner profile | 35007 | 2,170,876 | 193/192/193/195 | 976 |
| Frozen coverage artifact | 1 | 957,969 | 193/193/194/193 | 961 |

Every run closed every enabled schema-35 cross bin. The frozen seed executed
773 set-pressure actions with balanced valid/invalid state (385/388),
overlap (388/385), and target-C backpressure (386/387). It observed 9,222
dirty data beats and 9,140 clean releases, held 6,192 complete D transactions
across 387 backpressured actions, and retained the schema-34 Probe lifecycle
coverage with 64 unique B sources, 99 completed-source reuses, and two wraps.

The implementation passed all 187 Python unit tests, `check-rtl`, a rebuilt
smoke test, `dcache-coherence`, `dcache-errors`, and
`atomic-dchannel-errors`. Direct coverage/spec/corner endpoints and the
quad-only endpoint passed before the controller produced the frozen artifact.
The frozen runtime executable and manifest SHA-256 values are respectively
`711ae289cb74fef837b8a00dd6a1a29560e1186e3088be4f3790d3b5c8731fc9`
and `d51f87399bd9b57609b55aae990dbeb95815af3ef19c01945ee142b1af02dd37`.
The accepted artifact is
`build/memblock/schema35-coverage-1x1248.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=1248 elapsed_seconds=369.451136 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=340ac6bd878559d537f7a95fe7ac68b2524fa7054436be06599d09ca800be5e1
```

No CPU RTL defect was observed. Five-or-more simultaneous replacement
windows, replacement composition with Probe/CMO traffic, cross-operation
Probe bursts, and malformed coherence traffic remain explicit DCache breadth
gaps.

## Schema 36 CMO Probe-Burst Composition Closure

Date: 2026-09-09

Schema 36 composes successful CLEAN, FLUSH, and INVAL requests with the full
one-through-eight-entry ProbeQueue depth while CBOAck remains pending. Each
action warms zero through seven fresh clean auxiliary lines, holds C unready
until every selected B request is accepted, and then adds the operation-specific
target Probe. The online oracle matches every response by source and address,
checks exact dirty target data, and closes all 48 operation x clean/dirty x
depth bins. Opcode-qualified denied/corrupt CMO errors remain zero-Probe paths.

The independent offline verifier reconstructs each operation marginal from
successful Probe crosses plus its error crosses, reconstructs successful line
state and depth marginals, and computes manager Probe traffic from the
depth-weighted success counts. It also requires the measured outstanding depth
to reach the deepest observed class and retains exact 64-source lifecycle
accounting.

| Direction | Seed | Final cycle | Enabled CMO Probe depths | Maximum accepted outstanding |
| --- | ---: | ---: | --- | ---: |
| Depth-eight-only coverage endpoint | 36008 | 982,929 | 8 | 8 |
| Coverage profile | 36005 | 1,049,921 | 1 through 8 | 8 |
| Spec profile | 36006 | 2,049,385 | 1 through 8 | 8 |
| Corner profile | 36007 | 2,261,106 | 1 through 8 | 8 |
| Frozen coverage artifact | 1 | 1,068,392 | 1 through 8 | 8 |

The depth-eight-only endpoint covered all six operation/state classes at the
selected depth while retaining six CMO errors. The coverage endpoint observed
49 successful and seven error CMOs with depth marginals
`6,6,7,6,6,6,6,6`; the spec endpoint closed each of the 48 success bins
exactly once and observed no CMO errors. The frozen seed observed 52 successful
and six error CMOs, depth marginals `7,6,6,6,7,7,7,6` and
operation marginals `20,19,19`. Its 395 accepted manager Probes produced the
exact source lifecycle `64,331,6`. Existing schema-35 replacement coverage
also remained closed with window counts `195,193,192,193`, 386 held-refill
actions, and 387 target-C-backpressure actions.

All 187 Python unit tests, `check-rtl`, rebuilt smoke, `cmo-contracts`,
`dcache-coherence`, `dcache-errors`, and `atomic-dchannel-errors` passed.
The frozen runtime executable and manifest SHA-256 values are respectively
`a86f165e7af67c36277ead1d5bd36224ad33e24d554dccf96d48c2fd173f33b3`
and `869d2929978d01edbbf5582720587d51ce7d6705b1974ca1611e7c7b00b9057f`.
The accepted artifact is
`build/memblock/schema36-coverage-1x1296.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=1296 elapsed_seconds=413.115138 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=3057e027a6df31828e988be7cd4af9da5b45a4f61d785af001c2d0c9d12c8a45
```

No CPU RTL defect was observed. Multiple simultaneous CMO sources,
replacement/atomic Probe-burst composition, five-or-more simultaneous
replacement windows, and malformed coherence traffic remain explicit DCache
breadth gaps.

## Schema 37 Atomic Probe-Burst Composition Closure

Date: 2026-09-09

Schema 37 composes successful AMO, LR/SC, and AMOCAS operations at W and D
widths with zero through eight auxiliary manager Probes. Depth zero preserves
the ordinary atomic path. For nonzero depth the action selects a fresh cold
target, warms distinct clean auxiliary lines, holds the target D response, and
forbids early atomic writeback. The DCache may legally backpressure B while D
is held. After D is released, C remains unready until the complete selected B
burst is accepted; every no-data ProbeAck is matched by source and address
before exact atomic result and memory checks complete. Atomic error actions
remain separate zero-Probe paths.

The online and independent offline gates close all 54 family x width x depth
bins. They reconstruct each clean atomic outcome from the Probe crosses,
reconstruct every family, width, and depth marginal, add the depth-weighted
atomic bursts to total manager Probe traffic, and retain the exact 64-source
unique/reuse/wrap lifecycle check.

| Direction | Seed | Final cycle | Atomic success/error | Probe depth 0/1/2/3/4/5/6/7/8 | Manager Probes and lifecycle |
| --- | ---: | ---: | ---: | --- | --- |
| Depth-eight-only coverage endpoint | 37008 | 1,052,983 | 6/12 | 0/0/0/0/0/0/0/0/6 | 430; 64/366/6 |
| Coverage profile | 37005 | 1,065,652 | 58/12 | 6/6/6/8/7/7/6/6/6 | 610; 64/546/9 |
| Spec profile | 37006 | 2,082,012 | 54/0 | 6/6/6/6/6/6/6/6/6 | 593; 64/529/9 |
| Corner profile | 37007 | 2,293,934 | 56/18 | 6/6/6/6/6/6/6/8/6 | 622; 64/558/9 |
| Frozen coverage artifact | 1 | 1,058,628 | 55/12 | 6/6/6/6/7/6/6/6/6 | 597; 64/533/9 |

The frozen manager count is independently attributable to 32 ordinary primary
Probe sequences, 16 toB cleanup Probes, 112 ordinary burst auxiliaries, 217
CMO Probes, and 220 atomic auxiliaries. Its 55 successful atomics comprise
family counts `23/22/22`, width counts `34/33`, and all 54 enabled crosses;
the twelve error actions split evenly between corrupt and denied and retain
the exact `12/12/24/12/12` manager-error tuple. Existing schema-36 CMO and
schema-35 replacement coverage also remained closed.

The first depth-eight replay exposed an over-constrained UT timing assumption,
not an RTL defect: the initial oracle expected all B requests to be accepted
while the atomic refill D response was still held. The observed DCache legally
backpressured B until D could progress. The final oracle retains D hold and
early-writeback checks, releases D, and only then requires the exact eight
accepted outstanding Probe sources under C backpressure. The original seed
passes with this external ready/valid contract, so no CPU bug report was
created.

All 187 Python unit tests, `check-rtl`, rebuilt smoke, `dcache-coherence`,
`dcache-errors`, `cmo-contracts`, `atomic-contracts`,
`atomic-dchannel-errors`, the depth-eight endpoint, all three complete
constraint profiles, the all-zero atomic-depth rejection endpoint, and the
independent frozen-artifact verifier passed. The frozen executable and
runtime-manifest SHA-256 values are respectively
`429d8da08f5295931b8c0fe57d5edfdfebfc0b73e17a273112599a9c7eb9414a`
and `1cde510f6a7e85a6b94584d75be917c6e060b75bf003da1190c12de4deb90229`.
The accepted artifact is
`build/memblock/schema37-coverage-1x1344.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=1344 elapsed_seconds=409.249387 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=9b34ba4cd55aad2bcdd6d7707fc0cc507d0d6a904924dd8b2aaf8ad8f0e35501
```

No CPU RTL defect was observed. Multiple simultaneous CMO sources,
five-or-more simultaneous replacement windows, replacement composition with
Probe/CMO/atomic traffic, and malformed coherence traffic remain explicit
DCache breadth gaps.

## Schema 38 Eight-Window Replacement Closure

Date: 2026-09-09

Schema 38 extends the replacement-pressure oracle from one through four
simultaneous eight-way sets to one through eight. The former hierarchical
dual/triple/quad probabilities are replaced by eight direct relative weights,
so each constraint profile describes the complete window-count distribution
without dependent probability arithmetic. The target allocator repeats the
four address quartiles with a distinct set sequence, while a compile-time and
configuration/Scala/C++ contract confirms that eight held refills remain below
the generated DCache's 16 miss entries.

The online and independent offline gates close all 1,536 line-state x refill-
overlap x target-C-backpressure x window-count x dirty-depth x issue-width x
latency-regime bins. Schema-37 compatibility output remains available as an
exact-two-window versus non-two-window projection, but the eight-entry window
vector is now authoritative.

| Direction | Seed | Final cycle | Set-pressure actions | 1/2/3/4/5/6/7/8-window actions |
| --- | ---: | ---: | ---: | --- |
| Eight-window-only endpoint | 38008 | 936,926 | 194 | 0/0/0/0/0/0/0/194 |
| Coverage profile | 38005 | 3,004,385 | 1,539 | 193/192/192/192/194/192/192/192 |
| Spec profile | 38006 | 6,750,548 | 1,536 | 192/192/192/192/192/192/192/192 |
| Corner profile | 38007 | 7,024,803 | 1,538 | 192/192/192/192/192/193/192/193 |
| Frozen coverage artifact | 1 | 2,997,152 | 1,537 | 192/192/192/193/192/192/192/192 |

Every complete profile and the frozen seed hit every schema-38 cross bin. The
frozen seed also balanced valid/invalid state (768/769), held-refill overlap
(768/769), and target-C backpressure (768/769). The eight-window-only endpoint
reached maximum accepted Probe depth eight while isolating the new capacity
boundary. Existing atomic and CMO Probe-burst crosses remained closed in the
complete profiles.

The first complete replay exposed UT scheduling defects rather than RTL
failures. Building every clean target set while an overlap refill was already
held could self-block the 72-entry LQ, overlap sets selected as `target + 1`
could alias another target beyond four windows, and a dirty eight-window
baseline could exceed the 56-entry SQ before commit. The final sequence first
retires clean baselines and then holds only overflow refills, chooses overlap
sets disjoint from all targets, and commits dirty baselines in two four-way
batches. These changes preserve the external causal checks without encoding
an impossible queue schedule.

All 187 Python unit tests, `check-rtl`, rebuilt smoke, `dcache-coherence`,
`dcache-errors`, `cmo-contracts`, `atomic-contracts`,
`atomic-dchannel-errors`, the eight-window endpoint, all three complete
constraint profiles, the all-zero window-weight rejection endpoint, the
2,111-action minimum rejection endpoint, and the independent frozen-artifact
verifier passed. The frozen executable and runtime-manifest SHA-256 values are
respectively
`7f105d5d563e9306f35d37acb45ca85d8a45b558e0213c3854509aaca13d9c85`
and `145066c38a2f307a20fcfc48b81fd81b4cd179f4824456ba6f52365dc45494f1`.
The accepted artifact is
`build/memblock/schema38-coverage-1x2112.json`:

```text
MEMBLOCK_REGRESSION_ARTIFACT_PASS seeds=1..1 results=1 transactions=2112 elapsed_seconds=1256.170048 rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057 artifact_sha256=20fe0c6d42c8b269981cc116a86073eb4c5031df5fc64794cd551fd9f0724d2f
```

No CPU RTL defect was observed. Multiple simultaneous CMO sources,
replacement composition with Probe/CMO/atomic traffic, and malformed
coherence traffic remain explicit DCache breadth gaps.

## Schema 41 Vector Closure Bring-Up

Date: 2026-09-09

The schema-41 harness rebuild passed the 193-test Python unit suite and the
generated C++/RTL harness build. A first 3,072-action `random-mixed` run
(`--test random-mixed --seed 1 --transactions 3072`) reached action 2,907 and
then stopped in `random-miss-burst`. The scalar load writeback at
`0x1c50012e8` returned `0x0`, while the independent sparse-memory oracle
expected `0x908f8e8d8c8b8a89`; the expected bytes are the requested line's
incrementing fill at the selected `+40` byte offset. The failure is therefore
not a vector-cross or bank/replay/prefetch coverage gate.

This was initially retained as an unconfirmed candidate pending a shorter
deterministic miss-burst reproducer and request/response attribution review. A
no-backpressure replay was started to separate response scheduling from data
or address attribution, then stopped before completion to avoid spending a
long regression budget without additional evidence.

A focused 8,192-action replay then enabled only ordinary `miss-burst` traffic
while retaining every depth 2..16, legal issue width 1..3, and Bare/stage-1/
nested translation choice. It passed at cycle 1,402,528 after 8,091
`miss-burst` actions, 73,468 scalar writebacks, and a maximum externally
observed outstanding depth of 16. DCache A requests, complete refills, and
GrantAcks were each 73,492; LQ accounting was 73,528 completed plus one legal
cancellation out of 73,529 allocations. Two shorter reductions also passed:
256 actions with only Bare/depth-2/width-1, and 256 actions with all three
translation choices and depths 2..5.

These passing reductions ruled out an isolated ordinary miss-burst path and
simple repeated LQ/ROB wraparound as sufficient causes, and directed the
remaining investigation toward interactions with other `random-mixed`
operation classes. Correctness remained decided by the external
request/completion identity and independent sparse-memory model; bank
conflict, prefetch, replay, and cache residency were diagnostic observations
only.

The first interaction reduction matrix also passed with the same seed and the
short-mode online gates enabled:

| Focused profile | Actions | Observed focused classes | Final cycle | Result |
| --- | ---: | --- | ---: | --- |
| `set-pressure + miss-burst` (one clean 1-window bin) | 256 | 80 set-pressure, 75 miss-burst, max outstanding 2 | 47,200 | pass |
| scalar load/store + `miss-burst` (depth 2, width 1) | 512 | 135/122 scalar load/store, 154 miss-burst | 20,450 | pass |
| scalar/vector + `miss-burst` (one unit-stride EEW8/SEW8/LMUL1 shape) | 1,024 | 126/145 scalar load/store, 93/82 vector load/store, 477 miss-burst | 40,915 | pass |
| CMO CLEAN + `miss-burst` (clean line, depth 1) | 1,024 | 345 CMO, 418 miss-burst, 4,076 refills and GrantAcks | 297,150 | pass |
| bank-conflict + `miss-burst` | 1,024 | 239 bank waves, 481 miss-burst | 56,894 | pass |
| load-merge + `miss-burst` (depth 2, same address) | 1,024 | 310 merge actions, 503 miss-burst | 39,919 | pass |
| AMO.D + `miss-burst` (zero auxiliary Probe depth) | 1,024 | 436 AMO, 487 miss-burst | 85,062 | pass |
| scalar DCache denied/corrupt loads + `miss-burst` | 1,024 | 157 denied, 171 corrupt, 595 miss-burst; 5,622 refills and GrantAcks | 123,769 | pass |
| scalar + ordinary vector + AMO.D + `miss-burst` (one legal RVV shape) | 1,024 | 101/93 scalar/vector loads, 98 vector stores, 268 AMO, 363 miss-burst | 31,445 | pass |

The profiles intentionally disable unrelated dimensions and pin one legal
value for each retained cross, so they are reduction experiments rather than
replacement regression presets. They cover the SPEC-relevant scalar/vector,
MLP, merge, replacement, atomic, CMO, bank-stress, and error-recovery classes
without turning any implementation detail into a correctness condition. The
first full default seed-1 replay was stopped after 18 minutes without reaching
a terminal summary; it remained a known-reproducer control at that point.

The same schema-41 generator was then run with a vector-only constrained tail
(`seed=7`, 3,072 actions). All enabled ordinary vector dimensions were left
nonzero: both directions, all four unit/strided/indexed addressing modes, all
EEW/SEW values, all legal LMUL/derived-EMUL classes, and all five policy
dimensions. The run passed in 251,390 cycles with 1,790 complete vector
instructions and 5,101 vector uops (983 multi-uop instructions). The terminal
oracle reported 4,022 vector-load and 3,171 vector-store writebacks, exact
queue conservation, and no unaccounted cancellation. This closes the basic
ordinary-vector shape stimulus and per-element data/readback oracle; full
shapes inside every heterogeneous overlap-window slot remain a separate
composition gap.

On 2026-09-10, read-only Picker `mem_direct` debug resolved the seed-1
candidate as a UT false positive. Picker commit `012670c` fixes `mem_direct`
generation against the generated Verilator data types (including the observed
Verilator 5.048 `WData` build failure), and current Picker master `5e9e38d`
also adapts VCD tracing by Verilator version. The debug build used no VPI and
never wrote an internal signal: it sampled only Picker `U()`, `B()`, and
`GetBytes()` accessors. Its 482,626-line offset map SHA-256 was
`94939076a5b09e7438bb74d391fe62746a81ffcda650f933b4b28fcc6286608f`;
the diagnostic executable SHA-256 was
`794d0e9d03936ce2d38a16da9b5a0e6befca084542db3ae8b0ff905a5e8ef540`.

The trace established the complete external cause. A legal earlier request
for line `0x1c50012c0` fired at cycle 3,068,352 and received two all-zero D
beats at cycles 3,068,354 and 3,068,357. The test did not fill that backing
line until the later miss-burst action, after the DUT could already hold the
zero-valued line. The subsequent demand at `0x1c50012e8` therefore returned
the legally cached zero while the reference memory had been changed behind
the DUT to `0x908f8e8d8c8b8a89`. Defining the manager/reference image before
the first DUT clock changed those same D beats to the expected incrementing
bytes and produced an exact `0x908f8e8d8c8b8a89` writeback. This is a
backing-memory lifetime violation in the UT, not evidence of an RTL defect.

The stable fix lazily defines the large merge, miss-burst, and bank-stress
memory pools before traffic, with explicit modeled writes taking precedence.
Merge and miss-burst stimulus skips any line already observed at the external
manager. Their terminal oracle still requires exact per-ROB load data,
exactly one writeback and LQ dequeue per generated operation, and global
refill/GrantAck conservation; it no longer treats an exact A-request count,
arrival timing, hardware-prefetch traffic, or internal MSHR behavior as the
expected architectural answer. A second replay had exposed the old
`random-miss-burst-outstanding-depth` equality gate for a legally prefetched
line, so the external depth check now accepts `>= target` while requiring at
least one target request for every selected fresh line.

The final unchanged seed and command passed all 3,072 actions at cycle
3,119,615. It produced 72,076 scalar writebacks, 1,058 vector-load and 982
vector-store writebacks, 78,382 complete DCache refills, 78,384 GrantAcks, and
1,263 exact miss-burst scalar completions while reaching external outstanding
depth 16. LQ accounting was `76400+29/76429`, SQ accounting was
`37277+0/37277`, and no cancellation was unaccounted. The original line was
recognized from its earlier A history and was not incorrectly reused as a
cold miss. No `CPU_BUG_*` report is warranted.

The final verification pass completed all 195 Python unit tests,
`check-rtl`, and a clean standard Picker harness rebuild plus `smoke` without
the debug macro. The smoke test passed at cycle 38 on complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`.
