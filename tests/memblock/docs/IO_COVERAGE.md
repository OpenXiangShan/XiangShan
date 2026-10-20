# MemBlock Top-Level IO Coverage

## Scope

This document is the authoritative mapping from the generated MemBlock top-level
interface to constrained-random stimulus, monitors, legal-value classes, and
known coverage gaps. `config/expected_ports.json` remains the exact per-pin
inventory. `docs/PORTS.md` renders the generated group and protocol summary.

The current generated configuration has 1,335 flattened ports: 749 inputs and
586 outputs. A pin being driven does not imply that every protocol-valid value
combination has a semantic oracle. The status terms below are therefore used
strictly:

| Status | Meaning |
| --- | --- |
| **Semantic / constrained** | A typed driver generates legal transactions, an independent monitor/reference model checks observable behavior, and at least one focused or constrained-random coverage gate is enforced |
| **Semantic / directed** | A focused scenario drives and checks the interface, but it is not exposed as a general constrained-random dimension |
| **Monitored / partial** | The interface is sampled and several contracts are checked, but field combinations or negative cases remain exposed |
| **Structural only** | `pin-space` and idle policy exercise the pins, without claiming legal protocol semantics |
| **Integration-owned** | Correct meaning depends on logic outside the MemBlock boundary |
| **Planned gap** | No adequate producer, oracle, or acceptance rule exists yet |

All non-clock/non-primary-reset inputs participate in `pin-space`: zero,
all-one, and 254 deterministic mixed patterns are driven and read back, and all
outputs are sampled after each pattern. This proves wrapper connectivity and
broad raw value reachability. It does not close any semantic row below.

## Generated Inventory

The following block is generated from `config/expected_ports.json`; run
`make check-docs` after any RTL interface change.

<!-- BEGIN GENERATED INTERFACE INVENTORY -->
| Interface group | Ports | Inputs | Outputs |
| --- | ---: | ---: | ---: |
| `dcache_tilelink` | 53 | 22 | 31 |
| `frontend_tilelink` | 88 | 41 | 47 |
| `infrastructure` | 2 | 2 | 0 |
| `mem_to_ooo` | 260 | 0 | 260 |
| `misc` | 351 | 141 | 210 |
| `ooo_to_mem` | 532 | 523 | 9 |
| `perf` | 8 | 0 | 8 |
| `ptw_tilelink` | 21 | 10 | 11 |
| `uncache_tilelink` | 20 | 10 | 10 |
| **Total** | **1335** | **749** | **586** |
<!-- END GENERATED INTERFACE INVENTORY -->

## Interface Coverage Matrix

The legal-value column names the dimensions that the harness deliberately
varies or enumerates. Values outside these classes are either constrained as
illegal, checked only by `pin-space`, or listed as gaps.

Rows below are semantic families and can share a physical control pin. The
generated table above is the non-overlapping physical inventory.

| Interface family | Ports | Legal-value dimensions exercised | Driver and oracle | Status |
| --- | ---: | --- | --- | --- |
| Clock and primary reset | 2 | Clock generation, external reset assertion, synchronized release, reset during outstanding manager traffic | Environment reset controller; reset/recovery and quiescence checks | Semantic / directed |
| LSQ dispatch: `enqLsq_req_0..5`, `enqLsq_needAlloc_*`, `lsqio_scommit`, pending pointers, `flushSb` | 241 | Six dispatch lanes, allocation width, 24 exception classes, FU/op type, `flushPipe`, `uopIdx`, `lastUop`, ROB/LQ/SQ identity, `numLsElem`, commit/pending state | Typed LSQ enqueue/commit drivers and passive six-lane monitor; operation, lane, width, identity, and queue-flow conservation | Semantic / constrained |
| Scalar load issue: `issueLda_0..2` | 72 | `lb/lh/lw/ld/lbu/lhu/lwu`, three lanes, aligned/misaligned and cross-page VA, both Sv39/Sv48 canonical boundary directions and 2/4/8-byte crossing widths, Bare/Sv39/Sv48 and nested translation, cache/TLB/error outcomes | `issue_load`, load scoreboard, translation/PMP/PMA oracles; direction/lane/operation coverage | Semantic / constrained |
| Vector memory issue: `issueVldu_0..1` | 75 | EEW/SEW 8/16/32/64, legal LMUL/EMUL, unit/strided/indexed-unordered/indexed-ordered/segment, mask, `vl`, `vstart`, old destination, split/cross-page replay, VFOF and Segment FOF | Vector builders, byte-accurate vector scoreboard and RVV reference; legal-shape and uop-flow coverage | Semantic / constrained |
| Scalar store issue: `issueSta_0..1`, `issueStd_0..1` | 40 | `sb/sh/sw/sd`, both address and data lanes, explicit lane-0 MMIO/CMO/CBO.ZERO/split completions, two issue orders, byte masks, misaligned/replay, Sv39/Sv48 canonical range faults, forwarding, translation and manager errors | Store address/data drivers, store scoreboard, SQ/readback and forwarding oracles | Semantic / constrained |
| Translation controls: `sfence`, `tlbCsr_*` | 41 | Bare/Sv39/Sv48, VS/VU/G modes, all four nested `vsatp` x `hgatp` pairs, ASID/VMID/PPN changes, global/selective SFENCE/HFENCE kinds and scopes | `issue_sfence`, translation-context and fence matrices; stale-response suppression and exact PA checks | Semantic / constrained |
| Redirect and exception controls | 5 | ROB flag/value, redirect level, VLS-exception marker; all load/store/vector/atomic exception causes exercised by focused scenarios | Redirect driver, queue cancellation monitor, exception and writeback oracles | Semantic / constrained |
| CSR, trigger and backend controls | 63 | CSR-distribution controls, debug/memory/frontend triggers, CPU halted/critical-error/MSI ack, PMP CSRs | Distributed-CSR writers, trigger/PMP contracts and top-control bridge checks | Semantic / directed |
| MemBlock-to-OOO outputs | 260 | Writeback payload/identity/data/error, successful vector data `vdIdxInField` across ordinary/indexed/segment/whole-register shapes, wakeup/cancel lanes, IQ feedback, memory violation, top-down and interrupt outputs | Load/store/vector scoreboards, feedback and protocol monitors, top-control and top-down oracles | Monitored / partial |
| DCache A/B/C/D/E TileLink | 53 | A Get/Acquire opcodes and sizes; B Probe opcode/param/source/address/permissions; C Release/ReleaseData and CMO; D beat data/denied/corrupt; E GrantAck; A/C/D/E ready stalls and response latency | Coherent DCache agent, SVA, release-data model, manager accounting and constrained-random coverage schemas. ProbeAck(Data) is matched by address while its client-owned C source is observed; denied and last-beat-corrupt errors are also driven through cached poisoned lines into vector loads | Monitored / partial |
| PTW A/D TileLink | 21 | Legal Get requests, multibeat data, denied/corrupt at first/all/last beats, source/address matching, request/response stalls and long latency | PTW agent and software page-table walker; fault cutoff and retry checks | Monitored / partial |
| Uncache A/D TileLink | 20 | Get/Put, 1/2/4/8-byte accesses, mask/data, AccessAck/Data, denied/corrupt, device side effects, ready stalls and latency | Uncache agent, partial-write log, architectural and bus-memory comparison | Monitored / partial |
| Frontend ICache, ICache-control, instruction-Uncache TileLink | 88 | Get/PutFull/PutPartial, sizes 1/2/4/8 and line requests, source credit, address/mask/data/user fields, two-beat ICache D, corrupt/denied, request/response stalls | `frontend-bridge` independent scoreboards and source-credit checks; all 88 fields explicitly driven or checked | Semantic / directed |
| IFU-to-Mem PTW request/response | 64 | Sv39/Sv48 and all four nested pairs, VS/G leaf and non-leaf inputs, PBMT, faults, duplicate same-VPN requests, context changes, fences, response backpressure | `ifetch-ptw-bridge`; exact response fields, walk sharing, stale-response suppression | Semantic / directed |
| L2 DTLB boundary: `io_l2_tlb_req_*` | 13 | Ordinary/prefetch, kill, `no_translate`, cacheable/NC/IO, hit/miss, stage-1/nested faults, PMP allow/deny, all `sourceId` hint values | `l2-tlb-contracts`; exact registered response fields and PTW request accounting | Semantic / directed |
| L2 hint, flush, top-down, interrupt and WFI controls | 21 | Hint valid/source/keyword, flush completion, L1/L2/L3 miss and pressure, seven external interrupts, WFI request/safe | `l2-tlb-contracts`, `l2-flush-contracts`, `topdown-contracts`, `top-control-contracts`, `wfi-safety` | Semantic / directed |
| Software and hardware prefetch outputs | 11 | `prefetch.i/r/w`, three IFU-prefetch lanes, fixed-PC strides, L2/L3 sender addresses and sources; CSR suppression | `ifetch-prefetch`, `hardware-prefetch`, stride traffic in `random-mixed`; IFU output has an architectural oracle, hardware sender selection is characterization only | Monitored / partial |
| Trace bypass | 45 | Group-valid combinations, instruction types, retire counts, privilege, encoder ready/valid, FTQ offset, trap cause/tval | `trace-bridge-contracts`; exact transport transformation | Semantic / directed |
| DFT bridge | 24 | Seven SRAM broadcast controls and three DFT-reset controls; all 1,024 combinations; functional and scan reset effects | `dft-bridge-contracts`, `reset-tree-contracts`; combinational output and reset routing | Semantic / directed |
| Performance events and counters | 144 | Clock/reset operation and top-control transport for outer/inner hardware-counter lanes; `top-control-contracts` checks 67 shared lanes | Generated lane adapters and transport checks | Integration-owned |
| Remaining direct top-level controls | remaining | Hart ID, reset vector, L2 enable/flush, clock/power, MSI ack, I-cache BEU, DCache/Uncache ECC outputs, MBMC and maintenance controls | `top-control-contracts`, `mbmc-contracts`, ECC scenarios and cycle-level monitors | Monitored / partial |

## Constrained-Random Legal Inputs

The common `random-mixed` generator currently records
`constraint_schema=48`. Its canonical legal-input dimensions are documented in
`CONSTRAINED_RANDOM.md` and include:

- scalar loads/stores, vector loads/stores/segments, software prefetch and
  atomic AMO/LR/SC/AMOCAS operations;
- cacheable, PBMT-NC and PBMT-IO addresses, including MMIO store behavior;
- Bare, host stage-1 and nested VS/G translation, all supported Sv39/Sv48 mode
  pairs, Svnapot leaves, context switches, TLB flushes and translation fences;
- all decoder-legal ordinary vector direction/addressing/EEW/SEW/LMUL/EMUL
  classes, mask/tail agnostic policies, `vl`, `vstart`, alignment and replay;
- dirty/clean Probe traffic, one-through-eight Probe depth, source lifecycle,
  CMO operation/state/depth crosses and atomic Probe composition;
- scalar-load, atomic and PTW denied/corrupt manager errors, response latency,
  request/response stalls and six observed backpressure classes;
- redirects, queue pressure, load merge, set replacement, miss bursts, bank
  conflicts and cold/warm/hot locality;
- scalar accesses crossing either canonical boundary in Sv39/Sv48, every
  enabled load/store x mode x boundary direction x 2/4/8-byte width bin,
  exact fault address, unchanged edge data, and observed queue recovery.

Every enabled constrained class must be observed in each seed and must conserve
its target/actual coverage counters. Architectural data, exceptions, identity,
queue retirement and manager traffic remain independently checked.

## Insufficient Coverage

No current result supports the claim that every legal value combination on all
1,335 ports is covered. The following classes still lack a complete producer,
reference model, or externally observable acceptance contract. They are plan
gaps, not silently accepted behavior.

| Gap | Affected interfaces | Why current coverage is insufficient | Priority |
| --- | --- | --- | --- |
| Malformed, duplicate, unsolicited, early, or late manager responses | DCache, PTW and Uncache A/D TileLink | Legal denied/corrupt cases are covered, but negative protocol timing is not part of normal workload traffic | High |
| Replacement composed with Probe/CMO/atomic traffic; multiple simultaneous CMO sources | DCache TileLink and maintenance controls | Individual mechanisms have depth and error coverage; externally attributable multi-class ordering is incomplete | High |
| Full-core fence/fence.i integration and cross-hart LR/SC reservation interference | SFENCE, atomic issue/retirement, L2/L3 controls | MemBlock receives post-decode controls; cross-hart and whole-core ordering are outside this UT boundary | High |
| Broader hypervisor PMP/PMA/misalignment combinations, instruction PMP and fixed-PMA region edges | HLV/HLVX/HSV issue, PMP/PBMT/TLB controls | The 4-KiB NAPOT relation cross and one fixed device interval are covered; several cross-products remain | Medium |
| Software-visible `satp`/`vsatp`/`hgatp` WARL behavior and hardware A/D updates | TLB CSR/TLB feedback and PTW traffic | MemBlock consumes post-CSR controls; concurrent A/D and invalidation races need full-core or formal context | Medium |
| Broad cross-cause exception priority and ordered indexed side-effecting accesses | LSQ enqueue/issue, redirect and writeback outputs | Selected page-fault/PBMT/PMP overlaps are covered; the complete priority matrix and ordered MMIO/indexed target semantics are not | Medium |
| Reset during arbitrary LSQ producer/replay classes | OOO issue/enqueue, redirect and LSQ queues | Three manager-path reset recoveries are covered; additional producer and replay states remain | Medium |
| Vector canonical-range mixed traffic and stage-2 address limits | Scalar/vector issue and TLB controls | Directed Sv39/Sv48 scalar and strided/indexed vector stores cover canonical crossings; schema 48 composes scalar crossings into mixed traffic. Vector mixed crossings and 41/50-bit G-stage limits remain | Medium |
| Split-load timing near RAR capacity | Scalar/vector load issue and load feedback | One directed three-pending-split case exists; the wider delay/occupancy space is not randomized | Medium |
| Complete wakeup/cancel/memory-violation field attribution | MemBlock-to-OOO feedback outputs | Focused cases cover all exposed violation fields and representative wakeup/cancel classes, but there is no complete count/field relation for every producer, replay and exception cross | Medium |
| Matching-MSHR L2-hint replay and external-L2 retry policy | L2 hint and L2 DTLB request/response | The MemBlock-side transport and idle/no-MSHR behavior are checked; matching-MSHR policy and downstream retry ownership are outside this boundary | Integration-owned |
| Dynamic control reprogramming and remaining CSR/trigger cross-products | CSR, trigger, PMP, PMA, flush and maintenance controls | Default/legal static combinations are checked; arbitrary reprogramming during overlapping LSQ, cache and translation traffic is not exhaustively covered | Medium |
| Broader physical ECC injection and coherence recovery | DCache/Uncache ECC and manager error paths | Current bank/tag/data single- and double-bit classes and same-cycle isolation are covered; broader masks, delayed/persistent events and wider concurrency remain | Medium |
| Four-state/X behavior and uninitialized storage | All storage-bearing interfaces and internal arrays | Requires a four-state simulator or equivalent formal check; two-state Verilator cannot establish it | Medium |
| Performance-counter meaning, top-down attribution, hardware-prefetch selection and training metadata | Performance and L2/L3 prefetch outputs | These have transport/characterization checks but no stable architectural oracle at the MemBlock boundary | Integration-owned |
| MBIST and physical SRAM behavior | DFT and SRAM-facing controls | Functional reset/DFT routing is checked; physical array and MBIST behavior are below this UT boundary | Integration-owned |

`VERIFICATION_PLAN.md` contains the behavioral test-point inventory and closure
criteria. `CONSTRAINED_RANDOM.md` contains the executable constraint fields and
per-seed stimulus coverage contract. Any new top-level port or legal class must
update all three documents and pass `make check-docs`.
