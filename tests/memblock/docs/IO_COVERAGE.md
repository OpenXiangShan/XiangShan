# MemBlock Top-Level IO Coverage

## Scope and interpretation

This is the current stimulus/check map for the generated MemBlock boundary.
[DESIGN.md](DESIGN.md) explains transaction flow and ownership;
[ORACLES.md](ORACLES.md) defines expected behavior;
[VERIFICATION_PLAN.md](VERIFICATION_PLAN.md) owns behavioral points, gaps and
acceptance. No inventory count or sampled activity establishes semantic closure.

[expected_ports.json](../config/expected_ports.json) is the flattened interface
inventory. [memblock.json](../config/memblock.json) defines grouping, lane counts,
widths and fixed queue dimensions. [semantic_ports.json](../config/semantic_ports.json)
and [DutAdapter](../cpp/environment/dut_adapter.hpp) provide private semantic
access. Generated defaults/readback can touch a pin without implementing a legal
transaction or an oracle for it. The pin-space scenario applies 256 patterns to
747 non-clock/non-reset inputs and samples 586 outputs while reset is asserted;
input readback and an output digest are access checks only.

The catalog in [catalog.json](../cpp/scenarios/catalog.json) contains 91 scenarios:
90 directed scenarios and the constrained-random scenario. The benchmark selects
85 entries; cmo-store-flush, redirect-store-flush, tag-ecc-ways, vector-fof-high,
vector-segment-noncanonical and translation-alias are catalogued but not
benchmark-selected. This
is registration, not passing-run or coverage evidence. MixedScenario implements
the canonical constrained-random catalog entry. Constraint input/output uses
constraint_schema=58. The legal field definitions and per-run observed counters
are in [CONSTRAINED_RANDOM.md](CONSTRAINED_RANDOM.md), not duplicated here.

## Generated interface inventory

This block is generated from the manifest by
[check_io_coverage.py](../scripts/check_io_coverage.py). Keep it exact; changing
prose does not authorize regenerating the tracked DUT manifest.

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

## Driver, monitor and oracle owners

| Owner | Responsibility |
| --- | --- |
| [control.cpp](../cpp/environment/control.cpp), [issue.cpp](../cpp/environment/issue.cpp) | Capacity-aware dispatch; stable ready/valid issue; ROB controls; scalar/vector/atomic/prefetch stimulus. |
| [translation_context.cpp](../cpp/environment/translation_context.cpp), [translation_mapping.cpp](../cpp/environment/translation_mapping.cpp), [translation_ports.cpp](../cpp/environment/translation_ports.cpp), [page_table_builder.cpp](../cpp/environment/page_table_builder.cpp) | Independent page tables, mode/context/permission controls, IFetch PTW/L2 DTLB and trigger programming. |
| [manager_control.cpp](../cpp/environment/manager_control.cpp) | Configure response delays, channel backpressure, error injection and explicit probes. |
| [TileLinkMemoryAgent](../cpp/environment/dcache_agent.inc), [PtwMemoryAgent](../cpp/environment/ptw_agent.inc), [UncacheMemoryAgent](../cpp/environment/uncache_agent.inc) | Drive manager inputs and inspect accepted memory-side traffic; maintain separate bus memory and protocol records. |
| [FeedbackMonitor](../cpp/environment/feedback_monitor.cpp) | Sample writebacks before the edge, feed scoreboards, record feedback/sideband events and collect vector replay requests after the edge. |
| [QueueAccounting](../cpp/environment/queue_accounting.cpp) | Sample dispatch independently of driver counters; conserve aggregate allocation, dequeue and explicit cancellation, without a complete per-uop event identity oracle. |
| [BridgeMonitor](../cpp/environment/bridge_monitor.cpp) | Build delayed/held routing expectations from captured top-level inputs. |
| [model.inc](../cpp/environment/model.inc), [scoreboards.inc](../cpp/environment/scoreboards.inc), [completion.cpp](../cpp/environment/completion.cpp) | Byte/operation/translation reference state; match result identity/data/metadata; drive legal completion/commit/replay and cancellation. |
| [frontend.cpp](../cpp/environment/frontend.cpp), [reset.cpp](../cpp/environment/reset.cpp) | Frontend bridge transaction checks, reset recovery and reset-held pin-space checks. |

Environment::Impl::tick in [cycle.cpp](../cpp/environment/cycle.cpp) defines the
sampling phases. Response fields driven by a manager and then read back by that
same manager validate harness transport only. Independent result/side-effect
checks are still required to establish DUT behavior.

The generated [protocol SVA](../sv/memblock_protocol_sva.sv) checks valid/payload
stability for retained DUT producers including DCache A/C/E, PTW A, Uncache A,
frontend producer channels and IFU PTW response. These checks require an
assertion-enabled model containing the bind file. They do not replace source/
sink lifetime, accepted-multibeat or architectural side-effect checking.

## Pin-family stimulus and check map

Each listed item is an exact flattened-name prefix, unless it is a complete pin
name. A trailing asterisk includes every retained suffix and lane; brace notation
lists alternatives, not additional pins. These families cover the manifest;
consult it for individual fields, widths, direction and pruned fields. Driver
names below are Environment::Impl members unless an owning class is stated.
Existing stimulus refers to executable scenarios, not an assertion that all
legal field combinations are covered. G identifiers refer to the gap table.

### Backend operations and translation

| Exact pin family | Driver and observation/check | Existing legal stimulus | Remaining scope |
| --- | --- | --- | --- |
| io_ooo_to_mem_enqLsq_needAlloc_*, io_ooo_to_mem_enqLsq_req_* | enqueue_load_batch, enqueue_prefetch_batch, scalar/vector/atomic enqueue helpers; QueueAccounting samples accepted dispatch separately. | Mixed dispatch widths/lanes, queue pressure, scalar/vector allocation and redirect cancellation. | Capacity headroom and concrete generated shape limit the explored dispatch state; G5/G6. |
| io_ooo_to_mem_issueLda_* | issue_load/issue_load_batch/issue_prefetch; LoadScoreboard observes writeback by ROB/destination identity. | Integer/FP width/sign, signed immediate, dependency metadata, cache hit/miss, forwarding, software prefetch, misalignment, translation/fault paths; mixed concurrent loads. | Cross-products require observed per-run counters; no writeback-ready input; G5/G7. |
| io_ooo_to_mem_issueSta_*, io_ooo_to_mem_issueStd_* | issue.cpp address/data/atomic helpers; StoreScoreboard and scalar result scoreboard; constrained-random final word readback; `atomic-contracts` dirty-line ProbeAckData persistence. | Random older-store/younger-load byte dependency windows with a younger-store age control, address/data order and lane mixing, widths, forwarding, committed/exception stores, single/sequential/same-line/three-line consecutive CBO.ZERO with legal ROB retirement, age-direction forwarding controls, atomics, CMO, uncached/MMIO. | A CBO.ZERO stays at the ROB head until its deferred SBuffer completion; overlapping CBO completion windows are not legal UT inputs. Not every Scala atomic operand/uop shape has a driver; aliased/non-cacheable/fault-path global visibility remains partial; PBMT=NC CBO is an unspecified boundary; G6/G8. |
| io_ooo_to_mem_issueVldu_* | make_vector_memory_issue, vector issue/replay helpers; VectorMemoryScoreboard. | Unit-stride, strided/indexed, segment/FOF, whole-register, mask/tail/VSTART, register/uop shapes, split accesses, faults and partial store replay; full 128-bit mask operand, including directed high-element load/store/indexed cases. Legal multi-uop segment prefixes may defer a page/access fault to a later uop only with an explicit final-fault and side-effect oracle. A PMP page-crossing load checks exact later-element fault metadata and accepted demand through redirect recovery; its cold both-readable control checks data and second-page demand. | G4/G5: ordered side-effect targets and broader fault progress metadata remain partly checked; a two-uop fault with `vstart=8` and exact VA is directed; reserved/illegal shapes remain filtered. |
| io_mem_to_ooo_writebackLda_* | FeedbackMonitor::sample_before_edge -> LoadScoreboard::observe. | Scalar/prefetch/atomic/split-load identity, result, exception, replay, write-enable, trigger and configured debug class. | Metadata options and producer variations need explicit expectations; all source bits are not automatically checked by sampling. |
| io_mem_to_ooo_writebackSta_*, io_mem_to_ooo_writebackStd_* | FeedbackMonitor -> StoreScoreboard::observe_address/observe_data; mixed finish readback of tracked cacheable identity-mapped and declared permuted-page words. | Matching address/data acknowledgements, exception and configured sidebands; deferred uncached/CBO.ZERO cases; final cacheable word observation. | The acknowledgement pins alone do not prove persistent store bytes; the final readback excludes remaining legacy aliased and legally discarded/errored CMO effects and non-cacheable data is checked against the manager bus image; G6. |
| io_mem_to_ooo_writebackVldu_* | FeedbackMonitor -> VectorMemoryScoreboard::observe. | Returned bytes/mask, destination and vd-index, vector config, success VSTART, FOF fix-VL and configured exception/trigger/debug metadata. | G5: fault VSTART is optional and most config/data checks are bypassed for exception progress. |
| io_mem_to_ooo_staIqFeedback_*, io_mem_to_ooo_vstuIqFeedback_* | FeedbackMonitor::sample_after_edge records queue identity, hit/replay and vector replay mask/index; completion helpers reissue selected transactions. | iq-slow-feedback, store-tlb-miss-preserve, vector-store and mixed replay traffic. | Complete producer/exception/concurrency attribution is absent; G7. |
| io_mem_to_ooo_wakeup_*, io_mem_to_ooo_ldCancel_* | FeedbackMonitor records lane, destination/write class and cycle; load-feedback and forwarding scenarios assert selected relations. | Cache/refill wakeup, dependency/bank conflicts, load cancel and cancellation recovery. | Event recording alone does not validate each producer or exactly-once wakeup/cancel relation; G7. |
| io_mem_to_ooo_memoryViolation_* | FeedbackMonitor records complete retained redirect metadata; memory-violation and rar-violation compare independently chosen victims. | RAW/RAR, release/coherence overlap, ROB wrap and replay paths. | Complete mixed attribution and timing/occupancy combinations remain; G7. |
| io_ooo_to_mem_lsqio_*, io_mem_to_ooo_lqCancelCnt, io_mem_to_ooo_sqCancelCnt, io_mem_to_ooo_sqDeq, io_mem_to_ooo_lqDeq, io_mem_to_ooo_lqDeqPtr_* | set_rob_head, pending-load/store and commit helpers; QueueAccounting tracks retirement/cancellation; FeedbackMonitor records MMIO request identities. | Queue pressure/wrap, head authorization, commit and redirect drain. | Only scommit, pendingMMIOld, pendingst and pendingPtr survive on the generated input side. No generated lcommit/pendingld/pendingVst/commit input. |
| io_mem_to_ooo_lsqio_loadMmio_*, io_mem_to_ooo_lsqio_loadMmioUop_*, io_mem_to_ooo_lsqio_storeMmio* | wait_for_mmio_request/wait_for_mmio_store_request and FeedbackMonitor; scenario-level head checks and device journal. | MMIO ordering, pending-head request, uncached width/error/outstanding and non-idempotent device paths. | Broader overlapping device/redirect/error/reset combinations remain; G3/G6. |
| io_redirect_*, io_ooo_to_mem_isStoreException, io_mem_to_ooo_lsqio_vaddr, io_mem_to_ooo_lsqio_gpaddr, io_mem_to_ooo_lsqio_isForVSnonLeafPTE | redirect/flush helpers, exception selector and scenario reads; reference VA/GPA and no-effect checks. | Scalar/vector/guest exception addresses, selective cancellation, surviving work, vector-store flush. | Fault VSTART/VL standalone outputs are pruned; vector writeback progress still has G5. |
| io_ooo_to_mem_sfence_*, io_ooo_to_mem_tlbCsr_* | translation context/mapping helpers, issue_sfence and in-flight context tests; independent page-table/permission calculations, seeded ordinary host/VS/G page permutations with byte-wise data expectations and covering root-reuse fences; mixed hypervisor HSV/HLV family with PBMT/PMP relations, manager attribution and same-context HLV readback for permitted aligned cacheable stores. | Bare/Sv39/Sv48, VS/G combinations, ASID/VMID, leaf/superpage, permission/PBMT, pointer masking, MBMC, selective fences, context changes, boundary faults, PMA/PMA/NC/IO pairs, physical PMP allow/deny and cacheable final bytes. | Cross-alias committed store/load visibility is exercised after drain and with SBuffer non-empty at an in-flight load issue; first-access cross-alias mismatch checks the exact top-level redirect, observed queue recovery and correct-data refetch; wider IQ/LQ replay remains outside (G6). Permission-fault stores are read back through a readable mapping or Bare identity fallback to prove no data leak; a PMP-denied store is read back after re-enabling the region. Further nested, multi-active-element and dynamic-overlap combinations require distinct stimulus; G5/G6/G8. |
| io_ooo_to_mem_csrCtrl_distribute_csr_w_* | write_distributed_csr/configure_pmp plus explicit distribution settling; reference permissions and side effects. | PMP/PMA, bitmap/ECC injection, CSR-configured cache/protection paths. | Distribution transport is not a general proof of every writable CSR; G8/G9. |
| io_ooo_to_mem_csrCtrl_mem_trigger_* | configure_memory_trigger; result trigger/exception and forbidden-access checks in trigger-contracts. | Exposed load/store trigger enable, address/match/select/action/chain/debug configurations and mixed trigger directions. | Remaining legal trigger cross-products and reprogramming under overlap; G8. |
| io_ooo_to_mem_csrCtrl_frontend_trigger_*, io_ooo_to_mem_csrCtrl_bp_ctrl_*, io_ooo_to_mem_csrCtrl_fsIsOff | Generated idle/default and reset-held pin-space drive/readback. | Input access/width patterns only for these retained controls. | No complete frontend branch/trigger/FS functional oracle at this UT boundary; G10. |
| io_ooo_to_mem_csrCtrl_sbuffer_timeout, io_ooo_to_mem_csrCtrl_ldld_vio_check_enable, io_ooo_to_mem_csrCtrl_cache_error_enable, io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable, io_ooo_to_mem_csrCtrl_hd_misalign_st_enable, io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable | Typed setters/control configuration; completion, feedback and manager/error observations. | Timeout/drain, load violation enable, error suppression, outstanding uncache and hardware split-access enable. | Exhaustive dynamic control changes with live work are not implemented; G8. |

### Memory-side and frontend protocols

| Exact pin family | Driver and observation/check | Existing legal stimulus | Remaining scope |
| --- | --- | --- | --- |
| auto_inner_dcache_client_out_a_* | TileLinkMemoryAgent drives ready; capture_before_tick validates accepted request with dcache_a_protocol_error; tracks A sources until final matching D beat. | Get, AcquireBlock/AcquirePerm, CMO; alignment/size/mask/grow/source/user alias/page-offset; request stalls and delayed refills. | G1: generated SVA checks stalled A stability in assertion-enabled builds; A source lifecycle is not sink lifecycle. |
| auto_inner_dcache_client_out_b_* | request_dcache_probe/manager drive; accepted probes enter pending identity/report/data tracking and are matched by source to mark C eligibility. | Permission caps, data/no-data probes, source reuse/wrap, resident/absent lines and overlap with releases/refills. | Same-PA B presentation is serialized through final C and waits for prior final D plus E; alias recalls use stored VA colour and drained independent images. Arbitrary simultaneous synonyms remain outside. |
| auto_inner_dcache_client_out_c_* | Manager ready and capture_before_tick; generated SVA and C stalled-payload check; accept_probe_response/accept_release validate records, reports, line data, configured expected lines, fixed multibeat source/param/size/raw address, DUT full-line data-less Release size/alignment and shrink param 0..2, and per-beat corrupt clear where applicable. | ProbeAck/ProbeAckData, clean/dirty Release/ReleaseData, multibeat/stall windows, store visibility on release. | TileLink user/echo fields are DUT-specific and not required stable; data-less Release `corrupt` is ignored; Same-address Probes are serialized to disambiguate client-owned C sources; G6. |
| auto_inner_dcache_client_out_d_* | Manager responds with source/size/sink/permission/keyword/data/error; D handshake removes queued beat and eventually releases A source. Result scoreboards and error checks inspect DUT consequences. | Multibeat grants, read/permission/CMO acknowledgements, response/interbeat delay, held-line response and denied/corrupt beats. | Manager input readback is not DUT-consumption proof; no broad cross-source D-reordering/interleaving campaign; G1. |
| auto_inner_dcache_client_out_e_* | Manager drives ready; accepted sink must match expected_grant_acks_; sink must be busy from an accepted first Grant/GrantData beat (same-cycle D/E allowed) through E. | GrantAck count, sink identity, first-D ordering, sink reuse, stalls and error-refill acknowledgement. | G2: the expectation is still allocated when responding to A, but the active first-D/sink-busy table is an independent causal gate; generated SVA checks stalled E stability. |
| auto_inner_ptw_to_l2_buffer_out_a_*, auto_inner_ptw_to_l2_buffer_out_d_* | PtwMemoryAgent drives A ready/D; captures request shape, memory bytes, response identity, outstanding count and per-source busy lifetime; independent walk/permission result checks. | Translation modes/leaves, invalid/reserved PTEs, permission/guest/PBMT/MBMC paths, delayed PTW, denied/corrupt selected beats. | G3: per-source reuse and inactive-D are rejected, but general response-order permutations and malformed-response campaigns remain; generated SVA checks stalled A stability. |
| auto_inner_buffers_out_a_*, auto_inner_buffers_out_d_* | UncacheMemoryAgent validates accepted width/address/mask, services independent memory/device journal, drives D, counts outstanding work and checks per-source busy lifetime. | 1/2/4/8-byte loads/stores, signed/unsigned loads, outstanding/non-outstanding settings, MMIO/device effects and denied/corrupt response. | G3: per-source reuse and inactive-D are rejected; generated SVA checks stalled A stability, while malformed/response-order campaigns remain. |
| auto_inner_frontendBridge_icache_in_*, auto_inner_frontendBridge_icache_out_* | exercise_frontend_bridges drives frontend A and external A-ready/D; compares ordered source/address and two 256-bit D data beats with corruption. | Buffered fetch transport, ready stalls, source turnover, reset cancellation and surviving new requests. | No backend execution, ICache refill semantics or complete frontend TileLink legal-shape oracle; G10. |
| auto_inner_frontendBridge_instr_uncache_in_*, auto_inner_frontendBridge_instr_uncache_out_* | Same bridge exerciser; compare retained request address and 64-bit response source/data/corrupt. | Instruction-MMIO transport and reset recovery; constants are supplied by InstrUncacheBuffer. | Generated in-side has no D-ready pin and source is fixed; no instruction/device semantics; G10. |
| auto_inner_frontendBridge_icachectrl_in_*, auto_inner_frontendBridge_icachectrl_out_* | Same bridge exerciser; compare retained opcode/size/source/address/mask/data and D; frontend D-ready can stall. | Read/write control transport, source turnover, payload stability during selected stalls, reset-discarded A/D and clean recovery. | Transport checking is distinct from functionality of the external cache-control target; G10. |
| io_fetch_to_mem_itlb_req_*, io_fetch_to_mem_itlb_resp_* | start/complete/issue_ifetch_ptw_request; response backpressure/stability and independently built page-table response expectations. | Bare/stage-one/two-stage entries, errors/permissions, context/fence cancellation and held responses. | Wider requester overlap and integration-side consumer behavior are outside this boundary; G8/G10. |
| io_l2_tlb_req_*, io_l2_pmp_resp_* | issue_l2_tlb_request and issue_killed_l2_tlb_request; scenario-level address/PBMT/fault/PMP checks. | no-translate/prefetch/kill and translated/faulting accesses under protection configurations. | Generated request-ready/response-ready are absent; external L2 retry/queue ownership is not modeled; G10. |
| io_l2_hint_* | pulse_l2_hint; downstream activity/result checks in l2-tlb-contracts and related scenarios. | One-cycle hint inputs and idle/no-matching-MSHR behavior. | Matching-MSHR hint/replay policy is not comprehensively stimulated/checked; G7/G10. |

### Control, bypass and observations

| Exact pin family | Driver and observation/check | Existing legal stimulus | Remaining scope |
| --- | --- | --- | --- |
| clock, reset, io_reset_backend, io_resetInFrontendBypass_* | DutAdapter clock/step; reset clears agent link state and asserts reset; check_reset_backend_contract and BridgeMonitor. | Functional/DFT/scan reset assertions/releases, buffered/in-flight cancellation and restart. | Shared reset-domain assumption is modeled; four-state/uninitialized and arbitrary reset crosses remain G9/G10. |
| io_dft_*, io_dft_reset_*, io_dft_frnt_*, io_dft_reset_frnt_*, io_dft_bcknd_*, io_dft_reset_bcknd_* | check_dft_bridge_space enumerates 1024 input patterns under reset; direct routing equality and reset-tree checks. | All ten exposed control-bit combinations for frontend projection, backend cgen and reset projection. | Physical SRAM, MBIST execution and scan-chain semantics are outside the check; G10. |
| io_ooo_to_mem_flushSb, io_mem_to_ooo_sbIsEmpty, io_wfi_* | pulse_sbuffer_flush/set_wfi and require/run-until-safe; result/manager/queue checks. | Buffered-store/uncache drain, maintenance/atomic/translation work, WFI pending activity and reset recovery. | More overlapping producer/error states need explicit traffic and no-effect checks; G6/G8. |
| io_ooo_to_mem_csrCtrl_flush_l2_enable, io_l2_flush_done, io_outer_l2_flush_en, io_ooo_to_mem_csrCtrl_power_down_enable, io_outer_power_down_en | drive_l2_flush/drive_top_controls; BridgeMonitor direct enable and registered done expectations. | Enable/done and power control patterns during idle/selected live traffic. | External L2 flush completion and power-controller correctness are integration-owned; G10. |
| io_hartId, io_outer_reset_vector, io_inner_reset_vector, io_ooo_to_mem_backendToTopBypass_*, io_outer_cpu_halt, io_outer_cpu_critical_error, io_outer_msi_ack, io_mem_to_ooo_topToBackendBypass_* | drive_top_controls/drive_top_bridge_stimulus; BridgeMonitor direct/delayed input-derived comparisons. | Hart/reset-vector, halted/critical-error/MSI acknowledgement, interrupt/time/MSI and flush-done routing. | No full backend execution/trap/interrupt-service model; G10. |
| auto_inner_clint_int_sink_*, auto_inner_plic_int_sink_*, auto_inner_debug_int_sink_*, auto_inner_nmi_int_sink_*, auto_inner_beu_local_int_sink_*, io_fromTopToBackend_* | drive_top_bridge_stimulus; BridgeMonitor validates delayed external interrupts/time/MSI and NMI OR relation. | Individual/composed interrupt levels, valid payloads and quiet transitions. | Interrupt priority, claim/acknowledgement and service are not verified; G10. |
| io_inner_beu_errors_icache_*, io_outer_beu_errors_icache_* | drive_top_bridge_stimulus; BridgeMonitor registered error/address bypass. | Valid/error-address transport and deassertion. | Does not create or verify an ICache physical error; G10. |
| io_dcacheError_*, io_uncacheError_* | Manager response-error controls, distributed ECC injection; FeedbackMonitor bus-error records and scenario assertions. | D-channel denied/corrupt, selected tag/data ECC bit classes, enable/suppression and attribution/isolation. | Injection uses the configured test-control mechanism; broader persistent/multibit/overlap states remain; G9. |
| io_ooo_to_mem_csrCtrl_pf_ctrl_*, io_outer_l2PfCtrl_* | configure_stride_prefetch/configure_sms_pht_prefetch and top stimulus; BridgeMonitor two-stage control projection. | Enables/thresholds/stride/hit-training/control fields and selected trained traffic. | Control transport and exercised requests do not prove prefetch quality or complete algorithm selection; G10. |
| auto_inner_l2_pf_sender_*, auto_inner_l3_pf_sender_*, io_ifetchPrefetch_* | Software-prefetch issue/training traffic; FeedbackMonitor records addresses/source; ifetch-prefetch/hardware-prefetch assert selected positive/negative relations. | Instruction-prefetch forwarding; stride/SMS directions and disabled-output classes. | No architectural oracle for all predictor/training outcomes or usefulness; G10. |
| io_traceCoreInterfaceBypass_* | drive_trace_bridge_stimulus; BridgeMonitor captures inputs and tracks registered valid, enabled holds, trap/privilege and address-plus-offset. | Three groups, encoder control, offset/iretire/itype/size, valid/invalid hold cases. | Trace encoder/decoder and full instruction stream consistency are integration-owned; G10. |
| io_outer_hc_perfEvents_*, io_inner_hc_perfEvents_*, io_perf_* | Hc event drive/read and BridgeMonitor shared-lane registered equality; pin-space samples all remaining outputs. | Generated input lanes 1..68 and output lanes 0..67; routing equality uses shared 1..67. | Lane 0 output/performance selections require their own semantics; digest/activity is observational; G10. |
| io_topDownInfo_* | Top miss stimulus and BridgeMonitor delayed L2/L3 equality; topdown-contracts/queue traffic observe replay/SQ/Sbuffer/L1 events. | Selected miss/replay/full conditions and delayed external indicators. | Complete performance meaning/whole-core attribution is integration-owned; G10. |

## Constrained-random legal stimulus

MixedScenario composes existing transaction APIs. Its implementation is in
[mixed_scheduler.cpp](../cpp/scenarios/mixed_scheduler.cpp),
[mixed/scenario.hpp](../cpp/scenarios/mixed/scenario.hpp), the mixed action/policy
files and [constraints.inc](../cpp/random/constraints.inc). Profiles constrain
operations, address locality/boundaries, known-address byte dependencies and
one-store/one-load unresolved-address recovery with strict refetch data, vector shape and bounded
index permutations/repeated gathers with disjoint scatter spans, static
unrestricted host/VS/G page permutations with independent byte expectations
and physical final observation, translation/protection, errors, probes and independent response latency. A sampled field is
not evidence that its intended state was reached; per-seed observations and
report identifiers are defined in [CONSTRAINED_RANDOM.md](CONSTRAINED_RANDOM.md).

Directed scenarios are useful when a precise contract requires setup/observation
that the mixed path does not provide. The current catalog and implementation,
not this summary, determine which shapes are executable. No broad pin-pattern
sweep may be treated as legal random traffic.

## Unimplemented or incomplete stimulus and checks

These are present capability limits, not requests to weaken a check. New legal
stimulus must have an independent consequence oracle before its activity is
counted as covered.

| ID | Current limit | Missing legal stimulus/check |
| --- | --- | --- |
| G1 | DCache A is validated at acceptance and generated SVA checks stalled stability. A sources remain busy until final D. D responses are principally queued, with selected directed reverse-order cases rather than broad permutations. | Qualify assertion-enabled builds and exercise legal cross-source latency/order combinations with explicit source/beat/result matching and no response-derived expected data. |
| G2 | TileLinkMemoryAgent::respond still installs expected_grant_acks_ when A is serviced, but the accepted E now requires an independently observed first Grant/GrantData beat, or that same cycle, and a sink is busy from first D through E. Probe C acceptance now requires an accepted B expectation for the same address; C's client-owned source is not tied to B source. Generated SVA checks stalled E stability. | Same-PA Probes are serialized through final C and cross-alias grants wait for recall data. Broader reset/version/ordering combinations remain; qualify the existing E stability assertion and extend legal D/E order combinations. |
| G3 | PTW and uncache now reject source reuse before final D and D completion of an inactive source. Generated SVA checks their stalled A stability when enabled. | General response-order permutations, duplicate/early/late/malformed responses and reset cancellation with independent result/device checks remain. |
| G4 | Full mask bits 0..127 are represented (`mask_bits` plus `mask_high`) and driven on all 16 `src(3)` bytes. Directed vector-addressing cases check high-element unit-stride load/store, readback, LMUL8 full-range masks, ordinary indexed and special-indexed (EMUL>LMUL) shifts; mixed generation populates high bits when `vm=0`. The writeback comparator still observes a uop's 16 local mask bits, which is the width of that response field. | Ordered side-effect/device semantics, every reserved shape, and a standalone scoreboard test of all mask/element/vector-length combinations remain open. |
| G5 | VectorMemoryScoreboard compares exceptional VSTART only when expected_writeback_vstart is supplied. On exception progress it omits ordinary VSEW/EEW/LMUL/VL/NF and normal data/mask checks; vuop_idx and pdest identity matching remains active. Standalone fault VSTART/VL outputs are pruned. | Independent fault-element/progress expectations and checks for every meaningful retained exceptional field; broader multi-active-element, nested/boundary and masked exception/FOF partial progress. Existing success checks do not close this. |
| G6 | MixedScenario::finish now drains Sbuffer/DCache/queues, switches to Bare and reads back every tracked cacheable identity-mapped committed 8-byte word against the reference image. Permitted/aligned cacheable hypervisor stores also use a same-context HLV readback. `translation-alias` separately proves a committed store through one Sv39 VA is visible through a second VA to the same PA, both after drain and while the store is in flight with SBuffer non-empty at load issue. Committed non-cacheable store/atomic words are compared against the manager bus image after drain. CMO-maintained lines are dropped on both success and error paths, and remaining aliased commits are counted as skips. | The first-access cross-alias window requires a matching flush-itself `memoryViolation` before discarding wrong data, then drives redirect recovery and correct-data refetch with observed queue accounting; `ld2Cancel` alone never grants a data opt-out. Broader replay/refetch overlap remains. Extend the effect oracle to the remaining aliased translations, device side effects, fault-path forbidden bytes and legal cancellation/partial-fault expectations. |
| G7 | Feedback monitors record many sidebands; selected directed scenarios check relations. There is no universal identity/count/field oracle for every wakeup, cancel, violation, hint and replay producer. | Explicit producer expectations and mixed overlap checks, matching-MSHR hint scenarios, and wider split-load occupancy/delay states. |
| G8 | Controls/modes/permissions are richly configurable, but arbitrary dynamic reprogramming during overlapping cache, translation, vector, queue and maintenance traffic is not exhaustively generated. A PBMT=NC CBO.ZERO was probed and reports a store access fault before an uncache request; the architecture leaves CBO behavior for non-cacheable memory unspecified, so neither fault nor eight-write behavior is asserted. | Legal state transitions with independent fence/context/permission and surviving/canceled-work expectations; wider trigger/protection/context overlap. |
| G9 | Two-state simulation and bounded error injection do not establish four-state storage behavior or all physical ECC recovery cases. | Four-state/formal storage checks and broader persistent/multiple-event corruption/recovery stimulus where an external contract exists. |
| G10 | Frontend/top/DFT/trace/performance/prefetch paths include transport and characterization checks, not the complete external system consuming them. | Integration verification for interrupt service, power/flush controller, frontend execution/cache-control targets, trace codecs, MBIST/physical SRAM and full performance/prefetch semantics. |

The authoritative behavioral inventory and regression requirements remain in
[VERIFICATION_PLAN.md](VERIFICATION_PLAN.md). Interface additions require review
of this map, the generated inventory, semantic access, legal drivers and the
matching oracle; running a generator alone does not establish that consistency.
