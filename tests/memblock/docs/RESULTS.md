# Current MemBlock Qualification

## Scope

This file describes current readiness and its qualification boundary. It is
not a chronological log. Capability inventory comes from the current catalog,
producer/check implementations and the current-state documents, not preserved
campaign claims.

The catalog registers 91 scenarios: 90 directed-tests plus constrained-random.
The default benchmark selects 85; six additional directed contracts require
explicit execution. All claimed behavior remains limited by IO_COVERAGE.md
and ORACLES.md. There is no defensible overall functional-coverage percentage.

## CPU Bug Records

None.

## Current Changes Qualified Below

- Legacy issue helpers reject a bound retained driver before changing pins or
  advancing an edge, including an empty owner. Vector replay service rejects
  before consuming its request. Single STD offers preserve software ROB wrap
  for delayed cancellation despite the pruned public flag pin. These close
  driver correctness prerequisites; persistent ordinary issue remains open.

- Ordinary cacheable mixed recovery retains exact terminal expectations through
  all three monitored redirect-return edges. Only at the matching completed
  boundary does it cancel killed unauthorized store groups and remaining
  terminal obligations. Authorized groups and surviving work remain required;
  final canceled-STA classification includes propagation feedback. The manager's
  completed-boundary precondition remains intact. Host scripts and native mixed
  cancellation replay qualify this bounded correction; unmatched STD attribution
  and broader producer/operation timing remain open.

- The fixed-eight-load randomness check now removes dependency-selection records
  before measuring event diversity, and separately requires variation in
  admission/retirement interleavings and load issue order. Random addresses,
  payloads, timing or source choices cannot alone satisfy that event check.

- Schema 76 adds ordinary initial same-store STA/STD pairing when both operand
  events are ready. The retained driver independently accepts each lane after
  manager drive, marks store-scoreboard obligations before same-edge STD
  completion, and registers/authorizes release input groups before the edge.
  `stream-store-pair` controls selection; accepted same-edge/STA-first/STD-first
  counts and edge skew distinguish selection from actual overlap. Graph
  operands, retries and independent admission during a stalled pair remain
  separate work. Byte-prefix and final-state oracles remain strict.

- The shared retained producer driver owns independent ordinary lane slots,
  captures acceptance after manager drive at the common pre-edge boundary and
  emits events only after that edge. The legacy scalar/vector load pair now
  uses it, retaining scalar stalls and preserving vector timeout/retry
  obligations. Host witnesses check payload stability, exact events, circular
  redirect age and delayed cancellation tails. General sliding load/vector issue
  and independent admission during retained stalls still require migration.

- Schema 75 separates admitted generations, accepted initial issue events and
  architectural retirement after a mixed-suffix redirect. Canceled class/uop,
  dependency, VSTART and final STA-feedback records preserve exact lifecycle
  accounting. Repeated-Vd vector chains can cancel before consumer issue;
  generated chains and source reuses remain distinct from retired-uop counts.
  Data, identity, legal suffix cancellation and physical queue recovery checks
  remain separate requirements. This is a verification-accounting correction;
  the ordinary stream still uses blocking issue helpers.

- Legacy heterogeneous windows may offer a scalar/FP load and first vector-load
  uop together while preserving each request until its own handshake. The pair
  driver now retains the scalar frontend/dependency metadata used by ordinary
  issue. `coissue` requires both handshakes on one sampled edge; it is coverage
  evidence alongside independent scalar/vector terminal checks. The canonical
  sliding stream still selects one blocking producer helper per turn.

- The DCache manager provides independently grouped ordinary-input ReleaseData
  versions, with first-C snapshots, least-witness promotion, retained omitted
  updates, visible-progress floors, exact overrides and certified epoch anchors.
  Pre-epoch legacy updates cannot be replayed after certified epoch entry. This is a manager
  capability checkpoint; mixed-driver integration and its native qualification
  remain separate work.

- LSQ monitor schema 3 adds passive enqueue cadence while constraint schema
  remains 73. Ordinary admission uses both post-admission watermarks without a
  fixed quiet interval, ages public dequeue/reconciled cancellation credits by
  three further edges and blocks six further edges after each redirect-high
  sample. The guard restarts on repeated highs. Immediate correctness totals,
  lane/flow identity, terminal checks and pressure-helper behavior remain intact.
  This admits consecutive normal-workload inputs using existing capacity; it
  does not decouple blocking issue or architectural retirement from SQ dequeue.

- Schema 73 keeps ordinary scalar-store address feedback and retry state per
  instruction. An accepted STA returns to the ready-event scheduler; only its
  matching lane/SQ value/wrap feedback advances that attempt, a miss preserves
  the eight-cycle cooldown, and a hit suppresses further attempts. Other ready
  producer events may progress during the wait. Expected bytes, checked older
  address gating and terminal scoreboards remain active. `stream_sta` conserves
  attempts/hits/misses, measures accepted work during another store's wait and
  samples the pending high watermark at every increasing acceptance boundary.
  Blocking handshakes/retirement and the synchronous unresolved-graph operand
  helper remain open density limits. This removes one
  ordinary-workload barrier, without claiming complete producer independence.

- Schema 72 extends ordinary stream epochs from the former 8..32 draw to
  configurable inclusive bounds, default 8..256 and maximum 4096, clipped to
  remaining action budget. One byte image and translation context persist
  across prefix retirement without an intermediate per-32-instruction drain.
  Live depth stays 2..8 and existing vector-flow/head/replay gates remain.
  Admission rejects duplicate live ROB value/wrap identities; history reports
  extended epochs, longest length, late accepted overlap and terminal identity
  reuse. Dependency depth is bounded by observed epoch length, not just the
  configured cap. Existing declaration-before-issue, SQ-only replay generation
  and bare scalar STD attribution gaps remain explicit; longer histories do
  not establish universal stale-response detection or full CPU timing.

- Schema 71 adds ordinary integer-load producer edges inside canonical streams.
  Independent address/data edge draws choose older live producers, freeze values
  from the admission-order byte image and gate each consumer operand on checked
  producer completion. Shared producer lifetimes survive prefix retirement;
  unresolved graph roles remain excluded. Address edges feed scalar/FP/vector
  loads, stores and prefetch bases through a bounded external ALU calculation;
  scalar stores can independently consume full predicted data. Existing scalar
  stores crossing a page end retain their final-address clamp. No observed DUT
  value advances expected state. This improves workload-style dependency timing;
  general vector-result/index graphs, full rename history and unbounded continuous
  epochs remain gaps.

- Schema 70 separates legacy-window and sliding-stream eligibility. Legacy
  windows retain their required five producer classes and reserved queue budgets;
  stream-only profiles accept any nonempty ordinary scalar/FP, vector or prefetch
  subset. Each enabled vector direction needs a full instruction fitting actual
  stream queue headroom. Empty populations and non-fitting directions reject
  before traffic. Sparse accounting rejects disabled stream classes and legacy
  observations in stream-only mode. Expected bytes and completion/retirement
  oracles stay unchanged. Broader producer graphs and bounded
  epoch/context drains remain ordinary-workload priorities.

- Ordinary mixed streams sample variable scalar/FP load, vector load, scalar
  store, vector store and prefetch composition in configured 1..4096-instruction
  epochs (default draw 8..256) with
  2..8 live instructions. Ready events interleave independent STA/STD, vector
  uops and completed-head retirement; new admission continues after prefix retirement while younger work stays
  live. One deque holds the actual oldest ROB head, with distinct live register
  destinations, exact replay service and scalar/vector commit separation.
  An independent admission-order byte image freezes younger load values from
  initial bytes plus older input stores. Beat-crossing scalar STA waits for
  actual head eligibility so its blocking acceptance helper cannot stall older
  retirement. Schema 62 conserves initial issue, admission, retirement, replay
  and placement observations. Schema 67 records admission/producer choices
  made while a complete head could retire. The serial reservation is capped at
  half the available action budget, keeping short default spec/coverage/corner
  concurrency reachable. `concurrent-stream` defaults to 750; zero retains
  legacy prefix scheduling. Epoch/context drains, conservative older-address
  gating and full rename/cancellation histories remain explicit limits.

- `stream-unresolved` (schema 68) introduces variable 1..4-store/1..4-load
  episodes inside the same ordinary stream. An older mixed prefix retains its
  actual head, byte-image inputs, STD/replay progress and strict obligations.
  Exact oldest-victim redirect and whole-suffix refetch use observed LQ recovery;
  the graph's retirement anchor permits only surviving prefix SQ dequeue.
  A conflict waits for a non-store actual head or anchor. Episode completion
  resumes ordinary admission without queue/SBuffer/D/E drains, while graph
  destinations remain reserved through retirement. Execution resolution and
  final global committed-word observation stay separate; the isolated helper
  still requires its terminal readback. Schema 69 adds a conditional youngest
  unfinished cold scalar/FP role. Freshness excludes accepted A coverage across
  its physical line, explicit reference writes and older input-store bytes,
  retaining prefix progress without a SBuffer drain. Its strict expectation
  survives redirect propagation and cancels only at the exact positive observed
  LQ boundary. Target-line Grant completion requires both final D and E; accepted
  request history restarts the 32-cycle quiet interval even if a successor
  lifecycle completes between scheduler polls. Every later cold execution
  strictly completes. Broader unfinished overlap, store-headed redirect/walk
  levels and full producer/predictor/rename timing remain gaps.

- Ordinary scalar dependency traffic varies 1..4 older stores, 1..3 younger
  loads and an optional younger store, sampling ready STA, STD and load events.
  Loads wait for identified older address hits while older store data can remain
  unissued; optional younger operands interleave without entering the expected
  byte overlay. The common integer/FP oracle checks the completed value from
  initial bytes plus all older input stores. Schema 61 records accepted early
  loads and one/multiple overlapping pending-data stores without requiring any
  internal replay/bypass or nonzero coverage. A 4096-window generator sample has
  2978 event schedules after excluding payload, offsets, widths and delays;
  one fixed-count bucket contains 193 schedules in 196 samples. These measure
  the bounded builder, not unique full-run schedules or arbitrary sliding work.

- Canonical scalar stores retain dispatch-time LQ value/wrap snapshots; typed
  enqueue checks both queue cursors after cancellation. Scalar replay waits use
  exact ROB value/wrap and never reissue a completed STA while waiting for data.
  STA-hit waits select newly sampled lane/SQ value/wrap feedback. Vector-list
  waits preserve unrelated obligations/replays; vector commit uses its recorded
  SQ target without scalar `scommit`. Heterogeneous windows keep the actual
  oldest head and distinct live integer destinations. Compiled production-method
  tests and independent mutation review qualify these bounded prerequisites;
  the separate bounded stream provides sliding admission, while a complete
  retirement/rename ledger remains open.

- Canonical vector-data writing uops now select distinct destinations in 1..127;
  FOF fix-VL independently selects 1..31, and nonwriting uops drive zero.
  Repeated logical Vd in EMUL>LMUL indexed loads uses independently computed
  prior source bytes and the external backend's agnostic producer policy.
  Stores reuse unchanged source bytes. Unordered indexed issue samples ready
  dependency vertices; exact predecessor waits retain unissued siblings and
  cannot consume store replay feedback through a load's LQ snapshot. The raw
  vector comparator requires every old-destination byte unchanged for no-op
  instructions (`vstart >= vl`), including `vl=0`; ordinary body/agnostic rules
  remain separate. A scheduling-only sample contains 453 distinct legal orders
  out of 512 draws for four independent two-uop chains. Full allocation history,
  unbounded simultaneous writing-instruction allocation remains unqualified;
  the bounded stream reserves distinct destinations through prefix retirement.
- Canonical ordinary, heterogeneous-window, SQ-dependency and unresolved/refetch
  loads now select integer or FLH/FLW/FLD formats from seeded class/width weights.
  Independent mapped bytes and older input-store overlays determine FP payloads
  and NaN boxing; the scoreboard checks class, destination, identity and data.
  Integer/FP numeric destination domains are 1..223/1..191, with distinct live
  roles and the restored original destination on refetch. Format choices,
  execution epochs, payload signs, declared faults, misalignment and semantic
  byte sources have conserved counters. A topology-only FP dependency sample
  contains 509 distinct plans out of 512, excluding payload and timing/order.
  Complete rename history, FP physical-zero selection, auxiliary helper formats
  and exceptional misaligned FP destination-enable semantics remain unqualified.
- `translation-permuted` shuffles all 132 pages for ordinary host/VS/G traffic,
  allowing different VA/PA cache colours. A single-client PA/alias directory
  recalls the old alias and waits for final C before capturing replacement grant
  data. Prior final D and E, same-PA B serialization and racing ReleaseAck are
  modeled separately. Context/final-observation drains freeze independent owner
  images. Voluntary releases use grouped independent update histories so pending
  SBuffer stores do not force a newer cache image; aligned update atomicity and
  overlapping order remain checked. Automatic drained recalls and final physical
  readbacks stay exact. Immutable per-byte page maps support discontinuous pages;
  root-reuse fences, legal tail branch/refetch and integer destination bounds
  remain enforced. Arbitrary simultaneous synonyms and CMO/error-grant ownership
  remain outside this direction.
- `scalar-unresolved` draws 1..4 older stores and 1..4 younger scalar/FP loads.
  Every conflicting STA independently selects the oldest completed byte-overlap
  victim and recovers its whole suffix before the next STA. Victim age may move
  older/equal/younger. Strict intermediate images use only known-address older
  stores in program age; pending known STD is made ready, while unknown-address
  data remains invisible. Hidden writes may leave data unchanged. Loads restore
  ROB/pdest/PC/SQ identity and receive current FTQ and fresh LQ/scoreboard epochs;
  original untrained and subsequent trained-hit/nonwaiting metadata have an
  explicit external predictor witness. Every touched word receives terminal
  readback after age-ordered store commit. Schema 65 distinguishes unique loads
  from repeated executions and records round/victim/movement/knowledge/predictor
  observations. `scalar-unresolved-inflight` optionally reserves the youngest
  role as a held cold load. Its strict pending expectation survives redirect
  propagation, then requires a matching observed LQ cancellation. The old D/E
  response drains with no old writeback before same-identity suffix refetch.
  Schema 66 separates initial completions/cancellations, unavailable fresh-line
  attempts and strict cold refetches. Full producer timing, dynamic SSIT/LFST and
  rename history, broader unfinished overlap and arbitrary younger admission
  across sliding redirects remain gaps.
- Ordinary and segment indexed operands now vary bounded offset spacing,
  permutation and repeated gathers. Each instruction generates one complete
  architectural index vector before CPU-compatible uop slicing. Scatters use
  disjoint whole-field groups, so reference byte expectations remain exact.
  `vector-index-repeat` selects the load reuse rate, with strict endpoint
  controls; generated operand counters include inactive elements explicitly.
  A topology-only sample has 512 distinct offset vectors out of 512 plans.
  Production builder checks cover all 78 ordinary indexed shapes, and vector
  scoreboard mutations reject swapped/missing repeated-element data for
  ordinary and segment loads.
- The scalar alias-refetch oracle requires a simultaneous, fully identified
  flush-itself request before discarding data, preserves all non-data checks,
  returns the isolated backend redirect after two registered stages, and
  requires a fresh correct-data completion. The directed case reports
  `canceled=1 refetched=1 completed=1 lq=2+0/2`: one data pulse discarded,
  both LQ entries dequeued, no fabricated LQ cancellation.
- Ordinary random bare/cache/atomic/NAPOT and merge/miss/bank pools use lazy
  seed-and-physical-address initial words. Reference and manager images share
  initialization while later writes remain independent. Six production-model
  unit tests pass, including wrong-address data substitutions accepted by old
  repeating patterns and rejected by the actual load scoreboard with new data.
  Fresh-context independent review approves this scoped memory change with no
  findings; artifact `build/memblock/scalar-dependency-20261028/memory-review.txt`.
- The PMP page-crossing vector load now checks accepted external requests
  through its vector-exception redirect recovery and includes a cold
  both-readable data/demand control. The single-uop shape uses LMUL1.
  `translation-faults` passes with one fault, one request-history check and
  one permissive control in `build/memblock/pmp-cross-load-20261028/directed-v3`.
- Regression and benchmark sidecars use `coverage-reports/<run_id>/`; both
  aggregate and per-case documents record the campaign ID. Concurrent
  controllers cannot overwrite each other's sidecars if a parent is shared,
  and reruns retain previous sidecars. Operational isolation still requires
  unique parent output directories per concurrent controller. A subprocess integration test exercises
  two regressions plus a benchmark with identical scenario/seed/index and an
  aggregate-path rerun.
- Constrained-random finishes with a drain and an independent word readback of
  cacheable identity-mapped store/atomic and declared permuted-page store effects;
  permitted/aligned
  cacheable hypervisor stores add a same-context HLV readback.
- DCache E GrantAck is admitted only after a first Grant/GrantData D beat (or in
  that same cycle), with sink lifetime tracked through E.
- PTW and Uncache reject source reuse before final D and D completion of an
  inactive source; same-cycle final-D/source reuse is allowed.
- Successful CBO.CLEAN/FLUSH lines stay in the final readback; only CBO.INVAL
  and errored CMO lines are excluded. The observed-word count is incremented
  per completed batch and gated against the registered count.
- `atomic-contracts` forces a ProbeAckData on the dirty AMO line and checks the
  updated data bytes plus the manager image, so AMO post-state is not observed
  only through a cache-resident load.
- DCache C-channel ReleaseData now checks raw address/source/param/size/beat
  identity; ProbeAckData checks source/report/size/address identity; data-less
  Release checks this DUT's full-line size, line alignment and shrink param
  subset 0..2. `corrupt` is per-beat and not a data-less Release validity field;
  TileLink user/echo fields are DUT-specific, not stability fields.
- First-element FOF completions now carry an explicit exceptional `vstart`
  expectation. `cbo-zero-contracts` also contains sequential, same-line
  repeated and three-line consecutive CBO.ZERO retirement, both same-line
  CBO.ZERO/ordinary-store orders, a dirty-line zero, a younger load issued
  while the zero is still completing, and an
  older-load/younger-store/CBO age-direction control. Each driver advances the
  ROB pending pointer only after the preceding deferred CBO completion.
- Vector transactions model and drive mask bits 0..127 (`mask_bits` plus
  `mask_high`). `vector-addressing` checks high-element unit-stride load/store,
  readback, LMUL8 full-range masks, and ordinary/special indexed shifts; mixed
  vector generation draws high mask bits when `vm=0` and reports a `high_mask`
  count of masked uops with an enabled global element >=16 (seed 214: 56 in
  1024 actions; seed 215: 432 in 4608 actions). Four other 4608-action seeds
  pass with that generator, in addition to the directed cases.
- DCache ProbeAck requires an accepted B expectation for the same probe
  address; a C before B, a B without an expectation, and duplicate B
  acceptance fail. C's client-owned source is not tied to B source, so
  same-address attribution uses serialized B presentation through final C.
  The client-owned C source stays fixed across beats; it need not echo B source.
- `translation-faults` adds a two-uop vector page fault: uop0 reads the mapped
  tail of a page, uop1 faults at the next page with `vstart=8`. The exceptional
  writeback is checked for exact `vstart=8` and fault address `page+0x1000`.
- `translation-faults` also checks two single-uop page-crossing vector loads:
  one with element 0 mapped and element 1 in an unmapped page, and one with
  only element 1 active (mask bit 0 clear). Both require a load page fault and
  exact `vstart=1`; the masked case proves `vstart` is the architectural
  element index, not the active-element count. A third all-masked load at an
  unmapped VA must complete without a fault; the same run observes zero PTW
  and zero DCache requests for it. Both faulting cases require the exact
  faulting-element address in the separate exception-address payload; the
  sample follows a four-cycle settle because the vector feedback reaches the
  queue one cycle after the writeback. A vector store whose second element
  lands in a read-only page must fault with exact `vstart=1` and address; its
  read-only page is read back unchanged, while the writable first element may
  legally have been written before the fault. A masked variant activates only
  element 1 and requires both the inactive writable page and the read-only
  page to stay unchanged. A first-element variant puts element 0 itself in a
  read-only page and requires `vstart=0` with neither that page nor the
  following writable page written. A partially masked load with only element
  0 active must complete even though inactive element 1 points into an
  unmapped page.
- `vector-segment-noncanonical` issues a legal `nf=1` two-uop segment. It
  permits the first data uop to complete before the fault but requires the later
  uop of the same instruction to expose the exact noncanonical-address page
  fault, with no DCache/Uncache request or SBuffer side effect.
- `translation-alias` checks committed bytes through two Sv39 virtual pages
  mapped to one physical page. Both warmed-alias and cold-first-access loads
  use exact-identity flush-itself recovery and a strict fresh-data refetch.
  Stores retain typed dispatch snapshots, hold their pending head and retire
  after the scalar writeback guard, expose the next head on the next edge,
  and then wait for physical SQ cleanup. Native runs
  discard one provisional pulse and strictly refetch in each path. A pre-issue
  SBuffer-busy sample does not establish specific-store forwarding residence.
- Mixed finish also checks every committed non-cacheable store/atomic word
  against the manager bus image after drain. This observes the bytes carried by
  accepted A requests; it is not a DUT uncache readback, and device/CMO
  semantics remain scenario-owned.
- `translation-permissions` reads the target word back after a permission-fault
  store, using the same readable mapping or a Bare identity fallback. This
  checks that a faulting store did not leak bytes into the cache or backing
  image; ten fault-store cases are covered. `pmp-contracts` adds a PMP-denied
  store, reconfigures the region to allow reads, and verifies the original
  physical word by Bare identity readback.

## Current Qualification

The prepared-model baseline evidence uses these qualification commands:

- `make -C tests/memblock prepare-rtl check-ports check-rtl unit`
- `make -C tests/memblock picker-harness JOBS=8`
- All 90 directed catalog entries through the rebuilt harness
- A `coverage` constrained-random seed with 4608 actions

RTL generation and model rebuild were run after overlaying the current source
with public `origin/kunminghu-v2` revision
`a4eeaabb4c12349259094a8b55f3ba1c03a9d993`. The working tree also retains the
current branch's later MemBlock fixes. Artifacts are local under
`build/memblock/verify-kunminghu-v2-a4eeaabb4` and are not guaranteed by a clean
clone. Runtime metadata records layout, not source-to-model identity.

Current harness changes have the bounded qualifications in the rows below;
retained prepared-model evidence is not a new full-catalog run for each change.

| Check | Current result | Scope limit |
| --- | --- | --- |
| Ordinary-input manager host contracts | Seven methods pass in isolation against the pre-change headers; 58 passing modes, 729 independent histories/10,193 two-release transitions, 524,288 input-group stress and 9/9 compiled private faults | `build/memblock/store-retirement-20261030/manager-checkpoint-isolated/unit.log`, `manager-audit/final-evidence.json`; independent non-author forward model matches 4,000 additional decisions and rejects the stale-legacy counterexample. Synthetic manager ports do not establish native integration or full coherence coverage |
| Cross-alias producer/recovery qualification | Seven direct invocations pass across six scenarios, including exact replay of `translation-alias`. Warmed-alias recovery finishes at cycle 401 with one discarded pulse, strict refetch and total LQ 5+0/5; cold-first recovery finishes at cycle 312 with one discarded pulse, strict refetch and LQ 2+0/2. Four mixed seeds / 1024 actions pass both finite artifact verifiers and exactly match preceding summaries | `build/memblock/ordinary-async-20261030/alias-refetch-v1/{directed-v3-results.json,native-v3-comparison.json,provenance-v3.json}`. Secondary public trace confirms three store commits with pendingst high, immediate next-edge head advance and both simultaneous identified WB/redirects. Five host methods cover 26 load scripts, all 320 store ROB identities and seven private helper faults. Correct-first-data redirects and nonzero LQ cancellation remain synthetic-only; pre-issue SBuffer busy does not prove forwarding-stage residence. Corrected stimulus/recovery qualifies the UT; no CPU RTL bug or closure claim |
| Ordinary-input cancellation manager | Nine manager host methods pass, including 19 focused cancellation cases; nine new private cancellation faults and nine retained input-version faults reject | `WORKSPACE_ROOT/build/memblock/store-retirement-20261030/release-cancel-checkpoint/{evidence.json,host-ao5gtuoq/summary.json}`; cancellation removes only unauthorized groups after a caller-certified boundary, preserves first-C snapshots/history/floors and permits fresh reuse of never-applied orders. Synthetic manager ports; no mixed-suffix/native recovery qualification |
| Vector frontend input identity | Three host methods pass: 2496 whole ordinary-builder cases and 640 segment-metadata-block cases; both former independent per-uop assignments reject | `build/memblock/store-retirement-20261030/vector-frontend-identity-v1/`; sibling ROB/FTQ identity remains shared while instruction identities vary. Comparison with the former builder preserves non-frontend fields, queue cursors and PRNG state. Segment coverage is the actual FTQ assignment block, not its complete action/ports. Fresh non-author review accepts this bounded scope; its metadata probe passes 40960 instruction cases/315392 uops (`vector-frontend-review-v1/`). The isolated archived-HEAD-header suite also passes all three methods (`vector-frontend-isolated-v1/`). No native replay or CPU bug claim |
| Retained producer and scalar/vector pair issue | Eight finite native cases pass 2048 actions: default `spec`/`corner` and forced legacy `spec`/`corner`, two seeds each; all four full finite artifact verifiers pass. Forced `spec` cases observe three same-edge pairs each; forced corner observes zero/three. One additional 256-action `spec` replay has an identical terminal summary and passes its full verifier. Seven new retained-driver host methods check 9244 offers, 49921 age/redirect cases, timeout/retention boundaries, actual tick sampling and three private driver faults. | `build/memblock/ordinary-issue-20261030/retained-driver-v1/qualification.json`, `runtime/runtime.json`, `native-verification.json`, `retained-final.log`; fresh non-author review adds repeated retry, failure/exception cleanup and all-slot redirect challenges. Synthetic transport cases do not qualify store authorization, full vector legality or native redirect stages. Ordinary sliding simultaneous issue and admission plus issue remain gaps. Development qualification only. |
| Ordinary retained STA/STD pairing | Ten finite native cases pass 4096 actions and all six full finite artifact verifiers: two each default `spec`/`corner`, forced scalar pairing and separate-operand controls, plus exact split-pair replay and prior-runtime compatibility. Eight primary cases observe 393 same-edge pairs and one STD-first pair at one-edge skew; disabled controls observe zero. Seed 6621 replays its exact terminal summary. With pairing disabled, seed 6501 matches 399 semantic fields against the schema75 runtime, excluding schema/new pair fields. | `build/memblock/ordinary-issue-20261030/store-pair-v1/qualification.json`, `native-verification.json`, `replay-comparison.json`, `compat-comparison.json`. Three compiled helper methods include 256 schedules with real StoreScoreboard, same-edge STD and four failure controls; two accounting methods check 768 current/disabled/old-schema histories. The independent scheduler pair ledger compares all five outcome/skew bins; rate endpoints and three private direction/skew faults are checked. Canceled-store marginals provide necessary population bounds without joint-owner reconstruction. General heterogeneous issue, admission during stalls and native STA-first pairs remain gaps. Development qualification only. |
| Constrained-random structural uniqueness | The current pinned ordinary scheduler produces 64 distinct schedules from 64 fixed-eight-load seeds after removing addresses, payloads, delays and dependency-selection events; reductions retain 52 admission/retirement interleavings and 45 load issue orders. The executable test independently requires event-only diversity and both reductions. All 35 scheduler methods pass, including exact same-seed replay, oracle rollback, independent pair counters and private scheduling-fault sensitivity. | `build/memblock/ordinary-issue-20261030/redirect-tail-v1/structure-final-summary.json` records reductions and equality of all 22 current production hashes; `structure-final.json` and `full-unit-final.log` retain the full scheduler sample. Fresh independent review reproduces 64/52/45. This proves variation inside the implemented grammar, not broad full-CPU joint coverage. Blocking issue helpers, ordinary-stream same-cycle heterogeneous issue, adjacent-return/EXU arbitration and ordinary U-mode/context continuity remain explicit gaps. |
| Ordinary mixed redirect propagation and generation accounting | Frozen runtime-v2 passes six finite native cases / 5120 actions and all four full finite artifact verifiers: two each default `spec`/`corner`, seed 6271 with pairing disabled, and its exact replay. Both 2048-action cancellation cases finish at cycle 821593 with one redirect canceling six generations (four vector loads, one scalar store, one prefetch), 14 admitted/nine accepted vector-load uops, one unflushed VSTART and two unconsumed vector source-chain edges. Their entire terminal summaries also match the prior schema76 runtime. All 2437 final-word checks pass. Eight host methods exercise 168 exact terminal scripts, 30 corrupt/duplicate tails, five unknown late terminals, eight invalid boundaries, seven input lifecycles and nine propagation-feedback states; five author and four independent UT faults reject. | `build/memblock/ordinary-issue-20261030/redirect-tail-v1/qualification.json`, `runtime-v2/runtime.json`, `replay-comparison.json` and `review-disposition.json` retain commands, source comparisons, reviews and excluded preliminary attempts. Manager cancellation and pending-terminal erasure require the exact completed return; authorized stores and survivors remain live. Scripted edge/class tests qualify the checker, not native occurrence of every stage cross. Unmatched STD pulses, adjacent redirects, EXU/ROB arbitration, special-operation tails and full CPU reachability remain gaps. Development qualification only; no RTL bug or closure claim. |
| Redirect cancellation and replay host oracles | Eight replay-controller methods and six bulk-cancellation methods pass; randomized public-edge histories, ROB age/wrap and both redirect levels are checked, with ten controller and six scoreboard private faults detected. | `build/memblock/store-retirement-20261030/memory-replay-controller-v1/verification.json` and `build/memblock/store-retirement-20261030/mixed-scoreboard-cancel-v1/verification.json`; these are independent host contracts and do not certify native DUT timing or full-CPU recovery. |
| Ordinary store retirement separation | Nine store-retirement methods and two release-input methods pass. Architectural scalar/vector retirement is accounted separately from physical SQ drain, including pending committed records, vector-uop drain targets, nonzero-VSTART flush accounting and independent ReleaseData histories. | `build/memblock/store-retirement-20261030/store-retirement-*` and `release-cancel-checkpoint/`; the rebuilt harness passed 32 scheduler methods, while the corner replay above remains outside this bounded retirement qualification. |
| Retained issue ownership and STD wrap | Six ownership methods pass: 20 bound-owner rejections, 10 unbound controls/11 accepted edges, all 320 ROB value/wrap identities and four wrapped redirect cases. Ten author private faults and two additional reviewer faults reject. Four finite native cases pass 1024 actions and both full artifact verifiers; entire terminal summaries match the preceding runtime | `build/memblock/ordinary-async-20261030/ownership-v1/qualification.json`, `reviewer/review-v2.md`; guards precede pin/edge/replay mutation and same-edge STD completion remains checked. The retirement fixture integration failure was corrected and all 17 methods independently rerun. Native compatibility does not qualify new scheduler overlap, unmatched STD pulses or closure |
| Infrastructure unit | 610 tests pass; generated-interface, RTL and documentation checks pass | `build/memblock/ordinary-async-20261030/alias-refetch-v1/full-unit-v2.log`; final reviewed source includes corrected next-head timing, strict alias recovery and seven private helper faults. Its earlier `full-unit.log` predates the next-head fix and is excluded as final qualification. Unit checks do not establish DUT functional coverage |
| Admission/ledger host contracts | 20 independent methods pass: 13 production C++ schedules, 20 compiled private faults detected, 50 ledger records; non-author review independently enumerates 4803 arithmetic cases | `build/memblock/enqueue-admission-20261030/independent-tests/{summary.txt,evidence.json,ledger-evidence.json}`, `review-v1/review.md`; exact credit/redirect boundaries, repeated highs, both watermarks, unmonitored ticks, immediate conservation, holey batches, cadence and schema 2 compatibility. Synthetic public ports/edges; no DUT mutation claim |
| Ordinary enqueue/feedback DUT qualification | Frozen runtime-v1: 12/12 finite cases pass 22528 actions; six current-runtime artifact verifiers and one prior-monitor2 control pass; 9/9 adjacent directed scenarios pass | `build/memblock/enqueue-admission-20261030/qualification-v1.json`, `artifact-verifiers-v1.json`, `directed-v1/results.json`; spec/coverage/corner defaults, ordinary dependency streams, integrated recovery and store-only 641-instruction epochs. Whole-run cadence: 51092 enqueue edges, 8969 adjacent edges with older work, maximum run 19; this includes legacy/final readback and has no per-stream attribution. Stream-specific feedback overlap includes 315 admissions, 14 loads, 227 STA, 182 STD, 35 vector uops and 7 prefetches. 5478 ordinary hits, 715 misses/retries, maximum pending 2; 12303 stream retirements, 2748 full ROB identity reuses and 9929/9929 final words. Exact corner replays pass at 333.5/336.6s with 900s budget; initial 180s timeouts and malformed-constraint attempts are excluded. Functional/accounting development evidence, not closure |
| Store feedback structural and sensitivity checks | 29 scheduler methods and 16 independent replay/accounting methods pass; fresh focused 64/64 seeds produce distinct structural schedules and seed 1 replays exactly; 44 serialization round trips and 15 private replay/accounting faults remain detected | `build/memblock/enqueue-admission-20261030/scheduler-focused-v1.json`, `unit-v2.log`; focused schedules exclude addresses/payload/cycles/delays. Replay-state checks and retained serializer/fault evidence are at `build/memblock/store-feedback-20261030/independent-tests/summary-v2.json`. Synthetic host timing does not prove full CPU reachability; these are UT checks, not RTL mutation coverage |
| Store feedback independent review | Fresh non-author actual-diff review accepts the bounded change with no open blocking findings | `build/memblock/store-feedback-20261030/review-v1/review.md`, `review-disposition.json`; transient pending-peak undercount fixed at acceptance edges and guarded by independent ledger corruption tests, then rebuilt and requalified. Reviewed live-store identity and unchanged independent byte checks. No CPU bug or universal stale-generation claim |
| Sustained stream DUT qualification | Frozen runtime-v1: 18/18 finite current-runtime cases pass 59408 actions; all ten artifact verifiers pass, including two previous-runtime controls | `build/memblock/sustained-stream-20261029/qualification-v1.json`; spec/coverage/corner defaults, forced 641, cold recovery, exact 4096 mixed/load-only and short compatibility. 47 epochs exceed 32, maximum 4096, 13142 accepted-overlap admissions after ordinal 32 and 21247 full ROB identity reuses; 32055 stream retirements, 21244 address/3117 data edges consumed exactly once, maximum depth 164, 36 held/canceled/drained cold roles and 14098/14098 final words. The two 8192-action cap runs clip to 4095; 8200-action controls exercise exact 4096. Functional/accounting development qualification only |
| Sustained host and compatibility contracts | 28 compiled scheduler methods pass, including 19 sustained histories up to 4096 and exact long replay; ten independent history/accounting methods pass 167 serialized rows and 173 accepted/148 rejected accounting cases | `build/memblock/sustained-stream-20261029/scheduler-tests/report-v4.json`, `accounting-tests/report.json`; actual parser accepts 7/rejects 19 boundary configurations. Old schema 71 retains 32/depth 31 behavior; two min 8/max 32 DUT cases match 395 semantic fields each against the preceding runtime. Fixtures erase exactly one expectation/dispatch per independently complete terminal uop before identity reuse. Synthetic host timing/ports and sampled replay compatibility do not prove full CPU reachability |
| Sustained stream independent review | Fresh non-author actual-diff review accepts the bounded development slice with no open findings | `build/memblock/sustained-stream-20261029/review-v2/independent-review.md`; observed-epoch dependency-depth bound and stale length documentation findings resolved and re-reviewed. Reviewer independently reruns ten history tests and all ten artifact verifiers, reconciles all 18 current cases and both 395-field compatibility comparisons. Repeated-identity, replay-generation and scalar STD attribution limits remain explicit; no closure claim |
| Producer-dependency DUT qualification | Frozen runtime-v1: 22/22 finite DUT cases pass 19456 actions; eleven artifact verifiers accept including two previous-runtime controls | `build/memblock/dependency-stream-20261029/qualification-v1.json`; forced spec/coverage/corner dependencies, default profiles, integrated cold recovery, pure load chains, disabled and all-FP controls. 3729 address and 622 data edges each consumed once, 3043 pending sources selected, maximum depth 20, 2535 consumptions before and 1816 after producer retirement, 9110 retired stream instructions, 5247 accepted-overlap admissions and 8763/8763 final words. Functional/accounting development evidence only; no strict coverage or closure |
| Producer-dependency structural and accounting checks | 27 compiled scheduler methods pass; fixed eight-integer-load samples produce 55 distinct edge graphs in 64 seeds, excluding addresses/payload/timing. Private early-release, wrong-ALU, wrong-source and wrong-data mutations reject. Nine independent accounting methods pass 284 accepted/66 rejected records and 281 serialized rows | `build/memblock/dependency-stream-20261029/scheduler-tests/report-v3.json`, `accounting-tests/evidence-v2.json`; same-seed replay, disabled/FP-only controls, depth0..31, five consumer classes and producer-lifetime checking. Two disabled-rate DUT cases match 393 semantic fields each against the previous runtime. Host mutations are not RTL mutation evidence |
| Producer-dependency independent review | Fresh non-author actual-diff review accepts the bounded development slice with no open findings | `build/memblock/dependency-stream-20261029/review-v2/independent-review.md`; source eligibility/depth accounting, direct producer/consumer edge uniqueness, bound-source arithmetic sensitivity, page-clamp contract and preset-table findings resolved. Reviewer reruns eleven artifact verifiers, nine accounting tests, all64 fixed-eight-load cases and selected exact replays. No closure, CPU timing or RTL mutation claim |
| Stream operation-subset qualification | Frozen runtime-v1: 34/34 finite DUT cases pass 10896 actions; all fourteen artifact verifiers accept | `build/memblock/stream-profiles-20261029/qualification-v2.json`; twenty-two newly legal subset cases, default spec/coverage/corner, legacy and serialized controls. 4657 retired stream instructions, 2599 accepted-overlap admissions, 3471 continued admissions, 1447 FP checks and 6344/6344 final words. Load-only indexed e8/LMUL4 instructions exercise 64-flow admission; a serialized 128-flow control completes in queue-bounded uop windows. Four CLI controls reject empty/segment-only populations and oversized stream directions before traffic. The 64-flow profiles have one live instruction at a time; capacity evidence does not prove inter-instruction overlap. Functional/accounting development qualification, not strict coverage or closure |
| Stream endpoint and accounting contracts | Twelve independent host tests pass 344 validation and 224 accounting cases; production executor tests pass 248 cases over all 31 nonempty ordinary producer masks | `build/memblock/stream-profiles-20261029/constraint-tests/evidence.json`, `scheduler-tests/report-v2.json`; actual parser/validator, literal 32/64/128-flow instructions, executor boundaries, fitting-plus-oversized shape unions, disabled directions, malformed inputs and schema 69 compatibility. Synthetic executor checks retain distinct destinations, exactly-once issue/retirement and input-byte expectations; no full-CPU timing claim |
| Default-profile compatibility | Eight default spec/coverage/corner cases match 393 parsed semantic fields per seed against the preceding frozen runtime | `build/memblock/stream-profiles-20261029/default-compatibility-v1.json`; schema/controller metadata excluded. This supports unchanged seeded behavior for sampled cases, not all seeds or profiles |
| Ordinary sliding mixed DUT qualification | Frozen runtime-v1: 22/22 accepted finite seeds pass 11776 actions; all seven retained artifact verifiers accept; 8/8 adjacent directed cases pass | `build/memblock/ordinary-scheduling-20261029/qualification-v3.json`; unmodified short spec/coverage/corner plus stream-only and legacy controls. 138 epochs, 2584 retired stream instructions, 1569 accepted-overlap admissions, 1988 continued admissions, 809 replay issues, 959/1406 admission/producer choices with a completed head, 61 legacy windows and 5394/5394 final words. Development qualification only. Two CMO-stress controls lack terminal acceptance at 240s; previous-runtime seed 1 also times out with progress. Those controls remain excluded |
| Ordinary stream mechanisms | Fourteen compiled scheduler tests pass: 384 executor runs and 288 execute-budget runs; fixed eight-load/eight-producer population has 64 distinct schedules in 64 seeds; 12 exact seed replays; three private scheduling faults detected | `build/memblock/ordinary-scheduling-20261029/scheduler-tests/report-v2.json`; signatures omit address/payload/cycle/delay. Synthetic ports/completion times, Bare translation and representative vector shapes. Six byte-overlay and eight schema-accounting tests pass; independent review additionally rejects premature retirement |
| Retained-prefix stream recovery DUT | Frozen runtime-v2: 42/42 finite seed cases pass 16384 actions; ten artifact verifiers accept; 10/10 adjacent native directed cases pass | `build/memblock/unfinished-stream-20261029/qualification-v2.json`; forced/conflict/FP, disjoint, cold/parent-disabled, indexed prefixes and unmodified spec/coverage/corner. 385 episodes, 423 returned rounds, 671 strict suffix refetches and 164 observed LQ cancellations. All 109 held cold roles cancel explicitly and drain old responses, then strictly refetch 174 times; 43 cold roles are FP. Old-response phases retain one prefix producer selection and 25 prefix retirements. All 8336 tracked final words are observed. No fresh-line shortage sampled in DUT; unmodified short spec samples no cold role. Development qualification only |
| Recovery executor and ledger | 22 scheduler tests pass, including 192 completed-recovery executor cases and 128 cold-enabled cases with 55 actual cold roles; independent scalar byte/state checks, strict cancellation and Grant/quiet boundaries | `build/memblock/unfinished-stream-20261029/fixture-tests/report-v6.json`; same eight-load population has 64 structural schedules in 64 seeds, excluding address/payload/cycle/delay. Four cold faults, a line-base-only freshness fault and a missed-successor-history fault reject. Narrow prior-A, explicit-byte shortage, disjoint/disabled and seed replay controls pass. Synthetic timing/ports, stubbed replay, Bare translation; no full-CPU timing or RTL mutation claim. Shared ledger retains isolated terminal-readback gating |
| Recovery sparse accounting and lifecycle | 34 stream accounting tests pass; schema69 adds 6408 explicit cold execution histories and 160 mixed aggregates; eight manager lifecycle cases pass and nine private query faults reject | `build/memblock/unfinished-stream-20261029/accounting-tests/`, `line-grants/query-mutation-results.json`; initial strict completions plus canceled cold roles equal initial loads, and invalidated completed executions plus cold cancellations equal strict refetches. A selected first round requires a completed warm victim. Class/shape/suffix, disabled/schema68 and uint64 checks remain sparse necessary bounds, not full joint-history reconstruction. Per-line Acquire/Grant readiness includes pending acquires, holds and both D/E completion orders while unrelated lines stay active |
| Integrated recovery oracle sensitivity | Two private C++ faults linked to the unchanged frozen DUT model reject; matching golden seed1 passes | `build/memblock/unfinished-stream-20261029/dut-oracle-faults-v2/verification.json`; wrong cold-refetch data fails the strict scalar comparison and omitted initial cancellation fails the surviving-prefix obligation. Private UT/controller sensitivity, not RTL mutation or CPU bug evidence |
| Scalar dependency interleaving | Frozen runtime-v1: 10/10 finite seeds pass 4608 actions; all artifact verifiers accept | `build/memblock/dependency-interleave-20261029/qualification-v1.json`; focused/translated mixed/spec/corner. 759 dependency windows, 546 accepted early loads, 269 with overlapping older STD unissued, 35 with multiple, 146 heterogeneous windows and 5465/5465 final tracked words. Known older addresses; no arbitrary unresolved-address or closure claim |
| Dependency scheduler mechanics | Four compiled generator/oracle/accounting tests pass; private checks detect store-then-load, missing-STA-prerequisite, lost-event and duplicate-event mutants | `build/memblock/dependency-interleave-20261029/method-tests/verification.json`; actual generator and byte overlay, independent event invariants and sparse schema-61 accounting. Synthetic opportunity counts do not establish DUT timing |
| Dispatch/retirement helpers | 17 compiled production-method tests pass, including five stream head/query/replay cases; prior independent helper review detects ten private mutations | `build/memblock/retirement-contract-20261029/review-v2/report.md`; real scoreboards/queue accounting with synthetic scheduling/ports. No full-CPU reachability or RTL-mutation claim |
| Dispatch/retirement DUT qualification | Frozen runtime-v2: 14/14 finite seeds pass 12416 actions; artifact verifiers accept; 9/9 selected directed scenarios pass | `build/memblock/retirement-contract-20261029/qualification-v2.json`; translated indexed mixed/special, dirty pressure, spec/corner/CMO compatibility. 130 heterogeneous windows and 7512/7512 final tracked words. Development qualification, not closure |
| Vector operands and oracle | Frozen runtime-v2: 18/18 finite seeds pass 5888 actions; all retained artifact verifiers accept | `build/memblock/vector-operands-20261029/qualification.json`; ordered/unordered indexed and agnostic controls, coverage/spec/corner, mixed and legal ordinary/segment FOF profiles. 1804 load source-chain edges and 1804 checked predecessor waits, 1450 store-source reuses, 146 heterogeneous windows and 10563/10563 final tracked words; development qualification, no closure claim |
| Vector builder and mutation checks | Six production builder/source/scheduler/wait/scoreboard/accounting tests pass; 4992 builder cases, 192 no-op cases and 453/512 distinct dependency issue orders | Fresh non-author review detects eight private input/scheduler/replay mutations plus removal of the no-op comparator gate; 128 additional normal-body controls preserve active/mask/tail decisions. Helper/unit sensitivity, not RTL mutation coverage |
| Vector adjacent directed checks | 13/13 selected scenarios pass through runtime-v2 | `build/memblock/vector-operands-20261029/directed-v2/results.json`; full vector-segment matrix (676 unit/strided and 676 indexed configurations), ordinary/split/FOF/addressing/issue/forwarding plus translation/faults, scalar/FP and coherence; selected set, not full catalog |
| Canonical randomized FP | Frozen runtime-v1: 30/30 finite random/control seeds pass 6656 actions; all retained artifact verifiers accept | `build/memblock/fp-random-20261029/qualification.json`; focused ordinary/dependency/unresolved, coverage/spec/corner, mixed, integer-only, single-width and aligned injected-error controls. 2023 FP checker executions (649 FLH, 700 FLW, 674 FLD), 146 heterogeneous windows, 2281/2281 final tracked words; development qualification, no closure claim |
| FP oracle and structural sensitivity | Six production helper/factory/scoreboard/accounting tests pass; 509/512 unique FP dependency topologies | Literal narrow/64-bit payloads, discontinuous pages, older-store/refetch versions and destination domains; fresh non-author review detects private no-boxing, wrong-domain and wrong-class mutations. These are helper/unit mutations, not RTL mutation coverage |
| FP adjacent directed checks | 13/13 selected scenarios pass through runtime-v1 | `build/memblock/fp-random-20261029/directed/results.json`; scalar/FP, misalignment, forwarding/recovery, translation/alias/faults, atomic and coherence; selected set, not full catalog. Exceptional misaligned FP enable semantics remain unqualified |
| Ordinary nonidentity maps | Runtime-v3: 15/15 seeds pass 6403 actions; 4408 mapped samples across all six contexts/four kinds, 1138/1138 automatic recalls, 74 cross-page uops, 4182/4182 tracked final words | `build/memblock/alias-manager-20261029/{qualification-v3.json,runtime-v3}`; diverse, coverage/spec/corner, identity controls, mixed and one-store diagnostic; finite artifact verifiers accept. Static maps and drained context transitions only; zero runtime intra-element splits; no closure claim |
| Nonidentity oracle mechanics | Six map/oracle tests plus two production-manager tests pass; 128 seeded topologies/768 context maps; 24 manager lifecycle/version cases and 32 independently enumerated complete-line candidates | Wrong-page/contiguous-PA substitutions, old dirty alias data, D/E ordering, release races and torn/reordered/late versions are checked. Fresh review independently checks 5408 images across 96 update histories; uncheckpointed fallback and long histories remain unqualified |
| Nonidentity adjacent directed checks | 19/19 selected scenarios pass | Runtime-v3, native commands/logs at `build/memblock/alias-manager-20261029/directed-native-v3`; scalar/FP, forwarding/misalignment, full vector/segment, translation/alias/faults, CBO.ZERO, atomic, coherence/release and CMO; selected set, not full catalog |
| Mixed development validation | 2/2 coverage seeds 731..732 pass at 4608 actions; artifact verifier accepts | Frozen pre-PMP runtime; 33/32 heterogeneous windows and 2338/2288 final observed words; `build/memblock/report-namespace-20261028/random`; no closure claim |
| Unresolved recovery DUT | Frozen runtime-v3: 19/19 finite seeds pass 11936 actions; all ten artifact verifiers accept | `build/memblock/unfinished-recovery-20261029/qualification-v3.json`; 1838 windows, 4581 older stores, 4957 initial roles and 2084 redirects, including 637 multi-round windows. 762 accepted held cold loads yield 762 observed LQ cancellations, explicit pending-expectation cancellations and old-response drains, then 1477 strict cold refetches; 380 cold roles are FP. Total strict suffix executions are 5201, including 2841 unchanged input versions. Victims move older/equal/younger 134/770/115 times. All 6197 tracked final words are observed. Integer/FP, disjoint/conflict, cold-disabled/parent-disabled, translated, mixed and default spec; no unavailable fresh-line attempts sampled, no closure claim |
| Recovery mechanics and accounting | Eleven production cancellation/planner/reporter tests pass; 9600 ROB-age/wrap cases and 8888 independently derived unfinished byte histories | `unit/test_unfinished_load_cancellation.py` and `unit/test_unfinished_recovery_accounting.py`; legal redirect-propagation completions, stale/zero-budget/duplicate/wrong-identity cancellation and late-writeback rejection; 1024 unfinished plans yield 568 structural schedules excluding payload/address/delay. Old all-completed planner/epoch tests remain in the unit suite; sparse/unavailable populations and schema-65 compatibility are checked. Helper/report evidence, not full DUT schedules or full-CPU timing |
| Unfinished oracle sensitivity | Two private C++ faults linked to the unchanged DUT model reject; matching golden seed2101 passes | `build/memblock/unfinished-recovery-20261029/dut-oracle-faults-v3/verification.json`; wrong cold-refetch data fails the strict scalar comparison, and omitted initial cancellation fails the retained pending-obligation check. Private UT/controller sensitivity, not RTL mutations or CPU bug evidence |
| Random indexed operands | 4/4 focused seeds 861..864 pass at 128 actions each; repeat endpoints 0/1000 pass 2 seeds each | Frozen runtime `build/memblock/vector-indices-20261029/runtime`; ordinary/segment directions, repeats and descending offsets observed. All tracked final words read back; finite accounting verifiers accept. Overlapping scatter/device ordering remain excluded |
| Indexed mixed development | Custom mixed seed897 passes 1024 actions with 46 heterogeneous windows and 1313/1313 final words | Artifact `build/memblock/vector-indices-20261029/mixed`; 208 indexed instructions, 327 generated repeated offsets, 1172 descents; these operand counts include inactive elements |
| Indexed broad development | Seed891 passes 4608 coverage actions; finite artifact verifier accepts | `build/memblock/vector-indices-20261029/coverage-replay`; 32 heterogeneous windows, 113 dependency windows, 488 indexed instructions and 2325/2325 final words. Exact runtime/seed replay completes in 687 seconds after a 300-second budget timeout; no closure claim |
| Indexed profile compatibility | 2/2 spec and 2/2 corner seeds pass at 512 actions with compact latency; seed892 passes 512 with default spec timing | Artifacts `build/memblock/vector-indices-20261029/{spec,corner,spec-timing}`; seeds871/872 have zero indexed instructions and establish compatibility only. Seed892 executes one indexed load/store pair; corner seeds exercise both ordinary/segment directions. Strict coverage rejects unrelated sparse prefetch/probe bins; no closure claim |
| Refetch/forwarding regression | Current unfinished-recovery runtime-v3: 10/10 adjacent directed cases pass | `build/memblock/unfinished-recovery-20261029/{directed-v3,alias-v3}`; alias round trip, memory violation, forwarding, FP, store-TLB miss, scalar misalignment, redirect, reset recovery/partial load and DCache release. Selected contracts, not full catalog or general arbitration |
| Campaign report isolation | Concurrent two-regression/one-benchmark fixture and aggregate rerun pass | Fresh run-ID directory and matching document ID; previous sidecar bytes retained; artifact `build/memblock/report-namespace-20261028/check-unit.log` |
| check-rtl | Pass | Inventory/schema/generated-file consistency, not full prose or RTL-source provenance |
| Restored-original directed | 90/90 pass | Every catalogued directed test through the corrected harness and runtime from RTL `43b971f2`; artifact `build/memblock/verify-restored-rtl-20261028/directed` |
| Restored-original random | 4/4 seeds pass, 4608 actions each | `coverage` profile; artifact `build/memblock/verify-restored-rtl-20261028/random-4x4608`; development validation, not closure |
| Latest-RTL directed | 90/90 pass | `origin/kunminghu-v2` `a4eeaabb4` overlaid on the current branch, regenerated and rebuilt; artifact `build/memblock/verify-kunminghu-v2-a4eeaabb4/final2-directed` |
| Latest-RTL random | 1/1 seed passes, 4608 actions | `coverage` profile; artifact `build/memblock/verify-kunminghu-v2-a4eeaabb4/final2-random-1x4608`; development validation, not closure |
| Strict coverage audit | 2/2 seeds functionally pass, but strict coverage rejects the campaign | 2×4608 coverage, seeds 1..2; artifact `build/memblock/strict-coverage-results.json`; enabled probe-depth classes 4..7 were unobserved (`actual_probe_depth=[4,1,1,1,0,0,0,0]`); this is a sparse-coverage finding, not a functional failure |
| Strict coverage 16384 audit | 1/1 seed functionally passes at 16384 actions, but strict coverage still rejects | Seed 1; artifact `build/memblock/agent-campaign-20/strict-16384.json`; only 6 standalone probe sequences occurred (`actual_probe_depth=[2,2,0,0,0,0,0,2]`), so increasing action count alone does not select the enabled depth classes; a probe-weight override is the focused lever |
| Probe-heavy strict audit | 1/1 seed functionally passes at 4608 actions with `probe=500`; all eight probe-depth classes occur, but strict coverage still rejects | Artifact `build/memblock/agent-campaign-21/probe-strict-4608.json`; `actual_probe_sequences=150`, `actual_probe_depth=[82,33,6,4,6,11,4,4]`; strict now fails on enabled `actual_probe_cross` classes with zero observations, so the next lever is more probe sequences or a focused cross distribution |
| Balanced strict audit | 1/1 seed functionally passes at 4608 actions with `probe=1000 canonical-boundary=500`; vector canonical-boundary class 0 is now covered, but probe-cross classes re-sparsify | Artifact `build/memblock/agent-campaign-23/balanced-4608.json`; `actual_probe_sequences=168`; strict rejects enabled probe-cross bins at indices 13,14,18,21; demonstrates a distribution trade-off rather than a depth-only gap |
| Probe=1000 strict audit | 1/1 seed functionally passes at 4608 actions; all probe depth and probe-cross classes occur, but strict coverage rejects a vector canonical-boundary cross | Artifact `build/memblock/agent-campaign-22/probe1000-4608.json`; `actual_probe_sequences=317`, `actual_probe_depth=[149,75,15,15,17,18,14,14]`, all 32 probe-cross bins nonzero; the remaining gap is `actual_vector_canonical_boundary_cross` class (invalid_first=0, mode=0, addressing=1, eew=1). That behavior is already exercised by the directed `translation-faults` canonical-range vector-store cases (Sv39 strided eew=1 with a valid first byte); the strict rejection is a mixed-generator sparse-bin issue, not absent directed coverage |
| Deep probe window | `dcache-coherence` passes with 8 accepted B probes, 8 C responses and `deep_probe_depth=8` | Directed eight-line window; four resident clean lines report permission 2 and four absent lines report the DUT's no-permission encoding 5; the report values are scenario-owned, not an architectural permission oracle |
| Same-address B/C attribution | B presentation to a PA waits for previous final C, final D and E; accepted C is matched to that one eligible PA expectation | Client C sources are independent of B sources and fixed across data beats; manager fixture and selected coherence/release/atomic/CMO DUT checks pass |
| Accepted-B probe gate | 1/1 1024-action mixed seed passes with 407 probes, plus directed coherence/release/atomic | Seed 207; artifact `agent-campaign-17`; no dedicated counter for pre-B C because an illegal C fails immediately |
| CBO.ZERO ordering | Corrected sequence passes on both restored-original and latest-overlay RTL with `sequential_zero=2 consecutive_zero=2 following_store=1`, repeated same-line and three-line consecutive zero cases, both zero/store age directions, a dirty-line zero and an early younger load | CBO.ZERO remains the ROB head until deferred SBuffer completion; no simultaneous CBO completion window is legal or claimed |
| Segment noncanonical fault | Corrected legal two-uop `nf=1` segment load/store cases pass on both restored-original and latest-overlay RTL; the later uop exposes the exact page fault with no DCache/Uncache request or SBuffer side effect | The first data uop may defer the fault and is not an independent architectural oracle |
| Final word readback | observed words equal registered words in every passing mixed case | Cacheable identity path; aliases and CBO.INVAL/errored CMO lines excluded; successful CLEAN/FLUSH retained |
| High-mask driver mutation | Zeroing `src(3)` bytes 2..15 makes `vector-addressing` fail at `phase=high-mask-load`; the restored driver passes | Harness mutation only; it demonstrates directed-test sensitivity, not RTL mutation coverage |
| Vector fault vstart | Two-uop e16/LMUL2 page fault passes with `vstart=8` and exact fault VA. Two single-uop page-crossing faults pass with exact `vstart=1` and exact faulting-element addresses; the all-masked unmapped control passes and observes zero PTW and zero DCache requests; the partially masked load with an inactive unmapped element completes on the mapped element; `translation-faults` PASS log at `agent-campaign-18/translation-faults.log` | Broader fault-element/FOF progress remains open |
| Vector segment store fault side effect | `translation-faults` segment-store cases pass: field-1 fault with exact `vstart=0` and unchanged read-only page; element-1 fault with exact `vstart=1`, exact element-1 field-0 address, and unchanged read-only element page; first-field fault with `vstart=0` and both the read-only field-0 page and following writable field-1 page unchanged; writable earlier fields may be partially written | Three `nf=1` unit-stride segment shapes; other field/element combinations remain open |
| Vector PMP load fault | Single-element and masked no-access-page cases check the fault metadata and no requests. The page-crossing load requires exact element-1 access fault/VA/`vstart=1`; accepted request history through redirect recovery allows only the readable prefix line plus independent prefetch, with no uncache request. Its cold both-readable control checks both data elements and second-page demand. | Bare direct-map; wider shapes and concurrent PMP reconfiguration remain open |
| Vector PMP store fault side effect | `translation-faults` PMP cases pass: a vector store to a PMP read-only page raises store access fault at the element address, issues no DCache/uncache request, and the page reads back unchanged; the masked variant activates only element 1, faults at element 1 with `vstart=1`, and both element addresses read back unchanged; the cross-page variant faults at element 1 when element 0 is in a read/write page and element 1 in a read-only page, with the read-only page unchanged; the segment variant faults at field 1 with `vstart=0`, no request and the read-only field page unchanged; the first-field segment variant faults at field 0 and leaves the following writable field page unchanged; EEW 1 and EEW 2 single-element stores also fault with no request and unchanged page | Bare direct-map; wider segment field/element PMP shapes remain open |
| Vector store fault side effect | `translation-faults` unmasked vector-store case passes with exact `vstart=1` and fault address; its read-only target page word is unchanged. The masked variant activates only element 1 and leaves both the inactive writable page and the read-only page unchanged. The first-element variant passes with exact `vstart=0` and both pages unchanged | Writable first-element visibility and other fault-path targets remain partial |
| Permission-fault store side effects | Frozen runtime-recovery-v4 `translation-permissions` passes 194 cases: 16 stage1 load, 17 stage1 store, 75 two-stage load, 86 two-stage store, 84 implicit G permission faults, and 10/10 fault-store readbacks; `pmp-contracts` adds one PMP-denied store readback | These remain dedicated fresh-context permission cases; ordinary mixed streams still use permissive non-U mappings, and device fault paths need another observation route. Evidence: `build/memblock/store-retirement-20261030/translation-permissions-v4.log` |
| Non-cacheable final effects | committed NC store/atomic words match the manager bus image in passing mixed runs; 2/2 4608-action seeds (210/211) check 141/142 words, seed 215 checks 146, and smaller seeds cover the uncache-drain path | Manager bus image, not a DUT uncache read; device side effects, aliases and CMO remain outside |
| Atomic dirty-line persistence | `atomic-contracts` passes; ProbeAckData carries the updated line and updates the manager image | AMO target only; cross-hart/concurrent eviction not covered |

Queue accounting is exact in the mixed results, the observed-word count equals
the registered count, and per-case report commands and summaries agree with the
aggregate results. Full commands/runtime identity are in the local result
artifacts. Execution of all catalog entries is not proof of all legal input
combinations or missing oracle coverage.

## Readiness

Readiness is partial. Functional PASS means active checks accepted generated
traffic, not all legal classes occurred or every external effect was checked.
Current missing rules include remaining aliased/device final-byte observation,
uniform fault forbidden-demand checks, arbitrary simultaneous synonym/version
handling and broader C lifecycle/reset combinations, wider same-line CBO.ZERO
forward-timing variants beyond the older-store/younger-zero/younger-load case,
and independent verifier schema/backpressure checks.
The accepted-B-before-C gate, full 128-bit mask operand and explicit
first-access cross-alias redirect/refetch recovery are implemented. General IQ/LQ
replay, concurrent refetch epochs and backend redirect arbitration remain open.
Boundary faults, concurrency and reset/protection crosses also remain partial. PBMT=NC `CBO.ZERO` was probed and the DUT reports a store access fault
before an uncache request; the architecture leaves CBO behavior for
non-cacheable memory unspecified, so neither behavior is claimed. Four-state/
physical arrays and full-core/multi-hart behavior need other verification
boundaries.

An opt-in strict coverage check on 2×4608 coverage seeds rejects the campaign
because enabled probe-depth classes 4..7 were not observed, even though both
seeds pass functionally and the default accounting verifier accepts them. This
confirms that
the strict gate is active and that 4608 actions is not a closure-scale coverage
budget. The current 4×4608 run is a development validation, not the configured
8×16384 regression. Configured 48-hour breadth and million-action endurance
targets are not completed. Their current budgets and automated-check limitations
are defined in VERIFICATION_PLAN.md. No closure claim follows from a short
capability run or a controller/verifier return code alone.

## Current Review

Fresh-context non-author review accepts the bounded alias producer/recovery
change with no open findings. The retired-head finding is resolved: all three
native store commits expose the next non-store head on the following edge,
before SQ/cache waits. The reviewer checks full-CPU producer timing, all 18
source hashes, 98 prepared source comparisons, five host methods, seven native
invocations, four mixed cases/1024 actions and exact prior-summary compatibility.
The final complete suite passes 610 tests. Evidence and primary disposition are
`build/memblock/ordinary-async-20261030/alias-refetch-v1/reviewer/review-v2.md`
and `review-disposition-v2.json` in its parent. Positive LQ cancellation and
correct-first-data redirects retain synthetic-only evidence; arbitrary replay,
redirect arbitration, full rename history and specific-store forwarding-stage
residence remain outside approval. No CPU RTL bug or closure is claimed.

Fresh-context non-author review accepts the bounded issue-ownership and STD-wrap
correction with no remaining findings. The retirement-fixture compilation
finding is resolved by including the production type and initializing the
unbound pointer; all 17 retirement methods independently pass. Six ownership
methods, ten author faults and two additional reviewer faults check production
helper code and synthetic pin witnesses. Four finite native cases preserve exact prior summaries
and pass frozen-runtime/accounting checks; the final full suite passes 605
tests. Evidence and disposition are under
`build/memblock/ordinary-async-20261030/ownership-v1/`.
The initial failed fixture suite and unrelated alias diagnostic are excluded
from passing qualification. Persistent scheduling, admission during stalls,
broader recovery and unmatched STD attribution remain open.

Fresh-context non-author review accepts the bounded ordinary cacheable redirect
return and stronger event-only randomness check, with no open blocking findings.
The manager's completed-boundary contract is preserved; exact terminal and
identity checks remain active during propagation, and actual stream ledger code
classifies canceled STA feedback after return. The reviewer reruns all eight host
methods and independently rejects four final-edge/cancellation/feedback faults.
It reproduces all three structural reductions and verifies all six current
native cases / 5120 actions, exact cancellation replay and prior-runtime terminal
compatibility. Evidence is
`build/memblock/ordinary-issue-20261030/redirect-tail-v1/reviewer/review-v2.md`,
with primary disposition in `review-disposition.json` in the parent directory.
The host edge scripts do not establish native occurrence of every tail stage;
unmatched STD pulses, full CPU timing and broader operation/producer overlap
remain outside approval. No CPU RTL bug or closure is claimed.

Fresh-context non-author review accepts ordinary scalar-store pairing after
correcting a medium coverage-test gap: the compiled scheduler now independently
compares all five pair outcome/skew counters with accepted-edge records. The
original escaped direction mutation fails in all 30 affected cases with JSON
output removed, so the checker itself supplies sensitivity. Endpoint profiles,
same-edge STD attribution, callback failure and native artifact accounting remain
checked. Evidence is `build/memblock/ordinary-issue-20261030/store-pair-v1/review-v2/reviewer/review-v2.md`;
`review-v3/reviewer/review-v3.md` separately accepts the final emitter-domain
check, and `review-disposition.json` records primary resolution in the same
`store-pair-v1` parent. Canceled-operand marginals cannot reconstruct joint
owner attribution; broad native split timing, heterogeneous issue, admission
during stalls and retained recovery tails remain open. No RTL bug or closure
claim follows.

Fresh-context non-author review accepts the retained producer transport and
migrated scalar/vector load pair with no blocking findings. It reviews the
actual diff and CPU final-register/redirect contract, reruns four host suites
and independently challenges repeated timeout/reoffer, edge-fourteen readiness,
delayed killed acceptance, failure/exception cleanup and all nine slot kinds.
Evidence is `build/memblock/ordinary-issue-20261030/retained-driver-v1/review/reviewer/review-v1.md`;
primary disposition and qualification are in that artifact's `retained-driver-v1`
parent. Ordinary sliding integration, store authorization, vector replacement,
redirect-during-off-pin-retry, binding startup after an earlier redirect and
reset during a live binding remain outside this verdict. Terminal checks skipped
by `tick(false)` remain skipped; driver events do not substitute for them.

Fresh-context non-author review accepts schema 75 generation-cancellation
accounting with no remaining findings. Primary disposition resolves canceled
vector-source attribution, phase-ineligible unconsumed operands, retry attribution,
admitted/retired epoch bounds, malformed retry metadata and stale test anchors.
The reviewer independently challenges aggregate rules and reruns all five finite
native cases. Evidence and primary disposition are
`build/memblock/ordinary-issue-20261030/cancellation-review-v3/reviewer/review-v3.md`
and `cancellation-review-disposition-v3.json` in that artifact parent. The native
redirect covers canceled vector loads, a scalar store and a prefetch; other
classes and cancellation phases retain host-only accounting evidence. Aggregate
inequalities are necessary conditions, not reconstruction of every joint history.
This acceptance does not establish full-CPU input reachability, late-output
coverage or closure.

Fresh-context non-author review accepts the cancellation extension as a
manager-only source/host capability. Independent probes check cross-line
cancellation and reuse during frozen C, retained progress floors and rejected
batch atomicity against archived committed headers. The main-agent isolated
nine-method suite also passes with only the manager changed across 95 archived
C++ files. Evidence is under
`WORKSPACE_ROOT/build/memblock/store-retirement-20261030/release-cancel-review-v1/`
and `build/memblock/store-retirement-20261030/release-cancel-isolated/`.
Native bounded mixed-suffix recovery uses the current schema-75 qualification above and the independent oracle rollback test. A simulator PASS without generation-cancellation conservation is insufficient. Adjacent returns, EXU/ROB arbitration, broader late-response histories and full CPU input reachability remain unqualified.

Fresh-context non-author review accepts the ordinary-input manager source and
host contracts for bounded development use. It independently compares 4,000
multi-release decisions, checks exact overrides and epoch anchors, and rejects
an independent stale-legacy example. The covered-legacy-history defect was
corrected and re-reviewed. This checkpoint also passes its isolated seven-method
suite using the pre-change public headers, without the pending mixed-driver
patch. Evidence is `build/memblock/store-retirement-20261030/review-v2/reviewer/`
and `manager-checkpoint-isolated/`. Native integration, the replay/cancellation
boundary and the baseline alias diagnostic remain outside this acceptance.

Fresh-context non-author review accepts the LSQ schema 3 admission/cadence
source, independent tests and contract documentation for bounded development
qualification. Two verifier false negatives were corrected and re-reviewed:
impossible longest-run undercounts and understated adjacent/older-work
intersections. The reviewer reruns the 20 admission/ledger methods and 17 retirement
methods, detects all 20 private compiled faults and independently enumerates 4803
arithmetic cases. Main integration passes all 533 host tests, `check-rtl`/`check-docs`
and all seven finite artifact verifiers. The qualification retains exact runtime,
commands, hashes, profiles and excluded attempts in
`build/memblock/enqueue-admission-20261030/qualification-v1.json`,
`provenance-v1.json`, `artifact-verifiers-v1.json` and
`review-v1/review.md`; primary disposition is `review-disposition.json`.
Whole-run enqueue cadence includes legacy/final-readback phases and does not
attribute simultaneous heterogeneous issue. Stream feedback counters separately
observe 315 admissions during another store's wait. Blocking producer handshakes,
architectural commit/SQ separation, synchronous unresolved operands and general
accepted-generation attribution remain priorities. The pressure helper bypasses
normal admission guards under its separate contract. No full-CPU timing,
RTL-mutation coverage or verification-closure claim follows from this evidence.

Fresh-context non-author review accepts schema 70 stream operation-subset
validation and accounting for bounded development qualification, with no blocking
findings. It independently passes the twelve host methods, 248 validated-profile
executor cases, all fourteen campaign artifact verifiers, eight default-profile
comparisons and prepared-source/frozen-runtime hash checks. Supplementary mixed
64-flow loads and scalar stores retain strict byte checks while reaching eight
live instructions and five accepted pending instructions; this is bounded UT
overlap, not an exact queue-occupancy or full-CPU timing claim. Empty/non-fitting
profiles still reject. The broad strict-coverage checker remains unqualified for
subsets. Evidence and primary disposition are
`build/memblock/stream-profiles-20261029/review-v1/independent-review.md` and
`review-disposition.json` in its parent. No CPU RTL bug or closure is claimed.

Fresh-context non-author review accepts schema69 retained-prefix unfinished-load
recovery for bounded development qualification, with no open blocking findings.
The medium temporal-witness finding is resolved: counted accepted-A history
restarts quiet time when a complete same-line successor occurs between idle
polls inside a multi-tick prefix helper. Independent narrow-Get freshness and
missed-successor faults reject. The reviewer verifies all 174 source snapshot
hashes, ten post-fix campaign artifacts and five frozen-runtime hashes, and
rechecks all 42 records/16384 actions, 109 cold cancellations/drains, 174 strict
cold refetches and 8336/8336 final words. It inspects 459 passing unit tests and
22 passing production-executor fixture tests. Report and primary disposition are
`build/memblock/unfinished-stream-20261029/review-v2/independent-review.md`
and `review-disposition.json` in its parent. Full CPU timing/predictor/rename
history, broader unfinished overlap, store-headed redirect adaptation and
universal stale-response exclusion remain unqualified. No closure or CPU RTL
bug is claimed.

Fresh-context non-author review accepts bounded unfinished-load cancellation,
late-response draining, strict suffix refetch and schema-66 accounting against
`review-v2.hashes.json`, with no open blocking findings. The reviewer independently
confirms that prior SBuffer stores must drain before classifying a fresh held
line. Eleven unfinished and 31 existing unresolved tests pass; 11520 copied
actual-executor windows exercise cold, all-completed, unavailable, format,
round-count and release-gap cases against an independent byte/state surrogate.
Six helper/oracle and eight executor faults reject, and all ten published v3
artifact verifiers pass independently. These are UT/helper sensitivity checks,
not RTL mutations. Main qualification passes 409 unit tests, nineteen finite
seeds/11936 actions and ten adjacent directed cases, including 762 observed
unfinished LQ cancellations and old-response drains, 1477 strict cold refetches
and 6197/6197 final words. Evidence is
`build/memblock/unfinished-recovery-20261029/review-v1/report.md`, with
`review-v2.hashes.json`, `review-disposition.json`, `qualification-v3.json`
and `provenance-v3.json` in its parent. Fresh-line shortages were covered only
by helpers/surrogate, with none sampled by these DUT cases. The D/E drain and
32-cycle quiet interval qualify this ordinary aligned cacheable witness; they
are not a universal pipeline-empty or arbitrary-late-result proof. Broader
unfinished/early-release overlap beyond the bounded integrated role, arbitrary
younger admission across redirects, dynamic
predictor/rename histories and full-CPU producer timing remain outside approval.

Fresh-context non-author review accepts the bounded ordinary stream's ready
retirement event, short-budget reservation cap and schema-67 observations against
`build/memblock/ordinary-scheduling-20261029/review-v3.hashes.json`. Its low-severity
accounting finding is resolved: at least one initial producer per epoch is
excluded from waiting-head issues, and singleton-only populations require zero
waiting choices. Independent enumeration accepts 32 legal cases and rejects 64
impossible records; a private premature-retirement mutation is rejected by the
independent completion checker. Review and primary disposition are
`review-v1/review-verdict.txt` and `review-disposition.json` in that artifact root.
The actual byte oracle and commit helpers retain their earlier bounded contract.
Full rename/source history, broader mixed-class sliding recovery, complete CPU timing
reachability and closure remain outside approval.

Fresh-context non-author review approves the ordinary scalar ready-event
interleaving and schema-61 accounting change with no findings. It checks the
CPU's independent paired STA/STD producers, matching-data replay eligibility,
held oldest ROB head and independent byte overlay. Four generator/accounting
and 55 verifier tests pass; an independent enumeration checks 24576 plans and
49108 loads, and ten private sampler/oracle/verifier mutations are detected.
The four read-only artifact verifiers accept ten DUT seeds and 4608 actions.
Evidence is `build/memblock/dependency-interleave-20261029/review-v1/report.md`,
with `review-v1a.hashes.json`, `review-disposition.json`, `qualification-v1.json`
and `provenance-v1.json` in the parent directory. This is development evidence;
full-CPU timing, arbitrary replay/rename histories and multiple speculative
unresolved addresses remain outside the claim.

Fresh-context non-author review approves the vector destination/source-chain,
dependency-preserving issue, exact predecessor-wait, replay attribution and
schema-60 accounting changes. It independently detects eight private mutations,
including removal of the production builder's chain construction. Its P2
finding exposed a raw no-op comparator false negative: tail-agnostic corruption
was accepted despite `vstart >= vl`. The corrected decision requires all old
bytes unchanged. Re-review passes six vector operand and twelve scoreboard
tests, detects removal of the new gates, and checks 128 ordinary-body controls.
The finding is resolved in
`build/memblock/vector-operands-20261029/review-disposition.json`; verdict and
source snapshot are `review-v3.txt` and `review-v3.hashes.json` in that directory.
No CPU RTL bug is confirmed.

Fresh-context review also approves canonical randomized FP formats within the
four implemented builders and the checkpointed static-map PA/alias manager.
Their scoped evidence and dispositions remain
`build/memblock/fp-random-20261029/review-disposition.json` and
`build/memblock/alias-manager-20261029/review-disposition.json`.

The generator varies dependency graphs, vector shapes, index operands, page maps
and dependency-preserving indexed issue orders. Ordinary stream epochs now also
vary producer counts/ages and ready-event schedules, with sliding admission and
random placement among serial helpers. The separate unresolved graph supports
multiple older stores/loads, surviving load prefixes and whole-suffix recovery,
including one youngest unfinished cold role with observed cancellation and old
response drain. Integrated recovery now retains the older mixed prefix with a
youngest cold role and per-line lifecycle checks. Broader unfinished overlap,
arbitrary younger admission across redirects, epoch drains and broader address dependencies
remain normal-workload priorities.
Builder samples do not establish unique complete-run DUT schedules or guarantee
a design bug.

Fresh-context non-author review approves the dispatch/retirement prerequisite
patch against the CPU ROB, LSQ prefix, SQ vector-commit and completion-driven
LQ rules. All twelve production-method tests pass, and ten private extracted-
fixture mutations fail, covering global waits, ROB/SQ/LQ wrap, stale snapshots,
scalar commits on vector paths and wrong SQ targets. There are no blocking
findings. Review evidence is
`build/memblock/retirement-contract-20261029/review-v2/report.md` with pinned
source hashes and `mutations.json`; qualification and build provenance are in
`qualification-v2.json` and `provenance-v2.json`. This is a bounded helper and
canonical-callsite review, not complete CPU timing or sliding admission. The
underlying CPU-path audit remains
`build/memblock/mixed-scheduler-20261029/contract-audit.txt`.

Full rename history, exceptional misaligned FP enables, uncheckpointed release
fallback, pre-notification segment releases, long version histories,
simultaneous synonym/CMO/error ownership and native-model full source provenance
remain explicit limits. The structural audit is
`build/memblock/alias-manager-20261029/random-structure-audit.txt`.
