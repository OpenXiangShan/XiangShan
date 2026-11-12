# L2 SDBP implementation

This implementation follows `SDBPDetails.md` and the algorithm in
`/nfs/home/chenzhuo/SDBP-ROOT/src/mem/cache/replacement_policies/sdbp_core.cc`.
The six historical open-page runs in `SDBP-ROOT/baserun.md` use
`de1df30a09b46efd7948d75de46fcedd085abb54` and a 128-set, six-way sampler.
The current ROOT core retains those default algorithm rules; its optional
`sampler_demand_pc_only` filter is disabled for this RTL configuration.

## Configurations

- `SDBPConfig`: 1 MiB L2, four 256 KiB slices, eight ways and 512 sets per
  slice; 128 sampled sets and six sampler ways per slice; register sampler
  and register predictor. Cache prefetch other than ICache FDIP defaults to
  disabled; Constantin can override this without rebuilding the RTL.
- `SDBPLRUConfig`: the same geometry and prefetch controls, using true LRU.
- `WithL2SDBP(SDBPParams(...))`: configure sampler geometry, predictor entries
  and dead threshold. Both sampler and predictor use registers.
- Use Constantin `cache_prefetch_enable<hartId>` to control prefetch at simulator
  startup. Build with `--with-constantin` and pass `--cst-file` when running.
  ICache FDIP retains its normal CSR control.

The SDBP configurations retain this RTL's existing exclusive L3 implementation
and default L1D/L3 geometry. They do not claim whole-system equivalence with the
mostly inclusive GEM5 configuration or reproduce its performance score.

## Algorithm and timing

Each slice owns a sampler and predictor. There is no bypass path, sampler
request queue or multi-cycle sampler transaction lock. The sampler is a
register array. Per the updated implementation scope, SCArray also uses
registers; conversion to SRAM is deferred. No training queue, port arbitration
or additional predictor-ready gating is present.
Its independent set/way parameters support non-power-of-two associativity.
A single sampler way omits LRU state.

The sampler selects invalid, then the first dead way, then true LRU. L2
replacement selects an eligible invalid way, then the first eligible dead
way without an owning-core L1I/L1D copy, then true LRU within eligible ways.
Copy protection does not erase a dead bit and does not restrict the LRU
fallback. Busy and excluded ways are removed before selection.

The signature is ROOT's 15-bit XOR fold of `PC >> 1`. The three indices use
ROOT's unsigned 64-bit `mixed` hash and golden-ratio seeds. Counters start at
zero. A sum of at least eight predicts dead. Dead training saturatingly
increments all three counters; live training decrements tables zero/two and
right-shifts table one. Hash arithmetic explicitly truncates products to
64 bits. Current indices travel with requests before s1; the sampler's old
signature indices are computed in s2 for s3 training.

Directory and SDBP share the s1 acceptance event. Sampler snapshots and
predictor results are latched through s2, and MainPipe commits actual tag
hits and successful tag installations in s3. Initial misses, retries,
denied/corrupt requests and permission-only completions do not commit a
second sampler access. A missing PC can train an old valid signature but
stores an invalid new signature and clears its prediction. Prefetch sampler
hits leave the sampler and its recency unchanged; prefetch misses may train
a victim dead but insert an invalid signature and clear dead.

Prediction observes the state at s1, excluding training newly committed in
that same cycle. Training independently reads the old signature's current
counters and updates them at the clock edge. Consecutive training operations
observe the previous edge's updates, including when indices collide.

Sampler writes share Directory write exclusion. Get and Hint hits update
true-LRU state. Sampled Hints also use the existing MCP2 entrance interval;
unsampled Hints retain their original behavior. Existing RequestBuffer
same-set exclusion also protects replacement-state RAW dependencies.

The SDBP victim is selected in s3 and retained through DataStorage's MCP2
hold cycle. The refill-valid pulse ends before that hold cycle, and dead
bits or L1I residency can change at the s3 commit edge; these changes must
not recompute the victim of an already accepted request. Directory tests
check the victim again during validHold, including an occupied invalid way.

## PC and L1 residency

`PCValidKey` accompanies the existing `PCKey`. Implemented virtual PC bits
are sign-extended to RV64 canonical form before signature folding. PC zero is valid when the
valid bit is asserted. Load requests carry their PC; requests without a
single attributable PC, including Store Buffer requests, mark it invalid.
MissQueue preserves the first valid demand PC when requests merge,
including compressed enqueue groups and a load merging while Acquire fires.
Metadata reflects demands accepted before or alongside the outgoing Acquire;
a demand hidden inside L1 after that transfer cannot retroactively update L2.

L1 prefetch Acquires are identified by request source as well as opcode;
they are distinct from demand accesses without PCs. MSHR normal and merged
refills preserve the final signature, validity, indices and request type.
A merged demand also retains its request source through output arbitration.

D-copy protection uses Directory `clients`, which can be more conservative
than GEM5's instantaneous tag check. I-copy protection mirrors accepted
ICache metadata writes, actual per-way invalidations, reset and `fence.i`.
The direct BundleBridge has no event queue. Same-edge changes are forwarded
to victim queries. Physical lookups search all possible virtual alias sets.
Pending miss/response buffers are not counted as resident ICache lines.

## Validation commands

From `XiangShan`, use the local Mill launcher and the configured JDK:

```sh
export JAVA_HOME=/nfs/share/graalvm-jdk-17.0.11+7.1/graalvm-jdk-17.0.11+7.1
export COURSIER_MIRRORS=/nfs/home/chenzhuo/.metals/mirror.properties
export NOOP_HOME="$PWD"
.vscode/mill-metals --no-server xiangshan.test.testOnly cache.SDBPTest
.vscode/mill-metals --no-server xiangshan.test.testOnly cache.SDBPIntegrationTest
.vscode/mill-metals --no-server xiangshan.test.testOnly cache.SDBPMetadataTest
.vscode/mill-metals --no-server xiangshan.test.testOnly cache.SDBPMissQueuePCTest
.vscode/mill-metals --no-server xiangshan.test.runMain top.XiangShanSim \
  --config SDBPConfig --enable-difftest --target systemverilog \
  --split-verilog --target-dir build/sdbp-reg/rtl \
  --firtool-opt '-O=release --disable-annotation-unknown --lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none'
```

Full simulation generation needs
`NOOP_HOME` and `--enable-difftest`, consistent with this project's simulation
build. The ROOT hash fixture is generated by compiling the actual ROOT core;
its generator and all implementation-session logs are under
`../logs/sdbp-implementation-20261110/`.

Tests cover the sampler set/way matrix (2/64/128/512 sets and 1/3/6/8/12 ways),
invalid geometry, sampling coverage, ROOT hash vectors, masked true LRU,
register prediction snapshots before same-cycle training, independent colliding
training, saturation, reset, s3 commit qualification, missing PCs,
prefetch semantics, MSHR final metadata, real Directory copy/victim masks,
ICache alias residency, and RequestArb backpressure/Hint spacing.

Directory exports training, sampler-write, protected-copy and dead/LRU victim
counters. Existing Directory/MCP2 stall counters remain available. Functional
simulation does not establish
ASIC timing or area closure for the exact 64-bit hash and register backend.

## Validation results (2026-11-10)

The current register-only scope has 35 passing test cases. The final SCArray
snapshot test and nine integration/metadata/PC-selection tests were rerun after
removing the SRAM backend. The 25 unchanged hash, sampler geometry, ROOT trace,
masked-LRU and ICache-residency cases retain their earlier passing evidence.
`../logs/sdbp-implementation-20261110/test-evidence.json` maps each case to its log.
The PC-selection harness checks the merge selector and canonical extension;
its request acceptance/cancellation predicates are checked at the call sites.

The final source manifest was elaborated through the full `top.XiangShanSim`
hierarchy with `SDBPConfig` and difftest enabled. SystemVerilog generation
completed successfully and emitted 2,025 files into
`../logs/sdbp-implementation-20261110/rtl-register-only-final/`.
`elaborate-register-only-final.log` records the run. The emitted SDBP wrapper
connects only the register sampler and register predictor, with no training
queue or predictor-ready gating. Main and XSCache diffs pass `git diff --check`.

An earlier register RTL snapshot also completed the hello image with NEMU
difftest: GOOD TRAP, 561 instructions and 7,201 cycles (seed 6077). This is
supplemental smoke evidence: that snapshot precedes the CMO opcode-width fix,
cancelled-merge PC filtering and removal of unused SRAM interfaces.
CMO classification and predictor changes have directed tests; cancellation
predicates were reviewed at the merge call sites. The old smoke is not a
runtime test of the final source manifest. The build log and smoke log are
archived with the session.

## FDIP exception (2026-11-11)

The prefetch master switch explicitly excludes ICache FDIP. The prior implementation
also masked the ICache prefetch pipeline's CSR enable, disabling FDIP; that mask
has been removed. Software `prefetch.i` remains blocked at its request input.
Both `SDBPConfig` and `SDBPLRUConfig` use this policy. The validation snapshots
from 2026-11-10 predate this change and had FDIP disabled.

After this correction, Scala compilation and all three `SDBPIntegrationTest`
cases pass. The FDIP CSR connection and remaining software-prefetch gate were
reviewed directly; full RTL generation and an FDIP runtime workload have not
been rerun for this correction. Evidence is in
`../logs/no-prefetch-preserve-fdip-20261111/test-integration.log`.

## Constantin master switch (2026-11-11)

Build with `--with-constantin`, then set the per-hart startup parameter
`cache_prefetch_enable0` (hart 1 uses `cache_prefetch_enable1`, etc.):

```text
cache_prefetch_enable0 0
```

`0` blocks cache prefetch other than ICache FDIP. `1` permits the prefetchers
already configured in RTL, subject to their existing CSR and individual
Constantin enables. This does not force disabled CSR features on or instantiate
prefetchers excluded by other configurations. The format is plain text
`name value`, not JSON. The value is loaded when the simulator starts; it is
not a live switch while a workload is running.

The same record controls core data prefetchers and their L1/L2 outputs, data
software-prefetch misses, store prefetch, ICache software `prefetch.i`, and all
configured L2 prefetchers (PBOP/VBOP/TP/receiver/next-line). Prefetch training
and prefetch TLB requests are also gated while disabled. L2Top forwards the
core default so all readers for a hart use the same startup value. The configuration sets only
`EnableCachePrefetch`; L2Top supplies the L2 default when constructing L2. Disabled
Decoupled sources are consumed without emitting a downstream request. ICache
FDIP continues to use its unmodified `csrPfEnable` connection.

`SDBPConfig` and `SDBPLRUConfig` set the default to `0`; normal
configurations retain default `1`. `WithNoPrefetch` now changes defaults rather
than removing prefetcher instances or changing software/store feature options.
Without `--with-constantin`, the selected default is constant and a parameter
file cannot override it.

Single-hart example files are `scripts/constantin/cache-prefetch-off.txt` and
`scripts/constantin/cache-prefetch-on.txt`. Add a line for every additional hart
whose default must be overridden. For local use:

```sh
# Build the simulator with --with-constantin first.
build/emu --cst-file "$PWD/scripts/constantin/cache-prefetch-off.txt" <other emulator arguments>
build/emu --cst-file "$PWD/scripts/constantin/cache-prefetch-on.txt" <other emulator arguments>
```

The current remote CI already adds `--with-constantin` when `cst_file` is
nonempty. Set `cst_file` to the absolute path of the desired file on the runner's
shared filesystem. Use distinct filename stems for different settings because
CI reuses results by filename stem rather than file content. The existing
workflow currently creates separate emulator caches for these filenames,
although the generated simulator supports both values at runtime.

All RTL/smoke/area snapshots in earlier sections predate retaining the prefetch
hardware for this master switch. New validation evidence is recorded under
`../logs/prefetch-constantin-20261111/`.

The control/configuration suite has four passing cases. A separately built
RTL harness instantiates the real L2 PBOP/VBOP/receiver and the common core
request gate. The same binary passes default, off-file, on-file and independent
hart-key runs: disabled receiver request count is zero; enabled count is 46.
It also checks CSR disable/re-enable, backpressure, and 256 cycles of disabled
training with no L2/TLB requests and no training backpressure. A second harness
built with Constantin disabled verifies that the off default cannot be changed
by an on-file. These are focused RTL tests, not a whole-core performance run.

Full `top.XiangShanSim --config SDBPConfig --with-constantin --enable-difftest`
SystemVerilog generation also passes (2,091 files). The emitted ICache module
connects FDIP `io_csrPfEnable` directly to its original CSR input. Emitted
consumers cover L2, DCache load/store/main pipes, ICache software prefetch,
MemBlock store prefetch and the core prefetcher wrapper. All ten elaboration
readers for hart 0 report default zero; the generated C++ initializer agrees.
See `full-rtl-audit.json`, `runtime-results.json` and `verification.json` in the
new evidence directory. No whole-core workload was rerun for this change.

## Generic synthesis check

Yosys 0.60+102 was run on the default register SDBP block snapshot before
removing the unused SRAM interfaces, after FIRRTL
lowering (`proc; opt; memory_map; opt; techmap; opt; stat; ltp -noff`).
The result contains 310,668 generic bit-level cells and 52,200 flip-flops,
including pipeline state. It excludes Directory's 4,096 dead bits per slice
and the shared owning-core ICache mirror. The raw predictor state is 24,576
bits, sampler entries 25,344 bits, and sampler LRU 1,920 bits per slice.
The longest local combinational paths are 16 cells in SCArray and 101 in
the sampler, including the exact old-signature mixed hash in s2. These are
structural path lengths, not clock periods or technology-mapped cell areas.
The register/mixed-hash implementation needs a technology-specific timing
and area assessment before frequency signoff.

SRAM implementation and its queue/port verification are deferred. Archived
SRAM draft artifacts in the session log directory are outside the current
implementation scope.

The separate prefetch command-line and Makefile switches have been removed.
Use the Constantin parameter files above; configuration defaults are retained.
