# S1 validation

Base: `a253c1163982ae0b485b3a2ed9b61253d980d837`.
Production state budget: 8139 bits/core; not a synthesized area measurement.

## Policy and Stream integration

`./scripts/stream-history/test.sh` with assertions enabled:

```
Policy 1: 67273 differential cycles PASS
L1 native=21096 allowed=20208 blocked=888
L2 native=21096 allowed=12366 blocked=8730
L3 native=21096 allowed=9166 blocked=11930
Stream integration PASS
```

The oracle is the frozen V19 model, not the RTL implementation translated back to software.
It compares all valid-entry state fields and all three candidate decisions every cycle.
The integration fixture compares surviving regions/bitmaps and the training handshake with
an unfiltered native Stream instance. These counts are candidate groups, not F or IPC.

All three variants share identical source except their compile-time policy default.
S1 `--disable-perf` elaboration also verified that the diagnostic shadow and counters disappear.
The common diagnostic guard is identical in S2/S3.

## Full-core AM smoke

DefaultConfig emulator build passed with `EMU_THREADS=4`, `--threads 4`, and `NUM_CORES=1`:
four host emulator threads, one simulated hart. The workload ran on four distinct physical
host cores with NUMA-local allocation. NEMU differential checking remained enabled.

Reproduce after building the emulator, selecting four available physical CPU IDs:

```sh
AM_HOME=/path/to/nexus-am scripts/stream-history/run-am.sh 32,33,34,35
```

Workload SHA256: `a7104e7544a5e4b707edc7de4b13fb5f48b3ca19888885151264efeaf805f5a1`.
Tested emulator SHA256: `c9217920e4bcc9fbb3ae564727831105ba4f277ab5ff79541f4d53e91b448dd9`.

All five checksum stages and `HIT GOOD TRAP` passed:

```
PASS stable-forward checksum 1e5c05dfc00
PASS growth-confirmed-shrink checksum 43dd1da30c0
PASS stable-reverse checksum 1e5c05dfc00
PASS distant-l2-l3 checksum 5db851fe800
PASS after-fence checksum 1e5c05dfc00
STREAM_HISTORY_SMOKE PASS
```

| Level | Candidate block events | Blocked block events | Allowed block events | Conservation |
|---|---:|---:|---:|---|
| L1 | 38,430 | 1,727 | 36,703 | PASS |
| L2 | 72,846 | 34,686 | 38,160 | PASS |
| L3 | 128,235 | 61,033 | 67,202 | PASS |

L1 successful Stream-primary final-data refills F: **4,136**;
refill errors: **0**. These are a different population
from candidate events. L3 uses L2 policy thresholds and its own target address.

Algorithm coverage counters:

- `history_complete`: 410
- `history_seed`: 190
- `history_demand_overrun`: 62
- `history_direction_mismatch`: 126
- `history_flush`: 5

`summary.json` reports `pass=true` and an empty error list. Performance counters are read
from stderr together with checksum/trap results on stdout; repeated dumps retain only the
last value per hierarchy path, with distinct MSHR entries summed.

`make check-format`, explicit formatting checks of new Scala files, shell syntax checks,
and `git diff --check` passed. Final source cleanup removed two redundant PC assignments
that were immediately overwritten by identical existing assignments; this does not change
Chisel last-connect behavior. Other post-build Scala edits were whitespace only.

## Test limitations and diagnosed harness issue

An initial S1 attempt failed at a custom SPFCTL read because the bundled NEMU exposes only
the older low bits (REF `0x3d7b37`, RTL `0x7003d7b37`). The common workload now uses CSR
set/clear with `rd=x0` and does not read or restore that implementation-specific value.
No reference or RTL checks were disabled. All results above use the corrected workload.

This is a functional test, not a SPEC performance comparison. There is no paired baseline
run, causal weight estimate, synthesized area measurement, or IPC improvement claim.
Candidate filtering and bounded later-demand proxies are not exact useful-refill losses.

AI assistance: Codex / GPT-6, Chisel, Verilator and Python reference replay. Human review required.
