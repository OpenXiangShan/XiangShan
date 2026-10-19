# ZhuJiang Stash/Demand Timing Counters

## Goal

Add ZhuJiang performance counters that distinguish a stash line being useful
from a stash line arriving early enough to hide the demand miss. The counters
must cover both the distance from stash installation to first demand use and
the signed timing between the stash request ingress and the matching demand
ingress.

The instrumentation is observational only. It must not change request
selection, arbitration, cache state, or response timing.

## Existing signals

`dongjiang/backend/Commit.scala` already identifies the matching same-line
use through `stashFirstUse`, stores `stashInstallCycle`, and carries the
request ingress cycle in `taskReg.chi.perfIngressCycle`. The existing useful
event remains the denominator boundary:

```scala
stashFirstUse = taskReg.cmt.wriLLC && taskReg.dir.llc.hit &&
  taskReg.dir.llc.metaVec.head.stashPrefetched.get &&
  !taskReg.dir.llc.metaVec.head.stashAccessed.get && consumeRead
```

The new timing measurements are emitted only when `stashFirstUse` is true.
This keeps the timing samples tied to the same-line useful population and
avoids treating `l3_prefetch_hitCache` or PoS counters as useful events.

## Measurements

### Install-to-use distance

At the first demand use, calculate:

```text
use_after_install = perfTimer - stashInstallCycle
```

Export:

```text
zj_hn_stash_useful_install_to_use_sum
zj_hn_stash_useful_install_to_use_sampled
zj_hn_stash_useful_install_to_use_mean
```

Also export a distribution with short-distance bins:

```text
0-4, 5-9, 10-19, 20-39, 40-79, 80-159, 160+
```

These values measure how long a successfully installed stash line exists
before its first demand use. A high useful rate with most samples in the first
bins indicates that useful lines arrive with little lead time.

### Demand-versus-stash request timing

Store the stash request ingress cycle in the line metadata when a
`StashOnceShared` request is accepted:

```text
stash_request_cycle = taskReg.chi.perfIngressCycle
```

At `stashFirstUse`, calculate the signed demand lead by keeping separate
non-negative magnitudes for the two directions:

```text
before_distance = stash_request_cycle - taskReg.chi.perfIngressCycle
after_distance = taskReg.chi.perfIngressCycle - stash_request_cycle
```

Interpretation:

```text
negative: demand entered HN before the stash request
zero:     same ingress cycle
positive: stash entered HN before the demand
```

Export:

```text
zj_hn_stash_useful_demand_before_req_sum
zj_hn_stash_useful_demand_before_req_sampled
zj_hn_stash_useful_demand_after_req_sum
zj_hn_stash_useful_demand_after_req_sampled
zj_hn_stash_useful_demand_before_stash_req
zj_hn_stash_useful_demand_after_stash_req
```

The report derives the signed mean as
`(after_req_sum-before_req_sum)/(after_req_sampled+before_req_sampled)`.
Export separate unsigned histograms for before and after distances using:

```text
0..4, 5..9, 10..19, 20..39, 40..79, 80..159, 160+
```

The negative bins directly identify useful lines whose demand entered before
the stash request. The small positive bins identify stash requests that
arrived only a few cycles before demand entry.

### Metadata field

Add a 64-bit `stashRequestCycle` field beside the existing LLC stash metadata.
On a stash installation, write the request's `perfIngressCycle`; clear it
whenever the stash metadata is invalidated. Preserve it through ordinary
metadata updates in the same way as `stashInstallCycle`.

The implementation must guard the field and all derived measurements with
the existing performance-counter enable condition. If either cycle value is
invalid, do not increment a sample or sum counter.

## Report contract

The analysis parser must expose the new counters separately from:

- `l3_prefetch_useful`, which remains a count of first demand uses;
- `l3_prefetch_hitCache`, which remains an HN stash lookup hit/no-op event;
- `l3_prefetch_hitPos`, which remains the distinct-address stash block event;
- `l3_prefetch_late_pos`, which counts a stash request finding an existing
  same-address PoS entry, independent of later useful consumption.

The report must show both means and the short-distance histogram. It must not
derive a useful rate from the timing counters or use timing counts as a CPU
stall cycle total.

## Validation

Add focused tests for the report parser covering:

1. valid install-to-use samples and mean calculation;
2. negative, zero, and positive demand-minus-stash bins;
3. invalid or missing cycle values remaining absent rather than zero;
4. existing useful and stash counters remaining unchanged.

Run the focused ZhuJiang/XSCache tests and regenerate the two completed
`libquantum` reports after the RTL counters are available in a new run.
