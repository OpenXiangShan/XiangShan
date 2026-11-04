# FP-pipe writeback interval evidence

Run the consumer with a report containing archived tool source, launch, builds and closed windows:

```bash
python3 scripts/tea_dip_fp_wb_intervals_evidence.py --report-dir /path/to/report
```

The new tool is compared directly with its parent timestamp observer 3f7, with every old native multiset and six other execution evidence categories protected. Only the four named added interval sums are removed from the old-native comparison. No old ROB latency sum is exempted. The parent's four-point failure against 841 remains failed.

For each phase and each of four FP pipes, retain checked WB, order failures, denominator checked - failures and all four raw sums. Missing fields stay unknown. Require nonnegative sums/population, three subinterval sums equal to the total, zero sums for an empty cohort, actual FP events and zero ordering failures. Numeric conservation does not prove event correspondence or causal validity. Source/event applicability remains explicitly unknown until separately verified. No means, service latency or exclusive CPI is inferred.

The cohort is the existing FP-pipe output-valid event, including writes subsequently flushed. IQ to last selection includes retries. It is not the retired-instruction population. An empty report has zero collected windows and null pass flags. Real parent logs exercise missing-field handling and old-native protection; artificial conserved/error interval additions are labeled synthetic comparator checks, never actual RTL results.
