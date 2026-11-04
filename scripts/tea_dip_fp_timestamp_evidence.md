# FP timestamp observer evidence

Run with the experiment report containing closed GSIM inputs and the preserved analysis helpers:

```bash
python3 scripts/tea_dip_fp_timestamp_evidence.py --report-dir /path/to/experiment-report
```

The consumer keeps this diagnostic stage separate from optimization results. It binds source/launch SHA, CoreMark and SPEC command identity, raw manifests and binary/source, then checks baseline execution while allowing only the ten named fmul/fdiv_fsqrt stage sums to change. Every other old native multiset remains checked. All seven functional evidence categories are preserved, including complete PC signatures and NoCommit records.

FP writeback checks require all four pipe counter pairs and actual observed events. Missing counters stay unknown; raw negative sums remain rejected. Four fixed same-seed windows are required before declaring this observer validation complete. Empty stages report zero collected windows with null pass flags. Observed monotonicity and nonnegative sums do not prove criticality, exclusive CPI or general service latency. No averages or performance gains are invented.

The comparator has been exercised on a real closed baseline log: unchanged accepted, intended FP timestamp sum change accepted, unrelated old waitLoadCycle change rejected. Baseline logs lacking the new order counters remain unknown and their negative FP sums are retained. These checks do not replace real observer compilation, Difftest or same-hardware windows.
