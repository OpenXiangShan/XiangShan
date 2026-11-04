# Closed FP credit evidence

Use independently extracted, closed 1M+1M windows from the RTL DIP experiment report:

```bash
python3 scripts/tea_dip_credit_evidence.py --report-dir /path/to/report --variant fp-credit-guard
```

Supported candidates are fp-credit, fp-credit-guard and fp-credit-div, each compared with control. The report must already contain raw/analysis/summary.json, retirement-gap-deltas.csv, pc-deltas.csv, per-window audits and closed native logs. Output is raw/analysis/<variant>-evidence.json. A candidate with no collected windows has completed=0 and no result rows.

The evidence keeps full ROI CPI, exact TEA interiors and omitted edges, the exclusive DIP budget, freelist/ROB-head counters, sixteen miss-entry exposure sums, replay context, PC changes and 128/512 gap bounds. The 128–511 band is a conservative difference of the two long-gap bounds.

PC ties use PC, state, signature and label order after absolute delta, so input-row reordering does not change the shortlist. ROB-head type waits are retirement exposure; miss-entry cycles can overlap. Neither is average service latency or an exclusive loss budget. Printed prefetch labels remain raw context. No suite score or adoption decision is computed.
