# Native pipeline latency validity

Run on a closed simulator_err.txt:

```bash
python3 scripts/tea_dip_latency_validity.py /path/to/closed/simulator_err.txt --output validity.json
```

The existing native parser retains signed raw values and duplicate-name semantics. This tool inspects unique ROB per-FU stage-latency sums in every printed block. Negative elapsed sums are rejected, with the original field names and values preserved. No missing field is replaced by zero.

A block with stage fields and no negative sums remains not_validated; usable_for_stage_attribution is null. Numeric nonnegativity alone does not validate timestamp wiring, clock domains, stage applicability or criticality. Rejected groups must not be normalized into pipeline service latency or CPI. This is a data-quality gate, not a timestamp wiring repair or a performance optimization.
