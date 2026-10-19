# SMS Confidence Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task with review checkpoints.

**Goal:** Use L2-trained SMS Event512 accuracy as a runtime confidence gate for L1-generated SMS requests while preserving existing L2 request latency measurements.

**Architecture:** `PfPolicyCalibrator` already trains a source-level SMS Event512 score using the SMS source bin. `Prefetcher` will apply a dedicated SMS score gate in `policyAdmit`, so low-score requests are rejected before entering the per-bank prefetch queue and are rechecked at dequeue if the score changes. The L1 SMS producer remains behaviorally unchanged because its confidence is learned only in L2.

**Tech Stack:** Chisel/Scala RTL, XiangShan `XSPerf*` counters, static source contract checks, gsim build validation.

---

### Task 1: Define the SMS gate contract

**Files:**
- Create: `/tmp/test_sms_confidence_contract.py`

- [ ] **Step 1: Write the failing contract test**

```python
from pathlib import Path

prefetcher = Path(
    "/nfs/home/zhengzhongqiang/WorkL3Prefetch/worktrees/"
    "perf-zj-dual-base-e522fe48/XSCache/src/main/scala/"
    "coupledL2/prefetch/Prefetcher.scala"
).read_text()

assert "def smsAccuracyAdmit" in prefetcher
assert "baseAdmit && smsAccuracy" in prefetcher
assert "smsAccuracyDrop" in prefetcher
```

- [ ] **Step 2: Run the contract test and verify it fails**

Run: `python3 /tmp/test_sms_confidence_contract.py`

Expected: failure because the current source has only the report-only `smsAccuracyWouldDrop` helper and does not combine SMS accuracy with `policyAdmit`.

### Task 2: Implement the L2-trained SMS confidence gate

**Files:**
- Modify: `XSCache/src/main/scala/coupledL2/prefetch/Prefetcher.scala:414-432`

- [ ] **Step 1: Add the actual admission predicate**

```scala
def smsAccuracyAdmit(pfSource: UInt, event512Score: UInt, event512Valid: Bool): Bool =
  !smsAccuracyActive(pfSource) || !event512Valid || event512Score >= smsAccuracyMinScore
```

- [ ] **Step 2: Apply the predicate to the existing policy decision**

```scala
val baseAdmit = Mux(event512Ready(pfSource, event512Valid), calibrated, fallback)
baseAdmit && smsAccuracyAdmit(pfSource, event512Score, event512Valid)
```

- [ ] **Step 3: Make the existing SMS counters describe real drops**

Use the same `smsAccuracyAdmit` predicate for enqueue and dequeue drop conditions. A request is counted as an SMS accuracy drop only when it is an SMS request, its L2 Event512 history is valid, and its score is below `smsAccuracyMinScore`.

- [ ] **Step 4: Run the contract test and verify it passes**

Run: `python3 /tmp/test_sms_confidence_contract.py`

Expected: PASS.

### Task 3: Validate the existing L2 request latency feature

**Files:**
- Inspect: `XSCache/src/main/scala/coupledL2/Common.scala`
- Inspect: `XSCache/src/main/scala/coupledL2/MainPipe.scala`
- Inspect: `XSCache/src/main/scala/coupledL2/GrantBuffer.scala`
- Inspect: `utility/src/main/scala/utility/PerfCounterUtils.scala`

- [ ] **Step 1: Verify request classification**

Confirm `L2RequestPerfKind` covers `read_hit`, `read_miss`, `write_hit`, `write_miss`, and `read_hitmshr`.

- [ ] **Step 2: Verify start and MSHR allocation timestamps**

Confirm `MainPipe` records request kind, start time, MSHR presence, and allocation time into `reqPerf`.

- [ ] **Step 3: Verify completion latency counters**

Confirm `GrantBuffer` emits `l2_<kind>_processing_time_{sum,cnt,pre_mshr_sum,mshr_cnt,cross_roi_cnt}` through `XSPerfRequestLatency` when the final D beat fires.

### Task 4: Review and compile

**Files:**
- Modify: none unless validation finds an issue.

- [ ] **Step 1: Run `git diff --check` in top-level and submodules**
- [ ] **Step 2: Run `review_kill.py` for XiangShan and XSCache**
- [ ] **Step 3: Run the targeted gsim build validation**
- [ ] **Step 4: Remove the temporary `/tmp` contract test**

