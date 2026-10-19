# ZhuJiang Stash/Demand Timing Counters Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add ZhuJiang counters that measure both stash-install-to-useful-hit distance and signed same-line demand-versus-stash ingress timing.

**Architecture:** Preserve the existing LLC stash metadata and `stashFirstUse` boundary. Add one 64-bit stash request ingress cycle beside `stashInstallCycle`; compute both timing families only on the same-line first useful demand event; export sums, samples, means, and signed/short-distance distributions through `ZJPerf`.

**Tech Stack:** Chisel/Scala, ZhuJiang `ZJPerf`, Python prefetch report parser, standard-library unit tests.

---

### Task 1: Add parser tests for new timing metrics

**Files:**
- Modify: `/nfs/home/zhengzhongqiang/.codex/skills/hardware-skill/perf_analy/part_perf_ananly/l3-prefetch/tests/test_analyze.py`
- Modify: `/nfs/home/zhengzhongqiang/.codex/skills/hardware-skill/perf_analy/part_perf_ananly/l3-prefetch/scripts/analyze.py`

- [x] **Step 1: Write the failing test**

Add a synthetic final PERF epoch containing:

```text
zj_hn_stash_useful_install_to_use_sum=24
zj_hn_stash_useful_install_to_use_sampled=2
zj_hn_stash_useful_demand_before_req_sum=6
zj_hn_stash_useful_demand_before_req_sampled=1
zj_hn_stash_useful_demand_after_req_sum=10
zj_hn_stash_useful_demand_after_req_sampled=1
zj_hn_stash_useful_demand_before_stash_req=1
zj_hn_stash_useful_demand_after_stash_req=1
```

Assert that the parser preserves the two means, signed delta, and separate before/after counts.

- [x] **Step 2: Run the test to verify it fails**

Run:

```bash
python3 /nfs/home/zhengzhongqiang/.codex/skills/hardware-skill/perf_analy/part_perf_ananly/l3-prefetch/tests/test_analyze.py
```

Expected: FAIL because the new metrics are not yet in the L3 metric contract.

- [x] **Step 3: Extend the parser contract minimally**

Add the new metric names and derived means to the L3 parser without changing existing metric meanings or the selected-hart/shared-HN scopes.

- [x] **Step 4: Run the focused test to verify it passes**

Run the same command and expect `OK`.

### Task 2: Add stash request cycle metadata

**Files:**
- Modify: `/nfs/home/zhengzhongqiang/WorkL3Prefetch/worktrees/perf-zj-dual-base-e522fe48/XSCache/ZhuJiang/src/main/scala/dongjiang/bundle/ChiBundles.scala:81-86`
- Modify: `/nfs/home/zhengzhongqiang/WorkL3Prefetch/worktrees/perf-zj-dual-base-e522fe48/XSCache/ZhuJiang/src/main/scala/dongjiang/backend/Commit.scala:238-252`

- [x] **Step 1: Add `stashRequestCycle` beside `stashInstallCycle`**

Use the same 64-bit width and LLC-only optional field guard.

- [x] **Step 2: Preserve and clear the field with stash metadata**

When `stashInstall` is true, write `taskReg.chi.perfIngressCycle`; when `stashInvalid` is true, clear it; otherwise forward the stored value unchanged.

- [x] **Step 3: Compile the affected ZhuJiang sources**

Run the narrow available Scala compile/test target for the ZhuJiang submodule and confirm the new bundle field has no unconnected or width errors.

### Task 3: Export useful-hit timing counters

**Files:**
- Modify: `/nfs/home/zhengzhongqiang/WorkL3Prefetch/worktrees/perf-zj-dual-base-e522fe48/XSCache/ZhuJiang/src/main/scala/dongjiang/backend/Commit.scala:238-260`

- [x] **Step 1: Export install-to-useful timing**

Inside the existing `ZJPerf.whenEnabled` block, gate on `io.replTask.fire && stashFirstUse` and calculate `perfTimer - stashInstallCycle` only when the stored cycle is valid.

- [x] **Step 2: Export split demand-minus-stash timing**

For the same useful event, calculate separate non-negative distances for demand-before-stash and demand-at/after-stash. The parser derives the signed mean from the two direction sums and samples.

- [x] **Step 3: Add counter names from the approved design**

Export:

```text
zj_hn_stash_useful_install_to_use_sum
zj_hn_stash_useful_install_to_use_sampled
zj_hn_stash_useful_demand_before_req_sum
zj_hn_stash_useful_demand_before_req_sampled
zj_hn_stash_useful_demand_after_req_sum
zj_hn_stash_useful_demand_after_req_sampled
zj_hn_stash_useful_demand_before_stash_req
zj_hn_stash_useful_demand_after_stash_req
```

Use `ZJPerf.distribution` for the approved positive before/after bins. Do not add these values to CPU stall totals.

### Task 4: Render the new metrics

**Files:**
- Modify: `/nfs/home/zhengzhongqiang/.codex/skills/hardware-skill/perf_analy/part_perf_ananly/l3-prefetch/scripts/analyze.py`
- Modify: `/nfs/home/zhengzhongqiang/.codex/skills/hardware-skill/perf_analy/part_perf_ananly/prefetcher/scripts/prefetch_common.py` only if a signed histogram field requires a shared parser helper

- [x] **Step 1: Map raw counters with exact scopes and units**

Mark the timing sums as cycle, samples as events, and preserve `counter_absent` when a pre-instrumentation log lacks them.

- [x] **Step 2: Render means and timing classifications**

Add separate rows for install-to-use mean, demand-minus-stash mean, demand-before-stash count, and demand-after-stash count. Keep them separate from `l3_prefetch_hitCache`, `l3_prefetch_useful`, and the PoS alias counters.

- [x] **Step 3: Add parser tests for missing and signed values**

Cover absent cycle fields, negative means, zero samples, and preservation of existing stash metrics.

### Task 5: Validate and regenerate reports

**Files:**
- Generated: `libquantum_18954_0.091086/perf-skill/prefetch/l3/*`
- Generated: `libquantum_59303_0.127268/perf-skill/prefetch/l3/*`
- Generated: both `cache-system-analysis.md` files after new logs are available

- [x] **Step 1: Run focused Python tests**

Run the L3 parser tests and the existing prefetch common tests with the standard-library test runner.

- [x] **Step 2: Run the ZhuJiang compile/test target**

Run the narrow target identified during implementation; do not launch a full SPEC workload before the RTL compiles.

- [ ] **Step 3: Inspect emitted counter names**

Verify new counters appear in a fresh ZhuJiang `simulator_err.txt` final PERF epoch and that missing counters in old logs remain explicitly absent.

- [ ] **Step 4: Regenerate both completed libquantum reports**

Run the cache-system batch for `libquantum_18954_0.091086` and `libquantum_59303_0.127268`, then report the timing histograms and before/after counts.
