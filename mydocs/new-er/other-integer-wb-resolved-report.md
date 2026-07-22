# IntER Other-Integer Writeback-Resolved Implementation Report

## Summary

本报告对应 `mydocs/new-er/plan/int-er-other-integer-writeback-resolved-plan.md`。当前实现把严格白名单内的简单 ALU、MUL/DIV、`bku/i2f/i2v` 从 only-commit-resolved 前移到 final accepted writeback 后 resolved。Branch、Load/Store、AMO、CSR、Fence、VSet、FP/Vector、compressed/fusion/multi-uop entry 仍保守等待 actual normal commit。

| Item | Value |
| --- | --- |
| Artifact root | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721` |
| Git head captured by runner | `e191a553b21f864a6e22e6e01ffa45bfa29b06a6` |
| Current git HEAD | `e191a553b21f864a6e22e6e01ffa45bfa29b06a6` |
| Dirty status captured by runner | `M mydebug/new-er/README.md
 M mydebug/new-er/check_protocol.py
 M src/main/scala/top/Configs.scala
 M src/main/scala/xiangshan/Parameters.scala
 M src/main/scala/xiangshan/backend/IntEarlyReleaseBundles.scala
 M src/main/scala/xiangshan/backend/rob/Rob.scala
 M src/main/scala/xiangshan/backend/rob/RobBundles.scala
 M src/test/scala/xiangshan/backend/IntEarlyReleaseBundlesTest.scala
 M src/test/scala/xiangshan/backend/IntEarlyReleaseRobTest.scala` |
| Clean exit | 0 |
| Build exit | 0 |
| Matrix fail_name | MATRIX_PASS_ALL |
| Total instr | 30,145,500 |
| Total cycles | 19,645,871 |
| Aggregate IPC | 1.534445 |

## Config Evidence

- `DefaultConfig` 经过 `WithIntEarlyReleaseFunctional`，当前 functional IntER 开启，`observeOnly=false`，并显式开启 `enableOtherIntegerWritebackResolve=true`。
- Source evidence: `enableOtherIntegerWritebackResolve = true` in `src/main/scala/top/Configs.scala`; `enableOtherIntegerWritebackResolve = false` in `src/main/scala/top/Configs.scala`; `new WithIntEarlyReleaseFunctional` in `src/main/scala/top/Configs.scala`; `enableOtherIntegerWritebackResolve: Boolean = false` in `src/main/scala/xiangshan/Parameters.scala`; `def IntEREnableOtherIntegerWritebackResolve` in `src/main/scala/xiangshan/Parameters.scala`
- `IntEarlyReleaseParams()` 的裸默认值保持 `enableOtherIntegerWritebackResolve=false`，用于 bisect 和禁用配置。

## Verification Matrix

判定规则：除既有 `povray --max-instr` 特例外，以最终 `HIT GOOD TRAP` / runner classification 为准；`rvh-tests` 内部 self-check `FAILED/failed` 文本不覆盖最终 good-trap 判定。

| Workload | Exit | Verdict | Reason | Seconds | Good traps | Instr | Cycles | IPC |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `cputest` | 0 | PASS | hit-good-trap | 196 | 33 | 60,635 | 199,636 | 0.303728 |
| `riscv-tests` | 0 | PASS | hit-good-trap | 529 | 117 | 35,934 | 586,105 | 0.061310 |
| `misc-tests` | 1 | PASS | hit-good-trap | 578 | 2 | 462,095 | 633,003 | 0.730004 |
| `rvh-tests` | 0 | PASS | hit-good-trap | 7241 | 5 | 6,233,046 | 8,585,899 | 0.725963 |
| `microbench` | 0 | PASS | hit-good-trap | 202 | 1 | 326,375 | 219,921 | 1.484056 |
| `coremark` | 0 | PASS | hit-good-trap | 974 | 1 | 3,210,840 | 1,149,708 | 2.792744 |
| `linux-hello-opensbi` | 0 | PASS | hit-good-trap | 4537 | 1 | 14,424,222 | 5,527,986 | 2.609309 |
| `iopmp-test` | 0 | PASS | hit-good-trap | 61 | 1 | 4,930 | 32,825 | 0.150190 |
| `povray` | 0 | PASS | povray accepted max-instr stop | 1533 | 0 | 5,000,006 | 1,739,762 | 2.873960 |
| `copy_and_run` | 0 | PASS | hit-good-trap | 737 | 1 | 352,564 | 891,254 | 0.395582 |
| `f16_test` | 0 | PASS | hit-good-trap | 83 | 12 | 2,750 | 59,387 | 0.046306 |
| `zcb-test` | 0 | PASS | hit-good-trap | 32 | 1 | 32,103 | 20,385 | 1.574834 |

## Writeback-Resolved Counter Closure

| Closure | LHS | LHS value | RHS sum | Delta | Status |
| --- | --- | --- | --- | --- | --- |
| WB resolved class closure | resolved_by_writeback | 14,307,800 | 14,307,800 | 0 | PASS |
| WB final-candidate outcome closure | final_candidate | 14,519,112 | 14,519,112 | 0 | PASS |
| ST pending-work outcome closure | pending_work | 16,582,686 | 16,582,686 | 0 | PASS |
| ST valid-frontier reason closure | valid_frontier_blocker | 10,906,318 | 10,906,318 | 0 | PASS |

## Writeback-Resolved Events

| Class | Counter | Value | Share of resolved |
| --- | --- | --- | --- |
| alu | `int_er_rob_resolved_by_writeback_alu` | 14,133,660 | 98.783% |
| mul | `int_er_rob_resolved_by_writeback_mul` | 111,028 | 0.776% |
| div | `int_er_rob_resolved_by_writeback_div` | 7,271 | 0.051% |
| other | `int_er_rob_resolved_by_writeback_other` | 55,841 | 0.390% |

| Counter | Aggregate | `cputest` | `riscv-tests` | `misc-tests` | `rvh-tests` | `microbench` | `coremark` | `linux-hello-opensbi` | `iopmp-test` | `povray` | `copy_and_run` | `f16_test` | `zcb-test` |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `int_er_rob_wb_resolve_eligible_enq` | 15,902,264 | 38,794 | 30,860 | 392,380 | 3,006,571 | 211,381 | 1,483,599 | 9,566,113 | 1,379 | 977,023 | 180,931 | 649 | 12,584 |
| `int_er_rob_wb_resolve_final_candidate` | 14,519,112 | 32,569 | 21,390 | 340,587 | 2,290,275 | 150,302 | 1,430,623 | 9,229,448 | 1,319 | 849,388 | 160,005 | 649 | 12,557 |
| `int_er_rob_resolved_by_writeback` | 14,307,800 | 31,075 | 18,957 | 317,107 | 2,214,596 | 143,665 | 1,415,265 | 9,172,432 | 1,303 | 826,244 | 153,986 | 636 | 12,534 |
| `int_er_rob_resolved_by_writeback_alu` | 14,133,660 | 22,972 | 18,530 | 295,793 | 2,210,151 | 140,856 | 1,347,351 | 9,159,829 | 1,270 | 778,093 | 147,208 | 599 | 11,008 |
| `int_er_rob_resolved_by_writeback_mul` | 111,028 | 3,458 | 162 | 0 | 3,442 | 2,128 | 67,807 | 11,573 | 1 | 14,277 | 6,686 | 0 | 1,494 |
| `int_er_rob_resolved_by_writeback_div` | 7,271 | 3,616 | 66 | 0 | 1,003 | 649 | 70 | 998 | 0 | 809 | 60 | 0 | 0 |
| `int_er_rob_resolved_by_writeback_other` | 55,841 | 1,029 | 199 | 21,314 | 0 | 32 | 37 | 32 | 32 | 33,065 | 32 | 37 | 32 |
| `int_er_rob_wb_resolve_blocked_need_flush` | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| `int_er_rob_wb_resolve_blocked_redirect_recovery` | 211,312 | 1,494 | 2,433 | 23,480 | 75,679 | 6,637 | 15,358 | 57,016 | 16 | 23,144 | 6,019 | 13 | 23 |
| `int_er_rob_wb_resolve_rejected_identity_reuse_raw` | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| `int_er_rob_wb_resolved_entry_cycle` | 293,697,587 | 805,105 | 217,432 | 1,315,355 | 134,929,048 | 5,284,217 | 30,356,839 | 92,492,239 | 981,408 | 20,849,608 | 5,571,210 | 4,037 | 891,089 |
| `int_er_rob_interrupt_deferred_for_guard_cycle` | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| `int_er_rob_interrupt_deferred_for_guard_episode` | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| `int_er_rob_outstanding_guard_sum` | 304,882 | 1,107 | 426 | 13,318 | 100,922 | 7,678 | 36,733 | 129,886 | 35 | 10,190 | 4,505 | 13 | 69 |

## ST Blocker / Bottleneck Counters

| Class | Counter | Cycles | Share of valid-frontier blocker | Share of pending-work |
| --- | --- | --- | --- | --- |
| scalar_load | `int_er_rob_st_blocker_class_scalar_load_cycle` | 3,380,750 | 30.998% | 20.387% |
| scalar_store | `int_er_rob_st_blocker_class_scalar_store_cycle` | 2,630,676 | 24.121% | 15.864% |
| other_integer | `int_er_rob_st_blocker_class_other_integer_cycle` | 1,836,292 | 16.837% | 11.074% |
| csr | `int_er_rob_st_blocker_class_csr_cycle` | 1,064,798 | 9.763% | 6.421% |
| branch_jump | `int_er_rob_st_blocker_class_branch_jump_cycle` | 846,427 | 7.761% | 5.104% |
| fence | `int_er_rob_st_blocker_class_fence_cycle` | 423,741 | 3.885% | 2.555% |
| amo | `int_er_rob_st_blocker_class_amo_cycle` | 146,462 | 1.343% | 0.883% |

| Counter | Task34 pre-WB baseline | Current | Delta | Delta / baseline |
| --- | --- | --- | --- | --- |
| `int_er_rob_st_cycle` | 25,236,020 | 19,591,873 | -5,644,147 | -22.365% |
| `int_er_rob_st_no_work_cycle` | 3,216,661 | 3,009,187 | -207,474 | -6.450% |
| `int_er_rob_st_pending_work_cycle` | 22,019,359 | 16,582,686 | -5,436,673 | -24.690% |
| `int_er_rob_st_pending_global_stop_cycle` | 3,173,026 | 1,939,479 | -1,233,547 | -38.876% |
| `int_er_rob_st_pending_caught_up_after_scan_cycle` | 349,517 | 324,033 | -25,484 | -7.291% |
| `int_er_rob_st_pending_walk_width_limited_cycle` | 4,077,204 | 3,412,856 | -664,348 | -16.294% |
| `int_er_rob_st_pending_invalid_frontier_cycle` | 0 | 0 | 0 | - |
| `int_er_rob_st_pending_valid_frontier_blocker_cycle` | 14,419,612 | 10,906,318 | -3,513,294 | -24.365% |
| `int_er_rob_st_blocker_need_flush_cycle` | 443,766 | 425,114 | -18,652 | -4.203% |
| `int_er_rob_st_blocker_not_writebacked_cycle` | 13,110,511 | 9,915,864 | -3,194,647 | -24.367% |
| `int_er_rob_st_blocker_writebacked_wait_commit_cycle` | 0 | 0 | 0 | - |
| `int_er_rob_st_blocker_not_resolved_cycle` | 865,335 | 565,340 | -299,995 | -34.668% |
| `int_er_rob_st_blocker_class_scalar_load_cycle` | 5,043,502 | 3,380,750 | -1,662,752 | -32.968% |
| `int_er_rob_st_blocker_class_scalar_store_cycle` | 4,000,877 | 2,630,676 | -1,370,201 | -34.248% |
| `int_er_rob_st_blocker_class_branch_jump_cycle` | 831,146 | 846,427 | 15,281 | 1.839% |
| `int_er_rob_st_blocker_class_csr_cycle` | 1,185,952 | 1,064,798 | -121,154 | -10.216% |
| `int_er_rob_st_blocker_class_fence_cycle` | 442,536 | 423,741 | -18,795 | -4.247% |
| `int_er_rob_st_blocker_class_amo_cycle` | 146,334 | 146,462 | 128 | 0.087% |
| `int_er_rob_st_blocker_class_other_integer_cycle` | 2,197,927 | 1,836,292 | -361,635 | -16.453% |
| `int_er_rob_st_blocker_class_other_integer_reason_not_resolved_cycle` | 422,617 | 72,422 | -350,195 | -82.863% |
| `int_er_rob_st_blocker_class_other_integer_reason_not_writebacked_cycle` | 1,775,310 | 1,763,870 | -11,440 | -0.644% |
| `int_er_rob_st_blocker_class_other_integer_reason_need_flush_cycle` | 0 | 0 | 0 | - |
| `int_er_rob_st_blocker_class_other_integer_reason_writebacked_wait_commit_cycle` | 0 | 0 | 0 | - |

## Free-list, UCA, and IPC Trends

| Counter | Task34 pre-WB baseline | Current | Delta | Delta / baseline |
| --- | --- | --- | --- | --- |
| `int_er_rename_int_freelist_stall_cycle` | 229,860 | 202,395 | -27,465 | -11.949% |
| `int_er_me_freelist_free_reg_sum` | 3,456,699,969 | 2,809,241,932 | -647,458,037 | -18.731% |
| `int_er_uc_early_free` | 6,707,554 | 5,904,283 | -803,271 | -11.976% |
| `int_er_uc_early_free_eligible_all` | 6,707,554 | 5,904,283 | -803,271 | -11.976% |
| `int_er_uc_early_free_deferred_width` | 0 | 0 | 0 | - |
| `int_er_uc_commit_suppress` | 6,707,478 | 5,904,232 | -803,246 | -11.975% |
| `int_er_uc_released_reused_before_commit` | 653,203 | 656,069 | 2,866 | 0.439% |
| `int_er_uc_released_unreused_at_commit` | 6,054,275 | 5,248,163 | -806,112 | -13.315% |

## Debug Protocol

本任务未产生系统级失败 debug 记录；没有 `mydebug/new-er/records/*other-integer-wb-resolved*.md`。

## Post-Matrix Local Regression Evidence

完整 emu-basic matrix 通过后，重新复跑了本任务直接影响的 Chisel regression 和 difftest preprocess：

| Check | Result |
| --- | --- |
| `mill -i xiangshan.test.testOnly xiangshan.backend.IntSparseUCATest xiangshan.backend.IntEarlyReleaseBundlesTest xiangshan.backend.IntEarlyReleaseFreeListTest xiangshan.backend.IntEarlyReleaseDataPathTest xiangshan.backend.IntEarlyReleaseRobTest` | PASS: 114 tests, 5 suites, failed 0 |
| `mill -i difftest.test.testOnly difftest.PreprocessTest` | PASS: 6 tests, 1 suite, failed 0 |
| `git diff --check` | PASS |
| `python3 -m py_compile mydocs/new-er/generate_other_integer_wb_resolved_report.py` | PASS |
| `python3 mydebug/new-er/check_protocol.py` | PASS |

## Log Paths

| Workload | stdout | stderr | wave |
| --- | --- | --- | --- |
| `cputest` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/cputest/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/cputest/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/cputest` |
| `riscv-tests` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/riscv-tests/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/riscv-tests/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/riscv-tests` |
| `misc-tests` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/misc-tests/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/misc-tests/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/misc-tests` |
| `rvh-tests` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/rvh-tests/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/rvh-tests/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/rvh-tests` |
| `microbench` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/microbench/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/microbench/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/microbench` |
| `coremark` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/coremark/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/coremark/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/coremark` |
| `linux-hello-opensbi` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/linux-hello-opensbi/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/linux-hello-opensbi/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/linux-hello-opensbi` |
| `iopmp-test` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/iopmp-test/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/iopmp-test/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/iopmp-test` |
| `povray` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/povray/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/povray/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/povray` |
| `copy_and_run` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/copy_and_run/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/copy_and_run/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/copy_and_run` |
| `f16_test` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/f16_test/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/f16_test/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/f16_test` |
| `zcb-test` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/zcb-test/stdout.log` | `mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/matrix/zcb-test/stderr.log` | `/nfs/home/zengjinhong/work/k5/XiangShan/mydocs/new-er/other-integer-wb-resolved-emu-basic-20260721/waves/zcb-test` |

## Interpretation

- `int_er_rob_resolved_by_writeback` 统计实际由 final accepted writeback 提前置 resolved 的 entry。
- `int_er_rob_wb_resolve_final_candidate` 到 `resolved / blocked_need_flush / blocked_redirect_recovery` 的闭包用于确认安全拒绝路径没有漏计。
- `int_er_rob_wb_resolve_rejected_identity_reuse_raw` 不参与 accepted-candidate 闭包；它只诊断 raw writeback index 命中但完整 ROB generation/slot owner 不匹配的 stale/reuse 事件。
- `int_er_rob_interrupt_deferred_for_guard_*` 是正确性成本：存在 guard-emitted redefiner 时，ROB 延迟实际 interrupt flush，防止不可撤销 early-free 证明链被异步中断冲刷。
