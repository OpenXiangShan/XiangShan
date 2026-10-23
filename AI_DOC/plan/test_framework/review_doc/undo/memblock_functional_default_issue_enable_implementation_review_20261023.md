# MemBlock functional coverage default issue enable implementation Review

## 1. Review scope

本 review 检查 coverage runner 对 `default + tc_dispatch_real_smoke + virtual_base_sequence` 的
精确 override 是否已经实现、是否误改公共 cfg，以及原始和随机专项是否完成。责任范围限定为
`run_function_cfg_cov_20261022.sh` 的 case 编排；不修改 RTL、RM、testcase 或 sequence。

关联 plan：

```text
AI_DOC/plan/test_framework/plan/do/memblock_functional_default_issue_enable_plan_20261023.md
```

实现文件：

```text
mem_ut/ver/ut/memblock/scripts/run_function_cfg_cov_20261022.sh
```

## 2. Implementation check

runner 只在以下精确三元组追加：

```text
cfg=default
tc=tc_dispatch_real_smoke
ts=virtual_base_sequence
```

追加值为：

```text
+MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1
```

公共 `default.cfg` 仍保持 `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0`。dry-run 结果中有效值只有一个
`MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1`，且保留 `MEMBLOCK_MAIN_TRANS_NUM=1000` 和
`MEMBLOCK_CHECK_TRIGGER_EN=0`。`bash -n` 通过。

## 3. Verification evidence

| 批次 | seed | 结果 |
|---|---|---|
| 原始回归 seed | `315465133,886742139,1517022454,1891003954,1990296314` | `5/5 PASS` |
| 修复后随机专项 | `1967109724,1006518208,1868321970,1531581512,2139080182,1365826704,1689799034,1565004615,1681530011,1661553988` | `10/10 PASS` |

原始结果目录：

```text
mem_ut/ver/ut/memblock/sim/default_issue_enable_original5_20261023
```

修复后随机结果目录：

```text
mem_ut/ver/ut/memblock/sim/default_issue_enable_post_std_fix_random10_r2_20261023
```

两批次均无 `UVM_ERROR/UVM_FATAL`，`remote_unknown_jobs=0`。随机专项使用 STD owner filter 修复后
重新 compile 的 simv，确认 default row 不再被 value-only STD 双候选问题中断。

另有一轮未计入结果的配置审计误跑：直接调用通用 runner 时未显式传入 issue enable，10 个 seed
全部保持 `terminal_done_uid=0`；该目录已保留但状态为 ABORTED，不属于修复后通过证据。

## 4. Review conclusion

实现满足精确 per-case override、公共 default baseline 不变、原始 seed 和随机专项门禁。

**FINAL PASS**
