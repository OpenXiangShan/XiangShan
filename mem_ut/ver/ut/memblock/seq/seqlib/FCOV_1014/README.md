# 功能覆盖率专项 cfg

按用户指定，2026-10-14 将 `seqlib` 下原有的 8 个功能覆盖率配置目录合并到 `seq/seqlib/FCOV_1014`，共保留 54 个 cfg。第六轮与第十轮 StoreUnit attribute 的 3 组同名 cfg 有效配置项完全一致，仅注释不同，因此各保留第十轮版本。

通过现有 Makefile 的 `plus_file=../seq/seqlib/FCOV_1014` 指定此目录。下文保留第一轮归档时的历史说明：第一轮四个 cfg 对应四个用例，均复用 basicTest + memblock_dispatch_real_smoke_vseq。

第一轮（2026-09-28）配置已冻结：

| cfg | 主表配置数量 | 运行 seed |
|---|---:|---|
| fcov_load_boundaries_v1 | 64 | 1101、1102 |
| fcov_store_boundaries_v1 | 64 | 2101、2102 |
| fcov_mixed_reuse_v1 | 96 | 3101、3102 |
| fcov_prefetch_v1 | 16 | 4101、4102 |

从 mem_ut/ver/ut/memblock/sim 运行，例如：

```bash
make eda_batch_run REMOTE_HOST=172.28.10.101 \
  mode=report_cov_20260924 tc=basicTest \
  ts=memblock_dispatch_real_smoke_vseq \
  plus_file=../seq/seqlib/FCOV_1014 cfg=fcov_load_boundaries_v1 \
  seed=1101 ccov=on fcov=on wave=off udr=-covg_dump_range
```

该命令复用 9 月 24 日 simv。已有运行不得直接覆盖原 seed 的数据库；复现请用新产物目录并保存配置/编译来源。

未显式指定的 key 使用 plus.sv 编译默认值，不会自动继承 plus_cfg/default.cfg。第一轮继承随机 PTE 权限、PMA_PMP_MODEL_EN=0、HARD_XZ_CHECK_EN=0；成功用例结论受这些检查范围限制。每个配置的实际结果、通过清单、截图、未覆盖仓及后续方案位于：

/nfs/home/wangyan/Mem_env_work_20260716/Mem_930_report/functioncov/01_round1

第二轮配置尚未创建或运行，待用户审核 round2_proposal.md 后再进行。
