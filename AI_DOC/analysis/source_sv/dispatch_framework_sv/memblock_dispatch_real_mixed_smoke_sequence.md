# memblock_dispatch_real_mixed_smoke_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_mixed_smoke_sequence.sv`

## 1. 文件定位与使用场景

真实 DUT directed load/store smoke，继承 real smoke。它存在的原因是随机 real smoke 不一定稳定覆盖 load-store 混合顺序；该 sequence 用两条 directed transaction 固定构造一个 load/store 小场景，便于 debug 接口链路。

输入是 hard-coded directed 类型；输出仍走 real smoke 的公共服务循环和 agent sequence。

- `body()` 不随机建表，而是 `build_directed_mixed_main_table()`。
- `build_directed_mixed_main_table()` 导入 load 和 store 两条手动主表。
- `make_directed_transaction()` 只支持 INT load 和 STORE。

## 2. 调度关系与参数数据流

`memblock_dispatch_real_mixed_smoke_sequence` 是 `memblock_dispatch_real_smoke_sequence` 的 directed 版本。它保留 real smoke 的 monitor 服务循环和最终检查，只替换主表来源。

调度关系：

| 阶段 | 普通 real smoke | mixed real smoke |
|---|---|---|
| 主表来源 | 按 plus 权重随机或手动表 | 直接构造 directed load/store 表 |
| 接口驱动 | 由 LSQENQ/LINTSISSUE/LSQCOMMIT/L2TLB 并行 sequence 完成 | 相同 |
| monitor/replay/commit 服务 | `service_real_dispatch_flow()` | 相同 |
| 目标 | 覆盖随机真实流 | 稳定复现 load/store 混合链路 |

参数数据流：

- directed 表覆盖了随机 op class 选择，因此 op class 权重对这两条 transaction 不起主导作用。
- 后续 LSQ 入队、TLB 建表、issue 发射、writeback、commit 仍然全部通过公共 `common_data_transaction` 和真实 agent sequence。
- 如果 testcase 额外设置 `MEMBLOCK_STD_REAL_WB_PASS_EN` 或 pipe 数限制，仍会影响后续发射和 pass 判定。

该 sequence 适合用来 debug “真实接口链路是否通”，不用于替代随机覆盖。
