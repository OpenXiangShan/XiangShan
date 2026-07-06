# plus default value minimal update plan

## 1. Plan 定位

本文规划 mem_ut 测试框架 plus 默认值的最小成本修改。

本轮不做参数规则重构，不把 `default.cfg` 改为空白 cfg，而是采用更直接的方式：

```text
1. 先保存当前 default.cfg，作为历史默认配置备份。
2. 修改 plus.sv 内部默认值。
3. 同步修改 default.cfg 中会实际展开到命令行的默认 plusarg。
4. 同步相关方案文档。
5. 不修改参数管理规则。
6. 不强制修改 seq_csr_common.sv 静态 fallback 默认值。
```

目标默认行为：

```text
1. 默认不支持非对齐/跨边界场景。
2. 默认不支持 load/store 地址复用。
3. 默认只支持标量 INT load 和 store。
4. 默认不生成 FP load、prefetch、AMO、CBO。
```

本文只提供方案，不执行代码修改。

## 2. 最小修改范围

第一版功能必须修改：

```text
mem_ut/ver/ut/memblock/env/plus.sv
mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
mem_ut/ver/ut/memblock/seq/plus_cfg/default_before_plus_default_update_20260703.cfg
AI_DOC/plan/test_framework/plan/do/main_table_boundary_addr_reuse_integration_formal_plan_20260703.md
```

第一版不修改：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv
mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md
AI_DOC/mem_ut_flow_doc/main_table_boundary_profile_generation_flow.md
```

说明：

```text
seq_csr_common.sv 正常 flow 会通过 init_from_plus() 读取 plus.sv 最终值。
只要 plus.sv 和 default.cfg 同步成目标默认值，运行期默认行为即可生效。
seq_csr_common.sv 静态初始值只作为 fallback/源码一致性问题，第一版不纳入必须修改。

参数管理规则当前要求 default.cfg 提供同名默认项。
本方案继续保留 default.cfg 默认项，因此不需要修改规则文件。
```

## 3. 当前默认值生效路径

当前 Makefile 默认使用：

```text
plus_file = ../seq/plus_cfg
cfg       = default
```

因此运行时有两层默认：

```text
plus.sv：
  `MEMBLOCK_PLUS_ARGS_DEFINE(KEY, type, default_value)` 提供内部默认值。

default.cfg：
  `+KEY=VALUE` 会被 Makefile 展开成 runtime plusarg。
```

文字伪代码：

```text
仿真启动：
  Makefile 选择 cfg=default。
  读取 seq/plus_cfg/default.cfg。
  将 default.cfg 中的 +MEMBLOCK_*=VALUE 追加到 simv 命令行。

plus.sv 初始化：
  每个参数先使用 `MEMBLOCK_PLUS_ARGS_DEFINE` 内部默认值。
  如果命令行存在同名 +MEMBLOCK_*=VALUE：
    使用命令行值覆盖内部默认值。

所以：
  如果只改 plus.sv，不改 default.cfg：
    cfg=default 时仍会被 default.cfg 覆盖，目标默认值不会生效。

  如果 plus.sv 和 default.cfg 都改成目标默认值：
    cfg=default 和无 cfg/无 plusarg 路径都能得到一致默认行为。
```

## 4. default.cfg 处理方案

### 4.1 保存当前 default.cfg

先把当前 `default.cfg` 完整复制为：

```text
mem_ut/ver/ut/memblock/seq/plus_cfg/default_before_plus_default_update_20260703.cfg
```

用途：

```text
1. 保存本次修改前的完整默认配置。
2. 便于后续对比本次默认值修改范围。
3. 如需复现旧 default.cfg 行为，可显式指定：
   make eda_run tc=<tc> mode=<mode> cfg=default_before_plus_default_update_20260703
```

### 4.2 修改 default.cfg 为目标默认值

不清空 `default.cfg`，而是直接修改其中相关默认项。

必须修改：

```text
+MEMBLOCK_OP_CLASS_FP_LOAD_WT=0
+MEMBLOCK_OP_CLASS_PREFETCH_WT=0
+MEMBLOCK_OP_CLASS_AMO_WT=0
+MEMBLOCK_OP_CLASS_CBO_WT=0

+MEMBLOCK_ADDR_REUSE_EN_1_WT=0
+MEMBLOCK_ADDR_REUSE_EN_0_WT=1
```

保持或确认：

```text
+MEMBLOCK_OP_CLASS_INT_LOAD_WT=8
+MEMBLOCK_OP_CLASS_STORE_WT=6

+MEMBLOCK_BOUNDARY_PROFILE_GEN_EN=0
+MEMBLOCK_BOUNDARY_ALIGNED_WT=1
+MEMBLOCK_BOUNDARY_MISALIGN_WITHIN_8B_WT=0
+MEMBLOCK_BOUNDARY_CROSS_8B_WITHIN_16B_WT=0
+MEMBLOCK_BOUNDARY_CROSS_16B_SAME_LINE_WT=0
+MEMBLOCK_BOUNDARY_CROSS_CACHELINE_SAME_4K_WT=0
+MEMBLOCK_BOUNDARY_CROSS_4K_WT=0
```

地址复用 kind 权重第一版不改：

```text
+MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE_WT=1
+MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD_WT=0
+MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD_WT=1
+MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE_WT=0
```

原因：

```text
当前 seq_csr_common::validate_and_clamp() 要求 addr_reuse kind weights 不能全 0。
默认是否启用地址复用由 ADDR_REUSE_EN_1_WT / ADDR_REUSE_EN_0_WT 决定。
只要 EN_1=0 且 EN_0=1，默认就不会进入地址复用分支。
kind 权重保留只是为了避免 all-zero fatal，并方便 directed cfg 后续打开复用。
```

## 5. plus.sv 处理方案

`plus.sv` 内部默认值需要和 `default.cfg` 同步。

### 5.1 op_class 默认值

计划修改：

```text
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_INT_LOAD_WT, int, 8)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_FP_LOAD_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_STORE_WT, int, 6)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_PREFETCH_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_AMO_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_CBO_WT, int, 0)
```

文字伪代码：

```text
选择 op_class：
  读取 INT_LOAD/STORE/FP_LOAD/PREFETCH/AMO/CBO 权重。

  默认配置下：
    INT_LOAD 权重为 8。
    STORE 权重为 6。
    FP_LOAD/PREFETCH/AMO/CBO 权重为 0。

  weighted pick 只会选择非 0 权重候选。
  因此默认只生成标量 INT load 或 store。
```

### 5.2 boundary 默认值

计划保持：

```text
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_PROFILE_GEN_EN, bit, 1'b0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_ALIGNED_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_MISALIGN_WITHIN_8B_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_CROSS_8B_WITHIN_16B_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_CROSS_16B_SAME_LINE_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_CROSS_CACHELINE_SAME_4K_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_CROSS_4K_WT, int, 0)
```

文字伪代码：

```text
生成主表地址：
  如果 BOUNDARY_PROFILE_GEN_EN == 0：
    不进入 boundary profile 生成路径。
    使用普通合法地址模板。

  普通合法地址模板：
    生成对齐地址。
    不主动制造非对齐。
    不主动制造跨 16B、跨 cacheline 或跨 4K。
```

### 5.3 地址复用默认值

计划修改：

```text
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_EN_1_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_EN_0_WT, int, 1)
```

文字伪代码：

```text
apply_addr_reuse_window：
  sample = rand_weighted2(ADDR_REUSE_EN_1_WT, ADDR_REUSE_EN_0_WT)

  默认：
    ADDR_REUSE_EN_1_WT = 0
    ADDR_REUSE_EN_0_WT = 1

  rand_weighted2 只能命中“不复用”结果。

  如果 sample != 0：
    return。
    不选择参考 uid。
    不修改当前 transaction 类型。
    不复用参考 transaction 地址。
```

## 6. 为什么第一版不改 seq_csr_common.sv

正常运行路径：

```text
plus.sv load plusargs
seq_csr_common::init_from_plus()
seq_csr_common 从 plus.sv 读取最终值
sequence/helper 通过 seq_csr_common getter 使用参数
```

所以第一版功能生效不依赖修改 `seq_csr_common.sv` 的静态初始值。

文字伪代码：

```text
初始化阶段：
  plus.sv 得到最终参数值。
  seq_csr_common::init_from_plus() 读取 plus.sv 中的最终值。

后续 sequence 使用参数：
  不直接读 seq_csr_common 静态初始值。
  通过 getter 读取 init_from_plus() 后的值。
```

后续可选一致性补充：

```text
如果后续希望源码阅读更一致，可以单独把 seq_csr_common.sv 的静态初始值改成和 plus.sv 一致。
这不是第一版功能必须项。
```

## 7. 文档同步

第一版同步：

```text
AI_DOC/plan/test_framework/plan/do/main_table_boundary_addr_reuse_integration_formal_plan_20260703.md
```

补充内容：

```text
1. 本轮保留 default.cfg 作为默认 cfg。
2. 当前 default.cfg 修改前内容已保存到 default_before_plus_default_update_20260703.cfg。
3. plus.sv 和 default.cfg 同步承载目标默认值。
4. 默认只生成对齐标量 INT load/store。
5. 非对齐/跨界需要具名 cfg 或 plus_arg 显式打开。
6. 地址复用需要具名 cfg 或 plus_arg 显式打开 ADDR_REUSE_EN_1_WT。
```

不修改参数管理规则：

```text
mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md
```

原因：

```text
本方案仍然保留 default.cfg 提供同名默认项的模式。
因此和当前参数管理规则不冲突。
```

## 8. 验收标准

静态验收：

```text
1. default_before_plus_default_update_20260703.cfg 存在，且保存原 default.cfg 内容。
2. default.cfg 中 FP_LOAD/PREFETCH/AMO/CBO 权重为 0。
3. default.cfg 中 INT_LOAD/STORE 权重非 0。
4. default.cfg 中 ADDR_REUSE_EN_1_WT=0，ADDR_REUSE_EN_0_WT=1。
5. plus.sv 中对应默认值和 default.cfg 一致。
6. boundary 非对齐/跨边界默认权重保持 0。
7. main_table_boundary_addr_reuse_integration_formal_plan 已同步默认值策略。
```

推荐检查命令：

```bash
rg -n 'MEMBLOCK_OP_CLASS_(INT_LOAD|STORE|FP_LOAD|PREFETCH|AMO|CBO)_WT' \
  mem_ut/ver/ut/memblock/env/plus.sv \
  mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg

rg -n 'MEMBLOCK_ADDR_REUSE_EN_[01]_WT' \
  mem_ut/ver/ut/memblock/env/plus.sv \
  mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg

rg -n 'MEMBLOCK_BOUNDARY_(PROFILE_GEN_EN|MISALIGN|CROSS)' \
  mem_ut/ver/ut/memblock/env/plus.sv \
  mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg

git diff --check -- \
  mem_ut/ver/ut/memblock/env/plus.sv \
  mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg \
  mem_ut/ver/ut/memblock/seq/plus_cfg/default_before_plus_default_update_20260703.cfg \
  AI_DOC/plan/test_framework/plan/do/main_table_boundary_addr_reuse_integration_formal_plan_20260703.md
```

仿真验收：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

如果需要确认默认主表行为：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=default
```

预期：

```text
cfg=default 使用修改后的 default.cfg。
plus.sv 内部默认值和 default.cfg 保持一致。
默认主表只出现 INT_LOAD/STORE。
默认不出现 boundary 非对齐/跨界。
默认不出现地址复用。
```

## 9. 风险与后续处理

### 9.1 default.cfg 与 plus.sv 再次不一致

风险：

```text
后续有人只改 plus.sv 或只改 default.cfg，默认值再次分叉。
```

处理：

```text
本轮验收中明确要求检查 plus.sv 与 default.cfg 对应参数一致。
后续如要彻底消除该风险，可以再做 default.cfg 空白化重构。
```

### 9.2 seq_csr_common 静态默认值不一致

风险：

```text
如果存在 init_from_plus() 前读取 getter 的异常路径，可能读到 seq_csr_common.sv 的旧静态初始值。
```

处理：

```text
第一版通过仿真确认正常 flow 无问题。
后续如需降低维护风险，再单独同步 seq_csr_common.sv 静态初始值。
```

### 9.3 地址复用 kind 权重仍非 0

风险：

```text
ADDR_REUSE kind 权重仍有非 0，源码阅读时可能误以为默认支持复用。
```

处理：

```text
默认是否复用由 ADDR_REUSE_EN_1_WT/ADDR_REUSE_EN_0_WT 决定。
第一版总使能关闭即可。
kind 权重保留是为了避免现有 all-zero fatal。
```

后续可选优化：

```text
当 ADDR_REUSE_EN_1_WT == 0 时，validate_and_clamp() 不再强制检查 kind weights。
之后即可把四个 kind 权重也默认清 0。
```
