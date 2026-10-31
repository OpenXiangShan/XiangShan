# memblock functional coverage default issue enable 修复计划

## 1. 术语、最终稿及历史边界（2026-10-31同步）

| 术语 | 当前中文含义与落点 | 例子 |
|---|---|---|
| default cfg | 通用运行默认参数；当前允许已有issue sequence真正驱动请求 | `default.cfg`的ISSUE_SEQ_EN=1 |
| issue | 把已建立的测试请求送入DUT，不等同于仅建表或enqueue | 主表1000条必须实际发射后才能完成 |
| override | 运行参数覆盖cfg；旧方案只对矩阵一行追加的开关 | 7c阶段的单行ISSUE=1，a15已删除 |
| terminal prefix | 从UID0开始连续完成的范围 | 未issue时一直无法推进 |

**Git最终稿**：`7c3d766b847011833a6de8462cbff8585b104fc3`的单行runner override是历史方案；`a15c8e82739023e3cbbe44899aa8b2655d36b99f`删除override，并把`mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg:279`改为`MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1`。当前直接调用与矩阵均读取公共默认值，不再依赖runner例外。

抽象功能描述：默认参数选择是否启动已有issue sequence，使主动发送型测试真正发送请求；它不负责生成新payload，也不替换RM比较或完成检查。

最终行为仅改变默认issue使能，sequence、transaction随机化、终态语义、RM和RTL不改。关闭issue的调用方必须显式选择关闭值；不能保留“所有普通default调用不受影响”的旧断言。原始5＋10专项只验证旧runner方案。当前母矩阵`tc_dispatch_real_smoke / virtual_base_sequence / default`10/10通过，证据绝对目录：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_mother_integration_20261028/mem_ut/ver/ut/memblock/sim/mother_post_cancel_matrix_20261030_results/default`；它不证明全部可能default拓扑已穷尽。

未找到a15专门的事前独立方案review记录，本次不伪造批准；属于历史流程可追溯性缺口。2026-10-31仅以Git最终实现同步文档，未改源码、未补做或倒签历史验证。旧closure日志目录已删除，后文旧路径保留为历史标识；当前mother同相对路径方案/review可读。

## 2. 原方案历史记录（7c阶段，已被a15替代）

下文全部“保持default关闭”“禁止修改公共cfg”仅适用于2026-10-23方案；不是当前执行规则或当前批准结论。

### 2.1 问题摘要

functional coverage runner 的 default row 使用：

```text
default | tc_dispatch_real_smoke | virtual_base_sequence
```

runner 同时将主表 transaction 数覆盖为 1000，但 `default.cfg` 按项目规则关闭
`MEMBLOCK_DISPATCH_ISSUE_SEQ_EN`。主表被建立后没有真实 issue driver，terminal 前缀不能推进。

### 2.2 历史术语与责任边界

| 术语 | 含义 | 落点 |
|---|---|---|
| `coverage row` | runner 中一组 cfg/testcase/VSEQ 组合 | `CASES` 数组 |
| `per-case override` | 只对一个精确 row 追加的 runtime plusarg | `args` 数组 |
| `default baseline` | 普通测试使用的保守公共 cfg，主动 issue 默认关闭 | `default.cfg` |
| `terminal prefix` | 从 UID0 开始连续完成的 transaction 数 | `terminal_done_uid` |

责任属于测试框架 runner 编排。RTL 没有收到 issue 激励，RM 没有发生错误预测或比较。

### 2.3 历史修改方案

在 `run_function_cfg_cov_20261022.sh` 构建通用 `args` 后，对以下精确三元组增加条件：

```text
cfg=default
tc=tc_dispatch_real_smoke
ts=virtual_base_sequence
```

满足时追加：

```text
--plusarg +MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1
```

伪代码：

```text
先按所有 coverage row 的公共规则构造 runner 参数。
如果当前 row 正好是 default real-smoke legacy 组合，追加 issue enable override。
其它 row 不追加该参数，继续使用各自 cfg 的值。
```

### 2.4 历史方案保持不变项

- `default.cfg` 继续保持 `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0`。
- 普通 `cfg=default` 调用不受影响。
- transaction 生成、随机分布、payload 和接口协议不变。
- testcase、sequence、RM 和 RTL 不修改。
- runner 对其它 29 个 row 的公共参数不变。

该 row 的 DUT 激励会从“只 enqueue、不 issue”恢复为通过已有真实 issue sequence 驱动，这正是本修复
要补齐的闭环前置条件，不能描述为完全不改变 DUT 激励。

### 2.5 历史拒绝方案

- 不修改 `default.cfg` 全局默认值；它按规则必须保持主动 issue 默认关闭。
- 不新增复制型 cfg；当前 cfg 没有继承机制，复制默认文件会产生长期漂移。
- 不修改 transaction done 条件或让无 issue transaction 被伪完成。

### 2.6 历史验证计划

1. `bash -n` 检查 runner。
2. default row dry-run，确认 effective plusargs 中只有一个 `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1`，且
   `MAIN_TRANS_NUM=1000`、`CHECK_TRIGGER_EN=0` 不变。
3. 原 5 个 seed 保持原 tc/cfg/ts 和 1000 transaction，要求 5/5 PASS。
4. 至少 10 个新随机 seed，要求 10/10 PASS。
5. 最终完整 coverage 回归中确认该 row 产生有效 VDB 并参与 merge。
