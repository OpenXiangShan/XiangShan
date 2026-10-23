# memblock functional coverage default issue enable 修复计划

## 1. 问题摘要

functional coverage runner 的 default row 使用：

```text
default | tc_dispatch_real_smoke | virtual_base_sequence
```

runner 同时将主表 transaction 数覆盖为 1000，但 `default.cfg` 按项目规则关闭
`MEMBLOCK_DISPATCH_ISSUE_SEQ_EN`。主表被建立后没有真实 issue driver，terminal 前缀不能推进。

## 2. 术语与责任边界

| 术语 | 含义 | 落点 |
|---|---|---|
| `coverage row` | runner 中一组 cfg/testcase/VSEQ 组合 | `CASES` 数组 |
| `per-case override` | 只对一个精确 row 追加的 runtime plusarg | `args` 数组 |
| `default baseline` | 普通测试使用的保守公共 cfg，主动 issue 默认关闭 | `default.cfg` |
| `terminal prefix` | 从 UID0 开始连续完成的 transaction 数 | `terminal_done_uid` |

责任属于测试框架 runner 编排。RTL 没有收到 issue 激励，RM 没有发生错误预测或比较。

## 3. 修改方案

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

## 4. 保持不变

- `default.cfg` 继续保持 `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0`。
- 普通 `cfg=default` 调用不受影响。
- transaction 生成、随机分布、payload 和接口协议不变。
- testcase、sequence、RM 和 RTL 不修改。
- runner 对其它 29 个 row 的公共参数不变。

该 row 的 DUT 激励会从“只 enqueue、不 issue”恢复为通过已有真实 issue sequence 驱动，这正是本修复
要补齐的闭环前置条件，不能描述为完全不改变 DUT 激励。

## 5. 拒绝方案

- 不修改 `default.cfg` 全局默认值；它按规则必须保持主动 issue 默认关闭。
- 不新增复制型 cfg；当前 cfg 没有继承机制，复制默认文件会产生长期漂移。
- 不修改 transaction done 条件或让无 issue transaction 被伪完成。

## 6. 验证计划

1. `bash -n` 检查 runner。
2. default row dry-run，确认 effective plusargs 中只有一个 `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1`，且
   `MAIN_TRANS_NUM=1000`、`CHECK_TRIGGER_EN=0` 不变。
3. 原 5 个 seed 保持原 tc/cfg/ts 和 1000 transaction，要求 5/5 PASS。
4. 至少 10 个新随机 seed，要求 10/10 PASS。
5. 最终完整 coverage 回归中确认该 row 产生有效 VDB 并参与 merge。
