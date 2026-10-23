# memblock functional coverage default issue enable 计划 Review

## 1. 范围与术语

关联计划：
`AI_DOC/plan/test_framework/plan/do/memblock_functional_default_issue_enable_plan_20261023.md`

reviewer：独立只读 Codex reviewer；日期：`2026-10-23`。

| 术语 | 中文含义 | 代码落点 |
|---|---|---|
| `coverage row` | runner 中一个 cfg/testcase/VSEQ 组合 | `CASES` |
| `override` | 优先于 cfg 同名值的 runtime plusarg | runner `args` |
| `issue driver` | 从公共 issue queue 选择请求并驱动 DUT 的 sequence | `memblock_issue_dispatch_base_sequence` |

## 2. 责任分类 Review

确认属于测试框架 runner 的 case 编排错误，不是 RTL 或 RM。`default.cfg` 合法地关闭主动 issue；错误在于
runner 将该基线与必须完成真实 issue/commit/terminal 闭环的 testcase 组合，却未添加场景级 enable。

五个 seed 都出现 issue sequence idle、主表 1000 项和 `terminal_done_uid=0`。DUT 未收到预期 issue，
因此不能把无进展归责于 RTL；RM 也没有错误预测或比较。

## 3. 方案 Review

方案通过，要求在精确三元组：

```text
default + tc_dispatch_real_smoke + virtual_base_sequence
```

追加 `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1`。Makefile 已支持命令行同名参数覆盖 cfg 值，最终 effective
plusargs 只会保留一个 issue enable。

禁止修改公共 `default.cfg`，禁止复制新增 cfg，禁止修改 RTL、RM、testcase、sequence 或 terminal done
条件。该修改仅恢复 coverage row 必需的真实 issue 激励，不改变 transaction 内容和协议。

## 4. 验证门禁

- runner shell syntax PASS；
- default row effective plusargs 唯一值为 issue enable 1；
- 原 5 个 seed 5/5 PASS；
- 新随机 seed 至少 10/10 PASS；
- 最终 coverage row 生成有效 VDB。

## 5. Review 结论

未发现方案 blocker。最小且正确的责任层是 functional coverage runner 的精确 per-case override。
