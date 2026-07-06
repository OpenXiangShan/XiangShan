# mem_ut V2 分支策略

## 适用范围

本 profile 适用于 `mem_ut_uvm_v2` 分支，以及基于 `kunminghu-v2` 的 V2 设计同步工作。

## 分支映射

| 项目 | 取值 |
|---|---|
| mem_ut 工作分支 | `mem_ut_uvm_v2` |
| 上游设计分支 | `origin/kunminghu-v2` |
| 当前 V2 基线 commit | `2acbf327cf7fb514593acc00d4c41117ec499e08` |
| 基线 commit message | ``fix(MemBlock): `vSegmentUnit` needs to connect `ready` to `IQ` (#6123)`` |

## 更新流程

确认 `git status` 干净后，按以下命令更新：

```bash
git fetch origin kunminghu-v2
git rebase FETCH_HEAD
```

如果设计源码、RTL 生成源码或构建源码发生冲突，默认按 V2 侧语义解决；只有后续
V2 专项 mem_ut plan 明确记录例外时，才允许采用其他处理方式。

## 非目标

- 不要把本分支 rebase 到 `origin/kunminghu-v3`。
- 不要使用 V3 设计源码解决 V2 设计冲突。
- 不要自动 push。
