# mem_ut V3 分支策略

## 适用范围

本 profile 适用于 `mem_ut_uvm` 分支，以及基于 `kunminghu-v3` 的 V3 设计同步工作。

## 分支映射

| 项目 | 取值 |
|---|---|
| mem_ut 工作分支 | `mem_ut_uvm` |
| 上游设计分支 | `origin/kunminghu-v3` |
| 已记录 V3 mem_ut 基线 | `1f96d06acbd75f00d619885ca27155810f72d922` |
| 基线 commit message | `feat: add memblock virtual sequence dispatch` |

## 更新流程

确认 `git status` 干净后，按以下命令更新：

```bash
git fetch origin kunminghu-v3
git rebase FETCH_HEAD
```

## 非目标

- 不要把 `mem_ut_uvm` rebase 到 `origin/kunminghu-v2`。
- 不要把无关 XiangShan 脏改动混入 mem_ut commit。
- 不要自动 push。
