# mem_ut V3 branch policy

## Scope

This profile applies to the `mem_ut_uvm` branch and V3 design work based on
`kunminghu-v3`.

## Branch Mapping

| Item | Value |
|---|---|
| mem_ut branch | `mem_ut_uvm` |
| upstream design branch | `origin/kunminghu-v3` |
| recorded V3 mem_ut baseline | `1f96d06acbd75f00d619885ca27155810f72d922` |
| baseline commit message | `feat: add memblock virtual sequence dispatch` |

## Update Flow

After confirming `git status` is clean:

```bash
git fetch origin kunminghu-v3
git rebase FETCH_HEAD
```

## Non-goals

- Do not rebase `mem_ut_uvm` to `origin/kunminghu-v2`.
- Do not mix unrelated XiangShan dirty changes into mem_ut commits.
- Do not push automatically.
