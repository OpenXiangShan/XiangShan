# mem_ut V2 branch policy

## Scope

This profile applies to the `mem_ut_uvm_v2` branch and V2 design work based on
`kunminghu-v2`.

## Branch Mapping

| Item | Value |
|---|---|
| mem_ut branch | `mem_ut_uvm_v2` |
| upstream design branch | `origin/kunminghu-v2` |
| current V2 base commit | `2acbf327cf7fb514593acc00d4c41117ec499e08` |
| base commit message | ``fix(MemBlock): `vSegmentUnit` needs to connect `ready` to `IQ` (#6123)`` |

## Update Flow

After confirming `git status` is clean:

```bash
git fetch origin kunminghu-v2
git rebase FETCH_HEAD
```

Design source, RTL source and build source conflicts must be resolved to the V2
side unless a later V2-specific mem_ut plan explicitly documents the exception.

## Non-goals

- Do not rebase this branch to `origin/kunminghu-v3`.
- Do not use V3 design source to resolve V2 design conflicts.
- Do not push automatically.
