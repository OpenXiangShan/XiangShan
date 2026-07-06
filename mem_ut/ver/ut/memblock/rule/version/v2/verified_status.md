# V2 verified status

## Current Status

| Item | Status |
|---|---|
| Branch created from latest `origin/kunminghu-v2` | Done at `2acbf327cf7fb514593acc00d4c41117ec499e08` |
| Rule/profile routing | In progress in migration plan |
| V2 RTL generation | Pending until V2 script/harness is implemented |
| mem_ut base environment port | Pending |
| V2 DUT agent/interface adaptation | Not part of this migration plan |
| Remote VCS compile | Pending V2 RTL generation and base environment port |

## Verification Standard

After V2 RTL generation and base mem_ut environment migration, first compile
target:

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

`tc_sanity` runtime pass is not required before the dedicated V2 DUT adaptation
plan.
