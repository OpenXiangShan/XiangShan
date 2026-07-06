# V2 verified status

## Current Status

| Item | Status |
|---|---|
| Branch created from latest `origin/kunminghu-v2` | Done at `2acbf327cf7fb514593acc00d4c41117ec499e08` |
| Rule/profile routing | Done by `8003cc084 docs(mem_ut): add versioned v2 v3 rule profiles` |
| V2 RTL generation flow | Added by `cead7d6fc build(memblock): add v2 rtl generation flow` |
| V2 RTL generation result | Passed after submodule repair and V2 standalone wrapper migration; see `AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md` |
| mem_ut base environment port | Done by `d555ee14a mem_ut: port base environment to v2 rtl path` |
| V2 DUT agent/interface adaptation | Not part of this migration plan |
| Remote VCS compile | Pending/expected to expose V2 DUT interface deltas before the dedicated adaptation plan |

## Verification Standard

After V2 RTL generation succeeds and the base mem_ut environment is available,
first compile target:

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

`tc_sanity` runtime pass is not required before the dedicated V2 DUT adaptation
plan.
