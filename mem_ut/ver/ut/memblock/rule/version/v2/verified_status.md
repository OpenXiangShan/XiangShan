# V2 已验证状态

## 当前状态

| 项目 | 状态 |
|---|---|
| 从最新 `origin/kunminghu-v2` 创建分支 | 已在 `2acbf327cf7fb514593acc00d4c41117ec499e08` 完成 |
| 规则/profile 路由 | 已由 `8003cc084 docs(mem_ut): add versioned v2 v3 rule profiles` 完成 |
| V2 RTL 生成 flow | 已由 `cead7d6fc build(memblock): add v2 rtl generation flow` 添加 |
| V2 RTL 生成结果 | 修复 submodule 并迁移 V2 独立 wrapper 后已通过，见 `AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md` |
| mem_ut 基础环境迁移 | 已由 `d555ee14a mem_ut: port base environment to v2 rtl path` 完成 |
| V2 DUT agent/interface 适配 | 不属于本迁移 plan 范围 |
| 远端 VCS 编译 | 待执行，预期会在专项适配 plan 前暴露 V2 DUT interface delta |

## 验证标准

V2 RTL 生成成功且基础 mem_ut 环境可用后，优先编译以下目标：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

专项 V2 DUT 适配 plan 完成前，不要求 `tc_sanity` runtime pass。
