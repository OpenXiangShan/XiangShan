# V2 memblock RTL 规则 profile

## 版本事实

| 项目 | V2 取值 |
|---|---|
| 构建系统 | `build.sc` |
| Mill module | `xiangshan` |
| Chisel 版本 | `6.7.0` |
| firtool 来源 | `build.sc` 通过 `chisel3.BuildInfo.firtoolVersion` 解析 |
| V2 中已找到的默认顶层入口 | `top.TopMain` |
| 源码中已找到的 V2 config | `KunminghuV2Config` |
| 已有 MemBlock class | `xiangshan.mem.MemBlock` |
| 已有独立 MemBlock main | 本次迁移前不存在 |

## RTL 生成方向

V2 不得直接复用以下 V3-only 假设：

```text
top.MemBlockTopMain
TLConfig
Chisel 7.3.0
firtool 1.135.0 as a hard requirement
build.mill
```

V2 RTL flow 应使用 `build.sc`、Chisel 6.7.0，以及 V2 专用 MemBlock
harness/main。默认 config 为 `KunminghuV2Config`；只有后续 V2 验证明
确需要更小 config 时，才允许调整。

## 期望产物

mem_ut 环境仍期望以下产物：

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

如果 V2 生成得到不同但等价的文件名，必须在同一次变更中更新脚本和
`mem_ut/ver/ut/memblock/cfg/rtl.f`，并在本 profile 记录映射关系。

## UVM RTL 路径

V2 使用独立 worktree，因此 `mem_ut/ver/ut/memblock/cfg/rtl.f` 不得硬编码
`$MEMBLOCK_PROJECT/XiangShan/...`。

当前 V2 规则为：

```text
-F $MEMBLOCK_XS_HOME/build_memblock/rtl/filelist.f
```

`scripts/generate_memblock_rtl.sh` 和 `mem_ut/ver/ut/memblock/sim/eda01_entry.sh`
默认将 `MEMBLOCK_XS_HOME` 导出为当前 V2 XiangShan worktree 根目录。

## 当前状态

当前 worktree 在修复 submodule 并迁移独立 `MemBlockTop` wrapper 后，V2 RTL
生成已通过。生成结果和历史失败细节记录在：

```text
AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md
```
