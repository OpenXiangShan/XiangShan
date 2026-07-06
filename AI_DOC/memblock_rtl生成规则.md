# memblock RTL 生成规则

## 目的

本文档用于说明如何在当前 XiangShan 工作区生成 `memblock` RTL。

不同 XiangShan 版本的 Chisel、构建系统、main class、config、firtool 和输出产物
可能不同。本文只保存通用流程；版本差异必须从当前 profile 读取：

```text
mem_ut/ver/ut/memblock/rule/version/v2/memblock_rtl_profile.md
mem_ut/ver/ut/memblock/rule/version/v3/memblock_rtl_profile.md
```

## Profile 选择

处理 RTL 生成前必须先确定版本：

- 当前分支为 `mem_ut_uvm` 时，默认使用 V3 profile。
- 当前分支为 `mem_ut_uvm_v2` 时，默认使用 V2 profile。
- 用户显式指定 V2 或 V3 时，以用户指定版本为准。
- 当前分支与用户显式指定版本冲突时，停止并向用户确认。

## 推荐命令

从 XiangShan 仓库任意目录执行：

```bash
scripts/generate_memblock_rtl.sh
```

脚本必须自动定位仓库根目录，并按当前版本 profile 选择默认构建入口。

## 参数覆盖

脚本应支持通过环境变量覆盖默认值。通用变量包括：

- `TARGET_DIR`：RTL 输出目录，默认 `build_memblock/rtl`
- `CONFIG`：生成使用的 config，默认值由版本 profile 定义
- `JVM_XMX`、`JVM_XSS`：JVM 参数
- `MEMBLOCK_FIRTOOL` 或 `FIRTOOL`：firtool 覆盖入口，如该版本需要外部 firtool

具体支持的额外变量以当前版本 profile 和脚本实现为准。

## firtool 版本规则

不得跨版本硬套 firtool：

- V3 profile 已验证 Chisel 7.3.0 与 firtool 1.135.0。
- V2 profile 基于 build.sc、Chisel 6.7.0 和 build.sc 的 firtool resolver。

如果生成失败，应先确认当前 profile 的 firtool 来源、Chisel 版本、main class 和
config，而不是直接回退到共享旧版 firtool。

## 输出与验证标准

生成成功后必须检查当前 profile 定义的产物。默认 mem_ut 期望：

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

如果某版本使用等价产物或生成路径不同，必须在对应 profile 中明确记录，并同步
`mem_ut/ver/ut/memblock/cfg/rtl.f` 的引用规则。

## MEMBLOCK_PROJECT 规则

`mem_ut/ver/ut/memblock/cfg/rtl.f` 通常引用：

```text
-F $MEMBLOCK_PROJECT/XiangShan/build_memblock/rtl/filelist.f
```

因此默认 `MEMBLOCK_PROJECT` 必须指向 `XiangShan` 的上一级目录，而不是
`XiangShan` 本身。如果 V2 worktree 使用不同目录名或 symlink，必须在 V2 profile
中明确本地和 eda01 看到的路径。

## 与远端编译仿真的关系

本规则只负责生成 `build_memblock/rtl`。

生成 RTL 后，如需验证 UVM 编译仿真，继续按：

- `AI_DOC/mem_ut远端编译仿真方案与流程.md`

从 `mem_ut/ver/ut/memblock/sim` 使用 `make eda_compile ...` 等远端目标。
