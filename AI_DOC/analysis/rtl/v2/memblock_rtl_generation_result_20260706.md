# V2 memblock RTL 生成结果

## 背景

本文记录 2026-07-06 `mem_ut_uvm_v2` 迁移中的 V2 memblock RTL 生成结果。

分支上下文：

```text
worktree: /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2
branch: mem_ut_uvm_v2
base: origin/kunminghu-v2
base commit: 2acbf327cf7fb514593acc00d4c41117ec499e08
```

生成入口：

```text
scripts/generate_memblock_rtl.sh
src/main/scala/top/MemBlockTop.scala
```

V2 profile：

```text
build system: build.sc
default config: KunminghuV2Config
firtool: build.sc resolver by default
main class: top.MemBlockTopMain
expected output dir: build_memblock/rtl
```

## 历史失败记录

### Attempt 1：submodule 未初始化

命令：

```bash
./scripts/generate_memblock_rtl.sh
```

结果：

脚本在进入 mill 前停止，报告 V2 recursive submodule 未初始化，包括
`rocket-chip`、`ready-to-run`、`coupledL2`、`openLLC` 等依赖。

### Attempt 2：ready-to-run gitdir 损坏

命令：

```bash
git submodule update --init --recursive
```

结果：

`ready-to-run` 子模块的 gitdir 状态损坏，导致 recursive update 无法闭合。

### Attempt 3：build source 不完整

命令：

```bash
./scripts/generate_memblock_rtl.sh
```

结果：

脚本进入 mill 后仍失败，关键报错包括：

```text
ready-to-run/.../modules/ready-to-run is not a git repository
missing rocket-chip/cde/common.sc
missing rocket-chip/hardfloat/common.sc
missing rocket-chip/common.sc
```

该阶段失败原因归类为 V2 submodule/build-source closure 未闭合，不是 RTL
elaboration、firtool 或 UVM 编译失败。

## 修复动作

### submodule 状态修复

已执行：

```bash
git submodule deinit -f ready-to-run
proxychains git submodule update --init --recursive ready-to-run
proxychains git submodule update --init --recursive
```

修复后 `rocket-chip/common.sc`、`rocket-chip/cde/common.sc`、
`rocket-chip/hardfloat/common.sc` 均存在，`git status --short --branch`
可正常执行。

### V2 standalone wrapper 完成

`src/main/scala/top/MemBlockTop.scala` 从最初简单 clone wrapper，扩展为 V2
可 elaboration 的 standalone MemBlockTop wrapper：

- 实例仍为 V2 `xiangshan.mem.MemBlock()`，没有修改 V2 `MemBlock.scala` 内部设计逻辑。
- 补齐 frontend/backend BlackBox 边界，使 MemBlock 的 frontend/backend 交互可在 standalone 顶层闭合。
- 补齐 TL manager/client、interrupt node、prefetch bridge、PTW/DCache/Uncache 等 Diplomacy 连接。
- 按 V2 API 调整 `coreParams.icacheParameters`、BPU ctrl snake_case 字段和 `outer_cpu_halt`。
- 去除 V3-only 的 `localModulePrefix` 与 issueBlockParam 绑定逻辑。
- 对 `0x38022000/0x7f` 内部 MMIO 区间从 external uncache manager 中排除，解决地址重叠。

### V2 RTL 路径修复

V2 worktree 名为：

```text
XiangShan_V2_mem_ut_uvm_v2
```

因此不能继续让 `rtl.f` 固定引用：

```text
$MEMBLOCK_PROJECT/XiangShan/build_memblock/rtl/filelist.f
```

否则远端编译可能读到同级旧 `XiangShan` worktree 的 RTL。

当前 V2 规则改为：

```text
-F $MEMBLOCK_XS_HOME/build_memblock/rtl/filelist.f
```

`scripts/generate_memblock_rtl.sh` 与 `mem_ut/ver/ut/memblock/sim/eda01_entry.sh`
默认将 `MEMBLOCK_XS_HOME` 导出为当前 V2 XiangShan worktree 根目录。

## 成功结果

命令：

```bash
./scripts/generate_memblock_rtl.sh
```

结果：

```text
[memblock-rtl-v2] XS_HOME=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2
[memblock-rtl-v2] MEMBLOCK_XS_HOME=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2
[memblock-rtl-v2] MEMBLOCK_PROJECT=/nfs/home/lixiangrui/work/memblock_ut
[memblock-rtl-v2] config=KunminghuV2Config
[memblock-rtl-v2] target=build_memblock/rtl
[memblock-rtl-v2] firtool=build.sc resolver default
[memblock-rtl-v2] generated:
291 build_memblock/rtl/filelist.f
-rw-r--r-- ... 4.6M ... build_memblock/rtl/MemBlock.sv
-rw-r--r-- ... 2.0M ... build_memblock/rtl/MemBlockTop.sv
```

非空产物检查通过：

```bash
test -s build_memblock/rtl/filelist.f
test -s build_memblock/rtl/MemBlock.sv
test -s build_memblock/rtl/MemBlockTop.sv
```

当前生成时仍有 37 条 Chisel elaboration warning，均来自 V2 设计源码中的动态
index 宽度 warning；本轮未修改这些 V2 设计源码。

## 已观察到的后续适配点

V2 generated `MemBlockTop.sv` 已显示与当前 UVM connect 存在命名差异，例如：

```text
build_memblock/rtl/MemBlockTop.sv: outer_cpu_halt
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv: cpuWfi / io_outer_cpu_wfi
```

这证明后续必须基于 V2 generated RTL 做 DUT interface/agent 适配。该适配属于后续独立
V2 DUT adaptation plan，不在本次分支迁移和 RTL 生成能力迁移中直接完成。

## 结论

V2 RTL 生成能力已迁移成功，当前分支能够生成 V2 `build_memblock/rtl/filelist.f`、
`MemBlock.sv` 和 `MemBlockTop.sv`。

本次完成的是 V2 standalone RTL 生成入口和 mem_ut RTL 路径接入，不代表 UVM
testbench、agent、driver、monitor 已经完成 V2 DUT interface 适配。后续适配必须以本次
生成的 V2 Verilog 为权威输入。
