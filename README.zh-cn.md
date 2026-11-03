# 香山

香山（XiangShan）是一款开源的高性能 RISC-V 处理器。

[English](README.md) | 中文

## 处理器架构

![xs-arch-kunminghu](images/xs-arch-kunminghu.svg)

## 论文发表情况

### MICRO 2022: Towards Developing High Performance RISC-V Processors Using Agile Methodology

我们在 MICRO'22 会议上的论文介绍了香山处理器及敏捷开发实践经验，包括一些面向设计、功能验证、调试、性能评估等方面的敏捷开发工具。论文得到了 Artifact Evaluation 的所有三个徽章。

![Artifacts Available](https://talks-pubs.xiangshan.cc/publications/images/artifacts_available_dl.jpg)
![Artifacts Evaluated — Functional](https://talks-pubs.xiangshan.cc/publications/images/artifacts_evaluated_functional_dl.jpg)
![Results Reproduced](https://talks-pubs.xiangshan.cc/publications/images/results_reproduced_dl.jpg)

[Paper PDF](https://talks-pubs.xiangshan.cc/publications/micro2022-xiangshan.pdf) | [IEEE Xplore](https://ieeexplore.ieee.org/abstract/document/9923860) | [BibTeX](https://talks-pubs.xiangshan.cc/publications/micro2022-xiangshan.bib) | [Presentation Slides](https://talks-pubs.xiangshan.cc/publications/micro2022-xiangshan-slides.pdf) | [Presentation Video](https://www.bilibili.com/video/BV1FB4y1j7Jy)

[更多论文](https://docs.xiangshan.cc/zh-cn/latest/tutorials/publications/)

## 分支维护情况

| 代际-版本 | 分支                                                                                    | 维护?        | 开发?        |
| --------- | --------------------------------------------------------------------------------------- | ------------ | ------------ |
| 雁栖湖    | [yanqihu](https://github.com/OpenXiangShan/XiangShan/tree/yanqihu)                      | ✅(minimum)  |              |
| 南湖-V2   | [nanhu](https://github.com/OpenXiangShan/XiangShan/tree/nanhu)                          | ✅(minimum)  |              |
| 南湖-V3   | [Siudya/Nanhu](https://github.com/Siudya/Nanhu/tree/main)                               |              |              |
| 南湖-V5   | [OpenXiangShan-Nanhu/OpenNanhu-V5](https://github.com/OpenXiangShan-Nanhu/OpenNanhu-V5) | ✅           | ✅           |
| 昆明湖-V2 | [kunminghu-v2](https://github.com/OpenXiangShan/XiangShan/tree/kunminghu-v2)            | ✅           |              |
| 昆明湖-V3 | [kunminghu-v3](https://github.com/OpenXiangShan/XiangShan/tree/kunminghu-v3)            | ✅           | ✅           |

目前昆明湖 V3 仍处于快速迭代阶段，功能不稳定，如您希望基于香山进行研究、验证或应用，建议优先考虑昆明湖 V2。

本节内容时效性较强，如果您认为内容可能过时，欢迎在 issue 中提出，我们会尽快更新。

更新日期：2026/06/30

## 目录概览

以下是一些关键目录：

```
.
├── src
│   └── main/scala         # 设计文件
│       ├── device         # 用于仿真的虚拟设备
│       ├── system         # SoC 封装
│       ├── top            # 顶层模块
│       ├── utils          # 复用封装
│       └── xiangshan      # 主体设计代码
│           └── transforms # 一些实用的 firrtl 变换代码
├── scripts                # 用于敏捷开发的脚本文件
├── yunsuan                # 香山运算子模块
├── XSCache                # 香山缓存子系统子模块
├── difftest               # 香山协同仿真框架
└── read-to-run            # 预建的仿真镜像文件
```

## 开发

### 前置依赖

裸机、Nix devshell 和 Docker 的环境搭建、依赖安装与环境变量设置，请参考 [OpenXiangShan/xs-env](https://github.com/OpenXiangShan/xs-env)。在 xs-env 仓库根目录执行 `source ./env.sh` 可配置项目路径；`nix develop` 也会自动加载该脚本。

一句话说明：安装 Java、Mill、Clang 和仿真器（Verilator 或 GSIM），克隆 xs-env 或直接克隆本仓库，然后运行 `make init` 初始化子模块。

#### 计算资源

构建香山需要较多的 CPU 和内存资源。例如，`kunminghu-v3` 的 `DefaultConfig` 需要 **128 GB 内存**；**64 GB 内存 + 64 GB swap，并启用 zswap** 也可构建，但稍慢。`-j` 控制 Clang 编译的并行度，但单线程的 Verilator/GSIM 模型生成阶段仍需 **约 100 GB 内存**，这是降低 `-j` 无法绕过的硬性下限。

### IDE 支持

BSP: `make bsp`

IntelliJ IDEA:

- 2026.1 及更新版本: `make bsp`，随后请参考 [官方文档](https://www.jetbrains.com/help/idea/bsp-support.html)
- 旧版: `make idea`

### 环境准备

- 设定环境变量 `NOOP_HOME` 为香山工程文件夹的绝对路径，例如 `export NOOP_HOME=/path/to/XiangShan`。这是香山本身唯一必需的项目路径环境变量。
- 可选：自行编译 NEMU 作为 difftest 参考模型时，将 `NEMU_HOME` 设为 [OpenXiangShan/NEMU](https://github.com/OpenXiangShan/NEMU) 的绝对路径。通过 `--diff` 使用 `ready-to-run/` 提供的预编译参考模型库时，无需设置此变量。
- 可选：编译自己的测试用例时，将 `AM_HOME` 设为 [OpenXiangShan/nexus-am](https://github.com/OpenXiangShan/nexus-am) 的绝对路径。运行预编译测试程序时，无需设置此变量。

### 生成 Verilog

- 安装 Java 和 Mill。
- 运行 `make verilog` 以生成 Verilog 代码。该命令会在 `build/rtl/` 目录下生成多个 `.sv` 文件（例如 `build/rtl/XSTop.sv`）。
- 更多信息详见 `Makefile`。

### 使用仿真器构建与运行

- 安装开源 Verilog 仿真器 [Verilator](https://verilator.org/guide/latest/)，或 [OpenXiangShan/GSIM](https://github.com/OpenXiangShan/GSIM)。请按对应仓库的说明安装，并确保工具位于 `PATH` 中。
- 使用 Verilator 时，运行 `make emu` 构建 C++ 仿真器 `./build/emu`。
- 使用 GSIM 时，运行 `make gsim GSIM=1 -j10` 构建 `./build/emu`。请保留 `GSIM=1`，以便在生成硬件时选择 GSIM 的 difftest 配置。注意 GSIM 需要 `clang++` 19 或更新版本。
- 推荐使用 `numactl` 将仿真进程绑定到同一 NUMA 节点的物理核心及内存；参考 [`scripts/xiangshan.py`](scripts/xiangshan.py)（`--numa`、`--threads`）。
- 运行 `./build/emu --help` 可以获得仿真器的各种运行时参数。
- 更多细节详见 `Makefile`、`difftest/verilator.mk` 与 `difftest/gsim.mk`。

选择一条构建命令，然后运行测试程序：

```bash
make emu CONFIG=DefaultConfig EMU_THREADS=2 -j10  # Verilator
# 或者：make gsim GSIM=1 CONFIG=DefaultConfig -j10
./build/emu -b 0 -e 0 -i ./ready-to-run/coremark-2-iteration.bin --diff ./ready-to-run/riscv64-nemu-interpreter-so
```

### 测试

推荐通过仿真器运行测试程序：使用 [nexus-am](https://github.com/OpenXiangShan/nexus-am) 编写有针对性的回归用例，或运行 `ready-to-run/` 提供的 CoreMark 和 Linux 等二进制程序。编译自己的测试程序时，请设置 `AM_HOME` 并参考 nexus-am 的说明，例如在测试或应用目录中运行 `make ARCH=riscv64-xs`。

使用上述任一仿真器构建 `emu` 后，可使用相同的预编译 difftest 参考模型运行 Linux 冒烟测试：

```bash
./build/emu -i ./ready-to-run/linux.bin --diff ./ready-to-run/riscv64-nemu-interpreter-so
```

`src/test/scala/` 中的 ScalaTest/Chisel 单元测试是另一种测试方式。运行 `mill -i xiangshan.test.test` 执行测试，或运行 `mill -i xiangshan.test.testOnly xiangshan.frontend.bpu.SaturateCounterTest` 选择特定测试类。

### 运行 xspdb

有两种办法运行 xspdb：

1. 使用预编译二进制文件快速启动
    1. 从仓库 [Actions](https://github.com/OpenXiangShan/XiangShan/actions/workflows/release.yml?query=is%3Asuccess+event%3Apush) 工作流的运行中获取最新的 XSPdb。
    2. 解压并运行。

2. 从源码构建
    1. 安装多语言芯片验证辅助工具 [picker](https://github.com/XS-MLVP/picker)。
    2. 运行 `make pdb`，使用 picker 构建香山的二进制版本。
    3. 运行 `make pdb-run`，启动香山二进制版本。

示例输出与交互：

```bash
$ make pdb-run
[Info] Set PMEM_BASE to 0x80000000 (Current: 0x80000000)
[Info] Set FIRST_INST_ADDRESS to 0x80000000 (Current: 0x80000000)
Using simulated 32768B flash
[Info] reset dut complete
> XiangShan/scripts/pdb-run.py(13)run()
-> while True:
(XiangShan) xload ready-to-run/microbench.bin   # 加载二进制文件（支持 Tab 补全）
(XiangShan) xwatch_commit_pc 0x80000004         # 设置观察点
(XiangShan) xistep 3                            # 执行到接下来三条指令提交；若命中观察点则提前停止
[Info] Find break point (Inst commit), break (step 2107 cycles) at cycle: 2207 (0x89f)
[Info] Find break point (Inst commit, Target commit), break (step 2108 cycles) at cycle: 2208 (0x8a0)
(XiangShan) xpc                                 # 打印 PC 信息
PC[0]: 0x80000000    Instr: 0x00000093
PC[1]: 0x80000004    Instr: 0x00000113
PC[2]: 0x0    Instr: 0x0
...
PC[7]: 0x0    Instr: 0x0
(XiangShan) xistep 1000000                      # 执行至程序结束
[Info] Find break point (Inst commit), break (step 2037 cycles) at cycle: 2207 (0x89f)
[Info] Find break point (Inst commit), break (step 2180 cycles) at cycle: 2207 (0x89f)
...
HIT GOOD LOOP at pc = 0xf0001cb0
```

## 贡献

请务必在提交 issue 或 PR 之前阅读 [贡献指南](CONTRIBUTING.zh-cn.md)，尤其是[部分已知问题说明](CONTRIBUTING.zh-cn.md#部分已知问题说明)部分，并使用正确的 issue 模板。

## 致谢

香山处理器是绝佳的微架构学术研究平台，能够充分支持学术界的试验创新想法。香山处理器中已经实现或参考借鉴了如下论文，列举如下：[致谢](https://docs.xiangshan.cc/zh-cn/latest/acknowledgments/)。我们非常鼓励并期待，未来能够基于香山处理器实现更多的学术创新。

## 文档和报告

- 主文档站点：[网站](https://docs.xiangshan.cc) | [Github 仓库](https://github.com/OpenXiangShan/XiangShan-doc)
- 设计文档（面向硬件开发者）：[网站](https://docs.xiangshan.cc/projects/design/) | [Github Release](https://github.com/OpenXiangShan/XiangShan-Design-Doc/releases)
- 用户指南（面向软件开发者）：[网站](https://docs.xiangshan.cc/projects/user-guide/) | [Github Release](https://github.com/OpenXiangShan/XiangShan-User-Guide/releases)
- 性能看板：[网站](https://dashboard.xiangshan.cc) | [Github 仓库](https://github.com/OpenXiangShan/XiangShan-Dashboard)
- 论文发表：[网站](https://docs.xiangshan.cc/zh-cn/latest/tutorials/publications/)
- 致谢：[网站](https://docs.xiangshan.cc/zh-cn/latest/acknowledgments/)
- 会议报告：[网站](https://docs.xiangshan.cc/zh-cn/latest/tutorials/talks/)

所有香山文档均采用 CC-BY-4.0 协议授权。

我们正在使用 [Weblate](https://hosted.weblate.org/projects/openxiangshan/) 将本项目文档翻译为英文及其他语言。欢迎大家参与翻译工作，帮助我们一起完善文档！

<a href="https://www.star-history.com/?repos=openxiangshan%2Fxiangshan&type=date&legend=top-left">
 <picture>
   <source media="(prefers-color-scheme: dark)" srcset="https://api.star-history.com/chart?repos=openxiangshan/xiangshan&type=date&theme=dark&legend=top-left" />
   <source media="(prefers-color-scheme: light)" srcset="https://api.star-history.com/chart?repos=openxiangshan/xiangshan&type=date&legend=top-left" />
   <img alt="Star History Chart" src="https://api.star-history.com/chart?repos=openxiangshan/xiangshan&type=date&legend=top-left" />
 </picture>
</a>

## 关注我们

Wechat/微信：香山开源处理器

<div align=left><img width="340" height="117" src="images/wechat.png"/></div>

Zhihu/知乎：[香山开源处理器](https://www.zhihu.com/people/openxiangshan)

Weibo/微博：[香山开源处理器](https://weibo.com/u/7706264932)

可以通过[我们的邮件列表](mailto:xiangshan-all@ict.ac.cn)联系我们。列表中的所有邮件会存档到[这里](https://www.mail-archive.com/xiangshan-all@ict.ac.cn/)。

## 许可证

版权所有 © 2020-2026 中国科学院计算技术研究所。

版权所有 © 2021-2026 北京开源芯片研究院。

版权所有 © 2020-2022 鹏城实验室。

香山以 [木兰宽松许可证 第2版](LICENSE) 授权。
