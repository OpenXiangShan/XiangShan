# 香山

香山（XiangShan）是一款开源的高性能 RISC-V 处理器。

English Readme is [here](README.md).

©2020-2022 中国科学院计算技术研究所版权所有

©2020-2022 鹏城实验室版权所有

## 香山-DSE

这个分支是为了进行香山敏捷设计空间探索（DSE），基于香山南湖-G分支。

如果你想使用香山敏捷 DSE 框架，需要运行装载到 flash 中的驱动程序，我们提供了一个 DSE 驱动程序的例子，存放在 `dse-driver` 目录下。

注意：我们的敏捷 DSE 框架需要特定的 Difftest 和 NEMU 版本。在运行之前，请自行到 NEMU 和 Difftest 仓库下切换至 `dse` 分支，并重新编译 NEMU。

你也可以把 `ready-to-run` 子仓库的分支切换成 `dse`，以使用我们提供的 NEMU so 文件。

运行示例：
```
cd dse-driver
make
cd ..
./build/emu -i $AM_HOME/apps/coremark/build/coremark-riscv64-xs.bin --diff $NEMU_HOME/build/riscv64-nemu-interpreter-so --flash ./dse-driver/build/dse.bin --dse-max-instr=100000
```

这个驱动程序可以在不同的 RobSize 配置下 `[1024, 512, 256, 128, 64, 32, 16, 8, 4, 2]` 仿真运行 workload，并且评估不同配置下的 ipc，而不需要每次重新编译。

## 文档和报告

[XiangShan-doc](https://github.com/OpenXiangShan/XiangShan-doc) 是我们的官方文档仓库，其中包含了设计文档、技术报告、使用教程等内容。

* 香山微结构文档已经发布，欢迎访问 https://xiangshan-doc.readthedocs.io

## 关注我们

Wechat/微信：香山开源处理器

<div align=left><img width="340" height="117" src="images/wechat.png"/></div>

Zhihu/知乎：[香山开源处理器](https://www.zhihu.com/people/openxiangshan)

Weibo/微博：[香山开源处理器](https://weibo.com/u/7706264932)

可以通过[我们的邮件列表](mailto:xiangshan-all@ict.ac.cn)联系我们。列表中的所有邮件会存档到[这里](https://www.mail-archive.com/xiangshan-all@ict.ac.cn/)。

## 处理器架构

自 2020 年 6 月开始开发的[雁栖湖](https://github.com/OpenXiangShan/XiangShan/tree/yanqihu)为香山处理器的首个稳定的微架构。目前版本的香山（即南湖）正在 master 分支上不断开发中。

南湖微架构概览：
![xs-arch-nanhu](images/xs-arch-nanhu.svg)

## 目录概览

以下是一些关键目录：

```
.
├── src
│   └── main/scala         # design files
│       ├── device         # virtual device for simulation
│       ├── system         # SoC wrapper
│       ├── top            # top module
│       ├── utils          # utilization code
│       ├── xiangshan      # main design code
│       └── xstransforms   # some useful firrtl transforms
├── scripts                # scripts for agile development
├── fudian                 # floating unit submodule of XiangShan
├── huancun                # L2/L3 cache submodule of XiangShan
├── difftest               # difftest co-simulation framework
├── dse-driver             # driver program for agile DSE framework
└── ready-to-run           # pre-built simulation images
```

## IDE 支持

### bsp
```
make bsp
```

### IDEA
```
make idea
```


## 生成 Verilog

* 运行 `make verilog` 以生成 verilog 代码。输出文件为 `build/XSTop.v`。
* 更多信息详见 `Makefile`。

## 仿真运行

### 环境搭建

* 设定环境变量 `NEMU_HOME` 为[香山 NEMU](https://github.com/OpenXiangShan/NEMU) 在您机器上的绝对路径。
* 设定环境变量 `NOOP_HOME` 为香山工程文件夹的绝对路径。
* 设定环境变量 `AM_HOME` 为[香山 AM](https://github.com/OpenXiangShan/nexus-am) 的绝对路径。
* 项目使用 `mill` 进行 scala 编译，因此需要安装 `mill`，详见 [mill 手动安装指南](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html#_installation)（目前仅英文版本）。
* 克隆本项目，运行 `make init` 以初始化本项目引用的开源子模块。

### 运行仿真

* 安装开源 verilog 仿真器 [Verilator](https://verilator.org/guide/latest/)。
* 运行 `make emu` 以利用 Verilator 构建 C++ 仿真器 `./build/emu`。
* 运行 `./build/emu --help` 可以获得仿真器的各种运行时参数。
* 更多细节详见 `Makefile` 与 `verilator.mk`。

运行示例：

```bash
make emu CONFIG=MinimalConfig EMU_THREADS=2 -j10
./build/emu -b 0 -e 0 -i ./ready-to-run/coremark-2-iteration.bin --diff ./ready-to-run/riscv64-nemu-interpreter-so
```

## 错误排除指南

[Troubleshooting Guide](https://github.com/OpenXiangShan/XiangShan/wiki/Troubleshooting-Guide)

## 致谢

在香山的开发过程中，我们采用了来自开源社区的子模块。具体情况如下：

| 子模块         | 来源                                                       | 详细用途                                                       |
| ------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| L2 Cache/LLC       | [Sifive block-inclusivecache](https://github.com/ucb-bar/block-inclusivecache-sifive) | 我们的新 L2/L3 缓存设计受到了 Sifive `block-inclusivecache` 的启发. |
| Diplomacy/TileLink | [Rocket-chip](https://github.com/chipsalliance/rocket-chip)  | 我们复用了来自 rocket-chip 的 Diplomacy 框架和 Tilelink 工具，来协商总线. |

我们深深地感谢来自开源社区的支持，我们也鼓励其他开源项目在[木兰宽松许可证](LICENSE)的范围下复用我们的代码。
