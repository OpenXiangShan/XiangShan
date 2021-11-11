# 香山

香山是一款开源的高性能 RISC-V 处理器。采用 Chisel 硬件设计语言开发，支持 RV64GC 指令集。

详细文档将在未来放出。
English Readme is [here](README.md).

©2020-2021 中国科学院计算技术研究所版权所有。

## 文档和报告

在 2021 年 6 月的 RISC-V 中国峰会上，我们给出了超过 20 个技术报告。报告已经更新到[我们的文档仓库](https://github.com/OpenXiangShan/XiangShan-doc)。

更多的文档也将持续更新到相同的仓库。

## 关注我们

Wechat/微信：香山开源处理器

<div align=left><img width="340" height="117" src="images/wechat.png"/></div>

Zhihu/知乎：[香山开源处理器](https://www.zhihu.com/people/openxiangshan)

Weibo/微博：[香山开源处理器](https://weibo.com/u/7706264932)

可以通过[我们的邮件列表](mailto:xiangshan-all@ict.ac.cn)联系我们。列表中的所有邮件会存档到[这里](https://www.mail-archive.com/xiangshan-all@ict.ac.cn/)。

## 处理器架构

自 2020 年 6 月开始开发的[雁栖湖](https://github.com/OpenXiangShan/XiangShan/tree/yanqihu)为香山处理器的首个稳定的微架构。目前版本的香山（即南湖）正在 master 分支上不断开发中。

微架构概览：
![xs-arch-single](images/xs-arch-simple.svg)

## 目录概览

以下是一些关键目录：

```
.
├── fpga                   # 支持的 FPGA 开发板、用于构建 Vivado 项目的文件
├── read-to-run            # 预建的仿真镜像文件
├── scripts                # 用于敏捷开发的脚本文件
└── src
    ├── test               # 测试文件（包括差异测试（diff-test）和模块测试（module-test） 等）
    └── main/scala         # 设计文件
        ├── bus/tilelink   # tilelink 实用工具
        ├── device         # 用于仿真的虚拟设备
        ├── difftest       # chisel 差异测试接口
        ├── system         # SoC 封装
        ├── top            # 顶层模块
        ├── utils          # 复用封装
        ├── xiangshan      # 主体设计代码
        └── xstransforms   # 一些实用的 firrtl 变换代码
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

## 致谢

在香山的开发过程中，我们采用了来自开源社区的子模块。具体情况如下：

| 子模块         | 来源                                                       | 详细用途                                                       |
| ------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| L2 Cache/LLC       | [Sifive block-inclusivecache](https://github.com/ucb-bar/block-inclusivecache-sifive) | 我们增强了原模块的功能和时序，最终使之能胜任 L2/LLC 任务的缓存生成器 |
| Diplomacy/TileLink | [Rocket-chip](https://github.com/chipsalliance/rocket-chip)  | 我们复用了来自 rocket-chip 的外接框架和链接，来调度总线 |
| FPU                | [Berkeley hardfloat](https://github.com/ucb-bar/berkeley-hardfloat) | 我们使用了 Barkeley-hardfloat 作为浮点运算器并为之设计了 SRT-4 除法/开方运算单元。此外我们分割了 FMA 流水线以优化时序 |

我们深深地感谢来自开源社区的支持，我们也鼓励其他开源项目在[木兰宽松许可证](LICENSE)的范围下复用我们的代码。:)
