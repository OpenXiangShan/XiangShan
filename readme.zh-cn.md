# 香山

香山（XiangShan）是一款开源的高性能 RISC-V 处理器。

English Readme is [here](README.md).

©2020-2022 中国科学院计算技术研究所版权所有

©2020-2022 鹏城实验室版权所有

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

自 2020 年 6 月开始开发的[雁栖湖](https://github.com/OpenXiangShan/XiangShan/tree/yanqihu)为香山处理器的首个稳定的微架构。

香山的第二代微架构被命名为[南湖](https://github.com/OpenXiangShan/XiangShan/tree/nanhu)。

香山的第三代微架构（昆明湖）正在 master 分支上不断开发中。

南湖微架构概览：
![xs-arch-nanhu](images/xs-arch-nanhu.svg)

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
│       ├── xiangshan      # 主体设计代码
│       └── xstransforms   # 一些实用的 firrtl 变换代码
├── scripts                # 用于敏捷开发的脚本文件
├── fudian                 # 香山浮点子模块
├── huancun                # 香山 L2/L3 缓存子模块
├── difftest               # 香山协同仿真框架
└── read-to-run            # 预建的仿真镜像文件
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

## 论文发表情况

### MICRO 2022: Towards Developing High Performance RISC-V Processors Using Agile Methodology

我们在 MICRO'22 会议上的论文介绍了香山处理器及敏捷开发实践经验，包括一些面向设计、功能验证、调试、性能评估等方面的敏捷开发工具。论文得到了 Aritifact Evaluation 的所有三个徽章。

[Paper PDF](https://github.com/OpenXiangShan/XiangShan-doc/blob/main/publications/micro2022-xiangshan.pdf) | IEEE Xplore (TBD) | ACM DL (TBD) | BibTeX (TBD)
