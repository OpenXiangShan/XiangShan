# 香山

香山（XiangShan）是一款开源的高性能 RISC-V 处理器。

English Readme is [here](README.md).

## 文档和报告

香山的文档托管在 [docs.xiangshan.cc](https://docs.xiangshan.cc)。

[docs.xiangshan.cc](https://docs.xiangshan.cc) 上的微架构文档尚未根据最新架构（昆明湖）更新，我们正在撰写新的设计文档。

香山用户文档已单独发布，您可在此处查看：[XiangShan-User-Guide/releases](https://github.com/OpenXiangShan/XiangShan-User-Guide/releases)

## 论文发表情况

### MICRO 2022: Towards Developing High Performance RISC-V Processors Using Agile Methodology

我们在 MICRO'22 会议上的论文介绍了香山处理器及敏捷开发实践经验，包括一些面向设计、功能验证、调试、性能评估等方面的敏捷开发工具。论文得到了 Artifact Evaluation 的所有三个徽章。

![Artifacts Available](https://github.com/OpenXiangShan/XiangShan-doc/raw/main/publications/images/artifacts_available_dl.jpg)
![Artifacts Evaluated — Functional](https://github.com/OpenXiangShan/XiangShan-doc/raw/main/publications/images/artifacts_evaluated_functional_dl.jpg)
![Results Reproduced](https://github.com/OpenXiangShan/XiangShan-doc/raw/main/publications/images/results_reproduced_dl.jpg)

[Paper PDF](https://github.com/OpenXiangShan/XiangShan-doc/blob/main/publications/micro2022-xiangshan.pdf) | [IEEE Xplore](https://ieeexplore.ieee.org/abstract/document/9923860) | [BibTeX](https://github.com/OpenXiangShan/XiangShan-doc/blob/main/publications/micro2022-xiangshan.bib) | [Presentation Slides](https://github.com/OpenXiangShan/XiangShan-doc/blob/main/publications/micro2022-xiangshan-slides.pdf) | [Presentation Video](https://www.bilibili.com/video/BV1FB4y1j7Jy)


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

昆明湖微架构概览：
![xs-arch-kunminghu](images/xs-arch-kunminghu.svg)

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

香山处理器是绝佳的微架构学术研究平台，能够充分支持学术界的试验创新想法。香山处理器中已经实现或参考借鉴了如下论文，列举如下：[致谢](https://docs.xiangshan.cc/zh-cn/latest/acknowledgments/)。我们非常鼓励并期待，未来能够基于香山处理器实现更多的学术创新。

## 许可证

版权所有 © 2020-2025 中国科学院计算技术研究所

版权所有 © 2021-2025 北京开源芯片研究院

版权所有 © 2020-2022 鹏城实验室

香山以 [木兰宽松许可证 第2版](LICENSE) 授权。
