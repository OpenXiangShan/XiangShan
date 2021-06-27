# XiangShan

XiangShan (香山) is an open-source high-performance RISC-V processor project. 

中文说明[在此](README-ZH-HANS.md)。

Copyright 2020-2021 by Institute of Computing Technology, Chinese Academy of Sciences.

# Docs and slides
We gave 20+ presentations on RISC-V World Conference China 2021. XiangShan tutorial was held at the same place. Our slides for RVWC2021 have been updated on [our doc repo](https://github.com/OpenXiangShan/XiangShan-doc) (in Chinese).

我们在2021年RISC-V中国峰会的报告已经更新到[这里](https://github.com/OpenXiangShan/XiangShan-doc)。未来的文档和相关信息也将更新到相同的仓库。

## Architecture

The first stable micro-architecture of XiangShan is called Yanqihu (雁栖湖) on this [branch](https://github.com/OpenXiangShan/XiangShan/tree/yanqihu), which has been developed since June 2020. The current version of XiangShan, also known as Nanhu (南湖), is still under development on the master branch.

The micro-architecture overview is shown below.

![xs-arch-single](xs-arch-simple.svg)



## Sub-directories Overview

Some of the key directories are shown below.

```
.
├── fpga                   # supported FPGA boards and files to build a Vivado project
├── read-to-run            # pre-built simulation images
├── scripts                # scripts for agile development
└── src
    ├── test               # test files (including diff-test, module-test, etc.)
    └── main/scala         # design files
        ├── bus/tilelink   # tilelink utils
        ├── device         # virtual device for simulation
        ├── difftest       # diff-test chisel interface
        ├── system         # SoC wrapper
        ├── top            # top module
        ├── utils          # utilization code
        ├── xiangshan      # main design code
        └── xstransforms   # some useful firrtl transforms
```



## Generate Verilog

* Run `make verilog` to generate verilog code. The output file is `build/XSTop.v`.
* Refer to `Makefile` for more information.



## Run Programs by Simulation

### Prepare environment

* Set environment variable `NEMU_HOME` to the **absolute path** of the [NEMU project](https://github.com/OpenXiangShan/NEMU).
* Set environment variable `NOOP_HOME` to the **absolute path** of the XiangShan project.
* Set environment variable `AM_HOME` to the **absolute path** of the [AM project](https://github.com/OpenXiangShan/nexus-am).
* Install `mill`. Refer to [the Manual section in this guide](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html#_installation).
* Clone this project and run `make init` to initialize submodules.

### Run with simulator

* Install [Verilator](https://verilator.org/guide/latest/), the open-source Verilog simulator.
* Run `make emu` to build the C++ simulator `./build/emu` with Verilator.
* Refer to `./build/emu --help` for run-time arguments of the simulator. 
* Refer to `Makefile` and `verilator.mk` for more information.

Example:

```bash
make emu CONFIG=MinimalConfig SIM_ARGS=--disable-log EMU_THREADS=2 -j10
./build/emu -b 0 -e 0 -i ./ready-to-run/coremark-2-iteration.bin --diff ./ready-to-run/riscv64-nemu-interpreter-so
```

## Acknowledgement

In the development of XiangShan, some sub-modules from the open-source community are employed. All relevant usage is listed below.

| Sub-module         | Source                                                       | Detail                                                       |
| ------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| L2 Cache/LLC       | [Sifive block-inclusivecache](https://github.com/ucb-bar/block-inclusivecache-sifive) | We enhance the function and the timing of the original module, finally turning it into a Cache generator that can be configured as L2/LLC. |
| Diplomacy/TileLink | [Rocket-chip](https://github.com/chipsalliance/rocket-chip)  | We reused the diplomacy framework and TileLink utility that exist in rocket-chip to negotiate bus. |
| FPU                | [Berkeley hardfloat](https://github.com/ucb-bar/berkeley-hardfloat) | We use Berkeley-hardfloat as our FPU and implement an SRT-4 div/sqrt unit for it. Additionally, we split the FMA pipeline to optimize the timing. |

We are grateful for the support of the open-source community and encourage other open-source projects to reuse our code within the scope of the [license](LICENSE).

