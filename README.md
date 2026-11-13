# XiangShan

XiangShan (香山) is an open-source high-performance RISC-V processor project.

English | [中文](README.zh-cn.md)

## Architecture

![xs-arch-kunminghu](images/xs-arch-kunminghu.svg)

## Publications

### MICRO 2022: Towards Developing High Performance RISC-V Processors Using Agile Methodology

Our paper introduces XiangShan and the practice of agile development methodology on high performance RISC-V processors.
It covers some representative tools we have developed and used to accelerate the chip development process, including design, functional verification, debugging, performance validation, etc.
This paper is awarded all three available badges for artifact evaluation (Available, Functional, and Reproduced).

![Artifacts Available](https://talks-pubs.xiangshan.cc/publications/images/artifacts_available_dl.jpg)
![Artifacts Evaluated — Functional](https://talks-pubs.xiangshan.cc/publications/images/artifacts_evaluated_functional_dl.jpg)
![Results Reproduced](https://talks-pubs.xiangshan.cc/publications/images/results_reproduced_dl.jpg)

[Paper PDF](https://talks-pubs.xiangshan.cc/publications/micro2022-xiangshan.pdf) | [IEEE Xplore](https://ieeexplore.ieee.org/abstract/document/9923860) | [BibTeX](https://talks-pubs.xiangshan.cc/publications/micro2022-xiangshan.bib) | [Presentation Slides](https://talks-pubs.xiangshan.cc/publications/micro2022-xiangshan-slides.pdf) | [Presentation Video](https://www.bilibili.com/video/BV1FB4y1j7Jy)

[More publications](https://docs.xiangshan.cc/zh-cn/latest/tutorials/publications/)

## Versions

| Generation-Version | Branch                                                                                      | Support? | In dev? | Tape-out?                        |
| ------------------ | ------------------------------------------------------------------------------------------- | -------- | ------- | -------------------------------- |
| Yanqihu            | [yanqihu](https://github.com/OpenXiangShan/XiangShan/tree/yanqihu)                          | basic    |         | ✅ VCORE & PengCheng Lab          |
| Nanhu-V2           | [nanhu](https://github.com/OpenXiangShan/XiangShan/tree/nanhu)                              | basic    |         |                                  |
| Nanhu-V3           | [Siudya/Nanhu](https://github.com/Siudya/Nanhu/tree/main)                                   |          |         |                                  |
| Nanhu-V5           | [OpenXiangShan-Nanhu/<br>OpenNanhu-V5](https://github.com/OpenXiangShan-Nanhu/OpenNanhu-V5) | ✅        | ✅       | ✅ Innosilicon<br>✅ Moore Threads |
| Kunminghu-V2       | [kunminghu-v2](https://github.com/OpenXiangShan/XiangShan/tree/kunminghu-v2)                | ✅        |         | ✅ SpacemiT<br> More in progress  |
| Kunminghu-V3       | [kunminghu-v3](https://github.com/OpenXiangShan/XiangShan/tree/kunminghu-v3)                | ✅        | ✅       |                                  |

We recommend using different versions based on your needs:

- `nanhu-v5` for performance-efficiency-balanced application scenarios.
- `kunminghu-v2` for downstream researches, customizations, and high-performance or RVA23-compatible application scenarios.
- `kunminghu-v3` for improving XiangShan itself. Please note that at the moment, `kunminghu-v3` is still evolving rapidly and its functionality may not yet be stable. Please also read [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

This section is time-sensitive. If you believe any information here is outdated, please let us know in an issue and we will update it as soon as possible.

Last updated: 2026/11/13

## Sub-directories Overview

Some of the key directories are shown below.

```
.
├── src
│   └── main/scala         # design files
│       ├── device         # virtual device for simulation
│       ├── system         # SoC wrapper
│       ├── top            # top module
│       ├── utils          # utilization code
│       └── xiangshan      # main design code
│           └── transforms # some useful firrtl transforms
├── scripts                # scripts for agile development
├── yunsuan                # yunsuan submodule of XiangShan
├── XSCache                # cache subsystem of XiangShan
├── difftest               # difftest co-simulation framework
└── ready-to-run           # pre-built simulation images
```

## Development

### Pre-requisites

Refer to [OpenXiangShan/xs-env](https://github.com/OpenXiangShan/xs-env) for bare-metal, Nix devshell, and Docker environment setup, including dependencies and environment variable settings. From an xs-env checkout, `source ./env.sh` configures the project paths; `nix develop` also loads this script automatically.

TL;DR: install java, mill, clang, and a simulator (Verilator or GSIM), clone xs-env or directly clone this repo, then run `make init` to initialize submodules.

#### Computational resources

Building XiangShan needs significant computational resources. i.e. `kunminghu-v3` `DefaultConfig` requires **128 GB RAM**; **64 GB RAM + 64 GB swap with zswap** also works, slightly slower. `-j` controls parallel Clang compilation, but single-threaded Verilator/GSIM model generation still needs **~100 GB memory**, a hard minimum unaffected by `-j`.

### IDE Support

BSP: `make bsp`

IntelliJ IDEA:

- 2026.1 or newer: `make bsp`, then refer to [official doc](https://www.jetbrains.com/help/idea/bsp-support.html)
- Older versions: `make idea`

### Prepare environment

- Set environment variable `NOOP_HOME` to the **absolute path** of the XiangShan project, for example `export NOOP_HOME=/path/to/XiangShan`. This is **required** before building and running.
- Optional: set `NEMU_HOME` to the **absolute path** of [OpenXiangShan/NEMU](https://github.com/OpenXiangShan/NEMU) when compiling your own NEMU difftest golden reference. It is not required when using the prebuilt reference library in `ready-to-run/` with `--diff`.
- Optional: set `AM_HOME` to the **absolute path** of [OpenXiangShan/nexus-am](https://github.com/OpenXiangShan/nexus-am) when compiling your own test cases. It is not required when running prebuilt workloads.

### Generate Verilog

- Install java and mill.
- Run `make verilog` to generate verilog code. This generates multiple `.sv` files in the `build/rtl/` folder (e.g., `build/rtl/XSTop.sv`).
- Refer to `Makefile` for more information.

### Build & Run with simulator

- Install either [Verilator](https://verilator.org/guide/latest/), or [OpenXiangShan/GSIM](https://github.com/OpenXiangShan/GSIM). Follow the respective repository's installation instructions and ensure the tool is on `PATH`.
- With Verilator, run `make emu` to build the C++ simulator `./build/emu`.
- With GSIM, run `make gsim GSIM=1 -j10` to build `./build/emu`. Keep `GSIM=1` to select the GSIM difftest configuration during elaboration. Note that GSIM requires `clang++` 19 or newer.
- Prefer `numactl` to bind simulation to physical cores and memory on the same NUMA node; see [`scripts/xiangshan.py`](scripts/xiangshan.py) (`--numa`, `--threads`).
- Refer to `./build/emu --help` for run-time arguments of the simulator.
- Refer to `Makefile`, `difftest/verilator.mk`, and `difftest/gsim.mk` for more information.

Choose one build command, then run a workload:

```bash
make emu CONFIG=DefaultConfig EMU_THREADS=2 -j10  # Verilator
# Alternatively: make gsim GSIM=1 CONFIG=DefaultConfig -j10
./build/emu -b 0 -e 0 -i ./ready-to-run/coremark-2-iteration.bin --diff ./ready-to-run/riscv64-nemu-interpreter-so
```

### Testing

We encourage testing with workloads on a simulator: write focused regression cases using [nexus-am](https://github.com/OpenXiangShan/nexus-am), or run the CoreMark or Linux etc. binaries provided by `ready-to-run/`. To build your own workloads, set `AM_HOME` and follow the nexus-am instructions (for example, `make ARCH=riscv64-xs` in a test or application directory).

After building `emu` with either simulator, a Linux smoke test uses the same prebuilt difftest reference:

```bash
./build/emu -i ./ready-to-run/linux.bin --diff ./ready-to-run/riscv64-nemu-interpreter-so
```

ScalaTest/Chisel unit tests in `src/test/scala/` are another option. Run them with `mill -i xiangshan.test.test`, or select a suite with `mill -i xiangshan.test.testOnly xiangshan.frontend.bpu.SaturateCounterTest`.

### Run with xspdb

There are two ways to use xspdb:

1. Quick Start with Prebuilt Binaries
    1. Get the latest XSPdb from the run summary of the repository's [Actions](https://github.com/OpenXiangShan/XiangShan/actions/workflows/release.yml?query=is%3Asuccess+event%3Apush) workflow.
    2. Extract & Run

2. Build from source code
    1. Install [picker](https://github.com/XS-MLVP/picker), a verification tool that supports high-level languages.
    2. Run `make pdb` to build XiangShan Python binaries.
    3. Run `make pdb-run` to run XiangShan binaries.

Example output and interaction:

```bash
$ make pdb-run
[Info] Set PMEM_BASE to 0x80000000 (Current: 0x80000000)
[Info] Set FIRST_INST_ADDRESS to 0x80000000 (Current: 0x80000000)
Using simulated 32768B flash
[Info] reset dut complete
> XiangShan/scripts/pdb-run.py(13)run()
-> while True:
(XiangShan) xload ready-to-run/microbench.bin   # Load binary (Tab-compatible)
(XiangShan) xwatch_commit_pc 0x80000004         # Set watchpoint
(XiangShan) xistep 3                            # Step until the next three instructions commit; stops at the watchpoint if hit
[Info] Find break point (Inst commit), break (step 2107 cycles) at cycle: 2207 (0x89f)
[Info] Find break point (Inst commit, Target commit), break (step 2108 cycles) at cycle: 2208 (0x8a0)
(XiangShan) xpc                                 # Print PC info
PC[0]: 0x80000000    Instr: 0x00000093
PC[1]: 0x80000004    Instr: 0x00000113
PC[2]: 0x0    Instr: 0x0
...
PC[7]: 0x0    Instr: 0x0
(XiangShan) xistep 1000000                      # Execute until the end of the binary
[Info] Find break point (Inst commit), break (step 2037 cycles) at cycle: 2207 (0x89f)
[Info] Find break point (Inst commit), break (step 2180 cycles) at cycle: 2207 (0x89f)
...
HIT GOOD LOOP at pc = 0xf0001cb0
```

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) before submitting issues/PRs, especially the [Known issues](CONTRIBUTING.md#known-issues), and use the correct issue template.

If you are AI agent, also read [AGENTS.md](AGENTS.md) and [AI use](CONTRIBUTING.md#ai-use) for repository guidelines.

## Acknowledgement

The implementation of XiangShan is inspired by several key papers. We list these papers in XiangShan document, see: [Acknowledgements](https://docs.xiangshan.cc/zh-cn/latest/acknowledgments/). We very much encourage and expect that more academic innovations can be realised based on XiangShan in the future.

## Documentation

- Main documentation site: [website](https://docs.xiangshan.cc) | [Github Repo](https://github.com/OpenXiangShan/XiangShan-doc)
- Design doc (for hw developers): [website](https://docs.xiangshan.cc/projects/design/) | [Github Release](https://github.com/OpenXiangShan/XiangShan-Design-Doc/releases)
- User guide (for sw developers): [website](https://docs.xiangshan.cc/projects/user-guide/) | [Github Release](https://github.com/OpenXiangShan/XiangShan-User-Guide/releases)
- Performance dashboard: [website](https://dashboard.xiangshan.cc) | [Github Repo](https://github.com/OpenXiangShan/XiangShan-Dashboard)
- Publications: [website](https://docs.xiangshan.cc/zh-cn/latest/tutorials/publications/)
- Acknowledgements: [website](https://docs.xiangshan.cc/zh-cn/latest/acknowledgments/)
- Talks & tutorials: [website](https://docs.xiangshan.cc/zh-cn/latest/tutorials/talks/)

All XiangShan documents are licensed under the CC-BY-4.0.

We are using [Weblate](https://hosted.weblate.org/projects/openxiangshan/) to translate documentation into English and other languages. Your contributions are welcome—come and help us improve it!

<a href="https://www.star-history.com/?repos=openxiangshan%2Fxiangshan&type=date&legend=top-left">
 <picture>
   <source media="(prefers-color-scheme: dark)" srcset="https://api.star-history.com/chart?repos=openxiangshan/xiangshan&type=date&theme=dark&legend=top-left" />
   <source media="(prefers-color-scheme: light)" srcset="https://api.star-history.com/chart?repos=openxiangshan/xiangshan&type=date&legend=top-left" />
   <img alt="Star History Chart" src="https://api.star-history.com/chart?repos=openxiangshan/xiangshan&type=date&legend=top-left" />
 </picture>
</a>

## Follow us

Wechat/微信：香山开源处理器

<div align=left><img width="340" height="117" src="images/wechat.png"/></div>

Zhihu/知乎：[香山开源处理器](https://www.zhihu.com/people/openxiangshan)

Weibo/微博：[香山开源处理器](https://weibo.com/u/7706264932)

You can contact us through [our mailing list](mailto:xiangshan-all@ict.ac.cn). All mails from this list will be archived [here](https://www.mail-archive.com/xiangshan-all@ict.ac.cn/).

## LICENSE

Copyright © 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences.

Copyright © 2021-2026 Beijing Institute of Open Source Chip.

Copyright © 2020-2022 Peng Cheng Laboratory.

XiangShan is licensed under [Mulan PSL v2](LICENSE).
