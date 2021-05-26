# XiangShan

XiangShan is an open-source high-performance RISC-V processor.

NOTE: XiangShan has not been officially released to the public open-source community.
License and docs to be added later.

Copyright 2020-2021 by Institute of Computing Technology, Chinese Academy of Sciences.

## Prepare environment

* Set environment variable `NEMU_HOME` to the **absolute path** of the [NEMU project](https://github.com/OpenXiangShan/NEMU).
* Set environment variable `NOOP_HOME` to the **absolute path** of the XiangShan project.
* Set environment variable `AM_HOME` to the **absolute path** of the [AM project](https://github.com/OpenXiangShan/nexus-am).
* Install `mill`. Refer to [the Manual section in this guide](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html#_installation).
* Run `make init` to initialize submodules.

## Run simulation

* Install [Verilator](https://verilator.org/guide/latest/), the open-source Verilog simulator.
* Run `make emu` to build the C++ simulator `./build/emu` with Verilator.
* Refer to `./build/emu --help` for run-time arguments of the simulator. 
* Refer to `Makefile` and `verilator.mk` for more information.

Example:
```bash
make emu CONFIG=MinimalConfig SIM_ARGS=--disable-log EMU_THREADS=2 -j10
./build/emu -b 0 -e 0 -i $AM_HOME/apps/coremark/build/coremark-riscv64-noop.bin
```

## Generate Verilog
* Run `make verilog` to generate verilog code. The output file is `build/XSTop.v`.
* Refer to `Makefile` for more information.
