# NOOP

NOOP(NJU Out-of-Order Processor) is a processor targeting super-scalar out-of-order execution.
Currently it only supports riscv32.

## Compile chisel code

* Install `mill`. Refer to [this guide][mill].
* Run `make` to generate verilog code. The output file is `build/TopMain.v`.

[mill]: lihaoyi.com/mill

## Run programs by simulation

* Set a new environment variable `NEMU_HOME` to the **absolute path** of the NEMU project.
* Set a new environment variable `NOOP_HOME` to the **absolute path** of the NOOP project.
* Clone the [AM project](https://github.com/NJU-ProjectN/nexus-am.git).
* Set a new environment variable `AM_HOME` to the **absolute path** of the AM project.
* Add a new AM `riscv32-noop` in the AM project if it is not provided.
* Run the application in the AM project by `make ARCH=riscv32-noop run`.

## Generate Vivado project

```
cd fpga
make BOARD=zedboard PRJ=myproject
```
The project can be found at `fpga/board/zedboard/build/myproject-zedboard/`.
Supported boards are listed under `fpga/board/`.
