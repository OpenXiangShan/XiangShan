# NOOP

NOOP(NJU Out-of-Order Processor) is a processor targeting super-scalar out-of-order execution.
Currently it only supports riscv32.

## Compile chisel code

* Install `mill`. Refer to [the Manual section in this guide][mill].
* Run `make` to generate verilog code. The output file is `build/TopMain.v`.

[mill]: http://lihaoyi.com/mill#manual

## Run programs by simulation

* Set a new environment variable `NEMU_HOME` to the **absolute path** of the NEMU project.
* Set a new environment variable `NOOP_HOME` to the **absolute path** of the NOOP project.
* Clone the [AM project](https://github.com/NJU-ProjectN/nexus-am.git).
* Set a new environment variable `AM_HOME` to the **absolute path** of the AM project.
* Add a new AM `riscv64-noop` in the AM project if it is not provided.
* Run the application in the AM project by `make ARCH=riscv64-noop run`.

## Run on FPGA

### Sub-directories Overview
```
fpga
├── board              # supported FPGA boards and files to build a Vivado project
├── boot               # PS boot flow of zynq and zynqmp
├── lib                # HDL sources shared by different boards
├── Makefile
├── Makefile.check
└── noop.tcl           # wrapper of NOOP core in the Vivado project
```

### Build a Vivado project

* Install Vivado 2019.1, and source the setting of Vivado and SDK
* Run the following command to build a Vivado project
```
cd fpga
make PRJ=myproject BOARD=axu3cg
```
Change `axu3cg` to the target board you want. Supported boards are listed under `board/`.
The project will be created under `board/axu3cg/build/myproject-axu3cg`.
* Open the project with Vivado and generate bitstream.

### Prepare SD card

Refer to the instructions of [fpga/boot/README.md](fpga/boot/README.md).

NOTE: Remember to put the bitstream into BOOT.BIN, since the guide is going to boot everything from SD card.

### Set your board to SD boot mode

Please refer to the user guide of your board.
* [zedboard](http://www.zedboard.org/sites/default/files/ZedBoard_HW_UG_v1_1.pdf)
* [zcu102](https://www.xilinx.com/support/documentation/boards_and_kits/zcu102/ug1182-zcu102-eval-bd.pdf)
* [sidewinder](http://sidewinder.fidus.com)
* ultraZ (currently not avaliable to the public)
* axu3cg (currently not avaliable to the public)

### Boot linux in PS

Just insert the SD card into the board, open a serial terminal and powerup the board.

### Boot NOOP (the RISC-V subsystem)

To boot the RISC-V subsystem
* Send `fpga/resource/ddr-loader/ddr-loader.c` to PS.
This can be achieved by either copying the file to SD card,
or by sending the file with `scp` if you have your board connected to your host by network.
* Compile the loader by gcc on PS.
```
gcc -O2 -o ddr-loader ddr-loader.c
```
* Send the RISC-V program (bin file, should start at 0x80000000) to PS.
* Open minicom on PS to connect to the UART of NOOP.
Note that you can connect to PS via `ssh` and use `tmux` to get multiple terminals.
```
minicom -D /dev/ttyUL1
```
* Use the loader to load the program to NOOP memory and start running NOOP.
```
./ddr-loader axu3cg bin-file
```
* To shutdown the board, first run `poweroff` in PS.
