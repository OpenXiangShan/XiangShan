
## sidewinder

To make the SD card work, do the followings
* `fsbl/psu_init.c`: search for `IOU_SLCR_BANK1_CTRL5_OFFSET`, change `0x2000FFFU` to `0x3FFFFFFU`
* should use 2016.4 dts
* add "no-1-8-v;" property to the node of "sdhci@ff170000" in device tree

## u-boot

define marco `CONFIG_ENV_OVERWRITE` in `u-boot-xlnx/include/configs/xilinx-zynqmp.h` to make `ethaddr` environment variable writable
