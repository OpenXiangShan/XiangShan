
## sidewinder

To make the SD card work, do the followings
* use a class10 SD card
* `fsbl/psu_init.c`: search for `IOU_SLCR_BANK1_CTRL5_OFFSET`, change `0x2000FFFU` to `0x3FFFFFFU` (already done inside `mk.tcl`)
* add "disable-wp;" property to the node of "sdhci@ff170000" in device tree

* remove gem3.phy node

## ultraZ

To make the SD card work, do the followings
* should use 2016.4 dts
* add "no-1-8-v;" property to the node of "sdhci@ff170000" in device tree
* modify gem3.phyc.reg to <0x5>

## axu3cg

* should use u-boot.elf from petalinux
  * it seems that the default zcu102 config in u-boot can not adapt to this board
* modify gem3.phyc.reg to <0x1>
* remove `pinctrl-names` and `pinctrl-0` property from node `uart1`
  * this will fix the issue of unable to input in linux

## u-boot

define marco `CONFIG_ENV_OVERWRITE` in `u-boot-xlnx/include/configs/xilinx-zynqmp.h` to make `ethaddr` environment variable writable
