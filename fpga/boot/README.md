
# Preparing BOOT.BIN

Refer to www.wiki.xilinx.com/Fetch+Sources for more information.

## Build bl31.elf - Arm Trusted Firmware

This step is only needed for zynqmp. If your target board is zynq, skip this step.

```
git clone --depth 1 https://github.com/xilinx/arm-trusted-firmware
cd arm-trusted-firmware
make PLAT=zynqmp RESET_TO_BL31=1 CROSS_COMPILE=aarch64-none-elf-
mkdir -p path-to-labeled-RISC-V/fpga/boot/build/zynqmp
cp build/zynqmp/release/bl31/bl31.elf path-to-labeled-RISC-V/fpga/boot/build/zynqmp/
```

To clean, run
```
make PLAT=zynqmp RESET_TO_BL31=1 clean
```

## Build u-boot

```
git clone --depth 1 -b xilinx-v2017.4 https://github.com/xilinx/u-boot-xlnx
cd u-boot-xlnx

# for zynqmp
make xilinx_zynqmp_zcu102_rev1_0_defconfig  # can be found under u-boot-xlnx/configs/
make CROSS_COMPILE=aarch64-linux-gnu-
mkdir -p path-to-labeled-RISC-V/fpga/boot/build/zynqmp
cp u-boot-xlnx/u-boot.elf path-to-labeled-RISC-V/fpga/boot/build/zynqmp/

# for zynq
make zynq_zed_defconfig  # can be found under u-boot-xlnx/configs/
make CROSS_COMPILE=arm-linux-gnueabihf-
mkdir -p path-to-labeled-RISC-V/fpga/boot/build/zynq
cp u-boot-xlnx/u-boot.elf path-to-labeled-RISC-V/fpga/boot/build/zynq/
```

## Build BOOT.BIN and Device Tree Source

* (Optional) If you want to download the bitstream in fsbl,
also put the bitstream under `path-to-labeled-RISC-V/fpga/boot/build/{zynqmp,zynq}`,
and uncomment the description about bitstream in `bootgen-{zynqmp,zynq}.bif`.

```
# for zynqmp
path-to-labeled-RISC-V/fpga/boot $ ls build/zynqmp
bl31.elf   system_top.bit   u-boot.elf
path-to-labeled-RISC-V/fpga/boot $ cat bootgen-zynqmp.bif
the_ROM_image:
{
  [fsbl_config] a53_x64
  [bootloader] build/zynqmp/fsbl.elf
  [pmufw_image] build/zynqmp/pmufw.elf
  [destination_device=pl] build/zynqmp/system_top.bit
  [destination_cpu=a53-0, exception_level=el-3,trustzone] build/zynqmp/bl31.elf
  [destination_cpu=a53-0, exception_level=el-2] build/zynqmp/u-boot.elf
}

# for zynq
path-to-labeled-RISC-V/fpga/boot $ ls build/zynq
system_top.bit   u-boot.elf
path-to-labeled-RISC-V/fpga/boot $ cat bootgen-zynq.bif
the_ROM_image:
{
  [bootloader] build/zynq/fsbl.elf
  build/zynq/system_top.bit
  build/zynq/u-boot.elf
}
```
* generate hardware description file in Vivado
```
Vivado -> File -> Export -> Export Hardware
```
* set the correct path of device tree repo
```
git clone --depth 1 https://github.com/xilinx/device-tree-xlnx
# modify the `device_tree_repo_path` variable in `mk.tcl` to the repo just cloned
```
* generate BOOT.BIN and device tree source
```
cd path-to-labeled-RISC-V/fpga
make bootgen PRJ=my-project BOARD=your-target-board
```

Find `BOOT.BIN` and `dts` under `path-to-labeled-RISC-V/fpga/boot/build/myproject-your-target-board/`.

## Build linux kernel

```
git clone --depth 1 -b xilinx-v2017.4 https://github.com/xilinx/linux-xlnx
cd linux-xlnx

# for zynqmp
make ARCH=arm64 xilinx_zynqmp_defconfig # can be found under linux-xlnx/arch/arm64/configs/
make ARCH=arm64 menuconfig
  General setup -> Cross-compiler tool prefix: `aarch64-linux-gnu-`
  General setup -> unchoose `Initial RAM filesystem and RAM disk (initramfs/initrd) support`
make ARCH=arm64 -j16
# Find `Image` under `linux-xlnx/arch/arm64/boot/`


# for zynq
make ARCH=arm xilinx_zynq_defconfig # can be found under linux-xlnx/arch/arm/configs/
make ARCH=arm menuconfig
  General setup -> Cross-compiler tool prefix: `arm-linux-gnueabihf-`
  General setup -> unchoose `Initial RAM filesystem and RAM disk (initramfs/initrd) support`
  Devices Drivers -> Character devices -> Serial drivers -> choose `Xilinx uartilite serial port support`
                                                         -> then choose `Support for console on Xilinx uartlite serial port`
make ARCH=arm -j16
# Find `Image` under `linux-xlnx/arch/arm/boot/`
```

## Build system.dtb - Device Tree Blob

* create a new file `top.dts` to set bootargs and reserved-memory for the RISC-V subsystem
```
# for zynqmp
path-to-labeled-RISC-V/fpga/boot/build/myproject-your-target-board/dts $ cat top.dts
/include/ "system-top.dts"
/ {
  chosen {
    bootargs = "root=/dev/mmcblk0p2 rootfstype=ext4 rootwait earlycon clk_ignore_unused cpuidle.off=1";
  };
  reserved-memory {
    #address-cells = <2>;
    #size-cells = <2>;
    ranges;

    mem_reserved: buffer@800000000 {
      reg = <0x00000008 0x00000000 0x0 0x80000000>;
    };
	};
};
```
```
# for zynq
path-to-labeled-RISC-V/fpga/boot/build/myproject-your-target-board/dts $ cat top.dts
/include/ "system-top.dts"
/ {
  chosen {
    bootargs = "root=/dev/mmcblk0p2 rootfstype=ext4 rootwait earlycon clk_ignore_unused";
  };
  reserved-memory {
    #address-cells = <1>;
    #size-cells = <1>;
    ranges;

    mem_reserved: buffer@100000000 {
      reg = <0x10000008 0x10000000>;
    };
	};
};
```
* compile the dts to dtb
```
dtc -I dts -O dtb -o system.dtb top.dts
```

## Build rootfs in SD card

* New an `ext4` partition `mmcblk0p2` in SD card. Refer to the step of [here](https://wiki.debian.org/InstallingDebianOn/Xilinx/ZC702/wheezy#SD_Card_root) before executing `debootstrap`.
* download the debian base system to `mmcblk0p2` with `qemu-debootstrap`.

```
sudo qemu-debootstrap --arch arm64 stable /mnt http://ftp.debian.org/debian
# or for zynq
sudo qemu-debootstrap --arch armhf stable /mnt http://ftp.debian.org/debian

sudo chroot /mnt /bin/bash
passwd
apt-get update
apt-get install net-tools openssh-server vim build-essential minicom tmux libreadline-dev
# for nfs
apt-get install nfs-common autofs
exit
```
* Add a line of `ttyPS0` in `/mnt/etc/securetty` to allow login debian via `ttyPS0`. See [here](http://www.linuxquestions.org/questions/linux-newbie-8/login-incorrect-error-after-boot-no-password-prompted-881131/) for more details.
* Add a line of `PermitRootLogin yes` in `/mnt/etc/ssh/sshd_config` to enable root login via ssh. See [here](https://linuxconfig.org/enable-ssh-root-login-on-debian-linux-server) for more details.
* Add the following lines to `/mnt/etc/fstab`
```
# <file system> <mount point> <type>  <options> <dump>  <pass>
proc /proc proc defaults 0 0
/dev/mmcblk0p1 /boot vfat defaults 0 2
/dev/mmcblk0p2 / ext4 errors=remount-ro 0 1
```

Put `BOOT.BIN`, `Image` and `system.dtb` generated in the previous steps into `/dev/mmcblk0p1`.
Finally, insert the SD card into the board. It should be ready to boot debian.



### Some useful build-in commands under u-boot

* `load mmc 0:auto <addr> <file>` - load `file` in SD card into memory `addr`
* `fatls mmc 0 <path>` - list files under `path` in SD card
* `fatwrite mmc 0 <addr> <file> <byte_in_hex>` - write `file` of size `byte_in_hex` from memory `addr` into SD card
* `fpga loadb 0 <addr> <byte_in_hex>` - download bitstream of size `byte_in_hex` from memory `addr`

### Some useful customized commands

* set IP address and TFTP server address
```
setenv ipaddr 10.30.6.61
setenv serverip 10.30.6.123
```

* download FPGA, get linux kernel image and DTB file by TFTP, then boot the kernel
```
"netboot=tftpboot 0x100000 fpga.bit && fpga loadb 0 $fileaddr $filesize && " \
"tftpboot $kernel_addr Image && tftpboot $fdt_addr system.dtb && booti $kernel_addr - $fdt_addr \0"
```

* download file by TFTP to the boot partition (`mmcblk0p1`) of SD card
```
// should set $filename first
"download_sd=tftpboot 0x100000 $filename && fatwrite mmc 0 $fileaddr $filename $filesize\0"
```

* update the boot partition (`mmcblk0p1`) of SD card (`BOOT.BIN` and `system.dtb`) by TFTP
```
"update_sdboot=setenv filename BOOT.BIN && run download_sd && " \
"setenv filename system.dtb && run download_sd\0"
```
