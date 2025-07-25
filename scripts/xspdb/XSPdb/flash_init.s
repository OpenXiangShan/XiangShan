.section .text
.global _start
.align 2

# Purpose: Assign values to RISC-V integer and floating-point registers.
# Through PDB, values can be assigned to registers in RTL, but integer and floating-point registers have multiple internal registers and register renaming.
# Therefore, values must be assigned through a program.
# Data offsets:
#  mret  address: _start + 8
#  Integer register offset: _start + 16
#  Floating-point register offset: _start + 256

_start:
    j       _init                       # Skip the data section
    .align  3                           # Align the data section to 8 bytes
    .dword  0x80000000                  # Store the target address at the start of the data section
    .zero   31 * 8                      # Initial values for integer registers (x1-x31)
    .rept   32                          # Initial values for floating-point registers (f0-f31)
    .dword  0x3FF0000000000000          # Double-precision floating-point value 1.0
    .endr
_init:
    la      t0, _start
    addi    t0, t0, 8                   # Calculate the data section offset
    ld      t1, 0(t0)                   # Load the _entry address
    csrw    mepc, t1                    # Set the machine-mode exception program counter
    li      t1, (3 << 11)               # Set mret mode to MPP=11 (M-mode)
    csrs    mstatus, t1
    li      t1, 0x2000                  # Mask for the mstatus.FS field (bits 13-14, set mstatus.FS = 01 or 11)
    csrrs   x0, mstatus, t1             # Set FS to "Initial" state (enable floating-point instructions)
    # 初始化整型寄存器 (x1-x31)
    addi    t0, t0, 8                   # Skip the target address
    addi    t1, t0, 0                   # Save base to t1
    ld      x1, 0(t0)                   # Assign values
    ld      x2, 8(t0)
    ld      x3, 16(t0)
    ld      x4, 24(t0)                  # Temporarily skip  ld x5, 32(t0)
    ld      x7, 48(t0)                  # Temporarily skip  ld x6, 40(t0)
    ld      x8, 56(t0)
    ld      x9, 64(t0)
    ld      x10, 72(t0)
    ld      x11, 80(t0)
    ld      x12, 88(t0)
    ld      x13, 96(t0)
    ld      x14, 104(t0)
    ld      x15, 112(t0)
    ld      x16, 120(t0)
    ld      x17, 128(t0)
    ld      x18, 136(t0)
    ld      x19, 144(t0)
    ld      x20, 152(t0)
    ld      x21, 160(t0)
    ld      x22, 168(t0)
    ld      x23, 176(t0)
    ld      x24, 184(t0)
    ld      x25, 192(t0)
    ld      x26, 200(t0)
    ld      x27, 208(t0)
    ld      x28, 216(t0)
    ld      x29, 224(t0)
    ld      x30, 232(t0)
    ld      x31, 240(t0)
    # Initialize floating-point registers (f0-f31)
    addi    t0, t0, 31 * 8              # Skip the integer register section
    fld     f0, 0(t0)
    fld     f1, 8(t0)
    fld     f2, 16(t0)
    fld     f3, 24(t0)
    fld     f4, 32(t0)
    fld     f5, 40(t0)
    fld     f6, 48(t0)
    fld     f7, 56(t0)
    fld     f8, 64(t0)
    fld     f9, 72(t0)
    fld     f10, 80(t0)
    fld     f11, 88(t0)
    fld     f12, 96(t0)
    fld     f13, 104(t0)
    fld     f14, 112(t0)
    fld     f15, 120(t0)
    fld     f16, 128(t0)
    fld     f17, 136(t0)
    fld     f18, 144(t0)
    fld     f19, 152(t0)
    fld     f20, 160(t0)
    fld     f21, 168(t0)
    fld     f22, 176(t0)
    fld     f23, 184(t0)
    fld     f24, 192(t0)
    fld     f25, 200(t0)
    fld     f26, 208(t0)
    fld     f27, 216(t0)
    fld     f28, 224(t0)
    fld     f29, 232(t0)
    fld     f30, 240(t0)
    fld     f31, 248(t0)
    # Finally assign values to t0
    addi    t0, t1, 0
    ld x6,  40(t0)
    ld x5,  32(t0)
    # Jump to the target address
    .rept   16                          #  16 nop instructions
    nop
    .endr
    mret

## Compilation:
# riscv64-unknown-elf-gcc -march=rv64gc -mabi=lp64d -nostdlib -c XSPdb.S -o /tmp/XSPdb.o
# riscv64-unknown-elf-ld -T XSPdb.ld /tmp/XSPdb.o -o /tmp/XSPdb.elf
# riscv64-unknown-elf-objcopy -O binary /tmp/XSPdb.elf ready-to-run/XSPdb.bin

## View
# riscv64-unknown-elf-objdump -D -b binary -m riscv:rv64 ready-to-run/XSPdb.bin|less
