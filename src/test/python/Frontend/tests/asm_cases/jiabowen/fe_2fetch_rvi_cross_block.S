    # Name   : fe_2fetch_rvi_cross_block
    # Goal   : exercise RVI reconstruction across adjacent 32-byte-aligned FTQ
    #          blocks, including a physical 64-byte FetchBlockSize boundary
    # Covers : RVI low halfword at alignment offset 30, high halfword at the
    #          next alignment offset 0, and repeated sequential/loop fetches
    #          without dropping or duplicating a halfword

    .section .text, "ax"
    .option rvc
    .globl _start

    .macro rvi_inc reg
    .option push
    .option norvc
    addi \reg, \reg, 1
    .option pop
    .endm

_start:
    .option push
    .option norvc
    addi s0, zero, 64
    .option pop

    # Train a taken branch back to a non-aligned target.  The target-to-line
    # tail contributes 15 halfwords and the following block contributes 15,
    # keeping the aggregate below the 32-halfword 2-fetch size limit.  The RVI
    # at byte offset 62 crosses exactly between those two predicted blocks.
    .option push
    .option norelax
    jal zero, dual_cross_base
    .balign 64
dual_cross_base:
    .2byte 0xa00d               # c.j +34 -> dual_cross_start
    .rept 16
    c.nop
    .endr
dual_cross_start:
    .rept 14
    c.nop
    .endr
dual_cross_rvi:
    rvi_inc s1
    c.addi s0, -1
    .rept 11
    c.nop
    .endr
dual_cross_branch:
    .2byte 0xc011               # c.beqz s0, +4 -> dual_cross_exit
dual_cross_jump:
    .2byte 0xb7d9               # c.j -58 -> dual_cross_start
dual_cross_exit:

    .if ((dual_cross_start - dual_cross_base) != 34)
    .error "dual_cross_start must begin at cache-line offset 34"
    .endif
    .if ((dual_cross_rvi - dual_cross_base) != 62)
    .error "dual_cross_rvi must begin at cache-line offset 62"
    .endif
    .if ((dual_cross_branch - dual_cross_base) != 90)
    .error "dual_cross_branch must begin at cache-line offset 90"
    .endif
    .if ((dual_cross_jump - dual_cross_base) != 92)
    .error "dual_cross_jump must begin at cache-line offset 92"
    .endif
    .if ((dual_cross_exit - dual_cross_base) != 94)
    .error "dual_cross_exit must begin at cache-line offset 94"
    .endif
    .option pop

    .option push
    .option norvc
    addi s0, zero, 32
    .option pop

    .option push
    .option norelax
    jal zero, loop_head
    .option pop

    # DefaultConfig uses FetchBlockSize=64B and FetchBlockAlignSize=32B.  A 64B
    # alignment makes the three sites below cross the 32B, 64B, and next 32B
    # boundaries respectively; the middle site is therefore guaranteed to
    # exercise the end of the full 64B fetch span as well.
    .balign 64
loop_head:
    .rept 15
    c.nop
    .endr
cross_block_rvi_0:
    rvi_inc s1

    # The preceding RVI ends at offset 2 in this block.  Fourteen compressed
    # nops place the next RVI at offset 30 again.
    .rept 14
    c.nop
    .endr
cross_block_rvi_1:
    rvi_inc s2

    .rept 14
    c.nop
    .endr
cross_block_rvi_2:
    rvi_inc s3

    c.addi s0, -1
    c.bnez s0, loop_head

    # Keep the layout contract executable: any edit which moves a stitched
    # RVI away from the last halfword of its block must fail assembly.
    .if ((cross_block_rvi_0 - loop_head) != 30)
    .error "cross_block_rvi_0 must start at fetch-block offset 30"
    .endif
    .if ((cross_block_rvi_1 - loop_head) != 62)
    .error "cross_block_rvi_1 must start at fetch-block offset 30"
    .endif
    .if ((cross_block_rvi_2 - loop_head) != 94)
    .error "cross_block_rvi_2 must start at fetch-block offset 30"
    .endif

done:
    .option push
    .option norvc
    addi a0, zero, 0
    .word 0x0005006b
    .option pop

    # Leave executable padding for frontend lookahead beyond the terminating
    # instruction; it is not part of the committed NEMU trace.
    .rept 128
    c.nop
    .endr
