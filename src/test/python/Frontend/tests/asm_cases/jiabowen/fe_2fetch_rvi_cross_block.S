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
    addi s0, zero, 32
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
