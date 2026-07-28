    # Name        : fe_instr_uncache_boundary_mix
    # Goal        : mixed directed InstrUncache boundary stream runnable by NEMU
    # Entry       : _start at 0x10001000
    # Pattern     : offset matrix, JALR targets, branch targets, bus beat,
    #               32B fetch-block, page-near-tail, page-tail RVC, and long mixed stream
    # Boundary    : covers multiple uncache-safe boundaries including xffe
    # Check       : NEMU reaches GOOD TRAP with a0=0

    .section .text, "ax"
    .option norvc
    .globl _start

_start:
    addi s0, zero, 0
    addi s1, zero, 0
    addi s2, zero, 0
    jal zero, seq_offsets

    # Sequential RVI/RVC offset coverage within 8B beats.
    .org 0x40
seq_offsets:
    addi s0, s0, 1
    .option push
    .option rvc
    c.nop
    .option pop
    addi s0, s0, 2
    .option push
    .option rvc
    c.nop
    c.nop
    .option pop
    addi s0, s0, 3
    jal zero, jalr_targets

    # JALR into different offsets. Each target jumps to the next target.
    .org 0x80
jalr_targets:
    lui t0, %hi(jalr_tgt_off0)
    addi t0, t0, %lo(jalr_tgt_off0)
    jalr zero, 0(t0)

    .org 0xa0
jalr_tgt_off0:
    addi s1, s1, 1
    lui t0, %hi(jalr_tgt_off2)
    addi t0, t0, %lo(jalr_tgt_off2)
    jalr zero, 0(t0)

    .org 0xba
jalr_tgt_off2:
    addi s1, s1, 2
    lui t0, %hi(jalr_tgt_off4)
    addi t0, t0, %lo(jalr_tgt_off4)
    jalr zero, 0(t0)

    .org 0xd4
jalr_tgt_off4:
    addi s1, s1, 3
    lui t0, %hi(jalr_tgt_off6)
    addi t0, t0, %lo(jalr_tgt_off6)
    jalr zero, 0(t0)

    .org 0xee
jalr_tgt_off6:
    addi s1, s1, 4
    jal zero, branch_targets

    # Taken branch targets at nontrivial offsets.
    .org 0x120
branch_targets:
    addi t1, zero, 1
    beq t1, t1, br_tgt_off2
    addi s2, s2, -1

    .org 0x13a
br_tgt_off2:
    addi s2, s2, 1
    beq t1, t1, br_tgt_off4
    addi s2, s2, -1

    .org 0x154
br_tgt_off4:
    addi s2, s2, 2
    beq t1, t1, br_tgt_off6
    addi s2, s2, -1

    .org 0x16e
br_tgt_off6:
    addi s2, s2, 3
    jal zero, bus_beat_cases

    # 8B bus-beat tail cases: RVC at offset 6, then RVI at offset 6.
    .org 0x1a0
bus_beat_cases:
    addi s3, zero, 1
    .option push
    .option rvc
    c.nop
beat_tail_rvc:
    c.nop
    .option pop
    jal zero, beat_tail_rvi

    .org 0x1b6
beat_tail_rvi:
    addi s3, s3, 1
    jal zero, fetch_block_tail_rvc

    # 32B frontend fetch-block boundary, avoiding xffe.
    .org 0x1de
fetch_block_tail_rvc:
    .option push
    .option rvc
    c.nop
    .option pop
fetch_block_next_rvi:
    addi s4, s4, 1
    jal zero, page_near_tail

    # Page-near-tail safe offsets. Avoid page last halfword xffe.
    .org 0x1ff4
page_near_tail:
    addi s5, s5, 1
page_tail_safe_rvi:
    addi s5, s5, 2
    jal zero, long_mixed_stream

    # Long mixed-width stream across multiple 32B blocks.
    .org 0x2040
long_mixed_stream:
    addi t2, zero, 32
long_loop:
    .option push
    .option rvc
    c.nop
    .option pop
    addi s6, s6, 1
    .option push
    .option rvc
    c.nop
    .option pop
    addi t2, t2, -1
    bnez t2, long_loop

    # Page-tail RVC at 0x10002ffe.
    jal zero, page_tail_rvc

    .org 0x2ffe
page_tail_rvc:
    .option push
    .option rvc
    c.nop
    .option pop

done:
    addi a0, zero, 0
    .word 0x0005006b
