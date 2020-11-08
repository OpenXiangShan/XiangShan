# RISC-V Compliance Test I-MISALIGN_JMP-01
#
# Copyright (c) 2017, Codasip Ltd.
# Copyright (c) 2018, Imperas Software Ltd. Additions
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#      * Redistributions of source code must retain the above copyright
#        notice, this list of conditions and the following disclaimer.
#      * Redistributions in binary form must reproduce the above copyright
#        notice, this list of conditions and the following disclaimer in the
#        documentation and/or other materials provided with the distribution.
#      * Neither the name of the Codasip Ltd., Imperas Software Ltd. nor the
#        names of its contributors may be used to endorse or promote products
#        derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
# THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Codasip Ltd., Imperas Software Ltd.
# BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# Specification: RV32I Base Integer Instruction Set, Version 2.0
# Description: Testing MISALIGNED JUMP exception.

#include "compliance_test.h"
#include "compliance_io.h"
#include "test_macros.h"

# Test Virtual Machine (TVM) used by program.
RV_COMPLIANCE_RV32M

# Test code region
RV_COMPLIANCE_CODE_BEGIN

    RVTEST_IO_INIT
    RVTEST_IO_ASSERT_GPR_EQ(x30, x0, 0x00000000)
    RVTEST_IO_WRITE_STR(x30, "# Test Begin Reserved reg x31\n")

    # Save and set trap handler address
    la x1, _trap_handler
    csrrw x31, mtvec, x1

    # switch off C
    csrrci   x0, misa, 4

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part A1 - test JAL\n");

    # Address for test results
    la      x1, test_A1_res_exc

    # Test
    li      x2, 0x11111111
    jal     x0, 1f + 2
    li      x2, 0
1:

    //
    // Assert
    //
    RVTEST_IO_CHECK()
    RVTEST_IO_WRITE_STR(x30, "# Test part A1  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part A2 - test JALR - NOT causing the exception\n");

    # Address for test results
    la      x1, test_A2_res

    # Test
    li      x2, 0x22222222
    la      x4, 1f + 1
    jalr    x0, x4, 0
    li      x2, 0
1:
    sw      x2, 0(x1)
    addi    x1, x1, 4

    li      x2, 0x33333333
    la      x4, 1f
    jalr    x0, x4, 1
    li      x2, 0
1:
    sw      x2, 0(x1)
    addi    x1, x1, 4

    li      x2, 0x44444444
    la      x4, 1f
    jalr    x0, x4, -3
    li      x2, 0

    sw      x2, 0(x1)
1:
    addi    x1, x1, 4

    RVTEST_IO_WRITE_STR(x30, "# Test part A2  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part A3 - test JALR - causing the exception\n");

    # Address for test results
    la      x1, test_A3_res_exc

    # Test
    li      x2, 0x55555555
    la      x4, 1f + 2
    jalr    x0, x4, 0
    li      x2, 0
1:

    li      x2, 0x66666666
    la      x4, 1f + 3
    jalr    x0, x4, 0
    li      x2, 0
1:

    # Test
    li      x2, 0x77777777
    la      x4, 1f
    jalr    x0, x4, 2
    li      x2, 0
1:

    li      x2, 0x88888888
    la      x4, 1f
    jalr    x0, x4, 3
    li      x2, 0
1:

    RVTEST_IO_WRITE_STR(x30, "# Test part A3  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part B1 - test BEQ\n");

    # Address for test results
    la      x1, test_B1_res_exc

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    beq     x5, x6, 1f + 2
    li      x2, 0x99999999
1:
    nop
    nop
    beq     x5, x5, 1f + 2
    li      x2, 0
1:

    RVTEST_IO_WRITE_STR(x30, "# Test part A4  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part B2 - test BNE\n");

    # Address for test results
    la      x1, test_B2_res_exc

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    bne     x5, x5, 1f + 2
    li      x2, 0xAAAAAAAA
1:
    nop
    nop
    bne     x5, x6, 1f + 2
    li      x2, 0
1:

    RVTEST_IO_WRITE_STR(x30, "# Test part A5  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part B3 - test BLT\n");

    # Address for test results
    la      x1, test_B3_res_exc

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    blt     x6, x5, 1f + 2
    li      x2, 0xBBBBBBBB
1:
    nop
    nop
    blt     x5, x6, 1f + 2
    li      x2, 0
1:

    RVTEST_IO_WRITE_STR(x30, "# Test part B  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part B4 - test BLTU\n");

    # Address for test results
    la      x1, test_B4_res_exc

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    bltu    x6, x5, 1f + 2
    li      x2, 0xCCCCCCCC
1:
    nop
    nop
    bltu    x5, x6, 1f + 2
    li      x2, 0
1:

    RVTEST_IO_WRITE_STR(x30, "# Test part C  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part B5 - test BGE\n");

    # Address for test results
    la      x1, test_B5_res_exc

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    bge     x5, x6, 1f + 2
    li      x2, 0xDDDDDDDD
1:
    nop
    nop
    bge     x6, x5, 1f + 2
    li      x2, 0
1:

    RVTEST_IO_WRITE_STR(x30, "# Test part D  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part B6 - test BGEU\n");

    # Address for test results
    la      x1, test_B6_res_exc

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    bgeu    x5, x6, 1f + 2
    li      x2, 0xEEEEEEEE
1:
    nop
    nop
    bgeu    x6, x5, 1f + 2
    li      x2, 0
1:

    RVTEST_IO_WRITE_STR(x30, "# Test part E  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    # restore mtvec and jump to the end
    csrw mtvec, x31
    jal x0, test_end

    RVTEST_IO_WRITE_STR(x30, "# Test part A1  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    # Exception handler
_trap_handler:
    # increment return address
    csrr    x30, mtval
    addi    x30, x30, -2
    csrw    mepc, x30

    # store low bits of mtval
    csrr    x30, mtval
    andi    x30, x30, 3
    sw      x30, 0(x1)

    # Store MCAUSE
    csrr    x30, mcause
    sw      x30, 4(x1)

    # Store data from test
    sw      x2, 8(x1)

    # increment data_exc address
    addi    x1, x1, 12

    # return
    mret
    RVTEST_IO_WRITE_STR(x30, "# Test part A2  - Complete\n");

    # ---------------------------------------------------------------------------------------------

test_end:
    RVTEST_IO_WRITE_STR(x30, "# Test part A3  - Complete\n");

    RVTEST_IO_WRITE_STR(x30, "# Test End\n")

 # ---------------------------------------------------------------------------------------------
    # HALT
    RV_COMPLIANCE_HALT

RV_COMPLIANCE_CODE_END

# Input data section.
    .data
    .align 4


# Output data section.
RV_COMPLIANCE_DATA_BEGIN
    .align 4

test_A1_res_exc:
    .fill 3, 4, -1
test_A2_res:
    .fill 3, 4, -1
test_A3_res_exc:
    .fill 12, 4, -1
test_B1_res_exc:
    .fill 3, 4, -1
test_B2_res_exc:
    .fill 3, 4, -1
test_B3_res_exc:
    .fill 3, 4, -1
test_B4_res_exc:
    .fill 3, 4, -1
test_B5_res_exc:
    .fill 3, 4, -1
test_B6_res_exc:
    .fill 3, 4, -1

RV_COMPLIANCE_DATA_END
