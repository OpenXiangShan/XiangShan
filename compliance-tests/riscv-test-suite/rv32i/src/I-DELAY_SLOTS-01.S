# RISC-V Compliance Test I-DELAY_SLOTS-01
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
# Description: Testing delay slots of jump and branch instructions.

#include "compliance_test.h"
#include "compliance_io.h"
#include "test_macros.h"

# Test Virtual Machine (TVM) used by program.
RV_COMPLIANCE_RV32M

# Test code region
RV_COMPLIANCE_CODE_BEGIN

    RVTEST_IO_INIT
    RVTEST_IO_ASSERT_GPR_EQ(x31, x0, 0x00000000)
    RVTEST_IO_WRITE_STR(x31, "# Test Begin\n")

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part A1 - test JAL\n");

    # Address for test results
    la      x1, test_A1_res

    # Test
    li      x2, 0x11111111
    jal     x0, 1f
    li      x2, 0
1:

    # Store results
    sw      x2, 0(x1)

    //
    // Assert
    //
    RVTEST_IO_CHECK()
    RVTEST_IO_ASSERT_GPR_EQ(x1, x2, 0x11111111)

    RVTEST_IO_WRITE_STR(x31, "# Test part A1  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part A2 - test JALR\n");

    # Address for test results
    la      x1, test_A2_res

    # Test
    li      x2, 0x22222222
    la      x4, 1f
    jalr    x0, x4, 0
    li      x2, 0
1:

    # Store results
    sw      x2, 0(x1)

    RVTEST_IO_ASSERT_GPR_EQ(x1, x2, 0x22222222)

    RVTEST_IO_WRITE_STR(x31, "# Test part A2  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part B1 - test BEQ\n");

    # Address for test results
    la      x1, test_B1_res

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    li      x2, 0x33333333
    beq     x5, x5, 1f
    li      x2, 0
1:

    # Store results
    sw      x2, 0(x1)

    RVTEST_IO_ASSERT_GPR_EQ(x1, x2, 0x33333333)

    RVTEST_IO_WRITE_STR(x31, "# Test part A3  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part B2 - test BNE\n");

    # Address for test results
    la      x1, test_B2_res

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    li      x2, 0x44444444
    bne     x5, x6, 1f
    li      x2, 0
1:

    # Store results
    sw      x2, 0(x1)

    RVTEST_IO_ASSERT_GPR_EQ(x1, x2, 0x44444444)

    RVTEST_IO_WRITE_STR(x31, "# Test part A4  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part B3 - test BLT\n");

    # Address for test results
    la      x1, test_B3_res

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    li      x2, 0x55555555
    blt     x5, x6, 1f
    li      x2, 0
1:

    # Store results
    sw      x2, 0(x1)

    RVTEST_IO_ASSERT_GPR_EQ(x1, x2, 0x55555555)

    RVTEST_IO_WRITE_STR(x31, "# Test part A5  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part B4 - test BLTU\n");

    # Address for test results
    la      x1, test_B4_res

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    li      x2, 0x66666666
    bltu    x5, x6, 1f
    li      x2, 0
1:

    # Store results
    sw      x2, 0(x1)

    RVTEST_IO_ASSERT_GPR_EQ(x1, x2, 0x66666666)

    RVTEST_IO_WRITE_STR(x31, "# Test part B  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part B5 - test BGE\n");

    # Address for test results
    la      x1, test_B5_res

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    li      x2, 0x77777777
    bge     x6, x5, 1f
    li      x2, 0
1:

    # Store results
    sw      x2, 0(x1)

    RVTEST_IO_ASSERT_GPR_EQ(x1, x2, 0x77777777)

    RVTEST_IO_WRITE_STR(x31, "# Test part C  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part B6 - test BGEU\n");

    # Address for test results
    la      x1, test_B6_res

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    li      x2, 0x88888888
    bgeu    x6, x5, 1f
    li      x2, 0
1:

    # Store results
    sw      x2, 0(x1)

    RVTEST_IO_ASSERT_GPR_EQ(x1, x2, 0x88888888)

    RVTEST_IO_WRITE_STR(x31, "# Test part D  - Complete\n");

    RVTEST_IO_WRITE_STR(x31, "# Test End\n")

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

test_A1_res:
    .fill 1, 4, -1
test_A2_res:
    .fill 1, 4, -1
test_B1_res:
    .fill 1, 4, -1
test_B2_res:
    .fill 1, 4, -1
test_B3_res:
    .fill 1, 4, -1
test_B4_res:
    .fill 1, 4, -1
test_B5_res:
    .fill 1, 4, -1
test_B6_res:
    .fill 1, 4, -1

RV_COMPLIANCE_DATA_END
