# RISC-V Compliance Test I-RF_x0-01
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
# Description: Testing x0 is hardwired 0.

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
    RVTEST_IO_WRITE_STR(x31, "# Test part A1 - test computational instructions - register-immediate\n");

    # Address for test results
    la      x31, test_A1_res

    # Test
    lui     x0, 0xABCDE
    addi    x0, x0, 1
    ori     x0, x0, 0x7F0
    andi    x0, x0, 0x53F
    xori    x0, x0, 0xFFFFF803
    slli    x0, x0, 5
    srai    x0, x0, 2
    srli    x0, x0, 4

    # Store results
    sw      x0, 0(x31)

    //
    // Assert
    //
    RVTEST_IO_CHECK()
    RVTEST_IO_ASSERT_GPR_EQ(x31, x0, 0x00000000)

    RVTEST_IO_WRITE_STR(x31, "# Test part A1  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part A2 - test computational instructions - register-register\n");

    # Address for test results
    la      x31, test_A2_res

    # Register initialization
    li      x1,  0x1
    li      x2,  0x7F0
    li      x3,  0x53F
    li      x4,  0xFFFFF803
    li      x5,  0x5
    li      x6,  0x2
    li      x7,  0x4
    li      x8,  0x18

    # Test
    auipc   x0, 0xABCDE
    add     x0, x0, x1
    or      x0, x0, x2
    and     x0, x0, x3
    xor     x0, x0, x4
    sll     x0, x0, x5
    sra     x0, x0, x6
    srl     x0, x0, x7
    sub     x0, x0, x8

    # Store results
    sw      x0, 0(x31)

    RVTEST_IO_ASSERT_GPR_EQ(x31, x0, 0x00000000)

    RVTEST_IO_WRITE_STR(x31, "# Test part A2  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part A3 - test compare instructions\n");

    # Address for test results
    la      x31, test_A3_res

    # Register initialization
    li     x1,  0x1
    li     x2,  0x2

    # Test and store
    slt     x0, x1, x2
    sw      x0, 0(x31)

    sltu    x0, x1, x2
    sw      x0, 4(x31)

    slti    x0, x1, 2
    sw      x0, 8(x31)

    sltiu   x0, x1, 2
    sw      x0, 12(x31)

    RVTEST_IO_WRITE_STR(x31, "# Test part A3  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part A4 - test jump and link instructions\n");

    # Address for test results
    la      x31, test_A4_res

    # Test and store
    jal     x0, 1f
1:
    sw      x0, 0(x31)

    la      x1, 1f
    jalr    x0, x1, 0
1:
    sw      x0, 4(x31)

    RVTEST_IO_WRITE_STR(x31, "# Test part A4  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part A5 - test load instructions\n");

    # Addresses for test data and results
    la      x1, test_A5_data
    la      x31, test_A5_res

    # Test and store
    lw      x0, 0(x1)
    sw      x0, 0(x31)

    lh      x0, 0(x1)
    sw      x0, 4(x31)

    lb      x0, 0(x1)
    sw      x0, 8(x31)

    lbu     x0, 0(x1)
    sw      x0, 12(x31)

    RVTEST_IO_WRITE_STR(x31, "# Test part A5  - Complete\n");

    RVTEST_IO_WRITE_STR(x31, "# Test End\n")

 # ---------------------------------------------------------------------------------------------
    # HALT
    RV_COMPLIANCE_HALT

RV_COMPLIANCE_CODE_END

# Input data section.
    .data
    .align 4
test_A5_data:
    .word 0x42524E4F

# Output data section.
RV_COMPLIANCE_DATA_BEGIN
    .align 4

test_A1_res:
    .fill 1, 4, -1
test_A2_res:
    .fill 1, 4, -1
test_A3_res:
    .fill 4, 4, -1
test_A4_res:
    .fill 2, 4, -1
test_A5_res:
    .fill 4, 4, -1

RV_COMPLIANCE_DATA_END
