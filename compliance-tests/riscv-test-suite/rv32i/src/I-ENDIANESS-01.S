# RISC-V Compliance Test I-ENDIANESS-01
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
# Description: Testing ENDIANESS of RISC-V processor.

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
    RVTEST_IO_WRITE_STR(x31, "# Test part A - test loading word by LW, LH, LB\n");

    # Addresses for test data and results
    la      x16, test_A_data
    la      x17, test_A_res

    # Test
    lw      x1, 0(x16)
    lhu     x2, 0(x16)
    lhu     x3, 2(x16)
    lbu     x4, -1(x16)
    lbu     x5, 0(x16)
    lbu     x6, 1(x16)
    lbu     x7, 2(x16)
    lbu     x8, 3(x16)

    # Store results
    sw      x1, 0(x17)
    sw      x2, 4(x17)
    sw      x3, 8(x17)
    sw      x4, 12(x17)
    sw      x5, 16(x17)
    sw      x6, 20(x17)
    sw      x7, 24(x17)
    sw      x8, 28(x17)

    //
    // Assert
    //
    RVTEST_IO_CHECK()
    RVTEST_IO_ASSERT_GPR_EQ(x17, x1, 0x01234567)
    RVTEST_IO_ASSERT_GPR_EQ(x17, x2, 0x00004567)
    RVTEST_IO_ASSERT_GPR_EQ(x17, x3, 0x00000123)
    RVTEST_IO_ASSERT_GPR_EQ(x17, x4, 0x00000089)
    RVTEST_IO_ASSERT_GPR_EQ(x17, x5, 0x00000067)
    RVTEST_IO_ASSERT_GPR_EQ(x17, x6, 0x00000045)
    RVTEST_IO_ASSERT_GPR_EQ(x17, x7, 0x00000023)
    RVTEST_IO_ASSERT_GPR_EQ(x17, x8, 0x00000001)

    RVTEST_IO_WRITE_STR(x31, "# Test part A1  - Complete\n");

    RVTEST_IO_WRITE_STR(x31, "# Test End\n")

 # ---------------------------------------------------------------------------------------------
    # HALT
    RV_COMPLIANCE_HALT

RV_COMPLIANCE_CODE_END

# Input data section.
    .data
    .align 4
    .word 0x89ABCDEF
test_A_data:
    .word 0x01234567

# Output data section.
RV_COMPLIANCE_DATA_BEGIN
    .align 4

test_A_res:
    .fill 8, 4, -1

RV_COMPLIANCE_DATA_END
