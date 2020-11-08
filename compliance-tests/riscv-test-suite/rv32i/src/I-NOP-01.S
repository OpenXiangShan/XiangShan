# RISC-V Compliance Test I-NOP-01
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
# Description: Testing pseudo instruction NOP.

#include "compliance_test.h"
#include "compliance_io.h"
#include "test_macros.h"

# Test Virtual Machine (TVM) used by program.
RV_COMPLIANCE_RV32M

# Test code region.
RV_COMPLIANCE_CODE_BEGIN

    RVTEST_IO_INIT
    RVTEST_IO_ASSERT_GPR_EQ(x31, x0, 0x00000000)
    RVTEST_IO_WRITE_STR(x31, "# Test Begin\n")

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part A1 - general test, instruction should not change any register\n");

    # Addresses for test data and results
    la      x1, test_A1_res

    # Register initialization
    li      x2, 2
    li      x3, 3
    li      x4, 4
    li      x5, 5
    li      x6, 6
    li      x7, 7
    li      x8, 8
    li      x9, 9
    li      x10, 10
    li      x11, 11
    li      x12, 12
    li      x13, 13
    li      x14, 14
    li      x15, 15
    li      x16, 16
    li      x17, 17
    li      x18, 18
    li      x19, 19
    li      x20, 20
    li      x21, 21
    li      x22, 22
    li      x23, 23
    li      x24, 24
    li      x25, 25
    li      x26, 26
    li      x27, 27
    li      x28, 28
    li      x29, 29
    li      x30, 30
    li      x31, 31

    #Test
    nop
    nop
    nop
    nop
    nop
    nop

    # Store results
    sw      x0, 0( x1 )
    sw      x2, 4( x1 )
    sw      x3, 8( x1 )
    sw      x4, 12( x1 )
    sw      x5, 16( x1 )
    sw      x6, 20( x1 )
    sw      x7, 24( x1 )
    sw      x8, 28( x1 )
    sw      x9, 32( x1 )
    sw      x10, 36( x1 )
    sw      x11, 40( x1 )
    sw      x12, 44( x1 )
    sw      x13, 48( x1 )
    sw      x14, 52( x1 )
    sw      x15, 56( x1 )
    sw      x16, 60( x1 )
    sw      x17, 64( x1 )
    sw      x18, 68( x1 )
    sw      x19, 72( x1 )
    sw      x20, 76( x1 )
    sw      x21, 80( x1 )
    sw      x22, 84( x1 )
    sw      x23, 88( x1 )
    sw      x24, 92( x1 )
    sw      x25, 96( x1 )
    sw      x26, 100( x1 )
    sw      x27, 104( x1 )
    sw      x28, 108( x1 )
    sw      x29, 112( x1 )
    sw      x30, 116( x1 )
    sw      x31, 120( x1 )

    //
    // Assert
    //
    RVTEST_IO_CHECK()
    RVTEST_IO_ASSERT_GPR_EQ(x1, x0,  0x00000000)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x2,  0x00000002)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x3,  0x00000003)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x4,  0x00000004)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x5,  0x00000005)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x6,  0x00000006)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x7,  0x00000007)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x8,  0x00000008)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x9,  0x00000009)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x10, 0x0000000A)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x11, 0x0000000B)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x12, 0x0000000C)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x13, 0x0000000D)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x14, 0x0000000E)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x15, 0x0000000F)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x16, 0x00000010)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x17, 0x00000011)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x18, 0x00000012)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x19, 0x00000013)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x20, 0x00000014)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x21, 0x00000015)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x22, 0x00000016)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x23, 0x00000017)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x24, 0x00000018)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x25, 0x00000019)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x26, 0x0000001A)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x27, 0x0000001B)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x28, 0x0000001C)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x29, 0x0000001D)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x30, 0x0000001E)
    RVTEST_IO_ASSERT_GPR_EQ(x1, x31, 0x0000001F)

    RVTEST_IO_WRITE_STR(x31, "# Test part A1  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x31, "# Test part A2 - general test, only 5 cycles are executed\n");

    # Addresses for test data and results
    la          x3, test_A2_res

    # Register initialization
    auipc   x8, 0

    #Test
    nop
    nop
    nop
    nop
    nop

    # Store results
    auipc   x9, 0
    sub     x9, x9, x8

    # Store results
    sw      x9, 0( x3 )

    RVTEST_IO_ASSERT_GPR_EQ(x3, x9, 0x00000018)

    RVTEST_IO_WRITE_STR(x31, "# Test part A2  - Complete\n");

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
    .fill 31, 4, -1
test_A2_res:
    .fill 1, 4, -1

RV_COMPLIANCE_DATA_END     # End of test output data region.
