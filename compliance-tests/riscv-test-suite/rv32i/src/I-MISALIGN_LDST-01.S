# RISC-V Compliance Test I-MISALIGN_LDST-01
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
# Description: Testing MISALIGNED LOAD/STORE exception.

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

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part A1 - test LW\n");

    # Addresses for test data and results
    la      x3, test_A1_data
    la      x2, test_A1_res
    la      x1, test_A1_res_exc

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    lw      x4, 0(x3)
    sw      x4, 0(x2)

    lw      x4, 1(x3)
    sw      x4, 4(x2)

    lw      x4, 2(x3)
    sw      x4, 8(x2)

    lw      x4, 3(x3)
    sw      x4, 12(x2)

    //
    // Assert
    //
    RVTEST_IO_CHECK()
    RVTEST_IO_WRITE_STR(x30, "# Test part A1  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part A2 - test LH and LHU\n");

    # Addresses for test data and results
    la      x3, test_A2_data
    la      x2, test_A2_res
    la      x1, test_A2_res_exc

    # Register initialization
    li      x5, 5
    li      x6, 6

    # Test
    lh      x4, 0(x3)
    sw      x4, 0(x2)

    lh      x4, 1(x3)
    sw      x4, 4(x2)

    lh      x4, 2(x3)
    sw      x4, 8(x2)

    lh      x4, 3(x3)
    sw      x4, 12(x2)

    lhu     x4, 0(x3)
    sw      x4, 16(x2)

    lhu     x4, 1(x3)
    sw      x4, 20(x2)

    lhu     x4, 2(x3)
    sw      x4, 24(x2)

    lhu     x4, 3(x3)
    sw      x4, 28(x2)

    RVTEST_IO_WRITE_STR(x30, "# Test part A2  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part B1 - test SW\n");

    # Addresses for test data and results
    la      x2, test_B1_res
    la      x1, test_B1_res_exc

    # Register initialization
    li      x6, 0x0
    li      x5, 0x99999999

    # Init memory
    sw      x5, 0(x2)
    sw      x5, 4(x2)
    sw      x5, 8(x2)
    sw      x5, 12(x2)

    # Test
    sw      x6, 0(x2)
    addi    x2, x2, 4

    sw      x6, 1(x2)
    addi    x2, x2, 4

    sw      x6, 2(x2)
    addi    x2, x2, 4

    sw      x6, 3(x2)

    RVTEST_IO_WRITE_STR(x30, "# Test part A3  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part B2 - test SH\n");

    # Addresses for test data and results
    la      x2, test_B2_res
    la      x1, test_B2_res_exc

    # Register initialization
    li      x6, 0x0
    li      x5, 0x99999999

    # Init memory
    sw      x5, 0(x2)
    sw      x5, 4(x2)
    sw      x5, 8(x2)
    sw      x5, 12(x2)

    # Test
    sh      x6, 0(x2)
    addi    x2, x2, 4

    sh      x6, 1(x2)
    addi    x2, x2, 4

    sh      x6, 2(x2)
    addi    x2, x2, 4

    sh      x6, 3(x2)

    RVTEST_IO_WRITE_STR(x30, "# Test part A4  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    # restore mtvec and jump to the end
    csrw mtvec, x31
    jal x0, test_end

    RVTEST_IO_WRITE_STR(x30, "# Test part A5  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    # Exception handler
_trap_handler:
    # increment return address
    csrr    x30, mepc
    addi    x30, x30, 4
    csrw    mepc, x30

    # store low bits of mtval
    csrr    x30, mtval
    andi    x30, x30, 3
    sw      x30, 0(x1)

    # Store MCAUSE
    csrr    x30, mcause
    sw      x30, 4(x1)

    # increment data_exc address
    addi    x1, x1, 8

    # return
    mret

    RVTEST_IO_WRITE_STR(x30, "# Test part B  - Complete\n");

    # ---------------------------------------------------------------------------------------------

test_end:

    RVTEST_IO_WRITE_STR(x30, "# Test End\n")

 # ---------------------------------------------------------------------------------------------
    # HALT
    RV_COMPLIANCE_HALT

RV_COMPLIANCE_CODE_END

# Input data section.
    .data
    .align 4

test_A1_data:
    .word 0x91A1B1C1
test_A2_data:
    .word 0xD2E2F202

# Output data section.
RV_COMPLIANCE_DATA_BEGIN
    .align 4

test_A1_res:
    .fill 4, 4, -1
test_A1_res_exc:
    .fill 6, 4, -1
test_A2_res:
    .fill 8, 4, -1
test_A2_res_exc:
    .fill 8, 4, -1
test_B1_res:
    .fill 4, 4, -1
test_B1_res_exc:
    .fill 6, 4, -1
test_B2_res:
    .fill 4, 4, -1
test_B2_res_exc:
    .fill 4, 4, -1

RV_COMPLIANCE_DATA_END
