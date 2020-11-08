# RISC-V Compliance Test I-ECALL-01
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
# Description: Testing instruction ECALL.

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

    //
    // Assert
    //
    RVTEST_IO_CHECK()
    # ---------------------------------------------------------------------------------------------
    RVTEST_IO_WRITE_STR(x30, "# Test part A - test ECALL\n");

    # Address for test results
    la      x1, test_A_res_exc

    # Test
    li      x2, 0x11111111
    ecall
    sw      x0, 0(x1)

    RVTEST_IO_WRITE_STR(x30, "# Test part A1  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    # restore mtvec and jump to the end
    csrw mtvec, x31
    jal x0, test_end

    RVTEST_IO_WRITE_STR(x30, "# Test part A2  - Complete\n");

    # ---------------------------------------------------------------------------------------------
    # Exception handler
_trap_handler:
    # increment return address
    csrr    x30, mepc
    addi    x30, x30, 4
    csrw    mepc, x30

    # Store MCAUSE
    csrr    x30, mcause
    sw      x30, 0(x1)

    # Store data from test
    sw      x2, 4(x1)
    sw      x0, 8(x1)

    # increment data_exc address
    addi    x1, x1, 12

    # return
    mret

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


# Output data section.
RV_COMPLIANCE_DATA_BEGIN
    .align 4

test_A_res_exc:
    .fill 4, 4, -1

RV_COMPLIANCE_DATA_END
