# RISC-V Compliance Test SLTIU-01
#
# Copyright (c) 2019, Imperas Software Ltd.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#      * Redistributions of source code must retain the above copyright
#        notice, this list of conditions and the following disclaimer.
#      * Redistributions in binary form must reproduce the above copyright
#        notice, this list of conditions and the following disclaimer in the
#        documentation and/or other materials provided with the distribution.
#      * Neither the name of the Imperas Software Ltd. nor the
#        names of its contributors may be used to endorse or promote products
#        derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
# THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Imperas Software Ltd. BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# Specification: RV32I Base Integer Instruction Set, Version 2.1
# Description: Testing instruction 'SLTIU'.

#include "riscv_test_macros.h"
#include "compliance_test.h"
#include "compliance_io.h"

RV_COMPLIANCE_RV32M

RV_COMPLIANCE_CODE_BEGIN


	RVTEST_IO_INIT
	RVTEST_IO_ASSERT_GPR_EQ(x31, x0, 0x00000000)
	RVTEST_IO_WRITE_STR(x31, "Test Begin\n")

	# ---------------------------------------------------------------------------------------------

	RVTEST_IO_WRITE_STR(x31, "# Test number 1\n")


    # address for test results
	la	x5, test_1_res

	TEST_IMM_OP(sltiu, x0, x31, 0x0, -0x1, 0x0, x5, 0, x6)   # Testcase 0
	TEST_IMM_OP(sltiu, x1, x30, 0x1, 0x1, -0x7ff, x5, 4, x6)   # Testcase 1
	TEST_IMM_OP(sltiu, x2, x29, 0x1, 0x0, -0x1, x5, 8, x6)   # Testcase 2
	TEST_IMM_OP(sltiu, x3, x28, 0x1, 0x7ff, -0x800, x5, 12, x6)   # Testcase 3
	TEST_IMM_OP(sltiu, x4, x27, 0x1, 0x0, 0x800, x5, 16, x6)   # Testcase 4


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 2\n")


    # address for test results
	la	x1, test_2_res

	TEST_IMM_OP(sltiu, x5, x26, 0x1, 0x800, 0x800, x1, 0, x2)   # Testcase 5
	TEST_IMM_OP(sltiu, x6, x25, 0x1, 0x7654321, 0x800, x1, 4, x2)   # Testcase 6
	TEST_IMM_OP(sltiu, x7, x24, 0x0, 0x7fffffff, 0x1, x1, 8, x2)   # Testcase 7
	TEST_IMM_OP(sltiu, x8, x23, 0x1, 0x1, 0x800, x1, 12, x2)   # Testcase 8
	TEST_IMM_OP(sltiu, x9, x22, 0x0, 0xffffffff, 0x800, x1, 16, x2)   # Testcase 9


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 3\n")


    # address for test results
	la	x1, test_3_res

	TEST_IMM_OP(sltiu, x10, x21, 0x1, 0x1234, 0x800, x1, 0, x7)   # Testcase 10
	TEST_IMM_OP(sltiu, x11, x20, 0x0, 0x80000000, 0x0, x1, 4, x7)   # Testcase 11
	TEST_IMM_OP(sltiu, x12, x19, 0x0, -0x1234, 0x7ff, x1, 8, x7)   # Testcase 12
	TEST_IMM_OP(sltiu, x13, x18, 0x0, -0x1, -0x1, x1, 12, x7)   # Testcase 13
	TEST_IMM_OP(sltiu, x14, x17, 0x0, -0x7ff, 0x1, x1, 16, x7)   # Testcase 14


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 4\n")


    # address for test results
	la	x2, test_4_res

	TEST_IMM_OP(sltiu, x15, x16, 0x0, 0x0, 0x0, x2, 0, x3)   # Testcase 15
	TEST_IMM_OP(sltiu, x16, x15, 0x0, -0x1, 0x0, x2, 4, x3)   # Testcase 16
	TEST_IMM_OP(sltiu, x17, x14, 0x1, 0x1, -0x7ff, x2, 8, x3)   # Testcase 17
	TEST_IMM_OP(sltiu, x18, x13, 0x1, 0x0, -0x1, x2, 12, x3)   # Testcase 18
	TEST_IMM_OP(sltiu, x19, x12, 0x1, 0x7ff, -0x800, x2, 16, x3)   # Testcase 19


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 5\n")


    # address for test results
	la	x1, test_5_res

	TEST_IMM_OP(sltiu, x20, x11, 0x1, 0x0, 0x800, x1, 0, x2)   # Testcase 20
	TEST_IMM_OP(sltiu, x21, x10, 0x1, 0x800, 0x800, x1, 4, x2)   # Testcase 21
	TEST_IMM_OP(sltiu, x22, x9, 0x1, 0x7654321, 0x800, x1, 8, x2)   # Testcase 22
	TEST_IMM_OP(sltiu, x23, x8, 0x0, 0x7fffffff, 0x1, x1, 12, x2)   # Testcase 23
	TEST_IMM_OP(sltiu, x24, x7, 0x1, 0x1, 0x800, x1, 16, x2)   # Testcase 24


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 6\n")


    # address for test results
	la	x1, test_6_res

	TEST_IMM_OP(sltiu, x25, x6, 0x0, 0xffffffff, 0x800, x1, 0, x7)   # Testcase 25
	TEST_IMM_OP(sltiu, x26, x5, 0x1, 0x1234, 0x800, x1, 4, x7)   # Testcase 26
	TEST_IMM_OP(sltiu, x27, x4, 0x0, 0x80000000, 0x0, x1, 8, x7)   # Testcase 27
	TEST_IMM_OP(sltiu, x28, x3, 0x0, -0x1234, 0x7ff, x1, 12, x7)   # Testcase 28
	TEST_IMM_OP(sltiu, x29, x2, 0x0, -0x1, -0x1, x1, 16, x7)   # Testcase 29


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 7\n")


    # address for test results
	la	x2, test_7_res

	TEST_IMM_OP(sltiu, x30, x1, 0x0, -0x7ff, 0x1, x2, 0, x3)   # Testcase 30
	TEST_IMM_OP(sltiu, x31, x0, 0x0, 0x0, 0x0, x2, 4, x3)   # Testcase 31


	# ---------------------------------------------------------------------------------------------
	
	RVTEST_IO_WRITE_STR(x31, "Test End\n")

	# ---------------------------------------------------------------------------------------------

	RV_COMPLIANCE_HALT

RV_COMPLIANCE_CODE_END

# Input data section.
	.data

# Output data section.
RV_COMPLIANCE_DATA_BEGIN

test_1_res:
	.fill 5, 4, -1
test_2_res:
	.fill 5, 4, -1
test_3_res:
	.fill 5, 4, -1
test_4_res:
	.fill 5, 4, -1
test_5_res:
	.fill 5, 4, -1
test_6_res:
	.fill 5, 4, -1
test_7_res:
	.fill 5, 4, -1

RV_COMPLIANCE_DATA_END

