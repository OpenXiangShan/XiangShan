# RISC-V Compliance Test LUI-01
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
# Description: Testing instruction 'LUI'.

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

    TEST_CASE(x6, x0, 0x0, x5, 0, \
    lui x0, 0x0; \
    )
    TEST_CASE(x6, x1, 0x0, x5, 4, \
    lui x1, 0x0; \
    )
    TEST_CASE(x6, x2, 0x7ff000, x5, 8, \
    lui x2, 0x7ff; \
    )
    TEST_CASE(x6, x3, 0x1000, x5, 12, \
    lui x3, 0x1; \
    )
    TEST_CASE(x6, x4, 0x1234000, x5, 16, \
    lui x4, 0x1234; \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 2\n")


    # address for test results
	la	x1, test_2_res

    TEST_CASE(x2, x5, 0x80000000, x1, 0, \
    lui x5, 0x80000; \
    )
    TEST_CASE(x2, x6, 0x1234000, x1, 4, \
    lui x6, 0x1234; \
    )
    TEST_CASE(x2, x7, 0xfffff000, x1, 8, \
    lui x7, 0xfffff; \
    )
    TEST_CASE(x2, x8, 0x1000, x1, 12, \
    lui x8, 0x1; \
    )
    TEST_CASE(x2, x9, 0x7ffff000, x1, 16, \
    lui x9, 0x7ffff; \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 3\n")


    # address for test results
	la	x1, test_3_res

    TEST_CASE(x7, x10, 0x54321000, x1, 0, \
    lui x10, 0x54321; \
    )
    TEST_CASE(x7, x11, 0x800000, x1, 4, \
    lui x11, 0x800; \
    )
    TEST_CASE(x7, x12, 0x0, x1, 8, \
    lui x12, 0x0; \
    )
    TEST_CASE(x7, x13, 0x7ff000, x1, 12, \
    lui x13, 0x7ff; \
    )
    TEST_CASE(x7, x14, 0x0, x1, 16, \
    lui x14, 0x0; \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 4\n")


    # address for test results
	la	x2, test_4_res

    TEST_CASE(x3, x15, 0x1000, x2, 0, \
    lui x15, 0x1; \
    )
    TEST_CASE(x3, x16, 0x0, x2, 4, \
    lui x16, 0x0; \
    )
    TEST_CASE(x3, x17, 0x0, x2, 8, \
    lui x17, 0x0; \
    )
    TEST_CASE(x3, x18, 0x7ff000, x2, 12, \
    lui x18, 0x7ff; \
    )
    TEST_CASE(x3, x19, 0x1000, x2, 16, \
    lui x19, 0x1; \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 5\n")


    # address for test results
	la	x1, test_5_res

    TEST_CASE(x2, x20, 0x1234000, x1, 0, \
    lui x20, 0x1234; \
    )
    TEST_CASE(x2, x21, 0x80000000, x1, 4, \
    lui x21, 0x80000; \
    )
    TEST_CASE(x2, x22, 0x1234000, x1, 8, \
    lui x22, 0x1234; \
    )
    TEST_CASE(x2, x23, 0xfffff000, x1, 12, \
    lui x23, 0xfffff; \
    )
    TEST_CASE(x2, x24, 0x1000, x1, 16, \
    lui x24, 0x1; \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 6\n")


    # address for test results
	la	x1, test_6_res

    TEST_CASE(x7, x25, 0x7ffff000, x1, 0, \
    lui x25, 0x7ffff; \
    )
    TEST_CASE(x7, x26, 0x54321000, x1, 4, \
    lui x26, 0x54321; \
    )
    TEST_CASE(x7, x27, 0x800000, x1, 8, \
    lui x27, 0x800; \
    )
    TEST_CASE(x7, x28, 0x0, x1, 12, \
    lui x28, 0x0; \
    )
    TEST_CASE(x7, x29, 0x7ff000, x1, 16, \
    lui x29, 0x7ff; \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 7\n")


    # address for test results
	la	x2, test_7_res

    TEST_CASE(x3, x30, 0x0, x2, 0, \
    lui x30, 0x0; \
    )
    TEST_CASE(x3, x31, 0x1000, x2, 4, \
    lui x31, 0x1; \
    )


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

