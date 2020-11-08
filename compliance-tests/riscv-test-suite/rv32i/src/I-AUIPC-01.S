# RISC-V Compliance Test I-AUIPC-01
#
#
# Copyright (c) 2019 Imperas Software Ltd., www.imperas.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
# either express or implied.
#
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
#
# Specification: RV32I Base Integer Instruction Set, Version 2.1
# Description: Testing instruction 'AUIPC'.

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

	# Testcase 0:  imm:0x0, result rd:x0(0x0)
1: 
    auipc x0, 0x0
    la x7, 1b
    sub  x0,  x0, x7
	sw  x0, 0(x5)
    RVTEST_IO_ASSERT_GPR_EQ(x6, x0, 0x0)

	# Testcase 1:  imm:0x0, result rd:x1(0x0)
1: 
    auipc x1, 0x0
    la x7, 1b
    sub  x1,  x1, x7
	sw  x1, 4(x5)
    RVTEST_IO_ASSERT_GPR_EQ(x6, x1, 0x0)

	# Testcase 2:  imm:0x7ff, result rd:x2(0x7ff000)
1: 
    auipc x2, 0x7ff
    la x7, 1b
    sub  x2,  x2, x7
	sw  x2, 8(x5)
    RVTEST_IO_ASSERT_GPR_EQ(x6, x2, 0x7ff000)

	# Testcase 3:  imm:0x1, result rd:x3(0x1000)
1: 
    auipc x3, 0x1
    la x7, 1b
    sub  x3,  x3, x7
	sw  x3, 12(x5)
    RVTEST_IO_ASSERT_GPR_EQ(x6, x3, 0x1000)

	# Testcase 4:  imm:0x1234, result rd:x4(0x1234000)
1: 
    auipc x4, 0x1234
    la x7, 1b
    sub  x4,  x4, x7
	sw  x4, 16(x5)
    RVTEST_IO_ASSERT_GPR_EQ(x6, x4, 0x1234000)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 2\n")


    # address for test results
	la	x1, test_2_res

	# Testcase 5:  imm:0x80000, result rd:x5(0x80000000)
1: 
    auipc x5, 0x80000
    la x3, 1b
    sub  x5,  x5, x3
	sw  x5, 0(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x5, 0x80000000)

	# Testcase 6:  imm:0x1234, result rd:x6(0x1234000)
1: 
    auipc x6, 0x1234
    la x3, 1b
    sub  x6,  x6, x3
	sw  x6, 4(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x6, 0x1234000)

	# Testcase 7:  imm:0xfffff, result rd:x7(0xfffff000)
1: 
    auipc x7, 0xfffff
    la x3, 1b
    sub  x7,  x7, x3
	sw  x7, 8(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x7, 0xfffff000)

	# Testcase 8:  imm:0x1, result rd:x8(0x1000)
1: 
    auipc x8, 0x1
    la x3, 1b
    sub  x8,  x8, x3
	sw  x8, 12(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x8, 0x1000)

	# Testcase 9:  imm:0x7ffff, result rd:x9(0x7ffff000)
1: 
    auipc x9, 0x7ffff
    la x3, 1b
    sub  x9,  x9, x3
	sw  x9, 16(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x9, 0x7ffff000)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 3\n")


    # address for test results
	la	x1, test_3_res

	# Testcase 10:  imm:0x54321, result rd:x10(0x54321000)
1: 
    auipc x10, 0x54321
    la x8, 1b
    sub  x10,  x10, x8
	sw  x10, 0(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x10, 0x54321000)

	# Testcase 11:  imm:0x800, result rd:x11(0x800000)
1: 
    auipc x11, 0x800
    la x8, 1b
    sub  x11,  x11, x8
	sw  x11, 4(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x11, 0x800000)

	# Testcase 12:  imm:0x0, result rd:x12(0x0)
1: 
    auipc x12, 0x0
    la x8, 1b
    sub  x12,  x12, x8
	sw  x12, 8(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x12, 0x0)

	# Testcase 13:  imm:0x7ff, result rd:x13(0x7ff000)
1: 
    auipc x13, 0x7ff
    la x8, 1b
    sub  x13,  x13, x8
	sw  x13, 12(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x13, 0x7ff000)

	# Testcase 14:  imm:0x0, result rd:x14(0x0)
1: 
    auipc x14, 0x0
    la x8, 1b
    sub  x14,  x14, x8
	sw  x14, 16(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x14, 0x0)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 4\n")


    # address for test results
	la	x2, test_4_res

	# Testcase 15:  imm:0x1, result rd:x15(0x1000)
1: 
    auipc x15, 0x1
    la x4, 1b
    sub  x15,  x15, x4
	sw  x15, 0(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x15, 0x1000)

	# Testcase 16:  imm:0x0, result rd:x16(0x0)
1: 
    auipc x16, 0x0
    la x4, 1b
    sub  x16,  x16, x4
	sw  x16, 4(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x16, 0x0)

	# Testcase 17:  imm:0x0, result rd:x17(0x0)
1: 
    auipc x17, 0x0
    la x4, 1b
    sub  x17,  x17, x4
	sw  x17, 8(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x17, 0x0)

	# Testcase 18:  imm:0x7ff, result rd:x18(0x7ff000)
1: 
    auipc x18, 0x7ff
    la x4, 1b
    sub  x18,  x18, x4
	sw  x18, 12(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x18, 0x7ff000)

	# Testcase 19:  imm:0x1, result rd:x19(0x1000)
1: 
    auipc x19, 0x1
    la x4, 1b
    sub  x19,  x19, x4
	sw  x19, 16(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x19, 0x1000)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 5\n")


    # address for test results
	la	x1, test_5_res

	# Testcase 20:  imm:0x1234, result rd:x20(0x1234000)
1: 
    auipc x20, 0x1234
    la x3, 1b
    sub  x20,  x20, x3
	sw  x20, 0(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x20, 0x1234000)

	# Testcase 21:  imm:0x80000, result rd:x21(0x80000000)
1: 
    auipc x21, 0x80000
    la x3, 1b
    sub  x21,  x21, x3
	sw  x21, 4(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x21, 0x80000000)

	# Testcase 22:  imm:0x1234, result rd:x22(0x1234000)
1: 
    auipc x22, 0x1234
    la x3, 1b
    sub  x22,  x22, x3
	sw  x22, 8(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x22, 0x1234000)

	# Testcase 23:  imm:0xfffff, result rd:x23(0xfffff000)
1: 
    auipc x23, 0xfffff
    la x3, 1b
    sub  x23,  x23, x3
	sw  x23, 12(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x23, 0xfffff000)

	# Testcase 24:  imm:0x1, result rd:x24(0x1000)
1: 
    auipc x24, 0x1
    la x3, 1b
    sub  x24,  x24, x3
	sw  x24, 16(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x24, 0x1000)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 6\n")


    # address for test results
	la	x1, test_6_res

	# Testcase 25:  imm:0x7ffff, result rd:x25(0x7ffff000)
1: 
    auipc x25, 0x7ffff
    la x8, 1b
    sub  x25,  x25, x8
	sw  x25, 0(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x25, 0x7ffff000)

	# Testcase 26:  imm:0x54321, result rd:x26(0x54321000)
1: 
    auipc x26, 0x54321
    la x8, 1b
    sub  x26,  x26, x8
	sw  x26, 4(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x26, 0x54321000)

	# Testcase 27:  imm:0x800, result rd:x27(0x800000)
1: 
    auipc x27, 0x800
    la x8, 1b
    sub  x27,  x27, x8
	sw  x27, 8(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x27, 0x800000)

	# Testcase 28:  imm:0x0, result rd:x28(0x0)
1: 
    auipc x28, 0x0
    la x8, 1b
    sub  x28,  x28, x8
	sw  x28, 12(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x28, 0x0)

	# Testcase 29:  imm:0x7ff, result rd:x29(0x7ff000)
1: 
    auipc x29, 0x7ff
    la x8, 1b
    sub  x29,  x29, x8
	sw  x29, 16(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x29, 0x7ff000)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 7\n")


    # address for test results
	la	x2, test_7_res

	# Testcase 30:  imm:0x0, result rd:x30(0x0)
1: 
    auipc x30, 0x0
    la x4, 1b
    sub  x30,  x30, x4
	sw  x30, 0(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x30, 0x0)

	# Testcase 31:  imm:0x1, result rd:x31(0x1000)
1: 
    auipc x31, 0x1
    la x4, 1b
    sub  x31,  x31, x4
	sw  x31, 4(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x31, 0x1000)



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

