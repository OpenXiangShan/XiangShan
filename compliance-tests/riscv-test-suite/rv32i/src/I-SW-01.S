# RISC-V Compliance Test I-SW-01
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
# Description: Testing instruction 'SW'.

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


    # Addresses for test results
	la	x5, test_1_res

    # Clear memory
    sw      x0, 0(x5)
    # Load values and compute offsets
    li      x16, -0x1
    addi    x31, x5,0
    addi    x31, x31, 0x7d0
    # Test Instruction 
    sw      x16, -0x7d0(x31)
    # Check results:  mem[test_1_res+0] = -0x1
    lw x7, 0(x5)
    RVTEST_IO_ASSERT_GPR_EQ(x6, x7, -0x1)

    # Clear memory
    sw      x0, 4(x5)
    # Load values and compute offsets
    li      x15, 0x1
    addi    x30, x5,4
    addi    x30, x30, 0x0
    # Test Instruction 
    sw      x15, 0x0(x30)
    # Check results:  mem[test_1_res+4] = 0x1
    lw x7, 4(x5)
    RVTEST_IO_ASSERT_GPR_EQ(x6, x7, 0x1)

    # Clear memory
    sw      x0, 8(x5)
    # Load values and compute offsets
    li      x14, 0x0
    addi    x29, x5,8
    addi    x29, x29, 0x1
    # Test Instruction 
    sw      x14, -0x1(x29)
    # Check results:  mem[test_1_res+8] = 0x0
    lw x7, 8(x5)
    RVTEST_IO_ASSERT_GPR_EQ(x6, x7, 0x0)

    # Clear memory
    sw      x0, 12(x5)
    # Load values and compute offsets
    li      x13, 0x7ff
    addi    x28, x5,12
    addi    x28, x28, 0x7d0
    # Test Instruction 
    sw      x13, -0x7d0(x28)
    # Check results:  mem[test_1_res+12] = 0x7ff
    lw x7, 12(x5)
    RVTEST_IO_ASSERT_GPR_EQ(x6, x7, 0x7ff)

    # Clear memory
    sw      x0, 16(x5)
    # Load values and compute offsets
    li      x12, 0x0
    addi    x27, x5,16
    addi    x27, x27, -0x7d0
    # Test Instruction 
    sw      x12, 0x7d0(x27)
    # Check results:  mem[test_1_res+16] = 0x0
    lw x7, 16(x5)
    RVTEST_IO_ASSERT_GPR_EQ(x6, x7, 0x0)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 2\n")


    # Addresses for test results
	la	x1, test_2_res

    # Clear memory
    sw      x0, 0(x1)
    # Load values and compute offsets
    li      x11, 0x800
    addi    x26, x1,0
    addi    x26, x26, -0x7d0
    # Test Instruction 
    sw      x11, 0x7d0(x26)
    # Check results:  mem[test_2_res+0] = 0x800
    lw x3, 0(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x3, 0x800)

    # Clear memory
    sw      x0, 4(x1)
    # Load values and compute offsets
    li      x10, 0x7654321
    addi    x25, x1,4
    addi    x25, x25, -0x7d0
    # Test Instruction 
    sw      x10, 0x7d0(x25)
    # Check results:  mem[test_2_res+4] = 0x7654321
    lw x3, 4(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x3, 0x7654321)

    # Clear memory
    sw      x0, 8(x1)
    # Load values and compute offsets
    li      x9, 0x7fffffff
    addi    x24, x1,8
    addi    x24, x24, -0x1
    # Test Instruction 
    sw      x9, 0x1(x24)
    # Check results:  mem[test_2_res+8] = 0x7fffffff
    lw x3, 8(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x3, 0x7fffffff)

    # Clear memory
    sw      x0, 12(x1)
    # Load values and compute offsets
    li      x8, 0x1
    addi    x23, x1,12
    addi    x23, x23, -0x7d0
    # Test Instruction 
    sw      x8, 0x7d0(x23)
    # Check results:  mem[test_2_res+12] = 0x1
    lw x3, 12(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x3, 0x1)

    # Clear memory
    sw      x0, 16(x1)
    # Load values and compute offsets
    li      x7, 0xffffffff
    addi    x22, x1,16
    addi    x22, x22, -0x7d0
    # Test Instruction 
    sw      x7, 0x7d0(x22)
    # Check results:  mem[test_2_res+16] = 0xffffffff
    lw x3, 16(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x3, 0xffffffff)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 3\n")


    # Addresses for test results
	la	x1, test_3_res

    # Clear memory
    sw      x0, 0(x1)
    # Load values and compute offsets
    li      x6, 0x1234
    addi    x21, x1,0
    addi    x21, x21, -0x7d0
    # Test Instruction 
    sw      x6, 0x7d0(x21)
    # Check results:  mem[test_3_res+0] = 0x1234
    lw x8, 0(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x8, 0x1234)

    # Clear memory
    sw      x0, 4(x1)
    # Load values and compute offsets
    li      x5, 0x80000000
    addi    x20, x1,4
    addi    x20, x20, 0x0
    # Test Instruction 
    sw      x5, 0x0(x20)
    # Check results:  mem[test_3_res+4] = 0x80000000
    lw x8, 4(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x8, 0x80000000)

    # Clear memory
    sw      x0, 8(x1)
    # Load values and compute offsets
    li      x4, -0x1234
    addi    x19, x1,8
    addi    x19, x19, -0x7d0
    # Test Instruction 
    sw      x4, 0x7d0(x19)
    # Check results:  mem[test_3_res+8] = -0x1234
    lw x8, 8(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x8, -0x1234)

    # Clear memory
    sw      x0, 12(x1)
    # Load values and compute offsets
    li      x3, -0x1
    addi    x18, x1,12
    addi    x18, x18, 0x1
    # Test Instruction 
    sw      x3, -0x1(x18)
    # Check results:  mem[test_3_res+12] = -0x1
    lw x8, 12(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x8, -0x1)

    # Clear memory
    sw      x0, 16(x1)
    # Load values and compute offsets
    li      x2, -0x7ff
    addi    x17, x1,16
    addi    x17, x17, 0x0
    # Test Instruction 
    sw      x2, 0x0(x17)
    # Check results:  mem[test_3_res+16] = -0x7ff
    lw x8, 16(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x8, -0x7ff)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 4\n")


    # Addresses for test results
	la	x2, test_4_res

    # Clear memory
    sw      x0, 0(x2)
    # Load values and compute offsets
    li      x1, -0x2
    addi    x16, x2,0
    addi    x16, x16, -0x1
    # Test Instruction 
    sw      x1, 0x1(x16)
    # Check results:  mem[test_4_res+0] = -0x2
    lw x4, 0(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x4, -0x2)

    # Clear memory
    sw      x0, 4(x2)
    # Load values and compute offsets
    li      x0, -0x1
    addi    x15, x2,4
    addi    x15, x15, 0x7d0
    # Test Instruction 
    sw      x0, -0x7d0(x15)
    # Check results:  mem[test_4_res+4] = 0
    lw x4, 4(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x4, 0)

    # Clear memory
    sw      x0, 8(x2)
    # Load values and compute offsets
    li      x31, 0x1
    addi    x14, x2,8
    addi    x14, x14, 0x0
    # Test Instruction 
    sw      x31, 0x0(x14)
    # Check results:  mem[test_4_res+8] = 0x1
    lw x4, 8(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x4, 0x1)

    # Clear memory
    sw      x0, 12(x2)
    # Load values and compute offsets
    li      x30, 0x0
    addi    x13, x2,12
    addi    x13, x13, 0x1
    # Test Instruction 
    sw      x30, -0x1(x13)
    # Check results:  mem[test_4_res+12] = 0x0
    lw x4, 12(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x4, 0x0)

    # Clear memory
    sw      x0, 16(x2)
    # Load values and compute offsets
    li      x29, 0x7ff
    addi    x12, x2,16
    addi    x12, x12, 0x7d0
    # Test Instruction 
    sw      x29, -0x7d0(x12)
    # Check results:  mem[test_4_res+16] = 0x7ff
    lw x4, 16(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x4, 0x7ff)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 5\n")


    # Addresses for test results
	la	x1, test_5_res

    # Clear memory
    sw      x0, 0(x1)
    # Load values and compute offsets
    li      x28, 0x0
    addi    x11, x1,0
    addi    x11, x11, -0x7d0
    # Test Instruction 
    sw      x28, 0x7d0(x11)
    # Check results:  mem[test_5_res+0] = 0x0
    lw x3, 0(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x3, 0x0)

    # Clear memory
    sw      x0, 4(x1)
    # Load values and compute offsets
    li      x27, 0x800
    addi    x10, x1,4
    addi    x10, x10, -0x7d0
    # Test Instruction 
    sw      x27, 0x7d0(x10)
    # Check results:  mem[test_5_res+4] = 0x800
    lw x3, 4(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x3, 0x800)

    # Clear memory
    sw      x0, 8(x1)
    # Load values and compute offsets
    li      x26, 0x7654321
    addi    x9, x1,8
    addi    x9, x9, -0x7d0
    # Test Instruction 
    sw      x26, 0x7d0(x9)
    # Check results:  mem[test_5_res+8] = 0x7654321
    lw x3, 8(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x3, 0x7654321)

    # Clear memory
    sw      x0, 12(x1)
    # Load values and compute offsets
    li      x25, 0x7fffffff
    addi    x8, x1,12
    addi    x8, x8, -0x1
    # Test Instruction 
    sw      x25, 0x1(x8)
    # Check results:  mem[test_5_res+12] = 0x7fffffff
    lw x3, 12(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x3, 0x7fffffff)

    # Clear memory
    sw      x0, 16(x1)
    # Load values and compute offsets
    li      x24, 0x1
    addi    x7, x1,16
    addi    x7, x7, -0x7d0
    # Test Instruction 
    sw      x24, 0x7d0(x7)
    # Check results:  mem[test_5_res+16] = 0x1
    lw x3, 16(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x2, x3, 0x1)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 6\n")


    # Addresses for test results
	la	x1, test_6_res

    # Clear memory
    sw      x0, 0(x1)
    # Load values and compute offsets
    li      x23, 0xffffffff
    addi    x6, x1,0
    addi    x6, x6, -0x7d0
    # Test Instruction 
    sw      x23, 0x7d0(x6)
    # Check results:  mem[test_6_res+0] = 0xffffffff
    lw x8, 0(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x8, 0xffffffff)

    # Clear memory
    sw      x0, 4(x1)
    # Load values and compute offsets
    li      x22, 0x1234
    addi    x5, x1,4
    addi    x5, x5, -0x7d0
    # Test Instruction 
    sw      x22, 0x7d0(x5)
    # Check results:  mem[test_6_res+4] = 0x1234
    lw x8, 4(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x8, 0x1234)

    # Clear memory
    sw      x0, 8(x1)
    # Load values and compute offsets
    li      x21, 0x80000000
    addi    x4, x1,8
    addi    x4, x4, 0x0
    # Test Instruction 
    sw      x21, 0x0(x4)
    # Check results:  mem[test_6_res+8] = 0x80000000
    lw x8, 8(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x8, 0x80000000)

    # Clear memory
    sw      x0, 12(x1)
    # Load values and compute offsets
    li      x20, -0x1234
    addi    x3, x1,12
    addi    x3, x3, -0x7d0
    # Test Instruction 
    sw      x20, 0x7d0(x3)
    # Check results:  mem[test_6_res+12] = -0x1234
    lw x8, 12(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x8, -0x1234)

    # Clear memory
    sw      x0, 16(x1)
    # Load values and compute offsets
    li      x19, -0x1
    addi    x2, x1,16
    addi    x2, x2, 0x1
    # Test Instruction 
    sw      x19, -0x1(x2)
    # Check results:  mem[test_6_res+16] = -0x1
    lw x8, 16(x1)
    RVTEST_IO_ASSERT_GPR_EQ(x7, x8, -0x1)



	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 7\n")


    # Addresses for test results
	la	x2, test_7_res

    # Clear memory
    sw      x0, 0(x2)
    # Load values and compute offsets
    li      x18, -0x7ff
    addi    x1, x2,0
    addi    x1, x1, 0x0
    # Test Instruction 
    sw      x18, 0x0(x1)
    # Check results:  mem[test_7_res+0] = -0x7ff
    lw x4, 0(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x4, -0x7ff)

    # Clear memory
    sw      x0, 4(x2)
    # Load values and compute offsets
    li      x17, -0x2
    addi    x1, x2,4
    addi    x1, x1, -0x1
    # Test Instruction 
    sw      x17, 0x1(x1)
    # Check results:  mem[test_7_res+4] = -0x2
    lw x4, 4(x2)
    RVTEST_IO_ASSERT_GPR_EQ(x3, x4, -0x2)



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

