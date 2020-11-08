# RISC-V Compliance Test JALR-01
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
# Description: Testing instruction 'JALR'.

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
    
  
    // Result = 0
    TEST_CASE(x6, x31, 0xcccc, x5, 0, \
      li x31, 0xcccc; \
      la x16, 1f - 0; \
      jalr x0, x16,  0; \
      li x31, 0x123ab; \
1: \
    )
  
    // Result = 0
    TEST_CASE(x6, x30, 0xcccc, x5, 4, \
      li x30, 0xcccc; \
      la x15, 2f - 1; \
      jalr x1, x15,  1; \
      li x30, 0x123ab; \
2: \
    )
  

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x6, x29, 0xcccc, x5, 8, \
      li x29, 0xcccc; \
      la x14, 2b - 1; \
      jalr x2, x14,  1; \
      li x29, 0x123ab; \
4: \
    )

  
    // Result = 0
    TEST_CASE(x6, x28, 0xcccc, x5, 12, \
      li x28, 0xcccc; \
      la x13, 4f - 4; \
      jalr x3, x13,  4; \
      li x28, 0x123ab; \
4: \
    )
  
    // Result = 0
    TEST_CASE(x6, x27, 0xcccc, x5, 16, \
      li x27, 0xcccc; \
      la x12, 5f - 2; \
      jalr x4, x12,  2; \
      li x27, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 2\n")


    # address for test results
	la	x1, test_2_res
    
  
    // Result = 0
    TEST_CASE(x2, x26, 0xcccc, x1, 0, \
      li x26, 0xcccc; \
      la x11, 1f - 4; \
      jalr x5, x11,  4; \
      li x26, 0x123ab; \
1: \
    )
  
    // Result = 0
    TEST_CASE(x2, x25, 0xcccc, x1, 4, \
      li x25, 0xcccc; \
      la x10, 2f - 3; \
      jalr x6, x10,  3; \
      li x25, 0x123ab; \
2: \
    )
  

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x2, x24, 0xcccc, x1, 8, \
      li x24, 0xcccc; \
      la x9, 2b - 2; \
      jalr x7, x9,  2; \
      li x24, 0x123ab; \
4: \
    )

  
    // Result = 0
    TEST_CASE(x2, x23, 0xcccc, x1, 12, \
      li x23, 0xcccc; \
      la x8, 4f - 0; \
      jalr x8, x8,  0; \
      li x23, 0x123ab; \
4: \
    )
  
    // Result = 0
    TEST_CASE(x2, x22, 0xcccc, x1, 16, \
      li x22, 0xcccc; \
      la x7, 5f - 1; \
      jalr x9, x7,  1; \
      li x22, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 3\n")


    # address for test results
	la	x1, test_3_res
    
  
    // Result = 0
    TEST_CASE(x7, x21, 0xcccc, x1, 0, \
      li x21, 0xcccc; \
      la x6, 1f - 1; \
      jalr x10, x6,  1; \
      li x21, 0x123ab; \
1: \
    )
  
    // Result = 0
    TEST_CASE(x7, x20, 0xcccc, x1, 4, \
      li x20, 0xcccc; \
      la x5, 2f - 4; \
      jalr x11, x5,  4; \
      li x20, 0x123ab; \
2: \
    )
  

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x7, x19, 0xcccc, x1, 8, \
      li x19, 0xcccc; \
      la x4, 2b - 2; \
      jalr x12, x4,  2; \
      li x19, 0x123ab; \
4: \
    )

  
    // Result = 0
    TEST_CASE(x7, x18, 0xcccc, x1, 12, \
      li x18, 0xcccc; \
      la x3, 4f - 4; \
      jalr x13, x3,  4; \
      li x18, 0x123ab; \
4: \
    )
  
    // Result = 0
    TEST_CASE(x7, x17, 0xcccc, x1, 16, \
      li x17, 0xcccc; \
      la x2, 5f - 3; \
      jalr x14, x2,  3; \
      li x17, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 4\n")


    # address for test results
	la	x2, test_4_res
    
  
    // Result = 0
    TEST_CASE(x3, x16, 0xcccc, x2, 0, \
      li x16, 0xcccc; \
      la x1, 1f - 2; \
      jalr x15, x1,  2; \
      li x16, 0x123ab; \
1: \
    )
  
    // Result = 0
    TEST_CASE(x3, x15, 0xcccc, x2, 4, \
      li x15, 0xcccc; \
      la x1, 2f - 0; \
      jalr x16, x1,  0; \
      li x15, 0x123ab; \
2: \
    )
  

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x3, x14, 0xcccc, x2, 8, \
      li x14, 0xcccc; \
      la x31, 2b - 1; \
      jalr x17, x31,  1; \
      li x14, 0x123ab; \
4: \
    )

  
    // Result = 0
    TEST_CASE(x3, x13, 0xcccc, x2, 12, \
      li x13, 0xcccc; \
      la x30, 4f - 1; \
      jalr x18, x30,  1; \
      li x13, 0x123ab; \
4: \
    )
  
    // Result = 0
    TEST_CASE(x3, x12, 0xcccc, x2, 16, \
      li x12, 0xcccc; \
      la x29, 5f - 4; \
      jalr x19, x29,  4; \
      li x12, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 5\n")


    # address for test results
	la	x1, test_5_res
    
  
    // Result = 0
    TEST_CASE(x2, x11, 0xcccc, x1, 0, \
      li x11, 0xcccc; \
      la x28, 1f - 2; \
      jalr x20, x28,  2; \
      li x11, 0x123ab; \
1: \
    )
  
    // Result = 0
    TEST_CASE(x2, x10, 0xcccc, x1, 4, \
      li x10, 0xcccc; \
      la x27, 2f - 4; \
      jalr x21, x27,  4; \
      li x10, 0x123ab; \
2: \
    )
  

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x2, x9, 0xcccc, x1, 8, \
      li x9, 0xcccc; \
      la x26, 2b - 3; \
      jalr x22, x26,  3; \
      li x9, 0x123ab; \
4: \
    )

  
    // Result = 0
    TEST_CASE(x2, x8, 0xcccc, x1, 12, \
      li x8, 0xcccc; \
      la x25, 4f - 2; \
      jalr x23, x25,  2; \
      li x8, 0x123ab; \
4: \
    )
  
    // Result = 0
    TEST_CASE(x2, x7, 0xcccc, x1, 16, \
      li x7, 0xcccc; \
      la x24, 5f - 0; \
      jalr x24, x24,  0; \
      li x7, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 6\n")


    # address for test results
	la	x1, test_6_res
    
  
    // Result = 0
    TEST_CASE(x7, x6, 0xcccc, x1, 0, \
      li x6, 0xcccc; \
      la x23, 1f - 1; \
      jalr x25, x23,  1; \
      li x6, 0x123ab; \
1: \
    )
  
    // Result = 0
    TEST_CASE(x7, x5, 0xcccc, x1, 4, \
      li x5, 0xcccc; \
      la x22, 2f - 1; \
      jalr x26, x22,  1; \
      li x5, 0x123ab; \
2: \
    )
  

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x7, x4, 0xcccc, x1, 8, \
      li x4, 0xcccc; \
      la x21, 2b - 4; \
      jalr x27, x21,  4; \
      li x4, 0x123ab; \
4: \
    )

  
    // Result = 0
    TEST_CASE(x7, x3, 0xcccc, x1, 12, \
      li x3, 0xcccc; \
      la x20, 4f - 2; \
      jalr x28, x20,  2; \
      li x3, 0x123ab; \
4: \
    )
  
    // Result = 0
    TEST_CASE(x7, x2, 0xcccc, x1, 16, \
      li x2, 0xcccc; \
      la x19, 5f - 4; \
      jalr x29, x19,  4; \
      li x2, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 7\n")


    # address for test results
	la	x2, test_7_res
    
  
    // Result = 0
    TEST_CASE(x3, x1, 0xcccc, x2, 0, \
      li x1, 0xcccc; \
      la x18, 1f - 3; \
      jalr x30, x18,  3; \
      li x1, 0x123ab; \
1: \
    )
  
    // Result = 0
    TEST_CASE(x3, x0, 0, x2, 4, \
      li x0, 0xcccc; \
      la x17, 2f - 2; \
      jalr x31, x17,  2; \
      li x0, 0x123ab; \
2: \
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

