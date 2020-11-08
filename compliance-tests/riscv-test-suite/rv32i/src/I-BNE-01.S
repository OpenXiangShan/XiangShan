# RISC-V Compliance Test BNE-01
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
# Description: Testing instruction 'BNE'.

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
	la	x2, test_1_res
    
 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 0, \
      li x1, 0xcccc; \
	  li x31, -0x1; \
      li x16, 0x0; \
      bne x31, x16, 1f; \
      li x1, 0x123ab; \
1: \
    )
 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 4, \
      li x1, 0xcccc; \
	  li x30, 0x1; \
      li x15, -0x7ff; \
      bne x30, x15, 2f; \
      li x1, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 8, \
      li x1, 0xcccc; \
	  li x29, 0x0; \
      li x14, -0x1; \
      bne x29, x14, 2b; \
      li x1, 0x123ab; \
4: \
    )

 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 12, \
      li x1, 0xcccc; \
	  li x28, 0x7ff; \
      li x13, -0x1234; \
      bne x28, x13, 4f; \
      li x1, 0x123ab; \
4: \
    )
 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 16, \
      li x1, 0xcccc; \
	  li x27, 0x0; \
      li x12, 0x80000000; \
      bne x27, x12, 5f; \
      li x1, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 2\n")


    # address for test results
	la	x2, test_2_res
    
 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 0, \
      li x1, 0xcccc; \
	  li x26, 0x800; \
      li x11, 0x1234; \
      bne x26, x11, 1f; \
      li x1, 0x123ab; \
1: \
    )
 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 4, \
      li x1, 0xcccc; \
	  li x25, 0x7654321; \
      li x10, 0xffffffff; \
      bne x25, x10, 2f; \
      li x1, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 8, \
      li x1, 0xcccc; \
	  li x24, 0x7fffffff; \
      li x9, 0x1; \
      bne x24, x9, 2b; \
      li x1, 0x123ab; \
4: \
    )

 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 12, \
      li x1, 0xcccc; \
	  li x23, 0x1; \
      li x8, 0x7fffffff; \
      bne x23, x8, 4f; \
      li x1, 0x123ab; \
4: \
    )
 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 16, \
      li x1, 0xcccc; \
	  li x22, 0xffffffff; \
      li x7, 0x7654321; \
      bne x22, x7, 5f; \
      li x1, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 3\n")


    # address for test results
	la	x7, test_3_res
    
 
    // Result = 1
    TEST_CASE(x8, x1, 0xcccc, x7, 0, \
      li x1, 0xcccc; \
	  li x21, 0x1234; \
      li x6, 0x800; \
      bne x21, x6, 1f; \
      li x1, 0x123ab; \
1: \
    )
 
    // Result = 1
    TEST_CASE(x8, x1, 0xcccc, x7, 4, \
      li x1, 0xcccc; \
	  li x20, 0x80000000; \
      li x5, 0x0; \
      bne x20, x5, 2f; \
      li x1, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 1
    TEST_CASE(x8, x1, 0xcccc, x7, 8, \
      li x1, 0xcccc; \
	  li x19, -0x1234; \
      li x4, 0x7ff; \
      bne x19, x4, 2b; \
      li x1, 0x123ab; \
4: \
    )

 
    // Result = 0
    TEST_CASE(x8, x1, 0x123ab, x7, 12, \
      li x1, 0xcccc; \
	  li x18, -0x1; \
      li x3, -0x1; \
      bne x18, x3, 4f; \
      li x1, 0x123ab; \
4: \
    )
 
    // Result = 1
    TEST_CASE(x8, x1, 0xcccc, x7, 16, \
      li x1, 0xcccc; \
	  li x17, -0x7ff; \
      li x2, 0x1; \
      bne x17, x2, 5f; \
      li x1, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 4\n")


    # address for test results
	la	x3, test_4_res
    
 
    // Result = 0
    TEST_CASE(x4, x2, 0x123ab, x3, 0, \
      li x2, 0xcccc; \
	  li x16, 0x0; \
      li x1, 0x0; \
      bne x16, x1, 1f; \
      li x2, 0x123ab; \
1: \
    )
 
    // Result = 1
    TEST_CASE(x4, x1, 0xcccc, x3, 4, \
      li x1, 0xcccc; \
	  li x15, -0x1; \
      li x0, 0x0; \
      bne x15, x0, 2f; \
      li x1, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 1
    TEST_CASE(x4, x1, 0xcccc, x3, 8, \
      li x1, 0xcccc; \
	  li x14, 0x1; \
      li x31, -0x7ff; \
      bne x14, x31, 2b; \
      li x1, 0x123ab; \
4: \
    )

 
    // Result = 1
    TEST_CASE(x4, x1, 0xcccc, x3, 12, \
      li x1, 0xcccc; \
	  li x13, 0x0; \
      li x30, -0x1; \
      bne x13, x30, 4f; \
      li x1, 0x123ab; \
4: \
    )
 
    // Result = 1
    TEST_CASE(x4, x1, 0xcccc, x3, 16, \
      li x1, 0xcccc; \
	  li x12, 0x7ff; \
      li x29, -0x1234; \
      bne x12, x29, 5f; \
      li x1, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 5\n")


    # address for test results
	la	x2, test_5_res
    
 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 0, \
      li x1, 0xcccc; \
	  li x11, 0x0; \
      li x28, 0x80000000; \
      bne x11, x28, 1f; \
      li x1, 0x123ab; \
1: \
    )
 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 4, \
      li x1, 0xcccc; \
	  li x10, 0x800; \
      li x27, 0x1234; \
      bne x10, x27, 2f; \
      li x1, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 8, \
      li x1, 0xcccc; \
	  li x9, 0x7654321; \
      li x26, 0xffffffff; \
      bne x9, x26, 2b; \
      li x1, 0x123ab; \
4: \
    )

 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 12, \
      li x1, 0xcccc; \
	  li x8, 0x7fffffff; \
      li x25, 0x1; \
      bne x8, x25, 4f; \
      li x1, 0x123ab; \
4: \
    )
 
    // Result = 1
    TEST_CASE(x3, x1, 0xcccc, x2, 16, \
      li x1, 0xcccc; \
	  li x7, 0x1; \
      li x24, 0x7fffffff; \
      bne x7, x24, 5f; \
      li x1, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 6\n")


    # address for test results
	la	x7, test_6_res
    
 
    // Result = 1
    TEST_CASE(x8, x1, 0xcccc, x7, 0, \
      li x1, 0xcccc; \
	  li x6, 0xffffffff; \
      li x23, 0x7654321; \
      bne x6, x23, 1f; \
      li x1, 0x123ab; \
1: \
    )
 
    // Result = 1
    TEST_CASE(x8, x1, 0xcccc, x7, 4, \
      li x1, 0xcccc; \
	  li x5, 0x1234; \
      li x22, 0x800; \
      bne x5, x22, 2f; \
      li x1, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 1
    TEST_CASE(x8, x1, 0xcccc, x7, 8, \
      li x1, 0xcccc; \
	  li x4, 0x80000000; \
      li x21, 0x0; \
      bne x4, x21, 2b; \
      li x1, 0x123ab; \
4: \
    )

 
    // Result = 1
    TEST_CASE(x8, x1, 0xcccc, x7, 12, \
      li x1, 0xcccc; \
	  li x3, -0x1234; \
      li x20, 0x7ff; \
      bne x3, x20, 4f; \
      li x1, 0x123ab; \
4: \
    )
 
    // Result = 0
    TEST_CASE(x8, x1, 0x123ab, x7, 16, \
      li x1, 0xcccc; \
	  li x2, -0x1; \
      li x19, -0x1; \
      bne x2, x19, 5f; \
      li x1, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 7\n")


    # address for test results
	la	x3, test_7_res
    
 
    // Result = 1
    TEST_CASE(x4, x2, 0xcccc, x3, 0, \
      li x2, 0xcccc; \
	  li x1, -0x7ff; \
      li x18, 0x1; \
      bne x1, x18, 1f; \
      li x2, 0x123ab; \
1: \
    )
 
    // Result = 0
    TEST_CASE(x4, x1, 0x123ab, x3, 4, \
      li x1, 0xcccc; \
	  li x0, 0x0; \
      li x17, 0x0; \
      bne x0, x17, 2f; \
      li x1, 0x123ab; \
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

