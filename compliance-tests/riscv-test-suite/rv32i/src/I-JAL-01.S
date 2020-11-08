# RISC-V Compliance Test JAL-01
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
# Description: Testing instruction 'JAL'.

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
    TEST_CASE(x6, x0, 0, x5, 0, \
      li x0, 0xcccc; \
      jal x31, 1f; \
      li x0, 0x123ab; \
1: \
    )
 
    // Result = 0
    TEST_CASE(x6, x1, 0xcccc, x5, 4, \
      li x1, 0xcccc; \
      jal x30, 2f; \
      li x1, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x6, x2, 0xcccc, x5, 8, \
      li x2, 0xcccc; \
      jal x29, 2b; \
      li x2, 0x123ab; \
4: \
    )

 
    // Result = 0
    TEST_CASE(x6, x3, 0xcccc, x5, 12, \
      li x3, 0xcccc; \
      jal x28, 4f; \
      li x3, 0x123ab; \
4: \
    )
 
    // Result = 0
    TEST_CASE(x6, x4, 0xcccc, x5, 16, \
      li x4, 0xcccc; \
      jal x27, 5f; \
      li x4, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 2\n")


    # address for test results
	la	x1, test_2_res
    
 
    // Result = 0
    TEST_CASE(x2, x5, 0xcccc, x1, 0, \
      li x5, 0xcccc; \
      jal x26, 1f; \
      li x5, 0x123ab; \
1: \
    )
 
    // Result = 0
    TEST_CASE(x2, x6, 0xcccc, x1, 4, \
      li x6, 0xcccc; \
      jal x25, 2f; \
      li x6, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x2, x7, 0xcccc, x1, 8, \
      li x7, 0xcccc; \
      jal x24, 2b; \
      li x7, 0x123ab; \
4: \
    )

 
    // Result = 0
    TEST_CASE(x2, x8, 0xcccc, x1, 12, \
      li x8, 0xcccc; \
      jal x23, 4f; \
      li x8, 0x123ab; \
4: \
    )
 
    // Result = 0
    TEST_CASE(x2, x9, 0xcccc, x1, 16, \
      li x9, 0xcccc; \
      jal x22, 5f; \
      li x9, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 3\n")


    # address for test results
	la	x1, test_3_res
    
 
    // Result = 0
    TEST_CASE(x7, x10, 0xcccc, x1, 0, \
      li x10, 0xcccc; \
      jal x21, 1f; \
      li x10, 0x123ab; \
1: \
    )
 
    // Result = 0
    TEST_CASE(x7, x11, 0xcccc, x1, 4, \
      li x11, 0xcccc; \
      jal x20, 2f; \
      li x11, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x7, x12, 0xcccc, x1, 8, \
      li x12, 0xcccc; \
      jal x19, 2b; \
      li x12, 0x123ab; \
4: \
    )

 
    // Result = 0
    TEST_CASE(x7, x13, 0xcccc, x1, 12, \
      li x13, 0xcccc; \
      jal x18, 4f; \
      li x13, 0x123ab; \
4: \
    )
 
    // Result = 0
    TEST_CASE(x7, x14, 0xcccc, x1, 16, \
      li x14, 0xcccc; \
      jal x17, 5f; \
      li x14, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 4\n")


    # address for test results
	la	x2, test_4_res
    
 
    // Result = 0
    TEST_CASE(x3, x15, 0xcccc, x2, 0, \
      li x15, 0xcccc; \
      jal x16, 1f; \
      li x15, 0x123ab; \
1: \
    )
 
    // Result = 0
    TEST_CASE(x3, x16, 0xcccc, x2, 4, \
      li x16, 0xcccc; \
      jal x15, 2f; \
      li x16, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x3, x17, 0xcccc, x2, 8, \
      li x17, 0xcccc; \
      jal x14, 2b; \
      li x17, 0x123ab; \
4: \
    )

 
    // Result = 0
    TEST_CASE(x3, x18, 0xcccc, x2, 12, \
      li x18, 0xcccc; \
      jal x13, 4f; \
      li x18, 0x123ab; \
4: \
    )
 
    // Result = 0
    TEST_CASE(x3, x19, 0xcccc, x2, 16, \
      li x19, 0xcccc; \
      jal x12, 5f; \
      li x19, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 5\n")


    # address for test results
	la	x1, test_5_res
    
 
    // Result = 0
    TEST_CASE(x2, x20, 0xcccc, x1, 0, \
      li x20, 0xcccc; \
      jal x11, 1f; \
      li x20, 0x123ab; \
1: \
    )
 
    // Result = 0
    TEST_CASE(x2, x21, 0xcccc, x1, 4, \
      li x21, 0xcccc; \
      jal x10, 2f; \
      li x21, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x2, x22, 0xcccc, x1, 8, \
      li x22, 0xcccc; \
      jal x9, 2b; \
      li x22, 0x123ab; \
4: \
    )

 
    // Result = 0
    TEST_CASE(x2, x23, 0xcccc, x1, 12, \
      li x23, 0xcccc; \
      jal x8, 4f; \
      li x23, 0x123ab; \
4: \
    )
 
    // Result = 0
    TEST_CASE(x2, x24, 0xcccc, x1, 16, \
      li x24, 0xcccc; \
      jal x7, 5f; \
      li x24, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 6\n")


    # address for test results
	la	x1, test_6_res
    
 
    // Result = 0
    TEST_CASE(x7, x25, 0xcccc, x1, 0, \
      li x25, 0xcccc; \
      jal x6, 1f; \
      li x25, 0x123ab; \
1: \
    )
 
    // Result = 0
    TEST_CASE(x7, x26, 0xcccc, x1, 4, \
      li x26, 0xcccc; \
      jal x5, 2f; \
      li x26, 0x123ab; \
2: \
    )
 

    jal x0, 3f
2:
    jal x0, 4f
3:
    // Result = 0
    TEST_CASE(x7, x27, 0xcccc, x1, 8, \
      li x27, 0xcccc; \
      jal x4, 2b; \
      li x27, 0x123ab; \
4: \
    )

 
    // Result = 0
    TEST_CASE(x7, x28, 0xcccc, x1, 12, \
      li x28, 0xcccc; \
      jal x3, 4f; \
      li x28, 0x123ab; \
4: \
    )
 
    // Result = 0
    TEST_CASE(x7, x29, 0xcccc, x1, 16, \
      li x29, 0xcccc; \
      jal x2, 5f; \
      li x29, 0x123ab; \
5: \
    )


	# ---------------------------------------------------------------------------------------------
	RVTEST_IO_WRITE_STR(x31, "# Test number 7\n")


    # address for test results
	la	x2, test_7_res
    
 
    // Result = 0
    TEST_CASE(x3, x30, 0xcccc, x2, 0, \
      li x30, 0xcccc; \
      jal x1, 1f; \
      li x30, 0x123ab; \
1: \
    )
 
    // Result = 0
    TEST_CASE(x3, x31, 0xcccc, x2, 4, \
      li x31, 0xcccc; \
      jal x0, 2f; \
      li x31, 0x123ab; \
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

