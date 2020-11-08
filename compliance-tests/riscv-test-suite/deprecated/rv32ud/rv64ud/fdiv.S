# See LICENSE for license details.

#*****************************************************************************
# fdiv.S
#-----------------------------------------------------------------------------
#
# Test f{div|sqrt}.d instructions.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"


RVTEST_RV64UF
RV_COMPLIANCE_CODE_BEGIN

#if __riscv_xlen == 32
    # Replace the functions with the 32-bit variants defined in test_macros.h
    #undef TEST_FP_OP2_D
    #define TEST_FP_OP2_D TEST_FP_OP2_D32

    #undef TEST_FP_OP1_D
    #define TEST_FP_OP1_D TEST_FP_OP1_D32

    #undef TEST_FP_OP1_D_DWORD_RESULT
    #define TEST_FP_OP1_D_DWORD_RESULT TEST_FP_OP1_D32_DWORD_RESULT
#endif

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_FP_OP2_D( 2,  fdiv.d, 1, 1.1557273520668288, 3.14159265, 2.71828182 );
  TEST_FP_OP2_D( 3,  fdiv.d, 1,-0.9991093838555584,      -1234,     1235.1 );
  TEST_FP_OP2_D( 4,  fdiv.d, 0,         3.14159265, 3.14159265,        1.0 );

  TEST_FP_OP1_D( 5,  fsqrt.d, 1, 1.7724538498928541, 3.14159265 );
  TEST_FP_OP1_D( 6,  fsqrt.d, 0,                100,      10000 );

  TEST_FP_OP1_D_DWORD_RESULT(16,  fsqrt.d, 0x10,      0x7FF8000000000000,      -1.0 );

  TEST_FP_OP1_D( 7,  fsqrt.d, 1, 13.076696830622021, 171.0);

  TEST_FP_OP1_D( 8,  fsqrt.d, 1,0.00040099251863345283320230749702, 1.60795e-7);

  # show finished.
  li  TESTNUM, 17; 
  SWSIG (17, TESTNUM);

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

