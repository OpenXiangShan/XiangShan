# See LICENSE for license details.

#*****************************************************************************
# fadd.S
#-----------------------------------------------------------------------------
#
# Test f{add|sub|mul}.d instructions.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"


RVTEST_RV64UF
RV_COMPLIANCE_CODE_BEGIN

#if __riscv_xlen == 32
    # Replace the function with the 32-bit variant defined in test_macros.h
    #undef TEST_FP_OP2_D
    #define TEST_FP_OP2_D TEST_FP_OP2_D32
#endif

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_FP_OP2_D( 2,  fadd.d, 0,                3.5,        2.5,        1.0 );
  TEST_FP_OP2_D( 3,  fadd.d, 1,              -1234,    -1235.1,        1.1 );
  TEST_FP_OP2_D( 4,  fadd.d, 1,         3.14159266, 3.14159265, 0.00000001 );

  TEST_FP_OP2_D( 5,  fsub.d, 0,                1.5,        2.5,        1.0 );
  TEST_FP_OP2_D( 6,  fsub.d, 1,              -1234,    -1235.1,       -1.1 );
  TEST_FP_OP2_D( 7,  fsub.d, 1, 3.1415926400000001, 3.14159265, 0.00000001 );

  TEST_FP_OP2_D( 8,  fmul.d, 0,                2.5,        2.5,        1.0 );
  TEST_FP_OP2_D( 9,  fmul.d, 1,            1358.61,    -1235.1,       -1.1 );
  TEST_FP_OP2_D(10,  fmul.d, 1,      3.14159265e-8, 3.14159265, 0.00000001 );

  # Is the canonical NaN generated for Inf - Inf?
  TEST_FP_OP2_D(11,  fsub.d, 0x10, qNaN, Inf, Inf);

  # show finished.
  li  TESTNUM, 12; 
  SWSIG (12, TESTNUM);

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

