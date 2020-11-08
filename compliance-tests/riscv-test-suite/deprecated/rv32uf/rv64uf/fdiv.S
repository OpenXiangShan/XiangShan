# See LICENSE for license details.

#*****************************************************************************
# fdiv.S
#-----------------------------------------------------------------------------
#
# Test f{div|sqrt}.s instructions.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"


RVTEST_RV64UF
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  TEST_FP_OP2_S(2,  fdiv.s, 1, 1.1557273520668288, 3.14159265, 2.71828182 );
  TEST_FP_OP2_S(3,  fdiv.s, 1,-0.9991093838555584,      -1234,     1235.1 );
  TEST_FP_OP2_S(4,  fdiv.s, 0,         3.14159265, 3.14159265,        1.0 );

  TEST_FP_OP1_S(5,  fsqrt.s, 1, 1.7724538498928541, 3.14159265 );
  TEST_FP_OP1_S(6,  fsqrt.s, 0,                100,      10000 );

  TEST_FP_OP1_S_DWORD_RESULT(7,  fsqrt.s, 0x10, 0x7FC00000, -1.0 );

  TEST_FP_OP1_S(8,  fsqrt.s, 1, 13.076696, 171.0);

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

