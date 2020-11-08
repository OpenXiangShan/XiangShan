# See LICENSE for license details.

#*****************************************************************************
# fcvt.S
#-----------------------------------------------------------------------------
#
# Test fcvt.s.{wu|w|lu|l}, fcvt.s.d, and fcvt.d.s instructions.
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

  TEST_INT_FP_OP_S( 2,  fcvt.s.w,                   2.0,  2);
  TEST_INT_FP_OP_S( 3,  fcvt.s.w,                  -2.0, -2);

  TEST_INT_FP_OP_S( 4, fcvt.s.wu,                   2.0,  2);
  TEST_INT_FP_OP_S( 5, fcvt.s.wu,           4.2949673e9, -2);

#if __riscv_xlen >= 64
  TEST_INT_FP_OP_S( 6,  fcvt.s.l,                   2.0,  2);
  TEST_INT_FP_OP_S( 7,  fcvt.s.l,                  -2.0, -2);

  TEST_INT_FP_OP_S( 8, fcvt.s.lu,                   2.0,  2);
  TEST_INT_FP_OP_S( 9, fcvt.s.lu,          1.8446744e19, -2);
#endif

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

