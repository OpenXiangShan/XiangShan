# See LICENSE for license details.

#*****************************************************************************
# fcmp.S
#-----------------------------------------------------------------------------
#
# Test f{eq|lt|le}.s instructions.
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

  TEST_FP_CMP_OP_S( 2, feq.s, 0x00, 1, -1.36, -1.36)
  TEST_FP_CMP_OP_S( 3, fle.s, 0x00, 1, -1.36, -1.36)
  TEST_FP_CMP_OP_S( 4, flt.s, 0x00, 0, -1.36, -1.36)

  TEST_FP_CMP_OP_S( 5, feq.s, 0x00, 0, -1.37, -1.36)
  TEST_FP_CMP_OP_S( 6, fle.s, 0x00, 1, -1.37, -1.36)
  TEST_FP_CMP_OP_S( 7, flt.s, 0x00, 1, -1.37, -1.36)

  # Only sNaN should signal invalid for feq.
  TEST_FP_CMP_OP_S( 8, feq.s, 0x00, 0, NaN, 0)
  TEST_FP_CMP_OP_S( 9, feq.s, 0x00, 0, NaN, NaN)
  TEST_FP_CMP_OP_S(10, feq.s, 0x10, 0, sNaNf, 0)

  # qNaN should signal invalid for fle/flt.
  TEST_FP_CMP_OP_S(11, flt.s, 0x10, 0, NaN, 0)
  TEST_FP_CMP_OP_S(12, flt.s, 0x10, 0, NaN, NaN)
  TEST_FP_CMP_OP_S(13, flt.s, 0x10, 0, sNaNf, 0)
  TEST_FP_CMP_OP_S(14, fle.s, 0x10, 0, NaN, 0)
  TEST_FP_CMP_OP_S(15, fle.s, 0x10, 0, NaN, NaN)
  TEST_FP_CMP_OP_S(16, fle.s, 0x10, 0, sNaNf, 0)

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

