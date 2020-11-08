# See LICENSE for license details.

#*****************************************************************************
# fmin.S
#-----------------------------------------------------------------------------
#
# Test f{min|max}.s instructinos.
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

  TEST_FP_OP2_S( 2,  fmin.s, 0,        1.0,        2.5,        1.0 );
  TEST_FP_OP2_S( 3,  fmin.s, 0,    -1235.1,    -1235.1,        1.1 );
  TEST_FP_OP2_S( 4,  fmin.s, 0,    -1235.1,        1.1,    -1235.1 );
  TEST_FP_OP2_S( 5,  fmin.s, 0,    -1235.1,        NaN,    -1235.1 );
  TEST_FP_OP2_S( 6,  fmin.s, 0, 0.00000001, 3.14159265, 0.00000001 );
  TEST_FP_OP2_S( 7,  fmin.s, 0,       -2.0,       -1.0,       -2.0 );

  TEST_FP_OP2_S(12,  fmax.s, 0,        2.5,        2.5,        1.0 );
  TEST_FP_OP2_S(13,  fmax.s, 0,        1.1,    -1235.1,        1.1 );
  TEST_FP_OP2_S(14,  fmax.s, 0,        1.1,        1.1,    -1235.1 );
  TEST_FP_OP2_S(15,  fmax.s, 0,    -1235.1,        NaN,    -1235.1 );
  TEST_FP_OP2_S(16,  fmax.s, 0, 3.14159265, 3.14159265, 0.00000001 );
  TEST_FP_OP2_S(17,  fmax.s, 0,       -1.0,       -1.0,       -2.0 );

  # FMIN(sNaN, x) = x
  TEST_FP_OP2_S(20,  fmax.s, 0x10, 1.0, sNaNf, 1.0);
  # FMIN(qNaN, qNaN) = canonical NaN
  TEST_FP_OP2_S(21,  fmax.s, 0x00, qNaNf, NaN, NaN);

  # -0.0 < +0.0
  TEST_FP_OP2_S(30,  fmin.s, 0,       -0.0,       -0.0,        0.0 );
  TEST_FP_OP2_S(31,  fmin.s, 0,       -0.0,        0.0,       -0.0 );
  TEST_FP_OP2_S(32,  fmax.s, 0,        0.0,       -0.0,        0.0 );
  TEST_FP_OP2_S(33,  fmax.s, 0,        0.0,        0.0,       -0.0 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

