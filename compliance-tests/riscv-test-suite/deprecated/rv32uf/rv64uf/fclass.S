# See LICENSE for license details.

#*****************************************************************************
# fclass.S
#-----------------------------------------------------------------------------
#
# Test fclass.s instructions.
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

  TEST_FCLASS_S( 2, 1 << 0, 0xff800000 )
  TEST_FCLASS_S( 3, 1 << 1, 0xbf800000 )
  TEST_FCLASS_S( 4, 1 << 2, 0x807fffff )
  TEST_FCLASS_S( 5, 1 << 3, 0x80000000 )
  TEST_FCLASS_S( 6, 1 << 4, 0x00000000 )
  TEST_FCLASS_S( 7, 1 << 5, 0x007fffff )
  TEST_FCLASS_S( 8, 1 << 6, 0x3f800000 )
  TEST_FCLASS_S( 9, 1 << 7, 0x7f800000 )
  TEST_FCLASS_S(10, 1 << 8, 0x7f800001 )
  TEST_FCLASS_S(11, 1 << 9, 0x7fc00000 )

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 40, 4, -1
RV_COMPLIANCE_DATA_END

