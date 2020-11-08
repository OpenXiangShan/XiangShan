# See LICENSE for license details.

#*****************************************************************************
# lui.S
#-----------------------------------------------------------------------------
#
# Test lui instruction.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"


RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  #-------------------------------------------------------------
  # Basic tests
  #-------------------------------------------------------------

  TEST_CASE( 2, x1, 0x0000000000000000, lui x1, 0x00000 );
  TEST_CASE( 3, x1, 0xfffffffffffff800, lui x1, 0xfffff;sra x1,x1,1);
  TEST_CASE( 4, x1, 0x00000000000007ff, lui x1, 0x7ffff;sra x1,x1,20);
  TEST_CASE( 5, x1, 0xfffffffffffff800, lui x1, 0x80000;sra x1,x1,20);

  TEST_CASE( 6, x0, 0, lui x0, 0x80000 );

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 10, 4, -1
RV_COMPLIANCE_DATA_END


