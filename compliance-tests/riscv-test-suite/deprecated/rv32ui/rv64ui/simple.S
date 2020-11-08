# See LICENSE for license details.

#*****************************************************************************
# simple.S
#-----------------------------------------------------------------------------
#
# This is the most basic self checking test. If your simulator does not
# pass thiss then there is little chance that it will pass any of the
# more complicated self checking tests.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"

RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

RV_COMPLIANCE_HALT

RV_COMPLIANCE_CODE_END


  .data

RV_COMPLIANCE_DATA_BEGIN

test_res:
    .fill 8, 4, -1

RV_COMPLIANCE_DATA_END

