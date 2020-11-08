# See LICENSE for license details.

#include "riscv_test.h"
#undef RVTEST_RV64M
#define RVTEST_RV64M RVTEST_RV32M

#include "../rv64mi/mcsr.S"
