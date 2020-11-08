# See LICENSE for license details.

#include "riscv_test.h"
#undef RVTEST_RV64M
#define RVTEST_RV64M RVTEST_RV32M

#undef SATP_MODE_SV39
#define SATP_MODE_SV39 SATP_MODE_SV32

#include "../rv64si/dirty.S"
