#ifndef __COMMON_H__
#define __COMMON_H__

#include <stdint.h>
#include <assert.h>
#include <string.h>

typedef uint64_t rtlreg_t;

typedef uint64_t paddr_t;
typedef uint64_t vaddr_t;

typedef uint16_t ioaddr_t;

#include "macro.h"

// 0~31: GPRs, 32~63 FPRs
enum {
  DIFFTEST_THIS_PC = 64,
  DIFFTEST_MSTATUS,
  DIFFTEST_MCAUSE,
  DIFFTEST_MEPC,
  DIFFTEST_SSTATUS,
  DIFFTEST_SCAUSE,
  DIFFTEST_SEPC,
  DIFFTEST_NR_REG
};

#endif
