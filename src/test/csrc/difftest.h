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
  DIFFTEST_SATP,
  DIFFTEST_MIP,
  DIFFTEST_MIE,
  DIFFTEST_MSCRATCH,
  DIFFTEST_SSCRATCH,
  DIFFTEST_MIDELEG,
  DIFFTEST_MEDELEG,
  DIFFTEST_MODE,
  DIFFTEST_NR_REG
};
// DIFFTEST_MTVAL, DIFFTEST_STVAL will be updated while committing exception
// Compare / snapshot them is not necessary

struct SyncChannel {
  uint64_t scFailed; // sc inst commited, it failed beacuse lr_valid === 0
  // uint64_t lrscAddr;
};

struct DiffState {
  // Regs and mode for single step difftest
  int commit;
  uint64_t *reg_scala;
  uint32_t this_inst;
  int skip;
  int isRVC;
  uint64_t *wpc;
  uint64_t *wdata;
  uint32_t *wdst;
  int wen;
  uint64_t intrNO;
  int priviledgeMode;

  // Microarchitucural signal needed to sync status
  struct SyncChannel sync;
  // lrscValid needs to be synced as nemu does not know 
  // how many cycles were used to finish a lr/sc pair, 
  // this will lead to different sc results.
};

extern void (*ref_difftest_memcpy_from_dut)(paddr_t dest, void *src, size_t n);
extern void (*ref_difftest_memcpy_from_ref)(void *dest, paddr_t src, size_t n);
extern void (*ref_difftest_getregs)(void *c);
extern void (*ref_difftest_setregs)(const void *c);

void init_difftest();
int difftest_step(DiffState *s);
void difftest_display(uint8_t mode);

#endif
