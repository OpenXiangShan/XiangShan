#ifndef __COMMON_H__
#define __COMMON_H__

#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <goldenmem.h>

#define DIFFTEST_WIDTH 6

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
  DIFFTEST_MTVAL,
  DIFFTEST_STVAL,
  DIFFTEST_MTVEC,
  DIFFTEST_STVEC,
  DIFFTEST_MODE,
  DIFFTEST_NR_REG
};

struct SyncState {
  uint64_t lrscValid;
  uint64_t lrscAddr;
};

struct DiffState {
  // Regs and mode for single step difftest
  int commit;
  uint64_t *reg_scala;
  uint32_t thisINST;
  int skip;
  int isRVC;
  uint64_t *wpc;
  uint64_t *wdata;
  uint32_t *wdst;
  uint64_t *lpaddr;
  uint32_t *ltype;
  uint8_t  *lfu;
  int wen;
  uint64_t intrNO;
  uint64_t cause; // for disambiguate_exec
  int priviledgeMode;

  // Microarchitucural signal needed to sync status
  uint64_t scFailed;  // sc inst commited, it failed beacuse lr_valid === 0
  // lrscValid needs to be synced as nemu does not know 
  // how many cycles were used to finish a lr/sc pair, 
  // this will lead to different sc results.

  int store_commit;
  uint64_t store_addr[2];
  uint64_t store_data[2];
  uint8_t store_mask[2];
};

struct DisambiguationState {
  uint64_t exceptionNo;
  uint64_t mtval;
  uint64_t stval;
};

void init_difftest();
int difftest_step(DiffState *s, int coreid);
int difftest_store_step(uint64_t *saddr, uint64_t *sdata, uint8_t *smask, int coreid);
void difftest_display(uint8_t mode, int coreid);

uint64_t get_nemu_this_pc(int coreid);
void set_nemu_this_pc(uint64_t pc, int coreid);

#endif
