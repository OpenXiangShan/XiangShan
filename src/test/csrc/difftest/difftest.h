/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

#ifndef __DIFFTEST_H__
#define __DIFFTEST_H__

#include "common.h"

#include "nemuproxy.h"
#define DIFF_PROXY NemuProxy

#define DIFFTEST_CORE_NUMBER  EMU_CORES
#define DIFFTEST_COMMIT_WIDTH 6
#define DIFFTEST_STORE_WIDTH  2
#define DIFFTEST_LOAD_WIDTH   6

#define DIFFTEST_STORE_COMMIT



// Difftest structures
// trap events: self-defined traps
typedef struct {
  uint8_t  valid = 0;
  uint8_t  code;
  uint64_t pc;
  uint64_t cycleCnt = 0;
  uint64_t instrCnt = 0;
} trap_event_t;

// architectural events: interrupts and exceptions
// whose priority should be higher than normal commits
typedef struct {
  uint32_t interrupt = 0;
  uint32_t exception = 0;
  uint64_t exceptionPC = 0;
} arch_event_t;

typedef struct {
  uint8_t  valid = 0;
  uint64_t pc;
  uint32_t inst;
  uint8_t  skip;
  uint8_t  isRVC;
  uint8_t  scFailed;
  uint8_t  wen;
  uint8_t  wdest;
  uint64_t wdata;
} instr_commit_t;

typedef struct {
  uint64_t gpr[32];
  uint64_t fpr[32];
} arch_reg_state_t;

typedef struct __attribute__((packed)) {
  uint64_t this_pc;
  uint64_t mstatus;
  uint64_t mcause;
  uint64_t mepc;
  uint64_t sstatus;
  uint64_t scause;
  uint64_t sepc;
  uint64_t satp;
  uint64_t mip;
  uint64_t mie;
  uint64_t mscratch;
  uint64_t sscratch;
  uint64_t mideleg;
  uint64_t medeleg;
  uint64_t mtval;
  uint64_t stval;
  uint64_t mtvec;
  uint64_t stvec;
  uint64_t priviledgeMode;
} arch_csr_state_t;

const int DIFFTEST_NR_REG = (sizeof(arch_reg_state_t) + sizeof(arch_csr_state_t)) / sizeof(uint64_t);

typedef struct {
  uint8_t  resp = 0;
  uint64_t addr;
  uint8_t  data[64];
  uint64_t mask;
} sbuffer_state_t;

typedef struct {
  uint8_t  valid = 0;
  uint64_t addr;
  uint64_t data;
  uint8_t  mask;
} store_event_t;

typedef struct {
  uint8_t  valid = 0;
  uint64_t paddr;
  uint8_t  fuType;
  uint8_t  opType;
} load_event_t;

typedef struct {
  uint8_t  resp = 0;
  uint64_t addr;
  uint64_t data;
  uint8_t  mask;
  uint8_t  fuop;
  uint64_t out;
} atomic_event_t;

typedef struct {
  uint8_t  resp = 0;
  uint64_t addr;
  uint64_t data[4];
} ptw_event_t;

typedef struct {
  trap_event_t     trap;
  arch_event_t     event;
  instr_commit_t   commit[DIFFTEST_COMMIT_WIDTH];
  arch_reg_state_t regs;
  arch_csr_state_t csr;
  sbuffer_state_t  sbuffer;
  store_event_t    store[DIFFTEST_STORE_WIDTH];
  load_event_t     load[DIFFTEST_LOAD_WIDTH];
  atomic_event_t   atomic;
  ptw_event_t      ptw;
} difftest_core_state_t;

enum retire_inst_type {
  RET_NORMAL=0,
  RET_INT,
  RET_EXC
};

class DiffState {
public:
  DiffState(int history_length = 32);
  void record_group(uint64_t pc, uint64_t count) {
    retire_group_pc_queue [retire_group_pointer] = pc;
    retire_group_cnt_queue[retire_group_pointer] = count;
    retire_group_pointer = (retire_group_pointer + 1) % DEBUG_GROUP_TRACE_SIZE;
  };
  void record_inst(uint64_t pc, uint32_t inst, uint8_t en, uint8_t dest, uint64_t data) {
    retire_inst_pc_queue   [retire_inst_pointer] = pc;
    retire_inst_inst_queue [retire_inst_pointer] = inst;
    retire_inst_wen_queue  [retire_inst_pointer] = en;
    retire_inst_wdst_queue [retire_inst_pointer] = dest;
    retire_inst_wdata_queue[retire_inst_pointer] = data;
    retire_inst_type_queue[retire_inst_pointer] = RET_NORMAL;
    retire_inst_pointer = (retire_inst_pointer + 1) % DEBUG_INST_TRACE_SIZE;
  };
  void record_abnormal_inst(uint64_t pc, uint32_t inst, uint32_t abnormal_type, uint64_t cause) {
    retire_inst_pc_queue   [retire_inst_pointer] = pc;
    retire_inst_inst_queue [retire_inst_pointer] = inst;
    retire_inst_wdata_queue[retire_inst_pointer] = cause; // write cause to data queue to save space
    retire_inst_type_queue[retire_inst_pointer] = abnormal_type;
    retire_inst_pointer = (retire_inst_pointer + 1) % DEBUG_INST_TRACE_SIZE;
  };
  void display(int coreid);

private:
  const static size_t DEBUG_GROUP_TRACE_SIZE = 16;
  const static size_t DEBUG_INST_TRACE_SIZE = 16;

  int retire_group_pointer = 0;
  uint64_t retire_group_pc_queue[DEBUG_GROUP_TRACE_SIZE] = {0};
  uint32_t retire_group_cnt_queue[DEBUG_GROUP_TRACE_SIZE] = {0};

  int retire_inst_pointer = 0;
  uint64_t retire_inst_pc_queue[DEBUG_INST_TRACE_SIZE] = {0};
  uint32_t retire_inst_inst_queue[DEBUG_INST_TRACE_SIZE] = {0};
  uint64_t retire_inst_wen_queue[DEBUG_INST_TRACE_SIZE] = {0};
  uint32_t retire_inst_wdst_queue[DEBUG_INST_TRACE_SIZE] = {0};
  uint64_t retire_inst_wdata_queue[DEBUG_INST_TRACE_SIZE] = {0};
  uint32_t retire_inst_type_queue[DEBUG_INST_TRACE_SIZE] = {0};
};

class Difftest {
public:
  // Difftest public APIs for testbench
  // Its backend should be cross-platform (NEMU, Spike, ...)
  // Initialize difftest environments
  Difftest(int coreid);
  DIFF_PROXY *proxy;
  uint32_t num_commit = 0; // # of commits if made progress
  // Trigger a difftest checking procdure
  int step();
  inline bool get_trap_valid() {
    return dut.trap.valid;
  }
  inline int get_trap_code() {
    return dut.trap.code;
  }
  void display();

  // Difftest public APIs for dut: called from DPI-C functions (or testbench)
  // These functions generally do nothing but copy the information to core_state.
  inline trap_event_t *get_trap_event() {
    return &(dut.trap);
  }
  inline arch_event_t *get_arch_event() {
    return &(dut.event);
  }
  inline instr_commit_t *get_instr_commit(uint8_t index) {
    return &(dut.commit[index]);
  }
  inline arch_csr_state_t *get_csr_state() {
    return &(dut.csr);
  }
  inline arch_reg_state_t *get_arch_reg_state() {
    return &(dut.regs);
  }
  inline sbuffer_state_t *get_sbuffer_state() {
    return &(dut.sbuffer);
  }
  inline store_event_t *get_store_event(uint8_t index) {
    return &(dut.store[index]);
  }
  inline load_event_t *get_load_event(uint8_t index) {
    return &(dut.load[index]);
  }
  inline atomic_event_t *get_atomic_event() {
    return &(dut.atomic);
  }
  inline ptw_event_t *get_ptw_event() {
    return &(dut.ptw);
  }

private:
  const uint64_t firstCommit_limit = 5000;
  const uint64_t stuck_limit = 5000;

  int id;
  difftest_core_state_t dut;
  difftest_core_state_t ref;
  uint64_t *ref_regs_ptr = (uint64_t*)&ref.regs;
  uint64_t *dut_regs_ptr = (uint64_t*)&dut.regs;

  bool progress = false;
  bool has_commit = false;
  uint64_t ticks = 0;
  uint64_t last_commit = 0;


  uint64_t nemu_this_pc;
  DiffState *state;

  int check_timeout();
  void do_first_instr_commit();
  void do_interrupt();
  void do_exception();
  void do_instr_commit(int index);
  int do_store_check();
  int do_golden_memory_update();
  // inline uint64_t *ref_regs_ptr() { return (uint64_t*)&ref.regs; }
  // inline uint64_t *dut_regs_ptr() { return (uint64_t*)&dut.regs; }

  void raise_trap(int trapCode);
  void clear_step();
};

extern Difftest **difftest;
int difftest_init();
int difftest_step();
int difftest_state();

#endif
