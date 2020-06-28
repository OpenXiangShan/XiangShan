#include "common.h"
#include "difftest.h"
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

#ifndef REF_SO
# error Please define REF_SO to the path of NEMU shared object file
#endif

#define printCSR(x) printf(""#x": 0x%016lx    ", x)

void (*ref_difftest_memcpy_from_dut)(paddr_t dest, void *src, size_t n) = NULL;
void (*ref_difftest_getregs)(void *c) = NULL;
void (*ref_difftest_setregs)(const void *c) = NULL;
void (*ref_difftest_exec)(uint64_t n) = NULL;
void (*ref_difftest_raise_intr)(uint64_t NO) = NULL;
void (*ref_isa_reg_display)(void) = NULL;

static bool is_skip_ref;
static bool is_skip_dut;

// this is used to let ref skip instructions which
// can not produce consistent behavior with NEMU
void difftest_skip_ref() {
  is_skip_ref = true;
}

// this is used to deal with instruction packing in QEMU.
// Sometimes letting QEMU step once will execute multiple instructions.
// We should skip checking until NEMU's pc catches up with QEMU's pc.
void difftest_skip_dut() {
  if (is_skip_dut) return;

  ref_difftest_exec(1);
  is_skip_dut = true;
}

void init_difftest(uint64_t *reg) {
  void *handle;
  handle = dlopen(REF_SO, RTLD_LAZY | RTLD_DEEPBIND);
  assert(handle);

  ref_difftest_memcpy_from_dut = (void (*)(paddr_t, void *, size_t))dlsym(handle, "difftest_memcpy_from_dut");
  assert(ref_difftest_memcpy_from_dut);

  ref_difftest_getregs = (void (*)(void *))dlsym(handle, "difftest_getregs");
  assert(ref_difftest_getregs);

  ref_difftest_setregs = (void (*)(const void *))dlsym(handle, "difftest_setregs");
  assert(ref_difftest_setregs);

  ref_difftest_exec = (void (*)(uint64_t))dlsym(handle, "difftest_exec");
  assert(ref_difftest_exec);

  ref_difftest_raise_intr = (void (*)(uint64_t))dlsym(handle, "difftest_raise_intr");
  assert(ref_difftest_raise_intr);

  ref_isa_reg_display = (void (*)(void))dlsym(handle, "isa_reg_display");
  assert(ref_isa_reg_display);

  void (*ref_difftest_init)(void) = (void (*)(void))dlsym(handle, "difftest_init");
  assert(ref_difftest_init);

  ref_difftest_init();
  void* get_img_start();
  long get_img_size();
  ref_difftest_memcpy_from_dut(0x0, get_img_start(), get_img_size());
  ref_difftest_setregs(reg);
}

static const char *reg_name[DIFFTEST_NR_REG] = {
  "$0", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
  "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
  "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
  "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6",
  "ft0", "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7",
  "fs0", "fs1", "fa0", "fa1", "fa2", "fa3", "fa4", "fa5",
  "fa6", "fa7", "fs2", "fs3", "fs4", "fs5", "fs6", "fs7",
  "fs8", "fs9", "fs10", "fs11", "ft8", "ft9", "ft10", "ft11",
  "this_pc",
  "mstatus", "mcause", "mepc",
  "sstatus", "scause", "sepc"
};

int difftest_step(int commit, uint64_t *reg_scala, uint32_t this_inst,
  int skip, int isRVC, uint64_t intrNO, int priviledgeMode) {

  assert(!skip && !isRVC && intrNO == 0);
  #define DEBUG_RETIRE_TRACE_SIZE 16

  uint64_t ref_r[DIFFTEST_NR_REG];
  uint64_t this_pc = reg_scala[DIFFTEST_THIS_PC];
  // ref_difftest_getregs() will get the next pc,
  // therefore we must keep track this one
  static uint64_t nemu_this_pc = 0x80000000;
  static uint64_t pc_retire_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
  static uint32_t inst_retire_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
  static uint32_t retire_cnt_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
  static int pc_retire_pointer = 7;

  if (skip) {
    // printf("diff pc: %x isRVC %x\n", this_pc, isRVC);
    // MMIO accessing should not be a branch or jump, just +2/+4 to get the next pc
    reg_scala[DIFFTEST_THIS_PC] += isRVC ? 2 : 4;
    nemu_this_pc += isRVC ? 2 : 4;
    // to skip the checking of an instruction, just copy the reg state to reference design
    ref_difftest_setregs(reg_scala);
    pc_retire_pointer = (pc_retire_pointer+1) % DEBUG_RETIRE_TRACE_SIZE;
    pc_retire_queue[pc_retire_pointer] = this_pc;
    inst_retire_queue[pc_retire_pointer] = this_inst;
    retire_cnt_queue[pc_retire_pointer] = commit;
    return 0;
  }

  if (intrNO) {
    ref_difftest_raise_intr(intrNO);
    ref_difftest_exec(1);//TODO
  }

  assert(commit > 0 && commit <= 6);
  ref_difftest_exec(commit);
  ref_difftest_getregs(&ref_r);

  uint64_t next_pc = ref_r[DIFFTEST_THIS_PC];
  pc_retire_pointer = (pc_retire_pointer+1) % DEBUG_RETIRE_TRACE_SIZE;
  pc_retire_queue[pc_retire_pointer] = this_pc;
  inst_retire_queue[pc_retire_pointer] = this_inst;
  retire_cnt_queue[pc_retire_pointer] = commit;
  
  // TODO: fix mip.mtip
  // int isCSR = ((this_inst & 0x7f) ==  0x73);
  // int isCSRMip = ((this_inst >> 20) == 0x344) && isCSR;
  // if (isCSRMip) {
  //   // We can not handle NEMU.mip.mtip since it is driven by CLINT,
  //   // which is not accessed in NEMU due to MMIO.
  //   // Just sync the state of NEMU from NOOP.
  //   reg_scala[DIFFTEST_THIS_PC] = next_pc;
  //   nemu_this_pc = next_pc;
  //   ref_difftest_setregs(reg_scala);
  //   return 0;
  // }

  // replace with "this pc" for checking
  ref_r[DIFFTEST_THIS_PC] = nemu_this_pc;
  nemu_this_pc = next_pc;

  if (memcmp(reg_scala, ref_r, sizeof(ref_r)) != 0) {
    printf("\n==============Retire Trace==============\n");
    int j;
    for(j = 0; j < DEBUG_RETIRE_TRACE_SIZE; j++){
      printf("retire trace [%x]: pc %010lx inst %08x cmtcnt %d %s\n", j, pc_retire_queue[j], inst_retire_queue[j], retire_cnt_queue[j], (j==pc_retire_pointer)?"<--":"");
    }
    printf("\n==============  Reg Diff  ==============\n");
    ref_isa_reg_display();
    printCSR(priviledgeMode);
    puts("");
    int i;
    for (i = 0; i < DIFFTEST_NR_REG; i ++) {
      if (reg_scala[i] != ref_r[i]) {
        printf("%s different at pc = 0x%010lx, right= 0x%016lx, wrong = 0x%016lx\n",
            reg_name[i], this_pc, ref_r[i], reg_scala[i]);
      }
    }
    return 1;
  }
  return 0;
}
