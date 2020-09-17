#include "common.h"
#include "difftest.h"
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

#ifndef REF_SO
# error Please define REF_SO to the path of NEMU shared object file
#endif

#define selectBit(src, x) (src & (1 << x))
#define DEBUG_RETIRE_TRACE_SIZE 16
#define DEBUG_WB_TRACE_SIZE 16

void (*ref_difftest_memcpy_from_dut)(paddr_t dest, void *src, size_t n) = NULL;
void (*ref_difftest_memcpy_from_ref)(void *dest, paddr_t src, size_t n) = NULL;
void (*ref_difftest_getregs)(void *c) = NULL;
void (*ref_difftest_setregs)(const void *c) = NULL;
static void (*ref_difftest_sync)(uint64_t *skip) = NULL;
static void (*ref_difftest_exec)(uint64_t n) = NULL;
static void (*ref_difftest_raise_intr)(uint64_t NO) = NULL;
static void (*ref_isa_reg_display)(void) = NULL;

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

void init_difftest() {
  void *handle;
  handle = dlopen(REF_SO, RTLD_LAZY | RTLD_DEEPBIND);
  puts("Using " REF_SO " for difftest");
  assert(handle);

  ref_difftest_memcpy_from_dut = (void (*)(paddr_t, void *, size_t))dlsym(handle, "difftest_memcpy_from_dut");
  assert(ref_difftest_memcpy_from_dut);

  ref_difftest_memcpy_from_ref = (void (*)(void *, paddr_t, size_t))dlsym(handle, "difftest_memcpy_from_ref");
  assert(ref_difftest_memcpy_from_ref);

  ref_difftest_getregs = (void (*)(void *))dlsym(handle, "difftest_getregs");
  assert(ref_difftest_getregs);

  ref_difftest_setregs = (void (*)(const void *))dlsym(handle, "difftest_setregs");
  assert(ref_difftest_setregs);

  ref_difftest_sync = (void (*)(uint64_t *))dlsym(handle, "difftest_sync");
  assert(ref_difftest_sync);

  ref_difftest_exec = (void (*)(uint64_t))dlsym(handle, "difftest_exec");
  assert(ref_difftest_exec);

  ref_difftest_raise_intr = (void (*)(uint64_t))dlsym(handle, "difftest_raise_intr");
  assert(ref_difftest_raise_intr);

  ref_isa_reg_display = (void (*)(void))dlsym(handle, "isa_reg_display");
  assert(ref_isa_reg_display);

  void (*ref_difftest_init)(void) = (void (*)(void))dlsym(handle, "difftest_init");
  assert(ref_difftest_init);

  ref_difftest_init();
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
  "sstatus", "scause", "sepc",
  "satp", 
  "mip", "mie", "mscratch", "sscratch", "mideleg", "medeleg"   
};

static uint64_t nemu_this_pc = 0x80000000;
static uint64_t pc_retire_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
static uint32_t inst_retire_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
static uint32_t retire_cnt_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
static int pc_retire_pointer = DEBUG_RETIRE_TRACE_SIZE-1;
static uint64_t pc_wb_queue[DEBUG_WB_TRACE_SIZE] = {0};
static uint64_t wen_wb_queue[DEBUG_WB_TRACE_SIZE] = {0};
static uint32_t wdst_wb_queue[DEBUG_WB_TRACE_SIZE] = {0};
static uint64_t wdata_wb_queue[DEBUG_WB_TRACE_SIZE] = {0};
static int wb_pointer = 0;

uint64_t get_nemu_this_pc() { return nemu_this_pc; }
void set_nemu_this_pc(uint64_t pc) { nemu_this_pc = pc; }

void difftest_display(uint8_t mode) {
  printf("\n==============Retire Trace==============\n");
  int j;
  for(j = 0; j < DEBUG_RETIRE_TRACE_SIZE; j++){
    printf("retire trace [%x]: pc %010lx inst %08x cmtcnt %d %s\n",
        j, pc_retire_queue[j], inst_retire_queue[j], retire_cnt_queue[j], (j==pc_retire_pointer)?"<--":"");
  }
  printf("\n==============  WB Trace  ==============\n");
  for(j = 0; j < DEBUG_WB_TRACE_SIZE; j++){
    printf("wb trace [%x]: pc %010lx wen %x dst %08x data %016lx %s\n",
        j, pc_wb_queue[j], wen_wb_queue[j]!=0, wdst_wb_queue[j], wdata_wb_queue[j], (j==((wb_pointer-1)%DEBUG_WB_TRACE_SIZE))?"<--":"");
  }
  printf("\n==============  Reg Diff  ==============\n");
  ref_isa_reg_display();
  printf("priviledgeMode: %d\n", mode);
}

int difftest_step(DiffState *s) {
  // assert(!s->isRVC);

  uint64_t ref_r[DIFFTEST_NR_REG];
  uint64_t this_pc = s->reg_scala[DIFFTEST_THIS_PC];
  // ref_difftest_getregs() will get the next pc,
  // therefore we must keep track this one

  // if (skip) {
  //   // printf("diff pc: %x isRVC %x\n", this_pc, isRVC);
  //   // MMIO accessing should not be a branch or jump, just +2/+4 to get the next pc
  //   reg_scala[DIFFTEST_THIS_PC] += isRVC ? 2 : 4;
  //   nemu_this_pc += isRVC ? 2 : 4;
  //   // to skip the checking of an instruction, just copy the reg state to reference design
  //   ref_difftest_setregs(reg_scala);
  //   pc_retire_pointer = (pc_retire_pointer+1) % DEBUG_RETIRE_TRACE_SIZE;
  //   pc_retire_queue[pc_retire_pointer] = this_pc;
  //   inst_retire_queue[pc_retire_pointer] = this_inst;
  //   retire_cnt_queue[pc_retire_pointer] = commit;
  //   return 0;
  // }

  // sync lr/sc reg status
  ref_difftest_sync((uint64_t*)&s->sync); // sync lr/sc microarchitectural regs

  // single step difftest
  if (s->intrNO) {
    ref_difftest_raise_intr(s->intrNO);
    // ref_difftest_exec(1);//TODO
  }
  else {
    assert(s->commit > 0 && s->commit <= 6);
    for(int i = 0; i < s->commit; i++){
      pc_wb_queue[wb_pointer] = s->wpc[i];
      wen_wb_queue[wb_pointer] = selectBit(s->wen, i);
      wdst_wb_queue[wb_pointer] = s->wdst[i];
      wdata_wb_queue[wb_pointer] = s->wdata[i];
      wb_pointer = (wb_pointer+1) % DEBUG_WB_TRACE_SIZE;
      if(selectBit(s->skip, i)){
        // MMIO accessing should not be a branch or jump, just +2/+4 to get the next pc
        // printf("SKIP %d\n", i);
        // to skip the checking of an instruction, just copy the reg state to reference design
        ref_difftest_getregs(&ref_r);
        ref_r[DIFFTEST_THIS_PC] += selectBit(s->isRVC, i) ? 2 : 4;
        if(selectBit(s->wen, i)){
          if(s->wdst[i] != 0){
            ref_r[s->wdst[i]] = s->wdata[i];
          }
        }
        ref_difftest_setregs(ref_r);
      }else{
        ref_difftest_exec(1);
      }
    }
  }
  ref_difftest_getregs(&ref_r);

  uint64_t next_pc = ref_r[DIFFTEST_THIS_PC];
  pc_retire_pointer = (pc_retire_pointer+1) % DEBUG_RETIRE_TRACE_SIZE;
  pc_retire_queue[pc_retire_pointer] = this_pc;
  inst_retire_queue[pc_retire_pointer] = s->this_inst;
  retire_cnt_queue[pc_retire_pointer] = s->commit;
  
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

  if (memcmp(s->reg_scala, ref_r, sizeof(ref_r)) != 0) {
    difftest_display(s->priviledgeMode);
    int i;
    for (i = 0; i < DIFFTEST_NR_REG; i ++) {
      if (s->reg_scala[i] != ref_r[i]) {
        printf("%s different at pc = 0x%010lx, right= 0x%016lx, wrong = 0x%016lx\n",
            reg_name[i], this_pc, ref_r[i], s->reg_scala[i]);
      }
    }
    return 1;
  }
  return 0;
}
