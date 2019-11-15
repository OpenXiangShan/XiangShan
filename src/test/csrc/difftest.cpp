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

int difftest_step(
  uint64_t *reg_scala, 
  uint64_t this_pc, 
  int this_inst, 
  int isMMIO, 
  int isRVC, 
  uint64_t intrNO,
  int priviledgeMode,
  uint64_t mstatus,
  uint64_t sstatus,
  uint64_t mepc,
  uint64_t sepc,
  uint64_t mcause,
  uint64_t scause
) {

  #define DEBUG_RETIRE_TRACE_SIZE 16

  uint64_t ref_r[33];
  static uint64_t nemu_pc = 0x80000000;
  static uint64_t pc_retire_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
  static int inst_retire_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
  static int pc_retire_pointer = 7;

  if (isMMIO) {
    // printf("diff pc: %x isRVC %x\n", this_pc, isRVC);
    // MMIO accessing should not be a branch or jump, just +2/+4 to get the next pc
    reg_scala[32] += isRVC ? 2 : 4;
    nemu_pc += isRVC ? 2 : 4;
    // to skip the checking of an instruction, just copy the reg state to reference design
    ref_difftest_setregs(reg_scala);
    pc_retire_pointer = (pc_retire_pointer+1) % DEBUG_RETIRE_TRACE_SIZE;
    pc_retire_queue[pc_retire_pointer] = this_pc;
    inst_retire_queue[pc_retire_pointer] = this_inst;
    return 0;
  }

  if (intrNO) {
    ref_difftest_raise_intr(intrNO);
  } else {
    ref_difftest_exec(1);
  }

  ref_difftest_getregs(&ref_r);

  pc_retire_pointer = (pc_retire_pointer+1) % DEBUG_RETIRE_TRACE_SIZE;
  pc_retire_queue[pc_retire_pointer] = this_pc;
  inst_retire_queue[pc_retire_pointer] = this_inst;
  
  uint64_t temp = ref_r[32];
  ref_r[32] = nemu_pc;
  nemu_pc = temp;

  if (memcmp(reg_scala, ref_r, sizeof(ref_r)) != 0) {
    printf("\n==============Retire Trace==============\n");
    int j;
    for(j = 0; j < DEBUG_RETIRE_TRACE_SIZE; j++){
      printf("retire trace [%x]: pc %010lx inst %08x %s\n", j, pc_retire_queue[j], inst_retire_queue[j], (j==pc_retire_pointer)?"<--":"");
    }
    printf("\n==============  Reg Diff  ==============\n");
    ref_isa_reg_display();
    printf("\n==============  Csr Diff  ==============\n");
    printCSR(priviledgeMode);
    puts("");
    printCSR(mstatus);
    printCSR(mcause);
    printCSR(mepc);
    puts("");
    printCSR(sstatus);
    printCSR(scause);
    printCSR(sepc);
    puts("");
    int i;
    for (i = 0; i < 33; i ++) {
      if (reg_scala[i] != ref_r[i]) {
        printf("x%2d different at pc = 0x%010lx, right= 0x%016lx, wrong = 0x%016lx\n",
            i, this_pc, ref_r[i], reg_scala[i]);
      }
    }
    return 1;
  }
  return 0;
}
