#include "common.h"
#include "difftest.h"
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

#ifndef REF_SO
# error Please define REF_SO to the path of NEMU shared object file
#endif

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

void init_difftest(uint64_t *reg, const char *mainargs) {
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
  ref_difftest_memcpy_from_dut(0x100000, get_img_start(), get_img_size());
  ref_difftest_memcpy_from_dut(0x0, (void *)mainargs, strlen(mainargs) + 1);
  ref_difftest_setregs(reg);
}

int difftest_step(uint64_t *reg_scala, uint64_t this_pc, int isMMIO, uint64_t intrNO) {
  uint64_t ref_r[33];
  static uint64_t nemu_pc = 0x80100000;
  if (isMMIO) {
    // printf("diff pc: %x\n", this_pc);
    // MMIO accessing should not be a branch or jump, just +4 to get the next pc
    reg_scala[32] += 4;
    nemu_pc += 4;
    // to skip the checking of an instruction, just copy the reg state to reference design
    ref_difftest_setregs(reg_scala);
    return 0;
  }

  if (intrNO) {
    ref_difftest_raise_intr(intrNO);
  } else {
    ref_difftest_exec(1);
  }

  ref_difftest_getregs(&ref_r);

  uint64_t temp = ref_r[32];
  ref_r[32] = nemu_pc;
  nemu_pc = temp;

  if (memcmp(reg_scala, ref_r, sizeof(ref_r)) != 0) {
    ref_isa_reg_display();
    int i;
    for (i = 0; i < 33; i ++) {
      if (reg_scala[i] != ref_r[i]) {
        printf("x%2d different at pc = 0x%08lx, right= 0x%016lx, wrong = 0x%016lx\n",
            i, this_pc, ref_r[i], reg_scala[i]);
      }
    }
    return 1;
  }
  return 0;
}
