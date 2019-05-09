#include "common.h"
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

#define REF_SO "/home/yzh/projectn/nemu/build/riscv32-nemu-so"

void (*ref_difftest_memcpy_from_dut)(paddr_t dest, void *src, size_t n) = NULL;
void (*ref_difftest_getregs)(void *c) = NULL;
void (*ref_difftest_setregs)(const void *c) = NULL;
void (*ref_difftest_exec)(uint64_t n) = NULL;

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

static void load_img(char *img_file) {
  long size;
  int ret;

  FILE *fp = fopen(img_file, "rb");
  if (fp == NULL) {
    printf("Can not open '%s'\n", img_file);
    assert(0);
  }

  printf("The image is %s\n", img_file);

  fseek(fp, 0, SEEK_END);
  size = ftell(fp);

  fseek(fp, 0, SEEK_SET);
  void *buf = malloc(size);
  ret = fread(buf, size, 1, fp);
  assert(ret == 1);
  fclose(fp);

  ref_difftest_memcpy_from_dut(0x100000, buf, size);
  free(buf);
}

void init_difftest(char *img, uint32_t *reg) {
  assert(img != NULL);

  void *handle;
  handle = dlopen(REF_SO, RTLD_LAZY | RTLD_DEEPBIND);
  assert(handle);

  ref_difftest_memcpy_from_dut = dlsym(handle, "difftest_memcpy_from_dut");
  assert(ref_difftest_memcpy_from_dut);

  ref_difftest_getregs = dlsym(handle, "difftest_getregs");
  assert(ref_difftest_getregs);

  ref_difftest_setregs = dlsym(handle, "difftest_setregs");
  assert(ref_difftest_setregs);

  ref_difftest_exec = dlsym(handle, "difftest_exec");
  assert(ref_difftest_exec);

  void (*ref_difftest_init)(void) = dlsym(handle, "difftest_init");
  assert(ref_difftest_init);

  ref_difftest_init();
  load_img(img);
  ref_difftest_setregs(reg);
}

int difftest_step(uint32_t *reg_scala) {
  uint32_t ref_r[33];

  ref_difftest_exec(1);
  ref_difftest_getregs(&ref_r);

  if (memcmp(reg_scala, ref_r, sizeof(ref_r)) != 0) {
    int i;
    for (i = 0; i < 33; i ++) {
      if (reg_scala[i] != ref_r[i]) {
        printf("x%2d different at pc = 0x%08x, right= 0x%08x, wrong = 0x%08x\n",
            i, reg_scala[32], ref_r[i], reg_scala[i]);
      }
    }
    return 1;
  }
  return 0;
}
