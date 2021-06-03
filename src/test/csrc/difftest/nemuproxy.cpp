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

#include "nemuproxy.h"
#include <unistd.h>
#include <dlfcn.h>

uint8_t* goldenMem;
const char *difftest_ref_so = NULL;

NemuProxy::NemuProxy(int coreid) {
  if (difftest_ref_so == NULL) {
    printf("--diff is not given, "
        "try to use $(NEMU_HOME)/build/riscv64-nemu-interpreter-so by default\n");
    const char *nemu_home = getenv("NEMU_HOME");
    if (nemu_home == NULL) {
      printf("FATAL: $(NEMU_HOME) is not defined!\n");
      exit(1);
    }
    const char *so = "/build/riscv64-nemu-interpreter-so";
    char *buf = (char *)malloc(strlen(nemu_home) + strlen(so) + 1);
    strcpy(buf, nemu_home);
    strcat(buf, so);
    difftest_ref_so = buf;
  }

  printf("Using %s for difftest\n", difftest_ref_so);

  void *handle = dlmopen(LM_ID_NEWLM, difftest_ref_so, RTLD_LAZY | RTLD_DEEPBIND);
  assert(handle);

  memcpy_from_dut = (void (*)(paddr_t, void *, size_t))dlsym(handle, "difftest_memcpy_from_dut");
  assert(memcpy_from_dut);

  memcpy_from_ref = (void (*)(void *, paddr_t, size_t))dlsym(handle, "difftest_memcpy_from_ref");
  assert(memcpy_from_ref);

  get_regs = (void (*)(void *))dlsym(handle, "difftest_getregs");
  assert(get_regs);

  set_regs = (void (*)(const void *))dlsym(handle, "difftest_setregs");
  assert(set_regs);

  get_mastatus = (void (*)(void *))dlsym(handle, "difftest_get_mastatus");
  assert(get_mastatus);

  set_mastatus = (void (*)(const void *))dlsym(handle, "difftest_set_mastatus");
  assert(set_mastatus);

  get_csr = (void (*)(void *))dlsym(handle, "difftest_get_csr");
  assert(get_csr);

  set_csr = (void (*)(const void *))dlsym(handle, "difftest_set_csr");
  assert(set_csr);

  disambiguate_exec = (vaddr_t (*)(void *))dlsym(handle, "disambiguate_exec");
  assert(disambiguate_exec);

  store_commit = (int (*)(uint64_t*, uint64_t*, uint8_t*))dlsym(handle, "difftest_store_commit");
  assert(store_commit);

  exec = (void (*)(uint64_t))dlsym(handle, "difftest_exec");
  assert(exec);

  raise_intr = (void (*)(uint64_t))dlsym(handle, "difftest_raise_intr");
  assert(raise_intr);

  isa_reg_display = (void (*)(void))dlsym(handle, "isa_reg_display");
  assert(isa_reg_display);

  auto nemu_difftest_set_mhartid = (void (*)(int))dlsym(handle, "difftest_set_mhartid");
  auto nemu_misc_put_gmaddr = (void (*)(void*))dlsym(handle, "misc_put_gmaddr");

  if (EMU_CORES > 1) {
    assert(nemu_difftest_set_mhartid);
    assert(nemu_misc_put_gmaddr);
  }

  if (nemu_difftest_set_mhartid) {
    nemu_difftest_set_mhartid(coreid);
  }
  if (nemu_misc_put_gmaddr) {
    nemu_misc_put_gmaddr(goldenMem);
  }

  auto nemu_init = (void (*)(void))dlsym(handle, "difftest_init");
  assert(nemu_init);

  nemu_init();
}

void ref_misc_put_gmaddr(uint8_t* ptr) {
  goldenMem = ptr;
}
