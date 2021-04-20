#include "nemuproxy.h"
#include <unistd.h>
#include <dlfcn.h>

uint8_t* goldenMem;

#ifndef REF_SO
# error Please define REF_SO to the path of NEMU shared object file
#endif

NemuProxy::NemuProxy(int coreid) {
  puts("Using " REF_SO " for difftest");

  void *handle = dlmopen(LM_ID_NEWLM, REF_SO, RTLD_LAZY | RTLD_DEEPBIND);
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
  // assert(nemu_difftest_set_mhartid);
  auto nemu_misc_put_gmaddr = (void (*)(void*))dlsym(handle, "misc_put_gmaddr");
  // assert(nemu_misc_put_gmaddr);
  auto nemu_init = (void (*)(void))dlsym(handle, "difftest_init");
  assert(nemu_init);

  if (nemu_difftest_set_mhartid) {
    nemu_difftest_set_mhartid(coreid);
  }
  if (nemu_misc_put_gmaddr) {
    nemu_misc_put_gmaddr(goldenMem);
  }
  nemu_init();
}

void ref_misc_put_gmaddr(uint8_t* ptr) {
  goldenMem = ptr;
}
