#include <nemuproxy.h>
#include <unistd.h>
#include <dlfcn.h>

void (*nemu_difftest_memcpy_from_dut[NumCore])(paddr_t dest, void *src, size_t n);
void (*nemu_difftest_memcpy_from_ref[NumCore])(void *dest, paddr_t src, size_t n);
void (*nemu_difftest_getregs[NumCore])(void *c);
void (*nemu_difftest_setregs[NumCore])(const void *c);
void (*nemu_difftest_get_mastatus[NumCore])(void *s);
void (*nemu_difftest_set_mastatus[NumCore])(const void *s);
void (*nemu_difftest_get_csr[NumCore])(void *c);
void (*nemu_difftest_set_csr[NumCore])(const void *c);
vaddr_t (*nemu_disambiguate_exec[NumCore])(void *disambiguate_para);
int (*nemu_difftest_store_commit[NumCore])(uint64_t *saddr, uint64_t *sdata, uint8_t *smask);
void (*nemu_difftest_exec[NumCore])(uint64_t n);
void (*nemu_difftest_raise_intr[NumCore])(uint64_t NO);
void (*nemu_isa_reg_display[NumCore])(void);	

uint8_t* goldenMem;

void init_so(int coreid) {
  void *handle;
  handle = dlmopen(LM_ID_NEWLM, REF_SO, RTLD_LAZY | RTLD_DEEPBIND);
  puts("Using " REF_SO " for difftest");
  assert(handle);

  nemu_difftest_memcpy_from_dut[coreid] = (void (*)(paddr_t, void *, size_t))dlsym(handle, "difftest_memcpy_from_dut");
  assert(nemu_difftest_memcpy_from_dut[coreid]);

  nemu_difftest_memcpy_from_ref[coreid] = (void (*)(void *, paddr_t, size_t))dlsym(handle, "difftest_memcpy_from_ref");
  assert(nemu_difftest_memcpy_from_ref[coreid]);

  nemu_difftest_getregs[coreid] = (void (*)(void *))dlsym(handle, "difftest_getregs");
  assert(nemu_difftest_getregs[coreid]);

  nemu_difftest_setregs[coreid] = (void (*)(const void *))dlsym(handle, "difftest_setregs");
  assert(nemu_difftest_setregs[coreid]);

  nemu_difftest_get_mastatus[coreid] = (void (*)(void *))dlsym(handle, "difftest_get_mastatus");
  assert(nemu_difftest_get_mastatus[coreid]);

  nemu_difftest_set_mastatus[coreid] = (void (*)(const void *))dlsym(handle, "difftest_set_mastatus");
  assert(nemu_difftest_set_mastatus[coreid]);

  nemu_difftest_get_csr[coreid] = (void (*)(void *))dlsym(handle, "difftest_get_csr");
  assert(nemu_difftest_get_csr[coreid]);

  nemu_difftest_set_csr[coreid] = (void (*)(const void *))dlsym(handle, "difftest_set_csr");
  assert(nemu_difftest_set_csr[coreid]);

  nemu_disambiguate_exec[coreid] = (vaddr_t (*)(void *))dlsym(handle, "disambiguate_exec");
  assert(nemu_disambiguate_exec[coreid]);

  nemu_difftest_store_commit[coreid] = (int (*)(uint64_t*, uint64_t*, uint8_t*))dlsym(handle, "difftest_store_commit");
  assert(nemu_difftest_store_commit[coreid]);

  nemu_difftest_exec[coreid] = (void (*)(uint64_t))dlsym(handle, "difftest_exec");
  assert(nemu_difftest_exec[coreid]);

  nemu_difftest_raise_intr[coreid] = (void (*)(uint64_t))dlsym(handle, "difftest_raise_intr");
  assert(nemu_difftest_raise_intr[coreid]);

  nemu_isa_reg_display[coreid] = (void (*)(void))dlsym(handle, "isa_reg_display");
  assert(nemu_isa_reg_display[coreid]);

  void (*nemu_difftest_init)(void) = (void (*)(void))dlsym(handle, "difftest_init");
  assert(nemu_difftest_init);

  void (*nemu_difftest_set_mhartid)(int) = (void (*)(int))dlsym(handle, "difftest_set_mhartid");

  void (*nemu_misc_put_gmaddr)(void *) = (void (*)(void*))dlsym(handle, "misc_put_gmaddr");
  
  if (nemu_misc_put_gmaddr)
    nemu_misc_put_gmaddr(goldenMem);

  nemu_difftest_init();

  if (nemu_difftest_set_mhartid)
    nemu_difftest_set_mhartid(coreid);
}

void ref_difftest_memcpy_from_dut(paddr_t dest, void *src, size_t n, int coreid) {
  // EMU -> NEMU
  nemu_difftest_memcpy_from_dut[coreid](dest, src, n);
}

void ref_difftest_memcpy_from_ref(void *dest, paddr_t src, size_t n, int coreid) {
  // NEMU -> EMU
  nemu_difftest_memcpy_from_ref[coreid](dest, src, n);
}

void ref_difftest_getregs(void *c, int coreid) {
  nemu_difftest_getregs[coreid](c);
}

void ref_difftest_setregs(const void *c, int coreid) {
  nemu_difftest_setregs[coreid](c);
}

void ref_difftest_get_mastatus(void *s, int coreid) {
  nemu_difftest_get_mastatus[coreid](s);
}

void ref_difftest_set_mastatus(const void *s, int coreid) {
  nemu_difftest_set_mastatus[coreid](s);
}

void ref_difftest_get_csr(void *c, int coreid) {
  nemu_difftest_get_csr[coreid](c);
}

void ref_difftest_set_csr(const void *c, int coreid) {
  nemu_difftest_set_csr[coreid](c);
}

vaddr_t ref_disambiguate_exec(void *disambiguate_para, int coreid) {
  return nemu_disambiguate_exec[coreid](disambiguate_para);
}

int ref_difftest_store_commit(uint64_t *saddr, uint64_t *sdata, uint8_t *smask, int coreid) {
  return nemu_difftest_store_commit[coreid](saddr, sdata, smask);
}

void ref_difftest_exec(uint64_t n, int coreid) {
  nemu_difftest_exec[coreid](n);
}

void ref_difftest_raise_intr(uint64_t NO, int coreid) {
  nemu_difftest_raise_intr[coreid](NO);
}

void ref_isa_reg_display(int coreid) {
  nemu_isa_reg_display[coreid]();
}

void ref_difftest_init(int coreid) {
  init_so(coreid);
}

void ref_misc_put_gmaddr(uint8_t* ptr) {
  goldenMem = ptr;
}