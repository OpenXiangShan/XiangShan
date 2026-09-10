#include "trap.h"

int main(const char *args) {
  unsigned long vtype, vl, rd;
  __asm__ volatile("csrs mstatus, %0" : : "r"(1UL << 9) : "memory");

  __asm__ volatile(
      "vsetivli t0, 1, e8, m1, tu, mu\n"
      "vsetivli t1, 0, e16, m1, tu, mu\n"
      "csrr %0, vtype\n"
      "csrr %1, vl\n"
      "mv %2, t1\n"
      : "=r"(vtype), "=r"(vl), "=r"(rd)
      :
      : "t0", "t1", "memory");
  nemu_assert(vtype == 0x08 && vl == 0 && rd == 0);
  printf("Nonzero-rd control passed\n");
  if (args[0] == 'c') return 0;

  __asm__ volatile(
      "vsetivli t0, 1, e8, m1, tu, mu\n"
      "vsetivli zero, 0, e16, m1, tu, mu\n"
      "csrr %0, vtype\n"
      "csrr %1, vl\n"
      : "=r"(vtype), "=r"(vl)
      :
      : "t0", "memory");
  nemu_assert(vtype == 0x08 && vl == 0);
  printf("Zero-rd zero-AVL regression passed\n");
  return 0;
}
