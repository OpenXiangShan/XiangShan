#include "trap.h"

#define CHECK_MOVE(SEW, VALUE, EXPECTED_FP, EXPECTED_INT) do { \
  unsigned long fp, integer; \
  __asm__ volatile( \
      "vsetivli t0, 1, e" #SEW ", m1, tu, mu\n" \
      "vmv.v.i v8, " #VALUE "\n" \
      "vmv.x.s %1, v8\n" \
      "vfmv.f.s ft0, v8\n" \
      "fmv.x.d %0, ft0\n" \
      : "=&r"(fp), "=&r"(integer) \
      : \
      : "t0", "ft0", "memory"); \
  nemu_assert(fp == (EXPECTED_FP) && integer == (EXPECTED_INT)); \
} while (0)

int main(const char *args) {
  __asm__ volatile("csrs mstatus, %0" : : "r"((1UL << 9) | (1UL << 13)) : "memory");
  CHECK_MOVE(64, 1, 1UL, 1UL);
  CHECK_MOVE(32, -1, ~0UL, ~0UL);
  printf("Full-width and negative-value controls passed\n");
  if (args[0] == 'c') return 0;

  CHECK_MOVE(32, 1, 0xffffffff00000001UL, 1UL);
  CHECK_MOVE(32, 0, 0xffffffff00000000UL, 0UL);
  CHECK_MOVE(16, 1, 0xffffffffffff0001UL, 1UL);
  CHECK_MOVE(16, 0, 0xffffffffffff0000UL, 0UL);
  CHECK_MOVE(16, -1, ~0UL, ~0UL);
  printf("Floating scalar move NaN-boxing regression passed\n");
  return 0;
}
