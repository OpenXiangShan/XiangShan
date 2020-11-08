# See LICENSE for license details.

#*****************************************************************************
# rvc.S
#-----------------------------------------------------------------------------
#
# Test RVC corner cases.
#

#include "riscv_test.h"
#include "compliance_test.h"
#include "compliance_io.h"
#include "aw_test_macros.h"


RVTEST_RV64U
RV_COMPLIANCE_CODE_BEGIN

  .align 2
  .option push
  .option norvc

  #define RVC_TEST_CASE(n, r, v, code...) \
    TEST_CASE (n, r, v, .option push; .option rvc; code; .align 2; .option pop)

  // Make sure fetching a 4-byte instruction across a page boundary works.
  li TESTNUM, 2
  li a1, 666
  TEST_CASE (2, a1, 667, \
        j 1f; \
        .align 3; \
        data: \
          .dword 0xfedcba9876543210; \
          .dword 0xfedcba9876543210; \
        .align 12; \
        .skip 4094; \
      1: addi a1, a1, 1)

  li sp, 0x1234
  RVC_TEST_CASE (3, a0, 0x1234 + 1020, c.addi4spn a0, sp, 1020)
  RVC_TEST_CASE (4, sp, 0x1234 + 496, c.addi16sp sp, 496)
  RVC_TEST_CASE (5, sp, 0x1234 + 496 - 512, c.addi16sp sp, -512)

  la a1, data
  RVC_TEST_CASE (6, a2, 0xfffffffffedcba99, c.lw a0, 4(a1); addi a0, a0, 1; c.sw a0, 4(a1); c.lw a2, 4(a1))
#if __riscv_xlen == 64
  RVC_TEST_CASE (7, a2, 0xfedcba9976543211, c.ld a0, 0(a1); addi a0, a0, 1; c.sd a0, 0(a1); c.ld a2, 0(a1))
#endif

  RVC_TEST_CASE (8, a0, -15, ori a0, x0, 1; c.addi a0, -16)
  RVC_TEST_CASE (9, a5, -16, ori a5, x0, 1; c.li a5, -16)
#if __riscv_xlen == 64
  RVC_TEST_CASE (10, a0, 0x76543210, ld a0, (a1); c.addiw a0, -1)
#endif

  RVC_TEST_CASE (11, s0, 0xffffffffffffffe1, c.lui s0, 0xfffe1; c.srai s0, 12)
#if __riscv_xlen == 64
  RVC_TEST_CASE (12, s0, 0x000fffffffffffe1, c.lui s0, 0xfffe1; c.srli s0, 12)
#else
  RVC_TEST_CASE (12, s0, 0x000fffe1, c.lui s0, 0xfffe1; c.srli s0, 12)
#endif
  RVC_TEST_CASE (14, s0, ~0x11, c.li s0, -2; c.andi s0, ~0x10)
  RVC_TEST_CASE (15, s1, 14, li s1, 20; li a0, 6; c.sub s1, a0)
  RVC_TEST_CASE (16, s1, 18, li s1, 20; li a0, 6; c.xor s1, a0)
  RVC_TEST_CASE (17, s1, 22, li s1, 20; li a0, 6; c.or s1, a0)
  RVC_TEST_CASE (18, s1,  4, li s1, 20; li a0, 6; c.and s1, a0)
#if __riscv_xlen == 64
  RVC_TEST_CASE (19, s1, 0xffffffff80000000, li s1, 0x7fffffff; li a0, -1; c.subw s1, a0)
  RVC_TEST_CASE (20, s1, 0xffffffff80000000, li s1, 0x7fffffff; li a0, 1; c.addw s1, a0)
#endif
  RVC_TEST_CASE (21, s0, 0x12340, li s0, 0x1234; c.slli s0, 4)

  RVC_TEST_CASE (30, ra, 0, \
        li ra, 0; \
        c.j 1f; \
        c.j 2f; \
      1:c.j 1f; \
      2:j fail; \
      1:)

  RVC_TEST_CASE (31, x0, 0, \
        li a0, 0; \
        c.beqz a0, 1f; \
        c.j 2f; \
      1:c.j 1f; \
      2:j fail; \
      1:)

  RVC_TEST_CASE (32, x0, 0, \
        li a0, 1; \
        c.bnez a0, 1f; \
        c.j 2f; \
      1:c.j 1f; \
      2:j fail; \
      1:)

  RVC_TEST_CASE (33, x0, 0, \
        li a0, 1; \
        c.beqz a0, 1f; \
        c.j 2f; \
      1:c.j fail; \
      2:)

  RVC_TEST_CASE (34, x0, 0, \
        li a0, 0; \
        c.bnez a0, 1f; \
        c.j 2f; \
      1:c.j fail; \
      2:)

  RVC_TEST_CASE (35, ra, 0, \
        la t0, 1f; \
        li ra, 0; \
        c.jr t0; \
        c.j 2f; \
      1:c.j 1f; \
      2:j fail; \
      1:)

  RVC_TEST_CASE (36, ra, -2, \
        la t0, 1f; \
        li ra, 0; \
        c.jalr t0; \
        c.j 2f; \
      1:c.j 1f; \
      2:j fail; \
      1:sub ra, ra, t0)

#if __riscv_xlen == 32
  RVC_TEST_CASE (37, ra, -2, \
        la t0, 1f; \
        li ra, 0; \
        c.jal 1f; \
        c.j 2f; \
      1:c.j 1f; \
      2:j fail; \
      1:sub ra, ra, t0)
#endif

  la sp, data
  RVC_TEST_CASE (40, a2, 0xfffffffffedcba99, c.lwsp a0, 12(sp); addi a0, a0, 1; c.swsp a0, 12(sp); c.lwsp a2, 12(sp))
#if __riscv_xlen == 64
  RVC_TEST_CASE (41, a2, 0xfedcba9976543211, c.ldsp a0, 8(sp); addi a0, a0, 1; c.sdsp a0, 8(sp); c.ldsp a2, 8(sp))
#endif

  RVC_TEST_CASE (42, t0, 0x246, li a0, 0x123; c.mv t0, a0; c.add t0, a0)

  .option pop

  TEST_PASSFAIL

RV_COMPLIANCE_CODE_END

  .data
RV_COMPLIANCE_DATA_BEGIN
test_res:
    .fill 48, 4, -1
RV_COMPLIANCE_DATA_END

