# See LICENSE for license details.

#*****************************************************************************
# dirty.S
#-----------------------------------------------------------------------------
#
# Test VM referenced and dirty bits.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64M
RV_COMPLIANCE_CODE_BEGIN

  # Turn on VM
  li a0, (SATP_MODE & ~(SATP_MODE<<1)) * SATP_MODE_SV39
  la a1, page_table_1
  srl a1, a1, RISCV_PGSHIFT
  or a1, a1, a0
  csrw sptbr, a1
  sfence.vma

  # Set up MPRV with MPP=S, so loads and stores use S-mode
  li a1, ((MSTATUS_MPP & ~(MSTATUS_MPP<<1)) * PRV_S) | MSTATUS_MPRV
  csrs mstatus, a1

  # Try a faulting store to make sure dirty bit is not set
  li TESTNUM, 2
  li t2, 1
  sw t2, dummy - DRAM_BASE, a0

  # Set SUM=1 so user memory access is permitted
  li TESTNUM, 3
  li a1, ((MSTATUS_MPP & ~(MSTATUS_MPP<<1)) * PRV_S) | MSTATUS_SUM
  csrs mstatus, a1

  # Make sure SUM=1 works
  lw t0, dummy - DRAM_BASE
  bnez t0, die

  # Try a non-faulting store to make sure dirty bit is set
  sw t2, dummy - DRAM_BASE, a0

  # Make sure it succeeded
  lw t0, dummy - DRAM_BASE
  bne t0, t2, die

  # Leave MPRV
  li t0, MSTATUS_MPRV
  csrc mstatus, t0

  # Make sure D bit is set
  lw t0, page_table_1
  li a0, PTE_A | PTE_D
  and t0, t0, a0
  bne t0, a0, die

  # Enter MPRV again
  li t0, MSTATUS_MPRV
  csrs mstatus, t0

  # Make sure that superpage entries trap when PPN LSBs are set.
  li TESTNUM, 4
  lw a0, page_table_1 - DRAM_BASE
  or a0, a0, 1 << PTE_PPN_SHIFT
  sw a0, page_table_1 - DRAM_BASE, t0
  sfence.vma
  sw a0, page_table_1 - DRAM_BASE, t0
  j die
  
  RV_COMPLIANCE_HALT

  TEST_PASSFAIL

  .align 2
  .global mtvec_handler
mtvec_handler:
  csrr t0, mcause
  add t0, t0, -CAUSE_STORE_PAGE_FAULT
  bnez t0, die

  li t1, 2
  bne TESTNUM, t1, 1f
  # Make sure D bit is clear
  lw t0, page_table_1
  and t1, t0, PTE_D
  bnez t1, die
skip:
  csrr t0, mepc
  add t0, t0, 4
  csrw mepc, t0
  mret

1:
  li t1, 3
  bne TESTNUM, t1, 1f
  # The implementation doesn't appear to set D bits in HW.
  # Make sure the D bit really is clear.
  lw t0, page_table_1
  and t1, t0, PTE_D
  bnez t1, die
  # Set the D bit.
  or t0, t0, PTE_D
  sw t0, page_table_1, t1
  sfence.vma
  mret

1:
  li t1, 4
  bne TESTNUM, t1, 1f
  j pass

1:
die:
  RV_COMPLIANCE_HALT

RV_COMPLIANCE_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

.align 12
page_table_1: .dword (DRAM_BASE/RISCV_PGSIZE << PTE_PPN_SHIFT) | PTE_V | PTE_U | PTE_R | PTE_W | PTE_X | PTE_A
dummy: .dword 0

RVTEST_DATA_END
