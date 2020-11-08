// See LICENSE for license details.

#include <stdint.h>
#include <string.h>
#include <stdio.h>

#include "riscv_test.h"

void trap_entry();
void pop_tf(trapframe_t*);

volatile uint64_t tohost;
volatile uint64_t fromhost;

static void do_tohost(uint64_t tohost_value)
{
  while (tohost)
    fromhost = 0;
  tohost = tohost_value;
}

#define pa2kva(pa) ((void*)(pa) - DRAM_BASE - MEGAPAGE_SIZE)
#define uva2kva(pa) ((void*)(pa) - MEGAPAGE_SIZE)

#define flush_page(addr) asm volatile ("sfence.vma %0" : : "r" (addr) : "memory")

static uint64_t lfsr63(uint64_t x)
{
  uint64_t bit = (x ^ (x >> 1)) & 1;
  return (x >> 1) | (bit << 62);
}

static void cputchar(int x)
{
  do_tohost(0x0101000000000000 | (unsigned char)x);
}

static void cputstring(const char* s)
{
  while (*s)
    cputchar(*s++);
}

static void terminate(int code)
{
  do_tohost(code);
  while (1);
}

void wtf()
{
  terminate(841);
}

#define stringify1(x) #x
#define stringify(x) stringify1(x)
#define assert(x) do { \
  if (x) break; \
  cputstring("Assertion failed: " stringify(x) "\n"); \
  terminate(3); \
} while(0)

#define l1pt pt[0]
#define user_l2pt pt[1]
#if __riscv_xlen == 64
# define NPT 4
#define kernel_l2pt pt[2]
# define user_l3pt pt[3]
#else
# define NPT 2
# define user_l3pt user_l2pt
#endif
pte_t pt[NPT][PTES_PER_PT] __attribute__((aligned(PGSIZE)));

typedef struct { pte_t addr; void* next; } freelist_t;

freelist_t user_mapping[MAX_TEST_PAGES];
freelist_t freelist_nodes[MAX_TEST_PAGES];
freelist_t *freelist_head, *freelist_tail;

void printhex(uint64_t x)
{
  char str[17];
  for (int i = 0; i < 16; i++)
  {
    str[15-i] = (x & 0xF) + ((x & 0xF) < 10 ? '0' : 'a'-10);
    x >>= 4;
  }
  str[16] = 0;

  cputstring(str);
}

static void evict(unsigned long addr)
{
  assert(addr >= PGSIZE && addr < MAX_TEST_PAGES * PGSIZE);
  addr = addr/PGSIZE*PGSIZE;

  freelist_t* node = &user_mapping[addr/PGSIZE];
  if (node->addr)
  {
    // check accessed and dirty bits
    assert(user_l3pt[addr/PGSIZE] & PTE_A);
    uintptr_t sstatus = set_csr(sstatus, SSTATUS_SUM);
    if (memcmp((void*)addr, uva2kva(addr), PGSIZE)) {
      assert(user_l3pt[addr/PGSIZE] & PTE_D);
      memcpy((void*)addr, uva2kva(addr), PGSIZE);
    }
    write_csr(sstatus, sstatus);

    user_mapping[addr/PGSIZE].addr = 0;

    if (freelist_tail == 0)
      freelist_head = freelist_tail = node;
    else
    {
      freelist_tail->next = node;
      freelist_tail = node;
    }
  }
}

void handle_fault(uintptr_t addr, uintptr_t cause)
{
  assert(addr >= PGSIZE && addr < MAX_TEST_PAGES * PGSIZE);
  addr = addr/PGSIZE*PGSIZE;

  if (user_l3pt[addr/PGSIZE]) {
    if (!(user_l3pt[addr/PGSIZE] & PTE_A)) {
      user_l3pt[addr/PGSIZE] |= PTE_A;
    } else {
      assert(!(user_l3pt[addr/PGSIZE] & PTE_D) && cause == CAUSE_STORE_PAGE_FAULT);
      user_l3pt[addr/PGSIZE] |= PTE_D;
    }
    flush_page(addr);
    return;
  }

  freelist_t* node = freelist_head;
  assert(node);
  freelist_head = node->next;
  if (freelist_head == freelist_tail)
    freelist_tail = 0;

  uintptr_t new_pte = (node->addr >> PGSHIFT << PTE_PPN_SHIFT) | PTE_V | PTE_U | PTE_R | PTE_W | PTE_X;
  user_l3pt[addr/PGSIZE] = new_pte | PTE_A | PTE_D;
  flush_page(addr);

  assert(user_mapping[addr/PGSIZE].addr == 0);
  user_mapping[addr/PGSIZE] = *node;

  uintptr_t sstatus = set_csr(sstatus, SSTATUS_SUM);
  memcpy((void*)addr, uva2kva(addr), PGSIZE);
  write_csr(sstatus, sstatus);

  user_l3pt[addr/PGSIZE] = new_pte;
  flush_page(addr);

  __builtin___clear_cache(0,0);
}

void handle_trap(trapframe_t* tf)
{
  if (tf->cause == CAUSE_USER_ECALL)
  {
    int n = tf->gpr[10];

    for (long i = 1; i < MAX_TEST_PAGES; i++)
      evict(i*PGSIZE);

    terminate(n);
  }
  else if (tf->cause == CAUSE_ILLEGAL_INSTRUCTION)
  {
    assert(tf->epc % 4 == 0);

    int* fssr;
    asm ("jal %0, 1f; fssr x0; 1:" : "=r"(fssr));

    if (*(int*)tf->epc == *fssr)
      terminate(1); // FP test on non-FP hardware.  "succeed."
    else
      assert(!"illegal instruction");
    tf->epc += 4;
  }
  else if (tf->cause == CAUSE_FETCH_PAGE_FAULT || tf->cause == CAUSE_LOAD_PAGE_FAULT || tf->cause == CAUSE_STORE_PAGE_FAULT)
    handle_fault(tf->badvaddr, tf->cause);
  else
    assert(!"unexpected exception");

  pop_tf(tf);
}

static void coherence_torture()
{
  // cause coherence misses without affecting program semantics
  unsigned int random = ENTROPY;
  while (1) {
    uintptr_t paddr = DRAM_BASE + ((random % (2 * (MAX_TEST_PAGES + 1) * PGSIZE)) & -4);
#ifdef __riscv_atomic
    if (random & 1) // perform a no-op write
      asm volatile ("amoadd.w zero, zero, (%0)" :: "r"(paddr));
    else // perform a read
#endif
      asm volatile ("lw zero, (%0)" :: "r"(paddr));
    random = lfsr63(random);
  }
}

void vm_boot(uintptr_t test_addr)
{
  unsigned int random = ENTROPY;
  if (read_csr(mhartid) > 0)
    coherence_torture();

  _Static_assert(SIZEOF_TRAPFRAME_T == sizeof(trapframe_t), "???");

#if (MAX_TEST_PAGES > PTES_PER_PT) || (DRAM_BASE % MEGAPAGE_SIZE) != 0
# error
#endif
  // map user to lowermost megapage
  l1pt[0] = ((pte_t)user_l2pt >> PGSHIFT << PTE_PPN_SHIFT) | PTE_V;
  // map kernel to uppermost megapage
#if __riscv_xlen == 64
  l1pt[PTES_PER_PT-1] = ((pte_t)kernel_l2pt >> PGSHIFT << PTE_PPN_SHIFT) | PTE_V;
  kernel_l2pt[PTES_PER_PT-1] = (DRAM_BASE/RISCV_PGSIZE << PTE_PPN_SHIFT) | PTE_V | PTE_R | PTE_W | PTE_X | PTE_A | PTE_D;
  user_l2pt[0] = ((pte_t)user_l3pt >> PGSHIFT << PTE_PPN_SHIFT) | PTE_V;
  uintptr_t vm_choice = SPTBR_MODE_SV39;
#else
  l1pt[PTES_PER_PT-1] = (DRAM_BASE/RISCV_PGSIZE << PTE_PPN_SHIFT) | PTE_V | PTE_R | PTE_W | PTE_X | PTE_A | PTE_D;
  uintptr_t vm_choice = SPTBR_MODE_SV32;
#endif
  write_csr(sptbr, ((uintptr_t)l1pt >> PGSHIFT) |
                   (vm_choice * (SPTBR_MODE & ~(SPTBR_MODE<<1))));

  // Set up PMPs if present, ignoring illegal instruction trap if not.
  uintptr_t pmpc = PMP_NAPOT | PMP_R | PMP_W | PMP_X;
  asm volatile ("la t0, 1f\n\t"
                "csrrw t0, mtvec, t0\n\t"
                "csrw pmpaddr0, %1\n\t"
                "csrw pmpcfg0, %0\n\t"
                ".align 2\n\t"
                "1:"
                : : "r" (pmpc), "r" (-1UL) : "t0");

  // set up supervisor trap handling
  write_csr(stvec, pa2kva(trap_entry));
  write_csr(sscratch, pa2kva(read_csr(mscratch)));
  write_csr(medeleg,
    (1 << CAUSE_USER_ECALL) |
    (1 << CAUSE_FETCH_PAGE_FAULT) |
    (1 << CAUSE_LOAD_PAGE_FAULT) |
    (1 << CAUSE_STORE_PAGE_FAULT));
  // FPU on; accelerator on; allow supervisor access to user memory access
  write_csr(mstatus, MSTATUS_FS | MSTATUS_XS);
  write_csr(mie, 0);

  random = 1 + (random % MAX_TEST_PAGES);
  freelist_head = pa2kva((void*)&freelist_nodes[0]);
  freelist_tail = pa2kva(&freelist_nodes[MAX_TEST_PAGES-1]);
  for (long i = 0; i < MAX_TEST_PAGES; i++)
  {
    freelist_nodes[i].addr = DRAM_BASE + (MAX_TEST_PAGES + random)*PGSIZE;
    freelist_nodes[i].next = pa2kva(&freelist_nodes[i+1]);
    random = LFSR_NEXT(random);
  }
  freelist_nodes[MAX_TEST_PAGES-1].next = 0;

  trapframe_t tf;
  memset(&tf, 0, sizeof(tf));
  tf.epc = test_addr - DRAM_BASE;
  pop_tf(&tf);
}
