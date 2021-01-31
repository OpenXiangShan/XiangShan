#ifndef __MEMORY_PADDR_H__
#define __MEMORY_PADDR_H__

#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

typedef uint64_t paddr_t;
typedef uint64_t word_t;

#define Assert(cond, ...) \
  do { \
    if (!(cond)) { \
      fflush(stdout); \
      fprintf(stderr, "\33[1;31m"); \
      fprintf(stderr, __VA_ARGS__); \
      fprintf(stderr, "\33[0m\n"); \
      assert(cond); \
    } \
  } while (0)

#define panic(...) Assert(0, __VA_ARGS__)

#define PMEM_BASE 0x80000000
#define PMEM_SIZE (8 * 1024 * 1024 * 1024UL)
// #define PMEM_SIZE (256 * 1024 * 1024UL) 

extern uint8_t* pmem;

void init_goldenmem();
void update_goldenmem(paddr_t addr, void *data, uint64_t mask, int len);
void read_goldenmem(paddr_t addr, void *data, uint64_t len);

/* convert the guest physical address in the guest program to host virtual address in NEMU */
void* guest_to_host(paddr_t addr);
/* convert the host virtual address in NEMU to guest physical address in the guest program */
paddr_t host_to_guest(void *addr);

word_t paddr_read(paddr_t addr, int len);
void paddr_write(paddr_t addr, word_t data, int len);
bool is_sfence_safe(paddr_t addr, int len);

#endif
