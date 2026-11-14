#include <am.h>
#include <klib.h>
#include <stdint.h>

/* Cold-base jumps exceed L1 capacity; only the first word of each block matters.
 * This checks architecture/checksums. Counter checks prove policy exercise;
 * it is not a performance benchmark or an IPC claim.
 */
#define BLOCK_BYTES 64
#define REGION_BLOCKS 16
#define SLOT_BLOCKS 2048
#define SLOTS 24
static uint64_t data[SLOTS * SLOT_BLOCKS * 8] __attribute__((aligned(1024)));
extern uint64_t stream_walk(volatile uint64_t *, uint64_t, int64_t);

static uint64_t value(uint64_t b) { return (b * 0x10201ULL) ^ 0x512ab7ULL; }

static void run_phase(const char *name, const unsigned *lengths, unsigned count, int reverse) {
  uint64_t actual = 0, expected = 0;
  for (unsigned rep = 0; rep < count; ++rep) {
    unsigned blocks = lengths[rep] * REGION_BLOCKS;
    unsigned first = (rep % SLOTS) * SLOT_BLOCKS;
    if (reverse) first += blocks - 1;
    actual += stream_walk(&data[first * 8], blocks, reverse ? -BLOCK_BYTES : BLOCK_BYTES);
    for (unsigned b = 0; b < blocks; ++b) expected += value(reverse ? first - b : first + b);
  }
  if (actual != expected) {
    printf("FAIL %s checksum %lx expected %lx\n", name, actual, expected);
    _halt(1);
  }
  printf("PASS %s checksum %lx\n", name, actual);
}

int main(const char *args) {
  (void)args;
  for (unsigned b = 0; b < SLOTS * SLOT_BLOCKS; ++b) data[b * 8] = value(b);
  __asm__ volatile("fence rw,rw" ::: "memory");
  /* Keep native depths/controllers intact. Disable other L1 predictors so the
   * checksum workload trains Stream with the intended load PC. */
  /* The bundled NEMU predates the implementation-specific high SPFCTL bits.
   * Use rd=x0 CSR set/clear forms: no implementation-specific read value enters
   * architectural GPR state. All instructions still undergo difftest.
   */
  const uint64_t set_pf = (1ULL << 33) | (1ULL << 2) | (1ULL << 3);
  const uint64_t clear_pf = (1ULL << 32) | (1ULL << 16) | (1ULL << 5) | (1ULL << 4);
  __asm__ volatile("csrs 0x5c1, %0" :: "r"(set_pf) : "memory");
  __asm__ volatile("csrc 0x5c1, %0" :: "r"(clear_pf) : "memory");
  const unsigned stable[] = {8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8};
  const unsigned change[] = {16,16,16,16,8,8,8,8,4,4,4,4,24,24,24,24,6,6,6,6};
  const unsigned distant[] = {96,96,96,96,96,96,96,96};
  run_phase("stable-forward", stable, sizeof(stable)/sizeof(stable[0]), 0);
  run_phase("growth-confirmed-shrink", change, sizeof(change)/sizeof(change[0]), 0);
  run_phase("stable-reverse", stable, sizeof(stable)/sizeof(stable[0]), 1);
  run_phase("distant-l2-l3", distant, sizeof(distant)/sizeof(distant[0]), 0);
  /* Context fence must clear all history and in-flight table references. */
  __asm__ volatile("sfence.vma" ::: "memory");
  run_phase("after-fence", stable, sizeof(stable)/sizeof(stable[0]), 0);
  printf("STREAM_HISTORY_SMOKE PASS\n");
  return 0;
}
