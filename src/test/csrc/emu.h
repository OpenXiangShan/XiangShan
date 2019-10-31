#include <cstdlib>
#include <cassert>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>

//#include "VSimTop__Dpi.h"
#include "common.h"
#include "VNOOPSimTop.h"

class Emulator {
  const char *image;
  const char *mainargs;
  std::shared_ptr<VNOOPSimTop> dut_ptr;

  // emu control variable
  uint32_t seed;
  uint64_t max_cycles, cycles;

  std::vector<const char *> parse_args(int argc, const char *argv[]);

  static const struct option long_options[];
  static void print_help(const char *file);

  void read_emu_regs(uint64_t *r) {
#define macro(x) r[x] = dut_ptr->io_difftest_r_##x
    macro(0); macro(1); macro(2); macro(3); macro(4); macro(5); macro(6); macro(7);
    macro(8); macro(9); macro(10); macro(11); macro(12); macro(13); macro(14); macro(15);
    macro(16); macro(17); macro(18); macro(19); macro(20); macro(21); macro(22); macro(23);
    macro(24); macro(25); macro(26); macro(27); macro(28); macro(29); macro(30); macro(31);
    r[32] = dut_ptr->io_difftest_thisPC;
  }

  public:
  // argv decay to the secondary pointer
  Emulator(int argc, const char *argv[]):
    image(nullptr),
    mainargs(NULL),
    dut_ptr(new std::remove_reference<decltype(*dut_ptr)>::type),
    seed(0), max_cycles(-1), cycles(0)
  {
    // init emu
    auto args = parse_args(argc, argv);

    // srand
    srand(seed);
    srand48(seed);
    Verilated::randReset(2);

    // init ram
    extern void init_ram(const char *img, const char *mainargs);
    init_ram(image, mainargs);

    // init device
    extern void init_device(void);
    init_device();

    // init core
    reset_ncycles(10);

    extern void init_difftest(uint64_t *reg);
    uint64_t reg[33];
    read_emu_regs(reg);
    reg[32] = 0x80000000;
    init_difftest(reg);
  }

  void reset_ncycles(size_t cycles) {
    for(int i = 0; i < cycles; i++) {
      dut_ptr->reset = 1;
      dut_ptr->clock = 0;
      dut_ptr->eval();
      dut_ptr->clock = 1;
      dut_ptr->eval();
      dut_ptr->reset = 0;
    }
  }

  void single_cycle() {
    dut_ptr->clock = 0;
    dut_ptr->eval();

    dut_ptr->clock = 1;
    dut_ptr->eval();

    cycles ++;

  }

  void execute_cycles(uint64_t n) {
    extern bool is_finish();
    extern void poll_event(void);
    extern uint32_t uptime(void);
    extern void set_abort(void);
    uint32_t lasttime = 0;
    uint64_t lastcommit = n;
    int hascommit = 0;
    const int stuck_limit = 200;
    while (!is_finish() && n > 0) {
      single_cycle();
      n --;

      if (lastcommit - n > stuck_limit && hascommit) {
        eprintf("No instruction commits for %d cycles, maybe get stuck\n", stuck_limit);
        set_abort();
      }

      // difftest
      if (dut_ptr->io_difftest_commit) {
        uint64_t reg[33];
        read_emu_regs(reg);

        extern int difftest_step(uint64_t *reg_scala, uint64_t this_pc, int isMMIO, int isRVC, uint64_t intrNO);
        if (difftest_step(reg, dut_ptr->io_difftest_thisPC, dut_ptr->io_difftest_isMMIO, 
          dut_ptr->io_difftest_isRVC, dut_ptr->io_difftest_intrNO)) {
          set_abort();
        }
        lastcommit = n;
        hascommit = 1;
      }

      uint32_t t = uptime();
      if (t - lasttime > 100) {
        poll_event();
        lasttime = t;
      }
    }
  }

  void execute() { execute_cycles(max_cycles); }
  uint64_t get_cycles() const { return cycles; }
  uint64_t get_max_cycles() const { return max_cycles; }
};
