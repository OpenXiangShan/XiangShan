#include <cstdlib>
#include <cassert>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>
#include "difftest.h"

//#include "VSimTop__Dpi.h"
#include "common.h"
#include "VXSSimTop.h"
#if VM_TRACE
#include <verilated_vcd_c.h>	// Trace file format header
#endif


class Emulator {
  const char *image;
  const char *mainargs;
  std::shared_ptr<VXSSimTop> dut_ptr;
#if VM_TRACE
  VerilatedVcdC* tfp;
#endif

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
    macro(32); macro(33); macro(34); macro(35); macro(36); macro(37); macro(38); macro(39);
    macro(40); macro(41); macro(42); macro(43); macro(44); macro(45); macro(46); macro(47);
    macro(48); macro(49); macro(50); macro(51); macro(52); macro(53); macro(54); macro(55);
    macro(56); macro(57); macro(58); macro(59); macro(60); macro(61); macro(62); macro(63);
    r[DIFFTEST_THIS_PC] = dut_ptr->io_difftest_thisPC;
    r[DIFFTEST_MSTATUS] = dut_ptr->io_difftest_mstatus;
    r[DIFFTEST_SSTATUS] = dut_ptr->io_difftest_sstatus;
    r[DIFFTEST_MEPC   ] = dut_ptr->io_difftest_mepc;
    r[DIFFTEST_SEPC   ] = dut_ptr->io_difftest_sepc;
    r[DIFFTEST_MCAUSE ] = dut_ptr->io_difftest_mcause;
    r[DIFFTEST_SCAUSE ] = dut_ptr->io_difftest_scause;
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

#if VM_TRACE
    tfp->dump(cycles);
#endif

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
    const int stuck_limit = 2000;

#if VM_TRACE
    Verilated::traceEverOn(true);	// Verilator must compute traced signals
    VL_PRINTF("Enabling waves...\n");
    tfp = new VerilatedVcdC;
    dut_ptr->trace(tfp, 99);	// Trace 99 levels of hierarchy
    tfp->open("vlt_dump.vcd");	// Open the dump file
#endif

    while (!is_finish() && n > 0) {
      single_cycle();
      n --;

      if (lastcommit - n > stuck_limit && hascommit) {
        eprintf("No instruction commits for %d cycles, maybe get stuck\n"
            "(please also check whether a fence.i instruction requires more than %d cycles to flush the icache)\n",
            stuck_limit, stuck_limit);
#if VM_TRACE
        tfp->close();
#endif
        set_abort();
      }

      if (!hascommit && dut_ptr->io_difftest_thisPC == 0x80000000u) {
        hascommit = 1;
        extern void init_difftest(uint64_t *reg);
        uint64_t reg[DIFFTEST_NR_REG];
        read_emu_regs(reg);
        init_difftest(reg);
      }

      // difftest
      if (dut_ptr->io_difftest_commit && hascommit) {
        uint64_t reg[DIFFTEST_NR_REG];
        read_emu_regs(reg);

        extern int difftest_step(uint64_t *reg_scala, uint32_t this_inst,
          int isMMIO, int isRVC, uint64_t intrNO, int priviledgeMode);
        if (difftest_step(reg, dut_ptr->io_difftest_thisINST,
              dut_ptr->io_difftest_isMMIO, dut_ptr->io_difftest_isRVC,
              dut_ptr->io_difftest_intrNO, dut_ptr->io_difftest_priviledgeMode)) {
#if VM_TRACE
          tfp->close();
#endif
          set_abort();
        }
        lastcommit = n;
      }

      uint32_t t = uptime();
      if (t - lasttime > 100) {
        poll_event();
        lasttime = t;
      }
    }
  }

  void cache_test(uint64_t n) {
    while (n > 0) {
      single_cycle();
      n --;
    }
  }

  void execute() {
//#define CACHE_TEST

#ifdef CACHE_TEST
    eprintf(ANSI_COLOR_MAGENTA "This is random test for cache.\n" ANSI_COLOR_RESET);
    cache_test(max_cycles);
#else
    execute_cycles(max_cycles);
#endif
  }
  uint64_t get_cycles() const { return cycles; }
  uint64_t get_max_cycles() const { return max_cycles; }
};
