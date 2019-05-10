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
  const char *bram_image;
  std::shared_ptr<VNOOPSimTop> dut_ptr;

  // emu control variable
  uint32_t seed;
  uint64_t max_cycles, cycles;

  void prepare_block_ram(const char *image) {
    /*std::ofstream ofs(BRAM_BIN_TXT);
      if(image == NULL) {
      ofs << "3c088000\n"   // lui t0, 0x8000
      << "25080000\n"     // addiu t0, t0, 0
      << "01000008\n"     // jr t0
      << "00000000\n";    // nop

      for(int i = 0; i < 100; i ++) {
      ofs << "00000000\n";
      }
      } else {
      std::ifstream ifs(image, std::ios::binary);
      assert(ifs.good());
      ofs.fill('0');
      while(ifs.good()) {
      uint32_t val;
      ifs.read(reinterpret_cast<char*>(&val), sizeof(val));
      ofs << std::hex << std::setw(8) << val << "\n";
      }
      }
      */
  }

  std::vector<const char *> parse_args(int argc, const char *argv[]);

  static const struct option long_options[];
  static void print_help(const char *file);

  public:
  // argv decay to the secondary pointer
  Emulator(int argc, const char *argv[]):
    bram_image(nullptr),
    dut_ptr(new std::remove_reference<decltype(*dut_ptr)>::type),
    seed(0), max_cycles(-1), cycles(0)
  {
    // init emu
    auto args = parse_args(argc, argv);
    prepare_block_ram(bram_image);

    // srand
    srand(seed);
    srand48(seed);
    Verilated::randReset(2);

    // init device
    //init_device();

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

    cycles ++;

  }

  int execute_cycles(uint64_t n) {
    extern bool is_finish;
    while (!is_finish && n > 0) {
      single_cycle();
      n --;
    }

    return !is_finish;
  }

  int execute() { return execute_cycles(max_cycles); }
  uint64_t get_cycles() const { return max_cycles; }
  uint64_t get_max_cycles() const { return max_cycles; }
};
