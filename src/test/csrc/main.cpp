#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <memory>
#include <getopt.h>
#include <string.h>
#include <iomanip>
#include <fstream>
#include <functional>

#include "emu.h"

// junk, link for verilator
std::function<double()> get_sc_time_stamp = []() -> double { return 0; };
double sc_time_stamp() { return get_sc_time_stamp(); }

const struct option Emulator::long_options[] = {
  { "seed",           1, NULL, 's' },
  { "max-cycles",     1, NULL, 'C' },
  { "uImage",         1, NULL, 'u' }, // common
  { "help",           0, NULL, 'h' },
  // parse for nemu
  { "symbol",         1, NULL, 'S' },
  { "batch",          0, NULL, 'b' },
  { "image",          1, NULL, 'i' },
  { "elf",            1, NULL, 'e' },
  { NULL,             0, NULL,  0  },
};

void Emulator::print_help(const char *file) {
  printf("Usage: %s [OPTION...]\n", file);
  printf("\n");
  printf("  -s, --seed=NUM        use this seed\n");
  printf("  -C, --max-cycles=NUM  execute at most NUM cycles\n");
  printf("  -u, --uImage=FILE     specify uImage file\n");
  printf("  -h, --help            print program help info\n");
  printf("\n");
  printf("NEMU-Mips32 options\n");
  printf("  -S, --symbol=FILE     use this file to produce symbols\n");
  printf("  -u, --uImage=FILE     specify uImage file\n");
  printf("  -b, --batch           run on batch mode (default)\n");
  printf("  -i, --image=FILE      run with this image file\n");
  printf("  -e, --elf=FILE        run with this elf file\n");
  printf("\n");
  printf("Report bugs to 141242068@smail.nju.edu.cn.\n");
}

std::vector<const char *> Emulator::parse_args(int argc, const char *argv[]) {
  std::vector<const char *> args = { argv[0] };
  int o;
  while ( (o = getopt_long(argc, const_cast<char *const*>(argv), "-s:C:u:hS:bi:e:", long_options, NULL)) != -1) {
    switch (o) {
      case 's': 
        if(std::string(optarg) != "NO_SEED")
          seed = atoll(optarg);
        break;
      case 'C': max_cycles = atoll(optarg);  break;
      case 'u': bram_image = optarg;
                args.push_back("-u");
                args.push_back(optarg);
                break;
                /* store for nemu */
      case 'S':
                args.push_back("-S");
                args.push_back(optarg);
                break;
      case 'i':
                args.push_back("-i");
                args.push_back(optarg);
                break;
      case 'e':
                args.push_back("-e");
                args.push_back(optarg);
                break;
      default:
                print_help(argv[0]);
                exit(0);
    }
  }

  args.push_back("-b");
  return args; // optimized by rvo
}

int main(int argc, const char** argv) {
  auto emu = Emulator(argc, argv);

  get_sc_time_stamp = [&emu]() -> double {
    return emu.get_cycles();
  };

  auto ret = emu.execute();

  if (ret == STATE_RUNNING) {
    eprintf(ANSI_COLOR_RED "Timeout after %lld cycles\n" ANSI_COLOR_RESET, (long long)emu.get_max_cycles());
  } else if (ret == STATE_GOODTRAP) {
    eprintf(ANSI_COLOR_GREEN "HIT GOOD TRAP\n" ANSI_COLOR_RESET);
  } else {
    eprintf(ANSI_COLOR_RED "HIT BAD TRAP code: %d\n" ANSI_COLOR_RESET, ret);
  }

  return ret;
}
