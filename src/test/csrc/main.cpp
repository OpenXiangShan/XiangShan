#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <memory>
#include <getopt.h>
#include <string.h>
#include <sys/time.h>
#include <iomanip>
#include <fstream>
#include <functional>
#include <inttypes.h>

#include "emu.h"

// junk, link for verilator
std::function<double()> get_sc_time_stamp = []() -> double { return 0; };
double sc_time_stamp() { return get_sc_time_stamp(); }

const struct option Emulator::long_options[] = {
  { "seed",           1, NULL, 's' },
  { "max-cycles",     1, NULL, 'C' },
  { "image",          1, NULL, 'i' },
  { "log-begin",      1, NULL, 'b' },
  { "log-end",        1, NULL, 'e' },
  { "verbose",        1, NULL, 'v' },
  { "help",           0, NULL, 'h' },
  { 0,                0, NULL,  0  }
};

void Emulator::print_help(const char *file) {
  printf("Usage: %s [OPTION...]\n", file);
  printf("\n");
  printf("  -s, --seed=NUM        use this seed\n");
  printf("  -C, --max-cycles=NUM  execute at most NUM cycles\n");
  printf("  -i, --image=FILE      run with this image file\n");
  printf("  -b, --log-begin=NUM   display log from NUM th cycle\n");
  printf("  -e, --log-end=NUM     stop display log at NUM th cycle\n");
  printf("  -v, --verbose=STR     verbosity level, can be one of [ALL, DEBUG, INFO, WARN, ERROR]\n");
  printf("  -h, --help            print program help info\n");
  printf("\n");
}

std::vector<const char *> Emulator::parse_args(int argc, const char *argv[]) {
  std::vector<const char *> args = { argv[0] };
  int o;
  while ( (o = getopt_long(argc, const_cast<char *const*>(argv), "-s:C:hi:m:b:e:v:", long_options, NULL)) != -1) {
    switch (o) {
      case 's': 
        if(std::string(optarg) != "NO_SEED") {
          seed = atoll(optarg);
          printf("Using seed = %d\n", seed);
        }
        break;
      case 'C': max_cycles = atoll(optarg);  break;
      case 'i': image = optarg;
                args.push_back("-i");
                args.push_back(optarg);
                break;
      case 'b': log_begin = atoll(optarg);  break;
      case 'e': log_end = atoll(optarg); break;
      case 'v': log_level = getLogLevel(optarg); break;
      default:
                print_help(argv[0]);
                exit(0);
    }
  }

  return args; // optimized by rvo
}

int main(int argc, const char** argv) {
  auto emu = Emulator(argc, argv);

  get_sc_time_stamp = [&emu]() -> double {
    return emu.get_cycles();
  };

  emu.execute();

  extern uint32_t uptime(void);
  uint32_t ms = uptime();

  int display_trapinfo(uint64_t max_cycles);
  int ret = display_trapinfo(emu.get_max_cycles());
  eprintf(ANSI_COLOR_BLUE "Guest cycle spent: %" PRIu64 "\n" ANSI_COLOR_RESET, emu.get_cycles());
  eprintf(ANSI_COLOR_BLUE "Host time spent: %dms\n" ANSI_COLOR_RESET, ms);

  return ret;
}
