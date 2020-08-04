#include "emu.h"
#include <getopt.h>
#include <functional>

static char mybuf[BUFSIZ];

// junk, link for verilator
std::function<double()> get_sc_time_stamp = []() -> double { return 0; };
double sc_time_stamp() { return get_sc_time_stamp(); }

static inline void print_help(const char *file) {
  printf("Usage: %s [OPTION...]\n", file);
  printf("\n");
  printf("  -s, --seed=NUM        use this seed\n");
  printf("  -C, --max-cycles=NUM  execute at most NUM cycles\n");
  printf("  -i, --image=FILE      run with this image file\n");
  printf("  -b, --log-begin=NUM   display log from NUM th cycle\n");
  printf("  -e, --log-end=NUM     stop display log at NUM th cycle\n");
  printf("      --load-snapshot=PATH   load snapshot from PATH\n");
  printf("      --dump-wave       dump waveform when log is enabled\n");
  printf("  -h, --help            print program help info\n");
  printf("\n");
}

static inline EmuArgs parse_args(int argc, const char *argv[]) {
  EmuArgs args;
  int long_index = 0;
  const struct option long_options[] = {
    { "load-snapshot",  1, NULL,  0  },
    { "dump-wave",      0, NULL,  0  },
    { "seed",           1, NULL, 's' },
    { "max-cycles",     1, NULL, 'C' },
    { "image",          1, NULL, 'i' },
    { "log-begin",      1, NULL, 'b' },
    { "log-end",        1, NULL, 'e' },
    { "help",           0, NULL, 'h' },
    { 0,                0, NULL,  0  }
  };

  int o;
  while ( (o = getopt_long(argc, const_cast<char *const*>(argv),
          "-s:C:hi:m:b:e:", long_options, &long_index)) != -1) {
    switch (o) {
      case 0:
        switch (long_index) {
          case 0: args.snapshot_path = optarg; continue;
          case 1: args.enable_waveform = true; continue;
        }
        // fall through
      default:
        print_help(argv[0]);
        exit(0);
      case 's': 
        if(std::string(optarg) != "NO_SEED") {
          args.seed = atoll(optarg);
          printf("Using seed = %d\n", args.seed);
        }
        break;
      case 'C': args.max_cycles = atoll(optarg);  break;
      case 'i': args.image = optarg; break;
      case 'b': args.log_begin = atoll(optarg);  break;
      case 'e': args.log_end = atoll(optarg); break;
    }
  }

  return args;
}

int main(int argc, const char** argv) {
  setbuf(stderr, mybuf);

  auto args = parse_args(argc, argv);
  auto emu = new Emulator(args);

  get_sc_time_stamp = [&emu]() -> double {
    return emu->get_cycles();
  };

  uint64_t cycles = emu->execute(args.max_cycles);
  bool is_good_trap = emu->is_good_trap();
  delete emu;

  extern uint32_t uptime(void);
  uint32_t ms = uptime();

  eprintf(ANSI_COLOR_BLUE "Seed=%d Guest cycle spent: %" PRIu64
      " (this will be different from cycleCnt if emu loads a snapshot)\n" ANSI_COLOR_RESET, args.seed, cycles);
  eprintf(ANSI_COLOR_BLUE "Host time spent: %dms\n" ANSI_COLOR_RESET, ms);

  return !is_good_trap;
}
