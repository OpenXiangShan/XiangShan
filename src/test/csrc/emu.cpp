#include "emu.h"
#include "sdcard.h"
#include "difftest.h"
#include <getopt.h>
#include "ram.h"

void* get_ram_start();
long get_ram_size();
uint64_t get_nemu_this_pc();
void set_nemu_this_pc(uint64_t pc);


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

inline EmuArgs parse_args(int argc, const char *argv[]) {
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

  Verilated::commandArgs(argc, argv); // Prepare extra args for TLMonitor
  return args;
}


Emulator::Emulator(int argc, const char *argv[]):
  dut_ptr(new VXSSimSoC),
  cycles(0), hascommit(0), trapCode(STATE_RUNNING)
{
  args = parse_args(argc, argv);
  printf("Emu compiled at %s, %s UTC\n", __DATE__, __TIME__);

  // srand
  srand(args.seed);
  srand48(args.seed);
  Verilated::randReset(2);

  // init core
  reset_ncycles(10);

  // init ram
  init_ram(args.image);

  // init device
  extern void init_device(void);
  init_device();

  init_difftest();

#if VM_TRACE == 1
  enable_waveform = args.enable_waveform;
  if (enable_waveform) {
    Verilated::traceEverOn(true);	// Verilator must compute traced signals
    tfp = new VerilatedVcdC;
    dut_ptr->trace(tfp, 99);	// Trace 99 levels of hierarchy
    time_t now = time(NULL);
    tfp->open(waveform_filename(now));	// Open the dump file
  }
#else
  enable_waveform = false;
#endif

#ifdef VM_SAVABLE
  if (args.snapshot_path != NULL) {
    printf("loading from snapshot `%s`...\n", args.snapshot_path);
    snapshot_load(args.snapshot_path);
    printf("model cycleCnt = %" PRIu64 "\n", dut_ptr->io_trap_cycleCnt);
    hascommit = 1;
  }
#endif

  // set log time range and log level
  dut_ptr->io_logCtrl_log_begin = args.log_begin;
  dut_ptr->io_logCtrl_log_end = args.log_end;
}

Emulator::~Emulator() {
#ifdef WITH_DRAMSIM3
  dramsim3_finish();
#endif
#ifdef VM_SAVABLE
  snapshot_slot[0].save();
  snapshot_slot[1].save();
  printf("Please remove unused snapshots manually\n");
#endif
}

inline void Emulator::read_emu_regs(uint64_t *r) {
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
  r[DIFFTEST_SATP   ] = dut_ptr->io_difftest_satp;
  r[DIFFTEST_MIP    ] = dut_ptr->io_difftest_mip;
  r[DIFFTEST_MIE    ] = dut_ptr->io_difftest_mie;
  r[DIFFTEST_MSCRATCH]= dut_ptr->io_difftest_mscratch;
  r[DIFFTEST_SSCRATCH]= dut_ptr->io_difftest_sscratch;
  r[DIFFTEST_MIDELEG] = dut_ptr->io_difftest_mideleg;
  r[DIFFTEST_MEDELEG] = dut_ptr->io_difftest_medeleg;
  r[DIFFTEST_MTVAL]   = dut_ptr->io_difftest_mtval;
  r[DIFFTEST_STVAL]   = dut_ptr->io_difftest_stval;
  r[DIFFTEST_MTVEC]   = dut_ptr->io_difftest_mtvec;
  r[DIFFTEST_STVEC]   = dut_ptr->io_difftest_stvec;
  r[DIFFTEST_MODE]    = dut_ptr->io_difftest_priviledgeMode;
}

inline void Emulator::read_wb_info(uint64_t *wpc, uint64_t *wdata, uint32_t *wdst) {
#define dut_ptr_wpc(x)  wpc[x] = dut_ptr->io_difftest_wpc_##x
#define dut_ptr_wdata(x) wdata[x] = dut_ptr->io_difftest_wdata_##x
#define dut_ptr_wdst(x)  wdst[x] = dut_ptr->io_difftest_wdst_##x
  dut_ptr_wpc(0); dut_ptr_wdata(0); dut_ptr_wdst(0); 
  dut_ptr_wpc(1); dut_ptr_wdata(1); dut_ptr_wdst(1); 
  dut_ptr_wpc(2); dut_ptr_wdata(2); dut_ptr_wdst(2); 
  dut_ptr_wpc(3); dut_ptr_wdata(3); dut_ptr_wdst(3); 
  dut_ptr_wpc(4); dut_ptr_wdata(4); dut_ptr_wdst(4); 
  dut_ptr_wpc(5); dut_ptr_wdata(5); dut_ptr_wdst(5); 
}

inline void Emulator::reset_ncycles(size_t cycles) {
  for(int i = 0; i < cycles; i++) {
    dut_ptr->reset = 1;
    dut_ptr->clock = 0;
    dut_ptr->eval();
    dut_ptr->clock = 1;
    dut_ptr->eval();
    dut_ptr->reset = 0;
  }
}

inline void Emulator::single_cycle() {
  dut_ptr->clock = 0;
#ifdef WITH_DRAMSIM3
  axi_channel axi;
  axi_copy_from_dut_ptr(dut_ptr, axi);
  dramsim3_helper(axi);
  axi_set_dut_ptr(dut_ptr, axi);
#endif

  dut_ptr->eval();

  dut_ptr->clock = 1;
  dut_ptr->eval();

#if VM_TRACE == 1
  if (enable_waveform) {
    uint64_t cycle = dut_ptr->io_trap_cycleCnt;
    uint64_t begin = dut_ptr->io_logCtrl_log_begin;
    uint64_t end   = dut_ptr->io_logCtrl_log_end;
    bool in_range = (begin <= cycle) && (cycle <= end);
    if (in_range) { tfp->dump(cycle); }
  }
#endif

  if (dut_ptr->io_uart_out_valid) {
    printf("%c", dut_ptr->io_uart_out_ch);
    fflush(stdout);
  }
  if (dut_ptr->io_uart_in_valid) {
    extern uint8_t uart_getc();
    dut_ptr->io_uart_in_ch = uart_getc();
  }

  cycles ++;
}

uint64_t Emulator::execute(uint64_t n) {
  extern void poll_event(void);
  extern uint32_t uptime(void);
  uint32_t lasttime_poll = 0;
  uint32_t lasttime_snapshot = 0;
  uint64_t lastcommit = n;
  const int stuck_limit = 2000;

  uint32_t wdst[DIFFTEST_WIDTH];
  uint64_t wdata[DIFFTEST_WIDTH];
  uint64_t wpc[DIFFTEST_WIDTH];
  uint64_t reg[DIFFTEST_NR_REG];
  DiffState diff;
  diff.reg_scala = reg;
  diff.wpc = wpc;
  diff.wdata = wdata;
  diff.wdst = wdst;

  while (trapCode == STATE_RUNNING && n > 0) {
    single_cycle();
    n --;

    if (dut_ptr->io_trap_valid) trapCode = dut_ptr->io_trap_code;
    if (trapCode != STATE_RUNNING) break;

    if (lastcommit - n > stuck_limit && hascommit) {
      eprintf("No instruction commits for %d cycles, maybe get stuck\n"
          "(please also check whether a fence.i instruction requires more than %d cycles to flush the icache)\n",
          stuck_limit, stuck_limit);
      difftest_display(dut_ptr->io_difftest_priviledgeMode);
      trapCode = STATE_ABORT;
    }

    if (!hascommit && dut_ptr->io_difftest_commit && dut_ptr->io_difftest_thisPC == 0x80000000u) {
      hascommit = 1;
      read_emu_regs(reg);
      void* get_img_start();
      long get_img_size();
      ref_difftest_memcpy_from_dut(0x80000000, get_img_start(), get_img_size());
      ref_difftest_setregs(reg);
    }

    // difftest
    if (dut_ptr->io_difftest_commit && hascommit) {
      read_emu_regs(reg);
      read_wb_info(wpc, wdata, wdst);

      diff.commit = dut_ptr->io_difftest_commit;
      diff.this_inst = dut_ptr->io_difftest_thisINST;
      diff.skip = dut_ptr->io_difftest_skip;
      diff.isRVC = dut_ptr->io_difftest_isRVC;
      diff.wen = dut_ptr->io_difftest_wen;
      diff.intrNO = dut_ptr->io_difftest_intrNO;
      diff.cause = dut_ptr->io_difftest_cause;
      diff.priviledgeMode = dut_ptr->io_difftest_priviledgeMode;

      diff.sync.scFailed = dut_ptr->io_difftest_scFailed;

      if (difftest_step(&diff)) {
        trapCode = STATE_ABORT;
      }
      lastcommit = n;
    }

    uint32_t t = uptime();
    if (t - lasttime_poll > 100) {
      poll_event();
      lasttime_poll = t;
    }
#ifdef VM_SAVABLE
    static int snapshot_count = 0;
    if (trapCode != STATE_GOODTRAP && t - lasttime_snapshot > 1000 * SNAPSHOT_INTERVAL) {
      // save snapshot every 10s
      time_t now = time(NULL);
      snapshot_save(snapshot_filename(now));
      lasttime_snapshot = t;
      // dump snapshot to file every 10 minutes
      snapshot_count++;
      if (snapshot_count == 60) {
        snapshot_slot[0].save();
        snapshot_count = 0;
      }
    }
#endif
  }

#if VM_TRACE == 1
  if (enable_waveform) tfp->close();
#endif
  display_trapinfo();
  return cycles;
}

inline char* Emulator::timestamp_filename(time_t t, char *buf) {
  char buf_time[64];
  strftime(buf_time, sizeof(buf_time), "%F@%T", localtime(&t));
  char *noop_home = getenv("NOOP_HOME");
  assert(noop_home != NULL);
  int len = snprintf(buf, 1024, "%s/build/%s", noop_home, buf_time);
  return buf + len;
}

#ifdef VM_SAVABLE
inline char* Emulator::snapshot_filename(time_t t) {
  static char buf[1024];
  char *p = timestamp_filename(t, buf);
  strcpy(p, ".snapshot");
  return buf;
}
#endif

inline char* Emulator::waveform_filename(time_t t) {
  static char buf[1024];
  char *p = timestamp_filename(t, buf);
  strcpy(p, ".vcd");
  return buf;
}

void Emulator::display_trapinfo() {
  uint64_t pc = dut_ptr->io_trap_pc;
  uint64_t instrCnt = dut_ptr->io_trap_instrCnt;
  uint64_t cycleCnt = dut_ptr->io_trap_cycleCnt;

  switch (trapCode) {
    case STATE_GOODTRAP:
      eprintf(ANSI_COLOR_GREEN "HIT GOOD TRAP at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, pc);
      break;
    case STATE_BADTRAP:
      eprintf(ANSI_COLOR_RED "HIT BAD TRAP at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, pc);
      break;
    case STATE_ABORT:
      eprintf(ANSI_COLOR_RED "ABORT at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, pc);
      break;
    default:
      eprintf(ANSI_COLOR_RED "Unknown trap code: %d\n", trapCode);
  }

  double ipc = (double)instrCnt / (cycleCnt-500);
  eprintf(ANSI_COLOR_MAGENTA "total guest instructions = %" PRIu64 "\n" ANSI_COLOR_RESET, instrCnt);
  eprintf(ANSI_COLOR_MAGENTA "instrCnt = %" PRIu64 ", cycleCnt = %" PRIu64 ", IPC = %lf\n" ANSI_COLOR_RESET,
      instrCnt, cycleCnt, ipc);
}

#ifdef VM_SAVABLE
void Emulator::snapshot_save(const char *filename) {
  static int last_slot = 0;
  VerilatedSaveMem &stream = snapshot_slot[last_slot];
  last_slot = !last_slot;

  stream.init(filename);
  stream << *dut_ptr;
  stream.flush();

  long size = get_ram_size();
  stream.unbuf_write(&size, sizeof(size));
  stream.unbuf_write(get_ram_start(), size);

  uint64_t ref_r[DIFFTEST_NR_REG];
  ref_difftest_getregs(&ref_r);
  stream.unbuf_write(ref_r, sizeof(ref_r));

  uint64_t nemu_this_pc = get_nemu_this_pc();
  stream.unbuf_write(&nemu_this_pc, sizeof(nemu_this_pc));

  char *buf = new char[size];
  ref_difftest_memcpy_from_ref(buf, 0x80000000, size);
  stream.unbuf_write(buf, size);
  delete buf;

  struct SyncState sync_mastate;
  ref_difftest_get_mastatus(&sync_mastate);
  stream.unbuf_write(&sync_mastate, sizeof(struct SyncState));

  uint64_t csr_buf[4096];
  ref_difftest_get_csr(csr_buf);
  stream.unbuf_write(&csr_buf, sizeof(csr_buf));

  long sdcard_offset;
  if(fp)
    sdcard_offset = ftell(fp);
  else
    sdcard_offset = 0;
  stream.unbuf_write(&sdcard_offset, sizeof(sdcard_offset));

  // actually write to file in snapshot_finalize()
}

void Emulator::snapshot_load(const char *filename) {
  VerilatedRestore stream;
  stream.open(filename);
  stream >> *dut_ptr;

  long size;
  stream.read(&size, sizeof(size));
  assert(size == get_ram_size());
  stream.read(get_ram_start(), size);

  uint64_t ref_r[DIFFTEST_NR_REG];
  stream.read(ref_r, sizeof(ref_r));
  ref_difftest_setregs(&ref_r);

  uint64_t nemu_this_pc;
  stream.read(&nemu_this_pc, sizeof(nemu_this_pc));
  set_nemu_this_pc(nemu_this_pc);

  char *buf = new char[size];
  stream.read(buf, size);
  ref_difftest_memcpy_from_dut(0x80000000, buf, size);
  delete buf;

  struct SyncState sync_mastate;
  stream.read(&sync_mastate, sizeof(struct SyncState));
  ref_difftest_set_mastatus(&sync_mastate);

  uint64_t csr_buf[4096];
  stream.read(&csr_buf, sizeof(csr_buf));
  ref_difftest_set_csr(csr_buf);

  long sdcard_offset = 0;
  stream.read(&sdcard_offset, sizeof(sdcard_offset));
  if(fp)
    fseek(fp, sdcard_offset, SEEK_SET);
}
#endif
