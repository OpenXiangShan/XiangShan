#include "emu.h"
#include "sdcard.h"
#include "difftest.h"
#include <getopt.h>
#include <signal.h>
#include <unistd.h>
#include "ram.h"
#include "zlib.h"
#include "compress.h"

static inline void print_help(const char *file) {
  printf("Usage: %s [OPTION...]\n", file);
  printf("\n");
  printf("  -s, --seed=NUM             use this seed\n");
  printf("  -C, --max-cycles=NUM       execute at most NUM cycles\n");
  printf("  -I, --max-instr=NUM        execute at most NUM instructions\n");
  printf("  -i, --image=FILE           run with this image file\n");
  printf("  -b, --log-begin=NUM        display log from NUM th cycle\n");
  printf("  -e, --log-end=NUM          stop display log at NUM th cycle\n");
  printf("      --load-snapshot=PATH   load snapshot from PATH\n");
  printf("      --no-snapshot          disable saving snapshots\n");
  printf("      --dump-wave            dump waveform when log is enabled\n");
  printf("  -h, --help                 print program help info\n");
  printf("\n");
}

inline EmuArgs parse_args(int argc, const char *argv[]) {
  EmuArgs args;
  int long_index = 0;
  const struct option long_options[] = {
    { "load-snapshot",  1, NULL,  0  },
    { "dump-wave",      0, NULL,  0  },
    { "no-snapshot",    0, NULL,  0  },
    { "seed",           1, NULL, 's' },
    { "max-cycles",     1, NULL, 'C' },
    { "max-instr",      1, NULL, 'I' },
    { "image",          1, NULL, 'i' },
    { "log-begin",      1, NULL, 'b' },
    { "log-end",        1, NULL, 'e' },
    { "help",           0, NULL, 'h' },
    { 0,                0, NULL,  0  }
  };

  int o;
  while ( (o = getopt_long(argc, const_cast<char *const*>(argv),
          "-s:C:I:hi:m:b:e:", long_options, &long_index)) != -1) {
    switch (o) {
      case 0:
        switch (long_index) {
          case 0: args.snapshot_path = optarg; continue;
          case 1: args.enable_waveform = true; continue;
          case 2: args.enable_snapshot = false; continue;
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
      case 'I': args.max_instr = atoll(optarg);  break;
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

  // srand
  srand(args.seed);
  srand48(args.seed);
  Verilated::randReset(2);
  assert_init();

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
  ram_finish();
  assert_finish();

#ifdef VM_SAVABLE
  if (args.enable_snapshot && trapCode != STATE_GOODTRAP && trapCode != STATE_LIMIT_EXCEEDED) {
    printf("Saving snapshots to file system. Please wait.\n");
    snapshot_slot[0].save();
    snapshot_slot[1].save();
    printf("Please remove unused snapshots manually\n");
  }
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
#define dut_ptr_read_wb(x) dut_ptr_wpc(x); dut_ptr_wdata(x); dut_ptr_wdst(x);

#if DIFFTEST_WIDTH >= 13 || DIFFTEST_WIDTH < 6
#error "not supported difftest width"
#endif

  dut_ptr_read_wb(0);
  dut_ptr_read_wb(1);
  dut_ptr_read_wb(2);
  dut_ptr_read_wb(3);
  dut_ptr_read_wb(4);
  dut_ptr_read_wb(5);
#if DIFFTEST_WIDTH >= 7
  dut_ptr_read_wb(6);
#endif
#if DIFFTEST_WIDTH >= 8
  dut_ptr_read_wb(7);
#endif
#if DIFFTEST_WIDTH >= 9
  dut_ptr_read_wb(8);
#endif
#if DIFFTEST_WIDTH >= 10
  dut_ptr_read_wb(9);
#endif
#if DIFFTEST_WIDTH >= 11
  dut_ptr_read_wb(10);
#endif
#if DIFFTEST_WIDTH >= 12
  dut_ptr_read_wb(11);
#endif
}

inline void Emulator::read_store_info(uint64_t *saddr, uint64_t *sdata, uint8_t *smask) {
#define dut_ptr_saddr(x)  saddr[x] = dut_ptr->io_difftest_storeAddr_##x
#define dut_ptr_sdata(x) sdata[x] = dut_ptr->io_difftest_storeData_##x
#define dut_ptr_smask(x) smask[x] = dut_ptr->io_difftest_storeMask_##x
#define dut_ptr_read_store(x) dut_ptr_saddr(x); dut_ptr_sdata(x); dut_ptr_smask(x);
  dut_ptr_read_store(0);
  dut_ptr_read_store(1);
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
  dut_ptr->eval();

#ifdef WITH_DRAMSIM3
  axi_channel axi;
  axi_copy_from_dut_ptr(dut_ptr, axi);
  axi.aw.addr -= 0x80000000UL;
  axi.ar.addr -= 0x80000000UL;
  dramsim3_helper_rising(axi);
#endif

  dut_ptr->clock = 1;
  dut_ptr->eval();

#ifdef WITH_DRAMSIM3
  axi_copy_from_dut_ptr(dut_ptr, axi);
  axi.aw.addr -= 0x80000000UL;
  axi.ar.addr -= 0x80000000UL;
  dramsim3_helper_falling(axi);
  axi_set_dut_ptr(dut_ptr, axi);
#endif

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

uint64_t Emulator::execute(uint64_t max_cycle, uint64_t max_instr) {
  extern void poll_event(void);
  extern uint32_t uptime(void);
  uint32_t lasttime_poll = 0;
  uint32_t lasttime_snapshot = 0;
  uint64_t lastcommit[NumCore];
  uint64_t instr_left_last_cycle[NumCore];
  const int stuck_limit = 2000;
  uint64_t core_max_instr[NumCore];

  uint32_t wdst[NumCore][DIFFTEST_WIDTH];
  uint64_t wdata[NumCore][DIFFTEST_WIDTH];
  uint64_t wpc[NumCore][DIFFTEST_WIDTH];
  uint64_t reg[NumCore][DIFFTEST_NR_REG];
  DiffState diff[NumCore];
  for (int i = 0; i < NumCore; i++) {
    diff[i].reg_scala = reg[i];
    diff[i].wpc = wpc[i];
    diff[i].wdata = wdata[i];
    diff[i].wdst = wdst[i];
    lastcommit[i] = max_cycle;
    instr_left_last_cycle[i] = max_cycle;
    core_max_instr[i] = max_instr;
  }


#if VM_COVERAGE == 1
  // we dump coverage into files at the end
  // since we are not sure when an emu will stop
  // we distinguish multiple dat files by emu start time
  time_t coverage_start_time = time(NULL);
#endif

  while (!Verilated::gotFinish() && trapCode == STATE_RUNNING) {
    if (!(max_cycle > 0 && 
          core_max_instr[0] > 0 && 
          instr_left_last_cycle[0] >= core_max_instr[0])) {
      trapCode = STATE_LIMIT_EXCEEDED;  /* handle overflow */
      break;
    }
    if (assert_count > 0) {
      difftest_display(dut_ptr->io_difftest_priviledgeMode);
      eprintf("The simulation stopped. There might be some assertion failed.\n");
      trapCode = STATE_ABORT;
      break;
    }
    if (signal_num != 0) {
      trapCode = STATE_SIG;
      break;
    }

    single_cycle();
    max_cycle --;

    if (dut_ptr->io_trap_valid) trapCode = dut_ptr->io_trap_code;
    if (trapCode != STATE_RUNNING) break;

    if (lastcommit[0] - max_cycle > stuck_limit && hascommit) {
      eprintf("No instruction commits for %d cycles, maybe get stuck\n"
          "(please also check whether a fence.i instruction requires more than %d cycles to flush the icache)\n",
          stuck_limit, stuck_limit);
      difftest_display(dut_ptr->io_difftest_priviledgeMode);
      trapCode = STATE_ABORT;
    }

    if (!hascommit && dut_ptr->io_difftest_commit && dut_ptr->io_difftest_thisPC == 0x80000000u) {
      hascommit = 1;
      read_emu_regs(reg[0]);
      void* get_img_start();
      long get_img_size();
      ref_difftest_memcpy_from_dut(0x80000000, get_img_start(), get_img_size(), 0);
      ref_difftest_setregs(reg[0], 0);
      printf("The first instruction has commited. Difftest enabled. \n");
    }

    // difftest

    for (int i = 0; i < NumCore; i++) {
      if (dut_ptr->io_difftest_commit && hascommit) {
        read_emu_regs(reg[i]);
        read_wb_info(wpc[i], wdata[i], wdst[i]);

        diff[i].commit = dut_ptr->io_difftest_commit;
        diff[i].this_inst = dut_ptr->io_difftest_thisINST;
        diff[i].skip = dut_ptr->io_difftest_skip;
        diff[i].isRVC = dut_ptr->io_difftest_isRVC;
        diff[i].wen = dut_ptr->io_difftest_wen;
        diff[i].intrNO = dut_ptr->io_difftest_intrNO;
        diff[i].cause = dut_ptr->io_difftest_cause;
        diff[i].priviledgeMode = dut_ptr->io_difftest_priviledgeMode;

        diff[i].sync.scFailed = dut_ptr->io_difftest_scFailed;

        if (i == 0) {
          if (difftest_step(&diff[i])) {
            trapCode = STATE_ABORT;
          }
        }
        lastcommit[i] = max_cycle;

        // update instr_cnt
        instr_left_last_cycle[i] = core_max_instr[i];
        core_max_instr[i] -= diff[i].commit;
      }

#ifdef DIFFTEST_STORE_COMMIT
      for (int core = 0; core < NumCore; core++) {
        if (dut_ptr->io_difftest_storeCommit) {
          read_store_info(diff[core].store_addr, diff[core].store_data, diff[core].store_mask);

          for (int i = 0; i < dut_ptr->io_difftest_storeCommit; i++) {
            auto addr = diff[core].store_addr[i];
            auto data = diff[core].store_data[i];
            auto mask = diff[core].store_mask[i];
            if (difftest_store_step(&addr, &data, &mask)) {
              difftest_display(dut_ptr->io_difftest_priviledgeMode);
              printf("Mismatch for store commits: \n");
              printf("REF commits addr 0x%lx, data 0x%lx, mask 0x%x\n", addr, data, mask);
              printf("DUT commits addr 0x%lx, data 0x%lx, mask 0x%x\n",
                diff[core].store_addr[i], diff[core].store_data[i], diff[core].store_mask[i]);
              trapCode = STATE_ABORT;
              break;
            }
          }
        }
      }
#endif
    }

    uint32_t t = uptime();
    if (t - lasttime_poll > 100) {
      poll_event();
      lasttime_poll = t;
    }
#ifdef VM_SAVABLE
    static int snapshot_count = 0;
    if (args.enable_snapshot && trapCode != STATE_GOODTRAP && t - lasttime_snapshot > 1000 * SNAPSHOT_INTERVAL) {
      // save snapshot every 60s
      time_t now = time(NULL);
      snapshot_save(snapshot_filename(now));
      lasttime_snapshot = t;
      // dump one snapshot to file every 60 snapshots
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

#if VM_COVERAGE == 1
  save_coverage(coverage_start_time);
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
  printf("dump wave to %s...\n", buf);
  return buf;
}


#if VM_COVERAGE == 1
inline char* Emulator::coverage_filename(time_t t) {
  static char buf[1024];
  char *p = timestamp_filename(t, buf);
  strcpy(p, ".coverage.dat");
  return buf;
}

inline void Emulator::save_coverage(time_t t) {
  char *p = coverage_filename(t);
  VerilatedCov::write(p);
}
#endif


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
    case STATE_LIMIT_EXCEEDED:
      eprintf(ANSI_COLOR_YELLOW "EXCEEDING CYCLE/INSTR LIMIT at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, pc);
      break;
    case STATE_SIG:
      eprintf(ANSI_COLOR_YELLOW "SOME SIGNAL STOPS THE PROGRAM at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, pc);
      break;
    default:
      eprintf(ANSI_COLOR_RED "Unknown trap code: %d\n", trapCode);
  }

  double ipc = (double)instrCnt / (cycleCnt-500);
  eprintf(ANSI_COLOR_MAGENTA "total guest instructions = %'" PRIu64 "\n" ANSI_COLOR_RESET, instrCnt);
  eprintf(ANSI_COLOR_MAGENTA "instrCnt = %'" PRIu64 ", cycleCnt = %'" PRIu64 ", IPC = %lf\n" ANSI_COLOR_RESET,
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
  ref_difftest_getregs(&ref_r, 0);
  stream.unbuf_write(ref_r, sizeof(ref_r));

  uint64_t nemu_this_pc = get_nemu_this_pc();
  stream.unbuf_write(&nemu_this_pc, sizeof(nemu_this_pc));

  char *buf = (char *)mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  ref_difftest_memcpy_from_ref(buf, 0x80000000, size, 0);
  stream.unbuf_write(buf, size);
  munmap(buf, size);

  struct SyncState sync_mastate;
  ref_difftest_get_mastatus(&sync_mastate, 0);
  stream.unbuf_write(&sync_mastate, sizeof(struct SyncState));

  uint64_t csr_buf[4096];
  ref_difftest_get_csr(csr_buf, 0);
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
  VerilatedRestoreMem stream;
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

  char *buf = (char *)mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  stream.read(buf, size);
  ref_difftest_memcpy_from_dut(0x80000000, buf, size, 0);
  munmap(buf, size);

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
