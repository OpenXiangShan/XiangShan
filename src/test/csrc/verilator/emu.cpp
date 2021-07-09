/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

#include "emu.h"
#include "device.h"
#include "sdcard.h"
#include "difftest.h"
#include "nemuproxy.h"
#include "goldenmem.h"
#include "device.h"
#include <getopt.h>
#include <signal.h>
#include <unistd.h>
#include "ram.h"
#include "zlib.h"
#include "compress.h"
#include <list>

static inline void print_help(const char *file) {
  printf("Usage: %s [OPTION...]\n", file);
  printf("\n");
  printf("  -s, --seed=NUM             use this seed\n");
  printf("  -C, --max-cycles=NUM       execute at most NUM cycles\n");
  printf("  -I, --max-instr=NUM        execute at most NUM instructions\n");
  printf("  -W, --warmup-instr=NUM     the number of warmup instructions\n");
  printf("  -D, --stat-cycles=NUM      the interval cycles of dumping statistics\n");
  printf("  -i, --image=FILE           run with this image file\n");
  printf("  -b, --log-begin=NUM        display log from NUM th cycle\n");
  printf("  -e, --log-end=NUM          stop display log at NUM th cycle\n");
  printf("      --force-dump-result    force dump performance counter result in the end\n");
  printf("      --load-snapshot=PATH   load snapshot from PATH\n");
  printf("      --no-snapshot          disable saving snapshots\n");
  printf("      --dump-wave            dump waveform when log is enabled\n");
  printf("      --no-diff              disable differential testing\n");
  printf("      --diff=PATH            set the path of REF for differential testing\n");
  printf("  -h, --help                 print program help info\n");
  printf("\n");
}

inline EmuArgs parse_args(int argc, const char *argv[]) {
  EmuArgs args;
  int long_index = 0;
  extern const char *difftest_ref_so;
  const struct option long_options[] = {
    { "load-snapshot",     1, NULL,  0  },
    { "dump-wave",         0, NULL,  0  },
    { "no-snapshot",       0, NULL,  0  },
    { "force-dump-result", 0, NULL,  0  },
    { "diff",              1, NULL,  0  },
    { "no-diff",           0, NULL,  0  },
    { "seed",              1, NULL, 's' },
    { "max-cycles",        1, NULL, 'C' },
    { "max-instr",         1, NULL, 'I' },
    { "warmup-instr",      1, NULL, 'W' },
    { "stat-cycles",       1, NULL, 'D' },
    { "image",             1, NULL, 'i' },
    { "log-begin",         1, NULL, 'b' },
    { "log-end",           1, NULL, 'e' },
    { "help",              0, NULL, 'h' },
    { 0,                   0, NULL,  0  }
  };

  int o;
  while ( (o = getopt_long(argc, const_cast<char *const*>(argv),
          "-s:C:I:W:hi:m:b:e:", long_options, &long_index)) != -1) {
    switch (o) {
      case 0:
        switch (long_index) {
          case 0: args.snapshot_path = optarg; continue;
          case 1: args.enable_waveform = true; continue;
          case 2: args.enable_snapshot = false; continue;
          case 3: args.force_dump_result = true; continue;
          case 4: difftest_ref_so = optarg; continue;
          case 5: args.enable_diff = false; continue;
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
      case 'W': args.warmup_instr = atoll(optarg);  break;
      case 'D': args.stat_cycles = atoll(optarg);  break;
      case 'i': args.image = optarg; break;
      case 'b': args.log_begin = atoll(optarg);  break;
      case 'e': args.log_end = atoll(optarg); break;
    }
  }

  Verilated::commandArgs(argc, argv); // Prepare extra args for TLMonitor
  return args;
}


Emulator::Emulator(int argc, const char *argv[]):
  dut_ptr(new VSimTop),
  cycles(0), trapCode(STATE_RUNNING)
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

#if VM_TRACE == 1
#ifndef EN_FORKWAIT
  enable_waveform = args.enable_waveform;
  if (enable_waveform) {
    Verilated::traceEverOn(true);	// Verilator must compute traced signals
    tfp = new VerilatedVcdC;
    dut_ptr->trace(tfp, 99);	// Trace 99 levels of hierarchy
    time_t now = time(NULL);
    tfp->open(waveform_filename(now));	// Open the dump file
  }
#else
  // VM_TRACE =1 && EN_FORKWAIT
  enable_waveform = false;
#endif
#else
  // VM_TRACE =0
  enable_waveform = false;
#endif

#ifdef VM_SAVABLE
  if (args.snapshot_path != NULL) {
    printf("loading from snapshot `%s`...\n", args.snapshot_path);
    snapshot_load(args.snapshot_path);
    printf("model cycleCnt = %" PRIu64 "\n", dut_ptr->io_trap_cycleCnt);
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
    auto trap = difftest[0]->get_trap_event();
    uint64_t cycle = trap->cycleCnt;
    uint64_t begin = dut_ptr->io_logCtrl_log_begin;
    uint64_t end   = dut_ptr->io_logCtrl_log_end;
    bool in_range  = (begin <= cycle) && (cycle <= end);
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

  difftest_init();
  init_device();
  if (args.enable_diff) {
    init_goldenmem();
  }
  init_nemuproxy();

  uint32_t lasttime_poll = 0;
  uint32_t lasttime_snapshot = 0;
  uint64_t core_max_instr[NUM_CORES];
  for (int i = 0; i < NUM_CORES; i++) {
    core_max_instr[i] = max_instr;
  }

  uint32_t t = uptime();
  if (t - lasttime_poll > 100) {
    poll_event();
    lasttime_poll = t;
  }

#ifdef EN_FORKWAIT
  printf("[INFO] enable fork wait..\n");
  pid_t pid =-1;
  pid_t originPID = getpid();
  int status = -1;
  int slotCnt = 1;
  int waitProcess = 0;
  uint32_t timer = 0;
  std::list<pid_t> pidSlot = {};
  enable_waveform = false;

  //first process as a control process
  if ((pid = fork()) < 0) {
    perror("First fork failed..\n");
    FAIT_EXIT;
  } else if (pid > 0) {  //parent process
    printf("[%d] Control process first fork...child: %d\n ", getpid(), pid);
    prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0);
    forkshm.shwait();
    printf("[%d] Simulation finished, Control process exit..", getpid());
    return cycles;
  } else {
    forkshm.info->exitNum++;
    forkshm.info->flag = true;
    pidSlot.insert(pidSlot.begin(), getpid());
  }
#endif

#if VM_COVERAGE == 1
  // we dump coverage into files at the end
  // since we are not sure when an emu will stop
  // we distinguish multiple dat files by emu start time
  time_t coverage_start_time = time(NULL);
#endif

  while (!Verilated::gotFinish() && trapCode == STATE_RUNNING) {
    // cycle limitation
    if (!max_cycle) {
      trapCode = STATE_LIMIT_EXCEEDED;
      break;
    }
    // instruction limitation
    for (int i = 0; i < NUM_CORES; i++) {
      auto trap = difftest[i]->get_trap_event();
      if (trap->instrCnt >= core_max_instr[i]) {
        trapCode = STATE_LIMIT_EXCEEDED;
        break;
      }
    }
    // assertions
    if (assert_count > 0) {
      eprintf("The simulation stopped. There might be some assertion failed.\n");
      trapCode = STATE_ABORT;
      break;
    }
    // signals
    if (signal_num != 0) {
      trapCode = STATE_SIG;
    }
    if (trapCode != STATE_RUNNING) {
      break;
    }

    for (int i = 0; i < NUM_CORES; i++) {
      auto trap = difftest[i]->get_trap_event();
      if (trap->instrCnt >= args.warmup_instr) {
        printf("Warmup finished. The performance counters will be dumped and then reset.\n");
        dut_ptr->io_perfInfo_clean = 1;
        dut_ptr->io_perfInfo_dump = 1;
        args.warmup_instr = -1;
      }
      if (trap->cycleCnt % args.stat_cycles == args.stat_cycles - 1) {
        dut_ptr->io_perfInfo_clean = 1;
        dut_ptr->io_perfInfo_dump = 1;
      }
    }

    single_cycle();

    max_cycle --;
    dut_ptr->io_perfInfo_clean = 0;
    dut_ptr->io_perfInfo_dump = 0;

    if (args.enable_diff) {
      trapCode = difftest_state();
      if (trapCode != STATE_RUNNING) break;
      if (difftest_step()) {
        trapCode = STATE_ABORT;
        break;
      }
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

#ifdef EN_FORKWAIT
    timer = uptime();
    if (timer - lasttime_snapshot > 1000 * FORK_INTERVAL && !waitProcess) {   // time out need to fork
      lasttime_snapshot = timer;
      if (slotCnt == SLOT_SIZE) {     // kill first wait process
        pid_t temp = pidSlot.back();
        pidSlot.pop_back();
        kill(temp, SIGKILL);
        slotCnt--;
        forkshm.info->exitNum--;
      }
      // fork-wait
      if ((pid = fork()) < 0) {
        eprintf("[%d]Error: could not fork process!\n",getpid());
        return -1;
      } else if (pid != 0) {       // father fork and wait.
        waitProcess = 1;
        wait(&status);
        enable_waveform = forkshm.info->resInfo != STATE_GOODTRAP;
        if (enable_waveform) {
          Verilated::traceEverOn(true);	// Verilator must compute traced signals
          tfp = new VerilatedVcdC;
          dut_ptr->trace(tfp, 99);	// Trace 99 levels of hierarchy
          time_t now = time(NULL);
          tfp->open(waveform_filename(now));	// Open the dump file
        }
      } else {        //child insert its pid
        slotCnt++;
        forkshm.info->exitNum++;
        pidSlot.insert(pidSlot.begin(), getpid());
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

#ifdef EN_FORKWAIT
  if(!waitProcess) display_trapinfo();
  else printf("[%d] checkpoint process: dump wave complete, exit.\n",getpid());
  forkshm.info->exitNum--;
  forkshm.info->resInfo = trapCode;
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

void Emulator::trigger_stat_dump() {
  dut_ptr->io_perfInfo_dump = 1;
  if(get_args().force_dump_result) {
    dut_ptr->io_logCtrl_log_end = -1;
  }
  single_cycle();
}

void Emulator::display_trapinfo() {
  for (int i = 0; i < NUM_CORES; i++) {
    printf("Core %d: ", i);
    auto trap = difftest[i]->get_trap_event();
    uint64_t pc = trap->pc;
    uint64_t instrCnt = trap->instrCnt;
    uint64_t cycleCnt = trap->cycleCnt;

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

    double ipc = (double)instrCnt / cycleCnt;
    eprintf(ANSI_COLOR_MAGENTA "total guest instructions = %'" PRIu64 "\n" ANSI_COLOR_RESET, instrCnt);
    eprintf(ANSI_COLOR_MAGENTA "instrCnt = %'" PRIu64 ", cycleCnt = %'" PRIu64 ", IPC = %lf\n" ANSI_COLOR_RESET,
        instrCnt, cycleCnt, ipc);
  }

  if (trapCode != STATE_ABORT) {
    trigger_stat_dump();
  }
}

#ifdef EN_FORKWAIT
ForkShareMemory::ForkShareMemory() {
  if ((key_n = ftok(".", 's') < 0)) {
    perror("Fail to ftok\n");
    FAIT_EXIT
  }
  printf("key num:%d\n", key_n);

  if ((shm_id = shmget(key_n, 1024, 0666 | IPC_CREAT))==-1) {
    perror("shmget failed...\n");
    FAIT_EXIT
  }
  printf("share memory id:%d\n", shm_id);

  if ((info = (shinfo*)(shmat(shm_id, NULL, 0))) == NULL ) {
    perror("shmat failed...\n");
    FAIT_EXIT
  }

  info->exitNum   = 0;
  info->flag      = false;
  info->resInfo   = -1;           //STATE_RUNNING
}

ForkShareMemory::~ForkShareMemory() {
  if (shmdt(info) == -1) {
    perror("detach error\n");
  }
  shmctl(shm_id, IPC_RMID, NULL);
}

void ForkShareMemory::shwait() {
  while (true) {
    if (info->exitNum == 0 && info->flag) {
      break;
    }
    else {
      sleep(WAIT_INTERVAL);
    }
  }
}
#endif

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

  uint64_t nemu_this_pc = get_nemu_this_pc(0);
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
  ref_difftest_setregs(&ref_r, 0);

  uint64_t nemu_this_pc;
  stream.read(&nemu_this_pc, sizeof(nemu_this_pc));
  set_nemu_this_pc(nemu_this_pc, 0);

  char *buf = (char *)mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  stream.read(buf, size);
  ref_difftest_memcpy_from_dut(0x80000000, buf, size, 0);
  munmap(buf, size);

  struct SyncState sync_mastate;
  stream.read(&sync_mastate, sizeof(struct SyncState));
  ref_difftest_set_mastatus(&sync_mastate, 0);

  uint64_t csr_buf[4096];
  stream.read(&csr_buf, sizeof(csr_buf));
  ref_difftest_set_csr(csr_buf, 0);

  long sdcard_offset = 0;
  stream.read(&sdcard_offset, sizeof(sdcard_offset));
  if(fp)
    fseek(fp, sdcard_offset, SEEK_SET);
}
#endif
