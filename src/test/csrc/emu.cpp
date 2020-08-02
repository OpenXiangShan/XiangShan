#include "emu.h"
#include "difftest.h"

void* get_ram_start();
long get_ram_size();
uint64_t get_nemu_this_pc();
void set_nemu_this_pc(uint64_t pc);

Emulator::Emulator(EmuArgs &args):
  dut_ptr(new VXSSimTop),
  cycles(0), hascommit(0), trapCode(STATE_RUNNING)
{
  // srand
  srand(args.seed);
  srand48(args.seed);
  Verilated::randReset(2);

  // init ram
  extern void init_ram(const char *img);
  init_ram(args.image);

  // init device
  extern void init_device(void);
  init_device();

  // init core
  reset_ncycles(10);

  if (args.snapshot_path != NULL) {
    init_difftest();
    snapshot_load(args.snapshot_path);
    hascommit = 1;
  }

  // set log time range and log level
  dut_ptr->io_logCtrl_log_begin = args.log_begin;
  dut_ptr->io_logCtrl_log_end = args.log_end;
}

Emulator::~Emulator() {
  snapshot_slot[0].save();
  snapshot_slot[1].save();
  printf("Please remove unused snapshots manually\n");
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
  dut_ptr->eval();

  dut_ptr->clock = 1;
  dut_ptr->eval();

#if VM_TRACE
  tfp->dump(cycles);
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
  const int stuck_limit = 500;

  static uint32_t wdst[DIFFTEST_WIDTH];
  static uint64_t wdata[DIFFTEST_WIDTH];
  static uint64_t wpc[DIFFTEST_WIDTH];

  extern int difftest_step(int commit, uint64_t *reg_scala, uint32_t this_inst,
      int skip, int isRVC, uint64_t *wpc, uint64_t *wdata, uint32_t *wdst, int wen, uint64_t intrNO, int priviledgeMode);

#if VM_TRACE
  Verilated::traceEverOn(true);	// Verilator must compute traced signals
  VL_PRINTF("Enabling waves...\n");
  tfp = new VerilatedVcdC;
  dut_ptr->trace(tfp, 99);	// Trace 99 levels of hierarchy
  tfp->open("vlt_dump.vcd");	// Open the dump file
#endif

  while (trapCode == STATE_RUNNING && n > 0) {
    single_cycle();
    n --;

    if (dut_ptr->io_trap_valid) trapCode = dut_ptr->io_trap_code;
    if (trapCode != STATE_RUNNING) break;

    if (lastcommit - n > stuck_limit && hascommit) {
      eprintf("No instruction commits for %d cycles, maybe get stuck\n"
          "(please also check whether a fence.i instruction requires more than %d cycles to flush the icache)\n",
          stuck_limit, stuck_limit);
#if VM_TRACE
      tfp->close();
#endif
      // commit a fake inst to trigger error
      uint64_t reg[DIFFTEST_NR_REG];
      difftest_step(1, reg, 0, 0, 0, wpc, wdata, wdst, 0, 0, 0);
      trapCode = STATE_ABORT;
    }

    if (!hascommit && dut_ptr->io_difftest_commit && dut_ptr->io_difftest_thisPC == 0x80000000u) {
      hascommit = 1;
      uint64_t reg[DIFFTEST_NR_REG];
      read_emu_regs(reg);
      init_difftest();
      void* get_img_start();
      long get_img_size();
      ref_difftest_memcpy_from_dut(0x80000000, get_img_start(), get_img_size());
      ref_difftest_setregs(reg);
    }

    // difftest
    if (dut_ptr->io_difftest_commit && hascommit) {
      uint64_t reg[DIFFTEST_NR_REG];
      read_emu_regs(reg);
      read_wb_info(wpc, wdata, wdst);

      if (difftest_step(dut_ptr->io_difftest_commit, reg, dut_ptr->io_difftest_thisINST,
            dut_ptr->io_difftest_skip, dut_ptr->io_difftest_isRVC,
            wpc, wdata, wdst, dut_ptr->io_difftest_wen,
            dut_ptr->io_difftest_intrNO, dut_ptr->io_difftest_priviledgeMode)) {
#if VM_TRACE
        tfp->close();
#endif
        trapCode = STATE_ABORT;
      }
      lastcommit = n;
    }

    uint32_t t = uptime();
    if (t - lasttime_poll > 100) {
      poll_event();
      lasttime_poll = t;
    }
    if (t - lasttime_snapshot > 1000 * SNAPSHOT_INTERVAL) {
      // save snapshot every 10s
      time_t now = time(NULL);
      snapshot_save(snapshot_filename(now));
      lasttime_snapshot = t;
    }
  }

  display_trapinfo();
  return cycles;
}

inline char* Emulator::snapshot_filename(time_t t) {
  static char buf[1024];
  char buf_time[64];
  strftime(buf_time, sizeof(buf_time), "%F@%T", localtime(&t));
  char *noop_home = getenv("NOOP_HOME");
  assert(noop_home != NULL);
  snprintf(buf, 1024, "%s/build/%s.snapshot", noop_home, buf_time);
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
}
