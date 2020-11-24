#include "common.h"
#include "snapshot.h"
#include "VXSSimSoC.h"
#include <verilated_vcd_c.h>	// Trace file format header

#define DIFFTEST_WIDTH 6
#define SNAPSHOT_INTERVAL 10 // unit: second

struct EmuArgs {
  uint32_t seed;
  uint64_t max_cycles;
  uint64_t log_begin, log_end;
  const char *image;
  const char *snapshot_path;
  bool enable_waveform;

  EmuArgs() {
    seed = 0;
    max_cycles = -1;
    log_begin = 1;
    log_end = -1;
    snapshot_path = NULL;
    image = NULL;
    enable_waveform = false;
  }
};

class Emulator {
  VXSSimSoC *dut_ptr;
  VerilatedVcdC* tfp;
  bool enable_waveform;
#ifdef VM_SAVABLE
  VerilatedSaveMem snapshot_slot[2];
#endif
  EmuArgs args;

  enum {
    STATE_GOODTRAP = 0,
    STATE_BADTRAP,
    STATE_ABORT,
    STATE_RUNNING = -1
  };

  // emu control variable
  uint64_t cycles;
  int hascommit;
  int trapCode;

  inline void read_emu_regs(uint64_t *r);
  inline void read_wb_info(uint64_t *wpc, uint64_t *wdata, uint32_t *wdst);
  inline void reset_ncycles(size_t cycles);
  inline void single_cycle();
  void display_trapinfo();
  inline char* timestamp_filename(time_t t, char *buf);
  inline char* snapshot_filename(time_t t);
  void snapshot_save(const char *filename);
  void snapshot_load(const char *filename);
  inline char* waveform_filename(time_t t);

public:
  Emulator(int argc, const char *argv[]);
  ~Emulator();
  uint64_t execute(uint64_t n);
  uint64_t get_cycles() const { return cycles; }
  EmuArgs get_args() const { return args; }
  bool is_good_trap() { return trapCode == STATE_GOODTRAP; };
};
