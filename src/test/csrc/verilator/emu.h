#ifndef __EMU_H
#define __EMU_H

#include "common.h"
#include "snapshot.h"
#include "VSimTop.h"
#include <verilated_vcd_c.h>	// Trace file format header

#define SNAPSHOT_INTERVAL 60 // unit: second

struct EmuArgs {
  uint32_t seed;
  uint64_t max_cycles;
  uint64_t max_instr;
  uint64_t warmup_instr;
  uint64_t stat_cycles;
  uint64_t log_begin, log_end;
  const char *image;
  const char *snapshot_path;
  bool enable_waveform;
  bool enable_snapshot;

  EmuArgs() {
    seed = 0;
    max_cycles = -1;
    max_instr = -1;
    warmup_instr = -1;
    stat_cycles = -1;
    log_begin = 1;
    log_end = -1;
    snapshot_path = NULL;
    image = NULL;
    enable_waveform = false;
    enable_snapshot = true;
  }
};

class Emulator {
private:
  VSimTop *dut_ptr;
  VerilatedVcdC* tfp;
  bool enable_waveform;
#ifdef VM_SAVABLE
  VerilatedSaveMem snapshot_slot[2];
#endif
  EmuArgs args;

  enum {
    STATE_GOODTRAP = 0,
    STATE_BADTRAP = 1,
    STATE_ABORT = 2,
    STATE_LIMIT_EXCEEDED = 3,
    STATE_SIG = 4,
    STATE_RUNNING = -1
  };

  // emu control variable
  uint64_t cycles;
  // int hascommit[NumCore];
  int trapCode;
  // uint64_t max_instr[EMU_CORES];
  // uint64_t max_cycle[EMU_CORES];

  inline void reset_ncycles(size_t cycles);
  inline void single_cycle();
  void trigger_stat_dump();
  void display_trapinfo();
  inline char* timestamp_filename(time_t t, char *buf);
  inline char* snapshot_filename(time_t t);
  inline char* coverage_filename(time_t t);
  void snapshot_save(const char *filename);
  void snapshot_load(const char *filename);
  inline char* waveform_filename(time_t t);
#if VM_COVERAGE == 1
  inline void save_coverage(time_t t);
#endif

public:
  Emulator(int argc, const char *argv[]);
  ~Emulator();
  uint64_t execute(uint64_t max_cycle, uint64_t max_instr);
  uint64_t get_cycles() const { return cycles; }
  EmuArgs get_args() const { return args; }
  bool is_good_trap() {
    return trapCode == STATE_GOODTRAP || trapCode == STATE_LIMIT_EXCEEDED;
  };
  int get_trapcode() { return trapCode; }  
};

#endif
