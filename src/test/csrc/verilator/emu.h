/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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

#ifndef __EMU_H
#define __EMU_H

#include "common.h"
#include "snapshot.h"
#include "VSimTop.h"
#include <verilated_vcd_c.h>	// Trace file format header
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/prctl.h>
#include <stdlib.h>
#include <unistd.h>

#define FORK_INTERVAL 10 // unit: second
#define SLOT_SIZE 3
#define FAIT_EXIT    exit(EXIT_FAILURE);
#define WAIT_INTERVAL 1
#define SNAPSHOT_INTERVAL 60 // unit: second

typedef struct shinfo{
  int exitNum;
  int resInfo;
  bool flag;
} shinfo;

class ForkShareMemory{
    //private
    key_t  key_n ;
    int shm_id;

public:
    shinfo *info;    

    ForkShareMemory();
    ~ForkShareMemory();

    void shwait();
};


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
  bool force_dump_result;
  bool enable_diff;

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
    force_dump_result = false;
    enable_diff = true;
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
#ifdef EN_FORKWAIT
  ForkShareMemory forkshm;
#endif

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
  int trapCode;

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
