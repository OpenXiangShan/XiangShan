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

#include "difftest.h"
#include "goldenmem.h"
#include "ram.h"

static const char *reg_name[DIFFTEST_NR_REG+1] = {
  "$0",  "ra",  "sp",   "gp",   "tp",  "t0",  "t1",   "t2",
  "s0",  "s1",  "a0",   "a1",   "a2",  "a3",  "a4",   "a5",
  "a6",  "a7",  "s2",   "s3",   "s4",  "s5",  "s6",   "s7",
  "s8",  "s9",  "s10",  "s11",  "t3",  "t4",  "t5",   "t6",
  "ft0", "ft1", "ft2",  "ft3",  "ft4", "ft5", "ft6",  "ft7",
  "fs0", "fs1", "fa0",  "fa1",  "fa2", "fa3", "fa4",  "fa5",
  "fa6", "fa7", "fs2",  "fs3",  "fs4", "fs5", "fs6",  "fs7",
  "fs8", "fs9", "fs10", "fs11", "ft8", "ft9", "ft10", "ft11",
  "this_pc",
  "mstatus", "mcause", "mepc",
  "sstatus", "scause", "sepc",
  "satp",
  "mip", "mie", "mscratch", "sscratch", "mideleg", "medeleg",
  "mtval", "stval", "mtvec", "stvec", "mode",
};

Difftest **difftest = NULL;

int difftest_init() {
  // init global memory (used for consistency)
  ref_misc_put_gmaddr(pmem);

  difftest = new Difftest*[EMU_CORES];
  for (int i = 0; i < EMU_CORES; i++) {
    difftest[i] = new Difftest(i);
  }

  return 0;
}

int difftest_state() {
  for (int i = 0; i < EMU_CORES; i++) {
    // if (difftest[i]->step(&diff[i], i)) {
    //     trapCode = STATE_ABORT;
    //   }
      // lastcommit[i] = max_cycle;

      // // update instr_cnt
      // uint64_t commit_count = (core_max_instr[i] >= diff[i].commit) ? diff[i].commit : core_max_instr[i];
      // core_max_instr[i] -= commit_count;

    if (difftest[i]->get_trap_valid()) {
      return difftest[i]->get_trap_code();
    }
  }
  return -1;
}

int difftest_step() {
  for (int i = 0; i < EMU_CORES; i++) {
    int ret = difftest[i]->step();
    if (ret) {
      return ret;
    }
  }
  return 0;
}

Difftest::Difftest(int coreid) : id(coreid) {
  proxy = new DIFF_PROXY(coreid);
  state = new DiffState();
  clear_step();
  // nemu_this_pc = 0x80000000;
  // pc_retire_pointer = DEBUG_GROUP_TRACE_SIZE - 1;
}

int Difftest::step() {
  progress = false;
  ticks++;

  // TODO: update nemu/xs to fix this_pc comparison
  dut.csr.this_pc = dut.commit[0].pc;

  if (check_timeout()) {
    return 1;
  }
  do_first_instr_commit();
  if (do_store_check()) {
    return 1;
  }
  if (do_golden_memory_update()) {
    return 1;
  }

  if (!has_commit) {
    return 0;
  }

  num_commit = 0; // reset num_commit this cycle to 0
  // interrupt has the highest priority
  if (dut.event.interrupt) {
    dut.csr.this_pc = dut.event.exceptionPC;
    do_interrupt();
  } else if(dut.event.exception) { 
    // We ignored instrAddrMisaligned exception (0) for better debug interface
    // XiangShan should always support RVC, so instrAddrMisaligned will never happen
    // TODO: update NEMU, for now, NEMU will update pc when exception happen
    dut.csr.this_pc = dut.event.exceptionPC;
    do_exception();
  } else {
    // TODO: is this else necessary?
    while (num_commit < DIFFTEST_COMMIT_WIDTH && dut.commit[num_commit].valid) {
      do_instr_commit(num_commit);
      dut.commit[num_commit].valid = 0;
      num_commit++;
    }
  }

  if (!progress) {
    return 0;
  }

  proxy->get_regs(ref_regs_ptr);

  if (num_commit > 0) {
    state->record_group(dut.commit[0].pc, num_commit);
  }

  // swap nemu_pc and ref.csr.this_pc for comparison
  uint64_t nemu_next_pc = ref.csr.this_pc;
  ref.csr.this_pc = nemu_this_pc;
  nemu_this_pc = nemu_next_pc;
  if (memcmp(dut_regs_ptr, ref_regs_ptr, DIFFTEST_NR_REG * sizeof(uint64_t))) {
    display();
    for (int i = 0; i < DIFFTEST_NR_REG; i ++) {
      if (dut_regs_ptr[i] != ref_regs_ptr[i]) {
        printf("%7s different at pc = 0x%010lx, right= 0x%016lx, wrong = 0x%016lx\n",
            reg_name[i], ref.csr.this_pc, ref_regs_ptr[i], dut_regs_ptr[i]);
      }
    }
    return 1;
  }

  // 
  return 0;
}

void Difftest::do_interrupt() {
  state->record_abnormal_inst(dut.commit[0].pc, dut.commit[0].inst, RET_INT, dut.event.interrupt);
  proxy->raise_intr(dut.event.interrupt | (1ULL << 63));
  progress = true;
}

void Difftest::do_exception() {
  state->record_abnormal_inst(dut.event.exceptionPC, dut.commit[0].inst, RET_EXC, dut.event.exception);
  if (dut.event.exception == 12 || dut.event.exception == 13 || dut.event.exception == 15) {
    // printf("exception cause: %d\n", dut.event.exception);
    struct DisambiguationState ds;
    ds.exceptionNo = dut.event.exception;
    ds.mtval = dut.csr.mtval;
    ds.stval = dut.csr.stval;
    proxy->disambiguate_exec(&ds);
  } else {
    proxy->exec(1);
  }
  progress = true;
}

void Difftest::do_instr_commit(int i) {
  progress = true;
  last_commit = ticks;

  // store the writeback info to debug array
  state->record_inst(dut.commit[i].pc, dut.commit[i].inst, dut.commit[i].wen, dut.commit[i].wdest, dut.commit[i].wdata);

  // sync lr/sc reg status
  if (dut.commit[i].scFailed) {
    struct SyncState sync;
    sync.lrscValid = 0;
    sync.lrscAddr = 0;
    proxy->set_mastatus((uint64_t*)&sync); // sync lr/sc microarchitectural regs
  }

  // MMIO accessing should not be a branch or jump, just +2/+4 to get the next pc
  // to skip the checking of an instruction, just copy the reg state to reference design
  if (dut.commit[i].skip) {
    proxy->get_regs(ref_regs_ptr);
    ref.csr.this_pc += dut.commit[i].isRVC ? 2 : 4;
    if (dut.commit[i].wen && dut.commit[i].wdest != 0) {
      // TODO: FPR
      ref_regs_ptr[dut.commit[i].wdest] = dut.commit[i].wdata;
    }
    proxy->set_regs(ref_regs_ptr);
    return;
  }

  // single step exec
  proxy->exec(1);
  // IPF, LPF, SPF
  // if (dut.event.exception == 12 || dut.event.exception == 13 || dut.event.exception == 15) {
  // printf("exception cause: %ld\n", dut.event.exception);
  // struct DisambiguationState ds;
  // ds.exceptionNo = dut.event.exception;
  // ds.mtval = dut.csr.mtval;
  // ds.stval = dut.csr.stval;
  // proxy->disambiguate_exec(&ds);
  // }

  // Handle load instruction carefully for SMP
  if (dut.load[i].fuType == 0xC || dut.load[i].fuType == 0xF) {
    proxy->get_regs(ref_regs_ptr);
    if (dut.commit[i].wen && ref_regs_ptr[dut.commit[i].wdest] != dut.commit[i].wdata) {
      // printf("---[DIFF Core%d] This load instruction gets rectified!\n", this->id);
      // printf("---    ltype: 0x%x paddr: 0x%lx wen: 0x%x wdst: 0x%x wdata: 0x%lx pc: 0x%lx\n", dut.load[i].opType, dut.load[i].paddr, dut.commit[i].wen, dut.commit[i].wdest, dut.commit[i].wdata, dut.commit[i].pc);
      uint64_t golden;
      int len = 0;
      if (dut.load[i].fuType == 0xC) {
        switch (dut.load[i].opType) {
          case 0: len = 1; break;
          case 1: len = 2; break;
          case 2: len = 4; break;
          case 3: len = 8; break;
          case 4: len = 1; break;
          case 5: len = 2; break;
          case 6: len = 4; break;
          default:
            printf("Unknown fuOpType: 0x%x\n", dut.load[i].opType);
        }
      } else {  // dut.load[i].fuType == 0xF
        if (dut.load[i].opType % 2 == 0) {
          len = 4;
        } else {  // dut.load[i].opType % 2 == 1
          len = 8;
        }
      }
      read_goldenmem(dut.load[i].paddr, &golden, len);
      if (dut.load[i].fuType == 0xC) {
        switch (dut.load[i].opType) {
          case 0: golden = (int64_t)(int8_t)golden; break;
          case 1: golden = (int64_t)(int16_t)golden; break;
          case 2: golden = (int64_t)(int32_t)golden; break;
        }
      }
      // printf("---    golden: 0x%lx  original: 0x%lx\n", golden, ref_regs_ptr[dut.commit[i].wdest]);
      if (golden == dut.commit[i].wdata) {
        proxy->memcpy_from_dut(dut.load[i].paddr, &golden, len);
        if (dut.commit[i].wdest != 0) {
          ref_regs_ptr[dut.commit[i].wdest] = dut.commit[i].wdata;
          proxy->set_regs(ref_regs_ptr);
        }
      } else if (dut.load[i].fuType == 0xF) {  //  atomic instr carefully handled
        proxy->memcpy_from_dut(dut.load[i].paddr, &golden, len);
        if (dut.commit[i].wdest != 0) {
          ref_regs_ptr[dut.commit[i].wdest] = dut.commit[i].wdata;
          proxy->set_regs(ref_regs_ptr);
        }
      } else {
        // goldenmem check failed as well, raise error
        printf("---  SMP difftest mismatch!\n");
        printf("---  Trying to probe local data of another core\n");
        uint64_t buf;
        difftest[(EMU_CORES-1) - this->id]->proxy->memcpy_from_ref(&buf, dut.load[i].paddr, len);
        printf("---    content: %lx\n", buf);
      }
    }
  }
}

void Difftest::do_first_instr_commit() {
  if (!has_commit && dut.commit[0].valid && dut.commit[0].pc == 0x80000000) {
  // when dut commits the first instruction, its state should be copied to ref,
  // because dut is probably randomly initialized.
  // int first_instr_commit = dut_ptr->io_difftest_commit && dut_ptr->io_difftest_thisPC == 0x80000000u;
    has_commit = 1;
    nemu_this_pc = dut.csr.this_pc;

    proxy->memcpy_from_dut(0x80000000, get_img_start(), get_img_size());
    proxy->set_regs(dut_regs_ptr);

    printf("The first instruction of core %d has commited. Difftest enabled. \n", id);
  }
}

int Difftest::do_store_check() {
  for (int i = 0; i < DIFFTEST_STORE_WIDTH; i++) {
    if (!dut.store[i].valid) {
      return 0;
    }
    auto addr = dut.store[i].addr;
    auto data = dut.store[i].data;
    auto mask = dut.store[i].mask;
    if (proxy->store_commit(&addr, &data, &mask)) {
      display();
      printf("Mismatch for store commits %d: \n", i);
      printf("  REF commits addr 0x%lx, data 0x%lx, mask 0x%x\n", addr, data, mask);
      printf("  DUT commits addr 0x%lx, data 0x%lx, mask 0x%x\n",
        dut.store[i].addr, dut.store[i].data, dut.store[i].mask);
      return 1;
    }
    dut.store[i].valid = 0;
  }
  return 0;
}

inline int handle_atomic(int coreid, uint64_t atomicAddr, uint64_t atomicData, uint64_t atomicMask, uint8_t atomicFuop, uint64_t atomicOut) {
  // We need to do atmoic operations here so as to update goldenMem
  if (!(atomicMask == 0xf || atomicMask == 0xf0 || atomicMask == 0xff)) {
    printf("Unrecognized mask: %lx\n", atomicMask);
    return 1;
  }

  if (atomicMask == 0xff) {
    uint64_t rs = atomicData;  // rs2
    uint64_t t  = atomicOut;   // original value
    uint64_t ret;
    uint64_t mem;
    read_goldenmem(atomicAddr, &mem, 8);
    if (mem != t && atomicFuop != 007 && atomicFuop != 003) {  // ignore sc_d & lr_d
      printf("Core %d atomic instr mismatch goldenMem, mem: 0x%lx, t: 0x%lx, op: 0x%x, addr: 0x%lx\n", coreid, mem, t, atomicFuop, atomicAddr);
      return 1;
    }
    switch (atomicFuop) {
      case 002: case 003: ret = t; break;
      case 006: case 007: ret = rs; break;
      case 012: case 013: ret = rs; break;
      case 016: case 017: ret = t+rs; break;
      case 022: case 023: ret = (t^rs); break;
      case 026: case 027: ret = t & rs; break;
      case 032: case 033: ret = t | rs; break;
      case 036: case 037: ret = ((int64_t)t < (int64_t)rs)? t : rs; break;
      case 042: case 043: ret = ((int64_t)t > (int64_t)rs)? t : rs; break;
      case 046: case 047: ret = (t < rs) ? t : rs; break;
      case 052: case 053: ret = (t > rs) ? t : rs; break;
      default: printf("Unknown atomic fuOpType: 0x%x\n", atomicFuop);
    }
    update_goldenmem(atomicAddr, &ret, atomicMask, 8);
  }

  if (atomicMask == 0xf || atomicMask == 0xf0) {
    uint32_t rs = (uint32_t)atomicData;  // rs2
    uint32_t t  = (uint32_t)atomicOut;   // original value
    uint32_t ret;
    uint32_t mem;
    uint64_t mem_raw;
    uint64_t ret_sel;
    atomicAddr = (atomicAddr & 0xfffffffffffffff8);
    read_goldenmem(atomicAddr, &mem_raw, 8);

    if (atomicMask == 0xf)
      mem = (uint32_t)mem_raw;
    else
      mem = (uint32_t)(mem_raw >> 32);

    if (mem != t && atomicFuop != 006 && atomicFuop != 002) {  // ignore sc_w & lr_w
      printf("Core %d atomic instr mismatch goldenMem, rawmem: 0x%lx mem: 0x%x, t: 0x%x, op: 0x%x, addr: 0x%lx\n", coreid, mem_raw, mem, t, atomicFuop, atomicAddr);
      return 1;
    }
    switch (atomicFuop) {
      case 002: case 003: ret = t; break;
      case 006: case 007: ret = rs; break;  // TODO
      case 012: case 013: ret = rs; break;
      case 016: case 017: ret = t+rs; break;
      case 022: case 023: ret = (t^rs); break;
      case 026: case 027: ret = t & rs; break;
      case 032: case 033: ret = t | rs; break;
      case 036: case 037: ret = ((int32_t)t < (int32_t)rs)? t : rs; break;
      case 042: case 043: ret = ((int32_t)t > (int32_t)rs)? t : rs; break;
      case 046: case 047: ret = (t < rs) ? t : rs; break;
      case 052: case 053: ret = (t > rs) ? t : rs; break;
      default: printf("Unknown atomic fuOpType: 0x%x\n", atomicFuop);
    }
    ret_sel = ret;
    if (atomicMask == 0xf0)
      ret_sel = (ret_sel << 32);
    update_goldenmem(atomicAddr, &ret_sel, atomicMask, 8);
  }
  return 0;
}

int Difftest::do_golden_memory_update() {
  // Update Golden Memory info
  if (dut.sbuffer.resp) {
    dut.sbuffer.resp = 0;
    update_goldenmem(dut.sbuffer.addr, dut.sbuffer.data, dut.sbuffer.mask, 64);
  }
  if (dut.atomic.resp) {
    dut.atomic.resp = 0;
    int ret = handle_atomic(id, dut.atomic.addr, dut.atomic.data, dut.atomic.mask, dut.atomic.fuop, dut.atomic.out);
    if (ret) return ret;
  }
  return 0;
}

int Difftest::check_timeout() {
  // check whether there're any commits since the simulation starts
  if (!has_commit && ticks > last_commit + firstCommit_limit) {
    eprintf("No instruction commits for %lu cycles of core %d. Please check the first instruction.\n",
      firstCommit_limit, id);
    eprintf("Note: The first instruction may lie in 0x10000000 which may executes and commits after 500 cycles.\n");
    eprintf("   Or the first instruction may lie in 0x80000000 which may executes and commits after 2000 cycles.\n");
    display();
    return 1;
  }

  // check whether there're any commits in the last 5000 cycles
  if (has_commit && ticks > last_commit + stuck_limit) {
    eprintf("No instruction of core %d commits for %lu cycles, maybe get stuck\n"
        "(please also check whether a fence.i instruction requires more than %lu cycles to flush the icache)\n",
        id, stuck_limit, stuck_limit);
    eprintf("Let REF run one more instruction.\n");
    proxy->exec(1);
    display();
    return 1;
  }

  return 0;
}

void Difftest::raise_trap(int trapCode) {
  dut.trap.valid = 1;
  dut.trap.code = trapCode;
}

void Difftest::clear_step() {
  dut.trap.valid = 0;
  for (int i = 0; i < DIFFTEST_COMMIT_WIDTH; i++) {
    dut.commit[i].valid = 0;
  }
  dut.sbuffer.resp = 0;
  for (int i = 0; i < DIFFTEST_STORE_WIDTH; i++) {
    dut.store[i].valid = 0;
  }
  for (int i = 0; i < DIFFTEST_LOAD_WIDTH; i++) {
    dut.load[i].valid = 0;
  }
  dut.atomic.resp = 0;
  dut.ptw.resp = 0;
}

void Difftest::display() {
  state->display(this->id);

  printf("\n==============  REF Regs  ==============\n");
  fflush(stdout);
  proxy->isa_reg_display();
  printf("priviledgeMode: %lu\n", dut.csr.priviledgeMode);
}

void DiffState::display(int coreid) {
  printf("\n============== Commit Group Trace (Core %d) ==============\n", coreid);
  for (int j = 0; j < DEBUG_GROUP_TRACE_SIZE; j++) {
    printf("commit group [%x]: pc %010lx cmtcnt %d %s\n",
        j, retire_group_pc_queue[j], retire_group_cnt_queue[j],
        (j==((retire_group_pointer-1)%DEBUG_INST_TRACE_SIZE))?"<--":"");
  }
  printf("\n============== Commit Instr Trace ==============\n");
  for (int j = 0; j < DEBUG_INST_TRACE_SIZE; j++) {
    switch(retire_inst_type_queue[j]){
      case RET_NORMAL:
        printf("commit inst [%x]: pc %010lx inst %08x wen %x dst %08x data %016lx %s\n",
            j, retire_inst_pc_queue[j], retire_inst_inst_queue[j], retire_inst_wen_queue[j]!=0, retire_inst_wdst_queue[j],
            retire_inst_wdata_queue[j],
            (j==((retire_inst_pointer-1)%DEBUG_INST_TRACE_SIZE))?"<--":"");
        break;
      case RET_EXC:
        printf("exception   [%x]: pc %010lx inst %08x cause %016lx %s\n",
            j, retire_inst_pc_queue[j], retire_inst_inst_queue[j], retire_inst_wdata_queue[j],
            (j==((retire_inst_pointer-1)%DEBUG_INST_TRACE_SIZE))?"<--":"");
        break;
      case RET_INT:
        printf("interrupt   [%x]: pc %010lx inst %08x cause %016lx %s\n",
            j, retire_inst_pc_queue[j], retire_inst_inst_queue[j], retire_inst_wdata_queue[j],
            (j==((retire_inst_pointer-1)%DEBUG_INST_TRACE_SIZE))?"<--":"");
        break;
    }
  }
  fflush(stdout);
}

DiffState::DiffState(int history_length) {

}
