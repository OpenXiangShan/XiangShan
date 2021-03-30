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

  // nemu_this_pc = 0x80000000;
  // pc_retire_pointer = DEBUG_RETIRE_TRACE_SIZE - 1;
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

  if (!has_commit) {
    return 0;
  }

  if (do_store_check()) {
    return 1;
  }
  if (do_golden_memory_check()) {
    return 1;
  }

  int i = 0;
  // interrupt has the highest priority
  if (dut.event.interrupt) {
    do_interrupt();
  } else {
    // TODO: is this else necessary?
    for (i = 0; i < DIFFTEST_COMMIT_WIDTH && dut.commit[i].valid; i++) {
      do_instr_commit(i);
    }
  }

  if (!progress) {
    return 0;
  }

  proxy->get_regs(ref_regs_ptr);

  // uint64_t next_pc = ref.csr.this_pc;
  // pc_retire_pointer = (pc_retire_pointer + 1) % DEBUG_RETIRE_TRACE_SIZE;
  // pc_retire_queue[pc_retire_pointer] = dut.commit[0].pc;
  // inst_retire_queue[pc_retire_pointer] = dut.commit[0].pc;
  // retire_cnt_queue[pc_retire_pointer] = i;

  uint64_t nemu_next_pc = ref.csr.this_pc;
  ref.csr.this_pc = nemu_this_pc;
  nemu_this_pc = nemu_next_pc;

  if (memcmp(dut_regs_ptr, ref_regs_ptr, DIFFTEST_NR_REG * sizeof(uint64_t)) != 0) {
    display();
    for (int i = 0; i < DIFFTEST_NR_REG; i ++) {
      if (dut_regs_ptr[i] != ref_regs_ptr[i]) {
        printf("%7s different at pc = 0x%010lx, right= 0x%016lx, wrong = 0x%016lx\n",
            reg_name[i], ref.csr.this_pc, ref_regs_ptr[i], dut_regs_ptr[i]);
      }
    }
    return 1;
  }

  clear_step();
  return 0;
}

void Difftest::do_interrupt() {
  proxy->raise_intr(dut.event.interrupt);
  progress = true;
}

void Difftest::do_instr_commit(int i) {
  progress = true;
  last_commit = ticks;

  // printf("commit %d %lx\n", i, dut.commit[i].pc);
  // store the writeback info to debug array
  // pc_wb_queue[wb_pointer]    = dut.commit[i].pc;
  // wen_wb_queue[wb_pointer]   = dut.commit[i].wen;
  // wdst_wb_queue[wb_pointer]  = dut.commit[i].wdest;
  // wdata_wb_queue[wb_pointer] = dut.commit[i].wdata;
  // wb_pointer = (wb_pointer + 1) % DEBUG_WB_TRACE_SIZE;

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
  // IPF, LPF, SPF
  if (dut.event.exception == 12 || dut.event.exception == 13 || dut.event.exception == 15) {
    // printf("exception cause: %ld\n", dut.event.exception);
    struct DisambiguationState ds;
    ds.exceptionNo = dut.event.exception;
    ds.mtval = dut.csr.mtval;
    ds.stval = dut.csr.stval;
    proxy->disambiguate_exec(&ds);
  } else {
    proxy->exec(1);

    // // Load instruction
    // if (dut.load[i].fuType == 0xC || dut.load[i].fuType == 0xF) {
    //   proxy->get_regs(ref_regs_ptr);
    //   if (dut.commit[i].wen && ref_regs_ptr[dut.commit[i].wdest] != dut.commit[i].wdata) {
    //     // printf("---[DIFF Core%d] This load instruction gets rectified!\n", coreid);
    //     // printf("---    ltype: 0x%x paddr: 0x%lx wen: 0x%x wdst: 0x%x wdata: 0x%lx pc: 0x%lx\n", s->ltype[i], s->lpaddr[i], selectBit(s->wen, i), s->wdst[i], s->wdata[i], s->wpc[i]);
    //     uint64_t golden;
    //     int len = 0;
    //     if (dut.load[i].fuType == 0xC) {
    //       switch (dut.load[i].opType) {
    //         case 0: len = 1; break;
    //         case 1: len = 2; break;
    //         case 2: len = 4; break;
    //         case 3: len = 8; break;
    //         case 4: len = 1; break;
    //         case 5: len = 2; break;
    //         case 6: len = 4; break;
    //         default:
    //           printf("Unknown fuOpType: 0x%x\n", dut.load[i].opType);
    //       }
    //     } else if (dut.load[i].fuType == 0xF) {
    //       if (dut.load[i].opType % 2 == 0) {
    //         len = 4;
    //       } else if (dut.load[i].opType % 2 == 1) {
    //         len = 8;
    //       }
    //     }
    //     read_goldenmem(dut.load[i].paddr, &golden, len);
    //     if (dut.load[i].fuType == 0xC) {
    //       switch (dut.load[i].opType) {
    //         case 0: golden = (int64_t)(int8_t)golden; break;
    //         case 1: golden = (int64_t)(int16_t)golden; break;
    //         case 2: golden = (int64_t)(int32_t)golden; break;
    //       }
    //     }
    //     // printf("---    golden: 0x%lx  original: 0x%lx\n", golden, ref_r[dut.commit[i].wdest]);
    //     if (golden == dut.commit[i].wdata) {
    //       // ref_difftest_memcpy_from_dut(0x80000000, get_img_start(), get_img_size(), i);
    //       proxy->memcpy_from_dut(dut.load[i].paddr, &golden, len);
    //       if (dut.commit[i].wdest != 0) {
    //         ref_regs_ptr[dut.commit[i].wdest] = dut.commit[i].wdata;
    //         proxy->set_regs(ref_regs_ptr);
    //       }
    //     } else if (dut.load[i].fuType == 0xF) {
    //       proxy->memcpy_from_dut(dut.load[i].paddr, &golden, len);
    //       if (dut.commit[i].wdest != 0) {
    //         ref_regs_ptr[dut.commit[i].wdest] = dut.commit[i].wdata;
    //         proxy->set_regs(ref_regs_ptr);
    //       }
    //       // printf("---    atomic instr carefully handled\n");
    //     } else {
    //       // printf("---    goldenmem check failed as well\n");
    //     }
      // }
    // }
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
      printf("Mismatch for store commits: \n");
      printf("REF commits addr 0x%lx, data 0x%lx, mask 0x%x\n", addr, data, mask);
      printf("DUT commits addr 0x%lx, data 0x%lx, mask 0x%x\n",
        dut.store[i].addr, dut.store[i].data, dut.store[i].mask);
      return 1;
    }
  }
  return 0;
}

inline void handle_atomic(uint64_t atomicAddr, uint64_t atomicData, uint64_t atomicMask, uint8_t atomicFuop, uint64_t atomicOut) {
  if (!(atomicMask == 0xf || atomicMask == 0xf0 || atomicMask == 0xff)) {
    printf("Mask f**ked: %lx\n", atomicMask);
  }
  assert(atomicMask == 0xf || atomicMask == 0xf0 || atomicMask == 0xff);

  if (atomicMask == 0xff) {
    uint64_t rs = atomicData;  // rs2
    uint64_t t  = atomicOut;
    uint64_t ret;
    uint64_t mem;
    read_goldenmem(atomicAddr, &mem, 8);
    if (mem != t && atomicFuop != 007 && atomicFuop != 003) {
      printf("Atomic instr f**ked up, mem: 0x%lx, t: 0x%lx, op: 0x%x, addr: 0x%lx\n", mem, t, atomicFuop, atomicAddr);
      // assert(0);
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
    uint32_t t  = (uint32_t)atomicOut;
    uint32_t ret;
    uint32_t mem;
    uint64_t mem_temp;
    uint64_t ret_temp;
    atomicAddr = (atomicAddr & 0xfffffffffffffff8);
    read_goldenmem(atomicAddr, &mem_temp, 8);

    if (atomicMask == 0xf)
      mem = (uint32_t)mem_temp;
    else
      mem = (uint32_t)(mem_temp >> 32);

    if (mem != t && atomicFuop != 006 && atomicFuop != 002) {
      printf("Atomic instr f**ked up, rawmem: 0x%lx mem: 0x%x, t: 0x%x, op: 0x%x, addr: 0x%lx\n", mem_temp, mem, t, atomicFuop, atomicAddr);
      // assert(0);
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
    ret_temp = ret;
    if (atomicMask == 0xf0)
      ret_temp = (ret_temp << 32);
    update_goldenmem(atomicAddr, &ret_temp, atomicMask, 8);
  }

}

int Difftest::do_golden_memory_check() {
  // Update Golden Memory info
  if (dut.sbuffer.resp) {
    update_goldenmem(dut.sbuffer.addr, dut.sbuffer.data, dut.sbuffer.mask, 64);
  }

  if (dut.atomic.resp) {
    handle_atomic(dut.atomic.addr, dut.atomic.data, dut.atomic.mask, dut.atomic.fuop, dut.atomic.out);
  }

  // TODO
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
  printf("\n==============Retire Trace==============\n");
  int j;
  for(j = 0; j < DEBUG_RETIRE_TRACE_SIZE; j++){
    printf("retire trace [%x]: pc %010lx inst %08x cmtcnt %d %s\n",
        j, pc_retire_queue[j], inst_retire_queue[j], retire_cnt_queue[j],
        (j==pc_retire_pointer)?"<--":"");
  }
  printf("\n==============  WB Trace  ==============\n");
  for(j = 0; j < DEBUG_WB_TRACE_SIZE; j++){
    printf("wb trace [%x]: pc %010lx wen %x dst %08x data %016lx %s\n",
        j, pc_wb_queue[j], wen_wb_queue[j]!=0, wdst_wb_queue[j],
        wdata_wb_queue[j],
        (j==((wb_pointer-1)%DEBUG_WB_TRACE_SIZE))?"<--":"");
  }
  printf("\n==============  REF Regs  ==============\n");
  fflush(stdout);
  proxy->isa_reg_display();
  printf("priviledgeMode: %lu\n", dut.csr.priviledgeMode);
}
