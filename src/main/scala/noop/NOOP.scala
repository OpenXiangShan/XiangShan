package noop

import chisel3._
import chisel3.util._

import bus.simplebus.{SimpleBus, SimpleBusCrossbar}
import bus.axi4._
import utils._

trait NOOPConfig {
  val HasIcache = true
  val HasDcache = true
  val HasMExtension = true
  val HasDiv = true

  // [start, end)
  val AddressSpace = List(
    (0x80000000L, 0x90000000L),  // dram
    (0x40000000L, 0x50000000L),  // mmio
    (0x50000000L, 0x60000000L)   // uncache memory: vmem, gpuMetadata
  )
}

class NOOP extends Module with NOOPConfig with HasCSRConst with HasFuType {
  val io = IO(new Bundle {
    val imem = new AXI4
    val dmem = new AXI4
    val mmio = new SimpleBus
    val uncacheMem = new AXI4
    val difftest = new DiffTestIO
  })

  val ifu = Module(new IFU)
  val idu = Module(new IDU)
  val isu = Module(new ISU)
  val exu = Module(new EXU)
  val wbu = Module(new WBU)

  val icacheHit = WireInit(false.B)
  io.imem <> (if (HasIcache) {
    val icache = Module(new Cache(ro = true, name = "icache"))
    icacheHit := icache.io.hit
    icache.io.in <> ifu.io.imem
    icache.io.out
  } else { ifu.io.imem.toAXI4() })

  idu.io.in <> ifu.io.out
  isu.io.in <> idu.io.out
  exu.io.in <> isu.io.out
  wbu.io.in <> exu.io.out
  wbu.io.brIn <> exu.io.br
  isu.io.wb <> wbu.io.wb
  ifu.io.br <> wbu.io.brOut
  ifu.io.writeback := wbu.io.writeback

  val xbar = Module(new SimpleBusCrossbar(1, AddressSpace))
  val dmem = xbar.io.out(0)
  xbar.io.in(0) <> exu.io.dmem

  val dcacheHit = WireInit(false.B)
  io.dmem <> (if (HasDcache) {
    val dcache = Module(new Cache(ro = false, name = "dcache"))
    dcacheHit := dcache.io.hit
    dcache.io.in <> dmem
    dcache.io.out
  } else { dmem.toAXI4() })

  io.mmio <> xbar.io.out(1)
  io.uncacheMem <> xbar.io.out(2).toAXI4()

  // csr
  val csr = Module(new CSR)
  csr.access(
    valid = exu.io.csr.isCsr,
    src1 = exu.io.in.bits.data.src1,
    src2 = exu.io.in.bits.data.src2,
    func = exu.io.in.bits.ctrl.fuOpType
  )
  exu.io.csr.in <> csr.io.out
  ifu.io.csrjmp <> csr.io.csrjmp
  csr.io.pc := exu.io.in.bits.pc
  csr.io.isInvOpcode := exu.io.in.bits.ctrl.isInvOpcode

  // perfcnt
  csr.io.perfCntCond.map( _ := false.B )
  csr.setPerfCnt(Mcycle, true.B)
  csr.setPerfCnt(Minstret, wbu.io.writeback)
  csr.setPerfCnt(MImemStall, ifu.io.imemStall)
  // instruction types
  csr.setPerfCnt(MALUInstr, exu.io.csr.instrType(FuAlu))
  csr.setPerfCnt(MBRUInstr, exu.io.csr.instrType(FuBru))
  csr.setPerfCnt(MLSUInstr, exu.io.csr.instrType(FuLsu))
  csr.setPerfCnt(MMDUInstr, exu.io.csr.instrType(FuMdu))
  csr.setPerfCnt(MCSRInstr, exu.io.csr.instrType(FuCsr))
  // load/store before dcache
  csr.setPerfCnt(MLoadInstr, dmem.isRead() && dmem.req.fire())
  csr.setPerfCnt(MLoadStall, BoolStopWatch(dmem.isRead(), dmem.resp.fire()))
  csr.setPerfCnt(MStoreStall, BoolStopWatch(dmem.isWrite(), dmem.resp.fire()))
  // mmio
  csr.setPerfCnt(MmmioInstr, io.mmio.req.fire())
  // cache
  csr.setPerfCnt(MIcacheHit, icacheHit)
  csr.setPerfCnt(MDcacheHit, dcacheHit)
  // mul
  csr.setPerfCnt(MmulInstr, exu.io.csr.isMul)

  // monitor
  val mon = Module(new Monitor)
  mon.io.clk := clock
  mon.io.isNoopTrap := isu.io.out.bits.ctrl.isNoopTrap
  mon.io.trapCode := isu.io.out.bits.data.src1
  mon.io.trapPC := isu.io.out.bits.pc
  mon.io.cycleCnt := csr.io.sim.cycleCnt
  mon.io.instrCnt := csr.io.sim.instrCnt

  // difftest
  // latch writeback signal to let register files and pc update
  io.difftest.commit := RegNext(wbu.io.writeback)
  isu.io.difftestRegs.zipWithIndex.map { case(r, i) => io.difftest.r(i) := r }
  io.difftest.r(32) := ifu.io.out.bits.pc
}
