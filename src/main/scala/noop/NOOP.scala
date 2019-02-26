package noop

import chisel3._
import chisel3.util._

import bus.simplebus.{SimpleBus, SimpleBusCrossbar}

trait NOOPConfig {
  val HasIcache = true
  val HasMExtension = true
  val HasDiv = false

  // [start, end)
  val AddressSpace = List(
    (0x80000000L, 0xa0000000L),
    (0x40000000L, 0x60000000L)
  )
}

class NOOP extends Module with NOOPConfig with HasCSRConst with HasFuType {
  val io = IO(new Bundle {
    val imem = new SimpleBus
    val dmem = new SimpleBus
    val mmio = new SimpleBus
    val trap = Output(UInt(2.W))
    val sim = new Bundle {
      val cycleCnt = Output(UInt(32.W))
      val instrCnt = Output(UInt(32.W))
    }
  })

  val ifu = Module(new IFU)
  val idu = Module(new IDU)
  val isu = Module(new ISU)
  val exu = Module(new EXU)
  val wbu = Module(new WBU)

  if (HasIcache) {
    val icache = Module(new ICache)
    icache.io.in <> ifu.io.imem
    io.imem <> icache.io.out
  }
  else {
    io.imem <> ifu.io.imem
  }

  idu.io.in <> ifu.io.out
  isu.io.in <> idu.io.out
  exu.io.in <> isu.io.out
  wbu.io.in <> exu.io.out
  wbu.io.brIn <> exu.io.br
  isu.io.wb <> wbu.io.wb
  ifu.io.br <> wbu.io.brOut
  ifu.io.writeback := wbu.io.writeback

  val xbar = Module(new SimpleBusCrossbar(1, AddressSpace))
  xbar.io.in(0) <> exu.io.dmem
  io.dmem <> xbar.io.out(0)
  io.mmio <> xbar.io.out(1)

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
  csr.setPerfCnt(MALUInstr, exu.io.csr.instrType(FuAlu))
  csr.setPerfCnt(MBRUInstr, exu.io.csr.instrType(FuBru))
  csr.setPerfCnt(MLSUInstr, exu.io.csr.instrType(FuLsu))
  csr.setPerfCnt(MMDUInstr, exu.io.csr.instrType(FuMdu))
  csr.setPerfCnt(MCSRInstr, exu.io.csr.instrType(FuCsr))
  csr.setPerfCnt(MLoadInstr, exu.io.csr.isLoad)
  csr.setPerfCnt(MLoadStall, exu.io.csr.loadStall)
  csr.setPerfCnt(MStoreStall, exu.io.csr.storeStall)

  io.trap := isu.io.trap
  io.sim <> csr.io.sim
}
