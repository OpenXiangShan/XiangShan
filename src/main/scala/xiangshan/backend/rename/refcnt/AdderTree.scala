package xiangshan.backend.rename.refcnt

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils.{OneHot, ParallelOR}

class AdderTree(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val incVec = Vec(RenameWidth, Input(UInt(PhyRegIdxWidth.W)))
    val decVec = Vec(CommitWidth, Input(UInt(PhyRegIdxWidth.W)))

    val cntVec = Vec(NRPhyRegs, Input(UInt(2.W)))
    val nextCntVec = Vec(NRPhyRegs, Output(UInt(2.W)))
  })
  
  val incBitVec = Vec(NRPhyRegs, Wire(Bool()))
  val decBitVec = Vec(NRPhyRegs, Wire(Bool()))

  incBitVec zip ParallelOR(io.incVec.map(OneHot.UIntToOH1(_))).asBools() foreach {
    case (bit, i) => bit := i
  }

  decBitVec zip ParallelOR(io.decVec.map(OneHot.UIntToOH1(_))).asBools() foreach {
    case (bit, d) => bit := d
  }

  incBitVec zip decBitVec zip io.cntVec zip io.nextCntVec foreach {
    case (((inc, dec), cnt), nextCnt) =>
      val adder = Module(new Adder)
      adder.io.inc := inc 
      adder.io.dec := dec
      adder.io.cnt := cnt
      nextCnt := adder.io.nextCnt
  }
}
