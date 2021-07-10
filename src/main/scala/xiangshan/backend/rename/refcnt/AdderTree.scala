package xiangshan.backend.rename.refcnt

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils.{OneHot, ParallelOR}

class AdderTree(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    // inc/dec Vec must not contain duplicate indices
    val incVec = Vec(RenameWidth, Input(UInt(PhyRegIdxWidth.W)))
    val decVec = Vec(CommitWidth, Input(UInt(PhyRegIdxWidth.W)))

    val cntVec = Vec(NRPhyRegs, Input(UInt(2.W)))
    val nextCntVec = Vec(NRPhyRegs, Output(UInt(2.W)))
  })

  val incBitVec = Wire(Vec(NRPhyRegs, Bool()))
  val decBitVec = Wire(Vec(NRPhyRegs, Bool()))

  incBitVec zip ParallelOR(io.incVec.map(UIntToOH(_))).asBools() foreach {
    case (bit, i) => bit := i
  }

  decBitVec zip ParallelOR(io.decVec.map(UIntToOH(_))).asBools() foreach {
    case (bit, d) => bit := d
  }

  val adderVec = Array.fill(NRPhyRegs)(Module(new Adder))
  incBitVec zip decBitVec zip io.cntVec zip io.nextCntVec zip adderVec foreach {
    case ((((inc, dec), cnt), nextCnt), adder) =>
      adder.io.inc := inc
      adder.io.dec := dec
      adder.io.cnt := cnt
      nextCnt := adder.io.nextCnt
  }
}
