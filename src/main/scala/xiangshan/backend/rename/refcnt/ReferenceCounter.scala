package xiangshan.backend.rename.refcnt

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.XSModule

class ReferenceCounter(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    // inc/dec Vec must not contain duplicate indices
    val incVec = Vec(RenameWidth, Input(UInt(PhyRegIdxWidth.W)))
    val decVec = Vec(CommitWidth, Input(UInt(PhyRegIdxWidth.W)))

    val maxVec = Vec(NRPhyRegs, Output(Bool()))
    val zeroVec = Vec(NRPhyRegs, Output(Bool()))

    val reset = Flipped(ValidIO(Vec(32, UInt(PhyRegIdxWidth.W))))
  })

  val counterRegs = RegInit(VecInit(Seq.tabulate(NRPhyRegs){
    case i if (0 <= i && i < 32) => 1.U(2.W)
    case _ => 0.U(2.W)
  }))

  val adderTree = Module(new AdderTree())

  adderTree.io.incVec := io.incVec
  adderTree.io.decVec := io.decVec
  adderTree.io.cntVec := counterRegs
  counterRegs := adderTree.io.nextCntVec

  io.maxVec zip counterRegs foreach {
    case (max, counter) => max := counter.andR()
  }

  io.zeroVec zip counterRegs foreach {
    case (free, counter) => free := ~counter.orR()
  }

  when (io.reset.valid) {
    counterRegs.foreach(_ := 0.U)
    // set reference counts of indices in arch RAT as 1
    io.reset.bits foreach { idx =>
      counterRegs(idx) := 1.U
    }
  }
}