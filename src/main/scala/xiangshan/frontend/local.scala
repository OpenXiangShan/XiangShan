package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._

abstract class AbstractLocalBranchPredictorBank(implicit p: Parameters) extends XSModule with HasBPUConst {
  val io = IO(new Bundle {
    val f0_valid = Input(Bool())
    val f0_pc    = Input(UInt(VAddrBits.W))

    val f1_lhist  = Output(UInt(LHistoryLength.W))

    val f3_lhist = Output(UInt(LHistoryLength.W))

    val f3_taken_br = Input(Bool())
    val f3_fire = Input(Bool())

    val update = Input(new Bundle {
      val valid      = Input(Bool())
      val mispredict = Input(Bool())
      val repair     = Input(Bool())
      val pc         = Input(UInt(VAddrBits.W))
      val lhist      = Input(UInt(LHistoryLength.W))
    })
  })
}

class NullLocalBranchPredictorBank(implicit p: Parameters) extends AbstractLocalBranchPredictorBank
{
  io.f1_lhist := 0.U
  io.f3_lhist := 0.U
}

class LocalBranchPredictorBank(implicit p: Parameters) extends AbstractLocalBranchPredictorBank with HasBPUConst
{
  val nSets = LHistoryLength

  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nSets-1).U) { doing_reset := false.B }


  val entries = SyncReadMem(nSets, UInt(LHistoryLength.W))

  val s0_idx = io.f0_pc
  val s1_rhist = entries.read(s0_idx, io.f0_valid)
  val s2_rhist = RegNext(s1_rhist)
  val s3_rhist = RegNext(s2_rhist)
  io.f1_lhist := s1_rhist
  io.f3_lhist := s3_rhist

  val f3_do_update    = Reg(Bool())
  val f3_update_idx   = Reg(UInt(log2Ceil(nSets).W))
  val f3_update_lhist = Reg(UInt(LHistoryLength.W))


  f3_do_update := false.B
  f3_update_idx := DontCare
  f3_update_lhist := DontCare

  val s1_update = RegNext(io.update)
  val s1_update_idx = s1_update.pc

  when (s1_update.valid &&
        (s1_update.mispredict || s1_update.repair)) {
    f3_do_update    := true.B
    f3_update_idx   := s1_update_idx
    f3_update_lhist := s1_update.lhist

  } .elsewhen (io.f3_fire) {

    f3_do_update    := true.B
    f3_update_idx   := RegNext(RegNext(RegNext(s0_idx)))
    f3_update_lhist := s3_rhist << 1 | io.f3_taken_br

  }


  when (doing_reset || f3_do_update) {
    entries.write(Mux(doing_reset, reset_idx, f3_update_idx),
                  Mux(doing_reset, 0.U, f3_update_lhist))
  }

}
