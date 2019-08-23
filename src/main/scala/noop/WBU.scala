package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class WBU(implicit val p: NOOPConfig) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CommitIO))
    val wb = new WriteBackIO
    val brOut = new BranchIO
  })

  io.wb.rfWen := io.in.bits.ctrl.rfWen && io.in.valid
  io.wb.rfDest := io.in.bits.ctrl.rfDest
  io.wb.rfWdata := io.in.bits.commits(io.in.bits.ctrl.fuType).rfWdata
  io.in.ready := true.B

  io.brOut <> io.in.bits.br
  io.brOut.isTaken := io.in.bits.br.isTaken && io.in.valid && (io.in.bits.br.target =/= io.in.bits.npc)

  BoringUtils.addSource(io.in.valid, "perfCntCondMinstret")
  if (!p.FPGAPlatform) {
    BoringUtils.addSource(RegNext(io.in.valid), "difftestCommit")
    BoringUtils.addSource(RegNext(io.in.bits.pc), "difftestThisPC")
    BoringUtils.addSource(RegNext(io.in.bits.isMMIO), "difftestIsMMIO")
  }
}
