package xiangshan.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

class ArchZicfilpIO(implicit p: Parameters) extends XSBundle {
  val commitValid = Input(Vec(CommitWidth, Bool()))
  val commitJalr = Input(Vec(CommitWidth, Bool()))
  val enable = Input(Bool())
  val trap = Input(Bool())
  val xret = Flipped(ValidIO(Bool()))
  val archELP = Output(Bool())
}

class ArchZicfilp(implicit p: Parameters) extends XSModule {
  val io = IO(new ArchZicfilpIO)

  val archELP = RegInit(false.B)
  val hasCommit = io.commitValid.asUInt.orR
  val lastCommitIsJalr = PriorityMux(
    io.commitValid.reverse.zip(io.commitJalr.reverse) :+ (true.B -> false.B)
  )

  when(io.trap) {
    archELP := false.B
  }.elsewhen(io.xret.valid) {
    archELP := io.xret.bits
  }.elsewhen(!io.enable) {
    archELP := false.B
  }.elsewhen(hasCommit) {
    archELP := lastCommitIsJalr
  }

  io.archELP := archELP && io.enable
}
