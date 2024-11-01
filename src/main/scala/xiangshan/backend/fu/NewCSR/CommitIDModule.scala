package xiangshan.backend.fu.NewCSR

import chisel3._

import java.util.Properties

class CommitIDModule(shaWidth: Int) extends Module {
  val io = IO(new Bundle {
    val commitID = Output(UInt(shaWidth.W))
    val dirty    = Output(Bool())
  })

  val props = new Properties()
  props.load((os.resource / "gitStatus").getInputStream)

  val sha = props.get("SHA").asInstanceOf[String].take(shaWidth / 4)
  val dirty = props.get("dirty").asInstanceOf[String].toInt

  println(s"[CommitIDModule] SHA=$sha")
  println(s"[CommitIDModule] dirty=$dirty")

  io.commitID := BigInt(sha, 16).U(shaWidth.W)
  io.dirty := dirty.U
}
