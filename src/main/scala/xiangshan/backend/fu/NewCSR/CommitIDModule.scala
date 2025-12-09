package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.HasBlackBoxInline
import com.typesafe.scalalogging.LazyLogging

import java.util.Properties

class PrintCommitIDModule(shaWidth: Int, hartIdlen: Int) extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle{
    val hartID = Input(UInt(hartIdlen.W))
    val commitID = Input(UInt(shaWidth.W))
    val dirty = Input(Bool())
  })

  setInline("PrintCommitIDModule.v",
    s"""
      |module PrintCommitIDModule(
      |  input [${hartIdlen-1}:0] hartID,
      |  input [${shaWidth-1}:0] commitID,
      |  input dirty
      |);
      |  wire _dummy_unused = 1'b1;
      |`ifndef SYNTHESIS
      |  initial begin
      |    $$fwrite(32'h80000001, "Core %d's Commit SHA is: %h, dirty: %d\\n", hartID, commitID, dirty);
      |  end
      |`endif
      |
      |endmodule
      |""".stripMargin
  )
}

class CommitIDModule(shaWidth: Int, hartIdlen: Int) extends Module with LazyLogging {
  val io = IO(new Bundle {
    val hartId = Input(UInt(hartIdlen.W))
    val commitID = Output(UInt(shaWidth.W))
    val dirty    = Output(Bool())
  })

  val props = new Properties()
  props.load((os.resource / "gitStatus").getInputStream)

  val sha = props.get("SHA").asInstanceOf[String].take(shaWidth / 4)
  val dirty = props.get("dirty").asInstanceOf[String].toInt

  logger.info(s"SHA=$sha")
  logger.info(s"dirty=$dirty")

  io.commitID := BigInt(sha, 16).U(shaWidth.W)
  io.dirty := dirty.U

  val printCommitIDMod = Module(new PrintCommitIDModule(shaWidth, hartIdlen))
  printCommitIDMod.io.hartID := io.hartId
  printCommitIDMod.io.commitID := io.commitID
  printCommitIDMod.io.dirty := io.dirty
}
