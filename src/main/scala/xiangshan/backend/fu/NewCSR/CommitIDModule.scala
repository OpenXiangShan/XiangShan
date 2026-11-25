package xiangshan.backend.fu.NewCSR

import chisel3._
import difftest.DifftestModule

import java.util.Properties

class PrintCommitIDModule(shaWidth: Int, hartIdlen: Int) extends ExtModule {
  val io = IO(new Bundle{
    val hartID = Input(UInt(hartIdlen.W))
    val commitID = Input(UInt(shaWidth.W))
    val dirty = Input(Bool())
  })

  private def cppUIntType(width: Int): String = {
    val storageWidth = Seq(8, 16, 32, 64).find(_ >= width).getOrElse(64)
    s"uint${storageWidth}_t"
  }

  private val cppExtModule =
    raw"""
       |#include <algorithm>
       |#include <cinttypes>
       |#include <cstdio>
       |#include <vector>
       |
       |[[gnu::cold, gnu::noinline]]
       |static void printCommitIDForHart(
       |  ${cppUIntType(hartIdlen)} hartID, ${cppUIntType(shaWidth)} commitID,
       |  uint8_t dirty, bool& allHartsPrinted
       |) {
       |  static std::vector<${cppUIntType(hartIdlen)}> printedHarts;
       |  static unsigned numPrintedHarts = 0;
       |  if (std::find(printedHarts.begin(), printedHarts.end(), hartID) != printedHarts.end()) {
       |    return;
       |  }
       |  printedHarts.push_back(hartID);
       |  allHartsPrinted = ++numPrintedHarts == NUM_CORES;
       |  std::printf("Core %" PRIu64 "'s Commit SHA is: %" PRIx64 ", dirty: %u\n", hartID, commitID, dirty);
       |  std::fflush(stdout);
       |}
       |
       |void PrintCommitIDModule(${cppUIntType(hartIdlen)} hartID, ${cppUIntType(shaWidth)} commitID, uint8_t dirty) {
       |  // GSIM calls this external module on every step, do quick return before any check/print.
       |  static bool allHartsPrinted = false;
       |  if (likely(allHartsPrinted)) {
       |    return;
       |  }
       |  printCommitIDForHart(hartID, commitID, dirty, allHartsPrinted);
       |}
       |""".stripMargin
  DifftestModule.createCppExtModule("PrintCommitIDModule", cppExtModule)

  setInline("PrintCommitIDModule.v",
    s"""
      |module PrintCommitIDModule(
      |  input [${hartIdlen-1}:0] io_hartID,
      |  input [${shaWidth-1}:0] io_commitID,
      |  input io_dirty
      |);
      |  wire _dummy_unused = 1'b1;
      |`ifndef SYNTHESIS
      |  initial begin
      |    $$fwrite(32'h80000001, "Core %d's Commit SHA is: %h, dirty: %d\\n", io_hartID, io_commitID, io_dirty);
      |  end
      |`endif
      |
      |endmodule
      |""".stripMargin
  )
}

class CommitIDModule(shaWidth: Int, hartIdlen: Int) extends Module {
  val io = IO(new Bundle {
    val hartId = Input(UInt(hartIdlen.W))
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

  val printCommitIDMod = Module(new PrintCommitIDModule(shaWidth, hartIdlen))
  printCommitIDMod.io.hartID := io.hartId
  printCommitIDMod.io.commitID := io.commitID
  printCommitIDMod.io.dirty := io.dirty
}
