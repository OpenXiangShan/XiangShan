package xiangshan.backend.trace

import chisel3._
import chisel3.util.{RegEnable, ValidIO, log2Up}
import org.chipsalliance.cde.config.Parameters
import xiangshan.HasXSParameter

class TraceParams(
  val TraceGroupNum  : Int,
  val IaddrWidth     : Int,
  val PrivWidth      : Int,
  val ItypeWidth     : Int,
  val IlastsizeWidth : Int,
)

class TraceIO(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val in = new Bundle {
    val fromEncoder    = Input(new FromEncoder)
    val fromRob        = Flipped(new TraceBundle(hasIaddr = false, CommitWidth, IretireWidthInPipe))
  }
  val out = new Bundle {
    val toPcMem        = new TraceBundle(hasIaddr = false, TraceGroupNum, IretireWidthCompressed)
    val toEncoder      = new TraceBundle(hasIaddr = false,  TraceGroupNum, IretireWidthCompressed)
    val blockRobCommit = Output(Bool())
  }
}

class Trace(implicit val p: Parameters) extends Module with HasXSParameter {
  val io = IO(new TraceIO)
  val (fromEncoder, fromRob, toPcMem, toEncoder) = (io.in.fromEncoder, io.in.fromRob, io.out.toPcMem, io.out.toEncoder)

  /**
   * stage 0: CommitInfo from rob
   */
  val blockCommit = Wire(Bool())
  io.out.blockRobCommit := blockCommit

  /**
   * stage 1: regNext(robCommitInfo)
   */
  val s1_in = fromRob
  val s1_out = WireInit(0.U.asTypeOf(s1_in))
  for(i <- 0 until CommitWidth) {
    s1_out.blocks(i).valid := RegEnable(s1_in.blocks(i).valid, false.B, !blockCommit)
    s1_out.blocks(i).bits := RegEnable(s1_in.blocks(i).bits, 0.U.asTypeOf(s1_in.blocks(i).bits), s1_in.blocks(i).valid)
  }

  /**
   * stage 2: compress, s2_out(deq from traceBuffer) -> pcMem
   */
  val s2_in = s1_out
  val traceBuffer = Module(new TraceBuffer)
  traceBuffer.io.in.fromEncoder := fromEncoder
  traceBuffer.io.in.fromRob := s2_in
  val s2_out_groups = traceBuffer.io.out.groups
  blockCommit := traceBuffer.io.out.blockCommit

  /**
   * stage 3: groups with iaddr from pcMem(ftqidx & ftqOffset -> iaddr) -> encoder
   */
  val s3_in_groups = s2_out_groups
  val s3_out_groups = RegNext(s3_in_groups)
  toPcMem := s3_in_groups
  io.out.toEncoder := s3_out_groups
}
