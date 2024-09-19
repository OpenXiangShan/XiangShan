package xiangshan.backend.trace

import chisel3._
import chisel3.util.{RegEnable, ValidIO, log2Up}
import org.chipsalliance.cde.config.Parameters
import xiangshan.HasXSParameter

class TraceParams(
 val TraceGroupNum         : Int,
 val HasEncoder            : Boolean,
 val TraceEnable           : Boolean,
)

class TraceIO(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val fromEncoder    = Input(new FromEncoder)
  val fromRob        = Flipped(new TraceBundle(hasIaddr = false, CommitWidth, IretireWidthInPipe))
  val blockRobCommit = Output(Bool())
  val toPcMem        = Vec(TraceGroupNum, ValidIO(new TraceBlock(false, IretireWidthCompressed)))
  val fromPcMem      = Input(Vec(TraceGroupNum, UInt(IaddrWidth.W)))
  val toEncoder      = new TraceBundle(hasIaddr = true, TraceGroupNum, IretireWidthCompressed)
}

class Trace(implicit val p: Parameters) extends Module with HasXSParameter {
  val io = IO(new TraceIO)
  val (fromEncoder, fromRob, toPcMem, fromPcMem, toEncoder) = (io.fromEncoder, io.fromRob, io.toPcMem, io.fromPcMem, io.toEncoder)

  /**
   * stage 0: CommitInfo from rob
   */
  val blockCommit = Wire(Bool())
  io.blockRobCommit := blockCommit

  /**
   * stage 1: regNext(robCommitInfo)
   */
  val s1_in = fromRob
  val s1_out = WireInit(0.U.asTypeOf(s1_in))

  for(i <- 0 until CommitWidth) {
    if(i == 0){
      s1_out.trap := RegEnable(s1_in.trap, s1_in.blocks(i).valid)
    }
    s1_out.blocks(i).valid := RegEnable(s1_in.blocks(i).valid, !blockCommit)
    s1_out.blocks(i).bits := RegEnable(s1_in.blocks(i).bits, s1_in.blocks(i).valid)
  }

  /**
   * stage 2: compress, s2_out(deq from traceBuffer) -> pcMem
   */
  val s2_in = s1_out
  val traceBuffer = Module(new TraceBuffer)
  traceBuffer.io.in.fromEncoder := fromEncoder
  traceBuffer.io.in.fromRob := s2_in
  val s2_out_trap = traceBuffer.io.out.groups.trap
  val s2_out_block = traceBuffer.io.out.groups.blocks
  blockCommit := traceBuffer.io.out.blockCommit

  /**
   * stage 3: groups with iaddr from pcMem(ftqidx & ftqOffset -> iaddr) -> encoder
   */
  val s3_in_trap = s2_out_trap
  val s3_in_block = s2_out_block

  val s3_out_trap  = RegNext(s3_in_trap)
  val s3_out_block = RegNext(s3_in_block)

  toPcMem := s3_in_block

  io.toEncoder := DontCare
  for(i <- 0 until TraceGroupNum) {
    toEncoder.trap := s3_out_trap
    toEncoder.blocks(i).bits.iaddr.foreach(_ := fromPcMem(i))
    toEncoder.blocks(i).valid := s3_out_block(i).valid
    toEncoder.blocks(i).bits.tracePipe := s3_out_block(i).bits.tracePipe
  }
  if(backendParams.debugEn){
    dontTouch(io.toEncoder)
  }
}






