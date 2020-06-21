package xiangshan.backend.roq

import chisel3._
import chisel3.util._
import xiangshan._


class Roq extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val brqRedirect = Input(Valid(new Redirect))
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val roqIdxs = Output(Vec(RenameWidth, UInt(ExtendedRoqIdxWidth.W)))
    val redirect = Output(Valid(new Redirect))
    val exeWbResults = Vec(exuConfig.ExuCnt, Flipped(DecoupledIO(new ExuOutput)))
    val commits = Vec(CommitWidth, Valid(new RoqCommit))
  })

  val microOp = Reg(Vec(RoqSize, new MicroOp))
  // val brMask = Reg(Vec(RoqSize, UInt(BrqSize.W)))
  val valid = RegInit(VecInit(List.fill(RoqSize)false.B))
  val writebacked = Reg(Vec(RoqSize, Bool()))
  val redirect = Reg(Vec(RoqSize, new Redirect))
  val isMMIO = Reg(Vec(RoqSize, Bool()))//for debug
  val intrNO = Reg(Vec(RoqSize, UInt(XLEN.W)))//for debug

  val ringBufferHeadExtended = RegInit(0.U(ExtendedRoqIdxWidth.W))
  val ringBufferTailExtended = RegInit(0.U(ExtendedRoqIdxWidth.W))
  val ringBufferHead = ringBufferHead(RoqIdxWidth-1,0)
  val ringBufferTail = ringBufferTail(RoqIdxWidth-1,0)
  val ringBufferEmpty = ringBufferHead === ringBufferTail && ringBufferHead(RoqIdxWidth)===ringBufferTail(RoqIdxWidth)
  val ringBufferEmpty = ringBufferHead === ringBufferTail && ringBufferHead(RoqIdxWidth)=/=ringBufferTail(RoqIdxWidth)
  val ringBufferAllowin = !ringBufferFull 

  // Dispatch
  for(i <- 0 until RenameWidth){
    when(dp1Req(i).fire()){
      microOp(ringBufferHead+i) := io.dp1Req(i).bits
      valid(ringBufferHead+i) := true.B
    }
    io.dp1Req(i).ready := ringBufferAllowin && !vaild(ringBufferHead+i)
    io.roqIdxs(i) := ringBufferHeadExtended+i
  }

  // Writeback
  for(i <- 0 until exuConfig.ExuCnt){
    when(exeWbResults(i).fire()){
      writebacked(io.exeWbResults(i).bits.uop.roqIdx) := true.B
    }
  }

  // Commit
  // TODO
  // for(i <- 0 until CommitWidth){
  //   io.commits.valid := valid(ringBufferTail+i) && writebacked(ringBufferTail+i)
  //   io.commits.bits.uop := microOp(ringBufferTail+i)
  //   io.commits.bits.isWalk := DontCare //TODO
  // }
  // io.redirect := DontCare //TODO
  // io.redirect.valid := false.B //TODO

  // Flush
  // TODO

}
