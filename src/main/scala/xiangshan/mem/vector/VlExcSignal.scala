package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._

class vlExcSignalBundle(implicit p: Parameters) extends XSBundle{
  val vecloadRegIn = Vec(2,Decoupled(new VecOperand()))
  val vecwriteback = Vec(2,Flipped(Decoupled(new VecWriteback)))
  val vecFeedback = Vec(2,Input(Bool()))
}


class VlExcSignal(implicit p: Parameters) extends XSModule{
  val io = IO(new vlExcSignalBundle)

  val loadRegIn = Wire(Vec(2,new VecOperand()))
  val loadRegIn_valid = Wire(Vec(2,Bool()))

  for(i <- 0 until LoadPipelineWidth){
    loadRegIn(i).uop       := DontCare
    loadRegIn_valid(i)     := LFSR64(seed=Some(123L))(3,0) === 0.U
    loadRegIn(i).vmask     := LFSR64(seed = Some(123L))(63,0)
    loadRegIn(i).baseaddr  := 0x80000000L.U + LFSR64(seed = Some(123L))(8,0) << 3
    loadRegIn(i).stride    := LFSR64(seed = Some(123L))(XLEN-1,0)
    loadRegIn(i).index     := LFSR64(seed = Some(123L))(63,0)
    loadRegIn(i).pvd       := LFSR64(seed = Some(123L))(4,0)
    loadRegIn(i).lmul      := LFSR64(seed = Some(123L))(2,0)
    loadRegIn(i).sew       := LFSR64(seed = Some(123L))(1,0)
    loadRegIn(i).vma       := LFSR64(seed = Some(123L))(5,0) === 0.U
    loadRegIn(i).vta       := LFSR64(seed = Some(123L))(4,0) === 0.U
    loadRegIn(i).inner_idx := LFSR64(seed = Some(123L))(2,0)
    loadRegIn(i).vl        := LFSR64(seed = Some(123L))(7,0)
    loadRegIn(i).total_num := LFSR64(seed = Some(123L))(3,0)
    loadRegIn(i).uop.robIdx.value := LFSR64(seed = Some(123L))(7,0)
    loadRegIn(i).uop.cf.instr     := LFSR64(seed = Some(123L))(32,0)
    loadRegIn(i).uop.pdest        := LFSR64(seed = Some(123L))(PhyRegIdxWidth-1,0)

  }

  for(i <- 0 until LoadPipelineWidth){
    io.vecloadRegIn(i).valid := DelayN(loadRegIn_valid(i),1)
    io.vecloadRegIn(i).bits  := DelayN(loadRegIn(i),1)
  }
  io.vecwriteback.map(_.ready := LFSR64(seed=Some(123L))(3,0) === 0.U)
}