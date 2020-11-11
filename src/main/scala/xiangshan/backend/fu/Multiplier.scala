package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend._
import xiangshan.backend.fu.FunctionUnit._

class MulDivCtrl extends Bundle{
  val sign = Bool()
  val isW = Bool()
  val isHi = Bool() // return hi bits of result ?
}

class ArrayMultiplier(len: Int, latency: Int = 3)
  extends FunctionUnit(
    FuConfig(FuType.mul, 2, 0, writeIntRf = true, writeFpRf = false, hasRedirect = false, CertainLatency(latency)),
    len
  )
  with HasPipelineReg
{
  val ctrl = IO(Input(new MulDivCtrl))

  val (src1, src2) = (io.in.bits.src(0), io.in.bits.src(1))

  val mulRes = src1.asSInt() * src2.asSInt()

  var dataVec = Seq(mulRes.asUInt())
  var ctrlVec = Seq(ctrl)

  for(i <- 1 to latency){
    dataVec = dataVec :+ PipelineReg(i)(dataVec(i-1))
    ctrlVec = ctrlVec :+ PipelineReg(i)(ctrlVec(i-1))
  }

  val xlen = io.out.bits.data.getWidth
  val res = Mux(ctrlVec.last.isHi, dataVec.last(2*xlen-1, xlen), dataVec.last(xlen-1,0))
  io.out.bits.data := Mux(ctrlVec.last.isW, SignExt(res(31,0),xlen), res)

  XSDebug(p"validVec:${Binary(Cat(validVec))} flushVec:${Binary(Cat(flushVec))}\n")
}