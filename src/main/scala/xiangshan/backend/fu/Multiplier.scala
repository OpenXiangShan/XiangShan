package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import xiangshan.backend._
import xiangshan.backend.fu.FunctionUnit._

class MulDivCtrl extends Bundle{
  val uop = new MicroOp
  val sign = Bool()
  val isW = Bool()
  val isHi = Bool() // return hi bits of result ?
}

class MulDivOutput extends XSBundle {
  val data = UInt(XLEN.W)
  val uop = new MicroOp
}

class MulDivIO(val len: Int) extends XSBundle {
  val in = Flipped(DecoupledIO(new Bundle() {
    val src1, src2 = UInt(len.W)
    val ctrl = new MulDivCtrl
  }))
  val out = DecoupledIO(new MulDivOutput)
  val redirect = Flipped(ValidIO(new Redirect))
}

abstract class Multiplier
(
  val len: Int,
  val latency: Int = 3
) extends FunctionUnit(mulCfg) {
  val io = IO(new MulDivIO(len))
}

trait HasPipelineReg { this: ArrayMultiplier =>

  val validVec = io.in.valid +: Array.fill(latency)(RegInit(false.B))
  val rdyVec = Array.fill(latency)(Wire(Bool())) :+ io.out.ready
  val ctrlVec = io.in.bits.ctrl +: Array.fill(latency)(Reg(new MulDivCtrl))
  val flushVec = ctrlVec.zip(validVec).map(x => x._2 && x._1.uop.brTag.needFlush(io.redirect))

  for(i <- 0 until latency){
    rdyVec(i) := !validVec(i+1) || rdyVec(i+1)
  }

  for(i <- 1 to latency){
    when(flushVec(i) || rdyVec(i) && !validVec(i-1)){
      validVec(i) := false.B
    }.elsewhen(rdyVec(i-1) && validVec(i-1) && !flushVec(i-1)){
      validVec(i) := validVec(i-1)
      ctrlVec(i) := ctrlVec(i-1)
    }
  }

  io.in.ready := rdyVec(0)
  io.out.valid := validVec.last && !flushVec.last
  io.out.bits.uop := ctrlVec.last.uop

  def PipelineReg[T<:Data](i: Int)(next: T) = RegEnable(
    next,
    enable = validVec(i-1) && rdyVec(i-1) && !flushVec(i-1)
  )

  def S1Reg[T<:Data](next: T):T = PipelineReg[T](1)(next)
  def S2Reg[T<:Data](next: T):T = PipelineReg[T](2)(next)
  def S3Reg[T<:Data](next: T):T = PipelineReg[T](3)(next)
  def S4Reg[T<:Data](next: T):T = PipelineReg[T](4)(next)
  def S5Reg[T<:Data](next: T):T = PipelineReg[T](5)(next)
}

class ArrayMultiplier
(
  len: Int,
  latency: Int = 3,
  realArray: Boolean = false
) extends Multiplier(len, latency) with HasPipelineReg {

  val mulRes = io.in.bits.src1.asSInt() * io.in.bits.src2.asSInt()

  var dataVec = Seq(mulRes.asUInt())

  for(i <- 1 to latency){
    dataVec = dataVec :+ PipelineReg(i)(dataVec(i-1))
  }

  val xlen = io.out.bits.data.getWidth
  val res = Mux(ctrlVec.last.isHi, dataVec.last(2*xlen-1, xlen), dataVec.last(xlen-1,0))
  io.out.bits.data := Mux(ctrlVec.last.isW, SignExt(res(31,0),xlen), res)

  XSDebug(p"validVec:${Binary(Cat(validVec))} flushVec:${Binary(Cat(flushVec))}\n")(this.name)

  //  printf(p"t=${GTimer()} in: v${io.in.valid} r:${io.in.ready}\n")
  //  printf(p"t=${GTimer()} out: v:${io.out.valid} r:${io.out.ready} vec:${Binary(Cat(validVec))}\n")
}