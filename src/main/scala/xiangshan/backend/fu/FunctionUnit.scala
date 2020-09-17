package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import FunctionUnit._

/*
    XiangShan Function Unit
    A Exu can have one or more function units
 */

case class FuConfig
(
  fuType: UInt,
  numIntSrc: Int,
  numFpSrc: Int,
  writeIntRf: Boolean,
  writeFpRf: Boolean,
  hasRedirect: Boolean
) {
  def srcCnt: Int = math.max(numIntSrc, numFpSrc)
}

class FunctionUnitIO[TI <: Data, TO <: Data]
(
  cfg: FuConfig,
  len: Int,
  extIn: => TI = null,
  extOut: => TO = null
) extends XSBundle
{
  val in = Flipped(DecoupledIO(new Bundle() {
    val src = Vec(cfg.srcCnt, UInt(len.W))
    val uop = new MicroOp
    val ext = if(extIn == null) None else Some(extIn.cloneType)

    def connectToExuInput(exuIn: ExuInput): Unit = {
      val exuSrcIn = Seq(exuIn.src1, exuIn.src2, exuIn.src3)
      src.zip(exuSrcIn).foreach{case (x, y) => x := y}
      uop := exuIn.uop
    }
  }))

  val out = DecoupledIO(new Bundle() {
    val data = UInt(XLEN.W)
    val uop = new MicroOp
    val ext = if(extOut == null) None else Some(extOut.cloneType)
  })

  val redirectIn = Flipped(ValidIO(new Redirect))

  override def cloneType: FunctionUnitIO.this.type =
    new FunctionUnitIO(cfg, len, extIn, extOut).asInstanceOf[this.type]
}

abstract class FunctionUnit[TI <: Data, TO <: Data]
(
  cfg: FuConfig,
  len: Int = 64,
  extIn: => TI = null,
  extOut: => TO = null,
  val latency: Int = 0
) extends XSModule {

  val io = IO(new FunctionUnitIO[TI, TO](cfg, len, extIn, extOut))

}

trait HasPipelineReg[TI <: Data, TO <: Data] {
  this: FunctionUnit[TI, TO] =>

  val validVec = io.in.valid +: Array.fill(latency)(RegInit(false.B))
  val rdyVec = Array.fill(latency)(Wire(Bool())) :+ io.out.ready
  val uopVec = io.in.bits.uop +: Array.fill(latency)(Reg(new MicroOp))


  val flushVec = uopVec.zip(validVec).map(x => x._2 && x._1.needFlush(io.redirectIn))

  for (i <- 0 until latency) {
    rdyVec(i) := !validVec(i + 1) || rdyVec(i + 1)
  }

  for (i <- 1 to latency) {
    when(flushVec(i - 1) || rdyVec(i) && !validVec(i - 1)) {
      validVec(i) := false.B
    }.elsewhen(rdyVec(i - 1) && validVec(i - 1) && !flushVec(i - 1)) {
      validVec(i) := validVec(i - 1)
      uopVec(i) := uopVec(i - 1)
    }
  }

  io.in.ready := rdyVec(0)
  io.out.valid := validVec.last && !flushVec.last
  io.out.bits.uop := uopVec.last

  def PipelineReg[TT <: Data](i: Int)(next: TT) = RegEnable(
    next,
    enable = validVec(i - 1) && rdyVec(i - 1) && !flushVec(i - 1)
  )

  def S1Reg[TT <: Data](next: TT): TT = PipelineReg[TT](1)(next)

  def S2Reg[TT <: Data](next: TT): TT = PipelineReg[TT](2)(next)

  def S3Reg[TT <: Data](next: TT): TT = PipelineReg[TT](3)(next)

  def S4Reg[TT <: Data](next: TT): TT = PipelineReg[TT](4)(next)

  def S5Reg[TT <: Data](next: TT): TT = PipelineReg[TT](5)(next)
}

object FunctionUnit {

  val csrCfg =
    FuConfig(FuType.csr, 1, 0, writeIntRf = true, writeFpRf = false, hasRedirect = false)

  val jmpCfg =
    FuConfig(FuType.jmp, 1, 0, writeIntRf = true, writeFpRf = false, hasRedirect = true)

  val i2fCfg =
    FuConfig(FuType.i2f, 1, 0, writeIntRf = false, writeFpRf = true, hasRedirect = false)

  val aluCfg =
    FuConfig(FuType.alu, 2, 0, writeIntRf = true, writeFpRf = false, hasRedirect = true)

  val mulCfg =
    FuConfig(FuType.mul, 2, 0, writeIntRf = true, writeFpRf = false, hasRedirect = false)

  val divCfg =
    FuConfig(FuType.div, 2, 0, writeIntRf = true, writeFpRf = false, hasRedirect = false)

  val fenceCfg = 
    FuConfig(FuType.fence, 2, 0, writeIntRf = false, writeFpRf = false, hasRedirect = false/*NOTE: need redirect but when commit*/)

  val lduCfg =
    FuConfig(FuType.ldu, 1, 0, writeIntRf = true, writeFpRf = true, hasRedirect = false)

  val stuCfg =
    FuConfig(FuType.stu, 2, 1, writeIntRf = false, writeFpRf = false, hasRedirect = false)

  // use ldu's write back port, so set writeIntRf to false
  val mouCfg =
    FuConfig(FuType.mou, 2, 0, writeIntRf = false, writeFpRf = false, hasRedirect = false)

  val fmacCfg =
    FuConfig(FuType.fmac, 0, 3, writeIntRf = false, writeFpRf = true, hasRedirect = false)

  val fmiscCfg =
    FuConfig(FuType.fmisc, 0, 2, writeIntRf = false, writeFpRf = true, hasRedirect = false)

  val fDivSqrtCfg =
    FuConfig(FuType.fDivSqrt, 0, 2, writeIntRf = false, writeFpRf = true, hasRedirect = false)
}
