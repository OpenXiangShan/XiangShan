package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.{FuConfig, FuncUnit, HasPipelineReg}
import yunsuan.scalar.Mul
import yunsuan.MULOpType

class MulUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) with HasPipelineReg {
  override def latency: Int = 2

  private val len = cfg.destDataBits

  private val isMulw7 = MULOpType.isMulw7(io.in.bits.ctrl.fuOpType)
  private val mulw7Src = io.in.bits.data.src(0)(6, 0)
  private val src0 = Wire(UInt(XLEN.W))
  src0 := Mux(isMulw7, mulw7Src, io.in.bits.data.src(0))

  private val mulModule = Module(new Mul(len))
  mulModule.io.in.valid := io.in.valid
  mulModule.io.in.bits.fuOpType := io.in.bits.ctrl.fuOpType
  mulModule.io.in.bits.src(0) := src0
  mulModule.io.in.bits.src(1) := io.in.bits.data.src(1)

  private val result = Wire(UInt(len.W))
  result := mulModule.io.out

  io.out.bits.res.data := result
}
