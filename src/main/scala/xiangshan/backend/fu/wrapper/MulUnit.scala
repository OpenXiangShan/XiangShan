package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import utility.{LookupTree, SignExt, ZeroExt}
import xiangshan.MULOpType
import xiangshan.backend.fu.{ArrayMulDataModule, FuncUnit, HasPipelineReg, MulDivCtrl}
import xiangshan.backend.fu.FuConfig

class MulUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with HasPipelineReg
{
  override def latency: Int = 2

  private val xlen = cfg.dataBits

  val func = io.in.bits.ctrl.fuOpType
  val src = io.in.bits.data.src

  val op = MULOpType.getOp(func)

  private val mdCtrl = Wire(new MulDivCtrl)
  mdCtrl.isW := MULOpType.isW(func)
  mdCtrl.isHi := MULOpType.isH(func)
  mdCtrl.sign := DontCare

  val sext = SignExt(_: UInt, xlen + 1)
  val zext = ZeroExt(_: UInt, xlen + 1)
  val mulInputCvtTable: Seq[(UInt, (UInt => UInt, UInt => UInt))] = List(
    MULOpType.getOp(MULOpType.mul)    -> (zext, zext),
    MULOpType.getOp(MULOpType.mulh)   -> (sext, sext),
    MULOpType.getOp(MULOpType.mulhsu) -> (sext, zext),
    MULOpType.getOp(MULOpType.mulhu)  -> (zext, zext),
    MULOpType.getOp(MULOpType.mulw7)  -> (_.asUInt(6, 0), zext),
  )

  // len should be xlen + 1
  private val len = cfg.dataBits + 1
  private val mulDataModule = Module(new ArrayMulDataModule(len))

  mulDataModule.io.a := LookupTree(
    op,
    mulInputCvtTable.map { case (k, v) => (k, v._1(src(0))) }
  )

  mulDataModule.io.b := LookupTree(
    op,
    mulInputCvtTable.map { case (k, v) => (k, v._2(src(1))) }
  )

  mulDataModule.io.regEnables := VecInit((1 to latency) map (i => regEnable(i)))
  private val result = mulDataModule.io.result

  private var mdCtrlVec = Seq(mdCtrl)
  for (i <- 1 to latency) {
    mdCtrlVec = mdCtrlVec :+ PipelineReg(i)(mdCtrlVec(i - 1))
  }
  private val res = Mux(mdCtrlVec.last.isHi, result(2 * xlen - 1, xlen), result(xlen - 1, 0))

  io.out.bits.res.data := Mux(mdCtrlVec.last.isW, SignExt(res(31, 0), xlen), res)
}
