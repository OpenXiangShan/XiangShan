package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.vector.fu.util.Func._
import xiangshan.backend.vector.fu.util.{VecFixLatFunc, VecFuConfig}
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._
import yunsuan.vector.VectorConvert.VectorCvt

class VCVTWrapper(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {
  private val ex0 = ex(0)
  private val ex0opcode: UInt = fuOpType
  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode

  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule
  private val vlenb = VLEN / 8
  private val zeroFflags = 0.U.asTypeOf(Fflags())
  private val zeroFflagsE8 = VecInit(Seq.fill(vlenb)(zeroFflags))
  private val resStages = cfg.latency + 1
  require(resStages == 3)

  private val ex0NextInWidth = FCvtOpcode.getInputDataWidth(ex0NextOpcode)
  private val ex0NextOutWidth = FCvtOpcode.getOutputDataWidth(ex0NextOpcode)
  private val exInWidth   = makePipeReg(ex0NextInWidth, pipeRegValids)
  private val exOutWidth  = makePipeReg(ex0NextOutWidth, pipeRegValids)

  private val vfcvts = Seq.fill(numVecModule)(Module(new VectorCvt(dataWidthOfDataModule)))
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs2Vec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val inSew1H = Wire(UInt(4.W))
  private val outSew1H = Wire(UInt(4.W))

  vs2Split.io.inVecData := ex0vs2
  vs2Vec := vs2Split.io.outVec64b
  inSew1H := UIntToOH(exInWidth.ex0)
  outSew1H := UIntToOH(exOutWidth.ex0)

  vfcvts.zipWithIndex.foreach {
    case (mod, i) =>
      mod.fire := ex0.valid
      mod.src := vs2Vec(i)
      mod.opType := ex0opcode
      mod.rm := in.frm.get
      mod.inSew1H := inSew1H
      mod.outSew1H := outSew1H
  }

  out.ex(0).bits.data.vec.foreach {
    case vecData =>
      vecData.normal := 0.U
      vecData.narrow := 0.U
      vecData.maskE8 := 0.U
      vecData.maskE16 := 0.U
      vecData.maskE32 := 0.U
      vecData.maskE64 := 0.U
      vecData.fflagsE8.get := zeroFflagsE8
      vecData.narrowFflagsE8.get := zeroFflagsE8
  }

  out.ex(1).bits.data.vec.foreach {
    case vecData =>
      vecData.normal := Cat(vfcvts.map(_.io.resEx1).reverse)
      vecData.narrow := 0.U
      vecData.maskE8 := 0.U
      vecData.maskE16 := 0.U
      vecData.maskE32 := 0.U
      vecData.maskE64 := 0.U
      vecData.fflagsE8.get := vfcvts.flatMap(_.io.fflagsE8Ex1.toSeq)
      vecData.narrowFflagsE8.get := zeroFflagsE8
  }

  out.ex(2).bits.data.vec.foreach {
    case vecData =>
      vecData.normal := Cat(vfcvts.map(_.io.resEx2).reverse)
      vecData.narrow := 0.U
      vecData.maskE8 := 0.U
      vecData.maskE16 := 0.U
      vecData.maskE32 := 0.U
      vecData.maskE64 := 0.U
      vecData.fflagsE8.get := vfcvts.flatMap(_.io.fflagsE8Ex2.toSeq)
      vecData.narrowFflagsE8.get := zeroFflagsE8
  }
}