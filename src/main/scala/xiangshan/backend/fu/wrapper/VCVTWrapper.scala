package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.vector.fu.Func._
import xiangshan.backend.vector.fu.{VecFixLatFunc, VecFuConfig}
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
  private val zeroNarrowFflagsE8 = VecInit(Seq.fill(vlenb / 2)(zeroFflags))
  private val resStages = cfg.latency + 1
  require(resStages == 3)

  private val ex0NextInWidth = FCvtOpcode.getInputDataWidth(ex0NextOpcode)
  private val ex0NextOutWidth = FCvtOpcode.getOutputDataWidth(ex0NextOpcode)
  private val ex0NextInSew1H = UIntToOH(ex0NextInWidth, SewOH.width)
  private val ex0NextOutSew1H = UIntToOH(ex0NextOutWidth, SewOH.width)
  private val ex0NextIsWiden =
      (ex0NextInSew1H(1) && ex0NextOutSew1H(2)) || // 16 -> 32
      (ex0NextInSew1H(2) && ex0NextOutSew1H(3))    // 32 -> 64
  private val ex0NextIsNarrow =
      (ex0NextInSew1H(1) && ex0NextOutSew1H(0)) || // 16 -> 8
      (ex0NextInSew1H(2) && ex0NextOutSew1H(1)) || // 32 -> 16
      (ex0NextInSew1H(3) && ex0NextOutSew1H(2))    // 64 -> 32
  private val opType = makePipeReg(ex0NextOpcode, pipeRegValids)
  private val cvtInSew1H = Seq.fill(numVecModule)(makePipeReg(ex0NextInSew1H, pipeRegValids))
  private val cvtOutSew1H = Seq.fill(numVecModule)(makePipeReg(ex0NextOutSew1H, pipeRegValids))
  private val cvt64UseWidenSrc2 = Seq.fill(numVecModule)(makePipeReg(ex0NextIsWiden, pipeRegValids))
  private val cvt32UseWidenSrc2 = Seq.fill(numVecModule)(makePipeReg(ex0NextInSew1H(1) && ex0NextOutSew1H(2), pipeRegValids))
  private val widenSrcUpperSel = Seq.fill(numVecModule)(makePipeReg(ex0Next.bits.ctrl.uopIdx(0), pipeRegValids))
  private val isWiden     = makePipeReg(ex0NextIsWiden, pipeRegValids)
  private val isNarrow    = makePipeReg(ex0NextIsNarrow, pipeRegValids)

  private val vfcvts = Seq.fill(numVecModule)(Module(new VectorCvt(dataWidthOfDataModule)))
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs2Vec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val widenVs2Vec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val narrowVs2Vec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val narrowVs1Vec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val narrowDataWidth = dataWidthOfDataModule / 2

  vs2Split.io.inVecData := ex0vs2
  vs1Split.io.inVecData := ex0vs1 // vs1 = vs2 + 1
  val narrowSrcChunks = vs2Split.io.outVec64b.toSeq ++ vs1Split.io.outVec64b.toSeq
  for (i <- 0 until numVecModule) {
    val widenVs2Low32 = Mux(widenSrcUpperSel(i).ex0, vs2Split.io.outVec32b(i + numVecModule), vs2Split.io.outVec32b(i))
    vs2Vec(i) := vs2Split.io.outVec64b(i)
    widenVs2Vec(i) := Cat(0.U(32.W), widenVs2Low32)
    narrowVs2Vec(i) := narrowSrcChunks(2 * i)
    narrowVs1Vec(i) := narrowSrcChunks(2 * i + 1)
  }

  vfcvts.zipWithIndex.foreach {
    case (mod, i) =>
      mod.fire := ex0.valid
      mod.src2 := vs2Vec(i)
      mod.widenSrc2 := widenVs2Vec(i)
      mod.narrowSrc2 := narrowVs2Vec(i)
      mod.narrowSrc1 := narrowVs1Vec(i)
      mod.opType := opType.ex0
      mod.rm := in.frm.get
      mod.inSew1H := cvtInSew1H(i).ex0
      mod.outSew1H := cvtOutSew1H(i).ex0
      mod.cvt64UseWidenSrc2 := cvt64UseWidenSrc2(i).ex0
      mod.cvt32UseWidenSrc2 := cvt32UseWidenSrc2(i).ex0
  }

  out.ex(0).bits.data.vec.foreach {
    case vecData =>
      vecData.normal := 0.U
      vecData.narrow := 0.U
      vecData.maskE8 := 0.U
      vecData.maskE16 := 0.U
      vecData.maskE32 := 0.U
      vecData.maskE64 := 0.U
      vecData.isWiden.get := isWiden.ex0
      vecData.isNarrow.get := false.B
      vecData.fflagsE8.get := zeroFflagsE8
      vecData.narrowFflagsE8.get := zeroNarrowFflagsE8
  }

  out.ex(1).bits.data.vec.foreach {
    case vecData =>
      vecData.normal := Cat(vfcvts.map(_.io.resEx1).reverse)
      vecData.narrow := Cat(vfcvts.map(_.io.resEx1.take(narrowDataWidth)).reverse)
      vecData.maskE8 := 0.U
      vecData.maskE16 := 0.U
      vecData.maskE32 := 0.U
      vecData.maskE64 := 0.U
      vecData.isWiden.get := isWiden.ex1
      vecData.isNarrow.get := false.B
      vecData.fflagsE8.get := Mux(
        isNarrow.ex1,
        VecInit(vfcvts.flatMap(_.io.narrowFflagsE8Ex1.toSeq)),
        VecInit(vfcvts.flatMap(_.io.fflagsE8Ex1.toSeq))
      )
      vecData.narrowFflagsE8.get := zeroNarrowFflagsE8
  }

  out.ex(2).bits.data.vec.foreach {
    case vecData =>
      vecData.normal := Cat(vfcvts.map(_.io.resEx2).reverse)
      vecData.narrow := Cat(vfcvts.map(_.io.resEx2.take(narrowDataWidth)).reverse)
      vecData.maskE8 := 0.U
      vecData.maskE16 := 0.U
      vecData.maskE32 := 0.U
      vecData.maskE64 := 0.U
      vecData.isWiden.get := isWiden.ex2
      vecData.isNarrow.get := false.B
      vecData.fflagsE8.get := Mux(
        isNarrow.ex2,
        VecInit(vfcvts.flatMap(_.io.narrowFflagsE8Ex2.toSeq)),
        VecInit(vfcvts.flatMap(_.io.fflagsE8Ex2.toSeq))
      )
      vecData.narrowFflagsE8.get := zeroNarrowFflagsE8
  }
}
