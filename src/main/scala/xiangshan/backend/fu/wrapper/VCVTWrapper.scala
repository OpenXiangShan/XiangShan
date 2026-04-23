package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.vector.fu.util.Func._
import xiangshan.backend.vector.fu.util.{VecFixLatFunc, VecFuConfig}
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._
import yunsuan.vector.VectorConvert.VectorConvert

class VCVTWrapper(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {
  private val ex0 = ex(0)
  private val ex1 = ex(1)
  private val ex2 = ex(2)
  private val ex0opcode: UInt = fuOpType
  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode

  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val minElemBits = 16
  private val numVecModule = dataWidth / dataWidthOfDataModule
  private val fflagsLaneNum = dataWidth / minElemBits
  private val vlenb = VLEN / 8
  private val zeroFflags = 0.U.asTypeOf(Fflags())
  private val zeroFflagsE8 = VecInit(Seq.fill(vlenb)(zeroFflags))
  private val resultStages = cfg.latency + 1
  require(resultStages == 3)

  private val ex0NextInWidth = FCvtOpcode.getInputDataWidth(ex0NextOpcode)
  private val ex0NextOutWidth = FCvtOpcode.getOutputDataWidth(ex0NextOpcode)
  private val exInWidth     = makePipeReg(ex0NextInWidth, pipeRegValids)
  private val exOutWidth    = makePipeReg(ex0NextOutWidth, pipeRegValids)
  private val exIsWidenCvt  = makePipeReg(ex0NextOutWidth > ex0NextInWidth, pipeRegValids)
  private val exIsNarrowCvt = makePipeReg(ex0NextOutWidth < ex0NextInWidth, pipeRegValids)

  private def genExResult(exCtrl: InCtrl, exData: InData, exNarrow: Bool, rawResult: UInt): UInt = {
    val exNarrowNeedCat = exCtrl.uopIdx(0).asBool
    val narrowResult = Mux(
      exNarrowNeedCat,
      Cat(rawResult(dataWidth / 2 - 1, 0), exData.src(2)(dataWidth / 2 - 1, 0)),
      Cat(exData.src(2)(dataWidth - 1, dataWidth / 2), rawResult(dataWidth / 2 - 1, 0))
    )

    Mux(exNarrow, narrowResult, rawResult)
  }

  private def genExFFlagsE8(exOutWidth: UInt, fflagsData: Vec[UInt]): Vec[UInt] = {
    VecInit.tabulate(vlenb) { i =>
      Mux1H(Seq(
        (exOutWidth === VSew.e8)  -> (if (i < fflagsLaneNum) fflagsData(i) else zeroFflags),
        (exOutWidth === VSew.e16) -> fflagsData(i / 2),
        (exOutWidth === VSew.e32) -> fflagsData(i / 4),
        (exOutWidth === VSew.e64) -> fflagsData(i / 8),
      ))
    }
  }

  private val vfcvt = Module(new VectorConvert(dataWidth, dataWidthOfDataModule))
  private val vs2Vec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val inSew1H = Wire(UInt(4.W))
  private val outSew1H = Wire(UInt(4.W))

  vs2Vec := ex0vs2.asTypeOf(vs2Vec)
  inSew1H := UIntToOH(exInWidth.ex0)
  outSew1H := UIntToOH(exOutWidth.ex0)

  vfcvt.in.fire := ex0.valid
  vfcvt.in.ctrl.uopIdx := ex0uopIdx(0)
  vfcvt.in.data.src := vs2Vec
  vfcvt.in.ctrl.opType := ex0opcode
  vfcvt.in.ctrl.rm := in.frm.get
  vfcvt.in.ctrl.inSew1H := inSew1H
  vfcvt.in.ctrl.outSew1H := outSew1H
  vfcvt.in.ctrl.isWiden := exIsWidenCvt.ex0
  vfcvt.in.ctrl.isNarrow := exIsNarrowCvt.ex0

  private val resultEx1 = genExResult(ex1.bits.ctrl, ex1.bits.data, exIsNarrowCvt.ex1, vfcvt.out.ex1.result)
  private val resultEx2 = genExResult(ex2.bits.ctrl, ex2.bits.data, exIsNarrowCvt.ex2, vfcvt.out.ex2.result)
  private val fflagsEx1 = genExFFlagsE8(exOutWidth.ex1, vfcvt.out.ex1.fflags)
  private val fflagsEx2 = genExFFlagsE8(exOutWidth.ex2, vfcvt.out.ex2.fflags)

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
      vecData.normal := resultEx1
      vecData.narrow := 0.U
      vecData.maskE8 := 0.U
      vecData.maskE16 := 0.U
      vecData.maskE32 := 0.U
      vecData.maskE64 := 0.U
      vecData.fflagsE8.get := fflagsEx1
      vecData.narrowFflagsE8.get := zeroFflagsE8
  }

  out.ex(2).bits.data.vec.foreach {
    case vecData =>
      vecData.normal := resultEx2
      vecData.narrow := 0.U
      vecData.maskE8 := 0.U
      vecData.maskE16 := 0.U
      vecData.maskE32 := 0.U
      vecData.maskE64 := 0.U
      vecData.fflagsE8.get := fflagsEx2
      vecData.narrowFflagsE8.get := zeroFflagsE8
  }
}