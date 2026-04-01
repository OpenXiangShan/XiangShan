package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.{Mgu, VecPipedFuncUnit}
import xiangshan.ExceptionNO
import xiangshan.FuOpType
import yunsuan.VfpuType
import yunsuan.vector.VectorConvert.VectorCvt
import yunsuan.util._
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._


class VCVT(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfcvt OpType not supported")

  // params alias
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // io alias
  private val vfcvtRm = rm

  private val lmul = vlmul // -3->3 => 1/8 ->8

  val widen = 0.U // 0->single 1->widen 2->norrow => width of result
  val isWidenCvt = !widen(1) & widen(0)
  val isNarrowCvt = widen(1) & !widen(0)
  val fire = io.in.valid
  val fireReg = GatedValidRegNext(fire)

  // input width 8, 16, 32, 64
  val inSew1H = Wire(UInt(4.W))
  inSew1H := chisel3.util.experimental.decode.decoder(
    FCvtOpcode.getInputDataWidth(fuOpType),
    TruthTable(
      Seq(
        BitPat("b00") -> BitPat("b0001"), // 8
        BitPat("b01") -> BitPat("b0010"), // 16
        BitPat("b10") -> BitPat("b0100"), // 32
        BitPat("b11") -> BitPat("b1000"), // 64
      ),
      BitPat.N(4)
    )
  )

  // output width 8， 16， 32， 64
  val outSew1H = Wire(UInt(4.W))
  outSew1H := chisel3.util.experimental.decode.decoder(
    FCvtOpcode.getOutputDataWidth(fuOpType),
    TruthTable(
      Seq(
        BitPat("b00") -> BitPat("b0001"), // 8
        BitPat("b01") -> BitPat("b0010"), // 16
        BitPat("b10") -> BitPat("b0100"), // 32
        BitPat("b11") -> BitPat("b1000"), // 64
      ),
      BitPat.N(4)
    )
  )
  if(backendParams.debugEn) {
    dontTouch(inSew1H)
    dontTouch(outSew1H)
  }

  val outEew = RegEnable(RegEnable(Mux1H(outSew1H, Seq(0,1,2,3).map(i => i.U)), fire), fireReg)

  // modules
  private val vfcvt = Module(new VectorCvtTop(dataWidth, dataWidthOfDataModule))
  private val mgu = Module(new Mgu(dataWidth))

  val vs2Vec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  vs2Vec := vs2.asTypeOf(vs2Vec)

  /**
   * [[vfcvt]]'s in connection
   */
  vfcvt.fire     := fire
  vfcvt.uopIdx   := vuopIdx(0)
  vfcvt.src      := vs2Vec
  vfcvt.opType   := fuOpType
  vfcvt.rm       := vfcvtRm
  vfcvt.inSew1H  := inSew1H
  vfcvt.outSew1H := outSew1H
  vfcvt.isWiden  := isWidenCvt
  vfcvt.isNarrow := isNarrowCvt
  val vfcvtResult = vfcvt.io.result
  val vfcvtFflags = vfcvt.io.fflags

  /** fflags:
   */
  // val eNum1H = chisel3.util.experimental.decode.decoder(sew ## (isWidenCvt || isNarrowCvt),
  //   TruthTable(
  //     Seq(                     // 8, 4, 2, 1
  //       BitPat("b001") -> BitPat("b1000"), //8
  //       BitPat("b010") -> BitPat("b1000"), //8
  //       BitPat("b011") -> BitPat("b0100"), //4
  //       BitPat("b100") -> BitPat("b0100"), //4
  //       BitPat("b101") -> BitPat("b0010"), //2
  //       BitPat("b110") -> BitPat("b0010"), //2
  //     ),
  //     BitPat.N(4)
  //   )
  // )
  val eNum1H = 0.U
  val eNum1HEffect = Mux(isWidenCvt || isNarrowCvt, eNum1H << 1, eNum1H)
  // val eNumMax1H = Mux(lmul.head(1).asBool, eNum1HEffect >> ((~lmul.tail(1)).asUInt +1.U), eNum1HEffect << lmul.tail(1)).asUInt(6, 0)
  val eNumMax1H = 0.U
  val eNumMax = Mux1H(eNumMax1H, Seq(1,2,4,8,16,32,64).map(i => i.U)) //only for cvt intr, don't exist 128 in cvt
  val vlForFflags = Mux(vecCtrl.fpu.isFpToVecInst, 1.U, vl)
  val eNumEffectIdx = Mux(vlForFflags > eNumMax, eNumMax, vlForFflags)

  val eNum = Mux1H(eNum1H, Seq(1, 2, 4, 8).map(num =>num.U))
  val eStart = vuopIdx * eNum
  val maskForFflags = srcMask
  val maskPart = maskForFflags >> eStart
  val mask =  Mux1H(eNum1H, Seq(1, 2, 4, 8).map(num => maskPart(num-1, 0)))
  val fflagsEn = Wire(Vec(4 * numVecModule, Bool()))

  fflagsEn := mask.asBools.zipWithIndex.map{case(mask, i) => mask & (eNumEffectIdx > eStart + i.U) }

  val fflagsEnCycle2 = RegEnable(RegEnable(fflagsEn, fire), fireReg)
  val fflagsAll = Wire(Vec(8, UInt(5.W)))
  fflagsAll := vfcvtFflags.asTypeOf(fflagsAll)
  val fflags = fflagsEnCycle2.zip(fflagsAll).map{case(en, fflag) => Mux(en, fflag, 0.U(5.W))}.reduce(_ | _)
  io.out.bits.res.fflags.get := fflags


  /**
   * [[mgu]]'s in connection
   */
  val resultDataUInt = Wire(UInt(dataWidth.W))
  resultDataUInt := vfcvtResult

  private val narrow = RegEnable(RegEnable(isNarrowCvt, fire), fireReg)
  private val narrowNeedCat = outVecCtrl.vuopIdx(0).asBool && narrow
  private val outNarrowVd = Mux(narrowNeedCat, Cat(resultDataUInt(dataWidth / 2 - 1, 0), outOldVd(dataWidth / 2 - 1, 0)),
                                               Cat(outOldVd(dataWidth - 1, dataWidth / 2), resultDataUInt(dataWidth / 2 - 1, 0)))

  // mgu.io.in.vd := resultDataUInt
  mgu.io.in.vd := Mux(narrow, outNarrowVd, resultDataUInt)
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := outSrcMask
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := outVl
  mgu.io.in.info.vlmul := outVecCtrl.vlmul
  mgu.io.in.info.valid := io.out.valid
  mgu.io.in.info.vstart := outVecCtrl.vstart
  mgu.io.in.info.eew := outEew
  mgu.io.in.info.vsew := outVecCtrl.vsew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.narrow := narrow
  mgu.io.in.info.dstMask := outVecCtrl.isDstMask
  mgu.io.in.isIndexedVls := false.B

  io.out.bits.res.data := mgu.io.out.vd

}

class VectorCvtTopIO(vlen: Int, xlen: Int) extends Bundle{
  val fire     = Input(Bool())
  val uopIdx   = Input(Bool())
  val src      = Input(Vec(vlen / xlen, UInt(xlen.W)))
  val opType   = Input(FCvtOpcode())
  val rm       = Input(Frm())
  val inSew1H  = Input(Sew())
  val outSew1H = Input(Sew())
  val isWiden  = Input(Bool())
  val isNarrow = Input(Bool())

  val result = Output(UInt(vlen.W))
  val fflags = Output(Vec(vlen/16, Fflags()))
}



//according to uopindex, 1: high64 0:low64
class VectorCvtTop(vlen: Int, xlen: Int) extends Module{
  val io = IO(new VectorCvtTopIO(vlen, xlen))

  val (fire, uopIdx, src, opType, rm, inSew1H, outSew1H, isWiden, isNarrow) = (
    io.fire, io.uopIdx, io.src, io.opType, io.rm, io.inSew1H, io.outSew1H, io.isWiden, io.isNarrow
  )
  val fireReg = GatedValidRegNext(fire)

  val in0 = Mux(isWiden,
    Mux(uopIdx, src(1)(31, 0), src(0)(31, 0)),
    src(0)
  )

  val in1 = Mux(isWiden,
    Mux(uopIdx, src(1)(63, 32), src(0)(63, 32)),
    src(1)
  )

  val vectorCvt0 = Module(new VectorCvt(xlen))
  vectorCvt0.fire     := fire
  vectorCvt0.src      := in0
  vectorCvt0.opType   := opType
  vectorCvt0.rm       := rm
  vectorCvt0.inSew1H  := inSew1H
  vectorCvt0.outSew1H := outSew1H

  val vectorCvt1 = Module(new VectorCvt(xlen))
  vectorCvt1.fire     := fire
  vectorCvt1.src      := in1
  vectorCvt1.opType   := opType
  vectorCvt1.rm       := rm
  vectorCvt1.inSew1H  := inSew1H
  vectorCvt1.outSew1H := outSew1H

  val isNarrowCycle2 = RegEnable(RegEnable(isNarrow, fire), fireReg)
  val outSew1HCycle2 = RegEnable(RegEnable(outSew1H, fire), fireReg)

  //cycle2
  io.result := Mux(isNarrowCycle2,
    vectorCvt1.io.result(31, 0) ## vectorCvt0.io.result(31, 0),
    vectorCvt1.io.result ## vectorCvt0.io.result)

  val fflags = Mux1H(outSew1HCycle2, Seq(
    vectorCvt1.io.fflags.asUInt ## vectorCvt0.io.fflags.asUInt,
    Mux(isNarrowCycle2, vectorCvt1.io.fflags.asUInt.tail(10) ## vectorCvt0.io.fflags.asUInt.tail(10), vectorCvt1.io.fflags.asUInt ## vectorCvt0.io.fflags.asUInt),
    Mux(isNarrowCycle2, vectorCvt1.io.fflags(0) ## vectorCvt0.io.fflags(0), vectorCvt1.io.fflags.asUInt.tail(10) ## vectorCvt0.io.fflags.asUInt.tail(10)),
    vectorCvt1.io.fflags(0) ## vectorCvt0.io.fflags(0)
  ))

  for (i <- 0 until vlen/16) {
    io.fflags(i) := fflags(Fflags.width * (i + 1) - 1, Fflags.width * i)
  }
}


