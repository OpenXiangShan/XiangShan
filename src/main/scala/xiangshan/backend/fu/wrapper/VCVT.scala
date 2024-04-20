package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.{Mgu, VecPipedFuncUnit}
import xiangshan.ExceptionNO
import yunsuan.VfpuType
import yunsuan.vector.VectorConvert.VectorCvt
import yunsuan.util._


class VCVT(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfcvt OpType not supported")

  // params alias
  private val dataWidth = cfg.dataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // io alias
  private val opcode = fuOpType(8, 0)
  private val sew = vsew

  private val isRtz = opcode(2) & opcode(1)
  private val isRod = opcode(2) & !opcode(1) & opcode(0)
  private val isFrm = !isRtz && !isRod
  private val vfcvtRm = Mux1H(
    Seq(isRtz, isRod, isFrm),
    Seq(1.U, 6.U, rm)
  )

  private val lmul = vlmul // -3->3 => 1/8 ->8

  val widen = opcode(4, 3) // 0->single 1->widen 2->norrow => width of result
  val isSingleCvt = !widen(1) & !widen(0)
  val isWidenCvt = !widen(1) & widen(0)
  val isNarrowCvt = widen(1) & !widen(0)
  val fire = io.in.valid
  val fireReg = GatedValidRegNext(fire)

  // output width 8， 16， 32， 64
  val output1H = Wire(UInt(4.W))
  output1H := chisel3.util.experimental.decode.decoder(
    widen ## sew,
    TruthTable(
      Seq(
        BitPat("b00_01") -> BitPat("b0010"), // 16
        BitPat("b00_10") -> BitPat("b0100"), // 32
        BitPat("b00_11") -> BitPat("b1000"), // 64

        BitPat("b01_00") -> BitPat("b0010"), // 16
        BitPat("b01_01") -> BitPat("b0100"), // 32
        BitPat("b01_10") -> BitPat("b1000"), // 64

        BitPat("b10_00") -> BitPat("b0001"), // 8
        BitPat("b10_01") -> BitPat("b0010"), // 16
        BitPat("b10_10") -> BitPat("b0100"), // 32
      ),
      BitPat.N(4)
    )
  )
  if(backendParams.debugEn) {
    dontTouch(output1H)
  }
  val outputWidth1H = output1H
  val outIs32bits = RegNext(RegNext(outputWidth1H(2)))
  val outIsInt = !outCtrl.fuOpType(6)
  val outIsMvInst = outCtrl.fuOpType(8)

  val outEew = RegEnable(RegEnable(Mux1H(output1H, Seq(0,1,2,3).map(i => i.U)), fire), fireReg)
  private val needNoMask = outVecCtrl.fpu.isFpToVecInst
  val maskToMgu = Mux(needNoMask, allMaskTrue, outSrcMask)

  // modules
  private val vfcvt = Module(new VectorCvtTop(dataWidth, dataWidthOfDataModule))
  private val mgu = Module(new Mgu(dataWidth))

  val vs2Vec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  vs2Vec := vs2.asTypeOf(vs2Vec)

  /**
   * [[vfcvt]]'s in connection
   */
  vfcvt.uopIdx := vuopIdx(0)
  vfcvt.src := vs2Vec
  vfcvt.opType := opcode(7,0)
  vfcvt.sew := sew
  vfcvt.rm := vfcvtRm
  vfcvt.outputWidth1H := outputWidth1H
  vfcvt.isWiden := isWidenCvt
  vfcvt.isNarrow := isNarrowCvt
  vfcvt.fire := fire
  vfcvt.isFpToVecInst := vecCtrl.fpu.isFpToVecInst
  val vfcvtResult = vfcvt.io.result
  val vfcvtFflags = vfcvt.io.fflags

  /** fflags:
   */
  val eNum1H = chisel3.util.experimental.decode.decoder(sew ## (isWidenCvt || isNarrowCvt),
    TruthTable(
      Seq(                     // 8, 4, 2, 1
        BitPat("b001") -> BitPat("b1000"), //8
        BitPat("b010") -> BitPat("b1000"), //8
        BitPat("b011") -> BitPat("b0100"), //4
        BitPat("b100") -> BitPat("b0100"), //4
        BitPat("b101") -> BitPat("b0010"), //2
        BitPat("b110") -> BitPat("b0010"), //2
      ),
      BitPat.N(4)
    )
  )
  val eNumMax1H = Mux(lmul.head(1).asBool, eNum1H >> ((~lmul.tail(1)).asUInt +1.U), eNum1H << lmul.tail(1)).asUInt(6, 0)
  val eNumMax = Mux1H(eNumMax1H, Seq(1,2,4,8,16,32,64).map(i => i.U)) //only for cvt intr, don't exist 128 in cvt
  val vlForFflags = Mux(vecCtrl.fpu.isFpToVecInst, 1.U, vl)
  val eNumEffectIdx = Mux(vlForFflags > eNumMax, eNumMax, vlForFflags)

  val eNum = Mux1H(eNum1H, Seq(1, 2, 4, 8).map(num =>num.U))
  val eStart = vuopIdx * eNum
  val maskForFflags = Mux(vecCtrl.fpu.isFpToVecInst, allMaskTrue, srcMask)
  val maskPart = maskForFflags >> eStart
  val mask =  Mux1H(eNum1H, Seq(1, 2, 4, 8).map(num => maskPart(num-1, 0)))
  val fflagsEn = Wire(Vec(4 * numVecModule, Bool()))

  fflagsEn := mask.asBools.zipWithIndex.map{case(mask, i) => mask & (eNumEffectIdx > eStart + i.U) }

  val fflagsEnCycle2 = RegEnable(RegEnable(fflagsEn, fire), fireReg)
  val fflagsAll = Wire(Vec(8, UInt(5.W)))
  fflagsAll := vfcvtFflags.asTypeOf(fflagsAll)
  val fflags = fflagsEnCycle2.zip(fflagsAll).map{case(en, fflag) => Mux(en, fflag, 0.U(5.W))}.reduce(_ | _)
  io.out.bits.res.fflags.get := Mux(outIsMvInst, 0.U, fflags)


  /**
   * [[mgu]]'s in connection
   */
  val resultDataUInt = Wire(UInt(dataWidth.W))
  resultDataUInt := vfcvtResult

  private val narrow = RegEnable(RegEnable(isNarrowCvt, fire), fireReg)
  private val narrowNeedCat = outVecCtrl.vuopIdx(0).asBool && narrow
  private val outNarrowVd = Mux(narrowNeedCat, Cat(resultDataUInt(dataWidth / 2 - 1, 0), outOldVd(dataWidth / 2 - 1, 0)), resultDataUInt)

  mgu.io.in.vd := resultDataUInt
  mgu.io.in.vd := Mux(narrow, outNarrowVd, resultDataUInt)
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := maskToMgu
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := Mux(outVecCtrl.fpu.isFpToVecInst, 1.U, outVl)
  mgu.io.in.info.vlmul := outVecCtrl.vlmul
  mgu.io.in.info.valid := io.out.valid
  mgu.io.in.info.vstart := Mux(outVecCtrl.fpu.isFpToVecInst, 0.U, outVecCtrl.vstart)
  mgu.io.in.info.eew := outEew
  mgu.io.in.info.vsew := outVecCtrl.vsew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.narrow := narrow
  mgu.io.in.info.dstMask := outVecCtrl.isDstMask
  mgu.io.in.isIndexedVls := false.B

  // for scalar f2i cvt inst
  val isFp2VecForInt = outVecCtrl.fpu.isFpToVecInst && outIs32bits && outIsInt
  // for f2i mv inst
  val result = Mux(outIsMvInst, RegNext(RegNext(vs2.tail(64))), mgu.io.out.vd)

  io.out.bits.res.data := Mux(isFp2VecForInt,
    Fill(32, result(31)) ## result(31, 0),
    result
  )
  io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := mgu.io.out.illegal
}

class VectorCvtTopIO(vlen: Int, xlen: Int) extends Bundle{
  val fire = Input(Bool())
  val uopIdx = Input(Bool())
  val src = Input(Vec(vlen / xlen, UInt(xlen.W)))
  val opType = Input(UInt(8.W))
  val sew = Input(UInt(2.W))
  val rm = Input(UInt(3.W))
  val outputWidth1H = Input(UInt(4.W))
  val isWiden = Input(Bool())
  val isNarrow = Input(Bool())
  val isFpToVecInst = Input(Bool())

  val result = Output(UInt(vlen.W))
  val fflags = Output(UInt((vlen/16*5).W))
}



//according to uopindex, 1: high64 0:low64
class VectorCvtTop(vlen: Int, xlen: Int) extends Module{
  val io = IO(new VectorCvtTopIO(vlen, xlen))

  val (fire, uopIdx, src, opType, sew, rm, outputWidth1H, isWiden, isNarrow, isFpToVecInst) = (
    io.fire, io.uopIdx, io.src, io.opType, io.sew, io.rm, io.outputWidth1H, io.isWiden, io.isNarrow, io.isFpToVecInst
  )
  val fireReg = GatedValidRegNext(fire)

  val in0 = Mux(isWiden && !isFpToVecInst,
    Mux(uopIdx, src(1).tail(32), src(0).tail(32)),
    src(0)
  )

  val in1 = Mux(isWiden,
    Mux(uopIdx, src(1).head(32), src(0).head(32)),
    src(1)
  )

  val vectorCvt0 = Module(new VectorCvt(xlen))
  vectorCvt0.fire := fire
  vectorCvt0.src := in0
  vectorCvt0.opType := opType
  vectorCvt0.sew := sew
  vectorCvt0.rm := rm
  vectorCvt0.isFpToVecInst := isFpToVecInst

  val vectorCvt1 = Module(new VectorCvt(xlen))
  vectorCvt1.fire := fire
  vectorCvt1.src := in1
  vectorCvt1.opType := opType
  vectorCvt1.sew := sew
  vectorCvt1.rm := rm
  vectorCvt1.isFpToVecInst := isFpToVecInst

  val isNarrowCycle2 = RegEnable(RegEnable(isNarrow, fire), fireReg)
  val outputWidth1HCycle2 = RegEnable(RegEnable(outputWidth1H, fire), fireReg)

  //cycle2
  io.result := Mux(isNarrowCycle2,
    vectorCvt1.io.result.tail(32) ## vectorCvt0.io.result.tail(32),
    vectorCvt1.io.result ## vectorCvt0.io.result)

  io.fflags := Mux1H(outputWidth1HCycle2, Seq(
    vectorCvt1.io.fflags ## vectorCvt0.io.fflags,
    Mux(isNarrowCycle2, vectorCvt1.io.fflags.tail(10) ## vectorCvt0.io.fflags.tail(10), vectorCvt1.io.fflags ## vectorCvt0.io.fflags),
    Mux(isNarrowCycle2, vectorCvt1.io.fflags(4,0) ## vectorCvt0.io.fflags(4,0), vectorCvt1.io.fflags.tail(10) ## vectorCvt0.io.fflags.tail(10)),
    vectorCvt1.io.fflags(4,0) ## vectorCvt0.io.fflags(4,0)
  ))
}


