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


class VCVT(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfcvt OpType not supported")

  // params alias
  private val dataWidth = cfg.destDataBits
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
  
  // May be useful in the future.
  // val outIsMvInst = outCtrl.fuOpType === FuOpType.FMVXF
  val outIsMvInst = false.B

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
  private val uopIdxWidth = log2Ceil(numVecModule)
  vfcvt.uopIdx := vuopIdx(uopIdxWidth - 1, 0)
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
  val eNum1HEffect = Mux(isWidenCvt || isNarrowCvt, eNum1H << 1, eNum1H)
  val eNumBase = Mux1H(eNum1HEffect, Seq(1, 2, 4, 8).map(_.U))
  private val eNumScale = (numVecModule / 2).U
  val eNumMax = Mux(lmul.head(1).asBool, eNumBase * eNumScale >> ((~lmul.tail(1)).asUInt + 1.U), eNumBase * eNumScale << lmul.tail(1)).asUInt
  val vlForFflags = Mux(vecCtrl.fpu.isFpToVecInst, 1.U, vl)
  val eNumEffectIdx = Mux(vlForFflags > eNumMax, eNumMax, vlForFflags)

  val eNum = Mux1H(eNum1H, Seq(1, 2, 4, 8).map(num => (num.U * eNumScale)))
  val eStart = vuopIdx * eNum
  val maskForFflags = Mux(vecCtrl.fpu.isFpToVecInst, allMaskTrue, srcMask)
  val maskPart = maskForFflags >> eStart
  val fflagsEn = Wire(Vec(4 * numVecModule, Bool()))
  for (i <- 0 until 4 * numVecModule) {
    val maskBit = if (i < maskPart.getWidth) maskPart(i) else false.B
    fflagsEn(i) := maskBit && (eNumEffectIdx > (eStart + i.U))
  }

  val fflagsEnCycle2 = RegEnable(RegEnable(fflagsEn, fire), fireReg)
  val fflagsAll = Wire(Vec(4 * numVecModule, UInt(5.W)))
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
  private val outNarrowVd = Mux(narrowNeedCat, Cat(resultDataUInt(dataWidth / 2 - 1, 0), outOldVd(dataWidth / 2 - 1, 0)), 
                                               Cat(outOldVd(dataWidth - 1, dataWidth / 2), resultDataUInt(dataWidth / 2 - 1, 0)))

  // mgu.io.in.vd := resultDataUInt
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
  val uopIdx = Input(UInt(log2Ceil(vlen / xlen).W))
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

  private val numVecModule = vlen / xlen
  require(numVecModule % 2 == 0, "VectorCvtTop expects vlen/xlen to be even")
  private val groupIdxRaw = Mux(isWiden && !isFpToVecInst, uopIdx >> 1, uopIdx)
  private val pairIdxWidth = if (numVecModule > 2) log2Ceil(numVecModule / 2) else 1
  private val pairIdx = if (numVecModule > 2) groupIdxRaw(pairIdxWidth - 1, 0) else 0.U(1.W)
  private val isVecWiden = isWiden && !isFpToVecInst
  private val widenSel = Mux(isVecWiden, uopIdx(0), false.B)
  private val baseIdx = pairIdx << 1
  private val src0 = src(baseIdx)
  private val src1 = src(baseIdx + 1.U)

  val in0 = Mux(isVecWiden,
    Mux(widenSel, src1.tail(32), src0.tail(32)),
    src0
  )

  val in1 = Mux(isWiden,
    Mux(widenSel, src1.head(32), src0.head(32)),
    src1
  )

  val vectorCvt0 = Module(new VectorCvt(xlen))
  vectorCvt0.fire := fire
  vectorCvt0.src := in0
  vectorCvt0.opType := opType
  vectorCvt0.sew := sew
  vectorCvt0.rm := rm
  vectorCvt0.isFpToVecInst := isFpToVecInst
  vectorCvt0.isFround := 0.U
  vectorCvt0.isFcvtmod := false.B

  val vectorCvt1 = Module(new VectorCvt(xlen))
  vectorCvt1.fire := fire
  vectorCvt1.src := in1
  vectorCvt1.opType := opType
  vectorCvt1.sew := sew
  vectorCvt1.rm := rm
  vectorCvt1.isFpToVecInst := isFpToVecInst
  vectorCvt1.isFround := 0.U
  vectorCvt1.isFcvtmod := false.B

  val isNarrowCycle2 = RegEnable(RegEnable(isNarrow, fire), fireReg)
  val outputWidth1HCycle2 = RegEnable(RegEnable(outputWidth1H, fire), fireReg)
  val useFullCycle2 = RegEnable(RegEnable(!isWiden && !isNarrow, fire), fireReg)
  val isVecWidenCycle2 = RegEnable(RegEnable(isVecWiden, fire), fireReg)

  //cycle2
  private val resultPair = Mux(isNarrowCycle2,
    vectorCvt1.io.result.tail(32) ## vectorCvt0.io.result.tail(32),
    vectorCvt1.io.result ## vectorCvt0.io.result)

  private val resultPairVec = resultPair.asTypeOf(Vec(2, UInt(xlen.W)))
  private val resultVec = Wire(Vec(numVecModule, UInt(xlen.W)))
  resultVec.foreach(_ := 0.U)
  resultVec(baseIdx) := resultPairVec(0)
  resultVec(baseIdx + 1.U) := resultPairVec(1)
  val vectorCvtsFull = Seq.fill(numVecModule)(Module(new VectorCvt(xlen)))
  // For widening: select source half based on uopIdx(0), distribute 32-bit chunks
  private val widenSrcHalf = Wire(Vec(numVecModule / 2, UInt(xlen.W)))
  for (i <- 0 until numVecModule / 2) {
    widenSrcHalf(i) := Mux(uopIdx(0), src(numVecModule / 2 + i), src(i))
  }
  vectorCvtsFull.zipWithIndex.foreach { case (mod, i) =>
    mod.fire := fire
    val widenChunk = if (i % 2 == 0) widenSrcHalf(i / 2)(31, 0) else widenSrcHalf(i / 2)(63, 32)
    mod.src := Mux(isVecWiden, Cat(0.U(32.W), widenChunk), src(i))
    mod.opType := opType
    mod.sew := sew
    mod.rm := rm
    mod.isFpToVecInst := isFpToVecInst
    mod.isFround := 0.U
    mod.isFcvtmod := false.B
  }
  private val resultVecFull = VecInit(vectorCvtsFull.map(_.io.result))
  // For narrow ops at VLEN>128, all converters must contribute.
  // Each converter's narrow result is in its lower 32 bits; concatenate them.
  private val resultNarrowFull = Cat(vectorCvtsFull.map(_.io.result(xlen / 2 - 1, 0)).reverse)
  io.result := Mux(isNarrowCycle2, resultNarrowFull,
               Mux(useFullCycle2 || isVecWidenCycle2, resultVecFull.asUInt, resultVec.asUInt))

  private val fflagsPair = Mux1H(outputWidth1HCycle2, Seq(
    vectorCvt1.io.fflags ## vectorCvt0.io.fflags,
    Mux(isNarrowCycle2, vectorCvt1.io.fflags.tail(10) ## vectorCvt0.io.fflags.tail(10), vectorCvt1.io.fflags ## vectorCvt0.io.fflags),
    Mux(isNarrowCycle2, vectorCvt1.io.fflags(4,0) ## vectorCvt0.io.fflags(4,0), vectorCvt1.io.fflags.tail(10) ## vectorCvt0.io.fflags.tail(10)),
    vectorCvt1.io.fflags(4,0) ## vectorCvt0.io.fflags(4,0)
  ))
  private val fflagsPairWidth = Mux1H(outputWidth1HCycle2, Seq(
    (xlen / 16 * 5 * 2).U, // 8 or 16
    (xlen / 16 * 5 * 2).U, // 8 or 16
    (xlen / 32 * 5 * 2).U, // 32
    (xlen / 64 * 5 * 2).U  // 64
  ))
  private val fflagsAll = Wire(UInt((vlen / 16 * 5).W))
  fflagsAll := (fflagsPair.asUInt << (pairIdx * fflagsPairWidth)).asUInt
  private val fullPairNum = numVecModule / 2
  private val fflagsPairsFull = Wire(Vec(fullPairNum, UInt((xlen / 16 * 5 * 2).W)))
  for (p <- 0 until fullPairNum) {
    val lo = vectorCvtsFull(2 * p).io.fflags
    val hi = vectorCvtsFull(2 * p + 1).io.fflags
    fflagsPairsFull(p) := Mux1H(outputWidth1HCycle2, Seq(
      hi ## lo,
      Mux(isNarrowCycle2, hi.tail(10) ## lo.tail(10), hi ## lo),
      Mux(isNarrowCycle2, hi(4,0) ## lo(4,0), hi.tail(10) ## lo.tail(10)),
      hi(4,0) ## lo(4,0)
    ))
  }
  private val fflagsFull = fflagsPairsFull.zipWithIndex.map { case (pair, i) =>
    // For narrow, each pair produces half the fflags (narrowed elements),
    // so the shift must use the actual narrow fflags width, not the full width.
    val narrowFflagsPairWidth = Mux1H(outputWidth1HCycle2, Seq(
      (xlen / 8 * 5).U,  // 8-bit output: 4 elements per converter, 8 per pair
      (xlen / 16 * 5).U, // 16-bit output: 2 elements per converter, 4 per pair
      (xlen / 32 * 5).U, // 32-bit output: 1 element per converter, 2 per pair
      (xlen / 64 * 5).U  // 64-bit output
    ))
    val shiftWidth = Mux(isNarrowCycle2, narrowFflagsPairWidth, fflagsPairWidth)
    (pair.asUInt << (i.U * shiftWidth)).asUInt
  }.reduce(_ | _)
  io.fflags := Mux(useFullCycle2 || isNarrowCycle2 || isVecWidenCycle2, fflagsFull, fflagsAll)
}
