package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.decode.opcode.Opcode.VFMacOpcodes
import xiangshan.backend.decode.opcode.Opcode.VFMacOpcodes.isFmul
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.vector.fu.Func._
import xiangshan.backend.vector.fu.{Func, VecFixLatFunc, VecFuConfig}
import yunsuan.vector.Common.{Fflags, VSew}
import yunsuan.vector.vfalu.VectorFALU
import yunsuan.vector.vfmul.{VFMul2VFALUCtrlBundle, VFMul2VFALUOutput}

object VFAluWrapper {
  /**
    * The S1 product is not a register-file result.  It carries the complete
    * OP3 context into VFALU so its S1 result can use the normal vector
    * writeback and merge path.
    */
  class VFMulForward(cfg: VecFuConfig)(implicit p: Parameters) extends XSBundle {
    val mul = Vec(cfg.destDataBits / 64, new VFMul2VFALUOutput)
    val addend = Vec(cfg.destDataBits / 64, UInt(64.W))
    val isSub = Bool()
    val ctrlOpcode = UInt(6.W)
    val frm = UInt(3.W)
    val ctrl = new Func.InCtrl(cfg)
    val data = new Func.InData(cfg)
    val debug = Option.when(backendParams.debugEn)(new xiangshan.backend.vector.VecRegionModule.DebugBundle)
  }
}

class VFAluWrapper(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {

  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  private val vlenb = VLEN / 8
  private val zeroFflags = 0.U.asTypeOf(Fflags())
  private val zeroFflagsE8 = VecInit(Seq.fill(vlenb)(zeroFflags))
  private val zeroNarrowFflagsE8 = VecInit(Seq.fill(vlenb / 2)(zeroFflags))

  private val ex0opcode = fuOpType
  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode
  private val ex0vsew = ex0ctrl.vtype.get.vsew
  val fromVfmul = IO(Input(Valid(new VFAluWrapper.VFMulForward(cfg))))
  val op3OutContext = IO(Output(Valid(new VFAluWrapper.VFMulForward(cfg))))

  // VectorFALU S0 accepts the VFMUL S1 product.  Keep the original uop
  // alongside it so MergeUnit observes the matching oldVd/vl/vtype context.
  private val op3S1Valid = RegInit(false.B)
  private val op3Context = Reg(new VFAluWrapper.VFMulForward(cfg))
  op3S1Valid := fromVfmul.valid
  when (fromVfmul.valid) {
    op3Context := fromVfmul.bits
  }
  op3OutContext.valid := op3S1Valid && !op3Context.ctrl.robIdx.needFlush(in.flush)
  op3OutContext.bits := op3Context

  private val vfalus = Seq.fill(numVecModule)(Module(new VectorFALU)) // WARNING: Don't change this module. MUST USE VectorFloatMultiplier
//  val fromVfmul = IO(Output(Valid(Vec(numVecModule, new FuncUnitFaluInputFromFmul))))

  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val oldVdSplit = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))

  vs2Split.io.inVecData := ex0vs2
  vs1Split.io.inVecData := ex0vs1
  oldVdSplit.io.inVecData := ex0oldVd

  private val resultData = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val fflagsData = Wire(Vec(numVecModule, Vec(vlenb / numVecModule, Fflags())))
  private val absorbedMaskS1 = Reg(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val absorbedResultS1 = Reg(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val absorbedFflagsS1 = Reg(Vec(numVecModule, Vec(vlenb / numVecModule, Fflags())))
  private val absorbedFflagsMaskS1 = Reg(Vec(numVecModule, Vec(vlenb / numVecModule, Bool())))

  // The generic FALU far path loses bits when the exponent gap exceeds a
  // format's significand width. In that range the smaller finite operand only
  // selects the IEEE-754 neighbour under directed rounding.
  private def absorbedFloat(lhs: UInt, rhs: UInt, exponentWidth: Int, fractionWidth: Int, sew: UInt): (Bool, UInt, UInt) = {
    val floatWidth = 1 + exponentWidth + fractionWidth
    val lhsExp = lhs(floatWidth - 2, fractionWidth)
    val rhsExp = rhs(floatWidth - 2, fractionWidth)
    val lhsFinite = !lhsExp.andR
    val rhsFinite = !rhsExp.andR
    val lhsLarger = lhsExp >= rhsExp
    val expGap = Mux(lhsLarger, lhsExp - rhsExp, rhsExp - lhsExp)
    val isSub = VFMacOpcodes.isFsub(ex0opcode)
    val normalAddSub = VFMacOpcodes.isFadd(ex0opcode) || isSub
    val supportedFrm = frm <= yunsuan.vector.Common.Frm.rmm
    val enabled = normalAddSub && VFMacOpcodes.getDataType(ex0opcode) === sew &&
      supportedFrm && lhsFinite && rhsFinite && expGap > (fractionWidth + 1).U

    val largeRaw = Mux(lhsLarger, lhs, rhs)
    val smallRaw = Mux(lhsLarger, rhs, lhs)
    val largeIsRhs = !lhsLarger
    // a - b is a + (-b), so apply the subtraction sign to the operand that
    // occupies the logical rhs position before choosing a rounded neighbour.
    val largeSign = largeRaw(floatWidth - 1) ^ (isSub && largeIsRhs)
    val smallSign = smallRaw(floatWidth - 1) ^ (isSub && !largeIsRhs)
    val large = Cat(largeSign, largeRaw(floatWidth - 2, 0))
    val smallIsZero = !smallRaw(floatWidth - 2, 0).orR
    val smallIsNonZero = !smallIsZero
    val nextUp = Mux(largeSign, large -% 1.U(floatWidth.W), large +% 1.U(floatWidth.W))
    val nextDown = Mux(largeSign, large +% 1.U(floatWidth.W), large -% 1.U(floatWidth.W))

    val rtzAdjust = smallIsNonZero && (largeSign =/= smallSign)
    val rdnAdjust = smallIsNonZero && smallSign
    val rupAdjust = smallIsNonZero && !smallSign
    val adjustUp = (frm === yunsuan.vector.Common.Frm.rtz && rtzAdjust && largeSign) ||
      (frm === yunsuan.vector.Common.Frm.rup && rupAdjust)
    val adjustDown = (frm === yunsuan.vector.Common.Frm.rtz && rtzAdjust && !largeSign) ||
      (frm === yunsuan.vector.Common.Frm.rdn && rdnAdjust)
    val result = Mux(adjustUp, nextUp, Mux(adjustDown, nextDown, large))

    val largeIsMaxFinite = largeRaw(floatWidth - 2, fractionWidth) === ((1 << exponentWidth) - 2).U &&
      largeRaw(fractionWidth - 1, 0).andR
    val overflow = largeIsMaxFinite && ((adjustUp && !largeSign) || (adjustDown && largeSign))
    val fflags = Cat(0.U(2.W), overflow, false.B, smallIsNonZero)
    (enabled, result, fflags)
  }

  vfalus.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fire               := fromVfmul.valid || ex(0).valid
      mod.io.in.opcode          := Mux(fromVfmul.valid, fromVfmul.bits.ctrlOpcode, ex0opcode)
      mod.io.in.fpA             := Mux(fromVfmul.valid, fromVfmul.bits.mul(i).fpA, vs2Split.io.outVec64b(i))
      mod.io.in.fpB             := Mux(fromVfmul.valid, fromVfmul.bits.addend(i), vs1Split.io.outVec64b(i))
      mod.io.in.fpAAppend       := Mux(fromVfmul.valid, fromVfmul.bits.mul(i).fpAAppend, 0.U)
      mod.io.in.roundMode       := Mux(fromVfmul.valid, fromVfmul.bits.frm, frm)
      mod.io.in.inCtrlFromVFMul := Mux(fromVfmul.valid, fromVfmul.bits.mul(i).FMULToFADDCtrl, 0.U.asTypeOf(new VFMul2VFALUCtrlBundle))
      mod.io.in.isSubFromVFMul  := Mux(fromVfmul.valid, fromVfmul.bits.isSub, false.B)

      val fpA = vs2Split.io.outVec64b(i)
      val fpB = vs1Split.io.outVec64b(i)
      val f64 = absorbedFloat(fpA, fpB, 11, 52, yunsuan.vector.Common.VSew.e64)
      val f32 = Seq.tabulate(2) { elem =>
        absorbedFloat(fpA(32 * (elem + 1) - 1, 32 * elem), fpB(32 * (elem + 1) - 1, 32 * elem),
          8, 23, yunsuan.vector.Common.VSew.e32)
      }
      val f16 = Seq.tabulate(4) { elem =>
        absorbedFloat(fpA(16 * (elem + 1) - 1, 16 * elem), fpB(16 * (elem + 1) - 1, 16 * elem),
          5, 10, yunsuan.vector.Common.VSew.e16)
      }
      val dataType = VFMacOpcodes.getDataType(ex0opcode)
      val absorbedMask = Mux1H(Seq(
        (dataType === yunsuan.vector.Common.VSew.e64) -> Fill(64, f64._1),
        (dataType === yunsuan.vector.Common.VSew.e32) -> Cat(f32.map(x => Fill(32, x._1)).reverse),
        (dataType === yunsuan.vector.Common.VSew.e16) -> Cat(f16.map(x => Fill(16, x._1)).reverse),
      ))
      val absorbedResult = Mux1H(Seq(
        (dataType === yunsuan.vector.Common.VSew.e64) -> f64._2,
        (dataType === yunsuan.vector.Common.VSew.e32) -> Cat(f32.map(_._2).reverse),
        (dataType === yunsuan.vector.Common.VSew.e16) -> Cat(f16.map(_._2).reverse),
      ))
      val absorbedFflags = Wire(Vec(vlenb / numVecModule, Fflags()))
      val absorbedFflagsMask = Wire(Vec(vlenb / numVecModule, Bool()))
      for (byte <- 0 until vlenb / numVecModule) {
        absorbedFflags(byte) := Mux1H(Seq(
          (dataType === yunsuan.vector.Common.VSew.e64) -> f64._3,
          (dataType === yunsuan.vector.Common.VSew.e32) -> f32(byte / 4)._3,
          (dataType === yunsuan.vector.Common.VSew.e16) -> f16(byte / 2)._3,
        ))
        absorbedFflagsMask(byte) := Mux1H(Seq(
          (dataType === yunsuan.vector.Common.VSew.e64) -> f64._1,
          (dataType === yunsuan.vector.Common.VSew.e32) -> f32(byte / 4)._1,
          (dataType === yunsuan.vector.Common.VSew.e16) -> f16(byte / 2)._1,
        ))
      }
      when (ex(0).valid) {
        absorbedMaskS1(i) := absorbedMask
        absorbedResultS1(i) := absorbedResult
        absorbedFflagsS1(i) := absorbedFflags
        absorbedFflagsMaskS1(i) := absorbedFflagsMask
      }
      resultData(i) := Mux(op3S1Valid, mod.io.out.fpResult,
        (absorbedResultS1(i) & absorbedMaskS1(i)) | (mod.io.out.fpResult & ~absorbedMaskS1(i)))
      val mixedFflags = Wire(Vec(vlenb / numVecModule, Fflags()))
      for (byte <- 0 until vlenb / numVecModule) {
        mixedFflags(byte) := Mux(absorbedFflagsMaskS1(i)(byte), absorbedFflagsS1(i)(byte), mod.io.out.fflagsVec(byte))
      }
      fflagsData(i) := Mux(op3S1Valid, mod.io.out.fflagsVec, mixedFflags)
  }

  private def zeroVecData(vecData: VecSpecialData): Unit = {
    vecData.normal := 0.U
    vecData.narrow := 0.U
    vecData.maskE8 := 0.U
    vecData.maskE16 := 0.U
    vecData.maskE32 := 0.U
    vecData.maskE64 := 0.U
    vecData.isWiden.foreach(_ := false.B)
    vecData.isNarrow.foreach(_ := false.B)
    vecData.vxsatE8.foreach(_ := 0.U.asTypeOf(vecData.vxsatE8.get))
    vecData.narrowVxsatE8.foreach(_ := 0.U.asTypeOf(vecData.narrowVxsatE8.get))
    vecData.fflagsE8.foreach(_ := zeroFflagsE8)
    vecData.narrowFflagsE8.foreach(_ := zeroNarrowFflagsE8)
  }

  for (i <- 0 to cfg.latency) {
    out.ex(i).bits.data.vec.foreach(zeroVecData)
  }

  out.ex(cfg.latency).bits.data.vec.foreach { vecData =>
    vecData.normal := Cat(resultData.reverse)
    vecData.fflagsE8.get := fflagsData.asTypeOf(Vec(vlenb, Fflags()))
  }

  when (op3S1Valid) {
    out.ex(cfg.latency).bits.ctrl.robIdx := op3Context.ctrl.robIdx
    out.ex(cfg.latency).bits.ctrl.pdest := op3Context.ctrl.pdest
    out.ex(cfg.latency).bits.ctrl.pdestV0.zip(op3Context.ctrl.pdestV0).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.pdestVl.zip(op3Context.ctrl.pdestVl).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.rfWen.zip(op3Context.ctrl.rfWen).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.fpWen.zip(op3Context.ctrl.fpWen).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.vecWen.zip(op3Context.ctrl.vecWen).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.v0Wen.zip(op3Context.ctrl.v0Wen).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.vlWen.zip(op3Context.ctrl.vlWen).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.flushPipe.zip(op3Context.ctrl.flushPipe).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.fflagsWen.zip(op3Context.ctrl.fflagsWen).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.debug.zip(op3Context.debug).foreach { case (sink, source) => sink := source }
  }

  // The normal VFALU input and an OP3 product reserve the same S0.  The
  // issue-side writeback reservation prevents this collision.
  assert(!(fromVfmul.valid && ex(0).valid), "VFALU received OP2 and OP3 work in the same S0")
  assert(!(op3S1Valid && ex(cfg.latency).valid), "VFALU produced OP2 and OP3 results in the same cycle")
  out.ex(cfg.latency).valid := Mux(op3S1Valid, op3OutContext.valid, ex(cfg.latency).valid)

}
