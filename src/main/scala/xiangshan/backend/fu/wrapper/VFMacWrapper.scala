package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.decode.opcode.Opcode.VFMacOpcodes
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.{Frm => VecFrm}
import xiangshan.backend.vector.fu.Func._
import xiangshan.backend.vector.fu.{Func, VecFixLatFunc, VecFuConfig}
import yunsuan.vector.Common.Fflags
import yunsuan.vector.vfalu.{VectorFALU, VectorFALUInput}
import yunsuan.vector.vfmul.VectorFMUL

object VFMacWrapper {
  val laneWidth = 64

  /** R0: multiplicand register, shared by the mul and mac paths. */
  class MulInput(numLane: Int) extends Bundle {
    val frm    = Frm()
    val isFmul = Bool()
    val isNeg  = Bool()
    val fpFmt  = UInt(2.W)
    val fpA    = Vec(numLane, UInt(laneWidth.W))
    val fpB    = Vec(numLane, UInt(laneWidth.W))
  }
}

/**
  * Merged VFALU + VFMUL function unit.
  *
  * {{{ 
  *   mul : in -> R0(multiplicand)  -> R1(vfmul S0/S1) -> R2(vfmul S1/S2)      -> out  depth 3, latency 2 -> out.ex(2)
  *   add : in -> A0(operands)      -> A1(vfadd S0/S1)                         -> out  depth 2, latency 1 -> out.ex(1)
  *   mac : in -> R0 -> R1(product) -> A0(product + addend) -> A1(vfadd S0/S1) -> out  depth 4, latency 3 -> out.ex(3)
  * }}}
  *
  * Every op releases on the slot index equal to its own latency, i.e. on the ex stage the uop
  * itself occupies at its release cycle (vfadd on ex(1), vfmul on ex(2), vfmac on ex(3)). That
  * keeps `Func`'s per-slot gating (`ctrl.latency === i`) and the Exu's per-slot merge/writeback
  * aligned with the releasing uop, since the Exu takes ctrl, pdest, vecWen and robIdx from
  * `in.ex(i)`: a mac never borrows a younger uop's context, so no context override is needed.
  * `cfg.latency` (= 3) sizes the `in.ex`/`out.ex` vectors and the Exu's ex pipeline (ex(0..3));
  * the per-op latency that drives wake-up and the WB busy table comes from `Opcode.getLat`
  * (`ctrl.latency`).
  *
  * R1/R2 are VectorFMUL's internal registers, A1 is VectorFALU's; this wrapper only owns R0
  * and A0. A0 is shared by the add and mac paths (both hold a FALU input), which is safe
  * because the WB busy table blocks a latency-1 op issued `k = lat_mac - lat_add = 2` cycles
  * after a mac, i.e. exactly the cycle that would write A0 twice (asserted below). The same
  * rule keeps two in-flight uops from claiming one `out.ex(i)` slot in the same cycle.
  *
  * The op class is selected by `ctrl.latency` (1 = alu, 2 = mul, 3 = mac).
  */
class VFMacWrapper(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {
  import VFMacWrapper._

  require(cfg.latency == 3, s"${cfg.name}: VFMacWrapper releases on out.ex(1)/(2)/(3), so cfg.latency = 3")
  require(cfg.needSrcFrm, s"${cfg.name}: VFMacWrapper needs the instruction frm")

  private val numLane = cfg.destDataBits / laneWidth
  private val vlenb = VLEN / 8
  private val zeroFflags = 0.U.asTypeOf(Fflags())
  private val zeroFflagsE8 = VecInit(Seq.fill(vlenb)(zeroFflags))
  private val zeroNarrowFflagsE8 = VecInit(Seq.fill(vlenb / 2)(zeroFflags))

  private val inCtrl   = in.ex0Next.bits.ctrl
  private val inData   = in.ex0Next.bits.data
  private val inOpcode = inCtrl.opcode
  private val ex1Ctrl  = in.ex(1).bits.ctrl
  private val ex1Data  = in.ex(1).bits.data
  private val ex1Opcode = ex1Ctrl.opcode

  // op class per entry point; ctrl.latency is the same selector the release slot uses
  private val aluIn  = in.ex0Next.valid && inCtrl.latency === 1.U
  private val mulIn  = in.ex0Next.valid && inCtrl.latency === 2.U
  private val macIn  = in.ex0Next.valid && inCtrl.latency === 3.U
  private val macEx1 = in.ex(1).valid && ex1Ctrl.latency === 3.U

  /** Resolve DYN to the CSR value; same rule as the Exu's per-stage effective frm. */
  private def effectiveFrm(ctrl: Func.InCtrl): UInt =
    ctrl.frm.map(instFrm => Mux(instFrm === VecFrm.DYN, in.frm.get, instFrm)).get

  private def isFmadd(op: UInt): Bool = VFMacOpcodes.isFmadd(op) || VFMacOpcodes.isFnmadd(op) ||
    VFMacOpcodes.isFmsub(op) || VFMacOpcodes.isFnmsub(op)

  private def isNegProduct(op: UInt): Bool = VFMacOpcodes.isFnmadd(op) || VFMacOpcodes.isFnmsub(op) ||
    VFMacOpcodes.isFnmacc(op) || VFMacOpcodes.isFnmsac(op)

  private def isSubFromFmul(op: UInt): Bool = VFMacOpcodes.isFnmadd(op) || VFMacOpcodes.isFmsub(op) ||
    VFMacOpcodes.isFnmacc(op) || VFMacOpcodes.isFmsac(op)

  // ---------------------------------------------------------------------------
  // modules: R1/R2 (VectorFMUL) and A1 (VectorFALU) are their internal registers
  // ---------------------------------------------------------------------------
  private val vfmuls = Seq.fill(numLane)(Module(new VectorFMUL))
  private val vfalus = Seq.fill(numLane)(Module(new VectorFALU))

  // ---------------------------------------------------------------------------
  // R0: multiplicand register, written by the mul and mac paths (offset 0)
  // ---------------------------------------------------------------------------
  private val vs1Split = Module(new VecDataSplitModule(cfg.destDataBits, laneWidth))
  private val fpBSplit = Module(new VecDataSplitModule(cfg.destDataBits, laneWidth))
  // fmadd/fnmadd/fmsub/fnmsub take the addend from vs2, hence the multiplicand pair is (vs1, oldVd)
  vs1Split.io.inVecData := inData.src(0)
  fpBSplit.io.inVecData := Mux(isFmadd(inOpcode), inData.src(2), inData.src(1))

  private val r0Next = Wire(new MulInput(numLane))
  r0Next.frm    := effectiveFrm(inCtrl)
  r0Next.isFmul := VFMacOpcodes.isFmul(inOpcode)
  r0Next.isNeg  := isNegProduct(inOpcode)
  r0Next.fpFmt  := VFMacOpcodes.getDataType(inOpcode)
  for (i <- 0 until numLane) {
    r0Next.fpA(i) := vs1Split.io.outVec64b(i)
    r0Next.fpB(i) := fpBSplit.io.outVec64b(i)
  }
  private val r0Write = mulIn || macIn
  private val r0Valid = RegNext(r0Write, false.B)
  private val r0 = RegEnable(r0Next, r0Write)

  // ---------------------------------------------------------------------------
  // A0: VectorFALU input register, shared by the add and mac paths
  //      add: written from the in stage                (offset 0)
  //      mac: written from the product and ex(1) addend (offset 2)
  // ---------------------------------------------------------------------------
  private val a0FromOperands = Wire(Vec(numLane, new VectorFALUInput))
  private val a0FromProduct  = Wire(Vec(numLane, new VectorFALUInput))
  private val macAddend = Mux(isFmadd(ex1Opcode), ex1Data.src(1), ex1Data.src(2))

  // `in.frm` is only valid for the uop entering the pipe *this* cycle, so the mac path (which
  // rebuilds its FALU input from the product two cycles later) carries the resolved mode along
  // instead of re-sampling `in.frm` at its ex(1) stage, where it would belong to a younger uop.
  private val macFrmS1 = RegEnable(effectiveFrm(inCtrl), macIn)
  // `macFrmS1` is written at the mac's issue and held until the next mac, so a plain delay lands
  // the mac's own mode on the cycle it drives the FALU input register (`macEx1`, two cycles
  // after issue). An enable on `macEx1` would sample it one cycle too late, i.e. the mode of the
  // uop ahead of the mac.
  private val macFrmS2 = RegNext(macFrmS1, 0.U)

  for (i <- 0 until numLane) {
    val high = laneWidth * (i + 1) - 1
    val low = laneWidth * i

    a0FromOperands(i).opcode          := inOpcode
    a0FromOperands(i).fpA             := inData.src(1)(high, low)
    a0FromOperands(i).fpB             := inData.src(0)(high, low)
    a0FromOperands(i).fpAAppend       := 0.U
    a0FromOperands(i).roundMode       := effectiveFrm(inCtrl)
    a0FromOperands(i).inCtrlFromVFMul := 0.U.asTypeOf(a0FromOperands(i).inCtrlFromVFMul)
    a0FromOperands(i).isSubFromVFMul  := false.B

    a0FromProduct(i).opcode          := VFMacOpcodes.getCtrlOpcode(ex1Opcode)
    a0FromProduct(i).fpA             := vfmuls(i).io.outToFADD.fpA
    a0FromProduct(i).fpB             := macAddend(high, low)
    a0FromProduct(i).fpAAppend       := vfmuls(i).io.outToFADD.fpAAppend
    a0FromProduct(i).roundMode       := macFrmS2
    a0FromProduct(i).inCtrlFromVFMul := vfmuls(i).io.outToFADD.FMULToFADDCtrl
    a0FromProduct(i).isSubFromVFMul  := isSubFromFmul(ex1Opcode)
  }

  private val a0Write = aluIn || macEx1
  private val a0Valid = RegNext(a0Write, false.B)
  private val a0 = RegEnable(Mux(macEx1, a0FromProduct, a0FromOperands), a0Write)

  // A0 must never be written twice in the same cycle: the add writer is at offset 0 and the mac
  // writer at offset 2, and the WB busy table blocks exactly that pair (k = lat_mac - lat_add = 2).
  assert(!(aluIn && macEx1), "VFMacWrapper: add and mac both write the FALU input register")

  // ---------------------------------------------------------------------------
  // wire up the two sub-units; their `fire` also enables the internal stage registers
  // ---------------------------------------------------------------------------
  vfmuls.zipWithIndex.foreach { case (mod, i) =>
    mod.io.fire          := r0Valid
    mod.io.in.isFMUL     := r0.isFmul
    mod.io.in.isNeg      := r0.isNeg
    mod.io.in.fp_fmt     := r0.fpFmt
    mod.io.in.fp_a       := r0.fpA(i)
    mod.io.in.fp_b       := r0.fpB(i)
    mod.io.in.round_mode := r0.frm
  }

  vfalus.zipWithIndex.foreach { case (mod, i) =>
    mod.io.fire := a0Valid
    mod.io.in   := a0(i)
  }

  private val mulResultData = Wire(Vec(numLane, UInt(laneWidth.W)))
  private val mulFflagsData = Wire(Vec(numLane, Vec(vlenb / numLane, Fflags())))
  private val faluResultData = Wire(Vec(numLane, UInt(laneWidth.W)))
  private val faluFflagsData = Wire(Vec(numLane, Vec(vlenb / numLane, Fflags())))

  vfmuls.zipWithIndex.foreach { case (mod, i) =>
    mulResultData(i) := mod.io.out.fpResult
    mulFflagsData(i) := mod.io.out.fflagsVec
  }
  vfalus.zipWithIndex.foreach { case (mod, i) =>
    faluResultData(i) := mod.io.out.fpResult
    faluFflagsData(i) := mod.io.out.fflagsVec
  }

  private val mulResultUInt = Cat(mulResultData.reverse)
  private val faluResultUInt = Cat(faluResultData.reverse)

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

  // ---------------------------------------------------------------------------
  // Release: one exit per sub-unit, timed by the op's real latency (`Opcode.getLat` ->
  // `ctrl.latency`), so each op leaves on the slot index of the ex stage it occupies then:
  //   FADD port -> out.ex(1): vfadd / vfsgnj / vfclass (latency 1) at their ex(1) stage.
  //   FMUL port -> out.ex(2): vfmul (latency 2) at its ex(2) stage.
  //   FADD port -> out.ex(3): vfmac (latency 3) at its ex(3) stage.
  // `Func` already validates every slot (`ctrl.latency === i`), and the Exu reads the merge
  // inputs and the writeback metadata (ctrl, pdest, vecWen, robIdx) of `in.ex(i)`, so all
  // three paths need nothing but their own data here.
  // ---------------------------------------------------------------------------

  out.ex(1).bits.data.vec.foreach { vecData =>
    vecData.normal := faluResultUInt
    vecData.fflagsE8.get := faluFflagsData.asTypeOf(Vec(vlenb, Fflags()))
  }

  out.ex(2).bits.data.vec.foreach { vecData =>
    vecData.normal := mulResultUInt
    vecData.fflagsE8.get := mulFflagsData.asTypeOf(Vec(vlenb, Fflags()))
  }

  out.ex(3).bits.data.vec.foreach { vecData =>
    vecData.normal := faluResultUInt
    vecData.fflagsE8.get := faluFflagsData.asTypeOf(Vec(vlenb, Fflags()))
  }
}
