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

  // VectorFALU S0 accepts the VFMUL S1 product.  Keep the original uop
  // alongside it so MergeUnit observes the matching oldVd/vl/vtype context.
  private val op3Fire_s0 = in.fromVfmul.get.valid
  private val op3Fire_s1 = RegInit(false.B)
  op3Fire_s1 := op3Fire_s0
  private val op3Context_s0 = in.fromVfmul.get.bits
  private val op3Context_s1 = RegEnable(op3Context_s0, op3Fire_s0)

  out.op3OutContext.get.valid := op3Fire_s1 && !op3Context_s0.ctrl.robIdx.needFlush(in.flush)
  out.op3OutContext.get.bits := op3Context_s1

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

  vfalus.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fire               := op3Fire_s0 || ex(0).valid
      mod.io.in.opcode          := Mux(op3Fire_s0, op3Context_s0.ctrlOpcode,            fuOpType                               )
      mod.io.in.fpA             := Mux(op3Fire_s0, op3Context_s0.mul(i).fpA,            Mux(ex0ctrl.isReverse, vs1Split.io.outVec64b(i), vs2Split.io.outVec64b(i)))
      mod.io.in.fpB             := Mux(op3Fire_s0, op3Context_s0.addend(i),             Mux(ex0ctrl.isReverse, vs2Split.io.outVec64b(i), vs1Split.io.outVec64b(i)))
      mod.io.in.fpAAppend       := Mux(op3Fire_s0, op3Context_s0.mul(i).fpAAppend,      0.U                                    )
      mod.io.in.roundMode       := Mux(op3Fire_s0, op3Context_s0.frm,                   frm                                    )
      mod.io.in.inCtrlFromVFMul := Mux(op3Fire_s0, op3Context_s0.mul(i).FMULToFADDCtrl, 0.U.asTypeOf(new VFMul2VFALUCtrlBundle))
      mod.io.in.isSubFromVFMul  := Mux(op3Fire_s0, op3Context_s0.isSub,                 false.B                                )

      resultData(i) := mod.io.out.fpResult
      fflagsData(i) := mod.io.out.fflagsVec
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

  when (op3Fire_s1) {
    out.ex(cfg.latency).bits.ctrl.robIdx     :=  op3Context_s1.ctrl.robIdx
    out.ex(cfg.latency).bits.ctrl.pdest      :=  op3Context_s1.ctrl.pdest
    out.ex(cfg.latency).bits.ctrl.pdestV0   .zip(op3Context_s1.ctrl.pdestV0  ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.pdestVl   .zip(op3Context_s1.ctrl.pdestVl  ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.rfWen     .zip(op3Context_s1.ctrl.rfWen    ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.fpWen     .zip(op3Context_s1.ctrl.fpWen    ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.vecWen    .zip(op3Context_s1.ctrl.vecWen   ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.v0Wen     .zip(op3Context_s1.ctrl.v0Wen    ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.vlWen     .zip(op3Context_s1.ctrl.vlWen    ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.flushPipe .zip(op3Context_s1.ctrl.flushPipe).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.fflagsWen .zip(op3Context_s1.ctrl.fflagsWen).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.debug          .zip(op3Context_s1.debug         ).foreach { case (sink, source) => sink := source }
  }

  // The normal VFALU input and an OP3 product reserve the same S0.  The
  // issue-side writeback reservation prevents this collision.
  assert(!(op3Fire_s0 && ex(0).valid), "VFALU received OP2 and OP3 work in the same S0")
  assert(!(op3Fire_s1 && ex(cfg.latency).valid), "VFALU produced OP2 and OP3 results in the same cycle")
  out.ex(cfg.latency).valid := Mux(op3Fire_s1, out.op3OutContext.get.valid, ex(cfg.latency).valid)
}
