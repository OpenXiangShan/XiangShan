package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.vector.fu.Func._
import xiangshan.backend.vector.fu.{Func, VecFixLatFunc, VecFuConfig}
import yunsuan.vector.Common.Fflags
import yunsuan.encoding.Opcode.Opcodes.VFMiscOpcode
import yunsuan.vector.vfalu.{VectorFALU, VectorFALUInput}

object VFAluWrapper {
  /**
    * The S1 product is not a register-file result. Keep the original OP3 uop
    * alongside it so the final result uses the normal VFALU merge path.
    */
  class OP3Context(cfg: VecFuConfig)(implicit p: Parameters) extends XSBundle {
    val ctrl = new Func.InCtrl(cfg)
    val data = new Func.InData(cfg)
    val debug = Option.when(backendParams.debugEn)(new xiangshan.backend.vector.VecRegionModule.DebugBundle)
  }

  /**
    * Exu selects OP2/OP3 and registers every VectorFALU S0 input before this
    * interface. This keeps the wide selection muxes out of VectorFALU S0.
    */
  class VFAluInput(cfg: VecFuConfig)(implicit p: Parameters) extends XSBundle {
    val isOP3 = Bool()
    val lanes = Vec(cfg.destDataBits / 64, new VectorFALUInput)
    val op3Context = new OP3Context(cfg)
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

  // OP2/OP3 selection and its timing boundary live in Exu. isOP3 is only
  // needed to select the matching writeback/merge context one cycle later.
  private val vfaluInput_s0 = in.vfaluInput.get
  private val vfaluFire_s0 = vfaluInput_s0.valid
  private val op3Fire_s0 = vfaluFire_s0 && vfaluInput_s0.bits.isOP3
  private val op3Fire_s1 = RegInit(false.B)
  op3Fire_s1 := op3Fire_s0
  private val op3Context_s0 = vfaluInput_s0.bits.op3Context
  private val op3Context_s1 = RegEnable(op3Context_s0, op3Fire_s0)

  out.op3OutContext.get.valid := op3Fire_s1 && !op3Context_s1.ctrl.robIdx.needFlush(in.flush)
  out.op3OutContext.get.bits := op3Context_s1

  private val vfalus = Seq.fill(numVecModule)(Module(new VectorFALU))

  private val resultData = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val resultDataUInt = Cat(resultData.reverse)
  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode
  private val isMaskDest = makePipeReg(
    VFMiscOpcode.isDstMask(ex0NextOpcode),
    pipeRegValids
  )
  private val fflagsData = Wire(Vec(numVecModule, Vec(vlenb / numVecModule, Fflags())))

  vfalus.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fire := vfaluFire_s0
      mod.io.in := vfaluInput_s0.bits.lanes(i)

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
    when (isMaskDest.ex(cfg.latency) && !op3Fire_s1) {
      vecData.normal := 0.U
      vecData.maskE8 := VecInit((0 until vlenb).map(i => resultDataUInt(i * 8))).asUInt
      vecData.maskE16 := VecInit((0 until vlenb / 2).map(i => resultDataUInt(i * 16))).asUInt
      vecData.maskE32 := VecInit((0 until vlenb / 4).map(i => resultDataUInt(i * 32))).asUInt
      vecData.maskE64 := VecInit((0 until vlenb / 8).map(i => resultDataUInt(i * 64))).asUInt
    }.otherwise {
      vecData.normal := resultDataUInt
    }
    vecData.fflagsE8.get := fflagsData.asTypeOf(Vec(vlenb, Fflags()))
  }

  when (op3Fire_s1) {
    out.ex(cfg.latency).bits.ctrl.robIdx     :=  op3Context_s1.ctrl.robIdx
    out.ex(cfg.latency).bits.ctrl.pdest      :=  op3Context_s1.ctrl.pdest
    out.ex(cfg.latency).bits.ctrl.pdestVl   .zip(op3Context_s1.ctrl.pdestVl  ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.rfWen     .zip(op3Context_s1.ctrl.rfWen    ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.fpWen     .zip(op3Context_s1.ctrl.fpWen    ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.vecWen    .zip(op3Context_s1.ctrl.vecWen   ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.vlWen     .zip(op3Context_s1.ctrl.vlWen    ).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.flushPipe .zip(op3Context_s1.ctrl.flushPipe).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.ctrl.fflagsWen .zip(op3Context_s1.ctrl.fflagsWen).foreach { case (sink, source) => sink := source }
    out.ex(cfg.latency).bits.debug          .zip(op3Context_s1.debug         ).foreach { case (sink, source) => sink := source }
  }

  // The normal VFALU input and an OP3 product reserve the same S0.  The
  // issue-side writeback reservation prevents this collision.
  assert(
    !(op3Fire_s0 && ex(0).valid && !ex(0).bits.ctrl.robIdx.needFlush(in.flush)),
    "VFALU received non-flushed OP2 and OP3 work in the same S0"
  )
  assert(
    op3Fire_s0 || (vfaluFire_s0 === ex(0).valid),
    "VFALU registered input is not aligned with its OP2 uop"
  )
  assert(
    !(op3Fire_s1 && ex(cfg.latency).valid && !ex(cfg.latency).bits.ctrl.robIdx.needFlush(in.flush)),
    "VFALU produced non-flushed OP2 and OP3 results in the same cycle"
  )
  out.ex(cfg.latency).valid := Mux(op3Fire_s1, out.op3OutContext.get.valid, ex(cfg.latency).valid)
}
