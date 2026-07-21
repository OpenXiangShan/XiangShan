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
import xiangshan.backend.vector.fu.{VecFixLatFunc, VecFuConfig}
import yunsuan.vector.Common.{Fflags, VSew}
import yunsuan.vector.vfmul.{VectorFMUL, VFMul2VFALUOutput}
import yunsuan.vector.vfmul.utils.VFAlgoUtils

class VFMulWrapper(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {

  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule
  private val normalLatency = 2
  require(cfg.latency >= normalLatency)

  private val vlenb = VLEN / 8
  private val zeroFflags = 0.U.asTypeOf(Fflags())
  private val zeroFflagsE8 = VecInit(Seq.fill(vlenb)(zeroFflags))
  private val zeroNarrowFflagsE8 = VecInit(Seq.fill(vlenb / 2)(zeroFflags))

  private val ex0opcode = fuOpType
  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode
  private val ex0vsew = ex0ctrl.vtype.get.vsew
  private val isFmul = makePipeReg(VFMacOpcodes.isFmul(ex0NextOpcode), pipeRegValids)
  private val isOP3 = makePipeReg(VFMacOpcodes.isOP3(ex0NextOpcode), pipeRegValids)
  private val op3MulUsesOldVd =
    VFMacOpcodes.isFmadd(ex0opcode) || VFMacOpcodes.isFnmadd(ex0opcode) ||
    VFMacOpcodes.isFmsub(ex0opcode) || VFMacOpcodes.isFnmsub(ex0opcode)
  private val op3NegProduct =
    VFMacOpcodes.isFnmadd(ex0opcode) || VFMacOpcodes.isFnmsub(ex0opcode) ||
    VFMacOpcodes.isFnmacc(ex0opcode) || VFMacOpcodes.isFnmsac(ex0opcode)
//  private val resSew = Wire(Vec(normalLatency + 1, UInt(ex0vsew.getWidth.W)))

//  for (i <- 0 to cfg.latency) {
//    resSew(i) := ex(i).bits.ctrl.vtype.get.vsew
//  }

  private val vfmuls = Seq.fill(numVecModule)(Module(new VectorFMUL)) // WARNING: Don't change this module. MUST USE VectorFloatMultiplier
  val toVfalu = IO(Output(Valid(Vec(numVecModule, new VFMul2VFALUOutput))))

  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val oldVdSplit = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))

  vs2Split.io.inVecData := ex0vs2
  vs1Split.io.inVecData := ex0vs1
  oldVdSplit.io.inVecData := ex0oldVd

  private val resultData = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val fflagsData = Wire(Vec(numVecModule, Vec(vlenb / numVecModule, Fflags())))

//  val outToFaluFromFmul = out.outToVfaluFromVfmul.get

  vfmuls.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fire             := ex(0).valid
      mod.io.in.isFMUL        := isFmul.ex0
      mod.io.in.isNeg         := op3NegProduct
      mod.io.in.fp_fmt        := VFMacOpcodes.getDataType(ex0opcode)
      mod.io.in.fp_a          := vs1Split.io.outVec64b(i)
      mod.io.in.fp_b          := Mux(op3MulUsesOldVd, oldVdSplit.io.outVec64b(i), vs2Split.io.outVec64b(i))
      mod.io.in.round_mode    := frm

      toVfalu.bits(i) := mod.io.outToFADD

      resultData(i) := mod.io.out.fpResult
      fflagsData(i) := mod.io.out.fflagsVec
  }
  // VectorFMUL S1 is the unrounded product consumed by the vector adder.
  // OP2 multiplication must never enter this path.
  toVfalu.valid := ex(1).valid && isOP3.ex(1)

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

  out.ex(normalLatency).bits.data.vec.foreach { vecData =>
    vecData.normal := Cat(resultData.reverse)
    vecData.fflagsE8.get := fflagsData.asTypeOf(Vec(vlenb, Fflags()))
  }

  out.ex(normalLatency).valid := ex(normalLatency).valid &&
    ex(normalLatency).bits.ctrl.latency === normalLatency.U &&
    isFmul.ex(normalLatency)

}
