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
import yunsuan.vector.vfalu.VectorFALU
import yunsuan.vector.vfmul.{VFMul2VFALUCtrlBundle, VFMul2VFALUOutput}

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
  private val isOP3 = makePipeReg(VFMacOpcodes.isOP3(ex0NextOpcode), pipeRegValids)
  //  private val resSew = Wire(Vec(normalLatency + 1, UInt(ex0vsew.getWidth.W)))

  //  for (i <- 0 to cfg.latency) {
  //    resSew(i) := ex(i).bits.ctrl.vtype.get.vsew
  //  }

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

  //  val outToFaluFromFmul = out.outToVfaluFromVfmul.get

  vfalus.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fire               := ex(0).valid
      mod.io.in.opcode          := ex0opcode
      mod.io.in.fpA             := vs2Split.io.outVec64b(i)
      mod.io.in.fpB             := vs1Split.io.outVec64b(i)
      mod.io.in.fpAAppend       := 0.U
      mod.io.in.roundMode       := frm
      mod.io.in.inCtrlFromVFMul := 0.U.asTypeOf(new VFMul2VFALUCtrlBundle)
      mod.io.in.isSubFromVFMul  := false.B

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

  out.ex(cfg.latency).valid := ex(cfg.latency).valid

}
