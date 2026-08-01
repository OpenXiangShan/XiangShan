package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.decode.opcode.Opcode.VFMacOpcodes
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.vector.fu.Func._
import xiangshan.backend.vector.fu.{VecFixLatFunc, VecFuConfig}
import yunsuan.vector.Common.VSew
import yunsuan.vector.VectorFMA.VectorFMA

class VFMacWrapper(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {

  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val oldVdSplit = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vfmas = Seq.fill(numVecModule)(Module(new VectorFMA))

  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode
  private val ex0NextSew = VFMacOpcodes.getFormat(ex0NextOpcode)
  private val ex0VfmaInfo = ex0data.vfma.get

  private val ex0Sel16 = RegEnable(ex0NextSew === VSew.e16, ex0Next.valid)
  private val ex0Sel32 = RegEnable(ex0NextSew === VSew.e32, ex0Next.valid)
  private val ex0Sel64 = RegEnable(ex0NextSew === VSew.e64, ex0Next.valid)
  private val ex0ResWiden = RegEnable(VFMacOpcodes.isResWiden(ex0NextOpcode), ex0Next.valid)
  private val ex0IsVfmul = RegEnable(VFMacOpcodes.isFmul(ex0NextOpcode), ex0Next.valid)
  private val ex0IsVfmacc = RegEnable(VFMacOpcodes.isFmacc(ex0NextOpcode), ex0Next.valid)
  private val ex0IsVfnmacc = RegEnable(VFMacOpcodes.isFnmacc(ex0NextOpcode), ex0Next.valid)
  private val ex0IsVfmsac = RegEnable(VFMacOpcodes.isFmsac(ex0NextOpcode), ex0Next.valid)
  private val ex0IsVfnmsac = RegEnable(VFMacOpcodes.isFnmsac(ex0NextOpcode), ex0Next.valid)
  private val ex0IsVfmadd = RegEnable(VFMacOpcodes.isFmadd(ex0NextOpcode), ex0Next.valid)
  private val ex0IsVfnmadd = RegEnable(VFMacOpcodes.isFnmadd(ex0NextOpcode), ex0Next.valid)
  private val ex0IsVfmsub = RegEnable(VFMacOpcodes.isFmsub(ex0NextOpcode), ex0Next.valid)
  private val ex0IsVfnmsub = RegEnable(VFMacOpcodes.isFnmsub(ex0NextOpcode), ex0Next.valid)

  private val ex0frm = in.frm.get

  vs2Split.io.inVecData := ex0vs2
  vs1Split.io.inVecData := ex0vs1
  oldVdSplit.io.inVecData := ex0oldVd

  private val vs2Vec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val vs1Vec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val oldVdVec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val vs2WidenVec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val vs1WidenVec = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))

  for (i <- 0 until numVecModule) {
    vs2Vec(i) := vs2Split.io.outVec64b(i)
    vs1Vec(i) := vs1Split.io.outVec64b(i)
    oldVdVec(i) := oldVdSplit.io.outVec64b(i)
    vs2WidenVec(i) := Mux(ex0uopIdx(0), vs2Split.io.outVec32b(i + numVecModule), vs2Split.io.outVec32b(i)).pad(dataWidthOfDataModule)
    vs1WidenVec(i) := Mux(ex0uopIdx(0), vs1Split.io.outVec32b(i + numVecModule), vs1Split.io.outVec32b(i)).pad(dataWidthOfDataModule)
  }

  vfmas.zipWithIndex.foreach {
    case (mod, i) =>
      mod.in.ex0.valid := ex(0).valid
      mod.in.ex1Valid := ex(1).valid
      mod.in.ex2Valid := ex(2).valid
      mod.in.ex0.bits.ctrl.isVfmul := ex0IsVfmul
      mod.in.ex0.bits.ctrl.isVfmacc := ex0IsVfmacc
      mod.in.ex0.bits.ctrl.isVfnmacc := ex0IsVfnmacc
      mod.in.ex0.bits.ctrl.isVfmsac := ex0IsVfmsac
      mod.in.ex0.bits.ctrl.isVfnmsac := ex0IsVfnmsac
      mod.in.ex0.bits.ctrl.isVfmadd := ex0IsVfmadd
      mod.in.ex0.bits.ctrl.isVfnmadd := ex0IsVfnmadd
      mod.in.ex0.bits.ctrl.isVfmsub := ex0IsVfmsub
      mod.in.ex0.bits.ctrl.isVfnmsub := ex0IsVfnmsub
      mod.in.ex0.bits.ctrl.round_mode := ex0frm
      mod.in.ex0.bits.ctrl.sel16 := ex0Sel16
      mod.in.ex0.bits.ctrl.sel32 := ex0Sel32
      mod.in.ex0.bits.ctrl.sel64 := ex0Sel64
      mod.in.ex0.bits.ctrl.res_widening := ex0ResWiden

      mod.in.ex0.bits.data.fp_a := vs2Vec(i)
      mod.in.ex0.bits.data.fp_b := vs1Vec(i)
      mod.in.ex0.bits.data.fp_c := oldVdVec(i)
      mod.in.ex0.bits.data.widen_a := vs2WidenVec(i)
      mod.in.ex0.bits.data.widen_b := vs1WidenVec(i)
      mod.in.ex0.bits.data.fp_aIsFpCanonicalNAN := ex0VfmaInfo.fpAIsFpCanonicalNAN(i)
      mod.in.ex0.bits.data.fp_bIsFpCanonicalNAN := ex0VfmaInfo.fpBIsFpCanonicalNAN(i)
      mod.in.ex0.bits.data.fp_cIsFpCanonicalNAN := ex0VfmaInfo.fpCIsFpCanonicalNAN(i)

  }

  out.ex.indices.filterNot(stageIdx => stageIdx == 3).foreach { stageIdx =>
    out.ex(stageIdx).bits.data.vec.foreach {
      case vecData =>
        vecData.normal := 0.U.asTypeOf(vecData.normal)
        vecData.narrow := 0.U.asTypeOf(vecData.narrow)
        vecData.maskE8 := 0.U.asTypeOf(vecData.maskE8)
        vecData.maskE16 := 0.U.asTypeOf(vecData.maskE16)
        vecData.maskE32 := 0.U.asTypeOf(vecData.maskE32)
        vecData.maskE64 := 0.U.asTypeOf(vecData.maskE64)
        vecData.vxsatE8.foreach(_ := 0.U.asTypeOf(vecData.vxsatE8.get))
        vecData.narrowVxsatE8.foreach(_ := 0.U.asTypeOf(vecData.narrowVxsatE8.get))
        vecData.fflagsE8.foreach(_ := 0.U.asTypeOf(vecData.fflagsE8.get))
        vecData.narrowFflagsE8.foreach(_ := 0.U.asTypeOf(vecData.narrowFflagsE8.get))
    }
  }

  out.ex(3).bits.data.vec.foreach {
    case vecData =>
      vecData.normal := Cat(vfmas.map(_.out.fp_result).reverse)
      vecData.narrow := 0.U.asTypeOf(vecData.narrow)
      vecData.maskE8 := 0.U.asTypeOf(vecData.maskE8)
      vecData.maskE16 := 0.U.asTypeOf(vecData.maskE16)
      vecData.maskE32 := 0.U.asTypeOf(vecData.maskE32)
      vecData.maskE64 := 0.U.asTypeOf(vecData.maskE64)
      vecData.vxsatE8.foreach(_ := 0.U.asTypeOf(vecData.vxsatE8.get))
      vecData.narrowVxsatE8.foreach(_ := 0.U.asTypeOf(vecData.narrowVxsatE8.get))
      vecData.fflagsE8.foreach(_ := VecInit(vfmas.flatMap(_.out.fflags)))
      vecData.narrowFflagsE8.foreach(_ := 0.U.asTypeOf(vecData.narrowFflagsE8.get))
  }
}
