package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.decode.opcode.Opcode.VIAluOpcodes
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.vector.fu.Func._
import xiangshan.backend.vector.fu.{VecFixLatFunc, VecFuConfig}
import yunsuan.vector.Common.VSew
import yunsuan.vector.VectorALU.VIAlu

// Todo: support double narrow
class VIAluWrapper(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {

  private val ex0opcode: UInt = fuOpType
  // params
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // modules
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs2Ex1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Ex1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vialus = Seq.fill(numVecModule)(Module(new VIAlu))

  private val resultStages = cfg.latency + 1

  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode
  private val ex0NextSew = VIAluOpcodes.getDataWidth(ex0NextOpcode)

  private val isExt2 = makePipeReg(VIAluOpcodes.isExt2(ex0NextOpcode), pipeRegValids)
  private val isExt4 = makePipeReg(VIAluOpcodes.isExt4(ex0NextOpcode), pipeRegValids)
  private val isExt8 = makePipeReg(VIAluOpcodes.isExt8(ex0NextOpcode), pipeRegValids)

  private val sel8  = makePipeReg(ex0NextSew === VSew.e8 , pipeRegValids)
  private val sel16 = makePipeReg(ex0NextSew === VSew.e16, pipeRegValids)
  private val sel32 = makePipeReg(ex0NextSew === VSew.e32, pipeRegValids)
  private val sel64 = makePipeReg(ex0NextSew === VSew.e64, pipeRegValids)

  private val isWiden     = makePipeReg(VIAluOpcodes.isWiden         (ex0NextOpcode), pipeRegValids)
  private val isWidenVs2  = makePipeReg(VIAluOpcodes.isWidenVs2      (ex0NextOpcode), pipeRegValids)
  private val isSigned    = makePipeReg(VIAluOpcodes.isSigned        (ex0NextOpcode), pipeRegValids)
  private val isAddCarry  = makePipeReg(VIAluOpcodes.isAddCarry      (ex0NextOpcode), pipeRegValids)
  private val isNarrow    = makePipeReg(VIAluOpcodes.isNarrow        (ex0NextOpcode), pipeRegValids)
  private val isSub       = makePipeReg(VIAluOpcodes.isSub           (ex0NextOpcode), pipeRegValids)
  private val isCmpEq     = makePipeReg(VIAluOpcodes.isCmpEq         (ex0NextOpcode), pipeRegValids)
  private val isCmpNe     = makePipeReg(VIAluOpcodes.isCmpNe         (ex0NextOpcode), pipeRegValids)
  private val isCmpLt     = makePipeReg(VIAluOpcodes.isCmpLt         (ex0NextOpcode), pipeRegValids)
  private val isCmpLe     = makePipeReg(VIAluOpcodes.isCmpLe         (ex0NextOpcode), pipeRegValids)
  private val isCmpGt     = makePipeReg(VIAluOpcodes.isCmpGt         (ex0NextOpcode), pipeRegValids)
  private val isVmsbc     = makePipeReg(VIAluOpcodes.isVmsbc         (ex0NextOpcode), pipeRegValids)
  private val isAvg       = makePipeReg(VIAluOpcodes.isAvg           (ex0NextOpcode), pipeRegValids)
  private val isShiftLeft = makePipeReg(VIAluOpcodes.isLeftShiftLogic(ex0NextOpcode), pipeRegValids)
  private val isCtz       = makePipeReg(VIAluOpcodes.isCtz           (ex0NextOpcode), pipeRegValids)
  private val isSat       = makePipeReg(VIAluOpcodes.isSat           (ex0NextOpcode), pipeRegValids)
  private val isMaxMin    = makePipeReg(VIAluOpcodes.isMaxMin        (ex0NextOpcode), pipeRegValids)
  private val isMax       = makePipeReg(VIAluOpcodes.isMax           (ex0NextOpcode), pipeRegValids)
  private val isNClip     = makePipeReg(VIAluOpcodes.isNClip         (ex0NextOpcode), pipeRegValids)

  vs2Split.io.inVecData := ex0vs2
  vs1Split.io.inVecData := ex0vs1
  vs2Ex1Split.io.inVecData := vs2Ex(1)
  vs1Ex1Split.io.inVecData := vs1Ex(1)

  private val vs2Vec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))
  private val vs1Vec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))
  private val vs2WidenVec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))
  private val vs1WidenVec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))

  for (i <- 0 until numVecModule) {
    vs2Vec(i) := vs2Split.io.outVec64b(i)
    vs1Vec(i) := vs1Split.io.outVec64b(i)
    vs2WidenVec(i) := Mux(ex0uopIdx(0), vs2Split.io.outVec32b(i + 2), vs2Split.io.outVec32b(i))
    vs1WidenVec(i) := Mux(ex0uopIdx(0), vs1Split.io.outVec32b(i + 2), vs1Split.io.outVec32b(i))
  }

  vialus.zipWithIndex.foreach {
    case (mod, i) =>
      mod.in.vxrm.bits := vxrm

      mod.in.ex0.valid := ex(0).valid
      mod.in.ex0.bits.ctrl.opcode := ex0opcode
      mod.in.ex0.bits.ctrl.adder match { case adderCtrl =>
        adderCtrl.sel8       := sel8.ex0
        adderCtrl.sel16      := sel16.ex0
        adderCtrl.sel32      := sel32.ex0
        adderCtrl.sel64      := sel64.ex0
        adderCtrl.widenVs2   := isWidenVs2.ex0
        adderCtrl.widen      := isWiden.ex0
        adderCtrl.isSigned   := isSigned.ex0
        adderCtrl.isAddCarry := isAddCarry.ex0
        adderCtrl.isSub      := isSub.ex0
        adderCtrl.isCmpEq    := isCmpEq.ex0
        adderCtrl.isCmpNe    := isCmpNe.ex0
        adderCtrl.isCmpLt    := isCmpLt.ex0
        adderCtrl.isCmpLe    := isCmpLe.ex0
        adderCtrl.isCmpGt    := isCmpGt.ex0
        adderCtrl.isVmsbc    := isVmsbc.ex0
        adderCtrl.isAvg      := isAvg.ex0
        adderCtrl.vm         := ex0vm
      }

      mod.in.ex0.bits.ctrl.misc match { case miscCtrl =>
        miscCtrl.sel8             := sel8.ex0
        miscCtrl.sel16            := sel16.ex0
        miscCtrl.sel32            := sel32.ex0
        miscCtrl.sel64            := sel64.ex0
        miscCtrl.isVf2            := isExt2.ex0
        miscCtrl.isVf4            := isExt4.ex0
        miscCtrl.isVf8            := isExt8.ex0
        miscCtrl.widenVs2         := isWidenVs2.ex0
        miscCtrl.widen            := isWiden.ex0
        miscCtrl.isNarrow         := isNarrow.ex0
        miscCtrl.isSigned         := isSigned.ex0
        miscCtrl.isLeftShiftLogic := isShiftLeft.ex0
        miscCtrl.isCtz            := isCtz.ex0
      }

      mod.in.ex0.bits.data match { case ex0Data =>
        ex0Data.vs1 := vs1Vec(i)
        ex0Data.vs2 := vs2Vec(i)
        ex0Data.vs2Widen := vs2WidenVec(i)
        ex0Data.vs1Widen := vs1WidenVec(i)
        ex0Data.mask := (~0.U).asUInt // Todo: support vlenb mask
      }

      mod.in.ex1.valid := ex(1).valid
      mod.in.ex1.bits.ctrl.opcode := ex(1).bits.ctrl.opcode
      mod.in.ex1.bits.ctrl.fixPoint match { case fixPointCtrl =>
        fixPointCtrl.sel8     := sel8.ex1
        fixPointCtrl.sel16    := sel16.ex1
        fixPointCtrl.sel32    := sel32.ex1
        fixPointCtrl.sel64    := sel64.ex1
        fixPointCtrl.isSigned := isSigned.ex1
        fixPointCtrl.isSat    := isSat.ex1
        fixPointCtrl.isMaxMin := isMaxMin.ex1
        fixPointCtrl.isMax    := isMax.ex1
        fixPointCtrl.isNClip  := isNClip.ex1
      }
      mod.in.ex1.bits.data.vs2 := vs2Ex1Split.io.outVec64b(i)
      mod.in.ex1.bits.data.vs1 := vs1Ex1Split.io.outVec64b(i)
  }

  out.ex(0).bits.data.vec.foreach {
    case vecData =>
      vecData.normal := Cat(vialus.map(_.out.ex0.vd).reverse)
      vecData.narrow := Cat(vialus.map(_.out.ex0.narrowVd).reverse)
      vecData.maskE8 := Cat(vialus.map(_.out.ex0.mask.e8).reverse)
      vecData.maskE16 := Cat(vialus.map(_.out.ex0.mask.e16).reverse)
      vecData.maskE32 := Cat(vialus.map(_.out.ex0.mask.e32).reverse)
      vecData.maskE64 := Cat(vialus.map(_.out.ex0.mask.e64).reverse)
      vecData.isWiden.get := isWiden.ex0
      vecData.isNarrow.get := isNarrow.ex0
      vecData.vxsatE8.get := vialus.flatMap(_.out.ex0.vxsat.asBools)
      vecData.narrowVxsatE8.get := vialus.flatMap(_.out.ex0.narrowVxsat.asBools)
  }

  out.ex(1).bits.data.vec.foreach {
    case vecData =>
      vecData.normal := Cat(vialus.map(_.out.ex1.vd).reverse)
      vecData.narrow := Cat(vialus.map(_.out.ex1.narrowVd).reverse)
      vecData.maskE8 := Cat(vialus.map(_.out.ex1.mask.e8).reverse)
      vecData.maskE16 := Cat(vialus.map(_.out.ex1.mask.e16).reverse)
      vecData.maskE32 := Cat(vialus.map(_.out.ex1.mask.e32).reverse)
      vecData.maskE64 := Cat(vialus.map(_.out.ex1.mask.e64).reverse)
      vecData.isWiden.get := isWiden.ex1
      vecData.isNarrow.get := isNarrow.ex1
      vecData.vxsatE8.get := vialus.flatMap(_.out.ex1.vxsat.asBools)
      vecData.narrowVxsatE8.get := vialus.flatMap(_.out.ex1.narrowVxsat.asBools)
  }
}
