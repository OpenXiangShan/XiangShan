package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, Utils, VecPipedFuncUnit, VecSrcTypeModule}
import xiangshan.ExceptionNO
import yunsuan.VialuFixType
import yunsuan.encoding.Opcode.VimacOpcode
import yunsuan.encoding.{VdType, Vs1IntType, Vs2IntType}
import yunsuan.{OpType, VimacType}
import yunsuan.vector.mac.VIMac64b

class VIMacSrcTypeModule extends VecSrcTypeModule {

  private val opcode  = VimacType.getOpcode(fuOpType)
  private val vs2Sign = VimacType.vs2Sign(fuOpType)
  private val vs1Sign = VimacType.vs1Sign(fuOpType)
  private val vdSign  = VimacType.vdSign(fuOpType)
  private val format  = VimacType.getFormat(fuOpType)
  private val widen   = format === VimacType.FMT.VVW

  private val vs2IntType = Cat(0.U(1.W), vs2Sign)
  private val vs1IntType = Cat(0.U(1.W), vs1Sign)
  private val vdIntType  = Cat(0.U(1.W),  vdSign)

  private val vsewX2 = vsew + 1.U

  private val vs2Type = Cat(vs2IntType, vsew)
  private val vs1Type = Cat(vs1IntType, vsew)
  private val vdType  = Cat( vdIntType, Mux(widen, vsewX2, vsew))

  private val widenIllegal = widen && vsewX2 === VSew.e8

  io.out.illegal := widenIllegal
  io.out.vs2Type := vs2Type
  io.out.vs1Type := vs1Type
  io.out.vdType  := vdType
}

class VIMacU(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VimacType.dummy, "VialuF OpType not supported")

  // params alias
  private val dataWidth = cfg.dataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // io alias
  private val opcode  = VimacType.getOpcode(fuOpType)
  private val format  = VimacType.getFormat(fuOpType)
  private val widen   = format === VimacType.FMT.VVW
  private val exchangeVs2Vd = VimacOpcode.overWriteMultiplicand(opcode)

  // modules
  private val typeMod = Module(new VIMacSrcTypeModule)
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val oldVdSplit  = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vimacs = Seq.fill(numVecModule)(Module(new VIMac64b))
  private val mgu = Module(new Mgu(dataWidth))

  /**
    * [[typeMod]]'s in connection
    */
  typeMod.io.in.fuOpType := fuOpType
  typeMod.io.in.vsew := vsew
  typeMod.io.in.isReverse := isReverse
  typeMod.io.in.isExt := isExt
  typeMod.io.in.isDstMask := vecCtrl.isDstMask
  typeMod.io.in.isMove := isMove

  /**
    * In connection of [[vs2Split]], [[vs1Split]] and [[oldVdSplit]]
    */
  vs2Split.io.inVecData := vs2
  vs1Split.io.inVecData := vs1
  oldVdSplit.io.inVecData := oldVd

  /**
    * [[vimacs]]'s in connection
    */
  // Vec(vs2(31,0), vs2(63,32), vs2(95,64), vs2(127,96)) ==>
  // Vec(
  //   Cat(vs2(95,64),  vs2(31,0)),
  //   Cat(vs2(127,96), vs2(63,32)),
  // )
  private val vs2GroupedVec: Vec[UInt] = VecInit(vs2Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val vs1GroupedVec: Vec[UInt] = VecInit(vs1Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)

  private val vs2VecUsed: Vec[UInt] = Mux(widen, vs2GroupedVec, vs2Split.io.outVec64b)
  private val vs1VecUsed: Vec[UInt] = Mux(widen, vs1GroupedVec, vs1Split.io.outVec64b)
  private val oldVdVecUsed: Vec[UInt] = WireInit(oldVdSplit.io.outVec64b)

  vimacs.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.info.vm     := vm
      mod.io.info.ma     := vma
      mod.io.info.ta     := vta
      mod.io.info.vlmul  := vlmul
      mod.io.info.vl     := srcVConfig.vl
      mod.io.info.vstart := vstart
      mod.io.info.uopIdx := vuopIdx
      mod.io.info.vxrm   := vxrm
      mod.io.srcType(0)  := typeMod.io.out.vs2Type
      mod.io.srcType(1)  := typeMod.io.out.vs1Type
      mod.io.vdType      := typeMod.io.out.vdType
      mod.io.vs1         := vs1VecUsed(i)
      mod.io.vs2         := Mux(!exchangeVs2Vd, vs2VecUsed(i), oldVdVecUsed(i))
      mod.io.oldVd       := Mux(!exchangeVs2Vd, oldVdVecUsed(i), vs2VecUsed(i))
      mod.io.highHalf    := VimacOpcode.highHalf(opcode)
      mod.io.isMacc      := VimacOpcode.isMacc(opcode)
      mod.io.isSub       := VimacOpcode.isSub(opcode)
      mod.io.widen       := widen
      mod.io.isFixP      := VimacOpcode.isFixP(opcode)
  }

  /**
    * [[mgu]]'s in connection
    */
  private val outVd = Cat(vimacs.reverse.map(_.io.vd))
  private val outFormat = VimacType.getFormat(outCtrl.fuOpType)
  private val outWiden = outFormat === VimacType.FMT.VVW

  private val outEew = Mux(outWiden, outVecCtrl.vsew + 1.U, outVecCtrl.vsew)
  mgu.io.in.vd := outVd
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := outSrcMask
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := outVl
  mgu.io.in.info.vlmul := outVecCtrl.vlmul
  mgu.io.in.info.valid := io.out.valid
  mgu.io.in.info.vstart := outVecCtrl.vstart
  mgu.io.in.info.eew := outEew
  mgu.io.in.info.vsew := outVecCtrl.vsew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.narrow := 0.B
  mgu.io.in.info.dstMask := 0.B
  mgu.io.in.isIndexedVls := false.B

  io.out.bits.res.data := mgu.io.out.vd
  io.out.bits.res.vxsat.get := (Cat(vimacs.map(_.io.vxsat)) & mgu.io.out.keep).orR
  io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := mgu.io.out.illegal
}
