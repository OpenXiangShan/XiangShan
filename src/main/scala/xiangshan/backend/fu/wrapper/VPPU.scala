package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, Utils, VecPipedFuncUnit, VecSrcTypeModule}
import xiangshan.SrcType
import yunsuan.encoding.Opcode.VimacOpcode
import yunsuan.encoding.{VdType, Vs1IntType, Vs2IntType}
import yunsuan.{OpType, VpermType}
import yunsuan.vector.perm.Permutation

class VPermSrcTypeModule extends VecSrcTypeModule {
  private val srcVdType = Wire(new Bundle{
    val srcType2 = UInt(4.W)
    val srcType1 = UInt(4.W)
    val vdType = UInt(4.W)
  })
  srcVdType := VpermType.getSrcVdType(fuOpType, vsew)

  io.out.vs2Type := srcVdType.srcType2
  io.out.vs1Type := srcVdType.srcType1
  io.out.vdType  := srcVdType.vdType
}

class VPPU(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VpermType.dummy, "VpermType OpType not supported")

  // params alias
  private val dataWidth = cfg.dataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule
  private val vppuNeedClearMask = (VpermType.vcompress === io.in.bits.ctrl.fuOpType) && (vuopIdx(log2Up(MaxUopSize)-1,1) === 0.U)
  private val mask = Mux(vppuNeedClearMask, 0.U, srcMask)

  // io alias
  private val opcode  = VpermType.getOpcode(fuOpType)
  
  // modules
  private val typeMod = Module(new VIMacSrcTypeModule)
  private val vperms = Module(new Permutation)

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
    * [[vperms]]'s in connection
    */
  vperms.io match {
    case subIO =>
      subIO.in.valid            := io.in.valid
      subIO.in.bits.opcode.op   := opcode
      subIO.in.bits.info.vm     := vm
      subIO.in.bits.info.ma     := vma
      subIO.in.bits.info.ta     := vta
      subIO.in.bits.info.vlmul  := vlmul
      subIO.in.bits.info.vl     := srcVConfig.vl
      subIO.in.bits.info.vstart := vstart
      subIO.in.bits.info.uopIdx := vuopIdx
      subIO.in.bits.info.vxrm   := vxrm
      subIO.in.bits.srcType(0)  := typeMod.io.out.vs2Type
      subIO.in.bits.srcType(1)  := typeMod.io.out.vs1Type
      subIO.in.bits.vdType      := typeMod.io.out.vdType
      subIO.in.bits.vs1         := vs1
      subIO.in.bits.vs2         := vs2
      subIO.in.bits.old_vd      := oldVd
      subIO.in.bits.mask        := mask
  }

  io.out.bits.res.data := vperms.io.out.vd
  io.out.bits.res.vxsat.foreach(_ := vperms.io.out.vxsat)
}
