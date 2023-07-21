/****************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*       http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
****************************************************************************************
*/


package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import utils.XSError
import xiangshan.{SelImm, SrcType, UopSplitType, XSCoreParamsKey, XSModule}
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, Utils, VecPipedFuncUnit, VecSrcTypeModule}
import xiangshan.SrcType
import yunsuan.vector.alu.{VAluOpcode, VIAlu}
import yunsuan.{OpType, VipuType}
import scala.collection.Seq

class VIAluDecodeResultBundle extends Bundle {
  val opcode = UInt(6.W)
  val srcType2 = UInt(4.W)
  val srcType1 = UInt(4.W)
  val vdType = UInt(4.W)
}

class VIAluDecoder (implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle{
    val in = Input(new Bundle{
      val fuOpType = UInt(8.W)
      val sew = UInt(2.W)
    })
    val out = Output(new VIAluDecodeResultBundle)
  })

  // u 00 s 01 f 10 mask 1111
  val uSew   = Cat(0.U(2.W), io.in.sew)
  val uSew2  = Cat(0.U(2.W), (io.in.sew+1.U))
  val uSewf2 = Cat(0.U(2.W), (io.in.sew-1.U))
  val uSewf4 = Cat(0.U(2.W), (io.in.sew-2.U))
  val uSewf8 = Cat(0.U(2.W), (io.in.sew-3.U))
  val sSew   = Cat(1.U(2.W), io.in.sew)
  val sSew2  = Cat(1.U(2.W), (io.in.sew+1.U))
  val sSewf2 = Cat(1.U(2.W), (io.in.sew - 1.U))
  val sSewf4 = Cat(1.U(2.W), (io.in.sew - 2.U))
  val sSewf8 = Cat(1.U(2.W), (io.in.sew - 3.U))
  val mask   = "b1111".U(4.W)

  val out = LookupTree(io.in.fuOpType, List(
    // --------------------- opcode     srcType2 1 vdType
    VipuType.vredsum_vs   -> Cat(VAluOpcode.vredsum, uSew, uSew, uSew).asUInt(),
    VipuType.vredmaxu_vs  -> Cat(VAluOpcode.vredmax, uSew, uSew, uSew).asUInt(),
    VipuType.vredmax_vs   -> Cat(VAluOpcode.vredmax, sSew, sSew, sSew).asUInt(),
    VipuType.vredminu_vs  -> Cat(VAluOpcode.vredmin, uSew, uSew, uSew).asUInt(),
    VipuType.vredmin_vs   -> Cat(VAluOpcode.vredmin, sSew, sSew, sSew).asUInt(),
    VipuType.vredand_vs   -> Cat(VAluOpcode.vredand, uSew, uSew, uSew).asUInt(),
    VipuType.vredor_vs    -> Cat(VAluOpcode.vredor, uSew, uSew, uSew).asUInt(),
    VipuType.vredxor_vs   -> Cat(VAluOpcode.vredxor, uSew, uSew, uSew).asUInt(),

    VipuType.vwredsumu_vs -> Cat(VAluOpcode.vredsum, uSew, uSew, uSew2).asUInt(),
    VipuType.vwredsum_vs  -> Cat(VAluOpcode.vredsum, sSew, sSew, sSew2).asUInt(),

    VipuType.vcpop_m      -> Cat(VAluOpcode.vcpop, mask, mask, uSew).asUInt(),
    VipuType.vfirst_m     -> Cat(VAluOpcode.vfirst, mask, mask, uSew).asUInt(),
    VipuType.vmsbf_m      -> Cat(VAluOpcode.vmsbf, mask, mask, mask).asUInt(),
    VipuType.vmsif_m      -> Cat(VAluOpcode.vmsif, mask, mask, mask).asUInt(),
    VipuType.vmsof_m      -> Cat(VAluOpcode.vmsof, mask, mask, mask).asUInt(),

    VipuType.viota_m      -> Cat(VAluOpcode.viota, uSew, uSew, uSew).asUInt(),
    VipuType.vid_v        -> Cat(VAluOpcode.vid, uSew, uSew, uSew).asUInt()

   )).asTypeOf(new VIAluDecodeResultBundle)

   io.out <> out
}

class VIPU(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VipuType.dummy, "VIPU OpType not supported")

  // params alias
  private val dataWidth = cfg.dataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule
  private val needClearVs1 = (VipuType.vcpop_m === io.in.bits.ctrl.fuOpType && vuopIdx === 0.U) ||
                             (VipuType.viota_m === io.in.bits.ctrl.fuOpType && vuopIdx(log2Up(MaxUopSize)-1,1) === 0.U) ||
                             (VipuType.vid_v   === io.in.bits.ctrl.fuOpType && vuopIdx(log2Up(MaxUopSize)-1,1) === 0.U)    // dirty code TODO:  inset into IAlu
  private val needShiftVs1 = (VipuType.vwredsumu_vs === io.in.bits.ctrl.fuOpType || VipuType.vwredsum_vs === io.in.bits.ctrl.fuOpType) && vuopIdx < vlmul
  
  // modules
  private val decoder = Module(new VIAluDecoder)
  private val vialu   = Module(new VIAlu)
  
  /**
    * [[decoder]]'s in connection
    */
  decoder.io.in.fuOpType := fuOpType
  decoder.io.in.sew      := vsew(1,0)


  /**
    * [[vialu]]'s in connection
    */
  vialu.io match {
    case subIO =>
      subIO.in.valid            := io.in.valid
      subIO.in.bits.opcode.op   := decoder.io.out.opcode
      subIO.in.bits.info.vm     := vm
      subIO.in.bits.info.ma     := vma
      subIO.in.bits.info.ta     := vta
      subIO.in.bits.info.vlmul  := vlmul
      subIO.in.bits.info.vl     := srcVConfig.vl
      subIO.in.bits.info.vstart := vstart
      subIO.in.bits.info.uopIdx := vuopIdx
      subIO.in.bits.info.vxrm   := vxrm
      subIO.in.bits.srcType(0)  := decoder.io.out.srcType2
      subIO.in.bits.srcType(1)  := decoder.io.out.srcType1
      subIO.in.bits.vdType      := decoder.io.out.vdType
      subIO.in.bits.vs1         :=  Mux1H(Seq(needClearVs1 -> 0.U,
                                              needShiftVs1 -> SignExt(vs1(127,64), 128),
                                           ((!needClearVs1) && (!needShiftVs1)) -> vs1))
      subIO.in.bits.vs2         := vs2
      subIO.in.bits.old_vd      := oldVd
      subIO.in.bits.mask        := srcMask
  }

  io.out.bits.res.data := vialu.io.out.bits.vd
}
