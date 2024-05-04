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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.DecodeLogic
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
  val srcType2 = UInt(3.W)
  val srcType1 = UInt(3.W)
  val vdType = UInt(3.W)
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
  // Cat(1(1111),1(s)/0(u), 1(add1)/0(add0))
  val uSew   = BitPat("b000")
  val uSew2  = BitPat("b001")
  val sSew   = BitPat("b010")
  val sSew2  = BitPat("b011")
  val mask = BitPat("b100".U(3.W))
  val default = List(BitPat("b000000"),BitPat("b000"),BitPat("b000"),BitPat("b000"))
  val decodeTable : Array[(BitPat, List[BitPat])] = Array(
    BitPat(VipuType.vredsum_vs)   -> List(BitPat(VAluOpcode.vredsum), uSew, uSew, uSew),
    BitPat(VipuType.vredmaxu_vs)  -> List(BitPat(VAluOpcode.vredmax), uSew, uSew, uSew),
    BitPat(VipuType.vredmax_vs)   -> List(BitPat(VAluOpcode.vredmax), sSew, sSew, sSew),
    BitPat(VipuType.vredminu_vs)  -> List(BitPat(VAluOpcode.vredmin), uSew, uSew, uSew),
    BitPat(VipuType.vredmin_vs)   -> List(BitPat(VAluOpcode.vredmin), sSew, sSew, sSew),
    BitPat(VipuType.vredand_vs)   -> List(BitPat(VAluOpcode.vredand), uSew, uSew, uSew),
    BitPat(VipuType.vredor_vs)    -> List(BitPat(VAluOpcode.vredor), uSew, uSew, uSew),
    BitPat(VipuType.vredxor_vs)   -> List(BitPat(VAluOpcode.vredxor), uSew, uSew, uSew),

    BitPat(VipuType.vwredsumu_vs) -> List(BitPat(VAluOpcode.vredsum), uSew, uSew, uSew2),
    BitPat(VipuType.vwredsum_vs)  -> List(BitPat(VAluOpcode.vredsum), sSew, sSew, sSew2),

    BitPat(VipuType.vcpop_m)      -> List(BitPat(VAluOpcode.vcpop), mask, mask, uSew),
    BitPat(VipuType.vfirst_m)     -> List(BitPat(VAluOpcode.vfirst), mask, mask, uSew),
    BitPat(VipuType.vmsbf_m)      -> List(BitPat(VAluOpcode.vmsbf), mask, mask, mask),
    BitPat(VipuType.vmsif_m)      -> List(BitPat(VAluOpcode.vmsif), mask, mask, mask),
    BitPat(VipuType.vmsof_m)      -> List(BitPat(VAluOpcode.vmsof), mask, mask, mask),

    BitPat(VipuType.viota_m)      -> List(BitPat(VAluOpcode.viota), uSew, uSew, uSew),
    BitPat(VipuType.vid_v)        -> List(BitPat(VAluOpcode.vid), uSew, uSew, uSew),
    BitPat(VipuType.vmv_x_s)      -> List(BitPat(VAluOpcode.vmvxs), uSew, uSew, uSew)
  )
  val decoder = DecodeLogic(io.in.fuOpType, default, decodeTable)
  val outsig = Seq(io.out.opcode, io.out.srcType2, io.out.srcType1, io.out.vdType)
  outsig.zip(decoder).foreach({case (s, d) => s := d})
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
  private val lmul = MuxLookup(vlmul, 1.U(4.W))(Array(
    "b001".U -> 2.U,
    "b010".U -> 4.U,
    "b011".U -> 8.U
  ))
  private val needShiftVs1 = (VipuType.vwredsumu_vs === io.in.bits.ctrl.fuOpType || VipuType.vwredsum_vs === io.in.bits.ctrl.fuOpType) && vuopIdx < lmul

  // modules
  private val decoder = Module(new VIAluDecoder)
  private val vialu   = Module(new VIAlu)

  /**
   * [[decoder]]'s in connection
   */
  decoder.io.in.fuOpType := fuOpType
  decoder.io.in.sew      := vsew(1,0)

  val typeop2 = decoder.io.out.srcType2
  val typeop1 = decoder.io.out.srcType1
  val typevd = decoder.io.out.vdType
  val sew = decoder.io.in.sew

  val srcTypeVs2 = Cat(0.U | typeop2(2) , typeop2(1) | typeop2(2) , Fill(2,typeop2(2)) | (sew + typeop2(0)) )
  val srcTypeVs1 = Cat(0.U | typeop1(2) , typeop1(1) | typeop1(2) , Fill(2,typeop1(2)) | (sew + typeop1(0)) )
  val vdType =  Cat(0.U | typevd(2) , typevd(1) | typevd(2) , Fill(2,typevd(2)) | (sew + typevd(0)))
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
      subIO.in.bits.srcType(0)  := srcTypeVs2
      subIO.in.bits.srcType(1)  := srcTypeVs1
      subIO.in.bits.vdType      := vdType
      subIO.in.bits.vs1         := Mux1H(Seq(needClearVs1 -> 0.U,
        needShiftVs1 -> ZeroExt(vs1(127,64), 128),
        ((!needClearVs1) && (!needShiftVs1)) -> vs1))
      subIO.in.bits.vs2         := vs2
      subIO.in.bits.old_vd      := oldVd
      subIO.in.bits.mask        := srcMask
  }

  io.out.bits.res.data := vialu.io.out.bits.vd
}
