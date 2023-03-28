/****************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ****************************************************************************************
 */


package xiangshan.backend.fu.vector

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import yunsuan.vector.alu.{VAluOpcode, VIAlu}
import yunsuan.{VectorElementFormat, VipuType}
import xiangshan.{SelImm, SrcType, UopDivType, XSCoreParamsKey, XSModule,FuType}

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
  val uSew = Cat(0.U(2.W), io.in.sew)
  val uSew2 = Cat(0.U(2.W), (io.in.sew+1.U))
  val uSewf2 = Cat(0.U(2.W), (io.in.sew-1.U))
  val uSewf4 = Cat(0.U(2.W), (io.in.sew-2.U))
  val uSewf8 = Cat(0.U(2.W), (io.in.sew-3.U))
  val sSew = Cat(1.U(2.W), io.in.sew)
  val sSew2 = Cat(1.U(2.W), (io.in.sew+1.U))
  val sSewf2 = Cat(1.U(2.W), (io.in.sew - 1.U))
  val sSewf4 = Cat(1.U(2.W), (io.in.sew - 2.U))
  val sSewf8 = Cat(1.U(2.W), (io.in.sew - 3.U))
  val mask = "b1111".U(4.W)

  val out = LookupTree(io.in.fuOpType, List(
    // --------------------- opcode       srcType2 1 vdType
    VipuType.vredsum_vs -> Cat(VAluOpcode.vredsum, uSew, uSew, uSew).asUInt(),
    VipuType.vredmaxu_vs -> Cat(VAluOpcode.vredmax, uSew, uSew, uSew).asUInt(),
    VipuType.vredmax_vs -> Cat(VAluOpcode.vredmax, sSew, sSew, sSew).asUInt(),
    VipuType.vredminu_vs -> Cat(VAluOpcode.vredmin, uSew, uSew, uSew).asUInt(),
    VipuType.vredmin_vs -> Cat(VAluOpcode.vredmin, sSew, sSew, sSew).asUInt(),
    VipuType.vredand_vs -> Cat(VAluOpcode.vredand, uSew, uSew, uSew).asUInt(),
    VipuType.vredor_vs -> Cat(VAluOpcode.vredor, uSew, uSew, uSew).asUInt(),
    VipuType.vredxor_vs -> Cat(VAluOpcode.vredxor, uSew, uSew, uSew).asUInt(),

    VipuType.vwredsumu_vs -> Cat(VAluOpcode.vredsum, uSew, uSew, uSew2).asUInt(),
    VipuType.vwredsum_vs -> Cat(VAluOpcode.vredsum, sSew, sSew, sSew2).asUInt(),

    VipuType.vcpop_m -> Cat(VAluOpcode.vcpop, mask, mask, mask).asUInt(),
    VipuType.vfirst_m -> Cat(VAluOpcode.vfirst, mask, mask, mask).asUInt(),
    VipuType.vmsbf_m -> Cat(VAluOpcode.vmsbf, mask, mask, mask).asUInt(),
    VipuType.vmsif_m -> Cat(VAluOpcode.vmsif, mask, mask, mask).asUInt(),
    VipuType.vmsof_m -> Cat(VAluOpcode.vmsof, mask, mask, mask).asUInt(),

    VipuType.viota_m -> Cat(VAluOpcode.viota, mask, mask, uSew).asUInt(),
    VipuType.vid_v -> Cat(VAluOpcode.vid, uSew, uSew, uSew).asUInt()

  )).asTypeOf(new VIAluDecodeResultBundle)

  io.out <> out
}

class VIAluWrapper(implicit p: Parameters)  extends VPUDataModule{

  needReverse := false.B
  needClearMask := false.B

  // connect VIAlu
  val decoder = Module(new VIAluDecoder)
  val vialu = Module(new VIAlu)
  decoder.io.in.fuOpType := in.uop.ctrl.fuOpType
  decoder.io.in.sew := in.uop.ctrl.vconfig.vtype.vsew(1,0)

  vialu.io.in.bits.opcode := decoder.io.out.opcode.asTypeOf(vialu.io.in.bits.opcode.cloneType)
  vialu.io.in.bits.info.vm := in.uop.ctrl.vm
  vialu.io.in.bits.info.ma := in.uop.ctrl.vconfig.vtype.vma
  vialu.io.in.bits.info.ta := in.uop.ctrl.vconfig.vtype.vta
  vialu.io.in.bits.info.vlmul := in.uop.ctrl.vconfig.vtype.vlmul
  vialu.io.in.bits.info.vl := in.uop.ctrl.vconfig.vl

  vialu.io.in.bits.info.vstart := vstart // TODO :
  vialu.io.in.bits.info.uopIdx := in.uop.ctrl.uopIdx

  vialu.io.in.bits.info.vxrm := vxrm
  vialu.io.in.bits.srcType(0) := decoder.io.out.srcType2
  vialu.io.in.bits.srcType(1) := decoder.io.out.srcType1
  vialu.io.in.bits.vdType := decoder.io.out.vdType
  vialu.io.in.bits.vs1 := vs1
  vialu.io.in.bits.vs2 := vs2
  vialu.io.in.bits.old_vd := in.src(2)
  vialu.io.in.bits.mask := mask

  vialu.io.in.valid := io.in.valid

  // connect io
  io.out.bits.data := vialu.io.out.bits.vd
  vxsat := vialu.io.out.bits.vxsat
  io.out.valid := vialu.io.out.valid
}

class VIPU(implicit p: Parameters) extends VPUSubModule(p(XSCoreParamsKey).VLEN) {
  XSError(io.in.valid && io.in.bits.uop.ctrl.fuOpType === VipuType.dummy, "VIPU OpType not supported")
  override val dataModule = Seq(Module(new VIAluWrapper))
  override val select = Seq(
    io.in.bits.uop.ctrl.fuType === FuType.vipu
  )
  connectDataModule
}
