/***************************************************************************************
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
***************************************************************************************/

package xiangshan.backend.decode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.DecodeLogic
import freechips.rocketchip.rocket.Instructions._
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.fu.vector.Bundles.{VSew, VLmul}
import xiangshan.backend.Bundles.VPUCtrlSignals
import xiangshan.{FPUCtrlSignals, XSModule}

class FPToVecDecoder(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val instr = Input(UInt(32.W))
    val vpuCtrl = Output(new VPUCtrlSignals)
  })

  val inst = io.instr.asTypeOf(new XSInstBitFields)
  val fpToVecInsts = Seq(
    FADD_S, FSUB_S, FADD_D, FSUB_D, FADD_H, FSUB_H,
    FEQ_S, FLT_S, FLE_S, FEQ_D, FLT_D, FLE_D, FEQ_H, FLT_H, FLE_H,
    FMIN_S, FMAX_S, FMIN_D, FMAX_D, FMIN_H, FMAX_H,
    FMUL_S, FMUL_D, FMUL_H,
    FDIV_S, FDIV_D, FSQRT_S, FSQRT_D, FDIV_H, FSQRT_H,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S, FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D, FMADD_H, FMSUB_H, FNMADD_H, FNMSUB_H,
    FCLASS_S, FCLASS_D, FSGNJ_S, FSGNJ_D, FSGNJX_S, FSGNJX_D, FSGNJN_S, FSGNJN_D,
    FCLASS_H, FSGNJ_H, FSGNJX_H, FSGNJN_H,
    // scalar cvt inst
    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D, FCVT_S_D, FCVT_D_S,
    FCVT_S_H, FCVT_H_S, FCVT_H_D, FCVT_D_H,
    FMV_X_W, FMV_X_D, FMV_X_H,
    FCVT_W_H, FCVT_WU_H, FCVT_L_H, FCVT_LU_H,
    // zfa inst
    FLEQ_H, FLEQ_S, FLEQ_D, FLTQ_H, FLTQ_S, FLTQ_D, FMINM_H, FMINM_S, FMINM_D, FMAXM_H, FMAXM_S, FMAXM_D,
    FROUND_H, FROUND_S, FROUND_D, FROUNDNX_H, FROUNDNX_S, FROUNDNX_D, FCVTMOD_W_D,
  )
  val isFpToVecInst = fpToVecInsts.map(io.instr === _).reduce(_ || _)
  val isFP16Instrs = Seq(
    // zfh inst
    FADD_H, FSUB_H, FEQ_H, FLT_H, FLE_H, FMIN_H, FMAX_H,
    FMUL_H, FDIV_H, FSQRT_H,
    FMADD_H, FMSUB_H, FNMADD_H, FNMSUB_H,
    FCLASS_H, FSGNJ_H, FSGNJX_H, FSGNJN_H,
    // zfa inst
    FLEQ_H, FLTQ_H, FMINM_H, FMAXM_H,
    FROUND_H, FROUNDNX_H,
  )
  val isFP16Instr = isFP16Instrs.map(io.instr === _).reduce(_ || _)
  val isFP32Instrs = Seq(
    FADD_S, FSUB_S, FEQ_S, FLT_S, FLE_S, FMIN_S, FMAX_S,
    FMUL_S, FDIV_S, FSQRT_S,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S,
    FCLASS_S, FSGNJ_S, FSGNJX_S, FSGNJN_S,
    // zfa inst
    FLEQ_S, FLTQ_S, FMINM_S, FMAXM_S,
    FROUND_S, FROUNDNX_S,
  )
  val isFP32Instr = isFP32Instrs.map(io.instr === _).reduce(_ || _)
  val isFP64Instrs = Seq(
    FADD_D, FSUB_D, FEQ_D, FLT_D, FLE_D, FMIN_D, FMAX_D,
    FMUL_D, FDIV_D, FSQRT_D,
    FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D,
    FCLASS_D, FSGNJ_D, FSGNJX_D, FSGNJN_D,
  )
  val isFP64Instr = isFP64Instrs.map(io.instr === _).reduce(_ || _)
  // scalar cvt inst
  val isSew2Cvts = Seq(
    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
    FCVT_W_D, FCVT_WU_D, FCVT_S_D, FCVT_D_S,
    FMV_X_W,
    // zfa inst
    FCVTMOD_W_D,
  )
  /*
  The optype for FCVT_D_H and FCVT_H_D is the same,
  so the two instructions are distinguished by sew.
  FCVT_H_D:VSew.e64
  FCVT_D_H:VSew.e16
   */
  val isSew2Cvth = Seq(
    FCVT_S_H, FCVT_H_S, FCVT_D_H,
    FMV_X_H,
    FCVT_W_H, FCVT_L_H, FCVT_H_W,
    FCVT_H_L, FCVT_H_WU, FCVT_H_LU,
    FCVT_WU_H, FCVT_LU_H,
  )
  val isSew2Cvt32 = isSew2Cvts.map(io.instr === _).reduce(_ || _)
  val isSew2Cvt16 = isSew2Cvth.map(io.instr === _).reduce(_ || _)
  val isLmulMf4Cvts = Seq(
    FCVT_W_S, FCVT_WU_S,
    FMV_X_W,
  )
  val isLmulMf4Cvt = isLmulMf4Cvts.map(io.instr === _).reduce(_ || _)
  val needReverseInsts = Seq(
    FADD_S, FSUB_S, FADD_D, FSUB_D, FADD_H, FSUB_H,
    FEQ_S, FLT_S, FLE_S, FEQ_D, FLT_D, FLE_D, FEQ_H, FLT_H, FLE_H,
    FMIN_S, FMAX_S, FMIN_D, FMAX_D, FMIN_H, FMAX_H,
    FMUL_S, FMUL_D, FMUL_H,
    FDIV_S, FDIV_D, FSQRT_S, FSQRT_D, FDIV_H, FSQRT_H,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S, FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D,
    FMADD_H, FMSUB_H, FNMADD_H, FNMSUB_H,
    FCLASS_S, FCLASS_D, FSGNJ_S, FSGNJ_D, FSGNJX_S, FSGNJX_D, FSGNJN_S, FSGNJN_D,
    FCLASS_H, FSGNJ_H, FSGNJX_H, FSGNJN_H,
    // zfa inst
    FLEQ_H, FLEQ_S, FLEQ_D, FLTQ_H, FLTQ_S, FLTQ_D, FMINM_H, FMINM_S, FMINM_D, FMAXM_H, FMAXM_S, FMAXM_D,
  )
  val needReverseInst = needReverseInsts.map(_ === inst.ALL).reduce(_ || _)
  io.vpuCtrl := 0.U.asTypeOf(io.vpuCtrl)
  io.vpuCtrl.fpu.isFpToVecInst := isFpToVecInst
  io.vpuCtrl.fpu.isFP32Instr   := isFP32Instr
  io.vpuCtrl.fpu.isFP64Instr   := isFP64Instr
  io.vpuCtrl.vill  := false.B
  io.vpuCtrl.vma   := true.B
  io.vpuCtrl.vta   := true.B
  io.vpuCtrl.vsew  := Mux(isFP32Instr || isSew2Cvt32, VSew.e32, Mux(isFP16Instr || isSew2Cvt16, VSew.e16, VSew.e64))
  io.vpuCtrl.vlmul := Mux(isFP32Instr || isLmulMf4Cvt, VLmul.mf4, VLmul.mf2)
  io.vpuCtrl.vm    := inst.VM
  io.vpuCtrl.nf    := inst.NF
  io.vpuCtrl.veew := inst.WIDTH
  io.vpuCtrl.isReverse := needReverseInst
  io.vpuCtrl.isExt     := false.B
  io.vpuCtrl.isNarrow  := false.B
  io.vpuCtrl.isDstMask := false.B
  io.vpuCtrl.isOpMask  := false.B
  io.vpuCtrl.isDependOldVd := false.B
  io.vpuCtrl.isWritePartVd := false.B
}


class FPDecoder(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val instr = Input(UInt(32.W))
    val fpCtrl = Output(new FPUCtrlSignals)
  })

  private val inst: XSInstBitFields = io.instr.asTypeOf(new XSInstBitFields)

  def X = BitPat("b?")
  def T = BitPat("b??") //type
  def N = BitPat("b0")
  def Y = BitPat("b1")
  val s = BitPat(FPU.S(1,0))
  val d = BitPat(FPU.D(1,0))
  val i = BitPat(FPU.D(1,0))
  val h = BitPat(FPU.H(1,0))

  val default = List(X,T,T,N,N,N,X,X,X)

  // isAddSub tagIn tagOut fromInt wflags fpWen div sqrt fcvt
  val single: Array[(BitPat, List[BitPat])] = Array(
    // IntToFP
    FMV_W_X  -> List(N,i,s,Y,N,Y,N,N,N),
    FCVT_S_W -> List(N,i,s,Y,Y,Y,N,N,Y),
    FCVT_S_WU-> List(N,i,s,Y,Y,Y,N,N,Y),
    FCVT_S_L -> List(N,i,s,Y,Y,Y,N,N,Y),
    FCVT_S_LU-> List(N,i,s,Y,Y,Y,N,N,Y),
    // FPToInt
    FMV_X_W  -> List(N,d,i,N,N,N,N,N,N), // dont box result of fmv.fp.int
    FCLASS_S -> List(N,s,i,N,N,N,N,N,N),
    FCVT_W_S -> List(N,s,i,N,Y,N,N,N,Y),
    FCVT_WU_S-> List(N,s,i,N,Y,N,N,N,Y),
    FCVT_L_S -> List(N,s,i,N,Y,N,N,N,Y),
    FCVT_LU_S-> List(N,s,i,N,Y,N,N,N,Y),
    FEQ_S    -> List(N,s,i,N,Y,N,N,N,N),
    FLT_S    -> List(N,s,i,N,Y,N,N,N,N),
    FLE_S    -> List(N,s,i,N,Y,N,N,N,N),
    // FPToFP
    FSGNJ_S  -> List(N,s,s,N,N,Y,N,N,N),
    FSGNJN_S -> List(N,s,s,N,N,Y,N,N,N),
    FSGNJX_S -> List(N,s,s,N,N,Y,N,N,N),
    FMIN_S   -> List(N,s,s,N,Y,Y,N,N,N),
    FMAX_S   -> List(N,s,s,N,Y,Y,N,N,N),
    FADD_S   -> List(Y,s,s,N,Y,Y,N,N,N),
    FSUB_S   -> List(Y,s,s,N,Y,Y,N,N,N),
    FMUL_S   -> List(N,s,s,N,Y,Y,N,N,N),
    FMADD_S  -> List(N,s,s,N,Y,Y,N,N,N),
    FMSUB_S  -> List(N,s,s,N,Y,Y,N,N,N),
    FNMADD_S -> List(N,s,s,N,Y,Y,N,N,N),
    FNMSUB_S -> List(N,s,s,N,Y,Y,N,N,N),
    FDIV_S   -> List(N,s,s,N,Y,Y,Y,N,N),
    FSQRT_S  -> List(N,s,s,N,Y,Y,N,Y,N)
  )


  // isAddSub tagIn tagOut fromInt wflags fpWen div sqrt fcvt
  val double: Array[(BitPat, List[BitPat])] = Array(
    FMV_D_X  -> List(N,i,d,Y,N,Y,N,N,N),
    FCVT_D_W -> List(N,i,d,Y,Y,Y,N,N,Y),
    FCVT_D_WU-> List(N,i,d,Y,Y,Y,N,N,Y),
    FCVT_D_L -> List(N,i,d,Y,Y,Y,N,N,Y),
    FCVT_D_LU-> List(N,i,d,Y,Y,Y,N,N,Y),
    FMV_X_D  -> List(N,d,i,N,N,N,N,N,N),
    FCLASS_D -> List(N,d,i,N,N,N,N,N,N),
    FCVT_W_D -> List(N,d,i,N,Y,N,N,N,Y),
    FCVT_WU_D-> List(N,d,i,N,Y,N,N,N,Y),
    FCVT_L_D -> List(N,d,i,N,Y,N,N,N,Y),
    FCVT_LU_D-> List(N,d,i,N,Y,N,N,N,Y),
    FCVT_S_D -> List(N,d,s,N,Y,Y,N,N,Y),
    FCVT_D_S -> List(N,s,d,N,Y,Y,N,N,Y),
    FEQ_D    -> List(N,d,i,N,Y,N,N,N,N),
    FLT_D    -> List(N,d,i,N,Y,N,N,N,N),
    FLE_D    -> List(N,d,i,N,Y,N,N,N,N),
    FSGNJ_D  -> List(N,d,d,N,N,Y,N,N,N),
    FSGNJN_D -> List(N,d,d,N,N,Y,N,N,N),
    FSGNJX_D -> List(N,d,d,N,N,Y,N,N,N),
    FMIN_D   -> List(N,d,d,N,Y,Y,N,N,N),
    FMAX_D   -> List(N,d,d,N,Y,Y,N,N,N),
    FADD_D   -> List(Y,d,d,N,Y,Y,N,N,N),
    FSUB_D   -> List(Y,d,d,N,Y,Y,N,N,N),
    FMUL_D   -> List(N,d,d,N,Y,Y,N,N,N),
    FMADD_D  -> List(N,d,d,N,Y,Y,N,N,N),
    FMSUB_D  -> List(N,d,d,N,Y,Y,N,N,N),
    FNMADD_D -> List(N,d,d,N,Y,Y,N,N,N),
    FNMSUB_D -> List(N,d,d,N,Y,Y,N,N,N),
    FDIV_D   -> List(N,d,d,N,Y,Y,Y,N,N),
    FSQRT_D  -> List(N,d,d,N,Y,Y,N,Y,N)
  )

  val half : Array[(BitPat, List[BitPat])] = Array(
    // IntToFP
    FMV_H_X  -> List(N,i,h,Y,N,Y,N,N,N),
    FCVT_H_W -> List(N,i,h,Y,Y,Y,N,N,Y),
    FCVT_H_WU-> List(N,i,h,Y,Y,Y,N,N,Y),
    FCVT_H_L -> List(N,i,h,Y,Y,Y,N,N,Y),
    FCVT_H_LU-> List(N,i,h,Y,Y,Y,N,N,Y),
    // FPToInt
    FMV_X_H  -> List(N,h,i,N,N,N,N,N,N), // d or h ??
    FCLASS_H -> List(N,h,i,N,N,N,N,N,N),
    FCVT_W_H -> List(N,h,i,N,Y,N,N,N,Y),
    FCVT_WU_H-> List(N,h,i,N,Y,N,N,N,Y),
    FCVT_L_H -> List(N,h,i,N,Y,N,N,N,Y),
    FCVT_LU_H-> List(N,h,i,N,Y,N,N,N,Y),
    FEQ_H    -> List(N,h,i,N,Y,N,N,N,N),
    FLT_H    -> List(N,h,i,N,Y,N,N,N,N),
    FLE_H    -> List(N,h,i,N,Y,N,N,N,N),
    // FPToFP
    FSGNJ_H  -> List(N,h,h,N,N,Y,N,N,N),
    FSGNJN_H -> List(N,h,h,N,N,Y,N,N,N),
    FSGNJX_H -> List(N,h,h,N,N,Y,N,N,N),
    FMIN_H   -> List(N,h,h,N,Y,Y,N,N,N),
    FMAX_H   -> List(N,h,h,N,Y,Y,N,N,N),
    FADD_H   -> List(Y,h,h,N,Y,Y,N,N,N),
    FSUB_H   -> List(Y,h,h,N,Y,Y,N,N,N),
    FMUL_H   -> List(N,h,h,N,Y,Y,N,N,N),
    FMADD_H  -> List(N,h,h,N,Y,Y,N,N,N),
    FMSUB_H  -> List(N,h,h,N,Y,Y,N,N,N),
    FNMADD_H -> List(N,h,h,N,Y,Y,N,N,N),
    FNMSUB_H -> List(N,h,h,N,Y,Y,N,N,N),
    FDIV_H   -> List(N,h,h,N,Y,Y,Y,N,N),
    FSQRT_H  -> List(N,h,h,N,Y,Y,N,Y,N)
  )

  val table = single ++ double ++ half

  val decoder = DecodeLogic(io.instr, default, table)

  val ctrl = io.fpCtrl
  val sigs = Seq(
    ctrl.isAddSub, ctrl.typeTagIn, ctrl.typeTagOut,
    ctrl.fromInt, ctrl.wflags, ctrl.fpWen,
    ctrl.div, ctrl.sqrt, ctrl.fcvt
  )
  sigs.zip(decoder).foreach({case (s, d) => s := d})
  ctrl.typ := inst.TYP
  ctrl.fmt := inst.FMT
  ctrl.rm := inst.RM

  val fmaTable: Array[(BitPat, List[BitPat])] = Array(
    FADD_S  -> List(BitPat("b00"),N),
    FADD_D  -> List(BitPat("b00"),N),
    FADD_H  -> List(BitPat("b00"),N),
    FSUB_S  -> List(BitPat("b01"),N),
    FSUB_D  -> List(BitPat("b01"),N),
    FSUB_H  -> List(BitPat("b01"),N),
    FMUL_S  -> List(BitPat("b00"),N),
    FMUL_D  -> List(BitPat("b00"),N),
    FMUL_H  -> List(BitPat("b00"),N),
    FMADD_S -> List(BitPat("b00"),Y),
    FMADD_D -> List(BitPat("b00"),Y),
    FMADD_H -> List(BitPat("b00"),Y),
    FMSUB_S -> List(BitPat("b01"),Y),
    FMSUB_D -> List(BitPat("b01"),Y),
    FMSUB_H -> List(BitPat("b01"),Y),
    FNMADD_S-> List(BitPat("b11"),Y),
    FNMADD_D-> List(BitPat("b11"),Y),
    FNMADD_H-> List(BitPat("b11"),Y),
    FNMSUB_S-> List(BitPat("b10"),Y),
    FNMSUB_D-> List(BitPat("b10"),Y),
    FNMSUB_H-> List(BitPat("b10"),Y)
  )
  val fmaDefault = List(BitPat("b??"), N)
  Seq(ctrl.fmaCmd, ctrl.ren3).zip(
    DecodeLogic(io.instr, fmaDefault, fmaTable)
  ).foreach({
    case (s, d) => s := d
  })
}
