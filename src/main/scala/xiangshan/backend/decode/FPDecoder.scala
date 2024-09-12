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
    val instr  : UInt           = Input(UInt(32.W))
    val vpuCtrl: VPUCtrlSignals = Output(new VPUCtrlSignals)
  })

  private val inst = io.instr.asTypeOf(new XSInstBitFields)
  private val fpToVecInsts = Seq(
    FADD_S, FSUB_S, FADD_D, FSUB_D,
    FEQ_S, FLT_S, FLE_S, FEQ_D, FLT_D, FLE_D,
    FMIN_S, FMAX_S, FMIN_D, FMAX_D,
    FMUL_S, FMUL_D,
    FDIV_S, FDIV_D, FSQRT_S, FSQRT_D,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S, FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D,
    FCLASS_S, FCLASS_D, FSGNJ_S, FSGNJ_D, FSGNJX_S, FSGNJX_D, FSGNJN_S, FSGNJN_D,

    // scalar cvt inst
    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D, FCVT_S_D, FCVT_D_S,
    FCVT_S_H, FCVT_H_S, FCVT_H_D, FCVT_D_H,
    FMV_X_W, FMV_X_D, FMV_X_H,
    // zfa inst
    FLEQ_H, FLEQ_S, FLEQ_D, FLTQ_H, FLTQ_S, FLTQ_D, FMINM_H, FMINM_S, FMINM_D, FMAXM_H, FMAXM_S, FMAXM_D,
    FROUND_H, FROUND_S, FROUND_D, FROUNDNX_H, FROUNDNX_S, FROUNDNX_D, FCVTMOD_W_D,
  )
  private val isFpToVecInst = fpToVecInsts.map(io.instr === _).reduce(_ || _)
  private val isFP16Instrs = Seq(
    // zfa inst
    FLEQ_H, FLTQ_H, FMINM_H, FMAXM_H,
    FROUND_H, FROUNDNX_H,
  )
  private val isFP16Instr = isFP16Instrs.map(io.instr === _).reduce(_ || _)
  private val isFP32Instrs = Seq(
    FADD_S, FSUB_S, FEQ_S, FLT_S, FLE_S, FMIN_S, FMAX_S,
    FMUL_S, FDIV_S, FSQRT_S,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S,
    FCLASS_S, FSGNJ_S, FSGNJX_S, FSGNJN_S,
    // zfa inst
    FLEQ_S, FLTQ_S, FMINM_S, FMAXM_S,
    FROUND_S, FROUNDNX_S,
  )
  private val isFP32Instr  = isFP32Instrs.map(io.instr === _).reduce(_ || _)
  private val isFP64Instrs = Seq(
    FADD_D, FSUB_D, FEQ_D, FLT_D, FLE_D, FMIN_D, FMAX_D,
    FMUL_D, FDIV_D, FSQRT_D,
    FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D,
    FCLASS_D, FSGNJ_D, FSGNJX_D, FSGNJN_D,
  )
  private val isFP64Instr  = isFP64Instrs.map(io.instr === _).reduce(_ || _)
  // scalar cvt inst
  private val isSew2Cvts = Seq(
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
  private val isSew2Cvth   = Seq(
    FCVT_S_H, FCVT_H_S, FCVT_D_H,
    FMV_X_H,
  )
  private val isSew2Cvt32 = isSew2Cvts.map(io.instr === _).reduce(_ || _)
  private val isSew2Cvt16  = isSew2Cvth.map(io.instr === _).reduce(_ || _)
  private val isLmulMf4Cvts = Seq(
    FCVT_W_S, FCVT_WU_S,
    FMV_X_W,
  )
  private val isLmulMf4Cvt = isLmulMf4Cvts.map(io.instr === _).reduce(_ || _)
  private val needReverseInsts = Seq(
    FADD_S, FSUB_S, FADD_D, FSUB_D,
    FEQ_S, FLT_S, FLE_S, FEQ_D, FLT_D, FLE_D,
    FMIN_S, FMAX_S, FMIN_D, FMAX_D,
    FMUL_S, FMUL_D,
    FDIV_S, FDIV_D, FSQRT_S, FSQRT_D,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S, FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D,
    FCLASS_S, FCLASS_D, FSGNJ_S, FSGNJ_D, FSGNJX_S, FSGNJX_D, FSGNJN_S, FSGNJN_D,
    // zfa inst
    FLEQ_H, FLEQ_S, FLEQ_D, FLTQ_H, FLTQ_S, FLTQ_D, FMINM_H, FMINM_S, FMINM_D, FMAXM_H, FMAXM_S, FMAXM_D,
  )
  private val needReverseInst = needReverseInsts.map(_ === inst.ALL).reduce(_ || _)

  /** Give vpuCtrl a default value 0 */
  io.vpuCtrl := 0.U.asTypeOf(io.vpuCtrl)

  /** Pass signals to vpuCtrl as output */
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
  io.vpuCtrl.veew  := inst.WIDTH
  io.vpuCtrl.isReverse     := needReverseInst
  io.vpuCtrl.isExt         := false.B
  io.vpuCtrl.isNarrow      := false.B
  io.vpuCtrl.isDstMask     := false.B
  io.vpuCtrl.isOpMask      := false.B
  io.vpuCtrl.isDependOldvd := false.B
  io.vpuCtrl.isWritePartVd := false.B
}

/**
 * Float-Point instruction decoder
 */
class FPDecoder(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val instr : UInt           = Input(UInt(32.W))
    val fpCtrl: FPUCtrlSignals = Output(new FPUCtrlSignals)
  })

  /** Input instruction to decode */
  private val inst: XSInstBitFields = io.instr.asTypeOf(new XSInstBitFields)

  /* Abbreviation of different bit pattern */
  private def X = BitPat("b?")
  private def N = BitPat("b0")
  private def Y = BitPat("b1")
  private val s = BitPat(FPU.S(0)) // single
  private val d = BitPat(FPU.D(0)) // double
  private val i = BitPat(FPU.D(0)) // immediate

  /**
   * Default decode pattern for part of float-point instruction control signals.
   * Pattern: [isAddSub, tagIn, tagOut, fromInt, wflags, fpWen, div, sqrt, fcvt]
   * i.e. [is add or subtract, tag of input, tag of output, from integer, ?, float-point write enable,
   * divide, square root, float-point convert]
   */
  //                       isAddSub tagIn tagOut fromInt wflags fpWen div sqrt fcvt
  //                           |      |     |      |      |      |     |   |    |
  private val fdDefault = List(X,     X,    X,     N,     N,     N,    X,  X,   X   )

  /** decode table for single float-point instruction */
  // [isAddSub, tagIn, tagOut, fromInt, wflags, fpWen, div, sqrt, fcvt]
  private val single = Array(
    // IntToFP
    FMV_W_X   -> List(N,i,s,Y,N,Y,N,N,N),
    FCVT_S_W  -> List(N,i,s,Y,Y,Y,N,N,Y),
    FCVT_S_WU -> List(N,i,s,Y,Y,Y,N,N,Y),
    FCVT_S_L  -> List(N,i,s,Y,Y,Y,N,N,Y),
    FCVT_S_LU -> List(N,i,s,Y,Y,Y,N,N,Y),
    // FPToInt
    FMV_X_W   -> List(N,d,i,N,N,N,N,N,N), // dont box result of fmv.fp.int
    FCLASS_S  -> List(N,s,i,N,N,N,N,N,N),
    FCVT_W_S  -> List(N,s,i,N,Y,N,N,N,Y),
    FCVT_WU_S -> List(N,s,i,N,Y,N,N,N,Y),
    FCVT_L_S  -> List(N,s,i,N,Y,N,N,N,Y),
    FCVT_LU_S -> List(N,s,i,N,Y,N,N,N,Y),
    FEQ_S     -> List(N,s,i,N,Y,N,N,N,N),
    FLT_S     -> List(N,s,i,N,Y,N,N,N,N),
    FLE_S     -> List(N,s,i,N,Y,N,N,N,N),
    // FPToFP
    FSGNJ_S   -> List(N,s,s,N,N,Y,N,N,N),
    FSGNJN_S  -> List(N,s,s,N,N,Y,N,N,N),
    FSGNJX_S  -> List(N,s,s,N,N,Y,N,N,N),
    FMIN_S    -> List(N,s,s,N,Y,Y,N,N,N),
    FMAX_S    -> List(N,s,s,N,Y,Y,N,N,N),
    FADD_S    -> List(Y,s,s,N,Y,Y,N,N,N),
    FSUB_S    -> List(Y,s,s,N,Y,Y,N,N,N),
    FMUL_S    -> List(N,s,s,N,Y,Y,N,N,N),
    FMADD_S   -> List(N,s,s,N,Y,Y,N,N,N),
    FMSUB_S   -> List(N,s,s,N,Y,Y,N,N,N),
    FNMADD_S  -> List(N,s,s,N,Y,Y,N,N,N),
    FNMSUB_S  -> List(N,s,s,N,Y,Y,N,N,N),
    FDIV_S    -> List(N,s,s,N,Y,Y,Y,N,N),
    FSQRT_S   -> List(N,s,s,N,Y,Y,N,Y,N)
  )

  /** decode table for double float-point instruction */
  // [isAddSub, tagIn, tagOut, fromInt, wflags, fpWen, div, sqrt, fcvt]
  private val double = Array(
    FMV_D_X   -> List(N,i,d,Y,N,Y,N,N,N),
    FCVT_D_W  -> List(N,i,d,Y,Y,Y,N,N,Y),
    FCVT_D_WU -> List(N,i,d,Y,Y,Y,N,N,Y),
    FCVT_D_L  -> List(N,i,d,Y,Y,Y,N,N,Y),
    FCVT_D_LU -> List(N,i,d,Y,Y,Y,N,N,Y),
    FMV_X_D   -> List(N,d,i,N,N,N,N,N,N),
    FCLASS_D  -> List(N,d,i,N,N,N,N,N,N),
    FCVT_W_D  -> List(N,d,i,N,Y,N,N,N,Y),
    FCVT_WU_D -> List(N,d,i,N,Y,N,N,N,Y),
    FCVT_L_D  -> List(N,d,i,N,Y,N,N,N,Y),
    FCVT_LU_D -> List(N,d,i,N,Y,N,N,N,Y),
    FCVT_S_D  -> List(N,d,s,N,Y,Y,N,N,Y),
    FCVT_D_S  -> List(N,s,d,N,Y,Y,N,N,Y),
    FEQ_D     -> List(N,d,i,N,Y,N,N,N,N),
    FLT_D     -> List(N,d,i,N,Y,N,N,N,N),
    FLE_D     -> List(N,d,i,N,Y,N,N,N,N),
    FSGNJ_D   -> List(N,d,d,N,N,Y,N,N,N),
    FSGNJN_D  -> List(N,d,d,N,N,Y,N,N,N),
    FSGNJX_D  -> List(N,d,d,N,N,Y,N,N,N),
    FMIN_D    -> List(N,d,d,N,Y,Y,N,N,N),
    FMAX_D    -> List(N,d,d,N,Y,Y,N,N,N),
    FADD_D    -> List(Y,d,d,N,Y,Y,N,N,N),
    FSUB_D    -> List(Y,d,d,N,Y,Y,N,N,N),
    FMUL_D    -> List(N,d,d,N,Y,Y,N,N,N),
    FMADD_D   -> List(N,d,d,N,Y,Y,N,N,N),
    FMSUB_D   -> List(N,d,d,N,Y,Y,N,N,N),
    FNMADD_D  -> List(N,d,d,N,Y,Y,N,N,N),
    FNMSUB_D  -> List(N,d,d,N,Y,Y,N,N,N),
    FDIV_D    -> List(N,d,d,N,Y,Y,Y,N,N),
    FSQRT_D   -> List(N,d,d,N,Y,Y,N,Y,N)
  )

  /** Float-point control signals in output */
  private val ctrl = io.fpCtrl

  /** Decode table: for single and double float-point instructions */
  private val fdTable = single ++ double

  /** Generate a decoder */
  private val fdDecoder = DecodeLogic(io.instr, fdDefault, fdTable)

  /** Arrange certain signals as a sequence, to be connected to decoder */
  private val fdSigs = Seq(
    ctrl.isAddSub, ctrl.typeTagIn, ctrl.typeTagOut,
    ctrl.fromInt, ctrl.wflags, ctrl.fpWen,
    ctrl.div, ctrl.sqrt, ctrl.fcvt
  )

  /** Pass decode results to output */
  (fdSigs zip fdDecoder).foreach { case (sig, res) => sig := res }

  /** Assign some control signals directly from instruction fields */
  ctrl.typ := inst.TYP
  ctrl.fmt := inst.FMT
  ctrl.rm := inst.RM

  /* Abbreviation of different bit pattern */
  private val mn = BitPat("b10")
  private val na = BitPat("b01")
  private val ma = BitPat("b11")
  private val nn = BitPat("b00")

  /**
   * Default decode pattern for float-point multiplication/addition instruction to generate some control signals.
   * Pattern: [fmaCmd, ren3]
   * i.e. [float-point multiply/add command, read enable 3 ports]
   */
  //                                 fmaCmd   ren3
  //                                    |      |
  private val fmaDefault = List(BitPat("b??"), N)

  /** decode table for float-point mul/add instruction */
  private val fmaTable: Array[(BitPat, List[BitPat])] = Array(
    FADD_S  -> List(nn,N),
    FADD_D  -> List(nn,N),
    FSUB_S  -> List(na,N),
    FSUB_D  -> List(na,N),
    FMUL_S  -> List(nn,N),
    FMUL_D  -> List(nn,N),
    FMADD_S -> List(nn,Y),
    FMADD_D -> List(nn,Y),
    FMSUB_S -> List(na,Y),
    FMSUB_D -> List(na,Y),
    FNMADD_S-> List(ma,Y),
    FNMADD_D-> List(ma,Y),
    FNMSUB_S-> List(mn,Y),
    FNMSUB_D-> List(mn,Y)
  )

  /** Generate a decoder */
  private val fmaDecoder: Seq[UInt] = DecodeLogic(io.instr, fmaDefault, fmaTable)

  /** Arrange certain signals as a sequence, to be connected to decoder */
  private val fmaSigs = Seq(ctrl.fmaCmd, ctrl.ren3)

  /** Pass decode results to output */
  (fmaSigs zip fmaDecoder).foreach { case (sig, res) => sig := res }
}
