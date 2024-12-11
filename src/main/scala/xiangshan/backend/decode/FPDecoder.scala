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
import xiangshan.backend.fu.fpu.FPU
import xiangshan.{FPUCtrlSignals, XSModule}

class FPDecoder(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val instr = Input(UInt(32.W))
    val fpCtrl = Output(new FPUCtrlSignals)
  })

  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")
  val s = BitPat(FPU.S)
  val d = BitPat(FPU.D)
  val i = BitPat(FPU.D)

  val default = List(X,X,X,N,N,N,X,X,X)

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

  val table = single ++ double

  val decoder = DecodeLogic(io.instr, default, table)

  val ctrl = io.fpCtrl
  val sigs = Seq(
    ctrl.isAddSub, ctrl.typeTagIn, ctrl.typeTagOut,
    ctrl.fromInt, ctrl.wflags, ctrl.fpWen,
    ctrl.div, ctrl.sqrt, ctrl.fcvt
  )
  sigs.zip(decoder).foreach({case (s, d) => s := d})
  ctrl.typ := io.instr(21, 20)
  ctrl.fmt := io.instr(26, 25)
  ctrl.rm := io.instr(14, 12)

  val fmaTable: Array[(BitPat, List[BitPat])] = Array(
    FADD_S  -> List(BitPat("b00"),N),
    FADD_D  -> List(BitPat("b00"),N),
    FSUB_S  -> List(BitPat("b01"),N),
    FSUB_D  -> List(BitPat("b01"),N),
    FMUL_S  -> List(BitPat("b00"),N),
    FMUL_D  -> List(BitPat("b00"),N),
    FMADD_S -> List(BitPat("b00"),Y),
    FMADD_D -> List(BitPat("b00"),Y),
    FMSUB_S -> List(BitPat("b01"),Y),
    FMSUB_D -> List(BitPat("b01"),Y),
    FNMADD_S-> List(BitPat("b11"),Y),
    FNMADD_D-> List(BitPat("b11"),Y),
    FNMSUB_S-> List(BitPat("b10"),Y),
    FNMSUB_D-> List(BitPat("b10"),Y)
  )
  val fmaDefault = List(BitPat("b??"), N)
  Seq(ctrl.fmaCmd, ctrl.ren3).zip(
    DecodeLogic(io.instr, fmaDefault, fmaTable)
  ).foreach({
    case (s, d) => s := d
  })
}
