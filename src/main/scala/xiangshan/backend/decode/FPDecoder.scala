package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.DecodeLogic
import xiangshan.backend.decode.Instructions._
import xiangshan.{FPUCtrlSignals, XSModule}

class FPDecoder extends XSModule{
  val io = IO(new Bundle() {
    val instr = Input(UInt(32.W))
    val fpCtrl = Output(new FPUCtrlSignals)
  })

  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")
  val s = BitPat(S)
  val d = BitPat(D)

  val default = List(X,X,X,X,X,X,X,X,X)

  // isAddSub tagIn tagOut fromInt wflags fpWen div sqrt fcvt
  val single: Array[(BitPat, List[BitPat])] = Array(
    FMV_W_X  -> List(N,s,d,Y,N,Y,N,N,N),
    FCVT_S_W -> List(N,s,s,Y,Y,Y,N,N,Y),
    FCVT_S_WU-> List(N,s,s,Y,Y,Y,N,N,Y),
    FCVT_S_L -> List(N,s,s,Y,Y,Y,N,N,Y),
    FCVT_S_LU-> List(N,s,s,Y,Y,Y,N,N,Y),
    FMV_X_W  -> List(N,s,X,N,N,N,N,N,N),
    FCLASS_S -> List(N,s,X,N,N,N,N,N,N),
    FCVT_W_S -> List(N,s,X,N,Y,N,N,N,Y),
    FCVT_WU_S-> List(N,s,X,N,Y,N,N,N,Y),
    FCVT_L_S -> List(N,s,X,N,Y,N,N,N,Y),
    FCVT_LU_S-> List(N,s,X,N,Y,N,N,N,Y),
    FEQ_S    -> List(N,s,X,N,Y,N,N,N,N),
    FLT_S    -> List(N,s,X,N,Y,N,N,N,N),
    FLE_S    -> List(N,s,X,N,Y,N,N,N,N),
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

  val table = single

  val decoder = DecodeLogic(io.instr, default, table)

  val ctrl = io.fpCtrl
  val sigs = Seq(
    ctrl.isAddSub, ctrl.typeTagIn, ctrl.typeTagOut,
    ctrl.fromInt, ctrl.wflags, ctrl.fpWen,
    ctrl.div, ctrl.sqrt, ctrl.fcvt
  )
  sigs.zip(decoder).foreach({case (s, d) => s := d})
  ctrl.typ := io.instr(21,20)
  ctrl.fmt := io.instr(26,25)

  val fmaTable: Array[(BitPat, List[BitPat])] = Array(
    FADD_S  -> List(BitPat("b00"),N,Y),
    FSUB_S  -> List(BitPat("b01"),N,Y),
    FMUL_S  -> List(BitPat("b00"),N,Y),
    FMADD_S -> List(BitPat("b00"),Y,Y),
    FMSUB_S -> List(BitPat("b01"),Y,Y),
    FNMADD_S-> List(BitPat("b11"),Y,Y),
    FNMSUB_S-> List(BitPat("b10"),Y,Y)
  )
  val fmaDefault = List(BitPat("b??"), N, N)
  Seq(ctrl.fmaCmd, ctrl.ren3, ctrl.fma).zip(
    DecodeLogic(io.instr, fmaDefault, fmaTable)
  ).foreach({
    case (s, d) => s := d
  })
}
