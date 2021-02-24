// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.FType
import hardfloat.RecFNToIN
import utils.SignExt
import xiangshan.backend.fu.FunctionUnit


class FPToIntDataModule(latency: Int) extends FPUDataModule {
  val regEnables = IO(Input(Vec(latency, Bool())))
  val (src1, src2) = (io.in.src(0), io.in.src(1))

  val ctrl = io.in.fpCtrl

  // stage 1: unbox inputs
  val src1_d = RegEnable(unbox(src1, ctrl.typeTagIn, None), regEnables(0))
  val src2_d = RegEnable(unbox(src2, ctrl.typeTagIn, None), regEnables(0))
  val ctrl_reg = RegEnable(ctrl, regEnables(0))
  val rm_reg = RegEnable(rm, regEnables(0))

  // stage2

  val src1_ieee = ieee(src1_d)
  val move_out = Mux(ctrl_reg.typeTagIn === S,
    src1_ieee(FType.S.ieeeWidth - 1, 0),
    src1_ieee
  )

  val classify_out = Mux(ctrl_reg.typeTagIn === S,
    FType.S.classify(maxType.unsafeConvert(src1_d, FType.S)),
    FType.D.classify(src1_d)
  )

  val dcmp = Module(new hardfloat.CompareRecFN(maxExpWidth, maxSigWidth))
  dcmp.io.a := src1_d
  dcmp.io.b := src2_d
  dcmp.io.signaling := !rm_reg(1)

  val dcmp_out = ((~rm_reg).asUInt() & Cat(dcmp.io.lt, dcmp.io.eq)).orR()
  val dcmp_exc = dcmp.io.exceptionFlags

  val conv = Module(new RecFNToIN(maxExpWidth, maxSigWidth, XLEN))
  conv.io.in := src1_d
  conv.io.roundingMode := rm_reg
  conv.io.signedOut := ~ctrl_reg.typ(0)

  val conv_out = WireInit(conv.io.out)
  val conv_exc = WireInit(Cat(
    conv.io.intExceptionFlags(2, 1).orR(),
    0.U(3.W),
    conv.io.intExceptionFlags(0)
  ))

  val narrow = Module(new RecFNToIN(maxExpWidth, maxSigWidth, 32))
  narrow.io.in := src1_d
  narrow.io.roundingMode := rm_reg
  narrow.io.signedOut := ~ctrl_reg.typ(0)

  when(!ctrl_reg.typ(1)) { // fcvt.w/wu.fp
    val excSign = src1_d(maxExpWidth + maxSigWidth) && !maxType.isNaN(src1_d)
    val excOut = Cat(conv.io.signedOut === excSign, Fill(32 - 1, !excSign))
    val invalid = conv.io.intExceptionFlags(2) || narrow.io.intExceptionFlags(1)
    when(invalid) {
      conv_out := Cat(conv.io.out >> 32, excOut)
    }
    conv_exc := Cat(invalid, 0.U(3.W), !invalid && conv.io.intExceptionFlags(0))
  }


  val intData = Wire(UInt(XLEN.W))
  intData := Mux(ctrl_reg.wflags,
    Mux(ctrl_reg.fcvt, conv_out, dcmp_out),
    Mux(rm_reg(0), classify_out, move_out)
  )
  val doubleOut = Mux(ctrl_reg.fcvt, ctrl_reg.typ(1), ctrl_reg.fmt(0))
  val intValue = RegEnable(Mux(doubleOut,
    SignExt(intData, XLEN),
    SignExt(intData(31, 0), XLEN)
  ), regEnables(1))

  val exc = RegEnable(Mux(ctrl_reg.fcvt, conv_exc, dcmp_exc), regEnables(1))

  io.out.data := intValue
  fflags := exc
}

class FPToInt extends FPUPipelineModule {

  override def latency = FunctionUnit.f2iCfg.latency.latencyVal.get

  override val dataModule = Module(new FPToIntDataModule(latency))
  connectDataModule
  dataModule.regEnables <> VecInit((1 to latency) map (i => regEnable(i)))
}
