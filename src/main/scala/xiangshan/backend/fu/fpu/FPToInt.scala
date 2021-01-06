package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.FType
import hardfloat.RecFNToIN
import utils.SignExt
import xiangshan.backend.fu.FunctionUnit

class FPToInt extends FPUPipelineModule {

  override def latency = FunctionUnit.f2iCfg.latency.latencyVal.get

  val (src1, src2) = (io.in.bits.src(0), io.in.bits.src(1))

  val ctrl = io.in.bits.uop.ctrl.fpu

  val src1_s = unbox(src1, S, Some(FType.S))
  val src1_d = unbox(src1, ctrl.typeTagIn, None)
  val src2_d = unbox(src2, ctrl.typeTagIn, None)

  val src1_ieee = ieee(src1)
  val move_out = Mux(ctrl.typeTagIn === S, src1_ieee(31, 0), src1_ieee)

  val classify_out = Mux(ctrl.typeTagIn === S,
    FType.S.classify(src1_s),
    FType.D.classify(src1)
  )

  val dcmp = Module(new hardfloat.CompareRecFN(maxExpWidth, maxSigWidth))
  dcmp.io.a := src1_d
  dcmp.io.b := src2_d
  dcmp.io.signaling := !rm(1)

  val dcmp_out = ((~rm).asUInt() & Cat(dcmp.io.lt, dcmp.io.eq)).orR()
  val dcmp_exc = dcmp.io.exceptionFlags

  val conv = Module(new RecFNToIN(maxExpWidth, maxSigWidth, XLEN))
  conv.io.in := src1_d
  conv.io.roundingMode := rm
  conv.io.signedOut := ~ctrl.typ(0)

  val conv_out = WireInit(conv.io.out)
  val conv_exc = WireInit(Cat(
    conv.io.intExceptionFlags(2, 1).orR(),
    0.U(3.W),
    conv.io.intExceptionFlags(0)
  ))

  val narrow = Module(new RecFNToIN(maxExpWidth, maxSigWidth, 32))
  narrow.io.in := src1_d
  narrow.io.roundingMode := rm
  narrow.io.signedOut := ~ctrl.typ(0)

  when(!ctrl.typ(1)) { // fcvt.w/wu.fp
    val excSign = src1_d(maxExpWidth + maxSigWidth) && !maxType.isNaN(src1_d)
    val excOut = Cat(conv.io.signedOut === excSign, Fill(32 - 1, !excSign))
    val invalid = conv.io.intExceptionFlags(2) || narrow.io.intExceptionFlags(1)
    when(invalid) {
      conv_out := Cat(conv.io.out >> 32, excOut)
    }
    conv_exc := Cat(invalid, 0.U(3.W), !invalid && conv.io.intExceptionFlags(0))
  }


  val intData = Wire(UInt(XLEN.W))
  intData := Mux(ctrl.wflags,
    Mux(ctrl.fcvt, conv_out, dcmp_out),
    Mux(rm(0), classify_out, move_out)
  )
  val doubleOut = Mux(ctrl.fcvt, ctrl.typ(1), ctrl.fmt(0))
  val intValue = Mux(doubleOut,
    SignExt(intData, XLEN),
    SignExt(intData(31, 0), XLEN)
  )

  val exc = Mux(ctrl.fcvt, conv_exc, dcmp_exc)

  var dataVec = Seq(intValue)
  var excVec = Seq(exc)

  for (i <- 1 to latency) {
    dataVec = dataVec :+ PipelineReg(i)(dataVec(i - 1))
    excVec = excVec :+ PipelineReg(i)(excVec(i - 1))
  }

  io.out.bits.data := dataVec.last
  fflags := excVec.last
}
