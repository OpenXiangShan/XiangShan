// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import hardfloat.CompareRecFN
import xiangshan.backend.fu.FunctionUnit

class FPToFP extends FPUPipelineModule{

  override def latency: Int = FunctionUnit.f2iCfg.latency.latencyVal.get

  val ctrlIn = io.in.bits.uop.ctrl.fpu
  val ctrl = S1Reg(ctrlIn)
  val inTag = ctrl.typeTagIn
  val outTag = ctrl.typeTagOut
  val wflags = ctrl.wflags
  val src1 = S1Reg(unbox(io.in.bits.src(0), ctrlIn.typeTagIn, None))
  val src2 = S1Reg(unbox(io.in.bits.src(1), ctrlIn.typeTagIn, None))
  val rmReg = S1Reg(rm)

  val signNum = Mux(rmReg(1), src1 ^ src2, Mux(rmReg(0), ~src2, src2))
  val fsgnj = Cat(signNum(fLen), src1(fLen-1, 0))

  val fsgnjMux = Wire(new Bundle() {
    val data = UInt((XLEN+1).W)
    val exc = UInt(5.W)
  })
  fsgnjMux.data := fsgnj
  fsgnjMux.exc := 0.U

  val dcmp = Module(new CompareRecFN(maxExpWidth, maxSigWidth))
  dcmp.io.a := src1
  dcmp.io.b := src2
  dcmp.io.signaling := !rmReg(1)

  val lt = dcmp.io.lt || (dcmp.io.a.asSInt() < 0.S && dcmp.io.b.asSInt() >= 0.S)

  when(wflags){
    val isnan1 = maxType.isNaN(src1)
    val isnan2 = maxType.isNaN(src2)
    val isInvalid = maxType.isSNaN(src1) || maxType.isSNaN(src2)
    val isNaNOut = isnan1 && isnan2
    val isLHS = isnan2 || rmReg(0) =/= lt && !isnan1
    fsgnjMux.exc := isInvalid << 4
    fsgnjMux.data := Mux(isNaNOut, maxType.qNaN, Mux(isLHS, src1, src2))
  }

  val mux = WireInit(fsgnjMux)
  for(t <- floatTypes.init){
    when(outTag === typeTag(t).U){
      mux.data := Cat(fsgnjMux.data >> t.recodedWidth, maxType.unsafeConvert(fsgnjMux.data, t))
    }
  }

  when(ctrl.fcvt){
    if(floatTypes.size > 1){
      // widening conversions simply canonicalize NaN operands
      val widened = Mux(maxType.isNaN(src1), maxType.qNaN, src1)
      fsgnjMux.data := widened
      fsgnjMux.exc := maxType.isSNaN(src1) << 4

      // narrowing conversions require rounding (for RVQ, this could be
      // optimized to use a single variable-position rounding unit, rather
      // than two fixed-position ones)
      for(outType <- floatTypes.init){
        when(outTag === typeTag(outType).U && (typeTag(outType) == 0).B || (outTag < inTag)){
          val narrower = Module(new hardfloat.RecFNToRecFN(maxType.exp, maxType.sig, outType.exp, outType.sig))
          narrower.io.in := src1
          narrower.io.roundingMode := rmReg
          narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
          val narrowed = sanitizeNaN(narrower.io.out, outType)
          mux.data := Cat(fsgnjMux.data >> narrowed.getWidth, narrowed)
          mux.exc := narrower.io.exceptionFlags
        }
      }
    }
  }

  io.out.bits.data := S2Reg(mux.data)
  fflags := S2Reg(mux.exc)
}
