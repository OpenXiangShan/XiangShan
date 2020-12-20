package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import hardfloat.CompareRecFN
import xiangshan.backend.fu.FunctionUnit

class FPToFP extends FPUPipelineModule{

  override def latency: Int = FunctionUnit.f2iCfg.latency.latencyVal.get

  val ctrl = io.in.bits.uop.ctrl.fpu
  val inTag = ctrl.typeTagIn
  val outTag = ctrl.typeTagOut
  val src1 = unbox(io.in.bits.src(0), inTag, None)
  val src2 = unbox(io.in.bits.src(1), inTag, None)
  val wflags = ctrl.wflags

  val signNum = Mux(rm(1), src1 ^ src2, Mux(rm(0), ~src2, src2))
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
  dcmp.io.signaling := !rm(1)

  val lt = dcmp.io.lt || (dcmp.io.a.asSInt() < 0.S && dcmp.io.b.asSInt() >= 0.S)

  when(wflags){
    val isnan1 = maxType.isNaN(src1)
    val isnan2 = maxType.isNaN(src2)
    val isInvalid = maxType.isSNaN(src1) || maxType.isSNaN(src2)
    val isNaNOut = isnan1 && isnan2
    val isLHS = isnan2 || rm(0) =/= lt && !isnan1
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
          narrower.io.roundingMode := rm
          narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
          val narrowed = sanitizeNaN(narrower.io.out, outType)
          mux.data := Cat(fsgnjMux.data >> narrowed.getWidth, narrowed)
          mux.exc := narrower.io.exceptionFlags
        }
      }
    }
  }

  var resVec = Seq(mux)
  for(i <- 1 to latency){
    resVec = resVec :+ PipelineReg(i)(resVec(i-1))
  }

  io.out.bits.data := resVec.last.data
  fflags := resVec.last.exc
}
