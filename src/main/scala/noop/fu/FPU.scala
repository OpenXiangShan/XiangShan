package noop.fu

import chisel3.{util, _}
import chisel3.util._
import utils._
import noop._
import xiangshan.backend.fu.fpu._
import xiangshan.backend.fu.fpu.FPUIOFunc._
import xiangshan.backend.fu.fpu.divsqrt.DivSqrt
import xiangshan.backend.fu.fpu.fma.FMA

class FpInstr extends NOOPBundle {
  val func5 = UInt(5.W)
  val fmt = UInt(2.W)
  val rs2 = UInt(5.W)
  val rs1 = UInt(5.W)
  val rm = UInt(3.W)
  val rd = UInt(5.W)
  val op = UInt(7.W)
  assert(this.getWidth == 32)
}

class FpuCsrIO extends NOOPBundle {
  val fflags = Output(new Fflags)
  val isIllegal = Output(Bool())
  val dirty_fs = Output(Bool())
  val frm = Input(UInt(3.W))
}

class FPUIO extends FunctionUnitIO{
  // use XLEN because fpu share data path with cpu
  val src3 = Input(UInt(XLEN.W))
  val fpu_csr = new FpuCsrIO
  val fpWen = Input(Bool())
  val instr = Input(UInt(32.W))
  val inputFunc = Input(UInt(1.W))
  val outputFunc = Input(UInt(2.W))
}




class FPU extends NOOPModule{
//  require(XLEN >= FLEN)
  val io = IO(new FPUIO)
  val (valid, src1, src2, src3, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.src3, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, src3: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.src3 := src3
    this.func := func
    io.out.bits
  }

  val instr = io.instr.asTypeOf(new FpInstr)
  val isRVD = instr.fmt(0)
  val src = VecInit(Seq(src1, src2, src3)).map(x =>
    Mux(io.inputFunc === in_unbox, unboxF64ToF32(x), x)
  )

  val roudingMode = Mux(instr.rm===7.U, io.fpu_csr.frm, instr.rm)
  val op = func(2, 0)
  val fu = func(5, 3)

  val s_ready :: s_wait :: Nil = Enum(2)
  val state = RegInit(s_ready)
  switch(state){
    is(s_ready){
      when(io.in.valid){
        state := s_wait
      }
    }
    is(s_wait){
      when(io.out.fire()){
        state := s_ready
      }
    }
  }

  val subModuleInput = Wire(new FPUSubModuleInput)
  subModuleInput.a := src(0)
  subModuleInput.b := src(1)
  subModuleInput.c := src(2)
  subModuleInput.op := op
  subModuleInput.isDouble := isRVD
  subModuleInput.rm := roudingMode

  val subModules = Array[FPUSubModule](
    Module(new FMA),  // 0
    Module(new FCMP),           // 1
    Module(new FMV(XLEN)),     // 2
    Module(new FloatToInt),    // 3
    Module(new IntToFloat),    // 4
    Module(new F32toF64),  // 5
    Module(new F64toF32),  // 6
    Module(new DivSqrt)            //7
  )
  val outFuncReg = RegEnable(io.outputFunc, io.in.fire())
  val fuReg = RegEnable(fu, io.in.fire())
  for((module, idx) <- subModules.zipWithIndex){
    module.io.in.bits := subModuleInput
    module.io.in.valid := io.in.fire() && idx.U===fu
    module.io.out.ready := true.B
  }

  val subModuleOutput = Wire(Decoupled(new FPUSubModuleOutput))
  subModuleOutput := LookupTree(fuReg, subModules.zipWithIndex.map({
    case (module, idx) =>
      idx.U -> module.io.out
  }))
  val result = subModuleOutput.bits.result

  io.in.ready := state===s_ready
  io.out.valid := subModuleOutput.valid
  io.out.bits := MuxLookup(outFuncReg, result, Seq(
    out_sext -> SignExt(result(31, 0), XLEN),
    out_box -> boxF32ToF64(result)
  ))

  //TODO: check illegal rounding mode exception
  io.fpu_csr.isIllegal := false.B
  io.fpu_csr.dirty_fs := io.in.fire() && io.fpWen
  io.fpu_csr.fflags := Mux(io.out.valid, subModuleOutput.bits.fflags, 0.U.asTypeOf(new Fflags))
}

