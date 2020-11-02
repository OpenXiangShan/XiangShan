package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.fu.fpu._
import xiangshan.backend.fu.fpu.IntToFloatSingleCycle
import xiangshan.backend.fu.fpu.FPUOpType._

class I2fExeUnit extends Exu(Exu.i2fExeUnitCfg){

  val uopIn = io.in.bits.uop
  val isDouble = !uopIn.ctrl.isRVF
  val fuOp = uopIn.ctrl.fuOpType
  val fu = fuOp.head(4)
  val op = fuOp.tail(4)
  val frm = WireInit(0.U(3.W))
  BoringUtils.addSink(frm, "Frm")

  val valid = io.in.valid && !uopIn.roqIdx.needFlush(io.redirect)
  val intToFloat = Module(new IntToFloatSingleCycle)
  val extraInput = intToFloat.io.in.bits.ext.get
  val instr_rm = io.in.bits.uop.cf.instr(14, 12)
  extraInput.isDouble := isDouble
  extraInput.rm := Mux(instr_rm =/= 7.U, instr_rm, frm)
  extraInput.op := op
  intToFloat.io.out.ready := io.out.ready
  intToFloat.io.in.valid := valid && fu===("b"+FU_I2F).U
  intToFloat.io.in.bits.src(0) := io.in.bits.src1
  intToFloat.io.in.bits.uop := uopIn
  intToFloat.io.redirectIn := io.redirect
  io.out.valid := valid
  io.out.bits.data := Mux(intToFloat.io.out.valid,
    Mux(isDouble, intToFloat.io.out.bits.data, boxF32ToF64(intToFloat.io.out.bits.data)),
    Mux(isDouble, io.in.bits.src1, boxF32ToF64(io.in.bits.src1))
  )
  io.out.bits.fflags := Mux(intToFloat.io.out.valid,
    intToFloat.io.out.bits.ext.get,
    0.U.asTypeOf(new Fflags)
  )
  io.in.ready := true.B
  io.out.bits.uop := uopIn
  io.out.bits.redirect <> DontCare
  io.out.bits.redirectValid := false.B
  io.out.bits.debug <> DontCare
}
