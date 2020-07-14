package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.MDUOpType
import xiangshan.backend.fu.FunctionUnit._

class MulDivExeUnit extends Exu(Exu.mulDivExeUnitCfg){
  val (src1, src2, uop, func) =
    (io.in.bits.src1, io.in.bits.src2, io.in.bits.uop, io.in.bits.uop.ctrl.fuOpType)


  val isDiv = MDUOpType.isDiv(func)

  val mul = Module(new MulExeUnit)
  val div = Module(new DivExeUnit)

  for(x <- Seq(mul.io, div.io)){
    x.scommit <> DontCare
    x.dmem <> DontCare
    x.in.bits := io.in.bits
    x.redirect := io.redirect
  }

  mul.io.in.valid := io.in.valid && !isDiv
  div.io.in.valid := io.in.valid && isDiv

  io.in.ready := Mux(isDiv, div.io.in.ready, mul.io.in.ready)

  val arb = Module(new Arbiter(new ExuOutput, 2))

  arb.io.in(0) <> mul.io.out
  arb.io.in(1) <> div.io.out

  io.out <> arb.io.out

  XSDebug(io.in.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d) brTag:%x\n",
    io.in.valid, io.in.ready,
    io.out.valid, io.out.ready,
    io.redirect.valid,
    io.redirect.bits.isException,
    io.redirect.bits.brTag.value
  )
  XSDebug(io.in.valid, "src1:%x src2:%x pc:%x\n", src1, src2, io.in.bits.uop.cf.pc)
  XSDebug(io.out.valid, "Out(%d %d) res:%x pc:%x\n",
    io.out.valid, io.out.ready, io.out.bits.data, io.out.bits.uop.cf.pc
  )
}
