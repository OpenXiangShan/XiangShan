package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.fu.Divider
import xiangshan.backend.MDUOpType

class DivExeUnit extends XSModule {
  val io = IO(new ExuIO)

  val (src1, src2, uop, func) =
    (io.in.bits.src1, io.in.bits.src2, io.in.bits.uop, io.in.bits.uop.ctrl.fuOpType)

  val divider = Module(new Divider(XLEN))

  val isDiv = MDUOpType.isDiv(func)
  val isDivSign = MDUOpType.isDivSign(func)
  val isW = MDUOpType.isW(func)

  val divInputFunc = (x: UInt) => Mux(
    isW,
    Mux(isDivSign,
      SignExt(x(31,0), XLEN),
      ZeroExt(x(31,0), XLEN)
    ),
    x
  )

  divider.io.redirect := io.redirect
  divider.io.in.valid := io.in.valid
  divider.io.in.bits.ctrl.uop := io.in.bits.uop
  divider.io.in.bits.ctrl.sign := isDivSign
  divider.io.in.bits.ctrl.isW := isW
  divider.io.in.bits.ctrl.isHi := func(1)
  divider.io.in.bits.src1 := divInputFunc(src1)
  divider.io.in.bits.src2 := divInputFunc(src2)
  divider.io.out.ready := io.out.ready

  io.in.ready := divider.io.in.ready
  io.out.valid := divider.io.out.valid
  io.out.bits.uop := divider.io.out.bits.uop
  io.out.bits.data := divider.io.out.bits.data
  io.out.bits.redirectValid := false.B
  io.out.bits.redirect <> DontCare
  io.dmem <> DontCare
  io.out.bits.debug <> DontCare

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
