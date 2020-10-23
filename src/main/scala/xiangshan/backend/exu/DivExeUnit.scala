package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.fu.Divider
import xiangshan.backend.MDUOpType

class DivExeUnit extends Exu(Exu.divExeUnitCfg) {

  val (src1, src2, uop, func) =
    (io.in.bits.src1, io.in.bits.src2, io.in.bits.uop, io.in.bits.uop.ctrl.fuOpType)

  val divider = Module(new Divider(XLEN))

  val isDiv = MDUOpType.isDiv(func)
  val isDivSign = MDUOpType.isDivSign(func)
  val isW = MDUOpType.isW(func)
  val isH = MDUOpType.isH(func)

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
  divider.io.in.bits.ctrl.isHi := isH
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

  XSDebug(io.in.valid || io.redirect.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d %d) brTag:%x\n",
    io.in.valid, io.in.ready,
    io.out.valid, io.out.ready,
    io.redirect.valid,
    io.redirect.bits.isException,
    io.redirect.bits.isFlushPipe,
    io.redirect.bits.brTag.value
  )
  XSDebug(io.in.valid, p"src1: 0x${Hexadecimal(src1)} src2: 0x${Hexadecimal(src2)} func: ${Binary(func)} " +
    p"pc: ${io.in.bits.uop.cf.pc} roqIdx: ${io.in.bits.uop.roqIdx}\n")
  XSDebug(io.out.valid, p"Out(${io.out.valid} ${io.out.ready}) res: ${Hexadecimal(io.out.bits.data)} " +
    p"func: ${Binary(io.out.bits.uop.ctrl.fuOpType)} pc: ${Hexadecimal(io.out.bits.uop.cf.pc)} roqIdx: ${io.out.bits.uop.roqIdx}\n"
  )

}
