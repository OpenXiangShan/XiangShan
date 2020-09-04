package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.MDUOpType
import xiangshan.backend.fu.FunctionUnit._
import xiangshan.backend.fu.ArrayMultiplier


class MulExeUnit extends Exu(Exu.mulExeUnitCfg){
  val (src1, src2, uop, func) =
    (io.in.bits.src1, io.in.bits.src2, io.in.bits.uop, io.in.bits.uop.ctrl.fuOpType)

  val mul = Module(new ArrayMultiplier(XLEN+1))

  val signext = SignExt(_: UInt, XLEN+1)
  val zeroext = ZeroExt(_: UInt, XLEN+1)
  val mulInputFuncTable = List(
    MDUOpType.mul    -> (zeroext, zeroext),
    MDUOpType.mulh   -> (signext, signext),
    MDUOpType.mulhsu -> (signext, zeroext),
    MDUOpType.mulhu  -> (zeroext, zeroext)
  )

  val isW = MDUOpType.isW(func)
  val isH = MDUOpType.isH(func)
  val op  = MDUOpType.getMulOp(func)

  mul.io.redirect := io.redirect
  mul.io.in.bits.ctrl.uop := io.in.bits.uop
  mul.io.in.bits.ctrl.sign := DontCare //Mul don't use this
  mul.io.in.bits.ctrl.isW := isW
  mul.io.in.bits.ctrl.isHi := isH
  mul.io.in.bits.src1 := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1,0), p._2._1(src1)))
  )
  mul.io.in.bits.src2 := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1,0), p._2._2(src2)))
  )
  mul.io.in.valid := io.in.valid
  mul.io.out.ready := io.out.ready

  io.in.ready := mul.io.in.ready
  io.out.valid := mul.io.out.valid
  io.out.bits.uop := mul.io.out.bits.uop
  io.out.bits.data := mul.io.out.bits.data
  io.out.bits.redirectValid := false.B
  io.out.bits.redirect <> DontCare

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
  XSDebug(io.redirect.valid, p"redirect: ${io.redirect.bits.brTag}\n")
}
