package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.MDUOpType
import xiangshan.backend.exu.Exu.mulDivExeUnitCfg
import xiangshan.backend.fu.{AbstractDivider, ArrayMultiplier, FunctionUnit, Radix2Divider}

class MulDivExeUnit extends Exu(mulDivExeUnitCfg) {

  val func = io.fromInt.bits.uop.ctrl.fuOpType
  val (src1, src2) = (
    io.fromInt.bits.src1(XLEN - 1, 0),
    io.fromInt.bits.src2(XLEN - 1, 0)
  )

  val mul = supportedFunctionUnits.collectFirst {
    case m: ArrayMultiplier => m
  }.get

  val div = supportedFunctionUnits.collectFirst {
    case d: AbstractDivider => d
  }.orNull

  // override inputs
  val op = MDUOpType.getMulOp(func)
  val signext = SignExt(_: UInt, XLEN + 1)
  val zeroext = ZeroExt(_: UInt, XLEN + 1)
  val mulInputFuncTable = List(
    MDUOpType.mul -> (zeroext, zeroext),
    MDUOpType.mulh -> (signext, signext),
    MDUOpType.mulhsu -> (signext, zeroext),
    MDUOpType.mulhu -> (zeroext, zeroext)
  )

  mul.io.in.bits.src(0) := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1, 0), p._2._1(src1)))
  )
  mul.io.in.bits.src(1) := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1, 0), p._2._2(src2)))
  )

  val isW = MDUOpType.isW(func)
  val isH = MDUOpType.isH(func)
  mul.ctrl.isW := isW
  mul.ctrl.isHi := isH
  mul.ctrl.sign := DontCare

  val isDivSign = MDUOpType.isDivSign(func)
  val divInputFunc = (x: UInt) => Mux(
    isW,
    Mux(isDivSign,
      SignExt(x(31, 0), XLEN),
      ZeroExt(x(31, 0), XLEN)
    ),
    x
  )
  div.io.in.bits.src(0) := divInputFunc(src1)
  div.io.in.bits.src(1) := divInputFunc(src2)
  div.ctrl.isHi := isH
  div.ctrl.isW := isW
  div.ctrl.sign := isDivSign

  XSDebug(io.fromInt.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d)\n",
    io.fromInt.valid, io.fromInt.ready,
    io.out.valid, io.out.ready,
    io.redirect.valid,
    io.redirect.bits.level
  )
  XSDebug(io.fromInt.valid, "src1:%x src2:%x pc:%x\n", src1, src2, io.fromInt.bits.uop.cf.pc)
  XSDebug(io.out.valid, "Out(%d %d) res:%x pc:%x\n",
    io.out.valid, io.out.ready, io.out.bits.data, io.out.bits.uop.cf.pc
  )
}

