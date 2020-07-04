package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._

// TODO implement it
class Mul extends Exu(FuType.mul.litValue()){
  override def toString: String = "Mul"
  val (iovalid, src1, src2, pc, uop) = (io.in.valid, io.in.bits.src1, io.in.bits.src2,
    SignExt(io.in.bits.uop.cf.pc, AddrBits), io.in.bits.uop)

  val redirectHit = uop.brTag.needFlush(io.redirect)
  val valid = iovalid && !redirectHit

  val mulResult = src1 * src2;

  val mulLatency = 4
  val resultReg = Reg(Vec(mulLatency - 1, UInt(XLEN.W)))
  val validReg = Reg(Vec(mulLatency - 1, Bool()))
  val uopReg = Reg(Vec(mulLatency - 1, new MicroOp))
  for (i <- 0 until (mulLatency - 1)) {
    if (i == 0) {
      uopReg(i) := uop
      resultReg(i) := mulResult
      validReg(i) := valid
    }
    else {
      uopReg(i) := uopReg(i - 1)
      resultReg(i) := resultReg(i - 1)
      val cancel = uopReg(i - 1).brTag.needFlush(io.redirect)
      validReg(i) := validReg(i - 1) && !cancel
    }
  }

  io.in.ready := io.out.ready || !validReg(mulLatency - 2)
  io.out.bits.redirectValid := false.B
  io.out.bits.redirect <> DontCare

  val cancelResult = uopReg(mulLatency - 2).brTag.needFlush(io.redirect)
  io.out.valid := validReg(mulLatency - 2) && !cancelResult
  io.out.bits.uop := uopReg(mulLatency - 2)
  io.out.bits.data := resultReg(mulLatency - 2)

  XSDebug(io.in.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d %d) brTag:%x\n",
    io.in.valid, io.in.ready, io.out.valid, io.out.ready, io.redirect.valid, io.redirect.bits.isException, redirectHit, io.redirect.bits.brTag.value)
  XSDebug(io.in.valid, "src1:%x src2:%xpc:%x\n", src1, src2, pc)
  XSDebug(io.out.valid, "Out(%d %d) res:%x\n", io.out.valid, io.out.ready, io.out.bits.data)
}

// TODO implement it
class Mdu extends Exu(FuType.mdu.litValue()) {
  override def toString: String = "MulDiv"
  val (iovalid, src1, src2, pc, uop) = (io.in.valid, io.in.bits.src1, io.in.bits.src2,
    SignExt(io.in.bits.uop.cf.pc, AddrBits), io.in.bits.uop)

  val redirectHit = uop.brTag.needFlush(io.redirect)
  val valid = iovalid && !redirectHit

  val divResult = src1 / src2;

  val divLatency = 4
  val resultReg = Reg(Vec(divLatency - 1, UInt(XLEN.W)))
  val validReg = Reg(Vec(divLatency - 1, Bool()))
  val uopReg = Reg(Vec(divLatency - 1, new MicroOp))
  for (i <- 0 until (divLatency - 1)) {
    if (i == 0) {
      uopReg(i) := uop
      resultReg(i) := divResult
      validReg(i) := valid
    }
    else {
      uopReg(i) := uopReg(i - 1)
      resultReg(i) := resultReg(i - 1)
      val cancel = uopReg(i - 1).brTag.needFlush(io.redirect)
      validReg(i) := validReg(i - 1) && !cancel
    }
  }

  io.in.ready := io.out.ready || !validReg(divLatency - 2)
  io.out.bits.redirectValid := false.B
  io.out.bits.redirect <> DontCare

  val cancelResult = uopReg(divLatency - 2).brTag.needFlush(io.redirect)
  io.out.valid := validReg(divLatency - 2) && !cancelResult
  io.out.bits.uop := uopReg(divLatency - 2)
  io.out.bits.data := resultReg(divLatency - 2)

  XSDebug(io.in.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d %d) brTag:%x\n",
    io.in.valid, io.in.ready, io.out.valid, io.out.ready, io.redirect.valid, io.redirect.bits.isException, redirectHit, io.redirect.bits.brTag.value)
  XSDebug(io.in.valid, "src1:%x src2:%xpc:%x\n", src1, src2, pc)
  XSDebug(io.out.valid, "Out(%d %d) res:%x\n", io.out.valid, io.out.ready, io.out.bits.data)
}