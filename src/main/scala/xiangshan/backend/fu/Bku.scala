/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.fu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{LookupTreeDefault, ParallelMux, ParallelXOR, SignExt, ZeroExt}
import utility.{XSDebug, XSError}
import xiangshan._
import xiangshan.HasXSParameter
import xiangshan.backend.fu.util._

class BkuSrcInput(implicit p: Parameters) extends XSBundle {
  val src: Vec[UInt] = Vec(2, UInt(XLEN.W))
  val func: UInt = FuOpType()
  val regEnable: Bool = Bool()
  val funcReg: UInt = FuOpType()

  def connect(src: Vec[UInt], func: UInt, regEnable: Bool, funcReg: UInt): Unit = {
    this.src := src
    this.func := func
    this.regEnable := regEnable
    this.funcReg := funcReg
  }
}

/**
 * 2-input, 1-output function unit IO bundle for BKU
 */
class BkuSrcIO(implicit p: Parameters) extends XSBundle {
  val in : BkuSrcInput = Input(new BkuSrcInput)
  val out: UInt        = Output(UInt(XLEN.W))
}

/**
 * BKU module with 2 input and 1 output
 */
abstract class BkuSrcModule(implicit p: Parameters) extends XSModule {
  val io: BkuSrcIO = IO(new BkuSrcIO)
  val src1      = io.in.src(0)
  val src2      = io.in.src(1)
  val func      = io.in.func
  val regEnable = io.in.regEnable
  val funcReg   = io.in.funcReg
}

class CountModule(implicit p: Parameters) extends BkuSrcModule {
  def encode(bits: UInt): UInt = {
    LookupTreeDefault(bits, 0.U, List(0.U -> 2.U(2.W), 1.U -> 1.U(2.W)))
  }
  def clzi(msb: Int, left: UInt, right: UInt): UInt = {
    Mux(left(msb),
      Cat(left(msb) && right(msb), !right(msb), if (msb == 1) right(0) else right(msb - 1, 0)),
      left)
  }

  // stage 0
  val c0 = Wire(Vec(32, UInt(2.W)))
  val c1 = Wire(Vec(16, UInt(3.W)))
  val countSrc = Mux(func(1), Reverse(src1), src1)

  for(i <- 0 until 32){ c0(i) := encode(countSrc(2 * i + 1, 2 * i)) }
  for(i <- 0 until 16){ c1(i) := clzi(1, c0(i * 2 + 1), c0(i * 2)) }

  // pipeline registers
  val c2 = Reg(Vec(8, UInt(4.W)))
  val cpopTmp = Reg(Vec(4, UInt(5.W)))
  when (regEnable) {
    for (i <- 0 until 8) {
      c2(i) := clzi(2, c1(i * 2 + 1), c1(i * 2))
    }
    for (i <- 0 until 4) {
      cpopTmp(i) := PopCount(src1(i * 16 + 15, i * 16))
    }
  }

  // stage 1
  val c3 = Wire(Vec(4, UInt(5.W)))
  val c4 = Wire(Vec(2, UInt(6.W)))

  for (i <- 0 until  4) { c3(i) := clzi(3, c2(i * 2 + 1), c2(i * 2)) }
  for (i <- 0 until  2) { c4(i) := clzi(4, c3(i * 2 + 1), c3(i * 2)) }
  val zeroRes = clzi(5, c4(1), c4(0))
  val zeroWRes = Mux(funcReg(1), c4(1), c4(0))

  val cpopLo32 = cpopTmp(0) +& cpopTmp(1)
  val cpopHi32 = cpopTmp(2) +& cpopTmp(3)

  val cpopRes = cpopLo32 +& cpopHi32
  val cpopWRes = cpopLo32

  io.out := Mux(
    funcReg(2),
    Mux(funcReg(0), cpopWRes, cpopRes),
    Mux(funcReg(0), zeroWRes, zeroRes)
  )
}

class ClmulModule(implicit p: Parameters) extends BkuSrcModule {
  // stage 0
  val mul0 = Wire(Vec(64, UInt(128.W)))
  val mul1 = Wire(Vec(32, UInt(128.W)))
  val mul2 = Wire(Vec(16, UInt(128.W)))

  (0 until XLEN) foreach { i =>
    mul0(i) := Mux(src1(i), if(i==0) src2 else Cat(src2, 0.U(i.W)), 0.U)
  }
  (0 until 32) foreach { i => mul1(i) := mul0(i * 2) ^ mul0(i * 2 + 1)}
  (0 until 16) foreach { i => mul2(i) := mul1(i * 2) ^ mul1(i * 2 + 1)}

  // pipeline registers
  val mul3 = Reg(Vec(8, UInt(128.W)))
  when (regEnable) {
    (0 until 8) map { i => mul3(i) := mul2(i * 2) ^ mul2(i * 2 + 1)}
  }

  // stage 1
  val res = ParallelXOR(mul3)

  val clmul  = res(63,0)
  val clmulh = res(127,64)
  val clmulr = res(126,63)

  io.out := LookupTreeDefault(funcReg, clmul, List(
    BKUOpType.clmul  -> clmul,
    BKUOpType.clmulh -> clmulh,
    BKUOpType.clmulr -> clmulr
  ))
}

class MiscModule(implicit p: Parameters) extends BkuSrcModule {
  def xpermLUT(table: UInt, idx: UInt, width: Int) : UInt = {
    // ParallelMux((0 until XLEN/width).map( i => i.U -> table(i)).map( x => (x._1 === idx, x._2)))
    LookupTreeDefault(idx, 0.U(width.W), (0 until XLEN / width).map { i =>
      i.U -> table(i * width + width - 1, i * width)
    })
  }

  val xpermnVec = Wire(Vec(16, UInt(4.W)))
  xpermnVec.zipWithIndex.foreach { case (sink, i) =>
    sink := xpermLUT(src1, src2(i * 4 + 3, i * 4), 4)
  }
  val xpermn = Cat(xpermnVec.reverse)

  val xpermbVec = Wire(Vec(8, UInt(8.W)))
  xpermbVec.zipWithIndex.foreach { case (sink, i) =>
    sink := Mux(src2(i * 8 + 7, i * 8 + 3).orR, 0.U, xpermLUT(src1, src2(i * 8 + 2, i * 8), 8))
  }
  val xpermb = Cat(xpermbVec.reverse)

  io.out := RegEnable(Mux(func(0), xpermb, xpermn), regEnable)
}

class HashModule(implicit p: Parameters) extends BkuSrcModule {
  val sha256sum0 = SHA2.sha256sum0(src1)
  val sha256sum1 = SHA2.sha256sum1(src1)
  val sha256sig0 = SHA2.sha256sig0(src1)
  val sha256sig1 = SHA2.sha256sig1(src1)
  val sha512sum0 = SHA2.sha512sum0(src1)
  val sha512sum1 = SHA2.sha512sum1(src1)
  val sha512sig0 = SHA2.sha512sig0(src1)
  val sha512sig1 = SHA2.sha512sig1(src1)
  val sm3p0      = ROR32(src1, 23) ^ ROR32(src1, 15) ^ src1
  val sm3p1      = ROR32(src1, 9)  ^ ROR32(src1, 17) ^ src1

  require(XLEN == 64)
  val shaSource = VecInit(
    SignExt(sha256sum0, XLEN),
    SignExt(sha256sum1, XLEN),
    SignExt(sha256sig0, XLEN),
    SignExt(sha256sig1, XLEN),
    sha512sum0,
    sha512sum1,
    sha512sig0,
    sha512sig1
  )
  val sha = shaSource(func(2, 0))
  val sm3 = Mux(
    func(0),
    SignExt(sm3p1(31, 0), XLEN),
    SignExt(sm3p0(31, 0), XLEN)
  )

  io.out := RegEnable(Mux(func(3), sm3, sha), regEnable)
}

class BlockCipherModule(implicit p: Parameters) extends BkuSrcModule {
  val src1Bytes = VecInit((0 until 8).map(i => src1(i * 8 + 7, i * 8)))
  val src2Bytes = VecInit((0 until 8).map(i => src2(i * 8 + 7, i * 8)))

  // AES
  val aesSboxIn  = ForwardShiftRows(src1Bytes, src2Bytes)
  val aesSboxMid  = Reg(Vec(8, Vec(18, Bool())))
  val aesSboxOut  = Wire(Vec(8, UInt(8.W)))

  val iaesSboxIn = InverseShiftRows(src1Bytes, src2Bytes)
  val iaesSboxMid  = Reg(Vec(8, Vec(18, Bool())))
  val iaesSboxOut = Wire(Vec(8, UInt(8.W)))

  aesSboxOut.zip(aesSboxMid).zip(aesSboxIn)foreach { case ((out, mid), in) =>
    when (regEnable) {
      mid := SboxInv(SboxAesTop(in))
    }
    out := SboxAesOut(mid)
  }

  iaesSboxOut.zip(iaesSboxMid).zip(iaesSboxIn)foreach { case ((out, mid), in) =>
    when (regEnable) {
      mid := SboxInv(SboxIaesTop(in))
    }
    out := SboxIaesOut(mid)
  }

  val aes64es = aesSboxOut.asUInt
  val aes64ds = iaesSboxOut.asUInt

  val imMinIn  = RegEnable(src1Bytes, regEnable)

  val aes64esm = Cat(MixFwd(Seq(aesSboxOut(4), aesSboxOut(5), aesSboxOut(6), aesSboxOut(7))),
                     MixFwd(Seq(aesSboxOut(0), aesSboxOut(1), aesSboxOut(2), aesSboxOut(3))))
  val aes64dsm = Cat(MixInv(Seq(iaesSboxOut(4), iaesSboxOut(5), iaesSboxOut(6), iaesSboxOut(7))),
                     MixInv(Seq(iaesSboxOut(0), iaesSboxOut(1), iaesSboxOut(2), iaesSboxOut(3))))
  val aes64im  = Cat(MixInv(Seq(imMinIn(4), imMinIn(5), imMinIn(6), imMinIn(7))),
                     MixInv(Seq(imMinIn(0), imMinIn(1), imMinIn(2), imMinIn(3))))


  val rcon = VecInit(0x01.U, 0x02.U, 0x04.U, 0x08.U,
                     0x10.U, 0x20.U, 0x40.U, 0x80.U,
                     0x1b.U, 0x36.U, 0x00.U)

  val ksSboxIn  = Wire(Vec(4, UInt(8.W)))
  val ksSboxTop = Reg(Vec(4, Vec(21, Bool())))
  val ksSboxOut = Wire(Vec(4, UInt(8.W)))
  ksSboxIn(0) := Mux(src2(3, 0) === 0xa.U, src1Bytes(4), src1Bytes(5))
  ksSboxIn(1) := Mux(src2(3, 0) === 0xa.U, src1Bytes(5), src1Bytes(6))
  ksSboxIn(2) := Mux(src2(3, 0) === 0xa.U, src1Bytes(6), src1Bytes(7))
  ksSboxIn(3) := Mux(src2(3, 0) === 0xa.U, src1Bytes(7), src1Bytes(4))
  ksSboxOut.zip(ksSboxTop).zip(ksSboxIn).foreach{ case ((out, top), in) =>
    when (regEnable) {
      top := SboxAesTop(in)
    }
    out := SboxAesOut(SboxInv(top))
    }

  val ks1Idx = RegEnable(src2(3, 0), regEnable)
  val aes64ks1i = Cat(ksSboxOut.asUInt ^ rcon(ks1Idx), ksSboxOut.asUInt ^ rcon(ks1Idx))

  val aes64ks2Temp = src1(63, 32) ^ src2(31, 0)
  val aes64ks2 = RegEnable(Cat(aes64ks2Temp ^ src2(63, 32), aes64ks2Temp), regEnable)

  val aesResult = LookupTreeDefault(funcReg, aes64es, List(
    BKUOpType.aes64es   -> aes64es,
    BKUOpType.aes64esm  -> aes64esm,
    BKUOpType.aes64ds   -> aes64ds,
    BKUOpType.aes64dsm  -> aes64dsm,
    BKUOpType.aes64im   -> aes64im,
    BKUOpType.aes64ks1i -> aes64ks1i,
    BKUOpType.aes64ks2  -> aes64ks2
  ))

  // SM4
  val sm4SboxIn  = src2Bytes(func(1, 0))
  val sm4SboxTop = Reg(Vec(21, Bool()))
  when (regEnable) {
    sm4SboxTop := SboxSm4Top(sm4SboxIn)
  }
  val sm4SboxOut = SboxSm4Out(SboxInv(sm4SboxTop))

  val sm4ed = sm4SboxOut ^ (sm4SboxOut << 8).asUInt ^ (sm4SboxOut << 2).asUInt ^ (sm4SboxOut << 18).asUInt ^
              ((sm4SboxOut & 0x3f.U) << 26).asUInt ^ ((sm4SboxOut & 0xc0.U) << 10).asUInt
  val sm4ks = sm4SboxOut ^ ((sm4SboxOut & 0x07.U) << 29).asUInt ^ ((sm4SboxOut & 0xfe.U) << 7).asUInt ^
              ((sm4SboxOut & 0x01.U) << 23).asUInt ^ ((sm4SboxOut & 0xf8.U) << 13).asUInt
  val sm4Source = VecInit(
    sm4ed(31, 0),
    Cat(sm4ed(23, 0), sm4ed(31, 24)),
    Cat(sm4ed(15, 0), sm4ed(31, 16)),
    Cat(sm4ed( 7, 0), sm4ed(31,  8)),
    sm4ks(31, 0),
    Cat(sm4ks(23, 0), sm4ks(31, 24)),
    Cat(sm4ks(15, 0), sm4ks(31, 16)),
    Cat(sm4ks( 7, 0), sm4ks(31,  8))
  )
  val sm4Result = SignExt((sm4Source(funcReg(2, 0)) ^ RegEnable(src1(31, 0), regEnable))(31, 0), XLEN)

  io.out := Mux(funcReg(3), sm4Result, aesResult)
}

class Bku(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) with HasPipelineReg {
  override def latency = 2

  val bkuSrcInput = Wire(new BkuSrcInput)
  val src1 = io.in.bits.data.src(0)
  val src2 = io.in.bits.data.src(1)
  val func = io.in.bits.ctrl.fuOpType
  val fire = io.in.fire
  val funcReg = RegEnable(func, fire)
  bkuSrcInput.connect(VecInit(src1, src2), func, fire, funcReg)

  val countModule = Module(new CountModule)
  val clmulModule = Module(new ClmulModule)
  val miscModule = Module(new MiscModule)
  val hashModule = Module(new HashModule)
  val blockCipherModule = Module(new BlockCipherModule)

  countModule.io.in := bkuSrcInput
  clmulModule.io.in := bkuSrcInput
  miscModule.io.in := bkuSrcInput
  hashModule.io.in := bkuSrcInput
  blockCipherModule.io.in := bkuSrcInput

  // CountModule, ClmulModule, MiscModule, and CryptoModule have a latency of 1 cycle
  val result = Mux(
    funcReg(5),
    Mux(funcReg(4), hashModule.io.out, blockCipherModule.io.out),
    Mux(
      funcReg(3),
      countModule.io.out,
      Mux(funcReg(2), miscModule.io.out, clmulModule.io.out)
    )
  )

  io.out.bits.res.data := RegEnable(result, regEnable(2))
  // connectNonPipedCtrlSingal
}
