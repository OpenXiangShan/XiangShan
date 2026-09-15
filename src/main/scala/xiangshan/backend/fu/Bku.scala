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
import xiangshan.backend.fu.util._

class CountModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Input(UInt(XLEN.W))
    val func = Input(UInt())
    val regEnable = Input(Bool())
    val out = Output(UInt(XLEN.W))
  })

  def encode(bits: UInt): UInt = {
    LookupTreeDefault(bits, 0.U, List(0.U -> 2.U(2.W), 1.U -> 1.U(2.W)))
  }
  def clzi(msb: Int, left: UInt, right: UInt): UInt = {
    Mux(left(msb),
      Cat(left(msb) && right(msb), !right(msb), if(msb==1)right(0) else right(msb-1, 0)),
      left)
  }

  // stage 0
  val c0 = Wire(Vec(32, UInt(2.W)))
  val c1 = Wire(Vec(16, UInt(3.W)))
  val countSrc = Mux(io.func(1), Reverse(io.src), io.src)

  for(i <- 0 until 32){ c0(i) := encode(countSrc(2*i+1, 2*i)) }
  for(i <- 0 until 16){ c1(i) := clzi(1, c0(i*2+1), c0(i*2)) }

  // pipeline registers
  val funcReg = RegEnable(io.func, io.regEnable)
  val c2 = Reg(Vec(8, UInt(4.W)))
  val cpopTmp = Reg(Vec(4, UInt(5.W)))
  when (io.regEnable) {
    for (i <- 0 until 8) {
      c2(i) := clzi(2, c1(i*2+1), c1(i*2))
    }
    for (i <- 0 until 4) {
      cpopTmp(i) := PopCount(io.src(i*16+15, i*16))
    }
  }

  // stage 1
  val c3 = Wire(Vec(4, UInt(5.W)))
  val c4 = Wire(Vec(2, UInt(6.W)))

  for(i <- 0 until  4){ c3(i) := clzi(3, c2(i*2+1), c2(i*2)) }
  for(i <- 0 until  2){ c4(i) := clzi(4, c3(i*2+1), c3(i*2)) }
  val zeroRes = clzi(5, c4(1), c4(0))
  val zeroWRes = Mux(funcReg(1), c4(1), c4(0))

  val cpopLo32 = cpopTmp(0) +& cpopTmp(1)
  val cpopHi32 = cpopTmp(2) +& cpopTmp(3)

  val cpopRes = cpopLo32 +& cpopHi32
  val cpopWRes = cpopLo32

  io.out := Mux(funcReg(2), Mux(funcReg(0), cpopWRes, cpopRes), Mux(funcReg(0), zeroWRes, zeroRes))
}

class ClmulModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val func = Input(UInt())
    val regEnable = Input(Bool())
    val out = Output(UInt(XLEN.W))
  })

  // stage 0
  val (src1, src2) = (io.src(0), io.src(1))

  val mul0 = Wire(Vec(64, UInt(128.W)))
  val mul1 = Wire(Vec(32, UInt(128.W)))
  val mul2 = Wire(Vec(16, UInt(128.W)))

  (0 until XLEN) map { i =>
    mul0(i) := Mux(src1(i), if(i==0) src2 else Cat(src2, 0.U(i.W)), 0.U)
  }
  (0 until 32) map { i => mul1(i) := mul0(i*2) ^ mul0(i*2+1)}
  (0 until 16) map { i => mul2(i) := mul1(i*2) ^ mul1(i*2+1)}

  // pipeline registers
  val funcReg = RegEnable(io.func, io.regEnable)
  val mul3 = Reg(Vec(8, UInt(128.W)))
  when (io.regEnable) {
    (0 until 8) map { i => mul3(i) := mul2(i*2) ^ mul2(i*2+1)}
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

class MiscModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val func = Input(UInt())
    val regEnable = Input(Bool())
    val out = Output(UInt(XLEN.W))
  })

  val (src1, src2) = (io.src(0), io.src(1))

  def xpermLUT(table: UInt, idx: UInt, width: Int) : UInt = {
    // ParallelMux((0 until XLEN/width).map( i => i.U -> table(i)).map( x => (x._1 === idx, x._2)))
    LookupTreeDefault(idx, 0.U(width.W), (0 until XLEN/width).map( i => i.U -> table(i*width+width-1, i*width)))
  }

  val xpermnVec = Wire(Vec(16, UInt(4.W)))
  (0 until 16).map( i => xpermnVec(i) := xpermLUT(src1, src2(i*4+3, i*4), 4))
  val xpermn = Cat(xpermnVec.reverse)

  val xpermbVec = Wire(Vec(8, UInt(8.W)))
  (0 until 8).map( i => xpermbVec(i) := Mux(src2(i*8+7, i*8+3).orR, 0.U, xpermLUT(src1, src2(i*8+2, i*8), 8)))
  val xpermb = Cat(xpermbVec.reverse)

  io.out := RegEnable(Mux(io.func(0), xpermb, xpermn), io.regEnable)
}

class HashModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Input(UInt(XLEN.W))
    val func = Input(UInt())
    val regEnable = Input(Bool())
    val out = Output(UInt(XLEN.W))
  })

  val src1 = io.src

  val sha256sum0 = ROR32(src1, 2)  ^ ROR32(src1, 13) ^ ROR32(src1, 22)
  val sha256sum1 = ROR32(src1, 6)  ^ ROR32(src1, 11) ^ ROR32(src1, 25)
  val sha256sig0 = ROR32(src1, 7)  ^ ROR32(src1, 18) ^ SHR32(src1, 3)
  val sha256sig1 = ROR32(src1, 17) ^ ROR32(src1, 19) ^ SHR32(src1, 10)
  val sha512sum0 = ROR64(src1, 28) ^ ROR64(src1, 34) ^ ROR64(src1, 39)
  val sha512sum1 = ROR64(src1, 14) ^ ROR64(src1, 18) ^ ROR64(src1, 41)
  val sha512sig0 = ROR64(src1, 1)  ^ ROR64(src1, 8)  ^ SHR64(src1, 7)
  val sha512sig1 = ROR64(src1, 19) ^ ROR64(src1, 61) ^ SHR64(src1, 6)
  val sm3p0      = ROR32(src1, 23) ^ ROR32(src1, 15) ^ src1
  val sm3p1      = ROR32(src1, 9)  ^ ROR32(src1, 17) ^ src1

  val shaSource = VecInit(Seq(
    SignExt(sha256sum0(31,0), XLEN),
    SignExt(sha256sum1(31,0), XLEN),
    SignExt(sha256sig0(31,0), XLEN),
    SignExt(sha256sig1(31,0), XLEN),
    sha512sum0,
    sha512sum1,
    sha512sig0,
    sha512sig1
  ))
  val sha = shaSource(io.func(2,0))
  val sm3 = Mux(io.func(0), SignExt(sm3p1(31,0), XLEN), SignExt(sm3p0(31,0), XLEN))

  io.out := RegEnable(Mux(io.func(3), sm3, sha), io.regEnable)
}

class BlockCipherModule(implicit p: Parameters) extends XSModule {
  def asVecByWidth(x: UInt, width: Int): Vec[UInt] = {
    require(x.getWidth % width == 0)
    val numElem: Int = x.getWidth / width
    VecInit((0 until numElem).map(i => x(width * (i + 1) - 1, width * i)))
  }

  def asBytes(x: UInt): Vec[UInt] = asVecByWidth(x, 8)

  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val func = Input(UInt())
    val regEnable = Input(Bool())
    val out = Output(UInt(XLEN.W))
  })

  val (src1, src2, func, funcReg) = (io.src(0), io.src(1), io.func, RegEnable(io.func, io.regEnable))

  val src1Bytes = asBytes(src1)
  val src2Bytes = asBytes(src2)

  val src1_reg = RegEnable(src1, io.regEnable)
  val src2_reg = RegEnable(src2, io.regEnable)

  // AES
  val aesSboxIn   = Wire(Vec(8, UInt(8.W)))
  val aesSboxOut  = Wire(Vec(8, UInt(8.W)))
  val iaesSboxIn  = Wire(Vec(8, UInt(8.W)))
  val iaesSboxOut = Wire(Vec(8, UInt(8.W)))
  val ksSboxIn    = Wire(Vec(4, UInt(8.W)))
  val ksSboxOut   = Wire(Vec(4, UInt(8.W)))

  val isInvAes = func(1) // 0: aes64es/aes64esm/aes64ks1i  1: aes64ds/aes64dsm
  val isKsAes  = func(2) // 0: aes64es/aes64esm            1: aes64ks1i

  val aesEncSboxIn = Wire(Vec(8, UInt(8.W)))
  val aesDecSboxIn = Wire(Vec(8, UInt(8.W)))
  val aesEncSboxInLow  = Wire(Vec(4, UInt(8.W)))
  val aesEncSboxInHigh = Wire(Vec(4, UInt(8.W)))

  aesEncSboxIn := ForwardShiftRows(src1Bytes, src2Bytes)
  aesDecSboxIn := InverseShiftRows(src1Bytes, src2Bytes)
  aesEncSboxInLow  := aesEncSboxIn.take(4)
  aesEncSboxInHigh := aesEncSboxIn.drop(4)
  ksSboxIn := Mux(
    src2(3, 0) === 0xa.U(4.W),
    VecInit(src1Bytes(4), src1Bytes(5), src1Bytes(6), src1Bytes(7)),
    VecInit(src1Bytes(5), src1Bytes(6), src1Bytes(7), src1Bytes(4))
  )

  aesSboxIn  := Mux(isKsAes, ksSboxIn, aesEncSboxInLow) ++ aesEncSboxInHigh
  iaesSboxIn := aesDecSboxIn

  (aesSboxIn lazyZip iaesSboxIn lazyZip aesSboxOut lazyZip iaesSboxOut).foreach {
    case (fwdIn, invIn, fwdOut, invOut) =>
      val aesSBox1FwdOut = SboxAesTop(fwdIn)
      val aesSBox1InvOut = SboxIaesTop(invIn)

      val aesSBox2In  = Mux(isInvAes, aesSBox1InvOut, aesSBox1FwdOut)
      val aesSBox2Out = Reg(UInt(18.W)) // 1 cycle delay
      when (io.regEnable) {
        aesSBox2Out := SboxInv(aesSBox2In)
      }

      fwdOut := SboxAesOut(aesSBox2Out)
      invOut := SboxIaesOut(aesSBox2Out)
  }
  ksSboxOut := aesSboxOut.take(4)

  // AES encryption/decryption
  val aes64es = aesSboxOut.asUInt
  val aes64ds = iaesSboxOut.asUInt

  val imMinIn = asBytes(src1_reg)

  val aes64esm = Cat(MixFwd(Seq(aesSboxOut(4), aesSboxOut(5), aesSboxOut(6), aesSboxOut(7))),
                     MixFwd(Seq(aesSboxOut(0), aesSboxOut(1), aesSboxOut(2), aesSboxOut(3))))
  val aes64dsm = Cat(MixInv(Seq(iaesSboxOut(4), iaesSboxOut(5), iaesSboxOut(6), iaesSboxOut(7))),
                     MixInv(Seq(iaesSboxOut(0), iaesSboxOut(1), iaesSboxOut(2), iaesSboxOut(3))))
  val aes64im  = Cat(MixInv(Seq(imMinIn(4), imMinIn(5), imMinIn(6), imMinIn(7))),
                     MixInv(Seq(imMinIn(0), imMinIn(1), imMinIn(2), imMinIn(3))))

  // AES key schedule
  val rconSeq: Seq[Int] = Seq(
    0x01, 0x02, 0x04, 0x08,
    0x10, 0x20, 0x40, 0x80,
    0x1b, 0x36, 0x00
  )
  val rcon = VecInit(rconSeq.map(_.U(8.W)))

  val ks1Idx = src2_reg(3, 0)
  val aes64ks1i = Cat(ksSboxOut.asUInt ^ rcon(ks1Idx), ksSboxOut.asUInt ^ rcon(ks1Idx))

  val aes64ks2Temp = src1_reg(63, 32) ^ src2_reg(31, 0)
  val aes64ks2 = Cat(aes64ks2Temp ^ src2_reg(63, 32), aes64ks2Temp)

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
  val sm4SboxTop = Reg(UInt(21.W))
  when (io.regEnable) {
    sm4SboxTop := SboxSm4Top(sm4SboxIn)
  }
  val sm4SboxOut = SboxSm4Out(SboxInv(sm4SboxTop))
  // val sm4SboxTop = Reg(Vec(21, Bool()))
  // when (io.regEnable) {
  //   sm4SboxTop := SboxSm4Top(sm4SboxIn)
  // }
  // val sm4SboxOut = SboxSm4Out(SboxInv(sm4SboxTop))

  val sm4ed = sm4SboxOut ^ (sm4SboxOut << 8).asUInt ^ (sm4SboxOut << 2).asUInt ^ (sm4SboxOut << 18).asUInt ^
              ((sm4SboxOut & 0x3f.U) << 26).asUInt ^ ((sm4SboxOut & 0xc0.U) << 10).asUInt
  val sm4ks = sm4SboxOut ^ ((sm4SboxOut & 0x07.U) << 29).asUInt ^ ((sm4SboxOut & 0xfe.U) << 7).asUInt ^
              ((sm4SboxOut & 0x01.U) << 23).asUInt ^ ((sm4SboxOut & 0xf8.U) << 13).asUInt
  val sm4Source = VecInit(Seq(
    sm4ed(31, 0),
    Cat(sm4ed(23, 0), sm4ed(31, 24)),
    Cat(sm4ed(15, 0), sm4ed(31, 16)),
    Cat(sm4ed( 7, 0), sm4ed(31, 8)),
    sm4ks(31, 0),
    Cat(sm4ks(23, 0), sm4ks(31, 24)),
    Cat(sm4ks(15, 0), sm4ks(31, 16)),
    Cat(sm4ks( 7, 0), sm4ks(31, 8))
  ))
  val sm4ResultTemp = Wire(UInt(32.W))
  sm4ResultTemp := sm4Source(funcReg(2, 0)) ^ src1_reg(31, 0)
  val sm4Result = SignExt(sm4ResultTemp, XLEN)

  io.out := Mux(funcReg(3), sm4Result, aesResult)
}

class CryptoModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val func = Input(UInt())
    val regEnable = Input(Bool())
    val out = Output(UInt(XLEN.W))
  })

  val (src1, src2, func) = (io.src(0), io.src(1), io.func)
  val funcReg = RegEnable(func, io.regEnable)

  val hashModule = Module(new HashModule)
  hashModule.io.src := src1
  hashModule.io.func := func
  hashModule.io.regEnable := io.regEnable

  val blockCipherModule = Module(new BlockCipherModule)
  blockCipherModule.io.src(0) := src1
  blockCipherModule.io.src(1) := src2
  blockCipherModule.io.func := func
  blockCipherModule.io.regEnable := io.regEnable

  io.out := Mux(funcReg(4), hashModule.io.out, blockCipherModule.io.out)
}

class Bku(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) with HasPipelineReg {

  override def latency = 2

  val (src1, src2, func) = (
    io.in.bits.data.src(0),
    io.in.bits.data.src(1),
    io.in.bits.ctrl.fuOpType
  )

  val countModule = Module(new CountModule)
  countModule.io.src := src1
  countModule.io.func := func
  countModule.io.regEnable := regEnable(1)

  val clmulModule = Module(new ClmulModule)
  clmulModule.io.src(0) := src1
  clmulModule.io.src(1) := src2
  clmulModule.io.func := func
  clmulModule.io.regEnable := regEnable(1)

  val miscModule = Module(new MiscModule)
  miscModule.io.src(0) := src1
  miscModule.io.src(1) := src2
  miscModule.io.func := func
  miscModule.io.regEnable := regEnable(1)

  val cryptoModule = Module(new CryptoModule)
  cryptoModule.io.src(0) := src1
  cryptoModule.io.src(1) := src2
  cryptoModule.io.func := func
  cryptoModule.io.regEnable := regEnable(1)


  // CountModule, ClmulModule, MiscModule, and CryptoModule have a latency of 1 cycle
  val funcReg = RegEnable(func, io.in.fire)
  val result = Mux(funcReg(5), cryptoModule.io.out,
                  Mux(funcReg(3), countModule.io.out,
                      Mux(funcReg(2),miscModule.io.out, clmulModule.io.out)))

  io.out.bits.res.data := RegEnable(result, regEnable(2))
  // connectNonPipedCtrlSingal
}
