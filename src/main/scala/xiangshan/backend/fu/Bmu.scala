package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{LookupTreeDefault, ParallelMux, ParallelXOR, SignExt, XSDebug, ZeroExt}
import xiangshan._


class CountModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Input(UInt(XLEN.W))
    val isCpop = Input(Bool())
    val out = Output(UInt(XLEN.W))
  })

  def encode(bits: UInt): UInt = {
    LookupTreeDefault(bits, 0.U, List(0.U -> 2.U(2.W), 1.U -> 1.U(2.W)))
  }
  def clzi(msb: Int, left: UInt, right: UInt): UInt = {
    Mux(left(msb),
      Cat(left(msb) & right(msb), !right(msb), if(msb==1)right(0) else right(msb-1, 0)),
      left)
  }
  
  val c0 = Wire(Vec(32, UInt(2.W)))
  val c1 = Wire(Vec(16, UInt(3.W)))
  val c2 = Reg(Vec(8, UInt(4.W)))
  val c3 = Wire(Vec(4, UInt(5.W)))
  val c4 = Wire(Vec(2, UInt(6.W)))

  for(i <- 0 until 32){ c0(i) := encode(io.src(2*i+1, 2*i)) }
  for(i <- 0 until 16){ c1(i) := clzi(1, c0(i*2+1), c0(i*2)) }
  for(i <- 0 until 8){ c2(i) := clzi(2, c1(i*2+1), c1(i*2)) }
  for(i <- 0 until 4){ c3(i) := clzi(3, c2(i*2+1), c2(i*2)) }
  for(i <- 0 until 2){ c4(i) := clzi(4, c3(i*2+1), c3(i*2)) }
  val zeroRes = clzi(5, c4(1), c4(0))

  val cpopTmp = Reg(Vec(4, UInt(5.W)))

  for(i <- 0 until 4){
    cpopTmp(i) := PopCount(io.src(i*16+15, i*16))
  }
  
  val cpopLo32 = cpopTmp(0) +& cpopTmp(1)
  val cpopHi32 = cpopTmp(2) +& cpopTmp(3)

  val cpopRes = cpopLo32 +& cpopHi32

  io.out := Mux(io.isCpop, cpopRes, zeroRes)
}

class ClmulModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val func = Input(UInt())
    val out = Output(UInt(XLEN.W))
  })

  val (src1, src2) = (io.src(0), io.src(1))

  val mul0 = Wire(Vec(64, UInt(128.W)))
  val mul1 = Wire(Vec(32, UInt(128.W)))
  val mul2 = Wire(Vec(16, UInt(128.W)))
  val mul3 = Reg(Vec(8, UInt(128.W)))

  (0 until XLEN) map { i =>
    mul0(i) := Mux(src1(i), if(i==0) src2 else Cat(src2, 0.U(i.W)), 0.U)
  }

  (0 until 32) map { i => mul1(i) := mul0(i*2) ^ mul0(i*2+1)}
  (0 until 16) map { i => mul2(i) := mul1(i*2) ^ mul1(i*2+1)}
  (0 until 8) map { i => mul3(i) := mul2(i*2) ^ mul2(i*2+1)}
 
  val res = ParallelXOR(mul3)
 
  val clmul  = res(63,0)
  val clmulh = res(127,64)
  val clmulr = res(126,63)
 
  io.out := LookupTreeDefault(io.func, clmul, List(
    BMUOpType.clmul  -> clmul,
    BMUOpType.clmulh -> clmulh,
    BMUOpType.clmulr -> clmulr
  ))
}


class Bmu(implicit p: Parameters) extends FunctionUnit {

  val (src1, src2, func, uop) = (
    io.in.bits.src(0),
    io.in.bits.src(1),
    io.in.bits.uop.ctrl.fuOpType,
    io.in.bits.uop
  )

  val countModule = Module(new CountModule)
  countModule.io.src := src1
  countModule.io.isCpop := func(2)

  val clmulModule = Module(new ClmulModule)
  clmulModule.io.src(0) := src1
  clmulModule.io.src(1) := src2
  clmulModule.io.func := func

  val result = Mux(func(3), countModule.io.out, clmulModule.io.out)

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := clmulModule.io.out
}
