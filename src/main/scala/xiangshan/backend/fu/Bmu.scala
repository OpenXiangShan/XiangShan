package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{LookupTreeDefault, ParallelMux, ParallelXOR, SignExt, XSDebug, ZeroExt}
import xiangshan._


class CountModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Input(UInt(XLEN.W))
    val func = Input(UInt())
    val out = Output(UInt(XLEN.W))
  })

  val funcReg = RegNext(io.func)

  def encode(bits: UInt): UInt = {
    LookupTreeDefault(bits, 0.U, List(0.U -> 2.U(2.W), 1.U -> 1.U(2.W)))
  }
  def clzi(msb: Int, left: UInt, right: UInt): UInt = {
    Mux(left(msb),
      Cat(left(msb) && right(msb), !right(msb), if(msb==1)right(0) else right(msb-1, 0)),
      left)
  }
  
  val c0 = Wire(Vec(32, UInt(2.W)))
  val c1 = Wire(Vec(16, UInt(3.W)))
  val c2 = Reg(Vec(8, UInt(4.W)))
  val c3 = Wire(Vec(4, UInt(5.W)))
  val c4 = Wire(Vec(2, UInt(6.W)))

  val countSrc = Mux(io.func(1), Reverse(io.src), io.src)

  for(i <- 0 until 32){ c0(i) := encode(countSrc(2*i+1, 2*i)) }
  for(i <- 0 until 16){ c1(i) := clzi(1, c0(i*2+1), c0(i*2)) }
  for(i <- 0 until 8){ c2(i) := clzi(2, c1(i*2+1), c1(i*2)) }
  for(i <- 0 until 4){ c3(i) := clzi(3, c2(i*2+1), c2(i*2)) }
  for(i <- 0 until 2){ c4(i) := clzi(4, c3(i*2+1), c3(i*2)) }
  val zeroRes = clzi(5, c4(1), c4(0))
  val zeroWRes = Mux(funcReg(1), c4(1), c4(0))

  val cpopTmp = Reg(Vec(4, UInt(5.W)))

  for(i <- 0 until 4){
    cpopTmp(i) := PopCount(io.src(i*16+15, i*16))
  }
  
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
    val out = Output(UInt(XLEN.W))
  })

  val funcReg = RegNext(io.func)

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
 
  io.out := LookupTreeDefault(funcReg, clmul, List(
    BMUOpType.clmul  -> clmul,
    BMUOpType.clmulh -> clmulh,
    BMUOpType.clmulr -> clmulr
  ))
}

class MiscModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val func = Input(UInt())
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

  io.out := RegNext(Mux(io.func(0), xpermb, xpermn))
}

class Bmu(implicit p: Parameters) extends FunctionUnit with HasPipelineReg {

  override def latency = 1

  val (src1, src2, func, funcReg) = (
    io.in.bits.src(0),
    io.in.bits.src(1),
    io.in.bits.uop.ctrl.fuOpType,
    uopVec(latency).ctrl.fuOpType
  )

  val countModule = Module(new CountModule)
  countModule.io.src := src1
  countModule.io.func := func

  val clmulModule = Module(new ClmulModule)
  clmulModule.io.src(0) := src1
  clmulModule.io.src(1) := src2
  clmulModule.io.func := func

  val miscModule = Module(new MiscModule)
  miscModule.io.src(0) := src1
  miscModule.io.src(1) := src2
  miscModule.io.func := func


  val result = Mux(funcReg(4), miscModule.io.out, Mux(funcReg(3), countModule.io.out, clmulModule.io.out))

  // io.in.ready := io.out.ready
  // io.out.valid := io.in.valid
  // io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := result
}
