package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{LookupTreeDefault, ParallelMux, ParallelXOR, SignExt, XSDebug, ZeroExt}
import xiangshan._


abstract class BmuDataModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Input(Vec(2, UInt(XLEN.W)))
    val op = Input(UInt(6.W))
    val out = Output(UInt(XLEN.W))
  })
  val (src1, src2, op, out) = (io.src(0), io.src(1), io.op, io.out)
}

class ZbaModule(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val adduw, slliuw, sh1add, sh2add, sh3add, sh1adduw, sh2adduw, sh3adduw = Output(UInt(XLEN.W))
  })
  val src1 = io.src(0)
  val src2 = io.src(1)

  val shamt = src2(5,0)

  val adduw   = src1(31,0) + src2
  val slliuw  = src1(31,0) << shamt
  val sh1add  = Cat(src1(62,0), 0.U(1.W)) + src2
  val sh2add  = Cat(src1(61,0), 0.U(2.W)) + src2
  val sh3add  = Cat(src1(60,0), 0.U(3.W)) + src2
  val sh1adduw = Cat(src1(30,0), 0.U(1.W)) + src2
  val sh2adduw = Cat(src1(29,0), 0.U(2.W)) + src2
  val sh3adduw = Cat(src1(28,0), 0.U(3.W)) + src2
  io.adduw    := adduw
  io.slliuw   := slliuw
  io.sh1add   := sh1add
  io.sh2add   := sh2add
  io.sh3add   := sh3add
  io.sh1adduw := sh1adduw
  io.sh2adduw := sh2adduw
  io.sh3adduw := sh3adduw
}

class ZbaSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val adduw, slliuw, sh1add, sh2add, sh3add, sh1adduw, sh2adduw, sh3adduw = Input(UInt(XLEN.W))
    val op = Input(UInt(6.W))
    val out = Output(UInt(XLEN.W))
  })
  io.out := ParallelMux(List(
    BMUOpType.add_uw     -> io.adduw,
    BMUOpType.slli_uw    -> io.slliuw,
    BMUOpType.sh1add     -> io.sh1add,
    BMUOpType.sh2add     -> io.sh2add,
    BMUOpType.sh3add     -> io.sh3add,
    BMUOpType.sh1add_uw  -> io.sh1adduw,
    BMUOpType.sh2add_uw  -> io.sh2adduw,
    BMUOpType.sh3add_uw  -> io.sh3adduw
  ).map(x => (x._1 === io.op(4, 0), x._2)))
}



class SimpleModule(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val andn, orn, xnor, rev8, orcb  = Output(UInt(64.W))
  })

  val src1 = io.src(0)
  val src2 = io.src(1)
  val andn = src1 & ~src2
  val orn  = src1 | ~src2
  val xnor = src1 ^ ~src2
  val rev8 = Cat(src1(7,0), src1(15,8), src1(23,16), src1(31,24), src1(39,32), src1(47,40), src1(55,48), src1(63,56))
  val orcb = Cat(Reverse(src1(63,56)), Reverse(src1(55,48)), Reverse(src1(47,40)), Reverse(src1(39,32)),
                  Reverse(src1(31,24)), Reverse(src1(23,16)), Reverse(src1(15,8)), Reverse(src1(7,0)))

  io.andn := andn
  io.orn  := orn
  io.xnor := xnor
  io.rev8 := rev8
  io.orcb := orcb
}

class CmpModule(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val max, min, maxu, minu = Output(UInt(64.W))
  })
  val src1 = io.src(0)
  val src2 = io.src(1)

  val max = Mux(src1.asSInt() > src2.asSInt(), src1, src2)
  val min = Mux(src1.asSInt() < src2.asSInt(), src1, src2)
  val maxu = Mux(src1 > src2, src1, src2)
  val minu = Mux(src1 > src2, src2, src1)

  io.max  := max
  io.min  := min
  io.maxu := maxu
  io.minu := minu

}

class ExtModule(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val src = Input(UInt(XLEN.W))
    val sextb, sexth, zexth = Output(UInt(64.W))
  })
  val src1 = io.src

  val sextb = SignExt(src1(8, 0),XLEN)
  val sexth = SignExt(src1(15, 0),XLEN)
  val zexth = Cat(0.U(48.W), src1(15, 0))

  io.sextb := sextb
  io.sexth := sexth
  io.zexth := zexth

}

class ShiftModule(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val rol, rolw, ror, rorw = Output(UInt(64.W))
  })

  val src1 = io.src(0)
  val src1w = io.src(0)(31,0)
  val shamt = io.src(1)(5,0)
  val shamtw = io.src(1)(4,0)

  val rol  = src1<<shamt | src1>>((~shamt).asUInt()+&1.U)
  val rolw = SignExt((src1w<<shamtw | src1w>>(32.U-shamtw))(31,0), 64)
  val ror  = src1>>shamt | src1<<((~shamt).asUInt()+&1.U)
  val rorw = SignExt((src1w>>shamtw | src1w<<(32.U-shamtw))(31,0), 64)
  io.rol  := rol
  io.rolw := rolw
  io.ror  := ror
  io.rorw := rorw

}

class CntModule(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val src = Input(UInt(XLEN.W))
    val clz, clzw, ctz, ctzw, cpop, cpopw = Output(UInt(64.W))
  })
  val src1 = io.src
  val src1w = io.src(31,0)

  val clz   = ~Reverse(src1) & (Reverse(src1)-1.U)
  val clzw  = ~Reverse(src1w) & (Reverse(src1w)-1.U)
  val ctz   = ~src1 & (src1-1.U)
  val ctzw  = ~src1w & (src1w-1.U)
  val cpop  = PopCount(src1)
  val cpopw = PopCount(src1w)

  io.clz  := clz
  io.clzw := clzw
  io.ctz  := ctz
  io.ctzw := ctzw
  io.cpop := cpop
  io.cpopw:= cpopw

}

class ZbbModule(implicit p: Parameters) extends BmuDataModule{

  val simpleModule= Module(new SimpleModule)
  val cmpModule   = Module(new CmpModule)
  val extModule   = Module(new ExtModule)
  val shiftModule = Module(new ShiftModule)
  val cntModule   = Module(new CntModule)

  simpleModule.io.src(0) := src1
  cmpModule.io.src(0) := src1
  extModule.io.src := src1
  shiftModule.io.src(0) := src1
  cntModule.io.src := src1
  simpleModule.io.src(1) := src2
  cmpModule.io.src(1) := src2
  shiftModule.io.src(1) := src2


  val andn   = simpleModule.io.andn
  val orn    = simpleModule.io.orn
  val xnor   = simpleModule.io.xnor
  val max    = cmpModule.io.max
  val min    = cmpModule.io.min
  val maxu   = cmpModule.io.maxu
  val minu   = cmpModule.io.minu
  val sextb  = extModule.io.sextb
  val sexth  = extModule.io.sexth
  val zexth  = extModule.io.zexth
  val rev8   = simpleModule.io.rev8
  val orcb   = simpleModule.io.orcb
  val rol    = shiftModule.io.rol
  val rolw   = shiftModule.io.rolw
  val ror    = shiftModule.io.ror
  val rorw   = shiftModule.io.rorw
  val clz    = cntModule.io.clz
  val clzw   = cntModule.io.clzw
  val ctz    = cntModule.io.ctz
  val ctzw   = cntModule.io.ctzw
  val cpop   = cntModule.io.cpop
  val cpopw  = cntModule.io.cpopw
  
  out := ParallelMux(List(
    BMUOpType.andn      -> andn,
    BMUOpType.orn       -> orn,
    BMUOpType.xnor      -> xnor,
    BMUOpType.max       -> max,
    BMUOpType.min       -> min,
    BMUOpType.maxu      -> maxu,
    BMUOpType.minu      -> minu,
    BMUOpType.sext_b    -> sextb,
    BMUOpType.sext_h    -> sexth,
    BMUOpType.zext_h    -> zexth,
    BMUOpType.rev8      -> rev8,
    BMUOpType.orc_b     -> orcb,
    BMUOpType.rol       -> rol,
    BMUOpType.rolw      -> rolw,
    BMUOpType.ror       -> ror,
    BMUOpType.rorw      -> rorw,
    BMUOpType.clz       -> clz,
    BMUOpType.clzw      -> clzw,
    BMUOpType.ctz       -> ctz,
    BMUOpType.ctzw      -> ctzw,
    BMUOpType.cpop      -> cpop,
    BMUOpType.cpopw     -> cpopw
  ).map(x => (x._1 === op, x._2)))

}



// class ZbbModule(implicit p: Parameters) extends BmuDataModule{

//   val src1w = src1(31,0)
//   val shamt = Cat(op(0) && src2(5), src2(4, 0))

//   val andn = src1 & ~src2
//   val orn  = src1 | ~src2
//   val xnor = src1 ^ ~src2

//   val max = Mux(src1.asSInt() > src2.asSInt(), src1, src2)
//   val min = Mux(src1.asSInt() < src2.asSInt(), src1, src2)
//   val maxu = Mux(src1 > src2, src1, src2)
//   val minu = Mux(src1 > src2, src2, src1)

//   val sextb = SignExt(src1(8, 0),XLEN)
//   val sexth = SignExt(src1(15, 0),XLEN)
//   val zexth = Cat(0.U(48.W), src1(15, 0))

//   val rev8 = Cat(src1(7,0), src1(15,8), src1(23,16), src1(31,24), src1(39,32), src1(47,40), src1(55,48), src1(63,56))
//   val orcb = Cat(Reverse(src1(63,56)), Reverse(src1(55,48)), Reverse(src1(47,40)), Reverse(src1(39,32)), 
//     Reverse(src1(31,24)), Reverse(src1(23,16)), Reverse(src1(15,8)), Reverse(src1(7,0)))

//   val rol  = src1<<shamt | src1>>((~shamt).asUInt()+&1.U)
//   val rolw = SignExt((src1w<<shamt | src1w>>(32.U-shamt))(31,0), 64)
//   val ror  = src1>>shamt | src1<<((~shamt).asUInt()+&1.U)
//   val rorw = SignExt((src1w>>shamt | src1w<<(32.U-shamt))(31,0), 64)

//   val clz   = ~Reverse(src1) & (Reverse(src1)-1.U)
//   val clzw  = ~Reverse(src1w) & (Reverse(src1w)-1.U)
//   val ctz   = ~src1 & (src1-1.U)
//   val ctzw  = ~src1w & (src1w-1.U)
//   val cpop  = PopCount(src1)
//   val cpopw = PopCount(src1w)

//   out := ParallelMux(List(
//     BMUOpType.andn      -> andn,
//     BMUOpType.orn       -> orn,
//     BMUOpType.xnor      -> xnor,
//     BMUOpType.max       -> max,
//     BMUOpType.min       -> min,
//     BMUOpType.maxu      -> maxu,
//     BMUOpType.minu      -> minu,
//     BMUOpType.sext_b    -> sextb,
//     BMUOpType.sext_h    -> sexth,
//     BMUOpType.zext_h    -> zexth,
//     BMUOpType.rev8      -> rev8,
//     BMUOpType.orc_b     -> orcb,
//     BMUOpType.rol       -> rol,
//     BMUOpType.rolw      -> rolw,
//     BMUOpType.ror       -> ror,
//     BMUOpType.rorw      -> rorw,
//     BMUOpType.clz       -> clz,
//     BMUOpType.clzw      -> clzw,
//     BMUOpType.ctz       -> ctz,
//     BMUOpType.ctzw      -> ctzw,
//     BMUOpType.cpop      -> cpop,
//     BMUOpType.cpopw     -> cpopw
//   ).map(x => (x._1 === op, x._2)))


// //  out := LookupTreeDefault(op, andn, List(
// //    BMUOpType.andn      -> andn,
// //    BMUOpType.orn       -> orn,
// //    BMUOpType.xnor      -> xnor,
// //    BMUOpType.max       -> max,
// //    BMUOpType.min       -> min,
// //    BMUOpType.maxu      -> maxu,
// //    BMUOpType.minu      -> minu,
// //    BMUOpType.sext_b    -> sextb,
// //    BMUOpType.sext_h    -> sexth,
// //    BMUOpType.zext_h    -> zexth,
// //    BMUOpType.rev8      -> rev8,
// //    BMUOpType.orc_b     -> orcb,
// //    BMUOpType.rol       -> rol,
// //    BMUOpType.rolw      -> rolw,
// //    BMUOpType.ror       -> ror,
// //    BMUOpType.rorw      -> rorw,
// //    BMUOpType.clz       -> clz,
// //    BMUOpType.clzw      -> clzw,
// //    BMUOpType.ctz       -> ctz,
// //    BMUOpType.ctzw      -> ctzw,
// //    BMUOpType.cpop      -> cpop,
// //    BMUOpType.cpopw     -> cpopw
// //  ))
// }

//class ZbcModule(implicit p: Parameters) extends BmuDataModule{
//
//  val mul = Wire(Vec(64, UInt(128.W)))
//
//  for(i <- 0 until XLEN) {
//    if(i == 0) mul(i) := Mux(src2(i), src1, 0.U)
//    else mul(i) := mul(i-1) ^ Mux(src2(i), src1<<i, 0.U)
//  }
//
//  val clmul  = mul(63)(63,0)
//  val clmulh = mul(63)(127,64)
//  val clmulr = mul(63)(126,63)
//
//  out := LookupTreeDefault(op(3,0), clmul, List(
//    BMUOpType.clmul  -> clmul,
//    BMUOpType.clmulh -> clmulh,
//    BMUOpType.clmulr -> clmulr
//  ))
//}

class ZbcModule(implicit p: Parameters) extends BmuDataModule{

  val mul = Wire(Vec(64, UInt(128.W)))
  // val mul1 = Wire(Vec(32, UInt(128.W)))
  // val mul2 = Wire(Vec(16, UInt(128.W)))
  // val mul3 = Reg(Vec(8, UInt(128.W)))

  (0 until XLEN) map { i =>
    mul(i) := Mux(src1(i), if(i==0) src2 else Cat(src2, 0.U(i.W)), 0.U)
  }

  // (0 until 32) map { i => mul1(i) := mul(i*2) ^ mul(i*2+1)}
  // (0 until 16) map { i => mul2(i) := mul1(i*2) ^ mul1(i*2+1)}
  // (0 until 8) map { i => mul3(i) := mul2(i*2) ^ mul2(i*2+1)}

  val res = ParallelXOR(mul)


  val clmul  = res(63,0)
  val clmulh = res(127,64)
  val clmulr = res(126,63)

  out := LookupTreeDefault(op(3,0), clmul, List(
    BMUOpType.clmul  -> clmul,
    BMUOpType.clmulh -> clmulh,
    BMUOpType.clmulr -> clmulr
  ))
}

class ZbsModule(implicit p: Parameters) extends BmuDataModule{
  val shamt = src2(5, 0)
  val bset = src1 | 1.U << shamt
  val bclr = src1 & ~(1.U << shamt)
  val binv = src1 ^ 1.U << shamt
  val bext = (src1 >> shamt)(0)

  out := Mux(op(2), Mux(op(1), bset, binv), Mux(op(1), bext, bclr))
}


class Bmu(implicit p: Parameters) extends FunctionUnit {

  val (src1, src2, func, uop) = (
    io.in.bits.src(0),
    io.in.bits.src(1),
    io.in.bits.uop.ctrl.fuOpType,
    io.in.bits.uop
  )

  val zbaModule = Module(new ZbaModule)
  zbaModule.io.src(0) := src1
  zbaModule.io.src(1) := src2
  zbaModule.io.src(1) := src2
  // zbaModule.io.op := func

  val zbaSelect = Module(new ZbaSelect)
  zbaSelect.io.adduw     :=  zbaModule.io.adduw
  zbaSelect.io.slliuw    :=  zbaModule.io.slliuw
  zbaSelect.io.sh1add    :=  zbaModule.io.sh1add
  zbaSelect.io.sh2add    :=  zbaModule.io.sh2add
  zbaSelect.io.sh3add    :=  zbaModule.io.sh3add
  zbaSelect.io.sh1adduw  :=  zbaModule.io.sh1adduw
  zbaSelect.io.sh2adduw  :=  zbaModule.io.sh2adduw
  zbaSelect.io.sh3adduw  :=  zbaModule.io.sh3adduw
  zbaSelect.io.op := func

  val zbbModule = Module(new ZbbModule)
  zbbModule.io.src(0) := src1
  zbbModule.io.src(1) := src2
  zbbModule.io.op := func

  val zbcModule = Module(new ZbcModule)
  zbcModule.io.src(0) := src1
  zbcModule.io.src(1) := src2
  zbcModule.io.op := func

  val zbsModule = Module(new ZbsModule)
  zbsModule.io.src(0) := src1
  zbsModule.io.src(1) := src2
  zbsModule.io.op := func

  val result = Mux(func(5), zbbModule.io.out, Mux(func(4), zbaSelect.io.out,
                Mux(func(3), zbsModule.io.out, zbcModule.io.out)))

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := result
}
