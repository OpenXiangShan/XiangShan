package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{LookupTreeDefault, SignExt, XSDebug, ZeroExt}
import xiangshan._


abstract class BmuDataModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Input(Vec(2, UInt(XLEN.W)))
    val op = Input(UInt(6.W))
    val out = Output(UInt(XLEN.W))
  })
  val (src1, src2, op, out) = (io.src(0), io.src(1), io.op, io.out)
}

class ZbaModule(implicit p: Parameters) extends BmuDataModule{

  val src1w = src1(31,0)
  val shamt = src2(5,0)

  val adduw = src1w + src2
  val slliuw = src1w << shamt
  val sh1add = (src1 << 1) + src2
  val sh2add = (src1 << 2) + src2
  val sh3add = (src1 << 3) + src2
  val sh1adduw = (src1w << 1) + src2
  val sh2adduw = (src1w << 2) + src2
  val sh3adduw = (src1w << 3) + src2

  out := LookupTreeDefault(op, adduw, List(
    BMUOpType.add_uw     -> adduw,
    BMUOpType.slli_uw    -> slliuw,
    BMUOpType.sh1add    -> sh1add,
    BMUOpType.sh2add    -> sh2add,
    BMUOpType.sh3add    -> sh3add,
    BMUOpType.sh1add_uw  -> sh1adduw,
    BMUOpType.sh2add_uw  -> sh2adduw,
    BMUOpType.sh3add_uw  -> sh3adduw
  ))
}

class ZbbModule(implicit p: Parameters) extends BmuDataModule{

  val src1w = src1(31,0)
  val shamt = Cat(op(0) && src2(5), src2(4, 0))

  val andn = src1 & ~src2
  val orn  = src1 | ~src2
  val xnor = src1 ^ ~src2

  val max = Mux(src1.asSInt() > src2.asSInt(), src1, src2)
  val min = Mux(src1.asSInt() < src2.asSInt(), src1, src2)
  val maxu = Mux(src1 > src2, src1, src2)
  val minu = Mux(src1 > src2, src2, src1)

  val sextb = SignExt(src1(8, 0),XLEN)
  val sexth = SignExt(src1(15, 0),XLEN)
  val zexth = Cat(0.U(48.W), src1(15, 0))

  val rev8 = Cat(src1(7,0), src1(15,8), src1(23,16), src1(31,24), src1(39,32), src1(47,40), src1(55,48), src1(63,56))
  val orcb = Cat(Reverse(src1(63,56)), Reverse(src1(55,48)), Reverse(src1(47,40)), Reverse(src1(39,32)), 
    Reverse(src1(31,24)), Reverse(src1(23,16)), Reverse(src1(15,8)), Reverse(src1(7,0)))

  val rol  = src1<<shamt | src1>>((~shamt).asUInt()+&1.U)
  val rolw = SignExt((src1w<<shamt | src1w>>(32.U-shamt))(31,0), 64)
  val ror  = src1>>shamt | src1<<((~shamt).asUInt()+&1.U)
  val rorw = SignExt((src1w>>shamt | src1w<<(32.U-shamt))(31,0), 64)

  val clz   = ~Reverse(src1) & (Reverse(src1)-1.U)
  val clzw  = ~Reverse(src1w) & (Reverse(src1w)-1.U)
  val ctz   = ~src1 & (src1-1.U)
  val ctzw  = ~src1w & (src1w-1.U)
  val cpop  = PopCount(src1)
  val cpopw = PopCount(src1w)


  out := LookupTreeDefault(op, andn, List(
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

  ))
}

class ZbcModule(implicit p: Parameters) extends BmuDataModule{

  val mul = Wire(Vec(64, UInt(128.W)))

  for(i <- 0 until XLEN) {
    if(i == 0) mul(i) := Mux(src2(i), src1, 0.U)
    else mul(i) := mul(i-1) ^ Mux(src2(i), src1<<i, 0.U)
  }

  val clmul  = mul(63)(63,0)
  val clmulh = mul(63)(127,64)
  val clmulr = mul(63)(126,63)

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

  out := LookupTreeDefault(op, bset, List(
    BMUOpType.bset   -> bset,
    BMUOpType.bclr   -> bclr,
    BMUOpType.binv   -> binv,
    BMUOpType.bext   -> bext
  ))
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
  zbaModule.io.op := func

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

  val result = Mux(func(5), zbbModule.io.out, Mux(func(4), zbaModule.io.out,
                Mux(func(3), zbsModule.io.out, zbcModule.io.out)))

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := result
}
