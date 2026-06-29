package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import top.ArgParser
import xiangshan._
import xiangshan.backend.decode.isa.Instructions
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.vector.util.Verilog


class VTypeGen(implicit p: Parameters) extends XSModule {
  import VTypeGen._

  val in = IO(Input(new In))
  val out = IO(Output(new Out))

  private val vtypeSpec = RegInit(VType.init())

  private val vtypeArch = RegInit(VType.init())

  private val vtypeSpecNext = WireInit(vtypeSpec)

  private val vtypeArchNext = WireInit(vtypeArch)
  private val vtypeNewVec: Vec[VTypeNewEntry] = VecInit((0 until DecodeWidth).map { i =>
    val inst  = in.insts(i)
    val entry = VTypeNewEntry.fromInst(inst.bits, inst.valid)
    entry
  })

  /**
   * Binary-tree parallel-prefix inclusive scan over 9 elements (seed + 8 slots).
   *
   * s0---------------------------(0) vtypePrefix(0)
   *    \
   * s1-m01-----------------------(1) vtypePrefix(1)
   *      \   \
   * s2----\--m02-----------------(2) vtypePrefix(2)
   *    \   \
   * s3-m23-m03-------------------(2) vtypePrefix(3)
   *          \   \   \   \
   * s4--------\---\---\--m04-----(3) vtypePrefix(4)
   *    \       \   \   \
   * s5-m45------\---\--m05-------(3) vtypePrefix(5)
   *      \   \   \   \
   * s6----\--m46--\--m06---------(3) vtypePrefix(6)
   *    \   \       \
   * s7-m67-m47-----m07-----------(3) vtypePrefix(7)
   *                  \
   * s8---------------m08---------(4) vtypePrefix(8)
   */
  assert(DecodeWidth == 8, "VTypeGen: hardcoded binary-tree prefix requires DecodeWidth == 8")
  private val s0 = VTypeNewEntry.fromVType(vtypeSpec)
  private val Seq(s1, s2, s3, s4, s5, s6, s7, s8) = (0 until 8).map(vtypeNewVec(_))

  // Step 1: merge adjacent pairs
  private val m01 = s0.merge(s1)   // -> pref[1]
  private val m23 = s2.merge(s3)
  private val m45 = s4.merge(s5)
  private val m67 = s6.merge(s7)

  // Step 2: extend left edge downward
  private val m02 = m01.merge(s2)  // -> pref[2]
  private val m03 = m01.merge(m23) // -> pref[3]
  private val m46 = m45.merge(s6)
  private val m47 = m45.merge(m67)

  // Step 3: cross halves
  private val m04 = m03.merge(s4)  // -> pref[4]
  private val m05 = m03.merge(m45) // -> pref[5]
  private val m06 = m03.merge(m46) // -> pref[6]
  private val m07 = m03.merge(m47) // -> pref[7]

  // Step 4: final element
  private val m08 = m07.merge(s8)  // -> pref[8]

  private val vtypePrefix = IndexedSeq(s0, m01, m02, m03, m04, m05, m06, m07, m08)

  for (i <- 0 until DecodeWidth) {
    out.vtype(i) := vtypePrefix(i + 1).toVType
  }

  // oldVType(i) = speculative vtype before instruction i executes.
  // Build as [vtypeSpec, out.vtype(0..7)], then drop the extra tail element (out.vtype(7)).
  private val oldVType = vtypeSpec +: out.vtype
  out.oldVType := VecInit(oldVType.init)

  // assign the last vtype to vtypeSpec
  private val vtypeNew = out.vtype(DecodeWidth - 1)

  vtypeSpec := vtypeSpecNext
  vtypeArch := vtypeArchNext

  when(in.commitVType.hasVsetvl) {
    vtypeArchNext := in.vsetvlVType
  } .elsewhen(in.commitVType.vtype.valid) {
    vtypeArchNext := in.commitVType.vtype.bits
  }

  when(in.commitVType.hasVsetvl) {
    // when vsetvl instruction commit, also update vtypeSpec, because vsetvl flush pipe
    vtypeSpecNext := in.vsetvlVType
  }.elsewhen(in.walkVType.valid) {
    vtypeSpecNext := in.walkVType.bits
  }.elsewhen(in.walkToArchVType) {
    vtypeSpecNext := vtypeArch
  }.elsewhen(in.canUpdateVType) {
    vtypeSpecNext := vtypeNew
  }
}

object VTypeGen {
  def main(args: Array[String]): Unit = {
    val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
      args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

    val defaultConfig = config.alterPartial({
      case XSCoreParamsKey => config(XSTileKey).head
    })

    Verilog.emitVerilog(
      new VTypeGen()(defaultConfig),
      Array(
        "--full-stacktrace",
        "--target-dir", "build/VTypeGen",
      )
    )
  }

  class In()(implicit p: Parameters) extends XSBundle {
    val insts = Flipped(Vec(DecodeWidth, ValidIO(UInt(32.W))))
    val walkToArchVType = Input(Bool())
    val walkVType   = Flipped(Valid(new VType))
    val canUpdateVType = Input(Bool())
    val vsetvlVType = Input(new VType)
    val commitVType = new Bundle {
      val vtype = Flipped(Valid(new VType))
      val hasVsetvl = Input(Bool())
    }
  }

  class Out()(implicit p: Parameters) extends XSBundle {
    val vtype    = Output(Vec(DecodeWidth, new VType))
    val oldVType = Output(Vec(DecodeWidth, new VType))
  }
}

// Per-slot decoded vtype info
class VTypeNewEntry extends Bundle {
  val replVl  = Bool() // this vset replaces the VL ratio (i.e. not keepVl x0,x0)
  val isVset  = Bool() // this entry is a valid vset-family instruction
  val vtype   = new VType
  val vlratio = UInt(7.W)

  /** Merge two entries: this is in1 (older), in2 is the newer one. */
  def merge(in2: VTypeNewEntry): VTypeNewEntry = {
    val in1 = this
    val out = Wire(new VTypeNewEntry)

    out.replVl  := in2.replVl || in1.replVl
    out.isVset  := in2.isVset || in1.isVset
    out.vtype   := Mux(in2.isVset, in2.vtype, in1.vtype)
    out.vlratio := Mux(in2.replVl, in2.vlratio, in2.vlratio & in1.vlratio)
    out
  }

  /** Convert to VType: OR `vill` with `vlratio == 0`; zero all fields when illegal. */
  def toVType: VType = {
    val res = WireInit(this.vtype)
    val isIllegal = this.vtype.illegal || !this.vlratio.orR
    res.illegal := isIllegal
    when (isIllegal) {
      res.vma := 0.U
      res.vta := 0.U
      res.vsew := 0.U
      res.vlmul := 0.U
    }
    res
  }
}

object VTypeNewEntry {

  // Compute a 7-bit one-hot vlratio as vlmul/vsew * 16.
  def calVlratio(vtype: VType) : UInt = {
    Mux(vtype.illegal || vtype.vsew(2) , 0.U, 1.U << ((vtype.vlmul ^ 4.U) -& vtype.vsew))(7,1)
  }

  def fromVType(vtype: VType): VTypeNewEntry = {
    val out = Wire(new VTypeNewEntry)
    out.replVl  := true.B
    out.isVset  := false.B
    out.vtype   := vtype
    out.vlratio := calVlratio(vtype)
    out
  }

  def fromInst(inst: UInt, valid: Bool): VTypeNewEntry = {
    val instField  = inst.asTypeOf(new XSInstBitFields)
    val isVsetivli = Instructions.VSETIVLI === inst
    val isVsetvli  = Instructions.VSETVLI  === inst
    val isVseti    = isVsetivli || isVsetvli

    val out = Wire(new VTypeNewEntry)
    val vtype = out.vtype
    vtype.vlmul   := instField.ZIMM_VSETVLI(2, 0)
    vtype.vsew    := instField.ZIMM_VSETVLI(5, 3)
    vtype.vta     := instField.ZIMM_VSETVLI(6)
    vtype.vma     := instField.ZIMM_VSETVLI(7)
    vtype.illegal := Mux(
      isVsetivli,
      instField.ZIMM_VSETIVLI(9, 8) =/= 0.U,
      instField.ZIMM_VSETVLI(10, 8) =/= 0.U
    )

    val isKeepVl = isVsetvli && instField.RD === 0.U && instField.RS1 === 0.U
    val isVset   = isVseti && valid
    out.isVset  := isVset
    out.replVl  := isVset && !isKeepVl
    out.vlratio := Mux(isVset, calVlratio(vtype), 127.U)
    out
  }
}
