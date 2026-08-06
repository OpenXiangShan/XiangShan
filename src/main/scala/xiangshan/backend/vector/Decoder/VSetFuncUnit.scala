package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.vector.Bundles.{VType, Vl}
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.util.{BoolDecodeField, DecodeField, DecodeTable}
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField
import xiangshan.backend.vector.util.Verilog

class VSetFuncUnit(
  vlen: Int,
  elen: Int,
  xlen: Int,
) extends Module {
  import VSetFuncUnit._

  private val VlWidth = Vl.width(vlen)

  val in = IO(Input(new In(vlen, xlen)))
  val out = IO(Output(new Out(vlen)))

  private val oldVType = in.oldVType
  private val vsetvl = in.vsetvlVType.valid
  private val vsetvli = in.vsetvliVType.valid
  private val vsetivli = in.vsetivliVType.valid

  private val normalStripmining = (vsetvl || vsetvli || vsetivli) && !in.rs1IsZero
  private val setVlVlmax = (vsetvl || vsetvli) && in.rs1IsZero && !in.rdIsZero
  private val keepVl = (vsetvl || vsetvli) && in.rs1IsZero && in.rdIsZero

  // Rsvd: reserved
  private val vsetvlRsvdIll   = in.vsetvlVType.bits.drop(vtypeImmWidth) =/= 0.U
  private val vsetvliRsvdIll  = in.vsetvliVType.bits.drop(vtypeImmWidth) =/= 0.U
  private val vsetivliRsvdIll = in.vsetivliVType.bits.drop(vtypeImmWidth) =/= 0.U

  private val vtype = Mux1H(Seq(
    vsetvl   -> vsetvlRsvdIll   ## in.vsetvlVType.bits.take(vtypeImmWidth),
    vsetvli  -> vsetvliRsvdIll  ## in.vsetvliVType.bits.take(vtypeImmWidth),
    vsetivli -> vsetivliRsvdIll ## in.vsetivliVType.bits.take(vtypeImmWidth),
  )).asTypeOf(VType())

  private val avl = Mux1H(Seq(
    in.vlFromImm.valid -> in.vlFromImm.bits.pad(VlWidth),
    // if vlReg is more than VLEN - 1, make it be (VLEN + vlReg % VLEN) which is no less than VLEN
    in.vlFromGp.valid -> Cat(in.vlFromGp.bits.drop(VlWidth - 1).orR, in.vlFromGp.bits.take(VlWidth - 1)),
  ))

  private val vlmaxField = new VlmaxField(vlen, elen)
  private val villField = new VillField(elen)

  private val decodeTable = new DecodeTable(sewLmulPatterns, Seq(vlmaxField, villField))
  private val newVTypeDecodeBundle = decodeTable.decode(vtype.vsew ## vtype.vlmul)
  private val oldVlmax = decodeTable.decode(oldVType.bits.vsew ## oldVType.bits.vlmul)(vlmaxField)

  private val vlmax = newVTypeDecodeBundle(vlmaxField)
  private val villFromVTypeImm = newVTypeDecodeBundle(villField)

  // vlmax is OH, use |(a&b) will be cheaper than =/=
  private val vlmaxChange = (vlmax & oldVlmax).orR && oldVType.valid

  private val vill = vtype.illegal || villFromVTypeImm || in.rdIsZero && in.rs1IsZero && vlmaxChange

  private val vl =
    Mux(
      in.readVl.valid,
      in.readVl.bits,
      Mux1H(Seq(
        vill -> 0.U,
        (!vill && (normalStripmining && avl >= vlmax || setVlVlmax)) -> vlmax,
        (!vill && normalStripmining && avl < vlmax) -> avl,
        (!vill && keepVl && in.vlFromVl.valid) -> in.vlFromVl.bits,
      ))
    )


  out.vtype.illegal := vill
  out.vtype.vma     := Mux(!vill, vtype.vma, 0.U)
  out.vtype.vta     := Mux(!vill, vtype.vta, 0.U)
  out.vtype.vsew    := Mux(!vill, vtype.vsew, 0.U)
  out.vtype.vlmul   := Mux(!vill, vtype.vlmul, 0.U)
  out.vlmax := vlmax
  out.vl := vl

  dontTouch(vlmax)
  dontTouch(oldVlmax)
  dontTouch(vtype)
}

object VSetFuncUnit {
  class In(vlen: Int, xlen: Int) extends Bundle {
    val vsetvlVType = ValidIO(UInt(xlen.W))
    val vsetvliVType = ValidIO(UInt(11.W))
    val vsetivliVType = ValidIO(UInt(10.W))
    val readVl = ValidIO(Vl(vlen))
    val rdIsZero = Bool()
    val rs1IsZero = Bool()
    val oldVType = ValidIO(VType())
    // vl from general register
    val vlFromGp = ValidIO(UInt(xlen.W))
    // vl from vsetivli.uimm, the width of uimm is 5
    val vlFromImm = ValidIO(UInt(5.W))
    // vl from vl regfile
    val vlFromVl = ValidIO(Vl(vlen))
  }

  class Out(vlen: Int) extends Bundle {
    val vtype = VType()
    val vlmax = Vl(vlen)
    val vl = Vl(vlen)
  }

  val vtypeImmWidth = 8

  val sewLmulPatterns: Seq[DecodePatternComb2[SewPattern, LmulPattern]] = for {
    sewP <- SewPattern.all
    lmulP <- LmulPattern.all
  } yield {
    sewP ## lmulP
  }

  class VillField(elen: Int) extends BoolDecodeField[
    DecodePatternComb2[SewPattern, LmulPattern],
  ] {
    type Pattern = DecodePatternComb2[SewPattern, LmulPattern]

    // The patterns not passed in will assign vill to 1 as default
    override def default: BitPat = BitPat.Y()

    override def name: String = "vill"

    override def genTable(op: Pattern): BitPat = {
      val DecodePatternComb(vsewP: SewPattern, vlmulP: LmulPattern) = op
      val sew = vsewP.sewValue
      val lmul = vlmulP.lmulValue

      (sew == 0 || lmul == 0 || sew > lmul.min(1) * elen).B.toBitPat
    }
  }

  class VlmaxField(vlen: Int, elen: Int) extends DecodeField[
    DecodePatternComb2[SewPattern, LmulPattern],
    UInt,
  ] {
    type Pattern = DecodePatternComb2[SewPattern, LmulPattern]

    override def name: String = "vlmax"

    override def chiselType: UInt = Vl(vlen)

    override def genTable(op: Pattern): BitPat = {
      val DecodePatternComb(vsewP, vlmulP) = op
      val sew = vsewP.sewValue
      val lmul = vlmulP.lmulValue

      if (sew == 0 || lmul == 0 || sew > lmul.min(1) * elen)
        return BitPat.N(Vl.width(vlen))
      val vlmax = (vlen * lmul / sew).toInt
      vlmax.U(Vl.width(vlen).W).toBitPat
    }
  }

  def main(args: Array[String]): Unit = {
    val targetDir = "build/decoder"

    Verilog.emitVerilog(
      new VSetFuncUnit(vlen = 128, elen = 64, xlen = 64),
      Array("--full-stacktrace", "--target-dir", targetDir),
    )
  }
}
