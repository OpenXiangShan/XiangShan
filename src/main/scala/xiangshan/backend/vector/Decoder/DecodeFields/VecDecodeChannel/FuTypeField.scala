
package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.UInt
import chisel3.util.BitPat
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.{Opcode, Opcodes}
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.DecodePatterns.InstSewLmulNfPattern
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.DecodePatternComb
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.BitPatToExt

import scala.language.implicitConversions

class FuTypeField(uopIdx: Int) extends DecodeField[InstSewLmulNfPattern, UInt] {

  override def name: String = s"fuType$uopIdx"

  override def chiselType: UInt = FuType()

  override def genTable(op: InstSewLmulNfPattern): BitPat = {
    val DecodePatternComb(instP, sewP, lmulP, nfP) = op

    val uopSeq = UopInfoFieldVec.genUopSeq(op)
    if (!uopSeq.isDefinedAt(uopIdx)) {
      return default
    }
    val uop = uopSeq(uopIdx)
    val res: UInt = {
      try {
        FuTypeField.genFuType(uop).U
      }
      catch {
        case e: NoSuchElementException =>
          println(s"inst ${op.p1.name} is not in uop table")
          throw e
        case e: NullPointerException =>
          // println(s"inst ${op.p1.name} with sew ${sewP}, lmul ${lmulP}, nf ${nfP} has not decoded")
          BitPat.Y(FuType.width).U
        case e: MatchError =>
          println(s"inst ${op.p1.name}'s uop ${uop} futype ${uop.factory} not match")
          throw e
        case e: Throwable =>
          println(s"inst ${op.p1.name} throw exception")
          throw e
      }
    }

    res.pad(FuType.width)
  }

  implicit def UIntCastToBitPat(uint: UInt): BitPat = BitPat(uint)
}

object FuTypeField {
  def genFuType(opcodes: Opcodes): FuType.OHType = {
    opcodes match {
      case _: Opcode.AluOpcodes.type => FuType.alu
      case _: Opcode.BruOpcodes.type => FuType.brh
      case _: Opcode.LinkOpcodes.type => FuType.link
      case _: Opcode.NewJmpOpcodes.type => FuType.njmp
      case _: Opcode.MulOpcodes.type => FuType.mul
      case _: Opcode.DivOpcodes.type => FuType.div
      case _: Opcode.LduOpcodes.type => FuType.ldu
      case _: Opcode.StuOpcodes.type => FuType.stu
      case _: Opcode.AmoOpcodes.type => FuType.mou
      case _: Opcode.BkuOpcodes.type => FuType.bku
      case _: Opcode.CsrOpcodes.type => FuType.csr
      case _: Opcode.FenceOpcodes.type => FuType.fence
      case _: Opcode.FAluOpcodes.type => FuType.falu
      case _: Opcode.FMacOpcodes.type => FuType.fmul
      case _: Opcode.FDivOpcodes.type => FuType.fDivSqrt
      case _: Opcode.FCvtOpcodes.type => FuType.fcvt
      case _: Opcode.FMiscOpcodes.type => FuType.fcmp
      case _: Opcode.VFMacOpcodes.type => FuType.vfma
      case _: Opcode.VFDivOpcodes.type => FuType.vfdiv
      case _: Opcode.VFCvtOpcodes.type => FuType.vfcvt
      case _: Opcode.VFMiscOpcodes.type => FuType.vfalu
      case _: Opcode.VSetOpcodes.type => FuType.vset
      case _: Opcode.VIAluOpcodes.type => FuType.vialuF
      case _: Opcode.VMoveOpcodes.type => FuType.vmove
      case _: Opcode.VMAluOpcodes.type => FuType.vmpu
      case _: Opcode.VIMacOpcodes.type => FuType.vimac
      case _: Opcode.VIRedOpcodes.type => FuType.vipu
      case _: Opcode.VIPermOpcodes.type => FuType.vppu
      case _: Opcode.VIDivOpcodes.type => FuType.vidiv
      case _: Opcode.VFRedOpcodes.type => FuType.vfalu
      case _: Opcode.VSha256msOpcodes.type => FuType.vsha256ms
      case _: Opcode.VSha256cOpcodes.type => FuType.vsha256c
    }
  }

  def genFuType(uop: Opcode): FuType.OHType = {
    this.genFuType(uop.factory)
  }
}
