
package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.UInt
import chisel3.util.BitPat
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.Opcode
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.DecodePatterns.InstSewLmulNfPattern
import xiangshan.backend.vector.Decoder.InstPattern.VecMemInstPattern
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.DecodePatternComb
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.BitPatToExt
import xiangshan.backend.vector.util.Decode

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
        uop.factory match {
          case _: Opcode.AluOpcodes.type => FuType.alu.U
          case _: Opcode.BruOpcodes.type => FuType.brh.U
          case _: Opcode.JmpOpcodes.type => FuType.jmp.U
          case _: Opcode.MulOpcodes.type => FuType.mul.U
          case _: Opcode.DivOpcodes.type => FuType.div.U
          case _: Opcode.LduOpcodes.type => FuType.ldu.U
          case _: Opcode.StuOpcodes.type => FuType.stu.U
          case _: Opcode.AmoOpcodes.type => FuType.mou.U
          case _: Opcode.BkuOpcodes.type => FuType.bku.U
          case _: Opcode.CsrOpcodes.type => FuType.csr.U
          case _: Opcode.FenceOpcodes.type => FuType.fence.U
          case _: Opcode.FMacOpcodes.type => FuType.fmac.U
          case _: Opcode.FDivOpcodes.type => FuType.fDivSqrt.U
          case _: Opcode.FCvtOpcodes.type => FuType.fcvt.U
          case _: Opcode.FMiscOpcodes.type => FuType.fcmp.U
          case _: Opcode.VSetOpcodes.type => FuType.vset.U
          case _: Opcode.VIAluOpcodes.type => FuType.vialuF.U
          case _: Opcode.VMoveOpcodes.type => FuType.vmove.U
          case _: Opcode.VMAluOpcodes.type => FuType.vmpu.U
          case _: Opcode.VIMacOpcodes.type => FuType.vimac.U
          case _: Opcode.VIRedOpcodes.type => FuType.vipu.U
          case _: Opcode.VIPermOpcodes.type => FuType.vppu.U
          case _: Opcode.VIDivOpcodes.type => FuType.vidiv.U
          case _: Opcode.VSha256msOpcodes.type => FuType.vsha256ms.U
          case _: Opcode.VSha256cOpcodes.type => FuType.vsha256c.U
        }
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
