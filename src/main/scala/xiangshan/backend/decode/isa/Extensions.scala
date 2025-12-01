package xiangshan.backend.decode.isa

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.CustomInstructions.XSTrapType
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.vector.Decoder.Uop.ScalaUopTable._

object Extensions {
  sealed trait ExtBase {
    val types: Seq[InstType]
    val table: Map[BitPat, Opcode]
  }

  sealed abstract class UnprivExt(
    val types: Seq[InstType] = Seq(),
    val table: Map[BitPat, Opcode] = Map[BitPat, Opcode](),
  ) extends ExtBase

  sealed abstract class PrivExt(
    val types: Seq[InstType] = Seq(),
    val table: Map[BitPat, Opcode] = Map[BitPat, Opcode](),
  ) extends ExtBase

  sealed trait CombExt[T1 <: ExtBase, T2 <: ExtBase] extends ExtBase

  case object I extends UnprivExt(Seq(I64Type, IType), tableI)

  case object M extends UnprivExt(Seq(M64Type, MType), tableM)

  case object A extends UnprivExt(Seq(A64Type, AType), tableA)

  case object F extends UnprivExt(Seq(F64Type, FType), tableF)

  case object D extends UnprivExt(Seq(D64Type, DType), tableD)

  case object V extends UnprivExt(Seq(VType)) // Todo

  case object Za64rs extends UnprivExt()

  case object Zabha extends UnprivExt(Seq(ZABHAType))

  case object Zacas extends UnprivExt(Seq(ZACASType, ZACAS64Type))

  case object Zawrs extends UnprivExt(Seq(ZAWRSType), tableZawrs)

  case object Zba extends UnprivExt(Seq(ZBA64Type, ZBAType), tableZba)

  case object Zbb extends UnprivExt(Seq(ZBB64Type, ZBBType), tableZbb)

  case object Zbc extends UnprivExt(Seq(ZBCType), tableZbc)

  case object Zbkb extends UnprivExt(Seq(ZBKB64Type, ZBKBType), tableZbkb)

  case object Zbkc extends UnprivExt(Seq()) {
    // Todo: This extension has parts of instructions of Zbc
    //       But it has not been defined in riscv-opcodes
  }

  case object Zbkx extends UnprivExt(Seq(ZBKXType), tableZbkx)

  case object Zbs extends UnprivExt(Seq(ZBS64Type, ZBSType), tableZbs)

  case object Zcb extends UnprivExt(Seq(ZCBType, ZCB64Type))

  case object Zcmop extends UnprivExt(Seq(ZCMOPType))

  case object Zfa extends UnprivExt

  case object Zfh extends UnprivExt(Seq(ZFHType, ZFH64Type))

  case object Zfhmin extends UnprivExt {
    // Todo: This extension has parts of instructions of Zfh
    //       But it has not been defined in riscv-opcodes
    override val types: Seq[InstType] = Seq()
  }

  case object H extends PrivExt {
    override val types: Seq[InstType] = Seq(H64Type, HType)
  }

  case object S extends PrivExt(Seq(SType), tableS)

  case object ZacasZabha extends CombExt[Zacas.type, Zabha.type] {
    override val types: Seq[InstType] = Seq(ZABHA_ZACASType)
    override val table: Map[BitPat, Opcode] = Map()
  }

  case object ZfaF extends CombExt[Zfa.type, F.type] {
    override val types: Seq[InstType] = Seq(F_ZFAType)
    override val table: Map[BitPat, Opcode] = Map()
  }

  case object ZfaD extends CombExt[Zfa.type, D.type] {
    override val types: Seq[InstType] = Seq(D_ZFAType)
    override val table: Map[BitPat, Opcode] = Map()
  }

  case object ZfaZfh extends CombExt[Zfa.type, Zfh.type] {
    override val types: Seq[InstType] = Seq(ZFH_ZFAType)
    override val table: Map[BitPat, Opcode] = Map()
  }

  case object Zicsr extends UnprivExt(Seq(ZICSRType), tableZicsr)

  case object Sdtrig extends PrivExt
//  case object Sha extends PrivExt
  case object Shcounterenw extends PrivExt
  case object Shgatpa extends PrivExt
  case object Shlcofideleg extends PrivExt
  case object Shtvala extends PrivExt
  case object Shvsatpa extends PrivExt
  case object Shvstvala extends PrivExt

  case object System extends PrivExt(Seq(SYSTEMType), tableSystem)

  case object C extends UnprivExt {
    override val types: Seq[InstType] = Seq(CType, C64Type)
  }

  case object XSTrap extends UnprivExt(Seq(XSTrapType), tableXSTrap)

  trait HasInst { self: ExtBase =>
    val types: Seq[InstType]
    val table: Map[BitPat, Opcode]
  }
}
