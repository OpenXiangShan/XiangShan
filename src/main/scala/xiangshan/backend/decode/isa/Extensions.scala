package xiangshan.backend.decode.isa

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.CustomInstructions.XSTrapType
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.decode.opcode.Opcode.Opcode
import xiangshan.backend.vector.Decoder.Uop.ScalaUopTable._

object Extensions {
  sealed trait ExtBase {
    val types: Seq[InstType]
    val table: Map[BitPat, Seq[Opcode]]
  }

  sealed abstract class UnprivExt(
    val types: Seq[InstType] = Seq(),
    val table: Map[BitPat, Seq[Opcode]] = Map[BitPat, Seq[Opcode]](),
  ) extends ExtBase

  sealed abstract class PrivExt(
    val types: Seq[InstType] = Seq(),
    val table: Map[BitPat, Seq[Opcode]] = Map[BitPat, Seq[Opcode]](),
  ) extends ExtBase

  sealed trait CombExt[T1 <: ExtBase, T2 <: ExtBase] extends ExtBase

  case object I extends UnprivExt(Seq(I64Type, IType, JumpLinkType), tableI)

  case object M extends UnprivExt(Seq(M64Type, MType), tableM)

  case object A extends UnprivExt(Seq(A64Type, AType), tableA)

  case object F extends UnprivExt(Seq(F64Type, FType), tableF)

  case object D extends UnprivExt(Seq(D64Type, DType), tableD)

  case object V extends UnprivExt(Seq(VType)) // Todo

  case object Zvbb extends UnprivExt(Seq(ZVBBType))

  case object Zvknha extends UnprivExt(Seq(ZVKNHAType))

  case object Za64rs extends UnprivExt()

  case object Zabha extends UnprivExt(Seq(ZABHAType), tableZabha)

  case object Zacas extends UnprivExt(Seq(ZACASType, ZACAS64Type), tableZacas)

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

  case object Zfh extends UnprivExt(Seq(ZFHType, ZFH64Type), tableZfh)

  case object Zfhmin extends UnprivExt {
    // Todo: This extension has parts of instructions of Zfh
    //       But it has not been defined in riscv-opcodes
    override val types: Seq[InstType] = Seq(ZFHMINType)
    override val table: Map[BitPat, Seq[Opcode]] = tableZfhmin
  }

  case object H extends PrivExt {
    override val types: Seq[InstType] = Seq(H64Type, HType)
    override val table: Map[BitPat, Seq[Opcode]] = tableH
  }

  case object S extends PrivExt(Seq(SType), tableS)

  case object ZacasZabha extends CombExt[Zacas.type, Zabha.type] {
    override val types: Seq[InstType] = Seq(ZABHA_ZACASType)
    override val table: Map[BitPat, Seq[Opcode]] = tableZabhaZacas
  }

  case object ZfaF extends CombExt[Zfa.type, F.type] {
    override val types: Seq[InstType] = Seq(F_ZFAType)
    override val table: Map[BitPat, Seq[Opcode]] = tableZfaF
  }

  case object ZfaD extends CombExt[Zfa.type, D.type] {
    override val types: Seq[InstType] = Seq(D_ZFAType)
    override val table: Map[BitPat, Seq[Opcode]] = tableZfaD
  }

  case object ZfaZfh extends CombExt[Zfa.type, Zfh.type] {
    override val types: Seq[InstType] = Seq(ZFH_ZFAType)
    override val table: Map[BitPat, Seq[Opcode]] = tableZfaZfh
  }

  case object ZfhminD extends CombExt[Zfhmin.type, D.type] {
    override val types: Seq[InstType] = Seq(D_ZFHType)
    override val table: Map[BitPat, Seq[Opcode]] = tableZfhminD
  }

  case object Zicond extends UnprivExt(Seq(ZICONDType), tableZicond)

  case object Zicsr extends UnprivExt(Seq(ZICSRType), tableZicsr)

  case object Zifencei extends UnprivExt(Seq(ZIFENCEIType), tableZifencei)

  case object Zknd extends UnprivExt(Seq(ZKND64Type), tableZknd)

  case object Zkne extends UnprivExt(Seq(ZKNE64Type), tableZkne)

  case object Zknh extends UnprivExt(Seq(ZKNH64Type, ZKNHType), tableZknh)

  case object Zksed extends UnprivExt(Seq(ZKSEDType), tableZksed)

  case object Zksh extends UnprivExt(Seq(ZKSHType), tableZksh)

  case object Sdtrig extends PrivExt
//  case object Sha extends PrivExt
  case object Shcounterenw extends PrivExt
  case object Shgatpa extends PrivExt
  case object Shlcofideleg extends PrivExt
  case object Shtvala extends PrivExt
  case object Shvsatpa extends PrivExt
  case object Shvstvala extends PrivExt

  case object Svinval extends PrivExt(Seq(SVINVALType, SVINVAL_HType), tableSvinval)

  case object System extends PrivExt(Seq(SYSTEMType), tableSystem)

  case object C extends UnprivExt {
    override val types: Seq[InstType] = Seq(CType, C64Type)
  }

  case object XSTrap extends UnprivExt(Seq(XSTrapType), tableXSTrap)

  def extensions: Seq[ExtBase] = Seq(
    I, M, A, F, D, Zicsr,
    System, S, Svinval,
    Za64rs, Zabha, Zacas, ZacasZabha, Zawrs,
    Zba, Zbb, Zbc, Zbs, Zbkb, Zbkc, Zbkx,
    V, H,
    Zvknha,
    Zvbb,
    XSTrap,
    Zicond,
    Zifencei, Zknd, Zkne, Zknh, Zksed, Zksh,
    // Zcb, Zcmop,
    ZfaF, ZfaD, ZfaZfh, Zfh, Zfhmin, ZfhminD,
  )

  trait HasInst { self: ExtBase =>
    val types: Seq[InstType]
    val table: Map[BitPat, Seq[Opcode]]
  }
}
