package xiangshan.backend.decode

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.util.uintToBitPat
import utils._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan._
import freechips.rocketchip.rocket.Instructions._

abstract class VecType {
  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")
  def generate() : List[BitPat]
  def asOldDecodeOutput(): List[BitPat] = {
    val src1::src2::src3::fu::fuOp::xWen::fWen::vWen::mWen::xsTrap::noSpec::blockBack::flushPipe::selImm::Nil = generate()
    List (src1, src2, src3, fu, fuOp, xWen, fWen, xsTrap, noSpec, blockBack, flushPipe, selImm)
  }
}

case class OPIVV(fu: BitPat, fuOp: BitPat, vWen: Boolean, mWen: Boolean) extends VecType {
  def generate() : List[BitPat] = {
    List (SrcType.vp, SrcType.vp, SrcType.X, fu, fuOp, N, N, vWen.B, mWen.B, N, N, N, N, SelImm.X)
  }
}

case class OPIVX() extends VecType {
  def generate() : List[BitPat] = { null }
}

case class OPIVI() extends VecType {
  def generate() : List[BitPat] = { null }
}

case class OPMVV(fu: BitPat, fuOp: BitPat, xWen: Boolean, vWen: Boolean, mWen: Boolean, others: Any) extends VecType {
  def generate() : List[BitPat] = {
    List (SrcType.vp, SrcType.vp, SrcType.X, fu, fuOp, xWen.B, N, vWen.B, mWen.B, N, N, N, N, SelImm.X)
  }
}

case class OPMVX() extends VecType {
  def generate() : List[BitPat] = { null }
}

case class OPFVV() extends VecType {
  def generate() : List[BitPat] = { null }
}

case class OPFVF(fu: BitPat, fuOp: BitPat, fWen: Boolean, vWen: Boolean, mWen: Boolean) extends VecType {
  def generate() : List[BitPat] = {
    List (SrcType.vp, SrcType.fp, SrcType.X, fu, fuOp, N, fWen.B, vWen.B, mWen.B, N, N, N, N, SelImm.X)
  }
}

case class VSET() extends VecType {
  def generate() : List[BitPat] = { null }
}

case class VLS() extends VecType {
  def generate() : List[BitPat] = { null }
}

object VecDecoder extends DecodeConstants {
  private def F = false
  private def T = true

  val opivvTable: Array[(BitPat, List[BitPat])] = Array(
    VADD_VV   -> OPIVV(FuType.vipu, VipuType.dummy, T, F).generate(),

    VMSEQ_VV  -> OPIVV(FuType.vipu, VipuType.dummy, F, T).generate(),
  )

  val opivxTable: Array[(BitPat, List[BitPat])] = Array()
  val opiviTable: Array[(BitPat, List[BitPat])] = Array()

  val opmvvTable: Array[(BitPat, List[BitPat])] = Array()
  val opmvxTable: Array[(BitPat, List[BitPat])] = Array()

  val opfvvTable: Array[(BitPat, List[BitPat])] = Array()

  val opfvfTable: Array[(BitPat, List[BitPat])] = Array(
    VFADD_VF  -> OPFVF(FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
    VMFEQ_VF  -> OPFVF(FuType.vfpu, VfpuType.dummy, F, F, T).generate(),
  )

  val vsetTable: Array[(BitPat, List[BitPat])] = Array()
  val vlsTable: Array[(BitPat, List[BitPat])] = Array()

  val table = opivvTable ++ opivxTable ++ opiviTable ++
              opmvvTable ++ opmvxTable ++
              opfvvTable ++ opfvfTable ++
              vsetTable ++ vlsTable
}
