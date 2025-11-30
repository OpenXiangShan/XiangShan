package xiangshan.backend.vector.Decoder.Uop.UopTrait

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.FuType.OHType
import xiangshan.backend.vector.Decoder.Types._
import xiangshan.backend.vector.Decoder.Uop.UopInfoRename

import scala.language.experimental.macros

class MutableRef[A](var value: A) {
  def update(newVaue: A): Unit = value = newVaue

  def apply(): A = value
}

abstract class UopTrait

trait WenTrait extends UopTrait
trait VecWenTrait extends UopTrait

case object GpWen extends WenTrait
case object VlWen extends WenTrait
case object FpWen extends WenTrait
case object VpWen extends WenTrait with VecWenTrait
case object VsWen extends WenTrait with VecWenTrait
case object VwWen extends WenTrait with VecWenTrait
case object VmWen extends WenTrait with VecWenTrait
case object VwsWen extends WenTrait with VecWenTrait
case object VxsatWen extends WenTrait
case object FFlagsWen extends WenTrait

abstract class Src1Trait(val srcType: SrcType) extends UopTrait

case object Src1Gp  extends Src1Trait(Gp)
case object Src1Fp  extends Src1Trait(Fp)
case object Src1Vp  extends Src1Trait(Vp)
case object Src1Vm  extends Src1Trait(Vm)
case object Src1Vw  extends Src1Trait(Vw)
case object Src1Vs  extends Src1Trait(Vs)
case object Src1Vws extends Src1Trait(Vws)
case object Src1Imm extends Src1Trait(Imm)

abstract class Src2Trait(val srcType: SrcType) extends UopTrait

case object Src2Gp extends Src2Trait(Gp)
case object Src2Fp extends Src2Trait(Fp)
case object Src2Vp extends Src2Trait(Vp)
case object Src2Vw extends Src2Trait(Vw)
case object Src2Vs extends Src2Trait(Vs)
case object Src2Vm extends Src2Trait(Vm)

abstract class Src3Trait(val srcType: SrcType) extends UopTrait
case object Src3Gp extends Src3Trait(Gp)
case object Src3Fp extends Src3Trait(Fp)
case object Src3Vp extends Src3Trait(Vp)
case object Src3Vw extends Src3Trait(Vw)
case object Src3Vs extends Src3Trait(Vs)
case object Src3Vm extends Src3Trait(Vm)

trait V0Trait extends UopTrait
object V0RenAsMask extends V0Trait
object V0RenAsSrc extends V0Trait

object VlRen extends UopTrait

trait CtrlRen extends UopTrait
object VxrmRen extends CtrlRen

object Order extends UopTrait

abstract class UopBase(baseTraits: UopTrait*) {

  var fuType: UInt = null

  var opcode: UInt = null

  def setFuType(ohType: OHType): this.type = {
    this.fuType = ohType.U(ohType.size.W)
    this
  }

  def setOpcode(opcode: UInt): this.type = {
    this.opcode = opcode
    this
  }

  def allowedTraits: Set[UopTrait] = Set()

  def set[B](field: this.type => MutableRef[B], value: B): this.type = {
    field(this).update(value)
    this
  }

  def genUopInfoRenameBitPat: BitPat = {
    UopInfoRename.genBitPat(
      src1Type = this.getSrc1Type.map(_.srcType.operand),
      src2Type = this.getSrc2Type.map(_.srcType.operand),
      vlRen = vlRen,
      v0Ren = v0Ren,
      maskType = this match {
        case uop: VecUop => uop.maskType()
        case _ => NoMask
      },
      intRmRen = this.vxrmRen,
      readVdAsSrc = this.getSrc3Type.exists(_.srcType.operand == Operand.VP),
      gpWen = gpWen,
      fpWen = fpWen,
      vpWen = vpWen,
      vlWen = vlWen,
      vxsatWen = vxsatWen,
      vdAlloc = vdAlloc
    )
  }

  def uopInfoRenameString: String = {
    f"src1:${getSrc1Type.orNull}%-7s," +
    f"src2:${getSrc2Type.orNull}%-7s," +
    f"src3:${getSrc3Type.orNull}%-7s," +
    f"vlRen:${if (this.vlRen) "T" else "F"}," +
    f"v0Ren:${if (this.v0Ren) "T" else "F"}," +
    f"gpWen:${if (this.gpWen) "T" else "F"}," +
    f"fpWen:${if (this.fpWen) "T" else "F"}," +
    f"vpWen:${if (this.vpWen) "T" else "F"}," +
    f"vlWen:${if (this.vlWen) "T" else "F"},"
  }

  def getSrc1Type: Option[Src1Trait] = {
    this.getTraits.collectFirst { case t: Src1Trait => t }
  }

  def getSrc2Type: Option[Src2Trait] = {
    this.getTraits.collectFirst { case t: Src2Trait => t }
  }

  def getSrc3Type: Option[Src3Trait] = {
    this.getTraits.collectFirst { case t: Src3Trait => t }
  }

  def vlRen: Boolean = this.getTraits.contains(VlRen)

  def v0Ren: Boolean = this.getTraits.exists(_.isInstanceOf[V0Trait])

  def vxrmRen: Boolean = this.getTraits.contains(VxrmRen)

  def gpWen: Boolean = this.getTraits.contains(GpWen)

  def fpWen: Boolean = this.getTraits.contains(FpWen)

  def vpWen: Boolean = this.getTraits.exists(_.isInstanceOf[VecWenTrait])

  def vlWen: Boolean = this.getTraits.contains(VlWen)

  def vxsatWen: Boolean = this.getTraits.contains(VxsatWen)

  private def vdAlloc: Boolean = {
    this match {
      case uop: VecUop => uop.vdAlloc.value
      case _ => false
    }
  }

  def s1v: this.type = {
    require(this.allowedTraits.contains(Src1Vp), s"${allowedTraits.mkString("allowedTraits: (", ",", ")")}")
    require(!this.getTraits.exists(_.isInstanceOf[Src1Trait]), s"all traits: ${this.getTraits.collect { case x: Src1Trait => x} }")
    this + Src1Vp
  }

  def s1x: this.type = {
    require(this.allowedTraits.contains(Src1Gp) && !this.getTraits.exists(_.isInstanceOf[Src1Trait]))
    require(!this.getTraits.exists(_.isInstanceOf[Src1Trait]), s"all traits: ${this.getTraits.collect { case x: Src1Trait => x} }")
    this + Src1Gp
  }

  def s1f: this.type = {
    require(this.allowedTraits.contains(Src1Fp) && !this.getTraits.exists(_.isInstanceOf[Src1Trait]))
    this + Src1Fp
  }

  def s1i: this.type = {
    require(this.allowedTraits.contains(Src1Imm) && !this.getTraits.exists(_.isInstanceOf[Src1Trait]))
    this + Src1Imm
  }

  def getTraits: Set[UopTrait] = baseTraits.toSet ++ addTraits.toSet -- delTraits.toSet

  val delTraits = collection.mutable.Set[UopTrait]()

  val addTraits = collection.mutable.Set[UopTrait]()

  def +(t: UopTrait): this.type = {
    addTraits.add(t)
    this
  }

  def -(t: UopTrait): this.type = {
    delTraits.add(t)
    this
  }

  def allowedAndAdded(ts: Seq[UopTrait]): Unit = {
    ts.map(t => t.ensuring(this.allowedTraits.contains(t), s"$t is not in ${this.allowedTraits}"))

    for (t <- ts) {
      this + t
    }
  }
}

case class ScalaUop(baseTrais: UopTrait*) extends UopBase(baseTrais: _*)

abstract class VecUop(baseTraits: UopTrait*) extends UopBase(baseTraits: _*) {
  var maskType: MutableRef[MaskType] = new MutableRef(DestMask)

  var vdAlloc: MutableRef[Boolean] = new MutableRef[Boolean](true)
}

abstract class VecArithUop(baseTraits: UopTrait*) extends VecUop(baseTraits: _*) {
  var src12Rev: Boolean = false

  def setSrc12Rev: this.type = {
    this.src12Rev = true
    this
  }
}

case class VecConfigUop(ts: UopTrait*) extends VecUop {
  override def allowedTraits: Set[UopTrait] = Set(
    GpWen, VlWen,
    Src1Gp, Src2Gp,
    VlRen,
  )

  allowedAndAdded(ts)
}

abstract class VecMemUop(ts: UopTrait*) extends VecUop(ts: _*)

abstract class VecIntUop(baseTraits: UopTrait*) extends VecArithUop(baseTraits: _*) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Vp, Src1Gp, Src1Imm,
  )
  var src1Sgn: MutableRef[SignType] = new MutableRef[SignType](null)
  var src2Sgn: MutableRef[SignType] = new MutableRef[SignType](null)
  var destSgn: MutableRef[SignType] = new MutableRef[SignType](null)
  var sgn:     MutableRef[SignType] = new MutableRef[SignType](null)
}

abstract class VecFpUop(baseTraits: UopTrait*) extends VecArithUop(baseTraits: _*)
class VecLoadUop(ts: UopTrait*) extends VecMemUop(ts: _*)
class VecStoreUop(ts: UopTrait*) extends VecMemUop(ts: _*)

/**
 * if uop need read src1
 */
abstract class SrcType(val operand: Operand.Value)
case object Imm extends SrcType(Operand.IMM)
case object Gp extends SrcType(Operand.GP)
case object Fp extends SrcType(Operand.FP)
case object Vl extends SrcType(null)

abstract class Vec(operand: Operand.Value) extends SrcType(operand)
case object Vp extends Vec(Operand.VP)

/**
 * VpMask
 */
case object Vm extends Vec(Operand.VP)

/**
 * VpScala
 */
case object Vs extends Vec(Operand.VP)

/**
 * VpWiden
 */
case object Vw extends Vec(Operand.VP)

case object Vws extends Vec(Operand.VP)

class VecIntUopOp2(ts: UopTrait*) extends VecIntUop(VlRen +: V0RenAsMask +: ts: _*) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Vp, Src1Gp, Src1Imm,
    Src2Vp, Src2Vw,
    VlRen,
    V0RenAsMask,
    VpWen, VwWen, VmWen,
  )
}

abstract class VecIntUopOp3(ts: UopTrait*)
  extends VecIntUop(VlRen, V0RenAsMask, Src2Vp) {

  override def allowedTraits: Set[UopTrait] = Set(
    Src1Vp, Src1Gp, Src1Imm,
    Src3Vp, Src3Vw,
    VpWen, VwWen,
  )

  allowedAndAdded(ts)
}

case class VecIntUopVV_DV(ts: UopTrait*) extends VecIntUopOp2(Src2Vp, VpWen) {

}
case class VecIntUopVV_DW() extends VecIntUopOp2(Src2Vp, VwWen)
case class VecIntUopWV_DW() extends VecIntUopOp2(Src2Vw, VwWen)
case class VecIntUopWV_DV() extends VecIntUopOp2(Src2Vw, VpWen)
case class VecIntUopVV_DM() extends VecIntUopOp2(Src2Vp, VmWen)
case class VecIntUopVVV_DV() extends VecIntUopOp3(Src3Vp, VpWen)
case class VecIntUopVVW_DW() extends VecIntUopOp3(Src3Vw, VwWen)
case class VecIntFixUopVV_DV(ts: UopTrait*) extends VecIntUopOp2(Src2Vp, VpWen) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Vp, Src1Gp, Src1Imm,
    V0RenAsMask,
    VxsatWen, VxrmRen,
  )

  allowedAndAdded(ts)
}

case class VecIntFixUopWV_DV(ts: UopTrait*) extends VecIntUopOp2(Src2Vw, VpWen) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Vp, Src1Gp, Src1Imm,
    V0RenAsMask,
    VxsatWen, VxrmRen,
  )

  allowedAndAdded(ts)
}

abstract class VecIntCarryUop(ts: UopTrait*) extends VecIntUop(VlRen, V0RenAsSrc, Src2Vp) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Vp, Src1Gp, Src1Imm,
    VpWen, VmWen,
  )

  allowedAndAdded(ts)
}

case class VecIntUopVVM_DV() extends VecIntCarryUop(VpWen)

case class VecIntUopVVM_DM() extends VecIntCarryUop(VmWen)

case class VecIntUopMM_DM() extends VecIntUop(VlRen, V0RenAsMask, VmWen, Src1Vm, Src2Vm) {
  override def allowedTraits: Set[UopTrait] = Set()
}

case class VecIntUopVM_DV() extends VecIntUop(VlRen, VpWen, Src1Vm, Src2Vp) {
  override def allowedTraits: Set[UopTrait] = Set()
}

abstract class VecIntUopS2(ts: UopTrait*) extends VecIntUop(VlRen) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src2Vp, Src2Vm,
    VpWen, VmWen, GpWen,
  )

  allowedAndAdded(ts)
}

case class VecIntUopS1(ts: UopTrait*) extends VecIntUop(VlRen, V0RenAsMask) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Vp, Src1Gp, Src1Imm,
    VpWen, VsWen
  )

  allowedAndAdded(ts)
}

case class VecIntUopS2V_DV() extends VecIntUopS2(Src2Vp, VpWen)
case class VecIntUopS2M_DM() extends VecIntUopS2(Src2Vm, VmWen)
case class VecIntUopS2M_DV() extends VecIntUopS2(Src2Vm, VpWen)
case class VecIntUopS2M_DX() extends VecIntUopS2(Src2Vm, GpWen)
case class VecUopS2A(ts: UopTrait*) extends VecUop(Src2Vs) {
  override def allowedTraits: Set[UopTrait] = Set(
    GpWen, FpWen,
  )
  allowedAndAdded(ts)
}

case class VecUopS1_DA(ts: UopTrait*) extends VecUop(VsWen) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Fp, Src1Gp,
  )

  allowedAndAdded(ts)
}

case class VecIntUop_DV() extends VecIntUop(VlRen, VpWen) {
  override def allowedTraits: Set[UopTrait] = Set()
}

abstract class VecIntRegUop(ts: UopTrait*) extends VecIntUop(VlRen, Src2Vp) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Vs, Src1Vws,
    Src2Vp, Src2Vm,
    VsWen, VwsWen,
  )

  allowedAndAdded(ts)
}

case class VecIntRedUopVA_DA() extends VecIntRegUop(Src1Vs, VsWen)
case class VecIntWRedUopVA_DA() extends VecIntRegUop(Src1Vws, VwsWen)

case class VecFpUopS1(ts: UopTrait*) extends VecFpUop(VlRen) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Fp,
    VpWen, VsWen,
  )

  allowedAndAdded(ts)
}

class VecFpUopS2(ts: UopTrait*) extends VecFpUop(VlRen, V0RenAsMask) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src2Vp, Src2Vw,
    VpWen, VwWen,
  )

  allowedAndAdded(ts)
}

class VecFpUopOp2(ts: UopTrait*) extends VecFpUop(VlRen +: V0RenAsMask +: ts: _*) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Vp, Src1Fp,
    Src2Vp, Src2Vw,
    VpWen, VwWen, VmWen
  )
}

class VecFpUopOp3(ts: UopTrait*) extends VecFpUop(VlRen +: V0RenAsMask +: Src2Vp +: ts: _*) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Vp, Src1Fp,
    Src2Vp, Src2Vw,
    Src3Vp, Src3Vw,
    VpWen, VwWen,
  )
}

case class VecFpUopS2V_DV() extends VecFpUopS2(Src2Vp, VpWen)
case class VecFpUopS2V_DW() extends VecFpUopS2(Src2Vp, VwWen)
case class VecFpUopS2W_DV() extends VecFpUopS2(Src2Vw, VpWen)
case class VecFpUopVV_DV()  extends VecFpUopOp2(Src2Vp, VpWen)
case class VecFpUopVV_DW()  extends VecFpUopOp2(Src2Vp, VwWen)
case class VecFpUopVV_DM()  extends VecFpUopOp2(Src2Vp, VmWen)
case class VecFpUopWV_DW()  extends VecFpUopOp2(Src2Vw, VwWen)
case class VecFpUopVVV_DV() extends VecFpUopOp3(Src3Vp, VpWen)
case class VecFpUopVVW_DW() extends VecFpUopOp3(Src3Vw, VsWen)

class VecFpUopRed(ts: UopTrait*) extends VecFpUop(VlRen, V0RenAsMask, Src2Vp) {
  override def allowedTraits: Set[UopTrait] = Set(
    Src1Vs, Src1Vws,
    VsWen, VwsWen
  )
  allowedAndAdded(ts)
}

case class VecFpRedUopVA_DA()  extends VecFpUopRed(Src1Vs, VsWen)
case class VecFpWRedUopVA_DA() extends VecFpUopRed(Src1Vws, VwsWen)

object Util {

}