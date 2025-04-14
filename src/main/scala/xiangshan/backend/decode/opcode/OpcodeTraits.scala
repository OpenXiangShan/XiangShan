package xiangshan.backend.decode.opcode

import xiangshan.backend.vector.Decoder.Types.{DecodeSelImm, Operand}
import yunsuan.encoding.Opcode.OpcodeTraits.{OpcodeTrait => Trait}

object OpcodeTraits {
  trait WenTrait extends Trait
  trait VecWenTrait extends Trait

  case object GpWen extends WenTrait
  case object VlWen extends WenTrait
  case object FpWen extends WenTrait
  case object VpWen extends WenTrait with VecWenTrait
  case object VsWen extends WenTrait with VecWenTrait
  case object VwWen extends WenTrait with VecWenTrait
  case object VmWen extends WenTrait with VecWenTrait
  case object VwsWen extends WenTrait with VecWenTrait

  case object VxsatWen extends Trait
  case object FFlagsWen extends Trait

  abstract class Src1Trait(val srcType: SrcType) extends Trait

  case object Src1En  extends Trait
  case object Src1Gp  extends Src1Trait(Gp)
  case object Src1Fp  extends Src1Trait(Fp)
  case object Src1Vp  extends Src1Trait(Vp)
  case object Src1Vm  extends Src1Trait(Vm)
  case object Src1Vw  extends Src1Trait(Vw)
  case object Src1Vs  extends Src1Trait(Vs)
  case object Src1Vws extends Src1Trait(Vws)
  case object Src1Imm extends Src1Trait(Imm)

  abstract class Src2Trait(val srcType: SrcType) extends Trait

  case object Src2En extends Trait
  case object Src2Gp extends Src2Trait(Gp)
  case object Src2Fp extends Src2Trait(Fp)
  case object Src2Vp extends Src2Trait(Vp)
  case object Src2Vw extends Src2Trait(Vw)
  case object Src2Vs extends Src2Trait(Vs)
  case object Src2Vm extends Src2Trait(Vm)
  case class Src2Imm(selImm: DecodeSelImm.Type) extends Src2Trait(Imm)

  abstract class Src3Trait(val srcType: SrcType) extends Trait
  case object Src3Gp extends Src3Trait(Gp)
  case object Src3Fp extends Src3Trait(Fp)
  case object Src3Vp extends Src3Trait(Vp)
  case object Src3Vw extends Src3Trait(Vw)
  case object Src3Vs extends Src3Trait(Vs)
  case object Src3Vm extends Src3Trait(Vm)

  trait V0Trait extends Trait
  case object V0RenAsMask extends V0Trait
  case object V0RenAsSrc extends V0Trait

  case object VlRen extends Trait

  trait CtrlRen extends Trait
  case object VxrmRen extends CtrlRen

  case object Order extends Trait

  case object NoSpec extends Trait

  case object BlockBack extends Trait

  case object FlushPipe extends Trait

  case object CannotRobCompress extends Trait

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

  case object NeedVecEnable extends Trait

}
