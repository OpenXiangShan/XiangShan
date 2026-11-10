package xiangshan.backend.vector.Decoder.Split

import chisel3._
import org.chipsalliance.cde.config.{Parameters => P}
import xiangshan.SrcType
import xiangshan.backend.fu.vector.Bundles.{VLmul, VSew, VType}
import xiangshan.backend.vector.Decoder.Types._
import xiangshan.backend.vector.VecBundle
import xiangshan.backend.vector.VecModule
import xiangshan.backend.vector.util.BString._

class VecUopSplitModule(implicit val p: P) extends VecModule {
  val io = IO(new Bundle {
    val i = Input(new Bundle {
      val instType = new Bundle {
        val vec = Bool()
      }
      val lmul = EnumLMUL()
      val inst = UInt(32.W)
      val splitType = SplitType()
      val vs1 = Lreg()
      val vs2 = Lreg()
      val vd = Lreg()
    })
    val o = Output(new Bundle {
      val uopVec = Vec(maxSplitUopNum, new VecUopSplitUopOutput)
    })
  })

  private val splitType = io.i.splitType
  private val vs1 = io.i.vs1
  private val vs2 = io.i.vs2
  private val vd = io.i.vd
  private val lmul = io.i.lmul

  val src1group = WireInit(VecInit.fill(maxLMUL)(vs1))
  val src2group = WireInit(VecInit.fill(maxLMUL)(vs2))
  val destgroup = WireInit(VecInit.fill(maxLMUL)(vd))

  val unusedSrc2 = vs2
  val unusedSrc1 = vs1
  val unusedDest = vd

  val uopCtlTable = {
    import SplitType._
    val M1 = CtlFieldM1
    val M2 = CtlFieldM2
    val M4 = CtlFieldM4
    val M8 = CtlFieldM8
    val CF = SplitUopCtlField
    Seq(
      NONE -> CF(),
      VVV -> CF(),
      VVM -> CF(
        m2 = M2(vdAlloc = b"      01", uopDep = b"      10"),
        m4 = M4(vdAlloc = b"    0001", uopDep = b"    1110"),
        m8 = M8(vdAlloc = b"00000001", uopDep = b"11111110"),
      ),
      WVV -> CF(
        m2 = M2(vdAlloc = b"      01", uopDep = b"      10"),
        m4 = M4(vdAlloc = b"    0101", uopDep = b"    1010"),
        m8 = M8(vdAlloc = b"01010101", uopDep = b"10101010"),
      ),
      WVW -> CF(),
      VVW -> CF(),
      EXT4 -> CF(),
      EXT8 -> CF(),
      VREDU -> CF(
        m2 = M2(vdAlloc = b"      01", uopDep = b"      10"),
        m4 = M4(vdAlloc = b"    0001", uopDep = b"    1110"),
        m8 = M8(vdAlloc = b"00000001", uopDep = b"11111110"),
      ),
      VREDO -> CF(
        m2 = M2(vdAlloc = b"      01", uopDep = b"      10"),
        m4 = M4(vdAlloc = b"    0001", uopDep = b"    1110"),
        m8 = M8(vdAlloc = b"00000001", uopDep = b"11111110"),
      ),
      VWREDU -> CF(
        m2 = M2(vdAlloc = b"      01", uopDep = b"      10", widenSew = b"      10"),
        m4 = M4(vdAlloc = b"    0001", uopDep = b"    1110", widenSew = b"    1000"),
        m8 = M8(vdAlloc = b"00000001", uopDep = b"11111110", widenSew = b"10000000"),
      )


    )
  }

  io.o := 0.U.asTypeOf(io.o)

  //  val default: Vec[HWTuple3[UInt, UInt, UInt]] = VecInit.fill(maxLMUL)(tuple3hwtuple(unusedSrc2, unusedSrc1, unusedDest))
  //  (src2group, src1group, destgroup) := Mux1HLookUp(splitType, VecInit.fill(maxLMUL)(tuple3hwtuple(unusedSrc2, unusedSrc1, unusedDest)))(table)

//  for (i <- 0 until maxLMUL) {
//    src2group(i) := Mux1HLookUp(splitType, unusedSrc2)(lregIdxTable.map { case (k, v: Seq[(UInt, UInt, UInt)]) => k -> v(i)._1 })
//    src1group(i) := Mux1HLookUp(splitType, unusedSrc1)(lregIdxTable.map { case (k, v: Seq[(UInt, UInt, UInt)]) => k -> v(i)._2 })
//    destgroup(i) := Mux1HLookUp(splitType, unusedDest)(lregIdxTable.map { case (k, v: Seq[(UInt, UInt, UInt)]) => k -> v(i)._3 })
//  }
//
//  io.o.uopVec.zipWithIndex.foreach { case (uop, i) =>
//    uop.src2 := src2group(i)
//    uop.src1 := src1group(i)
//    uop.dest := destgroup(i)
//  }


  abstract class FixedWidthCtlField(width: Int)(
    vdAlloc : UInt,
    uopDep  : UInt,
    widenSew: UInt,
  ) {
    checkWidthMeetLmul

    def checkWidthMeetLmul: Unit = {
      Seq(vdAlloc, uopDep, widenSew)
        .ensuring(_.forall(_.getWidth == width))
    }
  }

  case class CtlFieldM1(
    vdAlloc : UInt = b"1",
    uopDep  : UInt = b"0",
    widenSew: UInt = b"0",
  ) extends FixedWidthCtlField(1)(vdAlloc, uopDep, widenSew)

  case class CtlFieldM2(
    vdAlloc : UInt = b"11",
    uopDep  : UInt = b"00",
    widenSew: UInt = b"00",
  ) extends FixedWidthCtlField(2)(vdAlloc, uopDep, widenSew)

  case class CtlFieldM4(
    vdAlloc : UInt = b"1111",
    uopDep  : UInt = b"0000",
    widenSew: UInt = b"0000",
  ) extends FixedWidthCtlField(4)(vdAlloc, uopDep, widenSew)

  case class CtlFieldM8(
    vdAlloc : UInt = b"11111111",
    uopDep  : UInt = b"00000000",
    widenSew: UInt = b"00000000",
  ) extends FixedWidthCtlField(8)(vdAlloc, uopDep, widenSew)

  case class SplitUopCtlField(
    m1: CtlFieldM1 = CtlFieldM1(),
    m2: CtlFieldM2 = CtlFieldM2(),
    m4: CtlFieldM4 = CtlFieldM4(),
    m8: CtlFieldM8 = CtlFieldM8(),
  )

  abstract class FuOp

  case class NonOp() extends FuOp


  // inst ---(inst compress encode)----[split]-----> uop
  // lmul --/
  // idx  -/
}

trait VecDecodedCommonBundle extends VecBundle {
  //////////////////////// dest ////////////////////////
  val wVr = Bool()
  val wMaskr = Bool()
  val wGr = Bool()
  val wFr = Bool()
  val wVl = Bool()
  val wVxsat = Bool()
  val wFflags = Bool()
  val dest = UInt(vecLregWidth.W)
  //////////////////////// src ////////////////////////
  val srcTypeVec = Vec(maxSrc, SrcType())
  val lsrcVec    = Vec(maxSrc, SrcType())
  // always use vd as source data
  val needSrcVD  = Bool()
  val widenVd   = Bool()
  val widenVs2  = Bool()

  //////////////////////// uop ctl ////////////////////////
  // if need reverse src1 and src2 like vrsub.vx.
  val src1Rev2 = Bool()
  val vtype = VType()
  // eew and emul are used, when there is a EEW of operator different with vtype.sew.
  // E.g.1: for vle8.v, EEW is always 8, while EMUL equals `EEW/SEW` times LMUL.
  // E.g.2: for vrgatherei16.vv, EEW is always 16, which used by VS1.
  // E.g.3: for vwadd.wv, EEW is always DOUBLE of SEW, which used by VS2 and VD.
  // E.g.4: for vzext.vf8, EEW is always 1/8 of SEW, which used by VS2.
  // E.g.5: for vluxei16.v, EEW is always 16, which used by VS1.
  val eew = VSew()
  val emul = VLmul()
  val uopNum = UInt(uopNumWidth.W)
  // can be used to accelerate some vector load/store uops.
  val vstartNZero = Bool()
}

class VecDecodedStg1Bundle(implicit val p: P) extends VecDecodedCommonBundle {
  val splitType = SplitType()
}

class VecDecodedStg2Bundle(implicit val p: P) extends VecDecodedCommonBundle {
  //////////////////////// uop ctl ////////////////////////
  val uopIdx = UInt(uopNumWidth.W)
  val firstUop = Bool()
  val lastUop = Bool()
}

class VecUopSplitUopOutput(implicit val p: P) extends VecBundle {
  val src1 = new SplitSrcInfo
  val src2 = new SplitSrcInfo
  val dest = new SplitSrcInfo

  // set 1, if uop use mask to select source operands
  // set 0, if uop use mask to choose elements of result of uop
  // E.g. vredsum.vs will be split to vadd.vv(src mask) and vredadd uops, vadd.vv(src mask) is different from the one
  // defined in RVV Spec. vadd.vv(src mask) use mask to deside the source elements are from vector register or are 0s.
  val useSrcMask = Bool()

  // vwred will be split to one vwop.vv, serval vwop.wv and one vred.vs, but the EEW of vred.vs is DOUBLE of SEW.
  val vredUseDoubleSew = Bool()
}
