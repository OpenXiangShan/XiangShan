package xiangshan.backend.vector.Decoder.Split

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.DecodePatterns.SewLmulPattern
import xiangshan.backend.vector.Decoder.InstPattern.{VecArithInstPattern, VecInstPattern, VecIntVVVPattern}
import xiangshan.backend.vector.Decoder.RVVDecodeUtil
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.Types.{NoMask, Src12Mask, Src2Mask}
import xiangshan.backend.vector.Decoder.Uop.UopTrait.{VecIntUop, VecUop}
import xiangshan.backend.vector.Decoder.Uop.VecUopDefines._

import scala.collection.immutable.SeqMap
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

object SplitTable {
  def main(args: Array[String]): Unit = {
    val hashMap = collection.mutable.HashMap[String, String]()

    import scala.reflect.runtime.currentMirror
    import scala.reflect.runtime.universe._
    val objectType = typeOf[xiangshan.backend.decode.isa.Instructions.type]
    val methods: Iterable[MethodSymbol] = objectType.decls.collect {
      case m: MethodSymbol if m.returnType =:= typeOf[BitPat] && m.paramLists.isEmpty => m
    }
    val instanceMirror = currentMirror.reflect(xiangshan.backend.decode.isa.Instructions)

    val insts = methods.map { method: MethodSymbol =>
      val methodMirror: MethodMirror = instanceMirror.reflectMethod(method)
      (method.name.toString, methodMirror().asInstanceOf[BitPat])
    }

    for ((name, bp) <- insts) {
      if (table.contains(bp)) {
        println(f"$name%-16s: ${table(bp)}")
      }
    }

//    methods.foreach { method: MethodSymbol =>
//      val methodMirror: MethodMirror = instanceMirror.reflectMethod(method)
//      val bitpat = methodMirror().asInstanceOf[BitPat]
//      val str = bitpat.rawString
//      hashMap(str) = method.name.toString
//    }
//
//    for ((key, value) <- opiDupTable.toSeq.sortBy(_._1.rawString)) {
//      println(f"${hashMap(key.rawString)}%10s: ${value.uopInfoRenameString}")
//    }
  }

  import LmulPattern.{m8, m4, m2, m1, mf2, mf4, mf8}
  import SewPattern.{e8, e16, e32, e64}
  import SewLmulPattern._

  private def SeqEachMap(elems: (Seq[SewLmulPattern], Seq[VecUop])*): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    SeqMap.from(elems.flatMap {
      case (patterns, uops) =>
        patterns.map(_ -> uops)
    })
  }

  private def dup(
    sewFunc: SewPattern.type => SewPattern,
  )(
    uop: => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val sew = sewFunc(SewPattern)

    SeqMap(
      sew ## m8 -> Seq.fill(8)(uop),
      sew ## m4 -> Seq.fill(4)(uop),
      sew ## m2 -> Seq.fill(2)(uop),
      sew ## m1 -> Seq.fill(1)(uop),
    ) ++
      SeqMap(sew.sewValue match {
        case 8 | 16 | 32 => sew ## mf2 -> Seq.fill(1)(uop)
        case _ => sew ## mf2 -> Seq.empty
      }) ++
      SeqMap(sew.sewValue match {
        case 8 | 16  => sew ## mf4 -> Seq.fill(1)(uop)
        case _ => sew ## mf4 -> Seq.empty
      }) ++
      SeqMap(sew.sewValue match {
        case 8 => sew ## mf8 -> Seq.fill(1)(uop)
        case _ => sew ## mf8 -> Seq.empty
      })
  }

  private def dup(
    e8uop : => VecUop,
    e16uop: => VecUop,
    e32uop: => VecUop,
    e64uop: => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val e8SeqMap  = dup(_. e8)( e8uop)
    val e16SeqMap = dup(_.e16)(e16uop)
    val e32SeqMap = dup(_.e32)(e32uop)
    val e64SeqMap = dup(_.e64)(e64uop)

    e8SeqMap ++ e16SeqMap ++ e32SeqMap ++ e64SeqMap
  }

  private def dupM(
    sewFunc: SewPattern.type => SewPattern,
  )(
    uop0: => VecUop,
    uopN: => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val sew = sewFunc(SewPattern)
    // if dupM has more than 1 uop, only the first uop need alloc vd
    SeqMap(
      sew ## m8 -> (uop0 +: Seq.fill(7)(uopN)),
      sew ## m4 -> (uop0 +: Seq.fill(3)(uopN)),
      sew ## m2 -> (uop0 +: Seq.fill(1)(uopN)),
      sew ## m1 -> Seq.fill(1)(uop0),
    ) ++
      SeqMap(sew.sewValue match {
        case 8 | 16 | 32 => sew ## mf2 -> Seq.fill(1)(uop0)
        case _ => sew ## mf2 -> Seq.empty
      }) ++
      SeqMap(sew.sewValue match {
        case 8 | 16  => sew ## mf4 -> Seq.fill(1)(uop0)
        case _ => sew ## mf4 -> Seq.empty
      }) ++
      SeqMap(sew.sewValue match {
        case 8 => sew ## mf8 -> Seq.fill(1)(uop0)
        case _ => sew ## mf8 -> Seq.empty
      })
  }

  private def dupM(
    e8uop : => VecUop,
    e16uop: => VecUop,
    e32uop: => VecUop,
    e64uop: => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val e8SeqMap  = dupM(_. e8)( e8uop, Option( e8uop).map(_.set(_.vdAlloc, false)).orNull)
    val e16SeqMap = dupM(_.e16)(e16uop, Option(e16uop).map(_.set(_.vdAlloc, false)).orNull)
    val e32SeqMap = dupM(_.e32)(e32uop, Option(e32uop).map(_.set(_.vdAlloc, false)).orNull)
    val e64SeqMap = dupM(_.e64)(e64uop, Option(e64uop).map(_.set(_.vdAlloc, false)).orNull)

    e8SeqMap ++ e16SeqMap ++ e32SeqMap ++ e64SeqMap
  }

  private def dupF2W(
    sewFunc: SewPattern.type => SewPattern,
  )(
    uop: => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val sew = sewFunc(SewPattern)
    SeqMap(
      sew ## m8 -> Seq(),
      sew ## m4 -> Seq.fill(8)(uop),
      sew ## m2 -> Seq.fill(4)(uop),
      sew ## m1 -> Seq.fill(2)(uop),
    ) ++
      SeqMap(sew.sewValue match {
        case 8 | 16 | 32 => sew ## mf2 -> Seq.fill(1)(uop)
        case _ => sew ## mf2 -> Seq.empty
      }) ++
      SeqMap(sew.sewValue match {
        case 8 | 16  => sew ## mf4 -> Seq.fill(1)(uop)
        case _ => sew ## mf4 -> Seq.empty
      }) ++
      SeqMap(sew.sewValue match {
        case 8 => sew ## mf8 -> Seq.fill(1)(uop)
        case _ => sew ## mf8 -> Seq.empty
      })
  }

  private def dupF2W(
    e8uop : => VecUop,
    e16uop: => VecUop,
    e32uop: => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val e8SeqMap  = dupF2W(_. e8)( e8uop)
    val e16SeqMap = dupF2W(_.e16)(e16uop)
    val e32SeqMap = dupF2W(_.e32)(e32uop)

    e8SeqMap ++ e16SeqMap ++ e32SeqMap
  }

  private def dupF2N(
    sewFunc: SewPattern.type => SewPattern,
  )(
    uopEven: => VecUop,
    uopOdd: => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val sew = sewFunc(SewPattern)
    SeqMap(
      sew ## m8 -> Seq.empty,
      sew ## m4 -> Seq.tabulate(8)(i => if (i % 2 == 0) uopEven else uopOdd),
      sew ## m2 -> Seq.tabulate(4)(i => if (i % 2 == 0) uopEven else uopOdd),
      sew ## m1 -> Seq.tabulate(2)(i => if (i % 2 == 0) uopEven else uopOdd),
    ) ++
      SeqMap(sew.sewValue match {
        case 8 | 16 | 32 => sew ## mf2 -> Seq.fill(1)(uopEven)
        case _ => sew ## mf2 -> Seq.empty
      }) ++
      SeqMap(sew.sewValue match {
        case 8 | 16  => sew ## mf4 -> Seq.fill(1)(uopEven)
        case _ => sew ## mf4 -> Seq.empty
      }) ++
      SeqMap(sew.sewValue match {
        case 8 => sew ## mf8 -> Seq.fill(1)(uopEven)
        case _ => sew ## mf8 -> Seq.empty
      })
  }

  private def dupF2N(
    e8uop : => VecUop,
    e16uop: => VecUop,
    e32uop: => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val e8SeqMap  = dupF2N(_. e8)( e8uop, Option( e8uop).map(_.set(_.vdAlloc, false)).orNull)
    val e16SeqMap = dupF2N(_.e16)(e16uop, Option(e16uop).map(_.set(_.vdAlloc, false)).orNull)
    val e32SeqMap = dupF2N(_.e32)(e32uop, Option(e32uop).map(_.set(_.vdAlloc, false)).orNull)

    e8SeqMap ++ e16SeqMap ++ e32SeqMap
  }

  private def same(uopSeq: => Seq[VecUop]): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    SeqEachMap(SewLmulPattern.all -> uopSeq)
  }

  private def redu(
    sewFunc: SewPattern.type => SewPattern
  )(
    reduop0: => VecUop,
    reduopLast: => VecUop,
    uop0: => VecUop,
    uopM: => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val sew = sewFunc(SewPattern)


    SeqEachMap(
      Seq(sew ## mf8, sew ## mf4, sew ## mf2, sew ## m1) -> Seq(reduop0),
    ) ++ SeqMap(
      sew ## m2 -> Seq(
        uop0,
        reduopLast,
      ),
      sew ## m4 -> Seq(
        uop0,
        uopM,
        uopM,
        reduopLast,
      ),
      sew ## m8 -> Seq(
        uop0,
        uopM,
        uopM,
        uopM,
        uopM,
        uopM,
        uopM,
        reduopLast,
      ),
    )
  }

  private def redu(
    e8reduop : => VecUop,
    e8uop : => VecUop,
    e16reduop: => VecUop,
    e16uop: => VecUop,
    e32reduop: => VecUop,
    e32uop: => VecUop,
    e64reduop: => VecUop,
    e64uop: => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val e8SeqMap = redu(_.e8)(
      Option(e8reduop).map(_.set(_.maskType, Src2Mask)).orNull,
      Option(e8reduop).map(_.set(_.maskType, NoMask)).orNull,
      Option(e8uop).map(_.set(_.maskType, Src12Mask)).orNull,
      Option(e8uop).map(_.set(_.maskType, Src2Mask)).orNull,
    )
    val e16SeqMap = redu(_.e16)(
      Option(e16reduop).map(_.set(_.maskType, Src2Mask)).orNull,
      Option(e16reduop).map(_.set(_.maskType, NoMask)).orNull,
      Option(e16uop).map(_.set(_.maskType, Src12Mask)).orNull,
      Option(e16uop).map(_.set(_.maskType, Src2Mask)).orNull
    )
    val e32SeqMap = redu(_.e32)(
      Option(e32reduop).map(_.set(_.maskType, Src2Mask)).orNull,
      Option(e32reduop).map(_.set(_.maskType, NoMask)).orNull,
      Option(e32uop).map(_.set(_.maskType, Src12Mask)).orNull,
      Option(e32uop).map(_.set(_.maskType, Src2Mask)).orNull
    )
    val e64SeqMap = redu(_.e64)(
      Option(e64reduop).map(_.set(_.maskType, Src2Mask)).orNull,
      Option(e64reduop).map(_.set(_.maskType, NoMask)).orNull,
      Option(e64uop).map(_.set(_.maskType, Src12Mask)).orNull,
      Option(e64uop).map(_.set(_.maskType, Src2Mask)).orNull
    )

    e8SeqMap ++ e16SeqMap ++ e32SeqMap ++ e64SeqMap
  }

  private def wredu(
    sewFunc: SewPattern.type => SewPattern
  )(
    wadd4uop: => VecUop,
    wreduop : => VecUop,
    adduop  : => VecUop,
    reduop  : => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val sew = sewFunc(SewPattern)

    SeqMap(
      sew ## mf8 -> Seq(wreduop),
      sew ## mf4 -> Seq(wreduop),
      sew ## mf2 -> Seq(wreduop),
      sew ## m1  -> Seq(wreduop),
      sew ## m2  -> Seq(
        wadd4uop.s1v.set(_.maskType, Src12Mask), // v[vtmp0] = v[vs2 + 0] + v[vs2 + 1]
        reduop.set(_.maskType, NoMask),          // v[vd]    = v[vs1][0]  + sum(v[vtmp0])
      ),
      sew ## m4  -> Seq(
        wadd4uop.s1v.set(_.maskType, Src12Mask), // v[vtmp0] = v[vs2 + 0] + v[vs2 + 1]
        wadd4uop.s1v.set(_.maskType, Src12Mask), // v[vtmp1] = v[vs2 + 2] + v[vs2 + 3]
        adduop.s1v.set(_.maskType, NoMask),      // v[vtmp0] = v[vtmp0]   + v[vtmp1]
        reduop.set(_.maskType, NoMask),          // v[vd]    = v[vs1][0]  + sum(v[vtmp0])
      ),
      sew ## m8  -> Seq(
        wadd4uop.s1v.set(_.maskType, Src12Mask), // v[vtmp0] = v[vs2 + 0] + v[vs2 + 1]
        wadd4uop.s1v.set(_.maskType, Src12Mask), // v[vtmp1] = v[vs2 + 2] + v[vs2 + 3]
        wadd4uop.s1v.set(_.maskType, Src12Mask), // v[vtmp2] = v[vs2 + 4] + v[vs2 + 5]
        wadd4uop.s1v.set(_.maskType, Src12Mask), // v[vtmp3] = v[vs2 + 6] + v[vs2 + 7]
        adduop.s1v.set(_.maskType, NoMask),      // v[vtmp0] = v[vtmp0]   + v[vtmp1]
        adduop.s1v.set(_.maskType, NoMask),      // v[vtmp1] = v[vtmp2]   + v[vtmp3]
        adduop.s1v.set(_.maskType, NoMask),      // v[vtmp2] = v[vtmp0]   + v[vtmp1]
        reduop.set(_.maskType, NoMask),          // v[vd]    = v[vs1][0]  + sum(v[vtmp0])
      ),
    )
  }

  private def wredu(
    wadd4e8uop : => VecUop,
    wrede8uop : => VecUop,
    adde16uop: => VecUop,
    rede16uop: => VecUop,
    wadd4e16uop: => VecUop,
    wrede16uop: => VecUop,
    adde32uop: => VecUop,
    rede32uop: => VecUop,
    wadd4e32uop: => VecUop,
    wrede32uop: => VecUop,
    adde64uop: => VecUop,
    rede64uop: => VecUop,
  ): SeqMap[SewLmulPattern, Seq[VecUop]] = {
    val e8SeqMap = wredu(_.e8)(wadd4e8uop, wrede8uop, adde16uop, rede16uop)
    val e16SeqMap = wredu(_.e16)(wadd4e16uop, wrede16uop, adde32uop, rede32uop)
    val e32SeqMap  = wredu(_.e32)(wadd4e32uop, wrede32uop, adde64uop, rede64uop)

    e8SeqMap ++ e16SeqMap ++ e32SeqMap
  }

  val table: SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]] = {

    val opi00Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      VADD_VV  -> dup(vadd_e8.s1v, vadd_e16.s1v, vadd_e32.s1v, vadd_e64.s1v),
      VADD_VX  -> dup(vadd_e8.s1x, vadd_e16.s1x, vadd_e32.s1x, vadd_e64.s1x),
      VADD_VI  -> dup(vadd_e8.s1i, vadd_e16.s1i, vadd_e32.s1i, vadd_e64.s1i),
      // Todo: VANDN
      VSUB_VV  -> dup(vsub_e8.s1v, vsub_e16.s1v, vsub_e32.s1v, vsub_e64.s1v),
      VSUB_VX  -> dup(vsub_e8.s1x, vsub_e16.s1x, vsub_e32.s1x, vsub_e64.s1x),
      VRSUB_VX -> dup(vsub_e8.s1x.setSrc12Rev, vsub_e16.s1x.setSrc12Rev, vsub_e32.s1x.setSrc12Rev, vsub_e64.s1x.setSrc12Rev),
      VRSUB_VI -> dup(vsub_e8.s1i.setSrc12Rev, vsub_e16.s1i.setSrc12Rev, vsub_e32.s1i.setSrc12Rev, vsub_e64.s1i.setSrc12Rev),
      VMINU_VV -> dup(vminu_e8.s1v, vminu_e16.s1v, vminu_e32.s1v, vminu_e64.s1v),
      VMINU_VX -> dup(vminu_e8.s1x, vminu_e16.s1x, vminu_e32.s1x, vminu_e64.s1x),
      VMIN_VV  -> dup(vmin_e8.s1v, vmin_e16.s1v, vmin_e32.s1v, vmin_e64.s1v),
      VMIN_VX  -> dup(vmin_e8.s1x, vmin_e16.s1x, vmin_e32.s1x, vmin_e64.s1x),
      VMAXU_VV -> dup(vmaxu_e8.s1v, vmaxu_e16.s1v, vmaxu_e32.s1v, vmaxu_e64.s1v),
      VMAXU_VX -> dup(vmaxu_e8.s1x, vmaxu_e16.s1x, vmaxu_e32.s1x, vmaxu_e64.s1x),
      VMAX_VV  -> dup(vmax_e8.s1v, vmax_e16.s1v, vmax_e32.s1v, vmax_e64.s1v),
      VMAX_VX  -> dup(vmax_e8.s1x, vmax_e16.s1x, vmax_e32.s1x, vmax_e64.s1x),
      VAND_VV  -> dup(vand_e8.s1v, vand_e16.s1v, vand_e32.s1v, vand_e64.s1v),
      VAND_VX  -> dup(vand_e8.s1x, vand_e16.s1x, vand_e32.s1x, vand_e64.s1x),
      VAND_VI  -> dup(vand_e8.s1i, vand_e16.s1i, vand_e32.s1i, vand_e64.s1i),
      VOR_VV   -> dup(vor_e8.s1v, vor_e16.s1v, vor_e32.s1v, vor_e64.s1v),
      VOR_VX   -> dup(vor_e8.s1x, vor_e16.s1x, vor_e32.s1x, vor_e64.s1x),
      VOR_VI   -> dup(vor_e8.s1i, vor_e16.s1i, vor_e32.s1i, vor_e64.s1i),
      VXOR_VV  -> dup(vxor_e8.s1v, vxor_e16.s1v, vxor_e32.s1v, vxor_e64.s1v),
      VXOR_VX  -> dup(vxor_e8.s1x, vxor_e16.s1x, vxor_e32.s1x, vxor_e64.s1x),
      VXOR_VI  -> dup(vxor_e8.s1i, vxor_e16.s1i, vxor_e32.s1i, vxor_e64.s1i),
      VRGATHER_VV     -> dup(vrgather_v_e8, vrgather_v_e16, vrgather_v_e32, vrgather_v_e64),
      VRGATHER_VX     -> dup(vrgather_x_e8, vrgather_x_e16, vrgather_x_e32, vrgather_x_e64),
      VRGATHER_VI     -> dup(vrgather_i_e8, vrgather_i_e16, vrgather_i_e32, vrgather_i_e64),
      VRGATHEREI16_VV -> dup(vrgatherei16_v_e8, vrgatherei16_v_e16, vrgatherei16_v_e32, vrgatherei16_v_e64),
      VSLIDEUP_VX     -> dup(vslideup_x_e8, vslideup_x_e16, vslideup_x_e32, vslideup_x_e64),
      VSLIDEUP_VI     -> dup(vslideup_i_e8, vslideup_i_e16, vslideup_i_e32, vslideup_i_e64),
      VSLIDEDOWN_VX   -> dup(vslidedown_x_e8, vslidedown_x_e16, vslidedown_x_e32, vslidedown_x_e64),
      VSLIDEDOWN_VI   -> dup(vslidedown_i_e8, vslidedown_i_e16, vslidedown_i_e32, vslidedown_i_e64),
    )

    val opi01Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      VADC_VVM   -> dup(vadc_e8.s1v, vadc_e16.s1v, vadc_e32.s1v, vadc_e64.s1v),
      VADC_VXM   -> dup(vadc_e8.s1x, vadc_e16.s1x, vadc_e32.s1x, vadc_e64.s1x),
      VADC_VIM   -> dup(vadc_e8.s1i, vadc_e16.s1i, vadc_e32.s1i, vadc_e64.s1i),
      VMADC_VV   -> dupM(vmadc_e8.s1v, vmadc_e16.s1v, vmadc_e32.s1v, vmadc_e64.s1v),
      VMADC_VX   -> dupM(vmadc_e8.s1x, vmadc_e16.s1x, vmadc_e32.s1x, vmadc_e64.s1x),
      VMADC_VI   -> dupM(vmadc_e8.s1i, vmadc_e16.s1i, vmadc_e32.s1i, vmadc_e64.s1i),
      VMADC_VVM  -> dupM(vmadc_e8.s1v, vmadc_e16.s1v, vmadc_e32.s1v, vmadc_e64.s1v),
      VMADC_VXM  -> dupM(vmadc_e8.s1x, vmadc_e16.s1x, vmadc_e32.s1x, vmadc_e64.s1x),
      VMADC_VIM  -> dupM(vmadc_e8.s1i, vmadc_e16.s1i, vmadc_e32.s1i, vmadc_e64.s1i),
      VSBC_VVM   -> dup(vsbc_e8.s1v, vsbc_e16.s1v, vsbc_e32.s1v, vsbc_e64.s1v),
      VSBC_VXM   -> dup(vsbc_e8.s1x, vsbc_e16.s1x, vsbc_e32.s1x, vsbc_e64.s1x),
      VMSBC_VV   -> dupM(vmsbc_e8.s1v, vmsbc_e16.s1v, vmsbc_e32.s1v, vmsbc_e64.s1v),
      VMSBC_VX   -> dupM(vmsbc_e8.s1x, vmsbc_e16.s1x, vmsbc_e32.s1x, vmsbc_e64.s1x),
      VMSBC_VVM  -> dupM(vmsbc_e8.s1v, vmsbc_e16.s1v, vmsbc_e32.s1v, vmsbc_e64.s1v),
      VMSBC_VXM  -> dupM(vmsbc_e8.s1x, vmsbc_e16.s1x, vmsbc_e32.s1x, vmsbc_e64.s1x),
      // Todo: vror, vrol
      VMERGE_VVM -> dup(vmerge_e8.s1v, vmerge_e16.s1v, vmerge_e32.s1v, vmerge_e64.s1v),
      VMERGE_VXM -> dup(vmerge_e8.s1x, vmerge_e16.s1x, vmerge_e32.s1x, vmerge_e64.s1x),
      VMERGE_VIM -> dup(vmerge_e8.s1i, vmerge_e16.s1i, vmerge_e32.s1i, vmerge_e64.s1i),
      VMV_V_V    -> dup(vmv_v2v_e8, vmv_v2v_e16, vmv_v2v_e32, vmv_v2v_e64),
      VMV_V_X    -> dup(vmv_x2v_e8, vmv_x2v_e16, vmv_x2v_e32, vmv_x2v_e64),
      VMV_V_I    -> dup(vmv_i2v_e8, vmv_i2v_e16, vmv_i2v_e32, vmv_i2v_e64),
      VMSEQ_VV   -> dupM(vmseq_e8.s1v, vmseq_e16.s1v, vmseq_e32.s1v, vmseq_e64.s1v),
      VMSEQ_VX   -> dupM(vmseq_e8.s1x, vmseq_e16.s1x, vmseq_e32.s1x, vmseq_e64.s1x),
      VMSEQ_VI   -> dupM(vmseq_e8.s1i, vmseq_e16.s1i, vmseq_e32.s1i, vmseq_e64.s1i),
      VMSNE_VV   -> dupM(vmsne_e8.s1v, vmsne_e16.s1v, vmsne_e32.s1v, vmsne_e64.s1v),
      VMSNE_VX   -> dupM(vmsne_e8.s1x, vmsne_e16.s1x, vmsne_e32.s1x, vmsne_e64.s1x),
      VMSNE_VI   -> dupM(vmsne_e8.s1i, vmsne_e16.s1i, vmsne_e32.s1i, vmsne_e64.s1i),
      VMSLTU_VV  -> dupM(vmsltu_e8.s1v, vmsltu_e16.s1v, vmsltu_e32.s1v, vmsltu_e64.s1v),
      VMSLTU_VX  -> dupM(vmsltu_e8.s1x, vmsltu_e16.s1x, vmsltu_e32.s1x, vmsltu_e64.s1x),
      VMSLT_VV   -> dupM(vmslt_e8.s1v, vmslt_e16.s1v, vmslt_e32.s1v, vmslt_e64.s1v),
      VMSLT_VX   -> dupM(vmslt_e8.s1x, vmslt_e16.s1x, vmslt_e32.s1x, vmslt_e64.s1x),
      VMSLEU_VV  -> dupM(vmsleu_e8.s1v, vmsleu_e16.s1v, vmsleu_e32.s1v, vmsleu_e64.s1v),
      VMSLEU_VX  -> dupM(vmsleu_e8.s1x, vmsleu_e16.s1x, vmsleu_e32.s1x, vmsleu_e64.s1x),
      VMSLEU_VI  -> dupM(vmsleu_e8.s1i, vmsleu_e16.s1i, vmsleu_e32.s1i, vmsleu_e64.s1i),
      VMSLE_VV   -> dupM(vmsle_e8.s1v, vmsle_e16.s1v, vmsle_e32.s1v, vmsle_e64.s1v),
      VMSLE_VX   -> dupM(vmsle_e8.s1x, vmsle_e16.s1x, vmsle_e32.s1x, vmsle_e64.s1x),
      VMSLE_VI   -> dupM(vmsle_e8.s1i, vmsle_e16.s1i, vmsle_e32.s1i, vmsle_e64.s1i),
      VMSGTU_VX  -> dupM(vmsgtu_e8.s1x, vmsgtu_e16.s1x, vmsgtu_e32.s1x, vmsgtu_e64.s1x),
      VMSGTU_VI  -> dupM(vmsgtu_e8.s1i, vmsgtu_e16.s1i, vmsgtu_e32.s1i, vmsgtu_e64.s1i),
      VMSGT_VX   -> dupM(vmsgt_e8.s1x, vmsgt_e16.s1x, vmsgt_e32.s1x, vmsgt_e64.s1x),
      VMSGT_VI   -> dupM(vmsgt_e8.s1i, vmsgt_e16.s1i, vmsgt_e32.s1i, vmsgt_e64.s1i),
    )

    val opi10Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      VSADDU_VV  -> dup(vsaddu_e8.s1v, vsaddu_e16.s1v, vsaddu_e32.s1v, vsaddu_e64.s1v),
      VSADDU_VX  -> dup(vsaddu_e8.s1x, vsaddu_e16.s1x, vsaddu_e32.s1x, vsaddu_e64.s1x),
      VSADDU_VI  -> dup(vsaddu_e8.s1i, vsaddu_e16.s1i, vsaddu_e32.s1i, vsaddu_e64.s1i),
      VSADD_VV   -> dup(vsadd_e8.s1v, vsadd_e16.s1v, vsadd_e32.s1v, vsadd_e64.s1v),
      VSADD_VX   -> dup(vsadd_e8.s1x, vsadd_e16.s1x, vsadd_e32.s1x, vsadd_e64.s1x),
      VSADD_VI   -> dup(vsadd_e8.s1i, vsadd_e16.s1i, vsadd_e32.s1i, vsadd_e64.s1i),
      VSSUBU_VV  -> dup(vssubu_e8.s1v, vssubu_e16.s1v, vssubu_e32.s1v, vssubu_e64.s1v),
      VSSUBU_VX  -> dup(vssubu_e8.s1x, vssubu_e16.s1x, vssubu_e32.s1x, vssubu_e64.s1x),
      VSSUB_VV   -> dup(vssub_e8.s1v, vssub_e16.s1v, vssub_e32.s1v, vssub_e64.s1v),
      VSSUB_VX   -> dup(vssub_e8.s1x, vssub_e16.s1x, vssub_e32.s1x, vssub_e64.s1x),
      VSLL_VV    -> dup(vsll_e8.s1v, vsll_e16.s1v, vsll_e32.s1v, vsll_e64.s1v),
      VSLL_VX    -> dup(vsll_e8.s1x, vsll_e16.s1x, vsll_e32.s1x, vsll_e64.s1x),
      VSLL_VI    -> dup(vsll_e8.s1i, vsll_e16.s1i, vsll_e32.s1i, vsll_e64.s1i),
      VSMUL_VV   -> dup(vsmul_e8.s1v, vsmul_e16.s1v, vsmul_e32.s1v, vsmul_e64.s1v),
      VSMUL_VX   -> dup(vsmul_e8.s1x, vsmul_e16.s1x, vsmul_e32.s1x, vsmul_e64.s1x),
      VMV1R_V    -> same(Seq.fill(1)(vmvnr)),
      VMV2R_V    -> same(Seq.fill(2)(vmvnr)),
      VMV4R_V    -> same(Seq.fill(4)(vmvnr)),
      VMV8R_V    -> same(Seq.fill(8)(vmvnr)),
      VSRL_VV    -> dup(vsrl_e8.s1v, vsrl_e16.s1v, vsrl_e32.s1v, vsrl_e64.s1v),
      VSRL_VX    -> dup(vsrl_e8.s1x, vsrl_e16.s1x, vsrl_e32.s1x, vsrl_e64.s1x),
      VSRL_VI    -> dup(vsrl_e8.s1i, vsrl_e16.s1i, vsrl_e32.s1i, vsrl_e64.s1i),
      VSRA_VV    -> dup(vsra_e8.s1v, vsra_e16.s1v, vsra_e32.s1v, vsra_e64.s1v),
      VSRA_VX    -> dup(vsra_e8.s1x, vsra_e16.s1x, vsra_e32.s1x, vsra_e64.s1x),
      VSRA_VI    -> dup(vsra_e8.s1i, vsra_e16.s1i, vsra_e32.s1i, vsra_e64.s1i),
      VSSRL_VV   -> dup(vssrl_e8.s1v, vssrl_e16.s1v, vssrl_e32.s1v, vssrl_e64.s1v),
      VSSRL_VX   -> dup(vssrl_e8.s1x, vssrl_e16.s1x, vssrl_e32.s1x, vssrl_e64.s1x),
      VSSRL_VI   -> dup(vssrl_e8.s1i, vssrl_e16.s1i, vssrl_e32.s1i, vssrl_e64.s1i),
      VSSRA_VV   -> dup(vssra_e8.s1v, vssra_e16.s1v, vssra_e32.s1v, vssra_e64.s1v),
      VSSRA_VX   -> dup(vssra_e8.s1x, vssra_e16.s1x, vssra_e32.s1x, vssra_e64.s1x),
      VSSRA_VI   -> dup(vssra_e8.s1i, vssra_e16.s1i, vssra_e32.s1i, vssra_e64.s1i),
      VNSRL_WV   -> dupF2N(vnsrl_e8.s1v, vnsrl_e16.s1v, vnsrl_e32.s1v),
      VNSRL_WX   -> dupF2N(vnsrl_e8.s1x, vnsrl_e16.s1x, vnsrl_e32.s1x),
      VNSRL_WI   -> dupF2N(vnsrl_e8.s1i, vnsrl_e16.s1i, vnsrl_e32.s1i),
      VNSRA_WV   -> dupF2N(vnsra_e8.s1v, vnsrl_e16.s1v, vnsrl_e32.s1v),
      VNSRA_WX   -> dupF2N(vnsra_e8.s1x, vnsrl_e16.s1x, vnsrl_e32.s1x),
      VNSRA_WI   -> dupF2N(vnsra_e8.s1i, vnsrl_e16.s1i, vnsrl_e32.s1i),
      VNCLIPU_WV -> dupF2N(vnclipu_e8.s1v, vnclipu_e16.s1v, vnclipu_e32.s1v),
      VNCLIPU_WX -> dupF2N(vnclipu_e8.s1x, vnclipu_e16.s1x, vnclipu_e32.s1x),
      VNCLIPU_WI -> dupF2N(vnclipu_e8.s1i, vnclipu_e16.s1i, vnclipu_e32.s1i),
      VNCLIP_WV  -> dupF2N(vnclip_e8.s1v, vnclip_e16.s1v, vnclip_e32.s1v),
      VNCLIP_WX  -> dupF2N(vnclip_e8.s1x, vnclip_e16.s1x, vnclip_e32.s1x),
      VNCLIP_WI  -> dupF2N(vnclip_e8.s1i, vnclip_e16.s1i, vnclip_e32.s1i),
    )

    val opi11Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      VWREDSUMU_VS -> wredu(
        vwadd4u_e8 , vwredsumu_e8 , vadd_e16, vredsum_e16,
        vwadd4u_e16, vwredsumu_e16, vadd_e32, vredsum_e32,
        vwadd4u_e32, vwredsumu_e32, vadd_e64, vredsum_e64,
      ),
      VWREDSUM_VS -> wredu(
        vwadd4_e8 , vwredsum_e8 , vadd_e16, vredsum_e16,
        vwadd4_e16, vwredsum_e16, vadd_e32, vredsum_e32,
        vwadd4_e32, vwredsum_e32, vadd_e64, vredsum_e64,
      )
    )

    val opm00Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      VREDSUM_VS -> redu(
        vredsum_e8 , vredsum_e16 , vredsum_e32 , vredsum_e64 ,
        vadd_e8.s1v, vadd_e16.s1v, vadd_e32.s1v, vadd_e64.s1v,
      ),
      VREDAND_VS -> redu(
        vredand_e8 , vredand_e16 , vredand_e32 , vredand_e64 ,
        vand_e8.s1v, vand_e16.s1v, vand_e32.s1v, vand_e64.s1v,
      ),
      VREDOR_VS -> redu(
        vredor_e8 , vredor_e16 , vredor_e32 , vredor_e64 ,
        vor_e8.s1v, vor_e16.s1v, vor_e32.s1v, vor_e64.s1v,
      ),
      VREDXOR_VS -> redu(
        vredxor_e8 , vredxor_e16 , vredxor_e32 , vredxor_e64 ,
        vxor_e8.s1v, vxor_e16.s1v, vxor_e32.s1v, vxor_e64.s1v,
      ),
      VREDMINU_VS -> redu(
        vredminu_e8 , vredminu_e16 , vredminu_e32 , vredminu_e64 ,
        vminu_e8.s1v, vminu_e16.s1v, vminu_e32.s1v, vminu_e64.s1v,
      ),
      VREDMIN_VS -> redu(
        vredmin_e8 , vredmin_e16 , vredmin_e32 , vredmin_e64 ,
        vmin_e8.s1v, vmin_e16.s1v, vmin_e32.s1v, vmin_e64.s1v,
      ),
      VREDMAXU_VS -> redu(
        vredmaxu_e8 , vredmaxu_e16 , vredmaxu_e32 , vredmaxu_e64 ,
        vmaxu_e8.s1v, vmaxu_e16.s1v, vmaxu_e32.s1v, vmaxu_e64.s1v,
      ),
      VREDMAX_VS -> redu(
        vredmax_e8 , vredmax_e16 , vredmax_e32 , vredmax_e64 ,
        vmax_e8.s1v, vmax_e16.s1v, vmax_e32.s1v, vmax_e64.s1v,
      ),

      VAADDU_VV   -> dup(vaaddu_e8.s1v, vaaddu_e16.s1v, vaaddu_e32.s1v, vaaddu_e64.s1v),
      VAADDU_VX   -> dup(vaaddu_e8.s1x, vaaddu_e16.s1x, vaaddu_e32.s1x, vaaddu_e64.s1x),
      VAADD_VV    -> dup(vaadd_e8.s1v, vaadd_e16.s1v, vaadd_e32.s1v, vaadd_e64.s1v),
      VAADD_VX    -> dup(vaadd_e8.s1x, vaadd_e16.s1x, vaadd_e32.s1x, vaadd_e64.s1x),
      VASUBU_VV   -> dup(vasubu_e8.s1v, vasubu_e16.s1v, vasubu_e32.s1v, vasubu_e64.s1v),
      VASUBU_VX   -> dup(vasubu_e8.s1x, vasubu_e16.s1x, vasubu_e32.s1x, vasubu_e64.s1x),
      VASUB_VV    -> dup(vasub_e8.s1v, vasub_e16.s1v, vasub_e32.s1v, vasub_e64.s1v),
      VASUB_VX    -> dup(vasub_e8.s1x, vasub_e16.s1x, vasub_e32.s1x, vasub_e64.s1x),

      // Todo: vclmul, vclmulh

      VSLIDE1UP_VX    -> dup(vslide1up_x_e8, vslide1up_x_e16, vslide1up_x_e32, vslide1up_x_e64),
      VSLIDE1DOWN_VX  -> dup(vslide1down_x_e8, vslide1down_x_e16, vslide1down_x_e32, vslide1down_x_e64),
    )

    val opm01Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      // VWXUNARY0
      VMV_X_S  -> same(Seq(vmv_vs2x)),
      VCPOP_M  -> same(Seq(vcpop_m)),
      VFIRST_M -> same(Seq(vfirst)),
      // VRXUNARY0
      VMV_S_X -> SeqEachMap(
        mf8All -> Seq(vmv_x2vs),
        mf4All -> Seq(vmv_x2vs),
        mf2All -> Seq(vmv_x2vs),
        m1All  -> Seq(vmv_x2vs),
        m2All  -> (vmv_x2vs +: Seq.fill(1)(vtail)),
        m4All  -> (vmv_x2vs +: Seq.fill(3)(vtail)),
        m8All  -> (vmv_x2vs +: Seq.fill(7)(vtail)),
      ),
      // VXUNARY0
      VZEXT_VF8 -> (
        SeqEachMap(
          Seq(
            e8mf8, e8mf4, e8mf2, e8m1, e8m2, e8m4, e8m8,
            e16mf4, e16mf2, e16m1, e16m2, e16m4, e16m8,
            e32mf2, e32m1, e32m2, e32m4, e32m8,
          ) -> Seq()
        ) ++ SeqMap(
          e64m1 -> Seq.fill(1)(vzext8_e8),
          e64m2 -> Seq.fill(2)(vzext8_e8),
          e64m4 -> Seq.fill(4)(vzext8_e8),
          e64m8 -> Seq.fill(8)(vzext8_e8),
        )
      ),
      VZEXT_VF4 -> (
        SeqEachMap(
          Seq(
            e8mf8, e8mf4, e8mf2, e8m1, e8m2, e8m4, e8m8,
            e16mf4, e16mf2, e16m1, e16m2, e16m4, e16m8,
          ) -> Seq()
        ) ++ SeqMap(
          e32mf2 -> Seq.fill(1)(vzext4_e8),
          e32m1  -> Seq.fill(1)(vzext4_e8),
          e32m2  -> Seq.fill(2)(vzext4_e8),
          e32m4  -> Seq.fill(4)(vzext4_e8),
          e32m8  -> Seq.fill(8)(vzext4_e8),
          e64m1  -> Seq.fill(1)(vzext4_e16),
          e64m2  -> Seq.fill(2)(vzext4_e16),
          e64m4  -> Seq.fill(4)(vzext4_e16),
          e64m8  -> Seq.fill(8)(vzext4_e16),
        )
      ),
      VZEXT_VF2 -> (
        SeqEachMap(
          Seq(
            e8mf8, e8mf4, e8mf2, e8m1, e8m2, e8m4, e8m8,
          ) -> Seq()
        ) ++ SeqMap(
          e16mf4 -> Seq.fill(1)(vzext2_e8),
          e16mf2 -> Seq.fill(1)(vzext2_e8),
          e16m1  -> Seq.fill(1)(vzext2_e8),
          e16m2  -> Seq.fill(2)(vzext2_e8),
          e16m4  -> Seq.fill(4)(vzext2_e8),
          e16m8  -> Seq.fill(8)(vzext2_e8),
          e32mf2 -> Seq.fill(1)(vzext2_e16),
          e32m1  -> Seq.fill(1)(vzext2_e16),
          e32m2  -> Seq.fill(2)(vzext2_e16),
          e32m4  -> Seq.fill(4)(vzext2_e16),
          e32m8  -> Seq.fill(8)(vzext2_e16),
          e64m1  -> Seq.fill(1)(vzext2_e32),
          e64m2  -> Seq.fill(2)(vzext2_e32),
          e64m4  -> Seq.fill(4)(vzext2_e32),
          e64m8  -> Seq.fill(8)(vzext2_e32),
        )
      ),
      VSEXT_VF8 -> (
        SeqEachMap(
          Seq(
            e8mf8, e8mf4, e8mf2, e8m1, e8m2, e8m4, e8m8,
            e16mf4, e16mf2, e16m1, e16m2, e16m4, e16m8,
            e32mf2, e32m1, e32m2, e32m4, e32m8,
          ) -> Seq()
        ) ++ SeqMap(
          e64m1 -> Seq.fill(1)(vsext8_e8),
          e64m2 -> Seq.fill(2)(vsext8_e8),
          e64m4 -> Seq.fill(4)(vsext8_e8),
          e64m8 -> Seq.fill(8)(vsext8_e8),
        )
      ),
      VSEXT_VF4 -> (
        SeqEachMap(
          Seq(
            e8mf8, e8mf4, e8mf2, e8m1, e8m2, e8m4, e8m8,
            e16mf4, e16mf2, e16m1, e16m2, e16m4, e16m8,
          ) -> Seq()
        ) ++ SeqMap(
          e32mf2 -> Seq.fill(1)(vsext4_e8),
          e32m1  -> Seq.fill(1)(vsext4_e8),
          e32m2  -> Seq.fill(2)(vsext4_e8),
          e32m4  -> Seq.fill(4)(vsext4_e8),
          e32m8  -> Seq.fill(8)(vsext4_e8),
          e64m1  -> Seq.fill(1)(vsext4_e16),
          e64m2  -> Seq.fill(2)(vsext4_e16),
          e64m4  -> Seq.fill(4)(vsext4_e16),
          e64m8  -> Seq.fill(8)(vsext4_e16),
        )
      ),
      VSEXT_VF2 -> (
        SeqEachMap(
          Seq(
            e8mf8, e8mf4, e8mf2, e8m1, e8m2, e8m4, e8m8,
          ) -> Seq()
        ) ++ SeqMap(
          e16mf4 -> Seq.fill(1)(vsext2_e8),
          e16mf2 -> Seq.fill(1)(vsext2_e8),
          e16m1  -> Seq.fill(1)(vsext2_e8),
          e16m2  -> Seq.fill(2)(vsext2_e8),
          e16m4  -> Seq.fill(4)(vsext2_e8),
          e16m8  -> Seq.fill(8)(vsext2_e8),
          e32mf2 -> Seq.fill(1)(vsext2_e16),
          e32m1  -> Seq.fill(1)(vsext2_e16),
          e32m2  -> Seq.fill(2)(vsext2_e16),
          e32m4  -> Seq.fill(4)(vsext2_e16),
          e32m8  -> Seq.fill(8)(vsext2_e16),
          e64m1  -> Seq.fill(1)(vsext2_e32),
          e64m2  -> Seq.fill(2)(vsext2_e32),
          e64m4  -> Seq.fill(4)(vsext2_e32),
          e64m8  -> Seq.fill(8)(vsext2_e32),
        )
      ),

      // Todo: vbrev8, vrev8, vbrev, vclz, vctz, vcpop
      VREV8_V     -> dup(vrev8_e8, vrev8_e16, vrev8_e32, vrev8_e64),

      // VMUNARY0
      VMSBF_M     -> same(Seq(vmsbf)),
      VMSOF_M     -> same(Seq(vmsof)),
      VMSIF_M     -> same(Seq(vmsif)),
      VIOTA_M     -> dup(viota_e8, viota_e16, viota_e32, viota_e64),
      VID_V       -> dup(vid_e8, vid_e16, vid_e32, vid_e64),

      VCOMPRESS_VM -> dup(vcompress_e8, vcompress_e16, vcompress_e32, vcompress_e64),

      VMANDN_MM   -> same(Seq(vmandn)),
      VMAND_MM    -> same(Seq(vmand)),
      VMOR_MM     -> same(Seq(vmor)),
      VMXOR_MM    -> same(Seq(vmxor)),
      VMORN_MM    -> same(Seq(vmorn)),
      VMNAND_MM   -> same(Seq(vmnand)),
      VMNOR_MM    -> same(Seq(vmnor)),
      VMXNOR_MM   -> same(Seq(vmxnor)),
    )

    val opm10Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      VDIVU_VV    -> dup(vdivu_e8.s1v, vdivu_e16.s1v, vdivu_e32.s1v, vdivu_e64.s1v),
      VDIVU_VX    -> dup(vdivu_e8.s1x, vdivu_e16.s1x, vdivu_e32.s1x, vdivu_e64.s1x),
      VDIV_VV     -> dup(vdiv_e8.s1v, vdiv_e16.s1v, vdiv_e32.s1v, vdiv_e64.s1v),
      VDIV_VX     -> dup(vdiv_e8.s1x, vdiv_e16.s1x, vdiv_e32.s1x, vdiv_e64.s1x),
      VREMU_VV    -> dup(vremu_e8.s1v, vremu_e16.s1v, vremu_e32.s1v, vremu_e64.s1v),
      VREMU_VX    -> dup(vremu_e8.s1x, vremu_e16.s1x, vremu_e32.s1x, vremu_e64.s1x),
      VREM_VV     -> dup(vrem_e8.s1v, vrem_e16.s1v, vrem_e32.s1v, vrem_e64.s1v),
      VREM_VX     -> dup(vrem_e8.s1x, vrem_e16.s1x, vrem_e32.s1x, vrem_e64.s1x),

      VMULHU_VV   -> dup(vmulhu_e8.s1v, vmulhu_e16.s1v, vmulhu_e32.s1v, vmulhu_e64.s1v),
      VMULHU_VX   -> dup(vmulhu_e8.s1x, vmulhu_e16.s1x, vmulhu_e32.s1x, vmulhu_e64.s1x),
      VMUL_VV     -> dup(vmul_e8.s1v, vmul_e16.s1v, vmul_e32.s1v, vmul_e64.s1v),
      VMUL_VX     -> dup(vmul_e8.s1x, vmul_e16.s1x, vmul_e32.s1x, vmul_e64.s1x),
      VMULHSU_VV  -> dup(vmulhsu_e8.s1v, vmulhsu_e16.s1v, vmulhsu_e32.s1v, vmulhsu_e64.s1v),
      VMULHSU_VX  -> dup(vmulhsu_e8.s1x, vmulhsu_e16.s1x, vmulhsu_e32.s1x, vmulhsu_e64.s1x),
      VMULH_VV    -> dup(vmulh_e8.s1v, vmulh_e16.s1v, vmulh_e32.s1v, vmulh_e64.s1v),
      VMULH_VX    -> dup(vmulh_e8.s1x, vmulh_e16.s1x, vmulh_e32.s1x, vmulh_e64.s1x),

      VMADD_VV    -> dup(vmadd_e8.s1v, vmadd_e16.s1v, vmadd_e32.s1v, vmadd_e64.s1v),
      VMADD_VX    -> dup(vmadd_e8.s1x, vmadd_e16.s1x, vmadd_e32.s1x, vmadd_e64.s1x),
      VNMSUB_VV   -> dup(vnmsub_e8.s1v, vnmsub_e16.s1v, vnmsub_e32.s1v, vnmsub_e64.s1v),
      VNMSUB_VX   -> dup(vnmsub_e8.s1x, vnmsub_e16.s1x, vnmsub_e32.s1x, vnmsub_e64.s1x),
      VMACC_VV    -> dup(vmacc_e8.s1v, vmacc_e16.s1v, vmacc_e32.s1v, vmacc_e64.s1v),
      VMACC_VX    -> dup(vmacc_e8.s1x, vmacc_e16.s1x, vmacc_e32.s1x, vmacc_e64.s1x),
      VNMSAC_VV   -> dup(vnmsac_e8.s1v, vnmsac_e16.s1v, vnmsac_e32.s1v, vnmsac_e64.s1v),
      VNMSAC_VX   -> dup(vnmsac_e8.s1x, vnmsac_e16.s1x, vnmsac_e32.s1x, vnmsac_e64.s1x),
    )

    val opm11Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      VWADDU_VV   -> dupF2W(vwaddu_e8.s1v, vwaddu_e16.s1v, vwaddu_e32.s1v),
      VWADDU_VX   -> dupF2W(vwaddu_e8.s1x, vwaddu_e16.s1x, vwaddu_e32.s1x),
      VWADD_VV    -> dupF2W(vwadd_e8.s1v, vwadd_e16.s1v, vwadd_e32.s1v),
      VWADD_VX    -> dupF2W(vwadd_e8.s1x, vwadd_e16.s1x, vwadd_e32.s1x),
      VWSUBU_VV   -> dupF2W(vwsubu_e8.s1v, vwsubu_e16.s1v, vwsubu_e32.s1v),
      VWSUBU_VX   -> dupF2W(vwsubu_e8.s1x, vwsubu_e16.s1x, vwsubu_e32.s1x),
      VWSUB_VV    -> dupF2W(vwsub_e8.s1v, vwsub_e16.s1v, vwsub_e32.s1v),
      VWSUB_VX    -> dupF2W(vwsub_e8.s1x, vwsub_e16.s1x, vwsub_e32.s1x),
      VWADDU_WV   -> dupF2W(vwaddu_w_e8.s1v, vwaddu_w_e16.s1v, vwaddu_w_e32.s1v),
      VWADDU_WX   -> dupF2W(vwaddu_w_e8.s1x, vwaddu_w_e16.s1x, vwaddu_w_e32.s1x),
      VWADD_WV    -> dupF2W(vwadd_w_e8.s1v, vwadd_w_e16.s1v, vwadd_w_e32.s1v),
      VWADD_WX    -> dupF2W(vwadd_w_e8.s1x, vwadd_w_e16.s1x, vwadd_w_e32.s1x),
      VWSUBU_WV   -> dupF2W(vwsubu_w_e8.s1v, vwsubu_w_e16.s1v, vwsubu_w_e32.s1v),
      VWSUBU_WX   -> dupF2W(vwsubu_w_e8.s1x, vwsubu_w_e16.s1x, vwsubu_w_e32.s1x),
      VWSUB_WV    -> dupF2W(vwsub_w_e8.s1v, vwsub_w_e16.s1v, vwsub_w_e32.s1v),
      VWSUB_WX    -> dupF2W(vwsub_w_e8.s1x, vwsub_w_e16.s1x, vwsub_w_e32.s1x),
      VWMULU_VV   -> dupF2W(vwmulu_e8.s1v, vwmulu_e16.s1v, vwmulu_e32.s1v),
      VWMULU_VX   -> dupF2W(vwmulu_e8.s1x, vwmulu_e16.s1x, vwmulu_e32.s1x),
      VWMULSU_VV  -> dupF2W(vwmulsu_e8.s1v, vwmulsu_e16.s1v, vwmulsu_e32.s1v),
      VWMULSU_VX  -> dupF2W(vwmulsu_e8.s1x, vwmulsu_e16.s1x, vwmulsu_e32.s1x),
      VWMUL_VV    -> dupF2W(vwmul_e8.s1v, vwmul_e16.s1v, vwmul_e32.s1v),
      VWMUL_VX    -> dupF2W(vwmul_e8.s1x, vwmul_e16.s1x, vwmul_e32.s1x),
      VWMACCU_VV  -> dupF2W(vwmaccu_e8.s1v, vwmaccu_e16.s1v, vwmaccu_e32.s1v),
      VWMACCU_VX  -> dupF2W(vwmaccu_e8.s1x, vwmaccu_e16.s1x, vwmaccu_e32.s1x),
      VWMACC_VV   -> dupF2W(vwmacc_e8.s1v, vwmacc_e16.s1v, vwmacc_e32.s1v),
      VWMACC_VX   -> dupF2W(vwmacc_e8.s1x, vwmacc_e16.s1x, vwmacc_e32.s1x),
      VWMACCUS_VX -> dupF2W(vwmaccus_e8.s1x, vwmaccus_e16.s1x, vwmaccus_e32.s1x),
      VWMACCSU_VV -> dupF2W(vwmaccsu_e8.s1v, vwmaccsu_e16.s1v, vwmaccsu_e32.s1v),
      VWMACCSU_VX -> dupF2W(vwmaccsu_e8.s1x, vwmaccsu_e16.s1x, vwmaccsu_e32.s1x),
    )

    val opf00Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      VFADD_VV -> dup(null, vfadd_fp16.s1v, vfadd_fp32.s1v, vfadd_fp64.s1v),
      VFADD_VF -> dup(null, vfadd_fp16.s1f, vfadd_fp32.s1f, vfadd_fp64.s1f),
      VFSUB_VV -> dup(null, vfsub_fp16.s1v, vfsub_fp32.s1v, vfsub_fp64.s1v),
      VFSUB_VF -> dup(null, vfsub_fp16.s1f, vfsub_fp32.s1f, vfsub_fp64.s1f),
      VFMIN_VV -> dup(null, vfmin_fp16.s1v, vfmin_fp32.s1v, vfmin_fp64.s1v),
      VFMIN_VF -> dup(null, vfmin_fp16.s1f, vfmin_fp32.s1f, vfmin_fp64.s1f),
      VFMAX_VV -> dup(null, vfmax_fp16.s1v, vfmax_fp32.s1v, vfmax_fp64.s1v),
      VFMAX_VF -> dup(null, vfmax_fp16.s1f, vfmax_fp32.s1f, vfmax_fp64.s1f),

      VFREDUSUM_VS -> redu(
        null, vfredosum_fp16, vfredosum_fp32, vfredosum_fp64,
        null, vfadd_fp16.s1v, vfadd_fp32.s1v, vfadd_fp64.s1v,
      ),
      VFREDOSUM_VS -> dup(null, vfredosum_fp16, vfredosum_fp32, vfredosum_fp64),
      VFREDMIN_VS -> redu(
        null, vfredmin_fp16, vfredmin_fp32, vfredmin_fp64,
        null, vfmin_fp16.s1v, vfmin_fp32.s1v, vfmin_fp64.s1v,
      ),
      VFREDMAX_VS -> redu(
        null, vfredmax_fp16, vfredmax_fp32, vfredmax_fp64,
        null, vfmax_fp16.s1v, vfmax_fp32.s1v, vfmax_fp64.s1v,
      ),

      VFSGNJ_VV  -> dup(null, vfsgnj_fp16.s1v, vfsgnj_fp32.s1v, vfsgnj_fp64.s1v),
      VFSGNJ_VF  -> dup(null, vfsgnj_fp16.s1f, vfsgnj_fp32.s1f, vfsgnj_fp64.s1f),
      VFSGNJN_VV -> dup(null, vfsgnjn_fp16.s1v, vfsgnjn_fp32.s1v, vfsgnjn_fp64.s1v),
      VFSGNJN_VF -> dup(null, vfsgnjn_fp16.s1f, vfsgnjn_fp32.s1f, vfsgnjn_fp64.s1f),
      VFSGNJX_VV -> dup(null, vfsgnjx_fp16.s1v, vfsgnjx_fp32.s1v, vfsgnjx_fp64.s1v),
      VFSGNJX_VF -> dup(null, vfsgnjx_fp16.s1f, vfsgnjx_fp32.s1f, vfsgnjx_fp64.s1f),

      VFSLIDE1UP_VF    -> dup(null, vslide1up_f_fp16, vslide1up_f_fp32, vslide1up_f_fp64),
      VFSLIDE1DOWN_VF  -> dup(null, vslide1down_f_fp16, vslide1down_f_fp32, vslide1down_f_fp64),
    )

    val opf01Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      // VWFUNARY0
      VFMV_F_S -> same(Seq(vmvVecScala2Fp)),

      // VRFUNARY0
      VFMV_S_F -> SeqEachMap(
        mf8All -> Seq(vmvFp2VecScala),
        mf4All -> Seq(vmvFp2VecScala),
        mf2All -> Seq(vmvFp2VecScala),
        m1All  -> Seq(vmvFp2VecScala),
        m2All  -> (vmvFp2VecScala +: Seq.fill(1)(vtail)),
        m4All  -> (vmvFp2VecScala +: Seq.fill(3)(vtail)),
        m8All  -> (vmvFp2VecScala +: Seq.fill(7)(vtail)),
      ),

      // VFUNARY0
      VFCVT_XU_F_V      -> dup(null, vfcvt_ui16_fp16, vfcvt_ui32_fp32, vfcvt_ui64_fp64),
      VFCVT_X_F_V       -> dup(null, vfcvt_si16_fp16, vfcvt_si32_fp32, vfcvt_si64_fp64),
      VFCVT_F_XU_V      -> dup(null, vfcvt_fp16_ui16, vfcvt_fp32_ui32, vfcvt_fp64_ui64),
      VFCVT_F_X_V       -> dup(null, vfcvt_fp16_si16, vfcvt_fp32_si32, vfcvt_fp64_si64),
      VFCVT_RTZ_XU_F_V  -> dup(null, vfcvt_fp16_ui16, vfcvt_fp32_ui32, vfcvt_fp64_ui64),
      VFCVT_RTZ_X_F_V   -> dup(null, vfcvt_fp16_si16, vfcvt_fp32_si32, vfcvt_fp64_si64),

      VFWCVT_XU_F_V     -> dupF2W(null, vfcvt_ui32_fp16, vfcvt_ui64_fp32),
      VFWCVT_X_F_V      -> dupF2W(null, vfcvt_si32_fp16, vfcvt_si64_fp32),
      VFWCVT_F_XU_V     -> dupF2W(null, vfcvt_fp32_ui16, vfcvt_fp64_ui32),
      VFWCVT_F_X_V      -> dupF2W(null, vfcvt_fp32_si16, vfcvt_fp64_si32),
      VFWCVT_F_F_V      -> dupF2W(null, vfcvt_fp32_fp16, vfcvt_fp64_fp32),
      VFWCVT_RTZ_XU_F_V -> dupF2W(null, vfcvt_ui32_fp16, vfcvt_ui64_fp32),
      VFWCVT_RTZ_X_F_V  -> dupF2W(null, vfcvt_si32_fp16, vfcvt_si64_fp32),

      VFNCVT_XU_F_W     -> dupF2N(vfcvt_ui8_fp16, vfcvt_ui16_fp32, vfcvt_ui32_fp64),
      VFNCVT_X_F_W      -> dupF2N(vfcvt_si8_fp16, vfcvt_si16_fp32, vfcvt_si32_fp64),
      VFNCVT_F_XU_W     -> dupF2N(null, vfcvt_fp16_ui32, vfcvt_fp32_ui64),
      VFNCVT_F_X_W      -> dupF2N(null, vfcvt_fp16_si32, vfcvt_fp32_si64),
      VFNCVT_F_F_W      -> dupF2N(null, vfcvt_fp16_fp32, vfcvt_fp32_fp64),
      VFNCVT_ROD_F_F_W  -> dupF2N(null, vfcvt_fp16_fp32, vfcvt_fp32_fp64),
      VFNCVT_RTZ_XU_F_W -> dupF2N(vfcvt_ui8_fp16, vfcvt_ui16_fp32, vfcvt_ui32_fp64),
      VFNCVT_RTZ_X_F_W  -> dupF2N(vfcvt_si8_fp16, vfcvt_si16_fp32, vfcvt_si32_fp64),

      // VFUNARY1
      VFSQRT_V          -> dup(null, vfsqrt_fp16, vfsqrt_fp32, vfsqrt_fp64),
      VFRSQRT7_V        -> dup(null, vfrsqrt7_fp16, vfrsqrt7_fp32, vfrsqrt7_fp64),
      VFREC7_V          -> dup(null, vfrec7_fp16, vfrec7_fp32, vfrec7_fp64),
      VFCLASS_V         -> dup(null, vfclass_fp16, vfclass_fp32, vfclass_fp64),
      VFMERGE_VFM       -> dup(null, vfmerge_fp16.s1f, vfmerge_fp32.s1f, vfmerge_fp64.s1f),
      VFMV_V_F          -> dup(null, vmv_f2v_fp16, vmv_f2v_fp32, vmv_f2v_fp64),

      VMFEQ_VV          -> dupM(null, vmfeq_fp16.s1v, vmfeq_fp32.s1v, vmfeq_fp64.s1v),
      VMFEQ_VF          -> dupM(null, vmfeq_fp16.s1f, vmfeq_fp32.s1f, vmfeq_fp64.s1f),
      VMFLE_VV          -> dupM(null, vmfle_fp16.s1v, vmfle_fp32.s1v, vmfle_fp64.s1v),
      VMFLE_VF          -> dupM(null, vmfle_fp16.s1f, vmfle_fp32.s1f, vmfle_fp64.s1f),
      VMFLT_VV          -> dupM(null, vmflt_fp16.s1v, vmflt_fp32.s1v, vmflt_fp64.s1v),
      VMFLT_VF          -> dupM(null, vmflt_fp16.s1f, vmflt_fp32.s1f, vmflt_fp64.s1f),
      VMFNE_VV          -> dupM(null, vmfne_fp16.s1v, vmfne_fp32.s1v, vmfne_fp64.s1v),
      VMFNE_VF          -> dupM(null, vmfne_fp16.s1f, vmfne_fp32.s1f, vmfne_fp64.s1f),
      VMFGT_VF          -> dupM(null, vmfgt_fp16.s1f, vmfgt_fp32.s1f, vmfgt_fp64.s1f),
      VMFGE_VF          -> dupM(null, vmfge_fp16.s1f, vmfge_fp32.s1f, vmfge_fp64.s1f),
    )

    val opf10Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      VFDIV_VV          -> dup(null, vfdiv_fp16.s1v, vfdiv_fp32.s1v, vfdiv_fp64.s1v),
      VFDIV_VF          -> dup(null, vfdiv_fp16.s1f, vfdiv_fp32.s1f, vfdiv_fp64.s1f),
      VFRDIV_VF         -> dup(null, vfdiv_fp16.s1f.setSrc12Rev, vfdiv_fp32.s1f.setSrc12Rev, vfdiv_fp64.s1f.setSrc12Rev),
      VFMUL_VV          -> dup(null, vfmul_fp16.s1v, vfmul_fp32.s1v, vfmul_fp64.s1v),
      VFMUL_VF          -> dup(null, vfmul_fp16.s1f, vfmul_fp32.s1f, vfmul_fp64.s1f),

      VFMADD_VV         -> dup(null, vfmadd_fp16.s1v, vfmadd_fp32.s1v, vfmadd_fp64.s1v),
      VFMADD_VF         -> dup(null, vfmadd_fp16.s1f, vfmadd_fp32.s1f, vfmadd_fp64.s1f),
      VFNMADD_VV        -> dup(null, vfnmadd_fp16.s1v, vfnmadd_fp32.s1v, vfnmadd_fp64.s1v),
      VFNMADD_VF        -> dup(null, vfnmadd_fp16.s1f, vfnmadd_fp32.s1f, vfnmadd_fp64.s1f),
      VFMSUB_VV         -> dup(null, vfmsub_fp16.s1v, vfmsub_fp32.s1v, vfmsub_fp64.s1v),
      VFMSUB_VF         -> dup(null, vfmsub_fp16.s1f, vfmsub_fp32.s1f, vfmsub_fp64.s1f),
      VFNMSUB_VV        -> dup(null, vfnmsub_fp16.s1v, vfnmsub_fp32.s1v, vfnmsub_fp64.s1v),
      VFNMSUB_VF        -> dup(null, vfnmsub_fp16.s1f, vfnmsub_fp32.s1f, vfnmsub_fp64.s1f),
      VFMACC_VV         -> dup(null, vfmacc_fp16.s1v, vfmacc_fp32.s1v, vfmacc_fp64.s1v),
      VFMACC_VF         -> dup(null, vfmacc_fp16.s1f, vfmacc_fp32.s1f, vfmacc_fp64.s1f),
      VFNMACC_VV        -> dup(null, vfnmacc_fp16.s1v, vfnmacc_fp32.s1v, vfnmacc_fp64.s1v),
      VFNMACC_VF        -> dup(null, vfnmacc_fp16.s1f, vfnmacc_fp32.s1f, vfnmacc_fp64.s1f),
      VFMSAC_VV         -> dup(null, vfmsac_fp16.s1v, vfmsac_fp32.s1v, vfmsac_fp64.s1v),
      VFMSAC_VF         -> dup(null, vfmsac_fp16.s1f, vfmsac_fp32.s1f, vfmsac_fp64.s1f),
      VFNMSAC_VV        -> dup(null, vfnmsac_fp16.s1v, vfnmsac_fp32.s1v, vfnmsac_fp64.s1v),
      VFNMSAC_VF        -> dup(null, vfnmsac_fp16.s1f, vfnmsac_fp32.s1f, vfnmsac_fp64.s1f),
    )

    val opf11Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      VFWADD_VV         -> dupF2W(null, vfwadd_fp16.s1v, vfwadd_fp32.s1v),
      VFWADD_VF         -> dupF2W(null, vfwadd_fp16.s1f, vfwadd_fp32.s1f),
      VFWSUB_VV         -> dupF2W(null, vfwsub_fp16.s1v, vfwsub_fp32.s1v),
      VFWSUB_VF         -> dupF2W(null, vfwsub_fp16.s1f, vfwsub_fp32.s1f),
      VFWADD_WV         -> dupF2W(null, vfwadd_w_fp16.s1v, vfwadd_w_fp32.s1v),
      VFWADD_WF         -> dupF2W(null, vfwadd_w_fp16.s1f, vfwadd_w_fp32.s1f),
      VFWSUB_WV         -> dupF2W(null, vfwsub_w_fp16.s1v, vfwsub_w_fp32.s1v),
      VFWSUB_WF         -> dupF2W(null, vfwsub_w_fp16.s1f, vfwsub_w_fp32.s1f),
      VFWMUL_VV         -> dupF2W(null, vfwmul_fp16.s1v, vfwmul_fp32.s1v),
      VFWMUL_VF         -> dupF2W(null, vfwmul_fp16.s1f, vfwmul_fp32.s1f),
      VFWMACC_VV        -> dupF2W(null, vfwmacc_fp16.s1v, vfwmacc_fp32.s1v),
      VFWMACC_VF        -> dupF2W(null, vfwmacc_fp16.s1f, vfwmacc_fp32.s1f),
      VFWNMACC_VV       -> dupF2W(null, vfwnmacc_fp16.s1v, vfwnmacc_fp32.s1v),
      VFWNMACC_VF       -> dupF2W(null, vfwnmacc_fp16.s1f, vfwnmacc_fp32.s1f),
      VFWMSAC_VV        -> dupF2W(null, vfwmsac_fp16.s1v, vfwmsac_fp32.s1v),
      VFWMSAC_VF        -> dupF2W(null, vfwmsac_fp16.s1f, vfwmsac_fp32.s1f),
      VFWNMSAC_VV       -> dupF2W(null, vfwnmsac_fp16.s1v, vfwnmsac_fp32.s1v),
      VFWNMSAC_VF       -> dupF2W(null, vfwnmsac_fp16.s1f, vfwnmsac_fp32.s1f),
    )

    val cryptoTable = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[VecUop]]](
      VSHA2MS_VV -> dup(null, null, vsha256ms_e32, null),
      VSHA2CL_VV -> dup(null, null, vsha256cl_e32, null),
      VSHA2CH_VV -> dup(null, null, vsha256ch_e32, null),
    )

    opi00Table ++ opi01Table ++ opi10Table ++ opi11Table ++
    opm00Table ++ opm01Table ++ opm10Table ++ opm11Table ++
    opf00Table ++ opf01Table ++ opf10Table ++ opf11Table ++ cryptoTable
  }
}
