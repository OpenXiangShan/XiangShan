package xiangshan.backend.vector.Decoder.Split

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.decode.opcode.Opcode.{Opcode, VMoveOpcodes, _}
import xiangshan.backend.vector.Decoder.DecodePatterns.SewLmulPattern
import xiangshan.backend.vector.Decoder.InstPattern.{VecArithInstPattern, VecInstPattern, VecIntVVVPattern}
import xiangshan.backend.vector.Decoder.RVVDecodeUtil
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.decode.opcode.Opcode.VIAluOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VMAluOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VIMacOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VIDivOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VIRedOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VIPermOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VMoveOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VSha256msOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VSha256cOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VFCvtOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VFMacOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VFDivOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VFMiscOpcodes._
import xiangshan.backend.decode.opcode.Opcode.VFRedOpcodes._
import xiangshan.backend.decode.opcode.OpcodeTraits._
import xiangshan.backend.vector.Decoder.Types.DecodeSelImm

import scala.collection.immutable.SeqMap
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
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

  private def SeqEachMap(elems: (Seq[SewLmulPattern], Seq[Opcode])*): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    SeqMap.from(elems.flatMap {
      case (patterns, uops) =>
        patterns.map(_ -> uops)
    })
  }

  private def dup(
    sewFunc: SewPattern.type => SewPattern,
  )(
    _uop: => Opcode,
  )(
    func: Opcode => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val sew: SewPattern = sewFunc(SewPattern)

    val uop = Option(_uop).map(_.copy()).map(func).orNull

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
    e8uop : => Opcode,
    e16uop: => Opcode,
    e32uop: => Opcode,
    e64uop: => Opcode,
  )(
    func: Opcode => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val e8SeqMap  = dup(_. e8)( e8uop)(func)
    val e16SeqMap = dup(_.e16)(e16uop)(func)
    val e32SeqMap = dup(_.e32)(e32uop)(func)
    val e64SeqMap = dup(_.e64)(e64uop)(func)

    e8SeqMap ++ e16SeqMap ++ e32SeqMap ++ e64SeqMap
  }

  private def dupM(
    sewFunc: SewPattern.type => SewPattern,
  )(
    uop: => Opcode,
  )(
    func: Opcode => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val sew = sewFunc(SewPattern)
    // if dupM has more than 1 uop, only the first uop need alloc vd
    val uop0: Opcode = Option(uop).map(_.copy()).map(func).map(_ + VmWen).orNull
    val uopN: Opcode = Option(uop).map(_.copy()).map(func).map(_ + VmWen + NoDestAlloc).orNull

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
      SeqMap(sew match {
        case SewPattern.e8 => sew ## mf8 -> Seq.fill(1)(uop0)
        case _ => sew ## mf8 -> Seq.empty
      })
  }

  private def dupM(
    e8uop : => Opcode,
    e16uop: => Opcode,
    e32uop: => Opcode,
    e64uop: => Opcode,
  )(
    func: Opcode => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val e8SeqMap  = dupM(_. e8)( e8uop)(func)
    val e16SeqMap = dupM(_.e16)(e16uop)(func)
    val e32SeqMap = dupM(_.e32)(e32uop)(func)
    val e64SeqMap = dupM(_.e64)(e64uop)(func)

    e8SeqMap ++ e16SeqMap ++ e32SeqMap ++ e64SeqMap
  }

  private def dupF2W(
    sewFunc: SewPattern.type => SewPattern,
  )(
    _uop: => Opcode,
  )(
    func: Opcode => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val uop = Option(_uop).map(_.copy()).map(func).orNull

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
    e8uop : => Opcode,
    e16uop: => Opcode,
    e32uop: => Opcode,
  )(
    func: Opcode => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val e8SeqMap  = dupF2W(_. e8)( e8uop)(func)
    val e16SeqMap = dupF2W(_.e16)(e16uop)(func)
    val e32SeqMap = dupF2W(_.e32)(e32uop)(func)

    e8SeqMap ++ e16SeqMap ++ e32SeqMap
  }

  private def dupF2N(
    sewFunc: SewPattern.type => SewPattern,
  )(
    uop: => Opcode,
  )(
    func: Opcode => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val uopEven = Option(uop).map(_.copy()).map(func).map(_ + NoDestAlloc).orNull
    val uopOdd = Option(uop).map(_.copy()).map(func).orNull

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
    e8uop : => Opcode,
    e16uop: => Opcode,
    e32uop: => Opcode,
  )(
    func: Opcode => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val e8SeqMap  = dupF2N(_. e8)( e8uop)(func)
    val e16SeqMap = dupF2N(_.e16)(e16uop)(func)
    val e32SeqMap = dupF2N(_.e32)(e32uop)(func)

    e8SeqMap ++ e16SeqMap ++ e32SeqMap
  }

  private def same(
    uops: => Seq[Opcode]
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    SeqEachMap(SewLmulPattern.all -> uops)
  }

  private def sewSame(
    _e8uop : => Opcode,
    _e16uop: => Opcode,
    _e32uop: => Opcode,
    _e64uop: => Opcode,
  )(
    func: Opcode => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    sewSame(
      Option(_e8uop).toSeq,
      Option(_e16uop).toSeq,
      Option(_e32uop).toSeq,
      Option(_e64uop).toSeq,
    )(func)
  }

  private def sewSame(
    _e8uop : => Seq[Opcode],
    _e16uop: => Seq[Opcode],
    _e32uop: => Seq[Opcode],
    _e64uop: => Seq[Opcode],
  )(
    func: Opcode => Opcode,
  )(implicit dummy: DummyImplicit): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val e8uop  = _e8uop .map(_.copy()).map(func)
    val e16uop = _e16uop.map(_.copy()).map(func)
    val e32uop = _e32uop.map(_.copy()).map(func)
    val e64uop = _e64uop.map(_.copy()).map(func)

    SeqEachMap(SewLmulPattern.e8All -> e8uop) ++
      SeqEachMap(SewLmulPattern.e16All -> e16uop) ++
      SeqEachMap(SewLmulPattern.e32All -> e32uop) ++
      SeqEachMap(SewLmulPattern.e64All -> e64uop)
  }

  private def redu(
    sewFunc: SewPattern.type => SewPattern
  )(
    _reduop: => Opcode,
    _uop: => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val sew = sewFunc(SewPattern)

    val reduop0 = Option(_reduop).map(_.copy() + Src2Mask).orNull
    val reduopLast = Option(_reduop).map(_.copy() + NoMask).orNull
    val uop0 = Option(_uop).map(_.copy() + Src12Mask).orNull
    val uopM = Option(_uop).map(_.copy() + Src2Mask).orNull

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
    e8reduop : => Opcode,
    e8uop : => Opcode,
    e16reduop: => Opcode,
    e16uop: => Opcode,
    e32reduop: => Opcode,
    e32uop: => Opcode,
    e64reduop: => Opcode,
    e64uop: => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val e8SeqMap = redu(_.e8)(
      e8reduop,
      e8uop,
    )
    val e16SeqMap = redu(_.e16)(
      e16reduop,
      e16uop,
    )
    val e32SeqMap = redu(_.e32)(
      e32reduop,
      e32uop,
    )
    val e64SeqMap = redu(_.e64)(
      e64reduop,
      e64uop,
    )

    e8SeqMap ++ e16SeqMap ++ e32SeqMap ++ e64SeqMap
  }

  private def wredu(
    sewFunc: SewPattern.type => SewPattern
  )(
    _wadd4uop: => Opcode,
    _wreduop : => Opcode,
    _adduop  : => Opcode,
    _reduop  : => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val sew = sewFunc(SewPattern)

    val wadd4uop: Opcode = _wadd4uop.copy().S1v + Src12Mask
    val wreduop : Opcode = _wreduop.copy()     + Src2Mask
    val adduop  : Opcode = _adduop.copy().S1v   + NoMask
    val reduop  : Opcode = _reduop.copy()      + NoMask

    SeqMap(
      sew ## mf8 -> Seq(wreduop),
      sew ## mf4 -> Seq(wreduop),
      sew ## mf2 -> Seq(wreduop),
      sew ## m1  -> Seq(wreduop),
      sew ## m2  -> Seq(
        wadd4uop, // v[vtmp0] = v[vs2 + 0] + v[vs2 + 1]
        reduop,   // v[vd]    = v[vs1][0]  + sum(v[vtmp0])
      ),
      sew ## m4  -> Seq(
        wadd4uop, // v[vtmp0] = v[vs2 + 0] + v[vs2 + 1]
        wadd4uop, // v[vtmp1] = v[vs2 + 2] + v[vs2 + 3]
        adduop,   // v[vtmp0] = v[vtmp0]   + v[vtmp1]
        reduop,   // v[vd]    = v[vs1][0]  + sum(v[vtmp0])
      ),
      sew ## m8  -> Seq(
        wadd4uop, // v[vtmp0] = v[vs2 + 0] + v[vs2 + 1]
        wadd4uop, // v[vtmp0] = v[vs2 + 2] + v[vs2 + 3]
        wadd4uop, // v[vtmp0] = v[vs2 + 4] + v[vs2 + 5]
        wadd4uop, // v[vtmp1] = v[vs2 + 6] + v[vs2 + 7]
        adduop,   // v[vtmp0] = v[vtmp0]   + v[vtmp1]
        adduop,   // v[vtmp2] = v[vtmp2]   + v[vtmp3]
        adduop,   // v[vtmp0] = v[vtmp0]   + v[vtmp1]
        reduop,   // v[vd]    = v[vs1][0]  + sum(v[vtmp0])
      ),
    )
  }

  private def wredu(
    wadd4e8uop : => Opcode,
    wrede8uop : => Opcode,
    adde16uop: => Opcode,
    rede16uop: => Opcode,
    wadd4e16uop: => Opcode,
    wrede16uop: => Opcode,
    adde32uop: => Opcode,
    rede32uop: => Opcode,
    wadd4e32uop: => Opcode,
    wrede32uop: => Opcode,
    adde64uop: => Opcode,
    rede64uop: => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val e8SeqMap = wredu(_.e8)(wadd4e8uop, wrede8uop, adde16uop, rede16uop)
    val e16SeqMap = wredu(_.e16)(wadd4e16uop, wrede16uop, adde32uop, rede32uop)
    val e32SeqMap  = wredu(_.e32)(wadd4e32uop, wrede32uop, adde64uop, rede64uop)

    e8SeqMap ++ e16SeqMap ++ e32SeqMap
  }

  private def fwredu(
    sewFunc: SewPattern.type => SewPattern
  )(
    _uop0: => Opcode,
    _uopM: => Opcode,
    _reduop: => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val sew = sewFunc(SewPattern)

    val uop0 = Option(_uop0).map(_.copy().S1v + Src12Mask).orNull
    val uopM = Option(_uopM).map(_.copy().S1v + Src2Mask).orNull
    val reduopMasked = Option(_reduop).map(_.copy() + Src2Mask).orNull
    val reduopLast = Option(_reduop).map(_.copy() + NoMask).orNull

    SeqMap(
      sew ## m8 -> Seq.empty,
      sew ## m4 -> (Seq(uop0) ++ Seq.fill(6)(uopM) :+ reduopLast),
      sew ## m2 -> (Seq(uop0) ++ Seq.fill(2)(uopM) :+ reduopLast),
      sew ## m1 -> Seq(uop0, reduopLast),
    ) ++
      SeqMap(sew.sewValue match {
        case 16 | 32 => sew ## mf2 -> Seq(reduopMasked)
        case _ => sew ## mf2 -> Seq.empty
      }) ++
      SeqMap(sew.sewValue match {
        case 16 => sew ## mf4 -> Seq(reduopMasked)
        case _ => sew ## mf4 -> Seq.empty
      })
  }

  private def fwredosum(
    sewFunc: SewPattern.type => SewPattern
  )(
    _reduop: => Opcode,
  ): SeqMap[SewLmulPattern, Seq[Opcode]] = {
    val sew = sewFunc(SewPattern)
    val reduop = Option(_reduop).map(_.copy() + Src2Mask).orNull

    SeqMap(
      sew ## m8 -> Seq.empty,
      sew ## m4 -> Seq.fill(8)(reduop),
      sew ## m2 -> Seq.fill(4)(reduop),
      sew ## m1 -> Seq.fill(2)(reduop),
    ) ++
      SeqMap(sew.sewValue match {
        case 16 | 32 => sew ## mf2 -> Seq(reduop)
        case _ => sew ## mf2 -> Seq.empty
      }) ++
      SeqMap(sew.sewValue match {
        case 16 => sew ## mf4 -> Seq(reduop)
        case _ => sew ## mf4 -> Seq.empty
      })
  }

  val table: SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]] = {

    val opi00Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      VADD_VV  -> dup(vadd_e8, vadd_e16, vadd_e32, vadd_e64)(_.S1v),
      VADD_VX  -> dup(vadd_e8, vadd_e16, vadd_e32, vadd_e64)(_.S2x),
      VADD_VI  -> dup(vadd_e8, vadd_e16, vadd_e32, vadd_e64)(_.vsi),
      // Todo: VANDN
      VSUB_VV  -> dup(vsub_e8, vsub_e16, vsub_e32, vsub_e64)(_.S1v),
      VSUB_VX  -> dup(vsub_e8, vsub_e16, vsub_e32, vsub_e64)(_.S2x),
      VRSUB_VX -> dup(vsub_e8, vsub_e16, vsub_e32, vsub_e64)(_.S2x.rev),
      VRSUB_VI -> dup(vsub_e8, vsub_e16, vsub_e32, vsub_e64)(_.vsi.rev),
      VMINU_VV -> dup(vminu_e8, vminu_e16, vminu_e32, vminu_e64)(_.S1v),
      VMINU_VX -> dup(vminu_e8, vminu_e16, vminu_e32, vminu_e64)(_.S2x),
      VMIN_VV  -> dup(vmin_e8, vmin_e16, vmin_e32, vmin_e64)(_.S1v),
      VMIN_VX  -> dup(vmin_e8, vmin_e16, vmin_e32, vmin_e64)(_.S2x),
      VMAXU_VV -> dup(vmaxu_e8, vmaxu_e16, vmaxu_e32, vmaxu_e64)(_.S1v),
      VMAXU_VX -> dup(vmaxu_e8, vmaxu_e16, vmaxu_e32, vmaxu_e64)(_.S2x),
      VMAX_VV  -> dup(vmax_e8, vmax_e16, vmax_e32, vmax_e64)(_.S1v),
      VMAX_VX  -> dup(vmax_e8, vmax_e16, vmax_e32, vmax_e64)(_.S2x),
      VAND_VV  -> dup(vand_e8, vand_e16, vand_e32, vand_e64)(_.S1v),
      VAND_VX  -> dup(vand_e8, vand_e16, vand_e32, vand_e64)(_.S2x),
      VAND_VI  -> dup(vand_e8, vand_e16, vand_e32, vand_e64)(_.vui),
      VOR_VV   -> dup(vor_e8, vor_e16, vor_e32, vor_e64)(_.S1v),
      VOR_VX   -> dup(vor_e8, vor_e16, vor_e32, vor_e64)(_.S2x),
      VOR_VI   -> dup(vor_e8, vor_e16, vor_e32, vor_e64)(_.vui),
      VXOR_VV  -> dup(vxor_e8, vxor_e16, vxor_e32, vxor_e64)(_.S1v),
      VXOR_VX  -> dup(vxor_e8, vxor_e16, vxor_e32, vxor_e64)(_.S2x),
      VXOR_VI  -> dup(vxor_e8, vxor_e16, vxor_e32, vxor_e64)(_.vui),
      VRGATHER_VV     -> dup(vrgather_v_e8, vrgather_v_e16, vrgather_v_e32, vrgather_v_e64)(_.S1v),
      VRGATHER_VX     -> dup(vrgather_x_e8, vrgather_x_e16, vrgather_x_e32, vrgather_x_e64)(_.S2x),
      VRGATHER_VI     -> dup(vrgather_i_e8, vrgather_i_e16, vrgather_i_e32, vrgather_i_e64)(_.vui),
      VRGATHEREI16_VV -> dup(vrgather_ei16_e8, vrgather_ei16_e16, vrgather_ei16_e32, vrgather_ei16_e64)(_.S1v),
      VSLIDEUP_VX     -> dup(vslideup_e8, vslideup_e16, vslideup_e32, vslideup_e64)(_.S2x),
      VSLIDEUP_VI     -> dup(vslideup_e8, vslideup_e16, vslideup_e32, vslideup_e64)(_.vui),
      VSLIDEDOWN_VX   -> dup(vslidedown_e8, vslidedown_e16, vslidedown_e32, vslidedown_e64)(_.S2x),
      VSLIDEDOWN_VI   -> dup(vslidedown_e8, vslidedown_e16, vslidedown_e32, vslidedown_e64)(_.vui),
    )

    val opi01Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      VADC_VVM   -> dup(vadc_e8, vadc_e16, vadc_e32, vadc_e64)(_.S1v),
      VADC_VXM   -> dup(vadc_e8, vadc_e16, vadc_e32, vadc_e64)(_.S2x),
      VADC_VIM   -> dup(vadc_e8, vadc_e16, vadc_e32, vadc_e64)(_.vui),
      VMADC_VV   -> dupM(vmadc_e8, vmadc_e16, vmadc_e32, vmadc_e64)(_.S1v),
      VMADC_VX   -> dupM(vmadc_e8, vmadc_e16, vmadc_e32, vmadc_e64)(_.S2x),
      VMADC_VI   -> dupM(vmadc_e8, vmadc_e16, vmadc_e32, vmadc_e64)(_.vsi),
      VMADC_VVM  -> dupM(vmadc_e8, vmadc_e16, vmadc_e32, vmadc_e64)(_.S1v),
      VMADC_VXM  -> dupM(vmadc_e8, vmadc_e16, vmadc_e32, vmadc_e64)(_.S2x),
      VMADC_VIM  -> dupM(vmadc_e8, vmadc_e16, vmadc_e32, vmadc_e64)(_.vsi),
      VSBC_VVM   -> dup(vsbc_e8, vsbc_e16, vsbc_e32, vsbc_e64)(_.S1v),
      VSBC_VXM   -> dup(vsbc_e8, vsbc_e16, vsbc_e32, vsbc_e64)(_.S2x),
      VMSBC_VV   -> dupM(vmsbc_e8, vmsbc_e16, vmsbc_e32, vmsbc_e64)(_.S1v),
      VMSBC_VX   -> dupM(vmsbc_e8, vmsbc_e16, vmsbc_e32, vmsbc_e64)(_.S2x),
      VMSBC_VVM  -> dupM(vmsbc_e8, vmsbc_e16, vmsbc_e32, vmsbc_e64)(_.S1v),
      VMSBC_VXM  -> dupM(vmsbc_e8, vmsbc_e16, vmsbc_e32, vmsbc_e64)(_.S2x),
      // Todo: vror, vrol
      VMERGE_VVM -> dup(vmerge_vv_e8, vmerge_vv_e16, vmerge_vv_e32, vmerge_vv_e64)(_.S1v),
      VMERGE_VXM -> dup(vmerge_vx_e8, vmerge_vx_e16, vmerge_vx_e32, vmerge_vx_e64)(_.S2x),
      VMERGE_VIM -> dup(vmerge_vx_e8, vmerge_vx_e16, vmerge_vx_e32, vmerge_vx_e64)(_.vsi),
      VMV_V_V    -> dup(vmv_v2v_e8, vmv_v2v_e16, vmv_v2v_e32, vmv_v2v_e64)(_.S1v),
      VMV_V_X    -> dup(vmv_x2v_e8, vmv_x2v_e16, vmv_x2v_e32, vmv_x2v_e64)(_.S2x),
      VMV_V_I    -> dup(vmv_x2v_e8, vmv_x2v_e16, vmv_x2v_e32, vmv_x2v_e64)(_.vsi),
      VMSEQ_VV   -> dupM(vmseq_e8, vmseq_e16, vmseq_e32, vmseq_e64)(_.S1v),
      VMSEQ_VX   -> dupM(vmseq_e8, vmseq_e16, vmseq_e32, vmseq_e64)(_.S2x),
      VMSEQ_VI   -> dupM(vmseq_e8, vmseq_e16, vmseq_e32, vmseq_e64)(_.vsi),
      VMSNE_VV   -> dupM(vmsne_e8, vmsne_e16, vmsne_e32, vmsne_e64)(_.S1v),
      VMSNE_VX   -> dupM(vmsne_e8, vmsne_e16, vmsne_e32, vmsne_e64)(_.S2x),
      VMSNE_VI   -> dupM(vmsne_e8, vmsne_e16, vmsne_e32, vmsne_e64)(_.vsi),
      VMSLTU_VV  -> dupM(vmsltu_e8, vmsltu_e16, vmsltu_e32, vmsltu_e64)(_.S1v),
      VMSLTU_VX  -> dupM(vmsltu_e8, vmsltu_e16, vmsltu_e32, vmsltu_e64)(_.S2x),
      VMSLT_VV   -> dupM(vmslt_e8, vmslt_e16, vmslt_e32, vmslt_e64)(_.S1v),
      VMSLT_VX   -> dupM(vmslt_e8, vmslt_e16, vmslt_e32, vmslt_e64)(_.S2x),
      VMSLEU_VV  -> dupM(vmsleu_e8, vmsleu_e16, vmsleu_e32, vmsleu_e64)(_.S1v),
      VMSLEU_VX  -> dupM(vmsleu_e8, vmsleu_e16, vmsleu_e32, vmsleu_e64)(_.S2x),
      VMSLEU_VI  -> dupM(vmsleu_e8, vmsleu_e16, vmsleu_e32, vmsleu_e64)(_.vui),
      VMSLE_VV   -> dupM(vmsle_e8, vmsle_e16, vmsle_e32, vmsle_e64)(_.S1v),
      VMSLE_VX   -> dupM(vmsle_e8, vmsle_e16, vmsle_e32, vmsle_e64)(_.S2x),
      VMSLE_VI   -> dupM(vmsle_e8, vmsle_e16, vmsle_e32, vmsle_e64)(_.vsi),
      VMSGTU_VX  -> dupM(vmsgtu_e8, vmsgtu_e16, vmsgtu_e32, vmsgtu_e64)(_.S2x),
      VMSGTU_VI  -> dupM(vmsgtu_e8, vmsgtu_e16, vmsgtu_e32, vmsgtu_e64)(_.vui),
      VMSGT_VX   -> dupM(vmsgt_e8, vmsgt_e16, vmsgt_e32, vmsgt_e64)(_.S2x),
      VMSGT_VI   -> dupM(vmsgt_e8, vmsgt_e16, vmsgt_e32, vmsgt_e64)(_.vsi),
    )

    val opi10Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      VSADDU_VV  -> dup(vsaddu_e8, vsaddu_e16, vsaddu_e32, vsaddu_e64)(_.S1v),
      VSADDU_VX  -> dup(vsaddu_e8, vsaddu_e16, vsaddu_e32, vsaddu_e64)(_.S2x),
      VSADDU_VI  -> dup(vsaddu_e8, vsaddu_e16, vsaddu_e32, vsaddu_e64)(_.vui),
      VSADD_VV   -> dup(vsadd_e8, vsadd_e16, vsadd_e32, vsadd_e64)(_.S1v),
      VSADD_VX   -> dup(vsadd_e8, vsadd_e16, vsadd_e32, vsadd_e64)(_.S2x),
      VSADD_VI   -> dup(vsadd_e8, vsadd_e16, vsadd_e32, vsadd_e64)(_.vsi),
      VSSUBU_VV  -> dup(vssubu_e8, vssubu_e16, vssubu_e32, vssubu_e64)(_.S1v),
      VSSUBU_VX  -> dup(vssubu_e8, vssubu_e16, vssubu_e32, vssubu_e64)(_.S2x),
      VSSUB_VV   -> dup(vssub_e8, vssub_e16, vssub_e32, vssub_e64)(_.S1v),
      VSSUB_VX   -> dup(vssub_e8, vssub_e16, vssub_e32, vssub_e64)(_.S2x),
      VSLL_VV    -> dup(vsll_e8, vsll_e16, vsll_e32, vsll_e64)(_.S1v),
      VSLL_VX    -> dup(vsll_e8, vsll_e16, vsll_e32, vsll_e64)(_.S2x),
      VSLL_VI    -> dup(vsll_e8, vsll_e16, vsll_e32, vsll_e64)(_.vui),
      VSMUL_VV   -> dup(vsmul_e8, vsmul_e16, vsmul_e32, vsmul_e64)(_.S1v),
      VSMUL_VX   -> dup(vsmul_e8, vsmul_e16, vsmul_e32, vsmul_e64)(_.S2x),
      VMV1R_V    -> same(Seq.fill(1)(vmvnr + Src2Vp)),
      VMV2R_V    -> same(Seq.fill(2)(vmvnr + Src2Vp)),
      VMV4R_V    -> same(Seq.fill(4)(vmvnr + Src2Vp)),
      VMV8R_V    -> same(Seq.fill(8)(vmvnr + Src2Vp)),
      VSRL_VV    -> dup(vsrl_e8, vsrl_e16, vsrl_e32, vsrl_e64)(_.S1v),
      VSRL_VX    -> dup(vsrl_e8, vsrl_e16, vsrl_e32, vsrl_e64)(_.S2x),
      VSRL_VI    -> dup(vsrl_e8, vsrl_e16, vsrl_e32, vsrl_e64)(_.vui),
      VSRA_VV    -> dup(vsra_e8, vsra_e16, vsra_e32, vsra_e64)(_.S1v),
      VSRA_VX    -> dup(vsra_e8, vsra_e16, vsra_e32, vsra_e64)(_.S2x),
      VSRA_VI    -> dup(vsra_e8, vsra_e16, vsra_e32, vsra_e64)(_.vui),
      VSSRL_VV   -> dup(vssrl_e8, vssrl_e16, vssrl_e32, vssrl_e64)(_.S1v),
      VSSRL_VX   -> dup(vssrl_e8, vssrl_e16, vssrl_e32, vssrl_e64)(_.S2x),
      VSSRL_VI   -> dup(vssrl_e8, vssrl_e16, vssrl_e32, vssrl_e64)(_.vui),
      VSSRA_VV   -> dup(vssra_e8, vssra_e16, vssra_e32, vssra_e64)(_.S1v),
      VSSRA_VX   -> dup(vssra_e8, vssra_e16, vssra_e32, vssra_e64)(_.S2x),
      VSSRA_VI   -> dup(vssra_e8, vssra_e16, vssra_e32, vssra_e64)(_.vui),
      VNSRL_WV   -> dupF2N(vnsrl_e8, vnsrl_e16, vnsrl_e32)(_.S1v),
      VNSRL_WX   -> dupF2N(vnsrl_e8, vnsrl_e16, vnsrl_e32)(_.S2x),
      VNSRL_WI   -> dupF2N(vnsrl_e8, vnsrl_e16, vnsrl_e32)(_.vui),
      VNSRA_WV   -> dupF2N(vnsra_e8, vnsra_e16, vnsra_e32)(_.S1v),
      VNSRA_WX   -> dupF2N(vnsra_e8, vnsra_e16, vnsra_e32)(_.S2x),
      VNSRA_WI   -> dupF2N(vnsra_e8, vnsra_e16, vnsra_e32)(_.vui),
      VNCLIPU_WV -> dupF2N(vnclipu_e8, vnclipu_e16, vnclipu_e32)(_.S1v),
      VNCLIPU_WX -> dupF2N(vnclipu_e8, vnclipu_e16, vnclipu_e32)(_.S2x),
      VNCLIPU_WI -> dupF2N(vnclipu_e8, vnclipu_e16, vnclipu_e32)(_.vui),
      VNCLIP_WV  -> dupF2N(vnclip_e8, vnclip_e16, vnclip_e32)(_.S1v),
      VNCLIP_WX  -> dupF2N(vnclip_e8, vnclip_e16, vnclip_e32)(_.S2x),
      VNCLIP_WI  -> dupF2N(vnclip_e8, vnclip_e16, vnclip_e32)(_.vui),
    )

    val opi11Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
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

    val opm00Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      VREDSUM_VS -> redu(
        vredsum_e8, vredsum_e16, vredsum_e32, vredsum_e64,
        vadd_e8.S1v, vadd_e16.S1v, vadd_e32.S1v, vadd_e64.S1v,
      ),
      VREDAND_VS -> redu(
        vredand_e8 , vredand_e16 , vredand_e32 , vredand_e64 ,
        vand_e8.S1v, vand_e16.S1v, vand_e32.S1v, vand_e64.S1v,
      ),
      VREDOR_VS -> redu(
        vredor_e8 , vredor_e16 , vredor_e32 , vredor_e64 ,
        vor_e8.S1v, vor_e16.S1v, vor_e32.S1v, vor_e64.S1v,
      ),
      VREDXOR_VS -> redu(
        vredxor_e8 , vredxor_e16 , vredxor_e32 , vredxor_e64 ,
        vxor_e8.S1v, vxor_e16.S1v, vxor_e32.S1v, vxor_e64.S1v,
      ),
      VREDMINU_VS -> redu(
        vredminu_e8 , vredminu_e16 , vredminu_e32 , vredminu_e64 ,
        vminu_e8.S1v, vminu_e16.S1v, vminu_e32.S1v, vminu_e64.S1v,
      ),
      VREDMIN_VS -> redu(
        vredmin_e8 , vredmin_e16 , vredmin_e32 , vredmin_e64 ,
        vmin_e8.S1v, vmin_e16.S1v, vmin_e32.S1v, vmin_e64.S1v,
      ),
      VREDMAXU_VS -> redu(
        vredmaxu_e8 , vredmaxu_e16 , vredmaxu_e32 , vredmaxu_e64 ,
        vmaxu_e8.S1v, vmaxu_e16.S1v, vmaxu_e32.S1v, vmaxu_e64.S1v,
      ),
      VREDMAX_VS -> redu(
        vredmax_e8 , vredmax_e16 , vredmax_e32 , vredmax_e64 ,
        vmax_e8.S1v, vmax_e16.S1v, vmax_e32.S1v, vmax_e64.S1v,
      ),

      VAADDU_VV   -> dup(vaaddu_e8, vaaddu_e16, vaaddu_e32, vaaddu_e64)(_.S1v),
      VAADDU_VX   -> dup(vaaddu_e8, vaaddu_e16, vaaddu_e32, vaaddu_e64)(_.S2x),
      VAADD_VV    -> dup(vaadd_e8, vaadd_e16, vaadd_e32, vaadd_e64)(_.S1v),
      VAADD_VX    -> dup(vaadd_e8, vaadd_e16, vaadd_e32, vaadd_e64)(_.S2x),
      VASUBU_VV   -> dup(vasubu_e8, vasubu_e16, vasubu_e32, vasubu_e64)(_.S1v),
      VASUBU_VX   -> dup(vasubu_e8, vasubu_e16, vasubu_e32, vasubu_e64)(_.S2x),
      VASUB_VV    -> dup(vasub_e8, vasub_e16, vasub_e32, vasub_e64)(_.S1v),
      VASUB_VX    -> dup(vasub_e8, vasub_e16, vasub_e32, vasub_e64)(_.S2x),

      // Todo: vclmul, vclmulh

      VSLIDE1UP_VX    -> dup(vslide1up_e8, vslide1up_e16, vslide1up_e32, vslide1up_e64)(_.S2x),
      VSLIDE1DOWN_VX  -> dup(vslide1down_e8, vslide1down_e16, vslide1down_e32, vslide1down_e64)(_.S2x),
    )

    val opm01Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      // VWXUNARY0
      VMV_X_S  -> sewSame(vmv_vs2x_e8, vmv_vs2x_e16, vmv_vs2x_e32, vmv_vs2x_e64)(_.dx),
      VCPOP_M  -> same(Seq(vcpop_m)),
      VFIRST_M -> same(Seq(vfirst)),
      // VRXUNARY0
      VMV_S_X -> SeqMap.from(
        Seq(
          e8 -> vmv_x2vs_e8,
          e16 -> vmv_x2vs_e16,
          e32 -> vmv_x2vs_e32,
          e64 -> vmv_x2vs_e64,
        ).flatMap { case (sew, uop) =>
          val leM1Uops = for (lmul <- Seq(mf8, mf4, mf2, m1)) yield {
            (sew ## lmul) -> Seq(uop)
          }
          val m2Uops = (sew ## m2) -> (uop +: Seq.fill(1)(vtail))
          val m4Uops = (sew ## m4) -> (uop +: Seq.fill(3)(vtail))
          val m8Uops = (sew ## m8) -> (uop +: Seq.fill(7)(vtail))

          leM1Uops :+ m2Uops :+ m4Uops :+ m8Uops
        }
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
      VREV8_V     -> dup(vrev8_e8, vrev8_e16, vrev8_e32, vrev8_e64)(_ + Src2Vp),

      // VMUNARY0
      VMSBF_M     -> same(Seq(vmsbf)),
      VMSOF_M     -> same(Seq(vmsof)),
      VMSIF_M     -> same(Seq(vmsif)),
      VIOTA_M     -> dup(viota_e8, viota_e16, viota_e32, viota_e64)(_ + Src2Vp),
      VID_V       -> dup(vid_e8, vid_e16, vid_e32, vid_e64)(identity),

      VCOMPRESS_VM -> dup(vcompress_e8, vcompress_e16, vcompress_e32, vcompress_e64)(_.S1v),

      VMANDN_MM   -> same(Seq(vmandn)),
      VMAND_MM    -> same(Seq(vmand)),
      VMOR_MM     -> same(Seq(vmor)),
      VMXOR_MM    -> same(Seq(vmxor)),
      VMORN_MM    -> same(Seq(vmorn)),
      VMNAND_MM   -> same(Seq(vmnand)),
      VMNOR_MM    -> same(Seq(vmnor)),
      VMXNOR_MM   -> same(Seq(vmxnor)),
    )

    val opm10Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      VDIVU_VV    -> dup(vdivu_e8, vdivu_e16, vdivu_e32, vdivu_e64)(_.S1v),
      VDIVU_VX    -> dup(vdivu_e8, vdivu_e16, vdivu_e32, vdivu_e64)(_.S2x),
      VDIV_VV     -> dup(vdiv_e8, vdiv_e16, vdiv_e32, vdiv_e64)(_.S1v),
      VDIV_VX     -> dup(vdiv_e8, vdiv_e16, vdiv_e32, vdiv_e64)(_.S2x),
      VREMU_VV    -> dup(vremu_e8, vremu_e16, vremu_e32, vremu_e64)(_.S1v),
      VREMU_VX    -> dup(vremu_e8, vremu_e16, vremu_e32, vremu_e64)(_.S2x),
      VREM_VV     -> dup(vrem_e8, vrem_e16, vrem_e32, vrem_e64)(_.S1v),
      VREM_VX     -> dup(vrem_e8, vrem_e16, vrem_e32, vrem_e64)(_.S2x),

      VMULHU_VV   -> dup(vmulhu_e8, vmulhu_e16, vmulhu_e32, vmulhu_e64)(_.S1v),
      VMULHU_VX   -> dup(vmulhu_e8, vmulhu_e16, vmulhu_e32, vmulhu_e64)(_.S2x),
      VMUL_VV     -> dup(vmul_e8, vmul_e16, vmul_e32, vmul_e64)(_.S1v),
      VMUL_VX     -> dup(vmul_e8, vmul_e16, vmul_e32, vmul_e64)(_.S2x),
      VMULHSU_VV  -> dup(vmulhsu_e8, vmulhsu_e16, vmulhsu_e32, vmulhsu_e64)(_.S1v),
      VMULHSU_VX  -> dup(vmulhsu_e8, vmulhsu_e16, vmulhsu_e32, vmulhsu_e64)(_.S2x),
      VMULH_VV    -> dup(vmulh_e8, vmulh_e16, vmulh_e32, vmulh_e64)(_.S1v),
      VMULH_VX    -> dup(vmulh_e8, vmulh_e16, vmulh_e32, vmulh_e64)(_.S2x),

      VMADD_VV    -> dup(vmadd_e8, vmadd_e16, vmadd_e32, vmadd_e64)(_.S1v),
      VMADD_VX    -> dup(vmadd_e8, vmadd_e16, vmadd_e32, vmadd_e64)(_.S2x),
      VNMSUB_VV   -> dup(vnmsub_e8, vnmsub_e16, vnmsub_e32, vnmsub_e64)(_.S1v),
      VNMSUB_VX   -> dup(vnmsub_e8, vnmsub_e16, vnmsub_e32, vnmsub_e64)(_.S2x),
      VMACC_VV    -> dup(vmacc_e8, vmacc_e16, vmacc_e32, vmacc_e64)(_.S1v),
      VMACC_VX    -> dup(vmacc_e8, vmacc_e16, vmacc_e32, vmacc_e64)(_.S2x),
      VNMSAC_VV   -> dup(vnmsac_e8, vnmsac_e16, vnmsac_e32, vnmsac_e64)(_.S1v),
      VNMSAC_VX   -> dup(vnmsac_e8, vnmsac_e16, vnmsac_e32, vnmsac_e64)(_.S2x),
    )

    val opm11Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      VWADDU_VV   -> dupF2W(vwaddu_e8, vwaddu_e16, vwaddu_e32)(_.S1v),
      VWADDU_VX   -> dupF2W(vwaddu_e8, vwaddu_e16, vwaddu_e32)(_.S2x),
      VWADD_VV    -> dupF2W(vwadd_e8, vwadd_e16, vwadd_e32)(_.S1v),
      VWADD_VX    -> dupF2W(vwadd_e8, vwadd_e16, vwadd_e32)(_.S2x),
      VWSUBU_VV   -> dupF2W(vwsubu_e8, vwsubu_e16, vwsubu_e32)(_.S1v),
      VWSUBU_VX   -> dupF2W(vwsubu_e8, vwsubu_e16, vwsubu_e32)(_.S2x),
      VWSUB_VV    -> dupF2W(vwsub_e8, vwsub_e16, vwsub_e32)(_.S1v),
      VWSUB_VX    -> dupF2W(vwsub_e8, vwsub_e16, vwsub_e32)(_.S2x),
      VWADDU_WV   -> dupF2W(vwaddu_w_e8, vwaddu_w_e16, vwaddu_w_e32)(_.S1v),
      VWADDU_WX   -> dupF2W(vwaddu_w_e8, vwaddu_w_e16, vwaddu_w_e32)(_.S2x),
      VWADD_WV    -> dupF2W(vwadd_w_e8, vwadd_w_e16, vwadd_w_e32)(_.S1v),
      VWADD_WX    -> dupF2W(vwadd_w_e8, vwadd_w_e16, vwadd_w_e32)(_.S2x),
      VWSUBU_WV   -> dupF2W(vwsubu_w_e8, vwsubu_w_e16, vwsubu_w_e32)(_.S1v),
      VWSUBU_WX   -> dupF2W(vwsubu_w_e8, vwsubu_w_e16, vwsubu_w_e32)(_.S2x),
      VWSUB_WV    -> dupF2W(vwsub_w_e8, vwsub_w_e16, vwsub_w_e32)(_.S1v),
      VWSUB_WX    -> dupF2W(vwsub_w_e8, vwsub_w_e16, vwsub_w_e32)(_.S2x),
      VWMULU_VV   -> dupF2W(vwmulu_e8, vwmulu_e16, vwmulu_e32)(_.S1v),
      VWMULU_VX   -> dupF2W(vwmulu_e8, vwmulu_e16, vwmulu_e32)(_.S2x),
      VWMULSU_VV  -> dupF2W(vwmulsu_e8, vwmulsu_e16, vwmulsu_e32)(_.S1v),
      VWMULSU_VX  -> dupF2W(vwmulsu_e8, vwmulsu_e16, vwmulsu_e32)(_.S2x),
      VWMUL_VV    -> dupF2W(vwmul_e8, vwmul_e16, vwmul_e32)(_.S1v),
      VWMUL_VX    -> dupF2W(vwmul_e8, vwmul_e16, vwmul_e32)(_.S2x),
      VWMACCU_VV  -> dupF2W(vwmaccu_e8, vwmaccu_e16, vwmaccu_e32)(_.S1v),
      VWMACCU_VX  -> dupF2W(vwmaccu_e8, vwmaccu_e16, vwmaccu_e32)(_.S2x),
      VWMACC_VV   -> dupF2W(vwmacc_e8, vwmacc_e16, vwmacc_e32)(_.S1v),
      VWMACC_VX   -> dupF2W(vwmacc_e8, vwmacc_e16, vwmacc_e32)(_.S2x),
      VWMACCUS_VX -> dupF2W(vwmaccus_e8, vwmaccus_e16, vwmaccus_e32)(_.S2x),
      VWMACCSU_VV -> dupF2W(vwmaccsu_e8, vwmaccsu_e16, vwmaccsu_e32)(_.S1v),
      VWMACCSU_VX -> dupF2W(vwmaccsu_e8, vwmaccsu_e16, vwmaccsu_e32)(_.S2x),
    )

    val opf00Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      VFADD_VV -> dup(null, vfadd_fp16, vfadd_fp32, vfadd_fp64)(_.S1v),
      VFADD_VF -> dup(null, vfadd_fp16, vfadd_fp32, vfadd_fp64)(_.S1f),
      VFSUB_VV -> dup(null, vfsub_fp16, vfsub_fp32, vfsub_fp64)(_.S1v),
      VFSUB_VF -> dup(null, vfsub_fp16, vfsub_fp32, vfsub_fp64)(_.S1f),
      VFMIN_VV -> dup(null, vfmin_fp16, vfmin_fp32, vfmin_fp64)(_.S1v),
      VFMIN_VF -> dup(null, vfmin_fp16, vfmin_fp32, vfmin_fp64)(_.S1f),
      VFMAX_VV -> dup(null, vfmax_fp16, vfmax_fp32, vfmax_fp64)(_.S1v),
      VFMAX_VF -> dup(null, vfmax_fp16, vfmax_fp32, vfmax_fp64)(_.S1f),

      VFREDUSUM_VS -> redu(
        null, null,
        vfredusum_fp16, vfadd_fp16.copy().S1v,
        vfredusum_fp32, vfadd_fp32.copy().S1v,
        vfredusum_fp64, vfadd_fp64.copy().S1v,
      ),
      VFREDOSUM_VS -> dup(null, vfredosum_fp16, vfredosum_fp32, vfredosum_fp64)(_ + Src2Mask),
      VFREDMIN_VS -> redu(
        null, null,
        vfredmin_fp16, vfmin_fp16.copy().S1v,
        vfredmin_fp32, vfmin_fp32.copy().S1v,
        vfredmin_fp64, vfmin_fp64.copy().S1v,
      ),
      VFREDMAX_VS -> redu(
        null, null,
        vfredmax_fp16, vfmax_fp16.copy().S1v,
        vfredmax_fp32, vfmax_fp32.copy().S1v,
        vfredmax_fp64, vfmax_fp64.copy().S1v,
      ),

      VFSGNJ_VV  -> dup(null, vfsgnj_fp16, vfsgnj_fp32, vfsgnj_fp64)(_.S1v),
      VFSGNJ_VF  -> dup(null, vfsgnj_fp16, vfsgnj_fp32, vfsgnj_fp64)(_.S1f),
      VFSGNJN_VV -> dup(null, vfsgnjn_fp16, vfsgnjn_fp32, vfsgnjn_fp64)(_.S1v),
      VFSGNJN_VF -> dup(null, vfsgnjn_fp16, vfsgnjn_fp32, vfsgnjn_fp64)(_.S1f),
      VFSGNJX_VV -> dup(null, vfsgnjx_fp16, vfsgnjx_fp32, vfsgnjx_fp64)(_.S1v),
      VFSGNJX_VF -> dup(null, vfsgnjx_fp16, vfsgnjx_fp32, vfsgnjx_fp64)(_.S1f),

      VFSLIDE1UP_VF    -> dup(null, vslide1up_e16, vslide1up_e32, vslide1up_e64)(_.S1f),
      VFSLIDE1DOWN_VF  -> dup(null, vslide1down_e16, vslide1down_e32, vslide1down_e64)(_.S1f),
    )

    val opf01Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      // VWFUNARY0
      VFMV_F_S -> sewSame(null, vmv_vs2x_e16, vmv_vs2x_e32, vmv_vs2x_e64)(_.df),

      // VRFUNARY0
      VFMV_S_F -> SeqMap.from(
        Seq(
          e8 -> vmv_x2vs_e8,
          e16 -> vmv_x2vs_e16,
          e32 -> vmv_x2vs_e32,
          e64 -> vmv_x2vs_e64,
        ).flatMap { case (sew, _uop) =>
          val uop = _uop.copy() - Src1Vp + Src1Fp
          val leM1Uops = for (lmul <- Seq(mf8, mf4, mf2, m1)) yield {
            (sew ## lmul) -> Seq(uop)
          }
          val m2Uops = (sew ## m2) -> (uop +: Seq.fill(1)(vtail))
          val m4Uops = (sew ## m4) -> (uop +: Seq.fill(3)(vtail))
          val m8Uops = (sew ## m8) -> (uop +: Seq.fill(7)(vtail))

          leM1Uops :+ m2Uops :+ m4Uops :+ m8Uops
        }
      ),

      // VFUNARY0
      VFCVT_XU_F_V      -> dup(null, vfcvt_ui16_fp16, vfcvt_ui32_fp32, vfcvt_ui64_fp64)(_ + Src2Vp),
      VFCVT_X_F_V       -> dup(null, vfcvt_si16_fp16, vfcvt_si32_fp32, vfcvt_si64_fp64)(_ + Src2Vp),
      VFCVT_F_XU_V      -> dup(null, vfcvt_fp16_ui16, vfcvt_fp32_ui32, vfcvt_fp64_ui64)(_ + Src2Vp),
      VFCVT_F_X_V       -> dup(null, vfcvt_fp16_si16, vfcvt_fp32_si32, vfcvt_fp64_si64)(_ + Src2Vp),
      VFCVT_RTZ_XU_F_V  -> dup(null, vfcvt_ui16_fp16, vfcvt_ui32_fp32, vfcvt_ui64_fp64)(_ + Src2Vp),
      VFCVT_RTZ_X_F_V   -> dup(null, vfcvt_si16_fp16, vfcvt_si32_fp32, vfcvt_si64_fp64)(_ + Src2Vp),

      VFWCVT_XU_F_V     -> dupF2W(null, vfcvt_ui32_fp16, vfcvt_ui64_fp32)(_ + Src2Vp),
      VFWCVT_X_F_V      -> dupF2W(null, vfcvt_si32_fp16, vfcvt_si64_fp32)(_ + Src2Vp),
      VFWCVT_F_XU_V     -> dupF2W(null, vfcvt_fp32_ui16, vfcvt_fp64_ui32)(_ + Src2Vp),
      VFWCVT_F_X_V      -> dupF2W(null, vfcvt_fp32_si16, vfcvt_fp64_si32)(_ + Src2Vp),
      VFWCVT_F_F_V      -> dupF2W(null, vfcvt_fp32_fp16, vfcvt_fp64_fp32)(_ + Src2Vp),
      VFWCVT_RTZ_XU_F_V -> dupF2W(null, vfcvt_ui32_fp16, vfcvt_ui64_fp32)(_ + Src2Vp),
      VFWCVT_RTZ_X_F_V  -> dupF2W(null, vfcvt_si32_fp16, vfcvt_si64_fp32)(_ + Src2Vp),

      VFNCVT_XU_F_W     -> dupF2N(vfcvt_ui8_fp16, vfcvt_ui16_fp32, vfcvt_ui32_fp64)(_ + Src2Vp),
      VFNCVT_X_F_W      -> dupF2N(vfcvt_si8_fp16, vfcvt_si16_fp32, vfcvt_si32_fp64)(_ + Src2Vp),
      VFNCVT_F_XU_W     -> dupF2N(null, vfcvt_fp16_ui32, vfcvt_fp32_ui64)(_ + Src2Vp),
      VFNCVT_F_X_W      -> dupF2N(null, vfcvt_fp16_si32, vfcvt_fp32_si64)(_ + Src2Vp),
      VFNCVT_F_F_W      -> dupF2N(null, vfcvt_fp16_fp32, vfcvt_fp32_fp64)(_ + Src2Vp),
      VFNCVT_ROD_F_F_W  -> dupF2N(null, vfcvt_fp16_fp32, vfcvt_fp32_fp64)(_ + Src2Vp),
      VFNCVT_RTZ_XU_F_W -> dupF2N(vfcvt_ui8_fp16, vfcvt_ui16_fp32, vfcvt_ui32_fp64)(_ + Src2Vp),
      VFNCVT_RTZ_X_F_W  -> dupF2N(vfcvt_si8_fp16, vfcvt_si16_fp32, vfcvt_si32_fp64)(_ + Src2Vp),

      // VFUNARY1
      VFSQRT_V          -> dup(null, vfsqrt_fp16, vfsqrt_fp32, vfsqrt_fp64)(_ + Src2Vp),
      VFRSQRT7_V        -> dup(null, vfrsqrt7_fp16, vfrsqrt7_fp32, vfrsqrt7_fp64)(_ + Src2Vp),
      VFREC7_V          -> dup(null, vfrec7_fp16, vfrec7_fp32, vfrec7_fp64)(_ + Src2Vp),
      VFCLASS_V         -> dup(null, vfclass_fp16, vfclass_fp32, vfclass_fp64)(_ + Src2Vp),
      VFMERGE_VFM       -> dup(null, vmerge_vx_e16, vmerge_vx_e32, vmerge_vx_e64)(_.S1f),
      VFMV_V_F          -> dup(null, vmerge_vx_e16, vmerge_vx_e32, vmerge_vx_e64)(_.S1f),

      VMFEQ_VV          -> dupM(null, vmfeq_fp16, vmfeq_fp32, vmfeq_fp64)(_.S1v),
      VMFEQ_VF          -> dupM(null, vmfeq_fp16, vmfeq_fp32, vmfeq_fp64)(_.S1f),
      VMFLE_VV          -> dupM(null, vmfle_fp16, vmfle_fp32, vmfle_fp64)(_.S1v),
      VMFLE_VF          -> dupM(null, vmfle_fp16, vmfle_fp32, vmfle_fp64)(_.S1f),
      VMFLT_VV          -> dupM(null, vmflt_fp16, vmflt_fp32, vmflt_fp64)(_.S1v),
      VMFLT_VF          -> dupM(null, vmflt_fp16, vmflt_fp32, vmflt_fp64)(_.S1f),
      VMFNE_VV          -> dupM(null, vmfne_fp16, vmfne_fp32, vmfne_fp64)(_.S1v),
      VMFNE_VF          -> dupM(null, vmfne_fp16, vmfne_fp32, vmfne_fp64)(_.S1f),
      VMFGT_VF          -> dupM(null, vmfgt_fp16, vmfgt_fp32, vmfgt_fp64)(_.S1f),
      VMFGE_VF          -> dupM(null, vmfge_fp16, vmfge_fp32, vmfge_fp64)(_.S1f),
    )

    val opf10Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      VFDIV_VV          -> dup(null, vfdiv_fp16, vfdiv_fp32, vfdiv_fp64)(_.S1v),
      VFDIV_VF          -> dup(null, vfdiv_fp16, vfdiv_fp32, vfdiv_fp64)(_.S1f),
      VFRDIV_VF         -> dup(null, vfdiv_fp16, vfdiv_fp32, vfdiv_fp64)(_.S1f.rev),
      VFMUL_VV          -> dup(null, vfmul_fp16, vfmul_fp32, vfmul_fp64)(_.S1v),
      VFMUL_VF          -> dup(null, vfmul_fp16, vfmul_fp32, vfmul_fp64)(_.S1f),
      VFRSUB_VF         -> dup(null, vfsub_fp16, vfsub_fp32, vfsub_fp64)(_.S1f.rev),

      VFMADD_VV         -> dup(null, vfmadd_fp16, vfmadd_fp32, vfmadd_fp64)(_.S1v),
      VFMADD_VF         -> dup(null, vfmadd_fp16, vfmadd_fp32, vfmadd_fp64)(_.S1f),
      VFNMADD_VV        -> dup(null, vfnmadd_fp16, vfnmadd_fp32, vfnmadd_fp64)(_.S1v),
      VFNMADD_VF        -> dup(null, vfnmadd_fp16, vfnmadd_fp32, vfnmadd_fp64)(_.S1f),
      VFMSUB_VV         -> dup(null, vfmsub_fp16, vfmsub_fp32, vfmsub_fp64)(_.S1v),
      VFMSUB_VF         -> dup(null, vfmsub_fp16, vfmsub_fp32, vfmsub_fp64)(_.S1f),
      VFNMSUB_VV        -> dup(null, vfnmsub_fp16, vfnmsub_fp32, vfnmsub_fp64)(_.S1v),
      VFNMSUB_VF        -> dup(null, vfnmsub_fp16, vfnmsub_fp32, vfnmsub_fp64)(_.S1f),
      VFMACC_VV         -> dup(null, vfmacc_fp16, vfmacc_fp32, vfmacc_fp64)(_.S1v),
      VFMACC_VF         -> dup(null, vfmacc_fp16, vfmacc_fp32, vfmacc_fp64)(_.S1f),
      VFNMACC_VV        -> dup(null, vfnmacc_fp16, vfnmacc_fp32, vfnmacc_fp64)(_.S1v),
      VFNMACC_VF        -> dup(null, vfnmacc_fp16, vfnmacc_fp32, vfnmacc_fp64)(_.S1f),
      VFMSAC_VV         -> dup(null, vfmsac_fp16, vfmsac_fp32, vfmsac_fp64)(_.S1v),
      VFMSAC_VF         -> dup(null, vfmsac_fp16, vfmsac_fp32, vfmsac_fp64)(_.S1f),
      VFNMSAC_VV        -> dup(null, vfnmsac_fp16, vfnmsac_fp32, vfnmsac_fp64)(_.S1v),
      VFNMSAC_VF        -> dup(null, vfnmsac_fp16, vfnmsac_fp32, vfnmsac_fp64)(_.S1f),
    )

    val opf11Table = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      VFWADD_VV         -> dupF2W(null, vfwadd_fp16, vfwadd_fp32)(_.S1v),
      VFWADD_VF         -> dupF2W(null, vfwadd_fp16, vfwadd_fp32)(_.S1f),
      VFWSUB_VV         -> dupF2W(null, vfwsub_fp16, vfwsub_fp32)(_.S1v),
      VFWSUB_VF         -> dupF2W(null, vfwsub_fp16, vfwsub_fp32)(_.S1f),
      VFWADD_WV         -> dupF2W(null, vfwadd_w_fp16, vfwadd_w_fp32)(_.S1v),
      VFWADD_WF         -> dupF2W(null, vfwadd_w_fp16, vfwadd_w_fp32)(_.S1f),
      VFWSUB_WV         -> dupF2W(null, vfwsub_w_fp16, vfwsub_w_fp32)(_.S1v),
      VFWSUB_WF         -> dupF2W(null, vfwsub_w_fp16, vfwsub_w_fp32)(_.S1f),
      VFWMUL_VV         -> dupF2W(null, vfwmul_fp16, vfwmul_fp32)(_.S1v),
      VFWMUL_VF         -> dupF2W(null, vfwmul_fp16, vfwmul_fp32)(_.S1f),
      VFWMACC_VV        -> dupF2W(null, vfwmacc_fp16, vfwmacc_fp32)(_.S1v),
      VFWMACC_VF        -> dupF2W(null, vfwmacc_fp16, vfwmacc_fp32)(_.S1f),
      VFWNMACC_VV       -> dupF2W(null, vfwnmacc_fp16, vfwnmacc_fp32)(_.S1v),
      VFWNMACC_VF       -> dupF2W(null, vfwnmacc_fp16, vfwnmacc_fp32)(_.S1f),
      VFWMSAC_VV        -> dupF2W(null, vfwmsac_fp16, vfwmsac_fp32)(_.S1v),
      VFWMSAC_VF        -> dupF2W(null, vfwmsac_fp16, vfwmsac_fp32)(_.S1f),
      VFWNMSAC_VV       -> dupF2W(null, vfwnmsac_fp16, vfwnmsac_fp32)(_.S1v),
      VFWNMSAC_VF       -> dupF2W(null, vfwnmsac_fp16, vfwnmsac_fp32)(_.S1f),
      // to do
      VFWREDUSUM_VS     -> (
        fwredu(_.e16)(vfwadd_fp16, vfwadd_w_fp16, vfwredusum_fp16) ++
        fwredu(_.e32)(vfwadd_fp32, vfwadd_w_fp32, vfwredusum_fp32)
      ),
      VFWREDOSUM_VS     -> (
        fwredosum(_.e16)(vfwredosum_fp16) ++
        fwredosum(_.e32)(vfwredosum_fp32)
      ),
    )

    val cryptoTable = SeqMap[BitPat, SeqMap[SewLmulPattern, Seq[Opcode]]](
      VSHA2MS_VV -> dup(null, null, vsha256ms, null)(_.S1v),
      VSHA2CL_VV -> dup(null, null, vsha256cl, null)(_.S1v),
      VSHA2CH_VV -> dup(null, null, vsha256ch, null)(_.S1v),
    )

    opi00Table ++ opi01Table ++ opi10Table ++ opi11Table ++
    opm00Table ++ opm01Table ++ opm10Table ++ opm11Table ++
    opf00Table ++ opf01Table ++ opf10Table ++ opf11Table ++ cryptoTable
  }
}
