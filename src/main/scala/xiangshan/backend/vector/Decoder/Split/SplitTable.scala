package xiangshan.backend.vector.Decoder.Split

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.InstPattern.{VecArithInstPattern, VecInstPattern, VecIntVVVPattern}
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.LmulPattern
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

  private val m8 = LmulPattern(8)
  private val m4 = LmulPattern(4)
  private val m2 = LmulPattern(2)
  private val m1 = LmulPattern(1)
  private val mf2 = LmulPattern(0.5)
  private val mf4 = LmulPattern(0.25)
  private val mf8 = LmulPattern(0.125)

  private def dup(uop: => VecUop): SeqMap[LmulPattern, Seq[VecUop]] = {
    SeqMap(
      m8 -> Seq.fill(8)(uop),
      m4 -> Seq.fill(4)(uop),
      m2 -> Seq.fill(2)(uop),
      m1 -> Seq.fill(1)(uop),
      mf2 -> Seq.fill(1)(uop),
      mf4 -> Seq.fill(1)(uop),
      mf8 -> Seq.fill(1)(uop),
    )
  }

  private def dupM(uop: => VecUop): SeqMap[LmulPattern, Seq[VecUop]] = {
    SeqMap(
      m8 -> (uop.set(_.vdAlloc, false) +: Seq.fill(7)(uop)),
      m4 -> (uop.set(_.vdAlloc, false) +: Seq.fill(3)(uop)),
      m2 -> (uop.set(_.vdAlloc, false) +: Seq.fill(1)(uop)),
      m1 -> Seq.fill(1)(uop),
      mf2 -> Seq.fill(1)(uop),
      mf4 -> Seq.fill(1)(uop),
      mf8 -> Seq.fill(1)(uop),
    )
  }

  private def dupF2W(uop: => VecUop): SeqMap[LmulPattern, Seq[VecUop]] = {
    SeqMap(
      m8 -> Seq(),
      m4 -> Seq.fill(8)(uop),
      m2 -> Seq.fill(4)(uop),
      m1 -> Seq.fill(2)(uop),
      mf2 -> Seq.fill(1)(uop),
      mf4 -> Seq.fill(1)(uop),
      mf8 -> Seq.fill(1)(uop),
    )
  }

  private def dupF2N(uop: => VecUop): SeqMap[LmulPattern, Seq[VecUop]] = {
    SeqMap(
      m8  -> Seq(),
      m4  -> Seq.tabulate(8)(i => if (i % 2 == 0) uop else uop.set(_.vdAlloc, false)),
      m2  -> Seq.tabulate(4)(i => if (i % 2 == 0) uop else uop.set(_.vdAlloc, false)),
      m1  -> Seq.tabulate(2)(i => if (i % 2 == 0) uop else uop.set(_.vdAlloc, false)),
      mf2 -> Seq.fill(1)(uop),
      mf4 -> Seq.fill(1)(uop),
      mf8 -> Seq.fill(1)(uop),
    )
  }

  private def same(uopSeq: => Seq[VecUop]): SeqMap[LmulPattern, Seq[VecUop]] = {
    SeqMap(
      m8  -> uopSeq,
      m4  -> uopSeq,
      m2  -> uopSeq,
      m1  -> uopSeq,
      mf2 -> uopSeq,
      mf4 -> uopSeq,
      mf8 -> uopSeq,
    )
  }

  private def redu(
    reduop: => VecUop,
    uopi: => VecUop,
  ): SeqMap[LmulPattern, Seq[VecUop]] = SeqMap(
    mf8 -> Seq(reduop.set(_.maskType, Src2Mask)),
    mf4 -> Seq(reduop.set(_.maskType, Src2Mask)),
    mf2 -> Seq(reduop.set(_.maskType, Src2Mask)),
    m1  -> Seq(reduop.set(_.maskType, Src2Mask)),
    m2  -> Seq(
      uopi.set(_.maskType, Src12Mask),
      reduop.set(_.maskType, NoMask),
    ),
    m4  -> Seq(
      uopi.set(_.maskType, Src12Mask),
      uopi.set(_.maskType, Src2Mask),
      uopi.set(_.maskType, Src2Mask),
      reduop.set(_.maskType, NoMask),
    ),
    m8  -> Seq(
      uopi.set(_.maskType, Src12Mask),
      uopi.set(_.maskType, Src2Mask),
      uopi.set(_.maskType, Src2Mask),
      uopi.set(_.maskType, Src2Mask),
      uopi.set(_.maskType, Src2Mask),
      uopi.set(_.maskType, Src2Mask),
      uopi.set(_.maskType, Src2Mask),
      reduop.set(_.maskType, NoMask),
    ),
  )

  val table: SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]] = {

    val opi00Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      VADD_VV  -> dup(vadd.s1v),
      VADD_VX  -> dup(vadd.s1x),
      VADD_VI  -> dup(vadd.s1i),
      // Todo: VANDN
      VSUB_VV  -> dup(vsub.s1v),
      VSUB_VX  -> dup(vsub.s1x),
      VRSUB_VX -> dup(vsub.s1x.setSrc12Rev),
      VRSUB_VI -> dup(vsub.s1i.setSrc12Rev),
      VMINU_VV -> dup(vminu.s1v),
      VMINU_VX -> dup(vminu.s1x),
      VMIN_VV  -> dup(vmin.s1v),
      VMIN_VX  -> dup(vmin.s1x),
      VMAXU_VV -> dup(vmaxu.s1v),
      VMAXU_VX -> dup(vmaxu.s1x),
      VMAX_VV  -> dup(vmax.s1v),
      VMAX_VX  -> dup(vmax.s1x),
      VAND_VV  -> dup(vand.s1v),
      VAND_VX  -> dup(vand.s1x),
      VAND_VI  -> dup(vand.s1i),
      VOR_VV   -> dup(vor.s1v),
      VOR_VX   -> dup(vor.s1x),
      VOR_VI   -> dup(vor.s1i),
      VXOR_VV  -> dup(vxor.s1v),
      VXOR_VX  -> dup(vxor.s1x),
      VXOR_VI  -> dup(vxor.s1i),
      VRGATHER_VV -> dup(vrgather_v),
      VRGATHER_VX -> dup(vrgather_x),
      VRGATHER_VI -> dup(vrgather_i),
      VRGATHEREI16_VV -> dup(vrgatherei16_v),
      VSLIDEUP_VX -> dup(vslideup_x),
      VSLIDEUP_VI -> dup(vslideup_i),
      VSLIDEDOWN_VX -> dup(vslidedown_x),
      VSLIDEDOWN_VI -> dup(vslidedown_i),
    )

    val opi01Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      VADC_VVM   -> dup(vadc.s1v),
      VADC_VXM   -> dup(vadc.s1x),
      VADC_VIM   -> dup(vadc.s1i),
      VMADC_VV   -> dupM(vmadc.s1v),
      VMADC_VX   -> dupM(vmadc.s1x),
      VMADC_VI   -> dupM(vmadc.s1i),
      VMADC_VVM  -> dupM(vmadc.s1v),
      VMADC_VXM  -> dupM(vmadc.s1x),
      VMADC_VIM  -> dupM(vmadc.s1i),
      VSBC_VVM   -> dup(vsbc.s1v),
      VSBC_VXM   -> dup(vsbc.s1x),
      VMSBC_VV   -> dupM(vmsbc.s1v),
      VMSBC_VX   -> dupM(vmsbc.s1x),
      VMSBC_VVM  -> dupM(vmsbc.s1v),
      VMSBC_VXM  -> dupM(vmsbc.s1x),
      // Todo: vror, vrol
      VMERGE_VVM -> dup(vmerge.s1v),
      VMERGE_VXM -> dup(vmerge.s1x),
      VMERGE_VIM -> dup(vmerge.s1i),
      VMV_V_V    -> dup(vmvVec2Vec),
      VMV_V_X    -> dup(vmvInt2Vec),
      VMV_V_I    -> dup(vmvImm2Vec),
      VMSEQ_VV   -> dupM(vmseq.s1v),
      VMSEQ_VX   -> dupM(vmseq.s1x),
      VMSEQ_VI   -> dupM(vmseq.s1i),
      VMSNE_VV   -> dupM(vmsne.s1v),
      VMSNE_VX   -> dupM(vmsne.s1x),
      VMSNE_VI   -> dupM(vmsne.s1i),
      VMSLTU_VV  -> dupM(vmsltu.s1v),
      VMSLTU_VX  -> dupM(vmsltu.s1x),
      VMSLT_VV   -> dupM(vmslt.s1v),
      VMSLT_VX   -> dupM(vmslt.s1x),
      VMSLEU_VV  -> dupM(vmsleu.s1v),
      VMSLEU_VX  -> dupM(vmsleu.s1x),
      VMSLEU_VI  -> dupM(vmsleu.s1i),
      VMSLE_VV   -> dupM(vmsle.s1v),
      VMSLE_VX   -> dupM(vmsle.s1x),
      VMSLE_VI   -> dupM(vmsle.s1i),
      VMSGTU_VX  -> dupM(vmsgtu.s1x),
      VMSGTU_VI  -> dupM(vmsgtu.s1i),
      VMSGT_VX   -> dupM(vmsgt.s1x),
      VMSGT_VI   -> dupM(vmsgt.s1i),
    )

    val opi10Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      VSADDU_VV  -> dup(vsaddu.s1v),
      VSADDU_VX  -> dup(vsaddu.s1x),
      VSADDU_VI  -> dup(vsaddu.s1i),
      VSADD_VV   -> dup(vsadd.s1v),
      VSADD_VX   -> dup(vsadd.s1x),
      VSADD_VI   -> dup(vsadd.s1i),
      VSSUBU_VV  -> dup(vssubu.s1v),
      VSSUBU_VX  -> dup(vssubu.s1x),
      VSSUB_VV   -> dup(vssub.s1v),
      VSSUB_VX   -> dup(vssub.s1x),
      VSLL_VV    -> dup(vsll.s1v),
      VSLL_VX    -> dup(vsll.s1x),
      VSLL_VI    -> dup(vsll.s1i),
      VSMUL_VV   -> dup(vsmul.s1v),
      VSMUL_VX   -> dup(vsmul.s1x),
      VMV1R_V    -> same(Seq.fill(1)(vmvnr)),
      VMV2R_V    -> same(Seq.fill(2)(vmvnr)),
      VMV4R_V    -> same(Seq.fill(4)(vmvnr)),
      VMV8R_V    -> same(Seq.fill(8)(vmvnr)),
      VSRL_VV    -> dup(vsrl.s1v),
      VSRL_VX    -> dup(vsrl.s1x),
      VSRL_VI    -> dup(vsrl.s1i),
      VSRA_VV    -> dup(vsra.s1v),
      VSRA_VX    -> dup(vsra.s1x),
      VSRA_VI    -> dup(vsra.s1i),
      VSSRL_VV   -> dup(vssrl.s1v),
      VSSRL_VX   -> dup(vssrl.s1x),
      VSSRL_VI   -> dup(vssrl.s1i),
      VSSRA_VV   -> dup(vssra.s1v),
      VSSRA_VX   -> dup(vssra.s1x),
      VSSRA_VI   -> dup(vssra.s1i),
      VNSRL_WV   -> dupF2N(vnsrl.s1v),
      VNSRL_WX   -> dupF2N(vnsrl.s1x),
      VNSRL_WI   -> dupF2N(vnsrl.s1i),
      VNSRA_WV   -> dupF2N(vnsra.s1v),
      VNSRA_WX   -> dupF2N(vnsra.s1x),
      VNSRA_WI   -> dupF2N(vnsra.s1i),
      VNCLIPU_WV -> dupF2N(vnclipu.s1v),
      VNCLIPU_WX -> dupF2N(vnclipu.s1x),
      VNCLIPU_WI -> dupF2N(vnclipu.s1i),
      VNCLIP_WV  -> dupF2N(vnclip.s1v),
      VNCLIP_WX  -> dupF2N(vnclip.s1x),
      VNCLIP_WI  -> dupF2N(vnclip.s1i),
    )

    val opi11Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      VWREDSUMU_VS -> SeqMap(
        mf8 -> Seq(
          vwaddu.s1v.set(_.maskType, Src12Mask),
          vwredsumu.set(_.maskType, NoMask),
        ),
        mf4 -> Seq(
          vwaddu.s1v.set(_.maskType, Src12Mask),
          vwredsumu.set(_.maskType, NoMask),
        ),
        mf2 -> Seq(
          vwaddu.s1v.set(_.maskType, Src12Mask),
          vwredsumu.set(_.maskType, NoMask),
        ),
        m1 -> Seq(
          vwaddu.s1v.set(_.maskType, Src12Mask),
          vwredsumu.set(_.maskType, NoMask),
        ),
        m2 -> Seq(
          vwaddu.s1v.set(_.maskType, Src12Mask),
          vwaddu_w.s1v.set(_.maskType, Src2Mask),
          vwaddu_w.s1v.set(_.maskType, Src2Mask),
          vwredsumu.set(_.maskType, NoMask),
        ),
        m4 -> Seq(
          vwaddu.s1v.set(_.maskType, Src12Mask),
          vwaddu_w.s1v.set(_.maskType, Src2Mask),
          vwaddu_w.s1v.set(_.maskType, Src2Mask),
          vwaddu_w.s1v.set(_.maskType, Src2Mask),
          vwaddu_w.s1v.set(_.maskType, Src2Mask),
          vwaddu_w.s1v.set(_.maskType, Src2Mask),
          vwaddu_w.s1v.set(_.maskType, Src2Mask),
          vwredsumu.set(_.maskType, NoMask),
        ),
        m8 -> Seq(),
      ),
      VWREDSUM_VS -> SeqMap(
        mf8 -> Seq(
          vwadd.s1v.set(_.maskType, Src12Mask),
          vwredsum.set(_.maskType, NoMask),
        ),
        mf4 -> Seq(
          vwadd.s1v.set(_.maskType, Src12Mask),
          vwredsum.set(_.maskType, NoMask),
        ),
        mf2 -> Seq(
          vwadd.s1v.set(_.maskType, Src12Mask),
          vwredsum.set(_.maskType, NoMask),
        ),
        m1 -> Seq(
          vwadd.s1v.set(_.maskType, Src12Mask),
          vwredsum.set(_.maskType, NoMask),
        ),
        m2 -> Seq(
          vwadd.s1v.set(_.maskType, Src12Mask),
          vwadd_w.s1v.set(_.maskType, Src2Mask),
          vwadd_w.s1v.set(_.maskType, Src2Mask),
          vwredsum.set(_.maskType, NoMask),
        ),
        m4 -> Seq(
          vwadd.s1v.set(_.maskType, Src12Mask),
          vwadd_w.s1v.set(_.maskType, Src2Mask),
          vwadd_w.s1v.set(_.maskType, Src2Mask),
          vwadd_w.s1v.set(_.maskType, Src2Mask),
          vwadd_w.s1v.set(_.maskType, Src2Mask),
          vwadd_w.s1v.set(_.maskType, Src2Mask),
          vwadd_w.s1v.set(_.maskType, Src2Mask),
          vwredsum.set(_.maskType, NoMask),
        ),
        m8 -> Seq(),
      ),
    )

    val opm00Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      VREDSUM_VS      -> redu(vredsum, vadd.s1v),
      VREDAND_VS      -> redu(vredand, vand.s1v),
      VREDOR_VS       -> redu(vredor, vor.s1v),
      VREDXOR_VS      -> redu(vredxor, vxor.s1v),
      VREDMINU_VS     -> redu(vredminu, vminu.s1v),
      VREDMIN_VS      -> redu(vredmin, vmin.s1v),
      VREDMAXU_VS     -> redu(vredmaxu, vmaxu.s1v),
      VREDMAX_VS      -> redu(vredmax, vmax.s1v),

      VAADDU_VV   -> dup(vaaddu.s1v),
      VAADDU_VX   -> dup(vaaddu.s1x),
      VAADD_VV    -> dup(vaadd.s1v),
      VAADD_VX    -> dup(vaadd.s1x),
      VASUBU_VV   -> dup(vasubu.s1v),
      VASUBU_VX   -> dup(vasubu.s1x),
      VASUB_VV    -> dup(vasub.s1v),
      VASUB_VX    -> dup(vasub.s1x),

      // Todo: vclmul, vclmulh

      VSLIDE1UP_VX    -> dup(vslide1up.s1x),
      VSLIDE1DOWN_VX  -> dup(vslide1down.s1x),
    )

    val opm01Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      // VWXUNARY0
      VMV_X_S  -> same(Seq(vmvVecScala2Int)),
      VCPOP_M  -> same(Seq(vcpop_m)),
      VFIRST_M -> same(Seq(vfirst)),
      // VRXUNARY0
      VMV_S_X -> SeqMap(
        mf8 -> Seq(vmvInt2VecScala),
        mf4 -> Seq(vmvInt2VecScala),
        mf2 -> Seq(vmvInt2VecScala),
        m1  -> Seq(vmvInt2VecScala),
        m2  -> (vmvInt2VecScala +: Seq.fill(1)(vtail)),
        m4  -> (vmvInt2VecScala +: Seq.fill(3)(vtail)),
        m8  -> (vmvInt2VecScala +: Seq.fill(7)(vtail)),
      ),
      // VXUNARY0
      VZEXT_VF8   -> dup(vzext8),
      VZEXT_VF4   -> dup(vzext4),
      VZEXT_VF2   -> dup(vzext2),
      VSEXT_VF8   -> dup(vsext8),
      VSEXT_VF4   -> dup(vsext4),
      VSEXT_VF2   -> dup(vsext2),
      // Todo: vbrev8, vrev8, vbrev, vclz, vctz, vcpop
      // VMUNARY0
      VMSBF_M     -> same(Seq(vmsbf)),
      VMSOF_M     -> same(Seq(vmsof)),
      VMSIF_M     -> same(Seq(vmsif)),
      VIOTA_M     -> dup(viota),
      VID_V       -> dup(viota),

      VCOMPRESS_VM -> dup(vcompress_m),

      VMANDN_MM   -> same(Seq(vmandn)),
      VMAND_MM    -> same(Seq(vmand)),
      VMOR_MM     -> same(Seq(vmor)),
      VMXOR_MM    -> same(Seq(vmxor)),
      VMORN_MM    -> same(Seq(vmorn)),
      VMNAND_MM   -> same(Seq(vmnand)),
      VMNOR_MM    -> same(Seq(vmnor)),
      VMXNOR_MM   -> same(Seq(vmxnor)),
    )

    val opm10Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      VDIVU_VV    -> dup(vdivu.s1v),
      VDIVU_VX    -> dup(vdivu.s1x),
      VDIV_VV     -> dup(vdiv.s1v),
      VDIV_VX     -> dup(vdiv.s1x),
      VREMU_VV    -> dup(vremu.s1v),
      VREMU_VX    -> dup(vremu.s1x),
      VREM_VV     -> dup(vrem.s1v),
      VREM_VX     -> dup(vrem.s1x),

      VMULHU_VV   -> dup(vmulhu.s1v),
      VMULHU_VX   -> dup(vmulhu.s1x),
      VMUL_VV     -> dup(vmul.s1v),
      VMUL_VX     -> dup(vmul.s1x),
      VMULHSU_VV  -> dup(vmulhsu.s1v),
      VMULHSU_VX  -> dup(vmulhsu.s1x),
      VMULH_VV    -> dup(vmulh.s1v),
      VMULH_VX    -> dup(vmulh.s1x),

      VMADD_VV    -> dup(vmadd.s1v),
      VMADD_VX    -> dup(vmadd.s1x),
      VNMSUB_VV   -> dup(vnmsub.s1v),
      VNMSUB_VX   -> dup(vnmsub.s1x),
      VMACC_VV    -> dup(vmacc.s1v),
      VMACC_VX    -> dup(vmacc.s1x),
      VNMSAC_VV   -> dup(vnmsac.s1v),
      VNMSAC_VX   -> dup(vnmsac.s1x),
    )

    val opm11Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      VWADDU_VV   -> dupF2W(vwaddu.s1v),
      VWADDU_VX   -> dupF2W(vwaddu.s1x),
      VWADD_VV    -> dupF2W(vwadd.s1v),
      VWADD_VX    -> dupF2W(vwadd.s1x),
      VWSUBU_VV   -> dupF2W(vwsubu.s1v),
      VWSUBU_VX   -> dupF2W(vwsubu.s1x),
      VWSUB_VV    -> dupF2W(vwsub.s1v),
      VWSUB_VX    -> dupF2W(vwsub.s1x),
      VWADDU_WV   -> dupF2W(vwaddu_w.s1v),
      VWADDU_WX   -> dupF2W(vwaddu_w.s1x),
      VWADD_WV    -> dupF2W(vwadd_w.s1v),
      VWADD_WX    -> dupF2W(vwadd_w.s1x),
      VWSUBU_WV   -> dupF2W(vwsubu_w.s1v),
      VWSUBU_WX   -> dupF2W(vwsubu_w.s1x),
      VWSUB_WV    -> dupF2W(vwsub_w.s1v),
      VWSUB_WX    -> dupF2W(vwsub_w.s1x),
      VWMULU_VV   -> dupF2W(vwmulu.s1v),
      VWMULU_VX   -> dupF2W(vwmulu.s1x),
      VWMULSU_VV  -> dupF2W(vwmulsu.s1v),
      VWMULSU_VX  -> dupF2W(vwmulsu.s1x),
      VWMUL_VV    -> dupF2W(vwmul.s1v),
      VWMUL_VX    -> dupF2W(vwmul.s1x),
      VWMACCU_VV  -> dupF2W(vwmaccu.s1v),
      VWMACCU_VX  -> dupF2W(vwmaccu.s1x),
      VWMACC_VV   -> dupF2W(vwmacc.s1v),
      VWMACC_VX   -> dupF2W(vwmacc.s1x),
      VWMACCUS_VX -> dupF2W(vwmaccus.s1x),
      VWMACCSU_VV -> dupF2W(vwmaccsu.s1v),
      VWMACCSU_VX -> dupF2W(vwmaccsu.s1x),
    )

    val opf00Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      VFADD_VV -> dup(vfadd.s1v),
      VFADD_VF -> dup(vfadd.s1f),
      VFSUB_VV -> dup(vfsub.s1v),
      VFSUB_VF -> dup(vfsub.s1f),
      VFMIN_VV -> dup(vfmin.s1v),
      VFMIN_VF -> dup(vfmin.s1f),
      VFMAX_VV -> dup(vfmax.s1v),
      VFMAX_VF -> dup(vfmax.s1f),

      VFREDUSUM_VS -> redu(vfredosum, vfadd.s1v),
      VFREDOSUM_VS -> dup(vfredosum),
      VFREDMIN_VS -> redu(vfredmin, vfmin.s1v),
      VFREDMAX_VS -> redu(vfredmax, vfmax.s1v),

      VFSGNJ_VV  -> dup(vfsgnj.s1v),
      VFSGNJ_VF  -> dup(vfsgnj.s1f),
      VFSGNJN_VV -> dup(vfsgnjn.s1v),
      VFSGNJN_VF -> dup(vfsgnjn.s1f),
      VFSGNJX_VV -> dup(vfsgnjx.s1v),
      VFSGNJX_VF -> dup(vfsgnjx.s1f),

      VFSLIDE1UP_VF    -> dup(vslide1up.s1f),
      VFSLIDE1DOWN_VF  -> dup(vslide1down.s1f),
    )

    val opf01Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      // VWFUNARY0
      VFMV_F_S -> same(Seq(vmvVecScala2Fp)),

      // VRFUNARY0
      VFMV_S_F -> SeqMap(
        mf8 -> Seq(vmvFp2VecScala),
        mf4 -> Seq(vmvFp2VecScala),
        mf2 -> Seq(vmvFp2VecScala),
        m1  -> Seq(vmvFp2VecScala),
        m2  -> (vmvFp2VecScala +: Seq.fill(1)(vtail)),
        m4  -> (vmvFp2VecScala +: Seq.fill(3)(vtail)),
        m8  -> (vmvFp2VecScala +: Seq.fill(7)(vtail)),
      ),

      // VFUNARY0
      VFCVT_XU_F_V      -> dup(vfcvt_xu_f),
      VFCVT_X_F_V       -> dup(vfcvt_x_f),
      VFCVT_F_XU_V      -> dup(vfcvt_f_xu),
      VFCVT_F_X_V       -> dup(vfcvt_f_x),
      VFCVT_RTZ_XU_F_V  -> dup(vfcvt_xu_f_rtz),
      VFCVT_RTZ_X_F_V   -> dup(vfcvt_x_f_rtz),

      VFWCVT_XU_F_V     -> dupF2W(vfwcvt_xu_f),
      VFWCVT_X_F_V      -> dupF2W(vfwcvt_x_f),
      VFWCVT_F_XU_V     -> dupF2W(vfwcvt_f_xu),
      VFWCVT_F_X_V      -> dupF2W(vfwcvt_f_x),
      VFWCVT_F_F_V      -> dupF2W(vfwcvt_f_f),
      VFWCVT_RTZ_XU_F_V -> dupF2W(vfwcvt_xu_f_rtz),
      VFWCVT_RTZ_X_F_V  -> dupF2W(vfwcvt_x_f_rtz),

      VFNCVT_XU_F_W     -> dupF2N(vfncvt_xu_f),
      VFNCVT_X_F_W      -> dupF2N(vfncvt_x_f),
      VFNCVT_F_XU_W     -> dupF2N(vfncvt_f_xu),
      VFNCVT_F_X_W      -> dupF2N(vfncvt_f_x),
      VFNCVT_F_F_W      -> dupF2N(vfncvt_f_f),
      VFNCVT_ROD_F_F_W  -> dupF2N(vfncvt_f_f_rod),
      VFNCVT_RTZ_XU_F_W -> dupF2N(vfncvt_xu_f_rtz),
      VFNCVT_RTZ_X_F_W  -> dupF2N(vfncvt_x_f_rtz),

      // VFUNARY1
      VFSQRT_V          -> dup(vfsqrt),
      VFRSQRT7_V        -> dup(vfrsqrt7),
      VFREC7_V          -> dup(vfrec7),
      VFCLASS_V         -> dup(vfclass),
      VFMERGE_VFM       -> dup(vfmerge.s1f),
      VFMV_V_F          -> dup(vmvFp2Vec),

      VMFEQ_VV          -> dup(vmfeq.s1v),
      VMFEQ_VF          -> dup(vmfeq.s1f),
      VMFLE_VV          -> dup(vmfle.s1v),
      VMFLE_VF          -> dup(vmfle.s1f),
      VMFLT_VV          -> dup(vmflt.s1v),
      VMFLT_VF          -> dup(vmflt.s1f),
      VMFNE_VV          -> dup(vmfne.s1v),
      VMFNE_VF          -> dup(vmfne.s1f),
      VMFGT_VF          -> dup(vmfgt.s1f),
      VMFGE_VF          -> dup(vmfge.s1f),
    )

    val opf10Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      VFDIV_VV          -> dup(vfdiv.s1v),
      VFDIV_VF          -> dup(vfdiv.s1f),
      VFRDIV_VF         -> dup(vfdiv.s1f.setSrc12Rev),
      VFMUL_VV          -> dup(vfmul.s1v),
      VFMUL_VF          -> dup(vfmul.s1f),

      VFMADD_VV         -> dup(vfmadd.s1v),
      VFMADD_VF         -> dup(vfmadd.s1f),
      VFNMADD_VV        -> dup(vfnmadd.s1v),
      VFNMADD_VF        -> dup(vfnmadd.s1f),
      VFMSUB_VV         -> dup(vfmsub.s1v),
      VFMSUB_VF         -> dup(vfmsub.s1f),
      VFNMSUB_VV        -> dup(vfnmsub.s1v),
      VFNMSUB_VF        -> dup(vfnmsub.s1f),
      VFMACC_VV         -> dup(vfmacc.s1v),
      VFMACC_VF         -> dup(vfmacc.s1f),
      VFNMACC_VV        -> dup(vfnmacc.s1v),
      VFNMACC_VF        -> dup(vfnmacc.s1f),
      VFMSAC_VV         -> dup(vfmsac.s1v),
      VFMSAC_VF         -> dup(vfmsac.s1f),
      VFNMSAC_VV        -> dup(vfnmsac.s1v),
      VFNMSAC_VF        -> dup(vfnmsac.s1f),
    )

    val opf11Table = SeqMap[BitPat, SeqMap[LmulPattern, Seq[VecUop]]](
      VFWADD_VV         -> dupF2W(vfwadd.s1v),
      VFWADD_VF         -> dupF2W(vfwadd.s1f),
      VFWSUB_VV         -> dupF2W(vfwsub.s1v),
      VFWSUB_VF         -> dupF2W(vfwsub.s1f),
      VFWADD_WV         -> dupF2W(vfwadd_w.s1v),
      VFWADD_WF         -> dupF2W(vfwadd_w.s1f),
      VFWSUB_WV         -> dupF2W(vfwsub_w.s1v),
      VFWSUB_WF         -> dupF2W(vfwsub_w.s1f),
      VFWMUL_VV         -> dupF2W(vfwmul.s1v),
      VFWMUL_VF         -> dupF2W(vfwmul.s1f),
      VFWMACC_VV        -> dupF2W(vfwmacc.s1v),
      VFWMACC_VF        -> dupF2W(vfwmacc.s1f),
      VFWNMACC_VV       -> dupF2W(vfwnmacc.s1v),
      VFWNMACC_VF       -> dupF2W(vfwnmacc.s1f),
      VFWMSAC_VV        -> dupF2W(vfwmsac.s1v),
      VFWMSAC_VF        -> dupF2W(vfwmsac.s1f),
      VFWNMSAC_VV       -> dupF2W(vfwnmsac.s1v),
      VFWNMSAC_VF       -> dupF2W(vfwnmsac.s1f),
    )

    opi00Table ++ opi01Table ++ opi10Table ++ opi11Table ++
    opm00Table ++ opm01Table ++ opm10Table ++ opm11Table ++
    opf00Table ++ opf01Table ++ opf10Table ++ opf11Table
  }

  val opiDupTable: Map[BitPat, VecIntUop] = Map(
    VADD_VV  -> vadd.s1v,
    VADD_VX  -> vadd.s1x,
    VADD_VI  -> vadd.s1i,
    VSUB_VV  -> vsub.s1v,
    VSUB_VX  -> vsub.s1x,
    VRSUB_VX -> vsub.s1x.setSrc12Rev,
    VRSUB_VI -> vsub.s1i.setSrc12Rev,
    VMINU_VV -> vminu.s1v,
    VMINU_VX -> vminu.s1x,
    VMIN_VV  -> vmin.s1v,
    VMIN_VX  -> vmin.s1x,
    VMAXU_VV -> vmaxu.s1v,
    VMAXU_VX -> vmaxu.s1x,
    VMAX_VV  -> vmax.s1v,
    VMAX_VX  -> vmax.s1x,
    VAND_VV  -> vand.s1v,
    VAND_VX  -> vand.s1x,
    VAND_VI  -> vand.s1i,
    VOR_VV   -> vor.s1v,
    VOR_VX   -> vor.s1x,
    VOR_VI   -> vor.s1i,
    VXOR_VV  -> vxor.s1v,
    VXOR_VX  -> vxor.s1x,
    VXOR_VI  -> vxor.s1i,

    VADC_VVM   -> vadc.s1v,
    VADC_VXM   -> vadc.s1x,
    VADC_VIM   -> vadc.s1i,
    VMADC_VV   -> vmadc.s1v,
    VMADC_VX   -> vmadc.s1x,
    VMADC_VI   -> vmadc.s1i,
    VMADC_VVM  -> vmadc.s1v,
    VMADC_VXM  -> vmadc.s1x,
    VMADC_VIM  -> vmadc.s1i,
    VSBC_VVM   -> vsbc.s1v,
    VSBC_VXM   -> vsbc.s1x,
    VMSBC_VV   -> vmsbc.s1v,
    VMSBC_VX   -> vmsbc.s1x,
    VMSBC_VVM  -> vmsbc.s1v,
    VMSBC_VXM  -> vmsbc.s1x,
    VMERGE_VVM -> vmerge.s1v,
    VMERGE_VXM -> vmerge.s1x,
    VMERGE_VIM -> vmerge.s1i,
    VMV_V_V    -> vmvVec2Vec,
    VMV_V_X    -> vmvInt2Vec,
    VMV_V_I    -> vmvImm2Vec,
    VMSEQ_VV   -> vmseq.s1v,
    VMSEQ_VX   -> vmseq.s1x,
    VMSEQ_VI   -> vmseq.s1i,
    VMSNE_VV   -> vmsne.s1v,
    VMSNE_VX   -> vmsne.s1x,
    VMSNE_VI   -> vmsne.s1i,
    VMSLTU_VV  -> vmsltu.s1v,
    VMSLTU_VX  -> vmsltu.s1x,
    VMSLT_VV   -> vmslt.s1v,
    VMSLT_VX   -> vmslt.s1x,
    VMSLEU_VV  -> vmsleu.s1v,
    VMSLEU_VX  -> vmsleu.s1x,
    VMSLEU_VI  -> vmsleu.s1i,
    VMSLE_VV   -> vmsle.s1v,
    VMSLE_VX   -> vmsle.s1x,
    VMSLE_VI   -> vmsle.s1i,
    VMSGTU_VX  -> vmsgtu.s1x,
    VMSGTU_VI  -> vmsgtu.s1i,
    VMSGT_VX   -> vmsgt.s1x,
    VMSGT_VI   -> vmsgt.s1i,
    VSADDU_VV  -> vsaddu.s1v,
    VSADDU_VX  -> vsaddu.s1x,
    VSADDU_VI  -> vsaddu.s1i,
    VSADD_VV   -> vsadd.s1v,
    VSADD_VX   -> vsadd.s1x,
    VSADD_VI   -> vsadd.s1i,
    VSSUBU_VV  -> vssubu.s1v,
    VSSUBU_VX  -> vssubu.s1x,
    VSSUB_VV   -> vssub.s1v,
    VSSUB_VX   -> vssub.s1x,
    VSLL_VV    -> vsll.s1v,
    VSLL_VX    -> vsll.s1x,
    VSLL_VI    -> vsll.s1i,
    VSMUL_VV   -> vsmul.s1v,
    VSMUL_VX   -> vsmul.s1x,
    VSRL_VV    -> vsrl.s1v,
    VSRL_VX    -> vsrl.s1x,
    VSRL_VI    -> vsrl.s1i,
    VSRA_VV    -> vsra.s1v,
    VSRA_VX    -> vsra.s1x,
    VSRA_VI    -> vsra.s1i,
    VSSRL_VV   -> vssrl.s1v,
    VSSRL_VX   -> vssrl.s1x,
    VSSRL_VI   -> vssrl.s1i,
    VSSRA_VV   -> vssra.s1v,
    VSSRA_VX   -> vssra.s1x,
    VSSRA_VI   -> vssra.s1i,
  )

  val opiLmulUnrelatedTable: Map[BitPat, Seq[VecUop]] = Map(
    VMV1R_V    -> Seq.fill(1)(vmvnr),
    VMV2R_V    -> Seq.fill(2)(vmvnr),
    VMV4R_V    -> Seq.fill(4)(vmvnr),
    VMV8R_V    -> Seq.fill(8)(vmvnr),
  )

  val opiTableF2: Map[BitPat, Seq[VecUop]] = Map(
    VNSRL_WV   -> Seq.fill(1)(vnsrl.s1v),
    VNSRL_WX   -> Seq.fill(1)(vnsrl.s1x),
    VNSRL_WI   -> Seq.fill(1)(vnsrl.s1i),
    VNSRA_WV   -> Seq.fill(1)(vnsra.s1v),
    VNSRA_WX   -> Seq.fill(1)(vnsra.s1x),
    VNSRA_WI   -> Seq.fill(1)(vnsra.s1i),
    VNCLIPU_WV -> Seq.fill(1)(vnclipu.s1v),
    VNCLIPU_WX -> Seq.fill(1)(vnclipu.s1x),
    VNCLIPU_WI -> Seq.fill(1)(vnclipu.s1i),
    VNCLIP_WV  -> Seq.fill(1)(vnclip.s1v),
    VNCLIP_WX  -> Seq.fill(1)(vnclip.s1x),
    VNCLIP_WI  -> Seq.fill(1)(vnclip.s1i),
  )

  val opiTable1: Map[BitPat, Seq[VecUop]] = Map(
    VNSRL_WV   -> Seq.fill(2)(vnsrl.s1v),
    VNSRL_WX   -> Seq.fill(2)(vnsrl.s1x),
    VNSRL_WI   -> Seq.fill(2)(vnsrl.s1i),
    VNSRA_WV   -> Seq.fill(2)(vnsra.s1v),
    VNSRA_WX   -> Seq.fill(2)(vnsra.s1x),
    VNSRA_WI   -> Seq.fill(2)(vnsra.s1i),
    VNCLIPU_WV -> Seq.fill(2)(vnclipu.s1v),
    VNCLIPU_WX -> Seq.fill(2)(vnclipu.s1x),
    VNCLIPU_WI -> Seq.fill(2)(vnclipu.s1i),
    VNCLIP_WV  -> Seq.fill(2)(vnclip.s1v),
    VNCLIP_WX  -> Seq.fill(2)(vnclip.s1x),
    VNCLIP_WI  -> Seq.fill(2)(vnclip.s1i),

    VRGATHER_VV -> Seq.fill(1)(vrgather_v),
    VRGATHER_VX -> Seq.fill(1)(vrgather_x),
    VRGATHER_VI -> Seq.fill(1)(vrgather_i),
    VSLIDEUP_VX -> Seq.fill(1)(vslideup_x),
    VSLIDEUP_VI -> Seq.fill(1)(vslideup_i),
    VRGATHEREI16_VV -> Seq.fill(1)(vrgatherei16_v),
    VSLIDEDOWN_VX   -> Seq.fill(1)(vslidedown_x),
    VSLIDEDOWN_VI   -> Seq.fill(1)(vslidedown_i),

    VWREDSUMU_VS -> Seq[VecUop](
      vwaddu.s1v.set(_.maskType, Src12Mask),
      vwredsumu.set(_.maskType, NoMask),
    ),
    VWREDSUM_VS -> Seq[VecUop](
      vwadd.s1v.set(_.maskType, Src12Mask),
      vwredsum.set(_.maskType, NoMask),
    )
  )

  val opiTable2: Map[BitPat, Seq[VecUop]] = Map(
    VNSRL_WV   -> Seq.fill(4)(vnsrl.s1v),
    VNSRL_WX   -> Seq.fill(4)(vnsrl.s1x),
    VNSRL_WI   -> Seq.fill(4)(vnsrl.s1i),
    VNSRA_WV   -> Seq.fill(4)(vnsra.s1v),
    VNSRA_WX   -> Seq.fill(4)(vnsra.s1x),
    VNSRA_WI   -> Seq.fill(4)(vnsra.s1i),
    VNCLIPU_WV -> Seq.fill(4)(vnclipu.s1v),
    VNCLIPU_WX -> Seq.fill(4)(vnclipu.s1x),
    VNCLIPU_WI -> Seq.fill(4)(vnclipu.s1i),
    VNCLIP_WV  -> Seq.fill(4)(vnclip.s1v),
    VNCLIP_WX  -> Seq.fill(4)(vnclip.s1x),
    VNCLIP_WI  -> Seq.fill(4)(vnclip.s1i),

    VRGATHER_VV -> Seq.fill(2)(vrgather_v),
    VRGATHER_VX -> Seq.fill(2)(vrgather_x),
    VRGATHER_VI -> Seq.fill(2)(vrgather_i),
    VSLIDEUP_VX -> Seq.fill(2)(vslideup_x),
    VSLIDEUP_VI -> Seq.fill(2)(vslideup_i),
    VRGATHEREI16_VV -> Seq.fill(2)(vrgatherei16_v),
    VSLIDEDOWN_VX -> Seq.fill(2)(vslidedown_x),
    VSLIDEDOWN_VI -> Seq.fill(2)(vslidedown_i),
    VWREDSUMU_VS -> Seq[VecUop](
      vwaddu.s1v.set(_.maskType, Src12Mask),
      vwaddu_w.s1v.set(_.maskType, Src2Mask),
      vwaddu_w.s1v.set(_.maskType, Src2Mask),
      vwredsumu.set(_.maskType, NoMask),
    ),
    VWREDSUM_VS -> Seq[VecUop](
      vwadd.s1v.set(_.maskType, Src12Mask),
      vwadd_w.s1v.set(_.maskType, Src2Mask),
      vwadd_w.s1v.set(_.maskType, Src2Mask),
      vwredsum.set(_.maskType, NoMask),
    ),
  )

  val opiTable4: Map[BitPat, Seq[VecUop]] = Map(
    VNSRL_WV   -> Seq.fill(8)(vnsrl.s1v),
    VNSRL_WX   -> Seq.fill(8)(vnsrl.s1x),
    VNSRL_WI   -> Seq.fill(8)(vnsrl.s1i),
    VNSRA_WV   -> Seq.fill(8)(vnsra.s1v),
    VNSRA_WX   -> Seq.fill(8)(vnsra.s1x),
    VNSRA_WI   -> Seq.fill(8)(vnsra.s1i),
    VNCLIPU_WV -> Seq.fill(8)(vnclipu.s1v),
    VNCLIPU_WX -> Seq.fill(8)(vnclipu.s1x),
    VNCLIPU_WI -> Seq.fill(8)(vnclipu.s1i),
    VNCLIP_WV  -> Seq.fill(8)(vnclip.s1v),
    VNCLIP_WX  -> Seq.fill(8)(vnclip.s1x),
    VNCLIP_WI  -> Seq.fill(8)(vnclip.s1i),

    VRGATHER_VV     -> Seq.fill(4)(vrgather_v),
    VRGATHER_VX     -> Seq.fill(4)(vrgather_x),
    VRGATHER_VI     -> Seq.fill(4)(vrgather_i),
    VSLIDEUP_VX     -> Seq.fill(4)(vslideup_x),
    VSLIDEUP_VI     -> Seq.fill(4)(vslideup_i),
    VRGATHEREI16_VV -> Seq.fill(4)(vrgatherei16_v),
    VSLIDEDOWN_VX   -> Seq.fill(4)(vslidedown_x),
    VSLIDEDOWN_VI   -> Seq.fill(4)(vslidedown_i),
    VWREDSUMU_VS    -> Seq[VecUop](
      vwaddu.s1v.set(_.maskType, Src12Mask),
      vwaddu_w.s1v.set(_.maskType, Src2Mask),
      vwaddu_w.s1v.set(_.maskType, Src2Mask),
      vwaddu_w.s1v.set(_.maskType, Src2Mask),
      vwaddu_w.s1v.set(_.maskType, Src2Mask),
      vwaddu_w.s1v.set(_.maskType, Src2Mask),
      vwaddu_w.s1v.set(_.maskType, Src2Mask),
      vwredsumu.set(_.maskType, NoMask),
    ),
    VWREDSUM_VS -> Seq[VecUop](
      vwadd.s1v.set(_.maskType, Src12Mask),
      vwadd_w.s1v.set(_.maskType, Src2Mask),
      vwadd_w.s1v.set(_.maskType, Src2Mask),
      vwadd_w.s1v.set(_.maskType, Src2Mask),
      vwadd_w.s1v.set(_.maskType, Src2Mask),
      vwadd_w.s1v.set(_.maskType, Src2Mask),
      vwadd_w.s1v.set(_.maskType, Src2Mask),
      vwredsum.set(_.maskType, NoMask),
    ),
  )

  val opiTable8: Map[BitPat, Seq[VecUop]] = Map(
    VNSRL_WV   -> Seq[VecUop](),
    VNSRL_WX   -> Seq(),
    VNSRL_WI   -> Seq(),
    VNSRA_WV   -> Seq(),
    VNSRA_WX   -> Seq(),
    VNSRA_WI   -> Seq(),
    VNCLIPU_WV -> Seq(),
    VNCLIPU_WX -> Seq(),
    VNCLIPU_WI -> Seq(),
    VNCLIP_WV  -> Seq(),
    VNCLIP_WX  -> Seq(),
    VNCLIP_WI  -> Seq(),

    VRGATHER_VV     -> Seq.fill(8)(vrgather_v),
    VRGATHER_VX     -> Seq.fill(8)(vrgather_x),
    VRGATHER_VI     -> Seq.fill(8)(vrgather_i),
    VSLIDEUP_VX     -> Seq.fill(8)(vslideup_x),
    VSLIDEUP_VI     -> Seq.fill(8)(vslideup_i),
    VRGATHEREI16_VV -> Seq.fill(8)(vrgatherei16_v),
    VSLIDEDOWN_VX   -> Seq.fill(8)(vslidedown_x),
    VSLIDEDOWN_VI   -> Seq.fill(8)(vslidedown_i),
    VWREDSUMU_VS    -> Seq(),
    VWREDSUM_VS     -> Seq(),
  )

  val opmDupTable: Map[BitPat, VecUop] = Map(
    VAADDU_VV   -> vaaddu.s1v,
    VAADDU_VX   -> vaaddu.s1x,
    VAADD_VV    -> vaadd.s1v,
    VAADD_VX    -> vaadd.s1x,
    VASUBU_VV   -> vasubu.s1v,
    VASUBU_VX   -> vasubu.s1x,
    VASUB_VV    -> vasub.s1v,
    VASUB_VX    -> vasub.s1x,
    // VXUNARY0
    VZEXT_VF8   -> vzext8,
    VZEXT_VF4   -> vzext4,
    VZEXT_VF2   -> vzext2,
    VSEXT_VF8   -> vsext8,
    VSEXT_VF4   -> vsext4,
    VSEXT_VF2   -> vsext2,

    VDIVU_VV    -> vdivu.s1v,
    VDIVU_VX    -> vdivu.s1x,
    VDIV_VV     -> vdiv.s1v,
    VDIV_VX     -> vdiv.s1x,
    VREMU_VV    -> vremu.s1v,
    VREMU_VX    -> vremu.s1x,
    VREM_VV     -> vrem.s1v,
    VREM_VX     -> vrem.s1x,

    VMULHU_VV   -> vmulhu.s1v,
    VMULHU_VX   -> vmulhu.s1x,
    VMUL_VV     -> vmul.s1v,
    VMUL_VX     -> vmul.s1x,
    VMULHSU_VV  -> vmulhsu.s1v,
    VMULHSU_VX  -> vmulhsu.s1x,
    VMULH_VV    -> vmulh.s1v,
    VMULH_VX    -> vmulh.s1x,

    VMADD_VV    -> vmadd.s1v,
    VMADD_VX    -> vmadd.s1x,
    VNMSUB_VV   -> vnmsub.s1v,
    VNMSUB_VX   -> vnmsub.s1x,
    VMACC_VV    -> vmacc.s1v,
    VMACC_VX    -> vmacc.s1x,
    VNMSAC_VV   -> vnmsac.s1v,
    VNMSAC_VX   -> vnmsac.s1x,
  )

  val opmLmulUnrelatedTable: Map[BitPat, Seq[VecUop]] = Map(
    // VWXUNARY0
    VMV_X_S     -> Seq[VecUop](vmvVecScala2Int),
    VCPOP_M     -> Seq(vcpop_m),
    VFIRST_M    -> Seq(vfirst),
    // VMUNARY0
    VMSBF_M     -> Seq(vmsbf),
    VMSOF_M     -> Seq(vmsof),
    VMSIF_M     -> Seq(vmsif),
    VIOTA_M     -> Seq(viota),
    VID_V       -> Seq(vid),

    VMANDN_MM   -> Seq(vmandn),
    VMAND_MM    -> Seq(vmand),
    VMOR_MM     -> Seq(vmor),
    VMXOR_MM    -> Seq(vmxor),
    VMORN_MM    -> Seq(vmorn),
    VMNAND_MM   -> Seq(vmnand),
    VMNOR_MM    -> Seq(vmnor),
    VMXNOR_MM   -> Seq(vmxnor),
  )

  val opmTableF2: Map[BitPat, Seq[VecUop]] = Map(
    VWADDU_VV   -> Seq.fill[VecUop](1)(vwaddu.s1v),
    VWADDU_VX   -> Seq.fill(1)(vwaddu.s1x),
    VWADD_VV    -> Seq.fill(1)(vwadd.s1v),
    VWADD_VX    -> Seq.fill(1)(vwadd.s1x),
    VWSUBU_VV   -> Seq.fill(1)(vwsubu.s1v),
    VWSUBU_VX   -> Seq.fill(1)(vwsubu.s1x),
    VWSUB_VV    -> Seq.fill(1)(vwsub.s1v),
    VWSUB_VX    -> Seq.fill(1)(vwsub.s1x),
    VWADDU_WV   -> Seq.fill(1)(vwaddu_w.s1v),
    VWADDU_WX   -> Seq.fill(1)(vwaddu_w.s1x),
    VWADD_WV    -> Seq.fill(1)(vwadd_w.s1v),
    VWADD_WX    -> Seq.fill(1)(vwadd_w.s1x),
    VWSUBU_WV   -> Seq.fill(1)(vwsubu_w.s1v),
    VWSUBU_WX   -> Seq.fill(1)(vwsubu_w.s1x),
    VWSUB_WV    -> Seq.fill(1)(vwsub_w.s1v),
    VWSUB_WX    -> Seq.fill(1)(vwsub_w.s1x),
    VWMULU_VV   -> Seq.fill(1)(vwmulu.s1v),
    VWMULU_VX   -> Seq.fill(1)(vwmulu.s1x),
    VWMULSU_VV  -> Seq.fill(1)(vwmulsu.s1v),
    VWMULSU_VX  -> Seq.fill(1)(vwmulsu.s1x),
    VWMUL_VV    -> Seq.fill(1)(vwmul.s1v),
    VWMUL_VX    -> Seq.fill(1)(vwmul.s1x),
    VWMACCU_VV  -> Seq.fill(1)(vwmaccu.s1v),
    VWMACCU_VX  -> Seq.fill(1)(vwmaccu.s1x),
    VWMACC_VV   -> Seq.fill(1)(vwmacc.s1v),
    VWMACC_VX   -> Seq.fill(1)(vwmacc.s1x),
    VWMACCUS_VX -> Seq.fill(1)(vwmaccus.s1x),
    VWMACCSU_VV -> Seq.fill(1)(vwmaccsu.s1v),
    VWMACCSU_VX -> Seq.fill(1)(vwmaccsu.s1x),
  )

  val opmTable1: Map[BitPat, Seq[VecUop]] = Map(
    // VRXUNARY0
    VMV_S_X     -> Seq[VecUop](vmvInt2VecScala),

    VWADDU_VV   -> Seq.fill(2)(vwaddu.s1v),
    VWADDU_VX   -> Seq.fill(2)(vwaddu.s1x),
    VWADD_VV    -> Seq.fill(2)(vwadd.s1v),
    VWADD_VX    -> Seq.fill(2)(vwadd.s1x),
    VWSUBU_VV   -> Seq.fill(2)(vwsubu.s1v),
    VWSUBU_VX   -> Seq.fill(2)(vwsubu.s1x),
    VWSUB_VV    -> Seq.fill(2)(vwsub.s1v),
    VWSUB_VX    -> Seq.fill(2)(vwsub.s1x),
    VWADDU_WV   -> Seq.fill(2)(vwaddu_w.s1v),
    VWADDU_WX   -> Seq.fill(2)(vwaddu_w.s1x),
    VWADD_WV    -> Seq.fill(2)(vwadd_w.s1v),
    VWADD_WX    -> Seq.fill(2)(vwadd_w.s1x),
    VWSUBU_WV   -> Seq.fill(2)(vwsubu_w.s1v),
    VWSUBU_WX   -> Seq.fill(2)(vwsubu_w.s1x),
    VWSUB_WV    -> Seq.fill(2)(vwsub_w.s1v),
    VWSUB_WX    -> Seq.fill(2)(vwsub_w.s1x),
    VWMULU_VV   -> Seq.fill(2)(vwmulu.s1v),
    VWMULU_VX   -> Seq.fill(2)(vwmulu.s1x),
    VWMULSU_VV  -> Seq.fill(2)(vwmulsu.s1v),
    VWMULSU_VX  -> Seq.fill(2)(vwmulsu.s1x),
    VWMUL_VV    -> Seq.fill(2)(vwmul.s1v),
    VWMUL_VX    -> Seq.fill(2)(vwmul.s1x),
    VWMACCU_VV  -> Seq.fill(2)(vwmaccu.s1v),
    VWMACCU_VX  -> Seq.fill(2)(vwmaccu.s1x),
    VWMACC_VV   -> Seq.fill(2)(vwmacc.s1v),
    VWMACC_VX   -> Seq.fill(2)(vwmacc.s1x),
    VWMACCUS_VX -> Seq.fill(2)(vwmaccus.s1x),
    VWMACCSU_VV -> Seq.fill(2)(vwmaccsu.s1v),
    VWMACCSU_VX -> Seq.fill(2)(vwmaccsu.s1x),

    VREDSUM_VS      -> Seq(vredsum.set(_.maskType, Src2Mask)),
    VREDAND_VS      -> Seq(vredand.set(_.maskType, Src2Mask)),
    VREDOR_VS       -> Seq(vredor.set(_.maskType, Src2Mask)),
    VREDXOR_VS      -> Seq(vredxor.set(_.maskType, Src2Mask)),
    VREDMINU_VS     -> Seq(vredminu.set(_.maskType, Src2Mask)),
    VREDMIN_VS      -> Seq(vredmin.set(_.maskType, Src2Mask)),
    VREDMAXU_VS     -> Seq(vredmaxu.set(_.maskType, Src2Mask)),
    VREDMAX_VS      -> Seq(vredmax.set(_.maskType, Src2Mask)),
    VSLIDE1UP_VX    -> Seq(vslide1up.s1x),
    VSLIDE1DOWN_VX  -> Seq(vslide1down.s1x),
    VCOMPRESS_VM    -> Seq(vcompress_m),
  )

  val opmTable2: Map[BitPat, Seq[VecUop]] = Map(
    // VRXUNARY0
    VMV_S_X         -> Seq[VecUop](
      vmvInt2VecScala,
      vtail,
    ),

    VWADDU_VV   -> Seq.fill(4)(vwaddu.s1v),
    VWADDU_VX   -> Seq.fill(4)(vwaddu.s1x),
    VWADD_VV    -> Seq.fill(4)(vwadd.s1v),
    VWADD_VX    -> Seq.fill(4)(vwadd.s1x),
    VWSUBU_VV   -> Seq.fill(4)(vwsubu.s1v),
    VWSUBU_VX   -> Seq.fill(4)(vwsubu.s1x),
    VWSUB_VV    -> Seq.fill(4)(vwsub.s1v),
    VWSUB_VX    -> Seq.fill(4)(vwsub.s1x),
    VWADDU_WV   -> Seq.fill(4)(vwaddu_w.s1v),
    VWADDU_WX   -> Seq.fill(4)(vwaddu_w.s1x),
    VWADD_WV    -> Seq.fill(4)(vwadd_w.s1v),
    VWADD_WX    -> Seq.fill(4)(vwadd_w.s1x),
    VWSUBU_WV   -> Seq.fill(4)(vwsubu_w.s1v),
    VWSUBU_WX   -> Seq.fill(4)(vwsubu_w.s1x),
    VWSUB_WV    -> Seq.fill(4)(vwsub_w.s1v),
    VWSUB_WX    -> Seq.fill(4)(vwsub_w.s1x),
    VWMULU_VV   -> Seq.fill(4)(vwmulu.s1v),
    VWMULU_VX   -> Seq.fill(4)(vwmulu.s1x),
    VWMULSU_VV  -> Seq.fill(4)(vwmulsu.s1v),
    VWMULSU_VX  -> Seq.fill(4)(vwmulsu.s1x),
    VWMUL_VV    -> Seq.fill(4)(vwmul.s1v),
    VWMUL_VX    -> Seq.fill(4)(vwmul.s1x),
    VWMACCU_VV  -> Seq.fill(4)(vwmaccu.s1v),
    VWMACCU_VX  -> Seq.fill(4)(vwmaccu.s1x),
    VWMACC_VV   -> Seq.fill(4)(vwmacc.s1v),
    VWMACC_VX   -> Seq.fill(4)(vwmacc.s1x),
    VWMACCUS_VX -> Seq.fill(4)(vwmaccus.s1x),
    VWMACCSU_VV -> Seq.fill(4)(vwmaccsu.s1v),
    VWMACCSU_VX -> Seq.fill(4)(vwmaccsu.s1x),

    VREDSUM_VS      -> Seq(
      vadd.s1v.set(_.maskType, Src12Mask),
      vredsum.set(_.maskType, NoMask),
    ),
    VREDAND_VS      -> Seq(
      vand.s1v.set(_.maskType, Src12Mask),
      vredand.set(_.maskType, NoMask),
    ),
    VREDOR_VS       -> Seq(
      vor.s1v.set(_.maskType, Src12Mask),
      vredor.set(_.maskType, NoMask),
    ),
    VREDXOR_VS      -> Seq(
      vxor.s1v.set(_.maskType, Src12Mask),
      vredxor.set(_.maskType, NoMask),
    ),
    VREDMINU_VS     -> Seq(
      vminu.s1v.set(_.maskType, Src12Mask),
      vredminu.set(_.maskType, NoMask),
    ),
    VREDMIN_VS      -> Seq(
      vmin.s1v.set(_.maskType, Src12Mask),
      vredmin.set(_.maskType, NoMask),
    ),
    VREDMAXU_VS     -> Seq(
      vmaxu.s1v.set(_.maskType, Src12Mask),
      vredmaxu.set(_.maskType, NoMask),
    ),
    VREDMAX_VS      -> Seq(
      vmax.s1v.set(_.maskType, Src12Mask),
      vredmax.set(_.maskType, NoMask),
    ),
    VSLIDE1UP_VX    -> Seq(
      vslide1up.s1x,
      vslide1up.s1x,
    ),
    VSLIDE1DOWN_VX  -> Seq(
      vslide1down.s1x,
      vslide1down.s1x,
    ),
    VCOMPRESS_VM    -> Seq(
      vcompress_m,
      vcompress_m,
    ),
  )

  val opmTable4: Map[BitPat, Seq[VecUop]] = Map(
    VMV_S_X         -> (Seq(
      vmvInt2VecScala,
    ) ++ Seq.fill(3)(vtail)),

    VWADDU_VV   -> Seq.fill(8)(vwaddu.s1v),
    VWADDU_VX   -> Seq.fill(8)(vwaddu.s1x),
    VWADD_VV    -> Seq.fill(8)(vwadd.s1v),
    VWADD_VX    -> Seq.fill(8)(vwadd.s1x),
    VWSUBU_VV   -> Seq.fill(8)(vwsubu.s1v),
    VWSUBU_VX   -> Seq.fill(8)(vwsubu.s1x),
    VWSUB_VV    -> Seq.fill(8)(vwsub.s1v),
    VWSUB_VX    -> Seq.fill(8)(vwsub.s1x),
    VWADDU_WV   -> Seq.fill(8)(vwaddu_w.s1v),
    VWADDU_WX   -> Seq.fill(8)(vwaddu_w.s1x),
    VWADD_WV    -> Seq.fill(8)(vwadd_w.s1v),
    VWADD_WX    -> Seq.fill(8)(vwadd_w.s1x),
    VWSUBU_WV   -> Seq.fill(8)(vwsubu_w.s1v),
    VWSUBU_WX   -> Seq.fill(8)(vwsubu_w.s1x),
    VWSUB_WV    -> Seq.fill(8)(vwsub_w.s1v),
    VWSUB_WX    -> Seq.fill(8)(vwsub_w.s1x),
    VWMULU_VV   -> Seq.fill(8)(vwmulu.s1v),
    VWMULU_VX   -> Seq.fill(8)(vwmulu.s1x),
    VWMULSU_VV  -> Seq.fill(8)(vwmulsu.s1v),
    VWMULSU_VX  -> Seq.fill(8)(vwmulsu.s1x),
    VWMUL_VV    -> Seq.fill(8)(vwmul.s1v),
    VWMUL_VX    -> Seq.fill(8)(vwmul.s1x),
    VWMACCU_VV  -> Seq.fill(8)(vwmaccu.s1v),
    VWMACCU_VX  -> Seq.fill(8)(vwmaccu.s1x),
    VWMACC_VV   -> Seq.fill(8)(vwmacc.s1v),
    VWMACC_VX   -> Seq.fill(8)(vwmacc.s1x),
    VWMACCUS_VX -> Seq.fill(8)(vwmaccus.s1x),
    VWMACCSU_VV -> Seq.fill(8)(vwmaccsu.s1v),
    VWMACCSU_VX -> Seq.fill(8)(vwmaccsu.s1x),

    VREDSUM_VS      -> Seq[VecUop](
      vadd.s1v.set(_.maskType, Src12Mask),
      vadd.s1v.set(_.maskType, Src2Mask),
      vadd.s1v.set(_.maskType, Src2Mask),
      vredsum.set(_.maskType, NoMask),
    ),
    VREDAND_VS      -> Seq(
      vand.s1v.set(_.maskType, Src12Mask),
      vand.s1v.set(_.maskType, Src2Mask),
      vand.s1v.set(_.maskType, Src2Mask),
      vredand.set(_.maskType, NoMask),
    ),
    VREDOR_VS       -> Seq(
      vor.s1v.set(_.maskType, Src12Mask),
      vor.s1v.set(_.maskType, Src2Mask),
      vor.s1v.set(_.maskType, Src2Mask),
      vredor.set(_.maskType, NoMask),
    ),
    VREDXOR_VS      -> Seq(
      vxor.s1v.set(_.maskType, Src12Mask),
      vxor.s1v.set(_.maskType, Src2Mask),
      vxor.s1v.set(_.maskType, Src2Mask),
      vredxor.set(_.maskType, NoMask),
    ),
    VREDMINU_VS     -> Seq(
      vminu.s1v.set(_.maskType, Src12Mask),
      vminu.s1v.set(_.maskType, Src2Mask),
      vminu.s1v.set(_.maskType, Src2Mask),
      vredminu.set(_.maskType, NoMask),
    ),
    VREDMIN_VS      -> Seq(
      vmin.s1v.set(_.maskType, Src12Mask),
      vmin.s1v.set(_.maskType, Src2Mask),
      vmin.s1v.set(_.maskType, Src2Mask),
      vredmin.set(_.maskType, NoMask),
    ),
    VREDMAXU_VS     -> Seq(
      vmaxu.s1v.set(_.maskType, Src12Mask),
      vmaxu.s1v.set(_.maskType, Src2Mask),
      vmaxu.s1v.set(_.maskType, Src2Mask),
      vredmaxu.set(_.maskType, NoMask),
    ),
    VREDMAX_VS      -> Seq(
      vmax.s1v.set(_.maskType, Src12Mask),
      vmax.s1v.set(_.maskType, Src2Mask),
      vmax.s1v.set(_.maskType, Src2Mask),
      vredmax.set(_.maskType, NoMask),
    ),
    VSLIDE1UP_VX    -> Seq(
      vslide1up.s1x,
      vslide1up.s1x,
      vslide1up.s1x,
      vslide1up.s1x,
    ),
    VSLIDE1DOWN_VX  -> Seq(
      vslide1down.s1x,
      vslide1down.s1x,
      vslide1down.s1x,
      vslide1down.s1x,
    ),
    VCOMPRESS_VM    -> Seq(
      vcompress_m,
      vcompress_m,
      vcompress_m,
      vcompress_m,
    ),
  )

  val opmTable8: Map[BitPat, Seq[VecUop]] = Map(
    VMV_S_X         -> (Seq(
      vmvInt2VecScala,
    ) ++ Seq.fill(7)(vtail)),
    VWADDU_VV   -> Seq(),
    VWADDU_VX   -> Seq(),
    VWADD_VV    -> Seq(),
    VWADD_VX    -> Seq(),
    VWSUBU_VV   -> Seq(),
    VWSUBU_VX   -> Seq(),
    VWSUB_VV    -> Seq(),
    VWSUB_VX    -> Seq(),
    VWADDU_WV   -> Seq(),
    VWADDU_WX   -> Seq(),
    VWADD_WV    -> Seq(),
    VWADD_WX    -> Seq(),
    VWSUBU_WV   -> Seq(),
    VWSUBU_WX   -> Seq(),
    VWSUB_WV    -> Seq(),
    VWSUB_WX    -> Seq(),
    VWMULU_VV   -> Seq(),
    VWMULU_VX   -> Seq(),
    VWMULSU_VV  -> Seq(),
    VWMULSU_VX  -> Seq(),
    VWMUL_VV    -> Seq(),
    VWMUL_VX    -> Seq(),
    VWMACCU_VV  -> Seq(),
    VWMACCU_VX  -> Seq(),
    VWMACC_VV   -> Seq(),
    VWMACC_VX   -> Seq(),
    VWMACCUS_VX -> Seq(),
    VWMACCSU_VV -> Seq(),
    VWMACCSU_VX -> Seq(),

    VREDSUM_VS      -> Seq(
      vadd.s1v.set(_.maskType, Src12Mask),
      vadd.s1v.set(_.maskType, Src2Mask),
      vadd.s1v.set(_.maskType, Src2Mask),
      vadd.s1v.set(_.maskType, Src2Mask),
      vadd.s1v.set(_.maskType, Src2Mask),
      vadd.s1v.set(_.maskType, Src2Mask),
      vadd.s1v.set(_.maskType, Src2Mask),
      vredsum.set(_.maskType, NoMask),
    ),
    VREDAND_VS      -> Seq(
      vand.s1v.set(_.maskType, Src12Mask),
      vand.s1v.set(_.maskType, Src2Mask),
      vand.s1v.set(_.maskType, Src2Mask),
      vand.s1v.set(_.maskType, Src2Mask),
      vand.s1v.set(_.maskType, Src2Mask),
      vand.s1v.set(_.maskType, Src2Mask),
      vand.s1v.set(_.maskType, Src2Mask),
      vredand.set(_.maskType, NoMask),
    ),
    VREDOR_VS       -> Seq(
      vor.s1v.set(_.maskType, Src12Mask),
      vor.s1v.set(_.maskType, Src2Mask),
      vor.s1v.set(_.maskType, Src2Mask),
      vor.s1v.set(_.maskType, Src2Mask),
      vor.s1v.set(_.maskType, Src2Mask),
      vor.s1v.set(_.maskType, Src2Mask),
      vor.s1v.set(_.maskType, Src2Mask),
      vredor.set(_.maskType, NoMask),
    ),
    VREDXOR_VS      -> Seq(
      vxor.s1v.set(_.maskType, Src12Mask),
      vxor.s1v.set(_.maskType, Src2Mask),
      vxor.s1v.set(_.maskType, Src2Mask),
      vxor.s1v.set(_.maskType, Src2Mask),
      vxor.s1v.set(_.maskType, Src2Mask),
      vxor.s1v.set(_.maskType, Src2Mask),
      vxor.s1v.set(_.maskType, Src2Mask),
      vredxor.set(_.maskType, NoMask),
    ),
    VREDMINU_VS     -> Seq(
      vminu.s1v.set(_.maskType, Src12Mask),
      vminu.s1v.set(_.maskType, Src2Mask),
      vminu.s1v.set(_.maskType, Src2Mask),
      vminu.s1v.set(_.maskType, Src2Mask),
      vminu.s1v.set(_.maskType, Src2Mask),
      vminu.s1v.set(_.maskType, Src2Mask),
      vminu.s1v.set(_.maskType, Src2Mask),
      vredminu.set(_.maskType, NoMask),
    ),
    VREDMIN_VS      -> Seq(
      vmin.s1v.set(_.maskType, Src12Mask),
      vmin.s1v.set(_.maskType, Src2Mask),
      vmin.s1v.set(_.maskType, Src2Mask),
      vmin.s1v.set(_.maskType, Src2Mask),
      vmin.s1v.set(_.maskType, Src2Mask),
      vmin.s1v.set(_.maskType, Src2Mask),
      vmin.s1v.set(_.maskType, Src2Mask),
      vredmin.set(_.maskType, NoMask),
    ),
    VREDMAXU_VS     -> Seq(
      vmaxu.s1v.set(_.maskType, Src12Mask),
      vmaxu.s1v.set(_.maskType, Src2Mask),
      vmaxu.s1v.set(_.maskType, Src2Mask),
      vmaxu.s1v.set(_.maskType, Src2Mask),
      vmaxu.s1v.set(_.maskType, Src2Mask),
      vmaxu.s1v.set(_.maskType, Src2Mask),
      vmaxu.s1v.set(_.maskType, Src2Mask),
      vredmaxu.set(_.maskType, NoMask),
    ),
    VREDMAX_VS      -> Seq(
      vmax.s1v.set(_.maskType, Src12Mask),
      vmax.s1v.set(_.maskType, Src2Mask),
      vmax.s1v.set(_.maskType, Src2Mask),
      vmax.s1v.set(_.maskType, Src2Mask),
      vmax.s1v.set(_.maskType, Src2Mask),
      vmax.s1v.set(_.maskType, Src2Mask),
      vmax.s1v.set(_.maskType, Src2Mask),
      vredmax.set(_.maskType, NoMask),
    ),
    VSLIDE1UP_VX    -> Seq(
      vslide1up.s1x,
      vslide1up.s1x,
      vslide1up.s1x,
      vslide1up.s1x,
      vslide1up.s1x,
      vslide1up.s1x,
      vslide1up.s1x,
      vslide1up.s1x,
    ),
    VSLIDE1DOWN_VX  -> Seq(
      vslide1down,
      vslide1down,
      vslide1down,
      vslide1down,
      vslide1down,
      vslide1down,
      vslide1down,
      vslide1down,
    ),
    VCOMPRESS_VM    -> Seq(
      vcompress_m,
      vcompress_m,
      vcompress_m,
      vcompress_m,
      vcompress_m,
      vcompress_m,
      vcompress_m,
      vcompress_m,
    ),
  )

  val opfDupTable: Map[BitPat, VecUop] = Map(
    VFADD_VV -> vfadd.s1v,
    VFADD_VF -> vfadd.s1f,
    VFSUB_VV -> vfsub.s1v,
    VFSUB_VF -> vfsub.s1f,
    VFMIN_VV -> vfmin.s1v,
    VFMIN_VF -> vfmin.s1f,
    VFMAX_VV -> vfmax.s1v,
    VFMAX_VF -> vfmax.s1f,
    VFSGNJ_VV -> vfsgnj.s1v,
    VFSGNJ_VF -> vfsgnj.s1f,
    VFSGNJN_VV -> vfsgnjn.s1v,
    VFSGNJN_VF -> vfsgnjn.s1f,
    VFSGNJX_VV -> vfsgnjx.s1v,
    VFSGNJX_VF -> vfsgnjx.s1f,
    // VFUNARY0
    VFCVT_XU_F_V      -> vfcvt_xu_f,
    VFCVT_X_F_V       -> vfcvt_x_f,
    VFCVT_F_XU_V      -> vfcvt_f_xu,
    VFCVT_F_X_V       -> vfcvt_f_x,
    VFCVT_RTZ_XU_F_V  -> vfcvt_xu_f_rtz,
    VFCVT_RTZ_X_F_V   -> vfcvt_x_f_rtz,

    // VFUNARY1
    VFSQRT_V          -> vfsqrt,
    VFRSQRT7_V        -> vfrsqrt7,
    VFREC7_V          -> vfrec7,
    VFCLASS_V         -> vfclass,

    VFMERGE_VFM       -> vfmerge.s1f,
    VFMV_V_F          -> vmvFp2Vec,

    VMFEQ_VV          -> vmfeq.s1v,
    VMFEQ_VF          -> vmfeq.s1f,
    VMFLE_VV          -> vmfle.s1v,
    VMFLE_VF          -> vmfle.s1f,
    VMFLT_VV          -> vmflt.s1v,
    VMFLT_VF          -> vmflt.s1f,
    VMFNE_VV          -> vmfne.s1v,
    VMFNE_VF          -> vmfne.s1f,
    VMFGT_VF          -> vmfgt.s1f,
    VMFGE_VF          -> vmfge.s1f,

    VFDIV_VV          -> vfdiv.s1v,
    VFDIV_VF          -> vfdiv.s1f,
    VFRDIV_VF         -> vfdiv.s1f.setSrc12Rev,
    VFMUL_VV          -> vfmul.s1v,
    VFMUL_VF          -> vfmul.s1f,

    VFRSUB_VF         -> vfsub.s1f.setSrc12Rev,

    VFMADD_VV         -> vfmadd.s1v,
    VFMADD_VF         -> vfmadd.s1f,
    VFNMADD_VV        -> vfnmadd.s1v,
    VFNMADD_VF        -> vfnmadd.s1f,
    VFMSUB_VV         -> vfmsub.s1v,
    VFMSUB_VF         -> vfmsub.s1f,
    VFNMSUB_VV        -> vfnmsub.s1v,
    VFNMSUB_VF        -> vfnmsub.s1f,
    VFMACC_VV         -> vfmacc.s1v,
    VFMACC_VF         -> vfmacc.s1f,
    VFNMACC_VV        -> vfnmacc.s1v,
    VFNMACC_VF        -> vfnmacc.s1f,
    VFMSAC_VV         -> vfmsac.s1v,
    VFMSAC_VF         -> vfmsac.s1f,
    VFNMSAC_VV        -> vfnmsac.s1v,
    VFNMSAC_VF        -> vfnmsac.s1f,
  )

  val opfLmulUnrelatedTable: Map[BitPat, Seq[VecUop]] = Map(
    // VWFUNARY0
    VFMV_F_S -> Seq(vmvVecScala2Fp),
  )

  val opfTableF2: Map[BitPat, Seq[VecUop]] = Map(
    VFNCVT_XU_F_W     -> Seq.fill(1)(vfncvt_xu_f),
    VFNCVT_X_F_W      -> Seq.fill(1)(vfncvt_x_f),
    VFNCVT_F_XU_W     -> Seq.fill(1)(vfncvt_f_xu),
    VFNCVT_F_X_W      -> Seq.fill(1)(vfncvt_f_x),
    VFNCVT_F_F_W      -> Seq.fill(1)(vfncvt_f_f),
    VFNCVT_ROD_F_F_W  -> Seq.fill(1)(vfncvt_f_f_rod),
    VFNCVT_RTZ_XU_F_W -> Seq.fill(1)(vfncvt_xu_f_rtz),
    VFNCVT_RTZ_X_F_W  -> Seq.fill(1)(vfncvt_x_f_rtz),

    VFWCVT_XU_F_V     -> Seq.fill(1)(vfwcvt_xu_f),
    VFWCVT_X_F_V      -> Seq.fill(1)(vfwcvt_x_f),
    VFWCVT_F_XU_V     -> Seq.fill(1)(vfwcvt_f_xu),
    VFWCVT_F_X_V      -> Seq.fill(1)(vfwcvt_f_x),
    VFWCVT_F_F_V      -> Seq.fill(1)(vfwcvt_f_f),
    VFWCVT_RTZ_XU_F_V -> Seq.fill(1)(vfwcvt_xu_f_rtz),
    VFWCVT_RTZ_X_F_V  -> Seq.fill(1)(vfwcvt_x_f_rtz),

    VFWADD_VV         -> Seq.fill(1)(vfwadd.s1v),
    VFWADD_VF         -> Seq.fill(1)(vfwadd.s1f),
    VFWSUB_VV         -> Seq.fill(1)(vfwsub.s1v),
    VFWSUB_VF         -> Seq.fill(1)(vfwsub.s1f),
    VFWADD_WV         -> Seq.fill(1)(vfwadd_w.s1v),
    VFWADD_WF         -> Seq.fill(1)(vfwadd_w.s1f),
    VFWSUB_WV         -> Seq.fill(1)(vfwsub_w.s1v),
    VFWSUB_WF         -> Seq.fill(1)(vfwsub_w.s1f),
    VFWMUL_VV         -> Seq.fill(1)(vfwmul.s1v),
    VFWMUL_VF         -> Seq.fill(1)(vfwmul.s1f),
    VFWMACC_VV        -> Seq.fill(1)(vfwmacc.s1v),
    VFWMACC_VF        -> Seq.fill(1)(vfwmacc.s1f),
    VFWNMACC_VV       -> Seq.fill(1)(vfwnmacc.s1v),
    VFWNMACC_VF       -> Seq.fill(1)(vfwnmacc.s1f),
    VFWMSAC_VV        -> Seq.fill(1)(vfwmsac.s1v),
    VFWMSAC_VF        -> Seq.fill(1)(vfwmsac.s1f),
    VFWNMSAC_VV       -> Seq.fill(1)(vfwnmsac.s1v),
    VFWNMSAC_VF       -> Seq.fill(1)(vfwnmsac.s1f),
  )

  val opfTable1: Map[BitPat, Seq[VecUop]] = Map(
    // VRFUNARY0
    VFMV_S_F -> Seq(vmvFp2VecScala),

    VFNCVT_XU_F_W     -> Seq.fill(2)(vfncvt_xu_f),
    VFNCVT_X_F_W      -> Seq.fill(2)(vfncvt_x_f),
    VFNCVT_F_XU_W     -> Seq.fill(2)(vfncvt_f_xu),
    VFNCVT_F_X_W      -> Seq.fill(2)(vfncvt_f_x),
    VFNCVT_F_F_W      -> Seq.fill(2)(vfncvt_f_f),
    VFNCVT_ROD_F_F_W  -> Seq.fill(2)(vfncvt_f_f_rod),
    VFNCVT_RTZ_XU_F_W -> Seq.fill(2)(vfncvt_xu_f_rtz),
    VFNCVT_RTZ_X_F_W  -> Seq.fill(2)(vfncvt_x_f_rtz),

    VFWCVT_XU_F_V     -> Seq.fill(2)(vfwcvt_xu_f),
    VFWCVT_X_F_V      -> Seq.fill(2)(vfwcvt_x_f),
    VFWCVT_F_XU_V     -> Seq.fill(2)(vfwcvt_f_xu),
    VFWCVT_F_X_V      -> Seq.fill(2)(vfwcvt_f_x),
    VFWCVT_F_F_V      -> Seq.fill(2)(vfwcvt_f_f),
    VFWCVT_RTZ_XU_F_V -> Seq.fill(2)(vfwcvt_xu_f_rtz),
    VFWCVT_RTZ_X_F_V  -> Seq.fill(2)(vfwcvt_x_f_rtz),

    VFWADD_VV         -> Seq.fill(2)(vfwadd.s1v),
    VFWADD_VF         -> Seq.fill(2)(vfwadd.s1f),
    VFWSUB_VV         -> Seq.fill(2)(vfwsub.s1v),
    VFWSUB_VF         -> Seq.fill(2)(vfwsub.s1f),
    VFWADD_WV         -> Seq.fill(2)(vfwadd_w.s1v),
    VFWADD_WF         -> Seq.fill(2)(vfwadd_w.s1f),
    VFWSUB_WV         -> Seq.fill(2)(vfwsub_w.s1v),
    VFWSUB_WF         -> Seq.fill(2)(vfwsub_w.s1f),
    VFWMUL_VV         -> Seq.fill(2)(vfwmul.s1v),
    VFWMUL_VF         -> Seq.fill(2)(vfwmul.s1f),
    VFWMACC_VV        -> Seq.fill(2)(vfwmacc.s1v),
    VFWMACC_VF        -> Seq.fill(2)(vfwmacc.s1f),
    VFWNMACC_VV       -> Seq.fill(2)(vfwnmacc.s1v),
    VFWNMACC_VF       -> Seq.fill(2)(vfwnmacc.s1f),
    VFWMSAC_VV        -> Seq.fill(2)(vfwmsac.s1v),
    VFWMSAC_VF        -> Seq.fill(2)(vfwmsac.s1f),
    VFWNMSAC_VV       -> Seq.fill(2)(vfwnmsac.s1v),
    VFWNMSAC_VF       -> Seq.fill(2)(vfwnmsac.s1f),

    VFSLIDE1UP_VF   -> Seq.fill(1)(vslide1up.s1f),
    VFSLIDE1DOWN_VF -> Seq.fill(1)(vslide1down.s1f),

    VFREDUSUM_VS -> Seq.fill(1)(vfredosum.set(_.maskType, Src2Mask)),
    VFREDOSUM_VS -> Seq.fill(1)(vfredosum.set(_.maskType, Src2Mask)),
    VFREDMIN_VS  -> Seq.fill(1)(vfredmin.set(_.maskType, Src2Mask)),
    VFREDMAX_VS  -> Seq.fill(1)(vfredmax.set(_.maskType, Src2Mask)),

    VFWREDUSUM_VS -> Seq[VecUop](
      vfwadd.s1v.set(_.maskType, Src12Mask),
      vfwredosum.set(_.maskType, NoMask),
    ),
    VFWREDOSUM_VS -> Seq(
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
    ),
    VFWREDOSUM_VS -> Seq(
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
    ),
  )

  val opfTable2: Map[BitPat, Seq[VecUop]] = Map(
    // VRFUNARY0
    VFMV_S_F -> (Seq(
      vmvFp2VecScala,
    ) ++ Seq.fill(1)(vtail)),

    VFNCVT_XU_F_W     -> Seq.fill(4)(vfncvt_xu_f),
    VFNCVT_X_F_W      -> Seq.fill(4)(vfncvt_x_f),
    VFNCVT_F_XU_W     -> Seq.fill(4)(vfncvt_f_xu),
    VFNCVT_F_X_W      -> Seq.fill(4)(vfncvt_f_x),
    VFNCVT_F_F_W      -> Seq.fill(4)(vfncvt_f_f),
    VFNCVT_ROD_F_F_W  -> Seq.fill(4)(vfncvt_f_f_rod),
    VFNCVT_RTZ_XU_F_W -> Seq.fill(4)(vfncvt_xu_f_rtz),
    VFNCVT_RTZ_X_F_W  -> Seq.fill(4)(vfncvt_x_f_rtz),

    VFWCVT_XU_F_V     -> Seq.fill(4)(vfwcvt_xu_f),
    VFWCVT_X_F_V      -> Seq.fill(4)(vfwcvt_x_f),
    VFWCVT_F_XU_V     -> Seq.fill(4)(vfwcvt_f_xu),
    VFWCVT_F_X_V      -> Seq.fill(4)(vfwcvt_f_x),
    VFWCVT_F_F_V      -> Seq.fill(4)(vfwcvt_f_f),
    VFWCVT_RTZ_XU_F_V -> Seq.fill(4)(vfwcvt_xu_f_rtz),
    VFWCVT_RTZ_X_F_V  -> Seq.fill(4)(vfwcvt_x_f_rtz),
    VFWADD_VV         -> Seq.fill(4)(vfwadd.s1v),
    VFWADD_VF         -> Seq.fill(4)(vfwadd.s1f),
    VFWSUB_VV         -> Seq.fill(4)(vfwsub.s1v),
    VFWSUB_VF         -> Seq.fill(4)(vfwsub.s1f),
    VFWADD_WV         -> Seq.fill(4)(vfwadd_w.s1v),
    VFWADD_WF         -> Seq.fill(4)(vfwadd_w.s1f),
    VFWSUB_WV         -> Seq.fill(4)(vfwsub_w.s1v),
    VFWSUB_WF         -> Seq.fill(4)(vfwsub_w.s1f),
    VFWMUL_VV         -> Seq.fill(4)(vfwmul.s1v),
    VFWMUL_VF         -> Seq.fill(4)(vfwmul.s1f),
    VFWMACC_VV        -> Seq.fill(4)(vfwmacc.s1v),
    VFWMACC_VF        -> Seq.fill(4)(vfwmacc.s1f),
    VFWNMACC_VV       -> Seq.fill(4)(vfwnmacc.s1v),
    VFWNMACC_VF       -> Seq.fill(4)(vfwnmacc.s1f),
    VFWMSAC_VV        -> Seq.fill(4)(vfwmsac.s1v),
    VFWMSAC_VF        -> Seq.fill(4)(vfwmsac.s1f),
    VFWNMSAC_VV       -> Seq.fill(4)(vfwnmsac.s1v),
    VFWNMSAC_VF       -> Seq.fill(4)(vfwnmsac.s1f),

    VFSLIDE1UP_VF   -> Seq.fill(2)(vslide1up.s1f),
    VFSLIDE1DOWN_VF -> Seq.fill(2)(vslide1down.s1f),
    VFREDUSUM_VS -> Seq(
      vfadd.s1v.set(_.maskType, Src12Mask),
      vfredosum.set(_.maskType, NoMask),
    ),
    VFREDOSUM_VS -> Seq(
      vfredosum.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, Src2Mask),
    ),
    VFREDMIN_VS  -> Seq(
      vfmin.s1v.set(_.maskType, Src12Mask),
      vfredmin.set(_.maskType, NoMask),
    ),
    VFREDMAX_VS  -> Seq(
      vfmax.s1v.set(_.maskType, Src12Mask),
      vfredmax.set(_.maskType, NoMask),
    ),
    VFWREDUSUM_VS -> Seq[VecUop](
      vfwadd.s1v.set(_.maskType, Src12Mask),
      vfwadd_w.s1v.set(_.maskType, Src2Mask),
      vfwadd_w.s1v.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, NoMask),
    ),
    VFWREDOSUM_VS -> Seq(
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
    ),
  )

  val opfTable4: Map[BitPat, Seq[VecUop]] = Map(
    // VRFUNARY0
    VFMV_S_F -> (Seq(
      vmvFp2VecScala,
    ) ++ Seq.fill(3)(vtail)),

    VFNCVT_XU_F_W     -> Seq.fill(8)(vfncvt_xu_f),
    VFNCVT_X_F_W      -> Seq.fill(8)(vfncvt_x_f),
    VFNCVT_F_XU_W     -> Seq.fill(8)(vfncvt_f_xu),
    VFNCVT_F_X_W      -> Seq.fill(8)(vfncvt_f_x),
    VFNCVT_F_F_W      -> Seq.fill(8)(vfncvt_f_f),
    VFNCVT_ROD_F_F_W  -> Seq.fill(8)(vfncvt_f_f_rod),
    VFNCVT_RTZ_XU_F_W -> Seq.fill(8)(vfncvt_xu_f_rtz),
    VFNCVT_RTZ_X_F_W  -> Seq.fill(8)(vfncvt_x_f_rtz),

    VFWCVT_XU_F_V     -> Seq.fill(8)(vfwcvt_xu_f),
    VFWCVT_X_F_V      -> Seq.fill(8)(vfwcvt_x_f),
    VFWCVT_F_XU_V     -> Seq.fill(8)(vfwcvt_f_xu),
    VFWCVT_F_X_V      -> Seq.fill(8)(vfwcvt_f_x),
    VFWCVT_F_F_V      -> Seq.fill(8)(vfwcvt_f_f),
    VFWCVT_RTZ_XU_F_V -> Seq.fill(8)(vfwcvt_xu_f_rtz),
    VFWCVT_RTZ_X_F_V  -> Seq.fill(8)(vfwcvt_x_f_rtz),

    VFWADD_VV         -> Seq.fill(8)(vfwadd.s1v),
    VFWADD_VF         -> Seq.fill(8)(vfwadd.s1f),
    VFWSUB_VV         -> Seq.fill(8)(vfwsub.s1v),
    VFWSUB_VF         -> Seq.fill(8)(vfwsub.s1f),
    VFWADD_WV         -> Seq.fill(8)(vfwadd_w.s1v),
    VFWADD_WF         -> Seq.fill(8)(vfwadd_w.s1f),
    VFWSUB_WV         -> Seq.fill(8)(vfwsub_w.s1v),
    VFWSUB_WF         -> Seq.fill(8)(vfwsub_w.s1f),
    VFWMUL_VV         -> Seq.fill(8)(vfwmul.s1v),
    VFWMUL_VF         -> Seq.fill(8)(vfwmul.s1f),
    VFWMACC_VV        -> Seq.fill(8)(vfwmacc.s1v),
    VFWMACC_VF        -> Seq.fill(8)(vfwmacc.s1f),
    VFWNMACC_VV       -> Seq.fill(8)(vfwnmacc.s1v),
    VFWNMACC_VF       -> Seq.fill(8)(vfwnmacc.s1f),
    VFWMSAC_VV        -> Seq.fill(8)(vfwmsac.s1v),
    VFWMSAC_VF        -> Seq.fill(8)(vfwmsac.s1f),
    VFWNMSAC_VV       -> Seq.fill(8)(vfwnmsac.s1v),
    VFWNMSAC_VF       -> Seq.fill(8)(vfwnmsac.s1f),

    VFSLIDE1UP_VF   -> Seq.fill(4)(vslide1up.s1f),
    VFSLIDE1DOWN_VF -> Seq.fill(4)(vslide1down.s1f),
    VFREDUSUM_VS -> Seq(
      vfadd.s1v.set(_.maskType, Src12Mask),
      vfadd.s1v.set(_.maskType, Src2Mask),
      vfadd.s1v.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, NoMask),
    ),
    VFREDOSUM_VS -> Seq(
      vfredosum.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, Src2Mask),
    ),
    VFREDMIN_VS  -> Seq(
      vfmin.s1v.set(_.maskType, Src12Mask),
      vfmin.s1v.set(_.maskType, Src2Mask),
      vfmin.s1v.set(_.maskType, Src2Mask),
      vfredmin.set(_.maskType, NoMask),
    ),
    VFREDMAX_VS  -> Seq(
      vfmax.s1v.set(_.maskType, Src12Mask),
      vfmax.s1v.set(_.maskType, Src2Mask),
      vfmax.s1v.set(_.maskType, Src2Mask),
      vfredmax.set(_.maskType, NoMask),
    ),
    VFWREDUSUM_VS -> Seq[VecUop](
      vfwadd.s1v.set(_.maskType, Src12Mask),
      vfwadd_w.s1v.set(_.maskType, Src2Mask),
      vfwadd_w.s1v.set(_.maskType, Src2Mask),
      vfwadd_w.s1v.set(_.maskType, Src2Mask),
      vfwadd_w.s1v.set(_.maskType, Src2Mask),
      vfwadd_w.s1v.set(_.maskType, Src2Mask),
      vfwadd_w.s1v.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, NoMask),
    ),
    VFWREDOSUM_VS -> Seq(
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
      vfwredosum.set(_.maskType, Src2Mask),
    ),
  )

  val opfTable8: Map[BitPat, Seq[VecUop]] = Map(
    // VRFUNARY0
    VFMV_S_F -> (Seq(
      vmvFp2VecScala,
    ) ++ Seq.fill(7)(vtail)),

    VFNCVT_XU_F_W     -> Seq(),
    VFNCVT_X_F_W      -> Seq(),
    VFNCVT_F_XU_W     -> Seq(),
    VFNCVT_F_X_W      -> Seq(),
    VFNCVT_F_F_W      -> Seq(),
    VFNCVT_ROD_F_F_W  -> Seq(),
    VFNCVT_RTZ_XU_F_W -> Seq(),
    VFNCVT_RTZ_X_F_W  -> Seq(),

    VFWCVT_XU_F_V     -> Seq(),
    VFWCVT_X_F_V      -> Seq(),
    VFWCVT_F_XU_V     -> Seq(),
    VFWCVT_F_X_V      -> Seq(),
    VFWCVT_F_F_V      -> Seq(),
    VFWCVT_RTZ_XU_F_V -> Seq(),
    VFWCVT_RTZ_X_F_V  -> Seq(),

    VFWADD_VV         -> Seq(),
    VFWADD_VF         -> Seq(),
    VFWSUB_VV         -> Seq(),
    VFWSUB_VF         -> Seq(),
    VFWADD_WV         -> Seq(),
    VFWADD_WF         -> Seq(),
    VFWSUB_WV         -> Seq(),
    VFWSUB_WF         -> Seq(),
    VFWMUL_VV         -> Seq(),
    VFWMUL_VF         -> Seq(),
    VFWMACC_VV        -> Seq(),
    VFWMACC_VF        -> Seq(),
    VFWNMACC_VV       -> Seq(),
    VFWNMACC_VF       -> Seq(),
    VFWMSAC_VV        -> Seq(),
    VFWMSAC_VF        -> Seq(),
    VFWNMSAC_VV       -> Seq(),
    VFWNMSAC_VF       -> Seq(),

    VFSLIDE1UP_VF   -> Seq.fill(8)(vslide1up.s1f),
    VFSLIDE1DOWN_VF -> Seq.fill(8)(vslide1down.s1f),
    VFREDUSUM_VS -> Seq(
      vfadd.s1v.set(_.maskType, Src12Mask),
      vfadd.s1v.set(_.maskType, Src2Mask),
      vfadd.s1v.set(_.maskType, Src2Mask),
      vfadd.s1v.set(_.maskType, Src2Mask),
      vfadd.s1v.set(_.maskType, Src2Mask),
      vfadd.s1v.set(_.maskType, Src2Mask),
      vfadd.s1v.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, NoMask),
    ),
    VFREDOSUM_VS -> Seq(
      vfredosum.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, Src2Mask),
      vfredosum.set(_.maskType, Src2Mask),
    ),
    VFREDMIN_VS  -> Seq(
      vfmin.s1v.set(_.maskType, Src12Mask),
      vfmin.s1v.set(_.maskType, Src2Mask),
      vfmin.s1v.set(_.maskType, Src2Mask),
      vfmin.s1v.set(_.maskType, Src2Mask),
      vfmin.s1v.set(_.maskType, Src2Mask),
      vfmin.s1v.set(_.maskType, Src2Mask),
      vfmin.s1v.set(_.maskType, Src2Mask),
      vfredmin.set(_.maskType, NoMask),
    ),
    VFREDMAX_VS  -> Seq(
      vfmax.s1v.set(_.maskType, Src12Mask),
      vfmax.s1v.set(_.maskType, Src2Mask),
      vfmax.s1v.set(_.maskType, Src2Mask),
      vfmax.s1v.set(_.maskType, Src2Mask),
      vfmax.s1v.set(_.maskType, Src2Mask),
      vfmax.s1v.set(_.maskType, Src2Mask),
      vfmax.s1v.set(_.maskType, Src2Mask),
      vfredmax.set(_.maskType, NoMask),
    ),
    VFWREDUSUM_VS -> Seq(),
    VFWREDOSUM_VS -> Seq(),
  )

  val dupTable = opiDupTable ++ opmDupTable ++ opfDupTable

  val lmulUnrelatedTable = opiLmulUnrelatedTable ++ opmLmulUnrelatedTable ++ opfLmulUnrelatedTable

  val lmul8Table: Map[BitPat, Seq[VecUop]] = opiTable8 ++ opmTable8 ++ opfTable8

  val lmul4Table: Map[BitPat, Seq[VecUop]] = opiTable4 ++ opmTable4 ++ opfTable4

  val lmul2Table: Map[BitPat, Seq[VecUop]] = opiTable2 ++ opmTable2 ++ opfTable2

  val lmul1Table: Map[BitPat, Seq[VecUop]] = opiTable1 ++ opmTable1 ++ opfTable1

  val lmulF2Table: Map[BitPat, Seq[VecUop]] = opiTableF2 ++ opmTableF2 ++ opfTableF2
}
