package xiangshan.backend.vector.Decoder.Uop

import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.Types.Sign
import xiangshan.backend.vector.Decoder.Uop.UopTrait._

object VecUopDefines {
  /**
   * [[vset_vtypex_vlx]] is used for [[VSETVL]] when rs1 != x0
   */
  def vset_vtypex_vlx = VecConfigUop(GpWen, VlWen, Src2Gp, Src1Gp)

  /**
   * [[vset_vtypex_vlmax]] is used for [[VSETVL]] when rs1 == x0 and rd != x0
   */
  def vset_vtypex_vlmax = VecConfigUop(GpWen, VlWen, Src2Gp)

  /**
   * [[vset_vtypex_vll]] is used for [[VSETVL]] when rs1 == x0 and rd == x0
   */
  def vset_vtypex_vll = VecConfigUop(VlWen, Src2Gp, VlRen)

  /**
   * [[vset_vtypei_vlx]] is used for [[VSETVLI]] when rs1 != x0
   */
  def vset_vtypei_vlx = new VecConfigUop(GpWen, VlWen, Src1Gp)

  /**
   * [[vset_vtypei_vlmax]] is used for [[VSETVLI]] when rs1 == x0
   */
  def vset_vtypei_vlmax = new VecConfigUop(GpWen, VlWen)

  /**
   * [[vset_vtypei_nop]] is used for [[VSETVLI]] when rs1 == x0 and rd == x0
   * This uop does not change vl but modifies vtype.
   * if vlmax shrink, [[vset_vtypei_ill]] should be used to set vill
   */
  def vset_vtypei_nop = VecConfigUop()

  /**
   * [[vset_vtypei_vli]] is used for [[VSETIVLI]]
   */
  def vset_vtypei_vli = VecConfigUop(GpWen, VlWen)

  /**
   * [[vset_vtypei_ill]] is used for illegal [[VSETVLI]] and [[VSETIVLI]] when rs1 == x0 and rd == x0.
   * When rs1 == x0, rd == x0 and SEW/LMUL ratio is changed, the instruction is reserved.
   * This uop will set vill = 1 and vl = 0.
   */
  def vset_vtypei_ill = VecConfigUop(GpWen, VlWen)

  // 11.1. Vector Single-Width Integer Add and Subtract
  def vadd = VecIntUopVV_DV().set(_.sgn, Sign.S)
  def vsub = VecIntUopVV_DV().set(_.sgn, Sign.S)

  // 11.2. Vector Widening Integer Add/Subtract
  def vwaddu   = VecIntUopVV_DW().set(_.sgn, Sign.U)
  def vwsubu   = VecIntUopVV_DW().set(_.sgn, Sign.U)
  def vwadd    = VecIntUopVV_DW().set(_.sgn, Sign.S)
  def vwsub    = VecIntUopVV_DW().set(_.sgn, Sign.S)
  def vwaddu_w = VecIntUopWV_DW().set(_.sgn, Sign.U)
  def vwsubu_w = VecIntUopWV_DW().set(_.sgn, Sign.U)
  def vwadd_w  = VecIntUopWV_DW().set(_.sgn, Sign.S)
  def vwsub_w  = VecIntUopWV_DW().set(_.sgn, Sign.S)

  // 11.3. Vector Integer Extension
  def vzext2 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.U)
  def vzext4 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.U)
  def vzext8 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.U)
  def vsext2 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.S)
  def vsext4 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.S)
  def vsext8 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.S)

  // 11.4. Vector Integer Add-with-Carry / Subtract-with-Borrow Instructions
  def vadc  = VecIntUopVVM_DV()
  def vmadc = VecIntUopVVM_DM()
  def vsbc  = VecIntUopVVM_DV()
  def vmsbc = VecIntUopVVM_DM()

  // 11.5. Vector Bitwise Logical Instructions
  def vand = VecIntUopVV_DV()
  def vor  = VecIntUopVV_DV()
  def vxor = VecIntUopVV_DV()

  // 11.6. Vector Single-Width Shift Instructions
  def vsll = VecIntUopVV_DV()
  def vsrl = VecIntUopVV_DV().set(_.sgn, Sign.U)
  def vsra = VecIntUopVV_DV().set(_.sgn, Sign.S)

  // 11.7. Vector Narrowing Integer Right Shift Instructions
  def vnsrl = VecIntUopWV_DV().set(_.sgn, Sign.U)
  def vnsra = VecIntUopWV_DV().set(_.sgn, Sign.S)

  // 11.8. Vector Integer Compare Instructions
  def vmseq = VecIntUopVV_DM()                     // ==
  def vmsne = VecIntUopVV_DM()                     // !=
  def vmsgt = VecIntUopVV_DM().set(_.sgn, Sign.S)  //  >
  def vmsle = VecIntUopVV_DM().set(_.sgn, Sign.S)  //  <=
  def vmslt = VecIntUopVV_DM().set(_.sgn, Sign.S)  //  <
  def vmsgtu = VecIntUopVV_DM().set(_.sgn, Sign.U) //  >
  def vmsleu = VecIntUopVV_DM().set(_.sgn, Sign.U) //  <=
  def vmsltu = VecIntUopVV_DM().set(_.sgn, Sign.U) //  <

  // 11.9. Vector Integer Min/Max Instructions
  def vmaxu = VecIntUopVV_DV().set(_.sgn, Sign.U)
  def vminu = VecIntUopVV_DV().set(_.sgn, Sign.U)
  def vmax  = VecIntUopVV_DV().set(_.sgn, Sign.S)
  def vmin  = VecIntUopVV_DV().set(_.sgn, Sign.S)

  // 11.10. Vector Single-Width Integer Multiply Instructions
  def vmulh   = VecIntUopVV_DV().set(_.sgn, Sign.S)
  def vmul    = VecIntUopVV_DV().set(_.sgn, Sign.S)
  def vmulhu  = VecIntUopVV_DV().set(_.sgn, Sign.U)
  def vmulhsu = VecIntUopVV_DV().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S).set(_.destSgn, Sign.S)

  // 11.11. Vector Integer Divide Instructions
  def vdiv  = VecIntUopVV_DV().set(_.sgn, Sign.S)
  def vrem  = VecIntUopVV_DV().set(_.sgn, Sign.S)
  def vdivu = VecIntUopVV_DV().set(_.sgn, Sign.U)
  def vremu = VecIntUopVV_DV().set(_.sgn, Sign.U)

  // 11.12. Vector Widening Integer Multiply Instructions
  def vwmulu  = VecIntUopVV_DW().set(_.sgn, Sign.U)
  def vwmul   = VecIntUopVV_DW().set(_.sgn, Sign.S)
  def vwmulsu = VecIntUopVV_DW().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S)

  // 11.13. Vector Single-Width Integer Multiply-Add Instructions
  def vmacc  = VecIntUopVVV_DV()
  def vmadd  = VecIntUopVVV_DV()
  def vnmsac = VecIntUopVVV_DV()
  def vnmsub = VecIntUopVVV_DV()

  // 11.14. Vector Widening Integer Multiply-Add Instructions
  def vwmaccu  = VecIntUopVVW_DW().set(_.sgn, Sign.U)
  def vwmacc   = VecIntUopVVW_DW().set(_.sgn, Sign.S)
  def vwmaccsu = VecIntUopVVW_DW().set(_.src1Sgn, Sign.S).set(_.src2Sgn, Sign.U)
  def vwmaccus = VecIntUopVVW_DW().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S)

  // 11.15. Vector Integer Merge Instructions
  def vmerge = VecIntUopVV_DV()

  // 11.16. Vector Integer Move Instructions
  def vmvInt2Vec = VecIntUopS1(Src1Gp, VpWen)
  def vmvVec2Vec = VecIntUopS1(Src1Vp, VpWen)
  def vmvImm2Vec = VecIntUopS1(Src1Imm, VpWen)

  // 12.1. Vector Single-Width Saturating Add and Subtract
  def vsaddu = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.U)
  def vssubu = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.U)
  def vsadd  = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.S)
  def vssub  = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.S)

  // 12.2. Vector Single-Width Averaging
  def vaaddu = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U)
  def vasubu = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U)
  def vaadd  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U)
  def vasub  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U)

  // 12.3. Vector Single-Width Fractional Multiply with Rounding and Saturation
  def vsmul = VecIntFixUopVV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.S)

  // 12.4. Vector Single-Width Scaling Shift Instructions
  def vssrl = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U)
  def vssra = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.S)

  // 12.5. Vector Narrowing Fixed-Point Clip Instructions
  def vnclipu = VecIntFixUopWV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.U)
  def vnclip  = VecIntFixUopWV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.S)

  // 13.2. Vector Single-Width Floating-Point Add/Subtract Instructions
  def vfadd = VecFpUopVV_DV()
  def vfsub = VecFpUopVV_DV()

  // 13.3. Vector Widening Floating-Point Add/Subtract Instructions
  def vfwadd   = VecFpUopVV_DW()
  def vfwsub   = VecFpUopVV_DW()
  def vfwadd_w = VecFpUopWV_DW()
  def vfwsub_w = VecFpUopWV_DW()

  // 13.4. Vector Single-Width Floating-Point Multiply/Divide Instructions
  def vfmul = VecFpUopVV_DV()
  def vfdiv = VecFpUopVV_DV()

  // 13.5. Vector Widening Floating-Point Multiply
  def vfwmul = VecFpUopVV_DW()

  // 13.6. Vector Single-Width Floating-Point Fused Multiply-Add Instructions
  def vfmacc  = VecFpUopVVV_DV()
  def vfnmacc = VecFpUopVVV_DV()
  def vfmsac  = VecFpUopVVV_DV()
  def vfnmsac = VecFpUopVVV_DV()
  def vfmadd  = VecFpUopVVV_DV()
  def vfnmadd = VecFpUopVVV_DV()
  def vfmsub  = VecFpUopVVV_DV()
  def vfnmsub = VecFpUopVVV_DV()

  // 13.7. Vector Widening Floating-Point Fused Multiply-Add Instructions
  def vfwmacc  = VecFpUopVVW_DW()
  def vfwnmacc = VecFpUopVVW_DW()
  def vfwmsac  = VecFpUopVVW_DW()
  def vfwnmsac = VecFpUopVVW_DW()

  // 13.8. Vector Floating-Point Square-Root Instruction
  // 13.9. Vector Floating-Point Reciprocal Square-Root Estimate Instruction
  // 13.10. Vector Floating-Point Reciprocal Estimate Instruction
  def vfsqrt   = VecFpUopS2V_DV()
  def vfrsqrt7 = VecFpUopS2V_DV()
  def vfrec7   = VecFpUopS2V_DV()

  // 13.11. Vector Floating-Point MIN/MAX Instructions
  def vfmin = VecFpUopVV_DV()
  def vfmax = VecFpUopVV_DV()

  // 13.12. Vector Floating-Point Sign-Injection Instructions
  def vfsgnj  = VecFpUopVV_DV()
  def vfsgnjn = VecFpUopVV_DV()
  def vfsgnjx = VecFpUopVV_DV()

  // 13.13. Vector Floating-Point Compare Instructions
  def vmfeq = VecFpUopVV_DM()
  def vmfne = VecFpUopVV_DM()
  def vmfle = VecFpUopVV_DM()
  def vmflt = VecFpUopVV_DM()
  def vmfge = VecFpUopVV_DM()
  def vmfgt = VecFpUopVV_DM()

  // 13.14. Vector Floating-Point Classify Instruction
  def vfclass = VecFpUopS2V_DV()

  // 13.15. Vector Floating-Point Merge Instruction
  def vfmerge = VecFpUopVV_DV()

  // 13.16. Vector Floating-Point Move Instruction
  def vmvFp2Vec = VecFpUopS1(Src1Fp, VpWen)

  // 13.17. Single-Width Floating-Point/Integer Type-Convert Instructions
  def vfcvt_xu_f     = VecFpUopS2V_DV()
  def vfcvt_xu_f_rtz = VecFpUopS2V_DV()
  def vfcvt_f_xu     = VecFpUopS2V_DV()
  def vfcvt_x_f      = VecFpUopS2V_DV()
  def vfcvt_x_f_rtz  = VecFpUopS2V_DV()
  def vfcvt_f_x      = VecFpUopS2V_DV()

  // 13.18. Widening Floating-Point/Integer Type-Convert Instructions
  def vfwcvt_xu_f     = VecFpUopS2V_DW()
  def vfwcvt_xu_f_rtz = VecFpUopS2V_DW()
  def vfwcvt_f_x      = VecFpUopS2V_DW()
  def vfwcvt_x_f      = VecFpUopS2V_DW()
  def vfwcvt_x_f_rtz  = VecFpUopS2V_DW()
  def vfwcvt_f_xu     = VecFpUopS2V_DW()
  def vfwcvt_f_f      = VecFpUopS2V_DW()

  // 13.19. Narrowing Floating-Point/Integer Type-Convert Instructions
  def vfncvt_xu_f     = VecFpUopS2W_DV()
  def vfncvt_xu_f_rtz = VecFpUopS2W_DV()
  def vfncvt_f_xu     = VecFpUopS2W_DV()
  def vfncvt_x_f      = VecFpUopS2W_DV()
  def vfncvt_x_f_rtz  = VecFpUopS2W_DV()
  def vfncvt_f_x      = VecFpUopS2W_DV()
  def vfncvt_f_f      = VecFpUopS2W_DV()
  def vfncvt_f_f_rod  = VecFpUopS2W_DV()

  // 14.1. Vector Single-Width Integer Reduction Instructions
  // reduction
  def vredand  = VecIntRedUopVA_DA()
  def vredor   = VecIntRedUopVA_DA()
  def vredxor  = VecIntRedUopVA_DA()
  def vredmaxu = VecIntRedUopVA_DA().set(_.sgn, Sign.U)
  def vredminu = VecIntRedUopVA_DA().set(_.sgn, Sign.U)
  def vredmax  = VecIntRedUopVA_DA().set(_.sgn, Sign.S)
  def vredmin  = VecIntRedUopVA_DA().set(_.sgn, Sign.S)
  def vredsum  = VecIntRedUopVA_DA().set(_.sgn, Sign.S)

  // 14.2. Vector Widening Integer Reduction Instructions
  def vwredsumu = VecIntWRedUopVA_DA().set(_.sgn, Sign.U)
  def vwredsum  = VecIntWRedUopVA_DA().set(_.sgn, Sign.S)

  // 14.3. Vector Single-Width Floating-Point Reduction Instructions
  def vfredosum = VecFpRedUopVA_DA()
  def vfredusum = VecFpRedUopVA_DA()
  def vfredmin = VecFpRedUopVA_DA()
  def vfredmax = VecFpRedUopVA_DA()

  // 14.4. Vector Widening Floating-Point Reduction Instructions
  def vfwredosum = VecFpWRedUopVA_DA()
  def vfwredusum = VecFpWRedUopVA_DA()

  // 15.1. Vector Mask-Register Logical Instructions
  def vmand  = VecIntUopMM_DM()
  def vmnand = VecIntUopMM_DM()
  def vmandn = VecIntUopMM_DM()
  def vmor   = VecIntUopMM_DM()
  def vmnor  = VecIntUopMM_DM()
  def vmorn  = VecIntUopMM_DM()
  def vmxor  = VecIntUopMM_DM()
  def vmxnor = VecIntUopMM_DM()

  // 15.2. Vector count population in mask vcpop.m
  // 15.3. vfirst find-first-set mask bit
  // 15.4. vmsbf.m set-before-first mask bit
  // 15.5. vmsif.m set-including-first mask bit
  // 15.6. vmsof.m set-only-first mask bit
  def vcpop_m = VecIntUopS2M_DX()
  def vfirst = VecIntUopS2M_DX()
  def vmsbf = VecIntUopS2M_DM()
  def vmsif = VecIntUopS2M_DM()
  def vmsof = VecIntUopS2M_DM()

  // 15.8. Vector Iota Instruction
  def viota = VecIntUopS2M_DV()

  // 15.9. Vector Element Index Instruction
  def vid = VecIntUop_DV()

  // 16.1. Integer Scalar Move Instructions
  /**
   * vmv.x.s performs its operation even if vstart >= vl or vl=0
   */
  def vmvVecScala2Int = VecUopS2A(GpWen)

  def vmvInt2VecScala = VecUopS1_DA(Src1Gp)

  // 16.2. Floating-Point Scalar Move Instructions
  /**
   * vfmv.f.s performs its operation even if vstart >= vl or vl=0
   */
  def vmvVecScala2Fp = VecUopS2A(FpWen)

  def vmvFp2VecScala = VecUopS1_DA(Src1Fp)

  // 16.3. Vector Slide Instructions
  def vslideup_x   = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask){}
  def vslideup_i   = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask){}
  def vslidedown_x = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask){}
  def vslidedown_i = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask){}
  def vslide1up  = new VecIntUop(Src2Vp, VpWen, VlRen, V0RenAsMask){
    override def allowedTraits: Set[UopTrait] = Set(Src1Gp, Src1Fp)
  }
  def vslide1down= new VecIntUop(Src2Vp, VpWen, VlRen, V0RenAsMask){
    override def allowedTraits: Set[UopTrait] = Set(Src1Gp, Src1Fp)
  }
  def vfslide1up   = new VecFpUop (Src2Vp, Src1Fp, VpWen, VlRen, V0RenAsMask){}
  def vfslide1down = new VecFpUop (Src2Vp, Src1Fp, VpWen, VlRen, V0RenAsMask){}

  // 16.4. Vector Register Gather Instructions
  def vrgather_v   = new VecIntUop(Src2Vp, Src1Vp, VpWen, VlRen, V0RenAsMask) {}
  def vrgatherei16_v = new VecIntUop(Src2Vp, Src1Vp, VpWen, VlRen, V0RenAsMask) {}

  def vrgather_x = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}
  def vrgather_i = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask) {}

  // 16.5. Vector Compress Instruction
  def vcompress_m   = VecIntUopVM_DV()

  // 16.6. Whole Vector Register Move
  def vmvnr = VecIntUopS2V_DV()

  //
  def vle   = new VecLoadUop(Src1Gp, VpWen, VlRen)
  def vleff = new VecLoadUop(Src1Gp, VpWen, VlRen)
  def vlm   = new VecLoadUop(Src1Gp, VmWen, VlRen)
  def vlnr  = new VecLoadUop(Src1Gp, VpWen)
  def vlse  = new VecLoadUop(Src1Gp, VpWen, VlRen, Src2Gp)
  def vluxe = new VecLoadUop(Src1Gp, VpWen, VlRen, Src2Vp)
  def vloxe = new VecLoadUop(Src1Gp, VpWen, VlRen, Src2Vp, Order)

  def vse   = new VecStoreUop(Src1Gp, Src3Vp, VlRen)
  def vsnr  = new VecStoreUop(Src1Gp, Src3Vp)
  def vsm   = new VecStoreUop(Src1Gp, Src3Vp, VlRen)
  def vsse  = new VecStoreUop(Src1Gp, Src3Vp, VlRen, Src2Gp)
  def vsuxe = new VecStoreUop(Src1Gp, Src3Vp, VlRen, Src2Vp)
  def vsoxe = new VecStoreUop(Src1Gp, Src3Vp, VlRen, Src2Vp, Order)

  // 1 src op
  def vbrev8  = VecIntUopS2V_DV()
  def vbrev   = VecIntUopS2V_DV()
  def vclz    = VecIntUopS2V_DV()
  def vctz    = VecIntUopS2V_DV()
  def vcpop_v = VecIntUopS2V_DV()
  def vrev8   = VecIntUopS2V_DV()

  def vandn = new VecIntUopVV_DV
  def vclmulh = new VecIntUopVV_DV
  def vclmul = new VecIntUopVV_DV

  def vrol = new VecIntUopVV_DV
  def vror = new VecIntUopVV_DV

  /**
   * This uop is used to fill tail of Vector Registers
   * E.g.
   */
  def vtail = VecIntUop_DV()

  lazy val allUops: Iterable[UopBase] = {
    import scala.reflect.runtime.currentMirror
    import scala.reflect.runtime.universe._
    val objectType = typeOf[this.type]
    val uopSymbols: Iterable[MethodSymbol] = objectType.decls.collect {
      case m: MethodSymbol if m.returnType <:< typeOf[UopBase] => m
    }
    val instanceMirror = currentMirror.reflect(this)

    val uops: Iterable[UopBase] = uopSymbols.map { uopSymbol: MethodSymbol =>
      val methodMirror: MethodMirror = instanceMirror.reflectMethod(uopSymbol)
      println(s"calling ${uopSymbol.name}")
      methodMirror().asInstanceOf[UopBase]
    }

    uops
  }

  lazy val allNames: Iterable[String] = {
    import scala.reflect.runtime.universe._
    val objectType = typeOf[this.type]
    val uopSymbols: Iterable[String] = objectType.decls.collect {
      case m: MethodSymbol if m.returnType <:< typeOf[UopBase] => m.name.toString
    }
    uopSymbols
  }

  def main(arg: Array[String]): Unit = {
    (allUops zip allNames).foreach{ case (uop, name) => println( f"${name}%-20s - ${uop.uopInfoRenameString}") }
  }
}
