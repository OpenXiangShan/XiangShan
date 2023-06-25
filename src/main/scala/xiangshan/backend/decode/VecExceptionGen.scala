package xiangshan.backend.decode

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import utility._
import utils._
import xiangshan._
import xiangshan.backend.Bundles.{DecodedInst, DynInst, StaticInst}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.backend.decode.isa.bitfield.{InstVType, XSInstBitFields}

object RegNumNotAlign {
  def apply(reg: UInt, emul: UInt): Bool = {
    emul === "b101".U && reg(0) =/= 0.U || emul === "b110".U && reg(1, 0) =/= 0.U || emul === "b111".U && reg(2, 0) =/= 0.U
  }
}

object NFtoLmul {
  def apply(nf: UInt): UInt = {
    LookupTree(nf, List(
      "b000".U -> 4.U,
      "b001".U -> 5.U,
      "b011".U -> 6.U,
      "b111".U -> 7.U
    ))
  }
}

object LmultoRegNum {
  def apply(lmul: UInt): UInt = {
    val numPow = Mux(lmul(2).asBool, lmul(1, 0), 0.U(2.W))
    val regNum = 1.U << numPow
    regNum
  }
}

class VecExceptionGen(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle(){
    val inst = Input(UInt(32.W))
    val decodedInst = Input(new DecodedInst)
    val vtype = Input(new VType)

    val illegalInst = Output(Bool())
  })

  private val inst: XSInstBitFields = io.inst.asTypeOf(new XSInstBitFields)
  private val isVector = io.decodedInst.fuType(24, 16).orR

  private val SEW = io.vtype.vsew(1, 0)
  private val LMUL = Cat(~io.vtype.vlmul(2), io.vtype.vlmul(1, 0))

  private val lsStrideInst = Seq(
    VLE8_V, VLE16_V, VLE32_V, VLE64_V, VSE8_V, VSE16_V, VSE32_V, VSE64_V, 
    VLSE8_V, VLSE16_V, VLSE32_V, VLSE64_V, VSSE8_V, VSSE16_V, VSSE32_V, VSSE64_V, 
    VLE8FF_V, VLE16FF_V, VLE32FF_V, VLE64FF_V
  ).map(_ === inst.ALL).reduce(_ || _)

  private val lsMaskInst = Seq(
    VLM_V, VSM_V
  ).map(_ === inst.ALL).reduce(_ || _)

  private val lsIndexInst = Seq(
    VLUXEI8_V, VLUXEI16_V, VLUXEI32_V, VLUXEI64_V, VLOXEI8_V, VLOXEI16_V, VLOXEI32_V, VLOXEI64_V, 
    VSUXEI8_V, VSUXEI16_V, VSUXEI32_V, VSUXEI64_V, VSOXEI8_V, VSOXEI16_V, VSOXEI32_V, VSOXEI64_V
  ).map(_ === inst.ALL).reduce(_ || _)

  private val lsWholeInst = Seq(
    VL1RE8_V, VL1RE16_V, VL1RE32_V, VL1RE64_V, 
    VL2RE8_V, VL2RE16_V, VL2RE32_V, VL2RE64_V, 
    VL4RE8_V, VL4RE16_V, VL4RE32_V, VL4RE64_V, 
    VL8RE8_V, VL8RE16_V, VL8RE32_V, VL8RE64_V, 
    VS1R_V, VS2R_V, VS4R_V, VS8R_V
  ).map(_ === inst.ALL).reduce(_ || _)

  private val vdWideningInst = Seq(
    //int
    VWADD_VV, VWADD_VX, VWADD_WV, VWADD_WX, VWADDU_VV, VWADDU_VX, VWADDU_WV, VWADDU_WX, 
    VWMACC_VV, VWMACC_VX, VWMACCSU_VV, VWMACCSU_VX, VWMACCU_VV, VWMACCU_VX, VWMACCUS_VX, 
    VWMUL_VV, VWMUL_VX, VWMULSU_VV, VWMULSU_VX, VWMULU_VV, VWMULU_VX, 
    VWSUB_VV, VWSUB_VX, VWSUB_WV, VWSUB_WX, VWSUBU_VV, VWSUBU_VX, VWSUBU_WV, VWSUBU_WX, 
    //fp
    VFWADD_VF, VFWADD_VV, VFWADD_WF, VFWADD_WV, VFWSUB_VF, VFWSUB_VV, VFWSUB_WF, VFWSUB_WV, 
    VFWMUL_VF, VFWMUL_VV, 
    VFWMACC_VF, VFWMACC_VV, VFWMSAC_VF, VFWMSAC_VV, VFWNMACC_VF, VFWNMACC_VV, VFWNMSAC_VF, VFWNMSAC_VV, 
    VFWCVT_F_F_V, VFWCVT_F_X_V, VFWCVT_F_XU_V, VFWCVT_RTZ_X_F_V, VFWCVT_RTZ_XU_F_V, VFWCVT_X_F_V, VFWCVT_XU_F_V
  ).map(_ === inst.ALL).reduce(_ || _)

  private val vs2WideningInst = Seq(
    //int
    VWADD_WV, VWADD_WX, VWADDU_WV, VWADDU_WX, 
    VWSUB_WV, VWSUB_WX, VWSUBU_WV, VWSUBU_WX, 
    //fp
    VFWADD_WF, VFWADD_WV, VFWSUB_WF, VFWSUB_WV
  ).map(_ === inst.ALL).reduce(_ || _)

  private val narrowingInst = Seq(
    //int
    VNCLIP_WI, VNCLIP_WV, VNCLIP_WX, VNCLIPU_WI, VNCLIPU_WV, VNCLIPU_WX, 
    VNSRA_WI, VNSRA_WV, VNSRA_WX, VNSRL_WI, VNSRL_WV, VNSRL_WX, 
    //fp
    VFNCVT_F_F_W, VFNCVT_F_X_W, VFNCVT_F_XU_W, VFNCVT_ROD_F_F_W, VFNCVT_RTZ_X_F_W, VFNCVT_RTZ_XU_F_W, VFNCVT_X_F_W, VFNCVT_XU_F_W
  ).map(_ === inst.ALL).reduce(_ || _)

  private val intExtInst = Seq(
    VSEXT_VF2, VSEXT_VF4, VSEXT_VF8, VZEXT_VF2, VZEXT_VF4, VZEXT_VF8
  ).map(_ === inst.ALL).reduce(_ || _)

  private val acsbInst = Seq(
    VMADC_VI, VMADC_VIM, VMADC_VV, VMADC_VVM, VMADC_VX, VMADC_VXM, 
    VMSBC_VV, VMSBC_VVM, VMSBC_VX, VMSBC_VXM
  ).map(_ === inst.ALL).reduce(_ || _)

  private val cmpInst = Seq(
    //int
    VMSEQ_VI, VMSEQ_VV, VMSEQ_VX, 
    VMSGT_VI, VMSGT_VX, VMSGTU_VI, VMSGTU_VX, 
    VMSLE_VI, VMSLE_VV, VMSLE_VX, VMSLEU_VI, VMSLEU_VV, VMSLEU_VX, 
    VMSLT_VV, VMSLT_VX, VMSLTU_VV, VMSLTU_VX, 
    VMSNE_VI, VMSNE_VV, VMSNE_VX, 
    //fp
    VMFEQ_VF, VMFEQ_VV, VMFNE_VF, VMFNE_VV, 
    VMFGE_VF, VMFGT_VF, VMFLE_VF, VMFLE_VV, VMFLT_VF, VMFLT_VV
  ).map(_ === inst.ALL).reduce(_ || _)

  private val redInst = Seq(
    VREDAND_VS, VREDMAX_VS, VREDMAXU_VS, VREDMIN_VS, VREDMINU_VS, VREDOR_VS, VREDSUM_VS, VREDXOR_VS, 
    VFREDMAX_VS, VFREDMIN_VS, VFREDOSUM_VS, VFREDUSUM_VS
  ).map(_ === inst.ALL).reduce(_ || _)

  private val redWideningInst = Seq(
    VWREDSUM_VS, VWREDSUMU_VS, 
    VFWREDOSUM_VS, VFWREDUSUM_VS
  ).map(_ === inst.ALL).reduce(_ || _)

  private val maskLogicalInst = Seq(
    VMAND_MM, VMNAND_MM, VMANDN_MM, VMXOR_MM, VMOR_MM, VMNOR_MM, VMORN_MM, VMXNOR_MM
  ).map(_ === inst.ALL).reduce(_ || _)

  private val maskArithmeticInst = Seq(
    VCPOP_M, VFIRST_M, VMSBF_M, VMSIF_M, VMSOF_M
  ).map(_ === inst.ALL).reduce(_ || _) || maskLogicalInst

  private val maskIndexInst = Seq(
    VIOTA_M, VID_V
  ).map(_ === inst.ALL).reduce(_ || _)

  private val vmvSingleInst = Seq(
    VMV_X_S, VMV_S_X, VFMV_F_S, VFMV_S_F
  ).map(_ === inst.ALL).reduce(_ || _)

  private val vmvWholeInst = Seq(
    VMV1R_V, VMV2R_V, VMV4R_V, VMV8R_V
  ).map(_ === inst.ALL).reduce(_ || _)

  private val vrgather16 = VRGATHEREI16_VV === inst.ALL
  private val vcompress = VCOMPRESS_VM === inst.ALL
  private val intExt2 = Seq(VSEXT_VF2, VZEXT_VF2).map(_ === inst.ALL).reduce(_ || _)
  private val intExt4 = Seq(VSEXT_VF4, VZEXT_VF4).map(_ === inst.ALL).reduce(_ || _)
  private val intExt8 = Seq(VSEXT_VF8, VZEXT_VF8).map(_ === inst.ALL).reduce(_ || _)

  private val notDependVtypeInst = Seq(VSETVLI, VSETIVLI, VSETVL).map(_ === inst.ALL).reduce(_ || _) || lsWholeInst || vmvWholeInst


  // 1. inst Illegal
  private val instIllegal = maskLogicalInst && inst.VM === 0.U

  // 2. vill Illegal
  private val villIllegal = io.vtype.illegal && isVector && !notDependVtypeInst

  // 3. EEW Illegal
  private val doubleFpInst = Seq(
    VFWCVT_F_X_V, VFWCVT_F_XU_V, VFNCVT_RTZ_X_F_W, VFNCVT_RTZ_XU_F_W, VFNCVT_X_F_W, VFNCVT_XU_F_W
  ).map(_ === inst.ALL).reduce(_ || _)
  private val fpEewIllegal = io.decodedInst.fuType(18).asBool && !doubleFpInst && SEW === 0.U

  private val intExtEewIllegal = intExt2 && SEW === 0.U ||
                                 intExt4 && SEW <= 1.U ||
                                 intExt8 && SEW <= 2.U

  private val wnEewIllegal = (vdWideningInst || narrowingInst || redWideningInst) && SEW === 3.U

  private val eewIllegal = fpEewIllegal || intExtEewIllegal || wnEewIllegal

  // 4. EMUL Illegal
  private val lsEmulIllegal = (lsStrideInst || lsIndexInst) && (LMUL +& inst.WIDTH(1, 0) < SEW +& 1.U || LMUL +& inst.WIDTH(1, 0) > SEW +& 7.U)

  private val intExtEmulIllegal = intExt2 && LMUL === 1.U ||
                                  intExt4 && LMUL <= 2.U ||
                                  intExt8 && LMUL <= 3.U

  private val wnEmulIllegal = (vdWideningInst || narrowingInst || redWideningInst) && LMUL === 7.U

  private val gather16EmulIllegal = vrgather16 && (LMUL < SEW || LMUL > SEW +& 6.U)

  private val NFIELDS = inst.NF +& 1.U
  private val segEmul = Mux(lsIndexInst, LMUL, LMUL +& inst.WIDTH(1, 0) - SEW)
  private val emulNumPow = Mux(segEmul(2), segEmul(1, 0), 0.U(2.W))
  private val segRegNum = NFIELDS << emulNumPow
  private val segRegMax = inst.VD +& segRegNum

  private val lsSegIllegal = (lsStrideInst || lsIndexInst) && inst.NF =/= 0.U && (segRegNum > 8.U || segRegMax > 32.U)
  
  private val emulIllegal = lsEmulIllegal || intExtEmulIllegal || wnEmulIllegal || gather16EmulIllegal || lsSegIllegal

  // 5. Reg Number Align
  private val vs1IsMask = maskArithmeticInst || vcompress
  private val vs1IsSingleElem = redInst
  private val vs1Eew = Mux(vrgather16, "b01".U, SEW)
  private val vs1Emul = Mux(vs1IsMask || vs1IsSingleElem, "b100".U, Mux(vrgather16, LMUL +& 1.U - SEW, LMUL))
  private val vs1NotAlign = SrcType.isVp(io.decodedInst.srcType(0)) && RegNumNotAlign(inst.VS1, vs1Emul)

  private val vs2IsMask = maskArithmeticInst || maskIndexInst
  private val vs2IsSingleElem = redWideningInst || vmvSingleInst
  private val vs2EewSel = Cat(lsIndexInst, (vs2WideningInst || narrowingInst || redWideningInst), intExt2, intExt4, intExt8)
  private val vs2Eew = LookupTreeDefault(vs2EewSel, SEW, List(
    "b10000".U  -> inst.WIDTH(1, 0),
    "b01000".U  -> (SEW + 1.U),
    "b00100".U  -> (SEW - 1.U),
    "b00010".U  -> (SEW - 2.U),
    "b00001".U  -> (SEW - 3.U)
  ))
  private val vs2EmulSel = Cat((vs2IsMask || vs2IsSingleElem), (vs2WideningInst || narrowingInst), vmvWholeInst, (intExtInst || lsIndexInst))
  private val vs2Emul = LookupTreeDefault(vs2EmulSel, LMUL, List(
    "b1000".U  -> "b100".U,
    "b0100".U  -> (LMUL + 1.U),
    "b0010".U  -> NFtoLmul(inst.NF),
    "b0001".U  -> (LMUL +& vs2Eew - SEW)
  ))
  private val vs2NotAlign = SrcType.isVp(io.decodedInst.srcType(1)) && RegNumNotAlign(inst.VS2, vs2Emul)

  private val vdIsMask = lsMaskInst || acsbInst || cmpInst || maskArithmeticInst
  private val vdIsSingleElem = redInst || redWideningInst || vmvSingleInst
  private val vdEew = Mux(lsStrideInst, inst.WIDTH(1, 0), Mux(vdWideningInst || redWideningInst, SEW + 1.U, SEW))
  private val vdEmulSel = Cat((vdIsMask || vdIsSingleElem), vdWideningInst, (lsWholeInst || vmvWholeInst), lsStrideInst)
  private val vdEmul = LookupTreeDefault(vdEmulSel, LMUL, List(
    "b1000".U  -> "b100".U,
    "b0100".U  -> (LMUL + 1.U),
    "b0010".U  -> NFtoLmul(inst.NF),
    "b0001".U  -> (LMUL +& vdEew - SEW)
  ))
  private val vdNotAlign = (SrcType.isVp(io.decodedInst.srcType(2)) || io.decodedInst.vecWen) && RegNumNotAlign(inst.VD, vdEmul)

  private val regNumIllegal = isVector && (vs1NotAlign || vs2NotAlign || vdNotAlign)

  // 6. v0 Overlap
  private val v0AllowOverlap = (vdIsMask || vdIsSingleElem) && !Seq(VMSBF_M, VMSIF_M, VMSOF_M).map(_ === inst.ALL).reduce(_ || _)
  private val v0Overlap = io.decodedInst.vecWen && inst.VM === 0.U && inst.VD === 0.U && !v0AllowOverlap

  // 7. Src Reg Overlap
  private val vs1RegLo = inst.VS1
  private val vs1RegHi = inst.VS1 +& LmultoRegNum(vs1Emul) - 1.U
  private val vs2RegLo = inst.VS2
  private val vs2RegHi = inst.VS2 +& LmultoRegNum(vs2Emul) - 1.U
  private val vdRegLo = inst.VD
  private val vdRegHi = Mux(lsStrideInst || lsIndexInst, segRegMax - 1.U, inst.VD + LmultoRegNum(vdEmul) - 1.U)

  private val notAllowOverlapInst = lsIndexInst && inst.NF =/= 0.U || Seq(VMSBF_M, VMSIF_M, VMSOF_M, VIOTA_M, 
                                    VSLIDEUP_VX, VSLIDEUP_VI, VSLIDE1UP_VX, VFSLIDE1UP_VF, VRGATHER_VV, VRGATHEREI16_VV, VRGATHER_VX, VRGATHER_VI, VCOMPRESS_VM).map(_ === inst.ALL).reduce(_ || _)

  //vs1
  private val vs1vdRegNotOverlap = vs1RegHi < vdRegLo || vdRegHi < vs1RegLo
  private val vs1Constraint1 = vs1IsMask && vdIsMask || !vs1IsMask && !vdIsMask && vs1Eew === vdEew
  private val vs1Constraint2 = (vdIsMask && !vs1IsMask || !vs1IsMask && !vdIsMask && vs1Eew > vdEew) && vdRegLo === vs1RegLo && vdRegHi <= vs1RegHi
  private val vs1Constraint3 = (!vdIsMask && vs1IsMask || !vs1IsMask && !vdIsMask && vs1Eew < vdEew) && vs1Emul >= "b100".U && vdRegHi === vs1RegHi && vdRegLo <= vs1RegLo
  private val vs1AllowOverlap = (vs1Constraint1 || vs1Constraint2 || vs1Constraint3 || vdIsSingleElem) && !notAllowOverlapInst
  private val vs1vdOverlap = (SrcType.isVp(io.decodedInst.srcType(0)) && io.decodedInst.vecWen) && !vs1vdRegNotOverlap && !vs1AllowOverlap
  //vs2
  private val vs2vdRegNotOverlap = vs2RegHi < vdRegLo || vdRegHi < vs2RegLo
  private val vs2Constraint1 = vs2IsMask && vdIsMask || !vs2IsMask && !vdIsMask && vs2Eew === vdEew
  private val vs2Constraint2 = (vdIsMask && !vs2IsMask || !vs2IsMask && !vdIsMask && vs2Eew > vdEew) && vdRegLo === vs2RegLo && vdRegHi <= vs2RegHi
  private val vs2Constraint3 = (!vdIsMask && vs2IsMask || !vs2IsMask && !vdIsMask && vs2Eew < vdEew) && vs2Emul >= "b100".U && vdRegHi === vs2RegHi && vdRegLo <= vs2RegLo
  private val vs2AllowOverlap = (vs2Constraint1 || vs2Constraint2 || vs2Constraint3 || vdIsSingleElem) && !notAllowOverlapInst
  private val vs2vdOverlap = (SrcType.isVp(io.decodedInst.srcType(1)) && io.decodedInst.vecWen) && !vs2vdRegNotOverlap && !vs2AllowOverlap

  private val regOverlapIllegal = v0Overlap || vs1vdOverlap || vs2vdOverlap

  io.illegalInst := instIllegal || villIllegal || eewIllegal || emulIllegal || regNumIllegal || regOverlapIllegal
}