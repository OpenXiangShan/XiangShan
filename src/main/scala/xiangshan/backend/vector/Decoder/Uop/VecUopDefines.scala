package xiangshan.backend.vector.Decoder.Uop

import xiangshan.backend.vector.Decoder.Uop.UopType._

object VecUopDefines {
  // vsetvl
  val vset_vtypex_vlx   = UopVecSet_VTYPEX_VLX()
  val vset_vtypex_vlmax = UopVecSet_VTYPEX_VLMAX()
  val vset_vtypex_vll   = UopVecSet_VTYPEX_VLL()
  // legal vsetvli
  val vset_vtypei_vlx   = UopVecSet_VTYPEI_VLX()
  val vset_vtypei_vlmax = UopVecSet_VTYPEI_VLMAX()
  val vset_vtypei_nop   = UopVecSet_VTYPEI_NOP()
  // legal vsetivli
  val vset_vtypei_vli   = UopVecSet_VTYPEI_VLI()
  // illegal vsetvli or vsetivli
  val vset_vtypei_ill   = UopVecSet_VTYPEI_ILL()

  // VI_VV_ULOOP_AVG
  // VI_VX_ULOOP_AVG
  // VI_VV_LOOP_AVG
  // VI_VX_LOOP_AVG
  val vaaddu = UopInt_S2UV_S1UVXI_DUV()
  val vasubu = UopInt_S2UV_S1UVXI_DUV()
  val vaadd  = UopInt_S2SV_S1SVXI_DSV()
  val vasub  = UopInt_S2SV_S1SVXI_DSV()

  // VI_VV_ULOOP
  val vandn = UopInt_S2UV_S1UVXI_DUV()
  val vand  = UopInt_S2UV_S1UVXI_DUV()
  val vor   = UopInt_S2UV_S1UVXI_DUV()
  val vxor  = UopInt_S2UV_S1UVXI_DUV()
  val vclmulh = UopInt_S2UV_S1UVXI_DUV()
  val vclmul  = UopInt_S2UV_S1UVXI_DUV()
  val vdivu = UopInt_S2UV_S1UVXI_DUV()
  val vremu = UopInt_S2UV_S1UVXI_DUV()
  val vmaxu = UopInt_S2UV_S1UVXI_DUV()
  val vminu = UopInt_S2UV_S1UVXI_DUV()
  val vmulhu = UopInt_S2UV_S1UVXI_DUV()
  val vrol = UopInt_S2UV_S1UVXI_DUV()
  val vror = UopInt_S2UV_S1UVXI_DUV()
  val vsll  = UopInt_S2SV_S1SVXI_DSV()
  val vsrl = UopInt_S2UV_S1UVXI_DUV()
  val vsaddu = UopInt_S2UV_S1UVXI_DUV().setOverflow
  val vssubu = UopInt_S2UV_S1UVXI_DUV().setOverflow
  val vssrl = UopInt_S2UV_S1UVXI_DUV().needIntRound
  // VI_VV_SU_LOOP
  // VI_VX_SU_LOOP
  val vmulhsu = UopInt_S2UV_S1SV_DSV()
  // VI_VI_MERGE_LOOP
  val vmerge = UopInt_S2UV_S1UVXI_DUV()
  // other to vector
  val vmvO2V = UopInt_S1UVXI_DV()
  val vmvX2S = UopInt_S1X_DA()
  val vmvS2X = UopInt_S2A_DX()

  // VI_VV_LOOP
  val vadd  = UopInt_S2SV_S1SVXI_DSV()
  val vsub  = UopInt_S2SV_S1SVXI_DSV()
  val vmulh = UopInt_S2SV_S1SVXI_DSV()
  val vmul  = UopInt_S2SV_S1SVXI_DSV()
  val vdiv  = UopInt_S2SV_S1SVXI_DSV()
  val vrem  = UopInt_S2SV_S1SVXI_DSV()
  val vmax  = UopInt_S2SV_S1SVXI_DSV()
  val vmin  = UopInt_S2SV_S1SVXI_DSV()
  val vmacc = UopInt_S2SV_S1SVXI_DSV().needAlwaysReadVd
  val vmadd = UopInt_S2SV_S1SVXI_DSV().needAlwaysReadVd
  val vnmsac= UopInt_S2SV_S1SVXI_DSV().needAlwaysReadVd
  val vnmsub= UopInt_S2SV_S1SVXI_DSV().needAlwaysReadVd
  val vsra  = UopInt_S2SV_S1SVXI_DSV()
  val vsadd = UopInt_S2SV_S1SVXI_DSV().setOverflow
  val vssub = UopInt_S2SV_S1SVXI_DSV().setOverflow
  val vsmul = UopInt_S2SV_S1SVXI_DSV().needIntRound.setOverflow
  val vssra = UopInt_S2SV_S1SVXI_DSV().needIntRound

  // VI_VV_LOOP_WIDEN
  // VI_VX_LOOP_WIDEN
  // 15 inst
  val vwaddu    = UopInt_S2UV_S1UV_DUW()
  val vwsubu    = UopInt_S2UV_S1UV_DUW()
  val vwsub     = UopInt_S2SV_S1SV_DSW()
  val vwadd     = UopInt_S2SV_S1SV_DSW()

  val vwmaccu   = UopInt_S2UV_S1UV_DUW().needAlwaysReadVd
  val vwmacc    = UopInt_S2SV_S1SV_DSW().needAlwaysReadVd
  val vwmulu    = UopInt_S2UV_S1UV_DUW()
  val vwmul     = UopInt_S2SV_S1SV_DSW()

  val vwmaccsu  = UopInt_S2UV_S1SV_DSW().needAlwaysReadVd
  val vwmaccus  = UopInt_S2SV_S1UV_DSW().needAlwaysReadVd
  val vwmulsu   = UopInt_S2UV_S1SV_DSW()

  val vwaddu_w  = UopInt_S2UW_S1UV_DUW()
  val vwsubu_w  = UopInt_S2UW_S1UV_DUW()
  val vwadd_w   = UopInt_S2SW_S1SV_DSW()
  val vwsub_w   = UopInt_S2SW_S1SV_DSW()

  // VI_VI_LOOP_NARROW
  // VI_VI_LOOP_NSHIFT
  val vnclipu   = UopInt_S2UW_S1UVXI_DUV().needIntRound.setOverflow
  val vnclip    = UopInt_S2SW_S1UVXI_DSV().needIntRound.setOverflow
  val vnsrl     = UopInt_S2UW_S1UVXI_DUV()
  val vnsra     = UopInt_S2SW_S1UVXI_DSV()

  // VI_VV_LOOP_WITH_CARRY
  // VI_XI_LOOP_WITH_CARRY
  // VI_VV_LOOP_CARRY
  // VI_XI_LOOP_CARRY
  val vadc   = UopIntCarry_S2V_S1VXI_V0C_DV()
  val vsbc   = UopIntCarry_S2V_S1VXI_V0C_DV()
  val vmadc_vvm = UopIntCarry_S2V_S1VXI_V0C_DM()
  val vmadc_vv  = UopIntCarry_S2V_S1VXI_V0N_DM()
  val vmsbc_vvm = UopIntCarry_S2V_S1VXI_V0C_DM()
  val vmsbc_vv  = UopIntCarry_S2V_S1VXI_V0N_DM()

  // VI_VV_LOOP_CMP
  // VI_VX_LOOP_CMP
  // VI_VI_LOOP_CMP
  val vmseq = UopIntCmp_S2SV_S1SVXI_DM() // ==
  val vmsgt = UopIntCmp_S2SV_S1SVXI_DM() // >
  val vmsle = UopIntCmp_S2SV_S1SVXI_DM() // <=
  val vmslt = UopIntCmp_S2SV_S1SVXI_DM() // <
  val vmsne = UopIntCmp_S2SV_S1SVXI_DM() // !=

  val vmsgtu = UopIntCmp_S2UV_S1UVXI_DM() // >
  val vmsleu = UopIntCmp_S2UV_S1UVXI_DM() // <=
  val vmsltu = UopIntCmp_S2UV_S1UVXI_DM() // <

  // VI_LOOP_MASK
  val vmand  = UopInt_S2M_S1M_DM()
  val vmandn = UopInt_S2M_S1M_DM()
  val vmnand = UopInt_S2M_S1M_DM()
  val vmnor  = UopInt_S2M_S1M_DM()
  val vmor   = UopInt_S2M_S1M_DM()
  val vmorn  = UopInt_S2M_S1M_DM()
  val vmxnor = UopInt_S2M_S1M_DM()
  val vmxor  = UopInt_S2M_S1M_DM()

  // Shuffle
  val vrgather_vv = UopShuffle()
  val vrgather_xi = UopShuffle()
  val vslide1up   = UopShuffle()
  val vslide1down = UopShuffle()
  val vslideup    = UopShuffle()
  val vslidedown  = UopShuffle()
  val vcompress   = UopShuffle()

  // 1 src op
  val vbrev8  = UopInt_S2UV_DV()
  val vbrev   = UopInt_S2UV_DV()
  val vclz    = UopInt_S2UV_DV()
  val vctz    = UopInt_S2UV_DV()
  val vcpop_v = UopInt_S2UV_DV()
  val vrev8   = UopInt_S2UV_DV()

  val vzext2  = UopInt_S2UV_DV()
  val vzext4  = UopInt_S2UV_DV()
  val vzext8  = UopInt_S2UV_DV()

  val vsext2  = UopInt_S2SV_DV()
  val vsext4  = UopInt_S2SV_DV()
  val vsext8  = UopInt_S2SV_DV()
  // 1 src op mask operation
  val vmsbf   = UopInt_S2M_DM()
  val vmsif   = UopInt_S2M_DM()
  val vmsof   = UopInt_S2M_DM()

  val viota   = UopInt_S2M_DV()

  val vcpop_m = UopInt_S2M_DX()
  val vfirst  = UopInt_S2M_DX()
  // mov
  val vmvnr   = UopInt_S2UV_DV()

  // 0 src op
  val vid     = UopInt_DV()

  // reduction
  val vredand   = UopIntRed_S1UA_S2UV_DUA()
  val vredor    = UopIntRed_S1UA_S2UV_DUA()
  val vredxor   = UopIntRed_S1UA_S2UV_DUA()
  val vredmaxu  = UopIntRed_S1UA_S2UV_DUA()
  val vredminu  = UopIntRed_S1UA_S2UV_DUA()
  val vredmax   = UopIntRed_S1SA_S2SV_DSA()
  val vredmin   = UopIntRed_S1SA_S2SV_DSA()
  val vredsum   = UopIntRed_S1SA_S2SV_DSA()

  val vwredsum  = UopIntWRed_S1SA_S2SV_DSA()
  val vwredsumu = UopIntWRed_S1UA_S2UV_DUA()

  val vmvS2F = UopFp_S2A_DF()

  val vmvF2S = UopFp_S1F_DA()

  val vmvF2V = UopFp_S1F_DV()

  val vfclass = UopFp_S2V_DV()

  val vfcvt_f_x = UopFp_S2V_DV()
  val vfcvt_f_xu = UopFp_S2V_DV()
  val vfcvt_x_f = UopFp_S2V_DV()
  val vfcvt_x_f_rtz = UopFp_S2V_DV()
  val vfcvt_xu_f = UopFp_S2V_DV()
  val vfcvt_xu_f_rtz = UopFp_S2V_DV()

  val vfrec7 = UopFp_S2V_DV()
  val vfrsqrt7 = UopFp_S2V_DV()
  val vfsqrt = UopFp_S2V_DV()

  val vfwcvt_f_f      = UopFp_S2V_DW()
  val vfwcvt_f_x      = UopFp_S2V_DW()
  val vfwcvt_f_xu     = UopFp_S2V_DW()
  val vfwcvt_x_f      = UopFp_S2V_DW()
  val vfwcvt_x_f_rtz  = UopFp_S2V_DW()
  val vfwcvt_xu_f     = UopFp_S2V_DW()
  val vfwcvt_xu_f_rtz = UopFp_S2V_DW()

  val vfncvt_f_f      = UopFp_S2W_DV()
  val vfncvt_f_x      = UopFp_S2W_DV()
  val vfncvt_f_xu     = UopFp_S2W_DV()
  val vfncvt_f_f_rod  = UopFp_S2W_DV()
  val vfncvt_x_f_rtz  = UopFp_S2W_DV()
  val vfncvt_x_f      = UopFp_S2W_DV()
  val vfncvt_xu_f_rtz = UopFp_S2W_DV()
  val vfncvt_xu_f     = UopFp_S2W_DV()

  val vfadd = UopFp_S2V_S1VF_DV()
  val vfsub = UopFp_S2V_S1VF_DV()
  val vfmin = UopFp_S2V_S1VF_DV()
  val vfmax = UopFp_S2V_S1VF_DV()
  val vfsgnj = UopFp_S2V_S1VF_DV()
  val vfsgnjn = UopFp_S2V_S1VF_DV()
  val vfsgnjx = UopFp_S2V_S1VF_DV()
  val vfmerge = UopFp_S2V_S1VF_DV()

  val vfdiv   = UopFp_S2V_S1VF_DV()
  val vfmul   = UopFp_S2V_S1VF_DV()
  val vfmadd  = UopFp_S2V_S1VF_DV().needAlwaysReadVd
  val vfnmadd = UopFp_S2V_S1VF_DV().needAlwaysReadVd
  val vfmsub  = UopFp_S2V_S1VF_DV().needAlwaysReadVd
  val vfnmsub = UopFp_S2V_S1VF_DV().needAlwaysReadVd
  val vfmacc  = UopFp_S2V_S1VF_DV().needAlwaysReadVd
  val vfnmacc = UopFp_S2V_S1VF_DV().needAlwaysReadVd
  val vfmsac  = UopFp_S2V_S1VF_DV().needAlwaysReadVd
  val vfnmsac = UopFp_S2V_S1VF_DV().needAlwaysReadVd

  val vfwadd  = UopFp_S2V_S1V_DW()
  val vfwsub  = UopFp_S2V_S1V_DW()
  val vfwmul  = UopFp_S2V_S1V_DW()

  val vfwadd_w = UopFp_S2W_S1V_DW()
  val vfwsub_w = UopFp_S2W_S1V_DW()

  val vfwmacc  = UopFp_S2V_S1V_DW().needAlwaysReadVd
  val vfwmsac  = UopFp_S2V_S1V_DW().needAlwaysReadVd
  val vfwnmacc = UopFp_S2V_S1V_DW().needAlwaysReadVd
  val vfwnmsac = UopFp_S2V_S1V_DW().needAlwaysReadVd

  val vmfeq = UopFp_S2V_S1V_DM()
  val vmfle = UopFp_S2V_S1V_DM()
  val vmflt = UopFp_S2V_S1V_DM()
  val vmfne = UopFp_S2V_S1V_DM()
  val vmfgt = UopFp_S2V_S1V_DM()
  val vmfge = UopFp_S2V_S1V_DM()

  val vfredosum = UopFpRed_S2V_S1A_DA()
  val vfredmin = UopFpRed_S2V_S1A_DA()
  val vfredmax = UopFpRed_S2V_S1A_DA()

  val vfwredosum = UopFpWRed_S2V_S1A_DA()

  val vle = UopUnitStrideLoad()
  val vleWhole = UopWholeRegisterLoad()
  val vlm = UopMaskLoad()
  val vlse = UopStridedLoad()
  val vlxe = UopIndexLoad()
  val vleff = UopUnitStrideLoad()

  val vse = UopUnitStrideStore()
  val vseWhole = UopWholeRegisterStore()
  val vsm = UopMaskStore()
  val vsse = UopStridedStore()
  val vsxe = UopIndexStore()

  // internal uops

  /**
   * This uop is used to fill tail of Vector Registers
   * E.g.
   */
  val vtail = UopInt_DV()
}
