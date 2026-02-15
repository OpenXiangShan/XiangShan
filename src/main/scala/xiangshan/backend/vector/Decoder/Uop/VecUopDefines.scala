package xiangshan.backend.vector.Decoder.Uop

import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.decode.opcode.Opcode._
import xiangshan.backend.vector.Decoder.Types.Sign
import xiangshan.backend.vector.Decoder.Uop.UopTrait._

object VecUopDefines {
  /**
   * [[vset_vtypex_vlx]] is used for [[VSETVL]] when rs1 != x0
   */
  def vset_vtypex_vlx = VecConfigUop(GpWen, VlWen, Src2Gp, Src1Gp).setOpcode(VSetOpcodes.uvset_vtypex_vlx)

  /**
   * [[vset_vtypex_vlmax]] is used for [[VSETVL]] when rs1 == x0 and rd != x0
   */
  def vset_vtypex_vlmax = VecConfigUop(GpWen, VlWen, Src2Gp).setOpcode(VSetOpcodes.uvset_vtypex_vlmax)

  /**
   * [[vset_vtypex_vll]] is used for [[VSETVL]] when rs1 == x0 and rd == x0
   */
  def vset_vtypex_vll = VecConfigUop(VlWen, Src2Gp, VlRen).setOpcode(VSetOpcodes.uvset_vtypex_vll)

  /**
   * [[vset_vtypei_vlx]] is used for [[VSETVLI]] when rs1 != x0
   */
  def vset_vtypei_vlx = new VecConfigUop(GpWen, VlWen, Src1Gp).setOpcode(VSetOpcodes.uvset_vtypei_vlx)

  /**
   * [[vset_vtypei_vlmax]] is used for [[VSETVLI]] when rs1 == x0
   */
  def vset_vtypei_vlmax = new VecConfigUop(GpWen, VlWen).setOpcode(VSetOpcodes.uvset_vtypei_vlmax)

  /**
   * [[vset_vtypei_nop]] is used for [[VSETVLI]] when rs1 == x0 and rd == x0
   * This uop does not change vl but modifies vtype.
   * if vlmax shrink, [[vset_vtypei_ill]] should be used to set vill
   */
  def vset_vtypei_nop = VecConfigUop().setOpcode(VSetOpcodes.uvset_vtypei_nop)

  /**
   * [[vset_vtypei_vli]] is used for [[VSETIVLI]]
   */
  def vset_vtypei_vli = VecConfigUop(GpWen, VlWen).setOpcode(VSetOpcodes.uvset_vtypei_vli)

  /**
   * [[vset_vtypei_ill]] is used for illegal [[VSETVLI]] and [[VSETIVLI]] when rs1 == x0 and rd == x0.
   * When rs1 == x0, rd == x0 and SEW/LMUL ratio is changed, the instruction is reserved.
   * This uop will set vill = 1 and vl = 0.
   */
  def vset_vtypei_ill = VecConfigUop(GpWen, VlWen).setOpcode(VSetOpcodes.uvset_ill)

  // 11.1. Vector Single-Width Integer Add and Subtract
  def vadd_e8  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vadd_e8)
  def vadd_e16 = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vadd_e16)
  def vadd_e32 = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vadd_e32)
  def vadd_e64 = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vadd_e64)
  def vsub_e8  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsub_e8)
  def vsub_e16 = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsub_e16)
  def vsub_e32 = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsub_e32)
  def vsub_e64 = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsub_e64)

  // 11.2. Vector Widening Integer Add/Subtract
  def vwaddu_e8    = VecIntUopVV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwaddu_e8)
  def vwaddu_e16   = VecIntUopVV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwaddu_e16)
  def vwaddu_e32   = VecIntUopVV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwaddu_e32)
  def vwsubu_e8    = VecIntUopVV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwsubu_e8)
  def vwsubu_e16   = VecIntUopVV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwsubu_e16)
  def vwsubu_e32   = VecIntUopVV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwsubu_e32)
  def vwadd_e8     = VecIntUopVV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwadd_e8)
  def vwadd_e16    = VecIntUopVV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwadd_e16)
  def vwadd_e32    = VecIntUopVV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwadd_e32)
  def vwsub_e8     = VecIntUopVV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwsub_e8)
  def vwsub_e16    = VecIntUopVV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwsub_e16)
  def vwsub_e32    = VecIntUopVV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwsub_e32)
  def vwaddu_w_e8  = VecIntUopWV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwaddu_w_e8)
  def vwaddu_w_e16 = VecIntUopWV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwaddu_w_e16)
  def vwaddu_w_e32 = VecIntUopWV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwaddu_w_e32)
  def vwsubu_w_e8  = VecIntUopWV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwsubu_w_e8)
  def vwsubu_w_e16 = VecIntUopWV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwsubu_w_e16)
  def vwsubu_w_e32 = VecIntUopWV_DW().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vwsubu_w_e32)
  def vwadd_w_e8   = VecIntUopWV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwadd_w_e8)
  def vwadd_w_e16  = VecIntUopWV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwadd_w_e16)
  def vwadd_w_e32  = VecIntUopWV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwadd_w_e32)
  def vwsub_w_e8   = VecIntUopWV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwsub_w_e8)
  def vwsub_w_e16  = VecIntUopWV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwsub_w_e16)
  def vwsub_w_e32  = VecIntUopWV_DW().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vwsub_w_e32)

  // 11.3. Vector Integer Extension
  def vzext2_e8  = VecIntUopS2V_DV().set(_.src2Sgn, Sign.U).setOpcode(VIAluOpcodes.vzext2_e8 )
  def vzext2_e16 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.U).setOpcode(VIAluOpcodes.vzext2_e16)
  def vzext2_e32 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.U).setOpcode(VIAluOpcodes.vzext2_e32)
  def vzext4_e8  = VecIntUopS2V_DV().set(_.src2Sgn, Sign.U).setOpcode(VIAluOpcodes.vzext4_e8 )
  def vzext4_e16 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.U).setOpcode(VIAluOpcodes.vzext4_e16)
  def vzext8_e8  = VecIntUopS2V_DV().set(_.src2Sgn, Sign.U).setOpcode(VIAluOpcodes.vzext8_e8 )
  def vsext2_e8  = VecIntUopS2V_DV().set(_.src2Sgn, Sign.S).setOpcode(VIAluOpcodes.vsext2_e8 )
  def vsext2_e16 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.S).setOpcode(VIAluOpcodes.vsext2_e16)
  def vsext2_e32 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.S).setOpcode(VIAluOpcodes.vsext2_e32)
  def vsext4_e8  = VecIntUopS2V_DV().set(_.src2Sgn, Sign.S).setOpcode(VIAluOpcodes.vsext4_e8 )
  def vsext4_e16 = VecIntUopS2V_DV().set(_.src2Sgn, Sign.S).setOpcode(VIAluOpcodes.vsext4_e16)
  def vsext8_e8  = VecIntUopS2V_DV().set(_.src2Sgn, Sign.S).setOpcode(VIAluOpcodes.vsext8_e8 )

  // 11.4. Vector Integer Add-with-Carry / Subtract-with-Borrow Instructions
  def vadc_e8   = VecIntUopVVM_DV().setOpcode(VIAluOpcodes.vadc_e8)
  def vadc_e16  = VecIntUopVVM_DV().setOpcode(VIAluOpcodes.vadc_e16)
  def vadc_e32  = VecIntUopVVM_DV().setOpcode(VIAluOpcodes.vadc_e32)
  def vadc_e64  = VecIntUopVVM_DV().setOpcode(VIAluOpcodes.vadc_e64)
  def vmadc_e8  = VecIntUopVVM_DM().setOpcode(VIAluOpcodes.vmadc_e8)
  def vmadc_e16 = VecIntUopVVM_DM().setOpcode(VIAluOpcodes.vmadc_e16)
  def vmadc_e32 = VecIntUopVVM_DM().setOpcode(VIAluOpcodes.vmadc_e32)
  def vmadc_e64 = VecIntUopVVM_DM().setOpcode(VIAluOpcodes.vmadc_e64)
  def vsbc_e8   = VecIntUopVVM_DV().setOpcode(VIAluOpcodes.vsbc_e8)
  def vsbc_e16  = VecIntUopVVM_DV().setOpcode(VIAluOpcodes.vsbc_e16)
  def vsbc_e32  = VecIntUopVVM_DV().setOpcode(VIAluOpcodes.vsbc_e32)
  def vsbc_e64  = VecIntUopVVM_DV().setOpcode(VIAluOpcodes.vsbc_e64)
  def vmsbc_e8  = VecIntUopVVM_DM().setOpcode(VIAluOpcodes.vmsbc_e8)
  def vmsbc_e16 = VecIntUopVVM_DM().setOpcode(VIAluOpcodes.vmsbc_e16)
  def vmsbc_e32 = VecIntUopVVM_DM().setOpcode(VIAluOpcodes.vmsbc_e32)
  def vmsbc_e64 = VecIntUopVVM_DM().setOpcode(VIAluOpcodes.vmsbc_e64)

  // 11.5. Vector Bitwise Logical Instructions
  def vand_e8  = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vand_e8)
  def vand_e16 = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vand_e16)
  def vand_e32 = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vand_e32)
  def vand_e64 = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vand_e64)
  def vor_e8   = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vor_e8)
  def vor_e16  = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vor_e16)
  def vor_e32  = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vor_e32)
  def vor_e64  = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vor_e64)
  def vxor_e8  = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vxor_e8)
  def vxor_e16 = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vxor_e16)
  def vxor_e32 = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vxor_e32)
  def vxor_e64 = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vxor_e64)

  // 11.6. Vector Single-Width Shift Instructions
  def vsll_e8  = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vsll_e8)
  def vsll_e16 = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vsll_e16)
  def vsll_e32 = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vsll_e32)
  def vsll_e64 = VecIntUopVV_DV().setOpcode(VIAluOpcodes.vsll_e64)
  def vsrl_e8  = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vsrl_e8)
  def vsrl_e16 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vsrl_e16)
  def vsrl_e32 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vsrl_e32)
  def vsrl_e64 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vsrl_e64)
  def vsra_e8  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsra_e8)
  def vsra_e16 = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsra_e16)
  def vsra_e32 = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsra_e32)
  def vsra_e64 = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsra_e64)

  // 11.7. Vector Narrowing Integer Right Shift Instructions
  def vnsrl_e8  = VecIntUopWV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vnsrl_e8)
  def vnsrl_e16 = VecIntUopWV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vnsrl_e16)
  def vnsrl_e32 = VecIntUopWV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vnsrl_e32)
  def vnsra_e8  = VecIntUopWV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vnsra_e8)
  def vnsra_e16 = VecIntUopWV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vnsra_e16)
  def vnsra_e32 = VecIntUopWV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vnsra_e32)

  // 11.8. Vector Integer Compare Instructions
  def vmseq_e8  = VecIntUopVV_DM().setOpcode(VIAluOpcodes.vmseq_e32)
  def vmseq_e16 = VecIntUopVV_DM().setOpcode(VIAluOpcodes.vmseq_e32)
  def vmseq_e32 = VecIntUopVV_DM().setOpcode(VIAluOpcodes.vmseq_e32)
  def vmseq_e64 = VecIntUopVV_DM().setOpcode(VIAluOpcodes.vmseq_e64)
  def vmsne_e8  = VecIntUopVV_DM().setOpcode(VIAluOpcodes.vmsne_e32)
  def vmsne_e16 = VecIntUopVV_DM().setOpcode(VIAluOpcodes.vmsne_e32)
  def vmsne_e32 = VecIntUopVV_DM().setOpcode(VIAluOpcodes.vmsne_e32)
  def vmsne_e64 = VecIntUopVV_DM().setOpcode(VIAluOpcodes.vmsne_e64)
  def vmsgt_e8  = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmsgt_e32)
  def vmsgt_e16 = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmsgt_e32)
  def vmsgt_e32 = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmsgt_e32)
  def vmsgt_e64 = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmsgt_e64)
  def vmsle_e8  = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmsle_e32)
  def vmsle_e16 = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmsle_e32)
  def vmsle_e32 = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmsle_e32)
  def vmsle_e64 = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmsle_e64)
  def vmslt_e8  = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmslt_e32)
  def vmslt_e16 = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmslt_e32)
  def vmslt_e32 = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmslt_e32)
  def vmslt_e64 = VecIntUopVV_DM().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmslt_e64)
  def vmsgtu_e8  = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsgtu_e32)
  def vmsgtu_e16 = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsgtu_e32)
  def vmsgtu_e32 = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsgtu_e32)
  def vmsgtu_e64 = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsgtu_e64)
  def vmsleu_e8  = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsleu_e32)
  def vmsleu_e16 = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsleu_e32)
  def vmsleu_e32 = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsleu_e32)
  def vmsleu_e64 = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsleu_e64)
  def vmsltu_e8  = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsltu_e32)
  def vmsltu_e16 = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsltu_e32)
  def vmsltu_e32 = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsltu_e32)
  def vmsltu_e64 = VecIntUopVV_DM().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmsltu_e64)

  // 11.9. Vector Integer Min/Max Instructions
  def vmaxu_e8  = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmaxu_e8 )
  def vmaxu_e16 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmaxu_e16)
  def vmaxu_e32 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmaxu_e32)
  def vmaxu_e64 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vmaxu_e64)
  def vminu_e8  = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vminu_e8)
  def vminu_e16 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vminu_e16)
  def vminu_e32 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vminu_e32)
  def vminu_e64 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vminu_e64)
  def vmax_e8   = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmax_e8)
  def vmax_e16  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmax_e16)
  def vmax_e32  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmax_e32)
  def vmax_e64  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmax_e64)
  def vmin_e8   = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmin_e8)
  def vmin_e16  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmin_e16)
  def vmin_e32  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmin_e32)
  def vmin_e64  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vmin_e64)

  // 11.10. Vector Single-Width Integer Multiply Instructions
  def vmulh_e8    = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vmulh_e8 )
  def vmulh_e16   = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vmulh_e16)
  def vmulh_e32   = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vmulh_e32)
  def vmulh_e64   = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vmulh_e64)
  def vmul_e8     = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vmul_e8 )
  def vmul_e16    = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vmul_e16)
  def vmul_e32    = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vmul_e32)
  def vmul_e64    = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vmul_e64)
  def vmulhu_e8   = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIMacOpcodes.vmulhu_e8 )
  def vmulhu_e16  = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIMacOpcodes.vmulhu_e16)
  def vmulhu_e32  = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIMacOpcodes.vmulhu_e32)
  def vmulhu_e64  = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIMacOpcodes.vmulhu_e64)
  def vmulhsu_e8  = VecIntUopVV_DV().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S).set(_.destSgn, Sign.S).setOpcode(VIMacOpcodes.vmulhsu_e8 )
  def vmulhsu_e16 = VecIntUopVV_DV().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S).set(_.destSgn, Sign.S).setOpcode(VIMacOpcodes.vmulhsu_e16)
  def vmulhsu_e32 = VecIntUopVV_DV().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S).set(_.destSgn, Sign.S).setOpcode(VIMacOpcodes.vmulhsu_e32)
  def vmulhsu_e64 = VecIntUopVV_DV().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S).set(_.destSgn, Sign.S).setOpcode(VIMacOpcodes.vmulhsu_e64)

  // 11.11. Vector Integer Divide Instructions
  def vdiv_e8   = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIDivOpcodes.vdiv_e8)
  def vdiv_e16  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIDivOpcodes.vdiv_e16)
  def vdiv_e32  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIDivOpcodes.vdiv_e32)
  def vdiv_e64  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIDivOpcodes.vdiv_e64)
  def vrem_e8   = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIDivOpcodes.vrem_e8)
  def vrem_e16  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIDivOpcodes.vrem_e16)
  def vrem_e32  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIDivOpcodes.vrem_e32)
  def vrem_e64  = VecIntUopVV_DV().set(_.sgn, Sign.S).setOpcode(VIDivOpcodes.vrem_e64)
  def vdivu_e8  = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIDivOpcodes.vdivu_e8)
  def vdivu_e16 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIDivOpcodes.vdivu_e16)
  def vdivu_e32 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIDivOpcodes.vdivu_e32)
  def vdivu_e64 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIDivOpcodes.vdivu_e64)
  def vremu_e8  = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIDivOpcodes.vremu_e8)
  def vremu_e16 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIDivOpcodes.vremu_e16)
  def vremu_e32 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIDivOpcodes.vremu_e32)
  def vremu_e64 = VecIntUopVV_DV().set(_.sgn, Sign.U).setOpcode(VIDivOpcodes.vremu_e64)

  // 11.12. Vector Widening Integer Multiply Instructions
  def vwmulu_e8   = VecIntUopVV_DW().set(_.sgn, Sign.U).setOpcode(VIMacOpcodes.vwmulu_e8 )
  def vwmulu_e16  = VecIntUopVV_DW().set(_.sgn, Sign.U).setOpcode(VIMacOpcodes.vwmulu_e16)
  def vwmulu_e32  = VecIntUopVV_DW().set(_.sgn, Sign.U).setOpcode(VIMacOpcodes.vwmulu_e32)
  def vwmul_e8    = VecIntUopVV_DW().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vwmul_e8 )
  def vwmul_e16   = VecIntUopVV_DW().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vwmul_e16)
  def vwmul_e32   = VecIntUopVV_DW().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vwmul_e32)
  def vwmulsu_e8  = VecIntUopVV_DW().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S).setOpcode(VIMacOpcodes.vwmulsu_e8 )
  def vwmulsu_e16 = VecIntUopVV_DW().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S).setOpcode(VIMacOpcodes.vwmulsu_e16)
  def vwmulsu_e32 = VecIntUopVV_DW().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S).setOpcode(VIMacOpcodes.vwmulsu_e32)

  // 11.13. Vector Single-Width Integer Multiply-Add Instructions
  def vmacc_e8   = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vmacc_e8 )
  def vmacc_e16  = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vmacc_e16)
  def vmacc_e32  = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vmacc_e32)
  def vmacc_e64  = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vmacc_e64)
  def vmadd_e8   = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vmadd_e8 )
  def vmadd_e16  = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vmadd_e16)
  def vmadd_e32  = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vmadd_e32)
  def vmadd_e64  = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vmadd_e64)
  def vnmsac_e8  = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vnmsac_e8 )
  def vnmsac_e16 = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vnmsac_e16)
  def vnmsac_e32 = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vnmsac_e32)
  def vnmsac_e64 = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vnmsac_e64)
  def vnmsub_e8  = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vnmsub_e8 )
  def vnmsub_e16 = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vnmsub_e16)
  def vnmsub_e32 = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vnmsub_e32)
  def vnmsub_e64 = VecIntUopVVV_DV().setOpcode(VIMacOpcodes.vnmsub_e64)

  // 11.14. Vector Widening Integer Multiply-Add Instructions
  def vwmaccu_e8   = VecIntUopVVW_DW().set(_.sgn, Sign.U).setOpcode(VIMacOpcodes.vwmaccu_e8 )
  def vwmaccu_e16  = VecIntUopVVW_DW().set(_.sgn, Sign.U).setOpcode(VIMacOpcodes.vwmaccu_e16)
  def vwmaccu_e32  = VecIntUopVVW_DW().set(_.sgn, Sign.U).setOpcode(VIMacOpcodes.vwmaccu_e32)
  def vwmacc_e8    = VecIntUopVVW_DW().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vwmacc_e8 )
  def vwmacc_e16   = VecIntUopVVW_DW().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vwmacc_e16)
  def vwmacc_e32   = VecIntUopVVW_DW().set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vwmacc_e32)
  def vwmaccsu_e8  = VecIntUopVVW_DW().set(_.src1Sgn, Sign.S).set(_.src2Sgn, Sign.U).setOpcode(VIMacOpcodes.vwmaccsu_e8 )
  def vwmaccsu_e16 = VecIntUopVVW_DW().set(_.src1Sgn, Sign.S).set(_.src2Sgn, Sign.U).setOpcode(VIMacOpcodes.vwmaccsu_e16)
  def vwmaccsu_e32 = VecIntUopVVW_DW().set(_.src1Sgn, Sign.S).set(_.src2Sgn, Sign.U).setOpcode(VIMacOpcodes.vwmaccsu_e32)
  def vwmaccus_e8  = VecIntUopVVW_DW().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S).setOpcode(VIMacOpcodes.vwmaccus_e8 )
  def vwmaccus_e16 = VecIntUopVVW_DW().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S).setOpcode(VIMacOpcodes.vwmaccus_e16)
  def vwmaccus_e32 = VecIntUopVVW_DW().set(_.src1Sgn, Sign.U).set(_.src2Sgn, Sign.S).setOpcode(VIMacOpcodes.vwmaccus_e32)

  // 11.15. Vector Integer Merge Instructions
  def vmerge_e8  = VecIntUopVV_DV().setOpcode(VMoveOpcodes.vmerge_vv_e8 )
  def vmerge_e16 = VecIntUopVV_DV().setOpcode(VMoveOpcodes.vmerge_vv_e16)
  def vmerge_e32 = VecIntUopVV_DV().setOpcode(VMoveOpcodes.vmerge_vv_e32)
  def vmerge_e64 = VecIntUopVV_DV().setOpcode(VMoveOpcodes.vmerge_vv_e64)

  // 11.16. Vector Integer Move Instructions
  def vmv_x2v_e8  = VecIntUopS1(Src1Gp , VpWen).setOpcode(VMoveOpcodes.vmerge_vx_e8 )
  def vmv_x2v_e16 = VecIntUopS1(Src1Gp , VpWen).setOpcode(VMoveOpcodes.vmerge_vx_e16)
  def vmv_x2v_e32 = VecIntUopS1(Src1Gp , VpWen).setOpcode(VMoveOpcodes.vmerge_vx_e32)
  def vmv_x2v_e64 = VecIntUopS1(Src1Gp , VpWen).setOpcode(VMoveOpcodes.vmerge_vx_e64)
  def vmv_v2v_e8  = VecIntUopS1(Src1Vp , VpWen).setOpcode(VMoveOpcodes.vmerge_vv_e8 )
  def vmv_v2v_e16 = VecIntUopS1(Src1Vp , VpWen).setOpcode(VMoveOpcodes.vmerge_vv_e16)
  def vmv_v2v_e32 = VecIntUopS1(Src1Vp , VpWen).setOpcode(VMoveOpcodes.vmerge_vv_e32)
  def vmv_v2v_e64 = VecIntUopS1(Src1Vp , VpWen).setOpcode(VMoveOpcodes.vmerge_vv_e64)
  def vmv_i2v_e8  = VecIntUopS1(Src1Imm, VpWen).setOpcode(VMoveOpcodes.vmerge_vx_e8 )
  def vmv_i2v_e16 = VecIntUopS1(Src1Imm, VpWen).setOpcode(VMoveOpcodes.vmerge_vx_e16)
  def vmv_i2v_e32 = VecIntUopS1(Src1Imm, VpWen).setOpcode(VMoveOpcodes.vmerge_vx_e32)
  def vmv_i2v_e64 = VecIntUopS1(Src1Imm, VpWen).setOpcode(VMoveOpcodes.vmerge_vx_e64)

  // 12.1. Vector Single-Width Saturating Add and Subtract
  def vsaddu_e8  = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vsaddu_e8)
  def vsaddu_e16 = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vsaddu_e16)
  def vsaddu_e32 = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vsaddu_e32)
  def vsaddu_e64 = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vsaddu_e64)
  def vssubu_e8  = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vssubu_e8)
  def vssubu_e16 = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vssubu_e16)
  def vssubu_e32 = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vssubu_e32)
  def vssubu_e64 = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vssubu_e64)
  def vsadd_e8   = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsadd_e8)
  def vsadd_e16  = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsadd_e16)
  def vsadd_e32  = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsadd_e32)
  def vsadd_e64  = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vsadd_e64)
  def vssub_e8   = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vssub_e8)
  def vssub_e16  = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vssub_e16)
  def vssub_e32  = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vssub_e32)
  def vssub_e64  = VecIntFixUopVV_DV(VxsatWen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vssub_e64)

  // 12.2. Vector Single-Width Averaging
  def vaaddu_e8  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vaaddu_e8)
  def vaaddu_e16 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vaaddu_e16)
  def vaaddu_e32 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vaaddu_e32)
  def vaaddu_e64 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vaaddu_e64)
  def vasubu_e8  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vasubu_e8)
  def vasubu_e16 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vasubu_e16)
  def vasubu_e32 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vasubu_e32)
  def vasubu_e64 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vasubu_e64)
  def vaadd_e8   = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vaadd_e8)
  def vaadd_e16  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vaadd_e16)
  def vaadd_e32  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vaadd_e32)
  def vaadd_e64  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vaadd_e64)
  def vasub_e8   = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vasub_e8)
  def vasub_e16  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vasub_e16)
  def vasub_e32  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vasub_e32)
  def vasub_e64  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vasub_e64)

  // 12.3. Vector Single-Width Fractional Multiply with Rounding and Saturation
  def vsmul_e8  = VecIntFixUopVV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vsmul_e8)
  def vsmul_e16 = VecIntFixUopVV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vsmul_e16)
  def vsmul_e32 = VecIntFixUopVV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vsmul_e32)
  def vsmul_e64 = VecIntFixUopVV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.S).setOpcode(VIMacOpcodes.vsmul_e64)

  // 12.4. Vector Single-Width Scaling Shift Instructions
  def vssrl_e8  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vssrl_e8)
  def vssrl_e16 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vssrl_e16)
  def vssrl_e32 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vssrl_e32)
  def vssrl_e64 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vssrl_e64)
  def vssra_e8  = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vssra_e8)
  def vssra_e16 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vssra_e16)
  def vssra_e32 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vssra_e32)
  def vssra_e64 = VecIntFixUopVV_DV(VxrmRen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vssra_e64)

  // 12.5. Vector Narrowing Fixed-Point Clip Instructions
  def vnclipu_e8  = VecIntFixUopWV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vnclipu_e8)
  def vnclipu_e16 = VecIntFixUopWV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vnclipu_e16)
  def vnclipu_e32 = VecIntFixUopWV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.U).setOpcode(VIAluOpcodes.vnclipu_e32)
  def vnclip_e8   = VecIntFixUopWV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vnclip_e8)
  def vnclip_e16  = VecIntFixUopWV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vnclip_e16)
  def vnclip_e32  = VecIntFixUopWV_DV(VxsatWen, VxrmRen).set(_.sgn, Sign.S).setOpcode(VIAluOpcodes.vnclip_e32)

  // 13.2. Vector Single-Width Floating-Point Add/Subtract Instructions
  def vfadd_fp16 = VecFpUopVV_DV().setOpcode(FMacOpcodes.vfadd_fp16)
  def vfadd_fp32 = VecFpUopVV_DV().setOpcode(FMacOpcodes.vfadd_fp32)
  def vfadd_fp64 = VecFpUopVV_DV().setOpcode(FMacOpcodes.vfadd_fp64)
  def vfsub_fp16 = VecFpUopVV_DV().setOpcode(FMacOpcodes.vfsub_fp16)
  def vfsub_fp32 = VecFpUopVV_DV().setOpcode(FMacOpcodes.vfsub_fp32)
  def vfsub_fp64 = VecFpUopVV_DV().setOpcode(FMacOpcodes.vfsub_fp64)

  // 13.3. Vector Widening Floating-Point Add/Subtract Instructions
  def vfwadd_fp16   = VecFpUopVV_DW().setOpcode(FMacOpcodes.vfwadd_fp16)
  def vfwadd_fp32   = VecFpUopVV_DW().setOpcode(FMacOpcodes.vfwadd_fp32)
  def vfwsub_fp16   = VecFpUopVV_DW().setOpcode(FMacOpcodes.vfwsub_fp16)
  def vfwsub_fp32   = VecFpUopVV_DW().setOpcode(FMacOpcodes.vfwsub_fp32)
  def vfwadd_w_fp16 = VecFpUopWV_DW().setOpcode(FMacOpcodes.vfwadd_w_fp16)
  def vfwadd_w_fp32 = VecFpUopWV_DW().setOpcode(FMacOpcodes.vfwadd_w_fp32)
  def vfwsub_w_fp16 = VecFpUopWV_DW().setOpcode(FMacOpcodes.vfwsub_w_fp16)
  def vfwsub_w_fp32 = VecFpUopWV_DW().setOpcode(FMacOpcodes.vfwsub_w_fp32)

  // 13.4. Vector Single-Width Floating-Point Multiply/Divide Instructions
  def vfmul_fp16 = VecFpUopVV_DV().setOpcode(FMacOpcodes.vfmul_fp16)
  def vfmul_fp32 = VecFpUopVV_DV().setOpcode(FMacOpcodes.vfmul_fp32)
  def vfmul_fp64 = VecFpUopVV_DV().setOpcode(FMacOpcodes.vfmul_fp64)
  def vfdiv_fp16 = VecFpUopVV_DV().setOpcode(FDivOpcodes.vfdiv_fp16)
  def vfdiv_fp32 = VecFpUopVV_DV().setOpcode(FDivOpcodes.vfdiv_fp32)
  def vfdiv_fp64 = VecFpUopVV_DV().setOpcode(FDivOpcodes.vfdiv_fp64)

  // 13.5. Vector Widening Floating-Point Multiply
  def vfwmul_fp16 = VecFpUopVV_DW().setOpcode(FMacOpcodes.vfwmul_fp16)
  def vfwmul_fp32 = VecFpUopVV_DW().setOpcode(FMacOpcodes.vfwmul_fp32)

  // 13.6. Vector Single-Width Floating-Point Fused Multiply-Add Instructions
  def vfmacc_fp16  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmacc_fp16)
  def vfmacc_fp32  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmacc_fp32)
  def vfmacc_fp64  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmacc_fp64)
  def vfnmacc_fp16 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmacc_fp16)
  def vfnmacc_fp32 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmacc_fp32)
  def vfnmacc_fp64 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmacc_fp64)
  def vfmsac_fp16  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmsac_fp16)
  def vfmsac_fp32  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmsac_fp32)
  def vfmsac_fp64  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmsac_fp64)
  def vfnmsac_fp16 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmsac_fp16)
  def vfnmsac_fp32 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmsac_fp32)
  def vfnmsac_fp64 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmsac_fp64)
  def vfmadd_fp16  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmadd_fp16)
  def vfmadd_fp32  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmadd_fp32)
  def vfmadd_fp64  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmadd_fp64)
  def vfnmadd_fp16 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmadd_fp16)
  def vfnmadd_fp32 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmadd_fp32)
  def vfnmadd_fp64 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmadd_fp64)
  def vfmsub_fp16  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmsub_fp16)
  def vfmsub_fp32  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmsub_fp32)
  def vfmsub_fp64  = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfmsub_fp64)
  def vfnmsub_fp16 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmsub_fp16)
  def vfnmsub_fp32 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmsub_fp32)
  def vfnmsub_fp64 = VecFpUopVVV_DV().setOpcode(FMacOpcodes.vfnmsub_fp64)

  // 13.7. Vector Widening Floating-Point Fused Multiply-Add Instructions
  def vfwmacc_fp16  = VecFpUopVVW_DW().setOpcode(FMacOpcodes.vfwmacc_fp16)
  def vfwmacc_fp32  = VecFpUopVVW_DW().setOpcode(FMacOpcodes.vfwmacc_fp32)
  def vfwnmacc_fp16 = VecFpUopVVW_DW().setOpcode(FMacOpcodes.vfwnmacc_fp16)
  def vfwnmacc_fp32 = VecFpUopVVW_DW().setOpcode(FMacOpcodes.vfwnmacc_fp32)
  def vfwmsac_fp16  = VecFpUopVVW_DW().setOpcode(FMacOpcodes.vfwmsac_fp16)
  def vfwmsac_fp32  = VecFpUopVVW_DW().setOpcode(FMacOpcodes.vfwmsac_fp32)
  def vfwnmsac_fp16 = VecFpUopVVW_DW().setOpcode(FMacOpcodes.vfwnmsac_fp16)
  def vfwnmsac_fp32 = VecFpUopVVW_DW().setOpcode(FMacOpcodes.vfwnmsac_fp32)

  // 13.8. Vector Floating-Point Square-Root Instruction
  // 13.9. Vector Floating-Point Reciprocal Square-Root Estimate Instruction
  // 13.10. Vector Floating-Point Reciprocal Estimate Instruction
  def vfsqrt_fp16   = VecFpUopS2V_DV().setOpcode(FDivOpcodes.vfsqrt_fp16)
  def vfsqrt_fp32   = VecFpUopS2V_DV().setOpcode(FDivOpcodes.vfsqrt_fp32)
  def vfsqrt_fp64   = VecFpUopS2V_DV().setOpcode(FDivOpcodes.vfsqrt_fp64)
  def vfrsqrt7_fp16 = VecFpUopS2V_DV()
  def vfrsqrt7_fp32 = VecFpUopS2V_DV()
  def vfrsqrt7_fp64 = VecFpUopS2V_DV()
  def vfrec7_fp16   = VecFpUopS2V_DV()
  def vfrec7_fp32   = VecFpUopS2V_DV()
  def vfrec7_fp64   = VecFpUopS2V_DV()

  // 13.11. Vector Floating-Point MIN/MAX Instructions
  def vfmin_fp16 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfmin_fp16)
  def vfmin_fp32 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfmin_fp32)
  def vfmin_fp64 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfmin_fp64)
  def vfmax_fp16 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfmax_fp16)
  def vfmax_fp32 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfmax_fp32)
  def vfmax_fp64 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfmax_fp64)

  // 13.12. Vector Floating-Point Sign-Injection Instructions
  def vfsgnj_fp16  = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfsgnj_fp16)
  def vfsgnj_fp32  = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfsgnj_fp32)
  def vfsgnj_fp64  = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfsgnj_fp64)
  def vfsgnjn_fp16 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfsgnjn_fp16)
  def vfsgnjn_fp32 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfsgnjn_fp32)
  def vfsgnjn_fp64 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfsgnjn_fp64)
  def vfsgnjx_fp16 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfsgnjx_fp16)
  def vfsgnjx_fp32 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfsgnjx_fp32)
  def vfsgnjx_fp64 = VecFpUopVV_DV().setOpcode(FMiscOpcodes.vfsgnjx_fp64)

  // 13.13. Vector Floating-Point Compare Instructions
  def vmfeq_fp16 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfeq_fp16)
  def vmfeq_fp32 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfeq_fp32)
  def vmfeq_fp64 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfeq_fp64)
  def vmfne_fp16 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfne_fp16)
  def vmfne_fp32 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfne_fp32)
  def vmfne_fp64 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfne_fp64)
  def vmfle_fp16 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfle_fp16)
  def vmfle_fp32 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfle_fp32)
  def vmfle_fp64 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfle_fp64)
  def vmflt_fp16 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmflt_fp16)
  def vmflt_fp32 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmflt_fp32)
  def vmflt_fp64 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmflt_fp64)
  def vmfge_fp16 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfge_fp16)
  def vmfge_fp32 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfge_fp32)
  def vmfge_fp64 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfge_fp64)
  def vmfgt_fp16 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfgt_fp16)
  def vmfgt_fp32 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfgt_fp32)
  def vmfgt_fp64 = VecFpUopVV_DM().setOpcode(FMiscOpcodes.vmfgt_fp64)

  // 13.14. Vector Floating-Point Classify Instruction
  def vfclass_fp16 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfclass_fp16)
  def vfclass_fp32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfclass_fp32)
  def vfclass_fp64 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfclass_fp64)

  // 13.15. Vector Floating-Point Merge Instruction
  def vfmerge_fp16 = VecFpUopVV_DV().setOpcode(VMoveOpcodes.vmerge_vx_e16)
  def vfmerge_fp32 = VecFpUopVV_DV().setOpcode(VMoveOpcodes.vmerge_vx_e32)
  def vfmerge_fp64 = VecFpUopVV_DV().setOpcode(VMoveOpcodes.vmerge_vx_e64)

  // 13.16. Vector Floating-Point Move Instruction
  def vmv_f2v_fp16 = VecFpUopS1(Src1Fp, VpWen).setOpcode(VMoveOpcodes.vmerge_vx_e16)
  def vmv_f2v_fp32 = VecFpUopS1(Src1Fp, VpWen).setOpcode(VMoveOpcodes.vmerge_vx_e32)
  def vmv_f2v_fp64 = VecFpUopS1(Src1Fp, VpWen).setOpcode(VMoveOpcodes.vmerge_vx_e64)

  // 13.17. Single-Width Floating-Point/Integer Type-Convert Instructions
  /// VFCVT_XU_F, VFCVT_XU_F_RTZ
  def vfcvt_ui16_fp16 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_ui16_fp16)
  def vfcvt_ui32_fp32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_ui32_fp32)
  def vfcvt_ui64_fp64 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_ui64_fp64)
  /// VFCVT_X_F, VFCVT_X_F_RTZ
  def vfcvt_si16_fp16 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_si16_fp16)
  def vfcvt_si32_fp32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_si32_fp32)
  def vfcvt_si64_fp64 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_si64_fp64)
  /// VFCVT_F_XU,
  def vfcvt_fp16_ui16 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp16_ui16)
  def vfcvt_fp32_ui32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp32_ui32)
  def vfcvt_fp64_ui64 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp64_ui64)
  /// VFCVT_F_X
  def vfcvt_fp16_si16 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp16_si16)
  def vfcvt_fp32_si32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp32_si32)
  def vfcvt_fp64_si64 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp64_si64)

  // 13.18. Widening Floating-Point/Integer Type-Convert Instructions
  // VFWCVT_XU_F, VFWCVT_XU_F_RTZ
  def vfcvt_ui32_fp16 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_ui32_fp16)
  def vfcvt_ui64_fp32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_ui64_fp32)
  /// VFWCVT_X_F, VFWCVT_X_F_RTZ
  def vfcvt_si32_fp16 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_si32_fp16)
  def vfcvt_si64_fp32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_si64_fp32)
  /// VFWCVT_F_X
  def vfcvt_fp32_si16 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp32_si16)
  def vfcvt_fp64_si32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp64_si32)
  /// VFWCVT_F_XU
  def vfcvt_fp32_ui16 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp32_ui16)
  def vfcvt_fp64_ui32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp64_ui32)
  /// VFWCVT_F_F
  def vfcvt_fp32_fp16 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp32_fp16)
  def vfcvt_fp64_fp32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp64_fp32)

  // 13.19. Narrowing Floating-Point/Integer Type-Convert Instructions
  /// VFNCVT_XU_F, VFNCVT_XU_F_RTZ
  def vfcvt_ui8_fp16  = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_ui8_fp16)
  def vfcvt_ui16_fp32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_ui16_fp32)
  def vfcvt_ui32_fp64 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_ui32_fp64)
  /// VFNCVT_X_F, VFNCVT_X_F_RTZ
  def vfcvt_si8_fp16  = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_si8_fp16)
  def vfcvt_si16_fp32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_si16_fp32)
  def vfcvt_si32_fp64 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_si32_fp64)
  /// VFNCVT_F_XU
  def vfcvt_fp16_ui32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp16_ui32)
  def vfcvt_fp32_ui64 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp32_ui64)
  /// VFNCVT_F_X
  def vfcvt_fp16_si32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp16_si32)
  def vfcvt_fp32_si64 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp32_si64)
  /// VFNCVT_F_F, VFNCVT_F_F_ROD
  def vfcvt_fp16_fp32 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp16_fp32)
  def vfcvt_fp32_fp64 = VecFpUopS2V_DV().setOpcode(FCvtOpcodes.vfcvt_fp32_fp64)

  // 14.1. Vector Single-Width Integer Reduction Instructions
  // reduction
  def vredand_e8   = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredand_e8)
  def vredand_e16  = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredand_e16)
  def vredand_e32  = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredand_e32)
  def vredand_e64  = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredand_e64)
  def vredor_e8    = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredor_e8)
  def vredor_e16   = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredor_e16)
  def vredor_e32   = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredor_e32)
  def vredor_e64   = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredor_e64)
  def vredxor_e8   = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredxor_e8)
  def vredxor_e16  = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredxor_e16)
  def vredxor_e32  = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredxor_e32)
  def vredxor_e64  = VecIntRedUopVA_DA().setOpcode(VIRedOpcodes.vredxor_e64)
  def vredmaxu_e8  = VecIntRedUopVA_DA().set(_.sgn, Sign.U).setOpcode(VIRedOpcodes.vredmaxu_e8)
  def vredmaxu_e16 = VecIntRedUopVA_DA().set(_.sgn, Sign.U).setOpcode(VIRedOpcodes.vredmaxu_e16)
  def vredmaxu_e32 = VecIntRedUopVA_DA().set(_.sgn, Sign.U).setOpcode(VIRedOpcodes.vredmaxu_e32)
  def vredmaxu_e64 = VecIntRedUopVA_DA().set(_.sgn, Sign.U).setOpcode(VIRedOpcodes.vredmaxu_e64)
  def vredminu_e8  = VecIntRedUopVA_DA().set(_.sgn, Sign.U).setOpcode(VIRedOpcodes.vredminu_e8)
  def vredminu_e16 = VecIntRedUopVA_DA().set(_.sgn, Sign.U).setOpcode(VIRedOpcodes.vredminu_e16)
  def vredminu_e32 = VecIntRedUopVA_DA().set(_.sgn, Sign.U).setOpcode(VIRedOpcodes.vredminu_e32)
  def vredminu_e64 = VecIntRedUopVA_DA().set(_.sgn, Sign.U).setOpcode(VIRedOpcodes.vredminu_e64)
  def vredmax_e8   = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredmax_e8)
  def vredmax_e16  = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredmax_e16)
  def vredmax_e32  = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredmax_e32)
  def vredmax_e64  = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredmax_e64)
  def vredmin_e8   = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredmin_e8)
  def vredmin_e16  = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredmin_e16)
  def vredmin_e32  = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredmin_e32)
  def vredmin_e64  = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredmin_e64)
  def vredsum_e8   = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredsum_e8)
  def vredsum_e16  = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredsum_e16)
  def vredsum_e32  = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredsum_e32)
  def vredsum_e64  = VecIntRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vredsum_e64)

  // 14.2. Vector Widening Integer Reduction Instructions
  def vwredsumu_e8  = VecIntWRedUopVA_DA().set(_.sgn, Sign.U).setOpcode(VIRedOpcodes.vwredsumu_e8)
  def vwredsumu_e16 = VecIntWRedUopVA_DA().set(_.sgn, Sign.U).setOpcode(VIRedOpcodes.vwredsumu_e16)
  def vwredsumu_e32 = VecIntWRedUopVA_DA().set(_.sgn, Sign.U).setOpcode(VIRedOpcodes.vwredsumu_e32)
  def vwredsum_e8   = VecIntWRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vwredsum_e8)
  def vwredsum_e16  = VecIntWRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vwredsum_e16)
  def vwredsum_e32  = VecIntWRedUopVA_DA().set(_.sgn, Sign.S).setOpcode(VIRedOpcodes.vwredsum_e32)
  def vwadd4u_e8  = VecIntUopVV_DW().setOpcode(VIAluOpcodes.vwadd4u_e8 )
  def vwadd4u_e16 = VecIntUopVV_DW().setOpcode(VIAluOpcodes.vwadd4u_e16)
  def vwadd4u_e32 = VecIntUopVV_DW().setOpcode(VIAluOpcodes.vwadd4u_e32)
  def vwadd4_e8  = VecIntUopVV_DW().setOpcode(VIAluOpcodes.vwadd4_e8 )
  def vwadd4_e16 = VecIntUopVV_DW().setOpcode(VIAluOpcodes.vwadd4_e16)
  def vwadd4_e32 = VecIntUopVV_DW().setOpcode(VIAluOpcodes.vwadd4_e32)

  // 14.3. Vector Single-Width Floating-Point Reduction Instructions
  def vfredosum_fp16 = VecFpRedUopVA_DA()
  def vfredosum_fp32 = VecFpRedUopVA_DA()
  def vfredosum_fp64 = VecFpRedUopVA_DA()
  def vfredusum_fp16 = VecFpRedUopVA_DA()
  def vfredusum_fp32 = VecFpRedUopVA_DA()
  def vfredusum_fp64 = VecFpRedUopVA_DA()
  def vfredmin_fp16 = VecFpRedUopVA_DA()
  def vfredmin_fp32 = VecFpRedUopVA_DA()
  def vfredmin_fp64 = VecFpRedUopVA_DA()
  def vfredmax_fp16 = VecFpRedUopVA_DA()
  def vfredmax_fp32 = VecFpRedUopVA_DA()
  def vfredmax_fp64 = VecFpRedUopVA_DA()

  // 14.4. Vector Widening Floating-Point Reduction Instructions
  def vfwredosum_fp16 = VecFpWRedUopVA_DA()
  def vfwredosum_fp32 = VecFpWRedUopVA_DA()
  def vfwredusum_fp16 = VecFpWRedUopVA_DA()
  def vfwredusum_fp32 = VecFpWRedUopVA_DA()

  // 15.1. Vector Mask-Register Logical Instructions
  def vmand  = VecIntUopMM_DM().setOpcode(VIAluOpcodes.vmand )
  def vmnand = VecIntUopMM_DM().setOpcode(VIAluOpcodes.vmnand)
  def vmandn = VecIntUopMM_DM().setOpcode(VIAluOpcodes.vmandn)
  def vmor   = VecIntUopMM_DM().setOpcode(VIAluOpcodes.vmor  )
  def vmnor  = VecIntUopMM_DM().setOpcode(VIAluOpcodes.vmnor )
  def vmorn  = VecIntUopMM_DM().setOpcode(VIAluOpcodes.vmorn )
  def vmxor  = VecIntUopMM_DM().setOpcode(VIAluOpcodes.vmxor )
  def vmxnor = VecIntUopMM_DM().setOpcode(VIAluOpcodes.vmxnor)

  // 15.2. Vector count population in mask vcpop.m
  // 15.3. vfirst find-first-set mask bit
  // 15.4. vmsbf.m set-before-first mask bit
  // 15.5. vmsif.m set-including-first mask bit
  // 15.6. vmsof.m set-only-first mask bit
  def vcpop_m = VecIntUopS2M_DX().setOpcode(VMAluOpcodes.vcpop_m)
  def vfirst  = VecIntUopS2M_DX().setOpcode(VMAluOpcodes.vfirst)
  def vmsbf   = VecIntUopS2M_DM().setOpcode(VMAluOpcodes.vmsbf)
  def vmsif   = VecIntUopS2M_DM().setOpcode(VMAluOpcodes.vmsif)
  def vmsof   = VecIntUopS2M_DM().setOpcode(VMAluOpcodes.vmsof)

  // 15.8. Vector Iota Instruction
  def viota_e8  = VecIntUopS2M_DV().setOpcode(VMAluOpcodes.viota_e8 )
  def viota_e16 = VecIntUopS2M_DV().setOpcode(VMAluOpcodes.viota_e16)
  def viota_e32 = VecIntUopS2M_DV().setOpcode(VMAluOpcodes.viota_e32)
  def viota_e64 = VecIntUopS2M_DV().setOpcode(VMAluOpcodes.viota_e64)

  // 15.9. Vector Element Index Instruction
  def vid_e8  = VecIntUop_DV().setOpcode(VMAluOpcodes.vid_e8 )
  def vid_e16 = VecIntUop_DV().setOpcode(VMAluOpcodes.vid_e16)
  def vid_e32 = VecIntUop_DV().setOpcode(VMAluOpcodes.vid_e32)
  def vid_e64 = VecIntUop_DV().setOpcode(VMAluOpcodes.vid_e64)

  // 16.1. Integer Scalar Move Instructions
  /**
   * vmv.x.s performs its operation even if vstart >= vl or vl=0
   */
  def vmv_vs2x = VecUopS2A(GpWen)

  def vmv_x2vs = VecUopS1_DA(Src1Gp)

  // 16.2. Floating-Point Scalar Move Instructions
  /**
   * vfmv.f.s performs its operation even if vstart >= vl or vl=0
   */
  def vmvVecScala2Fp = VecUopS2A(FpWen)

  def vmvFp2VecScala = VecUopS1_DA(Src1Fp)

  // 16.3. Vector Slide Instructions
  def vslideup_x_e8    = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslideup_e8 )
  def vslideup_x_e16   = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslideup_e16)
  def vslideup_x_e32   = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslideup_e32)
  def vslideup_x_e64   = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslideup_e64)
  def vslideup_i_e8    = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslideup_e8 )
  def vslideup_i_e16   = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslideup_e16)
  def vslideup_i_e32   = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslideup_e32)
  def vslideup_i_e64   = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslideup_e64)
  def vslidedown_x_e8  = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslidedown_e8 )
  def vslidedown_x_e16 = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslidedown_e16)
  def vslidedown_x_e32 = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslidedown_e32)
  def vslidedown_x_e64 = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslidedown_e64)
  def vslidedown_i_e8  = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslidedown_e8 )
  def vslidedown_i_e16 = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslidedown_e16)
  def vslidedown_i_e32 = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslidedown_e32)
  def vslidedown_i_e64 = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask){}.setOpcode(VIPermOpcodes.vslidedown_e64)

  def vslide1up_x_e8    = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1up_e8 )
  def vslide1up_x_e16   = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1up_e16)
  def vslide1up_x_e32   = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1up_e32)
  def vslide1up_x_e64   = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1up_e64)
  def vslide1up_f_fp16  = new VecIntUop(Src2Vp, Src1Fp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1up_e16)
  def vslide1up_f_fp32  = new VecIntUop(Src2Vp, Src1Fp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1up_e32)
  def vslide1up_f_fp64  = new VecIntUop(Src2Vp, Src1Fp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1up_e64)
  def vslide1down_x_e8  = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1down_e8 )
  def vslide1down_x_e16 = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1down_e16)
  def vslide1down_x_e32 = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1down_e32)
  def vslide1down_x_e64 = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1down_e64)
  def vslide1down_f_fp16 = new VecIntUop(Src2Vp, Src1Fp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1down_e16)
  def vslide1down_f_fp32 = new VecIntUop(Src2Vp, Src1Fp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1down_e32)
  def vslide1down_f_fp64 = new VecIntUop(Src2Vp, Src1Fp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vslide1down_e64)

  // 16.4. Vector Register Gather Instructions
  def vrgather_v_e8  = new VecIntUop(Src2Vp, Src1Vp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_v_e8 )
  def vrgather_v_e16 = new VecIntUop(Src2Vp, Src1Vp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_v_e16)
  def vrgather_v_e32 = new VecIntUop(Src2Vp, Src1Vp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_v_e32)
  def vrgather_v_e64 = new VecIntUop(Src2Vp, Src1Vp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_v_e64)
  def vrgatherei16_v_e8  = new VecIntUop(Src2Vp, Src1Vp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_ei16_e8 )
  def vrgatherei16_v_e16 = new VecIntUop(Src2Vp, Src1Vp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_ei16_e16)
  def vrgatherei16_v_e32 = new VecIntUop(Src2Vp, Src1Vp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_ei16_e32)
  def vrgatherei16_v_e64 = new VecIntUop(Src2Vp, Src1Vp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_ei16_e64)

  def vrgather_x_e8  = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_x_e8 )
  def vrgather_x_e16 = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_x_e16)
  def vrgather_x_e32 = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_x_e32)
  def vrgather_x_e64 = new VecIntUop(Src2Vp, Src1Gp, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_x_e64)
  def vrgather_i_e8  = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_i_e8 )
  def vrgather_i_e16 = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_i_e16)
  def vrgather_i_e32 = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_i_e32)
  def vrgather_i_e64 = new VecIntUop(Src2Vp, Src1Imm, VpWen, VlRen, V0RenAsMask) {}.setOpcode(VIPermOpcodes.vrgather_i_e64)

  // 16.5. Vector Compress Instruction
  def vcompress_e8  = VecIntUopVM_DV().setOpcode(VIPermOpcodes.vcompress_e8 )
  def vcompress_e16 = VecIntUopVM_DV().setOpcode(VIPermOpcodes.vcompress_e16)
  def vcompress_e32 = VecIntUopVM_DV().setOpcode(VIPermOpcodes.vcompress_e32)
  def vcompress_e64 = VecIntUopVM_DV().setOpcode(VIPermOpcodes.vcompress_e64)

  // 16.6. Whole Vector Register Move
  def vmvnr = VecIntUopS2V_DV()

  //
  def vle8  = new VecLoadUop(Src1Gp, VpWen, VlRen).setOpcode(LduOpcodes.vle8)
  def vle16 = new VecLoadUop(Src1Gp, VpWen, VlRen).setOpcode(LduOpcodes.vle16)
  def vle32 = new VecLoadUop(Src1Gp, VpWen, VlRen).setOpcode(LduOpcodes.vle32)
  def vle64 = new VecLoadUop(Src1Gp, VpWen, VlRen).setOpcode(LduOpcodes.vle64)
  def vleff = new VecLoadUop(Src1Gp, VpWen, VlRen)
  def vlm   = new VecLoadUop(Src1Gp, VmWen, VlRen)
  def vlnr  = new VecLoadUop(Src1Gp, VpWen)
  def vlse  = new VecLoadUop(Src1Gp, VpWen, VlRen, Src2Gp)
  def vluxe = new VecLoadUop(Src1Gp, VpWen, VlRen, Src2Vp)
  def vloxe = new VecLoadUop(Src1Gp, VpWen, VlRen, Src2Vp, Order)

  def vse8  = new VecStoreUop(Src1Gp, Src3Vp, VlRen).setOpcode(StuOpcodes.vse8 )
  def vse16 = new VecStoreUop(Src1Gp, Src3Vp, VlRen).setOpcode(StuOpcodes.vse16)
  def vse32 = new VecStoreUop(Src1Gp, Src3Vp, VlRen).setOpcode(StuOpcodes.vse32)
  def vse64 = new VecStoreUop(Src1Gp, Src3Vp, VlRen).setOpcode(StuOpcodes.vse64)
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
  def vrev8_e8  = VecIntUopS2V_DV().setOpcode(VIAluOpcodes.vrev8_e8 )
  def vrev8_e16 = VecIntUopS2V_DV().setOpcode(VIAluOpcodes.vrev8_e16)
  def vrev8_e32 = VecIntUopS2V_DV().setOpcode(VIAluOpcodes.vrev8_e32)
  def vrev8_e64 = VecIntUopS2V_DV().setOpcode(VIAluOpcodes.vrev8_e64)

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
