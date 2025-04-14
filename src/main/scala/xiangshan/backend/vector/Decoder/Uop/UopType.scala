package xiangshan.backend.vector.Decoder.Uop

import chisel3.UInt
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.Types._
import xiangshan.backend.vector.util.ScalaTypeExt._

package object UopType {
  private val T = true
  private val F = false

  import Sign._

  /**
   * This uop is used when rs1 of vsetvl is not x0.
   */
  case class UopVecSet_VTYPEX_VLX()
    extends VecConfigUopBase
      with UopSrc12Ren
      with UopGpWen
      with UopVlWen

  /**
   * This uop is used when rs1 of vsetvl is x0 and rd is not x0. <br/>
   * This uop set vl as vlmax
   */
  case class UopVecSet_VTYPEX_VLMAX()
    extends VecConfigUopBase
      with UopSrc2Ren
      with UopGpWen
      with UopVlWen

  /**
   * This uop is used when both vd and vs1 of vsetvli are x0. <br/>
   * If vtype from rs2 is illegal, uop write vl as 0. <br/>
   * Otherwise, uop read vl and write the same vl. <br/>
   * Since vtype is not known at decode stage, we need this uop to achieve 2 targets that setting vl as 0 when vtype is
   * illegal and don't change vl when vtype is legal.
   */
  case class UopVecSet_VTYPEX_VLL()
    extends VecConfigUopBase
      with UopSrc2Ren
      with UopVlRen
      with UopGpWen
      with UopVlWen

  /**
   * This uop is used when rs1 of vsetvli is not x0.
   */
  case class UopVecSet_VTYPEI_VLX()
    extends VecConfigUopBase
      with UopSrc1Ren
      with UopGpWen
      with UopVlWen

  /**
   * This uop is used when rs1 of vsetvli is x0.
   * This uop set vl as vlmax
   */
  case class UopVecSet_VTYPEI_VLMAX()
    extends VecConfigUopBase
      with UopGpWen
      with UopVlWen

  /**
   * This uop is used when both vd and vs1 of vsetvli are x0.
   * This uop is almost nop and does not change vl but only write vtype.
   */
  case class UopVecSet_VTYPEI_NOP()
    extends VecConfigUopBase

  /**
   * This uop is used when vsetvli and vsetivli contain illegal vtype.
   * Vl should be set to 0
   */
  case class UopVecSet_VTYPEI_ILL()
    extends VecConfigUopBase
      with UopVlWen
      with UopGpWen

  /**
   * This uop is used for legal vsetivli
   */
  case class UopVecSet_VTYPEI_VLI()
    extends VecConfigUopBase
      with UopVlWen
      with UopGpWen

  abstract class UopInt_S2V_S1VXI_DV(
    src1Sgn: SignType,
    src2Sgn: SignType,
  ) extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen {
    src1Sign = src1Sgn
    src2Sign = src2Sgn
    // if equal then U|S else S
    destSign = (if (src1Sgn == src2Sgn) src1Sgn else Sign.S)
  }

  abstract class UopIntCmp_S2V_S1VXI_DM(
    sign: SignType,
  ) extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen
      with UopVdEew1b {

    src1Sign = sign
    src2Sign = sign
    destSign = sign
  }

  case class UopIntCarry_S2V_S1VXI_V0C_DV()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen
      with VecUopReadV0AsSrc

  abstract class UopIntCarry_S2V_S1VXI_DM(needCarryIn: Boolean)
    extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen
      with UopVdEew1b
      with VecUopReadV0AsSrc {
    v0RenAsSrc = needCarryIn
  }

  abstract class UopInt_S2V_S1VXI_DW(
    src1Sgn: SignType,
    src2Sgn: SignType,
  ) extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen
      with VecUopVVW {

    src1Sign = src1Sgn
    src2Sign = src2Sgn
    // if equal then U|S else S
    destSign = (if (src1Sign == src2Sign) src1Sign else S)
  }

  abstract class UopInt_S2W_S1VXI_DW(
    sign: SignType,
  ) extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen
      with VecUopWVW {

    src1Sign = sign
    src2Sign = sign
    destSign = sign
  }


  abstract class UopInt_S2W_S1UVXI_DV(
    sign: SignType,
  ) extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen
      with VecUopWVV {

    src1Sign = Sign.U
    src2Sign = sign
    destSign = sign
  }

  case class UopInt_S2UV_S1UVXI_DUV() extends UopInt_S2V_S1VXI_DV(U, U)
  case class UopInt_S2SV_S1SVXI_DSV() extends UopInt_S2V_S1VXI_DV(S, S)

  case class UopInt_S2UV_S1SV_DSV() extends UopInt_S2V_S1VXI_DV(S, U)

  case class UopIntCmp_S2UV_S1UVXI_DM() extends UopIntCmp_S2V_S1VXI_DM(U)
  case class UopIntCmp_S2SV_S1SVXI_DM() extends UopIntCmp_S2V_S1VXI_DM(S)

  case class UopInt_S2UV_DV()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopVpWen {

    src2Sign = U
  }

  case class UopInt_S2SV_DV()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopVpWen {

    src2Sign = S
  }

  case class UopInt_S1X_DA()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc1Ren
      with UopVpWen
      with UopDestVecScala

  case class UopInt_S2A_DX()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopSrc2VecScala
      with UopGpWen

  case class UopInt_S1UVXI_DV()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc1Ren
      with UopVpWen

  case class UopInt_DV()
    extends VecArithUopBase
      with UopVlRen
      with UopVpWen

  abstract class UopIntRed_S1A_S2V_DA(sign: SignType, widen: Boolean)
    extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen
      with UopSrc1VecScala
      with UopDestVecScala {

    src1Sign = sign
    src2Sign = sign
    destSign = sign
    src1Widen = widen
    destWiden = widen

    def useDoubleSew: this.type = {
      this.destWiden = T
      this.src1Widen = T
      this.src2Widen = T
      this
    }
  }

  case class UopIntRed_S1SA_S2SV_DSA()  extends UopIntRed_S1A_S2V_DA(sign = S, widen = F)
  case class UopIntRed_S1UA_S2UV_DUA()  extends UopIntRed_S1A_S2V_DA(sign = S, widen = F)
  case class UopIntWRed_S1SA_S2SV_DSA() extends UopIntRed_S1A_S2V_DA(sign = S, widen = T)
  case class UopIntWRed_S1UA_S2UV_DUA() extends UopIntRed_S1A_S2V_DA(sign = S, widen = T)

  case class UopIntCarry_S2V_S1VXI_V0C_DM() extends UopIntCarry_S2V_S1VXI_DM(needCarryIn = T)
  case class UopIntCarry_S2V_S1VXI_V0N_DM() extends UopIntCarry_S2V_S1VXI_DM(needCarryIn = F)

  case class UopInt_S2UV_S1UV_DUW() extends UopInt_S2V_S1VXI_DW(U, U)
  case class UopInt_S2SV_S1UV_DSW() extends UopInt_S2V_S1VXI_DW(U, S) // vwmaccsu.vv
  case class UopInt_S2UV_S1SV_DSW() extends UopInt_S2V_S1VXI_DW(S, U)
  case class UopInt_S2SV_S1SV_DSW() extends UopInt_S2V_S1VXI_DW(S, S)
  case class UopInt_S2UW_S1UV_DUW() extends UopInt_S2W_S1VXI_DW(U)
  case class UopInt_S2SW_S1SV_DSW() extends UopInt_S2W_S1VXI_DW(S)

  case class UopInt_S2UW_S1UVXI_DUV() extends UopInt_S2W_S1UVXI_DV(U)
  case class UopInt_S2SW_S1UVXI_DSV() extends UopInt_S2W_S1UVXI_DV(S)

  case class UopInt_S2M_DX()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopGpWen

  case class UopInt_S2M_DV()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopVpWen

  case class UopInt_S2M_DM()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopVdEew1b
      with UopVpWen

  case class UopInt_S2M_S1M_DM()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVdEew1b
      with UopVpWen

  case class UopShuffle()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen

  case class UopFp_S2A_DF()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopSrc2VecScala
      with UopFpWen

  case class UopFp_S1F_DV()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc1Ren
      with UopVpWen

  case class UopFp_S1F_DA()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc1Ren
      with UopVpWen
      with UopDestVecScala

  case class UopFp_S2V_DV()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopVpWen

  case class UopFp_S2V_DW()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopVpWen
      with VecUopDestWiden

  case class UopFp_S2W_DV()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopVpWen
      with VecUopSrc2Widen

  case class UopFp_S2V_S1VF_DV()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen

  case class UopFp_S2V_S1V_DW()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen
      with VecUopVVW

  case class UopFp_S2W_S1V_DW()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen
      with VecUopWVW

  case class UopFp_S2V_S1V_DM()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopVpWen
      with UopVdEew1b

  case class UopFpRed_S2V_S1A_DA()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopSrc1VecScala
      with UopVpWen
      with UopDestVecScala

  case class UopFpWRed_S2V_S1A_DA()
    extends VecArithUopBase
      with UopVlRen
      with UopSrc12Ren
      with UopSrc1VecScala
      with UopVpWen
      with UopDestVecScala
      with VecUopSrc1Widen
      with VecUopDestWiden

  case class UopUnitStrideLoad()
    extends VecLoadUopBase
      with UopVlRen
      with UopV0RenAsMask

  case class UopMaskLoad()
    extends VecLoadUopBase
      with UopVlRen

  case class UopStridedLoad()
    extends VecLoadUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopV0RenAsMask

  case class UopIndexLoad()
    extends VecLoadUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopV0RenAsMask

  case class UopWholeRegisterLoad()
    extends VecLoadUopBase

  case class UopUnitStrideStore()
    extends VecStoreUopBase
      with UopVlRen
      with UopV0RenAsMask

  case class UopMaskStore()
    extends VecStoreUopBase
      with UopVlRen

  case class UopStridedStore()
    extends VecStoreUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopV0RenAsMask

  case class UopIndexStore()
    extends VecStoreUopBase
      with UopVlRen
      with UopSrc2Ren
      with UopV0RenAsMask

  case class UopWholeRegisterStore()
    extends VecStoreUopBase

  trait UopV0RenAsMask { this: UopBase =>
    v0RenAsMask = T
  }

  trait UopVlRen { this: UopBase =>
    vlRen = T
  }

  trait UopSrc1Ren { this: UopBase =>
    src1Type = Some(null)
  }

  trait UopSrc2Ren { this: UopBase =>
    src2Type = Some(null)
  }

  trait UopSrc12Ren extends UopSrc1Ren with UopSrc2Ren { this: UopBase => }

  trait UopSrc1VecScala { this: VecArithUopBase =>
    src1VecScala = T
  }

  trait UopSrc2VecScala { this: VecArithUopBase =>
    src1VecScala = T
  }

  trait UopDestVecScala { this: VecArithUopBase =>
    destVecScala = T
  }

  trait VecUopDestWiden { this: VecArithUopBase =>
    destWiden = T
  }

  trait VecUopSrc1Widen { this: VecArithUopBase =>
    src1Widen = T
  }

  trait VecUopSrc2Widen { this: VecArithUopBase =>
    src2Widen = T
  }

  trait VecUopVVW extends VecUopDestWiden { this: VecArithUopBase => }

  trait VecUopWVW extends VecUopDestWiden with VecUopSrc2Widen { this: VecArithUopBase => }

  trait VecUopWVV extends VecUopSrc2Widen { this: VecArithUopBase => }

  trait UopGpWen { this: UopBase =>
    gpWen = T
  }

  trait UopFpWen { this: UopBase =>
    fpWen = T
  }

  trait UopVpWen { this: UopBase =>
    vpWen = T
  }

  trait UopVlWen { this: UopBase =>
    vlWen = T
  }

  trait UopVdEew1b { this: VecArithUopBase =>
    vdEew1b = T
  }

  trait VecUopReadV0AsSrc { this: VecArithUopBase =>
    v0RenAsSrc = T
    v0RenAsMask = F
  }

  abstract class VecConfigUopBase extends UopBase

  abstract class VecArithUopBase extends UopBase {
    // subclass may modify this
    v0RenAsMask = T
  }

  abstract class VecMemUopBase extends UopBase

  abstract class VecLoadUopBase
    extends VecMemUopBase
      with UopSrc1Ren
      with UopVpWen

  abstract class VecStoreUopBase
    extends VecMemUopBase
      with UopSrc1Ren

  abstract class UopBase {
    // used from rename to writeback
    var src1Type: Option[OperandType] = None
    var src2Type: Option[OperandType] = None
    var vlRen      : Boolean = F
    var v0RenAsMask: Boolean = F
    var v0RenAsSrc : Boolean = F // for vadc.vvm

    var gpWen        : Boolean = F
    var fpWen        : Boolean = F
    var vpWen        : Boolean = F
    var vlWen        : Boolean = F
    var src1Sign     : SignType = U
    var src2Sign     : SignType = U
    var destSign     : SignType = U

    // used from datapath to execution
    var src1Widen   : Boolean = F
    var src2Widen   : Boolean = F
    var destWiden   : Boolean = F

    // if use vs1 as scala operand, like vredand.vs
    var src1VecScala: Boolean = F
    var src2VecScala: Boolean = F
    var destVecScala: Boolean = F

    // may be used in future
    var vdEew1b     : Boolean = F


    var vxsatWen : Boolean = F
    var intRound : Boolean = F
    var readVdAsSrc : Boolean = F

    // use mask for 2 src
    var maskType: MaskType = DestMask

    var src12Rev: Boolean = F

    def src3Type: Option[OperandType] = Option.when(this.readVdAsSrc)(Operand.VP)

    def setOverflow: this.type = {
      this.vxsatWen = T
      this
    }

    def needIntRound: this.type = {
      this.intRound = T
      this
    }

    def needAlwaysReadVd: this.type = {
      this.readVdAsSrc = T
      this
    }

    def src2Mask: this.type = {
      this.maskType = Src2Mask
      this
    }

    def src12Mask: this.type = {
      this.maskType = Src12Mask
      this
    }

    def noMask: this.type = {
      this.v0RenAsMask = false
      this
    }

    def isAlwaysTA: Boolean = {
      this.vdEew1b
    }

    def src1SelRS: this.type = {
      require(this.src1Type.nonEmpty)
      this.src1Type = Some(Operand.GP)
      this
    }

    def src1SelFS: this.type = {
      require(this.src1Type.nonEmpty)
      this.src1Type = Some(Operand.FP)
      this
    }

    def src1SelVS: this.type = {
      require(this.src1Type.nonEmpty)
      this.src1Type = Some(Operand.VP)
      this
    }

    def src1SelIMM: this.type = {
      require(this.src1Type.nonEmpty)
      this.src1Type = Some(Operand.IMM)
      this
    }

    def src2SelRS: this.type = {
      require(this.src2Type.nonEmpty)
      this.src2Type = Some(Operand.GP)
      this
    }

    def src2SelFS: this.type = {
      require(this.src2Type.nonEmpty)
      this.src2Type = Some(Operand.FP)
      this
    }

    def src2SelVS: this.type = {
      require(this.src2Type.nonEmpty)
      this.src2Type = Some(Operand.VP)
      this
    }

    def src2SelIMM: this.type = {
      require(this.src2Type.nonEmpty)
      this.src2Type = Some(Operand.IMM)
      this
    }

    def v: this.type = {
      requireOneSrc
      if (this.src1Type.nonEmpty)
        this.src1SelVS
      else
        this.src2SelVS
    }

    def x: this.type = {
      requireOneSrc
      if (this.src1Type.nonEmpty)
        this.src1SelRS
      else
        this.src2SelRS
    }

    def i: this.type = {
      require(this.src1Type.nonEmpty)
      this.src1SelIMM
    }

    def m: this.type = {
      requireOneSrc
      if (this.src1Type.nonEmpty)
        this.src1SelVS
      else
        this.src2SelVS
    }

    def f: this.type = {
      if (this.src1Type.nonEmpty)
        this.src1SelFS
      else
        this.src2SelFS
    }

    def xx: this.type = {
      this.src1SelRS.src2SelRS
    }

    def vv: this.type = {
      this.src2SelVS.src1SelVS
    }

    def vx: this.type = {
      this.src2SelVS.src1SelRS
    }

    def vi: this.type = {
      this.src2SelVS.src1SelIMM
    }

    def vf: this.type = {
      this.src2SelVS.src1SelFS
    }

    def mm: this.type = {
      this.src2SelVS.src1SelVS
    }

    def requireOneSrc: Unit = require(this.src1Type.nonEmpty ^ this.src2Type.nonEmpty)

    def setSrc12Rev: this.type = {
      this.src12Rev = true
      this
    }

    def apply(dest: UInt, src1: UInt, src2: UInt): this.type = {
      //        this.dest = dest
      //        this.src1 = src1
      //        this.src2 = src2
      this.clone().asInstanceOf[this.type]
    }

    def genUopInfoRenameBitPat: BitPat = {
      UopInfoRename.genBitPat(
        src1Type = src1Type,
        src2Type = src2Type,
        vlRen = this.vlRen,
        v0Ren = this.v0RenAsMask || this.v0RenAsSrc,
        maskType = this.maskType,
        intRmRen = this.intRound,
        readVdAsSrc = this.readVdAsSrc,
        gpWen = this.gpWen,
        fpWen = this.fpWen,
        vpWen = this.vpWen,
        vlWen = this.vlWen,
        vxsatWen = this.vxsatWen,
      )
    }

    def genSrc12RevBitPat: BitPat = this.src12Rev.toBitPat

    def genVxsatWenBitPat: BitPat = this.vxsatWen.toBitPat

    def genVdEew1bBitPat: BitPat = this.vdEew1b.toBitPat

    def genAlwaysReadVdBitPat: BitPat = this.readVdAsSrc.toBitPat

    def uopInfoRenameString: String = {
      s"src1:${src1Type}," +
        s"src2:${src2Type}," +
        s"src3:${src3Type}," +
        s"vlRen:$vlRen," +
        s"v0Ren:${this.v0RenAsMask || this.v0RenAsSrc}," +
        s"gpWen:${this.gpWen}," +
        s"fpWen:${this.fpWen}," +
        s"vpWen:${this.vpWen}," +
        s"vlWen:${this.vlWen}"
    }
  }
}
