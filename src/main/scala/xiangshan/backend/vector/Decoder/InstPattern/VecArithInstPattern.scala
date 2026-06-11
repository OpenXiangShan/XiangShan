package xiangshan.backend.vector.Decoder.InstPattern
import chisel3.util.BitPat

object VecArithInstPattern extends VecInstFieldDefination {
  def apply()(implicit rawInst: BitPat): VecArithInstPattern = {
    val OPIVV="000"; val OPFVV="001"; val OPMVV="010"
    val OPIVI="011"; val OPIVX="100"; val OPFVF="101"; val OPMVX="110"
    val f6=func6.rawString; val f3=category.rawString
    val op = opcode.rawString
    if (op == "1010111") {
      f3 match {
      case OPIVV => f6 match {
        case s"000000" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VADD.VV
        case s"000001" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VANDN.VV
        case s"000010" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSUB.VV
        case s"000100" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VMINU.VV
        case s"000101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VMIN.VV
        case s"000110" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VMAXU.VV
        case s"000111" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VMAX.VV
        case s"001001" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VAND.VV
        case s"001010" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VOR.VV
        case s"001011" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VXOR.VV
        case s"001100" => new VecGatherVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All with OverlapStrict // VRGATHER.VV
        case s"001110" => new VecGatherEI16Pattern()
                          with V2_VecNormal with V1_SpcGather16 with V3_VecNormal_DestOnly with SL_Gather16 with OverlapStrict // VRGATHEREI16.VV
        case s"010000" =>
          (vm.rawString, vs1.rawString) match {
            case ("0", _) => new VecCarryPattern()
                             with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VADC.VVM
            case _ => null
          }
        case s"010001" =>
          (vm.rawString, vs1.rawString) match {
            case ("0", _) => new VecCarryMPattern()
                             with V2_VecNormal with V1_VecNormal with V3_VecMask_DestOnly with SL_All // VMADC.VVM
            case ("1", _) => new VecCarryMPattern()
                             with V2_VecNormal with V1_VecNormal with V3_VecMask_DestOnly with SL_All // VMADC.VV
            case _ => null
          }
        case s"010010" =>
          (vm.rawString, vs1.rawString) match {
            case ("0", _) => new VecCarryPattern()
                             with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSBC.VVM
            case _ => null
          }
        case s"010011" =>
          (vm.rawString, vs1.rawString) match {
            case ("0", _) => new VecCarryMPattern()
                             with V2_VecNormal with V1_VecNormal with V3_VecMask_DestOnly with SL_All // VMSBC.VVM
            case ("1", _) => new VecCarryMPattern()
                             with V2_VecNormal with V1_VecNormal with V3_VecMask_DestOnly with SL_All // VMSBC.VV
            case _ => null
          }
        case s"010100" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VROR.VV
        case s"010101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VROL.VV
        case s"010111" =>
          (vm.rawString, vs1.rawString, vs2.rawString) match {
            case ("0", _, _) => new VecIntVVVPattern()
                             with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VMERGE.VVM
            case ("1", _, "00000") => new VecIntS1VDVPattern()
                             with V2_NotUsed with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VMV.V.V
            case _ => null
          }
        case s"011000" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecMask_DestOnly with SL_All // VMSEQ.VV
        case s"011001" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecMask_DestOnly with SL_All // VMSNE.VV
        case s"011010" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecMask_DestOnly with SL_All // VMSLTU.VV
        case s"011011" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecMask_DestOnly with SL_All // VMSLT.VV
        case s"011100" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecMask_DestOnly with SL_All // VMSLEU.VV
        case s"011101" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecMask_DestOnly with SL_All // VMSLE.VV
        case s"100000" => new VecIntSatVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSADDU.VV
        case s"100001" => new VecIntSatVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSADD.VV
        case s"100010" => new VecIntSatVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSSUBU.VV
        case s"100011" => new VecIntSatVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSSUB.VV
        case s"100101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSLL.VV
        case s"100111" => new VecIntSatMulVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSMUL.VV
        case s"101000" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSRL.VV
        case s"101001" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSRA.VV
        case s"101010" => new VecIntScaleShiftVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSSRL.VV
        case s"101011" => new VecIntScaleShiftVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VSSRA.VV
        case s"101100" => new VecIntNarrowShiftWVVPattern()
                          with V2_VecWiden  with V1_VecNormal with V3_VecNormal_DestOnly with SL_Widen // VNSRL.WV
        case s"101101" => new VecIntNarrowShiftWVVPattern()
                          with V2_VecWiden  with V1_VecNormal with V3_VecNormal_DestOnly with SL_Widen // VNSRA.WV
        case s"101110" => new VecIntClipWVVPattern()
                          with V2_VecWiden  with V1_VecNormal with V3_VecNormal_DestOnly with SL_Widen // VNCLIPU.WV
        case s"101111" => new VecIntClipWVVPattern()
                          with V2_VecWiden  with V1_VecNormal with V3_VecNormal_DestOnly with SL_Widen // VNCLIP.WV
        case s"110000" => new VecIntWRedPattern()
                          with V2_VecNormal with V1_ScaWiden  with V3_ScaWiden_DestOnly with SL_Widen // VWREDSUMU.VS
        case s"110001" => new VecIntWRedPattern()
                          with V2_VecNormal with V1_ScaWiden  with V3_ScaWiden_DestOnly with SL_Widen // VWREDSUM.VS
        case s"110101" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWSLL.VV
        case _ => null
      }
      case OPFVV => f6 match {
        case s"000000" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_DestOnly with SL_Float // VFADD.VV
        case s"000001" => new VecFpRedPattern()
                          with V2_VecFloat  with V1_ScaFloat  with V3_ScaFloat_DestOnly with SL_Float // VFREDUSUM.VS
        case s"000010" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_DestOnly with SL_Float // VFSUB.VV
        case s"000011" => new VecFpRedPattern()
                          with V2_VecFloat  with V1_ScaFloat  with V3_ScaFloat_DestOnly with SL_Float // VFREDOSUM.VS
        case s"000100" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_DestOnly with SL_Float // VFMIN.VV
        case s"000101" => new VecFpRedPattern()
                          with V2_VecFloat  with V1_ScaFloat  with V3_ScaFloat_DestOnly with SL_Float // VFREDMIN.VS
        case s"000110" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_DestOnly with SL_Float // VFMAX.VV
        case s"000111" => new VecFpRedPattern()
                          with V2_VecFloat  with V1_ScaFloat  with V3_ScaFloat_DestOnly with SL_Float // VFREDMAX.VS
        case s"001000" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_DestOnly with SL_Float // VFSGNJ.VV
        case s"001001" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_DestOnly with SL_Float // VFSGNJN.VV
        case s"001010" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_DestOnly with SL_Float // VFSGNJX.VV
        case s"010000" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", "00000") => new VecFpS2APattern()
                                   with V2_ScaFloat  with V1_NotUsed with V3_NotUsed with SL_Float // VFMV.F.S
            case _ => null
          }
        case s"010010" =>
          (vm.rawString, vs1.rawString) match {
            case (_, "00000") => new VecFpS2VPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Float // VFCVT.XU.F.V
            case (_, "00001") => new VecFpS2VPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Float // VFCVT.X.F.V
            case (_, "00010") => new VecFpS2VPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFCVT.F.XU.V
            case (_, "00011") => new VecFpS2VPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFCVT.F.X.V
            case (_, "00110") => new VecFpS2VPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Float // VFCVT.RTZ.XU.F.V
            case (_, "00111") => new VecFpS2VPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Float // VFCVT.RTZ.X.F.V
            case (_, "01000") => new VecFpS2VVWPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecWiden_DestOnly with SL_FloatWiden // VFWCVT.XU.F.V
            case (_, "01001") => new VecFpS2VVWPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecWiden_DestOnly with SL_FloatWiden // VFWCVT.X.F.V
            case (_, "01010") => new VecFpS2VVWPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_VecFloatWiden_DestOnly with SL_Widen // VFWCVT.F.XU.V
            case (_, "01011") => new VecFpS2VVWPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_VecFloatWiden_DestOnly with SL_Widen // VFWCVT.F.X.V
            case (_, "01100") => new VecFpS2VVWPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecFloatWiden_DestOnly with SL_FloatWiden // VFWCVT.F.F.V
            case (_, "01101") => new VecFpS2VVWPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecFloatWiden_DestOnly with SL_BF16Widen // VFWCVTBF16.F.F.V
            case (_, "01110") => new VecFpS2VVWPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecWiden_DestOnly with SL_FloatWiden // VFWCVT.RTZ.XU.F.V
            case (_, "01111") => new VecFpS2VVWPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecWiden_DestOnly with SL_FloatWiden // VFWCVT.RTZ.X.F.V
            case (_, "10000") => new VecFpS2WVIntPattern()
                                 with V2_VecFloatWiden   with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VFNCVT.XU.F.W
            case (_, "10001") => new VecFpS2WVIntPattern()
                                 with V2_VecFloatWiden   with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VFNCVT.X.F.W
            case (_, "10010") => new VecFpS2WVFpPattern()
                                 with V2_VecWiden  with V1_NotUsed with V3_VecFloat_DestOnly with SL_FloatWiden // VFNCVT.F.XU.W
            case (_, "10011") => new VecFpS2WVFpPattern()
                                 with V2_VecWiden  with V1_NotUsed with V3_VecFloat_DestOnly with SL_FloatWiden // VFNCVT.F.X.W
            case (_, "10100") => new VecFpS2WVFpPattern()
                                 with V2_VecFloatWiden   with V1_NotUsed with V3_VecFloat_DestOnly with SL_FloatWiden // VFNCVT.F.F.W
            case (_, "10101") => new VecFpS2WVFpPattern()
                                 with V2_VecFloatWiden   with V1_NotUsed with V3_VecFloat_DestOnly with SL_FloatWiden // VFNCVT.ROD.F.F.W
            case (_, "10110") => new VecFpS2WVIntPattern()
                                 with V2_VecFloatWiden   with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VFNCVT.RTZ.XU.F.W
            case (_, "10111") => new VecFpS2WVIntPattern()
                                 with V2_VecFloatWiden   with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VFNCVT.RTZ.X.F.W
            case (_, "11101") => new VecFpS2WVFpPattern()
                                 with V2_VecFloatWiden   with V1_NotUsed with V3_VecFloat_DestOnly with SL_BF16Widen // VFNCVTBF16.F.F.W
            case _ => null
          }
        case s"010011" =>
          (vm.rawString, vs1.rawString) match {
            case (_, "00000") => new VecFpS2VPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFSQRT.V
            case (_, "00100") => new VecFpS2VPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFRSQRT7.V
            case (_, "00101") => new VecFpS2VPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFREC7.V
            case (_, "10000") => new VecFpS2VPattern()
                                 with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFCLASS.V
            case _ => null
          }
        case s"011000" => new VecFpOp2VMPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecMask_DestOnly with SL_Float // VMFEQ.VV
        case s"011001" => new VecFpOp2VMPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecMask_DestOnly with SL_Float // VMFLE.VV
        case s"011011" => new VecFpOp2VMPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecMask_DestOnly with SL_Float // VMFLT.VV
        case s"011100" => new VecFpOp2VMPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecMask_DestOnly with SL_Float // VMFNE.VV
        case s"100000" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_DestOnly with SL_Float // VFDIV.VV
        case s"100100" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_DestOnly with SL_Float // VFMUL.VV
        case s"101000" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_SrcDest with SL_Float // VFMADD.VV
        case s"101001" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_SrcDest with SL_Float // VFNMADD.VV
        case s"101010" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_SrcDest with SL_Float // VFMSUB.VV
        case s"101011" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_SrcDest with SL_Float // VFNMSUB.VV
        case s"101100" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_SrcDest with SL_Float // VFMACC.VV
        case s"101101" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_SrcDest with SL_Float // VFNMACC.VV
        case s"101110" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_SrcDest with SL_Float // VFMSAC.VV
        case s"101111" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloat_SrcDest with SL_Float // VFNMSAC.VV
        case s"110000" => new VecFpOp2VVWPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloatWiden_DestOnly with SL_FloatWiden // VFWADD.VV
        case s"110001" => new VecFpWRedPattern()
                          with V2_VecFloat  with V1_ScaFloatWiden   with V3_ScaFloatWiden_DestOnly with SL_FloatWiden // VFWREDUSUM.VS
        case s"110010" => new VecFpOp2VVWPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloatWiden_DestOnly with SL_FloatWiden // VFWSUB.VV
        case s"110011" => new VecFpWRedPattern()
                          with V2_VecFloat  with V1_ScaFloatWiden   with V3_ScaFloatWiden_DestOnly with SL_FloatWiden // VFWREDOSUM.VS
        case s"110100" => new VecFpOp2WVWPattern()
                          with V2_VecFloatWiden   with V1_VecFloat  with V3_VecFloatWiden_DestOnly with SL_FloatWiden // VFWADD.WV
        case s"110110" => new VecFpOp2WVWPattern()
                          with V2_VecFloatWiden   with V1_VecFloat  with V3_VecFloatWiden_DestOnly with SL_FloatWiden // VFWSUB.WV
        case s"111000" => new VecFpOp2VVWPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloatWiden_DestOnly with SL_FloatWiden // VFWMUL.VV
        case s"111011" => new VecFpOp3VVWPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloatWiden_SrcDest with SL_BF16Widen // VFWMACCBF16.VV
        case s"111100" => new VecFpOp3VVWPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloatWiden_SrcDest with SL_FloatWiden // VFWMACC.VV
        case s"111101" => new VecFpOp3VVWPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloatWiden_SrcDest with SL_FloatWiden // VFWNMACC.VV
        case s"111110" => new VecFpOp3VVWPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloatWiden_SrcDest with SL_FloatWiden // VFWMSAC.VV
        case s"111111" => new VecFpOp3VVWPattern()
                          with V2_VecFloat  with V1_VecFloat  with V3_VecFloatWiden_SrcDest with SL_FloatWiden // VFWNMSAC.VV
        case _ => null
      }
      case OPMVV => f6 match {
        case s"000000" => new VecIntRedPattern()
                          with V2_VecNormal with V1_ScaNormal with V3_ScaNormal_DestOnly with SL_All // VREDSUM.VS
        case s"000001" => new VecIntRedPattern()
                          with V2_VecNormal with V1_ScaNormal with V3_ScaNormal_DestOnly with SL_All // VREDAND.VS
        case s"000010" => new VecIntRedPattern()
                          with V2_VecNormal with V1_ScaNormal with V3_ScaNormal_DestOnly with SL_All // VREDOR.VS
        case s"000011" => new VecIntRedPattern()
                          with V2_VecNormal with V1_ScaNormal with V3_ScaNormal_DestOnly with SL_All // VREDXOR.VS
        case s"000100" => new VecIntRedPattern()
                          with V2_VecNormal with V1_ScaNormal with V3_ScaNormal_DestOnly with SL_All // VREDMINU.VS
        case s"000101" => new VecIntRedPattern()
                          with V2_VecNormal with V1_ScaNormal with V3_ScaNormal_DestOnly with SL_All // VREDMIN.VS
        case s"000110" => new VecIntRedPattern()
                          with V2_VecNormal with V1_ScaNormal with V3_ScaNormal_DestOnly with SL_All // VREDMAXU.VS
        case s"000111" => new VecIntRedPattern()
                          with V2_VecNormal with V1_ScaNormal with V3_ScaNormal_DestOnly with SL_All // VREDMAX.VS
        case s"001000" => new VecIntAvgVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VAADDU.VV
        case s"001001" => new VecIntAvgVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VAADD.VV
        case s"001010" => new VecIntAvgVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VASUBU.VV
        case s"001011" => new VecIntAvgVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VASUB.VV
        case s"001100" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_CLMUL // VCLMUL.VV
        case s"001101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_CLMUL // VCLMULH.VV
        case s"010000" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", "00000") => new VecS2ADXPattern()
                                   with V2_ScaNormal with V1_NotUsed with V3_NotUsed with SL_All // VMV.X.S
            case (_, "10000") => new VecS2MDXPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_NotUsed with SL_All // VCPOP.M
            case (_, "10001") => new VecS2MDXPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_NotUsed with SL_All // VFIRST.M
            case _ => null
          }
        case s"010010" =>
          (vm.rawString, vs1.rawString) match {
            case (_, "00010") => new VecIntS2DVExtF8Pattern()
                                 with V2_VecNarrow8 with V1_NotUsed with V3_VecNormal_DestOnly with SL_Narrow8 // VZEXT.VF8
            case (_, "00011") => new VecIntS2DVExtF8Pattern()
                                 with V2_VecNarrow8 with V1_NotUsed with V3_VecNormal_DestOnly with SL_Narrow8 // VSEXT.VF8
            case (_, "00100") => new VecIntS2DVExtF4Pattern()
                                 with V2_VecNarrow4 with V1_NotUsed with V3_VecNormal_DestOnly with SL_Narrow4 // VZEXT.VF4
            case (_, "00101") => new VecIntS2DVExtF4Pattern()
                                 with V2_VecNarrow4 with V1_NotUsed with V3_VecNormal_DestOnly with SL_Narrow4 // VSEXT.VF4
            case (_, "00110") => new VecIntS2DVExtF2Pattern()
                                 with V2_VecNarrow2 with V1_NotUsed with V3_VecNormal_DestOnly with SL_Narrow2 // VZEXT.VF2
            case (_, "00111") => new VecIntS2DVExtF2Pattern()
                                 with V2_VecNarrow2 with V1_NotUsed with V3_VecNormal_DestOnly with SL_Narrow2 // VSEXT.VF2
            case (_, "01000") => new VecIntS2DVPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VBREV8.V
            case (_, "01001") => new VecIntS2DVPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VREV8.V
            case (_, "01010") => new VecIntS2DVPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VBREV.V
            case (_, "01100") => new VecIntS2DVPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VCLZ.V
            case (_, "01101") => new VecIntS2DVPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VCTZ.V
            case (_, "01110") => new VecIntS2DVPattern()
                                 with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VCPOP.V
            case _ => null
          }
        case s"010100" =>
          (vm.rawString, vs1.rawString, vs2.rawString) match {
            case (_, "10001", "00000") => new VecDVPattern()
                                 with V2_NotUsed with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VID.V
            case (_, "00001", _) => new VecS2MDMPattern()
                                 with V2_VecMask with V1_NotUsed with V3_VecMask_DestOnly with SL_All with OverlapStrict // VMSBF.M
            case (_, "00010", _) => new VecS2MDMPattern()
                                 with V2_VecMask with V1_NotUsed with V3_VecMask_DestOnly with SL_All with OverlapStrict // VMSOF.M
            case (_, "00011", _) => new VecS2MDMPattern()
                                 with V2_VecMask with V1_NotUsed with V3_VecMask_DestOnly with SL_All with OverlapStrict // VMSIF.M
            case (_, "10000", _) => new VecS2MDVPattern()
                                 with V2_VecMask with V1_NotUsed with V3_VecNormal_DestOnly with SL_All with OverlapStrict // VIOTA.M
            case _ => null
          }
        case s"010111" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", _) => new VecCompressPattern()
                             with V2_VecNormal with V1_VecMask with V3_VecNormal_DestOnly with SL_All with OverlapStrict // VCOMPRESS.VM
            case _ => null
          }
        case s"011000" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", _) => new VecIntMMMPattern()
                             with V2_VecMask with V1_VecMask with V3_VecMask_DestOnly with SL_All // VMANDN.MM
            case _ => null
          }
        case s"011001" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", _) => new VecIntMMMPattern()
                             with V2_VecMask with V1_VecMask with V3_VecMask_DestOnly with SL_All // VMAND.MM
            case _ => null
          }
        case s"011010" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", _) => new VecIntMMMPattern()
                             with V2_VecMask with V1_VecMask with V3_VecMask_DestOnly with SL_All // VMOR.MM
            case _ => null
          }
        case s"011011" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", _) => new VecIntMMMPattern()
                             with V2_VecMask with V1_VecMask with V3_VecMask_DestOnly with SL_All // VMXOR.MM
            case _ => null
          }
        case s"011100" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", _) => new VecIntMMMPattern()
                             with V2_VecMask with V1_VecMask with V3_VecMask_DestOnly with SL_All // VMORN.MM
            case _ => null
          }
        case s"011101" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", _) => new VecIntMMMPattern()
                             with V2_VecMask with V1_VecMask with V3_VecMask_DestOnly with SL_All // VMNAND.MM
            case _ => null
          }
        case s"011110" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", _) => new VecIntMMMPattern()
                             with V2_VecMask with V1_VecMask with V3_VecMask_DestOnly with SL_All // VMNOR.MM
            case _ => null
          }
        case s"011111" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", _) => new VecIntMMMPattern()
                             with V2_VecMask with V1_VecMask with V3_VecMask_DestOnly with SL_All // VMXNOR.MM
            case _ => null
          }
        case s"100000" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VDIVU.VV
        case s"100001" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VDIV.VV
        case s"100010" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VREMU.VV
        case s"100011" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VREM.VV
        case s"100100" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VMULHU.VV
        case s"100101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VMUL.VV
        case s"100110" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VMULHSU.VV
        case s"100111" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_DestOnly with SL_All // VMULH.VV
        case s"101001" => new VecIntVVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_SrcDest with SL_All // VMADD.VV
        case s"101011" => new VecIntVVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_SrcDest with SL_All // VNMSUB.VV
        case s"101101" => new VecIntVVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_SrcDest with SL_All // VMACC.VV
        case s"101111" => new VecIntVVVVPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecNormal_SrcDest with SL_All // VNMSAC.VV
        case s"110000" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWADDU.VV
        case s"110001" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWADD.VV
        case s"110010" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWSUBU.VV
        case s"110011" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWSUB.VV
        case s"110100" => new VecIntWVWPattern()
                          with V2_VecWiden  with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWADDU.WV
        case s"110101" => new VecIntWVWPattern()
                          with V2_VecWiden  with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWADD.WV
        case s"110110" => new VecIntWVWPattern()
                          with V2_VecWiden  with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWSUBU.WV
        case s"110111" => new VecIntWVWPattern()
                          with V2_VecWiden  with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWSUB.WV
        case s"111000" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWMULU.VV
        case s"111010" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWMULSU.VV
        case s"111011" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecWiden_DestOnly with SL_Widen // VWMUL.VV
        case s"111100" => new VecIntVVWWPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecWiden_SrcDest with SL_Widen // VWMACCU.VV
        case s"111101" => new VecIntVVWWPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecWiden_SrcDest with SL_Widen // VWMACC.VV
        case s"111111" => new VecIntVVWWPattern()
                          with V2_VecNormal with V1_VecNormal with V3_VecWiden_SrcDest with SL_Widen // VWMACCSU.VV
        case _ => null
      }
      case OPIVI => f6 match {
        case s"000000" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VADD.VI
        case s"000011" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VRSUB.VI
        case s"001001" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VAND.VI
        case s"001010" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VOR.VI
        case s"001011" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VXOR.VI
        case s"001100" => new VecGatherIPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All with OverlapStrict // VRGATHER.VI
        case s"001110" => new VecSlideIPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All with OverlapStrict // VSLIDEUP.VI
        case s"001111" => new VecSlideIPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSLIDEDOWN.VI
        case s"010000" =>
          (vm.rawString, vs1.rawString) match {
            case ("0", _) => new VecCarryPattern()
                             with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VADC.VIM
            case _ => null
          }
        case s"010001" =>
          (vm.rawString, vs1.rawString) match {
            case ("0", _) => new VecCarryMPattern()
                             with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMADC.VIM
            case ("1", _) => new VecCarryMPattern()
                             with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMADC.VI
            case _ => null
          }
        case s"010100" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VROR.VI
        case s"010101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VROR.VI
        case s"01010?" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VROR.VI
        case s"010111" =>
          (vm.rawString, vs1.rawString, vs2.rawString) match {
            case ("0", _, _) => new VecIntVVVPattern()
                             with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMERGE.VIM
            case ("1", _, "00000") => new VecIntS1XDVPattern()
                             with V2_NotUsed with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMV.V.I
            case _ => null
          }
        case s"011000" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSEQ.VI
        case s"011001" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSNE.VI
        case s"011100" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSLEU.VI
        case s"011101" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSLE.VI
        case s"011110" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSGTU.VI
        case s"011111" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSGT.VI
        case s"100000" => new VecIntSatVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSADDU.VI
        case s"100001" => new VecIntSatVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSADD.VI
        case s"100101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSLL.VI
        case s"100111" =>
          (vm.rawString, vs1.rawString) match {
            case ("1", "00000") => new VecIntS2DVWholeMvPattern()
                                   with V2_SpcMV1 with V1_NotUsed with V3_SpcMV1_DestOnly with SL_All // VMV1R.V
            case ("1", "00001") => new VecIntS2DVWholeMvPattern()
                                   with V2_SpcMV2 with V1_NotUsed with V3_SpcMV2_DestOnly with SL_All // VMV2R.V
            case ("1", "00011") => new VecIntS2DVWholeMvPattern()
                                   with V2_SpcMV4 with V1_NotUsed with V3_SpcMV4_DestOnly with SL_All // VMV4R.V
            case ("1", "00111") => new VecIntS2DVWholeMvPattern()
                                   with V2_SpcMV8 with V1_NotUsed with V3_SpcMV8_DestOnly with SL_All // VMV8R.V
            case _ => null
          }
        case s"101000" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSRL.VI
        case s"101001" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSRA.VI
        case s"101010" => new VecIntScaleShiftVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSSRL.VI
        case s"101011" => new VecIntScaleShiftVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSSRA.VI
        case s"101100" => new VecIntNarrowShiftWVVPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VNSRL.WI
        case s"101101" => new VecIntNarrowShiftWVVPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VNSRA.WI
        case s"101110" => new VecIntClipWVVPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VNCLIPU.WI
        case s"101111" => new VecIntClipWVVPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VNCLIP.WI
        case s"110101" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWSLL.VI
        case _ => null
      }
      case OPIVX => f6 match {
        case s"000000" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VADD.VX
        case s"000001" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VANDN.VX
        case s"000010" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSUB.VX
        case s"000011" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VRSUB.VX
        case s"000100" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMINU.VX
        case s"000101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMIN.VX
        case s"000110" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMAXU.VX
        case s"000111" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMAX.VX
        case s"001001" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VAND.VX
        case s"001010" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VOR.VX
        case s"001011" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VXOR.VX
        case s"001100" => new VecGatherXPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All with OverlapStrict // VRGATHER.VX
        case s"001110" => new VecSlideXPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All with OverlapStrict // VSLIDEUP.VX
        case s"001111" => new VecSlideXPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSLIDEDOWN.VX
        case s"010000" =>
          (vm.rawString, vs1.rawString) match {
            case ("0", _) => new VecCarryPattern()
                             with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VADC.VXM
            case _ => null
          }
        case s"010001" =>
          (vm.rawString, vs1.rawString) match {
            case ("0", _) => new VecCarryMPattern()
                             with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMADC.VXM
            case ("1", _) => new VecCarryMPattern()
                             with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMADC.VX
            case _ => null
          }
        case s"010010" =>
          (vm.rawString, vs1.rawString) match {
            case ("0", _) => new VecCarryPattern()
                             with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSBC.VXM
            case _ => null
          }
        case s"010011" =>
          (vm.rawString, vs1.rawString) match {
            case ("0", _) => new VecCarryMPattern()
                             with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSBC.VXM
            case ("1", _) => new VecCarryMPattern()
                             with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSBC.VX
            case _ => null
          }
        case s"010100" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VROR.VX
        case s"010101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VROL.VX
        case s"010111" =>
          (vm.rawString, vs1.rawString, vs2.rawString) match {
            case ("0", _, _) => new VecIntVVVPattern()
                             with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMERGE.VXM
            case ("1", _, "00000") => new VecIntS1XDVPattern()
                             with V2_NotUsed with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMV.V.X
            case _ => null
          }
        case s"011000" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSEQ.VX
        case s"011001" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSNE.VX
        case s"011010" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSLTU.VX
        case s"011011" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSLT.VX
        case s"011100" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSLEU.VX
        case s"011101" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSLE.VX
        case s"011110" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSGTU.VX
        case s"011111" => new VecIntVVMPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecMask_DestOnly with SL_All // VMSGT.VX
        case s"100000" => new VecIntSatVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSADDU.VX
        case s"100001" => new VecIntSatVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSADD.VX
        case s"100010" => new VecIntSatVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSSUBU.VX
        case s"100011" => new VecIntSatVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSSUB.VX
        case s"100101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSLL.VX
        case s"100111" => new VecIntSatMulVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSMUL.VX
        case s"101000" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSRL.VX
        case s"101001" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSRA.VX
        case s"101010" => new VecIntScaleShiftVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSSRL.VX
        case s"101011" => new VecIntScaleShiftVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSSRA.VX
        case s"101100" => new VecIntNarrowShiftWVVPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VNSRL.WX
        case s"101101" => new VecIntNarrowShiftWVVPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VNSRA.WX
        case s"101110" => new VecIntClipWVVPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VNCLIPU.WX
        case s"101111" => new VecIntClipWVVPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecNormal_DestOnly with SL_Widen // VNCLIP.WX
        case s"110101" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWSLL.VX
        case _ => null
      }
      case OPFVF => f6 match {
        case s"000000" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFADD.VF
        case s"000010" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFSUB.VF
        case s"000100" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFMIN.VF
        case s"000110" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFMAX.VF
        case s"001000" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFSGNJ.VF
        case s"001001" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFSGNJN.VF
        case s"001010" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFSGNJX.VF
        case s"001110" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float with OverlapStrict // VFSLIDE1UP.VF
        case s"001111" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFSLIDE1DOWN.VF
        case s"010000" =>
          (vm.rawString, vs1.rawString, vs2.rawString) match {
            case ("1", _, "00000") => new VecFpS1VPattern()
                             with V2_NotUsed with V1_NotUsed with V3_ScaFloat_DestOnly with SL_Float // VFMV.S.F
            case _ => null
          }
        case s"010111" =>
          (vm.rawString, vs1.rawString, vs2.rawString) match {
            case ("0", _, _) => new VecFpOp2VVPattern()
                             with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFMERGE.VFM
            case ("1", _, "00000") => new VecFpS1VPattern()
                             with V2_NotUsed with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFMV.V.F
            case _ => null
          }
        case s"011000" => new VecFpOp2VMPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecMask_DestOnly with SL_Float // VMFEQ.VF
        case s"011001" => new VecFpOp2VMPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecMask_DestOnly with SL_Float // VMFLE.VF
        case s"011011" => new VecFpOp2VMPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecMask_DestOnly with SL_Float // VMFLT.VF
        case s"011100" => new VecFpOp2VMPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecMask_DestOnly with SL_Float // VMFNE.VF
        case s"011101" => new VecFpOp2VMPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecMask_DestOnly with SL_Float // VMFGT.VF
        case s"011111" => new VecFpOp2VMPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecMask_DestOnly with SL_Float // VMFGE.VF
        case s"100000" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFDIV.VF
        case s"100001" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFRDIV.VF
        case s"100100" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFMUL.VF
        case s"100111" => new VecFpOp2VVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_DestOnly with SL_Float // VFRSUB.VF
        case s"101000" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_SrcDest with SL_Float // VFMADD.VF
        case s"101001" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_SrcDest with SL_Float // VFNMADD.VF
        case s"101010" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_SrcDest with SL_Float // VFMSUB.VF
        case s"101011" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_SrcDest with SL_Float // VFNMSUB.VF
        case s"101100" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_SrcDest with SL_Float // VFMACC.VF
        case s"101101" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_SrcDest with SL_Float // VFNMACC.VF
        case s"101110" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_SrcDest with SL_Float // VFMSAC.VF
        case s"101111" => new VecFpOp3VVVPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloat_SrcDest with SL_Float // VFNMSAC.VF
        case s"110000" => new VecFpOp2VVWPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloatWiden_DestOnly with SL_FloatWiden // VFWADD.VF
        case s"110010" => new VecFpOp2VVWPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloatWiden_DestOnly with SL_FloatWiden // VFWSUB.VF
        case s"110100" => new VecFpOp2WVWPattern()
                          with V2_VecFloatWiden   with V1_NotUsed with V3_VecFloatWiden_DestOnly with SL_FloatWiden // VFWADD.WF
        case s"110110" => new VecFpOp2WVWPattern()
                          with V2_VecFloatWiden   with V1_NotUsed with V3_VecFloatWiden_DestOnly with SL_FloatWiden // VFWSUB.WF
        case s"111000" => new VecFpOp2VVWPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloatWiden_DestOnly with SL_FloatWiden // VFWMUL.VF
        case s"111011" => new VecFpOp3VVWPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloatWiden_SrcDest with SL_BF16Widen // VFWMACCBF16.VF
        case s"111100" => new VecFpOp3VVWPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloatWiden_SrcDest with SL_FloatWiden // VFWMACC.VF
        case s"111101" => new VecFpOp3VVWPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloatWiden_SrcDest with SL_FloatWiden // VFWNMACC.VF
        case s"111110" => new VecFpOp3VVWPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloatWiden_SrcDest with SL_FloatWiden // VFWMSAC.VF
        case s"111111" => new VecFpOp3VVWPattern()
                          with V2_VecFloat  with V1_NotUsed with V3_VecFloatWiden_SrcDest with SL_FloatWiden // VFWNMSAC.VF
        case _ => null
      }
      case OPMVX => f6 match {
        case s"001000" => new VecIntAvgVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VAADDU.VX
        case s"001001" => new VecIntAvgVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VAADD.VX
        case s"001010" => new VecIntAvgVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VASUBU.VX
        case s"001011" => new VecIntAvgVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VASUB.VX
        case s"001100" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_CLMUL // VCLMUL.VX
        case s"001101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_CLMUL // VCLMULH.VX
        case s"001110" => new VecSlide1Pattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All with OverlapStrict // VSLIDE1UP.VX
        case s"001111" => new VecSlide1Pattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VSLIDE1DOWN.VX
        case s"010000" =>
          (vm.rawString, vs1.rawString, vs2.rawString) match {
            case ("1", _, "00000") => new VecS1XDAPattern()
                             with V2_NotUsed with V1_NotUsed with V3_ScaNormal_DestOnly with SL_All // VMV.S.X
            case _ => null
          }
        case s"100000" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VDIVU.VX
        case s"100001" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VDIV.VX
        case s"100010" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VREMU.VX
        case s"100011" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VREM.VX
        case s"100100" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMULHU.VX
        case s"100101" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMUL.VX
        case s"100110" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMULHSU.VX
        case s"100111" => new VecIntVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_DestOnly with SL_All // VMULH.VX
        case s"101001" => new VecIntVVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_SrcDest with SL_All // VMADD.VX
        case s"101011" => new VecIntVVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_SrcDest with SL_All // VNMSUB.VX
        case s"101101" => new VecIntVVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_SrcDest with SL_All // VMACC.VX
        case s"101111" => new VecIntVVVVPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecNormal_SrcDest with SL_All // VNMSAC.VX
        case s"110000" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWADDU.VX
        case s"110001" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWADD.VX
        case s"110010" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWSUBU.VX
        case s"110011" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWSUB.VX
        case s"110100" => new VecIntWVWPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWADDU.WX
        case s"110101" => new VecIntWVWPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWADD.WX
        case s"110110" => new VecIntWVWPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWSUBU.WX
        case s"110111" => new VecIntWVWPattern()
                          with V2_VecWiden  with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWSUB.WX
        case s"111000" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWMULU.VX
        case s"111010" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWMULSU.VX
        case s"111011" => new VecIntVVWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_DestOnly with SL_Widen // VWMUL.VX
        case s"111100" => new VecIntVVWWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_SrcDest with SL_Widen // VWMACCU.VX
        case s"111101" => new VecIntVVWWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_SrcDest with SL_Widen // VWMACC.VX
        case s"111110" => new VecIntVVWWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_SrcDest with SL_Widen // VWMACCUS.VX
        case s"111111" => new VecIntVVWWPattern()
                          with V2_VecNormal with V1_NotUsed with V3_VecWiden_SrcDest with SL_Widen // VWMACCSU.VX
        case _ => null
      }
      case _ => null
    }
    } else if (op == "1110111") {
      f3 match {
          case OPMVV => f6 match {
            case s"100000" => new VecCryptoVVVVPattern()
                              with V2_CryptW256E64 with V1_CryptW256E64 with V3_CryptW256E64_DestOnly with SL_CryptW256E64 with OverlapStrict // VSM3ME.VV
            case s"100001" => new VecCryptoVVVVPattern()
                              with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_DestOnly with SL_CryptW128E32 // VSM4K.VI
            case s"100010" => new VecCryptoVVVVPattern()
                              with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_DestOnly with SL_CryptW128E32 // VAESKFI.VI
            case s"101000" => (vm.rawString, vs1.rawString) match {
              case (_, "00000") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 // VAESDM.VV
              case (_, "00001") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 // VAESDF.VV
              case (_, "00010") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 // VAESEM.VV
              case (_, "00011") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 // VAESEF.VV
              case (_, "10000") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 // VSM4R.VV
              case (_, "10001") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 // VGMUL.VV
              case _ => null
            }
            case s"101001" => (vm.rawString, vs1.rawString) match {
              case (_, "00000") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 with OverlapStrict // VAESDM.VS
              case (_, "00001") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 with OverlapStrict // VAESDF.VS
              case (_, "00010") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 with OverlapStrict // VAESEM.VS
              case (_, "00011") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 with OverlapStrict // VAESEF.VS
              case (_, "00111") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 with OverlapStrict // VAESZ.VS
              case (_, "10000") => new VecCryptoVVVVPattern()
                                   with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 with OverlapStrict // VSM4R.VS
              case _ => null
            }
            case s"101010" => new VecCryptoVVVVPattern()
                              with V2_CryptW128E32 with V1_NotUsed with V3_CryptW128E32_SrcDest with SL_CryptW128E32 // VAESKF2.VI
            case s"101011" => new VecCryptoVVVVPattern()
                              with V2_CryptW256E64 with V1_NotUsed with V3_CryptW256E64_DestOnly with SL_CryptW256E64 with OverlapStrict // VSM3C.VI
            case s"101100" => new VecCryptoVVVVPattern()
                              with V2_CryptW128E32 with V1_CryptW128E32 with V3_CryptW128E32_SrcDest with SL_CryptW128E32 // VGHSH.VV
            case s"101101" => new VecCryptoVVVVPattern()
                              with V2_CryptBoth with V1_CryptBoth with V3_CryptBoth_SrcDest with SL_CryptBoth with OverlapStrict // VSHA2MS.VV
            case s"101110" => new VecCryptoVVVVPattern()
                              with V2_CryptBoth with V1_CryptBoth with V3_CryptBoth_SrcDest with SL_CryptBoth with OverlapStrict // VSHA2CH.VV
            case s"101111" => new VecCryptoVVVVPattern()
                              with V2_CryptBoth with V1_CryptBoth with V3_CryptBoth_SrcDest with SL_CryptBoth with OverlapStrict // VSHA2CL.VV
            case _ => null
          }
          case _ => null
        }
    } else {
      null
    }
  }
}
sealed class VecArithInstPattern()(
  implicit rawInst: BitPat,
) extends VecInstPattern() {
  def func6 : BitPat = rawInst(31, 26)
  def vm    : BitPat = rawInst(25)
  def category : BitPat = rawInst(14, 12)

  override def bitPat: BitPat = genPattern

  val genPattern = rawInst.ensuring(_.getWidth == 32)
}

abstract class VecIntArithInstPattern()(
  implicit rawInst: BitPat,
) extends VecArithInstPattern

abstract class VecFpArithInstPattern()(
  implicit rawInst: BitPat,
) extends VecArithInstPattern

abstract class VecCryptoInstPattern()(
  implicit rawInst: BitPat,
) extends VecArithInstPattern

case class VecIntVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntVVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntVVMPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntMMMPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntVVWPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntVVWWPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntWVWPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntSatVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntSatMulVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntScaleShiftVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntNarrowShiftWVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntClipWVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntAvgVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecGatherVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecGatherXPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecGatherIPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecGatherEI16Pattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecCompressPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecSlideXPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecSlideIPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecSlide1Pattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecCarryPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecCarryMPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntS1VDVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntS1XDVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecS1XDAPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntS2DVWholeMvPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntS2DVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntS2DVExtF8Pattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntS2DVExtF4Pattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntS2DVExtF2Pattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
// Todo: vmv.s.x treat 0 < idx < VLMAX as tail.
case class VecS2ADXPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecS2MDXPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecS2MDMPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecS2MDVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecDVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntRedPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntWRedPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecFpOp2VVPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpOp2VMPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpOp3VVVPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpRedPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpWRedPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpOp2VVWPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpOp2WVWPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpOp3VVWPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS2VPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS2VVWPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS2WVIntPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS2WVFpPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS2APattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS1VPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern

case class VecCryptoVVVVPattern()(implicit rawInst: BitPat) extends VecCryptoInstPattern

sealed trait Vs2Type extends VecArithInstPattern
sealed trait V2_SpcMV1 extends Vs2Type
sealed trait V2_SpcMV2 extends Vs2Type
sealed trait V2_SpcMV4 extends Vs2Type
sealed trait V2_SpcMV8 extends Vs2Type
sealed trait V2_CryptW128E32 extends Vs2Type
sealed trait V2_CryptW256E64 extends Vs2Type
sealed trait V2_CryptBoth extends Vs2Type
sealed trait V2_NotUsed extends Vs2Type
sealed trait V2_ScaFloat  extends Vs2Type
sealed trait V2_ScaNormal extends Vs2Type
sealed trait V2_VecFloatWiden   extends Vs2Type
sealed trait V2_VecFloat  extends Vs2Type
sealed trait V2_VecMask extends Vs2Type
sealed trait V2_VecNarrow2 extends Vs2Type
sealed trait V2_VecNarrow4 extends Vs2Type
sealed trait V2_VecNarrow8 extends Vs2Type
sealed trait V2_VecNormal extends Vs2Type
sealed trait V2_VecWiden  extends Vs2Type

sealed trait Vs1Type extends VecArithInstPattern
sealed trait V1_SpcGather16 extends Vs1Type
sealed trait V1_CryptW128E32 extends Vs1Type
sealed trait V1_CryptW256E64 extends Vs1Type
sealed trait V1_CryptBoth extends Vs1Type
sealed trait V1_NotUsed extends Vs1Type
sealed trait V1_ScaFloatWiden   extends Vs1Type
sealed trait V1_ScaFloat  extends Vs1Type
sealed trait V1_ScaNormal extends Vs1Type
sealed trait V1_ScaWiden  extends Vs1Type
sealed trait V1_VecFloat  extends Vs1Type
sealed trait V1_VecMask extends Vs1Type
sealed trait V1_VecNormal extends Vs1Type

sealed trait Vs3Type extends VecArithInstPattern
sealed trait V3_SpcMV1_DestOnly extends Vs3Type
sealed trait V3_SpcMV2_DestOnly extends Vs3Type
sealed trait V3_SpcMV4_DestOnly extends Vs3Type
sealed trait V3_SpcMV8_DestOnly extends Vs3Type
sealed trait V3_NotUsed extends Vs3Type
sealed trait V3_ScaFloatWiden_DestOnly extends Vs3Type
sealed trait V3_ScaFloat_DestOnly extends Vs3Type
sealed trait V3_ScaNormal_DestOnly extends Vs3Type
sealed trait V3_ScaWiden_DestOnly extends Vs3Type
sealed trait V3_VecFloatWiden_SrcDest extends Vs3Type
sealed trait V3_VecFloatWiden_DestOnly extends Vs3Type
sealed trait V3_VecFloat_SrcDest extends Vs3Type
sealed trait V3_VecFloat_DestOnly extends Vs3Type
sealed trait V3_VecMask_DestOnly extends Vs3Type
sealed trait V3_VecNormal_SrcDest extends Vs3Type
sealed trait V3_VecNormal_DestOnly extends Vs3Type
sealed trait V3_VecWiden_SrcDest extends Vs3Type
sealed trait V3_VecWiden_DestOnly extends Vs3Type
sealed trait V3_CryptW128E32_DestOnly extends Vs3Type
sealed trait V3_CryptW256E64_DestOnly extends Vs3Type
sealed trait V3_CryptBoth_SrcDest extends Vs3Type
sealed trait V3_CryptW128E32_SrcDest extends Vs3Type

sealed trait SewLmulType extends VecArithInstPattern
sealed trait SL_BF16Widen extends SewLmulType
sealed trait SL_CLMUL extends SewLmulType
sealed trait SL_FloatWiden extends SewLmulType
sealed trait SL_Gather16 extends SewLmulType
sealed trait SL_Float extends SewLmulType
sealed trait SL_Narrow2 extends SewLmulType
sealed trait SL_Narrow4 extends SewLmulType
sealed trait SL_Narrow8 extends SewLmulType
sealed trait SL_All extends SewLmulType
sealed trait SL_Widen extends SewLmulType
sealed trait SL_CryptW128E32 extends SewLmulType
sealed trait SL_CryptW256E64 extends SewLmulType
sealed trait SL_CryptBoth extends SewLmulType

sealed trait OverlapStrict extends VecArithInstPattern

trait VecInstFieldDefination {
  def opcode(implicit rawInst: BitPat): BitPat = rawInst(6, 0)
  def func6(implicit rawInst: BitPat): BitPat = rawInst(31, 26)
  def vm(implicit rawInst: BitPat): BitPat = rawInst(25)
  def vs2(implicit rawInst: BitPat): BitPat = rawInst(24, 20)
  def vs1(implicit rawInst: BitPat): BitPat = rawInst(19, 15)
  def category(implicit rawInst: BitPat): BitPat = rawInst(14, 12)
  def vd(implicit rawInst: BitPat): BitPat = rawInst(11, 7)
}
