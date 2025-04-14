//package xiangshan.backend.vector.Decoder
//
//import chisel3._
//import chisel3.experimental.hierarchy.{instantiable, public}
//import chisel3.util._
//import chisel3.util.experimental.decode.{DecodeBundle, DecodeTable}
//import freechips.rocketchip.rocket.Instructions._
//import freechips.rocketchip.rocket.{CSRs, Instructions}
//import org.chipsalliance.cde.config.Parameters
//import top.{ArgParser, Generator}
//import utility.LookupTree
//import xiangshan.ExceptionNO.{EX_II, breakPoint, illegalInstr, virtualInstr}
//import xiangshan.backend.Bundles.{DecodedInst, NeedFrmBundle}
//import xiangshan.backend.decode.VecDecoder.vset
//import xiangshan.backend.decode._
//import xiangshan.backend.decode.isa.PseudoInstructions
//import xiangshan.{ALUOpType, BKUOpType, CSROpType, CustomCSRCtrlIO, FenceOpType, LSUOpType, SelImm, SrcType, TriggerAction, UopSplitType, VSETOpType, VlduType, VstuType, XSCoreParamsKey, XSModule, XSTileKey}
//import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, OPCODE5Bit, Riscv32BitInst, XSInstBitFields}
//import xiangshan.backend.fu.FuType
//import xiangshan.backend.fu.vector.Bundles.{VLmul, VSew}
//import xiangshan.backend.fu.wrapper.CSRToDecode
//import xiangshan.backend.vector.Decoder.DecodeFields._
//import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
//import xiangshan.backend.vector.Decoder.Split.{SplitType, VecUopSplitModule}
//import xiangshan.backend.vector.Decoder.SplitCtlDecoderUtil.InstNfLmulSewPattern
//import xiangshan.backend.vector.Decoder.Types.EnumLMUL
//import xiangshan.backend.vector.Decoder.Uop.UopInfoRenameWithIllegal
//import xiangshan.backend.vector.Decoder.UopSplitModuleMain.args
//import xiangshan.backend.vector._
//import xiangshan.backend.vector.util.ChiselTypeExt._
//import xiangshan.backend.vector.util.Verilog
//import xiangshan.macros.InstanceNameMacro.getVariableName
//
//class NormalDecodeChannel()(implicit p: Parameters) extends XSModule {
//  val in = IO(Input(new Bundle {
//    val fromCSR = Input(new CSRToDecode)
//    val instInfo = new DecodeChannelInput
//    val csrCtrl = Input(new CustomCSRCtrlIO)
//  }))
//  val out = IO(Output(new Bundle {
//    val uop = new NormalDecodeChannelOutputUop
//  }))
//
//  private val fromCSR = in.fromCSR
//
//  private val inst = in.instInfo.rawInst
//  private val instFields: XSInstBitFields = inst.asTypeOf(new XSInstBitFields)
//
//  val decode_table: Array[(BitPat, List[BitPat])] = XDecode.table ++
//    FpDecode.table ++
//    BitmanipDecode.table ++
//    ScalarCryptoDecode.table ++
//    XSTrapDecode.table ++
//    CBODecode.table ++
//    SvinvalDecode.table ++
//    HypervisorDecode.table ++
//    FpDecoder.table ++
//    ZicondDecode.table ++
//    ZimopDecode.table ++
//    ZfaDecode.table
//
//  require(decode_table.map(_._2.length == 15).reduce(_ && _), "Decode tables have different column size")
//  // assertion for LUI: only LUI should be assigned `selImm === SelImm.IMM_U && fuType === FuType.alu`
//  val luiMatch = (t: Seq[BitPat]) => t(3).value == FuType.alu.ohid && t.reverse.head.value == SelImm.IMM_U.litValue
//  val luiTable = decode_table.filter(t => luiMatch(t._2)).map(_._1).distinct
//  assert(luiTable.length == 1 && luiTable.head == LUI, "Conflicts: LUI is determined by FuType and SelImm in Dispatch")
//
//  // output
//  val decodedInst: DecodedInst = Wire(new DecodedInst()).decode(instFields.inst, decode_table)
//
//  val fpDecoder = Module(new FPDecoder)
//  fpDecoder.io.instr := instFields.inst
//  decodedInst.fpu := fpDecoder.io.fpCtrl
//  decodedInst.fpu.wflags := fpDecoder.io.fpCtrl.wflags || decodedInst.wfflags
//
//  decodedInst.connectStaticInst(io.enq.ctrlFlow)
//
//  decodedInst.uopIdx := 0.U
//  decodedInst.firstUop := true.B
//  decodedInst.lastUop := true.B
//  decodedInst.numUops := 1.U
//  decodedInst.numWB   := 1.U
//
//  val isZimop = Seq(Zimop.MOP_R, Zimop.MOP_RR).map(_ === instFields.inst).reduce(_ || _)
//
//  val isMove = PseudoInstructions.MOV === instFields.inst
//  // temp decode zimop as move
//  decodedInst.isMove := (isMove || isZimop) && instFields.RD =/= 0.U && !in.csrCtrl.singlestep
//
//  // fmadd - b1000011
//  // fmsub - b1000111
//  // fnmsub- b1001011
//  // fnmadd- b1001111
//  private val isFMA = instFields.OPCODE === BitPat("b100??11")
//  private val isVppu = FuType.isVppu(decodedInst.fuType)
//  private val isVecOPF = FuType.isVecOPF(decodedInst.fuType)
//
//  // read src1~3 location
//  decodedInst.lsrc(0) := instFields.RS1
//  decodedInst.lsrc(1) := instFields.RS2
//  // src(2) of fma is fs3, src(2) of vector inst is old vd
//  decodedInst.lsrc(2) := Mux(isFMA, instFields.FS3, instFields.VD)
//  decodedInst.lsrc(3) := V0_IDX.U
//  decodedInst.lsrc(4) := Vl_IDX.U
//
//  // read dest location
//  decodedInst.ldest := instFields.RD
//
//  // init v0Wen vlWen
//  decodedInst.v0Wen := false.B
//  decodedInst.vlWen := false.B
//
//  private val isCboClean = CBO_CLEAN === instFields.inst
//  private val isCboFlush = CBO_FLUSH === instFields.inst
//  private val isCboInval = CBO_INVAL === instFields.inst
//  private val isCboZero  = CBO_ZERO  === instFields.inst
//
//  // Note that rnum of aes64ks1i must be in the range 0x0..0xA. The values 0xB..0xF are reserved.
//  private val isAes64ks1iIllegal =
//    FuType.FuTypeOrR(decodedInst.fuType, FuType.bku) && (decodedInst.fuOpType === BKUOpType.aes64ks1i) && instFields.isRnumIllegal
//
//  private val isAmocasQ = FuType.FuTypeOrR(decodedInst.fuType, FuType.mou) && decodedInst.fuOpType === LSUOpType.amocas_q
//  private val isAmocasQIllegal = isAmocasQ && (instFields.RD(0) === 1.U || instFields.RS2(0) === 1.U)
//
//  private val exceptionII =
//    decodedInst.selImm === SelImm.INVALID_INSTR ||
//      fromCSR.illegalInst.sfenceVMA  && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.sfence  ||
//      fromCSR.illegalInst.sfencePart && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.nofence ||
//      fromCSR.illegalInst.hfenceGVMA && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.hfence_g ||
//      fromCSR.illegalInst.hfenceVVMA && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.hfence_v ||
//      fromCSR.illegalInst.hlsv       && FuType.FuTypeOrR(decodedInst.fuType, FuType.ldu)   && (LSUOpType.isHlv(decodedInst.fuOpType) || LSUOpType.isHlvx(decodedInst.fuOpType)) ||
//      fromCSR.illegalInst.hlsv       && FuType.FuTypeOrR(decodedInst.fuType, FuType.stu)   && LSUOpType.isHsv(decodedInst.fuOpType) ||
//      fromCSR.illegalInst.fsIsOff    && (
//        FuType.FuTypeOrR(decodedInst.fuType, FuType.fpOP ++ Seq(FuType.f2v)) ||
//          (FuType.FuTypeOrR(decodedInst.fuType, FuType.ldu) && (decodedInst.fuOpType === LSUOpType.lh || decodedInst.fuOpType === LSUOpType.lw || decodedInst.fuOpType === LSUOpType.ld) ||
//            FuType.FuTypeOrR(decodedInst.fuType, FuType.stu) && (decodedInst.fuOpType === LSUOpType.sh || decodedInst.fuOpType === LSUOpType.sw || decodedInst.fuOpType === LSUOpType.sd)) && decodedInst.instr(2) ||
//          instFields.isOPFVF || instFields.isOPFVV
//        ) ||
//      fromCSR.illegalInst.vsIsOff    && FuType.FuTypeOrR(decodedInst.fuType, FuType.vecAll) ||
//      fromCSR.illegalInst.wfi        && FuType.FuTypeOrR(decodedInst.fuType, FuType.csr)   && CSROpType.isWfi(decodedInst.fuOpType) ||
//      fromCSR.illegalInst.wrs_nto    && FuType.FuTypeOrR(decodedInst.fuType, FuType.csr)   && CSROpType.isWrsNto(decodedInst.fuOpType) ||
//      (decodedInst.needFrm.scalaNeedFrm || FuType.isScalaNeedFrm(decodedInst.fuType)) && (((decodedInst.fpu.rm === 5.U) || (decodedInst.fpu.rm === 6.U)) || ((decodedInst.fpu.rm === 7.U) && fromCSR.illegalInst.frm)) ||
//      (decodedInst.needFrm.vectorNeedFrm || FuType.isVectorNeedFrm(decodedInst.fuType)) && fromCSR.illegalInst.frm ||
//      fromCSR.illegalInst.cboZ       && isCboZero ||
//      fromCSR.illegalInst.cboCF      && (isCboClean || isCboFlush) ||
//      fromCSR.illegalInst.cboI       && isCboInval ||
//      isAes64ks1iIllegal ||
//      isAmocasQIllegal
//
//  private val exceptionVI =
//    fromCSR.virtualInst.sfenceVMA  && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.sfence ||
//      fromCSR.virtualInst.sfencePart && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.nofence ||
//      fromCSR.virtualInst.hfence     && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && (decodedInst.fuOpType === FenceOpType.hfence_g || decodedInst.fuOpType === FenceOpType.hfence_v) ||
//      fromCSR.virtualInst.hlsv       && FuType.FuTypeOrR(decodedInst.fuType, FuType.ldu)   && (LSUOpType.isHlv(decodedInst.fuOpType) || LSUOpType.isHlvx(decodedInst.fuOpType)) ||
//      fromCSR.virtualInst.hlsv       && FuType.FuTypeOrR(decodedInst.fuType, FuType.stu)   && LSUOpType.isHsv(decodedInst.fuOpType) ||
//      fromCSR.virtualInst.wfi        && FuType.FuTypeOrR(decodedInst.fuType, FuType.csr)   && CSROpType.isWfi(decodedInst.fuOpType) ||
//      fromCSR.virtualInst.wrs_nto    && FuType.FuTypeOrR(decodedInst.fuType, FuType.csr)   && CSROpType.isWrsNto(decodedInst.fuOpType) ||
//      fromCSR.virtualInst.cboZ       && isCboZero ||
//      fromCSR.virtualInst.cboCF      && (isCboClean || isCboFlush) ||
//      fromCSR.virtualInst.cboI       && isCboInval
//
//
//  decodedInst.exceptionVec(illegalInstr) := exceptionII || io.enq.ctrlFlow.exceptionVec(EX_II)
//  decodedInst.exceptionVec(virtualInstr) := exceptionVI
//
//  //update exceptionVec: from frontend trigger's breakpoint exception. To reduce 1 bit of overhead in ibuffer entry.
//  decodedInst.exceptionVec(breakPoint) := TriggerAction.isExp(ctrl_flow.trigger)
//
//  decodedInst.imm := LookupTree(decodedInst.selImm, ImmUnion.immSelMap.map(
//    x => {
//      val minBits = x._2.minBitsFromInstr(inst)
//      require(minBits.getWidth == x._2.len)
//      x._1 -> minBits
//    }
//  ))
//
//  private val isLs = FuType.isLoadStore(decodedInst.fuType)
//  private val isVls = instFields.isVecStore || instFields.isVecLoad
//  private val isStore = FuType.isStore(decodedInst.fuType)
//  private val isAMO = FuType.isAMO(decodedInst.fuType)
//  private val isVStore = FuType.isVStore(decodedInst.fuType)
//  private val isBranch = !decodedInst.preDecodeInfo.notCFI || FuType.isJump(decodedInst.fuType)
//
//  decodedInst.commitType := Cat(isLs | isVls, (isStore && !isAMO) | isVStore | isBranch)
//
//  decodedInst.isVset := FuType.isVset(decodedInst.fuType)
//
//  private val needReverseInsts = Seq(VRSUB_VI, VRSUB_VX, VFRDIV_VF, VFRSUB_VF, VFMV_F_S)
//  private val vextInsts = Seq(VZEXT_VF2, VZEXT_VF4, VZEXT_VF8, VSEXT_VF2, VSEXT_VF4, VSEXT_VF8)
//  private val narrowInsts = Seq(
//    VNSRA_WV, VNSRA_WX, VNSRA_WI, VNSRL_WV, VNSRL_WX, VNSRL_WI,
//    VNCLIP_WV, VNCLIP_WX, VNCLIP_WI, VNCLIPU_WV, VNCLIPU_WX, VNCLIPU_WI,
//  )
//  private val maskDstInsts = Seq(
//    VMADC_VV, VMADC_VX,  VMADC_VI,  VMADC_VVM, VMADC_VXM, VMADC_VIM,
//    VMSBC_VV, VMSBC_VX,  VMSBC_VVM, VMSBC_VXM,
//    VMAND_MM, VMNAND_MM, VMANDN_MM, VMXOR_MM, VMOR_MM, VMNOR_MM, VMORN_MM, VMXNOR_MM,
//    VMSEQ_VV, VMSEQ_VX, VMSEQ_VI, VMSNE_VV, VMSNE_VX, VMSNE_VI,
//    VMSLE_VV, VMSLE_VX, VMSLE_VI, VMSLEU_VV, VMSLEU_VX, VMSLEU_VI,
//    VMSLT_VV, VMSLT_VX, VMSLTU_VV, VMSLTU_VX,
//    VMSGT_VX, VMSGT_VI, VMSGTU_VX, VMSGTU_VI,
//    VMFEQ_VV, VMFEQ_VF, VMFNE_VV, VMFNE_VF, VMFLT_VV, VMFLT_VF, VMFLE_VV, VMFLE_VF, VMFGT_VF, VMFGE_VF,
//  )
//  private val maskOpInsts = Seq(
//    VMAND_MM, VMNAND_MM, VMANDN_MM, VMXOR_MM, VMOR_MM, VMNOR_MM, VMORN_MM, VMXNOR_MM,
//  )
//  private val vmaInsts = Seq(
//    VMACC_VV, VMACC_VX, VNMSAC_VV, VNMSAC_VX, VMADD_VV, VMADD_VX, VNMSUB_VV, VNMSUB_VX,
//    VWMACCU_VV, VWMACCU_VX, VWMACC_VV, VWMACC_VX, VWMACCSU_VV, VWMACCSU_VX, VWMACCUS_VX,
//  )
//  private val wfflagsInsts = Seq(
//    // opfff
//    FADD_S, FSUB_S, FADD_D, FSUB_D, FADD_H, FSUB_H,
//    FEQ_S, FLT_S, FLE_S, FEQ_D, FLT_D, FLE_D, FEQ_H, FLT_H, FLE_H,
//    FMIN_S, FMAX_S, FMIN_D, FMAX_D, FMIN_H, FMAX_H,
//    FMUL_S, FMUL_D, FMUL_H,
//    FDIV_S, FDIV_D, FSQRT_S, FSQRT_D, FDIV_H, FSQRT_H,
//    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S, FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D, FMADD_H, FMSUB_H, FNMADD_H, FNMSUB_H,
//    FSGNJ_S, FSGNJN_S, FSGNJX_S, FSGNJ_H, FSGNJN_H, FSGNJX_H,
//    // opfvv
//    VFADD_VV, VFSUB_VV, VFWADD_VV, VFWSUB_VV, VFWADD_WV, VFWSUB_WV,
//    VFMUL_VV, VFDIV_VV, VFWMUL_VV,
//    VFMACC_VV, VFNMACC_VV, VFMSAC_VV, VFNMSAC_VV, VFMADD_VV, VFNMADD_VV, VFMSUB_VV, VFNMSUB_VV,
//    VFWMACC_VV, VFWNMACC_VV, VFWMSAC_VV, VFWNMSAC_VV,
//    VFSQRT_V,
//    VFMIN_VV, VFMAX_VV,
//    VMFEQ_VV, VMFNE_VV, VMFLT_VV, VMFLE_VV,
//    VFSGNJ_VV, VFSGNJN_VV, VFSGNJX_VV,
//    // opfvf
//    VFADD_VF, VFSUB_VF, VFRSUB_VF, VFWADD_VF, VFWSUB_VF, VFWADD_WF, VFWSUB_WF,
//    VFMUL_VF, VFDIV_VF, VFRDIV_VF, VFWMUL_VF,
//    VFMACC_VF, VFNMACC_VF, VFMSAC_VF, VFNMSAC_VF, VFMADD_VF, VFNMADD_VF, VFMSUB_VF, VFNMSUB_VF,
//    VFWMACC_VF, VFWNMACC_VF, VFWMSAC_VF, VFWNMSAC_VF,
//    VFMIN_VF, VFMAX_VF,
//    VMFEQ_VF, VMFNE_VF, VMFLT_VF, VMFLE_VF, VMFGT_VF, VMFGE_VF,
//    VFSGNJ_VF, VFSGNJN_VF, VFSGNJX_VF,
//    // vfred
//    VFREDOSUM_VS, VFREDUSUM_VS, VFREDMAX_VS, VFREDMIN_VS, VFWREDOSUM_VS, VFWREDUSUM_VS,
//    // fcvt & vfcvt
//    FCVT_S_W, FCVT_S_WU, FCVT_S_L, FCVT_S_LU,
//    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
//    FCVT_D_W, FCVT_D_WU, FCVT_D_L, FCVT_D_LU,
//    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D, FCVT_S_D, FCVT_D_S,
//    FCVT_S_H, FCVT_H_S, FCVT_H_D, FCVT_D_H,
//    FCVT_H_W, FCVT_H_WU, FCVT_H_L, FCVT_H_LU,
//    FCVT_W_H, FCVT_WU_H, FCVT_L_H, FCVT_LU_H,
//    VFCVT_XU_F_V, VFCVT_X_F_V, VFCVT_RTZ_XU_F_V, VFCVT_RTZ_X_F_V, VFCVT_F_XU_V, VFCVT_F_X_V,
//    VFWCVT_XU_F_V, VFWCVT_X_F_V, VFWCVT_RTZ_XU_F_V, VFWCVT_RTZ_X_F_V, VFWCVT_F_XU_V, VFWCVT_F_X_V, VFWCVT_F_F_V,
//    VFNCVT_XU_F_W, VFNCVT_X_F_W, VFNCVT_RTZ_XU_F_W, VFNCVT_RTZ_X_F_W, VFNCVT_F_XU_W, VFNCVT_F_X_W, VFNCVT_F_F_W,
//    VFNCVT_ROD_F_F_W, VFRSQRT7_V, VFREC7_V,
//    // zfa
//    FLEQ_H, FLEQ_S, FLEQ_D, FLTQ_H, FLTQ_S, FLTQ_D,
//    FMINM_H, FMINM_S, FMINM_D, FMAXM_H, FMAXM_S, FMAXM_D,
//    FROUND_H, FROUND_S, FROUND_D, FROUNDNX_H, FROUNDNX_S, FROUNDNX_D,
//    FCVTMOD_W_D,
//  )
//
//  private val scalaNeedFrmInsts = Seq(
//    FADD_S, FSUB_S, FADD_D, FSUB_D, FADD_H, FSUB_H,
//    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
//    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D, FCVT_S_D, FCVT_D_S,
//    FCVT_W_H, FCVT_WU_H, FCVT_L_H, FCVT_LU_H,
//    FCVT_S_H, FCVT_H_S, FCVT_H_D, FCVT_D_H,
//    FROUND_H, FROUND_S, FROUND_D, FROUNDNX_H, FROUNDNX_S, FROUNDNX_D,
//  )
//
//  private val vectorNeedFrmInsts = Seq (
//    VFSLIDE1UP_VF, VFSLIDE1DOWN_VF,
//  )
//
//  decodedInst.wfflags := wfflagsInsts.map(_ === instFields.ALL).reduce(_ || _)
//  decodedInst.needFrm.scalaNeedFrm := scalaNeedFrmInsts.map(_ === instFields.ALL).reduce(_ || _)
//  decodedInst.needFrm.vectorNeedFrm := vectorNeedFrmInsts.map(_ === instFields.ALL).reduce(_ || _)
//  decodedInst.vpu := 0.U.asTypeOf(decodedInst.vpu) // Todo: Connect vpu decoder
//  decodedInst.vpu.vill := io.enq.vtype.illegal
//  decodedInst.vpu.vma := io.enq.vtype.vma
//  decodedInst.vpu.vta := io.enq.vtype.vta
//  decodedInst.vpu.vsew := io.enq.vtype.vsew
//  decodedInst.vpu.vlmul := io.enq.vtype.vlmul
//  decodedInst.vpu.vm := instFields.VM
//  decodedInst.vpu.nf := instFields.NF
//  decodedInst.vpu.veew := instFields.WIDTH
//  decodedInst.vpu.isReverse := needReverseInsts.map(_ === instFields.ALL).reduce(_ || _)
//  decodedInst.vpu.isExt := vextInsts.map(_ === instFields.ALL).reduce(_ || _)
//  val isNarrow = narrowInsts.map(_ === instFields.ALL).reduce(_ || _)
//  val isDstMask = maskDstInsts.map(_ === instFields.ALL).reduce(_ || _)
//  val isOpMask = maskOpInsts.map(_ === instFields.ALL).reduce(_ || _)
//  val isVload = FuType.isVLoad(decodedInst.fuType)
//  val isVlx = isVload && (decodedInst.fuOpType === VlduType.vloxe || decodedInst.fuOpType === VlduType.vluxe)
//  val isVle = isVload && (decodedInst.fuOpType === VlduType.vle || decodedInst.fuOpType === VlduType.vleff || decodedInst.fuOpType === VlduType.vlse)
//  val isVlm = isVload && (decodedInst.fuOpType === VlduType.vlm)
//  val isFof = isVload && (decodedInst.fuOpType === VlduType.vleff)
//  val isWritePartVd = decodedInst.uopSplitType === UopSplitType.VEC_VRED || decodedInst.uopSplitType === UopSplitType.VEC_0XV || decodedInst.uopSplitType === UopSplitType.VEC_VWW
//  val isVma = vmaInsts.map(_ === instFields.ALL).reduce(_ || _)
//  val emulIsFrac = Cat(~decodedInst.vpu.vlmul(2), decodedInst.vpu.vlmul(1, 0)) +& decodedInst.vpu.veew < 4.U +& decodedInst.vpu.vsew
//  val vstartIsNotZero = io.enq.vstart =/= 0.U
//  decodedInst.vpu.isNarrow := isNarrow
//  decodedInst.vpu.isDstMask := isDstMask
//  decodedInst.vpu.isOpMask := isOpMask
//  decodedInst.vpu.isDependOldVd := isVppu || isVecOPF || isVStore || (isDstMask && !isOpMask) || isNarrow || isVlx || isVma || isFof || vstartIsNotZero
//  decodedInst.vpu.isWritePartVd := isWritePartVd || isVlm || isVle && emulIsFrac
//  decodedInst.vpu.vstart := io.enq.vstart
//  decodedInst.vpu.isVleff := isFof && instFields.NF === 0.U
//  decodedInst.vpu.specVill := io.enq.vtype.illegal
//  decodedInst.vpu.specVma := io.enq.vtype.vma
//  decodedInst.vpu.specVta := io.enq.vtype.vta
//  decodedInst.vpu.specVsew := io.enq.vtype.vsew
//  decodedInst.vpu.specVlmul := io.enq.vtype.vlmul
//
//  decodedInst.vlsInstr := isVls
//
//  decodedInst.srcType(3) := Mux(instFields.VM === 0.U, SrcType.vp, SrcType.DC) // mask src
//  decodedInst.srcType(4) := SrcType.vp // vconfig
//
//  val uopInfoGen = Module(new UopInfoGen)
//  uopInfoGen.io.in.preInfo.isVecArith := instFields.isVecArith
//  uopInfoGen.io.in.preInfo.isVecMem := instFields.isVecStore || instFields.isVecLoad
//  uopInfoGen.io.in.preInfo.isAmoCAS := instFields.isAMOCAS
//
//  uopInfoGen.io.in.preInfo.typeOfSplit := decodedInst.uopSplitType
//  uopInfoGen.io.in.preInfo.vsew := decodedInst.vpu.vsew
//  uopInfoGen.io.in.preInfo.vlmul := decodedInst.vpu.vlmul
//  uopInfoGen.io.in.preInfo.vwidth := instFields.RM
//  uopInfoGen.io.in.preInfo.vmvn := instFields.IMM5_OPIVI(2, 0)
//  uopInfoGen.io.in.preInfo.nf := instFields.NF
//  uopInfoGen.io.in.preInfo.isVlsr := decodedInst.fuOpType === VlduType.vlr || decodedInst.fuOpType === VstuType.vsr
//  uopInfoGen.io.in.preInfo.isVlsm := decodedInst.fuOpType === VlduType.vlm || decodedInst.fuOpType === VstuType.vsm
//  io.deq.isComplex := uopInfoGen.io.out.isComplex
//  io.deq.uopInfo.numOfUop := uopInfoGen.io.out.uopInfo.numOfUop
//  io.deq.uopInfo.numOfWB := uopInfoGen.io.out.uopInfo.numOfWB
//  io.deq.uopInfo.lmul := uopInfoGen.io.out.uopInfo.lmul
//
//  val isCsr = instFields.OPCODE5Bit === OPCODE5Bit.SYSTEM && instFields.FUNCT3(1, 0) =/= 0.U
//  val isCsrr = isCsr && instFields.FUNCT3 === BitPat("b?1?") && instFields.RS1 === 0.U
//  val isCsrw = isCsr && instFields.FUNCT3 === BitPat("b?01") && instFields.RD  === 0.U
//  dontTouch(isCsrr)
//  dontTouch(isCsrw)
//
//  // for csrr vl instruction, convert to vsetvl
//  val isCsrrVlenb = isCsrr && instFields.CSRIDX === CSRs.vlenb.U
//  val isCsrrVl    = isCsrr && instFields.CSRIDX === CSRs.vl.U
//
//  // decode for SoftPrefetch instructions (prefetch.w / prefetch.r / prefetch.i)
//  val isSoftPrefetch = instFields.OPCODE === BitPat("b0010011") && instFields.FUNCT3 === BitPat("b110") && instFields.RD === 0.U
//  val isPreW = isSoftPrefetch && instFields.RS2 === 3.U(5.W)
//  val isPreR = isSoftPrefetch && instFields.RS2 === 1.U(5.W)
//  val isPreI = isSoftPrefetch && instFields.RS2 === 0.U(5.W)
//
//  // for fli.s|fli.d instruction
//  val isFLI = instFields.FUNCT7 === BitPat("b11110??") && instFields.RS2 === 1.U && instFields.RM === 0.U && instFields.OPCODE5Bit === OPCODE5Bit.OP_FP
//
//  when (isCsrrVl) {
//    // convert to vsetvl instruction
//    decodedInst.srcType(0) := SrcType.no
//    decodedInst.srcType(1) := SrcType.no
//    decodedInst.srcType(2) := SrcType.no
//    decodedInst.srcType(3) := SrcType.no
//    decodedInst.srcType(4) := SrcType.vp
//    decodedInst.lsrc(4)    := Vl_IDX.U
//    decodedInst.waitForward   := false.B
//    decodedInst.blockBackward := false.B
//    decodedInst.exceptionVec(illegalInstr) := fromCSR.illegalInst.vsIsOff
//  }.elsewhen (isCsrrVlenb) {
//    // convert to addi instruction
//    decodedInst.srcType(0) := SrcType.reg
//    decodedInst.srcType(1) := SrcType.imm
//    decodedInst.srcType(2) := SrcType.no
//    decodedInst.srcType(3) := SrcType.no
//    decodedInst.srcType(4) := SrcType.no
//    decodedInst.selImm := SelImm.IMM_I
//    decodedInst.waitForward := false.B
//    decodedInst.blockBackward := false.B
//    decodedInst.canRobCompress := true.B
//    decodedInst.exceptionVec(illegalInstr) := fromCSR.illegalInst.vsIsOff
//  }.elsewhen (isPreW || isPreR || isPreI) {
//    decodedInst.selImm := SelImm.IMM_S
//    decodedInst.fuType := FuType.ldu.U
//    decodedInst.canRobCompress := false.B
//  }.elsewhen (isZimop) {
//    // set srcType for zimop
//    decodedInst.srcType(0) := SrcType.reg
//    decodedInst.srcType(1) := SrcType.imm
//    // use x0 as src1
//    decodedInst.lsrc(0) := 0.U
//  }
//
//  io.deq.decodedInst := decodedInst
//  io.deq.decodedInst.rfWen := (decodedInst.ldest =/= 0.U) && decodedInst.rfWen
//  io.deq.decodedInst.fuType := Mux1H(Seq(
//    // keep condition
//    (!FuType.FuTypeOrR(decodedInst.fuType, FuType.vldu, FuType.vstu) && !isCsrrVl && !isCsrrVlenb) -> decodedInst.fuType,
//    (isCsrrVl) -> FuType.vsetfwf.U,
//    (isCsrrVlenb) -> FuType.alu.U,
//
//    // change vlsu to vseglsu when NF =/= 0.U
//    ( FuType.FuTypeOrR(decodedInst.fuType, FuType.vldu, FuType.vstu) && instFields.NF === 0.U || (instFields.NF =/= 0.U && (instFields.MOP === "b00".U && instFields.SUMOP === "b01000".U))) -> decodedInst.fuType,
//    // MOP === b00 && SUMOP === b01000: unit-stride whole register store
//    // MOP =/= b00                    : strided and indexed store
//    ( FuType.FuTypeOrR(decodedInst.fuType, FuType.vstu)              && instFields.NF =/= 0.U && ((instFields.MOP === "b00".U && instFields.SUMOP =/= "b01000".U) || instFields.MOP =/= "b00".U)) -> FuType.vsegstu.U,
//    // MOP === b00 && LUMOP === b01000: unit-stride whole register load
//    // MOP =/= b00                    : strided and indexed load
//    ( FuType.FuTypeOrR(decodedInst.fuType, FuType.vldu)              && instFields.NF =/= 0.U && ((instFields.MOP === "b00".U && instFields.LUMOP =/= "b01000".U) || instFields.MOP =/= "b00".U)) -> FuType.vsegldu.U,
//  ))
//  io.deq.decodedInst.imm := MuxCase(decodedInst.imm, Seq(
//    isCsrrVlenb -> (VLEN / 8).U,
//    isZimop     -> 0.U,
//  ))
//
//  io.deq.decodedInst.fuOpType := MuxCase(decodedInst.fuOpType, Seq(
//    isCsrrVl    -> VSETOpType.csrrvl,
//    isCsrrVlenb -> ALUOpType.add,
//    isFLI       -> Cat(1.U, instFields.FMT, instFields.RS1),
//    (isPreW || isPreR || isPreI) -> Mux1H(Seq(
//      isPreW -> LSUOpType.prefetch_w,
//      isPreR -> LSUOpType.prefetch_r,
//      isPreI -> LSUOpType.prefetch_i,
//    )),
//    (isCboInval && fromCSR.special.cboI2F) -> LSUOpType.cbo_flush,
//  ))
//
//  // Don't compress in the same Rob entry when crossing Ftq entry boundary
//  io.deq.decodedInst.canRobCompress := decodedInst.canRobCompress && !io.enq.ctrlFlow.isLastInFtqEntry
//
//  out.uop.renameInfo
//  out.uop.src
//  out.uop.fu
//  out.uop.ctrl
//  out.uop.imm
//  out.uop.selImm
//
//  object FpDecoder extends DecodeConstants {
//    override val decodeArray: Array[(BitPat, XSDecodeBase)] = VecDecoder.opfff
//  }
//}
//
//class NormalDecodeChannelOutputUop extends Bundle with HasVectorSettings {
//  val renameInfo = new UopInfoRenameWithIllegal
//  val src = new UopSrcBundle
//  val fu = new FuInfo
//  val ctrl = new DecoderCtrlInfo
//  val imm = UInt(ImmUnion.maxLen.W)
//  val selImm = SelImm()
//}
//
//object NormalDecodeChannelMain extends App {
//  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
//    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")
//
//  val defaultConfig = config.alterPartial({
//    // Get XSCoreParams and pass it to the "small module"
//    case XSCoreParamsKey => config(XSTileKey).head
//    case XSVectorParamKey => XSVectorParameters(128)
//  })
//
//  Generator.execute(
//    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "backend",
//    new NormalDecodeChannel()(defaultConfig),
//    firtoolOpts
//  )
//}
