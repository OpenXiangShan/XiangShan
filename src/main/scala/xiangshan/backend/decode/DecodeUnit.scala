/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.backend.decode.isa.bitfield.{InstVType, XSInstBitFields}

abstract class Imm(val len: Int, val typEncode: UInt) {
  def toImm32(minBits: UInt): UInt = do_toImm32(minBits(len - 1, 0))
  def extract(width: Int)(minBits: UInt): UInt = ???
  def do_toImm32(minBits: UInt): UInt
  def minBitsFromInstr(instr: UInt): UInt
}

case class Imm_I() extends Imm(12, SelImm.IMM_I) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits(len - 1, 0), 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 20))
}

case class Imm_S() extends Imm(12, SelImm.IMM_S) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 25), instr(11, 7))
}

case class Imm_B() extends Imm(12, SelImm.IMM_SB) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31), instr(7), instr(30, 25), instr(11, 8))
}

case class Imm_U() extends Imm(20, SelImm.IMM_U) {
  override def do_toImm32(minBits: UInt): UInt = Cat(minBits(len - 1, 0), 0.U(12.W))

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(31, 12)
  }
}

case class Imm_J() extends Imm(20, SelImm.IMM_UJ) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(31), instr(19, 12), instr(20), instr(30, 25), instr(24, 21))
  }
}

case class Imm_Z() extends Imm(12 + 5 + 5, SelImm.IMM_Z) {
  override def do_toImm32(minBits: UInt): UInt = minBits

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(11, 7), instr(19, 15), instr(31, 20))
  }

  def getCSRAddr(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(11, 0)
  }

  def getRS1(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(16, 12)
  }

  def getRD(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(21, 17)
  }

  def getImm5(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(16, 12)
  }
}

case class Imm_OPIVIS() extends Imm(5, SelImm.IMM_OPIVIS) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def extract(width: Int)(imm: UInt): UInt = SignExt(imm.take(5), width)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(19, 15)
  }
}

case class Imm_OPIVIU() extends Imm(5, SelImm.IMM_OPIVIU) {
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)

  override def extract(width: Int)(imm: UInt): UInt = ZeroExt(imm.take(5), width)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(19, 15)
  }
}

case class Imm_FI() extends Imm(5, SelImm.IMM_FI) {
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)

  override def extract(width: Int)(imm: UInt): UInt = ZeroExt(imm.take(5), width)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(19, 15)
  }
}

case class Imm_VSETVLI() extends Imm(11, SelImm.IMM_VSETVLI) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(30, 20)
  }

  /**
   * get VType from extended imm
   * @param extedImm
   * @return VType
   */
  def getVType(extedImm: UInt): InstVType = {
    val vtype = Wire(new InstVType)
    vtype := extedImm(10, 0).asTypeOf(new InstVType)
    vtype
  }

  def getVTypei(imm: UInt): UInt = {
    imm(10, 0)
  }
}

case class Imm_VSETIVLI() extends Imm(15, SelImm.IMM_VSETIVLI) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    val rvInst: XSInstBitFields = instr.asTypeOf(new XSInstBitFields)
    val uimm5 = rvInst.UIMM_VSETIVLI
    val vtype8 = rvInst.ZIMM_VSETIVLI
    Cat(uimm5, vtype8)
  }

  /**
   * get VType from extended imm
   * @param extedImm
   * @return VType
   */
  def getVType(extedImm: UInt): InstVType = {
    val vtype = Wire(new InstVType)
    vtype := extedImm(9, 0).asTypeOf(new InstVType)
    vtype
  }

  def getVTypei(imm: UInt): UInt = {
    imm(9, 0)
  }

  def getAvl(extedImm: UInt): UInt = {
    extedImm(14, 10)
  }
}

case class Imm_LUI32() extends Imm(32, SelImm.IMM_LUI32) {
  override def do_toImm32(minBits: UInt): UInt = minBits(31, 0)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(31, 0)
  }
}

case class Imm_VRORVI() extends Imm(6, SelImm.IMM_VRORVI) {
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(26), instr(19, 15))
  }
}

object ImmUnion {
  val I = Imm_I()
  val S = Imm_S()
  val B = Imm_B()
  val U = Imm_U()
  val J = Imm_J()
  val Z = Imm_Z()
  val OPIVIS = Imm_OPIVIS()
  val OPIVIU = Imm_OPIVIU()
  val FI = Imm_FI()
  val VSETVLI = Imm_VSETVLI()
  val VSETIVLI = Imm_VSETIVLI()
  val LUI32 = Imm_LUI32()
  val VRORVI = Imm_VRORVI()

  // do not add special type lui32 to this, keep ImmUnion max len being 20.
  val imms = Seq(I, S, B, U, J, Z, OPIVIS, OPIVIU, FI, VSETVLI, VSETIVLI, VRORVI)
  val immSelMap = Seq(
    SelImm.IMM_I,
    SelImm.IMM_S,
    SelImm.IMM_SB,
    SelImm.IMM_U,
    SelImm.IMM_UJ,
    SelImm.IMM_Z,
    SelImm.IMM_OPIVIS,
    SelImm.IMM_OPIVIU,
    SelImm.IMM_FI,
    SelImm.IMM_VSETVLI,
    SelImm.IMM_VSETIVLI,
    SelImm.IMM_VRORVI,
  ).zip(imms)
  println(s"ImmUnion max len: $maxLen")
}

/**
 * IO bundle for the Decode unit
 */

class DecodeUnitEnqIO(implicit p: Parameters) extends XSBundle {
  val decodeInUop = Input(new DecodeInUop)
  val vtype = Input(new VType)
  val vstart = Input(Vl())
}

class DecodeUnitDeqIO(implicit p: Parameters) extends XSBundle {
  val decodedInst = Output(new DecodeOutUop)
  val isComplex = Output(Bool())
  val uopInfo = Output(new UopInfo)
}

class DecodeUnitIO(implicit p: Parameters) extends XSBundle {
  val enq = new DecodeUnitEnqIO
  //  val vconfig = Input(UInt(XLEN.W))
  val deq = new DecodeUnitDeqIO
  val csrCtrl = Input(new CustomCSRCtrlIO)
  val fromCSR = Input(new CSRToDecode)
}

/**
 * Decode unit that takes in a single CtrlFlow and generates a CfCtrl.
 */
class DecodeUnit(implicit p: Parameters) extends XSModule with DecodeUnitConstants {
  val io = IO(new DecodeUnitIO)

  val ctrl_flow = io.enq.decodeInUop // input with RVC Expanded

  private val inst: XSInstBitFields = io.enq.decodeInUop.instr.asTypeOf(new XSInstBitFields)
  val decode_table: Array[(BitPat, List[BitPat])] = XDecode.table ++
    FpDecode.table ++
//    FDivSqrtDecode.table ++
    BitmanipDecode.table ++
    ScalarCryptoDecode.table ++
    XSDebugDecode.table ++
    CBODecode.table ++
    SvinvalDecode.table ++
    HypervisorDecode.table ++
    VecDecoder.table ++
    ZicondDecode.table ++
    ZimopDecode.table ++
    ZfaDecode.table ++
    (if (HasMptCheck) MptFenceDecode.table else Array.empty[(BitPat, List[BitPat])])
  require(decode_table.map(_._2.length == 14).reduce(_ && _), "Decode tables have different column size")
  // assertion for LUI: only LUI should be assigned `selImm === SelImm.IMM_U && fuType === FuType.alu`
  val luiMatch = (t: Seq[BitPat]) => t(3).value == FuType.alu.ohid && t.reverse.head.value == SelImm.IMM_U.litValue
  val luiTable = decode_table.filter(t => luiMatch(t._2)).map(_._1).distinct
  assert(luiTable.length == 1 && luiTable.head == LUI, "Conflicts: LUI is determined by FuType and SelImm in Dispatch")

  // output
  val decodedInst: DecodeOutUop = Wire(new DecodeOutUop()).decode(ctrl_flow.instr, decode_table)
  decodedInst.connectDecodeInUop(io.enq.decodeInUop)

  decodedInst.uopIdx := 0.U
  decodedInst.firstUop := true.B
  decodedInst.lastUop := true.B
  val numWBIs2 = FuType.isStore(decodedInst.fuType) || FuType.isJump(decodedInst.fuType) && (decodedInst.ldest =/= 0.U)
  decodedInst.numWB   := Mux(numWBIs2, 2.U, 1.U)
  decodedInst.simple := false.B

  val isZimop = (BitPat("b1?00??0111??_?????_100_?????_1110011") === ctrl_flow.instr) ||
                (BitPat("b1?00??1?????_?????_100_?????_1110011") === ctrl_flow.instr)

  val isMove = BitPat("b000000000000_?????_000_?????_0010011") === ctrl_flow.instr
  // temp decode zimop as move
  decodedInst.isMove := (isMove || isZimop) && ctrl_flow.instr(RD_MSB, RD_LSB) =/= 0.U && !io.csrCtrl.singlestep

  // fmadd - b1000011
  // fmsub - b1000111
  // fnmsub- b1001011
  // fnmadd- b1001111
  private val isFMA = inst.OPCODE === BitPat("b100??11")
  private val isVppu = FuType.isVppu(decodedInst.fuType)

  // read src1~3 location
  decodedInst.lsrc(0) := inst.RS1
  decodedInst.lsrc(1) := inst.RS2
  // src(2) of fma is fs3, src(2) of vector inst is old vd
  decodedInst.lsrc(2) := Mux(isFMA, inst.FS3, inst.VD)
  decodedInst.lsrc(3) := V0_IDX.U

  // read dest location
  decodedInst.ldest := inst.RD

  // init v0Wen vlWen
  decodedInst.v0Wen := false.B
  decodedInst.vlWen := false.B

  val isCsr = inst.OPCODE5Bit === OPCODE5Bit.SYSTEM && inst.FUNCT3(1, 0) =/= 0.U
  val isCsrr = isCsr && inst.FUNCT3 === BitPat("b?1?") && inst.RS1 === 0.U
  val isCsrw = isCsr && inst.FUNCT3 === BitPat("b?01") && inst.RD  === 0.U
  dontTouch(isCsrr)
  dontTouch(isCsrw)

  // for csrr vl instruction, convert to vsetvl
  val isCsrrVlenb = isCsrr && inst.CSRIDX === CSRs.vlenb.U
  val isCsrrVl    = isCsrr && inst.CSRIDX === CSRs.vl.U

  private val isCboClean = CBO_CLEAN === io.enq.decodeInUop.instr
  private val isCboFlush = CBO_FLUSH === io.enq.decodeInUop.instr
  private val isCboInval = CBO_INVAL === io.enq.decodeInUop.instr
  private val isCboZero  = CBO_ZERO  === io.enq.decodeInUop.instr

  // Note that rnum of aes64ks1i must be in the range 0x0..0xA. The values 0xB..0xF are reserved.
  private val isAes64ks1iIllegal =
    FuType.FuTypeOrR(decodedInst.fuType, FuType.bku) && (decodedInst.fuOpType === BKUOpType.aes64ks1i) && inst.isRnumIllegal

  private val isAmocasQ = FuType.FuTypeOrR(decodedInst.fuType, FuType.mou) && decodedInst.fuOpType === LSUOpType.amocas_q
  private val isAmocasQIllegal = isAmocasQ && (inst.RD(0) === 1.U || inst.RS2(0) === 1.U)

  private val exceptionII =
    decodedInst.selImm === SelImm.INVALID_INSTR ||
    (if (HasMptCheck) (io.fromCSR.illegalInst.mfence.get && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.mfence) else false.B) ||
    io.fromCSR.illegalInst.sfenceVMA  && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.sfence  ||
    io.fromCSR.illegalInst.sfencePart && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.nofence ||
    io.fromCSR.illegalInst.hfenceGVMA && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.hfence_g ||
    io.fromCSR.illegalInst.hfenceVVMA && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.hfence_v ||
    io.fromCSR.illegalInst.hlsv       && FuType.FuTypeOrR(decodedInst.fuType, FuType.ldu)   && (LSUOpType.isHlv(decodedInst.fuOpType) || LSUOpType.isHlvx(decodedInst.fuOpType)) ||
    io.fromCSR.illegalInst.hlsv       && FuType.FuTypeOrR(decodedInst.fuType, FuType.stu)   && LSUOpType.isHsv(decodedInst.fuOpType) ||
    io.fromCSR.illegalInst.fsIsOff    && (
      FuType.FuTypeOrR(decodedInst.fuType, FuType.fpOP ++ Seq(FuType.f2v)) ||
      (FuType.FuTypeOrR(decodedInst.fuType, FuType.ldu) && (decodedInst.fuOpType === LSUOpType.lh || decodedInst.fuOpType === LSUOpType.lw || decodedInst.fuOpType === LSUOpType.ld) ||
      FuType.FuTypeOrR(decodedInst.fuType, FuType.stu) && (decodedInst.fuOpType === LSUOpType.sh || decodedInst.fuOpType === LSUOpType.sw || decodedInst.fuOpType === LSUOpType.sd)) && decodedInst.instr(2) ||
      inst.isOPFVF || inst.isOPFVV
    ) ||
    io.fromCSR.illegalInst.vsIsOff    && (FuType.FuTypeOrR(decodedInst.fuType, FuType.vecAll) || isCsrrVl || isCsrrVlenb) ||
    io.fromCSR.illegalInst.wfi        && FuType.FuTypeOrR(decodedInst.fuType, FuType.csr)   && CSROpType.isWfi(decodedInst.fuOpType) ||
    io.fromCSR.illegalInst.wrs_nto    && FuType.FuTypeOrR(decodedInst.fuType, FuType.csr)   && CSROpType.isWrsNto(decodedInst.fuOpType) ||
    (decodedInst.needFrm.scalaNeedFrm || FuType.isScalaNeedFrm(decodedInst.fuType)) && (((decodedInst.fpu.rm === 5.U) || (decodedInst.fpu.rm === 6.U)) || ((decodedInst.fpu.rm === 7.U) && io.fromCSR.illegalInst.frm)) ||
    (decodedInst.needFrm.vectorNeedFrm || FuType.isVectorNeedFrm(decodedInst.fuType)) && io.fromCSR.illegalInst.frm ||
    (io.fromCSR.illegalInst.cboZ  || !HasCMO.B) && isCboZero ||
    (io.fromCSR.illegalInst.cboCF || !HasCMO.B) && (isCboClean || isCboFlush) ||
    (io.fromCSR.illegalInst.cboI  || !HasCMO.B) && isCboInval ||
    isAes64ks1iIllegal ||
    isAmocasQIllegal

  private val exceptionVI =
    io.fromCSR.virtualInst.sfenceVMA  && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.sfence ||
    io.fromCSR.virtualInst.sfencePart && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && decodedInst.fuOpType === FenceOpType.nofence ||
    io.fromCSR.virtualInst.hfence     && FuType.FuTypeOrR(decodedInst.fuType, FuType.fence) && (decodedInst.fuOpType === FenceOpType.hfence_g || decodedInst.fuOpType === FenceOpType.hfence_v) ||
    io.fromCSR.virtualInst.hlsv       && FuType.FuTypeOrR(decodedInst.fuType, FuType.ldu)   && (LSUOpType.isHlv(decodedInst.fuOpType) || LSUOpType.isHlvx(decodedInst.fuOpType)) ||
    io.fromCSR.virtualInst.hlsv       && FuType.FuTypeOrR(decodedInst.fuType, FuType.stu)   && LSUOpType.isHsv(decodedInst.fuOpType) ||
    io.fromCSR.virtualInst.wfi        && FuType.FuTypeOrR(decodedInst.fuType, FuType.csr)   && CSROpType.isWfi(decodedInst.fuOpType) ||
    io.fromCSR.virtualInst.wrs_nto    && FuType.FuTypeOrR(decodedInst.fuType, FuType.csr)   && CSROpType.isWrsNto(decodedInst.fuOpType) ||
    io.fromCSR.virtualInst.cboZ       && isCboZero ||
    io.fromCSR.virtualInst.cboCF      && (isCboClean || isCboFlush) ||
    io.fromCSR.virtualInst.cboI       && isCboInval


  decodedInst.exceptionVec(illegalInstr) := exceptionII || io.enq.decodeInUop.exceptionVec(illegalInstr)
  decodedInst.exceptionVec(virtualInstr) := exceptionVI

  //update exceptionVec: from frontend trigger's breakpoint exception. To reduce 1 bit of overhead in ibuffer entry.
  decodedInst.exceptionVec(breakPoint) := TriggerAction.isExp(ctrl_flow.trigger)

  decodedInst.imm := LookupTree(decodedInst.selImm, ImmUnion.immSelMap.map(
    x => {
      val minBits = x._2.minBitsFromInstr(ctrl_flow.instr)
      require(minBits.getWidth == x._2.len)
      x._1 -> minBits
    }
  ))

  private val isLs = FuType.isLoadStore(decodedInst.fuType)
  private val isVls = inst.isVecStore || inst.isVecLoad
  private val isStore = FuType.isStore(decodedInst.fuType)
  private val isAMO = FuType.isAMO(decodedInst.fuType)
  private val isVStore = FuType.isVStore(decodedInst.fuType)

  decodedInst.commitType := Cat(isLs | isVls, (isStore && !isAMO) | isVStore)

  decodedInst.isVset := FuType.isVset(decodedInst.fuType)

  private val needReverseInsts = Seq(VRSUB_VI, VRSUB_VX, VFRDIV_VF, VFRSUB_VF)
  private val vextInsts = Seq(VZEXT_VF2, VZEXT_VF4, VZEXT_VF8, VSEXT_VF2, VSEXT_VF4, VSEXT_VF8)
  private val narrowInsts = Seq(
    VNSRA_WV, VNSRA_WX, VNSRA_WI, VNSRL_WV, VNSRL_WX, VNSRL_WI,
    VNCLIP_WV, VNCLIP_WX, VNCLIP_WI, VNCLIPU_WV, VNCLIPU_WX, VNCLIPU_WI,
  )
  private val maskDstInsts = Seq(
    VMADC_VV, VMADC_VX,  VMADC_VI,  VMADC_VVM, VMADC_VXM, VMADC_VIM,
    VMSBC_VV, VMSBC_VX,  VMSBC_VVM, VMSBC_VXM,
    VMAND_MM, VMNAND_MM, VMANDN_MM, VMXOR_MM, VMOR_MM, VMNOR_MM, VMORN_MM, VMXNOR_MM,
    VMSEQ_VV, VMSEQ_VX, VMSEQ_VI, VMSNE_VV, VMSNE_VX, VMSNE_VI,
    VMSLE_VV, VMSLE_VX, VMSLE_VI, VMSLEU_VV, VMSLEU_VX, VMSLEU_VI,
    VMSLT_VV, VMSLT_VX, VMSLTU_VV, VMSLTU_VX,
    VMSGT_VX, VMSGT_VI, VMSGTU_VX, VMSGTU_VI,
    VMFEQ_VV, VMFEQ_VF, VMFNE_VV, VMFNE_VF, VMFLT_VV, VMFLT_VF, VMFLE_VV, VMFLE_VF, VMFGT_VF, VMFGE_VF,
  )
  private val maskOpInsts = Seq(
    VMAND_MM, VMNAND_MM, VMANDN_MM, VMXOR_MM, VMOR_MM, VMNOR_MM, VMORN_MM, VMXNOR_MM,
  )
  private val vmaInsts = Seq(
    VMACC_VV, VMACC_VX, VNMSAC_VV, VNMSAC_VX, VMADD_VV, VMADD_VX, VNMSUB_VV, VNMSUB_VX,
    VWMACCU_VV, VWMACCU_VX, VWMACC_VV, VWMACC_VX, VWMACCSU_VV, VWMACCSU_VX, VWMACCUS_VX,
  )
  private val wfflagsInsts = Seq(
    // opfff
    FADD_S, FSUB_S, FADD_D, FSUB_D, FADD_H, FSUB_H,
    FEQ_S, FLT_S, FLE_S, FEQ_D, FLT_D, FLE_D, FEQ_H, FLT_H, FLE_H,
    FMIN_S, FMAX_S, FMIN_D, FMAX_D, FMIN_H, FMAX_H,
    FMUL_S, FMUL_D, FMUL_H,
    FDIV_S, FDIV_D, FSQRT_S, FSQRT_D, FDIV_H, FSQRT_H,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S, FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D, FMADD_H, FMSUB_H, FNMADD_H, FNMSUB_H,
    FSGNJ_S, FSGNJN_S, FSGNJX_S, FSGNJ_H, FSGNJN_H, FSGNJX_H,
    // opfvv
    VFADD_VV, VFSUB_VV, VFWADD_VV, VFWSUB_VV, VFWADD_WV, VFWSUB_WV,
    VFMUL_VV, VFDIV_VV, VFWMUL_VV,
    VFMACC_VV, VFNMACC_VV, VFMSAC_VV, VFNMSAC_VV, VFMADD_VV, VFNMADD_VV, VFMSUB_VV, VFNMSUB_VV,
    VFWMACC_VV, VFWNMACC_VV, VFWMSAC_VV, VFWNMSAC_VV,
    VFSQRT_V,
    VFMIN_VV, VFMAX_VV,
    VMFEQ_VV, VMFNE_VV, VMFLT_VV, VMFLE_VV,
    VFSGNJ_VV, VFSGNJN_VV, VFSGNJX_VV,
    // opfvf
    VFADD_VF, VFSUB_VF, VFRSUB_VF, VFWADD_VF, VFWSUB_VF, VFWADD_WF, VFWSUB_WF,
    VFMUL_VF, VFDIV_VF, VFRDIV_VF, VFWMUL_VF,
    VFMACC_VF, VFNMACC_VF, VFMSAC_VF, VFNMSAC_VF, VFMADD_VF, VFNMADD_VF, VFMSUB_VF, VFNMSUB_VF,
    VFWMACC_VF, VFWNMACC_VF, VFWMSAC_VF, VFWNMSAC_VF,
    VFMIN_VF, VFMAX_VF,
    VMFEQ_VF, VMFNE_VF, VMFLT_VF, VMFLE_VF, VMFGT_VF, VMFGE_VF,
    VFSGNJ_VF, VFSGNJN_VF, VFSGNJX_VF,
    // vfred
    VFREDOSUM_VS, VFREDUSUM_VS, VFREDMAX_VS, VFREDMIN_VS, VFWREDOSUM_VS, VFWREDUSUM_VS,
    // fcvt & vfcvt
    FCVT_S_W, FCVT_S_WU, FCVT_S_L, FCVT_S_LU,
    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
    FCVT_D_W, FCVT_D_WU, FCVT_D_L, FCVT_D_LU,
    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D, FCVT_S_D, FCVT_D_S,
    FCVT_S_H, FCVT_H_S, FCVT_H_D, FCVT_D_H,
    FCVT_H_W, FCVT_H_WU, FCVT_H_L, FCVT_H_LU,
    FCVT_W_H, FCVT_WU_H, FCVT_L_H, FCVT_LU_H,
    VFCVT_XU_F_V, VFCVT_X_F_V, VFCVT_RTZ_XU_F_V, VFCVT_RTZ_X_F_V, VFCVT_F_XU_V, VFCVT_F_X_V,
    VFWCVT_XU_F_V, VFWCVT_X_F_V, VFWCVT_RTZ_XU_F_V, VFWCVT_RTZ_X_F_V, VFWCVT_F_XU_V, VFWCVT_F_X_V, VFWCVT_F_F_V,
    VFNCVT_XU_F_W, VFNCVT_X_F_W, VFNCVT_RTZ_XU_F_W, VFNCVT_RTZ_X_F_W, VFNCVT_F_XU_W, VFNCVT_F_X_W, VFNCVT_F_F_W,
    VFNCVT_ROD_F_F_W, VFRSQRT7_V, VFREC7_V,
    // zfa
    FLEQ_H, FLEQ_S, FLEQ_D, FLTQ_H, FLTQ_S, FLTQ_D,
    FMINM_H, FMINM_S, FMINM_D, FMAXM_H, FMAXM_S, FMAXM_D,
    FROUND_H, FROUND_S, FROUND_D, FROUNDNX_H, FROUNDNX_S, FROUNDNX_D,
    FCVTMOD_W_D,
  )

  private val scalaNeedFrmInsts = Seq(
    FADD_S, FSUB_S, FADD_D, FSUB_D, FADD_H, FSUB_H,
    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D, FCVT_S_D, FCVT_D_S,
    FCVT_W_H, FCVT_WU_H, FCVT_L_H, FCVT_LU_H,
    FCVT_S_H, FCVT_H_S, FCVT_H_D, FCVT_D_H,
    FROUND_H, FROUND_S, FROUND_D, FROUNDNX_H, FROUNDNX_S, FROUNDNX_D,
  )

  private val vectorNeedFrmInsts = Seq (
    VFSLIDE1UP_VF, VFSLIDE1DOWN_VF,
  )

  private val vectorFloatNarrow = Seq (
    VFREDOSUM_VS, VFREDUSUM_VS, VFREDMAX_VS, VFREDMIN_VS, VFWREDOSUM_VS, VFWREDUSUM_VS,
    VFNCVT_XU_F_W, VFNCVT_X_F_W, VFNCVT_RTZ_XU_F_W, VFNCVT_RTZ_X_F_W, VFNCVT_F_XU_W, VFNCVT_F_X_W, VFNCVT_F_F_W, VFNCVT_ROD_F_F_W,
    VFMV_S_F,
  )

  private val scalarSew32 = Seq(
    FADD_S, FSUB_S, FEQ_S, FLT_S, FLE_S, FMIN_S, FMAX_S,
    FMUL_S, FDIV_S, FSQRT_S,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S,
    FCLASS_S, FSGNJ_S, FSGNJX_S, FSGNJN_S,
    // zfa inst
    FLEQ_S, FLTQ_S, FMINM_S, FMAXM_S,
    FROUND_S, FROUNDNX_S,

    // scalar cvt inst
    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
    FCVT_W_D, FCVT_WU_D, FCVT_S_D, FCVT_D_S,
    FMV_X_W,
    // zfa inst
    FCVTMOD_W_D,
    // i2f cvt & mv
    FCVT_S_W, FCVT_S_WU, FCVT_S_L, FCVT_S_LU,
    FCVT_D_W, FCVT_D_WU, FMV_W_X,
  )
  /*
  The optype for FCVT_D_H and FCVT_H_D is the same,
  so the two instructions are distinguished by sew.
  e64 -> e16: VSew.e64
  e16 -> e64: VSew.e16
   */
  private val scalarSew16 = Seq(
    // zfh inst
    FADD_H, FSUB_H, FEQ_H, FLT_H, FLE_H, FMIN_H, FMAX_H,
    FMUL_H, FDIV_H, FSQRT_H,
    FMADD_H, FMSUB_H, FNMADD_H, FNMSUB_H,
    FCLASS_H, FSGNJ_H, FSGNJX_H, FSGNJN_H,
    // zfa inst
    FLEQ_H, FLTQ_H, FMINM_H, FMAXM_H,
    FROUND_H, FROUNDNX_H,

    FCVT_S_H, FCVT_H_S, FCVT_D_H,
    FCVT_W_H, FCVT_L_H, FCVT_WU_H, FCVT_LU_H,
    FMV_X_H,
    // i2f cvt & mv
    FCVT_H_W, FCVT_H_WU, FMV_H_X,
  )
  private val scalarIsSew32 = scalarSew32.map(ctrl_flow.instr === _).reduce(_ || _)
  private val scalarIsSew16 = scalarSew16.map(ctrl_flow.instr === _).reduce(_ || _)

  private val isFmaNeedVd = FuType.isVecOPFFma(decodedInst.fuType) & (decodedInst.fuOpType(3, 0) =/= VfmaOpCode.vfmul)

  decodedInst.wfflags := wfflagsInsts.map(_ === inst.ALL).reduce(_ || _)
  decodedInst.needFrm.scalaNeedFrm := scalaNeedFrmInsts.map(_ === inst.ALL).reduce(_ || _)
  decodedInst.needFrm.vectorNeedFrm := vectorNeedFrmInsts.map(_ === inst.ALL).reduce(_ || _)
  decodedInst.vpu := 0.U.asTypeOf(decodedInst.vpu) // Todo: Connect vpu decoder
  decodedInst.vpu.vill := io.enq.vtype.illegal
  decodedInst.vpu.vma := io.enq.vtype.vma
  decodedInst.vpu.vta := io.enq.vtype.vta
  decodedInst.vpu.vsew := io.enq.vtype.vsew
  decodedInst.vpu.vlmul := io.enq.vtype.vlmul
  decodedInst.vpu.vm := inst.VM
  decodedInst.vpu.nf := inst.NF
  decodedInst.vpu.veew := inst.WIDTH
  decodedInst.vpu.isReverse := needReverseInsts.map(_ === inst.ALL).reduce(_ || _)
  decodedInst.vpu.isExt := vextInsts.map(_ === inst.ALL).reduce(_ || _)
  val isNarrow = narrowInsts.map(_ === inst.ALL).reduce(_ || _)
  val isDstMask = maskDstInsts.map(_ === inst.ALL).reduce(_ || _)
  val isOpMask = maskOpInsts.map(_ === inst.ALL).reduce(_ || _)
  val isVload = FuType.isVLoad(decodedInst.fuType)
  val isVlx = isVload && (decodedInst.fuOpType === VlduType.vloxe || decodedInst.fuOpType === VlduType.vluxe)
  val isVle = isVload && (decodedInst.fuOpType === VlduType.vle || decodedInst.fuOpType === VlduType.vleff || decodedInst.fuOpType === VlduType.vlse)
  val isVlm = isVload && (decodedInst.fuOpType === VlduType.vlm)
  val isVfNarrow = vectorFloatNarrow.map(_ === inst.ALL).reduce(_ || _)
  val isFof = isVload && (decodedInst.fuOpType === VlduType.vleff)
  val isWritePartVd = decodedInst.uopSplitType === UopSplitType.VEC_VRED || decodedInst.uopSplitType === UopSplitType.VEC_0XV || decodedInst.uopSplitType === UopSplitType.VEC_VWW
  val isVma = vmaInsts.map(_ === inst.ALL).reduce(_ || _)
  val positiveEmulIsFrac = decodedInst.vpu.vlmul +& decodedInst.vpu.veew < decodedInst.vpu.vsew
  val negativeEmulIsFrac = Cat(~decodedInst.vpu.vlmul(2), decodedInst.vpu.vlmul(1, 0)) +& decodedInst.vpu.veew < 3.U +& decodedInst.vpu.vsew
  val emulIsFrac = Mux(decodedInst.vpu.vlmul(2), negativeEmulIsFrac, positiveEmulIsFrac)
  decodedInst.vpu.isNarrow := isNarrow
  decodedInst.vpu.isDstMask := isDstMask
  decodedInst.vpu.isOpMask := isOpMask
  decodedInst.vpu.isDependOldVd := isVppu || (isVfNarrow || isFmaNeedVd) || isVStore || (isDstMask && !isOpMask) || isNarrow || isVlx || isVma || isFof
  decodedInst.vpu.isWritePartVd := isWritePartVd || isVlm || isVle && emulIsFrac
  decodedInst.vpu.vstart := io.enq.vstart
  decodedInst.vpu.isVleff := isFof && inst.NF === 0.U
  decodedInst.vpu.specVill := io.enq.vtype.illegal
  decodedInst.vpu.specVma := io.enq.vtype.vma
  decodedInst.vpu.specVta := io.enq.vtype.vta
  decodedInst.vpu.specVsew := io.enq.vtype.vsew
  decodedInst.vpu.specVlmul := io.enq.vtype.vlmul

  decodedInst.vlsInstr := isVls

  decodedInst.srcType(3) := Mux(inst.VM === 0.U, SrcType.vp, SrcType.DC) // mask src
  decodedInst.vlRen := true.B

  decodedInst.fpu.fmt := Mux(scalarIsSew32, VSew.e32, Mux(scalarIsSew16, VSew.e16, VSew.e64))
  decodedInst.fpu.wflags := decodedInst.wfflags
  decodedInst.fpu.rm := inst.RM

  val uopInfoGen = Module(new UopInfoGen)
  uopInfoGen.io.in.preInfo.isVecArith := inst.isVecArith
  uopInfoGen.io.in.preInfo.isVecMem := inst.isVecStore || inst.isVecLoad
  uopInfoGen.io.in.preInfo.isAmoCAS := inst.isAMOCAS

  uopInfoGen.io.in.preInfo.typeOfSplit := decodedInst.uopSplitType
  uopInfoGen.io.in.preInfo.vsew := decodedInst.vpu.vsew
  uopInfoGen.io.in.preInfo.vlmul := decodedInst.vpu.vlmul
  uopInfoGen.io.in.preInfo.vwidth := inst.RM
  uopInfoGen.io.in.preInfo.vmvn := inst.IMM5_OPIVI(2, 0)
  uopInfoGen.io.in.preInfo.nf := inst.NF
  uopInfoGen.io.in.preInfo.isVlsr := decodedInst.fuOpType === VlduType.vlr || decodedInst.fuOpType === VstuType.vsr
  uopInfoGen.io.in.preInfo.isVlsm := decodedInst.fuOpType === VlduType.vlm || decodedInst.fuOpType === VstuType.vsm
  io.deq.isComplex := uopInfoGen.io.out.isComplex
  io.deq.uopInfo.numOfWB := uopInfoGen.io.out.uopInfo.numOfWB
  io.deq.uopInfo.lmul := uopInfoGen.io.out.uopInfo.lmul

  // decode for SoftPrefetch instructions (prefetch.w / prefetch.r / prefetch.i)
  val isSoftPrefetch = inst.OPCODE === BitPat("b0010011") && inst.FUNCT3 === BitPat("b110") && inst.RD === 0.U
  val isPreW = isSoftPrefetch && inst.RS2 === 3.U(5.W)
  val isPreR = isSoftPrefetch && inst.RS2 === 1.U(5.W)
  val isPreI = isSoftPrefetch && inst.RS2 === 0.U(5.W)

  // for fli.s|fli.d instruction
  val isFLI = inst.FUNCT7 === BitPat("b11110??") && inst.RS2 === 1.U && inst.RM === 0.U && inst.OPCODE5Bit === OPCODE5Bit.OP_FP

  when (isCsrrVl) {
    // convert to vsetvl instruction
    decodedInst.srcType(0) := SrcType.no
    decodedInst.srcType(1) := SrcType.no
    decodedInst.srcType(2) := SrcType.no
    decodedInst.srcType(3) := SrcType.no
    decodedInst.vlRen := true.B
    decodedInst.waitForward   := false.B
    decodedInst.blockBackward := false.B
  }.elsewhen (isCsrrVlenb) {
    // convert to addi instruction
    decodedInst.srcType(0) := SrcType.reg
    decodedInst.srcType(1) := SrcType.imm
    decodedInst.srcType(2) := SrcType.no
    decodedInst.srcType(3) := SrcType.no
    decodedInst.vlRen := false.B
    decodedInst.selImm := SelImm.IMM_I
    decodedInst.waitForward := false.B
    decodedInst.blockBackward := false.B
    decodedInst.canRobCompress := true.B
  }.elsewhen (isPreW || isPreR || isPreI) {
    decodedInst.selImm := SelImm.IMM_S
    decodedInst.fuType := FuType.ldu.U
    decodedInst.canRobCompress := false.B
  }.elsewhen (isZimop) {
    // set srcType for zimop
    decodedInst.srcType(0) := SrcType.reg
    decodedInst.srcType(1) := SrcType.imm
    // use x0 as src1
    decodedInst.lsrc(0) := 0.U
  }

  io.deq.decodedInst := decodedInst
  io.deq.decodedInst.rfWen := (decodedInst.ldest =/= 0.U) && decodedInst.rfWen
  io.deq.decodedInst.fuType := Mux1H(Seq(
    // keep condition
    (!FuType.FuTypeOrR(decodedInst.fuType, FuType.vldu, FuType.vstu) && !isCsrrVl && !isCsrrVlenb) -> decodedInst.fuType,
    (isCsrrVl) -> FuType.vsetfwf.U,
    (isCsrrVlenb) -> FuType.alu.U,

    // change vlsu to vseglsu when NF =/= 0.U
    ( FuType.FuTypeOrR(decodedInst.fuType, FuType.vldu, FuType.vstu) && inst.NF === 0.U || (inst.NF =/= 0.U && (inst.MOP === "b00".U && inst.SUMOP === "b01000".U))) -> decodedInst.fuType,
    // MOP === b00 && SUMOP === b01000: unit-stride whole register store
    // MOP =/= b00                    : strided and indexed store
    ( FuType.FuTypeOrR(decodedInst.fuType, FuType.vstu)              && inst.NF =/= 0.U && ((inst.MOP === "b00".U && inst.SUMOP =/= "b01000".U) || inst.MOP =/= "b00".U)) -> FuType.vsegstu.U,
    // MOP === b00 && LUMOP === b01000: unit-stride whole register load
    // MOP =/= b00                    : strided and indexed load
    ( FuType.FuTypeOrR(decodedInst.fuType, FuType.vldu)              && inst.NF =/= 0.U && ((inst.MOP === "b00".U && inst.LUMOP =/= "b01000".U) || inst.MOP =/= "b00".U)) -> FuType.vsegldu.U,
  ))
  io.deq.decodedInst.imm := MuxCase(decodedInst.imm, Seq(
    isCsrrVlenb -> (VLEN / 8).U,
    isZimop     -> 0.U,
  ))

  io.deq.decodedInst.fuOpType := MuxCase(decodedInst.fuOpType, Seq(
    isCsrrVl    -> VSETOpType.csrrvl,
    isCsrrVlenb -> ALUOpType.add,
    isFLI       -> Cat(1.U, inst.FMT, inst.RS1),
    (isPreW || isPreR || isPreI) -> Mux1H(Seq(
      isPreW -> LSUOpType.prefetch_w,
      isPreR -> LSUOpType.prefetch_r,
      isPreI -> LSUOpType.prefetch_i,
    )),
    (isCboInval && io.fromCSR.special.cboI2F) -> LSUOpType.cbo_flush,
  ))

  io.deq.decodedInst.canRobCompress := decodedInst.canRobCompress
  // TODO: simplify it!!!
  io.deq.decodedInst.simple :=
    io.deq.decodedInst.canRobCompress &&
    io.csrCtrl.high_density_rob_compression_enable &&
    !FuType.isLoadStore(io.deq.decodedInst.fuType) &&
    !FuType.isBJU(io.deq.decodedInst.fuType) &&
    !FuType.isAMO(io.deq.decodedInst.fuType) &&
    !FuType.isFence(io.deq.decodedInst.fuType) &&
    !FuType.isCsr(io.deq.decodedInst.fuType) &&
    !FuType.isVset(io.deq.decodedInst.fuType) &&
    !FuType.isVArithMem(io.deq.decodedInst.fuType)

  //-------------------------------------------------------------
  // Debug Info
//  XSDebug("in:  instr=%x pc=%x excepVec=%b crossPageIPFFix=%d\n",
//    io.enq.ctrl_flow.instr, io.enq.ctrl_flow.pc, io.enq.ctrl_flow.exceptionVec.asUInt,
//    io.enq.ctrl_flow.crossPageIPFFix)
//  XSDebug("out: srcType(0)=%b srcType(1)=%b srcType(2)=%b lsrc(0)=%d lsrc(1)=%d lsrc(2)=%d ldest=%d fuType=%b fuOpType=%b\n",
//    io.deq.cf_ctrl.ctrl.srcType(0), io.deq.cf_ctrl.ctrl.srcType(1), io.deq.cf_ctrl.ctrl.srcType(2),
//    io.deq.cf_ctrl.ctrl.lsrc(0), io.deq.cf_ctrl.ctrl.lsrc(1), io.deq.cf_ctrl.ctrl.lsrc(2),
//    io.deq.cf_ctrl.ctrl.ldest, io.deq.cf_ctrl.ctrl.fuType, io.deq.cf_ctrl.ctrl.fuOpType)
//  XSDebug("out: rfWen=%d fpWen=%d isXSTrap=%d noSpecExec=%d isBlocked=%d flushPipe=%d imm=%x\n",
//    io.deq.cf_ctrl.ctrl.rfWen, io.deq.cf_ctrl.ctrl.fpWen, io.deq.cf_ctrl.ctrl.isXSTrap,
//    io.deq.cf_ctrl.ctrl.noSpecExec, io.deq.cf_ctrl.ctrl.blockBackward, io.deq.cf_ctrl.ctrl.flushPipe,
//    io.deq.cf_ctrl.ctrl.imm)
//  XSDebug("out: excepVec=%b\n", io.deq.cf_ctrl.cf.exceptionVec.asUInt)
}
