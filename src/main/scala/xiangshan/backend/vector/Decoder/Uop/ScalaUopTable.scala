package xiangshan.backend.vector.Decoder.Uop

import chisel3.util.BitPat
import xiangshan.backend.decode.opcode.Opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.AluOpcodes._
import xiangshan.backend.decode.opcode.Opcode.AmoOpcodes._
import xiangshan.backend.decode.opcode.Opcode.BkuOpcodes._
import xiangshan.backend.decode.opcode.Opcode.BruOpcodes._
import xiangshan.backend.decode.opcode.Opcode.CsrOpcodes._
import xiangshan.backend.decode.opcode.Opcode.DivOpcodes._
import xiangshan.backend.decode.opcode.Opcode.FCvtOpcodes._
import xiangshan.backend.decode.opcode.Opcode.FDivOpcodes._
import xiangshan.backend.decode.opcode.Opcode.FMacOpcodes._
import xiangshan.backend.decode.opcode.Opcode.FMiscOpcodes._
import xiangshan.backend.decode.opcode.Opcode.FenceOpcodes._
import xiangshan.backend.decode.opcode.Opcode.JmpOpcodes._
import xiangshan.backend.decode.opcode.Opcode.LduOpcodes._
import xiangshan.backend.decode.opcode.Opcode.MulOpcodes._
import xiangshan.backend.decode.opcode.Opcode.StuOpcodes._
import xiangshan.backend.decode.opcode.Opcode._

object ScalaUopTable {
  val tableI = {
    import xiangshan.backend.decode.isa.Instructions.{I64Type, IType}

    val tableI64Type = I64Type.mapOpcode(
      _.ADDIW -> addw,
      _.ADDW  -> addw,
      _.LD    -> ld,
      _.LWU   -> lwu,
      _.SD    -> sd,
      _.SLLI  -> sll,
      _.SLLIW -> sllw,
      _.SLLW  -> sllw,
      _.SRAI  -> sra,
      _.SRAIW -> sraw,
      _.SRAW  -> sraw,
      _.SRLI  -> srl,
      _.SRLIW -> srlw,
      _.SRLW  -> srlw,
      _.SUBW  -> subw,
    )

    val tableIType = IType.mapOpcode(
      _.ADD     -> add,
      _.ADDI    -> add,
      _.AND     -> and,
      _.ANDI    -> and,
      _.AUIPC   -> auipc,
      _.BEQ     -> beq,
      _.BGE     -> bge,
      _.BGEU    -> bgeu,
      _.BLT     -> blt,
      _.BLTU    -> bltu,
      _.BNE     -> bne,
      _.EBREAK  -> jmp,
      _.ECALL   -> jmp,
      _.FENCE   -> fence,
      _.JAL     -> jal,
      _.JALR    -> jalr,
      _.LB      -> lb,
      _.LBU     -> lbu,
      _.LH      -> lh,
      _.LHU     -> lhu,
      _.LUI     -> add,
      _.LW      -> lw,
      _.OR      -> or,
      _.ORI     -> or,
      _.SB      -> sb,
      _.SH      -> sh,
      _.SLL     -> sll,
      _.SLT     -> slt,
      _.SLTI    -> slt,
      _.SLTIU   -> sltu,
      _.SLTU    -> sltu,
      _.SRA     -> sra,
      _.SRL     -> srl,
      _.SUB     -> sub,
      _.SW      -> sw,
      _.XOR     -> xor,
      _.XORI    -> xor,
    )

    tableI64Type ++ tableIType
  }

  val tableM = {
    import xiangshan.backend.decode.isa.Instructions.{M64Type, MType}

    val tableM64Type = M64Type.mapOpcode(
      _.DIVUW -> divuw,
      _.DIVW  -> divw,
      _.MULW  -> mulw,
      _.REMUW -> remuw,
      _.REMW  -> remw,
    )

    val tableMType = MType.mapOpcode(
      _.DIV    -> div,
      _.DIVU   -> divu,
      _.MUL    -> mul,
      _.MULH   -> mulh,
      _.MULHSU -> mulhsu,
      _.MULHU  -> mulhu,
      _.REM    -> rem,
      _.REMU   -> remu,
    )

    tableM64Type ++ tableMType
  }

  val tableA = {
    import xiangshan.backend.decode.isa.Instructions.{A64Type, AType}

    val tableA64Type = A64Type.mapOpcode(
      _.AMOADD_D  -> amoadd_d ,
      _.AMOAND_D  -> amoand_d ,
      _.AMOMAX_D  -> amomax_d ,
      _.AMOMAXU_D -> amomaxu_d,
      _.AMOMIN_D  -> amomin_d ,
      _.AMOMINU_D -> amominu_d,
      _.AMOOR_D   -> amoor_d  ,
      _.AMOSWAP_D -> amoswap_d,
      _.AMOXOR_D  -> amoxor_d ,
      _.LR_D      -> lr_d     ,
      _.SC_D      -> sc_d     ,
    )

    val tableAType = AType.mapOpcode(
      _.AMOADD_W  -> amoadd_w ,
      _.AMOAND_W  -> amoand_w ,
      _.AMOMAX_W  -> amomax_w ,
      _.AMOMAXU_W -> amomaxu_w,
      _.AMOMIN_W  -> amomin_w ,
      _.AMOMINU_W -> amominu_w,
      _.AMOOR_W   -> amoor_w  ,
      _.AMOSWAP_W -> amoswap_w,
      _.AMOXOR_W  -> amoxor_w ,
      _.LR_W      -> lr_w     ,
      _.SC_W      -> sc_w     ,
    )

    tableA64Type ++ tableAType
  }

  val tableF = {
    import xiangshan.backend.decode.isa.Instructions.{F64Type, FType}

    val tableF64Type = F64Type.mapOpcode(
      _.FCVT_L_S  -> fcvt_si64_fp32,
      _.FCVT_LU_S -> fcvt_ui64_fp32,
      _.FCVT_S_L  -> I2fOpcodes.fcvt_fp32_si64,
      _.FCVT_S_LU -> I2fOpcodes.fcvt_fp32_ui64,
    )

    val tableFType = FType.mapOpcode(
      _.FADD_S    -> FAluOpcodes.fadd_fp32,
      _.FCLASS_S  -> fclass_fp32,
      _.FCVT_S_W  -> I2fOpcodes.fcvt_fp32_si32,
      _.FCVT_S_WU -> I2fOpcodes.fcvt_fp32_ui32,
      _.FCVT_W_S  -> fcvt_si32_fp32,
      _.FCVT_WU_S -> fcvt_ui32_fp32,
      _.FDIV_S    -> fdiv_fp32,
      _.FEQ_S     -> feq_fp32,
      _.FLE_S     -> fle_fp32,
      _.FLT_S     -> flt_fp32,
      _.FLW       -> lw,
      _.FMADD_S   -> fmadd_fp32,
      _.FMAX_S    -> FAluOpcodes.fmax_fp32,
      _.FMIN_S    -> FAluOpcodes.fmin_fp32,
      _.FMSUB_S   -> fmsub_fp32,
      _.FMUL_S    -> fmul_fp32,
      _.FMV_W_X   -> I2fOpcodes.fmv_fp32_i,
      _.FMV_X_W   -> fmv_i_fp32,
      _.FNMADD_S  -> fnmadd_fp32,
      _.FNMSUB_S  -> fnmsub_fp32,
      _.FSGNJ_S   -> FAluOpcodes.fsgnj_fp32,
      _.FSGNJN_S  -> FAluOpcodes.fsgnjn_fp32,
      _.FSGNJX_S  -> FAluOpcodes.fsgnjx_fp32,
      _.FSQRT_S   -> fsqrt_fp32,
      _.FSUB_S    -> FAluOpcodes.fsub_fp32,
      _.FSW       -> sw,
    )

    tableF64Type ++ tableFType
  }

  val tableD = {
    import xiangshan.backend.decode.isa.Instructions.{D64Type, DType}

    val tableD64Type = D64Type.mapOpcode(
      _.FCVT_D_L  -> I2fOpcodes.fcvt_fp64_si64,
      _.FCVT_D_LU -> I2fOpcodes.fcvt_fp64_ui64,
      _.FCVT_L_D  -> fcvt_si64_fp64,
      _.FCVT_LU_D -> fcvt_ui64_fp64,
      _.FMV_D_X   -> I2fOpcodes.fmv_fp64_i,
      _.FMV_X_D   -> fmv_i_fp64,
    )

    val tableDType = DType.mapOpcode(
      _.FADD_D    -> FAluOpcodes.fadd_fp64,
      _.FCLASS_D  -> fclass_fp64,
      _.FCVT_D_S  -> fcvt_fp64_fp32,
      _.FCVT_D_W  -> I2fOpcodes.fcvt_fp64_si32,
      _.FCVT_D_WU -> I2fOpcodes.fcvt_fp64_ui32,
      _.FCVT_S_D  -> fcvt_fp32_fp64,
      _.FCVT_W_D  -> fcvt_si32_fp64,
      _.FCVT_WU_D -> fcvt_ui32_fp64,
      _.FDIV_D    -> fdiv_fp64,
      _.FEQ_D     -> feq_fp64,
      _.FLE_D     -> fle_fp64,
      _.FLT_D     -> flt_fp64,
      _.FLD       -> ld,
      _.FMADD_D   -> fmadd_fp64,
      _.FMAX_D    -> FAluOpcodes.fmax_fp64,
      _.FMIN_D    -> FAluOpcodes.fmin_fp64,
      _.FMSUB_D   -> fmsub_fp64,
      _.FMUL_D    -> fmul_fp64,
      _.FNMADD_D  -> fnmadd_fp64,
      _.FNMSUB_D  -> fnmsub_fp64,
      _.FSGNJ_D   -> FAluOpcodes.fsgnj_fp64,
      _.FSGNJN_D  -> FAluOpcodes.fsgnjn_fp64,
      _.FSGNJX_D  -> FAluOpcodes.fsgnjx_fp64,
      _.FSQRT_D   -> fsqrt_fp64,
      _.FSUB_D    -> FAluOpcodes.fsub_fp64,
      _.FSD       -> sd,
    )

    tableD64Type ++ tableDType
  }

  val tableZawrs = {
    xiangshan.backend.decode.isa.Instructions.ZAWRSType.mapOpcode(
      _.WRS_NTO -> wrs_nto,
      _.WRS_STO -> wrs_sto,
    )
  }

  val tableZba: Map[BitPat, Opcode] = {
    import xiangshan.backend.decode.isa.Instructions.{ZBA64Type, ZBAType}

    val tableZba64Type = ZBA64Type.mapOpcode(
      _.ADD_UW -> adduw,
      _.SH1ADD_UW -> sh1adduw,
      _.SH2ADD_UW -> sh2adduw,
      _.SH3ADD_UW -> sh3adduw,
      _.SLLI_UW -> slliuw,
    )
    val tableZbaType = ZBAType.mapOpcode(
      _.SH1ADD -> sh1add,
      _.SH2ADD -> sh2add,
      _.SH3ADD -> sh3add,
    )

    tableZba64Type ++ tableZbaType
  }

  val tableZbb = {
    import xiangshan.backend.decode.isa.Instructions.{ZBB64Type, ZBBType}

    val tableZbb64Type = ZBB64Type.mapOpcode(
      _.CLZW  -> clzw,
      _.CPOPW -> cpopw,
      _.CTZW  -> ctzw,
      _.REV8  -> rev8,
      _.ROLW  -> rolw,
      _.RORI  -> ror,
      _.RORIW -> rorw,
      _.RORW  -> rorw,
    )
    val tableZbbType = ZBBType.mapOpcode(
      _.ANDN   -> andn,
      _.CLZ    -> clz,
      _.CPOP   -> cpop,
      _.CTZ    -> ctz,
      _.MAX    -> max,
      _.MAXU   -> maxu,
      _.MIN    -> min,
      _.MINU   -> minu,
      _.ORC_B  -> orcb,
      _.ORN    -> orn,
      _.ROL    -> rol,
      _.ROR    -> ror,
      _.SEXT_B -> sextb,
      _.SEXT_H -> sexth,
      _.XNOR   -> xnor,
    )

    tableZbb64Type ++ tableZbbType
  }

  val tableZbc = {
    import xiangshan.backend.decode.isa.Instructions.ZBCType

    ZBCType.mapOpcode(
      _.CLMUL  -> clmul,
      _.CLMULH -> clmulh,
      _.CLMULR -> clmulr,
    )
  }

  val tableZbkb = {
    import xiangshan.backend.decode.isa.Instructions.ZBKB64Type
    import xiangshan.backend.decode.isa.Instructions.ZBKBType

    val tableZbkb64Type = ZBKB64Type.mapOpcode(
      _.PACKW -> packw,
      _.REV8  -> rev8,
      _.ROLW  -> rolw,
      _.RORI  -> ror,
      _.RORIW -> rorw,
      _.RORW  -> rorw,
    )

    val tableZbkbType = ZBKBType.mapOpcode(
      _.ANDN    -> andn,
      _.BREV8   -> revb,
      _.ORN     -> orn,
      _.PACK    -> pack,
      _.PACKH   -> packh,
      _.ROL     -> rol,
      _.ROR     -> ror,
      _.XNOR    -> xnor,
    )

    tableZbkb64Type ++ tableZbkbType
  }

  val tableZbkx = {
    xiangshan.backend.decode.isa.Instructions.ZBKXType.mapOpcode(
      _.XPERM4 -> xpermn,
      _.XPERM8 -> xpermb,
    )
  }

  val tableZbs = {
    import xiangshan.backend.decode.isa.Instructions.{ZBS64Type, ZBSType}

    val tableZbs64Type = ZBS64Type.mapOpcode(
      _.BCLRI -> bclr,
      _.BEXTI -> bext,
      _.BINVI -> binv,
      _.BSETI -> bset,
    )

    val tableZbsTable = ZBSType.mapOpcode(
      _.BCLR -> bclr,
      _.BEXT -> bext,
      _.BINV -> binv,
      _.BSET -> bset,
    )

    tableZbs64Type ++ tableZbsTable
  }

  val tableSystem = {
    import xiangshan.backend.decode.isa.Instructions.SYSTEMType

    SYSTEMType.mapOpcode(
      _.MRET -> jmp,
      _.WFI  -> wfi,
    )
  }

  val tableS = {
    import xiangshan.backend.decode.isa.Instructions.SType

    SType.mapOpcode(
      _.SFENCE_VMA -> sfence,
      _.SRET -> jmp,
    )
  }

  def tableH = ???

  val tableZabha = {
    import xiangshan.backend.decode.isa.Instructions.ZABHAType
    ZABHAType.mapOpcode(
      _.AMOADD_B  -> amoadd_b,
      _.AMOADD_H  -> amoadd_h,
      _.AMOAND_B  -> amoand_b,
      _.AMOAND_H  -> amoand_h,
      _.AMOMAX_B  -> amomax_b,
      _.AMOMAX_H  -> amomax_h,
      _.AMOMAXU_B -> amomaxu_b,
      _.AMOMAXU_H -> amomaxu_h,
      _.AMOMIN_B  -> amomin_b,
      _.AMOMIN_H  -> amomin_h,
      _.AMOMINU_B -> amominu_b,
      _.AMOMINU_H -> amominu_h,
      _.AMOOR_B   -> amoor_b,
      _.AMOOR_H   -> amoor_h,
      _.AMOSWAP_B -> amoswap_b,
      _.AMOSWAP_H -> amoswap_h,
      _.AMOXOR_B  -> amoxor_b,
      _.AMOXOR_H  -> amoxor_h,
    )
  }

  val tableZacas = {
    import xiangshan.backend.decode.isa.Instructions.{ZACAS64Type,ZACASType}

    val tableZacas64 = ZACAS64Type.mapOpcode(
      _.AMOCAS_Q -> amocas_q,
    )
    val tableZacas = ZACASType.mapOpcode(
      _.AMOCAS_D -> amocas_d,
      _.AMOCAS_W -> amocas_w,
    )

    tableZacas ++ tableZacas64
  }

  val tableZabhaZacas = {
    import xiangshan.backend.decode.isa.Instructions.ZABHA_ZACASType

    ZABHA_ZACASType.mapOpcode(
      _.AMOCAS_B -> amocas_b,
      _.AMOCAS_H -> amocas_h,
    )
  }

  val tableZfaF = {
    import xiangshan.backend.decode.isa.Instructions.F_ZFAType

    F_ZFAType.mapOpcode(
      _.FLEQ_S     -> fleq_fp32,
      _.FLI_S      -> fleq_fp32, // todo
      _.FLTQ_S     -> fltq_fp32,
      _.FMAXM_S    -> fmaxm_fp32,
      _.FMINM_S    -> fminm_fp32,
      _.FROUND_S   -> frnd_fp32,
      _.FROUNDNX_S -> frndnx_fp32,
    )
  }

  val tableZfaD = {
    import xiangshan.backend.decode.isa.Instructions.D_ZFAType

    D_ZFAType.mapOpcode(
      _.FLEQ_D      -> fleq_fp64,
      _.FLI_D       -> fleq_fp64, // todo
      _.FLTQ_D      -> fltq_fp64,
      _.FMAXM_D     -> fmaxm_fp64,
      _.FMINM_D     -> fminm_fp64,
      _.FROUND_D    -> frnd_fp64,
      _.FROUNDNX_D  -> frndnx_fp64,
      _.FCVTMOD_W_D -> fcvtmod_si32_fp64,
    )
  }

  val tableZfaZfh = {
    import xiangshan.backend.decode.isa.Instructions.ZFH_ZFAType

    ZFH_ZFAType.mapOpcode(
      _.FLEQ_H      -> fleq_fp16,
      _.FLI_H       -> fleq_fp16, // todo
      _.FLTQ_H      -> fltq_fp16,
      _.FMAXM_H     -> fmaxm_fp16,
      _.FMINM_H     -> fminm_fp16,
      _.FROUND_H    -> frnd_fp16,
      _.FROUNDNX_H  -> frndnx_fp16,
    )
  }
  val tableZfh = {
    import xiangshan.backend.decode.isa.Instructions.{ZFH64Type,ZFHType}

    val tableZFH64Type = ZFH64Type.mapOpcode(
      _.FCVT_H_L  -> I2fOpcodes.fcvt_fp16_si64,
      _.FCVT_H_LU -> I2fOpcodes.fcvt_fp16_ui64,
      _.FCVT_L_H  -> fcvt_si64_fp16,
      _.FCVT_LU_H -> fcvt_ui64_fp16,
    )

    val tableZFHType = ZFHType.mapOpcode(
      _.FADD_H    -> FAluOpcodes.fadd_fp16,
      _.FCLASS_H  -> fclass_fp16,
      _.FCVT_H_S  -> fcvt_fp16_fp32,
      _.FCVT_H_W  -> I2fOpcodes.fcvt_fp16_si32,
      _.FCVT_H_WU -> I2fOpcodes.fcvt_fp16_ui32,
      _.FCVT_S_H  -> fcvt_fp32_fp16,
      _.FCVT_W_H  -> fcvt_si32_fp16,
      _.FCVT_WU_H -> fcvt_ui32_fp16,
      _.FDIV_H    -> fdiv_fp16,
      _.FEQ_H     -> feq_fp16,
      _.FLE_H     -> fle_fp16,
      _.FLT_H     -> flt_fp16,
      _.FLH       -> lh,
      _.FMADD_H   -> fmadd_fp16,
      _.FMAX_H    -> FAluOpcodes.fmax_fp16,
      _.FMIN_H    -> FAluOpcodes.fmin_fp16,
      _.FMSUB_H   -> fmsub_fp16,
      _.FMUL_H    -> fmul_fp16,
      _.FMV_H_X   -> I2fOpcodes.fmv_fp16_i,
      _.FMV_X_H   -> fmv_i_fp16,
      _.FNMADD_H  -> fnmadd_fp16,
      _.FNMSUB_H  -> fnmsub_fp16,
      _.FSGNJ_H   -> FAluOpcodes.fsgnj_fp16,
      _.FSGNJN_H  -> FAluOpcodes.fsgnjn_fp16,
      _.FSGNJX_H  -> FAluOpcodes.fsgnjx_fp16,
      _.FSH       -> sh,
      _.FSQRT_H   -> fsqrt_fp16,
      _.FSUB_H    -> FAluOpcodes.fsub_fp16,
    )

    tableZFHType ++ tableZFH64Type
  }

  val tableZfhDType = {
    import xiangshan.backend.decode.isa.Instructions.D_ZFHType

    D_ZFHType.mapOpcode(
      _.FCVT_D_H -> fcvt_fp64_fp16,
      _.FCVT_H_D -> fcvt_fp16_fp64,
    )
  }

  val tableZfhmin = {
    import xiangshan.backend.decode.isa.Instructions.ZFHMINType

    ZFHMINType.mapOpcode(
      _.FCVT_S_H -> fcvt_fp32_fp16,
      _.FCVT_H_S -> fcvt_fp16_fp32,
      _.FLH      -> lh,
      _.FMV_H_X  -> I2fOpcodes.fmv_fp16_i,
      _.FMV_X_H  -> fmv_i_fp16,
      _.FSH      -> sh,
    )
  }

  val tableZfhminD = {
    import xiangshan.backend.decode.isa.Instructions.D_ZFHType

    D_ZFHType.mapOpcode(
      _.FCVT_D_H -> fcvt_fp64_fp16,
      _.FCVT_H_D -> fcvt_fp16_fp64,
    )
  }

  def tableZicfi = ???
  def tableZicond = ???

  val tableZicsr = {
    import xiangshan.backend.decode.isa.Instructions.ZICSRType
    ZICSRType.mapOpcode(
      _.CSRRC  -> clr,
      _.CSRRCI -> clri,
      _.CSRRS  -> set,
      _.CSRRSI -> seti,
      _.CSRRW  -> wrt,
      _.CSRRWI -> wrti,
    )
  }

  val tableZifencei = {
    import xiangshan.backend.decode.isa.Instructions.ZIFENCEIType
    ZIFENCEIType.mapOpcode(
      _.FENCE_I -> fencei,
    )
  }

  def tableZimop = ???

  val tableZknd = {
    import xiangshan.backend.decode.isa.Instructions.ZKND64Type
    ZKND64Type.mapOpcode(
      _.AES64DS   -> aes64ds,
      _.AES64DSM  -> aes64dsm,
      _.AES64IM   -> aes64im,
      _.AES64KS1I -> aes64ks1i,
      _.AES64KS2  -> aes64ks2,
    )
  }

  val tableZkne = {
    import xiangshan.backend.decode.isa.Instructions.ZKNE64Type
    ZKNE64Type.mapOpcode(
      _.AES64ES  -> aes64es,
      _.AES64ESM -> aes64esm,
      _.AES64KS1I -> aes64ks1i,
      _.AES64KS2  -> aes64ks2,
    )
  }

  val tableZknh = {
    import xiangshan.backend.decode.isa.Instructions.{ZKNH64Type, ZKNHType}

    val tableZKNH64Type = ZKNH64Type.mapOpcode(
      _.SHA512SIG0 -> sha512sig0,
      _.SHA512SIG1 -> sha512sig1,
      _.SHA512SUM0 -> sha512sum0,
      _.SHA512SUM1 -> sha512sum1,
    )
    val tableZKNHType = ZKNHType.mapOpcode(
      _.SHA256SIG0 -> sha256sig0,
      _.SHA256SIG1 -> sha256sig1,
      _.SHA256SUM0 -> sha256sum0,
      _.SHA256SUM1 -> sha256sum1,
    )

    tableZKNH64Type ++ tableZKNHType
  }

  val tableZksed = {
    import xiangshan.backend.decode.isa.Instructions.ZKSEDType
    ZKSEDType.mapOpcode(
      _.SM4ED0 -> sm4ed0,
      _.SM4ED1 -> sm4ed1,
      _.SM4ED2 -> sm4ed2,
      _.SM4ED3 -> sm4ed3,
      _.SM4KS0 -> sm4ks0,
      _.SM4KS1 -> sm4ks1,
      _.SM4KS2 -> sm4ks2,
      _.SM4KS3 -> sm4ks3,
    )
  }

  val tableZksh = {
    import xiangshan.backend.decode.isa.Instructions.ZKSHType
    ZKSHType.mapOpcode(
      _.SM3P0 -> sm3p0,
      _.SM3P1 -> sm3p1,
    )
  }

  def tableSExt = {
  }

  val tableXSTrap = Map(
    xiangshan.backend.decode.isa.CustomInstructions.TRAP -> xstrap
  )
}
