package xiangshan.backend.vector.Decoder.Uop

import chisel3.util.BitPat
import xiangshan.backend.decode.opcode.Opcode
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
      _.FCVT_S_L  -> fcvt_fp32_si64,
      _.FCVT_S_LU -> fcvt_fp32_ui64,
    )

    val tableFType = FType.mapOpcode(
      _.FADD_S    -> fadd_fp32,
      _.FCLASS_S  -> fclass_fp32,
      _.FCVT_S_W  -> fcvt_fp32_si32,
      _.FCVT_S_WU -> fcvt_fp32_ui32,
      _.FCVT_W_S  -> fcvt_si32_fp32,
      _.FCVT_WU_S -> fcvt_ui32_fp32,
      _.FDIV_S    -> fdiv_fp32,
      _.FEQ_S     -> feq_fp32,
      _.FLE_S     -> fle_fp32,
      _.FLT_S     -> flt_fp32,
      _.FLW       -> lw,
      _.FMADD_S   -> fmadd_fp32,
      _.FMAX_S    -> fmax_fp32,
      _.FMIN_S    -> fmin_fp32,
      _.FMSUB_S   -> fmsub_fp32,
      _.FMUL_S    -> fmul_fp32,
      _.FMV_W_X   -> fmv_fp32_i,
      _.FMV_X_W   -> fmv_i_fp32,
      _.FNMADD_S  -> fnmadd_fp32,
      _.FNMSUB_S  -> fnmsub_fp32,
      _.FSGNJ_S   -> fsgnj_fp32,
      _.FSGNJN_S  -> fsgnjn_fp32,
      _.FSGNJX_S  -> fsgnjx_fp32,
      _.FSQRT_S   -> fsqrt_fp32,
      _.FSUB_S    -> fsub_fp32,
      _.FSW       -> sw,
    )

    tableF64Type ++ tableFType
  }

  val tableD = {
    import xiangshan.backend.decode.isa.Instructions.{D64Type, DType}

    val tableD64Type = D64Type.mapOpcode(
      _.FCVT_D_L  -> fcvt_fp64_si64,
      _.FCVT_D_LU -> fcvt_fp64_ui64,
      _.FCVT_L_D  -> fcvt_si64_fp64,
      _.FCVT_LU_D -> fcvt_ui64_fp64,
      _.FMV_D_X   -> fmv_fp64_i,
      _.FMV_X_D   -> fmv_i_fp64,
    )

    val tableDType = DType.mapOpcode(
      _.FADD_D    -> fadd_fp64,
      _.FCLASS_D  -> fclass_fp64,
      _.FCVT_D_S  -> fcvt_fp64_fp32,
      _.FCVT_D_W  -> fcvt_fp64_si32,
      _.FCVT_D_WU -> fcvt_fp64_ui32,
      _.FCVT_S_D  -> fcvt_fp32_fp64,
      _.FCVT_W_D  -> fcvt_si32_fp64,
      _.FCVT_WU_D -> fcvt_ui32_fp64,
      _.FDIV_D    -> fdiv_fp64,
      _.FEQ_D     -> feq_fp64,
      _.FLE_D     -> fle_fp64,
      _.FLT_D     -> flt_fp64,
      _.FLD       -> ld,
      _.FMADD_D   -> fmadd_fp64,
      _.FMAX_D    -> fmax_fp64,
      _.FMIN_D    -> fmin_fp64,
      _.FMSUB_D   -> fmsub_fp64,
      _.FMUL_D    -> fmul_fp64,
      _.FNMADD_D  -> fnmadd_fp64,
      _.FNMSUB_D  -> fnmsub_fp64,
      _.FSGNJ_D   -> fsgnj_fp64,
      _.FSGNJN_D  -> fsgnjn_fp64,
      _.FSGNJX_D  -> fsgnjx_fp64,
      _.FSQRT_D   -> fsqrt_fp64,
      _.FSUB_D    -> fsub_fp64,
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
  def tableZabha = ???
  def tableZacas = ???
  def tableZabhaZacas = ???
  def tableZfaF = ???
  def tableZfaD = ???
  def tableZfaZfh = ???
  def tableZfh = ???
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

  def tableZifencei = ???
  def tableZimop = ???
  def tableZknd = ???
  def tableZkne = ???
  def tableZknh = ???
  def tableZksed = ???
  def tableZsh = ???
  def tableSExt = {
  }

  val tableXSTrap = Map(
    xiangshan.backend.decode.isa.CustomInstructions.TRAP -> xstrap
  )
}
