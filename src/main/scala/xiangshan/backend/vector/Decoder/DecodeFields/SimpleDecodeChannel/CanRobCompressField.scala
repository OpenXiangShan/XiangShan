package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import freechips.rocketchip.rocket.CustomInstructions.MNRET
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.decode.isa.PseudoInstructions.PAUSE
import xiangshan.backend.vector.Decoder.InstPattern.{InstPattern, VecInstPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object CanRobCompressField extends BoolDecodeField[InstPattern] {
  override def name: String = "canRobCompress"

  override def genTable(inst: InstPattern): BitPat = {
    inst match {
      // all vector instruction can not apply to rob compression
      case _: VecInstPattern => n
      case _ =>
        if (falseInsts.contains(inst.name))
          n
        else
          y
    }

  }

  // LPAD 阅读：这是让 canRobCompress 字段取 0 的“指令名”集合，AUIPC 在其中。
  // 宏在生成译码表时取名字；运行时硬件没有字符串查询，也不据此选 Simple/Pseudo 通道。
  // 本分支用该位参与高密度 ROB 的 simple 分类；LPAD 独立条目另由 CompressUnit.changesELP 保证。
  val falseInsts: Set[String] = getVariableNameSeq(
    LB, LBU, LH, LHU, LW, LWU, LD,
    SB, SH, SW, SD,
    AUIPC,
    JAL, JALR,
    BEQ, BNE, BGE, BGEU, BLT, BLTU,
    CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI,
    EBREAK, ECALL,
    SRET, MRET, MNRET, DRET, WFI,
    SFENCE_VMA, FENCE_I, FENCE, MFENCE, PAUSE,
    WRS_NTO, WRS_STO,
    AMOADD_D, AMOXOR_D, AMOSWAP_D, AMOAND_D, AMOOR_D, AMOMIN_D, AMOMAX_D, AMOMINU_D, AMOMAXU_D, AMOCAS_D,
    AMOADD_W, AMOXOR_W, AMOSWAP_W, AMOAND_W, AMOOR_W, AMOMIN_W, AMOMAX_W, AMOMINU_W, AMOMAXU_W, AMOCAS_W,
    AMOCAS_Q,
    LR_D, SC_D,
    LR_W, SC_W,
    FLH, FLW, FLD,
    FSH, FSW, FSD,
    SINVAL_VMA,
    SFENCE_INVAL_IR, SFENCE_W_INVAL,
    CBO_ZERO, CBO_CLEAN, CBO_FLUSH, CBO_INVAL,
  ).toSet
}
