package xiangshan.backend.decode.isa

import chisel3.util._

object PseudoInstructions {
  // no write csr
  //                                    csr     | rs1 | funct3 |  rd | opcode
  def CSRRS_RO           = BitPat("b????????????_00000____?10___?????_1110011")
  def CSRRC_RO           = BitPat("b????????????_00000____?11___?????_1110011")

  // no read csr
  def CSRRW_WO           = BitPat("b????????????_xxxxx____?01___?????_1110011")
}

object CSRReadOnlyBlockInstructions {
  def hexToPaddedBinary(hex: Int): String = {
    "b" + String.format("%12s", hex.toBinaryString).replace(' ', '0')
  }
  // csr read only inorder
  //                                               csrIdx        | rs1 | funct3 | rd | opcode |
  def CSRs_readOnly = BitPat("b????????????"                  + "_00000___?1?___?????_1110011")
  def CSRs_fflags   = BitPat(hexToPaddedBinary(CSRs.fflags  ) + "_00000___?1?___?????_1110011")
  def CSRs_fcsr     = BitPat(hexToPaddedBinary(CSRs.fcsr    ) + "_00000___?1?___?????_1110011")
  def CSRs_vxsat    = BitPat(hexToPaddedBinary(CSRs.vxsat   ) + "_00000___?1?___?????_1110011")
  def CSRs_vcsr     = BitPat(hexToPaddedBinary(CSRs.vcsr    ) + "_00000___?1?___?????_1110011")
  def CSRs_vstart   = BitPat(hexToPaddedBinary(CSRs.vstart  ) + "_00000___?1?___?????_1110011")
  def CSRs_sstatus  = BitPat(hexToPaddedBinary(CSRs.sstatus ) + "_00000___?1?___?????_1110011")
  def CSRs_vsstatus = BitPat(hexToPaddedBinary(CSRs.vsstatus) + "_00000___?1?___?????_1110011")
  def CSRs_mstatus  = BitPat(hexToPaddedBinary(CSRs.mstatus ) + "_00000___?1?___?????_1110011")
  def CSRs_hstatus  = BitPat(hexToPaddedBinary(CSRs.hstatus ) + "_00000___?1?___?????_1110011")
  def CSRs_mnstatus = BitPat(hexToPaddedBinary(CSRs.mnstatus) + "_00000___?1?___?????_1110011")
  def CSRs_dcsr     = BitPat(hexToPaddedBinary(CSRs.dcsr    ) + "_00000___?1?___?????_1110011")
  def CSRs_vtype    = BitPat(hexToPaddedBinary(CSRs.vtype   ) + "_00000___?1?___?????_1110011")
  def CSRs_mireg    = BitPat(hexToPaddedBinary(CSRs.mireg   ) + "_00000___?1?___?????_1110011") // blockBackward
  def CSRs_sireg    = BitPat(hexToPaddedBinary(CSRs.sireg   ) + "_00000___?1?___?????_1110011") // blockBackward
  def CSRs_vsireg   = BitPat(hexToPaddedBinary(CSRs.vsireg  ) + "_00000___?1?___?????_1110011") // blockBackward
  def CSRs_mtopi    = BitPat(hexToPaddedBinary(CSRs.mtopi   ) + "_00000___?1?___?????_1110011") // blockBackward
  def CSRs_stopi    = BitPat(hexToPaddedBinary(CSRs.stopi   ) + "_00000___?1?___?????_1110011") // blockBackward
  def CSRs_vstopi   = BitPat(hexToPaddedBinary(CSRs.vstopi  ) + "_00000___?1?___?????_1110011") // blockBackward
  def CSRs_mtopei   = BitPat(hexToPaddedBinary(CSRs.mtopei  ) + "_00000___?1?___?????_1110011") // blockBackward
  def CSRs_stopei   = BitPat(hexToPaddedBinary(CSRs.stopei  ) + "_00000___?1?___?????_1110011") // blockBackward
  def CSRs_vstopei  = BitPat(hexToPaddedBinary(CSRs.vstopei ) + "_00000___?1?___?????_1110011") // blockBackward
}