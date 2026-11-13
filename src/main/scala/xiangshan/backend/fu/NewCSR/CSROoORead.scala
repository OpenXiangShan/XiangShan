package xiangshan.backend.fu.NewCSR

import xiangshan.backend.decode.isa.CSRs

object CSROoORead {
  /**
   * "Read only" CSRs that can be fully pipelined when read in CSRR instruction.
   * Only read by csr instructions.
   *
   * Note: SHOULD NOT CONTAIN VL and VLENB, since they have other decode results
   */
  val waitForwardCsrReadList = List(
    CSRs.fflags,
    CSRs.fcsr,
    CSRs.vxsat,
    CSRs.vcsr,
    CSRs.vstart,
    CSRs.sstatus,
    CSRs.vsstatus,
    CSRs.mstatus,
    CSRs.hstatus,
    CSRs.mnstatus,
    CSRs.dcsr,
    CSRs.vtype,
    CSRs.mireg,
    CSRs.sireg,
    CSRs.vsireg,
    CSRs.mtopi,
    CSRs.stopi,
    CSRs.vstopi,
    CSRs.mtopei,
    CSRs.stopei,
    CSRs.vstopei,
    CSRs.mip,
    CSRs.sip,
    CSRs.vsip,
    CSRs.hip,
    CSRs.hvip,
    CSRs.mvip,
    // Counters change on their own: read out of order, a minstret read behind a divide chain returned 1 instead of
    // at least 6 (a read must count every older instruction). Keep them in order (wait for older instructions).
    CSRs.minstret,
    CSRs.instret,
    CSRs.mcycle,
    CSRs.cycle,
    CSRs.time,
  ) ++
    (CSRs.mhpmcounter3 to CSRs.mhpmcounter31) ++
    (CSRs.hpmcounter3 to CSRs.hpmcounter31)
  val blockBackwardCsrReadList = List(
    CSRs.mireg,
    CSRs.sireg,
    CSRs.vsireg,
    CSRs.mtopi,
    CSRs.stopi,
    CSRs.vstopi,
    CSRs.mtopei,
    CSRs.stopei,
    CSRs.vstopei,
    CSRs.mip,
    CSRs.sip,
    CSRs.vsip,
    CSRs.hip,
    CSRs.hvip,
    CSRs.mvip,
  )
  def fullCSRList: List[Int] = (0 until 0x1000).toList
}
