package xiangshan.backend.fu.NewCSR

import xiangshan.backend.decode.isa.CSRs

object CSROoORead {
  /**
   * "Read only" CSRs that can be fully pipelined when read in CSRR instruction.
   * Only read by csr instructions.
   */
  val waitForwardInOrderCsrReadList = List(
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
  )
  val blockBackwardInOrderCsrReadList = List(
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
}
