package xiangshan.backend.fu.NewCSR

import xiangshan.backend.decode.isa.CSRs

/** CSR-read ordering policy catalog. These lists classify instruction reads; they
  * neither enforce ordering themselves nor imply that architectural registers are RO.
  */
object CSROoORead {
  /**
   * Reads that must observe older in-flight architectural updates before execution.
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
  // Interrupt/indirect views also prevent younger operations from passing the read.
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
