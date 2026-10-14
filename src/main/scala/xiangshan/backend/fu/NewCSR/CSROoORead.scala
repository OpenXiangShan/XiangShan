package xiangshan.backend.fu.NewCSR

import xiangshan.backend.decode.isa.CSRs

object CSROoORead {
  /**
   * Deny-lists: csrr of a CSR on a list keeps waitForward / blockBackward, every other CSR is read out of order.
   * List any CSR that a non-CSR instruction or hardware can change (e.g. ssp, open PR #6716) or whose read has side effects.
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
    // Set by hardware, and synchronised to the difftest reference when they change
    CSRs.hgeip,
    CSRs.scountovf,
    CSRs.sireg2, // mhpmevent through Smcdeleg
    0xBC1, // mflushpwr (custom): L2_FLUSH_DONE
    // No effect: csrrs rd, vl, x0 is pseudo-decoded to readvl (never serialised); other read forms see the unit's vl, always 0
    CSRs.vl,
  ) ++ (CSRs.mhpmevent3 to CSRs.mhpmevent31) ++ // OF is set by hardware
    // Counters change on their own: read in order, they count every older instruction and never go backwards
    (CSRs.mcycle to CSRs.mhpmcounter31) ++
    (CSRs.cycle to CSRs.hpmcounter31)
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
    CSRs.hgeip,
  )
}
