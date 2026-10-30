package xiangshan

// Static producer selection; the existing spfctl CSR remains the runtime gate.
case class OceanusPrefetchParameters(
  bop: Boolean = false,
  recv: Boolean = false,
  vbop: Boolean = false,
  cdp: Boolean = false
) {
  def tlb: Boolean = vbop || cdp
  def enabled: Boolean = bop || recv || vbop || cdp
}
