package xiangshan.backend.fu.NewCSR

import chisel3._
import freechips.rocketchip.rocket.CSRs
import org.chipsalliance.cde.config.Parameters
import xiangshan.{DistributedCSRIO, XSModule}
import xiangshan.backend.fu.NewCSR.CSRConfig._

class PFEvent(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val distribute_csr = Flipped(new DistributedCSRIO())
    val hpmevent = Output(Vec(perfCntNum, UInt(XLEN.W)))
  })

  val w = io.distribute_csr.w

  val perfEvents: Seq[CSRModule[_]] = (0 until perfCntNum).map(num =>
    Module(new CSRModule(s"perfEvents", new MhpmeventBundle))
      .setAddr(CSRs.mhpmevent3 + num)
  )

  perfEvents.zip(io.hpmevent).map{case(perf, out) => {
    perf.w.wen   := w.valid && (w.bits.addr === perf.addr.U)
    perf.w.wdata := w.bits.data
    out := perf.rdata
  }}
}