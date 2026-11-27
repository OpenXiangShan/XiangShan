package xiangshan.backend.ctrlblock

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.Redirect
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.WriteBackRobBundle
import xiangshan.backend.exu.ExeUnitParams

/**
  * Single-output shift queue for writebacks with a redirect field.
  *
  * - A writeback with a valid redirect enters the first stage. Entries advance
  *   one stage per cycle, and the last stage has priority on the output.
  *
  * - A writeback without a valid redirect bypasses when the last stage is empty.
  *   When the last stage drives the output, the incoming writeback enters the queue.
  *
  * - Redirects cancel writebacks entering the first stage or advancing between
  *   stages. The output uses the valid bit of the selected writeback.
  */
class WritebackWithRedirectDelayQueue(
  wbParams: ExeUnitParams,
  backendParams: BackendParams,
  delay: Int
)(implicit p: Parameters) extends Module {
  require(wbParams.hasRedirect, "Writeback delay queue requires a redirect field")
  require(delay > 0, "The shift queue requires at least one stage")

  val io = IO(new Bundle {
    val in = Flipped(Valid(new WriteBackRobBundle(wbParams, backendParams)))
    val out = Valid(new WriteBackRobBundle(wbParams, backendParams))
    val redirects = Input(Vec(2, Valid(new Redirect)))
  })

  private val queue = Reg(Vec(delay, chiselTypeOf(io.in.bits)))
  private val queueValid = RegInit(VecInit(Seq.fill(delay)(false.B)))
  private val redirects = io.redirects.toSeq
  private val tailValid = queueValid.last
  private val inHasRedirect = io.in.bits.redirect.get.valid
  private val enq = io.in.valid && (inHasRedirect || tailValid) && !io.in.bits.robIdx.needFlush(redirects)

  queueValid(0) := enq
  when(enq) {
    queue(0) := io.in.bits
  }
  for (i <- 1 until delay) {
    val advance = queueValid(i - 1) && !queue(i - 1).robIdx.needFlush(redirects)
    queueValid(i) := advance
    when(advance) {
      queue(i) := queue(i - 1)
    }
  }

  io.out.valid := tailValid || (io.in.valid && !inHasRedirect)
  io.out.bits := Mux(tailValid, queue.last, io.in.bits)
}
