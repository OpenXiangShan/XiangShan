package xiangshan.mem.cache

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import xiangshan.mem.{DCacheReq, DCacheResp, LSUDMemIO}
import xiangshan.utils.XSDebug
import bus.tilelink._
import _root_.utils.{Code, RandomReplacement, Transpose}
import xiangshan.mem.MemoryOpConstants


class StorePipe extends DCacheModule
{
  val io = IO(new DCacheBundle{
    val lsu   = Flipped(new LSUDMemIO)
    val data_write  = Output(Valid(new L1DataWriteReq))
    val data_resp  = Output(Vec(nWays, Vec(refillCycles, Bits(encRowBits.W))))
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_resp = Output(Vec(nWays, rstVal.cloneType))
  })


  // LSU requests
  io.lsu.req.ready := io.meta_read.ready
  io.meta_read.bits.valid := io.lsu.req.valid 

  val meta_read = io.meta_read.bits
  val data_read = io.data_read.bits
  for (w <- 0 until memWidth) {
    // Tag read for new requests
    meta_read.idx    := io.lsu.req.bits(w).bits.addr >> blockOffBits
    meta_read.way_en := ~0.U(nWays.W)
    meta_read.tag    := DontCare
  }

  // Pipeline
  // stage 0
  val s0_valid = io.lsu.req.fire()
  val s0_req = io.lsu.req.bits

  assert(!(s0_valid && s0_req.cmd =/= MemoryOpConstants.M_XWR), "StorePipe only accepts store req")

  dump_pipeline_reqs("StorePipe s0", s0_valid, s0_req, s0_type)

  // stage 1
  val s1_req = RegNext(s0_req)
  val s1_valid = RegNext(s0_valid, init = false.B)
  val s1_addr = s1_req.addr
  val s1_nack = false.B 

  dump_pipeline_reqs("StorePipe s1", s1_valid, s1_req, s1_type)

  // tag check
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta_resp(w).tag === (s1_addr >> untagBits)).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(i)(w) && meta(i).io.resp(w).coh.isValid()).asUInt


  // stage 2
  val s2_req   = RegNext(s1_req)
  val s2_valid = RegNext(s1_valid(w), init = false.B))

  dump_pipeline_reqs("StorePipe s2", s2_valid, s2_req, s2_type)

  val s2_tag_match_way = RegNext(s1_tag_match_way)
  val s2_tag_match     = s2_tag_match_way.orR
  val s2_hit_state     = Mux1H(s2_tag_match_way(i), wayMap((w: Int) => RegNext(meta_resp(w).coh)))
  val s2_has_permission = s2_hit_state.onAccess(s2_req.cmd)._1
  val s2_new_hit_state  = s2_hit_state.onAccess(s2_req.cmd)._3

  // we not only need permissions
  // we also require that state does not change on hit
  // thus we require new_hit_state === old_hit_state
  //
  // If state changes on hit,
  // we should treat it as not hit, and let mshr deal with it,
  // since we can not write meta data on the main pipeline.
  // It's possible that we had permission but state changes on hit:
  // eg: write to exclusive but clean block
  val s2_hit = s2_tag_match && s2_has_permission && s2_hit_state === s2_new_hit_state && !mshrs.io.block_hit
  val s2_nack = Wire(Bool())

  val s2_nack_hit    = RegNext(s1_nack)
  // Can't allocate MSHR for same set currently being written back
  // the same set is busy
  val s2_nack_set_busy  = s2_valid && mshrs.io.block_hit

  s2_nack           := s2_nack_hit || s2_nack_set_busy

  // write dcache if hit
  io.meta_write.valid := s2_valid && s2_hit
  io.meta_write.bits  := s2_req

  dump_pipeline_valids("StorePipe s2", "s2_hit", s2_hit)
  dump_pipeline_valids("StorePipe s2", "s2_nack", s2_nack)
  dump_pipeline_valids("StorePipe s2", "s2_nack_hit", s2_nack_hit)
  dump_pipeline_valids("StorePipe s2", "s2_nack_set_busy", s2_nack_set_busy)

  val resp = Wire(Valid(new DCacheResp))
  for (w <- 0 until memWidth) {
    resp.valid         := s2_valid
    resp.bits.data     := DontCare
    resp.bits.meta     := s2_req.meta
    resp.bits.nack     := s2_nack
  }

  io.lsu.resp(w) <> resp(w)

  when (resp.valid) {
    XSDebug(s"StorePipe resp: meta: %d nack: %b\n",
      resp.meta, resp.nack)
  }

  // -------
  // Debug logging functions
  def dump_pipeline_reqs(pipeline_stage_name: String, valid: Vec[Bool],
    reqs: Vec[DCacheReq], req_type: UInt) = {
      val anyValid = valid.reduce(_||_)
      when (anyValid) {
        (0 until memWidth) map { w =>
          when (valid(w)) {
            XSDebug(s"$pipeline_stage_name\n")
            XSDebug("channel %d: valid: %b \n", w.U, valid(w))
            when (req_type === t_replay) {
              XSDebug("req_type: replay ")
            } .elsewhen (req_type === t_lsu) {
              XSDebug("req_type: lsu ")
            } .otherwise {
              XSDebug("req_type: unknown ")
            }
            XSDebug("cmd: %x addr: %x data: %x mask: %x meta: %x\n",
              reqs(w).cmd, reqs(w).addr, reqs(w).data, reqs(w).mask, reqs(w).meta)
          }
        }
      }
  }

  def dump_pipeline_valids(pipeline_stage_name: String, signal_name: String, valid: Vec[Bool]) = {
    val anyValid = valid.reduce(_||_)
    when (anyValid) {
      (0 until memWidth) map { w =>
        when (valid(w)) {
          XSDebug(s"$pipeline_stage_name channel %d: $signal_name\n", w.U)
        }
      }
    }
  }
}
