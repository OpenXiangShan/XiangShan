package xiangshan.mem.cache

import chisel3._
import chisel3.util._

import utils.XSDebug
import xiangshan.mem.MemoryOpConstants

class StorePipe extends DCacheModule
{
  val io = IO(new DCacheBundle{
    val lsu        = Flipped(new DCacheStoreIO)
    val data_read  = Decoupled(new L1DataReadReq)
    val data_resp  = Output(Vec(nWays, Vec(refillCycles, Bits(encRowBits.W))))
    val data_write = Output(Valid(new L1DataWriteReq))
    val meta_read  = Decoupled(new L1MetaReadReq)
    val meta_resp  = Output(Vec(nWays, new L1Metadata))
  })


  // LSU requests
  io.lsu.req.ready := io.meta_read.ready && io.data_read.ready
  io.meta_read.valid := io.lsu.req.valid
  io.data_read.valid := io.lsu.req.valid

  val meta_read = io.meta_read.bits
  val data_read = io.data_read.bits

  // Tag read for new requests
  meta_read.idx    := io.lsu.req.bits.addr >> blockOffBits
  meta_read.way_en := ~0.U(nWays.W)
  meta_read.tag    := DontCare
  // Data read for new requests
  data_read.addr   := io.lsu.req.bits.addr
  data_read.way_en := ~0.U(nWays.W)

  // Pipeline
  // stage 0
  val s0_valid = io.lsu.req.fire()
  val s0_req = io.lsu.req.bits

  assert(!(s0_valid && s0_req.cmd =/= MemoryOpConstants.M_XWR), "StorePipe only accepts store req")

  dump_pipeline_reqs("StorePipe s0", s0_valid, s0_req)

  // stage 1
  val s1_req = RegNext(s0_req)
  val s1_valid = RegNext(s0_valid, init = false.B)
  val s1_addr = s1_req.addr
  val s1_nack = false.B 

  dump_pipeline_reqs("StorePipe s1", s1_valid, s1_req)

  val meta_resp = io.meta_resp
  // tag check
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta_resp(w).tag === (s1_addr >> untagBits)).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && meta_resp(w).coh.isValid()).asUInt


  // stage 2
  val s2_req   = RegNext(s1_req)
  val s2_valid = RegNext(s1_valid, init = false.B)

  dump_pipeline_reqs("StorePipe s2", s2_valid, s2_req)

  val s2_tag_match_way = RegNext(s1_tag_match_way)
  val s2_tag_match     = s2_tag_match_way.orR
  val s2_hit_way       = OHToUInt(s2_tag_match_way)
  val s2_hit_state     = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegNext(meta_resp(w).coh)))
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
  val s2_hit = s2_tag_match && s2_has_permission && s2_hit_state === s2_new_hit_state
  val s2_nack = Wire(Bool())

  val s2_nack_hit    = RegNext(s1_nack)
  val s2_nack_set_busy  = s2_valid && false.B

  s2_nack           := s2_nack_hit || s2_nack_set_busy

  val data_resp = io.data_resp
  val s2_data = data_resp(s2_hit_way)
  val wdata = Wire(Vec(refillCycles, UInt(encRowBits.W)))
  val wmask = Wire(Vec(refillCycles, UInt(rowBytes.W)))
  val wdata_merged = Wire(Vec(refillCycles, UInt(encRowBits.W)))

  def mergePutData(old_data: UInt, new_data: UInt, wmask: UInt): UInt = {
    val full_wmask = FillInterleaved(8, wmask)
    ((~full_wmask & old_data) | (full_wmask & new_data))
  }

  // now, we do not deal with ECC
  for (i <- 0 until refillCycles) {
    wdata(i)        := io.lsu.req.bits.data(rowBits * (i + 1), rowBits * i)
    wmask(i)        := io.lsu.req.bits.mask(rowBytes * (i + 1), rowBytes * i)
    wdata_merged(i) := Cat(s2_data(i)(encRowBits - 1, rowBits),
      mergePutData(s2_data(i)(rowBits - 1, 0), wdata(i), wmask(i)))
  }

  // write dcache if hit
  val data_write = io.data_write.bits
  io.data_write.valid := s2_valid && s2_hit
  data_write.rmask    := DontCare
  data_write.way_en   := s2_tag_match_way
  data_write.addr     := s2_req.addr
  data_write.wmask    := wmask
  data_write.data     := wdata_merged

  dump_pipeline_valids("StorePipe s2", "s2_hit", s2_hit)
  dump_pipeline_valids("StorePipe s2", "s2_nack", s2_nack)
  dump_pipeline_valids("StorePipe s2", "s2_nack_hit", s2_nack_hit)
  dump_pipeline_valids("StorePipe s2", "s2_nack_set_busy", s2_nack_set_busy)

  val resp = Wire(Valid(new DCacheResp))
  resp.valid     := s2_valid
  resp.bits.data := DontCare
  resp.bits.meta := s2_req.meta
  resp.bits.miss := !s2_hit
  resp.bits.nack := s2_nack

  io.lsu.resp <> resp

  when (resp.valid) {
    XSDebug(s"StorePipe resp: data: %x id: %d replay: %b miss: %b nack: %b\n",
      resp.bits.data, resp.bits.meta.id, resp.bits.meta.replay, resp.bits.miss, resp.bits.nack)
  }

  // -------
  // Debug logging functions
  def dump_pipeline_reqs(pipeline_stage_name: String, valid: Bool,
    req: DCacheStoreReq) = {
      when (valid) {
        XSDebug(s"$pipeline_stage_name cmd: %x addr: %x id: %d replay: %b\n",
          req.cmd, req.addr, req.meta.id, req.meta.replay)
        (0 until refillCycles) map { r =>
          XSDebug(s"cycle: $r data: %x wmask: %x\n",
            req.data(r), req.mask(r))
        }
      }
  }

  def dump_pipeline_valids(pipeline_stage_name: String, signal_name: String, valid: Bool) = {
    when (valid) {
      XSDebug(s"$pipeline_stage_name $signal_name\n")
    }
  }
}
