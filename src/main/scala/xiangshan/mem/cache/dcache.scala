package xiangshan.mem.cache

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import xiangshan.mem.{DCacheReq, DCacheResp, LSUDMemIO}
import xiangshan.utils.XSDebug
import bus.tilelink._
import _root_.utils.{Code, RandomReplacement, Transpose}

// DCache specific parameters
// L1 DCache is 64set, 8way-associative, with 64byte block, a total of 32KB
// It's a virtually indexed, physically tagged cache.
case class DCacheParameters(
    nSets: Int = 64,
    nWays: Int = 8,
    rowBits: Int = 64,
    numDCacheBanks: Int = 2,
    nTLBEntries: Int = 32,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    dataECCBytes: Int = 1,
    nMSHRs: Int = 1,
    nSDQ: Int = 17,
    nRPQ: Int = 16,
    nMMIOs: Int = 1,
    blockBytes: Int = 64,
    busParams: TLParameters) extends L1CacheParameters {

  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)

  def replacement = new RandomReplacement(nWays)
}

trait HasDCacheParameters extends HasL1CacheParameters {
  val cacheParams = dcacheParameters
  val cfg = cacheParams

  val numDCacheBanks = cfg.numDCacheBanks
  // the width of inner CPU data interface
  def wordBits = DataBits
  def wordBytes = DataBytes
  def wordOffBits = log2Up(wordBytes)
  def beatBytes = cfg.blockBytes / cacheDataBeats
  def beatWords = beatBytes / wordBytes
  def beatOffBits = log2Up(beatBytes)
  def idxMSB = untagBits-1
  def idxLSB = blockOffBits
  def offsetmsb = idxLSB-1
  def offsetlsb = wordOffBits
  def rowWords = rowBits/wordBits
  def doNarrowRead = DataBits * nWays % rowBits == 0
  def eccBytes = cacheParams.dataECCBytes
  val eccBits = cacheParams.dataECCBytes * 8
  val encBits = cacheParams.dataCode.width(eccBits)
  val encWordBits = encBits * (wordBits / eccBits)
  def encDataBits = cacheParams.dataCode.width(wordBits) // NBDCache only
  def encRowBits = encDataBits*rowWords
  def lrscCycles = LRSCCycles // ISA requires 16-insn LRSC sequences to succeed
  def lrscBackoff = 3 // disallow LRSC reacquisition briefly
  def blockProbeAfterGrantCycles = 8 // give the processor some time to issue a request after a grant
  def nIOMSHRs = cacheParams.nMMIOs
  def maxUncachedInFlight = cacheParams.nMMIOs

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  // To make things easier, now we assume:
  // core_data_width(wordBits) == L1_basic_storage_unit_width(rowBits) ==
  // outer_tilelink_interface_width(cacheDataBits)
  require(rowBits == wordBits, s"rowBits($rowBits) != wordBits($wordBits)")
  require(rowBits == cacheDataBits, s"rowBits($rowBits) != cacheDataBits($cacheDataBits)")
  require(pgIdxBits >= untagBits, s"page aliasing problem: pgIdxBits($pgIdxBits) < untagBits($untagBits)")
}

abstract class DCacheModule extends L1CacheModule
  with HasDCacheParameters

abstract class DCacheBundle extends L1CacheBundle
  with HasDCacheParameters

// basic building blocks for L1 DCache
class L1Metadata extends DCacheBundle {
  val coh = new ClientMetadata
  val tag = UInt(tagBits.W)
}

object L1Metadata {
  def apply(tag: Bits, coh: ClientMetadata) = {
    val meta = Wire(new L1Metadata)
    meta.tag := tag
    meta.coh := coh
    meta
  }
}

class L1MetaReadReq extends DCacheBundle {
  val idx    = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
  val tag    = UInt(tagBits.W)
}

class L1MetaWriteReq extends L1MetaReadReq {
  val data = new L1Metadata
}

class L1DataReadReq extends DCacheBundle {
  val way_en = Bits(nWays.W)
  val addr   = Bits(untagBits.W)
}

class L1DataWriteReq extends L1DataReadReq {
  val wmask  = Bits(rowWords.W)
  val data   = Bits(encRowBits.W)
}

class L1MetadataArray[T <: L1Metadata](onReset: () => T) extends DCacheModule {
  val rstVal = onReset()
  val io = IO(new Bundle {
    val read = Flipped(Decoupled(new L1MetaReadReq))
    val write = Flipped(Decoupled(new L1MetaWriteReq))
    val resp = Output(Vec(nWays, rstVal.cloneType))
  })
  val rst_cnt = RegInit(0.U(log2Up(nSets+1).W))
  val rst = rst_cnt < nSets.U
  val waddr = Mux(rst, rst_cnt, io.write.bits.idx)
  val wdata = Mux(rst, rstVal, io.write.bits.data).asUInt
  val wmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.write.bits.way_en.asSInt).asBools
  val rmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.read.bits.way_en.asSInt).asBools
  when (rst) { rst_cnt := rst_cnt + 1.U }

  val metabits = rstVal.getWidth
  val tag_array = SyncReadMem(nSets, Vec(nWays, UInt(metabits.W)))
  val wen = rst || io.write.valid
  when (wen) {
    tag_array.write(waddr, VecInit(Array.fill(nWays)(wdata)), wmask)
  }
  io.resp := tag_array.read(io.read.bits.idx, io.read.fire()).map(_.asTypeOf(rstVal))

  io.read.ready := !wen // so really this could be a 6T RAM
  io.write.ready := !rst
}


// argument general L1 DCache bundles with memWidth
class DCacheMetaReadReq extends DCacheBundle {
  val req = Vec(memWidth, new L1MetaReadReq)
}

class DCacheDataReadReq extends DCacheBundle {
  val req = Vec(memWidth, new L1DataReadReq)
  val valid = Vec(memWidth, Bool())
}

abstract class AbstractDataArray extends DCacheModule {
  val io = IO(new DCacheBundle {
    val read  = Input(Vec(memWidth, Valid(new L1DataReadReq)))
    val write = Input(Valid(new L1DataWriteReq))
    val resp  = Output(Vec(memWidth, Vec(nWays, Bits(encRowBits.W))))
    val nacks = Output(Vec(memWidth, Bool()))
  })

  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))
}

class DuplicatedDataArray extends AbstractDataArray
{

  val waddr = io.write.bits.addr >> rowOffBits
  for (j <- 0 until memWidth) {

    val raddr = io.read(j).bits.addr >> rowOffBits
    for (w <- 0 until nWays) {
      val array = SyncReadMem(nSets * refillCycles, Vec(rowWords, Bits(encDataBits.W)))
      when (io.write.bits.way_en(w) && io.write.valid) {
        val data = VecInit((0 until rowWords) map (i => io.write.bits.data(encDataBits*(i+1)-1,encDataBits*i)))
        array.write(waddr, data, io.write.bits.wmask.asBools)
      }
      io.resp(j)(w) := RegNext(array.read(raddr, io.read(j).bits.way_en(w) && io.read(j).valid).asUInt)
    }
    io.nacks(j) := false.B
  }
}

class BankedDataArray extends AbstractDataArray {

  val nBanks   = cfg.numDCacheBanks
  val bankSize = nSets * refillCycles / nBanks
  require (nBanks >= memWidth)
  require (bankSize > 0)

  val bankBits    = log2Ceil(nBanks)
  val bankOffBits = log2Ceil(rowWords) + log2Ceil(wordBytes)
  val bidxBits    = log2Ceil(bankSize)
  val bidxOffBits = bankOffBits + bankBits

  //----------------------------------------------------------------------------------------------------

  // 确定每个请求的bank
  // 问题？其实假如这里偷懒的话，也可以把bank不bank的信息暴露到LSQ那边，让它来处理bank不bank
  val s0_rbanks = if (nBanks > 1) VecInit(io.read.map(r => (r.bits.addr >> bankOffBits)(bankBits-1,0))) else VecInit(0.U)
  val s0_wbank  = if (nBanks > 1) (io.write.bits.addr >> bankOffBits)(bankBits-1,0) else 0.U
  //  每个请求的index
  val s0_ridxs  = VecInit(io.read.map(r => (r.bits.addr >> bidxOffBits)(bidxBits-1,0)))
  val s0_widx   = (io.write.bits.addr >> bidxOffBits)(bidxBits-1,0)

  val s0_read_valids    = VecInit(io.read.map(_.valid))
  // 把自己和自己左边的进行比较
  val s0_bank_conflicts = pipeMap(w => (0 until w).foldLeft(false.B)((c,i) => c || io.read(i).valid && s0_rbanks(i) === s0_rbanks(w)))
  // 只有当自己是valid并且与左边的不冲突时就可以读
  val s0_do_bank_read   = s0_read_valids zip s0_bank_conflicts map {case (v,c) => v && !c}
  // 这是啥？
  val s0_bank_read_gnts = Transpose(VecInit(s0_rbanks zip s0_do_bank_read map {case (b,d) => VecInit((UIntToOH(b) & Fill(nBanks,d)).asBools)}))
  // 写不会和任何人抢bank，它有自己的口
  val s0_bank_write_gnt = (UIntToOH(s0_wbank) & Fill(nBanks, io.write.valid)).asBools

  //----------------------------------------------------------------------------------------------------

  val s1_rbanks         = RegNext(s0_rbanks)
  val s1_ridxs          = RegNext(s0_ridxs)
  val s1_read_valids    = RegNext(s0_read_valids)
  val s1_pipe_selection = pipeMap(i => VecInit(PriorityEncoderOH(pipeMap(j =>
                            if (j < i) s1_read_valids(j) && s1_rbanks(j) === s1_rbanks(i)
                            else if (j == i) true.B else false.B))))
  val s1_ridx_match     = pipeMap(i => pipeMap(j => if (j < i) s1_ridxs(j) === s1_ridxs(i)
                                                    else if (j == i) true.B else false.B))
  val s1_nacks          = pipeMap(w => s1_read_valids(w) && (s1_pipe_selection(w).asUInt & ~s1_ridx_match(w).asUInt).orR)
  val s1_bank_selection = pipeMap(w => Mux1H(s1_pipe_selection(w), s1_rbanks))

  //----------------------------------------------------------------------------------------------------

  val s2_bank_selection = RegNext(s1_bank_selection)
  val s2_nacks          = RegNext(s1_nacks)

  for (w <- 0 until nWays) {
    val s2_bank_reads = Reg(Vec(nBanks, Bits(encRowBits.W)))

    for (b <- 0 until nBanks) {
      val array = SyncReadMem(bankSize, Vec(rowWords, Bits(encDataBits.W)))
      val ridx = Mux1H(s0_bank_read_gnts(b), s0_ridxs)
      val way_en = Mux1H(s0_bank_read_gnts(b), io.read.map(_.bits.way_en))
      s2_bank_reads(b) := array.read(ridx, way_en(w) && s0_bank_read_gnts(b).reduce(_||_)).asUInt

      when (io.write.bits.way_en(w) && s0_bank_write_gnt(b)) {
        val data = VecInit((0 until rowWords) map (i => io.write.bits.data(encDataBits*(i+1)-1,encDataBits*i)))
        array.write(s0_widx, data, io.write.bits.wmask.asBools)
      }
    }

    for (i <- 0 until memWidth) {
      io.resp(i)(w) := s2_bank_reads(s2_bank_selection(i))
    }
  }

  io.nacks := s2_nacks
}



class DCache extends DCacheModule
{
  val io = IO(new DCacheBundle{
    val lsu   = Flipped(new LSUDMemIO)
    val bus = new TLCached(cfg.busParams)
  })

  def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))

  // ------------
  // MSHR, Prober, WBU
  val wb = Module(new WritebackUnit)
  // val prober = Module(new ProbeUnit)
  val mshrs = Module(new MSHRFile)


  // ------------
  // Meta array
  def onReset = L1Metadata(0.U, ClientMetadata.onReset)
  val meta = Seq.fill(memWidth) { Module(new L1MetadataArray(onReset _)) }

  // 0 goes to MSHR refills, 1 goes to prober
  val MetaWritePortCount = 2
  val MSHRMetaWritePort = 0
  val ProberMetaWritePort = 1

  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, MetaWritePortCount))

  // 0 goes to MSHR replays, 1 goes to prober, 2 goes to wb, 3 goes to MSHR meta read,
  // 4 goes to pipeline, 5 goes to prefetcher
  val MetaReadPortCount = 3
  val ReplayMetaReadPort = 0
  val ProberMetaReadPort = 1
  val PipelineMetaReadPort = 2

  val metaReadArb = Module(new Arbiter(new DCacheMetaReadReq, MetaReadPortCount))

  for (w <- 0 until memWidth) {
    meta(w).io.write.valid := metaWriteArb.io.out.fire()
    meta(w).io.write.bits  := metaWriteArb.io.out.bits
    meta(w).io.read.valid  := metaReadArb.io.out.valid
    meta(w).io.read.bits   := metaReadArb.io.out.bits.req(w)
  }

  metaReadArb.io.out.ready  := meta.map(_.io.read.ready).reduce(_||_)
  metaWriteArb.io.out.ready := meta.map(_.io.write.ready).reduce(_||_)


  // ------------
  // Data array
  val data = Module(if (numDCacheBanks == 1) new DuplicatedDataArray else new BankedDataArray)

  // 0 goes to pipeline, 1 goes to MSHR refills
  val DataWritePortCount = 2
  val PipelineDataWritePort = 0
  val RefillDataWritePort = 1

  val dataWriteArb = Module(new Arbiter(new L1DataWriteReq, DataWritePortCount))

  // 0 goes to MSHR replays, 1 goes to wb, 2 goes to pipeline
  val DataReadPortCount = 3
  val ReplayDataReadPort = 0
  val WritebackDataReadPort = 1
  val PipelineDataReadPort = 2

  val dataReadArb = Module(new Arbiter(new DCacheDataReadReq, DataReadPortCount))

  for (w <- 0 until memWidth) {
    data.io.read(w).valid := dataReadArb.io.out.bits.valid(w) && dataReadArb.io.out.valid
    data.io.read(w).bits  := dataReadArb.io.out.bits.req(w)
  }
  dataReadArb.io.out.ready := true.B

  data.io.write.valid := dataWriteArb.io.out.fire()
  data.io.write.bits  := dataWriteArb.io.out.bits
  dataWriteArb.io.out.ready := true.B


  // assign default value to signals
  /*
  io.lsu.req.ready := false.B
  io.lsu.resp(0).vai
  io.bus := DontCare

  wb.io := DontCare
  mshrs.io := DontCare
  metaReadArb.io.in := DontCare
  metaWriteArb.io.in := DontCare

  dataReadArb.io.in := DontCare
  dataWriteArb.io.in := DontCare
  */


  // ------------
  // New requests
  val t_replay :: t_lsu :: Nil = Enum(2)

  // LSU requests
  io.lsu.req.ready := metaReadArb.io.in(PipelineMetaReadPort).ready && dataReadArb.io.in(PipelineDataReadPort).ready
  metaReadArb.io.in(PipelineMetaReadPort).valid := io.lsu.req.valid
  dataReadArb.io.in(PipelineDataReadPort).valid := io.lsu.req.valid
  for (w <- 0 until memWidth) {
    // Tag read for new requests
    metaReadArb.io.in(PipelineMetaReadPort).bits.req(w).idx    := io.lsu.req.bits(w).bits.addr >> blockOffBits
    metaReadArb.io.in(PipelineMetaReadPort).bits.req(w).way_en := DontCare
    metaReadArb.io.in(PipelineMetaReadPort).bits.req(w).tag    := DontCare
    // Data read for new requests
    dataReadArb.io.in(PipelineDataReadPort).bits.valid(w)      := io.lsu.req.bits(w).valid
    dataReadArb.io.in(PipelineDataReadPort).bits.req(w).addr   := io.lsu.req.bits(w).bits.addr
    dataReadArb.io.in(PipelineDataReadPort).bits.req(w).way_en := ~0.U(nWays.W)
  }

  // ------------
  // MSHR Replays
  val replay_req = Wire(Vec(memWidth, new DCacheReq))
  replay_req           := DontCare
  replay_req(0).cmd    := mshrs.io.replay.bits.cmd
  replay_req(0).addr   := mshrs.io.replay.bits.addr
  replay_req(0).data   := mshrs.io.replay.bits.data
  replay_req(0).mask   := mshrs.io.replay.bits.mask
  replay_req(0).meta   := mshrs.io.replay.bits.meta

  mshrs.io.replay.ready    := metaReadArb.io.in(ReplayMetaReadPort).ready && dataReadArb.io.in(ReplayDataReadPort).ready
  // Tag read for MSHR replays
  // We don't actually need to read the metadata, for replays we already know our way
  metaReadArb.io.in(ReplayMetaReadPort).valid              := mshrs.io.replay.valid
  metaReadArb.io.in(ReplayMetaReadPort).bits.req(0).idx    := mshrs.io.replay.bits.addr >> blockOffBits
  metaReadArb.io.in(ReplayMetaReadPort).bits.req(0).way_en := DontCare
  metaReadArb.io.in(ReplayMetaReadPort).bits.req(0).tag    := DontCare
  metaReadArb.io.in(ReplayMetaReadPort).bits.req(1)        := DontCare

  // Data read for MSHR replays
  dataReadArb.io.in(ReplayDataReadPort).valid              := mshrs.io.replay.valid
  dataReadArb.io.in(ReplayDataReadPort).bits.valid         := widthMap(w => (w == 0).B)
  dataReadArb.io.in(ReplayDataReadPort).bits.req(0).addr   := mshrs.io.replay.bits.addr
  dataReadArb.io.in(ReplayDataReadPort).bits.req(0).way_en := mshrs.io.replay.bits.way_en
  dataReadArb.io.in(ReplayDataReadPort).bits.req(1)        := DontCare

  // -----------
  // Write-backs
  // Data read for write-back
  dataReadArb.io.in(WritebackDataReadPort).valid        := wb.io.data_req.valid
  dataReadArb.io.in(WritebackDataReadPort).bits.valid   := widthMap(w => (w == 0).B)
  dataReadArb.io.in(WritebackDataReadPort).bits.req(0)  := wb.io.data_req.bits
  dataReadArb.io.in(WritebackDataReadPort).bits.req(1)  := DontCare
  wb.io.data_req.ready  := dataReadArb.io.in(WritebackDataReadPort).ready


  // -------
  // Pipeline
  def dump_pipeline_reqs(pipeline_stage_name: String, valid: Vec[Bool],
    reqs: Vec[DCacheReq], s0_type: UInt) = {
      (0 until memWidth) map { w =>
          XSDebug(s"$pipeline_stage_name")
          XSDebug("channel %d: valid: %b ", w.U, valid(w))
          when (valid(w)) {
            when (s0_type === t_replay) {
              XSDebug("type: reply ")
              } .elsewhen (s0_type === t_lsu) {
              XSDebug("type: reply ")
              } .otherwise {
                XSDebug("type: unknown ")
              }
              XSDebug("cmd: %x addr: %x data: %x mask: %x meta: %x\n",
                reqs(w).cmd, reqs(w).addr, reqs(w).data, reqs(w).mask, reqs(w).meta)
          }
      }
  }

  // stage 0
  val s0_valid = Mux(io.lsu.req.fire(), VecInit(io.lsu.req.bits.map(_.valid)),
    Mux(mshrs.io.replay.fire(), VecInit(1.U(memWidth.W).asBools),
      VecInit(0.U(memWidth.W).asBools)))
  val s0_req = Mux(io.lsu.req.fire(), VecInit(io.lsu.req.bits.map(_.bits)),
    replay_req)
  val s0_type = Mux(io.lsu.req.fire(), t_lsu, t_replay)

  dump_pipeline_reqs("DCache s0", s0_valid, s0_req, s0_type)


  // Does this request need to send a response or nack
  // for successfully executed load/stores, we send a resp 
  // for all other failures(bank conflict, blocked by mshr, in write back)
  // we send a nack
  // all pipeline requests requires response or nack
  // only mshr replayed loads needs to send resp
  val s0_send_resp_or_nack = Mux(io.lsu.req.fire(), s0_valid,
    VecInit(Mux(mshrs.io.replay.fire() && isRead(mshrs.io.replay.bits.cmd), 1.U(memWidth.W), 0.U(memWidth.W)).asBools))


  // stage 1
  val s1_req = RegNext(s0_req)
  // val s2_store_failed = Wire(Bool())
  val s1_valid = widthMap(w => RegNext(s0_valid(w), init=false.B))
  val s1_addr = s1_req.map(_.addr)
  val s1_nack = VecInit(0.U(memWidth.W).asBools)
  val s1_send_resp_or_nack = RegNext(s0_send_resp_or_nack)
  val s1_type = RegNext(s0_type)
  // For replays, the metadata isn't written yet
  val s1_replay_way_en = RegNext(mshrs.io.replay.bits.way_en)

  dump_pipeline_reqs("DCache s1", s1_valid, s1_req, s1_type)

  // tag check
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = widthMap(i => wayMap((w: Int) => meta(i).io.resp(w).tag === (s1_addr(i) >> untagBits)).asUInt)
  val s1_tag_match_way = widthMap(i =>
      Mux(s1_type === t_replay, s1_replay_way_en,
        wayMap((w: Int) => s1_tag_eq_way(i)(w) && meta(i).io.resp(w).coh.isValid()).asUInt))


  // stage 2
  val s2_req   = RegNext(s1_req)
  val s2_type  = RegNext(s1_type)
  val s2_valid = widthMap(w =>
                  RegNext(s1_valid(w), init = false.B))

  dump_pipeline_reqs("DCache s2", s2_valid, s2_req, s2_type)

  val s2_tag_match_way = RegNext(s1_tag_match_way)
  val s2_tag_match     = s2_tag_match_way.map(_.orR)
  val s2_hit_state     = widthMap(i => Mux1H(s2_tag_match_way(i), wayMap((w: Int) => RegNext(meta(i).io.resp(w).coh))))
  val s2_has_permission = widthMap(w => s2_hit_state(w).onAccess(s2_req(w).cmd)._1)
  val s2_new_hit_state  = widthMap(w => s2_hit_state(w).onAccess(s2_req(w).cmd)._3)

  // we not only need permissions
  // we also require that state does not change on hit
  // thus we require new_hit_state === old_hit_state
  //
  // If state changes on hit,
  // we should treat it as not hit, and let mshr deal with it,
  // since we can not write meta data on the main pipeline.
  // It's possible that we had permission but state changes on hit:
  // eg: write to exclusive but clean block
  val s2_hit = widthMap(w => (s2_tag_match(w) && s2_has_permission(w) && s2_hit_state(w) === s2_new_hit_state(w) && !mshrs.io.block_hit(w)) || (s2_type === t_replay))
  val s2_nack = Wire(Vec(memWidth, Bool()))
  assert(!(s2_type === t_replay && !s2_hit(0)), "Replays should always hit")

  val s2_data = Wire(Vec(memWidth, Vec(nWays, UInt(encRowBits.W))))
  for (i <- 0 until memWidth) {
    for (w <- 0 until nWays) {
      s2_data(i)(w) := data.io.resp(i)(w)
    }
  }

  val s2_data_muxed = widthMap(w => Mux1H(s2_tag_match_way(w), s2_data(w)))
  // the index of word in a row, in case rowBits != wordBits
  val s2_word_idx   = widthMap(w => if (rowWords == 1) 0.U else s2_req(w).addr(log2Up(rowWords*wordBytes)-1, log2Up(wordBytes)))

  // replacement policy
  val replacer = cacheParams.replacement
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegNext(replacer.way))
  val s2_repl_meta = widthMap(i => Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegNext(meta(i).io.resp(w))).toSeq))

  val s2_nack_hit    = RegNext(s1_nack)
  // Can't allocate MSHR for same set currently being written back
  // the same set is busy
  val s2_nack_set_busy  = widthMap(w => s2_valid(w) && mshrs.io.block_hit(w))
  // MSHRs not ready for request
  // two possibilities:
  // 1. all MSHR in use
  // 2. two misses in one cycle and MSHR can only handle one
  val s2_nack_no_mshr   = widthMap(w => s2_valid(w) && !s2_hit(w) && !mshrs.io.req(w).ready)
  // Bank conflict on data arrays
  val s2_nack_data   = widthMap(w => data.io.nacks(w))

  s2_nack           := widthMap(w => (s2_nack_hit(w) || s2_nack_set_busy(w) || s2_nack_no_mshr(w) || s2_nack_data(w)) && s2_type =/= t_replay)
  val s2_send_resp = widthMap(w => (RegNext(s1_send_resp_or_nack(w)) && !s2_nack(w) &&
                      (s2_hit(w))))
  val s2_send_nack = widthMap(w => (RegNext(s1_send_resp_or_nack(w)) && s2_nack(w)))
  for (w <- 0 until memWidth)
    assert(!(s2_send_resp(w) && s2_send_nack(w)))

  // hits always send a response
  // If MSHR is not available, LSU has to replay this request later
  // If MSHR is available and this is only a store(not a amo), we don't need to wait for resp later

  // Miss handling
  for (w <- 0 until memWidth) {
    mshrs.io.req(w).valid := s2_valid(w)          &&
                            !s2_hit(w)            &&
                            !s2_nack_hit(w)       &&
                            !s2_nack_set_busy(w)  &&
                            !s2_nack_data(w)      &&
                             s2_type === t_lsu
    assert(!(mshrs.io.req(w).valid && s2_type === t_replay), "Replays should not need to go back into MSHRs")
    mshrs.io.req(w).bits.cmd         := s2_req(w).cmd
    mshrs.io.req(w).bits.addr        := s2_req(w).addr
    mshrs.io.req(w).bits.mask        := s2_req(w).mask
    mshrs.io.req(w).bits.data        := s2_req(w).data
    mshrs.io.req(w).bits.meta        := s2_req(w).meta

    mshrs.io.req(w).bits.tag_match   := s2_tag_match(w)
    mshrs.io.req(w).bits.old_meta    := Mux(s2_tag_match(w), L1Metadata(s2_repl_meta(w).tag, s2_hit_state(w)), s2_repl_meta(w))
    mshrs.io.req(w).bits.way_en      := Mux(s2_tag_match(w), s2_tag_match_way(w), s2_replaced_way_en)
    mshrs.io.req(w).bits.sdq_id      := DontCare
  }

  when (mshrs.io.req.map(_.fire()).reduce(_||_)) { replacer.miss }
  io.bus.a <> mshrs.io.mem_acquire

  // probes and releases
  // we do not support probe for now
  io.bus.b.ready        := false.B
  metaReadArb.io.in(ProberMetaReadPort).valid := false.B
  metaReadArb.io.in(ProberMetaReadPort).bits := DontCare
  metaWriteArb.io.in(ProberMetaWritePort).valid := false.B
  metaWriteArb.io.in(ProberMetaWritePort).bits := DontCare

  // refills
  when (io.bus.d.bits.source === cfg.nMSHRs.U) {
    // This should be ReleaseAck
    io.bus.d.ready := true.B
    mshrs.io.mem_grant.valid := false.B
    mshrs.io.mem_grant.bits  := DontCare
  } .otherwise {
    // This should be GrantData
    mshrs.io.mem_grant <> io.bus.d
  }

  io.bus.e <> mshrs.io.mem_finish

  dataWriteArb.io.in(RefillDataWritePort) <> mshrs.io.refill
  metaWriteArb.io.in(MSHRMetaWritePort) <> mshrs.io.meta_write

  // writebacks
  val wbArb = Module(new Arbiter(new WritebackReq, 2))
  // 0 goes to prober, 1 goes to MSHR evictions
  wbArb.io.in(0).valid := false.B
  wbArb.io.in(0).bits  := DontCare
  wbArb.io.in(1)       <> mshrs.io.wb_req
  wb.io.req            <> wbArb.io.out
  wb.io.data_resp      := data.io.resp(0)
  mshrs.io.wb_resp     := wb.io.resp
  wb.io.mem_grant      := io.bus.d.fire() && io.bus.d.bits.source === cfg.nMSHRs.U

  TLArbiter.lowest(io.bus.c, wb.io.release)

  // load data gen
  val s2_data_word_prebypass = widthMap(w => s2_data_muxed(w) >> Cat(s2_word_idx(w), 0.U(log2Ceil(wordBits).W)))
  val s2_data_word = Wire(Vec(memWidth, UInt()))

  // Mux between cache responses and uncache responses
  val cache_resp = Wire(Vec(memWidth, Valid(new DCacheResp)))
  for (w <- 0 until memWidth) {
    cache_resp(w).valid         := s2_valid(w) && (s2_send_resp(w) || s2_send_nack(w))
    cache_resp(w).bits.data     := s2_data_word(w)
    cache_resp(w).bits.meta     := 0.U
    cache_resp(w).bits.nack     := s2_send_nack(w)
  }

  val resp = WireInit(cache_resp)

  // 返回结果
  for (w <- 0 until memWidth) {
    io.lsu.resp(w) <> resp(w)
  }

  // Store/amo hits
  val s3_req   = RegNext(s2_req(0))
  val s3_valid = RegNext(s2_valid(0) && s2_hit(0) && isWrite(s2_req(0).cmd) &&
                         !(s2_send_nack(0) && s2_nack(0)), init = false.B)
  for (w <- 1 until memWidth) {
    assert(!(s2_valid(w) && s2_hit(w) && isWrite(s2_req(w).cmd)),
      "Store must go through 0th pipe in L1D")
  }

  // For bypassing
  val s4_req   = RegNext(s3_req)
  val s4_valid = RegNext(s3_valid, init = false.B)
  val s5_req   = RegNext(s4_req)
  val s5_valid = RegNext(s4_valid, init = false.B)

  val s3_bypass = widthMap(w => s3_valid && ((s2_req(w).addr >> wordOffBits) === (s3_req.addr >> wordOffBits)))
  val s4_bypass = widthMap(w => s4_valid && ((s2_req(w).addr >> wordOffBits) === (s4_req.addr >> wordOffBits)))
  val s5_bypass = widthMap(w => s5_valid && ((s2_req(w).addr >> wordOffBits) === (s5_req.addr >> wordOffBits)))

  // Store -> Load bypassing
  for (w <- 0 until memWidth) {
    s2_data_word(w) := Mux(s3_bypass(w), s3_req.data,
                       Mux(s4_bypass(w), s4_req.data,
                       Mux(s5_bypass(w), s5_req.data,
                                         s2_data_word_prebypass(w))))
  }
  val amoalu   = Module(new AMOALU(DataBits))
  amoalu.io.cmd  := s2_req(0).cmd
  amoalu.io.mask := s2_req(0).mask
  amoalu.io.lhs  := s2_data_word(0)
  amoalu.io.rhs  := s2_req(0).data


  s3_req.data := amoalu.io.out
  val s3_way   = RegNext(s2_tag_match_way(0))

  dataWriteArb.io.in(PipelineDataWritePort).valid       := s3_valid
  dataWriteArb.io.in(PipelineDataWritePort).bits.addr   := s3_req.addr
  val wmask = if (rowWords == 1) 1.U else UIntToOH(s3_req.addr(rowOffBits-1,offsetlsb))
  dataWriteArb.io.in(PipelineDataWritePort).bits.wmask  := wmask
  dataWriteArb.io.in(PipelineDataWritePort).bits.data   := Fill(rowWords, s3_req.data)
  dataWriteArb.io.in(PipelineDataWritePort).bits.way_en := s3_way
}

class DcacheUserBundle extends Bundle
