package xiangshan.cache

import chisel3._
import chisel3.util._
import utils.{Code, ReplacementPolicy, HasTLDump, XSDebug, SRAMTemplate, XSPerf}
import xiangshan.{HasXSLog}

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange}
import freechips.rocketchip.tilelink.{TLClientNode, TLClientParameters,
  TLMasterParameters, TLMasterPortParameters, TLArbiter,
  TLEdgeOut, TLBundleA, TLBundleD,
  ClientStates, ClientMetadata
}

import scala.math.max


// L1plusCache specific parameters
// L1 L1plusCache is 256 set, 8 way associative, with 64byte block, a total of 128KB
// It's a virtually indexed, physically tagged cache.
case class L1plusCacheParameters
(
    nSets: Int = 256,
    nWays: Int = 8,
    rowBits: Int = 64,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    replacer: Option[String] = Some("random"),
    nMissEntries: Int = 1,
    blockBytes: Int = 64
) extends L1CacheParameters {

  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)

  def replacement = ReplacementPolicy.fromString(replacer,nWays,nSets)
}

trait HasL1plusCacheParameters extends HasL1CacheParameters {
  val cacheParams = l1plusCacheParameters
  val icacheParams = icacheParameters
  val cfg = cacheParams
  val icfg = icacheParams
  val pcfg = l1plusPrefetcherParameters

  def encRowBits = cacheParams.dataCode.width(rowBits)
  def codeWidth = encRowBits - rowBits
  def bankNum = 2
  def bankRows = blockRows / bankNum
  def blockEcodedBits = blockRows * encRowBits
  def plruAccessNum = 2  //hit and miss

  def missQueueEntryIdWidth = log2Up(cfg.nMissEntries)
  // def icacheMissQueueEntryIdWidth = log2Up(icfg.nMissEntries)
  // L1plusCache has 2 clients: ICacheMissQueue and L1plusPrefetcher
  def nClients = 2
  def icacheMissQueueId = 0
  def l1plusPrefetcherId = 1
  def clientIdWidth = log2Up(nClients)
  def icacheMissQueueEntryIdWidth = log2Up(icfg.nMissEntries)
  def l1plusPrefetcherEntryIdWidth = log2Up(pcfg.nEntries)// TODO
  def entryIdWidth = max(icacheMissQueueEntryIdWidth, l1plusPrefetcherEntryIdWidth)
  def idWidth = clientIdWidth + entryIdWidth
  def clientId(id: UInt) = id(idWidth - 1, entryIdWidth)
  def entryId(id: UInt) = id(entryIdWidth - 1, 0)

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
  require(full_divide(beatBits, rowBits), s"beatBits($beatBits) must be multiple of rowBits($rowBits)")
}

abstract class L1plusCacheModule extends L1CacheModule
  with HasL1plusCacheParameters

abstract class L1plusCacheBundle extends L1CacheBundle
  with HasL1plusCacheParameters

// basic building blocks for L1plusCache
// MetaArray and DataArray
// TODO: dedup with DCache
class L1plusCacheMetadata extends L1plusCacheBundle {
  val valid = Bool()
  val tag = UInt(tagBits.W)
}

object L1plusCacheMetadata {
  def apply(tag: Bits, valid: Bool) = {
    val meta = Wire(new L1plusCacheMetadata)
    meta.tag := tag
    meta.valid := valid
    meta
  }
}


/*  tagIdx is from the io.in.req (Wire)
 *  validIdx is from s1_addr (Register)
 */

class L1plusCacheMetaReadReq extends L1plusCacheBundle {
  val tagIdx    = UInt(idxBits.W)
  val validIdx  = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
  val tag    = UInt(tagBits.W)
}

class L1plusCacheMetaWriteReq extends L1plusCacheMetaReadReq {
  val data = new L1plusCacheMetadata
}

class L1plusCacheDataReadReq extends L1plusCacheBundle {
  // you can choose which bank to read to save power
  val rmask  = Bits(blockRows.W)
  val way_en = Bits(nWays.W)
  val addr   = Bits(untagBits.W)
}

// Now, we can write a cache-block in a single cycle
class L1plusCacheDataWriteReq extends L1plusCacheDataReadReq {
  val wmask  = Bits(blockRows.W)
  val data   = Vec(blockRows, Bits(encRowBits.W))
}

class L1plusCacheDataArray extends L1plusCacheModule {
  val io = IO(new L1plusCacheBundle {
    val read  = Flipped(DecoupledIO(new L1plusCacheDataReadReq))
    val write = Flipped(DecoupledIO(new L1plusCacheDataWriteReq))
    val resp  = Output(Vec(nWays, Vec(blockRows, Bits(encRowBits.W))))
  })

  val singlePort = true

  // write is always ready
  io.write.ready := true.B
  val waddr = (io.write.bits.addr >> blockOffBits).asUInt()
  val raddr = (io.read.bits.addr >> blockOffBits).asUInt()

  // for single port SRAM, do not allow read and write in the same cycle
  // for dual port SRAM, raddr === waddr is undefined behavior
  val rwhazard = if(singlePort) io.write.valid else io.write.valid && waddr === raddr
  io.read.ready := !rwhazard

  for (w <- 0 until nWays) {
    val array = List.fill(bankNum)(Module(new SRAMTemplate(UInt((bankRows * rowBits).W), set=nSets, way=1,
      shouldReset=false, holdRead=false, singlePort=singlePort)))
    val codeArray = Module(new SRAMTemplate(UInt((blockRows *codeWidth).W), set=nSets, way=1,
      shouldReset=false, holdRead=false, singlePort=singlePort))
    // data write
    for (b <- 0 until bankNum){
      val respData = VecInit(io.write.bits.data.map{row => row(rowBits - 1, 0)}).asUInt
      val respCode = VecInit(io.write.bits.data.map{row => row(encRowBits - 1, rowBits)}).asUInt
      array(b).io.w.req.valid := io.write.bits.way_en(w) && io.write.valid
      array(b).io.w.req.bits.apply(
        setIdx=waddr,
        data=respData((b+1)*blockBits/2 - 1, b*blockBits/2),
        waymask=1.U)
      
      codeArray.io.w.req.valid := io.write.bits.way_en(w) && io.write.valid
      codeArray.io.w.req.bits.apply(
        setIdx=waddr,
        data=respCode,
        waymask=1.U)
      
      // data read
      array(b).io.r.req.valid := io.read.bits.way_en(w) && io.read.valid
      array(b).io.r.req.bits.apply(setIdx=raddr)

      codeArray.io.r.req.valid := io.read.bits.way_en(w) && io.read.valid
      codeArray.io.r.req.bits.apply(setIdx=raddr)
      for (r <- 0 until blockRows) {
        if(r < blockRows/2){ io.resp(w)(r) := RegNext(Cat(codeArray.io.r.resp.data(0)((r + 1) * codeWidth - 1, r * codeWidth) ,array(0).io.r.resp.data(0)((r + 1) * rowBits - 1, r * rowBits) )) }
        else { 
          val r_half = r - blockRows/2
          io.resp(w)(r) := RegNext(Cat(codeArray.io.r.resp.data(0)((r + 1) * codeWidth - 1, r * codeWidth) ,array(1).io.r.resp.data(0)((r_half + 1) * rowBits - 1, r_half * rowBits))) 
        }
      }
    }
  }

  // since we use a RAM of block width
  // we must do full read and write
  when (io.write.valid) {
    assert (io.write.bits.wmask.andR)
  }

  // since we use a RAM of block width
  // we must do full read and write
  when (io.read.valid) {
    assert (io.read.bits.rmask.andR)
  }

  // debug output
  def dumpRead() = {
    when (io.read.valid) {
      XSDebug(s"DataArray Read valid way_en: %x addr: %x\n",
        io.read.bits.way_en, io.read.bits.addr)
    }
  }

  def dumpWrite() = {
    when (io.write.valid) {
      XSDebug(s"DataArray Write valid way_en: %x addr: %x\n",
        io.write.bits.way_en, io.write.bits.addr)

      (0 until blockRows) map { r =>
        XSDebug(s"cycle: $r data: %x wmask: %x\n",
          io.write.bits.data(r), io.write.bits.wmask(r))
      }
    }
  }

  def dumpResp() = {
      XSDebug(s"DataArray ReadResp\n")
      (0 until nWays) map { i =>
        (0 until blockRows) map { r =>
          XSDebug(s"way: $i cycle: $r data: %x\n", io.resp(i)(r))
        }
      }
  }

  def dump() = {
    dumpRead
    dumpWrite
    dumpResp
  }
}

class L1plusCacheMetadataArray extends L1plusCacheModule {
  val io = IO(new Bundle {
    val read = Flipped(Decoupled(new L1plusCacheMetaReadReq))
    val write = Flipped(Decoupled(new L1plusCacheMetaWriteReq))
    val resp = Output(Vec(nWays, new L1plusCacheMetadata))
    val flush = Input(Bool())
  })
  val waddr = io.write.bits.tagIdx
  val wvalid = io.write.bits.data.valid
  val wtag = io.write.bits.data.tag.asUInt
  val wmask = Mux((nWays == 1).B, (-1).asSInt, io.write.bits.way_en.asSInt).asBools
  val rmask = Mux((nWays == 1).B, (-1).asSInt, io.read.bits.way_en.asSInt).asBools

  def encTagBits = cacheParams.tagCode.width(tagBits)
  val tag_array = Module(new SRAMTemplate(UInt(encTagBits.W), set=nSets, way=nWays,
    shouldReset=false, holdRead=false, singlePort=true))
  val valid_array = Reg(Vec(nSets, UInt(nWays.W)))
  when (reset.toBool || io.flush) {
    for (i <- 0 until nSets) {
      valid_array(i) := 0.U
    }
  }
  XSDebug("valid_array:%x   flush:%d\n",valid_array.asUInt,io.flush)

  // tag write
  val wen = io.write.valid && !reset.toBool && !io.flush
  tag_array.io.w.req.valid := wen
  tag_array.io.w.req.bits.apply(
    setIdx=waddr,
    data=cacheParams.tagCode.encode(wtag),
    waymask=VecInit(wmask).asUInt)

  when (wen) {
    when (wvalid) {
      valid_array(waddr) := valid_array(waddr) | io.write.bits.way_en
    } .otherwise {
      valid_array(waddr) := valid_array(waddr) & ~io.write.bits.way_en
    }
  }

  // tag read
  tag_array.io.r.req.valid := io.read.fire()
  tag_array.io.r.req.bits.apply(setIdx=io.read.bits.tagIdx)
  val rtags = tag_array.io.r.resp.data.map(rdata =>
      cacheParams.tagCode.decode(rdata).corrected)

  for (i <- 0 until nWays) {
    io.resp(i).valid := valid_array(io.read.bits.validIdx)(i)
    io.resp(i).tag   := rtags(i)
  }

  // we use single port SRAM
  // do not allow read and write in the same cycle
  io.read.ready  := !io.write.valid && !reset.toBool && !io.flush && tag_array.io.r.req.ready
  io.write.ready := !reset.toBool && !io.flush && tag_array.io.w.req.ready

  def dumpRead() = {
    when (io.read.fire()) {
      XSDebug("MetaArray Read: idx: (t:%d v:%d) way_en: %x tag: %x\n",
        io.read.bits.tagIdx, io.read.bits.validIdx, io.read.bits.way_en, io.read.bits.tag)
    }
  }

  def dumpWrite() = {
    when (io.write.fire()) {
      XSDebug("MetaArray Write: idx: %d way_en: %x tag: %x new_tag: %x new_valid: %x\n",
        io.write.bits.tagIdx, io.write.bits.way_en, io.write.bits.tag, io.write.bits.data.tag, io.write.bits.data.valid)
    }
  }

  def dumpResp() = {
    (0 until nWays) map { i =>
      XSDebug(s"MetaArray Resp: way: $i tag: %x valid: %x\n",
        io.resp(i).tag, io.resp(i).valid)
    }
  }

  def dump() = {
    dumpRead
    dumpWrite
    dumpResp
  }
}

class L1plusCacheReq extends L1plusCacheBundle
{
  val cmd  = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val id   = UInt(idWidth.W)

  override def toPrintable: Printable = {
    p"cmd=${Binary(cmd)} addr=0x${Hexadecimal(addr)} id=${Binary(id)}"
  }
}

class L1plusCacheResp extends L1plusCacheBundle
{
  val data = UInt((cfg.blockBytes * 8).W)
  val eccWrong = Bool()
  val id   = UInt(idWidth.W)

  override def toPrintable: Printable = {
    p"id=${Binary(id)} data=${Hexadecimal(data)} eccWrong=${Binary(eccWrong)}"
  }
}

class L1plusCacheIO extends L1plusCacheBundle
{
  val req  = DecoupledIO(new L1plusCacheReq)
  val resp = Flipped(DecoupledIO(new L1plusCacheResp))
  val flush = Output(Bool())
  val empty = Input(Bool())

  override def toPrintable: Printable = {
    p"req: v=${req.valid} r=${req.ready} ${req.bits} " +
      p"resp: v=${resp.valid} r=${resp.ready} ${resp.bits}"
  }
}

class L1plusCache()(implicit p: Parameters) extends LazyModule with HasL1plusCacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "l1plusCache",
      sourceId = IdRange(0, cfg.nMissEntries)
    ))
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new L1plusCacheImp(this)
}


class L1plusCacheImp(outer: L1plusCache) extends LazyModuleImp(outer) with HasL1plusCacheParameters with HasXSLog {

  val io = IO(Flipped(new L1plusCacheIO))

  val (bus, edge) = outer.clientNode.out.head
  require(bus.d.bits.data.getWidth == l1BusDataWidth, "L1plusCache: tilelink width does not match")

  //----------------------------------------
  // core data structures
  val dataArray = Module(new L1plusCacheDataArray)

  val metaArray = Module(new L1plusCacheMetadataArray())
  dataArray.dump()
  metaArray.dump()


  //----------------------------------------
  val pipe = Module(new L1plusCachePipe)
  val missQueue = Module(new L1plusCacheMissQueue(edge))
  val resp_arb = Module(new Arbiter(new L1plusCacheResp, 2))

  val flush_block_req = Wire(Bool())
  val req_block = block_req(io.req.bits.addr) || flush_block_req
  block_decoupled(io.req, pipe.io.req, req_block)
  XSDebug(req_block, "Request blocked\n")

  pipe.io.data_read <> dataArray.io.read
  pipe.io.data_resp <> dataArray.io.resp
  pipe.io.meta_read <> metaArray.io.read
  pipe.io.meta_resp <> metaArray.io.resp
  pipe.io.miss_meta_write.valid := missQueue.io.meta_write.valid
  pipe.io.miss_meta_write.bits <> missQueue.io.meta_write.bits

  missQueue.io.req <> pipe.io.miss_req
  bus.a <> missQueue.io.mem_acquire
  missQueue.io.mem_grant <> bus.d
  metaArray.io.write <> missQueue.io.meta_write
  dataArray.io.write <> missQueue.io.refill

  // response
  io.resp           <> resp_arb.io.out
  resp_arb.io.in(0) <> pipe.io.resp
  resp_arb.io.in(1) <> missQueue.io.resp

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  // flush state machine
  val s_invalid :: s_drain_cache :: s_flush_cache :: s_send_resp :: Nil = Enum(4)
  val state = RegInit(s_invalid)

  switch (state) {
    is (s_invalid) {
      when (io.flush) {
        state := s_drain_cache
      }
    }
    is (s_drain_cache) {
      when (pipe.io.empty && missQueue.io.empty) {
        state := s_flush_cache
      }
    }
    is (s_flush_cache) {
      state := s_send_resp
    }
    is (s_send_resp) {
      state := s_invalid
    }
  }
  metaArray.io.flush := state === s_flush_cache
  io.empty := state === s_send_resp
  flush_block_req := state =/= s_invalid

  when (state =/= s_invalid) {
    XSDebug(s"L1plusCache flush state machine: %d\n", state)
  }

  // to simplify synchronization, we do not allow reqs with same indexes
  def block_req(addr: UInt) = {
    val pipe_idx_matches = VecInit(pipe.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val pipe_idx_match = pipe_idx_matches.reduce(_||_)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    pipe_idx_match || miss_idx_match
  }

  def block_decoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
  }

  // debug output
  when (io.req.valid) {
    XSDebug(s"L1plusCache req cmd: %x addr: %x id: %d\n",
      io.req.bits.cmd, io.req.bits.addr, io.req.bits.id)
  }

  when (io.resp.valid) {
    XSDebug(s"L1plusCache resp data: %x id: %d\n",
      io.resp.bits.data, io.resp.bits.id)
  }

  when (io.flush) {
    XSDebug(s"L1plusCache flush\n")
  }

  when (io.empty) {
    XSDebug(s"L1plusCache empty\n")
  }
}

class L1plusCachePipe extends L1plusCacheModule
{
  val io = IO(new L1plusCacheBundle{
    val req  = Flipped(DecoupledIO(new L1plusCacheReq))
    val resp = DecoupledIO(new L1plusCacheResp)
    val data_read  = DecoupledIO(new L1plusCacheDataReadReq)
    val data_resp  = Input(Vec(nWays, Vec(blockRows, Bits(encRowBits.W))))
    val meta_read  = DecoupledIO(new L1plusCacheMetaReadReq)
    val meta_resp  = Input(Vec(nWays, new L1plusCacheMetadata))
    val miss_req   = DecoupledIO(new L1plusCacheMissReq)
    val miss_meta_write = Flipped(ValidIO(new L1plusCacheMetaWriteReq))
    val inflight_req_idxes = Output(Vec(2, Valid(UInt())))
    val empty = Output(Bool())
  })

  val s0_passdown = Wire(Bool())
  val s1_passdown = Wire(Bool())
  val s2_passdown = Wire(Bool())

  val s0_valid = Wire(Bool())
  val s1_valid = Wire(Bool())
  val s2_valid = Wire(Bool())

  // requests
  val can_accept_req = !s1_valid || s1_passdown
  io.req.ready := io.meta_read.ready && io.data_read.ready && can_accept_req
  io.meta_read.valid := io.req.valid && can_accept_req
  io.data_read.valid := io.req.valid && can_accept_req

  val meta_read = io.meta_read.bits
  val data_read = io.data_read.bits

  // Tag read for new requests
  meta_read.tagIdx    := get_idx(io.req.bits.addr)
  meta_read.way_en := ~0.U(nWays.W)
  meta_read.tag    := DontCare
  // Data read for new requests
  data_read.addr   := io.req.bits.addr
  data_read.way_en := ~0.U(nWays.W)
  data_read.rmask  := ~0.U(blockRows.W)

  // Pipeline
  // stage 0
  s0_valid := io.req.fire()
  val s0_req = io.req.bits

  s0_passdown := s0_valid

  assert(!(s0_valid && s0_req.cmd =/= MemoryOpConstants.M_XRD && s0_req.cmd =/= MemoryOpConstants.M_PFR), "L1plusCachePipe only accepts read req")

  dump_pipeline_reqs("L1plusCachePipe s0", s0_valid, s0_req)
// stage 1
  val s1_req = RegEnable(s0_req, s0_passdown)
  val s1_valid_reg = RegEnable(s0_valid, init = false.B, enable = s0_passdown)
  val s1_addr = s1_req.addr
  when (s1_passdown && !s0_passdown) {
    s1_valid_reg := false.B
  }
  s1_valid := s1_valid_reg

  meta_read.validIdx  := get_idx(s1_addr)


  dump_pipeline_reqs("L1plusCachePipe s1", s1_valid, s1_req)

  val meta_resp = io.meta_resp
  // tag check
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta_resp(w).tag === (get_tag(s1_addr))).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && meta_resp(w).valid).asUInt

  s1_passdown := s1_valid && (!s2_valid || s2_passdown)

  // stage 2
  val s2_req   = RegEnable(s1_req, s1_passdown)
  val s2_valid_reg = RegEnable(s1_valid, init=false.B, enable=s1_passdown)
  when (s2_passdown && !s1_passdown) {
    s2_valid_reg := false.B
  }
  s2_valid := s2_valid_reg

  dump_pipeline_reqs("L1plusCachePipe s2", s2_valid, s2_req)

  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_passdown)
  val s2_hit           = s2_tag_match_way.orR
  val s2_hit_way       = OHToUInt(s2_tag_match_way, nWays)

  //replacement marker
  val replacer = cacheParams.replacement
  val (touch_sets, touch_ways) = ( Wire(Vec(plruAccessNum, UInt(log2Ceil(nSets).W))),  Wire(Vec(plruAccessNum, Valid(UInt(log2Ceil(nWays).W)))) )

  touch_sets(0)       := get_idx(s2_req.addr)  
  touch_ways(0).valid := s2_valid && s2_hit
  touch_ways(0).bits  := s2_hit_way

  replacer.access(touch_sets, touch_ways)

  val data_resp = io.data_resp
  val s2_data = data_resp(s2_hit_way)

  //TODO: only detect error but not correct it
  val s2_data_decoded = Cat((0 until blockRows).reverse map { r =>
      val data = s2_data(r)
      val decoded = cacheParams.dataCode.decode(data)
      decoded.uncorrected
    })

  val s2_data_wrong =  Cat((0 until blockRows).reverse map { r =>
      val data = s2_data(r)
      val decoded = cacheParams.dataCode.decode(data)
      assert(!(s2_valid && s2_hit && decoded.error))
      decoded.error
    })

  io.resp.valid     := s2_valid && s2_hit
  io.resp.bits.data := s2_data_decoded
  io.resp.bits.eccWrong := s2_data_wrong.asUInt.orR
  io.resp.bits.id   := s2_req.id

  // replacement policy
  val replaced_way_en = UIntToOH(replacer.way(get_idx(s2_req.addr)))

  io.miss_req.valid       := s2_valid && !s2_hit
  io.miss_req.bits.id     := s2_req.id
  io.miss_req.bits.cmd    := M_XRD
  io.miss_req.bits.addr   := s2_req.addr
  io.miss_req.bits.way_en := replaced_way_en

  val wayNum =  OHToUInt(io.miss_meta_write.bits.way_en.asUInt)
  touch_sets(1)       := io.miss_meta_write.bits.tagIdx
  touch_ways(1).valid := io.miss_meta_write.valid
  touch_ways(1).bits  := wayNum
  (0 until nWays).map{ w => 
    XSPerf("hit_way_" + Integer.toString(w, 10),  s2_valid && s2_hit && s2_hit_way === w.U)
    XSPerf("refill_way_" + Integer.toString(w, 10), io.miss_meta_write.valid && wayNum === w.U)
    XSPerf("access_way_" + Integer.toString(w, 10), (io.miss_meta_write.valid && wayNum === w.U) || (s2_valid && s2_hit && s2_hit_way === w.U))
  }

  s2_passdown := s2_valid && ((s2_hit && io.resp.ready) || (!s2_hit && io.miss_req.ready))

  val resp = io.resp
  when (resp.valid) {
    XSDebug(s"L1plusCachePipe resp: data: %x id: %d\n",
      resp.bits.data, resp.bits.id)
  }

  io.inflight_req_idxes(0).valid := s1_valid
  io.inflight_req_idxes(0).bits := get_idx(s1_req.addr)
  io.inflight_req_idxes(1).valid := s2_valid
  io.inflight_req_idxes(1).bits := get_idx(s2_req.addr)

  io.empty := !s0_valid && !s1_valid && !s2_valid

  // -------
  // Debug logging functions
  def dump_pipeline_reqs(pipeline_stage_name: String, valid: Bool, req: L1plusCacheReq) = {
      when (valid) {
        XSDebug(
          s"$pipeline_stage_name cmd: %x addr: %x id: %d\n",
          req.cmd, req.addr, req.id
        )
      }
  }

  XSPerf("req", s0_valid)
  XSPerf("miss", s2_valid && !s2_hit)

}

class L1plusCacheMissReq extends L1plusCacheBundle
{
  // transaction id
  val id     = UInt(idWidth.W)
  val cmd    = UInt(M_SZ.W)
  val addr   = UInt(PAddrBits.W)
  val way_en = UInt(nWays.W)
}

class L1plusCacheMissEntry(edge: TLEdgeOut) extends L1plusCacheModule
{
  val io = IO(new Bundle {
    val id          = Input(UInt())
    val req         = Flipped(DecoupledIO(new L1plusCacheMissReq))
    val resp        = DecoupledIO(new L1plusCacheResp)

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val meta_write  = DecoupledIO(new L1plusCacheMetaWriteReq)
    val refill      = DecoupledIO(new L1plusCacheDataWriteReq)

    val idx = Output(Valid(UInt()))
    val tag = Output(Valid(UInt()))
  })

  val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: s_data_write_req :: s_meta_write_req :: Nil = Enum(6)

  val state = RegInit(s_invalid)

  val req     = Reg(new L1plusCacheMissReq)
  val req_idx = get_idx(req.addr)
  val req_tag = get_tag(req.addr)

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)

  val refill_ctr  = Reg(UInt(log2Up(refillCycles).W))

  io.idx.valid := state =/= s_invalid
  io.tag.valid := state =/= s_invalid
  io.idx.bits  := req_idx
  io.tag.bits  := req_tag

  // assign default values to output signals
  io.req.ready           := false.B
  io.resp.valid          := false.B
  io.resp.bits           := DontCare

  io.mem_acquire.valid   := false.B
  io.mem_acquire.bits    := DontCare

  io.mem_grant.ready     := false.B

  io.meta_write.valid    := false.B
  io.meta_write.bits     := DontCare

  io.refill.valid        := false.B
  io.refill.bits         := DontCare

  when (state =/= s_invalid) {
    XSDebug("entry: %d state: %d\n", io.id, state)
    XSDebug("entry: %d idx_valid: %b idx: %x tag_valid: %b tag: %x\n",
      io.id, io.idx.valid, io.idx.bits, io.tag.valid, io.tag.bits)
  }


  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    io.req.ready := true.B

    when (io.req.fire()) {
      refill_ctr := 0.U
      req := io.req.bits
      state := s_refill_req
    }
  }

  // --------------------------------------------
  // refill
  when (state === s_refill_req) {
    io.mem_acquire.valid := true.B
    io.mem_acquire.bits  := edge.Get(
      fromSource      = io.id,
      toAddress       = req.addr,
      lgSize          = (log2Up(cfg.blockBytes)).U)._2
    when (io.mem_acquire.fire()) {
      state := s_refill_resp
    }
  }

  // not encoded data
  val refill_data_raw = Reg(Vec(blockRows, UInt(rowBits.W)))
  when (state === s_refill_resp) {
    io.mem_grant.ready := true.B

    when (edge.hasData(io.mem_grant.bits)) {
      when (io.mem_grant.fire()) {
        refill_ctr := refill_ctr + 1.U
        for (i <- 0 until beatRows) {
          val row = io.mem_grant.bits.data(rowBits * (i + 1) - 1, rowBits * i)
          refill_data_raw((refill_ctr << log2Floor(beatRows)) + i.U) := row
        }

        when (refill_ctr === (refillCycles - 1).U) {
          assert(refill_done, "refill not done!")
          state := s_send_resp
        }
      }
    }
  }

  // --------------------------------------------
  when (state === s_send_resp) {

    val resp_data = Cat((0 until blockRows).reverse map { r => refill_data_raw(r) })
    io.resp.valid     := true.B
    io.resp.bits.data := resp_data
    io.resp.bits.id   := req.id

    when (io.resp.fire()) {
      state := s_data_write_req
    }
  }

  // --------------------------------------------
  // data write
  when (state === s_data_write_req) {
    io.refill.valid        := true.B
    io.refill.bits.addr    := req.addr
    io.refill.bits.way_en  := req.way_en
    io.refill.bits.wmask   := ~0.U(blockRows.W)
    io.refill.bits.rmask   := DontCare
    io.refill.bits.data    := refill_data_raw.map(row => cacheParams.dataCode.encode(row))

    when (io.refill.fire()) {
      state := s_meta_write_req
    }
  }

  // --------------------------------------------
  // meta write
  when (state === s_meta_write_req) {
    io.meta_write.valid           := true.B
    io.meta_write.bits.tagIdx        := req_idx
    io.meta_write.bits.validIdx  := req_idx
    io.meta_write.bits.data.valid := true.B
    io.meta_write.bits.data.tag   := req_tag
    io.meta_write.bits.way_en     := req.way_en

    when (io.meta_write.fire()) {
      state := s_invalid
    }
  }
}

class L1plusCacheMissQueue(edge: TLEdgeOut) extends L1plusCacheModule with HasTLDump
{
  val io = IO(new Bundle {
    val req         = Flipped(DecoupledIO(new L1plusCacheMissReq))
    val resp        = DecoupledIO(new L1plusCacheResp)

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val meta_write  = DecoupledIO(new L1plusCacheMetaWriteReq)
    val refill      = DecoupledIO(new L1plusCacheDataWriteReq)
    val inflight_req_idxes = Output(Vec(cfg.nMissEntries, Valid(UInt())))
    val empty       = Output(Bool())
  })

  val resp_arb       = Module(new Arbiter(new L1plusCacheResp,           cfg.nMissEntries))
  val meta_write_arb = Module(new Arbiter(new L1plusCacheMetaWriteReq,   cfg.nMissEntries))
  val refill_arb     = Module(new Arbiter(new L1plusCacheDataWriteReq,   cfg.nMissEntries))

  // assign default values to output signals
  io.mem_grant.ready := false.B

  val idx_matches = Wire(Vec(cfg.nMissEntries, Bool()))
  val tag_matches = Wire(Vec(cfg.nMissEntries, Bool()))

  val tag_match   = Mux1H(idx_matches, tag_matches)
  val idx_match   = idx_matches.reduce(_||_)

  val req             = io.req
  val entry_alloc_idx = Wire(UInt())
  val pri_rdy         = WireInit(false.B)
  val pri_val         = req.valid && !idx_match

  val entries = (0 until cfg.nMissEntries) map { i =>
    val entry = Module(new L1plusCacheMissEntry(edge))

    entry.io.id := i.U(missQueueEntryIdWidth.W)

    idx_matches(i) := entry.io.idx.valid && entry.io.idx.bits === get_idx(req.bits.addr)
    tag_matches(i) := entry.io.tag.valid && entry.io.tag.bits === get_tag(req.bits.addr)
    io.inflight_req_idxes(i) <> entry.io.idx

    // req and resp
    entry.io.req.valid := (i.U === entry_alloc_idx) && pri_val
    when (i.U === entry_alloc_idx) {
      pri_rdy := entry.io.req.ready
    }
    entry.io.req.bits  := req.bits

    resp_arb.io.in(i)  <> entry.io.resp

    // tilelink
    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      entry.io.mem_grant <> io.mem_grant
    }

    // meta and data write
    meta_write_arb.io.in(i) <>  entry.io.meta_write
    refill_arb.io.in(i)     <>  entry.io.refill

    entry
  }

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.req.ready))

  // whenever index matches, do not let it in
  req.ready     := pri_rdy && !idx_match
  io.resp       <> resp_arb.io.out
  io.meta_write <> meta_write_arb.io.out
  io.refill     <> refill_arb.io.out

  TLArbiter.lowestFromSeq(edge, io.mem_acquire, entries.map(_.io.mem_acquire))

  io.empty := VecInit(entries.map(m=>m.io.req.ready)).asUInt.andR

  // print all input/output requests for debug purpose
  // print req
  XSDebug(req.fire(), "req id: %d cmd: %x addr: %x way_en: %x\n",
    req.bits.id, req.bits.cmd, req.bits.addr, req.bits.way_en)

  val resp = io.resp
  XSDebug(resp.fire(), s"resp: data: %x id: %d\n",
    resp.bits.data, resp.bits.id)

  // print refill
  XSDebug(io.refill.fire(), "refill addr %x\n", io.refill.bits.addr)

  // print meta_write
  XSDebug(io.meta_write.fire(), "meta_write idx %x way_en: %x old_tag: %x new_valid: %d new_tag: %x\n",
    io.meta_write.bits.tagIdx, io.meta_write.bits.way_en, io.meta_write.bits.tag,
    io.meta_write.bits.data.valid, io.meta_write.bits.data.tag)

  // print tilelink messages
  when (io.mem_acquire.fire()) {
    XSDebug("mem_acquire ")
    io.mem_acquire.bits.dump
  }
  when (io.mem_grant.fire()) {
    XSDebug("mem_grant ")
    io.mem_grant.bits.dump
  }
}
