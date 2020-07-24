package xiangshan.mem.cache

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import xiangshan.mem.{DCacheResp, LSUDMemIO}
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

abstract class DCacheModule extends Module
  with HasDCacheParameters

abstract class DCacheBundle extends Bundle
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
  val io = new Bundle {
    val read = Flipped(Decoupled(new L1MetaReadReq))
    val write = Flipped(Decoupled(new L1MetaWriteReq))
    val resp = Output(Vec(nWays, rstVal.cloneType))
  }
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
    val bus = Flipped(new TLCached(cfg.busParams))
  })

  def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))

  // 首先就是流水线上有若干种不同的处理状态，分别是replay，probe，wb之类的
  // 但是里面有些状态我是不太能理解的。
  // 我感觉只有lsu和replay需要上流水线其他的都不需要吧？
  // 另外，lsu和replay上流水线，直接一样处理就好了？也不需要区分吧？
  // 可能还是需要区分的，因为replay时只有一个？
  // val t_replay :: t_probe :: t_wb :: t_mshr_meta_read :: t_lsu :: t_prefetch :: Nil = Enum(6)

  val wb = Module(new WritebackUnit)
  // val prober = Module(new ProbeUnit)
  val mshrs = Module(new MSHRFile)
  // mshrs接受一个clear all信号，用来把mshr清空
  // 这个是哈？
  // mshr为啥要知道rob index呢？
  // tags
  def onReset = L1Metadata(0.U, ClientMetadata.onReset)
  val meta = Seq.fill(memWidth) { Module(new L1MetadataArray(onReset _)) }
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 2))
  // 0 goes to MSHR refills, 1 goes to prober
  val metaReadArb = Module(new Arbiter(new DCacheMetaReadReq, 6))
  // 0 goes to MSHR replays, 1 goes to prober, 2 goes to wb, 3 goes to MSHR meta read,
  // 4 goes to pipeline, 5 goes to prefetcher

  metaReadArb.io.in := DontCare
  for (w <- 0 until memWidth) {
    meta(w).io.write.valid := metaWriteArb.io.out.fire()
    meta(w).io.write.bits  := metaWriteArb.io.out.bits
    meta(w).io.read.valid  := metaReadArb.io.out.valid
    meta(w).io.read.bits   := metaReadArb.io.out.bits.req(w)
  }

  metaReadArb.io.out.ready  := meta.map(_.io.read.ready).reduce(_||_)
  metaWriteArb.io.out.ready := meta.map(_.io.write.ready).reduce(_||_)

  // data
  val data = Module(if (numDCacheBanks == 1) new DuplicatedDataArray else new BankedDataArray)
  val dataWriteArb = Module(new Arbiter(new L1DataWriteReq, 2))
  // 0 goes to pipeline, 1 goes to MSHR refills
  val dataReadArb = Module(new Arbiter(new DCacheDataReadReq, 3))
  // 0 goes to MSHR replays, 1 goes to wb, 2 goes to pipeline
  dataReadArb.io.in := DontCare

  for (w <- 0 until memWidth) {
    data.io.read(w).valid := dataReadArb.io.out.bits.valid(w) && dataReadArb.io.out.valid
    data.io.read(w).bits  := dataReadArb.io.out.bits.req(w)
  }
  dataReadArb.io.out.ready := true.B

  data.io.write.valid := dataWriteArb.io.out.fire()
  data.io.write.bits  := dataWriteArb.io.out.bits
  dataWriteArb.io.out.ready := true.B

  // ------------
  // New requests

  io.lsu.req.ready := metaReadArb.io.in(4).ready && dataReadArb.io.in(2).ready
  metaReadArb.io.in(4).valid := io.lsu.req.valid
  dataReadArb.io.in(2).valid := io.lsu.req.valid
  for (w <- 0 until memWidth) {
    // Tag read for new requests
    metaReadArb.io.in(4).bits.req(w).idx    := io.lsu.req.bits(w).bits.addr >> blockOffBits
    metaReadArb.io.in(4).bits.req(w).way_en := DontCare
    metaReadArb.io.in(4).bits.req(w).tag    := DontCare
    // Data read for new requests
    dataReadArb.io.in(2).bits.valid(w)      := io.lsu.req.bits(w).valid
    dataReadArb.io.in(2).bits.req(w).addr   := io.lsu.req.bits(w).bits.addr
    // way en到底是啥？
    dataReadArb.io.in(2).bits.req(w).way_en := ~0.U(nWays.W)
  }

  // 最最一开始的部分是抢metadata以及data的部分，应该是主要的同步点
  // ------------
  // MSHR Replays
  /*
  val replay_req = Wire(Vec(memWidth, new DCacheReq))
  replay_req               := DontCare
  replay_req(0).uop        := mshrs.io.replay.bits.uop
  replay_req(0).addr       := mshrs.io.replay.bits.addr
  replay_req(0).data       := mshrs.io.replay.bits.data
  replay_req(0).is_hella   := mshrs.io.replay.bits.is_hella
  // mshr也是等meta以及data就绪了就开始replay？
  mshrs.io.replay.ready    := metaReadArb.io.in(0).ready && dataReadArb.io.in(0).ready
  // Tag read for MSHR replays
  // We don't actually need to read the metadata, for replays we already know our way
  metaReadArb.io.in(0).valid              := mshrs.io.replay.valid
  metaReadArb.io.in(0).bits.req(0).idx    := mshrs.io.replay.bits.addr >> blockOffBits
  metaReadArb.io.in(0).bits.req(0).way_en := DontCare
  metaReadArb.io.in(0).bits.req(0).tag    := DontCare
  // Data read for MSHR replays
  dataReadArb.io.in(0).valid              := mshrs.io.replay.valid
  dataReadArb.io.in(0).bits.req(0).addr   := mshrs.io.replay.bits.addr
  dataReadArb.io.in(0).bits.req(0).way_en := mshrs.io.replay.bits.way_en
  dataReadArb.io.in(0).bits.valid         := widthMap(w => (w == 0).B)
  */

  // -----------
  // MSHR Meta read
  /*
  val mshr_read_req = Wire(Vec(memWidth, new DCacheReq))
  mshr_read_req             := DontCare
  mshr_read_req(0).uop      := NullMicroOp
  mshr_read_req(0).addr     := Cat(mshrs.io.meta_read.bits.tag, mshrs.io.meta_read.bits.idx) << blockOffBits
  mshr_read_req(0).data     := DontCare
  mshr_read_req(0).is_hella := false.B
  metaReadArb.io.in(3).valid       := mshrs.io.meta_read.valid
  metaReadArb.io.in(3).bits.req(0) := mshrs.io.meta_read.bits
  mshrs.io.meta_read.ready         := metaReadArb.io.in(3).ready
  */



  // -----------
  // Write-backs
  /*
  val wb_fire = wb.io.meta_read.fire() && wb.io.data_req.fire()
  val wb_req = Wire(Vec(memWidth, new DCacheReq))
  wb_req             := DontCare
  wb_req(0).uop      := NullMicroOp
  wb_req(0).addr     := Cat(wb.io.meta_read.bits.tag, wb.io.data_req.bits.addr)
  wb_req(0).data     := DontCare
  wb_req(0).is_hella := false.B
  // Couple the two decoupled interfaces of the WBUnit's meta_read and data_read
  // Tag read for write-back
  metaReadArb.io.in(2).valid        := wb.io.meta_read.valid
  metaReadArb.io.in(2).bits.req(0)  := wb.io.meta_read.bits
  wb.io.meta_read.ready := metaReadArb.io.in(2).ready && dataReadArb.io.in(1).ready
  // Data read for write-back
  dataReadArb.io.in(1).valid        := wb.io.data_req.valid
  dataReadArb.io.in(1).bits.req(0)  := wb.io.data_req.bits
  dataReadArb.io.in(1).bits.valid   := widthMap(w => (w == 0).B)
  wb.io.data_req.ready  := metaReadArb.io.in(2).ready && dataReadArb.io.in(1).ready
  assert(!(wb.io.meta_read.fire() ^ wb.io.data_req.fire()))
  */

  // -------
  // Prober
  /*
  val prober_fire  = prober.io.meta_read.fire()
  val prober_req   = Wire(Vec(memWidth, new DCacheReq))
  prober_req             := DontCare
  prober_req(0).uop      := NullMicroOp
  prober_req(0).addr     := Cat(prober.io.meta_read.bits.tag, prober.io.meta_read.bits.idx) << blockOffBits
  prober_req(0).data     := DontCare
  prober_req(0).is_hella := false.B
  // Tag read for prober
  metaReadArb.io.in(1).valid       := prober.io.meta_read.valid
  metaReadArb.io.in(1).bits.req(0) := prober.io.meta_read.bits
  prober.io.meta_read.ready := metaReadArb.io.in(1).ready
  // Prober does not need to read data array
  */

  // -------
  // Prefetcher
  /*
  val prefetch_fire = mshrs.io.prefetch.fire()
  val prefetch_req  = Wire(Vec(memWidth, new DCacheReq))
  prefetch_req    := DontCare
  prefetch_req(0) := mshrs.io.prefetch.bits
  // Tag read for prefetch
  metaReadArb.io.in(5).valid              := mshrs.io.prefetch.valid
  metaReadArb.io.in(5).bits.req(0).idx    := mshrs.io.prefetch.bits.addr >> blockOffBits
  metaReadArb.io.in(5).bits.req(0).way_en := DontCare
  metaReadArb.io.in(5).bits.req(0).tag    := DontCare
  mshrs.io.prefetch.ready := metaReadArb.io.in(5).ready
  // Prefetch does not need to read data array
  */

  // 这边主流水线是多个模块在竞争，谁先抢到了，谁就上线
  // MSHR以及其他的都只用port 0
  // MSHR read req和replay req怎么用到了同一个口？
  // 可能他们是都需要占用一段路，然后重合了？
  // 我总感觉这的一系列mux，其实就是暗示了优先级啊？
  // 问题：它们的fire是怎么排序的呢？
  val s0_valid = Mux(io.lsu.req.fire(), VecInit(io.lsu.req.bits.map(_.valid)),
    VecInit(Seq(false.B, false.B)))
  val s0_req   = VecInit(io.lsu.req.bits.map(_.bits))
  // 根据不同的请求有不同的处理吗？
  val s0_type  = 0.U

  // Does this request need to send a response or nack
  // 啥意思
  // MSHR read和lsu的请求是需要发response以及nack的？
  // 问题：LSU write也要吗？
  // write暂时先不发送response
  val s0_send_resp_or_nack = Mux(io.lsu.req.fire(), s0_valid,
    VecInit(Seq(false.B, false.B)))


  val s1_req = RegNext(s0_req)
  // 我们暂时不支持branch kill
  // br mask是啥？
  // 这边要允许branch被kill
  // 这边允许请求被branch kill是怎么处理的呢？
  // 这个又是啥？
  val s2_store_failed = Wire(Bool())
  // 这边的exception是啥？
  // 这边不应该出任何exception吧？
  val s1_valid = widthMap(w => RegNext(s0_valid(w), init=false.B))
  val s1_addr = s1_req.map(_.addr)
  // 不是，这个的意思是，prober io req not ready，意思是现在有正在处理的probe的请求，假如要写的meta idx正好冲突，那就s1 nack
  // 问题：s1_nack是在哪里用上的啊？
  // 所以s1这边是直接send response或者nack
  val s1_send_resp_or_nack = RegNext(s0_send_resp_or_nack)
  val s1_type         = RegNext(s0_type)

  // 这些是其他几条线要用到的私有数据
  // 也就是这三个用的是自己的way en

  // tag check
  // 这边分为way map和width map
  // way map是只搞好每个way
  // width map，是针对每个memWidth，都处理一遍
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  // 这是个啥鬼东西啊？
  // 这个其实就是一个两层循环
  // 假如拿imperative的模式来写，其实就是先定义一个tag eq way的变量
  // 然后对于它的每个bit，搞一个小循环来生成
  // 这个生成的是每个way是否eq的bitvector
  val s1_tag_eq_way = widthMap(i => wayMap((w: Int) => meta(i).io.resp(w).tag === (s1_addr(i) >> untagBits)).asUInt)
  // 这边是检查的时候，不仅要考虑到自己读出来是不是valid，还要考虑到replay，wb，mshr meta read等自带way en的家伙
  // 我们这里暂时不需要这个，我们暂时只需要能先检查tag，再检查coherence是不是hit就可以了。
  val s1_tag_match_way = widthMap(i => wayMap((w: Int) => s1_tag_eq_way(i)(w) && meta(i).io.resp(w).coh.isValid()).asUInt)

  val s2_req   = RegNext(s1_req)
  val s2_type  = RegNext(s1_type)
  val s2_valid = widthMap(w =>
                  RegNext(s1_valid(w), init = false.B))

  // 哪些way match了
  val s2_tag_match_way = RegNext(s1_tag_match_way)
  // 具体是哪个way match了？
  val s2_tag_match     = s2_tag_match_way.map(_.orR)
  // 这边是得到hit way的coh
  val s2_hit_state     = widthMap(i => Mux1H(s2_tag_match_way(i), wayMap((w: Int) => RegNext(meta(i).io.resp(w).coh))))
  // 检查是否有权限可以访问
  val s2_has_permission = widthMap(w => s2_hit_state(w).onAccess(s2_req(w).cmd)._1)
  // 老的state，和处理完后的新state
  // 我有点不太明白的是，假如lsu同时下来的是两写怎么处理啊？还是现在boom下来的只能一读一写？
  // 写肯定只能有一个，而且肯定是在固定的口的
  val s2_new_hit_state  = widthMap(w => s2_hit_state(w).onAccess(s2_req(w).cmd)._3)

  // replay和write back肯定hit
  // mshr还要有能力能block hit
  // 这边还要求hit之前的状态等于hit之后的状态是啥意思呢？
  // 不应该有权限就行了嘛？
  // 这边不仅要有权限，由于这边不能进行meta write，所以假如不需要改meta，那是最好不过的了。
  val s2_hit = widthMap(w => (s2_tag_match(w) && s2_has_permission(w) && s2_hit_state(w) === s2_new_hit_state(w)))

  val s2_data = Wire(Vec(memWidth, Vec(nWays, UInt(encRowBits.W))))
  for (i <- 0 until memWidth) {
    for (w <- 0 until nWays) {
      s2_data(i)(w) := data.io.resp(i)(w)
    }
  }

  val s2_data_muxed = widthMap(w => Mux1H(s2_tag_match_way(w), s2_data(w)))
  // s2_word_idx应该就是word在row里面的index
  val s2_word_idx   = widthMap(w => if (rowWords == 1) 0.U else s2_req(w).addr(log2Up(rowWords*wordBytes)-1, log2Up(wordBytes)))

  // replacement policy
  // replace似乎是在s1的时候出结果？
  val replacer = cacheParams.replacement
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegNext(replacer.way))
  val s2_repl_meta = widthMap(i => Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegNext(meta(i).io.resp(w))).toSeq))

  val s2_send_resp = widthMap(w => RegNext(s1_send_resp_or_nack(w)) && s2_hit(w))

  // hits always send a response
  // If MSHR is not available, LSU has to replay this request later
  // If MSHR is available and this is only a store(not a amo), we don't need to wait for resp later
  // AMO还是要send response的。
  // 对于store，假如MSHR not available，就要把它给堵住。

  // load data gen
  val s2_data_word_prebypass = widthMap(w => s2_data_muxed(w) >> Cat(s2_word_idx(w), 0.U(log2Ceil(wordBits).W)))
  val s2_data_word = Wire(Vec(memWidth, UInt()))

  // Mux between cache responses and uncache responses
  val cache_resp = Wire(Vec(memWidth, Valid(new DCacheResp)))
  for (w <- 0 until memWidth) {
    cache_resp(w).valid         := s2_valid(w) && s2_send_resp(w)
    cache_resp(w).bits.data     := s2_data_word(w)
  }

  val resp = WireInit(cache_resp)

  // 返回结果
  for (w <- 0 until memWidth) {
    io.lsu.resp(w).valid := resp(w).valid
    io.lsu.resp(w).bits.data := resp(w).bits
  }

  // Store/amo hits
  val s3_req   = RegNext(s2_req(0))
  val s3_valid = RegNext(s2_valid(0) && s2_hit(0) && isWrite(s2_req(0).cmd))
  for (w <- 1 until memWidth) {
    assert(!(s2_valid(w) && s2_hit(w) && isWrite(s2_req(w).cmd)),
      "Store must go through 0th pipe in L1D")
  }

  // For bypassing
  val s4_req   = RegNext(s3_req)
  val s4_valid = RegNext(s3_valid)
  val s5_req   = RegNext(s4_req)
  val s5_valid = RegNext(s4_valid)

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
  amoalu.io.mask := new StoreGen(0.U, s2_req(0).addr, 0.U, DataBits/8).mask
  amoalu.io.cmd  := s2_req(0).cmd
  // 这边看着就是把lhs和rhs中间拿op算一下
  amoalu.io.lhs  := s2_data_word(0)
  amoalu.io.rhs  := s2_req(0).data


  s3_req.data := amoalu.io.out
  val s3_way   = RegNext(s2_tag_match_way(0))

  dataWriteArb.io.in(0).valid       := s3_valid
  dataWriteArb.io.in(0).bits.addr   := s3_req.addr
  dataWriteArb.io.in(0).bits.wmask  := UIntToOH(s3_req.addr(rowOffBits-1,offsetlsb))
  dataWriteArb.io.in(0).bits.data   := Fill(rowWords, s3_req.data)
  dataWriteArb.io.in(0).bits.way_en := s3_way
}

class DcacheUserBundle extends Bundle
