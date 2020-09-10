package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, TLClientParameters, TLEdgeOut}
import utils.{Code, RandomReplacement, XSDebug}

import scala.math.max


// DCache specific parameters
// L1 DCache is 64set, 8way-associative, with 64byte block, a total of 32KB
// It's a virtually indexed, physically tagged cache.
case class DCacheParameters
(
    nSets: Int = 64,
    nWays: Int = 8,
    rowBits: Int = 64,
    numDCacheBanks: Int = 2,
    nTLBEntries: Int = 32,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    nMissEntries: Int = 1,
    nLoadMissEntries: Int = 1,
    nStoreMissEntries: Int = 1,
    nMiscMissEntries: Int = 1,
    nMMIOEntries: Int = 1,
    nSDQ: Int = 17,
    nRPQ: Int = 16,
    nMMIOs: Int = 1,
    blockBytes: Int = 64
) extends L1CacheParameters {

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

  def get_beat(addr: UInt) = addr(blockOffBits - 1, beatOffBits)
  def get_tag(addr: UInt) = (addr >> untagBits).asUInt()
  def get_idx(addr: UInt) = addr(untagBits-1, blockOffBits)
  def get_block(addr: UInt) = addr >> blockOffBits
  def get_block_addr(addr: UInt) = (addr >> blockOffBits) << blockOffBits

  def rowWords = rowBits/wordBits
  def doNarrowRead = DataBits * nWays % rowBits == 0
  def encDataBits = cacheParams.dataCode.width(wordBits) // NBDCache only
  def encRowBits = encDataBits*rowWords
  def lrscCycles = LRSCCycles // ISA requires 16-insn LRSC sequences to succeed
  def lrscBackoff = 3 // disallow LRSC reacquisition briefly
  def blockProbeAfterGrantCycles = 8 // give the processor some time to issue a request after a grant
  def nIOMSHRs = cacheParams.nMMIOs
  def maxUncachedInFlight = cacheParams.nMMIOs

  def missQueueEntryIdWidth = log2Up(cfg.nMissEntries)
  def loadMissQueueEntryIdWidth = log2Up(cfg.nLoadMissEntries)
  def storeMissQueueEntryIdWidth = log2Up(cfg.nStoreMissEntries)
  def miscMissQueueEntryIdWidth = log2Up(cfg.nMiscMissEntries)
  def clientMissQueueEntryIdWidth = max(
    max(loadMissQueueEntryIdWidth,
      storeMissQueueEntryIdWidth),
      miscMissQueueEntryIdWidth)

  def nClientMissQueues = 3
  def clientIdWidth = log2Up(nClientMissQueues)
  def missQueueClientIdWidth = clientIdWidth + clientMissQueueEntryIdWidth
  def clientIdMSB = missQueueClientIdWidth - 1
  def clientIdLSB = clientMissQueueEntryIdWidth
  def entryIdMSB = clientMissQueueEntryIdWidth - 1
  def entryIdLSB = 0
  def reqIdWidth = 64

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
  // you can choose which beat to read to save power
  val rmask  = Bits(refillCycles.W)
  val way_en = Bits(nWays.W)
  val addr   = Bits(untagBits.W)
}

// Now, we can write a cache-block in a single cycle
class L1DataWriteReq extends L1DataReadReq {
  val wmask  = Vec(refillCycles, Bits(rowWords.W))
  val data   = Vec(refillCycles, Bits(encRowBits.W))
}

abstract class AbstractDataArray extends DCacheModule {
  val io = IO(new DCacheBundle {
    val read  = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1DataReadReq)))
    val write = Flipped(DecoupledIO(new L1DataWriteReq))
    val resp  = Output(Vec(LoadPipelineWidth, Vec(nWays, Vec(refillCycles, Bits(encRowBits.W)))))
    val nacks = Output(Vec(LoadPipelineWidth, Bool()))
  })

  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until LoadPipelineWidth).map(f))

  def dumpRead() = {
    (0 until LoadPipelineWidth) map { w =>
      when (io.read(w).valid) {
        XSDebug(s"DataArray Read channel: $w valid way_en: %x addr: %x\n",
          io.read(w).bits.way_en, io.read(w).bits.addr)
      }
    }
  }

  def dumpWrite() = {
    when (io.write.valid) {
      XSDebug(s"DataArray Write valid way_en: %x addr: %x\n",
        io.write.bits.way_en, io.write.bits.addr)

      (0 until refillCycles) map { r =>
        XSDebug(s"cycle: $r data: %x wmask: %x\n",
          io.write.bits.data(r), io.write.bits.wmask(r))
      }
    }
  }

  def dumpResp() = {
    (0 until LoadPipelineWidth) map { w =>
      XSDebug(s"DataArray ReadResp channel: $w\n")
      (0 until nWays) map { i =>
        (0 until refillCycles) map { r =>
          XSDebug(s"way: $i cycle: $r data: %x\n", io.resp(w)(i)(r))
        }
      }
    }
  }

  def dumpNack() = {
    (0 until LoadPipelineWidth) map { w =>
      when (io.nacks(w)) {
        XSDebug(s"DataArray NACK channel: $w\n")
      }
    }
  }

  def dump() = {
    dumpRead
    dumpWrite
    dumpNack
    dumpResp
  }
}

class DuplicatedDataArray extends AbstractDataArray
{
  // write is always ready
  io.write.ready := true.B
  val waddr = (io.write.bits.addr >> blockOffBits).asUInt()
  for (j <- 0 until LoadPipelineWidth) {
    val raddr = (io.read(j).bits.addr >> blockOffBits).asUInt()
    // raddr === waddr is undefined behavior!
    // block read in this case
    io.read(j).ready := !io.write.valid || raddr =/= waddr
    for (w <- 0 until nWays) {
      for (r <- 0 until refillCycles) {
        val array = SyncReadMem(nSets, Vec(rowWords, Bits(encDataBits.W)))
        // data write
        when (io.write.bits.way_en(w) && io.write.valid) {
          val data = VecInit((0 until rowWords) map (i => io.write.bits.data(r)(encDataBits*(i+1)-1,encDataBits*i)))
          array.write(waddr, data, io.write.bits.wmask(r).asBools)
        }
        // data read
        io.resp(j)(w)(r) := RegNext(array.read(raddr, io.read(j).bits.way_en(w)
          && io.read(j).bits.rmask(r) && io.read(j).valid).asUInt)
      }
    }
    io.nacks(j) := false.B
  }
}

class L1MetadataArray(onReset: () => L1Metadata) extends DCacheModule {
  val rstVal = onReset()
  val io = IO(new Bundle {
    val read = Flipped(Decoupled(new L1MetaReadReq))
    val write = Flipped(Decoupled(new L1MetaWriteReq))
    val resp = Output(Vec(nWays, new L1Metadata))
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

  io.read.ready := !wen
  io.write.ready := !rst

  def dumpRead() = {
    when (io.read.fire()) {
      XSDebug("MetaArray Read: idx: %d way_en: %x tag: %x\n",
        io.read.bits.idx, io.read.bits.way_en, io.read.bits.tag)
    }
  }

  def dumpWrite() = {
    when (io.write.fire()) {
      XSDebug("MetaArray Write: idx: %d way_en: %x tag: %x new_tag: %x new_coh: %x\n",
        io.write.bits.idx, io.write.bits.way_en, io.write.bits.tag, io.write.bits.data.tag, io.write.bits.data.coh.state)
    }
  }

  def dumpResp() = {
    (0 until nWays) map { i =>
      XSDebug(s"MetaArray Resp: way: $i tag: %x coh: %x\n",
        io.resp(i).tag, io.resp(i).coh.state)
    }
  }

  def dump() = {
    dumpRead
    dumpWrite
    dumpResp
  }
}

class DuplicatedMetaArray extends DCacheModule {
  val io = IO(new DCacheBundle {
    val read  = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1MetaReadReq)))
    val write = Flipped(DecoupledIO(new L1MetaWriteReq))
    val resp  = Output(Vec(LoadPipelineWidth, Vec(nWays, new L1Metadata)))
  })

  def onReset = L1Metadata(0.U, ClientMetadata.onReset)
  val meta = Seq.fill(LoadPipelineWidth) { Module(new L1MetadataArray(onReset _)) }

  for (w <- 0 until LoadPipelineWidth) {
    meta(w).io.write <> io.write
    meta(w).io.read  <> io.read(w)
    io.resp(w) <> meta(w).io.resp
  }

  def dumpRead() = {
    (0 until LoadPipelineWidth) map { w =>
      when (io.read(w).fire()) {
        XSDebug("MetaArray Read channel: $w idx: %d way_en: %x tag: %x\n",
          io.read(w).bits.idx, io.read(w).bits.way_en, io.read(w).bits.tag)
      }
    }
  }

  def dumpWrite() = {
    when (io.write.fire()) {
      XSDebug("MetaArray Write: idx: %d way_en: %x tag: %x new_tag: %x new_coh: %x\n",
        io.write.bits.idx, io.write.bits.way_en, io.write.bits.tag, io.write.bits.data.tag, io.write.bits.data.coh.state)
    }
  }

  def dumpResp() = {
    (0 until LoadPipelineWidth) map { w =>
      (0 until nWays) map { i =>
        XSDebug(s"MetaArray Resp: channel: $w way: $i tag: %x coh: %x\n",
          io.resp(w)(i).tag, io.resp(w)(i).coh.state)
      }
    }
  }

  def dump() = {
    dumpRead
    dumpWrite
    dumpResp
  }
}
