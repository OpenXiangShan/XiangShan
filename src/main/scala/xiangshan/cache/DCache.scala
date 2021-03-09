package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, TLClientParameters, TLEdgeOut}
import utils.{Code, ReplacementPolicy, XSDebug, SRAMTemplate, ParallelOR}

import scala.math.max


// DCache specific parameters
// L1 DCache is 64set, 8way-associative, with 64byte block, a total of 32KB
// It's a virtually indexed, physically tagged cache.
case class DCacheParameters
(
    nSets: Int = 64,
    nWays: Int = 8,
    rowBits: Int = 128,
    nTLBEntries: Int = 32,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    replacer: Option[String] = Some("random"),
    nMissEntries: Int = 1,
    nProbeEntries: Int = 1,
    nReleaseEntries: Int = 1,
    nStoreReplayEntries: Int = 1,
    nMMIOEntries: Int = 1,
    nMMIOs: Int = 1,
    blockBytes: Int = 64
) extends L1CacheParameters {

  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = ReplacementPolicy.fromString(replacer, nWays, nSets)
}

trait HasDCacheParameters extends HasL1CacheParameters {
  val cacheParams = dcacheParameters
  val cfg = cacheParams

  def encWordBits = cacheParams.dataCode.width(wordBits)
  def encRowBits = encWordBits*rowWords
  def lrscCycles = LRSCCycles // ISA requires 16-insn LRSC sequences to succeed
  def lrscBackoff = 3 // disallow LRSC reacquisition briefly
  def blockProbeAfterGrantCycles = 8 // give the processor some time to issue a request after a grant
  def nIOMSHRs = cacheParams.nMMIOs
  def maxUncachedInFlight = cacheParams.nMMIOs

  def nSourceType = 3
  def sourceTypeWidth = log2Up(nSourceType)
  def LOAD_SOURCE = 0
  def STORE_SOURCE = 1
  def AMO_SOURCE = 2
  // each source use a id to distinguish its multiple reqs
  def reqIdWidth = 64

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
  require(full_divide(rowBits, wordBits), s"rowBits($rowBits) must be multiple of wordBits($wordBits)")
  require(full_divide(beatBits, rowBits), s"beatBits($beatBits) must be multiple of rowBits($rowBits)")
  // this is a VIPT L1 cache
  require(pgIdxBits >= untagBits, s"page aliasing problem: pgIdxBits($pgIdxBits) < untagBits($untagBits)")
  // require(rowWords == 1, "Our DCache Implementation assumes rowWords == 1")
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
  // you can choose which bank to read to save power
  val rmask  = Bits(blockRows.W)
  val way_en = Bits(nWays.W)
  val addr   = Bits(untagBits.W)
}

// Now, we can write a cache-block in a single cycle
class L1DataWriteReq extends L1DataReadReq {
  val wmask  = Bits(blockRows.W)
  val data   = Vec(blockRows, Bits(rowBits.W))
}

class ReplacementAccessBundle extends DCacheBundle {
  val set = UInt(log2Up(nSets).W)
  val way = UInt(log2Up(nWays).W)
}

abstract class AbstractDataArray extends DCacheModule {
  val io = IO(new DCacheBundle {
    val read  = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1DataReadReq)))
    val write = Flipped(DecoupledIO(new L1DataWriteReq))
    val resp = Output(Vec(LoadPipelineWidth, Vec(blockRows, Bits(encRowBits.W))))
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

      (0 until blockRows) map { r =>
        XSDebug(s"cycle: $r data: %x wmask: %x\n",
          io.write.bits.data(r), io.write.bits.wmask(r))
      }
    }
  }

  def dumpResp() = {
    (0 until LoadPipelineWidth) map { w =>
      XSDebug(s"DataArray ReadResp channel: $w\n")
      (0 until blockRows) map { r =>
        XSDebug(s"cycle: $r data: %x\n", io.resp(w)(r))
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

class DuplicatedDataArray extends AbstractDataArray {
  val singlePort = true
  val readHighPriority = false
  def eccBits = encWordBits - wordBits

  def getECCFromEncWord(encWord: UInt) = {
    require(encWord.getWidth == encWordBits)
    encWord(encWordBits - 1, wordBits)
  }

  def getECCFromRow(row: UInt) = {
    require(row.getWidth == rowBits)
    VecInit((0 until rowWords).map { w =>
      val word = row(wordBits * (w + 1) - 1, wordBits * w)
      getECCFromEncWord(cacheParams.dataCode.encode(word))
    })
  }
  
  val waddr = (io.write.bits.addr >> blockOffBits).asUInt()
  val raddrs = io.read.map(r => (r.bits.addr >> blockOffBits).asUInt)
  io.write.ready := (if (readHighPriority) {
    if (singlePort) {
      !VecInit(io.read.map(_.valid)).asUInt.orR
    } else {
      !(Cat(io.read.zipWithIndex.map { case (r, i) => r.valid && raddrs(i) === waddr }).orR)
    }
  } else {
    true.B
  })

  for (j <- 0 until LoadPipelineWidth) {
    val raddr = raddrs(j)
    val rmask = io.read(j).bits.rmask

    // for single port SRAM, do not allow read and write in the same cycle
    // for dual port SRAM, raddr === waddr is undefined behavior
    val rwhazard = if(singlePort) io.write.valid else io.write.valid && waddr === raddr
    io.read(j).ready := (if (readHighPriority) true.B else !rwhazard)

    // use way_en to select a way after data read out
    assert(!(RegNext(io.read(j).fire() && PopCount(io.read(j).bits.way_en) > 1.U)))
    val way_en = RegNext(io.read(j).bits.way_en)

    for (r <- 0 until blockRows) {
      val resp = Wire(Vec(rowWords, Vec(nWays, Bits(wordBits.W))))
      val resp_chosen = Wire(Vec(rowWords, Bits(wordBits.W)))
      val ecc_resp = Wire(Vec(rowWords, Vec(nWays, Bits(eccBits.W))))
      val ecc_resp_chosen = Wire(Vec(rowWords, Bits(eccBits.W)))

      val ecc_array = Module(new SRAMTemplate(
        Vec(rowWords, Bits(eccBits.W)),
        set = nSets,
        way = nWays,
        shouldReset = false,
        holdRead = false,
        singlePort = singlePort
      ))

      ecc_array.io.w.req.valid := io.write.valid && io.write.bits.wmask(r)
      ecc_array.io.w.req.bits.apply(
        setIdx = waddr,
        data = getECCFromRow(io.write.bits.data(r)),
        waymask = io.write.bits.way_en
      )
      when (ecc_array.io.w.req.valid) {
        XSDebug(p"write in ecc sram ${j.U} row ${r.U}: setIdx=${Hexadecimal(ecc_array.io.w.req.bits.setIdx)} ecc(0)=${Hexadecimal(getECCFromRow(io.write.bits.data(r))(0))} ecc(1)=${Hexadecimal(getECCFromRow(io.write.bits.data(r))(1))} waymask=${Hexadecimal(io.write.bits.way_en)}\n")
      }

      ecc_array.io.r.req.valid := io.read(j).valid && rmask(r)
      ecc_array.io.r.req.bits.apply(setIdx = raddr)

      for (w <- 0 until nWays) {
        val data_array = Module(new SRAMTemplate(
          Bits(rowBits.W),
          set = nSets,
          way = 1,
          shouldReset = false,
          holdRead = false,
          singlePort = singlePort
        ))

        // data write
        val wen = io.write.valid && io.write.bits.way_en(w) && io.write.bits.wmask(r)
        data_array.io.w.req.valid := wen
        data_array.io.w.req.bits.apply(
          setIdx = waddr,
          data = io.write.bits.data(r),
          waymask = 1.U
        )
        when (wen) {
          XSDebug(p"write in data sram ${j.U} row ${r.U} way ${w.U}: setIdx=${Hexadecimal(data_array.io.w.req.bits.setIdx)} data=${Hexadecimal(io.write.bits.data(r))}\n")
        }

        // data read
        // read all ways and choose one after resp
        val ren = io.read(j).fire() && rmask(r)
        data_array.io.r.req.valid := ren
        data_array.io.r.req.bits.apply(setIdx = raddr)
        (0 until rowWords).foreach(k => resp(k)(w) := data_array.io.r.resp.data(0)(wordBits * (k + 1) - 1, wordBits * k))
        (0 until rowWords).foreach(k => ecc_resp(k)(w) := ecc_array.io.r.resp.data(w)(k))
      }
      for (k <- 0 until rowWords) {
        resp_chosen(k) := Mux1H(way_en, resp(k))
        ecc_resp_chosen(k) := Mux1H(way_en, ecc_resp(k))
        // assert(!RegNext(cacheParams.dataCode.decode(Cat(ecc_resp_chosen(k), resp_chosen(k))).uncorrectable &&
        //   way_en.orR &&
        //   RegNext(io.read(j).fire() && rmask(r))))
      }
      io.resp(j)(r) := Cat((0 until rowWords).reverse map {k => Cat(ecc_resp_chosen(k), resp_chosen(k))})// resp_chosen.asUInt

    }

    io.nacks(j) := false.B
  }
}

class L1MetadataArray(onReset: () => L1Metadata) extends DCacheModule {
  val rstVal = onReset()
  val metaBits = rstVal.getWidth
  val encMetaBits = cacheParams.tagCode.width(metaBits)

  val io = IO(new Bundle {
    val read = Flipped(Decoupled(new L1MetaReadReq))
    val write = Flipped(Decoupled(new L1MetaWriteReq))
    val resp = Output(Vec(nWays, UInt(encMetaBits.W)))
  })
  val rst_cnt = RegInit(0.U(log2Up(nSets+1).W))
  val rst = rst_cnt < nSets.U
  val waddr = Mux(rst, rst_cnt, io.write.bits.idx)
  val wdata = Mux(rst, rstVal, io.write.bits.data).asUInt
  val wmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.write.bits.way_en.asSInt).asBools
  val rmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.read.bits.way_en.asSInt).asBools
  when (rst) { rst_cnt := rst_cnt + 1.U }

  val tag_array = Module(new SRAMTemplate(UInt(encMetaBits.W), set=nSets, way=nWays,
    shouldReset=false, holdRead=false, singlePort=true))

  // tag write
  val wen = rst || io.write.valid
  tag_array.io.w.req.valid := wen
  tag_array.io.w.req.bits.apply(
    setIdx=waddr,
    data=cacheParams.tagCode.encode(wdata),
    waymask=VecInit(wmask).asUInt)

  // tag read
  val ren = io.read.fire()
  tag_array.io.r.req.valid := ren
  tag_array.io.r.req.bits.apply(setIdx=io.read.bits.idx)
  io.resp := tag_array.io.r.resp.data

  io.write.ready := !rst
  io.read.ready := !wen

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

  // def dumpResp() = {
  //   (0 until nWays) map { i =>
  //     XSDebug(s"MetaArray Resp: way: $i tag: %x coh: %x\n",
  //       io.resp(i).tag, io.resp(i).coh.state)
  //   }
  // }

  def dump() = {
    dumpRead
    dumpWrite
    // dumpResp
  }
}

class DuplicatedMetaArray extends DCacheModule {
  def onReset = L1Metadata(0.U, ClientMetadata.onReset)
  val metaBits = onReset.getWidth
  val encMetaBits = cacheParams.tagCode.width(metaBits)

  val io = IO(new DCacheBundle {
    val read  = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1MetaReadReq)))
    val write = Flipped(DecoupledIO(new L1MetaWriteReq))
    val resp  = Output(Vec(LoadPipelineWidth, Vec(nWays, UInt(encMetaBits.W))))
  })
  val meta = Seq.fill(LoadPipelineWidth) { Module(new L1MetadataArray(onReset _)) }

  for (w <- 0 until LoadPipelineWidth) {
    // meta(w).io.write <> io.write
    meta(w).io.write.valid := io.write.valid
    meta(w).io.write.bits := io.write.bits
    meta(w).io.read  <> io.read(w)
    io.resp(w) <> meta(w).io.resp
  }
  // io.write.ready := VecInit(meta.map(_.io.write.ready)).asUInt.andR
  io.write.ready := true.B

  def dumpRead() = {
    (0 until LoadPipelineWidth) map { w =>
      when (io.read(w).fire()) {
        XSDebug(s"MetaArray Read channel: $w idx: %d way_en: %x tag: %x\n",
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

  // def dumpResp() = {
  //   (0 until LoadPipelineWidth) map { w =>
  //     (0 until nWays) map { i =>
  //       XSDebug(s"MetaArray Resp: channel: $w way: $i tag: %x coh: %x\n",
  //         io.resp(w)(i).tag, io.resp(w)(i).coh.state)
  //     }
  //   }
  // }

  def dump() = {
    dumpRead
    dumpWrite
    // dumpResp
  }
}
