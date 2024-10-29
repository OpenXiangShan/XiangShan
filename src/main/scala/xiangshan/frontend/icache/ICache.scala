/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.IdRange
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import huancun.AliasField
import huancun.PrefetchField
import org.chipsalliance.cde.config.Parameters
import utility._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.frontend._

case class ICacheParameters(
    nSets:               Int = 256,
    nWays:               Int = 4,
    rowBits:             Int = 64,
    nTLBEntries:         Int = 32,
    tagECC:              Option[String] = None,
    dataECC:             Option[String] = None,
    replacer:            Option[String] = Some("random"),
    PortNumber:          Int = 2,
    nFetchMshr:          Int = 4,
    nPrefetchMshr:       Int = 10,
    nWayLookupSize:      Int = 32,
    DataCodeUnit:        Int = 64,
    ICacheDataBanks:     Int = 8,
    ICacheDataSRAMWidth: Int = 66,
    // TODO: hard code, need delete
    partWayNum: Int = 4,
    nMMIOs:     Int = 1,
    blockBytes: Int = 64
) extends L1CacheParameters {

  val setBytes = nSets * blockBytes
  val aliasBitsOpt =
    DCacheParameters().aliasBitsOpt // if(setBytes > pageSize) Some(log2Ceil(setBytes / pageSize)) else None
  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    ReqSourceField()
  ) ++ aliasBitsOpt.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Nil
  def tagCode:    Code                 = Code.fromString(tagECC)
  def dataCode:   Code                 = Code.fromString(dataECC)
  def replacement = ReplacementPolicy.fromString(replacer, nWays, nSets)
}

trait HasICacheParameters extends HasL1CacheParameters with HasInstrMMIOConst with HasIFUConst {
  val cacheParams = icacheParameters

  def ICacheSets          = cacheParams.nSets
  def ICacheWays          = cacheParams.nWays
  def PortNumber          = cacheParams.PortNumber
  def nFetchMshr          = cacheParams.nFetchMshr
  def nPrefetchMshr       = cacheParams.nPrefetchMshr
  def nWayLookupSize      = cacheParams.nWayLookupSize
  def DataCodeUnit        = cacheParams.DataCodeUnit
  def ICacheDataBanks     = cacheParams.ICacheDataBanks
  def ICacheDataSRAMWidth = cacheParams.ICacheDataSRAMWidth
  def partWayNum          = cacheParams.partWayNum

  def ICacheMetaBits      = tagBits // FIXME: unportable: maybe use somemethod to get width
  def ICacheMetaCodeBits  = 1       // FIXME: unportable: maybe use cacheParams.tagCode.somemethod to get width
  def ICacheMetaEntryBits = ICacheMetaBits + ICacheMetaCodeBits

  def ICacheDataBits     = blockBits / ICacheDataBanks
  def ICacheDataCodeSegs = math.ceil(ICacheDataBits / DataCodeUnit).toInt // split data to segments for ECC checking
  def ICacheDataCodeBits =
    ICacheDataCodeSegs * 1 // FIXME: unportable: maybe use cacheParams.dataCode.somemethod to get width
  def ICacheDataEntryBits = ICacheDataBits + ICacheDataCodeBits
  def ICacheBankVisitNum  = 32 * 8 / ICacheDataBits + 1
  def highestIdxBit       = log2Ceil(nSets) - 1

  require((ICacheDataBanks >= 2) && isPow2(ICacheDataBanks))
  require(ICacheDataSRAMWidth >= ICacheDataEntryBits)
  require(isPow2(ICacheSets), s"nSets($ICacheSets) must be pow2")
  require(isPow2(ICacheWays), s"nWays($ICacheWays) must be pow2")

  def getBits(num: Int) = log2Ceil(num).W

  def generatePipeControl(lastFire: Bool, thisFire: Bool, thisFlush: Bool, lastFlush: Bool): Bool = {
    val valid = RegInit(false.B)
    when(thisFlush)(valid := false.B)
      .elsewhen(lastFire && !lastFlush)(valid := true.B)
      .elsewhen(thisFire)(valid := false.B)
    valid
  }

  def ResultHoldBypass[T <: Data](data: T, valid: Bool): T =
    Mux(valid, data, RegEnable(data, valid))

  def ResultHoldBypass[T <: Data](data: T, init: T, valid: Bool): T =
    Mux(valid, data, RegEnable(data, init, valid))

  def holdReleaseLatch(valid: Bool, release: Bool, flush: Bool): Bool = {
    val bit = RegInit(false.B)
    when(flush)(bit := false.B)
      .elsewhen(valid && !release)(bit := true.B)
      .elsewhen(release)(bit := false.B)
    bit || valid
  }

  def blockCounter(block: Bool, flush: Bool, threshold: Int): Bool = {
    val counter = RegInit(0.U(log2Up(threshold + 1).W))
    when(block)(counter := counter + 1.U)
    when(flush)(counter := 0.U)
    counter > threshold.U
  }

  def InitQueue[T <: Data](entry: T, size: Int): Vec[T] =
    return RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(entry.cloneType))))

  def encodeMetaECC(meta: UInt): UInt = {
    require(meta.getWidth == ICacheMetaBits)
    val code = cacheParams.tagCode.encode(meta) >> ICacheMetaBits
    code.asTypeOf(UInt(ICacheMetaCodeBits.W))
  }

  def encodeDataECC(data: UInt): UInt = {
    require(data.getWidth == ICacheDataBits)
    val datas = data.asTypeOf(Vec(ICacheDataCodeSegs, UInt((ICacheDataBits / ICacheDataCodeSegs).W)))
    val codes = VecInit(datas.map(cacheParams.dataCode.encode(_) >> (ICacheDataBits / ICacheDataCodeSegs)))
    codes.asTypeOf(UInt(ICacheDataCodeBits.W))
  }

  def getBankSel(blkOffset: UInt, valid: Bool = true.B): Vec[UInt] = {
    val bankIdxLow  = Cat(0.U(1.W), blkOffset) >> log2Ceil(blockBytes / ICacheDataBanks)
    val bankIdxHigh = (Cat(0.U(1.W), blkOffset) + 32.U) >> log2Ceil(blockBytes / ICacheDataBanks)
    val bankSel     = VecInit((0 until ICacheDataBanks * 2).map(i => (i.U >= bankIdxLow) && (i.U <= bankIdxHigh)))
    assert(
      !valid || PopCount(bankSel) === ICacheBankVisitNum.U,
      "The number of bank visits must be %d, but bankSel=0x%x",
      ICacheBankVisitNum.U,
      bankSel.asUInt
    )
    bankSel.asTypeOf(UInt((ICacheDataBanks * 2).W)).asTypeOf(Vec(2, UInt(ICacheDataBanks.W)))
  }

  def getLineSel(blkOffset: UInt)(implicit p: Parameters): Vec[Bool] = {
    val bankIdxLow = blkOffset >> log2Ceil(blockBytes / ICacheDataBanks)
    val lineSel    = VecInit((0 until ICacheDataBanks).map(i => i.U < bankIdxLow))
    lineSel
  }

  def getBlkAddr(addr:           UInt) = addr >> blockOffBits
  def getPhyTagFromBlk(addr:     UInt): UInt = addr >> (pgUntagBits - blockOffBits)
  def getIdxFromBlk(addr:        UInt) = addr(idxBits - 1, 0)
  def get_paddr_from_ptag(vaddr: UInt, ptag: UInt) = Cat(ptag, vaddr(pgUntagBits - 1, 0))
}

abstract class ICacheBundle(implicit p: Parameters) extends XSBundle
    with HasICacheParameters

abstract class ICacheModule(implicit p: Parameters) extends XSModule
    with HasICacheParameters

abstract class ICacheArray(implicit p: Parameters) extends XSModule
    with HasICacheParameters

class ICacheMetadata(implicit p: Parameters) extends ICacheBundle {
  val tag = UInt(tagBits.W)
}

object ICacheMetadata {
  def apply(tag: Bits)(implicit p: Parameters) = {
    val meta = Wire(new ICacheMetadata)
    meta.tag := tag
    meta
  }
}

class ICacheMetaArray()(implicit p: Parameters) extends ICacheArray {
  class ICacheMetaEntry(implicit p: Parameters) extends ICacheBundle {
    val meta: ICacheMetadata = new ICacheMetadata
    val code: UInt           = UInt(ICacheMetaCodeBits.W)
  }

  private object ICacheMetaEntry {
    def apply(meta: ICacheMetadata)(implicit p: Parameters): ICacheMetaEntry = {
      val entry = Wire(new ICacheMetaEntry)
      entry.meta := meta
      entry.code := encodeMetaECC(meta.asUInt)
      entry
    }
  }

  // sanity check
  require(ICacheMetaEntryBits == (new ICacheMetaEntry).getWidth)

  val io = IO(new Bundle {
    val write    = Flipped(DecoupledIO(new ICacheMetaWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle))
    val readResp = Output(new ICacheMetaRespBundle)
    val fencei   = Input(Bool())
  })

  val port_0_read_0 = io.read.valid && !io.read.bits.vSetIdx(0)(0)
  val port_0_read_1 = io.read.valid && io.read.bits.vSetIdx(0)(0)
  val port_1_read_1 = io.read.valid && io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine
  val port_1_read_0 = io.read.valid && !io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine

  val port_0_read_0_reg = RegEnable(port_0_read_0, 0.U.asTypeOf(port_0_read_0), io.read.fire)
  val port_0_read_1_reg = RegEnable(port_0_read_1, 0.U.asTypeOf(port_0_read_1), io.read.fire)
  val port_1_read_1_reg = RegEnable(port_1_read_1, 0.U.asTypeOf(port_1_read_1), io.read.fire)
  val port_1_read_0_reg = RegEnable(port_1_read_0, 0.U.asTypeOf(port_1_read_0), io.read.fire)

  val bank_0_idx = Mux(port_0_read_0, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  val bank_1_idx = Mux(port_0_read_1, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  val bank_idx   = Seq(bank_0_idx, bank_1_idx)

  val write_bank_0 = io.write.valid && !io.write.bits.bankIdx
  val write_bank_1 = io.write.valid && io.write.bits.bankIdx

  val write_meta_bits = ICacheMetaEntry(meta =
    ICacheMetadata(
      tag = io.write.bits.phyTag
    )
  )

  val tagArrays = (0 until 2) map { bank =>
    val tagArray = Module(new SRAMTemplate(
      new ICacheMetaEntry(),
      set = nSets / 2,
      way = nWays,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))

    // meta connection
    if (bank == 0) {
      tagArray.io.r.req.valid := port_0_read_0 || port_1_read_0
      tagArray.io.r.req.bits.apply(setIdx = bank_0_idx(highestIdxBit, 1))
      tagArray.io.w.req.valid := write_bank_0
      tagArray.io.w.req.bits.apply(
        data = write_meta_bits,
        setIdx = io.write.bits.virIdx(highestIdxBit, 1),
        waymask = io.write.bits.waymask
      )
    } else {
      tagArray.io.r.req.valid := port_0_read_1 || port_1_read_1
      tagArray.io.r.req.bits.apply(setIdx = bank_1_idx(highestIdxBit, 1))
      tagArray.io.w.req.valid := write_bank_1
      tagArray.io.w.req.bits.apply(
        data = write_meta_bits,
        setIdx = io.write.bits.virIdx(highestIdxBit, 1),
        waymask = io.write.bits.waymask
      )
    }

    tagArray
  }

  val read_set_idx_next = RegEnable(io.read.bits.vSetIdx, 0.U.asTypeOf(io.read.bits.vSetIdx), io.read.fire)
  val valid_array       = RegInit(VecInit(Seq.fill(nWays)(0.U(nSets.W))))
  val valid_metas       = Wire(Vec(PortNumber, Vec(nWays, Bool())))
  // valid read
  (0 until PortNumber).foreach(i =>
    (0 until nWays).foreach(way =>
      valid_metas(i)(way) := valid_array(way)(read_set_idx_next(i))
    )
  )
  io.readResp.entryValid := valid_metas

  io.read.ready := !io.write.valid && !io.fencei && tagArrays.map(_.io.r.req.ready).reduce(_ && _)

  // valid write
  val way_num = OHToUInt(io.write.bits.waymask)
  when(io.write.valid) {
    valid_array(way_num) := valid_array(way_num).bitSet(io.write.bits.virIdx, true.B)
  }

  XSPerfAccumulate("meta_refill_num", io.write.valid)

  io.readResp.metas <> DontCare
  io.readResp.codes <> DontCare
  val readMetaEntries = tagArrays.map(port => port.io.r.resp.asTypeOf(Vec(nWays, new ICacheMetaEntry())))
  val readMetas       = readMetaEntries.map(_.map(_.meta))
  val readCodes       = readMetaEntries.map(_.map(_.code))

  // TEST: force ECC to fail by setting readCodes to 0
  if (ICacheForceMetaECCError) {
    readCodes.foreach(_.foreach(_ := 0.U))
  }

  when(port_0_read_0_reg) {
    io.readResp.metas(0) := readMetas(0)
    io.readResp.codes(0) := readCodes(0)
  }.elsewhen(port_0_read_1_reg) {
    io.readResp.metas(0) := readMetas(1)
    io.readResp.codes(0) := readCodes(1)
  }

  when(port_1_read_0_reg) {
    io.readResp.metas(1) := readMetas(0)
    io.readResp.codes(1) := readCodes(0)
  }.elsewhen(port_1_read_1_reg) {
    io.readResp.metas(1) := readMetas(1)
    io.readResp.codes(1) := readCodes(1)
  }

  io.write.ready := true.B // TODO : has bug ? should be !io.cacheOp.req.valid

  // fencei logic : reset valid_array
  when(io.fencei) {
    (0 until nWays).foreach(way =>
      valid_array(way) := 0.U
    )
  }
}

class ICacheDataArray(implicit p: Parameters) extends ICacheArray {
  class ICacheDataEntry(implicit p: Parameters) extends ICacheBundle {
    val data = UInt(ICacheDataBits.W)
    val code = UInt(ICacheDataCodeBits.W)
  }

  object ICacheDataEntry {
    def apply(data: UInt)(implicit p: Parameters) = {
      val entry = Wire(new ICacheDataEntry)
      entry.data := data
      entry.code := encodeDataECC(data)
      entry
    }
  }

  val io = IO {
    new Bundle {
      val write = Flipped(DecoupledIO(new ICacheDataWriteBundle))
      // TODO: fix hard code
      val read     = Flipped(Vec(4, DecoupledIO(new ICacheReadBundle)))
      val readResp = Output(new ICacheDataRespBundle)
    }
  }

  /**
    ******************************************************************************
    * data array
    ******************************************************************************
    */
  val writeDatas   = io.write.bits.data.asTypeOf(Vec(ICacheDataBanks, UInt(ICacheDataBits.W)))
  val writeEntries = writeDatas.map(ICacheDataEntry(_).asUInt)

  val bankSel  = getBankSel(io.read(0).bits.blkOffset, io.read(0).valid)
  val lineSel  = getLineSel(io.read(0).bits.blkOffset)
  val waymasks = io.read(0).bits.wayMask
  val masks    = Wire(Vec(nWays, Vec(ICacheDataBanks, Bool())))
  (0 until nWays).foreach { way =>
    (0 until ICacheDataBanks).foreach { bank =>
      masks(way)(bank) := Mux(
        lineSel(bank),
        waymasks(1)(way) && bankSel(1)(bank).asBool,
        waymasks(0)(way) && bankSel(0)(bank).asBool
      )
    }
  }

  val dataArrays = (0 until nWays).map { way =>
    (0 until ICacheDataBanks).map { bank =>
      val sramBank = Module(new SRAMTemplateWithFixedWidth(
        UInt(ICacheDataEntryBits.W),
        set = nSets,
        width = ICacheDataSRAMWidth,
        shouldReset = true,
        holdRead = true,
        singlePort = true
      ))

      // read
      sramBank.io.r.req.valid := io.read(bank % 4).valid && masks(way)(bank)
      sramBank.io.r.req.bits.apply(setIdx =
        Mux(lineSel(bank), io.read(bank % 4).bits.vSetIdx(1), io.read(bank % 4).bits.vSetIdx(0))
      )
      // write
      sramBank.io.w.req.valid := io.write.valid && io.write.bits.waymask(way).asBool
      sramBank.io.w.req.bits.apply(
        data = writeEntries(bank),
        setIdx = io.write.bits.virIdx,
        // waymask is invalid when way of SRAMTemplate <= 1
        waymask = 0.U
      )
      sramBank
    }
  }

  /**
    ******************************************************************************
    * read logic
    ******************************************************************************
    */
  val masksReg = RegEnable(masks, 0.U.asTypeOf(masks), io.read(0).valid)
  val readDataWithCode = (0 until ICacheDataBanks).map(bank =>
    Mux1H(VecInit(masksReg.map(_(bank))).asTypeOf(UInt(nWays.W)), dataArrays.map(_(bank).io.r.resp.asUInt))
  )
  val readEntries = readDataWithCode.map(_.asTypeOf(new ICacheDataEntry()))
  val readDatas   = VecInit(readEntries.map(_.data))
  val readCodes   = VecInit(readEntries.map(_.code))

  // TEST: force ECC to fail by setting readCodes to 0
  if (ICacheForceDataECCError) {
    readCodes.foreach(_ := 0.U)
  }

  /**
    ******************************************************************************
    * IO
    ******************************************************************************
    */
  io.readResp.datas := readDatas
  io.readResp.codes := readCodes
  io.write.ready    := true.B
  io.read.foreach(_.ready := !io.write.valid)
}

class ICacheReplacer(implicit p: Parameters) extends ICacheModule {
  val io = IO(new Bundle {
    val touch  = Vec(PortNumber, Flipped(ValidIO(new ReplacerTouch)))
    val victim = Flipped(new ReplacerVictim)
  })

  val replacers = Seq.fill(PortNumber)(ReplacementPolicy.fromString(cacheParams.replacer, nWays, nSets / PortNumber))

  // touch
  val touch_sets = Seq.fill(PortNumber)(Wire(Vec(2, UInt(log2Ceil(nSets / 2).W))))
  val touch_ways = Seq.fill(PortNumber)(Wire(Vec(2, Valid(UInt(log2Ceil(nWays).W)))))
  (0 until PortNumber).foreach { i =>
    touch_sets(i)(0) := Mux(
      io.touch(i).bits.vSetIdx(0),
      io.touch(1).bits.vSetIdx(highestIdxBit, 1),
      io.touch(0).bits.vSetIdx(highestIdxBit, 1)
    )
    touch_ways(i)(0).bits  := Mux(io.touch(i).bits.vSetIdx(0), io.touch(1).bits.way, io.touch(0).bits.way)
    touch_ways(i)(0).valid := Mux(io.touch(i).bits.vSetIdx(0), io.touch(1).valid, io.touch(0).valid)
  }

  // victim
  io.victim.way := Mux(
    io.victim.vSetIdx.bits(0),
    replacers(1).way(io.victim.vSetIdx.bits(highestIdxBit, 1)),
    replacers(0).way(io.victim.vSetIdx.bits(highestIdxBit, 1))
  )

  // touch the victim in next cycle
  val victim_vSetIdx_reg =
    RegEnable(io.victim.vSetIdx.bits, 0.U.asTypeOf(io.victim.vSetIdx.bits), io.victim.vSetIdx.valid)
  val victim_way_reg = RegEnable(io.victim.way, 0.U.asTypeOf(io.victim.way), io.victim.vSetIdx.valid)
  (0 until PortNumber).foreach { i =>
    touch_sets(i)(1)       := victim_vSetIdx_reg(highestIdxBit, 1)
    touch_ways(i)(1).bits  := victim_way_reg
    touch_ways(i)(1).valid := RegNext(io.victim.vSetIdx.valid) && (victim_vSetIdx_reg(0) === i.U)
  }

  ((replacers zip touch_sets) zip touch_ways).map { case ((r, s), w) => r.access(s, w) }
}

class ICacheIO(implicit p: Parameters) extends ICacheBundle {
  val hartId       = Input(UInt(hartIdLen.W))
  val ftqPrefetch  = Flipped(new FtqToPrefetchIO)
  val softPrefetch = Vec(backendParams.LduCnt, Flipped(Valid(new SoftIfetchPrefetchBundle)))
  val stop         = Input(Bool())
  val fetch        = new ICacheMainPipeBundle
  val toIFU        = Output(Bool())
  val pmp          = Vec(2 * PortNumber, new ICachePMPBundle)
  val itlb         = Vec(PortNumber, new TlbRequestIO)
  val perfInfo     = Output(new ICachePerfInfo)
  val error        = ValidIO(new L1CacheErrorInfo)
  /* CSR control signal */
  val csr_pf_enable     = Input(Bool())
  val csr_parity_enable = Input(Bool())
  val fencei            = Input(Bool())
  val flush             = Input(Bool())
}

class ICache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {
  override def shouldBeInlined: Boolean = false

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, cacheParams.nFetchMshr + cacheParams.nPrefetchMshr + 1)
    )),
    requestFields = cacheParams.reqFields,
    echoFields = cacheParams.echoFields
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new ICacheImp(this)
}

class ICacheImp(outer: ICache) extends LazyModuleImp(outer) with HasICacheParameters with HasPerfEvents {
  val io = IO(new ICacheIO)

  println("ICache:")
  println("  TagECC: " + cacheParams.tagECC)
  println("  DataECC: " + cacheParams.dataECC)
  println("  ICacheSets: " + cacheParams.nSets)
  println("  ICacheWays: " + cacheParams.nWays)
  println("  PortNumber: " + cacheParams.PortNumber)
  println("  nFetchMshr: " + cacheParams.nFetchMshr)
  println("  nPrefetchMshr: " + cacheParams.nPrefetchMshr)
  println("  nWayLookupSize: " + cacheParams.nWayLookupSize)
  println("  DataCodeUnit: " + cacheParams.DataCodeUnit)
  println("  ICacheDataBanks: " + cacheParams.ICacheDataBanks)
  println("  ICacheDataSRAMWidth: " + cacheParams.ICacheDataSRAMWidth)

  val (bus, edge) = outer.clientNode.out.head

  val metaArray  = Module(new ICacheMetaArray)
  val dataArray  = Module(new ICacheDataArray)
  val mainPipe   = Module(new ICacheMainPipe)
  val missUnit   = Module(new ICacheMissUnit(edge))
  val replacer   = Module(new ICacheReplacer)
  val prefetcher = Module(new IPrefetchPipe)
  val wayLookup  = Module(new WayLookup)

  dataArray.io.write <> missUnit.io.data_write
  dataArray.io.read <> mainPipe.io.dataArray.toIData
  dataArray.io.readResp <> mainPipe.io.dataArray.fromIData

  metaArray.io.fencei := io.fencei
  metaArray.io.write <> missUnit.io.meta_write
  metaArray.io.read <> prefetcher.io.metaRead.toIMeta
  metaArray.io.readResp <> prefetcher.io.metaRead.fromIMeta

  prefetcher.io.flush             := io.flush
  prefetcher.io.csr_pf_enable     := io.csr_pf_enable
  prefetcher.io.csr_parity_enable := io.csr_parity_enable
  prefetcher.io.MSHRResp          := missUnit.io.fetch_resp
  prefetcher.io.flushFromBpu      := io.ftqPrefetch.flushFromBpu
  // cache softPrefetch
  private val softPrefetchValid = RegInit(false.B)
  private val softPrefetch      = RegInit(0.U.asTypeOf(new IPrefetchReq))
  /* FIXME:
   * If there is already a pending softPrefetch request, it will be overwritten.
   * Also, if there are multiple softPrefetch requests in the same cycle, only the first one will be accepted.
   * We should implement a softPrefetchQueue (like ibuffer, multi-in, single-out) to solve this.
   * However, the impact on performance still needs to be assessed.
   * Considering that the frequency of prefetch.i may not be high, let's start with a temporary dummy solution.
   */
  when(io.softPrefetch.map(_.valid).reduce(_ || _)) {
    softPrefetchValid := true.B
    softPrefetch.fromSoftPrefetch(MuxCase(
      0.U.asTypeOf(new SoftIfetchPrefetchBundle),
      io.softPrefetch.map(req => req.valid -> req.bits)
    ))
  }.elsewhen(prefetcher.io.req.fire) {
    softPrefetchValid := false.B
  }
  // pass ftqPrefetch
  private val ftqPrefetch = WireInit(0.U.asTypeOf(new IPrefetchReq))
  ftqPrefetch.fromFtqICacheInfo(io.ftqPrefetch.req.bits)
  // software prefetch has higher priority
  prefetcher.io.req.valid  := softPrefetchValid || io.ftqPrefetch.req.valid
  prefetcher.io.req.bits   := Mux(softPrefetchValid, softPrefetch, ftqPrefetch)
  io.ftqPrefetch.req.ready := prefetcher.io.req.ready && !softPrefetchValid

  missUnit.io.hartId := io.hartId
  missUnit.io.fencei := io.fencei
  missUnit.io.flush  := io.flush
  missUnit.io.fetch_req <> mainPipe.io.mshr.req
  missUnit.io.prefetch_req <> prefetcher.io.MSHRReq
  missUnit.io.mem_grant.valid := false.B
  missUnit.io.mem_grant.bits  := DontCare
  missUnit.io.mem_grant <> bus.d

  mainPipe.io.flush             := io.flush
  mainPipe.io.respStall         := io.stop
  mainPipe.io.csr_parity_enable := io.csr_parity_enable
  mainPipe.io.hartId            := io.hartId
  mainPipe.io.mshr.resp         := missUnit.io.fetch_resp
  mainPipe.io.fetch.req <> io.fetch.req
  mainPipe.io.wayLookupRead <> wayLookup.io.read

  wayLookup.io.flush := io.flush
  wayLookup.io.write <> prefetcher.io.wayLookupWrite
  wayLookup.io.update := missUnit.io.fetch_resp

  replacer.io.touch <> mainPipe.io.touch
  replacer.io.victim <> missUnit.io.victim

  io.pmp(0) <> mainPipe.io.pmp(0)
  io.pmp(1) <> mainPipe.io.pmp(1)
  io.pmp(2) <> prefetcher.io.pmp(0)
  io.pmp(3) <> prefetcher.io.pmp(1)

  io.itlb(0) <> prefetcher.io.itlb(0)
  io.itlb(1) <> prefetcher.io.itlb(1)

  // notify IFU that Icache pipeline is available
  io.toIFU    := mainPipe.io.fetch.req.ready
  io.perfInfo := mainPipe.io.perfInfo

  io.fetch.resp <> mainPipe.io.fetch.resp
  io.fetch.topdownIcacheMiss := mainPipe.io.fetch.topdownIcacheMiss
  io.fetch.topdownItlbMiss   := mainPipe.io.fetch.topdownItlbMiss

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  bus.a <> missUnit.io.mem_acquire

  // Parity error port
  val errors       = mainPipe.io.errors
  val errors_valid = errors.map(e => e.valid).reduce(_ | _)
  io.error.bits <> RegEnable(
    PriorityMux(errors.map(e => e.valid -> e.bits)),
    0.U.asTypeOf(errors(0).bits),
    errors_valid
  )
  io.error.valid := RegNext(errors_valid, false.B)

  XSPerfAccumulate(
    "softPrefetch_drop_not_ready",
    io.softPrefetch.map(_.valid).reduce(_ || _) && softPrefetchValid && !prefetcher.io.req.fire
  )
  XSPerfAccumulate("softPrefetch_drop_multi_req", PopCount(io.softPrefetch.map(_.valid)) > 1.U)
  XSPerfAccumulate("softPrefetch_block_ftq", softPrefetchValid && io.ftqPrefetch.req.valid)

  val perfEvents = Seq(
    ("icache_miss_cnt  ", false.B),
    ("icache_miss_penalty", BoolStopWatch(start = false.B, stop = false.B || false.B, startHighPriority = true))
  )
  generatePerfEvent()
}

class ICachePartWayReadBundle[T <: Data](gen: T, pWay: Int)(implicit p: Parameters)
    extends ICacheBundle {
  val req = Flipped(Vec(
    PortNumber,
    Decoupled(new Bundle {
      val ridx = UInt((log2Ceil(nSets) - 1).W)
    })
  ))
  val resp = Output(new Bundle {
    val rdata = Vec(PortNumber, Vec(pWay, gen))
  })
}

class ICacheWriteBundle[T <: Data](gen: T, pWay: Int)(implicit p: Parameters)
    extends ICacheBundle {
  val wdata    = gen
  val widx     = UInt((log2Ceil(nSets) - 1).W)
  val wbankidx = Bool()
  val wmask    = Vec(pWay, Bool())
}

class ICachePartWayArray[T <: Data](gen: T, pWay: Int)(implicit p: Parameters) extends ICacheArray {

  // including part way data
  val io = IO {
    new Bundle {
      val read  = new ICachePartWayReadBundle(gen, pWay)
      val write = Flipped(ValidIO(new ICacheWriteBundle(gen, pWay)))
    }
  }

  io.read.req.map(_.ready := !io.write.valid)

  val srams = (0 until PortNumber) map { bank =>
    val sramBank = Module(new SRAMTemplate(
      gen,
      set = nSets / 2,
      way = pWay,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))

    sramBank.io.r.req.valid := io.read.req(bank).valid
    sramBank.io.r.req.bits.apply(setIdx = io.read.req(bank).bits.ridx)

    if (bank == 0) sramBank.io.w.req.valid := io.write.valid && !io.write.bits.wbankidx
    else sramBank.io.w.req.valid           := io.write.valid && io.write.bits.wbankidx
    sramBank.io.w.req.bits.apply(
      data = io.write.bits.wdata,
      setIdx = io.write.bits.widx,
      waymask = io.write.bits.wmask.asUInt
    )

    sramBank
  }

  io.read.req.map(_.ready := !io.write.valid && srams.map(_.io.r.req.ready).reduce(_ && _))

  io.read.resp.rdata := VecInit(srams.map(bank => bank.io.r.resp.asTypeOf(Vec(pWay, gen))))

}

// Automatically partition the SRAM based on the width of the data and the desired width.
// final SRAM width = width * way
class SRAMTemplateWithFixedWidth[T <: Data](
    gen:         T,
    set:         Int,
    width:       Int,
    way:         Int = 1,
    shouldReset: Boolean = false,
    holdRead:    Boolean = false,
    singlePort:  Boolean = false,
    bypassWrite: Boolean = false
) extends Module {

  val dataBits  = gen.getWidth
  val bankNum   = math.ceil(dataBits.toDouble / width.toDouble).toInt
  val totalBits = bankNum * width

  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val wordType = UInt(width.W)
  val writeDatas = (0 until bankNum).map(bank =>
    VecInit((0 until way).map(i =>
      io.w.req.bits.data(i).asTypeOf(UInt(totalBits.W)).asTypeOf(Vec(bankNum, wordType))(bank)
    ))
  )

  val srams = (0 until bankNum) map { bank =>
    val sramBank = Module(new SRAMTemplate(
      wordType,
      set = set,
      way = way,
      shouldReset = shouldReset,
      holdRead = holdRead,
      singlePort = singlePort,
      bypassWrite = bypassWrite
    ))
    // read req
    sramBank.io.r.req.valid       := io.r.req.valid
    sramBank.io.r.req.bits.setIdx := io.r.req.bits.setIdx

    // write req
    sramBank.io.w.req.valid       := io.w.req.valid
    sramBank.io.w.req.bits.setIdx := io.w.req.bits.setIdx
    sramBank.io.w.req.bits.data   := writeDatas(bank)
    sramBank.io.w.req.bits.waymask.map(_ := io.w.req.bits.waymask.get)

    sramBank
  }

  io.r.req.ready := !io.w.req.valid
  (0 until way).foreach { i =>
    io.r.resp.data(i) := VecInit((0 until bankNum).map(bank =>
      srams(bank).io.r.resp.data(i)
    )).asTypeOf(UInt(totalBits.W))(dataBits - 1, 0).asTypeOf(gen.cloneType)
  }

  io.r.req.ready := srams.head.io.r.req.ready
  io.w.req.ready := srams.head.io.w.req.ready
}
