/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

package  xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import huancun.{AliasField, PrefetchField}
import org.chipsalliance.cde.config.Parameters
import utility._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.frontend._
import firrtl.ir.Block
import firrtl.options.DoNotTerminateOnExit

case class ICacheParameters(
    nSets: Int = 256,
    nWays: Int = 4,
    rowBits: Int = 64,
    nTLBEntries: Int = 32,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    replacer: Option[String] = Some("random"),
    nMissEntries: Int = 2,
    nReleaseEntries: Int = 1,
    nProbeEntries: Int = 2,
    // fdip default config
    enableICachePrefetch: Boolean = true,
    prefetchToL1: Boolean = false,
    prefetchPipeNum: Int = 2,
    nPrefetchEntries: Int = 12,
    nPrefBufferEntries: Int = 32,
    maxIPFMoveConf: Int = 1, // temporary use small value to cause more "move" operation
    minRangeFromIFUptr: Int = 2,
    maxRangeFromIFUptr: Int = 32,

    nFetchMshr: Int = 4,
    nPrefetchMshr: Int = 10,
    nWayLookupSize: Int = 32,

    nBanks: Int = 2,
    
    nMMIOs: Int = 1,
    blockBytes: Int = 64
)extends L1CacheParameters {

  val setBytes = nSets * blockBytes
  val aliasBitsOpt = DCacheParameters().aliasBitsOpt //if(setBytes > pageSize) Some(log2Ceil(setBytes / pageSize)) else None
  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    ReqSourceField()
  ) ++ aliasBitsOpt.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Nil
  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = ReplacementPolicy.fromString(replacer,nWays,nSets)
}

trait HasICacheParameters extends HasL1CacheParameters with HasInstrMMIOConst with HasIFUConst{
  val cacheParams = icacheParameters
  val dataCodeUnit = 16
  val dataCodeUnitNum  = blockBits/cacheParams.nBanks/dataCodeUnit

  def highestIdxBit = log2Ceil(nSets) - 1
  def encDataUnitBits   = cacheParams.dataCode.width(dataCodeUnit)
  def dataCodeBits      = encDataUnitBits - dataCodeUnit
  def dataCodeEntryBits = dataCodeBits * dataCodeUnitNum

  val ICacheSets = cacheParams.nSets
  val ICacheWays = cacheParams.nWays

  val ICacheSameVPAddrLength = 12
  val ReplaceIdWid = 5

  val ICacheWordOffset = 0
  val ICacheSetOffset = ICacheWordOffset + log2Up(blockBytes)
  val ICacheAboveIndexOffset = ICacheSetOffset + log2Up(ICacheSets)
  val ICacheTagOffset = ICacheAboveIndexOffset min ICacheSameVPAddrLength

  def PortNumber = 2

  def partWayNum = nWays
  def pWay = nWays/partWayNum

  def enableICachePrefetch      = cacheParams.enableICachePrefetch
  def prefetchToL1        = cacheParams.prefetchToL1
  def prefetchPipeNum     = cacheParams.prefetchPipeNum
  def nPrefetchEntries    = cacheParams.nPrefetchEntries
  def nPrefBufferEntries  = cacheParams.nPrefBufferEntries
  def maxIPFMoveConf      = cacheParams.maxIPFMoveConf
  def minRangeFromIFUptr  = cacheParams.minRangeFromIFUptr
  def maxRangeFromIFUptr  = cacheParams.maxRangeFromIFUptr

  def nFetchMshr          = cacheParams.nFetchMshr
  def nPrefetchMshr       = cacheParams.nPrefetchMshr
  def nWayLookupSize      = cacheParams.nWayLookupSize
  def nBanks              = cacheParams.nBanks

  def getBits(num: Int) = log2Ceil(num).W


  def generatePipeControl(lastFire: Bool, thisFire: Bool, thisFlush: Bool, lastFlush: Bool): Bool = {
    val valid  = RegInit(false.B)
    when(thisFlush)                    {valid  := false.B}
      .elsewhen(lastFire && !lastFlush)  {valid  := true.B}
      .elsewhen(thisFire)                 {valid  := false.B}
    valid
  }

  def ResultHoldBypass[T<:Data](data: T, valid: Bool): T = {
    Mux(valid, data, RegEnable(data, valid))
  }

  def holdReleaseLatch(valid: Bool, release: Bool, flush: Bool): Bool ={
    val bit = RegInit(false.B)
    when(flush)                   { bit := false.B  }
      .elsewhen(valid && !release)  { bit := true.B   }
      .elsewhen(release)            { bit := false.B  }
    bit || valid
  }

  def blockCounter(block: Bool, flush: Bool, threshold: Int): Bool = {
    val counter = RegInit(0.U(log2Up(threshold + 1).W))
    when (block) { counter := counter + 1.U }
    when (flush) { counter := 0.U}
    counter > threshold.U
  }

  def InitQueue[T <: Data](entry: T, size: Int): Vec[T] ={
    return RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(entry.cloneType))))
  }

  def getBlkAddr(addr: UInt) = addr >> blockOffBits
  def getPhyTagFromBlk(addr: UInt) = addr >> (pgUntagBits - blockOffBits)
  def getIdxFromBlk(addr: UInt) = addr(idxBits - 1, 0)

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
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


class ICacheMetaArray()(implicit p: Parameters) extends ICacheArray
{
  def onReset = ICacheMetadata(0.U)
  val metaBits = onReset.getWidth
  val metaEntryBits = cacheParams.tagCode.width(metaBits)

  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheMetaWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle))
    val readResp = Output(new ICacheMetaRespBundle)
    val fencei   = Input(Bool())
  }}

  io.read.ready := !io.write.valid

  val port_0_read_0 = io.read.valid  && !io.read.bits.vSetIdx(0)(0)
  val port_0_read_1 = io.read.valid  &&  io.read.bits.vSetIdx(0)(0)
  val port_1_read_1  = io.read.valid &&  io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine
  val port_1_read_0  = io.read.valid && !io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine

  val port_0_read_0_reg = RegEnable(port_0_read_0, io.read.fire)
  val port_0_read_1_reg = RegEnable(port_0_read_1, io.read.fire)
  val port_1_read_1_reg = RegEnable(port_1_read_1, io.read.fire)
  val port_1_read_0_reg = RegEnable(port_1_read_0, io.read.fire)

  val bank_0_idx = Mux(port_0_read_0, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  val bank_1_idx = Mux(port_0_read_1, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  val bank_idx   = Seq(bank_0_idx, bank_1_idx)

  val write_bank_0 = io.write.valid && !io.write.bits.bankIdx
  val write_bank_1 = io.write.valid &&  io.write.bits.bankIdx

  val write_meta_bits = Wire(UInt(metaEntryBits.W))

  val tagArrays = (0 until 2) map { bank =>
    val tagArray = Module(new SRAMTemplate(
      UInt(metaEntryBits.W),
      set=nSets/2,
      way=nWays,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))

    //meta connection
    if(bank == 0) {
      tagArray.io.r.req.valid := port_0_read_0 || port_1_read_0
      tagArray.io.r.req.bits.apply(setIdx=bank_0_idx(highestIdxBit,1))
      tagArray.io.w.req.valid := write_bank_0
      tagArray.io.w.req.bits.apply(data=write_meta_bits, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }
    else {
      tagArray.io.r.req.valid := port_0_read_1 || port_1_read_1
      tagArray.io.r.req.bits.apply(setIdx=bank_1_idx(highestIdxBit,1))
      tagArray.io.w.req.valid := write_bank_1
      tagArray.io.w.req.bits.apply(data=write_meta_bits, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }

    tagArray
  }

  val read_set_idx_next = RegEnable(io.read.bits.vSetIdx, io.read.fire)
  val valid_array = RegInit(VecInit(Seq.fill(nWays)(0.U(nSets.W))))
  val valid_metas = Wire(Vec(PortNumber, Vec(nWays, Bool())))
  // valid read
  (0 until PortNumber).foreach( i =>
    (0 until nWays).foreach( way =>
      valid_metas(i)(way) := valid_array(way)(read_set_idx_next(i))
    ))
  io.readResp.entryValid := valid_metas

  io.read.ready := !io.write.valid && !io.fencei && tagArrays.map(_.io.r.req.ready).reduce(_&&_)

  //Parity Decode
  val read_metas = Wire(Vec(2,Vec(nWays,new ICacheMetadata())))
  for((tagArray,i) <- tagArrays.zipWithIndex){
    val read_meta_bits = tagArray.io.r.resp.asTypeOf(Vec(nWays,UInt(metaEntryBits.W)))
    val read_meta_decoded = read_meta_bits.map{ way_bits => cacheParams.tagCode.decode(way_bits)}
    val read_meta_wrong = read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.error}
    val read_meta_corrected = VecInit(read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.corrected})
    read_metas(i) := read_meta_corrected.asTypeOf(Vec(nWays,new ICacheMetadata()))
    (0 until nWays).map{ w => io.readResp.errors(i)(w) := RegNext(read_meta_wrong(w)) && RegNext(RegNext(io.read.fire))}
  }

  //Parity Encode
  val write = io.write.bits
  write_meta_bits := cacheParams.tagCode.encode(ICacheMetadata(tag = write.phyTag).asUInt)

  // valid write
  val way_num = OHToUInt(io.write.bits.waymask)
  when (io.write.valid) {
    valid_array(way_num) := valid_array(way_num).bitSet(io.write.bits.virIdx, true.B)
  }

  XSPerfAccumulate("meta_refill_num", io.write.valid)

  io.readResp.metaData <> DontCare
  when(port_0_read_0_reg){
    io.readResp.metaData(0) := read_metas(0)
  }.elsewhen(port_0_read_1_reg){
    io.readResp.metaData(0) := read_metas(1)
  }

  when(port_1_read_0_reg){
    io.readResp.metaData(1) := read_metas(0)
  }.elsewhen(port_1_read_1_reg){
    io.readResp.metaData(1) := read_metas(1)
  }


  io.write.ready := true.B // TODO : has bug ? should be !io.cacheOp.req.valid

  // fencei logic : reset valid_array
  when (io.fencei) {
    (0 until nWays).foreach( way =>
      valid_array(way) := 0.U
    )
  }
}



class ICacheDataArray(implicit p: Parameters) extends ICacheArray
{
  require(nBanks == 2)
  val dataBits = blockBits / nBanks
  val codeBits = dataCodeEntryBits
  val dataEntryBits = dataBits + codeBits
  
  def getECCFromEncUnit(encUnit: UInt) = {
    require(encUnit.getWidth == encDataUnitBits)
    if (encDataUnitBits == dataCodeUnit) {
      0.U.asTypeOf(UInt(1.W))
    } else {
      encUnit(encDataUnitBits - 1, dataCodeUnit)
    }
  }

  def getECCFromBlock(cacheblock: UInt) = {
    // require(cacheblock.getWidth == blockBits)
    VecInit((0 until dataCodeUnitNum).map { w =>
      val unit = cacheblock(dataCodeUnit * (w + 1) - 1, dataCodeUnit * w)
      getECCFromEncUnit(cacheParams.dataCode.encode(unit))
    })
  }

  class ICacheDataEntry(implicit p: Parameters) extends ICacheBundle {
    val data = UInt(dataBits.W)
    val code = UInt(codeBits.W)

    def decode()(implicit p: Parameters) = {
      val datas = this.data.asTypeOf(Vec(dataCodeUnitNum, UInt(dataCodeUnit.W)))
      val codes = this.code.asTypeOf(Vec(dataCodeUnitNum, UInt(dataCodeBits.W)))
      val full = VecInit((0 until dataCodeUnitNum).map(i => Cat(codes(i), datas(i))))
      val errors = VecInit(full.map(cacheParams.dataCode.decode(_).error))
      val error = errors.reduce(_||_)
      error
    }
  }

  object ICacheDataEntry {
    def apply(data: UInt)(implicit p: Parameters) = {
      require(data.getWidth == dataBits)
      val entry = Wire(new ICacheDataEntry)
      entry.data := data
      entry.code := getECCFromBlock(data).asUInt
      entry
    }
  }

  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheDataWriteBundle))
    val read     = Flipped(Vec(nWays, DecoupledIO(new ICacheReadBundle)))
    val readResp = Output(new ICacheDataRespBundle)
  }}

  dontTouch(io)

  /**
    ******************************************************************************
    * data array
    ******************************************************************************
    */
  val data_no_code = io.write.bits.data.asTypeOf(Vec(nBanks, UInt(dataBits.W)))
  // encode
  val write_data_bits = data_no_code.map(ICacheDataEntry(_).asUInt)
  
  val dataArrays = (0 until nWays).map{ way =>
    (0 until nBanks).map { bank =>
      val sramBank = Module(new SRAMTemplate(
        UInt(dataEntryBits.W),
        set=nSets,
        shouldReset = true,
        holdRead = true,
        singlePort = true
      ))
      // SRAM read logic
      if (bank == 1) {
        sramBank.io.r.req.bits.apply(setIdx= io.read(way).bits.vSetIdx(0))
        sramBank.io.r.req.valid := io.read(way).valid && io.read(way).bits.waymask(0)(way)
      } else {
        // read low of startline if cross cacheline
        val setIdx = Mux(io.read(way).bits.isDoubleLine, io.read(way).bits.vSetIdx(1), io.read(way).bits.vSetIdx(0))
        val mask   = Mux(io.read(way).bits.isDoubleLine, io.read(way).bits.waymask(1), io.read(way).bits.waymask(0))
        sramBank.io.r.req.bits.apply(setIdx= setIdx)
        sramBank.io.r.req.valid := io.read(way).valid && mask(way)
      }

      // SRAM write logic
      val waymask = io.write.bits.waymask.asTypeOf(Vec(nWays, Bool()))(way)
      // waymask is invalid when way of SRAMTemplate is 1
      sramBank.io.w.req.valid := io.write.valid && waymask.asUInt.orR
      sramBank.io.w.req.bits.apply(
        data    = write_data_bits(bank),
        setIdx  = io.write.bits.virIdx,
        waymask = waymask.asUInt
      )
      sramBank
    }
  }

  /**
    ******************************************************************************
    * read logic
    ******************************************************************************
    */
  val isDoubleLineReg = RegEnable(io.read.last.bits.isDoubleLine, io.read.last.fire)
  val read_data_with_code  = Wire(Vec(nBanks, UInt(dataEntryBits.W)))
  // get bank waymask
  val read_waymask_temp = VecInit(Seq(Mux(io.read.last.bits.isDoubleLine, io.read.last.bits.waymask(1), io.read.last.bits.waymask(0)),
                                      io.read.last.bits.waymask(0)))
  val read_waymask = RegEnable(read_waymask_temp, io.read.last.fire)
                                
  val read_bank_data  = (0 until nBanks).map(i => Mux1H(read_waymask(i).asUInt, dataArrays.map(bankArrays => bankArrays(i).io.r.resp.asUInt)))
  read_data_with_code(0) := Mux(isDoubleLineReg, read_bank_data(1), read_bank_data(0))
  read_data_with_code(1) := Mux(isDoubleLineReg, read_bank_data(0), read_bank_data(1))

  val read_data_entry = read_data_with_code.map(_.asTypeOf(new ICacheDataEntry()))
  val read_datas      = read_data_entry.map(_.data)
  val read_errors     = read_data_entry.map(_.decode())

  /**
    ******************************************************************************
    * IO
    ******************************************************************************
    */
  io.readResp.datas   := read_datas
  io.readResp.errors  := read_errors
  io.write.ready      := true.B
  io.read.foreach( _.ready := !io.write.valid)
}


class ICacheReplacer(implicit p: Parameters) extends ICacheModule {
  val io = IO(new Bundle {
    val touch   = Vec(PortNumber, Flipped(ValidIO(new ReplacerTouch)))
    val victim  = Flipped(new ReplacerVictim)
  })

  val replacers = Seq.fill(PortNumber)(ReplacementPolicy.fromString(cacheParams.replacer,nWays,nSets/PortNumber))

  // touch
  val touch_sets = Seq.fill(PortNumber)(Wire(Vec(2, UInt(log2Ceil(nSets/2).W))))
  val touch_ways = Seq.fill(PortNumber)(Wire(Vec(2, Valid(UInt(log2Ceil(nWays).W)))))
  (0 until PortNumber).foreach {i =>
    touch_sets(i)(0)        := Mux(io.touch(i).bits.vSetIdx(0), io.touch(1).bits.vSetIdx(highestIdxBit, 1), io.touch(0).bits.vSetIdx(highestIdxBit, 1))
    touch_ways(i)(0).bits   := Mux(io.touch(i).bits.vSetIdx(0), io.touch(1).bits.way, io.touch(0).bits.way)
    touch_ways(i)(0).valid  := Mux(io.touch(i).bits.vSetIdx(0), io.touch(1).valid, io.touch(0).valid)
  }

  // victim
  io.victim.way := Mux(io.victim.vSetIdx.bits(0),
                       replacers(1).way(io.victim.vSetIdx.bits(highestIdxBit, 1)),
                       replacers(0).way(io.victim.vSetIdx.bits(highestIdxBit, 1)))

  // touch the victim in next cycle
  val victim_vSetIdx_reg  = RegEnable(io.victim.vSetIdx.bits, io.victim.vSetIdx.valid)
  val victim_way_reg      = RegEnable(io.victim.way,          io.victim.vSetIdx.valid)
  (0 until PortNumber).foreach {i =>
    touch_sets(i)(1)        := victim_vSetIdx_reg(highestIdxBit, 1)
    touch_ways(i)(1).bits   := victim_way_reg
    touch_ways(i)(1).valid  := RegNext(io.victim.vSetIdx.valid) && (victim_vSetIdx_reg(0) === i.U)
  }

  ((replacers zip touch_sets) zip touch_ways).map{case ((r, s),w) => r.access(s,w)}
}

class ICacheIO(implicit p: Parameters) extends ICacheBundle
{
  val hartId = Input(UInt(8.W))
  val prefetch    = Flipped(new FtqToPrefetchIO)
  val stop        = Input(Bool())
  val fetch       = new ICacheMainPipeBundle
  val toIFU       = Output(Bool())
  val pmp         = Vec(PortNumber + prefetchPipeNum, new ICachePMPBundle)
  val itlb        = Vec(PortNumber, new TlbRequestIO)
  val perfInfo    = Output(new ICachePerfInfo)
  val error       = new L1CacheErrorInfo
  /* CSR control signal */
  val csr_pf_enable = Input(Bool())
  val csr_parity_enable = Input(Bool())
  val fencei      = Input(Bool())
  val flush       = Input(Bool())
}

class ICache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {
  override def shouldBeInlined: Boolean = false

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, cacheParams.nFetchMshr + cacheParams.nPrefetchMshr + 1),
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
  println("  ICacheSets: "          + cacheParams.nSets)
  println("  ICacheWays: "          + cacheParams.nWays)
  println("  ICacheBanks: "         + PortNumber)

  println("  enableICachePrefetch:     " + cacheParams.enableICachePrefetch)
  println("  prefetchToL1:       " + cacheParams.prefetchToL1)
  println("  prefetchPipeNum:    " + cacheParams.prefetchPipeNum)
  println("  nPrefetchEntries:   " + cacheParams.nPrefetchEntries)
  println("  nPrefBufferEntries: " + cacheParams.nPrefBufferEntries)
  println("  maxIPFMoveConf:     " + cacheParams.maxIPFMoveConf)

  val (bus, edge) = outer.clientNode.out.head

  val metaArray         = Module(new ICacheMetaArray)
  val dataArray         = Module(new ICacheDataArray)
  val mainPipe          = Module(new ICacheMainPipe)
  val missUnit          = Module(new ICacheMissUnit(edge))
  val replacer          = Module(new ICacheReplacer)
  val prefetcher        = Module(new IPrefetchPipe)
  val wayLookup         = Module(new WayLookup)

  dataArray.io.write    <> missUnit.io.data_write
  dataArray.io.read     <> mainPipe.io.dataArray.toIData
  dataArray.io.readResp <> mainPipe.io.dataArray.fromIData

  metaArray.io.fencei   := io.fencei
  metaArray.io.write    <> missUnit.io.meta_write
  metaArray.io.read     <> prefetcher.io.metaRead.toIMeta
  metaArray.io.readResp <> prefetcher.io.metaRead.fromIMeta

  prefetcher.io.flush             := io.flush
  prefetcher.io.csr_pf_enable     := io.csr_pf_enable
  prefetcher.io.ftqReq            <> io.prefetch
  prefetcher.io.MSHRResp          := missUnit.io.fetch_resp

  missUnit.io.hartId            := io.hartId
  missUnit.io.fencei            := io.fencei
  missUnit.io.flush             := io.flush
  missUnit.io.fetch_req         <> mainPipe.io.mshr.req
  missUnit.io.prefetch_req      <> prefetcher.io.MSHRReq
  missUnit.io.mem_grant.valid   := false.B
  missUnit.io.mem_grant.bits    := DontCare
  missUnit.io.mem_grant         <> bus.d

  mainPipe.io.flush             := io.flush
  mainPipe.io.respStall         := io.stop
  mainPipe.io.csr_parity_enable := io.csr_parity_enable
  mainPipe.io.hartId            := io.hartId
  mainPipe.io.mshr.resp         := missUnit.io.fetch_resp
  mainPipe.io.fetch.req         <> io.fetch.req
  mainPipe.io.wayLookupRead     <> wayLookup.io.read

  wayLookup.io.flush            := io.flush
  wayLookup.io.write            <> prefetcher.io.wayLookupWrite
  wayLookup.io.update           := missUnit.io.fetch_resp

  replacer.io.touch   <> mainPipe.io.touch
  replacer.io.victim  <> missUnit.io.victim

  io.pmp(0) <> mainPipe.io.pmp(0)
  io.pmp(1) <> mainPipe.io.pmp(1)
  io.pmp(2) <> prefetcher.io.pmp(0)
  io.pmp(3) <> prefetcher.io.pmp(1)

  io.itlb(0) <> prefetcher.io.itlb(0)
  io.itlb(1) <> prefetcher.io.itlb(1)

  //notify IFU that Icache pipeline is available
  io.toIFU := mainPipe.io.fetch.req.ready
  io.perfInfo := mainPipe.io.perfInfo

  io.fetch.resp              <> mainPipe.io.fetch.resp
  io.fetch.topdownIcacheMiss := mainPipe.io.fetch.topdownIcacheMiss
  io.fetch.topdownItlbMiss   := mainPipe.io.fetch.topdownItlbMiss

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  bus.a <> missUnit.io.mem_acquire

  //Parity error port
  val errors = mainPipe.io.errors
  io.error <> RegNext(Mux1H(errors.map(e => e.valid -> e)))

  val perfEvents = Seq(
    ("icache_miss_cnt  ", false.B),
    ("icache_miss_penalty", BoolStopWatch(start = false.B, stop = false.B || false.B, startHighPriority = true)),
  )
  generatePerfEvent()
}

class ICachePartWayReadBundle[T <: Data](gen: T, pWay: Int)(implicit p: Parameters)
  extends ICacheBundle
{
  val req = Flipped(Vec(PortNumber, Decoupled(new Bundle{
    val ridx = UInt((log2Ceil(nSets) - 1).W)
  })))
  val resp = Output(new Bundle{
    val rdata  = Vec(PortNumber,Vec(pWay, gen))
  })
}

class ICacheWriteBundle[T <: Data](gen: T, pWay: Int)(implicit p: Parameters)
  extends ICacheBundle
{
  val wdata = gen
  val widx = UInt((log2Ceil(nSets) - 1).W)
  val wbankidx = Bool()
  val wmask = Vec(pWay, Bool())
}

class ICachePartWayArray[T <: Data](gen: T, pWay: Int)(implicit p: Parameters) extends ICacheArray
{

  //including part way data
  val io = IO{new Bundle {
    val read      = new  ICachePartWayReadBundle(gen,pWay)
    val write     = Flipped(ValidIO(new ICacheWriteBundle(gen, pWay)))
  }}

  io.read.req.map(_.ready := !io.write.valid)

  val srams = (0 until PortNumber) map { bank =>
    val sramBank = Module(new SRAMTemplate(
      gen,
      set=nSets/2,
      way=pWay,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))

    sramBank.io.r.req.valid := io.read.req(bank).valid
    sramBank.io.r.req.bits.apply(setIdx= io.read.req(bank).bits.ridx)

    if(bank == 0) sramBank.io.w.req.valid := io.write.valid && !io.write.bits.wbankidx
    else sramBank.io.w.req.valid := io.write.valid && io.write.bits.wbankidx
    sramBank.io.w.req.bits.apply(data=io.write.bits.wdata, setIdx=io.write.bits.widx, waymask=io.write.bits.wmask.asUInt)

    sramBank
  }

  io.read.req.map(_.ready := !io.write.valid && srams.map(_.io.r.req.ready).reduce(_&&_))

  io.read.resp.rdata := VecInit(srams.map(bank => bank.io.r.resp.asTypeOf(Vec(pWay,gen))))

}
