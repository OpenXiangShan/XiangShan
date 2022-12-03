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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import huancun.{AliasField, DirtyField, PreferCacheField, PrefetchField}
import xiangshan._
import xiangshan.frontend._
import xiangshan.cache._
import utils._
import xiangshan.backend.fu.PMPReqBundle
import xiangshan.cache.mmu.{TlbRequestIO, TlbReq}

case class ICacheParameters(
    nSets: Int = 256,
    nWays: Int = 8,
    rowBits: Int = 64,
    nTLBEntries: Int = 32,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    replacer: Option[String] = Some("random"),
    nMissEntries: Int = 2,
    nReleaseEntries: Int = 1,
    nProbeEntries: Int = 2,
    nPrefetchEntries: Int = 4,
    nPrefBufferEntries: Int = 8,
    hasPrefetch: Boolean = false,
    nMMIOs: Int = 1,
    blockBytes: Int = 64
)extends L1CacheParameters {

  val setBytes = nSets * blockBytes
  val aliasBitsOpt = if(setBytes > pageSize) Some(log2Ceil(setBytes / pageSize)) else None
  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    PreferCacheField()
  ) ++ aliasBitsOpt.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Seq(DirtyField())
  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = ReplacementPolicy.fromString(replacer,nWays,nSets)
}

trait HasICacheParameters extends HasL1CacheParameters with HasInstrMMIOConst with HasIFUConst{
  val cacheParams = icacheParameters
  val dataCodeUnit = 16
  val dataCodeUnitNum  = blockBits/dataCodeUnit

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

  def ReplacePipeKey = 0
  def MainPipeKey = 1
  def PortNumber = 2
  def ProbeKey   = 3

  def nPrefetchEntries = cacheParams.nPrefetchEntries
  def nIPFBufferSize   = cacheParams.nPrefBufferEntries
  def maxIPFMoveConf   = blockBytes/(instBytes*FetchWidth)

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

  def generateState(enable: Bool, release: Bool): Bool = {
    val stateReg = RegInit(false.B)
    val state    = Wire(Bool())
    when(enable)       {stateReg := true.B }
      .elsewhen(release && stateReg) {stateReg := false.B}

    state := stateReg || enable

    state
  }
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
  val coh = new ClientMetadata
  val tag = UInt(tagBits.W)
}

object ICacheMetadata {
  def apply(tag: Bits, coh: ClientMetadata)(implicit p: Parameters) = {
    val meta = Wire(new L1Metadata)
    meta.tag := tag
    meta.coh := coh
    meta
  }
}


class ICacheMetaArray()(implicit p: Parameters) extends ICacheArray
{
  def onReset = ICacheMetadata(0.U, ClientMetadata.onReset)
  val metaBits = onReset.getWidth
  val metaEntryBits = cacheParams.tagCode.width(metaBits)

  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheMetaWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle))
    val readResp = Output(new ICacheMetaRespBundle)
    val cacheOp  = Flipped(new L1CacheInnerOpIO) // customized cache op port
  }}

  io.read.ready := !io.write.valid

  val port_0_read_0 = io.read.valid  && !io.read.bits.vSetIdx(0)(0)
  val port_0_read_1 = io.read.valid  &&  io.read.bits.vSetIdx(0)(0)
  val port_1_read_1  = io.read.valid &&  io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine
  val port_1_read_0  = io.read.valid && !io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine

  val port_0_read_0_reg = RegEnable(port_0_read_0, io.read.fire())
  val port_0_read_1_reg = RegEnable(port_0_read_1, io.read.fire())
  val port_1_read_1_reg = RegEnable(port_1_read_1, io.read.fire())
  val port_1_read_0_reg = RegEnable(port_1_read_0, io.read.fire())

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

  val readIdxNext = RegEnable(next = io.read.bits.vSetIdx, enable = io.read.fire())
  val validArray = RegInit(0.U((nSets * nWays).W))
  val validMetas = VecInit((0 until 2).map{ bank =>
    val validMeta =  Cat((0 until nWays).map{w => validArray( Cat(readIdxNext(bank), w.U(log2Ceil(nWays).W)) )}.reverse).asUInt
    validMeta
  })

  io.read.ready := !io.write.valid && tagArrays.map(_.io.r.req.ready).reduce(_&&_)

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
  write_meta_bits := cacheParams.tagCode.encode(ICacheMetadata(tag = write.phyTag, coh = write.coh).asUInt)

  val wayNum   = OHToUInt(io.write.bits.waymask)
  val validPtr = Cat(io.write.bits.virIdx, wayNum)
  when(io.write.valid){
    validArray := validArray.bitSet(validPtr, true.B)
  }

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

  io.readResp.entryValid := validMetas.asTypeOf(Vec(2, Vec(nWays, Bool())))


  io.write.ready := true.B
  // deal with customized cache op
  require(nWays <= 32)
  io.cacheOp.resp.bits := DontCare
  val cacheOpShouldResp = WireInit(false.B)
  when(io.cacheOp.req.valid){
    when(
      CacheInstrucion.isReadTag(io.cacheOp.req.bits.opCode) ||
      CacheInstrucion.isReadTagECC(io.cacheOp.req.bits.opCode)
    ){
      for (i <- 0 until 2) {
        tagArrays(i).io.r.req.valid := true.B
        tagArrays(i).io.r.req.bits.apply(setIdx = io.cacheOp.req.bits.index)
      }
      cacheOpShouldResp := true.B
    }
    when(CacheInstrucion.isWriteTag(io.cacheOp.req.bits.opCode)){
      for (i <- 0 until 2) {
        tagArrays(i).io.w.req.valid := true.B
        tagArrays(i).io.w.req.bits.apply(
          data = io.cacheOp.req.bits.write_tag_low,
          setIdx = io.cacheOp.req.bits.index,
          waymask = UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
        )
      }
      cacheOpShouldResp := true.B
    }
  }
  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  io.cacheOp.resp.bits.read_tag_low := Mux(io.cacheOp.resp.valid,
    tagArrays(0).io.r.resp.asTypeOf(Vec(nWays, UInt(tagBits.W)))(io.cacheOp.req.bits.wayNum),
    0.U
  )
  io.cacheOp.resp.bits.read_tag_ecc := DontCare // TODO
  // TODO: deal with duplicated array
}


class ICacheDataArray(implicit p: Parameters) extends ICacheArray
{

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

  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheDataWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle))
    val readResp = Output(new ICacheDataRespBundle)
    val cacheOp  = Flipped(new L1CacheInnerOpIO) // customized cache op port
  }}

  val port_0_read_0 = io.read.valid  && !io.read.bits.vSetIdx(0)(0)
  val port_0_read_1 = io.read.valid  &&  io.read.bits.vSetIdx(0)(0)
  val port_1_read_1  = io.read.valid &&  io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine
  val port_1_read_0  = io.read.valid && !io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine

  val port_0_read_1_reg = RegEnable(port_0_read_1, io.read.fire())
  val port_1_read_0_reg = RegEnable(port_1_read_0, io.read.fire())

  val bank_0_idx = Mux(port_0_read_0, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  val bank_1_idx = Mux(port_0_read_1, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  val bank_idx   = Seq(bank_0_idx, bank_1_idx)

  val write_bank_0 = WireInit(io.write.valid && !io.write.bits.bankIdx)
  val write_bank_1 = WireInit(io.write.valid &&  io.write.bits.bankIdx)

  val write_data_bits = Wire(UInt(blockBits.W))
  val write_data_code = Wire(UInt(dataCodeEntryBits.W))

  val dataArrays = (0 until 2) map { i =>
    val dataArray = Module(new SRAMTemplate(
      UInt(blockBits.W),
      set=nSets/2,
      way=nWays,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))

    if(i == 0) {
      dataArray.io.r.req.valid := port_0_read_0 || port_1_read_0
      dataArray.io.r.req.bits.apply(setIdx=bank_0_idx(highestIdxBit,1))
      dataArray.io.w.req.valid := write_bank_0
      dataArray.io.w.req.bits.apply(data=write_data_bits, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }
    else {
      dataArray.io.r.req.valid := port_0_read_1 || port_1_read_1
      dataArray.io.r.req.bits.apply(setIdx=bank_1_idx(highestIdxBit,1))
      dataArray.io.w.req.valid := write_bank_1
      dataArray.io.w.req.bits.apply(data=write_data_bits, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }

    dataArray
  }

  val codeArrays = (0 until 2) map { i =>
    val codeArray = Module(new SRAMTemplate(
      UInt(dataCodeEntryBits.W),
      set=nSets/2,
      way=nWays,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))

    if(i == 0) {
      codeArray.io.r.req.valid := port_0_read_0 || port_1_read_0
      codeArray.io.r.req.bits.apply(setIdx=bank_0_idx(highestIdxBit,1))
      codeArray.io.w.req.valid := write_bank_0
      codeArray.io.w.req.bits.apply(data=write_data_code, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }
    else {
      codeArray.io.r.req.valid := port_0_read_1 || port_1_read_1
      codeArray.io.r.req.bits.apply(setIdx=bank_1_idx(highestIdxBit,1))
      codeArray.io.w.req.valid := write_bank_1
      codeArray.io.w.req.bits.apply(data=write_data_code, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }

    codeArray
  }

  io.read.ready := !io.write.valid && dataArrays.map(_.io.r.req.ready).reduce(_ && _) && codeArrays.map(_.io.r.req.ready).reduce(_ && _)

  //Parity Decode
  val read_datas = Wire(Vec(2,Vec(nWays,UInt(blockBits.W) )))
  val read_codes = Wire(Vec(2,Vec(nWays,UInt(dataCodeEntryBits.W) )))
  for(((dataArray,codeArray),i) <- dataArrays.zip(codeArrays).zipWithIndex){
    read_datas(i) := dataArray.io.r.resp.asTypeOf(Vec(nWays,UInt(blockBits.W)))
    read_codes(i) := codeArray.io.r.resp.asTypeOf(Vec(nWays,UInt(dataCodeEntryBits.W)))
  }


  //Parity Encode
  val write = io.write.bits
  val write_data = WireInit(write.data)
  write_data_code := getECCFromBlock(write_data).asUInt
  write_data_bits := write_data

  io.readResp.datas(0) := Mux( port_0_read_1_reg, read_datas(1) , read_datas(0))
  io.readResp.datas(1) := Mux( port_1_read_0_reg, read_datas(0) , read_datas(1))
  io.readResp.codes(0) := Mux( port_0_read_1_reg, read_codes(1) , read_codes(0))
  io.readResp.codes(1) := Mux( port_1_read_0_reg, read_codes(0) , read_codes(1))

  io.write.ready := true.B

  // deal with customized cache op
  require(nWays <= 32)
  io.cacheOp.resp.bits := DontCare
  val cacheOpShouldResp = WireInit(false.B)
  when(io.cacheOp.req.valid){
    when(
      CacheInstrucion.isReadData(io.cacheOp.req.bits.opCode) ||
      CacheInstrucion.isReadDataECC(io.cacheOp.req.bits.opCode)
    ){
      (0 until 2).map(i => {
        dataArrays(i).io.r.req.valid := true.B
        dataArrays(i).io.r.req.bits.apply(setIdx = io.cacheOp.req.bits.index)
      })
      cacheOpShouldResp := true.B
    }
    when(CacheInstrucion.isWriteData(io.cacheOp.req.bits.opCode)){
      (0 until 2).map(i => {
        dataArrays(i).io.w.req.valid := io.cacheOp.req.bits.bank_num === i.U
        dataArrays(i).io.w.req.bits.setIdx := io.cacheOp.req.bits.index
        dataArrays(i).io.w.req.bits.waymask match {
          case Some(waymask) => waymask := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
          case None =>
        }
      })
      write_data := io.cacheOp.req.bits.write_data_vec.asTypeOf(write_data.cloneType)
      cacheOpShouldResp := true.B
    }
  }
  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  val dataresp = Mux(io.cacheOp.req.bits.bank_num(0).asBool,
    read_datas(1),
    read_datas(0)
  )

  val numICacheLineWords = blockBits / 64
  require(blockBits >= 64 && isPow2(blockBits))
  for (wordIndex <- 0 until numICacheLineWords) {
    io.cacheOp.resp.bits.read_data_vec(wordIndex) := dataresp(io.cacheOp.req.bits.wayNum(4, 0))(64*(wordIndex+1)-1, 64*wordIndex)
  }
  // io.cacheOp.resp.bits.read_data_ecc := Mux(io.cacheOp.resp.valid,
    // bank_result(io.cacheOp.req.bits.bank_num).ecc,
    // 0.U
  // )
}


class ICacheIO(implicit p: Parameters) extends ICacheBundle
{
  val hartId = Input(UInt(8.W))
  val prefetch    = Flipped(new FtqPrefechBundle)
  val stop        = Input(Bool())
  val fetch       = Vec(PortNumber, new ICacheMainPipeBundle)
  val pmp         = Vec(PortNumber + 1, new ICachePMPBundle)
  val itlb        = Vec(PortNumber + 1, new TlbRequestIO)
  val perfInfo    = Output(new ICachePerfInfo)
  val error       = new L1CacheErrorInfo
  /* Cache Instruction */
  val csr         = new L1CacheToCsrIO
  /* CSR control signal */
  val csr_pf_enable = Input(Bool())
  val csr_parity_enable = Input(Bool())
  val fencei = Input(Bool())
}

class ICache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, cacheParams.nMissEntries + cacheParams.nReleaseEntries + cacheParams.nPrefetchEntries),
      supportsProbe = TransferSizes(blockBytes),
      supportsHint = TransferSizes(blockBytes)
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
  println("  ICacheSets: " + cacheParams.nSets)
  println("  ICacheWays: " + cacheParams.nWays)
  println("  ICacheBanks: " + PortNumber)
  println("  hasPrefetch: " + cacheParams.hasPrefetch)
  if (cacheParams.hasPrefetch) {
    println("  nPrefetchEntries: " + cacheParams.nPrefetchEntries)
    println("  nPrefetchBufferEntries: " + cacheParams.nPrefBufferEntries)
  }

  val (bus, edge) = outer.clientNode.out.head

  val metaArray = Module(new ICacheMetaArray)
  val metaArrayCopy = Module(new ICacheMetaArray)
  val dataArray = Module(new ICacheDataArray)
  val mainPipe = Module(new ICacheMainPipe)
  val missUnit = Module(new ICacheMissUnit(edge))
  val prefetchPipe = Module(new IPrefetchPipe)
  val ipfBuffer  = Module(new PrefetchBuffer)

  val meta_read_arb = Module(new Arbiter(new ICacheReadBundle, 1))
  val data_read_arb = Module(new Arbiter(new ICacheReadBundle, 1))
  val meta_write_arb = Module(new Arbiter(new ICacheMetaWriteBundle(), 2))
  val data_write_arb = Module(new Arbiter(new ICacheDataWriteBundle(), 2))
  // val tlb_req_arb     = Module(new Arbiter(new TlbReq, 2))

  mainPipe.io.PIQ <> missUnit.io.to_main_pipe
  ipfBuffer.io.read <> mainPipe.io.iprefetchBuf
  meta_write_arb.io.in(1) <> ipfBuffer.io.move.meta_write
  data_write_arb.io.in(1) <> ipfBuffer.io.move.data_write

  ipfBuffer.io.fencei := io.fencei
  missUnit.io.fencei := io.fencei

  ipfBuffer.io.write <> missUnit.io.piq_write_ipbuffer

  meta_read_arb.io.in(0) <> mainPipe.io.metaArray.toIMeta
  metaArray.io.read <> meta_read_arb.io.out
  metaArrayCopy.io.read <> prefetchPipe.io.toIMeta

  mainPipe.io.metaArray.fromIMeta <> metaArray.io.readResp
  prefetchPipe.io.fromIMeta <> metaArrayCopy.io.readResp

  data_read_arb.io.in(0) <> mainPipe.io.dataArray.toIData
  dataArray.io.read <> data_read_arb.io.out
  mainPipe.io.dataArray.fromIData <> dataArray.io.readResp

  mainPipe.io.respStall := io.stop
  io.perfInfo := mainPipe.io.perfInfo

  meta_write_arb.io.in(0) <> missUnit.io.meta_write
  data_write_arb.io.in(0) <> missUnit.io.data_write

  metaArray.io.write <> meta_write_arb.io.out
  metaArrayCopy.io.write <> meta_write_arb.io.out
  dataArray.io.write <> data_write_arb.io.out

  mainPipe.io.csr_parity_enable := io.csr_parity_enable

  if (cacheParams.hasPrefetch) {
    prefetchPipe.io.fromFtq <> io.prefetch
    when(!io.csr_pf_enable) {
      prefetchPipe.io.fromFtq.req.valid := false.B
      io.prefetch.req.ready := true.B
    }
  } else {
    prefetchPipe.io.fromFtq <> DontCare
  }

  io.pmp(0) <> mainPipe.io.pmp(0)
  io.pmp(1) <> mainPipe.io.pmp(1)
  io.pmp(2) <> prefetchPipe.io.pmp

  prefetchPipe.io.prefetchEnable := mainPipe.io.prefetchEnable
  prefetchPipe.io.prefetchDisable := mainPipe.io.prefetchDisable

  io.itlb(0) <> mainPipe.io.itlb(0)
  io.itlb(1) <> mainPipe.io.itlb(1)
  io.itlb(2) <> prefetchPipe.io.iTLBInter

  for (i <- 0 until PortNumber) {
    io.fetch(i).resp <> mainPipe.io.fetch(i).resp

    missUnit.io.req(i) <> mainPipe.io.mshr(i).toMSHR
    mainPipe.io.mshr(i).fromMSHR <> missUnit.io.resp(i)

  }

  missUnit.io.prefetch_req <> prefetchPipe.io.toMissUnit.enqReq
  missUnit.io.hartId := io.hartId
  prefetchPipe.io.fromMSHR <> missUnit.io.prefetch_check

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits := DontCare
  bus.e.valid := false.B
  bus.e.bits := DontCare

  bus.a <> missUnit.io.mem_acquire
  bus.e <> missUnit.io.mem_finish

  //Probe through bus b

  //Parity error port
  val errors = mainPipe.io.errors
  io.error <> RegNext(Mux1H(errors.map(e => e.valid -> e)))

  (0 until PortNumber).map { i =>
    mainPipe.io.fetch(i).req.valid := io.fetch(i).req.valid //&& !fetchShouldBlock(i)
    io.fetch(i).req.ready := mainPipe.io.fetch(i).req.ready //&& !fetchShouldBlock(i)
    mainPipe.io.fetch(i).req.bits := io.fetch(i).req.bits
  }

  bus.d.ready := false.B

  missUnit.io.mem_grant <> bus.d

  val perfEvents = Seq(
    ("icache_miss_cnt  ", false.B),
    ("icache_miss_penty", BoolStopWatch(start = false.B, stop = false.B || false.B, startHighPriority = true)),
  )
  generatePerfEvent()

  // Customized csr cache op support
  val cacheOpDecoder = Module(new CSRCacheOpDecoder("icache", CacheInstrucion.COP_ID_ICACHE))
  cacheOpDecoder.io.csr <> io.csr
  dataArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
  metaArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
  metaArrayCopy.io.cacheOp.req := cacheOpDecoder.io.cache.req
  cacheOpDecoder.io.cache.resp.valid :=
    dataArray.io.cacheOp.resp.valid ||
    metaArray.io.cacheOp.resp.valid
  cacheOpDecoder.io.cache.resp.bits := Mux1H(List(
    dataArray.io.cacheOp.resp.valid -> dataArray.io.cacheOp.resp.bits,
    metaArray.io.cacheOp.resp.valid -> metaArray.io.cacheOp.resp.bits,
  ))
  cacheOpDecoder.io.error := io.error
  assert(!((dataArray.io.cacheOp.resp.valid +& metaArray.io.cacheOp.resp.valid) > 1.U))
}
