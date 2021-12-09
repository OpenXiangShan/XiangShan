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
import huancun.{AliasField, PreferCacheField, PrefetchField,DirtyField}
import xiangshan._
import xiangshan.frontend._
import xiangshan.cache._
import utils._
import xiangshan.cache.mmu.BlockTlbRequestIO

case class ICacheParameters(
    nSets: Int = 256,
    nWays: Int = 8,
    rowBits: Int = 64,
    nTLBEntries: Int = 32,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    replacer: Option[String] = Some("random"),
    nMissEntries: Int = 2,
    nReleaseEntries: Int = 2,
    nProbeEntries: Int = 2,
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
  val dataCodeUnit = 8
  val dataUnitNum  = blockBits/dataCodeUnit

  def highestIdxBit = log2Ceil(nSets) - 1
  def dataCodeBits  = cacheParams.dataCode.width(dataCodeUnit)
  def dataEntryBits = dataCodeBits * dataUnitNum

  val ICacheSets = cacheParams.nSets
  val ICacheWays = cacheParams.nWays

  val ICacheSameVPAddrLength = 12
  val ReplaceIdWid = 5

  val ICacheWordOffset = 0
  val ICacheSetOffset = ICacheWordOffset + log2Up(blockBytes)
  val ICacheAboveIndexOffset = ICacheSetOffset + log2Up(ICacheSets)
  val ICacheTagOffset = ICacheAboveIndexOffset min ICacheSameVPAddrLength

  def ReplacePipeKey = 0
  def mainPipeKey = 1
  def ReleaseKey = 2
  def MissQueueKey = 3
  def ProbeKey = 4

  def PortNumber = 2

  def nMissEntries = cacheParams.nMissEntries

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

  require(isPow2(nMissEntries), s"nMissEntries($nMissEntries) must be pow2")
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
    val fencei   = Input(Bool())
    val cacheOp  = Flipped(new DCacheInnerOpIO) // customized cache op port 
  }}

  io.read.ready := !io.write.valid

  val port_0_read_0 = io.read.valid  && !io.read.bits.vSetIdx(0)(0)
  val port_0_read_1 = io.read.valid  &&  io.read.bits.vSetIdx(0)(0)
  val port_1_read_1  = io.read.valid &&  io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine
  val port_1_read_0  = io.read.valid && !io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine

  val port_0_read_0_reg = RegEnable(next = port_0_read_0, enable = io.read.fire())
  val port_0_read_1_reg = RegEnable(next = port_0_read_1, enable = io.read.fire())
  val port_1_read_1_reg = RegEnable(next = port_1_read_1, enable = io.read.fire())
  val port_1_read_0_reg = RegEnable(next = port_1_read_0, enable = io.read.fire())

  val bank_0_idx = Mux(port_0_read_0, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  val bank_1_idx = Mux(port_0_read_1, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))

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
  //Parity Decode
  val read_metas = Wire(Vec(2,Vec(nWays,new ICacheMetadata())))
  for((tagArray,i) <- tagArrays.zipWithIndex){
    val read_meta_bits = tagArray.io.r.resp.asTypeOf(Vec(nWays,UInt(metaEntryBits.W)))
    val read_meta_decoded = read_meta_bits.map{ way_bits => cacheParams.tagCode.decode(way_bits)}
    val read_meta_wrong = read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.error}
    val read_meta_corrected = VecInit(read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.corrected})
    read_metas(i) := read_meta_corrected.asTypeOf(Vec(nWays,new ICacheMetadata()))
    (0 until nWays).map{ w => io.readResp.errors(i)(w) := RegNext(io.read.fire()) && read_meta_wrong(w)}
  }

  //Parity Encode
  val write = io.write.bits
  write_meta_bits := cacheParams.tagCode.encode(ICacheMetadata(tag = write.phyTag, coh = write.coh).asUInt)

  //  when(io.write.valid){
  //      printf("[time:%d ] idx:%x  ptag:%x  waymask:%x coh:%x\n", GTimer().asUInt, write.virIdx, write.phyTag, write.waymask, write.coh.asUInt)
  //  }

  val readIdxNext = RegEnable(next = io.read.bits.vSetIdx, enable = io.read.fire())
  val validArray = RegInit(0.U((nSets * nWays).W))
  val validMetas = VecInit((0 until 2).map{ bank =>
    val validMeta =  Cat((0 until nWays).map{w => validArray( Cat(readIdxNext(bank), w.U(log2Ceil(nWays).W)) )}.reverse).asUInt
    validMeta
  })

  val wayNum   = OHToUInt(io.write.bits.waymask)
  val validPtr = Cat(io.write.bits.virIdx, wayNum)
  when(io.write.valid){
    validArray := validArray.bitSet(validPtr, true.B)
  }

  when(io.fencei){ validArray := 0.U }

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

  (io.readResp.valid zip validMetas).map  {case (io, reg)   => io := reg.asTypeOf(Vec(nWays,Bool()))}

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
    // TODO
    // when(CacheInstrucion.isWriteTagECC(io.cacheOp.req.bits.opCode)){
    //   for (i <- 0 until readPorts) {
    //     array(i).io.ecc_write.valid := true.B
    //     array(i).io.ecc_write.bits.idx := io.cacheOp.req.bits.index
    //     array(i).io.ecc_write.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
    //     array(i).io.ecc_write.bits.ecc := io.cacheOp.req.bits.write_tag_ecc
    //   }
    //   cacheOpShouldResp := true.B
    // }
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
  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheDataWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle))
    val readResp = Output(new ICacheDataRespBundle)
    val cacheOp  = Flipped(new DCacheInnerOpIO) // customized cache op port 
  }}

  io.read.ready := !io.write.valid

  val port_0_read_0 = io.read.valid  && !io.read.bits.vSetIdx(0)(0)
  val port_0_read_1 = io.read.valid  &&  io.read.bits.vSetIdx(0)(0)
  val port_1_read_1  = io.read.valid &&  io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine
  val port_1_read_0  = io.read.valid && !io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine

  val port_0_read_1_reg = RegEnable(next = port_0_read_1, enable = io.read.fire())
  val port_1_read_0_reg = RegEnable(next = port_1_read_0, enable = io.read.fire())

  val bank_0_idx = Mux(port_0_read_0, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  val bank_1_idx = Mux(port_0_read_1, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))

  val write_bank_0 = io.write.valid && !io.write.bits.bankIdx
  val write_bank_1 = io.write.valid &&  io.write.bits.bankIdx

  val write_data_bits = Wire(UInt(dataEntryBits.W))

  val dataArrays = (0 until 2) map { i =>
    val dataArray = Module(new SRAMTemplate(
      UInt(dataEntryBits.W),
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

  //Parity Decode
  val read_datas = Wire(Vec(2,Vec(nWays,UInt(blockBits.W) )))
  for((dataArray,i) <- dataArrays.zipWithIndex){
    val read_data_bits = dataArray.io.r.resp.asTypeOf(Vec(nWays,Vec(dataUnitNum, UInt(dataCodeBits.W))))
    val read_data_decoded = read_data_bits.map{way_bits => way_bits.map(unit =>  cacheParams.dataCode.decode(unit))}
    val read_data_wrong    = VecInit(read_data_decoded.map{way_bits_decoded => VecInit(way_bits_decoded.map(unit_decoded =>  unit_decoded.error ))})
    val read_data_corrected = VecInit(read_data_decoded.map{way_bits_decoded => VecInit(way_bits_decoded.map(unit_decoded =>  unit_decoded.corrected )).asUInt})
    read_datas(i) := read_data_corrected.asTypeOf(Vec(nWays,UInt(blockBits.W)))
    (0 until nWays).map{ w => io.readResp.errors(i)(w) := RegNext(io.read.fire()) && read_data_wrong(w).asUInt.orR } 
  } 

  //Parity Encode
  val write = io.write.bits
  val write_data = write.data.asTypeOf(Vec(dataUnitNum, UInt(dataCodeUnit.W)))
  val write_data_encoded = VecInit(write_data.map( unit_bits => cacheParams.dataCode.encode(unit_bits) ))
  write_data_bits := write_data_encoded.asUInt

  io.readResp.datas(0) := Mux( port_0_read_1_reg, read_datas(1) , read_datas(0))
  io.readResp.datas(1) := Mux( port_1_read_0_reg, read_datas(0) , read_datas(1))

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
        dataArrays(i).io.w.req.bits.apply(
          data = io.cacheOp.req.bits.write_data_vec.asUInt,
          setIdx = io.cacheOp.req.bits.index,
          waymask = UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
        )
      })
      cacheOpShouldResp := true.B
    }
    // when(CacheInstrucion.isWriteDataECC(io.cacheOp.req.bits.opCode)){
    //   for (bank_index <- 0 until DCacheBanks) {
    //     val ecc_bank = ecc_banks(bank_index)
    //     ecc_bank.io.w.req.valid := true.B
    //     ecc_bank.io.w.req.bits.apply(
    //       setIdx = io.cacheOp.req.bits.index,
    //       data = io.cacheOp.req.bits.write_data_ecc,
    //       waymask = UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
    //     )
    //   }
    //   cacheOpShouldResp := true.B
    // }
  }
  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  val dataresp = Mux(io.cacheOp.req.bits.bank_num(0).asBool,
    dataArrays(0).io.r.resp.data.asTypeOf(Vec(nWays, UInt(blockBits.W))),
    dataArrays(1).io.r.resp.data.asTypeOf(Vec(nWays, UInt(blockBits.W)))
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
  val fencei      = Input(Bool())
  val stop        = Input(Bool())
  val csr         = new L1CacheToCsrIO
  val fetch       = Vec(PortNumber, new ICacheMainPipeBundle)
  val pmp         = Vec(PortNumber, new ICachePMPBundle)
  val itlb        = Vec(PortNumber, new BlockTlbRequestIO)
  val perfInfo = Output(new ICachePerfInfo)
}

class ICache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, cacheParams.nMissEntries + cacheParams.nReleaseEntries),
      supportsProbe = TransferSizes(blockBytes)
    )),
    requestFields = cacheParams.reqFields,
    echoFields = cacheParams.echoFields
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new ICacheImp(this)
}

class ICacheImp(outer: ICache) extends LazyModuleImp(outer) with HasICacheParameters {
  val io = IO(new ICacheIO)

  val (bus, edge) = outer.clientNode.out.head

  val metaArray      = Module(new ICacheMetaArray)
  val dataArray      = Module(new ICacheDataArray)
  val mainPipe       = Module(new ICacheMainPipe)
  val missUnit      = Module(new ICacheMissUnit(edge))
  val releaseUnit    = Module(new ReleaseUnit(edge))
  val replacePipe     = Module(new ReplacePipe)
  val probeQueue     = Module(new ICacheProbeQueue(edge))

  val meta_read_arb   = Module(new Arbiter(new ICacheReadBundle,  2))
  val data_read_arb   = Module(new Arbiter(new ICacheReadBundle,  2))
  val meta_write_arb  = Module(new Arbiter(new ICacheMetaWriteBundle(),  2 ))
  val replace_req_arb     = Module(new Arbiter(new ReplacePipeReq, 2))

  meta_read_arb.io.in(ReplacePipeKey)   <> replacePipe.io.meta_read
  meta_read_arb.io.in(mainPipeKey)      <> mainPipe.io.metaArray.toIMeta
  metaArray.io.read                     <> meta_read_arb.io.out
  replacePipe.io.meta_response          <> metaArray.io.readResp
  mainPipe.io.metaArray.fromIMeta       <> metaArray.io.readResp

  data_read_arb.io.in(ReplacePipeKey) <> replacePipe.io.data_read
  data_read_arb.io.in(mainPipeKey)    <> mainPipe.io.dataArray.toIData
  dataArray.io.read                   <> data_read_arb.io.out
  replacePipe.io.data_response        <> dataArray.io.readResp
  mainPipe.io.dataArray.fromIData     <> dataArray.io.readResp

  mainPipe.io.respStall := io.stop
  io.perfInfo := mainPipe.io.perfInfo

  meta_write_arb.io.in(ReplacePipeKey)  <> replacePipe.io.meta_write
  meta_write_arb.io.in(mainPipeKey)     <> missUnit.io.meta_write

  metaArray.io.write <> meta_write_arb.io.out
  dataArray.io.write <> missUnit.io.data_write

  io.itlb           <>    mainPipe.io.itlb
  io.pmp            <>    mainPipe.io.pmp
  for(i <- 0 until PortNumber){
    io.fetch(i).resp     <>    mainPipe.io.fetch(i).resp

    missUnit.io.req(i)           <>   mainPipe.io.mshr(i).toMSHR
    mainPipe.io.mshr(i).fromMSHR <>   missUnit.io.resp(i)

  }

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  metaArray.io.fencei := io.fencei
  bus.a <> missUnit.io.mem_acquire
  bus.e <> missUnit.io.mem_finish

  releaseUnit.io.req(0)  <>  replacePipe.io.release_req
  releaseUnit.io.req(1)  <>  DontCare//mainPipe.io.toReleaseUnit(1)
  bus.c <> releaseUnit.io.mem_release

  // connect bus d
  missUnit.io.mem_grant.valid := false.B
  missUnit.io.mem_grant.bits  := DontCare

  releaseUnit.io.mem_grant.valid := false.B
  releaseUnit.io.mem_grant.bits  := DontCare

  //Probe through bus b
  probeQueue.io.mem_probe    <> bus.b


  /** Block set-conflict request */
 val probeReqValid = probeQueue.io.pipe_req.valid
 val probeReqVidx  = probeQueue.io.pipe_req.bits.vidx

  val hasVictim = VecInit(missUnit.io.victimInfor.map(_.valid))
  val victimSetSeq = VecInit(missUnit.io.victimInfor.map(_.vidx))

  val probeShouldBlock = VecInit(hasVictim.zip(victimSetSeq).map{case(valid, idx) =>  valid && probeReqValid && idx === probeReqVidx }).reduce(_||_)

 val releaseReqValid = missUnit.io.release_req.valid
 val releaseReqVidx  = missUnit.io.release_req.bits.vidx

  val hasConflict = VecInit(Seq(
        replacePipe.io.status.r1_set.valid,
        replacePipe.io.status.r2_set.valid
  ))

  val conflictIdx = VecInit(Seq(
        replacePipe.io.status.r1_set.bits,
        replacePipe.io.status.r2_set.bits
  ))

  val releaseShouldBlock = VecInit(hasConflict.zip(conflictIdx).map{case(valid, idx) =>  valid && releaseReqValid && idx === releaseReqVidx }).reduce(_||_)

  replace_req_arb.io.in(ReplacePipeKey) <> probeQueue.io.pipe_req
  replace_req_arb.io.in(ReplacePipeKey).valid := probeQueue.io.pipe_req.valid && !probeShouldBlock
  replace_req_arb.io.in(mainPipeKey)   <> missUnit.io.release_req
  replace_req_arb.io.in(mainPipeKey).valid := missUnit.io.release_req.valid && !releaseShouldBlock
  replacePipe.io.pipe_req               <> replace_req_arb.io.out

  when(releaseShouldBlock){
    missUnit.io.release_req.ready := false.B
  }

  when(probeShouldBlock){
    probeQueue.io.pipe_req.ready := false.B
  }


  missUnit.io.release_resp <> replacePipe.io.pipe_resp

  
  (0 until PortNumber).map{i => 
      mainPipe.io.fetch(i).req.valid := io.fetch(i).req.valid //&& !fetchShouldBlock(i)
      io.fetch(i).req.ready          :=  mainPipe.io.fetch(i).req.ready //&& !fetchShouldBlock(i)
      mainPipe.io.fetch(i).req.bits  := io.fetch(i).req.bits
  }

  // in L1ICache, we only expect GrantData and ReleaseAck
  bus.d.ready := false.B
  when ( bus.d.bits.opcode === TLMessages.GrantData) {
    missUnit.io.mem_grant <> bus.d
  } .elsewhen (bus.d.bits.opcode === TLMessages.ReleaseAck) {
    releaseUnit.io.mem_grant <> bus.d
  } .otherwise {
    assert (!bus.d.fire())
  }

  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(2))
  })
  val perfEvents = Seq(
    ("icache_miss_cnt         ", false.B                               ),
    ("icache_miss_penty       ", BoolStopWatch(start = false.B, stop = false.B || false.B, startHighPriority = true)                               ),
  )

    // Customized csr cache op support
  val cacheOpDecoder = Module(new CSRCacheOpDecoder("icache", CacheInstrucion.COP_ID_ICACHE))
  cacheOpDecoder.io.csr <> io.csr
  dataArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
  metaArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
  cacheOpDecoder.io.cache.resp.valid := 
    dataArray.io.cacheOp.resp.valid ||
    metaArray.io.cacheOp.resp.valid
  cacheOpDecoder.io.cache.resp.bits := Mux1H(List(
    dataArray.io.cacheOp.resp.valid -> dataArray.io.cacheOp.resp.bits,
    metaArray.io.cacheOp.resp.valid -> metaArray.io.cacheOp.resp.bits,
  ))
  assert(!((dataArray.io.cacheOp.resp.valid +& metaArray.io.cacheOp.resp.valid) > 1.U))

} 