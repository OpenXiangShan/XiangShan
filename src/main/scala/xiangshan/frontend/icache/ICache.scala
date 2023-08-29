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
import chisel3.util.{DecoupledIO, _}
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import coupledL2.{AliasField, DirtyField, PrefetchField}
import xiangshan._
import xiangshan.frontend._
import xiangshan.cache._
import utils._
import utility._
import xiangshan.backend.fu.PMPReqBundle
import xiangshan.cache.mmu.{TlbRequestIO, TlbReq}
import difftest._

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
    nPrefetchEntries: Int = 12,
    nPrefBufferEntries: Int = 32,
    prefetchPipeNum: Int = 2,
    hasPrefetch: Boolean = true,
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

  def PortNumber = 2

  def partWayNum = 4
  def pWay = nWays/partWayNum

  def nPrefetchEntries = cacheParams.nPrefetchEntries
  def totalMSHRNum = PortNumber + nPrefetchEntries
  def nIPFBufferSize   = cacheParams.nPrefBufferEntries
  def maxIPFMoveConf   = 1 // temporary use small value to cause more "move" operation
  def prefetchPipeNum = ICacheParameters().prefetchPipeNum

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
    val cacheOp  = Flipped(new L1CacheInnerOpIO) // customized cache op port
    val fencei   = Input(Bool())
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

  val read_set_idx_next = RegEnable(next = io.read.bits.vSetIdx, enable = io.read.fire)
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

  // fencei logic : reset valid_array
  when (io.fencei) {
    (0 until nWays).foreach( way =>
      valid_array(way) := 0.U
    )
  }
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
    val read     = Flipped(DecoupledIO(Vec(partWayNum, new ICacheReadBundle)))
    val readResp = Output(new ICacheDataRespBundle)
    val cacheOp  = Flipped(new L1CacheInnerOpIO) // customized cache op port
  }}

  val write_data_bits = Wire(UInt(blockBits.W))

  val port_0_read_0_reg = RegEnable(next = io.read.valid && io.read.bits.head.port_0_read_0, enable = io.read.fire())
  val port_0_read_1_reg = RegEnable(next = io.read.valid && io.read.bits.head.port_0_read_1, enable = io.read.fire())
  val port_1_read_1_reg = RegEnable(next = io.read.valid && io.read.bits.head.port_1_read_1, enable = io.read.fire())
  val port_1_read_0_reg = RegEnable(next = io.read.valid && io.read.bits.head.port_1_read_0, enable = io.read.fire())

  val bank_0_idx_vec = io.read.bits.map(copy =>  Mux(io.read.valid && copy.port_0_read_0, copy.vSetIdx(0), copy.vSetIdx(1)))
  val bank_1_idx_vec = io.read.bits.map(copy =>  Mux(io.read.valid && copy.port_0_read_1, copy.vSetIdx(0), copy.vSetIdx(1)))

  val dataArrays = (0 until partWayNum).map{ i =>
    val dataArray = Module(new ICachePartWayArray(
      UInt(blockBits.W),
      pWay,
    ))

    dataArray.io.read.req(0).valid :=  io.read.bits(i).read_bank_0 && io.read.valid
    dataArray.io.read.req(0).bits.ridx := bank_0_idx_vec(i)(highestIdxBit,1)
    dataArray.io.read.req(1).valid := io.read.bits(i).read_bank_1 && io.read.valid
    dataArray.io.read.req(1).bits.ridx := bank_1_idx_vec(i)(highestIdxBit,1)


    dataArray.io.write.valid         := io.write.valid
    dataArray.io.write.bits.wdata    := write_data_bits
    dataArray.io.write.bits.widx     := io.write.bits.virIdx(highestIdxBit,1)
    dataArray.io.write.bits.wbankidx := io.write.bits.bankIdx
    dataArray.io.write.bits.wmask    := io.write.bits.waymask.asTypeOf(Vec(partWayNum, Vec(pWay, Bool())))(i)

    dataArray
  }

  val read_datas = Wire(Vec(2,Vec(nWays,UInt(blockBits.W) )))

  (0 until PortNumber).map { port =>
    (0 until nWays).map { w =>
      read_datas(port)(w) := dataArrays(w / pWay).io.read.resp.rdata(port).asTypeOf(Vec(pWay, UInt(blockBits.W)))(w % pWay)
    }
  }

  io.readResp.datas(0) := Mux( port_0_read_1_reg, read_datas(1) , read_datas(0))
  io.readResp.datas(1) := Mux( port_1_read_0_reg, read_datas(0) , read_datas(1))


  val write_data_code = Wire(UInt(dataCodeEntryBits.W))
  val write_bank_0 = WireInit(io.write.valid && !io.write.bits.bankIdx)
  val write_bank_1 = WireInit(io.write.valid &&  io.write.bits.bankIdx)

  val bank_0_idx = bank_0_idx_vec.last
  val bank_1_idx = bank_1_idx_vec.last

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
      codeArray.io.r.req.valid := io.read.valid && io.read.bits.last.read_bank_0
      codeArray.io.r.req.bits.apply(setIdx=bank_0_idx(highestIdxBit,1))
      codeArray.io.w.req.valid := write_bank_0
      codeArray.io.w.req.bits.apply(data=write_data_code, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }
    else {
      codeArray.io.r.req.valid := io.read.valid && io.read.bits.last.read_bank_1
      codeArray.io.r.req.bits.apply(setIdx=bank_1_idx(highestIdxBit,1))
      codeArray.io.w.req.valid := write_bank_1
      codeArray.io.w.req.bits.apply(data=write_data_code, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }
    
    codeArray
  }
  
  io.read.ready := !io.write.valid &&
                    dataArrays.map(_.io.read.req.map(_.ready).reduce(_&&_)).reduce(_&&_) &&
                    codeArrays.map(_.io.r.req.ready).reduce(_ && _)

  //Parity Decode
  val read_codes = Wire(Vec(2,Vec(nWays,UInt(dataCodeEntryBits.W) )))
  for(((dataArray,codeArray),i) <- dataArrays.zip(codeArrays).zipWithIndex){
    read_codes(i) := codeArray.io.r.resp.asTypeOf(Vec(nWays,UInt(dataCodeEntryBits.W)))
  } 

  //Parity Encode
  val write = io.write.bits
  val write_data = WireInit(write.data)
  write_data_code := getECCFromBlock(write_data).asUInt
  write_data_bits := write_data

  io.readResp.codes(0) := Mux( port_0_read_1_reg, read_codes(1) , read_codes(0))
  io.readResp.codes(1) := Mux( port_1_read_0_reg, read_codes(0) , read_codes(1))

  io.write.ready := true.B

  // deal with customized cache op
  require(nWays <= 32)
  io.cacheOp.resp.bits := DontCare
  io.cacheOp.resp.valid := false.B
  val cacheOpShouldResp = WireInit(false.B) 
  val dataresp = Wire(Vec(nWays,UInt(blockBits.W) ))
  dataresp := DontCare
  when(io.cacheOp.req.valid){
    when(
      CacheInstrucion.isReadData(io.cacheOp.req.bits.opCode)
    ){
      for (i <- 0 until partWayNum) {
        dataArrays(i).io.read.req.zipWithIndex.map{ case(port,i) =>
          if(i ==0) port.valid     := !io.cacheOp.req.bits.bank_num(0)
          else      port.valid     :=  io.cacheOp.req.bits.bank_num(0)
          port.bits.ridx := io.cacheOp.req.bits.index(highestIdxBit,1)
        }
      }
      cacheOpShouldResp := dataArrays.head.io.read.req.map(_.fire()).reduce(_||_)
      dataresp :=Mux(io.cacheOp.req.bits.bank_num(0).asBool,  read_datas(1),  read_datas(0))
    }
    when(CacheInstrucion.isWriteData(io.cacheOp.req.bits.opCode)){
      for (i <- 0 until partWayNum) {
        dataArrays(i).io.write.valid := true.B
        dataArrays(i).io.write.bits.wdata := io.cacheOp.req.bits.write_data_vec.asTypeOf(write_data.cloneType)
        dataArrays(i).io.write.bits.wbankidx := io.cacheOp.req.bits.bank_num(0)
        dataArrays(i).io.write.bits.widx := io.cacheOp.req.bits.index(highestIdxBit,1)
        dataArrays(i).io.write.bits.wmask  := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0)).asTypeOf(Vec(partWayNum, Vec(pWay, Bool())))(i)
      }
      cacheOpShouldResp := true.B
    }
  }
  
  io.cacheOp.resp.valid := RegNext(cacheOpShouldResp)
  val numICacheLineWords = blockBits / 64
  require(blockBits >= 64 && isPow2(blockBits))
  for (wordIndex <- 0 until numICacheLineWords) {
    io.cacheOp.resp.bits.read_data_vec(wordIndex) := dataresp(io.cacheOp.req.bits.wayNum(4, 0))(64*(wordIndex+1)-1, 64*wordIndex)
  }

}


class ICacheIO(implicit p: Parameters) extends ICacheBundle
{
  val hartId = Input(UInt(8.W))
  val prefetch    = Flipped(new FtqPrefechBundle)
  val stop        = Input(Bool())
  val fetch       = new ICacheMainPipeBundle
  val toIFU       = Output(Bool())
  val pmp         = Vec(PortNumber + prefetchPipeNum, new ICachePMPBundle)
  val itlb        = Vec(PortNumber + prefetchPipeNum, new TlbRequestIO)
  val perfInfo    = Output(new ICachePerfInfo)
  val error       = new L1CacheErrorInfo
  /* Cache Instruction */
  val csr         = new L1CacheToCsrIO
  /* CSR control signal */
  val csr_pf_enable = Input(Bool())
  val csr_parity_enable = Input(Bool())
  val fencei      = Input(Bool())
}

class ICache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, cacheParams.nMissEntries + cacheParams.nPrefetchEntries),
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
  println("  hasPrefetch: "         + cacheParams.hasPrefetch)
  if(cacheParams.hasPrefetch){
    println("  nPrefetchEntries: "         + cacheParams.nPrefetchEntries)
    println("  nPrefetchBufferEntries: " + cacheParams.nPrefBufferEntries)
    println("  prefetchPipeNum: " + cacheParams.prefetchPipeNum)
  }

  val (bus, edge) = outer.clientNode.out.head

  val metaArray      = Module(new ICacheMetaArray)
  val bankedMetaArray = Module(new ICacheBankedMetaArray(prefetchPipeNum + 1)) // need add 1 port for IPF filter
  val dataArray      = Module(new ICacheDataArray)
  val mainPipe       = Module(new ICacheMainPipe)
  val missUnit      = Module(new ICacheMissUnit(edge))
  val prefetchPipes = (0 until prefetchPipeNum).map( i => Module(new IPrefetchPipe))
  val ipfBuffer  = Module(new PrefetchBuffer)

  val meta_read_arb   = Module(new Arbiter(new ICacheReadBundle,  1))
  val data_read_arb   = Module(new Arbiter(Vec(partWayNum, new ICacheReadBundle),  1))
  val meta_write_arb  = Module(new Arbiter(new ICacheMetaWriteBundle(),  2))
  val data_write_arb = Module(new Arbiter(new ICacheDataWriteBundle(), 2))
  val prefetch_req_arb = Module(new Arbiter(new PIQReq, prefetchPipeNum))

  mainPipe.io.hartId := io.hartId
  ipfBuffer.io.hartId := io.hartId
  mainPipe.io.PIQ <> missUnit.io.to_main_pipe
  ipfBuffer.io.read <> mainPipe.io.iprefetchBuf
  meta_write_arb.io.in(1) <> ipfBuffer.io.move.meta_write
  data_write_arb.io.in(1) <> ipfBuffer.io.move.data_write
  mainPipe.io.IPFBufMove <> ipfBuffer.io.replace
  (0 until prefetchPipeNum).foreach(i => ipfBuffer.io.filter_read(i) <> prefetchPipes(i).io.IPFBufferRead)
  (0 until prefetchPipeNum).foreach(i => mainPipe.io.missSlotInfo <> prefetchPipes(i).io.mainPipeMissSlotInfo)
  mainPipe.io.mainPipeMissInfo <> ipfBuffer.io.mainpipe_missinfo

  ipfBuffer.io.write <> missUnit.io.piq_write_ipbuffer

  meta_read_arb.io.in(0)      <> mainPipe.io.metaArray.toIMeta
  metaArray.io.read                     <> meta_read_arb.io.out
  bankedMetaArray.io.read(0) <> ipfBuffer.io.meta_filter_read_req
  (0 until prefetchPipeNum).foreach(i => bankedMetaArray.io.read(i + 1) <> prefetchPipes(i).io.toIMeta)

  mainPipe.io.metaArray.fromIMeta       <> metaArray.io.readResp
  ipfBuffer.io.meta_filter_read_resp <> bankedMetaArray.io.readResp(0)
  (0 until prefetchPipeNum).foreach(i => bankedMetaArray.io.readResp(i + 1) <> prefetchPipes(i).io.fromIMeta)

  data_read_arb.io.in(0)    <> mainPipe.io.dataArray.toIData
  dataArray.io.read                   <> data_read_arb.io.out
  mainPipe.io.dataArray.fromIData     <> dataArray.io.readResp

  mainPipe.io.respStall := io.stop
  io.perfInfo := mainPipe.io.perfInfo

  meta_write_arb.io.in(0)     <> missUnit.io.meta_write
  data_write_arb.io.in(0)     <> missUnit.io.data_write

  metaArray.io.write <> meta_write_arb.io.out
  bankedMetaArray.io.write <> meta_write_arb.io.out

  dataArray.io.write <> data_write_arb.io.out

  mainPipe.io.csr_parity_enable := io.csr_parity_enable

  if(cacheParams.hasPrefetch){
    // TODO : perf enhance
    val prefetchPipe_ready_vec = WireInit(VecInit(Seq.fill(prefetchPipeNum)(false.B)))
    val alloc = RegInit(0.U(log2Up(prefetchPipeNum).W))
    alloc := alloc + io.prefetch.req.fire
    (0 until prefetchPipeNum).foreach(i => {
      prefetchPipes(i).io.fromFtq.req.valid := io.prefetch.req.valid && i.U === alloc
      prefetchPipes(i).io.fromFtq.req.bits := io.prefetch.req.bits
      prefetchPipe_ready_vec(i) := prefetchPipes(i).io.fromFtq.req.ready && i.U === alloc
    })
    io.prefetch.req.ready := prefetchPipe_ready_vec.reduce(_||_)
    when(!io.csr_pf_enable){
      (0 until prefetchPipeNum).foreach(i => {
        prefetchPipes(i).io.fromFtq.req.valid := false.B
      })
      io.prefetch.req.ready := true.B
    }
  } else {
    (0 until prefetchPipeNum).foreach(i => prefetchPipes(i).io.fromFtq <> DontCare)
  }

  io.pmp(0) <> mainPipe.io.pmp(0)
  io.pmp(1) <> mainPipe.io.pmp(1)
  (0 until prefetchPipeNum).foreach(i => io.pmp(2 + i) <> prefetchPipes(i).io.pmp)
  (0 until prefetchPipeNum).foreach(i => {
    prefetchPipes(i).io.prefetchEnable := mainPipe.io.prefetchEnable
    prefetchPipes(i).io.prefetchDisable := mainPipe.io.prefetchDisable
  })


  //notify IFU that Icache pipeline is available
  io.toIFU := mainPipe.io.fetch.req.ready


  io.itlb(0)        <>    mainPipe.io.itlb(0)
  io.itlb(1)        <>    mainPipe.io.itlb(1)
  (0 until prefetchPipeNum).foreach(i => io.itlb(2 + i) <> prefetchPipes(i).io.iTLBInter)


  io.fetch.resp     <>    mainPipe.io.fetch.resp
  io.fetch.topdownIcacheMiss := mainPipe.io.fetch.topdownIcacheMiss
  io.fetch.topdownItlbMiss   := mainPipe.io.fetch.topdownItlbMiss

  for(i <- 0 until PortNumber){
    missUnit.io.req(i)           <>   mainPipe.io.mshr(i).toMSHR
    mainPipe.io.mshr(i).fromMSHR <>   missUnit.io.resp(i)
  }

  (0 until prefetchPipeNum).foreach(i => prefetch_req_arb.io.in(i) <> prefetchPipes(i).io.toMissUnit.enqReq)
  missUnit.io.prefetch_req <> prefetch_req_arb.io.out
  missUnit.io.hartId       := io.hartId
  (0 until prefetchPipeNum).foreach(i => {
    prefetchPipes(i).io.fromMSHR <> missUnit.io.mshr_info
    prefetchPipes(i).io.fencei := false.B
    prefetchPipes(i).io.freePIQEntry := missUnit.io.freePIQEntry
  })

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  bus.a <> missUnit.io.mem_acquire

  // connect bus d
  missUnit.io.mem_grant.valid := false.B
  missUnit.io.mem_grant.bits  := DontCare

  //Parity error port
  val errors = mainPipe.io.errors
  io.error <> RegNext(Mux1H(errors.map(e => e.valid -> e)))

  
  mainPipe.io.fetch.req <> io.fetch.req
  bus.d.ready := false.B
  missUnit.io.mem_grant <> bus.d

  // fencei connect
  metaArray.io.fencei := io.fencei
  bankedMetaArray.io.fencei := io.fencei
  ipfBuffer.io.fencei := io.fencei
  missUnit.io.fencei := io.fencei


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
  bankedMetaArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
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
    sramBank.io.w.req.bits.apply(data=io.write.bits.wdata, setIdx=io.write.bits.widx, waymask=io.write.bits.wmask.asUInt())

    sramBank
  }

  io.read.req.map(_.ready := !io.write.valid && srams.map(_.io.r.req.ready).reduce(_&&_))

  io.read.resp.rdata := VecInit(srams.map(bank => bank.io.r.resp.asTypeOf(Vec(pWay,gen))))

}
