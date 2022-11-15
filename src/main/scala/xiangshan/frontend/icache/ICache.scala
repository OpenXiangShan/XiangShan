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
import huancun.mbist.MBISTPipeline.placePipelines
import huancun.utils.SRAMTemplate
import huancun.{AliasField, DirtyField, PreferCacheField, PrefetchField}
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.mmu.BlockTlbRequestIO
import xiangshan.frontend._

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
    nPrefetchEntries: Int = 4,
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

  val missUnitIDStart      = 0
  val iprefetchUnitIDStart = missUnitIDStart + PortNumber
  val releaseUnitIDStart   = iprefetchUnitIDStart + cacheParams.nPrefetchEntries

  val ICacheWordOffset = 0
  val ICacheSetOffset = ICacheWordOffset + log2Up(blockBytes)
  val ICacheAboveIndexOffset = ICacheSetOffset + log2Up(ICacheSets)
  val ICacheTagOffset = ICacheAboveIndexOffset min ICacheSameVPAddrLength

  def ReplacePipeKey = 0
  def MainPipeKey = 1
  def PortNumber = 2
  def ProbeKey   = 3

  def partWayNum = 2
  def pWay = nWays/partWayNum

  def nPrefetchEntries = cacheParams.nPrefetchEntries

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


class ICacheMetaArray(parentName:String = "Unknown")(implicit p: Parameters) extends ICacheArray
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
      singlePort = true,
      parentName = parentName + s"bank${bank}_"
    ))


    tagArray
  }

  val tag_sram_write = Wire(Vec(2,tagArrays.head.io.w.req.cloneType))
  tag_sram_write.map(_.ready := true.B)

  (0 until 2).map{i =>
    //meta connection
    if(i == 0) {
      tagArrays(i).io.r.req.valid := port_0_read_0 || port_1_read_0
      tagArrays(i).io.r.req.bits.apply(setIdx=bank_0_idx(highestIdxBit,1))
      tag_sram_write(i).valid := write_bank_0
      tag_sram_write(i).bits.apply(data=write_meta_bits, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)

      tagArrays(i).io.w.req.valid := RegNext(tag_sram_write(i).valid,init = false.B)
      tagArrays(i).io.w.req.bits  := RegEnable(tag_sram_write(i).bits, enable =tag_sram_write(i).valid )
    }
    else {
      tagArrays(i).io.r.req.valid := port_0_read_1 || port_1_read_1
      tagArrays(i).io.r.req.bits.apply(setIdx=bank_1_idx(highestIdxBit,1))
      tag_sram_write(i).valid := write_bank_1
      tag_sram_write(i).bits.apply(data=write_meta_bits, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)

      tagArrays(i).io.w.req.valid :=  RegNext(tag_sram_write(i).valid, init = false.B)
      tagArrays(i).io.w.req.bits  := RegEnable(tag_sram_write(i).bits, enable =tag_sram_write(i).valid )

    }
  }

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
        tag_sram_write(i).valid := true.B
        tag_sram_write(i).bits.apply(
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



class ICacheDataArray(parentName:String = "Unknown")(implicit p: Parameters) extends ICacheArray
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
    val cache_req_dup = Vec(4, Flipped(Valid(new CacheCtrlReqInfo)))
  }}

  val write_data_bits = Wire(UInt(blockBits.W))

  val port_0_read_0_reg = RegEnable(next = io.read.valid && io.read.bits.head.port_0_read_0, enable = io.read.fire())
  val port_0_read_1_reg = RegEnable(next = io.read.valid && io.read.bits.head.port_0_read_1, enable = io.read.fire())
  val port_1_read_1_reg = RegEnable(next = io.read.valid && io.read.bits.head.port_1_read_1, enable = io.read.fire())
  val port_1_read_0_reg = RegEnable(next = io.read.valid && io.read.bits.head.port_1_read_0, enable = io.read.fire())

  //val bank_0_idx_vec = io.read.bits.map(copy =>  Mux(io.read.bits.dup_valids && copy.port_0_read_0, copy.vSetIdx(0), copy.vSetIdx(1)))
  //val bank_1_idx_vec = io.read.bits.map(copy =>  Mux(io.read.valid && copy.port_0_read_1, copy.vSetIdx(0), copy.vSetIdx(1)))

  val bank_0_idx_vec = (0 until partWayNum).map(i => Mux(io.read.bits(i).readValid && io.read.bits(i).port_0_read_0, io.read.bits(i).vSetIdx(0), io.read.bits(i).vSetIdx(1)))
  val bank_1_idx_vec = (0 until partWayNum).map(i => Mux(io.read.bits(i).readValid && io.read.bits(i).port_0_read_1, io.read.bits(i).vSetIdx(0), io.read.bits(i).vSetIdx(1)))


  val dataArrays = (0 until partWayNum).map{ i =>
    val dataArray = Module(new ICachePartWayArray(
      UInt(blockBits.W),
      pWay,
      parentName = parentName + s"dataArray${i}_"
    ))

    dataArray.io.read.req(0).valid :=  io.read.bits(i).read_bank_0 && io.read.bits(i).readValid
    dataArray.io.read.req(0).bits.ridx := bank_0_idx_vec(i)(highestIdxBit,1)
    dataArray.io.read.req(1).valid := io.read.bits(i).read_bank_1 && io.read.bits(i).readValid
    dataArray.io.read.req(1).bits.ridx := bank_1_idx_vec(i)(highestIdxBit,1)

    dataArray
  }

  val data_sram_write = Wire(Vec(partWayNum,dataArrays.head.io.write.cloneType))

  (0 until partWayNum).map{ i =>
    data_sram_write(i).valid         := io.write.bits.writeEn(i)
    data_sram_write(i).bits.wdata    := write_data_bits
    data_sram_write(i).bits.widx     := io.write.bits.virIdx(highestIdxBit,1)
    data_sram_write(i).bits.wbankidx := io.write.bits.bankIdx
    data_sram_write(i).bits.wmask    := io.write.bits.waymask.asTypeOf(Vec(partWayNum, Vec(pWay, Bool())))(i)

    dataArrays(i).io.write.valid := RegNext(data_sram_write(i).valid, init = false.B)
    dataArrays(i).io.write.bits  := RegEnable(data_sram_write(i).bits, enable = data_sram_write(i).valid)
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
      singlePort = true,
      parentName = parentName + s"codeArray${i}_"
    ))

    codeArray
  }

  val code_sram_write = Wire(Vec(2,codeArrays.head.io.w.req.cloneType))
  code_sram_write.map(_.ready := true.B)

  (0 until 2).map{i =>
    if(i == 0) {
      codeArrays(i).io.r.req.valid := io.read.valid && io.read.bits.last.read_bank_0
      codeArrays(i).io.r.req.bits.apply(setIdx=bank_0_idx(highestIdxBit,1))
      // codeArray.io.w.req.valid := write_bank_0
      // codeArray.io.w.req.bits.apply(data=write_data_code, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
      code_sram_write(i).valid := write_bank_0
      code_sram_write(i).bits.apply(data=write_data_code, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
      codeArrays(i).io.w.req.valid := RegNext(code_sram_write(i).valid, init = false.B)
      codeArrays(i).io.w.req.bits := RegEnable(code_sram_write(i).bits, enable = code_sram_write(i).valid)

    }
    else {
      codeArrays(i).io.r.req.valid := io.read.valid && io.read.bits.last.read_bank_1
      codeArrays(i).io.r.req.bits.apply(setIdx=bank_1_idx(highestIdxBit,1))
      // codeArray.io.w.req.valid := write_bank_1
      // codeArray.io.w.req.bits.apply(data=write_data_code, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
      code_sram_write(i).valid := write_bank_1
      code_sram_write(i).bits.apply(data=write_data_code, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
      codeArrays(i).io.w.req.valid := RegNext(code_sram_write(i).valid, init = false.B)
      codeArrays(i).io.w.req.bits := RegEnable(code_sram_write(i).bits, enable = code_sram_write(i).valid)
    }
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

  val data_resp_way = RegEnable(dataresp(RegNext(io.cacheOp.req.bits.wayNum(4, 0))), enable = RegNext(cacheOpShouldResp, init = false.B))

  for (w <- 0 until partWayNum) {
    when(io.cache_req_dup(w).valid){
      when(
        CacheInstrucion.isReadData(io.cache_req_dup(w).bits.opCode)
      ){
        dataArrays(w).io.read.req.zipWithIndex.map{ case(port,i) =>
          if(i ==0) port.valid     := !io.cache_req_dup(w).bits.bank_num(0)
          else      port.valid     :=  io.cache_req_dup(w).bits.bank_num(0)
          port.bits.ridx := io.cache_req_dup(w).bits.index(highestIdxBit,1)
        }
        cacheOpShouldResp := dataArrays.head.io.read.req.map(_.fire()).reduce(_||_)
        dataresp :=Mux(io.cache_req_dup(w).bits.bank_num(0).asBool,  read_datas(1),  read_datas(0))
      }
      when(CacheInstrucion.isWriteData(io.cache_req_dup(w).bits.opCode)){
        data_sram_write(w).valid         := true.B
        data_sram_write(w).bits.wdata    := io.cache_req_dup(w).bits.write_data_vec.asTypeOf(write_data.cloneType)
        data_sram_write(w).bits.wbankidx := io.cache_req_dup(w).bits.bank_num(0)
        data_sram_write(w).bits.widx     := io.cache_req_dup(w).bits.index(highestIdxBit,1)
        data_sram_write(w).bits.wmask    := UIntToOH(io.cache_req_dup(w).bits.wayNum(4, 0)).asTypeOf(Vec(partWayNum, Vec(pWay, Bool())))(w)
        cacheOpShouldResp := true.B
      }
    }
  }

  io.cacheOp.resp.valid := RegNext(RegNext(cacheOpShouldResp, init = false.B), init = false.B)
  val numICacheLineWords = blockBits / 64
  require(blockBits >= 64 && isPow2(blockBits))
  for (wordIndex <- 0 until numICacheLineWords) {
    io.cacheOp.resp.bits.read_data_vec(wordIndex) := data_resp_way(64*(wordIndex+1)-1, 64*wordIndex)
  }

}


class ICacheIO(implicit p: Parameters) extends ICacheBundle
{
  val prefetch    = Flipped(new FtqPrefechBundle)
  val stop        = Input(Bool())
  val fetch       = new ICacheMainPipeBundle
  val toIFU       = Output(Bool())
  val pmp         = Vec(PortNumber + 1, new ICachePMPBundle)
  val itlb        = Vec(PortNumber * 2 + 1, new BlockTlbRequestIO)
  val perfInfo    = Output(new ICachePerfInfo)
  val error       = new L1CacheErrorInfo
  /* Cache Instruction */
  val csr         = new L1CacheToCsrIO
  /* CSR control signal */
  val csr_pf_enable = Input(Bool())
  val csr_parity_enable = Input(Bool())
}

class ICache(parentName:String = "Unknown")(implicit p: Parameters) extends LazyModule with HasICacheParameters {

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

  lazy val module = new ICacheImp(parentName)(this)
}

class ICacheImp(parentName:String = "Unknown")(outer: ICache) extends LazyModuleImp(outer) with HasICacheParameters with HasPerfEvents {
  val io = IO(new ICacheIO)

  println("ICache:")
  println("  ICacheSets: "          + cacheParams.nSets)
  println("  ICacheWays: "          + cacheParams.nWays)
  println("  ICacheBanks: "         + PortNumber)
  println("  hasPrefetch: "         + cacheParams.hasPrefetch)
  if(cacheParams.hasPrefetch){
    println("  nPrefetchEntries: "         + cacheParams.nPrefetchEntries)
  }

  val (bus, edge) = outer.clientNode.out.head

  val metaArray      = Module(new ICacheMetaArray(parentName = parentName + "metaArray_")(p))
  val dataArray      = Module(new ICacheDataArray(parentName = parentName + "dataArray_")(p))
  val mainPipe       = Module(new ICacheMainPipe)
  val missUnit      = Module(new ICacheMissUnit(edge))
  val releaseUnit    = Module(new ReleaseUnit(edge))
  val replacePipe     = Module(new ICacheReplacePipe)
  val probeQueue     = Module(new ICacheProbeQueue(edge))
  val prefetchPipe    = Module(new IPrefetchPipe)

  val meta_read_arb   = Module(new Arbiter(new ICacheReadBundle,  3))
  val meta_write_arb  = Module(new Arbiter(new ICacheMetaWriteBundle(),  2 ))
  val replace_req_arb = Module(new Arbiter(new ReplacePipeReq, 2))
  // val tlb_req_arb     = Module(new Arbiter(new TlbReq, 2))

  meta_read_arb.io.in(ReplacePipeKey)   <> replacePipe.io.meta_read
  meta_read_arb.io.in(MainPipeKey)      <> mainPipe.io.metaArray.toIMeta
  meta_read_arb.io.in(2)                <> prefetchPipe.io.toIMeta
  metaArray.io.read                     <> meta_read_arb.io.out

  replacePipe.io.meta_response          <> metaArray.io.readResp
  mainPipe.io.metaArray.fromIMeta       <> metaArray.io.readResp
  prefetchPipe.io.fromIMeta             <> metaArray.io.readResp

  val data_read_arb_vec = Seq.fill(partWayNum)(Module(new Arbiter(new ICacheReadBundle,  2)))
  for(i <- 0 until partWayNum) {
    data_read_arb_vec(i).io.in(ReplacePipeKey).valid := replacePipe.io.data_read.bits(i).readValid
    data_read_arb_vec(i).io.in(ReplacePipeKey).bits  := replacePipe.io.data_read.bits(i)
    replacePipe.io.data_read.ready := data_read_arb_vec(i).io.in(ReplacePipeKey).ready

    data_read_arb_vec(i).io.in(MainPipeKey).valid    := mainPipe.io.dataArray.toIData.bits(i).readValid
    data_read_arb_vec(i).io.in(MainPipeKey).bits     := mainPipe.io.dataArray.toIData.bits(i)
    mainPipe.io.dataArray.toIData.ready   := data_read_arb_vec(i).io.in(MainPipeKey).ready
    dataArray.io.read.bits(i) <> data_read_arb_vec(i).io.out.bits
  }

  dataArray.io.read.valid := data_read_arb_vec.head.io.out.valid
  data_read_arb_vec.map(arb => arb.io.out.ready := dataArray.io.read.ready)

  replacePipe.io.data_response        <> dataArray.io.readResp
  mainPipe.io.dataArray.fromIData     <> dataArray.io.readResp

  mainPipe.io.respStall := io.stop
  io.perfInfo := mainPipe.io.perfInfo

  meta_write_arb.io.in(ReplacePipeKey)  <> replacePipe.io.meta_write
  meta_write_arb.io.in(MainPipeKey)     <> missUnit.io.meta_write

  metaArray.io.write <> meta_write_arb.io.out
  dataArray.io.write <> missUnit.io.data_write


  mainPipe.io.csr_parity_enable := io.csr_parity_enable
  replacePipe.io.csr_parity_enable := io.csr_parity_enable

  if(cacheParams.hasPrefetch){
    prefetchPipe.io.fromFtq <> io.prefetch
    when(!io.csr_pf_enable){
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

  //notify IFU that Icache pipeline is available
  io.toIFU := mainPipe.io.fetch.req.ready

  // tlb_req_arb.io.in(0) <> mainPipe.io.itlb(0).req
  // tlb_req_arb.io.in(1) <> prefetchPipe.io.iTLBInter.req
  // io.itlb(0).req       <>    tlb_req_arb.io.out

  // mainPipe.io.itlb(0).resp  <>  io.itlb(0).resp
  // prefetchPipe.io.iTLBInter.resp  <>  io.itlb(0).resp

  // when(mainPipe.io.itlb(0).req.fire() && prefetchPipe.io.iTLBInter.req.fire())
  // {
  //   assert(false.B, "Both mainPipe ITLB and prefetchPipe ITLB fire!")
  // }

  io.itlb(0)        <>    mainPipe.io.itlb(0)
  io.itlb(1)        <>    mainPipe.io.itlb(1)
  io.itlb(2)        <>    mainPipe.io.itlb(2)
  io.itlb(3)        <>    mainPipe.io.itlb(3)
  io.itlb(4)        <>    prefetchPipe.io.iTLBInter


  io.fetch.resp     <>    mainPipe.io.fetch.resp

  for(i <- 0 until PortNumber){
    missUnit.io.req(i)           <>   mainPipe.io.mshr(i).toMSHR
    mainPipe.io.mshr(i).fromMSHR <>   missUnit.io.resp(i)
  }

  missUnit.io.prefetch_req <> prefetchPipe.io.toMissUnit.enqReq

  prefetchPipe.io.fromMSHR <> missUnit.io.prefetch_check

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  bus.a <> missUnit.io.mem_acquire
  bus.e <> missUnit.io.mem_finish

  releaseUnit.io.req <>  replacePipe.io.release_req
  replacePipe.io.release_finish := releaseUnit.io.finish
  bus.c <> releaseUnit.io.mem_release

  // connect bus d
  missUnit.io.mem_grant.valid := false.B
  missUnit.io.mem_grant.bits  := DontCare

  releaseUnit.io.mem_grant.valid := false.B
  releaseUnit.io.mem_grant.bits  := DontCare

  //Probe through bus b
  probeQueue.io.mem_probe    <> bus.b

  //Parity error port
  val errors = mainPipe.io.errors ++ Seq(replacePipe.io.error)
  io.error <> RegNext(Mux1H(errors.map(e => e.valid -> e)))


  /** Block set-conflict request */
 val probeReqValid = probeQueue.io.pipe_req.valid
 val probeReqVidx  = probeQueue.io.pipe_req.bits.vidx

  val hasVictim = VecInit(missUnit.io.victimInfor.map(_.valid))
  val victimSetSeq = VecInit(missUnit.io.victimInfor.map(_.vidx))

  val probeShouldBlock = VecInit(hasVictim.zip(victimSetSeq).map{case(valid, idx) =>  valid && probeReqValid && idx === probeReqVidx }).reduce(_||_)

 val releaseReqValid = missUnit.io.release_req.valid
 val releaseReqVidx  = missUnit.io.release_req.bits.vidx

  val hasConflict = VecInit(Seq(
        replacePipe.io.status.r0_set.valid,
        replacePipe.io.status.r1_set.valid,
        replacePipe.io.status.r2_set.valid,
        replacePipe.io.status.r3_set.valid
  ))

  val conflictIdx = VecInit(Seq(
        replacePipe.io.status.r0_set.bits,
        replacePipe.io.status.r1_set.bits,
        replacePipe.io.status.r2_set.bits,
        replacePipe.io.status.r3_set.bits
  ))

  val releaseShouldBlock = VecInit(hasConflict.zip(conflictIdx).map{case(valid, idx) =>  valid && releaseReqValid && idx === releaseReqVidx }).reduce(_||_)

  replace_req_arb.io.in(ReplacePipeKey) <> probeQueue.io.pipe_req
  replace_req_arb.io.in(ReplacePipeKey).valid := probeQueue.io.pipe_req.valid && !probeShouldBlock
  replace_req_arb.io.in(MainPipeKey)   <> missUnit.io.release_req
  replace_req_arb.io.in(MainPipeKey).valid := missUnit.io.release_req.valid && !releaseShouldBlock
  replacePipe.io.pipe_req               <> replace_req_arb.io.out

  when(releaseShouldBlock){
    missUnit.io.release_req.ready := false.B
  }

  when(probeShouldBlock){
    probeQueue.io.pipe_req.ready := false.B
  }


  missUnit.io.release_resp <> replacePipe.io.pipe_resp

  mainPipe.io.fetch.req <> io.fetch.req //&& !fetchShouldBlock(i)
  // in L1ICache, we only expect GrantData and ReleaseAck
  bus.d.ready := false.B
  when ( bus.d.bits.opcode === TLMessages.GrantData) {
    missUnit.io.mem_grant <> bus.d
  } .elsewhen (bus.d.bits.opcode === TLMessages.ReleaseAck) {
    releaseUnit.io.mem_grant <> bus.d
  } .otherwise {
    assert (!bus.d.fire())
  }

  val perfEvents = Seq(
    ("icache_miss_cnt  ", false.B),
    ("icache_miss_penalty", BoolStopWatch(start = false.B, stop = false.B || false.B, startHighPriority = true)),
  )
  generatePerfEvent()

  // Customized csr cache op support
  val cacheOpDecoder = Module(new CSRCacheOpDecoder("icache", CacheInstrucion.COP_ID_ICACHE))
  cacheOpDecoder.io.csr <> io.csr
  dataArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
  dataArray.io.cache_req_dup(0) := cacheOpDecoder.io.cache_req_dup(0)
  dataArray.io.cache_req_dup(1) := cacheOpDecoder.io.cache_req_dup(1)
  dataArray.io.cache_req_dup(2) := cacheOpDecoder.io.cache_req_dup(2)
  dataArray.io.cache_req_dup(3) := cacheOpDecoder.io.cache_req_dup(3)

  metaArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
  cacheOpDecoder.io.cache.resp.valid :=
    dataArray.io.cacheOp.resp.valid ||
    metaArray.io.cacheOp.resp.valid
  cacheOpDecoder.io.cache.resp.bits := Mux1H(List(
    dataArray.io.cacheOp.resp.valid -> dataArray.io.cacheOp.resp.bits,
    metaArray.io.cacheOp.resp.valid -> metaArray.io.cacheOp.resp.bits,
  ))
  cacheOpDecoder.io.error := io.error
  assert(!((dataArray.io.cacheOp.resp.valid +& metaArray.io.cacheOp.resp.valid) > 1.U))

  val (icacheMbistPipelineSram,icacheMbistPipelineRf,icacheMbistPipelineSramRepair,icacheMbistPipelineRfRepair) = placePipelines(level = 2,infoName = s"MBISTPipeline_icache")
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

class ICachePartWayArray[T <: Data](gen: T, pWay: Int, parentName: String = "Unknown")(implicit p: Parameters) extends ICacheArray
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
      singlePort = true,
      parentName = parentName + s"bank${bank}_"
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
