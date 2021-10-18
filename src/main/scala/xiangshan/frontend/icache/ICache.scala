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
import huancun.{AliasField, PreferCacheField, PrefetchField}
import xiangshan._
import xiangshan.frontend._
import xiangshan.cache._
import utils._

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
  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = ReplacementPolicy.fromString(replacer,nWays,nSets)
}

trait HasICacheParameters extends HasL1CacheParameters with HasInstrMMIOConst with HasIFUConst{
  val cacheParams = icacheParameters
  val dataCodeUnit = 8
  val dataUnitNum  = blockBits/dataCodeUnit

  def dataCodeBits  = cacheParams.dataCode.width(dataCodeUnit)
  def dataEntryBits = dataCodeBits * dataUnitNum

  val ICacheSets = cacheParams.nSets
  val ICacheWays = cacheParams.nWays

  val ICacheSameVPAddrLength = 12

  val ICacheWordOffset = 0
  val ICacheSetOffset = ICacheWordOffset + log2Up(blockBytes)
  val ICacheAboveIndexOffset = ICacheSetOffset + log2Up(ICacheSets)
  val ICacheTagOffset = ICacheAboveIndexOffset min ICacheSameVPAddrLength

  def ProbeKey = 0
  def FetchKey = 1

  def nMissEntries = cacheParams.nMissEntries

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
      tagArray.io.r.req.bits.apply(setIdx=bank_0_idx)
      tagArray.io.w.req.valid := write_bank_0
      tagArray.io.w.req.bits.apply(data=write_meta_bits, setIdx=io.write.bits.virIdx, waymask=io.write.bits.waymask)
    }
    else {
      tagArray.io.r.req.valid := port_0_read_1 || port_1_read_1
      tagArray.io.r.req.bits.apply(setIdx=bank_1_idx)
      tagArray.io.w.req.valid := write_bank_1
      tagArray.io.w.req.bits.apply(data=write_meta_bits, setIdx=io.write.bits.virIdx, waymask=io.write.bits.waymask)
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
}


class ICacheDataArray(implicit p: Parameters) extends ICacheArray
{
  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheDataWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle))
    val readResp = Output(new ICacheDataRespBundle)
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
      dataArray.io.r.req.bits.apply(setIdx=bank_0_idx)
      dataArray.io.w.req.valid := write_bank_0
      dataArray.io.w.req.bits.apply(data=write_data_bits, setIdx=io.write.bits.virIdx, waymask=io.write.bits.waymask)
    }
    else {
      dataArray.io.r.req.valid := port_0_read_1 || port_1_read_1
      dataArray.io.r.req.bits.apply(setIdx=bank_1_idx)
      dataArray.io.w.req.valid := write_bank_1
      dataArray.io.w.req.bits.apply(data=write_data_bits, setIdx=io.write.bits.virIdx, waymask=io.write.bits.waymask)
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
}


class ICacheIO(implicit p: Parameters) extends ICacheBundle
{
  val metaRead    = new ICacheCommonReadBundle(isMeta = true)
  val dataRead    = new ICacheCommonReadBundle(isMeta = false)
  val missQueue   = new ICacheMissBundle
  val releaseUnit = new ICacheReleaseBundle
  val fencei      = Input(Bool())
}

class ICache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, cacheParams.nMissEntries + cacheParams.nReleaseEntries),
      supportsProbe = TransferSizes(blockBytes)
    )),
    requestFields = cacheParams.reqFields
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new ICacheImp(this)
}

class ICacheImp(outer: ICache) extends LazyModuleImp(outer) with HasICacheParameters {
  val io = IO(new ICacheIO)

  val (bus, edge) = outer.clientNode.out.head

  val metaArray      = Module(new ICacheMetaArray)
  val dataArray      = Module(new ICacheDataArray)
  val missQueue      = Module(new ICacheMissQueue(edge))
  val releaseUnit    = Module(new ReleaseUnit(edge))
  val probe          = Module(new ICacheProbe)
  val probeQueue     = Module(new ICacheProbeQueue(edge))

  val meta_read_arb = Module(new Arbiter(new ICacheReadBundle,  2))
  val data_read_arb = Module(new Arbiter(new ICacheReadBundle,  2))
  val meta_write_arb = Module(new Arbiter(new ICacheMetaWriteBundle(),  2))
  val release_arb    = Module(new Arbiter(new RealeaseReq, 2))

  meta_read_arb.io.in(ProbeKey) <> probe.io.meta_read
  meta_read_arb.io.in(FetchKey) <> io.metaRead.req
  metaArray.io.read      <> meta_read_arb.io.out
  metaArray.io.readResp  <> probe.io.meta_response
  metaArray.io.readResp  <> io.metaRead.resp

  data_read_arb.io.in(ProbeKey) <> probe.io.data_read
  data_read_arb.io.in(FetchKey) <> io.dataRead.req
  dataArray.io.read      <> data_read_arb.io.out
  dataArray.io.readResp  <> probe.io.data_response
  dataArray.io.readResp  <> io.dataRead.resp

  meta_write_arb.io.in(FetchKey) <> missQueue.io.meta_write
  meta_write_arb.io.in(ProbeKey) <> probe.io.meta_write

  metaArray.io.write <> meta_write_arb.io.out
  dataArray.io.write <> missQueue.io.data_write

  release_arb.io.in(ProbeKey) <> probe.io.release_req
  release_arb.io.in(FetchKey) <> io.releaseUnit.req(0)

  for(i <- 0 until 2){
    missQueue.io.req(i)           <> io.missQueue.req(i)
    missQueue.io.resp(i)          <> io.missQueue.resp(i)
  }
  
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  missQueue.io.flush := io.missQueue.flush
  missQueue.io.fencei := io.fencei
  metaArray.io.fencei := io.fencei
  bus.a <> missQueue.io.mem_acquire
  bus.e <> missQueue.io.mem_finish

  releaseUnit.io.req(0)  <>  release_arb.io.out
  releaseUnit.io.req(1)  <>  io.releaseUnit.req(1)
  bus.c <> releaseUnit.io.mem_release

  // connect bus d
  missQueue.io.mem_grant.valid := false.B
  missQueue.io.mem_grant.bits  := DontCare

  releaseUnit.io.mem_grant.valid := false.B
  releaseUnit.io.mem_grant.bits  := DontCare

  //Probe through bus b
  probeQueue.io.mem_probe    <> bus.b
  probe.io.req               <> probeQueue.io.pipe_req

  // in L1ICache, we only expect GrantData and ReleaseAck
  bus.d.ready := false.B
  when ( bus.d.bits.opcode === TLMessages.GrantData) {
    missQueue.io.mem_grant <> bus.d
  } .elsewhen (bus.d.bits.opcode === TLMessages.ReleaseAck) {
    releaseUnit.io.mem_grant <> bus.d
  } .otherwise {
    assert (!bus.d.fire())
  }
} 