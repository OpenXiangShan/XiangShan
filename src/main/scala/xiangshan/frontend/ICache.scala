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

package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange}
import freechips.rocketchip.tilelink._
import xiangshan._
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
    nMissEntries: Int = 1,
    nMMIOs: Int = 1,
    blockBytes: Int = 64
)extends L1CacheParameters {

  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = ReplacementPolicy.fromString(replacer,nWays,nSets)
}

trait HasICacheParameters extends HasL1CacheParameters with HasInstrMMIOConst {
  val cacheParams = icacheParameters
  def highestIdxBit = log2Ceil(nSets) - 1

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
}

abstract class ICacheBundle(implicit p: Parameters) extends XSBundle
  with HasICacheParameters

abstract class ICacheModule(implicit p: Parameters) extends XSModule
  with HasICacheParameters

abstract class ICacheArray(implicit p: Parameters) extends XSModule
  with HasICacheParameters

class ICacheReadBundle(implicit p: Parameters) extends ICacheBundle
{
  val isDoubleLine  = Bool()
  val vSetIdx       = Vec(2,UInt(log2Ceil(nSets).W))
}

class ICacheMetaRespBundle(implicit p: Parameters) extends ICacheBundle
{
  val tags  = Vec(2,Vec(nWays ,UInt(tagBits.W)))
  val valid = Vec(2,Vec(nWays ,Bool()))
}

class ICacheMetaWriteBundle(implicit p: Parameters) extends ICacheBundle
{
  val virIdx  = UInt(idxBits.W)
  val phyTag  = UInt(tagBits.W)
  val waymask = UInt(nWays.W)
  val bankIdx = Bool()

  def apply(tag:UInt, idx:UInt, waymask:UInt, bankIdx: Bool){
    this.virIdx  := idx
    this.phyTag  := tag
    this.waymask := waymask
    this.bankIdx   := bankIdx
  }

}

class ICacheDataWriteBundle(implicit p: Parameters) extends ICacheBundle
{
  val virIdx  = UInt(idxBits.W)
  val data    = UInt(blockBits.W)
  val waymask = UInt(nWays.W)
  val bankIdx = Bool()

  def apply(data:UInt, idx:UInt, waymask:UInt, bankIdx: Bool){
    this.virIdx  := idx
    this.data    := data
    this.waymask := waymask
    this.bankIdx := bankIdx
  }

}

class ICacheDataRespBundle(implicit p: Parameters) extends ICacheBundle
{
  val datas = Vec(2,Vec(nWays,UInt(blockBits.W)))
}

class ICacheMetaReadBundle(implicit p: Parameters) extends ICacheBundle
{
    val req     = Flipped(DecoupledIO(new ICacheReadBundle))
    val resp = Output(new ICacheMetaRespBundle)
}

class ICacheCommonReadBundle(isMeta: Boolean)(implicit p: Parameters) extends ICacheBundle
{
    val req     = Flipped(DecoupledIO(new ICacheReadBundle))
    val resp    = if(isMeta) Output(new ICacheMetaRespBundle) else Output(new ICacheDataRespBundle)
}


class ICacheMetaArray(implicit p: Parameters) extends ICacheArray
{
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

  val tagArrays = (0 until 2) map { bank =>
    val tagArray = Module(new SRAMTemplate(
      UInt(tagBits.W),
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
      tagArray.io.w.req.bits.apply(data=io.write.bits.phyTag, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }
    else {
      tagArray.io.r.req.valid := port_0_read_1 || port_1_read_1
      tagArray.io.r.req.bits.apply(setIdx=bank_1_idx(highestIdxBit,1))
      tagArray.io.w.req.valid := write_bank_1
      tagArray.io.w.req.bits.apply(data=io.write.bits.phyTag, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }
    tagArray
  }

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

  io.readResp.tags <> DontCare
  when(port_0_read_0_reg){
    io.readResp.tags(0) := tagArrays(0).io.r.resp.asTypeOf(Vec(nWays, UInt(tagBits.W)))
  }.elsewhen(port_0_read_1_reg){
    io.readResp.tags(0) := tagArrays(1).io.r.resp.asTypeOf(Vec(nWays, UInt(tagBits.W)))
  }

  when(port_1_read_0_reg){
    io.readResp.tags(1) := tagArrays(0).io.r.resp.asTypeOf(Vec(nWays, UInt(tagBits.W)))
  }.elsewhen(port_1_read_1_reg){
    io.readResp.tags(1) := tagArrays(1).io.r.resp.asTypeOf(Vec(nWays, UInt(tagBits.W)))
  }

  (io.readResp.valid zip validMetas).map  {case (io, reg)   => io := reg.asTypeOf(Vec(nWays,Bool()))}

  io.write.ready := DontCare

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
  io.write.ready := true.B
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
      dataArray.io.w.req.bits.apply(data=io.write.bits.data, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }
    else {
      dataArray.io.r.req.valid := port_0_read_1 || port_1_read_1
      dataArray.io.r.req.bits.apply(setIdx=bank_1_idx(highestIdxBit,1))
      dataArray.io.w.req.valid := write_bank_1
      dataArray.io.w.req.bits.apply(data=io.write.bits.data, setIdx=io.write.bits.virIdx(highestIdxBit,1), waymask=io.write.bits.waymask)
    }

    dataArray
  }

  io.readResp.datas(0) := Mux( port_0_read_1_reg, dataArrays(1).io.r.resp.asTypeOf(Vec(nWays, UInt(blockBits.W))) , dataArrays(0).io.r.resp.asTypeOf(Vec(nWays, UInt(blockBits.W))))
  io.readResp.datas(1) := Mux( port_1_read_0_reg, dataArrays(0).io.r.resp.asTypeOf(Vec(nWays, UInt(blockBits.W))) , dataArrays(1).io.r.resp.asTypeOf(Vec(nWays, UInt(blockBits.W))))
  
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


abstract class ICacheMissQueueModule(implicit p: Parameters) extends XSModule
  with HasICacheParameters

abstract class ICacheMissQueueBundle(implicit p: Parameters) extends XSBundle
  with HasICacheParameters

class ICacheMissReq(implicit p: Parameters) extends ICacheBundle
{
    val addr      = UInt(PAddrBits.W)
    val vSetIdx   = UInt(idxBits.W)
    val waymask   = UInt(16.W)
    val clientID  = UInt(1.W)
    def apply(missAddr:UInt, missIdx:UInt, missWaymask:UInt, source:UInt) = {
      this.addr := missAddr
      this.vSetIdx  := missIdx
      this.waymask := missWaymask
      this.clientID := source
    }
    override def toPrintable: Printable = {
      p"addr=0x${Hexadecimal(addr)} vSetIdx=0x${Hexadecimal(vSetIdx)} waymask=${Binary(waymask)} clientID=${Binary(clientID)}"
    }
}

class ICacheMissResp(implicit p: Parameters) extends ICacheBundle
{
    val data     = UInt(blockBits.W)
    val clientID = UInt(1.W)
}

class ICacheMissBundle(implicit p: Parameters) extends ICacheBundle{
    val req         = Vec(2, Flipped(DecoupledIO(new ICacheMissReq)))
    val resp        = Vec(2,DecoupledIO(new ICacheMissResp))
    val flush       = Input(Bool())
}

class ICacheMissEntry(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheMissQueueModule
{
    val io = IO(new Bundle{
        // MSHR ID
        val id          = Input(UInt(1.W))

        val req         = Flipped(DecoupledIO(new ICacheMissReq))
        val resp        = DecoupledIO(new ICacheMissResp)

        val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
        val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

        val meta_write  = DecoupledIO(new ICacheMetaWriteBundle)
        val data_write  = DecoupledIO(new ICacheDataWriteBundle)

        val flush = Input(Bool())
    })

    val s_idle :: s_memReadReq :: s_memReadResp :: s_write_back :: s_wait_resp :: Nil = Enum(5)
    val state = RegInit(s_idle)

    //req register
    val req = Reg(new ICacheMissReq)
    val req_idx = req.vSetIdx         //virtual index
    val req_tag = get_phy_tag(req.addr)           //physical tag
    val req_waymask = req.waymask

    val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)

    //8 for 64 bits bus and 2 for 256 bits
    val readBeatCnt = Reg(UInt(log2Up(refillCycles).W))
    val respDataReg = Reg(Vec(refillCycles,UInt(beatBits.W)))

    //initial
    io.resp.bits := DontCare
    io.mem_acquire.bits := DontCare
    io.mem_grant.ready := true.B
    io.meta_write.bits := DontCare
    io.data_write.bits := DontCare

    io.req.ready := state === s_idle
    io.mem_acquire.valid := state === s_memReadReq

    //flush register
    val needFlush = RegInit(false.B)
    when(io.flush && (state =/= s_idle) && (state =/= s_wait_resp)){ needFlush := true.B }
    .elsewhen((state=== s_wait_resp) && needFlush){ needFlush := false.B }

    //state change
    switch(state){
      is(s_idle){
        when(io.req.fire()){
          readBeatCnt := 0.U
          state := s_memReadReq
          req := io.req.bits
        }
      }

      // memory request
      is(s_memReadReq){
        when(io.mem_acquire.fire()){
          state := s_memReadResp
        }
      }

      is(s_memReadResp){
        when (edge.hasData(io.mem_grant.bits)) {
          when (io.mem_grant.fire()) {
            readBeatCnt := readBeatCnt + 1.U
            respDataReg(readBeatCnt) := io.mem_grant.bits.data
            when (readBeatCnt === (refillCycles - 1).U) {
              assert(refill_done, "refill not done!")
              state := Mux(needFlush || io.flush, s_wait_resp, s_write_back)
            }
          }
        }
      }

      is(s_write_back){
          state := s_wait_resp
      }

      is(s_wait_resp){
        io.resp.bits.data := respDataReg.asUInt
	      io.resp.bits.clientID := req.clientID
        when(io.resp.fire() || needFlush ){ state := s_idle }
      }

    }

    //refill write and meta write
    //WARNING: Maybe could not finish refill in 1 cycle
    io.meta_write.valid := (state === s_write_back) && !needFlush
    io.meta_write.bits.apply(tag=req_tag, idx=req_idx, waymask=req_waymask, bankIdx=req_idx(0))

    io.data_write.valid := (state === s_write_back) && !needFlush
    io.data_write.bits.apply(data=respDataReg.asUInt, idx=req_idx, waymask=req_waymask, bankIdx=req_idx(0))

    //mem request
    io.mem_acquire.bits  := edge.Get(
      fromSource      = io.id,
      toAddress       = Cat(req.addr(PAddrBits - 1, log2Ceil(blockBytes)), 0.U(log2Ceil(blockBytes).W)),
      lgSize          = (log2Up(cacheParams.blockBytes)).U)._2


    //resp to icache
    io.resp.valid := (state === s_wait_resp) && !needFlush

    XSDebug("[ICache MSHR %d] (req)valid:%d  ready:%d req.addr:%x waymask:%b  || Register: req:%x  \n",io.id.asUInt,io.req.valid,io.req.ready,io.req.bits.addr,io.req.bits.waymask,req.asUInt)
    XSDebug("[ICache MSHR %d] (Info)state:%d  needFlush:%d\n",io.id.asUInt,state,needFlush)
    XSDebug("[ICache MSHR %d] (mem_acquire) valid%d ready:%d\n",io.id.asUInt,io.mem_acquire.valid,io.mem_acquire.ready)
    XSDebug("[ICache MSHR %d] (mem_grant)   valid%d ready:%d data:%x \n",io.id.asUInt,io.mem_grant.valid,io.mem_grant.ready,io.mem_grant.bits.data)
    XSDebug("[ICache MSHR %d] (meta_write)  valid%d ready:%d  tag:%x \n",io.id.asUInt,io.meta_write.valid,io.meta_write.ready,io.meta_write.bits.phyTag)
    XSDebug("[ICache MSHR %d] (refill)  valid%d ready:%d  data:%x \n",io.id.asUInt,io.data_write.valid,io.data_write.ready,io.data_write.bits.data.asUInt())
    XSDebug("[ICache MSHR %d] (resp)  valid%d ready:%d \n",io.id.asUInt,io.resp.valid,io.resp.ready)
  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(2))
  })
  val perfEvents = Seq(
    ("icache_miss_cnt         ", io.req.fire()                                ),
    ("icache_miss_penty       ", BoolStopWatch(start = io.req.fire(), stop = io.resp.fire() || io.flush, startHighPriority = true)                               ),
  )
  for (((perf_out,(perf_name,perf)),i) <- perfinfo.perfEvents.perf_events.zip(perfEvents).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }


}

class ICacheMissQueue(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheMissQueueModule
{
  val io = IO(new Bundle{
    val req         = Vec(2, Flipped(DecoupledIO(new ICacheMissReq)))
    val resp        = Vec(2, DecoupledIO(new ICacheMissResp))

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val meta_write  = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write  = DecoupledIO(new ICacheDataWriteBundle)

    val flush       = Input(Bool())
    val fencei       = Input(Bool())

  })

  // assign default values to output signals
  io.mem_grant.ready := false.B

  val meta_write_arb = Module(new Arbiter(new ICacheMetaWriteBundle,  2))
  val refill_arb     = Module(new Arbiter(new ICacheDataWriteBundle,  2))

  io.mem_grant.ready := true.B

  val entries = (0 until 2) map { i =>
    val entry = Module(new ICacheMissEntry(edge))

    entry.io.id := i.U(1.W)
    entry.io.flush := io.flush || io.fencei

    // entry req
    entry.io.req.valid := io.req(i).valid
    entry.io.req.bits  := io.req(i).bits
    io.req(i).ready    := entry.io.req.ready

    // entry resp
    meta_write_arb.io.in(i)     <>  entry.io.meta_write
    refill_arb.io.in(i)         <>  entry.io.data_write

    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      entry.io.mem_grant <> io.mem_grant
    }

    io.resp(i) <> entry.io.resp

    XSPerfAccumulate(
      "entryPenalty" + Integer.toString(i, 10),
      BoolStopWatch(
        start = entry.io.req.fire(),
        stop = entry.io.resp.fire() || entry.io.flush,
        startHighPriority = true)
    )
    XSPerfAccumulate("entryReq" + Integer.toString(i, 10), entry.io.req.fire())

    entry
  }

  TLArbiter.lowestFromSeq(edge, io.mem_acquire, entries.map(_.io.mem_acquire))

  io.meta_write     <> meta_write_arb.io.out
  io.data_write     <> refill_arb.io.out
  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(2*2))
  })
  val entry_0_perf = entries(0).perfEvents.map(_._1).zip(entries(0).perfinfo.perfEvents.perf_events)
  val entry_1_perf = entries(1).perfEvents.map(_._1).zip(entries(1).perfinfo.perfEvents.perf_events)
  val perfEvents = entry_0_perf ++ entry_1_perf
  perfinfo.perfEvents.perf_events := entries(0).perfinfo.perfEvents.perf_events ++ entries(1).perfinfo.perfEvents.perf_events

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_0_refill_way_" + Integer.toString(w, 10),  entries(0).io.meta_write.valid && OHToUInt(entries(0).io.meta_write.bits.waymask)  === w.U)
    XSPerfAccumulate("line_1_refill_way_" + Integer.toString(w, 10),  entries(1).io.meta_write.valid && OHToUInt(entries(1).io.meta_write.bits.waymask)  === w.U)
  }

}

class ICacheIO(implicit p: Parameters) extends ICacheBundle
{
  val metaRead    = new ICacheCommonReadBundle(isMeta = true)
  val dataRead    = new ICacheCommonReadBundle(isMeta = false)
  val missQueue   = new ICacheMissBundle
  val fencei      = Input(Bool())
  val csr         = new L1CacheToCsrIO
}

class ICache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, cacheParams.nMissEntries)
    ))
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

  metaArray.io.write <> missQueue.io.meta_write
  dataArray.io.write <> missQueue.io.data_write

  metaArray.io.read      <> io.metaRead.req
  metaArray.io.readResp  <> io.metaRead.resp

  dataArray.io.read      <> io.dataRead.req
  dataArray.io.readResp  <> io.dataRead.resp

  for(i <- 0 until 2){
    missQueue.io.req(i)           <> io.missQueue.req(i)
    missQueue.io.resp(i)          <> io.missQueue.resp(i)
  }

  missQueue.io.flush := io.missQueue.flush
  missQueue.io.fencei := io.fencei
  metaArray.io.fencei := io.fencei
  bus.a <> missQueue.io.mem_acquire
  missQueue.io.mem_grant      <> bus.d

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

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

  val perfEvents = missQueue.perfEvents.map(_._1).zip(missQueue.perfinfo.perfEvents.perf_events)
  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(2*2))
  })
  perfinfo.perfEvents := missQueue.perfinfo.perfEvents
} 
