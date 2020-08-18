package xiangshan.cache

import chisel3._
import chisel3.util._
import xiangshan.frontend
import utils._

import bus.tilelink.{TLParameters, TLPermissions, ClientMetadata}
import utils.{Code, RandomReplacement}

// DCache specific parameters
// L1 DCache is 64set, 8way-associative, with 64byte block, a total of 32KB
// It's a virtually indexed, physically tagged cache.
case class ICacheParameters(
    nSets: Int = 64,
    nWays: Int = 4,
    rowBits: Int = 64,
    nTLBEntries: Int = 32,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    dataECCBytes: Int = 1,
    nMSHRs: Int = 1,
    nSDQ: Int = 17,
    nRPQ: Int = 16,
    nMMIOs: Int = 1,
    blockBytes: Int = 64) extends L1CacheParameters {

  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)

  def replacement = new RandomReplacement(nWays)
}

trait HasICacheParameters extends HasL1CacheParameters {
  val cacheParams = dcacheParameters
  val cfg = cacheParams

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
  def rowWords = rowBits/wordBits
  def doNarrowRead = DataBits * nWays % rowBits == 0
  def eccBytes = cacheParams.dataECCBytes
  val eccBits = cacheParams.dataECCBytes * 8
  val encBits = cacheParams.dataCode.width(eccBits)
  val encWordBits = encBits * (wordBits / eccBits)
  def encDataBits = cacheParams.dataCode.width(wordBits) // NBDCache only
  def encRowBits = encDataBits*rowWords
  
  def get_beat(addr: UInt) = addr(blockOffBits - 1, beatOffBits)
  def get_tag(addr: UInt) = addr >> untagBits
  def get_idx(addr: UInt) = addr(untagBits-1, blockOffBits)
  def get_block_addr(addr: UInt) = (addr >> blockOffBits) << blockOffBits
  
  val groupAlign = log2Up(FetchWidth * 4 * 2)
  def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  // To make things easier, now we assume:
  // core_data_width(wordBits) == L1_basic_storage_unit_width(rowBits) ==
  // outer_tilelink_interface_width(cacheDataBits)
  require(rowBits == wordBits, s"rowBits($rowBits) != wordBits($wordBits)")
  require(rowBits == cacheDataBits, s"rowBits($rowBits) != cacheDataBits($cacheDataBits)")
}

sealed abstract class ICacheModule extends Module
  with HasICacheParameters

sealed abstract class ICacheBundle extends Bundle
  with HasICacheParameters

sealed class ICacheMetaBundle extends ICacheBundle
{
  val tag = UInt(tagBit.W)
  val valid = Bool()
  //val coh = new ClientMetadata
}

sealed class ICacheDataBundle extends ICacheBundle
{
  val data = UInt(cacheDataBits.W)
}

class ICacheReq extends ICacheBundle
{
  //TODO
  val addr = UInt(VAddrBits.W)
  val mask = UInt(PredictWidth.W)
}

class ICacheResp extends ICacheBundle
{
  //TODO
  val data = UInt((FetchWidth * 32).W)
  val mask = UInt(PredictWidth.W)
}

class ICacheIO extends ICacheBundle
{
  val req = Flipped(DecoupledIO(new ICacheReq))
  val resp = DecoupledIO(new ICacheResp)
  val mem_acquire = DecoupledIO(new FakeIcacheReq)
  val mem_grant = Flipped(DecoupledIO(new FakeIcacheResp))
  val flush = Input(UInt(2.W))
}


class ICache extends ICacheModule
{
  val io = IO(new ICacheIO)

  val metaArray = Module(new SRAMTemplate(new ICacheMetaBundle, set=nSets, way=nWays, shouldReset = true))
  val dataArray = List.fill(cacheDataBeats){ Module(new SRAMTemplate(new ICacheDataBundle, set=nSets, way = nWays))}

  //-----------Stage 1-------------
  val s1_valid = io.req.fire()
  val s1_req = io.req.bits
  val s1_idx = get_idx(s1_req.addr)
  val s2_ready = WireInit(false.B)
  val s1_fire = s1_valid && s2_ready

  metaArray.io.r.req.valid := s1_valid
  metaArray.io.r.req.btis.setIdx := s1_idx
  for(b <- 0 until cacheDataBeats){
    dataArray(i).io.r.req.valid := s1_valid
    dataArray(i).io.r.req.bits := s1_idx
  }
  //-----------Stage 2--------------
  val s2_valid = RegEnable(next = s1_valid, init = false.B, enable = s1_fire)
  val s2_req = RegEnable(next = s1_req,init = 0.U, enable = s1_fire)
  val s2_tag = get_tag(s2_req.addr)
  val s2_hit = WireInit(false.B)
  val s3_ready = WireInit(false.B)
  val s2_fire = s2_valid && s3_ready

  val metas = metaArray.io.r.resp.asTypeOf(Vec(nWays,new ICacheMetaBundle))
  val datas = dataArray.map(b => b.io.r.resp.asTypeOf(Vec(nWays,new ICacheDataBundle)))

  val hitVec = VecInit(metas.map(w => s2_valid && (w.tag === s2_tag) && w.valid))
  val victimWayMask = (1.U << LFSR64()(log2Up(nWays)-1,0))
  val invalidVec = VecInit(metaWay.map(m => !m.valid))
  val hasInvalidWay = ParallelOR(invalidVec)
  val refillInvalidWaymask = Mux(invalidVec >= 8.U, "b1000".U,
    Mux(invalidVec >= 4.U, "b0100".U,
    Mux(invalidVec >= 2.U, "b0010".U, "b0001".U)))
  
  val waymask = Mux(io.out.bits.hit, hitVec, Mux(hasInvalidWay, refillInvalidWaymask, victimWaymask))
 
  s2_hit := ParallelOR(hitVec)
  s2_ready := s2_fire || !s2_valid || io.flush(0)


  //------------Stage 3-------------
  val s3_valid = RegEnable(next=s2_valid,init=false.B,enable=s2_fire)
  val s3_req = RegEnable(next=s2_req,init=false.B,enable=s2_fire)
  val s3_data = RegEnable(next=datas,init=0.U,enable=s2_fire)
  val s3_hit = RegEnable(next=s2_hit,init=false.B,enable=s2_fire)
  val s3_wayMask = RegEnable(next=waymask,init=0.U,enable=s2_fire)
  val s3_miss = s3_valid && !s3_hit

  //icache hit
  val dataHitWay = s3_data.map(b => Mux1H(s3_wayMask,b))
  val chooseMask = Reverse(s3_req.mask).asTypeOf(Vec(PredictWidth,Bool()))
  val allInBlock = ParallelAND(chooseMask)
  val fetchPacketStart = get_beat(s3_req.pc)
  val outPacket =  Wire(UInt((FetchWidth * 32).W))
  outPacket := dataHitWay >> (s3_req.pc(5,1) << 4)  //TODO: this is ugly

  //icache miss
  val s_idle :: s_memReadReq :: s_memReadResp :: s_wait_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val readBeatCnt = Counter(cacheDataBeats)

  switch(state){
    is(s_idle){
      when(s3_miss && io.flush == 0.U){
        state := s_memReadReq
        readBeatCnt := 0.U
      }
    }

    is(s_memReadReq){ when(io.mem_acquire.fire()){ state := s_memReadResp}}
    is(s_memReadResp){
      when(io.mem_grant.fire()){
        readBeatCnt.inc()
        when(io.mem_grant.bits.finish){state := s_wait_resp}
      }
    }

    is(s_wait_resp){
      when(io.out.fire()||io.flush(0)||io.flush(1)){state := s_idle}
    }

  }

  io.mem_acquire.valid := (state === s_memReadReq) 
  io.mem_acquire.bits.addr := groupPC(s3_req.addr)

  io.mem_grant.ready := true.B

  //refill write
  val metaWrite = WireInit(new ICacheMetaBundle)
  metaWrite.tag := get_tag(s3_req.addr)
  metaWrite.valid := true.B
  metaArray.io.w.req.valid := (state === s_memReadResp) && io.mem_grant.fire() && io.mem_grant.bits.finish
  metaArray.io.w.req.bits.setIdx := get_idx(s3_req.addr)
  metaArray.io.w.req.bits.data := metaWrite
  metaArray.io.w.req.bits.waymask := s3_wayMask

  val refillDataReg = Reg(Vec(cacheDataBeats,new ICacheDataBundle))   //TODO: this is ugly
  val refillDataOut = refillDataReg >> (s3_req.pc(5,1) << 4)
  for(b <- 0 until cacheDataBeats){
    dataArray(i).io.w.req.valid := (state === s_memReadResp) && io.mem_grant.fire() && (i.U === readBeatCnt.value)
    dataArray(i).io.w.req.bits.setIdx := get_idx(s3_req.addr)
    dataArray(i).io.w.req.bits.data := io.mem_grant.bits.data
    dataArray(i).io.w.req.bits.waymask := s3_wayMask

    when((state === s_memReadResp) && io.mem_grant.fire()){refillDataReg(i) := io.mem_grant.bits.data}
  }


  s3_ready := !s3_valid || io.resp.fire() || io.flush(1)

  //TODO: coherence

  //-----------out put------------
  io.req.ready := metaArray.io.r.req.ready && dataArray.io.r.req.ready && s2_ready
  
  io.resp.valid := (s3_valid && s3_hit) || (state === s_wait_resp)
  io.resp.bits.data := Mux((s3_valid && s3_hit),outPacket,refillDataOut)
  io.resp.bits.mask := s3_req.mask

  when (io.flush(0)) { s2_valid := s1_fire }
  when (io.flush(1)) { s3_valid := false.B }


}

