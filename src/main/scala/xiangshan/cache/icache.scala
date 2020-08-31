package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import device._
import xiangshan._
import xiangshan.frontend._
import utils._

import freechips.rocketchip.tilelink.{TLBundleA,TLBundleD,TLBundleE,TLEdgeOut}
import freechips.rocketchip.diplomacy.{AddressSet,IdRange,LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLClientNode, TLClientParameters, TLMasterParameters, TLMasterPortParameters, TLArbiter}
import bus.tilelink.{TLParameters, TLPermissions, ClientMetadata}

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
  val cacheParams = icacheParameters

  // the width of inner CPU data interface
  // override  def tagBits = VAddrBits - untagBits
  def wordBits = DataBits
  def wordBytes = DataBytes
  def wordOffBits = log2Up(wordBytes)
  def beatBytes = cacheParams.blockBytes / cacheDataBeats
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
  def cacheID = 0
  
  def get_beat(addr: UInt) = addr(blockOffBits - 1, beatOffBits)
  def get_tag(addr: UInt) = addr >> untagBits
  def get_idx(addr: UInt) = addr(untagBits-1, blockOffBits)
  def get_block_addr(addr: UInt) = (addr >> blockOffBits) << blockOffBits
  
  val groupAlign = log2Up(FetchWidth * 4 * 2)
  // def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))
  def groupPC(pc: UInt): UInt = Cat(pc(PAddrBits-1, groupAlign), 0.U(groupAlign.W))

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  // To make things easier, now we assume:
  // core_data_width(wordBits) == L1_basic_storage_unit_width(rowBits) ==
  // outer_tilelink_interface_width(cacheDataBits)
  require(rowBits == wordBits, s"rowBits($rowBits) != wordBits($wordBits)")
  require(rowBits == cacheDataBits, s"rowBits($rowBits) != cacheDataBits($cacheDataBits)")
}

// sealed abstract class ICacheModule extends XSModule
//   with HasICacheParameters

abstract class ICacheBundle extends XSBundle
  with HasICacheParameters
abstract class ICacheModule(outer: ICache) extends LazyModuleImp(outer)
  with HasICacheParameters with HasXSLog


sealed class ICacheMetaBundle extends ICacheBundle
{
  val tag = UInt(tagBits.W)
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
  val pc = UInt(VAddrBits.W)
  val data = UInt((FetchWidth * 32).W)
  val mask = UInt(PredictWidth.W)
  val ipf = Bool()
}


class ICacheIO(edge: TLEdgeOut) extends ICacheBundle
{
  val req = Flipped(DecoupledIO(new ICacheReq))
  val resp = DecoupledIO(new ICacheResp)
  val tlb = new BlockTlbRequestIO
  val flush = Input(UInt(2.W))
}


class ICache()(implicit p: Parameters) extends LazyModule
  with HasICacheParameters
{
  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache"))
  )
  val clientNode = TLClientNode(Seq(clientParameters))
  lazy val module = new ICacheImp(this)
  
}


/* ------------------------------------------------------------
 * This module is a SRAM with 4-way associated mapping
 * ------------------------------------------------------------
 */
class ICacheImp(outer: ICache) extends ICacheModule(outer)
{
  val (bus, edge) = outer.clientNode.out.head
  val io = IO(new ICacheIO(edge))

  val (_, _, refill_done, refill_cnt) = edge.count(bus.d)

  //------------------------------------
  //Memory
  val metaArray = Module(new SRAMTemplate(new ICacheMetaBundle, set=nSets, way=nWays, shouldReset = true))
  val dataArray = List.fill(cacheDataBeats){ Module(new SRAMTemplate(new ICacheDataBundle, set=nSets, way = nWays))}

  //----------------------------
  //    Stage 1
  //----------------------------
  val s1_valid = io.req.fire()
  val s1_req_pc = io.req.bits.addr
  val s1_req_mask = io.req.bits.mask
  val s2_ready = WireInit(false.B)
  val s1_fire = s1_valid && (s2_ready || io.flush(0))
  val s1_idx = get_idx(s1_req_pc)

  metaArray.io.r.req.valid := s1_valid
  metaArray.io.r.req.bits.apply(setIdx=s1_idx)
  for(b <- 0 until cacheDataBeats){
    dataArray(b).io.r.req.valid := s1_valid
    dataArray(b).io.r.req.bits.apply(setIdx=s1_idx)
  }
  XSDebug("[Stage 1] v : r : f  (%d  %d  %d)  request pc: 0x%x  mask: %b\n",s1_valid,s2_ready,s1_fire,s1_req_pc,s1_req_mask)
  XSDebug("[Stage 1] index: %d\n",s1_idx)
  
  
  //----------------------------
  //    Stage 2
  //----------------------------
  val s2_valid = RegInit(false.B)
  val s2_req_pc = RegEnable(next = s1_req_pc,init = 0.U, enable = s1_fire)
  val s2_req_mask = RegEnable(next = s1_req_mask,init = 0.U, enable = s1_fire)
  val s3_ready = WireInit(false.B)
  val s2_fire = s2_valid && s3_ready && !io.flush(0) && io.tlb.resp.fire()
  // val s2_tag = get_tag(s2_req_pc)
  val s2_tlb_resp = WireInit(io.tlb.resp.bits)
  val s2_tag = get_tag(s2_tlb_resp.paddr)
  val s2_hit = WireInit(false.B)
  when(io.flush(0)) {s2_valid := s1_fire}
  .elsewhen(s1_fire) { s2_valid := s1_valid}
  .elsewhen(s2_fire) { s2_valid := false.B}

  val metas = metaArray.io.r.resp.asTypeOf(Vec(nWays,new ICacheMetaBundle))
  val datas =dataArray.map(b => RegEnable(next=b.io.r.resp.asTypeOf(Vec(nWays,new ICacheDataBundle)), enable=s2_fire))

  val hitVec = VecInit(metas.map(w => s2_valid && (w.tag === s2_tag) && w.valid))
  val victimWayMask = (1.U << LFSR64()(log2Up(nWays)-1,0))
  val invalidVec = VecInit(metas.map(m => !m.valid))
  val invalidValue = invalidVec.asUInt
  val hasInvalidWay = ParallelOR(invalidVec).asBool
  val refillInvalidWaymask = Mux(invalidValue >= 8.U, "b1000".U,
    Mux(invalidValue >= 4.U, "b0100".U,
    Mux(invalidValue >= 2.U, "b0010".U, "b0001".U)))
  
  val waymask = Mux(s2_hit, hitVec.asUInt, Mux(hasInvalidWay, refillInvalidWaymask, victimWayMask))
 
  s2_hit := ParallelOR(hitVec) || s2_tlb_resp.excp.pf.instr
  s2_ready := s2_fire || !s2_valid || io.flush(0)

  XSDebug("[Stage 2] v : r : f  (%d  %d  %d)  pc: 0x%x  mask: %b\n",s2_valid,s3_ready,s2_fire,s2_req_pc,s2_req_mask)
  XSDebug("[Stage 2] tag: %x  hit:%d\n",s2_tag,s2_hit)
  XSDebug("[Stage 2] victimWayMaks:%b   invalidVec:%b    hitVec:%b    waymask:%b\n",victimWayMask,invalidVec.asUInt,hitVec.asUInt,waymask.asUInt)
  
  
  //----------------------------
  //    Stage 3
  //----------------------------
  val s3_valid = RegInit(false.B)
  val s3_req_pc = RegEnable(next = s2_req_pc,init = 0.U, enable = s2_fire)
  val s3_req_mask = RegEnable(next = s2_req_mask,init = 0.U, enable = s2_fire)
  val s3_tlb_resp = RegEnable(next = s2_tlb_resp, init = 0.U.asTypeOf(new TlbResp), enable = s2_fire)
  val s3_data = datas
  val s3_tag = RegEnable(s2_tag, s2_fire)
  val s3_hit = RegEnable(next=s2_hit,init=false.B,enable=s2_fire)
  val s3_wayMask = RegEnable(next=waymask,init=0.U,enable=s2_fire)
  val s3_miss = s3_valid && !s3_hit
  when(io.flush(1)) { s3_valid := false.B }
  .elsewhen(s2_fire) { s3_valid := s2_valid }
  .elsewhen(io.resp.fire()) { s3_valid := false.B } 

  //icache hit
  val dataHitWay = s3_data.map(b => Mux1H(s3_wayMask,b).asUInt)
  val dataHitWayUInt = (Cat(dataHitWay(7),dataHitWay(6),dataHitWay(5),dataHitWay(4),dataHitWay(3),dataHitWay(2),dataHitWay(1),dataHitWay(0))).asUInt //TODO: this is ugly
  val allInBlock = s3_req_mask.andR
  val outPacket =  Wire(UInt((FetchWidth * 32).W))
  outPacket := dataHitWayUInt >> (s3_req_pc(5,1) << 4)  //TODO: this is ugly

  //icache miss
  val s_idle :: s_memReadReq :: s_memReadResp :: s_wait_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val readBeatCnt = Counter(cacheDataBeats)

  val needFlush = RegInit(false.B)
  when(io.flush(1) && (state =/= s_idle) && (state =/= s_wait_resp)){ needFlush := true.B }
  .elsewhen((state=== s_wait_resp) && needFlush){ needFlush := false.B }

  val refillDataReg = Reg(Vec(cacheDataBeats,new ICacheDataBundle))   //TODO: this is ugly

  switch(state){
    is(s_idle){
      when(s3_miss && io.flush === 0.U){
        state := s_memReadReq
        readBeatCnt.value := 0.U
      }
    }

    is(s_memReadReq){ 
      when(bus.a.fire()){ 
        state := s_memReadResp
      }
    }

    is(s_memReadResp){
      when (edge.hasData(bus.d.bits)) {
        when(bus.d.fire()){
          readBeatCnt.inc()
          refillDataReg(readBeatCnt.value) := bus.d.bits.data.asTypeOf(new ICacheDataBundle)
          when(readBeatCnt.value === (cacheDataBeats - 1).U){
            assert(refill_done, "refill not done!")
            state := s_wait_resp
          }
        }
      }
    }

    is(s_wait_resp){
      when(io.resp.fire() || needFlush){state := s_idle}
    }

  }


  //refill write
  val metaWrite = Wire(new ICacheMetaBundle)
  // metaWrite.tag := get_tag(s3_req_pc)
  metaWrite.tag := s3_tag
  metaWrite.valid := true.B
  metaArray.io.w.req.valid := (state === s_memReadResp) && bus.d.fire() && refill_done
  metaArray.io.w.req.bits.apply(data=metaWrite, setIdx=get_idx(s3_req_pc), waymask=s3_wayMask)

  val refillDataOut = refillDataReg.asUInt >> (s3_req_pc(5,1) << 4)
  for(b <- 0 until cacheDataBeats){
    val writeOneBeat = (state === s_memReadResp) && bus.d.fire() && (b.U === readBeatCnt.value)
    dataArray(b).io.w.req.valid := writeOneBeat
    dataArray(b).io.w.req.bits.apply(   setIdx=get_idx(s3_req_pc), 
                                        data=bus.d.bits.data.asTypeOf(new ICacheDataBundle), 
                                        waymask=s3_wayMask)

  }

  s3_ready := ((io.resp.fire() || !s3_valid) && !needFlush) || (needFlush && state === s_wait_resp)

  //TODO: coherence
  XSDebug("[Stage 3] valid:%d   pc: 0x%x  mask: %b \n",s3_valid,s3_req_pc,s3_req_mask)
  XSDebug("[Stage 3] hit:%d  miss:%d  waymask:%x \n",s3_hit,s3_miss,s3_wayMask.asUInt)
  XSDebug("[Stage 3] state: %d\n",state)
  XSDebug("[Stage 3] needflush:%d, refilldone:%d",needFlush,refill_done)
  XSDebug("[Stage 3] tag: %x    idx: %d\n",get_tag(s3_req_pc),get_idx(s3_req_pc))
  XSDebug("[Chanel A] valid:%d  ready:%d\n",bus.a.valid,bus.a.ready)
  XSDebug("[Chanel D] valid:%d  ready:%d  data:%x  readBeatcnt:%d \n",bus.d.valid,bus.d.ready,bus.d.bits.data,readBeatCnt.value)
  XSDebug("[Stage 3] ---------Hit Way--------- \n")
  for(i <- 0 until cacheDataBeats){
      XSDebug("[Stage 3] %x\n",dataHitWay(i))
  }
  XSDebug("[Stage 3] outPacket :%x\n",outPacket)
  XSDebug("[Stage 3] refillDataOut :%x\n",refillDataOut)
  //-----------out put------------
  val dataArrayReadyVec = dataArray.map(b => b.io.r.req.ready)
  io.req.ready := metaArray.io.r.req.ready && ParallelOR(dataArrayReadyVec) && s2_ready
  
  io.resp.valid := s3_valid && (s3_hit || state === s_wait_resp)
  io.resp.bits.data := Mux((s3_valid && s3_hit),outPacket,refillDataOut)
  io.resp.bits.mask := s3_req_mask
  io.resp.bits.pc := s3_req_pc
  io.resp.bits.ipf := s3_tlb_resp.excp.pf.instr

  io.tlb.resp.ready := s3_ready
  io.tlb.req.valid := s2_valid
  io.tlb.req.bits.vaddr := s2_req_pc
  io.tlb.req.bits.cmd := TlbCmd.exec
  io.tlb.req.bits.roqIdx := DontCare
  io.tlb.req.bits.debug.pc := s2_req_pc
  io.tlb.req.bits.debug.lsroqIdx := DontCare
  
  bus.b.ready := true.B
  bus.c.valid := false.B
  bus.e.valid := false.B
  bus.a.valid := (state === s_memReadReq)
  bus.a.bits  := edge.Get(
    fromSource      = cacheID.U,
    // toAddress       = groupPC(s3_req_pc),
    toAddress       = groupPC(s3_tlb_resp.paddr)
    lgSize          = (log2Up(cacheParams.blockBytes)).U)._2 

  bus.d.ready := true.B


  XSDebug("[flush] flush_0:%d  flush_1:%d\n",io.flush(0),io.flush(1))
}

//TODO: consider L2 or L3 cache connection

