package xiangshan.cache

import chisel3._
import chisel3.util._
import device._
import xiangshan._
import xiangshan.frontend._
import utils._
import chisel3.ExcitingUtils._
import chisel3.util.experimental.BoringUtils
import chipsalliance.rocketchip.config.Parameters

import freechips.rocketchip.tilelink.{TLBundleA,TLBundleD,TLBundleE,TLEdgeOut}
import freechips.rocketchip.diplomacy.{AddressSet,IdRange,LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLClientNode, TLClientParameters, TLMasterParameters, TLMasterPortParameters, TLArbiter}
import bus.tilelink.{TLParameters, TLPermissions, ClientMetadata}

case class ICacheParameters(
    nSets: Int = 64,
    nWays: Int = 4,
    rowBits: Int = 64,
    nTLBEntries: Int = 32,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    nSDQ: Int = 17,
    nRPQ: Int = 16,
    nMMIOs: Int = 1,
    blockBytes: Int = 64
)extends L1CacheParameters {

  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = new RandomReplacement(nWays)
}

trait HasICacheParameters extends HasL1CacheParameters {
  val cacheParams = icacheParameters

  // the width of inner CPU data interface
  def cacheID = 0
  // RVC instruction length
  def RVCInsLen = 16
  val groupAlign = log2Up(FetchWidth * 4 * 2)
  def groupPC(pc: UInt): UInt = Cat(pc(PAddrBits-1, groupAlign), 0.U(groupAlign.W))

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
  require(full_divide(rowBits, wordBits), s"rowBits($rowBits) must be multiple of wordBits($wordBits)")
  require(full_divide(beatBits, rowBits), s"beatBits($beatBits) must be multiple of rowBits($rowBits)")
  // this is a VIPT L1 cache
  require(pgIdxBits >= untagBits, s"page aliasing problem: pgIdxBits($pgIdxBits) < untagBits($untagBits)")
}

abstract class ICacheBundle extends XSBundle
  with HasICacheParameters

abstract class ICacheModule(outer: ICache) extends LazyModuleImp(outer)
  with HasICacheParameters 
  with HasXSLog
  with ICacheBase


sealed class ICacheMetaBundle extends ICacheBundle
{
  val tag = UInt(tagBits.W)
}

sealed class ICacheDataBundle extends ICacheBundle
{
  val data = UInt(wordBits.W)
}

class ICacheReq extends ICacheBundle
{
  val addr = UInt(VAddrBits.W)
  val mask = UInt(PredictWidth.W)
}

class ICacheResp extends ICacheBundle
{
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

/* ------------------------------------------------------------
 * The 3-stage pipeline register
 * ------------------------------------------------------------
 */
trait ICacheBase extends HasICacheParameters
{
  //----------------------------
  //    Stage 1
  //----------------------------
  val s1_valid = WireInit(false.B)
  val s1_req_pc = Wire(UInt(VAddrBits.W))
  val s1_req_mask = Wire(UInt(PredictWidth.W))
  val s1_fire = WireInit(false.B)

  //----------------------------
  //    Stage 2
  //----------------------------
  val s2_valid = RegInit(false.B)
  val s2_req_pc = RegEnable(next = s1_req_pc,init = 0.U, enable = s1_fire)
  val s2_req_mask = RegEnable(next = s1_req_mask,init = 0.U, enable = s1_fire)
  val s2_ready = WireInit(false.B)
  val s2_fire = WireInit(false.B)

  //----------------------------
  //    Stage 3
  //----------------------------
  val s3_valid = RegInit(false.B)
  val s3_req_pc = RegEnable(next = s2_req_pc,init = 0.U, enable = s2_fire)
  val s3_req_mask = RegEnable(next = s2_req_mask,init = 0.U, enable = s2_fire)
  val s3_ready = WireInit(false.B)

}

/* ------------------------------------------------------------
 * This module is the Top tilelink module of Icache
 * ------------------------------------------------------------
 */
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
 * The hardware implementation of ICache
 * ------------------------------------------------------------
 */
class ICacheImp(outer: ICache) extends ICacheModule(outer)
{
  // cut a cacheline into a fetch packet
  def cutHelper(sourceVec: Vec[UInt], startPtr: UInt, mask: UInt): UInt = {
    val sourceVec_16bit = Wire(Vec(blockWords * 4,UInt(RVCInsLen.W)))
    (0 until blockWords).foreach{ i =>
      (0 until 4).foreach{ j =>
        sourceVec_16bit(i*4 + j) := sourceVec(i)(j*16+15, j*16)
      }
    }
    val cutPacket = WireInit(VecInit(Seq.fill(blockWords * 2){0.U(RVCInsLen.W)}))
    (0 until blockWords * 2).foreach{ i =>
      cutPacket(i) := Mux(mask(i).asBool,sourceVec_16bit(startPtr + i.U),0.U)
    }
    cutPacket.asUInt
  }

  // generate the one hot code according to a UInt between 0-8
  def PriorityMask(sourceVec: UInt) : UInt = {
    val oneHot = Mux(sourceVec >= 8.U, "b1000".U,
             Mux(sourceVec >= 4.U, "b0100".U,
             Mux(sourceVec >= 2.U, "b0010".U, "b0001".U)))
    oneHot
  }


  val (bus, edge) = outer.clientNode.out.head
  require(bus.d.bits.data.getWidth == l1BusDataWidth, "ICache: tilelink width does not match")
  val io = IO(new ICacheIO(edge))
  val (_, _, refill_done, refill_cnt) = edge.count(bus.d)

  //----------------------------
  //    Memory Part
  //----------------------------
  val metaArray = Module(new SRAMTemplate(new ICacheMetaBundle, set=nSets, way=nWays, shouldReset = true))
  val dataArray = List.fill(blockWords){ Module(new SRAMTemplate(new ICacheDataBundle, set=nSets, way = nWays))}
  // 256-bit valid
  val validArray = RegInit(0.U((nSets * nWays).W)) 

  //----------------------------
  //    Stage 1
  //----------------------------
  s1_valid := io.req.fire()
  s1_req_pc := io.req.bits.addr
  s1_req_mask := io.req.bits.mask
  s2_ready := WireInit(false.B)
  s1_fire := s1_valid && (s2_ready || io.flush(0))
  
  // SRAM(Meta and Data) read request
  val s1_idx = get_idx(s1_req_pc)
  metaArray.io.r.req.valid := s1_valid
  metaArray.io.r.req.bits.apply(setIdx=s1_idx)
  for(b <- 0 until blockWords){
    dataArray(b).io.r.req.valid := s1_valid
    dataArray(b).io.r.req.bits.apply(setIdx=s1_idx)
  }
  XSDebug("[Stage 1] v : r : f  (%d  %d  %d)  request pc: 0x%x  mask: %b\n",s1_valid,s2_ready,s1_fire,s1_req_pc,s1_req_mask)
  XSDebug("[Stage 1] index: %d\n",s1_idx)
  
  
  //----------------------------
  //    Stage 2
  //----------------------------
  val s2_idx = get_idx(s2_req_pc)
  val s2_tlb_resp = WireInit(io.tlb.resp.bits)
  val s2_tag = get_tag(s2_tlb_resp.paddr)
  val s2_hit = WireInit(false.B)
  s2_fire := s2_valid && s3_ready && !io.flush(0) && io.tlb.resp.fire()
  when(io.flush(0)) {s2_valid := s1_fire}
  .elsewhen(s1_fire) { s2_valid := s1_valid}
  .elsewhen(s2_fire) { s2_valid := false.B}

  // SRAM(Meta and Data) read reseponse
  val metas = metaArray.io.r.resp.asTypeOf(Vec(nWays,new ICacheMetaBundle))
  val datas =dataArray.map(b => RegEnable(next=b.io.r.resp.asTypeOf(Vec(nWays,new ICacheDataBundle)), enable=s2_fire))

  val validMeta = Cat((0 until nWays).map{w => validArray(Cat(s2_idx, w.U(2.W)))}.reverse).asUInt

  // hit check and generate victim cacheline mask
  val hitVec = VecInit((0 until nWays).map{w => metas(w).tag === s2_tag && validMeta(w) === 1.U})
  val victimWayMask = (1.U << LFSR64()(log2Up(nWays)-1,0))
  val invalidVec = ~validMeta
  val hasInvalidWay = invalidVec.orR
  val refillInvalidWaymask = PriorityMask(invalidVec)
  
  val waymask = Mux(s2_hit, hitVec.asUInt, Mux(hasInvalidWay, refillInvalidWaymask, victimWayMask))
 
  s2_hit := ParallelOR(hitVec) || s2_tlb_resp.excp.pf.instr
  s2_ready := s2_fire || !s2_valid || io.flush(0)

  XSDebug("[Stage 2] v : r : f  (%d  %d  %d)  pc: 0x%x  mask: %b\n",s2_valid,s3_ready,s2_fire,s2_req_pc,s2_req_mask)
  XSDebug(p"[Stage 2] tlb req:  v ${io.tlb.req.valid} r ${io.tlb.req.ready} ${io.tlb.req.bits}\n")
  XSDebug(p"[Stage 2] tlb resp: v ${io.tlb.resp.valid} r ${io.tlb.resp.ready} ${s2_tlb_resp}\n")
  XSDebug("[Stage 2] tag: %x  hit:%d\n",s2_tag,s2_hit)
  XSDebug("[Stage 2] validMeta: %b  victimWayMaks:%b   invalidVec:%b    hitVec:%b    waymask:%b \n",validMeta,victimWayMask,invalidVec.asUInt,hitVec.asUInt,waymask.asUInt)
  
  
  //----------------------------
  //    Stage 3
  //----------------------------
  val s3_tlb_resp = RegEnable(next = s2_tlb_resp, init = 0.U.asTypeOf(new TlbResp), enable = s2_fire)
  val s3_data = datas
  val s3_tag = RegEnable(s2_tag, s2_fire)
  val s3_hit = RegEnable(next=s2_hit,init=false.B,enable=s2_fire)
  val s3_wayMask = RegEnable(next=waymask,init=0.U,enable=s2_fire)
  val s3_miss = s3_valid && !s3_hit
  when(io.flush(1)) { s3_valid := false.B }
  .elsewhen(s2_fire) { s3_valid := s2_valid }
  .elsewhen(io.resp.fire()) { s3_valid := false.B } 
  val refillDataReg = Reg(Vec(refillCycles,UInt(beatBits.W)))

  // icache hit 
  // simply cut the hit cacheline
  val dataHitWay = s3_data.map(b => Mux1H(s3_wayMask,b).asUInt)
  val outPacket =  Wire(UInt((FetchWidth * 32).W))
  outPacket := cutHelper(VecInit(dataHitWay),s3_req_pc(5,1).asUInt,s3_req_mask.asUInt)

  //icache miss
  val s_idle :: s_memReadReq :: s_memReadResp :: s_wait_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val readBeatCnt = Counter(refillCycles)

  //pipeline flush register
  val needFlush = RegInit(false.B)
  when(io.flush(1) && (state =/= s_idle) && (state =/= s_wait_resp)){ needFlush := true.B }
  .elsewhen((state=== s_wait_resp) && needFlush){ needFlush := false.B }

  //cache flush register
  val icacheFlush = WireInit(false.B)
  val cacheflushed = RegInit(false.B)
  BoringUtils.addSink(icacheFlush, "FenceI")
  XSDebug("[Fence.i] icacheFlush:%d, cacheflushed:%d\n",icacheFlush,cacheflushed)
  when(icacheFlush && (state =/= s_idle) && (state =/= s_wait_resp)){ cacheflushed := true.B}
  .elsewhen((state=== s_wait_resp) && cacheflushed) {cacheflushed := false.B }

  val waitForRefillDone = needFlush || cacheflushed

  // state change to wait for a cacheline refill
  val countFull = readBeatCnt.value === (refillCycles - 1).U
  switch(state){
    is(s_idle){
      when(s3_miss && io.flush === 0.U){
        state := s_memReadReq
        readBeatCnt.value := 0.U
      }
    }


    // memory request
    is(s_memReadReq){ 
      when(bus.a.fire()){ 
        state := s_memReadResp
      }
    }

    is(s_memReadResp){
      when (edge.hasData(bus.d.bits) && bus.d.fire()) {
        readBeatCnt.inc()
	      refillDataReg(readBeatCnt.value) := bus.d.bits.data
        when(countFull){
          assert(refill_done, "refill not done!")
          state := s_wait_resp
        }
      }
    }

    is(s_wait_resp){
      when(io.resp.fire() || needFlush ){state := s_idle}
    }

  }


  //refill write
  val metaWrite = Wire(new ICacheMetaBundle)
  val refillFinalOneBeat = (state === s_memReadResp) && bus.d.fire() && refill_done
  val wayNum = OHToUInt(s3_wayMask.asTypeOf(Vec(nWays,Bool())))
  val validPtr = Cat(get_idx(s3_req_pc),wayNum)
  metaWrite.tag := s3_tag
  metaArray.io.w.req.valid := refillFinalOneBeat
  metaArray.io.w.req.bits.apply(data=metaWrite, setIdx=get_idx(s3_req_pc), waymask=s3_wayMask)

  if(beatBits == 64){
    for(b <- 0 until blockWords){
      val writeOneBeat = (state === s_memReadResp) && bus.d.fire() && (b.U === readBeatCnt.value)
      dataArray(b).io.w.req.valid := writeOneBeat
      dataArray(b).io.w.req.bits.apply(   setIdx=get_idx(s3_req_pc), 
                                          data=bus.d.bits.data.asTypeOf(new ICacheDataBundle), 
                                          waymask=s3_wayMask)

    }
  }
  else{
    val writeFirstHalf = (state === s_memReadResp) && bus.d.fire() && (readBeatCnt.value === 0.U)
    (0 until blockWords/2).foreach{ b =>
      dataArray(b).io.w.req.valid := writeFirstHalf
      dataArray(b).io.w.req.bits.apply( setIdx=get_idx(s3_req_pc),
                                        data=bus.d.bits.data(b * 64 +63, b*64).asTypeOf(new ICacheDataBundle),
                                          waymask=s3_wayMask)

    }
    val writeLastHalf = (state === s_memReadResp) && bus.d.fire() && (readBeatCnt.value === 1.U)
    (blockWords/2 until blockWords).foreach{ b =>
      val index = b - blockWords/2
      dataArray(b).io.w.req.valid := writeLastHalf
      dataArray(b).io.w.req.bits.apply( setIdx=get_idx(s3_req_pc),
                                          data=bus.d.bits.data(index * 64 +63, index*64).asTypeOf(new ICacheDataBundle), 
                                          waymask=s3_wayMask)

    }

 }

  when(refillFinalOneBeat && !cacheflushed){
    validArray := validArray.bitSet(validPtr, true.B)
  }

  //icache flush: only flush valid Array register
  when(icacheFlush){ validArray := 0.U }

  val refillDataVec = refillDataReg.asTypeOf(Vec(blockWords,UInt(wordBits.W)))
  val refillDataOut = cutHelper(refillDataVec, s3_req_pc(5,1),s3_req_mask )

  s3_ready := ((io.resp.fire() || !s3_valid) && !waitForRefillDone) || (waitForRefillDone && state === s_wait_resp)

  //TODO: coherence
  XSDebug("[Stage 3] valid:%d   pc: 0x%x  mask: %b ipf:%d\n",s3_valid,s3_req_pc,s3_req_mask,s3_tlb_resp.excp.pf.instr)
  XSDebug("[Stage 3] hit:%d  miss:%d  waymask:%x \n",s3_hit,s3_miss,s3_wayMask.asUInt)
  XSDebug("[Stage 3] state: %d\n",state)
  XSDebug("[Stage 3] needflush:%d, refilldone:%d\n",needFlush,refill_done)
  XSDebug("[Stage 3] tag: %x    idx: %d\n",s3_tag,get_idx(s3_req_pc))
  XSDebug(p"[Stage 3] tlb resp: ${s3_tlb_resp}\n")
  XSDebug("[Chanel A] valid:%d  ready:%d\n",bus.a.valid,bus.a.ready)
  XSDebug("[Chanel D] valid:%d  ready:%d  data:%x  readBeatcnt:%d \n",bus.d.valid,bus.d.ready,bus.d.bits.data,readBeatCnt.value)
  XSDebug("[Stage 3] ---------Hit Way--------- \n")
  for(i <- 0 until blockWords){
      XSDebug("[Stage 3] %x\n",dataHitWay(i))
  }
  XSDebug("[Stage 3] outPacket :%x\n",outPacket)
  XSDebug("[Stage 3] refillDataOut :%x\n",refillDataOut)

  //----------------------------
  //    Out Put
  //----------------------------
  //icache request
  val dataArrayReadyVec = dataArray.map(b => b.io.r.req.ready)
  io.req.ready := metaArray.io.r.req.ready && ParallelOR(dataArrayReadyVec) && s2_ready
  
  //icache response: to pre-decoder
  io.resp.valid := s3_valid && (s3_hit || state === s_wait_resp)
  io.resp.bits.data := Mux((s3_valid && s3_hit),outPacket,refillDataOut)
  io.resp.bits.mask := s3_req_mask
  io.resp.bits.pc := s3_req_pc
  io.resp.bits.ipf := s3_tlb_resp.excp.pf.instr

  //to itlb
  io.tlb.resp.ready := s3_ready
  io.tlb.req.valid := s2_valid
  io.tlb.req.bits.vaddr := s2_req_pc
  io.tlb.req.bits.cmd := TlbCmd.exec
  io.tlb.req.bits.roqIdx := DontCare
  io.tlb.req.bits.debug.pc := s2_req_pc
  io.tlb.req.bits.debug.lsroqIdx := DontCare
  
  //tilelink
  bus.b.ready := true.B
  bus.c.valid := false.B
  bus.e.valid := false.B
  bus.a.valid := (state === s_memReadReq)
  val memTileReq  = edge.Get(
    fromSource      = cacheID.U,
    toAddress       = groupPC(s3_tlb_resp.paddr),
    lgSize          = (log2Up(cacheParams.blockBytes)).U )._2 
  bus.a.bits :=   memTileReq
  bus.d.ready := true.B

  XSDebug("[flush] flush_0:%d  flush_1:%d\n",io.flush(0),io.flush(1))

  //Performance Counter
  if (!env.FPGAPlatform ) {
    ExcitingUtils.addSource( s3_valid && (state === s_idle), "perfCntIcacheReqCnt", Perf)
    ExcitingUtils.addSource( s3_valid && (state === s_idle) && s3_miss, "perfCntIcacheMissCnt", Perf)
  }
}

