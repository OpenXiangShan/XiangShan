package xiangshan.cache

import chisel3._
import chisel3.util._
import device._
import xiangshan._
import xiangshan.frontend._
import utils._
import chisel3.ExcitingUtils._

case class ICacheParameters(
    nSets: Int = 64,
    nWays: Int = 4,
    rowBits: Int = 64,
    nTLBEntries: Int = 32,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    nSDQ: Int = 17,
    nRPQ: Int = 16,
    nMissEntries: Int = 1,
    nMMIOs: Int = 1,
    blockBytes: Int = 64
)extends L1CacheParameters {

  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = new RandomReplacement(nWays)
}

trait HasICacheParameters extends HasL1CacheParameters with HasIFUConst {
  val cacheParams = icacheParameters
  val groupAlign = log2Up(cacheParams.blockBytes)
  val packetInstNum = packetBytes/instBytes
  val packetInstNumBit = log2Up(packetInstNum)
  val ptrHighBit = log2Up(groupBytes) - 1 
  val ptrLowBit = log2Up(packetBytes)


  def accessBorder =  0x80000000L
  def cacheID = 0
  def insLen = if (HasCExtension) 16 else 32
  def RVCInsLen = 16
  def groupPC(pc: UInt): UInt = Cat(pc(PAddrBits-1, groupAlign), 0.U(groupAlign.W))
  def encRowBits = cacheParams.dataCode.width(rowBits)
  def encTagBits = cacheParams.tagCode.width(tagBits)

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
  require(full_divide(rowBits, wordBits), s"rowBits($rowBits) must be multiple of wordBits($wordBits)")
  require(full_divide(beatBits, rowBits), s"beatBits($beatBits) must be multiple of rowBits($rowBits)")
  // this is a VIPT L1 cache
  require(pgIdxBits >= untagBits, s"page aliasing problem: pgIdxBits($pgIdxBits) < untagBits($untagBits)")
}

abstract class ICacheBundle extends XSBundle
  with HasICacheParameters

abstract class ICacheModule extends XSModule
  with HasICacheParameters
  with ICacheBase

abstract class ICacheArray extends XSModule
  with HasICacheParameters

abstract class ICachArray extends XSModule
  with HasICacheParameters

// sealed class ICacheMetaBundle extends ICacheBundle
// {
//   val tag = UInt(tagBits.W)
// }

// sealed class ICacheDataBundle extends ICacheBundle
// {
//   val data = UInt(encRowBits.W)
// }

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
  val acf = Bool()
}


class ICacheIO extends ICacheBundle
{
  val req = Flipped(DecoupledIO(new ICacheReq))
  val resp = DecoupledIO(new ICacheResp)
  val mem_acquire = DecoupledIO(new L1plusCacheReq)
  val mem_grant   = Flipped(DecoupledIO(new L1plusCacheResp))
  val prefetchTrainReq = ValidIO(new IcacheMissReq)
  val tlb = new BlockTlbRequestIO
  val flush = Input(UInt(2.W))
  val l1plusflush = Output(Bool())
  val fencei = Input(Bool())
  val prev = Flipped(Valid(UInt(16.W)))
  val prev_pc = Input(UInt(VAddrBits.W))
  val prev_ipf = Input(Bool())
  val pd_out = Output(new PreDecodeResp)
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
  // val s1_valid = WireInit(false.B)
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

class ICacheMetaWriteBundle extends ICacheBundle
{
  val virIdx = UInt(idxBits.W)
  val phyTag = UInt(tagBits.W)
  val waymask = UInt(nWays.W)

  def apply(tag:UInt, idx:UInt, waymask:UInt){
    this.virIdx := idx
    this.phyTag := tag
    this.waymask := waymask
  }

}

class ICacheDataWriteBundle extends ICacheBundle
{
  val virIdx = UInt(idxBits.W)
  val data = UInt(blockBits.W)
  val waymask = UInt(nWays.W)

  def apply(data:UInt, idx:UInt, waymask:UInt){
    this.virIdx := idx
    this.data := data
    this.waymask := waymask
  }

}

class ICacheMetaArray extends ICachArray
{
  val io=IO{new Bundle{
    val write = Flipped(DecoupledIO(new ICacheMetaWriteBundle))
    val read  = Flipped(DecoupledIO(UInt(idxBits.W)))
    val readResp = Output(Vec(nWays,UInt(tagBits.W)))
  }}

  val metaArray = Module(new SRAMTemplate(UInt(encTagBits.W), set=nSets, way=nWays, shouldReset = true))

  //read
  metaArray.io.r.req.valid := io.read.valid
  io.read.ready := metaArray.io.r.req.ready
  io.write.ready := DontCare
  metaArray.io.r.req.bits.apply(setIdx=io.read.bits)

  val rtag = metaArray.io.r.resp.asTypeOf(Vec(nWays,UInt(encTagBits.W)))
  val tag_encoded = VecInit(rtag.map(wtag => cacheParams.tagCode.decode(wtag).corrected))
  io.readResp :=tag_encoded.asTypeOf(Vec(nWays,UInt(tagBits.W)))
  //write
  val write = io.write.bits
  val wdata_encoded = cacheParams.tagCode.encode(write.phyTag.asUInt)
  metaArray.io.w.req.valid := io.write.valid
  metaArray.io.w.req.bits.apply(data=wdata_encoded, setIdx=write.virIdx, waymask=write.waymask)


}

class ICacheDataArray extends ICachArray
{
  val io=IO{new Bundle{
    val write = Flipped(DecoupledIO(new ICacheDataWriteBundle))
    val read  = Flipped(DecoupledIO(UInt(idxBits.W)))
    val readResp = Output(Vec(blockWords,Vec(nWays,UInt(encRowBits.W))))
  }}

  val dataArray = List.fill(blockWords){ Module(new SRAMTemplate(UInt(encRowBits.W), set=nSets, way = nWays))}

  //read
  //do ECC decoding after way choose
  for(b <- 0 until blockWords){
    dataArray(b).io.r.req.valid := io.read.valid
    dataArray(b).io.r.req.bits.apply(setIdx=io.read.bits)
  }
  val dataArrayReadyVec = dataArray.map(b => b.io.r.req.ready)

  io.read.ready := ParallelOR(dataArrayReadyVec)
  io.write.ready := DontCare
  io.readResp := VecInit(dataArray.map(b => b.io.r.resp.asTypeOf(Vec(nWays,UInt(encRowBits.W)))))

  //write
  val write = io.write.bits
  val write_data = write.data.asTypeOf(Vec(blockWords,UInt(rowBits.W)))
  val write_data_encoded = write_data.map(wdata => cacheParams.tagCode.encode(wdata))

  for(b <- 0 until blockWords){
    dataArray(b).io.w.req.valid := io.write.valid
    dataArray(b).io.w.req.bits.apply(   setIdx=write.virIdx,
                                        data=write_data_encoded(b),
                                        waymask=write.waymask)

  }

}

/* ------------------------------------------------------------
 * This module is a SRAM with 4-way associated mapping
 * The hardware implementation of ICache
 * ------------------------------------------------------------
 */
class ICache extends ICacheModule
{
  // cut a cacheline into a fetch packet
  def cutHelper(sourceVec: Vec[UInt], pc: UInt, mask: UInt): UInt = {
    val sourceVec_inst = Wire(Vec(blockWords*wordBytes/instBytes,UInt(insLen.W)))
    (0 until blockWords).foreach{ i =>
      (0 until wordBytes/instBytes).foreach{ j =>
        sourceVec_inst(i*wordBytes/instBytes + j) := sourceVec(i)(j*insLen+insLen-1, j*insLen)
      }
    }
    val cutPacket = WireInit(VecInit(Seq.fill(PredictWidth){0.U(insLen.W)}))
    val start = Cat(pc(ptrHighBit,ptrLowBit),0.U(packetInstNumBit.W))
    (0 until PredictWidth ).foreach{ i =>
      cutPacket(i) := Mux(mask(i).asBool,sourceVec_inst(start + i.U),0.U)
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

  val io = IO(new ICacheIO)

  val s2_flush = io.flush(0)
  val s3_flush = io.flush(1)
  //----------------------------
  //    Memory Part
  //----------------------------
  val metaArray = Module(new ICacheMetaArray)
  val dataArray = Module(new ICacheDataArray)
  // 256-bit valid
  val validArray = RegInit(0.U((nSets * nWays).W))

  //----------------------------
  //    Stage 1
  //----------------------------
  s1_fire := io.req.valid
  s1_req_pc := io.req.bits.addr
  s1_req_mask := io.req.bits.mask
  s2_ready := WireInit(false.B)
  // s1_fire := s1_valid && (s2_ready || s2_flush)

  // SRAM(Meta and Data) read request
  val s1_idx = get_idx(s1_req_pc)

  metaArray.io.read.valid := s1_fire
  metaArray.io.read.bits  :=s1_idx
  dataArray.io.read.valid := s1_fire
  dataArray.io.read.bits  :=s1_idx

  XSDebug("[Stage 1] r : f  (%d  %d)  request pc: 0x%x  mask: %b\n",s2_ready,s1_fire,s1_req_pc,s1_req_mask)
  XSDebug("[Stage 1] index: %d\n",s1_idx)


  //----------------------------
  //    Stage 2
  //----------------------------
  val s2_idx = get_idx(s2_req_pc)
  val s2_tlb_resp = WireInit(io.tlb.resp.bits)
  val s2_tag = get_tag(s2_tlb_resp.paddr)
  val s2_hit = WireInit(false.B)
  val s2_access_fault = WireInit(false.B)
  val s2_allValid = s2_valid && io.tlb.resp.valid
  s2_fire := s2_allValid && s3_ready
  when(s1_fire)       { s2_valid := true.B }
  .elsewhen(s2_flush) { s2_valid := false.B }
  .elsewhen(s2_fire)  { s2_valid := false.B }

  //physical address < 0x80000000
  //TODO: May have bugs
  s2_access_fault := (s2_tlb_resp.paddr < accessBorder.U) && s2_valid

  // SRAM(Meta and Data) read reseponse
  val metas = metaArray.io.readResp
  val datas =RegEnable(next=dataArray.io.readResp, enable=s2_fire)

  val validMeta = Cat((0 until nWays).map{w => validArray(Cat(s2_idx, w.U(log2Ceil(nWays).W)))}.reverse).asUInt

  // hit check and generate victim cacheline mask
  val hitVec = VecInit((0 until nWays).map{w => metas(w)=== s2_tag && validMeta(w) === 1.U})
  val victimWayMask = (1.U << LFSR64()(log2Up(nWays)-1,0))
  val invalidVec = ~validMeta
  val hasInvalidWay = invalidVec.orR
  val refillInvalidWaymask = PriorityMask(invalidVec)

  val waymask = Mux(s2_hit, hitVec.asUInt, Mux(hasInvalidWay, refillInvalidWaymask, victimWayMask))

  s2_hit := ParallelOR(hitVec) || s2_tlb_resp.excp.pf.instr || s2_access_fault
  s2_ready := s3_ready || !s2_valid

  XSDebug("[Stage 2] v : r : f  (%d  %d  %d)  pc: 0x%x  mask: %b acf:%d\n",s2_valid,s3_ready,s2_fire,s2_req_pc,s2_req_mask,s2_access_fault)
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
  val s3_idx = get_idx(s3_req_pc)
  val s3_access_fault = RegEnable(s2_access_fault,init=false.B,enable=s2_fire)
  when(s3_flush)                  { s3_valid := false.B }
  .elsewhen(s2_fire && !s2_flush) { s3_valid := true.B }
  .elsewhen(io.resp.fire())       { s3_valid := false.B }
  val refillDataReg = Reg(Vec(refillCycles,UInt(beatBits.W)))

  // icache hit
  // data ECC encoding
  // simply cut the hit cacheline
  val dataHitWay = VecInit(s3_data.map(b => Mux1H(s3_wayMask,b).asUInt))
  val outPacket =  Wire(UInt((FetchWidth * 32).W))
  val dataHitWayDecoded = VecInit(
    (0 until blockWords).map{r =>
      val row = dataHitWay.asTypeOf(Vec(blockWords,UInt(encRowBits.W)))(r)
      val decodedRow = cacheParams.dataCode.decode(row)
      assert(!(s3_valid && s3_hit && decodedRow.uncorrectable))
      decodedRow.corrected
    }
  )
  outPacket := cutHelper(dataHitWay,s3_req_pc.asUInt,s3_req_mask.asUInt)



  //ICache MissQueue
  val icacheMissQueue = Module(new IcacheMissQueue)
  val blocking = RegInit(false.B)
  val isICacheResp = icacheMissQueue.io.resp.valid && icacheMissQueue.io.resp.bits.clientID === cacheID.U(2.W)
  icacheMissQueue.io.req.valid := s3_miss && !s3_flush && !blocking//TODO: specificate flush condition
  icacheMissQueue.io.req.bits.apply(missAddr=groupPC(s3_tlb_resp.paddr),missIdx=s3_idx,missWaymask=s3_wayMask,source=cacheID.U(2.W))
  icacheMissQueue.io.resp.ready := io.resp.ready
  icacheMissQueue.io.flush := s3_flush

  when(icacheMissQueue.io.req.fire()){blocking := true.B}
  .elsewhen(blocking && ((icacheMissQueue.io.resp.fire() && isICacheResp) || s3_flush) ){blocking := false.B}

  XSDebug(blocking && s3_flush,"check for icache non-blocking")
  //cache flush register
  val icacheFlush = io.fencei
  val cacheflushed = RegInit(false.B)
  XSDebug("[Fence.i] icacheFlush:%d, cacheflushed:%d\n",icacheFlush,cacheflushed)
  when(icacheFlush && blocking && !isICacheResp){ cacheflushed := true.B}
  .elsewhen(isICacheResp && cacheflushed) {cacheflushed := false.B }

  //TODO: Prefetcher

  //refill write
  val metaWriteReq = icacheMissQueue.io.meta_write.bits
  icacheMissQueue.io.meta_write.ready := true.B
  metaArray.io.write.valid := icacheMissQueue.io.meta_write.valid
  metaArray.io.write.bits.apply(tag=metaWriteReq.meta_write_tag,
                                idx=metaWriteReq.meta_write_idx,
                                waymask=metaWriteReq.meta_write_waymask)

  val wayNum = OHToUInt(metaWriteReq.meta_write_waymask.asTypeOf(Vec(nWays,Bool())))
  val validPtr = Cat(metaWriteReq.meta_write_idx,wayNum)
  when(icacheMissQueue.io.meta_write.valid && !cacheflushed){
    validArray := validArray.bitSet(validPtr, true.B)
  }

  //data
  icacheMissQueue.io.refill.ready := true.B
  val refillReq = icacheMissQueue.io.refill.bits
  dataArray.io.write.valid := icacheMissQueue.io.refill.valid
  dataArray.io.write.bits.apply(data=refillReq.refill_data,
                                idx=refillReq.refill_idx,
                                waymask=refillReq.refill_waymask)

  //icache flush: only flush valid Array register
  when(icacheFlush){ validArray := 0.U }

  val refillDataVec = icacheMissQueue.io.resp.bits.data.asTypeOf(Vec(blockWords,UInt(wordBits.W)))
  val refillDataOut = cutHelper(refillDataVec, s3_req_pc,s3_req_mask )

  s3_ready := ((io.resp.ready && s3_hit || !s3_valid) && !blocking) || (blocking && icacheMissQueue.io.resp.valid && io.resp.ready)


  val pds = Seq.fill(nWays)(Module(new PreDecode))
  for (i <- 0 until nWays) {
    val wayResp = Wire(new ICacheResp)
    val wayData = cutHelper(VecInit(s3_data.map(b => b(i).asUInt)), s3_req_pc, s3_req_mask)
    val refillData = cutHelper(refillDataVec, s3_req_pc,s3_req_mask)
    wayResp.pc := s3_req_pc
    wayResp.data := Mux(s3_valid && s3_hit, wayData, refillData)
    wayResp.mask := s3_req_mask
    wayResp.ipf := s3_tlb_resp.excp.pf.instr
    wayResp.acf := s3_access_fault
    pds(i).io.in := wayResp
    pds(i).io.prev <> io.prev
    pds(i).io.prev_pc := io.prev_pc
    // if a fetch packet triggers page fault, set the pf instruction to nop
    when ((!(HasCExtension.B) || io.prev.valid) && s3_tlb_resp.excp.pf.instr ) {
      val instrs = Wire(Vec(FetchWidth, UInt(32.W)))
      (0 until FetchWidth).foreach(i => instrs(i) := ZeroExt("b0010011".U, 32)) // nop
      pds(i).io.in.data := instrs.asUInt
    }.elsewhen (HasCExtension.B && io.prev.valid && (io.prev_ipf || s3_tlb_resp.excp.pf.instr)) {
      pds(i).io.prev.bits := ZeroExt("b0010011".U, 16)
      val instrs = Wire(Vec(FetchWidth, UInt(32.W)))
      (0 until FetchWidth).foreach(i => instrs(i) := Cat(ZeroExt("b0010011".U, 16), Fill(16, 0.U(1.W))))
      pds(i).io.in.data := instrs.asUInt
    }
  }
  io.pd_out := Mux1H(s3_wayMask, pds.map(_.io.out))

  //TODO: coherence
  XSDebug("[Stage 3] valid:%d   pc: 0x%x  mask: %b ipf:%d acf:%d \n",s3_valid,s3_req_pc,s3_req_mask,s3_tlb_resp.excp.pf.instr,s3_access_fault)
  XSDebug("[Stage 3] hit:%d  miss:%d  waymask:%x blocking:%d\n",s3_hit,s3_miss,s3_wayMask.asUInt,blocking)
  XSDebug("[Stage 3] tag: %x    idx: %d\n",s3_tag,get_idx(s3_req_pc))
  XSDebug(p"[Stage 3] tlb resp: ${s3_tlb_resp}\n")
  XSDebug("[mem_acquire] valid:%d  ready:%d\n",io.mem_acquire.valid,io.mem_acquire.ready)
  XSDebug("[mem_grant] valid:%d  ready:%d  data:%x id:%d \n",io.mem_grant.valid,io.mem_grant.ready,io.mem_grant.bits.data,io.mem_grant.bits.id)
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
  io.req.ready := s2_ready

  //icache response: to pre-decoder
  io.resp.valid := s3_valid && (s3_hit || icacheMissQueue.io.resp.valid)
  io.resp.bits.data := Mux((s3_valid && s3_hit),outPacket,refillDataOut)
  io.resp.bits.mask := s3_req_mask
  io.resp.bits.pc := s3_req_pc
  io.resp.bits.ipf := s3_tlb_resp.excp.pf.instr
  io.resp.bits.acf := s3_access_fault

  //to itlb
  io.tlb.resp.ready := true.B // DontCare
  io.tlb.req.valid := s2_valid
  io.tlb.req.bits.vaddr := s2_req_pc
  io.tlb.req.bits.cmd := TlbCmd.exec
  io.tlb.req.bits.roqIdx := DontCare
  io.tlb.req.bits.debug.pc := s2_req_pc

  //To L1 plus
  io.mem_acquire <> icacheMissQueue.io.mem_acquire
  icacheMissQueue.io.mem_grant <> io.mem_grant
  
  // to train l1plus prefetcher
  io.prefetchTrainReq.valid := s3_valid && icacheMissQueue.io.req.fire()
  io.prefetchTrainReq.bits := DontCare
  io.prefetchTrainReq.bits.addr := groupPC(s3_tlb_resp.paddr)

  io.l1plusflush := icacheFlush

  XSDebug("[flush] flush_0:%d  flush_1:%d\n",s2_flush,s3_flush)

  //Performance Counter
  if (!env.FPGAPlatform ) {
    ExcitingUtils.addSource( s3_valid && !blocking, "perfCntIcacheReqCnt", Perf)
    ExcitingUtils.addSource( s3_miss && blocking && io.resp.fire(), "perfCntIcacheMissCnt", Perf)
  }
}

