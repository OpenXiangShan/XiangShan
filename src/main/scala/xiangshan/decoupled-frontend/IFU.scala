package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class IfuToFtqIO(implicit p:Parameters) extends XSBundle {
  val pdWb = Valid(new PredecodeWritebackBundle)
}

class FtqInterface(implicit p: Parameters) extends XSBundle {
  val fromFtq = Flipped(Decoupled(new FtqToFetch))
  val toFtq   = Valid(new FetchToFtq)
}

class ICacheInterface(implicit p: Parameters) extends XSBundle {
  val toIMeta       = Decoupled(new ICacheReadBundle)
  val toIData       = Decoupled(new ICacheReadBundle)
  val toMissQueue   = Vec(2,Decoupled(new ICacheMissReq))
  val fromIMeta     = Input(new ICacheMetaRespBundle)
  val fromIData     = Input(new ICacheDataRespBundle)
  val fromMissQueue = Flipped(Decoupled(ICacheMissResp))
}

class NewIFUIO(implicit p: Parameters) extends XSBundle {
  val ftqInter        = new FtqInterface  
  val icacheInter     = new ICacheInterface 
  val toIbuffer       = Decoupled(new FetchToIBuffer)
  val iTLBInter       = new BlockTlbRequestIO  
}

@chiselName
class NewIFU(implicit p: Parameters) extends XSModule
{
  val io = IO(new IFUIO)
  val (toFtq, fromFtq)    = (io.ftqInter.toFtq, io.ftqInter.fromFtq)
  val (toMeta, toData, meta_resp, data_resp) =  (io.icacheInter.toIMeta, io.icacheInter.toIData, io.icacheInter.fromIMeta, io.icacheInter.fromIData)
  val (toMissQueue, fromMissQueue) = (io.icacheInter.toMissQueue, io.icacheInter.fromMissQueue)
  val (toITLB, fromITLB) = (io.iTLBInter.req, io.iTLBInter.resp)
  
  def isDoubleLine(startAddr: UInt, endOffset: UInt): Bool = {
  
  }

  def getIdx(vaddr: UInt): UInt = {

  }

  def getTag(pAddr: UInt): UInt ={

  }

  val (f0_valid, f1_ready)                 = (fromFtq.valid, WireInit(false.B))
  val f0_fire                              = f0_valid && f1_fire 
  val f0_ftqIdx                            = fromFtq.bits.ftqIdx 
  val f0_startAddr                         = fromtFtq.bits.startAddr
  val f0_isDoubleLine                      = isDoubleLine(fromFtq.bits.startAddr, fromFtq.bits.endOffset)
  val f0_vSetIdx                           = VecInit(Seq(getIdx(fromFtq.bits.startAddr),getIdx(fromtFtq.bits.startAddr + endOffset)))


  //fetch: send addr to Meta/TLB and Data simultaneously
  val fetch_req = Seq(toMeta, toData)
  fetch_req.map(channel => 
    channel.valid               := f0_valid 
    channel.bits.isDoubleLine   := f0_isDoubleLine 
    channel.bits.vSetIdx        := f0_vSetIdx 
  )

  //TODO: tlb req 
  io.iTLBInter.req <> DontCare

  //hit check
  val tlbRespValid = io.iTLBInter.resp.valid 
  val (tlbMiss, tlbHit, pAddr) = 
  val exception =
  
 val f1_valid      = RegInit(false.B)
  val f1_ftqIdx     = RegEnable(next = f0_ftqIdx, enable=f0_fire)
  val f1_vSetIdx    = RegEnable(next = f0_vSetIdx,enable=f0_fire)
  val f1_ready      = WireInit(false.B)
  val f1_fire       = f1_valid && tlbHit && f2_ready 
  
  when(f0_fire)      {f1_valid  := true.B} 
  .elsewhen(f1_fire) {f1_valid  := false.B}

  val f1_pAddrs             = VecInit(fromITLB.bits.pAddr)   //Vec(2,UInt(pAddrBits.W))
  val f1_pTags              = VecInit(f1_pAddrs.map{pAddr => getTag(pAddr)})
  val (f1_tags, f1_cacheline_valid)   = (meta_resp.tags, meta_resp.valid) 
  val bank0_hit_vec         = VecInit(Cat(f1_tags(0).map(way_tag => f1_cacheline_valid(0)  way_tag ===  f1_pTags(0))).reverse)
  val bank1_hit_vec         = VecInit(Cat(f1_tags(1).map(way_tag => f1_cacheline_valid(1)  way_tag ===  f1_pTags(1))).reverse)
  val (bank0_hit,bank1_hit) = (ParallelAND(bank0_hit_vec), ParallelAND(bank1_hit_vec)) 
  val f1_hit                = bank0_hit && bank1_hit && f1_valid 
  
  val replacers       = Seq.fill(2)(ReplacementPolicy.fromString(Some("random"),nWays,nSets/2))
  val f1_victim_masks = replacers.map{replacer => UIntToOH(replacer.way())}

  val touch_sets = Seq.fill(2)(Wire(Vec(plruAccessNum, UInt(log2Ceil(nSets/2).W))))
  val touch_ways = Seq.fill(2)(Wire(Vec(plruAccessNum, Valid(UInt(log2Ceil(nWays).W)))) )
  
  touch_sets(0)(0)       := f1_vSetIdxi(0) 
  touch_ways(0)(0).valid := bank0_hit 
  touch_ways(0)(0).bits  := OHToUInt(bank0_hit_vec) 
  touch_sets(1)(0)       := f1_vSetIdx(1) 
  touch_ways(1)(0).valid := bank1_hit 
  touch_ways(1)(0).bits  := OHToUInt(bank1_hit_vec) 
   
  ((replacer zip touch_sets) zip touch_ways).map{case ((r, s),w) => r.access(s,w)}
  
  f1_ready := f2_ready || !f2_valid 

  //fetch response
  val f2_valid    = RegInit(false.B)
  val f2_ftqIdx   = RegEnable(next = f1_ftqIdx, enable = f1_fire)
  val f2_fire     = io.toIbuffer.fire()
  when(f1_fire)                   {f2_valid := true.B}
  .elsewhen(io.toIbuffer.fire())  {f2_valid := false.B}

  val f2_pAddrs   = RegEnable(next = f1_pAddrs, enable = f1_fire)
  val f2_hit      = RegEnable(next = f1_hit   , enable = f1_fire)
  val f2_bank_hit = RegEnable(next = VecInit(bank0_hit, bank1_hit), enable = f1_fire)
  val f2_miss     = f2_valid && !f2_hit 
  val (f2_vSetIdx, f2_pTags) = (RegEnable(next = f1_vSetIdx, enable = f1_fire), RegEnable(next = f1_pTags, enable = f1_fire))
  val f2_waymask  = RegEnable(next = f1_victim_masks, enable = f1_fire)

  toMissQueue.zipWithIndex.map{case (p, i) =>
    p.valid         := f2_valid && !f2_bank_hit(i)
    p.bits.addr     := f2_pAddrs(i)
    p.bits.vSetIdx  := f2_vSetIdx(i)
    p.bits.waymask  := f2_waymask(2)
  } 

  when(f1_fire){
    f2_valid := true.B
  } .elsewhen(f2_fire) {
    f2_valid := false.B
  }

  f2_ready := io.toIbuffer.ready
  
}


