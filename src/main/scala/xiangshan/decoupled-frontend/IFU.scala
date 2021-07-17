package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import utils._

trait HasInstrMMIOConst extends HasXSParameter with HasIFUConst{
  def mmioBusWidth = 64
  def mmioBusBytes = mmioBusWidth /8
  def mmioBeats = FetchWidth * 4 * 8 / mmioBusWidth
  def mmioMask  = VecInit(List.fill(PredictWidth)(true.B)).asUInt
  def mmioBusAligned(pc :UInt): UInt = align(pc, mmioBusBytes)
}

trait HasIFUConst extends HasXSParameter {
  val resetVector = 0x80000000L//TODO: set reset vec
  def align(pc: UInt, bytes: Int): UInt = Cat(pc(VAddrBits-1, log2Ceil(bytes)), 0.U(log2Ceil(bytes).W))
  val groupBytes = 64 // correspond to cache line size
  val groupOffsetBits = log2Ceil(groupBytes)
  val groupWidth = groupBytes / instBytes
  val packetBytes = PredictWidth * instBytes
  val packetOffsetBits = log2Ceil(packetBytes)
  def offsetInPacket(pc: UInt) = pc(packetOffsetBits-1, instOffsetBits)
  def packetIdx(pc: UInt) = pc(VAddrBits-1, log2Ceil(packetBytes))
  def groupAligned(pc: UInt)  = align(pc, groupBytes)
  def packetAligned(pc: UInt) = align(pc, packetBytes)
  def mask(pc: UInt): UInt = ((~(0.U(PredictWidth.W))) << offsetInPacket(pc))(PredictWidth-1,0)
  def snpc(pc: UInt): UInt = packetAligned(pc) + packetBytes.U

  val enableGhistRepair = true
  val IFUDebug = true
}
class IfuToFtqIO(implicit p:Parameters) extends XSBundle {
  val pdWb = Valid(new PredecodeWritebackBundle)
}

class FtqInterface(implicit p: Parameters) extends XSBundle {
  val fromFtq = Flipped(new FtqToIfuIO)
  val toFtq   = new IfuToFtqIO 
}

class ICacheInterface(implicit p: Parameters) extends XSBundle {
  val toIMeta       = Decoupled(new ICacheReadBundle)
  val toIData       = Decoupled(new ICacheReadBundle)
  val toMissQueue   = Vec(2,Decoupled(new ICacheMissReq))
  val fromIMeta     = Input(new ICacheMetaRespBundle)
  val fromIData     = Input(new ICacheDataRespBundle)
  val fromMissQueue = Vec(2,Flipped(Decoupled(new ICacheMissResp)))
}

class NewIFUIO(implicit p: Parameters) extends XSBundle {
  val ftqInter        = new FtqInterface  
  val icacheInter     = new ICacheInterface 
  val toIbuffer       = Decoupled(new FetchToIBuffer)
  val iTLBInter       = new BlockTlbRequestIO  
}

class IfuToPreDecode(implicit p: Parameters) extends XSBundle {
  val data          = Vec(17, UInt(16.W))   //34Bytes 
  val startAddr     = UInt(VAddrBits.W)
  val ftqOffset     = Valid(UInt(log2Ceil(32).W))
  val target        = UInt(VAddrBits.W)
}

class NewIFU(implicit p: Parameters) extends XSModule with Temperary with HasICacheParameters
{
  val io = IO(new NewIFUIO)
  val (toFtq, fromFtq)    = (io.ftqInter.toFtq, io.ftqInter.fromFtq)
  val (toMeta, toData, meta_resp, data_resp) =  (io.icacheInter.toIMeta, io.icacheInter.toIData, io.icacheInter.fromIMeta, io.icacheInter.fromIData)
  val (toMissQueue, fromMissQueue) = (io.icacheInter.toMissQueue, io.icacheInter.fromMissQueue)
  val (toITLB, fromITLB) = (io.iTLBInter.req, io.iTLBInter.resp)
  
  def isCrossLineReq(start: UInt, end: UInt): Bool = start(offBits) ^ end(offBits)

  def isLastInCacheline(fallThruAddr: UInt): Bool = fallThruAddr(offBits - 1, 1) === 0.U

  def getIdx(vaddr: UInt): UInt = vaddr(tagBits - 1, idxBits)

  def getTag(pAddr: UInt): UInt = pAddr(PAddrBits - 1, tagBits)


  //---------------------------------------------
  //  Fetch Stage 1 :
  //  * Send req to ICache Meta/Data
  //  * Check whether need 2 line fetch
  //  * Send req to ITLB
  //---------------------------------------------

  val (f0_valid, f1_ready)                 = (fromFtq.req.valid, WireInit(false.B))
  val f0_ftq_req                           = fromFtq.req.bits
  val f0_situation                         = VecInit(Seq(isCrossLineReq(f0_ftq_req.startAddr, f0_ftq_req.fallThruAddr), isLastInCacheline(f0_ftq_req.fallThruAddr)))
  val f0_doubleLine                        = f0_situation(0) || f0_situation(1)
  val f0_vSetIdx                           = VecInit(getIdx((f0_ftq_req.startAddr)), getIdx(f0_ftq_req.fallThruAddr))
  val f0_fire                              = fromFtq.req.fire()

  //fetch: send addr to Meta/TLB and Data simultaneously
  val fetch_req = List(toMeta, toData)
  for(i <- 0 until 2) {
    fetch_req(i).valid := f0_valid
    fetch_req(i).bits.isDoubleLine := f0_doubleLine
    fetch_req(i).bits.vSetIdx := f0_vSetIdx
  }

  fromFtq.req.ready := fetch_req(0).ready && fetch_req(1).ready && f1_ready && GTimer() > 500.U

  //TODO: tlb req
  io.iTLBInter.req <> DontCare
  io.iTLBInter.resp.ready := true.B

  //---------------------------------------------
  //  Fetch Stage 2 :
  //  * TLB Response (Get Paddr)
  //  * ICache Response (Get Meta and Data)
  //  * Hit Check (Generate hit signal and hit vector)
  //  * Get victim way
  //---------------------------------------------
  val tlbRespValid = io.iTLBInter.resp.valid 
  val tlbMiss      = WireInit(false.B)
  val tlbHit       = WireInit(true.B)        //TODO: Temporary assignment
  //TODO: handle fetch exceptions

  val f2_ready = WireInit(false.B)

  val f1_valid      = RegInit(false.B)
  val f1_ftq_req    = RegEnable(next = f0_ftq_req,    enable=f0_fire)
  val f1_situation  = RegEnable(next = f0_situation,  enable=f0_fire)
  val f1_doubleLine = RegEnable(next = f0_doubleLine, enable=f0_fire)
  val f1_vSetIdx    = RegEnable(next = f0_vSetIdx,    enable=f0_fire)
  val f1_fire       = f1_valid && tlbHit && f2_ready

  val preDecoder      = Module(new PreDecode)
  val (preDecoderIn, preDecoderOut)   = (preDecoder.io.in, preDecoder.io.out)

  //flush generate and to Ftq
  val predecodeOutValid = WireInit(false.B)
  val flush = (preDecoderOut.misOffset.valid && predecodeOutValid) || fromFtq.redirect.valid

  when(flush)        {f1_valid  := false.B}
  .elsewhen(f0_fire) {f1_valid  := true.B}
  .elsewhen(f1_fire) {f1_valid  := false.B}

  val f1_pAddrs             = VecInit(Seq(Cat(0.U(1.W), f1_ftq_req.startAddr), Cat(0.U(1.W), f1_ftq_req.fallThruAddr)))   //TODO: Temporary assignment
  val f1_pTags              = VecInit(f1_pAddrs.map{pAddr => getTag(pAddr)})
  val (f1_tags, f1_cacheline_valid, f1_datas)   = (meta_resp.tags, meta_resp.valid, data_resp.datas)
  val bank0_hit_vec         = VecInit(f1_tags(0).zipWithIndex.map{ case(way_tag,i) => f1_cacheline_valid(0)(i) && way_tag ===  f1_pTags(0) })
  val bank1_hit_vec         = VecInit(f1_tags(1).zipWithIndex.map{ case(way_tag,i) => f1_cacheline_valid(1)(i) && way_tag ===  f1_pTags(1) })
  val (bank0_hit,bank1_hit) = (ParallelAND(bank0_hit_vec), ParallelAND(bank1_hit_vec)) 
  val f1_hit                = bank0_hit && bank1_hit && f1_valid 
  val f1_bank_hit_vec       = VecInit(Seq(bank0_hit_vec, bank1_hit_vec))
  val f1_bank_hit           = VecInit(Seq(bank0_hit, bank1_hit))
  
  val replacers       = Seq.fill(2)(ReplacementPolicy.fromString(Some("random"),nWays,nSets/2))
  val f1_victim_masks = VecInit(replacers.zipWithIndex.map{case (replacer, i) => UIntToOH(replacer.way(f1_vSetIdx(i)))})

  val touch_sets = Seq.fill(2)(Wire(Vec(plruAccessNum, UInt(log2Ceil(nSets/2).W))))
  val touch_ways = Seq.fill(2)(Wire(Vec(plruAccessNum, Valid(UInt(log2Ceil(nWays).W)))) )
   
  ((replacers zip touch_sets) zip touch_ways).map{case ((r, s),w) => r.access(s,w)}
  
  val f1_hit_data      =  VecInit(f1_datas.zipWithIndex.map { case(bank, i) =>
    val bank_hit_data = Mux1H(f1_bank_hit_vec(i).asUInt, bank)
    bank_hit_data
  })

  //---------------------------------------------
  //  Fetch Stage 3 :
  //  * get data from last stage (hit from f1_hit_data/miss from missQueue response)
  //  * if at least one needed cacheline miss, wait for miss queue response (a wait_state machine) THIS IS TOO UGLY!!!
  //  * cut cacheline(s) and send to PreDecode
  //  * check if prediction is right (branch target and type, jump direction and type , jal target )
  //---------------------------------------------
  val f2_valid      = RegInit(false.B)
  val f2_ftq_req    = RegEnable(next = f1_ftq_req, enable = f1_fire)
  val f2_situation  = RegEnable(next = f1_situation, enable=f1_fire)
  val f2_doubleLine = RegEnable(next = f1_doubleLine, enable=f1_fire)
  val f2_fire       = io.toIbuffer.fire()

  f1_ready := f2_ready || !f1_valid

  when(flush)                     {f2_valid := false.B}
  .elsewhen(f1_fire)              {f2_valid := true.B }
  .elsewhen(io.toIbuffer.fire())  {f2_valid := false.B}

  val f2_pAddrs   = RegEnable(next = f1_pAddrs, enable = f1_fire)
  val f2_hit      = RegEnable(next = f1_hit   , enable = f1_fire)
  val f2_bank_hit = RegEnable(next = VecInit(bank0_hit, bank1_hit), enable = f1_fire)
  val f2_miss     = f2_valid && !f2_hit 
  val (f2_vSetIdx, f2_pTags) = (RegEnable(next = f1_vSetIdx, enable = f1_fire), RegEnable(next = f1_pTags, enable = f1_fire))
  val f2_waymask  = RegEnable(next = f1_victim_masks, enable = f1_fire)

  //instruction 
  val wait_idle :: wait_send_req  :: wait_two_resp :: wait_0_resp :: wait_1_resp :: wait_one_resp ::wait_finish :: Nil = Enum(7)
  val wait_state = RegInit(wait_idle)

  fromMissQueue.map{port => port.ready := true.B}

  val (miss0_resp, miss1_resp) = (fromMissQueue(0).fire(), fromMissQueue(1).fire())
  val (bank0_fix, bank1_fix)   = (miss0_resp  && !f2_bank_hit(0), miss1_resp && f2_doubleLine && !f2_bank_hit(1))

  val  only_0 = f2_valid && !f2_hit && !f2_doubleLine 
  val (hit_0_miss_1 ,  miss_0_hit_1,  miss_0_miss_1) = (  (f2_valid && !f2_bank_hit(1) && f2_bank_hit(0) && f2_doubleLine),
                                                          (f2_valid && !f2_bank_hit(0) && f2_bank_hit(1) && f2_doubleLine),
                                                          (f2_valid && !f2_bank_hit(0) && !f2_bank_hit(1) && f2_doubleLine),
                                                       )

  toMissQueue <> DontCare
  switch(wait_state){
    is(wait_idle){
      when(f2_valid && !f2_hit){ 
        (0 until 2).map { i =>
          if(i == 1) toMissQueue(i).valid := hit_0_miss_1 || miss_0_miss_1
            else     toMissQueue(i).valid := only_0 || miss_0_hit_1 || miss_0_miss_1
          toMissQueue(i).bits.addr    := f2_pAddrs(i)
          toMissQueue(i).bits.vSetIdx := f2_vSetIdx(i)
          toMissQueue(i).bits.waymask := f2_waymask(i)
        }
        when( only_0  || miss_0_hit_1){
          wait_state :=  Mux(toMissQueue(0).fire(), wait_send_req ,wait_idle )
        }.elsewhen(hit_0_miss_1){
          wait_state :=  Mux(toMissQueue(1).fire(), wait_send_req ,wait_idle )
        }.elsewhen( miss_0_miss_1 ){
            wait_state := Mux(toMissQueue(0).fire() && toMissQueue(1).fire(), wait_send_req ,wait_idle)
        }
      }
    }

    //TODO: naive logic for wait icache response

    is(wait_send_req) {
      when( only_0 || hit_0_miss_1 || miss_0_hit_1){
        wait_state :=  wait_one_resp
      }.elsewhen( miss_0_miss_1 ){
        wait_state := wait_two_resp
      }
    }

    is(wait_one_resp) {
      when( (only_0 || miss_0_hit_1) && fromMissQueue(0).fire()){
        wait_state := wait_finish
      }.elsewhen( hit_0_miss_1 && fromMissQueue(1).fire()){
        wait_state := wait_finish
      }
    }

    is(wait_two_resp) {
      when(fromMissQueue(0).fire() && fromMissQueue(1).fire()){
        wait_state := wait_finish
      }.elsewhen( !fromMissQueue(0).fire() && fromMissQueue(1).fire() ){
        wait_state := wait_0_resp
      }.elsewhen(fromMissQueue(0).fire() && !fromMissQueue(1).fire()){
        wait_state := wait_1_resp
      }
    }

    is(wait_0_resp) {
      when(fromMissQueue(0).fire()){
        wait_state := wait_finish
      }
    }

    is(wait_1_resp) {
      when(fromMissQueue(1).fire()){
        wait_state := wait_finish
      }
    }

    is(wait_finish) {
      when(io.toIbuffer.fire()) {wait_state := wait_idle }
    }
  }

  val miss_all_fix = wait_state === wait_finish
  f2_ready := (io.toIbuffer.ready && (f2_hit || miss_all_fix)) || !f2_valid

  (touch_ways zip touch_sets).zipWithIndex.map{ case((t_w,t_s), i) =>
    t_s(0)         := f1_vSetIdx(i)
    t_w(0).valid   := f1_bank_hit(i)
    t_w(0).bits    := OHToUInt(f1_bank_hit_vec(i))

    t_s(1)         := f2_vSetIdx(i)
    t_w(1).valid   := f2_valid && !f2_bank_hit(i)
    t_w(1).bits    := OHToUInt(f2_waymask(i))
  }
  
  val f2_hit_datas    = RegEnable(next = f1_hit_data, enable = f1_fire) 
  val f2_mq_datas     = RegInit(VecInit(fromMissQueue.map(p => p.bits.data)))    //TODO: Implement miss queue response
  val f2_datas        = Mux(f2_hit, f2_hit_datas, f2_mq_datas) // TODO: f1_hit_datas is error

  def cut(cacheline: UInt, start: UInt) : Vec[UInt] ={
    val result   = Wire(Vec(17, UInt(16.W)))
    val dataVec  = cacheline.asTypeOf(Vec(64, UInt(16.W)))
    val startPtr = Cat(0.U(1.W), start(offBits-1, 1))
    (0 until 17).foreach( i =>
      result(i) := dataVec(startPtr + i.U)
    )
    result
  }

  preDecoderIn.data       :=  cut(Cat(f2_datas.map(cacheline => cacheline.asUInt )).asUInt, f2_ftq_req.startAddr)
  preDecoderIn.startAddr  :=  f2_ftq_req.startAddr
  preDecoderIn.ftqOffset  :=  f2_ftq_req.ftqOffset
  preDecoderIn.target     :=  f2_ftq_req.target

  predecodeOutValid       := (f2_valid && f2_hit) || miss_all_fix

  io.toIbuffer.valid          := (f2_valid && f2_hit) || miss_all_fix
  io.toIbuffer.bits.instrs    := preDecoderOut.instrs
  io.toIbuffer.bits.valid     := VecInit(preDecoderOut.pd.map(instr => instr.valid)).asUInt
  io.toIbuffer.bits.pd        := preDecoderOut.pd
  io.toIbuffer.bits.ftqPtr    := f2_ftq_req.ftqIdx
  io.toIbuffer.bits.pc        := preDecoderOut.pc
  io.toIbuffer.bits.ftqOffset.zipWithIndex.map{case(a, i) => a.bits := i.U; a.valid := preDecoderOut.takens(i)}
  io.toIbuffer.bits.foldpc    := preDecoderOut.pc.map(i => XORFold(i(VAddrBits-1,1), MemPredPCWidth))


  toFtq.pdWb.valid           := (f2_valid && f2_hit) || miss_all_fix
  toFtq.pdWb.bits.pc         := preDecoderOut.pc
  toFtq.pdWb.bits.pd         := preDecoderOut.pd
  toFtq.pdWb.bits.ftqIdx     := f2_ftq_req.ftqIdx
  toFtq.pdWb.bits.ftqOffset  := f2_ftq_req.ftqOffset.bits 
  toFtq.pdWb.bits.misOffset  := preDecoderOut.misOffset
  toFtq.pdWb.bits.cfiOffset  := preDecoderOut.cfiOffset
  toFtq.pdWb.bits.target     := preDecoderOut.target



}
