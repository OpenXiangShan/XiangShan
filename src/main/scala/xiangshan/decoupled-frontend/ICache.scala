package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import utils._

case class ICacheParameters(
    nSets: Int = 64,
    nWays: Int = 4,
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

trait Temperary {
  val idxBits = log2Ceil(nSets)
  val wayBits = log2Ceil(nWays)
  val offBits = log2Ceil(64)
  val tagBits = 39 - idxBits - offBits
  val bbBits  = 5
  def plruAccessNum = 2  //hit and miss

  val nSets   = 128           //32 KB
  val nWays   = 4

  val nMissEntries = 2

}

abstract class ICacheBundle(implicit p: Parameters) extends XSBundle
  with HasICacheParameters
  with Temperary 

abstract class ICacheModule(implicit p: Parameters) extends XSModule
  with HasICacheParameters
  with Temperary 

abstract class ICacheArray(implicit p: Parameters) extends XSModule
  with HasICacheParameters
  with Temperary 

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
  val bankIdx   = Bool()

  def apply(tag:UInt, idx:UInt, waymask:UInt, bankIdx: Bool){
    this.virIdx  := idx
    this.phyTag  := tag
    this.waymask := waymask
    this.brIdx   := bankId 
  }

}

class ICacheDataWriteBundle(implicit p: Parameters) extends ICacheBundle
{
  val virIdx  = UInt(idxBits.W)
  val data    = Vec(blockRows,UInt(blockBits.W))
  val waymask = UInt(nWays.W)
  val bankIdx = Bool()

  def apply(data:Vec[UInt], idx:UInt, waymask:UInt, bankIdx: Bool){
    this.virIdx  := idx
    this.data    := data
    this.waymask := waymask
    this.bankIdx := bankIdx 
  }

}

class ICacheDataRespBundle(implicit p: Parameters) extends ICacheBundle
{
  val datas = Vec(2,Vec(nWays,Vec(blockRows,UInt(blockBits.W))))
}


class ICacheMetaArray(implicit p: Parameters) extends ICacheArray
{
  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheMetaWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle))
    val readResp = Output(new ICacheMetaRespBundle)
  }}

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
    if(i == 0) tagArray.io.r.req.valid := io.read.valid 
    else tagArray.io.r.req.valid := io.read.valid && io.read.bits.isDoubleLine 
    tagArray.io.r.req.bits.apply(setIdx=io.read.bits.vSetIdx)

    if(i == 0) tagArray.io.w.req.valid := io.write.valid && !io.write.bits.bankIdx
    else       tagArray.io.w.req.valid := io.write.valid &&  io.write.bits.bankIdx
    tagArray.io.w.req.bits.apply(data=io.write.bits.phyTag, setIdx=io.write.virIdx, waymask=io.write.waymask)
   
    tagArray  
  }

  val readIdxNext = RegNext(io.read.bits.vSetIdx)
  val validArray = RegInit(0.U(nSets * nWays).W)
  val validMetas = VecInit(readIdx.map{ bank =>
    val validMeta =  Cat((0 until nWays).map{w => validArray(Cat(readIdxNext(bank), w.U(log2Ceil(nWays).W)))}.reverse).asUInt
    validMeta
  })

  val wayNum   = OHToUInt(io.write.bits.waymask)
  val validPtr = Cat(io.write.bits.vitIdx, wayNum)
  when(io.write.valid){
    validArray := validArray.bitSet(validPtr, true.B)
  }

  (io.resp.tags zip tagArray).map    {case (io, sram) => io := sram.io.r.resp.asTypeOf(Vec(nWays, UInt(tagBits.W)))}
  (io.resp.valid zip validMetas).map {case (io, reg)  => io := reg.asTypeOf(Vec(nWays,Bool()))}
}


class ICacheDataArray(implicit p: Parameters) extends ICacheArray
{
  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheDataWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle)))
    val readResp = Output(new ICacheDataRespBundle)
  }}

  //dataEntryBits = 144 
  val dataArrays = (0 untils 2) map { i =>
    val dataArray = List.fill(nWays){Module(new SRAMTemplate(
      UInt(blockBits.W),
      set=nSets/2,
      way=nWays,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))}

    //port 
    dataArray.map{ way =>
      //meta connection
      if(i == 0) way.io.r.req.valid := io.read.valid 
      else way.io.r.req.valid := io.read.valid && io.read.bits.isDoubleLine 
      way.io.r.req.bits.apply(setIdx=io.read.bits.vSetIdx)

      if(i == 0) way.io.w.req.valid := io.write.valid && !io.write.bits.bankIdx
      else       way.io.w.req.valid := io.write.valid &&  io.write.bits.bankIdx
      way.io.w.req.bits.apply(data=io.write.bits.phyTag, setIdx=io.write.virIdx, waymask=io.write.waymask)
     
    }

    dataArray 
  }

  (io.resp.datas zip dataArrays).map {case (io, sram) => io := Cat(sram.map(way => way.io.r.resp.asTypeOf(Vec(blockRows, UInt(rowBits.W))))).asTypeOf(Vec(nWays, Vec(blockRows, UInt(rowBits.W))))}  

  io.write.ready := DontCare
}


abstract class ICacheMissQueueModule(implicit p: Parameters) extends XSModule
  with HasICacheParameters 
  with Temperary 

abstract class ICacheMissQueueBundle(implicit p: Parameters) extends XSBundle
  with HasICacheParameters
  with Temperary 

class ICacheMissReq(implicit p: Parameters) extends ICacheBundle
{
    val addr      = UInt(PAddrBits.W)
    val vSetIdx   = UInt(idxBits.W)
    val waymask   = UInt(16.W)
    val clientID  = UInt(log2Ceil(cacheParams.nMissEntries).W)
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
    val clientID = UInt(log2Ceil(cacheParams.nMissEntries).W) 
}

class ICacheMissEntry(implicit p: Parameters) extends ICacheMissQueueModule
{
    val io = IO(new Bundle{
        // MSHR ID
        val id          = Input(UInt(log2Up(cacheParamstries).W))

        val req         = Flipped(DecoupledIO(new ICacheMissReq))
        val resp        = DecoupledIO(new ICacheMissResp)
        
        val mem_acquire = DecoupledIO(new L1plusCacheReq)
        val mem_grant   = Flipped(DecoupledIO(new L1plusCacheResp))

        val meta_write  = DecoupledIO(new ICacheMetaWriteBundle)
        val data_write  = DecoupledIO(new ICacheDataRespBundle)
    
        val flush = Input(Bool())
    })

    val s_idle :: s_memReadReq :: s_memReadResp :: s_write_back :: s_wait_resp :: Nil = Enum(5)
    val state = RegInit(s_idle)

    //req register
    val req = Reg(new ICacheMissReq)
    val req_idx = req.vSetIdx         //virtual index
    val req_tag = get_tag(req.addr)           //physical tag
    val req_waymask = req.waymask

    //8 for 64 bits bus and 2 for 256 bits
    val readBeatCnt = Counter(refillCycles)
    //val respDataReg = Reg(Vec(refillCycles,UInt(beatBits.W)))
    val respDataReg = Reg(UInt(blockBits.W))

    //initial
    io.resp.bits := DontCare
    io.mem_acquire.bits := DontCare
    io.mem_grant.ready := true.B   
    io.meta_write.bits := DontCare
    io.refill.bits := DontCare

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
        when (io.mem_grant.bits.id === io.id && io.mem_grant.fire()) {
	        respDataReg := io.mem_grant.bits.data
          state := Mux(needFlush || io.flush,s_wait_resp,s_write_back)
        }
      }

      //TODO: Maybe this sate is noe necessary so we don't need respDataReg
      is(s_write_back){
        when((io.data_write.fire() && io.meta_write.fire()) || needFlush){
          state := s_wait_resp
        }
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
    io.meta_write.bits.apply(tag=req_tag, virIdx=req_idx, waymask=req_waymask, bankIdx=req_idx(0))
   
    io.data_write.valid := (state === s_write_back) && !needFlush 
    io.data_write.bits.apply(data=respDataReg.asTypeOf(Vec(blockRows, rowBits)), vSetIdx=req_idx, waymask=req_waymask, bankIdx=req_idx(0))

    //mem request
    io.mem_acquire.bits.cmd  := MemoryOpConstants.M_XRD
    io.mem_acquire.bits.addr := req.addr
    io.mem_acquire.bits.id   := io.id

    //resp to icache
    io.resp.valid := (state === s_wait_resp) && !needFlush

    XSDebug("[ICache MSHR %d] (req)valid:%d  ready:%d req.addr:%x waymask:%b  || Register: req:%x  \n",io.id.asUInt,io.req.valid,io.req.ready,io.req.bits.addr,io.req.bits.waymask,req.asUInt)
    XSDebug("[ICache MSHR %d] (Info)state:%d  needFlush:%d\n",io.id.asUInt,state,needFlush)
    XSDebug("[ICache MSHR %d] (mem_acquire) valid%d ready:%d\n",io.id.asUInt,io.mem_acquire.valid,io.mem_acquire.ready)
    XSDebug("[ICache MSHR %d] (mem_grant)   valid%d ready:%d data:%x \n",io.id.asUInt,io.mem_grant.valid,io.mem_grant.ready,io.mem_grant.bits.data)
    XSDebug("[ICache MSHR %d] (meta_write)  valid%d ready:%d  tag:%x \n",io.id.asUInt,io.meta_write.valid,io.meta_write.ready,io.meta_write.bits.meta_write_tag)
    XSDebug("[ICache MSHR %d] (refill)  valid%d ready:%d  data:%x \n",io.id.asUInt,io.refill.valid,io.refill.ready,io.refill.bits.refill_data)
    XSDebug("[ICache MSHR %d] (resp)  valid%d ready:%d \n",io.id.asUInt,io.resp.valid,io.resp.ready)


}

class ICacheMissQueue(implicit p: Parameters) extends ICacheMissQueueModule
{
  val io = IO(new Bundle{
    val req_1         = Flipped(DecoupledIO(new ICacheMissReq))
    val req_2         = Flipped(DecoupledIO(new ICacheMissReq))
    val resp        = DecoupledIO(new ICacheMissResp)
    
    val mem_acquire = DecoupledIO(new L1plusCacheReq)
    val mem_grant   = Flipped(DecoupledIO(new L1plusCacheResp))

    val meta_write  = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write  = DecoupledIO(new ICacheDataWriteBundle)

  })

  val resp_arb       = Module(new Arbiter(new ICacheMissResp,   cacheParams.nMissEntries))
  val meta_write_arb = Module(new Arbiter(new ICacheMetaWriteBundle,  cacheParams.nMissEntries))
  val refill_arb     = Module(new Arbiter(new ICacheDataWriteBundle,     cacheParams.nMissEntries))
  val mem_acquire_arb= Module(new Arbiter(new L1plusCacheReq,   cacheParams.nMissEntries))

  //initial
  io.mem_grant.ready := true.B
  
  val entry_alloc_idx = Wire(UInt())
  val req_ready = WireInit(false.B)

  val entries = (0 until cacheParams.nMissEntries) map { i =>
    val entry = Module(new ICacheMissEntry)

    entry.io.id := i.U(log2Up(cacheParams.nMissEntries).W)

    // entry req
    entry.io.req.valid := (i.U === entry_alloc_idx) && io.req.valid
    entry.io.req.bits  := io.req.bits
    when (i.U === entry_alloc_idx) {
      req_ready := entry.io.req.ready
    }

    // entry resp
    resp_arb.io.in(i)       <>  entry.io.resp
    meta_write_arb.io.in(i) <>  entry.io.meta_write
    refill_arb.io.in(i)     <>  entry.io.refill
    mem_acquire_arb.io.in(i)    <>   entry.io.mem_acquire

    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.id === i.U) {
      entry.io.mem_grant <> io.mem_grant
    }

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

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.req.ready))

  io.req.ready  := req_ready
  io.resp <> resp_arb.io.out
  io.meta_write <> meta_write_arb.io.out
  io.refill     <> refill_arb.io.out
  io.mem_acquire <> mem_acquire_arb.io.out

}
