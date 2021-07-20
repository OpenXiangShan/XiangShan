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

trait HasICacheParameters extends HasL1CacheParameters with HasIFUConst with HasInstrMMIOConst {
  val cacheParams = icacheParameters
  val groupAlign = log2Up(cacheParams.blockBytes)
  val packetInstNum = packetBytes/instBytes
  val packetInstNumBit = log2Up(packetInstNum)
  val ptrHighBit = log2Up(groupBytes) - 1
  val ptrLowBit = log2Up(packetBytes)
  val encUnitBits = 8
  val bankRows = 2
  val bankBits = bankRows * rowBits
  val nBanks = blockRows/bankRows
  val bankUnitNum = (bankBits / encUnitBits)

  def cacheID = 0
  def insLen = if (HasCExtension) 16 else 32
  def RVCInsLen = 16
  def groupPC(pc: UInt): UInt = Cat(pc(PAddrBits-1, groupAlign), 0.U(groupAlign.W))
  def plruAccessNum = 2  //hit and miss
  // def encRowBits = cacheParams.dataCode.width(rowBits)
  // def encTagBits = cacheParams.tagCode.width(tagBits)

  //
  def encMetaBits = cacheParams.tagCode.width(tagBits)
  def metaEntryBits = encMetaBits
  def encDataBits = cacheParams.dataCode.width(encUnitBits)
  def dataEntryBits = encDataBits * bankUnitNum
  // def encDataBits
  // def encCacheline


  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
  require(full_divide(rowBits, wordBits), s"rowBits($rowBits) must be multiple of wordBits($wordBits)")
  require(full_divide(beatBits, rowBits), s"beatBits($beatBits) must be multiple of rowBits($rowBits)")
  // this is a VIPT L1 cache
  require(pgIdxBits >= untagBits, s"page aliasing problem: pgIdxBits($pgIdxBits) < untagBits($untagBits)")
}

trait Temperary {
  // val idxBits = log2Ceil(nSets)
  // val wayBits = log2Ceil(nWays)
  val offBits = log2Ceil(64)
  // val tagBits = 39 - idxBits - offBits
  // val bbBits  = 5
  // def plruAccessNum = 2  //hit and miss

  // val nSets   = 128           //32 KB
  // val nWays   = 4

  // val nMissEntries = 2

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


class ICacheMetaArray(implicit p: Parameters) extends ICacheArray
{
  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheMetaWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle))
    val readResp = Output(new ICacheMetaRespBundle)
  }}

  io.read.ready := !io.write.valid

  val tagArrays = (0 until 2) map { bank =>
    val tagArray = Module(new SRAMTemplate(
      UInt(tagBits.W),
      set=nSets,
      way=nWays,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))

    //meta connection
    if(bank == 0) tagArray.io.r.req.valid := io.read.valid
    else tagArray.io.r.req.valid := io.read.valid && io.read.bits.isDoubleLine 
    tagArray.io.r.req.bits.apply(setIdx=io.read.bits.vSetIdx(bank))

    tagArray.io.w.req.valid := io.write.valid 
    tagArray.io.w.req.bits.apply(data=io.write.bits.phyTag, setIdx=io.write.bits.virIdx, waymask=io.write.bits.waymask)
   
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

  (io.readResp.tags zip tagArrays).map    {case (io, sram) => io  := sram.io.r.resp.asTypeOf(Vec(nWays, UInt(tagBits.W)))}
  (io.readResp.valid zip validMetas).map  {case (io, reg)   => io := reg.asTypeOf(Vec(nWays,Bool()))}

  io.write.ready := DontCare
}


class ICacheDataArray(implicit p: Parameters) extends ICacheArray
{
  val io=IO{new Bundle{
    val write    = Flipped(DecoupledIO(new ICacheDataWriteBundle))
    val read     = Flipped(DecoupledIO(new ICacheReadBundle))
    val readResp = Output(new ICacheDataRespBundle)
  }}

  io.read.ready := !io.write.valid
  
  val dataArrays = (0 until 2) map { i =>
    val dataArray = Module(new SRAMTemplate(
      UInt(blockBits.W),
      set=nSets,
      way=nWays,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))

    //meta connection
    if(i == 0) dataArray.io.r.req.valid := io.read.valid 
    else dataArray.io.r.req.valid := io.read.valid && io.read.bits.isDoubleLine 
    dataArray.io.r.req.bits.apply(setIdx=io.read.bits.vSetIdx(i))

    dataArray.io.w.req.valid := io.write.valid 
    dataArray.io.w.req.bits.apply(data=io.write.bits.data, setIdx=io.write.bits.virIdx, waymask=io.write.bits.waymask)

    dataArray 
  }

  (io.readResp.datas zip dataArrays).map {case (io, sram) => io :=  sram.io.r.resp.data.asTypeOf(Vec(nWays, UInt(blockBits.W)))  }

  io.write.ready := true.B
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

class ICacheMissEntry(implicit p: Parameters) extends ICacheMissQueueModule
{
    val io = IO(new Bundle{
        // MSHR ID
        val id          = Input(UInt(1.W))

        val req         = Flipped(DecoupledIO(new ICacheMissReq))
        val resp        = DecoupledIO(new ICacheMissResp)
        
        val mem_acquire = DecoupledIO(new L1plusCacheReq)
        val mem_grant   = Flipped(DecoupledIO(new L1plusCacheResp))

        val meta_write  = DecoupledIO(new ICacheMetaWriteBundle)
        val data_write  = DecoupledIO(new ICacheDataWriteBundle)
    
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
        //when((io.data_write.fire() && io.meta_write.fire()) || needFlush){
          state := s_wait_resp
        //}
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
    io.data_write.bits.apply(data=respDataReg, idx=req_idx, waymask=req_waymask, bankIdx=req_idx(0))

    //mem request
    io.mem_acquire.bits.cmd  := MemoryOpConstants.M_XRD
    io.mem_acquire.bits.addr := Cat(req.addr(PAddrBits - 1, log2Ceil(blockBytes)), 0.U(log2Ceil(blockBytes).W))
    io.mem_acquire.bits.id   := io.id

    //resp to icache
    io.resp.valid := (state === s_wait_resp) && !needFlush

    XSDebug("[ICache MSHR %d] (req)valid:%d  ready:%d req.addr:%x waymask:%b  || Register: req:%x  \n",io.id.asUInt,io.req.valid,io.req.ready,io.req.bits.addr,io.req.bits.waymask,req.asUInt)
    XSDebug("[ICache MSHR %d] (Info)state:%d  needFlush:%d\n",io.id.asUInt,state,needFlush)
    XSDebug("[ICache MSHR %d] (mem_acquire) valid%d ready:%d\n",io.id.asUInt,io.mem_acquire.valid,io.mem_acquire.ready)
    XSDebug("[ICache MSHR %d] (mem_grant)   valid%d ready:%d data:%x \n",io.id.asUInt,io.mem_grant.valid,io.mem_grant.ready,io.mem_grant.bits.data)
    XSDebug("[ICache MSHR %d] (meta_write)  valid%d ready:%d  tag:%x \n",io.id.asUInt,io.meta_write.valid,io.meta_write.ready,io.meta_write.bits.phyTag)
    XSDebug("[ICache MSHR %d] (refill)  valid%d ready:%d  data:%x \n",io.id.asUInt,io.data_write.valid,io.data_write.ready,io.data_write.bits.data.asUInt())
    XSDebug("[ICache MSHR %d] (resp)  valid%d ready:%d \n",io.id.asUInt,io.resp.valid,io.resp.ready)


}

//TODO: This is a stupid missqueue that has only 2 entries
class ICacheMissQueue(implicit p: Parameters) extends ICacheMissQueueModule
{
  val io = IO(new Bundle{
    val req         = Vec(2, Flipped(DecoupledIO(new ICacheMissReq)))
    val resp        = Vec(2, DecoupledIO(new ICacheMissResp))
    
    val mem_acquire = DecoupledIO(new L1plusCacheReq)
    val mem_grant   = Flipped(DecoupledIO(new L1plusCacheResp))

    val meta_write  = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write  = DecoupledIO(new ICacheDataWriteBundle)

    val flush       = Input(Bool())

  })

  val meta_write_arb = Module(new Arbiter(new ICacheMetaWriteBundle,  2))
  val refill_arb     = Module(new Arbiter(new ICacheDataWriteBundle,  2))
  val mem_acquire_arb= Module(new Arbiter(new L1plusCacheReq,   2))

  io.mem_grant.ready := true.B

  val entries = (0 until 2) map { i =>
    val entry = Module(new ICacheMissEntry)

    entry.io.id := i.U(1.W)
    entry.io.flush := io.flush

    // entry req
    entry.io.req.valid := io.req(i).valid
    entry.io.req.bits  := io.req(i).bits
    io.req(i).ready    := entry.io.req.ready

    // entry resp
    meta_write_arb.io.in(i)     <>  entry.io.meta_write
    refill_arb.io.in(i)         <>  entry.io.data_write
    mem_acquire_arb.io.in(i)    <>  entry.io.mem_acquire

    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.id === i.U) {
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

  io.meta_write     <> meta_write_arb.io.out
  io.data_write     <> refill_arb.io.out
  io.mem_acquire    <> mem_acquire_arb.io.out

}
