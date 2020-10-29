package xiangshan.cache

import chisel3._
import chisel3.util._
import device._
import xiangshan._
import utils._
import chisel3.ExcitingUtils._
import chisel3.util.experimental.BoringUtils

abstract class ICacheMissQueueModule extends XSModule
  with HasICacheParameters 
  with HasXSLog

abstract class ICacheMissQueueBundle extends XSBundle
  with HasICacheParameters

class ICacheRefill extends ICacheMissQueueBundle
{
    val refill_idx = UInt(idxBits.W)
    val refill_data = UInt(blockBits.W)
    val refill_waymask = UInt(nWays.W)

    def apply(data:UInt, setIdx:UInt, waymask:UInt) = {
      this.refill_idx := setIdx
      this.refill_data := data
      this.refill_waymask := waymask
    }
}

class ICacheMetaWrite extends ICacheMissQueueBundle
{
    val meta_write_idx = UInt(idxBits.W)
    val meta_write_tag = UInt(tagBits.W)
    val meta_write_waymask = UInt(nWays.W)

    def apply(tag:UInt, setIdx:UInt, waymask:UInt) = {
      this.meta_write_idx := setIdx
      this.meta_write_tag := tag
      this.meta_write_waymask := waymask
    }
}

class IcacheMissReq extends ICacheBundle
{
    val addr  = UInt(PAddrBits.W)
    val setIdx   = UInt(idxBits.W)
    val waymask = UInt(PredictWidth.W)
    val clientID = UInt(2.W)
    def apply(missAddr:UInt, missIdx:UInt, missWaymask:UInt, source:UInt) = {
      this.addr := missAddr
      this.setIdx  := missIdx
      this.waymask := missWaymask
      this.clientID := source
    }
}

class IcacheMissResp extends ICacheBundle
{
    val data = UInt(blockBits.W)
    val clientID = UInt(2.W)
}

class IcacheMissEntry extends ICacheMissQueueModule
{
    val io = IO(new Bundle{
        // MSHR ID
        val id = Input(UInt(log2Up(nMSHRs).W))

        val req = Flipped(DecoupledIO(new IcacheMissReq))
        val resp = DecoupledIO(new IcacheMissResp)
        
        val mem_acquire = DecoupledIO(new L1plusCacheReq)
        val mem_grant   = FFlipped(DecoupledIO(new L1plusCacheResp))

        val meta_write = DecoupledIO(new ICacheMetaWrite)
        val refill = DecoupledIO(new ICacheRefill)

        val flush = Input(Bool())
    })

    val s_idle :: s_memReadReq :: s_memReadResp :: s_write_back :: s_wait_resp :: Nil = Enum(5)
    val state = RegInit(s_idle)

    //req register
    val req = Reg(new IcacheMissReq)
    val req_idx = get_idx(req.setIdx)         //virtual index
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
    io.resp.valid := state === s_wait_resp

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
          readBeatCnt.value := 0.U
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

      is(s_write_back){
        when(io.refill.fire() && io.meta_write.fire()){
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
    io.meta_write.bits.apply(tag=req_tag, setIdx=req_idx, waymask=req_waymask)
   
    io.refill.valid := (state === s_write_back) && !needFlush
    io.refill.bits.apply(data=respDataReg.asUInt,
                        setIdx=req_idx,
                        waymask=req_waymask)

    //mem request
    io.mem_acquire.bits.cmd := M_SZ.U 
    io.mem_acquire.bits.addr := req.



    XSDebug("[ICache MSHR %d] (req)valid:%d  ready:%d req.addr:%x waymask:%b  || Register: req:%x  \n",io.id.asUInt,io.req.valid,io.req.ready,io.req.bits.addr,io.req.bits.waymask,req.asUInt)
    XSDebug("[ICache MSHR %d] (Info)state:%d  refill_done:%d contFull:%d readBeatCnt:%d  needFlush:%d\n",io.id.asUInt,state,refill_done,countFull,readBeatCnt.value,needFlush)
    XSDebug("[ICache MSHR %d] (mem_acquire) valid%d ready:%d\n",io.id.asUInt,io.mem_acquire.valid,io.mem_acquire.ready)
    XSDebug("[ICache MSHR %d] (mem_grant)   valid%d ready:%d data:%x \n",io.id.asUInt,io.mem_grant.valid,io.mem_grant.ready,io.mem_grant.bits.data)
    XSDebug("[ICache MSHR %d] (meta_write)  valid%d ready:%d  tag:%x \n",io.id.asUInt,io.meta_write.valid,io.meta_write.ready,io.meta_write.bits.meta_write_tag)
    XSDebug("[ICache MSHR %d] (refill)  valid%d ready:%d  data:%x \n",io.id.asUInt,io.refill.valid,io.refill.ready,io.refill.bits.refill_data)
    XSDebug("[ICache MSHR %d] (resp)  valid%d ready:%d \n",io.id.asUInt,io.resp.valid,io.resp.ready)


}

class IcacheMissQueue(edge: TLEdgeOut) extends ICacheMissQueueModule 
{
  val io = IO(new Bundle{
    val req = Flipped(DecoupledIO(new IcacheMissReq))
    val resp = DecoupledIO(new IcacheMissResp)
    
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val meta_write = DecoupledIO(new ICacheMetaWrite)
    val refill = DecoupledIO(new ICacheRefill)

    val flush = Input(Bool())

  })

  val resp_arb       = Module(new Arbiter(new IcacheMissResp,   nMSHRs))
  val meta_write_arb = Module(new Arbiter(new ICacheMetaWrite,  nMSHRs))
  val refill_arb     = Module(new Arbiter(new ICacheRefill,     nMSHRs))

  //initial
  io.mem_grant.ready := true.B
  
  val entry_alloc_idx = Wire(UInt())
  val req_ready = WireInit(false.B)

  val entries = (0 until nMSHRs) map { i =>
    val entry = Module(new IcacheMissEntry(edge))

    entry.io.id := i.U(log2Up(nMSHRs).W)
    entry.io.flush := io.flush

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

    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      entry.io.mem_grant <> io.mem_grant
    }
    entry
  }

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.req.ready))
  XSDebug("[ICache MissQueue] (ready vector) %b idx:%d \n",PriorityEncoder(entries.map(m=>m.io.req.ready)),entry_alloc_idx)

  io.req.ready  := req_ready
  io.resp <> resp_arb.io.out
  io.meta_write <> meta_write_arb.io.out
  io.refill     <> refill_arb.io.out

  TLArbiter.lowestFromSeq(edge, io.mem_acquire, entries.map(_.io.mem_acquire))
}
