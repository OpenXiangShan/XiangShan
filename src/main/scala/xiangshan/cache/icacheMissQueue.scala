package xiangshan.cache

import chisel3._
import chisel3.util._
import device._
import xiangshan._
import utils._
import chisel3.ExcitingUtils._
import chisel3.util.experimental.BoringUtils
import chipsalliance.rocketchip.config.Parameters

import freechips.rocketchip.tilelink.{TLBundleA,TLBundleD,TLBundleE,TLEdgeOut}
import freechips.rocketchip.diplomacy.{AddressSet,IdRange,LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLClientNode, TLClientParameters, TLMasterParameters, TLMasterPortParameters, TLArbiter}
import bus.tilelink.{TLParameters, TLPermissions, ClientMetadata}

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
    val waymask = UInt(PredictWidth.W)
    //val clientID = Bool()
    def apply(missAddr:UInt, missWaymask:UInt) = {
      this.addr := missAddr
      this.waymask := missWaymask
    }
}

class IcacheMissResp extends ICacheBundle
{
    val data = UInt(blockBits.W)
}

class IcacheMissEntry(edge: TLEdgeOut) extends ICacheMissQueueModule
{
    val io = IO(new Bundle{
        // MSHR ID
        val id = Input(UInt())

        val req = Flipped(DecoupledIO(new IcacheMissReq))
        val resp = DecoupledIO(new IcacheMissResp)
        
        val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
        val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

        val meta_write = DecoupledIO(new ICacheMetaWrite)
        val refill = DecoupledIO(new ICacheRefill)

        val flush = Input(Bool())
    })

    val s_idle :: s_memReadReq :: s_memReadResp :: s_write_back :: s_wait_resp :: Nil = Enum(5)
    val state = RegInit(s_idle)

    //req register
    val req = Reg(new IcacheMissReq)
    val req_idx = get_idx(req.addr)
    val req_tag = get_tag(req.addr)
    val req_waymask = req.waymask

    //8 for 64 bits bus and 2 for 256 bits
    val readBeatCnt = Counter(refillCycles)
    val refillDataReg = Reg(Vec(refillCycles,UInt(beatBits.W)))

    val (_, _, refill_done, refill_cnt) = edge.count(io.mem_grant)

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
    val countFull = readBeatCnt.value === (refillCycles - 1).U
    switch(state){
      is(s_idle){
        when(io.req.fire()){
          state := s_memReadReq
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
        when (edge.hasData(io.mem_grant.bits) && io.mem_grant.fire()) {
          readBeatCnt.inc()
	        refillDataReg(readBeatCnt.value) := io.mem_grant.bits.data
          when(countFull){
            assert(refill_done, "refill not done!")
            state := Mux(needFlush,s_idle,s_write_back)
          }
        }
      }

      is(s_write_back){
        when(io.refill.fire() && io.meta_write.fire()){
          state := Mux(needFlush,s_idle,s_wait_resp)
        }
      }

      is(s_wait_resp){
        io.resp.bits.data := refillDataReg.asUInt
        when(io.resp.fire() || needFlush ){ state := s_idle }
      }

    }

    //refill write and meta write
    //WARNING: Maybe could not finish refill in 1 cycle
    io.meta_write.valid := (state === s_write_back) && !needFlush
    io.meta_write.bits.apply(tag=req_tag, setIdx=req_idx, waymask=req_waymask)
   
    io.refill.valid := (state === s_write_back) && !needFlush
    io.refill.bits.apply(data=refillDataReg.asUInt,
                        setIdx=req_idx,
                        waymask=req_waymask)



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

  io.req.ready  := req_ready
  io.resp.valid := resp_arb.io.out.valid
  io.resp.bits  := resp_arb.io.out.bits
  resp_arb.io.out.ready := true.B

  io.meta_write <> meta_write_arb.io.out
  io.refill     <> refill_arb.io.out

  TLArbiter.lowestFromSeq(edge, io.mem_acquire, entries.map(_.io.mem_acquire))
}