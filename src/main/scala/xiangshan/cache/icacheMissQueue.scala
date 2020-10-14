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

class ICacheRefill extends XSBundle
{
    refill_idx = UInt()
    refill_data = UInt()
    refill_waymask = UInt()
}

class ICacheMetaWrite extends XSBundle
{
    meta_wirte_idx = UInt()
    meta_wirte_tag = UInt()
    meta_write_waymask = UInt()
}

class IcacheMissReq extends ICacheBundle
{
    val addr  = UInt(PAddrBits.W)
    val waymask = UInt(PredictWidth.W)
    val clientID = Bool()
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
        val resp = Flipped(DecoupledIO(new IcacheMissResp))
        
        val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
        val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

        val meta_wirte = DecoupledIO(new ICacheMetaWrite)
        val refill = DecoupledIO(new ICacheRefill)

        val flush = UInt(2.W)
    })

    val s_idle :: s_memReadReq :: s_memReadResp :: s_write_back :: s_wait_resp :: Nil = Enum(7)
    val state = RegInit(s_idle)

    //req register
    val req = Reg(new IcacheMissReq)
    val req_idx = get_idx(req.addr)
    val req_tag = get_tag(req.addr)

    //8 for 64 bits bus and 2 for 256 bits
    val readBeatCnt = Counter(refillCycles)
    val refillDataReg = Reg(Vec(refillCycles,UInt(beatBits.W)))

    val (_, _, refill_done, refill_cnt) = edge.count(io.mem_grant)

    //initial
    io.req.ready := false.B
    io.resp.valid := false.B
    io.resp.bits := DontCare

    io.mem_acquire.valid := false.B
    io.mem_acquire.bits := DontCare

    io.mem_grant.ready := true.B   

    io.meta_wirte.valid := false.B
    io.meta_wirte.bits := DontCare

    io.refill.valid := false.B
    io.refill.bits := DontCare

    //state change
    val countFull = readBeatCnt.value === (refillCycles - 1).U
    switch(state){
      is(s_idle){
        io.req.ready := true.B
        when(io.req.fire() && io.flush === 0.U){
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
        io.mem_acquire.valid := true.B
        when (edge.hasData(io.mem_grant.bits) && io.mem_grant.d.fire()) {
          readBeatCnt.inc()
	      refillDataReg(readBeatCnt.value) := io.mem_grant.bits.data
          when(countFull){
            assert(refill_done, "refill not done!")
            state := s_wait_resp
          }
        }
      }

      is(s_write_back){
          when(io.refill.fire()){ state := s_wait_resp}
      }

      is(s_wait_resp){
        io.resp.valid := true.B
        io.resp.bits.data := refillDataReg
        when(io.resp.fire() || needFlush ){ state := s_idle }
      }

    }

    //refill write
    val metaWrite = Wire(new ICacheMetaBundle)
    val refillFinalOneBeat = (state === s_memReadResp) && io.mem_grant.fire() && refill_done
    val wayNum = OHToUInt(waymask)
    val validPtr = Cat(get_idx(s3_req_pc),wayNum)
    metaWrite.tag := get_tag(s3_req_pc)
    io.meta_wirte.valid := refillFinalOneBeat
    io.meta_wirte.bits.apply(data=metaWrite, setIdx=get_idx(s3_req_pc), waymask=s3_wayMask)
   
    if(beatBits == 64){
        for(b <- 0 until blockWords){
        val writeOneBeat = (state === s_memReadResp) && io.mem_grant.fire() && (b.U === readBeatCnt.value)
        io.refill.valid := writeOneBeat
        io.refill.bits.apply(   setIdx=get_idx(s3_req_pc), 
                                            data=io.mem_grant.bits.data.asTypeOf(new ICacheDataBundle), 
                                            waymask=s3_wayMask)
   
        }
    }
    else{
        val writeFirstHalf = (state === s_memReadResp) && io.mem_grant.fire() && (readBeatCnt.value === 0.U)
        (0 until blockWords/2).foreach{ b =>
        io.refill.valid := writeFirstHalf
        io.refill.bits.apply( setIdx=get_idx(s3_req_pc),
                                            data=io.mem_grant.bits.data(b * 64 +63, b*64).asTypeOf(new ICacheDataBundle),
                                            waymask=s3_wayMask)
   
        }
        val writeLastHalf = (state === s_memReadResp) && io.mem_grant.fire() && (readBeatCnt.value === 1.U)
        (blockWords/2 until blockWords).foreach{ b =>
        val index = b - blockWords/2
        io.refill.valid := writeLastHalf
        io.refill.bits.apply( setIdx=get_idx(s3_req_pc),
                                            data=io.mem_grant.bits.data(index * 64 +63, index*64).asTypeOf(new ICacheDataBundle), 
                                            waymask=s3_wayMask)
   
        }
   
    }



}

class IcacheMissQueue(edge: TLEdgeOut) extends ICacheMissQueueModule 
{
  val io = IO(new Bundle{
    val req = Flipped(DecoupledIO(new IcacheMissReq))
    val resp = Flipped(DecoupledIO(new IcacheMissResp))
    
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val meta_wirte = DecoupledIO(new ICacheMetaWrite)
    val refill = DecoupledIO(new ICacheRefill)

    val flush = UInt(2.W)

  })

  val resp_arb = 
}