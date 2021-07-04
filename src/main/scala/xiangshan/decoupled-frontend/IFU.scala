package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class FtqInterface(implicit p: Parameters) extends XSBundle {
  val fromFtq = Flipped(DecoupledIO(new FtqToFetch))
  val toFtq   = Valid(new FetchToFtq)
}

class ICacheInterface（implicit p: Parameters) extends XSBundle {
  val toIMeta       = DecoupledIO(new FetchToICache)
  val toIData       = DecoupledIO(new FetchToICache)
  val toMissQueue   = DecoupledIO(new FetchToMissQueue)
  val fromIMeta     = Flipped(ValidIO(new IMetaToFetch))
  val fromIData     = Flipped(ValidIO(new IDataToFetch))
  val fromMissQueue = Flipped(DecoupledIO(FetchToMissQueue))
}


class IFUIO(implicit p: Parameters) extends XSBundle {
  val ftqInter        = new FtqInterface  
  val icacheInter     = new ICacheInterface 
  val toIbuffer       = DecoupledIO(new FetchToIBuffer)
  
}

@chiselName
class IFU(implicit p: Parameters) extends XSModule
{
  val io = IO(new IFUIO)
  val (toFtq, fromFtq)    = (io.ftqInter.toFtq, io.ftqInter.fromFtq)
  val (toMeta, toData, meta_resp, data_resp) =  (io.icacheInter.toIMeta, io.icacheInter.toIData, io.icacheInter.fromIMeta, io.icacheInter.fromIData)
  val (toMissQueue, fromMissQueue) = (io.icacheInter.toMissQueue, io.icacheInter.fromMissQueue)

  //fetch: send addr to Meta/TLB and Data simultaneously
  val fetch_req = Seq(toMeta, toData)
  fetch_req.map(channel => 
    channel.valid        := fromFtq.valid 
    channel.bits.start   := fromFtq.startAddr
    channel.bits.endOffset  := fromFtq.bits.endOffset 
  )

  //fetch response
  val fetch_valid = meta_resp.valid && meta_resp.bits.hit 
  val fetch_fail  = meta_resp.valid && !meta_resp.bits.hit 
  
  io.toIbuffer.valid := fetch_valid || (fetch_miss_register && REFILL_VALID)
  (0 until FetchWidth).map(i  => io.toIbuffer.bits.instr(i) := data_resp.bits.instr(i))

  val f1_valid  = RegInit(false.B)
  val f1_ftqIdx = RegEnable(next = fromFtq.bits.ftqIdx, enable=fromFtq.valid)
  val f1_ready  = WireInit(false.B)
  val f1_fire   = f1_valid && f1_ready 
  when(fromFtq.valid){
    f1_valid  := true.B
  } .elsewhen(f1_fire) {
    f1_valid  := false.B
  }

  f1_ready := f2_ready && ITLB_RESP_VALID || !f2_valid

  val f2_valid  = RegInit(false.B)
  val f2_ftqIdx = RegEnable(next = f1_ftqIdx, enable = f1_fire)
  //TODO: other meta info like hit/mmio/exception 
  
  when(f1_fire){
    f2_valid := true.B
  } .elsewhen(f2_fire) {
    f2_valid := false.B
  }

  f2_ready := io.toIbuffer.ready
  
}


