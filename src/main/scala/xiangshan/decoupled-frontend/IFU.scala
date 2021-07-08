// package xiangshan.frontend
//
// import chipsalliance.rocketchip.config.Parameters
// import chisel3._
// import chisel3.util._
// import xiangshan._
// import utils._
//
// class FtqInterface(implicit p: Parameters) extends XSBundle {
//   val startAddr = UInt(64.W)
//   val endAddr   = UInt(5.W)
//   val ftqIdx    = UInt(log2Ceil(FtqSize).W)
// }
//
//
// class IFUIO(implicit p: Parameters) extends XSBundle {
//   val FtqInterface
//   val ICacheInterface
//   val toIbuffer
//
// }
//
// @chiselName
// class IFU(implicit p: Parameters) extends XSModule
// {
//   val io = IO(new IFUIO)
//   val (toFtq, fromFtq)    = (io.FtqInterface.toFtq, io.FtqInterface.fromFtq)
//   val (toMeta, toData, meta_resp, data_resp) =  (io.ICacheInterface.toMeta, io.ICacheInterface.toData, io.ICacheInterface.fromMeta, io.ICacheInterface.fromData)
//   val (toMissQueue, fromMissQueue) = (io.ICacheInterface.toMissQueue, io.ICacheInterface.fromMissQueue)
//
//   //fetch: send addr to Meta/TLB and Data simultaneously
//   val fetch_req = Seq(toMeta, toData)
//   fetch_req.map(channel =>
//     channel.valid        := fromFtq.valid
//     channel.bits.start   := fromFtq.bits.startAddr
//     channel.bits.offset  := fromFtq.bits.offset
//   )
//
//   //fetch response
//   val fetch_valid = meta_resp.valid && meta_resp.bits.hit
//   val fetch_fail  = meta_resp.valid && !meta_resp.bits.hit
//
//   io.toIbuffer.valid := fetch_valid
//   (0 until FetchWidth).map(i  => io.toIbuffer.bits.instr(i) := data_resp.bits.instr(i))
//
//   //fetch miss
//   val fetch_miss_register = RegInit(false.B)
//   fetch_miss_register.valid := fetch_fail
//
//   val fetch_miss_ftqIdx   = RegNext(fromFtq.bits.idx)
//
//   toFtq.valid := (fectch_valid) || (fetch_miss_register.valid && REFILL_VALID)
//   toFtq.entryState := Mux(fetch_valid,"fetch_resp","replay_resp")
//   toFtq.bits.replayIdx := fetch_miss_ftqIdx
//
//   toMissQueue.valid    :=  fetch_fail
//   toMissQueue.bits     :=  fetch_miss_ftq_idx
//
// }
