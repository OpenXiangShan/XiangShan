package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, TlbRequestIO, MemoryOpConstants}
import xiangshan.backend.LSUOpType
import xiangshan.mem._
import xiangshan.backend.roq.RoqPtr

class ExceptionAddrIO extends XSBundle {
  val lsIdx = Input(new LSIdx)
  val isStore = Input(Bool())
  val vaddr = Output(UInt(VAddrBits.W))
}


class LsqEntry extends XSBundle {
  val vaddr = UInt(VAddrBits.W) // TODO: need opt
  val paddr = UInt(PAddrBits.W)
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
  val exception = UInt(16.W) // TODO: opt size
  val mmio = Bool()
  val fwdMask = Vec(8, Bool())
  val fwdData = Vec(8, UInt(8.W))
}


class LSQueueData(size: Int, nchannel: Int) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val wb = Vec(nchannel, new Bundle() {
      val wen = Input(Bool())
      val index = Input(UInt(log2Up(size).W))
      val wdata = Input(new LsqEntry)
    })
    val uncache = new Bundle() {
      val wen = Input(Bool())
      val index = Input(UInt(log2Up(size).W))
      val wdata = Input(UInt(XLEN.W))
    }
    val refill = new Bundle() {
      val wen = Input(Vec(size, Bool()))
      val dcache = Input(new DCacheLineResp)
    }
    val needForward = Input(Vec(nchannel, Vec(2, UInt(size.W))))
    val forward = Vec(nchannel, Flipped(new LoadForwardQueryIO))
    val rdata = Output(Vec(size, new LsqEntry))
    
    // val debug = new Bundle() {
    //   val debug_data = Vec(LoadQueueSize, new LsqEntry)
    // }

    def wbWrite(channel: Int, index: UInt, wdata: LsqEntry): Unit = {
      require(channel < nchannel && channel >= 0)
      // need extra "this.wb(channel).wen := true.B"
      this.wb(channel).index := index
      this.wb(channel).wdata := wdata
    }

    def uncacheWrite(index: UInt, wdata: UInt): Unit = {
      // need extra "this.uncache.wen := true.B"
      this.uncache.index := index
      this.uncache.wdata := wdata
    }

    def forwardQuery(channel: Int, paddr: UInt, needForward1: Data, needForward2: Data): Unit = {
      this.needForward(channel)(0) := needForward1
      this.needForward(channel)(1) := needForward2
      this.forward(channel).paddr := paddr
    }
    
    // def refillWrite(ldIdx: Int): Unit = {
    // }
    // use "this.refill.wen(ldIdx) := true.B" instead
  })

  io := DontCare

  val data = Reg(Vec(size, new LsqEntry))

  // writeback to lq/sq
  (0 until 2).map(i => {
    when(io.wb(i).wen){
      data(io.wb(i).index) := io.wb(i).wdata
    }
  })

  when(io.uncache.wen){
    data(io.uncache.index).data := io.uncache.wdata
  }

  // refill missed load
  def mergeRefillData(refill: UInt, fwd: UInt, fwdMask: UInt): UInt = {
    val res = Wire(Vec(8, UInt(8.W)))
    (0 until 8).foreach(i => {
      res(i) := Mux(fwdMask(i), fwd(8 * (i + 1) - 1, 8 * i), refill(8 * (i + 1) - 1, 8 * i))
    })
    res.asUInt
  }

  // split dcache result into words
  val words = VecInit((0 until blockWords) map { i =>
    io.refill.dcache.data(DataBits * (i + 1) - 1, DataBits * i)
  })


  (0 until size).map(i => {
    when(io.refill.wen(i) ){
      val refillData = words(get_word(data(i).paddr))
      data(i).data := mergeRefillData(refillData, data(i).fwdData.asUInt, data(i).fwdMask.asUInt)
      XSDebug("miss resp: pos %d addr %x data %x + %x(%b)\n", i.U, data(i).paddr, refillData, data(i).fwdData.asUInt, data(i).fwdMask.asUInt)
    }
  })

  // forwarding
  // Compare ringBufferTail (deqPtr) and forward.sqIdx, we have two cases:
  // (1) if they have the same flag, we need to check range(tail, sqIdx)
  // (2) if they have different flags, we need to check range(tail, LoadQueueSize) and range(0, sqIdx)
  // Forward1: Mux(same_flag, range(tail, sqIdx), range(tail, LoadQueueSize))
  // Forward2: Mux(same_flag, 0.U,                   range(0, sqIdx)    )
  // i.e. forward1 is the target entries with the same flag bits and forward2 otherwise

  // entry with larger index should have higher priority since it's data is younger
  (0 until nchannel).map(i => {

    val forwardMask1 = WireInit(VecInit(Seq.fill(8)(false.B)))
    val forwardData1 = WireInit(VecInit(Seq.fill(8)(0.U(8.W))))
    val forwardMask2 = WireInit(VecInit(Seq.fill(8)(false.B)))
    val forwardData2 = WireInit(VecInit(Seq.fill(8)(0.U(8.W))))

    for (j <- 0 until size) {
      val needCheck = io.forward(i).paddr(PAddrBits - 1, 3) === data(j).paddr(PAddrBits - 1, 3)
      (0 until XLEN / 8).foreach(k => {
        when (needCheck && data(j).mask(k)) {
          when (io.needForward(i)(0)(j)) {
            forwardMask1(k) := true.B
            forwardData1(k) := data(j).data(8 * (k + 1) - 1, 8 * k)
          }
          when (io.needForward(i)(1)(j)) {
            forwardMask2(k) := true.B
            forwardData2(k) := data(j).data(8 * (k + 1) - 1, 8 * k)
          }
          XSDebug(io.needForward(i)(0)(j) || io.needForward(i)(1)(j),
            p"forwarding $k-th byte ${Hexadecimal(data(j).data(8 * (k + 1) - 1, 8 * k))} " +
            p"from ptr $j\n")
        }
      })
    }

    // merge forward lookup results
    // forward2 is younger than forward1 and should have higher priority
    (0 until XLEN / 8).map(k => {
      io.forward(i).forwardMask(k) := forwardMask1(k) || forwardMask2(k)
      io.forward(i).forwardData(k) := Mux(forwardMask2(k), forwardData2(k), forwardData1(k))
    })
  })

  // data read
  io.rdata := data
  // io.debug.debug_data := data
}

// inflight miss block reqs
class InflightBlockInfo extends XSBundle {
  val block_addr = UInt(PAddrBits.W)
  val valid = Bool()
}

// Load / Store Queue Wrapper for XiangShan Out of Order LSU
class LsqWrappper extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle() {
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val lsIdxs = Output(Vec(RenameWidth, new LSIdx))
    val brqRedirect = Input(Valid(new Redirect))
    val loadIn = Vec(LoadPipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheWordReq))
    val ldout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val stout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val commits = Flipped(Vec(CommitWidth, Valid(new RoqCommit)))
    val rollback = Output(Valid(new Redirect))
    val dcache = new DCacheLineIO
    val uncache = new DCacheWordIO
    val roqDeqPtr = Input(new RoqPtr)
    val oldestStore = Output(Valid(new RoqPtr))
    val exceptionAddr = new ExceptionAddrIO
  })

  val loadQueue = Module(new LoadQueue)
  val storeQueue = Module(new StoreQueue)

  // load queue wiring
  loadQueue.io.dp1Req <> io.dp1Req
  loadQueue.io.brqRedirect <> io.brqRedirect
  loadQueue.io.loadIn <> io.loadIn
  loadQueue.io.storeIn <> io.storeIn
  loadQueue.io.ldout <> io.ldout
  loadQueue.io.commits <> io.commits
  loadQueue.io.rollback <> io.rollback
  loadQueue.io.dcache <> io.dcache
  loadQueue.io.roqDeqPtr <> io.roqDeqPtr
  loadQueue.io.exceptionAddr.lsIdx := io.exceptionAddr.lsIdx
  loadQueue.io.exceptionAddr.isStore := DontCare

  // store queue wiring
  // storeQueue.io <> DontCare
  storeQueue.io.dp1Req <> io.dp1Req
  storeQueue.io.brqRedirect <> io.brqRedirect
  storeQueue.io.storeIn <> io.storeIn
  storeQueue.io.sbuffer <> io.sbuffer
  storeQueue.io.stout <> io.stout
  storeQueue.io.commits <> io.commits
  storeQueue.io.roqDeqPtr <> io.roqDeqPtr
  storeQueue.io.oldestStore <> io.oldestStore
  storeQueue.io.exceptionAddr.lsIdx := io.exceptionAddr.lsIdx
  storeQueue.io.exceptionAddr.isStore := DontCare

  loadQueue.io.forward <> io.forward
  storeQueue.io.forward <> io.forward // overlap forwardMask & forwardData, DO NOT CHANGE SEQUENCE

  io.exceptionAddr.vaddr := Mux(io.exceptionAddr.isStore, storeQueue.io.exceptionAddr.vaddr, loadQueue.io.exceptionAddr.vaddr)

  // naive uncache arbiter
  val s_idle :: s_load :: s_store :: Nil = Enum(3)
  val uncacheState = RegInit(s_idle)

  switch(uncacheState){
    is(s_idle){
      when(io.uncache.req.fire()){
        uncacheState := Mux(loadQueue.io.uncache.req.valid, s_load, s_store)
      }
    }
    is(s_load){
      when(io.uncache.resp.fire()){
        uncacheState := s_idle
      }
    }
    is(s_store){
      when(io.uncache.resp.fire()){
        uncacheState := s_idle
      }
    }
  }

  loadQueue.io.uncache := DontCare
  storeQueue.io.uncache := DontCare
  loadQueue.io.uncache.resp.valid := false.B
  storeQueue.io.uncache.resp.valid := false.B
  when(loadQueue.io.uncache.req.valid){
    io.uncache.req <> loadQueue.io.uncache.req
  }.otherwise{
    io.uncache.req <> storeQueue.io.uncache.req
  }
  when(uncacheState === s_load){
    io.uncache.resp <> loadQueue.io.uncache.resp
  }.otherwise{
    io.uncache.resp <> storeQueue.io.uncache.resp
  }

  assert(!(loadQueue.io.uncache.req.valid && storeQueue.io.uncache.req.valid))
  assert(!(loadQueue.io.uncache.resp.valid && storeQueue.io.uncache.resp.valid))
  assert(!((loadQueue.io.uncache.resp.valid || storeQueue.io.uncache.resp.valid) && uncacheState === s_idle))

  // fix valid, allocate lq / sq index
  (0 until RenameWidth).map(i => {
    val isStore = CommitType.lsInstIsStore(io.dp1Req(i).bits.ctrl.commitType)
    loadQueue.io.dp1Req(i).valid := !isStore && io.dp1Req(i).valid
    storeQueue.io.dp1Req(i).valid := isStore && io.dp1Req(i).valid
    loadQueue.io.lqIdxs(i) <> io.lsIdxs(i).lqIdx
    storeQueue.io.sqIdxs(i) <> io.lsIdxs(i).sqIdx
    loadQueue.io.lqReady <> storeQueue.io.lqReady
    loadQueue.io.sqReady <> storeQueue.io.sqReady
    io.dp1Req(i).ready := storeQueue.io.dp1Req(i).ready && loadQueue.io.dp1Req(i).ready
  })
}
