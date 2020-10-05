package xiangshan.mem

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.cache._
import utils.ParallelAND
import utils.TrueLRU


trait HasSbufferCst extends HasXSParameter {
  val SbufferIndexWidth: Int = log2Up(StoreBufferSize)
  // paddr = tag + offset
  val CacheLineBytes: Int = CacheLineSize / 8
  val CacheLineWords: Int = CacheLineBytes / DataBytes
  val OffsetWidth: Int = log2Up(CacheLineBytes)
  val TagWidth: Int = PAddrBits - OffsetWidth
}

class SbufferBundle extends XSBundle with HasSbufferCst

class SbufferLine extends SbufferBundle {
  val tag = UInt(TagWidth.W)
  val data = UInt(CacheLineSize.W)
  val mask = UInt(CacheLineBytes.W)
}

class NewSbuffer extends XSModule with HasSbufferCst {
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheWordReq)))
    val dcache = new DCacheLineIO
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val flush = new Bundle {
      val valid = Input(Bool())
      val empty = Output(Bool())
    } // sbuffer flush
  })

  val s_invalid :: s_valid :: s_inflight_req :: s_inflight_resp :: Nil = Enum(4)

  val buffer = Mem(StoreBufferSize, new SbufferLine)
  val stateVec = RegInit(VecInit(Seq.fill(StoreBufferSize)(s_invalid)))
  val lru = new TrueLRU(StoreBufferSize)
  // 2 * enq + 1 * deq
  val lruAccessWays = Wire(Vec(io.in.getWidth+1, new Valid(UInt(SbufferIndexWidth.W))))
  for(w <- lruAccessWays){
    w.bits := DontCare
    w.valid := false.B
  }

  /*
       idle --[flush]--> drian_sbuffer --[buf empty]--> idle
            --[buf full]--> replace --[dcache resp]--> idle
    */
  val x_idle :: x_drain_sbuffer :: x_replace :: Nil = Enum(3)
  val sbuffer_state = RegInit(x_idle)

  // ---------------------- Store Enq Sbuffer ---------------------
  // (state, lineBuf)
  type SbufferEntry = (UInt, SbufferLine)

  def getTag(pa: UInt): UInt =
    pa(PAddrBits - 1, PAddrBits - TagWidth)

  def getAddr(tag: UInt): UInt =
    Cat(tag, 0.U((PAddrBits - TagWidth).W))

  def getByteOffset(pa: UInt): UInt =
    Cat(pa(OffsetWidth - 1, 3), 0.U(3.W))

  def getWordOffset(pa: UInt): UInt = pa(OffsetWidth-1, 3)


  def witdhMap[T <: Data](f: Int => T) = (0 until StoreBufferSize) map f


  def maskData(mask: UInt, data: UInt): UInt = {
    assert(mask.getWidth * 8 == data.getWidth)
    Cat((0 until mask.getWidth).map(i => data(i*8+7, i*8) & Fill(8, mask(i))).reverse)
  }

  def wordReqToBufLine(req: DCacheWordReq): SbufferLine = {
    val bufLine = Wire(new SbufferLine)
    val wordIdx = getWordOffset(req.addr)
    val dataVec = Wire(Vec(CacheLineWords, Vec(DataBytes, UInt(8.W))))
    val mask = VecInit(Seq.fill(CacheLineWords){
      VecInit(Seq.fill(DataBytes)(false.B))
    })
    dataVec := DontCare
    for(i <- 0 until DataBytes){
      when(req.mask(i)){
        mask(wordIdx)(i) := true.B
        dataVec(wordIdx)(i) := req.data(i*8+7, i*8)
      }
    }
    bufLine.mask := mask.asUInt()
    bufLine.data := dataVec.asUInt()
    bufLine
  }

  def mergeWordReq(req: DCacheWordReq, oldLine: SbufferLine): SbufferLine = {
    val newLine = WireInit(oldLine)
    val wordIdx = getWordOffset(req.addr)
    val mask = oldLine.mask.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, Bool())))
    val data = oldLine.data.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, UInt(8.W))))
    for(i <- 0 until DataBytes){
      when(req.mask(i)){
        mask(wordIdx)(i) := true.B
        data(wordIdx)(i) := req.data(i*8+7, i*8)
      }
    }
    newLine.mask := mask.asUInt()
    newLine.data := data.asUInt()
    newLine
  }

  type ReqWithIdx = (DecoupledIO[DCacheWordReq], Int)

  def enqSbuffer(buf: Seq[SbufferEntry], reqWithIdx: ReqWithIdx): Seq[SbufferEntry] = {
    val req = reqWithIdx._1
    val reqIdx = reqWithIdx._2
    val state = VecInit(buf.map(_._1))
    val mem = VecInit(buf.map(_._2))

    def stateCanMerge(s: UInt): Bool = s===s_valid || s===s_inflight_req

    val mergeMask = witdhMap(i =>
      req.valid && stateCanMerge(state(i)) && getTag(req.bits.addr)===mem(i).tag
    )
    val canMerge = Cat(mergeMask).orR()
    val invalidMask = state.map(s => s===s_invalid)
    val notFull = Cat(invalidMask).orR()
    req.ready := notFull || canMerge
    val mergeIdx = PriorityEncoder(mergeMask)
    val insertIdx = PriorityEncoder(invalidMask)
    when(canMerge){
      mem(mergeIdx) := mergeWordReq(req.bits, mem(mergeIdx))
      lruAccessWays(reqIdx).valid := true.B
      lruAccessWays(reqIdx).bits := mergeIdx
    }.elsewhen(notFull && req.valid){
      state(insertIdx) := s_valid
      mem(insertIdx) := wordReqToBufLine(req.bits)
      lruAccessWays(reqIdx).valid := true.B
      lruAccessWays(reqIdx).bits := insertIdx
    }
    state.zip(mem)
  }

  val bufferRead = VecInit((0 until StoreBufferSize) map (i => buffer.read(i.U)))
  val initialSbuffer = stateVec.zip(bufferRead)
  val updatedSbuffer = io.in.zipWithIndex.foldLeft[Seq[SbufferEntry]](initialSbuffer)(enqSbuffer)
  val updatedState = updatedSbuffer.map(_._1)
  val updatedSbufferLine = VecInit(updatedSbuffer.map(_._2))

  for(i <- 0 until StoreBufferSize){
    buffer.write(i.U, updatedSbufferLine(i))
    stateVec(i) := updatedState(i)
  }


  // ---------------------- Send Dcache Req ---------------------

  val may_full = Cat(stateVec.map(s => s=/=s_invalid)).andR()
  val empty = Cat(stateVec.map(s => s===s_invalid)).andR() && !Cat(io.in.map(_.valid)).orR()
  val replaceIdx = lru.way
  val firstValidEntry = PriorityEncoder(stateVec.map(s => s===s_valid))

  io.flush.empty := empty
  switch(sbuffer_state){
    is(x_idle){
      when(io.flush.valid){
        sbuffer_state := x_drain_sbuffer
      }.elsewhen(may_full){
        sbuffer_state := x_replace
      }
    }
    is(x_drain_sbuffer){
      when(empty){
        sbuffer_state := x_idle
      }
    }
    is(x_replace){
      when(io.flush.valid){
        sbuffer_state := x_drain_sbuffer
      }.elsewhen(!may_full){
        sbuffer_state := x_idle
      }
    }
  }

  val evictionIdxWire = Mux(stateVec(replaceIdx)===s_valid, replaceIdx, firstValidEntry)
  val evictionIdxEnqReq = Wire(DecoupledIO(UInt(SbufferIndexWidth.W)))
  val evictionIdxQueue = Module(new Queue(UInt(SbufferIndexWidth.W), StoreBufferSize, pipe = true, flow = false))

  evictionIdxEnqReq.valid := (
    sbuffer_state===x_drain_sbuffer || sbuffer_state===x_replace
    ) && stateVec(evictionIdxWire)===s_valid

  evictionIdxEnqReq.bits := evictionIdxWire
  evictionIdxQueue.io.enq <> evictionIdxEnqReq
  assert(evictionIdxEnqReq.ready)

  when(evictionIdxEnqReq.fire()){
    stateVec(evictionIdxWire) := s_inflight_req
    lruAccessWays.last.valid := true.B
    lruAccessWays.last.bits := evictionIdxWire
  }

  // update lru
  lru.access(lruAccessWays)
  when(sbuffer_state === x_drain_sbuffer && empty){
    lru.flush()
  }


  val wbIdx = evictionIdxQueue.io.deq.bits
  val wbLine = updatedSbufferLine(wbIdx)
  io.dcache.req.valid := evictionIdxQueue.io.deq.valid
  io.dcache.req.bits.addr := getAddr(wbLine.tag)
  io.dcache.req.bits.data := wbLine.data
  io.dcache.req.bits.mask := wbLine.mask
  io.dcache.req.bits.cmd := MemoryOpConstants.M_XWR
  io.dcache.req.bits.meta := DontCare
  io.dcache.req.bits.meta.id := wbIdx
  when(io.dcache.req.fire()){ stateVec(wbIdx) := s_inflight_resp }

  // For now, dcache access is 'blocking access'
  evictionIdxQueue.io.deq.ready := io.dcache.resp.fire()
  when(io.dcache.resp.fire()){
    stateVec(wbIdx) := s_invalid
    assert(wbIdx === io.dcache.resp.bits.meta.id)
    assert(stateVec(wbIdx) === s_inflight_resp)
  }

  // ---------------------- Load Data Forward ---------------------

  def forwardQuery(forward: LoadForwardQueryIO, bufLine: SbufferLine): LoadForwardQueryIO = {
    val forwardWire = WireInit(forward)
    val forwardMask = forwardWire.forwardMask
    val forwardData = forwardWire.forwardData
    val dataVec = VecInit((0 until CacheLineBytes).map(i =>
      bufLine.data(i*8+7, i*8)
    ))

    (0 until DataBytes).map(i => {
      val lineOffset = Cat(getByteOffset(forward.paddr), i.U(3.W))
      when(bufLine.mask(lineOffset) && forward.mask(i)){
        forwardMask(i) := true.B
        forwardData(i) := dataVec(lineOffset)
      }
    })
    forwardWire
  }

  for(forward <- io.forward){
    val tag_matches = witdhMap(i => bufferRead(i).tag===getTag(forward.paddr))
    val valid_tag_matches = witdhMap(i => tag_matches(i) && stateVec(i)===s_valid)
    val inflight_tag_matches = witdhMap(i =>
      tag_matches(i) && (stateVec(i)===s_inflight_req || stateVec(i)===s_inflight_resp)
    )
    val (valid_forward_idx, valid_tag_match) = PriorityEncoderWithFlag(valid_tag_matches)
    val (inflight_forwad_idx, inflight_tag_match) = PriorityEncoderWithFlag(inflight_tag_matches)

    val valid_line = bufferRead(valid_forward_idx)
    val inflight_line = bufferRead(inflight_forwad_idx)

    val initialForward = WireInit(forward)
    initialForward.forwardMask := 0.U.asTypeOf(Vec(DataBytes, Bool()))
    initialForward.forwardData := DontCare

    val forwardResult = Seq(inflight_line, valid_line).foldLeft(initialForward)(forwardQuery)
    forward.forwardMask := forwardResult.forwardMask
    forward.forwardData := forwardResult.forwardData
  }
}

