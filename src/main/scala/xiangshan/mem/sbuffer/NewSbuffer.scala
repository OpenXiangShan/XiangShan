package xiangshan.mem

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.cache._
import utils.ParallelAND
import utils.TrueLRU


trait HasSbufferCst extends HasXSParameter {
  // def s_invalid :: s_valid :: s_inflight_req :: s_inflight_resp :: Nil = Enum(4)

  def s_invalid = 0.U(2.W)
  def s_valid = 1.U(2.W)
  def s_inflight_req = 2.U(2.W)
  def s_inflight_resp = 3.U(2.W)

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

  override def toPrintable: Printable = {
    p"tag:${Hexadecimal(tag)} data:${Hexadecimal(data)} mask:${Binary(mask)}\n"
  }
}

class AbstractEvictor extends XSModule with HasSbufferCst{
  val io = IO(new Bundle{
    val states = Input(Vec(StoreBufferSize, UInt(s_invalid.getWidth.W)))
    val do_eviction = Output(Bool())
  })
}


class NaiveEvictor(threshold: Int) extends AbstractEvictor{

  require(threshold >= 0 && threshold <= StoreBufferSize)

  val entryCnt = PopCount(io.states.map(s => s=/=s_invalid))

  io.do_eviction := entryCnt >= threshold.U((SbufferIndexWidth+1).W)

  XSDebug("sbuffer entry cnt: %d\n", entryCnt)

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

  val buffer = Mem(StoreBufferSize, new SbufferLine)
  val stateVec = RegInit(VecInit(Seq.fill(StoreBufferSize)(s_invalid)))
  //val lru = new SbufferLRU(StoreBufferSize)
  val lru = new SbufferLRU(StoreBufferSize)
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

  def isOneOf(key: UInt, seq: Seq[UInt]): Bool =
    if(seq.isEmpty) false.B else Cat(seq.map(_===key)).orR()

  def widthMap[T <: Data](f: Int => T) = (0 until StoreBufferSize) map f


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
    bufLine.tag := getTag(req.addr)
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
    val state_old = VecInit(buf.map(_._1))
    val mem_old = VecInit(buf.map(_._2))
    val state_new = WireInit(state_old)
    val mem_new = WireInit(mem_old)

    def stateCanMerge(s: UInt): Bool = isOneOf(s, Seq(s_valid, s_inflight_req))

    val mergeMask = widthMap(i =>
      req.valid && stateCanMerge(state_old(i)) && getTag(req.bits.addr)===mem_old(i).tag
    )
    val canMerge = Cat(mergeMask).orR()
    val invalidMask = state_old.map(s => s===s_invalid)
    val notFull = Cat(invalidMask).orR()
    req.ready := notFull || canMerge
    val mergeIdx = PriorityEncoder(mergeMask)
    val insertIdx = PriorityEncoder(invalidMask)
    when(canMerge){
      mem_new(mergeIdx) := mergeWordReq(req.bits, mem_old(mergeIdx))
      lruAccessWays(reqIdx).valid := true.B
      lruAccessWays(reqIdx).bits := mergeIdx
      XSDebug(p"merge req $reqIdx to line [$mergeIdx]\n")
    }.elsewhen(notFull && req.valid){
      state_new(insertIdx) := s_valid
      mem_new(insertIdx) := wordReqToBufLine(req.bits)
      lruAccessWays(reqIdx).valid := true.B
      lruAccessWays(reqIdx).bits := insertIdx
      XSDebug(p"insert req $reqIdx to line[$insertIdx]\n")
    }
    state_new.zip(mem_new)
  }

  val bufferRead = VecInit((0 until StoreBufferSize) map (i => buffer(i)))
  val initialSbuffer = stateVec.zip(bufferRead)
  val updatedSbuffer = io.in.zipWithIndex.foldLeft[Seq[SbufferEntry]](initialSbuffer)(enqSbuffer)
  val updatedState = updatedSbuffer.map(_._1)
  val updatedSbufferLine = VecInit(updatedSbuffer.map(_._2))
  when (!io.in(0).ready) {
    io.in(1).ready := false.B
  }

  for(i <- 0 until StoreBufferSize){
    buffer.write(i.U, updatedSbufferLine(i))
    stateVec(i) := updatedState(i)
  }

  for(i <- 0 until StoreBufferSize){
    XSDebug(stateVec(i)=/=s_invalid,
      p"[$i] state:${stateVec(i)} buf:${bufferRead(i)}\n"
    )
  }

  for((req, i) <- io.in.zipWithIndex){
    XSDebug(req.fire(),
      p"accept req [$i]: " +
        p"addr:${Hexadecimal(req.bits.addr)} " +
        p"mask:${Binary(req.bits.mask)} " +
        p"data:${Hexadecimal(req.bits.data)}\n"
    )
    XSDebug(req.valid && !req.ready,
      p"req [$i] blocked by sbuffer\n"
    )
  }


  // ---------------------- Send Dcache Req ---------------------

  val do_eviction = Wire(Bool())
  val empty = Cat(stateVec.map(s => s===s_invalid)).andR() && !Cat(io.in.map(_.valid)).orR()
  val replaceIdx = lru.way(stateVec.map(s => s===s_valid))
  val firstValidEntry = PriorityEncoder(stateVec.map(s => s===s_valid))

  val evictor = Module(new NaiveEvictor(StoreBufferSize-4))
  evictor.io.states := stateVec
  do_eviction := evictor.io.do_eviction

  io.flush.empty := empty
  switch(sbuffer_state){
    is(x_idle){
      when(io.flush.valid){
        sbuffer_state := x_drain_sbuffer
      }.elsewhen(do_eviction){
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
      }.elsewhen(!do_eviction){
        sbuffer_state := x_idle
      }
    }
  }
  XSDebug(p"sbuffer state:${sbuffer_state} do eviction:${do_eviction} empty:${empty}\n")

  //XSDebug(p"replaceIdx:${replaceIdx}\n")
  //val evictionIdxWire = replaceIdx
  val evictionIdxWire = Mux(stateVec(replaceIdx)===s_valid, replaceIdx, firstValidEntry)
  val evictionIdxEnqReq = Wire(DecoupledIO(UInt(SbufferIndexWidth.W)))
  val evictionIdxQueue = Module(new Queue(UInt(SbufferIndexWidth.W), StoreBufferSize, pipe = true, flow = false))

  def noSameBlockInflight(idx: UInt): Bool = {
    val tag = updatedSbufferLine(idx).tag
    !Cat(widthMap(i => {
      // stateVec(idx) itself must not be s_inflight*
      isOneOf(stateVec(i), Seq(s_inflight_req, s_inflight_resp)) &&
        tag===updatedSbufferLine(i).tag
    })).orR()
  }

  /*
      If there is a inflight dcache req which has same tag with evictionIdx's tag,
      current eviction should be blocked.
   */
  evictionIdxEnqReq.valid :=
    isOneOf(sbuffer_state, Seq(x_drain_sbuffer, x_replace)) &&
      stateVec(evictionIdxWire)===s_valid &&
      noSameBlockInflight(evictionIdxWire)

  evictionIdxEnqReq.bits := evictionIdxWire
  evictionIdxQueue.io.enq <> evictionIdxEnqReq

  when(evictionIdxEnqReq.fire()){
    stateVec(evictionIdxWire) := s_inflight_req
    lruAccessWays.last.valid := true.B
    lruAccessWays.last.bits := evictionIdxWire
  }

  // update lru
  lru.access(lruAccessWays)
  when(sbuffer_state === x_drain_sbuffer && empty){
    lru.flush()
    XSDebug("drain sbuffer finish, flush lru\n")
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
  evictionIdxQueue.io.deq.ready := io.dcache.req.ready

  XSDebug(io.dcache.req.fire(),
    p"send buf [$wbIdx] to Dcache, req fire\n"
  )

  io.dcache.resp.ready := true.B // sbuffer always ready to recv dcache resp
  val respId = io.dcache.resp.bits.meta.id
  when(io.dcache.resp.fire()){
    stateVec(respId) := s_invalid
    assert(stateVec(respId) === s_inflight_resp)
    XSDebug(p"recv cache resp: id=[$respId]\n")
  }

  // ---------------------- Load Data Forward ---------------------

  for ((forward, i) <- io.forward.zipWithIndex) {
    val tag_matches = widthMap(i => bufferRead(i).tag===getTag(forward.paddr))
    val valid_tag_matches = widthMap(i => tag_matches(i) && stateVec(i)===s_valid)
    val inflight_tag_matches = widthMap(i =>
      tag_matches(i) && (stateVec(i)===s_inflight_req || stateVec(i)===s_inflight_resp)
    )
    val line_offset_mask = UIntToOH(getWordOffset(forward.paddr))

    val valid_tag_match_reg = valid_tag_matches.map(RegNext(_))
    val inflight_tag_match_reg = inflight_tag_matches.map(RegNext(_))
    val line_offset_reg = RegNext(line_offset_mask)

    val selectedValidLine = Mux1H(valid_tag_match_reg, bufferRead)
    val selectedValidMask = Mux1H(line_offset_reg, selectedValidLine.mask.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, Bool()))))
    val selectedValidData = Mux1H(line_offset_reg, selectedValidLine.data.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))

    val selectedInflightLine = Mux1H(inflight_tag_match_reg, bufferRead)
    val selectedInflightMask = Mux1H(line_offset_reg, selectedInflightLine.mask.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, Bool()))))
    val selectedInflightData = Mux1H(line_offset_reg, selectedInflightLine.data.asTypeOf(Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))

    for (j <- 0 until DataBytes) {
      forward.forwardMask(j) := false.B
      forward.forwardData(j) := DontCare

      // valid entries have higher priority than inflight entries
      when (selectedInflightMask(j)) {
        forward.forwardMask(j) := true.B
        forward.forwardData(j) := selectedInflightData(j)
      }
      when (selectedValidMask(j)) {
        forward.forwardMask(j) := true.B
        forward.forwardData(j) := selectedValidData(j)
      }
    }

    XSDebug(Cat(inflight_tag_matches).orR || Cat(valid_tag_matches).orR,
      p"[$i] forward paddr:${Hexadecimal(forward.paddr)}\n"
    )
  }
}

object NewSbuffer extends App {
  override def main(args: Array[String]): Unit = {
    chisel3.Driver.execute(args, ()=> new NewSbuffer)
  }
}
