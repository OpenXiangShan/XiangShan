package xiangshan.mem

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.cache._

trait HasSbufferCst extends HasXSParameter {

  // use 1h to speedup selection
  def s_invalid  = (1<<0).U(4.W)
  def s_valid    = (1<<1).U(4.W)
  def s_prepare  = (1<<2).U(4.W)
  def s_inflight = (1<<3).U(4.W)

  def isInvalid(i: UInt): Bool = i(0).asBool
  def isValid(i: UInt): Bool = i(1).asBool
  def isPrepare(i: UInt): Bool = i(2).asBool
  def isInflight(i: UInt): Bool = i(3).asBool

  val evictCycle = 1 << 20
  require(isPow2(evictCycle))
  val countBits = log2Up(evictCycle+1)

  val SbufferIndexWidth: Int = log2Up(StoreBufferSize)
  // paddr = tag + offset
  val CacheLineBytes: Int = CacheLineSize / 8
  val CacheLineWords: Int = CacheLineBytes / DataBytes
  val OffsetWidth: Int = log2Up(CacheLineBytes)
  val WordsWidth: Int = log2Up(CacheLineWords)
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

class ChooseReplace(nWay: Int) extends XSModule {
  val io = IO(new Bundle{
    val mask = Vec(nWay, Input(Bool()))
    val way = Output(UInt(nWay.W))
    val flush = Input(Bool())
  })
  val wayReg = RegInit(0.U(log2Up(nWay).W))
  val wayMask = ~((UIntToOH(wayReg)<<1.U)(nWay-1,0) - 1.U)
  val stateMask = Cat(io.mask.reverse)
  val loMask = (wayMask & stateMask)(nWay-1,0)

  val nextWay = PriorityEncoder(Cat(stateMask, loMask))(log2Up(nWay)-1, 0)
  XSDebug(p"nextWay[${nextWay}]\n")

  wayReg := nextWay
  io.way := wayReg

  when(io.flush){
    wayReg := 0.U
  }
}

class SbufferLru(nWay: Int) extends XSModule {
  val io = IO(new Bundle{
    val in = Vec(StorePipelineWidth, Input(UInt(nWay.W)))
    val mask = Vec(StoreBufferSize, Input(Bool()))
    val way = Output(UInt(nWay.W))
    val flush = Input(Bool())
  })

  val lruRect = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U(nWay.W))))
  val count = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U(log2Up(nWay+1).W))))
  val idx = RegInit(VecInit(Seq.tabulate(StoreBufferSize)(i => i.U)))

  //update
  val updataMask = ParallelOR(io.in)
  val updateValue = (~updataMask).asUInt()
  for(i <- 0 until nWay){
    val lruUpdate = Mux(updataMask(i), updateValue, lruRect(i) & updateValue)
    lruRect(i) := lruUpdate
    count(i) := PopCount(lruUpdate)
  }

  // get evictionIdx
  val maskCount = Wire(Vec(StoreBufferSize, UInt((log2Up(1 + nWay) + log2Up(nWay)).W)))    // (popcount, Idx)
  val countZipIdx = maskCount.zip((0 until nWay).map(_.U))
  for(i <- 0 until nWay){
    val value = Mux(io.mask(i), count(i), nWay.U)
    maskCount(i) := Cat(value, idx(i))
  }

  io.way := ParallelMin(maskCount)(log2Up(nWay)-1,0)

  // flush
  when(io.flush){
    for(i <- 0 until nWay){
      lruRect(i) := 0.U
      count(i) := nWay.U
    }
    XSDebug("drain sbuffer finish, flush lru\n")
  }
}



class NewSbuffer extends XSModule with HasSbufferCst {
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheWordReq)))  //Todo: store logic only support Width == 2 now
    val dcache = new DCacheLineIO
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val sqempty = Input(Bool())
    val flush = new Bundle {
      val valid = Input(Bool())
      val empty = Output(Bool())
    } // sbuffer flush
  })
  val difftestIO = IO(new Bundle() {
    val sbufferResp = Output(Bool())
    val sbufferAddr = Output(UInt(64.W))
    val sbufferData = Output(Vec(64, UInt(8.W)))
    val sbufferMask = Output(UInt(64.W))
  })
  difftestIO <> DontCare

  val buffer = Mem(StoreBufferSize, new SbufferLine)
  val tag = Reg(Vec(StoreBufferSize, UInt(TagWidth.W)))
  val mask = Reg(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, Bool()))))
  val data = Reg(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, UInt(8.W))))) // TODO: will be replaced by SyncDataModuleTemplate
  val stateVec = RegInit(VecInit(Seq.fill(StoreBufferSize)(s_invalid)))
  val cohCount = Reg(Vec(StoreBufferSize, UInt(countBits.W)))
  /*
       idle --[flush]--> drian_sbuffer --[buf empty]--> idle
            --[buf full]--> replace --[dcache resp]--> idle
  */
  val x_idle :: x_drain_sbuffer :: x_replace :: Nil = Enum(3)
  val sbuffer_state = RegInit(x_idle)

  // ---------------------- Store Enq Sbuffer ---------------------

  def getTag(pa: UInt): UInt =
    pa(PAddrBits - 1, PAddrBits - TagWidth)

  def getWord(pa: UInt): UInt =
    pa(PAddrBits-1, 3)

  def getWordOffset(pa: UInt): UInt =
    pa(OffsetWidth-1, 3)

  def getAddr(tag: UInt): UInt =
    Cat(tag, 0.U((PAddrBits - TagWidth).W))

  def getByteOffset(offect: UInt): UInt =
    Cat(offect(OffsetWidth - 1, 3), 0.U(3.W))

  def isOneOf(key: UInt, seq: Seq[UInt]): Bool =
    if(seq.isEmpty) false.B else Cat(seq.map(_===key)).orR()

  def widthMap[T <: Data](f: Int => T) = (0 until StoreBufferSize) map f

  // sbuffer entry count
  val invalidCount = RegInit(StoreBufferSize.U((log2Up(StoreBufferSize) + 1).W))
  val validCount = RegInit(0.U((log2Up(StoreBufferSize) + 1).W))
  val full = invalidCount === 0.U // full = TODO: validCount(log2Up(StoreBufferSize))

  val lru = Module(new ChooseReplace(StoreBufferSize))
  val evictionIdx = lru.io.way

  lru.io.mask := stateVec.map(isValid(_))

  val intags = io.in.map(in => getTag(in.bits.addr))
  val sameTag = intags(0) === intags(1)
  val firstWord = getWord(io.in(0).bits.addr)
  val secondWord = getWord(io.in(1).bits.addr)
  val sameWord = firstWord === secondWord


  // merge condition
  val mergeMask = Wire(Vec(StorePipelineWidth, Vec(StoreBufferSize, Bool())))
  val mergeIdx = mergeMask.map(PriorityEncoder(_))
  val canMerge = mergeMask.map(ParallelOR(_))

  for(i <- 0 until StorePipelineWidth){
    mergeMask(i) := widthMap(j =>
      intags(i) === tag(j) && isValid(stateVec(j))
    )
  }

  // insert confition
  // firstInsert: the first invalid entry
  // if first entry canMerge or second entry has the same tag with the first entry , secondInsert equal the first invalid entry, otherwise, the second invalid entry
  val invalidMask = stateVec.map(s => isInvalid(s))
  val evenInvalidMask = GetEvenBits(VecInit(invalidMask).asUInt)
  val oddInvalidMask = GetOddBits(VecInit(invalidMask).asUInt)

  val (evenRawInsertIdx, evenCanInsert) = PriorityEncoderWithFlag(evenInvalidMask)
  val (oddRawInsertIdx, oddCanInsert) = PriorityEncoderWithFlag(oddInvalidMask)
  val evenInsertIdx = Cat(evenRawInsertIdx, 0.U(1.W))
  val oddInsertIdx = Cat(oddRawInsertIdx, 1.U(1.W))

  val enbufferSelReg = RegInit(false.B)
  when(io.in(0).valid) {
    enbufferSelReg := ~enbufferSelReg
  }

  val firstInsertIdx = Mux(enbufferSelReg, evenInsertIdx, oddInsertIdx)
  val secondInsertIdx = Mux(sameTag, 
    firstInsertIdx,
    Mux(~enbufferSelReg, evenInsertIdx, oddInsertIdx)
  )
  val firstCanInsert = Mux(enbufferSelReg, evenCanInsert, oddCanInsert)
  val secondCanInsert = Mux(sameTag,
    firstCanInsert,
    Mux(~enbufferSelReg, evenCanInsert, oddCanInsert)
  )

  io.in(0).ready := firstCanInsert
  io.in(1).ready := secondCanInsert && !sameWord && io.in(0).ready

  def wordReqToBufLine(req: DCacheWordReq, reqtag: UInt, insertIdx: UInt, wordOffset: UInt, flushMask: Bool): Unit = {
    stateVec(insertIdx) := s_valid
    cohCount(insertIdx) := 0.U
    tag(insertIdx) := reqtag

    when(flushMask){
      for(j <- 0 until CacheLineWords){
        for(i <- 0 until DataBytes){
          mask(insertIdx)(j)(i) := false.B
        }
      }
    }

    for(i <- 0 until DataBytes){
      when(req.mask(i)){
        mask(insertIdx)(wordOffset)(i) := true.B
        data(insertIdx)(wordOffset)(i) := req.data(i*8+7, i*8)
      }
    }
  }

  def mergeWordReq(req: DCacheWordReq, mergeIdx:UInt, wordOffset:UInt): Unit = {
    cohCount(mergeIdx) := 0.U
    for(i <- 0 until DataBytes){
      when(req.mask(i)){
        mask(mergeIdx)(wordOffset)(i) := true.B
        data(mergeIdx)(wordOffset)(i) := req.data(i*8+7, i*8)
      }
    }
  }

  // first store
  when(io.in(0).fire()){
    when(canMerge(0)){
      mergeWordReq(io.in(0).bits, mergeIdx(0), firstWord)
      XSDebug(p"merge req 0 to line [${mergeIdx(0)}]\n")
    }.otherwise{
      wordReqToBufLine(io.in(0).bits, intags(0), firstInsertIdx, firstWord, true.B)
      XSDebug(p"insert req 0 to line[$firstInsertIdx]\n")
    }
  }

  // second store
  when(io.in(1).fire()){
    when(canMerge(1)){
      mergeWordReq(io.in(1).bits, mergeIdx(1), secondWord)
      XSDebug(p"merge req 1 to line [${mergeIdx(1)}]\n")
    }.otherwise{
      wordReqToBufLine(io.in(1).bits, intags(1), secondInsertIdx, secondWord, !sameTag)
      XSDebug(p"insert req 1 to line[$secondInsertIdx]\n")
    }
  }

  for(i <- 0 until StoreBufferSize){
    XSDebug(stateVec(i)=/=s_invalid,
      p"[$i] timeout:${cohCount(i)(countBits-1)} state:${stateVec(i)}\n"
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
  val empty = Cat(stateVec.map(s => isInvalid(s))).andR() && !Cat(io.in.map(_.valid)).orR()

  do_eviction := validCount >= 12.U

  io.flush.empty := RegNext(empty && io.sqempty)
  lru.io.flush := sbuffer_state === x_drain_sbuffer && empty
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

  def noSameBlockInflight(idx: UInt): Bool = {
    val atag = tag(idx)
    !Cat(widthMap(i => {
      // stateVec(idx) itself must not be s_inflight*
      (isInflight(stateVec(i)) || isPrepare(stateVec(i))) &&
        atag === tag(i)
    })).orR()
  }

  /*
      If there is a inflight dcache req which has same tag with evictionIdx's tag,
      current eviction should be blocked.
   */
//  val evictionEntry = Wire(DecoupledIO(UInt(SbufferIndexWidth.W)))
//
//  evictionEntry.valid :=
//    do_eviction && sbuffer_state === x_replace || sbuffer_state === x_drain_sbuffer &&
//      stateVec(evictionIdx)===s_valid &&
//      noSameBlockInflight(evictionIdx)
//
//  evictionEntry.bits := evictionIdx

  val prepareValid = ((do_eviction && sbuffer_state === x_replace) || (sbuffer_state === x_drain_sbuffer)) &&
                      isValid(stateVec(evictionIdx)) &&
                      noSameBlockInflight(evictionIdx)

  when(prepareValid){
    stateVec(evictionIdx) := s_prepare
  }

  val prepareMask = stateVec.map(s => isPrepare(s))
  val (prepareIdx, prepareEn) = PriorityEncoderWithFlag(prepareMask)

  val dcacheReqValid = RegInit(false.B)
  val dcacheCandidate = Reg(new DCacheLineReq)
  when(io.dcache.req.fire()){
    dcacheReqValid := false.B
  }
  when(prepareEn && (!dcacheReqValid || io.dcache.req.fire())) {
    dcacheCandidate.addr := getAddr(tag(prepareIdx))
    dcacheCandidate.data := data(prepareIdx).asUInt
    dcacheCandidate.mask := mask(prepareIdx).asUInt
    dcacheCandidate.cmd := MemoryOpConstants.M_XWR
    dcacheCandidate.id := prepareIdx
    stateVec(prepareIdx) := s_inflight
    dcacheReqValid := true.B
  }

  io.dcache.req.valid := dcacheReqValid
  io.dcache.req.bits := dcacheCandidate
//  evictionEntry.ready := io.dcache.req.ready

  XSDebug(io.dcache.req.fire(),
    p"send buf [$prepareIdx] to Dcache, req fire\n"
  )

  io.dcache.resp.ready := true.B // sbuffer always ready to recv dcache resp
  val respId = io.dcache.resp.bits.id
  when(io.dcache.resp.fire()){
    stateVec(respId) := s_invalid
    assert(stateVec(respId) === s_inflight)
    XSDebug(p"recv cache resp: id=[$respId]\n")
  }

  if (!env.FPGAPlatform) {
    difftestIO.sbufferResp := WireInit(io.dcache.resp.fire())
    difftestIO.sbufferAddr := WireInit(getAddr(tag(respId)))
    difftestIO.sbufferData := WireInit(data(respId).asTypeOf(Vec(CacheLineBytes, UInt(8.W))))
    difftestIO.sbufferMask := WireInit(mask(respId).asUInt)
  }

  val needSpace = (io.in(0).fire && !canMerge(0)) +& (io.in(1).fire && !canMerge(1) && !sameTag)
  invalidCount := invalidCount - needSpace + io.dcache.resp.fire()
  validCount := validCount + needSpace - prepareValid

  XSDebug(p"needSpace[$needSpace] invalidCount[$invalidCount]  validCount[$validCount]\n")


  //-------------------------cohCount-----------------------------
  // insert and merge: cohCount=0
  // every cycle cohCount+=1
  // if cohCount(countBits-1)==1,evict
  for(i <- 0 until StoreBufferSize){
    when(isValid(stateVec(i))){
      when(cohCount(i)(countBits-1)){
        assert(stateVec(i) === s_valid)
        stateVec(i) := s_prepare
      }
      cohCount(i) := cohCount(i)+1.U
    }
  }

  // ---------------------- Load Data Forward ---------------------

  for ((forward, i) <- io.forward.zipWithIndex) {
    val tag_matches = widthMap(i => tag(i) === getTag(forward.paddr))
    val valid_tag_matches = widthMap(i => tag_matches(i) && isValid(stateVec(i)))
    val inflight_tag_matches = widthMap(i =>
      tag_matches(i) && (isInflight(stateVec(i)) || isPrepare(stateVec(i)))
    )
    val line_offset_mask = UIntToOH(getWordOffset(forward.paddr))

    val valid_tag_match_reg = valid_tag_matches.map(RegNext(_))
    val inflight_tag_match_reg = inflight_tag_matches.map(RegNext(_))
    val line_offset_reg = RegNext(line_offset_mask)

    val selectedValidMask = Mux1H(line_offset_reg, Mux1H(valid_tag_match_reg, mask).asTypeOf(Vec(CacheLineWords, Vec(DataBytes, Bool()))))
    val selectedValidData = Mux1H(line_offset_reg, Mux1H(valid_tag_match_reg, data).asTypeOf(Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))

    val selectedInflightMask = Mux1H(line_offset_reg, Mux1H(inflight_tag_match_reg, mask).asTypeOf(Vec(CacheLineWords, Vec(DataBytes, Bool()))))
    val selectedInflightData = Mux1H(line_offset_reg, Mux1H(inflight_tag_match_reg, data).asTypeOf(Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))

    for (j <- 0 until DataBytes) {
      forward.forwardMask(j) := false.B
      forward.forwardData(j) := DontCare

      // valid entries have higher priority than inflight entries
      when(selectedInflightMask(j)) {
        forward.forwardMask(j) := true.B
        forward.forwardData(j) := selectedInflightData(j)
      }
      when(selectedValidMask(j)) {
        forward.forwardMask(j) := true.B
        forward.forwardData(j) := selectedValidData(j)
      }
    }
  }
}

object NewSbuffer extends App {
  override def main(args: Array[String]): Unit = {
    chisel3.Driver.execute(args, ()=> new NewSbuffer)
  }
}
