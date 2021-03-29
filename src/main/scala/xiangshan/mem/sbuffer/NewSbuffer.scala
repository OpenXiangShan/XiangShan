package xiangshan.mem

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.cache._

class SbufferFlushBundle extends Bundle {
  val valid = Output(Bool())
  val empty = Input(Bool())
}

trait HasSbufferConst extends HasXSParameter {

  // use 1h to speedup selection
  def s_invalid  = (1<<0).U(3.W)
  def s_valid    = (1<<1).U(3.W)
  def s_inflight = (1<<2).U(3.W)

  def isInvalid(i: UInt): Bool = i(0).asBool
  def isValid(i: UInt): Bool = i(1).asBool
  def isInflight(i: UInt): Bool = i(2).asBool

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
  val WordOffsetWidth: Int = PAddrBits - WordsWidth
}

class SbufferBundle extends XSBundle with HasSbufferConst

class DataWriteReq extends SbufferBundle {
  val idx = UInt(SbufferIndexWidth.W)
  val mask = UInt((DataBits/8).W)
  val data = UInt(DataBits.W)
  val wordOffset = UInt(WordOffsetWidth.W)
}

class SbufferData extends XSModule with HasSbufferConst {
  val io = IO(new Bundle(){
    val writeReq = Vec(StorePipelineWidth, Flipped(ValidIO(new DataWriteReq)))
    val dataOut = Output(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))
  })

  val data = Reg(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))

  val req = io.writeReq

  for(i <- 0 until StorePipelineWidth) {
    when(req(i).valid){
      for(j <- 0 until DataBytes){
        when(req(i).bits.mask(j)){
          data(req(i).bits.idx)(req(i).bits.wordOffset)(j) := req(i).bits.data(j*8+7, j*8)
        }
      }
    }
  }

  io.dataOut := data
}

class NewSbuffer extends XSModule with HasSbufferConst {
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheWordReq)))  //Todo: store logic only support Width == 2 now
    val dcache = new DCacheLineIO
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val sqempty = Input(Bool())
    val flush = Flipped(new SbufferFlushBundle)
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
  })
  val difftestIO = IO(new Bundle() {
    val sbufferResp = Output(Bool())
    val sbufferAddr = Output(UInt(64.W))
    val sbufferData = Output(Vec(64, UInt(8.W)))
    val sbufferMask = Output(UInt(64.W))
  })
  difftestIO <> DontCare

  val dataModule = Module(new SbufferData)
  dataModule.io.writeReq <> DontCare
  val writeReq = dataModule.io.writeReq

  val tag = Reg(Vec(StoreBufferSize, UInt(TagWidth.W)))
  val mask = Reg(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, Bool()))))
  val data = dataModule.io.dataOut
  val stateVec = RegInit(VecInit(Seq.fill(StoreBufferSize)(s_invalid)))
  val cohCount = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U(countBits.W))))

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

  val plru = new PseudoLRU(StoreBufferSize)
  val accessIdx = Wire(Vec(StorePipelineWidth + 1, Valid(UInt(SbufferIndexWidth.W))))

  val replaceIdx = plru.way
  plru.access(accessIdx)

  //-------------------------cohCount-----------------------------
  // insert and merge: cohCount=0
  // every cycle cohCount+=1
  // if cohCount(countBits-1)==1, evict
  val timeOutMask = VecInit(widthMap(i => cohCount(i)(countBits - 1)))
  val (timeOutIdx, hasTimeOut) = PriorityEncoderWithFlag(timeOutMask)

  val validMask = VecInit(stateVec.map(s => isValid(s)))
  val drainIdx = PriorityEncoder(validMask)

  val inflightMask = VecInit(stateVec.map(s => isInflight(s)))

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
      intags(i) === tag(j) && validMask(j)
    )
  }

  // insert condition
  // firstInsert: the first invalid entry
  // if first entry canMerge or second entry has the same tag with the first entry,
  // secondInsert equal the first invalid entry, otherwise, the second invalid entry
  val invalidMask = VecInit(stateVec.map(s => isInvalid(s)))
  val evenInvalidMask = GetEvenBits(invalidMask.asUInt)
  val oddInvalidMask = GetOddBits(invalidMask.asUInt)

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
//        data(insertIdx)(wordOffset)(i) := req.data(i*8+7, i*8)
      }
    }
  }

  def mergeWordReq(req: DCacheWordReq, mergeIdx:UInt, wordOffset:UInt): Unit = {
    cohCount(mergeIdx) := 0.U
    for(i <- 0 until DataBytes){
      when(req.mask(i)){
        mask(mergeIdx)(wordOffset)(i) := true.B
//        data(mergeIdx)(wordOffset)(i) := req.data(i*8+7, i*8)
      }
    }
  }

  for(((in, wordOffset), i) <- io.in.zip(Seq(firstWord, secondWord)).zipWithIndex){
    writeReq(i).valid := in.fire()
    writeReq(i).bits.wordOffset := wordOffset
    writeReq(i).bits.mask := in.bits.mask
    writeReq(i).bits.data := in.bits.data
    val insertIdx = if(i == 0) firstInsertIdx else secondInsertIdx
    val flushMask = if(i == 0) true.B else !sameTag
    accessIdx(i).valid := RegNext(in.fire())
    accessIdx(i).bits := RegNext(Mux(canMerge(i), mergeIdx(i), insertIdx))
    when(in.fire()){
      when(canMerge(i)){
        writeReq(i).bits.idx := mergeIdx(i)
        mergeWordReq(in.bits, mergeIdx(i), wordOffset)
        XSDebug(p"merge req $i to line [${mergeIdx(i)}]\n")
      }.otherwise({
        writeReq(i).bits.idx := insertIdx
        wordReqToBufLine(in.bits, intags(i), insertIdx, wordOffset, flushMask)
        XSDebug(p"insert req $i to line[$insertIdx]\n")
      })
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

  val empty = Cat(invalidMask).andR() && !Cat(io.in.map(_.valid)).orR()
  val threshold = RegNext(io.csrCtrl.sbuffer_threshold +& 1.U)
  val validCount = PopCount(validMask)
  val do_eviction = RegNext(validCount >= threshold, init = false.B)

  XSDebug(p"validCount[$validCount]\n")

  io.flush.empty := RegNext(empty && io.sqempty)
  // lru.io.flush := sbuffer_state === x_drain_sbuffer && empty
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
    // stateVec(idx) itself must not be s_inflight
    !Cat(widthMap(i => inflightMask(i) && tag(idx) === tag(i))).orR()
  }

  val need_drain = sbuffer_state === x_drain_sbuffer
  val need_replace = do_eviction || (sbuffer_state === x_replace)
  val evictionIdx = Mux(need_drain,
    drainIdx,
    Mux(hasTimeOut, timeOutIdx, replaceIdx)
  )
  /*
      If there is a inflight dcache req which has same tag with evictionIdx's tag,
      current eviction should be blocked.
   */
  val prepareValid = (need_drain || hasTimeOut || need_replace) &&
    noSameBlockInflight(evictionIdx) && validMask(evictionIdx)
  val prepareValidReg = RegInit(false.B)
  val canSendDcacheReq = io.dcache.req.ready || !prepareValidReg
  val willSendDcacheReq = prepareValid && canSendDcacheReq
  when(io.dcache.req.fire()){
    prepareValidReg := false.B
  }
  when(canSendDcacheReq){
    prepareValidReg := prepareValid
  }
  when(willSendDcacheReq){
    stateVec(evictionIdx) := s_inflight
    XSDebug(p"$evictionIdx will be sent to Dcache\n")
  }
  XSDebug(p"need drain:$need_drain hasTimeOut: $hasTimeOut need replace:$need_replace\n")
  XSDebug(p"drainIdx:$drainIdx tIdx:$timeOutIdx replIdx:$replaceIdx " +
    p"blocked:${!noSameBlockInflight(evictionIdx)} v:${validMask(evictionIdx)}\n")
  XSDebug(p"prepareValid:$prepareValid evictIdx:$evictionIdx dcache ready:${io.dcache.req.ready}\n")
  // Note: if other dcache req in the same block are inflight,
  // the lru update may note accurate
  accessIdx(StorePipelineWidth).valid := invalidMask(replaceIdx) || (
    need_replace && !need_drain && !hasTimeOut && canSendDcacheReq && validMask(replaceIdx))
  accessIdx(StorePipelineWidth).bits := replaceIdx
  val evictionIdxReg = RegEnable(evictionIdx, enable = willSendDcacheReq)
  val evictionTag = RegEnable(tag(evictionIdx), enable = willSendDcacheReq)

  io.dcache.req.valid := prepareValidReg
  io.dcache.req.bits.addr := getAddr(evictionTag)
  io.dcache.req.bits.data := data(evictionIdxReg).asUInt
  io.dcache.req.bits.mask := mask(evictionIdxReg).asUInt
  io.dcache.req.bits.cmd := MemoryOpConstants.M_XWR
  io.dcache.req.bits.id := evictionIdxReg

  XSDebug(io.dcache.req.fire(),
    p"send buf [$evictionIdxReg] to Dcache, req fire\n"
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

  for(i <- 0 until StoreBufferSize){
    when(validMask(i) && !timeOutMask(i)){
      cohCount(i) := cohCount(i)+1.U
    }
  }

  // ---------------------- Load Data Forward ---------------------

  for ((forward, i) <- io.forward.zipWithIndex) {
    val tag_matches = widthMap(w => tag(w) === getTag(forward.paddr))
    val valid_tag_matches = widthMap(w => tag_matches(w) && validMask(w))
    val inflight_tag_matches = widthMap(w => tag_matches(w) && inflightMask(w))
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
