/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.cache._
import difftest._

class SbufferFlushBundle extends Bundle {
  val valid = Output(Bool())
  val empty = Input(Bool())
}

trait HasSbufferConst extends HasXSParameter {
  val EvictCycles = 1 << 20
  val SbufferReplayDelayCycles = 16
  require(isPow2(EvictCycles))
  val EvictCountBits = log2Up(EvictCycles+1)
  val MissqReplayCountBits = log2Up(SbufferReplayDelayCycles) + 1

  val SbufferIndexWidth: Int = log2Up(StoreBufferSize)
  // paddr = ptag + offset
  val CacheLineBytes: Int = CacheLineSize / 8
  val CacheLineWords: Int = CacheLineBytes / DataBytes
  val OffsetWidth: Int = log2Up(CacheLineBytes)
  val WordsWidth: Int = log2Up(CacheLineWords)
  val PTagWidth: Int = PAddrBits - OffsetWidth
  val VTagWidth: Int = VAddrBits - OffsetWidth
  val WordOffsetWidth: Int = PAddrBits - WordsWidth
}

class SbufferEntryState (implicit p: Parameters) extends SbufferBundle {
  val state_valid    = Bool() // this entry is active
  val state_inflight = Bool() // sbuffer is trying to write this entry to dcache
  val w_timeout = Bool() // with timeout resp, waiting for resend store pipeline req timeout
  val w_sameblock_inflight = Bool() // same cache block dcache req is inflight
  val s_recheck_inflight = Bool() // recheck if same cache block dcache req is inflight

  def isInvalid(): Bool = !state_valid
  def isValid(): Bool = state_valid
  def isActive(): Bool = state_valid && !state_inflight
  def isInflight(): Bool = state_inflight
  def isDcacheReqCandidate(): Bool = state_valid && !state_inflight && !w_sameblock_inflight
}

class SbufferBundle(implicit p: Parameters) extends XSBundle with HasSbufferConst

class DataWriteReq(implicit p: Parameters) extends SbufferBundle {
  // val idx = UInt(SbufferIndexWidth.W)
  val wvec = UInt(StoreBufferSize.W)
  val mask = UInt((DataBits/8).W)
  val data = UInt(DataBits.W)
  val wordOffset = UInt(WordOffsetWidth.W)
  val wline = Bool()
}

class SbufferData(implicit p: Parameters) extends XSModule with HasSbufferConst {
  val io = IO(new Bundle(){
    val writeReq = Vec(EnsbufferWidth, Flipped(ValidIO(new DataWriteReq)))
    val dataOut = Output(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))
  })

  val data = Reg(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))

  val req = io.writeReq

  for(i <- 0 until EnsbufferWidth) {
    when(req(i).valid){
      for(line <- 0 until StoreBufferSize){
        for(word <- 0 until CacheLineWords){
          for(byte <- 0 until DataBytes){
            when(
              req(i).bits.wvec(line) && (
                req(i).bits.mask(byte) && (req(i).bits.wordOffset(WordsWidth-1, 0) === word.U) || 
                req(i).bits.wline
              )
            ){
              data(line)(word)(byte) := req(i).bits.data(byte*8+7, byte*8)
            }
          }
        }
      }
    }
  }

  io.dataOut := data
}

class Sbuffer(implicit p: Parameters) extends DCacheModule with HasSbufferConst with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    val in = Vec(EnsbufferWidth, Flipped(Decoupled(new DCacheWordReqWithVaddr)))
    val dcache = Flipped(new DCacheToSbufferIO)
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val sqempty = Input(Bool())
    val flush = Flipped(new SbufferFlushBundle)
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
  })

  val dataModule = Module(new SbufferData)
  dataModule.io.writeReq <> DontCare
  val writeReq = dataModule.io.writeReq

  val ptag = Reg(Vec(StoreBufferSize, UInt(PTagWidth.W)))
  val vtag = Reg(Vec(StoreBufferSize, UInt(VTagWidth.W)))
  val mask = Reg(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, Bool()))))
  val waitInflightMask = Reg(Vec(StoreBufferSize, UInt(StoreBufferSize.W)))
  val data = dataModule.io.dataOut
  val stateVec = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U.asTypeOf(new SbufferEntryState))))
  val cohCount = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U(EvictCountBits.W))))
  val missqReplayCount = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U(MissqReplayCountBits.W))))

  val willSendDcacheReq = Wire(Bool())

  /*
       idle --[flush]   --> drain   --[buf empty]--> idle
            --[buf full]--> replace --[dcache resp]--> idle
  */
  // x_drain_all: drain store queue and sbuffer
  // x_drain_sbuffer: drain sbuffer only, block store queue to sbuffer write
  val x_idle :: x_replace :: x_drain_all :: x_drain_sbuffer :: Nil = Enum(4)
  def needDrain(state: UInt): Bool =
    state(1)
  val sbuffer_state = RegInit(x_idle)

  // ---------------------- Store Enq Sbuffer ---------------------

  def getPTag(pa: UInt): UInt =
    pa(PAddrBits - 1, PAddrBits - PTagWidth)

  def getVTag(va: UInt): UInt =
    va(VAddrBits - 1, VAddrBits - VTagWidth)

  def getWord(pa: UInt): UInt =
    pa(PAddrBits-1, 3)

  def getWordOffset(pa: UInt): UInt =
    pa(OffsetWidth-1, 3)

  def getAddr(ptag: UInt): UInt =
    Cat(ptag, 0.U((PAddrBits - PTagWidth).W))

  def getByteOffset(offect: UInt): UInt =
    Cat(offect(OffsetWidth - 1, 3), 0.U(3.W))

  def isOneOf(key: UInt, seq: Seq[UInt]): Bool =
    if(seq.isEmpty) false.B else Cat(seq.map(_===key)).orR()

  def widthMap[T <: Data](f: Int => T) = (0 until StoreBufferSize) map f

  // sbuffer entry count

  val plru = new PseudoLRU(StoreBufferSize)
  val accessIdx = Wire(Vec(EnsbufferWidth + 1, Valid(UInt(SbufferIndexWidth.W))))

  val replaceIdx = plru.way
  plru.access(accessIdx)

  //-------------------------cohCount-----------------------------
  // insert and merge: cohCount=0
  // every cycle cohCount+=1
  // if cohCount(EvictCountBits-1)==1, evict
  val cohTimeOutMask = VecInit(widthMap(i => cohCount(i)(EvictCountBits - 1) && stateVec(i).isActive()))
  val (cohTimeOutIdx, cohHasTimeOut) = PriorityEncoderWithFlag(cohTimeOutMask)
  val missqReplayTimeOutMask = VecInit(widthMap(i => missqReplayCount(i)(MissqReplayCountBits - 1) && stateVec(i).w_timeout))
  val (missqReplayTimeOutIdx, missqReplayMayHasTimeOut) = PriorityEncoderWithFlag(missqReplayTimeOutMask)
  val missqReplayHasTimeOut = RegNext(missqReplayMayHasTimeOut) && !RegNext(willSendDcacheReq)
  val missqReplayTimeOutIdxReg = RegEnable(missqReplayTimeOutIdx, missqReplayMayHasTimeOut)

  val activeMask = VecInit(stateVec.map(s => s.isActive()))
  val drainIdx = PriorityEncoder(activeMask)

  val inflightMask = VecInit(stateVec.map(s => s.isInflight()))

  val inptags = io.in.map(in => getPTag(in.bits.addr))
  val invtags = io.in.map(in => getVTag(in.bits.vaddr))
  val sameTag = Seq.tabulate(io.in.length)(x => Seq.tabulate(io.in.length)(y => inptags(x) === inptags(y)))
  val words = (0 until EnsbufferWidth).map(i => getWord(io.in(i).bits.addr))
  val sameWord = Seq.tabulate(EnsbufferWidth)(x => Seq.tabulate(EnsbufferWidth)(y => words(x) === words(y)))

  // merge condition
  val mergeMask = Wire(Vec(EnsbufferWidth, Vec(StoreBufferSize, Bool())))
  val mergeIdx = mergeMask.map(PriorityEncoder(_)) // avoid using mergeIdx for better timing
  val canMerge = mergeMask.map(ParallelOR(_))
  val mergeVec = mergeMask.map(_.asUInt)

  for(i <- 0 until EnsbufferWidth){
    mergeMask(i) := widthMap(j =>
      inptags(i) === ptag(j) && activeMask(j)
    )
    assert(!(PopCount(mergeMask(i).asUInt) > 1.U && io.in(i).fire()))
  }

  // insert condition
  // firstInsert: the first invalid entry
  // if first entry canMerge or second entry has the same ptag with the first entry,
  // secondInsert equal the first invalid entry, otherwise, the second invalid entry
  val invalidMask = VecInit(stateVec.map(s => s.isInvalid()))
  val remInvalidMask = GetRemBits(EnsbufferWidth)(invalidMask.asUInt)

  def getFirstOneOH(input: UInt): UInt = {
    assert(input.getWidth > 1)
    val output = WireInit(VecInit(input.asBools))
    (1 until input.getWidth).map(i => {
      output(i) := !input(i - 1, 0).orR && input(i)
    })
    output.asUInt
  }

  val remRawInsertVec = remInvalidMask.map(getFirstOneOH(_))
  val remRawInsert = remInvalidMask.map(PriorityEncoderWithFlag(_)).unzip
  val (remRawInsertIdx, remCanInsert) = (remRawInsert._1, VecInit(remRawInsert._2))
  val remInsertIdx = VecInit(remRawInsertIdx.zipWithIndex.map { case (raw, idx) =>
    if (EnsbufferWidth > 1) Cat(raw, idx.U(log2Ceil(EnsbufferWidth).W))
    else raw
  }) // slow to generate, for debug only
  val remInsertVec = VecInit(GetRemBits.reverse(EnsbufferWidth)(remRawInsertVec))

  val enbufferSelReg = RegInit(0.U(log2Up(EnsbufferWidth).W))
  if (EnsbufferWidth > 1) when(io.in(0).valid) {
    enbufferSelReg := enbufferSelReg + 1.U
  }

  val insertIdxs = (0 until EnsbufferWidth).map(i =>
    PriorityMuxDefault(if (i == 0) Seq(0.B -> 0.U) else (0 until i).map(j => sameTag(i)(j) -> remInsertIdx(enbufferSelReg + j.U)), remInsertIdx(enbufferSelReg + i.U))
  ) // slow to generate, for debug only
  val insertVecs = (0 until EnsbufferWidth).map(i =>
    PriorityMuxDefault(if (i == 0) Seq(0.B -> 0.U) else (0 until i).map(j => sameTag(i)(j) -> remInsertVec(enbufferSelReg + j.U)), remInsertVec(enbufferSelReg + i.U))
  ) // slow to generate, for debug only
  val canInserts = (0 until EnsbufferWidth).map(i =>
    PriorityMuxDefault(if (i == 0) Seq(0.B -> 0.B) else (0 until i).map(j => sameTag(i)(j) -> remCanInsert(enbufferSelReg + j.U)), remCanInsert(enbufferSelReg + i.U))
  ).map(_ && sbuffer_state =/= x_drain_sbuffer)
  val forward_need_uarch_drain = WireInit(false.B)
  val merge_need_uarch_drain = WireInit(false.B)
  val do_uarch_drain = RegNext(forward_need_uarch_drain) || RegNext(RegNext(merge_need_uarch_drain))
  XSPerfAccumulate("do_uarch_drain", do_uarch_drain)

  (0 until EnsbufferWidth).foreach(i =>
    io.in(i).ready := canInserts(i) && (if (i == 0) 1.B else !sameWord(0)(i) && io.in(i - 1).ready)
  )

  def wordReqToBufLine(req: DCacheWordReq, reqptag: UInt, reqvtag: UInt, insertIdx: UInt, insertVec: UInt, wordOffset: UInt, flushMask: Bool): Unit = {
    assert(UIntToOH(insertIdx) === insertVec)
    val sameBlockInflightMask = genSameBlockInflightMask(reqptag)
    (0 until StoreBufferSize).map(entryIdx => {
      when(insertVec(entryIdx)){
        stateVec(entryIdx).state_valid := true.B
        stateVec(entryIdx).w_sameblock_inflight := sameBlockInflightMask.orR // set w_sameblock_inflight when a line is first allocated
        when(sameBlockInflightMask.orR){
          waitInflightMask(entryIdx) := sameBlockInflightMask
        }
        cohCount(entryIdx) := 0.U
        // missqReplayCount(insertIdx) := 0.U
        ptag(entryIdx) := reqptag
        vtag(entryIdx) := reqvtag // update vtag iff a new sbuffer line is allocated
        when(flushMask){
          for(j <- 0 until CacheLineWords){
            for(i <- 0 until DataBytes){
              mask(entryIdx)(j)(i) := false.B
            }
          }
        }
        for(i <- 0 until DataBytes){
          when(req.mask(i)){
            mask(entryIdx)(wordOffset)(i) := true.B
          }
        }
      }
    })
  }

  def mergeWordReq(req: DCacheWordReq, reqptag: UInt, reqvtag: UInt, mergeIdx: UInt, mergeVec: UInt, wordOffset: UInt): Unit = {
    assert(UIntToOH(mergeIdx) === mergeVec)
    (0 until StoreBufferSize).map(entryIdx => {
      when(mergeVec(entryIdx)) {
        cohCount(entryIdx) := 0.U
        // missqReplayCount(entryIdx) := 0.U
        for(i <- 0 until DataBytes){
          when(req.mask(i)){
            mask(entryIdx)(wordOffset)(i) := true.B
    //        data(entryIdx)(wordOffset)(i) := req.data(i*8+7, i*8)
          }
        }
        // check if vtag is the same, if not, trigger sbuffer flush
        when(reqvtag =/= vtag(entryIdx)) {
          XSDebug("reqvtag =/= sbufvtag req(vtag %x ptag %x) sbuffer(vtag %x ptag %x)\n",
            reqvtag << OffsetWidth,
            reqptag << OffsetWidth,
            vtag(entryIdx) << OffsetWidth,
            ptag(entryIdx) << OffsetWidth
          )
          merge_need_uarch_drain := true.B
        }
      }
    })
  }

  for(((in, wordOffset), i) <- io.in.zip(words).zipWithIndex){
    writeReq(i).valid := in.fire()
    writeReq(i).bits.wordOffset := wordOffset
    writeReq(i).bits.mask := in.bits.mask
    writeReq(i).bits.data := in.bits.data
    writeReq(i).bits.wline := in.bits.wline
    val debug_insertIdx = insertIdxs(i)
    val insertVec = insertVecs(i)
    assert(!((PopCount(insertVec) > 1.U) && in.fire()))
    val insertIdx = OHToUInt(insertVec)
    val flushMask = if(i == 0) true.B else (0 until i).map(j => !sameTag(i)(j)).reduce(_ && _)
    flushMask.suggestName(s"flushMask_${i}")
    accessIdx(i).valid := RegNext(in.fire())
    accessIdx(i).bits := RegNext(Mux(canMerge(i), mergeIdx(i), insertIdx))
    when(in.fire()){
      when(canMerge(i)){
        // writeReq(i).bits.idx := mergeIdx(i)
        writeReq(i).bits.wvec := mergeVec(i)
        mergeWordReq(in.bits, inptags(i), invtags(i), mergeIdx(i), mergeVec(i), wordOffset)
        XSDebug(p"merge req $i to line [${mergeIdx(i)}]\n")
      }.otherwise({
        // writeReq(i).bits.idx := insertIdx
        writeReq(i).bits.wvec := insertVec
        wordReqToBufLine(in.bits, inptags(i), invtags(i), insertIdx, insertVec, wordOffset, flushMask)
        XSDebug(p"insert req $i to line[$insertIdx]\n")
        assert(debug_insertIdx === insertIdx)
      })
    }
  }


  for(i <- 0 until StoreBufferSize){
    XSDebug(stateVec(i).isValid(),
      p"[$i] timeout:${cohCount(i)(EvictCountBits-1)} state:${stateVec(i)}\n"
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

  val sbuffer_empty = Cat(invalidMask).andR()
  val sq_empty = !Cat(io.in.map(_.valid)).orR()
  val empty = sbuffer_empty && sq_empty
  val threshold = RegNext(io.csrCtrl.sbuffer_threshold +& 1.U)
  val validCount = PopCount(activeMask)
  val do_eviction = RegNext(validCount >= threshold || validCount === (StoreBufferSize-1).U, init = false.B)
  require((StoreBufferThreshold + 1) <= StoreBufferSize)

  XSDebug(p"validCount[$validCount]\n")

  io.flush.empty := RegNext(empty && io.sqempty)
  // lru.io.flush := sbuffer_state === x_drain_all && empty
  switch(sbuffer_state){
    is(x_idle){
      when(io.flush.valid){
        sbuffer_state := x_drain_all
      }.elsewhen(do_uarch_drain){
        sbuffer_state := x_drain_sbuffer
      }.elsewhen(do_eviction){
        sbuffer_state := x_replace
      }
    }
    is(x_drain_all){
      when(empty){
        sbuffer_state := x_idle
      }
    }
    is(x_drain_sbuffer){
      when(io.flush.valid){
        sbuffer_state := x_drain_all
      }.elsewhen(sbuffer_empty){
        sbuffer_state := x_idle
      }
    }
    is(x_replace){
      when(io.flush.valid){
        sbuffer_state := x_drain_all
      }.elsewhen(do_uarch_drain){
        sbuffer_state := x_drain_sbuffer
      }.elsewhen(!do_eviction){
        sbuffer_state := x_idle
      }
    }
  }
  XSDebug(p"sbuffer state:${sbuffer_state} do eviction:${do_eviction} empty:${empty}\n")

  def noSameBlockInflight(idx: UInt): Bool = {
    // stateVec(idx) itself must not be s_inflight
    !Cat(widthMap(i => inflightMask(i) && ptag(idx) === ptag(i))).orR()
  }

  def genSameBlockInflightMask(ptag_in: UInt): UInt = {
    val mask = VecInit(widthMap(i => inflightMask(i) && ptag_in === ptag(i))).asUInt // quite slow, use it with care
    assert(!(PopCount(mask) > 1.U))
    mask
  }

  def haveSameBlockInflight(ptag_in: UInt): Bool = {
    genSameBlockInflightMask(ptag_in).orR
  }

  val need_drain = needDrain(sbuffer_state)
  val need_replace = do_eviction || (sbuffer_state === x_replace)
  val evictionIdx = Mux(missqReplayHasTimeOut,
    missqReplayTimeOutIdxReg,
    Mux(need_drain,
      drainIdx,
      Mux(cohHasTimeOut, cohTimeOutIdx, replaceIdx)
    )
  )
  
  /*
      If there is a inflight dcache req which has same ptag with evictionIdx's ptag,
      current eviction should be blocked.
   */
  val prepareValid = missqReplayHasTimeOut || 
    stateVec(evictionIdx).isDcacheReqCandidate() && (need_drain || cohHasTimeOut || need_replace)
  assert(!(stateVec(evictionIdx).isDcacheReqCandidate && !noSameBlockInflight(evictionIdx)))
  val prepareValidReg = RegInit(false.B)
  // when canSendDcacheReq, send dcache req stored in pipeline reg to dcache
  val canSendDcacheReq = io.dcache.req.ready || !prepareValidReg
  // when willSendDcacheReq, read dcache req data and store them in a pipeline reg 
  willSendDcacheReq := prepareValid && canSendDcacheReq
  when(io.dcache.req.fire()){
    prepareValidReg := false.B
  }
  when(canSendDcacheReq){
    prepareValidReg := prepareValid
  }
  when(willSendDcacheReq){
    stateVec(evictionIdx).state_inflight := true.B
    stateVec(evictionIdx).w_timeout := false.B
    // stateVec(evictionIdx).s_pipe_req := true.B
    XSDebug(p"$evictionIdx will be sent to Dcache\n")
  }
  XSDebug(p"need drain:$need_drain cohHasTimeOut: $cohHasTimeOut need replace:$need_replace\n")
  XSDebug(p"drainIdx:$drainIdx tIdx:$cohTimeOutIdx replIdx:$replaceIdx " +
    p"blocked:${!noSameBlockInflight(evictionIdx)} v:${activeMask(evictionIdx)}\n")
  XSDebug(p"prepareValid:$prepareValid evictIdx:$evictionIdx dcache ready:${io.dcache.req.ready}\n")
  // Note: if other dcache req in the same block are inflight,
  // the lru update may not accurate
  accessIdx(EnsbufferWidth).valid := invalidMask(replaceIdx) || (
    need_replace && !need_drain && !cohHasTimeOut && !missqReplayHasTimeOut && canSendDcacheReq && activeMask(replaceIdx))
  accessIdx(EnsbufferWidth).bits := replaceIdx
  val evictionIdxReg = RegEnable(evictionIdx, willSendDcacheReq)
  val evictionPTag = RegEnable(ptag(evictionIdx), willSendDcacheReq)
  val evictionVTag = RegEnable(vtag(evictionIdx), willSendDcacheReq)

  io.dcache.req.valid := prepareValidReg
  io.dcache.req.bits := DontCare
  io.dcache.req.bits.cmd    := MemoryOpConstants.M_XWR
  io.dcache.req.bits.addr   := getAddr(evictionPTag)
  io.dcache.req.bits.vaddr   := getAddr(evictionVTag)
  io.dcache.req.bits.data  := data(evictionIdxReg).asUInt
  io.dcache.req.bits.mask  := mask(evictionIdxReg).asUInt
  io.dcache.req.bits.id := evictionIdxReg

  when (io.dcache.req.fire()) {
    assert(!(io.dcache.req.bits.vaddr === 0.U))
    assert(!(io.dcache.req.bits.addr === 0.U))
  }

  XSDebug(io.dcache.req.fire(),
    p"send buf [$evictionIdxReg] to Dcache, req fire\n"
  )

  // update sbuffer status according to dcache resp source

  def id_to_sbuffer_id(id: UInt): UInt = {
    require(id.getWidth >= log2Up(StoreBufferSize))
    id(log2Up(StoreBufferSize)-1, 0)
  }

  // hit resp
  io.dcache.hit_resps.map(resp => {
    val dcache_resp_id = resp.bits.id
    when (resp.fire()) {
      stateVec(dcache_resp_id).state_inflight := false.B
      stateVec(dcache_resp_id).state_valid := false.B
      assert(!resp.bits.replay)
      assert(!resp.bits.miss) // not need to resp if miss, to be opted
      assert(stateVec(dcache_resp_id).state_inflight === true.B)
    }

    // Update w_sameblock_inflight flag is delayed for 1 cycle
    //
    // When a new req allocate a new line in sbuffer, sameblock_inflight check will ignore 
    // current dcache.hit_resps. Then, in the next cycle, we have plenty of time to check
    // if the same block is still inflight
    (0 until StoreBufferSize).map(i => {
      when(
        stateVec(i).w_sameblock_inflight && 
        stateVec(i).state_valid &&
        RegNext(resp.fire()) &&
        waitInflightMask(i) === UIntToOH(RegNext(id_to_sbuffer_id(dcache_resp_id)))
      ){
        stateVec(i).w_sameblock_inflight := false.B
      }
    })
  })


  // replay resp
  val replay_resp_id = io.dcache.replay_resp.bits.id
  when (io.dcache.replay_resp.fire()) {
    missqReplayCount(replay_resp_id) := 0.U
    stateVec(replay_resp_id).w_timeout := true.B
    // waiting for timeout
    assert(io.dcache.replay_resp.bits.replay)
    assert(stateVec(replay_resp_id).state_inflight === true.B)
  }
  
  // TODO: reuse cohCount
  (0 until StoreBufferSize).map(i => {
    when(stateVec(i).w_timeout && stateVec(i).state_inflight && !missqReplayCount(i)(MissqReplayCountBits-1)) {
      missqReplayCount(i) := missqReplayCount(i) + 1.U 
    }
    when(activeMask(i) && !cohTimeOutMask(i)){
      cohCount(i) := cohCount(i)+1.U
    }
  })

  if (env.EnableDifftest) {
    // hit resp
    io.dcache.hit_resps.zipWithIndex.map{case (resp, index) => {
      val difftest = Module(new DifftestSbufferEvent)
      val dcache_resp_id = resp.bits.id
      difftest.io.clock := clock
      difftest.io.coreid := io.hartId
      difftest.io.index := index.U
      difftest.io.sbufferResp := RegNext(resp.fire())
      difftest.io.sbufferAddr := RegNext(getAddr(ptag(dcache_resp_id)))
      difftest.io.sbufferData := RegNext(data(dcache_resp_id).asTypeOf(Vec(CacheLineBytes, UInt(8.W))))
      difftest.io.sbufferMask := RegNext(mask(dcache_resp_id).asUInt)
    }}
  }

  // ---------------------- Load Data Forward ---------------------
  val mismatch = Wire(Vec(LoadPipelineWidth, Bool()))
  XSPerfAccumulate("vaddr_match_failed", mismatch.reduce(_ || _))
  for ((forward, i) <- io.forward.zipWithIndex) {
    val vtag_matches = VecInit(widthMap(w => vtag(w) === getVTag(forward.vaddr)))
    val ptag_matches = VecInit(widthMap(w => ptag(w) === getPTag(forward.paddr)))
    val tag_matches = vtag_matches
    val tag_mismatch = RegNext(forward.valid) && VecInit(widthMap(w =>
      RegNext(vtag_matches(w)) =/= RegNext(ptag_matches(w)) && RegNext((activeMask(w) || inflightMask(w)))
    )).asUInt.orR
    mismatch(i) := tag_mismatch
    when (tag_mismatch) {
      XSDebug("forward tag mismatch: pmatch %x vmatch %x vaddr %x paddr %x\n",
        RegNext(ptag_matches.asUInt),
        RegNext(vtag_matches.asUInt),
        RegNext(forward.vaddr),
        RegNext(forward.paddr)
      )
      forward_need_uarch_drain := true.B
    }
    val valid_tag_matches = widthMap(w => tag_matches(w) && activeMask(w))
    val inflight_tag_matches = widthMap(w => tag_matches(w) && inflightMask(w))
    val line_offset_mask = UIntToOH(getWordOffset(forward.paddr))

    val valid_tag_match_reg = valid_tag_matches.map(RegNext(_))
    val inflight_tag_match_reg = inflight_tag_matches.map(RegNext(_))
    val line_offset_reg = RegNext(line_offset_mask)
    val forward_mask_candidate_reg = RegEnable(
      VecInit(mask.map(entry => entry(getWordOffset(forward.paddr)))),
      forward.valid
    )
    val forward_data_candidate_reg = RegEnable(
      VecInit(data.map(entry => entry(getWordOffset(forward.paddr)))),
      forward.valid
    )

    val selectedValidMask = Mux1H(valid_tag_match_reg, forward_mask_candidate_reg)
    val selectedValidData = Mux1H(valid_tag_match_reg, forward_data_candidate_reg)
    selectedValidMask.suggestName("selectedValidMask_"+i)
    selectedValidData.suggestName("selectedValidData_"+i)

    val selectedInflightMask = Mux1H(inflight_tag_match_reg, forward_mask_candidate_reg)
    val selectedInflightData = Mux1H(inflight_tag_match_reg, forward_data_candidate_reg)
    selectedInflightMask.suggestName("selectedInflightMask_"+i)
    selectedInflightData.suggestName("selectedInflightData_"+i)

    // currently not being used
    val selectedInflightMaskFast = Mux1H(line_offset_mask, Mux1H(inflight_tag_matches, mask).asTypeOf(Vec(CacheLineWords, Vec(DataBytes, Bool()))))
    val selectedValidMaskFast = Mux1H(line_offset_mask, Mux1H(valid_tag_matches, mask).asTypeOf(Vec(CacheLineWords, Vec(DataBytes, Bool()))))

    forward.dataInvalid := false.B // data in store line merge buffer is always ready
    forward.matchInvalid := tag_mismatch // paddr / vaddr cam result does not match
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

      forward.forwardMaskFast(j) := selectedInflightMaskFast(j) || selectedValidMaskFast(j)
    }
  }

  for (i <- 0 until StoreBufferSize) {
    XSDebug("sbf entry " + i + " : ptag %x vtag %x valid %x active %x inflight %x w_timeout %x\n",
      ptag(i) << OffsetWidth,
      vtag(i) << OffsetWidth,
      stateVec(i).isValid(),
      activeMask(i),
      inflightMask(i),
      stateVec(i).w_timeout
    )
  }

  val perf_valid_entry_count = PopCount(VecInit(stateVec.map(s => !s.isInvalid())).asUInt)
  XSPerfHistogram("util", perf_valid_entry_count, true.B, 0, StoreBufferSize, 1)
  XSPerfAccumulate("sbuffer_req_valid", PopCount(VecInit(io.in.map(_.valid)).asUInt))
  XSPerfAccumulate("sbuffer_req_fire", PopCount(VecInit(io.in.map(_.fire())).asUInt))
  XSPerfAccumulate("sbuffer_merge", PopCount(VecInit(io.in.zipWithIndex.map({case (in, i) => in.fire() && canMerge(i)})).asUInt))
  XSPerfAccumulate("sbuffer_newline", PopCount(VecInit(io.in.zipWithIndex.map({case (in, i) => in.fire() && !canMerge(i)})).asUInt))
  XSPerfAccumulate("dcache_req_valid", io.dcache.req.valid)
  XSPerfAccumulate("dcache_req_fire", io.dcache.req.fire())
  XSPerfAccumulate("sbuffer_idle", sbuffer_state === x_idle)
  XSPerfAccumulate("sbuffer_flush", sbuffer_state === x_drain_sbuffer)
  XSPerfAccumulate("sbuffer_replace", sbuffer_state === x_replace)
  (0 until EnsbufferWidth).foreach(i => XSPerfAccumulate(s"canInserts_${i}", canInserts(i)))
  XSPerfAccumulate("mainpipe_resp_valid", io.dcache.main_pipe_hit_resp.fire())
  XSPerfAccumulate("refill_resp_valid", io.dcache.refill_hit_resp.fire())
  XSPerfAccumulate("replay_resp_valid", io.dcache.replay_resp.fire())
  XSPerfAccumulate("coh_timeout", cohHasTimeOut)

  // val (store_latency_sample, store_latency) = TransactionLatencyCounter(io.lsu.req.fire(), io.lsu.resp.fire())
  // XSPerfHistogram("store_latency", store_latency, store_latency_sample, 0, 100, 10)
  // XSPerfAccumulate("store_req", io.lsu.req.fire())

  val perfEvents = Seq(
    ("sbuffer_req_valid ", PopCount(VecInit(io.in.map(_.valid)).asUInt)                                                                ),
    ("sbuffer_req_fire  ", PopCount(VecInit(io.in.map(_.fire())).asUInt)                                                               ),
    ("sbuffer_merge     ", PopCount(VecInit(io.in.zipWithIndex.map({case (in, i) => in.fire() && canMerge(i)})).asUInt)                ),
    ("sbuffer_newline   ", PopCount(VecInit(io.in.zipWithIndex.map({case (in, i) => in.fire() && !canMerge(i)})).asUInt)               ),
    ("dcache_req_valid  ", io.dcache.req.valid                                                                                         ),
    ("dcache_req_fire   ", io.dcache.req.fire()                                                                                        ),
    ("sbuffer_idle      ", sbuffer_state === x_idle                                                                                    ),
    ("sbuffer_flush     ", sbuffer_state === x_drain_sbuffer                                                                           ),
    ("sbuffer_replace   ", sbuffer_state === x_replace                                                                                 ),
    ("mpipe_resp_valid  ", io.dcache.main_pipe_hit_resp.fire()                                                                         ),
    ("refill_resp_valid ", io.dcache.refill_hit_resp.fire()                                                                            ),
    ("replay_resp_valid ", io.dcache.replay_resp.fire()                                                                                ),
    ("coh_timeout       ", cohHasTimeOut                                                                                               ),
    ("sbuffer_1_4_valid ", (perf_valid_entry_count < (StoreBufferSize.U/4.U))                                                          ),
    ("sbuffer_2_4_valid ", (perf_valid_entry_count > (StoreBufferSize.U/4.U)) & (perf_valid_entry_count <= (StoreBufferSize.U/2.U))    ),
    ("sbuffer_3_4_valid ", (perf_valid_entry_count > (StoreBufferSize.U/2.U)) & (perf_valid_entry_count <= (StoreBufferSize.U*3.U/4.U))),
    ("sbuffer_full_valid", (perf_valid_entry_count > (StoreBufferSize.U*3.U/4.U)))
  )
  generatePerfEvent()

}
