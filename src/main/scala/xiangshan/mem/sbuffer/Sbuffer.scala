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
import freechips.rocketchip.util._

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

  // dcache write hit resp has 2 sources
  // refill pipe resp and main pipe resp
  val NumDcacheWriteResp = 2 // hardcoded

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

  def isInvalid(): Bool = !state_valid
  def isValid(): Bool = state_valid
  def isActive(): Bool = state_valid && !state_inflight
  def isInflight(): Bool = state_inflight
  def isDcacheReqCandidate(): Bool = state_valid && !state_inflight && !w_sameblock_inflight
}

class SbufferBundle(implicit p: Parameters) extends XSBundle with HasSbufferConst

class DataWriteReq(implicit p: Parameters) extends SbufferBundle {
  // univerisal writemask
  val wvec = UInt(StoreBufferSize.W)
  // 2 cycle update
  val mask = UInt((DataBits/8).W)
  val data = UInt(DataBits.W)
  val wordOffset = UInt(WordOffsetWidth.W)
  val wline = Bool() // write full cacheline
}

class MaskFlushReq(implicit p: Parameters) extends SbufferBundle {
  // univerisal writemask
  val wvec = UInt(StoreBufferSize.W)
}

class SbufferData(implicit p: Parameters) extends XSModule with HasSbufferConst {
  val io = IO(new Bundle(){
    // update data and mask when alloc or merge
    val writeReq = Vec(StorePipelineWidth, Flipped(ValidIO(new DataWriteReq)))
    // clean mask when deq
    val maskFlushReq = Vec(NumDcacheWriteResp, Flipped(ValidIO(new MaskFlushReq)))
    val dataOut = Output(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))
    val maskOut = Output(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, Bool()))))
  })

  val data = Reg(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, UInt(8.W)))))
  // val mask = Reg(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, Bool()))))
  val mask = RegInit(
    VecInit(Seq.fill(StoreBufferSize)(
      VecInit(Seq.fill(CacheLineWords)(
        VecInit(Seq.fill(DataBytes)(false.B))
      ))
    ))
  )

  // 2 cycle line mask clean
  for(line <- 0 until StoreBufferSize){
    val line_mask_clean_flag = RegNext(
      io.maskFlushReq.map(a => a.valid && a.bits.wvec(line)).reduce(_ || _)
    )
    line_mask_clean_flag.suggestName("line_mask_clean_flag_"+line)
    when(line_mask_clean_flag){
      for(word <- 0 until CacheLineWords){
        for(byte <- 0 until DataBytes){
          mask(line)(word)(byte) := false.B
        }
      }
    }
  }

  // 2 cycle data / mask update
  for(i <- 0 until StorePipelineWidth) {
    val req = io.writeReq(i)
    for(line <- 0 until StoreBufferSize){
      val sbuffer_in_s1_line_wen = req.valid && req.bits.wvec(line)
      val sbuffer_in_s2_line_wen = RegNext(sbuffer_in_s1_line_wen)
      val line_write_buffer_data = RegEnable(req.bits.data, sbuffer_in_s1_line_wen)
      val line_write_buffer_wline = RegEnable(req.bits.wline, sbuffer_in_s1_line_wen)
      val line_write_buffer_mask = RegEnable(req.bits.mask, sbuffer_in_s1_line_wen)
      val line_write_buffer_offset = RegEnable(req.bits.wordOffset(WordsWidth-1, 0), sbuffer_in_s1_line_wen)
      sbuffer_in_s1_line_wen.suggestName("sbuffer_in_s1_line_wen_"+line)
      sbuffer_in_s2_line_wen.suggestName("sbuffer_in_s2_line_wen_"+line)
      line_write_buffer_data.suggestName("line_write_buffer_data_"+line)
      line_write_buffer_wline.suggestName("line_write_buffer_wline_"+line)
      line_write_buffer_mask.suggestName("line_write_buffer_mask_"+line)
      line_write_buffer_offset.suggestName("line_write_buffer_offset_"+line)
      for(word <- 0 until CacheLineWords){
        for(byte <- 0 until DataBytes){
          val write_byte = sbuffer_in_s2_line_wen && (
            line_write_buffer_mask(byte) && (line_write_buffer_offset === word.U) || 
            line_write_buffer_wline
          )
          when(write_byte){
            data(line)(word)(byte) := line_write_buffer_data(byte*8+7, byte*8)
            mask(line)(word)(byte) := true.B
          }
        }
      }
    }
  }

  // 1 cycle line mask clean
  // for(i <- 0 until StorePipelineWidth) {
  //   val req = io.writeReq(i)
  //   when(req.valid){
  //     for(line <- 0 until StoreBufferSize){
  //       when(
  //         req.bits.wvec(line) && 
  //         req.bits.cleanMask
  //       ){
  //         for(word <- 0 until CacheLineWords){
  //           for(byte <- 0 until DataBytes){
  //             mask(line)(word)(byte) := false.B
  //             val debug_last_cycle_write_byte = RegNext(req.valid && req.bits.wvec(line) && (
  //               req.bits.mask(byte) && (req.bits.wordOffset(WordsWidth-1, 0) === word.U) || 
  //               req.bits.wline
  //             ))
  //             assert(!debug_last_cycle_write_byte)
  //           }
  //         }
  //       }
  //     }
  //   }
  // }

  io.dataOut := data
  io.maskOut := mask
}

class Sbuffer(implicit p: Parameters) extends DCacheModule with HasSbufferConst with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheWordReqWithVaddr)))  //Todo: store logic only support Width == 2 now
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
  val debug_mask = Reg(Vec(StoreBufferSize, Vec(CacheLineWords, Vec(DataBytes, Bool()))))
  val waitInflightMask = Reg(Vec(StoreBufferSize, UInt(StoreBufferSize.W)))
  val data = dataModule.io.dataOut
  val mask = dataModule.io.maskOut
  val stateVec = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U.asTypeOf(new SbufferEntryState))))
  val cohCount = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U(EvictCountBits.W))))
  val missqReplayCount = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U(MissqReplayCountBits.W))))

  val sbuffer_out_s0_fire = Wire(Bool())

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
  val accessIdx = Wire(Vec(StorePipelineWidth + 1, Valid(UInt(SbufferIndexWidth.W))))

  val replaceIdx = plru.way
  val replaceIdxOH = UIntToOH(plru.way)
  plru.access(accessIdx)

  //-------------------------cohCount-----------------------------
  // insert and merge: cohCount=0
  // every cycle cohCount+=1
  // if cohCount(EvictCountBits-1)==1, evict
  val cohTimeOutMask = VecInit(widthMap(i => cohCount(i)(EvictCountBits - 1) && stateVec(i).isActive()))
  val (cohTimeOutIdx, cohHasTimeOut) = PriorityEncoderWithFlag(cohTimeOutMask)
  val cohTimeOutOH = PriorityEncoderOH(cohTimeOutMask)
  val missqReplayTimeOutMask = VecInit(widthMap(i => missqReplayCount(i)(MissqReplayCountBits - 1) && stateVec(i).w_timeout))
  val (missqReplayTimeOutIdxGen, missqReplayHasTimeOutGen) = PriorityEncoderWithFlag(missqReplayTimeOutMask)
  val missqReplayHasTimeOut = RegNext(missqReplayHasTimeOutGen) && !RegNext(sbuffer_out_s0_fire)
  val missqReplayTimeOutIdx = RegEnable(missqReplayTimeOutIdxGen, missqReplayHasTimeOutGen)

  //-------------------------sbuffer enqueue-----------------------------

  // Now sbuffer enq logic is divided into 3 stages:

  // sbuffer_in_s0: 
  // * read data and meta from store queue
  // * store them in 2 entry fifo queue

  // sbuffer_in_s1: 
  // * read data and meta from fifo queue
  // * update sbuffer meta (vtag, ptag, flag)
  // * prevert that line from being sent to dcache (add a block condition)
  // * prepare cacheline level write enable signal, RegNext() data and mask 

  // sbuffer_in_s2: 
  // * use cacheline level buffer to update sbuffer data and mask
  // * remove dcache write block (if there is)

  val activeMask = VecInit(stateVec.map(s => s.isActive()))
  val drainIdx = PriorityEncoder(activeMask)

  val inflightMask = VecInit(stateVec.map(s => s.isInflight()))

  val inptags = io.in.map(in => getPTag(in.bits.addr))
  val invtags = io.in.map(in => getVTag(in.bits.vaddr))
  val sameTag = inptags(0) === inptags(1)
  val firstWord = getWord(io.in(0).bits.addr)
  val secondWord = getWord(io.in(1).bits.addr)
  val sameWord = firstWord === secondWord

  // merge condition
  val mergeMask = Wire(Vec(StorePipelineWidth, Vec(StoreBufferSize, Bool())))
  val mergeIdx = mergeMask.map(PriorityEncoder(_)) // avoid using mergeIdx for better timing
  val canMerge = mergeMask.map(ParallelOR(_))
  val mergeVec = mergeMask.map(_.asUInt)

  for(i <- 0 until StorePipelineWidth){
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
  val evenInvalidMask = GetEvenBits(invalidMask.asUInt)
  val oddInvalidMask = GetOddBits(invalidMask.asUInt)

  def getFirstOneOH(input: UInt): UInt = {
    assert(input.getWidth > 1)
    val output = WireInit(VecInit(input.asBools))
    (1 until input.getWidth).map(i => {
      output(i) := !input(i - 1, 0).orR && input(i)
    })
    output.asUInt
  }

  val evenRawInsertVec = getFirstOneOH(evenInvalidMask)
  val oddRawInsertVec = getFirstOneOH(oddInvalidMask)
  val (evenRawInsertIdx, evenCanInsert) = PriorityEncoderWithFlag(evenInvalidMask)
  val (oddRawInsertIdx, oddCanInsert) = PriorityEncoderWithFlag(oddInvalidMask)
  val evenInsertIdx = Cat(evenRawInsertIdx, 0.U(1.W)) // slow to generate, for debug only
  val oddInsertIdx = Cat(oddRawInsertIdx, 1.U(1.W)) // slow to generate, for debug only
  val evenInsertVec = GetEvenBits.reverse(evenRawInsertVec)
  val oddInsertVec = GetOddBits.reverse(oddRawInsertVec)

  val enbufferSelReg = RegInit(false.B)
  when(io.in(0).valid) {
    enbufferSelReg := ~enbufferSelReg
  }

  val firstInsertIdx = Mux(enbufferSelReg, evenInsertIdx, oddInsertIdx) // slow to generate, for debug only
  val secondInsertIdx = Mux(sameTag,
    firstInsertIdx,
    Mux(~enbufferSelReg, evenInsertIdx, oddInsertIdx)
  ) // slow to generate, for debug only
  val firstInsertVec = Mux(enbufferSelReg, evenInsertVec, oddInsertVec)
  val secondInsertVec = Mux(sameTag,
    firstInsertVec,
    Mux(~enbufferSelReg, evenInsertVec, oddInsertVec)
  ) // slow to generate, for debug only
  val firstCanInsert = sbuffer_state =/= x_drain_sbuffer && Mux(enbufferSelReg, evenCanInsert, oddCanInsert)
  val secondCanInsert = sbuffer_state =/= x_drain_sbuffer && Mux(sameTag,
    firstCanInsert,
    Mux(~enbufferSelReg, evenCanInsert, oddCanInsert)
  )
  val forward_need_uarch_drain = WireInit(false.B)
  val merge_need_uarch_drain = WireInit(false.B)
  val do_uarch_drain = RegNext(forward_need_uarch_drain) || RegNext(RegNext(merge_need_uarch_drain))
  XSPerfAccumulate("do_uarch_drain", do_uarch_drain)

  io.in(0).ready := firstCanInsert
  io.in(1).ready := secondCanInsert && !sameWord && io.in(0).ready

  def wordReqToBufLine( // allocate a new line in sbuffer
    req: DCacheWordReq,
    reqptag: UInt,
    reqvtag: UInt,
    insertIdx: UInt,
    insertVec: UInt,
    wordOffset: UInt
  ): Unit = {
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
      }
    })
  }

  def mergeWordReq( // merge write req into an existing line
    req: DCacheWordReq,
    reqptag: UInt,
    reqvtag: UInt,
    mergeIdx: UInt,
    mergeVec: UInt,
    wordOffset: UInt
  ): Unit = {
    assert(UIntToOH(mergeIdx) === mergeVec)
    (0 until StoreBufferSize).map(entryIdx => {
      when(mergeVec(entryIdx)) {
        cohCount(entryIdx) := 0.U
        // missqReplayCount(entryIdx) := 0.U
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

  for(((in, wordOffset), i) <- io.in.zip(Seq(firstWord, secondWord)).zipWithIndex){
    writeReq(i).valid := in.fire()
    writeReq(i).bits.wordOffset := wordOffset
    writeReq(i).bits.mask := in.bits.mask
    writeReq(i).bits.data := in.bits.data
    writeReq(i).bits.wline := in.bits.wline
    val debug_insertIdx = if(i == 0) firstInsertIdx else secondInsertIdx
    val insertVec = if(i == 0) firstInsertVec else secondInsertVec
    assert(!((PopCount(insertVec) > 1.U) && in.fire()))
    val insertIdx = OHToUInt(insertVec)
    accessIdx(i).valid := RegNext(in.fire())
    accessIdx(i).bits := RegNext(Mux(canMerge(i), mergeIdx(i), insertIdx))
    when(in.fire()){
      when(canMerge(i)){
        writeReq(i).bits.wvec := mergeVec(i)
        mergeWordReq(in.bits, inptags(i), invtags(i), mergeIdx(i), mergeVec(i), wordOffset)
        XSDebug(p"merge req $i to line [${mergeIdx(i)}]\n")
      }.otherwise({
        writeReq(i).bits.wvec := insertVec
        wordReqToBufLine(in.bits, inptags(i), invtags(i), insertIdx, insertVec, wordOffset)
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

  // ---------------------------------------------------------------------------
  // sbuffer to dcache pipeline
  // ---------------------------------------------------------------------------

  // Now sbuffer deq logic is divided into 2 stages:

  // sbuffer_out_s0:
  // * read data and meta from sbuffer
  // * RegNext() them
  // * set line state to inflight

  // sbuffer_out_s1:
  // * send write req to dcache

  // sbuffer_out_extra:
  // * receive write result from dcache
  // * update line state

  val sbuffer_out_s1_ready = Wire(Bool())

  // ---------------------------------------------------------------------------
  // sbuffer_out_s0
  // ---------------------------------------------------------------------------

  val need_drain = needDrain(sbuffer_state)
  val need_replace = do_eviction || (sbuffer_state === x_replace)
  val sbuffer_out_s0_evictionIdx = Mux(missqReplayHasTimeOut,
    missqReplayTimeOutIdx,
    Mux(need_drain,
      drainIdx,
      Mux(cohHasTimeOut, cohTimeOutIdx, replaceIdx)
    )
  )

  // If there is a inflight dcache req which has same ptag with sbuffer_out_s0_evictionIdx's ptag,
  // current eviction should be blocked.
  val sbuffer_out_s0_valid = missqReplayHasTimeOut || 
    stateVec(sbuffer_out_s0_evictionIdx).isDcacheReqCandidate() &&
    (need_drain || cohHasTimeOut || need_replace)
  assert(!(
    stateVec(sbuffer_out_s0_evictionIdx).isDcacheReqCandidate && 
    !noSameBlockInflight(sbuffer_out_s0_evictionIdx)
  ))
  val sbuffer_out_s0_cango = sbuffer_out_s1_ready
  sbuffer_out_s0_fire := sbuffer_out_s0_valid && sbuffer_out_s0_cango

  // ---------------------------------------------------------------------------
  // sbuffer_out_s1
  // ---------------------------------------------------------------------------

  // TODO: use EnsbufferWidth
  val shouldWaitWriteFinish = RegNext(VecInit((0 until StorePipelineWidth).map{i =>
    (writeReq(i).bits.wvec.asUInt & UIntToOH(sbuffer_out_s0_evictionIdx).asUInt).orR &&
    writeReq(i).valid
  }).asUInt.orR)
  // block dcache write if read / write hazard
  val blockDcacheWrite = shouldWaitWriteFinish

  val sbuffer_out_s1_valid = RegInit(false.B)
  sbuffer_out_s1_ready := io.dcache.req.ready && !blockDcacheWrite || !sbuffer_out_s1_valid
  val sbuffer_out_s1_fire = io.dcache.req.fire()

  // when sbuffer_out_s1_fire, send dcache req stored in pipeline reg to dcache
  when(sbuffer_out_s1_fire){
    sbuffer_out_s1_valid := false.B
  }
  // when sbuffer_out_s0_fire, read dcache req data and store them in a pipeline reg 
  when(sbuffer_out_s0_cango){
    sbuffer_out_s1_valid := sbuffer_out_s0_valid
  }
  when(sbuffer_out_s0_fire){
    stateVec(sbuffer_out_s0_evictionIdx).state_inflight := true.B
    stateVec(sbuffer_out_s0_evictionIdx).w_timeout := false.B
    // stateVec(sbuffer_out_s0_evictionIdx).s_pipe_req := true.B
    XSDebug(p"$sbuffer_out_s0_evictionIdx will be sent to Dcache\n")
  }

  XSDebug(p"need drain:$need_drain cohHasTimeOut: $cohHasTimeOut need replace:$need_replace\n")
  XSDebug(p"drainIdx:$drainIdx tIdx:$cohTimeOutIdx replIdx:$replaceIdx " +
    p"blocked:${!noSameBlockInflight(sbuffer_out_s0_evictionIdx)} v:${activeMask(sbuffer_out_s0_evictionIdx)}\n")
  XSDebug(p"sbuffer_out_s0_valid:$sbuffer_out_s0_valid evictIdx:$sbuffer_out_s0_evictionIdx dcache ready:${io.dcache.req.ready}\n")
  // Note: if other dcache req in the same block are inflight,
  // the lru update may not accurate
  accessIdx(StorePipelineWidth).valid := invalidMask(replaceIdx) || (
    need_replace && !need_drain && !cohHasTimeOut && !missqReplayHasTimeOut && sbuffer_out_s0_cango && activeMask(replaceIdx))
  accessIdx(StorePipelineWidth).bits := replaceIdx
  val sbuffer_out_s1_evictionIdx = RegEnable(sbuffer_out_s0_evictionIdx, enable = sbuffer_out_s0_fire)
  val sbuffer_out_s1_evictionPTag = RegEnable(ptag(sbuffer_out_s0_evictionIdx), enable = sbuffer_out_s0_fire)
  val sbuffer_out_s1_evictionVTag = RegEnable(vtag(sbuffer_out_s0_evictionIdx), enable = sbuffer_out_s0_fire)

  io.dcache.req.valid := sbuffer_out_s1_valid && !blockDcacheWrite
  io.dcache.req.bits := DontCare
  io.dcache.req.bits.cmd   := MemoryOpConstants.M_XWR
  io.dcache.req.bits.addr  := getAddr(sbuffer_out_s1_evictionPTag)
  io.dcache.req.bits.vaddr := getAddr(sbuffer_out_s1_evictionVTag)
  io.dcache.req.bits.data  := data(sbuffer_out_s1_evictionIdx).asUInt
  io.dcache.req.bits.mask  := mask(sbuffer_out_s1_evictionIdx).asUInt
  io.dcache.req.bits.id := sbuffer_out_s1_evictionIdx

  when (sbuffer_out_s1_fire) {
    assert(!(io.dcache.req.bits.vaddr === 0.U))
    assert(!(io.dcache.req.bits.addr === 0.U))
  }

  XSDebug(sbuffer_out_s1_fire,
    p"send buf [$sbuffer_out_s1_evictionIdx] to Dcache, req fire\n"
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

  io.dcache.hit_resps.zip(dataModule.io.maskFlushReq).map{case (resp, maskFlush) => {
    maskFlush.valid := resp.fire()
    maskFlush.bits.wvec := UIntToOH(resp.bits.id)
  }}

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
  XSPerfAccumulate("vaddr_match_failed", mismatch(0) || mismatch(1))
  for ((forward, i) <- io.forward.zipWithIndex) {
    val vtag_matches = VecInit(widthMap(w => vtag(w) === getVTag(forward.vaddr)))
    // ptag_matches uses paddr from dtlb, which is far from sbuffer
    val ptag_matches = VecInit(widthMap(w => RegEnable(ptag(w), forward.valid) === RegEnable(getPTag(forward.paddr), forward.valid)))
    val tag_matches = vtag_matches
    val tag_mismatch = RegNext(forward.valid) && VecInit(widthMap(w =>
      RegNext(vtag_matches(w)) =/= ptag_matches(w) && RegNext((activeMask(w) || inflightMask(w)))
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

  val perf_valid_entry_count = RegNext(PopCount(VecInit(stateVec.map(s => !s.isInvalid())).asUInt))
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
  XSPerfAccumulate("evenCanInsert", evenCanInsert)
  XSPerfAccumulate("oddCanInsert", oddCanInsert)
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
