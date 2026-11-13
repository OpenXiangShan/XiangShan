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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] James E. Smith, and Andrew R. Pleszkun. "[Implementation of precise interrupts in pipelined processors.]
* (https://dl.acm.org/doi/10.5555/327010.327125)" 12th Annual International Symposium on Computer Architecture (ISCA).
* 1985.
***************************************************************************************/

package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import difftest.common._
import difftest.plugin.topdown._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils._
import xiangshan._
import xiangshan.PerfDebugInfo
import xiangshan.backend.{BackendParams, Bundles, GPAMemEntry, RatToVecExcpMod, RegWriteFromRab, VecExcpInfo}
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.NewCSR.CSREvents.TargetPCBundle
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfo, LsTopdownInfo}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.{SnapshotGenerator, CompressType, NoCompressReason}
import xiangshan.backend.trace._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.decode.TruthTable
import xiangshan.TopDownCounters._
import xiangshan.backend.dispatch._
import xiangshan.backend.rob.Rob._

class Rob(params: BackendParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  lazy val module = new RobImp(this)(p, params)
}

class RobImp(override val wrapper: Rob)(implicit p: Parameters, params: BackendParams) extends LazyModuleImp(wrapper)
  with HasXSParameter with HasCircularQueuePtrHelper with HasPerfEvents with HasCriticalErrors {
  private val LduCnt = params.LduCnt
  private val StaCnt = params.StaCnt

  val in = IO(new In(params))
  val out = IO(new Out(params))

  override lazy val io_perf: Vec[PerfEvent] = out.perfEvents
  override lazy val io_error: Vec[Bool] = out.criticalErrors

  val exuWBs: Seq[ValidIO[WriteBackRobBundle]] = in.exuWriteback
  val exceptionWBs = in.writeback.filter(x => x.bits.params.needExceptionGen).toSeq
  val redirectWBs = in.writeback.filter(x => x.bits.redirect.nonEmpty).toSeq
  val branchWBs = in.exuWriteback.filter(_.bits.params.hasBrhFu).toSeq
  val isBrhOrJmpWBs = in.exuWriteback.filter(x => (x.bits.params.hasBrhFu || x.bits.params.hasJmpFu)).toSeq
  val csrWBs = in.exuWriteback.filter(x => x.bits.params.hasCSR).toSeq
  val ldWBs = in.exuWriteback.filter(x => x.bits.params.hasLoadFu).toSeq

  if (backendParams.debugEn){
    PerfCCT.tick(clock, reset)
    in.exuWriteback.zipWithIndex.foreach{ case (wb, i) =>
      PerfCCT.updateInstPos(wb.bits.debug_seqNum.get, PerfCCT.InstPos.AtWriteVal.id.U, wb.valid, clock, reset)
    }
  }

  val numExuWbPorts = exuWBs.length
  val bankAddrWidth = log2Up(CommitWidth)

  println(s"Rob: size $RobSize, numExuWbPorts: $numExuWbPorts, commitwidth: $CommitWidth")

  val rab = Module(new RenameBuffer(RabSize))
  val vtypeBuffer = Module(new VTypeBuffer(VTypeBufferSize))
  val bankNum = 8
  assert(RobSize % bankNum == 0, "RobSize % bankNum must be 0")
  val robEntries = RegInit(VecInit.fill(RobSize)((new RobEntryBundle).Lit(_.valid -> false.B)))
  // pointers
  // For enqueue ptr, we don't duplicate it since only enqueue needs it.
  val enqPtrVec = Wire(Vec(RenameWidth, new RobPtr))
  val deqPtrVec = Wire(Vec(CommitWidth, new RobPtr))
  val deqPtrVec_next = Wire(Vec(CommitWidth, Output(new RobPtr)))
  val walkPtrVec = Reg(Vec(CommitWidth, new RobPtr))
  val walkPtrTrue = Reg(new RobPtr)
  val lastWalkPtr = Reg(new RobPtr)
  val allowEnqueue = RegInit(true.B)
  val allowEnqueueForDispatch = RegInit(true.B)
  val vecExcpInfo = RegInit(ValidIO(new VecExcpInfo).Lit(
    _.valid -> false.B,
  ))

  /**
   * Enqueue (from dispatch)
   */
  // special cases
  val hasBlockBackward = RegInit(false.B)
  val hasWaitForward = RegInit(false.B)
  val enqPtr = enqPtrVec(0)
  val deqPtr = deqPtrVec(0)
  val walkPtr = walkPtrVec(0)
  val allocatePtrVec = VecInit((0 until RenameWidth).map(i => enqPtrVec(PopCount(in.enq.req.take(i).map(req => req.valid && req.bits.firstUop)))))
  in.enq.canAccept := allowEnqueue && !hasBlockBackward && rab.in.canEnq && vtypeBuffer.in.canEnq && !in.fromVecExcpMod.busy
  in.enq.canAcceptForDispatch := allowEnqueueForDispatch && !hasBlockBackward && rab.in.canEnqForDispatch && vtypeBuffer.in.canEnqForDispatch && !in.fromVecExcpMod.busy
  in.enq.resp := allocatePtrVec
  val uopCanEnqueue = VecInit(in.enq.req.map(req => req.valid && in.enq.canAccept))
  val canEnqueue = VecInit(in.enq.req.map(req => req.valid && req.bits.firstUop && in.enq.canAccept))
  val timer = GTimer()
  private val traceInstrCountWidth = log2Ceil(RenameWidth + 1)
  private val traceEncodeTable = (1 to RenameWidth).flatMap { instrNum =>
    (0 to instrNum).map(nonRVCNum => (instrNum, nonRVCNum))
  }
  private val traceDecodeTable = traceEncodeTable.map { case (instrNum, nonRVCNum) =>
    (instrNum, instrNum + nonRVCNum)
  }
  private def decodeTraceInstrCount(traceIretire: UInt): UInt = {
    chisel3.util.experimental.decode.decoder(
      traceIretire,
      TruthTable(
        traceDecodeTable.zipWithIndex.map { case ((instrNum, _), encode) =>
          BitPat((encode + 1).U(IretireWidthEncoded.W)) -> BitPat(instrNum.U(traceInstrCountWidth.W))
        },
        BitPat.N(traceInstrCountWidth)
      )
    )
  }
  private def decodeTraceHalfWords(traceIretire: UInt): UInt = {
    chisel3.util.experimental.decode.decoder(
      traceIretire,
      TruthTable(
        traceDecodeTable.zipWithIndex.map { case ((_, halfWords), encode) =>
          BitPat((encode + 1).U(IretireWidthEncoded.W)) -> BitPat(halfWords.U(IretireWidthCommited.W))
        },
        BitPat.N(IretireWidthCommited)
      )
    )
  }
  private def traceHalfWordsToBytes(halfWords: UInt): UInt = {
    val formerLenWidth = log2Ceil(RenameWidth * 4 + 1)
    Cat(halfWords, 0.U(1.W))(formerLenWidth - 1, 0)
  }

  val hartId = p(XSCoreParamsKey).HartId
  // robEntries enqueue
  for (i <- 0 until RobSize) {
    val enqOH = VecInit(canEnqueue.zip(allocatePtrVec.map(_.value === i.U)).map(x => x._1 && x._2))
    val entryEnqValid = uopCanEnqueue.zip(in.enq.req).map { case (valid, req) =>
      valid && req.bits.robIdx.value === i.U
    }
    assert(PopCount(enqOH) < 2.U, s"robEntries$i enqOH is not one hot")
    when(enqOH.asUInt.orR && !in.redirect.valid){
      // TODO: enq
      val entryHeadUop = Mux1H(enqOH, in.enq.req.map(_.bits))
      robEntries(i).connectEnq(entryHeadUop)
      val youngestEnqUop = PriorityMux(entryEnqValid.reverse, in.enq.req.map(_.bits).reverse)
      robEntries(i).traceBlockInPipe.itype := youngestEnqUop.traceBlockInPipe.itype
      robEntries(i).traceBlockInPipe.ilastsize := youngestEnqUop.traceBlockInPipe.ilastsize
      robEntries(i).formerTraceBlockInPipe.iretire := entryHeadUop.formerTraceIretire
      val formerEntryEnqValid = entryEnqValid.zip(in.enq.req).map { case (valid, req) =>
        valid && req.bits.robIdx.isFormer
      }
      when(VecInit(formerEntryEnqValid).asUInt.orR) {
        val youngestFormerUop = PriorityMux(formerEntryEnqValid.reverse, in.enq.req.map(_.bits).reverse)
        robEntries(i).formerTraceBlockInPipe.itype := youngestFormerUop.traceBlockInPipe.itype
        robEntries(i).formerTraceBlockInPipe.ilastsize := youngestFormerUop.traceBlockInPipe.ilastsize
      }
    }.elsewhen(in.redirect.valid && in.redirect.bits.robIdx.value === i.U){
      val entryPairType = robEntries(i).entryPairType
      val redirectIsFormer = in.redirect.bits.robIdx.isFormer
      val redirectFlushItSelf = in.redirect.bits.flushItself()
      val downgradeToFormer = CompressType.isNotNORMAL(entryPairType) && (
        redirectIsFormer && !redirectFlushItSelf ||
        !redirectIsFormer && redirectFlushItSelf
      )
      when(downgradeToFormer) {
        robEntries(i).entryPairType := CompressType.NORMAL
        robEntries(i).noCompressReason := NoCompressReason.flushedHalf
        robEntries(i).traceBlockInPipe := robEntries(i).formerTraceBlockInPipe
      }
    }
  }
  // robBanks0 include robidx : 0 8 16 24 32 ...
  val robBanks = VecInit((0 until bankNum).map(i => VecInit(robEntries.zipWithIndex.filter(_._2 % bankNum == i).map(_._1))))
  // each Bank has 20 Entries, read addr is one hot
  // all banks use same raddr
  val eachBankEntrieNum = robBanks(0).length
  val robBanksRaddrThisLine = RegInit(1.U(eachBankEntrieNum.W))
  val robBanksRaddrNextLine = Wire(UInt(eachBankEntrieNum.W))
  robBanksRaddrThisLine := robBanksRaddrNextLine
  val bankNumWidth = log2Up(bankNum)
  val deqPtrWidth = deqPtr.value.getWidth
  val highDeqPtrThisLine = deqPtr.value(deqPtrWidth - 1, bankNumWidth)
  val highDeqPtrMax = (RobSize - 1).U(deqPtrWidth - 1, bankNumWidth)
  val highDeqPtrNextLine = Mux(highDeqPtrThisLine === highDeqPtrMax, 0.U, highDeqPtrThisLine + 1.U)
  val robIdxThisLine = VecInit((0 until bankNum).map(i => Cat(highDeqPtrThisLine, i.U(bankNumWidth.W))))
  val robIdxNextLine = VecInit((0 until bankNum).map(i => Cat(highDeqPtrNextLine, i.U(bankNumWidth.W))))
  // robBanks read
  val robBanksRdataThisLine = VecInit(robBanks.map{ case bank =>
    Mux1H(robBanksRaddrThisLine, bank)
  })
  val robBanksRdataNextLine = VecInit(robBanks.map{ case bank =>
    val shiftBank = bank.drop(1) :+ bank(0)
    Mux1H(robBanksRaddrThisLine, shiftBank)
  })
  val robBanksRdataThisLineUpdate = Wire(Vec(CommitWidth, new RobEntryBundle))
  val robBanksRdataNextLineUpdate = Wire(Vec(CommitWidth, new RobEntryBundle))
  val needUpdateCommitW = Wire(Vec(2 * CommitWidth, Bool()))
  val commitValidThisLine = Wire(Vec(CommitWidth, Bool()))
  val hasCommitted = RegInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val donotNeedWalk = RegInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val allCommitted = Wire(Bool())

  when(allCommitted) {
    hasCommitted := 0.U.asTypeOf(hasCommitted)
  }.elsewhen(out.commits.isCommit){
    for (i <- 0 until CommitWidth){
      hasCommitted(i) := commitValidThisLine(i) || hasCommitted(i)
    }
  }
  allCommitted := out.commits.isCommit && commitValidThisLine.last
  val walkPtrHead = Wire(new RobPtr)
  val changeBankAddrToDeqPtr = walkPtrVec.head.addEntries(CommitWidth.U).isAfterSlot(lastWalkPtr)
  when(in.redirect.valid){
    robBanksRaddrNextLine := UIntToOH(walkPtrHead.value(walkPtrHead.value.getWidth-1, bankAddrWidth))
  }.elsewhen(allCommitted || out.commits.isWalk && !changeBankAddrToDeqPtr){
    robBanksRaddrNextLine := Mux(robBanksRaddrThisLine.head(1) === 1.U, 1.U, robBanksRaddrThisLine << 1)
  }.elsewhen(out.commits.isWalk && changeBankAddrToDeqPtr){
    robBanksRaddrNextLine := UIntToOH(deqPtr.value(deqPtr.value.getWidth-1, bankAddrWidth))
  }.otherwise(
    robBanksRaddrNextLine := robBanksRaddrThisLine
  )
  val robDeqGroup = Reg(Vec(bankNum, new RobCommitEntryBundle))
  val rawInfo = VecInit((0 until CommitWidth).map(i => robDeqGroup(deqPtrVec(i).value(bankAddrWidth-1, 0)))).toSeq
  val commitInfo = VecInit((0 until CommitWidth).map(i => robDeqGroup(deqPtrVec(i).value(bankAddrWidth-1,0)))).toSeq
  val walkInfo = VecInit((0 until CommitWidth).map(i => robDeqGroup(walkPtrVec(i).value(bankAddrWidth-1, 0)))).toSeq
  for (i <- 0 until CommitWidth) {
    robDeqGroup(i).connectCommitEntry(robBanksRdataThisLineUpdate(i), needUpdateCommitW(i))
    when(allCommitted){
      robDeqGroup(i).connectCommitEntry(robBanksRdataNextLineUpdate(i), needUpdateCommitW(i + CommitWidth))
    }
  }

  /*
    decode iretireEncoded => iretires, instrSize
    (instrNum((log2Ceil(RenameWidth + 1)).W), iretire(IretireWidthInPipe.W), encode(IretireWidthEncode.W))
    val instrSizeTable = Seq(
      (1, 1, 1), (1, 2, 2),
      (2, 2, 3), (2, 3, 4), (2, 4, 5),
      (3, 3, 6), (3, 4, 7), (3, 5, 8), (3, 6, 9),
      (4, 4, 10), (4, 5, 11), (4, 6, 12), (4, 7, 13), (4, 8, 14),
      (5, 5, 15), (5, 6, 16), (5, 7, 17), (5, 8, 18), (5, 9, 19), (5, 10, 20),
      (6, 6, 21), (6, 7, 22), (6, 8, 23), (6, 9, 24), (6, 10, 25), (6, 11, 26), (6, 12, 27),
    )
  */
  val iretireCommit = Wire(Vec(CommitWidth, UInt(IretireWidthCommited.W)))
  val instrSizeCommit = Wire(Vec(CommitWidth, UInt(traceInstrCountWidth.W)))
  val formerInstrCntCommit = Wire(Vec(CommitWidth, UInt(traceInstrCountWidth.W)))
  val latterInstrCntCommit = Wire(Vec(CommitWidth, UInt(traceInstrCountWidth.W)))
  val formerHalfWordsCommit = Wire(Vec(CommitWidth, UInt(IretireWidthCommited.W)))
  val formerLenCommit = Wire(Vec(CommitWidth, UInt(log2Ceil(RenameWidth * 4 + 1).W)))

  for (i <- 0 until CommitWidth) {
    iretireCommit(i) := decodeTraceHalfWords(rawInfo(i).traceBlockInPipe.iretire)
    instrSizeCommit(i) := decodeTraceInstrCount(rawInfo(i).traceBlockInPipe.iretire)
    formerInstrCntCommit(i) := decodeTraceInstrCount(rawInfo(i).formerTraceIretire)
    formerHalfWordsCommit(i) := decodeTraceHalfWords(rawInfo(i).formerTraceIretire)
    latterInstrCntCommit(i) := Mux(
      CompressType.isNotNORMAL(rawInfo(i).entryPairType),
      instrSizeCommit(i) - formerInstrCntCommit(i),
      0.U
    )
    formerLenCommit(i) := traceHalfWordsToBytes(formerHalfWordsCommit(i))
    when(rawInfo(i).commit_v && CompressType.isNotNORMAL(rawInfo(i).entryPairType)) {
      assert(instrSizeCommit(i) >= formerInstrCntCommit(i))
    }
    when(rawInfo(i).commit_v && CompressType.isNORMAL(rawInfo(i).entryPairType)) {
      assert(latterInstrCntCommit(i) === 0.U)
    }
  }
  val commitInstrCntByEntry = instrSizeCommit.toSeq
  for (i <- 0 until CommitWidth) {
    commitInfo(i).ftqOffset := 0.U
    commitInfo(i).ftqIdx := rawInfo(i).ftqIdx - 1.U
  }

  // data for debug
  // Warn: debug_* prefix should not exist in generated verilog.
  val debug_microOp = Reg(Vec(RobSize, Vec(2, new EnqRobUop)))
  val debug_lqIdx = Reg(Vec(RobSize, Vec(2, new LqPtr)))
  val debug_sqIdx = Reg(Vec(RobSize, Vec(2, new SqPtr)))
  val debug_exuData = Reg(Vec(RobSize, Vec(2, UInt(XLEN.W))))
  val debug_exuDebug = Reg(Vec(RobSize, Vec(2, new DebugBundle)))
  val debug_lsInfo = RegInit(VecInit(Seq.fill(RobSize)(DebugLsInfo.init)))
  val debug_lsTopdownInfo = RegInit(VecInit(Seq.fill(RobSize)(LsTopdownInfo.init)))
  val debug_lqIdxValid = RegInit(VecInit.fill(RobSize)(VecInit(Seq.fill(2)(false.B))))
  val debug_lsIssued = RegInit(VecInit.fill(RobSize)(false.B))
  val enqHalfWritten = RegInit(VecInit(Seq.fill(RobSize)(VecInit(Seq.fill(2)(false.B)))))

  private def slotIndex(ptr: RobPtr): UInt = (!ptr.isFormer).asUInt
  private def debugMeta(uop: EnqRobUop): RenameOutUopDebug =
    uop.debug.getOrElse(0.U.asTypeOf(new RenameOutUopDebug))

  val isEmpty = enqPtr.isSameEntry(deqPtr)
  val snptEnq = in.enq.canAccept && in.enq.req.map(x => x.valid && x.bits.snapshot).reduce(_ || _)
  val snapshotPtrVec = Wire(Vec(CommitWidth, new RobPtr))
  snapshotPtrVec(0) := in.enq.req(0).bits.robIdx
  for (i <- 1 until CommitWidth) {
    snapshotPtrVec(i) := snapshotPtrVec(0).addEntries(i.U).asFormer
  }
  val snapshots = SnapshotGenerator(snapshotPtrVec, snptEnq, in.snpt.snptDeq, in.redirect.valid, in.snpt.flushVec)
  val debug_lsIssue = WireDefault(debug_lsIssued)
  debug_lsIssue(deqPtr.value) := in.debugHeadLsIssue

  /**
   * states of Rob
   */
  val s_idle :: s_walk :: Nil = Enum(2)
  val state = RegInit(s_idle)
  val state_next = Wire(chiselTypeOf(state))

  val tip_computing :: tip_stalled :: tip_walk :: tip_drained :: Nil = Enum(4)
  val tip_state = WireInit(0.U(4.W))
  when(!isEmpty) {  // One or more inst in ROB
    when(state === s_walk || in.redirect.valid) {
      tip_state := tip_walk
    }.elsewhen(out.commits.isCommit && PopCount(out.commits.commitValid) =/= 0.U) {
      tip_state := tip_computing
    }.otherwise {
      tip_state := tip_stalled
    }
  }.otherwise {
    tip_state := tip_drained
  }
  val tip_table = ChiselDB.createTable("Tip_" + p(XSCoreParamsKey).HartId.toString, new TipEntry)
  val tip_data = Wire(new TipEntry())
  tip_data.state := tip_state
  tip_data.commits := out.commits
  tip_data.redirect := in.redirect
  tip_data.redirect_pc := debugMeta(debug_microOp(in.redirect.bits.robIdx.value)(slotIndex(in.redirect.bits.robIdx))).pc
  tip_data.debugLsInfo := debug_lsInfo(out.commits.robIdx(0).value)
  tip_table.log(tip_data, true.B, "", clock, reset)

  val exceptionGen = Module(new ExceptionGen(params))
  val exceptionDataRead = exceptionGen.out.state
  out.robDeqPtr := deqPtr

  /**
   * connection of [[rab]]
   */
  rab.in.redirect := in.redirect.valid

  rab.in.req.zip(in.enq.req).map { case (dest, src) =>
    dest.bits := src.bits
    dest.valid := src.valid && in.enq.canAccept
  }

  // TODO:
  val walkDestSizeDeqGroup = RegInit(VecInit(Seq.fill(CommitWidth)(0.U(log2Up(MaxUopSize + 1).W))))
  val realDestSizeSeq = VecInit(robDeqGroup.zip(hasCommitted).map{case (r, h) => Mux(h, 0.U, r.realDestSize)})
  val walkDestSizeSeq = VecInit(robDeqGroup.zip(donotNeedWalk).map{case (r, d) => Mux(d, 0.U, r.realDestSize)})
  val commitSizeSumSeq = VecInit((0 until CommitWidth).map(i => realDestSizeSeq.take(i + 1).reduce(_ +& _)))
  val walkSizeSumSeq   = VecInit((0 until CommitWidth).map(i => walkDestSizeSeq.take(i + 1).reduce(_ +& _)))
  val commitSizeSumCond = VecInit(commitValidThisLine.zip(hasCommitted).map{case (c,h) => (c || h) && out.commits.isCommit})
  val walkSizeSumCond   = VecInit(out.commits.walkValid.zip(donotNeedWalk).map{case (w,d) => (w || d) && out.commits.isWalk})
  val commitSizeSum = PriorityMuxDefault(commitSizeSumCond.reverse.zip(commitSizeSumSeq.reverse), 0.U)
  val walkSizeSum   = PriorityMuxDefault(walkSizeSumCond.reverse.zip(walkSizeSumSeq.reverse), 0.U)

  val deqVlsExceptionNeedCommit = RegInit(false.B)
  val deqVlsExceptionCommitSize = RegInit(0.U(log2Up(MaxUopSize + 1).W))
  val deqVlsCanCommit= RegInit(false.B)
  rab.in.fromRob.commitSize := Mux(deqVlsExceptionNeedCommit, deqVlsExceptionCommitSize, commitSizeSum)
  rab.in.fromRob.walkSize := walkSizeSum
  rab.in.fromRob.vecLoadExcp.valid := RegNext(exceptionDataRead.valid && exceptionDataRead.bits.isVecLoad)
  rab.in.fromRob.vecLoadExcp.bits.isStrided := RegEnable(exceptionDataRead.bits.isStrided, exceptionDataRead.valid)
  rab.in.fromRob.vecLoadExcp.bits.isVlm := RegEnable(exceptionDataRead.bits.isVlm, exceptionDataRead.valid)
  rab.in.snpt := in.snpt
  rab.in.snpt.snptEnq := snptEnq

  // pipe rab commits for better timing and area
  out.rabCommits := RegNext(rab.out.commits)
  out.vlCommits := RegNext(vtypeBuffer.out.commits)

  /**
   * connection of [[vtypeBuffer]]
   */

  vtypeBuffer.in.redirect := in.redirect.valid

  vtypeBuffer.in.req.zip(in.enq.req).map { case (sink, source) =>
    sink.valid := source.valid && in.enq.canAccept
    sink.bits := source.bits
  }

  private val commitIsVTypeVec = VecInit(out.commits.commitValid.zip(out.commits.info).map { case (valid, info) => out.commits.isCommit && valid && info.needVTB })
  private val walkIsVTypeVec = VecInit(out.commits.walkValid.zip(walkInfo).map { case (valid, info) => out.commits.isWalk && valid && info.needVTB })
  vtypeBuffer.in.fromRob.commitSize := PopCount(commitIsVTypeVec)
  vtypeBuffer.in.fromRob.walkSize := PopCount(walkIsVTypeVec)
  vtypeBuffer.in.snpt := in.snpt
  vtypeBuffer.in.snpt.snptEnq := snptEnq
  out.toDecode.walkToArchVType := vtypeBuffer.out.toDecode.walkToArchVType
  out.toDecode.commitVType := vtypeBuffer.out.toDecode.commitVType
  out.toDecode.walkVType := vtypeBuffer.out.toDecode.walkVType

  // When blockBackward instruction leaves Rob (commit or walk), hasBlockBackward should be set to false.B
  // To reduce registers usage, for hasBlockBackward cases, we allow enqueue after ROB is empty.
  // TODO: How about a lot of fence instr and XSTrap instr
  val hasWB = VecInit(exuWBs.map(wb => wb.valid)).asUInt.orR
  //  when(isEmpty || (hasBlockBackward && hasWB)) {
  when(isEmpty) {
    hasBlockBackward := false.B
  }
  // When any instruction commits, hasNoSpecExec should be set to false.B
  when(out.commits.hasWalkInstr || out.commits.hasCommitInstr) {
    hasWaitForward := false.B
  }

  out.debugWaitForward.foreach(_ := hasWaitForward)
  out.debugBlockBackward.foreach(_ := hasBlockBackward)

  // The wait-for-interrupt (WFI) instruction waits in the ROB until an interrupt might need servicing.
  // in.csr.wfiEvent will be asserted if the WFI can resume execution, and we change the state to s_wfi_idle.
  // It does not affect how interrupts are serviced. Note that WFI is noSpecExec and it does not trigger interrupts.
  val hasWFI = RegInit(false.B)
  val wfiSafe = out.wfi.safeFromMem && out.wfi.safeFromFrontend
  out.wfi.wfiReq := hasWFI
  out.cpu_wfi := hasWFI && wfiSafe
  // WFI Timeout: 2^20 = 1M cycles
  val wfi_cycles = RegInit(0.U(20.W))
  if (wfiResume) {
    when(hasWFI) {
      wfi_cycles := wfi_cycles + 1.U
    }.elsewhen(!hasWFI && RegNext(hasWFI)) {
      wfi_cycles := 0.U
    }
  }
  val wfi_timeout = wfi_cycles.andR
  when(RegNext(RegNext(in.csr.wfiEvent)) || out.flushOut.valid || wfi_timeout) {
    hasWFI := false.B
  }

  for (i <- 0 until RenameWidth) {
    // we don't check whether in.redirect is valid here since redirect has higher priority
    val hasEarlierSameHalf =
      if (i == 0) {
        false.B
      } else {
        VecInit((0 until i).map { j =>
          in.enq.canAccept &&
          in.enq.req(j).valid &&
          in.enq.req(j).bits.robIdx.isSameSlot(in.enq.req(i).bits.robIdx)
        }).asUInt.orR
      }
    val hasEarlierEntryReset =
      if (i == 0) {
        false.B
      } else {
        VecInit((0 until i).map { j =>
          in.enq.canAccept &&
          in.enq.req(j).valid &&
          in.enq.req(j).bits.firstUop &&
          in.enq.req(j).bits.robIdx.isSameEntry(in.enq.req(i).bits.robIdx)
        }).asUInt.orR
      }
    when(in.enq.canAccept && in.enq.req(i).valid) {
      val enqUop = in.enq.req(i).bits
      val enqIndex = in.enq.req(i).bits.robIdx.value
      val subIndex = !in.enq.req(i).bits.robIdx.isFormer
      val slotWritten = Mux(enqUop.firstUop || hasEarlierEntryReset, false.B, enqHalfWritten(enqIndex)(subIndex))
      when(enqUop.firstUop && !hasEarlierEntryReset) {
        enqHalfWritten(enqIndex)(0) := false.B
        enqHalfWritten(enqIndex)(1) := false.B
      }
      when(!hasEarlierSameHalf && !slotWritten) {
        // Keep the first instruction metadata in each rob-half for difftest/debug.
        debug_microOp(enqIndex)(subIndex) := enqUop
        debug_microOp(enqIndex)(subIndex).debug.foreach { debug =>
          debug.perfDebugInfo.dispatchTime := timer
          debug.perfDebugInfo.enqRsTime := timer
          debug.perfDebugInfo.selectTime := timer
          debug.perfDebugInfo.issueTime := timer
          debug.perfDebugInfo.writebackTime := timer
          debug.perfDebugInfo.tlbFirstReqTime := timer
          debug.perfDebugInfo.tlbRespTime := timer
        }
        when(enqUop.robIdx.isFormer) {
          robEntries(enqIndex).perfDebugInfo.foreach(_.dispatchTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.enqRsTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.selectTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.issueTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.writebackTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.tlbFirstReqTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.tlbRespTime := timer)
        }
        debug_lqIdxValid(enqIndex)(subIndex) := false.B
        enqHalfWritten(enqIndex)(subIndex) := true.B
      }
      when(enqUop.firstUop && !hasEarlierEntryReset) {
        debug_lsInfo(enqIndex) := DebugLsInfo.init
        debug_lsTopdownInfo(enqIndex) := LsTopdownInfo.init
        debug_lsIssued(enqIndex) := false.B
      }
      when(enqUop.waitForward) {
        hasWaitForward := true.B
      }
      val enqTriggerActionIsDebugMode = TriggerAction.isDmode(in.enq.req(i).bits.trigger)
      val enqHasException = enqUop.exceptionVec.orR
      when(enqUop.isWFI && !enqHasException && !enqTriggerActionIsDebugMode) {
        hasWFI := true.B
      }
    }
  }

  for (i <- 0 until RenameWidth) {
    val enqUop = in.enq.req(i)
    when(enqUop.valid && enqUop.bits.blockBackward && in.enq.canAccept) {
      hasBlockBackward := true.B
    }
  }

  val dispatchNum = Mux(in.enq.canAccept, PopCount(in.enq.req.map(req => req.valid && req.bits.firstUop)), 0.U)
  in.enq.isEmpty := RegNext(isEmpty && !VecInit(in.enq.req.map(_.valid)).asUInt.orR)

  when(!in.wfi_enable) {
    hasWFI := false.B
  }
  // sel vsetvl's flush position
  val vs_idle :: vs_waitVinstr :: vs_waitFlush :: Nil = Enum(3)
  val vsetvlState = RegInit(vs_idle)

  val firstVInstrFtqPtr = RegInit(0.U.asTypeOf(new FtqPtr))
  val firstVInstrFtqOffset = RegInit(0.U.asTypeOf(UInt(FetchBlockInstOffsetWidth.W)))
  val firstVInstrRobIdx = RegInit(RobPtr(false.B, 0.U))

  val enq0 = in.enq.req(0)
  val enq0IsVset = enq0.bits.isVset && enq0.bits.lastUop && canEnqueue(0)
  val enq0IsVsetFlush = enq0IsVset && enq0.bits.flushPipe
  val enqIsVInstrVec = in.enq.req.zip(canEnqueue).map { case (req, fire) => FuType.isVArith(req.bits.fuType) && fire }
  // for vs_idle
  val firstVInstrIdle = PriorityMux(enqIsVInstrVec.zip(in.enq.req).drop(1) :+ (true.B, 0.U.asTypeOf(in.enq.req(0).cloneType)))
  // for vs_waitVinstr
  val enqIsVInstrOrVset = (enqIsVInstrVec(0) || enq0IsVset) +: enqIsVInstrVec.drop(1)
  val firstVInstrWait = PriorityMux(enqIsVInstrOrVset, in.enq.req)
  when(vsetvlState === vs_idle) {
    firstVInstrFtqPtr := firstVInstrIdle.bits.ftqPtr
    firstVInstrFtqOffset := firstVInstrIdle.bits.ftqOffset
    firstVInstrRobIdx := firstVInstrIdle.bits.robIdx
  }.elsewhen(vsetvlState === vs_waitVinstr) {
    when(Cat(enqIsVInstrOrVset).orR) {
      firstVInstrFtqPtr := firstVInstrWait.bits.ftqPtr
      firstVInstrFtqOffset := firstVInstrWait.bits.ftqOffset
      firstVInstrRobIdx := firstVInstrWait.bits.robIdx
    }
  }

  val hasVInstrAfterI = Cat(enqIsVInstrVec(0)).orR
  when(vsetvlState === vs_idle && !in.redirect.valid) {
    when(enq0IsVsetFlush) {
      vsetvlState := Mux(hasVInstrAfterI, vs_waitFlush, vs_waitVinstr)
    }
  }.elsewhen(vsetvlState === vs_waitVinstr) {
    when(in.redirect.valid) {
      vsetvlState := vs_idle
    }.elsewhen(Cat(enqIsVInstrOrVset).orR) {
      vsetvlState := vs_waitFlush
    }
  }.elsewhen(vsetvlState === vs_waitFlush) {
    when(in.redirect.valid) {
      vsetvlState := vs_idle
    }
  }

  // lqEnq
  in.debugEnqLsq.needAlloc.map(_(0)).zip(in.debugEnqLsq.req).foreach { case (alloc, req) =>
    when(in.debugEnqLsq.canAccept && alloc && req.valid) {
      val ptr = req.bits.uop.robIdx
      debug_lqIdx(ptr.value)(slotIndex(ptr)) := req.bits.uop.lqIdx
      debug_lqIdxValid(ptr.value)(slotIndex(ptr)) := true.B
      when(ptr.isFormer) {
        robEntries(ptr.value).debug_lqIdx.foreach(_ := req.bits.uop.lqIdx)
      }
    }
  }

  // lsIssue
  when(in.debugHeadLsIssue) {
    debug_lsIssued(deqPtr.value) := true.B
  }

  /**
   * Writeback (from execution units)
   */
  for (wb <- exuWBs) {
    val wbIdx = wb.bits.robIdx.value
    val subIdx = slotIndex(wb.bits.robIdx)
    val debug_Uop = debug_microOp(wbIdx)(subIdx)
    val debug = debugMeta(debug_Uop)
    when(wb.valid) {
      debug_exuData(wbIdx)(subIdx) := wb.bits.data
      debug_exuDebug(wbIdx)(subIdx) := wb.bits.debug
      wb.bits.perfDebugInfo.foreach { x =>
        debug_microOp(wbIdx)(subIdx).debug.foreach { slotDebug =>
          slotDebug.perfDebugInfo.enqRsTime := x.enqRsTime
          slotDebug.perfDebugInfo.selectTime := x.selectTime
          slotDebug.perfDebugInfo.issueTime := x.issueTime
          slotDebug.perfDebugInfo.writebackTime := x.writebackTime
          slotDebug.perfDebugInfo.tlbFirstReqTime := x.tlbFirstReqTime
          slotDebug.perfDebugInfo.tlbRespTime := x.tlbRespTime
        }
        when(wb.bits.robIdx.isFormer) {
          robEntries(wbIdx).perfDebugInfo.foreach(_.enqRsTime := x.enqRsTime)
          robEntries(wbIdx).perfDebugInfo.foreach(_.selectTime := x.selectTime)
          robEntries(wbIdx).perfDebugInfo.foreach(_.issueTime := x.issueTime)
          robEntries(wbIdx).perfDebugInfo.foreach(_.writebackTime := x.writebackTime)
          robEntries(wbIdx).perfDebugInfo.foreach(_.tlbFirstReqTime := x.tlbFirstReqTime)
          robEntries(wbIdx).perfDebugInfo.foreach(_.tlbRespTime := x.tlbRespTime)
        }
      }
      debug_lqIdx(wbIdx)(subIdx) := wb.bits.lqIdx.getOrElse(0.U.asTypeOf(new LqPtr))
      debug_sqIdx(wbIdx)(subIdx) := wb.bits.sqIdx.getOrElse(0.U.asTypeOf(new SqPtr))
      when(wb.bits.robIdx.isFormer) {
        robEntries(wbIdx).debug_lqIdx.foreach(_ := wb.bits.lqIdx.getOrElse(0.U.asTypeOf(new LqPtr)))
        robEntries(wbIdx).debug_sqIdx.foreach(_ := wb.bits.sqIdx.getOrElse(0.U.asTypeOf(new SqPtr)))
      }
    }
    XSInfo(wb.valid,
      p"writebacked pc 0x${Hexadecimal(debug.pc)} wen ${debug_Uop.rfWen} " +
        p"data 0x${Hexadecimal(wb.bits.data)} ldst ${debug_Uop.ldest} pdst ${debug_Uop.pdest} " +
        p"skip ${wb.bits.debug.isSkipDiff} robIdx: ${wb.bits.robIdx}\n"
    )
  }

  // The entry-level trace block describes the youngest instruction in the
  // surviving compressed entry, while debug/difftest keep both slots exact.
  for (wb <- branchWBs) {
    val wbIdx = wb.bits.robIdx.value
    val subIdx = slotIndex(wb.bits.robIdx)
    when(wb.valid && wb.bits.redirect.get.bits.taken &&
      Itype.isNonTaken(debug_microOp(wbIdx)(subIdx).traceBlockInPipe.itype)) {
      debug_microOp(wbIdx)(subIdx).traceBlockInPipe.itype := Itype.Taken
    }
    when(wb.valid && wb.bits.robIdx.isFormer && wb.bits.redirect.get.bits.taken &&
      Itype.isNonTaken(robEntries(wbIdx).formerTraceBlockInPipe.itype)) {
      robEntries(wbIdx).formerTraceBlockInPipe.itype := Itype.Taken
    }
  }

  val writebackNum = PopCount(exuWBs.map(_.valid))
  XSInfo(writebackNum =/= 0.U, "writebacked %d insts\n", writebackNum)


  /**
   * RedirectOut: Interrupt and Exceptions
   */
  val debug_deqUop = debug_microOp(deqPtr.value).head

  val deqPtrEntry = rawInfo(0)
  val deqPtrEntryValid = deqPtrEntry.commit_v
  val deqHasFlushed = RegInit(false.B)
  val intrBitSetReg = RegNext(in.csr.intrBitSet)
  val intrEnable = intrBitSetReg && !hasWaitForward && deqPtrEntry.interruptSafe && !deqHasFlushed
  val deqPtrFormer = Wire(new RobPtr)
  val deqPtrLatter = Wire(new RobPtr)
  deqPtrFormer := deqPtr
  deqPtrLatter := deqPtr
  deqPtrFormer.isFormer := true.B
  deqPtrLatter.isFormer := false.B
  val deqExceptionIsFormer = intrEnable || deqPtrEntry.slotNeedFlushMask(0)
  val deqExceptionUop = Mux(deqExceptionIsFormer, debug_microOp(deqPtr.value)(0), debug_microOp(deqPtr.value)(1))
  // Select the exact slot (former/latter) that should provide exception state for the head entry.
  val deqExceptionRobIdx = Mux(deqPtrEntry.slotNeedFlushMask(0), deqPtrFormer, deqPtrLatter)
  val formerNotFlush = !deqPtrFormer.needFlush(in.redirect)
  val latterNotFlush = !deqPtrLatter.needFlush(in.redirect)
  val deqNeedFlush = (deqPtrEntry.slotNeedFlushMask(0) && formerNotFlush || deqPtrEntry.slotNeedFlushMask(1) && CompressType.isNotNORMAL(deqPtrEntry.entryPairType) && latterNotFlush) && deqPtrEntry.commit_v && deqPtrEntry.commit_w
  val deqHitExceptionGenState = exceptionDataRead.valid && exceptionDataRead.bits.robIdx.isSameSlot(deqExceptionRobIdx)
  val deqNeedFlushAndHitExceptionGenState = deqNeedFlush && deqHitExceptionGenState
  val exceptionGenStateIsException = exceptionDataRead.bits.exceptionVec.orR || exceptionDataRead.bits.singleStep || TriggerAction.isDmode(exceptionDataRead.bits.trigger)
  val commit_w_delay = RegNext(deqPtrEntry.commit_w)
  val deqHasException = deqNeedFlushAndHitExceptionGenState && exceptionGenStateIsException && commit_w_delay
  val deqHasFlushPipe = deqNeedFlushAndHitExceptionGenState && exceptionDataRead.bits.flushPipe && !deqHasException && commit_w_delay
  val deqHasReplayInst = deqNeedFlushAndHitExceptionGenState && exceptionDataRead.bits.replayInst
  val deqIsVlsException = deqHasException && deqExceptionUop.isVectorMemory && !exceptionDataRead.bits.isEnqExcp
  val deqIsVlsFlushPipe = deqHasFlushPipe && deqExceptionUop.isVectorMemory
  val deqCanException = deqHasException && (!deqIsVlsException || deqVlsCanCommit)
  val deqCanFlushPipe = deqHasFlushPipe && (!deqIsVlsFlushPipe || deqVlsCanCommit)
  // delay 2 cycle wait exceptionGen out
  // vls exception can be committed only when RAB commit all its reg pairs
  deqVlsCanCommit := RegNext(RegNext(deqPtrEntry.commit_w)) && rab.in.status.commitEnd

  // lock at assertion of deqVlsExceptionNeedCommit until condition not assert
  val deqVlsExcpLock = RegInit(false.B)
  val handleVlsExcp = deqIsVlsException && deqVlsCanCommit && !deqVlsExcpLock && state === s_idle
  when(handleVlsExcp) {
    deqVlsExcpLock := true.B
  }.elsewhen(deqPtrVec.head =/= deqPtrVec_next.head) {
    deqVlsExcpLock := false.B
  }

  // Only assert once when deqVlsExcp occurs until condition not assert to avoid multi message passed to RAB
  when (deqVlsExceptionNeedCommit) {
    deqVlsExceptionNeedCommit := false.B
  }.elsewhen(handleVlsExcp){
    deqVlsExceptionCommitSize := deqPtrEntry.realDestSize
    deqVlsExceptionNeedCommit := true.B
  }

  val hasCommit = out.commits.isCommit && out.commits.commitValid.asUInt.orR
  val retiredSlotPtrs = out.commits.robIdx.zip(out.commits.info).map { case (ptr, info) =>
    val retired = WireDefault(ptr)
    retired.isFormer := CompressType.isNORMAL(info.entryPairType)
    retired
  }
  val newestCommit = PriorityMuxDefault(
    out.commits.commitValid.zip(retiredSlotPtrs).reverse,
    deqPtr
  )
  out.diffRatCommitRobIdx.foreach { commitRobIdx =>
    commitRobIdx.valid := hasCommit || deqVlsExceptionNeedCommit
    commitRobIdx.bits := Mux(deqVlsExceptionNeedCommit, deqPtr.asFormer, newestCommit)
  }
  out.diffRatCommitRobIdxVec.foreach { commitRobIdxVec =>
    assert(!(deqVlsExceptionNeedCommit && hasCommit), "VLS and normal commits overlap")
    for (i <- 0 until CommitWidth) {
      commitRobIdxVec(i).valid := out.commits.isCommit && out.commits.commitValid(i) &&
        !deqVlsExceptionNeedCommit
      commitRobIdxVec(i).bits := retiredSlotPtrs(i)
    }
    when(deqVlsExceptionNeedCommit) {
      commitRobIdxVec.head.valid := true.B
      commitRobIdxVec.head.bits := deqPtr.asFormer
    }
  }

  XSDebug(deqHasException && exceptionDataRead.bits.singleStep, "Debug Mode: Deq has singlestep exception\n")
  XSDebug(deqHasException && TriggerAction.isDmode(exceptionDataRead.bits.trigger), "Debug Mode: Deq has trigger entry debug Mode\n")

  val isFlushPipe = deqPtrEntry.commit_w && (deqHasFlushPipe || deqHasReplayInst)

  // vsetvl instruction need another one cycle to write to vtype gen
  val isVsetFlushPipe = deqPtrEntry.commit_w && deqHasFlushed && exceptionDataRead.bits.isVset
  val isVsetFlushPipeReg = RegNext(isVsetFlushPipe)
  //  val needModifyFtqIdxOffset = isVsetFlushPipe && (vsetvlState === vs_waitFlush)
  val needModifyFtqIdxOffset = false.B
  out.isVsetFlushPipe := isVsetFlushPipe
  out.toDecode.isResumeVType := vtypeBuffer.out.toDecode.isResumeVType || isVsetFlushPipeReg
  // out.flushOut will trigger redirect at the next cycle.
  // Block any redirect or commit at the next cycle.
  val lastCycleFlush = RegNext(out.flushOut.valid)

  out.flushOut.valid := (state === s_idle) && deqPtrEntryValid && (intrEnable || deqCanException || deqCanFlushPipe) && !lastCycleFlush
  out.flushOut.bits := DontCare
  val flushIsFormer = intrEnable || deqPtrEntry.slotNeedFlushMask(0)
  val flushBaseIsRVC = Mux(flushIsFormer, deqPtrEntry.slotHeadRvcMask(0), Mux(deqPtrEntry.predTaken, deqPtrEntry.slotHeadRvcMask(1), deqPtrEntry.slotHeadRvcMask(0)))
  out.flushOut.bits.isRVC := flushBaseIsRVC
  out.flushOut.bits.robIdx := Mux(needModifyFtqIdxOffset, firstVInstrRobIdx, deqPtr)
  out.flushOut.bits.robIdx.isFormer := Mux(needModifyFtqIdxOffset, true.B, flushIsFormer)
  out.flushOut.bits.ftqIdx :=
    Mux(needModifyFtqIdxOffset,
      firstVInstrFtqPtr,
      Mux(flushIsFormer,
        deqPtrEntry.ftqIdx,
        Mux(deqPtrEntry.predTaken,
          deqPtrEntry.ftqIdx + 1.U,
          deqPtrEntry.ftqIdx
        )
      )
    )
  out.flushOut.bits.ftqOffset :=
    Mux(needModifyFtqIdxOffset,
      firstVInstrFtqOffset,
      Mux(flushIsFormer,
        deqPtrEntry.ftqOffset,
        Mux(deqPtrEntry.predTaken,
          Mux(deqPtrEntry.slotHeadRvcMask(1),
            0.U.asTypeOf(deqPtrEntry.ftqOffset),
            1.U.asTypeOf(deqPtrEntry.ftqOffset)
          ),
          deqPtrEntry.ftqOffset
        )
      )
    )
  out.flushPcInfo.formerLen := Mux(
    needModifyFtqIdxOffset || flushIsFormer || deqPtrEntry.predTaken,
    0.U.asTypeOf(out.flushPcInfo.formerLen),
    formerLenCommit(0)
  )
  out.flushPcInfo.flushIsRVC := Mux(
    needModifyFtqIdxOffset,
    out.flushOut.bits.isRVC,
    Mux(flushIsFormer, deqPtrEntry.slotHeadRvcMask(0), deqPtrEntry.slotHeadRvcMask(1))
  )
  // TODO: it could use the former instr's ftqIdx and ftqOffset to calculate the latter instr's PC
  // rather than calculate the latter instr's ftqIdx and ftqOffset which could be wrong!!!

  out.flushOut.bits.level := Mux(deqHasReplayInst || intrEnable || deqHasException || needModifyFtqIdxOffset, RedirectLevel.flush, RedirectLevel.flushAfter) // TODO use this to implement "exception next"
  out.flushOut.bits.interrupt := !isFlushPipe
  out.flushOut.bits.satpFlush := isFlushPipe && exceptionDataRead.bits.satpFlush
  XSPerfAccumulate("flush_num", out.flushOut.valid)
  XSPerfAccumulate("interrupt_num", out.flushOut.valid && intrEnable)
  XSPerfAccumulate("exception_num", out.flushOut.valid && deqHasException)
  XSPerfAccumulate("flush_pipe_num", out.flushOut.valid && isFlushPipe)
  XSPerfAccumulate("replay_inst_num", out.flushOut.valid && isFlushPipe && deqHasReplayInst)

  val exceptionHappen = (state === s_idle) && deqPtrEntryValid && (intrEnable || deqCanException) && !lastCycleFlush
  val exceptionIsFormer = deqExceptionIsFormer
  val exceptionUop = deqExceptionUop
  val exceptionDebug = debugMeta(exceptionUop)
  out.exception.valid := RegNext(exceptionHappen)
  out.exception.bits.pc := RegEnable(exceptionDebug.pc, exceptionHappen)
  out.exception.bits.gpaddr := out.readGPAMemData.gpaddr
  out.exception.bits.isForVSnonLeafPTE := out.readGPAMemData.isForVSnonLeafPTE
  out.exception.bits.instr := RegEnable(exceptionDebug.instr, exceptionHappen)
  out.exception.bits.commitType := RegEnable(exceptionUop.commitType, exceptionHappen)
  out.exception.bits.isFormer := RegEnable(exceptionIsFormer, exceptionHappen)
  out.exception.bits.isStore := RegEnable(FuType.isStore(exceptionUop.fuType), exceptionHappen)
  out.exception.bits.exceptionVec extendFrom RegEnable(exceptionDataRead.bits.exceptionVec, exceptionHappen)
  out.exception.bits.satpFlushFirstFetchFault := RegEnable(exceptionDataRead.bits.satpFlushFirstFetchFault && deqHasException, exceptionHappen)
  // fetch trigger fire or execute ebreak
  out.exception.bits.isPcBkpt := RegEnable(
    exceptionDataRead.bits.exceptionVec(ExceptionNO.EX_BP) && (
      exceptionDataRead.bits.isEnqExcp ||
      exceptionDataRead.bits.trigger === TriggerAction.None
    ),
    exceptionHappen,
  )
  out.exception.bits.isFetchMalAddr := RegEnable(exceptionDataRead.bits.isFetchMalAddr && deqHasException, exceptionHappen)
  out.exception.bits.singleStep := RegEnable(exceptionDataRead.bits.singleStep, exceptionHappen)
  out.exception.bits.crossPageIPFFix := RegEnable(exceptionDataRead.bits.crossPageIPFFix, exceptionHappen)
  out.exception.bits.isInterrupt := RegEnable(intrEnable, exceptionHappen)
  out.exception.bits.isHls := RegEnable(exceptionUop.isHls, exceptionHappen)
  out.exception.bits.vls := RegEnable(exceptionUop.isVectorMemory, exceptionHappen)
  out.exception.bits.trigger := RegEnable(exceptionDataRead.bits.trigger, exceptionHappen)

  // A latter exception flushes itself but preserves its older former slot.
  // Hold only the difftest observation until that slot really retires after walk.
  val diffTrapPending = WireDefault(false.B)
  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val pending = RegInit(false.B)
    val pendingPtr = Reg(new RobPtr)
    val release = pending && out.commits.isCommit && out.commits.commitValid.head &&
      deqPtr.isSameEntry(pendingPtr)
    when(exceptionHappen && !exceptionIsFormer) {
      assert(!pending, "A latter exception observation is already pending")
      assert(!exceptionUop.isVectorMemory, "Vector memory instructions must not be compressed")
      pending := true.B
      pendingPtr := deqPtr
    }.elsewhen(release) {
      pending := false.B
    }
    when(release) {
      assert(CompressType.isNORMAL(out.commits.info.head.entryPairType))
      assert(PopCount(out.commits.commitValid) === 1.U)
      assert(in.diffArchEvent.get.valid && in.diffArchEvent.get.bits,
        "The surviving former and its latter exception must be observed together")
    }
    diffTrapPending := pending
    out.diffCommitForTrap.get := release
  }

  // data will be one cycle after valid
  out.readGPAMemAddr.valid := exceptionHappen
  out.readGPAMemAddr.bits.ftqPtr := exceptionDataRead.bits.ftqPtr
  out.readGPAMemAddr.bits.ftqOffset := exceptionDataRead.bits.ftqOffset
  out.readGPAMemAddr.bits.isRVC := exceptionDataRead.bits.isRVC

  XSDebug(out.flushOut.valid,
    p"generate redirect: pc 0x${Hexadecimal(out.exception.bits.pc)} intr $intrEnable " +
      p"excp $deqHasException flushPipe $isFlushPipe " +
      p"Trap_target 0x${Hexadecimal(in.csr.trapTarget.pc)} exceptionVec ${Binary(exceptionDataRead.bits.exceptionVec.asUInt)}\n")

  /**
   * Commits (and walk)
   * They share the same width.
   */
  // T redirect.valid, T+1 use walkPtrVec read robEntries, T+2 start walk, shouldWalkVec used in T+2
  val shouldWalkVec = Wire(Vec(CommitWidth,Bool()))
  val walkingPtrVec = RegNext(walkPtrVec)
  when(in.redirect.valid){
    shouldWalkVec := 0.U.asTypeOf(shouldWalkVec)
  }.elsewhen(RegNext(in.redirect.valid)){
    shouldWalkVec := 0.U.asTypeOf(shouldWalkVec)
  }.elsewhen(state === s_walk){
    shouldWalkVec := VecInit(walkingPtrVec.map(_.isNotAfterSlot(lastWalkPtr)).zip(donotNeedWalk).map(x => x._1 && !x._2))
  }.otherwise(
    shouldWalkVec := 0.U.asTypeOf(shouldWalkVec)
  )
  val walkFinished = walkPtrTrue.isAfterSlot(lastWalkPtr)
  rab.in.fromRob.walkEnd := state === s_walk && walkFinished
  vtypeBuffer.in.fromRob.walkEnd := state === s_walk && walkFinished

  require(RenameWidth <= CommitWidth)

  // wiring to csr
  // update when enq
  val enqFlagTracker = Module(new RobSetFlagTracker(
    2,
    in.enq.req.size,
    CommitWidth,
    orderedUpdatePorts = true
  ))
  enqFlagTracker.in.update.zip(in.enq.req).zip(uopCanEnqueue).foreach {
    case ((update, req), canEnqueue) =>
      update.valid := canEnqueue
      update.bits.robIdx := req.bits.robIdx
      update.bits.setMask := Cat(req.bits.dirtyVs, req.bits.dirtyFs || req.bits.fflagsWen)
  }
  enqFlagTracker.in.redirect := in.redirect
  enqFlagTracker.in.commit.zip(out.commits.commitValid).zip(out.commits.robIdx).foreach {
    case ((commit, valid), robIdx) =>
      commit.valid := out.commits.isCommit && valid
      commit.bits := robIdx
  }
  val dirty_fs = enqFlagTracker.out.commitSetMask(0)
  val dirty_vs = enqFlagTracker.out.commitSetMask(1)

  val deqPtrCmp = Wire(new RobPtr)
  val enqPtrCmp = Wire(new RobPtr)
  deqPtrCmp := deqPtr
  enqPtrCmp := enqPtr
  deqPtrCmp.isFormer := true.B
  enqPtrCmp.isFormer := true.B
  private def wbSlotStillValid(robIdx: RobPtr): Bool = {
    val wbEntry = robEntries(robIdx.value) // TODO: this may cause timing issue
    wbEntry.valid && (robIdx.isFormer || CompressType.isNotNORMAL(wbEntry.entryPairType))
  }

  // update when writeback
  val fflagsWidth = 5

  private val writebackFflagsCandidates = (0 until in.writeback.size).filter { index =>
    in.writeback(index).bits.fflags.nonEmpty
  }
  private val writebackVxsatCandidates = (0 until in.writeback.size).filter { index =>
    in.writeback(index).bits.vxsat.nonEmpty
  }
  private val writebackFlagCandidates =
    Seq.fill(fflagsWidth)(writebackFflagsCandidates) :+ writebackVxsatCandidates
  val writebackFlagTracker = Module(new RobSetFlagTracker(
    fflagsWidth + 1,
    in.writeback.size,
    CommitWidth,
    writebackFlagCandidates
  ))
  writebackFlagTracker.in.update.zip(in.writeback).foreach { case (update, wb) =>
    val wbInRobWindow = wb.bits.robIdx.isNotBeforeSlot(deqPtrCmp) && wb.bits.robIdx.isBeforeSlot(enqPtrCmp)
    update.valid := wb.valid && wbInRobWindow && wbSlotStillValid(wb.bits.robIdx)
    update.bits.robIdx := wb.bits.robIdx
    update.bits.setMask := Cat(
      wb.bits.vxsat.getOrElse(false.B),
      wb.bits.fflags
        .map(flags => Mux(wb.bits.fflagsWen.getOrElse(false.B), flags, 0.U(fflagsWidth.W)))
        .getOrElse(0.U(fflagsWidth.W))
    )
  }
  writebackFlagTracker.in.redirect := in.redirect
  writebackFlagTracker.in.commit.zip(out.commits.commitValid).zip(out.commits.robIdx).foreach {
    case ((commit, valid), robIdx) =>
      commit.valid := out.commits.isCommit && valid
      commit.bits := robIdx
  }

  val resetVstart = dirty_vs && !in.vstartIsZero

  vecExcpInfo.valid := exceptionHappen && !intrEnable && exceptionDataRead.bits.vstartEn && exceptionDataRead.bits.isVecLoad && !exceptionDataRead.bits.isEnqExcp
  when (exceptionHappen) {
    vecExcpInfo.bits.nf := exceptionDataRead.bits.nf
    vecExcpInfo.bits.vsew := exceptionDataRead.bits.vsew
    vecExcpInfo.bits.veew := exceptionDataRead.bits.veew
    vecExcpInfo.bits.vlmul := exceptionDataRead.bits.vlmul
    vecExcpInfo.bits.isStride := exceptionDataRead.bits.isStrided
    vecExcpInfo.bits.isIndexed := exceptionDataRead.bits.isIndexed
    vecExcpInfo.bits.isWhole := exceptionDataRead.bits.isWhole
    vecExcpInfo.bits.isVlm := exceptionDataRead.bits.isVlm
    vecExcpInfo.bits.vstart := exceptionDataRead.bits.vstart
  }

  out.csr.vstart.valid := RegNext(Mux(exceptionHappen && deqHasException, exceptionDataRead.bits.vstartEn, resetVstart))
  out.csr.vstart.bits := RegNext(Mux(exceptionHappen && deqHasException, exceptionDataRead.bits.vstart, 0.U))

  // when mispredict branches writeback, stop commit in the next 2 cycles
  // TODO: don't check all exu write back
  val misPredWb = Cat(VecInit(redirectWBs.map(wb =>
    wb.bits.redirect.get.bits.isMisPred && wb.bits.redirect.get.valid && wb.valid
  ).toSeq)).orR
  val misPredBlockCounter = Reg(UInt(3.W))
  misPredBlockCounter := Mux(misPredWb,
    "b111".U,
    misPredBlockCounter >> 1.U
  )
  val misPredBlock = misPredBlockCounter(0)
  val deqFlushBlockCounter = Reg(UInt(3.W))
  val deqFlushBlock = deqFlushBlockCounter(0)
  val deqHasCommitted = out.commits.isCommit && out.commits.commitValid(0)
  // TODO *** WARNING ***
  // Blocking commit. Don't change this before we fully understand the logic.
  val deqHitRedirectReg = RegNext(in.redirect.valid && in.redirect.bits.robIdx.isSameEntry(deqPtr)) ||
    RegNext(RegNext(in.redirect.valid && in.redirect.bits.robIdx.isSameEntry(deqPtr)))
  val criticalErrorState = in.csr.criticalErrorState
  when(deqNeedFlush && deqHitRedirectReg){
    deqFlushBlockCounter := "b111".U
  }.otherwise{
    deqFlushBlockCounter := deqFlushBlockCounter >> 1.U
  }
  when(deqHasCommitted){
    deqHasFlushed := false.B
  }.elsewhen(deqNeedFlush && out.flushOut.valid && !out.flushOut.bits.flushItself()){
    deqHasFlushed := true.B
  }
  val traceBlock = out.trace.blockCommit
  val blockCommit = misPredBlock || lastCycleFlush || hasWFI || in.redirect.valid ||
    (deqNeedFlush && !deqHasFlushed) || deqFlushBlock || criticalErrorState || traceBlock

  out.commits.isWalk := state === s_walk
  out.commits.isCommit := state === s_idle && !blockCommit

  val walk_v = VecInit(walkingPtrVec.map(ptr => robEntries(ptr.value).valid))
  val commit_vDeqGroup = VecInit(robDeqGroup.map(_.commit_v))
  val commit_wDeqGroup = VecInit(robDeqGroup.map(_.commit_w))
  val realCommitLast = deqPtrVec(0).lineHeadPtr.addEntries(Fill(bankAddrWidth, 1.U)).asFormer
  val commit_block = VecInit((0 until CommitWidth).map(i => !commit_wDeqGroup(i) && !hasCommitted(i)))
  val allowOnlyOneCommit = VecInit(robDeqGroup.map(x => x.commit_v && (x.slotNeedFlushMask(0) || x.slotNeedFlushMask(1) && CompressType.isNotNORMAL(x.entryPairType)))).asUInt.orR || intrBitSetReg || diffTrapPending
  // for instructions that may block others, we don't allow them to commit
  out.commits.commitValid := PriorityMux(commitValidThisLine, (0 until CommitWidth).map(i => (commitValidThisLine.asUInt >> i).asUInt.asTypeOf(out.commits.commitValid)))

  for (i <- 0 until CommitWidth) {
    // defaults: state === s_idle and instructions commit
    // when intrBitSetReg, allow only one instruction to commit at each clock cycle
    val isBlocked = intrEnable || (deqNeedFlush && !deqHasFlushed)
    val isBlockedByOlder = if (i != 0) commit_block.asUInt(i, 0).orR || allowOnlyOneCommit && !hasCommitted.asUInt(i - 1, 0).andR else false.B
    commitValidThisLine(i) := commit_vDeqGroup(i) && commit_wDeqGroup(i) && !isBlocked && !isBlockedByOlder && !hasCommitted(i)
    out.commits.info(i) := commitInfo(i)
    out.commits.robIdx(i) := deqPtrVec(i)
    val deqDebugInst = debug_microOp(deqPtrVec(i).value).head
    deqDebugInst.debug.foreach { debug =>
      PerfCCT.commitInstMeta(i.U, debug.debug_seqNum.seqNum, commitInstrCntByEntry(i), out.commits.isCommit && out.commits.commitValid(i), clock, reset)
    }

    out.commits.walkValid(i) := shouldWalkVec(i)
    XSError(
      state === s_walk &&
      out.commits.isWalk && state === s_walk && shouldWalkVec(i) &&
      !walk_v(i),
      s"The walking entry($i) should be valid\n")

    XSInfo(out.commits.isCommit && out.commits.commitValid(i),
      "retired pc %x wen %d ldest %d pdest %x data %x\n",
      debugMeta(debug_microOp(deqPtrVec(i).value).head).pc,
      out.commits.info(i).rfWen,
      out.commits.info(i).basicDebug.map(_.ldest).getOrElse(0.U),
      out.commits.info(i).basicDebug.map(_.pdest).getOrElse(0.U),
      debug_exuData(deqPtrVec(i).value)(0)
    )
    XSInfo(state === s_walk && out.commits.walkValid(i), "walked pc %x wen %d ldst %d data %x\n",
      debugMeta(debug_microOp(walkPtrVec(i).value).head).pc,
      out.commits.info(i).rfWen,
      out.commits.info(i).basicDebug.map(_.ldest).getOrElse(0.U),
      debug_exuData(walkPtrVec(i).value)(0)
    )
  }

  val noCommitCycleCnt = RegInit(0.U(20.W))
  when(out.commits.isCommit && out.commits.commitValid(0)) {
    noCommitCycleCnt := 0.U
  }.otherwise {
    noCommitCycleCnt := noCommitCycleCnt + 1.U
  }
  // Read the architectural ROB entry explicitly for the debug-only counters.
  // `robDeqGroup` is a `RobCommitEntryBundle`, which intentionally no longer
  // carries the shadow uop state used to derive these values.
  val deqPtrRobEntry: RobEntryBundle = robEntries(deqPtr.value)
  val deqPtrFormerUopNum = deqPtrRobEntry.decodeFormerUopNum(
    deqPtrRobEntry.entryPairType,
    deqPtrRobEntry.uopState
  )
  val deqPtrLatterUopNum = deqPtrRobEntry.decodeLatterUopNum(
    deqPtrRobEntry.entryPairType,
    deqPtrRobEntry.uopState
  )
  when(noCommitCycleCnt === 14999.U) {
    printf(p"[CROB-STUCK] deq=${Hexadecimal(deqPtr.value)} valid=${deqPtrEntry.commit_v} " +
      p"realDest=${deqPtrEntry.realDestSize} " +
      p"ctype=${Binary(deqPtrEntry.entryPairType)} slotNeedFlush=${Binary(deqPtrEntry.slotNeedFlushMask)} " +
      p"formerCnt=${formerInstrCntCommit(0)} latterCnt=${latterInstrCntCommit(0)} " +
      p"formerUopNum=${deqPtrFormerUopNum} latterUopNum=${deqPtrLatterUopNum} " +
      p"headPC=${Hexadecimal(debugMeta(debug_microOp(deqPtr.value).head).pc)} " +
      p"tailPC=${Hexadecimal(debugMeta(debug_microOp(deqPtr.value)(1)).pc)}\n")
  }

  // sync fflags/dirty_fs/vxsat to csr
  for(i <- 0 until fflagsWidth) {
    out.csr.fflags(i) := RegNext(writebackFlagTracker.out.commitSetMask(i), false.B)
  }
  out.csr.dirty_fs := GatedValidRegNext(dirty_fs)
  out.csr.dirty_vs := GatedValidRegNext(dirty_vs)
  out.csr.vxsat    := RegNext(writebackFlagTracker.out.commitSetMask(fflagsWidth), false.B)

  // commit load/store to lsq
  val ldCommitVec = VecInit((0 until CommitWidth).flatMap { i =>
    (0 until 2).map { j =>
      val slotValid = out.commits.commitValid(i) && (j == 0).B ||
        out.commits.commitValid(i) && !CompressType.isNORMAL(out.commits.info(i).entryPairType)
      slotValid && debug_microOp(deqPtrVec(i).value)(j).commitType === CommitType.LOAD
    }
  })
  // TODO: Check if meet the require that only set scommit when commit scala store uop
//  val stCommitVec = VecInit((0 until CommitWidth).map(i => out.commits.commitValid(i) && out.commits.info(i).commitType === CommitType.STORE && !robEntries(deqPtrVec(i).value).vls ))
  val newStCommit = VecInit((0 until CommitWidth).map(i => out.commits.commitValid(i) && out.commits.info(i).entryHasStore && !robEntries(deqPtrVec(i).value).vls)).asUInt.orR
  out.lsq.lcommit := RegNext(Mux(out.commits.isCommit, PopCount(ldCommitVec), 0.U))
//  out.lsq.scommit := RegNext(Mux(out.commits.isCommit, PopCount(stCommitVec), 0.U))
//  out.lsq.scommit := RegNext(Mux(out.commits.isCommit && newStCommit, 1.U, 0.U))

  out.lsq.commit := RegNext(out.commits.isCommit && out.commits.commitValid(0))

  val pendingEntry = robEntries(deqPtr.value)

  val pendingBackPtr = Wire(new RobPtr)
  pendingBackPtr := deqPtr
  pendingBackPtr.isFormer := false.B

  val flushesHeadEntry = in.redirect.valid &&
    in.redirect.bits.robIdx.isSameEntry(deqPtr) &&
    in.redirect.bits.robIdx.isFormer &&
    in.redirect.bits.flushItself()
  val clearsHeadLatter = in.redirect.valid &&
    in.redirect.bits.robIdx.isSameEntry(deqPtr) &&
    CompressType.isNotNORMAL(pendingEntry.entryPairType) && (
      in.redirect.bits.robIdx.isFormer && !in.redirect.bits.flushItself() ||
      !in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself()
    )
  val resetsPendingSlot = flushesHeadEntry || clearsHeadLatter
  // Entry validity is cleared by redirectValidReg one cycle after the redirect.
  // Keep the former slot selected until that delayed clear has taken effect.
  val resetsPendingSlotReg = RegNext(resetsPendingSlot, false.B)
  val pendingPtrIsFormer = RegInit(true.B)
  when (
    resetsPendingSlot || resetsPendingSlotReg ||
      out.commits.isCommit && out.commits.commitValid.asUInt.orR
  ) {
    pendingPtrIsFormer := true.B
  }.elsewhen(
    !pendingBackPtr.needFlush(in.redirect) &&
    pendingEntry.valid &&
    CompressType.isNotNORMAL(pendingEntry.entryPairType) &&
    pendingEntry.formerUopNum === 0.U &&
    pendingEntry.latterUopNum =/= 0.U
  ) {
    pendingPtrIsFormer := false.B
  }

  val pendingPtrInput = WireInit(deqPtr)
  pendingPtrInput.isFormer := Mux(resetsPendingSlot, true.B, pendingPtrIsFormer)
  val pendingPtrReg = RegInit(RobPtr(false.B, 0.U))
  pendingPtrReg := pendingPtrInput

  // VirtualStoreQueue samples this boundary in the redirect cycle.  Do not let
  // a stale latter state survive either a whole-entry flush or a redirect that
  // removes only the latter half of the head entry.
  out.lsq.pendingPtr := Mux(resetsPendingSlot, deqPtr.asFormer, pendingPtrReg)

  out.lsq.pendingPtrNext := RegNext(deqPtrVec_next.head)
  out.lsq.scommit := RegNext(
    Mux(out.commits.isCommit && newStCommit ||
      CompressType.isCC(pendingEntry.entryPairType) &&
      pendingEntry.entryHasStore &&
      pendingEntry.formerUopNum === 0.U &&
      pendingEntry.latterUopNum =/= 0.U, 1.U, 0.U))

  /**
   * state changes
   * (1) redirect: switch to s_walk
   * (2) walk: when walking comes to the end, switch to s_idle
   */
  state_next := Mux(
    in.redirect.valid || RegNext(in.redirect.valid), s_walk,
    Mux(
      state === s_walk && walkFinished && rab.in.status.walkEnd && vtypeBuffer.in.status.walkEnd, s_idle,
      state
    )
  )
  XSPerfAccumulate("s_idle_to_idle", state === s_idle && state_next === s_idle)
  XSPerfAccumulate("s_idle_to_walk", state === s_idle && state_next === s_walk)
  XSPerfAccumulate("s_walk_to_idle", state === s_walk && state_next === s_idle)
  XSPerfAccumulate("s_walk_to_walk", state === s_walk && state_next === s_walk)
  state := state_next

  /**
   * pointers and counters
   */
  val deqPtrGenModule = Module(new RobDeqPtrWrapper)
  deqPtrGenModule.in.state := state
  deqPtrGenModule.in.deq_v := commit_vDeqGroup
  deqPtrGenModule.in.deq_w := commit_wDeqGroup
  deqPtrGenModule.in.exception_state := exceptionDataRead
  deqPtrGenModule.in.intrBitSetReg := intrBitSetReg
  deqPtrGenModule.in.hasNoSpecExec := hasWaitForward
  deqPtrGenModule.in.allowOnlyOneCommit := allowOnlyOneCommit
  deqPtrGenModule.in.interruptSafe := robDeqGroup(deqPtr.value(bankAddrWidth-1,0)).interruptSafe
  deqPtrGenModule.in.blockCommit := blockCommit
  deqPtrGenModule.in.hasCommitted := hasCommitted
  deqPtrGenModule.in.allCommitted := allCommitted
  deqPtrVec := deqPtrGenModule.out.ptrVec
  deqPtrVec_next := deqPtrGenModule.out.nextPtrVec

  val enqPtrGenModule = Module(new RobEnqPtrWrapper)
  enqPtrGenModule.in.redirect := in.redirect
  enqPtrGenModule.in.allowEnqueue := allowEnqueue && rab.in.canEnq && !in.fromVecExcpMod.busy
  enqPtrGenModule.in.hasBlockBackward := hasBlockBackward
  enqPtrGenModule.in.enq := VecInit(in.enq.req.map(req => req.valid && req.bits.firstUop))
  enqPtrVec := enqPtrGenModule.out.ptrVec

  // next walkPtrVec:
  // (1) redirect occurs: update according to state
  // (2) walk: move forwards
  val deqPtrReadBank = deqPtrVec_next(0).lineHeadPtr
  val deqPtrVecForWalk = VecInit((0 until CommitWidth).map(i => deqPtrReadBank.addEntries(i.U).asFormer))
  val snapPtrReadBank = snapshots(in.snpt.snptSelect)(0).lineHeadPtr
  val snapPtrVecForWalk = VecInit((0 until CommitWidth).map(i => snapPtrReadBank.addEntries(i.U).asFormer))
  val walkPtrVec_next: Vec[RobPtr] = Mux(in.redirect.valid,
    Mux(in.snpt.useSnpt, snapPtrVecForWalk, deqPtrVecForWalk),
    Mux((state === s_walk) && !walkFinished, VecInit(walkPtrVec.map(_.addEntries(CommitWidth.U).asFormer)), walkPtrVec)
  )
  val walkPtrTrue_next: RobPtr = Mux(in.redirect.valid,
    Mux(in.snpt.useSnpt, snapshots(in.snpt.snptSelect)(0), deqPtrVec_next(0)),
    Mux((state === s_walk) && !walkFinished, walkPtrVec_next.head, walkPtrTrue)
  )
  walkPtrHead := walkPtrVec_next.head
  walkPtrVec := walkPtrVec_next
  walkPtrTrue := walkPtrTrue_next
  // T in.redirect.valid, T+1 walkPtrLowBits update, T+2 donotNeedWalk update
  val walkPtrLowBits = Reg(UInt(bankAddrWidth.W))
  when(in.redirect.valid){
    walkPtrLowBits := Mux(in.snpt.useSnpt, snapshots(in.snpt.snptSelect)(0).value(bankAddrWidth-1, 0), deqPtrVec_next(0).value(bankAddrWidth-1, 0))
  }
  when(in.redirect.valid) {
    donotNeedWalk := Fill(donotNeedWalk.length, true.B).asTypeOf(donotNeedWalk)
  }.elsewhen(RegNext(in.redirect.valid)){
    donotNeedWalk := (0 until CommitWidth).map(i => (i.U < walkPtrLowBits))
  }.otherwise{
    donotNeedWalk := 0.U.asTypeOf(donotNeedWalk)
  }
  walkDestSizeDeqGroup.zip(walkPtrVec_next).map {
    case (reg, ptrNext) => reg := deqPtrEntry.realDestSize
  }
  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  val commitCnt = PopCount(out.commits.commitValid)

  allowEnqueue := numValidEntries + dispatchNum <= (RobSize - RenameWidth).U
  allowEnqueueForDispatch := numValidEntries + dispatchNum <= (RobSize - 2 * RenameWidth).U

  val redirectWalkDistance = distanceBetween(in.redirect.bits.robIdx, deqPtrVec_next(0))
  when(in.redirect.valid) {
    // TODO: lastWalkPtr points the last valid entry
    lastWalkPtr := Mux(
      in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself(),
      in.redirect.bits.robIdx.subEntries(1.U).asFormer,
      in.redirect.bits.robIdx
    )
  }


  /**
   * States
   * We put all the stage bits changes here.
   *
   * All events: (1) enqueue (dispatch); (2) writeback; (3) cancel; (4) dequeue (commit);
   * All states: (1) valid; (2) writebacked; (3) flagBkup
   */

  val deqPtrGroup = Wire(Vec(2 * CommitWidth, new RobPtr))
  deqPtrGroup.zipWithIndex.map { case (deq, i) => deq := deqPtrVec(0).addEntries(i.U).asFormer }
  val commitReadAddr = Mux(state === s_idle, VecInit(deqPtrVec.map(_.value)), VecInit(walkPtrVec.map(_.value)))

  val redirectValidReg = RegNext(in.redirect.valid)
  val redirectBegin = Reg(UInt(log2Up(RobSize).W))
  val redirectEnd = Reg(UInt(log2Up(RobSize).W))
  val redirectAll = RegInit(false.B)
  when(in.redirect.valid){
    redirectBegin := Mux(in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself(),
      Mux(in.redirect.bits.robIdx.value === 0.U, (RobSize - 1).U, in.redirect.bits.robIdx.value - 1.U),
      in.redirect.bits.robIdx.value
    )
    redirectEnd := enqPtr.value
    // Only former+flushItself removes the redirect entry itself.  A latter-slot
    // redirect must retain the former slot even when the physical range wraps.
    redirectAll := in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself() &&
      (in.redirect.bits.robIdx.value === enqPtr.value) && (in.redirect.bits.robIdx.flag ^ enqPtr.flag)
  }

  // update robEntries valid
  val enqOHByEntry = (0 until RobSize).map { i =>
    VecInit(canEnqueue.zip(allocatePtrVec.map(_.value === i.U)).map {
      case (valid, ptrMatch) => valid && ptrMatch
    })
  }
  for (i <- 0 until RobSize) {
    val enqOH = enqOHByEntry(i)
    val commitCond = out.commits.isCommit && out.commits.commitValid.zip(deqPtrVec.map(_.value === i.U)).map(x => x._1 && x._2).reduce(_ || _)
    assert(PopCount(enqOH) < 2.U, s"robEntries$i enqOH is not one hot")
    // TODO: fix the needFlush logic for CROB
    // redirectBegin is still valid, but redirectBegin+1 is invalid
    val needFlush = redirectValidReg && (Mux(
      redirectEnd > redirectBegin,
      (i.U > redirectBegin) && (i.U < redirectEnd),
      (i.U > redirectBegin) || (i.U < redirectEnd)
    ) || redirectAll)
    when(commitCond) {
      robEntries(i).valid := false.B
    }.elsewhen(enqOH.asUInt.orR && !in.redirect.valid) {
      robEntries(i).valid := true.B
    }.elsewhen(needFlush){
      robEntries(i).valid := false.B
    }
  }

  // debug_inst update
  for (i <- 0 until (LduCnt + StaCnt)) {
    debug_lsInfo(in.debug_ls.debugLsInfo(i).s1_robIdx.value).s1SignalEnable(in.debug_ls.debugLsInfo(i))
    debug_lsInfo(in.debug_ls.debugLsInfo(i).s2_robIdx.value).s2SignalEnable(in.debug_ls.debugLsInfo(i))
    debug_lsInfo(in.debug_ls.debugLsInfo(i).s3_robIdx).s3SignalEnable(in.debug_ls.debugLsInfo(i))
  }
  for (i <- 0 until LduCnt) {
    debug_lsTopdownInfo(in.lsTopdownInfo(i).s1.robIdx).s1SignalEnable(in.lsTopdownInfo(i))
    debug_lsTopdownInfo(in.lsTopdownInfo(i).s2.robIdx).s2SignalEnable(in.lsTopdownInfo(i))
  }
  // status field: writebacked
  // enqueue logic set 6 writebacked to false

  // writeback logic set numWbPorts writebacked to true

  // if the first uop of an instruction is valid , write writebackedCounter
  val uopEnqValidSeq = in.enq.req.map(req => in.enq.canAccept && req.valid)
  val enqNeedWriteRFSeq = in.enq.req.map(_.bits.needEnqRab)
  val enqHasExcpSeq = in.enq.req.map(_.bits.hasException)
  val enqRobIdxSeq = in.enq.req.map(req => req.bits.robIdx.value)
  val enqWBNumVec = VecInit(in.enq.req.map(req => req.bits.numWB))
  private val enqWriteStdVec = VecInit(in.enq.req.map(req => req.bits.stdwriteNeed))
  val enqFormerWBNumVec = VecInit(in.enq.req.map(req => req.bits.formerNumWB))
  val enqLatterWBNumVec = VecInit(in.enq.req.map(req => req.bits.latterNumWB))

  private def saturatingDecPacked(value: UInt, decRaw: UInt): UInt = {
    val compareWidth = math.max(value.getWidth, decRaw.getWidth)
    val valueExt = value.pad(compareWidth)
    val decExt = decRaw.pad(compareWidth)
    val clippedDec = Mux(decExt > valueExt, valueExt, decExt)
    (valueExt - clippedDec)(value.getWidth - 1, 0)
  }

  private def calcNextPackedUopState(
    entryValid: Bool,
    entryUopState: UInt,
    entryCompressType: UInt,
    redirectClearsLatter: Bool,
    needFlushWriteBackFormer: Bool,
    needFlushWriteBackLater: Bool,
    instCanEnqFlag: Bool,
    enqState: UInt,
    formerWbCntRaw: UInt,
    latterWbCntRaw: UInt
  ): UInt = {
    val normalFormerState = entryUopState(NormalUopNumWidth - 1, 0)
    val compressedFormerState = entryUopState(CompressedSlotUopNumWidth - 1, 0)
    val compressedLatterState = entryUopState(
      2 * CompressedSlotUopNumWidth - 1,
      CompressedSlotUopNumWidth
    )
    val currentIsNormal = CompressType.isNORMAL(entryCompressType)
    val currentFormerForNormal = Mux(
      currentIsNormal,
      normalFormerState,
      compressedFormerState.pad(NormalUopNumWidth)
    )

    val normalFormerAfterWb = saturatingDecPacked(currentFormerForNormal, formerWbCntRaw)
    val compressedFormerAfterWb = saturatingDecPacked(compressedFormerState, formerWbCntRaw)
    val compressedLatterAfterWb = saturatingDecPacked(compressedLatterState, latterWbCntRaw)

    val normalFormerNext = Mux(needFlushWriteBackFormer, 0.U, normalFormerAfterWb)
    val compressedFormerNext = Mux(needFlushWriteBackFormer, 0.U, compressedFormerAfterWb)
    val compressedLatterNext = Mux(
      needFlushWriteBackLater || redirectClearsLatter,
      0.U,
      compressedLatterAfterWb
    )

    val nextEntryPairType = Mux(
      redirectClearsLatter,
      CompressType.NORMAL,
      entryCompressType
    )
    val nextNormalState = Cat(0.U(1.W), normalFormerNext)
    val nextCompressedState = Cat(compressedLatterNext, compressedFormerNext)
    val stateAfterWriteback = Mux(
      CompressType.isNORMAL(nextEntryPairType),
      nextNormalState,
      nextCompressedState
    )
    val currentCanonicalState = Mux(
      currentIsNormal,
      Cat(0.U(1.W), normalFormerState),
      Cat(compressedLatterState, compressedFormerState)
    )
    val clearBothByFormerExceptionFlush =
      needFlushWriteBackFormer && CompressType.isNotNORMAL(entryCompressType)

    val nextState = Wire(UInt(entryUopState.getWidth.W))
    nextState := currentCanonicalState
    when(!entryValid && instCanEnqFlag) {
      nextState := enqState
    }.elsewhen(entryValid) {
      nextState := Mux(
        clearBothByFormerExceptionFlush,
        0.U(entryUopState.getWidth.W),
        stateAfterWriteback
      )
    }
    nextState
  }

  for (i <- 0 until RobSize) {

    val enqOH = enqOHByEntry(i)
    val robIdxMatchSeq = in.enq.req.map(_.bits.robIdx.value === i.U)
    val uopCanEnqSeq = uopEnqValidSeq.zip(robIdxMatchSeq).map { case (valid, isMatch) => valid && isMatch }
    val instCanEnqFlag = enqOH.asUInt.orR
    val isFirstEnq = !robEntries(i).valid && instCanEnqFlag
    val realDestEnqNum = PopCount(enqNeedWriteRFSeq.zip(uopCanEnqSeq).map { case (writeFlag, valid) => writeFlag && valid })
    val entryPairType = robEntries(i).entryPairType
    val redirectIsFormer = in.redirect.bits.robIdx.isFormer
    val redirectFlushItSelf = in.redirect.bits.flushItself()
    when(isFirstEnq){
      robEntries(i).realDestSize := realDestEnqNum
    }.elsewhen(robEntries(i).valid && Cat(uopCanEnqSeq).orR){
      robEntries(i).realDestSize := robEntries(i).realDestSize + realDestEnqNum
    }.elsewhen(in.redirect.valid && in.redirect.bits.robIdx.value === i.U){
      val clearsLatter = CompressType.isNotNORMAL(entryPairType) && (
        redirectIsFormer && !redirectFlushItSelf ||
        !redirectIsFormer && redirectFlushItSelf
      )
      when(clearsLatter && (CompressType.isCC(entryPairType) || CompressType.isCS(entryPairType))){
        robEntries(i).realDestSize := robEntries(i).complexSlotHasDest
      }.elsewhen(clearsLatter && CompressType.isSC(entryPairType)){
        robEntries(i).realDestSize := robEntries(i).realDestSize - robEntries(i).complexSlotHasDest
      }
    }
    val enqFormerWBNum = Mux1H(enqOH, enqFormerWBNumVec)
    val enqLatterWBNum = Mux1H(enqOH, enqLatterWBNumVec)
    val formerWbCnt = PopCount(in.writeback.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U && writeback.bits.robIdx.isFormer))
    val latterWbCnt = PopCount(in.writeback.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U && !writeback.bits.robIdx.isFormer))
    val slotNeedFlushMask = robEntries(i).slotNeedFlushMask
    val canWbExceptionNeedFlushSeq = exceptionWBs.zip(in.writebackNeedFlush).map { case (writeback, needFlushWb) =>
      writeback.valid && needFlushWb && writeback.bits.robIdx.value === i.U
    }
    val canWbFormerExceptionNeedFlushSeq = canWbExceptionNeedFlushSeq.zip(exceptionWBs).map {
      case (canWbExceptionNeedFlush, writeback) => canWbExceptionNeedFlush && writeback.bits.robIdx.isFormer
    }
    val canWbLaterExceptionNeedFlushSeq = canWbExceptionNeedFlushSeq.zip(exceptionWBs).map {
      case (canWbExceptionNeedFlush, writeback) => canWbExceptionNeedFlush && !writeback.bits.robIdx.isFormer
    }
    val needFlushWriteBack = Cat(canWbExceptionNeedFlushSeq).orR
    val needFlushWriteBackFormer = Cat(canWbFormerExceptionNeedFlushSeq).orR
    val needFlushWriteBackLater = Cat(canWbLaterExceptionNeedFlushSeq).orR
    when(robEntries(i).valid && needFlushWriteBack){
      slotNeedFlushMask := slotNeedFlushMask | Cat(needFlushWriteBackLater, needFlushWriteBackFormer)
    }

    val redirectHitThisEntry = robEntries(i).valid &&
      in.redirect.valid &&
      in.redirect.bits.robIdx.value === i.U
    val redirectClearsLatter = redirectHitThisEntry &&
      CompressType.isNotNORMAL(entryPairType) && (
        in.redirect.bits.robIdx.isFormer && !in.redirect.bits.flushItself() ||
        !in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself()
      )
    val enqEntryPairType = Mux1H(enqOH, in.enq.req.map(_.bits.entryPairType))
    val nextPackedUopState = calcNextPackedUopState(
      entryValid = robEntries(i).valid,
      entryUopState = robEntries(i).uopState,
      entryCompressType = robEntries(i).entryPairType,
      redirectClearsLatter = redirectClearsLatter,
      needFlushWriteBackFormer = needFlushWriteBackFormer,
      needFlushWriteBackLater = needFlushWriteBackLater,
      instCanEnqFlag = instCanEnqFlag,
      enqState = robEntries(i).encodeUopState(enqEntryPairType, enqFormerWBNum, enqLatterWBNum),
      formerWbCntRaw = formerWbCnt,
      latterWbCntRaw = latterWbCnt
    )
    robEntries(i).uopState := nextPackedUopState

    // trace
    val youngestSlotIsFormer = CompressType.isNORMAL(robEntries(i).entryPairType) || redirectClearsLatter
    val taken = branchWBs.map(writeback =>
      writeback.valid &&
      writeback.bits.robIdx.value === i.U &&
      (writeback.bits.robIdx.isFormer === youngestSlotIsFormer) &&
      writeback.bits.redirect.get.bits.taken
    ).reduce(_ || _)
    when(robEntries(i).valid && Itype.isNonTaken(robEntries(i).traceBlockInPipe.itype) && taken){
      // BranchType code(notaken itype = 4) must be correctly replaced!
      // TODO: In the future, there are two itype in trace
      robEntries(i).traceBlockInPipe.itype := Itype.Taken
    }
  }

  // begin update robBanksRdata
  val robBanksRdata = VecInit(robBanksRdataThisLine ++ robBanksRdataNextLine)
  val needUpdate = Wire(Vec(2 * CommitWidth, new RobEntryBundle))
  needUpdate := VecInit(robBanksRdataThisLine ++ robBanksRdataNextLine)
  val needUpdateRobIdx = robIdxThisLine ++ robIdxNextLine
  for (i <- 0 until 2 * CommitWidth) {
    val enqHeadOH = VecInit(canEnqueue.zip(allocatePtrVec.map(_.value === needUpdateRobIdx(i))).map {
      case (valid, ptrMatch) => valid && ptrMatch
    })
    val robIdxMatchSeq = in.enq.req.map(_.bits.robIdx.value === needUpdateRobIdx(i))
    val uopCanEnqSeq = uopEnqValidSeq.zip(robIdxMatchSeq).map { case (valid, isMatch) => valid && isMatch }
    val instCanEnqFlag = enqHeadOH.asUInt.orR
    val realDestEnqNum = PopCount(enqNeedWriteRFSeq.zip(uopCanEnqSeq).map { case (writeFlag, valid) => writeFlag && valid })
    when(!needUpdate(i).valid && instCanEnqFlag) {
      needUpdate(i).realDestSize := realDestEnqNum
    }.elsewhen(needUpdate(i).valid && instCanEnqFlag) {
      needUpdate(i).realDestSize := robBanksRdata(i).realDestSize + realDestEnqNum
    }
    val enqFormerWBNum = Mux1H(enqHeadOH, enqFormerWBNumVec)
    val enqLatterWBNum = Mux1H(enqHeadOH, enqLatterWBNumVec)
    val formerWbCnt = PopCount(in.writeback.map(writeback => writeback.valid && writeback.bits.robIdx.value === needUpdateRobIdx(i) && writeback.bits.robIdx.isFormer))
    val latterWbCnt = PopCount(in.writeback.map(writeback => writeback.valid && writeback.bits.robIdx.value === needUpdateRobIdx(i) && !writeback.bits.robIdx.isFormer))
    val slotNeedFlushMask = robBanksRdata(i).slotNeedFlushMask
    val canWbExceptionNeedFlushSeq = exceptionWBs.zip(in.writebackNeedFlush).map { case (writeback, needFlushWb) =>
      writeback.valid && needFlushWb && (writeback.bits.robIdx.value === needUpdateRobIdx(i))
    }
    val canWbFormerExceptionNeedFlushSeq = canWbExceptionNeedFlushSeq.zip(exceptionWBs).map {
      case (canWbExceptionNeedFlush, writeback) => canWbExceptionNeedFlush && writeback.bits.robIdx.isFormer
    }
    val canWbLaterExceptionNeedFlushSeq = canWbExceptionNeedFlushSeq.zip(exceptionWBs).map {
      case (canWbExceptionNeedFlush, writeback) => canWbExceptionNeedFlush && !writeback.bits.robIdx.isFormer
    }
    val needFlushWriteBack = Cat(canWbExceptionNeedFlushSeq).orR
    val needFlushWriteBackFormer = Cat(canWbFormerExceptionNeedFlushSeq).orR
    val needFlushWriteBackLater = Cat(canWbLaterExceptionNeedFlushSeq).orR
    when(needUpdate(i).valid && needFlushWriteBack) {
      needUpdate(i).slotNeedFlushMask := slotNeedFlushMask | Cat(needFlushWriteBackLater, needFlushWriteBackFormer)
    }

    val redirectHitThisEntry = needUpdate(i).valid &&
      in.redirect.valid &&
      in.redirect.bits.robIdx.value === needUpdateRobIdx(i)
    val entryPairType = robBanksRdata(i).entryPairType
    val redirectClearsLatter = redirectHitThisEntry &&
      CompressType.isNotNORMAL(entryPairType) && (
        in.redirect.bits.robIdx.isFormer && !in.redirect.bits.flushItself() ||
        !in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself()
      )
    val enqEntryPairType = Mux1H(enqHeadOH, in.enq.req.map(_.bits.entryPairType))
    val nextPackedUopState = calcNextPackedUopState(
      entryValid = needUpdate(i).valid,
      entryUopState = robBanksRdata(i).uopState,
      entryCompressType = robBanksRdata(i).entryPairType,
      redirectClearsLatter = redirectClearsLatter,
      needFlushWriteBackFormer = needFlushWriteBackFormer,
      needFlushWriteBackLater = needFlushWriteBackLater,
      instCanEnqFlag = instCanEnqFlag,
      enqState = robBanksRdata(i).encodeUopState(enqEntryPairType, enqFormerWBNum, enqLatterWBNum),
      formerWbCntRaw = formerWbCnt,
      latterWbCntRaw = latterWbCnt
    )
    needUpdate(i).uopState := nextPackedUopState
    needUpdateCommitW(i) := !nextPackedUopState.orR

    // trace
    val youngestSlotIsFormer = CompressType.isNORMAL(robBanksRdata(i).entryPairType) || redirectClearsLatter
    val taken = branchWBs.map(writeback =>
      writeback.valid &&
      writeback.bits.robIdx.value === needUpdateRobIdx(i) &&
      (writeback.bits.robIdx.isFormer === youngestSlotIsFormer) &&
      writeback.bits.redirect.get.bits.taken
    ).reduce(_ || _)
    when(robBanksRdata(i).valid && Itype.isNonTaken(robBanksRdata(i).traceBlockInPipe.itype) && taken){
      // BranchType code(notaken itype = 4) must be correctly replaced!
      needUpdate(i).traceBlockInPipe.itype := Itype.Taken
    }
  }
  robBanksRdataThisLineUpdate := VecInit(needUpdate.take(bankNum))
  robBanksRdataNextLineUpdate := VecInit(needUpdate.drop(bankNum))
  // end update robBanksRdata

  // interruptSafe
//  for (i <- 0 until RenameWidth) {
//    when(canEnqueue(i)) {
//      // For now, we allow non-load-store instructions to trigger interrupts
//      // For MMIO instructions, they should not trigger interrupts since they may
//      // be sent to lower level before it writes back.
//      // However, we cannot determine whether a load/store instruction is MMIO.
//      // Thus, we don't allow load/store instructions to trigger an interrupt.
//      // TODO: support non-MMIO load-store instructions to trigger interrupts
//      val allow_interrupts = !CommitType.isLoadStore(in.enq.req(i).bits.commitType) && !FuType.isFence(in.enq.req(i).bits.fuType) && !FuType.isCsr(in.enq.req(i).bits.fuType) && !FuType.isVset(in.enq.req(i).bits.fuType)
//      robEntries(allocatePtrVec(i).value).interruptSafe := allow_interrupts
//    }
//  }

  /**
   * read and write of data modules
   */
  val commitReadAddr_next = Mux(state_next === s_idle,
    VecInit(deqPtrVec_next.map(_.value)),
    VecInit(walkPtrVec_next.map(_.value))
  )

  exceptionGen.in.redirect <> in.redirect
  exceptionGen.in.flush := out.flushOut.valid

  val canEnqueueEG = uopCanEnqueue
  for (i <- 0 until RenameWidth) {
    exceptionGen.in.enq(i).valid := canEnqueueEG(i)
    exceptionGen.in.enq(i).bits.robIdx := in.enq.req(i).bits.robIdx
    exceptionGen.in.enq(i).bits.ftqPtr := in.enq.req(i).bits.ftqPtr
    exceptionGen.in.enq(i).bits.ftqOffset := in.enq.req(i).bits.ftqOffset
    exceptionGen.in.enq(i).bits.isRVC := in.enq.req(i).bits.isRVC
    exceptionGen.in.enq(i).bits.exceptionVec := in.enq.req(i).bits.exceptionVec
    exceptionGen.in.enq(i).bits.satpFlushFirstFetchFault := in.enq.req(i).bits.satpFlushFirstFetchFault
    exceptionGen.in.enq(i).bits.hasException := in.enq.req(i).bits.hasException
    exceptionGen.in.enq(i).bits.isEnqExcp := in.enq.req(i).bits.hasException
    exceptionGen.in.enq(i).bits.isFetchMalAddr := in.enq.req(i).bits.isFetchMalAddr
    exceptionGen.in.enq(i).bits.flushPipe := in.enq.req(i).bits.flushPipe
    exceptionGen.in.enq(i).bits.satpFlush := false.B
    exceptionGen.in.enq(i).bits.isVset := in.enq.req(i).bits.isVset
    exceptionGen.in.enq(i).bits.replayInst := false.B
    XSError(canEnqueue(i) && in.enq.req(i).bits.replayInst, "enq should not set replayInst")
    exceptionGen.in.enq(i).bits.singleStep := in.enq.req(i).bits.singleStep
    exceptionGen.in.enq(i).bits.crossPageIPFFix := in.enq.req(i).bits.crossPageIPFFix
    exceptionGen.in.enq(i).bits.trigger := in.enq.req(i).bits.trigger
    exceptionGen.in.enq(i).bits.vstartEn := false.B //DontCare
    exceptionGen.in.enq(i).bits.vstart := 0.U //DontCare
    exceptionGen.in.enq(i).bits.vuopIdx := 0.U
    exceptionGen.in.enq(i).bits.isVecLoad := false.B
    exceptionGen.in.enq(i).bits.isVlm := false.B
    exceptionGen.in.enq(i).bits.isStrided := false.B
    exceptionGen.in.enq(i).bits.isIndexed := false.B
    exceptionGen.in.enq(i).bits.isWhole := false.B
    exceptionGen.in.enq(i).bits.nf := 0.U
    exceptionGen.in.enq(i).bits.vsew := 0.U
    exceptionGen.in.enq(i).bits.veew := 0.U
    exceptionGen.in.enq(i).bits.vlmul := 0.U
  }

  println(s"ExceptionGen:")
  println(s"num of exceptions: ${params.numException}")
  require(exceptionWBs.length == exceptionGen.in.wb.length,
    f"exceptionWBs.length: ${exceptionWBs.length}, " +
      f"exceptionGen.in.wb.length: ${exceptionGen.in.wb.length}")
  for (((wb, exc_wb), i) <- exceptionWBs.zip(exceptionGen.in.wb).zipWithIndex) {
    exc_wb.valid       := wb.valid
    exc_wb.bits.robIdx := wb.bits.robIdx
    // only enq inst use ftqPtr to read gpa
    exc_wb.bits.ftqPtr          := 0.U.asTypeOf(exc_wb.bits.ftqPtr)
    exc_wb.bits.ftqOffset       := 0.U.asTypeOf(exc_wb.bits.ftqOffset)
    exc_wb.bits.isRVC           := false.B
    exc_wb.bits.exceptionVec    := wb.bits.exceptionVec
    exc_wb.bits.satpFlushFirstFetchFault := false.B
    exc_wb.bits.hasException    := wb.bits.exceptionVec.orR // Todo: use in.writebackNeedFlush(i) instead
    exc_wb.bits.isEnqExcp       := false.B
    exc_wb.bits.isFetchMalAddr  := false.B
    exc_wb.bits.flushPipe       := wb.bits.flushPipe.getOrElse(false.B)
    exc_wb.bits.satpFlush       := wb.bits.satpFlush.getOrElse(false.B)
    exc_wb.bits.isVset          := false.B
    exc_wb.bits.replayInst      := wb.bits.replay.getOrElse(false.B)
    exc_wb.bits.singleStep      := false.B
    exc_wb.bits.crossPageIPFFix := false.B
    val trigger = wb.bits.trigger.getOrElse(TriggerAction.None).asTypeOf(exc_wb.bits.trigger)
    exc_wb.bits.trigger := trigger
    exc_wb.bits.vstartEn := false.B // Todo[Vector]: support vector ls exception
    exc_wb.bits.vstart := 0.U // Todo[Vector]: support vector ls exception
    exc_wb.bits.vuopIdx :=  0.U // Todo[Vector]: support vector ls exception
    exc_wb.bits.isVecLoad := false.B // Todo[Vector]: support vector ls exception
    exc_wb.bits.isVlm := false.B // Todo[Vector]: support vector ls exception
    exc_wb.bits.isStrided := false.B // Todo[Vector]: remove it
    exc_wb.bits.isIndexed := false.B // Todo[Vector]: remove it
    exc_wb.bits.isWhole := false.B // Todo[Vector]: remove it
    exc_wb.bits.nf := 0.U // Todo[Vector]: support vector ls exception
    exc_wb.bits.vsew := 0.U // Todo[Vector]: support vector ls exception
    exc_wb.bits.veew := 0.U // Todo[Vector]: support vector ls exception
    exc_wb.bits.vlmul := 0.U // Todo[Vector]: support vector ls exception
  }

  val isCommit = out.commits.isCommit
  val isCommitReg = GatedValidRegNext(out.commits.isCommit)
  val instrCntReg = RegInit(0.U(64.W))
  val fusionCommitCountNow = out.commits.commitValid.zip(out.commits.info).zip(deqPtrVec).map {
    case ((valid, info), ptr) =>
      val formerFusionCount = debugMeta(debug_microOp(ptr.value)(0)).fusionNum
      val latterFusionCount = debugMeta(debug_microOp(ptr.value)(1)).fusionNum
      val survivingFusionCount = formerFusionCount +& Mux(
        CompressType.isNotNORMAL(info.entryPairType),
        latterFusionCount,
        0.U
      )
      Mux(valid, survivingFusionCount, 0.U)
  }.reduce(_ +& _)
  val fuseCommitCnt = RegEnable(fusionCommitCountNow, isCommit)
  val trueCommitCnt = RegEnable(out.commits.commitValid.zip(commitInstrCntByEntry).map { case (v, instrCnt) => Mux(v, instrCnt, 0.U) }.reduce(_ +& _), isCommit)
  val retireCounter = Mux(isCommitReg, trueCommitCnt, 0.U)
  val instrCnt = instrCntReg + retireCounter
  when(isCommitReg){
    instrCntReg := instrCnt
  }
  out.csr.perfinfo.retiredInstr := retireCounter
  out.robFull := !allowEnqueue

  out.toVecExcpMod.logicPhyRegMap := rab.out.toVecExcpMod.logicPhyRegMap
  out.toVecExcpMod.excpInfo := vecExcpInfo

  /**
   * trace
   */

  // trace output
  val traceValids = out.trace.traceCommitInfo.blocks.map(_.valid)
  val traceBlocks = out.trace.traceCommitInfo.blocks
  val traceBlockInPipe = out.trace.traceCommitInfo.blocks.map(_.bits.tracePipe)

  for (i <- 0 until CommitWidth) {
    traceBlocks(i).bits.ftqIdx.foreach(_ := rawInfo(i).ftqIdx)
    traceBlocks(i).bits.ftqOffset.foreach(_ := rawInfo(i).ftqOffset)
    traceBlockInPipe(i).itype := rawInfo(i).traceBlockInPipe.itype
    traceBlockInPipe(i).iretire := iretireCommit(i)
    traceBlockInPipe(i).ilastsize := rawInfo(i).traceBlockInPipe.ilastsize
    traceValids(i) := out.commits.isCommit && out.commits.commitValid(i)
    // exception only occur in block(0).
    if(i == 0) {
      when(out.exception.valid){ // trace exception
        traceBlocks(i).bits.tracePipe.itype := Mux(out.exception.bits.isInterrupt,
          Itype.Interrupt,
          Itype.Exception
        )
        traceValids(i) := true.B
        traceBlockInPipe(i).iretire := 0.U
      }
    }
  }

  /**
   * debug info
   */
  XSDebug(p"enqPtr ${enqPtr} deqPtr ${deqPtr}\n")
  XSDebug("")
  XSError(enqPtr.isBeforeSlot(deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")
  for (i <- 0 until RobSize) {
    XSDebug(false, !robEntries(i).valid, "-")
    XSDebug(false, robEntries(i).valid && robEntries(i).isWritebacked, "w")
    XSDebug(false, robEntries(i).valid && !robEntries(i).isWritebacked, "v")
  }
  XSDebug(false, true.B, "\n")

  for (i <- 0 until RobSize) {
    if (i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x ", debugMeta(debug_microOp(i).head).pc)
    XSDebug(false, !robEntries(i).valid, "- ")
    XSDebug(false, robEntries(i).valid && robEntries(i).isWritebacked, "w ")
    XSDebug(false, robEntries(i).valid && !robEntries(i).isWritebacked, "v ")
    if (i % 4 == 3) XSDebug(false, true.B, "\n")
  }

  def ifCommit(counter: UInt): UInt = Mux(isCommit, counter, 0.U)

  def ifCommitReg(counter: UInt): UInt = Mux(isCommitReg, counter, 0.U)

  val commitDebugUop = deqPtrVec.map(_.value).map(debug_microOp(_)).flatten
  val commitSlotValid = out.commits.commitValid.zip(out.commits.info).flatMap { case (valid, info) =>
    Seq(valid, valid && !CompressType.isNORMAL(info.entryPairType))
  }
  XSPerfAccumulate("clock_cycle", 1.U, XSPerfLevel.CRITICAL)
  QueuePerf(RobSize, numValidEntries, numValidEntries === RobSize.U)
  XSPerfAccumulate("commitUop", ifCommit(commitCnt))
  XSPerfAccumulate("commitInstr", ifCommitReg(trueCommitCnt), XSPerfLevel.CRITICAL)
  XSPerfRolling("ipc", ifCommitReg(trueCommitCnt), 1000, clock, reset)
  XSPerfRolling("cpi", perfCnt = 1.U /*Cycle*/ , eventTrigger = ifCommitReg(trueCommitCnt), granularity = 1000, clock, reset)
  XSPerfAccumulate("commitInstrFused", ifCommitReg(fuseCommitCnt))
  val commitIsLoad = deqPtrVec.map(ptr => debug_microOp(ptr.value)(0).commitType === CommitType.LOAD)
  val commitLoadValid = commitSlotValid.zip(commitDebugUop).map { case (v, uop) => v && uop.commitType === CommitType.LOAD }
  XSPerfAccumulate("commitInstrLoad", ifCommit(PopCount(commitLoadValid)))
  val commitIsBranch = deqPtrVec.map(ptr => FuType.isBrh(debug_microOp(ptr.value)(0).fuType))
  val commitBranchValid = commitSlotValid.zip(commitDebugUop).map { case (v, uop) => v && FuType.isBrh(uop.fuType) }
  XSPerfAccumulate("commitInstrBranch", ifCommit(PopCount(commitBranchValid)))
  val commitIsStore = deqPtrVec.map(ptr => debug_microOp(ptr.value)(0).commitType === CommitType.STORE)
  val commitStoreValid = commitSlotValid.zip(commitDebugUop).map { case (v, uop) => v && uop.commitType === CommitType.STORE }
  XSPerfAccumulate("commitInstrStore", ifCommit(PopCount(commitStoreValid)))
  XSPerfAccumulate("writeback", PopCount((0 until RobSize).map(i => robEntries(i).valid && robEntries(i).isWritebacked)))
  // XSPerfAccumulate("enqInstr", PopCount(io.dp1Req.map(_.fire)))
  // XSPerfAccumulate("d2rVnR", PopCount(io.dp1Req.map(p => p.valid && !p.ready)))
  XSPerfAccumulate("walkInstr", Mux(out.commits.isWalk, PopCount(out.commits.walkValid), 0.U))
  XSPerfAccumulate("walkCycleTotal", state === s_walk)
  XSPerfAccumulate("waitRabWalkEnd", state === s_walk && walkFinished && !rab.in.status.walkEnd)
  private val walkCycle = RegInit(0.U(8.W))
  private val waitRabWalkCycle = RegInit(0.U(8.W))
  walkCycle := Mux(in.redirect.valid, 0.U, Mux(state === s_walk, walkCycle + 1.U, 0.U))
  waitRabWalkCycle := Mux(state === s_walk && walkFinished, 0.U, Mux(state === s_walk, walkCycle + 1.U, 0.U))

  XSPerfHistogram("walkRobCycleHist", walkCycle, state === s_walk && walkFinished, 0, 32)
  XSPerfHistogram("walkRabExtraCycleHist", waitRabWalkCycle, state === s_walk && walkFinished && rab.in.status.walkEnd, 0, 32)
  XSPerfHistogram("walkTotalCycleHist", walkCycle, state === s_walk && state_next === s_idle, 0, 32)

  private val deqNotWritebacked = robEntries(deqPtr.value).valid && !robEntries(deqPtr.value).isWritebacked
  private val deqUopNotWritebacked = robEntries(deqPtr.value).valid && !robEntries(deqPtr.value).isUopWritebacked
  private val deqHeadInfo = debug_microOp(deqPtr.value).head
  private val deqHeadInfoFuType = deqHeadInfo.fuType
  val deqUopCommitType = deqHeadInfo.commitType

  XSPerfAccumulate("waitAluCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.alu.U)
  XSPerfAccumulate("waitMulCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.mul.U)
  XSPerfAccumulate("waitDivCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.div.U)
  XSPerfAccumulate("waitBrhCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.brh.U)
  XSPerfAccumulate("waitJmpCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.jmp.U)
  XSPerfAccumulate("waitLinkCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.link.U)
  XSPerfAccumulate("waitCsrCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.csr.U)
  XSPerfAccumulate("waitFenCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.fence.U)
  XSPerfAccumulate("waitBkuCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.bku.U)
  XSPerfAccumulate("waitLduCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.ldu.U)
  XSPerfAccumulate("waitStuCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.stu.U)
  XSPerfAccumulate("waitAtmCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.mou.U)

  XSPerfAccumulate("waitfaluCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.falu.U)
  XSPerfAccumulate("waitfmulCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.fmul.U)
  XSPerfAccumulate("waitfcvtCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.fcvt.U)
  XSPerfAccumulate("waitfDivSqrtCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.fDivSqrt.U)
  XSPerfAccumulate("waitfcmpCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.fcmp.U)

  XSPerfAccumulate("waitVfmacCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.vfmac.U)
  XSPerfAccumulate("waitVfdivCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.vfdiv.U)

  XSPerfAccumulate("waitNormalCycle", deqNotWritebacked && deqUopCommitType === CommitType.NORMAL)
  XSPerfAccumulate("waitBranchCycle", deqNotWritebacked && FuType.isBrh(deqHeadInfoFuType))
  XSPerfAccumulate("waitLoadCycle", deqNotWritebacked   && deqUopCommitType === CommitType.LOAD)
  XSPerfAccumulate("waitStoreCycle", deqNotWritebacked  && deqUopCommitType === CommitType.STORE)
  XSPerfReference("robHeadPC",
    RegEnable(out.commits.info(0).debug_pc.getOrElse(0.U),
              0.U,
              out.commits.isCommit && out.commits.commitValid(0)))
  XSPerfAccumulate("commitCompressCntAll", PopCount(out.commits.commitValid.zip(commitInstrCntByEntry).map { case (valid, instrCnt) => out.commits.isCommit && valid && instrCnt > 1.U }))
  (1 to RenameWidth).foreach(i =>
    XSPerfAccumulate(s"commitCompressCnt${i}", PopCount(out.commits.commitValid.zip(commitInstrCntByEntry).map { case (valid, instrCnt) => out.commits.isCommit && valid && instrCnt === i.U }))
  )
  XSPerfAccumulate("compressSize", out.commits.commitValid.zip(commitInstrCntByEntry).map { case (valid, instrCnt) => Mux(out.commits.isCommit && valid && instrCnt > 1.U, instrCnt, 0.U) }.reduce(_ +& _))
  val compressTypeIsNORMAL = out.commits.commitValid.zip(out.commits.info).map(x => x._1 && CompressType.isNORMAL(x._2.entryPairType))
  val compressTypeIsCC = out.commits.commitValid.zip(out.commits.info).map(x => x._1 && CompressType.isCC(x._2.entryPairType))
  val compressTypeIsCS = out.commits.commitValid.zip(out.commits.info).map(x => x._1 && CompressType.isCS(x._2.entryPairType))
  val compressTypeIsSC = out.commits.commitValid.zip(out.commits.info).map(x => x._1 && CompressType.isSC(x._2.entryPairType))
  XSPerfAccumulate("compressType_NORMAL", ifCommit(PopCount(compressTypeIsNORMAL)))
  XSPerfAccumulate("compressType_CC", ifCommit(PopCount(compressTypeIsCC)))
  XSPerfAccumulate("compressType_CS", ifCommit(PopCount(compressTypeIsCS)))
  XSPerfAccumulate("compressType_SC", ifCommit(PopCount(compressTypeIsSC)))
  XSPerfAccumulate("RobEntriesCommited", ifCommit(PopCount(out.commits.commitValid)))

  // By CommitType
//  val commitIsLoad   = out.commits.info.map(x => x.commitType === CommitType.LOAD)
//  val commitIsStore  = out.commits.info.map(x => x.commitType === CommitType.STORE)
//  val commitIsBranch = out.commits.info.map(x => x.commitType === CommitType.BRANCH)
  val commitIsNormal = out.commits.info.map(x => x.commitType === CommitType.NORMAL)

  XSPerfAccumulate("NoCompressIsLoad", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsLoad).map(x => x._1 && x._2))))
  XSPerfAccumulate("NoCompressIsStore", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsStore).map(x => x._1 && x._2))))
  XSPerfAccumulate("NoCompressIsBranch", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsBranch).map(x => x._1 && x._2))))
  XSPerfAccumulate("NoCompressIsNORMAL", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsNormal).map(x => x._1 && x._2))))

  // By FuType
  for (fuType <- FuType.functionNameMap.keys) {
    val fuName = FuType.functionNameMap(fuType)
    val commitIsFuType = compressTypeIsNORMAL.zip(deqPtrVec.map(_.value).map(debug_microOp(_).head)).map {
      case (commit, uop) => commit && uop.fuType === fuType.U
    }
    XSPerfAccumulate(s"NoCompress_$fuName", ifCommit(PopCount(commitIsFuType)))
    val NoCompress_Source = commitIsFuType.zip(out.commits.info).map {
      case (valid, info) =>
        val noEnoughInstr  = valid && info.noCompressReason === NoCompressReason.noEnoughInstr
        val flushedHalf    = valid && info.noCompressReason === NoCompressReason.flushedHalf
        val cannotCompress = valid && info.noCompressReason === NoCompressReason.cannotCompress
        (noEnoughInstr, flushedHalf, cannotCompress)
    }
    XSPerfAccumulate(s"NoCompress_${fuName}_by_noEnoughInstr", ifCommit(PopCount(NoCompress_Source.map(_._1))))
    XSPerfAccumulate(s"NoCompress_${fuName}_by_flushedHalf", ifCommit(PopCount(NoCompress_Source.map(_._2))))
    XSPerfAccumulate(s"NoCompress_${fuName}_by_cannotCompress", ifCommit(PopCount(NoCompress_Source.map(_._3))))
  }

  val dispatchLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.dispatchTime - debugMeta(uop).perfDebugInfo.renameTime)
  val enqRsLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.enqRsTime - debugMeta(uop).perfDebugInfo.dispatchTime)
  val selectLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.selectTime - debugMeta(uop).perfDebugInfo.enqRsTime)
  val issueLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.issueTime - debugMeta(uop).perfDebugInfo.selectTime)
  val executeLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.writebackTime - debugMeta(uop).perfDebugInfo.issueTime)
  val rsFuLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.writebackTime - debugMeta(uop).perfDebugInfo.enqRsTime)
  val commitLatency = commitDebugUop.map(uop => timer - debugMeta(uop).perfDebugInfo.writebackTime)
  val tlbLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.tlbRespTime - debugMeta(uop).perfDebugInfo.tlbFirstReqTime)

  def latencySum(cond: Seq[Bool], latency: Seq[UInt]): UInt = {
    cond.zip(latency).map(x => Mux(x._1, x._2, 0.U)).reduce(_ +& _)
  }

  for (fuType <- FuType.functionNameMap.keys) {
    val fuName = FuType.functionNameMap(fuType)
    val commitIsFuType = commitSlotValid.zip(commitDebugUop).map(x => x._1 && x._2.fuType === fuType.U)
    XSPerfRolling(s"ipc_futype_${fuName}", ifCommit(PopCount(commitIsFuType)), 1000, clock, reset)
    XSPerfAccumulate(s"${fuName}_instr_cnt", ifCommit(PopCount(commitIsFuType)))
    XSPerfAccumulate(s"${fuName}_latency_dispatch", ifCommit(latencySum(commitIsFuType, dispatchLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs", ifCommit(latencySum(commitIsFuType, enqRsLatency)))
    XSPerfAccumulate(s"${fuName}_latency_select", ifCommit(latencySum(commitIsFuType, selectLatency)))
    XSPerfAccumulate(s"${fuName}_latency_issue", ifCommit(latencySum(commitIsFuType, issueLatency)))
    XSPerfAccumulate(s"${fuName}_latency_execute", ifCommit(latencySum(commitIsFuType, executeLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs_execute", ifCommit(latencySum(commitIsFuType, rsFuLatency)))
    XSPerfAccumulate(s"${fuName}_latency_commit", ifCommit(latencySum(commitIsFuType, commitLatency)))
  }
  XSPerfAccumulate(s"redirect_use_snapshot", in.redirect.valid && in.snpt.useSnpt)

  // top-down info
  out.debugTopDown.toCore.robHeadVaddr.valid := debug_lsTopdownInfo(deqPtr.value).s1.vaddr_valid
  out.debugTopDown.toCore.robHeadVaddr.bits := debug_lsTopdownInfo(deqPtr.value).s1.vaddr_bits
  out.debugTopDown.toCore.robHeadPaddr.valid := debug_lsTopdownInfo(deqPtr.value).s2.paddr_valid
  out.debugTopDown.toCore.robHeadPaddr.bits := debug_lsTopdownInfo(deqPtr.value).s2.paddr_bits
  out.debugTopDown.toDispatch.robTrueCommit := ifCommitReg(trueCommitCnt)
  out.debugTopDown.toDispatch.robHeadLsIssue := debug_lsIssue(deqPtr.value)
  out.debugTopDown.robHeadLqIdx.valid := debug_lqIdxValid(deqPtr.value)(0)
  out.debugTopDown.robHeadLqIdx.bits := debug_lqIdx(deqPtr.value)(0)

  // rolling
  out.debugRolling.robTrueCommit := ifCommitReg(trueCommitCnt)

  // debug trigger for simulation
  if (backendParams.debugEn) {
    val debug_sim_trig = dontTouch(Wire(Bool()))
    debug_sim_trig := out.commits.isCommit && (commitSlotValid zip commitDebugUop map {
      case (valid, uop) => valid && debugMeta(uop).debug_sim_trig
    }).reduce(_ || _)
  }

  /**
   * DataBase info:
   * log trigger is at writeback valid
   * */
  if (!env.FPGAPlatform) {
    val instTableName = "InstTable" + p(XSCoreParamsKey).HartId.toString
    val instSiteName = "Rob" + p(XSCoreParamsKey).HartId.toString
    val debug_instTable = ChiselDB.createTable(instTableName, new InstInfoEntry, basicDB = true)
    for (wb <- exuWBs) {
      when(wb.valid) {
        val debug_instData = Wire(new InstInfoEntry)
        val idx = wb.bits.robIdx.value
        debug_instData.robIdx := idx
        debug_instData.dvaddr := wb.bits.debug.vaddr
        debug_instData.dpaddr := wb.bits.debug.paddr
        debug_instData.issueTime := wb.bits.perfDebugInfo.get.issueTime
        debug_instData.writebackTime := wb.bits.perfDebugInfo.get.writebackTime
        debug_instData.dispatchLatency := wb.bits.perfDebugInfo.get.dispatchTime - wb.bits.perfDebugInfo.get.renameTime
        debug_instData.enqRsLatency := wb.bits.perfDebugInfo.get.enqRsTime - wb.bits.perfDebugInfo.get.dispatchTime
        debug_instData.selectLatency := wb.bits.perfDebugInfo.get.selectTime - wb.bits.perfDebugInfo.get.enqRsTime
        debug_instData.issueLatency := wb.bits.perfDebugInfo.get.issueTime - wb.bits.perfDebugInfo.get.selectTime
        debug_instData.executeLatency := wb.bits.perfDebugInfo.get.writebackTime - wb.bits.perfDebugInfo.get.issueTime
        debug_instData.rsFuLatency := wb.bits.perfDebugInfo.get.writebackTime - wb.bits.perfDebugInfo.get.enqRsTime
        debug_instData.tlbLatency := wb.bits.perfDebugInfo.get.tlbRespTime - wb.bits.perfDebugInfo.get.tlbFirstReqTime
        debug_instData.exceptType := wb.bits.exceptionVec.asUInt
        debug_instData.lsInfo := debug_lsInfo(idx)
        // debug_instData.globalID := wb.bits.uop.ctrl.debug_globalID
        // debug_instData.instType := wb.bits.uop.ctrl.fuType
        // debug_instData.ivaddr := wb.bits.uop.cf.pc
        // debug_instData.mdpInfo.ssid := wb.bits.uop.cf.ssid
        // debug_instData.mdpInfo.waitAllStore := wb.bits.uop.cf.loadWaitStrict && wb.bits.uop.cf.loadWaitBit
        debug_instTable.log(
          data = debug_instData,
          en = wb.valid,
          site = instSiteName,
          clock = clock,
          reset = reset
        )
      }
    }
  }

  // log when committing
  val load_debug_table = ChiselDB.createTable("LoadDebugTable" + p(XSCoreParamsKey).HartId.toString, new LoadInfoEntry, basicDB = true)
  for (i <- 0 until CommitWidth) {
    val commit_index = out.commits.robIdx(i).value
    for (j <- 0 until 2) {
      val slot = 2 * i + j
      val uop = commitDebugUop(slot)
      val log_enable = out.commits.isCommit && commitSlotValid(slot) && uop.commitType === CommitType.LOAD
      val load_debug_data = Wire(new LoadInfoEntry)

      load_debug_data.pc := debugMeta(uop).pc
      load_debug_data.vaddr := debug_lsTopdownInfo(commit_index).s1.vaddr_bits
      load_debug_data.paddr := debug_lsTopdownInfo(commit_index).s2.paddr_bits
      load_debug_data.cacheMiss := debug_lsTopdownInfo(commit_index).s2.first_real_miss
      load_debug_data.tlbQueryLatency := tlbLatency(slot)
      load_debug_data.exeLatency := executeLatency(slot)

      load_debug_table.log(
        data = load_debug_data,
        en = log_enable,
        site = "LoadDebugTable",
        clock = clock,
        reset = reset
      )
    }
  }

  val debug_VecOtherPdest = RegInit(VecInit.fill(RobSize)(VecInit.fill(8)(0.U(PhyRegIdxWidth.W))))

  // topdown
  val notIssue = !debug_lsIssue(deqPtr.value)
  val tlbReplay = in.debugTopDown.fromCore.fromMem.robHeadTlbReplay
  val tlbMiss = in.debugTopDown.fromCore.fromMem.robHeadTlbMiss
  val vioReplay = in.debugTopDown.fromCore.fromMem.robHeadLoadVio
  val mshrReplay = in.debugTopDown.fromCore.fromMem.robHeadLoadMSHR
  val l1Miss = in.debugTopDown.fromCore.fromMem.robHeadMissInDCache
  val l2Miss = in.debugTopDown.fromCore.l2MissMatch
  val l3Miss = in.debugTopDown.fromCore.l3MissMatch
  val enableTea = Constantin.createRecord(s"enableTea$hartId", false)
  val enableTeaStats = Constantin.createRecord(s"enableTeaStats$hartId", false)
  if (!backendParams.teaTrackerEn) {
    assert(!enableTea.asBool, "TEA tracker was not elaborated; rebuild with WITH_TEA=1")
  }
  if (!backendParams.teaCycleStatsEn) {
    assert(!enableTeaStats.asBool, "TEA cycle stats were not elaborated; rebuild with WITH_TEA_STATS=1")
  }
  if (backendParams.teaTrackerEn || backendParams.dipEn || backendParams.teaCycleStatsEn) {
    val teaCycle = Wire(UInt(64.W))
    if (!backendParams.teaTrackerEn) {
      val cycle = RegInit(0.U(64.W))
      cycle := cycle + 1.U
      teaCycle := cycle
    }
    val teaRedirectPtr = in.redirect.bits.robIdx
    val teaFlushPtr = deqExceptionRobIdx
    val teaRedirectCtrlMispred = in.redirect.valid &&
      in.redirect.bits.debugIsCtrl && in.redirect.bits.isMisPred
    val teaRedirectMemVio = in.redirect.valid && in.redirect.bits.debugIsMemVio
    val teaExceptionFlush = out.flushOut.valid && deqHasException && !intrEnable
    val teaRobFlushEcho = RegNext(out.flushOut.valid, false.B)
    val hasTeaCommit = out.commits.isCommit && out.commits.commitValid.asUInt.orR
    val teaRetired = out.commits.commitValid.zip(commitInstrCntByEntry).map { case (valid, count) =>
      Mux(out.commits.isCommit && valid, count, 0.U)
    }.reduce(_ +& _)
    val teaCommits = Wire(Vec(2 * CommitWidth, Valid(new RobPtr)))
    for (slot <- 0 until 2 * CommitWidth) {
      teaCommits(slot).valid := out.commits.isCommit && commitSlotValid(slot)
      teaCommits(slot).bits := out.commits.robIdx(slot / 2)
      teaCommits(slot).bits.isFormer := (slot % 2 == 0).B
    }
    // Recovery events identify the instruction; a compressed slot pointer alone does not.
    // Control redirects carry the fetch-block PC; memory redirects already carry the instruction PC.
    val teaRedirectInstrPc = in.redirect.bits.pc +
      Mux(in.redirect.bits.debugIsCtrl, SignExt(in.redirect.bits.getPcOffset(), GuardedVAddrBits), 0.U)
    val teaRedirectPc = in.debugInstrAddrTransType.extend(teaRedirectInstrPc(VAddrBits - 1, 0), XLEN)
    val teaExceptionPc = in.debugInstrAddrTransType.extend(exceptionDebug.pc, XLEN)
    val teaRedirectPsvDefault = Mux(teaRedirectCtrlMispred, TeaEvent.bit(TeaEvent.FL_MB),
      Mux(teaRedirectMemVio, TeaEvent.bit(TeaEvent.FL_MO), TeaPsvOps.empty))
    val teaRedirectPsv = WireDefault(teaRedirectPsvDefault)
    val teaExceptionPsv = WireDefault(TeaEvent.bit(TeaEvent.FL_EX))

    out.dip.foreach { dip =>
      dip := 0.U.asTypeOf(new DipRob)
      dip.cycle := teaCycle
      dip.retired := teaRetired
      dip.commits := teaCommits
      // ROB flush is observed before the later redirect echo clears state.
      dip.flush.valid := out.flushOut.valid || in.redirect.valid && !teaRobFlushEcho
      dip.flush.bits.robIdx := Mux(out.flushOut.valid, out.flushOut.bits.robIdx, teaRedirectPtr)
      dip.flush.bits.self := Mux(out.flushOut.valid, out.flushOut.bits.flushItself(), in.redirect.bits.flushItself())
      dip.flush.bits.pc := Mux(out.flushOut.valid, teaExceptionPc, teaRedirectPc)
      dip.flush.bits.cause := Mux(out.flushOut.valid,
        Mux(teaExceptionFlush, 2.U, Mux(intrEnable, 16.U, 8.U)),
        Mux(teaRedirectCtrlMispred, 1.U, Mux(teaRedirectMemVio, 4.U, 8.U)))
    }

    val teaVisibleCause = WireInit(0.U.asTypeOf(new TeaFlushCause))
    if (backendParams.teaTrackerEn || backendParams.teaCycleStatsEn) {
      val current = WireInit(0.U.asTypeOf(new TeaFlushCause))
      when(teaRedirectCtrlMispred || teaRedirectMemVio) {
        current.valid := true.B
        current.pc := teaRedirectPc
        current.psv := teaRedirectPsv
      }.elsewhen(teaExceptionFlush) {
        current.valid := true.B
        current.pc := teaExceptionPc
        current.psv := teaExceptionPsv
      }
      val invalid = out.flushOut.valid || in.redirect.valid && !teaRobFlushEcho
      val refill = canEnqueue.asUInt.orR && !in.redirect.valid && state =/= s_walk
      val saved = RegInit(0.U.asTypeOf(new TeaFlushCause))
      when(current.valid) {
        saved := current
      }.elsewhen(invalid || refill) {
        saved.valid := false.B
      }
      teaVisibleCause := saved
      when(current.valid) {
        teaVisibleCause := current
      }.elsewhen(invalid) {
        teaVisibleCause.valid := false.B
      }
    }
    val teaState = TeaState.classify(isEmpty, state === s_walk || in.redirect.valid,
      hasTeaCommit, teaVisibleCause.valid)
    if (backendParams.teaTrackerEn) {
      case class TeaPsvSetSource(valid: Bool, robIdx: RobPtr, mask: UInt)
      val teaPsvSetSources = scala.collection.mutable.ArrayBuffer(
        TeaPsvSetSource(teaRedirectCtrlMispred, teaRedirectPtr, TeaEvent.bit(TeaEvent.FL_MB)),
        TeaPsvSetSource(teaRedirectMemVio, teaRedirectPtr, TeaEvent.bit(TeaEvent.FL_MO)),
        TeaPsvSetSource(teaExceptionFlush, teaFlushPtr, TeaEvent.bit(TeaEvent.FL_EX))
      )
      for (i <- 0 until (LduCnt + StaCnt)) {
        val lsInfo = in.debug_ls.debugLsInfo(i)
        teaPsvSetSources += TeaPsvSetSource(lsInfo.s1_isTlbMiss, lsInfo.s1_robIdx, TeaEvent.bit(TeaEvent.ST_TLB))
        teaPsvSetSources += TeaPsvSetSource(lsInfo.s2_isDcacheFirstMiss, lsInfo.s2_robIdx, TeaEvent.bit(TeaEvent.ST_L1))
      }
      val teaSelector = Module(new TeaSampleSelector)
      teaSelector.io.enable := enableTea.asBool
      teaSelector.io.samplePeriod := Constantin.createRecord(s"teaSamplePeriod$hartId", 256)
      teaSelector.io.state := teaState
      teaSelector.io.flushCause := teaVisibleCause
      teaCycle := teaSelector.io.cycle
      val tracker = Module(new TeaMemberTracker(in.writeback.size, teaPsvSetSources.length))
      // Keep outstanding sampled members alive when new sampling is disabled.
      val started = RegInit(false.B)
      when(enableTea.asBool) { started := true.B }
      tracker.io.enable := enableTea.asBool || started
      tracker.io.cycle := teaCycle
      tracker.io.addrTransType := in.debugInstrAddrTransType
      tracker.io.head := deqPtr
      tracker.io.redirect := in.redirect
      tracker.io.readPtr(0) := teaRedirectPtr
      tracker.io.readPtr(1) := teaFlushPtr
      tracker.io.readPc(0) := teaRedirectPc
      tracker.io.readPc(1) := teaExceptionPc
      teaRedirectPsv := Mux(tracker.io.read(0).valid, tracker.io.read(0).bits, teaRedirectPsvDefault)
      teaExceptionPsv := Mux(tracker.io.read(1).valid, tracker.io.read(1).bits, TeaEvent.bit(TeaEvent.FL_EX))
      // Keep the helper input independent of its read-valid outputs used by state classification.
      tracker.io.markStalled := teaSelector.io.sampleValid && !isEmpty &&
        state =/= s_walk && !in.redirect.valid && !hasTeaCommit
      teaSelector.io.stalledTarget := tracker.io.stalled

      for (lane <- 0 until RenameWidth) {
        val member = in.teaMembers.get(lane)
        tracker.io.alloc(lane).valid := member.valid && !in.redirect.valid &&
          VecInit(in.enq.req.zip(canEnqueue).map { case (req, accept) =>
            accept && req.bits.robIdx.isSameEntry(member.bits.robIdx)
          }).asUInt.orR
        tracker.io.alloc(lane).bits := member.bits
        val req = in.enq.req(lane)
        tracker.io.enqPsv(lane).valid := uopCanEnqueue(lane) && !in.redirect.valid
        tracker.io.enqPsv(lane).bits.robIdx := req.bits.robIdx
        tracker.io.enqPsv(lane).bits.rank := req.bits.debug.get.perfDebugInfo.teaMemberRank
        tracker.io.enqPsv(lane).bits.mask := req.bits.teaPsv
      }
      tracker.io.writeback.zip(in.writeback).foreach { case (sink, source) =>
        sink.valid := source.valid
        sink.bits.robIdx := source.bits.robIdx
        sink.bits.rank := source.bits.perfDebugInfo.get.teaMemberRank
      }
      tracker.io.events.zip(teaPsvSetSources).foreach { case (sink, source) =>
        sink.valid := source.valid
        sink.bits.robIdx := source.robIdx
        sink.bits.mask := source.mask
      }
      for (entry <- 0 until RobSize) {
        tracker.io.slotReady(entry)(0) := !robEntries(entry).formerUopNum.orR
        tracker.io.slotReady(entry)(1) := !robEntries(entry).latterUopNum.orR
      }
      tracker.io.commits := teaCommits
      teaSelector.io.commitMask := VecInit(tracker.io.committed.map(_.valid)).asUInt
      teaSelector.io.commitPc := VecInit(tracker.io.committed.map(_.bits.pc))
      teaSelector.io.commitPsv := VecInit(tracker.io.committed.map(_.bits.psv))
      for (lane <- 0 until CommitWidth) {
        val observed = PopCount(tracker.io.committed.slice(lane * RenameWidth, (lane + 1) * RenameWidth).map(_.valid))
        // Entries allocated before ROI have no profiling history.
        when(out.commits.isCommit && out.commits.commitValid(lane) && observed.orR) {
          assert(observed ===
            commitInstrCntByEntry(lane), "TEA members disagree with architectural retirement count")
        }
      }
      val sample = WireInit(teaSelector.io.sample)
      sample.context.retired := teaRetired
      // Bit order is part of the trace schema; preserve overlapping raw conditions.
      sample.context.commitBlockMask := VecInit(Seq(misPredBlock, lastCycleFlush, hasWFI,
        in.redirect.valid, deqNeedFlush && !deqHasFlushed, deqFlushBlock,
        criticalErrorState, traceBlock, state === s_walk)).asUInt
      sample.context.headReady := !isEmpty && robEntries(deqPtr.value).isWritebacked
      sample.context.headReasonValid := !isEmpty && !hasTeaCommit && out.debugRobHeadStall.get.valid
      sample.context.headReason := Mux(sample.context.headReasonValid, out.debugRobHeadStall.get.bits, 0.U)

      val teaTable = ChiselDB.createTable(s"Tea_$hartId", new TeaEntry, basicDB = true)
      teaTable.log(sample, teaSelector.io.sampleValid, "", clock, reset)
      val finalTable = ChiselDB.createTable(s"TeaDelayedFinal_$hartId", new TeaDelayedFinal, basicDB = true)
      tracker.io.finals.zipWithIndex.foreach { case (res, rank) =>
        finalTable.log(res, s"member$rank", clock, reset)
      }
      finalTable.log(teaSelector.io.drainFinal, "drain", clock, reset)
    }
    if (backendParams.teaCycleStatsEn) {
      val teaGapThreshold = Constantin.createRecord(s"teaGapThreshold$hartId", 128)
      val teaStatsEnable = enableTeaStats.asBool || enableTea.asBool
      XSLog.registerCaller { control =>
        import utility.XSLog.tapOrGet
        val teaClock = tapOrGet(clock)
        val teaReset = tapOrGet(reset)
        withClockAndReset(teaClock, teaReset) {
          val stats = Module(new TeaCycleStats)
          stats.io.enable := tapOrGet(teaStatsEnable)
          stats.io.clean := control.clean
          stats.io.dump := control.dump
          stats.io.cycle := tapOrGet(teaCycle)
          stats.io.state := tapOrGet(teaState)
          stats.io.retired := tapOrGet(teaRetired)
          stats.io.gapThreshold := tapOrGet(teaGapThreshold)
          val summaryTable = ChiselDB.createTable(s"TeaCycleSummary_$hartId", new TeaCycleSummary, basicDB = true)
          summaryTable.log(stats.io.summary, "summary", teaClock, teaReset)
          val intervalTable = ChiselDB.createTable(s"TeaNoCommit_$hartId", new TeaNoCommit, basicDB = true)
          intervalTable.log(stats.io.interval, "interval", teaClock, teaReset)
        }
      }

    }
  }
  val ldReason = Mux(l3Miss, LoadMemStall.id.U,
    Mux(l2Miss, LoadL3Stall.id.U,
      Mux(l1Miss, LoadL2Stall.id.U,
        Mux(notIssue, MemNotReadyStall.id.U,
          Mux(tlbMiss, LoadTLBStall.id.U,
            Mux(tlbReplay, LoadTLBStall.id.U,
              Mux(mshrReplay, LoadMSHRReplayStall.id.U,
                Mux(vioReplay, LoadVioReplayStall.id.U,
                  LoadL1Stall.id.U))))))))


  val allIssueParams = backendParams.allIssueParams.filter(_.StdCnt == 0)
  val allExuParams = allIssueParams.map(_.exuBlockParams).flatten
  val allFuConfigs = allExuParams.map(_.fuConfigs).flatten.toSet.toSeq
  val sortedFuConfigs = allFuConfigs.sortBy(_.fuType.id)
  val sortedFulatency = sortedFuConfigs.map(_.latency.latencyVal.getOrElse(4).asUInt)
  val bypassLatency = 3.U
  val writeBackLatency = 1.U


  if (backendParams.debugEn) {
    val topdownRobInfoCollect = Module(new TopdownRobInfoCollect(backendParams.iqEntryNum, RobSize))
    topdownRobInfoCollect.io.in.zip(in.topdownIQInfoVec.get).foreach{ case (sink, source) =>
      sink.valid := source.valid
      sink.robIdx := source.bits.robIdx.value
      sink.robFlag := source.bits.robIdx.flag
      sink.issued := source.bits.issued
      sink.cancelSource := source.bits.cancelSource
      sink.idealIssueTime := source.bits.idealIssueTime
    }
    val topdownRobCandidate = topdownRobInfoCollect.io.out
    for (i <- 0 until RobSize) {
      when(robEntries(i).valid){
        val hasWriteBack = robEntries(i).isWritebacked
        val isRobHead = i.U === deqPtr.value

        val topdownIQInfoCandidate = Mux(topdownRobCandidate(i).valid, topdownRobCandidate(i),
          0.U.asTypeOf(new TopdownRobInfo))
        val topdownIQInfoValid = topdownIQInfoCandidate.valid
        val topdownIQIssued = topdownIQInfoCandidate.issued
        val topdownIQIdealIssue = topdownIQInfoCandidate.idealIssueTime

        val topdownIQCancelSource = topdownIQInfoCandidate.cancelSource

        val topdownIssuedUpdate = topdownIQIssued || robEntries(i).topdownIssued.get
        val topdownCanceled= !topdownIQIssued && robEntries(i).topdownIssued.get && topdownIQInfoValid &&
          topdownIQCancelSource =/= IQCancelSource.none
        val topdownCanceledUpdate = topdownCanceled || robEntries(i).topdownCanceled.get
        val topdownRobHeadUpdate = isRobHead || robEntries(i).topdownRobHead.get
        val topdownIdealIssueUpdate = topdownIQIdealIssue || robEntries(i).topdownIdealIssue.get
        val topdownCancelSrcUpdate = Mux(topdownCanceled, topdownIQCancelSource,
          robEntries(i).topdownCancelSource.get)
        val topdownCancelTimeUpdate = topdownCanceled
        robEntries(i).topdownCanceled.foreach(_ := topdownCanceledUpdate)
        robEntries(i).topdownIssued.foreach(_ := topdownIssuedUpdate)
        robEntries(i).topdownRobHead.foreach(_ := topdownRobHeadUpdate)
        robEntries(i).topdownIdealIssue.foreach(_ := topdownIdealIssueUpdate)

        robEntries(i).topdownIssueTime.foreach(_ := Mux(hasWriteBack , 0.U,
          robEntries(i).topdownIssueTime.get +& topdownIssuedUpdate))
        robEntries(i).topdownLastIssueTime.foreach(_ := Mux(hasWriteBack , 0.U,
          Mux(topdownCanceled, 0.U,
            robEntries(i).topdownLastIssueTime.get +& topdownIssuedUpdate)
        ))
        robEntries(i).topdownRobHeadTime.foreach(_ := Mux(hasWriteBack , 0.U,
          robEntries(i).topdownRobHeadTime.get +& topdownRobHeadUpdate))
        // only caculate rising edge of ideal issue
        robEntries(i).topdownIdealIssueTime.foreach(_ := Mux(hasWriteBack , 0.U,
          Mux(topdownIQIdealIssue && !robEntries(i).topdownIdealIssue.get , 1.U,
            robEntries(i).topdownIdealIssueTime.get +& robEntries(i).topdownIdealIssue.get)
        ))
        robEntries(i).topdownCancelSource.foreach(_ := topdownCancelSrcUpdate)
        robEntries(i).topdownCancelTimeVec.foreach(_.zipWithIndex.foreach{ case (cancelTime, index) =>
          val cancelMatch = topdownCancelSrcUpdate === index.asUInt
          cancelTime := Mux(hasWriteBack, 0.U,
            Mux(topdownCanceled && cancelMatch, robEntries(i).topdownCancelTimeVec.get(index) + robEntries(i).topdownLastIssueTime.get,
              robEntries(i).topdownCancelTimeVec.get(index) +& (cancelMatch & topdownCancelTimeUpdate)))
        })
      }
    }
    out.debugRobHeadStall.foreach{ case stall =>
      val deqEntry = robEntries(deqPtr.value)
      val deqEntryNormalLatency = Mux1H(deqEntry.debug_fuType.get, sortedFulatency) +& bypassLatency +& writeBackLatency
      val issueTime = deqEntry.topdownIssueTime.get
      val lastIssueTime = deqEntry.topdownLastIssueTime.get
      val idealIssueTime = deqEntry.topdownIdealIssueTime.get
      val robHeadTime = deqEntry.topdownRobHeadTime.get

      val topdownIQInfoCandidate = topdownRobCandidate(deqPtr.value)
      val issued = topdownIQInfoCandidate.issued && topdownIQInfoCandidate.valid

      when(deqEntry.valid){
        assert(issueTime >= lastIssueTime)
        assert(issueTime <= idealIssueTime)
      }
      val waitTime = idealIssueTime - robHeadTime
      val idealWaitTime = idealIssueTime - issueTime
      val robHeadFutype = deqEntry.debug_fuType.get
      val robHeadExecStall = deqEntry.valid && (lastIssueTime > deqEntryNormalLatency) && (lastIssueTime > waitTime)
      val robHeadNotIssued = deqEntry.valid && (!deqEntry.topdownIssued.get && !issued)
      val robHeadIssueCancel = deqEntry.valid && deqEntry.topdownCanceled.get && (issueTime > deqEntryNormalLatency)
      // issueDelay priority than cancel; only calculate idealWaitTime stall
      val robHeadIssueDelay = deqEntry.valid && (idealIssueTime > deqEntryNormalLatency) &&
        (robHeadTime < idealWaitTime)
      val robHeadExecStallReason =  MuxCase(OtherNotReadyStall.id.U, Seq(
        FuType.isAMO(robHeadFutype)          -> AtomicStall.id.U          ,
        FuType.isStore(robHeadFutype)  -> StoreStall.id.U           ,
        FuType.isLoad(robHeadFutype)    -> ldReason                  ,
        FuType.isDivSqrt(robHeadFutype)      -> DivStall.id.U             ,
        FuType.isInt(robHeadFutype)          -> IntNotReadyStall.id.U     ,
        FuType.isFArith(robHeadFutype)       -> FPNotReadyStall.id.U      ,
      ))

      val robHeadCancelSource = deqEntry.topdownCancelSource.get

      val cancelTimeVec = deqEntry.topdownCancelTimeVec.get
      val cancelTimeFixVec = deqEntry.topdownCancelTimeFixVec.get
      val fixedCancelTimeVec = cancelTimeVec.zip(cancelTimeFixVec).map{ case (time, fix) =>
        Mux(time < fix, 0.U, time - fix)
      }
      val maxCancelTime = fixedCancelTimeVec.reduce{ (a,b) => Mux(a > b, a, b)}
      val maxCancelTimeVec = VecInit(fixedCancelTimeVec.map(_ === maxCancelTime)).asUInt
      val lastMaxCancelTimeOH = Reverse(PriorityEncoderOH(Reverse(maxCancelTimeVec)))
      val lastMaxIdx = OHToUInt(lastMaxCancelTimeOH)
      val cancelTimeFixUpdateVec = VecInit(cancelTimeFixVec.zip(lastMaxCancelTimeOH.asBools).map{
        case (time, hit) => time +& hit
      })

      if (backendParams.debugEn){
        dontTouch(VecInit(lastMaxCancelTimeOH.asBools))
      }


      val cancelStallReason = MuxCase(IssueCancelStallOther.id.U, Seq(
        IQCancelSource.isog0(lastMaxIdx) -> IssueCancelStallOg0.id.U  ,
        IQCancelSource.isog1(lastMaxIdx) -> IssueCancelStallOg1.id.U  ,
        IQCancelSource.isld(lastMaxIdx)  -> IssueCancelStallLd.id.U   ,
        IQCancelSource.isst(lastMaxIdx)  -> IssueCancelStallSt.id.U   ,
      ))
      deqEntry.topdownCancelTimeFixVec.foreach(_ := cancelTimeFixUpdateVec)


      stall.valid := robHeadExecStall || robHeadNotIssued || robHeadIssueCancel || robHeadIssueDelay
      stall.bits := MuxCase(BackendOtherCoreStall.id.U, Seq(
        robHeadNotIssued                    -> RobHeadNotIssued.id.U     ,
        robHeadExecStall                    -> robHeadExecStallReason    ,
        robHeadIssueDelay                   -> IssueDelayStall.id.U      ,
        robHeadIssueCancel                  -> cancelStallReason         ,
      ))
    }
  }

  //difftest signals
  val firstValidCommit = deqPtr.addEntries(PriorityMux(out.commits.commitValid, VecInit(List.tabulate(CommitWidth)(_.U(log2Up(CommitWidth).W))))).value

  val wdata = Wire(Vec(2 * CommitWidth, UInt(XLEN.W)))
  val wpc = Wire(Vec(2 * CommitWidth, UInt(XLEN.W)))

  for (i <- 0 until CommitWidth; j <- 0 until 2) {
    val idx = deqPtrVec(i).value
    wdata(2 * i + j) := debug_exuData(idx)(j)
    wpc(2 * i + j) := SignExt(debugMeta(commitDebugUop(2 * i + j)).pc, XLEN)
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    // Keep the mainline debug payload, independently sampled at each slot head.
    val fullBasicDiff = env.EnableDifftest || env.FullBasicDiff
    val dt_basicDebug = Reg(Vec(RobSize, Vec(2, new BasicDebugInfo)))
    val dt_pcTransType = Option.when(fullBasicDiff)(Reg(Vec(RobSize, Vec(2, new AddrTransType))))
    val dt_exuDebug = Reg(Vec(RobSize, Vec(2, new DebugBundle)))

    for (i <- 0 until RenameWidth) {
      val hasEarlierSameHalf = (0 until i).map { j =>
        in.enq.canAccept && in.enq.req(j).valid &&
          in.enq.req(j).bits.robIdx.isSameSlot(in.enq.req(i).bits.robIdx)
      }.foldLeft(false.B)(_ || _)
      val hasEarlierEntryReset = (0 until i).map { j =>
        in.enq.canAccept && in.enq.req(j).valid && in.enq.req(j).bits.firstUop &&
          in.enq.req(j).bits.robIdx.isSameEntry(in.enq.req(i).bits.robIdx)
      }.foldLeft(false.B)(_ || _)
      when(in.enq.canAccept && in.enq.req(i).valid) {
        val uop = in.enq.req(i).bits
        val ptr = uop.robIdx.value
        val slot = slotIndex(uop.robIdx)
        val slotWritten = Mux(uop.firstUop || hasEarlierEntryReset, false.B, enqHalfWritten(ptr)(slot))
        when(!hasEarlierSameHalf && !slotWritten) {
          dt_basicDebug(ptr)(slot).connectBasicDebug(uop)
          dt_pcTransType.foreach(_(ptr)(slot) := in.debugInstrAddrTransType)
        }
      }
    }
    for (wb <- exuWBs) {
      when(wb.valid) {
        dt_exuDebug(wb.bits.robIdx.value)(slotIndex(wb.bits.robIdx)) := wb.bits.debug
      }
    }

    val event = in.diffArchEvent.get
    when(event.valid) {
      when(event.bits) {
        assert(out.diffCommitForTrap.get && out.commits.isCommit && out.commits.commitValid.head)
      }.otherwise {
        assert(!hasCommit, "An event at the former slot must not overlap ordinary retirement")
      }
    }
    for (i <- 0 until CommitWidth; j <- 0 until 2) {
      val index = 2 * i + j
      val ptr = deqPtrVec(i).value
      val uop = debug_microOp(ptr)(j)
      val uopDebug = debugMeta(uop)
      val exuOut = dt_exuDebug(ptr)(j)
      val basicDebug = dt_basicDebug(ptr)(j)
      val ordinaryValid = out.commits.isCommit && commitSlotValid(index)
      val slotInstrCount = if (j == 0) {
        Mux(CompressType.isNORMAL(out.commits.info(i).entryPairType), instrSizeCommit(i), formerInstrCntCommit(i))
      } else {
        latterInstrCntCommit(i)
      }
      val skip = !basicDebug.eliminatedMove && exuOut.isSkipDiff
      val isVLoad = uop.isVectorMemory && FuType.isLoad(uop.fuType)
      val diffMaxPhyRegs = Seq(MaxPhyRegs, 2 * (V0PhyRegs + VfPhyRegs)).max
      val difftest = DifftestModule(new DiffInstrCommit(diffMaxPhyRegs), delay = 3, dontCare = true)
      difftest.coreid := in.hartId
      difftest.index := index.U
      difftest.valid := ordinaryValid
      difftest.skip := ordinaryValid && skip
      difftest.isRVC := ordinaryValid && uop.isRVC
      difftest.rfwen := ordinaryValid && uop.rfWen && basicDebug.ldest =/= 0.U
      difftest.fpwen := ordinaryValid && basicDebug.fpWen
      difftest.vecwen := ordinaryValid && basicDebug.vecWen
      difftest.v0wen := ordinaryValid && (basicDebug.v0Wen || isVLoad && basicDebug.vd === 0.U)
      difftest.wpdest := basicDebug.pdest
      difftest.wdest := Mux(isVLoad, basicDebug.vd, basicDebug.ldest)
      difftest.nFused := Mux(ordinaryValid, slotInstrCount - 1.U, 0.U)
      when(ordinaryValid) {
        assert(slotInstrCount >= 1.U)
      }
      if (fullBasicDiff) {
        // Vector instructions are never compressed; retain mainline vector mapping.
        difftest.otherwpdest := debug_VecOtherPdest(ptr).zipWithIndex.flatMap { case (pdest, idx) =>
          val vecDest = if (idx == 0) Mux(difftest.v0wen, pdest, pdest + V0PhyRegs.U)
            else pdest + V0PhyRegs.U
          val splitDest = (vecDest << 1).asUInt
          Seq(splitDest, splitDest + 1.U)
        }
        val pcTransType = dt_pcTransType.get(ptr)(j)
        difftest.pc := pcTransType.extend(uopDebug.pc, XLEN)
        difftest.instr := uopDebug.instr
        difftest.robIdx := ZeroExt(ptr, 10)
        difftest.lqIdx := ZeroExt(debug_lqIdx(ptr)(j).value, 7)
        difftest.sqIdx := ZeroExt(debug_sqIdx(ptr)(j).value, 7)
        difftest.isLoad := ordinaryValid && uop.commitType === CommitType.LOAD
        difftest.isStore := ordinaryValid && uop.commitType === CommitType.STORE
      }
      if (env.EnableDifftest) {
        val difftestLoadEvent = DifftestModule(new DiffLoadEvent, delay = 3)
        val loadCheck = (FuType.isAMO(uop.fuType) || FuType.isLoad(uop.fuType) || isVLoad) && !skip
        difftestLoadEvent.coreid := in.hartId
        difftestLoadEvent.index := index.U
        difftestLoadEvent.valid := ordinaryValid && loadCheck
        difftestLoadEvent.paddr := exuOut.paddr
        difftestLoadEvent.opType := LSUOpType.formLoadEventOpcode(FuType.isAMO(uop.fuType), uop.fuOpType)
        difftestLoadEvent.isAtomic := FuType.isAMO(uop.fuType)
        difftestLoadEvent.isLoad := FuType.isLoad(uop.fuType)
        difftestLoadEvent.isVLoad := isVLoad
      }
    }
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val trapVec = commitSlotValid.zip(commitDebugUop).map { case (valid, uop) =>
      out.commits.isCommit && valid && uop.isXSTrap
    }
    val hitTrap = trapVec.reduce(_ || _)
    val difftest = DifftestModule(new DiffTrapEvent, dontCare = true)
    difftest.coreid := in.hartId
    difftest.hasTrap := hitTrap
    difftest.cycleCnt := timer
    difftest.instrCnt := instrCnt
    difftest.hasWFI := hasWFI

    if (env.EnableDifftest || env.FullBasicDiff) {
      val trapCode = PriorityMux(wdata.zip(trapVec).map(x => x._2 -> x._1))
      val trapPC = SignExt(PriorityMux(wpc.zip(trapVec).map(x => x._2 -> x._1)), XLEN)
      difftest.code := trapCode
      difftest.pc := trapPC
    }
  }

  //store evetn difftest information
  in.storeDebugInfo := DontCare
  if (env.EnableDifftest || env.FullBasicDiff) {
    in.storeDebugInfo.map{port =>
      port.pc := debugMeta(debug_microOp(port.robidx.value)(slotIndex(port.robidx))).pc
      port.storeOffset.foreach(_ := decodeTraceInstrCount(robEntries(port.robidx.value).traceBlockInPipe.iretire) - 1.U)
      // Conservatively include stores from the current head entry, even when it retires this cycle.
      port.preCommit.foreach(_ := port.robidx.isSameEntry(deqPtr))
    }
  }

  val misPred = in.redirect.valid && in.redirect.bits.isMisPred
  val brhJump = PopCount(isBrhOrJmpWBs.map(wb => wb.valid))

  XSPerfAccumulate("branch_jump", brhJump)
  XSPerfAccumulate("br_mis_pred", misPred)
  XSPerfAccumulate("total_flush", in.redirect.valid)

  val commitLoadVec = VecInit(commitLoadValid)
  val commitBranchVec = VecInit(commitBranchValid)
  val commitStoreVec = VecInit(commitStoreValid)
  val perfEvents = Seq(
    ("rob_interrupt_num      ", out.flushOut.valid && intrEnable),
    ("rob_exception_num      ", out.flushOut.valid && deqHasException),
    ("rob_flush_pipe_num     ", out.flushOut.valid && isFlushPipe),
    ("rob_replay_inst_num    ", out.flushOut.valid && isFlushPipe && deqHasReplayInst),
    ("rob_commitUop          ", ifCommit(commitCnt)),
    ("rob_commitInstr        ", ifCommitReg(trueCommitCnt)),
    ("rob_commitInstrFused   ", ifCommitReg(fuseCommitCnt)),
    ("rob_commitInstrLoad    ", ifCommitReg(PopCount(RegEnable(commitLoadVec, isCommit)))),
    ("rob_commitInstrBranch  ", ifCommitReg(PopCount(RegEnable(commitBranchVec, isCommit)))),
    ("rob_commitInstrStore   ", ifCommitReg(PopCount(RegEnable(commitStoreVec, isCommit)))),
    ("rob_walkInstr          ", Mux(out.commits.isWalk, PopCount(out.commits.walkValid), 0.U)),
    ("rob_walkCycle          ", (state === s_walk)),
    ("rob_1_4_valid          ", numValidEntries <= (RobSize / 4).U),
    ("rob_2_4_valid          ", numValidEntries > (RobSize / 4).U && numValidEntries <= (RobSize / 2).U),
    ("rob_3_4_valid          ", numValidEntries > (RobSize / 2).U && numValidEntries <= (RobSize * 3 / 4).U),
    ("rob_4_4_valid          ", numValidEntries > (RobSize * 3 / 4).U),
    ("BRANCH_JUMP            ", brhJump),
    ("BR_MIS_PRED            ", misPred),
    ("TOTAL_FLUSH            ", in.redirect.valid)
  )
  require(perfEvents.length == out.perfEvents.length)
  generatePerfEvent()

  // max commit-stuck cycle
  val mmioBusy = out.lsq.mmioBusy // lsq know uncache request is rob head
  val commitStuck = (!out.commits.commitValid.reduce(_ || _) || !out.commits.isCommit) && !mmioBusy && (wfiResume.B || !hasWFI)
  val commitStuckCounter = Module(new CommitStuckCounter(
    width = log2Up(maxCommitStuck),
    forceEnable = (env.EnableDifftest || env.FullBasicDiff).B
  ))
  val commitStuckOverflowEnabled = if (wfiResume) true.B else !hasWFI
  commitStuckCounter.in.stuck := commitStuck
  commitStuckCounter.in.runtimeEnable := in.commit_stuck_check_enable
  commitStuckCounter.in.overflowEnabled := commitStuckOverflowEnabled
  val commitStuckCycle = commitStuckCounter.out.count
  // check if stuck > 2^maxCommitStuckCycle
  val commitStuck_overflow = commitStuckCounter.out.overflow
  val criticalErrors = Seq(
    ("rob_commit_stuck  ", commitStuck_overflow),
  )
  require(criticalErrors.length == out.criticalErrors.length)
  generateCriticalErrors()


  // dontTouch for debug
  if (backendParams.debugEn) {
    // Unpack the compressed ROB writeback state only for debug builds.
    val debugFormerUopNum = Wire(Vec(RobSize, UInt(NormalUopNumWidth.W)))
    val debugLatterUopNum = Wire(Vec(RobSize, UInt(NormalUopNumWidth.W)))
    for (i <- 0 until RobSize) {
      debugFormerUopNum(i) := robEntries(i).formerUopNum
      debugLatterUopNum(i) := robEntries(i).latterUopNum
    }
    dontTouch(debugFormerUopNum)
    dontTouch(debugLatterUopNum)
    dontTouch(enqPtrVec)
    dontTouch(deqPtrVec)
    dontTouch(robEntries)
    dontTouch(robDeqGroup)
    dontTouch(robBanks)
    dontTouch(robBanksRaddrThisLine)
    dontTouch(robBanksRaddrNextLine)
    dontTouch(robBanksRdataThisLine)
    dontTouch(robBanksRdataNextLine)
    dontTouch(robBanksRdataThisLineUpdate)
    dontTouch(robBanksRdataNextLineUpdate)
    dontTouch(needUpdate)
    val exceptionWBsVec = MixedVecInit(exceptionWBs)
    dontTouch(exceptionWBsVec)
    dontTouch(commit_wDeqGroup)
    dontTouch(commit_vDeqGroup)
    dontTouch(commitSizeSumSeq)
    dontTouch(walkSizeSumSeq)
    dontTouch(commitSizeSumCond)
    dontTouch(walkSizeSumCond)
    dontTouch(commitSizeSum)
    dontTouch(walkSizeSum)
    dontTouch(realDestSizeSeq)
    dontTouch(walkDestSizeSeq)
    dontTouch(out.commits)
    dontTouch(commitIsVTypeVec)
    dontTouch(walkIsVTypeVec)
    dontTouch(commitValidThisLine)
    dontTouch(commitReadAddr_next)
    dontTouch(donotNeedWalk)
    dontTouch(walkPtrVec_next)
    dontTouch(walkPtrVec)
    dontTouch(deqPtrVec_next)
    dontTouch(deqPtrVecForWalk)
    dontTouch(snapPtrReadBank)
    dontTouch(snapPtrVecForWalk)
    dontTouch(shouldWalkVec)
    dontTouch(walkFinished)
    dontTouch(changeBankAddrToDeqPtr)
  }
  if (env.EnableDifftest || env.FullBasicDiff) {
    out.commits.info.map(info => dontTouch(info.debug_pc.get))
  }
}

object Rob {
  private val PerfEventCount = 19
  private val CriticalErrorCount = 1

  class In(params: BackendParams)(implicit p: Parameters) extends XSBundle {
    // Core identity.
    val hartId = Input(UInt(hartIdLen.W))

    // Enqueue requests and admission responses.
    val enq = new RobEnqIO
    val teaMembers = Option.when(backendParams.teaTrackerEn)(Input(Vec(RenameWidth, Valid(new TeaRenameMember))))

    // Execution results without redirect filtering.
    val exuWriteback: MixedVec[ValidIO[WriteBackRobBundle]] = Flipped(params.genWrite2RobBundles)

    // ROB writeback reports with redirect filtering.
    val writeback: MixedVec[ValidIO[WriteBackRobBundle]] = Flipped(params.genWrite2RobBundles)

    // Writeback counts.
    val writebackNums = Flipped(Vec(writeback.size, ValidIO(UInt(writeback.size.U.getWidth.W))))

    // Writeback exception summaries.
    val writebackNeedFlush = Input(Vec(params.getWrite2RobSize(_.needExceptionGen), Bool()))

    // Accepted redirect.
    val redirect = Input(Valid(new Redirect))

    // Recovery snapshots.
    val snpt = Input(new SnapshotPort)

    // CSR control and trap state.
    val csr = new RobCSRInIO

    // Commit watchdog control.
    val commit_stuck_check_enable = Input(Bool())

    // Vector restart state.
    val vstartIsZero = Input(Bool())

    // Vector exception backpressure.
    val fromVecExcpMod = Input(new RobFromVecExcpModIO)

    // WFI control.
    val wfi_enable = Input(Bool())

    // Difftest architectural events.
    val diffArchEvent = Option.when(env.EnableDifftest || env.AlwaysBasicDiff)(Input(Valid(Bool())))

    // Store debug queries and responses.
    val storeDebugInfo = Vec(EnsbufferWidth, new RobStoreDebugIO)

    // Load/store debug events.
    val debug_ls = Flipped(new DebugLSIO)

    // LSQ enqueue observation.
    val debugEnqLsq = Input(new LsqEnqIO)

    // Head memory issue observation.
    val debugHeadLsIssue = Input(Bool())

    // Instruction address translation mode.
    val debugInstrAddrTransType = Input(new AddrTransType)

    // Core TopDown observations.
    val debugTopDown = new RobTopDownInIO

    // Issue queue TopDown samples.
    val topdownIQInfoVec = Option.when(backendParams.debugEn)(Input(Vec(backendParams.iqEntryNum, Flipped(ValidIO(new TopdownIQExtendedInfo())))))

    // Load pipeline TopDown samples.
    val lsTopdownInfo = Vec(params.LduCnt + params.HyuCnt, Input(new LsTopdownInfo))
  }

  class Out(params: BackendParams)(implicit p: Parameters) extends XSBundle {
    // ROB occupancy.
    val robFull = Output(Bool())
    val dip = Option.when(backendParams.dipEn)(Output(new DipRob))

    // ROB commit notifications.
    val commits = Output(new RobCommitIO)

    // Register mapping notifications.
    val rabCommits = Output(new RabCommitIO)

    // VL mapping notifications.
    val vlCommits = Output(new VlCommitBundle(RabCommitWidth))

    // ROB dequeue pointer status.
    val robDeqPtr = Output(new RobPtr)

    // Flush requests.
    val flushOut = ValidIO(new Redirect)
    val flushPcInfo = Output(new RobFlushPcInfo)

    // Precise exception reports.
    val exception = ValidIO(new ExceptionInfo)

    // Guest physical address requests and readback.
    val readGPAMemAddr = ValidIO(new RobGPAMemReadAddr)
    val readGPAMemData = Input(new GPAMemEntry)

    // CSR retirement side effects.
    val csr = new RobCSROutIO

    // Vector exception recovery.
    val toVecExcpMod = Output(new RobToVecExcpModIO)

    // Vset flush status.
    val isVsetFlushPipe = Output(Bool())

    // Decode vector configuration.
    val toDecode = new RobToDecodeIO

    // Memory retirement and ordering feedback.
    val lsq = new RobLsqIO

    // WFI status.
    val cpu_wfi = Output(Bool())

    // WFI requests and quiescence acknowledgments.
    val wfi = new RobWfiIO

    // Instruction trace and backpressure.
    val trace = new RobTraceIO

    // Difftest trap notifications.
    val diffCommitForTrap = Option.when(env.EnableDifftest || env.AlwaysBasicDiff)(Output(Bool()))

    // Difftest RAT selection.
    val diffRatCommitRobIdx = if (backendParams.basicDebugEn) Some(Output(Valid(new RobPtr))) else None
    val diffRatCommitRobIdxVec =
      if (backendParams.basicDebugEn) Some(Output(Vec(CommitWidth, Valid(new RobPtr)))) else None

    // Serialization debug state.
    val debugBlockBackward = Option.when(backendParams.debugEn)(Output(Bool()))
    val debugWaitForward   = Option.when(backendParams.debugEn)(Output(Bool()))

    // Core TopDown reports.
    val debugTopDown = new RobTopDownOutIO

    // ROB head stall reason.
    val debugRobHeadStall = Option.when(backendParams.debugEn)(ValidIO(UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W)))

    // Rolling retirement statistics.
    val debugRolling = new RobDebugRollingIO

    // Hardware performance events.
    val perfEvents = Output(Vec(PerfEventCount, new PerfEvent))

    // Critical error notifications.
    val criticalErrors = Output(Vec(CriticalErrorCount, Bool()))
  }

  class RobTraceIO(implicit p: Parameters) extends XSBundle {
    // Trace backpressure.
    val blockCommit = Input(Bool())

    // Trace output.
    val traceCommitInfo = new TraceBundle(hasIaddr = false, CommitWidth, IretireWidthCommited)
  }

  class RobWfiIO extends Bundle {
    // WFI request.
    val wfiReq = Output(Bool())

    // Quiescence acknowledgments.
    val safeFromMem = Input(Bool())
    val safeFromFrontend = Input(Bool())
  }

  class RobCommitVTypeIO(implicit p: Parameters) extends XSBundle {
    // Committed VTYPE.
    val vtype = ValidIO(VType())

    // Committed vsetvl indication.
    val hasVsetvl = Output(Bool())
  }

  class RobToDecodeIO(implicit p: Parameters) extends XSBundle {
    // VTYPE recovery.
    val isResumeVType = Output(Bool())
    val walkToArchVType = Output(Bool())
    val walkVType = ValidIO(VType())

    // Committed VTYPE.
    val commitVType = new RobCommitVTypeIO
  }

  class RobFromVecExcpModIO extends Bundle {
    // Vector exception backpressure.
    val busy = Bool()
  }

  class RobGPAMemReadAddr(implicit p: Parameters) extends XSBundle {
    // Faulting instruction location.
    val ftqPtr = new FtqPtr()
    val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
    val isRVC = Bool()
  }

  class RobToVecExcpModIO(implicit p: Parameters) extends XSBundle {
    // Recovery register mappings.
    val logicPhyRegMap = Vec(RabCommitWidth, ValidIO(new RegWriteFromRab))

    // Vector exception metadata.
    val excpInfo = ValidIO(new VecExcpInfo)
  }

  class RobTopDownInIO(implicit p: Parameters) extends XSBundle {
    // Memory stall feedback.
    val fromCore = new CoreDispatchTopDownIO
  }

  class RobTopDownOutIO(implicit p: Parameters) extends XSBundle {
    // Core address observations.
    val toCore = new RobCoreTopDownIO

    // Dispatch retirement observations.
    val toDispatch = new RobDispatchTopDownIO

    // Head load queue identity.
    val robHeadLqIdx = Valid(new LqPtr)
  }

  class RobStoreDebugIO(implicit p: Parameters) extends XSBundle {
    // Store PC query.
    val robidx = Input(new RobPtr)
    val pc     = Output(UInt(VAddrBits.W))

    // Difftest store ordering.
    val storeOffset = Option.when(env.EnableDifftest)(Output(UInt(log2Ceil(RenameWidth + 1).W)))

    // Queried entry at ROB head.
    val preCommit = Option.when(env.EnableDifftest)(Output(Bool()))
  }

  class RobCSRPerfInfo extends Bundle {
    // Retired instruction count.
    val retiredInstr = Output(UInt(7.W))
  }

  class RobCSRInIO(implicit p: Parameters) extends XSBundle {
    // Pending interrupt.
    val intrBitSet = Input(Bool())

    // Trap target.
    val trapTarget = Input(new TargetPCBundle)

    // WFI wakeup.
    val wfiEvent   = Input(Bool())

    // Critical error status.
    val criticalErrorState = Input(Bool())
  }

  class RobCSROutIO(implicit p: Parameters) extends XSBundle {
    // Floating-point exception flags.
    val fflags     = Output(Vec(5, Bool()))

    // Vector saturation flag.
    val vxsat      = Output(Bool())

    // Vector restart index.
    val vstart     = Output(Valid(UInt(XLEN.W)))

    // Floating-point state dirtiness.
    val dirty_fs   = Output(Bool())

    // Vector state dirtiness.
    val dirty_vs   = Output(Bool())

    // Retired instruction count.
    val perfinfo   = new RobCSRPerfInfo
  }

  class RobCSRIO(implicit p: Parameters) extends XSBundle {
    // Pending interrupt.
    val intrBitSet = Input(Bool())

    // Trap target.
    val trapTarget = Input(new TargetPCBundle)

    // WFI wakeup.
    val wfiEvent   = Input(Bool())

    // Critical error status.
    val criticalErrorState = Input(Bool())

    // Floating-point exception flags.
    val fflags     = Output(Vec(5, Bool()))

    // Vector saturation flag.
    val vxsat      = Output(Bool())

    // Vector restart index.
    val vstart     = Output(Valid(UInt(XLEN.W)))

    // Floating-point state dirtiness.
    val dirty_fs   = Output(Bool())

    // Vector state dirtiness.
    val dirty_vs   = Output(Bool())

    // Retired instruction count.
    val perfinfo   = new RobCSRPerfInfo
  }

  class RobLsqIO(implicit p: Parameters) extends XSBundle {
    // Load retirement count (up to two slots per entry).
    val lcommit = Output(UInt(log2Up(2 * CommitWidth + 1).W))

    // Store release count.
    val scommit = Output(UInt(log2Up(CommitWidth + 1).W))

    // Head entry retirement.
    val commit = Output(Bool())

    // Ordered replay boundary.
    val pendingPtr = Output(new RobPtr)
    val pendingPtrNext = Output(new RobPtr)

    // MMIO serialization.
    val mmioBusy = Input(Bool())
  }

  class RobEnqIO(implicit p: Parameters) extends XSBundle {
    // Enqueue permissions.
    val canAccept = Output(Bool())
    val canAcceptForDispatch = Output(Bool())

    // ROB occupancy.
    val isEmpty = Output(Bool())

    // Entry allocation intent.
    val needAlloc = Vec(RenameWidth, Input(Bool()))

    // Enqueue uops.
    val req = Vec(RenameWidth, Flipped(ValidIO(new EnqRobUop)))

    // Assigned ROB pointers.
    val resp = Vec(RenameWidth, Output(new RobPtr))
  }

  class RobCoreTopDownIO(implicit p: Parameters) extends XSBundle {
    // Head memory addresses.
    val robHeadVaddr = Valid(UInt(VAddrBits.W))
    val robHeadPaddr = Valid(UInt(PAddrBits.W))
  }

  class RobDispatchTopDownIO extends Bundle {
    // Retired instruction count.
    val robTrueCommit = Output(UInt(64.W))

    // Head memory issue status.
    val robHeadLsIssue = Output(Bool())
  }

  class RobDebugRollingIO extends Bundle {
    // Retired instruction count.
    val robTrueCommit = Output(UInt(64.W))
  }

  class TipEntry()(implicit p: Parameters) extends XSBundle {
    val state = UInt(4.W)
    val commits = new RobCommitIO()      // info of commit
    val redirect = Valid(new Redirect)   // info of redirect
    val redirect_pc = UInt(VAddrBits.W)  // PC of the redirect uop
    val debugLsInfo = new DebugLsInfo()
  }
}
