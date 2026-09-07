package xiangshan.backend.float

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility.{PerfCCT, SelectOne}
import utils.NamedUInt
import xiangshan._
import xiangshan.backend.Bundles.{DispatchOutUop, IssueQueueInDebug, RegionInUop, UopIdx}
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.decode.opcode.{Latency, Opcode}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.issue.{AgeDetector, NewAgeDetector}
import xiangshan.backend.regfile.PregParams
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.float.FltIssueQueue._
import xiangshan.backend.vector.IssueParam
import xiangshan.backend.vector.ExuParam
import xiangshan.backend.vector.VecRegionModule
import xiangshan.backend.vector.VecIssueQueue.{Entry, Enq, Deq, Status, SrcStatus, BypassDelay, RespBundle, WakeUpBundle}
import xiangshan.mem.SqPtr


class FltIssueQueue(
  override val wrapper: FltIssueQueue.LazyMod
)(implicit p: Parameters, val param: IssueParam) extends LazyModuleImp(wrapper) with HasXSParameter {
  override def desiredName: String = param.getDefinitionNameOfIQ

  require(param.numExu <= 2, "IssueQueue has not supported more than 2 deq ports")
  require(param.numEnq <= 2, "IssueQueue has not supported more than 2 enq ports")
  require(param.numSlowEntry == 0 || param.numSlowEntry >= param.numEnq, "numSlowEntry should be 0 or at least not less than numEnq")
  require(param.numFastEntry == 0 || param.numFastEntry >= param.numEnq, "numFastEntry should be 0 or at least not less than numEnq")

  val in = IO(Input(new In))
  val out = IO(Output(new Out))

  in.enq.foreach { case enq =>
    enq.bits.debug.foreach(x => PerfCCT.updateInstPos(x.debug_seqNum, PerfCCT.InstPos.AtIssueQue.id.U, enq.valid, clock, reset))
  }

  // Modules

  // Regs
  private val enqEntries = RegInit(VecInit.fill(param.numEnq)(ValidIO(new Entry).Lit(_.valid -> false.B)))
  private val fastEntries = RegInit(VecInit.fill(param.numFastEntry)(ValidIO(new Entry).Lit(_.valid -> false.B)))
  private val entries = enqEntries ++ fastEntries

  // A new uop replace the old one in the entry
  private val enqEntriesEnqNext: Vec[ValidIO[Entry]] = Wire(chiselTypeOf(enqEntries))
  private val fastEntriesEnqNext: Vec[ValidIO[Entry]] = Wire(chiselTypeOf(fastEntries))

  // The uop is not replaced by another one
  private val enqEntriesKeepNext = Wire(chiselTypeOf(enqEntries))
  private val fastEntriesKeepNext = Wire(chiselTypeOf(fastEntries))

  dontTouch(enqEntriesKeepNext)
  dontTouch(fastEntriesKeepNext)
  /**
   * In this issue queue, there are two kind of entries named [[enqEntries]] and [[fastEntries]].
   * And there are two next bundles for each of [[entries]]
   *
   * The data flow graph is as follows.
   *
   *                                      [[in.enq]] --> [[enqEntriesEnqNext]]
   *                                                                |
   *                                                                | enqEntriesEnqNext.valid
   *                                                                v
   *                                                          [[enqEntries]]*
   *                                                           ^          |
   *      enqEntriesKeepNext.valid && !fastEntryEmptySel.valid |          | !issueSuccess && !flushed
   *                                                           |          v
   *                                                      [[enqEntriesKeepNext]]
   *                                                                |       numEnqEntry -> numFastEntry
   *                                                                | enqEntriesKeepNext.valid && fastEntryEmptySel.valid
   *                                                                v
   *                                                      [[fastEntriesEnqNext]]
   *                                                                |
   *                                                                | fastEntriesEnqNext.valid
   *                                                                v
   *                                                         [[fastEntries]]*
   *                                                           ^         |
   *                                 fastEntriesKeepNext.valid |         | !issueSuccess && !flushed
   *                                                           |         v
   *                                                       [[fastEntriesKeepNext]]
   */

  // Wires
  private val enqEntryValid: UInt = VecInit(enqEntries.map(_.valid)).asUInt
  private val fastEntryValid: UInt = VecInit(fastEntries.map(_.valid)).asUInt

  private val enqEntryEnqNotFlush = WireInit(VecInit(in.enq.map(enq => enq.valid && !in.flush.valid)))
  private val fastEntryEnqNotFlush = Wire(Vec(param.numFastEntry, Bool()))

  private val enqEntryEnq = Wire(Vec(param.numEnq, ValidIO(new Entry)))
  private val fastEntryEnq = Wire(Vec(param.numFastEntry, ValidIO(new Entry)))

  private val enqEntryFlush   = WireInit(VecInit( enqEntries.map(ety => ety.bits.status.robIdx.needFlush(in.flush))))
  private val fastEntryFlush  = WireInit(VecInit(fastEntries.map(ety => ety.bits.status.robIdx.needFlush(in.flush))))

  private val wakeup = in.wakeup
  private val fpWbD1WakeUp = in.wakeup.fpWbM2D1Vec


  private val enqEntryFpWbM2WakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.fpWbM2Vec.size, Bool()))))
  private val enqEntryFpWbM2D1WakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.fpWbM2Vec.size, Bool()))))
  private val fastEntryEnqFpWbM2WakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.fpWbM2Vec.size, Bool()))))
  private val fastEntryFpWbM2WakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.fpWbM2Vec.size, Bool()))))
  dontTouch(enqEntryFpWbM2WakeUpMatchVec)
  dontTouch(enqEntryFpWbM2D1WakeUpMatchVec)
  dontTouch(fastEntryEnqFpWbM2WakeUpMatchVec)
  dontTouch(fastEntryFpWbM2WakeUpMatchVec)
  private val enqEntrySrcCancel = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Bool())))
  private val fastEntrySrcCancel = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Bool())))
  private val entrySrcCancel = enqEntrySrcCancel ++ fastEntrySrcCancel
  private val enqEntryCanIssue = VecInit(enqEntries.zipWithIndex.map {
    case (ety, enqIdx) =>
      entryCanIssueWithWakeUp(
        entryIdx = enqIdx,
        status = ety.bits.status,
        entryValid = ety.valid,
        fpWbM2WakeUpMatchVec = enqEntryFpWbM2WakeUpMatchVec(enqIdx),
      )
  })
  private val enqEntryCanIssueVec: Vec[UInt] = VecInit(
    (0 until param.numDeq).map(deqIdx => VecInit(enqEntries.zip(enqEntryCanIssue).map {
      case (ety, canIssue) => FltIssueQueue.entryCanIssueOnDeq(in.fromWbFuBusyTable, param, ety.valid, ety.bits, canIssue, deqIdx)
    }).asUInt)
  )
  private val fastEntryCanIssue = VecInit(fastEntries.zipWithIndex.map {
    case (ety, fastIdx) =>
      entryCanIssueWithWakeUp(
        entryIdx = param.numEnq + fastIdx,
        status = ety.bits.status,
        entryValid = ety.valid,
        fpWbM2WakeUpMatchVec = fastEntryFpWbM2WakeUpMatchVec(fastIdx),
      )
  })
  private val fastEntryCanIssueVec: Vec[UInt] = VecInit(
    (0 until param.numDeq).map(deqIdx => VecInit(fastEntries.zip(fastEntryCanIssue).map {
      case (ety, canIssue) => FltIssueQueue.entryCanIssueOnDeq(in.fromWbFuBusyTable, param, ety.valid, ety.bits, canIssue, deqIdx)
    }).asUInt)
  )
  dontTouch(enqEntryCanIssueVec)
  dontTouch(fastEntryCanIssueVec)

  private val enqEntryDeqSel = Wire(Vec(param.numEnq, Bool()))
  private val fastEntryDeqSel = Wire(Vec(param.numFastEntry, Bool()))
  private val entryDeqSel: Seq[Bool] = enqEntryDeqSel ++ fastEntryDeqSel

  private val enqEntryCancel = Wire(Vec(param.numEnq, Bool()))
  private val fastEntryCancel = Wire(Vec(param.numFastEntry, Bool()))
  private val entryCancel: Seq[Bool] = enqEntryCancel ++ fastEntryCancel
  private val ldWBPort = param.backendParams.getIntRegionParam.issueParams.filter(_.hasLdu).map(_.fpWbPortIds.head)
  private val fltExuWBPort = param.backendParams.getFltRegionParam.issueParams.map(_.fpWbPortIds.head)
  private val ldCancel = in.ldCancel.map(_.ld2Cancel)
  private val entriesKeepNext = enqEntriesKeepNext ++ fastEntriesKeepNext
  for ((cancel, entry, srcCancel) <- entryCancel.lazyZip(entries).lazyZip(entrySrcCancel)) {
    cancel := Mux1H(Seq(
      in.resps.is0(entry.bits.status.deqPortIdx).fail -> (entry.bits.status.issued && entry.bits.status.issuedTimer === 0.U),
      in.resps.is1(entry.bits.status.deqPortIdx).fail -> (entry.bits.status.issued && entry.bits.status.issuedTimer === 1.U),
      in.resps.ex0(entry.bits.status.deqPortIdx).fail -> (entry.bits.status.issued && entry.bits.status.issuedTimer === 2.U),
      ))
    srcCancel.zipWithIndex.map { case (srccancel, srcidx) =>
      val issuedSrcCancel =
        in.resps.is0(entry.bits.status.deqPortIdx).srcCancel(srcidx) && entry.bits.status.issued && entry.bits.status.issuedTimer === 0.U ||
        in.resps.is1(entry.bits.status.deqPortIdx).srcCancel(srcidx) && entry.bits.status.issued && entry.bits.status.issuedTimer === 1.U ||
        in.resps.ex0(entry.bits.status.deqPortIdx).srcCancel(srcidx) && entry.bits.status.issued && entry.bits.status.issuedTimer === 2.U
      val srcStatus = entry.bits.status.srcStatus(srcidx)
      val notIssuedSrcCancelByLoad = !entry.bits.status.issued && ldWBPort.zip(ldCancel).map{ case (idx, ldcancel) =>
        srcStatus.bypassSource.idx === idx.U && srcStatus.bypassDelay === BypassDelay.delay2 && ldcancel
      }.reduce(_ || _)
      val notIssuedSrcCancelByFltExu = !entry.bits.status.issued && fltExuWBPort.zip(in.resps.ex0RespFailLat1).map { case (idx, fltExuCancel) =>
        srcStatus.bypassSource.idx === idx.U && srcStatus.bypassDelay === BypassDelay.delay0 && fltExuCancel
      }.reduce(_ || _)
      srccancel := issuedSrcCancel || notIssuedSrcCancelByLoad || notIssuedSrcCancelByFltExu
      dontTouch(issuedSrcCancel)
      dontTouch(notIssuedSrcCancelByLoad)
      dontTouch(notIssuedSrcCancelByFltExu)
    }
  }

  private val enqEntrySuccess = Wire(Vec(param.numEnq, Bool()))
  private val fastEntrySuccess = Wire(Vec(param.numFastEntry, Bool()))
  private val entrySuccess: Seq[Bool] = enqEntrySuccess ++ fastEntrySuccess

  for ((success, entry) <- entrySuccess.zip(entries)) {
    success := Mux1H(Seq(
      in.resps.is0(entry.bits.status.deqPortIdx).success -> (entry.bits.status.issued && entry.bits.status.issuedTimer === 0.U),
      in.resps.is1(entry.bits.status.deqPortIdx).success -> (entry.bits.status.issued && entry.bits.status.issuedTimer === 1.U),
      in.resps.ex0(entry.bits.status.deqPortIdx).success -> (entry.bits.status.issued && entry.bits.status.issuedTimer === 2.U),
    ))
  }

  private val fastEntryEmptySel: Vec[ValidIO[UInt]] = EnqPolicy((~fastEntryValid).asUInt, param.numEnq)
  private val fastEntryEmptyValid = VecInit(fastEntryEmptySel.map(_.valid))
  private val enqEntryCanTransfer = VecInit(
    enqEntries.zipWithIndex.map {
      case (ety, enqIdx) =>
        ety.valid && !enqEntryFlush(enqIdx) && !enqEntrySuccess(enqIdx)
    }
  )
  private val fastEntryEnqNotFlushOH: Vec[UInt] = VecInit(
    fastEntryEmptySel zip enqEntryCanTransfer map {
      case (empty, canTransfer) =>
        Mux(
          empty.valid && canTransfer,
          empty.bits,
          0.U,
        )
    }
  )

  for ((enq, etyIdx) <- enqEntryEnq.zipWithIndex) {
    val inEnq = in.enq(etyIdx)
    val inEnqBits = inEnq.bits
    enq.valid := inEnq.valid
    enq.bits.payload.fromEnq(inEnqBits)
    enq.bits.status.fromEnq(inEnqBits)
  }

  // NewAgeDetector can be used here, because the 2nd port has uop only if the 1st port has uop.
  private val enqEntryOldest: Vec[ValidIO[UInt]] = NewAgeDetector(
    numEntries = param.numEnq,
    enq = enqEntryEnqNotFlush,
    canIssue = enqEntryCanIssueVec,
  )

  private val fastEntryOldest: Vec[ValidIO[UInt]] = AgeDetector(
    numEntries = param.numFastEntry,
    enq = fastEntryEnqNotFlushOH,
    canIssue = fastEntryCanIssueVec,
  )

  for (etyIdx <- in.enq.indices) {
    val enqBits = in.enq(etyIdx).bits
    val etyBits = enqEntries(etyIdx).bits
    for (srcIdx <- enqBits.psrc.indices) {
      enqEntryFpWbM2WakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.fpWbM2Vec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
      enqEntryFpWbM2D1WakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.fpWbM2D1Vec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
    }
  }

  for (etyIdx <- fastEntries.indices) {
    val etyBits = fastEntries(etyIdx).bits

    fastEntryEnqFpWbM2WakeUpMatchVec(etyIdx) := Mux1H(fastEntryEmptySel.map(sel => sel.valid && sel.bits(etyIdx)), enqEntryFpWbM2WakeUpMatchVec)
    fastEntryEnqNotFlush(etyIdx) := Mux1H(fastEntryEmptySel.map(sel => sel.valid && sel.bits(etyIdx)), enqEntryCanTransfer)

    for (srcIdx <- etyBits.status.srcStatus.indices) {
      fastEntryFpWbM2WakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.fpWbM2Vec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
    }
  }

  /**
   * Assignment for [[enqEntries]]
   */
  for (((ety, keep, enq), enqIdx) <- (enqEntries lazyZip enqEntriesKeepNext lazyZip enqEntriesEnqNext).zipWithIndex) {
    ety.valid := Mux1H(Seq(
      enq.valid -> !in.flush.valid,
      keep.valid -> !fastEntryEmptyValid(enqIdx),
    ))

    ety.bits := Mux1H(Seq(
      enq.valid -> enq.bits,
      keep.valid -> keep.bits,
    ))
  }

  /**
   * Assignment for [[fastEntries]]
   */
  for (((ety, keep, enq), fastIdx) <- (fastEntries lazyZip fastEntriesKeepNext lazyZip fastEntriesEnqNext).zipWithIndex) {
    ety.valid := enq.valid || keep.valid

    ety.bits := Mux1H(Seq(
      enq.valid -> enq.bits,
      keep.valid -> keep.bits,
    ))
  }

  /**
   * Assignment for [[enqEntriesEnqNext]]
   */
  for (((etyEnqNext: ValidIO[Entry], enq: ValidIO[Entry]), enqIdx) <- (enqEntriesEnqNext lazyZip enqEntryEnq).zipWithIndex) {
    // Connect all bundles by default
    etyEnqNext := enq
    // Only change the connection of status
//    (etyEnqNext.bits.status, enq.bits.status) match { case (sNext: Status, es: Status) =>
//      enqNextUpdate(
//        entryIdx = enqIdx,
//        statusSink = sNext,
//        statusSource = es,
//        fpWbM2WakeUpMatchVec = enqEntryEnqFpWbM2WakeUpMatchVec(enqIdx),
//        fpWbM2D1WakeUpMatchVec = enqEntryEnqFpWbM2D1WakeUpMatchVec(enqIdx),
//      )
//    }
  }

  /**
   * Assignment for [[enqEntriesKeepNext]]
   */
  for (((etyKeepNext: ValidIO[Entry], ety: ValidIO[Entry]), enqIdx) <- (enqEntriesKeepNext lazyZip enqEntries).zipWithIndex) {
    // Connect all bundles by default
    etyKeepNext := ety
    // Invalid the entry if it is issued successfully or is flushed
    etyKeepNext.valid := ety.valid && !enqEntrySuccess(enqIdx) && !enqEntryFlush(enqIdx) && !fastEntryEmptySel(enqIdx).valid
    // Change the connection of status
    keepNextUpdate(
      entryIdx = enqIdx,
      statusSink = etyKeepNext.bits.status,
      statusSource = ety.bits.status,
      entryValid = ety.valid,
      fpWbM2WakeUpMatchVec = enqEntryFpWbM2WakeUpMatchVec(enqIdx),
      fpWbM2D1WakeUpMatchVec = enqEntryFpWbM2D1WakeUpMatchVec(enqIdx),
      deqSel = enqEntryDeqSel(enqIdx),
      cancel = enqEntryCancel(enqIdx),
    )
  }

  /**
   * Assignment for [[fastEntryEnq]]
   */
  for ((enq, etyIdx) <- fastEntryEnq.zipWithIndex) {
    // Todo[timing]: check if can only use x.bits(etyIdx) as select signal
    enq.bits := Mux1H(fastEntryEmptySel.map(x => x.valid && x.bits(etyIdx)), enqEntriesKeepNext.map(_.bits))
    enq.valid := Mux1H(
      fastEntryEmptySel.map(x => x.valid && x.bits(etyIdx)),
      enqEntryCanTransfer,
    )
  }

  /**
   * Assignment for [[fastEntriesEnqNext]]
   */
  for (((etyEnqNext: ValidIO[Entry], enq: ValidIO[Entry]), fastIdx) <- (fastEntriesEnqNext lazyZip fastEntryEnq).zipWithIndex) {
    // Connect all bundles by default
    etyEnqNext := enq
    // Only change the connection of status
//    (etyEnqNext.bits.status, enq.bits.status) match { case (sNext: Status, es: Status) =>
//      enqNextUpdate(
//        entryIdx = fastIdx + enqEntries.size,
//        statusSink = sNext,
//        statusSource = es,
//        fpWbM2WakeUpMatchVec = fastEntryEnqFpWbM2WakeUpMatchVec(fastIdx),
//        fpWbM2D1WakeUpMatchVec = 0.U.asTypeOf(fastEntryEnqFpWbM2WakeUpMatchVec(fastIdx)),
//      )
//    }
  }

  /**
   * Assignment for [[fastEntriesKeepNext]]
   */
  for (((etyKeepNext: ValidIO[Entry], ety: ValidIO[Entry]), fastIdx) <- (fastEntriesKeepNext lazyZip fastEntries).zipWithIndex) {
    // Connect all bundles by default
    etyKeepNext := ety
    // Invalid the entry if it is issued successfully
    etyKeepNext.valid := ety.valid && !fastEntrySuccess(fastIdx) && !fastEntryFlush(fastIdx)
    // Change the connection of status
    keepNextUpdate(
      entryIdx = fastIdx + enqEntries.size,
      statusSink = etyKeepNext.bits.status,
      statusSource = ety.bits.status,
      entryValid = ety.valid,
      fpWbM2WakeUpMatchVec = fastEntryFpWbM2WakeUpMatchVec(fastIdx),
      fpWbM2D1WakeUpMatchVec = 0.U.asTypeOf(fastEntryFpWbM2WakeUpMatchVec(fastIdx)),
      deqSel = fastEntryDeqSel(fastIdx),
      cancel = fastEntryCancel(fastIdx),
    )
  }

  private val deqValidVec = WireInit(VecInit(
    fastEntryOldest zip enqEntryOldest map { case (fast, enq) => fast.valid || enq.valid }
  ))
  // Cat(fast, enq)
  private val deqSelOHVec = Wire(Vec(param.numDeq, UInt(param.numEntry.W)))

  for (deqIdx <- 0 until param.numDeq) {
    deqSelOHVec(deqIdx) := Cat(
      fastEntryOldest(deqIdx).bits,
      Mux(
        fastEntryOldest(deqIdx).valid,
        0.U(param.numEnq.W),
        enqEntryOldest(deqIdx).bits,
      )
    )
  }

  for ((sel, i) <- entryDeqSel.zipWithIndex) {
    sel := VecInit(deqSelOHVec.map(_(i))).asUInt.orR
  }

  /**
   * Using [[enqEntriesKeepNext]] and [[fastEntriesKeepNext]] to pass updated bypassDelay and bypassSource.
   * Only these bypass info are from wakeup, the other signals should be read directly from status and payload Reg
   */
  private val deqEntries: Vec[Entry] = VecInit(deqSelOHVec.map(
    deqSelOH => Mux1H(deqSelOH, (enqEntriesKeepNext ++ fastEntriesKeepNext).map(_.bits))
  ))

  for ((deq: ValidIO[Deq], valid, deqEty) <- out.deq lazyZip deqValidVec lazyZip deqEntries) {
    deq.valid := valid
    deq.bits.fromEntry(deqEty)
  }

  private val deqPrevValid = RegInit(VecInit(Seq.fill(param.numDeq)(false.B)))
  private val deqPrevRobIdx = RegInit(VecInit(Seq.fill(param.numDeq)(0.U.asTypeOf(new RobPtr))))
  private val deqPrevUopIdx = RegInit(VecInit(Seq.fill(param.numDeq)(0.U.asTypeOf(UopIdx()))))

  for ((deq, deqIdx) <- out.deq.zipWithIndex) {
    val sameAsPrev = deq.valid &&
      deqPrevValid(deqIdx) &&
      deq.bits.robIdx === deqPrevRobIdx(deqIdx) &&
      deq.bits.uopIdx === deqPrevUopIdx(deqIdx)

    assert(
      !sameAsPrev,
      s"FltIssueQueue out.deq($deqIdx) robIdx/uopIdx unchanged for more than 1 cycle while valid"
    )

    deqPrevValid(deqIdx) := deq.valid
    when(deq.valid) {
      deqPrevRobIdx(deqIdx) := deq.bits.robIdx
      deqPrevUopIdx(deqIdx) := deq.bits.uopIdx
    }
  }
  connectWbFuBusyTableIn(
    sink = out.toWbFuBusyTable.intWbFuBusyTableIn,
    wbPortIds = param.intWbPortIds,
    deqWbPortIds = param.exuParams.map(_.getGpWriteCfg.map(_.port)),
    deqWen = out.deq.map(_.bits.gpWen),
    deq = out.deq,
  )
  connectWbFuBusyTableIn(
    sink = out.toWbFuBusyTable.fpWbFuBusyTableIn,
    wbPortIds = param.fpWbPortIds,
    deqWbPortIds = param.exuParams.map(_.getFpWriteCfg.map(_.port)),
    deqWen = out.deq.map(x => x.bits.fpWen || x.bits.gpWen), // TODO, merge FuBusyTable
    deq = out.deq,
  )
  connectWbFuBusyTableIn(
    sink = out.toWbFuBusyTable.vpWbFuBusyTableIn,
    wbPortIds = param.vpWbPortIds,
    deqWbPortIds = param.exuParams.map(_.getVpWriteCfg.map(_.port)),
    deqWen = out.deq.map(_.bits.vpWen),
    deq = out.deq,
  )
  connectWbFuBusyTableIn(
    sink = out.toWbFuBusyTable.v0WbFuBusyTableIn,
    wbPortIds = param.v0WbPortIds,
    deqWbPortIds = param.exuParams.map(_.getV0WriteCfg.map(_.port)),
    deqWen = out.deq.map(_.bits.v0Wen),
    deq = out.deq,
  )
  connectWbFuBusyTableIn(
    sink = out.toWbFuBusyTable.vlWbFuBusyTableIn,
    wbPortIds = param.vlWbPortIds,
    deqWbPortIds = param.exuParams.map(_.getVlWriteCfg.map(_.port)),
    deqWen = out.deq.map(_.bits.vlWen),
    deq = out.deq,
  )
  out.canAccept := PopCount(fastEntries.map(!_.valid)) >= 2.U
  // Todo: optimize it
  out.validNum := PopCount(entries.map(_.valid))

  private def handleSrcWakeUp(
    isKeep: Boolean,
  )(
    entryIdx              : Int,
    srcIdx                : Int,
    statusNext            : SrcStatus,
    status                : SrcStatus,
    fpWbM2WakeUpMatchVec  : Seq[Bool],
    fpWbM2D1WakeUpMatchVec: Seq[Bool],
  ): Unit = {
    val fpWbM2WakeUpVec = fpWbM2WakeUpMatchVec.map(_ && status.fpRen)
    val fpWbM2D1WakeUpVec = fpWbM2D1WakeUpMatchVec.map(_ && status.fpRen)
    val fpWakeUp = Cat(fpWbM2WakeUpVec).orR
    val fpD1WakeUp = fpWbM2D1WakeUpVec.fold(false.B)(_ || _)

    val wakeUp: Bool = fpWakeUp
    val scalarD1WakeUp: Bool = fpD1WakeUp
    val delayWakeUp: Bool = scalarD1WakeUp
    val srcCancel: Bool = entrySrcCancel(entryIdx)(srcIdx)

    statusNext.srcState := Mux(wakeUp || scalarD1WakeUp, true.B, Mux(srcCancel, false.B, status.srcState))

    when (!wakeUp && !delayWakeUp) {
      if (isKeep) {
        statusNext.bypassDelay := Mux(
          status.bypassDelay === BypassDelay.delay3,
          BypassDelay.delay3,
          status.bypassDelay + 1.U
        )
      } else {
        statusNext.bypassDelay := status.bypassDelay
      }
    }.otherwise {
      statusNext.bypassDelay := Mux1H(
        Seq(
          fpWbM2WakeUpVec zip in.wakeup.fpWbM2Vec.map(x => BypassDelay.delay0),
          fpWbM2D1WakeUpVec zip in.wakeup.fpWbM2D1Vec.map(wakeup => BypassDelay.delay1)
        ).reduce(_ ++ _)
      )
    }

    // bypassSource is not needed for waking up from writeback.
    when (!wakeUp && !delayWakeUp) {
      statusNext.bypassSource := status.bypassSource
      statusNext.loadDependency.get.zip(status.loadDependency.get).foreach { case (sink, source) =>
        sink := source << 1
      }
    }.otherwise {
      statusNext.bypassSource.idx := Mux1H(
        Seq(
          fpWbM2WakeUpVec.zipWithIndex,
          fpWbM2D1WakeUpVec.zipWithIndex,
        ).reduce(_ ++ _).map { case (wakeUpMath, exuIdx) => wakeUpMath -> exuIdx.U }
      )
      statusNext.loadDependency.get := Mux1H(
        Seq(
          fpWbM2WakeUpVec.zip(in.wakeup.fpWbM2Vec.map(_.loadDependency)),
          fpWbM2D1WakeUpVec.zip(in.wakeup.fpWbM2D1Vec.map(_.loadDependency)),
        ).reduce(_ ++ _).map { case (wakeUpMath, loadDependency) => wakeUpMath -> loadDependency }
      )
    }
  }

  private def enqNextUpdate(
    entryIdx              : Int,
    statusSink            : Status,
    statusSource          : Status,
    fpWbM2WakeUpMatchVec  : Seq[Seq[Bool]],
    fpWbM2D1WakeUpMatchVec: Seq[Seq[Bool]],
  ): Unit = {
    (statusSink.srcStatus zip statusSource.srcStatus).zipWithIndex.foreach {
      case ((ssSink, ssSource), srcIdx) =>
        this.handleSrcWakeUp(
          isKeep = false
        )(
          entryIdx = entryIdx,
          srcIdx   = srcIdx,
          statusNext = ssSink,
          status = ssSource,
          fpWbM2WakeUpMatchVec = fpWbM2WakeUpMatchVec(srcIdx),
          fpWbM2D1WakeUpMatchVec = fpWbM2D1WakeUpMatchVec(srcIdx),
        )
    }
  }

  private def keepNextUpdate(
    entryIdx              : Int,
    statusSink            : Status,
    statusSource          : Status,
    entryValid            : Bool,
    fpWbM2WakeUpMatchVec  : Seq[Seq[Bool]],
    fpWbM2D1WakeUpMatchVec: Seq[Seq[Bool]],
    deqSel                : Bool,
    cancel                : Bool,
  ): Unit = {
    (statusSink.srcStatus zip statusSource.srcStatus).zipWithIndex.foreach {
      case ((ssSink, ssSource), srcIdx) =>
        this.handleSrcWakeUp(
          isKeep = true
        )(
          entryIdx = entryIdx,
          srcIdx = srcIdx,
          statusNext = ssSink,
          status = ssSource,
          fpWbM2WakeUpMatchVec = fpWbM2WakeUpMatchVec(srcIdx),
          fpWbM2D1WakeUpMatchVec = fpWbM2D1WakeUpMatchVec(srcIdx),
        )
    }

    statusSink.issued := Mux1H(Seq(
      deqSel -> true.B,
      cancel -> false.B,
      (!deqSel && !cancel) -> statusSource.issued,
    ))

    when(deqSel || cancel) {
      statusSink.issuedTimer := IssuedTimer.init
    }.elsewhen(entryValid && statusSource.issued) {
      statusSink.issuedTimer := Mux(
        statusSource.issuedTimer =/= IssuedTimer.maxValue,
        statusSource.issuedTimer + 1.U,
        statusSource.issuedTimer,
      )
    }.otherwise {
      statusSink.issuedTimer := statusSource.issuedTimer
    }
  }

  private def entryCanIssueWithWakeUp(
    entryIdx          : Int,
    status            : Status,
    entryValid        : Bool,
    fpWbM2WakeUpMatchVec: Seq[Seq[Bool]],
  ): Bool = {
    val srcReadyOrWake = VecInit(status.srcStatus.zipWithIndex.map {
      case (srcStatus, srcIdx) =>
        val fpWake = fpWbM2WakeUpMatchVec(srcIdx).map(_ && srcStatus.fpRen).foldLeft(false.B)(_ || _)
        Mux(fpWake, true.B, Mux(entrySrcCancel(entryIdx)(srcIdx), false.B, srcStatus.srcState))
    }).asUInt.andR

    val srcCanIssue = srcReadyOrWake
    entryValid && !status.issued && !status.blocked && srcCanIssue
  }
}

object FltIssueQueue {
  class LazyMod (implicit p: Parameters, val param: IssueParam) extends LazyModule with HasXSParameter {
    override def shouldBeInlined: Boolean = false

    lazy val module = new FltIssueQueue(this)
  }

  val IntCrossRegionVecCycle = 1
  val FpCrossRegionVecCycle = 1

  class WbFuBusyTableReadBundle(implicit p: Parameters, param: IssueParam) extends XSBundle {
    private val intWbPortIds = param.intWbPortIds
    private val fpWbPortIds = param.fpWbPortIds
    private val vpWbPortIds = param.vpWbPortIds
    private val v0WbPortIds = param.v0WbPortIds
    private val vlWbPortIds = param.vlWbPortIds

    val intWbFuBusyTableRead = busyTableRead(intWbPortIds)
    val fpWbFuBusyTableRead = busyTableRead(fpWbPortIds)
    val intCtrlBlockRead = ctrlBlockRead(intWbPortIds)
    val fpCtrlBlockRead = ctrlBlockRead(fpWbPortIds)

    private def busyTableRead(wbPortIds: Seq[Int]): Option[Vec[UInt]] =
      Option.when(wbPortIds.nonEmpty)(
        Vec(wbPortIds.size, UInt(WbFuBusyTable.fixedLatBusyTableEntries().W))
      )

    private def ctrlBlockRead(wbPortIds: Seq[Int]): Option[Vec[WbFuBusyTable.CtrlBlockEntry]] =
      Option.when(wbPortIds.nonEmpty)(
        Vec(wbPortIds.size, new WbFuBusyTable.CtrlBlockEntry)
      )
  }

  class WbFuBusyTableWriteBundle(implicit p: Parameters, param: IssueParam) extends XSBundle {
    private val intWbPortIds = param.intWbPortIds
    private val fpWbPortIds = param.fpWbPortIds
    private val vpWbPortIds = param.vpWbPortIds
    private val v0WbPortIds = param.v0WbPortIds
    private val vlWbPortIds = param.vlWbPortIds

    val intWbFuBusyTableIn = Option.when(intWbPortIds.nonEmpty)(
      new WbFuBusyTable.In(intWbPortIds.size)
    )
    val fpWbFuBusyTableIn = Option.when(fpWbPortIds.nonEmpty)(
      new WbFuBusyTable.In(fpWbPortIds.size)
    )
    val vpWbFuBusyTableIn = Option.when(vpWbPortIds.nonEmpty)(
      new WbFuBusyTable.In(vpWbPortIds.size)
    )
    val v0WbFuBusyTableIn = Option.when(v0WbPortIds.nonEmpty)(
      new WbFuBusyTable.In(v0WbPortIds.size)
    )
    val vlWbFuBusyTableIn = Option.when(vlWbPortIds.nonEmpty)(
      new WbFuBusyTable.In(vlWbPortIds.size)
    )
  }

  class In(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val flush = ValidIO(new Redirect)
    val enq = Vec(param.numEnq, ValidIO(new Enq))
    val resps = new InResp
    val fromWbFuBusyTable = new WbFuBusyTableReadBundle()
    val wakeup = new InWakeUp()
    val ldCancel = Vec(backendParams.LdExuCnt, Input(new LoadCancelIO))
  }

  class Out(implicit p: Parameters, param: IssueParam) extends XSBundle {

    val toWbFuBusyTable = new WbFuBusyTableWriteBundle()

    val deq: MixedVec[ValidIO[Deq]] = param.genIssueBundle(ValidIO(_))

    val canAccept: Bool = Bool()

    val validNum: UInt = UInt(log2Ceil(param.numEntry + 1).W)
  }

  def connectWbFuBusyTableIn(
    sink: Option[WbFuBusyTable.In],
    wbPortIds: Seq[Int],
    deqWbPortIds: Seq[Option[Int]],
    deqWen: Seq[Bool],
    deq: Seq[ValidIO[Deq]],
  ): Unit = {
    final case class WbIssueMatch(valid: Bool, slot: UInt)

    def matchedDeqOps(portId: Int): Seq[(ValidIO[Deq], Int)] =
      deq.zipWithIndex.collect {
        case (deqPort, deqIdx) if deqWbPortIds(deqIdx).contains(portId) => deqPort -> deqIdx
      }

    sink.foreach { in =>
      in.fromIssueQueue.zip(wbPortIds).foreach { case (portIn, portId) =>
        val matches = matchedDeqOps(portId).map { case (deqPort, deqIdx) =>
          val isNonFixedLatFu = FuType.FuTypeOrR(deqPort.bits.fuType, FuType.fDivSqrt)
          val valid = deqPort.valid && deqWen(deqIdx) && !isNonFixedLatFu
          val slot = WbFuBusyTable.writebackSlot(deqPort.bits.latency, busyTableInsertLatencyOffset)
          WbIssueMatch(valid, slot)
        }

        portIn.valid := false.B
        portIn.bits := 0.U.asTypeOf(portIn.bits)

        if (matches.nonEmpty) {
          val matchValid = matches.map(_.valid)
          val matchSlot = matches.map(_.slot)
          portIn.valid := VecInit(matchValid).asUInt.orR
          when (portIn.valid) {
            portIn.bits := Mux1H(matchValid zip matchSlot)
          }
          assert(
            PopCount(matchValid) <= 1.U,
            s"FltIssueQueue drives WB busy table port $portId more than once in one cycle"
          )
        }
      }

      in.fromIssueQueueNonFixedLatFu.zip(wbPortIds).foreach { case (portIn, portId) =>
        val matches = matchedDeqOps(portId).map { case (deqPort, deqIdx) =>
          val isNonFixedLatFu = FuType.FuTypeOrR(deqPort.bits.fuType, FuType.vidiv)
          deqPort.valid && deqWen(deqIdx) && isNonFixedLatFu
        }

        portIn := false.B
        if (matches.nonEmpty) {
          portIn := VecInit(matches).asUInt.orR
          assert(
            PopCount(matches) <= 1.U,
            s"FltIssueQueue drives non-fixed-latency WB busy table port $portId more than once in one cycle"
          )
        }
      }

      in.fromNonFixedLatFu.foreach { portIn =>
        portIn.valid := false.B
        portIn.bits := 0.U.asTypeOf(portIn.bits)
      }
    }
  }

  class FltRespBundle(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val fail = Bool()
    val success = Bool()
    val srcCancel = Vec(param.numRegSrc, Bool())
  }

  class InResp(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val is0 = Vec(param.numDeq, new FltRespBundle)
    val is1 = Vec(param.numDeq, new FltRespBundle)
    val ex0 = Vec(param.numDeq, new FltRespBundle)
    val ex0RespFailLat1 = Vec(backendParams.getFltRegionParam.getFpWriteSize, Bool())
  }

  class InWakeUp(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val fpWbM2Vec = Vec(backendParams.getFpRfWriteSize, new FltWakeUpBundle(backendParams.fpPregParams))
    val fpWbM2D1Vec = Vec(backendParams.getFpRfWriteSize, new FltWakeUpBundle(backendParams.fpPregParams))
  }

  class Payload(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val fuType    = FuType()
    val opcode    = FuOpType()
    val vm        = Option.when(param.needVM)(Bool())
    val vtype     = Option.when(param.readVType)(VType())
    val oldVType  = Option.when(param.readOldVType)(VType())
    val immType   = Option.when(param.needImm)(SelImm())
    val imm       = Option.when(param.needImm)(UInt(param.deqImmTypesMaxLen.W))
    val frm       = Option.when(param.readFrm)(Frm())
    val fflagsWen = Option.when(param.needFflagsWen)(Bool())
    val vxsatWen  = Option.when(param.needVxsatWen)(Bool())
    val latency   = Latency()
    val uopIdx    = UopIdx()
    val lastUop   = Bool()
    val srcType   = Vec(param.numRegSrc, SrcType())
    val gpWen     = Option.when(param.needGpWen)(Bool())
    val fpWen     = Option.when(param.needFpWen)(Bool())
    val vpWen     = Option.when(param.needVpWen)(Bool())
    val v0Wen     = Option.when(param.needV0Wen)(Bool())
    val vlWen     = Option.when(param.needVlWen)(Bool())
    val pdest     = UInt(PhyRegIdxWidth.W)
    val pdestV0   = Option.when(param.needV0Wen)(UInt(V0PhyRegIdxWidth.W))
    val pdestVl   = Option.when(param.needVlWen)(UInt(VlPhyRegIdxWidth.W))
    val sqIdx     = Option.when(param.needSqIdx)(new SqPtr)

    val flushPipe = Option.when(param.needFlushPipe)(Bool())
    val debug     = Option.when(backendParams.debugEn)(new IssueQueueInDebug)

    def fromEnq(enq: Enq): Unit = {
      this.fuType := enq.fuType
      this.opcode := enq.opcode
      this.vm.foreach(_ := enq.vm)
      this.vtype.foreach(_ := enq.vtype)
      this.oldVType.foreach(_ := enq.oldVType)
      this.immType.foreach(_ := enq.selImm)
      this.imm.foreach(_ := enq.imm)
      this.frm.foreach(_ := enq.frm.get)
      this.fflagsWen.foreach(_ := enq.fflagsWen)
      this.vxsatWen.foreach(_ := enq.vxsatWen)
      this.latency := enq.latency
      this.uopIdx := enq.uopIdx
      this.lastUop := enq.lastUop
      this.srcType := enq.srcType
      this.gpWen.foreach(_ := enq.gpWen)
      this.fpWen.foreach(_ := enq.fpWen)
      this.vpWen.foreach(_ := enq.vpWen)
      this.v0Wen.foreach(_ := enq.v0Wen)
      this.vlWen.foreach(_ := enq.vlWen)
      this.pdest := enq.pdest
      this.pdestV0.foreach(_ := enq.pdestV0)
      this.pdestVl.foreach(_ := enq.pdestVl)
      this.sqIdx.foreach(_ := enq.sqIdx)
      this.flushPipe.foreach(_ := enq.flushPipe)
      this.debug.foreach(_ := enq.debug.get)
    }
  }

  class VlSrcStatus(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val ren         = Bool()
    val psrc        = UInt(backendParams.getPregParams(VlData()).addrWidth.W)
    val srcState    = SrcState()
    val bypassDelay = BypassDelay()
  }

  class V0SrcStatus(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val ren         = Bool()
    val psrc        = UInt(backendParams.getPregParams(V0Data()).addrWidth.W)
    val srcState    = SrcState()
    val bypassDelay = BypassDelay()
  }

  class BypassSource(implicit p: Parameters) extends XSBundle {
    // Todo: make it configurable
    val idx = UInt(4.W)
  }

  object SrcState {
    def apply(): Bool = Bool()
  }

  object IssuedTimer {
    def apply()(implicit param: IssueParam): UInt = UInt(width.W)

    def width(implicit param: IssueParam): Int = param.issuedTimerWidth

    def init(implicit param: IssueParam): UInt = 0.U(width.W)

    def maxValue(implicit param: IssueParam): UInt = 3.U(width.W)
  }

  def entryCanIssueOnDeq(
    fromWbFuBusyTable: WbFuBusyTableReadBundle,
    param: IssueParam,
    entryValid: Bool,
    entry: Entry,
    canIssue: Bool,
    deqIdx: Int,
  ): Bool = {
    entryValid &&
      canIssue &&
      entry.status.deqPortIdx === deqIdx.U &&
      !WbFuBusyTable.entryWbConflict(
        fromWbFuBusyTable,
        param,
        entry,
        deqIdx,
        WbFuBusyTable.writebackSlot(entry.payload.latency, busyTableConflictLatencyOffset)
      )
  }

  class EnqPolicy(numEntry: Int, numEnq: Int) extends Module {
    val canEnq = IO(Input(UInt(numEntry.W)))
    val enqSelOHVec = IO(Vec(numEnq, ValidIO(UInt(numEntry.W))))

    val canEnqVec = canEnq.asBools
    // Todo: support more policies
    val selVec: Seq[(Bool, Vec[Bool])] = enqSelOHVec.indices.map(i => SelectOne("circ", canEnqVec, numEnq).getNthOH(i + 1))

    enqSelOHVec.zip(selVec).foreach { case (enqOH, (selValid, selOH)) =>
      enqOH.valid := selValid
      enqOH.bits := selOH.asUInt
    }
  }

  private val busyTableConflictLatencyOffset = 4
  private val busyTableInsertLatencyOffset = busyTableConflictLatencyOffset - 1


  object EnqPolicy {
    def apply(canEnq: UInt, numEnq: Int): Vec[ValidIO[UInt]] = {
      val enqPolicy = Module(new EnqPolicy(canEnq.getWidth, numEnq))
      enqPolicy.canEnq := canEnq
      enqPolicy.enqSelOHVec
    }
  }

  class FltWakeUpBundle(val pregParams: PregParams)(implicit p: Parameters) extends XSBundle {
    val wen = Bool()
    val pdest = UInt(pregParams.addrWidth.W)
    val loadDependency = Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))
  }
}
