package xiangshan.backend.vector

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
import xiangshan.backend.vector.VecIssueQueue._
import xiangshan.mem.SqPtr


class VecIssueQueue(
  override val wrapper: VecIssueQueue.LazyMod
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
  // Delay1 wakeup regs are used to pass the bypass source info to the uops that have not enqueued when wakeup valid.
  // Comparing use many register in BusyTable to record bypass source info, delaying them has less cost at area.
  // Since there are 3 stages between vis0 and vex0, the waking up signal should be sent out 3 cycles ahead of it
  // writing back. The delay1 signal stores an aged bypassDelay because the entry will be aged again before deq.
  private val gpWbD1WakeUp: Vec[WakeUpBundle] = Reg(chiselTypeOf(wakeup.gpWbVec))
  gpWbD1WakeUp zip wakeup.gpWbVec foreach {
    case (delay1, delay0) =>
      delay1.wen := delay0.wen
      when(delay0.wen) {
        delay1.pdest := delay0.pdest
        delay1.delay := delay0.delay
      }
  }
  private val fpWbD1WakeUp: Vec[WakeUpBundle] = Reg(chiselTypeOf(wakeup.fpWbVec))
  fpWbD1WakeUp zip wakeup.fpWbVec foreach {
    case (delay1, delay0) =>
      delay1.wen := delay0.wen
      when(delay0.wen) {
        delay1.pdest := delay0.pdest
        delay1.delay := delay0.delay
      }
  }
  private val vpWbM3D1WakeUp: Vec[WakeUpBundle] = Reg(chiselTypeOf(wakeup.vpWbM3Vec))
  vpWbM3D1WakeUp zip wakeup.vpWbM3Vec foreach {
    case (delay1, delay0) =>
      delay1.wen := delay0.wen
      when (delay0.wen) {
        delay1.pdest := delay0.pdest
        delay1.delay := delay0.delay
      }
  }

  private val enqEntryEnqGpWbWakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.gpWbVec.size, Bool()))))
  private val enqEntryEnqFpWbWakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.fpWbVec.size, Bool()))))
  private val enqEntryEnqGpWbD1WakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.gpWbVec.size, Bool()))))
  private val enqEntryEnqFpWbD1WakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.fpWbVec.size, Bool()))))
  private val enqEntryEnqV0WbWakeUpMatchVec = in.wakeup.v0WbVec.map(x => Wire(Vec(param.numEnq, Vec(x.size, Bool()))))
  private val enqEntryEnqVlWbWakeUpMatchVec = in.wakeup.vlWb0Vec.map(x => Wire(Vec(param.numEnq, Vec(x.size, Bool()))))
  private val enqEntryEnqVpWbM3WakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.vpWbM3Vec.size, Bool()))))
  private val enqEntryEnqVpWbM3D1WakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.vpWbM3Vec.size, Bool()))))

  private val fastEntryEnqGpWbWakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.gpWbVec.size, Bool()))))
  private val fastEntryEnqFpWbWakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.fpWbVec.size, Bool()))))
  private val fastEntryEnqV0WbWakeUpMatchVec = in.wakeup.v0WbVec.map(x => Wire(Vec(param.numFastEntry, Vec(x.size, Bool()))))
  private val fastEntryEnqVlWbWakeUpMatchVec = in.wakeup.vlWb0Vec.map(x => Wire(Vec(param.numFastEntry, Vec(x.size, Bool()))))
  private val fastEntryEnqVpWbM3WakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.vpWbM3Vec.size, Bool()))))

  private val enqEntryGpWbWakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.gpWbVec.size, Bool()))))
  private val enqEntryFpWbWakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.fpWbVec.size, Bool()))))
  private val enqEntryV0WbWakeUpMatchVec = in.wakeup.v0WbVec.map(x => Wire(Vec(param.numEnq, Vec(x.size, Bool()))))
  private val enqEntryVlWbWakeUpMatchVec = in.wakeup.vlWb0Vec.map(x => Wire(Vec(param.numEnq, Vec(x.size, Bool()))))
  private val enqEntryVpWbM3WakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.vpWbM3Vec.size, Bool()))))

  private val fastEntryGpWbWakeUpMatchVec: Vec[Vec[Vec[Bool]]] = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.gpWbVec.size, Bool()))))
  private val fastEntryFpWbWakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.fpWbVec.size, Bool()))))
  private val fastEntryV0WbWakeUpMatchVec = in.wakeup.v0WbVec.map(x => Wire(Vec(param.numFastEntry, Vec(x.size, Bool()))))
  private val fastEntryVlWbWakeUpMatchVec: Option[Vec[Vec[Bool]]] = in.wakeup.vlWb0Vec.map(x => Wire(Vec(param.numFastEntry, Vec(x.size, Bool()))))
  private val fastEntryVpWbM3WakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.vpWbM3Vec.size, Bool()))))

  private val enqEntryCanIssue = VecInit(enqEntries.zipWithIndex.map {
    case (ety, enqIdx) =>
      entryCanIssueWithWakeUp(
        status = ety.bits.status,
        entryValid = ety.valid,
        gpWbWakeUpMatchVec = enqEntryGpWbWakeUpMatchVec(enqIdx),
        fpWbWakeUpMatchVec = enqEntryFpWbWakeUpMatchVec(enqIdx),
        vpWbM3WakeUpMatchVec = enqEntryVpWbM3WakeUpMatchVec(enqIdx),
        v0WbWakeUpMatchVec = enqEntryV0WbWakeUpMatchVec.map(_(enqIdx)),
        vlWbWakeUpMatchVec = enqEntryVlWbWakeUpMatchVec.map(_(enqIdx)),
      )
  })
  private val enqEntryCanIssueVec: Vec[UInt] = VecInit(
    (0 until param.numDeq).map(deqIdx => VecInit(enqEntries.zip(enqEntryCanIssue).map {
      case (ety, canIssue) => VecIssueQueue.entryCanIssueOnDeq(in.fromWbFuBusyTable, param, ety.valid, ety.bits, canIssue, deqIdx)
    }).asUInt)
  )
  private val fastEntryCanIssue = VecInit(fastEntries.zipWithIndex.map {
    case (ety, fastIdx) =>
      entryCanIssueWithWakeUp(
        status = ety.bits.status,
        entryValid = ety.valid,
        gpWbWakeUpMatchVec = fastEntryGpWbWakeUpMatchVec(fastIdx),
        fpWbWakeUpMatchVec = fastEntryFpWbWakeUpMatchVec(fastIdx),
        vpWbM3WakeUpMatchVec = fastEntryVpWbM3WakeUpMatchVec(fastIdx),
        v0WbWakeUpMatchVec = fastEntryV0WbWakeUpMatchVec.map(_(fastIdx)),
        vlWbWakeUpMatchVec = fastEntryVlWbWakeUpMatchVec.map(_(fastIdx)),
      )
  })
  private val fastEntryCanIssueVec: Vec[UInt] = VecInit(
    (0 until param.numDeq).map(deqIdx => VecInit(fastEntries.zip(fastEntryCanIssue).map {
      case (ety, canIssue) => VecIssueQueue.entryCanIssueOnDeq(in.fromWbFuBusyTable, param, ety.valid, ety.bits, canIssue, deqIdx)
    }).asUInt)
  )

  private val enqEntryDeqSel = Wire(Vec(param.numEnq, Bool()))
  private val fastEntryDeqSel = Wire(Vec(param.numFastEntry, Bool()))
  private val entryDeqSel: Seq[Bool] = enqEntryDeqSel ++ fastEntryDeqSel

  private val enqEntryCancel = Wire(Vec(param.numEnq, Bool()))
  private val fastEntryCancel = Wire(Vec(param.numFastEntry, Bool()))
  private val entryCancel: Seq[Bool] = enqEntryCancel ++ fastEntryCancel

  for ((cancel, entry) <- entryCancel.zip(entries)) {
    cancel := Mux1H(Seq(
      in.resps.is0(entry.bits.status.deqPortIdx).fail -> (entry.bits.status.issued && entry.bits.status.issuedTimer === 0.U),
      in.resps.is1(entry.bits.status.deqPortIdx).fail -> (entry.bits.status.issued && entry.bits.status.issuedTimer === 1.U),
    ))
  }

  private val enqEntrySuccess = Wire(Vec(param.numEnq, Bool()))
  private val fastEntrySuccess = Wire(Vec(param.numFastEntry, Bool()))
  private val entrySuccess: Seq[Bool] = enqEntrySuccess ++ fastEntrySuccess

  for ((success, entry) <- entrySuccess.zip(entries)) {
    success := Mux1H(Seq(
      in.resps.is0(entry.bits.status.deqPortIdx).success -> (entry.bits.status.issued && entry.bits.status.issuedTimer === 0.U),
      in.resps.is1(entry.bits.status.deqPortIdx).success -> (entry.bits.status.issued && entry.bits.status.issuedTimer === 1.U),
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
      enqEntryEnqGpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.gpWbVec.map(x => x.wen && x.pdest === enqBits.psrc(srcIdx))
      enqEntryEnqFpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.fpWbVec.map(x => x.wen && x.pdest === enqBits.psrc(srcIdx))
      enqEntryEnqGpWbD1WakeUpMatchVec(etyIdx)(srcIdx) := gpWbD1WakeUp.map(x => x.wen && x.pdest === enqBits.psrc(srcIdx))
      enqEntryEnqFpWbD1WakeUpMatchVec(etyIdx)(srcIdx) := fpWbD1WakeUp.map(x => x.wen && x.pdest === enqBits.psrc(srcIdx))
      enqEntryEnqVpWbM3WakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.vpWbM3Vec.map(x => x.wen && x.pdest === enqBits.psrc(srcIdx))
      enqEntryEnqVpWbM3D1WakeUpMatchVec(etyIdx)(srcIdx) := vpWbM3D1WakeUp.map(x => x.wen && x.pdest === enqBits.psrc(srcIdx))
      enqEntryGpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.gpWbVec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
      enqEntryFpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.fpWbVec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)

      enqEntryVpWbM3WakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.vpWbM3Vec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
    }
    enqEntryEnqV0WbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.v0WbVec.get.map(x => x.wen && x.pdest === enqBits.psrcV0))
    enqEntryEnqVlWbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.vlWb0Vec.get.map(x => x.wen && x.pdest === enqBits.psrcVl))
    enqEntryV0WbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.v0WbVec.get.map(
      x => etyBits.status.srcStatusV0
        .map(_.psrc === x.pdest && x.wen)
        .getOrElse(false.B)
    ))
    enqEntryVlWbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.vlWb0Vec.get.map(
      x => etyBits.status.srcStatusVl
        .map(_.psrc === x.pdest && x.wen)
        .getOrElse(false.B)
    ))
  }

  for (etyIdx <- fastEntries.indices) {
    val etyBits = fastEntries(etyIdx).bits

    fastEntryEnqGpWbWakeUpMatchVec(etyIdx) := Mux1H(fastEntryEmptySel.map(sel => sel.valid && sel.bits(etyIdx)), enqEntryGpWbWakeUpMatchVec)
    fastEntryEnqFpWbWakeUpMatchVec(etyIdx) := Mux1H(fastEntryEmptySel.map(sel => sel.valid && sel.bits(etyIdx)), enqEntryFpWbWakeUpMatchVec)
    fastEntryEnqV0WbWakeUpMatchVec.foreach(_(etyIdx) := Mux1H(fastEntryEmptySel.map(sel => sel.valid && sel.bits(etyIdx)), enqEntryV0WbWakeUpMatchVec.get))
    fastEntryEnqVlWbWakeUpMatchVec.foreach(_(etyIdx) := Mux1H(fastEntryEmptySel.map(sel => sel.valid && sel.bits(etyIdx)), enqEntryVlWbWakeUpMatchVec.get))

    fastEntryEnqVpWbM3WakeUpMatchVec(etyIdx) := Mux1H(fastEntryEmptySel.map(sel => sel.valid && sel.bits(etyIdx)), enqEntryVpWbM3WakeUpMatchVec)

    fastEntryEnqNotFlush(etyIdx) := Mux1H(fastEntryEmptySel.map(sel => sel.valid && sel.bits(etyIdx)), enqEntryCanTransfer)

    for (srcIdx <- etyBits.status.srcStatus.indices) {
      fastEntryGpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.gpWbVec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
      fastEntryFpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.fpWbVec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)

      fastEntryVpWbM3WakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.vpWbM3Vec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
    }
    fastEntryV0WbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.v0WbVec.get.map(x => x.wen && x.pdest === etyBits.status.srcStatusV0.get.psrc))
    fastEntryVlWbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.vlWb0Vec.get.map(x => x.wen && x.pdest === etyBits.status.srcStatusVl.get.psrc))
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
    (etyEnqNext.bits.status, enq.bits.status) match { case (sNext: Status, es: Status) =>
      enqNextUpdate(
        statusSink = sNext,
        statusSource = es,
        gpWbWakeUpMatchVec = enqEntryEnqGpWbWakeUpMatchVec(enqIdx),
        fpWbWakeUpMatchVec = enqEntryEnqFpWbWakeUpMatchVec(enqIdx),
        vpWbM3WakeUpMatchVec = enqEntryEnqVpWbM3WakeUpMatchVec(enqIdx),
        v0WbWakeUpMatchVec = enqEntryEnqV0WbWakeUpMatchVec.map(_(enqIdx)),
        vlWbWakeUpMatchVec = enqEntryEnqVlWbWakeUpMatchVec.map(_(enqIdx)),
        gpWbD1WakeUpMatchVec = Some(enqEntryEnqGpWbD1WakeUpMatchVec(enqIdx)),
        fpWbD1WakeUpMatchVec = Some(enqEntryEnqFpWbD1WakeUpMatchVec(enqIdx)),
        vpWbM3D1WakeUpMatchVec = Some(enqEntryEnqVpWbM3D1WakeUpMatchVec(enqIdx)),
      )
    }
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
      statusSink = etyKeepNext.bits.status,
      statusSource = ety.bits.status,
      entryValid = ety.valid,
      gpWbWakeUpMatchVec = enqEntryGpWbWakeUpMatchVec(enqIdx),
      fpWbWakeUpMatchVec = enqEntryFpWbWakeUpMatchVec(enqIdx),
      vpWbM3WakeUpMatchVec = enqEntryVpWbM3WakeUpMatchVec(enqIdx),
      v0WbWakeUpMatchVec = enqEntryV0WbWakeUpMatchVec.map(_(enqIdx)),
      vlWbWakeUpMatchVec = enqEntryVlWbWakeUpMatchVec.map(_(enqIdx)),
      deqSel = enqEntryDeqSel(enqIdx),
      cancel = enqEntryCancel(enqIdx),
      gpWbD1WakeUpMatchVec = None,
      fpWbD1WakeUpMatchVec = None,
      vpWbM3D1WakeUpMatchVec = None,
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
    (etyEnqNext.bits.status, enq.bits.status) match { case (sNext: Status, es: Status) =>
      enqNextUpdate(
        statusSink = sNext,
        statusSource = es,
        gpWbWakeUpMatchVec = fastEntryEnqGpWbWakeUpMatchVec(fastIdx),
        fpWbWakeUpMatchVec = fastEntryEnqFpWbWakeUpMatchVec(fastIdx),
        vpWbM3WakeUpMatchVec = fastEntryEnqVpWbM3WakeUpMatchVec(fastIdx),
        v0WbWakeUpMatchVec = fastEntryEnqV0WbWakeUpMatchVec.map(_(fastIdx)),
        vlWbWakeUpMatchVec = fastEntryEnqVlWbWakeUpMatchVec.map(_(fastIdx)),
        gpWbD1WakeUpMatchVec = None,
        fpWbD1WakeUpMatchVec = None,
        vpWbM3D1WakeUpMatchVec = None,
      )
    }
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
      statusSink = etyKeepNext.bits.status,
      statusSource = ety.bits.status,
      entryValid = ety.valid,
      gpWbWakeUpMatchVec = fastEntryGpWbWakeUpMatchVec(fastIdx),
      fpWbWakeUpMatchVec = fastEntryFpWbWakeUpMatchVec(fastIdx),
      vpWbM3WakeUpMatchVec = fastEntryVpWbM3WakeUpMatchVec(fastIdx),
      v0WbWakeUpMatchVec = fastEntryV0WbWakeUpMatchVec.map(_(fastIdx)),
      vlWbWakeUpMatchVec = fastEntryVlWbWakeUpMatchVec.map(_(fastIdx)),
      deqSel = fastEntryDeqSel(fastIdx),
      cancel = fastEntryCancel(fastIdx),
      gpWbD1WakeUpMatchVec = None,
      fpWbD1WakeUpMatchVec = None,
      vpWbM3D1WakeUpMatchVec = None,
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
      s"VecIssueQueue out.deq($deqIdx) robIdx/uopIdx unchanged for more than 1 cycle while valid"
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
    deqWen = out.deq.map(_.bits.fpWen),
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
    statusNext            : SrcStatus,
    status                : SrcStatus,
    gpWbWakeUpMatchVec    : Seq[Bool],
    fpWbWakeUpMatchVec    : Seq[Bool],
    vpWbM3WakeUpMatchVec  : Seq[Bool],
    gpWbD1WakeUpMatchVec  : Option[Seq[Bool]],
    fpWbD1WakeUpMatchVec  : Option[Seq[Bool]],
    vpWbM3D1WakeUpMatchVec: Option[Seq[Bool]],
  ): Unit = {
    val gpWbWakeUpVec = gpWbWakeUpMatchVec.map(_ && status.gpRen)
    val fpWbWakeUpVec = fpWbWakeUpMatchVec.map(_ && status.fpRen)
    val vpWbM3WakeUpVec = vpWbM3WakeUpMatchVec.map(_ && status.vpRen)
    // Only used to set bypassDelay and bypassSource
    // Only used between enqEntryEnq and enqEntries. Otherwise, this bundle should be None
    val gpWbD1WakeUpVec: Seq[Bool] = gpWbD1WakeUpMatchVec.map(_.map(_ && status.gpRen)).getOrElse(Seq())
    val fpWbD1WakeUpVec: Seq[Bool] = fpWbD1WakeUpMatchVec.map(_.map(_ && status.fpRen)).getOrElse(Seq())
    val vpWbM3D1WakeUpVec: Seq[Bool] = vpWbM3D1WakeUpMatchVec.map(_.map(_ && status.vpRen)).getOrElse(Seq())
    val gpWakeUp = Cat(gpWbWakeUpVec).orR
    val fpWakeUp = Cat(fpWbWakeUpVec).orR
    val vpWbM3WakeUp = Cat(vpWbM3WakeUpVec).orR
    val gpD1WakeUp = gpWbD1WakeUpVec.fold(false.B)(_ || _)
    val fpD1WakeUp = fpWbD1WakeUpVec.fold(false.B)(_ || _)
    val vpD1WakeUp = vpWbM3D1WakeUpVec.fold(false.B)(_ || _)

    val wakeUp: Bool = gpWakeUp || fpWakeUp || vpWbM3WakeUp
    val scalarD1WakeUp: Bool = gpD1WakeUp || fpD1WakeUp
    val delayWakeUp: Bool = scalarD1WakeUp || vpD1WakeUp

    statusNext.srcState := status.srcState || wakeUp || scalarD1WakeUp

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
          gpWbWakeUpVec zip in.wakeup.gpWbVec.map(_.delay),
          fpWbWakeUpVec zip in.wakeup.fpWbVec.map(_.delay),
          vpWbM3WakeUpVec zip in.wakeup.vpWbM3Vec.map(_.delay),
          gpWbD1WakeUpVec zip Iterator.continually(BypassDelay.delay2),
          fpWbD1WakeUpVec zip Iterator.continually(BypassDelay.delay2),
          vpWbM3D1WakeUpVec zip vpWbM3D1WakeUp.map(wakeup => Mux(
            wakeup.delay === BypassDelay.delay3,
            BypassDelay.delay3,
            wakeup.delay + 1.U
          )),
        ).reduce(_ ++ _)
      )
    }

    // bypassSource is not needed for waking up from writeback.
    when (!wakeUp && !delayWakeUp) {
      statusNext.bypassSource := status.bypassSource
    }.otherwise {
      statusNext.bypassSource.idx := Mux1H(
        Seq(
          gpWbWakeUpVec.zipWithIndex,
          fpWbWakeUpVec.zipWithIndex,
          vpWbM3WakeUpVec.zipWithIndex,
          gpWbD1WakeUpVec.zipWithIndex,
          fpWbD1WakeUpVec.zipWithIndex,
          vpWbM3D1WakeUpVec.zipWithIndex,
        ).reduce(_ ++ _).map { case (wakeUpMath, exuIdx) => wakeUpMath -> exuIdx.U }
      )
    }
  }

  private def enqNextUpdate(
    statusSink            : Status,
    statusSource          : Status,
    gpWbWakeUpMatchVec    : Seq[Seq[Bool]],
    fpWbWakeUpMatchVec    : Seq[Seq[Bool]],
    vpWbM3WakeUpMatchVec  : Seq[Seq[Bool]],
    v0WbWakeUpMatchVec    : Option[Seq[Bool]],
    vlWbWakeUpMatchVec    : Option[Seq[Bool]],
    gpWbD1WakeUpMatchVec  : Option[Seq[Seq[Bool]]],
    fpWbD1WakeUpMatchVec  : Option[Seq[Seq[Bool]]],
    vpWbM3D1WakeUpMatchVec: Option[Seq[Seq[Bool]]],
  ): Unit = {
    (statusSink.srcStatus zip statusSource.srcStatus).zipWithIndex.foreach {
      case ((ssSink, ssSource), srcIdx) =>
        this.handleSrcWakeUp(
          isKeep = false
        )(
          statusNext = ssSink,
          status = ssSource,
          gpWbWakeUpMatchVec = gpWbWakeUpMatchVec(srcIdx),
          fpWbWakeUpMatchVec = fpWbWakeUpMatchVec(srcIdx),
          vpWbM3WakeUpMatchVec = vpWbM3WakeUpMatchVec(srcIdx),
          gpWbD1WakeUpMatchVec = gpWbD1WakeUpMatchVec.map(_(srcIdx)),
          fpWbD1WakeUpMatchVec = fpWbD1WakeUpMatchVec.map(_(srcIdx)),
          vpWbM3D1WakeUpMatchVec = vpWbM3D1WakeUpMatchVec.map(_(srcIdx)),
        )
    }

    (statusSink.srcStatusV0 lazyZip statusSource.srcStatusV0).foreach {
      case (ssNext, ess) =>
        val wakeUpMatch = v0WbWakeUpMatchVec.get
        ssNext.srcState := ess.srcState || Cat(wakeUpMatch).orR
        ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.v0WbVec.get.map(_.delay))
    }

    (statusSink.srcStatusVl lazyZip statusSource.srcStatusVl).foreach {
      case (ssNext, ess) =>
        val wakeUpMatch = vlWbWakeUpMatchVec.get
        ssNext.srcState := ess.srcState || Cat(wakeUpMatch).orR
        ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.vlWb0Vec.get.map(_.delay))
    }
  }

  private def keepNextUpdate(
    statusSink            : Status,
    statusSource          : Status,
    entryValid            : Bool,
    gpWbWakeUpMatchVec    : Seq[Seq[Bool]],
    fpWbWakeUpMatchVec    : Seq[Seq[Bool]],
    vpWbM3WakeUpMatchVec  : Seq[Seq[Bool]],
    v0WbWakeUpMatchVec    : Option[Seq[Bool]],
    vlWbWakeUpMatchVec    : Option[Seq[Bool]],
    gpWbD1WakeUpMatchVec  : Option[Seq[Seq[Bool]]],
    fpWbD1WakeUpMatchVec  : Option[Seq[Seq[Bool]]],
    vpWbM3D1WakeUpMatchVec: Option[Seq[Seq[Bool]]],
    deqSel                : Bool,
    cancel                : Bool,
  ): Unit = {
    (statusSink.srcStatus zip statusSource.srcStatus).zipWithIndex.foreach {
      case ((ssSink, ssSource), srcIdx) =>
        this.handleSrcWakeUp(
          isKeep = true
        )(
          statusNext = ssSink,
          status = ssSource,
          gpWbWakeUpMatchVec = gpWbWakeUpMatchVec(srcIdx),
          fpWbWakeUpMatchVec = fpWbWakeUpMatchVec(srcIdx),
          vpWbM3WakeUpMatchVec = vpWbM3WakeUpMatchVec(srcIdx),
          gpWbD1WakeUpMatchVec = gpWbD1WakeUpMatchVec.map(_(srcIdx)),
          fpWbD1WakeUpMatchVec = fpWbD1WakeUpMatchVec.map(_(srcIdx)),
          vpWbM3D1WakeUpMatchVec = vpWbM3D1WakeUpMatchVec.map(_(srcIdx)),
        )
    }

    (statusSink.srcStatusV0 lazyZip statusSource.srcStatusV0).foreach {
      case (ssNext, ess) =>
        val wakeUpMatch = v0WbWakeUpMatchVec.get
        ssNext.srcState := ess.srcState || Cat(wakeUpMatch).orR
        ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.v0WbVec.get.map(_.delay))
    }

    (statusSink.srcStatusVl lazyZip statusSource.srcStatusVl).foreach {
      case (ssNext, ess) =>
        val wakeUpMatch = vlWbWakeUpMatchVec.get
        ssNext.srcState := ess.srcState || Cat(wakeUpMatch).orR
        ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.vlWb0Vec.get.map(_.delay))
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
    status            : Status,
    entryValid        : Bool,
    gpWbWakeUpMatchVec: Seq[Seq[Bool]],
    fpWbWakeUpMatchVec: Seq[Seq[Bool]],
    vpWbM3WakeUpMatchVec: Seq[Seq[Bool]],
    v0WbWakeUpMatchVec: Option[Seq[Bool]],
    vlWbWakeUpMatchVec: Option[Seq[Bool]],
  ): Bool = {
    val srcReadyOrWake = VecInit(status.srcStatus.zipWithIndex.map {
      case (srcStatus, srcIdx) =>
        val gpWake = gpWbWakeUpMatchVec(srcIdx).map(_ && srcStatus.gpRen).foldLeft(false.B)(_ || _)
        val fpWake = fpWbWakeUpMatchVec(srcIdx).map(_ && srcStatus.fpRen).foldLeft(false.B)(_ || _)
        val vpWake = vpWbM3WakeUpMatchVec(srcIdx).map(_ && srcStatus.vpRen).foldLeft(false.B)(_ || _)
        srcStatus.srcState || gpWake || fpWake || vpWake
    }).asUInt.andR

    require(
      status.srcStatusV0.isDefined == v0WbWakeUpMatchVec.isDefined,
      "status.srcStatusV0 and v0WbWakeUpMatchVec should have the same Option condition"
    )
    require(
      status.srcStatusVl.isDefined == vlWbWakeUpMatchVec.isDefined,
      "status.srcStatusVl and vlWbWakeUpMatchVec should have the same Option condition"
    )

    val v0ReadyOrWake = status.srcStatusV0.map { srcStatusV0 =>
      val v0Wake = v0WbWakeUpMatchVec.get.foldLeft(false.B)(_ || _)
      srcStatusV0.srcState || (srcStatusV0.ren && v0Wake)
    }.getOrElse(true.B)

    val vlReadyOrWake = status.srcStatusVl.map { srcStatusVl =>
      val vlWake = vlWbWakeUpMatchVec.get.foldLeft(false.B)(_ || _)
      srcStatusVl.srcState || (srcStatusVl.ren && vlWake)
    }.getOrElse(true.B)

    val srcCanIssue = srcReadyOrWake && v0ReadyOrWake && vlReadyOrWake
    entryValid && !status.issued && !status.blocked && srcCanIssue
  }
}

object VecIssueQueue {
  class LazyMod (implicit p: Parameters, val param: IssueParam) extends LazyModule with HasXSParameter {
    override def shouldBeInlined: Boolean = false

    lazy val module = new VecIssueQueue(this)
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
    val vpWbFuBusyTableRead = busyTableRead(vpWbPortIds)
    val v0WbFuBusyTableRead = busyTableRead(v0WbPortIds)
    val vlWbFuBusyTableRead = busyTableRead(vlWbPortIds)
    val intCtrlBlockRead = ctrlBlockRead(intWbPortIds)
    val fpCtrlBlockRead = ctrlBlockRead(fpWbPortIds)
    val vpCtrlBlockRead = ctrlBlockRead(vpWbPortIds)
    val v0CtrlBlockRead = ctrlBlockRead(v0WbPortIds)
    val vlCtrlBlockRead = ctrlBlockRead(vlWbPortIds)

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
          val isNonFixedLatFu = FuType.FuTypeOrR(deqPort.bits.fuType, FuType.vidiv)
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
            s"VecIssueQueue drives WB busy table port $portId more than once in one cycle"
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
            s"VecIssueQueue drives non-fixed-latency WB busy table port $portId more than once in one cycle"
          )
        }
      }

      in.fromNonFixedLatFu.foreach { portIn =>
        portIn.valid := false.B
        portIn.bits := 0.U.asTypeOf(portIn.bits)
      }
    }
  }

  class Enq(implicit p: Parameters, param: IssueParam) extends XSBundle {
    def immWidth = 32
    def numRegSrc = param.numRegSrc

    // from decode
    val fuType    = FuType()
    val opcode    = FuOpType()

    val vm        = Bool()
    val vtype     = VType()
    val oldVType  = VType()

    val srcType   = Vec(numRegSrc, SrcType())
    val v0Ren     = Bool()
    val vlRen     = Bool()
    val uopIdx    = UopIdx()
    val lastUop   = Bool()
    val gpWen     = Bool()
    val fpWen     = Bool()
    val vpWen     = Bool()
    val v0Wen     = Bool()
    val vlWen     = Bool()
    val selImm    = SelImm()
    val imm       = UInt(immWidth.W)
    val frm       = Option.when(param.readFrm)(Frm())
    val fflagsWen = Bool()
    val vxsatWen  = Bool()
    val flushPipe = Bool()
    val latency   = Latency()

    // from rename
    val robIdx    = new RobPtr
    val psrc      = Vec(numRegSrc, UInt(PhyRegIdxWidth.W))
    val psrcV0    = UInt(V0PhyRegIdxWidth.W)
    val psrcVl    = UInt(VlPhyRegIdxWidth.W)
    val pdest     = UInt(PhyRegIdxWidth.W)
    val pdestV0   = UInt(V0PhyRegIdxWidth.W)
    val pdestVl   = UInt(VlPhyRegIdxWidth.W)

    // from dispatch
    val srcState  = Vec(numRegSrc, SrcState())
    val srcStateV0 = SrcState()
    val srcStateVl = SrcState()

    val sqIdx     = new SqPtr

    val debug     = Option.when(backendParams.debugEn)(new IssueQueueInDebug)

    def fromDispatchOutUop(source: DispatchOutUop): Unit = {
      this.fuType := source.fuType
      this.opcode := source.fuOpType

      this.vm := source.vm
      this.vtype := source.vtype
      this.oldVType := source.oldVType

      this.srcType := source.srcType
      this.v0Ren := true.B // Todo
      this.vlRen := true.B // Todo
      this.uopIdx := source.uopIdx
      this.lastUop := source.lastUop
      this.gpWen := source.rfWen
      this.fpWen := source.fpWen
      this.vpWen := source.vecWen
      this.v0Wen := source.v0Wen
      this.vlWen := source.vlWen

      this.selImm := source.selImm
      this.imm := source.imm
      this.frm.foreach(_ := source.frm)
      this.fflagsWen := source.fflagsWen
      this.vxsatWen := false.B // Todo
      this.flushPipe := false.B // Todo: Check if it is needed
      this.latency := source.latency
      this.robIdx := source.robIdx
      this.psrc := source.psrc
      this.psrcV0 := source.psrcV0
      this.psrcVl := source.psrcVl
      this.pdest := source.pdest
      this.pdestV0 := source.pdestV0
      this.pdestVl := source.pdestVl

      this.srcState := source.srcState
      this.srcStateV0 := source.srcStateV0
      this.srcStateVl := source.srcStateVl

      this.sqIdx := source.sqIdx

      this.debug.foreach(_ := source.debug.get)
    }

    def fromRegionInUop(source: RegionInUop): Unit = {
      this.fuType := source.fuType
      this.opcode := source.fuOpType

      this.vm := source.vm.getOrElse(false.B)
      this.vtype := source.vtype.getOrElse(0.U.asTypeOf(this.vtype))
      this.oldVType := source.oldVType.getOrElse(0.U.asTypeOf(this.vtype))

      this.srcType := source.srcType
      this.v0Ren := true.B // Todo
      this.vlRen := true.B // Todo

      this.uopIdx := source.uopIdx.get
      this.lastUop := source.lastUop.get
      this.gpWen := source.rfWen.getOrElse(false.B)
      this.fpWen := source.fpWen.getOrElse(false.B)
      this.vpWen := source.vecWen.getOrElse(false.B)
      this.v0Wen := source.v0Wen.getOrElse(false.B)
      this.vlWen := source.vlWen.getOrElse(false.B)

      this.selImm := source.selImm.getOrElse(0.U)
      this.imm := source.imm.getOrElse(0.U)
      this.frm.foreach(_ := source.frm.getOrElse(0.U.asTypeOf(Frm())))
      this.fflagsWen := source.fflagsWen.getOrElse(false.B)
      this.vxsatWen := false.B // Todo
      this.flushPipe := false.B // Todo: Check if it is needed
      this.latency := source.latency
      this.robIdx := source.robIdx
      this.psrc := source.psrc
      this.psrcV0 := source.psrcV0.getOrElse(0.U)
      this.psrcVl := source.psrcVl.getOrElse(0.U)
      this.pdest := source.pdest
      this.pdestV0 := source.pdestV0.getOrElse(0.U)
      this.pdestVl := source.pdestVl.getOrElse(0.U)

      this.srcState := source.srcState
      this.srcStateV0 := source.srcStateV0.getOrElse(0.U)
      this.srcStateVl := source.srcStateVl.getOrElse(0.U)

      this.sqIdx := source.sqIdx.getOrElse(0.U.asTypeOf(this.sqIdx))

      this.debug.foreach(_ := source.debug.get)
    }
  }

  class Deq(val exuParam: ExuParam)(implicit p: Parameters) extends XSBundle {
    implicit val issueParam: IssueParam = exuParam.getIssueParam()

    val fuType       = FuType()
    val opcode       = Opcode()

    val robIdx       = new RobPtr
    val uopIdx       = UopIdx()

    val gpRen        = Vec(exuParam.numRegSrc, Bool())
    val fpRen        = Vec(exuParam.numRegSrc, Bool())
    val vpRen        = Vec(exuParam.numRegSrc, Bool())
    val psrc         = Vec(exuParam.numRegSrc, UInt(PhyRegIdxWidth.W))

    val psrcV0       = Option.when(exuParam.readV0Rf)(ValidIO(UInt(V0PhyRegIdxWidth.W)))
    val psrcVl       = Option.when(exuParam.readVlRf)(ValidIO(UInt(VlPhyRegIdxWidth.W)))
    val imm          = Option.when(exuParam.needImm)(UInt(exuParam.immWidth.W))
    val immType      = Option.when(exuParam.needImm)(SelImm())

    val bypassDelay  = Vec(exuParam.numRegSrc, BypassDelay())
    val bypassSource = Vec(exuParam.numRegSrc, new BypassSource)

    val gpWen        = Bool()
    val fpWen        = Bool()
    val vpWen        = Bool()
    val pdest        = UInt(PhyRegIdxWidth.W)
    val v0Wen        = Bool()
    val vlWen        = Bool()
    val pdestV0      = Option.when(exuParam.needV0Wen)(UInt(V0PhyRegIdxWidth.W))
    val pdestVl      = Option.when(exuParam.needVlWen)(UInt(VlPhyRegIdxWidth.W))

    val fflagsWen    = Option.when(exuParam.needFFlagsWen)(Bool())
    val vxsatWen     = Option.when(exuParam.needVxsatWen)(Bool())
    val latency      = Latency()

    val sqIdx        = Option.when(exuParam.needSqIdx)(new SqPtr)

    val flushPipe    = Option.when(exuParam.needFlushPipe)(Bool())

    val vm           = Option.when(exuParam.needVM)(Bool())
    val frm          = Option.when(exuParam.readFrm)(Frm())
    val vtype        = Option.when(exuParam.readVType)(VType())
    val oldVType     = Option.when(exuParam.readOldVType)(VType())

    val debug        = Option.when(backendParams.debugEn)(new VecRegionModule.DebugBundle)

    def fromEntry(entry: Entry): Unit = {
      this.fuType := entry.payload.fuType
      this.opcode := entry.payload.opcode
      this.vm.foreach(_ := entry.payload.vm.get)
      this.robIdx := entry.status.robIdx
      this.uopIdx := entry.status.uopIdx

      this.gpRen := entry.status.srcStatus.map(_.gpRen)
      this.fpRen := entry.status.srcStatus.map(_.fpRen)
      this.vpRen := entry.status.srcStatus.map(_.vpRen)
      this.psrc := entry.status.srcStatus.map(_.psrc)
      this.psrcV0.foreach(_.valid := entry.status.srcStatusV0.get.ren)
      this.psrcV0.foreach(_.bits := entry.status.srcStatusV0.get.psrc)
      this.psrcVl.foreach(_.valid := entry.status.srcStatusVl.get.ren)
      this.psrcVl.foreach(_.bits := entry.status.srcStatusVl.get.psrc)
      this.immType.foreach(_ := entry.payload.immType.get)
      this.imm.foreach(_ := entry.payload.imm.get)

      this.bypassDelay := entry.status.srcStatus.map(_.bypassDelay)
      this.bypassSource := entry.status.srcStatus.map(_.bypassSource)

      this.gpWen := entry.payload.gpWen.getOrElse(false.B)
      this.fpWen := entry.payload.fpWen.getOrElse(false.B)
      this.vpWen := entry.payload.vpWen.getOrElse(false.B)
      this.v0Wen := entry.payload.v0Wen.getOrElse(false.B)
      this.vlWen := entry.payload.vlWen.getOrElse(false.B)
      this.pdest := entry.payload.pdest
      this.pdestV0.foreach(_ := entry.payload.pdestV0.get)
      this.pdestVl.foreach(_ := entry.payload.pdestVl.get)

      this.fflagsWen.foreach(_ := entry.payload.fflagsWen.get)
      this.vxsatWen.foreach(_ := entry.payload.vxsatWen.get)
      this.latency := entry.payload.latency

      this.sqIdx.foreach(_ := entry.payload.sqIdx.get)

      this.flushPipe.foreach(_ := entry.payload.flushPipe.get)

      this.vm.foreach(_ := entry.payload.vm.get)
      this.frm.foreach(_ := entry.payload.frm.get)
      this.vtype.foreach(_ := entry.payload.vtype.get)
      this.oldVType.foreach(_ := entry.payload.oldVType.get)

      this.debug.foreach { case debug =>
        debug.debug := 0.U.asTypeOf(debug.debug)
        debug.perfDebugInfo := entry.payload.debug.get.perfDebugInfo
        debug.seqNum := entry.payload.debug.get.debug_seqNum
      }
    }
  }

  class InResp(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val is0 = Vec(param.numDeq, new RespBundle)
    val is1 = Vec(param.numDeq, new RespBundle)
  }

  class InWakeUp(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val gpWbVec = Vec(backendParams.getIntRfWriteSize, new WakeUpBundle(backendParams.gpPregParams))
    val fpWbVec = Vec(backendParams.getFpRfWriteSize,  new WakeUpBundle(backendParams.fpPregParams))
    val v0WbVec = Option.when(param.readV0Rf)(Vec(backendParams.getV0RfWriteSize, new WakeUpBundle(backendParams.v0PregParams)))
    val vlWb0Vec = Option.when(param.readVlRf)(Vec(backendParams.getVlRfWriteSize, new WakeUpBundle(backendParams.vlPregParams)))
    // Todo: early wakeup
    val vpWbM3Vec = Vec(backendParams.getVpWriteSize, new WakeUpBundle(backendParams.vpPregParams))
  }

  class Entry(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val status = new Status()
    val payload = new Payload()
  }

  class Status(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val robIdx      = new RobPtr
    val uopIdx      = UopIdx()
    val fuType      = FuType()
    val srcStatus   = Vec(param.numRegSrc, new SrcStatus())
    val srcStatusV0 = Option.when(param.readV0Rf)(new V0SrcStatus)
    val srcStatusVl = Option.when(param.readVlRf)(new VlSrcStatus)

    //issue status
    val blocked     = Bool()
    val issued      = Bool()
    val firstIssue  = Bool()
    val issuedTimer = IssuedTimer()
    val deqPortIdx  = UInt(1.W)

    def srcReady: Bool = VecInit(srcStatus.map(_.srcState)).asUInt.andR &&
      srcStatusV0.map(_.srcState).getOrElse(true.B) &&
      srcStatusVl.map(_.srcState).getOrElse(true.B)

    def canIssue: Bool = this.srcReady && !this.issued && !this.blocked

    def fromEnq(enq: Enq): Unit = {
      this.robIdx := enq.robIdx
      this.uopIdx := enq.uopIdx
      this.fuType := enq.fuType
      for ((s, srcIdx) <- this.srcStatus.zipWithIndex) {
        s.gpRen := SrcType.isXp(enq.srcType(srcIdx))
        s.fpRen := SrcType.isFp(enq.srcType(srcIdx))
        s.vpRen := SrcType.isVp(enq.srcType(srcIdx))
        s.psrc := enq.psrc(srcIdx)
        s.srcState := enq.srcState(srcIdx)
        s.bypassDelay := BypassDelay.delay3
        s.bypassSource.idx := 0.U
      }
      this.srcStatusV0.foreach { case s =>
        s.ren := enq.v0Ren
        s.psrc := enq.psrcV0
        s.srcState := enq.srcStateV0
        s.bypassDelay := BypassDelay.delay3
      }
      this.srcStatusVl.foreach { case s =>
        s.ren := enq.vlRen
        s.psrc := enq.psrcVl
        s.srcState := enq.srcStateVl
        s.bypassDelay := BypassDelay.delay3
      }

      this.blocked := false.B
      this.issued := false.B
      this.firstIssue := true.B
      this.issuedTimer := IssuedTimer.init
      this.deqPortIdx := 0.U // Todo
    }
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

  class SrcStatus(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val gpRen        = Bool()
    val fpRen        = Bool()
    val vpRen        = Bool()
    val psrc         = UInt(PhyRegIdxWidth.W)
    val srcState     = SrcState()
    val bypassDelay  = BypassDelay()
    val bypassSource = new BypassSource()
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

  class RespBundle(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val fail = Bool()
    val success = Bool()
  }

  object BypassDelay extends NamedUInt(2) {
    // get data from wb0
    val delay0 = "b00".U(width.W)
    // get data from wb1
    val delay1 = "b01".U(width.W)
    // get data from wb2
    val delay2 = "b10".U(width.W)
    // get data from wb3
    val delay3 = "b11".U(width.W)
  }

  class BypassSource(implicit p: Parameters) extends XSBundle {
    // Todo: make it configurable
    val idx = UInt(4.W)
  }

  class WakeUpBundle(val pregParams: PregParams)(implicit p: Parameters) extends XSBundle {
    val wen = Bool()
    val pdest = UInt(pregParams.addrWidth.W)
    val delay = BypassDelay()

    def toValidAddr: ValidIO[UInt] = {
      Pipe(wen, pdest, 0)
    }
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
}
