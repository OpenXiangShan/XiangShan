package xiangshan.backend.vector

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals.AddVecLiteralConstructor
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility.{PerfCCT, SelectOne}
import utils.NamedUInt
import xiangshan.backend.Bundles.{DispatchOutUop, IssueQueueInDebug, RegionInUop, UopIdx}
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.decode.opcode.{Latency, Opcode}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.issue.{AgeDetector, NewAgeDetector}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.vector.VecIssueQueue._
import xiangshan._
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

  private val enqEntriesNext = Wire(chiselTypeOf(enqEntries))
  private val fastEntriesNext = Wire(chiselTypeOf(fastEntries))

  enqEntries := enqEntriesNext
  fastEntries := fastEntriesNext

  // Wires
  private val enqEntryValid: UInt = VecInit(enqEntries.map(_.valid)).asUInt
  private val fastEntryValid: UInt = VecInit(fastEntries.map(_.valid)).asUInt

  private val enqEntryEnqNotFlush = WireInit(VecInit(in.enq.map(enq => enq.valid && !in.flush.valid)))
  private val fastEntryEnqNotFlush = Wire(Vec(param.numFastEntry, Bool()))
  private val enqTransFastNotFlush = Wire(Vec(param.numEnq, Bool()))

  private val enqEntryEnq = Wire(Vec(param.numEnq, ValidIO(new Entry)))
  private val fastEntryEnq = Wire(Vec(param.numFastEntry, ValidIO(new Entry)))

  private val enqEntryFlush   = WireInit(VecInit( enqEntries.map(ety => ety.bits.status.robIdx.needFlush(in.flush))))
  private val fastEntryFlush  = WireInit(VecInit(fastEntries.map(ety => ety.bits.status.robIdx.needFlush(in.flush))))

  private val enqEntryEnqGpWbWakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.gpWbVec.size, Bool()))))
  private val enqEntryEnqFpWbWakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.fpWbVec.size, Bool()))))
  private val enqEntryEnqVpWbWakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.vpWbVec.size, Bool()))))
  private val enqEntryEnqV0WbWakeUpMatchVec = in.wakeup.v0WbVec.map(x => Wire(Vec(param.numEnq, Vec(x.size, Bool()))))
  private val enqEntryEnqVlWbWakeUpMatchVec = in.wakeup.vlWbVec.map(x => Wire(Vec(param.numEnq, Vec(x.size, Bool()))))

  private val fastEntryEnqGpWbWakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.gpWbVec.size, Bool()))))
  private val fastEntryEnqFpWbWakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.fpWbVec.size, Bool()))))
  private val fastEntryEnqVpWbWakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.vpWbVec.size, Bool()))))
  private val fastEntryEnqV0WbWakeUpMatchVec = in.wakeup.v0WbVec.map(x => Wire(Vec(param.numFastEntry, Vec(x.size, Bool()))))
  private val fastEntryEnqVlWbWakeUpMatchVec = in.wakeup.vlWbVec.map(x => Wire(Vec(param.numFastEntry, Vec(x.size, Bool()))))

  private val enqEntryGpWbWakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.gpWbVec.size, Bool()))))
  private val enqEntryFpWbWakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.fpWbVec.size, Bool()))))
  private val enqEntryVpWbWakeUpMatchVec = Wire(Vec(param.numEnq, Vec(param.numRegSrc, Vec(in.wakeup.vpWbVec.size, Bool()))))
  private val enqEntryV0WbWakeUpMatchVec = in.wakeup.v0WbVec.map(x => Wire(Vec(param.numEnq, Vec(x.size, Bool()))))
  private val enqEntryVlWbWakeUpMatchVec = in.wakeup.vlWbVec.map(x => Wire(Vec(param.numEnq, Vec(x.size, Bool()))))

  private val fastEntryGpWbWakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.gpWbVec.size, Bool()))))
  private val fastEntryFpWbWakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.fpWbVec.size, Bool()))))
  private val fastEntryVpWbWakeUpMatchVec = Wire(Vec(param.numFastEntry, Vec(param.numRegSrc, Vec(in.wakeup.vpWbVec.size, Bool()))))
  private val fastEntryV0WbWakeUpMatchVec = in.wakeup.v0WbVec.map(x => Wire(Vec(param.numFastEntry, Vec(x.size, Bool()))))
  private val fastEntryVlWbWakeUpMatchVec: Option[Vec[Vec[Bool]]] = in.wakeup.vlWbVec.map(x => Wire(Vec(param.numFastEntry, Vec(x.size, Bool()))))

  private val enqEntryCanIssue = VecInit(enqEntries.map(ety => ety.valid && ety.bits.status.canIssue))
  private val enqEntryCanIssueVec: Vec[UInt] = VecInit(
    (0 until param.numDeq).map(deqIdx => enqEntryCanIssue.asUInt & VecInit(enqEntries.map(_.bits.status.deqPortIdx === deqIdx.U)).asUInt)
  )
  private val fastEntryCanIssue = VecInit(fastEntries.map(ety => ety.valid && ety.bits.status.canIssue))
  private val fastEntryCanIssueVec: Vec[UInt] = VecInit(
    (0 until param.numDeq).map(deqIdx => fastEntryCanIssue.asUInt & VecInit(fastEntries.map(_.bits.status.deqPortIdx === deqIdx.U)).asUInt)
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
  private val fastEntryEmptyOH = VecInit(fastEntryEmptySel.map(empty => Mux(empty.valid, empty.bits, 0.U)))
  private val fastEntryEnqNotFlushOH: Vec[UInt] = VecInit(
    fastEntryEmptySel zip enqEntries zip enqEntryFlush map {
      case ((empty, enqEty), flush) => Mux(empty.valid && enqEty.valid && !flush, empty.bits, 0.U)
    }
  )

  enqTransFastNotFlush := fastEntryEmptyValid zip enqEntryFlush map { case (fastEmpty, flush) => fastEmpty && !flush}

  for ((enq, etyIdx) <- enqEntryEnq.zipWithIndex) {
    val inEnq = in.enq(etyIdx)
    val inEnqBits = inEnq.bits
    enq.valid := inEnq.valid
    enq.bits.payload.fromEnq(inEnqBits)
    enq.bits.status.fromEnq(inEnqBits)
  }

  for ((enq, etyIdx) <- fastEntryEnq.zipWithIndex) {
    enq.bits := Mux1H(fastEntryEmptySel.map(x => x.valid && x.bits(etyIdx)), enqEntries.map(_.bits))
    enq.valid := Mux1H(
      fastEntryEmptySel.map(x => x.valid && x.bits(etyIdx)),
      enqEntries.map(_.valid),
    )
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
      enqEntryEnqVpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.vpWbVec.map(x => x.wen && x.pdest === enqBits.psrc(srcIdx))
      enqEntryGpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.gpWbVec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
      enqEntryFpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.fpWbVec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
      enqEntryVpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.vpWbVec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
    }
    enqEntryEnqV0WbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.v0WbVec.get.map(x => x.wen && x.pdest === enqBits.psrcV0))
    enqEntryEnqVlWbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.vlWbVec.get.map(x => x.wen && x.pdest === enqBits.psrcVl))
    enqEntryV0WbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.v0WbVec.get.map(
      x => etyBits.status.srcStatusV0
        .map(_.psrc === x.pdest && x.wen)
        .getOrElse(false.B)
    ))
    enqEntryVlWbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.vlWbVec.get.map(
      x => etyBits.status.srcStatusVl
        .map(_.psrc === x.pdest && x.wen)
        .getOrElse(false.B)
    ))
  }

  for (etyIdx <- fastEntries.indices) {
    val etyBits = fastEntries(etyIdx).bits

    fastEntryEnqGpWbWakeUpMatchVec(etyIdx) := Mux1H(fastEntryEmptySel.map(_.bits(etyIdx)), enqEntryGpWbWakeUpMatchVec)
    fastEntryEnqFpWbWakeUpMatchVec(etyIdx) := Mux1H(fastEntryEmptySel.map(_.bits(etyIdx)), enqEntryFpWbWakeUpMatchVec)
    fastEntryEnqVpWbWakeUpMatchVec(etyIdx) := Mux1H(fastEntryEmptySel.map(_.bits(etyIdx)), enqEntryVpWbWakeUpMatchVec)
    fastEntryEnqV0WbWakeUpMatchVec.foreach(_(etyIdx) := Mux1H(fastEntryEmptySel.map(_.bits(etyIdx)), enqEntryV0WbWakeUpMatchVec.get))
    fastEntryEnqVlWbWakeUpMatchVec.foreach(_(etyIdx) := Mux1H(fastEntryEmptySel.map(_.bits(etyIdx)), enqEntryVlWbWakeUpMatchVec.get))
    fastEntryEnqNotFlush(etyIdx) := Mux1H(fastEntryEmptySel.map(_.bits(etyIdx)), enqEntryFlush.map(!_))

    for (srcIdx <- etyBits.status.srcStatus.indices) {
      fastEntryGpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.gpWbVec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
      fastEntryFpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.fpWbVec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
      fastEntryVpWbWakeUpMatchVec(etyIdx)(srcIdx) := in.wakeup.vpWbVec.map(x => x.wen && x.pdest === etyBits.status.srcStatus(srcIdx).psrc)
    }
    fastEntryV0WbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.v0WbVec.get.map(x => x.wen && x.pdest === etyBits.status.srcStatusV0.get.psrc))
    fastEntryVlWbWakeUpMatchVec.foreach(_(etyIdx) := in.wakeup.vlWbVec.get.map(x => x.wen && x.pdest === etyBits.status.srcStatusVl.get.psrc))
  }

  for (((entryNext, entry, enq: ValidIO[Entry]), enqIdx) <- (enqEntriesNext lazyZip enqEntries lazyZip enqEntryEnq).zipWithIndex) {
    entryNext.valid := Mux(
      enq.valid && !in.flush.valid,
      true.B,
      Mux(
        enqEntryFlush(enqIdx) || enqEntrySuccess(enqIdx) || fastEntryEmptyValid(enqIdx),
        false.B,
        entry.valid,
      )
    )
    when (enq.valid) {
      entryNext.bits.payload := enq.bits.payload
      entryNext.bits.status := enq.bits.status
      (entryNext.bits.status, enq.bits.status) match { case (sNext: Status, e: Status) =>
        (sNext.srcStatus lazyZip e.srcStatus).zipWithIndex.foreach { case ((ssNext, ess), srcIdx) =>
          val gpWbWakeUpMatch = enqEntryEnqGpWbWakeUpMatchVec(enqIdx)(srcIdx)
          val fpWbWakeUpMatch = enqEntryEnqFpWbWakeUpMatchVec(enqIdx)(srcIdx)
          val vpWbWakeUpMatch = enqEntryEnqVpWbWakeUpMatchVec(enqIdx)(srcIdx)
          val gpWakeUpVec = gpWbWakeUpMatch.map(_ && ess.gpRen)
          val fpWakeUpVec = fpWbWakeUpMatch.map(_ && ess.fpRen)
          val vpWakeUpVec = vpWbWakeUpMatch.map(_ && ess.vpRen)
          val gpWakeUp = ess.gpRen && Cat(gpWbWakeUpMatch).orR
          val fpWakeUp = ess.fpRen && Cat(fpWbWakeUpMatch).orR
          val vpWakeUp = ess.vpRen && Cat(vpWbWakeUpMatch).orR

          val wakeUp = gpWakeUp || fpWakeUp || vpWakeUp

          ssNext.srcState := ess.srcState || wakeUp
          ssNext.bypassDelay := Mux1H(
            Seq(
              gpWakeUpVec zip in.wakeup.gpWbVec.map(_.delay),
              fpWakeUpVec zip in.wakeup.fpWbVec.map(_.delay),
              vpWakeUpVec zip in.wakeup.vpWbVec.map(_.delay),
            ).reduce(_ ++ _)
          )
          ssNext.bypassSource.idx := Mux1H(
            Seq(
              gpWakeUpVec.zipWithIndex,
              fpWakeUpVec.zipWithIndex,
              vpWakeUpVec.zipWithIndex,
            ).reduce(_ ++ _).map { case (wakeUpMath, exuIdx) => wakeUpMath -> exuIdx.U }
          )
        }

        (sNext.srcStatusV0 lazyZip e.srcStatusV0).foreach {
          case (ssNext, ess) =>
            val wakeUpMatch = enqEntryEnqV0WbWakeUpMatchVec.get(enqIdx)
            ssNext.srcState := ess.srcState || Cat(wakeUpMatch).orR
            ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.v0WbVec.get.map(_.delay))
        }

        (sNext.srcStatusVl lazyZip e.srcStatusVl).foreach {
          case (ssNext, ess) =>
            val wakeUpMatch = enqEntryEnqVlWbWakeUpMatchVec.get(enqIdx)
            ssNext.srcState := ess.srcState || Cat(wakeUpMatch).orR
            ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.vlWbVec.get.map(_.delay))
        }
      }
    }.otherwise {
      // !enq.valid
      entryNext.bits := entry.bits
      (entryNext.bits.status, entry.bits.status) match { case (sNext, s) =>
        (sNext.srcStatus lazyZip s.srcStatus).zipWithIndex.foreach { case ((ssNext, ss), srcIdx) =>

          val gpWbWakeUpMatch = enqEntryGpWbWakeUpMatchVec(enqIdx)(srcIdx)
          val fpWbWakeUpMatch = enqEntryFpWbWakeUpMatchVec(enqIdx)(srcIdx)
          val vpWbWakeUpMatch = enqEntryVpWbWakeUpMatchVec(enqIdx)(srcIdx)
          val gpWakeUpVec = gpWbWakeUpMatch.map(_ && ss.gpRen)
          val fpWakeUpVec = fpWbWakeUpMatch.map(_ && ss.fpRen)
          val vpWakeUpVec = vpWbWakeUpMatch.map(_ && ss.vpRen)
          val gpWakeUp = ss.gpRen && Cat(gpWbWakeUpMatch).orR
          val fpWakeUp = ss.fpRen && Cat(fpWbWakeUpMatch).orR
          val vpWakeUp = ss.vpRen && Cat(vpWbWakeUpMatch).orR

          val wakeUp = gpWakeUp || fpWakeUp || vpWakeUp

          ssNext.srcState := ss.srcState || wakeUp
          ssNext.bypassDelay := Mux1H(
            Seq(
              gpWakeUpVec zip in.wakeup.gpWbVec.map(_.delay),
              fpWakeUpVec zip in.wakeup.fpWbVec.map(_.delay),
              vpWakeUpVec zip in.wakeup.vpWbVec.map(_.delay),
            ).reduce(_ ++ _)
          )
          ssNext.bypassSource.idx := Mux1H(
            Seq(
              gpWakeUpVec.zipWithIndex,
              fpWakeUpVec.zipWithIndex,
              vpWakeUpVec.zipWithIndex,
            ).reduce(_ ++ _).map { case (wakeUpMath, exuIdx) => wakeUpMath -> exuIdx.U }
          )
        }

        (sNext.srcStatusV0 lazyZip s.srcStatusV0).foreach {
          case (ssNext, ss) =>
            val wakeUpMatch = enqEntryV0WbWakeUpMatchVec.get(enqIdx)
            ssNext.srcState := ss.srcState || Cat(wakeUpMatch).orR
            ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.v0WbVec.get.map(_.delay))
        }

        (sNext.srcStatusVl lazyZip s.srcStatusVl).foreach {
          case (ssNext, ss) =>
            val wakeUpMatch = enqEntryVlWbWakeUpMatchVec.get(enqIdx)
            ssNext.srcState := ss.srcState || Cat(wakeUpMatch).orR
            ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.vlWbVec.get.map(_.delay))
        }

        sNext.issued := MuxCase(
          s.issued,
          Seq(
            enqEntryDeqSel(enqIdx) -> true.B,
            enqEntryCancel(enqIdx) -> false.B,
          ),
        )
        when(entry.valid && s.issued) {
          sNext.issuedTimer := Mux(
            s.issuedTimer =/= IssuedTimer.maxValue,
            s.issuedTimer + 1.U,
            s.issuedTimer,
          )
        }.otherwise {
          sNext.issuedTimer := s.issuedTimer
        }
      }
    }
  }

  for (((entryNext: ValidIO[Entry], entry: ValidIO[Entry], enq: ValidIO[Entry]), fastIdx) <- (fastEntriesNext lazyZip fastEntries lazyZip fastEntryEnq).zipWithIndex) {
    entryNext.valid := Mux(
      enq.valid && fastEntryEnqNotFlush(fastIdx),
      true.B,
      Mux(
        fastEntryFlush(fastIdx) || fastEntrySuccess(fastIdx),
        false.B,
        entry.valid,
      )
    )

    when (enq.valid) {
      entryNext.bits.payload := enq.bits.payload
      entryNext.bits.status := enq.bits.status
      (entryNext.bits.status, enq.bits.status) match { case (sNext: Status, e: Status) =>
        (sNext.srcStatus lazyZip e.srcStatus).zipWithIndex.foreach {  case ((ssNext, ess), srcIdx) =>
          val gpWbWakeUpMatch = fastEntryEnqGpWbWakeUpMatchVec(fastIdx)(srcIdx)
          val fpWbWakeUpMatch = fastEntryEnqFpWbWakeUpMatchVec(fastIdx)(srcIdx)
          val vpWbWakeUpMatch = fastEntryEnqVpWbWakeUpMatchVec(fastIdx)(srcIdx)
          val gpWakeUpVec = gpWbWakeUpMatch.map(_ && ess.gpRen)
          val fpWakeUpVec = fpWbWakeUpMatch.map(_ && ess.fpRen)
          val vpWakeUpVec = vpWbWakeUpMatch.map(_ && ess.vpRen)
          val gpWakeUp = ess.gpRen && Cat(gpWbWakeUpMatch).orR
          val fpWakeUp = ess.fpRen && Cat(fpWbWakeUpMatch).orR
          val vpWakeUp = ess.vpRen && Cat(vpWbWakeUpMatch).orR

          val wakeUp = gpWakeUp || fpWakeUp || vpWakeUp

          ssNext.srcState := ess.srcState || wakeUp
          ssNext.bypassDelay := Mux1H(
            Seq(
              gpWakeUpVec zip in.wakeup.gpWbVec.map(_.delay),
              fpWakeUpVec zip in.wakeup.fpWbVec.map(_.delay),
              vpWakeUpVec zip in.wakeup.vpWbVec.map(_.delay),
            ).reduce(_ ++ _)
          )
          ssNext.bypassSource.idx := Mux1H(
            Seq(
              gpWakeUpVec.zipWithIndex,
              fpWakeUpVec.zipWithIndex,
              vpWakeUpVec.zipWithIndex,
            ).reduce(_ ++ _).map { case (wakeUpMath, exuIdx) => wakeUpMath -> exuIdx.U }
          )
        }

        (sNext.srcStatusV0 lazyZip e.srcStatusV0).foreach {
          case (ssNext, ess) =>
            val wakeUpMatch = fastEntryEnqV0WbWakeUpMatchVec.get(fastIdx)
            ssNext.srcState := ess.srcState || Cat(wakeUpMatch).orR
            ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.v0WbVec.get.map(_.delay))
        }

        (sNext.srcStatusVl lazyZip e.srcStatusVl).foreach {
          case (ssNext, ess) =>
            val wakeUpMatch = fastEntryEnqVlWbWakeUpMatchVec.get(fastIdx)
            ssNext.srcState := ess.srcState || Cat(wakeUpMatch).orR
            ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.vlWbVec.get.map(_.delay))
        }
      }
    }.otherwise {
      // !enq.valid
      entryNext.bits := entry.bits
      (entryNext.bits.status, entry.bits.status) match { case (sNext, s) =>
        (sNext.srcStatus lazyZip s.srcStatus).zipWithIndex.foreach { case ((ssNext, ss), srcIdx) =>

          val gpWbWakeUpMatch = fastEntryGpWbWakeUpMatchVec(fastIdx)(srcIdx)
          val fpWbWakeUpMatch = fastEntryFpWbWakeUpMatchVec(fastIdx)(srcIdx)
          val vpWbWakeUpMatch = fastEntryVpWbWakeUpMatchVec(fastIdx)(srcIdx)
          val gpWakeUpVec = gpWbWakeUpMatch.map(_ && ss.gpRen)
          val fpWakeUpVec = fpWbWakeUpMatch.map(_ && ss.fpRen)
          val vpWakeUpVec = vpWbWakeUpMatch.map(_ && ss.vpRen)
          val gpWakeUp = ss.gpRen && Cat(gpWbWakeUpMatch).orR
          val fpWakeUp = ss.fpRen && Cat(fpWbWakeUpMatch).orR
          val vpWakeUp = ss.vpRen && Cat(vpWbWakeUpMatch).orR

          val wakeUp = gpWakeUp || fpWakeUp || vpWakeUp

          ssNext.srcState := ss.srcState || wakeUp
          ssNext.bypassDelay := Mux1H(
            Seq(
              gpWakeUpVec zip in.wakeup.gpWbVec.map(_.delay),
              fpWakeUpVec zip in.wakeup.fpWbVec.map(_.delay),
              vpWakeUpVec zip in.wakeup.vpWbVec.map(_.delay),
            ).reduce(_ ++ _)
          )
          ssNext.bypassSource.idx := Mux1H(
            Seq(
              gpWakeUpVec.zipWithIndex,
              fpWakeUpVec.zipWithIndex,
              vpWakeUpVec.zipWithIndex,
            ).reduce(_ ++ _).map { case (wakeUpMath, exuIdx) => wakeUpMath -> exuIdx.U }
          )
        }

        (sNext.srcStatusV0 lazyZip s.srcStatusV0).foreach {
          case (ssNext, ss) =>
            val wakeUpMatch = fastEntryV0WbWakeUpMatchVec.get(fastIdx)
            ssNext.srcState := ss.srcState || Cat(wakeUpMatch).orR
            ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.v0WbVec.get.map(_.delay))
        }

        (sNext.srcStatusVl lazyZip s.srcStatusVl).foreach {
          case (ssNext, ss) =>
            val wakeUpMatch = fastEntryVlWbWakeUpMatchVec.get(fastIdx)
            ssNext.srcState := ss.srcState || Cat(wakeUpMatch).orR
            ssNext.bypassDelay := Mux1H(wakeUpMatch, in.wakeup.vlWbVec.get.map(_.delay))
        }

        sNext.issued := MuxCase(
          s.issued,
          Seq(
            fastEntryDeqSel(fastIdx) -> true.B,
            fastEntryCancel(fastIdx) -> false.B,
          ),
        )
        when(entry.valid && s.issued) {
          sNext.issuedTimer := Mux(
            s.issuedTimer =/= IssuedTimer.maxValue,
            s.issuedTimer + 1.U,
            s.issuedTimer,
          )
        }.otherwise {
          sNext.issuedTimer := s.issuedTimer
        }
      }
    }
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
        enqEntryOldest(deqIdx).valid,
      )
    )
  }

  for ((sel, i) <- entryDeqSel.zipWithIndex) {
    sel := VecInit(deqSelOHVec.map(_(i))).asUInt.orR
  }

  private val deqEntries: Vec[Entry] = VecInit(deqSelOHVec.map(
    deqSelOH => Mux1H(deqSelOH, (enqEntries ++ fastEntries).map(_.bits))
  ))

  for ((deq: ValidIO[Deq], valid, deqEty) <- out.deq lazyZip deqValidVec lazyZip deqEntries) {
    deq.valid := valid
    deq.bits.fromEntry(deqEty)
  }

  out.canAccept := PopCount(fastEntries.map(!_.valid)) >= 2.U
  // Todo: optimize it
  out.validNum := PopCount(entries.map(_.valid))
}

object VecIssueQueue {
  class LazyMod (implicit p: Parameters, val param: IssueParam) extends LazyModule with HasXSParameter {
    override def shouldBeInlined: Boolean = false

    lazy val module = new VecIssueQueue(this)
  }

  val IntCrossRegionVecCycle = 1
  val FpCrossRegionVecCycle = 1

  class In(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val flush = ValidIO(new Redirect)
    val enq = Vec(param.numEnq, ValidIO(new Enq))
    val resps = new InResp
    val wakeup = new Bundle {
      val gpWbVec = Vec(backendParams.getIntRfWriteSize, new WakeUpBundle(IntPhyRegIdxWidth, IntData()))
      val fpWbVec = Vec(backendParams.getFpRfWriteSize,  new WakeUpBundle(FpPhyRegIdxWidth, FpData()))
      val vpWbVec = Vec(backendParams.getVfRfWriteSize,  new WakeUpBundle(VfPhyRegIdxWidth, VecData()))
      val v0WbVec = Option.when(param.readV0Rf)(Vec(backendParams.getV0RfWriteSize, new WakeUpBundle(V0PhyRegIdxWidth, V0Data())))
      val vlWbVec = Option.when(param.readVlRf)(Vec(backendParams.getVlRfWriteSize, new WakeUpBundle(VlPhyRegIdxWidth, VlData())))
      // Todo: iq wakeup
    }
  }

  class Out(implicit p: Parameters, param: IssueParam) extends XSBundle {
    val deq: MixedVec[ValidIO[Deq]] = param.genIssueBundle(ValidIO(_))

    val canAccept: Bool = Bool()

    val validNum: UInt = UInt(log2Ceil(param.numEntry + 1).W)
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
        s.bypassDelay := 0.U
        s.bypassSource.idx := 0.U
      }
      this.srcStatusV0.foreach { case s =>
        s.ren := enq.v0Ren
        s.psrc := enq.psrcV0
        s.srcState := enq.srcStateV0
        s.bypassDelay := 0.U
      }
      this.srcStatusVl.foreach { case s =>
        s.ren := enq.vlRen
        s.psrc := enq.psrcVl
        s.srcState := enq.srcStateVl
        s.bypassDelay := 0.U
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
    // bypass data with 0 cycle delay
    val delay0 = "b01".U(width.W)
    // bypass data with 1 cycle delay
    val delay1 = "b10".U(width.W)
    // read from regfile
    val delay2 = "b11".U(width.W)
  }

  class BypassSource(implicit p: Parameters, param: IssueParam) extends XSBundle {
    // Todo: make it configurable
    val idx = UInt(4.W)
  }

  class WakeUpBundle(pregIdxWidth: Int, dataConfig: DataConfig)(implicit p: Parameters) extends XSBundle {
    val wen = Bool()
    val pdest = UInt(pregIdxWidth.W)
    val delay = BypassDelay()
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

  object EnqPolicy {
    def apply(canEnq: UInt, numEnq: Int): Vec[ValidIO[UInt]] = {
      val enqPolicy = Module(new EnqPolicy(canEnq.getWidth, numEnq))
      enqPolicy.canEnq := canEnq
      enqPolicy.enqSelOHVec
    }
  }
}
