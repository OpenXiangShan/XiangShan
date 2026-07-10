package xiangshan.backend.vector.vagq

import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.mem._
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.FuType

trait HasVAGQHelper extends HasCircularQueuePtrHelper { this: HasVAGQParameters =>
  protected def entryAt(entries: Vec[VAGQEntry], idx: UInt): VAGQEntry = {
    Mux1H((0 until vagqSize).map(i => (idx === i.U(vagqEntryIdxWidth.W)) -> entries(i)))
  }

  protected def idxValid(idx: UInt): Bool = idx < vagqSize.U

  protected def uopByteRangeLen(totalElem: UInt, deew: UInt, uopIdx: UInt): UInt = {
    val byteCountWidth = log2Up(VLEN) + 1 + 3
    val totalBytes = (Cat(0.U(3.W), totalElem) << deew)(byteCountWidth - 1, 0)
    val byteQuot = totalBytes(byteCountWidth - 1, vagqFlowByteWidth)
    val byteRem = totalBytes(vagqFlowByteWidth - 1, 0)

    Mux1H(Seq(
      (uopIdx < byteQuot)   -> vagqFlowBytes.U(vagqUvlByteWidth.W),
      (uopIdx === byteQuot) -> byteRem,
      (uopIdx > byteQuot)   -> 0.U(vagqUvlByteWidth.W)
    ))
  }

  protected def elemMaskBit(byteIdx: Int, deew: UInt, v0Mask: UInt): Bool = {
    MuxLookup(deew, v0Mask(byteIdx))(Seq(
      0.U -> v0Mask(byteIdx),
      1.U -> v0Mask(byteIdx / 2),
      2.U -> v0Mask(byteIdx / 4),
      3.U -> v0Mask(byteIdx / 8),
    ))
  }

  protected def idxHitSeq(idx: UInt, numEntries: Int): Seq[Bool] = {
    (0 until numEntries).map(i => idx === i.U)
  }

  protected def respMatchesEntry(resp: VAGQResp, entries: Vec[CtrlInput], numEntries: Int): Bool = {
    val hit = idxHitSeq(resp.entryIdx, numEntries)
    VecInit(hit).asUInt.orR && Mux1H(hit, entries.map(x => x.entry.valid && x.entry.robIdx === resp.robIdx))
  }

  protected def entryAlive(entry: VAGQEntryMeta, redirect: ValidIO[Redirect]): Bool = {
    entry.valid && !entry.robIdx.needFlush(redirect)
  }

  protected def prefixMask(limit: UInt): UInt = {
    VecInit((0 until vagqFlowBytes).map(i => i.U < limit)).asUInt
  }

  protected def lowBit(mask: UInt): UInt = PriorityEncoder(mask)

  protected def highBit(mask: UInt): UInt = (vagqFlowBytes - 1).U - PriorityEncoder(Reverse(mask))

  protected def bitMask(idx: UInt): UInt = UIntToOH(idx, vagqFlowBytes)

  protected def storeFlowData(data: UInt, elemIdx: UInt, alignedType: UInt): UInt = {
    genVWdata(genVSData(data, elemIdx, alignedType), alignedType)
  }

  protected def byteMaskToEntryMask(mask: UInt, deew: UInt): UInt = {
    VecInit((0 until vagqFlowBytes).map { elem =>
      VecInit((0 until vagqFlowBytes).map { byte =>
        mask(byte) && ((byte.U(vagqFlowByteWidth.W) >> deew) === elem.U(vagqFlowByteWidth.W))
      }).asUInt.orR
    }).asUInt
  }

  protected def mergeEntryAt(entries: Vec[CtrlInput], idx: UInt, numEntries: Int): CtrlInput = {
    Mux1H(idxHitSeq(idx, numEntries), entries)
  }

  protected def oldestEntryOH(valids: Seq[Bool], entries: Vec[CtrlInput], numEntries: Int): UInt = {
    require(valids.length == numEntries)
    VecInit((0 until numEntries).map { i =>
      val thisEntry = entries(i).entry
      val olderThanAll = (0 until numEntries).map { j =>
        if (i == j) {
          true.B
        } else {
          val thatEntry = entries(j).entry
          val sameRob = thatEntry.robIdx === thisEntry.robIdx
          val thisOlderSameRob = sameRob && (
            thisEntry.uopIdx < thatEntry.uopIdx ||
              (thisEntry.uopIdx === thatEntry.uopIdx && (i < j).B)
          )
          !valids(j) || isAfter(thatEntry.robIdx, thisEntry.robIdx) || thisOlderSameRob
        }
      }.reduce(_ && _)
      valids(i) && olderThanAll
    }).asUInt
  }

  protected def mergeBytes(oldData: UInt, newData: UInt, mask: UInt): UInt = {
    VecInit((0 until vagqFlowBytes).map(i =>
      Mux(mask(i), newData(8 * (i + 1) - 1, 8 * i), oldData(8 * (i + 1) - 1, 8 * i))
    )).asUInt
  }

  protected def elemNum(deew: UInt): UInt = {
    MuxLookup(deew, 4.U(3.W))(Seq(
      0.U -> 4.U,
      1.U -> 3.U,
      2.U -> 2.U,
      3.U -> 1.U
    ))
  }

  protected def faultVstart(entry: VAGQEntryMeta): UInt = {
    val elemIdx = entry.faultElemIdx >> entry.deew
    ((entry.uopIdx << elemNum(entry.deew)) + elemIdx)(VAGQConstants.FaultVstartWidth - 1, 0)
  }

  protected def enterSplit(entry: VAGQEntryMeta): Unit = {
    entry.state   := VAGQEntryState.split
    entry.reqSent := 0.U
    entry.reqAck  := 0.U
    entry.exceptionNumber := 0.U
    entry.faultElemIdx    := 0.U
  }

  protected def initPending(entry: VAGQEntryMeta, stateNext: UInt): Unit = {
    entry.valid := true.B
    entry.state := stateNext
    entry.reqSent := 0.U
    entry.reqAck  := 0.U
    entry.exceptionNumber := 0.U
    entry.faultElemIdx    := 0.U
  }

  protected def fitUInt(source: UInt, width: Int): UInt = {
    if (source.getWidth >= width) source(width - 1, 0) else ZeroExt(source, width)
  }

  protected def isVagqAddrUop(source: NewExuInput): Bool = {
    val fuOpType = source.ctrl.fuOpType
    val isVagqLoad = VlduType.isVecLd(fuOpType) &&
      (VlduType.isStrided(fuOpType) || VlduType.isIndexed(fuOpType))
    val isVagqStore = VstuType.isVecSt(fuOpType) &&
      (VstuType.isStrided(fuOpType) || VstuType.isIndexed(fuOpType))
    isVagqLoad || isVagqStore
  }

  protected def isVagqStrideDataUop(fuType: UInt, fuOpType: UInt): Bool = {
    val isVLoad = FuType.isLoad(fuType)
    val isVStore = FuType.isStore(fuType)
    val isStride = LSUOpType.isStrided(fuOpType)
    val isVecMemOp = LSUOpType.isVecMemOp(fuOpType)
    (isVLoad || isVStore) && isStride && isVecMemOp
  }

  protected def isVagqStrideDataUop(source: ExuInput): Bool = {
    isVagqStrideDataUop(source.fuType, source.fuOpType)
  }

  protected def isVagqIndexedDataUop(fuType: UInt, fuOpType: UInt): Bool = {
    val isVLoad = FuType.isLoad(fuType)
    val isVStore = FuType.isStore(fuType)
    val isIndexed = LSUOpType.isIndexed(fuOpType)
    val isVecMemOp = LSUOpType.isVecMemOp(fuOpType)
    (isVLoad || isVStore) && isIndexed && isVecMemOp
  }

  protected def isVagqIndexedDataUop(source: xiangshan.backend.vector.Exu.InUop): Bool = {
    isVagqIndexedDataUop(source.ctrl.fuType, source.ctrl.opcode)
  }

  protected def buildVagqStrideDataUop(source: ExuInput): VAGQDataSideUop = {
    val data = Wire(new VAGQDataSideUop)
    data := 0.U.asTypeOf(data)
    data.entryIdx := 0.U // TODO: replace with VOQ-provided entryIdx together with addr side.
    data.robIdx := source.robIdx
    data.op2Data := fitUInt(source.src(0), VLEN)
    data.psrc2 := 0.U // Todo
    data
  }

  protected def buildVagqIndexedDataUop(source: xiangshan.backend.vector.Exu.InUop): VAGQDataSideUop = {
    val data = Wire(new VAGQDataSideUop)
    data := 0.U.asTypeOf(data)
    data.entryIdx := 0.U // TODO: replace with VOQ-provided entryIdx together with addr side.
    data.robIdx := source.ctrl.robIdx
    data.op2Data := fitUInt(source.data.src(0), VLEN)
    data.psrc2 := 0.U // Todo
    data
  }

  protected def buildVagqAddrUop(source: NewExuInput): VAGQAddrSideUop = {
    val addr = Wire(new VAGQAddrSideUop)
    val vpu = source.ctrl.vpu.get
    val fuOpType = source.ctrl.fuOpType
    val isLoad = VlduType.isVecLd(fuOpType)
    val isStride = VlduType.isStrided(fuOpType) || VstuType.isStrided(fuOpType)
    val isOrdered = fuOpType(6, 5) === "b11".U
    val deew = Mux(isStride, vpu.veew(1, 0), vpu.vsew(1, 0))

    addr := 0.U.asTypeOf(addr)
    addr.meta.pc := source.data.pc.getOrElse(0.U.asTypeOf(addr.meta.pc))
    addr.meta.isRVC := source.ctrl.isRVC.getOrElse(false.B)
    addr.meta.ftqPtr := source.ctrl.ftqIdx.getOrElse(0.U.asTypeOf(addr.meta.ftqPtr))
    addr.meta.ftqOffset := source.ctrl.ftqOffset.getOrElse(0.U.asTypeOf(addr.meta.ftqOffset))
    addr.meta.lqIdx := source.lqIdx.getOrElse(0.U.asTypeOf(addr.meta.lqIdx))
    addr.meta.sqIdx := source.sqIdx.getOrElse(0.U.asTypeOf(addr.meta.sqIdx))
    addr.meta.trigger := TriggerAction.None
    addr.meta.perfDebugInfo := source.perfDebugInfo.getOrElse(0.U.asTypeOf(addr.meta.perfDebugInfo))
    addr.meta.debug_seqNum := source.debug_seqNum.getOrElse(0.U.asTypeOf(addr.meta.debug_seqNum))
    addr.entryIdx := 0.U // TODO
    addr.uopType := Mux(
      isLoad,
      Mux(isStride, VAGQUopType.strideLoad, Mux(isOrdered, VAGQUopType.indexedOrderedLoad, VAGQUopType.indexedUnorderedLoad)),
      Mux(isStride, VAGQUopType.strideStore, Mux(isOrdered, VAGQUopType.indexedOrderedStore, VAGQUopType.indexedUnorderedStore))
    )
    addr.robIdx := source.robIdx
    addr.pdest := fitUInt(source.toRF.pdest, addr.pdest.getWidth)
    addr.baseAddr := source.data.src(0)(XLEN - 1, 0)
    addr.uvlByte := uopByteRangeLen(source.data.vl.get, deew, vpu.vuopIdx)
    addr.vstart := fitUInt(vpu.vstart, addr.vstart.getWidth)
    addr.useVstart := vpu.vstart =/= 0.U
    addr.vm := vpu.vm
    addr.v0Mask := source.data.v0.get(VAGQConstants.FlowBytes - 1, 0)
    addr.deew := deew
    addr.ieew := vpu.veew(1, 0)
    addr.vma := vpu.vma
    addr.vta := vpu.vta
    addr.uopIdx := vpu.vuopIdx
    addr.nf := fitUInt(vpu.nf, addr.nf.getWidth)
    addr
  }
}

object VAGQWritebackConnect {
  def toRob(sink: WriteBackRobBundle, source: VAGQWritebackReq): Unit = {
    sink := 0.U.asTypeOf(sink)
    sink.robIdx := source.robIdx
    sink.exceptionVec.zeroInit()
    sink.exceptionVec.indices.foreach { num =>
      sink.exceptionVec(num) := source.exception && source.exceptionNumber === num.U
    }
    sink.trigger.foreach(_ := source.meta.trigger)
    sink.lqIdx.foreach(_ := source.meta.lqIdx)
    sink.sqIdx.foreach(_ := source.meta.sqIdx)
    sink.entryIdx.foreach(_ := source.entryIdx)
    sink.vls.foreach { vls =>
      vls := 0.U.asTypeOf(vls)
      vls.vpu.vstart := source.faultVstart
      vls.vpu.vuopIdx := source.uopIdx
      vls.vpu.nf := source.nf
      vls.vpu.vsew := 0.U
      vls.vpu.veew := source.deew
      vls.vpu.vlmul := 0.U
      vls.isIndexed := VAGQUopType.isIndexed(source.uopType)
      vls.isStrided := VAGQUopType.isStride(source.uopType)
      vls.isWhole := false.B
      vls.isVecLoad := VAGQUopType.isLoad(source.uopType)
      vls.isVlm := false.B
      vls.isMasked := false.B
    }
    sink.debug.isMMIO := false.B
    sink.debug.isNCIO := false.B
    sink.debug.isPerfCnt := false.B
    sink.debug.paddr := 0.U
    sink.debug.vaddr := 0.U
    sink.perfDebugInfo.foreach(_ := source.meta.perfDebugInfo)
    sink.debug_seqNum.foreach(_ := source.meta.debug_seqNum)
  }
}
