package xiangshan.backend

import chisel3.util._
import chisel3._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.fu.vector.Bundles._

class VecExcpDataMergeModule(implicit p: Parameters) extends XSModule {
  private val MaxLMUL = 8
  private val VdIdxInGroupWidth = log2Ceil(MaxLMUL) // hold 0~7
  private val minElemLen = 8 // 8 bits
  private val maxElemNumPerVreg = VLEN / minElemLen
  private val tailZeroBit = log2Ceil(maxElemNumPerVreg) // 16 -> 4

  val i = IO(Input(new Bundle {
    val fromExceptionGen = ValidIO(new VecExcpInfo)
    val fromRab = new RabToVecExcpMod
    val fromRat = new RatToVecExcpMod
    val fromVprf = new VprfToExcpMod(maxMergeNumPerCycle * 2)
  }))
  val o = IO(Output(new Bundle {
    val toVPRF = new ExcpModToVprf(maxMergeNumPerCycle * 2, maxMergeNumPerCycle)
    val status = new Bundle {
      val busy = Bool()
    }
  }))

  private val oldPregVecFromRat: Vec[ValidIO[UInt]] = Wire(Vec(RabCommitWidth, ValidIO(UInt(VfPhyRegIdxWidth.W))))
  oldPregVecFromRat.zipWithIndex.foreach { case (oldPreg: ValidIO[UInt], idx) =>
    val vecOldVd = i.fromRat.vecOldVdPdest(idx)
    val v0OldVd  = i.fromRat.v0OldVdPdest(idx)
    oldPreg.valid := (vecOldVd.valid || v0OldVd.valid)
    oldPreg.bits := Mux1H(Seq(
      vecOldVd.valid -> vecOldVd.bits,
      v0OldVd.valid -> v0OldVd.bits,
    ))
  }

  private val lregNewPregVecFromRab = WireInit(i.fromRab.logicPhyRegMap)

  private val preMergedOldVd = WireInit(VecInit(i.fromVprf.rdata.take(maxMergeNumPerCycle).map(_.bits.asTypeOf(new VecElemData(VLEN)))))
  private val preMergedNewVd = WireInit(VecInit(i.fromVprf.rdata.drop(maxMergeNumPerCycle).map(_.bits.asTypeOf(new VecElemData(VLEN)))))
  private val preMoveOldVd   = WireInit(VecInit(i.fromVprf.rdata.map(_.bits.asTypeOf(new VecElemData(VLEN)))))

  private val sNoExcp_vecExcpInfo = WireInit(i.fromExceptionGen)
  private val sNoExcp_vemul = sNoExcp_vecExcpInfo.bits.vlmul + sNoExcp_vecExcpInfo.bits.veew - sNoExcp_vecExcpInfo.bits.vsew
  // data vemul
  private val sNoExcp_dvemul = Mux(
    sNoExcp_vecExcpInfo.bits.isIndexed,
    sNoExcp_vecExcpInfo.bits.vlmul,
    sNoExcp_vemul,
  )
  // index vemul
  private val sNoExcp_ivemul = WireInit(VLmul(), sNoExcp_vemul)
  dontTouch(sNoExcp_vemul)
  dontTouch(sNoExcp_dvemul)
  dontTouch(sNoExcp_ivemul)
  private val sNoExcp_dvemulNoLessThanM1 = VLmul.makeNoLessThanM1(sNoExcp_dvemul).take(2)
  private val sNoExcp_ivemulNoLessThanM1 = VLmul.makeNoLessThanM1(sNoExcp_ivemul).take(2)

  // if ivemul - dvemul = idx
  private val sNoExcp_vemul_i_d = VecInit.tabulate(4)(idx =>
    sNoExcp_ivemulNoLessThanM1 === (sNoExcp_dvemulNoLessThanM1 +& idx.U) ||
    (idx == 0).B && (sNoExcp_ivemulNoLessThanM1 < sNoExcp_dvemulNoLessThanM1)
  )
  private val sNoExcp_nonSegIndexed = sNoExcp_vecExcpInfo.bits.isIndexed && sNoExcp_vecExcpInfo.bits.nf === 0.U

  private val commitNeeded = RegInit(VecInit.fill(MaxLMUL)(false.B))
  private val rabCommitted = RegInit(VecInit.fill(MaxLMUL)(false.B))
  private val ratCommitted = RegInit(VecInit.fill(MaxLMUL)(false.B))
  private val hasReadRf    = RegInit(VecInit.fill(MaxLMUL)(false.B))

  private val regMaps = Reg(Vec(MaxLMUL, new LogicPhyRegMap))

  private val currentIdx = RegInit(0.U(log2Up(8 + 1).W))
  private val currentIdxVec = (0 until maxMergeNumPerCycle).map(idx => currentIdx + idx.U)

  private val mergedVd = Reg(Vec(maxMergeNumPerCycle, new VecElemData(VLEN)))

  private val sNoExcp_eewOH = SewOH.convertFromVSew(sNoExcp_vecExcpInfo.bits.veew)
  private val sNoExcp_sewOH = SewOH.convertFromVSew(sNoExcp_vecExcpInfo.bits.vsew)
  private val sNoExcp_deewOH = Mux(
    sNoExcp_vecExcpInfo.bits.isIndexed,
    sNoExcp_sewOH,
    sNoExcp_eewOH,
  )
  private val sNoExcp_voffset = Module(new GetE8OffsetInVreg(VLEN))(sNoExcp_deewOH, sNoExcp_vecExcpInfo.bits.vstart)
  private val sNoExcp_idxRangeVec: Vec[HWRange] = 
    Module(new NfMappedElemIdx(VLEN))(
      Mux(!sNoExcp_vecExcpInfo.bits.isWhole, sNoExcp_vecExcpInfo.bits.nf, 0.U), 
      sNoExcp_deewOH
    )
  private val sNoExcp_vstartIsAligned: Bool = Mux(!sNoExcp_vecExcpInfo.bits.isVlm, sNoExcp_voffset === 0.U, false.B)

  private val sNoExcp_inRangeVec: Vec[Bool] = VecInit((0 until 8).map(idx =>
    if (idx == 0) {
      sNoExcp_vecExcpInfo.bits.isVlm ||
      sNoExcp_idxRangeVec(idx).inRange (sNoExcp_vecExcpInfo.bits.vstart)
    } else {
      !sNoExcp_vecExcpInfo.bits.isVlm &&
      sNoExcp_idxRangeVec(idx).inRange (sNoExcp_vecExcpInfo.bits.vstart)
    }
  ))
  // The last no exception vdIdx, hold 0~7.
  // No need to hold 8, since if all vd are new, there is no exception occuration.
  private val sNoExcp_useNewVdUntil: UInt = PriorityEncoder(sNoExcp_inRangeVec)
  // The last exception vdIdx, hold 0~8.
  // Need to hold 8.
  private val sNoExcp_needMergeUntil: UInt = sNoExcp_useNewVdUntil +
    Mux(!sNoExcp_vecExcpInfo.bits.isWhole, sNoExcp_vecExcpInfo.bits.nf, 0.U) +&
    1.U
  // the max vd idx need to write
  private val sNoExcp_maxVdIdx = Mux(
    sNoExcp_vecExcpInfo.valid,
    MuxCase(
      default = ((sNoExcp_vecExcpInfo.bits.nf +& 1.U) << sNoExcp_dvemulNoLessThanM1).asUInt,
      Seq(
        sNoExcp_vecExcpInfo.bits.isVlm -> 1.U,
        sNoExcp_vecExcpInfo.bits.isWhole -> (sNoExcp_vecExcpInfo.bits.nf +& 1.U),
      )
    ),
    0.U
  )

  private val sNoExcp_handleUntil = sNoExcp_maxVdIdx(3, 0) // [1, 8]
  // strided vector load need 2 uop to move data, so skip these reg maps
  private val sNoExcp_writeOffset = Mux(sNoExcp_vecExcpInfo.bits.isStride, 2.U, 1.U)

  private val sWaitRab_vecExcpInfo     = RegNextWithEnable(sNoExcp_vecExcpInfo)

  // At the beginning of waitRab,
  // when not offset not aligned, currentIdx = useNewVdUntil <= needMergeUntil <= handleUntil
  // otherwise, currentIdx = needMergeUntil <= handleUntil
  private val sWaitRab_useNewVdUntil   = RegEnable(sNoExcp_useNewVdUntil, sNoExcp_vecExcpInfo.valid)
  private val sWaitRab_needMergeUntil  = RegEnable(sNoExcp_needMergeUntil, sNoExcp_vecExcpInfo.valid)
  private val sWaitRab_e8offset        = RegEnable(
    Mux1H((0 until 4).map(idx => sNoExcp_deewOH(idx) -> ZeroExt(sNoExcp_voffset(tailZeroBit - 1, 0), tailZeroBit))),
    sNoExcp_vecExcpInfo.valid
  )
  private val sWaitRab_idxRangeVec     = RegEnable(sNoExcp_idxRangeVec, sNoExcp_vecExcpInfo.valid)
  private val sWaitRab_vstartIsAligned = RegEnable(sNoExcp_vstartIsAligned, sNoExcp_vecExcpInfo.valid)
  private val sWaitRab_handleUntil     = RegEnable(sNoExcp_handleUntil, sNoExcp_vecExcpInfo.valid)

  private val sWaitRab_nonSegIndexed   = RegEnable(sNoExcp_nonSegIndexed, sNoExcp_vecExcpInfo.valid)
  private val sWaitRab_vemul_i_d       = RegEnable(sNoExcp_vemul_i_d, sNoExcp_vecExcpInfo.valid)
  private val sWaitRab_dvemulNoLessThanM1 = RegEnable(sNoExcp_dvemulNoLessThanM1, sNoExcp_vecExcpInfo.valid)

  private val sWaitRab_rabWriteOffset = Reg(UInt(4.W)) // [1,10]
  private val sWaitRab_ratWriteOffset = Reg(UInt(4.W)) // [1,10]

  // segShuffledRegIdxTable(nf)(dvemul)(vdIdx)
  private val segShuffledRegIdxTable: Seq[Seq[Seq[Int]]] = Seq.tabulate(8, 4) {
    case (nf, dvemul) =>
      val nField = nf + 1     // 1~8
      val dEMUL = 1 << dvemul // 1, 2, 4, 8
      if (nField == 2 && dEMUL == 2) {
        Seq(0, 2, 1, 3, 0, 0, 0, 0)
      }
      else if (nField == 2 && dEMUL == 4) {
        Seq(0, 4, 1, 5, 2, 6, 3, 7)
      }
      else if (nField == 3 && dEMUL == 2) {
        Seq(0, 2, 4, 1, 3, 5, 0, 0)
      }
      else if (nField == 4 && dEMUL == 2) {
        Seq(0, 2, 4, 6, 1, 3, 5, 7)
      }
      else {
        Seq(0, 1, 2, 3, 4, 5, 6, 7)
      }
  }
  private val segRegTableHW: Vec[Vec[Vec[UInt]]] = WireInit(VecInit.tabulate(8, 4) {
    case (nf, dvemul) => VecInit(segShuffledRegIdxTable(nf)(dvemul).map(_.U(VdIdxInGroupWidth.W)))
  })

  // when nonSegIndexed load, iemul/demul = 1 << 2, vdLoc will be mapped as (0, 1, 2, 3, ...) -> (0, 4, ...)
  private val oldVdLocVec: Vec[UInt] = VecInit(currentIdxVec.map(idx =>
    Mux(
      sWaitRab_nonSegIndexed,
      Mux1H(sWaitRab_vemul_i_d.zipWithIndex.map { case (i_d_n, ii) => i_d_n -> (idx << ii).asUInt }),
      Mux(
        sWaitRab_vecExcpInfo.bits.isWhole,
        idx,
        segRegTableHW(sWaitRab_vecExcpInfo.bits.nf)(sWaitRab_dvemulNoLessThanM1)(idx),
      )
    ).take(VdIdxInGroupWidth)
  ))

  // when nonSegIndexed load, iemul/demul = 1 << 2, vdLoc will be mapped as (0, 1, 2, 3, ...) -> (3, 7, ...)
  private val newVdLocVec = VecInit(currentIdxVec.map(idx =>
    Mux(
      sWaitRab_nonSegIndexed,
      Mux1H(sWaitRab_vemul_i_d.zipWithIndex.map { case (i_d_n, ii) => i_d_n -> ((idx << ii).asUInt | ((1 << ii) - 1).U) }),
      Mux(
        sWaitRab_vecExcpInfo.bits.isWhole,
        idx,
        segRegTableHW(sWaitRab_vecExcpInfo.bits.nf)(sWaitRab_dvemulNoLessThanM1)(idx),
      )
    ).take(VdIdxInGroupWidth)
  ))

  dontTouch(oldVdLocVec)
  dontTouch(newVdLocVec)

  private object State extends ChiselEnum {
    val noExcp  = Value
    val waitRab = Value
    val mergeVd = Value
    val mvOldVd = Value
    val finish  = Value
  }

  private val state: State.Type = RegInit(State.noExcp)
  private val stateNext = WireInit(state)
  state := stateNext

  private val collectedAllRegMap = Wire(Bool())
  private val mergeFinished = currentIdx >= sWaitRab_needMergeUntil
  private val mvFinished = currentIdx >= sWaitRab_handleUntil

  // get lreg and new preg, the last mapped newPdest
  private val filteredRabCommitedVec: Vec[Vec[Bool]] = WireInit(VecInit.tabulate(4, MaxLMUL) { case (i_d_n, vdIdx) =>
    val vdLoc = ((vdIdx + 1) << i_d_n) - 1
    rabCommitted(if (vdLoc >= MaxLMUL) 0 else vdLoc)
  })
  // get old preg, the first mapped oldPdest
  private val filteredRatCommitedVec: Vec[Vec[Bool]] = WireInit(VecInit.tabulate(4, MaxLMUL) { case (i_d_n, vdIdx) =>
    val vdLoc = vdIdx << i_d_n
    ratCommitted(if (vdLoc >= MaxLMUL) 0 else vdLoc)
  })

  private val filteredRabCommited = Wire(Vec(MaxLMUL, Bool()))
  private val filteredRatCommited = Wire(Vec(MaxLMUL, Bool()))
  when (sWaitRab_nonSegIndexed) {
    filteredRabCommited := Mux1H(sWaitRab_vemul_i_d, filteredRabCommitedVec)
    filteredRatCommited := Mux1H(sWaitRab_vemul_i_d, filteredRatCommitedVec)
  }.otherwise {
    // No need to shuffle, since the vdIdx always compressed towards zero and left tail unused.
    filteredRabCommited := rabCommitted
    filteredRatCommited := ratCommitted
  }

  // 1. no need commit
  // 2. need commit and both rab and rat committed
  collectedAllRegMap := ((~commitNeeded.asUInt).asUInt | (commitNeeded.asUInt & filteredRabCommited.asUInt & filteredRatCommited.asUInt)).andR

  switch(state) {
    is(State.noExcp) {
      when (i.fromExceptionGen.valid) {
        stateNext := State.waitRab
      }
    }
    is(State.waitRab) {
      when (collectedAllRegMap) {
        stateNext := State.mergeVd
        currentIdx := sWaitRab_useNewVdUntil
      }
    }
    is(State.mergeVd) {
      when (mvFinished) {
        stateNext := State.finish
      }.elsewhen (mergeFinished) {
        stateNext := State.mvOldVd
      }
      when(o.toVPRF.w.head.valid) {
        currentIdx := currentIdx + PopCount(o.toVPRF.w.map(_.valid))
      }
    }
    is(State.mvOldVd) {
      when (mvFinished) {
        stateNext := State.finish
      }
      when(o.toVPRF.w.head.valid) {
        currentIdx := currentIdx + PopCount(o.toVPRF.w.map(_.valid))
      }
    }
    is(State.finish) {
      stateNext := State.noExcp
      currentIdx := 0.U
    }
  }

  private val regWriteFromRabVec: Vec[ValidIO[RegWriteFromRab]] = i.fromRab.logicPhyRegMap
  private val regWriteFromRatVec: Vec[ValidIO[UInt]] = oldPregVecFromRat

  val mergedVdWData: Vec[VecE8Vec] = Wire(Vec(maxMergeNumPerCycle, new VecE8Vec(VLEN)))
  mergedVdWData.zipWithIndex.foreach { case (vd, vIdx) =>
    vd.data.zipWithIndex.foreach { case (vde, eIdx) =>
      vde := Mux(
        state === State.mergeVd,
        Mux(
          eIdx.U >= sWaitRab_e8offset,
          preMergedOldVd(vIdx).e8Vec(eIdx),
          preMergedNewVd(vIdx).e8Vec(eIdx),
        ),
        preMoveOldVd(vIdx).e8Vec(eIdx),
      )
    }
  }

  private val hasRabWrite = regWriteFromRabVec.head.valid
  private val hasRatWrite = regWriteFromRatVec.head.valid
  require(
    2 * RabCommitWidth >= (MaxLMUL + 2),
    "Cannot receive all 10 reg maps from RAB and RAT in two cycles. " +
      "This module should be rewrited to support more than 2 cycles receiving"
  )

  switch (state) {
    is (State.noExcp) {
      when (stateNext === State.waitRab) {
        sWaitRab_rabWriteOffset := 0.U
        sWaitRab_ratWriteOffset := 0.U
        commitNeeded.zipWithIndex.foreach { case (needed, idx) =>
          needed := sNoExcp_maxVdIdx > idx.U
        }
      }
    }
    is (State.waitRab) {
      when (hasRabWrite) {
        sWaitRab_rabWriteOffset := sWaitRab_rabWriteOffset +
          PriorityMux((0 until RabCommitWidth).map(
            idx => i.fromRab.logicPhyRegMap.reverse(idx).valid -> (6 - idx).U
          ))
      }
      when (hasRatWrite) {
        sWaitRab_ratWriteOffset := sWaitRab_ratWriteOffset +
          PriorityMux((0 until RabCommitWidth).map(
            idx => regWriteFromRatVec.reverse(idx).valid -> (6 - idx).U
          ))
      }

      when(sWaitRab_rabWriteOffset === 0.U) {
        // the first patch of RAB commit consider offset
        when(sWaitRab_vecExcpInfo.bits.isStride) {
          (2 until RabCommitWidth).map { idx =>
            val vdIdx = idx - 2
            when(regWriteFromRabVec(idx).valid) {
              regMaps(vdIdx).lreg := regWriteFromRabVec(idx).bits.lreg
              regMaps(vdIdx).newPreg := regWriteFromRabVec(idx).bits.preg
              rabCommitted(vdIdx) := true.B
            }
          }
        }.otherwise {
          (1 until RabCommitWidth).map { idx =>
            val vdIdx = idx - 1
            when(regWriteFromRabVec(idx).valid) {
              regMaps(vdIdx).lreg := regWriteFromRabVec(idx).bits.lreg
              regMaps(vdIdx).newPreg := regWriteFromRabVec(idx).bits.preg
              rabCommitted(vdIdx) := true.B
            }
          }
        }
      }.otherwise {
        // the second patch of RAB/RAT commit need no offset
        when(sWaitRab_vecExcpInfo.bits.isStride) {
          (0 until (MaxLMUL + 2 - RabCommitWidth)).map { idx =>
            val vdIdx = idx - 2 + RabCommitWidth
            when(regWriteFromRabVec(idx).valid) {
              regMaps(vdIdx).lreg := regWriteFromRabVec(idx).bits.lreg
              regMaps(vdIdx).newPreg := regWriteFromRabVec(idx).bits.preg
              rabCommitted(vdIdx) := true.B
            }
          }
        }.otherwise {
          (0 until MaxLMUL + 1 - RabCommitWidth).map { idx =>
            val vdIdx = idx - 1 + RabCommitWidth
            when(regWriteFromRabVec(idx).valid) {
              regMaps(vdIdx).lreg := regWriteFromRabVec(idx).bits.lreg
              regMaps(vdIdx).newPreg := regWriteFromRabVec(idx).bits.preg
              rabCommitted(vdIdx) := true.B
            }
          }
        }
      }

      when (sWaitRab_ratWriteOffset === 0.U) {
        // the first patch of RAT commit consider offset
        when(sWaitRab_vecExcpInfo.bits.isStride) {
          (2 until RabCommitWidth).map { idx =>
            val vdIdx = idx - 2
            when(regWriteFromRatVec(idx).valid) {
              regMaps(vdIdx).oldPreg := regWriteFromRatVec(idx).bits
              ratCommitted(vdIdx) := true.B
            }
          }
        }.otherwise {
          (1 until RabCommitWidth).map { idx =>
            val vdIdx = idx - 1
            when(regWriteFromRatVec(idx).valid) {
              regMaps(vdIdx).oldPreg := regWriteFromRatVec(idx).bits
              ratCommitted(vdIdx) := true.B
            }
          }
        }
      }.otherwise {
        // the second patch of RAT commit need no offset
        when(sWaitRab_vecExcpInfo.bits.isStride) {
          (0 until (MaxLMUL + 2 - RabCommitWidth)).map { idx =>
            val vdIdx = idx - 2 + RabCommitWidth
            when(regWriteFromRatVec(idx).valid) {
              regMaps(vdIdx).oldPreg := regWriteFromRatVec(idx).bits
              ratCommitted(vdIdx) := true.B
            }
          }
        }.otherwise {
          (0 until MaxLMUL + 1 - RabCommitWidth).map { idx =>
            val vdIdx = idx - 1 + RabCommitWidth
            when(regWriteFromRatVec(idx).valid) {
              regMaps(vdIdx).oldPreg := regWriteFromRatVec(idx).bits
              ratCommitted(vdIdx) := true.B
            }
          }
        }
      }
    }
    is (State.finish) {
      commitNeeded.foreach(_ := false.B)
      rabCommitted.foreach(_ := false.B)
      ratCommitted.foreach(_ := false.B)
      hasReadRf   .foreach(_ := false.B)
      sWaitRab_rabWriteOffset := 0.U
      sWaitRab_ratWriteOffset := 0.U
      sWaitRab_vecExcpInfo.valid := false.B
    }
  }

  switch (state) {
    is (State.mergeVd, State.mvOldVd) {
      (0 until maxMergeNumPerCycle).map(vIdx =>
        when(i.fromVprf.rdata(vIdx).valid) {
          mergedVd(vIdx) := mergedVdWData(vIdx).asTypeOf(new VecElemData(VLEN))
        }
      )
    }
  }

  when (state === State.mergeVd) {
    (0 until maxMergeNumPerCycle).foreach { case (idx) =>
      val vdIdx = currentIdxVec(idx)
      // when nonSegIndexed load, iemul/demul = 1 << 2, vdLoc will be mapped as (0, 1, 2, 3, ...) -> (0, 4, ...)
      val oldVdLoc = oldVdLocVec(idx)
      // when nonSegIndexed load, iemul/demul = 1 << 2, vdLoc will be mapped as (0, 1, 2, 3, ...) -> (3, 7, ...)
      val newVdLoc = newVdLocVec(idx)
      o.toVPRF.r(idx).valid := commitNeeded(vdIdx) && !hasReadRf(vdIdx) && vdIdx < sWaitRab_needMergeUntil
      o.toVPRF.r(idx).bits.addr := regMaps(oldVdLoc).oldPreg
      o.toVPRF.r(idx).bits.isV0 := (regMaps(oldVdLoc).lreg === 0.U) && (idx == 0).B
      o.toVPRF.r(idx + maxMergeNumPerCycle).valid := commitNeeded(vdIdx) && !hasReadRf(vdIdx) && vdIdx < sWaitRab_needMergeUntil
      o.toVPRF.r(idx + maxMergeNumPerCycle).bits.addr := regMaps(newVdLoc).newPreg
      o.toVPRF.r(idx + maxMergeNumPerCycle).bits.isV0 := (regMaps(newVdLoc).lreg === 0.U) && (idx == 0).B
      hasReadRf(vdIdx) := true.B && vdIdx < sWaitRab_needMergeUntil
    }
  }.elsewhen (state === State.mvOldVd) {
    (0 until maxMergeNumPerCycle).foreach { case (idx) =>
      val vdIdx = currentIdxVec(idx)
      // when nonSegIndexed load, iemul/demul = 1 << 2, vdLoc will be mapped as (0, 1, 2, 3, ...) -> (0, 4, ...)
      val oldVdLoc = oldVdLocVec(idx)
      // when nonSegIndexed load, iemul/demul = 1 << 2, vdLoc will be mapped as (0, 1, 2, 3, ...) -> (3, 7, ...)
      val newVdLoc = newVdLocVec(idx)
      o.toVPRF.r(idx).valid := commitNeeded(vdIdx) && !hasReadRf(vdIdx) && vdIdx < sWaitRab_handleUntil
      o.toVPRF.r(idx).bits.addr := regMaps(oldVdLoc).oldPreg
      o.toVPRF.r(idx).bits.isV0 := (regMaps(oldVdLoc).lreg === 0.U) && (idx == 0).B
      o.toVPRF.r(idx + maxMergeNumPerCycle).valid := 0.U
      o.toVPRF.r(idx + maxMergeNumPerCycle).bits.addr := 0.U
      o.toVPRF.r(idx + maxMergeNumPerCycle).bits.isV0 := false.B
      hasReadRf(vdIdx) := true.B && vdIdx < sWaitRab_handleUntil
    }
  }.otherwise {
    o.toVPRF.r := 0.U.asTypeOf(chiselTypeOf(o.toVPRF.r))
  }

  o.toVPRF.w.zipWithIndex.foreach { case (w, idx) =>
    val vdIdx = currentIdxVec(idx)
    // when nonSegIndexed load, iemul/demul = 1 << 2, vdLoc will be mapped as (0, 1, 2, 3, ...) -> (0, 4, ...)
    val oldVdLoc = oldVdLocVec(idx)
    // when nonSegIndexed load, iemul/demul = 1 << 2, vdLoc will be mapped as (0, 1, 2, 3, ...) -> (3, 7, ...)
    val newVdLoc = newVdLocVec(idx)
    w.valid          := RegNext(i.fromVprf.rdata(idx).valid)
    w.bits.isV0      := (regMaps(newVdLoc).lreg === 0.U) && (idx == 0).B
    w.bits.newVdAddr := regMaps(newVdLoc).newPreg
    w.bits.newVdData := mergedVd(idx.U).asUInt
  }

  o.status.busy := DelayN(state.isOneOf(State.waitRab, State.mergeVd, State.mvOldVd), 1)
}

class LogicPhyRegMap(implicit p: Parameters) extends XSBundle {
  val lreg = UInt(LogicRegsWidth.W)
  val newPreg = UInt(VfPhyRegIdxWidth.W)
  val oldPreg = UInt(VfPhyRegIdxWidth.W)
}

class RegWriteFromRab(implicit p: Parameters) extends XSBundle {
  private val maxVregLMUL = 8
  val lreg = UInt(LogicRegsWidth.W)
  val preg = UInt(VfPhyRegIdxWidth.W)
}

class RabToVecExcpMod(implicit p: Parameters) extends XSBundle {
  val logicPhyRegMap = Vec(RabCommitWidth, ValidIO(new RegWriteFromRab))
}

class VecExcpInfo(implicit p: Parameters) extends XSBundle {
  val vstart = Vstart()
  val vsew = VSew()
  val veew = VSew()
  val vlmul = VLmul()
  val nf = Nf()
  val isStride = Bool()
  val isIndexed = Bool()
  val isWhole = Bool()
  val isVlm = Bool()
}

class RatToVecExcpMod(implicit p: Parameters) extends XSBundle {
  val vecOldVdPdest = Vec(RabCommitWidth, ValidIO(UInt(VfPhyRegIdxWidth.W)))
  val v0OldVdPdest = Vec(RabCommitWidth, ValidIO(UInt(VfPhyRegIdxWidth.W)))
}

class VprfToExcpMod(numPort: Int)(implicit p: Parameters) extends XSBundle {
  val rdata = Vec(numPort, ValidIO(UInt(VLEN.W)))
}

class ExcpModToVprf(numReadPort: Int, numWritePort: Int)(implicit p: Parameters) extends XSBundle {
  val r = Vec(numReadPort, ValidIO(new Bundle {
    val isV0 = Bool()
    val addr = UInt(VfPhyRegIdxWidth.W)
  }))
  val w = Vec(numWritePort, ValidIO(new Bundle {
    val isV0      = Bool()
    val newVdAddr = UInt(VfPhyRegIdxWidth.W)
    val newVdData = UInt(VLEN.W)
  }))
}

class NfMappedElemIdx(vlen: Int) extends Module {
  require(isPow2(vlen))
  // vlen = 128, idxWidth = 8, hold 0~128
  val idxWidth = log2Up(vlen + 1)

  val in = IO(Input(new Bundle {
    val nf = Nf()
    val eewOH = SewOH()
  }))
  val out = IO(Output(new Bundle {
    val idxRangeVec = Vec(8, new HWRange(idxWidth))
  }))

  private val minElemLen = 8
  private val maxElemNumPerVreg = vlen / minElemLen

  private val rangeTable: Vec[Vec[HWRange]] = VecInit.tabulate(8, 8) { case(nf, vdIdx) =>
    val nFields = nf + 1
    // vector register group
    val vrgIdx = vdIdx / nFields
    HWRange(idxWidth)((maxElemNumPerVreg * vrgIdx).U, (maxElemNumPerVreg * (vrgIdx + 1)).U)
  }

  out.idxRangeVec := VecInit(rangeTable.map { case rangeVec: Vec[HWRange] =>
    Mux1H(
      (0 until 4).map(i =>
        in.eewOH(i) -> VecInit(rangeVec.map(
          x => HWRange(idxWidth)(x.from >> i, x.until >> i)
        ))
      )
    )
  })(in.nf)

  dontTouch(out.idxRangeVec)

  def apply(nf: UInt, eewOH: UInt): Vec[HWRange] = {
    this.in.nf := nf
    this.in.eewOH := eewOH
    this.out.idxRangeVec
  }
}

class GetE8OffsetInVreg(vlen: Int) extends Module {
  require(isPow2(vlen))
  private val minElemLen = 8
  private val maxElemNumPerVreg = vlen / minElemLen
  private val tailZeroBit = log2Ceil(maxElemNumPerVreg) // 16 -> 4

  val in = IO(Input(new Bundle {
    val eewOH = SewOH()
    val idx = UInt(log2Up(vlen).W)
  }))
  val out = IO(Output(new Bundle {
    val offset = UInt(tailZeroBit.W)
  }))

  out.offset := Mux1H(
    (0 until 4).map(
      // eew=32(0b0100), idx=1, get offset=4
      i => in.eewOH(i) -> (in.idx << i)
    )
  )

  def apply(eewOH: UInt, idx: UInt): UInt = {
    this.in.eewOH := eewOH
    this.in.idx := idx
    this.out.offset
  }
}

class VecElemData(vlen: Int) extends Bundle {
  val rawData = UInt(vlen.W)

  def e8Vec  = this.rawData.asTypeOf(new VecE8Vec(vlen))
  def e16Vec = this.rawData.asTypeOf(new VecE16Vec(vlen))
  def e32Vec = this.rawData.asTypeOf(new VecE32Vec(vlen))
  def e64Vec = this.rawData.asTypeOf(new VecE64Vec(vlen))
}

class VecE8Vec(vlen: Int) extends Bundle {
  val data = Vec(vlen / 8, UInt(8.W))

  def apply(idx: Int): UInt = this.data(idx)
}

class VecE16Vec(vlen: Int) extends Bundle {
  val data = Vec(vlen / 16, UInt(16.W))

  def apply(idx: Int): UInt = this.data(idx)
}

class VecE32Vec(vlen: Int) extends Bundle {
  val data = Vec(vlen / 32, UInt(32.W))

  def apply(idx: Int): UInt = this.data(idx)
}

class VecE64Vec(vlen: Int) extends Bundle {
  val data = Vec(vlen / 64, UInt(64.W))

  def apply(idx: Int): UInt = this.data(idx)
}

class HWRange(w: Int) extends Bundle {
  val from  = UInt(w.W)
  val until = UInt(w.W)

  def inRange(uint: UInt) = {
    uint >= this.from && uint < this.until
  }

  def apply(_from: Bits, _until: Bits): this.type = {
    this.from := _from
    this.until := _until
    this
  }
}

object HWRange {
  def apply(w: Int)(_from: Bits, _until: Bits): HWRange = Wire(new HWRange(w)).apply(_from, _until)
}

