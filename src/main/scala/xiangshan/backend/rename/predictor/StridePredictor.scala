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

package xiangshan.backend.rename.predictor

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.frontend.FtqPtr
import xiangshan.backend.BackendParams
import freechips.rocketchip.util.SeqToAugmentedSeq
import freechips.rocketchip.util.SeqBoolBitwiseOps
import chisel3.experimental.BundleLiterals._

trait StridePredictorParams {
  val NumEntries: Int = 128
  val NumWay: Int = 4
  val TagWidth: Int = 16
  val PcOffset: Int = 2

  val StrideWidth = 8
  val InflightWidth = 7
  val ConfidenceWidth = 4
  val UtilityWidth = 2

  val CommitUpdateSize = 2
  val CommitBufferSize = 24

  def NumGroup = NumEntries / NumWay
  def GroupWidth = log2Up(NumGroup)
  def ValidPcWidth = PcOffset + GroupWidth + TagWidth - 1

  def MaxConfidenceVal = (math.pow(2, ConfidenceWidth) - 1).toInt
  def MaxUtilityVal = (math.pow(2, UtilityWidth) - 1).toInt

  def get_group(pc: UInt): UInt = {
    pc(GroupWidth + PcOffset - 1, PcOffset)
  }

  def get_tag(pc: UInt): UInt = {
    Cat(pc(TagWidth + GroupWidth + PcOffset - 2, GroupWidth + PcOffset), pc(1))
  }
}

class StridePredictor()(implicit p: Parameters) extends XSModule with StridePredictorParams with HasCircularQueuePtrHelper {

  val io = IO(new StridePredictorIO())

  println(s"[StridePredictor] NumEntries: ${NumEntries}, NumWay: ${NumWay}, NumGroup: ${NumGroup}, GroupWidth: ${GroupWidth}, ValidPcWidth: ${ValidPcWidth}")

  require(NumEntries % NumWay == 0, "NumEntries % NumWay must be 0")
  require(NumGroup == (math.pow(2, GroupWidth)).toInt, "NumGroup must be a power of 2")

  val spEntries = RegInit(VecInit.fill(NumGroup)(VecInit.fill(NumWay)((new StridePredictorEntry).Lit(_.valid -> false.B))))

  // 1. read status
  val readEnableVec = io.spReadPort.map(x => x.ren)
  val readAddrVec = io.spReadPort.map(x => get_group(x.pc))
  val readTagVec = io.spReadPort.map(x => get_tag(x.pc))
  val readValidVec = readAddrVec.map(spEntries(_).map(_.valid))
  val readEntryVec = readAddrVec.map(spEntries(_))

  val readMatchOHVec: IndexedSeq[Vec[Bool]] = readTagVec.zipWithIndex.map{ case (tag, i) =>
    val matchOH = VecInit(readValidVec(i).zip(readEntryVec(i)).map{ case (v, entry) =>
      v && entry.tag === tag
    })
    assert(PopCount(matchOH) <= 1.U, s"readMatchOH(${i}) is not one-hot")
    matchOH
  }
  val readMatchVec = readMatchOHVec.map(_.asUInt.orR)

  io.spReadPort.zipWithIndex.foreach{ case (rport, i) =>
    rport.needPf   := Mux1H(readMatchOHVec(i), readEntryVec(i).map(x => x.confidence >= 4.U))
    rport.predAddr := Mux1H(readMatchOHVec(i), readEntryVec(i).map(x => x.prevAddr + x.stride * (x.inflight + 1.U)))
  }

  // update inflight counter
  val finalMatchOHVec = readMatchOHVec.zipWithIndex.map{ case (matchOH, i) =>
    val updateEnable = (readEnableVec.lazyZip(readAddrVec).lazyZip(readTagVec)).take(i).map{ case (ren, group, tag) =>
      !(ren && group === readAddrVec(i) && tag === readTagVec(i))
    }.fold(true.B)(_ && _) && readEnableVec(i)
    VecInit(matchOH.map(_ && updateEnable))
  }
  val finalMatchCountVec = Wire(Vec(RenameWidth, UInt(log2Up(RenameWidth + 1).W)))
  finalMatchCountVec.zipWithIndex.foreach{ case (count, i) =>
    count := (readEnableVec.lazyZip(readAddrVec).lazyZip(readTagVec)).takeRight(RenameWidth - i - 1).map{ case (ren, group, tag) =>
      Mux(ren && group === readAddrVec(i) && tag === readTagVec(i), 1.U, 0.U)
    }.fold(1.U)(_ +& _)
  }

  // read update
  val readUpdateInflightVec = Wire(Vec(RenameWidth, Vec(NumWay, UInt(InflightWidth.W))))
  readUpdateInflightVec.zipWithIndex.foreach{ case (inflightVec, i) =>
    inflightVec.zipWithIndex.foreach{ case (inflight, j) =>
      inflight := readEntryVec(i)(j).inflight + finalMatchCountVec(i)
    }
  }

  // 2. allocate entry
  val needAllocVec = io.spReadPort.zip(readMatchVec).map(x => x._1.ren && !x._2)

  // find entry to replace
  val allocOHVec: IndexedSeq[Vec[Bool]] = readValidVec.zip(readEntryVec).zipWithIndex.map{ case ((validVec, entryVec), idx) =>
    val age = Wire(Vec(NumWay, Vec(NumWay, Bool())))
    for((row, i) <- age.zipWithIndex) {
      for((elem, j) <- row.zipWithIndex) {
        if (i == j) {
          // an entry is always older than itself
          elem := true.B
        }
        else if (i < j) {
          when (validVec(i) && !validVec(j)) {
            elem := false.B
          }.elsewhen (!validVec(i) && validVec(j)) {
            elem := true.B
          }.otherwise {
            elem := entryVec(i).utility <= entryVec(j).utility
          }
        }
        else {
          elem := !age(j)(i)
        }
      }
    }
    val oldestOH = VecInit(age.map(_.asUInt.andR))
    assert(PopCount(oldestOH) <= 1.U, s"oldestOH(${idx}) is not one-hot")
    oldestOH
  }

  // check same entry
  val finalAllocOHVec = allocOHVec.zipWithIndex.map{ case (allocOH, i) =>
    val allocEnable = (needAllocVec.lazyZip(readAddrVec)).take(i).map{ case (need, group) =>
      !(need && group === readAddrVec(i))
    }.fold(true.B)(_ && _) && needAllocVec(i)
    VecInit(allocOH.map(_ && allocEnable))
  }

  // allocate update
  val allocateUpdateEntryVec = Wire(Vec(RenameWidth, new StridePredictorEntry))
  allocateUpdateEntryVec.zipWithIndex.foreach{ case (entry, i) =>
    entry := 0.U.asTypeOf(new StridePredictorEntry)
    entry.tag := readTagVec(i)
    entry.inflight := finalMatchCountVec(i)
  }

  // 3. commit update entry
  val updateValid = RegInit(VecInit(Seq.fill(CommitUpdateSize)(false.B)))
  val updateInfo = Reg(Vec(CommitUpdateSize, new SPUpdateBundle))
  val updatePC = io.fromSPPcMem.map(_.pc)

  val updateAddrVec = updatePC.map(x => get_group(x))
  val updateTagVec = updatePC.map(x => get_tag(x))
  val updateValidVec = updateAddrVec.map(spEntries(_).map(_.valid))
  val updateEntryVec = updateAddrVec.map(spEntries(_))

  val updateMatchOHVec: IndexedSeq[Vec[Bool]] = updateTagVec.zipWithIndex.map{ case (tag, i) =>
    val matchOH = VecInit(updateValidVec(i).zip(updateEntryVec(i)).map{ case (v, entry) =>
      v && entry.tag === tag
    })
    assert(PopCount(matchOH) <= 1.U, s"updateMatchOH(${i}) is not one-hot")
    matchOH
  }
  val updateMatchVec = updateMatchOHVec.map(_.asUInt.orR)

  // check same entry
  val finalUpdateMatchOHVec: IndexedSeq[Vec[Bool]] = updateMatchOHVec.zipWithIndex.map{ case (matchOH, i) =>
    val updateEnable = (updateValid.lazyZip(updateAddrVec).lazyZip(updateTagVec)).take(i).map{ case (wen, group, tag) =>
      !(wen && group === updateAddrVec(i) && tag === updateTagVec(i))
    }.fold(true.B)(_ && _) && updateValid(i)
    VecInit(matchOH.map(_ && updateEnable))
  }
  val finalUpdateMatchCountVec = Wire(Vec(CommitUpdateSize, UInt(log2Up(CommitUpdateSize + 1).W)))
  finalUpdateMatchCountVec.zipWithIndex.foreach{ case (count, i) =>
    count := (updateValid.lazyZip(updateAddrVec).lazyZip(updateTagVec)).takeRight(CommitUpdateSize - i - 1).map{ case (wen, group, tag) =>
      Mux(wen && group === updateAddrVec(i) && tag === updateTagVec(i), 1.U, 0.U)
    }.fold(1.U)(_ +& _)
  }
  val updateMatchReadVec: IndexedSeq[Vec[Bool]] = updateAddrVec.zip(updateTagVec).map{ case (upgroup, uptag) =>
    VecInit(readEnableVec.lazyZip(readAddrVec).lazyZip(readTagVec).map{ case (ren, group, tag) =>
      ren && upgroup === group && uptag === tag
    })
  }
  val updateMatchReadCountVec = updateMatchReadVec.map{ case matchOH =>
    PriorityMuxDefault(matchOH.zip(finalMatchCountVec), 0.U)
  }

  // commit update
  val commitUpdateEntryVec = Wire(Vec(CommitUpdateSize, Vec(NumWay, new StridePredictorEntry)))
  commitUpdateEntryVec.zipWithIndex.foreach{ case (commitEntryVec, i) =>
    commitEntryVec.zipWithIndex.foreach{ case (entry, j) =>
      // note: only work in CommitUpdateSize = 2
      entry := updateEntryVec(i)(j)
      if (i != 0) {
        assert(finalUpdateMatchCountVec(i) <= 1.U, s"only i == 0 can have match count == 2")
        entry.prevAddr := updateInfo(i).currAddr
        entry.inflight := updateEntryVec(i)(j).inflight + updateMatchReadCountVec(i) - 1.U
        when (updateInfo(i).pfHit) {
          entry.confidence := Mux(updateEntryVec(i)(j).confidence === MaxConfidenceVal.U, MaxConfidenceVal.U, updateEntryVec(i)(j).confidence + 1.U)
          entry.utility := Mux(updateEntryVec(i)(j).utility === MaxUtilityVal.U, MaxUtilityVal.U, updateEntryVec(i)(j).utility + 1.U)
        }.otherwise {
          entry.utility := 0.U
          when (updateEntryVec(i)(j).confidence === 0.U) {
            entry.stride := updateInfo(i).currAddr - updateEntryVec(i)(j).prevAddr
            entry.confidence := 0.U
          }.otherwise {
            entry.confidence := updateEntryVec(i)(j).confidence >> 1
          }
        }
      }
      else {
        when (finalUpdateMatchCountVec(i) === 2.U) {
          entry.prevAddr := updateInfo(i + 1).currAddr
          entry.inflight := updateEntryVec(i)(j).inflight + updateMatchReadCountVec(i) - 2.U
          when (updateInfo(i).pfHit && updateInfo(i + 1).pfHit) {
            entry.confidence := Mux(updateEntryVec(i)(j).confidence >= (MaxConfidenceVal - 1).U, MaxConfidenceVal.U, updateEntryVec(i)(j).confidence + 2.U)
            entry.utility := Mux(updateEntryVec(i)(j).utility >= (MaxUtilityVal - 1).U, MaxUtilityVal.U, updateEntryVec(i)(j).utility + 2.U)
          }.elsewhen (updateInfo(i).pfHit && !updateInfo(i + 1).pfHit) {
            entry.confidence := updateEntryVec(i)(j).confidence >> 1
            entry.utility := 0.U
          }.elsewhen (!updateInfo(i).pfHit && updateInfo(i + 1).pfHit) {
            entry.confidence := (updateEntryVec(i)(j).confidence >> 1) + 1.U
            entry.utility := 1.U
          }.otherwise {
            entry.utility := 0.U
            when (updateEntryVec(i)(j).confidence <= 1.U) {
              entry.stride := updateInfo(i + 1).currAddr - updateInfo(i).currAddr
              entry.confidence := 0.U
            }.otherwise {
              entry.confidence := updateEntryVec(i)(j).confidence >> 2
            }
          }
        }.otherwise {
          entry.prevAddr := updateInfo(i).currAddr
          entry.inflight := updateEntryVec(i)(j).inflight + updateMatchReadCountVec(i) - 1.U
          when (updateInfo(i).pfHit) {
            entry.confidence := Mux(updateEntryVec(i)(j).confidence === MaxConfidenceVal.U, MaxConfidenceVal.U, updateEntryVec(i)(j).confidence + 1.U)
            entry.utility := Mux(updateEntryVec(i)(j).utility === MaxUtilityVal.U, MaxUtilityVal.U, updateEntryVec(i)(j).utility + 1.U)
          }.otherwise {
            entry.utility := 0.U
            when (updateEntryVec(i)(j).confidence === 0.U) {
              entry.stride := updateInfo(i).currAddr - updateEntryVec(i)(j).prevAddr
              entry.confidence := 0.U
            }.otherwise {
              entry.confidence := updateEntryVec(i)(j).confidence >> 1
            }
          }
        }
      }
    }
  }

  // 4. write entry
  for((group, i) <- spEntries.zipWithIndex) {
    for((entry, j) <- group.zipWithIndex) {
      val commitOH = updateAddrVec.zip(finalUpdateMatchOHVec).map{ case (addr, matchOH) =>
        addr === i.U && matchOH(j)
      }
      val commitEn = commitOH.orR
      val allocOH = readAddrVec.zip(finalAllocOHVec).map{ case (addr, allocOH) =>
        addr === i.U && allocOH(j)
      }
      val allocEn = allocOH.orR
      val readOH = readAddrVec.zip(finalMatchOHVec).map{ case (addr, matchOH) =>
        addr === i.U && matchOH(j)
      }
      val readEn = readOH.orR
      assert(PopCount(commitOH) <= 1.U, s"entry(${i})(${j}) commitOH is not one-hot")
      assert(PopCount(allocOH) <= 1.U, s"entry(${i})(${j}) allocOH is not one-hot")
      assert(PopCount(readOH) <= 1.U, s"entry(${i})(${j}) readOH is not one-hot")

      when (commitEn) {
        entry := Mux1H(commitOH, commitUpdateEntryVec.map(_(j)))
      }
      .elsewhen (readEn) {
        entry.inflight := Mux1H(readOH, readUpdateInflightVec.map(_(j)))
      }
      .elsewhen (allocEn) {
        entry := Mux1H(allocOH, allocateUpdateEntryVec)
      }
    }
  }

  // 5. commit buffer
  val spCommitBuffer = Reg(Vec(CommitBufferSize, new SPCommitPort))
  val enqPtrVec = RegInit(VecInit.tabulate(CommitWidth)(_.U.asTypeOf(new SPCommitBufferPtr(CommitBufferSize))))
  val deqPtrVec = RegInit(VecInit.tabulate(CommitUpdateSize)(_.U.asTypeOf(new SPCommitBufferPtr(CommitBufferSize))))
  val enqPtrHead = enqPtrVec(0)
  val deqPtrHead = deqPtrVec(0)

  // enq
  val commitValidVec = io.spCommitPort.map(_.wen)
  val enqNum = Wire(UInt(log2Up(CommitWidth + 1).W))
  val enqNumVec = Wire(Vec(CommitWidth, UInt(log2Up(CommitWidth + 1).W)))
  enqNumVec.zipWithIndex.foreach{ case (num, i) =>
    num := io.spCommitPort.take(i + 1).map(_.wen.asUInt).reduce(_ +& _)
  }
  enqNum := enqNumVec.last

  val enqEntries = Wire(Vec(CommitWidth, new SPCommitPort))
  enqEntries.zipWithIndex.foreach{ case (enqEntry, i) =>
    if (i == 0) {
      val selVec = commitValidVec(0) +: enqNumVec.dropRight(1).map(_ === 0.U).zip(commitValidVec.drop(1)).map(x => x._1 && x._2)
      enqEntry := Mux1H(selVec, io.spCommitPort)
      assert(PopCount(selVec) <= 1.U, s"selVec(${i}) is not one-hot")
    }
    else if (i != CommitWidth - 1) {
      val selVec = enqNumVec.drop(i - 1).dropRight(1).map(_ === i.U).zip(commitValidVec.drop(i)).map(x => x._1 && x._2)
      enqEntry := Mux1H(selVec, io.spCommitPort.drop(i))
      assert(PopCount(selVec) <= 1.U, s"selVec(${i}) is not one-hot")
    }
    else {
      enqEntry := Mux(enqNum === CommitWidth.U, io.spCommitPort.last, 0.U.asTypeOf(enqEntry))
    }
  }

  val validNum = distanceBetween(enqPtrHead, deqPtrHead)
  val freeNum = CommitBufferSize.U - validNum
  val OverflowNum = Mux((enqNum <= freeNum), 0.U, enqNum - freeNum)
  enqPtrVec.zipWithIndex.foreach{ case (enqPtr, i) =>
    enqPtr := enqPtr + enqNum
    when (i.U < enqNum && i.U < freeNum) {
      spCommitBuffer(enqPtr.value) := enqEntries(i)
    }
  }
  XSPerfAccumulate("overflow_instr_cnt", OverflowNum)
  XSPerfHistogram("valid_entry_cnt", validNum, true.B, 0, NumEntries + 1)

  // deq
  val deqNum = Wire(UInt(log2Up(CommitUpdateSize + 1).W))
  val deqEntries = deqPtrVec.map(x => spCommitBuffer(x.value))
  deqNum := deqEntries.map(_.wen.asUInt).reduce(_ +& _)
  assert(deqNum <= validNum, "deqNum should be small than or equal to validNum")

  deqPtrVec.zipWithIndex.foreach{ case (deqPtr, i) =>
    deqPtr := deqPtr + deqNum
    when (i.U < deqNum) {
      spCommitBuffer(deqPtr.value).wen := false.B
    }
  }

  updateValid.lazyZip(updateInfo).lazyZip(deqEntries).lazyZip(io.fromSPPcMem).foreach{ case (valid, info, deqEntry, readPC) =>
    valid := deqEntry.wen
    info.pfHit := deqEntry.pfHit
    info.currAddr := deqEntry.currAddr
    readPC.ren := deqEntry.wen
    readPC.ftqPtr := deqEntry.ftqPtr
    readPC.ftqOffset := deqEntry.ftqOffset
  }
}

class StridePredictorEntry()(implicit p: Parameters) extends XSBundle with StridePredictorParams {
  val valid      = Bool()
  val tag        = UInt(TagWidth.W)

  val stride     = UInt(StrideWidth.W)
  val prevAddr   = UInt(VAddrBits.W)

  val inflight   = UInt(InflightWidth.W)

  val confidence = UInt(ConfidenceWidth.W)
  val utility    = UInt(UtilityWidth.W)
}

class SPUpdateBundle()(implicit p: Parameters) extends XSBundle with StridePredictorParams {
  val pfHit     = Bool()
  val currAddr  = UInt(VAddrBits.W)
}

class SPCommitBufferPtr(entries: Int) extends CircularQueuePtr[SPCommitBufferPtr](entries) with HasCircularQueuePtrHelper {
}

class SPReadPort()(implicit p: Parameters) extends XSBundle with StridePredictorParams {
  val ren       = Input(Bool())
  val pc        = Input(UInt(ValidPcWidth.W))
  val needPf    = Output(Bool())
  val predAddr  = Output(UInt(VAddrBits.W))
}

class SPCommitPort()(implicit p: Parameters) extends XSBundle with StridePredictorParams {
  val wen       = Bool()
  val ftqPtr    = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val pfHit     = Bool()
  val currAddr  = UInt(VAddrBits.W)
}

class StridePredictorIO()(implicit p: Parameters) extends XSBundle with StridePredictorParams{

  val spReadPort = Vec(RenameWidth, new SPReadPort)

  val spCommitPort = Vec(CommitWidth, Input(new SPCommitPort))

  val fromSPPcMem = Flipped(Vec(CommitUpdateSize, new SPPcMemReadPort))
}
