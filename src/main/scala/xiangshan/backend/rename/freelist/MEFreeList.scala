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

package xiangshan.backend.rename.freelist

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chipsalliance.rocketchip.config

class MEFreeList(implicit val p: config.Parameters) extends MultiIOModule with MEFreeListIO with HasXSParameter with HasCircularQueuePtrHelper {
  val flush = IO(Input(Bool()))
  val redirect = IO(Input(Bool()))
  val walk = IO(Input(Bool()))

  val allocateReq = IO(Input(Vec(RenameWidth, Bool())))
  val allocatePhyReg = IO(Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W))))
  val canAllocate = IO(Output(Bool()))
  val doAllocate = IO(Input(Bool()))

  val freeReq = IO(Input(Vec(CommitWidth, Bool())))
  val freePhyReg = IO(Input(Vec(CommitWidth, UInt(PhyRegIdxWidth.W))))

  val stepBack = IO(Input(UInt(log2Up(CommitWidth + 1).W)))

  // additional ports designed for move elimination
  val psrcOfMove = IO(Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W)))))
  val eliminatedMove = IO(Vec(CommitWidth, Input(Bool())))
  val multiRefPhyReg = IO(Vec(CommitWidth, Input(UInt(PhyRegIdxWidth.W))))
  val maxVec = IO(Vec(NRPhyRegs, Output(Bool())))


  class FreeListPtr extends CircularQueuePtr[FreeListPtr](MEFreeListSize)

  object FreeListPtr {
    def apply(f: Bool, v: UInt): FreeListPtr = {
      val ptr = Wire(new FreeListPtr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }


  // recording referenced times of each physical registers
  val archRefCounter = RegInit(VecInit(Seq.fill(NRPhyRegs)(0.U(IntRefCounterWidth.W))))
  val specRefCounter = RegInit(VecInit(Seq.fill(NRPhyRegs)(0.U(IntRefCounterWidth.W))))
  val cmtCounter = RegInit(VecInit(Seq.fill(NRPhyRegs)(0.U(IntRefCounterWidth.W))))

  val archRefCounterNext = Wire(Vec(NRPhyRegs, UInt(IntRefCounterWidth.W)))
  archRefCounterNext.foreach(_ := DontCare)
  val updateArchRefCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B)))
  val clearArchRefCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B)))

  val specRefCounterNext = Wire(Vec(NRPhyRegs, UInt(IntRefCounterWidth.W)))
  specRefCounterNext.foreach(_ := DontCare)
  val updateSpecRefCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B))) // update with xxxNext
  val clearSpecRefCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B))) // reset to zero

  val cmtCounterNext = Wire(Vec(NRPhyRegs, UInt(IntRefCounterWidth.W)))
  cmtCounterNext.foreach(_ := DontCare)
  val updateCmtCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B)))
  val clearCmtCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B)))

  // send max flag of spec ref counter to rename stage
  maxVec zip specRefCounter foreach { case (max, cnt) =>
    max := cnt.andR()
  }


  // number of free registers
  val freeRegCnt = Wire(UInt())

  // free list as circular buffer
  val freeList = RegInit(VecInit(Seq.tabulate(MEFreeListSize){
    case n if (n >= 0 && n < NRPhyRegs - 32) => (n + 32).U
    case _ => DontCare
  }))

  // head and tail pointer
  val headPtr = RegInit(FreeListPtr(false.B, 0.U))

  val tailPtr = RegInit(FreeListPtr(false.B, (NRPhyRegs-32).U))


  /*
  Decrements: from rob commits
   */
  val freeVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B))) // if dec(i).bits is freed and ready for writing back to free list

  val updateCmtCounterVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val updateArchRefCounterVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val decreaseSpecRefCounterVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B))) // used when walking ME instructions
  val decreaseSpecRefCounterValueVec = Wire(Vec(CommitWidth, UInt(log2Ceil(CommitWidth-1).W)))

  // handle duplicate INC requirements on cmtCounter and archRefCounter
  val old_pdests_cmp = Wire(MixedVec(List.tabulate(CommitWidth-1)(i => UInt((i+1).W))))
  val pdests_cmp = Wire(MixedVec(List.tabulate(CommitWidth-1)(i => UInt((i+1).W))))

  for (i <- 1 until CommitWidth) {
    // compare pdest and old_pdest with former inputs
    old_pdests_cmp(i - 1) := Cat((0 until i).map(j => {
      freeReq(i) && freeReq(j) && freePhyReg(i) === freePhyReg(j)
    }).reverse)
    pdests_cmp(i - 1) := Cat((0 until i).map(j => {
      freeReq(i) && freeReq(j) && eliminatedMove(i) && eliminatedMove(j) && multiRefPhyReg(i) === multiRefPhyReg(j)
    }).reverse)
  }

  def getCompareResult(m: MixedVec[UInt]): (Vec[Bool], Vec[UInt]) = {
    val is_last = WireInit(VecInit(Seq.tabulate(CommitWidth){
      case last if (last == CommitWidth - 1) => true.B
      case i => !(Cat((i until (CommitWidth - 1)).map(j => m(j)(i))).orR)
    }))
    val times = WireInit(VecInit(Seq.tabulate(CommitWidth){
      case 0 => 0.U(log2Ceil(CommitWidth-1).W)
      case i => PopCount(m(i - 1))
    }))
    (is_last, times)
  }

  val (old_pdests_is_last, old_pdests_times) = getCompareResult(old_pdests_cmp)
  val (pdests_is_last, pdests_times) = getCompareResult(pdests_cmp)

  for (i <- 0 until CommitWidth) {

    freeVec(i) := (cmtCounter(freePhyReg(i)) + old_pdests_times(i) === specRefCounter(freePhyReg(i))) &&
      freeReq(i) && freePhyReg(i) =/= 0.U && !walk

    updateCmtCounterVec(i) := freeReq(i) && old_pdests_is_last(i) && freePhyReg(i) =/= 0.U && !walk

    updateArchRefCounterVec(i) := freeReq(i) && eliminatedMove(i) && pdests_is_last(i) && multiRefPhyReg(i) =/= 0.U && !walk

    // pdests_is_last equals old_pdests_is_last when walk.valid
    decreaseSpecRefCounterVec(i) := freeReq(i) && eliminatedMove(i) && pdests_is_last(i) && freePhyReg(i) =/= 0.U && walk
    decreaseSpecRefCounterValueVec(i) := pdests_times(i) + 1.U


    // write freed preg into free list at tail ptr
    val offset = i match {
      case 0 => 0.U
      case n => PopCount(freeVec.take(n))
    }
    val ptr = tailPtr + offset
    val idx = ptr.value
    when (freeVec(i)) {
      freeList(idx) := freePhyReg(i)
      XSDebug(p"[$i] Free List enqueue: [ preg ${freePhyReg(i)} ]\n")
    }
  }

  // set counters-update flag
  for (preg <- 1 until NRPhyRegs) {
    // set clear bit
    freeVec.zipWithIndex.foreach { case (ready, idx) =>
      when (ready && preg.U === freePhyReg(idx)) {
        clearArchRefCounter(preg) := true.B
        clearSpecRefCounter(preg) := true.B
        clearCmtCounter(preg) := true.B
      }
    }

    // set update bit
    updateCmtCounterVec.zipWithIndex.foreach { case (ready, idx) =>
      when (ready && preg.U === freePhyReg(idx)) {
        updateCmtCounter(preg) := true.B
        // cmt counter after incrementing/ stay not change
        // free vec has higher priority than cmtCounterNext, so normal free wouldn't cause cmtCounter increasing
        cmtCounterNext(preg) := cmtCounter(preg) + 1.U + old_pdests_times(idx)
      }
    }

    updateArchRefCounterVec.zipWithIndex.foreach { case (ready, idx) =>
      when (ready && preg.U === multiRefPhyReg(idx)) {
        updateArchRefCounter(preg) := true.B
        // arch ref counter of pdest
        archRefCounterNext(preg) := archRefCounter(preg) + 1.U + pdests_times(idx)
      }
    }
  }

  // arch ref counter of #0 register
  val archRefCntZero = RegInit(0.U(5.W))
  // when old_pdest = 0 -> cnt[0]--
  val zeroCntDecValue = PopCount(freeReq.zip(freePhyReg).map{ case (v, r) => v && r === 0.U })
  // when pdest = 0 && isMove -> cnt[0]++
  val zeroCntIncValue = PopCount(freeReq.zip(eliminatedMove).zip(multiRefPhyReg).map{ case ((v, m), r) => v && m && r === 0.U })
  archRefCntZero := Mux(!flush && !walk, archRefCntZero + zeroCntIncValue - zeroCntDecValue, archRefCntZero)


  /*
  Increments: from rename stage
   */
  val needAllocatingVec = WireInit(VecInit(Seq.fill(RenameWidth)(false.B)))
  val increaseSpecRefCounterVec = WireInit(VecInit(Seq.fill(RenameWidth)(false.B)))

  val allocatePtr = (0 until RenameWidth).map(i => headPtr + i.U)
  val phyRegCandidates = VecInit(allocatePtr.map(ptr => freeList(ptr.value)))

  for (i <- 0 until RenameWidth) {
    val renameEnable = allocateReq(i) && canAllocate && doAllocate && !flush && !redirect && !walk
    // enqueue instr, isn't move elimination
    needAllocatingVec(i) := renameEnable && !psrcOfMove(i).valid
    // enqueue instr, is move elimination
    increaseSpecRefCounterVec(i) := renameEnable && psrcOfMove(i).valid

    allocatePhyReg(i) := phyRegCandidates(PopCount(needAllocatingVec.take(i)))
  }

  for (preg <- 1 until NRPhyRegs) {
    val increaseCmpVec = WireInit(VecInit(Seq.tabulate(RenameWidth)(i => increaseSpecRefCounterVec(i) && psrcOfMove(i).bits === preg.U)))
    val decreaseCmpVec = WireInit(VecInit(Seq.tabulate(CommitWidth)(i => decreaseSpecRefCounterVec(i) && freePhyReg(i) === preg.U)))

    val doIncrease = increaseCmpVec.asUInt.orR
    val doDecrease = decreaseCmpVec.asUInt.orR

    updateSpecRefCounter(preg) := doIncrease || doDecrease
    val addOne = specRefCounter(preg) + 1.U
    val subN = specRefCounter(preg) - decreaseSpecRefCounterValueVec(OHToUInt(decreaseCmpVec))
    specRefCounterNext(preg) := Mux(doDecrease, subN, addOne)
  }


  /*
  Flush: directly flush reference counter according to arch-rat
  - replace specRefCounter with archRefCounter; reset headPtr
   */

  val dupRegCntPos = RegInit(0.U(5.W))
  val dupCntIncVec = WireInit(VecInit(Seq.tabulate(CommitWidth)(i => Mux(updateArchRefCounterVec(i), pdests_times(i) + 1.U, 0.U))))
  dupRegCntPos := Mux(!flush && !walk, dupRegCntPos + dupCntIncVec.reduceTree(_ + _), dupRegCntPos)

  val dupRegCntNeg = RegInit(0.U(5.W))
  val dupCntDecVec = WireInit(VecInit(Seq.tabulate(CommitWidth)(i =>
    Mux(updateCmtCounterVec(i) && !freeVec(i), old_pdests_times(i) + 1.U,
    Mux(freeVec(i) && specRefCounter(freePhyReg(i)) =/= 0.U, old_pdests_times(i), 0.U)))))
  dupRegCntNeg := Mux(!flush && !walk, dupRegCntNeg + dupCntDecVec.reduceTree(_ + _), dupRegCntNeg)

  val dupRegCnt = Wire(UInt(5.W))
  dupRegCnt := dupRegCntPos - dupRegCntNeg


  // update tail pointer
  val tailPtrNext = Mux(walk, tailPtr, tailPtr + PopCount(freeVec))
  // update head pointer
  val walkValidVec = WireInit(VecInit(freeReq.zip(eliminatedMove).map{ case (rq, em) => rq && !em }))
  val headPtrNext = Mux(flush, tailPtr - (NRPhyRegs-32).U - dupRegCnt - archRefCntZero,
                      Mux(walk, headPtr - PopCount(walkValidVec),
                      headPtr + PopCount(needAllocatingVec))) // when io.redirect is valid, needAllocatingVec is all-zero

  freeRegCnt := distanceBetween(tailPtr, headPtrNext)
  canAllocate := RegNext(freeRegCnt >= RenameWidth.U)

  headPtr := headPtrNext
  tailPtr := tailPtrNext

  // update reg counter
  for (i <- 1 until NRPhyRegs) {
    specRefCounter(i) := Mux(flush, archRefCounter(i),
                           Mux(clearSpecRefCounter(i), 0.U, Mux(updateSpecRefCounter(i), specRefCounterNext(i), specRefCounter(i))))
    archRefCounter(i) :=   Mux(clearArchRefCounter(i), 0.U, Mux(updateArchRefCounter(i), archRefCounterNext(i), archRefCounter(i) ))
    cmtCounter(i)     :=   Mux(clearCmtCounter(i),     0.U, Mux(updateCmtCounter(i),     cmtCounterNext(i),     cmtCounter(i)     ))
  }
}
