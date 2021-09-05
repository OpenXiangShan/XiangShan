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
  Decrements: from roq commits
   */
  val freeVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B))) // if dec(i).bits is freed and ready for writing back to free list
  val freeRegCandidates = (0 until CommitWidth).map(freePhyReg(_))

  val updateCmtCounterVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val updateArchRefCounterVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val decreaseSpecRefCounterVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B))) // used when walking ME instructions
  val decreaseSpecRefCounterValueVec = Wire(Vec(CommitWidth, UInt(log2Ceil(CommitWidth-1).W)))

  val oldPdestIsUnique = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val oldPdestNotUniqueButLast = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val pdestIsUnique = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val pdestNotUniqueButLast = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))

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

  def getCompareResult(m: MixedVec[UInt]): (Vec[Bool], Vec[Bool], Vec[UInt]) = {
    val is_last = WireInit(VecInit(Seq.tabulate(CommitWidth){
      case last if (last == CommitWidth - 1) => true.B
      case i => !(Cat((i until (CommitWidth - 1)).map(j => m(j)(i))).orR)
    }))
    val has_same_before = WireInit(VecInit(Seq.tabulate(CommitWidth){
      case 0 => false.B
      case i => m(i - 1).orR()
    }))
    val times = WireInit(VecInit(Seq.tabulate(CommitWidth){
      case 0 => 0.U(log2Ceil(CommitWidth-1).W)
      case i => PopCount(m(i - 1))
    }))
    (is_last, has_same_before, times)
  }

  val (old_pdests_is_last, old_pdests_has_same_before, old_pdests_times) = getCompareResult(old_pdests_cmp)
  val (pdests_is_last, pdests_has_same_before, pdests_times) = getCompareResult(pdests_cmp)

  for (i <- 0 until CommitWidth) {
    XSDebug(p"decReq:${freeReq(i)},dec_old_pdst:${freePhyReg(i)},dec_is_me:${eliminatedMove(i)},dec_pdest:${multiRefPhyReg(i)}(isWalk:${walk})\n")

    val preg = freeRegCandidates(i) // physical register waiting for freeing

    oldPdestIsUnique(i) := old_pdests_is_last(i) && !old_pdests_has_same_before(i)
    oldPdestNotUniqueButLast(i) := old_pdests_is_last(i) && old_pdests_has_same_before(i)

    XSDebug(freeReq(i), p"port[$i]:old_pdest:${freePhyReg(i)},isUnique:${oldPdestIsUnique(i)},notUniqueButLast:${oldPdestNotUniqueButLast(i)}\n")

    pdestIsUnique(i) := pdests_is_last(i) && !pdests_has_same_before(i)
    pdestNotUniqueButLast(i) := pdests_is_last(i) && pdests_has_same_before(i)

    XSDebug(freeReq(i) && eliminatedMove(i), p"port[$i]:pdest:${multiRefPhyReg(i)},isUnique:${pdestIsUnique(i)},notUniqueButLast:${pdestNotUniqueButLast(i)}\n")

    freeVec(i) := ((oldPdestIsUnique(i) && (cmtCounter(preg) === Mux(updateSpecRefCounter(preg), specRefCounterNext(preg), specRefCounter(preg)))) 
      || (oldPdestNotUniqueButLast(i) && (cmtCounter(preg) + old_pdests_times(i) === Mux(updateSpecRefCounter(preg), specRefCounterNext(preg), specRefCounter(preg))))) && freeReq(i) && !walk


    updateCmtCounterVec(i) := freeReq(i) && (oldPdestIsUnique(i) || oldPdestNotUniqueButLast(i)) && !walk

    XSDebug(p"port[$i]cmtCounterInfo:plus_1=${cmtCounter(preg) + 1.U},plus_1_plus_times=${cmtCounter(preg) + 1.U + old_pdests_times(i)}\n")
    XSDebug(p"port[$i]cmtCounterCtl:plus_1=${(freeReq(i) && oldPdestIsUnique(i)).asBool()},plus_1_plus_times=${freeReq(i) && oldPdestNotUniqueButLast(i)},clear=${freeVec(i)}\n")


    updateArchRefCounterVec(i) := freeReq(i) && eliminatedMove(i) && (pdestIsUnique(i) || pdestNotUniqueButLast(i)) && !walk

    XSDebug((specRefCounter(preg) === 0.U) && freeVec(i), p"normal preg free, preg:${preg}\n")
    XSDebug((cmtCounter(preg) === specRefCounter(preg) && (specRefCounter(preg) =/= 0.U)) && freeVec(i), p"multi referenced preg free, preg:${preg}\n")


    decreaseSpecRefCounterVec(i) := freeReq(i) && eliminatedMove(i) && walk && (pdestIsUnique(i) || pdestNotUniqueButLast(i))
    decreaseSpecRefCounterValueVec(i) := pdests_times(i) + 1.U


    // write freed preg into free list at tail ptr
    val offset = i match {
      case 0 => 0.U
      case n => PopCount(freeVec.take(n))
    }
    val ptr = tailPtr + offset
    val idx = ptr.value
    when (freeVec(i)) {
      freeList(idx) := freeRegCandidates(i)
      XSDebug(p"[$i] Free List enqueue: [ preg ${freeRegCandidates(i)} ]\n")
    }
  }

  // set counters-update flag
  for (preg <- 0 until NRPhyRegs) {
    // set clear bit
    freeVec.zipWithIndex.foreach { case (ready, idx) => 
      when (ready && preg.U === freeRegCandidates(idx)) { 
        clearArchRefCounter(preg) := true.B
        clearSpecRefCounter(preg) := true.B
        clearCmtCounter(preg) := true.B
      } 
    }

    // set update bit
    updateCmtCounterVec.zipWithIndex.foreach { case (ready, idx) => 
      when (ready && preg.U === freeRegCandidates(idx)) {
        updateCmtCounter(preg) := true.B
        // cmt counter after incrementing/ stay not change
        // free vec has higher priority than cmtCounterNext, so normal free wouldn't cause cmtCounter increasing
        cmtCounterNext(preg) := Mux(freeReq(idx) && oldPdestIsUnique(idx), cmtCounter(preg) + 1.U,
                        Mux(freeReq(idx) && oldPdestNotUniqueButLast(idx), cmtCounter(preg) + 1.U + old_pdests_times(idx), 
                                                /* stay not change */ cmtCounter(preg)))
      }
    }

    updateArchRefCounterVec.zipWithIndex.foreach { case (ready, idx) => 
      when (ready && preg.U === multiRefPhyReg(idx)) {
        updateArchRefCounter(preg) := true.B
        // arch ref counter of pdest
        archRefCounterNext(multiRefPhyReg(idx)) := Mux(/* if this is me inst */freeReq(idx) && eliminatedMove(idx) && pdestIsUnique(idx), archRefCounter(multiRefPhyReg(idx)) + 1.U, 
          Mux(freeReq(idx) && eliminatedMove(idx) && pdestNotUniqueButLast(idx), archRefCounter(multiRefPhyReg(idx)) + 1.U + pdests_times(idx), archRefCounter(multiRefPhyReg(idx))))
      }
    }
  }


  /*
  Increments: from rename stage
   */
  val needAllocatingVec = WireInit(VecInit(Seq.fill(RenameWidth)(false.B)))
  XSDebug(needAllocatingVec.asUInt().orR(), p"needAllocatingVec:${Binary(needAllocatingVec.asUInt)}\n")

  val increaseSpecRefCounterVec = WireInit(VecInit(Seq.fill(RenameWidth)(false.B)))

  for (i <- 0 until RenameWidth) {
    // enqueue instr, isn't move elimination
    needAllocatingVec(i) := allocateReq(i) && canAllocate && doAllocate && !flush && !psrcOfMove(i).valid && !redirect && !walk

    // enqueue instr, is move elimination
    when (allocateReq(i) && canAllocate && doAllocate && !flush && psrcOfMove(i).valid && !redirect && !walk) {
      // specRefCounterNext(psrcOfMove(i).bits) := specRefCounter(psrcOfMove(i).bits) + 1.U
      // updateSpecRefCounter(psrcOfMove(i).bits) := true.B
      increaseSpecRefCounterVec(i) := true.B
    }

    val offset = i match {
      case 0 => 0.U
      case n => PopCount(needAllocatingVec.take(n))
    }
    val ptr = headPtr + offset
    allocatePhyReg(i) := freeList(ptr.value)
  }

  for (preg <- 0 until NRPhyRegs) {
    val increaseCmpVec = WireInit(VecInit(Seq.tabulate(RenameWidth)(i => increaseSpecRefCounterVec(i) && psrcOfMove(i).bits === preg.U)))
    val decreaseCmpVec = WireInit(VecInit(Seq.tabulate(CommitWidth)(i => decreaseSpecRefCounterVec(i) && freeRegCandidates(i) === preg.U)))

    val doIncrease = increaseCmpVec.asUInt.orR
    val doDecrease = decreaseCmpVec.asUInt.orR

    updateSpecRefCounter(preg) := doIncrease || doDecrease
    specRefCounterNext(preg) := specRefCounter(preg) + doIncrease.asUInt - Mux(doDecrease, decreaseSpecRefCounterValueVec(OHToUInt(decreaseCmpVec)), 0.U)
  }


  /*
  Flush: directly flush reference counter according to arch-rat
  - replace specRefCounter with archRefCounter; reset headPtr to [ tailPtr - (NRPhyRegs-32) - (archRefCounter(i) - cmtCounter(i)).reduce(_ + _) ]
   */


  // update tail pointer
  val tailPtrNext = Mux(walk, tailPtr, tailPtr + PopCount(freeVec))
  // update head pointer
  val dupRegVec = WireInit(VecInit(archRefCounter.zip(cmtCounter).map{ case (a, c) => a - c }))
  val headPtrNext = Mux(flush, tailPtr - (NRPhyRegs-32).U - dupRegVec.reduceTree(_ +& _), // FIXME Maybe this is too complicated?
                      Mux(walk, headPtr - PopCount(freeReq.zip(eliminatedMove).map{ case (rq, em) => rq && !em }), 
                      headPtr + PopCount(needAllocatingVec))) // when io.redirect is valid, needAllocatingVec is all-zero

  freeRegCnt := distanceBetween(tailPtrNext, headPtrNext)
  canAllocate := RegNext(freeRegCnt >= RenameWidth.U)

  headPtr := headPtrNext
  tailPtr := tailPtrNext

  // update reg counter
  for (i <- 0 until NRPhyRegs) {
    specRefCounter(i) := Mux(flush, archRefCounter(i), 
                           Mux(clearSpecRefCounter(i), 0.U, Mux(updateSpecRefCounter(i), specRefCounterNext(i), specRefCounter(i))))
    archRefCounter(i) :=   Mux(clearArchRefCounter(i), 0.U, Mux(updateArchRefCounter(i), archRefCounterNext(i), archRefCounter(i) ))
    cmtCounter(i)     :=   Mux(clearCmtCounter(i),     0.U, Mux(updateCmtCounter(i),     cmtCounterNext(i),     cmtCounter(i)     ))
  }


  /*
  Re-direct: restore by walking, handled by rename using `dec` port
   */



  /*
  Debug Info
   */
  for (i <- 0 until NRPhyRegs) {
    XSDebug(specRefCounter(i) =/= 0.U || archRefCounter(i) =/= 0.U || cmtCounter(i) =/= 0.U, 
      p"preg[$i] specRefCounter:${specRefCounter(i)} archRefCounter:${archRefCounter(i)} cmtCounter:${cmtCounter(i)}\n")
    XSDebug(specRefCounter(i) =/= 0.U || archRefCounter(i) =/= 0.U || cmtCounter(i) =/= 0.U, 
      p"preg[$i] specRefCounterNext:${specRefCounterNext(i)} archRefCounterNext:${archRefCounterNext(i)} cmtCounterNext:${cmtCounterNext(i)}\n")

    // specRefCounter(i) must >= cmtCounter(i)
    XSError(specRefCounter(i) < cmtCounter(i), p"Commits Overflow of preg${i}")
  }

  XSDebug(Array.range(0, MEFreeListSize).map(x => x.toString()).mkString("Free List (idx): ", "\t", "\n"))
  XSDebug(p"Free List (val): " + Array.range(0, MEFreeListSize).map(x => p"${freeList(x)}\t").reduceLeft(_ + _) + "\n")

  XSDebug(p"head:$headPtr tail:$tailPtr headPtrNext:$headPtrNext tailPtrNext:$tailPtrNext freeRegCnt:$freeRegCnt\n")

  XSDebug(p"flush ${flush} redirect ${redirect} walk ${walk}\n")

  XSDebug(PopCount(freeReq) =/= PopCount(freeVec), p"WARNING: Please check DEC requirement\n")
  XSDebug(PopCount(allocateReq) =/= PopCount(needAllocatingVec), p"WARNING: Please check INC requirement\n")


  /*
  Assertions
   */
  val enableFreeListCheck = false

  if (enableFreeListCheck) {

    for (i <- 0 until RenameWidth) {
      for (j <- (i + 1) until RenameWidth) {
        XSError(needAllocatingVec(i) && needAllocatingVec(j) && allocatePhyReg(i) === allocatePhyReg(j),
          p"Duplicate INC requirements detected!" + allocatePhyReg.zipWithIndex.map{case (p, idx) => p" ($idx):$p"}.reduceLeft(_ + _) + "\n")
        XSError(allocateReq(i) && allocateReq(j) && canAllocate && doAllocate && psrcOfMove(i).valid && psrcOfMove(j).valid && psrcOfMove(i).bits === psrcOfMove(j).bits,
          p"Duplicate ME requirements detected! Cannot inc same specRefCount in 1 cycle!\n")
      }
      // also, we cannot count ref numbers more than 3 (which is very rare)
      XSError(needAllocatingVec(i) && !psrcOfMove(i).valid && specRefCounter(allocatePhyReg(i)).andR(), p"(norm) Exceeding specRefCounter Max Value: preg[${allocatePhyReg(i)}]\n")
      XSError(allocateReq(i) && canAllocate && doAllocate && psrcOfMove(i).valid && specRefCounter(psrcOfMove(i).bits).andR(), p"(move) Exceeding specRefCounter Max Value: preg[${psrcOfMove(i).bits}]\n")
    }

    for (i <- 0 until CommitWidth) {
      // we cannot handle duplicate inc/dec requirements on a preg in 1 cycle for now
      for (j <- (i + 1) until CommitWidth) {
        XSInfo(freeReq(i) && freeReq(j) && freePhyReg(i) === freePhyReg(j), 
          p"Duplicate DEC requirements detected!" + freePhyReg.zipWithIndex.map{case (p, idx) => p" ($idx):$p"}.reduceLeft(_ + _) + "\n")
        XSInfo(freeReq(i) && freeReq(j) && eliminatedMove(i) && eliminatedMove(j) && multiRefPhyReg(i) === multiRefPhyReg(j), 
          p"Duplicate INC requirements on archRefCount detected!" + multiRefPhyReg.zipWithIndex.map{case (p, idx) => p" ($idx):$p"}.reduceLeft(_ + _) + "\n")
        XSError(freeVec(i) && freeVec(j) && freePhyReg(i) === freePhyReg(j), "Fatal Error: free 1 reg 2 times in 1 cycle!\n")
      }
      // not inc and dec same reg in 1 cycle
      for (j <- 0 until RenameWidth) {
        XSDebug(allocateReq(j) && canAllocate && doAllocate && psrcOfMove(j).valid && !redirect && !walk &&
          freeReq(i) && freePhyReg(i) === allocatePhyReg(j), p"INC and DEC Conflict Detected! inc($j): preg ${allocatePhyReg(j)}, dec($i): preg ${freePhyReg(i)}\n")
      }
    }
  }
}
