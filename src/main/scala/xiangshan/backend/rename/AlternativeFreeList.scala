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

package xiangshan.backend.rename

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.rename._
import utils._

class IntFreeListPtr(implicit val p: Parameters) extends CircularQueuePtr[IntFreeListPtr](
  p => p(XSCoreParamsKey).NRPhyRegs // TODO depends on size of free list
)

object IntFreeListPtr {
  def apply(f: Bool, v:UInt)(implicit p: Parameters): IntFreeListPtr = {
    val ptr = Wire(new IntFreeListPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class AlternativeFreeList(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {

    val flush = Input(Bool())
    val redirect = Input(Bool())
    val walk = Input(Bool())

    // increase physical registers reference count (rename)
    val inc = new Bundle {
      // need to increase reference count (not actually do the increment)
      val req = Vec(RenameWidth, Input(Bool()))

      // have enough free registers (>= RenameWidth)
      val canInc = Output(Bool())
      // prepared pdest according to req
      val pdests = Vec(RenameWidth, Output(UInt(PhyRegIdxWidth.W)))

      // actually do the increment
      val doInc = Input(Bool())
      // psrc of move instructions ready for elimination
      val psrcOfMove = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    }

    // decrease physical registers reference count (commit or walk/redirect/recover)
    val dec = new Bundle {
      // instruction need commit/redirect
      val req = Vec(CommitWidth, Input(Bool()))
      // free old p_dest reg
      val old_pdests = Vec(CommitWidth, Input(UInt(PhyRegIdxWidth.W)))
      // instruction fits move elimination
      val eliminatedMove = Vec(CommitWidth, Input(Bool()))
      // for eliminated move instruction, increase arch ref count of (new) p_dest reg
      val pdests = Vec(CommitWidth, Input(UInt(PhyRegIdxWidth.W)))
    }

    // max vector from speculative reference counter
    val maxVec = Vec(NRPhyRegs, Output(Bool()))
  })

  val FL_SIZE = NRPhyRegs // TODO calculate max number of free list using NRPhyRegs and width of counter
  val COUNTER_WIDTH = 2.W // width of reference counters below

  // recording referenced times of each physical registers
  val archRefCounter = RegInit(VecInit(Seq.fill(NRPhyRegs)(0.U(COUNTER_WIDTH))))
  val specRefCounter = RegInit(VecInit(Seq.fill(NRPhyRegs)(0.U(COUNTER_WIDTH))))
  val cmtCounter = RegInit(VecInit(Seq.fill(NRPhyRegs)(0.U(COUNTER_WIDTH))))

  val archRefCounterNext = Wire(Vec(NRPhyRegs, UInt(COUNTER_WIDTH)))
  archRefCounterNext.foreach(_ := DontCare)
  val updateArchRefCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B)))
  val clearArchRefCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B)))

  val specRefCounterNext = Wire(Vec(NRPhyRegs, UInt(COUNTER_WIDTH)))
  specRefCounterNext.foreach(_ := DontCare)
  val updateSpecRefCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B))) // update with xxxNext
  val clearSpecRefCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B))) // reset to zero

  val cmtCounterNext = Wire(Vec(NRPhyRegs, UInt(COUNTER_WIDTH)))
  cmtCounterNext.foreach(_ := DontCare)
  val updateCmtCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B)))
  val clearCmtCounter = WireInit(VecInit(Seq.fill(NRPhyRegs)(false.B)))

  // send max flag of spec ref counter to rename stage
  io.maxVec zip specRefCounter foreach { case (max, cnt) =>
    max := cnt.andR()
  }


  // number of free registers
  val freeRegCnt = Wire(UInt())

  // free list as circular buffer
  val freeList = RegInit(VecInit(Seq.tabulate(FL_SIZE){
    case n if (n >= 0 && n < NRPhyRegs - 32) => (n + 32).U
    case _ => DontCare
  }))

  // head and tail pointer
  val headPtr = RegInit(IntFreeListPtr(false.B, 0.U))

  val tailPtr = RegInit(IntFreeListPtr(false.B, (NRPhyRegs-32).U)) // TODO change 128 into parameters


  /*
  Decrements: from roq commits
   */
  val freeVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B))) // if dec(i).bits is freed and ready for writing back to free list
  val freeRegCandidates = (0 until CommitWidth).map(io.dec.old_pdests(_))

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
      io.dec.req(i) && io.dec.req(j) && io.dec.old_pdests(i) === io.dec.old_pdests(j)
    }).reverse)
    pdests_cmp(i - 1) := Cat((0 until i).map(j => {
      io.dec.req(i) && io.dec.req(j) && io.dec.eliminatedMove(i) && io.dec.eliminatedMove(j) && io.dec.pdests(i) === io.dec.pdests(j)
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
    XSDebug(p"decReq:${io.dec.req(i)},dec_old_pdst:${io.dec.old_pdests(i)},dec_is_me:${io.dec.eliminatedMove(i)},dec_pdest:${io.dec.pdests(i)}(isWalk:${io.walk})\n")

    val preg = freeRegCandidates(i) // physical register waiting for freeing

    oldPdestIsUnique(i) := old_pdests_is_last(i) && !old_pdests_has_same_before(i)
    oldPdestNotUniqueButLast(i) := old_pdests_is_last(i) && old_pdests_has_same_before(i)

    XSDebug(io.dec.req(i), p"port[$i]:old_pdest:${io.dec.old_pdests(i)},isUnique:${oldPdestIsUnique(i)},notUniqueButLast:${oldPdestNotUniqueButLast(i)}\n")

    pdestIsUnique(i) := pdests_is_last(i) && !pdests_has_same_before(i)
    pdestNotUniqueButLast(i) := pdests_is_last(i) && pdests_has_same_before(i)

    XSDebug(io.dec.req(i) && io.dec.eliminatedMove(i), p"port[$i]:pdest:${io.dec.pdests(i)},isUnique:${pdestIsUnique(i)},notUniqueButLast:${pdestNotUniqueButLast(i)}\n")

    freeVec(i) := ((oldPdestIsUnique(i) && (cmtCounter(preg) === Mux(updateSpecRefCounter(preg), specRefCounterNext(preg), specRefCounter(preg)))) 
      || (oldPdestNotUniqueButLast(i) && (cmtCounter(preg) + old_pdests_times(i) === Mux(updateSpecRefCounter(preg), specRefCounterNext(preg), specRefCounter(preg))))) && io.dec.req(i) && !io.walk


    updateCmtCounterVec(i) := io.dec.req(i) && (oldPdestIsUnique(i) || oldPdestNotUniqueButLast(i)) && !io.walk

    XSDebug(p"port[$i]cmtCounterInfo:plus_1=${cmtCounter(preg) + 1.U},plus_1_plus_times=${cmtCounter(preg) + 1.U + old_pdests_times(i)}\n")
    XSDebug(p"port[$i]cmtCounterCtl:plus_1=${(io.dec.req(i) && oldPdestIsUnique(i)).asBool()},plus_1_plus_times=${io.dec.req(i) && oldPdestNotUniqueButLast(i)},clear=${freeVec(i)}\n")


    updateArchRefCounterVec(i) := io.dec.req(i) && io.dec.eliminatedMove(i) && (pdestIsUnique(i) || pdestNotUniqueButLast(i)) && !io.walk

    XSDebug((specRefCounter(preg) === 0.U) && freeVec(i), p"normal preg free, preg:${preg}\n")
    XSDebug((cmtCounter(preg) === specRefCounter(preg) && (specRefCounter(preg) =/= 0.U)) && freeVec(i), p"multi referenced preg free, preg:${preg}\n")


    decreaseSpecRefCounterVec(i) := io.dec.req(i) && io.dec.eliminatedMove(i) && io.walk && (pdestIsUnique(i) || pdestNotUniqueButLast(i))
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
        cmtCounterNext(preg) := Mux(io.dec.req(idx) && oldPdestIsUnique(idx), cmtCounter(preg) + 1.U,
                        Mux(io.dec.req(idx) && oldPdestNotUniqueButLast(idx), cmtCounter(preg) + 1.U + old_pdests_times(idx), 
                                                /* stay not change */ cmtCounter(preg)))
      }
    }

    updateArchRefCounterVec.zipWithIndex.foreach { case (ready, idx) => 
      when (ready && preg.U === io.dec.pdests(idx)) {
        updateArchRefCounter(preg) := true.B
        // arch ref counter of pdest
        archRefCounterNext(io.dec.pdests(idx)) := Mux(/* if this is me inst */io.dec.req(idx) && io.dec.eliminatedMove(idx) && pdestIsUnique(idx), archRefCounter(io.dec.pdests(idx)) + 1.U, 
          Mux(io.dec.req(idx) && io.dec.eliminatedMove(idx) && pdestNotUniqueButLast(idx), archRefCounter(io.dec.pdests(idx)) + 1.U + pdests_times(idx), archRefCounter(io.dec.pdests(idx))))
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
    io.inc.pdests(i) := DontCare
    // enqueue instr, isn't move elimination
    needAllocatingVec(i) := io.inc.req(i) && io.inc.canInc && io.inc.doInc && !io.flush && !io.inc.psrcOfMove(i).valid && !io.redirect && !io.walk

    // enqueue instr, is move elimination
    when (io.inc.req(i) && io.inc.canInc && io.inc.doInc && !io.flush && io.inc.psrcOfMove(i).valid && !io.redirect && !io.walk) {
      // specRefCounterNext(io.inc.psrcOfMove(i).bits) := specRefCounter(io.inc.psrcOfMove(i).bits) + 1.U
      // updateSpecRefCounter(io.inc.psrcOfMove(i).bits) := true.B
      increaseSpecRefCounterVec(i) := true.B
    }

    val offset = i match {
      case 0 => 0.U
      case n => PopCount(needAllocatingVec.take(n))
    }
    val ptr = headPtr + offset
    when (needAllocatingVec(i)) {
      val pdest = freeList(ptr.value)
      XSDebug(p"[$i] Allocate phy reg $pdest\n")
      io.inc.pdests(i) := pdest
    }
  }

  for (preg <- 0 until NRPhyRegs) {
    val increaseCmpVec = WireInit(VecInit(Seq.tabulate(RenameWidth)(i => increaseSpecRefCounterVec(i) && io.inc.psrcOfMove(i).bits === preg.U)))
    val decreaseCmpVec = WireInit(VecInit(Seq.tabulate(CommitWidth)(i => decreaseSpecRefCounterVec(i) && freeRegCandidates(i) === preg.U)))

    val doIncrease = increaseCmpVec.asUInt.orR
    val doDecrease = decreaseCmpVec.asUInt.orR

    updateSpecRefCounter(preg) := doIncrease || doDecrease
    specRefCounterNext(preg) := specRefCounter(preg) + doIncrease.asUInt - Mux(doDecrease, decreaseSpecRefCounterValueVec(OHToUInt(decreaseCmpVec)), 0.U)
  }


  /*
  Flush: directly flush reference counter according to arch-rat
  - replace specRefCounter with archRefCounter; reset headPtr to [ tailPtr - (NRPhyRegs-32) - archRefCounter.reduce(_ + _) ]
   */


  // update tail pointer
  val tailPtrNext = Mux(io.walk, tailPtr, tailPtr + PopCount(freeVec))
  // update head pointer
  val headPtrNext = Mux(io.flush, tailPtr - (NRPhyRegs-32).U - archRefCounter.reduceTree(_ + _), // FIXME Maybe this is too complicated?
                      Mux(io.walk, headPtr - PopCount(io.dec.req.zip(io.dec.eliminatedMove).map{ case (rq, em) => rq && !em }), 
                      headPtr + PopCount(needAllocatingVec))) // when io.redirect is valid, needAllocatingVec is all-zero

  freeRegCnt := distanceBetween(tailPtrNext, headPtrNext)
  io.inc.canInc := RegNext(freeRegCnt >= RenameWidth.U)

  headPtr := headPtrNext
  tailPtr := tailPtrNext

  // update reg counter
  for (i <- 0 until NRPhyRegs) {
    specRefCounter(i) := Mux(io.flush, archRefCounter(i), 
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

  XSDebug(Array.range(0, FL_SIZE).map(x => x.toString()).mkString("Free List (idx): ", "\t", "\n"))
  XSDebug(p"Free List (val): " + Array.range(0, FL_SIZE).map(x => p"${freeList(x)}\t").reduceLeft(_ + _) + "\n")

  XSDebug(p"head:$headPtr tail:$tailPtr headPtrNext:$headPtrNext tailPtrNext:$tailPtrNext freeRegCnt:$freeRegCnt\n")

  XSDebug(p"io.flush ${io.flush} io.redirect ${io.redirect} io.walk ${io.walk}\n")

  XSDebug(PopCount(io.dec.req) =/= PopCount(freeVec), p"WARNING: Please check DEC requirement\n")
  XSDebug(PopCount(io.inc.req) =/= PopCount(needAllocatingVec), p"WARNING: Please check INC requirement\n")


  /*
  Assertions
   */
  val enableFreeListCheck = false

  if (enableFreeListCheck) {

    for (i <- 0 until RenameWidth) {
      for (j <- (i + 1) until RenameWidth) {
        XSError(needAllocatingVec(i) && needAllocatingVec(j) && io.inc.pdests(i) === io.inc.pdests(j),
          p"Duplicate INC requirements detected!" + io.inc.pdests.zipWithIndex.map{case (p, idx) => p" ($idx):$p"}.reduceLeft(_ + _) + "\n")
        XSError(io.inc.req(i) && io.inc.req(j) && io.inc.canInc && io.inc.doInc && io.inc.psrcOfMove(i).valid && io.inc.psrcOfMove(j).valid && io.inc.psrcOfMove(i).bits === io.inc.psrcOfMove(j).bits,
          p"Duplicate ME requirements detected! Cannot inc same specRefCount in 1 cycle!\n")
      }
      // also, we cannot count ref numbers more than 3 (which is very rare)
      XSError(needAllocatingVec(i) && !io.inc.psrcOfMove(i).valid && specRefCounter(io.inc.pdests(i)).andR(), p"(norm) Exceeding specRefCounter Max Value: preg[${io.inc.pdests(i)}]\n")
      XSError(io.inc.req(i) && io.inc.canInc && io.inc.doInc && io.inc.psrcOfMove(i).valid && specRefCounter(io.inc.psrcOfMove(i).bits).andR(), p"(move) Exceeding specRefCounter Max Value: preg[${io.inc.psrcOfMove(i).bits}]\n")
    }

    for (i <- 0 until CommitWidth) {
      // we cannot handle duplicate inc/dec requirements on a preg in 1 cycle for now
      for (j <- (i + 1) until CommitWidth) {
        XSInfo(io.dec.req(i) && io.dec.req(j) && io.dec.old_pdests(i) === io.dec.old_pdests(j), 
          p"Duplicate DEC requirements detected!" + io.dec.old_pdests.zipWithIndex.map{case (p, idx) => p" ($idx):$p"}.reduceLeft(_ + _) + "\n")
        XSInfo(io.dec.req(i) && io.dec.req(j) && io.dec.eliminatedMove(i) && io.dec.eliminatedMove(j) && io.dec.pdests(i) === io.dec.pdests(j), 
          p"Duplicate INC requirements on archRefCount detected!" + io.dec.pdests.zipWithIndex.map{case (p, idx) => p" ($idx):$p"}.reduceLeft(_ + _) + "\n")
        XSError(freeVec(i) && freeVec(j) && io.dec.old_pdests(i) === io.dec.old_pdests(j), "Fatal Error: free 1 reg 2 times in 1 cycle!\n")
      }
      // not inc and dec same reg in 1 cycle
      for (j <- 0 until RenameWidth) {
        XSDebug(io.inc.req(j) && io.inc.canInc && io.inc.doInc && io.inc.psrcOfMove(j).valid && !io.redirect && !io.walk &&
          io.dec.req(i) && io.dec.old_pdests(i) === io.inc.pdests(j), p"INC and DEC Conflict Detected! inc($j): preg ${io.inc.pdests(j)}, dec($i): preg ${io.dec.old_pdests(i)}\n")
      }
    }
  }
}