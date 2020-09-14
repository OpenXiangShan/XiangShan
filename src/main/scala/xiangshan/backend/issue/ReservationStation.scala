package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.{Exu, ExuConfig}
import xiangshan.backend.rename.FreeListPtr
import utils._
import xiangshan.backend.fu.FunctionUnit._
import xiangshan.backend.regfile.RfReadPort


trait HasIQConst extends HasXSParameter{
  val iqSize = IssQueSize
  val iqIdxWidth = log2Up(iqSize)
}

object OneCycleFire {
  def apply(fire: Bool) = {
    val valid = RegInit(false.B)
    when (valid) { valid := false.B }
    when (fire) { valid := true.B }
    valid
  }
}

class ReservationStation
(
  val exuCfg: ExuConfig,
  val wakeupCnt: Int,
  val bypassCnt: Int = 0,
  val enableBypass: Boolean = false,
  val fifo: Boolean = false
) extends XSModule with HasIQConst {

  val src2Use = true
  val src3Use = (exuCfg.intSrcCnt > 2) || (exuCfg.fpSrcCnt > 2)
  val src2Listen = true
  val src3Listen = (exuCfg.intSrcCnt > 2) || (exuCfg.fpSrcCnt > 2)

  val io = IO(new Bundle() {
    // flush Issue Queue
    val redirect = Flipped(ValidIO(new Redirect))

    // enq Ctrl sigs at dispatch-2
    val enqCtrl = Flipped(DecoupledIO(new MicroOp))
    // enq Data at next cycle (regfile has 1 cycle latency)
    val enqData = Input(new ExuInput)

    //  broadcast selected uop to other issue queues which has bypasses
    val selectedUop = if(enableBypass) ValidIO(new MicroOp) else null

    // send to exu
    val deq = DecoupledIO(new ExuInput)

    // listen to write back bus
    val wakeUpPorts = Vec(wakeupCnt, Flipped(ValidIO(new ExuOutput)))

    // use bypass uops to speculative wake-up
    val bypassUops = Vec(bypassCnt, Flipped(ValidIO(new MicroOp)))
    val bypassData = Vec(bypassCnt, Flipped(ValidIO(new ExuOutput)))

    // to Dispatch
    val numExist = Output(UInt(iqIdxWidth.W))
  })

  val srcAllNum = 3
  val srcUseNum = 1 + (if(src2Use) 1 else 0) + (if(src3Use) 1 else 0)// when src2Use is false, then src3Use must be false
  val srcListenNum = 1 + (if(src2Listen) 1 else 0) + (if(src3Listen) 1 else 0) // when src2Listen is false, then src3Listen must be false
  // when use is false, Listen must be false
  require(!(!src2Use && src2Listen))
  require(!(!src3Use && src3Listen))
  require(!(!src2Use && src3Use))
  require(!(!src2Listen && src3Listen))

  // Issue Queue
  // val issQue = IndexableMem(iqSize, new ExuInput, mem = false, init = None)
  val issQue = Mem(iqSize, new ExuInput)
  // val issQue = Reg(Vec(iqSize, new ExuInput))
  val validQue = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val idQue = RegInit(VecInit((0 until iqSize).map(_.U(iqIdxWidth.W))))
  val idValidQue = VecInit((0 until iqSize).map(i => validQue(idQue(i)))).asUInt
  val tailAll = RegInit(0.U((iqIdxWidth+1).W))
  val tail = tailAll(iqIdxWidth-1, 0)
  val full = tailAll(iqIdxWidth)

  // alias failed, turn to independent storage(Reg)
  val psrc = VecInit(List.tabulate(iqSize)(i => VecInit(List(issQue(i.U).uop.psrc1, issQue(i.U).uop.psrc2, issQue(i.U).uop.psrc3)))) // NOTE: indexed by IssQue's idx
  val srcRdyVec = Reg(Vec(iqSize, Vec(srcAllNum, Bool()))) // NOTE: indexed by IssQue's idx
  val srcData = Reg(Vec(iqSize, Vec(srcAllNum, UInt(XLEN.W)))) // NOTE: indexed by IssQue's idx
  val srcRdy = VecInit(srcRdyVec.map(a => if(src3Listen) { if(src2Listen) a(0)&&a(1)&&a(2) else a(0)&&a(2) } else  { if(src2Listen) a(0)&&a(1) else a(0) }))// NOTE: indexed by IssQue's idx
  val srcIdRdy = VecInit((0 until iqSize).map(i => srcRdy(idQue(i)))).asUInt // NOTE: indexed by IdQue's idx
  val srcType = List.tabulate(iqSize)(i => List(issQue(i).uop.ctrl.src1Type, issQue(i).uop.ctrl.src2Type, issQue(i).uop.ctrl.src3Type)) // NOTE: indexed by IssQue's idx

  // val srcDataWire = Wire(srcData)
  val srcDataWire = Wire(Vec(iqSize, Vec(srcAllNum, UInt(XLEN.W)))) // NOTE: indexed by IssQue's idx
  srcDataWire := srcData
  srcData := srcDataWire

  // there are three stages
  // |-------------|--------------------|--------------|
  // |Enq:get state|Deq: select/get data| fire stage   |
  // |-------------|--------------------|--------------|

  //-----------------------------------------
  // Enqueue
  //-----------------------------------------
  val enqRedHit = Wire(Bool())
  val enqFire = io.enqCtrl.fire() && !enqRedHit
  val deqFire = io.deq.fire()
  val popOne = Wire(Bool())
  io.enqCtrl.ready := !full || popOne
  val enqSelIq = Wire(UInt(iqIdxWidth.W))
  val enqSrcRdy = List(Mux(SrcType.isPcImm(io.enqCtrl.bits.ctrl.src1Type), true.B, io.enqCtrl.bits.src1State === SrcState.rdy),
                       Mux(SrcType.isPcImm(io.enqCtrl.bits.ctrl.src2Type), true.B, io.enqCtrl.bits.src2State === SrcState.rdy),
                       Mux(SrcType.isPcImm(io.enqCtrl.bits.ctrl.src3Type), true.B, io.enqCtrl.bits.src3State === SrcState.rdy))

  // state enq
  when (enqFire) {
    issQue(enqSelIq).uop := io.enqCtrl.bits
    validQue(enqSelIq) := true.B
    assert(!validQue(enqSelIq) || popOne/* && idQue(deqSel)===enqSelIq*/)

    srcRdyVec(enqSelIq)(0) := enqSrcRdy(0)
    if(src2Listen) { srcRdyVec(enqSelIq)(1) := enqSrcRdy(1) }
    if(src3Listen) { srcRdyVec(enqSelIq)(2) := enqSrcRdy(2) }
  }

  // data enq
  val enqSelIqNext = RegEnable(enqSelIq, enqFire)
  // val enqSelIqNext = RegNext(enqSelIq)
  val enqFireNext = RegInit(false.B)
  when (enqFireNext) { enqFireNext := false.B }
  when (enqFire) { enqFireNext := true.B }

  val enqDataVec = List(io.enqData.src1, io.enqData.src2, io.enqData.src3)
  when (enqFireNext) {
    for(i <- 0 until srcUseNum) {
      srcDataWire(enqSelIqNext)(i) := enqDataVec(i)
    }
  }

  //-----------------------------------------
  // tail
  //-----------------------------------------
  val tailInc = enqFire
  val tailDec = popOne
  val tailKeep = tailInc === tailDec
  val tailAdd = tailAll + 1.U
  val tailSub = tailAll - 1.U
  tailAll := Mux(tailKeep, tailAll, Mux(tailInc, tailAdd, tailSub))
  assert(tailAll < 9.U)
  // Select to Dequeue
  val deqSel = if (fifo) 0.U else PriorityEncoder(idValidQue & srcIdRdy) //may not need idx, just need oneHot, idx by IdQue's idx
  val deqSelIq = idQue(deqSel)
  val deqSelOH = PriorityEncoderOH(idValidQue & srcIdRdy)
  val has1Rdy = if (fifo) idValidQue(deqSel) && srcIdRdy(deqSel) else ParallelOR((validQue.asUInt & srcRdy.asUInt).asBools).asBool()

  //-----------------------------------------
  // idQue Move
  //-----------------------------------------
  def UIntToMHP(in: UInt) = {
    // UInt to Multi-Hot plus 1: 1.U -> "11".U; 2.U(2.W) -> "0111".U; 3.U(3.W) -> "00001111".W
    val a = Seq.fill(in.getWidth)(2).product
    val s = (1 << (a-1)).S
    Reverse((s(a-1,0).asSInt >> in)(a-1,0).asUInt)
  }
  def UIntToMH(in: UInt) = {
    val a = Seq.fill(in.getWidth)(2).product
    val s = (1 << (a-1)).S
    Reverse((s(a-1,0).asSInt >> in)(a-1,0).asUInt) ^ UIntToOH(in)
  }
  def PriorityDot(in: UInt) = {
    // "1100".U -> "0111".U; "1010".U -> "0011".U; "0000".U -> "0000".U
    val a = Array.fill(iqSize)(1)
    for(i <- 1 until in.getWidth) {
      a(i) = a(i-1)*2 + 1
    }
    Mux(in===0.U, 0.U(in.getWidth.W), PriorityMux(in, a.map(_.U(in.getWidth.W))))
  }
  val tailDot = Mux(full, VecInit(Seq.fill(iqSize)(true.B)).asUInt, UIntToMHP(tail))
  val tailDot2 = Mux(full, VecInit(Seq.fill(iqSize)(true.B)).asUInt, UIntToMH(tail))
  val selDot = UIntToMHP(deqSel) // FIXIT: PriorityEncoder -> UIntToMHP means long latency
  val nonValid = ~(idValidQue | ~tailDot2)
  val popSel = PriorityEncoder(nonValid) // Note: idxed by IDque's index
  val popDot = PriorityDot(nonValid)
  val isPop = ParallelOR(nonValid.asBools).asBool()
  val moveDot = Mux(isPop, tailDot ^ popDot, tailDot ^ selDot)

  assert(!(popOne&&moveDot(0)))
  when (popOne) {
    for(i <- 1 until iqSize) {
      when (moveDot(i)) { idQue(i-1) := idQue(i) }
    }
    val ptr_tmp = Mux(full, VecInit(Seq.fill(iqIdxWidth)(true.B)).asUInt, tail)
    idQue(ptr_tmp) := idQue(Mux(isPop, popSel, deqSel))
  }
  assert(ParallelAND(List.tabulate(iqSize)(i => ParallelOR(List.tabulate(iqSize)(j => i.U === idQue(j))))).asBool)

  //-----------------------------------------
  // Redirect
  //-----------------------------------------
  // redirect enq
  enqRedHit := io.redirect.valid && io.enqCtrl.bits.needFlush(io.redirect)

  // redirect issQue
  val redHitVec = List.tabulate(iqSize)(i => issQue(i).uop.needFlush(io.redirect))
  for (i <- validQue.indices) {
    when (redHitVec(i) && validQue(i)) {
      validQue(i) := false.B
    }
  }
  // reditect deq(issToExu)
  val redIdHitVec = List.tabulate(iqSize)(i => issQue(idQue(i)).uop.needFlush(io.redirect))
  val selIsRed = ParallelOR((deqSelOH & VecInit(redIdHitVec).asUInt).asBools).asBool

  //-----------------------------------------
  // Dequeue (or to Issue Stage)
  //-----------------------------------------
  val issueToExu = Reg(new ExuInput)
  val issueToExuValid = RegInit(false.B)
  val deqFlushHit = issueToExu.uop.needFlush(io.redirect)
  val deqCanIn = !issueToExuValid || io.deq.ready || deqFlushHit
  
  val toIssFire = deqCanIn && has1Rdy && !isPop && !selIsRed
  popOne := deqCanIn && (has1Rdy || isPop) // send a empty or valid term to issueStage

  when (toIssFire) {
    issueToExu := issQue(deqSelIq)
    issueToExuValid := true.B
    validQue(deqSelIq) := enqFire && enqSelIq===deqSelIq
    assert(validQue(deqSelIq))
    issueToExu.src1 := srcDataWire(deqSelIq)(0)
    if (src2Use) { issueToExu.src2 := srcDataWire(deqSelIq)(1) } else { issueToExu.src2 := DontCare }
    if (src3Use) { issueToExu.src3 := srcDataWire(deqSelIq)(2) } else { issueToExu.src3 := DontCare }
  }
  when ((deqFire || deqFlushHit) && !toIssFire) {
    issueToExuValid := false.B
  }

  io.deq.valid := issueToExuValid && !deqFlushHit
  io.deq.bits := issueToExu

  enqSelIq := Mux(full,
    Mux(isPop,
      idQue(popSel),
      deqSelIq
    ),
    idQue(tail)
  ) // Note: direct by IQue's idx, different from deqSel

  io.numExist := Mux(tailAll === iqSize.U, (iqSize-1).U, tailAll)
  assert(tailAll < 9.U)

  //-----------------------------------------
  // Issue with No Delay
  //-----------------------------------------
  // when enq is ready && no other rdy && no pop &&  fireStage is ready && no flush
  // send out directly without store the data
  val enqAlreadyRdy = if(src3Listen) { if(src2Listen) enqSrcRdy(0)&&enqSrcRdy(1)&&enqSrcRdy(2) else enqSrcRdy(0)&&enqSrcRdy(2) } else  { if(src2Listen) enqSrcRdy(0)&&enqSrcRdy(1) else enqSrcRdy(0) }
  val enqALRdyNext = OneCycleFire(enqAlreadyRdy && enqFire)
  val enqSendFlushHit = issQue(enqSelIqNext).uop.needFlush(io.redirect)
  val enqSendEnable = if(fifo) { RegNext(tailAll===0.U) && enqALRdyNext && (!issueToExuValid || deqFlushHit) && (enqSelIqNext === deqSelIq) && !isPop && !enqSendFlushHit/* && has1Rdy*//* && io.deq.ready*/ } else { enqALRdyNext && (!issueToExuValid || deqFlushHit) && (enqSelIqNext === deqSelIq) && !isPop && !enqSendFlushHit/* && has1Rdy*//* && io.deq.ready*/ } // FIXME: has1Rdy has combination loop
  when (enqSendEnable) {
    io.deq.valid := true.B
    io.deq.bits := issQue(enqSelIqNext)
    io.deq.bits.src1 := enqDataVec(0)
    if (src2Use) { io.deq.bits.src2 := enqDataVec(1) }
    if (src3Use) { io.deq.bits.src3 := enqDataVec(2) }
    issueToExuValid := false.B
    when (!io.deq.ready) { // if Func Unit is not ready, store it to FireStage
      issueToExuValid := true.B
    }
  }

  //-----------------------------------------
  // Wakeup and Bypass
  //-----------------------------------------
    val cdbValid = io.wakeUpPorts.map(_.valid)
    val cdbData  = io.wakeUpPorts.map(_.bits.data)
    val cdbPdest = io.wakeUpPorts.map(_.bits.uop.pdest)
    val cdbrfWen = io.wakeUpPorts.map(_.bits.uop.ctrl.rfWen)
    val cdbfpWen = io.wakeUpPorts.map(_.bits.uop.ctrl.fpWen)

    for(i <- idQue.indices) { // Should be IssQue.indices but Mem() does not support
      for(j <- 0 until srcListenNum) {
        val hitVec = cdbValid.indices.map(k => psrc(i)(j) === cdbPdest(k) && cdbValid(k) && (srcType(i)(j)===SrcType.reg && cdbrfWen(k) || srcType(i)(j)===SrcType.fp && cdbfpWen(k)))
        val hit = ParallelOR(hitVec).asBool
        val data = ParallelMux(hitVec zip cdbData)
        when (validQue(i) && !srcRdyVec(i)(j) && hit) { 
          srcDataWire(i)(j) := data
          srcRdyVec(i)(j) := true.B
        }
        // XSDebug(validQue(i) && !srcRdyVec(i)(j) && hit, "WakeUp: Sel:%d Src:(%d|%d) Rdy:%d Hit:%d HitVec:%b Data:%x\n", i.U, j.U, psrc(i)(j), srcRdyVec(i)(j), hit, VecInit(hitVec).asUInt, data)
        for (k <- cdbValid.indices) {
          XSDebug(validQue(i) && !srcRdyVec(i)(j) && hit && hitVec(k), "WakeUpHit: IQIdx:%d Src%d:%d Ports:%d Data:%x Pc:%x RoqIdx:%x\n", i.U, j.U, psrc(i)(j), k.U, cdbData(k), io.wakeUpPorts(k).bits.uop.cf.pc, io.wakeUpPorts(k).bits.uop.roqIdx)
        }
      }
    }
  
    val bpPdest = io.bypassUops.map(_.bits.pdest)
    val bpValid = io.bypassUops.map(_.valid)
    val bpData  = io.bypassData.map(_.bits.data)
    val bprfWen = io.bypassUops.map(_.bits.ctrl.rfWen)
    val bpfpWen = io.bypassUops.map(_.bits.ctrl.fpWen)

    for (i <- idQue.indices) { // Should be IssQue.indices but Mem() does not support
      for (j <- 0 until srcListenNum) {
        val hitVec = bpValid.indices.map(k => psrc(i)(j) === bpPdest(k) && bpValid(k) && (srcType(i)(j)===SrcType.reg && bprfWen(k) || srcType(i)(j)===SrcType.fp && bpfpWen(k)))
        val hitVecNext = hitVec.map(RegNext(_))
        val hit = ParallelOR(hitVec).asBool
        when (validQue(i) && !srcRdyVec(i)(j) && hit) {
          srcRdyVec(i)(j) := true.B
        }
        when (RegNext(validQue(i) && !srcRdyVec(i)(j) && hit)) {
          srcDataWire(i)(j) := PriorityMux(hitVecNext zip bpData)
        }
        // XSDebug(validQue(i) && !srcRdyVec(i)(j) && hit, "BypassCtrl: Sel:%d Src:(%d|%d) Rdy:%d Hit:%d HitVec:%b\n", i.U, j.U, psrc(i)(j), srcRdyVec(i)(j), hit, VecInit(hitVec).asUInt)
        for (k <- bpValid.indices) {
          XSDebug(validQue(i) && !srcRdyVec(i)(j) && hit && hitVec(k), "BypassCtrlHit: IQIdx:%d Src%d:%d Ports:%d Pc:%x RoqIdx:%x\n", i.U, j.U, psrc(i)(j), k.U, io.bypassUops(k).bits.cf.pc, io.bypassUops(k).bits.roqIdx)
        }
        // XSDebug(RegNext(validQue(i) && !srcRdyVec(i)(j) && hit), "BypassData: Sel:%d Src:(%d|%d) HitVecNext:%b Data:%x (for last cycle's Ctrl)\n", i.U, j.U, psrc(i)(j), VecInit(hitVecNext).asUInt, ParallelMux(hitVecNext zip bpData))
        for (k <- bpValid.indices) {
          XSDebug(RegNext(validQue(i) && !srcRdyVec(i)(j) && hit && hitVec(k)),
            "BypassDataHit: IQIdx:%d Src%d:%d Ports:%d Data:%x Pc:%x RoqIdx:%x\n",
            i.U, j.U, psrc(i)(j), k.U, bpData(k), io.bypassUops(k).bits.cf.pc, io.bypassUops(k).bits.roqIdx)
        }
      }
    }

    // Enqueue Bypass
    val enqCtrl = io.enqCtrl
    val enqPsrc = List(enqCtrl.bits.psrc1, enqCtrl.bits.psrc2, enqCtrl.bits.psrc3)
    val enqSrcType = List(enqCtrl.bits.ctrl.src1Type, enqCtrl.bits.ctrl.src2Type, enqCtrl.bits.ctrl.src3Type)
    for (i <- 0 until srcListenNum) {
      val hitVec = bpValid.indices.map(j => enqPsrc(i)===bpPdest(j) && bpValid(j) && (enqSrcType(i)===SrcType.reg && bprfWen(j) || enqSrcType(i)===SrcType.fp && bpfpWen(j)))
      val hitVecNext = hitVec.map(RegNext(_))
      val hit = ParallelOR(hitVec).asBool
      when (enqFire && hit && !enqSrcRdy(i)) {
        srcRdyVec(enqSelIq)(i) := true.B
      }
      when (RegNext(enqFire && hit && !enqSrcRdy(i))) {
        srcDataWire(enqSelIqNext)(i) := ParallelMux(hitVecNext zip bpData)
      }
      // XSDebug(enqFire && hit, "EnqBypassCtrl: enqSelIq:%d Src:(%d|%d) Hit:%d HitVec:%b \n", enqSelIq, i.U, enqPsrc(i), hit, VecInit(hitVec).asUInt)
      for (k <- bpValid.indices) {
        XSDebug(enqFire && hit && !enqSrcRdy(i) && hitVec(k), "EnqBypassCtrlHit: enqSelIq:%d Src%d:%d Ports:%d Pc:%x RoqIdx:%x\n", enqSelIq, i.U, enqPsrc(i), k.U, io.bypassUops(k).bits.cf.pc, io.bypassUops(k).bits.roqIdx)
      }
      // XSDebug(RegNext(enqFire && hit), "EnqBypassData: enqSelIqNext:%d Src:(%d|%d) HitVecNext:%b Data:%x (for last cycle's Ctrl)\n", enqSelIqNext, i.U, enqPsrc(i), VecInit(hitVecNext).asUInt, ParallelMux(hitVecNext zip bpData))
      for (k <- bpValid.indices) {
        XSDebug(RegNext(enqFire && hit && !enqSrcRdy(i) && hitVec(k)), "EnqBypassDataHit: enqSelIq:%d Src%d:%d Ports:%d Data:%x Pc:%x RoqIdx:%x\n", enqSelIq, i.U, enqPsrc(i), k.U, bpData(k), io.bypassUops(k).bits.cf.pc, io.bypassUops(k).bits.roqIdx)
      }
    }
  
  if (enableBypass) {
    // send out bypass
    val sel = io.selectedUop
    sel.valid := toIssFire && !enqSendEnable
    sel.bits := DontCare
    sel.bits.pdest := issQue(deqSelIq).uop.pdest
    sel.bits.cf.pc := issQue(deqSelIq).uop.cf.pc
    sel.bits.roqIdx := issQue(deqSelIq).uop.roqIdx
    sel.bits.ctrl.rfWen := issQue(deqSelIq).uop.ctrl.rfWen
    sel.bits.ctrl.fpWen := issQue(deqSelIq).uop.ctrl.fpWen
  }
  XSInfo(io.redirect.valid, "Redirect: valid:%d isExp:%d isFpp:%d brTag:%d redHitVec:%b redIdHitVec:%b enqHit:%d selIsRed:%d\n", io.redirect.valid, io.redirect.bits.isException, io.redirect.bits.isFlushPipe, io.redirect.bits.brTag.value, VecInit(redHitVec).asUInt, VecInit(redIdHitVec).asUInt, enqRedHit, selIsRed)
  XSInfo(enqFire, s"EnqCtrl(%d %d) enqSelIq:%d Psrc/Rdy(%d:%d %d:%d %d:%d) Dest:%d oldDest:%d pc:%x roqIdx:%x\n", io.enqCtrl.valid, io.enqCtrl.ready, enqSelIq
    , io.enqCtrl.bits.psrc1, io.enqCtrl.bits.src1State, io.enqCtrl.bits.psrc2, io.enqCtrl.bits.src2State, io.enqCtrl.bits.psrc3, io.enqCtrl.bits.src3State, io.enqCtrl.bits.pdest, io.enqCtrl.bits.old_pdest, io.enqCtrl.bits.cf.pc, io.enqCtrl.bits.roqIdx)
  XSInfo(enqFireNext, "EnqData: src1:%x src2:%x src3:%x pc:%x roqIdx:%x(for last cycle's Ctrl)\n", io.enqData.src1, io.enqData.src2, io.enqData.src3, issQue(enqSelIqNext).uop.cf.pc, issQue(enqSelIqNext).uop.roqIdx)
  XSInfo(deqFire, "Deq:(%d %d) [%d|%x][%d|%x][%d|%x] pdest:%d pc:%x roqIdx:%x\n", io.deq.valid, io.deq.ready, io.deq.bits.uop.psrc1, io.deq.bits.src1, io.deq.bits.uop.psrc2, io.deq.bits.src2, io.deq.bits.uop.psrc3, io.deq.bits.src3, io.deq.bits.uop.pdest, io.deq.bits.uop.cf.pc, io.deq.bits.uop.roqIdx)
  XSDebug("tailAll:%d KID(%d%d%d) tailDot:%b tailDot2:%b selDot:%b popDot:%b moveDot:%b In(%d %d) Out(%d %d)\n", tailAll, tailKeep, tailInc, tailDec, tailDot, tailDot2, selDot, popDot, moveDot, io.enqCtrl.valid, io.enqCtrl.ready, io.deq.valid, io.deq.ready)
  XSInfo(issueToExuValid, "FireStage:Out(%d %d) src1(%d|%x) src2(%d|%x) src3(%d|%x) deqFlush:%d pc:%x roqIdx:%d\n", io.deq.valid, io.deq.ready, issueToExu.uop.psrc1, issueToExu.src1, issueToExu.uop.psrc2, issueToExu.src2, issueToExu.uop.psrc3, issueToExu.src3, deqFlushHit, issueToExu.uop.cf.pc, issueToExu.uop.roqIdx)
  if(enableBypass) {
    XSDebug("popOne:%d isPop:%d popSel:%d deqSel:%d deqCanIn:%d toIssFire:%d has1Rdy:%d selIsRed:%d nonValid:%b SelUop:(%d, %d)\n", popOne, isPop, popSel, deqSel, deqCanIn, toIssFire, has1Rdy, selIsRed, nonValid, io.selectedUop.valid, io.selectedUop.bits.pdest)
  } else {
    XSDebug("popOne:%d isPop:%d popSel:%d deqSel:%d deqCanIn:%d toIssFire:%d has1Rdy:%d selIsRed:%d nonValid:%b\n", popOne, isPop, popSel, deqSel, deqCanIn, toIssFire, has1Rdy, selIsRed, nonValid)
  }

  XSDebug(enqSendEnable, p"NoDelayIss: enqALRdy:${enqAlreadyRdy} *Next:${enqALRdyNext} En:${enqSendEnable} flush:${enqSendFlushHit} enqSelIqNext:${enqSelIqNext} deqSelIq:${deqSelIq} deqReady:${io.deq.ready}\n")
  XSDebug(s"id|v|r|psrc|r|   src1         |psrc|r|   src2         |psrc|r|   src3         |brTag|    pc    |roqIdx Exu:${exuCfg.name}\n")

  for (i <- 0 until iqSize) {
    when (i.U===tail && tailAll=/=8.U) {
      XSDebug("%d |%d|%d| %d|%b|%x| %d|%b|%x| %d|%b|%x| %x |%x|%x <-\n",
        idQue(i),
        idValidQue(i),
        srcRdy(idQue(i)),
        psrc(idQue(i))(0),
        srcRdyVec(idQue(i))(0),
        srcData(idQue(i))(0),
        psrc(idQue(i))(1),
        srcRdyVec(idQue(i))(1),
        srcData(idQue(i))(1),
        psrc(idQue(i))(2),
        srcRdyVec(idQue(i))(2),
        srcData(idQue(i))(2),
        issQue(idQue(i)).uop.brTag.value,
        issQue(idQue(i)).uop.cf.pc,
        issQue(idQue(i)).uop.roqIdx
      )
    }.otherwise {
      XSDebug("%d |%d|%d| %d|%b|%x| %d|%b|%x| %d|%b|%x| %x |%x|%x\n",
        idQue(i),
        idValidQue(i),
        srcRdy(idQue(i)),
        psrc(idQue(i))(0),
        srcRdyVec(idQue(i))(0),
        srcData(idQue(i))(0),
        psrc(idQue(i))(1),
        srcRdyVec(idQue(i))(1),
        srcData(idQue(i))(1),
        psrc(idQue(i))(2),
        srcRdyVec(idQue(i))(2),
        srcData(idQue(i))(2),
        issQue(idQue(i)).uop.brTag.value,
        issQue(idQue(i)).uop.cf.pc,
        issQue(idQue(i)).uop.roqIdx
      )
    }
  }

  XSPerf("utilization", tailAll)
}