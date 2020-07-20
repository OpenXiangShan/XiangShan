package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.ALUOpType
import xiangshan.backend.JumpOpType
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap

class TableAddr(val idxBits: Int, val banks: Int) extends XSBundle {
  def tagBits = VAddrBits - idxBits - 1

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val offset = UInt(1.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(VAddrBits.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx
  def getBank(x: UInt) = getIdx(x)(log2Up(banks) - 1, 0)
  def getBankIdx(x: UInt) = getIdx(x)(idxBits - 1, log2Up(banks))
}

class Stage1To2IO extends XSBundle {
  val pc = Output(UInt(VAddrBits.W))
  val btb = new Bundle {
    val hits = Output(UInt(FetchWidth.W))
    val targets = Output(Vec(FetchWidth, UInt(VAddrBits.W)))
  }
  val jbtac = new Bundle {
    val hitIdx = Output(UInt(FetchWidth.W))
    val target = Output(UInt(VAddrBits.W))
  }
  val tage = new Bundle {
    val hits = Output(UInt(FetchWidth.W))
    val takens = Output(Vec(FetchWidth, Bool()))
  }
  val hist = Output(Vec(FetchWidth, UInt(HistoryLength.W)))
  val btbPred = ValidIO(new BranchPrediction)
}

class BPUStage1 extends XSModule {
  val io = IO(new Bundle() {
    val in = new Bundle { val pc = Flipped(Decoupled(UInt(VAddrBits.W))) }
    // from backend
    val redirectInfo = Input(new RedirectInfo)
    // from Stage3
    val flush = Input(Bool())
    val s3RollBackHist = Input(UInt(HistoryLength.W))
    val s3Taken = Input(Bool())
    // to ifu, quick prediction result
    val s1OutPred = ValidIO(new BranchPrediction)
    // to Stage2
    val out = Decoupled(new Stage1To2IO)
  })

  io.in.pc.ready := true.B

  // flush Stage1 when io.flush
  val flushS1 = BoolStopWatch(io.flush, io.in.pc.fire(), startHighPriority = true)

  // global history register
  val ghr = RegInit(0.U(HistoryLength.W))
  // modify updateGhr and newGhr when updating ghr
  val updateGhr = WireInit(false.B)
  val newGhr = WireInit(0.U(HistoryLength.W))
  when (updateGhr) { ghr := newGhr }
  // use hist as global history!!!
  val hist = Mux(updateGhr, newGhr, ghr)

  // Tage predictor
  // val tage = Module(new FakeTAGE)
  val tage = if(EnableBPD) Module(new Tage) else Module(new FakeTAGE)
  tage.io.req.valid := io.in.pc.fire()
  tage.io.req.bits.pc := io.in.pc.bits
  tage.io.req.bits.hist := hist
  tage.io.redirectInfo <> io.redirectInfo
  io.out.bits.tage <> tage.io.out
  io.s1OutPred.bits.tageMeta := tage.io.meta

  // latch pc for 1 cycle latency when reading SRAM
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.fire())

  val r = io.redirectInfo.redirect
  val updateFetchpc = r.pc - (r.fetchIdx << 2.U)
  // BTB
  val btb = Module(new BTB)
  btb.io.in.pc <> io.in.pc
  btb.io.in.pcLatch := pcLatch
  // TODO: pass real mask in
  btb.io.in.mask := "b1111111111111111".asUInt
  btb.io.redirectValid := io.redirectInfo.valid
  btb.io.flush := io.flush

  // btb.io.update.fetchPC := updateFetchpc
  // btb.io.update.fetchIdx := r.fetchIdx
  btb.io.update.pc := r.pc
  btb.io.update.hit := r.btbHitWay
  btb.io.update.misPred := io.redirectInfo.misPred
  // btb.io.update.writeWay := r.btbVictimWay
  btb.io.update.oldCtr := r.btbPredCtr
  btb.io.update.taken := r.taken
  btb.io.update.target := r.brTarget
  btb.io.update.btbType := r.btbType
  // TODO: add RVC logic
  btb.io.update.isRVC := DontCare

  val btbHit = btb.io.out.hit
  val btbTaken = btb.io.out.taken
  val btbTakenIdx = btb.io.out.takenIdx
  val btbTakenTarget = btb.io.out.target
  // val btbWriteWay = btb.io.out.writeWay
  val btbNotTakens = btb.io.out.notTakens
  val btbCtrs = VecInit(btb.io.out.dEntries.map(_.pred))
  val btbValids = btb.io.out.hits
  val btbTargets = VecInit(btb.io.out.dEntries.map(_.target))
  val btbTypes = VecInit(btb.io.out.dEntries.map(_.btbType))


  val jbtac = Module(new JBTAC)
  jbtac.io.in.pc <> io.in.pc
  jbtac.io.in.pcLatch := pcLatch
  jbtac.io.in.hist := hist
  jbtac.io.redirectValid := io.redirectInfo.valid
  jbtac.io.flush := io.flush

  jbtac.io.update.fetchPC := updateFetchpc
  jbtac.io.update.fetchIdx := r.fetchIdx << 1
  jbtac.io.update.misPred := io.redirectInfo.misPred
  jbtac.io.update.btbType := r.btbType
  jbtac.io.update.target := r.target
  jbtac.io.update.hist := r.hist

  val jbtacHit = jbtac.io.out.hit
  val jbtacTarget = jbtac.io.out.target
  val jbtacHitIdx = jbtac.io.out.hitIdx

  // calculate global history of each instr
  val firstHist = RegNext(hist)
  val histShift = Wire(Vec(FetchWidth, UInt(log2Up(FetchWidth).W)))
  val shift = Wire(Vec(FetchWidth, Vec(FetchWidth, UInt(1.W))))
  (0 until FetchWidth).foreach(i => shift(i) := Mux(!btbNotTakens(i), 0.U, ~LowerMask(UIntToOH(i.U), FetchWidth)).asTypeOf(Vec(FetchWidth, UInt(1.W))))
  for (j <- 0 until FetchWidth) {
    var tmp = 0.U
    for (i <- 0 until FetchWidth) {
      tmp = tmp + shift(i)(j)
    }
    histShift(j) := tmp
  }
  (0 until FetchWidth).foreach(i => io.s1OutPred.bits.hist(i) := firstHist << histShift(i))

  // update ghr
  updateGhr := io.s1OutPred.bits.redirect || io.flush
  val brJumpIdx = Mux(!(btbHit && btbTaken), 0.U, UIntToOH(btbTakenIdx))
  val indirectIdx = Mux(!jbtacHit, 0.U, UIntToOH(jbtacHitIdx))
  //val newTaken = Mux(io.redirectInfo.flush(), !(r.btbType === BTBtype.B && !r.taken), )
  newGhr := Mux(io.redirectInfo.flush(),    (r.hist << 1.U) | !(r.btbType === BTBtype.B && !r.taken),
            Mux(io.flush,                   Mux(io.s3Taken, (io.s3RollBackHist << 1.U) | 1.U, io.s3RollBackHist),
            Mux(io.s1OutPred.bits.redirect, ((PriorityMux(brJumpIdx | indirectIdx, io.s1OutPred.bits.hist) << 1.U) | 1.U),
                                            io.s1OutPred.bits.hist(0) << PopCount(btbNotTakens))))

  // redirect based on BTB and JBTAC
  // io.out.valid := RegNext(io.in.pc.fire()) && !flushS1u 
  io.out.valid := RegNext(io.in.pc.fire()) && !io.flush

  io.s1OutPred.valid := io.out.valid
  io.s1OutPred.bits.redirect := btbHit && btbTaken || jbtacHit


  def getInstrValid(i: Int): UInt = {
    val mask = Wire(UInt(FetchWidth.W))
    val vec = Wire(Vec(FetchWidth, UInt(1.W)))
    for (j <- 0 until FetchWidth) {
      if (j <= i)
        vec(j) := 1.U
      else
        vec(j) := 0.U
    }
    mask := vec.asUInt
    mask
  }
  io.s1OutPred.bits.instrValid := (Fill(FetchWidth, ~io.s1OutPred.bits.redirect).asUInt |
    PriorityMux(brJumpIdx | indirectIdx, (0 until FetchWidth).map(getInstrValid(_)))).asTypeOf(Vec(FetchWidth, Bool()))
  io.s1OutPred.bits.target := Mux(brJumpIdx === LowestBit(brJumpIdx | indirectIdx, FetchWidth), btbTakenTarget, jbtacTarget)
  io.s1OutPred.bits.predCtr := btbCtrs
  io.s1OutPred.bits.btbHitWay := btbHit
  io.s1OutPred.bits.rasSp := DontCare
  io.s1OutPred.bits.rasTopCtr := DontCare

  io.out.bits.pc := pcLatch
  io.out.bits.btb.hits := btbValids.asUInt
  (0 until FetchWidth).foreach(i => io.out.bits.btb.targets(i) := btbTargets(i))
  io.out.bits.jbtac.hitIdx := UIntToOH(jbtacHitIdx)
  io.out.bits.jbtac.target := jbtacTarget
  // TODO: we don't need this repeatedly!
  io.out.bits.hist := io.s1OutPred.bits.hist
  io.out.bits.btbPred := io.s1OutPred



  // debug info
  XSDebug(true.B, "in:(%d %d)   pc=%x ghr=%b\n", io.in.pc.valid, io.in.pc.ready, io.in.pc.bits, hist)
  XSDebug(true.B, "outPred:(%d) pc=0x%x, redirect=%d instrValid=%b tgt=%x\n",
    io.s1OutPred.valid, pcLatch, io.s1OutPred.bits.redirect, io.s1OutPred.bits.instrValid.asUInt, io.s1OutPred.bits.target)
  XSDebug(io.flush && io.redirectInfo.flush(),
    "flush from backend: pc=%x tgt=%x brTgt=%x btbType=%b taken=%d oldHist=%b fetchIdx=%d isExcpt=%d\n",
    r.pc, r.target, r.brTarget, r.btbType, r.taken, r.hist, r.fetchIdx, r.isException)
  XSDebug(io.flush && !io.redirectInfo.flush(),
    "flush from Stage3:  s3Taken=%d s3RollBackHist=%b\n", io.s3Taken, io.s3RollBackHist)

}

class Stage2To3IO extends Stage1To2IO {
}

class BPUStage2 extends XSModule {
  val io = IO(new Bundle() {
    // flush from Stage3
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new Stage1To2IO))
    val out = Decoupled(new Stage2To3IO)
  })

  // flush Stage2 when Stage3 or banckend redirects
  val flushS2 = BoolStopWatch(io.flush, io.in.fire(), startHighPriority = true)
  val inLatch = RegInit(0.U.asTypeOf(io.in.bits))
  when (io.in.fire()) { inLatch := io.in.bits }
  val validLatch = RegInit(false.B)
  when (io.flush) {
    validLatch := false.B
  }.elsewhen (io.in.fire()) {
    validLatch := true.B
  }.elsewhen (io.out.fire()) {
    validLatch := false.B
  }

  io.out.valid := !io.flush && !flushS2 && validLatch
  io.in.ready := !validLatch || io.out.fire()

  // do nothing
  io.out.bits := inLatch

  // debug info
  XSDebug(true.B, "in:(%d %d) pc=%x out:(%d %d) pc=%x\n",
    io.in.valid, io.in.ready, io.in.bits.pc, io.out.valid, io.out.ready, io.out.bits.pc)
  XSDebug(true.B, "validLatch=%d pc=%x\n", validLatch, inLatch.pc)
  XSDebug(io.flush, "flush!!!\n")
}

class BPUStage3 extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new Stage2To3IO))
    val out = ValidIO(new BranchPrediction)
    // from icache
    val predecode = Flipped(ValidIO(new Predecode))
    // from backend
    val redirectInfo = Input(new RedirectInfo)
    // to Stage1 and Stage2
    val flushBPU = Output(Bool())
    // to Stage1, restore ghr in stage1 when flushBPU is valid
    val s1RollBackHist = Output(UInt(HistoryLength.W))
    val s3Taken = Output(Bool())
  })

  val flushS3 = BoolStopWatch(io.flush, io.in.fire(), startHighPriority = true)
  val inLatch = RegInit(0.U.asTypeOf(io.in.bits))
  val validLatch = RegInit(false.B)
  when (io.in.fire()) { inLatch := io.in.bits }
  when (io.flush) {
    validLatch := false.B
  }.elsewhen (io.in.fire()) {
    validLatch := true.B
  }.elsewhen (io.out.valid) {
    validLatch := false.B
  }
  io.out.valid := validLatch && io.predecode.valid && !flushS3 && !io.flush
  io.in.ready := !validLatch || io.out.valid

  // RAS
  // TODO: split retAddr and ctr
  def rasEntry() = new Bundle {
    val retAddr = UInt(VAddrBits.W)
    val ctr = UInt(8.W) // layer of nested call functions
  }
  val ras = RegInit(VecInit(Seq.fill(RasSize)(0.U.asTypeOf(rasEntry()))))
  val sp = Counter(RasSize)
  val rasTop = ras(sp.value)
  val rasTopAddr = rasTop.retAddr

  // get the first taken branch/jal/call/jalr/ret in a fetch line
  // brNotTakenIdx indicates all the not-taken branches before the first jump instruction


  val brs = inLatch.btb.hits & Reverse(Cat(io.predecode.bits.fuOpTypes.map { t => ALUOpType.isBranch(t) }).asUInt) & io.predecode.bits.mask
  val brTakens = brs & inLatch.tage.takens.asUInt
  val jals = inLatch.btb.hits & Reverse(Cat(io.predecode.bits.fuOpTypes.map { t => t === JumpOpType.jal }).asUInt) & io.predecode.bits.mask
  val calls = inLatch.btb.hits & io.predecode.bits.mask & Reverse(Cat(io.predecode.bits.fuOpTypes.map { t => t === JumpOpType.call }).asUInt)
  val jalrs = inLatch.jbtac.hitIdx & io.predecode.bits.mask & Reverse(Cat(io.predecode.bits.fuOpTypes.map { t => t === JumpOpType.jalr }).asUInt)
  val rets = io.predecode.bits.mask & Reverse(Cat(io.predecode.bits.fuOpTypes.map { t => t === JumpOpType.ret }).asUInt)

  val brTakenIdx = PriorityMux(brTakens, (0 until FetchWidth).map(_.U))
  val jalIdx = PriorityMux(jals, (0 until FetchWidth).map(_.U))
  val callIdx = PriorityMux(calls, (0 until FetchWidth).map(_.U))
  val jalrIdx = PriorityMux(jalrs, (0 until FetchWidth).map(_.U))
  val retIdx = PriorityMux(rets, (0 until FetchWidth).map(_.U))

  val jmps = (if (EnableRAS) {brTakens | jals | calls | jalrs | rets} else {brTakens | jals | calls | jalrs})
  val jmpIdx = MuxCase(0.U, (0 until FetchWidth).map(i => (jmps(i), i.U)))
  io.s3Taken := MuxCase(false.B, (0 until FetchWidth).map(i => (jmps(i), true.B)))

  val brNotTakens = VecInit((0 until FetchWidth).map(i => brs(i) && ~inLatch.tage.takens(i) && i.U <= jmpIdx && io.predecode.bits.mask(i)))


  io.out.bits.predCtr := inLatch.btbPred.bits.predCtr
  io.out.bits.btbHitWay := inLatch.btbPred.bits.btbHitWay
  io.out.bits.tageMeta := inLatch.btbPred.bits.tageMeta
  //io.out.bits.btbType := Mux(jmpIdx === retIdx, BTBtype.R,
  //  Mux(jmpIdx === jalrIdx, BTBtype.I,
  //  Mux(jmpIdx === brTakenIdx, BTBtype.B, BTBtype.J)))
  val firstHist = inLatch.btbPred.bits.hist(0)
  // there may be several notTaken branches before the first jump instruction,
  // so we need to calculate how many zeroes should each instruction shift in its global history.
  // each history is exclusive of instruction's own jump direction.
  val histShift = Wire(Vec(FetchWidth, UInt(log2Up(FetchWidth).W)))
  val shift = Wire(Vec(FetchWidth, Vec(FetchWidth, UInt(1.W))))
  (0 until FetchWidth).foreach(i => shift(i) := Mux(!brNotTakens(i), 0.U, ~LowerMask(UIntToOH(i.U), FetchWidth)).asTypeOf(Vec(FetchWidth, UInt(1.W))))
  for (j <- 0 until FetchWidth) {
    var tmp = 0.U
    for (i <- 0 until FetchWidth) {
      tmp = tmp + shift(i)(j)
    }
    histShift(j) := tmp
  }
  (0 until FetchWidth).foreach(i => io.out.bits.hist(i) := firstHist << histShift(i))
  // save ras checkpoint info
  io.out.bits.rasSp := sp.value
  io.out.bits.rasTopCtr := rasTop.ctr

  // flush BPU and redirect when target differs from the target predicted in Stage1
  val tToNt = inLatch.btbPred.bits.redirect && ~io.s3Taken
  val ntToT = ~inLatch.btbPred.bits.redirect && io.s3Taken
  val dirDiffers = tToNt || ntToT
  val tgtDiffers = inLatch.btbPred.bits.redirect && io.s3Taken && io.out.bits.target =/= inLatch.btbPred.bits.target
  io.out.bits.redirect := (if (EnableBPD) {dirDiffers || tgtDiffers} else false.B)
  io.out.bits.target := Mux(!io.s3Taken, inLatch.pc + (PopCount(io.predecode.bits.mask) << 2.U), // TODO: RVC
    Mux(jmpIdx === retIdx, rasTopAddr,
    Mux(jmpIdx === jalrIdx, inLatch.jbtac.target,
    inLatch.btb.targets(jmpIdx))))
  for (i <- 0 until FetchWidth) {
    io.out.bits.instrValid(i) := ((io.s3Taken && i.U <= jmpIdx) || ~io.s3Taken) && io.predecode.bits.mask(i)
  }
  io.flushBPU := io.out.bits.redirect && io.out.valid

  // speculative update RAS
  val rasWrite = WireInit(0.U.asTypeOf(rasEntry()))
  val retAddr = inLatch.pc + (callIdx << 2.U) + 4.U
  rasWrite.retAddr := retAddr
  val allocNewEntry = rasWrite.retAddr =/= rasTopAddr
  rasWrite.ctr := Mux(allocNewEntry, 1.U, rasTop.ctr + 1.U)
  val rasWritePosition = Mux(allocNewEntry, sp.value + 1.U, sp.value)
  when (io.out.valid) {
    when (jmpIdx === callIdx) {
      ras(rasWritePosition) := rasWrite
      when (allocNewEntry) { sp.value := sp.value + 1.U }
    }.elsewhen (jmpIdx === retIdx) {
      when (rasTop.ctr === 1.U) {
        sp.value := Mux(sp.value === 0.U, 0.U, sp.value - 1.U)
      }.otherwise {
        ras(sp.value) := Cat(rasTop.ctr - 1.U, rasTopAddr).asTypeOf(rasEntry())
      }
    }
  }
  // use checkpoint to recover RAS
  val recoverSp = io.redirectInfo.redirect.rasSp
  val recoverCtr = io.redirectInfo.redirect.rasTopCtr
  when (io.redirectInfo.valid && io.redirectInfo.misPred) {
    sp.value := recoverSp
    ras(recoverSp) := Cat(recoverCtr, ras(recoverSp).retAddr).asTypeOf(rasEntry())
  }

  // roll back global history in S1 if S3 redirects
  io.s1RollBackHist := Mux(io.s3Taken, io.out.bits.hist(jmpIdx), io.out.bits.hist(0) << PopCount(brs & ~inLatch.tage.takens.asUInt))

  XSDebug(io.in.fire() && callIdx.orR, "[RAS]:pc=0x%x, rasWritePosition=%d, rasWriteAddr=0x%x\n",
            io.in.bits.pc, rasWritePosition, retAddr)

  // debug info
  XSDebug(io.in.fire(), "in:(%d %d) pc=%x\n", io.in.valid, io.in.ready, io.in.bits.pc)
  XSDebug(io.out.valid, "out:%d pc=%x redirect=%d predcdMask=%b instrValid=%b tgt=%x\n",
    io.out.valid, inLatch.pc, io.out.bits.redirect, io.predecode.bits.mask, io.out.bits.instrValid.asUInt, io.out.bits.target)
  XSDebug(true.B, "flushS3=%d\n", flushS3)
  XSDebug(true.B, "validLatch=%d predecode.valid=%d\n", validLatch, io.predecode.valid)
  XSDebug(true.B, "jmpIdx=%d, brs=%b brTakenIdx=%d brNTakens=%b jalIdx=%d jalrIdx=%d callIdx=%d retIdx=%d\n",
    jmpIdx, brs, brTakenIdx, brNotTakens.asUInt, jalIdx, jalrIdx, callIdx, retIdx)
  XSDebug(true.B, "tgtDiffers:%d, dirDiffers:%d, s3taken=%d\n", tgtDiffers, dirDiffers, io.s3Taken)

  // BPU's TEMP Perf Cnt
  // BoringUtils.addSource(io.out.valid, "MbpS3Cnt")
  // BoringUtils.addSource(io.out.valid && io.out.bits.redirect, "MbpS3TageRed")
  // BoringUtils.addSource(io.out.valid && (inLatch.btbPred.bits.redirect ^ io.s3Taken), "MbpS3TageRedDir")
  // BoringUtils.addSource(io.out.valid && (inLatch.btbPred.bits.redirect 
  //             && io.s3Taken && (io.out.bits.target =/= inLatch.btbPred.bits.target)), "MbpS3TageRedTar")
}

class BPU extends XSModule {
  val io = IO(new Bundle() {
    // from backend
    // flush pipeline if misPred and update bpu based on redirect signals from brq
    val redirectInfo = Input(new RedirectInfo)

    val in = new Bundle { val pc = Flipped(Valid(UInt(VAddrBits.W))) }

    val btbOut = ValidIO(new BranchPrediction)
    val tageOut = ValidIO(new BranchPrediction)

    // predecode info from icache
    // TODO: simplify this after implement predecode unit
    val predecode = Flipped(ValidIO(new Predecode))
  })

  val s1 = Module(new BPUStage1)
  val s2 = Module(new BPUStage2)
  val s3 = Module(new BPUStage3)

  s1.io.redirectInfo <> io.redirectInfo
  s1.io.flush := s3.io.flushBPU || io.redirectInfo.flush()
  s1.io.in.pc.valid := io.in.pc.valid
  s1.io.in.pc.bits <> io.in.pc.bits
  io.btbOut <> s1.io.s1OutPred
  s1.io.s3RollBackHist := s3.io.s1RollBackHist
  s1.io.s3Taken := s3.io.s3Taken

  s1.io.out <> s2.io.in
  s2.io.flush := s3.io.flushBPU || io.redirectInfo.flush()

  s2.io.out <> s3.io.in
  s3.io.flush := io.redirectInfo.flush()
  s3.io.predecode <> io.predecode
  io.tageOut <> s3.io.out
  s3.io.redirectInfo <> io.redirectInfo

  // TODO: temp and ugly code, when perf counters is added( may after adding CSR), please mv the below counter
  // val bpuPerfCntList = List(
  //   ("MbpInstr","         "),
  //   ("MbpRight","         "),
  //   ("MbpWrong","         "),
  //   ("MbpBRight","        "),
  //   ("MbpBWrong","        "),
  //   ("MbpJRight","        "),
  //   ("MbpJWrong","        "),
  //   ("MbpIRight","        "),
  //   ("MbpIWrong","        "),
  //   ("MbpRRight","        "),
  //   ("MbpRWrong","        "),
  //   ("MbpS3Cnt","         "),
  //   ("MbpS3TageRed","     "),
  //   ("MbpS3TageRedDir","  "),
  //   ("MbpS3TageRedTar","  ")
  // )

  // val bpuPerfCnts = List.fill(bpuPerfCntList.length)(RegInit(0.U(XLEN.W)))
  // val bpuPerfCntConds = List.fill(bpuPerfCntList.length)(WireInit(false.B))
  // (bpuPerfCnts zip bpuPerfCntConds) map { case (cnt, cond) => { when (cond) { cnt := cnt + 1.U }}}

  // for(i <- bpuPerfCntList.indices) {
  //   BoringUtils.addSink(bpuPerfCntConds(i), bpuPerfCntList(i)._1)
  // }

  // val xsTrap = WireInit(false.B)
  // BoringUtils.addSink(xsTrap, "XSTRAP_BPU")

  // // if (!p.FPGAPlatform) {
  //   when (xsTrap) {
  //     printf("=================BPU's PerfCnt================\n")
  //     for(i <- bpuPerfCntList.indices) {
  //       printf(bpuPerfCntList(i)._1 + bpuPerfCntList(i)._2 + " <- " + "%d\n", bpuPerfCnts(i))
  //     }
  //   }
  // // }
}