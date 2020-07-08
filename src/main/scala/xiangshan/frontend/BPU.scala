package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import utils._

class TableAddr(val idxBits: Int, val banks: Int) extends XSBundle {
  def tagBits = VAddrBits - idxBits - 2

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val offset = UInt(2.W)

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
    val targets = Output(Vec(FetchWidth, UInt(VAddrBits.B)))
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
    val redirectInfo = Flipped(new RedirectInfo)
    // from Stage3
    val flush = Input(Bool())
    val s3RollBackHist = Input(UInt(HistoryLength.W))
    // to ifu, quick prediction result
    val btbOut = ValidIO(new BranchPrediction)
    // to Stage2
    val out = Decoupled(new Stage1To2IO)
  })

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
  val tage = Module(new Tage)
  tage.io.req.valid := io.in.pc.fire()
  tage.io.req.bits.pc := io.in.pc.bits
  tage.io.req.bits.hist := hist
  tage.io.redirectInfo <> io.redirectInfo
  io.out.bits.tage <> tage.io.out
  io.btbOut.bits.tageMeta := tage.io.meta

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
  io.out.valid := !flushS2 && RegNext(io.in.fire())
  io.in.ready := !io.out.valid || io.out.fire()

  // do nothing
  io.out.bits := RegEnable(io.in.bits, io.in.fire())
}

class BPUStage3 extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new Stage2To3IO))
    val out = ValidIO(new BranchPrediction)
    // from icache
    val predecode = Flipped(ValidIO(new Predecode))
    // from backend
    val redirectInfo = Flipped(new RedirectInfo)
    // to Stage1 and Stage2
    val flushBPU = Output(Bool())
    // to Stage1, restore ghr in stage1 when flushBPU is valid
    val s1RollBackHist = Output(UInt(HistoryLength.W))
  })

  val flushS3 = BoolStopWatch(io.flush, io.in.fire(), startHighPriority = true)
  val inLatch = RegInit(0.U.asTypeOf(io.in.bits))
  val validLatch = RegInit(false.B)
  when (io.in.fire()) { inLatch := io.in.bits }
  when (io.in.fire()) {
    validLatch := !io.in.flush
  }.elsewhen (io.out.valid) {
    validLatch := false.B
  }
  io.out.valid := validLatch && io.predecode.valid && !flushS3
  io.in.ready := !validLatch || io.out.valid

  // RAS
  // TODO: split retAddr and ctr
  def rasEntry() = new Bundle {
    val retAddr = UInt(VAddrBits.W)
    val ctr = UInt(8.W) // layer of nested call functions
  }
  val ras = RegInit(VecInit(RasSize, 0.U.asTypeOf(rasEntry())))
  val sp = Counter(RasSize)
  val rasTop = ras(sp.value)
  val rasTopAddr = rasTop.retAddr

  // get the first taken branch/jal/call/jalr/ret in a fetch line
  // brTakenIdx/jalIdx/callIdx/jalrIdx/retIdx/jmpIdx is one-hot encoded.
  // brNotTakenIdx indicates all the not-taken branches before the first jump instruction.
  val brIdx = inLatch.btb.hits & io.predecode.bits.fuTypes.map { t => ALUOpType.isBranch(t) }.asUInt & io.predecode.bits.mask
  val brTakenIdx = LowestBit(brIdx & inLatch.tage.takens.asUInt, FetchWidth)
  val jalIdx = LowestBit(inLatch.btb.hits & io.predecode.bits.fuTypes.map { t => t === ALUOpType.jal }.asUInt & io.predecode.bits.mask, FetchWidth)
  val callIdx = LowestBit(inLatch.btb.hits & io.predecode.bits.mask & io.predecode.bits.fuTypes.map { t => t === ALUOpType.call }.asUInt, FetchWidth)
  val jalrIdx = LowestBit(inLatch.jbtac.hitIdx & io.predecode.bits.mask & io.predecode.bits.fuTypes.map { t => t === ALUOpType.jalr }.asUInt, FetchWidth)
  val retIdx = LowestBit(io.predecode.bits.mask & io.predecode.bits.fuTypes.map { t => t === ALUOpType.ret }.asUInt, FetchWidth)

  val jmpIdx = LowestBit(brTakenIdx | jalIdx | callIdx | jalrIdx | retIdx, FetchWidth)
  val brNotTakenIdx = brIdx & ~inLatch.tage.takens.asUInt & LowerMask(jmpIdx, FetchWidth)

  io.out.bits.redirect := jmpIdx.orR.asBool
  io.out.bits.target := Mux(jmpIdx === retIdx, rasTopAddr,
    Mux(jmpIdx === jalrIdx, inLatch.jbtac.target,
    Mux(jmpIdx === 0.U, inLatch.pc + 4.U, // TODO: RVC
    PriorityMux(jmpIdx, inLatch.btb.targets))))
  io.out.bits.instrValid := LowerMask(jmpIdx, FetchWidth).asTypeOf(Vec(FetchWidth, Bool()))
  io.out.bits.tageMeta := inLatch.btbPred.bits.tageMeta
  //io.out.bits._type := Mux(jmpIdx === retIdx, BTBtype.R,
  //  Mux(jmpIdx === jalrIdx, BTBtype.I,
  //  Mux(jmpIdx === brTakenIdx, BTBtype.B, BTBtype.J)))
  val firstHist = inLatch.btbPred.bits.hist
  // there may be several notTaken branches before the first jump instruction,
  // so we need to calculate how many zeroes should each instruction shift in its global history.
  // each history is exclusive of instruction's own jump direction.
  val histShift = WireInit(VecInit(FetchWidth, 0.U(log2Up(FetchWidth).W)))
  histShift := (0 until FetchWidth).map(i => Mux(!brNotTakenIdx(i), 0.U, ~LowerMask(UIntToOH(i.U), FetchWidth))).reduce(_+_)
  (0 until FetchWidth).map(i => io.out.bits.hist(i) := firstHist << histShift)
  // save ras checkpoint info
  io.out.bits.rasSp := sp.value
  io.out.bits.rasTopCtr := rasTop.ctr

  // flush BPU and redirect when target differs from the target predicted in Stage1
  io.out.bits.redirect := !inLatch.btbPred.bits.redirect ^ jmpIdx.orR.asBool ||
    inLatch.btbPred.bits.redirect && jmpIdx.orR.asBool && io.out.bits.target =/= inLatch.btbPred.bits.target
  io.flushBPU := io.out.bits.redirect && io.out.valid

  // speculative update RAS
  val rasWrite = WireInit(0.U.asTypeOf(rasEntry()))
  rasWrite.retAddr := inLatch.pc + OHToUInt(callIdx) << 2.U + 4.U
  val allocNewEntry = rasWrite.retAddr =/= rasTopAddr
  rasWrite.ctr := Mux(allocNewEntry, 1.U, rasTop.ctr + 1.U)
  when (io.out.valid) {
    when (jmpIdx === callIdx) {
      ras(Mux(allocNewEntry, sp.value + 1.U, sp.value)) := rasWrite
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
  io.s1RollBackHist := PriorityMux(jmpIdx, io.out.bits.hist)
}

class BPU extends XSModule {
  val io = IO(new Bundle() {
    // from backend
    // flush pipeline if misPred and update bpu based on redirect signals from brq
    val redirectInfo = Flipped(new RedirectInfo)

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
  io.btbOut <> s1.io.btbOut
  s1.io.s3RollBackHist := s3.io.s1RollBackHist

  s1.io.out <> s2.io.in
  s2.io.flush := s3.io.flushBPU || io.redirectInfo.flush()

  s2.io.out <> s3.io.in
  s3.io.flush := io.redirectInfo.flush()
  s3.io.predecode <> io.predecode
  io.tageOut <> s3.io.out
  s3.io.redirectInfo <> io.redirectInfo

  // TODO: delete this and put BTB and JBTAC into Stage1
  
  val flush = BoolStopWatch(io.redirect.valid, io.in.pc.valid, startHighPriority = true)
  
  // BTB makes a quick prediction for branch and direct jump, which is
  // 4-way set-associative, and each way is divided into 4 banks. 
  val btbAddr = new TableAddr(log2Up(BtbSets), BtbBanks)
  def btbEntry() = new Bundle {
    val valid = Bool()
    // TODO: don't need full length of tag and target
    val tag = UInt(btbAddr.tagBits.W)
    val _type = UInt(2.W)
    val target = UInt(VAddrBits.W)
    val pred = UInt(2.W) // 2-bit saturated counter as a quick predictor
  }

  val btb = List.fill(BtbBanks)(List.fill(BtbWays)(
    Module(new SRAMTemplate(btbEntry(), set = BtbSets / BtbBanks, shouldReset = true, holdRead = true, singlePort = true))))

  // val fetchPkgAligned = btbAddr.getBank(io.in.pc.bits) === 0.U
  val HeadBank = btbAddr.getBank(io.in.pc.bits)
  val TailBank = btbAddr.getBank(io.in.pc.bits + FetchWidth.U << 2.U - 4.U)
  for (b <- 0 until BtbBanks) {
    for (w <- 0 until BtbWays) {
      btb(b)(w).reset := reset.asBool
      btb(b)(w).io.r.req.valid := io.in.pc.valid && Mux(TailBank > HeadBank, b.U >= HeadBank && b.U <= TailBank, b.U >= TailBank || b.U <= HeadBank)
      btb(b)(w).io.r.req.bits.setIdx := btbAddr.getBankIdx(io.in.pc.bits)
    }
  }
  // latch pc for 1 cycle latency when reading SRAM
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  val btbRead = Wire(Vec(BtbBanks, Vec(BtbWays, btbEntry())))
  val btbHits = Wire(Vec(FetchWidth, Bool()))
  val btbTargets = Wire(Vec(FetchWidth, UInt(VAddrBits.W)))
  val btbTypes = Wire(Vec(FetchWidth, UInt(2.W)))
  // val btbPreds = Wire(Vec(FetchWidth, UInt(2.W)))
  val btbTakens = Wire(Vec(FetchWidth, Bool()))
  for (b <- 0 until BtbBanks) {
    for (w <- 0 until BtbWays) {
      btbRead(b)(w) := btb(b)(w).io.r.resp.data(0)
    }
  }
  for (i <- 0 until FetchWidth) {
    btbHits(i) := false.B
    for (b <- 0 until BtbBanks) {
      for (w <- 0 until BtbWays) {
        when (b.U === btbAddr.getBank(pcLatch) && btbRead(b)(w).valid && btbRead(b)(w).tag === btbAddr.getTag(Cat(pcLatch(VAddrBits - 1, 2), 0.U(2.W)) + i.U << 2)) {
          btbHits(i) := !flush && RegNext(btb(b)(w).io.r.req.fire(), init = false.B)
          btbTargets(i) := btbRead(b)(w).target
          btbTypes(i) := btbRead(b)(w)._type
          // btbPreds(i) := btbRead(b)(w).pred
          btbTakens(i) := (btbRead(b)(w).pred)(1).asBool
        }.otherwise {
          btbHits(i) := false.B
          btbTargets(i) := DontCare
          btbTypes(i) := DontCare
          btbTakens(i) := DontCare
        }
      }
    }
  }

  // JBTAC, divided into 8 banks, makes prediction for indirect jump except ret.
  val jbtacAddr = new TableAddr(log2Up(JbtacSize), JbtacBanks)
  def jbtacEntry() = new Bundle {
    val valid = Bool()
    // TODO: don't need full length of tag and target
    val tag = UInt(jbtacAddr.tagBits.W)
    val target = UInt(VAddrBits.W)
  }

  val jbtac = List.fill(JbtacBanks)(Module(new SRAMTemplate(jbtacEntry(), set = JbtacSize / JbtacBanks, shouldReset = true, holdRead = true, singlePort = true)))

  (0 until JbtacBanks).map(i => jbtac(i).reset := reset.asBool)
  (0 until JbtacBanks).map(i => jbtac(i).io.r.req.valid := io.in.pc.valid)
  (0 until JbtacBanks).map(i => jbtac(i).io.r.req.bits.setIdx := jbtacAddr.getBankIdx(Cat((io.in.pc.bits)(VAddrBits - 1, 2), 0.U(2.W)) + i.U << 2))

  val jbtacRead = Wire(Vec(JbtacBanks, jbtacEntry()))
  (0 until JbtacBanks).map(i => jbtacRead(i) := jbtac(i).io.r.resp.data(0))
  val jbtacHits = Wire(Vec(FetchWidth, Bool()))
  val jbtacTargets = Wire(Vec(FetchWidth, UInt(VAddrBits.W)))
  val jbtacHeadBank = jbtacAddr.getBank(Cat(pcLatch(VAddrBits - 1, 2), 0.U(2.W)))
  for (i <- 0 until FetchWidth) {
    jbtacHits(i) := false.B
    for (b <- 0 until JbtacBanks) {
      when (jbtacHeadBank + i.U === b.U) {
        jbtacHits(i) := jbtacRead(b).valid && jbtacRead(b).tag === jbtacAddr.getTag(Cat(pcLatch(VAddrBits - 1, 2), 0.U(2.W)) + i.U << 2) &&
          !flush && RegNext(jbtac(b).io.r.req.fire(), init = false.B)
        jbtacTargets(i) := jbtacRead(b).target
      }.otherwise {
        jbtacHits(i) := false.B
        jbtacTargets(i) := DontCare
      }
    }
  }

  // redirect based on BTB and JBTAC
  (0 until FetchWidth).map(i => io.predMask(i) := btbHits(i) && Mux(btbTypes(i) === BTBtype.B, btbTakens(i), true.B) || jbtacHits(i))
  (0 until FetchWidth).map(i => io.predTargets(i) := Mux(btbHits(i) && !(btbTypes(i) === BTBtype.B && !btbTakens(i)), btbTargets(i), jbtacTargets(i)))


  // update bpu, including BTB, JBTAC...
  // 1. update BTB
  // 1.1 read the selected bank
  for (b <- 0 until BtbBanks) {
    for (w <- 0 until BtbWays) {
      btb(b)(w).io.r.req.valid := io.redirect.valid && btbAddr.getBank(io.redirect.bits.pc) === b.U
      btb(b)(w).io.r.req.bits.setIdx := btbAddr.getBankIdx(io.redirect.bits.pc)
    }
  }

  // 1.2 match redirect pc tag with the 4 tags in a btb line, find a way to write
  // val redirectLatch = RegEnable(io.redirect.bits, io.redirect.valid)
  val redirectLatch = RegNext(io.redirect.bits, init = 0.U.asTypeOf(new Redirect))
  val bankLatch = btbAddr.getBank(redirectLatch.pc)
  val btbUpdateRead = Wire(Vec(BtbWays, btbEntry()))
  val btbValids = Wire(Vec(BtbWays, Bool()))
  val btbUpdateTagHits = Wire(Vec(BtbWays, Bool()))
  for (b <- 0 until BtbBanks) {
    for (w <- 0 until BtbWays) {
      when (b.U === bankLatch) {
        btbUpdateRead(w) := btb(b)(w).io.r.resp.data(0)
        btbValids(w) := btbUpdateRead(w).valid && RegNext(btb(b)(w).io.r.req.fire(), init = false.B)
      }.otherwise {
        btbUpdateRead(w) := 0.U.asTypeOf(btbEntry())
        btbValids(w) := false.B
      }
    }
  }
  (0 until BtbWays).map(w => btbUpdateTagHits(w) := btbValids(w) && btbUpdateRead(w).tag === btbAddr.getTag(redirectLatch.pc))
  // val btbWriteWay = Wire(Vec(BtbWays, Bool()))
  val btbWriteWay = Wire(UInt(BtbWays.W))
  val btbInvalids = ~ btbValids.asUInt
  when (btbUpdateTagHits.asUInt.orR) {
    // tag hits
    btbWriteWay := btbUpdateTagHits.asUInt
  }.elsewhen (!btbValids.asUInt.andR) {
    // no tag hits but there are free entries
    btbWriteWay := Mux(btbInvalids >= 8.U, "b1000".U,
      Mux(btbInvalids >= 4.U, "b0100".U,
      Mux(btbInvalids >= 2.U, "b0010".U, "b0001".U)))
  }.otherwise {
    // no tag hits and no free entry, select a victim way
    btbWriteWay := UIntToOH(LFSR64()(log2Up(BtbWays) - 1, 0))
  }

  // 1.3 calculate new 2-bit counter value
  val btbWrite = WireInit(0.U.asTypeOf(btbEntry()))
  btbWrite.valid := true.B
  btbWrite.tag := btbAddr.getTag(redirectLatch.pc)
  btbWrite._type := redirectLatch._type
  btbWrite.target := redirectLatch.brTarget
  val oldPred = WireInit("b01".U)
  oldPred := PriorityMux(btbWriteWay.asTypeOf(Vec(BtbWays, Bool())), btbUpdateRead.map{ e => e.pred })
  val newPred = Mux(redirectLatch.taken, Mux(oldPred === "b11".U, "b11".U, oldPred + 1.U),
    Mux(oldPred === "b00".U, "b00".U, oldPred - 1.U))
  btbWrite.pred := Mux(btbUpdateTagHits.asUInt.orR && redirectLatch._type === BTBtype.B, newPred, "b01".U)
  
  // 1.4 write BTB
  for (b <- 0 until BtbBanks) {
    for (w <- 0 until BtbWays) {
      when (b.U === bankLatch) {
        btb(b)(w).io.w.req.valid := OHToUInt(btbWriteWay) === w.U &&
          RegNext(io.redirect.valid, init = false.B) &&
          (redirectLatch._type === BTBtype.B || redirectLatch._type === BTBtype.J)
        btb(b)(w).io.w.req.bits.setIdx := btbAddr.getBankIdx(redirectLatch.pc)
        btb(b)(w).io.w.req.bits.data := btbWrite
      }.otherwise {
        btb(b)(w).io.w.req.valid := false.B
        btb(b)(w).io.w.req.bits.setIdx := DontCare
        btb(b)(w).io.w.req.bits.data := DontCare
      }
    }
  }

  // 2. update JBTAC
  val jbtacWrite = WireInit(0.U.asTypeOf(jbtacEntry()))
  jbtacWrite.valid := true.B
  jbtacWrite.tag := jbtacAddr.getTag(io.redirect.bits.pc)
  jbtacWrite.target := io.redirect.bits.target
  (0 until JbtacBanks).map(b =>
    jbtac(b).io.w.req.valid := io.redirect.valid &&
      b.U === jbtacAddr.getBank(io.redirect.bits.pc) &&
      io.redirect.bits._type === BTBtype.I)
  (0 until JbtacBanks).map(b => jbtac(b).io.w.req.bits.setIdx := jbtacAddr.getBankIdx(io.redirect.bits.pc))
  (0 until JbtacBanks).map(b => jbtac(b).io.w.req.bits.data := jbtacWrite)
  */
}
