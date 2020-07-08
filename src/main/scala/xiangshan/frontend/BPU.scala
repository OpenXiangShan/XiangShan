package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import xiangshan.backend.ALUOpType
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
    val redirectInfo = Flipped(new RedirectInfo)
    // from Stage3
    val flush = Input(Bool())
    val s3RollBackHist = Input(UInt(HistoryLength.W))
    // to ifu, quick prediction result
    val s1OutPred = ValidIO(new BranchPrediction)
    // to Stage2
    val out = Decoupled(new Stage1To2IO)
  })

<<<<<<< Updated upstream
=======
  // flush Stage1 when io.flush || io.redirect.valid

  val predictWidth = 8
  def btbTarget = new Bundle {
    val addr = UInt(VAddrBits.W)
    val pred = UInt(2.W) // 2-bit saturated counter as a quick predictor
    val _type = UInt(2.W)
    val offset = if (offsetBits()) Some(UInt(offsetBits().W)) else None

    def offsetBits() = log2Up(FetchWidth / predictWidth)
  }

  def btbEntry() = new Bundle {
    val valid = Bool()
    // TODO: don't need full length of tag and target
    val tag = UInt(btbAddr.tagBits.W)
    val target = Vec(predictWidth, btbTarget)
  }

  val btb = List.fill(BtbWays)(List.fill(BtbBanks)(
    Module(new SRAMTemplate(btbEntry(), set = BtbSets / BtbBanks, shouldReset = true, holdRead = true, singlePort = false))))
  
  // val btbReadBank = btbAddr.getBank(io.in.pc.bits)

  // BTB read requests
  for (w <- 0 until BtbWays) {
    for (b <- 0 until BtbBanks) {
      btb(w)(b).reset := reset.asBool
      btb(w)(b).io.r.req.valid := io.in.pc.valid && b.U === btbAddr.getBank(io.in.pc.bits)
      btb(w)(b).io.r.req.bits.setIdx := btbAddr.getBankIdx(io.in.pc.bits)
    }
  }

  // latch pc for 1 cycle latency when reading SRAM
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  val btbRead = Wire(Vec(BtbWays, Vec(BtbBanks, btbEntry())))
  val btbHits = Wire(Vec(BtbWays, Bool()))

  // #(predictWidth) results
  val btbTargets = Wire(Vec(predictWidth, UInt(VAddrBits.W)))
  val btbTypes = Wire(Vec(predictWidth, UInt(2.W)))
  // val btbPreds = Wire(Vec(FetchWidth, UInt(2.W)))
  val btbTakens = Wire(Vec(predictWidth, Bool()))
  for (w <- 0 until BtbWays) {
    for (b <- 0 until BtbBanks) {
      btbRead(w)(b) := btb(w)(b).io.r.resp.data(0)
    }
  }
  for (i <- 0 until predictWidth) {
    // btbHits(i) := false.B
    for (w <- 0 until BtbWays) {
      btbHits(w) := false.B
      for (b <- 0 until BtbBanks) {
        when (b.U === btbAddr.getBank(pcLatch) && btbRead(w)(b).valid && btbRead(w)(b).tag === btbAddr.getTag(pcLatch))) {
          btbHits(w) := !flush && RegNext(btb(w)(b).io.r.req.fire(), init = false.B)
          btbTargets(i) := btbRead(w)(b).target(i)
          btbTypes(i) := btbRead(w)(b)._type(i)
          btbTakens(i) := (btbRead(b)(w).pred(i))(1).asBool
        }.otherwise {
          btbHits(w) := false.B
          btbTargets(i) := DontCare
          btbTypes(i) := DontCare
          btbTakens(i) := DontCare
        }
      }
    }
  }

  val btbTakenidx := MuxCase(0.U, (0 until predictWidth).map(i => btbTakens(i)) zip (0.U until predictWidth.U))
  val btbTakenTarget := btbTargets(btbTakenidx)
  val btbTakenType := btbTypes(btbTakenidx)

  // JBTAC, divided into 8 banks, makes prediction for indirect jump except ret.
  val jbtacAddr = new TableAddr(log2Up(JbtacSize), JbtacBanks)
  def jbtacEntry() = new Bundle {
    val valid = Bool()
    // TODO: don't need full length of tag and target
    val tag = UInt(jbtacAddr.tagBits.W)
    val target = UInt(VAddrBits.W)
    val offset = UInt(log2Up(FetchWidth).W)
  }

  val jbtac = List.fill(JbtacBanks)(Module(new SRAMTemplate(jbtacEntry(), set = JbtacSize / JbtacBanks, shouldReset = true, holdRead = true, singlePort = false)))

  (0 until JbtacBanks).map(b => jbtac(b).reset := reset.asBool)
  (0 until JbtacBanks).map(b => jbtac(b).io.r.req.valid := io.in.pc.valid && b.U === jbtacAddr.getBank(io.in.pc.bits))
  (0 until JbtacBanks).map(b => jbtac(b).io.r.req.bits.setIdx := jbtacAddr.getBankIdx(io.in.pc.bits))
  val jbtacRead = Wire(Vec(JbtacBanks, jbtacEntry()))
  (0 until JbtacBanks).map(b => jbtacRead(b) := jbtac(b).io.r.resp.data(0))
  
  val jbtacHits = Wire(Vec(JbtacBanks, Bool()))
  val jbtacHitIdxs = Wire(UInt(log2Up(FetchWidth).W))
  val jbtacTargets = Wire(UInt(VAddrBits.W))
  
  val jbtacHit = Wire(Bool())
  val jbtacHitIdx = Wire(UInt(log2Up(FetchWidth).W))
  val jbtacTarget = Wire(UInt(VAddrBits.W))

  jbtacHit := jbtacRead(b).valid
  jbtacHitIdx := jbtacRead.offset
  jbtacTarget := jbtacRead.target
  for (b <- 0 until JbtacBanks) {
    when (jbtacAddr.getBank(pcLatch) === b.U && jbtacRead(b).valid && jbtacRead(b).ta === jbtacAddr.getTag(pcLatch)) {
      jbtacHit := !flush && RegNext(jbtac(b).io.r.req.fire(), init = false.B)
      jbtacTarget := jbtacRead(b).target
    }.otherwise {
      jbtacHits(i) := false.B
      jbtacTargets(i) := DontCare
    }
  }

  // redirect based on BTB and JBTAC
  (0 until FetchWidth).map(i => io.predMask(i) := btbHits(i) && Mux(btbTypes(i) === BTBtype.B, btbTakens(i), true.B) || jbtacHits(i))
  (0 until FetchWidth).map(i => io.predTargets(i) := Mux(btbHits(i) && !(btbTypes(i) === BTBtype.B && !btbTakens(i)), btbTargets(i), jbtacTargets(i)))

  def getLowerMask(idx: UInt, len: Int) = (0 until len).map(i => idx >> i.U).reduce(_|_)
  def getLowestBit(idx: UInt, len: Int) = Mux(idx(0), 1.U(len.W), Reverse(((0 until len).map(i => Reverse(idx(len - 1, 0)) >> i.U).reduce(_|_) + 1.U) >> 1.U))
  

  io.s1OutPred.valid := RegNext(io.in.pc.valid)
  io.s1OutPred.redirect := btbHits.orR && btbTakens.orR
  io.s1OutPred.instrValid := ~getLowerMask(btbTakenidx, FetchWidth)
  io.s1OutPred.target := btbTakenTarget
  io.s1OutPred.hist := DontCare
  io.s1OutPred.rasSp := DontCare
  io.s1OutPred.rasTopCtr := DontCare



>>>>>>> Stashed changes
  // TODO: delete this!!!
  io.in.pc.ready := true.B

  io.out.valid := false.B
  io.out.bits := DontCare

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
  io.s1OutPred.bits.tageMeta := tage.io.meta

  // flush Stage1 when io.flush || io.redirect.valid
  val flush = flushS1 || io.redirectInfo.valid

  val btbAddr = new TableAddr(log2Up(BtbSets), BtbBanks)
  val predictWidth = FetchWidth
  def btbTarget = new Bundle {
    val addr = UInt(VAddrBits.W)
    val pred = UInt(2.W) // 2-bit saturated counter as a quick predictor
    val _type = UInt(2.W)
    val offset = UInt(offsetBits().W) // Could be zero

    def offsetBits() = log2Up(FetchWidth / predictWidth)
  }

  def btbEntry() = new Bundle {
    val valid = Bool()
    // TODO: don't need full length of tag and target
    val tag = UInt(btbAddr.tagBits.W)
    val target = Vec(predictWidth, btbTarget)
  }

  val btb = List.fill(BtbWays)(List.fill(BtbBanks)(
    Module(new SRAMTemplate(btbEntry(), set = BtbSets / BtbBanks, shouldReset = true, holdRead = true, singlePort = false))))
  
  // val btbReadBank = btbAddr.getBank(io.in.pc.bits)

  // BTB read requests
  // read addr comes from pc[6:2]
  // read 4 ways in parallel
  (0 until BtbWays).map(
    w => (0 until BtbBanks).map(
      b => {
        btb(w)(b).reset := reset.asBool
        btb(w)(b).io.r.req.valid := io.in.pc.valid && b.U === btbAddr.getBank(io.in.pc.bits)
        btb(w)(b).io.r.req.bits.setIdx := btbAddr.getBankIdx(io.in.pc.bits)
      }))

  // latch pc for 1 cycle latency when reading SRAM
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  // Entries read from SRAM
  val btbRead = Wire(Vec(BtbWays, Vec(BtbBanks, btbEntry())))
  // 1/4 hit
  val btbHits = Wire(Vec(BtbWays, Bool()))

  // #(predictWidth) results
  val btbTargets = Wire(Vec(predictWidth, UInt(VAddrBits.W)))
  val btbTypes = Wire(Vec(predictWidth, UInt(2.W)))
  // val btbPreds = Wire(Vec(FetchWidth, UInt(2.W)))
  val btbTakens = Wire(Vec(predictWidth, Bool()))

  val btbHitWay = Wire(UInt(log2Up(BtbWays).W))

  (0 until BtbWays).map(
    w => (0 until BtbBanks).map(
      b => btbRead(w)(b) := btb(w)(b).io.r.resp.data(0)
    )
  )

  
  for (w <- 0 until BtbWays) {
    for (b <- 0 until BtbBanks) {
      when (b.U === btbAddr.getBank(pcLatch) && btbRead(w)(b).valid && btbRead(w)(b).tag === btbAddr.getTag(pcLatch)) {
        btbHits(w) := !flush && RegNext(btb(w)(b).io.r.req.fire(), init = false.B)
        btbHitWay := w.U
        for (i <- 0 until predictWidth) {
          btbTargets(i) := btbRead(w)(b).target(i).addr
          btbTypes(i) := btbRead(w)(b).target(i)._type
          btbTakens(i) := (btbRead(b)(w).target(i).pred)(1).asBool
        }
      }.otherwise {
        btbHits(w) := false.B
        btbHitWay := DontCare
        for (i <- 0 until predictWidth) {
          btbTargets(i) := DontCare
          btbTypes(i) := DontCare
          btbTakens(i) := DontCare
        }
      }
    }
  }


  val btbHit = btbHits.reduce(_|_)

  // Priority mux which corresponds with inst orders
  // BTB only produce one single prediction
  val btbTakenTarget = MuxCase(0.U, btbTakens zip btbTargets)
  val btbTakenType   = MuxCase(0.U, btbTakens zip btbTypes)
  val btbTaken       = btbTakens.reduce(_|_)
  // Record which inst is predicted taken
  val btbTakenIdx = MuxCase(0.U, btbTakens zip (0 until predictWidth).map(_.U))

  // JBTAC, divided into 8 banks, makes prediction for indirect jump except ret.
  val jbtacAddr = new TableAddr(log2Up(JbtacSize), JbtacBanks)
  def jbtacEntry() = new Bundle {
    val valid = Bool()
    // TODO: don't need full length of tag and target
    val tag = UInt(jbtacAddr.tagBits.W)
    val target = UInt(VAddrBits.W)
    val offset = UInt(log2Up(FetchWidth).W)
  }

  val jbtac = List.fill(JbtacBanks)(Module(new SRAMTemplate(jbtacEntry(), set = JbtacSize / JbtacBanks, shouldReset = true, holdRead = true, singlePort = false)))

  val jbtacRead = Wire(Vec(JbtacBanks, jbtacEntry()))

  val jbtacFire = Wire(Vec(JbtacBanks, RegInit(Bool(), init=false.B)))
  // Only read one bank
  (0 until JbtacBanks).map(
    b=>{
      jbtac(b).reset := reset.asBool
      jbtac(b).io.r.req.valid := io.in.pc.valid && b.U === jbtacAddr.getBank(io.in.pc.bits)
      jbtac(b).io.r.req.bits.setIdx := jbtacAddr.getBankIdx(io.in.pc.bits)
      jbtacFire(b) := jbtac(b).io.r.req.fire()
      jbtacRead(b) := jbtac(b).io.r.resp.data(0)
    }
  )

  val jbtacBank = jbtacAddr.getBank(pcLatch)
  val jbtacHit = jbtacRead(jbtacBank).valid && jbtacRead(jbtacBank).tag === jbtacAddr.getTag(pcLatch) && !flush && jbtacFire(jbtacBank)
  val jbtacHitIdx = jbtacRead(jbtacBank).offset
  val jbtacTarget = jbtacRead(jbtacBank).target


  // redirect based on BTB and JBTAC
  // (0 until FetchWidth).map(i => io.predMask(i) := btbHits(i) && Mux(btbTypes(i) === BTBtype.B, btbTakens(i), true.B) || jbtacHits(i))
  // (0 until FetchWidth).map(i => io.predTargets(i) := Mux(btbHits(i) && !(btbTypes(i) === BTBtype.B && !btbTakens(i)), btbTargets(i), jbtacTargets(i)))

  io.out.valid := RegNext(io.in.pc.valid) && !flush

  io.s1OutPred.valid := RegNext(io.in.pc.valid)
  io.s1OutPred.bits.redirect := btbHit && btbTaken || jbtacHit
  io.s1OutPred.bits.instrValid := ~LowerMask(btbTakenIdx, FetchWidth) & ~LowerMask(jbtacHitIdx, FetchWidth)
  io.s1OutPred.bits.target := Mux(btbTakenIdx < jbtacHitIdx, btbTakenTarget, jbtacTarget)
  io.s1OutPred.bits.hist := DontCare
  io.s1OutPred.bits.rasSp := DontCare
  io.s1OutPred.bits.rasTopCtr := DontCare

  io.out.bits.pc := pcLatch
  io.out.bits.btb.hits := btbHit
  (0 until FetchWidth).map(i=>io.out.bits.btb.targets(i) := btbTargets(i))
  io.out.bits.jbtac.hitIdx := jbtacHitIdx
  io.out.bits.jbtac.target := jbtacTarget
  io.out.bits.tage := DontCare
  io.out.bits.hist := DontCare
  io.out.bits.btbPred := io.s1OutPred


  // TODO: delete this!!!
  io.in.pc.ready := true.B

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
    validLatch := !io.flush
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
  val ras = RegInit(VecInit(Seq.fill(RasSize)(0.U.asTypeOf(rasEntry()))))
  val sp = Counter(RasSize)
  val rasTop = ras(sp.value)
  val rasTopAddr = rasTop.retAddr

  // get the first taken branch/jal/call/jalr/ret in a fetch line
  // brTakenIdx/jalIdx/callIdx/jalrIdx/retIdx/jmpIdx is one-hot encoded.
  // brNotTakenIdx indicates all the not-taken branches before the first jump instruction.
  val brIdx = inLatch.btb.hits & Cat(io.predecode.bits.fuTypes.map { t => ALUOpType.isBranch(t) }).asUInt & io.predecode.bits.mask
  val brTakenIdx = LowestBit(brIdx & inLatch.tage.takens.asUInt, FetchWidth)
  val jalIdx = LowestBit(inLatch.btb.hits & Cat(io.predecode.bits.fuTypes.map { t => t === ALUOpType.jal }).asUInt & io.predecode.bits.mask, FetchWidth)
  val callIdx = LowestBit(inLatch.btb.hits & io.predecode.bits.mask & Cat(io.predecode.bits.fuTypes.map { t => t === ALUOpType.call }).asUInt, FetchWidth)
  val jalrIdx = LowestBit(inLatch.jbtac.hitIdx & io.predecode.bits.mask & Cat(io.predecode.bits.fuTypes.map { t => t === ALUOpType.jalr }).asUInt, FetchWidth)
  val retIdx = LowestBit(io.predecode.bits.mask & Cat(io.predecode.bits.fuTypes.map { t => t === ALUOpType.ret }).asUInt, FetchWidth)

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
  val firstHist = inLatch.btbPred.bits.hist(0)
  // there may be several notTaken branches before the first jump instruction,
  // so we need to calculate how many zeroes should each instruction shift in its global history.
  // each history is exclusive of instruction's own jump direction.
  val histShift = Wire(Vec(FetchWidth, UInt(log2Up(FetchWidth).W)))
  val shift = Wire(Vec(FetchWidth, Vec(FetchWidth, UInt(1.W))))
  (0 until FetchWidth).map(i => shift(i) := Mux(!brNotTakenIdx(i), 0.U, ~LowerMask(UIntToOH(i.U), FetchWidth)).asTypeOf(Vec(FetchWidth, UInt(1.W))))
  for (j <- 0 until FetchWidth) {
    var tmp = 0.U
    for (i <- 0 until FetchWidth) {
      tmp = tmp + shift(i)(j)
    }
    histShift(j) := tmp
  }
  (0 until FetchWidth).map(i => io.out.bits.hist(i) := firstHist << histShift(i))
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
  io.btbOut <> s1.io.s1OutPred
  s1.io.s3RollBackHist := s3.io.s1RollBackHist

  s1.io.out <> s2.io.in
  s2.io.flush := s3.io.flushBPU || io.redirectInfo.flush()

  s2.io.out <> s3.io.in
  s3.io.flush := io.redirectInfo.flush()
  s3.io.predecode <> io.predecode
  io.tageOut <> s3.io.out
  s3.io.redirectInfo <> io.redirectInfo
}